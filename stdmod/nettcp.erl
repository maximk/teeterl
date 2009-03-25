%%
%% Copyright (c) 2009, Maxim Kharchenko
%% All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions are met:
%%     * Redistributions of source code must retain the above copyright
%%       notice, this list of conditions and the following disclaimer.
%%     * Redistributions in binary form must reproduce the above copyright
%%       notice, this list of conditions and the following disclaimer in the
%%       documentation and/or other materials provided with the distribution.
%%     * Neither the name of the author nor the names of his contributors
%%		 may be used to endorse or promote products derived from this software
%%		 without specific prior written permission.
%%
%% THIS SOFTWARE IS PROVIDED BY Maxim Kharchenko ''AS IS'' AND ANY
%% EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
%% WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
%% DISCLAIMED. IN NO EVENT SHALL Maxim Kharchenko BE LIABLE FOR ANY
%% DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
%% (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
%% LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
%% ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
%% (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
%% SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
%%
-module(nettcp).

-export([start/1,stop/0]).
-export([open_sink/1,dispatch/2]).

-import(lists,[foreach/2,keytake/3,keydelete/3,delete/2]).

%% internal exports
-export([server/1,listener/2,acceptor/3,reader/2]).

-record(nt,{handler,
  sinks=[],				%% [Listener]
  connections=[]		%% [#co]
}).

-record(co,{spec,		%% endpoint spec if known
  sock,					%% a socket to send messages
  reader,				%% pid reading incoming messages
  last					%% last time connection used
}).

-define(LISTEN_HB, 1000).

-define(dbg(X, Y), io:format(X, Y)).

%% interface

start(Handler) ->
  case whereis(nettcp) of
  undefined ->
    Server = spawn(?MODULE, server, [#nt{handler=Handler}]),
    register(nettcp, Server),
    {ok,Server};

  Pid when is_pid(Pid) ->
    {error,{already_started,Pid}}
  end.

stop() ->
  case whereis(nettcp) of
  undefined ->
    {error,not_found};
  Pid when is_pid(Pid) ->
    nettcp ! {stop,self()},
    receive {ok,_} -> ok end
  end.

open_sink(Spec) ->
  nettcp ! {open_sink,self(),Spec},
  receive X -> X end.

dispatch(Spec, Msg) ->
  nettcp ! {send,self(),Spec,Msg},
  receive
  {ok,_} ->
    ok;
  {error,_,Error} ->
    {error,Error}
  end.

%% private

server(St0) ->
  receive
  {open_sink,From,Port} ->
	case gen_tcp:listen(Port, []) of
	{ok,ListenSock} ->
	  From ! ok,
	  Sink = spawn(?MODULE, listener, [ListenSock,St0#nt.handler]),
	  server(St0#nt{sinks=[Sink|St0#nt.sinks]});
	{error,_}=E ->
	  From ! E
	end;

  {sink_fail,Sink,Error} ->
	?dbg("nettcp: sink closed on error: ~p~n", [Error]),
	Sinks = delete(Sink, St0#nt.sinks),
	server(St0#nt{sinks=Sinks});

  {connection,Spec,Sock,Reader} ->
	%?dbg("nettcp: connection added: ~w~n", [Reader]),
	
	%% change the controlling process for the socket
	%% so that messages can be sent from nettcp
	
	gen_tcp:controlling_process(Sock, self()),

	Conn = #co{spec=Spec,sock=Sock,reader=Reader,last=now()},
	server(St0#nt{connections=[Conn|St0#nt.connections]});

  {conn_fail,Reader,Error} ->
	?dbg("nettcp: connection closed on error: ~w~n", [Error]),
	Conns = keydelete(Reader, #co.reader, St0#nt.connections),
	server(St0#nt{connections=Conns});

  {send,From,{Addr,Port}=Spec,Msg} ->
	{Conn,Conns} = case keytake(Spec, #co.spec, St0#nt.connections) of
	{value,C,Cs} ->
	  %% connection is already open
	  {C,Cs};
	  
	false ->
	  Self = self(),
	  Reader=spawn(fun() ->
		case gen_tcp:connect(Addr, Port, []) of
		{ok,S} ->	  
		  Self ! {connected,S},
		  reader(S, St0#nt.handler),
		  gen_tcp:close(S);
		{error,_}=E ->
		  Self ! E
		end
	  end),
	  
	  receive
	  {connected,Sock} ->
		%?dbg("nettcp: connected: ~w~n", [Reader]),
		
		%% make it suitable for sending messages
		gen_tcp:controlling_process(Sock, self()),
		
		C = #co{spec=Spec,sock=Sock,reader=Reader},
		{C,St0#nt.connections};
	  {error,_Err} ->
		?dbg("nettcp: conn err: ~p~n", [_Err]),
		{error,St0#nt.connections}
	  end
	end,
	
	if Conn /= error ->
	  Bin = if is_binary(Msg) -> Msg;
		true -> term_to_binary(Msg) end,
		
	  %% add 2-byte size field
	  N = size(Bin),
	  Bin2 = <<N:16,Bin/binary>>,
	  
	  case gen_tcp:send(Conn#co.sock, Bin2) of
	  ok ->
		From ! {ok,self()},
		server(St0#nt{connections=[Conn#co{last=now()}|Conns]});
	  
	  {error,Err} ->
		?dbg("nettcp: send err: ~p~n", [Err]),
		From ! {error,self(),Err},
		server(St0#nt{connections=Conns})	  
	  end;

	true ->
	  From ! {error,self(),unreachable},
	  server(St0#nt{connections=Conns})
	end;

  {stop,From} ->
    foreach(fun(Sink) ->
      Sink ! {stop,self()},
      receive {acceptor_stopped,_} -> ok end
    end, St0#nt.sinks),

    foreach(fun(#co{reader=Reader}) ->
      Reader ! {stop,self()},
      receive {reader_stopped,_} -> ok end
    end, St0#nt.connections),
    
    unregister(nettcp),
    From ! {ok,self()};

  X ->
	?dbg("nettcp ignored: ~p~n", [X])
  end.

listener(ListenSock, Handler) ->
  Acceptor = spawn(?MODULE, acceptor, [self(),ListenSock,Handler]),
  receive
  {accepted,_Sock} ->
	Acceptor ! ok,
	listener(ListenSock, Handler);
  {error,Err} ->
	nettcp ! {sink_fail,self(),Err};
  {stop,From} ->
	Acceptor ! {stop,self()},
	receive {stopped,_} -> ok end,
	From ! {acceptor_stopped,self()}
  end.

acceptor(Listener, ListenSock, Handler) ->
  case gen_tcp:accept(ListenSock, 1000) of
  {ok,Sock} ->
	%?dbg("accept returns: ~w~n", [X]),
	nettcp ! {connection,none,Sock,self()},	%% XXX: determine spec

	Listener ! {accepted,Sock},
	receive ok -> ok end,		  %% should be sync
	
	reader(Sock, Handler),
	gen_tcp:close(Sock);
  {error,timeout} ->
	receive
	{stop,From} ->
	  From ! {stopped,self()},
	  ok  %% process exits
	after 0 ->
	  acceptor(Listener, ListenSock, Handler)
	end
  end.

reader(Sock, Handler) ->
  case gen_tcp:recv(Sock, 2, 1000) of
  {ok,<<N:16>>} ->
	reader0(Sock, Handler, N);
  {error,timeout} ->
	receive
	{stop,From} ->
	  From ! {reader_stopped,self()}    %% server exits
	after 0 ->
	  reader(Sock, Handler)
	end;
  {error,Error} ->
	nettcp ! {conn_fail,self(),Error},
	gen_tcp:close(Sock)
  end.

reader0(Sock, Handler, N) ->
  case gen_tcp:recv(Sock, N, 1000) of
  {ok,Data} ->
	Handler(Data),
	reader(Sock, Handler);
  {error,timeout} ->
	receive
	{stop,From} ->
	  From ! {reader_stopped,self()}    %% server exits
	after 0 ->
	  reader0(Sock, Handler, N)
	end;
  {error,Error} ->
	nettcp ! {conn_fail,self(),Error},
	gen_tcp:close(Sock)
  end.
        
%% EOF
