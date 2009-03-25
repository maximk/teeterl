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
-module(netudp).

-export([start/1,stop/0]).
-export([open_sink/1,dispatch/2]).

-import(lists,[delete/2,foreach/2]).

%% internal export
-export([server/1,listener/2]).

-record(nu,{
    origin,            %% a socket to send mesage from,
    handler,           %% fun(Bin) - message processing routine
    sinks=[]           %% pids of listening servers
}).

-define(LISTEN_HB, 1000).

-define(dbg(X, Y), io:format(X, Y)).

%% interface

start(Handler) ->
    case whereis(netudp) of
    undefined ->
        Server = spawn(?MODULE, server, [#nu{handler=Handler}]),
        register(netudp, Server),
        {ok,Server};

    Pid when is_pid(Pid) ->
        {error,{already_started,Pid}}
    end.

stop() ->
    case whereis(netudp) of
    undefined ->
        {error,not_found};
    Pid when is_pid(Pid) ->
        netudp ! {stop,self()},
        receive {ok,_} -> ok end
    end.

open_sink(Spec) ->
    netudp ! {sink,self(),Spec},
    receive ok -> ok end.

dispatch(Spec, Msg) ->
    netudp ! {send,self(),Spec,Msg},
    receive
    {ok,_} ->
        ok;
    {error,_,Error} ->
        {error,Error}
    end.

%% private

server(St0) ->
    receive
    {stop,From} ->
        foreach(fun(Sink) ->
            Sink ! {stop,self()},
            receive ok -> ok end
        end, St0#nu.sinks),
        
        unregister(netudp),
        From ! {ok,self()};
        
    {sink,From,{_Addr,Port}} ->
        self() ! {sink,From,Port},
        server(St0);
        
    {sink,From,Port} ->
		Self = self(),
		Pid = spawn(fun() ->
			case gen_udp:open(Port) of
			{ok,Sock} ->
				Self ! {ok,self(),Sock},
				listener(Sock, St0);
	        {error,Error} ->
		        ?dbg("Error opening udp sink: ~p~n", [Error]),
		        Self ! {error,self(),Error}
		    end
		end),
		
		receive
		{ok,_,Sock} ->

            %% the first sink is set as origin
            St1 = if St0#nu.origin /= undefined ->
				St0;
			true ->
				St0#nu{origin=Sock}
			end,
            
            From ! ok,
            server(St1#nu{sinks=[Pid|St0#nu.sinks]});
            
        {error,_,_} ->
            From ! ok,	  %% ignored
            server(St0)
        end;
    
    {sink_fail,From,_Sock,Error} ->
        ?dbg("Sink closed on error: ~p~n", [Error]),
        Sinks = delete(From, St0#nu.sinks),
        server(St0#nu{sinks=Sinks});

    {send,From,{Addr,Port},Msg} ->
		if St0#nu.origin == undefined ->
			From ! {error,self(),no_origin},
			server(St0);
		true ->
			Origin = St0#nu.origin,
            Bin = if is_binary(Msg) -> Msg; true -> term_to_binary(Msg) end,
            Reply = case gen_udp:send(Origin, Addr, Port, Bin) of
            ok -> {ok,self()}; {error,Error} -> {error,self(),Error} end,
            From ! Reply,
            server(St0)
        end
    end.

listener(Sock, St0) ->
    case gen_udp:recv(Sock, 65536, ?LISTEN_HB) of
    {ok,{_,_,Data}} ->
		H = St0#nu.handler,
		H(Data),
        listener(Sock, St0);
    {error,timeout} ->
        receive
        {stop,From} ->
            From ! ok    %% server exits
        after 0 ->
            listener(Sock, St0)
        end;
    {error,Error} ->
		?dbg("netudp listener err: ~p~n", [Error]),
        netudp ! {sink_fail,self(),Error},
        gen_udp:close(Sock)
    end.

%% EOF
