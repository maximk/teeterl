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
-module(net_kernel).
-export([start/2]).
-export([connect/1]).

%% internal exports
-export([start0/1]).
-export([udp_sink/1,udp_sink0/3]).

-import(lists, [keysearch/3,keydelete/3,keyreplace/4]).
-import(lists, [partition/2,foreach/2]).

-record(ns, {sinks,
	routes = [],	%% routes known upon start
	udp_sock,		%% UDP socket to use in send
	waiting = [],
	spawning = [],
	connections = []}).

-define(dbg(X, Y), io:format(X, Y)).

start(Sinks, Routes) ->
	Pid = spawn(?MODULE, start0, [#ns{sinks=Sinks,routes=Routes}]),
	Pid ! {open_sinks,self()},
	receive
	{sinks_opened,_Rs} ->
		?dbg("Opening sinks: ~p~n", [_Rs]),
		register(net_kernel, Pid)
	end.

start0(St0) ->
	?dbg("start0 is ~w~n", [self()]),
	receive
	{open_sinks,From} ->
		Rs = open_sinks(St0),
		From ! {sinks_opened,Rs},
		
		%% kludgy
		UdpSock = hd([Sock || {ok,Sock} <- Rs]),
		
		R = (catch handle_messages(St0#ns{udp_sock=UdpSock})),
		
		?dbg("start0 died: ~p~n", [R])
	end.

connect(Node) ->
	net_kernel ! {connect_node,self(),Node},
	receive
	{node_connected,Node} ->
		ok;
	E ->
		{error,E}
	end.

open_sinks(St0) -> open_sinks(St0#ns.sinks, [], St0).
open_sinks([{udp,_Addr,Port}|Sinks], Rs, St0) ->
	Pid = spawn(?MODULE, udp_sink, [St0]),
	Pid ! {open_sink,self(),Port},
	receive
	{sink_opened,R} ->
		open_sinks(Sinks, [R|Rs], St0);
	{sink_error,E} ->
		open_sinks(Sinks, [E|Rs], St0)
	end;
open_sinks([], Rs, _) -> Rs.

udp_sink(St0) ->
	receive
	{open_sink,From,Port} ->
		case gen_udp:open(Port) of
		{ok,Sock}=R ->
			From ! {sink_opened,R},
			udp_sink0(Sock, From, St0);
		{error,_}=E ->
			From ! {sink_error,E}
		end
	end.

udp_sink0(Sock, Owner, St0) ->
	case gen_udp:recv(Sock, 65536) of
	{ok,{_Addr,_Port,Data}} ->
		case catch binary_to_term(Data) of
		{'EXIT',R} ->
			?dbg("Error: ~p~n", [R]),
			udp_sink0(Sock, Owner, St0);
		T ->
			?dbg("Message received: ~p~n", [T]),
			Owner ! T,
			udp_sink0(Sock, Owner, St0)
		end;
	{error,E} ->
		?dbg("Error receving message: ~p~n", [E]),
		udp_sink0(Sock, Owner, St0)
	end.

handle_messages(St0) ->
	receive
	{connect_node,From,Node} ->
		%% may have already been connected
		?dbg("connect_node(~w,~w) received~n", [From,Node]),
		case route(Node, St0#ns.routes) of
		{ok,Route} ->
			Sink = hd(St0#ns.sinks),
			Msg = {hello,node(),Sink},
			Bin = term_to_binary(Msg),
			dispatch_message(Route, Bin, St0),
			Ws = St0#ns.waiting,	%% [{Node,Pid}]
			handle_messages(St0#ns{waiting=[{Node,From}|Ws]});
		{error,Error} ->
			From ! {connect_error,Error},
			handle_messages(St0)
		end;
	{net_spawn,From,Node,{M,F,As}} ->
		%% must be connected
		Ref = erlang:make_ref(),
		send_message(Node, {spawn,node(),{M,F,As},Ref}, St0),
		Ss = St0#ns.spawning,
		handle_messages(St0#ns{spawning=[{Ref,From}|Ss]});
	{send_message,_From,Node,Pid,Msg} ->
		send_message(Node, {deliver,node(),Pid,Msg}, St0),
		handle_messages(St0);
	{hello,Node,Where} ->
		?dbg("hello(~w,~w) received~n", [Node,Where]),
		St1 = St0#ns{connections=[{Node,Where}|St0#ns.connections]},
		Sink = hd(St0#ns.sinks),
		send_message(Node, {hi,node(),Sink}, St1),
		handle_messages(St1);
	{hi,Node,Where} ->
		%% notify pids waiting for the connection
		?dbg("Waiting pids: ~p~n", [St0#ns.waiting]),
		{Ws1,Ws2} = partition(fun({N,_}) when N =:= Node ->
			true;
		(_) ->
			false
		end, St0#ns.waiting),
		foreach(fun({_,Pid}) ->
			Pid ! {node_connected,Node}
		end, Ws1),
		Connections = [{Node,Where}|St0#ns.connections],
		handle_messages(St0#ns{connections=Connections,waiting=Ws2});
	{goodbye,Node} ->
		send_message(Node, {bye,node()}, St0),
		Connections = keydelete(Node, 1, St0#ns.connections),
		handle_messages(St0#ns{connections=Connections});
	{bye,_Node} ->
		erlang:error(not_implemented);
	{spawn,Node,{M,F,As},Ref} ->
		Pid = spawn(M, F, As),
		send_message(Node, {spawned,node(),Ref,Pid}, St0),
		handle_messages(St0);
	{spawned,_Node,Ref,Pid} ->
		%%TODO: a single process is waiting, use keytake
		%% notify pids waiting for the spawn
		?dbg("Waiting for spawn: ~p~n", [St0#ns.spawning]),
		{Ss1,Ss2} = partition(fun({R,_}) when R =:= Ref ->
			true;
		(_) ->
			false
		end, St0#ns.spawning),
		foreach(fun({_,From}) ->
			From ! {net_spawned,Pid}
		end, Ss1),
		handle_messages(St0#ns{spawning=Ss2});
	{deliver,_Node,Pid,Msg} when is_pid(Pid) ->
		Pid ! Msg,
		handle_messages(St0);
	Msg ->
		?dbg("Unknown message received: ~p~n", [Msg]),
		handle_messages(St0)
	end.

route(Node, Rs) ->
	case keysearch(Node, 1, Rs) of
	{value,{_,Route}} ->
		{ok,Route};
	false ->
		{error,noroute}
	end.

send_message(Node, Msg, St0) ->
	?dbg("Connections: ~p~n", [St0#ns.connections]),
	?dbg("Sending message ~p to '~w'~n", [Msg,Node]),
	case keysearch(Node, 1, St0#ns.connections) of
	{value,{_,Where}} ->
		Bin = term_to_binary(Msg),
		dispatch_message(Where, Bin, St0);
	false ->
		?dbg("Route to ~w not found~n", [Node]),
		ok		%% dropped
	end.

dispatch_message({udp,Addr,Port}, Bin, St0) ->
	?dbg("Dispatching message ~p to ~p:~w~n", [Bin,Addr,Port]),
	Sock = St0#ns.udp_sock,
	gen_udp:send(Sock, Addr, Port, Bin).

%% EOF
