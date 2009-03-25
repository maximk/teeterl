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
-module(erlang).
-export([abs/1]).
-export([erase/0,erase/1,get/1,get_keys/1,put/2]).
-export([self/0,apply/3,exit/1,throw/1,error/1,error/2]).
-export([raise/3,get_stacktrace/0,get_stacktrace/1]).
-export([link/1,unlink/1]).
-export([get_module_info/1,get_module_info/2]).
-export([system_info/1]).
-export([spawn/3,spawn/1,spawn/4,spawn/2,spawn_link/3,spawn_link/1]).
-export([spawn_link/4,spawn_link/2]).
-export([spawn_monitor/3,spawn_monitor/1,spawn_monitor/4,spawn_monitor/2]).
-export([group_leader/0,group_leader/1]).
-export([monitor/2,demonitor/1]).

-export([binary_to_list/1,pid_to_list/1,ref_to_list/1,port_to_list/1,fun_to_list/1]).

-export([integer_to_list/1,list_to_integer/1]).
-export([integer_to_list/2,list_to_integer/2]).
-export([list_to_pid/1]).
-export([iolist_size/1]).
-export([is_alive/0,nodes/0]).
-export([hash/2]).
-export([processes/0,process_states/0,process_flag/2]).

-export([is_integer/1,is_atom/1,is_boolean/1,is_float/1,is_function/1,is_function/2]).
-export([is_number/1,is_record/3,is_list/1,is_tuple/1,is_binary/1,is_bitstring/1]).
-export(['not'/1,'bnot'/1,'++'/2,'--'/2,'+'/2,'+'/1,'-'/2,'-'/1]).
-export(['*'/2,'/'/2,'div'/2,'rem'/2]).
-export(['and'/2,'band'/2,'or'/2,'bor'/2,'xor'/2,'bxor'/2,'bsl'/2,'bsr'/2]).
-export(['<'/2,'=<'/2,'>'/2,'>='/2,'=='/2,'/='/2,'=:='/2,'=/='/2]).

-define(NET_TIMEOUT, 5000).

abs(N) when is_integer(N), N < 0 -> -N;
abs(N) when is_integer(N) -> N;
abs(Q) when is_float(Q), Q < 0 -> -Q;
abs(Q) when is_float(Q) -> Q.
	
%%
%% dictionary routines
%%

erase() ->
	D = erlang:get(),
	erlang:put([]),
	D.

erase(Key) ->
	case lists:keytake(Key, 1, erlang:get()) of
	{value,{_,Val},D1} ->
		erlang:put(D1),
		Val;
	false ->
		undefined
	end.

get(Key) ->
	case lists:keysearch(Key, 1, erlang:get()) of
	{value,{_,Val}} -> Val;
	false -> undefined
	end.

get_keys(Val) ->
	[Key || {Key,Val1} <- erlang:get(),Val1 =:= Val].

put(Key, Val) ->
	OldVal = case lists:keysearch(Key, 1, erlang:get()) of
	{value,{_,X}} -> X;
	false -> undefined
	end,
	D1 = lists:keystore(Key, 1, erlang:get(), {Key,Val}),
	erlang:put(D1),
	OldVal.

%%
%% entry to provide for apply(erlang, self, []), compiler just emits opcode 'self'
%%
self() ->
	erlang:self().

apply(M, F, As) when is_atom(M), is_atom(F), is_list(As) ->
	erlang:apply(M, F, As).

exit(R) ->
	erlang:exit(R).

throw(T) ->
	erlang:throw(T).

error(E) ->
	erlang:error(E).

error(E1, E2) ->
	erlang:error(E1, E2).

raise(Class, Reason, StackTrace) ->
	erlang:error(Class, {Reason,StackTrace}).	%% TODO: use raise op

link(Id) when is_pid(Id); is_port(Id) ->		%% true
	{init,node(Id)} ! {link,erlang:self(),Id},
	receive
	ok ->
		true
	after ?NET_TIMEOUT ->
		erlang:error(timeout)
	end.

unlink(Id) when is_pid(Id); is_port(Id) ->		%% true
	{init,node(Id)} ! {link,erlang:self(),Id},
	receive
	ok ->
		true
	after ?NET_TIMEOUT ->
		erlang:error(timeout)
	end.

get_stacktrace() ->
	erlang:get_stacktrace(erlang:self()).

get_stacktrace(Pid) ->
	Trace0 = erlang:get_stacktrace0(Pid),
	%% Trace0 -> [{Module,Offset}], the first record is {Module,Offset,ArgVars}

	lists:map(fun({Mod,Off}) ->
		case code:function_info(Mod, Off) of
		{F,N} ->
			{Mod,F,N};
		false ->
			unknown_frame
		end;
	({Mod,Off,ArgVars}) ->
		case code:function_info(Mod, Off) of
		{F,N} ->
			As = if length(ArgVars) >= N ->
				{Args,_Vars} = lists:split(N, ArgVars),
				Args;
			true ->
				[]	  %% TODO: it's an error
			end,
			{Mod,F,As};
		false ->
			unknown_first_frame
		end
	end, Trace0).

%% TODO: stub
system_info(machine) -> "isle";
system_info(_) -> undefined.

get_module_info(_Mod) -> todo.
get_module_info(_Mod, _Info) -> todo.

spawn(M, F, As) when is_atom(M), is_atom(F), is_list(As) ->
	init ! {spawn,erlang:self(),{M,F,As}},
	receive {spawned,Pid} -> Pid end.

spawn(F) when is_function(F, 0) ->
	init ! {spawn,erlang:self(),F},
	receive {spawned,Pid} -> Pid end.

spawn(N, M, F, As) when is_atom(N), is_atom(M), is_atom(F), is_list(As) ->
	{init,N} ! {spawn,erlang:self(),{M,F,As}},
	receive
	{spawned,Pid} ->
		Pid
	after ?NET_TIMEOUT ->
		erlang:error(timeout)
	end.

spawn(N, F) when is_atom(N), is_function(F, 0) ->
	{init,N} ! {spawn,erlang:self(),F},
	receive
	{spawned,Pid} ->
		Pid
	after ?NET_TIMEOUT ->
		erlang:error(timeout)
	end.

spawn_link(M, F, As) when is_atom(M), is_atom(F), is_list(As) ->
	init ! {spawn_link,erlang:self(),{M,F,As}},
	receive {spawned,Pid} -> Pid end.

spawn_link(F) when is_function(F, 0) ->
	init ! {spawn_link,erlang:self(),F},
	receive {spawned,Pid} -> Pid end.

spawn_link(N, M, F, As) when is_atom(N), is_atom(M), is_atom(F), is_list(As) ->
	{init,N} ! {spawn_link,erlang:self(),{M,F,As}},
	receive
	{spawned,Pid} ->
		Pid
	after ?NET_TIMEOUT ->
		erlang:error(timeout)
	end.

spawn_link(N, F) when is_atom(N), is_function(F, 0) ->
	{init,N} ! {spawn_link,erlang:self(),F},
	receive
	{spawned,Pid} ->
		Pid
	after ?NET_TIMEOUT ->
		erlang:error(timeout)
	end.

spawn_monitor(M, F, As) when is_atom(M), is_atom(F), is_list(As) ->
	init ! {spawn_monitor,erlang:self(),{M,F,As}},
	receive {spawned,Pid,Ref} -> {Pid,Ref} end.

spawn_monitor(F) when is_function(F, 0) ->
	init ! {spawn_monitor,erlang:self(),F},
	receive {spawned,Pid,Ref} -> {Pid,Ref} end.

spawn_monitor(N, M, F, As) when is_atom(M), is_atom(F), is_list(As) ->
	{init,N} ! {spawn_monitor,erlang:self(),{M,F,As}},
	receive
	{spawned,Pid,Ref} ->
	  {Pid,Ref}
	after ?NET_TIMEOUT ->
	  erlang:error(timeout)
	end.

spawn_monitor(N, F) when is_function(F, 0) ->
	{init,N} ! {spawn_monitor,erlang:self(),F},
	receive
	{spawned,Pid,Ref} ->
	  {Pid,Ref}
	after ?NET_TIMEOUT ->
	  erlang:error(timeout)
	end.

group_leader() -> stdio.
group_leader(_Pid) -> stdio.

monitor(process, Pid) when is_pid(Pid) ->
	{init,node(Pid)} ! {monitor,erlang:self(),process,Pid},
	receive
	{ok,Ref} ->
		Ref
	after ?NET_TIMEOUT ->
		erlang:error(timeout)
	end.

demonitor(Ref) ->
	{init,node(Ref)} ! {demonitor,erlang:self(),Ref},
	receive
	ok ->
		ok
	after ?NET_TIMEOUT ->
		erlang:error(timeout)
	end.

binary_to_list(<<>>) ->
	[];
binary_to_list(Bin) ->
	erlang:binary_to_list(Bin, 1, size(Bin)).

pid_to_list(Pid) when is_pid(Pid), node(Pid) =:= local ->
	{_Node,Serial,Creation} = erlang:prp_triple(Pid),
	io_lib:format("<~w.~w>", [Serial,Creation]);
pid_to_list(Pid) when is_pid(Pid) ->
	{Node,Serial,Creation} = erlang:prp_triple(Pid),
	io_lib:format("<~w.~w.~w>", [Node,Serial,Creation]).

ref_to_list(Ref) when is_reference(Ref), node(Ref) =:= local ->
	{_Node,Serial,Creation} = erlang:prp_triple(Ref),
	io_lib:format("#Ref<~w.~w>", [Serial,Creation]);
ref_to_list(Ref) when is_reference(Ref) ->
	{Node,Serial,Creation} = erlang:prp_triple(Ref),
	io_lib:format("#Ref<~w.~w.~w>", [Node,Serial,Creation]).

port_to_list(Port) when is_port(Port), node(Port) =:= local ->
	{_Node,Serial,Creation} = erlang:prp_triple(Port),
	io_lib:format("#Port<~w.~w>", [Serial,Creation]);
port_to_list(Port) when is_port(Port) ->
	{Node,Serial,Creation} = erlang:prp_triple(Port),
	io_lib:format("#Port<~w.~w.~w>", [Node,Serial,Creation]).

fun_to_list(Fun) when is_function(Fun) ->
	{_,M} = erlang:fun_info(Fun, module),
	{_,F} = erlang:fun_info(Fun, name),
	{_,N} = erlang:fun_info(Fun, arity),
	{_,E} = erlang:fun_info(Fun, env),
	
	["#Fun<",
		atom_to_list(M), ":",
		atom_to_list(F), "/",
		erlang:integer_to_list(N), ",",
		io_lib:format("~w", [E]),
	 ">"].

integer_to_list(I) ->
    erlang:integer_to_list(I, 10).

integer_to_list(I, Base) 
  when is_integer(I), is_integer(Base), Base >= 2, Base =< 1+$Z-$A+10 ->
    if I < 0 ->
	    [$-|integer_to_list(-I, Base, [])];
       true ->
	    integer_to_list(I, Base, [])
    end;
integer_to_list(I, Base) ->
    erlang:error(badarg, [I, Base]).

integer_to_list(I0, Base, R0) ->
    D = I0 rem Base,
    I1 = I0 div Base,
    R1 = if D >= 10 ->
		 [D-10+$A|R0];
	    true ->
		 [D+$0|R0]
	 end,
    if I1 =:= 0 ->
	    R1;
       true ->
	    integer_to_list(I1, Base, R1)
    end.

list_to_integer(L) ->
	erlang:list_to_integer(L, 10).
	
list_to_integer(L, Base)
  when is_list(L), is_integer(Base), Base >= 2, Base =< 1+$Z-$A+10 ->
    case list_to_integer_sign(L, Base) of 
	I when is_integer(I) ->
	    I;
	Fault ->
	    erlang:error(Fault, [L,Base])
    end;
list_to_integer(L, Base) ->
    erlang:error(badarg, [L,Base]).

list_to_integer_sign([$-|[_|_]=L], Base) ->
    case list_to_integer(L, Base, 0) of
	I when is_integer(I) ->
	    -I;
	I ->
	    I
    end;
list_to_integer_sign([$+|[_|_]=L], Base) ->
    list_to_integer(L, Base, 0);
list_to_integer_sign([_|_]=L, Base) ->
    list_to_integer(L, Base, 0);
list_to_integer_sign(_, _) ->
    badarg.

list_to_integer([D|L], Base, I) 
  when is_integer(D), D >= $0, D =< $9, D < Base+$0 ->
    list_to_integer(L, Base, I*Base + D-$0);
list_to_integer([D|L], Base, I) 
  when is_integer(D), D >= $A, D < Base+$A-10 ->
    list_to_integer(L, Base, I*Base + D-$A+10);
list_to_integer([D|L], Base, I) 
  when is_integer(D), D >= $a, D < Base+$a-10 ->
    list_to_integer(L, Base, I*Base + D-$a+10);
list_to_integer([], _, I) ->
    I;
list_to_integer(_, _, _) ->
    badarg.

list_to_pid(L) ->
	
	$< = hd(L),
	$> = lists:last(L),
	
	L1 = string:substr(L, 2, string:len(L)-2),
	[Node,Serial,Creation] = string:tokens(L1, "."),
	
	N = list_to_atom(Node),
	S = erlang:list_to_integer(Serial),
	C = erlang:list_to_integer(Creation),
	
	erlang:make_pid(N, S, C).

iolist_size([H|T]) ->
	erlang:iolist_size(H) + erlang:iolist_size(T);
iolist_size(Bin) when is_binary(Bin) ->
	size(Bin);
iolist_size(Char) when is_integer(Char) ->
	1;
iolist_size([]) ->
	0.

is_alive() -> node() /= local.

nodes() ->
	case erlang:is_alive() of
	true ->
		netmesh:nodes();
	false ->
		[]
	end.

hash(Term, Range) -> erlang:phash(Term, Range).

process_states() ->
	init ! {processes,erlang:self()},
	receive
	{processes,_,Qs,Ws} ->
		{Qs,Ws}
	end.

processes() ->
	init ! {processes,erlang:self()},
	receive
	{processes,InitPid,Qs,Ws} ->
		Pss = [[Pid || {_,Pid,_} <- Ws]] ++ [Ps || {_,Ps} <- Qs],
		[InitPid|lists:append(Pss)]
	end.

process_flag(Flag, Value) ->
	erlang:process_flag(erlang:self(), Flag, Value).

%% for runtime calculations
is_integer(X) -> erlang:is_integer(X).
is_atom(X) -> erlang:is_atom(X).
is_boolean(X) -> erlang:is_boolean(X).
is_float(X) -> erlang:is_float(X).
is_function(X) -> erlang:is_function(X).
is_function(X, N) -> erlang:is_function(X, N).
is_number(X) -> erlang:is_number(X).
is_record(X, R, N) -> erlang:is_record(X, R, N).
is_list(X) -> erlang:is_list(X).
is_tuple(X) -> erlang:is_tuple(X).
is_binary(X) -> erlang:is_binary(X).

is_bitstring(X) -> erlang:is_binary(X).		%% for now, binary is the only valid bitstring

'not'(X) -> erlang:'not'(X).
'bnot'(X) -> erlang:'bnot'(X).
'++'(X, Y) -> erlang:'++'(X, Y).
'--'(X, Y) -> erlang:'--'(X, Y).
'+'(X, Y) -> erlang:'+'(X, Y).
'+'(X) -> X.
'-'(X, Y) -> erlang:'-'(X, Y).
'-'(X) -> erlang:'-'(X).

'*'(X, Y) -> erlang:'*'(X, Y).
'/'(X, Y) -> erlang:'/'(X, Y).
'div'(X, Y) -> erlang:'div'(X, Y).
'rem'(X, Y) -> erlang:'rem'(X, Y).

'and'(X, Y) -> erlang:'and'(X, Y).
'band'(X, Y) -> erlang:'band'(X, Y).
'or'(X, Y) -> erlang:'or'(X, Y).
'bor'(X, Y) -> erlang:'bor'(X, Y).
'xor'(X, Y) -> erlang:'xor'(X, Y).
'bxor'(X, Y) -> erlang:'bxor'(X, Y).
'bsl'(X, Y) -> erlang:'bsl'(X, Y).
'bsr'(X, Y) -> erlang:'bsr'(X, Y).

'<'(X, Y) -> erlang:'<'(X, Y).
'=<'(X, Y) -> erlang:'=<'(X, Y).
'>'(X, Y) -> erlang:'>'(X, Y).
'>='(X, Y) -> erlang:'>='(X, Y).
'=='(X, Y) -> erlang:'=='(X, Y).
'/='(X, Y) -> erlang:'/='(X, Y).
'=:='(X, Y) -> erlang:'=:='(X, Y).
'=/='(X, Y) -> erlang:'=/='(X, Y).

%% EOF
