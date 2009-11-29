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
-module(rpc).

-export([server/1,call/4,call/5]).

%% for testing with exec
-export([test/1]).

-import(lists, [reverse/1]).

call(Node, Module, Function, Args) ->	  %% Res | {badrpc, Reason}
	call(Node, Module, Function, Args, infinity).

call(Node, Module, Function, Args, Timeout)
		when is_atom(Node), is_atom(Module), is_atom(Function), is_list(Args) ->
	case is_alive() of
	true ->
		Ref = make_ref(),		
		{rpc,Node} ! {call,self(),Ref,Module,Function,Args},
		receive
		{returns,Ref,Value} ->
			Value
		after Timeout ->
			{badrpc,timeout}
		end;
	false ->
		{badrpc,not_alive}
	end.
	
server(_) ->
	receive
	{call,From,Ref,M,F,As} ->
		spawn(fun() ->
			Value = apply(M, F, As),
			From ! {returns,Ref,Value}
		end),
		server(state);
	{stop,From} ->
		From ! {stopped,self()}
	end.

test([_F]) -> ok.

%% EOF
