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
-module(stdio).
-export([server/0]).

-export([add_hook/2,remove_hook/1]).

-record(is, {stdout=[],hooks=[]}).

server() ->
	server0(#is{}).

server0(St0) ->
	receive
	{io_request,ReplyTo,_Me,{put_chars,M,F,As}} ->
	
		S = case catch apply(M, F, As) of
		{'EXIT',_}=E ->
			erlang:display(E),
			[];
		X ->
			print_hooked_iolist(X, St0),
			X
		end,
		
		ReplyTo ! {io_reply,self(),ok},
		server0(St0#is{stdout=[S|St0#is.stdout]});

	{io_request,ReplyTo,_Me,{put_chars,Chars}} ->
		print_hooked_iolist(Chars, St0),
		ReplyTo ! {io_reply,self(),ok},
		server0(St0#is{stdout=[Chars|St0#is.stdout]});
	
	{io_request,ReplyTo,_Me,flush} ->
		ReplyTo ! {io_reply,self(),lists:reverse(St0#is.stdout)},
		server0(St0#is{stdout=[]});
	
	{add_hook,From,Tag,Fun} when is_function(Fun, 1) ->
		Hooks1 = [{Tag,Fun}|St0#is.hooks],
		From ! {ok,self()},
		server0(St0#is{hooks=Hooks1});
	
	{remove_hook,From,Tag} ->
		Hooks1 = lists:keydelete(Tag, 1, St0#is.hooks),
		From ! {ok,self()},
		server0(St0#is{hooks=Hooks1});
	
	{stop,From} ->
		From ! {stopped,self()}
	end.

print_hooked_iolist(S, #is{hooks=Hooks}) ->
	lists:foreach(fun({_,F}) ->
	  case (catch F(S)) of
	  {'EXIT',Err} ->
		erlang:display({stdio_hook_failed,Err});
	  _ ->
		ok
	  end
	end, Hooks),
	io:print_iolist(S).

%%---------------------------------------------------

add_hook(Tag, Fun) when is_function(Fun,1) ->
	stdio ! {add_hook,self(),Tag,Fun},
	receive {ok,_} -> ok end.

remove_hook(Tag) ->
	stdio ! {remove_hook,self(),Tag},
	receive {ok,_} -> ok end.

%% EOF
