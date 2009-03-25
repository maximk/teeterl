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
-module(opsgen2).
-export([file/1]).

-import(string, [strip/1,strip/2,strip/3]).
-import(string, [tokens/2]).
-import(lists, [foreach/2,reverse/1]).

file([OpTabFile,OpsErlFile]) ->
	{ok,In} = file:open(OpTabFile, read),
	OpTab = optab(In),
	file:close(In),
	
	{OpTab1,_} = lists:mapfoldl(fun(O, N) ->
		{{N,O},N+1}
	end, 0, OpTab),
	
	{ok,Out} = file:open(OpsErlFile, write),
	io:format(Out, "-module(ops).~n", []),
	io:format(Out, "-export([asm/1,disasm/1,switch/0]).~n~n", []),
	foreach(fun({C,{M,_,A,_}}) ->
		io:format(Out, "asm(~w) -> ~w;~n", [M,{C,A}]);
	({C,{M,_}}) ->
		io:format(Out, "asm(~w) -> ~w;~n", [M,{C,none}])
	end, OpTab1),
	io:format(Out, "asm(Op) -> exit({op,Op}).~n~n", []),
	foreach(fun({C,{M,_,A,Levs}}) ->
		io:format(Out, "disasm(~w) -> ~w;~n", [C,{M,A,Levs}]);
	({C,{M,Levs}}) ->
		io:format(Out, "disasm(~w) -> ~w;~n", [C,{M,none,Levs}])
	end, OpTab1),
	
	%% {13,{is_tuple_of_arity,[arity],n,{1,1,1}}}
	
	OpTab2 = lists:map(fun({C,{M,Ns,A,_}}) -> {C,{M,Ns,A}};
		({C,{M,_}}) -> {C,M} end, OpTab1),
	
	io:format(Out, "disasm(Op) -> exit({op,Op}).~n~n", []),
	io:format(Out, "switch() ->~n", []),
	io:format(Out, "~p.~n~n", [OpTab2]),
	io:format(Out, "%% EOF~n", []),
	file:close(Out).

optab(In) -> optab(In, []).
optab(In, OpTab) ->
	case io:get_line(In, '') of
	  eof ->
		reverse(OpTab);
	  "\n" ->
		optab(In, OpTab);
	  "#"++_ ->
		optab(In, OpTab);
	  L ->
		
		%% io:format("~p~n", [L]),
	    %% call		m,f,n/mfn	(...)
	    
	    case regexp:first_match(L, "\\(.*\\)") of
	    {match,Start1,Len1} ->
			StackSpec = string:substr(L, Start1+1, Len1-2),
			OldSpec = string:left(L, Start1-1),
			
			%io:format("~s:~s~n", [OldSpec,StackSpec]),
			Levs = stack_levels(StackSpec),
			
			case tokens(OldSpec, " \t") of
			  [M] ->
				optab(In, [{list_to_atom(M),Levs}|OpTab]);
			  [M,S0] ->
				[S1,A] = tokens(S0, "/"),
				Ns = tokens(S1, ","),
				Ns1 = lists:map(fun list_to_atom/1, Ns),
				optab(In, [{list_to_atom(M),Ns1,list_to_atom(A),Levs}|OpTab]);
			  _ ->
				io:format("bad spec: ~s~n", [OldSpec]),
				optab(In, OpTab)
			end;
		_ ->
			io:format("parsing error: ~s~n", [L]),
			optab(In, OpTab)
	    end
	end.

stack_levels(Spec) ->
	case regexp:first_match(Spec, "\\-\\>") of
	{match,Start1,Len1} ->
		Before = string:left(Spec, Start1-1),
		After2 = string:right(Spec, length(Spec)-Start1-Len1+1),
		
		case string:chr(After2, $|) of
		0 ->
			B = stack(Before),
			A = stack(After2),		
			{B,A,A};
		_ ->
			[After0,After1] = string:tokens(After2, "|"),
			B = stack(Before),
			A0 = stack(After0),
			A1 = stack(After1),
			{B,A0,A1}
		end;
	_ ->
		io:format("bad stack use spec: ~s~n", [Spec]),
		{0,0}
	end.

stack(X) ->
	case string:tokens(X, " \t") of
	["#"] -> noret;
	["*"] -> arg;
	Ts -> length(Ts)
	end.

%% EOF
