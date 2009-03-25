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
-module(x_asm2).
-export([module/2]).

-import(lists, [reverse/1]).

-include("compiler/xmodule.hrl").

module({Mod,Exps,Attrs,Asm0}, _Opts) ->
	%io:format("~p~n", [Asm0]),
	{Asm1,Ls,LineInfo,FuncOffs} = flat(Asm0),
	
	LineInfo1 = source(lists:keysort(1, LineInfo)),
	%% io:format("LineInfo1=~p~n", [LineInfo1]),

	Exps1 = lists:map(fun({_,_}=L)->
		{value,{_,Off}} = lists:keysearch(L, 1, Ls),
		{L,Off}
	end, Exps),
	
	Asm2 = lists:map(fun({ref, L}) ->
		case lists:keysearch(L, 1, Ls) of
		{value,{_,Off}} ->
			{off,Off};
		false ->
			io:format("~w: label not defined: ~w~n", [Mod,L]),
			{off,0}
		end;
	(X) ->
		X
	end, Asm1),
	
	%%
	%%	LineInfo:
	%%	[{Off1,Off2,Line}]
	%%
	
	%%
	%%	module layout:
	%%	{x,Name,100,Exps,Attrs,Code,[{lieinfo,LineInfo},{funcoffs,FuncOffs}]}
	%%
	
	{ok,#x{name=Mod,
		version=?XVERSION,
		exports=Exps1,
		attrs=Attrs,
		code=Asm2,
		misc=[{lineinfo,LineInfo1},
			  {funcoffs,FuncOffs}]}}.
	
	%{ok,{Mod,Exps1,Attrs,Asm2,LineInfo1}}.

source([{Off,Line}|LineInfo]) -> source(LineInfo, [{Off,Off,Line}]).
source([{Off,Line}|LineInfo], [{Off1,_,Line}|LineInfo1]) ->
	source(LineInfo, [{Off1,Off,Line}|LineInfo1]);
source([{Off,Line}|LineInfo], [{Off1,_,Line1}|LineInfo1]) ->
	source(LineInfo, [{Off,Off,Line},{Off1,Off-1,Line1}|LineInfo1]);
source([], LineInfo1) -> reverse(LineInfo1).

flat(Asm0) -> flat(Asm0, [], [], [], []).
flat([{l,_,{F,N}=L}|Asm0], Ls, LineInfo, FuncOffs, Asm1) when is_atom(F), is_integer(N) ->
	Off = length(Asm1),
	flat(Asm0, [{L,Off}|Ls], LineInfo, [{F,N,Off}|FuncOffs], Asm1);
flat([{l,_,L}|Asm0], Ls, LineInfo, FuncOffs, Asm1) ->
	Off = length(Asm1),
	flat(Asm0, [{L,Off}|Ls], LineInfo, FuncOffs, Asm1);
flat([{Op,[Line|_]}|Asm0], Ls, LineInfo, FuncOffs, Asm1) when integer(Line) ->
	Off = length(Asm1),
	{N,none} = ops:asm(Op),
	flat(Asm0, Ls, [{Off,Line}|LineInfo], FuncOffs, [N]++Asm1);
flat([{Op,_}|Asm0], Ls, LineInfo, FuncOffs, Asm1) ->
	{N,none} = ops:asm(Op),
	flat(Asm0, Ls, LineInfo, FuncOffs, [N]++Asm1);
flat([{Op,[Line|_],A}|Asm0], Ls, LineInfo, FuncOffs, Asm1) when integer(Line) ->
	Off = length(Asm1),
	{N,T} = ops:asm(Op),
	flat(Asm0, Ls, [{Off,Line}|LineInfo], FuncOffs, reverse(args(T, A))++[N]++Asm1);
flat([{Op,_,A}|Asm0], Ls, LineInfo, FuncOffs, Asm1) ->
	{N,T} = ops:asm(Op),
	flat(Asm0, Ls, LineInfo, FuncOffs, reverse(args(T, A))++[N]++Asm1);
flat([], Ls, LineInfo, FuncOffs, Asm1) ->
	{reverse(Asm1),Ls,LineInfo,reverse(FuncOffs)}.

args(mfn, {M,F,N}) ->
	[{atom,M},{atom,F},N];
args(term, T) ->
	[{term,T}];
args(label, L) ->
	[{ref, L}];
args(e0, N) ->
	[{bif, N}];
args(e1, N) ->
	[{bif, N}];
args(e2, N) ->
	[{bif, N}];
args(e3, N) ->
	[{bif, N}];
args(e4, N) ->
	[{bif, N}];
args(_, N) ->
	[N].

%% EOF
