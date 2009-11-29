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
-module(x_disasm2).
-export([file/1,disasm/1]).

-import(lists, [reverse/1,keysearch/3]).

-include("compiler/xmodule.hrl").

file([File]) ->
	disasm(File).

disasm(File) ->
	{ok,Bin} = file:read_file(File),
	case binary_to_term(Bin) of
	#x{name=Name,version=?XVERSION,exports=Exps,code=Code,misc=Misc} ->
		{value,LineInfo} = keysearch(lineinfo, 1, Misc),
		io:format("' module '~w'~n", [Name]),
		output2(Code, Exps, LineInfo);
	#x{version=Ver} when Ver =/= ?XVERSION ->
		erlang:error({badversion,Ver});
	_ ->
		erlang:error({badfile,File})
	end.

output2(Asm, Exps, LineInfo) ->
	%% [{Off,Mnemo,Arg,Value,{Before,After}}]
	
	{Is,Labs} = instructions(Asm),

	Offs = [{Off,0} || {_,Off} <- Exps],
	Offs1 = [Off || {_,Off} <- Exps],
	
	%% compute stack levels, check for errors
	{Ls,Errs} = levels(Is, Offs),
	
	format(Is, lists:usort(Offs1++Labs), Ls, Errs, Exps, LineInfo).

format([{Off,_,_,_,_}|_]=Is, [Off|Labs], Ls, Errs, Exps, LineInfo) ->
	case lists:keysearch(Off, 2, Exps) of
	  {value,{{F,N},_}} ->
		io:format("~n~w/~w:~n", [F,N]);
	  false ->
		io:format("lab~w:~n", [Off])
	end,
	format(Is, Labs, Ls, Errs, Exps, LineInfo);
format([{Off,Mnemo,Arg,Value,_}|Is], Labs, Ls, Errs, Exps, LineInfo) ->
	case Arg of
	  term ->
		io:format("\t~w ~w", [Mnemo,Value]);
	  n ->
		io:format("\t~w ~w", [Mnemo,Value]);
	  e0 ->
		{X,Y,_} = Value,
		io:format("\t~w ~w:~w", [Mnemo,X,Y]);
	  e1 ->
		{X,Y,_} = Value,
		io:format("\t~w ~w:~w", [Mnemo,X,Y]);
	  e2 ->
		{X,Y,_} = Value,
		io:format("\t~w ~w:~w", [Mnemo,X,Y]);
	  e3 ->
		{X,Y,_} = Value,
		io:format("\t~w ~w:~w", [Mnemo,X,Y]);
	  e4 ->
		{X,Y,_} = Value,
		io:format("\t~w ~w:~w", [Mnemo,X,Y]);
	  label ->
		io:format("\t~w lab~w", [Mnemo,Value]);
	  mfn ->
		{X,Y,Z} = Value,
		io:format("\t~w ~w:~w/~w", [Mnemo,X,Y,Z]);
	  none ->
		io:format("\t~w", [Mnemo])
	end,

	Level = case keysearch(Off, 1, Ls) of
	{value,{_,L}} -> L;
	false -> unknown
	end,
	
	case keysearch(Off, 1, Errs) of
	{value,{_,E}} ->
		case source_line(Off, LineInfo) of
		false ->
			io:format("\t'~w --- ~s~n", [Level,E]);
		Line ->
			io:format("\t'~w --- [~w] ~s~n", [Level,Line,E])
		end;
	false ->
		io:format("\t'~w~n", [Level])
	end,
	
	format(Is, Labs, Ls, Errs, Exps, LineInfo);
format([], _, _, _, _, _) ->
	ok.

source_line(Off, [{Off1,Off2,Line}|_]) when Off >= Off1, Off =< Off2 -> Line;
source_line(Off, [_|LineInfo]) -> source_line(Off, LineInfo);
source_line(_, []) -> false.

instructions(Asm) -> instructions(Asm, [], 0, []).
instructions([C|Asm], Is, Off, Labs) ->
	case ops:disasm(C) of
	  {Mnemo,term,Levs} ->
		[{term,T}|Asm1] = Asm,
		I = {Off,Mnemo,term,T,Levs},
		instructions(Asm1, [I|Is], Off+2, Labs);
	  {Mnemo,n,{arg,After}} ->
		[N|Asm1] = Asm,
		I = {Off,Mnemo,n,N,{N,After}},
		instructions(Asm1, [I|Is], Off+2, Labs);
	  {Mnemo,n,{Before,arg}} ->
		[N|Asm1] = Asm,
		I = {Off,Mnemo,n,N,{Before,N}},
		instructions(Asm1, [I|Is], Off+2, Labs);
	  {Mnemo,n,Levs} ->
		[N|Asm1] = Asm,
		I = {Off,Mnemo,n,N,Levs},
		instructions(Asm1, [I|Is], Off+2, Labs);
	  {Mnemo,e0,Levs} ->
		[{bif,N}|Asm1] = Asm,
		I = {Off,Mnemo,e0,N,Levs},
		instructions(Asm1, [I|Is], Off+2, Labs);
	  {Mnemo,e1,Levs} ->
		[{bif,N}|Asm1] = Asm,
		I = {Off,Mnemo,e1,N,Levs},
		instructions(Asm1, [I|Is], Off+2, Labs);
	  {Mnemo,e2,Levs} ->
		[{bif,N}|Asm1] = Asm,
		I = {Off,Mnemo,e2,N,Levs},
		instructions(Asm1, [I|Is], Off+2, Labs);
	  {Mnemo,e3,Levs} ->
		[{bif,N}|Asm1] = Asm,
		I = {Off,Mnemo,e3,N,Levs},
		instructions(Asm1, [I|Is], Off+2, Labs);
	  {Mnemo,e4,Levs} ->
		[{bif,N}|Asm1] = Asm,
		I = {Off,Mnemo,e4,N,Levs},
		instructions(Asm1, [I|Is], Off+2, Labs);
	  {Mnemo,label,Levs} ->
		[{off,N}|Asm1] = Asm,
		I = {Off,Mnemo,label,N,Levs},
		instructions(Asm1, [I|Is], Off+2, [N|Labs]);
	  {Mnemo,mfn,Levs} ->
		[{atom,X},{atom,Y},Z|Asm1] = Asm,
		I = {Off,Mnemo,mfn,{X,Y,Z},Levs},
		instructions(Asm1, [I|Is], Off+4, Labs);
	  {Mnemo,none,Levs} ->
		I = {Off,Mnemo,none,undefined,Levs},
		instructions(Asm, [I|Is], Off+1, Labs)
	end;
instructions([], Is, _, Labs) -> {reverse(Is),Labs}.

levels(Is, Offs) -> levels(Is, Offs, [], []).
levels(Is, [{Off,N}|Offs], Ls, Errs) ->
	case keysearch(Off, 1, Ls) of
	{value,{_,N1}} when N =/= N1 ->
		E = io_lib:format("stack level mismatch for branches, ~w vs ~w", [N1,N]),
		levels(Is, Offs, Ls, [{Off,E}|Errs]);
	{value,_} ->
		levels(Is, Offs, Ls, Errs);
	false ->
		levels1(suffix(Is, Off), Is, Offs, N, Ls, Errs)
	end;
levels(_, [], Ls, Errs) -> {Ls,Errs}.

levels1([{_,_,n,N,{arg,A1,A2}}|_]=Is, Whole, Offs, L, Ls, Errs) ->
	levels2({N,A1,A2}, Is, Whole, Offs, L, Ls, Errs);
levels1([{_,_,n,N,{B,arg,arg}}|_]=Is, Whole, Offs, L, Ls, Errs) ->
	levels2({B,N,N}, Is, Whole, Offs, L, Ls, Errs);
levels1([{_,_,_,_,{B,A1,A2}}|_]=Is, Whole, Offs, L, Ls, Errs) ->
	levels2({B,A1,A2}, Is, Whole, Offs, L, Ls, Errs).

levels2({B,_,_}=Levs, [{Off,Mnemo,_,_,_}|_]=Is, Whole, Offs, L, Ls, Errs) when B > L ->
	E = io_lib:format("~w requires ~w value(s), only ~w expected", [Mnemo,B,L]),
	levels2(Levs, Is, Whole, Offs, B, Ls, [{Off,E}|Errs]);
levels2(Levs, [{Off,_,_,_,_}|_]=Is, Whole, Offs, L, Ls, Errs) ->
	levels3(Levs, Is, Whole, Offs, L, [{Off,L}|Ls], Errs).

levels3({B,noret,A2}, [{_,_,label,N,_}|_], Whole, Offs, L, Ls, Errs) ->
	levels(Whole, [{N,L+A2-B}|Offs], Ls, Errs);
levels3({_,noret,_}, _, Whole, Offs, _, Ls, Errs) ->
	levels(Whole, Offs, Ls, Errs);
levels3({B,A1,A2}, [{_,_,label,N,_}|Is], Whole, Offs, L, Ls, Errs) ->
	levels1(Is, Whole, [{N,L+A2-B}|Offs], L+A1-B, Ls, Errs);
levels3({B,A1,_}, [{_,_,_,_,_}|Is], Whole, Offs, L, Ls, Errs) ->
	levels1(Is, Whole, Offs, L+A1-B, Ls, Errs);
levels3(_, [], Whole, _, _, Ls, Errs) ->
	{Off,_,_,_,_} = lists:last(Whole),
	E = "instruction sequence extends beyond the boundary",
	{Ls,[{Off,E}|Errs]}.

suffix([{Off,_,_,_,_}|_]=Is, Off) -> Is;
suffix([{Off1,_,_,_,_}|_], Off) when Off1 > Off ->
	io:format("invalid offset referenced: ~w~n", [Off]),
	erlang:error(badfile);
suffix([_|Is], Off) -> suffix(Is, Off).

%% EOF
