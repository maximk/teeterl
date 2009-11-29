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
-module(atoms_gen).

-export([compile_atoms/1, compile_atoms/5]).

-import(lists, [foreach/2]).
-import(io, [nl/1]).

-define(ATOM_INDEX_STARTS, 1).

%%#
%%# Standard atoms
%%################
%%#
%%false
%%true
%%
%%EXIT = A_EXIT__
%%...
%%

compile_atoms(LstFile, BifFile, DefsFile, IncFile, IndexFile) ->
	Atoms = atom_infos(LstFile, BifFile),
		
	dump_defs(Atoms, DefsFile),
	dump_inc(Atoms, IncFile),
	dump_index(Atoms, IndexFile),
	ok.

compile_atoms([LstFile,BifFile,DefsFile,IncFile,IndexFile]) ->
	compile_atoms(LstFile, BifFile, DefsFile, IncFile,IndexFile).

atom_infos(LstFile, BifFile) ->
	%% io:format("~p~n", [LstFile]),
	{ok,LstIn} = file:open(LstFile, read),
	Infos1 = atom_infos1(LstIn, []),
	file:close(LstIn),
	{ok,BifIn} = file:open(BifFile, read),
	Infos2 = atom_infos2(BifIn, Infos1),
	file:close(BifIn),
	{Atoms,_} = lists:mapfoldl(fun({A,S}, N) ->
		{{N,A,S},N+1}
	end, ?ATOM_INDEX_STARTS, lists:ukeysort(1, Infos2)),
	Atoms.

atom_infos1(In, Atoms) ->
	case io:get_line(In, '') of
	  eof ->
	    Atoms;
	  [10] ->
		atom_infos1(In, Atoms);
	  [$#|_] ->
	    atom_infos1(In, Atoms);
	  Line ->
		Line1 = string:strip(Line, right, 10),
	    case string:chr(Line1, $=) of
	     0 ->
			A = string:strip(Line1),
			Info = {list_to_atom(A), "A_" ++ string:to_upper(A)},
			sense_dups(Info, Atoms),
			atom_infos1(In, [Info|Atoms]);
	     N ->
			{A1, [$=|A2]} = lists:split(N-1, Line1),
			Info = {list_to_atom(string:strip(A1)), string:strip(A2)},
			sense_dups(Info, Atoms),
			atom_infos1(In, [Info|Atoms])
	    end
	end.

sense_dups({A,_}, Atoms) ->
	case lists:keysearch(A, 1, Atoms) of
	  false ->
	    ok;
	  _ ->
	    io:format("Atom '~w' appeared on the list more than once~n", [A]),
	    false
	end.

atom_infos2(In, Atoms) ->
	case io:get_line(In, '') of
	  eof -> Atoms;
	  [$#|_] -> atom_infos2(In, Atoms);
	  "\n" -> atom_infos2(In, Atoms);
	  L ->
		{M,F,_,_} = bif_spec(string:strip(L, right, $\n)),
		Atoms1 = add_atom(M, Atoms),
		atom_infos2(In, add_atom(F, Atoms1))
	end.

add_atom(A, Atoms) ->
	case lists:keymember(A, 1, Atoms) of
	  false ->
	    [{A, "A_" ++ string:to_upper(atom_to_list(A))}|Atoms];
	  true ->
	    Atoms
	end.

%% erlang:is_integer(T)		bif_is_integer1

bif_spec(L) ->
	%io:format("spec: ~p~n", [L]),
	[L1,Impl] = string:tokens(L, " \t"),
	{M,F,As} = case string:tokens(L1, "(") of
	  [L2,L3] ->
		[X,Y] = string:tokens(L2, ":"),
		{X,Y,string:tokens(string:strip(L3, right, $)), ",")};
	  [L2] ->
		[X,Y] = string:tokens(L2, ":"),
		{X,Y,[]}
	end,
	
	M1 = list_to_atom(M),
	F1 = list_to_atom(F),
	As1 = lists:map(fun list_to_atom/1, As),
	Impl1 = list_to_atom(Impl),
	
	{M1,F1,As1,Impl1}.

dump_defs(Atoms, DefsFile) ->
	{ok, DF} = file:open(DefsFile, write),
	foreach(fun({N, _, CName}) ->
		      io:format(DF, "#define ~s (tag_atom(~w))~n", [CName, N])
		    end, Atoms),
	file:close(DF).

dump_inc(Atoms, IncFile) ->
	{ok, IF} = file:open(IncFile, write),
	Count = length(Atoms),
	io:format(IF, "    ATOM_INC_SPACE = apr_array_make(ATOM_INC_POOL, ~p, sizeof(atom_info_t));~n", [Count]),
	io:format(IF, "    ATOM_INC_SPACE->nelts = ~p;~n", [Count]),
	io:format(IF, "    ATOM_INC_HASH = apr_hash_make(ATOM_INC_POOL);~n", []),
	nl(IF),
	io:format(IF, "    ATOM_INC_PTR = (atom_info_t *)ATOM_INC_SPACE->elts;~n", []),
	nl(IF),
	
	foreach(fun({N, Atom, _}) ->
		L = atom_to_list(Atom),
	    io:format(IF, "    ATOM_INC_PTR->index = ~w;~n", [N]),
	    io:format(IF, "    ATOM_INC_PTR->str = (cstr_t *)\"\\0~.8b~s\";~n", [length(L), L]),
	    io:format(IF, "    apr_hash_set(ATOM_INC_HASH, ATOM_INC_PTR->str->data, ATOM_INC_PTR->str->size, ATOM_INC_PTR);~n", []),
	    io:format(IF, "    ATOM_INC_PTR++;~n", []),
	    nl(IF)
	end, Atoms),

	file:close(IF).

dump_index(Atoms, IndexFile) ->
	{ok,F} = file:open(IndexFile, write),
	io:format(F, "-module(atoms).~n", []),
	io:format(F, "-export([index/1,std_atom/1]).~n~n", []),
	foreach(fun({N,Atom,_}) ->
		io:format(F, "index(~w) -> ~w;~n", [Atom,N])
	end, Atoms),
	io:format(F, "index(_) -> nonstd.~n~n", []),
	foreach(fun({N,Atom,_}) ->
		io:format(F, "std_atom(~w) -> ~w;~n", [N,Atom])
	end, Atoms),
	io:format(F, "std_atom(X) -> erlang:error(X).~n", []),
	io:format(F, "~n%%EOF~n", []),
	ok = file:close(F).

%% EOF
