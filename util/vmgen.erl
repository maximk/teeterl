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
-module(vmgen).
-export([run_cases/1,bif_tab/1,bif_list/1,addmods/1,declmods/1]).

-import(string, [strip/1,strip/2,strip/3,tokens/2]).
-import(lists, [foldl/3,foreach/2,keysearch/3,map/2]).

run_cases([SwitchFile|SnipFiles]) ->
	Snips = foldl(fun(File, Snips) ->
		{ok,In} = file:open(File, read),
		Snips1 = snips(In, []),
		file:close(In),
		[{Op,Body,File} || {Op,Body} <- Snips1] ++ Snips
	end, [], SnipFiles),
	
	%io:format("Snips=~p~n", [Snips]),
	
	{ok,Out} = file:open(SwitchFile, write),
	foreach(fun({C,{M,[X],term}}) ->
		case keysearch(M, 1, Snips) of
		  {value,{_,Body,File}} ->
			io:format(Out, "case ~w: // ~w (~s)~n", [C,M,File]),
			io:format(Out, "{~n", []),
			io:format(Out, "\tterm_t ~w = (term_t) *proc->ip++;~n", [X]),
			io:format(Out, "~s~n\tbreak;~n", [Body]),
			io:format(Out, "}~n", []);
		  false ->
			io:format("no implementation found for '~w'~n", [M])
		end;
	({C,{M,[X],n}}) ->
		case keysearch(M, 1, Snips) of
		  {value,{_,Body,File}} ->
			io:format(Out, "case ~w: // ~w (~s)~n", [C,M,File]),
			io:format(Out, "{~n", []),
			io:format(Out, "\tapr_int32_t ~w = (apr_int32_t) *proc->ip++;~n", [X]),
			io:format(Out, "~s~n\tbreak;~n", [Body]),
			io:format(Out, "}~n", []);
		  false ->
			io:format("no implementation found for '~w'~n", [M])
		end;
	({C,{M,[X],label}}) ->
		case keysearch(M, 1, Snips) of
		  {value,{_,Body,File}} ->
			io:format(Out, "case ~w: // ~w (~s)~n", [C,M,File]),
			io:format(Out, "{~n", []),
			io:format(Out, "\tcelem_t *~w = (celem_t *) *proc->ip++;~n", [X]),
			io:format(Out, "~s~n\tbreak;~n", [Body]),
			io:format(Out, "}~n", []);
		  false ->
			io:format("no implementation found for '~w'~n", [M])
		end;
	({C,{M,[X,Y,Z],mfn}}) ->
		case keysearch(M, 1, Snips) of
		  {value,{_,Body,File}} ->
			io:format(Out, "case ~w: // ~w (~s)~n", [C,M,File]),
			io:format(Out, "{~n", []),
			io:format(Out, "\tterm_t ~w = (term_t) *proc->ip++;~n", [X]),
			io:format(Out, "\tterm_t ~w = (term_t) *proc->ip++;~n", [Y]),
			io:format(Out, "\tint ~w = (int) *proc->ip++;~n", [Z]),
			io:format(Out, "~s~n\tbreak;~n", [Body]),
			io:format(Out, "}~n", []);
		  false ->
			io:format("no implementation found for '~w'~n", [M])
		end;
	({C,{M,[X],e0}}) ->
		case keysearch(M, 1, Snips) of
		  {value,{_,Body,File}} ->
			io:format(Out, "case ~w: // ~w (~s)~n", [C,M,File]),
			io:format(Out, "{~n", []),
			io:format(Out, "\tbif0_t ~w = (bif0_t) *proc->ip++;~n", [X]),
			io:format(Out, "~s~n\tbreak;~n", [Body]),
			io:format(Out, "}~n", []);
		  false ->
			io:format("no implementation found for '~w'~n", [M])
		end;
	({C,{M,[X],e1}}) ->
		case keysearch(M, 1, Snips) of
		  {value,{_,Body,File}} ->
			io:format(Out, "case ~w: // ~w (~s)~n", [C,M,File]),
			io:format(Out, "{~n", []),
			io:format(Out, "\tbif1_t ~w = (bif1_t) *proc->ip++;~n", [X]),
			io:format(Out, "~s~n\tbreak;~n", [Body]),
			io:format(Out, "}~n", []);
		  false ->
			io:format("no implementation found for '~w'~n", [M])
		end;
	({C,{M,[X],e2}}) ->
		case keysearch(M, 1, Snips) of
		  {value,{_,Body,File}} ->
			io:format(Out, "case ~w: // ~w (~s)~n", [C,M,File]),
			io:format(Out, "{~n", []),
			io:format(Out, "\tbif2_t ~w = (bif2_t) *proc->ip++;~n", [X]),
			io:format(Out, "~s~n\tbreak;~n", [Body]),
			io:format(Out, "}~n", []);
		  false ->
			io:format("no implementation found for '~w'~n", [M])
		end;
	({C,{M,[X],e3}}) ->
		case keysearch(M, 1, Snips) of
		  {value,{_,Body,File}} ->
			io:format(Out, "case ~w: // ~w (~s)~n", [C,M,File]),
			io:format(Out, "{~n", []),
			io:format(Out, "\tbif3_t ~w = (bif3_t) *proc->ip++;~n", [X]),
			io:format(Out, "~s~n\tbreak;~n", [Body]),
			io:format(Out, "}~n", []);
		  false ->
			io:format("no implementation found for '~w'~n", [M])
		end;
	({C,{M,[X],e4}}) ->
		case keysearch(M, 1, Snips) of
		  {value,{_,Body,File}} ->
			io:format(Out, "case ~w: // ~w (~s)~n", [C,M,File]),
			io:format(Out, "{~n", []),
			io:format(Out, "\tbif4_t ~w = (bif4_t) *proc->ip++;~n", [X]),
			io:format(Out, "~s~n\tbreak;~n", [Body]),
			io:format(Out, "}~n", []);
		  false ->
			io:format("no implementation found for '~w'~n", [M])
		end;
	({C,M}) ->
		case keysearch(M, 1, Snips) of
		  {value,{_,Body,File}} ->
			io:format(Out, "case ~w: // ~w (~s)~n", [C,M,File]),
			io:format(Out, "{~n", []),
			io:format(Out, "~s~n\tbreak;~n", [Body]),
			io:format(Out, "}~n", []);
		  false ->
			io:format("no implementation found for '~w'~n", [M])
		end
	end, ops:switch()),
	file:close(Out).

snips(In, Snips) ->
	case io:get_line(In, '') of
	  eof ->
		Snips;
	  "///" ++ L ->
		snips1(strip(L, right, $\n), In, Snips);
	  _ ->
		snips(In, Snips)
	end.

snips1(L, In, Snips) ->
	Op = list_to_atom(hd(tokens(strip(L), "("))),
	snips2(Op, In, [], Snips).

snips2(Op, In, Body, Snips) ->
	case io:get_line(In, '') of
	  eof ->
		add_snip(Op, Body, Snips);
	  "///" ++ L ->
		snips1(strip(L, right, $\n), In, add_snip(Op, Body, Snips));
	  L ->
		snips2(Op, In, Body++L, Snips)
	end.

add_snip(Op, Body, Snips) ->
	case keysearch(Op, 1, Snips) of
	  false ->
		[{Op,strip(Body, right, $\n)}|Snips];
	  _ ->
	    io:format("multiple implementations found for '~w'~n", [Op]),
	    Snips
	end.

%%
%%	BIFs
%%

bif_tab([BifTabFile,BifErlFile]) ->
	{ok,In} = file:open(BifTabFile, read),
	Bifs = bifs(In),
	file:close(In),

	{Bifs1,_} = lists:mapfoldl(fun({M,F,As,Impl}, N) ->
		{{N,M,F,As,Impl},N+1}
	end, 0, Bifs),
	%% io:format("~p~n", [Bifs1]),	
	
	{ok,Out} = file:open(BifErlFile, write),
	io:format(Out, "-module(bif).~n", []),
	io:format(Out, "-export([is_builtin/3]).~n", []),
	io:nl(Out),
	foreach(fun({_,M,F,As,_}) ->
		io:format(Out, "is_builtin(~w, ~w, ~w) -> true;~n", [M,F,length(As)])
	end, Bifs1),
	io:format(Out, "is_builtin(_, _, _) -> false.~n", []),
	io:nl(Out),
	%foreach(fun({N,M,F,As,_}) ->
	%	io:format(Out, "bif_index(~w, ~w, ~w) -> ~w;~n", [M,F,length(As),N])
	%end, Bifs1),
	%io:format(Out, "bif_index(_, _, _) -> false.~n", []),
	%io:nl(Out),
	%foreach(fun({N,M,F,As,_}) ->
	%	io:format(Out, "bif_name(~w) -> {~w, ~w, ~w};~n", [N,M,F,length(As)])
	%end, Bifs1),
	%io:format(Out, "bif_name(_) -> false.~n", []),
	io:format(Out, "~n%% EOF~n", []),
	file:close(Out).

bif_list([BifTabFile,BifHdrFile,BifListFile]) ->
	{ok,In} = file:open(BifTabFile, read),
	Bifs = bifs(In),
	file:close(In),

	{Bifs1,_} = lists:mapfoldl(fun({M,F,As,Impl}, N) ->
		{{N,M,F,As,Impl},N+1}
	end, 0, Bifs),
	%% io:format("~p~n", [Bifs1]),	
	
	%% #ifndef BIF_H
	%% #define BIF_H
	%% 
	%% #include "proc.h"
	%% #include "term.h"
	%%
	%% #define result(r)	proc_bif_result(r)
	%% 
	%% // a generic function pointer to avoid warnings
	%% typedef term_t (*bifN_t)(process_t *ctx);
	%% 
	%% typedef term_t (*bif0_t)(process_t *ctx);
	%% typedef term_t (*bif1_t)(term_t a, process_t *ctx);
	%% typedef term_t (*bif2_t)(term_t a, term_t b, process_t *ctx);
	%% typedef term_t (*bif3_t)(term_t a, term_t b, term_t c, process_t *ctx);
	%% typedef term_t (*bif4_t)(term_t a, term_t b, term_t c, term_t d, process_t *ctx);
	%%
	%% #endif
	
	{ok,Hdr} = file:open(BifHdrFile, write),
	io:format(Hdr, "#ifndef BIF_H~n", []),
	io:format(Hdr, "#define BIF_H~n", []),
	io:nl(Hdr),
	io:format(Hdr, "#include \"proc.h\"~n", []),
	io:format(Hdr, "#include \"term.h\"~n", []),
	io:format(Hdr, "#include \"atom.h\"~n", []),
	io:nl(Hdr),
	io:format(Hdr, "#define result(r)\tproc_bif_result(ctx, (r))~n", []),
	io:nl(Hdr),
	io:format(Hdr, "// a generic function pointer to avoid warnings~n", []),
	io:format(Hdr, "typedef term_t (*bifN_t)(process_t *ctx);~n", []),
	io:nl(Hdr),
	io:format(Hdr, "typedef term_t (*bif0_t)(process_t *ctx);~n", []),
	io:format(Hdr, "typedef term_t (*bif1_t)(term_t a, process_t *ctx);~n", []),
	io:format(Hdr, "typedef term_t (*bif2_t)(term_t a, term_t b, process_t *ctx);~n", []),
	io:format(Hdr, "typedef term_t (*bif3_t)(term_t a, term_t b, term_t c, process_t *ctx);~n", []),
	io:format(Hdr, "typedef term_t (*bif4_t)(term_t a, term_t b, term_t c, term_t d, process_t *ctx);~n", []),
	io:nl(Hdr),
	foreach(fun({N,M,F,As,Impl}) ->
		io:format(Hdr, "// ~w:~w/~w [~w]~n", [M,F,length(As),N]),
		ArgStr = string:join(["term_t " ++ atom_to_list(A) || A <- As] ++ ["process_t *ctx"], ", "),
		io:format(Hdr, "term_t ~w(~s);~n", [Impl,ArgStr])
	end, Bifs1),
	io:nl(Hdr),
	io:format(Hdr, "#endif~n", []),
	file:close(Hdr),

	%% struct builtin_t {
	%%	struct bif_key_t {
	%%		term_t mod;
	%%		term_t fun;
	%%		int arity;
	%%	} key;
	%%
	%%	bifN_t entry;
	%% };

	{ok,Lst} = file:open(BifListFile, write),
	foreach(fun({_,M,F,As,Impl}) ->
		Arity = length(As),
		M1 = "A_" ++ string:to_upper(atom_to_list(M)),
		F1 = "A_" ++ string:to_upper(atom_to_list(F)),
		io:format(Lst, "\t{{~s, ~s, ~w}, (bifN_t) ~w},~n", [M1,F1,Arity,Impl])
	end, Bifs1),
	file:close(Lst).

%% -module(bif).
%% -export([is_builtin/2]).
%%
%% is_builtin(erlang, is_integer, 1) -> true;
%% ...
%% is_builtin(_, _, _) -> false.
%%
%% %% EOF

bifs(In) -> bifs(In, []).
bifs(In, Bifs) ->
	case io:get_line(In, '') of
	  eof -> Bifs;
	  [$#|_] -> bifs(In, Bifs);
	  "\n" -> bifs(In, Bifs);
	  L -> bifs(In, [bif_spec(strip(L, right, $\n))|Bifs])
	end.

%% erlang:is_integer(T)		bif_is_integer1

bif_spec(L) ->
	%% io:format("spec: ~p~n", [L]),
	[L1,Impl] = tokens(L, " \t"),
	{M,F,As} = case tokens(L1, "(") of
	  [L2,L3] ->
		[X,Y] = tokens(L2, ":"),
		{X,Y,tokens(strip(L3, right, $)), ",")};
	  [L2] ->
		[X,Y] = tokens(L2, ":"),
		{X,Y,[]}
	end,
	
	M1 = list_to_atom(M),
	F1 = list_to_atom(F),
	As1 = map(fun list_to_atom/1, As),
	Impl1 = list_to_atom(Impl),
	
	{M1,F1,As1,Impl1}.

addmods([IncFile|Mods]) ->
	{ok,Out} = file:open(IncFile, write),
	foreach(fun(M) ->
		NameExt = lists:last(string:tokens(M, "/")),
		[Name,_Ext] = string:tokens(NameExt, "."),
		io:format(Out, "\tteeterl_add_mod(\"\\0~.8b~s\", ~s_bin_data, ~s_bin_size);~n", [length(Name),Name,Name,Name])
	end, Mods),	
	file:close(Out).

declmods([DeclFile|Mods]) ->
	{ok,Decl} = file:open(DeclFile, write),
	
	io:format(Decl, "#ifndef MODBIN_H~n", []),
	io:format(Decl, "#define MODBIN_H~n~n", []),
	%%io:format(Decl, "#include \"xmod.h\"~n~n", []),

	foreach(fun(M) ->
		NameExt = lists:last(string:tokens(M, "/")),
		[Name,_Ext] = string:tokens(NameExt, "."),
		%%extern unsigned int lists_bin_size;
		%%extern unsigned char lists_bin_data[];
		io:format(Decl, "extern unsigned int ~s_bin_size;~n", [Name]),
		io:format(Decl, "extern unsigned char ~s_bin_data[];~n", [Name])
	end, Mods),

	io:format(Decl, "~n#endif~n", []),
	file:close(Decl).

%% EOF
