-module(bifs_gen).
-export([builtins/1,builtins/4]).

-import(lists, [foreach/2,map/2]).
-import(string, [tokens/2,strip/3]).

builtins([BifTabFile,BifHdrFile,BifListFile,BifErlFile]) ->
	builtins(BifTabFile, BifHdrFile, BifListFile, BifErlFile).

builtins(BifTabFile, BifHdrFile, BifListFile, BifErlFile) ->
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
	%% ...
	%%
	%% #endif
	
	{ok,Hdr} = file:open(BifHdrFile, write),
	io:format(Hdr, "#ifndef BIF_H~n", []),
	io:format(Hdr, "#define BIF_H~n", []),
	io:nl(Hdr),
	io:format(Hdr, "#include \"proc.h\"~n", []),
	io:format(Hdr, "#include \"term.h\"~n", []),
	io:nl(Hdr),
	foreach(fun({N,M,F,As,Impl}) ->
		io:format(Hdr, "// ~w:~w/~w [~w]~n", [M,F,length(As),N]),
		ArgStr = string:join(["term_t " ++ atom_to_list(A) || A <- As] ++ ["proc_t *proc"], ", "),
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
		
		%%
		%% NB: the proper way would be to consult atoms.tab;
		%% for now we just plug in a few exceptions
		%%
		
		M1 = atom_define(M),
		F1 = atom_define(F),
		
		io:format(Lst, "\t{{~s, ~s, ~w}, (bifN_t) ~w},~n", [M1,F1,Arity,Impl])
	end, Bifs1),
	file:close(Lst),

%% -module(bif).
%% -export([is_builtin/3]).
%%
%% is_builtin(erlang, is_integer, 1) -> true;
%% ...
%% is_builtin(_, _, _) -> false.
%%
%% %% EOF

	{ok,Be} = file:open(BifErlFile, write),
	io:format(Be, "-module(bifs).~n", []),
	io:format(Be, "-export([is_builtin/3,bif_index/3,bif_spec/1]).~n~n", []),
	foreach(fun({_,M,F,As,_}) ->
		io:format(Be, "is_builtin(~w, ~w, ~w) -> true;~n", [M,F,length(As)])
	end, Bifs1),
	io:format(Be, "is_builtin(_, _, _) -> false.~n~n", []),
	foreach(fun({N,M,F,As,_}) ->
		io:format(Be, "bif_index(~w, ~w, ~w) -> ~w;~n", [M,F,length(As),N])
	end, Bifs1),
	io:format(Be, "bif_index(_, _, _) -> none.~n~n", []),
	foreach(fun({N,M,F,As,_}) ->
		io:format(Be, "bif_spec(~w) -> {~w, ~w, ~w};~n", [N,M,F,length(As)])
	end, Bifs1),
	io:format(Be, "bif_spec(_) -> none.~n", []),
	io:format(Be, "~n%%EOF~n", []),
	file:close(Be).

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

atom_define('!') ->
	"ASEND__";
atom_define('++') ->
	"APLUSPLUS__";
atom_define('--') ->
	"AMINUSMINUS__";
atom_define(A) ->
	"A_" ++ string:to_upper(atom_to_list(A)).

%%EOF
