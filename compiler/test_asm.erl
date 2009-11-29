%%
%%
%%

-module(test_asm).
-compile(export_all).

-import(lists,[mapfoldl/3,append/1,filter/2,keyfind/3,map/2,foldl/3]).

% a() -> 100 + 25.

sample() ->
	[{frame,0,1,0},
	 {set,{r,100},{literal,100}},
	 {set,{r,101},{literal,25}},
	 {add,{r,100},{r,101}},
	 {return,{r,100}}].

%% a() -> b().
%% b() -> c().
%% c() -> 3.
%
%sample() ->
%	[{frame,1,0},
%	 {set,{r,1},{integer,5}},
%	 {call,{l,1},0},
%	 {setret,{r,0}},
%	 {return,{r,0}},
%	 {label,1},
%	 {frame,1,0},
%	 {set,{r,1},{integer,7}},
%	 {call,{l,2},0},
%	 {setret,{r,0}},
%	 {return,{r,0}},
%	 {label,2},
%	 {set,{r,0},{integer,3}},
%	 {return,{r,0}}].
%
%% a() -> b(1) + 1.
%% b(_) -> c(2) + 2.
%% c(_) -> d(3) + 3.
%% d(_) -> e(4) + 4.
%% e(_) -> f(5) + 5.
%% f(_) -> 6.
%%
%
%sample() ->
%	[{frame,2,0},
%	 {set,{r,0},{integer,1}},
%	 {call,{l,1},1,{r,0}},
%	 {set,{r,1},{integer,1}},
%	 {add,{r,0},{r,1}},
%	 {return,{r,0}},
%	 {label,1},		%% b(X)
%	 {frame,2,0},
%	 {set,{r,0},{integer,2}},
%	 {call,{l,2},1,{r,0}},
%	 {set,{r,1},{integer,2}},
%	 {add,{r,0},{r,1}},
%	 {return,{r,0}},
%	 {label,2},		%% c(X)
%	 {frame,2,0},
%	 {set,{r,0},{integer,3}},
%	 {call,{l,3},1,{r,0}},
%	 {set,{r,1},{integer,3}},
%	 {add,{r,0},{r,1}},
%	 {return,{r,0}},
%	 {label,3},		%% d(X)
%	 {frame,2,0},
%	 {set,{r,0},{integer,4}},
%	 {call,{l,4},1,{r,0}},
%	 {set,{r,1},{integer,4}},
%	 {add,{r,0},{r,1}},
%	 {return,{r,0}},
%	 {label,4},		%% e(X)
%	 {frame,2,0},
%	 {set,{r,0},{integer,5}},
%	 {call,{l,5},1,{r,0}},
%	 {set,{r,1},{integer,5}},
%	 {add,{r,0},{r,1}},
%	 {return,{r,0}},
%	 {label,5},		%% f(X)
%	 {frame,0,0},
%	 {set,{r,0},{integer,6}},
%	 {return,{r,0}}].
%
write_test([File]) ->
	{Asm,{Li,_}} = mapfoldl(fun({label,L}, {Li,Off}) ->
		{{label,L},{[{L,Off}|Li],Off}};
	(A, {Li,Off}) ->
		Os = opcodes:asm(A),
		{Os, {Li,Off+length(Os)}}
	end, {[],0}, sample()),
	
	Code = append(filter(fun({label,_}) -> false; (_) -> true end, Asm)),
	
	Code1 = map(fun({l,L}) ->
		{_,Off} = keyfind(L, 1, Li),
		{at,Off};
	(X) ->
		X
	end, Code),
	
	{ok,Ou} = file:open(File, write),
	
	W = fun({at,Off}) ->
		io:format(Ou, "@~w", [Off]);
	(X) ->
		io:format(Ou, "~w", [X])
	end,
	
	foldl(fun(X, N) when N rem 8 =/= 0 ->
		W(X), io:format(Ou, " ", []),
		N+1;
	(X, N) ->
		W(X), io:nl(Ou),
		N+1
	end, 1, Code1),
	
	file:close(File).

%% EOF
