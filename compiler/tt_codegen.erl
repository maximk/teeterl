%%
%%
%%

-module(tt_codegen).
-export([module/2,format_error/1]).

-import(lists, [reverse/1,map/2,mapfoldl/3,sort/1,sort/2,member/2,zip/2,partition/2]).
-import(lists, [keysort/2,keyfind/3,keydelete/3,keytake/3,seq/2,foldl/3]).
-import(lists, [keymember/3,split/2,delete/2,flatmap/2,concat/1,filter/2]).
-import(lists, [nth/2,all/2,dropwhile/2,usort/1]).
-import(erlang, [max/2]).

-include("v3_life.hrl").
-include("os.hrl").

-define(NUM_REGS, 256).

-define(BOPTS_SIGNED, 1).
-define(BOPTS_LITTLE, 2).

-define(Tmp, '$1').
-define(Tmp1, '$2').

-record(cg, {
	lcount,
	break,
	recv_loop,
	heap_needed,
	functable = gb_trees:empty(),
	lambdas = []
}).

-record(sr, {
	unused,			%% I
	hots = [],		%% {I,V}
	colds = [],		%% {I,V,N}
	stack = []		%% {N,V}
}).

-record(x, {
	s,
	in = [],
	out = [],
	i = none,
	hn = 0
}).

format_error(not_enough_registers) ->
	io_lib:format("not enough registers to compile").

module({Mod,Exp0,Attr,Forms}, Options) ->	%% {ok,Code}
	put(?MODULE, Options),
	{Asm,St} = flatmapfoldl(fun(Form, St) -> function(Form, St) end, #cg{lcount=1}, Forms),
	erase(?MODULE),
	
	Exp = map(fun(NameArity) ->
		{value,L} = gb_trees:lookup(NameArity, St#cg.functable),
		{NameArity,L}
	end, Exp0),
	
	%% Unused funs not get listed in St#cg.lambdas,
	%% Thus St#cg.lambdas should be updated with
	%% dummy entries for such funs

	Lambdas = dummy_funs(St#cg.lambdas),
	
	{ok,{Mod,Exp,Lambdas,Attr,Asm}}.

dummy_funs(Funs) ->	%% [{Uniq,L}]
	dummy_funs(keysort(1, Funs), 0, []).

dummy_funs([{N,Uniq,L}|Funs], N, Snuf) ->
	dummy_funs(Funs, N+1, [{Uniq,L}|Snuf]);
dummy_funs([], _, Snuf) ->
	reverse(Snuf);
dummy_funs(Funs, N, Snuf) ->
	dummy_funs(Funs, N+1, [{0,0}|Snuf]).

function({function,Name,Arity,Hvs,Les,Vdb}, St0) ->	%% {Asm,St}
	try
	Unused = seq(0, ?NUM_REGS-1),
	Sr0 = clear_dead(easy_new_vars(Hvs, #sr{unused=Unused}), 0, Vdb),

	{EntryLabel,St1} = local_func_label({Name,Arity}, St0),
	{Is,Sr,St2} = flatmapfoldl2(fun(Le, Sr, St) ->
		cg(Le, Vdb, Sr, St)
	end, Sr0, St1#cg{heap_needed=0}, Les),
	Sline = source(hd(Les)),
	Fsize = length(Sr#sr.stack),
	Hneeded = St2#cg.heap_needed,
	{[{label,EntryLabel}]
		++ Sline ++
	 [{frame,Arity,{literal,Fsize},{literal,Hneeded}}]
		++ Is,St2}
    catch
	Class:Error ->
	    Stack = erlang:get_stacktrace(),
	    io:fwrite("Function: ~w/~w\n", [Name,Arity]),
	    erlang:raise(Class, Error, Stack)
    end.

get_line([Line|_]) when is_integer(Line) -> Line;
get_line([_|T]) -> get_line(T);
get_line([]) -> none.
    
get_file([{file,File}|_]) -> File;
get_file([_|T]) -> get_file(T);
get_file([]) -> none.

source(#l{a=Anno}) ->
	File = get_file(Anno),
	Line = get_line(Anno),
	if File =/= none, Line =/= none -> [{source,File,Line}];
			true -> [] end.

cg(Le, Vdb, Sr, St) ->	%% {Is,Sr,St}
	cg(Le#l.ke, Le, Vdb, Sr, St).

cg({match,B,Rvs}, Le, Vdb, Sr, St) ->
	match_cg(B, Rvs, Le, Vdb, Sr, St);
cg({guard_match,B,Rvs}, Le, Vdb, Sr, St) ->
	guard_match_cg(B, Rvs, Le, Vdb, Sr, St);
cg({match_fail,R}, Le, Vdb, Sr, St) ->
	match_fail_cg(R, Le, Vdb, Sr, St);
cg({bif,Spec,As,Rs}, Le, Vdb, Sr, St) ->
	bif_cg(Spec, As, Rs, Le, Vdb, Sr, St);
cg({gc_bif,Bif,As,Rs}, Le, Vdb, Sr, St) ->
	gc_bif_cg(Bif, As, Rs, Le, Vdb, Sr, St);
cg({enter,S,As}, Le, Vdb, Sr0, St0) ->
	{Is,Sr,St} = enter_cg(S, As, Le, Vdb, Sr0, St0),
	{Is,clear_dead(Sr, Le#l.i, Vdb),St};	%% clear_dead removed from block
cg({call,S,As,Rs}, Le, Vdb, Sr0, St0) ->
	{Is,Sr,St} = call_cg(S, As, Rs, Le, Vdb, Sr0, St0),
	{Is,clear_dead(Sr, Le#l.i, Vdb),St};	%% clear_dead removed from block	
cg({set,Vs,E}, Le, Vdb, Sr, St) ->
	set_cg(Vs, E, Le, Vdb, Sr, St);
cg({receive_loop,Timeout,Msg,B,Tes,Rs}, Le, Vdb, Sr, St) ->
	recv_loop_cg(Timeout, Msg, B, Tes, Rs, Le, Vdb, Sr, St);
cg(receive_accept, Le, Vdb, Sr, St) ->
	recv_accept_cg(Le, Vdb, Sr, St);
cg(receive_next, Le, Vdb, Sr, St) ->
	recv_next_cg(Le, Vdb, Sr, St);
cg({'catch',Es,Vv}, Le, Vdb, Sr, St) ->
	catch_cg(Es, Vv, Le, Vdb, Sr, St);
cg({break,Es}, Le, Vdb, Sr, St) ->
	break_cg(Es, Le, Vdb, Sr, St);
cg({guard_break,Es,Locked}, Le, Vdb, Sr, St) ->
	guard_break_cg(Es, Locked, Le, Vdb, Sr, St);
cg({return,Rs}, Le, Vdb, Sr, St) ->
	return_cg(Rs, Le, Vdb, Sr, St).

operand({var,V}) -> V;
operand({integer,I}) -> {literal,I};
operand({float,F}) -> {literal,F};
operand({atom,A}) ->
	case atoms:index(A) of
	nonstd -> {literal,A};
	N -> {atom,N}
	end;
operand({literal,_}=X) -> X.

match_cg(B, Rvs, Le, Vdb, Sr0, St0) ->	%% {Is,Sr,St}
	{Bl,St} = new_label(St0),

	{Bis,Sr,St1} = match_cg(B, none, Sr0, St#cg{break=Bl}),
	
	%% bind values in registers to variables, in sequence
	Sr1 = easy_new_vars(Rvs, Sr),	
	%ok = io:format("m6(~w):Sr1=~w~nVdb=~w~n", [Le#l.i,Sr1,Vdb]),
	
	{Bis ++ [{label,Bl}],
		clear_dead(Sr1, Le#l.i, Vdb),
		St1#cg{break=St0#cg.break}}.

%% {match_fail,{function_clause,[{var,cor0}]}}
match_fail_cg(S, Le, Vdb, Sr0, St0) ->
	{Is,_Sr,St} = fail_match(S, Le, Vdb, Sr0, St0),
	{_,Sr} = cg_call_args([], [], Le#l.i, Vdb, Sr0),
	{Is,Sr,St}.

fail_match({function_clause,Vvs}, _Le, Vdb, Sr0, St0) ->
	[{var,V}|Svv] = reverse(Vvs),
	Xs = [#x{s={consup_nil,V,V},in=[V],out=[],hn=?HSIZE_CONS}] ++
		 [#x{s={consup,V,W,V},in=[W,V],hn=?HSIZE_CONS} || {var,W} <- Svv] ++
		 [#x{s={match_fail,operand({atom,function_clause}),V},in=[V]}],
	flatmapfoldl2(fun(X, Sr, St) ->
		block_scf(X, Vdb, Sr, St)
	end, Sr0, St0, Xs);
fail_match({case_clause,{var,V}}, _Le, Vdb, Sr, St) ->
	X = #x{s={match_fail,operand({atom,case_clause}),V},in=[V]},
	block_scf(X, Vdb, Sr, St);
fail_match({badmatch,{var,V}}, _Le, Vdb, Sr, St) ->
	X = #x{s={match_fail,operand({atom,badmatch}),V},in=[V]},
	block_scf(X, Vdb, Sr, St);
fail_match(if_clause, _Le, _Vdb, Sr, St) ->
	Is = [{match_fail,operand({atom,if_clause})}],
	{Is,Sr,St}.

bif_cg({make_fun,Name,Arity,Index,Uniq}, [], [{var,V}], _Le, Vdb, Sr, St) ->
	X = #x{s={make_fun_nil,V,Arity,{literal,Index},{literal,Uniq}},out=[V],hn=?HSIZE_FUN},
	{L,St1} = local_func_label({Name,Arity}, St),
	Lambdas = [{Index,Uniq,L}] ++ St1#cg.lambdas,
	block_scf(X, Vdb, Sr, St1#cg{lambdas=Lambdas});
bif_cg({make_fun,Name,Arity,Index,Uniq}, Frozen, [{var,V}], Le, Vdb, Sr0, St0) ->

	%% Frozen is a list of variables; moreover, unique variables
	[{var,W0}|Nezorf] = reverse(Frozen),
	
	Xs = [#x{s={consup_nil,?Tmp,W0},in=[W0],out=[?Tmp],hn=?HSIZE_CONS}] ++
		 [#x{s={consup,?Tmp,W,?Tmp},in=[?Tmp,W],hn=?HSIZE_CONS} || {var,W} <- Nezorf] ++
		 [#x{s={make_fun,V,Arity,{literal,Index},{literal,Uniq},?Tmp},
			in=[?Tmp],out=[V],i=Le#l.i,hn=?HSIZE_FUN}],
	{L,St} = local_func_label({Name,Arity}, St0),
	Lambdas = [{Index,Uniq,L}] ++ St#cg.lambdas,
	flatmapfoldl2(fun(X, Sra, Sta) ->
		block_scf(X, Vdb, Sra, Sta)
	end, Sr0, St#cg{lambdas=Lambdas}, Xs);
bif_cg(is_function, [{var,A},{integer,N}], [{var,C}], _Le, Vdb, Sr, St) ->
	X = #x{s={is_function,C,A,N},in=[A],out=[C]},
	block_scf(X, Vdb, Sr, St);

bif_cg(bs_context_to_binary, _, [], _Le, _Vdb, Sr, St) ->
	{[],Sr,St};

bif_cg(self, [], [{var,V}], _Le, Vdb, Sr, St) ->
	X = #x{s={self,V},out=[V]},
	block_scf(X, Vdb, Sr, St);

% make these BIF calls; BEAM has opcodes for them (?)
bif_cg(get, As, Rs, Le, Vdb, Sr, St) ->
	call_cg({remote,{atom,erlang},{atom,get}}, As, Rs, Le, Vdb, Sr, St);
bif_cg(put, As, Rs, Le, Vdb, Sr, St) ->
	call_cg({remote,{atom,erlang},{atom,put}}, As, Rs, Le, Vdb, Sr, St);

bif_cg('=:=', [{var,A},nil], [{var,C}], Le, Vdb, Sr, St) ->
	X = #x{s={is_nil,C,A},in=[A],out=[C],i=Le#l.i},
	block_scf(X, Vdb, Sr, St);
bif_cg('=/=', [{var,A},nil], [{var,C}], Le, Vdb, Sr, St) ->
	X = #x{s={is_not_nil,C,A},in=[A],out=[C],i=Le#l.i},
	block_scf(X, Vdb, Sr, St);

bif_cg(element, [{integer,_}=I,{var,B}], [{var,C}], Le, Vdb, Sr0, St0) ->
	Xs = [#x{s={set,?Tmp,operand(I)},out=[?Tmp]},
		  #x{s={getel2,C,?Tmp,B},in=[?Tmp,B],out=[C],i=Le#l.i}],
	flatmapfoldl2(fun(X, Sra, Sta) ->
		block_scf(X, Vdb, Sra, Sta)
	end, Sr0, St0, Xs);
bif_cg(element, [{var,I},{literal,_}=O], [{var,C}], Le, Vdb, Sr0, St0) ->
	Xs = [#x{s={set,?Tmp,operand(O)},out=[?Tmp]},
		  #x{s={getel2,C,I,?Tmp},in=[I,?Tmp],out=[C],i=Le#l.i}],
	flatmapfoldl2(fun(X, Sra, Sta) ->
		block_scf(X, Vdb, Sra, Sta)
	end, Sr0, St0, Xs);

bif_cg(Op, [{var,A},{var,B}], [{var,C}], Le, Vdb, Sr, St) ->
	X = #x{s={bif_op(Op),C,A,B},in=[A,B],out=[C],i=Le#l.i},
	block_scf(X, Vdb, Sr, St);
bif_cg(Op, [{var,A},O], [{var,C}], Le, Vdb, Sr, St) ->
	X = #x{s={bif_op(Op),C,A,operand(O)},in=[A],out=[C],i=Le#l.i},
	block_scf(X, Vdb, Sr, St);

%% meanwhile, setelement is a BIF

bif_cg(dsetelement, [{integer,N},{var,B},nil], [], _Le, Vdb, Sr, St) ->
	X = #x{s={dsetel_nil,B,{literal,N-1}},in=[B]},
	block_scf(X, Vdb, Sr, St);
bif_cg(dsetelement, [{integer,N},{var,B},{var,C}], [], _Le, Vdb, Sr, St) ->
	X = #x{s={dsetel,B,{literal,N-1},C},in=[B,C]},
	block_scf(X, Vdb, Sr, St);
	
bif_cg(Op, [O,{var,B}], [{var,C}], Le, Vdb, Sr, St) ->
	X = #x{s={complement_op(Op),C,B,operand(O)},in=[B],out=[C],i=Le#l.i},
	block_scf(X, Vdb, Sr, St);

bif_cg(Op, [{var,A}], [{var,C}], Le, Vdb, Sr, St) ->
	X = #x{s={Op,C,A},in=[A],out=[C],i=Le#l.i},
	block_scf(X, Vdb, Sr, St).

bif_op('and') -> 'and';
bif_op('or') -> 'or';
bif_op('xor') -> 'xor';
bif_op('==') -> eq;
bif_op('=:=') -> eq;
bif_op('/=') -> neq;
bif_op('=/=') -> neq;
bif_op('=<') -> lesseq;
bif_op('>=') -> moreeq;
bif_op('<') -> less;
bif_op('>') -> more;
bif_op(element) -> getel2.

complement_op('and') -> 'and';
complement_op('or') -> 'or';
complement_op('xor') -> 'xor';
complement_op('==') -> eq;
complement_op('=:=') -> eq;
complement_op('/=') -> neq;
complement_op('=/=') -> neq;
complement_op('=<') -> more;
complement_op('>=') -> less;
complement_op('<') -> moreeq;
complement_op('>') -> lesseq.

gc_bif_cg(Op, [{var,A}], [{var,C}], Le, Vdb, Sr, St) ->
	X = #x{s={gc_bif_unary(Op),C,A},in=[A],out=[C],i=Le#l.i,hn=gb_heap_unary(Op)},
	block_scf(X, Vdb, Sr, St);
gc_bif_cg(Op, [{var,A},{var,B}], [{var,C}], Le, Vdb, Sr, St) ->
	X = #x{s={gc_bif_op(Op),C,A,B},in=[A,B],out=[C],i=Le#l.i,hn=gb_heap(Op)},
	block_scf(X, Vdb, Sr, St);
gc_bif_cg(Op, [{var,A},O], [{var,C}], Le, Vdb, Sr, St) ->
	case is_mult_op(Op) of
	true ->
		Xs = [#x{s={set,?Tmp,operand(O)},out=[?Tmp]},
			  #x{s={gc_bif_op(Op),C,A,?Tmp},in=[A,?Tmp],out=[C],i=Le#l.i,hn=gb_heap(Op)}],
		flatmapfoldl2(fun(X, Sra, Sta) ->
			block_scf(X, Vdb, Sra, Sta)
		end, Sr, St, Xs);
	false ->
		X = #x{s={gc_bif_op(Op),C,A,operand(O)},in=[A],out=[C],i=Le#l.i,hn=gb_heap(Op)},
		block_scf(X, Vdb, Sr, St)
	end;
gc_bif_cg(Op, [O,{var,B}], [{var,C}], Le, Vdb, Sr, St) ->
	case is_commut_op(Op) of
	true ->
		gc_bif_cg(Op, [{var,B},O], [{var,C}], Le, Vdb, Sr, St);
	false ->
		case is_mult_op(Op) of
		true ->
			Xs = [#x{s={set,?Tmp,operand(O)},out=[?Tmp]},
				#x{s={gc_bif_op(Op),C,?Tmp,B},in=[?Tmp,B],out=[C],i=Le#l.i,hn=gb_heap(Op)}],
			flatmapfoldl2(fun(X, Sra, Sta) ->
				block_scf(X, Vdb, Sra, Sta)
			end, Sr, St, Xs);
		false ->
			X = #x{s={gc_bif_op(Op),C,operand(O),B},in=[B],out=[C],i=Le#l.i,hn=gb_heap(Op)},
			block_scf(X, Vdb, Sr, St)
		end
	end.

is_commut_op('+') -> true;
is_commut_op('*') -> true;
is_commut_op('bor') -> true;
is_commut_op('band') -> true;
is_commut_op('bxor') -> true;
is_commut_op(_) -> false.

%% multiplication/division operations are limited to registers only

is_mult_op('*') -> true;
is_mult_op('/') -> true;
is_mult_op('div') -> true;
is_mult_op('rem') -> true;
is_mult_op('bsl') -> true;
is_mult_op('bsr') -> true;
is_mult_op(_) -> false.

gc_bif_op('+') -> add;
gc_bif_op('-') -> sub;
gc_bif_op('*') -> mult;
gc_bif_op('/') -> 'div';
gc_bif_op('div') -> idiv;
gc_bif_op('rem') -> mod;
gc_bif_op('bsl') -> 'bsl';
gc_bif_op('bsr') -> 'bsr';
gc_bif_op('bor') -> 'bor';
gc_bif_op('band') -> 'band';
gc_bif_op('bxor') -> 'bxor'.

gc_bif_unary(length) -> list_len;
gc_bif_unary(size) -> gen_size;
gc_bif_unary(bit_size) -> bit_size;
gc_bif_unary(byte_size) -> byte_size;
gc_bif_unary('-') -> neg;
gc_bif_unary('not') -> 'not';
gc_bif_unary('bnot') -> 'bnot';
gc_bif_unary(abs) -> abs;
gc_bif_unary(trunc) -> trunc;
gc_bif_unary(round) -> round;
gc_bif_unary(float) -> float.

gb_heap('+') -> ?HSIZE_FLOAT;
gb_heap('-') -> 0;
gb_heap('*') -> 0;
gb_heap('/') -> ?HSIZE_FLOAT;
gb_heap('div') -> 0;
gb_heap('rem') -> 0;
gb_heap('bsl') -> 0;
gb_heap('bsr') -> 0;
gb_heap('bor') -> 0;
gb_heap('band') -> 0;
gb_heap('bxor') -> 0.

gb_heap_unary(length) -> 0;
gb_heap_unary(size) -> 0;
gb_heap_unary(bit_size) -> 0;
gb_heap_unary(byte_size) -> 0;
gb_heap_unary('-') -> 0;
gb_heap_unary('not') -> 0;
gb_heap_unary('bnot') -> 0;
gb_heap_unary(abs) -> ?HSIZE_FLOAT;
gb_heap_unary(trunc) -> 0;
gb_heap_unary(round) -> 0;
gb_heap_unary(float) -> 0.

cg_enter_args(As, Sr) ->	%% Os
	Arity = length(As),
	ordered_args(seq(0, Arity-1), As, [], Sr).

enter_cg(S, As, _Le, _Vdb, Sr, St0) when is_atom(S) ->
	N = length(As),
	{L,St} = local_func_label({S,N}, St0),
	Ais = cg_enter_args(As, Sr),
	{Ais ++ [{enter,{l,L}}],Sr,St};

enter_cg({remote,{atom,erlang},{atom,apply}}, As, _Le, _Vdb, Sr, St) ->
	Ais = cg_enter_args(As, Sr),
	{Ais ++ [enter_apply],Sr,St};

enter_cg({remote,{atom,Mod}=M,{atom,Fun}=F}, As, _Le, _Vdb, Sr, St) ->
	N = length(As),
	Ais = cg_enter_args(As, Sr),
	case bifs:is_builtin(Mod, Fun, N) of
	true ->
		Index = bifs:bif_index(Mod, Fun, N),
		{Ais ++ [{enter_bif,N,{bif,Index}}],Sr,St};
	false ->
		{Ais ++ [{enter,operand(M),operand(F),N}],Sr,St}
	end;
enter_cg({remote,M,F}, As, _Le, _Vdb, Sr, St) ->
	N = length(As),
	true = N+1 < ?NUM_REGS,
	Ais = cg_enter_args(As ++ [M,F], Sr),
	{Ais ++ [{enter,{r,N},{r,N+1},N}],Sr,St};
enter_cg({var,_}=F, As, _Le, _Vdb, Sr, St) ->
	N = length(As),
	true = N < ?NUM_REGS,
	Ais = cg_enter_args(As ++ [F], Sr),
	{Ais ++ [{enter_fun,N,{r,N}}],Sr,St}.

cg_call_args(As, Rs, T, Vdb, Sr) ->
	Arity = length(As),

	LiveStack = map(fun(dead) ->
		dead;
	(V) ->
		case vdb_find(V, Vdb) of
		{_,_,Ttl} when Ttl > T -> V;
		{_,_,_} -> dead
		end
	end, Sr#sr.stack),
	
	IVTTs = map(fun({I,V}) ->
		{_,T1,T2} = vdb_find(V, Vdb),
		{I,V,{T1,V},T2}		%% compound sorting key
	end, Sr#sr.hots),
	
	%% push variables created earlier first
	SortedHots = keysort(3, IVTTs),
		
	{Moves0,Stk} = flatmapfoldl(fun({I,V,_,T2}, Stk) when T2 > T ->
		{N,Stk1} = push_var(V, Stk),
		{[{{r,I},{s,N}}],Stk1};
	(_, Stk) ->
		{[],Stk}
	end, LiveStack, SortedHots),
	
	Moves1 = ordered_args(seq(0, Arity-1), As, Moves0, Sr),
	
	Sr1 = case Rs of
	[] ->		%% TODO: it is possible not to recreate unused?
		 Sr#sr{unused=seq(0, ?NUM_REGS-1),hots=[],colds=[],stack=Stk};
	[{var,V}] ->
		 Sr#sr{unused=seq(1, ?NUM_REGS-1),hots=[{0,V}],colds=[],stack=Stk}
	end,
	
	{Moves1,Sr1}.

cg_bif_args(As, Rs, T, Vdb, Sr) ->
	Na = length(As),
	Nr = 1,		%length(Rs), return value may be discarded but r0 is ruined anyway
	Nm = if Na > Nr -> Na; true -> Nr end,
	
	Sr1 = clear_dead(Sr, T, Vdb),

	IVTs = map(fun({I,V}) ->
		{_,T1,_} = vdb_find(V, Vdb),
		{I,V,{T1,V}}	%% sic, compound sorting key
	end, Sr1#sr.hots),

	% To make compatible with cg_call_args
	% We have to push enough so that subsequent
	% cg_call_args does not fail
	
	% Arity may be zero but we need to push r0
	% as it is where return value resides

	PushUs = reverse(dropwhile(fun({I,_,_}) ->
		I >= Nm
	end, reverse(keysort(3, IVTs)))),

	{Hots1,Hots2} = partition(fun({I,_}) -> I < Nm end, Sr1#sr.hots),
	{Colds1,Colds2} = partition(fun({I,_,_}) -> I < Nm end, Sr1#sr.colds),
	Unused = [I || {I,_} <- Hots1] ++ [I || {I,_,_} <- Colds1] ++ Sr1#sr.unused,
	
	{Moves0,Colds,Stk} = flatmapfoldl2(fun({I,V,_}, Colds, Stk) ->
		{N,Stk1} = push_var(V, Stk),
		if I < Nm -> {[{{r,I},{s,N}}],Colds,Stk1};
			true -> {[{{r,I},{s,N}}],[{I,V,N}|Colds],Stk1} end
	end, Colds2, Sr1#sr.stack, PushUs),
	
	% Hots2 contain the same registers as part of PushUs (I >= Arity1); clean up
	Hots = Hots2 -- [{I,V} || {I,V,_} <- PushUs],
	
	Moves1 = ordered_args(seq(0, Na-1), As, Moves0, Sr),
	Sr2 = Sr1#sr{unused=Unused,hots=Hots,colds=Colds,stack=Stk},
	
	%TODO: what if length(Rs) > length(As)?
	{Moves1,easy_new_vars(Rs, Sr2)}.

call_cg(S, As, Rs, Le, Vdb, Sr0, St0) when is_atom(S) ->
	N = length(As),
	{L,St} = local_func_label({S,N}, St0),

	{Ais,Sr} = cg_call_args(As, Rs, Le#l.i, Vdb, Sr0),
	{Ais ++ [{call,{l,L}}],Sr,St};
	
call_cg({remote,{atom,erlang},{atom,apply}}, As, Rs, Le, Vdb, Sr0, St) ->
	{Ais,Sr} = cg_call_args(As, Rs, Le#l.i, Vdb, Sr0),
	{Ais ++ [call_apply],Sr,St};
	
call_cg({remote,{atom,Mod}=M,{atom,Fun}=F}, As, Rs, Le, Vdb, Sr0, St) ->
	N = length(As),
	case bifs:is_builtin(Mod, Fun, N) of
	true ->
		Index = bifs:bif_index(Mod, Fun, N),

		% gen moves to save hot registers < N which are live after the call
		% gen moves for call arguments
		% clear first N registers and, possibly, designate r0 as Rs
		
		{Ais,Sr} = cg_bif_args(As, Rs, Le#l.i, Vdb, Sr0),
		{Ais ++ [{call_bif,N,{bif,Index}}],Sr,St};
	false ->
		{Ais,Sr} = cg_call_args(As, Rs, Le#l.i, Vdb, Sr0),
		{Ais ++ [{call,operand(M),operand(F),N}],Sr,St}
	end;
call_cg({remote,M,F}, As, Rs, Le, Vdb, Sr0, St) ->
	N = length(As),
	true = N+1 < ?NUM_REGS,
	{Ais,Sr} = cg_call_args(As ++ [M,F], Rs, Le#l.i, Vdb, Sr0),
	{Ais ++ [{call,{r,N},{r,N+1},N}],Sr,St};
call_cg({var,_}=F, As, Rs, Le, Vdb, Sr0, St) ->
	N = length(As),
	true = N < ?NUM_REGS,
	{Ais,Sr} = cg_call_args(As ++ [F], Rs, Le#l.i, Vdb, Sr0),
	{Ais ++ [{call_fun,N,{r,N}}],Sr,St}.

vars(As) ->
	vars(As, []).

vars([{var,V}|As], Os) ->
	vars(As, [V|Os]);
vars([_|As], Os) ->
	vars(As, Os);
vars([], Os) ->
	Os.

set_cg([{var,V}], {cons,[nil,T]}, Le, Vdb, Sr, St) ->
	X = #x{s={nil_consup,V,operand(T)},in=vars([T]),out=[V],i=Le#l.i,hn=?HSIZE_CONS},
	block_scf(X, Vdb, Sr, St);
set_cg([{var,V}], {cons,[H,nil]}, Le, Vdb, Sr, St) ->
	X = #x{s={consup_nil,V,operand(H)},in=vars([H]),out=[V],i=Le#l.i,hn=?HSIZE_CONS},
	block_scf(X, Vdb, Sr, St);
set_cg([{var,V}], {cons,[H,T]}, Le, Vdb, Sr, St) ->
	X = #x{s={consup,V,operand(H),operand(T)},in=vars([H,T]),out=[V],i=Le#l.i,hn=?HSIZE_CONS},
	block_scf(X, Vdb, Sr, St);
set_cg([{var,V}], {tuple,Es}, Le, Vdb, Sr0, St0) ->
	Arity = length(Es),
	Xs1 = [#x{s={tuple,V,{literal,Arity}},out=[V],hn=?HSIZE_TUPLE(Arity)}],
	{Xs2,_} = mapfoldl(fun(nil, N) -> {#x{s={dsetel_nil,V,{literal,N}},in=[V]},N+1};
		({var,W}, N) -> {#x{s={dsetel,V,{literal,N},W},in=[V,W]},N+1};
		(O, N) -> {#x{s={dsetel,V,{literal,N},operand(O)},in=[V]},N+1}
	end, 0, Es),
	Xs3 = [#x{s=noop,i=Le#l.i}],
	flatmapfoldl2(fun(X, Sr, St) ->
		block_scf(X, Vdb, Sr, St)
	end, Sr0, St0, Xs1 ++ Xs2 ++ Xs3);
set_cg([{var,V}], {binary,Seg}, Le, Vdb, Sr0, St0) ->
	Xs1 = case bin_size(Seg) of
	{Sz,[]} ->
		[#x{s={binary,V,Sz},out=[V],hn=?HSIZE_BINARY(Sz)}];
	{Sz,WUs} ->
		[#x{s={set,V,operand({integer,Sz})},out=[V]}] ++
		map(fun({W,all}) ->
			#x{s={add_bit_size,V,W},in=[W,V]};
		({W,U}) ->
			#x{s={add_mult,V,W,U},in=[W,V]}
		end, WUs) ++
		[#x{s={binary,V,V},in=[V]}]
	end,
	Xs2 = bin_splices(Seg, V),
	Xs3 = [#x{s=noop,i=Le#l.i}],
	flatmapfoldl2(fun(X, Sr, St) ->
		block_scf(X, Vdb, Sr, St)
	end, Sr0, St0, Xs1 ++ Xs2 ++ Xs3);
set_cg([{var,V}], O, Le, Vdb, Sr, St) ->
	X = #x{s={set,V,operand(O)},out=[V],i=Le#l.i},
	block_scf(X, Vdb, Sr, St).

%
% F:receive
%	RecvB -> F
%	break -> B
% T:AfterB
%	break -> B
% B:
%

recv_loop_cg({atom,infinity}, {var,V}, B, _Tes, Rvs, _Le, Vdb, Sr, St0) ->
	NRegs1 = foldl(fun({I,_}, N) when I >= N -> I+1; (_, N) -> N end, 0, Sr#sr.hots),
	NRegs2 = foldl(fun({I,_,_}, N) when I >= N -> I+1; (_, N) -> N end, NRegs1, Sr#sr.colds),
	{[Bl,Rl],St} = new_labels(2, St0),
	Is1 = [recv_reset_inf],
		% {label,Rl}
	X = #x{s={recv_next,V,NRegs2},out=[V]},
	{Is2,Sr1,St1} = match_scf(X, Vdb, Sr, St),
	{Is3,Sr2,St2} = match_cg(B, Rl, Sr1, St1#cg{recv_loop=Rl,break=Bl}),
		% {label,Bl}
	{Is1 ++
	 [{label,Rl}] ++ Is2 ++ Is3 ++
	 [{label,Bl}],
	 easy_new_vars(Rvs, Sr2),
	 St2#cg{break=St0#cg.break}};
recv_loop_cg(Timeout, {var,V}, B, Tes, Rvs, _Le, Vdb, Sr, St0) ->
	NRegs1 = foldl(fun({I,_}, N) when I >= N -> I+1; (_, N) -> N end, 0, Sr#sr.hots),
	NRegs2 = foldl(fun({I,_,_}, N) when I >= N -> I+1; (_, N) -> N end, NRegs1, Sr#sr.colds),
	{[Tl,Bl,Rl],St} = new_labels(3, St0),
	{Is1,Sr1,St1} = case Timeout of
		{integer,_}=To ->
			{[{recv_reset,operand(To)}],Sr,St};
		{var,W} ->
			Tx = #x{s={recv_reset,W},in=[W]},
			match_scf(Tx, Vdb, Sr, St)
		end,
		% {label,Rl}
	X = #x{s={recv_next,V,NRegs2,{l,Tl}},out=[V]},
	{Is2,Sr2,St2} = match_scf(X, Vdb, Sr1, St1),
	{Is3,Sr3,St3} = match_cg(B, Rl, Sr2, St2#cg{recv_loop=Rl,break=Bl}),
		% {label,Tl}
	{Is4,Sr4,St4} = flatmapfoldl2(fun(Te, Sra, Sta) ->
		cg(Te, Tes#l.vdb, Sra, Sta)
	end, Sr1, St3, Tes#l.ke),	%% NB: Sr1; timeout code does not see V
		% {label,Bl}
	{Is1 ++
	 [{label,Rl}] ++ Is2 ++ Is3 ++
	 [{label,Tl}] ++ Is4 ++
	 [{label,Bl}],
	 easy_new_vars(Rvs, sr_merge(Sr3, Sr4)),
	 St4#cg{break=St0#cg.break}}.

recv_accept_cg(_Le, _Vdb, Sr, St) ->
	{[recv_accept],Sr,St}.		%% drop_msg

%% NB: the only expression returning nomerge
%% the only expression jumping backwards
recv_next_cg(_Le, _Vdb, _Sr, St) ->
	{[{jump,{l,St#cg.recv_loop}}],nomerge,St}.

catch_cg(Es, Vv, Le, _Vdb, Sr0, St0) ->
	Vdb = Le#l.vdb,
	{[Cl,Bl],Sta} = new_labels(2, St0),
	
	{Is1,Sr1,St1} = flatmapfoldl2(fun(E, Sr, St) ->
		cg(E, Vdb, Sr, St)
	end, clear_dead(Sr0, Le#l.i, Vdb), Sta#cg{break=Bl}, Es),

	{[{'catch',{l,Cl}}] ++ Is1 ++
	 [{label,Bl}] ++
	 [drop_catch] ++
	 [{label,Cl}],
	 easy_new_vars([Vv], Sr1),
	 St1}.

guard_match_cg(B, Rvs, Le, Vdb, Sr0, St0) ->	%% {Is,Sr,St}
	{Bl,St} = new_label(St0),
	%ok = io:format("gm(~w):Sr0=~w~n", [Le#l.i,Sr0]),
	
	{Bis,Sr,St1} = match_cg(B, none, Sr0, St#cg{break=Bl}),
	
	%% bind values in registers marked as {J,gbreakvar} to variables
	Rs = [V || {var,V} <- Rvs],	
	Hots = map(fun({I,{J,gbreakvar}}) ->
		{I,nth(J+1, Rs)};
	(X) ->
		X
	end, Sr#sr.hots),
	Sr1 = Sr#sr{hots=Hots},

	{Bis ++ [{label,Bl}],
		clear_dead(Sr1, Le#l.i, Vdb),
		St1#cg{break=St0#cg.break}}.

guard_break_cg(Es, _Locked, Le, Vdb, Sr0, St) ->
	N = length(Es),
	%ok = io:format("gb(~w):Sr0=~w~nVdb=~w~n", [Le#l.i,Sr0,Vdb]),

	Sr = clear_dead(Sr0, Le#l.i, Vdb),
	{Is,Us1} = split(N, Sr#sr.unused),
	Us2 = [I || {I,_,_} <- Sr#sr.colds],
	Os = ordered_args(Is, Es, [], Sr0),
	Hots = zip(Is, [{J,gbreakvar} || J <- seq(0, N-1)]) ++ Sr#sr.hots,

	Sr1 = Sr#sr{unused=Us2++Us1,hots=Hots,colds=[]},
	
	{Os ++ [{jump,{l,St#cg.break}}],Sr1,St}.

break_cg(Es, Le, Vdb, Sr0, St) ->
	{Is,Sr} = cg_call_args(Es, [], Le#l.i, Vdb, Sr0),
	%io:format("break_cg:I=~w:Sr0=~w:Sr=~w~nVdb=~w~n", [Le#l.i,Sr0,Sr,Vdb]),
	{Is ++ [{jump,{l,St#cg.break}}],Sr,St}.

return_cg(Rs, Le, Vdb, Sr0, St) ->
	{Is,Sr} = cg_call_args(Rs, [], Le#l.i, Vdb, Sr0),	%% TODO: better name?
	{Is ++ [return],Sr,St}.

bin_size(Seg) ->
	bin_size(Seg, 0, []).

bin_size({bin_end,_}, Sz, VUs) ->
	{Sz,VUs};
bin_size({bin_seg,_,{integer,N},U,_,_,[_,Next]}, Sz, VUs) ->
	bin_size(Next, Sz+N*U, VUs);
bin_size({bin_seg,_,{var,V},U,_,_,[_,Next]}, Sz, VUs) ->
	bin_size(Next, Sz, [{V,U}|VUs]);
bin_size({bin_seg,_,{atom,all},_U,_,_,[{var,V},Next]}, Sz, VUs) ->
	bin_size(Next, Sz, [{V,all}|VUs]).

bin_splices(Seg, B) ->
	bin_splices(Seg, B, []).

bin_splices({bin_end,_}, _B, Sc) ->
	Sc;
bin_splices({bin_seg,_,{integer,Sz},U,T,Opts,[{var,W},Next]}, B, Sc) ->
	bin_splices(Next, B, Sc ++ [#x{s={bspl(T),B,W,{literal,Sz*U},bopts(Opts)},in=[B,W]}]);
bin_splices({bin_seg,_,{integer,Sz},U,T,Opts,[O,Next]}, B, Sc) ->
	bin_splices(Next, B, Sc ++ [#x{s={bspl(T),B,operand(O),{literal,Sz*U},bopts(Opts)},in=[B]}]);
bin_splices({bin_seg,_,{atom,all},8,binary,Opts,[{var,W},Next]}, B, Sc) ->
	bin_splices(Next, B, Sc ++ [#x{s={bspl_b_all,B,W,bopts(Opts)},in=[B,W]}]);
bin_splices({bin_seg,_,{var,Sz},U,T,Opts,[{var,W},Next]}, B, Sc) ->
	bin_splices(Next, B, Sc ++ [#x{s={bspl(T),B,W,Sz,U,bopts(Opts)},in=[B,W,Sz]}]);
bin_splices({bin_seg,_,{var,Sz},U,T,Opts,[O,Next]}, B, Sc) ->
	bin_splices(Next, B, Sc ++ [#x{s={bspl(T),B,operand(O),Sz,U-1,bopts(Opts)},in=[B,Sz]}]).

bspl(integer) -> bspl_i;
bspl(float) -> bspl_f;
bspl(binary) -> bspl_b.

bopts(Opts) ->
	case {member(signed, Opts),member(little, Opts)} of
	{true, true} -> ?BOPTS_SIGNED bor ?BOPTS_LITTLE;
	{true, false} -> ?BOPTS_SIGNED;
	{false, true} -> ?BOPTS_LITTLE;
	{false, false} -> 0
	end.

match_cg(Le, Fail, Sr, St) ->
	match_cg(Le#l.ke, Le, Fail, Sr, St).

match_cg({block,Les}, Le, _Fail, Sr0, St0) ->
	%ok = io:format("blk:I=~w:Sr0=~w~n", [Le#l.i,Sr0]),
	Vdb = Le#l.vdb, %% no vdb in expressions, use upper's
	flatmapfoldl2(fun(E, Sr, St) ->
		Sl = source(E),
		{Is,Sra,Sta} = cg(E, Vdb, Sr, St),
		{Sl ++ Is,Sra,Sta}
	end, clear_dead(Sr0, Le#l.i, Vdb), St0, Les);
match_cg({alt,F,S}, _Le, Fail, Sr, St0) ->
	{Tf,St} = new_label(St0),
	{Fis,Sr1,St1} = match_cg(F, Tf, Sr, St),
	{Sis,Sr2,St2} = match_cg(S, Fail, Sr, St1#cg{break=St#cg.break}),	%% probably, recv_loop should be restored too?
	%ok = io:format("alt(~w)~nSr1=~w~nSr2=~w~n", [_Le#l.i,Sr1,Sr2]),
	Srm = sr_merge(Sr1, Sr2),
	{Fis ++ [{label,Tf}] ++ Sis,Srm,St2};
match_cg({select,{var,V},Scs}, Le, Fail, Sr, St) ->
	select_cg(Scs, V, Le, Fail, Sr, St);
match_cg({guard,Gcs}, _Le, Fail, Sr0, St0) ->
	match_fmf(fun(C, Tf, Sr, St) ->
		guard_clause(C, Tf, Sr, St)
	end, Fail, Sr0, St0, Gcs).

select_cg(Scs, V, _Le, Fail, Sr0, St0) ->
	match_fmf(fun(C, Tf, Sr, St) ->
		Sl = source(C),
		{Is,Sra,Sta} = select_type_cg(C, V, Tf, Fail, Sr, St),
		{Sl ++ Is,Sra,Sta}
	end, Fail, Sr0, St0, Scs).

select_type_cg(#l{ke={type_clause,cons,Vcs}}, V, Tf, Vf, Sr, St) ->
	select_cons(Vcs, V, Tf, Vf, Sr, St);
select_type_cg(#l{ke={type_clause,nil,Vcs},vdb=Vdb}, V, Tf, _Vf, Sr, St) ->
	select_nil(Vcs, V, Tf, Vdb, Sr, St);
select_type_cg(#l{ke={type_clause,tuple,Vcs},vdb=Vdb}, V, Tf, _Vf, Sr, St) ->
	select_tuple(Vcs, V, Tf, Vdb, Sr, St);
select_type_cg(#l{ke={type_clause,literal,Vcs},vdb=Vdb}, V, Tf, _Vf, Sr0, St0) ->
	match_fmf(fun(C, Fail, Sr, St) ->
		select_val_cg(C, V, Fail, Vdb, Sr, St)
	end, Tf, Sr0, St0, Vcs);	%% TODO: Tf not Vf because literal type intersects with other types?
select_type_cg(#l{ke={type_clause,binary,Vcs},vdb=Vdb}, V, Tf, Vf, Sr, St) ->
	select_binary(Vcs, V, Tf, Vf, Vdb, Sr, St);
select_type_cg(#l{ke={type_clause,bin_seg,Vcs},vdb=Vdb}, V, Tf, _Vf, Sr, St) ->
	select_bin_segs(Vcs, V, Tf, Vdb, Sr, St);
select_type_cg(#l{ke={type_clause,bin_end,Vcs},vdb=Vdb}, V, Tf, _Vf, Sr, St) ->
	select_bin_end(Vcs, V, Tf, Vdb, Sr, St);
select_type_cg(#l{ke={type_clause,Type,Vcs},vdb=Vdb}, V, Tf, Vf, Sr0, St0) ->
	X = case Type of
	integer -> #x{s={is_integer,V,{l,Tf}},in=[V]};
	atom -> #x{s={is_atom,V,{l,Tf}},in=[V]};
	float -> #x{s={is_float,V,{l,Tf}},in=[V]}
	end,
	{Is,Sr,St} = match_scf(X, Vdb, Sr0, St0),
	{Is1,Sr1,St1} = match_fmf(fun(C, Fail, Sra, Sta) ->
		select_val_cg(C, V, Fail, Vdb, Sra, Sta)
	end, Vf, Sr, St, Vcs),
	{Is ++ Is1,Sr1,St1}.

select_val_cg(#l{ke={val_clause,Val,B}}=Le, V, Fail, Vdb, Sr0, St0) ->
	X = case Val of
	{atom,true} -> #x{s={is_true,V,{l,Fail}},in=[V]};
	{atom,false} -> #x{s={is_false,V,{l,Fail}},in=[V]};
	O -> #x{s={eq,V,operand(O),{l,Fail}},in=[V]}
	end,
	{Is,Sr,St} = match_scf(X, Vdb, Sr0, St0),
	{Is1,Sr1,St1} = match_cg(B, Fail, Sr, St),
	Sl = source(Le),
	{Sl ++ Is ++ Is1,Sr1,St1}.

select_nil([#l{ke={val_clause,nil,B}}], V, Tf, Vdb, Sr0, St0) ->
	X = #x{s={is_nil,V,{l,Tf}},in=[V]},
	{Is,Sr,St} = match_scf(X, Vdb, Sr0, St0),
	{Is1,Sr1,St1} = match_cg(B, Tf, Sr, St),
	{Is ++ Is1,Sr1,St1}.

select_cons([#l{ke={val_clause,{cons,Vvs},B},i=I,vdb=Vdb}], V, Tf, Vf, Sr0, St0) ->
	%ok = io:format("select_cons:~w:~w->~w~nSr=~w,Vdb=~w~n", [I,V,Vvs,Sr0,Vdb]),
	[{var,H},{var,T}] = Vvs,
	{_,_,Th} = vdb_find(H, Vdb),
	{_,_,Tl} = vdb_find(T, Vdb),
	X1 = #x{s={is_cons,V,{l,Tf}},in=[V]},
	{Is,Sr,St} = match_scf(X1, Vdb, Sr0, St0),
	{Is1,Sr1,St1} = if Th >= I, Tl >= I ->
		X2 = #x{s={uncons,V,H,T},in=[V],out=[H,T],i=I},
		match_scf(X2, Vdb, Sr, St);
	Th >= I ->
		X2 = #x{s={hd,H,V},in=[V],out=[H],i=I},
		match_scf(X2, Vdb, Sr, St);
	Tl >= I ->
		X2 = #x{s={tl,T,V},in=[V],out=[T],i=I},
		match_scf(X2, Vdb, Sr, St);
	true ->
		{[],Sr,St}
	end,
	{Is2,Sr2,St2} = match_cg(B, Vf, Sr1, St1),
	{Is ++ Is1 ++ Is2,Sr2,St2}.

%% {val_clause,{tuple,[{var,ker1},{var,ker0}]},B}
select_tuple(Vcs, V, Tf, Vdb, Sr0, St0) ->
	match_fmf(fun(C, Fail, Sr, St) ->
		select_extract_tuple(C, V, Fail, Vdb, Sr, St)
	end, Tf, Sr0, St0, Vcs).

select_extract_tuple(#l{ke={val_clause,{tuple,Vvs},B}}=Le, V, Fail, _Vdb, Sr0, St0) ->
	Arity = length(Vvs),
	VNs = zip([W || {var,W} <- Vvs], seq(0, Arity-1)),
	
	LiveVNs = filter(fun({W,_}) ->
		{_,_,Ttl} = vdb_find(W, Le#l.vdb),
		Ttl > Le#l.i
	end, VNs),
	
	Xs = [#x{s={is_tuple_of_arity,V,{literal,Arity},{l,Fail}},in=[V]}] ++
		 [#x{s={getel,W,{literal,N},V},in=[V],out=[W]} || {W,N} <- LiveVNs],
	{Is,Sr,St} = flatmapfoldl2(fun(X, Sr, St) ->
		match_scf(X, Le#l.vdb, Sr, St)
	end, Sr0, St0, Xs),
	{Is1,Sr1,St1} = match_cg(B, Fail, Sr, St),
	{Is ++ Is1,Sr1,St1}.

select_binary([#l{ke={val_clause,{binary,{var,C}},B},vdb=Vdb}], V, Tf, Vf, _Vdb, Sr0, St0) ->
	X1 = #x{s={is_binary,V,{l,Tf}},in=[V]},
	{Is,Sr,St} = match_scf(X1, Vdb, Sr0, St0),
	X2 = #x{s={move,C,V},in=[V],out=[C]},
	{Is1,Sr1,St1} = match_scf(X2, Vdb, Sr, St),
	{Is2,Sr2,St2} = match_cg(B, Vf, Sr1, St1),
	{Is ++ Is1 ++ Is2,Sr2,St2}.

select_bin_segs(Vcs, V, Fail, _Vdb, Sr0, St0) ->
	match_fmf(fun(#l{ke={val_clause,Seg,B}}=Le, Tf, Sr, St) ->
		select_bin_seg(Seg, B, V, Le, Tf, Sr, St)
	end, Fail, Sr0, St0, Vcs).

select_bin_seg({bin_seg,_,{atom,all},8,binary,Opts,[{var,C}]}, B, V, _Le, Fail, Sr0, St0) ->

	%% NB: per nasty chance, V may not be listed in Vdb properly

	X = #x{s={bchip_b_all,C,V,bopts(Opts)},
			in=[V],
			out=[C],
			hn=?HSIZE_BINARY_SHARED},
	{Is,Sr,St} = match_scf(X, B#l.vdb, Sr0, St0),	%% TODO: match_scf does not need Vdb
	
	%%
	%% Fix:
	%% V is not listed in Vdb and thus can not be cleared
	%%		out when block code reached
	%%
	
	SrFix = forget_var(V, Sr),
	
	{Is1,Sr1,St1} = match_cg(B, Fail, SrFix, St),
	{Is ++ Is1,Sr1,St1};

select_bin_seg({bin_seg,_,{integer,Sz},U,T,Opts,[{var,C},{var,R}]}, B, V, _Le, Fail, Sr0, St0) ->

	%% NB: per nasty chance, R (and possibly V) are not listed in Vdb properly

	X = #x{s={bchip(T),C,R,V,{literal,Sz*U},bopts(Opts),{l,Fail}},
			in=[V],
			out=[C,R],
			hn=?HSIZE_BINARY_SHARED},
	{Is,Sr,St} = match_scf(X, B#l.vdb, Sr0, St0),	%% TODO: match_scf does not need Vdb
	
	%%
	%% Fix:
	%% V is not listed in Vdb and thus can not be cleared
	%%		out when block code reached
	%%
	
	SrFix = forget_var(V, Sr),
	
	{Is1,Sr1,St1} = match_cg(B, Fail, SrFix, St),
	{Is ++ Is1,Sr1,St1};

select_bin_seg({bin_seg,_,{var,S},U,T,Opts,[{var,C},{var,R}]}, B, V, _Le, Fail, Sr0, St0) ->

	%% NB: per nasty chance, R (and possibly V) are not listed in Vdb properly

	X = #x{s={bchip(T),C,R,V,S,U,bopts(Opts),{l,Fail}},
			in=[V,S],
			out=[C,R],
			hn=?HSIZE_BINARY_SHARED},
	{Is,Sr,St} = match_scf(X, B#l.vdb, Sr0, St0),	%% TODO: match_scf does not need Vdb
	
	%%
	%% Fix:
	%% V is not listed in Vdb and thus can not be cleared
	%%		out when block code reached
	%%
	
	SrFix = forget_var(V, Sr),
	
	{Is1,Sr1,St1} = match_cg(B, Fail, SrFix, St),
	{Is ++ Is1,Sr1,St1}.

select_bin_end([#l{ke={val_clause,{bin_end,_},B}}=Le], V, Fail, _Vdb, Sr0, St0) ->

	%% per nasty chance, R (and possibly V) are not listed in Vdb properly

	X = #x{s={is_empty_binary,V,{l,Fail}},in=[V]},
	{Is,Sr,St} = match_scf(X, Le#l.vdb, Sr0, St0),
	%ok = io:format("bin_end:~w:Sr=~w~nVdb(B)=~w~n", [Le#l.i,Sr,B#l.vdb]),

	%%
	%% Fix:
	%% V is not listed in Vdb and thus can not be cleared
	%%		out when block code reached
	%%
	
	SrFix = forget_var(V, Sr),
	
	{Is1,Sr1,St1} = match_cg(B, Fail, SrFix, St),	%% TODO: clear_dead may be needed here
	{Is ++ Is1,Sr1,St1}.

bchip(integer) -> bchip_i;
bchip(float) -> bchip_f;
bchip(binary) -> bchip_b.

guard_clause(#l{ke={guard_clause,G,B},vdb=Vdb}, Gf, Sr, St) -> % {Is,Sr,St}
	{Gis,Sr1,St1} = guard_cg(G, Gf, Vdb, clear_dead(Sr, G#l.i, Vdb), St),
	{Bis,Sr2,St2} = match_cg(B, Gf, Sr1, St1),
	Gl = source(G),
	{Gl ++ Gis ++ Bis,Sr2,St2}.

guard_cg(#l{ke={protected,Les,_Rs},vdb=Vdb}, Fail, _Vdb, Sr0, St0) ->	%% TODO: what is the meaning of _Rs?
	flatmapfoldl2(fun(E, Sr, St) ->
		{Eis,Sr1,St1} = guard_cg(E, Fail, Vdb, Sr, St),
		{Eis,clear_dead(Sr1, E#l.i, Vdb),St1}
	end, Sr0, St0, Les);
guard_cg(#l{ke={test,is_record,[{var,A},{atom,_}=Rec,{integer,_}=N]}}, Fail, Vdb, Sr, St) ->
	X = #x{s={is_record,A,operand(Rec),operand(N),{l,Fail}},in=[A]},
	match_scf(X, Vdb, Sr, St);
guard_cg(#l{ke={test,is_function,[{var,A},{integer,N}]}}, Fail, Vdb, Sr, St) ->
	X = #x{s={is_function,A,N,{l,Fail}},in=[A]},
	match_scf(X, Vdb, Sr, St);
guard_cg(#l{ke={test,'=:=',[{var,A},nil]}}, Fail, Vdb, Sr, St) ->
	X = #x{s={is_nil,A,{l,Fail}},in=[A]},
	match_scf(X, Vdb, Sr, St);
guard_cg(#l{ke={test,'=/=',[{var,A},nil]}}, Fail, Vdb, Sr, St) ->
	X = #x{s={is_not_nil,A,{l,Fail}},in=[A]},
	match_scf(X, Vdb, Sr, St);
guard_cg(#l{ke={test,'=:=',[{var,A},{atom,true}]}}, Fail, Vdb, Sr, St) ->
	X = #x{s={is_true,A,{l,Fail}},in=[A]},
	match_scf(X, Vdb, Sr, St);
guard_cg(#l{ke={test,'=:=',[{var,A},{atom,false}]}}, Fail, Vdb, Sr, St) ->
	X = #x{s={is_false,A,{l,Fail}},in=[A]},
	match_scf(X, Vdb, Sr, St);
guard_cg(#l{ke={test,Op,[{var,A},{var,B}]}}, Fail, Vdb, Sr, St) ->
	X = #x{s={guard_test_op2(Op),A,B,{l,Fail}},in=[A,B]},
	match_scf(X, Vdb, Sr, St);
guard_cg(#l{ke={test,Op,[{var,A},O]}}, Fail, Vdb, Sr, St) ->
	X = #x{s={guard_test_op2(Op),A,operand(O),{l,Fail}},in=[A]},
	match_scf(X, Vdb, Sr, St);
guard_cg(#l{ke={test,Op,[O,{var,B}]}}, Fail, Vdb, Sr, St) ->
	X = #x{s={complement_op(Op),B,operand(O),{l,Fail}},in=[B]},
	match_scf(X, Vdb, Sr, St);
guard_cg(#l{ke={test,Op,[{var,A}]}}, Fail, Vdb, Sr, St) ->
	X = #x{s={guard_test_op1(Op),A,{l,Fail}},in=[A]},
	match_scf(X, Vdb, Sr, St);
guard_cg(E, _Fail, Vdb, Sr, St) ->
	cg(E, Vdb, Sr, St).

guard_test_op1(is_atom) -> is_atom;
guard_test_op1(is_binary) -> is_binary;
guard_test_op1(is_float) -> is_float;
guard_test_op1(is_function) -> is_function;
guard_test_op1(is_integer) -> is_integer;
guard_test_op1(is_list) -> is_list;
guard_test_op1(is_cons) -> is_cons;
guard_test_op1(is_nil) -> is_nil;
guard_test_op1(is_number) -> is_number;
guard_test_op1(is_pid) -> is_pid;
guard_test_op1(is_tuple) -> is_tuple.

guard_test_op2('==') -> eq;
guard_test_op2('=:=') -> eq;
guard_test_op2('/=') -> neq;
guard_test_op2('=/=') -> neq;
guard_test_op2('>=') -> moreeq;
guard_test_op2('=<') -> lesseq;
guard_test_op2('>') -> more;
guard_test_op2('<') -> less.

match_fmf(F, Fail, Sr, St, [C]) ->
	F(C, Fail, Sr, St);
match_fmf(F, LastFail, Sr, St0, [C|Cs]) ->
	{Fail,St} = new_label(St0),
	{Is1,Sr1,St1} = F(C, Fail, Sr, St),
	{Is2,Sr2,St2} = match_fmf(F, LastFail, Sr, St1, Cs),
	%ok = io:format("fmf merge~n~w:Sr1=~w~nSr2=~w~n", [C#l.i,Sr1,Sr2]),
	Srm = sr_merge(Sr1, Sr2),
	{Is1 ++ [{label,Fail}] ++ Is2,Srm,St2}.

ordered_args(Is, As, Moves0, Sr) ->	%% Os
	ANs = map(fun({{var,V},J}=X) ->
		case find_var(V, Sr) of
		{r,I} -> {x,{r,I},{r,J}};
		{s,N} -> {x,{s,N},{r,J}};
		_ -> X
		end;
	(X) ->
		X
	end, zip(As, Is)),
	
	{Xs,SNs} = partition(fun({x,_,_}) -> true; (_) -> false end, ANs),
	Moves = [{From,To} || {x,From,To} <- Xs, From =/= To] ++ Moves0,
	
	Cycles = find_cycles(Moves),
	Swaps = concat([tl(reverse(C)) || C <- Cycles]),
	
	MovesNoCycles = Moves -- concat(Cycles),
	
	Os1 = [{move,To,From}
			|| {From,To} <- sort_moves(MovesNoCycles)],
			
	Os2 = map(fun({{s,_}=From,To}) ->
		{swap,To,From};
	({From,To}) ->
		{swap,From,To}
	end, Swaps),
		
	Os3 = map(fun({nil,J}) ->
		{nil,{r,J}};
	({O,J}) ->
		{set,{r,J},operand(O)}
	end, SNs),
	
	Os1 ++ Os2 ++ Os3.

sort_moves(Moves) ->
	% find roots; moves that do not have predessors
	{Roots,Leaves} = partition(fun({I,_}) -> not keymember(I, 2, Moves) end, Moves),
	sort_moves([{I,J,0} || {I,J} <- Roots], Leaves).

sort_moves(Growing, []) ->
	[{I,J} || {I,J,_} <- keysort(3, Growing)];
sort_moves(Growing, Flying) ->
	{Budding,FlyingStill} = partition(fun({I,_}) ->
		keymember(I, 2, Growing)
	end, Flying),
	NewGrowth = [{I1,J1,Depth-1}
		|| {_,J,Depth} <- Growing, {I1,J1} <- Budding, J =:= I1],
	sort_moves(Growing ++ NewGrowth, FlyingStill).

find_cycles(Moves) ->
	Steps = [{IJ,[IJ]} || IJ <- Moves],
	find_cycles(Steps, Steps, []).

find_cycles(_, [], Cycles) ->
	rem_dups([Links || {_,Links} <- Cycles]);
find_cycles(Steps, Paths, Cycles) ->
	United = [unite(P1, P2) || {{_,J},_}=P1 <- Paths, {{I,_},_}=P2 <- Steps, I =:= J],
	{Cycles1,Paths1} = lists:partition(fun({{N,N},_}) -> true; (_) -> false end, United),
	
	find_cycles(Steps, Paths1, Cycles1 ++ Cycles).

unite({{I1,J1},Links1},{{I2,J2},Links2}) when J1 =:= I2 ->
	{{I1,J2},Links1 ++ Links2}.

rem_dups([]) ->
	[];
rem_dups([[IJ|_]=P|Ps]) ->
	Ps1 = lists:filter(fun(X) -> not lists:member(IJ, X) end, Ps),
	[P|rem_dups(Ps1)].

%%
%% Preemptive register allocations
%%
block_scf(#x{s=noop,i=I}, Vdb, Sr, St) ->	%% {Is,Sr,St}
	{[],clear_dead(Sr, I, Vdb),St};

block_scf(#x{s=S,in=In,out=Out,i=I,hn=HN}, Vdb, Sr, St) ->	%% {Is,Sr,St}
	{Os1,IVs1,Sra} = fetch_vars(In, Vdb, Sr),
	
	Sr1 = if I =/= none -> clear_dead(Sra, I, Vdb);
		true -> Sra end,
	%ok = io:format("b_scf:~w:Sr1=~w~nVdb=~w~n", [I,Sr1,Vdb]),

	{Os2,IVs2,Sr2} = alloc_vars(Out, Out ++ In, Vdb, Sr1),
	O = place_vars(S, IVs1 ++ IVs2),
	
	if HN =:= 0 ->
		{Os1 ++ Os2 ++ [O],Sr2,St};
	true ->
		{Os1 ++ Os2 ++ [O],Sr2,St#cg{heap_needed=HN+St#cg.heap_needed}}
	end.

%%
%% Compatible register allocations
%%
match_scf(#x{s=S,in=In,out=Out,i=I,hn=HN}, Vdb, Sr, St) ->	%% {Is,Sr,St}
	{Os1,IVs1,Sra} = fetch_vars_compat(In, Vdb, Sr),

	Sr1 = if I =/= none -> clear_dead(Sra, I, Vdb);
		true -> Sra end,
		
	{IVs2,Sr2} = alloc_vars_compat(Out, Vdb, Sr1),
	%ok = io:format("S=~w~nIVs1=~w,IVs2=~w~nSr=~w~n", [S,IVs1,IVs2,Sr2]),
	O = place_vars(S, IVs1 ++ IVs2),

	if HN =:= 0 ->
		{Os1 ++ [O],Sr2,St};
	true ->
		{Os1 ++ [O],Sr2,St#cg{heap_needed=HN+St#cg.heap_needed}}
	end.

find_var(V, Sr) ->	%% {r,I} | {s,N} | false
	case keyfind(V, 2, Sr#sr.hots) of
	false ->
		case keyfind(V, 2, Sr#sr.colds) of
		false ->
			N = index(V, Sr#sr.stack),
			
			%% DEBUG
			if N =:= false ->
				ok = io:format("!find_var(~w, ~w)~n", [V, Sr]);
			true ->
				ok
			end,
			
			{s,N};
		{I,_,_} ->
			{r,I}
		end;
	{I,_} ->
		{r,I}
	end.

place_vars(S, IVs) ->
	place_vars(S, IVs, 2).

place_vars(S, IVs, N) when N =< size(S) ->
	case element(N, S) of
	V when is_atom(V) ->
		{I,_} = keyfind(V, 2, IVs),
		place_vars(setelement(N, S, {r,I}), IVs, N+1);
	_ ->
		place_vars(S, IVs, N+1)
	end;
place_vars(S, _, _) ->
	S.

easy_new_vars(Vvs, Sr) ->	%% Sr
	Vs = [V || {var,V} <- Vvs],
	new_vars(seq(0, length(Vs)-1), Vs, Sr).

new_vars(Is, Vs, Sr) ->		%% Sr
	Us = Sr#sr.unused -- Is,
	Hots = zip(Is, Vs) ++ Sr#sr.hots,
	Sr#sr{unused=Us,hots=Hots}.

% sr_validate(Sr, Line) ->
% 	Is1 = [I || {I,_} <- Sr#sr.hots],
% 	Is2 = [I || {I,_,_} <- Sr#sr.colds],
% 	Is3 = Sr#sr.unused,
% 	
% 	%% TODO: other checks?
% 	
% 	Is = Is1 ++ Is2 ++ Is3,
% 	L1 = length(Is),
% 	L2 = length(usort(Is)),
% 	
% 	if L1 =/= ?NUM_REGS orelse L1 =/= L2 ->
% 		io:format("[~w] invalid sr: ~w~n", [Line,Sr]);
% 	true ->
% 		ok
% 	end.

sr_merge(nomerge, Sr) ->	%% Sr
	Sr;
sr_merge(Sr, nomerge) ->
	Sr;
sr_merge(Sr1, Sr2) ->
	%% Assertions can be removed to improve speed
	Us1 = sort(Sr1#sr.unused),
	Us2 = sort(Sr2#sr.unused),
	Hs1 = keysort(1, Sr1#sr.hots),
	Hs2 = keysort(1, Sr2#sr.hots),
	Cs1 = keysort(1, Sr1#sr.colds),
	Cs2 = keysort(1, Sr2#sr.colds),
	if Us1 =/= Us2; Hs1 =/= Hs2; Cs1 =/= Cs2 ->
		io:format("!sr_merge(Sr1, Sr2)~nSr1=~w~nSr2=~w~n", [Sr1,Sr2]);
	true -> ok end,
	Us1 = Us2,
	Hs1 = Hs2,
	Cs1 = Cs2,
	Stack = longest(Sr1#sr.stack, Sr2#sr.stack),
	#sr{unused=Us1,hots=Hs1,colds=Cs1,stack=Stack}.

longest(Vs1, Vs2) ->	%% Vs
	longest(Vs1, Vs2, []).

longest([], [], Sv) ->
	reverse(Sv);
longest([], [dead|Vs2], Sv) ->
	longest([], Vs2, [dead|Sv]);
longest([dead|Vs1], [], Sv) ->
	longest(Vs1, [], [dead|Sv]);
longest([X|Vs1], [X|Vs2], Sv) ->
	longest(Vs1, Vs2, [X|Sv]).

%% unused:	I
%% hots:	{I,V}
%% colds:	{I,V,N}
%% stack:	V

%%
%% fetch_vars_compat:
%%
%% fetch variables to register not mangling current
%% register allocations, so that other branches would
%% not notice
%%
fetch_vars_compat(Vs, _Vdb, Sr0) ->	%% {Os,IVs,Sr}
	foldl(fun(V, {Os,IVs,Sr}) ->
		case find_var(V, Sr) of
		{r,I} ->
			{Os,[{I,V}] ++ IVs,Sr};
		{s,N} ->
			case Sr#sr.unused of
			[] ->
				erlang:error(not_enough_registers);
			[U|Us] ->
				{Os ++ [{move,{r,U},{s,N}}],
				 [{U,V}] ++ IVs,
				 Sr#sr{unused=Us,colds=[{U,V,N}] ++ Sr#sr.colds}}
			end
		end
	end, {[],[],Sr0}, Vs).

fetch_vars(Vs, Vdb, Sr0) ->	%% {Os,IVs,Sr}
	foldl(fun(V, {Os,IVs,Sr}) ->
		case keyfind(V, 2, Sr#sr.hots) of
		false ->
			case keyfind(V, 2, Sr#sr.colds) of
			false ->
				{Os1,I,Sr1} = free_reg(Vs, Vdb, Sr),
				%% NB: I is not on the unused list of Sr1
				N = index(V, Sr1#sr.stack),
				
				%% DEBUG
				if N =:= false ->
					io:format("!f_v(~w, ~w)~nI=~w,Sr1=~w~n", [Vs,Sr0,I,Sr1]),
					erlang:error(unknown_variable);
				true -> ok end,
				
				{Os ++ Os1 ++ [{move,{r,I},{s,N}}],
				 [{I,V}|IVs],
				 Sr1#sr{colds=[{I,V,N}|Sr1#sr.colds]}};
			{I,V,_} ->
				{Os,[{I,V}|IVs],Sr}
			end;
		IV ->
			{Os,[IV|IVs],Sr}
		end
	end, {[],[],Sr0}, Vs).

%% see fetch_vars_compat()

alloc_vars_compat(Vs, _Vdb, Sr) ->	%% {IVs,Sr}
	N = length(Vs),
	if length(Sr#sr.unused) < N ->
		erlang:error(not_enough_registers);
	true ->
		{Us1,Us2} = split(N, Sr#sr.unused),
		IVs = zip(Us1, Vs),
		Hots = IVs ++ Sr#sr.hots,
		{IVs,Sr#sr{unused=Us2,hots=Hots}}
	end.

alloc_vars(Vs, Locked, Vdb, Sr0) ->	%% {Os,IVs,Sr}
	foldl(fun(V, {Os,IVs,Sr}) ->
		{Os1,I,Sr1} = free_reg(Locked, Vdb, Sr),
		%% NB: I is not on the unused list of Sr1
		{Os ++ Os1,
		 [{I,V}|IVs],
		 Sr1#sr{hots=[{I,V}|Sr1#sr.hots]}}
	end, {[],[],Sr0}, Vs).

forget_var(V, Sr) ->	%% Sr
	case keytake(V, 2, Sr#sr.hots) of
	{value,{U,_},Hots} ->
		Sr#sr{unused=[U|Sr#sr.unused],hots=Hots};
	false ->
		Stack = delete(V, Sr#sr.stack),
		case keytake(V, 2, Sr#sr.colds) of
		{value,{I,V,_},Colds} ->
			Sr#sr{unused=[I|Sr#sr.unused],colds=Colds,stack=Stack};
		false ->
			Sr#sr{stack=Stack}
		end
	end.

free_reg(_Vs, _Vdb, #sr{unused=[U|Us]}=Sr) ->	%% {Os,I,Sr}
	{[],U,Sr#sr{unused=Us}};
free_reg(Vs, Vdb, Sr) ->
	IVNTs = map(fun({_,V,_}=IVN) ->
		{_,_,Ttl} = vdb_find(V, Vdb),
		case member(V, Vs) of
		true ->	{IVN, locked};
		false -> {IVN, Ttl}
		end
	end, Sr#sr.colds),
	case keysort(2, IVNTs) of
	[] ->
		freeze_hot(Vs, Vdb, Sr);
	[{_,locked}|_] ->
		freeze_hot(Vs, Vdb, Sr);
	[{{I,V,_},_}|_] ->
		Colds = keydelete(V, 2, Sr#sr.colds),
		{[],I,Sr#sr{colds=Colds}}
	end.

freeze_hot(Vs, Vdb, Sr) ->	%% {Os,I,Sr}
	%ok = io:format("f_h:Vs=~w,Sr=~w~nVdb=~w~n", [Vs,Sr,Vdb]),
	IVTs = map(fun({_,V}=IV) ->	
		{_,_,Ttl} = vdb_find(V, Vdb),
		case member(V, Vs) of
		true ->	{IV, locked};
		false -> {IV, Ttl}
		end
	end, Sr#sr.hots),
	case keysort(2, IVTs) of
	[] ->
		erlang:error(not_enough_registers);
	[{_,locked}|_] ->
		erlang:error(not_enough_registers);
	[{{I,V},_}|_] ->
		Hots = keydelete(V, 2, Sr#sr.hots),
		{N,Stack} = push_var(V, Sr#sr.stack),
		{[{move,{s,N},{r,I}}],I,Sr#sr{hots=Hots,stack=Stack}}
	end.

push_var(V, Vs) ->
	push_var(V, Vs, 0, []).
	
push_var(V, [dead|Vs], N, Sv) ->
	{N,reverse(Sv) ++ [V] ++ Vs};
push_var(V, [X|Vs], N, Sv) ->
	push_var(V, Vs, N+1, [X|Sv]);
push_var(V, [], N, Sv) ->
	{N,reverse(Sv) ++ [V]}.

clear_dead(Sr, T, Vdb) ->	%% Sr
	%io:format("c_d:Sr=~w:~w:Vdb=~w~n", [Sr,T,Vdb]),
	{Hs,Us} = foldl(fun({_,{_,gbreakvar}}=IV, {Hs,Us}) ->
		{[IV|Hs],Us};
	({I,V}=IV, {Hs,Us}) ->
		case vdb_find(V, Vdb) of
		{_,_,Ttl} when Ttl =< T ->
			{Hs,[I|Us]};
		{_,_,_} ->
			{[IV|Hs],Us}
		end
	end, {[],Sr#sr.unused}, Sr#sr.hots),
	{Stack,{Cs,Fs}} = mapfoldl(fun(dead, X) ->
		{dead,X};
	(V, {Cs,Fs}) ->
		case vdb_find(V, Vdb) of
		{_,_,Ttl} when Ttl =< T ->
			{Cs1,Fs1} = case keytake(V, 2, Cs) of
			false -> {Cs,Fs};
			{value,{I,V,_},Csa} -> {Csa,[I|Fs]}
			end,
			{dead,{Cs1,Fs1}};
		{_,_,_} ->
			{V,{Cs,Fs}}
		end
	end, {Sr#sr.colds,Us}, Sr#sr.stack),
	Sr#sr{unused=Fs,hots=Hs,colds=Cs,stack=Stack}.

vdb_find(?Tmp=V, _) ->
	{V,0,0};
vdb_find(?Tmp1=V, _) ->
	{V,0,0};
vdb_find(V, Vdb) ->
	case keyfind(V, 1, Vdb) of
	false -> error;
	X -> X
	end.

index(X, Xs) ->
	index(X, Xs, 0).

index(_, [], _) -> false;
index(X, [X|_], I) -> I;
index(X, [_|Xs], I) -> index(X, Xs, I+1).

local_func_label(NameArity, St) ->
	case gb_trees:lookup(NameArity, St#cg.functable) of
	{value,EntryLabel} ->
		{EntryLabel,St};
	none ->
		{EntryLabel,St1} = new_label(St),
		FuncTable = gb_trees:insert(NameArity, EntryLabel, St1#cg.functable),
		{EntryLabel,St1#cg{functable=FuncTable}}
	end.

new_label(#cg{lcount=L}=St) ->
	{L,St#cg{lcount=L+1}}.

new_labels(N, St) ->
	new_labels(N, St, []).

new_labels(0, St, Ls) ->
	{Ls,St};
new_labels(N, #cg{lcount=L}=St, Ls) ->
	new_labels(N-1, St#cg{lcount=L+1}, [L|Ls]).

flatmapfoldl(F, Accu0, [Hd|Tail]) ->
    {R,Accu1} = F(Hd, Accu0),
    {Rs,Accu2} = flatmapfoldl(F, Accu1, Tail),
    {R++Rs,Accu2};
flatmapfoldl(_, Accu, []) -> {[],Accu}.

%% flatmapfoldl version with two parameters
flatmapfoldl2(F, A0, B0, [Hd|Tail]) ->
    {R,A1,B1} = F(Hd, A0, B0),
    {Rs,A2,B2} = flatmapfoldl2(F, A1, B1, Tail),
    {R++Rs,A2,B2};
flatmapfoldl2(_, A, B, []) -> {[],A,B}.

%%EOF
