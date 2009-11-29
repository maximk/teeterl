-module(ops_gen).
-export([opcodes/1,opcodes/3]).

-import(string,[chr/2,strip/3,substr/2,substr/3,tokens/2]).
-import(lists,[reverse/1,seq/2,map/2,flatmap/2,foldl/2,usort/1,foldl/3,mapfoldl/3]).
-import(lists,[all/2,foreach/2,filter/2]).
-import(string,[to_upper/1,join/2]).

-define(OP_CODES_START, 0).		%% faster dispatch this is zero

opcodes([OpsTab, DefHdr, AsmErl]) ->
	opcodes(OpsTab, DefHdr, AsmErl).

%% move Rx, Ry
%% move R[0..8], Rx
%% move R[0..8], R[0..8]

opcodes(OpsTab, DefHdr, AsmErl) ->

	%% {"move",[{r,{0,15},"dst_reg"},{r,{0,15},"src_reg"}]}
	%%
	%% -define(OP_MOVE, 98).
	%% -define(OP_MOVE_R0_R1, 101).
	%% {move,{r,X},{r,Y}} -> [?OP_MOVE,X,Y].
	%% {move,{r,0},{r,1}} -> [?OP_MOVE_0_1].

	%% #define op_move_r0_r1	101
	%% #define op_move			98
	
	Ops = foldl(fun(F, Ops) ->
		flatmap(fun({OpCode,Args}) ->
			[{OpCode,As} || As <- F(Args)]
		end, Ops)
	end, opstab(OpsTab), [fun frozen/1, fun ranged/1]),
	
	{OpsN,_} = mapfoldl(fun({OpCode,Args}, N) ->
		{{N,OpCode,Args},N+1}
	end, ?OP_CODES_START, Ops),
	
	%ok = io:format("OpsN=~p~n", [OpsN]),
	
	write_defines(DefHdr, OpsN),
	write_asm(AsmErl, OpsN).

frozen([]) ->
	[[]];
frozen([{Type,none,Desc}|Args]) ->
	[[{Type,any,Desc}|As] || As <- frozen(Args)];
frozen([{Type,_,Desc}=A|Args]) ->
	[[A|As] || As <- frozen(Args)] ++
	[[{Type,any,Desc}|As] || As <- frozen(Args)].

ranged([]) ->
	[[]];
ranged([{_,any,_}=A|Args]) ->
	[[A|As] || As <- ranged(Args)];
ranged([{Type,{Min,Max},Desc}|Args]) ->
	flatmap(fun(N) ->
		[[{Type,N,Desc}|As] || As <- ranged(Args)]
	end, seq(Min, Max)).

opstab(OpsTab) ->
	{ok,In} = file:open(OpsTab, read),
	opstab(In, []).

opstab(In, Ops) ->
	case io:get_line(In, '') of
	eof ->
		file:close(In),
		reverse(Ops);
	
	"\n" ->
		opstab(In, Ops);
	"#" ++ _ ->
		opstab(In, Ops);
	
	Spec0 ->
		Spec = strip(Spec0, right, $\n),
		case chr(Spec, $ ) of
		0 ->
			opstab(In, [{Spec,[]}|Ops]);
		N ->
			OpCode = substr(Spec, 1, N-1),
			Args = substr(Spec, N+1),
			opstab(In, [{OpCode,args(Args)}|Ops])
		end
	end.

args(Args0) ->	
	map(fun(A) ->
		case re:run(A,
			"(\\w+)(\\[(\\d+)(\\.\\.(\\d+))?\\])?(/(.*))?",
			[{capture,all_but_first,list}]) of
		{match,[Type]} ->
			{list_to_atom(Type),none,none};
		{match,[Type,[],_,_,_,_,Comment]} ->
			{list_to_atom(Type),none,Comment};
		{match,[Type,_,Val,[],[],_,Comment]} ->
			N = list_to_integer(Val),
			{list_to_atom(Type),{N,N},Comment};
		{match,[Type,_,Min,_,Max,_,Comment]} ->
			Lower = list_to_integer(Min),
			Upper = list_to_integer(Max),
			{list_to_atom(Type),{Lower,Upper},Comment};
		{match,[Type,_,Min,_,Max]} ->
			Lower = list_to_integer(Min),
			Upper = list_to_integer(Max),
			{list_to_atom(Type),{Lower,Upper},[]}
		end
	end, tokens(Args0, ", ")).

macro(OpCode, Args) ->
	case all(fun({_,any,_}) -> true; (_) -> false end, Args) of
	true ->
		%% all operands are arbitrary; use OP_MOVE
		"OP_" ++ to_upper(OpCode);
	false ->
		%% some operands are specific; use OP_MOVE_RX_R1
		"OP_" ++ to_upper(OpCode) ++
		flatmap(fun({r,any,_}) -> "_RX";
		({r,X,_}) -> "_R" ++ integer_to_list(X);
		({n,any,_}) -> "_N";
		({n,X,_}) -> "_" ++ integer_to_list(X);
		({a,_,_}) -> "_A";
		({t,_,_}) -> "_T";
		({l,_,_}) -> "_L";
		({bif,_,_}) -> "_B";
		(_) -> "_#WHAT?#"
		end, Args)
	end.

write_defines(DefHdr, OpsN) ->
	{ok,Dh} = file:open(DefHdr, write),
	io:format(Dh, "#ifndef OPCODES_H~n", []),
	io:format(Dh, "#define OPCODES_H~n~n", []),
	
	foreach(fun({N,OpCode,Args}) ->
		Macro = case all(fun({_,any,_}) -> true; (_) -> false end, Args) of
		true ->
			%% all operands are arbitrary; use OP_MOVE
			"OP_" ++ to_upper(OpCode);
		false ->
			%% some operands are specific; use OP_MOVE_RX_R1
			"OP_" ++ to_upper(OpCode) ++
			flatmap(fun({r,any,_}) -> "_RX";
			({r,X,_}) -> "_R" ++ integer_to_list(X);
			({n,any,_}) -> "_N";
			({n,X,_}) -> "_" ++ integer_to_list(X);
			({a,_,_}) -> "_A";
			({t,_,_}) -> "_T";
			({l,_,_}) -> "_L";
			({bif,_,_}) -> "_B";
			(_) -> "_#WHAT?#"
			end, Args)
		end,
		io:format(Dh, "#define ~s ~w~n", [Macro,N])
	end, OpsN),
	
	io:format(Dh, "~n#endif~n", []),
	file:close(Dh).

%% -define(OP_MOVE, 98).
%% -define(OP_MOVE_R0_R1, 101).
%% {move,{r,X},{r,Y}} -> [?OP_MOVE,X,Y].
%% {move,{r,0},{r,1}} -> [?OP_MOVE_0_1].

write_asm(AsmErl, OpsN) ->
	{ok,Ae} = file:open(AsmErl, write),
	io:format(Ae, "-module(opcodes).~n", []),
	io:format(Ae, "-compile(export_all).~n~n", []),

	foreach(fun({N,OpCode,Args}) ->
		Macro = macro(OpCode, Args),
		io:format(Ae, "-define(~s, ~w).~n", [Macro,N])
	end, OpsN),
	io:nl(Ae),
	foreach(fun({_,OpCode,[]}) ->
		Macro = macro(OpCode, []),
		io:format(Ae, "asm(~s) -> [?~s].~n", [OpCode,Macro]);
	({_,OpCode,Args}) ->
		Macro = macro(OpCode, Args),
		{Pats,_} = mapfoldl(fun({r,any,_}, X) -> {"{r," ++ var_name(X) ++ "}",X+1};
			({r,R,_}, X) -> {"{r," ++ integer_to_list(R) ++ "}",X+1};
			({n,any,_}, X) -> {var_name(X),X+1};
			({n,I,_}, X) -> {integer_to_list(I),X+1};
			({a,any,_}, X) -> {"{atom," ++ var_name(X) ++ "}",X+1};
			({t,any,_}, X) -> {"{literal," ++ var_name(X) ++ "}",X+1};
			({l,any,_}, X) -> {"{l,L}",X+1};
			({bif,any,_}, X) -> {"{bif,N}",X+1};
			(_, X) -> {"_#WHAT?#",X+1}
		end, 0, Args),
		Head = "asm({" ++ OpCode ++ "," ++ join(Pats, ",") ++ "})",
		{Vals,_} = mapfoldl(fun({r,any,_}, X) -> {var_name(X),X+1};
			({n,any,_}, X) -> {var_name(X),X+1};
			({a,any,_}, X) -> {"{a," ++ var_name(X) ++ "}",X+1};
			({t,any,_}, X) -> {"{t," ++ var_name(X) ++ "}",X+1};
			({l,any,_}, X) -> {"{l,L}",X+1};
			({bif,any,_}, X) -> {"{bif,N}",X+1};
			(_, X) -> {[],X+1}
		end, 0, Args),
		MacroVals = filter(fun([]) -> false; (_) -> true end, [Macro|Vals]),
		Body = "[?" ++ join(MacroVals, ",") ++ "]",
		io:format(Ae, "~s -> ~s;~n", [Head,Body])
	end, OpsN),
	io:format(Ae, "asm(X) -> {badop,X}.~n", []),
	
	io:format(Ae, "~n%%EOF~n", []),
	file:close(Ae).

var_name(0) -> "X";
var_name(1) -> "Y";
var_name(2) -> "Z";
var_name(3) -> "P";
var_name(4) -> "Q";
var_name(5) -> "R".

%%EOF
