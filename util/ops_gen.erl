-module(ops_gen).
-export([opcodes/1,opcodes/4]).

-define(OP_CODES_START, 0).		%% for faster dispatch this is zero

-include("../compiler/os.hrl").

-import(lists, [mapfoldl/3,reverse/1,map/2,mapfoldl/3,foreach/2,foldl/3]).
-import(lists, [partition/2,keysort/2,duplicate/2,flatmap/2,usort/1]).
-import(string, [strip/3,tokens/2,chr/2,substr/2,substr/3,join/2,to_upper/1]).

opcodes([OpsTab, DefHdr, AsmErl, CaseHints]) ->
	opcodes(OpsTab, DefHdr, AsmErl, CaseHints);

opcodes([OpsTab, DefHdr, AsmErl]) ->
	opcodes(OpsTab, DefHdr, AsmErl, []).

opcodes(OpsTab, DefHdr, AsmErl, CaseHints) ->

	%%
	%% u	unsigned, unit size
	%% d	unsigned, double size
	%%
	
	OpTs = opstab(OpsTab),
	{OpsN,_} = mapfoldl(fun({Mnemo,Args,_}, N) ->
		{{N,Mnemo,numbered_args(Args)},N+1}
	end, ?OP_CODES_START, OpTs),
	Terminals = usort([Mnemo || {Mnemo,_,IsT} <- OpTs, IsT =:= true]),

	%ok = io:format("OpsN=~p~n", [OpsN]),
	write_defines(DefHdr, OpsN),
	write_asm(AsmErl, CaseHints, OpsN, Terminals).

numbered_args(Args) ->
	{NumberedArgs,_} = mapfoldl(fun({Type,Comment}, N) ->
		{{N,Type,Comment},N+1}
	end, 1, Args),
	NumberedArgs.

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
		{Mnemo0,Args} = case chr(Spec, $ ) of
		0 ->
			{Spec,[]};
		N ->
			M = substr(Spec, 1, N-1),
			As = substr(Spec, N+1),
			{M,args(As)}
		end,
		{Mnemo,IsTerminal} = case Mnemo0 of
		"~" ++ X ->
			{X,true};
		X ->
			{X,false}
		end,
		opstab(In, [{Mnemo,Args,IsTerminal}|Ops])
	end.

args(Args0) ->	
	map(fun(A) ->
		case re:run(A,
			"(\\w+)(/(.*))?",
			[{capture,all_but_first,list}]) of
		{match,[Type]} ->
			{list_to_atom(Type),none};
		{match,[Type,_,Comment]} ->
			{list_to_atom(Type),Comment};
		_ ->
			erlang:error({bad_arg_spec,A})
		end
	end, tokens(Args0, ", ")).

write_asm(AsmErl, _CaseHints, OpsN, Terminals) ->
	{ok,Ae} = file:open(AsmErl, write),
	io:format(Ae, "-module(opcodes).~n", []),
	io:format(Ae, "-export([asm/1,is_terminal_op/1]).~n~n", []),
	
	io:format(Ae, "-include(\"os.hrl\").~n~n", []),

	foreach(fun({OpCode,Mnemo,Args}) ->
		BodyWords = pack_args(Args, OpCode),
		case arg_pats(Args) of
		{[],[]} ->
			io:format(Ae, "asm('~s') -> [?xxxu(~w)];~n", [Mnemo,OpCode]);	
		{ArgPats,[]} ->
			io:format(Ae, "asm({'~s',~s}) -> ~s;~n", [Mnemo,ArgPats,BodyWords]);
		{ArgPats,ArgGuars} ->
			io:format(Ae, "asm({'~s',~s}) when ~s -> ~s;~n", [Mnemo,ArgPats,ArgGuars,BodyWords])
		end
	end, OpsN),
	io:format(Ae, "asm(X) -> {badop,X}.~n~n", []),
	
	io:format(Ae, "is_terminal_op(Op) when is_tuple(Op) ->~n", []),
	io:format(Ae, "\tis_terminal_op(element(1, Op));~n", []),
	foreach(fun(Mnemo) ->
		io:format(Ae, "is_terminal_op(~s) -> true;~n", [Mnemo])
	end, Terminals),
	io:format(Ae, "is_terminal_op(_) -> false.~n", []),
	
	io:format(Ae, "~n%%EOF~n", []),
	
	ok = file:close(Ae).

arg_pats([]) ->
	{[],[]};
arg_pats(Args) ->
	{Pats,Guards} = foldl(fun({N,i2,_}, {Pats,Guards}) ->
		P = "{literal," ++ var_name(N) ++ "}",
		Gi = "is_integer(" ++ var_name(N) ++ ")",	%% otherwise, floats pry in
		Gmax = var_name(N) ++ " =< " ++ integer_to_list(?MAX_SIGNED_DOUBLE),
		Gmin = var_name(N) ++ " >= " ++ integer_to_list(?MIN_SIGNED_DOUBLE),
		{Pats ++ [P],Guards ++ [Gi,Gmax,Gmin]};
	({N,i1,_}, {Pats,Guards}) ->
		P = "{literal," ++ var_name(N) ++ "}",
		Gi = "is_integer(" ++ var_name(N) ++ ")",
		Gmax = var_name(N) ++ " =< " ++ integer_to_list(?MAX_SIGNED_UNIT),
		Gmin = var_name(N) ++ " >= " ++ integer_to_list(?MIN_SIGNED_UNIT),
		{Pats ++ [P],Guards ++ [Gi,Gmax,Gmin]};
	({N,u2,_}, {Pats,Guards}) ->
		P = "{literal," ++ var_name(N) ++ "}",
		Gmax = var_name(N) ++ " =< " ++ integer_to_list(?MAX_UNSIGNED_DOUBLE),
		Gmin = var_name(N) ++ " >= 0",
		{Pats ++ [P],Guards ++ [Gmax,Gmin]};
	({N,u1,_}, {Pats,Guards}) ->
		P = "{literal," ++ var_name(N) ++ "}",
		Gmax = var_name(N) ++ " =< " ++ integer_to_list(?MAX_UNSIGNED_UNIT),
		Gmin = var_name(N) ++ " >= 0",
		{Pats ++ [P],Guards ++ [Gmax,Gmin]};
	({N,n,_}, {Pats,Guards}) ->
		P = var_name(N),
		Gmax = var_name(N) ++ " =< " ++ integer_to_list(?MAX_UNSIGNED_UNIT),
		Gmin = var_name(N) ++ " >= 0",
		{Pats ++ [P],Guards ++ [Gmax,Gmin]};
	({N,s,_}, {Pats,Guards}) ->
		P = "{s," ++ var_name(N) ++ "}",
		{Pats ++ [P],Guards};
	({N,s2,_}, {Pats,Guards}) ->
		P = "{s," ++ var_name(N) ++ "}",
		Gmax = var_name(N) ++ " =< " ++ integer_to_list(?MAX_UNSIGNED_DOUBLE),
		Gmin = var_name(N) ++ " >= 0",
		{Pats ++ [P],Guards ++ [Gmax,Gmin]};
	({N,s1,_}, {Pats,Guards}) ->
		P = "{s," ++ var_name(N) ++ "}",
		Gmax = var_name(N) ++ " =< " ++ integer_to_list(?MAX_UNSIGNED_UNIT),
		Gmin = var_name(N) ++ " >= 0",
		{Pats ++ [P],Guards ++ [Gmax,Gmin]};
	({N,a,_}, {Pats,Guards}) ->
		P = "{atom," ++ var_name(N) ++ "}",
		{Pats ++ [P],Guards};
	({N,r,_}, {Pats,Guards}) ->
		P = "{r," ++ var_name(N) ++ "}",
		{Pats ++ [P],Guards};
	({N,l,_}, {Pats,Guards}) ->
		P = "{l," ++ var_name(N) ++ "}",
		{Pats ++ [P],Guards};
	({N,b,_}, {Pats,Guards}) ->
		P = "{bif," ++ var_name(N) ++ "}",
		{Pats ++ [P],Guards};
	({N,t,_}, {Pats,Guards}) ->
		P = "{literal," ++ var_name(N) ++ "}",
		{Pats ++ [P],Guards}
	end, {[],[]}, Args),
	{join(Pats, ","),join(Guards, ", ")}.

var_name(1) -> "X";
var_name(2) -> "Y";
var_name(3) -> "Z";
var_name(4) -> "P";
var_name(5) -> "Q";
var_name(6) -> "R";
var_name(7) -> "T";
var_name(8) -> "S".

pack_args(Args, OpCode) ->
	SizedArgs = map(fun({N,Type,Comment}) ->
		{N,Type,arg_size(Type),Comment}
	end, Args),
	
	{PackableArgs,LargeArgs} =
		partition(fun({_,_,?TEE_SIZE_FULL,_}) -> false; (_) -> true end, SizedArgs),
	
	{_BestSize,BestArgs} = hd(keysort(1, map(fun(Perm) ->
		{pack_size(Perm),Perm}
	end, perms(PackableArgs)))),

	PackList = pack_list(BestArgs ++ LargeArgs, OpCode),
	"[" ++ join(PackList, ", ") ++ "]".	

arg_size(i2) -> ?TEE_SIZE_DOUBLE;
arg_size(i1) -> ?TEE_SIZE_UNIT;
arg_size(u2) -> ?TEE_SIZE_DOUBLE;
arg_size(u1) -> ?TEE_SIZE_UNIT;
arg_size(s) -> ?TEE_SIZE_FULL;
arg_size(s2) -> ?TEE_SIZE_DOUBLE;
arg_size(s1) -> ?TEE_SIZE_UNIT;
arg_size(a) -> ?TEE_SIZE_UNIT;
arg_size(n) -> ?TEE_SIZE_UNIT;
arg_size(r) -> ?TEE_SIZE_UNIT;
arg_size(l) -> ?TEE_SIZE_FULL;
arg_size(b) -> ?TEE_SIZE_FULL;
arg_size(t) -> ?TEE_SIZE_FULL.

pack_size(Args) ->
	{_,Is} = foldl(fun({_,_,Size,_}, {Bs,Is}) when Bs + Size =< ?TEE_SIZE_FULL ->
		{Bs+Size,Is};
	({_,_,Size,_}, {_,Is}) ->
		{Size,Is+1}
	end, {?TEE_SIZE_UNIT,1}, Args),	% single byte for opcode
	Is.

perms([]) -> [[]];
perms(L) ->
	[[H|T] || H <- L, T <- perms(L--[H])].

pack_list(Args, OpCode) ->
	{QChars,QAs,_,Bs} = foldl(fun({N,Type,?TEE_SIZE_FULL,_}, {[],[],0,Bs}) ->
		{[],[],0,Bs ++ [large_arg(N, Type)]};
	({N,Type,?TEE_SIZE_FULL,_}, {QChars,QAs,_QSize,Bs}) ->
		{[],[],0,Bs ++ [quad(QChars, QAs),large_arg(N, Type)]};
	({N,_,Sz,_}, {QChars,QAs,QSize,Bs}) when QSize + Sz =< ?TEE_SIZE_FULL ->
		{qchars(Sz) ++ QChars,[var_name(N)] ++ QAs,QSize+Sz,Bs};
	({N,_Type,Sz,_}, {QChars,QAs,_QSize,Bs}) ->
		{qchars(Sz),[var_name(N)],Sz,Bs ++ [quad(QChars, QAs)]}
	end, {"u",[integer_to_list(OpCode)],?TEE_SIZE_UNIT,[]}, Args),
	if QChars /= [] -> Bs ++ [quad(QChars, QAs)];
		true -> Bs end.

qchars(1) -> "u";
qchars(2) -> "dd".

large_arg(N, i) -> var_name(N);
large_arg(N, a) -> "{atom," ++ var_name(N) ++ "}";
large_arg(N, n) -> var_name(N);
large_arg(N, s) -> var_name(N);
large_arg(N, l) -> "{l," ++ var_name(N) ++ "}";
large_arg(N, b) -> "{bif," ++ var_name(N) ++ "}";
large_arg(N, t) -> "{literal," ++ var_name(N) ++ "}".

quad(QChars, QAs) ->
	Head = "?" ++ duplicate(4-length(QChars), $x) ++ QChars,
	Head ++ "(" ++ join(QAs, ", ") ++ ")".
	
write_defines(DefHdr, OpsN) ->
	{ok,Dh} = file:open(DefHdr, write),
	io:format(Dh, "#ifndef OPCODES_H~n", []),
	io:format(Dh, "#define OPCODES_H~n~n", []),
	
	foreach(fun({OpCode,Mnemo,Args}) ->
		Macro = "OP_" ++ to_upper(Mnemo) ++
			flatmap(fun({_,Type,_Comment}) ->
				"_" ++ to_upper(atom_to_list(Type))
			end, Args),
		io:format(Dh, "#define ~s ~w~n", [Macro,OpCode])
	end, OpsN),
	
	io:format(Dh, "~n#endif~n", []),
	file:close(Dh).

%%EOF
