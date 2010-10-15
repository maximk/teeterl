%%
%%
%%

-module(tt_asm).
-export([opt_jump_next/1]).
-export([opt_unused_labels/1]).
-export([opt_labeled_jumps/1]).
-export([opt_op_jump_op/1]).
-export([preload/1]).
-export([listing_with_offsets/1]).

-export([format_error/1]).

-export([write_simple_preload/2,write_binary_preload/2,write_text_preload/2]).

-import(lists,[reverse/1,foldl/3,mapfoldl/3,filter/2,usort/1]).
-import(lists,[append/1,map/2,keyfind/3,foreach/2,partition/2,keymember/3]).

-include("teeterl.hrl").

-record(ul,{
	xlab = gb_trees:empty(),	%% L -> Nl
	usage = gb_sets:empty(),	%% label use
	decls = gb_sets:empty(),	%% label declarations
	dups = []	%% list of duplicate declarations
}).

format_error({duplicate_label_declarations,Dups}) ->
	io_lib:format("duplicate label declarations: ~w", [Dups]);
format_error({undeclared_labels,Ls}) ->
	io_lib:format("undeclared labels: ~w", [Ls]).

%%-----------------------------------------------------------------------------

% Remove such sequences:
%
%	{jump,L}
%	{label,L}

opt_jump_next({Mod,Exp,Lambdas,Attr,Asm0}) ->
	Asm = opt_remove_jl(Asm0),
	{ok,{Mod,Exp,Lambdas,Attr,Asm}}.

opt_remove_jl(Asm) ->
	opt_remove_jl(Asm, []).

opt_remove_jl([], Msa) ->
	reverse(Msa);
opt_remove_jl([{jump,{l,L}},{label,L}=LL|Asm], Msa) ->
	opt_remove_jl(Asm, [LL|Msa]);
opt_remove_jl([X|Asm], Msa) ->
	opt_remove_jl(Asm, [X|Msa]).

%%-----------------------------------------------------------------------------

% Remove redundant and unused labels

opt_unused_labels({Mod,Exp,Lambdas,Attr,Asm}) ->

	%%
	%% collapse twin labels and remove unused ones
	%%
	%% pass1 (does not alter instruction list):
	%% build correspondence (L -> Nl)
	%% build usage set
	%% build declarations set
	%% keep list of duplicate declarations
	%%
	%% pass2:
	%% remove twin labels
	%% replace labels in commands
	%%
	
	St = foldl(fun({_NameArity,L}, St) ->
		label_used(L, St)
	end, #ul{}, Exp),
	
	Sta = foldl(fun({0,0}, Sta) ->
		Sta; %% ignore dummy slots
	({_Uniq,L}, Sta) ->
		label_used(L, Sta)
	end, St, Lambdas),
	
	St1 = opt_unused_labels_1(Asm, Sta),
	
	if St1#ul.dups =/= [] ->
		Es = [{none,?MODULE,{duplicate_label_declarations,St1#ul.dups}}],
		{error,Es};
	true ->
		Undecls = gb_sets:subtract(St1#ul.usage, St1#ul.decls),
		Undeclared = gb_sets:to_list(Undecls),
		if Undeclared =/= [] ->
			Es = [{none,?MODULE,{undeclared_labels,Undeclared}}],
			{error,Es};
		true ->
			Unused = gb_sets:subtract(St1#ul.decls, St1#ul.usage),
			Asm1 = opt_unused_labels_2(Asm, Unused, St1),
			
			Exp1 = map(fun({NameArity,L}) ->
				X = xlate_label(L, St1),
				{NameArity,X}
			end, Exp),
			
			Lambdas1 = map(fun({0,0}=Dummy) ->
				Dummy;
			({Uniq,L}) ->
				X = xlate_label(L, St1),
				{Uniq,X}
			end, Lambdas),
			
			{ok,{Mod,Exp1,Lambdas1,Attr,Asm1}}
		end
	end.

opt_unused_labels_1(Asm, St0) ->
	{St1,_Twin} = foldl(fun({label,L}, {St,none}) ->
		{label_declared(L, St),L};
	({label,L}, {St,Twin}) ->
		Sta = label_declared(L, St),
		Xlab = gb_trees:enter(L, Twin, Sta#ul.xlab),
		{Sta#ul{xlab=Xlab},Twin};
	({_OpCode,{l,L}}, {St,_Twin}) ->
		{label_used(L, St),none};
	({_OpCode,_Arg,{l,L}}, {St,_Twin}) ->
		{label_used(L, St),none};
	({_OpCode,_Arg1,_Arg2,{l,L}}, {St,_Twin}) ->
		{label_used(L, St),none};
	({_OpCode,_Arg1,_Arg2,_Arg3,{l,L}}, {St,_Twin}) ->
		{label_used(L, St),none};
	({_OpCode,_Arg1,_Arg2,_Arg3,_Arg4,{l,L}}, {St,_Twin}) ->
		{label_used(L, St),none};
	({_OpCode,_Arg1,_Arg2,_Arg3,_Arg4,_Arg5,{l,L}}, {St,_Twin}) ->
		{label_used(L, St),none};
	({_OpCode,_Arg1,_Arg2,_Arg3,_Arg4,_Arg5,_Arg6,{l,L}}, {St,_Twin}) ->
		{label_used(L, St),none};
	({_OpCode,_Arg1,_Arg2,_Arg3,_Arg4,_Arg5,_Arg6,_Arg7,{l,L}}, {St,_Twin}) ->
		{label_used(L, St),none};
	(_, {St,_Twin}) ->
		{St,none}
	end, {St0,none}, Asm),
	St1.

opt_unused_labels_2(Asm, Unused, St) ->
	opt_unused_labels_2(Asm, Unused, St, none, []).

opt_unused_labels_2([], _Unused, _St, _Twin, Oas) ->
	reverse(Oas);
opt_unused_labels_2([{label,L}|Asm], Unused, St, none, Oas) ->
	case gb_sets:is_member(L, Unused) of
	true ->
		opt_unused_labels_2(Asm, Unused, St, none, Oas);
	false ->
		X = xlate_label(L, St),
		opt_unused_labels_2(Asm, Unused, St, X, [{label,X}|Oas])
	end;
opt_unused_labels_2([{label,_}|Asm], Unused, St, Twin, Oas) ->
	opt_unused_labels_2(Asm, Unused, St, Twin, Oas);
opt_unused_labels_2([OpCodeArgs|Asm], Unused, St, _Twin, Oas) when is_tuple(OpCodeArgs) ->
	Arity = size(OpCodeArgs),
	case element(Arity, OpCodeArgs) of	%% {l,L} is always the last argument
	{l,L} ->
		X = xlate_label(L, St),
		OpCodeArgs1 = setelement(Arity, OpCodeArgs, {l,X}),
		opt_unused_labels_2(Asm, Unused, St, none, [OpCodeArgs1|Oas]);
	_ ->
		opt_unused_labels_2(Asm, Unused, St, none, [OpCodeArgs|Oas])
	end;
opt_unused_labels_2([Op|Asm], Unused, St, _Twin, Oas) ->
	opt_unused_labels_2(Asm, Unused, St, none, [Op|Oas]).
	
xlate_label(L, St) ->
	case gb_trees:lookup(L, St#ul.xlab) of
	{value,X} -> X;
	none -> L end.

label_used(L, St) ->
	Usage = gb_sets:add(L, St#ul.usage),
	St#ul{usage=Usage}.

label_declared(L, St) ->
	case gb_sets:is_member(L, St#ul.decls) of
	true -> % duplicate
		Dups = [L] ++ St#ul.dups,
		St#ul{dups=Dups};
	false ->
		Decls = gb_sets:add(L, St#ul.decls),
		St#ul{decls=Decls}
	end.

%%-----------------------------------------------------------------------------

% Remove such sequences:
%
%	{label,L}
%	{jump,M}

opt_labeled_jumps({Mod,Exp,Lambdas,Attr,Asm0}) ->
	
	{Asm,Subs0} = opt_collect_lj(Asm0, []),

	Subs = combine_subs(Subs0),
	
	Asm1 = map(fun(OpArgs) when is_tuple(OpArgs) ->
		N = tuple_size(OpArgs),
		case element(N, OpArgs) of
		{l,L} ->
			case keyfind(L, 1, Subs) of
			{_,X} ->
				setelement(N, OpArgs, {l,X});
			false -> OpArgs
			end;
		_ ->
			OpArgs
		end;
	(X) ->
		X
	end, Asm),
	
	{ok,{Mod,Exp,Lambdas,Attr,Asm1}}.

opt_collect_lj(Asm, Subs) ->
	opt_collect_lj(Asm, Subs, []).

opt_collect_lj([], Subs, Msa) ->
	{reverse(Msa),Subs};
opt_collect_lj([{label,L},{jump,{l,M}}|Asm], Subs, Msa) ->
	opt_collect_lj(Asm, [{L,M}|Subs], Msa);
opt_collect_lj([X|Asm], Subs, Msa) ->
	opt_collect_lj(Asm, Subs, [X|Msa]).

combine_subs(Subs) ->
	{Roots,Leaves} = partition(fun({L,_}) -> not keymember(L, 2, Subs) end, Subs),
	combine_subs(Roots, Leaves).

combine_subs(Subs, []) ->
	Subs;
combine_subs(Growing, Flying) ->
	{Fly1,Fly2} = partition(fun({I,_}) -> keymember(I, 2, Growing) end, Flying),
	{Grow1,Grow2} = partition(fun({_,J}) -> keymember(J, 1, Flying) end, Growing),

	Extended = [{I1,J2} || {I1,J1} <- Grow1, {I2,J2} <- Fly1, J1 =:= I2],
	
	combine_subs(Grow2 ++ Extended ++ Fly1, Fly2).

%%-----------------------------------------------------------------------------

% Simplify sequence:
%
%	Op
%	{jump,L}
%	Op
%	{label,L}

opt_op_jump_op({Mod,Exp,Lambdas,Attr,Asm0}) ->
	Asm = opt_remove_ojo(Asm0),
	{ok,{Mod,Exp,Lambdas,Attr,Asm}}.

opt_remove_ojo(Asm) ->
	opt_remove_ojo(Asm, []).

opt_remove_ojo([], Msa) ->
	reverse(Msa);
opt_remove_ojo([Op,{jump,{l,L}},Op,{label,L}=LL|Asm], Msa) ->
	opt_remove_ojo(Asm, [Op,LL|Msa]);
opt_remove_ojo([X|Asm], Msa) ->
	opt_remove_ojo(Asm, [X|Msa]).

%%
%% Convert ASM to preloaded format
%%

preload({Mod,Exp,Lambdas,Attr,Asm}) ->
	{Pre,Li,Si} = assemble(Asm),
	
	Pre1 = map(fun({l,L}) ->
		{_,Off} = keyfind(L, 1, Li),
		{'@',Off};
	({atom,A}) when is_atom(A) ->
		{a,A};
	({integer,I}) when is_integer(I) ->
		I;
	({bif,N}) when is_integer(N) ->
		{b,N};
	
	({literal,{'$NT',N,F}}) when is_atom(N), is_atom(F) ->

		%%
		%% Named tuple field reference
		%%	translated into integer when the module is loaded
		%%
	
		{n,{N,F}};

	({literal,L}) ->
	
		%%
		%% TODO: for small numbers we could spit
		%%		 an immediate term as integer here
		%%
		
		{t,L};
	(X) when is_integer(X) ->
		X
	end, Pre),
	
	Exp1 = map(fun({{Name,Arity},L}) ->
		{_,Off} = keyfind(L, 1, Li),
		{Name,Arity,Off}
	end, Exp),
	
	Lambdas1 = map(fun({0,0}=Dummy) ->
		Dummy;
	({Uniq,L}) ->
		{_,Off} = keyfind(L, 1, Li),
		{Uniq,Off}
	end, Lambdas),
	
	%% source line info
	%%		Si -> {{File,Line},Starts,Ends}
	%%
	
	Files = usort([File || {{File,_},_,_} <- Si]),
	SourceRefs = [{index(F, Files),L,S,E} || {{F,L},S,E} <- Si],
	ShortFiles = [filename:basename(File) || File <- Files],
	
	{ok,
		#teeterl{
			module=Mod,
			exports=Exp1,
			funs=Lambdas1,
			attrs=Attr,
			preloaded=Pre1,
			misc=[{files,ShortFiles},
				  {source,SourceRefs}]}}.

index(X, Xs) ->
	index(X, Xs, 0).

index(_, [], _) -> false;
index(X, [X|_], I) -> I;
index(X, [_|Xs], I) -> index(X, Xs, I+1).

assemble(Asm) ->
	assemble(Asm, 0, [], 0, none, [], []).

assemble([], _Off, LabelOffsets, _BlockStarts, _FileLine, SourceInfo, Pre) ->
	{append(reverse(Pre)),LabelOffsets,SourceInfo};

assemble([{label,L}|Asm],
		Off, LabelOffsets, BlockStarts, FileLine, SourceInfo, Pre) ->
	LabelOffsets1 = [{L,Off}|LabelOffsets],
	assemble(Asm, Off, LabelOffsets1, BlockStarts, FileLine, SourceInfo, Pre);

assemble([{source,File,Line}|Asm],
		Off, LabelOffsets, BlockStarts, none, SourceInfo, Pre) ->
	assemble(Asm, Off, LabelOffsets, BlockStarts, {File,Line}, SourceInfo, Pre);

assemble([{source,File,Line}|Asm],
		Off, LabelOffsets, BlockStarts, FileLine, SourceInfo, Pre) ->
	SourceInfo1 = [{FileLine,BlockStarts,Off}|SourceInfo],
	assemble(Asm, Off, LabelOffsets, Off, {File,Line}, SourceInfo1, Pre);

assemble([A|Asm],
		Off, LabelOffsets, BlockStarts, FileLine, SourceInfo, Pre) ->
	case opcodes:asm(A) of
	{badop,X} ->
		io:format("Error: unknown op: ~w~n", [X]),
		assemble(Asm, Off, LabelOffsets, BlockStarts, FileLine, SourceInfo, Pre);
	Os ->
		Off1 = Off+length(Os),
		case opcodes:is_terminal_op(A) of
		true ->
			SourceInfo1 = if FileLine =/= none ->
				[{FileLine,{BlockStarts,Off1}}|SourceInfo];
			true ->
				SourceInfo
			end,
			assemble(Asm, Off1, LabelOffsets, Off1, none, SourceInfo1, [Os|Pre]);
		false ->
			assemble(Asm, Off1, LabelOffsets,
				BlockStarts, FileLine, SourceInfo, [Os|Pre])
		end
	end.

tmpfile(Ofile) ->
    reverse([$#|tl(reverse(Ofile))]).

write_simple_preload(Ofile, Code) ->
    Tfile = tmpfile(Ofile),		%Temp working file

	{ok,Ou} = file:open(Tfile, write),
	
	W = fun({'@',Off}) ->
		io:format(Ou, "@~w", [Off]);
	({a,A}) ->
		io:format(Ou, "&~w", [A]);
	({b,N}) ->
		io:format(Ou, "*~w", [N]);
	(X) ->
		io:format(Ou, "~w", [X])
	end,
	
	foldl(fun(X, N) when N rem 8 =/= 0 ->
		W(X), io:format(Ou, " ", []),
		N+1;
	(X, N) ->
		W(X), io:nl(Ou),
		N+1
	end, 1, Code),
	
	ok = file:close(Ou),
	ok = file:rename(Tfile, Ofile).

write_binary_preload(Ofile, Code) ->
    Tfile = tmpfile(Ofile),		%Temp working file
	{ok,Ou} = file:open(Tfile, write),
	ok = file:write(Ou, term_to_binary(Code)),
	ok = file:close(Ou),
	ok = file:rename(Tfile, Ofile).

write_text_preload(Ofile, #teeterl{module=Mod}=Code) ->

	Bin = term_to_binary(Code),
	Ms = atom_to_list(Mod),

    Tfile = tmpfile(Ofile),		%Temp working file
	{ok,Out} = file:open(Tfile, write),
	io:format(Out, "//~n//~n//~n~n", []),
	io:format(Out, "#include \"modbin.h\"~n~n", []),
	io:format(Out, "modbin_t ~s_modbin = {~n", [Ms]),
	%% (cstr_t *)"\05queue", 0, 15451, {
	io:format(Out, "\t(cstr_t *)\"\\0~.8b~s\", 0, ~w, {~n", [length(Ms),Ms,size(Bin)]),
	hex(Out, Bin),
	io:format(Out, "\t}~n", []),
	io:format(Out, "};~n~n", []),
	io:format(Out, "// EOF~n", []),
	ok = file:close(Out),
	ok = file:rename(Tfile, Ofile).

hex(Out, <<B0,B1,B2,B3,B4,B5,B6,B7,B8,B9,BA,BB,BC,BD,BE,BF,Bin/binary>>) ->
	io:format(Out, "\t\t~4.16x, ~4.16x, ~4.16x, ~4.16x, ~4.16x, ~4.16x, ~4.16x, ~4.16x, "
				   "~4.16x, ~4.16x, ~4.16x, ~4.16x, ~4.16x, ~4.16x, ~4.16x, ~4.16x,~n",
				   [B0,"0x",B1,"0x",B2,"0x",B3,"0x",B4,"0x",B5,"0x",B6,"0x",B7,"0x",
				    B8,"0x",B9,"0x",BA,"0x",BB,"0x",BC,"0x",BD,"0x",BE,"0x",BF,"0x"]),
	hex(Out, Bin);
hex(Out, <<B0,B1,B2,B3,B4,B5,B6,B7,B8,B9,BA,BB,BC,BD,BE,Bin/binary>>) ->
	io:format(Out, "\t\t~4.16x, ~4.16x, ~4.16x, ~4.16x, ~4.16x, ~4.16x, ~4.16x, ~4.16x, "
				   "~4.16x, ~4.16x, ~4.16x, ~4.16x, ~4.16x, ~4.16x, ~4.16x,~n",
				   [B0,"0x",B1,"0x",B2,"0x",B3,"0x",B4,"0x",B5,"0x",B6,"0x",B7,"0x",
				    B8,"0x",B9,"0x",BA,"0x",BB,"0x",BC,"0x",BD,"0x",BE,"0x"]),
	hex(Out, Bin);
hex(Out, <<B0,B1,B2,B3,B4,B5,B6,B7,B8,B9,BA,BB,BC,BD,Bin/binary>>) ->
	io:format(Out, "\t\t~4.16x, ~4.16x, ~4.16x, ~4.16x, ~4.16x, ~4.16x, ~4.16x, ~4.16x, "
				   "~4.16x, ~4.16x, ~4.16x, ~4.16x, ~4.16x, ~4.16x,~n",
				   [B0,"0x",B1,"0x",B2,"0x",B3,"0x",B4,"0x",B5,"0x",B6,"0x",B7,"0x",
				    B8,"0x",B9,"0x",BA,"0x",BB,"0x",BC,"0x",BD,"0x"]),
	hex(Out, Bin);
hex(Out, <<B0,B1,B2,B3,B4,B5,B6,B7,B8,B9,BA,BB,BC,Bin/binary>>) ->
	io:format(Out, "\t\t~4.16x, ~4.16x, ~4.16x, ~4.16x, ~4.16x, ~4.16x, ~4.16x, ~4.16x, "
				   "~4.16x, ~4.16x, ~4.16x, ~4.16x, ~4.16x,~n",
				   [B0,"0x",B1,"0x",B2,"0x",B3,"0x",B4,"0x",B5,"0x",B6,"0x",B7,"0x",
				    B8,"0x",B9,"0x",BA,"0x",BB,"0x",BC,"0x"]),
	hex(Out, Bin);
hex(Out, <<B0,B1,B2,B3,B4,B5,B6,B7,B8,B9,BA,BB,Bin/binary>>) ->
	io:format(Out, "\t\t~4.16x, ~4.16x, ~4.16x, ~4.16x, ~4.16x, ~4.16x, ~4.16x, ~4.16x, "
				   "~4.16x, ~4.16x, ~4.16x, ~4.16x,~n",
				   [B0,"0x",B1,"0x",B2,"0x",B3,"0x",B4,"0x",B5,"0x",B6,"0x",B7,"0x",
				    B8,"0x",B9,"0x",BA,"0x",BB,"0x"]),
	hex(Out, Bin);
hex(Out, <<B0,B1,B2,B3,B4,B5,B6,B7,B8,B9,BA,Bin/binary>>) ->
	io:format(Out, "\t\t~4.16x, ~4.16x, ~4.16x, ~4.16x, ~4.16x, ~4.16x, ~4.16x, ~4.16x, "
				   "~4.16x, ~4.16x, ~4.16x,~n",
				   [B0,"0x",B1,"0x",B2,"0x",B3,"0x",B4,"0x",B5,"0x",B6,"0x",B7,"0x",
				    B8,"0x",B9,"0x",BA,"0x"]),
	hex(Out, Bin);
hex(Out, <<B0,B1,B2,B3,B4,B5,B6,B7,B8,B9,Bin/binary>>) ->
	io:format(Out, "\t\t~4.16x, ~4.16x, ~4.16x, ~4.16x, ~4.16x, ~4.16x, ~4.16x, ~4.16x, "
				   "~4.16x, ~4.16x,~n",
				   [B0,"0x",B1,"0x",B2,"0x",B3,"0x",B4,"0x",B5,"0x",B6,"0x",B7,"0x",
				    B8,"0x",B9,"0x"]),
	hex(Out, Bin);
hex(Out, <<B0,B1,B2,B3,B4,B5,B6,B7,B8,Bin/binary>>) ->
	io:format(Out, "\t\t~4.16x, ~4.16x, ~4.16x, ~4.16x, ~4.16x, ~4.16x, ~4.16x, ~4.16x, "
				   "~4.16x,~n",
				   [B0,"0x",B1,"0x",B2,"0x",B3,"0x",B4,"0x",B5,"0x",B6,"0x",B7,"0x",
				    B8,"0x"]),
	hex(Out, Bin);
hex(Out, <<B0,B1,B2,B3,B4,B5,B6,B7,Bin/binary>>) ->
	io:format(Out, "\t\t~4.16x, ~4.16x, ~4.16x, ~4.16x, ~4.16x, ~4.16x, ~4.16x, ~4.16x,~n",
				   [B0,"0x",B1,"0x",B2,"0x",B3,"0x",B4,"0x",B5,"0x",B6,"0x",B7,"0x"]),
	hex(Out, Bin);
hex(Out, <<B0,B1,B2,B3,B4,B5,B6,Bin/binary>>) ->
	io:format(Out, "\t\t~4.16x, ~4.16x, ~4.16x, ~4.16x, ~4.16x, ~4.16x, ~4.16x,~n",
				   [B0,"0x",B1,"0x",B2,"0x",B3,"0x",B4,"0x",B5,"0x",B6,"0x"]),
	hex(Out, Bin);
hex(Out, <<B0,B1,B2,B3,B4,B5,Bin/binary>>) ->
	io:format(Out, "\t\t~4.16x, ~4.16x, ~4.16x, ~4.16x, ~4.16x, ~4.16x,~n",
				   [B0,"0x",B1,"0x",B2,"0x",B3,"0x",B4,"0x",B5,"0x"]),
	hex(Out, Bin);
hex(Out, <<B0,B1,B2,B3,B4,Bin/binary>>) ->
	io:format(Out, "\t\t~4.16x, ~4.16x, ~4.16x, ~4.16x, ~4.16x,~n",
				   [B0,"0x",B1,"0x",B2,"0x",B3,"0x",B4,"0x"]),
	hex(Out, Bin);
hex(Out, <<B0,B1,B2,B3,Bin/binary>>) ->
	io:format(Out, "\t\t~4.16x, ~4.16x, ~4.16x, ~4.16x,~n",
				   [B0,"0x",B1,"0x",B2,"0x",B3,"0x"]),
	hex(Out, Bin);
hex(Out, <<B0,B1,B2,Bin/binary>>) ->
	io:format(Out, "\t\t~4.16x, ~4.16x, ~4.16x,~n",
				   [B0,"0x",B1,"0x",B2,"0x"]),
	hex(Out, Bin);
hex(Out, <<B0,B1,Bin/binary>>) ->
	io:format(Out, "\t\t~4.16x, ~4.16x,~n",
				   [B0,"0x",B1,"0x"]),
	hex(Out, Bin);
hex(Out, <<B0,Bin/binary>>) ->
	io:format(Out, "\t\t~4.16x,~n",
				   [B0,"0x"]),
	hex(Out, Bin);
hex(_Out, <<>>) ->
	true.

listing_with_offsets({_Mod,Exp,_Lambdas,_Attr,Asm}) ->
	%io:format("Exp=~p~n", [Exp]),

	{AsmOffs,_} = mapfoldl(fun({label,_}=LL, Off) ->
		{{Off,LL},Off};

	({source,_,_}=SS, Off) ->
		{{Off,SS},Off};

	(A, Off) ->
		case opcodes:asm(A) of
		{badop,X} ->
			io:format("Error: unknown op: ~w~n", [X]),
			{{Off,A}, Off};
		Os ->
			{{Off,A}, Off+length(Os)}
		end
	end, 0, Asm),

	foreach(fun({Off,{label,L}}) ->
		case keyfind(L, 2, Exp) of
		false ->
			io:format("~6w > ~w~n", [Off,L]);
		{{Name,Arity},_} ->
			io:format("~6w~n~6w > ~w ~w/~w~n~6w~n", [Off,Off,L,Name,Arity,Off])
		end;
	
	({_Off,{source,File,Line}}) ->
		io:format("~s:~w:~n", [File,abs(Line)]);

	({Off,OpArgs}) when is_tuple(OpArgs), tuple_size(OpArgs) >= 1 ->
		[Op|Args] = tuple_to_list(OpArgs),
		Args1 = [pretty_arg(A) || A <- Args],
		S = string:join(Args1, ", "),
		%io:format("~6w\t\t~w\t~s~n", [Off,Op,S]);
		io:format("~6w\t\t~w\t~s -- ~w~n", [Off,Op,S,opcodes:asm(OpArgs)]);

	({Off,Op}) when is_atom(Op) ->
		io:format("~6w\t\t~w~n", [Off,Op])
	end, AsmOffs).

pretty_arg({r,I}) ->
	io_lib:format("r~w", [I]);
pretty_arg({s,N}) ->
	io_lib:format("S[~w]", [N]);
pretty_arg({l,L}) ->
	io_lib:format(">~w", [L]);
pretty_arg({integer,N}) ->
	io_lib:format("~w", [N]);
pretty_arg({atom,N}) ->
	atom_to_list(atoms:std_atom(N));
pretty_arg({literal,X}) ->
	io_lib:format("~p", [X]);
pretty_arg({bif,I}) ->
	{M,F,_} = bifs:bif_spec(I),
	io_lib:format("~w:~w", [M,F]);
pretty_arg(X) ->
	io_lib:format("~w", [X]).

%%EOF
