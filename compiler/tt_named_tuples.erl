-module(tt_named_tuples).
-export([resolve_names/2]).

-import(lists, [map/2,mapfoldl/3,member/2,keytake/3]).

resolve_names(Forms, _Opts) ->
	{ExpandedForms,Nt} = mapfoldl(fun node/2, [], Forms),

	Attrs = map(fun({Name,Fields}) ->
		AtomFields = [erl_syntax:atom(Field) ||  Field <- Fields],
		erl_syntax:attribute(named_tuple,
			[erl_syntax:atom(Name),
			 erl_syntax:tuple(AtomFields)])
	end, Nt),

	Attrs ++ ExpandedForms.

node({named_tuple,Line,Expr,Name,Upds}, Nt) when Name /= '' ->
	{Expr1,Nt1} = node(Expr, Nt),
	{Upds1,Nt2} = mapfoldl(fun({named_tuple_field,La,{atom,_,Field},Value}, Nta) ->
		{Value1,Ntb} = node(Value, add_field(Name, Field, Nta)),
		{{named_tuple_field,La,Field,Value1},Ntb}
	end, Nt1, Upds),
	{{named_tuple,Line,Expr1,Name,Upds1},Nt2};

node({named_tuple,Line,Name,Upds}, Nt) when Name /= '' ->
	{Upds1,Nt1} = mapfoldl(fun({named_tuple_field,La,{atom,_,Field}=A,Value}, Nta) ->
		{Value1,Ntb} = node(Value, add_field(Name, Field, Nta)),
		{{named_tuple_field,La,A,Value1},Ntb}
	end, Nt, Upds),
	{{named_tuple,Line,Name,Upds1},Nt1};

node({named_tuple_field,Line,Expr,Name,{atom,_,Field}=A}, Nt) when Name /= '' ->
	{Expr1,Nt1} = node(Expr, add_field(Name, Field, Nt)),
	{{named_tuple_field,Line,Expr1,Name,A},Nt1};

node({named_tuple_index,_Line,Name,{atom,_,Field}}=Node, Nt) ->
	{Node,add_field(Name, Field, Nt)};
	
node(Nodes, Nt) when is_list(Nodes) ->
	mapfoldl(fun node/2, Nt, Nodes);

node(Node, Nt) when is_tuple(Node) ->
	[What,Line|Nodes] = tuple_to_list(Node),
	{Nodes1,Nt1} = mapfoldl(fun node/2, Nt, Nodes),
	{list_to_tuple([What,Line|Nodes1]),Nt1};

node(Node, Nt) ->
	{Node,Nt}.

add_field(Name, Field, Nt) ->
	case keytake(Name, 1, Nt) of
	{value,{_,Fields},Nt1} ->
		case member(Field, Fields) of
		true ->
			Nt;
		false ->
			Fields1 = [Field|Fields],
			[{Name,Fields1}|Nt1]
		end;
	false ->
		[{Name,[Field]}|Nt]
	end.

%%EOF
