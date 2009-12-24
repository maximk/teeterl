-module(tt_named_tuples).
-export([attributes/2]).

-import(lists, [map/2,foldl/3,member/2,keytake/3]).

%% {attribute, Pos, module, {Name,Vars}}
%% {attribute, Pos, module, Name}

attributes(Forms, _Opts) ->
	Nt = foldl(fun node/2, [], Forms),
	map(fun({Name,Fields}) ->
		AtomFields = [{atom,0,Field} ||  Field <- Fields],
		{attribute,0,named_tuple,{{atom,0,Name},AtomFields}}
	end, Nt).

node({named_tuple,_,Expr,Name,Upds}, Nt) when Name /= '' ->
	Nt1 = node(Expr, Nt),
	foldl(fun({named_tuple_field,_,{atom,_,Field},Value}, Nta) ->
		node(Value, add_field(Name, Field, Nta))
	end, Nt1, Upds);

node({named_tuple,_,Name,Upds}, Nt) when Name /= '' ->
	foldl(fun({named_tuple_field,_,{atom,_,Field},Value}, Nta) ->
		node(Value, add_field(Name, Field, Nta))
	end, Nt, Upds);

node({named_tuple_field,_,Expr,Name,{atom,_,Field}}, Nt) when Name /= '' ->
	node(Expr, add_field(Name, Field, Nt));

node({named_tuple_index,_,Name,{atom,_,Field}}, Nt) ->
	add_field(Name, Field, Nt);
	
node(Nodes, Nt) when is_list(Nodes) ->
	foldl(fun node/2, Nt, Nodes);

node(Node, Nt) when is_tuple(Node) ->
	[_What,_Line|Nodes] = tuple_to_list(Node),
	foldl(fun node/2, Nt, Nodes);

node(_Node, Nt) ->
	Nt.

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
