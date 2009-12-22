-module(typed_tuple).
-compile(export_all).

create_typed_tuples() ->
	T1 = {customer.name <- "Robert", age <- 27},
	T2 = {name <- "Nina"},
	{T1,T2}.

access_typed_tuple_fields(A) ->
	V1 = A.customer.name,
	V2 = A.name,
	{V1,V2}.

update_typed_tuple(A) ->
	A1 = {A.customer.age <- 32},
	A2 = {A.age <- 37},
	{A1,A2}.

match_on_typed_tuple({customer.name <- X}) ->
	X;
match_on_typed_tuple({name <- Y}) ->
	short.

typed_tuple_field_index() ->
	I1 = customer.name,
	{I1}.

%%EOF
