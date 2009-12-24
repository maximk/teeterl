-module(named_tuple).
-compile(export_all).

create_named_tuples() ->
	T1 = {customer#name <- "Robert", age <- 27},
	T2 = {name <- "Nina"},
	T3 = {customer#},
	{T1,T2,T3}.

access_named_tuple_fields(A) ->
	V1 = A.customer#name,
	V2 = A.name,
	{V1,V2}.

update_named_tuple(A) ->
	A1 = {A.customer#age <- 32},
	A2 = {A.age <- 37},
	{A1,A2}.

match_on_named_tuple({customer#name <- X}) ->
	X;
match_on_named_tuple({name <- Y}) ->
	short.

named_tuple_field_index() ->
	I1 = customer#name,
	{I1}.

%%EOF
