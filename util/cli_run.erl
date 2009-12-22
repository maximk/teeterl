-module(cli_run).
-compile(export_all).

exec1([Mod,Fun,A]) ->
	M = list_to_atom(Mod),
	F = list_to_atom(Fun),
	M:F(A).

%%EOF
