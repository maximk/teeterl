-module(cli_run).
-compile(export_all).

exec1([Mod,Fun,A]) ->
	M = list_to_atom(Mod),
	F = list_to_atom(Fun),
	M:F(A).

unload_mods(Mods) ->
	lists:foreach(fun(Mod) ->
		M = list_to_atom(Mod),
		code:purge(M),
		code:delete(M)
	end, Mods).

%%EOF
