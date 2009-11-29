%%
%%
%%
-module(mods_gen).
-export([modules/1,modules/2]).

-import(lists,[foreach/2]).

modules([IncFile|Mods]) ->
	modules(IncFile, Mods).

modules(IncFile, Mods) ->

	Modules = lists:map(fun(M) ->
		NameExt = lists:last(string:tokens(M, "/")),
		[Name,_Ext] = string:tokens(NameExt, "."),
		list_to_atom(Name)
	end, Mods),

	{ok,Out} = file:open(IncFile, write),
	foreach(fun(M) ->
		io:format(Out, "extern modbin_t ~w_modbin;~n", [M])
	end, Modules),
	io:format(Out, "~nmodbin_t *modbins[] = {~n", []),
	foreach(fun(M) ->
		io:format(Out, "\t&~w_modbin,~n", [M])
	end, Modules),
	io:format(Out, "\t0,~n};~n", []),
	file:close(Out).

%%EOF
