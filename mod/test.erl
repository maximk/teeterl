-module(test).
-export([run_file/1]).
-export([fail/1]).
-export([lookup_config/2]).
-export([navel_request/2]).

-import(lists, [concat/1]).

-include("navel.hrl").

%%-----------------------------------------------------------------------------

dissect() ->
	{car#model <- coo1(), mpg <- 30}.

coo1() ->
	"Honda".
	
%%-----------------------------------------------------------------------------

run_file(File) ->		%% HTML

	case file:read_file(File) of
	{ok,Bin} ->
		case code:load_module(Bin) of
		{module,Module} ->
			spawn(fun() ->
				Msg = case run_tests(Module, []) of
				{0,Total} ->
					concat([Total," test(s) run successfully"]);
				{Failed,Total} ->
					concat([Failed," out of ",Total," test(s) failed"])
				end,
				report(Msg)
			end),
			"ok";
		badfile ->
			concat(["Bad file"])
		end;
	{error,_} ->
		concat([File," not found"])
	end.

run_tests(Module, Config) ->	%% {Failed,Total}
	run_tests(Module, Config, [all], 0, 0).

run_tests(_Module, _Config, [], Failed, Total) ->
	{Failed,Total};
run_tests(Module, Config, [Test|Tests], Failed, Total) ->
	case catch Module:Test(suite) of
	[] ->
		F = case catch Module:Test(Config) of
		{'EXIT',Reason} ->

			case get(test_loc) of
			undefined ->
				Msg = {Test,failed_with_reason,Reason},
				report(Msg);
			ML ->
				Msg = {Test,failed_at,ML,with_reason,Reason},
				report(Msg)
			end,
			1;
		{comment,Comment} ->
			comet:post_message("/std/out", Comment),
			report({Test,ok}),
			0;
		ok ->
			report({Test,ok}),
			0
		end,
		run_tests(Module, Config, Tests, Failed+F, Total+1);
		
	More when is_list(More) ->
		run_tests(Module, Config, More ++ Tests, Failed, Total);
		
	_ ->
		%% TODO: bark
		run_tests(Module, Config, Tests, Failed, Total)
	end.

fail(_) -> todo.

lookup_config(_What, _Config) -> todo.

report(Msg) ->
	%%erlang:display(Msg ++ "\n"),
	comet:post_message("/test/out", Msg).

navel_request(#navreq{what="/test/run"} = Req, _) ->
	Results = test:run_file(Req#navreq.params),
	navel:r(Req, ok, [], Results);
navel_request(#navreq{what="/test/dissect"} = Req, _) ->
	R = (catch dissect()),
	navel:r(Req, ok, [], format:to_json(R));
navel_request(_, _) -> false.
	
%%EOF
