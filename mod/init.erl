%%
%%
%%

-module(init).
-export([main/1]).
-export([navel_request/2]).

-import(lists, [concat/1]).

-include("navel.hrl").
-include("trace.hrl").

%%
%% TODO:
%%
%% stdout with comet
%% tests
%% gc
%% bignums
%%

main(_Args) ->
	register(init, self()),

	comet:start(),
	navel:start(#navconf{www_root="navel",
						 mod_path="xbin",
						 handlers=[init,test,comet]}),

	% mostly traces dispatch
	loop().

loop() ->
	receive
	#postmortem{result=?SLICE_RESULT_DONE} ->
		loop();
	#postmortem{result=?SLICE_RESULT_EXIT,reason=normal} ->
		loop();
	#postmortem{}=Pm ->
		comet:post_message("/sys/scheduler/exit", Pm),
		erlang:display(concat(["--- exited [",Pm#postmortem.reg_name,"]\n"])),
		loop()
	end.

navel_request(#navreq{what="/fish"} = Req, _Config) ->
	comet:post_message("/std/out", perch),
	navel:r(Req, ok, [], "ok");
navel_request(_, _) -> false.

%%EOF
