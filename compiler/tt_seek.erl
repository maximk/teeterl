%%
%%
%%
-module(tt_seek).
-export([module/2]).

-import(lists, [foreach/2]).

-include("v3_life.hrl").

%% seeks v3_life tree for protected
%% expressions with non-empty resull list
%% no such beast found to date

module({_,_,_,Forms}, _) ->
	seek(Forms),
	{error,stop}.

seek(Ts) when is_list(Ts) ->
	foreach(fun(T) ->
		seek(T)
	end, Ts);
seek({protected,B,[]}) ->
	seek(B);
seek({protected,B,_}) ->
	io:format("~w~n", [B#l.a]);
seek(T) when is_tuple(T) ->
	foreach(fun(E) ->
		seek(E)
	end, tuple_to_list(T));
seek(_) -> ok.

%%EOF
