%%
%%
%%
-module(comet).
-export([start/0,stop/0]).
-export([post_message/2,get_messages/1,get_messages/2]).
-export([navel_request/2]).

-import(lists, [foreach/2,filter/2,concat/1]).

-export([loop/0]).

-include("navel.hrl").

-record(comet, {next_id = 0,
				seen = -1,
				messages = [],
				pollers = []}).

start() ->
	Pid = spawn(?MODULE, loop, []),
	register(comet, Pid).

stop() ->
	comet ! {stop,self()}.

post_message(Channel, Data) ->
	comet ! {msg,self(),Channel,Data},
	ok.

get_messages(Timeout) ->
	get_messages(-1, Timeout).

get_messages(LastSeen, Timeout) ->
	comet ! {poll,self(),LastSeen},
	receive
	{comet,_From,Messages} ->
		{ok,Messages}
	after Timeout ->
		timeout
	end.

loop() ->
	loop(#comet{}).

loop(State) ->
	receive
	{msg,_From,Channel,Data} ->
	
		NextId = State#comet.next_id,
		Msg = {NextId,Channel,Data},
		Messages = [Msg|State#comet.messages],
		
		loop(broadcast(State#comet{next_id = NextId+1, messages = Messages}));
		
	{poll,From,Id} ->
	
		Pollers = [From|State#comet.pollers],
		LastSeen = if Id > State#comet.seen -> Id;
			true -> State#comet.seen end,
		
		loop(broadcast(State#comet{pollers = Pollers, seen = LastSeen}));

	{stop,_} ->
		done
	end.

broadcast(State) ->		%% State

	LastSeen = State#comet.seen,
	UnseenMessages = filter(fun({Id,_,_}) ->
		Id > LastSeen
	end, State#comet.messages),
	
	if UnseenMessages =/= [], State#comet.pollers =/= [] ->
		foreach(fun(Pid) ->
			Pid ! {comet,self(),UnseenMessages}
		end, State#comet.pollers),
		State#comet{messages = [], pollers = []};
	
	true ->
		State#comet{messages = UnseenMessages}
	end.

format_messages(Messages) ->
	Ms = [concat(["{","id:",Id,",",
		"channel:'",Channel,"',",
		"data:",format:to_json(Data),"}"]) || {Id,Channel,Data} <- Messages],
	["[",string:join(Ms, ","),"]"].

navel_request(#navreq{what="/comet",params=Params}=Req, _Config) ->
	LastSeen = if Params =/= "" -> list_to_integer(Params);
		true -> -1 end,
	case comet:get_messages(LastSeen, 30000) of
	{ok,Messages} ->
		navel:r(Req, ok, [], format_messages(Messages));
	timeout ->
		navel:r(Req, ok, [], "[]")
	end;
navel_request(_, _) -> false.

%%EOF
