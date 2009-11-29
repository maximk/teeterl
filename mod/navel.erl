%%
%%
%%
-module(navel).
-export([start/1]).
-export([r/4]).

-import(lists, [concat/1,reverse/1]).
-import(format, [from_hex/2]).

-export([req1/2,req2/2,req3/2,req4/2]).

-include("navel.hrl").

start(Config) ->
	
	case gen_tcp:listen(Config#navconf.port, []) of
	{ok,Listener} ->
	
		spawn(fun() -> acceptor(Listener, Config) end),
		
		{ok,Listener};

	{error,_}=E -> E
	end.

acceptor(Listener, Config) ->

	case gen_tcp:accept(Listener) of
	{ok,Sock} ->
	
		spawn(fun() -> acceptor(Listener, Config) end),
		
		requestor(Sock, Config);
	
	{error,closed} -> ok
	end.

requestor(Sock, Config) ->
	requestor(Sock, Config, []).

requestor(Sock, Config, Pre) ->

	case request(Sock, Pre) of
	done ->
		ok;
		
	{Req,Pre1} ->
		
		case catch handle_request(Req, Config) of
		done ->
			requestor(Sock, Config, Pre1);
		false ->
			do_request(Req, Config);
		{'EXIT',Reason} ->
			handler_fails(Req, Reason, Config)
		end,
		
		requestor(Sock, Config, Pre1)
	end.

handle_request(Req, Config) ->
	handle_request(Req, Config#navconf.handlers, Config).

handle_request(_Req, [], _Config) ->
	false;
handle_request(Req, [Handler|Other], Config) when is_function(Handler, 2) ->
	case Handler(Req, Config) of
	false -> handle_request(Req, Other, Config);
	X -> X
	end;
handle_request(Req, [Handler|Other], Config) when is_atom(Handler) ->
	case Handler:navel_request(Req, Config) of
	false -> handle_request(Req, Other, Config);
	X -> X
	end.

request(Sock, Pre) ->
	request(Sock, Pre, [], req1).

request(Sock, [], Post, State) ->
	case gen_tcp:recv(Sock) of
	{ok,Data} ->
		Pre = binary_to_list(Data),
		request(Sock, Pre, Post, State);
	{error,closed} ->
		done;
	{error,E} ->
		erlang:error(E)
	end;

request(Sock, Pre, Post, State) ->
	case ?MODULE:State(Pre, Post) of
	{more,State1,Post1} ->
		request(Sock, [], Post1, State1);
	
	{done,Pre1,Post1} ->
		{Meth,What,Params,Vsn,Hdrs} = status_headers(Post1),

		Value = proplists:get_value("content-length", Hdrs, "0"),
		BodySize = list_to_integer(Value),
		{Body,Pre2} = body(Sock, Pre1, BodySize),
		
		{#navreq{sock=Sock,
			method=Meth,
			what=expand(What),
			params=Params,
			vsn=Vsn,
			headers=Hdrs,
			body=Body},Pre2}
	end.

% req1 - initial
% req2 - \r read
% req3 - \r\n read
% req4 - \r\n\r read

req1("\r" ++ Pre, Post) -> req2(Pre, "\r" ++ Post);
req1([Any|Pre], Post) -> req1(Pre, [Any|Post]);
req1([], Post) -> {more,req1,Post}.

req2("\n" ++ Pre, Post) -> req3(Pre, "\n" ++ Post);
req2("\r" ++ Pre, Post) -> req2(Pre, "\r" ++ Post);
req2([Any|Pre], Post) -> req1(Pre, [Any|Post]);
req2([], Post) -> {more,req2,Post}.

req3("\r" ++ Pre, Post) -> req4(Pre, "\r" ++ Post);
req3([Any|Pre], Post) -> req1(Pre, [Any|Post]);
req3([], Post) -> {more,req3,Post}.

req4("\n" ++ Pre, Post) -> {done,Pre,reverse("\n" ++ Post)};
req4("\r" ++ Pre, Post) -> req2(Pre, "\r" ++ Post);
req4([Any|Pre], Post) -> req1(Pre, [Any|Post]);
req4([], Post) -> {more,req4,Post}.

status_headers(S) -> %% {Meth,What,Vsn,Hdrs}
	{ok,[ReqLine|HdrLines]} = regexp_split(S, "\r\n"),
	{Meth,WP,Vsn} = req_line(ReqLine),
	
	[What,Params] = case string:tokens(WP, "?") of
		[W,P] -> [W,P];
		[W] -> [W,[]]
	end,	

	Hdrs = hdr_lines(HdrLines),

	{Meth,What,Params,Vsn,Hdrs}.

req_line(Line) ->
	[Verb,What,Proto0] = string:tokens(Line, " "),
	[_,Vs] = string:tokens(Proto0, "/"),
	[Major,Minor] = string:tokens(Vs, "."),
	Proto = {list_to_integer(Major),list_to_integer(Minor)},
	{Verb,What,Proto}.

hdr_lines(Lines) ->
	[{string:to_lower(Key),Value}
		|| {ok,[Key,Value]} <- [regexp_split(Line, ": ")
			|| Line <- Lines, Line =/= []]].

body(Sock, Chunk, N) ->
	body(Sock, Chunk, N, []).
	
body(_Sock, Chunk, 0, Body) ->
	{concat(reverse(Body)),Chunk};
body(Sock, [], N, Body) ->
	case gen_tcp:recv(Sock) of
	{ok,Bin} ->
		body(Sock, binary_to_list(Bin), N, Body);
	{error,E} ->
		erlang:error(E)
	end;
body(Sock, Chunk, N, Body) when length(Chunk) =< N ->
	body(Sock, [], N-length(Chunk), [Chunk|Body]);
body(Sock, LargeChunk, N, Body) ->
	{Chunk,Tail} = lists:split(N, LargeChunk),
	body(Sock, Tail, 0, [Chunk|Body]).

regexp_split(Text, [_,_]=Sep) ->
	regexp_split(Text, Sep, [], []).

regexp_split([S1,S2|Text], [S1,S2], Txet, Pieces) ->
	regexp_split(Text, [S1,S2], [], [reverse(Txet)|Pieces]);
regexp_split([X|Text], Sep, Txet, Pieces) ->
	regexp_split(Text, Sep, [X|Txet], Pieces);
regexp_split([], _, [], Pieces) ->
	{ok,reverse(Pieces)};
regexp_split([], _, Txet, Pieces) ->
	Last = reverse(Txet),
	{ok,reverse([Last|Pieces])}.

expand(L) -> expand(L, []).
expand([$%,H1,H2|L], R) -> expand(L, [from_hex(H1, H2)|R]);
expand([$+|L], R) -> expand(L, [$ |R]);
expand([H|L], R) -> expand(L, [H|R]);
expand([], R) -> lists:reverse(R).

handler_fails(Req, Reason, _Config) ->
	comet:post_message("/navel/handler_error", Reason),
	r(Req, request_failed, [], "fail").

do_request(Req, Config) ->

	What = Req#navreq.what,
    File = if What =/= "/" -> What;
		true -> "/index.html" end,
	
	PathFile = Config#navconf.www_root ++ File,
    case file:read_file(PathFile) of
    {ok,Bin} ->
        r(Req, ok, [], Bin);
    {error,_} ->
        r(Req, 404, [], "<html><body>Not found</body></html>")
    end.

r(Req, Status, Hdrs, Body) ->
	Body1 = if is_list(Body) ->
		list_to_binary(Body);
	true ->
		Body
	end,
	
	{NumStat,TxtStat} = status(Status),
	StatusLine = ["HTTP/1.1 ",integer_to_list(NumStat)," ",TxtStat,"\r\n"],

	Rsp = [StatusLine,
		   [[Key,": ",Value] || {Key,Value} <- Hdrs],
		   ["Content-Length: ",integer_to_list(size(Body1)),"\r\n"],
		   "\r\n",
		   Body1],

	gen_tcp:send(Req#navreq.sock, list_to_binary(Rsp)),
	done.	%% return value handy for custom handlers

status(ok) ->				{200,"OK"};
status(request_failed) ->	{302,"Server error"};
status(bad_request) ->		{400,"Bad Request"};
status(404) ->				{404,"Not found"}.

%%EOF
