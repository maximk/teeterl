%%
%% Copyright (c) 2009, Maxim Kharchenko
%% All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions are met:
%%     * Redistributions of source code must retain the above copyright
%%       notice, this list of conditions and the following disclaimer.
%%     * Redistributions in binary form must reproduce the above copyright
%%       notice, this list of conditions and the following disclaimer in the
%%       documentation and/or other materials provided with the distribution.
%%     * Neither the name of the author nor the names of his contributors
%%		 may be used to endorse or promote products derived from this software
%%		 without specific prior written permission.
%%
%% THIS SOFTWARE IS PROVIDED BY Maxim Kharchenko ''AS IS'' AND ANY
%% EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
%% WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
%% DISCLAIMED. IN NO EVENT SHALL Maxim Kharchenko BE LIABLE FOR ANY
%% DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
%% (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
%% LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
%% ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
%% (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
%% SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
%%
-module(file_io_srv).

%% A simple file server for io to one file instance per server instance.

-export([format_error/1]).
-export([start/2, start_link/2,stop/1]).
-export([server_loop/1]).

-import(lists,[reverse/1]).

-record(io, {port,buf= <<>>,read_mode=binary}).

-define(eat_message(M, T), receive M -> M after T -> timeout end).

-define(READ_SIZE_LIST, 128).
-define(READ_SIZE_BINARY, 8192).
-define(WRITE_SIZE, 8192).

%%%-----------------------------------------------------------------
%%% Exported functions

format_error({_Line, ?MODULE, Reason}) ->
    io_lib:format("~w", [Reason]);
format_error({_Line, Mod, Reason}) ->
    Mod:format_error(Reason);
format_error(ErrorId) ->
    erl_posix_msg:message(ErrorId).

start(FileName, Modes) 
  when is_list(FileName) ->
    do_start(spawn, FileName, Modes).

start_link(FileName, Modes) 
  when is_list(FileName) ->
    do_start(spawn_link, FileName, Modes).

stop(Out) ->
	Out ! {file_request,self(),Out,close},
	receive {file_reply, _, Reply} -> Reply end.

%%%-----------------------------------------------------------------
%%% Server starter, dispatcher and helpers

do_start(Spawn, FileName, Modes) ->
	case file:open(FileName, Modes) of	%% raw file is open
	{ok,Port} ->
		Pid = erlang:Spawn(?MODULE,
			server_loop, [#io{port=Port}]),
		{ok,Pid};
	{error,_}=E ->
		E
	end.

server_loop(State) ->
    receive
	{file_request, From, ReplyAs, Request} when is_pid(From) ->
	    case file_request(Request, State) of
		{reply, Reply, NewState} ->
		    file_reply(From, ReplyAs, Reply),
		    server_loop(NewState);
		{error, Reply, NewState} ->
		    %% error is the same as reply, except that
		    %% it breaks the io_request_loop further down
		    file_reply(From, ReplyAs, Reply),
		    server_loop(NewState);
		{stop, Reason, Reply, _NewState} ->
		    file_reply(From, ReplyAs, Reply),
		    exit(Reason)
	    end;
	{io_request, From, ReplyAs, Request} when is_pid(From) ->
	    case io_request(Request, State) of
		{reply, Reply, NewState} ->
		    io_reply(From, ReplyAs, Reply),
		    server_loop(NewState);
		{error, Reply, NewState} ->
		    %% error is the same as reply, except that
		    %% it breaks the io_request_loop further down
		    io_reply(From, ReplyAs, Reply),
		    server_loop(NewState);
		{stop, Reason, Reply, _NewState} ->
		    io_reply(From, ReplyAs, Reply),
		    exit(Reason)
	    end;
	X ->
		erlang:display({file_io_server_receives,X}),
	    server_loop(State)
    end.

file_reply(From, ReplyAs, Reply) ->
    From ! {file_reply, ReplyAs, Reply}.

io_reply(From, ReplyAs, Reply) ->
    From ! {io_reply, ReplyAs, Reply}.

%%%-----------------------------------------------------------------
%%% file requests

file_request(close, 
	     #io{port=Port}=State) ->
	Reply = file:close(Port),
    {stop,normal,Reply,State#io{port=undefined}};
file_request(Unknown, 
	     #io{}=State) ->
    Reason = {request,Unknown},
    {error,{error,Reason},State}.

std_reply({error,_}=Reply, State) ->
    {error,Reply,State#io{buf= <<>>}};
std_reply(Reply, State) ->
    {reply,Reply,State#io{buf= <<>>}}.

%%%-----------------------------------------------------------------
%%% I/O request 

io_request({put_chars,Chars}, State) -> % binary(Chars) new in R9C
    put_chars(Chars, State);
io_request({put_chars,Mod,Func,Args}, 
	   #io{}=State) ->
    case catch apply(Mod, Func, Args) of
	Chars when is_list(Chars); is_binary(Chars) ->
	    io_request({put_chars,Chars}, State);
	_ ->
	    {error,{error,Func},State}
    end;
io_request({get_until,_Prompt,Mod,Func,XtraArgs}, 
	   #io{}=State) ->
    get_chars(io_lib, get_until, {Mod, Func, XtraArgs}, State);
io_request({get_chars,_Prompt,N}, % New in R9C
	   #io{}=State) ->
    get_chars(N, State);
io_request({get_chars,_Prompt,Mod,Func,XtraArg}, % New in R9C
	   #io{}=State) ->
    get_chars(Mod, Func, XtraArg, State);
io_request({get_line,_Prompt}, % New in R9C
	   #io{}=State) ->
    get_chars(io_lib, collect_line, [], State);
io_request({requests,Requests}, 
	   #io{}=State) when is_list(Requests) ->
    io_request_loop(Requests, {reply,ok,State});
io_request(Unknown, 
	   #io{}=State) ->
    Reason = {request,Unknown},
    {error,{error,Reason},State}.

%% Process a list of requests as long as the results are ok.

io_request_loop([], Result) ->
    Result;
io_request_loop([_Request|_Tail], 
		{stop,_Reason,_Reply,_State}=Result) ->
    Result;
io_request_loop([_Request|_Tail],
		{error,_Reply,_State}=Result) ->
    Result;
io_request_loop([Request|Tail], 
		{reply,_Reply,State}) ->
    io_request_loop(Tail, io_request(Request, State)).

%% I/O request put_chars
%%
put_chars(Chars, #io{port=Port}=State) ->
	Reply = file:write(Port, Chars),
	{reply,Reply,State}.

%% Process the I/O request get_chars
%%

get_chars(0, #io{read_mode=ReadMode}=State) ->
 	{reply,cast(<<>>, ReadMode),State};
get_chars(N, #io{buf=Buf,read_mode=ReadMode}=State) 
  when integer(N), N > 0, N =< size(Buf) ->
    {B1,B2} = split_binary(Buf, N),
    {reply,cast(B1, ReadMode),State#io{buf=B2}};
get_chars(N, #io{port=Port,buf=Buf,read_mode=ReadMode}=State) 
  when integer(N), N > 0 ->
    BufSize = size(Buf),
    NeedSize = N-BufSize,
    Size = max(NeedSize, ?READ_SIZE_BINARY),
    case file:read(Port, Size) of
        {ok, B} ->
            if BufSize+size(B) < N ->
                    std_reply(cat(Buf, B, ReadMode), State);
               true ->
                    {B1,B2} = split_binary(B, NeedSize),
                    {reply,cat(Buf, B1, ReadMode),State#io{buf=B2}}
            end;
        eof when BufSize==0 ->
            {reply,eof,State};
        eof ->
            std_reply(cast(Buf, ReadMode), State);
        {error,Reason}=Error ->
            {stop,Reason,Error,State#io{buf= <<>>}}
    end;
get_chars(_N, #io{}=State) ->
    {error,{error,get_chars},State}.

get_chars(Mod, Func, XtraArg, #io{buf= <<>>}=State) ->
    get_chars_empty(Mod, Func, XtraArg, start, State);
get_chars(Mod, Func, XtraArg, #io{buf=Buf}=State) ->
    get_chars_apply(Mod, Func, XtraArg, start, State#io{buf= <<>>}, Buf).

get_chars_empty(Mod, Func, XtraArg, S, 
                #io{port=Port,read_mode=ReadMode}=State) ->
    case file:read(Port, read_size(ReadMode)) of
        {ok,Bin} ->
            get_chars_apply(Mod, Func, XtraArg, S, State, Bin);
        eof ->
            get_chars_apply(Mod, Func, XtraArg, S, State, eof);
        {error,Reason}=Error ->
            {stop,Reason,Error,State}
    end.

get_chars_apply(Mod, Func, XtraArg, S0, 
                #io{read_mode=ReadMode}=State, Data0) ->
    Data1 = case ReadMode of
               list when binary(Data0) -> binary_to_list(Data0);
               _ -> Data0
            end,
    case catch Mod:Func(S0, Data1, XtraArg) of
        {stop,Result,Buf} ->
            {reply,Result,State#io{buf=cast_binary(Buf)}};
        {'EXIT',Reason} ->
            {stop,Reason,{error,err_func(Mod, Func, XtraArg)},State};
        S1 ->
            get_chars_empty(Mod, Func, XtraArg, S1, State)
    end.

%% Convert error code to make it look as before
err_func(io_lib, get_until, {_,F,_}) ->
    F;
err_func(_, F, _) ->
    F.

%% Concatenate two binaries and convert the result to list or binary
cat(B1, B2, binary) ->
    list_to_binary([B1,B2]);
cat(B1, B2, list) ->
    binary_to_list(B1)++binary_to_list(B2).

%% Cast binary to list or binary
cast(B, binary) ->
    B;
cast(B, list) ->
    binary_to_list(B).

%% Convert buffer to binary
cast_binary(Binary) when binary(Binary) ->
    Binary;
cast_binary(List) when list(List) ->
    list_to_binary(List);
cast_binary(_EOF) ->
    <<>>.

%% Read size for different read modes
read_size(binary) ->
    ?READ_SIZE_BINARY;
read_size(list) ->
    ?READ_SIZE_LIST.

max(A, B) when A >= B ->
    A;
max(_, B) ->
    B.

%% EOF
