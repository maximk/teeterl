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
-module(gen_tcp).
-export([connect/3,connect/4]).
-export([listen/2]).
-export([accept/1,accept/2]).
-export([recv/1,recv/2,recv/3]).
-export([close/1]).
-export([send/2]).

-define(SEND_BUF_SIZE, 4096).

-record(tcp, {family=inet,	%% inet or inet6
	ip=any,					%% local interface
	port=0}).				%% local port

connect(Addr, TcpPort, Opts) ->
	connect(Addr, TcpPort, Opts, infinity).

connect(Addr, TcpPort, Opts, Timeout) when is_integer(TcpPort), is_list(Opts) ->
	Tcp = tcp_opts(Opts),
	case inet:getaddrs(Addr, Tcp#tcp.family) of
	{ok,IPs} ->
		connect1(IPs, TcpPort, Tcp, Timeout, ok);
	{error,_}=Error ->
		Error
	end.

tcp_opts(Opts) -> tcp_opts(Opts, #tcp{}).
tcp_opts([inet|Opts], Tcp) -> tcp_opts(Opts, Tcp#tcp{family=inet});
tcp_opts([inet6|Opts], Tcp) -> tcp_opts(Opts, Tcp#tcp{family=inet6});
tcp_opts([{ip,IP}|Opts], Tcp) -> tcp_opts(Opts, Tcp#tcp{ip=IP});
tcp_opts([{port,Port}|Opts], Tcp) -> tcp_opts(Opts, Tcp#tcp{port=Port});
tcp_opts(_, Tcp) -> Tcp.

connect1([IP|IPs], TcpPort, Tcp, Timeout, _) ->
	%% inet family assumed
	RemoteZ = inet:addr_to_z(IP),
	LocalZ = inet:addr_to_z(Tcp#tcp.ip),
	case catch gen_tcp:connect_socket(RemoteZ, TcpPort, LocalZ, Tcp#tcp.port) of
	{'EXIT',Error} ->
		connect1(IPs, TcpPort, Tcp, Timeout, Error);
	_Outlet ->
		receive
		{tcp_connected,Sock} ->
			{ok,Sock}
		after Timeout ->
			connect1(IPs, TcpPort, Tcp, Timeout, timeout)
		end
	end;
connect1([], _, _, _, LastError) -> {error,LastError}.

listen(Port, Opts) ->
	Tcp = tcp_opts(Opts),
	LocalZ = inet:addr_to_z(Tcp#tcp.ip),
	case catch gen_tcp:listen_socket(LocalZ, Port) of
	{'EXIT',Error} ->
		{error,Error};
	Sock ->
		{ok,Sock}
	end.

accept(Sock) -> accept(Sock, infinity).
accept(Sock, Timeout) ->
	case erlang:property(Sock, accept, self()) of
	false ->
		{error,closed};
	_ ->
		receive
		{tcp_accepted,Sock,Incoming} ->
			{ok,Incoming};
		{tcp_closed,Sock} ->
			{error,closed}
		after Timeout ->
			{error,timeout}
		end
	end.

recv(Sock) -> recv(Sock, 0).
recv(Sock, Len) -> recv(Sock, Len, infinity).
recv(Sock, Len, Timeout) ->
	erlang:property(Sock, expect, Len),
	receive
	{tcp,Sock,Data} ->
		{ok,Data};
	{tcp_closed,Sock} ->
		{error,closed}
	after Timeout ->
		{error,timeout}
	end.

close(Sock) ->
	erlang:close(Sock).

send(Sock, <<Chunk:?SEND_BUF_SIZE/binary,Rest/binary>>) ->
	send0(Sock, Chunk),
	send(Sock, Rest);
send(Sock, Rest) when is_binary(Rest) ->
	send0(Sock, Rest).

send0(Sock, Bin) ->
	erlang:property(Sock, require, size(Bin)),
	receive
	{tcp_space,Sock,_} ->
		Sock ! Bin, ok;
	{tcp_closed,Sock} ->
		{error,closed}
	end.

%% EOF
