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
-module(gen_udp).
-export([open/1,open/2]).
-export([send/4]).
-export([recv/2,recv/3]).
-export([close/1]).

-record(udp, {family,ip=any}).

open(Port) -> open(Port, []).
open(Port, Opts) ->
	Udp = udp_opts(Opts),
	LocalZ = inet:addr_to_z(Udp#udp.ip),
	case catch gen_udp:open_socket(LocalZ, Port) of
	{'EXIT',Error} ->
		{error,Error};
	Sock ->
		{ok,Sock}
	end.

send(Socket, Address, Port, Packet) ->
	AddrZ = inet:addr_to_z(Address),
	case catch gen_udp:sendto(Socket, AddrZ, Port, Packet) of
	{'EXIT',Error} ->
		{error,Error};
	_ ->
		ok
	end.

recv(Sock, Len) -> recv(Sock, Len, infinity).
recv(Sock, Len, Timeout) ->
	erlang:set_port_option(Sock, expect, Len),
	receive
	{udp,Sock,Addr,Port,Data} ->
		{ok,{Addr,Port,Data}};
	{udp_closed,Sock} ->
		{error,closed}
	after Timeout ->
		{error,timeout}
	end.

close(Sock) ->
	inet:close(Sock).

udp_opts(Opts) -> udp_opts(Opts, #udp{}).
udp_opts([inet|Opts], Udp) -> udp_opts(Opts, Udp#udp{family=inet});
udp_opts([inet6|Opts], Udp) -> udp_opts(Opts, Udp#udp{family=inet6});
udp_opts([{ip,IP}|Opts], Udp) -> udp_opts(Opts, Udp#udp{ip=IP});
udp_opts(_, Udp) -> Udp.

%% EOF
