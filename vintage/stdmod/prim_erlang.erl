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
-module(prim_erlang).
-export(['!'/2]).
-export([catch0/2,catch1/2]).

-import(erlang,[is_local_node/1]).

'!'(Rcpt, Msg) when is_atom(Rcpt) ->
	erlang:send_msg0(Rcpt, Msg);

'!' ({RegName,Node}, Msg) when is_atom(RegName), is_atom(Node) ->
	case is_local_node(Node) of
	true ->
		erlang:send_msg0(RegName, Msg);
	false ->
		netmesh:deliver(Node, RegName, Msg)
	end;

'!'(Rcpt, Msg) ->
	Node = node(Rcpt),
	case is_local_node(Node) of
	true ->
		erlang:send_msg0(Rcpt, Msg);
	false ->	%% remote send
		netmesh:deliver(Node, Rcpt, Msg)
	end.

catch0(error, Reason) ->
	{'EXIT',{Reason,erlang:get_stacktrace()}};
catch0(exit, Reason) ->
	{'EXIT',Reason};
catch0(_, Reason) ->
	Reason.

catch1(error, Reason) when is_tuple(Reason) ->
	if size(Reason) > 0 -> element(1, Reason);	%% avoid eternal loop if size is in guard
	true -> Reason end;
catch1(_, Reason) ->
	Reason.

%% EOF
