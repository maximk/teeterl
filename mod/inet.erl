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
-module(inet).
-export([getaddrs/2,addr_to_z/1]).

getaddrs(Address, Family) ->
	Zaddr = addr_to_z(Address),
	case catch inet:getaddrs0(Zaddr, Family) of
	{'EXIT',Error} ->
		{error,Error};
	IPs ->
		{ok,IPs}
	end.

addr_to_z({A,B,C,D}) when is_integer(A), is_integer(B), is_integer(C), is_integer(D) ->
	%IoList = io_lib:format("~w.~w.~w.~w\0", [A,B,C,D]),
	IoList = [integer_to_list(A),".",
	          integer_to_list(B),".",
	          integer_to_list(C),".",
	          integer_to_list(D),0],
	list_to_binary(IoList);
addr_to_z(Addr) when is_list(Addr) ->
	list_to_binary(Addr ++ [0]);
addr_to_z(Addr) ->
	Addr.

%% EOF
