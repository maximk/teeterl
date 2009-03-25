-module(appgen).
-export([main/1]).

-import(lists, [reverse/1,foreach/2]).

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

%//
%//
%//
%
%#include "teeterl.h"
%
%int main(int ac, char *av[])
%{
%	teeterl_init();
%
%	teeterl_add_stdmods();
%	teeterl_add_compmods();
%
%	teeterl_exec(av[1], av[2], av+3);
%}
%
%//EOF

main([_Node|Args]) when length(Args) >= 2 ->
	[Dir|Mods] = reverse(Args),
	MainFile = Dir ++ "/main.c",
	{ok,O} = file:open(MainFile, [write,truncate]),
	io:format(O, "//~n//~n//~n~n", []),
	io:format(O, "#include \"teeterl.h\"~n~n", []),
	foreach(fun(Name) ->
		io:format(O, "extern unsigned int ~s_bin_size;~n", [Name]),
		io:format(O, "extern unsigned char ~s_bin_data[];~n", [Name])
	end, Mods),
	io:nl(O),
	io:format(O, "int main(int ac, char *av[])~n", []),
	io:format(O, "{~n", []),
	%% io:format(O, "\tteeterl_init(\"\\0~.8b~s\");~n~n", [length(Node),Node]),
	io:format(O, "\tteeterl_init();~n~n", []),
	io:format(O, "\tteeterl_add_stdmods();~n~n", []),
	io:format(O, "\tteeterl_add_compmods();~n~n", []),
	foreach(fun(Name) ->
		io:format(O, "\tteeterl_add_mod(\"\\0~.8b~s\", ~s_bin_data, ~s_bin_size);~n", [length(Name),Name,Name,Name])
	end, Mods),
	io:nl(O),
	Start = lists:last(Mods),
	io:format(O, "\tteeterl_exec(\"~s\", \"start\", av+1);~n", [Start]),
	io:format(O, "}~n~n", []),
	io:format(O, "//EOF~n", []),
	file:close(O).

%%EOF
