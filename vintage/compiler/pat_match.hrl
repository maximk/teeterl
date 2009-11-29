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

%% pattern-matching compilation nodes
-record(kwadler, {anno=[],
	body}).

%-record(kwadler2, {anno=[],
%	arg,
%	var,
%	body}).

-record(kreceive, {anno=[],
	var,
	body,
	timeout,
	action}).

-record(kselect, {anno=[],
	var,
	matches=[],
	otherwise}).

-record(kmatch, {anno=[],
	class,
	vars=[],
	then}).

-record(krange, {anno=[],
	min,
	max}).

-record(kliteral, {anno=[],
	val}).
	
-record(kcons, {anno=[]}).

-record(ktuple, {anno=[],
	arity}).

-record(kbinary0, {anno=[]}).

-record(kbinary, {anno=[],
	segments}).

-record(kseg, {anno=[],
	type,				%% atom: integer, float, binary
	size,				%% integer or #c_var
	unit,				%% integer
	signed=false,		%% boolean
	endian=big}).		%% atom: big, little, native

-record(kswitch, {anno=[],
	cases=[],
	otherwise}).

-record(kcase, {anno=[],
	binds=[],
	guard,
	body}).

-record(kbodyref, {anno=[],
	ref}).

-record(kref, {anno=[],
	body}).

-record(kfail, {anno=[],
	desc}).

-record(krecloop, {anno=[]}).

%% EOF
