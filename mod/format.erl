-module(format).
-export([from_hex/2,to_hex/1]).
-export([to_json/1]).

-import(lists, [map/2]).

%%	Number -> Number
%%		12	->	12
%%		1.2	->	1.2
%%		12345678901234 -> {n:'12345678901234'}
%%
%%	Atom -> String
%%		abc	->	'abc'
%%
%%	Binaries -> Object
%%		<<1,2,3>>	->	{b:'010203'}
%%		<<1,2,1:3>>	->	{b:{wb:'0102',tr:1,ts:3}}
%%
%%	Fun -> Object
%%		#fun	->	{f:''}
%%
%%  Oid -> Object
%%		#Outlet<local.2.0>	->	{o:2}
%%		#Outlet<remote.2.3>	->	{o:{id:2,nd:'remote',cn:3}}
%%
%%  Pid -> Object
%%		<local.2.0>	->	{p:2}
%%		<remote.2.3>	->	{p:{id:2,nd:'remote',cn:3}}
%%
%%	Tuple -> Object
%%		{1,2,3}	->	{t:[1,2,3]}
%%
%%	List -> Array
%%		[1,2,3]	->	[1,2,3]
%%		[]		->	[]
%%
%%	String	->	Object
%%		"hello"	->	{s:'hello'}
%%
%%	partials ->	Object
%%		<<1,2,3,...>>	{x:{pf:{b:'010203'},ts:1024}}
%%		{1,2,3,...}		{x:{pf:{t:[1,2,3]},ts:1024}}
%%		[1,2,3,...]		{x:{pf:[1,2,3],ts:1024}}
%%		"hello..."		{x:{pf:{s:"hello"},ts:1024}}

to_json(N) when is_integer(N) ->
	if N > 9007199254740992; N < -9007199254740992 ->
		["{n:'",integer_to_list(N),"'}"];
	true ->
		integer_to_list(N)
	end;

to_json(F) when is_float(F) ->
	float_to_list(F);

to_json(A) when is_atom(A) ->
	["'",atom_to_list(A),"'"];

to_json(B) when is_binary(B) ->
	BitSize = bit_size(B),
	
	if BitSize rem 8 =:= 0 ->
	
		%% {b:'010203'}
		["{b:'",[to_hex(X) || X <- binary_to_list(B)],"'}"];
		
	true ->

%TODO: binary matching is flaky
%
%		N = byte_size(B)-1,
%		Ts = BitSize rem 8,
%		<<WholeBytes:N/binary,Trailer:Ts>> = B,
%		
%		%% {b:{wb:'0102',tr:1,ts:3}}
%		Wb = [to_hex(X) || X <- binary_to_list(WholeBytes)],
%		["{b:{wb:'",Wb,"',tr:'",integer_to_list(Trailer),",ts:",integer_to_list(Ts),"}}"]
		
		"{b:'beef'}"
	end;

to_json(F) when is_function(F) ->
	"{f:''}";

%% TODO: is_oid not allowed in guards
%
%to_json(Oid) when is_oid(Oid) ->
%	%% TODO: BIF to dismember oids (and pids)
%	
%	%% {o:2}
%	%% {o:{id:2,nd:'remote',cn:3}}
%
%	"{o:100}";

to_json(Pid) when is_pid(Pid) ->
	%% TODO: BIF to dismember pids (and oids)

	%% {p:2}
	%% {p:{id:2,nd:'remote',cn:3}}

	"{p:100}";

to_json(Tuple) when is_tuple(Tuple) ->
	Es = [to_json(E) || E <- tuple_to_list(Tuple)],
	
	%% {t:[1,2,3]}
	["{t:[",string:join(Es,","),"]}"];

to_json(List) when is_list(List) ->
	case printable_list(List) of
	false ->
		Es = [to_json(E) || E <- List],
	
		%% [1,2,3]
		["[",string:join(Es,","),"]"];
	
	true ->
		
		%% {s:'hello'}
		["{s:'",List,"'}"]
	end;

to_json(_) ->
	"'###'".

%% TODO: partials
%% TODO: printable bins

to_hex(X)	->	%% "d3"
	[hex(X div 16),hex(X rem 16)].

hex(N) when N >= 0, N =< 9 -> N + $0;
hex(N) when N >= 10, N =< 15 -> N + $a.

from_hex(D1, D2) ->
	xeh(D1) * 16 + xeh(D2).

xeh(H) when H >= $0, H =< $9 -> H - $0;
xeh(H) when H >= $a, H =< $f -> H - $a + 10;
xeh(H) when H >= $A, H =< $F -> H - $A + 10.

%% printable_list([Char]) -> bool()
%%  Return true if CharList is a list of printable characters, else
%%  false.

printable_list([C|Cs]) when is_integer(C), C >= $\040, C =< $\176 ->
    printable_list(Cs);
printable_list([C|Cs]) when is_integer(C), C >= $\240, C =< $\377 ->
    printable_list(Cs);
printable_list([$\n|Cs]) -> printable_list(Cs);
printable_list([$\r|Cs]) -> printable_list(Cs);
printable_list([$\t|Cs]) -> printable_list(Cs);
printable_list([$\v|Cs]) -> printable_list(Cs);
printable_list([$\b|Cs]) -> printable_list(Cs);
printable_list([$\f|Cs]) -> printable_list(Cs);
printable_list([$\e|Cs]) -> printable_list(Cs);
printable_list([]) -> true;
printable_list(_) -> false.			%Everything else is false

%EOF
