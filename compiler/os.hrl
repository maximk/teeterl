%%
%% The following macros determine the packing of codels and
%% are thus does not depend on 32/64bit int size
%%
%% codel size is 4 bytes on 32-bit OS and 8 bytes on 64-bit
%% but when codel is integer or packed integer then only 4 bytes
%% are used; this is needed for 32/64-bit portability of code
%% binaries
%%

-define(TEE_SIZE_UNIT, 1).
-define(TEE_SIZE_DOUBLE, 2).
-define(TEE_SIZE_FULL, 4).

-define(MAX_SIGNED_UNIT, 127).
-define(MIN_SIGNED_UNIT, -128).
-define(MAX_SIGNED_DOUBLE, 32767).
-define(MIN_SIGNED_DOUBLE, -32768).
-define(MAX_UNSIGNED_UNIT, 255).
-define(MAX_UNSIGNED_DOUBLE, 65535).
-define(MAX_INT_VALUE, 134217727).
-define(MIN_INT_VALUE, -134217728).

-define(xxxu(X), X).
-define(xxuu(X, Y), (X bsl 8) bor Y).
-define(xuuu(X, Y, Z), (X bsl 16) bor (Y bsl 8) bor Z).
-define(uuuu(X, Y, Z, Q), (X bsl 24) bor (Y bsl 16) bor (Z bsl 8) bor Q).
-define(xddu(X, Y), (X bsl 8) bor Y).
-define(xudd(X, Y), (X bsl 16) bor Y).
-define(dduu(X, Y, Z), (X bsl 16) bor (Y bsl 8) bor Z).
-define(uddu(X, Y, Z), (X bsl 24) bor (Y bsl 8) bor Z).

%%
%%	Macros depending on whether it is a 32-bit or 64-bit OS
%%

-define(HBOUND, 8).
-define(HALIGN(Size), ((Size + (?HBOUND-1)) band (bnot (?HBOUND-1)))).

-define(WSZ, 4).
-define(HSIZE_BIGNUM(N), ?HALIGN(?WSZ + N*?WSZ)).
-define(HSIZE_CONS, ?HALIGN(2*?WSZ)).
-define(HSIZE_TUPLE(Arity), ?HALIGN(?WSZ + Arity*?WSZ)).
-define(HSIZE_FUN, ?HALIGN(6*?WSZ)).
-define(HSIZE_BINARY(BitSize), ?HALIGN(4*?WSZ) + ?HALIGN((BitSize + 7) bsr 3)).
-define(HSIZE_BINARY_SHARED, ?HALIGN(4*?WSZ)).
-define(HSIZE_FLOAT, ?HALIGN(8)).	%% TODO: there are 160 bit float I heard

%%EOF
