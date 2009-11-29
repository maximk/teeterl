%%
%%
%%

-module(erlang).
-export([integer_to_list/1,integer_to_list/2]).
-export([list_to_integer/1,list_to_integer/2]).
-export([split_binary/2]).

integer_to_list(I) ->
    erlang:integer_to_list(I, 10).

integer_to_list(I, Base) 
  when is_integer(I), is_integer(Base), Base >= 2, Base =< 1+$Z-$A+10 ->
    if I < 0 ->
	    [$-|integer_to_list(-I, Base, [])];
       true ->
	    integer_to_list(I, Base, [])
    end;
integer_to_list(I, Base) ->
    erlang:error(badarg, [I, Base]).

integer_to_list(I0, Base, R0) ->
    D = I0 rem Base,
    I1 = I0 div Base,
    R1 = if D >= 10 ->
		 [D-10+$A|R0];
	    true ->
		 [D+$0|R0]
	 end,
    if I1 =:= 0 ->
	    R1;
       true ->
	    integer_to_list(I1, Base, R1)
    end.

list_to_integer(L) ->
	erlang:list_to_integer(L, 10).
	
list_to_integer(L, Base)
  when is_list(L), is_integer(Base), Base >= 2, Base =< 1+$Z-$A+10 ->
    case list_to_integer_sign(L, Base) of 
	I when is_integer(I) ->
	    I;
	Fault ->
	    erlang:error(Fault, [L,Base])
    end;
list_to_integer(L, Base) ->
    erlang:error(badarg, [L,Base]).

list_to_integer_sign([$-|[_|_]=L], Base) ->
    case list_to_integer(L, Base, 0) of
	I when is_integer(I) ->
	    -I;
	I ->
	    I
    end;
list_to_integer_sign([$+|[_|_]=L], Base) ->
    list_to_integer(L, Base, 0);
list_to_integer_sign([_|_]=L, Base) ->
    list_to_integer(L, Base, 0);
list_to_integer_sign(_, _) ->
    badarg.

list_to_integer([D|L], Base, I) 
  when is_integer(D), D >= $0, D =< $9, D < Base+$0 ->
    list_to_integer(L, Base, I*Base + D-$0);
list_to_integer([D|L], Base, I) 
  when is_integer(D), D >= $A, D < Base+$A-10 ->
    list_to_integer(L, Base, I*Base + D-$A+10);
list_to_integer([D|L], Base, I) 
  when is_integer(D), D >= $a, D < Base+$a-10 ->
    list_to_integer(L, Base, I*Base + D-$a+10);
list_to_integer([], _, I) ->
    I;
list_to_integer(_, _, _) ->
    badarg.

split_binary(B, N) when is_binary(B), is_integer(N) ->
	<<B1:N/binary,B2/binary>> = B,
	{B1,B2}.

%%EOF
