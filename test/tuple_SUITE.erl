%% ``The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%% 
%%     $Id$
%%
-module(tuple_SUITE).
-export([all/1, t_size/1, t_element/1, t_setelement/1,
	 t_list_to_tuple/1, t_tuple_to_list/1,
	 t_make_tuple/1, t_append_element/1,
	 build_and_match/1, tuple_with_case/1, tuple_in_guard/1]).
-include("test.hrl").

%% Tests tuples and the BIFs:
%%
%% size(Tuple)
%% element/2
%% setelement/3
%% tuple_to_list/1
%% list_to_tuple/1
%% make_tuple/2
%%

all(suite) -> [build_and_match, t_size,
	       t_list_to_tuple, t_tuple_to_list,
	       t_element, t_setelement, t_make_tuple, t_append_element,
	       tuple_with_case, tuple_in_guard].

build_and_match(suite) -> [];
build_and_match(Config) when is_list(Config) ->
    ?line {} = {},
    ?line {1} = {1},
    ?line {1, 2} = {1, 2},
    ?line {1, 2, 3} = {1, 2, 3},
    ?line {1, 2, 3, 4} = {1, 2, 3, 4},
    ?line {1, 2, 3, 4, 5} = {1, 2, 3, 4, 5},
    ?line {1, 2, 3, 4, 5, 6} = {1, 2, 3, 4, 5, 6},
    ?line {1, 2, 3, 4, 5, 6} = {1, 2, 3, 4, 5, 6},
    ?line {1, 2, 3, 4, 5, 6, 7} = {1, 2, 3, 4, 5, 6, 7},
    ?line {1, 2, 3, 4, 5, 6, 7, 8} = {1, 2, 3, 4, 5, 6, 7, 8},
    ok.

%% Tests size(Tuple).

t_size(suite) -> [];
t_size(Config) when is_list(Config) ->
    ?line 0 = size({}),
    ?line 1 = size({a}),
    ?line 1 = size({{a}}),
    ?line 2 = size({{a}, {b}}),
    ?line 3 = size({1, 2, 3}),
    ok.

%% Tests element/2.

t_element(suite) -> [];
t_element(Config) when is_list(Config) ->
    ?line a = element(1, {a}),
    ?line a = element(1, {a, b}),

    ?line List = lists:seq(1, 4096),
    ?line Tuple = list_to_tuple(lists:seq(1, 4096)),
    ?line get_elements(List, Tuple, 1),

	t_element1(0, 3, 1, 1.5, [a,b], 42).

t_element1(I0, I3, I1, I1_5, T1, T2) ->
    ?line {'EXIT', _} = (catch element(I0, {a, b})),
    ?line {'EXIT', _} = (catch element(I3, {a, b})),
    ?line {'EXIT', _} = (catch element(I1, {})),
    ?line {'EXIT', _} = (catch element(I1, T1)),
    ?line {'EXIT', _} = (catch element(I1, T2)),
    ?line {'EXIT', _} = (catch element(I1_5, {a, b})),

    ok.

get_elements([Element|Rest], Tuple, Pos) ->
    ?line Element = element(Pos, Tuple),
    ?line get_elements(Rest, Tuple, Pos+1);
get_elements([], _Tuple, _Pos) ->
    ok.
    
%% Tests set_element/3.

t_setelement(suite) -> [];
t_setelement(Config) when is_list(Config) ->
    ?line {x} = setelement(1, {1}, x),
    ?line {x, 2} = setelement(1, {1, 2}, x),
    ?line {1, x} = setelement(2, {1, 2}, x),

    ?line Tuple = list_to_tuple(lists:duplicate(2048, x)),
    ?line NewTuple = set_all_elements(Tuple, 1),
    ?line NewTuple = list_to_tuple(lists:seq(1+7, 2048+7)),

	t_setelement1(0, 3, 1, 1.5, [a,b]).

t_setelement1(I0, I3, I1, I1_5, T1) ->
    ?line {'EXIT', _} = (catch setelement(I0, {a, b}, x)),
    ?line {'EXIT', _} = (catch setelement(I3, {a, b}, x)),
    ?line {'EXIT', _} = (catch setelement(I1, {}, x)),
    ?line {'EXIT', _} = (catch setelement(I1, T1, x)),
    ?line {'EXIT', _} = (catch setelement(I1_5, {a, b}, x)),

    ok.

set_all_elements(Tuple, Pos) when Pos =< size(Tuple) ->
    set_all_elements(setelement(Pos, Tuple, Pos+7), Pos+1);
set_all_elements(Tuple, Pos) when Pos > size(Tuple) ->
    Tuple.

%% Tests list_to_tuple/1.

t_list_to_tuple(suite) -> [];
t_list_to_tuple(Config) when is_list(Config) ->
    ?line {} = list_to_tuple([]),
    ?line {a} = list_to_tuple([a]),
    ?line {a, b} = list_to_tuple([a, b]),
    ?line {a, b, c} = list_to_tuple([a, b, c]),
    ?line {a, b, c, d} = list_to_tuple([a, b, c, d]),
    ?line {a, b, c, d, e} = list_to_tuple([a, b, c, d, e]),

    ?line Size = 4096,
    ?line Tuple = list_to_tuple(lists:seq(1, Size)),
    ?line Size = size(Tuple),

	t_list_to_tuple1({a,b}, [a|b], [a|b]).
	
t_list_to_tuple1(L1, L2, L3) ->
    ?line {'EXIT', _} = (catch list_to_tuple(L1)),
    ?line {'EXIT', _} = (catch list_to_tuple(L2)),
    ?line {'EXIT', _} = (catch list_to_tuple(L3)),

    ok.

%% Tests tuple_to_list/1.

t_tuple_to_list(suite) -> [];
t_tuple_to_list(Config) when is_list(Config) ->
    ?line [] = tuple_to_list({}),
    ?line [a] = tuple_to_list({a}),
    ?line [a, b] = tuple_to_list({a, b}),
    ?line [a, b, c] = tuple_to_list({a, b, c}),
    ?line [a, b, c, d] = tuple_to_list({a, b, c, d}),
    ?line [a, b, c, d] = tuple_to_list({a, b, c, d}),

    ?line Size = 4096,
    ?line List = lists:seq(1, Size),
    ?line Tuple = list_to_tuple(List),
    ?line Size = size(Tuple),
    ?line List = tuple_to_list(Tuple),

	t_tuple_to_list1(a, 42).

t_tuple_to_list1(T1, T2) ->
    ?line {'EXIT', _} = (catch tuple_to_list(T1)),
    ?line {'EXIT', _} = (catch tuple_to_list(T2)),

    ok.

t_make_tuple(doc) ->
    "Tests the make_tuple/2 BIF.";
t_make_tuple(suite) ->
    [];
t_make_tuple(Config) when is_list(Config) ->
    ?line t_make_tuple1([]),
    ?line t_make_tuple1(42),
    ?line t_make_tuple1(a),
    ?line t_make_tuple1({}),
    ?line t_make_tuple1({a}),
    ?line t_make_tuple1(erlang:make_tuple(400, [])),
    ok.

t_make_tuple1(Element) ->
    lists:foreach(fun(Size) -> t_make_tuple(Size, Element) end,
		  [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 255, 256, 511, 512, 999,
		   1000, 1023, 1024, 4095, 4096]).

t_make_tuple(Size, Element) ->
    Tuple = erlang:make_tuple(Size, Element),
    lists:foreach(fun(Element1) when Element1 =:= Element ->
			  ok;
		     (Other) ->
			  test:fail({got, Other, expected, Element})
		  end, tuple_to_list(Tuple)).

t_append_element(doc) ->
    "Tests the append_element/2 BIF.";
t_append_element(suite) ->
    [];
t_append_element(Config) when is_list(Config) ->
    t_append_element({}, 2048, 2048).

t_append_element(_Tuple, 0, _High) ->
    ok;
t_append_element(Tuple, N, High) ->
    ?line NewTuple = erlang:append_element(Tuple, N),
    ?line verify_seq(tuple_to_list(Tuple), High, N),
    ?line t_append_element(NewTuple, N-1, High).

verify_seq([], High, High) ->
    ok;
verify_seq([High], High, High) ->
    ok;
verify_seq([High|T], High, Lower) ->
    verify_seq(T, High-1, Lower).

%% Tests that a case nested inside a tuple is ok.
%% (This is known to crash earlier versions of BEAM.)

tuple_with_case(suite) -> [];
tuple_with_case(Config) when is_list(Config) ->
    ?line {reply, true} = tuple_with_case(),
    ok.

tuple_with_case() ->
    %% The following comments apply to the BEAM compiler.
    foo(),                              % Reset var count.
    {reply,                             % Compiler will choose {x,1} for tuple.
     case foo() of                      % Call will reset var count.
         {'EXIT', Reason} ->		% Case will return in {x,1} (first free).
             {error, Reason};           % but the tuple will be build in {x,1},
         _ ->                           % so case value is lost and a circular
             true                       % data element is built.
     end}.

foo() -> ignored.

%% Test to build a tuple in a guard.

tuple_in_guard(suite) -> [];
tuple_in_guard(Config) when is_list(Config) ->
    ?line Tuple1 = {a, b},
    ?line Tuple2 = {a, b, c},
    tuple_in_guard1(Tuple1, Tuple2).

tuple_in_guard1(Tuple1, Tuple2) ->
    ?line if
	      Tuple1 == {element(1, Tuple2), element(2, Tuple2)} ->
		  ok;
	      true ->
		  ?line test:fail()
	  end,
    ?line if
	      Tuple2 == {element(1, Tuple2), element(2, Tuple2),
			 element(3, Tuple2)} ->
		  ok;
	      true ->
		  ?line test:fail()
	  end,
    ok.

