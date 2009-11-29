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
%%%----------------------------------------------------------------
%%% Purpose:Test Suite for the 'lists' module.
%%%-----------------------------------------------------------------
-module(lists_SUITE).
-include("test.hrl").


% Default timetrap timeout (set in init_per_testcase).
% This should be set relatively high (10-15 times the expected
% max testcasetime).
-define(default_timeout, ?t:seconds(60)).

% Test server specific exports
-export([all/1]).
-export([init_per_testcase/2, fin_per_testcase/2]).

% Test cases must be exported.
-export([member/1, reverse/1,
	 keymember/1, keysearch/1,
	 append/1, append_1/1, append_2/1,
	 seq/1, seq_loop/1, seq_2/1, seq_3/1, seq_2_e/1, seq_3_e/1,
	 sublist/1, flatten/1,
	 sublist_2/1, sublist_3/1, sublist_2_e/1, sublist_3_e/1,
	 flatten_1/1, flatten_2/1, flatten_1_e/1, flatten_2_e/1,
	 sort/1, sort_1/1, sort_stable/1, merge/1, rmerge/1, sort_rand/1,
         usort/1, usort_1/1, usort_stable/1, umerge/1, rumerge/1,usort_rand/1,
	 keymerge/1, rkeymerge/1,
	 keysort/1, keysort_1/1, keysort_i/1, keysort_stable/1,
	 keysort_rand/1, keysort_error/1,
	 ukeymerge/1, rukeymerge/1,
	 ukeysort/1, ukeysort_1/1, ukeysort_i/1, ukeysort_stable/1,
	 ukeysort_rand/1, ukeysort_error/1,
	 funmerge/1, rfunmerge/1,
	 funsort/1, funsort_1/1, funsort_stable/1, funsort_rand/1,
	 funsort_error/1,
	 ufunmerge/1, rufunmerge/1,
	 ufunsort/1, ufunsort_1/1, ufunsort_stable/1, ufunsort_rand/1,
	 ufunsort_error/1]).

%% Sort randomized lists until stopped.
%%
%% If you update some of the sort or merge functions, you should
%% definitely let sort_loop work for a couple of hours or days. Try
%% both sort_loop/0 and sort_loop/1 with a small argument (30-50 say).

-export([sort_loop/0, sort_loop/1, sloop/1]).

%% Internal export.
-export([make_fun/1]).

%%
%% all/1
%%
all(doc) ->
    [];
all(suite) ->
    [append, reverse, member, keymember, keysearch,
     sort, usort, keysort, ukeysort, 
     funsort, ufunsort, sublist, flatten, seq].

init_per_testcase(_Case, Config) ->
    ?line Dog=test:timetrap(?default_timeout),
    [{watchdog, Dog}|Config].

fin_per_testcase(_Case, Config) ->
    Dog=?config(watchdog, Config),
    test:timetrap_cancel(Dog),
    ok.

%
% Test cases starts here.
%
append(doc) ->
    ["Tests lists:append/1 & lists:append/2"];
append(suite) ->
    [append_1, append_2].

append_1(doc) ->   [];
append_1(suite) -> [];
append_1(Config) when is_list(Config) ->
    ?line "abcdef"=lists:append(["abc","def"]),
    ?line [hej, du,[glade, [bagare]]]=
	lists:append([[hej], [du], [[glade, [bagare]]]]),
    ?line [10, [elem]]=lists:append([[10], [[elem]]]),
    ok.

append_2(doc) ->   [];
append_2(suite) -> [];
append_2(Config) when is_list(Config) ->
    ?line "abcdef"=lists:append("abc", "def"),
    ?line [hej, du]=lists:append([hej], [du]),
    ?line [10, [elem]]=lists:append([10], [[elem]]),
    ok.

reverse(suite) ->
    [];
reverse(doc) ->
    ["Tests the lists:reverse() implementation. The function is "
     "`non-blocking', and only processes a fixed number of elements "
     "at a time."];
reverse(Config) when is_list(Config) ->
    ?line reverse_test(0),
    ?line reverse_test(1),
    ?line reverse_test(2),
    ?line reverse_test(128),
    ?line reverse_test(256),
    ?line reverse_test(1000),
    ?line reverse_test(1998),
    ?line reverse_test(1999),
    ?line reverse_test(2000),
    ?line reverse_test(2001),
    ?line reverse_test(3998),
    ?line reverse_test(3999),
    ?line reverse_test(4000),
    ?line reverse_test(4001),
    ?line reverse_test(60001),
    ?line reverse_test(100007),
    ok.

reverse_test(0) ->
    case lists:reverse([]) of
	[] ->
	    ok;
	_Other ->
	    error
    end;
reverse_test(Num) ->
    List0 = ['The Element'|lists:duplicate(Num, 'Ele')],
    List = lists:reverse(List0),
    ['Ele'|_] = List,
    'The Element' = lists:last(List),
    List0 = lists:reverse(List),
    ok.

member(doc) ->
    ["Tests the lists:member() implementation."
     "This test case depends on lists:reverse() to work, "
     "wich is tested in a separate test case."];
member(Config) when is_list(Config) ->
    ?line {'EXIT',_} = (catch lists:member(45, {a,b,c})),
    ?line false = lists:member(4233, []),
    ?line member_test(1),
    ?line member_test(100),
    ?line member_test(256),
    ?line member_test(1000),
    ?line member_test(1998),
    ?line member_test(1999),
    ?line member_test(2000),
    ?line member_test(2001),
    ?line member_test(3998),
    ?line member_test(3999),
    ?line member_test(4000),
    ?line member_test(4001),
    ?line member_test(100008),
    ok.

member_test(Num) ->
    List0 = ['The Element'|lists:duplicate(Num, 'Elem')],
    true = lists:member('The Element', List0),
    true = lists:member('Elem', List0),
    false = lists:member(arne_anka, List0),
    false = lists:member({a,b,c}, List0),
    List = lists:reverse(List0),
    true = lists:member('The Element', List),
    true = lists:member('Elem', List),
    false = lists:member(arne_anka, List),
    false = lists:member({a,b,c}, List).

keymember(Config) when is_list(Config) ->
    ?line false = lists:keymember(anything_goes, 1, []),
    ?line {'EXIT',_} = (catch lists:keymember(anything_goes, -1, [])),
    ?line {'EXIT',_} = (catch lists:keymember(anything_goes, 0, [])),
    ?line {'EXIT',_} = (catch lists:keymember(anything_goes, 1, {1,2,3})),
    List = [{52.0,a},{-19,b,c},{37.5,d},an_atom,42.0,{39},{45,{x,y,z}}],

    ?line false = lists:keymember(333, 5, List),
    ?line false = lists:keymember(333, 999, List),
    ?line false = lists:keymember(37, 1, List),

    ?line true = lists:keymember(52.0, 1, List),
    ?line true = lists:keymember(52, 1, List),
    ?line true = lists:keymember(-19, 1, List),
    ?line true = lists:keymember(-19.0, 1, List),
    ?line true = lists:keymember(37.5, 1, List),
    ?line true = lists:keymember(39, 1, List),
    ?line true = lists:keymember(39.0, 1, List),
    ?line true = lists:keymember(45, 1, List),
    ?line true = lists:keymember(45.0, 1, List),

    ?line true = lists:keymember(a, 2, List),
    ?line true = lists:keymember(b, 2, List),
    ?line true = lists:keymember(c, 3, List),
    ?line true = lists:keymember(d, 2, List),
    ?line true = lists:keymember({x,y,z}, 2, List),

    ?line Long0 = lists:seq(1, 100007),
    ?line false = lists:keymember(kalle, 1, Long0),
    ?line Long = lists:foldl(fun(E, A) -> [{1/E,E}|A] end, [], Long0),
    ?line true = lists:keymember(1, 2, Long),
    ?line true = lists:keymember(2, 2, Long),
    ?line true = lists:keymember(1.0, 2, Long),
    ?line true = lists:keymember(2.0, 2, Long),
    ?line true = lists:keymember(100006, 2, Long),
    ok.

keysearch(Config) when is_list(Config) ->
    ?line false = lists:keysearch(anything_goes, 1, []),
    ?line {'EXIT',_} = (catch lists:keysearch(anything_goes, -1, [])),
    ?line {'EXIT',_} = (catch lists:keysearch(anything_goes, 0, [])),
    ?line {'EXIT',_} = (catch lists:keysearch(anything_goes, 1, {1,2,3})),

    First = {x,42.0},
    Second = {y,-77},
    Third = {z,[a,b,c],{5.0}},
    List = [First,Second,Third],
    
    ?line false = lists:keysearch(333, 1, []),
    ?line false = lists:keysearch(333, 5, List),
    ?line false = lists:keysearch(333, 999, List),
    ?line false = lists:keysearch(37, 1, List),

    ?line {value,First} = lists:keysearch(42, 2, List),
    ?line {value,First} = lists:keysearch(42.0, 2, List),

    ?line {value,Second} = lists:keysearch(-77, 2, List),
    ?line {value,Second} = lists:keysearch(-77.0, 2, List),

    ?line {value,Third} = lists:keysearch(z, 1, List),
    ?line {value,Third} = lists:keysearch([a,b,c], 2, List),
    ?line {value,Third} = lists:keysearch({5}, 3, List),
    ?line {value,Third} = lists:keysearch({5.0}, 3, List),

    ?line Long0 = lists:seq(1, 100007),
    ?line false = lists:keysearch(kalle, 1, Long0),
    ?line Long = lists:foldl(fun(E, A) -> [{1/E,float(E)}|A] end, [], Long0),
    ?line {value,{_,1.0}} = lists:keysearch(1, 2, Long),
    ?line {value,{_,1.0}} = lists:keysearch(1.0, 2, Long),
    ?line {value,{_,2.0}} = lists:keysearch(2, 2, Long),
    ?line {value,{_,2.0}} = lists:keysearch(2.0, 2, Long),
    ?line {value,{_,33988.0}} = lists:keysearch(33988, 2, Long),
    ?line {value,{_,33988.0}} = lists:keysearch(33988.0, 2, Long),
    ok.

sort(doc) ->
    ["Tests merge functions and lists:sort/1"];
sort(suite) ->
    %% [merge, rmerge, sort_1, sort_rand, sort_stable].
    [merge, rmerge, sort_1, sort_rand].

merge(doc) ->   ["merge functions"];
merge(suite) -> [];
merge(Config) when is_list(Config) ->

    %% merge list of lists
    ?line [] = lists:merge([]),
    ?line [] = lists:merge([[]]),
    ?line [] = lists:merge([[],[]]),
    ?line [] = lists:merge([[],[],[]]),
    ?line [1] = lists:merge([[1]]),
    ?line [1,1,2,2] = lists:merge([[1,2],[1,2]]),
    ?line [1] = lists:merge([[1],[],[]]),
    ?line [1] = lists:merge([[],[1],[]]),
    ?line [1] = lists:merge([[],[],[1]]),
    ?line [1,2] = lists:merge([[1],[2],[]]),
    ?line [1,2] = lists:merge([[1],[],[2]]),
    ?line [1,2] = lists:merge([[],[1],[2]]),
    ?line [1,2,3,4,5,6] = lists:merge([[1,2],[],[5,6],[],[3,4],[]]),
    ?line [1,2,3,4] = lists:merge([[4],[3],[2],[1]]),
    ?line [1,2,3,4,5] = lists:merge([[1],[2],[3],[4],[5]]),
    ?line [1,2,3,4,5,6] = lists:merge([[1],[2],[3],[4],[5],[6]]),
    ?line [1,2,3,4,5,6,7,8,9] = 
	lists:merge([[1],[2],[3],[4],[5],[6],[7],[8],[9]]),
    Seq = lists:seq(1,100),
    ?line true = Seq == lists:merge(lists:map(fun(E) -> [E] end, Seq)),

    Two = [1,2],
    Six = [1,2,3,4,5,6],

    %% 2-way merge
    ?line [] = lists:merge([], []),
    ?line Two = lists:merge(Two, []),
    ?line Two = lists:merge([], Two),
    ?line Six = lists:merge([1,3,5], [2,4,6]),
    ?line Six = lists:merge([2,4,6], [1,3,5]),
    ?line Six = lists:merge([1,2,3], [4,5,6]),
    ?line Six = lists:merge([4,5,6], [1,2,3]),
    ?line Six = lists:merge([1,2,5],[3,4,6]),
    ?line [1,2,3,5,7] = lists:merge([1,3,5,7], [2]),
    ?line [1,2,3,4,5,7] = lists:merge([1,3,5,7], [2,4]),
    ?line [1,2,3,4,5,6,7] = lists:merge([1,3,5,7], [2,4,6]),
    ?line [1,2,3,5,7] = lists:merge([2], [1,3,5,7]),
    ?line [1,2,3,4,5,7] = lists:merge([2,4], [1,3,5,7]),
    ?line [1,2,3,4,5,6,7] = lists:merge([2,4,6], [1,3,5,7]),

    %% 3-way merge
    ?line [] = lists:merge3([], [], []),
    ?line Two = lists:merge3([], [], Two),
    ?line Two = lists:merge3([], Two, []),
    ?line Two = lists:merge3(Two, [], []),
    ?line Six = lists:merge3([], [1,3,5], [2,4,6]),
    ?line Six = lists:merge3([1,3,5], [], [2,4,6]),
    ?line Six = lists:merge3([1,3,5], [2,4,6], []),
    ?line Nine = lists:merge3([1,4,7],[2,5,8],[3,6,9]),
    ?line Nine = lists:merge3([1,4,7],[3,6,9],[2,5,8]),
    ?line Nine = lists:merge3([3,6,9],[1,4,7],[2,5,8]),
    ?line Nine = lists:merge3([4,5,6],[1,2,3],[7,8,9]),
    ?line Nine = lists:merge3([1,2,3],[4,5,6],[7,8,9]),
    ?line Nine = lists:merge3([7,8,9],[4,5,6],[1,2,3]),
    ?line Nine = lists:merge3([4,5,6],[7,8,9],[1,2,3]),

    ok.

rmerge(doc) ->   ["reverse merge functions"];
rmerge(suite) -> [];
rmerge(Config) when is_list(Config) ->

    Two = [2,1],
    Six = [6,5,4,3,2,1],

    %% 2-way reversed merge
    ?line [] = lists:rmerge([], []),
    ?line Two = lists:rmerge(Two, []),
    ?line Two = lists:rmerge([], Two),
    ?line Six = lists:rmerge([5,3,1], [6,4,2]),
    ?line Six = lists:rmerge([6,4,2], [5,3,1]),
    ?line Six = lists:rmerge([3,2,1], [6,5,4]),
    ?line Six = lists:rmerge([6,5,4], [3,2,1]),
    ?line Six = lists:rmerge([4,3,2],[6,5,1]),
    ?line [7,6,5,3,1] = lists:rmerge([7,5,3,1], [6]),
    ?line [7,6,5,4,3,1] = lists:rmerge([7,5,3,1], [6,4]),
    ?line [7,6,5,4,3,2,1] = lists:rmerge([7,5,3,1], [6,4,2]),
    ?line [7,5,3,2,1] = lists:rmerge([2], [7,5,3,1]),
    ?line [7,5,4,3,2,1] = lists:rmerge([4,2], [7,5,3,1]),
    ?line [7,6,5,4,3,2,1] = lists:rmerge([6,4,2], [7,5,3,1]),

    Nine = [9,8,7,6,5,4,3,2,1],

    %% 3-way reversed merge
    ?line [] = lists:rmerge3([], [], []),
    ?line Two = lists:rmerge3([], [], Two),
    ?line Two = lists:rmerge3([], Two, []),
    ?line Two = lists:rmerge3(Two, [], []),
    ?line Six = lists:rmerge3([], [5,3,1], [6,4,2]),
    ?line Six = lists:rmerge3([5,3,1], [], [6,4,2]),
    ?line Six = lists:rmerge3([5,3,1], [6,4,2], []),
    ?line Nine = lists:rmerge3([7,4,1],[8,5,2],[9,6,3]),
    ?line Nine = lists:rmerge3([7,4,1],[9,6,3],[8,5,2]),
    ?line Nine = lists:rmerge3([9,6,3],[7,4,1],[8,5,2]),
    ?line Nine = lists:rmerge3([6,5,4],[3,2,1],[9,8,7]),
    ?line Nine = lists:rmerge3([3,2,1],[6,5,4],[9,8,7]),
    ?line Nine = lists:rmerge3([9,8,7],[6,5,4],[3,2,1]),
    ?line Nine = lists:rmerge3([6,5,4],[9,8,7],[3,2,1]),

    ok.

sort_1(doc) ->   ["sort/1"];
sort_1(suite) -> [];
sort_1(Config) when is_list(Config) ->
    ?line [] = lists:sort([]),
    ?line [a] = lists:sort([a]),
    ?line [a,a] = lists:sort([a,a]),
    ?line [a,b] = lists:sort([a,b]),
    ?line [a,b] = lists:sort([b,a]),
    ?line [1,1] = lists:sort([1,1]),
    ?line [1,1,2,3] = lists:sort([1,1,3,2]),
    ?line [1,2,3,3] = lists:sort([3,3,1,2]),
    ?line [1,1,1,1] = lists:sort([1,1,1,1]),
    ?line [1,1,1,2,2,2,3,3,3] = lists:sort([3,3,3,2,2,2,1,1,1]),
    ?line [1,1,1,2,2,2,3,3,3] = lists:sort([1,1,1,2,2,2,3,3,3]),

    ?line lists:foreach(fun check/1, perms([1,2,3])),
    %?line lists:foreach(fun check/1, perms([1,2,3,4,5,6,7,8])),	%% TODO: too much without GC
    ok.

sort_rand(doc) ->   ["sort/1 on big randomized lists"];
sort_rand(suite) -> [];
sort_rand(Config) when is_list(Config) ->
    ?line ok = check(biglist(10)),
    ?line ok = check(biglist(100)),
    ?line ok = check(biglist(1000)),
    ?line ok = check(biglist(10000)),
    ok.

%% sort/1 was really stable for a while - the order of equal elements
%% was kept - but since the performance suffered a bit, this "feature"
%% was removed.
sort_stable(doc) ->   ["sort/1 should be stable for equal terms."];
sort_stable(suite) -> [];
sort_stable(Config) when is_list(Config) ->
    %?line ok = check_stability(bigfunlist(10)),
    %?line ok = check_stability(bigfunlist(100)),	%% TODO: uncomment after GC wired in
    %?line ok = check_stability(bigfunlist(1000)),
    %?line ok = check_stability(bigfunlist(10000)),
    ok.

check([]) ->
    ok;
check(L) ->
    S = lists:sort(L),
    case {length(L) == length(S), check(hd(S), tl(S))} of
	{true,ok} ->
	    ok;
	_ ->
	    erlang:error({check,L})
    end.

check(_A, []) ->
    ok;
check(A, [B | L]) when A =< B ->
    check(B, L);
check(_A, _L) ->
    no.

%% The check that sort/1 is stable is no longer used.
%% Equal elements are no longer always kept in order.
%check_stability(L) ->
%    S = lists:sort(L),
%    LP = explicit_pid(L),
%    SP = explicit_pid(S),
%    check_sorted(1, 2, LP, SP).

explicit_pid(L) ->
    lists:reverse(expl_pid(L, [])).

expl_pid([{I,F} | T], L) when is_function(F) ->
    expl_pid(T, [{I,fun_pid(F)} | L]);
expl_pid([], L) ->
    L.

usort(doc) ->
    ["Tests unique merge functions and lists:usort/1"];
usort(suite) ->
    [umerge, rumerge, usort_1, usort_rand, usort_stable].

usort_1(suite) -> [];
usort_1(doc) -> [""];
usort_1(Conf) when is_list(Conf) ->
    ?line [] = lists:usort([]),
    ?line [1] = lists:usort([1]), 
    ?line [1] = lists:usort([1,1]),
    ?line [1] = lists:usort([1,1,1,1,1]),
    ?line [1,2] = lists:usort([1,2]),
    ?line [1,2] = lists:usort([1,2,1]),
    ?line [1,2] = lists:usort([1,2,2]),
    ?line [1,2,3] = lists:usort([1,3,2]),
    ?line [1,3] = lists:usort([3,1,3]),
    ?line [0,1,3] = lists:usort([3,1,0]),
    ?line [1,2,3] = lists:usort([3,1,2]),
    ?line [1,2] = lists:usort([2,1,1]),
    ?line [1,2] = lists:usort([2,1]),
    ?line [0,3,4,8,9] = lists:usort([3,8,9,0,9,4]),

    ?line lists:foreach(fun ucheck/1, perms([1,2,3])),
    %?line lists:foreach(fun ucheck/1, perms([1,2,3,4,5,6,2,1])),	%% TODO: too much without GC

    ok.

umerge(suite) -> [];
umerge(doc) -> [""];
umerge(Conf) when is_list(Conf) ->
    %% merge list of lists
    ?line [] = lists:umerge([]),
    ?line [] = lists:umerge([[]]),
    ?line [] = lists:umerge([[],[]]),
    ?line [] = lists:umerge([[],[],[]]),
    ?line [1] = lists:umerge([[1]]),
    ?line [1,2] = lists:umerge([[1,2],[1,2]]),
    ?line [1] = lists:umerge([[1],[],[]]),
    ?line [1] = lists:umerge([[],[1],[]]),
    ?line [1] = lists:umerge([[],[],[1]]),
    ?line [1,2] = lists:umerge([[1],[2],[]]),
    ?line [1,2] = lists:umerge([[1],[],[2]]),
    ?line [1,2] = lists:umerge([[],[1],[2]]),
    ?line [1,2,3,4,5,6] = lists:umerge([[1,2],[],[5,6],[],[3,4],[]]),
    ?line [1,2,3,4] = lists:umerge([[4],[3],[2],[1]]),
    ?line [1,2,3,4,5] = lists:umerge([[1],[2],[3],[4],[5]]),
    ?line [1,2,3,4,5,6] = lists:umerge([[1],[2],[3],[4],[5],[6]]),
    ?line [1,2,3,4,5,6,7,8,9] = 
        lists:umerge([[1],[2],[3],[4],[5],[6],[7],[8],[9]]),
    ?line [1,2,4,6,8] = lists:umerge([[1,2],[2,4,6,8]]),
    Seq = lists:seq(1,100),
    ?line true = Seq == lists:umerge(lists:map(fun(E) -> [E] end, Seq)),

    Two = [1,2],
    Six = [1,2,3,4,5,6],

    %% 2-way unique merge
    ?line [] = lists:umerge([], []),
    ?line Two = lists:umerge(Two, []),
    ?line Two = lists:umerge([], Two),
    ?line Six = lists:umerge([1,3,5], [2,4,6]),
    ?line Six = lists:umerge([2,4,6], [1,3,5]),
    ?line Six = lists:umerge([1,2,3], [4,5,6]),
    ?line Six = lists:umerge([4,5,6], [1,2,3]),
    ?line Six = lists:umerge([1,2,5],[3,4,6]),
    ?line [1,2,3,5,7] = lists:umerge([1,3,5,7], [2]),
    ?line [1,2,3,4,5,7] = lists:umerge([1,3,5,7], [2,4]),
    ?line [1,2,3,4,5,6,7] = lists:umerge([1,3,5,7], [2,4,6]),
    ?line [1,2,3,5,7] = lists:umerge([2], [1,3,5,7]),
    ?line [1,2,3,4,5,7] = lists:umerge([2,4], [1,3,5,7]),
    ?line [1,2,3,4,5,6,7] = lists:umerge([2,4,6], [1,3,5,7]),

    ?line [1,2,3,5,7] = lists:umerge([1,2,3,5,7], [2]),
    ?line [1,2,3,4,5,7] = lists:umerge([1,2,3,4,5,7], [2,4]),
    ?line [1,2,3,4,5,6,7] = lists:umerge([1,2,3,4,5,6,7], [2,4,6]),
    ?line [1,2,3,5,7] = lists:umerge([2], [1,2,3,5,7]),
    ?line [1,2,3,4,5,7] = lists:umerge([2,4], [1,2,3,4,5,7]),
    ?line [1,2,3,4,5,6,7] = lists:umerge([2,4,6], [1,2,3,4,5,6,7]),

    %% 3-way unique merge
    ?line [] = lists:umerge3([], [], []),
    ?line Two = lists:umerge3([], [], Two),
    ?line Two = lists:umerge3([], Two, []),
    ?line Two = lists:umerge3(Two, [], []),
    ?line Six = lists:umerge3([], [1,3,5], [2,4,6]),
    ?line Six = lists:umerge3([1,3,5], [], [2,4,6]),
    ?line Six = lists:umerge3([1,3,5], [2,4,6], []),
    ?line Nine = lists:umerge3([1,4,7],[2,5,8],[3,6,9]),
    ?line Nine = lists:umerge3([1,4,7],[3,6,9],[2,5,8]),
    ?line Nine = lists:umerge3([3,6,9],[1,4,7],[2,5,8]),
    ?line Nine = lists:umerge3([4,5,6],[1,2,3],[7,8,9]),
    ?line Nine = lists:umerge3([1,2,3],[4,5,6],[7,8,9]),
    ?line Nine = lists:umerge3([7,8,9],[4,5,6],[1,2,3]),
    ?line Nine = lists:umerge3([4,5,6],[7,8,9],[1,2,3]),

    ?line [1,2,3] = lists:umerge3([1,2,3],[1,2,3],[1,2,3]),
    ?line [1,2,3,4] = lists:umerge3([2,3,4],[1,2,3],[2,3,4]),
    ?line [1,2,3] = lists:umerge3([1,2,3],[2,3],[1,2,3]),
    ?line [1,2,3,4] = lists:umerge3([2,3,4],[3,4],[1,2,3]),

    ok.

rumerge(suite) -> [];
rumerge(doc) -> [""];
rumerge(Conf) when is_list(Conf) ->
    Two = [2,1],
    Six = [6,5,4,3,2,1],

    %% 2-way reversed unique merge
    ?line [] = lists:rumerge([], []),
    ?line Two = lists:rumerge(Two, []),
    ?line Two = lists:rumerge([], Two),
    ?line Six = lists:rumerge([5,3,1], [6,4,2]),
    ?line Six = lists:rumerge([6,4,2], [5,3,1]),
    ?line Six = lists:rumerge([3,2,1], [6,5,4]),
    ?line Six = lists:rumerge([6,5,4], [3,2,1]),
    ?line Six = lists:rumerge([4,3,2],[6,5,1]),
    ?line [7,6,5,3,1] = lists:rumerge([7,5,3,1], [6]),
    ?line [7,6,5,4,3,1] = lists:rumerge([7,5,3,1], [6,4]),
    ?line [7,6,5,4,3,2,1] = lists:rumerge([7,5,3,1], [6,4,2]),
    ?line [7,5,3,2,1] = lists:rumerge([2], [7,5,3,1]),
    ?line [7,5,4,3,2,1] = lists:rumerge([4,2], [7,5,3,1]),
    ?line [7,6,5,4,3,2,1] = lists:rumerge([6,4,2], [7,5,3,1]),

    ?line [7,6,5,3,1] = lists:rumerge([7,6,5,3,1], [6]),
    ?line [7,6,5,4,3,1] = lists:rumerge([7,6,5,4,3,1], [6,4]),
    ?line [7,6,5,4,3,2,1] = lists:rumerge([7,6,5,4,3,2,1], [6,4,2]),
    ?line [7,5,3,2,1] = lists:rumerge([2], [7,5,3,2,1]),
    ?line [7,5,4,3,2,1] = lists:rumerge([4,2], [7,5,4,3,2,1]),
    ?line [7,6,5,4,3,2,1] = lists:rumerge([6,4,2], [7,6,5,4,3,2,1]),

    Nine = [9,8,7,6,5,4,3,2,1],

    %% 3-way reversed unique merge
    ?line [] = lists:rumerge3([], [], []),
    ?line Two = lists:rumerge3([], [], Two),
    ?line Two = lists:rumerge3([], Two, []),
    ?line Two = lists:rumerge3(Two, [], []),
    ?line Six = lists:rumerge3([], [5,3,1], [6,4,2]),
    ?line Six = lists:rumerge3([5,3,1], [], [6,4,2]),
    ?line Six = lists:rumerge3([5,3,1], [6,4,2], []),
    ?line Nine = lists:rumerge3([7,4,1],[8,5,2],[9,6,3]),
    ?line Nine = lists:rumerge3([7,4,1],[9,6,3],[8,5,2]),
    ?line Nine = lists:rumerge3([9,6,3],[7,4,1],[8,5,2]),
    ?line Nine = lists:rumerge3([6,5,4],[3,2,1],[9,8,7]),
    ?line Nine = lists:rumerge3([3,2,1],[6,5,4],[9,8,7]),
    ?line Nine = lists:rumerge3([9,8,7],[6,5,4],[3,2,1]),
    ?line Nine = lists:rumerge3([6,5,4],[9,8,7],[3,2,1]),

    ?line [3,2,1] = lists:rumerge3([3,2,1],[3,2,1],[3,2,1]),
    ?line [4,3,2,1] = lists:rumerge3([4,3,2],[3,2,1],[3,2,1]),
    ?line [5,4,3,2,1] = lists:rumerge3([4,3,2],[5,4,3,2],[5,4,3,2,1]),
    ?line [6,5,4,3,2] = lists:rumerge3([4,3,2],[5,4,3,2],[6,5,4,3]),

    L1 = [c,d,e],
    L2 = [b,c,d],
    ?line true = 
	lists:umerge(L1, L2) == 
	lists:reverse(lists:rumerge(lists:reverse(L1), lists:reverse(L2))),
    ok.

usort_rand(doc) ->   ["usort/1 on big randomized lists"];
usort_rand(suite) -> [];
usort_rand(Config) when is_list(Config) ->
    ?line ok = ucheck(biglist(10)),
    ?line ok = ucheck(biglist(100)),
    ?line ok = ucheck(biglist(1000)),
    ?line ok = ucheck(biglist(10000)),

    ?line ok = ucheck(ubiglist(10)),
    ?line ok = ucheck(ubiglist(100)),
    ?line ok = ucheck(ubiglist(1000)),
    ?line ok = ucheck(ubiglist(10000)),
    ok.

usort_stable(doc) ->   ["usort/1 should keep the first duplicate."];
usort_stable(suite) -> [];
usort_stable(Config) when is_list(Config) ->
    %?line ok = ucheck_stability(bigfunlist(3)),
    %?line ok = ucheck_stability(bigfunlist(10)),
    %?line ok = ucheck_stability(bigfunlist(100)),	%% TODO: when GC is in
    %?line ok = ucheck_stability(bigfunlist(1000)),
    %?line ok = ucheck_stability(bigfunlist(10000)),
    ok.

ucheck([]) ->
    ok;
ucheck(L) ->
    S = lists:usort(L),
    case ucheck(hd(S), tl(S)) of
	ok ->
	    ok;
	_ ->
	    erlang:error({ucheck,L})
    end.

ucheck(_A, []) ->
    ok;
ucheck(A, [B | L]) when A < B ->
    ucheck(B, L);
ucheck(_A, _L) ->
    no.

%% Check that usort/1 is stable and correct relative ukeysort/2.
ucheck_stability(L) ->
    S = no_dups(lsort(L)),
    U = lists:usort(L),
    check_stab(L, U, S, "usort/1", "ukeysort/2").

keysort(doc) ->
    ["Tests lists:keysort/2"];
keysort(suite) ->
    [keymerge, rkeymerge,
     keysort_1, keysort_rand, keysort_i, keysort_stable, keysort_error].

keymerge(doc) -> ["Key merge two lists."];
keymerge(suite) -> [];
keymerge(Config) when is_list(Config) ->

    Two = [{1,a},{2,b}],
    Six = [{1,a},{2,b},{3,c},{4,d},{5,e},{6,f}],

    %% 2-way keymerge
    ?line [] = lists:keymerge(1, [], []),
    ?line Two = lists:keymerge(1, Two, []),
    ?line Two = lists:keymerge(1, [], Two),
    ?line Six = lists:keymerge(1, [{1,a},{3,c},{5,e}], [{2,b},{4,d},{6,f}]),
    ?line Six = lists:keymerge(1, [{2,b},{4,d},{6,f}], [{1,a},{3,c},{5,e}]),
    ?line Six = lists:keymerge(1, [{1,a},{2,b},{3,c}], [{4,d},{5,e},{6,f}]),
    ?line Six = lists:keymerge(1, [{4,d},{5,e},{6,f}], [{1,a},{2,b},{3,c}]),
    ?line Six = lists:keymerge(1, [{1,a},{2,b},{5,e}],[{3,c},{4,d},{6,f}]),
    ?line [{1,a},{2,b},{3,c},{5,e},{7,g}] = 
	lists:keymerge(1, [{1,a},{3,c},{5,e},{7,g}], [{2,b}]),
    ?line [{1,a},{2,b},{3,c},{4,d},{5,e},{7,g}] = 
	lists:keymerge(1, [{1,a},{3,c},{5,e},{7,g}], [{2,b},{4,d}]),
    ?line [{1,a},{2,b},{3,c},{4,d},{5,e},{6,f},{7,g}] = 
	lists:keymerge(1, [{1,a},{3,c},{5,e},{7,g}], [{2,b},{4,d},{6,f}]),
    ?line [{1,a},{2,b},{3,c},{5,e},{7,g}] = 
	lists:keymerge(1, [{2,b}], [{1,a},{3,c},{5,e},{7,g}]),
    ?line [{1,a},{2,b},{3,c},{4,d},{5,e},{7,g}] = 
	lists:keymerge(1, [{2,b},{4,d}], [{1,a},{3,c},{5,e},{7,g}]),
    ?line [{1,a},{2,b},{3,c},{4,d},{5,e},{6,f},{7,g}] = 
	lists:keymerge(1, [{2,b},{4,d},{6,f}], [{1,a},{3,c},{5,e},{7,g}]),

    ?line [{b,2},{c,11},{c,12},{c,21},{c,22},{e,5}] =
	lists:keymerge(1,[{c,11},{c,12},{e,5}], [{b,2},{c,21},{c,22}]),

    ok.

rkeymerge(doc) -> ["Reverse key merge two lists."];
rkeymerge(suite) -> [];
rkeymerge(Config) when is_list(Config) ->

    Two = [{2,b},{1,a}],
    Six = [{6,f},{5,e},{4,d},{3,c},{2,b},{1,a}],

    %% 2-way reversed keymerge
    ?line [] = lists:rkeymerge(1, [], []),
    ?line Two = lists:rkeymerge(1, Two, []),
    ?line Two = lists:rkeymerge(1, [], Two),
    ?line Six = lists:rkeymerge(1, [{5,e},{3,c},{1,a}], [{6,f},{4,d},{2,b}]),
    ?line Six = lists:rkeymerge(1, [{6,f},{4,d},{2,b}], [{5,e},{3,c},{1,a}]),
    ?line Six = lists:rkeymerge(1, [{3,c},{2,b},{1,a}], [{6,f},{5,e},{4,d}]),
    ?line Six = lists:rkeymerge(1, [{6,f},{5,e},{4,d}], [{3,c},{2,b},{1,a}]),
    ?line Six = lists:rkeymerge(1, [{4,d},{3,c},{2,b}],[{6,f},{5,e},{1,a}]),
    ?line [{7,g},{6,f},{5,e},{3,c},{1,a}] = 
	lists:rkeymerge(1, [{7,g},{5,e},{3,c},{1,a}], [{6,f}]),
    ?line [{7,g},{6,f},{5,e},{4,d},{3,c},{1,a}] = 
	lists:rkeymerge(1, [{7,g},{5,e},{3,c},{1,a}], [{6,f},{4,d}]),
    ?line [{7,g},{6,f},{5,e},{4,d},{3,c},{2,b},{1,a}] = 
	lists:rkeymerge(1, [{7,g},{5,e},{3,c},{1,a}], [{6,f},{4,d},{2,b}]),
    ?line [{7,g},{5,e},{3,c},{2,b},{1,a}] = 
	lists:rkeymerge(1, [{2,b}], [{7,g},{5,e},{3,c},{1,a}]),
    ?line [{7,g},{5,e},{4,d},{3,c},{2,b},{1,a}] = 
	lists:rkeymerge(1, [{4,d},{2,b}], [{7,g},{5,e},{3,c},{1,a}]),
    ?line [{7,g},{6,f},{5,e},{4,d},{3,c},{2,b},{1,a}] = 
	lists:rkeymerge(1, [{6,f},{4,d},{2,b}], [{7,g},{5,e},{3,c},{1,a}]),

    L1 = [{c,11},{c,12},{e,5}],
    L2 = [{b,2},{c,21},{c,22}],
    ?line true = 
	lists:keymerge(1, L1, L2) == 
	lists:reverse(lists:rkeymerge(1,lists:reverse(L1), 
				      lists:reverse(L2))),

    ok.

keysort_1(doc) ->   ["keysort"];
keysort_1(suite) -> [];
keysort_1(Config) when is_list(Config) ->
    ?line ok = keysort_check(1, [], []),
    ?line ok = keysort_check(1, [{a,b}], [{a,b}]),
    ?line ok = keysort_check(1, [{a,b},{a,b}], [{a,b},{a,b}]),
    ?line ok = keysort_check(1, [{a,b},{b,c}], [{a,b},{b,c}]),
    ?line ok = keysort_check(1, [{b,c},{a,b}], [{a,b},{b,c}]),
    ?line ok = keysort_check(1,
			     [{1,e},{3,f},{2,y},{0,z},{x,14}],
			     [{0,z},{1,e},{2,y},{3,f},{x,14}]),
    ?line ok = keysort_check(1,
			     [{1,a},{1,a},{1,a},{1,a}],
			     [{1,a},{1,a},{1,a},{1,a}]),

    ?line [{b,1},{c,1}] = lists:keysort(1, [{c,1},{b,1}]),
    ?line [{a,0},{b,2},{c,3},{d,4}] = 
	  lists:keysort(1, [{d,4},{c,3},{b,2},{a,0}]),
    ?line [{a,0},{b,1},{b,2},{c,1}] =
	  lists:keysort(1, [{c,1},{b,1},{b,2},{a,0}]),
    ?line [{a,0},{b,1},{b,2},{c,1},{d,4}] =
	  lists:keysort(1, [{c,1},{b,1},{b,2},{a,0},{d,4}]),

    SFun = fun(L) -> fun(X) -> keysort_check(1, X, L) end end,
    L1 = [{1,a},{2,b},{3,c}],
    ?line lists:foreach(SFun(L1), perms(L1)),
    L2 = [{1,a},{1,a},{2,b}],
    ?line lists:foreach(SFun(L2), perms(L2)),
    L3 = [{1,a},{1,a},{1,a},{2,b}],
    ?line lists:foreach(SFun(L3), perms(L3)),
    %L4 = [{a,1},{a,1},{b,2},{b,2},{c,3},{d,4},{e,5},{f,6}],
    %?line lists:foreach(SFun(L4), perms(L4)),		%% TODO: too much without GC

    ok.

keysort_stable(doc) ->   ["keysort should be stable"];
keysort_stable(suite) -> [];
keysort_stable(Config) when is_list(Config) ->
    ?line ok = keysort_check(1, [{1,b},{1,c}], [{1,b},{1,c}]),
    ?line ok = keysort_check(1, [{1,c},{1,b}], [{1,c},{1,b}]),
    ?line ok = keysort_check(1,
			     [{1,c},{1,b},{2,x},{3,p},{2,a}],
			     [{1,c},{1,b},{2,x},{2,a},{3,p}]),
    ?line ok = keysort_check(1, 
			     [{1,a},{1,b},{1,a},{1,a}],
			     [{1,a},{1,b},{1,a},{1,a}]),
    ok.

keysort_error(doc) ->   ["keysort should exit when given bad arguments"];
keysort_error(suite) -> [];
keysort_error(Config) when is_list(Config) ->
    ?line {'EXIT', _} = (catch lists:keysort(0, [{1,b},{1,c}])),
    ?line {'EXIT', _} = (catch lists:keysort(3, [{1,b},{1,c}])),
    ?line {'EXIT', _} = (catch lists:keysort(1.5, [{1,b},{1,c}])),
    ?line {'EXIT', _} = (catch lists:keysort(x, [{1,b},{1,c}])),
    ?line {'EXIT', _} = (catch lists:keysort(x, [])),
    ?line {'EXIT', _} = (catch lists:keysort(x, [{1,b}])),
    ?line {'EXIT', _} = (catch lists:keysort(1, [a,b])),
    ?line {'EXIT', _} = (catch lists:keysort(1, [{1,b} | {1,c}])),
    ok.

keysort_i(doc) ->   ["keysort with other key than first element"];
keysort_i(suite) -> [];
keysort_i(Config) when is_list(Config) ->
    ?line ok = keysort_check(2, [{a,2},{b,1},{c,3}], [{b,1},{a,2},{c,3}]),
    ok.

keysort_rand(doc) ->   ["keysort on big randomized lists"];
keysort_rand(suite) -> [];
keysort_rand(Config) when is_list(Config) ->
    ?line ok = keysort_check3(1, biglist(10)),
    ?line ok = keysort_check3(1, biglist(100)),
    ?line ok = keysort_check3(1, biglist(1000)),
    ?line ok = keysort_check3(1, biglist(10000)),

    ?line ok = keysort_check3(2, biglist(10)),
    ?line ok = keysort_check3(2, biglist(100)),
    ?line ok = keysort_check3(2, biglist(1000)),
    ?line ok = keysort_check3(2, biglist(10000)),
    ok.

%%% Keysort a list, check that the returned list is what we expected,
%%% and that it is actually sorted.
keysort_check(I, Input, Expected) ->
    Expected = lists:keysort(I, Input),
    check_sorted(I, Input, Expected).

keysort_check3(I, Input) ->
    check_sorted(I, 3, Input, lists:keysort(I, Input)).

check_sorted(I, Input, L) ->
    check_sorted(I, I, Input, L).

%%% Check that a list is keysorted by element I. Elements comparing equal
%%% should be sorted according to element J.
check_sorted(_I, _J, _Input, []) ->
    ok;
check_sorted(I, J, Input, [A | Rest]) ->
    case catch check_sorted1(I, J, A, Rest) of
	{'EXIT', _} ->
	    erlang:error({check_sorted,Input});
	Reply ->
	    Reply
    end.

check_sorted1(_I, _J, _A, []) ->
    ok;
check_sorted1(I, J, A, [B | Rest]) ->
    ok = keycompare(I, J, A, B),
    check_sorted1(I, J, B, Rest).

keycompare(I, _J, A, B) when element(I, A) < element(I, B) ->
    ok;
keycompare(I, J, A, B) when element(I, A) == element(I, B), 
			    element(J, A) =< element(J, B) ->
    ok.

ukeysort(doc) ->
    ["Tests lists:ukeysort/2"];
ukeysort(suite) ->
    [ukeymerge, rukeymerge,
     ukeysort_1, ukeysort_rand, ukeysort_i, ukeysort_stable, ukeysort_error].

ukeymerge(suite) -> [];
ukeymerge(doc) -> ["Merge two lists while removing duplicates."];
ukeymerge(Conf) when is_list(Conf) ->

    Two = [{1,a},{2,b}],
    Six = [{1,a},{2,b},{3,c},{4,d},{5,e},{6,f}],

    %% 2-way unique keymerge
    ?line [] = lists:ukeymerge(1, [], []),
    ?line Two = lists:ukeymerge(1, Two, []),
    ?line Two = lists:ukeymerge(1, [], Two),
    ?line [] = lists:ukeymerge(1, [], []),
    ?line Two = lists:ukeymerge(1, Two, []),
    ?line Two = lists:ukeymerge(1, [], Two),
    ?line Six = lists:ukeymerge(1, [{1,a},{3,c},{5,e}], [{2,b},{4,d},{6,f}]),
    ?line Six = lists:ukeymerge(1, [{2,b},{4,d},{6,f}], [{1,a},{3,c},{5,e}]),
    ?line Six = lists:ukeymerge(1, [{1,a},{2,b},{3,c}], [{4,d},{5,e},{6,f}]),
    ?line Six = lists:ukeymerge(1, [{4,d},{5,e},{6,f}], [{1,a},{2,b},{3,c}]),
    ?line Six = lists:ukeymerge(1, [{1,a},{2,b},{5,e}],[{3,c},{4,d},{6,f}]),
    ?line [{1,a},{2,b},{3,c},{5,e},{7,g}] = 
	lists:ukeymerge(1, [{1,a},{3,c},{5,e},{7,g}], [{2,b}]),
    ?line [{1,a},{2,b},{3,c},{4,d},{5,e},{7,g}] = 
	lists:ukeymerge(1, [{1,a},{3,c},{5,e},{7,g}], [{2,b},{4,d}]),
    ?line [{1,a},{2,b},{3,c},{4,d},{5,e},{6,f},{7,g}] = 
	lists:ukeymerge(1, [{1,a},{3,c},{5,e},{7,g}], [{2,b},{4,d},{6,f}]),
    ?line [{1,a},{2,b},{3,c},{5,e},{7,g}] = 
	lists:ukeymerge(1, [{2,b}], [{1,a},{3,c},{5,e},{7,g}]),
    ?line [{1,a},{2,b},{3,c},{4,d},{5,e},{7,g}] = 
	lists:ukeymerge(1, [{2,b},{4,d}], [{1,a},{3,c},{5,e},{7,g}]),
    ?line [{1,a},{2,b},{3,c},{4,d},{5,e},{6,f},{7,g}] = 
	lists:ukeymerge(1, [{2,b},{4,d},{6,f}], [{1,a},{3,c},{5,e},{7,g}]),

    ?line [{1,a},{2,b},{3,c},{5,e},{7,g}] = 
	lists:ukeymerge(1, [{1,a},{2,b},{3,c},{5,e},{7,g}], [{2,b}]),
    ?line [{1,a},{2,b},{3,c},{4,d},{5,e},{7,g}] = 
	lists:ukeymerge(1, [{1,a},{2,b},{3,c},{4,d},{5,e},{7,g}], 
			[{2,b},{4,d}]),
    ?line [{1,a},{2,b},{3,c},{4,d},{5,e},{6,f},{7,g}] = 
	lists:ukeymerge(1, [{1,a},{3,c},{5,e},{6,f},{7,g}], 
			[{2,b},{4,d},{6,f}]),
    ?line [{1,a},{2,b},{3,c},{5,e},{7,g}] = 
	lists:ukeymerge(1, [{2,b}], [{1,a},{2,b},{3,c},{5,e},{7,g}]),
    ?line [{1,a},{2,b},{3,c},{4,d},{5,e},{7,g}] = 
	lists:ukeymerge(1, [{2,b},{4,d}], 
			[{1,a},{2,b},{3,c},{4,d},{5,e},{7,g}]),
    ?line [{1,a},{2,b},{3,c},{4,d},{5,e},{6,f},{7,g}] = 
	lists:ukeymerge(1, [{2,b},{4,d},{6,f}], 
		     [{1,a},{2,b},{3,c},{4,d},{5,e},{6,f},{7,g}]),

    L1 = [{1,a},{1,b},{1,a}],
    L2 = [{1,a},{1,b},{1,a}],
    ?line [{1,a},{1,b},{1,a},{1,b},{1,a}] = lists:ukeymerge(1, L1, L2),
    ?line [{b,2},{c,11},{c,12},{c,21},{c,22},{e,5}] =
	lists:ukeymerge(1, [{c,11},{c,12},{e,5}], [{b,2},{c,21},{c,22}]),

    ok.

rukeymerge(suite) -> [];
rukeymerge(doc) -> 
    ["Reverse merge two lists while removing duplicates."];
rukeymerge(Conf) when is_list(Conf) ->

    Two = [{2,b},{1,a}],
    Six = [{6,f},{5,e},{4,d},{3,c},{2,b},{1,a}],

    %% 2-way reversed unique keymerge
    ?line [] = lists:rukeymerge(1, [], []),
    ?line Two = lists:rukeymerge(1, Two, []),
    ?line Two = lists:rukeymerge(1, [], Two),
    ?line Six = lists:rukeymerge(1, [{5,e},{3,c},{1,a}], [{6,f},{4,d},{2,b}]),
    ?line Six = lists:rukeymerge(1, [{6,f},{4,d},{2,b}], [{5,e},{3,c},{1,a}]),
    ?line Six = lists:rukeymerge(1, [{3,c},{2,b},{1,a}], [{6,f},{5,e},{4,d}]),
    ?line Six = lists:rukeymerge(1, [{6,f},{5,e},{4,d}], [{3,c},{2,b},{1,a}]),
    ?line Six = lists:rukeymerge(1, [{4,d},{3,c},{2,b}],[{6,f},{5,e},{1,a}]),
    ?line [{7,g},{6,f},{5,e},{3,c},{1,a}] = 
	lists:rukeymerge(1, [{7,g},{5,e},{3,c},{1,a}], [{6,f}]),
    ?line [{7,g},{6,f},{5,e},{4,d},{3,c},{1,a}] = 
	lists:rukeymerge(1, [{7,g},{5,e},{3,c},{1,a}], [{6,f},{4,d}]),
    ?line [{7,g},{6,f},{5,e},{4,d},{3,c},{2,b},{1,a}] = 
	lists:rukeymerge(1, [{7,g},{5,e},{3,c},{1,a}], [{6,f},{4,d},{2,b}]),
    ?line [{7,g},{5,e},{3,c},{2,b},{1,a}] = 
	lists:rukeymerge(1, [{2,b}], [{7,g},{5,e},{3,c},{1,a}]),
    ?line [{7,g},{5,e},{4,d},{3,c},{2,b},{1,a}] = 
	lists:rukeymerge(1, [{4,d},{2,b}], [{7,g},{5,e},{3,c},{1,a}]),
    ?line [{7,g},{6,f},{5,e},{4,d},{3,c},{2,b},{1,a}] = 
	lists:rukeymerge(1, [{6,f},{4,d},{2,b}], [{7,g},{5,e},{3,c},{1,a}]),

    ?line [{7,g},{6,f},{5,e},{3,c},{1,a}] = 
	lists:rukeymerge(1, [{7,g},{6,f},{5,e},{3,c},{1,a}], [{6,f}]),
    ?line [{7,g},{6,f},{5,e},{4,d},{3,c},{1,a}] = 
	lists:rukeymerge(1, [{7,g},{6,f},{5,e},{4,d},{3,c},{1,a}], 
			 [{6,f},{4,d}]),
    ?line [{7,g},{6,f},{5,e},{4,d},{3,c},{2,b},{1,a}] = 
	lists:rukeymerge(1, [{7,g},{6,f},{5,e},{4,d},{3,c},{2,b},{1,a}], 
		      [{6,f},{4,d},{2,b}]),
    ?line [{7,g},{5,e},{3,c},{2,b},{1,a}] = 
	lists:rukeymerge(1, [{2,b}], [{7,g},{5,e},{3,c},{2,b},{1,a}]),
    ?line [{7,g},{5,e},{4,d},{3,c},{2,b},{1,a}] = 
	lists:rukeymerge(1, [{4,d},{2,b}], 
			 [{7,g},{5,e},{4,d},{3,c},{2,b},{1,a}]),
    ?line [{7,g},{6,f},{5,e},{4,d},{3,c},{2,b},{1,a}] = 
	lists:rukeymerge(1, [{6,f},{4,d},{2,b}], 
		      [{7,g},{6,f},{5,e},{4,d},{3,c},{2,b},{1,a}]),

    L1 = [{1,a},{1,b},{1,a}],
    L2 = [{1,a},{1,b},{1,a}],
    ?line true = 
	lists:ukeymerge(1, L1, L2) == 
	lists:reverse(lists:rukeymerge(1, lists:reverse(L1), 
				       lists:reverse(L2))),

    L3 = [{c,11},{c,12},{e,5}],
    L4 = [{b,2},{c,21},{c,22}],
    ?line true = 
	lists:ukeymerge(1, L3, L4) == 
	lists:reverse(lists:rukeymerge(1,lists:reverse(L3), 
				       lists:reverse(L4))),

    ok.

ukeysort_1(doc) ->   ["ukeysort"];
ukeysort_1(suite) -> [];
ukeysort_1(Config) when is_list(Config) ->
    ?line ok = ukeysort_check(1, [], []),
    ?line ok = ukeysort_check(1, [{a,b}], [{a,b}]),
    ?line ok = ukeysort_check(1, [{a,b},{a,b}], [{a,b}]),
    ?line ok = ukeysort_check(1, [{a,b},{b,c}], [{a,b},{b,c}]),
    ?line ok = ukeysort_check(1, [{b,c},{a,b}], [{a,b},{b,c}]),
    ?line ok = ukeysort_check(1,
                              [{1,e},{3,f},{2,y},{0,z},{x,14}],
                              [{0,z},{1,e},{2,y},{3,f},{x,14}]),
    ?line ok = ukeysort_check(1, [{1,a},{1,a},{1,a},{1,a}], [{1,a}]),

    L1 = [{1,a},{1,b},{1,a}],
    L2 = [{1,a},{1,b},{1,a}],
    ?line ok = ukeysort_check(1, lists:keymerge(1, L1, L2), 
                              lists:ukeymerge(1, L1, L2)),
    L3 = [{1,a},{1,b},{1,a},{2,a}],
    ?line ok = ukeysort_check(1, lists:keymerge(1, L3, L2), 
                              lists:ukeymerge(1, L3, L2)),
    L4 = [{1,b},{1,a}],
    ?line ok = ukeysort_check(1, lists:keymerge(1, L1, L4), 
                              lists:ukeymerge(1, L1, L4)),
    L5 = [{1,a},{1,b},{1,a},{2,a}],
    ?line ok = ukeysort_check(1, lists:keymerge(1, [], L5), 
                              lists:ukeymerge(1, [], L5)),
    ?line ok = ukeysort_check(1, lists:keymerge(1, L5, []), 
                              lists:ukeymerge(1, L5, [])),
    L6 = [{3,a}],
    ?line ok = ukeysort_check(1, lists:keymerge(1, L5, L6), 
                              lists:ukeymerge(1, L5, L6)),

    ?line [{b,1},{c,1}] = lists:ukeysort(1, [{c,1},{c,1},{c,1},{c,1},{b,1}]),
    ?line [{a,0},{b,2},{c,3},{d,4}] = 
	  lists:ukeysort(1, [{d,4},{c,3},{b,2},{b,2},{a,0}]),
    ?line [{a,0},{b,1},{b,2},{c,1}] =
	  lists:ukeysort(1, [{c,1},{b,1},{b,1},{b,2},{b,2},{a,0}]),
    ?line [{a,0},{b,1},{b,2},{c,1},{d,4}] =
	  lists:ukeysort(1, [{c,1},{b,1},{b,2},{a,0},{a,0},{d,4},{d,4}]),

    SFun = fun(L) -> fun(X) -> ukeysort_check(2, X, L) end end,
    %PL = [{a,1},{b,2},{c,3},{d,4},{e,5},{f,6}],
    %Ps = perms([{a,1},{b,2},{c,3},{d,4},{e,5},{f,6},{b,2},{a,1}]),
    %?line lists:foreach(SFun(PL), Ps),		%% TODO: too much without GC

    M1L = [{1,a},{1,a},{2,b}],
    M1s = [{1,a},{2,b}],
    ?line lists:foreach(SFun(M1s), perms(M1L)),
    M3 = [{1,a},{2,b},{3,c}],
    ?line lists:foreach(SFun(M3), perms(M3)),

    ok.

ukeysort_stable(doc) ->   ["ukeysort should be stable"];
ukeysort_stable(suite) -> [];
ukeysort_stable(Config) when is_list(Config) ->
    ?line ok = ukeysort_check(1, [{1,b},{1,c}], [{1,b},{1,c}]),
    ?line ok = ukeysort_check(1, [{1,c},{1,b}], [{1,c},{1,b}]),
    ?line ok = ukeysort_check(1,
                              [{1,c},{1,b},{2,x},{3,p},{2,a}],
                              [{1,c},{1,b},{2,x},{2,a},{3,p}]),

    ?line ok = ukeysort_check(1, [{1,a},{1,b},{1,b}], [{1,a},{1,b}]),
    ?line ok = ukeysort_check(1, [{2,a},{1,b},{2,a}], [{1,b},{2,a}]),

    %?line ok = ukeysort_check_stability(bigfunlist(3)),
    %?line ok = ukeysort_check_stability(bigfunlist(10)),
    %?line ok = ukeysort_check_stability(bigfunlist(100)),	%% TODO: after GC
    %?line ok = ukeysort_check_stability(bigfunlist(1000)),
    %?line ok = ukeysort_check_stability(bigfunlist(10000)),
    ok.

ukeysort_error(doc) ->   ["ukeysort should exit when given bad arguments"];
ukeysort_error(suite) -> [];
ukeysort_error(Config) when is_list(Config) ->
    ?line {'EXIT', _} = (catch lists:ukeysort(0, [{1,b},{1,c}])),
    ?line {'EXIT', _} = (catch lists:ukeysort(3, [{1,b},{1,c}])),
    ?line {'EXIT', _} = (catch lists:ukeysort(1.5, [{1,b},{1,c}])),
    ?line {'EXIT', _} = (catch lists:ukeysort(x, [{1,b},{1,c}])),
    ?line {'EXIT', _} = (catch lists:ukeysort(x, [])),
    ?line {'EXIT', _} = (catch lists:ukeysort(x, [{1,b}])),
    ?line {'EXIT', _} = (catch lists:ukeysort(1, [a,b])),
    ?line {'EXIT', _} = (catch lists:ukeysort(1, [{1,b} | {1,c}])),
    ok.

ukeysort_i(doc) ->   ["ukeysort with other key than first element"];
ukeysort_i(suite) -> [];
ukeysort_i(Config) when is_list(Config) ->
    ?line ok = ukeysort_check(2, [{a,2},{b,1},{c,3}], [{b,1},{a,2},{c,3}]),
    ok.

ukeysort_rand(doc) ->   ["ukeysort on big randomized lists"];
ukeysort_rand(suite) -> [];
ukeysort_rand(Config) when is_list(Config) ->
    ?line ok = ukeysort_check3(2, biglist(10)),
    ?line ok = ukeysort_check3(2, biglist(100)),
    ?line ok = ukeysort_check3(2, biglist(1000)),
    ?line ok = ukeysort_check3(2, biglist(10000)),

    ?line ok = gen_ukeysort_check(1, ubiglist(10)),
    ?line ok = gen_ukeysort_check(1, ubiglist(100)),
    ?line ok = gen_ukeysort_check(1, ubiglist(1000)),
    ?line ok = gen_ukeysort_check(1, ubiglist(10000)),
    ok.

%% Check that ukeysort/2 is stable and correct relative keysort/2.
%% (this is not affected by the fact that keysort/2 is no longer really 
%%  stable; ucheck_stability/1 checks ukeysort/2 (and usort/1, of course))
gen_ukeysort_check(I, Input) ->
    U = lists:ukeysort(I, Input),
    S = lists:keysort(I, Input),
    case U == no_dups(S) of
	true ->
	    ok;
	false ->
	    erlang:error({gen_ukeysort_check,Input})
    end.

%% Used for checking that the first copy is kept.
ukeysort_check_stability(L) ->
    I = 1,
    U = lists:ukeysort(I, L),
    S = no_dups(lkeysort(I, L)),
    check_stab(L, U, S, "ukeysort/2", "usort/2").

%%% Uniquely keysort a list, check that the returned list is what we
%%% expected, and that it is actually sorted.
ukeysort_check(I, Input, Expected) ->
    Expected = lists:ukeysort(I, Input),
    ucheck_sorted(I, Input, Expected).

ukeysort_check3(I, Input) ->
    ucheck_sorted(I, 3, Input, lists:ukeysort(I, Input)).

ucheck_sorted(I, Input, L) ->
    ucheck_sorted(I, I, Input, L).

%%% Check that a list is ukeysorted by element I. Elements comparing
%%% equal should be sorted according to element J.
ucheck_sorted(_I, _J, _Input, []) ->
    ok;
ucheck_sorted(I, J, Input, [A | Rest]) ->
    case catch ucheck_sorted1(I, J, A, Rest) of
	{'EXIT', _} ->
	    erlang:error({ucheck_sorted,Input});
	Reply ->
	    Reply
    end.

ucheck_sorted1(_I, _J, _A, []) ->
    ok;
ucheck_sorted1(I, J, A, [B | Rest]) ->
    ok = ukeycompare(I, J, A, B),
    ucheck_sorted1(I, J, B, Rest).

ukeycompare(I, _J, A, B) when element(I, A) < element(I, B) ->
    ok;
ukeycompare(I, J, A, B) when A =/= B,
			     element(I, A) == element(I, B), 
			     element(J, A) =< element(J, B) ->
    ok.


funsort(doc) ->
    ["Tests lists:sort/2"];
funsort(suite) ->
    [funmerge, rfunmerge, 
     funsort_1, funsort_stable, funsort_error, funsort_rand].

funmerge(doc) -> ["Merge two lists using a fun."];
funmerge(suite) -> [];
funmerge(Config) when is_list(Config) ->

    Two = [1,2],
    Six = [1,2,3,4,5,6],
    F = fun(X, Y) -> X =< Y end,

    %% 2-way merge
    ?line [] = lists:merge(F, [], []),
    ?line Two = lists:merge(F, Two, []),
    ?line Two = lists:merge(F, [], Two),
    ?line Six = lists:merge(F, [1,3,5], [2,4,6]),
    ?line Six = lists:merge(F, [2,4,6], [1,3,5]),
    ?line Six = lists:merge(F, [1,2,3], [4,5,6]),
    ?line Six = lists:merge(F, [4,5,6], [1,2,3]),
    ?line Six = lists:merge(F, [1,2,5],[3,4,6]),
    ?line [1,2,3,5,7] = lists:merge(F, [1,3,5,7], [2]),
    ?line [1,2,3,4,5,7] = lists:merge(F, [1,3,5,7], [2,4]),
    ?line [1,2,3,4,5,6,7] = lists:merge(F, [1,3,5,7], [2,4,6]),
    ?line [1,2,3,5,7] = lists:merge(F, [2], [1,3,5,7]),
    ?line [1,2,3,4,5,7] = lists:merge(F, [2,4], [1,3,5,7]),
    ?line [1,2,3,4,5,6,7] = lists:merge(F, [2,4,6], [1,3,5,7]),

    F2 = fun(X,Y) -> element(1,X) =< element(1,Y) end,
    ?line [{b,2},{c,11},{c,12},{c,21},{c,22},{e,5}] =
	lists:merge(F2,[{c,11},{c,12},{e,5}], [{b,2},{c,21},{c,22}]),

    ok.

rfunmerge(doc) -> ["Reverse merge two lists using a fun."];
rfunmerge(suite) -> [];
rfunmerge(Config) when is_list(Config) ->

    Two = [2,1],
    Six = [6,5,4,3,2,1],
    F = fun(X, Y) -> X =< Y end,

    %% 2-way reversed merge
    ?line [] = lists:rmerge(F, [], []),
    ?line Two = lists:rmerge(F, Two, []),
    ?line Two = lists:rmerge(F, [], Two),
    ?line Six = lists:rmerge(F, [5,3,1], [6,4,2]),
    ?line Six = lists:rmerge(F, [6,4,2], [5,3,1]),
    ?line Six = lists:rmerge(F, [3,2,1], [6,5,4]),
    ?line Six = lists:rmerge(F, [6,5,4], [3,2,1]),
    ?line Six = lists:rmerge(F, [4,3,2],[6,5,1]),
    ?line [7,6,5,3,1] = lists:rmerge(F, [7,5,3,1], [6]),
    ?line [7,6,5,4,3,1] = lists:rmerge(F, [7,5,3,1], [6,4]),
    ?line [7,6,5,4,3,2,1] = lists:rmerge(F, [7,5,3,1], [6,4,2]),
    ?line [7,5,3,2,1] = lists:rmerge(F, [2], [7,5,3,1]),
    ?line [7,5,4,3,2,1] = lists:rmerge(F, [4,2], [7,5,3,1]),
    ?line [7,6,5,4,3,2,1] = lists:rmerge(F, [6,4,2], [7,5,3,1]),

    F2 = fun(X,Y) -> element(1,X) =< element(1,Y) end,
    L1 = [{c,11},{c,12},{e,5}],
    L2 = [{b,2},{c,21},{c,22}],
    ?line true = 
	lists:merge(F2, L1, L2) == 
	lists:reverse(lists:rmerge(F2,lists:reverse(L1), lists:reverse(L2))),

    ok.


funsort_1(doc) ->   ["sort/2"];
funsort_1(suite) -> [];
funsort_1(Config) when is_list(Config) ->
    ?line ok = funsort_check(1, [], []),
    ?line ok = funsort_check(1, [{a,b}], [{a,b}]),
    ?line ok = funsort_check(1, [{a,b},{a,b}], [{a,b},{a,b}]),
    ?line ok = funsort_check(1, [{a,b},{b,c}], [{a,b},{b,c}]),
    ?line ok = funsort_check(1, [{b,c},{a,b}], [{a,b},{b,c}]),
    ?line ok = funsort_check(1,
			     [{1,e},{3,f},{2,y},{0,z},{x,14}],
			     [{0,z},{1,e},{2,y},{3,f},{x,14}]),
    F = funsort_fun(1),

    ?line [{b,1},{c,1}] = lists:sort(F, [{c,1},{b,1}]),
    ?line [{a,0},{b,2},{c,3},{d,4}] = 
	  lists:sort(F, [{d,4},{c,3},{b,2},{a,0}]),
    ?line [{a,0},{b,1},{b,2},{c,1}] =
	  lists:sort(F, [{c,1},{b,1},{b,2},{a,0}]),
    ?line [{a,0},{b,1},{b,2},{c,1},{d,4}] =
	  lists:sort(F, [{c,1},{b,1},{b,2},{a,0},{d,4}]),

    %SFun = fun(L) -> fun(X) -> funsort_check(1, X, L) end end,
    %L1 = [{1,a},{1,a},{2,b},{2,b},{3,c},{4,d},{5,e},{6,f}],
    %?line lists:foreach(SFun(L1), perms(L1)),		%% TODO: too much without GC

    ok.

funsort_stable(doc) ->   ["sort/2 should be stable"];
funsort_stable(suite) -> [];
funsort_stable(Config) when is_list(Config) ->
    ?line ok = funsort_check(1, [{1,b},{1,c}], [{1,b},{1,c}]),
    ?line ok = funsort_check(1, [{1,c},{1,b}], [{1,c},{1,b}]),
    ?line ok = funsort_check(1,
			     [{1,c},{1,b},{2,x},{3,p},{2,a}],
			     [{1,c},{1,b},{2,x},{2,a},{3,p}]),
    ok.

funsort_error(doc) ->   ["sort/2 should exit when given bad arguments"];
funsort_error(suite) -> [];
funsort_error(Config) when is_list(Config) ->
    ?line {'EXIT', _} = (catch lists:sort(1, [{1,b} , {1,c}])),
    ?line {'EXIT', _} = (catch lists:sort(fun(X,Y) -> X =< Y end, 
					  [{1,b} | {1,c}])),
    ok.

funsort_rand(doc) ->   ["sort/2 on big randomized lists"];
funsort_rand(suite) -> [];
funsort_rand(Config) when is_list(Config) ->
    ?line ok = funsort_check3(1, biglist(10)),
    ?line ok = funsort_check3(1, biglist(100)),
    ?line ok = funsort_check3(1, biglist(1000)),
    ?line ok = funsort_check3(1, biglist(10000)),
    ok.

% Do a keysort
funsort(I, L) ->
    lists:sort(funsort_fun(I), L).

funsort_check3(I, Input) ->
    check_sorted(I, 3, Input, funsort(I, Input)).

%%% Keysort a list, check that the returned list is what we expected,
%%% and that it is actually sorted.
funsort_check(I, Input, Expected) ->
    ?line Expected = funsort(I, Input),
    check_sorted(I, Input, Expected).

ufunsort(doc) ->
    ["Tests lists:usort/2"];
ufunsort(suite) ->
    [ufunmerge, rufunmerge, 
     ufunsort_1, ufunsort_stable, ufunsort_error, ufunsort_rand].

ufunmerge(suite) -> [];
ufunmerge(doc) -> ["Merge two lists while removing duplicates using a fun."];
ufunmerge(Conf) when is_list(Conf) ->

    Two = [1,2],
    Six = [1,2,3,4,5,6],
    F = fun(X, Y) -> X =< Y end,

    %% 2-way unique merge
    ?line [] = lists:umerge(F, [], []),
    ?line Two = lists:umerge(F, Two, []),
    ?line Two = lists:umerge(F, [], Two),
    ?line Six = lists:umerge(F, [1,3,5], [2,4,6]),
    ?line Six = lists:umerge(F, [2,4,6], [1,3,5]),
    ?line Six = lists:umerge(F, [1,2,3], [4,5,6]),
    ?line Six = lists:umerge(F, [4,5,6], [1,2,3]),
    ?line Six = lists:umerge(F, [1,2,5],[3,4,6]),
    ?line [1,2,3,5,7] = lists:umerge(F, [1,3,5,7], [2]),
    ?line [1,2,3,4,5,7] = lists:umerge(F, [1,3,5,7], [2,4]),
    ?line [1,2,3,4,5,6,7] = lists:umerge(F, [1,3,5,7], [2,4,6]),
    ?line [1,2,3,5,7] = lists:umerge(F, [2], [1,3,5,7]),
    ?line [1,2,3,4,5,7] = lists:umerge(F, [2,4], [1,3,5,7]),
    ?line [1,2,3,4,5,6,7] = lists:umerge(F, [2,4,6], [1,3,5,7]),

    ?line [1,2,3,5,7] = lists:umerge(F, [1,2,3,5,7], [2]),
    ?line [1,2,3,4,5,7] = lists:umerge(F, [1,2,3,4,5,7], [2,4]),
    ?line [1,2,3,4,5,6,7] = lists:umerge(F, [1,3,5,6,7], [2,4,6]),
    ?line [1,2,3,5,7] = lists:umerge(F, [2], [1,2,3,5,7]),
    ?line [1,2,3,4,5,7] = lists:umerge(F, [2,4], [1,2,3,4,5,7]),
    ?line [1,2,3,4,5,6,7] = lists:umerge(F, [2,4,6], [1,2,3,4,5,6,7]),

    L1 = [{1,a},{1,b},{1,a}],
    L2 = [{1,a},{1,b},{1,a}],
    F2 = fun(X,Y) -> element(1,X) =< element(1,Y) end,
    ?line [{1,a},{1,b},{1,a},{1,b},{1,a}] = lists:umerge(F2, L1, L2),
    ?line [{b,2},{c,11},{c,12},{c,21},{c,22},{e,5}] =
	lists:umerge(F2, [{c,11},{c,12},{e,5}], [{b,2},{c,21},{c,22}]),

    ok.

rufunmerge(suite) -> [];
rufunmerge(doc) -> 
    ["Reverse merge two lists while removing duplicates using a fun."];
rufunmerge(Conf) when is_list(Conf) ->
    Two = [2,1],
    Six = [6,5,4,3,2,1],
    F = fun(X, Y) -> X =< Y end,

    %% 2-way reversed unique merge
    ?line [] = lists:rumerge(F, [], []),
    ?line Two = lists:rumerge(F, Two, []),
    ?line Two = lists:rumerge(F, [], Two),
    ?line Six = lists:rumerge(F, [5,3,1], [6,4,2]),
    ?line Six = lists:rumerge(F, [6,4,2], [5,3,1]),
    ?line Six = lists:rumerge(F, [3,2,1], [6,5,4]),
    ?line Six = lists:rumerge(F, [6,5,4], [3,2,1]),
    ?line Six = lists:rumerge(F, [4,3,2],[6,5,1]),
    ?line [7,6,5,3,1] = lists:rumerge(F, [7,5,3,1], [6]),
    ?line [7,6,5,4,3,1] = lists:rumerge(F, [7,5,3,1], [6,4]),
    ?line [7,6,5,4,3,2,1] = lists:rumerge(F, [7,5,3,1], [6,4,2]),
    ?line [7,5,3,2,1] = lists:rumerge(F, [2], [7,5,3,1]),
    ?line [7,5,4,3,2,1] = lists:rumerge(F, [4,2], [7,5,3,1]),
    ?line [7,6,5,4,3,2,1] = lists:rumerge(F, [6,4,2], [7,5,3,1]),

    ?line [7,6,5,3,1] = lists:rumerge(F, [7,6,5,3,1], [6]),
    ?line [7,6,5,4,3,1] = lists:rumerge(F, [7,6,5,4,3,1], [6,4]),
    ?line [7,6,5,4,3,2,1] = lists:rumerge(F, [7,6,5,4,3,2,1], [6,4,2]),
    ?line [7,5,3,2,1] = lists:rumerge(F, [2], [7,5,3,2,1]),
    ?line [7,5,4,3,2,1] = lists:rumerge(F, [4,2], [7,5,4,3,2,1]),
    ?line [7,6,5,4,3,2,1] = lists:rumerge(F, [6,4,2], [7,6,5,4,3,2,1]),

    F2 = fun(X,Y) -> element(1,X) =< element(1,Y) end,
    L1 = [{1,a},{1,b},{1,a}],
    L2 = [{1,a},{1,b},{1,a}],
    ?line true = lists:umerge(F2, L1, L2) == 
	lists:reverse(lists:rumerge(F, lists:reverse(L2), lists:reverse(L1))),

    L3 = [{c,11},{c,12},{e,5}],
    L4 = [{b,2},{c,21},{c,22}],
    ?line true = 
	lists:umerge(F2, L3, L4) == 
	lists:reverse(lists:rumerge(F2,lists:reverse(L3), lists:reverse(L4))),

    ok.

ufunsort_1(doc) ->   ["usort/2"];
ufunsort_1(suite) -> [];
ufunsort_1(Config) when is_list(Config) ->
    ?line ok = ufunsort_check(1, [], []),
    ?line ok = ufunsort_check(1, [{a,b}], [{a,b}]),
    ?line ok = ufunsort_check(1, [{a,b},{a,b}], [{a,b}]),
    ?line ok = ufunsort_check(1, [{a,b},{b,c}], [{a,b},{b,c}]),
    ?line ok = ufunsort_check(1, [{b,c},{a,b}], [{a,b},{b,c}]),
    ?line ok = ufunsort_check(1,
			      [{1,e},{3,f},{2,y},{0,z},{x,14}],
			      [{0,z},{1,e},{2,y},{3,f},{x,14}]),
    ?line ok = ufunsort_check(1,
			      [{1,a},{2,b},{3,c},{2,b},{1,a},{2,b},{3,c},
			       {2,b},{1,a}],
			      [{1,a},{2,b},{3,c}]),
    ?line ok = ufunsort_check(1, 
			      [{1,a},{1,a},{1,b},{1,b},{1,a},{2,a}],
			      [{1,a},{1,b},{1,a},{2,a}]),

    F = funsort_fun(1),
    L1 = [{1,a},{1,b},{1,a}],
    L2 = [{1,a},{1,b},{1,a}],
    ?line ok = ufunsort_check(1, lists:keymerge(1, L1, L2), 
                              lists:umerge(F, L1, L2)),
    L3 = [{1,a},{1,b},{1,a},{2,a}],
    ?line ok = ufunsort_check(1, lists:keymerge(1, L3, L2), 
                              lists:umerge(F, L3, L2)),
    L4 = [{1,b},{1,a}],
    ?line ok = ufunsort_check(1, lists:keymerge(1, L1, L4), 
                              lists:umerge(F, L1, L4)),
    L5 = [{1,a},{1,b},{1,a},{2,a}],
    ?line ok = ufunsort_check(1, lists:keymerge(1, L5, []), 
                              lists:umerge(F, L5, [])),
    L6 = [{3,a}],
    ?line ok = ufunsort_check(1, lists:keymerge(1, L5, L6), 
                              lists:umerge(F, L5, L6)),

    ?line [{b,1},{c,1}] = lists:usort(F, [{c,1},{c,1},{b,1}]),
    ?line [{a,0},{b,2},{c,3},{d,4}] = 
	  lists:usort(F, [{d,4},{c,3},{b,2},{b,2},{a,0}]),
    ?line [{a,0},{b,1},{b,2},{c,1}] =
	  lists:usort(F, [{c,1},{b,1},{b,1},{b,2},{b,2},{a,0}]),
    ?line [{a,0},{b,1},{b,2},{c,1},{d,4}] =
	  lists:usort(F, [{c,1},{b,1},{b,2},{a,0},{a,0},{d,4},{d,4}]),

    %SFun = fun(L) -> fun(X) -> ufunsort_check(1, X, L) end end,
    %PL = [{1,a},{2,b},{3,c},{4,d},{5,e},{6,f}],
    %Ps = perms([{1,a},{2,b},{3,c},{4,d},{5,e},{6,f},{2,b},{1,a}]),
    %?line lists:foreach(SFun(PL), Ps),		%% TODO: too much without GC

    ok.

ufunsort_stable(doc) ->   ["usort/2 should be stable"];
ufunsort_stable(suite) -> [];
ufunsort_stable(Config) when is_list(Config) ->
    ?line ok = ufunsort_check(1, [{1,b},{1,c}], [{1,b},{1,c}]),
    ?line ok = ufunsort_check(1, [{1,c},{1,b}], [{1,c},{1,b}]),
    ?line ok = ufunsort_check(1,
			      [{1,c},{1,b},{2,x},{3,p},{2,a}],
			      [{1,c},{1,b},{2,x},{2,a},{3,p}]),

    %?line ok = ufunsort_check_stability(bigfunlist(10)),
    %?line ok = ufunsort_check_stability(bigfunlist(100)),	%% TODO: after GC
    %?line ok = ufunsort_check_stability(bigfunlist(1000)),
    %?line ok = ufunsort_check_stability(bigfunlist(10000)),
    ok.

ufunsort_error(doc) ->   ["usort/2 should exit when given bad arguments"];
ufunsort_error(suite) -> [];
ufunsort_error(Config) when is_list(Config) ->
    ?line {'EXIT', _} = (catch lists:usort(1, [{1,b} , {1,c}])),
    ?line {'EXIT', _} = (catch lists:usort(fun(X,Y) -> X =< Y end, 
					   [{1,b} | {1,c}])),
    ok.

ufunsort_rand(doc) ->   ["usort/2 on big randomized lists"];
ufunsort_rand(suite) -> [];
ufunsort_rand(Config) when is_list(Config) ->
    ?line ok = ufunsort_check3(1, biglist(10)),
    ?line ok = ufunsort_check3(1, biglist(100)),
    ?line ok = ufunsort_check3(1, biglist(1000)),
    ?line ok = ufunsort_check3(1, biglist(10000)),

    ?line ok = gen_ufunsort_check(1, ubiglist(100)),
    ?line ok = gen_ufunsort_check(1, ubiglist(1000)),
    ?line ok = gen_ufunsort_check(1, ubiglist(10000)),
    ok.

%% Check that usort/2 is stable and correct relative sort/2.
gen_ufunsort_check(I, Input) ->
    U = ufunsort(I, Input),
    S = funsort(I, Input),
    case U == no_dups(S) of
	true ->
	    ok;
	false ->
	    erlang:error({gen_ufunsort_check,Input})
    end.

%% Used for checking that the first copy is kept.
ufunsort_check_stability(L) ->
    I = 1,
    U = ufunsort(I, L),
    S = no_dups(funsort(I, L)),
    check_stab(L, U, S, "usort/2", "sort/2").

ufunsort_check3(I, Input) ->
    ucheck_sorted(I, 3, Input, ufunsort(I, Input)).

%%% Keysort a list, check that the returned list is what we expected,
%%% and that it is actually sorted.
ufunsort_check(I, Input, Expected) ->
    ?line Expected = ufunsort(I, Input),
    ucheck_sorted(I, Input, Expected).

% Do a keysort
ufunsort(I, L) ->
    lists:usort(funsort_fun(I), L).

funsort_fun(I) ->
    fun(A, B) when size(A) >= I, size(B) >= I, is_tuple(A), is_tuple(B) ->
            element(I, A) =< element(I, B)
    end.

check_stab(L, U, S, US, SS) ->
    UP = explicit_pid(U),
    SP = explicit_pid(S),
    case UP == SP of
	true ->
	    ok;
	false ->
	    io:format("In: ~w~n", [explicit_pid(L)]),
	    io:format("~s: ~w~n", [US, UP]),
	    io:format("~s:  ~w~n", [SS, SP]),
	    erlang:error(unstable)
    end.

%%%------------------------------------------------------------
%%% Generate lists of given length, containing 3-tuples with
%%% random integer elements in the range 0..44 as elements 1 and 2.
%%% Element 3 in the tuple is the position of the tuple in the list.

biglist(N) ->
    {A, B, C} = get_seed(),
    random:seed(A, B, C),
    biglist(N, []).

biglist(0, L) ->
    L;
biglist(N, L) ->
    E = random_tuple(45, N),
    biglist(N-1, [E|L]).

%%%------------------------------------------------------------
%%% Generate lists of given length, containing 2-tuples with
%%% random integer elements in the range 0..10 as element 1.
%%% Element 2 in the tuple is a random integer in the range 0..5.
%%% No sequence number.

ubiglist(N) ->
    {A, B, C} = get_seed(),
    random:seed(A, B, C),
    ubiglist(N, []).

ubiglist(0, L) ->
    L;
ubiglist(N, L) ->
    E = urandom_tuple(11, 6),
    ubiglist(N-1, [E|L]).
    
urandom_tuple(N, I) ->
    R1 = randint(N),
    R2 = randint(I),
    {R1, R2}.

%%%------------------------------------------------------------
%%% Generate lists of given length, containing 2-tuples with random
%%% integer elements in the range 0..10 as elements 1. All tuples have
%%% the same function as element 2, but every function is created in a
%%% unique process. ==/2 will return 'true' for any pair of functions,
%%% but erlang:fun_info(Fun, pid) can be used for distinguishing
%%% functions created in different processes. The pid acts like a
%%% sequence number.

bigfunlist(N) ->
    {A, B, C} = get_seed(),
    random:seed(A, B, C),
    bigfunlist_1(N).

bigfunlist_1(N) when N < 30000 -> % Now (R8) max 32000 different pids.
    case catch bigfunlist(N, 0, []) of
	{'EXIT', _} ->
	    bigfunlist_1(N);	%% TODO: it means recurse forever is there is no fun_info()
	Reply ->
	    Reply
    end.

bigfunlist(0, _P, L) ->
    lists:reverse(L);
bigfunlist(N, P, L) ->
    {E, NP} = random_funtuple(P, 11),
    bigfunlist(N-1, NP, [E | L]).
    
random_funtuple(P, N) ->
    R = randint(N),
    F = make_fun(),
    NP = fun_pid(F),
    true = NP > P,
    {{R, F}, NP}.

make_fun() ->
    Pid = spawn(?MODULE, make_fun, [self()]),
    receive {Pid, Fun} -> Fun end.
    
make_fun(Pid) ->
    Pid ! {self(), fun make_fun/1}.

fun_pid(Fun) ->
    erlang:fun_info(Fun, pid).

get_seed() ->
    case random:seed() of
	undefined ->
	    now();
	Tuple ->
	    Tuple
    end.

random_tuple(N, Seq) ->
    R1 = randint(N),
    R2 = randint(N),
    {R1, R2, Seq}.

randint(N) ->
    trunc(random:uniform() * N).

%% The first duplicate is kept.
no_dups([]) ->
    [];
no_dups([H | T]) ->
    no_dups(H, T, []).

no_dups(H, [H1 | T], L) when H == H1 ->
    no_dups(H, T, L);
no_dups(H, [H1 | T], L) ->
    no_dups(H1, T, [H | L]);
no_dups(H, [], L) ->
    lists:reverse([H | L]).

perms([]) ->
    [[]];
perms(L) ->
    [[H|T] || H <- L, T <- perms(L--[H])].

%%%------------------------------------------------------------
%%% Test the sort routines with randomly generated lists.

-record(state, {sort = 0, usort = 0, stable = 0}).

%% Run it interactively. 'stop' or 'info' recognized commands.
sort_loop() ->
    sort_loop(5000).

sort_loop(N) when is_integer(N), N > 0 ->
    Pid = spawn_link(?MODULE, sloop, [N]),
    sort_loop_1(Pid).

sort_loop_1(Pid) ->
    case io:get_line('? ') of
	eof ->
	    ok;
	"stop\n" ->
	    Pid ! {self(), stop},
	    receive {Pid, S} -> display_state(S) end;
	"info\n" ->
	    Pid ! {self(), info},
	    receive {Pid, S} -> display_state(S) end,
	    sort_loop_1(Pid);
	_Other ->
	    sort_loop_1(Pid)
    end.

sloop(N) ->
    {A, B, C} = get_seed(),
    random:seed(A, B, C),
    sloop(N, #state{}).

sloop(N, S) ->
    receive
	{From, stop} ->
	    From ! {self(), S};
	{From, info} ->
	    From ! {self(), S},
	    sloop(N, S)
    after 0 ->
	    Len = randint(N),
	    NS = case randint(3) of
		     0 ->
			 BL = biglist(Len, []),
			 ok = check(BL),
			 ok = keysort_check3(1, BL),
			 ok = funsort_check3(1, BL),
			 S#state{sort = S#state.sort + 1};
		     1 ->
			 BL = ubiglist(Len, []),
			 ok = ucheck(BL),
			 ok = gen_ukeysort_check(1, BL),
			 ok = gen_ufunsort_check(1, BL),
			 S#state{usort = S#state.usort + 1};
		     2 ->
			 BL = bigfunlist(Len),
			 %% ok = check_stability(BL),
			 ok = ucheck_stability(BL),
			 ok = ukeysort_check_stability(BL),
			 ok = ufunsort_check_stability(BL),
			 S#state{stable = S#state.stable + 1}
		 end,
	    sloop(N, NS)
    end.
	    
display_state(S) ->    
    io:format("sort:   ~p~n", [S#state.sort]),
    io:format("usort:  ~p~n", [S#state.usort]),
    io:format("stable: ~p~n", [S#state.stable]).

%% This version of sort/1 is really stable; the order of equal
%% elements is kept. It is used for checking the current
%% implementation of usort/1 etc.

lsort([X, Y | L] = L0) when X =< Y ->
    case L of
	[] -> 
	    L0;
	[Z] when Y =< Z ->
	    L0;
	[Z] when X =< Z ->
	    [X, Z, Y];
	[Z] ->
	    [Z, X, Y];
	_ ->
	    split_1(X, Y, L, [], [])
    end;
lsort([X, Y | L]) ->
    case L of
	[] ->
	    [Y, X];
	[Z] when X =< Z ->
	    [Y, X | L];
	[Z] when Y =< Z ->
	    [Y, Z, X];
	[Z] ->
	    [Z, Y, X];
	_ ->
	    split_2(X, Y, L, [], [])
    end;
lsort([_] = L) ->
    L;
lsort([] = L) ->
    L.

split_1(X, Y, [Z | L], R, Rs) when Z >= Y ->
    split_1(Y, Z, L, [X | R], Rs);
split_1(X, Y, [Z | L], R, Rs) when Z >= X ->
    split_1(Z, Y, L, [X | R], Rs);
split_1(X, Y, [Z | L], [], Rs) ->
    split_1(X, Y, L, [Z], Rs);
split_1(X, Y, [Z | L], R, Rs) ->
    split_1_1(X, Y, L, R, Rs, Z);
split_1(X, Y, [], R, Rs) ->
    rmergel([[Y, X | R] | Rs], [], asc).

split_1_1(X, Y, [Z | L], R, Rs, S) when Z >= Y ->
    split_1_1(Y, Z, L, [X | R], Rs, S);
split_1_1(X, Y, [Z | L], R, Rs, S) when Z >= X ->
    split_1_1(Z, Y, L, [X | R], Rs, S);
split_1_1(X, Y, [Z | L], R, Rs, S) when S =< Z ->
    split_1(S, Z, L, [], [[Y, X | R] | Rs]);
split_1_1(X, Y, [Z | L], R, Rs, S) ->
    split_1(Z, S, L, [], [[Y, X | R] | Rs]);
split_1_1(X, Y, [], R, Rs, S) ->
    rmergel([[S], [Y, X | R] | Rs], [], asc).

split_2(X, Y, [Z | L], R, Rs) when Z < Y ->
    split_2(Y, Z, L, [X | R], Rs);
split_2(X, Y, [Z | L], R, Rs) when Z < X ->
    split_2(Z, Y, L, [X | R], Rs);
split_2(X, Y, [Z | L], [], Rs) ->
    split_2(X, Y, L, [Z], Rs);
split_2(X, Y, [Z | L], R, Rs) ->
    split_2_1(X, Y, L, R, Rs, Z);
split_2(X, Y, [], R, Rs) ->
    mergel([[Y, X | R] | Rs], [], desc).

split_2_1(X, Y, [Z | L], R, Rs, S) when Z < Y ->
    split_2_1(Y, Z, L, [X | R], Rs, S);
split_2_1(X, Y, [Z | L], R, Rs, S) when Z < X ->
    split_2_1(Z, Y, L, [X | R], Rs, S);
split_2_1(X, Y, [Z | L], R, Rs, S) when S > Z ->
    split_2(S, Z, L, [], [[Y, X | R] | Rs]);
split_2_1(X, Y, [Z | L], R, Rs, S) ->
    split_2(Z, S, L, [], [[Y, X | R] | Rs]);
split_2_1(X, Y, [], R, Rs, S) ->
    mergel([[S], [Y, X | R] | Rs], [], desc).

mergel([[] | L], Acc, O) ->
    mergel(L, Acc, O);
mergel([T1, [H2 | T2] | L], Acc, asc) ->
    mergel(L, [merge2_1(T1, H2, T2, []) | Acc], asc);
mergel([[H2 | T2], T1 | L], Acc, desc) ->
    mergel(L, [merge2_1(T1, H2, T2, []) | Acc], desc);
mergel([L], [], _O) ->
    L;
mergel([L], Acc, O) ->
    rmergel([lists:reverse(L, []) | Acc], [], O);
mergel([], [], _O) ->
    [];
mergel([], Acc, O) ->
    rmergel(Acc, [], O);
mergel([A, [] | L], Acc, O) ->
    mergel([A | L], Acc, O);
mergel([A, B, [] | L], Acc, O) ->
    mergel([A, B | L], Acc, O).

rmergel([[H2 | T2], T1 | L], Acc, asc) ->
    rmergel(L, [rmerge2_1(T1, H2, T2, []) | Acc], asc);
rmergel([T1, [H2 | T2] | L], Acc, desc) ->
    rmergel(L, [rmerge2_1(T1, H2, T2, []) | Acc], desc);
rmergel([L], Acc, O) ->
    mergel([lists:reverse(L, []) | Acc], [], O);
rmergel([], Acc, O) ->
    mergel(Acc, [], O).

merge2_1([H1 | T1], H2, T2, M) when H1 =< H2 ->
    merge2_1(T1, H2, T2, [H1 | M]);
merge2_1([H1 | T1], H2, T2, M) ->
    merge2_2(T1, H1, T2, [H2 | M]);
merge2_1([], H2, T2, M) ->
    lists:reverse(T2, [H2 | M]).

merge2_2(T1, H1, [H2 | T2], M) when H1 =< H2 ->
    merge2_1(T1, H2, T2, [H1 | M]);
merge2_2(T1, H1, [H2 | T2], M) ->
    merge2_2(T1, H1, T2, [H2 | M]);
merge2_2(T1, H1, [], M) ->
    lists:reverse(T1, [H1 | M]).

rmerge2_1([H1 | T1], H2, T2, M) when H1 =< H2 ->
    rmerge2_2(T1, H1, T2, [H2 | M]);
rmerge2_1([H1 | T1], H2, T2, M) ->
    rmerge2_1(T1, H2, T2, [H1 | M]);
rmerge2_1([], H2, T2, M) ->
    lists:reverse(T2, [H2 | M]).

rmerge2_2(T1, H1, [H2 | T2], M) when H1 =< H2 ->
    rmerge2_2(T1, H1, T2, [H2 | M]);
rmerge2_2(T1, H1, [H2 | T2], M) ->
    rmerge2_1(T1, H2, T2, [H1 | M]);
rmerge2_2(T1, H1, [], M) ->
    lists:reverse(T1, [H1 | M]).



%% This version of keysort/2 is really stable; the order of equal
%% elements is kept. It is used for checking the current
%% implementation of ukeysort/2 etc.

lkeysort(Index, L) when is_integer(Index), Index > 0 ->
    case L of
	[] -> L;
	[_] -> L;
	[X, Y | T] ->
	    EX = element(Index, X),
	    EY = element(Index, Y),
	    if
		EX =< EY ->
		    keysplit_1(Index, X, EX, Y, EY, T, [], []);
		true ->
		    keysplit_2(Index, Y, EY, T, [X])
	    end
    end.

keysplit_1(I, X, EX, Y, EY, [Z | L], R, Rs) ->
    EZ = element(I, Z),
    if 
	EY =< EZ ->
	    keysplit_1(I, Y, EY, Z, EZ, L, [X | R], Rs);
	EX =< EZ ->
	    keysplit_1(I, Z, EZ, Y, EY, L, [X | R], Rs);
	true, R == [] ->
	    keysplit_1(I, X, EX, Y, EY, L, [Z], Rs);
	true ->
	    keysplit_1_1(I, X, EX, Y, EY, L, R, Rs, Z, EZ)
    end;
keysplit_1(I, X, _EX, Y, _EY, [], R, Rs) ->
    rkeymergel(I, [[Y, X | R] | Rs], []).

%% One out-of-order element, S.
keysplit_1_1(I, X, EX, Y, EY, [Z | L], R, Rs, S, ES) ->
    EZ = element(I, Z),
    if
	EY =< EZ ->
	    keysplit_1_1(I, Y, EY, Z, EZ, L, [X | R], Rs, S, ES);
	EX =< EZ ->
	    keysplit_1_1(I, Z, EZ, Y, EY, L, [X | R], Rs, S, ES);
	ES =< EZ ->
	    keysplit_1(I, S, ES, Z, EZ, L, [], [[Y, X | R] | Rs]);
	true ->
	    keysplit_1(I, Z, EZ, S, ES, L, [], [[Y, X | R] | Rs])
    end;
keysplit_1_1(I, X, _EX, Y, _EY, [], R, Rs, S, _ES) ->
    rkeymergel(I, [[S], [Y, X | R] | Rs], []).

%% Descending.
keysplit_2(I, Y, EY, [Z | L], R) ->
    EZ = element(I, Z),
    if
	EY =< EZ ->
            keysplit_1(I, Y, EY, Z, EZ, L, [], [lists:reverse(R, [])]);
        true ->
            keysplit_2(I, Z, EZ, L, [Y | R])
    end;
keysplit_2(_I, Y, _EY, [], R) ->
    [Y | R].

keymergel(I, [T1, [H2 | T2] | L], Acc) ->
    keymergel(I, L, [keymerge2_1(I, T1, element(I, H2), H2, T2, []) | Acc]);
keymergel(_I, [L], []) ->
    L;
keymergel(I, [L], Acc) ->
    rkeymergel(I, [lists:reverse(L, []) | Acc], []);
keymergel(I, [], Acc) ->
    rkeymergel(I, Acc, []).

rkeymergel(I, [[H2 | T2], T1 | L], Acc) ->
    rkeymergel(I, L, [rkeymerge2_1(I, T1, element(I, H2), H2, T2, []) | Acc]);
rkeymergel(I, [L], Acc) ->
    keymergel(I, [lists:reverse(L, []) | Acc], []);
rkeymergel(I, [], Acc) ->
    keymergel(I, Acc, []).

keymerge2_1(I, [H1 | T1], E2, H2, T2, M) ->
    E1 = element(I, H1),
    if 
	E1 =< E2 ->
	    keymerge2_1(I, T1, E2, H2, T2, [H1 | M]);
	true ->
	    keymerge2_2(I, T1, E1, H1, T2, [H2 | M])
    end;
keymerge2_1(_I, [], _E2, H2, T2, M) ->
    lists:reverse(T2, [H2 | M]).

keymerge2_2(I, T1, E1, H1, [H2 | T2], M) ->
    E2 = element(I, H2),
    if
	E1 =< E2 ->
	    keymerge2_1(I, T1, E2, H2, T2, [H1 | M]);
	true ->
	    keymerge2_2(I, T1, E1, H1, T2, [H2 | M])
    end;
keymerge2_2(_I, T1, _E1, H1, [], M) ->
    lists:reverse(T1, [H1 | M]).

rkeymerge2_1(I, [H1 | T1], E2, H2, T2, M) ->
    E1 = element(I, H1),
    if
	E1 =< E2 ->
	    rkeymerge2_2(I, T1, E1, T2, [H2 | M], H1);
	true ->
	    rkeymerge2_1(I, T1, E2, H2, T2, [H1 | M])
    end;
rkeymerge2_1(_I, [], _E2, H2, T2, M) ->
    lists:reverse(T2, [H2 | M]).

rkeymerge2_2(I, T1, E1, [H2 | T2], M, H1) ->
    E2 = element(I, H2),
    if
	E1 =< E2 ->
	    rkeymerge2_2(I, T1, E1, T2, [H2 | M], H1);
	true ->
	    rkeymerge2_1(I, T1, E2, H2, T2, [H1 | M])
    end;
rkeymerge2_2(_I, T1, _E1, [], M, H1) ->
    lists:reverse(T1, [H1 | M]).


%%%------------------------------------------------------------

seq(doc) ->
    ["Tests lists:seq/3"];
seq(suite) ->
    [
     seq_loop,
     seq_2, seq_3, seq_2_e, seq_3_e].

seq_loop(doc) ->
    ["Test for infinite loop (OTP-2404)."];
seq_loop(suite) ->
    [];
seq_loop(Config) when is_list(Config) ->
    ?line _ = (catch lists:seq(1, 5, -1)),
    ok.

seq_2(doc) ->
    ["Non-error cases for seq/2"];
seq_2(suite) ->
    [];
seq_2(Config) when is_list(Config) ->
    ?line [1,2,3] = lists:seq(1,3),
    ?line [1] = lists:seq(1,1),
    ?line Big = 748274827583793785928592859,
    ?line Big1 = Big+1,
    ?line Big2 = Big+2,
    ?line [Big, Big1, Big2] = lists:seq(Big, Big+2),
    ok.

seq_2_e(doc) ->
    ["Error cases for seq/2"];
seq_2_e(suite) ->
    [];
seq_2_e(Config) when is_list(Config) ->
    ?line seq_error([4, 2]),
    ?line seq_error([1, a]),
    ?line seq_error([1.0, 2.0]),
    ok.

seq_error(Args) ->
    {'EXIT', _} = (catch apply(lists, seq, Args)).

seq_3(doc) ->
    ["Non-error cases for seq/3"];
seq_3(suite) ->
    [];
seq_3(Config) when is_list(Config) ->
	?line [] = lists:seq(4, 3),
	?line [] = lists:seq(4, 3, 1),
	?line [] = lists:seq(3, 4, -1),

    ?line [1,2,3] = lists:seq(1,3,1),
    ?line [1] = lists:seq(1,1,1),
    ?line Big = 748274827583793785928592859,
    ?line Big1 = Big+1,
    ?line Big2 = Big+2,
    ?line [Big, Big1, Big2] = lists:seq(Big, Big+2,1),

    ?line [3,2,1] = lists:seq(3,1,-1),
    ?line [1] = lists:seq(1,1,-1),

    ?line [3,1] = lists:seq(3,1,-2),
    ?line [1] = lists:seq(1, 10, 10),
    ?line [1, 4, 7, 10, 13, 16, 19] = lists:seq(1, 19, 3),
    ?line [1, 4, 7, 10, 13, 16, 19] = lists:seq(1, 20, 3),
    ?line [1, 4, 7, 10, 13, 16, 19] = lists:seq(1, 21, 3),

    ?line [1] = lists:seq(1, 1, 0),		%OTP-2613
    ok.

seq_3_e(doc) ->
    ["Error cases for seq/3"];
seq_3_e(suite) ->
    [];
seq_3_e(Config) when is_list(Config) ->
    ?line seq_error([4, 2, 1]),
    ?line seq_error([2, 4, -1]),
    ?line seq_error([1, a, 1]),
    ?line seq_error([1.0, 2.0, 1]),

    ?line seq_error([1, 3, 1.0]),
    ?line seq_error([1, 3, a]),
    ?line seq_error([1, 3, 0]),

    ?line seq_error([a, a, 0]),
    ok.

%%%------------------------------------------------------------

sublist(doc) ->
    ["Tests lists:sublist/[2,3]"];
sublist(suite) ->
    [sublist_2, sublist_3, sublist_2_e, sublist_3_e].

-define(sublist_error2(X,Y), ?line {'EXIT', _} = (catch lists:sublist(X,Y))).
-define(sublist_error3(X,Y,Z), ?line {'EXIT', _} = (catch lists:sublist(X,Y,Z))).

sublist_2(doc) ->   ["sublist/2"];
sublist_2(suite) -> [];
sublist_2(Config) when is_list(Config) ->
    ?line [] = lists:sublist([], 0),
    ?line [] = lists:sublist([], 1),
    ?line [] = lists:sublist([a], 0),
    ?line [a] = lists:sublist([a], 1),
    ?line [a] = lists:sublist([a], 2),
    ?line [a] = lists:sublist([a|b], 1),

    ?line [a,b] = lists:sublist([a,b|c], 2),

    ok.

sublist_2_e(doc) ->   ["sublist/2 error cases"];
sublist_2_e(suite) -> [];
sublist_2_e(Config) when is_list(Config) ->
    ?sublist_error2([], -1),
    ?sublist_error2(a, -1),
    ?sublist_error2(a, 0),
    ?sublist_error2([a|b], 2),
    ?sublist_error2([a], x),
    ?sublist_error2([a], 1.5),
    ?sublist_error2([], x),
    ?sublist_error2([], 1.5),
    ok.

sublist_3(doc) ->   ["sublist/3"];
sublist_3(suite) -> [];
sublist_3(Config) when is_list(Config) ->
    ?line [] = lists:sublist([], 1, 0),
    ?line [] = lists:sublist([], 1, 1),
    ?line [] = lists:sublist([a], 1, 0),
    ?line [a] = lists:sublist([a], 1, 1),
    ?line [a] = lists:sublist([a], 1, 2),
    ?line [a] = lists:sublist([a|b], 1, 1),

    ?line [] = lists:sublist([], 1, 0),
    ?line [] = lists:sublist([], 1, 1),
    ?line [] = lists:sublist([a], 1, 0),
    ?line [a] = lists:sublist([a], 1, 1),
    ?line [a] = lists:sublist([a], 1, 2),
    ?line [] = lists:sublist([a], 2, 1),
    ?line [] = lists:sublist([a], 2, 2),
    ?line [] = lists:sublist([a], 2, 79),
    ?line [] = lists:sublist([a,b|c], 1, 0),
    ?line [] = lists:sublist([a,b|c], 2, 0),
    ?line [a] = lists:sublist([a,b|c], 1, 1),
    ?line [b] = lists:sublist([a,b|c], 2, 1),
    ?line [a,b] = lists:sublist([a,b|c], 1, 2),

    ?line [] = lists:sublist([a], 2, 0),

    ok.

sublist_3_e(doc) ->   ["sublist/3 error cases"];
sublist_3_e(suite) -> [];
sublist_3_e(Config) when is_list(Config) ->
    ?sublist_error3([], 1, -1),
    ?sublist_error3(a, 1, -1),
    ?sublist_error3(a, 1, 0),
    ?sublist_error3([a|b], 1, 2),
    ?sublist_error3([a], 1, x),
    ?sublist_error3([a], 1, 1.5),
    ?sublist_error3([], 1, x),
    ?sublist_error3([], 1, 1.5),

    ?sublist_error3([], -1, 0),
    ?sublist_error3(a, x, -1),
    ?sublist_error3([a,b], 0.5, 1),
    ?sublist_error3([a,b], 1.5, 1),
    ?sublist_error3([a], 1, x),
    ?sublist_error3([a], 1, 1.5),
    ?sublist_error3([], 1, x),
    ?sublist_error3([], 1, 1.5),

    ?sublist_error3([a], 0, -1),
    ?sublist_error3([a], 1, -1),
    ?sublist_error3([a], 2, -1),
    ?sublist_error3([a], 0, 0),
    ?sublist_error3([a], 0, 1),

    ?sublist_error3([a,b|c], 2, 2),
    ?sublist_error3([a,b|c], 3, 0),
    ?sublist_error3([a,b|c], 3, 1),
    ok.

%%%------------------------------------------------------------

flatten(doc) ->
    ["Tests lists:flatten/[1,2]"];
flatten(suite) ->
    [flatten_1, flatten_2, flatten_1_e, flatten_2_e].

-define(flatten_error1(X), ?line {'EXIT', _} = (catch lists:flatten(X))).
-define(flatten_error2(X,Y), ?line {'EXIT', _} = (catch lists:flatten(X,Y))).

flatten_1(doc) ->   ["flatten/1"];
flatten_1(suite) -> [];
flatten_1(Config) when is_list(Config) ->
    ?line [] = lists:flatten([]),
    ?line [1,2] = lists:flatten([1,2]),
    ?line [1,2] = lists:flatten([1,[2]]),
    ?line [1,2] = lists:flatten([[1],2]),
    ?line [1,2] = lists:flatten([[1],[2]]),
    ?line [1,2] = lists:flatten([[1,2]]),
    ?line [a,b,c,d] = lists:flatten([[a],[b,c,[d]]]),

    ok.

flatten_1_e(doc) ->   ["flatten/1 error cases"];
flatten_1_e(suite) -> [];
flatten_1_e(Config) when is_list(Config) ->
    ?flatten_error1(a),
    ?flatten_error1([a|b]),
    ?flatten_error1([[a],[b|c],[d]]),
    ok.

%%% [arndt] What if second arg isn't a proper list? This issue isn't
%%% clear-cut. Right now, I think that any term should be allowed.
%%% But I also wish this function didn't exist at all.

flatten_2(doc) ->   ["flatten/2"];
flatten_2(suite) -> [];
flatten_2(Config) when is_list(Config) ->
    ?line [] = lists:flatten([]),
    ?line [a] = lists:flatten([a]),
    ok.

flatten_2_e(doc) ->   ["flatten/2 error cases"];
flatten_2_e(suite) -> [];
flatten_2_e(Config) when is_list(Config) ->
    ok.
