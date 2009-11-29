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
-module(x_asmopt).
-export([optimize/3]).

-import(lists, [reverse/1,member/2,keysearch/3,map/2,keysort/2]).
-import(lists, [foldl/3]).

-define(MAX_OPT_SEQ, 16).

-record(os, {updated=false,entries=[]}).

optimize(Asm0, Entries, Opts) ->
	case member(best_opt_seq, Opts) of
	true ->
		Algos = [
			{unused_labels, fun unused_labels/2},
			{rewrites, fun rewrites/2},
			{dead_wood, fun dead_wood/2},
			{coalesce_labels, fun coalesce_labels/2},
			{continuation, fun continuation/2}
		],
		best_sequence(Asm0, length(Asm0), Algos, ?MAX_OPT_SEQ, #os{entries=Entries});
	false ->
		Seq = [
			fun dead_wood/2,
			fun unused_labels/2,
			fun rewrites/2,
			fun coalesce_labels/2,
			fun continuation/2,
			fun dead_wood/2
		],
		
		{Asm1,_} = foldl(fun(F, {Asm1,St1}) ->
			F(Asm1, St1)
		end, {Asm0,#os{entries=Entries}}, Seq),

		{ok,Asm1}
	end.

best_sequence(Asm, _, _, 0, St) -> {Asm,St};
best_sequence(Asm, OrigLen, Algos, Depth, St) ->
	N0 = length(Asm),
	
	Results = map(fun({Name,F}) ->
		{Asm1,_} = F(Asm, St),
		{Name,Asm1,length(Asm1)}
	end, Algos),
	[{Best,Asm1,N1}|_] = keysort(3, Results),
	
	Drop = (N0-N1)*100 div OrigLen,
	io:format("~w: ~w (~w%)~n", [Best,N0-N1,Drop]),
	
	best_sequence(Asm1, OrigLen, Algos, Depth-1, St).

%%
%%  Remove unused labels
%%

%% TODO: derive which comands use labels from optab2

label_uses([{jump_if,_,L}|Asm], Ls) -> label_uses(Asm, [L|Ls]);
label_uses([{jump_if_not,_,L}|Asm], Ls) -> label_uses(Asm, [L|Ls]);
label_uses([{jump,_,L}|Asm], Ls) -> label_uses(Asm, [L|Ls]);
label_uses([{call,_,L}|Asm], Ls) -> label_uses(Asm, [L|Ls]);
label_uses([{tail_call,_,L}|Asm], Ls) -> label_uses(Asm, [L|Ls]);
label_uses([{'catch',_,L}|Asm], Ls) -> label_uses(Asm, [L|Ls]);
label_uses([{get_msg,_,L}|Asm], Ls) -> label_uses(Asm, [L|Ls]);
label_uses([_|Asm], Ls) -> label_uses(Asm, Ls);
label_uses([], Ls) -> Ls.

unused_labels(Asm, St) ->
    unused_labels(Asm, label_uses(Asm, St#os.entries), [], St).
unused_labels([{l,_,L}=A|Asm], Ls, O, St) ->
    case member(L, Ls) of
      true -> unused_labels(Asm, Ls, [A|O], St);
      false -> unused_labels(Asm, Ls, O, St#os{updated=true})
    end;
unused_labels([A|Asm], Ls, O, St) ->
    unused_labels(Asm, Ls, [A|O], St);
unused_labels([], _, O, St) -> {reverse(O),St}.

%%
%%  Rewrite certain code sequences
%%

rewrites(Asm, St) -> rewrites(Asm, [], St).

%% remove redundant list_copy

rewrites([{make_cons,_}=A,{list_copy,_}|Asm], O, St) ->
	rewrites(Asm, [A|O], St#os{updated=true});
rewrites([{make_cons_nil,_}=A,{list_copy,_}|Asm], O, St) ->
	rewrites(Asm, [A|O], St#os{updated=true});

rewrites([{push_args,_,0}|Asm], O, St) ->
	rewrites(Asm, O, St#os{updated=true});
rewrites([{set_args_args,_,0}|Asm], O, St) ->
	rewrites(Asm, O, St#os{updated=true});

rewrites([{drop,_,0}|Asm], O, St) ->
    rewrites(Asm, O, St#os{updated=true});

rewrites([{drop,A,N},{drop,_,K}|Asm], O, St) ->
    rewrites([{drop,A,N+K}|Asm], O, St#os{updated=true});

rewrites([{lit,_,true},{jump_if_not,_,_}|Asm], O, St) ->
    rewrites(Asm, O, St#os{updated=true});
rewrites([{lit,_,false},{jump_if,_,_}|Asm], O, St) ->
    rewrites(Asm, O, St#os{updated=true});

rewrites([{dup,_},{set_var,A,N},{drop,_,1}|Asm], O, St) ->
    rewrites(Asm, [{set_var,A,N}|O], St#os{updated=true});
rewrites([{dup,_},{set_var,A0,N},{drop,A1,K}|Asm], O, St) ->
    rewrites(Asm, [{drop,A1,K-1},{set_var,A0,N}|O], St#os{updated=true});

rewrites([{jump,_,L},{l,_,L}=A|Asm], O, St) ->
    rewrites(Asm, [A|O], St#os{updated=true});
rewrites([{jump_if_not,A,L1},{jump,_,L2},{l,_,L1}|Asm], O, St) ->
    rewrites(Asm, [{l,[],L1},{jump_if,A,L2}|O], St#os{updated=true});
rewrites([{jump_if,A,L1},{jump,_,L2},{l,_,L1}|Asm], O, St) ->
    rewrites(Asm, [{l,[],L1},{jump_if_not,A,L2}|O], St#os{updated=true});

rewrites([{set_var,_,N},{clear_var,_,N}|Asm], O, St) ->
    rewrites(Asm, O, St#os{updated=true});

rewrites([{dup,_},{drop,_,1}|Asm], O, St) ->
    rewrites(Asm, O, St#os{updated=true});
rewrites([{get_arg,_,_},{drop,_,1}|Asm], O, St) ->
    rewrites(Asm, O, St#os{updated=true});
rewrites([{get_var,_,_},{drop,_,1}|Asm], O, St) ->
    rewrites(Asm, O, St#os{updated=true});

rewrites([A|Asm], O, St) -> rewrites(Asm, [A|O], St);
rewrites([], O, St) -> {reverse(O),St}.

%%
%%  Remove unreachable code
%%

dead_wood(Asm, St) -> burn_wood(Asm, [], St).

%% TODO: certain calls never return (exit(),error(),etc)

burn_wood([{l,_,_}=A|Asm], O, St) -> dead_wood(Asm, [A|O], St);
burn_wood([_|Asm], O, St) -> burn_wood(Asm, O, St#os{updated=true});
burn_wood([], O, St) -> {reverse(O),St}.

dead_wood([{ret,_,_}=A|Asm], O, St) -> burn_wood(Asm, [A|O], St);
dead_wood([{jump,_,_}=A|Asm], O, St) -> burn_wood(Asm, [A|O], St);
dead_wood([{tail_call,_,_}=A|Asm], O, St) -> burn_wood(Asm, [A|O], St);
dead_wood([{tail_call_far,_,_}=A|Asm], O, St) -> burn_wood(Asm, [A|O], St);
dead_wood([{raise,_}=A|Asm], O, St) -> burn_wood(Asm, [A|O], St);
dead_wood([A|Asm], O, St) -> dead_wood(Asm, [A|O], St);
dead_wood([], O, St) -> {reverse(O),St}.

%%
%%  Coalesce equivalent labels
%%

coalesce_labels(Asm0, St0) ->
    {Asm1,Ls,St1} = equal_labels(Asm0, St0),
    {rename_labels(Asm1, Ls),St1}.

equal_labels(Asm, St) -> equal_labels(Asm, [], [], St).

equal_labels([{l,_,L}=A|Asm], Ls, O, St) ->
    equal_labels1(Asm, Ls, [A|O], L, St);
equal_labels([A|Asm], Ls, O, St) ->
    equal_labels(Asm, Ls, [A|O], St);
equal_labels([], Ls, O, St) -> {reverse(O),Ls,St}.

equal_labels1([{l,_,L}|Asm], Ls, O, E, St) ->
    equal_labels1(Asm, [{L,E}|Ls], O, E, St#os{updated=true});
equal_labels1([A|Asm], Ls, O, _, St) ->
    equal_labels(Asm, Ls, [A|O], St).

%%
%%	lab_L:
%%		jump lab_L1
%%

continuation(Asm0, St0) -> continuation(Asm0, St0, [], []).
continuation([{l,_,L},{jump,_,L1}=J|Asm0], St0, Os, Es) ->
	continuation(Asm0, St0#os{updated=true}, [J|Os], [{L,L1}|Es]);
continuation([A|Asm0], St0, Os, Es) ->
	continuation(Asm0, St0, [A|Os], Es);
continuation([], St0, Os, Es) ->
	Es1 = terminate(Es),
	Os1 = rename_labels(reverse(Os), Es1),
	{Os1,St0}.

terminate(Es) ->
	Es0 = lists:sort(fun({_,A},{B,_}) -> A =:= B end, Es),
	
	lists:foldl(fun({A,B}, Es1) ->
		lists:map(fun({X,Y}) when Y =:= A ->
			{X,B};
		(XY) ->
			XY
		end, Es1)
	end, Es0, Es0).

%%%%
	
rename_labels(Asm, Ls) -> rename_labels(Asm, Ls, []).

rename_labels([A|Asm], Ls, O) ->
    rename_labels(Asm, Ls, [ren_lab(A, Ls)|O]);
rename_labels([], _, O) -> reverse(O).

ren_lab({jump_if,A,L}, Ls) -> {jump_if,A,rl(L, Ls)};
ren_lab({jump_if_not,A,L}, Ls) -> {jump_if_not,A,rl(L, Ls)};
ren_lab({jump,A,L}, Ls) -> {jump,A,rl(L, Ls)};
ren_lab({call,A,L}, Ls) -> {call,A,rl(L, Ls)};
ren_lab({tail_call,A,L}, Ls) -> {tail_call,A,rl(L, Ls)};
ren_lab({'catch',A,L}, Ls) -> {'catch',A,rl(L, Ls)};
ren_lab({get_msg,A,L}, Ls) -> {get_msg,A,rl(L, Ls)};
ren_lab(A, _) -> A.

rl(L, Ls) ->
    case keysearch(L, 1, Ls) of
      {value,{_,R}} -> R;
      false -> L
    end.

%% EOF
