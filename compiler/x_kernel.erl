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
-module(x_kernel).
-export([module/2]).

-import(lists, [reverse/1,map/2,foldl/3,mapfoldl/3]).
-import(lists, [member/2,keysearch/3,keyreplace/4,seq/2]).
-import(lists, [zip/2,zip3/3,unzip3/1,duplicate/2,filter/2]).
-import(lists, [partition/2,mapfoldr/3,foldr/3]).
-import(lists, [usort/1,split/2]).

-include("core_parse.hrl").
-include("pat_match.hrl").

-record(ks, {mod,vars=[],defs=[],subs=[],tail=true}).
-record(kr, {next=1,outer=[],lambdas=[],seen=[],vno=0,life=[],calls=[]}).

-define(SPLIT_MATCH_RANGE, 6).

module(#c_module{name=#c_literal{val=Mod},
		exports=Exps,defs=Ds}=Code0, _Opts) ->
	%io:format("Code0=~p~n", [Code0]),
	_Entries = [{F,N} || #c_fname{id=F,arity=N} <- Exps],
    {Ds1,Rs1} = mapfoldl(fun(D, Rs) ->
            kernel(D, #ks{mod=Mod}, Rs)
        end, #kr{}, Ds),
    %io:format("Ds1=~p~n", [Ds1]),

    %io:format("Entries,Calls=~p,~p~n", [Entries,Rs1#kr.calls]),
    %% any local function which was called is retained
    %% irregardless on whether it was called from the
    %% exported function chain or not -- TODO
    
    %Ds2 = filter(fun({#c_fname{id=F,arity=N},_}) ->
	%		member({F,N}, Entries) orelse
	%		member({F,N}, Rs1#kr.calls) orelse
	%		member({F,N,export}, Rs1#kr.calls)
	%	end, Ds1),
	%io:format("Ds2=~p~n", [Ds2]),
	
	%% no filtering for now
	Ds2 = Ds1,
	
	%% export funs
	Exps2 = [FN || {FN,_} <- Rs1#kr.lambdas],
	Exps3 = [#c_fname{id=F,arity=N} || {F,N,export} <- Rs1#kr.calls],
	
    {ok,Code0#c_module{exports=Exps3++Exps2++Exps,defs=Ds2++Rs1#kr.lambdas}}.

kernel({#c_fname{id=F,arity=N},
        #c_fun{anno=A,vars=Vs,body=E}}, Sub0, Rs0) ->
    
    {Vs1,Sub1,Rs1} = new_vars(Vs, true,
        #ks{mod=Sub0#ks.mod,defs=Sub0#ks.defs},
        #kr{next=Rs0#kr.next,calls=Rs0#kr.calls}),
    {L,Rs2} = new_label(Rs1),
    {E1,Rs3} = kernel(E, Sub1, Rs2),
    %io:format("Rs3=~p~n", [Rs3]),
    
    Outer = Rs3#kr.outer,
    Vs2 = [#c_var{name=V} || V <- Outer],
    N1 = length(Outer),
    
    A1 = if
      N1 > 0 ->
		{Life1,Life2} = partition(fun({_,ice,_}) ->
				true;
			(_) ->
				false
			end, Rs3#kr.life),
		{Names,_,Lasts} = unzip3(reverse(Life1)),
		ArgLife1 = zip3(Names,
			[{arg,X} || X <- seq(N, N+N1-1)],
			Lasts),
		[{life,ArgLife1++Life2}|A];
	  true->
		[{life,Rs3#kr.life}|A]
	end,
 
    Code0 = #c_fun{anno=A1,vars=Vs1++Vs2,body=E1},
    {{#c_fname{anno=[{l,L},{outer,Outer}],id=F,arity=N+N1},Code0},
        Rs0#kr{next=Rs3#kr.next,
			lambdas=Rs0#kr.lambdas++Rs3#kr.lambdas,
			calls=Rs3#kr.calls}};

kernel(#c_letrec{defs=Ds,body=E}, Sub0, #kr{next=M}=Rs0) ->

    %% Ds are transformed two times, from the first time
    %% only outer vars are used, everything else is
    %% discarded -- potentially is exponentially complex.
    %% Reason: provide for proper c_apply transformation
    %% after outer vars are known --
    
    {Ds1,_} = mapfoldl(fun(D, Rs) ->
            kernel(D, #ks{mod=Sub0#ks.mod,defs=Sub0#ks.defs}, Rs)
        end, Rs0#kr{next=M+1}, Ds),

    Defs = map(fun({#c_fname{anno=A,id=F,arity=N},_}) ->
		X = atom_to_list(F) ++ "-" ++ integer_to_list(M),
		F1 = list_to_atom(X),
        case keysearch(outer, 1, A) of
        {value,{_,Outer}} ->
            N1 = length(Outer),
            {{F,N-N1},Outer,F1}; %% kludgy :-)
        false ->
            {{F,N},[],F1}
        end
    end, Ds1),       
    %io:format("~p~n~n", [Defs]),
        
    %% second pass -- see above
    {Ds2,Rs2} = mapfoldl(fun({#c_fname{id=F,arity=N}=FN,B}=D, Rs) ->
		case keysearch({F,N}, 1, Defs) of
		{value,{_,_,F1}} ->
			kernel({FN#c_fname{id=F1},B},
				#ks{mod=Sub0#ks.mod,defs=Defs++Sub0#ks.defs}, Rs);
		false -> %% happens for nested comprehensions
			kernel(D, #ks{mod=Sub0#ks.mod,defs=Defs++Sub0#ks.defs}, Rs)
		end	
    end, Rs0#kr{next=M+1}, Ds),

    kernel(E,
        Sub0#ks{defs=Defs++Sub0#ks.defs},
        Rs2#kr{lambdas=Ds2++Rs2#kr.lambdas,next=Rs2#kr.next});
  
%% generated by 'fun abc/1' expessions
kernel(#c_fname{id=F,arity=N}, _Sub0, #kr{calls=Calls}=Rs0) ->
	{#c_call{module=#c_literal{val=erlang},
		name=#c_literal{val=make_fun},
		args=[#c_literal{val=F},#c_literal{val=N},#c_literal{val={}}]},
			Rs0#kr{calls=[{F,N,export}|Calls]}};

kernel(#c_fun{anno=A,vars=Vs,body=E}=Code0, Sub0, Rs0) ->
    {Vs1,Sub1,Rs1} = new_vars(Vs, true,
        #ks{mod=Sub0#ks.mod},
        #kr{next=Rs0#kr.next,calls=Rs0#kr.calls}),
    {L,Rs2} = new_label(Rs1),
    {E1,RsN} = kernel(E, Sub1, Rs2),
    
    {F,Rs3} = case keysearch(id, 1, A) of
    {value,{id,{_,_,Y}}} -> %% already generated
		{Y,RsN};
    false -> %% generate from label number
		Y = "-fun-" ++ integer_to_list(RsN#kr.next),
		{list_to_atom(Y),RsN#kr{next=RsN#kr.next+1}}
	end,
    
    Vs2 = [#c_var{name=V} || V <- Rs3#kr.outer],
    Vars = Vs1++Vs2,
    N = length(Vars),
    
    N1 = length(Rs3#kr.outer),
    A1 = if
      N1 > 0 ->
		{Life1,Life2} = partition(fun({_,ice,_}) ->
				true;
			(_) ->
				false
			end, Rs3#kr.life),
		{Names,_,Lasts} = unzip3(reverse(Life1)),
		ArgLife1 = zip3(Names,
			[{arg,X} || X <- seq(N-N1, N-1)],
			Lasts),
		[{life,ArgLife1++Life2}|A];
	  true->
		[{life,Rs3#kr.life}|A]
	end,

    Code1 = {#c_fname{anno=[{l,L}],id=F,arity=N},
        Code0#c_fun{anno=A1,vars=Vars,body=E1}},
    As = [#c_literal{val=F},#c_literal{val=N},#c_tuple{es=Vs2}],
    Code2 = #c_call{module=#c_literal{val=erlang},
        name=#c_literal{val=make_fun},
        args=As},
    kernel(Code2, Sub0, Rs0#kr{next=Rs3#kr.next,
        lambdas=[Code1|Rs0#kr.lambdas++Rs3#kr.lambdas],
        calls=Rs3#kr.calls});

kernel(#c_let{anno=A,vars=Vs,arg=R,body=E}, Sub0, Rs0) ->
    {R1,Rs1} = kernel(R, Sub0#ks{tail=false}, Rs0),
    {Vs1,Sub1,Rs2} = new_vars(Vs, false, Sub0, Rs1),
    {L,Rs3} = new_label(Rs2),
    {E1,Rs4} = kernel(E, Sub1, Rs3),
    {#c_let{anno=[{l,L}|A],vars=Vs1,arg=R1,body=E1},Rs4};

kernel(#c_try{anno=A,arg=R,vars=Vs,body=B,evars=Es,handler=H}, Sub0, Rs0) ->
    {L,RsN} = new_label(Rs0),
    {R1,Rs1} = kernel(R, Sub0#ks{tail=false}, RsN),
    {Vs1,Sub1,Rs2} = new_vars(Vs, false, Sub0, Rs1),
    {B1,Rs3} = kernel(B, Sub1, Rs2),
    {Es1,Sub2,Rs4} = new_vars(Es, false, Sub1, Rs3),
    {H1,Rs5} = kernel(H, Sub2, Rs4),
    {#c_try{anno=[{l,L}|A],
        arg=R1,vars=Vs1,body=B1,evars=Es1,handler=H1},Rs5};

kernel(#c_receive{anno=A,clauses=Cs,timeout=T,action=E}, Sub0, #kr{vno=Vno}=Rs0) ->
	U = #c_var{name=list_to_atom("_u" ++ integer_to_list(Vno))},
	{L,RsN} = new_label(Rs0#kr{vno=Vno+1}),
	{S1,Rs1} = match1([U], Cs, #krecloop{}, Sub0, RsN),
	%io:format("S1=~p~n", [S1]),
	kernel(#kreceive{anno=[{l,L}|A],var=U,body=S1,timeout=T,action=E}, Sub0, Rs1);

kernel(#c_case{arg=Arg,clauses=Cs}, Sub0, Rs0) ->
	Us0 = case Arg of
	#c_values{es=Us} -> Us;
	X -> [X]
	end,
	{Us1,{Vs,As,Vno}} = mapfoldl(fun(#c_var{}=U, {Vs,As,Vno}) ->
		{U,{Vs,As,Vno}};
	(E, {Vs,As,Vno}) ->
		U = #c_var{name=list_to_atom("_u" ++ integer_to_list(Vno))},
		{U,{[U|Vs],[E|As],Vno+1}}
	end, {[],[],Rs0#kr.vno}, Us0),
	{L,RsN} = new_label(Rs0#kr{vno=Vno}),
	{S1,Rs1} = match1(Us1, Cs, #kfail{desc=compiler}, Sub0, RsN),
	Wadler = #kwadler{anno=[{l,L}],body=S1},
	T = if Vs =:= [] -> Wadler;
	true -> #c_let{vars=Vs,arg=#c_values{es=As},body=Wadler} end,
	kernel(T, Sub0, Rs1);

%kernel(#c_case{arg=Arg,clauses=Cs}, Sub0, #kr{vno=Vno}=Rs0) ->
%	U = #c_var{name=list_to_atom("_u" ++ integer_to_list(Vno))},
%	{L,RsN} = new_label(Rs0#kr{vno=Vno+1}),
%	{S1,Rs1} = match1([U], Cs, #kfail{desc=compiler}, Sub0, RsN),
%	%io:format("S1=~p~n", [S1]),
%	kernel(#kwadler2{anno=[{l,L}],arg=Arg,var=U,body=S1}, Sub0, Rs1);

kernel(#kwadler{anno=A,body=E}, Sub0, Rs0) ->
	{E1,Rs1} = kernel(E, Sub0, Rs0),
	{#kwadler{anno=A,body=E1},Rs1};

%kernel(#kwadler2{anno=A,arg=Arg,var=U,body=E}, Sub0, Rs0) ->
%	{Arg1,Rs1} = kernel(Arg, Sub0#ks{tail=false}, Rs0),
%	{[U1],Sub1,Rs2} = new_vars([U], false, Sub0, Rs1),
%	{E1,Rs3} = kernel(E, Sub1, Rs2),
%	{#kwadler2{anno=A,arg=Arg1,var=U1,body=E1},Rs3};

kernel(#kreceive{anno=A,var=U,body=B,timeout=T,action=E}, Sub0, Rs0) ->
	{[U1],Sub1,Rs1} = new_vars([U], false, Sub0, Rs0),
	{B1,Rs2} = kernel(B, Sub1, Rs1),
	{T1,Rs3} = kernel(T, Sub1, Rs2),
	{E1,Rs4} = kernel(E, Sub1, Rs3),
	{#kreceive{anno=A,var=U1,body=B1,timeout=T1,action=E1},Rs4};

kernel(#kselect{anno=A,var=U,matches=Ms,otherwise=E}, Sub0, Rs0) ->
	{L,RsN} = new_label(Rs0),
	{U1,Rs1} = kernel(U, Sub0, RsN),
	{Ms1,Rs2} = mapfoldl(fun(M, Rs2) ->
		{_,Rs3} = kernel(U1, Sub0, Rs2),
		kernel(M, Sub0, Rs3)
	end, Rs1, Ms),
	{E1,Rs3} = kernel(E, Sub0, Rs2),
	{#kselect{anno=[{l,L}|A],var=U1,matches=Ms1,otherwise=E1},Rs3};

kernel(#kmatch{anno=A,class=Class,vars=Us,then=E}, Sub0, Rs0) ->
	{Vs,Sub1,RsN} = new_vars(Us, false, Sub0, Rs0),
	%io:format("Vs=~p~n", [Vs]),

	%% Class may have var expressions, kernel() then too
	{Class1,RsA} = case Class of
	  #kbinary{segments=Ss} ->
		{Ss0,RsB} = lists:mapfoldl(fun(#kseg{size=#c_var{}=Sz}=Seg, RsB) ->
			{Sz1,Rs} = kernel(Sz, Sub1, RsB),
			{Seg#kseg{size=Sz1},Rs};
		(Seg, RsB) ->
			{Seg,RsB}
		end, RsN, Ss),
		{#kbinary{segments=Ss0},RsB};
	  X ->
		{X,RsN}
	end,
	
	{E1,Rs1} = kernel(E, Sub1, RsA),
	{L,Rs2} = new_label(Rs1),
	{#kmatch{anno=[{l,L}|A],class=Class1,vars=Vs,then=E1},Rs2};

kernel(#kfail{}=E, _, Rs0) -> {E,Rs0};
kernel(#krecloop{}=E, _, Rs0) -> {E,Rs0};

kernel(#kbodyref{}=E, _, Rs0) -> {E,Rs0};

kernel(#kref{anno=A,body=E}, Sub0, Rs0) ->
	{E1,Rs1} = kernel(E, Sub0, Rs0),
	{#kref{anno=A,body=E1},Rs1};

kernel(#kswitch{anno=A,cases=Cs,otherwise=E}, Sub0, Rs0) ->
	{Cs1,Rs1} = mapfoldl(fun(C, Rs1) ->
		kernel(C, Sub0, Rs1)
	end, Rs0, Cs),
	{E1,Rs2} = kernel(E, Sub0, Rs1),
	{#kswitch{anno=A,cases=Cs1,otherwise=E1},Rs2};

kernel(#kcase{anno=A,binds=Bs,guard=G,body=E}, #ks{subs=Ss}=Sub0, Rs0) ->
    {L,Rs1} = new_label(Rs0),

	{Bs1,RsN} = mapfoldl(fun({From,To}, Rs2) ->
		{To1,Rs3} = kernel(To, Sub0, Rs2),
		{{From,To1},Rs3}
	end, Rs1, Bs),
    
    %% Old name may be already on the substitution list
    Ss1 = foldl(fun({#c_var{name=O},#c_var{name=N}}, Ss0) ->
		lists:keystore(O, 1, Ss0, {O,N})
	end, Ss, Bs1),
    
    {G1,Rs2} = kernel(G, Sub0#ks{subs=Ss1,tail=false}, RsN),
    {E1,Rs3} = kernel(E, Sub0#ks{subs=Ss1}, Rs2),
    {#kcase{anno=[{l,L}|A],binds=[],guard=G1,body=E1},Rs3};

kernel(#c_var{anno=A,name=V}, Sub0, Rs0) ->
	%io:format("valueof=~w~n", [V]),
    V1 =
      case keysearch(V, 1, Sub0#ks.subs) of
      {value,{_,K}} -> K;
      false -> V
      end,
    Rs1 =
      case member(V1, Sub0#ks.vars) of
      false ->
        case member(V1, Rs0#kr.outer) of
        false ->
            Rs0#kr{outer=Rs0#kr.outer++[V1]};
        true ->
            Rs0
        end;
      true ->
        Rs0
      end,
    {N2,Rs2} = new_label(Rs1),
    Life =
      case keysearch(V1, 1, Rs1#kr.life) of
      {value,{_,N1,_}} ->
        keyreplace(V1, 1, Rs1#kr.life, {V1,N1,N2});
      false ->
        [{V1,ice,N2}|Rs1#kr.life]
      end,
    {#c_var{anno=[{l,N2}|A],name=V1},Rs2#kr{life=Life}};

kernel(#c_binary{segments=Ss}=Code0, Sub0, Rs0) ->
    {Ss1,Rs1} = mapfoldl(fun(S, Rs) ->
            kernel(S, Sub0, Rs)
        end, Rs0, Ss),
    {Code0#c_binary{segments=Ss1},Rs1};

kernel(#c_bitstr{val=E,size=Sz}=Code0, Sub0, Rs0) ->
    {E1,Rs1} = kernel(E, Sub0, Rs0),
    {Sz1,Rs2} = kernel(Sz, Sub0, Rs1),
    {Code0#c_bitstr{val=E1,size=Sz1},Rs2};

kernel(#c_cons{hd=H,tl=T}=Code0, Sub0, Rs0) ->
    {H1,Rs1} = kernel(H, Sub0, Rs0),
    {T1,Rs2} = kernel(T, Sub0, Rs1),
    {Code0#c_cons{hd=H1,tl=T1},Rs2};

kernel(#c_tuple{es=Es}=Code0, Sub0, Rs0) ->
    {Es1,Rs1} = mapfoldl(fun(E, Rs) ->
            kernel(E, Sub0, Rs)
        end, Rs0, Es),
    {Code0#c_tuple{es=Es1},Rs1};

kernel(#c_values{es=Es}=Code0, Sub0, Rs0) ->
    {Es1,Rs1} = mapfoldl(fun(E, Rs) ->
            kernel(E, Sub0, Rs)
        end, Rs0, Es),
    {Code0#c_values{es=Es1},Rs1};

kernel(#c_seq{arg=F,body=S}=Code0, Sub0, Rs0) ->
    {F1,Rs1} = kernel(F, Sub0#ks{tail=false}, Rs0),
    {S1,Rs2} = kernel(S, Sub0, Rs1),
    {Code0#c_seq{arg=F1,body=S1},Rs2};

kernel(#c_apply{op=#c_fname{id=F,arity=N},args=As}, Sub0, Rs0) ->
	%% TODO: function may have renamed (lc$^1-23, etc)
	L = {F,N},
	Code0 =
    case keysearch(L, 1, Sub0#ks.defs) of
    {value,{_,Outer,F1}} ->
        Vs = [#c_var{name=V} || V <- Outer],
        #c_call{module=#c_literal{val=Sub0#ks.mod},
            name=#c_literal{val=F1},
            args=As++Vs};
    false ->
        #c_call{module=#c_literal{val=Sub0#ks.mod},
            name=#c_literal{val=F},args=As}
    end,
	kernel(Code0, Sub0, Rs0#kr{calls=[L|Rs0#kr.calls]});

%% dynamic c_apply -- convert to erlang:apply/3
kernel(#c_apply{op=F,args=As}, Sub0, Rs0) ->
    Cons = foldl(fun(A, C) ->
            #c_cons{hd=A,tl=C}
        end, #c_literal{val=[]}, reverse(As)),
    Code0 = #c_call{module=#c_literal{val=erlang},
            name=#c_literal{val=apply},args=[F,Cons]},
    kernel(Code0, Sub0, Rs0);

kernel(#c_primop{args=As}=Code0, Sub0, Rs0) ->
    {As1,Rs1} = mapfoldl(fun(A, Rs) ->
            kernel(A, Sub0, Rs)
        end, Rs0, As),
    {Code0#c_primop{args=As1},Rs1};

kernel(#c_catch{anno=A,body=E}=Code0, Sub0, Rs0) ->
	{L,RsN} = new_label(Rs0),
    {E1,Rs1} = kernel(E, Sub0#ks{tail=false}, RsN),
    {Code0#c_catch{anno=[{l,L}|A],body=E1},Rs1};

kernel(#c_call{anno=A,module=#c_literal{val=M},
	name=#c_literal{val=F},args=As}=Code0, Sub0, Rs0) ->

    {As1,Rs1} = mapfoldl(fun(Arg, Rs) ->
            kernel(Arg, Sub0, Rs)
        end, Rs0, As),
        	
	%%
	%% if call is tail optimized than arguments should
	%% not be cleared as they are assumed to be in place
	%%
	%% the current approach is to set the last use label
	%% to label of the call statement
	%%
	
	{L,Rs2} = new_label(Rs1),
	Life1 = if Sub0#ks.tail ->
		map(fun({_,{arg,_},_}=Lf) ->
			setelement(3, Lf, L);
		(Lf) ->
			Lf
		end, Rs2#kr.life);
	true -> %% tail is false
		Rs2#kr.life
	end,

    A1 = [{l,L},{tail,Sub0#ks.tail}|A],
    {Code0#c_call{anno=A1,module=#c_literal{val=M},
        name=#c_literal{val=F},args=As1},Rs2#kr{life=Life1}};

%% dynamic c_call -- convert to erlang:apply/3
kernel(#c_call{module=M,name=F,args=As}, Sub0, Rs0) ->
    Cons = foldl(fun(A, C) ->
            #c_cons{hd=A,tl=C}
        end, #c_literal{val=[]}, reverse(As)),
    Code0 = #c_call{module=#c_literal{val=erlang},
            name=#c_literal{val=apply},args=[M,F,Cons]},
    kernel(Code0, Sub0, Rs0);

kernel(#c_literal{}=Code0, _, Rs0) -> {Code0,Rs0}.

new_label(Rs0) ->
    N = Rs0#kr.next,
    {N,Rs0#kr{next=N+1}}.

new_vars([], _, Sub0, Rs0) -> {[],Sub0,Rs0};

new_vars(Vs, Args, Sub0, Rs0) ->
    %% io:format("new: ~p~n", [Vs]),
    As = [A || #c_var{name=A} <- Vs],
    New = As -- Rs0#kr.seen,
    Seen = As -- New,
    M = length(Seen),
    Ks =
      if
      M > 0 ->
        [list_to_atom("_ker" ++ integer_to_list(N))
            || N <- seq(Rs0#kr.vno, Rs0#kr.vno + M - 1)];
      true ->
        []
      end,
    Subs = zip(Seen, Ks),
    As1 = map(fun(A) ->
            case keysearch(A, 1, Subs) of
            {value,{_,K}} -> K;
            false -> A
            end
        end, As),
    N = length(As1),
    Bs = zip(As1, seq(Rs0#kr.next, Rs0#kr.next+N-1)),
    Life =
      case Args of
      false ->
        [{A,L,never} || {A,L} <- Bs];
      true ->
        zip3(As1,
          [{arg,K} || K <- seq(0, N-1)],
          duplicate(N, never))
    end,
    Vs1 = [#c_var{anno=[{l,L}],name=V}
                || {V,L} <- Bs],
    {Vs1,Sub0#ks{vars=As1++Sub0#ks.vars,
            subs=Subs++Sub0#ks.subs},
        Rs0#kr{seen=New++Rs0#kr.seen,
            vno=Rs0#kr.vno+M,
            next=Rs0#kr.next+N,
            life=Life++Rs0#kr.life}}.

%%
%%	Pattern-matching compilation
%%

match1(Us, Cs, E, Sub0, Rs0) ->
	Ks = [{exp_pats(Ps),#kcase{guard=G,body=B}} || #c_clause{pats=Ps,guard=G,body=B} <- Cs],
	match(Us, Ks, E, Sub0, Rs0).

%% TODO: adjust algo not to expand literals
%% needed for literal strings only

%%
%% Expand string literals inside alias patterns too
%%

exp_pats(Ps) -> exp_pats(Ps, []).
exp_pats([P|Ps], Qs) -> exp_pats(Ps, [exp_pat(P)|Qs]);
exp_pats([], Qs) -> reverse(Qs).

exp_pat(#c_literal{val=Ls}) when is_list(Ls) ->
	foldr(fun(N, Ls1) when is_integer(N) ->
		#c_cons{hd=#c_literal{val=N},tl=Ls1}
	end, #c_literal{val=[]}, Ls);
	
exp_pat(#c_alias{pat=#c_literal{val=Ls}}=Alias) when is_list(Ls) ->
	Pat1=foldr(fun(N, Ls1) when is_integer(N) ->
		#c_cons{hd=#c_literal{val=N},tl=Ls1}
	end, #c_literal{val=[]}, Ls),
	Alias#c_alias{pat=Pat1};
	
exp_pat(#c_literal{val=V}) when not is_number(V), not is_atom(V) ->
	erlang:error({unexpected_literal_pattern,V});
exp_pat(P) -> P.

match([], [], E, _, Rs0) ->
	{E,Rs0};
	
match([], Ks, E, _, Rs0) ->
	%io:format("Ks=~p~n", [Ks]),
	{#kswitch{cases=[C || {[],C} <- Ks],otherwise=E},Rs0};

match(Us, Ks, E, Sub0, Rs0) ->	
	Kss = multipart(fun({[#c_var{}|_],_}) ->
		true;
	(_) ->
		false
	end, Ks),

	foldr(fun([{[#c_var{}|_],_}|_]=Ks1, {E1,Rs1}) ->
		match_vars(Us, Ks1, E1, Sub0, Rs1);
	(Ks1, {E1,Rs1}) ->
		match_values(Us, Ks1, E1, Sub0, Rs1)
	end, {E,Rs0}, Kss).

match_bins([U|Us], Ks, E, Sub0, Rs0) ->
	%%	Ks = [{Ps,C}]
	%%	Ms = [{Class,Vs,[{Ps,C}]}]
	
	{Ms,_,Rs1} = foldr(fun({[#c_binary{segments=Ss}|Ps],C}, {Ms,Class0,#kr{vno=Vno}=Rs1}) ->
		%% Ss = [#c_bitstr{val,size,unit,type,flags}]
		
		{Ss0,RsN} = {Ss,Rs1},
		
		%% NB: Assigning labels to var expressions here
		%% does not follow the train of execution
		%% and leads to bad var life spans
		
		%%{Ss0,RsN} = lists:mapfoldl(fun(#c_bitstr{size=#c_var{}=Sz}=Seg, RsN) ->
		%%	{Sz1,Rs} = kernel(Sz, Sub0, RsN),
		%%	{Seg#c_bitstr{size=Sz1},Rs};
		%%(Seg, RsN) ->
		%%	{Seg,RsN}
		%%end, Rs1, Ss),
		
		{Es,Ss1} = lists:unzip(map(fun(#c_bitstr{val=Value,
			size=#c_var{}=Sz,
			unit=#c_literal{val=Unit},
			type=#c_literal{val=T},
			flags=#c_literal{val=Fs}}) ->
			
			{Value,bin_flags(#kseg{size=Sz,unit=Unit,type=T},Fs)};
		
		(#c_bitstr{val=Value,
			size=#c_literal{val=Sz},
			unit=#c_literal{val=Unit},
			type=#c_literal{val=T},
			flags=#c_literal{val=Fs}}) ->
			
			{Value,bin_flags(#kseg{size=Sz,unit=Unit,type=T},Fs)}
		end, Ss0)),
		
		Class = #kbinary{segments=Ss1},
		
		if Class =:= Class0 ->
			[{_,Vs,Ks1}|Ms0] = Ms,
			{[{Class,Vs,[{Es++Ps,C}|Ks1]}|Ms0],Class,RsN};

		true ->
			N = length(Es),
			{Vs,Rs2} = case N of
			0 ->
				{[],RsN};
			N ->
				As = [list_to_atom("_u" ++ integer_to_list(M)) || M <- seq(Vno, Vno+N-1)],
				{[#c_var{name=A} || A <- As],RsN#kr{vno=Vno+N}}
			end,
			M = {Class,Vs,[{Es++Ps,C}]},
			{[M|Ms],Class,Rs2}
		end		
	end, {[],undefined,Rs0}, Ks),
	
	foldr(fun({Class,Vs,Ks1}, {E1,Rs2}) ->
		{L,RsN} = new_label(Rs2),
		E2 = #kref{anno=[{l,L}],body=E1},
		{Then,Rs3} = match(Vs++Us, Ks1, #kbodyref{ref=L}, Sub0, RsN),
		Matches=[#kmatch{class=Class,vars=Vs,then=Then}],
		{#kselect{var=U,matches=Matches,otherwise=E2},Rs3}
	end, {E,Rs1}, Ms).

bin_flags(S, [unsigned|Fs]) -> bin_flags(S#kseg{signed=false}, Fs);
bin_flags(S, [signed|Fs]) -> bin_flags(S#kseg{signed=true}, Fs);
bin_flags(S, [big|Fs]) -> bin_flags(S#kseg{endian=big}, Fs);
bin_flags(S, [little|Fs]) -> bin_flags(S#kseg{endian=little}, Fs);
bin_flags(S, [native|Fs]) -> bin_flags(S#kseg{endian=native}, Fs);
bin_flags(S, []) -> S.

%% the first pattern is var for all clauses
match_vars([New|Us], Ks, E, Sub0, Rs0) ->
    Ks1 = map(fun({[Old|Ps],#kcase{binds=Bs}=K}) ->
        {Ps,K#kcase{binds=[{Old,New}|Bs]}}
    end, Ks),
    match(Us, Ks1, E, Sub0, Rs0).

%% the first pattern is non-var for all clauses
match_values([U|Us], Ks, E, Sub0, Rs0) ->

	%%
	%% values may be buried inside c_alias structure
	%%
	
    Ns = usort([N || {[#c_literal{val=N}|_],_} <- Ks] ++
			   [N || {[#c_alias{pat=#c_literal{val=N}}|_],_} <- Ks]),
    
    Nss = if length(Ns) >= ?SPLIT_MATCH_RANGE ->
		{Ls,Hs} = split(length(Ns) div 2, Ns),
		[Ls,Hs]; %% it is possible to split into more parts
    true ->
        []
    end,
    
    Ranges = [{hd(Xs),lists:last(Xs)} || Xs <- Nss],

    CKs = [match_class(U, P, Ps, Ranges, C) || {[P|Ps],C} <- Ks],

    CKs1 = [{Class, [K || {Class1,K} <- CKs, Class1 =:= Class]}
		||
    Class <- usort([Class || {Class,_} <- CKs])],

	{L,Rs1} = new_label(Rs0),
	E1 = #kref{anno=[{l,L}],body=E},

    {Matches,Rs2} = mapfoldl(fun({#kbinary0{}=Class,Ks1}, Rs2) ->
		{Then,Rs3} = match_bins([U|Us], Ks1, #kbodyref{ref=L}, Sub0, Rs2),
		{#kmatch{class=Class,vars=[],then=Then},Rs3};

    ({#krange{}=Class,Ks1}, Rs2) ->
		{Then,Rs4} = match([U|Us], Ks1, #kbodyref{ref=L}, Sub0, Rs2),
		{#kmatch{class=Class,vars=[],then=Then},Rs4};

    ({Class,Ks1}, Rs2) ->
        {Vs,Rs3} = make_class_vars(Class, Rs2),
		{Then,Rs4} = match(Vs++Us, Ks1, #kbodyref{ref=L}, Sub0, Rs3),
		{#kmatch{class=Class,vars=Vs,then=Then},Rs4}
    end, Rs1, CKs1),

    {#kselect{var=U,matches=Matches,otherwise=E1},Rs2}.

match_class(_, #c_literal{val=N}=P, Ps, Ranges, C) ->
    case match_range(N, Ranges) of
    {Min,Max} ->
        {#krange{min=Min,max=Max},{[P|Ps],C}};   %% stick c_literal pattern back
    false ->
        {#kliteral{val=N},{Ps,C}}
    end;
match_class(_, #c_cons{hd=H,tl=T}, Ps, _, C) ->
    {#kcons{},{[H,T]++Ps,C}};
match_class(_, #c_tuple{es=Es}, Ps, _, C) ->
    {#ktuple{arity=length(Es)},{Es++Ps,C}};
match_class(_, #c_binary{}=P, Ps, _, C) ->
    {#kbinary0{},{[P|Ps],C}};   %% stick c_binary pattern back
match_class(U, #c_alias{var=V,pat=#c_literal{val=N}=P}, Ps, Ranges, #kcase{binds=Bs}=C) ->
    C1 = C#kcase{binds=[{V,U}|Bs]},
    case match_range(N, Ranges) of
    {Min,Max} ->
        {#krange{min=Min,max=Max},{[P|Ps],C1}};   %% stick c_literal pattern back
    false ->
        {#kliteral{val=N},{Ps,C1}}
    end;
match_class(U, #c_alias{var=V,pat=#c_cons{hd=H,tl=T}}, Ps, _, #kcase{binds=Bs}=C) ->
    {#kcons{},{[H,T|Ps],C#kcase{binds=[{V,U}|Bs]}}};
match_class(U, #c_alias{var=V,pat=#c_tuple{es=Es}}, Ps, _, #kcase{binds=Bs}=C) ->
    {#ktuple{arity=length(Es)},{Es++Ps,C#kcase{binds=[{V,U}|Bs]}}};
match_class(U, #c_alias{var=V,pat=#c_binary{}=P}, Ps, _, #kcase{binds=Bs}=C) ->
    {#kbinary0{},{[P|Ps],C#kcase{binds=[{V,U}|Bs]}}}.

match_range(N, [{Min,_}|_]) when N < Min -> false;
match_range(N, [{Min,Max}=R|_]) when N >= Min, N =< Max -> R;
match_range(N, [_|Ranges]) -> match_range(N, Ranges);
match_range(_, []) -> false.

make_class_vars(#kcons{}, Rs0) -> make_some_vars(2, Rs0);
make_class_vars(#ktuple{arity=K}, Rs0) -> make_some_vars(K, Rs0);
make_class_vars(_, Rs0) -> {[],Rs0}.

make_some_vars(0, Rs0) -> {[],Rs0};
make_some_vars(K, #kr{vno=Vno}=Rs0) when K > 0 ->
    Vs = [list_to_atom("_u" ++ integer_to_list(M)) || M <- seq(Vno, Vno+K-1)],
	{[#c_var{name=V} || V <- Vs],Rs0#kr{vno=Vno+K}}.

%%
%%	Partition the list into sublists sequentially
%%	satisfying or not satisfying the predicate
%%
multipart(_, []) -> [];
multipart(Pred, [L|Ls]) -> multipart(Pred, Ls, Pred(L), [L], []).
multipart(Pred, [L|Ls], F, Ss, Os) ->
	case Pred(L) =:= F of
	true -> multipart(Pred, Ls, F, [L|Ss], Os);
	false -> multipart(Pred, Ls, Pred(L), [L], [reverse(Ss)|Os])
	end;
multipart(_, [], _, Ss, Os) -> reverse([reverse(Ss)|Os]).

%% EOF
