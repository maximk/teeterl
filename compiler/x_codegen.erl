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
-module(x_codegen).
-export([module/2]).

-import(lists, [map/2,keysearch/3,keyreplace/4,keysort/2]).
-import(lists, [filter/2,foldl/3,sort/2,usort/1,nth/2,split/2,partition/2]).
-import(lists, [takewhile/2,duplicate/2,seq/2,reverse/1,member/2]).

-include("core_parse.hrl").
-include("pat_match.hrl").

-record(cg, {mod,
	opts,
	vars=[],
	num_args,
	in_recv,
	success}).

module(#c_module{name=#c_literal{val=Mod},
		exports=Es,
		attrs=_As,
		defs=Ds}, Opts) ->
	%io:format("~p~n", [Ds]),
    Asm = map(fun(D) ->
            code(D, #cg{opts=Opts,mod=Mod})
        end, Ds),
    %io:format("exports: ~p~n", [Es]),
    Exps = [{F,N} || #c_fname{id=F,arity=N} <- Es],
    %Attrs = [{T,cons_to_list(L)} || {#c_literal{val=T},L} <- As], %%TODO
    Attrs=[],
    %io:format("~p~n", [Asm]),
    {ok,{Mod,Exps,Attrs,lists:flatten(Asm)}}.

%cons_to_list(Cs) -> cons_to_list(Cs, []).
%cons_to_list(#c_cons{hd=#c_literal{val=V},tl=T}, O) ->
%	cons_to_list(T, [V|O]);
%cons_to_list(#c_literal{val=[]}, O) -> reverse(O).

code({#c_fname{id=F,arity=N},#c_fun{anno=A2,body=E}}, St0) ->
	S = line(A2),
    Life = my_life(A2),
    Vars = alloc_vars(Life),
    %% io:format("Vars=~p~n", [Vars]),
    Locs = foldl(fun({_,{var,Slot,_}}, Max) when Slot >= Max ->
            Slot+1;
        (_, Max) ->
            Max
        end, 0, Vars),
    [
      {l,[],{F,N}},
      {enter,S,Locs},
      code(E, St0#cg{vars=Vars,num_args=N}),
      {leave,S},
      {ret,S,N}
    ];

code(#c_call{anno=A,module=#c_literal{val=erlang},
        name=#c_literal{val=is_integer},args=[E]}, St0) ->
    [
      code(E, St0),
      {is_integer,line(A)}
    ];

code(#c_call{anno=A,module=#c_literal{val=erlang},
        name=#c_literal{val=is_atom},args=[E]}, St0) ->
    [
      code(E, St0),
      {is_atom,line(A)}
    ];

code(#c_call{anno=A,module=#c_literal{val=erlang},
        name=#c_literal{val=is_boolean},args=[E]}, St0) ->
    [
      code(E, St0),
      {is_boolean,line(A)}
    ];

code(#c_call{anno=A,module=#c_literal{val=erlang},
        name=#c_literal{val=is_float},args=[E]}, St0) ->
    [
      code(E, St0),
      {is_float,line(A)}
    ];

code(#c_call{anno=A,module=#c_literal{val=erlang},
        name=#c_literal{val=is_function},args=[E]}, St0) ->
    [
      code(E, St0),
      {is_function,line(A)}
    ];

code(#c_call{anno=A,module=#c_literal{val=erlang},
        name=#c_literal{val=is_function},args=[E1,E2]}, St0) ->
    [
      code(E1, St0),
      code(E2, St0),
      {is_function2,line(A)}
    ];

code(#c_call{anno=A,module=#c_literal{val=erlang},
        name=#c_literal{val=is_number},args=[E]}, St0) ->
    [
      code(E, St0),
      {is_number,line(A)}
    ];

code(#c_call{anno=A,module=#c_literal{val=erlang},
        name=#c_literal{val=is_pid},args=[E]}, St0) ->
    [
      code(E, St0),
      {is_pid,line(A)}
    ];

code(#c_call{anno=A,module=#c_literal{val=erlang},
        name=#c_literal{val=is_port},args=[E]}, St0) ->
    [
      code(E, St0),
      {is_port,line(A)}
    ];

code(#c_call{anno=A,module=#c_literal{val=erlang},
        name=#c_literal{val=is_record},args=[E1,E2,E3]}, St0) ->
    [
      code(E1, St0),
      code(E2, St0),
      code(E3, St0),
      {is_record,line(A)}
    ];

code(#c_call{anno=A,module=#c_literal{val=erlang},
        name=#c_literal{val=is_reference},args=[E]}, St0) ->
    [
      code(E, St0),
      {is_reference,line(A)}
    ];

code(#c_call{anno=A,module=#c_literal{val=erlang},
        name=#c_literal{val=is_list},args=[E]}, St0) ->
    [
      code(E, St0),
      {is_list,line(A)}
    ];

code(#c_call{anno=A,module=#c_literal{val=erlang},
        name=#c_literal{val=is_tuple},args=[E]}, St0) ->
    [
      code(E, St0),
      {is_tuple,line(A)}
    ];

code(#c_call{anno=A,module=#c_literal{val=erlang},
        name=#c_literal{val=is_binary},args=[E]}, St0) ->
    [
      code(E, St0),
      {is_binary,line(A)}
    ];

code(#c_call{anno=A,module=#c_literal{val=erlang},
        name=#c_literal{val='not'},args=[E]}, St0) ->
    [
      code(E, St0),
      {'not',line(A)}
    ];

code(#c_call{anno=A,module=#c_literal{val=erlang},
        name=#c_literal{val='bnot'},args=[E]}, St0) ->
    [
      code(E, St0),
      {'bnot',line(A)}
    ];

code(#c_call{anno=A,module=#c_literal{val=erlang},
        name=#c_literal{val='++'},args=[E1,E2]}, St0) ->
    [
      code(E1, St0),
      {list_copy,line(A)},
      code(E2, St0),
      {list_append,line(A)}
    ];

code(#c_call{anno=A,module=#c_literal{val=erlang},
        name=#c_literal{val='--'},args=[E1,E2]}, St0) ->
    [
      code(E1, St0),
      code(E2, St0),
      {list_subtract,line(A)}
    ];

code(#c_call{anno=A,module=#c_literal{val=erlang},
        name=#c_literal{val='+'},args=[E1,#c_literal{val=N}]}, St0)
            when integer(N), N =< 16#7fffffff, N >= -16#80000000 ->
    [
      code(E1, St0),
      {addi,line(A),N}
    ];

code(#c_call{anno=A,module=#c_literal{val=erlang},
        name=#c_literal{val='+'},args=[#c_literal{val=N},E1]}, St0)
            when integer(N) ->
    [
      code(E1, St0),
      {addi,line(A),N}
    ];

code(#c_call{anno=A,module=#c_literal{val=erlang},
        name=#c_literal{val='+'},args=[E1,E2]}, St0) ->
    [
      code(E1, St0),
      code(E2, St0),
      {add,line(A)}
    ];

code(#c_call{anno=A,module=#c_literal{val=erlang},
        name=#c_literal{val='-'},args=[E1,#c_literal{val=N}]}, St0)
            when integer(N), N =< 16#7fffffff, N >= -16#80000000 ->
    [
      code(E1, St0),
      {subi,line(A),N}
    ];

code(#c_call{anno=A,module=#c_literal{val=erlang},
        name=#c_literal{val='-'},args=[#c_literal{val=N},E1]}, St0)
            when integer(N) ->
    [
      code(E1, St0),
      {subi,line(A),N},
      {negate,line(A)}
    ];

code(#c_call{anno=A,module=#c_literal{val=erlang},
        name=#c_literal{val='-'},args=[E1,E2]}, St0) ->
    [
      code(E1, St0),
      code(E2, St0),
      {sub,line(A)}
    ];

code(#c_call{anno=A,module=#c_literal{val=erlang},
        name=#c_literal{val='-'},args=[E]}, St0) ->
    [
      code(E, St0),
      {negate,line(A)}
    ];

code(#c_call{anno=A,module=#c_literal{val=erlang},
        name=#c_literal{val='*'},args=[E1,E2]}, St0) ->
    [
      code(E1, St0),
      code(E2, St0),
      {mult,line(A)}
    ];

code(#c_call{anno=A,module=#c_literal{val=erlang},
        name=#c_literal{val='/'},args=[E1,E2]}, St0) ->
    [
      code(E1, St0),
      code(E2, St0),
      {'div',line(A)}
    ];

code(#c_call{anno=A,module=#c_literal{val=erlang},
        name=#c_literal{val='div'},args=[E1,E2]}, St0) ->
    [
      code(E1, St0),
      code(E2, St0),
      {idiv,line(A)}
    ];

code(#c_call{anno=A,module=#c_literal{val=erlang},
        name=#c_literal{val='rem'},args=[E1,E2]}, St0) ->
    [
      code(E1, St0),
      code(E2, St0),
      {'rem',line(A)}
    ];

code(#c_call{anno=A,module=#c_literal{val=erlang},
        name=#c_literal{val='and'},args=[E1,E2]}, St0) ->
    [
      code(E1, St0),
      code(E2, St0),
      {'and',line(A)}
    ];

code(#c_call{anno=A,module=#c_literal{val=erlang},
        name=#c_literal{val='band'},args=[E1,E2]}, St0) ->
    [
      code(E1, St0),
      code(E2, St0),
      {'band',line(A)}
    ];

code(#c_call{anno=A,module=#c_literal{val=erlang},
        name=#c_literal{val='or'},args=[E1,E2]}, St0) ->
    [
      code(E1, St0),
      code(E2, St0),
      {'or',line(A)}
    ];

code(#c_call{anno=A,module=#c_literal{val=erlang},
        name=#c_literal{val='bor'},args=[E1,E2]}, St0) ->
    [
      code(E1, St0),
      code(E2, St0),
      {'bor',line(A)}
    ];

code(#c_call{anno=A,module=#c_literal{val=erlang},
        name=#c_literal{val='xor'},args=[E1,E2]}, St0) ->
    [
      code(E1, St0),
      code(E2, St0),
      {'xor',line(A)}
    ];

code(#c_call{anno=A,module=#c_literal{val=erlang},
        name=#c_literal{val='bxor'},args=[E1,E2]}, St0) ->
    [
      code(E1, St0),
      code(E2, St0),
      {'bxor',line(A)}
    ];

code(#c_call{anno=A,module=#c_literal{val=erlang},
        name=#c_literal{val='bsl'},args=[E1,E2]}, St0) ->
    [
      code(E1, St0),
      code(E2, St0),
      {'bsl',line(A)}
    ];

code(#c_call{anno=A,module=#c_literal{val=erlang},
        name=#c_literal{val='bsr'},args=[E1,E2]}, St0) ->
    [
      code(E1, St0),
      code(E2, St0),
      {'bsr',line(A)}
    ];

code(#c_call{anno=A,module=#c_literal{val=erlang},
        name=#c_literal{val='<'},args=[E1,E2]}, St0) ->
    [
      code(E1, St0),
      code(E2, St0),
      {less,line(A)}
    ];

code(#c_call{anno=A,module=#c_literal{val=erlang},
        name=#c_literal{val='=<'},args=[E1,E2]}, St0) ->
    [
      code(E1, St0),
      code(E2, St0),
      {lesseq,line(A)}
    ];

code(#c_call{anno=A,module=#c_literal{val=erlang},
        name=#c_literal{val='>'},args=[E1,E2]}, St0) ->
    [
      code(E1, St0),
      code(E2, St0),
      {more,line(A)}
    ];

code(#c_call{anno=A,module=#c_literal{val=erlang},
        name=#c_literal{val='>='},args=[E1,E2]}, St0) ->
    [
      code(E1, St0),
      code(E2, St0),
      {moreeq,line(A)}
    ];

code(#c_call{anno=A,module=#c_literal{val=erlang},
        name=#c_literal{val='=='},args=[E1,E2]}, St0) ->
    [
      code(E1, St0),
      code(E2, St0),
      {equal,line(A)}
    ];

code(#c_call{anno=A,module=#c_literal{val=erlang},
        name=#c_literal{val='/='},args=[E1,E2]}, St0) ->
    [
      code(E1, St0),
      code(E2, St0),
      {neq,line(A)}
    ];

code(#c_call{anno=A,module=#c_literal{val=erlang},
        name=#c_literal{val='=:='},args=[E1,E2]}, St0) ->
    [
      code(E1, St0),
      code(E2, St0),
      {exeq,line(A)}
    ];

code(#c_call{anno=A,module=#c_literal{val=erlang},
        name=#c_literal{val='=/='},args=[E1,E2]}, St0) ->
    [
      code(E1, St0),
      code(E2, St0),
      {nexeq,line(A)}
    ];

code(#c_call{module=#c_literal{val=erlang},
        name=#c_literal{val='!'}}=C, St0) ->
    code(C#c_call{module=#c_literal{val=prim_erlang}}, St0);

code(#c_call{anno=A,module=#c_literal{val=erlang},
        name=#c_literal{val=error},args=[E]}, St0) ->
    S = line(A),
    [
      {lit,S,error},
	  code(E, St0),
      {raise,S}
    ];

code(#c_call{anno=A,module=#c_literal{val=erlang},
        name=#c_literal{val=error},args=[E1,E2]}, St0) ->
    S = line(A),
    [
      {lit,S,error},
	  code(E1, St0),
	  code(E2, St0),
	  {pack_tuple,S,2},
      {raise,S}
    ];

code(#c_call{anno=A,module=#c_literal{val=erlang},
        name=#c_literal{val=exit},args=[E]}, St0) ->
    S = line(A),
    [
      {lit,S,exit},
	  code(E, St0),
      {raise,S}
    ];

code(#c_call{anno=A,module=#c_literal{val=erlang},
        name=#c_literal{val=throw},args=[E]}, St0) ->
    S = line(A),
    [
      {lit,S,throw},
      code(E, St0),
      {raise,S}
    ];

code(#c_call{anno=A,module=#c_literal{val=erlang},
        name=#c_literal{val=self},args=[]}, _St0) ->
    [
      {self,line(A)}
    ];

code(#c_call{anno=A,module=#c_literal{val=erlang},
        name=#c_literal{val=apply},args=[E1,E2,E3]}, St0) ->
    [
	  code(E1, St0),
	  code(E2, St0),
	  code(E3, St0),
      {apply,line(A)}
    ];

code(#c_call{anno=A,module=#c_literal{val=erlang},
        name=#c_literal{val=apply},args=[F,As]}, St0) ->
    [
	  code(F, St0),
	  code(As, St0),
      {apply_fun,line(A)}
    ];

code(#c_call{anno=A,module=#c_literal{val=erlang},
        name=#c_literal{val=apply2},args=[E1,E2,E3]}, St0) ->
    [
	  code(E1, St0),
	  code(E2, St0),
	  code(E3, St0),
      {apply2,line(A)}
    ];

code(#c_call{anno=A,module=#c_literal{val=M},
        name=#c_literal{val=F},args=As}, St0) ->
    N = length(As),
    S = line(A),
    case bif:is_builtin(M, F, N) of
	  true ->
		%Index = bif:bif_index(M, F, N),
		[
		  [code(X, St0) || X <- As],
		  {bif_call(N),S,{M,F,N}}
		];
      false ->
		[
		  if
			St0#cg.mod =:= M ->
			case my_tail(A) of
			  true when St0#cg.num_args =:= N ->
				[
				  set_args(As, St0),
				  {tail_call,S,{F,N}}
				];
			  _ ->
				[
				  [code(X, St0) || X <- As],
				  {push_args,S,N},
				  {call,S,{F,N}}
				]
			end;
		  true ->
			case my_tail(A) of
			  true when St0#cg.num_args =:= N ->
			    [
			      set_args(As, St0),
			      {tail_call_far,S,{M,F,N}}
			    ];
			  _ ->
			    [
			      [code(X, St0) || X <- As],
			      {push_args,S,N},
			      {call_far,S,{M,F,N}}
			    ]
			end
		  end
		]
	end;

code(#c_values{es=Es}, St0) ->
    map(fun(E) -> code(E, St0) end, Es);

code(#c_seq{anno=N,arg=A,body=B}, St0) ->
    [
      code(A, St0),
      {drop,line(N),1},
      code(B, St0)
    ];

code(#c_let{vars=Vs,arg=A,body=B}, St0) ->
    [
      code(A, St0),
      map(fun(#c_var{name=V}) ->
		save_var(V, St0)
	  end, reverse(Vs)),
      code(B, St0)
    ];

code(#c_cons{anno=A}=E, St0) ->
    case prefix(E) of
      {[],#c_cons{hd=H,tl=#c_literal{val=[]}}} ->
        [
          code(H, St0),
          {make_cons_nil,line(A)}
        ];
      {[],#c_cons{hd=H,tl=T}} ->
        [
          code(H, St0),
          code(T, St0),
          {make_cons,line(A)}
        ];
      {Chars,#c_literal{val=[]}} ->
        [
          {lit,[],Chars}
        ];
      {Chars,Rest} ->
        [
          {lit,line(A),Chars},
          {list_copy,line(A)},
          code(Rest, St0),
          {list_append,line(A)}
        ]
    end;

code(#c_tuple{anno=A,es=Es}, St0) ->
    [
      [code(E, St0) || E <- Es],
      {pack_tuple,line(A),length(Es)}
    ];

code(#c_var{anno=A,name=V}, St0) ->
    L = my_label(A),
	S = line(A),
    case where(V, St0) of
      {arg,N,L} -> {clear_arg,[{name,V}|S],N};
      {var,N,L} -> {clear_var,[{name,V}|S],N};
      {arg,N,_} -> {get_arg,[{name,V}|S],N};
      {var,N,_} -> {get_var,[{name,V}|S],N};
      void -> [{lit,S,error},{lit,S,void},{raise,S}]
    end;

code(#c_catch{anno=A,body=E}, St0) ->
    L = my_label(A),
    S = line(A),
    [
      {'catch',S,L},
      code(E, St0),
      {drop_catch,S},
      {jump,S,{L,continue}},
      {l,[],L},
      
      %% converts 'error' and 'exit' to 'EXIT', etc
      {push_args,S,2},
      {call_far,S,{prim_erlang,catch0,2}},

      {l,[],{L,continue}}
    ];

code(#c_try{anno=A,arg=E,vars=Vs,body=B,evars=[Evar1,Evar2],handler=H}, St0) ->
    L = my_label(A),
    S = line(A),
    [
      {'catch',S,L},
      code(E, St0),
      map(fun(#c_var{name=V}) -> save_var(V, St0) end, Vs),
      code(B, St0),
      {drop_catch,S},
      {jump,S,{L,ok}},
      {l,[],L},
      
      %% (class reason)
      
      {swap,S},
      {dup,S},
      save_var1(Evar1, St0),
      {swap,S},
      
      %% (class reason)
      
      %% extracts reason atom only:
      %% function_clause -> function_clause
      %% {function_clause,_} -> function_clause
      
      {push_args,S,2},
      {call_far,S,{prim_erlang,catch1,2}},
      save_var1(Evar2, St0),
      
      code(H, St0),
      {l,[],{L,ok}}
    ];

code(#c_try{anno=A,arg=E,vars=Vs,body=B,evars=[Evar1,Evar2,Evar3],handler=H}, St0) ->
    L = my_label(A),
    S = line(A),
    [
      {'catch',S,L},
      code(E, St0),
      map(fun(#c_var{name=V}) -> save_var(V, St0) end, Vs),
      code(B, St0),
      {drop_catch,S},
      {jump,S,{L,ok}},
      {l,[],L},

      %% (class reason)
      
      {swap,S},
      {dup,S},
      save_var1(Evar1, St0),
      {swap,S},
      
      %% (class reason)

      %% extracts reason atom only:
      %% function_clause -> function_clause
      %% {function_clause,_} -> function_clause

      {push_args,S,2},
      {call_far,S,{prim_erlang,catch1,2}},
      {dup,S},
      save_var1(Evar3, St0),
      save_var1(Evar2, St0),

      code(H, St0),
      {l,[],{L,ok}}
    ];

code(#kwadler{anno=A,body=E}, St0) ->
	L = my_label(A),
	S = line(A),
	[
	  code(E, St0#cg{in_recv=false,success=L}),
	  {lit,S,error},
	  {lit,S,compiler},
	  {raise,S},
	  {l,[],L}
	];

%code(#kwadler2{anno=A,arg=Arg,var=U,body=E}, St0) ->
%	L = my_label(A),
%	[
%	  code(Arg, St0),
%	  save_var1(U, St0),
%	  code(E, St0#cg{in_recv=false,success=L}),
%	  {l,[],L}
%	];

code(#kreceive{anno=A,var=U,body=B,timeout=T,action=E}, St0) ->
	L = my_label(A),
	S = line(A),
	[
	  code(T, St0),
	  {reset_msgs,S},
	  {l,[],{L,loop}},
	  {get_msg,S,{L,timeout}},
	  save_var1(U, St0),
	  code(B, St0#cg{in_recv=true,success=L}),
	  {drop,S,1},
	  {jump,S,{L,loop}},
	  {l,[],{L,timeout}},
	  code(E, St0),
	  {l,[],L}
	];

code(#kselect{anno=_A,var=U,matches=Ms,otherwise=E}, St0) ->
	[
	  map(fun(#kmatch{anno=A1,class=Class,vars=Vs,then=Then}) ->
		L = my_label(A1),
		S = line(A1),
		
		case Class of
		#krange{max=Max} ->	%% ranges guaranteed to be in ascending order
			[
			  code(U, St0),
			  {lesseq_than,S,Max},
			  {jump_if_not,S,L},
			  code(Then, St0),
			  {drop,S,1},
			  {l,[],L}
			];
		#kliteral{val=N} ->
			[
			  code(U, St0),
			  {equal_to,S,N},
			  {jump_if_not,S,L},
			  code(Then, St0),
			  {drop,S,1},
			  {l,[],L}
			];
		#kcons{} ->
			[H,T] = Vs,
			[
			  code(U, St0),
			  {dup,S},
			  {is_cons,S},
			  {jump_if_not,S,L},
			  {dup,S},
			  {car,S},
			  save_var1(H, St0),
			  {cdr,S},
			  save_var1(T, St0),
			  code(Then, St0),
			  {l,[],L},
			  {drop,S,1}
			];
		#ktuple{arity=0} ->
			[
			  code(U, St0),
			  {is_tuple_of_arity,S,0},
			  {jump_if_not,S,L},
			  code(Then, St0),
			  {drop,S,1},
			  {l,[],L}
			];
		#ktuple{arity=K} ->
			[
			  code(U, St0),
			  {dup,S},
			  {is_tuple_of_arity,S,K},
			  {jump_if_not,S,L},
			  {unpack_tuple,S,K},
			  [save_var1(V, St0) || V <- reverse(Vs)],
			  code(Then, St0),
			  {l,[],L},
			  {drop,S,1}
			];
		#kbinary0{} ->
			[
			  code(U, St0),
			  {is_binary,S},
			  {jump_if_not,S,L},
			  code(Then, St0),
  			  {drop,S,1},
			  {l,[],L}
			];
        #kbinary{segments=Ss} ->
			{N,VarSizes,Op} = foldl(fun(#kseg{size=all}, {N,VarSizes,_}) ->
				{N,VarSizes,bin_size_is_at_least};
			(#kseg{size=Sz,unit=Unit}, {N,VarSizes,Op}) when is_integer(Sz) ->
				{N+Sz*Unit,VarSizes,Op};
			(#kseg{size=Variable,unit=1}, {N,VarSizes,Op}) ->
				{N,[Variable|VarSizes],Op};
			(#kseg{size=Variable,unit=Unit}, {N,VarSizes,Op}) ->
				{N,[{Variable,Unit}|VarSizes],Op}
			end, {0,[],bin_size_is}, Ss),
			[
				code(U, St0),
				{lit,S,N},
				map(fun({Variable,Unit}) ->
				[
					%% BUG: clear_var could be emitted,
					%% while the value is needed farther down --> $1
					
					%% FIX: suppress label information in the statement
					%% and in the similar case below
					
					code(Variable#c_var{anno=[]}, St0),		
					{lit,S,Unit},
					{mult,S},
					{add,S}
				];
				(Variable) ->
				[
					code(Variable#c_var{anno=[]}, St0),
					{add,S}
				]
				end, VarSizes),
				{Op,S},
				{jump_if_not,S,L},
				code(U, St0),
				{bin_fetch_start,S},
				map2(fun(#kseg{size=Sz}=Seg, V) ->
					[
						case Sz of
						all ->
							{lit,S,all};
						Sz when is_integer(Sz) ->
							{lit,S,Sz};
						Variable ->
							code(Variable, St0)		%% $1 see above
						end,
						fetch_op(Seg),
						save_var1(V, St0)
					]
				end, Ss, Vs),
				{is_bin_consumed,S},
				{jump_if_not,S,L},
				code(Then, St0),
				{drop,S,1},
				{l,[],L}
			]
		end
	  end, Ms),
	  code(E, St0)
	];

code(#kfail{anno=A,desc=D}, _St0) ->
	S = line(A),
	[
	  {lit,S,error},
	  {lit,S,D},
	  {raise,S}
	];

code(#krecloop{anno=A}, _) ->
	{lit,line(A),loop};

code(#kbodyref{anno=A,ref=Ref}, _St0) ->
	{jump,line(A),Ref};

code(#kswitch{cases=Cs,otherwise=Otherwise}, St0) ->
	Cs1 = until_true(Cs),
	[
		map(fun(#kcase{anno=A1,binds=Bs,guard=#c_literal{val=true},body=E}) ->
			S = line(A1),
			[
			  map(fun({Old,New}) ->
				[
				  code(New, St0),
				  save_var1(Old, St0)
				]
			  end, Bs),
			  if St0#cg.in_recv -> {drop_msg,S};
				  true -> [] end,
			  code(E, St0),
			  {jump,S,St0#cg.success}
			];
		(#kcase{anno=A1,binds=Bs,guard=G,body=E}) ->
			L = my_label(A1),
			S = line(A1),
			[
			  map(fun({Old,New}) ->
				[
				  code(New, St0),
				  save_var1(Old, St0)
				]
			  end, Bs),
			  code(G, St0),
			  {jump_if_not,S,L},
			  if St0#cg.in_recv -> {drop_msg,S};
				  true -> [] end,
			  code(E, St0),
			  {jump,S,St0#cg.success},
			  {l,[],L}
			]
		end, Cs1),
		code(Otherwise, St0)
	];

code(#kref{anno=A,body=E}, St0) ->
	[
	  {l,[],my_label(A)},
	  code(E, St0)
	];

code(#c_primop{anno=A,name=#c_literal{val=match_fail},args=[Value]}, St0) ->
	S = line(A),
    [
      {lit,S,error},
	  %% {lit,S,match},
      code(Value, St0),
      %% {pack_tuple,S,2},
      {raise,line(A)}
    ];

code(#c_primop{anno=A,name=#c_literal{val=raise},args=[Class,Desc]}, St0) ->
    [
      code(Class, St0),
      code(Desc, St0),
      {raise,line(A)}
    ];

code(#c_primop{anno=A,name=#c_literal{val=bs_context_to_binary},args=[_]}, _St0) ->
    [
      {bin_get_context,line(A)}
    ];

code(#c_primop{anno=_A,name=#c_literal{val=bs_init_writable}}, _St0) ->
    [
      %% do not know what it is, ignore
    ];

code(#c_literal{anno=A,val=V}, _St0) ->
	{lit,line(A),V};

code(#c_binary{anno=A,segments=Ss}, St0) ->
	S = line(A),
	[
	  {beg_bin,S},
	  map(fun(#c_bitstr{val=V,size=Sz,
		unit=#c_literal{val=U},type=#c_literal{val=T},flags=#c_literal{val=Fs}}) ->
		[
			code(V, St0),
			code(Sz, St0),
			bin_add(U, T, Fs)
		]
	  end, Ss),
	  {end_bin,S}
	].

set_args(As, St0) ->
	%% see how many args are there already
	{N,K} = foldl(fun(#c_var{name=V}, {N,K}) ->
		case where(V, St0) of
		{arg,N,_} -> {N+1,K+1};
		_ -> {N+1,K}
		end;
	(_, {N,K}) ->
		{N+1,K}
	end, {0,0}, As),
	%io:format("~w arg(s) (out of ~w) are there already~n", [K,N]),
	
	%% N - total args
	%% K - args in place
	
	if K > N div 3 -> %% enough is enough, nothing but vars
	
		%% first calculate all expressions, then set
		%% appropriate args, as expressions may depend
		%% on argument values
		
		AsN0 = lists:zip(As, seq(0, N-1)),
		
		AsN = filter(fun({#c_var{name=V}, M}) ->
		  case where(V, St0) of
		    {arg,M,_} -> false; %% skip
		    _ -> true
		  end;
		(_) ->
			true
		end, AsN0),
		
		[
		  [code(A, St0) || {A,_} <- AsN],
		  [{set_arg,[],M} || {_,M} <- reverse(AsN)]
		];
	true ->
		[
		  [code(A, St0) || A <- As],
		  {set_args,[],N}
		]
	end.

bin_add(U, integer, [_,big]) -> {bin_add_i_b,[],U};
bin_add(U, integer, [_,little]) -> {bin_add_i_l,[],U};
bin_add(U, integer, [_,native]) -> {bin_add_i_n,[],U};
bin_add(U, float, [_,big]) -> {bin_add_f_b,[],U};
bin_add(U, float, [_,little]) -> {bin_add_f_l,[],U};
bin_add(U, float, [_,native]) -> {bin_add_f_n,[],U};
bin_add(U, binary, _) -> {bin_add_b,[],U}.

fetch_op(#kseg{unit=U,type=integer,signed=false,endian=big}) ->
	{bin_fetch_i_u_b,[],U};
fetch_op(#kseg{unit=U,type=integer,signed=false,endian=little}) ->
	{bin_fetch_i_u_l,[],U};
fetch_op(#kseg{unit=U,type=integer,signed=false,endian=native}) ->
	{bin_fetch_i_u_n,[],U};
fetch_op(#kseg{unit=U,type=integer,signed=true,endian=big}) ->
	{bin_fetch_i_s_b,[],U};
fetch_op(#kseg{unit=U,type=integer,signed=true,endian=little}) ->
	{bin_fetch_i_s_l,[],U};
fetch_op(#kseg{unit=U,type=integer,signed=true,endian=native}) ->
	{bin_fetch_i_s_n,[],U};
fetch_op(#kseg{unit=U,type=float,endian=big}) ->
	{bin_fetch_f_b,[],U};
fetch_op(#kseg{unit=U,type=float,endian=little}) ->
	{bin_fetch_f_l,[],U};
fetch_op(#kseg{unit=U,type=float,endian=native}) ->
	{bin_fetch_f_n,[],U};
fetch_op(#kseg{unit=U,type=binary}) ->
	{bin_fetch_bin,[],U}.	  

until_true(Cs) -> until_true(Cs, []).
until_true([#kcase{guard=#c_literal{val=true}}=C|_], Cs1) ->
	reverse([C|Cs1]);
until_true([C|Cs], Cs1) ->
	until_true(Cs, [C|Cs1]);
until_true([], Cs1) ->
	reverse(Cs1).

bif_call(0) -> bif_call0;
bif_call(1) -> bif_call1;
bif_call(2) -> bif_call2;
bif_call(3) -> bif_call3;
bif_call(4) -> bif_call4.

%%
%%  String prefix of cons expression
%%

prefix(E) -> prefix(E, []).

prefix(#c_cons{hd=#c_literal{val=X},tl=T}, S)
        when integer(X), X >= 0, X < 256 ->
    prefix(T, [X|S]);
prefix(E, S) -> {reverse(S),E}.

alloc_vars(Life) ->
    alloc_vars(keysort(2, Life), [], []).

alloc_vars([{V,{arg,N},Died}|Vs], Slots, Alloc) ->
    alloc_vars(Vs, Slots, [{V,{arg,N,Died}}|Alloc]);

alloc_vars([{V,_,never}|Vs], Slots, Alloc) ->
    alloc_vars(Vs, Slots, [{V,void}|Alloc]);

alloc_vars([{V,Born,Died}|Vs], Slots, Alloc) ->
    N = length(Slots),
    Slot = foldl(fun({S,D}, Avail) when D =< Born, S < Avail ->
            S;
        (_, Avail) ->
            Avail
        end, N, Slots),
    Slots1 =
    if
      Slot =:= N ->
        [{N,Died}|Slots];
      true ->
        keyreplace(Slot, 1, Slots, {Slot,Died})
    end,
    %% io:format("Slots1=~p~n", [Slots1]),
    alloc_vars(Vs, Slots1, [{V,{var,Slot,Died}}|Alloc]);

alloc_vars([], _, Alloc) -> Alloc.

my_label(A) ->
    case keysearch(l, 1, A) of
    {value,{_,L}} -> L;
    false -> no_label
    end.

my_life(A) ->
    case keysearch(life, 1, A) of
    {value,{_,Life}} -> Life;
    false -> no_life
    end.

my_tail(A) ->
    case keysearch(tail, 1, A) of
    {value,{_,T}} -> T;
    false -> no_tail
    end.

line([]) -> [];
line([N|_]) when integer(N) -> [N];
line([_|L]) -> line(L).

where(V, St0) ->
    %{value,{_,Where}} = keysearch(V, 1, St0#cg.vars),
    %Where.
    case keysearch(V, 1, St0#cg.vars) of
    {value,{_,Where}} -> Where;
    _ -> code:croak({V,St0}), void
    end.

save_var(V, St0) ->
    case where(V, St0) of
      {arg,N,_} -> {set_arg,[],N};
      {var,N,_} -> {set_var,[],N};
      void -> {drop,[],1}
    end.
   
save_var1(#c_var{name=V}, St0) -> save_var(V, St0).

map2(F, [A|As], [B|Bs]) ->
	[F(A,B)|map2(F, As, Bs)];
map2(_, [], []) -> [].

%% EOF
