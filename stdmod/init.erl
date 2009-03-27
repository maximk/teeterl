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
-module(init).
-export([run/1]).

-export([launcher/1,trace/3]).

-import(queue, [is_empty/1,in/2,out/1]).
-import(lists, [foreach/2,member/2,foldl/3,map/2,delete/2,partition/2,keydelete/3,filter/2,flatten/1]).
-import(erlang, [port_info/2,ports/0]).

-define(NORMAL_ADVANTAGE, 8).
-define(REDUCTIONS, 10000).
-define(MAX_HEAP_SIZE, 4*1024*1024).

%% -define(TRACE, true).

-ifdef(TRACE).
-define(strace(T), T = now()).
-define(trace(T,Pid,What), trace(T, Pid, What)).
-else.
-define(strace(T), true).
-define(trace(T,Pid,What), true).
-endif.

-record(sc, {highq,	%% Pid
	normq,			%% Pid
	lowq,			%% Pid
	waits,			%% {Timeout,Pid,Priority}
	waits2,			%% {Pid,Prio}
	breaks,			%% {Pid,Prio}
	links,			%% {Pid,PidPort}
	norm_sched=0,	%% normal queue advantage
	services=[],
	monitors=[]}).	%% {BreakPid,NotifyPid}

run(Args) ->
	register(init, self()),

	Services = [{stdio,stdio,server,[]},
			    {code,code,server,[]},
			    %% {files,files,server,[]}, --deprecated
			    %% {fs2,files2,server,[]}, --was bad idea
			    {rpc,rpc,server,[state]}],
	Q0 = start_services(Services),
	
	%[M,F|As] = Args,	
	%Mod = list_to_atom(M),
	%Fun = list_to_atom(F),
	%Pid = code:spawn0(Mod, Fun, [As]),
	Pid = code:spawn0(init, launcher, [Args]),
	
	NormQ = queue:in(Pid, Q0),
	
	schedule(#sc{highq=queue:new(),
		normq=NormQ,
		lowq=queue:new(),
		waits=[],
		waits2=[],
		breaks=[],
		links=[],
		services=Services}).

reverse(L) -> reverse(L, []).
reverse([H|T], L) -> reverse(T, [H|L]);
reverse([], L) -> L.

start_services(Ss) -> start_services(Ss, queue:new()).
start_services([{RegAs,M,F,As}|Ss], Q) ->
	Pid = code:spawn0(M, F, As),
	register(RegAs, Pid),
	start_services(Ss, queue:in(Pid, Q));
start_services([], Q) -> Q.

stop_services(Ss) -> stop_services(Ss, false).
stop_services([{RegAs,_,_,_}|Ss], _) ->
	RegAs ! {stop,self()},
	stop_services(Ss, true);
stop_services([], Active) -> Active.

wakeup_on_mail(Ps) -> wakeup_on_mail(Ps, [], []).
wakeup_on_mail([{_,Pid,Prio}=P|Ps], Ws, Rs) ->
	case process_info(Pid, new_mail) of
	{_,false} ->
		wakeup_on_mail(Ps, [P|Ws], Rs);
	{_,true} ->
		process_flag(Pid, new_mail, false),
		wakeup_on_mail(Ps, Ws, [{Pid,Prio}|Rs])
	end;
wakeup_on_mail([], Ws, Rs) -> {reverse(Ws),reverse(Rs)}.

wakeup_on_mail2(Ps) -> wakeup_on_mail2(Ps, [], []).
wakeup_on_mail2([{Pid,_}=P|Ps], Ws, Rs) ->
	case process_info(Pid, new_mail) of
	{_,false} ->
		wakeup_on_mail2(Ps, [P|Ws], Rs);
	{_,true} ->
		process_flag(Pid, new_mail, false),
		wakeup_on_mail2(Ps, Ws, [P|Rs])
	end;
wakeup_on_mail2([], Ws, Rs) -> {reverse(Ws),reverse(Rs)}.

park_runnable(Pid, high, St0) ->
	St0#sc{highq=in(Pid, St0#sc.highq)};
park_runnable(Pid, normal, St0) ->
	St0#sc{normq=in(Pid, St0#sc.normq)};
park_runnable(Pid, low, St0) ->
	St0#sc{lowq=in(Pid, St0#sc.lowq)}.

park_runnables([{Pid,Prio}|Ps], St0) ->
	St1 = park_runnable(Pid, Prio, St0),
	park_runnables(Ps, St1);
park_runnables([], St0) ->
	St0.

select_runnable(St0) ->
	case out(St0#sc.highq) of
	  {{value,Pid},Q} ->
		{Pid,high,St0#sc{highq=Q}};
	  {empty,_} ->
	    if
	      St0#sc.norm_sched > ?NORMAL_ADVANTAGE ->
			case out(St0#sc.lowq) of
			  {{value,Pid},Q} ->
	            {Pid,low,St0#sc{lowq=Q,norm_sched=0}};
	          {empty,_} ->
	            case out(St0#sc.normq) of
	              {{value,Pid},Q} ->
	                {Pid,normal,St0#sc{normq=Q,norm_sched=0}};
	              {empty,_} ->
	                empty
	            end
	        end;
	      true ->
			case out(St0#sc.normq) of
			  {{value,Pid},Q} ->
	            {Pid,normal,St0#sc{normq=Q,norm_sched=St0#sc.norm_sched+1}};
	          {empty,_} ->
	            case out(St0#sc.lowq) of
	              {{value,Pid},Q} ->
	                {Pid,low,St0#sc{lowq=Q,norm_sched=St0#sc.norm_sched+1}};
	              {empty,_} ->
	                empty
	            end
	        end
	    end
	end.

requests(St0) ->
	receive
	{spawn,From,Spec} ->
		Child = case Spec of
		{M,F,As} -> code:spawn0(M, F, As);
		F -> code:spawn0(F)
		end,
		St1 = park_runnable(Child, normal, St0),
		From ! {spawned,Child},
		requests(St1);
	
	{spawn_link,From,Spec} ->
		Child = case Spec of
		{M,F,As} -> code:spawn0(M, F, As);
		F -> code:spawn0(F)
		end,
		St1 = park_runnable(Child, normal, St0),
		From ! {spawned,Child},
		requests(St1#sc{links=[{From,Child}|St1#sc.links]});

	{spawn_monitor,From,Spec} ->
		Child = case Spec of
		{M,F,As} -> code:spawn0(M, F, As);
		F -> code:spawn0(F)
		end,
		St1 = park_runnable(Child, normal, St0),
		Ref = make_ref(),
		From ! {spawned,Child,Ref},
		Monitors = [{Ref,process,Child,From}|St1#sc.monitors],		
		requests(St1#sc{monitors=Monitors});

	{link,From,Id} ->
		case member({From,Id}, St0#sc.links) orelse member({Id,From}, St0#sc.links) of
		true ->
			From ! ok,
			requests(St0);
		false ->
			From ! ok,
			requests(St0#sc{links=[{From,Id}|St0#sc.links]})
		end;
	
	{unlink,From,Id} ->
		From ! ok,
		Links0 = delete({From,Id}, St0#sc.links),
		Links1 = delete({Id,From}, Links0),
		requests(St0#sc{links=Links1});
	
	{exit,Pid,_,kill} ->
		St1 = erase_runnable(Pid, St0),
		St2 = process_exits(Pid, kill, St1),
		requests(St2);
	
	{exit,Port,_LinkPid,Reason} when is_port(Port) ->
		St1 = process_exits(Port, Reason, St0),
		requests(St1);

	{exit,Pid,LinkPid,Reason} ->
		try
			case process_info(Pid, trap_exit) of
			{_,true} ->
				Pid ! {'EXIT',LinkPid,Reason},
				requests(St0);
			{_,false} ->
				St1 = erase_runnable(Pid, St0),
				St2 = process_exits(Pid, Reason, St1),
				requests(St2)
			end
		catch
		_:_ ->
			%% Pid may have exited already
			erlang:display({already_exited,Pid,ignored}),
			requests(St0)
		end;
	
	{processes,From} ->	  %% account for breaks
		%erlang:display({processes_requested,From}),
		Qs = [{high,queue:to_list(St0#sc.highq)},
			  {normal,queue:to_list(St0#sc.normq)},
			  {low,queue:to_list(St0#sc.lowq)}],
		Ws = St0#sc.waits ++ [{forever,Pid,Prio} || {Pid,Prio} <- St0#sc.waits2],
		From ! {processes,self(),Qs,Ws},
		%% TODO: pids in BREAK state ignored
		requests(St0);
	
	{monitor,From,process,Pid} ->
		Ref = make_ref(),
		From ! {ok,Ref},
		Monitors = [{Ref,process,Pid,From}|St0#sc.monitors],
		requests(St0#sc{monitors=Monitors});

	{demonitor,From,Ref} ->
		From ! ok,
		Monitors = lists:keydelete(Ref, 1, St0#sc.monitors),
		requests(St0#sc{monitors=Monitors});

	%{continue,_From,Pid} ->
	%	case lists:keytake(Pid, 1, St0#sc.breaks) of
	%	{value,{_,Prio},Breaks} ->
	%		St1 = park_runnable(Pid, Prio, St0#sc{breaks=Breaks}),
	%		requests(St1);
	%	false ->
	%		requests(St0)
	%	end;
	
	{stopped,_} ->	  %% answers from services being stopped, ignore
		requests(St0);
		
	Req ->
		erlang:display({unexpected_request,Req}),
		requests(St0)

	after 0 ->
		St0
	end.

trash_burner(Pid) ->
	%erlang:display({trash_burn,Pid}),
	case process_info(Pid, heap_size) of
	%% allow for 25% slack, not to invoke gc many times before dying
	{heap_size,HeapSize} when HeapSize > ?MAX_HEAP_SIZE + ?MAX_HEAP_SIZE div 4 ->
		
		?strace(T4),
		erlang:garbage_collect(Pid),
		?trace(T4, Pid, gc),
		
		{heap_size,HeapSize1} = process_info(Pid, heap_size),
		if HeapSize1 > ?MAX_HEAP_SIZE ->
			{module,Module} = process_info(Pid, module),
			{offset,Offset} = process_info(Pid, offset),
			erlang:display({gc_jam,Pid,HeapSize1,{Module,Offset}}),
			jam;
		true ->
			%R = HeapSize-HeapSize1,
			%erlang:display({gc_report,Pid,R,R/HeapSize}),
			%erlang:display(process_info(Pid, registered_name)),
			%erlang:display(process_info(Pid, module)),
			%erlang:display(process_info(Pid, offset)),
			%erlang:display(process_info(Pid, heap_size)),
			ok
		end;
	_ ->
		ok
	end.

select_expired(_, #sc{waits=[]}) ->
	empty;
select_expired(Now, St0) ->
	select_expired1(Now, St0, St0#sc.waits, []).

select_expired1(Now, St0, [{T,Pid,Prio}|Ws], Exps) when Now >= T ->
	select_expired1(Now, St0, Ws, [{Pid,Prio}|Exps]);
select_expired1(Now, _, [{T,_,_}|_], []) ->
	{Mega1,Sec1,Micro1} = Now,
	{Mega2,Sec2,Micro2} = T,
	Millis = (Micro2-Micro1) div 1000 +
		(Sec2-Sec1)*1000 +
		(Mega2-Mega1)*1000*1000*1000,
	{gap,Millis};
select_expired1(_, St0, Ws, Exps) ->
	{expired,Exps,St0#sc{waits=Ws}}.

insert_by_timeout(Now, Pid, Prio, Ws) -> insert_by_timeout(Now, Pid, Prio, Ws, []).
insert_by_timeout(Now, Pid, Prio, [{Now0,_,_}=W|Ws], Hs) when Now0 < Now ->
	insert_by_timeout(Now, Pid, Prio, Ws, [W|Hs]);
insert_by_timeout(Now, Pid, Prio, Ws, Hs)->
	reverse(Hs) ++ [{Now,Pid,Prio}] ++ Ws.

schedule(St0) ->
	St1 = requests(St0),
	
	{Ws,Rs} = wakeup_on_mail(St1#sc.waits),
	St2 = park_runnables(Rs, St1#sc{waits=Ws}),
	{Ws2,Rs2} = wakeup_on_mail2(St2#sc.waits2),
	St3 = park_runnables(Rs2, St2#sc{waits2=Ws2}),

	trash_burner(self()),	%% gc jam ignored for init

	case select_runnable(St3) of
	empty ->
		case select_expired(now(), St3) of
		{expired,Exps,St4} ->
			schedule(park_runnables(Exps, St4));

		{gap,Gap} ->
			%erlang:display({gap,Gap}),
			
			?strace(T1),
			code:poll_ports(Gap*1000),	  %% poll_ports expects microseconds
			?trace(T1, init, gap_poll),
			
			schedule(St3);

		empty ->
			case erlang:ports() of
			[] ->
				case stop_services(St3#sc.services) of
				true ->	%% some services were stopped, continue
					schedule(St3#sc{services=[]});
				false -> %% no service has been stopped
					if St3#sc.waits2 =/= [] ->
						erlang:display({not_exited,St3#sc.waits2});
					true ->
						done
					end
				end;
			_ ->
				%% poll for infinity
				
				?strace(T2),
				code:poll_ports(100*1000*1000),
				?trace(T2, init, infinity_poll),
				
				schedule(St3)
			end
		end;

	{Pid,Prio,St4} ->
		%erlang:display({running,Pid}),	
		
		?strace(T3),
		Status = code:run_slice(Pid, ?REDUCTIONS),
		?trace(T3, Pid, run_slice),
		
		case Status of
		{'$DONE',R} ->
			%erlang:display({exited_normally,Pid,R}),
			
			St5 = process_exits(Pid, R, St4),
			code:poll_ports(0),
			schedule(St5);

		'$YIELD' ->
			%% collect garbage if needed
			case trash_burner(Pid) of
			jam ->
				St5 = process_exits(Pid, gc_jam, St4),
				code:poll_ports(0),
				schedule(St5);
			ok ->
				schedule(park_runnable(Pid, Prio, St4))
			end;
		
		{'$BREAK',{_Module,_Offset}} ->
			%% breakpoint reached
			Breaks = [{Pid,Prio}|St4#sc.breaks],
			
			%% NB: module and offset can be looked up using process_info/2
			%{messages,Mailbox} = process_info(Pid, messages),
			
			%% not get_stacktrace(Pid) because it interacts with code server
			%Strace = erlang:get_stacktrace0(Pid),
			%Locals = erlang:get_locals(Pid),
			
			%Monitors = send_notifications(Pid,
			%  {'BREAK',[{pid,Pid},
			%           {module,Module},
			%           {offset,Offset},
			%           {strace,Strace},
			%           {locals,Locals},
			%           {mailbox,Mailbox}]},
			%  St4#sc.monitors),
			
			code:poll_ports(0),
			schedule(St4#sc{breaks=Breaks});
			
		{'$WAIT',infinity} ->
			Waits2 = [{Pid,Prio}|St4#sc.waits2],
			code:poll_ports(0),
			schedule(St4#sc{waits2=Waits2});
		
		{'$WAIT',Now} ->
			%erlang:display({wait,Now}),
			Waits = insert_by_timeout(Now, Pid, Prio, St4#sc.waits),			
			code:poll_ports(0),
			schedule(St4#sc{waits=Waits});

		{exit,normal} ->
			%erlang:display({normal_exit,Pid}),
			%{module,Module} = process_info(Pid, module),
			%{offset,Offset} = process_info(Pid, offset),

			St5 = process_exits(Pid, normal, St4),
			
			%% process_exits propagates notifications
			%Monitors = send_notifications(Pid,
			%  {'EXIT',[{pid,Pid},
			%           {reason,normal}]},
			%  St5#sc.monitors),
			
			code:poll_ports(0),
			schedule(St5);
			
		X ->
			
			{module,Module} = process_info(Pid, module),
			{offset,Offset} = process_info(Pid, offset),
			%{messages,Mailbox} = process_info(Pid, messages),
			erlang:display({exit_on_failure,Pid,X,{Module,Offset}}),
			
			%% not get_stacktrace(Pid) because it interacts with code server
			%Strace = erlang:get_stacktrace0(Pid),
			%Locals = erlang:get_locals(Pid),
			
			St5 = process_exits(Pid, X, St4),
			
			%% process_exits propagates notifications
			%Monitors = send_notifications(Pid,
			%  {'EXIT',[{pid,Pid},
			%           {reason,X},
			%           {module,Module},
			%           {offset,Offset},
			%           {strace,Strace},
			%           {locals,Locals},
			%           {mailbox,Mailbox}]},
			%  St5#sc.monitors),

			code:poll_ports(0),
			schedule(St5)
		end
	end.

process_exits(Pid, Reason, St0) ->
	%erlang:display({process_exits,Pid,Reason}),

	{AffectedLinks,OtherLinks} = partition(fun({P1,P2})
		when P1 =:= Pid; P2 =:= Pid -> true; (_) -> false end, St0#sc.links),
	
	AffectedIds = map(fun({P1,P2})
		when P1 =:= Pid -> P2; ({P1,_}) -> P1 end, AffectedLinks),
		
	Monitors = send_notifications(Pid, Reason, St0#sc.monitors),

	foreach(fun(P) when is_pid(P) ->
		{init,node(P)} ! {exit,P,Pid,Reason}
	end, AffectedIds),
	
	if is_port(Pid) ->
		erlang:close_port(Pid);
	true ->
		%%
		%% Close all ports controlled by the exiting process
		%%

		OwnedPorts = filter(fun(Port) ->
			case port_info(Port, connected) of
			{connected,Owner} ->
				Owner =:= Pid;
			undefined ->
				false
			end
		end, ports()),
		%erlang:display({owned_ports,OwnedPorts,Pid,Reason}),
		
		foreach(fun(Port) ->
			init ! {exit,Port,Pid,Reason}
		end, OwnedPorts),
		
		code:destroy_process(Pid)
	end,
	
	St0#sc{links=OtherLinks,monitors=Monitors}.

%% the routine is needed because a process may exit
%% on exit signal from linked process, the routine
%% is costly and is not called for processes exiting
%% for other reasons

erase_runnable(Pid, #sc{highq=H,normq=N,lowq=L}=St0) ->

	Hs = queue:to_list(H),
	Ns = queue:to_list(N),
	Ls = queue:to_list(L),
	
	Hs1 = delete(Pid, Hs),
	Ns1 = delete(Pid, Ns),
	Ls1 = delete(Pid, Ls),
	
	H1 = queue:from_list(Hs1),
	N1 = queue:from_list(Ns1),
	L1 = queue:from_list(Ls1),
	
	Ws = keydelete(Pid, 2, St0#sc.waits),
	Ws2 = keydelete(Pid, 1, St0#sc.waits2),
	Bs = keydelete(Pid, 1, St0#sc.breaks),
	
	St0#sc{highq=H1,normq=N1,lowq=L1,waits=Ws,waits2=Ws2,breaks=Bs}.	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% runs an function, recognizes import/export declarations
launcher(Args) ->
	{M,F,As} = options(Args),
	
	erlang:apply(M, F, [As]),

	done.

options([M,F|As]) ->
	{list_to_atom(M),
	 list_to_atom(F),
	 As}.

%% excludes monitoring processes that are not alive
send_notifications(Pid, Reason, Monitors) ->
	filter(fun({Ref,_,Pid1,NotifyMe}) when Pid1 =:= Pid ->
	  try
		NotifyMe ! {'DOWN', Ref, process, Pid, Reason},
		false	  %% remove the notification	   
	  catch	_:_ -> false end;
	  
	(_) -> true
	
	end, Monitors).

trace({Mega0,Sec0,Micro0}, Pid, What) ->
	{Mega1,Sec1,Micro1} = now(),

	%% Elapsed = (Mega1-Mega0) * 1000000000 + (Sec1-Sec0) * 1000 + (Micro1-Micro0) div 1000,
	Elapsed = (Mega1-Mega0) * 1000000000000 + (Sec1-Sec0) * 1000000 + (Micro1-Micro0),

	if Elapsed > 0 ->
		{module,Module} = process_info(Pid, module),
		{offset,Offset} = process_info(Pid, offset),
	
		erlang:display({trace,Pid,Module,Offset,Elapsed,What});
	true ->
		ok
	end.

%% EOF
