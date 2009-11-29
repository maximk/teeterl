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
-module(netmesh).

-record(nm,{
    handler,      %% fun(Bin) - incoming message processor
    endpoints,    %% [{Node,Mod,Spec}], e.g. {xxx,tcp,{host1,port1}}
    routes,       %% [{Node,Inter,Metric}], e.g. {x,abc,10}
    subsystems=[] %% [Mod], e.g. {tcp,<muse.102.0>}
}).

-export([start/4,deliver/3,nodes/0,stop/0]).
-export([info/1]).

-import(lists,[foldl/3,foreach/2,map/2,foldl/3,any/2,member/2]).
-import(lists,[filter/2,keysort/2,delete/2,usort/1]).

%% internal exports
-export([server/1]).


-define(dbg(X, Y), io:format(X, Y)).

%% interface
start(local, _, _, _) ->
    {error,not_allowed};
start(Node, Sinks, Endpoints, Routes) ->
    case whereis(netmesh) of
    undefined ->

        Handler = fun(Bin) ->
            case catch binary_to_term(Bin) of
            {'EXIT',_} ->
                ?dbg("netmesh malformed msg: ~p~n", [Bin]);

            {msg,Node,Rcpt,Body} ->
				case erlang:is_local_node(Node) of
				true ->
					Rcpt ! Body;
				false ->
					netmesh:deliver(Node, Rcpt, Body)
				end;
			
            {hi,Node1,Endpoints1,Routes1} ->
                %% jot down advertised info
                netmesh ! {node,self(),Node1},
                receive {ok,_} -> ok end,
                netmesh ! {endpoints,self(),Endpoints1},
                receive {ok,_} -> ok end,
                netmesh ! {routes,self(),Routes1},
                receive {ok,_} -> ok end
            end
        end,
        
        erlang:set_node(Node),
    
        Server = spawn(?MODULE, server,
            [#nm{handler=Handler,endpoints=Endpoints,routes=Routes}]),
        register(netmesh, Server),
    
        netmesh ! {sinks,self(),Sinks},
        receive {ok,_} -> ok end;
    
    Pid when is_pid(Pid) ->
        {error,already_started}
    end.

deliver(Node, Rcpt, Msg) ->					  %% Msg
	netmesh ! {send,Node,Rcpt,Msg},
	Msg.

nodes() ->
	netmesh ! {known_nodes,self()},
	receive
	{nodes,_,Nodes} ->
		Nodes
	end.

stop() ->
    case whereis(netmesh) of
    undefined ->
        {error,not_found};
    _ ->
        netmesh ! {shutdown,self()},
        receive
        {ok,_} ->
			erlang:set_node(local),
            ok;
        {error,_,Error} ->
            {error,Error}
        end
    end.

info(What) ->
	netmesh ! {info,self(),What},
	receive {ok,_,Info} -> Info end.

%% private

server(St0) ->
    receive
    {sinks,From,Sinks} ->
    
        St1 = foldl(fun({Mod,Spec}, St) ->
            case subsystem(Mod, St) of
            {ok,St2} ->
                Mod:open_sink(Spec),
                St2;
            {error,Error} ->
                ?dbg("Error starting subsystem ~w: ~p~n", [Mod,Error]),
                St
            end
        end, St0, Sinks),
        From ! {ok,self()},
        server(St1);
    
    {known_nodes,From} ->
		Nodes = usort(map(fun({Node,_,_}) -> Node end, St0#nm.endpoints)
			       ++ map(fun({Node,_,_}) -> Node end, St0#nm.routes)),
		From ! {nodes,self(),Nodes},
		server(St0);
    
    {shutdown,From} ->
        Rs = map(fun(Mod) ->
            Mod:stop()
        end, St0#nm.subsystems),
        case any(fun(ok) -> false; (_) -> true end, Rs) of
        true ->
            From ! {error,self(),shutdown};
        false ->
            From ! {ok,self()}
        end;
        %% server exits
    
    {send,Node,Rcpt,Msg}=X ->
        case route(Node, St0) of
        {ok,{_Node1,Mod,Spec}=Route} ->
        
            %% _Node1 may not be the same as node(Rcpt) or Node
            %% if forwarding takes place; more than one hop
            
            case subsystem(Mod, St0) of
            {ok,St1} ->
                case Mod:dispatch(Spec, {msg,Node,Rcpt,Msg}) of
                ok ->
                    server(St1);
                {error,Error} ->
					?dbg("Error dispatching message ~w: ~p, endpoint removed~n", [Msg,Error]),
					
                    self() ! X,
                    
                    St2 = noroute(Route, St1),
                    server(St2)
                end;
            {error,Error} ->
                ?dbg("Error starting subsystem2 ~w: ~p~n", [Mod,Error]),
                
                self() ! X,

                St1 = nosubsystem(Mod, St0),
                server(St1)
            end;
        {error,_Err} ->
			?dbg("Node ~w is unreachable, message ~p dropped...~n", [Node,Msg]),
			
            server(St0)
        end;

    {node,From,_Node} ->
        From ! {ok,self()},
        server(St0);
        
    {endpoints,From,_Endpoints} ->
        From ! {ok,self()},
        server(St0);

    {routes,From,_Routes} ->
        From ! {ok,self()},
        server(St0);
    
    {info,From,endpoints} ->
        From ! {ok,self(),St0#nm.endpoints},
        server(St0);
    {info,From,routes} ->
        From ! {ok,self(),St0#nm.routes},
        server(St0);
    {info,From,subsystems} ->
        From ! {ok,self(),St0#nm.subsystems},
        server(St0);
    {info,From,_} ->
        From ! {ok,self(),undefined},
        server(St0)
    end.

subsystem(Mod, St) ->        %% {ok,St2} | {error,Error}
    case member(Mod, St#nm.subsystems) of
    true ->
        {ok,St};
    false ->
        case Mod:start(St#nm.handler) of
        {ok,_} ->
            St2 = St#nm{subsystems=[Mod|St#nm.subsystems]},
            {ok,St2};
        E ->
            E
        end
    end.

%% endpoints,    %% {Node,Mod,Spec}, e.g. {xxx,tcp,{host1,port1}}
%% routes,       %% {Node,Inter,Metric}, e.g. {x,abc,10}    

nosubsystem(Mod, St) ->               %% St2
	Endpoints = filter(fun({_,Mod1,_}) when Mod =:= Mod1 -> false;
		(_) -> true end, St#nm.endpoints),
	St#nm{endpoints=Endpoints}.

route(Node, St) ->                    %% {ok,{Node,Mod,Spec}} | {error,Error}
    case filter(fun({N,_,_}) when N =:= Node -> true;
		(_) -> false end, St#nm.endpoints) of
	[] ->
		case filter(fun({N,_,_}) when N =:= Node -> true;
			(_) -> false end, St#nm.routes) of
		[] ->
			{error,unreachable};
		Rs ->
			[{_,Inter,_}|_] = keysort(3, Rs),	  %% best metric
			route(Inter, St)
		end;
	[Route|_] ->	%% TODO: be smarter here
		{ok,Route}
	end.

noroute({_,Mod,Spec}=_Route, St) ->                 %% St2,	  TODO: change names
	%% route is always an endpoint {Node,Mod,Spec}
	Endpoints = filter(fun({_,Mod1,Spec1}) when Mod1 =:= Mod, Spec1 =:= Spec -> false;
		(_) -> true end, St#nm.endpoints),
	St#nm{endpoints=Endpoints}.

%% EOF
