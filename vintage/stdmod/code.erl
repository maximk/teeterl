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
-module(code).
-export([server/0]).
-export([add_module/2,load_module/1,which/1,list_external/0,mtime/1]).

-export([set_breakpoint/2,unset_breakpoint/2,toggle_breakpoint/2]).

-export([function_info/2, source_line/2]).

-import(lists, [keysearch/3,keymember/3]).

-include("compiler/xmodule.hrl").

-record(cs, {bins=[],modinfo,path}).

%% do not use during bootstrapping
add_module(Mod, Bin) when is_atom(Mod), is_binary(Bin) ->
	code ! {add_module,self(),Mod,Bin},
	receive
	{module_added,_,Mod} -> ok;
	{error_,Error} -> {error,Error}
	end.

load_module(Mod) when is_atom(Mod) ->
	code ! {load_module,self(),Mod},
	receive
	{module_loaded,_,Mod} -> ok;
	{error,_,Error} -> {error,Error}
	end.

which(Mod) when is_atom(Mod) ->
	code ! {which_module,self(),Mod},
	receive
	{module_is,_,Mod,Which} ->
		Which
	end.

mtime(Mod) when is_atom(Mod) ->
	code ! {module_mtime,self(),Mod},
	receive
	{module_added_on,_,Mod,Mtime} ->
		Mtime
	end.

list_external() ->
	code ! {list_external,self()},
	receive
	{external_modules,_,Ms} ->
		Ms
	end.

%%====

set_breakpoint(Mod, Line) ->
	code ! {set_breakpoint,self(),Mod,Line},
	receive
	{breakpoint_set,_} ->
		ok;
	{error,_,Err} ->
		{error,Err}
	end.
  
unset_breakpoint(Mod, Line) ->
	code ! {unset_breakpoint,self(),Mod,Line},
	receive
	{breakpoint_unset,_} ->
		ok;
	{error,_,Err} ->
		{error,Err}
	end.

toggle_breakpoint(Mod, Line) ->
	code ! {toggle_breakpoint,self(),Mod,Line},
	receive
	{ok,_,State} ->
		{ok,State};
	{error,_,Err} ->
		{error,Err}
	end.

function_info(Mod, Off) ->
	code ! {function_by_offset,erlang:self(),Mod,Off},
	receive
	{function_found,_,F,N} ->
		{F,N};
	{function_not_found,_,Mod,Off} ->
		false
	end.

source_line(Mod, Offset) ->
	code ! {source_line,self(),Mod,Offset-1}, %% -1 is intentional
	receive
	{ok,_,Line} ->
		{ok,Line};
	{error,_,Err} ->
		{error,Err}
	end.

%%----

server() ->
	server0(#cs{modinfo=[]}).

server0(St0) ->
	%erlang:display({code_server0,St0}),
	receive
	{add_module,From,Mod,Bin} ->
		{Reply,St1} = request_add(Mod, Bin, St0),
		From ! Reply,
		server0(St1);
	{load_module,From,Mod} ->
		{Reply,St1} = request_load(Mod, St0),
		From ! Reply,
		server0(St1);
	{which_module,From,Mod} ->
		{Reply,St1} = request_which(Mod, St0),
		From ! Reply,
		server0(St1);
	{list_external,From} ->
		{Reply,St1} = request_list_external(St0),
		From ! Reply,
		server0(St1);
	{module_mtime,From,Mod} ->
		{Reply,St1} = request_mtime(Mod, St0),
		From ! Reply,
		server0(St1);
		
	{set_breakpoint,From,Mod,Line} ->
		{Reply,St1} = request_set_breakpoint(Mod, Line, St0),
		From ! Reply,
		server0(St1);
	{unset_breakpoint,From,Mod,Line} ->
		{Reply,St1} = request_unset_breakpoint(Mod, Line, St0),
		From ! Reply,
		server0(St1);
	{toggle_breakpoint,From,Mod,Line} ->
		{Reply,St1} = request_toggle_breakpoint(Mod, Line, St0),
		From ! Reply,
		server0(St1);
	
	{source_line,From,Mod,Offset} ->
		{Reply,St1} = request_source_line(Mod, Offset, St0),
		From ! Reply,
		server0(St1);
	
	{function_by_offset,From,Mod,Off} ->
		case lookup_function(Mod, Off, St0) of
		{F,N} ->
			From ! {function_found,self(),F,N};
		false ->
			From ! {function_not_found,self(),Mod,Off}
		end,
		server0(St0);
	{set_path,From,Path} ->
		From ! {path_set,self(),Path},
		server0(St0#cs{path=Path});
	{stop,From} ->
		From ! {stopped,self()}
	end.

request_add(Mod, Bin, St0) ->
	%%
	%% addition time of the module saved too
	%% it can be requested by 
	Bins = lists:keystore(Mod, 1, St0#cs.bins, {Mod,Bin,now()}),
	St1 = St0#cs{bins=Bins},
	{{module_added,self(),Mod},St1}.

request_load(Mod, St0) ->
	case find_module(Mod, St0) of
	Bin when is_binary(Bin) ->
		try
			T = binary_to_term(Bin),

			Mod = T#x.name,	%% check module name

			case code:load_module0(T#x.name, T#x.exports, T#x.code) of
			true ->
				Li = misc_item(T, lineinfo),
				Fo = misc_item(T, funcoffs),
				
				St1 = St0#cs{modinfo=[{Mod,Li,Fo}|St0#cs.modinfo]},
				{{module_loaded,self(),Mod},St1};
			false ->
				{{error,self(),load_module0},St0}
			end
		catch
			error:R ->
				{{error,self(),R},St0}
		end;
	false ->
		{{error,self(),noent},St0}
	end.

request_which(Mod, St0) ->
	R =	case code:embedded_module(Mod) of
	{ok,_,true} -> preloaded;
	{ok,_,false} ->	embedded;
	false ->
		case keymember(Mod, 1, St0#cs.bins) of
		true ->	external;
		false -> non_existing
		end
	end,
	{{module_is,self(),Mod,R},St0}.

request_list_external(St0) ->
	Ms = lists:map(fun({Mod,Bin,_Mtime}) ->
		{Mod,size(Bin)}
	end, St0#cs.bins),
	{{external_modules,self(),Ms},St0}.

request_mtime(Mod, St0) ->
	Mtime = case keysearch(Mod, 1, St0#cs.bins) of
	{value,{_,_,T}} -> T;
	false -> false
	end,
	{{module_added_on,self(),Mod,Mtime},St0}.

request_set_breakpoint(_Mod, _Line, St0) ->
	{{error,self(),not_supported},St0}.
%	case lists:keysearch(Mod, 1, St0#cs.modinfo) of
%	{value,{_,Li,_}} ->
%		case lists:keysearch(Line, 3, Li) of
%		{value,{Off,_,_}} ->
%			case code:set_brk0(Mod, Off) of
%			true ->
%				{{breakpoint_set,self()},St0};
%			false ->
%				{{error,self(),set_brk0},St0}
%			end;
%		false ->
%			{{error,self(),no_offset},St0}
%		end;
%	false ->
%		{{error,self(),no_module},St0}
%	end.

request_unset_breakpoint(_Mod, _Line, St0) ->
	{{error,self(),not_supported},St0}.
%	case lists:keysearch(Mod, 1, St0#cs.modinfo) of
%	{value,{_,Li,_}} ->
%		case lists:keysearch(Line, 3, Li) of
%		{value,{Off,_,_}} ->
%			case code:unset_brk0(Mod, Off) of
%			true ->
%				{{breakpoint_unset,self()},St0};
%			false ->
%				{{error,self(),unset_brk0},St0}
%			end;
%		false ->
%			{{error,self(),no_offset},St0}
%		end;
%	false ->
%		{{error,self(),no_module},St0}
%	end.

request_toggle_breakpoint(_Mod, _Line, St0) ->
	{{error,self(),not_supported},St0}.
%	case lists:keysearch(Mod, 1, St0#cs.modinfo) of
%	{value,{_,Li,_}} ->
%		case lists:keysearch(Line, 3, Li) of
%		{value,{Off,_,_}} ->
%			case code:toggle_brk0(Mod, Off) of
%			true ->
%				{{ok,self(),set},St0};
%			false ->
%				{{ok,self(),unset},St0};
%			error ->
%				{{error,self(),toggle_brk0},St0}
%			end;
%		false ->
%			{{error,self(),no_offset},St0}
%		end;
%	false ->
%		{{error,self(),no_module},St0}
%	end.

request_source_line(Mod, Offset, St0) ->
	case lists:keysearch(Mod, 1, St0#cs.modinfo) of
	{value,{_,Li,_}} ->
		case lists:filter(fun({Off1,Off2,_})
			when Off1 =< Offset, Off2 >= Offset -> true; (_) -> false end, Li) of
		[] ->
			{{error,self(),no_source},St0};
		[{_,_,Line}|_] ->
			{{ok,self(),Line},St0}
		end;
	false ->
		{{error,self(),no_module},St0}
	end.

find_module(Mod, St0) ->

	%%
	%% the order should provide for replacement
	%% of embedded modules by extenally loaded
	%%

	case keysearch(Mod, 1, St0#cs.bins) of
	{value,{_,Bin,_}} ->
		Bin;
	false ->
		case code:embedded_module(Mod) of
		{ok,Bin,_} -> Bin;
		false -> false
		end
	end.	

misc_item(#x{misc=Misc}, Item) ->
	case lists:keysearch(Item, 1, Misc) of
	{value,{_,Value}} ->
		Value;
	false ->
		none
	end.

lookup_function(Mod, Off, St0) ->
	case lists:keysearch(Mod, 1, St0#cs.modinfo) of
	{value,{_,_Li,Fo}} ->
		function_offset(Off, Fo);
	false ->
		false
	end.

%% NB: Off points to right after the last executed command

function_offset(Off, [{F,N,Off1},{_,_,Off2}|_]) when Off > Off1, Off =< Off2 -> {F,N};
function_offset(Off, [{F,N,Off1}]) when Off > Off1 -> {F,N};
function_offset(Off, [_|Fs]) -> function_offset(Off, Fs);
function_offset(_, []) -> false.

%% EOF
