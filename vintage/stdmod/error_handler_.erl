-module(error_handler).
-export([undefined_function/3]).

-import(erlang, [error/1]).

-include("compiler/xmodule.hrl").

undefined_function(Mod, Func, Args) ->
	case code:module_loaded(Mod) of
	false ->
		{_,Path} = process_info(self(), mod_path),
		case find_module(Mod, Path) of
		{ok,Bin,File} ->
		  try
			M = binary_to_term(Bin),
			
			Mod = M#x.name,	%% check module name
			
			case code:load_module(M#x.name, M#x.exports, M#x.code) of
			true ->
				%% save source line info and function offsets if present
				init ! {mod_info,
					Mod,
					File,
					misc_item(M, lineinfo),
					misc_item(M, funcoffs)};
			false ->
				error(load_module)
			end
			
		  catch
		    error:R -> error({badfile,File,R})
		  end;
		notfound ->
			error({undef,Mod,Func,Args})
		end,

		%% apply2 assumes loaded Erlang code (not a bif, no error_handler)		
		erlang:apply2(Mod, Func, Args);
	true ->
		error({undef0,Mod,Func,Args})
	end.

misc_item(#x{misc=Misc}, Item) ->
	case lists:keysearch(Item, 1, Misc) of
	{value,{_,Value}} ->
		Value;
	false ->
		none
	end.

find_module(_, []) -> notfound;
find_module(Mod, [Dir|Path]) ->
	File = Dir ++ "/" ++ atom_to_list(Mod) ++ ".x",
	case file:read_file(File) of
	{ok,Bin} ->
		{ok,Bin,File};
	_ ->
		find_module(Mod, Path)
	end.

%% EOF