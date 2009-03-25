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
-module(files).
-export([ls/0,ls/1]).
-export([pwd/0]).
-export([cd/0,cd/1]).
-export([md/1]).
-export([rd/1]).
-export([hibernate/1,restore/1]).
-export([list_dir/1]).

-export([server/0]).

-import(lists, [keysearch/3,keyreplace/4,keydelete/3,keystore/4,foreach/2]).

-record(fs, {root,cwd}).

-record(folder, {name,
	subfolders = [],
	files = []}).

-record(file, {name,data}).

server() ->
	Root = #folder{name="/"},
	server0(#fs{root=Root,cwd="/"}).

server0(Fs0) ->
	receive
	{list,From,Dir} ->
		From ! dir_list(Dir, Fs0),
		server0(Fs0);
	{chdir,From,Dir} ->
		{Reply,Fs1} = change_dir(Dir, Fs0),
		From ! Reply,
		server0(Fs1);
	{getcwd,From} ->
		From ! {curdir,self(),Fs0#fs.cwd},
		server0(Fs0);
	{makedir,From,Dir} ->
		{Reply,Fs1} = make_dir(Dir, Fs0),
		From ! Reply,
		server0(Fs1);
	{remdir,From,Dir} ->
		{Reply,Fs1} = remove_dir(Dir, Fs0),
		From ! Reply,
		server0(Fs1);
	{makefile,From,File,Bin} ->
		{Reply,Fs1} = make_file(File, Bin, Fs0),
		From ! Reply,
		server0(Fs1);
	{readfile,From,File} ->
		From ! file_data(File, Fs0),
		server0(Fs0);
	{rename,From,OldFile,NewName} ->
		{Reply,Fs1} = rename_file(OldFile, NewName, Fs0),
		From ! Reply,
		server0(Fs1);
	{delete,From,File} ->
		{Reply,Fs1} = delete_file(File, Fs0),
		From ! Reply,
		server0(Fs1);
	{hibernate,From,IceFile} ->
		{Reply,Fs1} = hibernate_files(IceFile, Fs0),
		From ! Reply,
		server0(Fs1);
	{restore,From,IceFile} ->
		{Reply,Fs1} = restore_files(IceFile, Fs0),
		From ! Reply,
		server0(Fs1);
	{stop,From} ->
		From ! {stopped,self()}
	end.

dir_list(Dir, Fs0) ->
	case directory(Dir, Fs0) of
	{ok,Folder} ->
		Ls = 
		[{dir,Name} || #folder{name=Name} <- Folder#folder.subfolders]
			++
		[{file,Name,size(Data)} || #file{name=Name,data=Data} <- Folder#folder.files],
		{listed,self(),Ls};
	{error,_Error} ->
		{error,self(),Dir ++ " not found"}
	end.

change_dir(Dir, Fs0) ->
	case directory(Dir, Fs0) of
	{ok,_} ->
		{{curdir,self(),Dir},Fs0#fs{cwd=Dir}};
	{error,_} ->
		{{error,self(),"cannot chdir to " ++ Dir},Fs0}
	end.

make_dir("/" ++ _ =Dir, Fs0) ->
	Ds = string:tokens(Dir, "/"),
	R = make_dir0(Ds, Fs0#fs.root),
	{{ok,self()},Fs0#fs{root=R}};
make_dir(Dir, Fs0) ->
	Ds = string:tokens(Fs0#fs.cwd ++ Dir, "/"),
	R = make_dir0(Ds, Fs0#fs.root),
	{{ok,self()},Fs0#fs{root=R}}.

make_dir0([D|Ds], #folder{subfolders=Ss0}=Root0) ->
	case keysearch(D, #folder.name, Ss0) of
	{value,Folder} ->
		Folder1 = make_dir0(Ds, Folder),
		Ss1 = keyreplace(D, #folder.name, Ss0, Folder1),
		Root0#folder{subfolders=Ss1};
	false ->
		Folder = make_dir0(Ds, #folder{name=D}),
		Ss1 = [Folder|Ss0],
		Root0#folder{subfolders=Ss1}
	end;
make_dir0([], Folder) ->
	Folder.

remove_dir(_Dir, Fs0) ->
	{{error,self(),"not implemented"},Fs0}.

make_file("/" ++ File, Bin, Fs0) ->
	make_file0(File, Bin, Fs0);
make_file("./" ++ File, Bin, Fs0) ->
	make_file0(Fs0#fs.cwd ++ File, Bin, Fs0);
make_file(File, Bin, Fs0) ->
	make_file0(Fs0#fs.cwd ++ File, Bin, Fs0).

make_file0(File, Bin, Fs0) ->
	Ds = string:tokens(File, "/"),
	case make_file1(Ds, Fs0#fs.root, Bin) of
	#folder{}=Root ->
		{{ok,self()},Fs0#fs{root=Root}};
	{error,_} ->
		{{error,self(),"cannot make file " ++ File},Fs0}
	end.

make_file1([File], #folder{files=Files0}=F, Bin) ->
	Files1 = keystore(File, #file.name, Files0, #file{name=File,data=Bin}),
	F#folder{files=Files1};
make_file1([D|Ds], #folder{subfolders=Ss0}=F, Bin) ->
	case keysearch(D, #folder.name, Ss0) of
	{value,S0} ->
		S1 = make_file1(Ds, S0, Bin),
		Ss1 = keyreplace(D, #folder.name, Ss0, S1),
		F#folder{subfolders=Ss1};
	false ->
		%% create intermediate dirs as needed
		S1 = make_file1(Ds, #folder{name=D}, Bin),
		F#folder{subfolders=[S1|Ss0]}
	end.

file_data("/" ++ File, Fs0) ->
	file_data0(File, Fs0);
file_data("./" ++ File, Fs0) ->
	file_data0(Fs0#fs.cwd ++ File, Fs0);
file_data(File, Fs0) ->
	file_data0(Fs0#fs.cwd ++ File, Fs0).

file_data0(File, Fs0) ->
	case file_data1(string:tokens(File, "/"), Fs0#fs.root) of
	Data when is_binary(Data) ->
		{filedata,self(),Data};
	{error,E} ->
		{error,self(),E}
	end.

file_data1([File], #folder{files=Files}) ->
	case keysearch(File, #file.name, Files) of
	{value,#file{data=Data}} ->
		Data;
	false ->
		{error,enoent}
	end;
file_data1([D|Ds], #folder{subfolders=Ss}) ->
	case keysearch(D, #folder.name, Ss) of
	{value,F} ->
		file_data1(Ds, F);
	false ->
		{error,enoent}
	end.

%% TODO: rename is simplified, not move between directories
rename_file("/" ++ _ =OldFile, NewName, Fs0) ->
	rename_file0(OldFile, NewName, Fs0);
rename_file(OldFile, NewName, Fs0) ->
	rename_file0(Fs0#fs.cwd ++ OldFile, NewName, Fs0).

rename_file0(OldFile, NewName, Fs0) ->
	Ds = string:tokens(OldFile, "/"),
	Name = lists:last(string:tokens(NewName, "/")),
	case rename_file1(Ds, Fs0#fs.root, Name) of
	#folder{}=Root ->
		{{ok,self()},Fs0#fs{root=Root}};
	{error,E} ->
		{{error,self(),E},Fs0}
	end.

rename_file1([File], #folder{files=Files0}=F, Name) ->
	case keysearch(File, #file.name, Files0) of
	{value,T} ->
		Files1 = keyreplace(File, #file.name, Files0, T#file{name=Name}),
		F#folder{files=Files1};
	false ->
		{error,enoent}
	end;
rename_file1([D|Ds], #folder{subfolders=Ss0}=F, Name) ->
	case keysearch(D, #folder.name, Ss0) of
	{value,T0} ->
		case rename_file1(Ds, T0, Name) of
		#folder{}=T1 ->
			Ss1 = keyreplace(D, #folder.name, Ss0, T1),
			F#folder{subfolders=Ss1};
		{error,_}=E ->
			E
		end;
	false ->
		{error,enoent}
	end.

delete_file("/" ++ _ =File, Fs0) ->
	delete_file0(File, Fs0);
delete_file(File, Fs0) ->
	delete_file0(Fs0#fs.cwd ++ File, Fs0).

delete_file0(File, Fs0) ->
	Ds = string:tokens(File, "/"),
	case delete_file1(Ds, Fs0#fs.root) of
	#folder{}=Root ->
		{{ok,self()},Fs0#fs{root=Root}};
	{error,E} ->
		{{error,self(),E},Fs0}
	end.

delete_file1([File], #folder{files=Files0}=F) ->
	Files1 = keydelete(File, #file.name, Files0),
	F#folder{files=Files1};
delete_file1([D|Ds], #folder{subfolders=Ss0}=F) ->
	case keysearch(D, #folder.name, Ss0) of
	{value,T0} ->
		case delete_file1(Ds, T0) of
		#folder{}=T1 ->
			Ss1 = keyreplace(D, #folder.name, Ss0, T1),
			F#folder{subfolders=Ss1};
		{error,_}=E ->
			E
		end;
	false ->
		{error,enoent}
	end.

hibernate_files(IceFile, Fs0) ->
	Bin = term_to_binary(Fs0),
	case efile:write_file(IceFile, Bin) of
	{error,E} ->
		{{error,self(),E},Fs0};
	ok ->
		{{frozen,self()},Fs0}
	end.

restore_files(IceFile, Fs0) ->
	case efile:read_file(IceFile) of
	{error,E} ->
		{{error,self(),E},Fs0};
	{ok,Bin} ->
		case catch binary_to_term(Bin) of
		{'EXIT',E} ->
			{{error,self(),E},Fs0};
		Fs1 ->
			{{restored,self()},Fs1}
		end
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

directory("/" ++ _ =Dir, Fs0) ->
	Ds = string:tokens(Dir, "/"),
	directory0(Ds, Fs0#fs.root);
directory(Dir, Fs0) ->
	Ds = string:tokens(Fs0#fs.cwd ++ Dir, "/"),
	directory0(Ds, Fs0#fs.root).

directory0([D|Ds], #folder{subfolders=Ss}) ->
	case keysearch(D, #folder.name, Ss) of
	{value,Folder} ->
		directory0(Ds, Folder);
	false ->
		{error,enoent}
	end;
directory0([], Folder) ->
	{ok,Folder}.
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ls() -> ls("").
ls(Dir) ->
	files ! {list,self(),Dir},
	receive
	{listed,_,Ls} ->
		foreach(fun({dir,Name}) ->
			io:format(">~s~n", [Name]);
		({file,Name,Size}) ->
			io:format("~s\t~w~n", [Name,Size])
		end, Ls);
	{error,_,M} ->
		io:format("Error: ~s~n", [M])
	end.

cd() -> cd("").
cd(Dir) ->
	files ! {chdir,self(),Dir},
	receive
	{curdir,_,Dir} ->
		ok;
	{error,_,M} ->
		io:format("Error: ~s~n", [M])
	end.

pwd() ->
	files ! {getcwd,self()},
	receive
	{curdir,_,Dir} ->
		io:format("~s~n", [Dir])
	end.

md(Dir) ->
	files ! {makedir,self(),Dir},
	receive
	{ok,_} ->
		ok;
	{error,_,M} ->
		io:format("Error: ~s~n", [M])
	end.

rd(Dir) ->
	files ! {remdir,self(),Dir},
	receive
	{ok,_} ->
		ok;
	{error,_,M} ->
		io:format("Error: ~s~n", [M])
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

hibernate(IceFile) ->
	files ! {hibernate,self(),IceFile},
	receive
	{frozen,_} ->
		ok;
	{error,_,E} ->
		{error,E}
	end.

restore(IceFile) ->
	files ! {restore,self(),IceFile},
	receive
	{restored,_} ->
		ok;
	{error,_,E} ->
		{error,E}
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

list_dir(Dir) ->
	files ! {list,self(),Dir},
	receive
	{listed,_,Ls} ->
		{ok,Ls};
	{error,_,M} ->
		{error,M}
	end.

%% EOF
