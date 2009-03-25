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
-module(file).
-export([format_error/1]).
-export([open/2,path_open/3]).
-export([read/2,write/2]).
-export([read_file/1,write_file/2,write_file/3]).
-export([close/1]).

-export([change_group/2,change_owner/2,change_owner/3,change_time/2,change_time/3]).
-export([copy/2,del_dir/1]).
-export([read_file_info/1,list_dir3/1]).

-import(lists, [member/2,delete/2,reverse/1,map/2]).

-define(CHUNK_SIZE, 8192).

format_error({Line, Mod, Reason}) ->
    io_lib:format("~w: ~s", [Line, Mod:format_error(Reason)]);
format_error({enoent,StackTrace}) ->
	io_lib:format("file not found; stacktrace ~p", [StackTrace]);
format_error(E) ->
	io_lib:format("~w", [E]).

%% return file_io_serv intance suitable for fancy io calls or a file port

open(FileName, Mode) when is_atom(Mode) ->
	open(FileName, [Mode]);
open(FileName, Modes) ->
	case member(raw, Modes) of
	true ->
	
		%% low-level file port
		open2(FileName, delete(raw, Modes));
	
	false ->
		%%
		%% Modes: read, write, append
		%%
		%%
		
		R = member(read, Modes),
		W = member(write, Modes),
		A = member(append, Modes),
		
		Modes2 = if W, not R, not A ->
			[create,truncate|Modes];
		A, not W ->
			[write,create|Modes];
		A ->
			[create|Modes];
		true ->
			Modes
		end,
		
		file_io_srv:start(FileName, [raw|Modes2])
	end.

path_open([Dir|Dirs], File, Mode) ->
	FullName = filename:join(Dir, File),
	case open(FullName, Mode) of
	{ok,Io} ->
		{ok,Io,FullName};
	_ ->
		path_open(Dirs, File, Mode)
	end;
path_open([], _, _) ->
	{error, enoent}.

close(Io) when is_pid(Io) ->
	file_io_srv:stop(Io);
close(Io) when is_port(Io) ->
	erlang:close_port(Io),
	ok.

%%TODO: APR internal representation assumed

%% file open mode
-define(FOPEN_READ, 16#00001).	%% Open the file for reading
-define(FOPEN_WRITE, 16#00002). %% Open the file for writing
-define(FOPEN_CREATE, 16#00004). %% Create the file if not there
-define(FOPEN_APPEND, 16#00008). %% Append to the end of the file
-define(FOPEN_TRUNCATE, 16#00010). %% Open the file and truncate to 0 length
-define(FOPEN_BINARY, 16#00020). %% Open the file in binary mode
-define(FOPEN_EXCL, 16#00040). %% Open should fail if APR_CREATE and file exists.
-define(FOPEN_BUFFERED, 16#00080). %% Open the file for buffered I/O
-define(FOPEN_DELONCLOSE, 16#00100). %% Delete the file after close

%% file access permissions
-define(FPROT_USETID, 16#8000). %% Set user id
-define(FPROT_UREAD, 16#0400). %% Read by user
-define(FPROT_UWRITE, 16#0200). %% Write by user
-define(FPROT_UEXECUTE, 16#0100). %% Execute by user
-define(FPROT_GSETID, 16#4000). %% Set group id
-define(FPROT_GREAD, 16#0040). %% Read by group
-define(FPROT_GWRITE, 16#0020). %% Write by group
-define(FPROT_GEXECUTE, 16#0010). %% Execute by group
-define(FPROT_WSTICKY, 16#2000). %% Sticky bit
-define(FPROT_WREAD, 16#0004). %% Read by others
-define(FPROT_WWRITE, 16#0002). %% Write by others
-define(FPROT_WEXECUTE, 16#0001). %% Execute by others
-define(FPROT_OS_DEFAULT, 16#0FFF). %% use OS's default permissions

%% opens a low-level file port

open2(File, Modes) ->
	open2(File, Modes, default).
open2(File, Modes, Perms) when is_list(File) ->
	M = open_modes(Modes),
	P = open_perms(Perms),
	FileZ = list_to_binary([File,0]),
	case catch file:open0(FileZ, M, P) of
	{'EXIT',Error} ->
		{error,Error};
	Port ->
		{ok,Port}
	end.

open_modes(Ms) -> open_modes(Ms, 0).
open_modes([read|Ms], Bits) -> open_modes(Ms, Bits bor ?FOPEN_READ);
open_modes([write|Ms], Bits) -> open_modes(Ms, Bits bor ?FOPEN_WRITE);
open_modes([create|Ms], Bits) -> open_modes(Ms, Bits bor ?FOPEN_CREATE);
open_modes([append|Ms], Bits) -> open_modes(Ms, Bits bor ?FOPEN_APPEND);
open_modes([truncate|Ms], Bits) -> open_modes(Ms, Bits bor ?FOPEN_TRUNCATE);
open_modes([binary|Ms], Bits) -> open_modes(Ms, Bits bor ?FOPEN_BINARY);
open_modes([excl|Ms], Bits) -> open_modes(Ms, Bits bor ?FOPEN_EXCL);
open_modes([buffered|Ms], Bits) -> open_modes(Ms, Bits bor ?FOPEN_BUFFERED);
open_modes([delonclose|Ms], Bits) -> open_modes(Ms, Bits bor ?FOPEN_DELONCLOSE);
open_modes([], Bits) -> Bits.

open_perms(default) -> ?FPROT_OS_DEFAULT;
open_perms(Ps) -> open_perms(Ps, 0).
open_perms([set_uid|Ps], Bits) -> open_perms(Ps, Bits bor ?FPROT_USETID);
open_perms([ur|Ps], Bits) -> open_perms(Ps, Bits bor ?FPROT_UREAD);
open_perms([uw|Ps], Bits) -> open_perms(Ps, Bits bor ?FPROT_UWRITE);
open_perms([ux|Ps], Bits) -> open_perms(Ps, Bits bor ?FPROT_UEXECUTE);
open_perms([set_gid|Ps], Bits) -> open_perms(Ps, Bits bor ?FPROT_GSETID);
open_perms([gr|Ps], Bits) -> open_perms(Ps, Bits bor ?FPROT_GREAD);
open_perms([gw|Ps], Bits) -> open_perms(Ps, Bits bor ?FPROT_GWRITE);
open_perms([gx|Ps], Bits) -> open_perms(Ps, Bits bor ?FPROT_GEXECUTE);
open_perms([sticky|Ps], Bits) -> open_perms(Ps, Bits bor ?FPROT_WSTICKY);
open_perms([wr|Ps], Bits) -> open_perms(Ps, Bits bor ?FPROT_WREAD);
open_perms([ww|Ps], Bits) -> open_perms(Ps, Bits bor ?FPROT_WWRITE);
open_perms([wx|Ps], Bits) -> open_perms(Ps, Bits bor ?FPROT_WEXECUTE);
open_perms([], Bits) -> Bits.

read(F, N) when is_port(F), is_integer(N) ->
	case catch file:read0(F, N) of
	eof ->
		eof;
	Bin when is_binary(Bin) ->
		{ok,Bin};
	{'EXIT',E} ->
		{error,E}
	end.

write(F, IoList) when is_port(F) ->
	Bin = if is_binary(IoList) -> IoList;
	true -> list_to_binary(IoList) end,
	
	case catch file:write0(F, Bin) of
	N when is_integer(N), N =:= size(Bin) ->
		ok;
	{'EXIT',E} ->
		{error,E}
	end.

read_file(File) ->
	case open2(File, [read]) of
	{ok,F} ->
		case read_file(F, []) of
		{ok,Cs} -> {ok,list_to_binary(Cs)};
		E -> E
		end;
	{error,_}=E ->
		E
	end.
read_file(F, Cs) ->
	case catch file:read0(F, ?CHUNK_SIZE) of
	eof ->
		close2(F),
		{ok,reverse(Cs)};
	Bin when is_binary(Bin) ->
		read_file(F, [Bin|Cs]);
	{'EXIT',E} ->
		close2(F),
		{error,E}
	end.

write_file(File, Bin) ->
	write_file(File, Bin, []).

write_file(File, Bin, _Opts) ->		%% possible option is 'compressed'
	case open2(File, [write,create,truncate]) of
	{ok,F} ->
		R = write_file1(F, Bin),
		close2(F),
		R;
	{error,_}=E ->
		E
	end.

write_file1(_, <<>>) ->
	ok;
write_file1(F, Bin) when is_binary(Bin), size(Bin) =< ?CHUNK_SIZE ->
	case catch file:write0(F, Bin) of
	N when is_integer(N), N =:= size(Bin) ->
		ok;
	{'EXIT',Error} ->
		{error,Error}
	end;
write_file1(F, <<Chunk:?CHUNK_SIZE/binary,Rest/binary>>) ->
	case write_file1(F, Chunk) of
	ok -> write_file1(F, Rest);
	E -> E
	end.

%% close a low-level file port

close2(F) ->
	erlang:close_port(F).

change_group(_Filename, _Gid) ->	%% ok | {error, Reason}
	{error,not_implemented}.
	
change_owner(_Filename, _Uid) ->	%% ok | {error, Reason}
	{error,not_implemented}.
	
change_owner(_Filename, _Uid, _Gid) ->		%% ok | {error, Reason}
	{error,not_implemented}.
	
change_time(_Filename, _Mtime) ->		%% ok | {error, Reason}
	{error,not_implemented}.
	
change_time(_Filename, _Mtime, _Atime) ->	%% ok | {error, Reason}
	{error,not_implemented}.

copy(_Source, _Destination) ->	%% {ok, BytesCopied} | {error, Reason}
	{error,not_implemented}.

del_dir(_Dir) ->		%% ok | {error, Reason}
	{error,not_implemented}.

understand_file_info({file_info0,FileType,Name,Size,Mtime,Ctime,Atime,User,Group,Protection}) ->

	FileType1 = case FileType of
	1 -> regular;
	2 -> directory;
	3 -> chardev;
	4 -> blockdev;
	5 -> fifo;
	6 -> symlink;
	7 -> socket;
	_ -> unknown
	end,
	
	[Mtime1,Ctime1,Atime1] = map(fun(Time) ->
		{(Time div 1000000) div 1000000,
		 (Time div 1000000) rem 1000000,
		 (Time rem 1000000)}
	end, [Mtime,Ctime,Atime]),
	
	Wprot = Protection band 7,
	Gprot = (Protection bsr 4) band 7,
	Uprot = (Protection bsr 8) band 7,
	Setid = (Protection bsr 13) band 7,
	
	%% #define APR_FPROT_USETID      0x8000 /**< Set user id */
	%% #define APR_FPROT_UREAD       0x0400 /**< Read by user */
	%% #define APR_FPROT_UWRITE      0x0200 /**< Write by user */
	%% #define APR_FPROT_UEXECUTE    0x0100 /**< Execute by user */
	%% 
	%% #define APR_FPROT_GSETID      0x4000 /**< Set group id */
	%% #define APR_FPROT_GREAD       0x0040 /**< Read by group */
	%% #define APR_FPROT_GWRITE      0x0020 /**< Write by group */
	%% #define APR_FPROT_GEXECUTE    0x0010 /**< Execute by group */
	%% 
	%% #define APR_FPROT_WSTICKY     0x2000 /**< Sticky bit */
	%% #define APR_FPROT_WREAD       0x0004 /**< Read by others */
	%% #define APR_FPROT_WWRITE      0x0002 /**< Write by others */
	%% #define APR_FPROT_WEXECUTE    0x0001 /**< Execute by others */
	
	{file_info,FileType1,
		Name,
		Size,
		Mtime1,
		Ctime1,
		Atime1,
		User,
		Group,
		{Setid,Wprot,Gprot,Uprot}}.

%% TODO: use record from file.hrl

read_file_info(Filename) ->		%% {ok,...} | {error,Reason}
	case file:read_file_info0(Filename) of
	{error,_}=E ->
		E;
	RawFileInfo ->
		FileInfo = understand_file_info(RawFileInfo),
		{ok,FileInfo}
	end.

list_dir3(Dir) ->		%% {ok,[{file_info,FileType,Name,Size,Mtime,Ctime,Atime,User,Group,Prot}]} | {error,Error}
	case file:list_dir3_0(Dir) of
	{ok,RawFileInfos} ->
		{ok,map(fun(RawFileInfo) ->
			understand_file_info(RawFileInfo)
		end, RawFileInfos)};
	{error,_}=E ->
		E
	end.

%% EOF
