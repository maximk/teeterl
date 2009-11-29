-module(file).
-export([read_file/1]).

-import(lists, [reverse/1]).

-define(CHUNK_SIZE, 8192).

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

%% opens a low-level file outlet

open2(File, Modes) ->
	open2(File, Modes, default).
open2(File, Modes, Perms) when is_list(File) ->
	M = open_modes(Modes),
	P = open_perms(Perms),
	FileZ = list_to_binary([File,0]),
	case catch file:open0(FileZ, M, P) of
	{'EXIT',Error} ->
		{error,Error};
	Outlet ->
		{ok,Outlet}
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

close2(F) ->
	erlang:close(F).

%%EOF
