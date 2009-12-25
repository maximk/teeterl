
CC			= cl.exe
LINK		= link.exe
LIBR		= lib.exe

INCLUDES	= /I include /I include/apr /I include/apr-util
OPT_OPT		= /MDd /Od
#OPT_OPT	= /MD /O2 /Ot
CCOPTS		= /nologo /Gm /Zi /W3 /EHsc $(OPT_OPT) /Fo$B/ \
			  $(INCLUDES) \
			  /D WIN32 /D APR_DECLARE_STATIC /D APU_DECLARE_STATIC \
			  /D DEBUG

LDOPTS		= /nologo /debug /subsystem:console /incremental:no \
			  /nodefaultlib:msvcrt \
			  /fixed:no

#LDOPTS		= /nologo /subsystem:console /incremental:no \
#			  /ignore:4089 /fixed:no

# NB: 64-bit systems may require larger stack

APRLIBS		= lib/apr-1.lib \
			  lib/aprutil-1.lib
STDLIBS		= kernel32.lib advapi32.lib ws2_32.lib wsock32.lib \
			  ole32.lib shell32.lib rpcrt4.lib

ERLC		= "C:\Program Files\erl5.7.3\bin\erlc.exe"
ERL			= "C:\Program Files\erl5.7.3\bin\erl.exe"

STALE_MODS	= erl_parse erl_lint erl_expand_records

B			= bin
E			= ebin
X			= xbin
U			= util
C			= compiler
T			= test

ATOMTAB		= spec/atoms.tab
BIFTAB		= spec/bif.tab
OPSTAB		= spec/teeops.tab

OBJ			= $B/cstr.obj \
			  $B/atom.obj \
			  $B/named_tuple.obj \
			  $B/heap.obj \
			  $B/heap_gc.obj \
			  $B/mixed.obj \
			  $B/bits.obj \
			  $B/mpi.obj \
			  $B/list.obj \
			  $B/term_test.obj \
			  $B/compare.obj \
			  $B/compare_test.obj \
			  $B/exterm.obj \
			  $B/exterm_test.obj \
			  $B/code_base.obj \
			  $B/bif_erlang.obj \
			  $B/bif_code.obj \
			  $B/bif_lists.obj \
			  $B/bif_inet.obj \
			  $B/bif_gen_tcp.obj \
			  $B/bif_file.obj \
			  $B/msg_queue.obj \
			  $B/proc.obj \
			  $B/proc_main.obj \
			  $B/proc_test.obj \
			  $B/proc_queue.obj \
			  $B/buffer.obj \
			  $B/outlet.obj \
			  $B/outlet_mall.obj \
			  $B/ol_socket.obj \
			  $B/ol_listener.obj \
			  $B/ol_file.obj \
			  $B/scheduler.obj \
			  $B/modbin.obj \
			  $B/teeterl.obj


COMP		= $E/tt_compile.beam \
			  $E/tt_named_tuples.beam \
			  $E/tt_codegen.beam \
			  $E/tt_asm.beam \
			  $E/erl_parse.beam \
			  $E/erl_lint.beam \
			  $E/erl_expand_records.beam \
			  $E/atoms.beam \
			  $E/opcodes.beam \
			  $E/bifs.beam \
			  $E/test_asm.beam

MODS		= $X/init.cx \
			  $X/error_handler.cx \
			  $X/erlang.cx \
			  $X/lists.cx \
			  $X/queue.cx \
			  $X/string.cx \
			  $X/format.cx \
			  $X/proplists.cx \
			  $X/file.cx \
			  $X/inet.cx \
			  $X/gen_tcp.cx \
			  $X/random.cx \
			  $X/comet.cx \
			  $X/test.cx \
			  $X/navel.cx

TESTS		= $X/tuple_SUITE.x \
			  $X/lists_SUITE.x \
			  $X/estone_SUITE.x

XOBJ		= $B/init.obj \
			  $B/error_handler.obj \
			  $B/erlang.obj \
			  $B/lists.obj \
			  $B/queue.obj \
			  $B/string.obj \
			  $B/format.obj \
			  $B/proplists.obj \
			  $B/file.obj \
			  $B/inet.obj \
			  $B/gen_tcp.obj \
			  $B/random.obj \
			  $B/comet.obj \
			  $B/test.obj \
			  $B/navel.obj

$(MODS):	$(COMP)
$(TESTS):	$(COMP)

include/atom_defs.h atoms.inc compiler/atoms.erl:	$E/atoms_gen.beam $(ATOMTAB) $(BIFTAB)
	$(ERL) -pa $E -run atoms_gen compile_atoms $(ATOMTAB) $(BIFTAB) \
		include/atom_defs.h atoms.inc compiler/atoms.erl -run init stop -noshell

include/opcodes.h compiler/opcodes.erl:	$E/ops_gen.beam $(OPSTAB)
	$(ERL) -pa $E -run ops_gen opcodes $(OPSTAB) include/opcodes.h compiler/opcodes.erl -run init stop -noshell

include/bif.h builtins.inc compiler/bifs.erl: $E/bifs_gen.beam $(BIFTAB)
	$(ERL) -pa $E -run bifs_gen builtins $(BIFTAB) \
		include/bif.h builtins.inc compiler/bifs.erl -run init stop -noshell

modbin.c: modbin.inc

modbin.inc: $E/mods_gen.beam $(MODS)
	$(ERL) -pa $E -run mods_gen modules modbin.inc $(MODS) -run init stop -noshell

$B/teeterl.exe:	$(OBJ) $(XOBJ)
	$(LINK) $(LDOPTS) /out:"$@" $(OBJ) $(XOBJ) $(APRLIBS) $(STDLIBS)

$C/erl_parse.erl: $C/erl_parse.yrl $E/cli_run.beam
	$(ERL) -pa $E -run cli_run exec1 yecc file $C/erl_parse.yrl -run init stop -noshell
#
#	Under observation: comment out when done
#
#$X/named_tuple.cx: test/named_tuple.erl $(COMP)
#	$(ERL) -pa $E \
#		-run cli_run unload_mods $(STALE_MODS) \
#		-run tt_compile files_outdir test/named_tuple.erl $X \
#		-run init stop -noshell
	
all:		$(COMP) $(OBJ) $(TESTS) $B/teeterl.exe

.SUFFIXES:: .erl .c .cx .x

.c{$B}.obj::
	$(CC) /c $(CCOPTS) $<

{bif}.c{$B}.obj::
	$(CC) /c $(CCOPTS) $<

{$X}.cx{$B}.obj::
	$(CC) /c /TC $(CCOPTS) $<

{$U}.erl{$E}.beam::
	$(ERLC) -o $E $<

{$C}.erl{$E}.beam::
	$(ERLC) -o $E $<

{mod}.erl{$X}.cx::
	$(ERL) -pa $E \
		-run cli_run unload_mods $(STALE_MODS) \
		-run tt_compile files_outdir $< $X \
		-run init stop -noshell

{$T}.erl{$X}.x::
	$(ERL) -pa $E \
		-run cli_run unload_mods $(STALE_MODS) \
		-run tt_compile files_outdir_binary $< $X \
		-run init stop -noshell

#EOF
