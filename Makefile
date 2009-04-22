CC = gcc
LD = gcc
LIBR = ar

#
# The following two lines should be edited to pinpoint the location of your
# copy of APR (Apache Portable Runtime Library)
#
APR_LIBS = /opt/local/lib/libapr-1.a /opt/local/lib/libaprutil-1.a
APR_INCLUDES = -I/opt/local/include/apr-1 -I/opt/local/include/apr-util-1

#
# The following two variable refer to the location of Erlang/OTP compiler
# and runtime system needed to bootstrap teeterl
#
# When a working version of teeterl is there these may be replaced by
# teeterl version -- not recommended as built will be slower
#
ERLC = /opt/local/bin/erlc
ERL = /opt/local/bin/erl

STD_LIBS =

CCOPTS = -ggdb -iquotexvm $(APR_INCLUDES) -DAPR_DECLARE_STATIC -DAPU_DECLARE_STATIC
#CCOPTS = -O3 -fast -iquotexvm $(APR_INCLUDES) -DAPR_DECLARE_STATIC -DAPU_DECLARE_STATIC

#
# Output files
#
TEETERL_LIB = $B/teeterl.a
TEETERL_EXEC = $B/exec

B		       = bin
E		       = ebin
X		       = xbin
U		       = util
C		       = compiler
V		       = xvm
S		       = stdmod
P		       = snip
F		       = bif

BIFTAB = spec/biftab.txt
OPTAB = spec/optab2.txt
ATOMTAB = spec/atoms.lst

UTIL	    = $E/vmgen.beam \
			  $E/opsgen2.beam \
			  $E/aegen.beam

COMPILER	= $E/x_compile.beam \
			  $E/x_kernel.beam \
			  $E/x_codegen.beam \
			  $E/x_asmopt.beam \
			  $E/x_asm2.beam \
			  $E/bif.beam \
			  $E/ops.beam \
			  $E/x_disasm2.beam

SNIP	    = $P/arith.c $P/binary.c $P/compare.c $P/jump.c \
			  $P/list.c $P/literal.c $P/msg.c $P/recog.c \
			  $P/stack.c $P/vararg.c

OBJ		    = $B/atom.o \
			  $B/atom_cache.o \
			  $B/bignum.o \
			  $B/bits.o \
			  $B/buffer.o \
			  $B/code_base.o \
			  $B/cstr.o \
			  $B/errors.o \
			  $B/exterm.o \
			  $B/hash.o \
			  $B/md5.o \
			  $B/sha1.o \
			  $B/lit_pool.o \
			  $B/msg_queue.o \
			  $B/teeterl.o \
			  $B/port.o \
			  $B/port_listener.o \
			  $B/port_pipe.o \
			  $B/port_socket.o \
			  $B/port_udp.o \
			  $B/port_file.o \
			  $B/run_code.o \
			  $B/tcomp.o \
			  $B/term.o \
			  $B/xpool.o \
			  $B/bif_erlang.o \
			  $B/bif_lists.o \
			  $B/bif_code.o \
			  $B/bif_io.o \
			  $B/bif_inet.o \
			  $B/bif_file.o \
			  $B/bif_gen_tcp.o \
			  $B/bif_gen_udp.o \
			  $B/bif_crypto.o \
			  $B/bif_os.o

PREX	    = $X/prim_erlang.cx \
			  $X/init.cx \
			  $X/code.cx \
			  $X/error_handler.cx \
			  $X/queue.cx

STDX	    = $X/lists.cx \
			  $X/erlang.cx \
			  $X/gen_tcp.cx \
			  $X/gen_udp.cx \
			  $X/inet.cx \
			  $X/file.cx \
			  $X/filename.cx \
			  $X/os.cx \
			  $X/string.cx \
			  $X/regexp.cx \
			  $X/random.cx \
			  $X/file_io_srv.cx \
			  $X/stdio.cx \
			  $X/io.cx \
			  $X/io_lib.cx \
			  $X/io_lib_format.cx \
			  $X/io_lib_fread.cx \
			  $X/io_lib_pretty.cx \
			  $X/erl_scan.cx \
			  $X/erl_parse.cx \
			  $X/erl_eval.cx \
			  $X/erl_pp.cx \
			  $X/erl_lint.cx \
			  $X/erl_internal.cx \
			  $X/erl_expand_records.cx \
			  $X/erl_bits.cx \
			  $X/eval_bits.cx \
			  $X/proplists.cx \
			  $X/ordsets.cx \
			  $X/gb_sets.cx \
			  $X/gb_trees.cx \
			  $X/sets.cx \
			  $X/sofs.cx \
			  $X/dict.cx \
			  $X/orddict.cx \
			  $X/epp.cx \
			  $X/packages.cx \
			  $X/netmesh.cx \
			  $X/netudp.cx \
			  $X/nettcp.cx \
			  $X/rpc.cx \
		   	  $X/x_internal.cx \
			  $X/crypto.cx

COMPX		= $X/x_compile.cx \
			  $X/x_kernel.cx \
			  $X/x_codegen.cx \
			  $X/x_asmopt.cx \
			  $X/x_asm2.cx \
			  $X/bif.cx \
			  $X/ops.cx \
			  $X/x_disasm2.cx \
			  $X/core_lib.cx \
			  $X/erl_bifs.cx \
			  $X/sys_core_fold.cx \
			  $X/sys_core_inline.cx \
			  $X/sys_pre_expand.cx \
			  $X/v3_core.cx \
			  $X/appgen.cx

XOBJ	    = $B/prim_erlang.o \
			  $B/init.o \
			  $B/code.o \
			  $B/error_handler.o \
			  $B/queue.o \
			  $B/lists.o \
			  $B/erlang.o \
			  $B/gen_tcp.o \
			  $B/gen_udp.o \
			  $B/inet.o \
			  $B/file.o \
			  $B/filename.o \
			  $B/os.o \
			  $B/string.o \
			  $B/regexp.o \
			  $B/random.o \
			  $B/file_io_srv.o \
			  $B/stdio.o \
			  $B/io.o \
			  $B/io_lib.o \
			  $B/io_lib_format.o \
			  $B/io_lib_fread.o \
			  $B/io_lib_pretty.o \
			  $B/erl_scan.o \
			  $B/erl_parse.o \
			  $B/erl_eval.o \
			  $B/erl_pp.o \
			  $B/erl_lint.o \
			  $B/erl_internal.o \
			  $B/erl_expand_records.o \
			  $B/erl_bits.o \
			  $B/eval_bits.o \
			  $B/proplists.o \
			  $B/ordsets.o \
			  $B/gb_sets.o \
			  $B/gb_trees.o \
			  $B/sets.o \
			  $B/sofs.o \
			  $B/dict.o \
			  $B/orddict.o \
			  $B/epp.o \
			  $B/netmesh.o \
			  $B/netudp.o \
			  $B/nettcp.o \
			  $B/rpc.o \
			  $B/packages.o \
			  $B/net_kernel.o \
			  $B/x_internal.o \
			  $B/x_compile.o \
			  $B/x_kernel.o \
			  $B/x_codegen.o \
			  $B/x_asmopt.o \
			  $B/x_asm2.o \
			  $B/bif.o \
			  $B/ops.o \
			  $B/x_disasm2.o \
			  $B/core_lib.o \
			  $B/erl_bifs.o \
			  $B/sys_core_fold.o \
			  $B/sys_core_inline.o \
			  $B/sys_pre_expand.o \
			  $B/v3_core.o \
			  $B/appgen.o \
			  $B/crypto.o

$C/bif.erl:     $E/vmgen.beam $(BIFTAB)
	$(ERL) -pa $E -run vmgen bif_tab $(BIFTAB) $C/bif.erl -run init stop -noshell

$C/ops.erl:     $E/opsgen2.beam $(OPTAB)
	$(ERL) -pa $E -run opsgen2 file $(OPTAB) $C/ops.erl -run init stop -noshell

$V/bif.h $V/builtins.inc:       $E/vmgen.beam $(BIFTAB)
	$(ERL) -pa $E -run vmgen bif_list $(BIFTAB) $V/bif.h $V/builtins.inc -run init stop -noshell

$V/atom_defs.h $V/atoms.inc:    $E/aegen.beam $(ATOMTAB) $(BIFTAB)
	$(ERL) -pa $E -run aegen compile_atoms $(ATOMTAB) $(BIFTAB) $V/atom_defs.h $V/atoms.inc -run init stop -noshell

$V/run_cases.inc:       $E/vmgen.beam $(SNIP)
	$(ERL) -pa $E -run vmgen run_cases $V/run_cases.inc $(SNIP) -run init stop -noshell

$V/modbin.h $V/premods.inc $V/stdmods.inc $V/compmods.inc:	      $E/vmgen.beam
	$(ERL) -pa $E -run vmgen declmods $V/modbin.h $(PREX) $(STDX) $(COMPX) -run init stop -noshell
	$(ERL) -pa $E -run vmgen addmods $V/premods.inc $(PREX) -run init stop -noshell
	$(ERL) -pa $E -run vmgen addmods $V/stdmods.inc $(STDX) -run init stop -noshell
	$(ERL) -pa $E -run vmgen addmods $V/compmods.inc $(COMPX) -run init stop -noshell

$B/code_base.o: $V/code_base.c $V/bif.h $V/builtins.inc $V/atom_defs.h
	$(CC) -c $(CCOPTS) -o $@ $V/code_base.c

$B/run_code.o:  $V/run_code.c $V/run_cases.inc $V/atom_defs.h
	$(CC) -c $(CCOPTS) -o $@ $V/run_code.c

$B/atom.o:      $V/atom.c $V/atoms.inc $V/atom_defs.h
	$(CC) -c $(CCOPTS) -o $@ $V/atom.c

$B/teeterl.o:      $V/modbin.h $V/premods.inc $V/stdmods.inc $V/compmods.inc
	$(CC) -c $(CCOPTS) -o $@ $V/teeterl.c

$(STDX):	$(COMPILER)

$(PREX):	$(COMPILER)

$(OBJ):	 $V/atom_defs.h

$(TEETERL_LIB):      $(OBJ) $(XOBJ)
	$(LIBR) r $@ $(OBJ) $(XOBJ)

$(TEETERL_EXEC):	$B/exec.o $(TEETERL_LIB)
	$(LD) $(LDOPTS) -Xlinker -lpthread -o $@ $B/exec.o $(TEETERL_LIB) $(APR_LIBS) $(STD_LIBS)

all:	    $(PREX) $(STDX) $(COMPX) $(XOBJ) $(TEETERL_LIB) $(TEETERL_EXEC)

clean:
	-rm $(TEETERL_LIB) $(TEETERL_EXEC) $B/*.o $E/*.beam $X/*.cx $X/*.x 

.SUFFIXES:
.SUFFIXES: .beam .erl .c .cx

$E/%.beam : $C/%.erl
	$(ERLC) -o $E $<

$E/%.beam : $U/%.erl
	$(ERLC) -o $E $<

$X/%.cx : $S/%.erl
	$(ERL) -pa $E -run x_compile files $< $X -run init stop -noshell

$X/%.cx : $C/%.erl
	$(ERL) -pa $E -run x_compile files $< $X -run init stop -noshell

$B/%.o : $F/%.c
	$(CC) -c $(CCOPTS) -o $@ $<

$B/%.o : $X/%.cx
	$(CC) -c $(CCOPTS) -o $@ -x c $<

$B/%.o : $V/%.c
	$(CC) -c $(CCOPTS) -o $@ $<

$B/%.o : $C/%.c
	$(CC) -c $(CCOPTS) -o $@ $<

# EOF
