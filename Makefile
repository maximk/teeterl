
ERLC := /usr/local/bin/erlc
ERL := /usr/local/bin/erl

ATOMTAB	:= spec/atoms.tab
BIFTAB := spec/bif.tab
OPSTAB := spec/teeops.tab

E := ebin
C := compiler
U := util
F := bif
M := mod
X := xbin
T := test

COMP := $(patsubst $C/%.erl,$E/%.beam,$(wildcard $C/*.erl))
UTIL := $(patsubst $U/%.erl,$E/%.beam,$(wildcard $U/*.erl))
BIF := $(patsubst %.c,%.o,$(wildcard $F/*.c))

OBJS := $(patsubst %.c,%.o,$(wildcard *.c))

MODS := $(patsubst $M/%.erl,$X/%.cx,$(wildcard $M/*.erl))
XOBJS := $(patsubst %.cx,%.o,$(MODS))

APRLIBS := lib/libapr-1.a lib/libaprutil-1.a

TARGET := teeterl

CPPFLAGS := -I include -I include/apr -I include/apr-util
CPPFLAGS += -DAPR_DECLARE_STATIC -DAPU_DECLARE_STATIC
CPPFLAGS += -DLINUX=2 -D_REENTRANT -D_GNU_SOURCE -D_LARGEFILE64_SOURCE
CPPFLAGS += -DDEBUG
CFLAGS := -Wall -Werror -g
LDFLAGS := -pthread

.PHONY: default
default: $(COMP) $(BIF) $(TARGET)

ALL_OBJS := $(OBJS) $(BIF) $(XOBJS)

$(OBJS): %.o: %.c $(DEPS)
	$(CC) $(CFLAGS) $(CPPFLAGS) -c $< -o $@

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

$C/erl_parse.erl: $C/erl_parse.yrl $E/cli_run.beam
	$(ERL) -pa $E -run cli_run exec1 yecc file $C/erl_parse.yrl -run init stop -noshell

$(COMP): $E/%.beam: $C/%.erl
	$(ERLC) -o $E $<

$(UTIL): $E/%.beam: $U/%.erl
	$(ERLC) -o $E $<

$(BIF): %.o: %.c
	$(CC) $(CFLAGS) $(CPPFLAGS) -c $< -o $@

$(XOBJS): %.o: %.cx
	$(CC) -c -x c $(CFLAGS) $(CPPFLAGS) $< -o $@

$(MODS): $X/%.cx: $M/%.erl
	$(ERL) -pa $E \
		-run tt_compile files_outdir $< $X \
		-run init stop -noshell

$(TARGET):	$(OBJS) $(BIF) $(XOBJS)
	$(CC) $(LDFLAGS) -o $@ $(OBJS) $(BIF) $(XOBJS) $(APRLIBS)

define all_sources
     (find . -follow -name '*.[chS]' -print)
endef

.PHONY: cscope
cscope:
	$(all_sources) > cscope.files
	cscope -k -b -q

#EOF

