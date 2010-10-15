//
//
//

#include "bifimpl.h"
#include "modbin.h"
#include "code_base.h"
#include "exterm.h"

#include <stdio.h>
#include <stdlib.h>

term_t bif_load_module1(term_t Where, proc_t *proc)
{
	term_t bin = noval;
	term_t code;
	term_box_t *tb;
	term_t module;

	if (!is_atom(Where) && !is_binary(Where))
		bif_bad_arg(Where);

	if (is_atom(Where))
	{
		modbin_t **mb;
		cstr_t *name;

		if (code_base_lookup(proc->teevm->base, Where) != 0)
			return A_NOT_PURGED;

		name = atoms_get(proc->teevm->atoms, atom_index(Where));

		mb = modbins;
		while (*mb)
		{
			if (scomp((*mb)->name, name))
			{
				bin = heap_binary(proc->heap, (*mb)->size*8, (*mb)->data);
				break;
			}
			mb++;
		}

		if (bin == noval)
			return A_FALSE;
	}
	else
		bin = Where;

	code = binary_to_term(bin, proc->teevm->atoms, proc->heap);

	if (code == noval)
		return A_BADFILE;
	
	tb = peel(code);
	if (!code_base_load(proc->teevm->base, proc->teevm->nm_tuples,
		tb->tuple.elts[1],
		tb->tuple.elts[2],
		tb->tuple.elts[3],
		tb->tuple.elts[4],
		tb->tuple.elts[5],
		tb->tuple.elts[6]) == 0)
		return A_BADFILE;

	module = tb->tuple.elts[1];
	if (is_atom(Where))
	{
		if (Where != module)
			return A_BADFILE;
		return A_TRUE;
	}
	else
		return heap_tuple2(proc->heap, A_MODULE, module);
}

term_t bif_is_loaded1(term_t Module, proc_t *proc)
{
	if (!is_atom(Module))
		bif_bad_arg(Module);
	if (code_base_lookup(proc->teevm->base, Module) == 0)
		return A_FALSE;
	else
		return A_TRUE;
}

// code:module_offset/3 [7]
term_t bif_module_offset3(term_t Module, term_t File, term_t Line, proc_t *proc)
{
	not_implemented("code:module_offset/3");
}

// code:source_line/2 [8]
term_t bif_source_line2(term_t Module, term_t Offset, proc_t *proc)
{
	module_t *m;

	if (!is_atom(Module))
		bif_bad_arg(Module);
	if (!is_int(Offset))
		bif_bad_arg(Offset);

	m = code_base_lookup(proc->teevm->base, Module);
	if (m != 0)
	{
		const char *file;
		int line;
		
		if (!module_source_from_offset(m, int_value(Offset), &file, &line))
			return A_FALSE;

		return heap_tuple2(proc->heap,
			ztol(file, proc->heap),
			tag_int(line));
	}
	
	return A_FALSE;
}

//EOF
