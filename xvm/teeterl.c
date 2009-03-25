/*
* Copyright (c) 2009, Maxim Kharchenko
* All rights reserved.
*
* Redistribution and use in source and binary forms, with or without
* modification, are permitted provided that the following conditions are met:
*     * Redistributions of source code must retain the above copyright
*       notice, this list of conditions and the following disclaimer.
*     * Redistributions in binary form must reproduce the above copyright
*       notice, this list of conditions and the following disclaimer in the
*       documentation and/or other materials provided with the distribution.
*     * Neither the name of the author nor the names of his contributors
*	    may be used to endorse or promote products derived from this software
*		without specific prior written permission.
*
* THIS SOFTWARE IS PROVIDED BY Maxim Kharchenko ''AS IS'' AND ANY
* EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
* WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
* DISCLAIMED. IN NO EVENT SHALL Maxim Kharchenko BE LIABLE FOR ANY
* DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
* (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
* LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
* ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
* (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
* SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

#include <apr_file_io.h>
#include <apr_strings.h>
#include <apr_tables.h>

#include "teeterl.h"
#include "xmod.h"
#include "term.h"
#include "exterm.h"
#include "code_base.h"
#include "modbin.h"

static apr_pool_t *g_mempool;
static xpool_t *g_xpool;
static atoms_t *g_atoms;
static code_base_t *g_base;

apr_array_header_t *module_bins;

apr_status_t teeterl_init()
{
	xmod_bin_t *mp, *me;

	apr_initialize();
	atexit(apr_terminate);

	apr_pool_create(&g_mempool, 0);
	g_xpool = xpool_make(g_mempool);

	atoms_create(&g_atoms, g_mempool);
	g_base = code_base_make(g_mempool);

	module_bins = apr_array_make(g_mempool, 8, sizeof(xmod_bin_t));

	//teeterl_add_mod(init_xmod_bin);
	//...
#include "premods.inc"

	//preload all modules registered to date
	mp = (xmod_bin_t *)module_bins->elts;
	me = mp + module_bins->nelts;
	while (mp < me)
	{
		atom_cache_t *cache = atom_cache_make(g_xpool);
		term_t code = unpack_term(mp->data, mp->size, cache, g_atoms, g_xpool);

		mp->is_preloaded = 1;
		if (!code_base_load2(g_base, code))
			return APR_EBADF;

		mp++;
	}
	return APR_SUCCESS;
}

apr_status_t teeterl_add_stdmods(void)
{
	//teeterl_add_mod("\05lists", lists_bin_data, lists_bin_size);
	//...
#include "stdmods.inc"
	return APR_SUCCESS;
}

apr_status_t teeterl_add_compmods(void)
{
	//teeterl_add_mod("\05lists", lists_bin_data, lists_bin_size);
	//...
#include "compmods.inc"
	return APR_SUCCESS;
}

apr_status_t teeterl_add_mod(const char *name, unsigned char *data, unsigned int size)
{
	xmod_bin_t *bin;

	//TODO: check for duplicates?
	bin = (xmod_bin_t *)apr_array_push(module_bins);
	bin->name = (cstr_t *)name;
	bin->data = (apr_byte_t *)data;
	bin->size = (apr_size_t)size;
	bin->is_preloaded = 0;
	return APR_SUCCESS;
}

apr_status_t teeterl_exec(const char *m, const char *f, const char *as[])
{
	process_t *proc;
	term_t result, retval;

	term_t args = nil;
	term_t args1; // [args]
	term_t cons = nil;

	lst_add(args, cons, ztol(m, g_xpool), g_xpool);
	lst_add(args, cons, ztol(f, g_xpool), g_xpool);
	if (as != 0)
	{
		const char **p = as;
		while (*p)
			lst_add(args, cons, ztol(*p++, g_xpool), g_xpool);
	}
	args1 = make_list(args, g_xpool);

	proc = proc_spawn(g_base, g_atoms, A_INIT, A_RUN, args1);
	if (proc == 0)
	{
		printf("Unable to spawn init:run()\n");
		return APR_ENOPROC;
	}

	do {
		result = proc_main(proc, 1000000, &retval);
	} while (result == AI_YIELD);

	if (result == A_ERROR)
	{
		proc_print_error(proc, retval);
		return APR_ENOPROC;
	}
	else if (result == A_THROW)
	{
		printf("*** not caught: %s\n", stringify_term(retval, g_atoms, g_mempool));
		return APR_ENOPROC;
	}
	else if (result == A_EXIT)
	{
		printf("*** exited: %s\n", stringify_term(retval, g_atoms, g_mempool));
		return APR_ENOPROC;
	}
	else if (result == AI_DONE)
	{
		//normal exit, do nothing
	}
	else
	{
		proc_print_init_exits_unexpectedly(proc, result, retval);
		return APR_ENOPROC;
	}
	return APR_SUCCESS;
}

// EOF
