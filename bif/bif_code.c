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

#include "bif.h"

#include "code_base.h"
#include "port.h"
#include "errors.h"
#include "xmod.h"

extern apr_array_header_t *module_bins;

term_t bif_embedded_module1(term_t Mod, process_t *ctx)
{
	xmod_bin_t *mp, *me;
	cstr_t *s;
	
	if (!is_atom(Mod))
		return A_BADARG;

	s = atoms_get(proc_atoms(ctx), index(Mod));
	
	mp = (xmod_bin_t *)module_bins->elts;
	me = mp + module_bins->nelts;
	while (mp < me)
	{
		if (scomp(mp->name, s))
		{
			term_t bin = make_binary(intnum(mp->size), mp->data, proc_gc_pool(ctx));
			result(make_tuple3(A_OK, bin, bool(mp->is_preloaded), proc_gc_pool(ctx)));
			return AI_OK;
		}
		mp++;
	}
	result(A_FALSE);
	return AI_OK;
}

term_t bif_list_embedded0(process_t *ctx)
{
	term_t l = nil, cons = nil;
	xmod_bin_t *mp, *me;
	
	mp = (xmod_bin_t *)module_bins->elts;
	me = mp + module_bins->nelts;
	while (mp < me)
	{
		//{Mod,Size,IsPreloaded}
		term_t mod = atom(atoms_set(proc_atoms(ctx), mp->name));
		term_t size = intnum(mp->size);
		term_t is_preloaded = bool(mp->is_preloaded);

		term_t triple = make_tuple3(mod, size, is_preloaded, proc_gc_pool(ctx));
		lst_add(l, cons, triple, proc_gc_pool(ctx));

		mp++;
	}
	result(l);
	return AI_OK;
}

term_t bif_load_module0_3(term_t Mod, term_t Exports, term_t Preloaded, process_t *ctx)
{
	code_base_t *base = proc_code_base(ctx);

	// TODO: check params

	int ok = code_base_load(base, Mod, Exports, Preloaded);
	result(bool(ok));
	return AI_OK;
}

term_t bif_is_loaded1(term_t Mod, process_t *ctx)
{
	term_t loaded;
	code_base_t *base = proc_code_base(ctx);

	if (!is_atom(Mod))
		return A_BADARG;
	
	loaded = bool(code_base_starts2(base, Mod) != 0);
	result(loaded);
	return AI_OK;
}

term_t bif_all_loaded0(process_t *ctx)
{
	term_t mlst = code_base_list_modules(proc_code_base(ctx), proc_gc_pool(ctx));
	result(mlst);
	return AI_OK;
}

term_t bif_spawn0_3(term_t Mod, term_t Fun, term_t Args, process_t *ctx)
{
	process_t *proc;

	if (!is_atom(Mod) || !is_atom(Fun) || !is_list(Args))
		return A_BADARG;

	proc = proc_spawn(proc_code_base(ctx), proc_atoms(ctx), Mod, Fun, Args);
	if (proc == 0)
		return A_BADARG;

	result(proc_pid(proc, proc_gc_pool(ctx)));
	return AI_OK;
}

term_t bif_spawn0_1(term_t F, process_t *ctx)
{
	process_t *proc;
	term_t mod, fun, args = nil;
	term_t cons = nil;
	term_t fridge;
	int i, nfree;

	if (!is_fun(F))
		return A_BADARG;

	fridge = fun_fridge(F);
	nfree = int_value2(tup_size(fridge));

	if (int_value2(fun_arity(F)) != nfree)
		return A_BADARG;

	for (i = 0; i < nfree; i++)
		lst_add(args, cons, tup_elts(fridge)[i], proc_gc_pool(ctx));

	mod = fun_amod(F);
	fun = fun_afun(F);

	proc = proc_spawn(proc_code_base(ctx), proc_atoms(ctx), mod, fun, args);
	if (proc == 0)
		return A_BADARG;

	result(proc_pid(proc, proc_gc_pool(ctx)));
	return AI_OK;
}

term_t bif_run_slice2(term_t Pid, term_t Reductions, process_t *ctx)
{
	process_t *proc;
	term_t retval, retval1;
	term_t res;
	if (!is_pid(Pid) || !is_int(Reductions))
		return A_BADARG;
	proc = proc_lookup(pid_serial(Pid));
	res = proc_main(proc, int_value2(Reductions), &retval);

	if (res == AI_DONE)
	{
		//proc_destroy(proc);
		//- process should not be destroyed now
		//- as we may need to notify links first

		retval1 = marshal_term(retval, proc_gc_pool(ctx));
		result(make_tuple2(AI_DONE, retval1, proc_gc_pool(ctx)));
	}
	else
	{
		if (res == AI_YIELD)
			result(res);
		else
		{
			retval1 = marshal_term(retval, proc_gc_pool(ctx));
			result(make_tuple2(res, retval1, proc_gc_pool(ctx)));
		}
	}

	return AI_OK;
}

term_t bif_destroy_process1(term_t Pid, process_t *ctx)
{
	process_t *proc;
	if (!is_pid(Pid))
		return A_BADARG;
	proc = proc_lookup(pid_serial(Pid));
	if (proc)
		proc_destroy(proc);
	result(A_TRUE);
	return AI_OK;
}

term_t bif_poll_ports1(term_t Time, process_t *ctx)
{
	//apr_time_t t1, t2;

	apr_status_t rs;
	apr_interval_time_t micros;

	if (!is_int(Time))
		return A_BADARG;
	micros = int_value(Time);

	//if (micros != 0)
	//	printf("ports_poll for %ld\n", micros);

	// XXX
	//t1 = apr_time_now();

	rs = ports_poll(micros);

	//t2 = apr_time_now();

	//if (t2 - t1 < micros)
	//	printf("ports_poll slept for less then requested: %ld instead of %ld\n",
	//		t2 - t1, micros);

	if (rs && !APR_STATUS_IS_TIMEUP(rs))
		return decipher_status(rs);
	result(A_TRUE);
	return AI_OK;
}

term_t bif_undefined_builtin0(process_t *ctx)
{
	return A_UNDEF;
}

// breakpoints, module x
term_t bif_b1_0(process_t *ctx)
{
	result(A_TRUE);
	return AI_OK;
}

term_t bif_b2_0(process_t *ctx)
{
	result(A_TRUE);
	return AI_OK;
}

term_t bif_b3_0(process_t *ctx)
{
	result(A_TRUE);
	return AI_OK;
}

term_t bif_b4_0(process_t *ctx)
{
	result(A_TRUE);
	return AI_OK;
}

term_t bif_delete_module1(term_t Mod, process_t *ctx)
{
	if (!is_atom(Mod))
		return A_BADARG;

	if (code_base_delete(proc_code_base(ctx), Mod))
		result(A_TRUE);
	else
		result(A_FALSE);

	return AI_OK;
}

term_t bif_purge1(term_t Mod, process_t *ctx)
{
	return A_NOT_IMPLEMENTED;
}

term_t bif_soft_purge1(term_t Mod, process_t *ctx)
{
	apr_uint32_t mod_index;

	if (!is_atom(Mod))
		return A_BADARG;

	mod_index = code_base_mod_index(proc_code_base(ctx), Mod, 1);
	if (mod_index == MOD_INDEX_NONE)
		result(A_FALSE);
	else
	{
		if (proc_is_lingering(mod_index))
			result(A_FALSE);

		code_base_purge(proc_code_base(ctx), mod_index);
		result(A_TRUE);
	}

	return AI_OK;
}

term_t bif_set_brk0_2(term_t Mod, term_t Off, process_t *ctx)
{
	apr_uint32_t mod_index;

	if (!is_atom(Mod) || !is_int(Off))
		return A_BADARG;

	mod_index = code_base_mod_index(proc_code_base(ctx), Mod, 0);
	if (mod_index == MOD_INDEX_NONE)
		result(A_FALSE);
	else
	{
		if (code_base_breakpoint_set(proc_code_base(ctx),
			mod_index, int_value2(Off)))
		  result(A_TRUE);
		else
		  result(A_FALSE);
	}

	return AI_OK;
}

term_t bif_unset_brk0_2(term_t Mod, term_t Off, process_t *ctx)
{
	apr_uint32_t mod_index;

	if (!is_atom(Mod) || !is_int(Off))
		return A_BADARG;

	mod_index = code_base_mod_index(proc_code_base(ctx), Mod, 0);
	if (mod_index == MOD_INDEX_NONE)
		result(A_FALSE);
	else
	{
		if (code_base_breakpoint_unset(proc_code_base(ctx),
			mod_index, int_value2(Off)))
		  result(A_TRUE);
		else
		  result(A_FALSE);
	}

	return AI_OK;
}

term_t bif_toggle_brk0_2(term_t Mod, term_t Off, process_t *ctx)
{
	apr_uint32_t mod_index;

	if (!is_atom(Mod) || !is_int(Off))
		return A_BADARG;

	mod_index = code_base_mod_index(proc_code_base(ctx), Mod, 0);
	if (mod_index == MOD_INDEX_NONE)
		result(A_ERROR);
	else
	{
		switch (code_base_breakpoint_toggle(proc_code_base(ctx),
			mod_index, int_value2(Off)))
		{
		case -1:
		  result(A_ERROR);
		  break;
		case 0:
		  result(A_FALSE);
		  break;
		case 1:
		  result(A_TRUE);
		  break;
		}
	}

	return AI_OK;
}

term_t bif_clear_brks0_0(process_t *ctx)
{
	int n = code_base_clear_breakpoints(proc_code_base(ctx));
	result(intnum(n));
	return AI_OK;
}

// EOF
