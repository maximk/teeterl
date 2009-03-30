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

#include <apr_general.h>
#include <apr_tables.h>
#include <apr_hash.h>
#include <apr_time.h>

#include "proc.h"
#include "code_base.h"
#include "scheduler.h"
#include "msg_queue.h"
#include "bif.h"
#include "tcomp.h"
#include "bits.h"
#include "getput.h"
#include "errors.h"
#include "port.h"

#define DEBUG_STACKS

apr_hash_t *processes = 0;	//serial
apr_hash_t *registry = 0;	//registered_name
apr_uint32_t next_pid = 0;

typedef struct catch_t catch_t;
struct catch_t {
	int csp;
	int dsp;
	int ebp;
	apr_uint32_t mod_index;
	celem_t *ip;
};

struct process_t {
	apr_uint32_t serial;	//key
	term_t registered_name;

	code_base_t *base;
	atoms_t *atoms;

	apr_pool_t *pool;

	xpool_t *gc_a;
	xpool_t *gc_b;
	xpool_t *gc_cur;

	//process dictionary
	term_t dict;

	apr_array_header_t *dstack;	// term_t
	apr_array_header_t *cstack;	// term_t
	apr_uint32_t ebp;

	apr_array_header_t *catches; // catch_t
	term_t stack_trace;
	term_t locals_trace;

	//construction works
	bin_pad_t *bsite;
	term_t worm;
	int marker;

	celem_t *code;
	celem_t *ip;
	apr_uint32_t mod_index;

	msg_queue_t *mailbox;
	apr_time_t timeout;
	term_t new_mail;	//a flag is set if new mail is delivered

	term_t mod_path;	//XXX: obsolete, do not use
	term_t trap_exit;

	int stopped_on_breakpoint;
	apr_byte_t breakpoint_command;
};

static void proc_apply(process_t *proc, term_t mod, term_t fun, term_t args);
static void gc_copy_stack(apr_array_header_t *terms, xpool_t *xp);

// NB: no marshalling
term_t proc_get_info(process_t *proc, term_t what)
{
	if (what == A_MODULE)
		return code_base_mod_name(proc->base, proc->mod_index);
	else if (what == A_OFFSET)
		return intnum(proc->ip - proc->code);
	else if (what == A_HEAP_SIZE)
		return intnum((xpool_size(proc->gc_cur)+3)/4);
	else if (what == A_MESSAGE_QUEUE_LEN)
		return intnum(msg_queue_len(proc->mailbox));
	else if (what == A_MESSAGES)
		return msg_queue_to_term(proc->mailbox, proc->gc_cur);
	else if (what == A_NEW_MAIL)
		return proc->new_mail;
	else if (what == A_MOD_PATH)
		return proc->mod_path;
	else if (what == A_REGISTERED_NAME)
		return proc->registered_name;
	else if (what == A_TRAP_EXIT)
		return proc->trap_exit;
	else
		return AI_UNDEFINED;
}

// NB: no marshalling
term_t proc_set_flag(process_t *proc, term_t what, term_t value)
{
	term_t old_value = AI_UNDEFINED;
	if (what == A_NEW_MAIL)
	{
		old_value = proc->new_mail;
		proc->new_mail = value;
	}
	if (what == A_MOD_PATH)
	{
		old_value = proc->mod_path;
		proc->mod_path = value;
	}
	if (what == A_TRAP_EXIT)
	{
		old_value = proc->trap_exit;
		proc->trap_exit = value;
	}
	return old_value;
}

xpool_t *proc_gc_pool(process_t *self)
{
	return self->gc_cur;
}

code_base_t *proc_code_base(process_t *self)
{
	return self->base;
}

atoms_t *proc_atoms(process_t *self)
{
	return self->atoms;
}

apr_uint32_t proc_serial(process_t *self)
{
	return self->serial;
}

void proc_bif_result(process_t *self, term_t Result)
{
	*(term_t *)apr_array_push(self->dstack) = Result;
}

process_t *proc_lookup(apr_uint32_t serial)
{
	return apr_hash_get(processes, &serial, sizeof(serial));
}

void proc_destroy(process_t *proc)
{
	apr_hash_set(processes, &proc->serial, sizeof(proc->serial), 0);
	apr_pool_destroy(proc->pool);	//destroys gc pools too
}

process_t *proc_spawn(code_base_t *base,
	atoms_t *atoms,	term_t amod, term_t afun, term_t args)
{
	apr_pool_t *pool, *gc_pool;
	process_t *proc;
	term_t args1;

	apr_pool_create(&pool, 0);
	apr_pool_create(&gc_pool, pool);

	proc = apr_palloc(pool, sizeof(process_t));
	proc->serial = next_pid++;
	proc->registered_name = A_UNDEFINED;
	proc->base = base;
	proc->atoms = atoms;
	proc->pool = pool;

	proc->gc_a = xpool_make(pool);
	proc->gc_b = xpool_make(pool);
	proc->gc_cur = proc->gc_a;

	proc->dict = nil;
	proc->dstack = apr_array_make(pool, INIT_DSTACK_SIZE, sizeof(term_t));
	proc->cstack = apr_array_make(pool, INIT_CSTACK_SIZE, sizeof(term_t));
	proc->ebp = 0;
	proc->catches = apr_array_make(pool, 4, sizeof(catch_t));
	proc->stack_trace = nil;
	proc->locals_trace = nil;
	proc->bsite = 0;
	proc->worm = AI_UNDEFINED;
	proc->marker = 0;
	proc->mailbox = msg_queue_make(pool);
	proc->new_mail = A_FALSE;
	proc->mod_path = nil;
	proc->trap_exit = A_FALSE;

	proc->stopped_on_breakpoint;
	proc->breakpoint_command = 125;	// XXX: opcode for 'nop'

	proc->mod_index = MOD_INDEX_NONE;
	proc->code = 0;
	proc->ip = 0;

	args1 = marshal_term(args, proc->gc_cur);
	proc_apply(proc, amod, afun, args1);

	if (processes == 0)
	{
		apr_pool_t *p;
		apr_pool_create(&p, 0);
		processes = apr_hash_make(p);
	}

	apr_hash_set(processes,
		&proc->serial, sizeof(proc->serial), proc);

	return proc;
}

static void proc_apply(process_t *proc, term_t mod, term_t fun, term_t args)
{
	celem_t *ip;
	int arity = lst_len(args);

	term_t mdi = intnum(proc->mod_index);
	term_t off = intnum(proc->ip - proc->code);

	ip = code_base_lookup(proc->base,
		mod, fun, arity, &proc->mod_index, &proc->code);
	if (ip == 0)
	{
		*(term_t *)apr_array_push(proc->cstack) = args;
		*(term_t *)apr_array_push(proc->cstack) = fun;
		*(term_t *)apr_array_push(proc->cstack) = mod;

		ip = code_base_lookup(proc->base,
			A_ERROR_HANDLER, A_UNDEFINED_FUNCTION, 3, &proc->mod_index, &proc->code);
	}
	else
	{
		// pushing args in reverse order: kludgy

		term_t l;
		int i;
		for (i = 0; i < arity; i++)
			*(term_t *)apr_array_push(proc->cstack) = AI_UNDEFINED;

		l = args;
		i = 0;
		while (l != nil)
		{
			(((term_t *)proc->cstack->elts)[proc->cstack->nelts-i-1]) = lst_value(l);
			l = lst_next(l);
			i++;
		}
	}

	*(term_t *)apr_array_push(proc->cstack) = mdi;
	*(term_t *)apr_array_push(proc->cstack) = off;

	proc->ip = ip;
}

int proc_register(process_t *self, term_t name)
{
	if (self->registered_name != A_UNDEFINED)
		return 0;
	if (registry == 0)
	{
		apr_pool_t *pool;
		apr_pool_create(&pool, 0);
		registry = apr_hash_make(pool);
	}
	else
	{
		process_t *proc = apr_hash_get(registry, &name, sizeof(name));
		if (proc)
			return 0;
	}
	self->registered_name = name;
	apr_hash_set(registry, &self->registered_name, sizeof(self->registered_name), self);
	return 1;
}

process_t *proc_registered(term_t name)
{
	if (registry == 0)
		return 0;
	return apr_hash_get(registry, &name, sizeof(name));
}

int proc_unregister(term_t name)
{
	process_t *proc;
	if (registry == 0)
		return 0;
	proc = apr_hash_get(registry, &name, sizeof(name));
	if (proc == 0)
		return 0;

	apr_hash_set(registry, &proc->registered_name, sizeof(proc->registered_name), 0);
	proc->registered_name = A_UNDEFINED;

	return 1;
}

term_t proc_list_registered(xpool_t *xp)
{
	apr_hash_index_t *hi;

	term_t r = nil;
	term_t cons = nil;

	for (hi = apr_hash_first(0, registry); hi; hi = apr_hash_next(hi))
	{
		process_t *proc;
		apr_hash_this(hi, 0, 0, (void **)&proc);
		if (proc->registered_name != A_UNDEFINED)
		  lst_add(r, cons, proc->registered_name, xp);
	}

	return r;
}

void proc_new_mail(process_t *self, term_t msg)
{
	term_t copy = marshal_term(msg, self->gc_cur);
	msg_queue_push(self->mailbox, copy);
	self->new_mail = A_TRUE;
}

term_t proc_pid(process_t *self, xpool_t *xp)
{
	//return make_pid(my_node, self->serial, 0, xp);

	//pid is guaranteed to be local
	return localpid(self->serial);
}

void proc_run_gc(process_t *self)
{
	xpool_t *new_pool = (self->gc_cur == self->gc_a)
		?self->gc_b
		:self->gc_a;

	//process dictionary
	self->dict = gc_copy_term(self->dict, new_pool);

	//stack trace and locals trace
	self->stack_trace = gc_copy_term(self->stack_trace, new_pool);
	self->locals_trace = gc_copy_term(self->locals_trace, new_pool);

	//binary being dissected
	self->worm = gc_copy_term(self->worm, new_pool);

	//mailbox
	msg_queue_gc_copy(self->mailbox, new_pool);

	//dstack
	gc_copy_stack(self->dstack, new_pool);
	//cstack
	gc_copy_stack(self->cstack, new_pool);

	//module path
	self->mod_path = gc_copy_term(self->mod_path, new_pool);

	xpool_clear(self->gc_cur);
	self->gc_cur = new_pool;
}

//
// Stack frame layout:
//
//		argN
//		...
//		arg0
//		mod
//		off
//		saved_ebp
// ebp->var0
//		...
//		varM
// csp->

#define cstack(n) ((term_t *)self->cstack->elts)[n]

term_t proc_trace_stack(process_t *self)
{
	term_t r = nil;
	term_t cons = nil;
	int ebp = self->ebp;
	term_t mod, thrice, pair;
	int saved_ebp, i;

	term_t avs = nil; //args+vars
	term_t cons1 = nil;

	// arity of the current function is unknown
	// however, it is known that the arity is less
	// or equal to saved_ebp - cur_ebp - 3

	saved_ebp = int_value2(cstack(ebp-1));

	for (i = ebp-4; i >= saved_ebp; i--)
	{
		term_t av = cstack(i);
		lst_add(avs, cons1, av, self->gc_cur);
	}

	mod = code_base_mod_name(self->base, self->mod_index);
	thrice = make_tuple3(mod, intnum(self->ip - self->code), avs, self->gc_cur);
	lst_add(r, cons, thrice, self->gc_cur);

	while (1)
	{
		int ebp1 = int_value2(cstack(ebp-1));
		int offset = int_value2(cstack(ebp-2));
		int mod_index = int_value2(cstack(ebp-3));
		term_t cons = nil;

		if (mod_index == MOD_INDEX_NONE)
			break;

		mod = code_base_mod_name(self->base, mod_index);
		pair = make_tuple2(mod, intnum(offset), self->gc_cur);

		lst_add(r, cons, pair, self->gc_cur);

		ebp = ebp1;
	}

	self->stack_trace = r;
	return self->stack_trace;
}

term_t proc_trace_locals(process_t *self)
{
	term_t r = nil;
	term_t cons = nil;
	int i;

	for (i = self->ebp; i < self->cstack->nelts; i++)
	{
		term_t local = cstack(i);
		lst_add(r, cons, local, self->gc_cur);
	}

	self->locals_trace = r;
	return self->locals_trace;
}

term_t proc_get_stacktrace(process_t *self)
{
	return self->stack_trace;
}

term_t proc_get_locals(process_t *self)
{
	return self->locals_trace;
}

int proc_is_lingering(apr_uint32_t mod_index)
{
	apr_hash_index_t *hi;
	for (hi = apr_hash_first(0, processes); hi; hi = apr_hash_next(hi))
	{
		int ebp;
		process_t *self;	//call it self, for cstack to work :-)
		apr_hash_this(hi, 0, 0, (void **)&self);

		if (self->mod_index == mod_index)
			return 1;

		ebp = self->ebp;
		while (1)
		{
			int return_mod_index = int_value2(cstack(ebp-3));
			if (return_mod_index == MOD_INDEX_NONE)
				break;
			if (return_mod_index == mod_index)
				return 1;

			ebp = int_value2(cstack(ebp-1));
		}
	}

	return 0;
}

term_t proc_dict(process_t *self)
{
	return self->dict;
}

void proc_set_dict(process_t *self, term_t d)
{
	self->dict = d;
}

void proc_print_error(process_t *self, term_t e)
{
	term_t m = code_base_mod_name(self->base, self->mod_index);
	printf("*** error: %s at offset %d of module '%s'\n",
		stringify_term(e, self->atoms, self->pool),
		(int)(self->ip - self->code),
		stringify_term(m, self->atoms, self->pool));
}

void proc_print_init_exits_unexpectedly(process_t *self, term_t result, term_t retval)
{
	term_t m = code_base_mod_name(self->base, self->mod_index);
	printf("*** init exits unexpectedly: %s, %s at offset %d of module '%s'\n",
		stringify_term(result, self->atoms, self->pool),
		stringify_term(retval, self->atoms, self->pool),
		(int)(self->ip - self->code),
		stringify_term(m, self->atoms, self->pool));
}

static void gc_copy_stack(apr_array_header_t *terms, xpool_t *xp)
{
	term_t *ptr = (term_t *)terms->elts;
	term_t *end = ptr + terms->nelts;
	while (ptr < end)
	{
		term_t twin = gc_copy_term(*ptr, xp);
		*ptr = twin;
		ptr++;
	}
}

term_t bignum_to_term(bignum_t *a, xpool_t *xp)
{
	if (bn_size(a) == 0)
		return intnum(0);
	if (bn_sign(a) == 0 && bn_size(a) == 1 && a->digits[0] <= MAX_INT_VALUE)
		return intnum(a->digits[0]);
	if (bn_sign(a) == 1 && bn_size(a) == 1 && a->digits[0] <= MAX_INT_VALUE+1)
		return intnum(-(int_value_t)a->digits[0]);
	if (bn_sign(a) == 0 && bn_size(a) == 2)
	{
		apr_uint64_t v = (apr_uint64_t)a->digits[0] << 32 + a->digits[1];
		if (v <= MAX_INT_VALUE)
			return intnum(v);
	}
	if (bn_sign(a) == 1 && bn_size(a) == 2)
	{
		apr_uint64_t v = (apr_uint64_t)a->digits[0] << 32 + a->digits[1];
		if (v <= MAX_INT_VALUE+1)
			return intnum(-(int_value_t)v);
	}

	return bignum(a);
}

bignum_t *bignum_from_int_value(int_value_t v, xpool_t *xp)
{
	return bignum_from64((apr_int64_t)v, xp);
}

#define push(t)		*(term_t *)apr_array_push(ds) = (t)
#define pop()		(*(term_t *)apr_array_pop(ds))
#define top()		(((term_t *)ds->elts)[ds->nelts-1])
#define top1()		(((term_t *)ds->elts)[ds->nelts-2])

#define rpush(t)	*(term_t *)apr_array_push(cs) = (t)
#define rpop()		(*(term_t *)apr_array_pop(cs))
#define rtop()		(((term_t *)cs->elts)[cs->nelts-1])

//
// Stack frame layout:
//
//		argN
//		...
//		arg0
//		mod
//		off
//		saved_ebp
// ebp->var0
//		...
//		varM
// csp->

#define arg(n) (((term_t *)cs->elts)[proc->ebp-4-(n)])
#define var(n) (((term_t *)cs->elts)[proc->ebp+(n)])

#define exception(c,r) \
	do { \
		exc_class = (c); \
		reason = (r); \
		goto cellar; \
	} while (0)

#define bad_arg() exception(A_ERROR, A_BADARG)

term_t proc_main(process_t *proc, int reductions, term_t *retval)
{
	term_t exc_class;
	term_t reason;

	apr_array_header_t *cs = proc->cstack;
	apr_array_header_t *ds = proc->dstack;

	while (reductions > 0)
	{
		apr_byte_t opcode;

resume_after_catch:

		if (proc->stopped_on_breakpoint)
		{
			opcode = proc->breakpoint_command;
			proc->stopped_on_breakpoint = 0;
		}
		else
		{
			opcode = (apr_byte_t)*proc->ip++;
		} 

		switch (opcode)
		{
#include "run_cases.inc"
		default:
			fatal_err("Bad opcode");
		}
	}

	return AI_YIELD;

cellar:
	// before we forget many stack frames, should be done even outside catch
	proc_trace_stack(proc);
	proc_trace_locals(proc);

	if (proc->catches->nelts > 0)
	{
		catch_t *cat = ((catch_t *)proc->catches->elts)+ proc->catches->nelts-1;

		proc->cstack->nelts = cat->csp;
		proc->dstack->nelts = cat->dsp;
		proc->ebp = cat->ebp;

		proc->mod_index = cat->mod_index;
		proc->code = code_base_starts(proc->base, proc->mod_index);
		proc->ip = cat->ip;

		apr_array_pop(proc->catches);

		push(exc_class);
		push(reason);
		
		// NB: yielding now may cause a performance hit as compiler invisibly
		// generates helluva catches in guards, try to continue without yielding
		//return AI_YIELD;
		
		goto resume_after_catch;
	}
	else
	{
		*retval = reason;
		return exc_class;
	}
}

//EOF
