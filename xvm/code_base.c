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

#include "code_base.h"

#include <apr_general.h>
#include <apr_tables.h>
#include <apr_hash.h>

#include "cstr.h"
#include "atom.h"
#include "lit_pool.h"
#include "errors.h"

#define IMGTAB_TEXT		0xaa
#define IMGTAB_ATOMS	0xbb
#define IMGTAB_EXPORTS	0xcc

typedef struct builtin_t builtin_t;
struct builtin_t {
	struct bif_key_t {
		term_t mod;
		term_t fun;
		int arity;
	} key;

	bifN_t entry;
};

static apr_uint32_t next_mod_index = MOD_INDEX_STARTS;

static builtin_t builtins[] = {
#include "builtins.inc"
};

typedef struct export_t export_t;
struct export_t {
	struct exp_key_t {
		term_t afun;
		int arity;
	} key;

	celem_t *entry;		//loaded bytecode
};

typedef struct breakpoint_t breakpoint_t;
struct breakpoint_t {
	apr_uint32_t offset;
	apr_byte_t saved_command;
};	

typedef struct module_t module_t;
struct module_t {
	// each module has a separate pool which
	// is destroyed when the module is unloaded
	apr_pool_t *pool;
	xpool_t *xp;

	apr_uint32_t index;

	struct mod_key_t {
		term_t amod;	//TODO: rename to mod
		int is_old;
	} key;

	celem_t *code;

	apr_hash_t *exports; //key = afun:arity

	apr_hash_t *breakpoints; //key = offset
};

struct code_base_t {
	lit_pool_t *literals;
	builtin_t *builtins;
	apr_hash_t *bifs_by_mfn;		//key = mod:fun:arity
	apr_hash_t *modules;			//key = amod:is_old
	apr_hash_t *modules_by_index;	//key = index
	apr_uint32_t next_index;
};

code_base_t *code_base_make(apr_pool_t *pool)
{
	builtin_t *b;
	code_base_t *base = apr_palloc(pool, sizeof(code_base_t));
	base->literals = lit_pool_make(pool);
	base->builtins = builtins;
	
	base->bifs_by_mfn = apr_hash_make(pool);
	b = base->builtins;
	while (b->entry)
	{
		apr_hash_set(base->bifs_by_mfn, &b->key, sizeof(b->key), b);
		b++;
	}

	base->modules = apr_hash_make(pool);
	base->modules_by_index = apr_hash_make(pool);
	base->next_index = MOD_INDEX_STARTS;
	return base;
}

celem_t *code_base_lookup(code_base_t *self,
	term_t amod, term_t afun, apr_byte_t arity, apr_uint32_t *new_mod_index, celem_t **new_code)
{
	module_t *module;
	export_t *export_item;
	struct mod_key_t mod_key;
	struct exp_key_t exp_key;

	//*new_mod_index = MOD_INDEX_NONE;
	//*new_code = 0;

	mod_key.amod = amod;
	mod_key.is_old = 0;
	module = apr_hash_get(self->modules, &mod_key, sizeof(mod_key));

	if (module == 0)
		return 0;

	exp_key.afun = afun;
	exp_key.arity = arity;
	export_item = apr_hash_get(module->exports, &exp_key, sizeof(exp_key));

	if (export_item == 0)
		return 0;

	*new_mod_index = module->index;
	*new_code = module->code;
	return export_item->entry;
}

bifN_t code_base_bif(code_base_t *self, term_t mod, term_t fun, apr_byte_t arity)
{
	builtin_t *blt;
	struct bif_key_t key;
	key.mod = mod;
	key.fun = fun;
	key.arity = arity;

	blt = apr_hash_get(self->bifs_by_mfn, &key, sizeof(key));
	if (blt == 0)
		return 0;

	return blt->entry;
}

celem_t *code_base_starts2(code_base_t *self, term_t mod)
{
	module_t *m;
	struct mod_key_t key;
	key.amod = mod;
	key.is_old = 0;
	m = apr_hash_get(self->modules, &key, sizeof(key));
	if (m == 0)
		return 0;
	return m->code;
}

celem_t *code_base_starts(code_base_t *self, apr_uint32_t index)
{
	module_t *m = apr_hash_get(self->modules_by_index, &index, sizeof(index));
	if (m == 0)
		return 0;
	return m->code;
}

int code_base_load2(code_base_t *self, term_t code)
{
	//bin = {x,Ver,Mod,Exps,Attrs,Preloaded,Misc}
	if (!is_tuple(code) || !tup_size(code) == intnum(7))
		return 0;
	return code_base_load(self,
		tup_elts(code)[2],	//amod
		tup_elts(code)[3],	//exports
		tup_elts(code)[5]);	//preloaded
	//TODO: Attrs are ignored
}

int code_base_load(code_base_t *self,
	term_t amod, term_t exports, term_t preloaded)
{
	apr_pool_t *pool;
	xpool_t *xp;
	celem_t *code = 0;
	celem_t *ip;
	int len = lst_len(preloaded);
	apr_hash_t *e;
	module_t *m;
	term_t l;

	//TODO: check for existing version of the module

	apr_pool_create(&pool, 0);
	xp = xpool_make(pool);

	l = preloaded;
	code = apr_palloc(pool, sizeof(celem_t)*len);
	ip = code;
	while (l != nil)
	{
		term_t v = lst_value(l);
		if (is_int(v))
			*ip++ = (celem_t)int_value(v);
		else if (is_tuple(v) && tup_size(v) == intnum(2))
		{
			term_t selector = tup_elts(v)[0];
			term_t w = tup_elts(v)[1];
			if (selector == A_ATOM && is_atom(w))
				*ip++ = (celem_t)w;
			else if (selector == A_TERM)
			{
				//lit_pool enabled
				*ip++ = (celem_t)lit_pool_store(self->literals, w);
				//*ip++ = marshal_term(w, xp);
			}
			else if (selector == A_OFF && is_int(w))
			{
				int off = int_value(w);
				*ip++ = (celem_t)(code + off);
			}
			else if (selector == A_BIF && is_tuple(w) && tup_size(w) == intnum(3))
			{
				term_t m = tup_elts(w)[0];
				term_t f = tup_elts(w)[1];
				term_t n = tup_elts(w)[2];
				bifN_t entry = code_base_bif(self, m, f, int_value(n));
				if (entry == 0)
					entry = code_base_bif(self, A_CODE, A_UNDEFINED_BUILTIN, 0);
				if (entry == 0)
					return 0;
				*ip++ = (celem_t)entry;
			}
			else
				return 0; //unkown selector
		}
		l = lst_next(l);
	}

	e = apr_hash_make(pool);
	l = exports;
	while (l != nil)
	{
		export_t *exp;

		term_t v = lst_value(l);
		term_t fa, f, a, offset;

		assert(is_tuple(v));
		assert(tup_size(v) == intnum(2));

		fa = tup_elts(v)[0];
		offset = tup_elts(v)[1];

		assert(is_tuple(fa));
		assert(tup_size(fa) == intnum(2));

		f = tup_elts(fa)[0];
		a = tup_elts(fa)[1];

		exp = apr_palloc(pool, sizeof(export_t));
		exp->key.afun = f;
		exp->key.arity = int_value(a);
		exp->entry = (code + int_value(offset));

		apr_hash_set(e, &exp->key, sizeof(exp->key), exp);

		l = lst_next(l);
	}

	m = apr_palloc(pool, sizeof(module_t));
	m->pool = pool;
	m->xp = xp;
	m->key.amod = amod;
	m->key.is_old = 0;
	m->index = next_mod_index++;
	m->code = code;
	m->exports = e;
	m->breakpoints = apr_hash_make(pool);

	apr_hash_set(self->modules, &m->key, sizeof(m->key), m);
	apr_hash_set(self->modules_by_index, &m->index, sizeof(m->index), m);
	return 1;
}

term_t code_base_mod_name(code_base_t *self, apr_uint32_t mod_index)
{
	module_t *m;
	
	m = apr_hash_get(self->modules_by_index, &mod_index, sizeof(mod_index));
	if (m == 0)
		return AI_UNDEFINED;
	else
		return m->key.amod;
}

apr_uint32_t code_base_mod_index(code_base_t *self, term_t amod, int is_old)
{
	module_t *m;
	struct mod_key_t key;

	key.amod = amod;
	key.is_old = is_old;
	m = apr_hash_get(self->modules, &key, sizeof(key));
	if (m == 0)
		return MOD_INDEX_NONE;
	return m->index;
}

term_t code_base_list_modules(code_base_t *self, xpool_t *xp)
{
	apr_hash_index_t *hi;
	term_t l = nil;
	term_t cons = nil;

	for (hi = apr_hash_first(0, self->modules); hi; hi = apr_hash_next(hi))
	{
		module_t *m;
		apr_hash_this(hi, 0, 0, (void **)&m);

		if (!m->key.is_old)
			lst_add(l, cons, m->key.amod, xp);
	}

	return l;
}

int code_base_delete(code_base_t *self, term_t amod)
{
	module_t *m;
	struct mod_key_t mod_key;
	
	mod_key.amod = amod;
	mod_key.is_old = 1;
	m = apr_hash_get(self->modules, &mod_key, sizeof(mod_key));
	if (m)
		return 0;

	mod_key.amod = amod;
	mod_key.is_old = 0;
	m = apr_hash_get(self->modules, &mod_key, sizeof(mod_key));
	if (m == 0)
		return 0;

	//resinsert the module under different key
	apr_hash_set(self->modules, &mod_key, sizeof(mod_key), 0);
	m->key.is_old = 1;
	apr_hash_set(self->modules, &m->key, sizeof(m->key), m);
	return 1;
}

int code_base_purge(code_base_t *self, apr_uint32_t mod_index)
{
	module_t *m = apr_hash_get(self->modules_by_index, &mod_index, sizeof(mod_index));
	if (m == 0)
		return 0;

	apr_hash_set(self->modules, &m->key, sizeof(m->key), 0);
	apr_hash_set(self->modules_by_index, &mod_index, sizeof(mod_index), 0);

	apr_pool_destroy(m->pool);
	return 1;
}

int code_base_breakpoint_set(code_base_t *self, apr_uint32_t mod_index, apr_uint32_t offset)
{
	breakpoint_t *b;
	module_t *m = apr_hash_get(self->modules_by_index, &mod_index, sizeof(mod_index));
	if (m == 0)
		return 0;

	b = apr_hash_get(m->breakpoints, &offset, sizeof(offset));
	if (b)
	  return 1;	//breakpoint already set

	b = xalloc(m->xp, sizeof(*b));
	b->offset = offset;
	b->saved_command = m->code[offset];

	apr_hash_set(m->breakpoints, &b->offset, sizeof(b->offset), b);

	m->code[offset] = 126;	// XXX: code for 'break'
	return 1;
}

int code_base_breakpoint_unset(code_base_t *self, apr_uint32_t mod_index, apr_uint32_t offset)
{
	breakpoint_t *b;
	module_t *m = apr_hash_get(self->modules_by_index, &mod_index, sizeof(mod_index));
	if (m == 0)
		return 0;

	b = apr_hash_get(m->breakpoints, &offset, sizeof(offset));
	if (b == 0)
	  return 0;	//no such breakpoint

	m->code[b->offset] = b->saved_command;

	apr_hash_set(m->breakpoints, &offset, sizeof(offset), 0);
	return 1;
}

int code_base_breakpoint_toggle(code_base_t *self, apr_uint32_t mod_index, apr_uint32_t offset)
{
	breakpoint_t *b;
	module_t *m = apr_hash_get(self->modules_by_index, &mod_index, sizeof(mod_index));
	if (m == 0)
		return -1;

	b = apr_hash_get(m->breakpoints, &offset, sizeof(offset));
	if (b == 0)
	{
		//no breakpoint yet -> set
		b = xalloc(m->xp, sizeof(*b));
		b->offset = offset;
		b->saved_command = m->code[offset];

		apr_hash_set(m->breakpoints, &b->offset, sizeof(b->offset), b);

		m->code[offset] = 126;	// XXX: code for 'break'
		return 1;
	}
	else
	{
		//breakpoint is there -> unset
		m->code[b->offset] = b->saved_command;
		apr_hash_set(m->breakpoints, &offset, sizeof(offset), 0);
		return 0;
	}
}

int code_base_clear_breakpoints(code_base_t *self)
{
	apr_hash_index_t *hi;
	int cleared = 0;

	for (hi = apr_hash_first(0, self->modules); hi; hi = apr_hash_next(hi))
	{
		apr_hash_index_t *hi2;

		module_t *m;
		apr_hash_this(hi, 0, 0, (void **)&m);
		
		for (hi2 = apr_hash_first(m->pool, m->breakpoints); hi2; hi2 = apr_hash_next(hi2))
		{
			breakpoint_t *b;
			apr_hash_this(hi2, 0, 0, (void **)&b);

			m->code[b->offset] = b->saved_command;
			cleared++;
		}

		apr_hash_clear(m->breakpoints);
	}

	return cleared;
}

apr_byte_t code_base_breakpoint_command(code_base_t *self, apr_uint32_t mod_index, apr_uint32_t offset)
{
	breakpoint_t *b;
	module_t *m = apr_hash_get(self->modules_by_index, &mod_index, sizeof(mod_index));
	if (m == 0)
		return 255;

	b = apr_hash_get(m->breakpoints, &offset, sizeof(offset));
	if (b == 0)
	  return 255;	//no such breakpoint

	return b->saved_command;
}

//EOF
