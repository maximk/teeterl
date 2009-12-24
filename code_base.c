//
//
//

#include "code_base.h"

#include <apr_hash.h>

#include "bif.h"
#include "atom_defs.h"
#include "mpi.h"
#include "list.h"

builtin_t builtins[] = {
#include "builtins.inc"
};

struct code_base_t
{
	apr_hash_t *bifs_by_mfn;		//key = mod:fun:arity
	apr_hash_t *modules;			//key = amod:is_old
};

code_base_t *code_base_make(apr_pool_t *pool)
{
	code_base_t *base = apr_palloc(pool, sizeof(*base));
	base->bifs_by_mfn = apr_hash_make(pool);
	base->modules = apr_hash_make(pool);
	return base;
}

module_t *code_base_lookup(code_base_t *self, term_t module)
{
	struct mod_key_t mkey;
	module_t *m;

	mkey.module = module;
	mkey.is_old = 0;
	m = apr_hash_get(self->modules, &mkey, sizeof(mkey));
	return m;
}

codel_t *module_lookup(module_t *m, term_t function, int arity)
{
	export_t *e;
	struct exp_key_t ekey;
	ekey.function = function;
	ekey.arity = arity;
	e = apr_hash_get(m->exports, &ekey, sizeof(ekey));
	if (e == 0)
		return 0;
	return e->entry;
}

bifN_t code_base_bif(code_base_t *self, term_t module, term_t function, int arity)
{
	//TODO
	return 0;
}

apr_array_header_t *source_files_names(term_t info, apr_pool_t *pool);
apr_array_header_t *source_line_blocks(term_t info, apr_pool_t *pool);

int code_base_load(code_base_t *self,
	term_t module, term_t exports, term_t fun_table, term_t attrs, term_t preloaded, term_t misc)
{
	module_t *m;
	apr_pool_t *pool;
	
	apr_pool_create(&pool, 0);
	m = apr_palloc(pool, sizeof(*m));
	m->mod_pool = pool;
	m->literals = heap_make(pool);
	m->key.module = module;
	m->key.is_old = 0;
	m->code_size = 0;
	m->code = 0;
	m->exports = apr_hash_make(pool);
	m->nfuns = 0;
	m->funs = 0;
	m->files = 0;
	m->source = 0;

	if (preloaded != nil)
	{
		int i;
		int n = list_length(preloaded);
		term_t cons = preloaded;
		int ok = 1;
		m->code = apr_palloc(pool, n*sizeof(codel_t));
		m->code_size = n;
		i = 0;
		while (ok && is_cons(cons))
		{
			term_box_t *cbox = peel(cons);
			if (is_int(cbox->cons.head))
			{
				m->code[i].i = int_value(cbox->cons.head);
			}
			else if (is_tuple(cbox->cons.head))
			{
				term_box_t *tbox = peel(cbox->cons.head);
				if (tbox->tuple.size == 2)
				{
					term_t selector = tbox->tuple.elts[0];
					term_t value = tbox->tuple.elts[1];
					switch (selector)
					{
					case AT__:		// {'@',Offset}
						m->code[i].l = m->code + int_value(value);
						break;
					case A_T:		// {t,Literal}
						m->code[i].t = heap_marshal(value, m->literals);
						break;
					case A_B:
						m->code[i].bif = builtins[int_value(value)].entry;
						break;
					case A_N:		// {n,{N,F}}
						m->code[i].t = tag_int(7);	//TODO
						break;
					default:
						ok = 0;
					}
				}
			}
			else if (is_bignum(cbox->cons.head))
			{
				mp_int mp = bignum_to_mp(cbox->cons.head);
                m->code[i].i = mp_get_int(&mp);
			}
			else
				ok = 0;

			i++;
			cons = cbox->cons.tail;
		}

		if (!ok)
		{
			apr_pool_destroy(pool);
			return 1;
		}
	}

	// misc:
	// source line info:
	// {file,Files}
	// {source,[{F,L,S,E}]}

	if (misc != nil)
	{
		term_t cons = misc;
		while (is_cons(cons))
		{
			term_box_t *cb = peel(cons);
			term_t t = cb->cons.head;
			if (is_tuple(t))
			{
				term_box_t *tb = peel(t);
				if (tb->tuple.size >= 2)
				{
					term_t selector = tb->tuple.elts[0];
					term_t info = tb->tuple.elts[1];
					switch (selector)
					{
					case A_FILES:
						m->files = source_files_names(info, pool);
						break;
					case A_SOURCE:
						m->source = source_line_blocks(info, pool);
						break;
					}
				}
			}
			cons = cb->cons.tail;
		}
	}

	if (fun_table != nil)
	{
		int i;
		int nfuns = list_length(fun_table);
		term_t cons = fun_table;
		int ok = 1;
		m->funs = apr_palloc(pool, nfuns*sizeof(fun_slot_t));
		m->nfuns = nfuns;
		for (i = 0; ok && i < nfuns; i++)
		{
			term_box_t *cbox = peel(cons);
			if (is_tuple(cbox->cons.head))
			{
				term_box_t *tbox = peel(cbox->cons.head);
				if (tbox->tuple.size == 2)
				{
					term_t uniq = tbox->tuple.elts[0];
					term_t offset = tbox->tuple.elts[1];
					if ((is_int(uniq) || is_bignum(uniq)) && is_int(offset))
					{
						fun_slot_t *slot = &m->funs[i];
						if (is_int(uniq))
							slot->uniq = int_value(uniq);
						else
						{
							mp_int mp = bignum_to_mp(uniq);
							slot->uniq = (uint)mp_get_int(&mp);
						}
						slot->entry = m->code + int_value(offset);
					}
					else
						ok = 0;

				}
				else
					ok = 0;
			}
			else
				ok = 0;

			cons = cbox->cons.tail;
		}

		if (!ok)
		{
			apr_pool_destroy(pool);
			return 1;
		}
	}

	//TODO: attrs ingnored

	if (exports != nil)
	{
		int ok = 1;
		term_t cons = exports;
		while (ok && is_cons(cons))
		{
			term_box_t *cbox = peel(cons);
			// {Function,Arity,Offset}
			if (is_tuple(cbox->cons.head))
			{
				term_box_t *tbox = peel(cbox->cons.head);
				if (tbox->tuple.size == 3)
				{
					term_t function = tbox->tuple.elts[0];
					term_t arity = tbox->tuple.elts[1];
					term_t offset = tbox->tuple.elts[2];
					if (is_atom(function) && is_int(arity) && is_int(offset))
					{
						export_t *exp = apr_palloc(pool, sizeof(*exp));
						exp->key.function = function;
						exp->key.arity = int_value(arity);
						exp->entry = m->code + int_value(offset);
						apr_hash_set(m->exports, &exp->key, sizeof(exp->key), exp);
					}
					else
						ok = 0;
				}
				else
					ok = 0;
			}
			else
				ok = 0;

			cons = cbox->cons.tail;
		}

		if (!ok)
		{
			apr_pool_destroy(pool);
			return 1;
		}
	}

	apr_hash_set(self->modules, &m->key, sizeof(m->key), m);
	return 0;
}

apr_array_header_t *source_files_names(term_t info, apr_pool_t *pool)
{
	apr_array_header_t *files = apr_array_make(pool, 1, sizeof(const char *));
	term_t cons = info;
	while (is_cons(cons))
	{
		term_box_t *cb = peel(cons);
		APR_ARRAY_PUSH(files, const char *) = ltoz(cb->cons.head, pool);
		cons = cb->cons.tail;
	}
	return files;
}

apr_array_header_t *source_line_blocks(term_t info, apr_pool_t *pool)
{
	apr_array_header_t *refs = apr_array_make(pool, 64, sizeof(source_ref_t));
	term_t cons = info;
	while (is_cons(cons))
	{
		term_box_t *cb = peel(cons);
		term_t t = cb->cons.head;
		//{F,L,S,E}
		if (is_tuple(t))
		{
			term_box_t *tb = peel(t);
			if (tb->tuple.size == 4)
			{
				source_ref_t *ref = &APR_ARRAY_PUSH(refs, source_ref_t);
				ref->file_index = int_value(tb->tuple.elts[0]);
				ref->source_line = int_value(tb->tuple.elts[1]);
				ref->off_starts = int_value(tb->tuple.elts[2]);
				ref->off_ends = int_value(tb->tuple.elts[3]);
			}
		}
		cons = cb->cons.tail;
	}
	return refs;
}

int code_base_delete(code_base_t *self, term_t module)
{
	//TODO
	return 0;
}

//int code_base_purge(code_base_t *self, uint mod_id)
//{
//	//TODO
//	return 0;
//}

int module_source_from_offset(module_t *m, int offset, const char **file, int *line)
{
	source_ref_t *ptr, *end;

	if (m->files == 0 || m->source == 0)
		return 0;

	ptr = (source_ref_t *)m->source->elts;
	end = ptr + m->source->nelts;

	while (ptr < end)
	{
		if (offset >= ptr->off_starts && offset < ptr->off_ends)
		{
			*file = APR_ARRAY_IDX(m->files, ptr->file_index, const char *);
			*line = ptr->source_line;
			return 1;
		}
		if (offset > ptr->off_ends)
			return 0;	// source is sorted
		ptr++;
	}
	return 0;	//not found
}

int module_offset_from_source(module_t *m, const char *file, int line, int *offset)
{
	source_ref_t *ptr, *end;
	int file_index = 0;

	if (m->files == 0 || m->source == 0)
		return 0;

	if (file != NULL)
	{
		int i;
		for (i = 0; i < m->files->nelts; i++)
		{
			// TODO: relax comparison
			if (strcmp(file, APR_ARRAY_IDX(m->files, i, const char *)) == 0)
			{
				file_index = i;
				break;
			}
		}
	}

	ptr = (source_ref_t *)m->source->elts;
	end = ptr + m->source->nelts;

	while (ptr < end)
	{
		if (ptr->file_index == file_index && ptr->source_line == line)
		{
			*offset = ptr->off_starts;
			return 1;
		}
		ptr++;
	}

	return 0;	//not found
}

//EOF
