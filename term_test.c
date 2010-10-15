//
//
//

#include <apr_general.h>
#include <apr_strings.h>
#include <apr_tables.h>

#include "teeterl.h"
#include "term.h"
#include "atom.h"
#include "heap.h"
#include "binary.h"
#include "list.h"
#include "navel.h"

#include <stdlib.h>

#define T2H_MAX_SIMPLE_LIST		10
#define T2H_MAX_SIMPLE_TUPLE	5

const char *term_test_xml1(navel_context_t *cont, apr_pool_t *pool);

const char *term_test_handler(char *request, navel_context_t *cont, apr_pool_t *rp)
{
	if (strcmp(request, "/xml1") == 0)
		return term_test_xml1(cont, rp);
	else
		return "Unknown term_test request";
}

const char *term_test_xml1(navel_context_t *cont, apr_pool_t *pool)
{
	return "term/xml1: not implemented";
}

int list_elements_are_all_small(term_t cons)
{
	while (is_cons(cons))
	{
		term_box_t *cb = peel(cons);
		term_t v = cb->cons.head;
		if (!is_immed(v) && !is_float(v) && !is_fun(v) && !is_long_pid(v))
			return 0;
		cons = cb->cons.tail;
	}
	return (is_immed(cons) || is_float(cons) || is_fun(cons) || is_long_pid(cons));
}

int tuple_elements_are_all_small(term_t tuple)
{
	int i;
	term_box_t *tb = peel(tuple);
	for (i = 0; i < tb->tuple.size; i++)
	{
		term_t e = tb->tuple.elts[i];
		if (!is_immed(e) && !is_float(e) && !is_fun(e) && !is_long_pid(e))
			return 0;
	}
	return 1;
}

const char *term2html(term_t t, atoms_t *atoms, apr_pool_t *pool)
{
	// list - tuple - binary - fun - bignum - pid - float

	if (t == noval)
		return "<div class='tee-noval'>#noval#</div>";
	if (is_nil(t))
		return "<div class='tee-nil'>[]</div>";
	if (is_int(t))
		return apr_psprintf(pool, "<div class='tee-integer'>%d</div>", int_value(t));
	if (is_atom(t) && atoms != NULL)
	{
		cstr_t *s = atoms_get(atoms, atom_index(t));
		return apr_psprintf(pool, "<div class='tee-atom'>%.*s</div>", s->size, s->data);
	}
	if (is_atom(t))
		return apr_psprintf(pool, "<div class='tee-atom'>#%d</div>", atom_index(t));
	if (is_short_pid(t))
		return apr_psprintf(pool, "<div class='tee-pid'>&lt;local.%d&gt;</div>", pid_serial(t));
	if (is_short_oid(t))
		return apr_psprintf(pool, "<div class='tee-oid'>#outlet&lt;local.%d&gt;</div>", oid_serial(t));
	if (is_list(t))
	{
		apr_array_header_t *bs = apr_array_make(pool, 8, sizeof(char *));

		if (list_elements_are_all_small(t) && list_length(t) <= T2H_MAX_SIMPLE_LIST)
		{
			APR_ARRAY_PUSH(bs, const char *) = "<div class='tee-list'>[";
			while (is_cons(t))
			{
				term_box_t *box = peel(t);
				APR_ARRAY_PUSH(bs, const char *) = term2html(box->cons.head, atoms, pool);
				t = box->cons.tail;
				if (!is_list(t))
				{
					APR_ARRAY_PUSH(bs, const char *) = "|";
					APR_ARRAY_PUSH(bs, const char *) = term2html(t, atoms, pool);
					break;
				}
				else if (is_cons(t))
					APR_ARRAY_PUSH(bs, const char *) = ",";
			}
			APR_ARRAY_PUSH(bs, const char *) = "]</div>";
			return apr_array_pstrcat(pool, bs, 0);
		}
		else	// leading/body/trailing list
		{
			APR_ARRAY_PUSH(bs, const char *) = "<div class='tee-list'>";
			APR_ARRAY_PUSH(bs, const char *) = "<div class='tee-list-leading'>[</div>";
			APR_ARRAY_PUSH(bs, const char *) = "<div class='tee-list-body'>";
			while (is_cons(t))
			{
				term_box_t *box = peel(t);
				t = box->cons.tail;

				APR_ARRAY_PUSH(bs, const char *) = "<div class='tee-list-elem'>";
				APR_ARRAY_PUSH(bs, const char *) = term2html(box->cons.head, atoms, pool);
				if (is_cons(t))
					APR_ARRAY_PUSH(bs, const char *) = ",";
				APR_ARRAY_PUSH(bs, const char *) = "</div>";

				if (!is_list(t))
				{
					APR_ARRAY_PUSH(bs, const char *) = "<div class='tee-list-trailing'>|";
					APR_ARRAY_PUSH(bs, const char *) = term2html(t, atoms, pool);
					APR_ARRAY_PUSH(bs, const char *) = "</div>";
					break;
				}
			}
			APR_ARRAY_PUSH(bs, const char *) = "]</div></div>";
			return apr_array_pstrcat(pool, bs, 0);
		}
	}
	if (is_tuple(t))
	{
		term_box_t *tb = peel(t);
		apr_array_header_t *bs = apr_array_make(pool, 8, sizeof(char *));

		if (tuple_elements_are_all_small(t) && tb->tuple.size <= T2H_MAX_SIMPLE_TUPLE)
		{
			int i;
			APR_ARRAY_PUSH(bs, const char *) = "<div class='tee-tuple'>{";
			for (i = 0; i < tb->tuple.size; i++)
			{
				APR_ARRAY_PUSH(bs, const char *) = term2html(tb->tuple.elts[i], atoms, pool);
				if (i < tb->tuple.size-1)
					APR_ARRAY_PUSH(bs, const char *) = ",";
			}
			APR_ARRAY_PUSH(bs, const char *) = "}</div>";
			return apr_array_pstrcat(pool, bs, 0);
		}
		else // leading/body/trailing tuple
		{
			int i;
			term_t leading = tb->tuple.elts[0];

			APR_ARRAY_PUSH(bs, const char *) = "<div class='tee-tuple'>";
			APR_ARRAY_PUSH(bs, const char *) = "<div class='tee-tuple-leading'>{";
			APR_ARRAY_PUSH(bs, const char *) = term2html(leading, atoms, pool);
			APR_ARRAY_PUSH(bs, const char *) = ",</div>";

			APR_ARRAY_PUSH(bs, const char *) = "<div class='tee-tuple-body'>";
			for (i = 1; i < tb->tuple.size; i++)
			{
				APR_ARRAY_PUSH(bs, const char *) = "<div class='tee-tuple-elem'>";
				APR_ARRAY_PUSH(bs, const char *) = term2html(tb->tuple.elts[i], atoms, pool);
				if (i < tb->tuple.size-1)
					APR_ARRAY_PUSH(bs, const char *) = ",";
				APR_ARRAY_PUSH(bs, const char *) = "</div>";
			}
			APR_ARRAY_PUSH(bs, const char *) = "}</div></div>";
			return apr_array_pstrcat(pool, bs, 0);
		}
	}
	if (is_float(t))
	{
		return apr_psprintf(pool, "<div class='tee-float'>%f</div>", float_value(t));
	}
	if (is_bignum(t))
	{
		return "<div class='tee-integer'>#bignum#</div>";
	}
	if (is_binary(t))
	{
		term_box_t *box = peel(t);
		int bit_size = box->binary.bit_size;
		apr_byte_t *data = box->binary.data;
		const char *s, *e = "", *f = "";
		int trailer_size = BIN_TRAILER_SIZE(bit_size);

		switch (BIN_WHOLE_BYTES(bit_size))
		{
		case 0: s = ""; break;
		case 1: s = apr_psprintf(pool, "%d", data[0]); break;
		case 2: s = apr_psprintf(pool, "%d,%d", data[0], data[1]); break;
		case 3: s = apr_psprintf(pool, "%d,%d,%d", data[0], data[1], data[2]); break;
		case 4: s = apr_psprintf(pool, "%d,%d,%d,%d", data[0], data[1], data[2], data[3]); break;
		default: s = apr_psprintf(pool, "%d,%d,%d,%d...", data[0], data[1], data[2], data[3]); break;
		}

		if (bit_size < 40 && trailer_size != 0)
		{
			int byte_size = BIN_BYTE_SIZE(bit_size);
			int trailer = data[byte_size-1] >> (8-trailer_size);
			e = apr_psprintf(pool, "%s%d:%d",
				(bit_size>8) ?"," :"",
				trailer,
				trailer_size);
		}

		if (box->binary.parent != noval)
			f = apr_psprintf(pool, ", shared (offset=%d)", box->binary.offset);

		return apr_psprintf(pool,
			"<div class='tee-binary'>&lt;&lt;%s%s&gt;&gt;, %d bit(s) long%s</div>", s, e, bit_size, f);
	}
	if (is_fun(t))
	{
		return "<div class='tee-fun'>#fun#</div>";
	}

	assert(is_long_pid(t));

	return "<div class='tee-pid'>#pid#</div>";
}

//EOF
