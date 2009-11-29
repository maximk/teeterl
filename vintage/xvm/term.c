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

#include "term.h"

#include <apr_strings.h>
#include <apr_tables.h>
#include <apr_hash.h>
#include <apr_lib.h>

#include "atom.h"

// to make local pids if possible
extern term_t my_node;
extern term_t my_prev_node;
extern apr_uint32_t my_creation;

static const char *quote_atom(cstr_t *print_name, apr_pool_t *pool);

term_t make_float(double d, xpool_t *xp)
{
	float_nest_t *dp = xalloc(xp, sizeof(*dp));
	dp->e_tag = ETAG_FLOAT;
	dp->value = d;
	return flonum(dp);
}

term_t make_tuple(int nelts, xpool_t *xp)
{
	tuple_t *tp = xalloc(xp, sizeof(tuple_t) + nelts * sizeof(term_t));
	tp->size = intnum(nelts);
	//values are undefined!
	return tuple(tp);
}

term_t make_tuple0(xpool_t *xp)
{
	tuple_t *tp = xalloc(xp, sizeof(tuple_t));
	tp->size = intnum(0);
	return tuple(tp);
}

term_t make_tuple1(term_t e1, xpool_t *xp)
{
	tuple_t *tp = xalloc(xp, sizeof(tuple_t) + sizeof(term_t));
	tp->size = intnum(1);
	tp->elts[0] = e1;
	return tuple(tp);
}

term_t make_tuple2(term_t e1, term_t e2, xpool_t *xp)
{
	tuple_t *tp = xalloc(xp, sizeof(tuple_t) + 2 * sizeof(term_t));
	tp->size = intnum(2);
	tp->elts[0] = e1;
	tp->elts[1] = e2;
	return tuple(tp);
}

term_t make_tuple3(term_t e1, term_t e2, term_t e3, xpool_t *xp)
{
	tuple_t *tp = xalloc(xp, sizeof(tuple_t) + 3 * sizeof(term_t));
	tp->size = intnum(3);
	tp->elts[0] = e1;
	tp->elts[1] = e2;
	tp->elts[2] = e3;
	return tuple(tp);
}

term_t make_tuple4(term_t e1, term_t e2, term_t e3, term_t e4, xpool_t *xp)
{
	tuple_t *tp = xalloc(xp, sizeof(tuple_t) + 4 * sizeof(term_t));
	tp->size = intnum(4);
	tp->elts[0] = e1;
	tp->elts[1] = e2;
	tp->elts[2] = e3;
	tp->elts[3] = e4;
	return tuple(tp);
}

term_t make_tuple5(term_t e1, term_t e2, term_t e3, term_t e4, term_t e5, xpool_t *xp)
{
	tuple_t *tp = xalloc(xp, sizeof(tuple_t) + 5 * sizeof(term_t));
	tp->size = intnum(5);
	tp->elts[0] = e1;
	tp->elts[1] = e2;
	tp->elts[2] = e3;
	tp->elts[3] = e4;
	tp->elts[4] = e5;
	return tuple(tp);
}

term_t make_tuple6(term_t e1, term_t e2, term_t e3, term_t e4, term_t e5, term_t e6, xpool_t *xp)
{
	tuple_t *tp = xalloc(xp, sizeof(tuple_t) + 6 * sizeof(term_t));
	tp->size = intnum(6);
	tp->elts[0] = e1;
	tp->elts[1] = e2;
	tp->elts[2] = e3;
	tp->elts[3] = e4;
	tp->elts[4] = e5;
	tp->elts[5] = e6;
	return tuple(tp);
}

term_t make_tuple7(term_t e1, term_t e2, term_t e3, term_t e4, term_t e5, term_t e6, term_t e7, xpool_t *xp)
{
	tuple_t *tp = xalloc(xp, sizeof(tuple_t) + 7 * sizeof(term_t));
	tp->size = intnum(7);
	tp->elts[0] = e1;
	tp->elts[1] = e2;
	tp->elts[2] = e3;
	tp->elts[3] = e4;
	tp->elts[4] = e5;
	tp->elts[5] = e6;
	tp->elts[6] = e7;
	return tuple(tp);
}

term_t make_tuple8(term_t e1, term_t e2, term_t e3, term_t e4, term_t e5, term_t e6, term_t e7, term_t e8, xpool_t *xp)
{
	tuple_t *tp = xalloc(xp, sizeof(tuple_t) + 8 * sizeof(term_t));
	tp->size = intnum(8);
	tp->elts[0] = e1;
	tp->elts[1] = e2;
	tp->elts[2] = e3;
	tp->elts[3] = e4;
	tp->elts[4] = e5;
	tp->elts[5] = e6;
	tp->elts[6] = e7;
	tp->elts[7] = e8;
	return tuple(tp);
}

term_t make_tuple_elts(int nelts, term_t *elts, xpool_t *xp)
{
	int i;
	tuple_t *tp = xalloc(xp, sizeof(tuple_t) + nelts * sizeof(term_t));
	tp->size = intnum(nelts);
	for (i = 0; i < nelts; i++)
		tp->elts[i] = elts[i];
	return tuple(tp);
}

term_t make_binary(term_t size, apr_byte_t *data, xpool_t *xp)
{
	binary_t *bp = xalloc(xp, sizeof(binary_t));
	bp->e_size = int_value2(size) | ETAG_BINARY;
	bp->data = data;
	return binary(bp);
}

term_t make_fun(term_t fridge, term_t amod, term_t afun, term_t arity, xpool_t *xp)
{
	fun_t *fp = xalloc(xp, sizeof(fun_t));
	fp->e_arity = int_value2(arity) | ETAG_FUN;
	fp->fridge = fridge;
	fp->amod = amod;
	fp->afun = afun;
	return fun(fp);
}

term_t make_list(term_t value, xpool_t *xp)
{
	cons_t *lp = xalloc(xp, sizeof(cons_t));
	lp->value = value;
	lp->next = nil;
	return list(lp);
}

term_t make_list2(term_t value, term_t next, xpool_t *xp)
{
	cons_t *lp = xalloc(xp, sizeof(cons_t));
	lp->value = value;
	lp->next = next;
	return list(lp);
}

term_t make_ref(term_t node, apr_uint32_t serial, apr_byte_t creation, xpool_t *xp)
{
	prp_t *pp = xalloc(xp, sizeof(prp_t));
	pp->e_serial = serial | ETAG_REF;
	pp->node = node;
	pp->creation = creation;
	return prp(pp);
}

term_t make_pid(term_t node, apr_uint32_t serial, apr_byte_t creation, xpool_t *xp)
{
	if ((node == my_node || node == my_prev_node) && creation == my_creation)
		return localpid(serial);
	else
	{
		prp_t *pp = xalloc(xp, sizeof(prp_t));
		pp->e_serial = serial | ETAG_PID;
		pp->node = node;
		pp->creation = creation;
		return prp(pp);
	}
}

term_t make_port(term_t node, apr_uint32_t serial, apr_byte_t creation, xpool_t *xp)
{
	prp_t *pp = xalloc(xp, sizeof(prp_t));
	pp->e_serial = serial | ETAG_PORT;
	pp->node = node;
	pp->creation = creation;
	return prp(pp);
}

term_t pin_term(term_t t)
{
	if (is_not_ptr(t) || is_pinned(t))
		return t;
	if (is_cons(t))
	{
		lst_value(t) = pin_term(lst_value(t));
		lst_next(t) = pin_term(lst_next(t));
	}
	if (is_tuple(t))
	{
		int i, arity = int_value2(tup_size(t));
		for (i = 0; i < arity; i++)
			tup_elts(t)[i] = pin_term(tup_elts(t)[i]);
	}
	if (is_fun(t))
	{
		//unlikely to happen
		fun_fridge(t) = pin_term(fun_fridge(t));
	}
	return pin(t);
}

term_t copy_list(term_t l, xpool_t *xp)
{
	term_t l2 = nil;
	term_t cons = nil;
	while (l != nil)
	{
		lst_add(l2, cons, lst_value(l), xp);
		l = lst_next(l);
	}
	return l2;
}

term_t marshal_term(term_t t, xpool_t *xp)
{
	term_t nt;

	if (is_not_ptr(t) || is_pinned(t))
		return t;

	if (is_float(t))
		nt = make_float(dbl_value(t), xp);
	else if (is_tuple(t))
	{
		int i;
		int n = int_value2(tup_size(t));
		term_t copy = make_tuple(n, xp);
		for (i = 0; i < n; i++)
			tup_elts(copy)[i] = marshal_term(tup_elts(t)[i], xp);
		nt = copy;
	}
	else if (is_binary(t))
	{
		int n = int_value2(bin_size(t));
		apr_byte_t *data = xalloc(xp, n);
		memcpy(data, bin_data(t), n);
		nt = make_binary(intnum(n), data, xp);
	}
	else if (is_fun(t))
	{
		nt = make_fun(marshal_term(fun_fridge(t), xp),
			fun_amod(t),
			fun_afun(t),
			fun_arity(t),
			xp);
	}
	else if (is_bignum(t))
	{
		bignum_t *bn = bignum_copy(bn_value(t), xp);
		nt = bignum(bn);
	}
	else if (is_cons(t))
	{
		//
		// NB: stop after pinned cons is seen: see gc_copy_term
		//
		// NB: stack overflow if the following is recursive
		//

		term_t first = nil;
		term_t last = nil;

		do {
			term_t v2 = marshal_term(lst_value(t), xp);
			lst_add(first, last, v2, xp);
			t = lst_next(t);
		} while (is_cons(t) && !is_pinned(t));


		// works for both pinned and wierd tails
		if (!is_nil(t))
			lst_next(last) = marshal_term(t, xp);

		nt = first;
	}
	else if (is_ref(t))
		nt = make_ref(prp_node(t), prp_serial(t), prp_creation(t), xp);
	else if (is_pid(t))
		nt = make_pid(pid_node(t), pid_serial(t), pid_creation(t), xp);
	else if (is_port(t))
		nt = make_port(prp_node(t), prp_serial(t), prp_creation(t), xp);
	else
		return AI_UNDEFINED; //never happens

	return nt;
}

// destructive term copy for gc
term_t gc_copy_term(term_t t, xpool_t *xp)
{
	term_t nt;

	if (is_not_ptr(t) || is_pinned(t))
		return t;

	if (is_grave(t))
		return grv_skel(t);

	if (is_float(t))
		nt = make_float(dbl_value(t), xp);
	else if (is_tuple(t))
	{
		int i;
		int n = int_value2(tup_size(t));
		term_t copy = make_tuple(n, xp);
		for (i = 0; i < n; i++)
			tup_elts(copy)[i] = gc_copy_term(tup_elts(t)[i], xp);
		nt = copy;
	}
	else if (is_binary(t))
	{
		int n = int_value2(bin_size(t));
		apr_byte_t *data = xalloc(xp, n);
		memcpy(data, bin_data(t), n);
		nt = make_binary(intnum(n), data, xp);
	}
	else if (is_fun(t))
	{
		nt = make_fun(gc_copy_term(fun_fridge(t), xp),
			fun_amod(t),
			fun_afun(t),
			fun_arity(t),
			xp);
	}
	else if (is_bignum(t))
	{
		bignum_t *bn = bignum_copy(bn_value(t), xp);
		nt = bignum(bn);
	}
	else if (is_cons(t))
	{
		term_t first = nil;
		term_t last = nil;

		do {
			term_t lot = t;
			term_t v2 = gc_copy_term(lst_value(t), xp);
			lst_add(first, last, v2, xp);
			t = lst_next(t);

			grv_cross(lot) = CROSS_MAGIC;
			grv_skel(lot) = last;

		} while (is_cons(t) && !is_pinned(t) && !is_grave(t));

		// works for both pinned and wierd tails
		if (!is_nil(t))
			lst_next(last) = gc_copy_term(t, xp);

		return first; // do not bury twice
	}
	else if (is_ref(t))
		nt = make_ref(prp_node(t), prp_serial(t), prp_creation(t), xp);
	else if (is_pid(t))
		nt = make_pid(pid_node(t), pid_serial(t), pid_creation(t), xp);
	else if (is_port(t))
		nt = make_port(prp_node(t), prp_serial(t), prp_creation(t), xp);
	else
		return AI_UNDEFINED; //never happens

	grv_cross(t) = CROSS_MAGIC;
	grv_skel(t) = nt;

	return nt;
}

int is_proper_list(term_t l)
{
	if (!is_list(l))
		return 0;
	while (is_cons(l))
		l = lst_next(l);
	return is_nil(l);
}

//helper: check whether list is made of printable chars
int printable_chars(term_t l)
{
	int n = 0;
	while (l != nil)
	{
		term_t v = lst_value(l);
		int ch;
		if (!is_int(v))
			return 0;
		ch = int_value2(v);
		if (ch < 32 || ch > 255)
			return 0;
		//if (!apr_isprint(int_value(v)))
		//	return 0;
		n++;

		l = lst_next(l);

		if (!is_list(l))	//wierd lists are never strings
			return 0;
	}
	return n;
}

int printable_chars2(apr_byte_t *buf, int len)
{
	int i;
	for (i = 0; i < len; i++)
		if (!apr_isprint(buf[i]))
			return 0;
	return 1;
}

const char *stringify_term(term_t t, atoms_t *atoms, apr_pool_t *pool)
{
	if (t == 0xcdcdcdcd)
		return "#UNINITIALIZED#";	//happens too many times to justify slight importability
	if (is_int(t))
		return apr_itoa(pool, int_value(t));
	else if (is_atom(t))
	{
		cstr_t *print_name;
		if (atoms == 0)
			return apr_psprintf(pool, "#%d", (int)index(t));

		print_name = atoms_get(atoms, index(t));
		if (print_name == 0)
			return apr_psprintf(pool, "#%d", (int)index(t));
		else
			return quote_atom(print_name, pool);
	}
	else if (is_float(t))
	{
		
		//TODO: the following three lines formatted to demonstrate wierd
		//behaviour of apr_psprintf which returns many zeroes from
		//apr_psprintf(pool, "%lf", 1.7e308). Is this MacOs specific?

		double d = dbl_value(t);
		const char *s = apr_psprintf(pool, "%lf", d);
		return s;
	}
	else if (is_tuple(t))
	{
		int i;
		int n = int_value2(tup_size(t));
		apr_array_header_t *bits = apr_array_make(pool, 8, sizeof(char *));
		*(char **)apr_array_push(bits) = "{";
		for (i = 0; i < n; i++)
		{
			*(const char **)apr_array_push(bits) =
				stringify_term(tup_elts(t)[i], atoms, pool);
			if (i < n-1)
				*(char **)apr_array_push(bits) = ",";
		}
		*(char **)apr_array_push(bits) = "}";
		return apr_array_pstrcat(pool, bits, 0);
	}
	else if (is_binary(t))
	{
		int i;
		int n = int_value2(bin_size(t));
		apr_array_header_t *bits = apr_array_make(pool, 8, sizeof(char *));
		*(char **)apr_array_push(bits) = "<<";
		if (printable_chars2(bin_data(t), n) && n >= 3)
		{
			*(char **)apr_array_push(bits) = "\"";
			*(char **)apr_array_push(bits) = apr_psprintf(pool, "%.*s", n, bin_data(t));
			*(char **)apr_array_push(bits) = "\"";
		}
		else
		{
			for (i = 0; i < n; i++)
			{
				*(char **)apr_array_push(bits) = apr_itoa(pool, bin_data(t)[i]);
				if (i < n-1)
					*(char **)apr_array_push(bits) = ",";
			}
		}
		*(char **)apr_array_push(bits) = ">>";
		return apr_array_pstrcat(pool, bits, 0);
	}
	else if (is_fun(t))
	{
		apr_array_header_t *bits = apr_array_make(pool, 8, sizeof(char *));
		*(char **)apr_array_push(bits) = "#fun<";
		*(char **)apr_array_push(bits) = apr_itoa(pool, int_value(fun_arity(t)));
		*(char **)apr_array_push(bits) = ",";
		if (fun_fridge(t) != atom(AI_UNDEFINED))
			*(char **)apr_array_push(bits) = (char *)stringify_term(fun_fridge(t), atoms, pool);
		else
			*(char **)apr_array_push(bits) = "no_fridge";		
		*(char **)apr_array_push(bits) = ">";
		return apr_array_pstrcat(pool, bits, 0);
	}
	else if (is_bignum(t))
		return bignum_to_str(bn_value(t), pool);
	else if (is_list(t))
	{
		int size;
		if (size = printable_chars(t))
		{
			int i = 1;
			char *buf = apr_palloc(pool, size+2+1);
			buf[0] = '"';
			while (t != nil)
			{
				buf[i++] = int_value(lst_value(t));
				t = lst_next(t);
			}
			buf[i++] = '"';
			buf[i++] = 0;
			return buf;
		}
		else
		{
			apr_array_header_t *bits = apr_array_make(pool, 8, sizeof(char *));
			*(char **)apr_array_push(bits) = "[";
			while (t != nil)
			{
				*(const char **)apr_array_push(bits) =
					stringify_term(lst_value(t), atoms, pool);
				t = lst_next(t);
				if (!is_list(t))
				{
					*(char **)apr_array_push(bits) = "|";
					*(char **)apr_array_push(bits) =
						(char *)stringify_term(t, atoms, pool);
					break;
				}
				else if (t != nil)
					*(char **)apr_array_push(bits) = ",";
			}
			*(char **)apr_array_push(bits) = "]";
			return apr_array_pstrcat(pool, bits, 0);
		}
	}
	else if (is_ref(t))
	{
		apr_array_header_t *bits = apr_array_make(pool, 3, sizeof(char *));
		*(char **)apr_array_push(bits) = "#Ref<";
		if (prp_node(t) != A_LOCAL)
		{
			*(char **)apr_array_push(bits) = (char *)stringify_term(prp_node(t), atoms, pool);
			*(char **)apr_array_push(bits) = ".";
		}
		*(char **)apr_array_push(bits) = apr_itoa(pool, prp_serial(t));
		*(char **)apr_array_push(bits) = ".";
		*(char **)apr_array_push(bits) = apr_ltoa(pool, prp_creation(t));
		*(char **)apr_array_push(bits) = ">";
		return apr_array_pstrcat(pool, bits, 0);
	}
	else if (is_pid(t))
	{
		apr_array_header_t *bits = apr_array_make(pool, 3, sizeof(char *));
		*(char **)apr_array_push(bits) = "<";
		if (pid_node(t) != A_LOCAL)
		{
			*(char **)apr_array_push(bits) = (char *)stringify_term(pid_node(t), atoms, pool);
			*(char **)apr_array_push(bits) = ".";
		}
		*(char **)apr_array_push(bits) = apr_itoa(pool, pid_serial(t));
		*(char **)apr_array_push(bits) = ".";
		*(char **)apr_array_push(bits) = apr_ltoa(pool, pid_creation(t));
		*(char **)apr_array_push(bits) = ">";
		return apr_array_pstrcat(pool, bits, 0);
	}
	else if (is_port(t))
	{
		apr_array_header_t *bits = apr_array_make(pool, 3, sizeof(char *));
		*(char **)apr_array_push(bits) = "#Port<";
		if (prp_node(t) != A_LOCAL)
		{
			*(char **)apr_array_push(bits) = (char *)stringify_term(prp_node(t), atoms, pool);
			*(char **)apr_array_push(bits) = ".";
		}
		*(char **)apr_array_push(bits) = apr_itoa(pool, prp_serial(t));
		*(char **)apr_array_push(bits) = ".";
		*(char **)apr_array_push(bits) = apr_ltoa(pool, prp_creation(t));
		*(char **)apr_array_push(bits) = ">";
		return apr_array_pstrcat(pool, bits, 0);
	}
	else
		return "$unknown$";
}

const char *ltoz(term_t l, apr_pool_t *pool)
{
	int n = 0;
	term_t x = l;
	char *buf, *p;

	while (x != nil)
	{
		n++;
		x = lst_next(x);
	}

	if (n == 0)
		return "";

	buf = apr_palloc(pool, n+1); //+1 for null byte
	p = buf;

	x = l;
	while (x != nil)
	{
		term_t v = lst_value(x);
		*p++ = int_value(v);
		x = lst_next(x);
	}
	buf[n] = 0;

	return buf;
}

term_t ztol(const char *z, xpool_t *xp)
{
	term_t l = nil;
	term_t cons = nil;
	while (*z)
		lst_add(l, cons, intnum(*z++), xp);
	return l;
}

int lst_len(term_t list)
{
	int len = 0;
	while (list != nil)
	{
		len++;
		list = lst_next(list);
	}
	return len;
}

static const char *quote_atom(cstr_t *print_name, apr_pool_t *pool)
{
	int quotes_needed = 0;
	char *buf, *p2;
	apr_byte_t *p;

	if (print_name->size == 0)
		quotes_needed = 1;
	else if ((print_name->data[0] >= 'A' && print_name->data[0] <= 'Z') || print_name->data[0] == '_')
		quotes_needed = 1;
	else
	{
		p = print_name->data;
		while (p < print_name->data + print_name->size)
		{
			if (!(*p >= 'a' && *p <= 'z') &&
				!(*p >= 'A' && *p <= 'Z') &&
				!(*p >= '0' && *p <= '9') &&
				!(*p == '_') && !(*p == '@'))
			{
				quotes_needed = 1;
				break;
			}
			p++;
		}
	}
	if (!quotes_needed)
		return stoz(print_name, pool);
	buf = apr_palloc(pool, print_name->size * 4 + 2 + 1);
	p2 = buf;
	p = print_name->data;
	*p2++ = '\'';
	while (p < print_name->data + print_name->size)
	{
		if (apr_isprint(*p))
		{
			*p2++ = *p++;
			continue;
		}
		switch (*p)
		{
		case '\b':
			*p2++ = '\\';
			*p2++ = 'b';
			break;
		case '\f':
			*p2++ = '\\';
			*p2++ = 'f';
			break;
		case '\n':
			*p2++ = '\\';
			*p2++ = 'n';
			break;
		case '\r':
			*p2++ = '\\';
			*p2++ = 'r';
			break;
		case '\t':
			*p2++ = '\\';
			*p2++ = 't';
			break;
		case '\v':
			*p2++ = '\\';
			*p2++ = 'v';
			break;
		case '\\':
			*p2++ = '\\';
			*p2++ = '\\';
			break;
		case '\'':
			*p2++ = '\\';
			*p2++ = '\'';
			break;
		case '"':
			*p2++ = '\\';
			*p2++ = '"';
			break;
		default:
			{
				unsigned char a, b, c;
				c = (*p) & 7;
				b = ((*p) >> 3) & 7;
				a = ((*p) >> 6) & 3;
				*p2++ = '\\';
				*p2++ = '0';
				*p2++ = a + '0';
				*p2++ = b + '0';
				*p2++ = c + '0';
			}
		}
		p++;
	}
	*p2++ = '\'';
	*p2++ = 0;
	return buf;
}

//EOF
