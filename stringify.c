/**
 *
 *
 *
 */
 
#include <apr_strings.h>

#include "term.h"
#include "atom.h"
#include "binary.h"

#include "cstr.h"
#include "heap.h"
#include "mpi.h"

static const char *stringify_term2(term_t t, atoms_t *atoms, apr_pool_t *pool, heap_t *hp);
static int printable_chars(term_t l);
static const char *quote_atom(cstr_t *print_name, apr_pool_t *pool, heap_t *hp);

const char *stringify_term(term_t t, atoms_t *atoms, apr_pool_t *pool)
{
	heap_t *hp = heap_make(pool);	// tmp heap needed for bignums
	return stringify_term2(t, atoms, pool, hp);
}

static const char *stringify_term2(term_t t, atoms_t *atoms, apr_pool_t *pool, heap_t *hp)
{
	if (is_int(t))
		return apr_itoa(pool, int_value(t));
	else if (is_atom(t))
	{
		cstr_t *print_name;
		if (atoms == 0)
			return apr_psprintf(pool, "#%d", (int)atom_index(t));

		print_name = atoms_get(atoms, atom_index(t));
		if (print_name == 0)
			return apr_psprintf(pool, "#%d", (int)atom_index(t));
		else
			return quote_atom(print_name, pool, hp);
	}
	else if (is_float(t))
	{
		
		//TODO: the following three lines formatted to demonstrate wierd
		//behaviour of apr_psprintf which returns many zeroes from
		//apr_psprintf(pool, "%lf", 1.7e308). Is this MacOs specific?

		double d = float_value(t);
		const char *s = apr_psprintf(pool, "%lf", d);
		return s;
	}
	else if (is_tuple(t))
	{
		term_box_t *tb = peel(t);
		int i;
		apr_array_header_t *bits = apr_array_make(pool, 8, sizeof(char *));
		*(char **)apr_array_push(bits) = "{";
		for (i = 0; i < tb->tuple.size; i++)
		{
			*(const char **)apr_array_push(bits) =
				stringify_term(tb->tuple.elts[i], atoms, pool);
			if (i < tb->tuple.size-1)
				*(char **)apr_array_push(bits) = ",";
		}
		*(char **)apr_array_push(bits) = "}";
		return apr_array_pstrcat(pool, bits, 0);
	}
	else if (is_binary(t))
	{
		term_box_t *tb = peel(t);
		int i;
		int n = BIN_WHOLE_BYTES(tb->binary.bit_size);
		if (n > 8)
			n = 8;
		apr_array_header_t *bits = apr_array_make(pool, 8, sizeof(char *));
		*(char **)apr_array_push(bits) = "<<";
		for (i = 0; i < n; i++)
		{
			*(char **)apr_array_push(bits) = apr_itoa(pool, tb->binary.data[i]);
			if (i < n-1)
				*(char **)apr_array_push(bits) = ",";
		}
		*(char **)apr_array_push(bits) = apr_psprintf(pool, " %d bit(s) long >>", tb->binary.bit_size);
		return apr_array_pstrcat(pool, bits, 0);
	}
	else if (is_fun(t))
	{
		return "fun<>";
	}
	else if (is_bignum(t))
	{
		bignum_t *bn = &peel(t)->bignum;
		int size = mp_radix_size(&bn, 10);
		apr_byte_t *buf = apr_palloc(pool, size);
		mp_toradix(&bn, buf, 10, hp);
		return (char *)buf;
	}
	else if (is_list(t))
	{
		int size;
		if ((size = printable_chars(t)) > 0)
		{
			int i = 1;
			char *buf = apr_palloc(pool, size+2+1);
			buf[0] = '"';
			while (t != nil)
			{
				term_box_t *tb = peel(t);
				int ch = int_value(tb->cons.head);
				if (ch == 10)
				{
					buf[i++] = '\\';
					buf[i++] = 'n';
				}
				else
					buf[i++] = ch;
				t = tb->cons.tail;
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
				term_box_t *tb = peel(t);
				*(const char **)apr_array_push(bits) =
					stringify_term(tb->cons.head, atoms, pool);
				t = tb->cons.tail;
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
	else if (is_pid(t))
	{
		return apr_psprintf(pool, "<%d.0>", pid_serial(t));
	}
	else if (is_oid(t))
	{
		return apr_psprintf(pool, "out<%d.0>", oid_serial(t));
	}
	else
		return "$unknown$";
}

//helper: check whether list is made of printable chars
static int printable_chars(term_t l)
{
	int n = 0;
	while (l != nil)
	{
		term_box_t *tb = peel(l);
		if (!is_int(tb->cons.head))
			return 0;
		int ch = int_value(tb->cons.head);
		if (ch == 10)
			n++;
		else if (ch < 32 || ch > 255)
			return 0;
		n++;

		l = tb->cons.tail;
		if (!is_list(l))	//wierd lists are never strings
			return 0;
	}
	return n;
}

static const char *quote_atom(cstr_t *print_name, apr_pool_t *pool, heap_t *hp)
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
		return stoz(print_name, hp);
	buf = apr_palloc(pool, print_name->size * 4 + 2 + 1);
	p2 = buf;
	p = print_name->data;
	*p2++ = '\'';
	while (p < print_name->data + print_name->size)
	{
		if (*p < 32 || *p > 255)
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

/*EOF*/


