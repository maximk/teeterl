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

#include "exterm.h"

#include "tcomp.h"
#include "atom.h"
#include "getput.h"
#include "errors.h"

#define APR_WANT_STDIO
#include <apr_want.h>

#include <apr_strings.h>
#include <apr_lib.h>

// to make local pids if possible
extern term_t my_node;
extern apr_uint32_t my_creation;

#define put(b) *(apr_byte_t *)apr_array_push(pad) = (apr_byte_t)(b)
#define put32(i) \
	do { \
		put((i) >> 24); \
		put((i) >> 16); \
		put((i) >> 8); \
		put((i)); \
	} while (0)
#define put32lsb(i) \
	do { \
		put((i)); \
		put((i) >> 8); \
		put((i) >> 16); \
		put((i) >> 24); \
	} while (0)
#define put16(i) \
	do { \
		put((i) >> 8); \
		put((i)); \
	} while (0)
#define put16lsb(i) \
	do { \
		put((i)); \
		put((i) >> 8); \
	} while (0)
#define put_data(data, size) \
	do { \
		int i; \
		for (i = 0; i < (size); i++) \
			put((data)[i]); \
	} while (0)

#define get(buf)	(buf->size--, *buf->data++)

typedef struct buf_t buf_t;
struct buf_t {
	apr_byte_t *data;
	int size;
};

static apr_uint32_t hash_atom(cstr_t *s, int max_val);

static apr_uint32_t get32(buf_t *buf);
static apr_uint16_t get16(buf_t *buf);
static apr_uint32_t get32lsb(buf_t *buf);
static apr_uint16_t get16lsb(buf_t *buf);

static void pack(apr_array_header_t *pad, term_t t, atom_cache_t *cache, atoms_t *atoms);
static term_t unpack(buf_t *buf, atom_cache_t *cache, atoms_t *atoms, xpool_t *xp);

apr_array_header_t *pack_term(term_t t, atom_cache_t *cache, atoms_t *atoms)
{
	apr_pool_t *pad_pool;	//NB: caller should destroy the returned array
	apr_array_header_t *pad;
	
	apr_pool_create(&pad_pool, 0);
	pad = apr_array_make(pad_pool, 32, 1);
	put(EXTERM_MAGIC);
	pack(pad, t, cache, atoms);

	return pad;
}

static void pack(apr_array_header_t *pad, term_t t, atom_cache_t *cache, atoms_t *atoms)
{
	if (is_int(t))
	{
		int v = int_value(t);
		if (v >= 0 && v <= 255)
		{
			put(EXTERM_BYTE);
			put(v);
		}
		else
		{
			put(EXTERM_INT);
			put32(v);
		}
	}
	if (is_bignum(t))
	{
		bignum_t *bn = bn_value(t);
		int i;

		if (bn_size(bn) < 64)
		{
			put(EXTERM_BIGNUM_SMALL);
			put(bn_size(bn)*4);
			put((bn_sign(bn) != 0));
		}
		else
		{
			put(EXTERM_BIGNUM);
			put32(bn_size(bn)*4);
			put((bn_sign(bn) != 0));
		}
		for (i = bn_size(bn)-1; i >= 0; i--)
			put32lsb(bn->digits[i]);
	}
	else if (is_float(t))
	{
		apr_byte_t buf[31];
		memset(buf, 0, sizeof(buf));
		apr_snprintf((char *)buf, sizeof(buf), "%.20e", dbl_value(t));

		put(EXTERM_FLOAT);
		put_data(buf, sizeof(buf));
	}
	else if (is_atom(t))
	{
		if (cache == 0)
		{
			cstr_t *print_name = atoms_get(atoms, index(t));

			put(EXTERM_ATOM);
			put(0);
			put(print_name->size);
			put_data(print_name->data, print_name->size);
		}
		else
		{
			cstr_t *s = atoms_get(atoms, index(t));
			apr_uint32_t h = hash_atom(s, 256);
			cstr_t *cached = atom_cache_lookup(cache, h);
			
			
			if (cached && scomp(s, cached))
			{
				//packed cached
				put(EXTERM_ATOM_CACHED);
				put(h);
			}
			else
			{
				atom_cache_update(cache, h, s);

				//pack update
				put(EXTERM_ATOM_UPDATE);
				put(h);
				put(0);
				put(s->size);
				put_data(s->data, s->size);
			}
		}
	}
	else if (is_ref(t))
	{
		put(EXTERM_REF);
		pack(pad, prp_node(t), cache, atoms);
		put32(prp_serial(t));
		put(prp_creation(t));
	}
	else if (is_port(t))
	{
		put(EXTERM_PORT);
		pack(pad, prp_node(t), cache, atoms);
		put32(prp_serial(t));
		put(prp_creation(t));
	}
	else if (is_pid(t))
	{
		put(EXTERM_PID);
		pack(pad, pid_node(t), cache, atoms);
		put32(pid_serial(t));	// only 15bits are significant?
		put32(0);	// improves security, what?
		put(pid_creation(t));
	}
	else if (is_tuple(t))
	{
		int size = int_value(tup_size(t));
		if (size < 256)
		{
			int i;
			put(EXTERM_SMALL_TUPLE);
			put(size);
			for (i = 0; i < size; i++)
				pack(pad, tup_elts(t)[i], cache, atoms);
		}
		else
		{
			int i;
			put(EXTERM_TUPLE);
			put32(size);
			for (i = 0; i < size; i++)
				pack(pad, tup_elts(t)[i], cache, atoms);
		}
	}
	else if (is_list(t))
	{
		if (t == nil)
			put(EXTERM_NIL);
		else
		{
			int print_only = 1;
			int length = 0;
			term_t l = t;
			while (is_cons(l))
			{
				term_t v = lst_value(l);
				if (!is_int(v))
					print_only = 0;
				else
				{
					int i = int_value(v);
					if (i < 0 || i > 255)
						print_only = 0;
				}
				length++;
				l = lst_next(l);
			}
			if (print_only && length <= 65535) //string, not too long
			{
				put(EXTERM_STRING);
				put16(length);
				l = t;
				while (l != nil)
				{
					term_t v = lst_value(l);
					put(int_value(v));
					l = lst_next(l);
				}
			}
			else
			{
				put(EXTERM_LIST);
				put32(length);	// there are length+1 elements following actually
				l = t;
				while (is_cons(l))
				{
					term_t v = lst_value(l);
					pack(pad, v, cache, atoms);
					l = lst_next(l);
				}
				pack(pad, l, cache, atoms);	// works for wierd lists
			}
		}
	}
	else if (is_binary(t))
	{
		int n = int_value(bin_size(t));
		put(EXTERM_BINARY);
		put32(n);
		put_data(bin_data(t), n);
	}
}

term_t unpack_term(apr_byte_t *data, int size, atom_cache_t *cache, atoms_t *atoms, xpool_t *xp)
{
	buf_t buf;
	term_t r;
	if (size < 1)
		return AI_UNDEFINED;
	if (data[0] != EXTERM_MAGIC)
		return AI_UNDEFINED;
	buf.data = data+1;
	buf.size = size-1;
	r = unpack(&buf, cache, atoms, xp);
	if (buf.size != 0)
		return AI_UNDEFINED;
	return r;
}

static term_t unpack(buf_t *buf, atom_cache_t *cache, atoms_t *atoms, xpool_t *xp)
{
	apr_byte_t tag;
	if (buf->size < 1)
		return AI_UNDEFINED;

	tag = *buf->data++;
	buf->size--;

	switch (tag)
	{
	case EXTERM_BYTE:
		if (buf->size < 1)
			return AI_UNDEFINED;
		return intnum(get(buf));
	case EXTERM_INT:
		if (buf->size < 4)
			return AI_UNDEFINED;
		return intnum(get32(buf));
	case EXTERM_FLOAT:
	{
		double d;
		if (buf->size < 31)
			return AI_UNDEFINED;
		if (sscanf((char *)buf->data, "%lf", &d) != 1)
			return AI_UNDEFINED;
		buf->data += 31;
		buf->size -= 31;
		return make_float(d, xp);
	}		
	case EXTERM_ATOM:
	{
		int n;
		atom_t a;
		if (buf->size < 2 || buf->data[0] != 0)
			return AI_UNDEFINED;
		n = buf->data[1];
		if (buf->size < n + 2)
			return AI_UNDEFINED;
		a = atoms_set(atoms, (cstr_t *)(buf->data+1));	//set2
		buf->data += n+2;
		buf->size -= n+2;
		return atom(a);
	}
	case EXTERM_ATOM_UPDATE:
	{
		int n;
		atom_t a;
		if (buf->size < 3 || buf->data[1] != 0)
			return AI_UNDEFINED;
		n = buf->data[2];
		if (buf->size < n + 3)
			return AI_UNDEFINED;
		a = atoms_set(atoms, (cstr_t *)(buf->data+2));	//set2
		atom_cache_update(cache, buf->data[0], (cstr_t *)(buf->data+2));
		buf->data += n+3;
		buf->size -= n+3;
		return atom(a);
	}
	case EXTERM_ATOM_CACHED:
	{
		cstr_t *print_name;
		atom_t a;
		if (buf->size < 1)
			return AI_UNDEFINED;
		print_name = atom_cache_lookup(cache, buf->data[0]);
		if (print_name == 0)
			return AI_UNDEFINED;
		a = atoms_set(atoms, print_name);	//set2
		buf->data++;
		buf->size--;
		return atom(a);
	}
	case EXTERM_REF:
	{
		apr_uint32_t serial;
		apr_byte_t creation;
		term_t node = unpack(buf, cache, atoms, xp);
		if (node == AI_UNDEFINED)
			return AI_UNDEFINED;
		if (buf->size < 5)
			return AI_UNDEFINED;
		serial = get32(buf);
		creation = get(buf);
		return make_ref(node, serial, creation, xp);
	}
	case EXTERM_PORT:
	{
		apr_uint32_t serial;
		apr_byte_t creation;
		term_t node = unpack(buf, cache, atoms, xp);
		if (node == AI_UNDEFINED)
			return AI_UNDEFINED;
		if (buf->size < 5)
			return AI_UNDEFINED;
		serial = get32(buf);
		creation = get(buf);
		return make_port(node, serial, creation, xp);
	}
	case EXTERM_PID:
	{
		apr_uint32_t serial;
		apr_byte_t creation;
		term_t node = unpack(buf, cache, atoms, xp);
		if (node == AI_UNDEFINED)
			return AI_UNDEFINED;
		if (buf->size < 9)
			return AI_UNDEFINED;
		serial = get32(buf);
		get32(buf);	//another serial, dropped
		creation = get(buf);
		return make_pid(node, serial, creation, xp);
	}
	case EXTERM_SMALL_TUPLE:
	{
		term_t r;
		int i, n;
		if (buf->size < 1)
			return AI_UNDEFINED;
		n = get(buf);
		r = make_tuple(n, xp);
		for (i = 0; i < n; i++)
		{
			term_t e = unpack(buf, cache, atoms, xp);
			if (e == AI_UNDEFINED)
				return AI_UNDEFINED;
			tup_elts(r)[i] = e;
		}
		return r;
	}
	case EXTERM_TUPLE:
	{
		term_t r;
		int i, n;
		if (buf->size < 4)
			return AI_UNDEFINED;
		n = get32(buf);
		r = make_tuple(n, xp);
		for (i = 0; i < n; i++)
		{
			term_t e = unpack(buf, cache, atoms, xp);
			if (e == AI_UNDEFINED)
				return AI_UNDEFINED;
			tup_elts(r)[i] = e;
		}
		return r;
	}
	case EXTERM_NIL:
		return nil;
	case EXTERM_STRING:
	{
		term_t r = nil;
		term_t cons = nil;
		apr_uint32_t n;
		if (buf->size < 2)
			return AI_UNDEFINED;
		n = get16(buf);
		while (n > 0)
		{
			int e;
			if (buf->size < 1)
				return AI_UNDEFINED;
			e = get(buf);
			lst_add(r, cons, intnum(e), xp);
			n--;
		}
		return r;
	}
	case EXTERM_LIST:
	{
		term_t r = nil;
		int i, n;
		term_t *elems;
		if (buf->size < 4)
			return AI_UNDEFINED;
		n = get32(buf);
		elems = xalloc(xp, 4*(n+1));
		for (i = 0; i <= n; i++)
		{
			elems[i] = unpack(buf, cache, atoms, xp);
			if (elems[i] == AI_UNDEFINED)
				return AI_UNDEFINED;
		}
		r = elems[n];
        while (--n >= 0)
			r = make_list2(elems[n], r, xp);
		return r;
	}
	case EXTERM_BINARY:
	{
		term_t r;
		apr_byte_t *data;
		int n;
		if (buf->size < 4)
			return AI_UNDEFINED;
		n = get32(buf);
		if (n > buf->size)
			return AI_UNDEFINED;
		data = xalloc(xp, n);
		memcpy(data, buf->data, n);
		r = make_binary(intnum(n), data, xp);
		buf->data += n;
		buf->size -= n;

		return r;
	}
	case EXTERM_BIGNUM_SMALL:
	{
		//12345678999 -> <<131,110,5,0,151,28,220,223,2>>

		int i, n, p, q;
		apr_byte_t sign;
		bignum_t *bn;
		apr_byte_t slack[4];
		int base = 0;
		if (buf->size < 2)
			return AI_UNDEFINED;
		n = buf->data[0];
		sign = buf->data[1];
		buf->data += 2;
		buf->size -= 2;
		if (buf->size < n)
			return AI_UNDEFINED;

		p = n / 4;
		q = n % 4;

		if (q > 0)
			base = 1;

		bn = bignum_make0(sign, p+base, xp);

		for (i = p-1; i >= 0; i--)
			bn->digits[base+i] = get32lsb(buf);

		if (q > 0)
		{
			memset(slack, 0, 4);
			for (i = 0; i < q; i++)
				slack[i] = get(buf);
			bn->digits[0] = GET32_LE(slack);
		}

		return bignum(bn);
	}
	case EXTERM_BIGNUM:
	{
		int i, n, p, q;
		apr_byte_t sign;
		bignum_t *bn;
		apr_byte_t slack[4];
		int base = 0;
		if (buf->size < 5)
			return AI_UNDEFINED;
		n = get32(buf);
		sign = get(buf);
		if (buf->size < n)
			return AI_UNDEFINED;

		p = n / 4;
		q = n % 4;

		if (q > 0)
			base = 1;

		bn = bignum_make0(sign, p+base, xp);

		for (i = p-1; i >= 0; i--)
			bn->digits[base+i] = get32lsb(buf);

		if (q > 0)
		{
			memset(slack, 0, 4);
			for (i = 0; i < q; i++)
				slack[i] = get(buf);
			bn->digits[0] = GET32_LE(slack);
		}

		return bignum(bn);
	}
	default:
		return AI_UNDEFINED;
	}
}

static apr_uint32_t get32(buf_t *buf)
{
	apr_uint32_t v = GET32(buf->data);
	buf->data += 4;
	buf->size -= 4;
	return v;
}

static apr_uint32_t get32lsb(buf_t *buf)
{
	apr_uint32_t v = GET32_LE(buf->data);
	buf->data += 4;
	buf->size -= 4;
	return v;
}

static apr_uint16_t get16(buf_t *buf)
{
	apr_uint16_t v = GET16(buf->data);
	buf->data += 2;
	buf->size -= 2;
	return v;
}

static apr_uint16_t get16lsb(buf_t *buf)
{
	apr_uint16_t v = GET16_LE(buf->data);
	buf->data += 2;
	buf->size -= 2;
	return v;
}

static apr_uint32_t hash_atom(cstr_t *s, int max_val)
{
	int i;
	apr_uint32_t a = 0;
	for (i = 0; i < s->size; i++)
	{
		apr_uint32_t j = (a << 4) + s->data[i];
		a = (j & 0x0fffffff) ^ ((j >> 24) & 0xf0);
	}
	return a % max_val;
}

//apr_array_header_t *pack_term(term_t t, atom_cache_t *cache, atoms_t *atoms, xpool_t *xp);
//term_t unpack_term(apr_byte_t *data, int size, atom_cache_t *cache, atoms_t *atoms, xpool_t *xp);

//void exterm_test(void)
//{
//	xpool_t *xp;
//	atoms_t *atoms;
//	apr_array_header_t *pad;
//	term_t a, b;
//
//	apr_pool_create(&pool, 0);
//	atoms_create(&atoms, pool);
//
//	a = A_TRUE;
//	pad = pack_term(a, 0, atoms, pool);
//	b = unpack_term(pad->elts, pad->nelts, 0, atoms, pool);
//	assert(terms_are_equal(a, b, 1));
//
//	a = intnum(123);
//	pad = pack_term(a, 0, atoms, pool);
//	b = unpack_term(pad->elts, pad->nelts, 0, atoms, pool);
//	assert(terms_are_equal(a, b, 1));
//
//	a = nil;
//	pad = pack_term(a, 0, atoms, pool);
//	b = unpack_term(pad->elts, pad->nelts, 0, atoms, pool);
//	assert(terms_are_equal(a, b, 1));
//
//	printf("Exterm tests are successful\n");
//}

//EOF
