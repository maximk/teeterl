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

#include "bignum.h"

#include <apr_tables.h>

static bignum_t *addup(int n1, const digit_t *d1, int n2, const digit_t *d2, xpool_t *xp);
static bignum_t *subtract(int n1, const digit_t *d1, int n2, const digit_t *d2, xpool_t *xp);
static int compare(int n1, const digit_t *d1, int n2, const digit_t *d2);
static bignum_t *subtract2(int n1, const digit_t *d1, int n2, const digit_t *d2, xpool_t *xp);

bignum_t *bignum_make0(int sign, int n, xpool_t *xp)
{
	bignum_t *a = xalloc(xp, 4 + sizeof(digit_t)*n);
	a->e_n = bn_pack(sign, n);
	return a; //digits contain garbage
}

bignum_t *bignum_make(int sign, int n, const digit_t *digits, xpool_t *xp)
{
	bignum_t *a = xalloc(xp, 4 + sizeof(digit_t)*n);
	a->e_n = bn_pack(sign, n);
	if (n > 0)
		memcpy(a->digits, digits, sizeof(digit_t)*n);
	return a;
}

bignum_t *bignum_from32(apr_int32_t v, xpool_t *xp)
{
	bignum_t *a = xalloc(xp, 4 + sizeof(digit_t));
	if (v < 0)
	{
		a->e_n = bn_pack(1, 1);
		a->digits[0] = -v;
	}
	else
	{
		a->e_n = bn_pack(0, 1);
		a->digits[0] = v;
	}
	return a;
}

bignum_t *bignum_from64(apr_int64_t v, xpool_t *xp)
{
	//make the smallest bignum possible

	digit_t ds[2];

	if (v == 0)
		return bignum_zero();

	ds[0] = (apr_uint32_t)(v >> 32);
	ds[1] = (apr_uint32_t)v;

	if (ds[0] == 0xffffffff)
	{
		digit_t d2 = (digit_t)(-v);
		return bignum_make(1, 1, &d2, xp);
	}
	if (ds[0] == 0x00000000)
		return bignum_make(0, 1, ds+1, xp);
	else if (v < 0)
	{
		digit_t ds2[2];
		ds2[0] = (apr_uint32_t)((-v) >> 32);
		ds2[1] = (apr_uint32_t)(-v);

		return bignum_make(1, 2, ds2, xp);
	}
	else
		return bignum_make(0, 2, ds, xp);
}

bignum_t *bignum_copy(const bignum_t *a, xpool_t *xp)
{
	bignum_t *copy = xalloc(xp, 4 + sizeof(digit_t)*bn_size(a));
	copy->e_n = a->e_n;
	if (bn_size(a) > 0)
		memcpy(copy->digits, a->digits, sizeof(digit_t)*bn_size(a));
	return copy;
}

// comparisons
int bignum_are_equal(const bignum_t *a, const bignum_t *b)
{
	int i;
	if (a->e_n != b->e_n)
		return 0;
	for (i = 0; i < (int)bn_size(a); i++)
		if (a->digits[i] != b->digits[i])
			return 0;
	return 1;
}

int bignum_are_less(const bignum_t *a, const bignum_t *b)
{
	int cmp;

	//if (a->n == 0 && b->n == 0)
	//	return 0;

	if (bn_sign(a) && !bn_sign(b))
		return 1;
	if (!bn_sign(a) && bn_sign(b))
		return 0;

	cmp = compare(bn_size(a), a->digits, bn_size(b), b->digits);
	if (cmp == 0)
		return 0;

	return (cmp == -1) == (bn_sign(a) == 0);
}

// constants
bignum_t *bignum_zero()
{
	static apr_uint32_t zero = bn_pack(0, 0);
	return (bignum_t *)&zero;
}

bignum_t *bignum_one()
{
	static apr_uint32_t one[] = {bn_pack(0, 1), 1};
	return (bignum_t *)one;
}

bignum_t *bignum_minus_one()
{
	static apr_uint32_t minus_one[] = {bn_pack(1, 1), 1};
	return (bignum_t *)minus_one;
}

// arithmetic
bignum_t *bignum_add(const bignum_t *a, const bignum_t *b, xpool_t *xp)
{
	bignum_t *s;
	int negate = bn_sign(a);

	if (bn_size(a) == 0)
		return (bignum_t *)b;
	if (bn_size(b) == 0)
		return (bignum_t *)a;

	if (bn_sign(a) == bn_sign(b))
		s = addup(bn_size(a), a->digits, bn_size(b), b->digits, xp);
	else
		s = subtract(bn_size(a), a->digits, bn_size(b), b->digits, xp);

	if (negate && bn_size(s) != 0)
		s->e_n = bn_pack(!bn_sign(s), bn_size(s));

	return s;
}

bignum_t *bignum_add1(const bignum_t *a, apr_int32_t v, xpool_t *xp)
{
	apr_uint32_t value[2];

	if (bn_size(a) == 0)
		return bignum_from32(v, xp);
	if (v == 0)
		return (bignum_t *)a;

	if (v < 0)
	{
		value[0] = bn_pack(1, 1);
		value[1] = -v;
	}
	else
	{
		value[0] = bn_pack(0, 1);
		value[1] = v;
	}

	return bignum_add(a, (const bignum_t *)value, xp);
}

bignum_t *bignum_add2(const bignum_t *a, apr_int64_t v, xpool_t *xp)
{
	if (v <= 0x7fffffff && v > -0x80000000)
		return bignum_add1(a, (apr_int32_t)v, xp);
	else
	{
		bignum_t *b = bignum_from64(v, xp);
		return bignum_add(a, (const bignum_t *)b, xp);
	}
}

bignum_t *bignum_sub(const bignum_t *a, const bignum_t *b, xpool_t *xp)
{
	bignum_t *s;
	int negate = bn_sign(a);

	if (bn_size(a) == 0)
	{
		bignum_t *copy = bignum_copy(b, xp);
		copy->e_n = bn_pack(!bn_sign(copy), bn_size(copy));
		return copy;
	}
	if (bn_size(b) == 0)
		return (bignum_t *)a;

	if (bn_sign(a) != bn_sign(b))
		s = addup(bn_size(a), a->digits, bn_size(b), b->digits, xp);
	else
		s = subtract(bn_size(a), a->digits, bn_size(b), b->digits, xp);

	if (negate && bn_size(s) > 0)	//NB: avoid negative zero
		s->e_n = bn_pack(!bn_sign(s), bn_size(s));

	return s;
}

bignum_t *bignum_mult(const bignum_t *a, const bignum_t *b, xpool_t *xp)
{
	apr_pool_t *sink;
	xpool_t *tmp;
	bignum_t *r = bignum_zero();
	bignum_t *s = (bignum_t *)a;
	bignum_t *result;
	int i = bn_size(b);

	if (bn_size(a) == 0 || bn_size(b) == 0)
		return bignum_zero();

	apr_pool_create(&sink, 0);
	tmp = xpool_make(sink);

	while (i > 0)
	{
		bignum_t *p = bignum_mult1(s, b->digits[--i], tmp);
		r = bignum_add(r, p, tmp);

		// left shift
		s = bignum_make(bn_sign(s), bn_size(s)+1, s->digits, tmp);
		s->digits[bn_size(s)-1] = 0;
	}

	// marshal to pool
	result = bignum_copy(r, xp);
	if (bn_sign(b))
		bn_negate(result);

	apr_pool_destroy(sink);

	return result;
}

bignum_t *bignum_mult1(const bignum_t *a, digit_t b, xpool_t *xp)
{
	int i = bn_size(a);
	int sign = bn_sign(a);
	digit_t carry = 0;
	digit_t *p;

	if (bn_size(a) == 0 || b == 0)
		return bignum_zero();

	p = xalloc(xp, sizeof(digit_t)*bn_size(a));

	while (i > 0)
	{
		apr_uint64_t q = (apr_uint64_t)a->digits[--i] * b + carry;
		p[i] =(digit_t)q;
		carry = (digit_t)(q >> 32);
	}

	if (carry > 0)
	{
		bignum_t *r = bignum_make0(sign, bn_size(a)+1, xp);
		r->digits[0] = carry;
		memcpy(r->digits+1, p, sizeof(digit_t)*bn_size(a));
		return r;
	}
	else
		return bignum_make(sign, bn_size(a), p, xp);
}

bignum_t *bignum_mult2(const bignum_t *a, apr_uint64_t v, xpool_t *xp)
{
	if (v <= 0xffffffff)
		return bignum_mult1(a, (digit_t)v, xp);
	else
	{
		bignum_t *b = bignum_from64(v, xp);
		return bignum_mult(a, (const bignum_t *)b, xp);
	}
}

bignum_t *bignum_div(const bignum_t *a, const bignum_t *b, bignum_t **r, xpool_t *xp)
{
	int cmp;

	apr_pool_t *sink;
	xpool_t *tmp;

	int na, nb;
	digit_t normalizer;
	bignum_t *p, *d;
	int qsize;
	digit_t *q;
	int i, skip;
	bignum_t *result;

	if (bn_size(b) == 0)
		return 0; // division by zero
	if (bn_size(a) == 0)
	{
		if (r)
			*r = bignum_zero();
		return bignum_zero();
	}

	cmp = compare(bn_size(a), a->digits, bn_size(b), b->digits);
	if (cmp == 0)
	{
		if (r)
			*r = bignum_zero();
		if (bn_sign(a) == bn_sign(b))
			return bignum_one();
		else
			return bignum_from32(-1, xp);
	}
	
	if (cmp < 0)
	{
		if (r)
			*r = (bignum_t *)a;
		return bignum_zero();
	}

	//TODO: limited to positive small numbers for now
	if (bn_sign(b) == 0 && bn_size(b) == 1 && b->digits[0] <= 0x7fffffff)
	{
		digit_t reminder;
		result = bignum_div1(a, b->digits[0], &reminder, xp);

		if (bn_sign(b))
			bn_negate(result);

		// the sign of reminder is the same as a's
		if (r)
		{
			//fix reminder sign
			if (bn_sign(a))
				*r = bignum_from32(-(int)reminder, xp);
			else
				*r = bignum_from32(reminder, xp);
		}

		return result;
	}

	// a1 a2 a3 ... an
	// b1 b2 b3 ... bm
	// m =< n

	na = bn_size(a);
	nb = bn_size(b);

	// 90 / 9 = 10
	// 10 / 9 = 1
	// max result size is nb - na + 1

	apr_pool_create(&sink, 0);
	tmp = xpool_make(sink);

	qsize = na - nb + 1;
	q = xalloc(tmp, sizeof(digit_t) * qsize);

	//normalize to have a large first digit of divisor
	normalizer = (digit_t)(0x100000000LL / ((apr_uint64_t)b->digits[0] + 1));

	// initial normalized residue
	p = bignum_mult1(a, normalizer, tmp);

	//drop sign
	p->e_n = bn_pack(0, bn_size(p));

	// shift b (na-nb) times
	d = bignum_make0(0, na, tmp);
	memset(d->digits, 0, sizeof(digit_t)*na);
	memcpy(d->digits, b->digits, sizeof(digit_t)*nb);

	// d->digits[0] is more than radix/2 now
	d = bignum_mult1(d, normalizer, tmp);

	// a1 a2 a3    ...   an
	// b1 b2 ... bm 0 ... 0

	i = 0; // index to result

	do {
		cmp = compare(bn_size(p), p->digits, bn_size(d), d->digits);
		if (cmp < 0)
		{
			q[i++] = 0;
			d->e_n = bn_size(d)-1;
		}
		else
		{
			// guess the next digit
			apr_uint64_t phead = ((apr_uint64_t)p->digits[0] << 32) + p->digits[1];
			digit_t guess;
			bignum_t *val1;

			if (bn_size(d) < bn_size(p))
				guess = (digit_t)(phead / d->digits[0]);
			else
				guess = (digit_t)((phead / d->digits[0]) >> 32);

			val1 = bignum_mult1(d, guess, tmp);
			p = bignum_sub(p, val1, tmp);

			// guess can only undershoot -- is this right?

			// if p > d then p -= d, guess++

			while (bn_sign(p))
			{
				p = bignum_add(p, d, tmp);
				guess--; // possible underflow?
			}

			while (compare(bn_size(p), p->digits, bn_size(d), d->digits) > 0)
			{
				p = bignum_sub(p, d, tmp);
				guess++; // possible overflow?
			}

			q[i++] = guess;
			d->e_n = bn_size(d)-1;
		}
	} while (i < qsize);

	if (r)
	{
		// unnormalize reminder
		p->e_n = bn_pack(bn_sign(a), bn_size(p)); // copy sign from a
		*r = bignum_div1(p, normalizer, 0, xp);
	}

	skip = (q[0] == 0)? 1: 0;
	result = bignum_make(bn_sign(a) != bn_sign(b), qsize-skip, q+skip, xp);

	apr_pool_destroy(sink);

	return result;
}

//BUG: sign of reminder is always positive
bignum_t *bignum_div1(const bignum_t *a, digit_t b, digit_t *r, xpool_t *xp)
{
	digit_t *pad;
	int i;
	digit_t reminder;

	if (bn_size(a) == 0)
		return bignum_zero();

	pad = xalloc(xp, sizeof(digit_t)*bn_size(a));

	reminder = 0;
	for (i = 0; i < (int)bn_size(a); i++)
	{
		apr_uint64_t nom = ((apr_uint64_t)(reminder) << 32) + a->digits[i];

		pad[i] = (digit_t)(nom / b);
		reminder = (digit_t)(nom % b);
	}

	if (r)
		*r = reminder;

	if (pad[0] == 0)
		return bignum_make(bn_sign(a), bn_size(a)-1, pad+1, xp);
	else
		return bignum_make(bn_sign(a), bn_size(a), pad, xp);
}

// binary
bignum_t *bignum_not(const bignum_t *a, xpool_t *xp)
{
	int c_size;
	digit_t c_pad;
	const digit_t *cs = bignum_complement(a, &c_size, &c_pad, xp);

	digit_t *rs = xalloc(xp, c_size*sizeof(digit_t));
	int i;

    for (i = 0; i < c_size; i++)
        rs[i] = ~cs[i];

    return bignum_decomplement(rs, c_size, xp);
}

bignum_t *bignum_and(const bignum_t *a, const bignum_t *b, xpool_t *xp)
{
    digit_t pa, pb;
	int na, nb;
    const digit_t *csa = bignum_complement(a, &na, &pa, xp);
    const digit_t *csb = bignum_complement(b, &nb, &pb, xp);

	int n = (na > nb) ?na :nb;
    digit_t *rs = xalloc(xp, n*sizeof(digit_t));
    int i;

    for (i = 0; i < n; i++)
    {
        int ia = i + na - n;
        int ib = i + nb - n;
        digit_t da = (ia < 0) ?pa :csa[ia];
        digit_t db = (ib < 0) ?pb :csb[ib];
        rs[i] = da & db;
    }

    return bignum_decomplement(rs, n, xp);
}

bignum_t *bignum_or(const bignum_t *a, const bignum_t *b, xpool_t *xp)
{
    digit_t pa, pb;
	int na, nb;
    const digit_t *csa = bignum_complement(a, &na, &pa, xp);
    const digit_t *csb = bignum_complement(b, &nb, &pb, xp);

	int n = (na > nb) ?na :nb;
    digit_t *rs = xalloc(xp, n*sizeof(digit_t));
    int i;

    for (i = 0; i < n; i++)
    {
        int ia = i + na - n;
        int ib = i + nb - n;
        digit_t da = (ia < 0) ?pa :csa[ia];
        digit_t db = (ib < 0) ?pb :csb[ib];
        rs[i] = da | db;
    }

    return bignum_decomplement(rs, n, xp);
}

bignum_t *bignum_xor(const bignum_t *a, const bignum_t *b, xpool_t *xp)
{
    digit_t pa, pb;
	int na, nb;
    const digit_t *csa = bignum_complement(a, &na, &pa, xp);
    const digit_t *csb = bignum_complement(b, &nb, &pb, xp);

	int n = (na > nb) ?na :nb;
    digit_t *rs = xalloc(xp, n*sizeof(digit_t));
    int i;

    for (i = 0; i < n; i++)
    {
        int ia = i + na - n;
        int ib = i + nb - n;
        digit_t da = (ia < 0) ?pa :csa[ia];
        digit_t db = (ib < 0) ?pb :csb[ib];
        rs[i] = da ^ db;
    }

    return bignum_decomplement(rs, n, xp);
}

//
// complement is a special kind of bignums
// when negative are represented as 1's complements;
// such bignums are always positive itself
//

const digit_t *bignum_complement(const bignum_t *a, int *r_size, digit_t *r_pad, xpool_t *xp)
{
	bignum_t *a1;
	int high_bit_set;

	if (bn_size(a) == 0)
	{
		*r_size = 0;
		*r_pad = 0;
		return 0;
	}

	if (bn_sign(a))
	{
		// add the number to 1000..000, 1-bit longer than the number

		int n = bn_size(a);
		bignum_t *wall = bignum_make0(0, n+1, xp);
		int i;

		wall->digits[0] = 1;
		for (i = 1; i < n+1; i++)
			wall->digits[i] = 0;

		a1 = bignum_add(wall, a, xp);
		*r_pad = 0xffffffff;
	}
	else
	{
		a1 = (bignum_t *)a;
		*r_pad = 0;
	}

	high_bit_set = ((a1->digits[0] & 0x80000000) != 0);

	if (bn_sign(a) != high_bit_set)
	{
		int n = bn_size(a1);
		digit_t *ds = xalloc(xp, (n+1)*sizeof(digit_t));
		ds[0] = *r_pad;
		memcpy(ds+1, a1->digits, n*sizeof(digit_t));

		*r_size = n+1;
		return ds;
	}
	else
	{
		*r_size = bn_size(a1);
		return a1->digits;
	}
}

bignum_t *bignum_decomplement(const digit_t *digits, int n, xpool_t *xp)
{
	int high_bit_set;

	if (n == 0)
		return bignum_zero();

	high_bit_set = ((digits[0] & 0x80000000) != 0);

	if (!high_bit_set)
	{
		//positive, skip a few first
		
		int skip = 0;
		
		while (skip < n && digits[skip] == 0)
			skip++;

		return bignum_make(0, n-skip, digits+skip, xp);
	}
	else
	{
		// negative, bang off the wall

		bignum_t *a = bignum_make(0, n, digits, xp);
		bignum_t *wall = bignum_make0(0, n+1, xp);
		int i;

		wall->digits[0] = 1;
		for (i = 1; i < n+1; i++)
			wall->digits[i] = 0;

		return bignum_sub(a, wall, xp);
	}
}

bignum_t *bignum_bsl(const bignum_t *a, int shifts, xpool_t *xp)
{
	int n;
	digit_t pad;
	const digit_t *cs;

	digit_t *rs;
	digit_t carry;
	int i;

	int w, rest;

	if (shifts == 0)
		return (bignum_t *)a;
	if (shifts < 0)
		return bignum_bsr(a, -shifts, xp);

	cs = bignum_complement(a, &n, &pad, xp);

	w = shifts / 32;
	rest = shifts % 32;

	rs = xalloc(xp, (n+w+1)*sizeof(digit_t));
	rs[0] = pad;
	memcpy(rs+1, cs, n*sizeof(digit_t));
	memset(rs+n+1, 0, w*sizeof(digit_t));

	carry = 0;
	for (i = n; i >= 0; i--)
	{
		apr_uint64_t shuttle = (((apr_uint64_t)rs[i]) << rest) | carry;
		rs[i] = (digit_t)shuttle;
		carry = (digit_t)(shuttle >> 32);
	}

	return bignum_decomplement(rs, n+w+1, xp);
}

bignum_t *bignum_bsr(const bignum_t *a, int shifts, xpool_t *xp)
{
	int n;
	digit_t pad;
	const digit_t *cs;

	digit_t *rs;
	digit_t carry;
	int i;

	int w, rest;

	if (shifts == 0)
		return (bignum_t *)a;
	if (shifts < 0)
		return bignum_bsl(a, -shifts, xp);

	cs = bignum_complement(a, &n, &pad, xp);

	w = shifts / 32;
	rest = shifts % 32;

	if (w >= n)
		return bignum_from32((int)pad, xp);

	rs = xalloc(xp, (n-w)*sizeof(digit_t));
	memcpy(rs, cs, (n-w)*sizeof(digit_t));

	carry = pad;
	for (i = 0; i < n-w; i++)
	{
		apr_uint64_t shuttle = ((apr_uint64_t)carry << 32) | rs[i];
		shuttle >>= rest;
		carry = rs[i];
		rs[i] = (digit_t)shuttle;
	}
	
	return bignum_decomplement(rs, n-w, xp);
}

// conversion
double bignum_to_double(const bignum_t *a)
{
	double result = 0;
	apr_uint64_t radix = (apr_uint64_t)1 << 32;
	int i;
	for (i = 0; i < (int)bn_size(a); i++)
		result = result * radix + a->digits[i];
	if (bn_sign(a))
		result = -result;
	return result;
}

const char *bignum_to_str(const bignum_t *a, apr_pool_t *pool)
{
	xpool_t *tmp;
	apr_array_header_t *buf;
	char *result;
	int i;

	if (bn_size(a) == 0)
		return "0";

	tmp = xpool_make(pool);
	buf = apr_array_make(pool, 1, 1);

	while (bn_size(a) > 0)
	{
		digit_t d;
		a = bignum_div1(a, 10, &d, tmp);
		*(char *)apr_array_push(buf) = (char)('0' + d);
	}

	if (bn_sign(a))
		*(char *)apr_array_push(buf) = '-';

	result = apr_palloc(pool, buf->nelts+1);
	for (i = 0; i < buf->nelts; i++)
		result[buf->nelts-i-1] = ((char *)buf->elts)[i];
	result[buf->nelts] = 0;

	xpool_destroy(tmp);
	return result;
}

static bignum_t *addup(int n1, const digit_t *d1, int n2, const digit_t *d2, xpool_t *xp)
{
	int n3 = (n1 > n2)? n1: n2;
	int i1 = n1;
	int i2 = n2;
	int i3 = n3;
	digit_t carry = 0;
	digit_t *d3 = xalloc(xp, sizeof(digit_t)*n3);

	while (i3 > 0)
	{
		apr_uint64_t q =
			(apr_uint64_t)((--i1 >= 0)? d1[i1]: 0) +
			((--i2 >= 0)? d2[i2]: 0) +
			carry;
		d3[--i3] = (digit_t)q;
		carry = (digit_t)(q >> 32);
	}

	if (carry > 0)
	{
		bignum_t *r = bignum_make0(0, n3+1, xp);
		r->digits[0] = 1;
		memcpy(r->digits+1, d3, sizeof(digit_t)*n3);
		return r;
	}
	else
		return bignum_make(0, n3, d3, xp);
}

static bignum_t *subtract(int n1, const digit_t *d1, int n2, const digit_t *d2, xpool_t *xp)
{
	int o = compare(n1, d1, n2, d2);
	if (o == 0)
		return bignum_zero();
	if (o > 0)
		return subtract2(n1, d1, n2, d2, xp);
	else
	{
		bignum_t *r = subtract2(n2, d2, n1, d1, xp);
		r->e_n = bn_pack(1, bn_size(r));
		return r;
	}
}

static int compare(int n1, const digit_t *d1, int n2, const digit_t *d2)
{
	int i;
	if (n1 > n2)
		return 1;
	if (n1 < n2)
		return -1;
	for (i = 0; i < n1; i++)
	{
		if (d1[i] > d2[i])
			return 1;
		if (d1[i] < d2[i])
			return -1;
	}
	return 0;
}

static bignum_t *subtract2(int n1, const digit_t *d1, int n2, const digit_t *d2, xpool_t *xp)
{
	int n3 = (n1 > n2)? n1: n2;
	int i1 = n1;
	int i2 = n2;
	int i3 = n3;
	digit_t borrow = 0;
	digit_t *d3 = xalloc(xp, sizeof(digit_t)*n3);
	int skip = 0;

	while (i3 > 0)
	{
		apr_int64_t q =
			(apr_int64_t)((--i1 >= 0)? d1[i1]: 0) -
			((--i2 >= 0)? d2[i2]: 0) -
			borrow;
		d3[--i3] = (digit_t)q;
		borrow = (q < 0)? 1: 0;
	}

	// assert borrow == 0
	while (skip < n3)
	{
		if (d3[skip] != 0)
			break;
		skip++;
	}

	return bignum_make(0, n3-skip, d3+skip, xp);
}

//EOF
