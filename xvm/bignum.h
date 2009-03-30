#ifndef BIGNUM_H
#define BIGNUM_H

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

#include "xpool.h"

typedef apr_uint32_t digit_t;

typedef struct bignum_t bignum_t;
struct bignum_t {
	apr_uint32_t e_n;
	digit_t digits[1];
};

// e_n layout:
// 010s xxxx ....
//
// 010 is BIGNUM etag, s is sign

#define BIGNUM_SIGN_MASK	0x10000000

#define ETAG_MASK			0xe0000000
#define ETAG_BIGNUM			0x40000000

#define bn_sign(v)		(((v)->e_n & BIGNUM_SIGN_MASK) != 0)
#define bn_size(v)		((int)(v)->e_n & ~ETAG_MASK & ~BIGNUM_SIGN_MASK)
#define bn_pack(s,n)	(((s)? ((n) | BIGNUM_SIGN_MASK): (n)) | ETAG_BIGNUM)
#define bn_negate(v)	do { (v)->e_n = bn_pack(!bn_sign(v), bn_size(v)); } while(0)

bignum_t *bignum_make0(int sign, int n, xpool_t *xp);
bignum_t *bignum_make(int sign, int n, const digit_t *digits, xpool_t *xp);

bignum_t *bignum_from32(apr_int32_t v, xpool_t *xp);
bignum_t *bignum_from64(apr_int64_t v, xpool_t *xp);
bignum_t *bignum_copy(const bignum_t *bn, xpool_t *xp);

// comparisons
int bignum_are_equal(const bignum_t *a, const bignum_t *b);
int bignum_are_less(const bignum_t *a, const bignum_t *b);

// constants
bignum_t *bignum_zero();
bignum_t *bignum_one();
bignum_t *bignum_minus_one();

// arithmetic
bignum_t *bignum_add(const bignum_t *a, const bignum_t *b, xpool_t *xp);
bignum_t *bignum_add1(const bignum_t *a, apr_int32_t v, xpool_t *xp);
bignum_t *bignum_add2(const bignum_t *a, apr_int64_t v, xpool_t *xp);
bignum_t *bignum_sub(const bignum_t *a, const bignum_t *b, xpool_t *xp);

bignum_t *bignum_mult(const bignum_t *a, const bignum_t *b, xpool_t *xp);
bignum_t *bignum_mult1(const bignum_t *a, digit_t b, xpool_t *xp);
bignum_t *bignum_mult2(const bignum_t *a, apr_uint64_t v, xpool_t *xp);
bignum_t *bignum_div(const bignum_t *a, const bignum_t *b, bignum_t **r, xpool_t *xp);
bignum_t *bignum_div1(const bignum_t *a, digit_t b, digit_t *r, xpool_t *xp);

// logical
bignum_t *bignum_not(const bignum_t *a, xpool_t *xp);

bignum_t *bignum_and(const bignum_t *a, const bignum_t *b, xpool_t *xp);
bignum_t *bignum_or(const bignum_t *a, const bignum_t *b, xpool_t *xp);
bignum_t *bignum_xor(const bignum_t *a, const bignum_t *b, xpool_t *xp);

bignum_t *bignum_bsl(const bignum_t *a, int n, xpool_t *xp);
bignum_t *bignum_bsr(const bignum_t *a, int n, xpool_t *xp);

// conversion
double bignum_to_double(const bignum_t *a);
const char *bignum_to_str(const bignum_t *a, apr_pool_t *pool);

#endif
