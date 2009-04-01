#ifndef TERM_H
#define TERM_H

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
#include <apr_hash.h>

#include "xpool.h"
#include "bignum.h"

//#if APR_SIZEOF_VOIDP != 4
//#error Only 32-bit systems supported
//#endif

#if APR_SIZEOF_VOIDP == 4

typedef apr_uint32_t term_t;
typedef apr_uint32_t celem_t;		// "code element", for *ip, etc
typedef apr_int32_t int_value_t;	// returned by int_value()

#else

typedef apr_uint64_t term_t;
typedef apr_uint64_t celem_t;
typedef apr_int64_t int_value_t;

#endif

// The following term_t layout is optimized for 32-bit
// 64-bit term_t may need different layout

#define TAG_PTR_SIZE	3
#define TAG_PTR_MASK	0x6

#define TAG_IMMED_SIZE	4
#define TAG_IMMED_MASK	0xf

#define TAG_INT_SIZE	3
#define TAG_INT_MASK	0x7

#define PIN_BIT_MASK	0x1

//x000 - TAG_INT
//0001 - TAG_ATOM
//1001 - TAG_LOCAL_PID
//x01p - TAG_OTHER (FLOAT,BIGNUM,BINARY,FUN,PID,REF,PORT)
//x10p - TAG_TUPLE
//x11p - TAG_LIST

#define is_bool(a)		((a) == A_TRUE || (a) == A_FALSE)

#define is_not_ptr(t)	(((t) & TAG_PTR_MASK) == 0x0 || is_nil(t))
#define is_ptr(t)		(((t) & TAG_PTR_MASK) != 0x0 && !is_nil(t))

#define is_float(t)		(((t) & TAG_PTR_MASK) == 0x2 && etag(t) == ETAG_FLOAT)
#define is_bignum(t)	(((t) & TAG_PTR_MASK) == 0x2 && etag(t) == ETAG_BIGNUM)
#define is_binary(t)	(((t) & TAG_PTR_MASK) == 0x2 && etag(t) == ETAG_BINARY)
#define is_fun(t)		(((t) & TAG_PTR_MASK) == 0x2 && etag(t) == ETAG_FUN)

#define is_tuple(t)		(((t) & TAG_PTR_MASK) == 0x4)
#define is_list(t)		(((t) & TAG_PTR_MASK) == 0x6)
#define is_cons(t)		(((t) & TAG_PTR_MASK) == 0x6 && (t) != nil)

#define is_int(t)		(((t) & TAG_INT_MASK) == 0x0)
#define is_atom(t)		(((t) & TAG_IMMED_MASK) == 0x1)

#define is_local_pid(t)	(((t) & TAG_IMMED_MASK) == 0x9)
#define is_long_pid(t)	(((t) & TAG_PTR_MASK) == 0x2 && etag(t) == ETAG_PID)

#define is_number(t)	(is_int(t) || is_float(t) || is_bignum(t))

#define is_prp(t)		(is_pid(t) || is_ref(t) || is_port(t))
#define is_ref(t)		(((t) & TAG_PTR_MASK) == 0x2 && etag(t) == ETAG_REF)
#define is_pid(t)		(is_local_pid(t) || is_long_pid(t))
#define is_port(t)		(((t) & TAG_PTR_MASK) == 0x2 && etag(t) == ETAG_PORT)

#define is_grave(t)		(grv_cross(t) == CROSS_MAGIC)
#define grv_cross(t)	(((grave_t *)PTR(t))->cross)
#define grv_skel(t)		(((grave_t *)PTR(t))->skeleton)

#define is_nil(t)		((t) == nil)

#define is_string(t)	(is_list(t) && printable_chars(t))

#define PTR(t)			((t) & ~TAG_PTR_MASK & ~PIN_BIT_MASK)

#define pin(t)			((t) | PIN_BIT_MASK)
#define is_pinned(t)	(((t) & PIN_BIT_MASK) != 0)

#define tup_size(t)		(((tuple_t *)PTR(t))->size)
#define tup_elts(t)		(((tuple_t *)PTR(t))->elts)
#define bin_size(t)		(intnum(((binary_t *)PTR(t))->e_size & ~ETAG_MASK))
#define bin_data(t)		(((binary_t *)PTR(t))->data)
#define fun_fridge(t)	(((fun_t *)PTR(t))->fridge)
#define fun_amod(t)		(((fun_t *)PTR(t))->amod)
#define fun_afun(t)		(((fun_t *)PTR(t))->afun)
#define fun_arity(t)	(intnum(((fun_t *)PTR(t))->e_arity & ~ETAG_MASK))
#define lst_value(t)	(((cons_t *)PTR(t))->value)
#define lst_next(t)		(((cons_t *)PTR(t))->next)

//list helpers
#define lst_add(f, l, v, xp) \
	do { \
		term_t __n = make_list((v), (xp)); \
		if ((f) == nil) \
			(f) = __n; \
		if ((l) != nil) \
			lst_next((l)) = __n; \
		(l) = __n; \
	} while (0)

#define pid_node(t)			(is_local_pid(t) ? my_node : prp_node(t))
#define pid_serial(t)		(is_local_pid(t) ? index(t) : prp_serial(t))
#define pid_creation(t)		(is_local_pid(t) ? my_creation : prp_creation(t))

#define prp_node(t)			(((prp_t *)PTR(t))->node)
#define prp_serial(t)		(((prp_t *)PTR(t))->e_serial & ~ETAG_MASK)
#define prp_creation(t)		(((prp_t *)PTR(t))->creation)

//	int_value() and int_value2() are equivalent on 32-bit systems
//	for 64-bits, int_value returns int_value_t, int_value2 returns int

#define dbl_value(t)	(((float_nest_t *)PTR(t))->value)
#define int_value(t)	((int_value_t)(t) >> TAG_INT_SIZE)
#define int_value2(t)	((int)((t) >> TAG_INT_SIZE))
#define bn_value(t)		((bignum_t *)PTR(t))

#if APR_SIZEOF_VOIDP == 4
#define MAX_UINT_VALUE	((1 << (32 - TAG_INT_SIZE))-1)
#define MAX_INT_VALUE	((1 << (32 - TAG_INT_SIZE - 1))-1)
#define MIN_INT_VALUE	((-1 << (32 - TAG_INT_SIZE - 1)))
#else
#define MAX_UINT_VALUE	((1l << (64 - TAG_INT_SIZE))-1)
#define MAX_INT_VALUE	((1l << (64 - TAG_INT_SIZE - 1))-1)
#define MIN_INT_VALUE	((-1l << (64 - TAG_INT_SIZE - 1)))
#endif

#define bool(ok) ((ok)? A_TRUE: A_FALSE)

#if APR_SIZEOF_VOIDP == 4
#define nil			((term_t)0xffffffff)
#else
#define nil			((term_t)0xffffffffffffffff)
#endif

#define intnum(n)	(((term_t)(n) << TAG_INT_SIZE) | 0x0)
#define atom(n)		(((term_t)(n) << TAG_IMMED_SIZE) | 0x1)
#define localpid(n)	(((term_t)(n) << TAG_IMMED_SIZE) | 0x9)

#define flonum(p)	((term_t)(p) | 0x2)
#define bignum(p)	((term_t)(p) | 0x2)
#define binary(p)	((term_t)(p) | 0x2)
#define fun(p)		((term_t)(p) | 0x2)

#define prp(p)		((term_t)(p) | 0x2)

#define tuple(p)	((term_t)(p) | 0x4)
#define list(p)		((term_t)(p) | 0x6)

//works for atom and local pid
#define index(t)	((t) >> TAG_IMMED_SIZE)

//helper struct for gc
typedef struct pair_t pair_t;
struct pair_t
{
	term_t one;
	term_t two;
};

#ifndef ATOM_H
typedef struct atoms_t atoms_t;
#endif

typedef struct tuple_t tuple_t;
typedef struct binary_t binary_t;
typedef struct fun_t fun_t;
typedef struct cons_t cons_t;
typedef struct prp_t prp_t;
typedef struct grave_t grave_t;

typedef struct etagged_t etagged_t;

typedef struct float_nest_t float_nest_t;

term_t make_float(double d, xpool_t *xp);
term_t make_tuple(int nelts, xpool_t *xp); //values are undefined
term_t make_tuple0(xpool_t *xp);
term_t make_tuple1(term_t e1, xpool_t *xp);
term_t make_tuple2(term_t e1, term_t e2, xpool_t *xp);
term_t make_tuple3(term_t e1, term_t e2, term_t e3, xpool_t *xp);
term_t make_tuple4(term_t e1, term_t e2, term_t e3, term_t e4, xpool_t *xp);
term_t make_tuple5(term_t e1, term_t e2, term_t e3, term_t e4, term_t e5, xpool_t *xp);
term_t make_tuple6(term_t e1, term_t e2, term_t e3, term_t e4, term_t e5, term_t e6, xpool_t *xp);
term_t make_tuple7(term_t e1, term_t e2, term_t e3, term_t e4, term_t e5, term_t e6, term_t e7, xpool_t *xp);
term_t make_tuple8(term_t e1, term_t e2, term_t e3, term_t e4, term_t e5, term_t e6, term_t e7, term_t e8, xpool_t *xp);
term_t make_tuple_elts(int nelts, term_t *elts, xpool_t *xp);
term_t make_binary(term_t size, apr_byte_t *data, xpool_t *xp);
term_t make_fun(term_t fridge, term_t amod, term_t afun, term_t arity, xpool_t *xp);
term_t make_list(term_t value, xpool_t *xp);
term_t make_list2(term_t value, term_t next, xpool_t *xp);

term_t make_ref(term_t node, apr_uint32_t serial, apr_byte_t creation, xpool_t *xp);
term_t make_pid(term_t node, apr_uint32_t serial, apr_byte_t creation, xpool_t *xp);
term_t make_port(term_t node, apr_uint32_t serial, apr_byte_t creation, xpool_t *xp);

term_t pin_term(term_t t);

term_t copy_list(term_t t, xpool_t *xp);
term_t marshal_term(term_t t, xpool_t *xp);
term_t gc_copy_term(term_t t, xpool_t *xp);

int is_proper_list(term_t l);

int printable_chars(term_t l);
int printable_chars2(apr_byte_t *buf, int len);

const char *stringify_term(term_t t, atoms_t *atoms, apr_pool_t *pool);

const char *ltoz(term_t l, apr_pool_t *pool);	//converts a list into null-terminated string
term_t ztol(const char *z, xpool_t *xp);		//convert null-terminated string into a list

int lst_len(term_t list);

// etagged struct
// tagval layout:
// 000x xxxx ... unused
// 001x xxxx ... FLOAT
// 010x xxxx ... BIGNUM
// 011x xxxx ... BINARY
// 100x xxxx ... FUN
// 101x xxxx ... PID
// 110x xxxx ... REF
// 111x xxxx ... PORT

#define ETAG_MASK	  0xe0000000

#define ETAG_FLOAT	  0x20000000
#define ETAG_BIGNUM	  0x40000000
#define ETAG_BINARY	  0x60000000
#define ETAG_FUN	  0x80000000
#define ETAG_PID	  0xa0000000
#define ETAG_REF	  0xc0000000
#define ETAG_PORT	  0xe0000000

#define etag(t)				(((etagged_t *)PTR(t))->tagval & ETAG_MASK)

struct etagged_t {
	apr_uint32_t tagval;
};

// ETAG_FLOAT
struct float_nest_t {
	apr_uint32_t e_tag;
	double value;
};

// TAG_TUPLE
struct tuple_t {
	term_t size;
	term_t elts[0];
};

// ETAG_BINARY
struct binary_t {
	apr_uint32_t e_size;
	apr_byte_t *data;
};

// ETAG_FUN
struct fun_t {
	apr_uint32_t e_arity;
	term_t fridge;
	term_t amod;
	term_t afun;
};

// ETAG_BIGNUM
//bignum_t * defined in bignum.h

// TAG_LIST - x11p - all 1's means empty list
struct cons_t {
	term_t value;
	term_t next;
};

struct prp_t {
	apr_uint32_t e_serial;
	term_t node;
	apr_byte_t creation;
};

#define CROSS_MAGIC		0xf73b810a
struct grave_t {
	apr_uint32_t cross;		//equals CROSS_MAGIC
	term_t skeleton;
};

#endif
