#ifndef TERM_H
#define TERM_H

//
// Terms are either immediate or pointer
//
// Immediate terms:
//	integer (28-bit or 60-bit)
//	atom (including special atoms)
//	local PID
//	local OID
//	nil
//
// Pointer terms:
//	float
//	bignum
//	binary
//	fun
//	PID
//	OID
//	tuple
//	list
//
//	Immediate terms contain their value within 32bits (or 64bits)
//	Pointer terms is a tagged memory reference
//
// Decreasing frequency guess:
// list - tuple - binary - fun - bignum - pid - float - oid
//

#include <apr_general.h>

//MPI--------------------------------------------------------------------------
typedef char               mp_sign;
typedef unsigned int       mp_size;
typedef int                mp_err;
#define MP_DIGIT_BIT       (CHAR_BIT*sizeof(mp_digit))
#define MP_WORD_BIT        (CHAR_BIT*sizeof(mp_word))
#define RADIX              (MP_DIGIT_MAX+1)
//MPI--------------------------------------------------------------------------

#if APR_SIZEOF_VOIDP == 4
typedef apr_uint32_t term_t;
typedef apr_uint32_t uint;
#define SIGN_BIT	0x80000000

//MPI--------------------------------------------------------------------------
typedef apr_uint16_t mp_digit;
typedef apr_uint32_t mp_word;

#define MP_DIGIT_MAX       ((1 << 16)-1)
#define MP_WORD_MAX        ((1 << 32)-1)
#define MP_DIGIT_SIZE      2
#define DIGIT_FMT          "%04x"
//MPI--------------------------------------------------------------------------

#define MAX_INT_VALUE	((1 << (32 - TAG_IMMED1_SIZE - 1)) - 1)
#define MIN_INT_VALUE	(-1 << (32 - TAG_IMMED1_SIZE - 1))

#define INT_BITS	32
#define INT_BYTES	4
#define int_to_bytes_msb(buf, i) \
	do { \
		buf[0] = i >> 24; \
		buf[1] = i >> 16; \
		buf[2] = i >> 8; \
		buf[3] = i; \
	} while (0)

#elif APR_SIZEOF_VOIDP == 8
typedef apr_uint64_t term_t;
typedef apr_uint32_t uint;
#define SIGN_BIT	0x8000000000000000

//MPI--------------------------------------------------------------------------
typedef apr_uint32_t mp_digit;
typedef apr_uint64_t mp_word;

#define MP_DIGIT_MAX       ((1 << 32)-1)
#define MP_WORD_MAX        ((1 << 64)-1)
#define MP_DIGIT_SIZE      4
#define DIGIT_FMT          "%08x"
//MPI--------------------------------------------------------------------------

#define MAX_INT_VALUE	((1 << (64 - TAG_IMMED1_SIZE - 1)) - 1)
#define MIN_INT_VALUE	(-1 << (64 - TAG_IMMED1_SIZE - 1))

#define INT_BITS	64
#define INT_BYTES	8
#define int_to_bytes_msb(buf, i) \
	do { \
		buf[0] = i >> 56; \
		buf[1] = i >> 48; \
		buf[2] = i >> 40; \
		buf[3] = i >> 32; \
		buf[4] = i >> 24; \
		buf[5] = i >> 16; \
		buf[6] = i >> 8; \
		buf[7] = i; \
	} while (0)

#else
#error Only 32-bit and 64-bit OSes are supported
#endif

#define fits_int(i) ((i <= MAX_INT_VALUE) && (i >= MIN_INT_VALUE))

// Term tagging (last 5bits):
//
// xx0000	integer
// x01000	atom
// 011000	local PID
// 111000	local OID
// xxx001	float
// xxx010	bignum
// xxx011	binary
// xxx100	fun
// xxx101	long PID / long OID
// xxx110	tuple
// xxx111	list

#define TAG_IMMED1_MASK	0xf
#define TAG_IMMED1_SIZE	4
#define TAG_IMMED2_MASK	0x1f
#define TAG_IMMED2_SIZE	5
#define TAG_IMMED3_MASK	0x3f
#define TAG_IMMED3_SIZE 6
#define TAG_PTR_MASK	0x7
#define TAG_PTR_SIZE	3

#define is_immed(t)		(((t) & TAG_PTR_MASK) == 0 || t == nil)
#define is_ptr(t)		(((t) & TAG_PTR_MASK) != 0 && t != nil)
#define are_both_immed_and_not_nil(a, b)		((((a) | (b)) & TAG_PTR_MASK) == 0)
#define are_both_int(a, b)						((((a) | (b)) & TAG_IMMED1_MASK) == 0)

// basic type checks
#define is_int(t)		(((t) & TAG_IMMED1_MASK) == 0x0)
#define is_atom(t)		(((t) & TAG_IMMED2_MASK) == 0x8)
#define is_short_pid(t)	(((t) & TAG_IMMED3_MASK) == 0x18)
#define is_short_oid(t)	(((t) & TAG_IMMED3_MASK) == 0x38)
#define is_float(t)		(((t) & TAG_PTR_MASK) == 0x1)
#define is_bignum(t)	(((t) & TAG_PTR_MASK) == 0x2)
#define is_binary(t)	(((t) & TAG_PTR_MASK) == 0x3)
#define is_fun(t)		(((t) & TAG_PTR_MASK) == 0x4)
#define is_long_id(t)	(((t) & TAG_PTR_MASK) == 0x5)
#define is_tuple(t)		(((t) & TAG_PTR_MASK) == 0x6)
#define is_list(t)		(((t) & TAG_PTR_MASK) == 0x7)
#define is_nil(t)		((t) == nil)

// special value terms:
//	nil = empty list (is_list is true)
//	noval = no value (is_atom is true)

#define nil		((term_t)-1)
#define noval	((term_t)8)

// atom indexes start at 1 to provide room for noval (and possibly other special values)
#define ATOM_INDEX_BASE	1

// compound type checks
#define is_ordinary_atom(t)		(is_atom(t) && (t) != noval)
#define is_cons(t)				(is_list(t) && (t) != nil)
#define is_pid(t)				(is_short_pid(t) || is_long_pid(t))
#define is_oid(t)				(is_short_oid(t) || is_long_oid(t))

// type tagging
#define tag_int(i)			((term_t)(i) << TAG_IMMED1_SIZE)
#define tag_atom(i)			(((term_t)(i) << TAG_IMMED2_SIZE) | 0x8)
#define tag_short_pid(i)	(((term_t)(i) << TAG_IMMED3_SIZE) | 0x18)
#define tag_short_oid(i)	(((term_t)(i) << TAG_IMMED3_SIZE) | 0x38)
#define tag_float(p)		((term_t)(p) | 0x1)
#define tag_bignum(p)		((term_t)(p) | 0x2)
#define tag_binary(p)		((term_t)(p) | 0x3)
#define tag_fun(p)			((term_t)(p) | 0x4)
#define tag_long_id(p)		((term_t)(p) | 0x5)
#define tag_tuple(p)		((term_t)(p) | 0x6)
#define tag_list(p)			((term_t)(p) | 0x7)

// basic term unpacking
#define peel(t)			((term_box_t *)((t) & ~TAG_PTR_MASK))
#define int_value(t)	((int)(t) >> TAG_IMMED1_SIZE)
#define atom_index(t)	((uint)(t) >> TAG_IMMED2_SIZE)
#define pid_serial(t)	((uint)(t) >> TAG_IMMED3_SIZE)
#define oid_serial(t)	((uint)(t) >> TAG_IMMED3_SIZE)
#define float_value(t)		(peel(t)->float_value.value)

//
// Typical use:
//
// term_t t;
// term_box_t *b = peel(t);
// b->tuple.size = ...
//

#define FLOAT_VALUE_T_SIZE		APR_ALIGN_DEFAULT(sizeof(float_value_t))
#define BIGNUM_T_SIZE(ndigits)	APR_ALIGN_DEFAULT(sizeof(bignum_t) + sizeof(uint)*ndigits)
#define BINARY_T_SIZE			APR_ALIGN_DEFAULT(sizeof(binary_t))
#define FUN_T_SIZE				APR_ALIGN_DEFAULT(sizeof(fun_t))
#define LONG_ID_T_SIZE			APR_ALIGN_DEFAULT(sizeof(long_id_t))
#define TUPLE_T_SIZE(nelems)	APR_ALIGN_DEFAULT(sizeof(tuple_t) + sizeof(term_t)*nelems)
#define CONS_T_SIZE				APR_ALIGN_DEFAULT(sizeof(cons_t))

typedef struct float_value_t float_value_t;
struct float_value_t {
	double value;
};

typedef struct bignum_t bignum_t;
struct bignum_t {
  mp_sign       sign;    /* sign of this quantity      */
  mp_size       alloc;   /* how many digits allocated  */
  mp_size       used;    /* how many digits used       */
  mp_digit      dp[0];      /* the digits themselves      */
};

typedef bignum_t *mp_int;

/* Macros for accessing the mp_int internals           */
#define  SIGN(MP)     ((*MP)->sign)
#define  USED(MP)     ((*MP)->used)
#define  ALLOC(MP)    ((*MP)->alloc)
#define  DIGITS(MP)   ((*MP)->dp)
#define  DIGIT(MP,N)  (*MP)->dp[(N)]

typedef struct binary_t binary_t;
struct binary_t {
	int bit_size;
	apr_byte_t *data;
	term_t parent;	// the term data are shared with; noval for root binaries
	int offset;		// offset from parent data, needed for gc
};

typedef struct fun_t fun_t;
struct fun_t {
	term_t module;
	term_t function;
	int arity;
	int index;
	uint uniq;
	term_t frozen;	// list of free var values
};

#define LONG_ID_TAG_MASK	1
#define LONG_ID_TAG_SIZE	1

#define tag_pid(c__)			((c__ << LONG_ID_TAG_SIZE) | 1)
#define tag_oid(c__)			((c__ << LONG_ID_TAG_SIZE) | 0)

//#define lid_creation(t_c__)		((t_c__) >> LONG_ID_TAG_SIZE)
#define lid_is_pid(t_c__)		(((t_c__) & LONG_ID_TAG_MASK) == 0)
#define lid_is_oid(t_c__)		(((t_c__) & LONG_ID_TAG_MASK) == 1)

#define is_long_pid(t)	(is_long_id(t) && lid_is_pid(peel(t)->long_id.tag_creation))
#define is_long_oid(t)	(is_long_id(t) && lid_is_oid(peel(t)->long_id.tag_creation))

typedef struct long_id_t long_id_t;
struct long_id_t {
	uint tag_creation;
	uint serial;
	term_t node;
};

typedef struct tuple_t tuple_t;
struct tuple_t {
	int size;
	term_t elts[0];
};

typedef struct cons_t cons_t;
struct cons_t {
	term_t head;
	term_t tail;
};

#define MAGIC_CROSS			0xd492e5f5

// this is the only one looking up farther than tag
#define is_grave(t)		(peel(t)->grave.cross == MAGIC_CROSS)

typedef struct grave_t grave_t;
struct grave_t {
	uint cross;
	term_t skeleton;
};

typedef union term_box_t term_box_t;
union term_box_t {
	float_value_t float_value;
	bignum_t bignum;
	binary_t binary;
	fun_t fun;
	long_id_t long_id;
	tuple_t tuple;
	cons_t cons;
	grave_t grave;
};

#endif
