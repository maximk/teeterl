/*
    mpi.c

    by Michael J. Fromberger <http://www.dartmouth.edu/~sting/>
    Copyright (C) 1998 Michael J. Fromberger, All Rights Reserved

    Arbitrary precision integer arithmetic library

    $Id: mpi.c,v 1.1 2004/02/08 04:29:29 sting Exp $
 */

#include "mpi.h"
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#if MP_DEBUG
#include <stdio.h>

#define DIAG(T,V) {fprintf(stderr,T);mp_print(V,stderr);fputc('\n',stderr);}
#else
#define DIAG(T,V)
#endif

/* 
   If MP_LOGTAB is not defined, use the math library to compute the
   logarithms on the fly.  Otherwise, use the static table below.
   Pick which works best for your system.
 */
#if MP_LOGTAB

/* {{{ s_logv_2[] - log table for 2 in various bases */

/*
  A table of the logs of 2 for various bases (the 0 and 1 entries of
  this table are meaningless and should not be referenced).  

  This table is used to compute output lengths for the mp_toradix()
  function.  Since a number n in radix r takes up about log_r(n)
  digits, we estimate the output size by taking the least integer
  greater than log_r(n), where:

  log_r(n) = log_2(n) * log_r(2)

  This table, therefore, is a table of log_r(2) for 2 <= r <= 36,
  which are the output bases supported.  
 */

#include "logtab.h"

/* }}} */
#define LOG_V_2(R)  s_logv_2[(R)]

#else

#include <math.h>
#define LOG_V_2(R)  (log(2.0)/log(R))

#endif

/* Default precision for newly created mp_int's      */
static unsigned int s_mp_defprec = MP_DEFPREC;

/* {{{ Digit arithmetic macros */

/*
  When adding and multiplying digits, the results can be larger than
  can be contained in an mp_digit.  Thus, an mp_word is used.  These
  macros mask off the upper and lower digits of the mp_word (the
  mp_word may be more than 2 mp_digits wide, but we only concern
  ourselves with the low-order 2 mp_digits)

  If your mp_word DOES have more than 2 mp_digits, you need to
  uncomment the first line, and comment out the second.
 */

/* #define  CARRYOUT(W)  (((W)>>DIGIT_BIT)&MP_DIGIT_MAX) */
#define  CARRYOUT(W)  ((W)>>DIGIT_BIT)
#define  ACCUM(W)     ((W)&MP_DIGIT_MAX)

/* }}} */

/* {{{ Comparison constants */

#define  MP_LT       -1
#define  MP_EQ        0
#define  MP_GT        1

/* }}} */

/* {{{ Constant strings */

/* Constant strings returned by mp_strerror() */
static const char *mp_err_string[] = {
  "unknown result code",     /* say what?            */
  "boolean true",            /* MP_OKAY, MP_YES      */
  "boolean false",           /* MP_NO                */
  "out of memory",           /* MP_MEM               */
  "argument out of range",   /* MP_RANGE             */
  "invalid input parameter", /* MP_BADARG            */
  "result is undefined"      /* MP_UNDEF             */
};

/* Value to digit maps for radix conversion   */

/* s_dmap_1 - standard digits and letters */
static const char *s_dmap_1 = 
  "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz+/";

#if 0
/* s_dmap_2 - base64 ordering for digits  */
static const char *s_dmap_2 =
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
#endif

/* }}} */

/* {{{ Static function declarations */

/* 
   If MP_MACRO is false, these will be defined as actual functions;
   otherwise, suitable macro definitions will be used.  This works
   around the fact that ANSI C89 doesn't support an 'inline' keyword
   (although I hear C9x will ... about bloody time).  At present, the
   macro definitions are identical to the function bodies, but they'll
   expand in place, instead of generating a function call.

   I chose these particular functions to be made into macros because
   some profiling showed they are called a lot on a typical workload,
   and yet they are primarily housekeeping.
 */
#if MP_MACRO == 0
 void     s_mp_setz(mp_digit *dp, mp_size count); /* zero digits           */
 void     s_mp_copy(mp_digit *sp, mp_digit *dp, mp_size count); /* copy    */
 void    *s_mp_alloc(size_t nb, size_t ni);       /* general allocator     */
 void     s_mp_free(void *ptr);                   /* general free function */
#else

 /* Even if these are defined as macros, we need to respect the settings
    of the MP_MEMSET and MP_MEMCPY configuration options...
  */
 #if MP_MEMSET == 0
  #define  s_mp_setz(dp, count) \
       {int ix;for(ix=0;ix<(count);ix++)(dp)[ix]=0;}
 #else
  #define  s_mp_setz(dp, count) memset(dp, 0, (count) * sizeof(mp_digit))
 #endif /* MP_MEMSET */

 #if MP_MEMCPY == 0
  #define  s_mp_copy(sp, dp, count) \
       {int ix;for(ix=0;ix<(count);ix++)(dp)[ix]=(sp)[ix];}
 #else
  #define  s_mp_copy(sp, dp, count) memcpy(dp, sp, (count) * sizeof(mp_digit))
 #endif /* MP_MEMCPY */

 #define  s_mp_alloc(nb, ni)  calloc(nb, ni)
 #define  s_mp_free(ptr) {if(ptr) free(ptr);}
#endif /* MP_MACRO */

mp_err   s_mp_grow(mp_int *mp, mp_size min, heap_t *hp);   /* increase allocated size */
mp_err   s_mp_pad(mp_int *mp, mp_size min, heap_t *hp);    /* left pad with zeroes    */

void     s_mp_clamp(mp_int *mp);               /* clip leading zeroes     */

void     s_mp_exch(mp_int *a, mp_int *b);      /* swap a and b in place   */

mp_err   s_mp_lshd(mp_int *mp, mp_size p, heap_t *hp);     /* left-shift by p digits  */
void     s_mp_rshd(mp_int *mp, mp_size p);     /* right-shift by p digits */
void     s_mp_div_2d(mp_int *mp, mp_digit d);  /* divide by 2^d in place  */
void     s_mp_mod_2d(mp_int *mp, mp_digit d);  /* modulo 2^d in place     */
mp_err   s_mp_mul_2d(mp_int *mp, mp_digit d, heap_t *hp);  /* multiply by 2^d in place*/
void     s_mp_div_2(mp_int *mp);               /* divide by 2 in place    */
mp_err   s_mp_mul_2(mp_int *mp, heap_t *hp);               /* multiply by 2 in place  */
mp_digit s_mp_norm(mp_int *a, mp_int *b, heap_t *hp);      /* normalize for division  */
mp_err   s_mp_add_d(mp_int *mp, mp_digit d, heap_t *hp);   /* unsigned digit addition */
mp_err   s_mp_sub_d(mp_int *mp, mp_digit d);   /* unsigned digit subtract */
mp_err   s_mp_mul_d(mp_int *mp, mp_digit d, heap_t *hp);   /* unsigned digit multiply */
mp_err   s_mp_div_d(mp_int *mp, mp_digit d, mp_digit *r, heap_t *hp);
		                               /* unsigned digit divide   */
mp_err   s_mp_add(mp_int *a, mp_int *b, heap_t *hp);       /* magnitude addition      */
mp_err   s_mp_sub(mp_int *a, mp_int *b);       /* magnitude subtract      */
mp_err   s_mp_mul(mp_int *a, mp_int *b, heap_t *hp);       /* magnitude multiply      */
#if 0
void     s_mp_kmul(mp_digit *a, mp_digit *b, mp_digit *out, mp_size len);
                                               /* multiply buffers in place */
#endif
#if MP_SQUARE
mp_err   s_mp_sqr(mp_int *a);                  /* magnitude square        */
#else
#define  s_mp_sqr(a, hp) s_mp_mul(a, a, hp)
#endif
mp_err   s_mp_div(mp_int *a, mp_int *b, heap_t *hp);       /* magnitude divide        */
mp_err   s_mp_2expt(mp_int *a, mp_digit k, heap_t *hp);    /* a = 2^k                 */
int      s_mp_cmp(mp_int *a, mp_int *b);       /* magnitude comparison    */
int      s_mp_cmp_d(mp_int *a, mp_digit d);    /* magnitude digit compare */
int      s_mp_ispow2(mp_int *v);               /* is v a power of 2?      */
int      s_mp_ispow2d(mp_digit d);             /* is d a power of 2?      */

int      s_mp_tovalue(char ch, int r);          /* convert ch to value    */
char     s_mp_todigit(int val, int r, int low); /* convert val to digit   */
int      s_mp_outlen(int bits, int r);          /* output length in bytes */

/* }}} */

/* mp_int representation as terms */

// result is_int or is_bignum
term_t mp_to_term(mp_int ma)
{
	if (USED(&ma) <= 2)
	{
		int z = (int)mp_get_int(&ma);
		if (fits_int(z))
			return tag_int(z);
	}
	return tag_bignum(ma);
}

// t is_int or is_bignum
mp_int term_to_mp(term_t t, heap_t *hp)
{
	mp_int mp;
	assert(is_int(t) || is_bignum(t));
	if (is_int(t))
	{
		mp_init_size(&mp, 2, hp);
		mp_set_int(&mp, int_value(t), hp);
	}
	else
		mp = (mp_int)peel(t);
	return mp;
}

term_t int_to_term(long z, heap_t *hp)
{
	if (fits_int(z))
		return tag_int((int)z);
	else
	{
		mp_int mp;
		mp_init_size(&mp, 4, hp);
		mp_set_int(&mp, z, hp);
		return tag_bignum(mp);
	}
}

mp_int bignum_to_mp(term_t b)
{
	assert(is_bignum(b));
	return (mp_int)peel(b);
}

double term_to_double(term_t t)
{
	mp_int ma;
	assert(is_int(t) || is_bignum(t) || is_float(t));
	if (is_int(t))
		return (double)int_value(t);
	if (is_float(t))
		return float_value(t);
	//is_bignum
	ma = bignum_to_mp(t);
	return mp_get_double(&ma);
}

/* {{{ Default precision manipulation */

unsigned int mp_get_prec(void)
{
  return s_mp_defprec;

} /* end mp_get_prec() */

void         mp_set_prec(unsigned int prec)
{
  if(prec == 0)
    s_mp_defprec = MP_DEFPREC;
  else
    s_mp_defprec = prec;

} /* end mp_set_prec() */

/* }}} */

/*------------------------------------------------------------------------*/
/* {{{ mp_init_size(mp, prec) */

/*
  mp_init_size(mp, prec)

  Initialize a new zero-valued mp_int with at least the given
  precision; returns MP_OKAY if successful, or MP_MEM if memory could
  not be allocated for the structure.
 */

mp_err mp_init_size(mp_int *mp, mp_size prec, heap_t *hp)
{
  ARGCHK(mp != NULL && prec > 0, MP_BADARG);

  *mp = bignum_to_mp(heap_bignum0(hp, MP_ZPOS, prec));

  return MP_OKAY;

} /* end mp_init_size() */

/* }}} */

/* {{{ mp_init_copy(mp, from) */

/*
  mp_init_copy(mp, from)

  Initialize mp as an exact copy of from.  Returns MP_OKAY if
  successful, MP_MEM if memory could not be allocated for the new
  structure.
 */

mp_err mp_init_copy(mp_int *mp, mp_int *from, heap_t *hp)
{
  ARGCHK(mp != NULL && from != NULL, MP_BADARG);

  if(mp == from)
    return MP_OKAY;

  *mp = bignum_to_mp(heap_bignum(hp, SIGN(from), USED(from), DIGITS(from)));

  return MP_OKAY;

} /* end mp_init_copy() */

/* }}} */

/* {{{ mp_copy(from, to) */

/*
  mp_copy(from, to)

  Copies the mp_int 'from' to the mp_int 'to'.  It is presumed that
  'to' has already been initialized (if not, use mp_init_copy()
  instead). If 'from' and 'to' are identical, nothing happens.
 */

mp_err mp_copy(mp_int *from, mp_int *to, heap_t *hp)
{
  ARGCHK(from != NULL && to != NULL, MP_BADARG);

  if(from == to)
    return MP_OKAY;

  { /* copy */
    /*
      If the allocated buffer in 'to' already has enough space to hold
      all the used digits of 'from', we'll re-use it to avoid hitting
      the memory allocater more than necessary; otherwise, we'd have
      to grow anyway, so we just allocate a hunk and make the copy as
      usual
     */
    if(ALLOC(to) >= USED(from)) {
      s_mp_setz(DIGITS(to) + USED(from), ALLOC(to) - USED(from));
      s_mp_copy(DIGITS(from), DIGITS(to), USED(from));

      /* Copy the precision and sign from the original */
	  USED(to) = USED(from);
	  SIGN(to) = SIGN(from);
      
    } else {
		*to = bignum_to_mp(heap_bignum(hp, SIGN(from), USED(from), DIGITS(from)));
    }

  } /* end copy */

  return MP_OKAY;

} /* end mp_copy() */

/* }}} */

/* {{{ mp_exch(mp1, mp2) */

/*
  mp_exch(mp1, mp2)

  Exchange mp1 and mp2 without allocating any intermediate memory
  (well, unless you count the stack space needed for this call and the
  locals it creates...).  This cannot fail.
 */

void mp_exch(mp_int *mp1, mp_int *mp2)
{
#if MP_ARGCHK == 2
  assert(mp1 != NULL && mp2 != NULL);
#else
  if(mp1 == NULL || mp2 == NULL)
    return;
#endif

  s_mp_exch(mp1, mp2);

} /* end mp_exch() */

/* }}} */

/* {{{ mp_zero(mp) */

/*
  mp_zero(mp) 

  Set mp to zero.  Does not change the allocated size of the structure,
  and therefore cannot fail (except on a bad argument, which we ignore)
 */
void   mp_zero(mp_int *mp)
{
  if(mp == NULL)
    return;

  s_mp_setz(DIGITS(mp), ALLOC(mp));
  USED(mp) = 1;
  SIGN(mp) = MP_ZPOS;

} /* end mp_zero() */

/* }}} */

/* {{{ mp_set(mp, d) */

void   mp_set(mp_int *mp, mp_digit d)
{
  if(mp == NULL)
    return;

  mp_zero(mp);
  DIGIT(mp, 0) = d;

} /* end mp_set() */

/* }}} */

/* {{{ mp_set_int(mp, z) */

mp_err mp_set_int(mp_int *mp, long z, heap_t *hp)
{
  int            ix;
  unsigned long  v = abs(z);
  mp_err         res;

  ARGCHK(mp != NULL, MP_BADARG);

  mp_zero(mp);
  if(z == 0)
    return MP_OKAY;  /* shortcut for zero */

  for(ix = sizeof(long) - 1; ix >= 0; ix--) {

    if((res = s_mp_mul_2d(mp, CHAR_BIT, hp)) != MP_OKAY)
      return res;

    res = s_mp_add_d(mp, 
		     (mp_digit)((v >> (ix * CHAR_BIT)) & UCHAR_MAX), hp);
    if(res != MP_OKAY)
      return res;

  }

  if(z < 0)
    SIGN(mp) = MP_NEG;

  return MP_OKAY;

} /* end mp_set_int() */

/* }}} */

/*------------------------------------------------------------------------*/
/* {{{ Digit arithmetic */

/* {{{ mp_add_d(a, d, b) */

/*
  mp_add_d(a, d, b)

  Compute the sum b = a + d, for a single digit d.  Respects the sign of
  its primary addend (single digits are unsigned anyway).
 */

mp_err mp_add_d(mp_int *a, mp_digit d, mp_int *b, heap_t *hp)
{
  mp_err   res = MP_OKAY;

  ARGCHK(a != NULL && b != NULL, MP_BADARG);

  if((res = mp_copy(a, b, hp)) != MP_OKAY)
    return res;

  if(SIGN(b) == MP_ZPOS) {
    res = s_mp_add_d(b, d, hp);
  } else if(s_mp_cmp_d(b, d) >= 0) {
    res = s_mp_sub_d(b, d);
  } else {
    SIGN(b) = MP_ZPOS;

    DIGIT(b, 0) = d - DIGIT(b, 0);
  }

  return res;

} /* end mp_add_d() */

/* }}} */

/* {{{ mp_sub_d(a, d, b) */

/*
  mp_sub_d(a, d, b)

  Compute the difference b = a - d, for a single digit d.  Respects the
  sign of its subtrahend (single digits are unsigned anyway).
 */

mp_err mp_sub_d(mp_int *a, mp_digit d, mp_int *b, heap_t *hp)
{
  mp_err   res;

  ARGCHK(a != NULL && b != NULL, MP_BADARG);

  if((res = mp_copy(a, b, hp)) != MP_OKAY)
    return res;

  if(SIGN(b) == MP_NEG) {
    if((res = s_mp_add_d(b, d, hp)) != MP_OKAY)
      return res;

  } else if(s_mp_cmp_d(b, d) >= 0) {
    if((res = s_mp_sub_d(b, d)) != MP_OKAY)
      return res;

  } else {
    mp_neg(b, b, hp);

    DIGIT(b, 0) = d - DIGIT(b, 0);
    SIGN(b) = MP_NEG;
  }

  if(s_mp_cmp_d(b, 0) == 0)
    SIGN(b) = MP_ZPOS;

  return MP_OKAY;

} /* end mp_sub_d() */

/* }}} */

/* {{{ mp_mul_d(a, d, b) */

/*
  mp_mul_d(a, d, b)

  Compute the product b = a * d, for a single digit d.  Respects the sign
  of its multiplicand (single digits are unsigned anyway)
 */

mp_err mp_mul_d(mp_int *a, mp_digit d, mp_int *b, heap_t *hp)
{
  mp_err  res;

  ARGCHK(a != NULL && b != NULL, MP_BADARG);

  if(d == 0) {
    mp_zero(b);
    return MP_OKAY;
  }

  if((res = mp_copy(a, b, hp)) != MP_OKAY)
    return res;

  res = s_mp_mul_d(b, d, hp);

  return res;

} /* end mp_mul_d() */

/* }}} */

/* {{{ mp_mul_2(a, c) */

mp_err mp_mul_2(mp_int *a, mp_int *c, heap_t *hp)
{
  mp_err  res;

  ARGCHK(a != NULL && c != NULL, MP_BADARG);

  if((res = mp_copy(a, c, hp)) != MP_OKAY)
    return res;

  return s_mp_mul_2(c, hp);

} /* end mp_mul_2() */

/* }}} */

/* {{{ mp_div_d(a, d, q, r) */

/*
  mp_div_d(a, d, q, r)

  Compute the quotient q = a / d and remainder r = a mod d, for a
  single digit d.  Respects the sign of its divisor (single digits are
  unsigned anyway).
 */

mp_err mp_div_d(mp_int *a, mp_digit d, mp_int *q, mp_digit *r, heap_t *hp)
{
  mp_err   res;
  mp_digit rem;
  int      pow;

  ARGCHK(a != NULL, MP_BADARG);

  if(d == 0)
    return MP_RANGE;

  /* Shortcut for powers of two ... */
  if((pow = s_mp_ispow2d(d)) >= 0) {
    mp_digit  mask;

    mask = (1 << pow) - 1;
    rem = DIGIT(a, 0) & mask;

    if(q) {
      mp_copy(a, q, hp);
      s_mp_div_2d(q, pow);
    }

    if(r)
      *r = rem;

    return MP_OKAY;
  }

  /*
    If the quotient is actually going to be returned, we'll try to
    avoid hitting the memory allocator by copying the dividend into it
    and doing the division there.  This can't be any _worse_ than
    always copying, and will sometimes be better (since it won't make
    another copy)

    If it's not going to be returned, we need to allocate a temporary
    to hold the quotient, which will just be discarded.
   */
  if(q) {
    if((res = mp_copy(a, q, hp)) != MP_OKAY)
      return res;

    res = s_mp_div_d(q, d, &rem, hp);
    if(s_mp_cmp_d(q, 0) == MP_EQ)
      SIGN(q) = MP_ZPOS;

  } else {
    mp_int  qp;

    if((res = mp_init_copy(&qp, a, hp)) != MP_OKAY)
      return res;

    res = s_mp_div_d(&qp, d, &rem, hp);
    if(s_mp_cmp_d(&qp, 0) == 0)
      SIGN(&qp) = MP_ZPOS;

    mp_clear(&qp);
  }

  if(r)
    *r = rem;

  return res;

} /* end mp_div_d() */

/* }}} */

/* {{{ mp_div_2(a, c) */

/*
  mp_div_2(a, c)

  Compute c = a / 2, disregarding the remainder.
 */

mp_err mp_div_2(mp_int *a, mp_int *c, heap_t *hp)
{
  mp_err  res;

  ARGCHK(a != NULL && c != NULL, MP_BADARG);

  if((res = mp_copy(a, c, hp)) != MP_OKAY)
    return res;

  s_mp_div_2(c);

  return MP_OKAY;

} /* end mp_div_2() */

/* }}} */

/*------------------------------------------------------------------------*/
/* {{{ Full arithmetic */

/* {{{ mp_abs(a, b) */

/*
  mp_abs(a, b)

  Compute b = |a|.  'a' and 'b' may be identical.
 */

mp_err mp_abs(mp_int *a, mp_int *b, heap_t *hp)
{
  mp_err   res;

  ARGCHK(a != NULL && b != NULL, MP_BADARG);

  if((res = mp_copy(a, b, hp)) != MP_OKAY)
    return res;

  SIGN(b) = MP_ZPOS;

  return MP_OKAY;

} /* end mp_abs() */

/* }}} */

/* {{{ mp_neg(a, b) */

/*
  mp_neg(a, b)

  Compute b = -a.  'a' and 'b' may be identical.
 */

mp_err mp_neg(mp_int *a, mp_int *b, heap_t *hp)
{
  mp_err   res;

  ARGCHK(a != NULL && b != NULL, MP_BADARG);

  if((res = mp_copy(a, b, hp)) != MP_OKAY)
    return res;

  if(s_mp_cmp_d(b, 0) == MP_EQ) 
    SIGN(b) = MP_ZPOS;
  else 
    SIGN(b) = (SIGN(b) == MP_NEG) ? MP_ZPOS : MP_NEG;

  return MP_OKAY;

} /* end mp_neg() */

/* }}} */

/* {{{ mp_add(a, b, c) */

/*
  mp_add(a, b, c)

  Compute c = a + b.  All parameters may be identical.
 */

mp_err mp_add(mp_int *a, mp_int *b, mp_int *c, heap_t *hp)
{
  mp_err  res;
  int     cmp;

  ARGCHK(a != NULL && b != NULL && c != NULL, MP_BADARG);

  if(SIGN(a) == SIGN(b)) { /* same sign:  add values, keep sign */

    /* Commutativity of addition lets us do this in either order,
       so we avoid having to use a temporary even if the result 
       is supposed to replace the output
     */
    if(c == b) {
      if((res = s_mp_add(c, a, hp)) != MP_OKAY)
	return res;
    } else {
      if(c != a && (res = mp_copy(a, c, hp)) != MP_OKAY)
	return res;

      if((res = s_mp_add(c, b, hp)) != MP_OKAY) 
	return res;
    }

  } else if((cmp = s_mp_cmp(a, b)) > 0) {  /* different sign: a > b   */

    /* If the output is going to be clobbered, we will use a temporary
       variable; otherwise, we'll do it without touching the memory 
       allocator at all, if possible
     */
    if(c == b) {
      mp_int  tmp;

      if((res = mp_init_copy(&tmp, a, hp)) != MP_OKAY)
	return res;
      if((res = s_mp_sub(&tmp, b)) != MP_OKAY) {
	mp_clear(&tmp);
	return res;
      }

      s_mp_exch(&tmp, c);
      mp_clear(&tmp);

    } else {

      if(c != a && (res = mp_copy(a, c, hp)) != MP_OKAY)
	return res;
      if((res = s_mp_sub(c, b)) != MP_OKAY)
	return res;

    }

  } else if(cmp == 0) {             /* different sign, a == b   */

    mp_zero(c);
    return MP_OKAY;

  } else {                          /* different sign: a < b    */

    /* See above... */
    if(c == a) {
      mp_int  tmp;

      if((res = mp_init_copy(&tmp, b, hp)) != MP_OKAY)
	return res;
      if((res = s_mp_sub(&tmp, a)) != MP_OKAY) {
	mp_clear(&tmp);
	return res;
      }

      s_mp_exch(&tmp, c);
      mp_clear(&tmp);

    } else {

      if(c != b && (res = mp_copy(b, c, hp)) != MP_OKAY)
	return res;
      if((res = s_mp_sub(c, a)) != MP_OKAY)
	return res;

    }
  }

  if(USED(c) == 1 && DIGIT(c, 0) == 0)
    SIGN(c) = MP_ZPOS;

  return MP_OKAY;

} /* end mp_add() */

/* }}} */

/* {{{ mp_sub(a, b, c) */

/*
  mp_sub(a, b, c)

  Compute c = a - b.  All parameters may be identical.
 */

mp_err mp_sub(mp_int *a, mp_int *b, mp_int *c, heap_t *hp)
{
  mp_err  res;
  int     cmp;

  ARGCHK(a != NULL && b != NULL && c != NULL, MP_BADARG);

  if(SIGN(a) != SIGN(b)) {
    if(c == a) {
      if((res = s_mp_add(c, b, hp)) != MP_OKAY)
	return res;
    } else {
      if(c != b && ((res = mp_copy(b, c, hp)) != MP_OKAY))
	return res;
      if((res = s_mp_add(c, a, hp)) != MP_OKAY)
	return res;
      SIGN(c) = SIGN(a);
    }

  } else if((cmp = s_mp_cmp(a, b)) > 0) { /* Same sign, a > b */
    if(c == b) {
      mp_int  tmp;

      if((res = mp_init_copy(&tmp, a, hp)) != MP_OKAY)
	return res;
      if((res = s_mp_sub(&tmp, b)) != MP_OKAY) {
	mp_clear(&tmp);
	return res;
      }
      s_mp_exch(&tmp, c);
      mp_clear(&tmp);

    } else {
      if(c != a && ((res = mp_copy(a, c, hp)) != MP_OKAY))
	return res;

      if((res = s_mp_sub(c, b)) != MP_OKAY)
	return res;
    }

  } else if(cmp == 0) {  /* Same sign, equal magnitude */
    mp_zero(c);
    return MP_OKAY;

  } else {               /* Same sign, b > a */
    if(c == a) {
      mp_int  tmp;

      if((res = mp_init_copy(&tmp, b, hp)) != MP_OKAY)
	return res;

      if((res = s_mp_sub(&tmp, a)) != MP_OKAY) {
	mp_clear(&tmp);
	return res;
      }
      s_mp_exch(&tmp, c);
      mp_clear(&tmp);

    } else {
      if(c != b && ((res = mp_copy(b, c, hp)) != MP_OKAY)) 
	return res;

      if((res = s_mp_sub(c, a)) != MP_OKAY)
	return res;
    }

    SIGN(c) = !SIGN(b);
  }

  if(USED(c) == 1 && DIGIT(c, 0) == 0)
    SIGN(c) = MP_ZPOS;

  return MP_OKAY;

} /* end mp_sub() */

/* }}} */

/* {{{ mp_mul(a, b, c) */

/*
  mp_mul(a, b, c)

  Compute c = a * b.  All parameters may be identical.
 */

mp_err mp_mul(mp_int *a, mp_int *b, mp_int *c, heap_t *hp)
{
  mp_err   res;
  mp_sign  sgn;

  ARGCHK(a != NULL && b != NULL && c != NULL, MP_BADARG);

  sgn = (SIGN(a) == SIGN(b)) ? MP_ZPOS : MP_NEG;

  if(c == b) {
    if((res = s_mp_mul(c, a, hp)) != MP_OKAY)
      return res;

  } else {
    if((res = mp_copy(a, c, hp)) != MP_OKAY)
      return res;

    if((res = s_mp_mul(c, b, hp)) != MP_OKAY)
      return res;
  }
  
  if(sgn == MP_ZPOS || s_mp_cmp_d(c, 0) == MP_EQ)
    SIGN(c) = MP_ZPOS;
  else
    SIGN(c) = sgn;
  
  return MP_OKAY;

} /* end mp_mul() */

/* }}} */

/* {{{ mp_mul_2d(a, d, c) */

/*
  mp_mul_2d(a, d, c)

  Compute c = a * 2^d.  a may be the same as c.
 */

mp_err mp_mul_2d(mp_int *a, mp_digit d, mp_int *c, heap_t *hp)
{
  mp_err   res;

  ARGCHK(a != NULL && c != NULL, MP_BADARG);

  if((res = mp_copy(a, c, hp)) != MP_OKAY)
    return res;

  if(d == 0)
    return MP_OKAY;

  return s_mp_mul_2d(c, d, hp);

} /* end mp_mul() */

/* }}} */

/* {{{ mp_div(a, b, q, r) */

/*
  mp_div(a, b, q, r)

  Compute q = a / b and r = a mod b.  Input parameters may be re-used
  as output parameters.  If q or r is NULL, that portion of the
  computation will be discarded (although it will still be computed)

  Pay no attention to the hacker behind the curtain.
 */

mp_err mp_div(mp_int *a, mp_int *b, mp_int *q, mp_int *r, heap_t *hp)
{
  mp_err   res;
  mp_int   qtmp, rtmp;
  int      cmp;

  ARGCHK(a != NULL && b != NULL, MP_BADARG);

  if(mp_cmp_z(b) == MP_EQ)
    return MP_RANGE;

  /* If a <= b, we can compute the solution without division, and
     avoid any memory allocation
   */
  if((cmp = s_mp_cmp(a, b)) < 0) {
    if(r) {
      if((res = mp_copy(a, r, hp)) != MP_OKAY)
	return res;
    }

    if(q) 
      mp_zero(q);

    return MP_OKAY;

  } else if(cmp == 0) {

    /* Set quotient to 1, with appropriate sign */
    if(q) {
      int qneg = (SIGN(a) != SIGN(b));

      mp_set(q, 1);
      if(qneg)
	SIGN(q) = MP_NEG;
    }

    if(r)
      mp_zero(r);

    return MP_OKAY;
  }

  /* If we get here, it means we actually have to do some division */

  /* Set up some temporaries... */
  if((res = mp_init_copy(&qtmp, a, hp)) != MP_OKAY)
    return res;
  if((res = mp_init_copy(&rtmp, b, hp)) != MP_OKAY)
    goto CLEANUP;

  if((res = s_mp_div(&qtmp, &rtmp, hp)) != MP_OKAY)
    goto CLEANUP;

  /* Compute the signs for the output  */
  SIGN(&rtmp) = SIGN(a); /* Sr = Sa              */
  if(SIGN(a) == SIGN(b))
    SIGN(&qtmp) = MP_ZPOS;  /* Sq = MP_ZPOS if Sa = Sb */
  else
    SIGN(&qtmp) = MP_NEG;   /* Sq = MP_NEG if Sa != Sb */

  if(s_mp_cmp_d(&qtmp, 0) == MP_EQ)
    SIGN(&qtmp) = MP_ZPOS;
  if(s_mp_cmp_d(&rtmp, 0) == MP_EQ)
    SIGN(&rtmp) = MP_ZPOS;

  /* Copy output, if it is needed      */
  if(q) 
    s_mp_exch(&qtmp, q);

  if(r) 
    s_mp_exch(&rtmp, r);

CLEANUP:
  mp_clear(&rtmp);
  mp_clear(&qtmp);

  return res;

} /* end mp_div() */

/* }}} */

/* {{{ mp_div_2d(a, d, q, r) */

mp_err mp_div_2d(mp_int *a, mp_digit d, mp_int *q, mp_int *r, heap_t *hp)
{
  mp_err  res;

  ARGCHK(a != NULL, MP_BADARG);

  if(q) {
    if((res = mp_copy(a, q, hp)) != MP_OKAY)
      return res;

    s_mp_div_2d(q, d);
  }

  if(r) {
    if((res = mp_copy(a, r, hp)) != MP_OKAY)
      return res;

    s_mp_mod_2d(r, d);
  }

  return MP_OKAY;

} /* end mp_div_2d() */

/* }}} */

/* {{{ mp_2expt(a, k) */

/* Compute a = 2^k */

mp_err mp_2expt(mp_int *a, mp_digit k, heap_t *hp)
{
  ARGCHK(a != NULL, MP_BADARG);

  return s_mp_2expt(a, k, hp);

} /* end mp_2expt() */

/* }}} */

/* {{{ mp_mod(a, m, c) */

/*
  mp_mod(a, m, c)

  Compute c = a (mod m).  Result will always be 0 <= c < m.
 */

mp_err mp_mod(mp_int *a, mp_int *m, mp_int *c, heap_t *hp)
{
  mp_err  res;
  int     mag;

  ARGCHK(a != NULL && m != NULL && c != NULL, MP_BADARG);

  if(SIGN(m) == MP_NEG)
    return MP_RANGE;

  /*
     If |a| > m, we need to divide to get the remainder and take the
     absolute value.  

     If |a| < m, we don't need to do any division, just copy and adjust
     the sign (if a is negative).

     If |a| == m, we can simply set the result to zero.

     This order is intended to minimize the average path length of the
     comparison chain on common workloads -- the most frequent cases are
     that |a| != m, so we do those first.
   */
  if((mag = s_mp_cmp(a, m)) > 0) {
    if((res = mp_div(a, m, NULL, c, hp)) != MP_OKAY)
      return res;
    
    if(SIGN(c) == MP_NEG) {
      if((res = mp_add(c, m, c, hp)) != MP_OKAY)
	return res;
    }

  } else if(mag < 0) {
    if((res = mp_copy(a, c, hp)) != MP_OKAY)
      return res;

    if(mp_cmp_z(a) < 0) {
      if((res = mp_add(c, m, c, hp)) != MP_OKAY)
	return res;

    }
    
  } else {
    mp_zero(c);

  }

  return MP_OKAY;

} /* end mp_mod() */

/* }}} */

/* {{{ mp_mod_d(a, d, c) */

/*
  mp_mod_d(a, d, c)

  Compute c = a (mod d).  Result will always be 0 <= c < d
 */
mp_err mp_mod_d(mp_int *a, mp_digit d, mp_digit *c, heap_t *hp)
{
  mp_err   res;
  mp_digit rem;

  ARGCHK(a != NULL && c != NULL, MP_BADARG);

  if(s_mp_cmp_d(a, d) > 0) {
    if((res = mp_div_d(a, d, NULL, &rem, hp)) != MP_OKAY)
      return res;

  } else {
    if(SIGN(a) == MP_NEG)
      rem = d - DIGIT(a, 0);
    else
      rem = DIGIT(a, 0);
  }

  if(c)
    *c = rem;

  return MP_OKAY;

} /* end mp_mod_d() */

/* }}} */

/*------------------------------------------------------------------------*/
/* {{{ Comparison functions */

/* {{{ mp_cmp_z(a) */

/*
  mp_cmp_z(a)

  Compare a <=> 0.  Returns <0 if a<0, 0 if a=0, >0 if a>0.
 */

int    mp_cmp_z(mp_int *a)
{
  if(SIGN(a) == MP_NEG)
    return MP_LT;
  else if(USED(a) == 1 && DIGIT(a, 0) == 0)
    return MP_EQ;
  else
    return MP_GT;

} /* end mp_cmp_z() */

/* }}} */

/* {{{ mp_cmp_d(a, d) */

/*
  mp_cmp_d(a, d)

  Compare a <=> d.  Returns <0 if a<d, 0 if a=d, >0 if a>d
 */

int    mp_cmp_d(mp_int *a, mp_digit d)
{
  ARGCHK(a != NULL, MP_EQ);

  if(SIGN(a) == MP_NEG)
    return MP_LT;

  return s_mp_cmp_d(a, d);

} /* end mp_cmp_d() */

/* }}} */

/* {{{ mp_cmp(a, b) */

int    mp_cmp(mp_int *a, mp_int *b)
{
  ARGCHK(a != NULL && b != NULL, MP_EQ);

  if(SIGN(a) == SIGN(b)) {
    int  mag;

    if((mag = s_mp_cmp(a, b)) == MP_EQ)
      return MP_EQ;

    if(SIGN(a) == MP_ZPOS)
      return mag;
    else
      return -mag;

  } else if(SIGN(a) == MP_ZPOS) {
    return MP_GT;
  } else {
    return MP_LT;
  }

} /* end mp_cmp() */

/* }}} */

/* {{{ mp_cmp_mag(a, b) */

/*
  mp_cmp_mag(a, b)

  Compares |a| <=> |b|, and returns an appropriate comparison result
 */

int    mp_cmp_mag(mp_int *a, mp_int *b)
{
  ARGCHK(a != NULL && b != NULL, MP_EQ);

  return s_mp_cmp(a, b);

} /* end mp_cmp_mag() */

/* }}} */

/* {{{ mp_cmp_int(a, z) */

/*
  This just converts z to an mp_int, and uses the existing comparison
  routines.  This is sort of inefficient, but it's not clear to me how
  frequently this wil get used anyway.  For small positive constants,
  you can always use mp_cmp_d(), and for zero, there is mp_cmp_z().
 */
int    mp_cmp_int(mp_int *a, long z, heap_t *hp)
{
  mp_int  tmp;
  int     out;

  ARGCHK(a != NULL, MP_EQ);
  
  mp_init_size(&tmp, 4, hp); mp_set_int(&tmp, z, hp);
  out = mp_cmp(a, &tmp);
  mp_clear(&tmp);

  return out;

} /* end mp_cmp_int() */

/* }}} */

/* {{{ mp_isodd(a) */

/*
  mp_isodd(a)

  Returns a true (non-zero) value if a is odd, false (zero) otherwise.
 */
int    mp_isodd(mp_int *a)
{
  ARGCHK(a != NULL, 0);

  return (DIGIT(a, 0) & 1);

} /* end mp_isodd() */

/* }}} */

/* {{{ mp_iseven(a) */

int    mp_iseven(mp_int *a)
{
  return !mp_isodd(a);

} /* end mp_iseven() */

/* }}} */

/*------------------------------------------------------------------------*/
/* {{{ More I/O Functions */

/* {{{ mp_read_signed_bin(mp, str, len) */

/* 
   mp_read_signed_bin(mp, str, len)

   Read in a raw value (base 256) into the given mp_int
 */

mp_err  mp_read_signed_bin(mp_int *mp, unsigned char *str, int len, heap_t *hp)
{
  mp_err         res;

  ARGCHK(mp != NULL && str != NULL && len > 0, MP_BADARG);

  if((res = mp_read_unsigned_bin(mp, str + 1, len - 1, hp)) == MP_OKAY) {
    /* Get sign from first byte */
    if(str[0])
      SIGN(mp) = MP_NEG;
    else
      SIGN(mp) = MP_ZPOS;
  }

  return res;

} /* end mp_read_signed_bin() */

/* }}} */

/* {{{ mp_signed_bin_size(mp) */

int    mp_signed_bin_size(mp_int *mp)
{
  ARGCHK(mp != NULL, 0);

  return mp_unsigned_bin_size(mp) + 1;

} /* end mp_signed_bin_size() */

/* }}} */

/* {{{ mp_to_signed_bin(mp, str) */

mp_err mp_to_signed_bin(mp_int *mp, unsigned char *str)
{
  ARGCHK(mp != NULL && str != NULL, MP_BADARG);

  /* Caller responsible for allocating enough memory (use mp_raw_size(mp)) */
  str[0] = (char)SIGN(mp);

  return mp_to_unsigned_bin(mp, str + 1);

} /* end mp_to_signed_bin() */

/* }}} */

/* {{{ mp_read_unsigned_bin(mp, str, len) */

/*
  mp_read_unsigned_bin(mp, str, len)

  Read in an unsigned value (base 256) into the given mp_int
 */

mp_err  mp_read_unsigned_bin(mp_int *mp, unsigned char *str, int len, heap_t *hp)
{
  int     ix;
  mp_err  res;

  ARGCHK(mp != NULL && str != NULL && len > 0, MP_BADARG);

  mp_zero(mp);

  for(ix = 0; ix < len; ix++) {
    if((res = s_mp_mul_2d(mp, CHAR_BIT, hp)) != MP_OKAY)
      return res;

    if((res = mp_add_d(mp, str[ix], mp, hp)) != MP_OKAY)
      return res;
  }
  
  return MP_OKAY;
  
} /* end mp_read_unsigned_bin() */

/* }}} */

/* {{{ mp_read_unsigned_bin(mp, str, len) */

/*
  mp_read_unsigned_bin_lsb(mp, str, len)

  Read in an unsigned value (base 256) into the given mp_int, LSB first
 */

mp_err  mp_read_unsigned_bin_lsb(mp_int *mp, unsigned char *str, int len, heap_t *hp)
{
  int     ix;
  mp_err  res;

  ARGCHK(mp != NULL && str != NULL && len > 0, MP_BADARG);

  mp_zero(mp);

  for(ix = len-1; ix >= 0; ix--) {
    if((res = s_mp_mul_2d(mp, CHAR_BIT, hp)) != MP_OKAY)
      return res;

    if((res = mp_add_d(mp, str[ix], mp, hp)) != MP_OKAY)
      return res;
  }
  
  return MP_OKAY;
  
} /* end mp_read_unsigned_bin_lsb() */

/* }}} */

/* {{{ mp_unsigned_bin_size(mp) */

int     mp_unsigned_bin_size(mp_int *mp) 
{
  mp_digit   topdig;
  int        count;

  ARGCHK(mp != NULL, 0);

  /* Special case for the value zero */
  if(USED(mp) == 1 && DIGIT(mp, 0) == 0)
    return 1;

  count = (USED(mp) - 1) * sizeof(mp_digit);
  topdig = DIGIT(mp, USED(mp) - 1);

  while(topdig != 0) {
    ++count;
    topdig >>= CHAR_BIT;
  }

  return count;

} /* end mp_unsigned_bin_size() */

/* }}} */

/* {{{ mp_to_unsigned_bin(mp, str) */

mp_err mp_to_unsigned_bin(mp_int *mp, unsigned char *str)
{
  mp_digit      *dp, *end, d;
  unsigned char *spos;

  ARGCHK(mp != NULL && str != NULL, MP_BADARG);

  dp = DIGITS(mp);
  end = dp + USED(mp) - 1;
  spos = str;

  /* Special case for zero, quick test */
  if(dp == end && *dp == 0) {
    *str = '\0';
    return MP_OKAY;
  }

  /* Generate digits in reverse order */
  while(dp < end) {
    int      ix;

    d = *dp;
    for(ix = 0; ix < sizeof(mp_digit); ++ix) {
      *spos = d & UCHAR_MAX;
      d >>= CHAR_BIT;
      ++spos;
    }

    ++dp;
  }

  /* Now handle last digit specially, high order zeroes are not written */
  d = *end;
  while(d != 0) {
    *spos = d & UCHAR_MAX;
    d >>= CHAR_BIT;
    ++spos;
  }

  /* Reverse everything to get digits in the correct order */
  while(--spos > str) {
    unsigned char t = *str;
    *str = *spos;
    *spos = t;

    ++str;
  }

  return MP_OKAY;

} /* end mp_to_unsigned_bin() */

/* }}} */

/* {{{ mp_to_unsigned_bin_lsb(mp, str) */

mp_err mp_to_unsigned_bin_lsb(mp_int *mp, unsigned char *str)
{
  mp_digit      *dp, *end, d;
  unsigned char *spos;

  ARGCHK(mp != NULL && str != NULL, MP_BADARG);

  dp = DIGITS(mp);
  end = dp + USED(mp) - 1;
  spos = str;

  /* Special case for zero, quick test */
  if(dp == end && *dp == 0) {
    *str = '\0';
    return MP_OKAY;
  }

  /* Generate digits in reverse order */
  while(dp < end) {
    int      ix;

    d = *dp;
    for(ix = 0; ix < sizeof(mp_digit); ++ix) {
      *spos = d & UCHAR_MAX;
      d >>= CHAR_BIT;
      ++spos;
    }

    ++dp;
  }

  /* Now handle last digit specially, high order zeroes are not written */
  d = *end;
  while(d != 0) {
    *spos = d & UCHAR_MAX;
    d >>= CHAR_BIT;
    ++spos;
  }

  return MP_OKAY;

} /* end mp_to_unsigned_bin() */

/* }}} */

/* {{{ mp_count_bits(mp) */

int    mp_count_bits(mp_int *mp)
{
  int      len;
  mp_digit d;

  ARGCHK(mp != NULL, MP_BADARG);

  len = DIGIT_BIT * (USED(mp) - 1);
  d = DIGIT(mp, USED(mp) - 1);

  while(d != 0) {
    ++len;
    d >>= 1;
  }

  return len;
  
} /* end mp_count_bits() */

/* }}} */

/* {{{ mp_read_radix(mp, str, radix) */

/*
  mp_read_radix(mp, str, radix)

  Read an integer from the given string, and set mp to the resulting
  value.  The input is presumed to be in base 10.  Leading non-digit
  characters are ignored, and the function reads until a non-digit
  character or the end of the string.
 */

mp_err  mp_read_radix(mp_int *mp, unsigned char *str, int radix, heap_t *hp)
{
  int     ix = 0, val = 0;
  mp_err  res;
  mp_sign sig = MP_ZPOS;

  ARGCHK(mp != NULL && str != NULL && radix >= 2 && radix <= MAX_RADIX, 
	 MP_BADARG);

  mp_zero(mp);

  /* Skip leading non-digit characters until a digit or '-' or '+' */
  while(str[ix] && 
	(s_mp_tovalue(str[ix], radix) < 0) && 
	str[ix] != '-' &&
	str[ix] != '+') {
    ++ix;
  }

  if(str[ix] == '-') {
    sig = MP_NEG;
    ++ix;
  } else if(str[ix] == '+') {
    sig = MP_ZPOS; /* this is the default anyway... */
    ++ix;
  }

  while((val = s_mp_tovalue(str[ix], radix)) >= 0) {
    if((res = s_mp_mul_d(mp, radix, hp)) != MP_OKAY)
      return res;
    if((res = s_mp_add_d(mp, val, hp)) != MP_OKAY)
      return res;
    ++ix;
  }

  if(s_mp_cmp_d(mp, 0) == MP_EQ)
    SIGN(mp) = MP_ZPOS;
  else
    SIGN(mp) = sig;

  return MP_OKAY;

} /* end mp_read_radix() */

/* }}} */

/* {{{ mp_radix_size(mp, radix) */

int    mp_radix_size(mp_int *mp, int radix)
{
  int  len;
  ARGCHK(mp != NULL, 0);

  len = s_mp_outlen(mp_count_bits(mp), radix) + 1; /* for NUL terminator */

  if(mp_cmp_z(mp) < 0)
    ++len; /* for sign */

  return len;

} /* end mp_radix_size() */

/* }}} */

/* {{{ mp_value_radix_size(num, qty, radix) */

/* num = number of digits
   qty = number of bits per digit
   radix = target base
   
   Return the number of digits in the specified radix that would be
   needed to express 'num' digits of 'qty' bits each.
 */
int    mp_value_radix_size(int num, int qty, int radix)
{
  ARGCHK(num >= 0 && qty > 0 && radix >= 2 && radix <= MAX_RADIX, 0);

  return s_mp_outlen(num * qty, radix);

} /* end mp_value_radix_size() */

/* }}} */

/* {{{ mp_toradix(mp, str, radix) */

mp_err mp_toradix(mp_int *mp, unsigned char *str, int radix, heap_t *hp)
{
  int  ix, pos = 0;

  ARGCHK(mp != NULL && str != NULL, MP_BADARG);
  ARGCHK(radix > 1 && radix <= MAX_RADIX, MP_RANGE);

  if(mp_cmp_z(mp) == MP_EQ) {
    str[0] = '0';
    str[1] = '\0';
  } else {
    mp_err   res;
    mp_int   tmp;
    mp_sign  sgn;
    mp_digit rem, rdx = (mp_digit)radix;
    char     ch;

    if((res = mp_init_copy(&tmp, mp, hp)) != MP_OKAY)
      return res;

    /* Save sign for later, and take absolute value */
    sgn = SIGN(&tmp); SIGN(&tmp) = MP_ZPOS;

    /* Generate output digits in reverse order      */
    while(mp_cmp_z(&tmp) != 0) {
      if((res = s_mp_div_d(&tmp, rdx, &rem, hp)) != MP_OKAY) {
	mp_clear(&tmp);
	return res;
      }

      /* Generate digits, use capital letters */
      ch = s_mp_todigit(rem, radix, 0);

      str[pos++] = ch;
    }

    /* Add - sign if original value was negative */
    if(sgn == MP_NEG)
      str[pos++] = '-';

    /* Add trailing NUL to end the string        */
    str[pos--] = '\0';

    /* Reverse the digits and sign indicator     */
    ix = 0;
    while(ix < pos) {
      char tmp = str[ix];

      str[ix] = str[pos];
      str[pos] = tmp;
      ++ix;
      --pos;
    }
    
    mp_clear(&tmp);
  }

  return MP_OKAY;

} /* end mp_toradix() */

/* }}} */

/* {{{ mp_char2value(ch, r) */

int    mp_char2value(char ch, int r)
{
  return s_mp_tovalue(ch, r);

} /* end mp_tovalue() */

/* }}} */

long	mp_get_int(mp_int *mp)
{
	long value = 0;
	int ix;
	for (ix = USED(mp)-1; ix >= 0; ix--)
	{
		value <<= MP_DIGIT_BIT;
		value += DIGIT(mp, ix);
	}
	if (mp_cmp_z(mp) < 0)
		value = -value;
	return value;
}

double	mp_get_double(mp_int *mp)
{
	double value = 0;
	int ix;
	for (ix = USED(mp)-1; ix >= 0; ix--)
	{
		value *= (1ull << MP_DIGIT_BIT);
		value += DIGIT(mp, ix);
	}
	if (mp_cmp_z(mp) < 0)
		value = -value;
	return value;
}

/* {{{ mp_strerror(ec) */

/*
  mp_strerror(ec)

  Return a string describing the meaning of error code 'ec'.  The
  string returned is allocated in static memory, so the caller should
  not attempt to modify or free the memory associated with this
  string.
 */
const char  *mp_strerror(mp_err ec)
{
  int   aec = (ec < 0) ? -ec : ec;

  /* Code values are negative, so the senses of these comparisons
     are accurate */
  if(ec < MP_LAST_CODE || ec > MP_OKAY) {
    return mp_err_string[0];  /* unknown error code */
  } else {
    return mp_err_string[aec + 1];
  }

} /* end mp_strerror() */

/* }}} */

/*========================================================================*/
/*------------------------------------------------------------------------*/
/* Static function definitions (internal use only)                        */

/* {{{ Memory management */

/* {{{ s_mp_grow(mp, min, hp) */

/* Make sure there are at least 'min' digits allocated to mp              */
mp_err   s_mp_grow(mp_int *mp, mp_size min, heap_t *hp)
{
  if(min > ALLOC(mp)) {
    mp_int tmp;

    /* Set min to next nearest default precision block size */
    min = ((min + (s_mp_defprec - 1)) / s_mp_defprec) * s_mp_defprec;

	tmp = bignum_to_mp(heap_bignum0(hp, SIGN(mp), min));
    s_mp_copy(DIGITS(mp), DIGITS(&tmp), USED(mp));
	USED(&tmp) = USED(mp);
	*mp = tmp;
  }

  return MP_OKAY;

} /* end s_mp_grow() */

/* }}} */

/* {{{ s_mp_pad(mp, min) */

/* Make sure the used size of mp is at least 'min', growing if needed     */
mp_err   s_mp_pad(mp_int *mp, mp_size min, heap_t *hp)
{
  if(min > USED(mp)) {
    mp_err  res;

    /* Make sure there is room to increase precision  */
    if(min > ALLOC(mp) && (res = s_mp_grow(mp, min, hp)) != MP_OKAY)
      return res;

	s_mp_setz(DIGITS(mp) + USED(mp), min - USED(mp));
    /* Increase precision */
    USED(mp) = min;
  }

  return MP_OKAY;

} /* end s_mp_pad() */

/* }}} */

/* {{{ s_mp_setz(dp, count) */

#if MP_MACRO == 0
/* Set 'count' digits pointed to by dp to be zeroes                       */
void s_mp_setz(mp_digit *dp, mp_size count)
{
#if MP_MEMSET == 0
  int  ix;

  for(ix = 0; ix < count; ix++)
    dp[ix] = 0;
#else
  memset(dp, 0, count * sizeof(mp_digit));
#endif

} /* end s_mp_setz() */
#endif

/* }}} */

/* {{{ s_mp_copy(sp, dp, count) */

#if MP_MACRO == 0
/* Copy 'count' digits from sp to dp                                      */
void s_mp_copy(mp_digit *sp, mp_digit *dp, mp_size count)
{
#if MP_MEMCPY == 0
  int  ix;

  for(ix = 0; ix < count; ix++)
    dp[ix] = sp[ix];
#else
  memcpy(dp, sp, count * sizeof(mp_digit));
#endif

} /* end s_mp_copy() */
#endif

/* }}} */

/* {{{ s_mp_alloc(nb, ni) */

#if MP_MACRO == 0
/* Allocate ni records of nb bytes each, and return a pointer to that     */
void    *s_mp_alloc(size_t nb, size_t ni)
{
  return calloc(nb, ni);

} /* end s_mp_alloc() */
#endif

/* }}} */

/* {{{ s_mp_free(ptr) */

#if MP_MACRO == 0
/* Free the memory pointed to by ptr                                      */
void     s_mp_free(void *ptr)
{
  if(ptr)
    free(ptr);

} /* end s_mp_free() */
#endif

/* }}} */

/* {{{ s_mp_clamp(mp) */

/* Remove leading zeroes from the given value                             */
void     s_mp_clamp(mp_int *mp)
{
  mp_size   du = USED(mp);
  mp_digit *zp = DIGITS(mp) + du - 1;

  while(du > 1 && !*zp--)
    --du;

  if(du == 1 && *zp == 0)
    SIGN(mp) = MP_ZPOS;

  USED(mp) = du;

} /* end s_mp_clamp() */


/* }}} */

/* {{{ s_mp_exch(a, b) */

/* Exchange the data for a and b; (b, a) = (a, b)                         */
void     s_mp_exch(mp_int *a, mp_int *b)
{
  mp_int   tmp;

  tmp = *a;
  *a = *b;
  *b = tmp;

} /* end s_mp_exch() */

/* }}} */

/* }}} */

/* {{{ Arithmetic helpers */

/* {{{ s_mp_lshd(mp, p) */

/* 
   Shift mp leftward by p digits, growing if needed, and zero-filling
   the in-shifted digits at the right end.  This is a convenient
   alternative to multiplication by powers of the radix
 */   

mp_err   s_mp_lshd(mp_int *mp, mp_size p, heap_t *hp)
{
  mp_err   res;
  mp_size  pos;
  mp_digit *dp;
  int     ix;

  if(p == 0)
    return MP_OKAY;

  if((res = s_mp_pad(mp, USED(mp) + p, hp)) != MP_OKAY)
    return res;

  pos = USED(mp) - 1;
  dp = DIGITS(mp);

  /* Shift all the significant figures over as needed */
  for(ix = pos - p; ix >= 0; ix--) 
    dp[ix + p] = dp[ix];

  /* Fill the bottom digits with zeroes */
  for(ix = 0; ix < (int)p; ix++)
    dp[ix] = 0;

  return MP_OKAY;

} /* end s_mp_lshd() */

/* }}} */

/* {{{ s_mp_rshd(mp, p) */

/* 
   Shift mp rightward by p digits.  Maintains the invariant that
   digits above the precision are all zero.  Digits shifted off the
   end are lost.  Cannot fail.
 */

void     s_mp_rshd(mp_int *mp, mp_size p)
{
  mp_size  ix;
  mp_digit *dp;

  if(p == 0)
    return;

  /* Shortcut when all digits are to be shifted off */
  if(p >= USED(mp)) {
    s_mp_setz(DIGITS(mp), ALLOC(mp));
    USED(mp) = 1;
    SIGN(mp) = MP_ZPOS;
    return;
  }

  /* Shift all the significant figures over as needed */
  dp = DIGITS(mp);
  for(ix = p; ix < USED(mp); ix++)
    dp[ix - p] = dp[ix];

  /* Fill the top digits with zeroes */
  ix -= p;
  while(ix < USED(mp))
    dp[ix++] = 0;

  /* Strip off any leading zeroes    */
  s_mp_clamp(mp);

} /* end s_mp_rshd() */

/* }}} */

/* {{{ s_mp_div_2(mp) */

/* Divide by two -- take advantage of radix properties to do it fast      */
void     s_mp_div_2(mp_int *mp)
{
  s_mp_div_2d(mp, 1);

} /* end s_mp_div_2() */

/* }}} */

/* {{{ s_mp_mul_2(mp) */

mp_err s_mp_mul_2(mp_int *mp, heap_t *hp)
{
  int      ix;
  mp_digit kin = 0, kout, *dp = DIGITS(mp);
  mp_err   res;

  /* Shift digits leftward by 1 bit */
  for(ix = 0; ix < (int)USED(mp); ix++) {
    kout = (dp[ix] >> (DIGIT_BIT - 1)) & 1;
    dp[ix] = (dp[ix] << 1) | kin;

    kin = kout;
  }

  /* Deal with rollover from last digit */
  if(kin) {
    if(ix >= (int)ALLOC(mp)) {
      if((res = s_mp_grow(mp, ALLOC(mp) + 1, hp)) != MP_OKAY)
	return res;
      dp = DIGITS(mp);
    }

    dp[ix] = kin;
    USED(mp) += 1;
  }

  return MP_OKAY;

} /* end s_mp_mul_2() */

/* }}} */

/* {{{ s_mp_mod_2d(mp, d) */

/*
  Remainder the integer by 2^d, where d is a number of bits.  This
  amounts to a bitwise AND of the value, and does not require the full
  division code
 */
void     s_mp_mod_2d(mp_int *mp, mp_digit d)
{
  unsigned int  ndig = (d / DIGIT_BIT), nbit = (d % DIGIT_BIT);
  unsigned int  ix;
  mp_digit      dmask, *dp = DIGITS(mp);

  if(ndig >= USED(mp))
    return;

  /* Flush all the bits above 2^d in its digit */
  dmask = (1 << nbit) - 1;
  dp[ndig] &= dmask;

  /* Flush all digits above the one with 2^d in it */
  for(ix = ndig + 1; ix < USED(mp); ix++)
    dp[ix] = 0;

  s_mp_clamp(mp);

} /* end s_mp_mod_2d() */

/* }}} */

/* {{{ s_mp_mul_2d(mp, d) */

/*
  Multiply by the integer 2^d, where d is a number of bits.  This
  amounts to a bitwise shift of the value, and does not require the
  full multiplication code.
 */
mp_err    s_mp_mul_2d(mp_int *mp, mp_digit d, heap_t *hp)
{
  mp_err   res;
  mp_digit save, next, mask, *dp;
  mp_size  used;
  int      ix;

  if((res = s_mp_lshd(mp, d / DIGIT_BIT, hp)) != MP_OKAY)
    return res;

  dp = DIGITS(mp); used = USED(mp);
  d %= DIGIT_BIT;

  mask = (1 << d) - 1;

  /* If the shift requires another digit, make sure we've got one to
     work with */
  if((dp[used - 1] >> (DIGIT_BIT - d)) & mask) {
    if((res = s_mp_grow(mp, used + 1, hp)) != MP_OKAY)
      return res;
    dp = DIGITS(mp);
  }

  /* Do the shifting... */
  save = 0;
  for(ix = 0; ix < (int)used; ix++) {
    next = (dp[ix] >> (DIGIT_BIT - d)) & mask;
    dp[ix] = (dp[ix] << d) | save;
    save = next;
  }

  /* If, at this point, we have a nonzero carryout into the next
     digit, we'll increase the size by one digit, and store it...
   */
  if(save) {
    dp[used] = save;
    USED(mp) += 1;
  }

  s_mp_clamp(mp);
  return MP_OKAY;

} /* end s_mp_mul_2d() */

/* }}} */

/* {{{ s_mp_div_2d(mp, d) */

/*
  Divide the integer by 2^d, where d is a number of bits.  This
  amounts to a bitwise shift of the value, and does not require the
  full division code (used in Barrett reduction, see below)
 */
void     s_mp_div_2d(mp_int *mp, mp_digit d)
{
  int       ix;
  mp_digit  save, next, mask, *dp = DIGITS(mp);

  s_mp_rshd(mp, d / DIGIT_BIT);
  d %= DIGIT_BIT;

  mask = (1 << d) - 1;

  save = 0;
  for(ix = USED(mp) - 1; ix >= 0; ix--) {
    next = dp[ix] & mask;
    dp[ix] = (dp[ix] >> d) | (save << (DIGIT_BIT - d));
    save = next;
  }

  s_mp_clamp(mp);

} /* end s_mp_div_2d() */

/* }}} */

/* {{{ s_mp_norm(a, b) */

/*
  s_mp_norm(a, b)

  Normalize a and b for division, where b is the divisor.  In order
  that we might make good guesses for quotient digits, we want the
  leading digit of b to be at least half the radix, which we
  accomplish by multiplying a and b by a constant.  This constant is
  returned (so that it can be divided back out of the remainder at the
  end of the division process).

  We multiply by the smallest power of 2 that gives us a leading digit
  at least half the radix.  By choosing a power of 2, we simplify the 
  multiplication and division steps to simple shifts.
 */
mp_digit s_mp_norm(mp_int *a, mp_int *b, heap_t *hp)
{
  mp_digit  t, d = 0;

  t = DIGIT(b, USED(b) - 1);
  while(t < (RADIX / 2)) {
    t <<= 1;
    ++d;
  }
    
  if(d != 0) {
    s_mp_mul_2d(a, d, hp);
    s_mp_mul_2d(b, d, hp);
  }

  return d;

} /* end s_mp_norm() */

/* }}} */

/* }}} */

/* {{{ Primitive digit arithmetic */

/* {{{ s_mp_add_d(mp, d) */

/* Add d to |mp| in place                                                 */
mp_err   s_mp_add_d(mp_int *mp, mp_digit d, heap_t *hp)    /* unsigned digit addition */
{
  mp_word   w, k = 0;
  mp_size   ix = 1, used = USED(mp);
  mp_digit *dp = DIGITS(mp);

  w = dp[0] + d;
  dp[0] = ACCUM(w);
  k = CARRYOUT(w);

  while(ix < used && k) {
    w = dp[ix] + k;
    dp[ix] = ACCUM(w);
    k = CARRYOUT(w);
    ++ix;
  }

  if(k != 0) {
    mp_err  res;

    if((res = s_mp_pad(mp, USED(mp) + 1, hp)) != MP_OKAY)
      return res;

    DIGIT(mp, ix) = k;
  }

  return MP_OKAY;

} /* end s_mp_add_d() */

/* }}} */

/* {{{ s_mp_sub_d(mp, d) */

/* Subtract d from |mp| in place, assumes |mp| > d                        */
mp_err   s_mp_sub_d(mp_int *mp, mp_digit d)    /* unsigned digit subtract */
{
  mp_word   w, b = 0;
  mp_size   ix = 1, used = USED(mp);
  mp_digit *dp = DIGITS(mp);

  /* Compute initial subtraction    */
  w = (RADIX + dp[0]) - d;
  b = CARRYOUT(w) ? 0 : 1;
  dp[0] = ACCUM(w);

  /* Propagate borrows leftward     */
  while(b && ix < used) {
    w = (RADIX + dp[ix]) - b;
    b = CARRYOUT(w) ? 0 : 1;
    dp[ix] = ACCUM(w);
    ++ix;
  }

  /* Remove leading zeroes          */
  s_mp_clamp(mp);

  /* If we have a borrow out, it's a violation of the input invariant */
  if(b)
    return MP_RANGE;
  else
    return MP_OKAY;

} /* end s_mp_sub_d() */

/* }}} */

/* {{{ s_mp_mul_d(a, d) */

/* Compute a = a * d, single digit multiplication                         */
mp_err   s_mp_mul_d(mp_int *a, mp_digit d, heap_t *hp)
{
  mp_word w, k = 0;
  mp_size ix, max;
  mp_err  res;
  mp_digit *dp = DIGITS(a);

  /*
    Single-digit multiplication will increase the precision of the
    output by at most one digit.  However, we can detect when this
    will happen -- if the high-order digit of a, times d, gives a
    two-digit result, then the precision of the result will increase;
    otherwise it won't.  We use this fact to avoid calling s_mp_pad()
    unless absolutely necessary.
   */
  max = USED(a);
  w = dp[max - 1] * d;
  if(CARRYOUT(w) != 0) {
    if((res = s_mp_pad(a, max + 1, hp)) != MP_OKAY)
      return res;
    dp = DIGITS(a);
  }

  for(ix = 0; ix < max; ix++) {
    w = (dp[ix] * d) + k;
    dp[ix] = ACCUM(w);
    k = CARRYOUT(w);
  }

  /* If there is a precision increase, take care of it here; the above
     test guarantees we have enough storage to do this safely.
   */
  if(k) {
    dp[max] = k; 
    USED(a) = max + 1;
  }

  s_mp_clamp(a);

  return MP_OKAY;
  
} /* end s_mp_mul_d() */

/* }}} */

/* {{{ s_mp_div_d(mp, d, r) */

/*
  s_mp_div_d(mp, d, r)

  Compute the quotient mp = mp / d and remainder r = mp mod d, for a
  single digit d.  If r is null, the remainder will be discarded.
 */

mp_err   s_mp_div_d(mp_int *mp, mp_digit d, mp_digit *r, heap_t *hp)
{
  mp_word   w = 0, t;
  mp_int    quot;
  mp_err    res;
  mp_digit *dp = DIGITS(mp), *qp;
  int       ix;

  if(d == 0)
    return MP_RANGE;

  /* Make room for the quotient */
  if((res = mp_init_size(&quot, USED(mp), hp)) != MP_OKAY)
    return res;

  USED(&quot) = USED(mp); /* so clamping will work below */
  qp = DIGITS(&quot);

  /* Divide without subtraction */
  for(ix = USED(mp) - 1; ix >= 0; ix--) {
    w = (w << DIGIT_BIT) | dp[ix];

    if(w >= d) {
      t = w / d;
      w = w % d;
    } else {
      t = 0;
    }

    qp[ix] = t;
  }

  /* Deliver the remainder, if desired */
  if(r)
    *r = w;

  s_mp_clamp(&quot);
  mp_exch(&quot, mp);
  mp_clear(&quot);

  return MP_OKAY;

} /* end s_mp_div_d() */

/* }}} */

/* }}} */

/* {{{ Primitive full arithmetic */

/* {{{ s_mp_add(a, b) */

/* Compute a = |a| + |b|                                                  */
mp_err   s_mp_add(mp_int *a, mp_int *b, heap_t *hp)        /* magnitude addition      */
{
  mp_word   w = 0;
  mp_digit *pa, *pb;
  mp_size   ix, used = USED(b);
  mp_err    res;

  /* Make sure a has enough precision for the output value */
  if((used > USED(a)) && (res = s_mp_pad(a, used, hp)) != MP_OKAY)
    return res;

  /*
    Add up all digits up to the precision of b.  If b had initially
    the same precision as a, or greater, we took care of it by the
    padding step above, so there is no problem.  If b had initially
    less precision, we'll have to make sure the carry out is duly
    propagated upward among the higher-order digits of the sum.
   */
  pa = DIGITS(a);
  pb = DIGITS(b);
  for(ix = 0; ix < used; ++ix) {
    w += *pa + *pb++;
    *pa++ = ACCUM(w);
    w = CARRYOUT(w);
  }

  /* If we run out of 'b' digits before we're actually done, make
     sure the carries get propagated upward...  
   */
  used = USED(a);
  while(w && ix < used) {
    w += *pa;
    *pa++ = ACCUM(w);
    w = CARRYOUT(w);
    ++ix;
  }

  /* If there's an overall carry out, increase precision and include
     it.  We could have done this initially, but why touch the memory
     allocator unless we're sure we have to?
   */
  if(w) {
    if((res = s_mp_pad(a, used + 1, hp)) != MP_OKAY)
      return res;

    DIGIT(a, ix) = w;  /* pa may not be valid after s_mp_pad() call */
  }

  return MP_OKAY;

} /* end s_mp_add() */

/* }}} */

/* {{{ s_mp_sub(a, b) */

/* Compute a = |a| - |b|, assumes |a| >= |b|                              */
mp_err   s_mp_sub(mp_int *a, mp_int *b)        /* magnitude subtract      */
{
  mp_word   w = 0;
  mp_digit *pa, *pb;
  mp_size   ix, used = USED(b);

  /*
    Subtract and propagate borrow.  Up to the precision of b, this
    accounts for the digits of b; after that, we just make sure the
    carries get to the right place.  This saves having to pad b out to
    the precision of a just to make the loops work right...
   */
  pa = DIGITS(a);
  pb = DIGITS(b);

  for(ix = 0; ix < used; ++ix) {
    w = (RADIX + *pa) - w - *pb++;
    *pa++ = ACCUM(w);
    w = CARRYOUT(w) ? 0 : 1;
  }

  used = USED(a);
  while(ix < used) {
    w = RADIX + *pa - w;
    *pa++ = ACCUM(w);
    w = CARRYOUT(w) ? 0 : 1;
    ++ix;
  }

  /* Clobber any leading zeroes we created    */
  s_mp_clamp(a);

  /* 
     If there was a borrow out, then |b| > |a| in violation
     of our input invariant.  We've already done the work,
     but we'll at least complain about it...
   */
  if(w)
    return MP_RANGE;
  else
    return MP_OKAY;

} /* end s_mp_sub() */

/* }}} */

/* {{{ s_mp_mul(a, b) */

/* Compute a = |a| * |b|                                                  */
mp_err   s_mp_mul(mp_int *a, mp_int *b, heap_t *hp)
{
  mp_word   w, k = 0;
  mp_int    tmp;
  mp_err    res;
  mp_size   ix, jx, ua = USED(a), ub = USED(b);
  mp_digit *pa, *pb, *pt, *pbt;

  if((res = mp_init_size(&tmp, ua + ub, hp)) != MP_OKAY)
    return res;

  /* This has the effect of left-padding with zeroes... */
  USED(&tmp) = ua + ub;

  /* We're going to need the base value each iteration */
  pbt = DIGITS(&tmp);

  /* Outer loop:  Digits of b */

  pb = DIGITS(b);
  for(ix = 0; ix < ub; ++ix, ++pb) {
    if(*pb == 0) 
      continue;

    /* Inner product:  Digits of a */
    pa = DIGITS(a);
    for(jx = 0; jx < ua; ++jx, ++pa) {
      pt = pbt + ix + jx;
      w = *pb * *pa + k + *pt;
      *pt = ACCUM(w);
      k = CARRYOUT(w);
    }

    pbt[ix + jx] = k;
    k = 0;
  }

  s_mp_clamp(&tmp);
  s_mp_exch(&tmp, a);

  mp_clear(&tmp);

  return MP_OKAY;

} /* end s_mp_mul() */

/* }}} */

/* {{{ s_mp_kmul(a, b, out, len) */

#if 0
void   s_mp_kmul(mp_digit *a, mp_digit *b, mp_digit *out, mp_size len)
{
  mp_word   w, k = 0;
  mp_size   ix, jx;
  mp_digit *pa, *pt;

  for(ix = 0; ix < len; ++ix, ++b) {
    if(*b == 0)
      continue;
    
    pa = a;
    for(jx = 0; jx < len; ++jx, ++pa) {
      pt = out + ix + jx;
      w = *b * *pa + k + *pt;
      *pt = ACCUM(w);
      k = CARRYOUT(w);
    }

    out[ix + jx] = k;
    k = 0;
  }

} /* end s_mp_kmul() */
#endif

/* }}} */

/* {{{ s_mp_sqr(a) */

/*
  Computes the square of a, in place.  This can be done more
  efficiently than a general multiplication, because many of the
  computation steps are redundant when squaring.  The inner product
  step is a bit more complicated, but we save a fair number of
  iterations of the multiplication loop.
 */
#if MP_SQUARE
mp_err   s_mp_sqr(mp_int *a)
{
  mp_word  w, k = 0;
  mp_int   tmp;
  mp_err   res;
  mp_size  ix, jx, kx, used = USED(a);
  mp_digit *pa1, *pa2, *pt, *pbt;

  if((res = mp_init_size(&tmp, 2 * used)) != MP_OKAY)
    return res;

  /* Left-pad with zeroes */
  USED(&tmp) = 2 * used;

  /* We need the base value each time through the loop */
  pbt = DIGITS(&tmp);

  pa1 = DIGITS(a);
  for(ix = 0; ix < used; ++ix, ++pa1) {
    if(*pa1 == 0)
      continue;

    w = DIGIT(&tmp, ix + ix) + (*pa1 * *pa1);

    pbt[ix + ix] = ACCUM(w);
    k = CARRYOUT(w);

    /*
      The inner product is computed as:

         (C, S) = t[i,j] + 2 a[i] a[j] + C

      This can overflow what can be represented in an mp_word, and
      since C arithmetic does not provide any way to check for
      overflow, we have to check explicitly for overflow conditions
      before they happen.
     */
    for(jx = ix + 1, pa2 = DIGITS(a) + jx; jx < used; ++jx, ++pa2) {
      mp_word  u = 0, v;
      
      /* Store this in a temporary to avoid indirections later */
      pt = pbt + ix + jx;

      /* Compute the multiplicative step */
      w = *pa1 * *pa2;

      /* If w is more than half MP_WORD_MAX, the doubling will
	 overflow, and we need to record a carry out into the next
	 word */
      u = (w >> (MP_WORD_BIT - 1)) & 1;

      /* Double what we've got, overflow will be ignored as defined
	 for C arithmetic (we've already noted if it is to occur)
       */
      w *= 2;

      /* Compute the additive step */
      v = *pt + k;

      /* If we do not already have an overflow carry, check to see
	 if the addition will cause one, and set the carry out if so 
       */
      u |= ((MP_WORD_MAX - v) < w);

      /* Add in the rest, again ignoring overflow */
      w += v;

      /* Set the i,j digit of the output */
      *pt = ACCUM(w);

      /* Save carry information for the next iteration of the loop.
	 This is why k must be an mp_word, instead of an mp_digit */
      k = CARRYOUT(w) | (u << DIGIT_BIT);

    } /* for(jx ...) */

    /* Set the last digit in the cycle and reset the carry */
    k = DIGIT(&tmp, ix + jx) + k;
    pbt[ix + jx] = ACCUM(k);
    k = CARRYOUT(k);

    /* If we are carrying out, propagate the carry to the next digit
       in the output.  This may cascade, so we have to be somewhat
       circumspect -- but we will have enough precision in the output
       that we won't overflow 
     */
    kx = 1;
    while(k) {
      k = pbt[ix + jx + kx] + 1;
      pbt[ix + jx + kx] = ACCUM(k);
      k = CARRYOUT(k);
      ++kx;
    }
  } /* for(ix ...) */

  s_mp_clamp(&tmp);
  s_mp_exch(&tmp, a);

  mp_clear(&tmp);

  return MP_OKAY;

} /* end s_mp_sqr() */
#endif

/* }}} */

/* {{{ s_mp_div(a, b) */

/*
  s_mp_div(a, b)

  Compute a = a / b and b = a mod b.  Assumes b > a.
 */

mp_err   s_mp_div(mp_int *a, mp_int *b, heap_t *hp)
{
  mp_int   quot, rem, t;
  mp_word  q;
  mp_err   res;
  mp_digit d;
  int      ix;

  if(mp_cmp_z(b) == 0)
    return MP_RANGE;

  /* Shortcut if b is power of two */
  if((ix = s_mp_ispow2(b)) >= 0) {
    mp_copy(a, b, hp);  /* need this for remainder */
    s_mp_div_2d(a, (mp_digit)ix);
    s_mp_mod_2d(b, (mp_digit)ix);

    return MP_OKAY;
  }

  /* Allocate space to store the quotient */
  if((res = mp_init_size(&quot, USED(a), hp)) != MP_OKAY)
    return res;

  /* A working temporary for division     */
  if((res = mp_init_size(&t, USED(a), hp)) != MP_OKAY)
    goto T;

  /* Allocate space for the remainder     */
  if((res = mp_init_size(&rem, USED(a), hp)) != MP_OKAY)
    goto REM;

  /* Normalize to optimize guessing       */
  d = s_mp_norm(a, b, hp);

  /* Perform the division itself...woo!   */
  ix = USED(a) - 1;

  while(ix >= 0) {
    /* Find a partial substring of a which is at least b */
    while(s_mp_cmp(&rem, b) < 0 && ix >= 0) {
      if((res = s_mp_lshd(&rem, 1, hp)) != MP_OKAY) 
	goto CLEANUP;

      if((res = s_mp_lshd(&quot, 1, hp)) != MP_OKAY)
	goto CLEANUP;

      DIGIT(&rem, 0) = DIGIT(a, ix);
      s_mp_clamp(&rem);
      --ix;
    }

    /* If we didn't find one, we're finished dividing    */
    if(s_mp_cmp(&rem, b) < 0) 
      break;    

    /* Compute a guess for the next quotient digit       */
    q = DIGIT(&rem, USED(&rem) - 1);
    if(q <= DIGIT(b, USED(b) - 1) && USED(&rem) > 1)
      q = (q << DIGIT_BIT) | DIGIT(&rem, USED(&rem) - 2);

    q /= DIGIT(b, USED(b) - 1);

    /* The guess can be as much as RADIX + 1 */
    if(q >= RADIX)
      q = RADIX - 1;

    /* See what that multiplies out to                   */
    mp_copy(b, &t, hp);
    if((res = s_mp_mul_d(&t, q, hp)) != MP_OKAY)
      goto CLEANUP;

    /* 
       If it's too big, back it off.  We should not have to do this
       more than once, or, in rare cases, twice.  Knuth describes a
       method by which this could be reduced to a maximum of once, but
       I didn't implement that here.
     */
    while(s_mp_cmp(&t, &rem) > 0) {
      --q;
      s_mp_sub(&t, b);
    }

    /* At this point, q should be the right next digit   */
    if((res = s_mp_sub(&rem, &t)) != MP_OKAY)
      goto CLEANUP;

    /*
      Include the digit in the quotient.  We allocated enough memory
      for any quotient we could ever possibly get, so we should not
      have to check for failures here
     */
    DIGIT(&quot, 0) = q;
  }

  /* Denormalize remainder                */
  if(d != 0) 
    s_mp_div_2d(&rem, d);

  s_mp_clamp(&quot);
  s_mp_clamp(&rem);

  /* Copy quotient back to output         */
  s_mp_exch(&quot, a);
  
  /* Copy remainder back to output        */
  s_mp_exch(&rem, b);

CLEANUP:
  mp_clear(&rem);
REM:
  mp_clear(&t);
T:
  mp_clear(&quot);

  return res;

} /* end s_mp_div() */

/* }}} */

/* {{{ s_mp_2expt(a, k) */

mp_err   s_mp_2expt(mp_int *a, mp_digit k, heap_t *hp)
{
  mp_err    res;
  mp_size   dig, bit;

  dig = k / DIGIT_BIT;
  bit = k % DIGIT_BIT;

  mp_zero(a);
  if((res = s_mp_pad(a, dig + 1, hp)) != MP_OKAY)
    return res;
  
  DIGIT(a, dig) |= (1 << bit);

  return MP_OKAY;

} /* end s_mp_2expt() */

/* }}} */

/* {{{ Primitive comparisons */

/* {{{ s_mp_cmp(a, b) */

/* Compare |a| <=> |b|, return 0 if equal, <0 if a<b, >0 if a>b           */
int      s_mp_cmp(mp_int *a, mp_int *b)
{
  mp_size   ua = USED(a), ub = USED(b);

  if(ua > ub)
    return MP_GT;
  else if(ua < ub)
    return MP_LT;
  else {
    int      ix = ua - 1;
    mp_digit *ap = DIGITS(a) + ix, *bp = DIGITS(b) + ix;

    while(ix >= 0) {
      if(*ap > *bp)
	return MP_GT;
      else if(*ap < *bp)
	return MP_LT;

      --ap; --bp; --ix;
    }

    return MP_EQ;
  }

} /* end s_mp_cmp() */

/* }}} */

/* {{{ s_mp_cmp_d(a, d) */

/* Compare |a| <=> d, return 0 if equal, <0 if a<d, >0 if a>d             */
int      s_mp_cmp_d(mp_int *a, mp_digit d)
{
  mp_size  ua = USED(a);
  mp_digit *ap = DIGITS(a);

  if(ua > 1)
    return MP_GT;

  if(*ap < d) 
    return MP_LT;
  else if(*ap > d)
    return MP_GT;
  else
    return MP_EQ;

} /* end s_mp_cmp_d() */

/* }}} */

/* {{{ s_mp_ispow2(v) */

/*
  Returns -1 if the value is not a power of two; otherwise, it returns
  k such that v = 2^k, i.e. lg(v).
 */
int      s_mp_ispow2(mp_int *v)
{
  mp_digit d, *dp;
  mp_size  uv = USED(v);
  int      extra = 0, ix;

  d = DIGIT(v, uv - 1); /* most significant digit of v */

  while(d && ((d & 1) == 0)) {
    d >>= 1;
    ++extra;
  }

  if(d == 1) {
    ix = uv - 2;
    dp = DIGITS(v) + ix;

    while(ix >= 0) {
      if(*dp)
	return -1; /* not a power of two */

      --dp; --ix;
    }

    return ((uv - 1) * DIGIT_BIT) + extra;
  } 

  return -1;

} /* end s_mp_ispow2() */

/* }}} */

/* {{{ s_mp_ispow2d(d) */

int      s_mp_ispow2d(mp_digit d)
{
  int   pow = 0;

  while((d & 1) == 0) {
    ++pow; d >>= 1;
  }

  if(d == 1)
    return pow;

  return -1;

} /* end s_mp_ispow2d() */

/* }}} */

/* }}} */

/* {{{ Primitive I/O helpers */

/* {{{ s_mp_tovalue(ch, r) */

/*
  Convert the given character to its digit value, in the given radix.
  If the given character is not understood in the given radix, -1 is
  returned.  Otherwise the digit's numeric value is returned.

  The results will be odd if you use a radix < 2 or > 62, you are
  expected to know what you're up to.
 */
int      s_mp_tovalue(char ch, int r)
{
  int    val, xch;
  
  if(r > 36)
    xch = ch;
  else
    xch = toupper(ch);

  if(isdigit(xch))
    val = xch - '0';
  else if(isupper(xch))
    val = xch - 'A' + 10;
  else if(islower(xch))
    val = xch - 'a' + 36;
  else if(xch == '+')
    val = 62;
  else if(xch == '/')
    val = 63;
  else 
    return -1;

  if(val < 0 || val >= r)
    return -1;

  return val;

} /* end s_mp_tovalue() */

/* }}} */

/* {{{ s_mp_todigit(val, r, low) */

/*
  Convert val to a radix-r digit, if possible.  If val is out of range
  for r, returns zero.  Otherwise, returns an ASCII character denoting
  the value in the given radix.

  The results may be odd if you use a radix < 2 or > 64, you are
  expected to know what you're doing.
 */
  
char     s_mp_todigit(int val, int r, int low)
{
  char   ch;

  if(val < 0 || val >= r)
    return 0;

  ch = s_dmap_1[val];

  if(r <= 36 && low)
    ch = tolower(ch);

  return ch;

} /* end s_mp_todigit() */

/* }}} */

/* {{{ s_mp_outlen(bits, radix) */

/* 
   Return an estimate for how long a string is needed to hold a radix
   r representation of a number with 'bits' significant bits.

   Does not include space for a sign or a NUL terminator.
 */
int      s_mp_outlen(int bits, int r)
{
  return (int)((double)bits * LOG_V_2(r) + 0.5);

} /* end s_mp_outlen() */

/* }}} */

/* }}} */

/*------------------------------------------------------------------------*/
/* HERE THERE BE DRAGONS                                                  */
