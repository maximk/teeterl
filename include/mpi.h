/*
    mpi.h

    by Michael J. Fromberger <http://www.dartmouth.edu/~sting/>
    Copyright (C) 1998 Michael J. Fromberger, All Rights Reserved

    Arbitrary precision integer arithmetic library

    $Id: mpi.h,v 1.1 2004/02/08 04:29:29 sting Exp $
 */

#ifndef _H_MPI_
#define _H_MPI_

#include "term.h"
#include "heap.h"

#include "mpi-config.h"

#if MP_DEBUG
#undef MP_IOFUNC
#define MP_IOFUNC 1
#endif

#if MP_IOFUNC
#include <stdio.h>
#include <ctype.h>
#endif

#include <limits.h>

#define  MP_NEG  1
#define  MP_ZPOS 0

/* Included for compatibility... */
#define  NEG     MP_NEG
#define  ZPOS    MP_ZPOS

#define  MP_OKAY          0 /* no error, all is well */
#define  MP_YES           0 /* yes (boolean result)  */
#define  MP_NO           -1 /* no (boolean result)   */
#define  MP_MEM          -2 /* out of memory         */
#define  MP_RANGE        -3 /* argument out of range */
#define  MP_BADARG       -4 /* invalid parameter     */
#define  MP_UNDEF        -5 /* answer is undefined   */
#define  MP_LAST_CODE    MP_UNDEF

//#include "mpi-types.h"

/* Included for compatibility... */
#define DIGIT_BIT         MP_DIGIT_BIT
#define DIGIT_MAX         MP_DIGIT_MAX

// moved to term.h
///* Macros for accessing the mp_int internals           */
//#define  SIGN(MP)     ((MP)->sign)
//#define  USED(MP)     ((MP)->used)
//#define  ALLOC(MP)    ((MP)->alloc)
//#define  DIGITS(MP)   ((MP)->dp)
//#define  DIGIT(MP,N)  (MP)->dp[(N)]

#if MP_ARGCHK == 1
#define  ARGCHK(X,Y)  {if(!(X)){return (Y);}}
#elif MP_ARGCHK == 2
#include <assert.h>
#define  ARGCHK(X,Y)  assert(X)
#else
#define  ARGCHK(X,Y)  /*  */
#endif

/* This defines the maximum I/O base (minimum is 2)   */
#define MAX_RADIX         64

/*------------------------------------------------------------------------*/
/* Term conversions                                                       */

term_t	mp_to_term(mp_int mp);
mp_int	term_to_mp(term_t t, heap_t *hp);
term_t	int_to_term(long z, heap_t *hp);
mp_int	bignum_to_mp(term_t b);
double term_to_double(term_t t);

/*------------------------------------------------------------------------*/
/* Default precision                                                      */

unsigned int mp_get_prec(void);
void         mp_set_prec(unsigned int prec);

/*------------------------------------------------------------------------*/
/* Memory management                                                      */

mp_err mp_init_size(mp_int *mp, mp_size prec, heap_t *hp);
mp_err mp_init_copy(mp_int *mp, mp_int *from, heap_t *hp);
mp_err mp_copy(mp_int *from, mp_int *to, heap_t *hp);
void   mp_exch(mp_int *mp1, mp_int *mp2);
void   mp_zero(mp_int *mp);
void   mp_set(mp_int *mp, mp_digit d);
mp_err mp_set_int(mp_int *mp, long z, heap_t *hp);

#define mp_clear(mp)

/*------------------------------------------------------------------------*/
/* Single digit arithmetic                                                */

mp_err mp_add_d(mp_int *a, mp_digit d, mp_int *b, heap_t *hp);
mp_err mp_sub_d(mp_int *a, mp_digit d, mp_int *b, heap_t *hp);
mp_err mp_mul_d(mp_int *a, mp_digit d, mp_int *b, heap_t *hp);
mp_err mp_mul_2(mp_int *a, mp_int *c, heap_t *hp);
mp_err mp_div_d(mp_int *a, mp_digit d, mp_int *q, mp_digit *r, heap_t *hp);
mp_err mp_div_2(mp_int *a, mp_int *c, heap_t *hp);
mp_err mp_expt_d(mp_int *a, mp_digit d, mp_int *c, heap_t *hp);

/*------------------------------------------------------------------------*/
/* Sign manipulations                                                     */

mp_err mp_abs(mp_int *a, mp_int *b, heap_t *hp);
mp_err mp_neg(mp_int *a, mp_int *b, heap_t *hp);

/*------------------------------------------------------------------------*/
/* Full arithmetic                                                        */

mp_err mp_add(mp_int *a, mp_int *b, mp_int *c, heap_t *hp);
mp_err mp_sub(mp_int *a, mp_int *b, mp_int *c, heap_t *hp);
mp_err mp_mul(mp_int *a, mp_int *b, mp_int *c, heap_t *hp);
mp_err mp_mul_2d(mp_int *a, mp_digit d, mp_int *c, heap_t *hp);
mp_err mp_div(mp_int *a, mp_int *b, mp_int *q, mp_int *r, heap_t *hp);
mp_err mp_div_2d(mp_int *a, mp_digit d, mp_int *q, mp_int *r, heap_t *hp);
mp_err mp_2expt(mp_int *a, mp_digit k, heap_t *hp);

mp_err mp_mod(mp_int *a, mp_int *m, mp_int *c, heap_t *hp);
mp_err mp_mod_d(mp_int *a, mp_digit d, mp_digit *c, heap_t *hp);

/*------------------------------------------------------------------------*/
/* Comparisons                                                            */

int    mp_cmp_z(mp_int *a);
int    mp_cmp_d(mp_int *a, mp_digit d);
int    mp_cmp(mp_int *a, mp_int *b);
int    mp_cmp_mag(mp_int *a, mp_int *b);
int    mp_cmp_int(mp_int *a, long z, heap_t *hp);
int    mp_isodd(mp_int *a);
int    mp_iseven(mp_int *a);

/*------------------------------------------------------------------------*/
/* Base conversion                                                        */

#define BITS     1
#define BYTES    CHAR_BIT

mp_err mp_read_signed_bin(mp_int *mp, unsigned char *str, int len, heap_t *hp);
int    mp_signed_bin_size(mp_int *mp);
mp_err mp_to_signed_bin(mp_int *mp, unsigned char *str);

mp_err mp_read_unsigned_bin(mp_int *mp, unsigned char *str, int len, heap_t *hp);
mp_err mp_read_unsigned_bin_lsb(mp_int *mp, unsigned char *str, int len, heap_t *hp);
int    mp_unsigned_bin_size(mp_int *mp);
mp_err mp_to_unsigned_bin(mp_int *mp, unsigned char *str);
mp_err mp_to_unsigned_bin_lsb(mp_int *mp, unsigned char *str);

int    mp_count_bits(mp_int *mp);

mp_err mp_read_radix(mp_int *mp, unsigned char *str, int radix, heap_t *hp);
int    mp_radix_size(mp_int *mp, int radix);
int    mp_value_radix_size(int num, int qty, int radix);
mp_err mp_toradix(mp_int *mp, unsigned char *str, int radix, heap_t *hp);

int    mp_char2value(char ch, int r);

#define mp_tobinary(M, S, H)  mp_toradix((M), (S), 2, (H))
#define mp_tooctal(M, S, H)   mp_toradix((M), (S), 8, (H))
#define mp_todecimal(M, S, H) mp_toradix((M), (S), 10, (H))
#define mp_tohex(M, S, H)     mp_toradix((M), (S), 16, (H))

long	mp_get_int(mp_int *mp);
double	mp_get_double(mp_int *mp);

/*------------------------------------------------------------------------*/
/* Error strings                                                          */

const  char  *mp_strerror(mp_err ec);

#endif /* end _H_MPI_ */
