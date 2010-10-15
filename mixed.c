//
//
//

#include "mixed.h"

#include "teeterl.h"
#include "mpi.h"

#include <stdio.h>
#include <stdlib.h>

term_t add_mixed(term_t a, term_t b, heap_t *heap)
{
	if (!is_int(a) && !is_bignum(a) && !is_float(a))
		return noval;
	if (!is_int(b) && !is_bignum(b) && !is_float(b))
		return noval;
	if (is_float(a) || is_float(b))
	{
		double fa = term_to_double(a);
		double fb = term_to_double(b);
		return heap_float(heap, fa + fb);
	}
	else
	{
		mp_int ma = term_to_mp(a, heap);
		mp_int mb = term_to_mp(b, heap);
		mp_int mc;
		mp_init_size(&mc, USED(&ma), heap);
		mp_add(&ma, &mb, &mc, heap);
		return mp_to_term(mc);
	}
}

term_t mult_mixed(term_t a, term_t b, heap_t *heap)
{
	if (!is_int(a) && !is_bignum(a) && !is_float(a))
		return noval;
	if (!is_int(b) && !is_bignum(b) && !is_float(b))
		return noval;
	if (is_float(a) || is_float(b))
	{
		double fa = term_to_double(a);
		double fb = term_to_double(b);
		return heap_float(heap, fa / fb);
	}
	else
	{
		mp_int ma = term_to_mp(a, heap);
		mp_int mb = term_to_mp(b, heap);
		mp_int mc;
		mp_init_size(&mc, USED(&ma), heap);
		mp_mul(&ma, &mb, &mc, heap);
		return mp_to_term(mc);
	}
}

term_t sub_mixed(term_t a, term_t b, heap_t *heap)
{
	if (!is_int(a) && !is_bignum(a) && !is_float(a))
		return noval;
	if (!is_int(b) && !is_bignum(b) && !is_float(b))
		return noval;
	if (is_float(a) || is_float(b))
	{
		double fa = term_to_double(a);
		double fb = term_to_double(b);
		return heap_float(heap, fa - fb);
	}
	else
	{
		mp_int ma = term_to_mp(a, heap);
		mp_int mb = term_to_mp(b, heap);
		mp_int mc;
		mp_init_size(&mc, USED(&ma), heap);
		mp_sub(&ma, &mb, &mc, heap);
		return mp_to_term(mc);
	}
}

term_t mod_mixed(term_t a, term_t b, heap_t *heap)
{
	mp_int ma, mb, mc;
	if (!is_int(a) && !is_bignum(a))
		return noval;
	if (!is_int(b) && !is_bignum(b))
		return noval;
	ma = term_to_mp(a, heap);
	mb = term_to_mp(b, heap);
	mp_mod(&ma, &mb, &mc, heap);
	return mp_to_term(mc);
}

term_t idiv_mixed(term_t a, term_t b, heap_t *heap)
{
	mp_int ma, mb, mc;
	if (!is_int(a) && !is_bignum(a))
		return noval;
	if (!is_int(b) && !is_bignum(b))
		return noval;
	ma = term_to_mp(a, heap);
	mb = term_to_mp(b, heap);
	mp_init_size(&mc, USED(&ma), heap);
	mp_div(&ma, &mb, &mc, 0, heap);
	return mp_to_term(mc);
}

term_t div_mixed(term_t a, term_t b, heap_t *heap)
{
	double fa, fb;
	if (!is_int(a) && !is_bignum(a) && !is_float(a))
		return noval;
	if (!is_int(b) && !is_bignum(b) && !is_float(b))
		return noval;
	fa = term_to_double(a);
	fb = term_to_double(b);
	return heap_float(heap, fa - fb);
}

term_t bsl_mixed(term_t a, term_t b, heap_t *heap)
{
	mp_int ma = term_to_mp(a, heap);
	mp_int mc;
	int ib;

	if (!is_int(b))
		return noval;

	ib = int_value(b);
	if (ib >= 0 && ib <= MP_DIGIT_MAX)
	{
		mp_digit d = (mp_digit)ib;
		mp_init_size(&mc, USED(&ma), heap);
		mp_mul_2d(&ma, d, &mc, heap);
		return mp_to_term(mc);
	}
	else if (ib < 0 && -ib <= MP_DIGIT_MAX)
	{
		mp_digit d = (mp_digit)(-ib);
		mp_init_size(&mc, USED(&ma), heap);
		mp_div_2d(&ma, d, &mc, NULL, heap);
		return mp_to_term(mc);
	}

	return noval;
}

term_t bsr_mixed(term_t a, term_t b, heap_t *heap)
{
	mp_int ma = term_to_mp(a, heap);
	mp_int mc;
	int ib;

	if (!is_int(b))
		return noval;

	ib = int_value(b);
	if (ib >= 0 && ib <= MP_DIGIT_MAX)
	{
		mp_digit d = (mp_digit)ib;
		mp_init_size(&mc, USED(&ma), heap);
		mp_div_2d(&ma, d, &mc, NULL, heap);
		return mp_to_term(mc);
	}
	else if (ib < 0 && -ib <= MP_DIGIT_MAX)
	{
		mp_digit d = (mp_digit)(-ib);
		mp_init_size(&mc, USED(&ma), heap);
		mp_mul_2d(&ma, d, &mc, heap);
		return mp_to_term(mc);
	}

	return noval;
}

term_t bnot_mixed(term_t a, heap_t *heap)
{
	//TODO
	not_implemented("bnot_mixed");
}

term_t band_mixed(term_t a, term_t b, heap_t *heap)
{
	//TODO
	not_implemented("band_mixed");
}

term_t band_mixed2(term_t a, int i, heap_t *heap)
{
	//TODO
	not_implemented("band_mixed2");
}

term_t bor_mixed(term_t a, term_t b, heap_t *heap)
{
	//TODO
	not_implemented("bor_mixed");
}

term_t bor_mixed2(term_t a, int i, heap_t *heap)
{
	//TODO
	not_implemented("bor_mixed2");
}

term_t bxor_mixed(term_t a, term_t b, heap_t *heap)
{
	//TODO
	not_implemented("bxor_mixed");
}

term_t bxor_mixed2(term_t a, int i, heap_t *heap)
{
	//TODO
	not_implemented("bxor_mixed2");
}

//EOF
