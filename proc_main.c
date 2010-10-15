//
// Teeterl 3
//

#include "proc.h"
#include "code_base.h"
#include "named_tuple.h"

#include "teeterl.h"
#include "opcodes.h"

#include "mpi.h"
#include "binary.h"
#include "list.h"
#include "compare.h"
#include "mixed.h"
#include "bits.h"

#include "scheduler.h"

#include <stdlib.h>

#define NUM_REGS		256
#define NUM_FAST_REGS	2

void proc_secure_stack_space(proc_t *proc, int nslots);
void report_and_exit(proc_t *proc);

term_t proc_main(proc_t *proc, module_t *module, codel_t *ip, int argc, term_t argv[])
{

#if NUM_FAST_REGS == 2

	register term_t r0, r1;
	term_t rs[NUM_REGS - NUM_FAST_REGS];

#define get_reg(x)		((x == 0) ?r0 :(x == 1) ?r1 :rs[x-2])
#define set_reg(x, t) \
do { \
	if (x == 0) r0 = t; \
	else if (x == 1) r1 = t; \
	else rs[x-2] = t; \
} while (0)
#elif NUM_FAST_REGS == 0

	term_t rs[NUM_REGS];

#define get_reg(x)		rs[x]
#define set_reg(x, t)	do { rs[x] = t; } while (0)
#endif

	frame_t *my_frame = (frame_t *)(proc->stack_node->first_avail - sizeof(frame_t));

//NB: does not depend on int size; 32/64-bit portability
#define xxxu(__i)	(ip[__i].n & 255)
#define xxux(__i)	((ip[__i].n >> 8) & 255)
#define xuxx(__i)	((ip[__i].n >> 16) & 255)
#define uxxx(__i)	((ip[__i].n >> 24) & 255)
#define xxdd(__i)	(ip[__i].n & 65535)
#define xddx(__i)	((ip[__i].n >> 8) & 65535)
#define ddxx(__i)	((ip[__i].n >> 16) & 65535)
#define DDxx(__i)	(ip[__i].i >> 16)
#define Uxxx(__i)	(ip[__i].i >> 24)
#define xUxx(__i)	((ip[__i].i << 8) >> 24)

	// keeps track of anticipated heap allocations
	// size to give later reclaim memory to the heap
	// and number of slots; both are updated by 'frame'

	int nslots = 0;
	int heap_needed = 0;

	int reductions = SLICE_REDUCTIONS;

	// temporary variable to provide for jumps between cases
	int tmp1, tmp2;
	term_t q1, q2;

	// offset for binary splicing operations
	int bspl_off = 0;

#define next(__n)	do { ip += __n; goto dispatch; } while (0)
#define jump(__n)	do { ip = ip[__n].l; goto dispatch; } while (0)

#define exception(__class, __reason) \
do { \
	proc->result.what = __class; \
	proc->result.reason = __reason; \
	\
	proc->capsule.module = module;	\
	proc->capsule.ip = ip;	\
	\
	goto schedule; \
} while (0)

#define exception0() \
do { \
	\
	proc->capsule.module = module;	\
	proc->capsule.ip = ip;	\
	\
	goto schedule; \
} while (0)

#define bad_arg(v)		exception(SLICE_RESULT_ERROR, A_BADARG)
#define bad_arg0()		exception(SLICE_RESULT_ERROR, A_BADARG)
#define bad_arith(v)	exception(SLICE_RESULT_ERROR, A_BADARITH)
#define bad_arith0()	exception(SLICE_RESULT_ERROR, A_BADARITH)

#if NUM_FAST_REGS == 2
	if (argc > 0)
		r0 = argv[0];
	if (argc > 1)
		r1 = argv[1];
	if (argc > NUM_FAST_REGS)
		memcpy(rs, argv + NUM_FAST_REGS, (argc - NUM_FAST_REGS)*sizeof(term_t));
#elif NUM_FAST_REGS == 0
	memcpy(rs, argv, argc*sizeof(term_t));
#endif

dispatch:
switch(ip[0].n & 255)
{
//asm({'move',{r,X},{r,Y}}) -> [?xuuu(Y, X, 0)];
case OP_MOVE_R_R:
{
	int dst = xxux(0);
	int src = xuxx(0);
	term_t tmp = get_reg(src);
	set_reg(dst, tmp);
	next(1);
}

//asm({'move',{v,X},{r,Y}}) when X =< 65535, X >= 0 -> [?uddu(Y, X, 1)];
case OP_MOVE_S2_R:
{
	int r = uxxx(0);
	int s = xddx(0);
	my_frame->slots[s] = get_reg(r);
	next(1);
}

//asm({'move',{v,X},{r,Y}}) -> [?xxuu(Y, 2), X];
case OP_MOVE_S_R:
{
	int r = xxux(0);
	my_frame->slots[ip[1].n] = get_reg(r);
	next(2);
}

//asm({'move',{r,X},{v,Y}}) when Y =< 65535, Y >= 0 -> [?dduu(Y, X, 3)];
case OP_MOVE_R_S2:
{
	int r = xxux(0);
	int s = ddxx(0);
	term_t tmp = my_frame->slots[s];
	set_reg(r, tmp);
	next(1);
}

//asm({'move',{r,X},{v,Y}}) -> [?xxuu(X, 4), Y];
case OP_MOVE_R_S:
{
	int r = xxux(0);
	term_t tmp = my_frame->slots[ip[1].n];
	set_reg(r, tmp);
	next(2);
}

//asm({'swap',{r,X},{r,Y}}) -> [?xuuu(Y, X, 5)];
case OP_SWAP_R_R:
{
	int ra = xxux(0);
	int rb = xuxx(0);
	term_t a = get_reg(ra);
	term_t b = get_reg(rb);
	set_reg(ra, b);
	set_reg(rb, a);
	next(1);
}

//asm({'swap',{r,X},{s,Y}}) when Y =< 65535, Y >= 0 -> [?dduu(Y, X, 6)];
case OP_SWAP_R_S2:
{
	int ra = xxux(0);
	int s = ddxx(0);
	term_t a = get_reg(ra);
	term_t b = my_frame->slots[s];
	set_reg(ra, b);
	my_frame->slots[s] = a;
	next(1);
}

//asm({'swap',{r,X},{s,Y}}) -> [?xxuu(X, 6), Y];
case OP_SWAP_R_S:
{
	int ra = xxux(0);
	int s = ip[1].i;
	term_t a = get_reg(ra);
	term_t b = my_frame->slots[s];
	set_reg(ra, b);
	my_frame->slots[s] = a;
	next(2);
}

//asm({'set',{r,X},{integer,Y}}) when Y =< 32767, Y >= -32768 -> [?dduu(Y, X, 5)];
case OP_SET_R_I2:
{
	int r = xxux(0);
	int i = DDxx(0);
	term_t tmp = tag_int(i);
	set_reg(r, tmp);
	next(1);
}

//asm({'set',{r,X},{atom,Y}}) -> [?xuuu(Y, X, 7)];
case OP_SET_R_A:
{
	int r = xxux(0);
	int index = xuxx(0);
	term_t tmp = tag_atom(index);
	set_reg(r, tmp);
	next(1);
}

//asm({'set',{r,X},{literal,Y}}) -> [?xxuu(X, 8), {literal,Y}];
case OP_SET_R_T:
{
	int r = xxux(0);
	term_t tmp = ip[1].t;
	set_reg(r, tmp);
	next(2);
}

//asm({'add',{r,X},{r,Y},{r,Z}}) -> [?uuuu(Z, Y, X, 9)];
case OP_ADD_R_R_R:
{
	int rc = xxux(0);
	int ra = xuxx(0);
	int rb = uxxx(0);
	term_t a = get_reg(ra);
	term_t b = get_reg(rb);
	term_t c;
	if (are_both_int(a, b))
	{
		int i = int_value(a) + int_value(b);
		c = int_to_term(i, proc->heap);
	}
	else if (is_float(a) && is_float(b))
	{
		term_box_t *htop = (term_box_t *)heap_htop(proc->heap);
		htop->float_value.value = float_value(a) + float_value(b);
		heap_htop(proc->heap) += FLOAT_VALUE_T_SIZE;
		heap_needed -= FLOAT_VALUE_T_SIZE;
		c = tag_float(htop);
	}
	else
	{
		c = add_mixed(a, b, proc->heap);
		if (c == noval)
			bad_arith0();
	}
	set_reg(rc, c);
	next(1);
}

//asm({'add',{r,X},{r,Y},{integer,Z}}) when Z =< 127, Z >= -128 -> [?uuuu(Z, Y, X, 10)];
case OP_ADD_R_R_I1:
{
	int rc = xxux(0);
	int ra = xuxx(0);
	int i = Uxxx(0);
	term_t a = get_reg(ra);
	term_t c;
	if (is_int(a))
	{
		i += int_value(a);
		c = int_to_term(i, proc->heap);
	}
	else if (!is_bignum(a) && !is_float(a))
		bad_arith(a);
	else
		c = add_mixed(a, tag_int(i), proc->heap);
	set_reg(rc, c);
	next(1);
}

//asm({'add',{r,X},{r,Y},{literal,Z}}) -> [?xuuu(Y, X, 13), {literal,Z}];
case OP_ADD_R_R_T:
{
	int rc = xxux(0);
	int ra = xuxx(0);
	term_t a = get_reg(ra);
	term_t b = ip[1].t;
	term_t c = add_mixed(a, b, proc->heap);
	if (c == noval)
		bad_arith0();
	set_reg(rc, c);
	next(2);
}

//asm({'mult',{r,X},{r,Y},{r,Z}}) -> [?uuuu(Z, Y, X, 12)];
case OP_MULT_R_R_R:
{
	int rc = xxux(0);
	int ra = xuxx(0);
	int rb = uxxx(0);
	term_t a = get_reg(ra);
	term_t b = get_reg(rb);
	term_t c;
	if (are_both_int(a, b))
	{
#if APR_SIZEOF_VOIDP == 4
		apr_int64_t i = int_value(a) * int_value(b);
		c = int_to_term((long)i, proc->heap);	//TODO: possible loss of data, also below
#else
		not_implemented("mult: 64-bit ints");
#endif
	}
	else
	{
		c = mult_mixed(a, b, proc->heap);
		if (c == noval)
			bad_arith0();
	}
	set_reg(rc, c);
	next(1);
}

//asm({'mult',{r,X},{r,Y},{integer,Z}}) when Z =< 127, Z >= -128 -> [?uuuu(Z, Y, X, 13)];
case OP_MULT_R_R_I1:
{
	int rc = xxux(0);
	int ra = xuxx(0);
	int s = uxxx(0);
	term_t a = get_reg(ra);
	term_t c;
	if (is_int(a))
	{
#if APR_SIZEOF_VOIDP == 4
		apr_int64_t i = int_value(a) * s;
		c = int_to_term((long)i, proc->heap);
#else
		not_implemented("mult: 64-bit ints");
#endif
	}
	else if (!is_bignum(a) && !is_float(a))
		bad_arith0();
	else
		c = mult_mixed(a, tag_int(s), proc->heap);
	set_reg(rc, c);
	next(1);
}

//asm({'mult',{r,X},{r,Y},{literal,Z}}) -> [?xuuu(Y, X, 16), {literal,Z}];
case OP_MULT_R_R_T:
{
	int rc = xxux(0);
	int ra = xuxx(0);
	term_t a = get_reg(ra);
	term_t b = ip[1].t;
	term_t c;
	if (are_both_int(a, b))
	{
#if APR_SIZEOF_VOIDP == 4
		apr_int64_t i = int_value(a) * int_value(b);
		c = int_to_term((long)i, proc->heap);
#else
		not_implemented("mult: 64-bit ints");
#endif
	}
	else
	{
		c = mult_mixed(a, b, proc->heap);
		if (c == noval)
			bad_arith0();
	}
	set_reg(rc, c);
	next(2);
}

//asm({'sub',{r,X},{r,Y},{r,Z}}) -> [?uuuu(Z, Y, X, 15)];
case OP_SUB_R_R_R:
{
	int rc = xxux(0);
	int ra = xuxx(0);
	int rb = uxxx(0);
	term_t a = get_reg(ra);
	term_t b = get_reg(rb);
	term_t c;
	if (are_both_int(a, b))
	{
		int i = int_value(a) - int_value(b);
		c = int_to_term(i, proc->heap);
	}
	else
	{
		c = sub_mixed(a, b, proc->heap);
		if (c == noval)
			bad_arith0();
	}
	set_reg(rc, c);
	next(1);
}

//asm({'sub',{r,X},{r,Y},{integer,Z}}) when Z =< 127, Z >= -128 -> [?uuuu(Z, Y, X, 16)];
case OP_SUB_R_R_I1:
{
	int rc = xxux(0);
	int ra = xuxx(0);
	int i = Uxxx(0);
	term_t a = get_reg(ra);
	term_t c;
	if (is_int(a))
	{
		i = int_value(a) - i;
		c = int_to_term(i, proc->heap);
	}
	else if (!is_bignum(a) && !is_float(a))
		bad_arith0();
	else
		c = sub_mixed(a, tag_int(i), proc->heap);
	set_reg(rc, c);
	next(1);
}

//asm({'sub',{r,X},{r,Y},{literal,Z}}) -> [?xuuu(Y, X, 19), {literal,Z}];
case OP_SUB_R_R_T:
{
	int rc = xxux(0);
	int ra = xuxx(0);
	term_t a = get_reg(ra);
	term_t b = ip[1].t;
	term_t c;
	if (are_both_int(a, b))
	{
		int i = int_value(a) - int_value(b);
		c = int_to_term(i, proc->heap);
	}
	else
	{
		c = sub_mixed(a, b, proc->heap);
		if (c == noval)
			bad_arith0();
	}
	set_reg(rc, c);
	next(2);
}

//asm({'sub',{r,X},{integer,Y},{r,Z}}) when Y =< 127, Y >= -128 -> [?uuuu(Z, Y, X, 18)];
case OP_SUB_R_I1_R:
{
	int rc = xxux(0);
	int i = xUxx(0);
	int rb = uxxx(0);
	term_t b = get_reg(rb);
	term_t c;
	if (is_int(b))
	{
		i -= int_value(b);
		c = int_to_term(i, proc->heap);
	}
	else if (!is_bignum(b) && !is_float(b))
		bad_arith0();
	else
		c = sub_mixed(tag_int(i), b, proc->heap);
	set_reg(rc, c);
	next(1);
}

//asm({'sub',{r,X},{literal,Y},{r,Z}}) -> [?xuuu(Z, X, 21), {literal,Y}];
case OP_SUB_R_T_R:
{
	int rc = xxux(0);
	int rb = xuxx(0);
	term_t a = ip[1].t;
	term_t b = get_reg(rb);
	term_t c;
	if (are_both_int(a, b))
	{
		int i = int_value(a) - int_value(b);
		c = int_to_term(i, proc->heap);
	}
	else
	{
		c = sub_mixed(a, b, proc->heap);
		if (c == noval)
			bad_arith0();
	}
	set_reg(rc, c);
	next(2);
}

//asm({'div',{r,X},{r,Y},{r,Z}}) -> [?uuuu(Z, Y, X, 20)];
case OP_DIV_R_R_R:
{
	int rc = xxux(0);
	int ra = xuxx(0);
	int rb = uxxx(0);
	term_t a = get_reg(ra);
	term_t b = get_reg(rb);
	term_t c;
	double fa, fb;
	term_box_t *htop;

	if (is_int(a))
		fa = (double)int_value(a);
	else if (is_float(a))
		fa = float_value(a);
	else if (is_bignum(a))
	{
		mp_int mp = bignum_to_mp(a);
		fa = mp_get_double(&mp);
	}
	else
		bad_arith(a);

	if (is_int(b))
		fb = (double)int_value(b);
	else if (is_float(b))
		fb = float_value(b);
	else if (is_bignum(b))
	{
		mp_int mp = bignum_to_mp(b);
		fb = mp_get_double(&mp);
	}
	else
		bad_arith(b);

	htop = (term_box_t *)heap_htop(proc->heap);
	htop->float_value.value = fa / fb;
	heap_htop(proc->heap) += FLOAT_VALUE_T_SIZE;
	heap_needed -= FLOAT_VALUE_T_SIZE;
	c = tag_float(htop);
	
	set_reg(rc, c);
	next(1);
}

//asm({'idiv',{r,X},{r,Y},{r,Z}}) -> [?uuuu(Z, Y, X, 25)];
case OP_IDIV_R_R_R:
{
	int rc = xxux(0);
	int ra = xuxx(0);
	int rb = uxxx(0);
	term_t a = get_reg(ra);
	term_t b = get_reg(rb);
	term_t c;
	if (are_both_int(a, b))
	{
		int i = int_value(a) / int_value(b);
		c = tag_int(i);	// guaranteed to be a small int
	}
	else
	{
		c = idiv_mixed(a, b, proc->heap);
		if (c == noval)
			bad_arith0();
	}
	set_reg(rc, c);
	next(1);
}

//asm({'mod',{r,X},{r,Y},{r,Z}}) -> [?uuuu(Z, Y, X, 30)];
case OP_MOD_R_R_R:
{
	int rc = xxux(0);
	int ra = xuxx(0);
	int rb = uxxx(0);
	term_t a = get_reg(ra);
	term_t b = get_reg(rb);
	term_t c;
	if (are_both_int(a, b))
	{
		int modulo = int_value(a) % int_value(b);
		c = tag_int(modulo);	// guaranteed to be a small int
	}
	else
	{
		c = mod_mixed(a, b, proc->heap);
		if (c == noval)
			bad_arith0();
	}
	set_reg(rc, c);
	next(1);
}

//asm({'neg',{r,X},{r,Y}}) -> [?xuuu(Y, X, 35)];
case OP_NEG_R_R:
{
	int rb = xxux(0);
	int ra = xuxx(0);
	term_t a = get_reg(ra);
	term_t b;
	if (is_int(a))
	{
		int i = -int_value(a);
		b = int_to_term(i, proc->heap);
	}
	else if (is_float(a))
	{
		double f = float_value(a);
		b = heap_float(proc->heap, -f);
	}
	else if (is_bignum(a))
	{
		mp_int ma = bignum_to_mp(a);
		mp_int mb;
		mp_init_size(&mb, USED(&ma), proc->heap);
		mp_neg(&ma, &mb, proc->heap);
		b = mp_to_term(ma);
	}
	else
		bad_arg(a);
	set_reg(rb, b);
	next(1);
}

//asm({'bnot',{r,X},{r,Y}}) -> [?xuuu(Y, X, 36)];
case OP_BNOT_R_R:
{
	int rb = xxux(0);
	int ra = xuxx(0);
	term_t a = get_reg(ra);
	term_t b;
	if (is_int(a))
	{
		int i = ~int_value(a);
		b = tag_int(i);	// small int guaranteed
	}
	else
	{
		b = bnot_mixed(a, proc->heap);
		if (b == noval)
			bad_arith(a);
	}
	set_reg(rb, b);
	next(1);
}

//asm({'not',{r,X},{r,Y}}) -> [?xuuu(Y, X, 37)];
case OP_NOT_R_R:
{
	int rb = xxux(0);
	int ra = xuxx(0);
	term_t a = get_reg(ra);
	term_t b;
	if (a == A_TRUE)
		b = A_FALSE;
	else
		b = A_TRUE;
	set_reg(rb, b);
	next(1);
}

//asm({'band',{r,X},{r,Y},{r,Z}}) -> [?uuuu(Z, Y, X, 38)];
case OP_BAND_R_R_R:
{
	int rc = xxux(0);
	int ra = xuxx(0);
	int rb = uxxx(0);
	term_t a = get_reg(ra);
	term_t b = get_reg(rb);
	term_t c;
	if (are_both_int(a, b))
	{
		int i = int_value(a) & int_value(b);
		c = tag_int(i);
	}
	else
	{
		c = band_mixed(a, b, proc->heap);
		if (c == noval)
			bad_arith0();
	}
	set_reg(rc, c);
	next(1);
}

//asm({'band',{r,X},{r,Y},{integer,Z}}) when Z =< 127, Z >= -128 -> [?uuuu(Z, Y, X, 39)];
case OP_BAND_R_R_I1:
{
	int rc = xxux(0);
	int ra = xuxx(0);
	int i = Uxxx(0);
	term_t a = get_reg(ra);
	term_t c;
	if (is_int(a))
	{
		i &= int_value(a);
		c = tag_int(i);
	}
	else
	{
		c = band_mixed2(a, i, proc->heap);
		if (c == noval)
			bad_arith(a);
	}
	set_reg(rc, c);
	next(1);
}

//asm({'band',{r,X},{r,Y},{literal,Z}}) -> [?xuuu(Y, X, 30), {literal,Z}];
case OP_BAND_R_R_T:
{
	int rc = xxux(0);
	int ra = xuxx(0);
	term_t a = get_reg(ra);
	term_t b = ip[1].t;
	term_t c;
	if (are_both_int(a, b))
	{
		int i = int_value(a) & int_value(b);
		c = int_to_term(i, proc->heap);
	}
	else
	{
		c = band_mixed(a, b, proc->heap);
		if (c == noval)
			bad_arith0();
	}
	set_reg(rc, c);
	next(2);
}

//asm({'and',{r,X},{r,Y},{r,Z}}) -> [?uuuu(Z, Y, X, 41)];
case OP_AND_R_R_R:
{
	int rc = xxux(0);
	int ra = xuxx(0);
	int rb = uxxx(0);
	term_t a = get_reg(ra);
	term_t b = get_reg(rb);
	term_t c;
	if (a == A_TRUE && b == A_TRUE)
		c = A_TRUE;
	else
		c = A_FALSE;
	set_reg(rc, c);
	next(1);
}

//asm({'bor',{r,X},{r,Y},{r,Z}}) -> [?uuuu(Z, Y, X, 42)];
case OP_BOR_R_R_R:
{
	int rc = xxux(0);
	int ra = xuxx(0);
	int rb = uxxx(0);
	term_t a = get_reg(ra);
	term_t b = get_reg(rb);
	term_t c;
	if (are_both_int(a, b))
	{
		int i = int_value(a) | int_value(b);
		c = tag_int(i);
	}
	else
	{
		c = bor_mixed(a, b, proc->heap);
		if (c == noval)
			bad_arith0();
	}
	set_reg(rc, c);
	next(1);
}

//asm({'bor',{r,X},{r,Y},{integer,Z}}) when Z =< 127, Z >= -128 -> [?uuuu(Z, Y, X, 43)];
case OP_BOR_R_R_I1:
{
	int rc = xxux(0);
	int ra = xuxx(0);
	int i = Uxxx(0);
	term_t a = get_reg(ra);
	term_t c;
	if (is_int(a))
	{
		i |= int_value(a);
		c = tag_int(i);
	}
	else
	{
		c = bor_mixed2(a, i, proc->heap);
		if (c == noval)
			bad_arith(a);
	}
	set_reg(rc, c);
	next(1);
}

//asm({'bor',{r,X},{r,Y},{literal,Z}}) -> [?xuuu(Y, X, 34), {literal,Z}];
case OP_BOR_R_R_T:
{
	int rc = xxux(0);
	int ra = xuxx(0);
	term_t a = get_reg(ra);
	term_t b = ip[1].t;
	term_t c;
	if (are_both_int(a, b))
	{
		int i = int_value(a) | int_value(b);
		c = int_to_term(i, proc->heap);
	}
	else
	{
		c = bor_mixed(a, b, proc->heap);
		if (c == noval)
			bad_arith0();
	}
	set_reg(rc, c);
	next(2);
}

//asm({'or',{r,X},{r,Y},{r,Z}}) -> [?uuuu(Z, Y, X, 45)];
case OP_OR_R_R_R:
{
	int rc = xxux(0);
	int ra = xuxx(0);
	int rb = uxxx(0);
	term_t a = get_reg(ra);
	term_t b = get_reg(rb);
	term_t c;
	if (a == A_TRUE || b == A_TRUE)
		c = A_TRUE;
	else
		c = A_FALSE;
	set_reg(rc, c);
	next(1);
}

//asm({'bxor',{r,X},{r,Y},{r,Z}}) -> [?uuuu(Z, Y, X, 46)];
case OP_BXOR_R_R_R:
{
	int rc = xxux(0);
	int ra = xuxx(0);
	int rb = uxxx(0);
	term_t a = get_reg(ra);
	term_t b = get_reg(rb);
	term_t c;
	if (are_both_int(a, b))
	{
		int i = int_value(a) ^ int_value(b);
		c = tag_int(i);
	}
	else
	{
		c = bxor_mixed(a, b, proc->heap);
		if (c == noval)
			bad_arith0();
	}
	set_reg(rc, c);
	next(1);
}

//asm({'bxor',{r,X},{r,Y},{integer,Z}}) when Z =< 127, Z >= -128 -> [?uuuu(Z, Y, X, 47)];
case OP_BXOR_R_R_I1:
{
	int rc = xxux(0);
	int ra = xuxx(0);
	int i = Uxxx(0);
	term_t a = get_reg(ra);
	term_t c;
	if (is_int(a))
	{
		i ^= int_value(a);
		c = tag_int(i);
	}
	else
	{
		c = bxor_mixed2(a, i, proc->heap);
		if (c == noval)
			bad_arith(a);
	}
	set_reg(rc, c);
	next(1);
}

//asm({'bxor',{r,X},{r,Y},{literal,Z}}) -> [?xuuu(Y, X, 38), {literal,Z}];
case OP_BXOR_R_R_T:
{
	int rc = xxux(0);
	int ra = xuxx(0);
	term_t a = get_reg(ra);
	term_t b = ip[1].t;
	term_t c;
	if (are_both_int(a, b))
	{
		int i = int_value(a) ^ int_value(b);
		c = tag_int(i);
	}
	else
	{
		c = bxor_mixed(a, b, proc->heap);
		if (c == noval)
			bad_arith0();
	}
	set_reg(rc, c);
	next(2);
}

//asm({'xor',{r,X},{r,Y},{r,Z}}) -> [?uuuu(Z, Y, X, 49)];
case OP_XOR_R_R_R:
{
	int rc = xxux(0);
	int ra = xuxx(0);
	int rb = uxxx(0);
	term_t a = get_reg(ra);
	term_t b = get_reg(rb);
	term_t c;
	if ((a == A_TRUE) != (b == A_TRUE))
		c = A_TRUE;
	else
		c = A_FALSE;
	set_reg(rc, c);
	next(1);
}

//asm({'bsl',{r,X},{r,Y},{r,Z}}) -> [?uuuu(Z, Y, X, 50)];
case OP_BSL_R_R_R:
{
	int rc = xxux(0);
	int ra = xuxx(0);
	int rb = uxxx(0);
	term_t a = get_reg(ra);
	term_t b = get_reg(rb);
	term_t c;
	if (is_int(a) && is_int(b))
	{
		int ia = int_value(a);
		int ib = int_value(b);
		if (ib <= 0)
			c = tag_int(ia >> (-ib));
		else
		{
			c = bsl_mixed(a, b, proc->heap);
			if (c == noval)
				bad_arith0();
		}
	}
	else
	{
		c = bsl_mixed(a, b, proc->heap);
		if (c == noval)
			bad_arith0();
	}
	set_reg(rc, c);
	next(1);
}

//asm({'bsr',{r,X},{r,Y},{r,Z}}) -> [?uuuu(Z, Y, X, 55)];
case OP_BSR_R_R_R:
{
	int rc = xxux(0);
	int ra = xuxx(0);
	int rb = uxxx(0);
	term_t a = get_reg(ra);
	term_t b = get_reg(rb);
	term_t c;
	if (is_int(a) && is_int(b))
	{
		int ia = int_value(a);
		int ib = int_value(b);
		if (ib >= 0)
			c = tag_int(ia >> ib);
		else
		{
			c = bsr_mixed(a, b, proc->heap);
			if (c == noval)
				bad_arith0();
		}
	}
	else
	{
		c = bsr_mixed(a, b, proc->heap);
		if (c == noval)
			bad_arith0();
	}
	set_reg(rc, c);
	next(1);
}

//asm({'abs',{r,X},{r,Y}}) -> [?xuuu(Y, X, 42)];
case OP_ABS_R_R:
{
	int rr = xxux(0);
	int ra = xuxx(0);
	term_t n = get_reg(ra);
	if (is_int(n))
	{
		int i = int_value(n);
		if (i < 0)
		{
			n = tag_int(-i);
			set_reg(rr, n);
		}
	}
	else if (is_float(n))
	{
		double f =  float_value(n);
		if (f < 0)
		{
			term_box_t *htop = (term_box_t *)heap_htop(proc->heap);
			htop->float_value.value = -f;
			heap_htop(proc->heap) += FLOAT_VALUE_T_SIZE;
			heap_needed -= FLOAT_VALUE_T_SIZE;
			n = tag_float(htop);
			set_reg(rr, n);
		}
	}
	else if (is_bignum(n))
		not_implemented("abs for bignumes");
	else
		bad_arg(n);
	next(1);
}

//asm({'trunc',{r,X},{r,Y}}) -> [?xuuu(Y, X, 42)];
case OP_TRUNC_R_R:
{
	int rr = xxux(0);
	int ra = xuxx(0);
	term_t a = get_reg(ra);
	if (is_float(a))
		//a = double_to_term(float_value(a));
		a = tag_int((int)float_value(a));
	else if (!is_int(a) && !is_bignum(a))
		bad_arg(a);
	set_reg(rr, a);
	next(1);
}

//asm({'round',{r,X},{r,Y}}) -> [?xuuu(Y, X, 44)];
case OP_ROUND_R_R:
{
	int rr = xxux(0);
	int ra = xuxx(0);
	term_t a = get_reg(ra);
	if (is_float(a))
	{
		double fa = float_value(a);
		if (fa >= 0)
			a = tag_int((int)(fa + 0.5));
		else
			a = tag_int((int)(fa - 0.5));
	}
	else if (!is_int(a) && !is_bignum(a))
		bad_arg(a);
	set_reg(rr, a);
	next(1);
}

//asm({'float',{r,X},{r,Y}}) -> [?xuuu(Y, X, 43)];
case OP_FLOAT_R_R:
{
	not_implemented("float r, r");
}

//asm({'is_atom',{r,X},{l,Y}}) -> [?xxuu(X, 60), {l,Y}];
case OP_IS_ATOM_R_L:
{
	int r = xxux(0);
	term_t t = get_reg(r);
	if (is_atom(t))
		next(2);
	else
		jump(1);
}

//asm({'is_binary',{r,X},{l,Y}}) -> [?xxuu(X, 61), {l,Y}];
case OP_IS_BINARY_R_L:
{
	int r = xxux(0);
	term_t t = get_reg(r);
	if (is_binary(t))
		next(2);
	else
		jump(1);
}

//asm({'is_float',{r,X},{l,Y}}) -> [?xxuu(X, 62), {l,Y}];
case OP_IS_FLOAT_R_L:
{
	int r = xxux(0);
	term_t t = get_reg(r);
	if (is_float(t))
		next(2);
	else
		jump(1);
}

//asm({'is_function',{r,X},{l,Y}}) -> [?xxuu(X, 63), {l,Y}];
case OP_IS_FUNCTION_R_L:
{
	int r = xxux(0);
	term_t t = get_reg(r);
	if (is_fun(t))
		next(2);
	else
		jump(1);
}

//asm({'is_function',{r,X},Y,{l,Z}}) when Y =< 255, Y >= 0 -> [?xuuu(Y, X, 64), {l,Z}];
case OP_IS_FUNCTION_R_N_L:
{
	int r = xxux(0);
	int n = xuxx(0);
	term_t t = get_reg(r);
	term_box_t *fb;
	int nfree;
	if (!is_fun(t))
		jump(1);
	fb = peel(t);
	nfree = list_length(fb->fun.frozen);
	if (fb->fun.arity == n + nfree)
		next(2);
	else
		jump(1);
}

//asm({'is_integer',{r,X},{l,Y}}) -> [?xxuu(X, 65), {l,Y}];
case OP_IS_INTEGER_R_L:
{
	int r = xxux(0);
	term_t t = get_reg(r);
	if (is_int(t) || is_bignum(t))
		next(2);
	else
		jump(1);
}

//asm({'is_list',{r,X},{l,Y}}) -> [?xxuu(X, 66), {l,Y}];
case OP_IS_LIST_R_L:
{
	int r = xxux(0);
	term_t t = get_reg(r);
	if (is_list(t))
		next(2);
	else
		jump(1);
}

//asm({'is_cons',{r,X},{l,Y}}) -> [?xxuu(X, 67), {l,Y}];
case OP_IS_CONS_R_L:
{
	int r = xxux(0);
	term_t t = get_reg(r);
	if (is_cons(t))
		next(2);
	else
		jump(1);
}

//asm({'is_nil',{r,X},{l,Y}}) -> [?xxuu(X, 68), {l,Y}];
case OP_IS_NIL_R_L:
{
	int r = xxux(0);
	term_t t = get_reg(r);
	if (is_nil(t))
		next(2);
	else
		jump(1);
}

//asm({'is_not_nil',{r,X},{l,Y}}) -> [?xxuu(X, 69), {l,Y}];
case OP_IS_NOT_NIL_R_L:
{
	int r = xxux(0);
	term_t t = get_reg(r);
	if (!is_nil(t))
		next(2);
	else
		jump(1);
}

//asm({'is_number',{r,X},{l,Y}}) -> [?xxuu(X, 69), {l,Y}];
case OP_IS_NUMBER_R_L:
{
	int r = xxux(0);
	term_t t = get_reg(r);
	if (is_int(t) || is_bignum(t) || is_float(t))
		next(2);
	else
		jump(1);
}

//asm({'is_pid',{r,X},{l,Y}}) -> [?xxuu(X, 70), {l,Y}];
case OP_IS_PID_R_L:
{
	int r = xxux(0);
	term_t t = get_reg(r);
	if (is_pid(t))
		next(2);
	else
		jump(1);
}

//asm({'is_oid',{r,X},{l,Y}}) -> [?xxuu(X, 70), {l,Y}];
case OP_IS_OID_R_L:
{
	int r = xxux(0);
	term_t t = get_reg(r);
	if (is_oid(t))
		next(2);
	else
		jump(1);
}

//asm({'is_tuple',{r,X},{l,Y}}) -> [?xxuu(X, 71), {l,Y}];
case OP_IS_TUPLE_R_L:
{
	int r = xxux(0);
	term_t t = get_reg(r);
	if (is_tuple(t))
		next(2);
	else
		jump(1);
}

//asm({'is_tuple_of_arity',{r,X},{literal,Y},{l,Z}}) when Y =< 255, Y >= 0 -> [?xuuu(Y, X, 59), {l,Z}];
case OP_IS_TUPLE_OF_ARITY_R_U1_L:
{
	int r = xxux(0);
	int arity = xuxx(0);
	term_t t = get_reg(r);
	if (is_tuple(t) && peel(t)->tuple.size == arity)
		next(2);
	else
		jump(1);
}

//asm({'is_tuple_of_arity',{r,X},{literal,Y},{l,Z}}) -> [?xxuu(X, 60), {literal,Y}, {l,Z}];
case OP_IS_TUPLE_OF_ARITY_R_T_L:
{
	int r = xxux(0);
	term_t a = ip[1].t;
	term_t t = get_reg(r);
	if (!is_int(a))
		bad_arg(a);
	if (is_tuple(t) && peel(t)->tuple.size == int_value(a))
		next(2);
	else
		jump(1);
}

//asm({'is_record',{r,X},{atom,Y},{literal,Z},{l,P}}) when Z =< 255, Z >= 0 -> [?uuuu(Z, Y, X, 61), {l,P}];
case OP_IS_RECORD_R_A_U1_L:
{
	int rr = xxux(0);
	term_t rec = tag_atom(xuxx(0));
	int arity = uxxx(0);
	int tuple = get_reg(rr);
	term_box_t *tb;
	if (!is_tuple(tuple))
		jump(1);
	tb = peel(tuple);
	if (tb->tuple.size != arity || tb->tuple.elts[0] != rec)
		jump(1);
	next(2);
}

//asm({'is_record',{r,X},{atom,Y},{literal,Z},{l,P}}) -> [?xuuu(Y, X, 62), {literal,Z}, {l,P}];
case OP_IS_RECORD_R_A_T_L:
{
	int rr = xxux(0);
	term_t rec = tag_atom(xuxx(0));
	term_t a = ip[1].t;
	int arity;
	int tuple = get_reg(rr);
	term_box_t *tb;
	if (!is_int(a))
		bad_arg(a);
	arity = int_value(a);
	if (!is_tuple(tuple))
		jump(2);
	tb = peel(tuple);
	if (tb->tuple.size != arity || tb->tuple.elts[0] != rec)
		jump(2);
	next(3);
}

//asm({'is_record',{r,X},{literal,Y},{literal,Z},{l,P}}) when Z =< 65535, Z >= 0 -> [?dduu(Z, X, 63), {literal,Y}, {l,P}];
case OP_IS_RECORD_R_T_U2_L:
{
	int rr = xxux(0);
	term_t rec = ip[1].t;
	int arity = ddxx(0);
	int tuple = get_reg(rr);
	term_box_t *tb;
	if (!is_tuple(tuple))
		jump(2);
	tb = peel(tuple);
	if (tb->tuple.size != arity || tb->tuple.elts[0] != rec)
		jump(2);
	next(3);
}

//asm({'is_record',{r,X},{literal,Y},{literal,Z},{l,P}}) -> [?xxuu(X, 64), {literal,Y}, {literal,Z}, {l,P}];
case OP_IS_RECORD_R_T_T_L:
{
	int rr = xxux(0);
	term_t rec = ip[1].t;
	term_t a = ip[2].t;
	int arity;
	int tuple = get_reg(rr);
	term_box_t *tb;
	if (!is_int(a))
		bad_arg(a);
	arity = int_value(a);
	if (!is_tuple(tuple))
		jump(3);
	tb = peel(tuple);
	if (tb->tuple.size != arity || tb->tuple.elts[0] != rec)
		jump(3);
	next(4);
}

//asm({'is_atom',{r,X},{r,Y}}) -> [?xuuu(Y, X, 72)];
case OP_IS_ATOM_R_R:
{
	int rb = xxux(0);
	int ra = xuxx(0);
	term_t a = get_reg(ra);
	term_t b = (is_atom(a)) ?A_TRUE :A_FALSE;
	set_reg(rb, b);
	next(1);
}

//asm({'is_binary',{r,X},{r,Y}}) -> [?xuuu(Y, X, 73)];
case OP_IS_BINARY_R_R:
{
	int rb = xxux(0);
	int ra = xuxx(0);
	term_t a = get_reg(ra);
	term_t b = (is_binary(a)) ?A_TRUE :A_FALSE;
	set_reg(rb, b);
	next(1);
}

//asm({'is_float',{r,X},{r,Y}}) -> [?xuuu(Y, X, 74)];
case OP_IS_FLOAT_R_R:
{
	int rb = xxux(0);
	int ra = xuxx(0);
	term_t a = get_reg(ra);
	term_t b = (is_float(a)) ?A_TRUE :A_FALSE;
	set_reg(rb, b);
	next(1);
}

//asm({'is_function',{r,X},{r,Y}}) -> [?xuuu(Y, X, 75)];
case OP_IS_FUNCTION_R_R:
{
	int rb = xxux(0);
	int ra = xuxx(0);
	term_t a = get_reg(ra);
	term_t b = (is_fun(a)) ?A_TRUE :A_FALSE;
	set_reg(rb, b);
	next(1);
}

//asm({'is_function',{r,X},{r,Y},Z}) when Z =< 255, Z >= 0 -> [?uuuu(Z, Y, X, 76)];
case OP_IS_FUNCTION_R_R_N:
{
	int rb = xxux(0);
	int ra = xuxx(0);
	int n = uxxx(0);
	term_t a = get_reg(ra);
	term_t b = (is_fun(a) && peel(a)->fun.arity == n)
		?A_TRUE :A_FALSE;
	set_reg(rb, b);
	next(1);
}

//asm({'is_integer',{r,X},{r,Y}}) -> [?xuuu(Y, X, 77)];
case OP_IS_INTEGER_R_R:
{
	int rb = xxux(0);
	int ra = xuxx(0);
	term_t a = get_reg(ra);
	term_t b = (is_int(a) || is_bignum(a)) ?A_TRUE :A_FALSE;
	set_reg(rb, b);
	next(1);
}

//asm({'is_list',{r,X},{r,Y}}) -> [?xuuu(Y, X, 78)];
case OP_IS_LIST_R_R:
{
	int rb = xxux(0);
	int ra = xuxx(0);
	term_t a = get_reg(ra);
	term_t b = (is_list(a)) ?A_TRUE :A_FALSE;
	set_reg(rb, b);
	next(1);
}

//asm({'is_cons',{r,X},{r,Y}}) -> [?xuuu(Y, X, 79)];
case OP_IS_CONS_R_R:
{
	int rb = xxux(0);
	int ra = xuxx(0);
	term_t a = get_reg(ra);
	term_t b = (is_cons(a)) ?A_TRUE :A_FALSE;
	set_reg(rb, b);
	next(1);
}

//asm({'is_nil',{r,X},{r,Y}}) -> [?xuuu(Y, X, 80)];
case OP_IS_NIL_R_R:
{
	int rb = xxux(0);
	int ra = xuxx(0);
	term_t a = get_reg(ra);
	term_t b = (is_nil(a)) ?A_TRUE :A_FALSE;
	set_reg(rb, b);
	next(1);
}

//asm({'is_not_nil',{r,X},{r,Y}}) -> [?xuuu(Y, X, 67)];
case OP_IS_NOT_NIL_R_R:
{
	int rb = xxux(0);
	int ra = xuxx(0);
	term_t a = get_reg(ra);
	term_t b = (is_nil(a)) ?A_FALSE :A_TRUE;
	set_reg(rb, b);
	next(1);
}

//asm({'is_number',{r,X},{r,Y}}) -> [?xuuu(Y, X, 81)];
case OP_IS_NUMBER_R_R:
{
	int rb = xxux(0);
	int ra = xuxx(0);
	term_t a = get_reg(ra);
	term_t b = (is_int(a) || is_bignum(a) || is_float(a))
		?A_TRUE :A_FALSE;
	set_reg(rb, b);
	next(1);
}

//asm({'is_pid',{r,X},{r,Y}}) -> [?xuuu(Y, X, 82)];
case OP_IS_PID_R_R:
{
	int rb = xxux(0);
	int ra = xuxx(0);
	term_t a = get_reg(ra);
	term_t b = (is_pid(a)) ?A_TRUE :A_FALSE;
	set_reg(rb, b);
	next(1);
}

//asm({'is_oid',{r,X},{r,Y}}) -> [?xuuu(Y, X, 82)];
case OP_IS_OID_R_R:
{
	int rb = xxux(0);
	int ra = xuxx(0);
	term_t a = get_reg(ra);
	term_t b = (is_oid(a)) ?A_TRUE :A_FALSE;
	set_reg(rb, b);
	next(1);
}

//asm({'is_tuple',{r,X},{r,Y}}) -> [?xuuu(Y, X, 83)];
case OP_IS_TUPLE_R_R:
{
	int rb = xxux(0);
	int ra = xuxx(0);
	term_t a = get_reg(ra);
	term_t b = (is_tuple(a)) ?A_TRUE :A_FALSE;
	set_reg(rb, b);
	next(1);
}

//asm({'eq',{r,X},{r,Y},{l,Z}}) -> [?xuuu(Y, X, 84), {l,Z}];
case OP_EQ_R_R_L:
{
	int ra = xxux(0);
	int rb = xuxx(0);
	term_t a = get_reg(ra);
	term_t b = get_reg(rb);
	if (are_terms_equal(a, b))
		next(2);
	else
		jump(1);
}

//asm({'eq',{r,X},{integer,Y},{l,Z}}) when Y =< 32767, Y >= -32768 -> [?dduu(Y, X, 85), {l,Z}];
case OP_EQ_R_I2_L:
{
	int ra = xxux(0);
	int i = DDxx(0);
	term_t a = get_reg(ra);
	if (is_int(a) && int_value(a) == i)
		next(2);
	else
		jump(1);
}

//asm({'eq',{r,X},{atom,Y},{l,Z}}) -> [?xuuu(Y, X, 87), {l,Z}];
case OP_EQ_R_A_L:
{
	int ra = xxux(0);
	int i = xuxx(0);
	term_t a = get_reg(ra);
	if (is_atom(a) && atom_index(a) == i)
		next(2);
	else
		jump(1);
}

//asm({'eq',{r,X},{literal,Y},{l,Z}}) -> [?xxuu(X, 88), {literal,Y}, {l,Z}];
case OP_EQ_R_T_L:
{
	int ra = xxux(0);
	int t = ip[1].t;
	term_t a = get_reg(ra);
	if (are_terms_equal(a, t))
		next(3);
	else
		jump(2);
}

//asm({'neq',{r,X},{r,Y},{l,Z}}) -> [?xuuu(Y, X, 89), {l,Z}];
case OP_NEQ_R_R_L:
{
	int ra = xxux(0);
	int rb = xuxx(0);
	term_t a = get_reg(ra);
	term_t b = get_reg(rb);
	if (!are_terms_equal(a, b))
		next(2);
	else
		jump(1);
}

//asm({'neq',{r,X},{integer,Y},{l,Z}}) when Y =< 32767, Y >= -32768 -> [?dduu(Y, X, 90), {l,Z}];
case OP_NEQ_R_I2_L:
{
	int ra = xxux(0);
	int i = DDxx(0);
	term_t a = get_reg(ra);
	if (!is_int(a) || int_value(a) != i)
		next(2);
	else
		jump(1);
}

//asm({'neq',{r,X},{atom,Y},{l,Z}}) -> [?xuuu(Y, X, 92), {l,Z}];
case OP_NEQ_R_A_L:
{
	int ra = xxux(0);
	int i = xuxx(0);
	term_t a = get_reg(ra);
	if (!is_atom(a) || atom_index(a) != i)
		next(2);
	else
		jump(1);
}

//asm({'neq',{r,X},{literal,Y},{l,Z}}) -> [?xxuu(X, 93), {literal,Y}, {l,Z}];
case OP_NEQ_R_T_L:
{
	int ra = xxux(0);
	int t = ip[1].t;
	term_t a = get_reg(ra);
	if (!are_terms_equal(a, t))
		next(3);
	else
		jump(2);
}

//asm({'lesseq',{r,X},{r,Y},{l,Z}}) -> [?xuuu(Y, X, 94), {l,Z}];
case OP_LESSEQ_R_R_L:
{
	int ra = xxux(0);
	int rb = xuxx(0);
	term_t a = get_reg(ra);
	term_t b = get_reg(rb);
	if (!is_term_bigger(a, b, proc->teevm->atoms))
		next(2);
	else
		jump(1);
}

//asm({'lesseq',{r,X},{integer,Y},{l,Z}}) when Y =< 32767, Y >= -32768 -> [?dduu(Y, X, 95), {l,Z}];
case OP_LESSEQ_R_I2_L:
{
	int ra = xxux(0);
	int i = DDxx(0);
	term_t a = get_reg(ra);
	if (!is_term_bigger(a, tag_int(i), proc->teevm->atoms))
		next(2);
	else
		jump(1);
}

//asm({'lesseq',{r,X},{atom,Y},{l,Z}}) -> [?xuuu(Y, X, 97), {l,Z}];
case OP_LESSEQ_R_A_L:
{
	int ra = xxux(0);
	int i = xuxx(0);
	term_t a = get_reg(ra);
	if (!is_term_bigger(a, tag_atom(i), proc->teevm->atoms))
		next(2);
	else
		jump(1);
}

//asm({'lesseq',{r,X},{literal,Y},{l,Z}}) -> [?xxuu(X, 98), {literal,Y}, {l,Z}];
case OP_LESSEQ_R_T_L:
{
	int ra = xxux(0);
	int t = ip[1].t;
	term_t a = get_reg(ra);
	if (!is_term_bigger(a, t, proc->teevm->atoms))
		next(3);
	else
		jump(2);
}

//asm({'moreeq',{r,X},{r,Y},{l,Z}}) -> [?xuuu(Y, X, 99), {l,Z}];
case OP_MOREEQ_R_R_L:
{
	int ra = xxux(0);
	int rb = xuxx(0);
	term_t a = get_reg(ra);
	term_t b = get_reg(rb);
	if (!is_term_smaller(a, b, proc->teevm->atoms))
		next(2);
	else
		jump(1);
}

//asm({'moreeq',{r,X},{integer,Y},{l,Z}}) when Y =< 32767, Y >= -32768 -> [?dduu(Y, X, 100), {l,Z}];
case OP_MOREEQ_R_I2_L:
{
	int ra = xxux(0);
	int i = DDxx(0);
	term_t a = get_reg(ra);
	if (!is_term_smaller(a, tag_int(i), proc->teevm->atoms))
		next(2);
	else
		jump(1);
}

//asm({'moreeq',{r,X},{atom,Y},{l,Z}}) -> [?xuuu(Y, X, 102), {l,Z}];
case OP_MOREEQ_R_A_L:
{
	int ra = xxux(0);
	int i = xuxx(0);
	term_t a = get_reg(ra);
	if (!is_term_smaller(a, tag_atom(i), proc->teevm->atoms))
		next(2);
	else
		jump(1);
}

//asm({'moreeq',{r,X},{literal,Y},{l,Z}}) -> [?xxuu(X, 103), {literal,Y}, {l,Z}];
case OP_MOREEQ_R_T_L:
{
	int ra = xxux(0);
	int t = ip[1].t;
	term_t a = get_reg(ra);
	if (!is_term_smaller(a, t, proc->teevm->atoms))
		next(3);
	else
		jump(2);
}

//asm({'less',{r,X},{r,Y},{l,Z}}) -> [?xuuu(Y, X, 104), {l,Z}];
case OP_LESS_R_R_L:
{
	int ra = xxux(0);
	int rb = xuxx(0);
	term_t a = get_reg(ra);
	term_t b = get_reg(rb);
	if (is_term_smaller(a, b, proc->teevm->atoms))
		next(2);
	else
		jump(1);
}

//asm({'less',{r,X},{integer,Y},{l,Z}}) when Y =< 32767, Y >= -32768 -> [?dduu(Y, X, 105), {l,Z}];
case OP_LESS_R_I2_L:
{
	int ra = xxux(0);
	int i = DDxx(0);
	term_t a = get_reg(ra);
	if (is_int(a) && int_value(a) < i) // numbers are less than other terms
		next(2);
	else
		jump(1);
}

//asm({'less',{r,X},{atom,Y},{l,Z}}) -> [?xuuu(Y, X, 107), {l,Z}];
case OP_LESS_R_A_L:
{
	int ra = xxux(0);
	int i = xuxx(0);
	term_t a = get_reg(ra);
	if (is_term_smaller(a, tag_atom(i), proc->teevm->atoms))
		next(2);
	else
		jump(1);
}

//asm({'less',{r,X},{literal,Y},{l,Z}}) -> [?xxuu(X, 108), {literal,Y}, {l,Z}];
case OP_LESS_R_T_L:
{
	int ra = xxux(0);
	int t = ip[1].t;
	term_t a = get_reg(ra);
	if (is_term_smaller(a, t, proc->teevm->atoms))
		next(3);
	else
		jump(2);
}

//asm({'more',{r,X},{r,Y},{l,Z}}) -> [?xuuu(Y, X, 109), {l,Z}];
case OP_MORE_R_R_L:
{
	int ra = xxux(0);
	int rb = xuxx(0);
	term_t a = get_reg(ra);
	term_t b = get_reg(rb);
	if (is_term_bigger(a, b, proc->teevm->atoms))
		next(2);
	else
		jump(1);
}

//asm({'more',{r,X},{integer,Y},{l,Z}}) when Y =< 32767, Y >= -32768 -> [?dduu(Y, X, 110), {l,Z}];
case OP_MORE_R_I2_L:
{
	int ra = xxux(0);
	int i = DDxx(0);
	term_t a = get_reg(ra);
	if (!is_int(a) || int_value(a) > i) // numbers are less than other terms
		next(2);
	else
		jump(1);
}

//asm({'more',{r,X},{atom,Y},{l,Z}}) -> [?xuuu(Y, X, 112), {l,Z}];
case OP_MORE_R_A_L:
{
	int ra = xxux(0);
	int i = xuxx(0);
	term_t a = get_reg(ra);
	if (is_term_bigger(a, tag_atom(i), proc->teevm->atoms))
		next(2);
	else
		jump(1);
}

//asm({'more',{r,X},{literal,Y},{l,Z}}) -> [?xxuu(X, 113), {literal,Y}, {l,Z}];
case OP_MORE_R_T_L:
{
	int ra = xxux(0);
	int t = ip[1].t;
	term_t a = get_reg(ra);
	if (is_term_bigger(a, t, proc->teevm->atoms))
		next(3);
	else
		jump(2);
}

//asm({'is_true',{r,X},{l,Y}}) -> [?xxuu(X, 114), {l,Y}];
case OP_IS_TRUE_R_L:
{
	int ra = xxux(0);
	if (get_reg(ra) == A_TRUE)
		next(2);
	else
		jump(1);
}

//asm({'is_false',{r,X},{l,Y}}) -> [?xxuu(X, 115), {l,Y}];
case OP_IS_FALSE_R_L:
{
	int ra = xxux(0);
	if (get_reg(ra) == A_FALSE)
		next(2);
	else
		jump(1);
}

//asm({'eq',{r,X},{r,Y},{r,Z}}) -> [?uuuu(Z, Y, X, 116)];
case OP_EQ_R_R_R:
{
	int rc = xxux(0);
	int ra = xuxx(0);
	int rb = uxxx(0);
	term_t a = get_reg(ra);
	term_t b = get_reg(rb);
	term_t c = (are_terms_equal(a, b))
		?A_TRUE :A_FALSE;
	set_reg(rc, c);
	next(1);
}

//asm({'eq',{r,X},{r,Y},{integer,Z}}) when Z =< 127, Z >= -128 -> [?uuuu(Z, Y, X, 117)];
case OP_EQ_R_R_I1:
{
	int rc = xxux(0);
	int ra = xuxx(0);
	int i = Uxxx(0);
	term_t a = get_reg(ra);
	term_t c = (is_int(a) && int_value(a) == i)
		?A_TRUE :A_FALSE;
	set_reg(rc, c);
	next(1);
}

//asm({'eq',{r,X},{r,Y},{atom,Z}}) -> [?uuuu(Z, Y, X, 119)];
case OP_EQ_R_R_A:
{
	int rc = xxux(0);
	int ra = xuxx(0);
	int i = xuxx(0);
	term_t a = get_reg(ra);
	term_t c = (is_atom(a) && atom_index(a) == i)
		?A_TRUE :A_FALSE;
	set_reg(rc, c);
	next(1);
}

//asm({'eq',{r,X},{r,Y},{literal,Z}}) -> [?xuuu(Y, X, 120), {literal,Z}];
case OP_EQ_R_R_T:
{
	int rc = xxux(0);
	int ra = xuxx(0);
	int t = ip[1].t;
	term_t a = get_reg(ra);
	term_t c = (are_terms_equal(a, t))
		?A_TRUE :A_FALSE;
	set_reg(rc, c);
	next(2);
}

//asm({'neq',{r,X},{r,Y},{r,Z}}) -> [?uuuu(Z, Y, X, 121)];
case OP_NEQ_R_R_R:
{
	int rc = xxux(0);
	int ra = xuxx(0);
	int rb = uxxx(0);
	term_t a = get_reg(ra);
	term_t b = get_reg(rb);
	term_t c = (!are_terms_equal(a, b))
		?A_TRUE :A_FALSE;
	set_reg(rc, c);
	next(1);
}

//asm({'neq',{r,X},{r,Y},{integer,Z}}) when Z =< 127, Z >= -128 -> [?uuuu(Z, Y, X, 122)];
case OP_NEQ_R_R_I1:
{
	int rc = xxux(0);
	int ra = xuxx(0);
	int i = Uxxx(0);
	term_t a = get_reg(ra);
	term_t c = (!is_int(a) || int_value(a) != i)
		?A_TRUE :A_FALSE;
	set_reg(rc, c);
	next(1);
}

//asm({'neq',{r,X},{r,Y},{atom,Z}}) -> [?uuuu(Z, Y, X, 124)];
case OP_NEQ_R_R_A:
{
	int rc = xxux(0);
	int ra = xuxx(0);
	int i = xuxx(0);
	term_t a = get_reg(ra);
	term_t c = (!is_atom(a) || atom_index(a) != i)
		?A_TRUE :A_FALSE;
	set_reg(rc, c);
	next(1);
}

//asm({'neq',{r,X},{r,Y},{literal,Z}}) -> [?xuuu(Y, X, 125), {literal,Z}];
case OP_NEQ_R_R_T:
{
	int rc = xxux(0);
	int ra = xuxx(0);
	int t = ip[1].t;
	term_t a = get_reg(ra);
	term_t c = (!are_terms_equal(a, t))
		?A_TRUE :A_FALSE;
	set_reg(rc, c);
	next(2);
}

//asm({'lesseq',{r,X},{r,Y},{r,Z}}) -> [?uuuu(Z, Y, X, 126)];
case OP_LESSEQ_R_R_R:
{
	int rc = xxux(0);
	int ra = xuxx(0);
	int rb = uxxx(0);
	term_t a = get_reg(ra);
	term_t b = get_reg(rb);
	term_t c = (!is_term_bigger(a, b, proc->teevm->atoms))
		?A_TRUE :A_FALSE;
	set_reg(rc, c);
	next(1);
}

//asm({'lesseq',{r,X},{r,Y},{integer,Z}}) when Z =< 127, Z >= -128 -> [?uuuu(Z, Y, X, 114)];
case OP_LESSEQ_R_R_I1:
{
	int rc = xxux(0);
	int ra = xuxx(0);
	int i = Uxxx(0);
	term_t a = get_reg(ra);
	term_t c = (!is_term_bigger(a, tag_int(i), proc->teevm->atoms))
		?A_TRUE :A_FALSE;
	set_reg(rc, c);
	next(1);
}

//asm({'lesseq',{r,X},{r,Y},{atom,Z}}) -> [?uuuu(Z, Y, X, 129)];
case OP_LESSEQ_R_R_A:
{
	int rc = xxux(0);
	int ra = xuxx(0);
	int i = xuxx(0);
	term_t a = get_reg(ra);
	term_t c = (!is_term_bigger(a, tag_atom(i), proc->teevm->atoms))
		?A_TRUE :A_FALSE;
	set_reg(rc, c);
	next(1);
}

//asm({'lesseq',{r,X},{r,Y},{literal,Z}}) -> [?xuuu(Y, X, 130), {literal,Z}];
case OP_LESSEQ_R_R_T:
{
	int rc = xxux(0);
	int ra = xuxx(0);
	int t = ip[1].t;
	term_t a = get_reg(ra);
	term_t c = (!is_term_bigger(a, t, proc->teevm->atoms))
		?A_TRUE :A_FALSE;
	set_reg(rc, c);
	next(2);
}

//asm({'moreeq',{r,X},{r,Y},{r,Z}}) -> [?uuuu(Z, Y, X, 131)];
case OP_MOREEQ_R_R_R:
{
	int rc = xxux(0);
	int ra = xuxx(0);
	int rb = uxxx(0);
	term_t a = get_reg(ra);
	term_t b = get_reg(rb);
	term_t c = (!is_term_smaller(a, b, proc->teevm->atoms))
		?A_TRUE :A_FALSE;
	set_reg(rc, c);
	next(1);
}

//asm({'moreeq',{r,X},{r,Y},{integer,Z}}) when Z =< 127, Z >= -128 -> [?uuuu(Z, Y, X, 132)];
case OP_MOREEQ_R_R_I1:
{
	int rc = xxux(0);
	int ra = xuxx(0);
	int i = Uxxx(0);
	term_t a = get_reg(ra);
	term_t c = (!is_term_smaller(a, tag_int(i), proc->teevm->atoms))
		?A_TRUE :A_FALSE;
	set_reg(rc, c);
	next(1);
}

//asm({'moreeq',{r,X},{r,Y},{atom,Z}}) -> [?uuuu(Z, Y, X, 134)];
case OP_MOREEQ_R_R_A:
{
	int rc = xxux(0);
	int ra = xuxx(0);
	int i = xuxx(0);
	term_t a = get_reg(ra);
	term_t c = (!is_term_smaller(a, tag_atom(i), proc->teevm->atoms))
		?A_TRUE :A_FALSE;
	set_reg(rc, c);
	next(1);
}

//asm({'moreeq',{r,X},{r,Y},{literal,Z}}) -> [?xuuu(Y, X, 135), {literal,Z}];
case OP_MOREEQ_R_R_T:
{
	int rc = xxux(0);
	int ra = xuxx(0);
	int t = ip[1].t;
	term_t a = get_reg(ra);
	term_t c = (!is_term_smaller(a, t, proc->teevm->atoms))
		?A_TRUE :A_FALSE;
	set_reg(rc, c);
	next(2);
}

//asm({'less',{r,X},{r,Y},{r,Z}}) -> [?uuuu(Z, Y, X, 136)];
case OP_LESS_R_R_R:
{
	int rc = xxux(0);
	int ra = xuxx(0);
	int rb = uxxx(0);
	term_t a = get_reg(ra);
	term_t b = get_reg(rb);
	term_t c = (is_term_smaller(a, b, proc->teevm->atoms))
		?A_TRUE :A_FALSE;
	set_reg(rc, c);
	next(1);
}

//asm({'less',{r,X},{r,Y},{integer,Z}}) when Z =< 127, Z >= -128 -> [?uuuu(Z, Y, X, 137)];
case OP_LESS_R_R_I1:
{
	int rc = xxux(0);
	int ra = xuxx(0);
	int i = Uxxx(0);
	term_t a = get_reg(ra);
	term_t c = (is_term_smaller(a, tag_int(i), proc->teevm->atoms))
		?A_TRUE :A_FALSE;
	set_reg(rc, c);
	next(1);
}

//asm({'less',{r,X},{r,Y},{atom,Z}}) -> [?uuuu(Z, Y, X, 139)];
case OP_LESS_R_R_A:
{
	int rc = xxux(0);
	int ra = xuxx(0);
	int i = xuxx(0);
	term_t a = get_reg(ra);
	term_t c = (is_term_smaller(a, tag_atom(i), proc->teevm->atoms))
		?A_TRUE :A_FALSE;
	set_reg(rc, c);
	next(1);
}

//asm({'less',{r,X},{r,Y},{literal,Z}}) -> [?xuuu(Y, X, 140), {literal,Z}];
case OP_LESS_R_R_T:
{
	int rc = xxux(0);
	int ra = xuxx(0);
	int t = ip[1].t;
	term_t a = get_reg(ra);
	term_t c = (is_term_smaller(a, t, proc->teevm->atoms))
		?A_TRUE :A_FALSE;
	set_reg(rc, c);
	next(2);
}

//asm({'more',{r,X},{r,Y},{r,Z}}) -> [?uuuu(Z, Y, X, 141)];
case OP_MORE_R_R_R:
{
	int rc = xxux(0);
	int ra = xuxx(0);
	int rb = uxxx(0);
	term_t a = get_reg(ra);
	term_t b = get_reg(rb);
	term_t c = (is_term_bigger(a, b, proc->teevm->atoms))
		?A_TRUE :A_FALSE;
	set_reg(rc, c);
	next(1);
}

//asm({'more',{r,X},{r,Y},{integer,Z}}) when Z =< 127, Z >= -128 -> [?uuuu(Z, Y, X, 142)];
case OP_MORE_R_R_I1:
{
	int rc = xxux(0);
	int ra = xuxx(0);
	int i = Uxxx(0);
	term_t a = get_reg(ra);
	term_t c = (is_term_bigger(a, tag_int(i), proc->teevm->atoms))
		?A_TRUE :A_FALSE;
	set_reg(rc, c);
	next(1);
}

//asm({'more',{r,X},{r,Y},{atom,Z}}) -> [?uuuu(Z, Y, X, 144)];
case OP_MORE_R_R_A:
{
	int rc = xxux(0);
	int ra = xuxx(0);
	int i = xuxx(0);
	term_t a = get_reg(ra);
	term_t c = (is_term_bigger(a, tag_atom(i), proc->teevm->atoms))
		?A_TRUE :A_FALSE;
	set_reg(rc, c);
	next(1);
}

//asm({'more',{r,X},{r,Y},{literal,Z}}) -> [?xuuu(Y, X, 145), {literal,Z}];
case OP_MORE_R_R_T:
{
	int rc = xxux(0);
	int ra = xuxx(0);
	int t = ip[1].t;
	term_t a = get_reg(ra);
	term_t c = (is_term_bigger(a, t, proc->teevm->atoms))
		?A_TRUE :A_FALSE;
	set_reg(rc, c);
	next(2);
}

//asm({'frame',X,{literal,Y},{literal,Z}}) when X =< 255, X >= 0, Y =< 255, Y >= 0, Z =< 255, Z >= 0 -> [?uuuu(Z, Y, X, 129)];
case OP_FRAME_N_U1_U1:
{
	tmp1 = xxux(0); // arity
	tmp2 = 1;

	nslots = xuxx(0);
	heap_needed = uxxx(0);

op_frame:

	if (--reductions == 0)
	{
		int i;

		proc->capsule.module = module;
		proc->capsule.ip = ip;
		proc->capsule.arity = tmp1;

		proc->capsule.my_frame = my_frame;
		proc->capsule.nslots = nslots;
		proc->capsule.heap_needed = heap_needed;

		for (i = 0; i < tmp1; i++)
			proc->capsule.registers[i] = get_reg(i);

		proc->result.what = SLICE_RESULT_YIELD;
		goto schedule;
	}

	// make sure that the current frame (created by call
	// or spawn) has enough space for slots

	proc->stack_node->first_avail = (char *)my_frame->slots;
	proc_secure_stack_space(proc, nslots);
	my_frame = (frame_t *)(proc->stack_node->first_avail - sizeof(frame_t));
	proc->stack_node->first_avail += nslots*sizeof(term_t);

	heap_anticipate_need(proc->heap, heap_needed);

	next(tmp2);
}

//asm({'frame',X,{literal,Y},{literal,Z}}) when X =< 255, X >= 0, Y =< 65535, Y >= 0 -> [?dduu(Y, X, 130), {literal,Z}];
case OP_FRAME_N_U2_T:
{
	term_t t = ip[1].t;
	if (!is_int(t))
		bad_arg(t);

	tmp1 = xxux(0);	// arity
	tmp2 = 2;

	nslots = ddxx(0);
	heap_needed = int_value(t);

	goto op_frame;
}

//asm({'frame',X,{literal,Y},{literal,Z}}) when X =< 255, X >= 0, Z =< 65535, Z >= 0 -> [?dduu(Z, X, 131), {literal,Y}];
case OP_FRAME_N_T_U2:
{
	term_t t = ip[1].t;
	if (!is_int(t))
		bad_arg(t);

	tmp1 = xxux(0);	// arity
	tmp2 = 2;

	nslots = int_value(t);
	heap_needed = ddxx(0);

	goto op_frame;
}

//asm({'enter',{atom,X},{atom,Y},Z}) when Z =< 255, Z >= 0 -> [?uuuu(Z, Y, X, 149)];
case OP_ENTER_A_A_N:
{
	module_t *m;
	codel_t *entry;

	int a = xxux(0);
	int b = xuxx(0);

	tmp1 = uxxx(0);		// arity
	q1 = tag_atom(a);	// mod
	q2 = tag_atom(b);	// fun

op_enter:

	m = code_base_lookup(proc->teevm->base, q1);
	if (m == 0)
	{
		term_t first = nil;
		term_t last = nil;
		int i;

		m = code_base_lookup(proc->teevm->base, A_ERROR_HANDLER);
		entry = module_lookup(m, A_UNDEFINED_FUNCTION, 3);

		for (i = 0; i < tmp1; i++)
		{
			term_t v = get_reg(i);
			cons_up(first, last, v, proc->heap);
		}

		set_reg(0, q1);
		set_reg(1, q2);
		set_reg(2, first);
	}
	else
	{
		entry = module_lookup(m, q2, tmp1);
		if (entry == 0)
		{
			term_t reason = heap_tuple4(proc->heap, A_UNDEF, q1, q2, tag_int(tmp1));
			exception(SLICE_RESULT_ERROR, reason);
		}
	}

	heap_reclaim_unused(proc->heap, heap_needed);
	module = m;
	ip = entry;
	goto dispatch;
}

//asm({'enter',{atom,X},{literal,Y},Z}) when Z =< 255, Z >= 0 -> [?xuuu(Z, X, 150), {literal,Y}];
case OP_ENTER_A_T_N:
{
	int a = xxux(0);

	tmp1 = xuxx(0);		// arity
	q1 = tag_atom(a);	// mod
	q2 = ip[1].t;		// fun

	goto op_enter;
}

//asm({'enter',{literal,X},{atom,Y},Z}) when Z =< 255, Z >= 0 -> [?xuuu(Z, Y, 151), {literal,X}];
case OP_ENTER_T_A_N:
{
	int a = xxux(0);

	tmp1 = xuxx(0);		// arity
	q1 = ip[1].t;		// mod
	q2 = tag_atom(a);	// fun

	goto op_enter;
}

//asm({'enter',{literal,X},{literal,Y},Z}) when Z =< 255, Z >= 0 -> [?xxuu(Z, 152), {literal,X}, {literal,Y}];
case OP_ENTER_T_T_N:
{
	tmp1 = xxux(0);		// arity
	q1 = ip[1].t;		// mod
	q2 = ip[2].t;		// fun

	goto op_enter;
}

//asm({'enter',{r,X},{r,Y},Z}) when Z =< 255, Z >= 0 -> [?uuuu(Z, Y, X, 138)];
case OP_ENTER_R_R_N:
{
	int rm = xxux(0);
	int rf = xuxx(0);
	
	tmp1 = uxxx(0);
	q1 = get_reg(rm);	// mod
	q2 = get_reg(rf);	// fun

	goto op_enter;
}

//asm({'enter',{l,X}}) -> [?xxxu(153), {l,X}];
case OP_ENTER_L:
{
	heap_reclaim_unused(proc->heap, heap_needed);
	jump(1);
}

//asm({'enter_fun',X,{r,Y}}) when X =< 255, X >= 0 -> [?xuuu(Y, X, 154)];
case OP_ENTER_FUN_N_R:
{
	int arity = xxux(0);
	int r = xuxx(0);
	term_t f = get_reg(r);
	term_box_t *fb = peel(f);
	int nfree = list_length(fb->fun.frozen);

	if (fb->fun.arity != arity + nfree)
		bad_arg(f);

	if (!module->key.is_old && fb->fun.module == module->key.module)
	{
		fun_slot_t *slot = &module->funs[fb->fun.index];
		int i = arity;
		term_t cons = fb->fun.frozen;

		// TODO: if (slot->uniq != fb->fun.uniq)

		while(is_cons(cons))
		{
			term_box_t *cb = peel(cons);
			term_t v = cb->cons.head;
			set_reg(i, v);
			i++;
			cons = cb->cons.tail;
		}
		
		heap_reclaim_unused(proc->heap, heap_needed);
		ip = slot->entry;
		goto dispatch;
	}
	else
		not_implemented("enter_fun: different module");
}

//asm({'enter_bif',X,{bif,Y}}) when X =< 255, X >= 0 -> [?xxuu(X, 155), {bif,Y}];
case OP_ENTER_BIF_N_B:
{
	int n = xxux(0);
	bifN_t entry = ip[1].bif;
	switch (n)
	{
	case 0: set_reg(0, ((bif0_t)entry)(proc)); break;
	case 1: set_reg(0, ((bif1_t)entry)(get_reg(0), proc)); break;
	case 2: set_reg(0, ((bif2_t)entry)(get_reg(0), get_reg(1), proc)); break;
	case 3: set_reg(0, ((bif3_t)entry)(get_reg(0), get_reg(1), get_reg(2), proc)); break;
	case 4: set_reg(0, ((bif4_t)entry)(get_reg(0), get_reg(1), get_reg(2), get_reg(3), proc)); break;
	case 5: set_reg(0, ((bif5_t)entry)(get_reg(0), get_reg(1), get_reg(2), get_reg(3), get_reg(4), proc)); break;
	case 6: set_reg(0, ((bif6_t)entry)(get_reg(0), get_reg(1), get_reg(2), get_reg(3), get_reg(4), get_reg(5), proc)); break;
	case 7: set_reg(0, ((bif7_t)entry)(get_reg(0), get_reg(1), get_reg(2), get_reg(3), get_reg(4), get_reg(5), get_reg(6), proc)); break;
	case 8: set_reg(0, ((bif8_t)entry)(get_reg(0), get_reg(1), get_reg(2), get_reg(3), get_reg(4), get_reg(5), get_reg(6), get_reg(7), proc)); break;
	}

	if (get_reg(0) == noval)
		exception0();		//result is filled in appropriately

	goto return_now;		// TODO: fall-through to OP_RETURN
}

//asm('enter_apply') -> [?xxxu(145)];
case OP_ENTER_APPLY:
{
	term_t m = get_reg(0);
	term_t f = get_reg(1);
	term_t a = get_reg(2);

	module = code_base_lookup(proc->teevm->base, m);
	if (module == 0)
	{
		module = code_base_lookup(proc->teevm->base, A_ERROR_HANDLER);
		ip = module_lookup(module, A_UNDEFINED_FUNCTION, 3);

		set_reg(0, m);
		set_reg(1, f);
		set_reg(2, a);
	}
	else
	{
		int i = 0;
		int arity = list_length(a);
		ip = module_lookup(module, f, arity);
		if (ip == 0)
		{
			term_t reason = heap_tuple4(proc->heap, A_UNDEF, m, f, tag_int(arity));
			exception(SLICE_RESULT_ERROR, reason);
		}

		while (is_cons(a))
		{
			term_box_t *cb = peel(a);
			term_t v = cb->cons.head;
			set_reg(i, v);
			i++;
			a = cb->cons.tail;
		}
	}

	heap_reclaim_unused(proc->heap, heap_needed);
	goto dispatch;
}

//asm({'jump',{l,X}}) -> [?xxxu(156), {l,X}];
case OP_JUMP_L:
{
	jump(1);
}

//asm({'call',{atom,X},{atom,Y},Z}) when Z =< 255, Z >= 0 -> [?uuuu(Z, Y, X, 157)];
case OP_CALL_A_A_N:
{
	module_t *m;
	codel_t *entry;

	q1 = tag_atom(xxux(0));	// mod
	q2 = tag_atom(xuxx(0));	// fun
	tmp1 = uxxx(0);			// arity
	tmp2 = 1;				// op size

op_call:

	m = code_base_lookup(proc->teevm->base, q1);
	if (m == 0)
	{
		term_t first = nil;
		term_t last = nil;
		int i;

		m = code_base_lookup(proc->teevm->base, A_ERROR_HANDLER);
		entry = module_lookup(m, A_UNDEFINED_FUNCTION, 3);

		for (i = 0; i < tmp1; i++)
		{
			term_t v = get_reg(i);
			cons_up(first, last, v, proc->heap);
		}

		set_reg(0, q1);
		set_reg(1, q2);
		set_reg(2, first);
	}
	else
	{
		entry = module_lookup(m, q2, tmp1);
		if (entry == 0)
		{
			term_t reason = heap_tuple4(proc->heap, A_UNDEF, q1, q2, tag_int(tmp1));
			exception(SLICE_RESULT_ERROR, reason);
		}
	}

	my_frame = (frame_t *)proc->stack_node->first_avail;
	my_frame->saved_ip = ip + tmp2;
	my_frame->saved_module = module;
	my_frame->saved_nslots = nslots;
	my_frame->saved_heap_needed = heap_needed;
	proc->stack_node->first_avail += sizeof(frame_t);

	module = m;
	ip = entry;
	goto dispatch;
}

//asm({'call',{atom,X},{literal,Y},Z}) when Z =< 255, Z >= 0 -> [?xuuu(Z, X, 158), {literal,Y}];
case OP_CALL_A_T_N:
{
	q1 = tag_atom(xxux(0));	// mod
	q2 = ip[1].t;			// fun
	tmp1 = xuxx(0);			// arity
	tmp2 = 2;				// op size

	goto op_call;
}

//asm({'call',{literal,X},{atom,Y},Z}) when Z =< 255, Z >= 0 -> [?xuuu(Z, Y, 159), {literal,X}];
case OP_CALL_T_A_N:
{
	q1 = ip[1].t;			// mod
	q2 = tag_atom(xxux(0));	// fun
	tmp1 = xuxx(0);			// arity
	tmp2 = 2;				// op size

	goto op_call;
}

//asm({'call',{literal,X},{literal,Y},Z}) when Z =< 255, Z >= 0 -> [?xxuu(Z, 160), {literal,X}, {literal,Y}];
case OP_CALL_T_T_N:
{
	q1 = ip[1].t;			// mod
	q2 = ip[2].t;			// fun
	tmp1 = xxux(0);			// arity
	tmp2 = 3;				// op size

	goto op_call;
}

//asm({'call',{r,X},{r,Y},Z}) when Z =< 255, Z >= 0 -> [?uuuu(Z, Y, X, 138)];
case OP_CALL_R_R_N:
{
	int rm = xxux(0);
	int rf = xuxx(0);

	q1 = get_reg(rm);	// mod
	q2 = get_reg(rf);	// fun
	tmp1 = uxxx(0);		// arity
	tmp2 = 1;			// op size

	goto op_call;
}

//asm({'call',{l,X}}) -> [?xxxu(161), {l,X}];
case OP_CALL_L:
{
	// proc_secure_stack_space allocates a space for empty frame_t too
	// no need to recheck here

	my_frame = (frame_t *)proc->stack_node->first_avail;
	my_frame->saved_ip = ip + 2;
	my_frame->saved_module = module;
	my_frame->saved_nslots = nslots;
	my_frame->saved_heap_needed = heap_needed;
	proc->stack_node->first_avail += sizeof(frame_t);

	jump(1);
}

//asm({'call_fun',X,{r,Y}}) when X =< 255, X >= 0 -> [?xuuu(Y, X, 162)];
case OP_CALL_FUN_N_R:
{
	int arity = xxux(0);
	int r = xuxx(0);
	term_t f = get_reg(r);
	term_box_t *fb;
	int nfree;
	fun_slot_t *slot;
	int i;
	term_t cons;

	if (!is_fun(f))
		bad_arg(f);
	fb = peel(f);
	nfree = list_length(fb->fun.frozen);

	if (arity + nfree != fb->fun.arity)
		bad_arg(f);

	if (module->key.is_old || fb->fun.module != module->key.module)
	{
		module = code_base_lookup(proc->teevm->base, fb->fun.module);
		if (module == 0)
			not_implemented("undefined_fun");
	}

	slot = &module->funs[fb->fun.index];

	if (slot->uniq != fb->fun.uniq)
		exception(SLICE_RESULT_ERROR, A_STALE_FUN);

	i = arity;
	cons = fb->fun.frozen;

	while(is_cons(cons))
	{
		term_box_t *cb = peel(cons);
		term_t v = cb->cons.head;
		set_reg(i, v);
		i++;
		cons = cb->cons.tail;
	}

	// proc_secure_stack_space allocates a space for empty frame_t too
	// no need to recheck here

	my_frame = (frame_t *)proc->stack_node->first_avail;
	my_frame->saved_ip = ip + 1;		// call_fun is shorter than call l
	my_frame->saved_module = module;
	my_frame->saved_nslots = nslots;
	my_frame->saved_heap_needed = heap_needed;
	proc->stack_node->first_avail += sizeof(frame_t);

	ip = slot->entry;
	goto dispatch;
}

//asm({'call_bif',X,{bif,Y}}) when X =< 255, X >= 0 -> [?xxuu(X, 163), {bif,Y}];
case OP_CALL_BIF_N_B:
{
	int n = xxux(0);
	bifN_t entry = ip[1].bif;
	switch (n)
	{
	case 0: set_reg(0, ((bif0_t)entry)(proc)); break;
	case 1: set_reg(0, ((bif1_t)entry)(get_reg(0), proc)); break;
	case 2: set_reg(0, ((bif2_t)entry)(get_reg(0), get_reg(1), proc)); break;
	case 3: set_reg(0, ((bif3_t)entry)(get_reg(0), get_reg(1), get_reg(2), proc)); break;
	case 4: set_reg(0, ((bif4_t)entry)(get_reg(0), get_reg(1), get_reg(2), get_reg(3), proc)); break;
	case 5: set_reg(0, ((bif5_t)entry)(get_reg(0), get_reg(1), get_reg(2), get_reg(3), get_reg(4), proc)); break;
	case 6: set_reg(0, ((bif6_t)entry)(get_reg(0), get_reg(1), get_reg(2), get_reg(3), get_reg(4), get_reg(5), proc)); break;
	case 7: set_reg(0, ((bif7_t)entry)(get_reg(0), get_reg(1), get_reg(2), get_reg(3), get_reg(4), get_reg(5), get_reg(6), proc)); break;
	case 8: set_reg(0, ((bif8_t)entry)(get_reg(0), get_reg(1), get_reg(2), get_reg(3), get_reg(4), get_reg(5), get_reg(6), get_reg(7), proc)); break;
	}

	if (get_reg(0) == noval)
		exception0();		//result is filled in

	next(2);
}

//asm('call_apply') -> [?xxxu(155)];
case OP_CALL_APPLY:
{
	term_t m = get_reg(0);
	term_t f = get_reg(1);
	term_t a = get_reg(2);

	my_frame = (frame_t *)proc->stack_node->first_avail;
	my_frame->saved_ip = ip + 1;
	my_frame->saved_module = module;
	my_frame->saved_nslots = nslots;
	my_frame->saved_heap_needed = heap_needed;
	proc->stack_node->first_avail += sizeof(frame_t);

	module = code_base_lookup(proc->teevm->base, m);
	if (module == 0)
	{
		module = code_base_lookup(proc->teevm->base, A_ERROR_HANDLER);
		ip = module_lookup(module, A_UNDEFINED_FUNCTION, 3);

		set_reg(0, m);
		set_reg(1, f);
		set_reg(2, a);
	}
	else
	{
		int i = 0;
		int arity = list_length(a);
		ip = module_lookup(module, f, arity);
		if (ip == 0)
		{
			term_t reason = heap_tuple4(proc->heap, A_UNDEF, m, f, tag_int(arity));
			exception(SLICE_RESULT_ERROR, reason);
		}

		while (is_cons(a))
		{
			term_box_t *cb = peel(a);
			term_t v = cb->cons.head;
			set_reg(i, v);
			i++;
			a = cb->cons.tail;
		}
	}

	goto dispatch;
}

//asm('return') -> [?xxxu(164)];
case OP_RETURN:
{
	char *waterline;

return_now:		// from enter_bif

	// tell heap how much we consumed and how much
	// we have to spare
	heap_reclaim_unused(proc->heap, heap_needed);

	ip = my_frame->saved_ip;
	module = my_frame->saved_module;
	nslots = my_frame->saved_nslots;
	heap_needed = my_frame->saved_heap_needed;

	waterline = (char *)my_frame;
	my_frame = (frame_t *)((char *)my_frame
		- sizeof(term_t)*nslots
		- sizeof(frame_t));
	proc->stack_node->first_avail = waterline;

	if (ip == 0)
	{
		proc->result.what = SLICE_RESULT_DONE;
		proc->result.reason = A_NORMAL;

		proc->capsule.registers[0] = get_reg(0);
		goto schedule;
	}

	goto dispatch;
}

//asm({'make_fun',{r,X},Y,{literal,Z},{literal,P},{r,Q}}) when Y =< 255, Y >= 0 -> [?uuuu(Q, Y, X, 152), {literal,Z}, {literal,P}];
case OP_MAKE_FUN_R_N_T_T_R:
{
	int rc = xxux(0);
	int arity = xuxx(0);
	int rf = uxxx(0);
	term_t t1 = ip[1].t;
	term_t t2 = ip[2].t;
	uint index;
	uint uniq;
	term_t tmp;
	term_box_t *htop;

	if (!is_int(t1))
		bad_arg(t1);
	index = int_value(t1);
	if (is_int(t2))
		uniq = (uint)int_value(t2);
	else if (is_bignum(t2))
	{
		mp_int mp = bignum_to_mp(t2);
		uniq = (uint)mp_get_int(&mp);
	}
	else
		bad_arg(t2);

	htop = (term_box_t *)heap_htop(proc->heap);
	htop->fun.module = module->key.module;
	htop->fun.function = noval;
	htop->fun.arity = arity;
	htop->fun.uniq = uniq;
	htop->fun.index = index;
	htop->fun.frozen = get_reg(rf);
	tmp = tag_fun(htop);
	heap_htop(proc->heap) += FUN_T_SIZE;
	heap_needed -= FUN_T_SIZE;
	set_reg(rc, tmp);
	next(3);
}

//asm({'make_fun_nil',{r,X},Y,{literal,Z},{literal,P}}) when Y =< 255, Y >= 0 -> [?xuuu(Y, X, 153), {literal,Z}, {literal,P}];
case OP_MAKE_FUN_NIL_R_N_T_T:
{
	int rc = xxux(0);
	int arity = xuxx(0);
	term_t t1 = ip[1].t;
	term_t t2 = ip[2].t;
	uint index;
	uint uniq;
	term_t tmp;
	term_box_t *htop;

	if (!is_int(t1))
		bad_arg(t1);
	index = int_value(t1);
	if (is_int(t2))
		uniq = (uint)int_value(t2);
	else if (is_bignum(t2))
	{
		mp_int mp = bignum_to_mp(t2);
		uniq = (uint)mp_get_int(&mp);
	}
	else
		bad_arg(t2);

	htop = (term_box_t *)heap_htop(proc->heap);
	htop->fun.module = module->key.module;
	htop->fun.function = noval;
	htop->fun.arity = arity;
	htop->fun.uniq = uniq;
	htop->fun.index = index;
	htop->fun.frozen = nil;
	tmp = tag_fun(htop);
	heap_htop(proc->heap) += FUN_T_SIZE;
	heap_needed -= FUN_T_SIZE;
	set_reg(rc, tmp);
	next(3);
}

//asm({'match_fail',{atom,X},{r,Y}}) -> [?xuuu(Y, X, 166)];
case OP_MATCH_FAIL_A_R:
{
	int r = xxux(0);
	int a = xuxx(0);
	term_t reason = heap_tuple2(proc->heap, tag_atom(a), get_reg(r));
	exception(SLICE_RESULT_ERROR, reason);
}

//asm({'match_fail',{literal,X},{r,Y}}) -> [?xxuu(Y, 167), {literal,X}];
case OP_MATCH_FAIL_T_R:
{
	int r = xxux(0);
	int t = ip[1].t;
	term_t reason = heap_tuple2(proc->heap, t, get_reg(r));
	exception(SLICE_RESULT_ERROR, reason);
}

//asm({'match_fail',{atom,X}}) -> [?xxuu(X, 168)];
case OP_MATCH_FAIL_A:
{
	int a = xxux(0);
	term_t reason = tag_atom(a);
	exception(SLICE_RESULT_ERROR, reason);
}

//asm({'match_fail',{literal,X}}) -> [?xxxu(169), {literal,X}];
case OP_MATCH_FAIL_T:
{
	term_t reason = ip[1].t;
	exception(SLICE_RESULT_ERROR, reason);
}

//asm({'consup',{r,X},{r,Y},{r,Z}}) -> [?uuuu(Z, Y, X, 173)];
case OP_CONSUP_R_R_R:
{
	int rc = xxux(0);
	int ra = xuxx(0);
	int rb = uxxx(0);
	term_t tmp;
	term_box_t *htop = (term_box_t *)heap_htop(proc->heap);
	htop->cons.head = get_reg(ra);
	htop->cons.tail = get_reg(rb);
	tmp = tag_list(htop);
	heap_htop(proc->heap) += CONS_T_SIZE;
	heap_needed -= CONS_T_SIZE;
	set_reg(rc, tmp);
	next(1);
}

//asm({'consup',{r,X},{atom,Y},{r,Z}}) -> [?uuuu(Z, Y, X, 174)];
case OP_CONSUP_R_A_R:
{
	int rc = xxux(0);
	int index = xuxx(0);
	int rb = uxxx(0);
	term_t tmp;
	term_box_t *htop = (term_box_t *)heap_htop(proc->heap);
	htop->cons.head = tag_atom(index);
	htop->cons.tail = get_reg(rb);
	tmp = tag_list(htop);
	heap_htop(proc->heap) += CONS_T_SIZE;
	heap_needed -= CONS_T_SIZE;
	set_reg(rc, tmp);
	next(1);
}

//asm({'consup',{r,X},{integer,Y},{r,Z}}) when Y =< 127, Y >= -128 -> [?uuuu(Z, Y, X, 175)];
case OP_CONSUP_R_I1_R:
{
	int rc = xxux(0);
	int i = xUxx(0);
	int rb = uxxx(0);
	term_t tmp;
	term_box_t *htop = (term_box_t *)heap_htop(proc->heap);
	htop->cons.head = tag_int(i);
	htop->cons.tail = get_reg(rb);
	tmp = tag_list(htop);
	heap_htop(proc->heap) += CONS_T_SIZE;
	heap_needed -= CONS_T_SIZE;
	set_reg(rc, tmp);
	next(1);
}

//asm({'consup',{r,X},{literal,Y},{r,Z}}) -> [?xuuu(Z, X, 178), {literal,Y}];
case OP_CONSUP_R_T_R:
{
	int rc = xxux(0);
	int rb = xuxx(0);
	term_t tmp;
	term_box_t *htop;
	htop = (term_box_t *)heap_htop(proc->heap);
	htop->cons.head = ip[1].t;
	htop->cons.tail = get_reg(rb);
	tmp = tag_list(htop);
	heap_htop(proc->heap) += CONS_T_SIZE;
	heap_needed -= CONS_T_SIZE;
	set_reg(rc, tmp);
	next(2);
}

//asm({'consup',{r,X},{r,Y},{literal,Z}}) -> [?xuuu(Y, X, 179), {literal,Z}];
case OP_CONSUP_R_R_T:
{
	int rc = xxux(0);
	int ra = xuxx(0);
	term_t tmp;
	term_box_t *htop;
	htop = (term_box_t *)heap_htop(proc->heap);
	htop->cons.head = get_reg(ra);
	htop->cons.tail = ip[1].t;
	tmp = tag_list(htop);
	heap_htop(proc->heap) += CONS_T_SIZE;
	heap_needed -= CONS_T_SIZE;
	set_reg(rc, tmp);
	next(2);
}

//asm({'nil_consup',{r,X},{r,Y}}) -> [?xuuu(Y, X, 179)];
case OP_NIL_CONSUP_R_R:
{
	int rc = xxux(0);
	int rb = xuxx(0);
	term_t tmp;
	term_box_t *htop = (term_box_t *)heap_htop(proc->heap);
	htop->cons.head = nil;
	htop->cons.tail = get_reg(rb);
	tmp = tag_list(htop);
	heap_htop(proc->heap) += CONS_T_SIZE;
	heap_needed -= CONS_T_SIZE;
	set_reg(rc, tmp);
	next(1);
}

//asm({'consup_nil',{r,X},{r,Y}}) -> [?xuuu(Y, X, 177)];
case OP_CONSUP_NIL_R_R:
{
	int rc = xxux(0);
	int ra = xuxx(0);
	term_t tmp;
	term_box_t *htop = (term_box_t *)heap_htop(proc->heap);
	htop->cons.head = get_reg(ra);
	htop->cons.tail = nil;
	tmp = tag_list(htop);
	heap_htop(proc->heap) += CONS_T_SIZE;
	heap_needed -= CONS_T_SIZE;
	set_reg(rc, tmp);
	next(1);
}

//asm({'uncons',{r,X},{r,Y},{r,Z}}) -> [?uuuu(Z, Y, X, 178)];
case OP_UNCONS_R_R_R:
{
	int rc = xxux(0);
	int rh = xuxx(0);
	int rt = uxxx(0);
	term_box_t *box = peel(get_reg(rc));
	term_t h = box->cons.head;
	term_t t = box->cons.tail;
	set_reg(rh, h);
	set_reg(rt, t);
	next(1);
}

//asm({'hd',{r,X},{r,Y}}) -> [?xuuu(Y, X, 185)];
case OP_HD_R_R:
{
	int rh = xxux(0);
	int rc = xuxx(0);
	term_box_t *cb = peel(get_reg(rc));
	term_t hd = cb->cons.head;
	set_reg(rh, hd);
	next(1);
}

//asm({'tl',{r,X},{r,Y}}) -> [?xuuu(Y, X, 186)];
case OP_TL_R_R:
{
	int rt = xxux(0);
	int rc = xuxx(0);
	term_box_t *cb = peel(get_reg(rc));
	term_t tl = cb->cons.tail;
	set_reg(rt, tl);
	next(1);
}

//asm({'list_len',{r,X},{r,Y}}) -> [?xuuu(Y, X, 179)];
case OP_LIST_LEN_R_R:
{
	int ra = xxux(0);
	int rb = xuxx(0);
	int len;
	term_t a;
	term_t b = get_reg(rb);
	if (!is_list(b))
		bad_arg(b);
	len = list_length(b);
	a = tag_int(len);
	set_reg(ra, a);
	next(1);
}

//asm({'nil',{r,X}}) -> [?xxuu(X, 180)];
case OP_NIL_R:
{
	int r = xxux(0);
	set_reg(r, nil);
	next(1);
}

//asm({'tuple',{r,X},Y}) when Y =< 65535, Y >= 0 -> [?dduu(Y, X, 181)];
case OP_TUPLE_R_U2:
{
	int r = xxux(0);
	int arity = ddxx(0);
	term_t tmp;
	term_box_t *htop = (term_box_t *)heap_htop(proc->heap);
	htop->tuple.size = arity;
	tmp = tag_tuple(htop);
	heap_htop(proc->heap) += TUPLE_T_SIZE(arity);
	heap_needed -= TUPLE_T_SIZE(arity);
	set_reg(r, tmp);
	next(1);
}

//asm({'tuple',{r,X},{literal,Y}}) -> [?xxuu(X, 171), {literal,Y}];
case OP_TUPLE_R_T:
{
	int r = xxux(0);
	term_t t = ip[1].t;
	int arity;
	term_t tmp;
	term_box_t *htop;

	if (is_int(t))
		arity = (uint)int_value(t);
	else if (is_bignum(t))
	{
		mp_int mp = bignum_to_mp(t);
		arity = mp_get_int(&mp);
	}
	else
		bad_arg(t);

	htop = (term_box_t *)heap_htop(proc->heap);
	htop->tuple.size = arity;
	tmp = tag_tuple(htop);
	heap_htop(proc->heap) += TUPLE_T_SIZE(arity);
	heap_needed -= TUPLE_T_SIZE(arity);
	set_reg(r, tmp);
	next(1);
}

//asm({'ntuple',{r,X},{literal,Y}}) -> [?xxuu(X, 173), {literal,Y}];
case OP_NTUPLE_R_T:
{
	int r = xxux(0);
	term_t name = ip[1].t;
	int arity = named_tuples_arity(proc->teevm->nm_tuples, name);
	term_t tmp = heap_tuple(proc->heap, arity);
	term_box_t *tb = peel(tmp);
	int i;
	tb->tuple.elts[0] = name;
	for (i = 1; i < arity; i++)
		tb->tuple.elts[i] = A_UNDEFINED;
	set_reg(r, tmp);
	next(2);
}

//asm({'tuple_size',{r,X},Y,{l,Z}}) when Y =< 65535, Y >= 0 -> [?dduu(Y, X, 183), {l,Z}];
case OP_TUPLE_SIZE_R_U2_L:
{
	int r = xxux(0);
	int arity = ddxx(0);
	term_t tuple = get_reg(r);
	if (peel(tuple)->tuple.size == arity)
		next(2);
	else
		jump(1);
}

//asm({'tuple_size',{r,X},{literal,Y},{l,Z}}) -> [?xxuu(X, 173), {literal,Y}, {l,Z}];
case OP_TUPLE_SIZE_R_T_L:
{
	int r = xxux(0);
	term_t t = ip[1].t;
	int arity;
	term_t tuple = get_reg(r);

	if (is_int(t))
		arity = (uint)int_value(t);
	else if (is_bignum(t))
	{
		mp_int mp = bignum_to_mp(t);
		arity = mp_get_int(&mp);
	}
	else
		bad_arg(t);

	if (peel(tuple)->tuple.size == arity)
		next(3);
	else
		jump(2);
}

//asm({'tuple_size',{r,X},{r,Y}}) -> [?xuuu(Y, X, 175)];
case OP_TUPLE_SIZE_R_R:
{
	int rr = xxux(0);
	int rt = xuxx(0);
	term_t tuple = get_reg(rt);
	int arity;
	term_t v;
	if (!is_tuple(tuple))
		bad_arg(tuple);
	arity = peel(tuple)->tuple.size;
	v = tag_int(arity);
	set_reg(rr, v);
	next(1);
}

//asm({'dsetel',{r,X},{literal,Y},{r,Z}}) when Y =< 255, Y >= 0 -> [?uuuu(Z, Y, X, 175)];
case OP_DSETEL_R_U1_R:
{
	int rt = xxux(0);
	int index = xuxx(0);
	int re = uxxx(0);
	term_t tuple = get_reg(rt);
	peel(tuple)->tuple.elts[index] = get_reg(re);
	next(1);
}

//asm({'dsetel',{r,X},{literal,Y},{r,Z}}) -> [?xuuu(Z, X, 176), {literal,Y}];
case OP_DSETEL_R_T_R:
{
	int rt = xxux(0);
	int re = xuxx(0);
	term_t a = ip[1].t;
	int index;
	term_t tuple = get_reg(rt);
	if (!is_int(a))
		bad_arg(a);
	index = int_value(a);
	peel(tuple)->tuple.elts[index] = get_reg(re);
	next(2);
}

//asm({'dsetel',{r,X},{literal,Y},{literal,Z}}) when Y =< 255, Y >= 0, Z =< 127, Z >= -128 -> [?uuuu(Z, Y, X, 177)];
case OP_DSETEL_R_U1_I1:
{
	int rt = xxux(0);
	int index = xuxx(0);
	int i = Uxxx(0);
	term_t tuple = get_reg(rt);
	peel(tuple)->tuple.elts[index] = tag_int(i);
	next(1);
}

//asm({'dsetel',{r,X},{literal,Y},{atom,Z}}) when Y =< 255, Y >= 0 -> [?uuuu(Z, Y, X, 178)];
case OP_DSETEL_R_U1_A:
{
	int rt = xxux(0);
	int index = xuxx(0);
	int a = uxxx(0);
	term_t tuple = get_reg(rt);
	peel(tuple)->tuple.elts[index] = tag_atom(a);
	next(1);
}

//asm({'dsetel',{r,X},{literal,Y},{literal,Z}}) -> [?xxuu(X, 179), {literal,Y}, {literal,Z}];
case OP_DSETEL_R_T_T:
{
	int rt = xxux(0);
	int index;
	term_t t1 = ip[1].t;
	term_t t2 = ip[2].t;
	term_t tuple = get_reg(rt);
	if (!is_int(t1))
		bad_arg(t1);
	index = int_value(t1);
	peel(tuple)->tuple.elts[index] = t2;
	next(3);
}

//asm({'dsetel_nil',{r,X},Y}) when Y =< 65535, Y >= 0 -> [?dduu(Y, X, 193)];
case OP_DSETEL_NIL_R_U2:
{
	int rt = xxux(0);
	int index = ddxx(0);
	term_t tuple = get_reg(rt);
	peel(tuple)->tuple.elts[index] = nil; //TODO: dsetel should index from 0
	next(1);
}

//asm({'dsetel_nil',{r,X},{literal,Y}}) -> [?xxuu(X, 181), {literal,Y}];
case OP_DSETEL_NIL_R_T:
{
	int rt = xxux(0);
	term_t t = ip[1].t;
	int index;
	term_t tuple = get_reg(rt);
	if (!is_int(t))
		bad_arg(t);
	index = int_value(t);
	peel(tuple)->tuple.elts[index] = nil;
	next(2);
}

//asm({'getel',{r,X},{r,Y},{r,Z}}) -> [?uuuu(Z, Y, X, 195)];
case OP_GETEL_R_R_R:
{
	int re = xxux(0);
	int rn = xuxx(0);
	int rt = uxxx(0);
	term_t tuple = get_reg(rt);
	term_t n = get_reg(rn);
	int index = int_value(n)-1;
	term_t e = peel(tuple)->tuple.elts[index];
	set_reg(re, e);
	next(1);
}

//asm({'getel',{r,X},{literal,Y},{r,Z}}) when Y =< 255, Y >= 0 -> [?uuuu(Z, Y, X, 183)];
case OP_GETEL_R_U1_R:
{
	int re = xxux(0);
	int index = xuxx(0);
	int rt = uxxx(0);
	term_t tuple = get_reg(rt);
	term_t e = peel(tuple)->tuple.elts[index];
	set_reg(re, e);
	next(1);
}

//asm({'getel',{r,X},Y,{r,Z}}) -> [?xuuu(Z, X, 197), Y];
case OP_GETEL_R_T_R:
{
	int re = xxux(0);
	int rt = xuxx(0);
	term_t t = ip[1].t;
	int index;
	term_t tuple = get_reg(rt);
	term_t e;
	if (!is_int(t))
		bad_arg(t);
	index = int_value(t);
	e = peel(tuple)->tuple.elts[index];
	set_reg(re, e);
	next(2);
}

//asm({'getel2',{r,X},{r,Y},{r,Z}}) -> [?uuuu(Z, Y, X, 195)];
case OP_GETEL2_R_R_R:
{
	int re = xxux(0);
	int rn = xuxx(0);
	int rt = uxxx(0);
	term_t tuple = get_reg(rt);
	term_t n = get_reg(rn);
	term_box_t *tb;
	int index;
	term_t e;

	if (!is_tuple(tuple))
		bad_arg(tuple);
	tb = peel(tuple);
	if (!is_int(n))
		bad_arg(n);
    index = int_value(n)-1;
	if (index < 0 || index >= tb->tuple.size)
		bad_arg(n);

	e = tb->tuple.elts[index];
	set_reg(re, e);
	next(1);
}

//asm({'gen_size',{r,X},{r,Y}}) -> [?xuuu(Y, X, 202)];
case OP_GEN_SIZE_R_R:
{
	int ra = xxux(0);
	int rb = xuxx(0);
	int sz;
	term_t a;
	term_t b = get_reg(rb);
	if (!is_tuple(b) && !is_binary(b))
		bad_arg(b);
	if (is_tuple(b))
		sz = peel(b)->tuple.size;
	else
		sz = BIN_BYTE_SIZE(peel(b)->binary.bit_size);
	a = tag_int(sz);
	set_reg(ra, a);
	next(1);
}

//asm('recv_reset_inf') -> [?xxxu(211)];
case OP_RECV_RESET_INF:
{
	assert(proc->ntimeouts < RECV_IN_RECV);
	proc->timeouts[proc->ntimeouts++] = INFINITY;	// TODO: check for overflows, also below
	msg_queue_reset(proc->mailbox);
	next(1);
}

//asm({'recv_reset',{r,X}}) -> [?xxuu(X, 212)];
case OP_RECV_RESET_R:
{
	int r = xxux(0);
	term_t t = get_reg(r);

	assert(proc->ntimeouts < RECV_IN_RECV);
	if (t == A_INFINITY)
		proc->timeouts[proc->ntimeouts++] = INFINITY;
	else
	{
		uint period_ms;
		if (!is_int(t) && !is_bignum(t))
			exception(SLICE_RESULT_ERROR,
				heap_tuple2(proc->heap, A_TIMEOUT_VALUE, t));
		if (is_int(t))
		{
			if (int_value(t) < 0)
				exception(SLICE_RESULT_ERROR,
					heap_tuple2(proc->heap, A_TIMEOUT_VALUE, t));
			period_ms = (uint)int_value(t);
		}
		else //bignum
		{
			mp_int mp = bignum_to_mp(t);
			if (mp_cmp_z(&mp) < 0)	// TODO: check upper limit
				exception(SLICE_RESULT_ERROR,
					heap_tuple2(proc->heap, A_TIMEOUT_VALUE, t));
			period_ms = mp_get_int(&mp);
		}

		if (period_ms == 0)	//special case, no wait
			proc->timeouts[proc->ntimeouts++] = TIMEOUT_ZERO;
		else
		{
			apr_time_t now = apr_time_now();
			proc->timeouts[proc->ntimeouts++] = now + period_ms * 1000;
		}
	}

	msg_queue_reset(proc->mailbox);
	next(1);
}

//asm({'recv_reset',X}) when X =< 65535, X >= 0 -> [?xddu(X, 213)];
case OP_RECV_RESET_U2:
{
	uint period_ms = xddx(0);

	assert(proc->ntimeouts < RECV_IN_RECV);
	if (period_ms == 0)	//special case, no wait
		proc->timeouts[proc->ntimeouts++] = TIMEOUT_ZERO;
	else
	{
		apr_time_t now = apr_time_now();
		proc->timeouts[proc->ntimeouts++] = now + period_ms * 1000;
	}

	msg_queue_reset(proc->mailbox);
	next(1);
}

//asm({'recv_reset',{literal,X}}) -> [?xxxu(190), {literal,X}];
case OP_RECV_RESET_T:
{
	term_t t = ip[1].t;
	uint period_ms;
	apr_time_t now;

	if (is_int(t))
		period_ms = (uint)int_value(t);
	else if (is_bignum(t))
	{
		mp_int mp = bignum_to_mp(t);
		period_ms = mp_get_int(&mp);
	}
	else
		bad_arg(t);

	// never zero; previous case would be in effect
	now = apr_time_now();
	assert(proc->ntimeouts < RECV_IN_RECV);
	proc->timeouts[proc->ntimeouts++] = now + period_ms * 1000;

	msg_queue_reset(proc->mailbox);
	next(2);
}

//asm({'recv_next',{r,X},Y,{l,Z}}) when Y =< 255, Y >= 0 -> [?xuuu(Y, X, 215), {l,Z}];
case OP_RECV_NEXT_R_N_L:
{
	term_t msg = msg_queue_next(proc->mailbox);

	if (msg != noval)
	{
		int r = xxux(0);
		set_reg(r, msg);
	}
	else
	{
		int i;
		apr_time_t now = apr_time_now();
		apr_time_t timeout = proc->timeouts[proc->ntimeouts-1];
		int nregs;

		if (timeout == TIMEOUT_ZERO ||
			(timeout != INFINITY && now >= timeout))
		{
			proc->ntimeouts--;
			jump(1);
		}

		nregs = xuxx(0);

		proc->capsule.module = module;
		proc->capsule.ip = ip;
		proc->capsule.arity = nregs;

		proc->capsule.my_frame = my_frame;
		proc->capsule.nslots = nslots;
		proc->capsule.heap_needed = heap_needed;

		for (i = 0; i < nregs; i++)
			proc->capsule.registers[i] = get_reg(i);

		proc->result.what = SLICE_RESULT_WAIT;
		proc->result.until_when = timeout;
		goto schedule;
	}

	next(2);
}

//asm({'recv_next',{r,X},Y}) when Y =< 255, Y >= 0 -> [?xuuu(Y, X, 216)];
case OP_RECV_NEXT_R_N:
{
	//TODO: use the code above and tmp1,tmp2,etc

	term_t msg = msg_queue_next(proc->mailbox);

	if (msg != noval)
	{
		int r = xxux(0);
		set_reg(r, msg);
	}
	else
	{
		int i, nregs;

		assert(proc->timeouts[proc->ntimeouts-1] == INFINITY);

		nregs = xuxx(0);

		proc->capsule.module = module;
		proc->capsule.ip = ip;
		proc->capsule.arity = nregs;

		proc->capsule.my_frame = my_frame;
		proc->capsule.nslots = nslots;
		proc->capsule.heap_needed = heap_needed;

		for (i = 0; i < nregs; i++)
			proc->capsule.registers[i] = get_reg(i);

		proc->result.what = SLICE_RESULT_WAIT;
		proc->result.until_when = INFINITY;
		goto schedule;
	}

	next(1);
}

//asm('recv_accept') -> [?xxxu(217)];
case OP_RECV_ACCEPT:
{
	proc->ntimeouts--;
	msg_queue_drop(proc->mailbox);
	next(1);
}

//asm({'self',{r,X}}) -> [?xxuu(X, 218)];
case OP_SELF_R:
{
	int r = xxux(0);
	term_t pid = proc_id(proc);
	set_reg(r, pid);
	next(1);
}

//asm({'node',{r,X}}) -> [?xxuu(X, 196)];
case OP_NODE_R:
{
	int r = xxux(0);
	term_t node = A_LOCAL;
	set_reg(r, node);
	next(1);
}

//asm({'node',{r,X},{r,Y}}) -> [?xuuu(Y, X, 197)];
case OP_NODE_R_R:
{
	int rr = xxux(0);
	int ra = xuxx(0);
	term_t id = get_reg(ra);
	term_t node;
	if (is_long_id(id))
	{
		term_box_t *ib = peel(id);
		node = ib->long_id.node;
	}
	else if (is_short_pid(id) || is_short_oid(id))
		node = A_LOCAL;
	else
		bad_arg(id);
	set_reg(rr, node);
	next(1);
}

//asm({'binary',{r,X},{r,Y}}) -> [?xuuu(Y, X, 219)];
case OP_BINARY_R_R:
{
	int rb = xxux(0);
	int rn = xuxx(0);
	term_t bs = get_reg(rn);
	int bit_size;
	term_t bin;

	if (!is_int(bs))
		bad_arg(bs);	// TODO: longer binaries

	bit_size = int_value(bs);
	bin = heap_binary0(proc->heap, bit_size);
	set_reg(rb, bin);
	bspl_off = 0;
	next(1);
}

//asm({'binary',{r,X},Y}) when Y =< 65535, Y >= 0 -> [?dduu(Y, X, 219)];
case OP_BINARY_R_U2:
{
	int rb = xxux(0);
	int bit_size = ddxx(0);
	term_t bin = heap_binary0(proc->heap, bit_size);
	set_reg(rb, bin);
	bspl_off = 0;
	next(1);
}

//asm({'binary',{r,X},{literal,Y}}) -> [?xxuu(X, 197), {literal,Y}];
case OP_BINARY_R_T:
{
	int rb = xxux(0);
	term_t t = ip[1].t;
	int bit_size;
	term_t bin;

	if (is_int(t))
		bit_size = (uint)int_value(t);
	else if (is_bignum(t))
	{
		mp_int mp = bignum_to_mp(t);
		bit_size = mp_get_int(&mp);
	}
	else
		bad_arg(t);

	bin = heap_binary0(proc->heap, bit_size);
	set_reg(rb, bin);
	bspl_off = 0;
	next(2);
}

//asm({'add_mult',{r,X},{r,Y},Z}) when Z =< 255, Z >= 0 -> [?uuuu(Z, Y, X, 220)];
case OP_ADD_MULT_R_R_N:
{
	exception(SLICE_RESULT_ERROR, A_TRUE);
	//not_implemented("add_mult r, r, n");
}

//asm({'add_bit_size',{r,X},{r,Y}}) -> [?xuuu(Y, X, 221)];
case OP_ADD_BIT_SIZE_R_R:
{
	not_implemented("add_bit_size r, r");
}

//asm({'bit_size',{r,X},{r,Y}}) -> [?xuuu(Y, X, 201)];
case OP_BIT_SIZE_R_R:
{
	int rr = xxux(0);
	int rb = xuxx(0);
	term_t bin = get_reg(rb);
	int bit_size;
	term_t v;
	if (!is_binary(bin))
		bad_arg(bin);
	bit_size = peel(bin)->binary.bit_size;
	v = tag_int(bit_size);
	set_reg(rr, v);
	next(1);
}

//asm({'byte_size',{r,X},{r,Y}}) -> [?xuuu(Y, X, 202)];
case OP_BYTE_SIZE_R_R:
{
	int rr = xxux(0);
	int rb = xuxx(0);
	term_t bin = get_reg(rb);
	int bit_size;
	term_t v;
	if (!is_binary(bin))
		bad_arg(bin);
	bit_size = peel(bin)->binary.bit_size;
	v = tag_int(BIN_BYTE_SIZE(bit_size));
	set_reg(rr, v);
	next(1);
}

//asm({'bspl_i',{r,X},{r,Y},{r,Z},P,Q}) when P =< 255, P >= 0, Q =< 255, Q >= 0 -> [?uuuu(Z, Y, X, 202), ?xxuu(Q, P)];
case OP_BSPL_I_R_R_R_N_N:
{
	not_implemented("bspl_i r, r, r, n, n");
}

//asm({'bspl_i',{r,X},{integer,Y},{r,Z},P,Q}) when Y =< 32767, Y >= -32768, P =< 255, P >= 0, Q =< 255, Q >= 0 -> [?dduu(Y, X, 203), ?xuuu(Q, P, Z)];
case OP_BSPL_I_R_I2_R_N_N:
{
	int rb = xxux(0);
	term_t bin = get_reg(rb);
	term_box_t *tb = peel(bin);
	int i = DDxx(0);
	int rsz = xxxu(1);
	term_t sz = get_reg(rsz);
	uint unit = xxux(1)+1;
	int opts = uxxx(1);
	int nbits, off;
	apr_byte_t buf[INT_BYTES];

	if (opts != 0)
		not_implemented("bspl_i for signed or little-endian");

	if (!is_int(sz))
		bad_arg(sz);

	nbits = int_value(sz)*unit;
	int_to_bytes_msb(buf, i);
	off = (INT_BITS - nbits) >> 3;
	splice_bits(tb->binary.data, bspl_off, buf+off, nbits);
	bspl_off += nbits;
	next(2);
}

//asm({'bspl_i',{r,X},{literal,Y},{r,Z},P,Q}) when P =< 255, P >= 0, Q =< 255, Q >= 0 -> [?uuuu(P, Z, X, 204), ?xxxu(Q), {literal,Y}];
case OP_BSPL_I_R_T_R_N_N:
{
	not_implemented("bspl_i r, t, r, n, n");
}

//asm({'bspl_i',{r,X},{r,Y},{literal,Z},P}) when P =< 255, P >= 0 -> [?uuuu(P, Y, X, 205), {literal,Z}];
case OP_BSPL_I_R_R_T_N:
{
	not_implemented("bspl_i r, r, t, n");
}

//asm({'bspl_i',{r,X},{integer,Y},{literal,Z},P}) when Y =< 127, Y >= -128, P =< 255, P >= 0 -> [?uuuu(P, Y, X, 206), {literal,Z}];
case OP_BSPL_I_R_I1_T_N:
{
	not_implemented("bspl_i r, i1, t, n");
}

//asm({'bspl_i',{r,X},{literal,Y},{literal,Z},P}) when Z =< 255, Z >= 0, P =< 255, P >= 0 -> [?uuuu(P, Z, X, 207), {literal,Y}];
case OP_BSPL_I_R_T_U1_N:
{
	not_implemented("bspl_i r, t, u1, n");
}

//asm({'bspl_i',{r,X},{integer,Y},Z,P}) when Y =< 32767, Y >= -32768, Z =< 65535, Z >= 0, P =< 255, P >= 0 -> [?dduu(Y, X, 208), ?xudd(P, Z)];
case OP_BSPL_I_R_I2_U2_N:
{
	not_implemented("bspl_i r, i2, u2, n");
}

//asm({'bspl_i',{r,X},{literal,Y},{literal,Z},P}) when P =< 255, P >= 0 -> [?xuuu(P, X, 209), {literal,Y}, {literal,Z}];
case OP_BSPL_I_R_T_T_N:
{
	not_implemented("bspl_i r, t, t, n");
}

//asm({'bspl_f',{r,X},{r,Y},{r,Z},P,Q}) when P =< 255, P >= 0, Q =< 255, Q >= 0 -> [?uuuu(Z, Y, X, 210), ?xxuu(Q, P)];
case OP_BSPL_F_R_R_R_N_N:
{
	not_implemented("bspl_f r, r, r, n, n");
}

//asm({'bspl_f',{r,X},{literal,Y},{r,Z},P,Q}) when P =< 255, P >= 0, Q =< 255, Q >= 0 -> [?uuuu(P, Z, X, 211), ?xxxu(Q), {literal,Y}];
case OP_BSPL_F_R_T_R_N_N:
{
	not_implemented("bspl_f r, t, r, n, n");
}

//asm({'bspl_f',{r,X},{r,Y},{literal,Z},P}) when P =< 255, P >= 0 -> [?uuuu(P, Y, X, 212), {literal,Z}];
case OP_BSPL_F_R_R_T_N:
{
	not_implemented("bspl_f r, r, t, n");
}

//asm({'bspl_f',{r,X},{literal,Y},{literal,Z},P}) when Z =< 255, Z >= 0, P =< 255, P >= 0 -> [?uuuu(P, Z, X, 213), {literal,Y}];
case OP_BSPL_F_R_T_U1_N:
{
	not_implemented("bspl_f r, t, u1, n");
}

//asm({'bspl_f',{r,X},{literal,Y},{literal,Z},P}) when P =< 255, P >= 0 -> [?xuuu(P, X, 214), {literal,Y}, {literal,Z}];
case OP_BSPL_F_R_T_T_N:
{
	not_implemented("bspl_f r, t, t, n");
}

//asm({'bspl_b',{r,X},{r,Y},{r,Z},P,Q}) when P =< 255, P >= 0, Q =< 255, Q >= 0 -> [?uuuu(Z, Y, X, 233), ?xxuu(Q, P)];
case OP_BSPL_B_R_R_R_N_N:
{
	not_implemented("bspl_b r, r, r, n, n");
}

//asm({'bspl_b',{r,X},{literal,Y},{r,Z},P,Q}) when P =< 255, P >= 0, Q =< 255, Q >= 0 -> [?uuuu(P, Z, X, 234), ?xxxu(Q), {literal,Y}];
case OP_BSPL_B_R_T_R_N_N:
{
	not_implemented("bspl_b r, t, r, n, n");
}

//asm({'bspl_b',{r,X},{r,Y},{literal,Z},P}) when P =< 255, P >= 0 -> [?uuuu(P, Y, X, 217), {literal,Z}];
case OP_BSPL_B_R_R_T_N:
{
	not_implemented("bspl_b r, r, t, n");
}

//asm({'bspl_b',{r,X},{literal,Y},{literal,Z},P}) when Z =< 255, Z >= 0, P =< 255, P >= 0 -> [?uuuu(P, Z, X, 218), {literal,Y}];
case OP_BSPL_B_R_T_U1_N:
{
	not_implemented("bspl_b r, t, u1, n");
}

//asm({'bspl_b',{r,X},{literal,Y},{literal,Z},P}) when P =< 255, P >= 0 -> [?xuuu(P, X, 219), {literal,Y}, {literal,Z}];
case OP_BSPL_B_R_T_T_N:
{
	not_implemented("bspl_b r, t, t, n");
}

//asm({'bspl_b_all',{r,X},{r,Y},Z}) when Z =< 255, Z >= 0 -> [?uuuu(Z, Y, X, 238)];
case OP_BSPL_B_ALL_R_R_N:
{
	not_implemented("bspl_b_all r, r, n");
}

//bchip_i r/chip, r/rest, r/binary, r/size, n/unit, n/options, l/too_short
//asm({'bchip_i',{r,X},{r,Y},{r,Z},{r,P},Q,R,{l,T}})
//	when Q =< 255, Q >= 0, R =< 255, R >= 0 -> [?uuuu(Z, Y, X, 221), ?xuuu(R, Q, P), {l,T}];
case OP_BCHIP_I_R_R_R_R_N_N_L:
{
	not_implemented("bchip_i r, r, r, r, n, n, l");
}

//bchip_i r/chip, r/rest, r/binary, u2/size, n/options, l/too_short
//asm({'bchip_i',{r,X},{r,Y},{r,Z},P,Q,{l,R}})
//	when P =< 65535, P >= 0, Q =< 255, Q >= 0 -> [?uuuu(Z, Y, X, 219), ?xudd(Q, P), {l,R}];
case OP_BCHIP_I_R_R_R_U2_N_L:
{
	int rc = xxux(0);
	int rr = xuxx(0);
	int rb = uxxx(0);
	int nbits = xxdd(1);
	int opts = xuxx(1);
	int chip;
	term_t rest;

	term_t bin = get_reg(rb);
	term_box_t *bb = peel(bin);

	term_box_t *htop = (term_box_t *)heap_htop(proc->heap);

	if (bb->binary.bit_size < nbits)
		jump(2);

	if (opts != 0)
		not_implemented("bchip_i: signed or little-endian");

	if (nbits != 8)
		not_implemented("bchip_i: sizes other than 8");

	chip = bb->binary.data[0];
	set_reg(rc, tag_int(chip));		//XXX: macros arg

	htop->binary.bit_size = bb->binary.bit_size - nbits;
	htop->binary.data = bb->binary.data+1;
	htop->binary.parent = bin;
	htop->binary.offset = 1;
	rest = tag_binary(htop);

	heap_htop(proc->heap) += BINARY_T_SIZE;
	heap_needed -= BINARY_T_SIZE;

	set_reg(rr, rest);
	next(3);
}

//bchip_i r/chip, r/rest, r/binary, t/size, n/options, l/too_short
//asm({'bchip_i',{r,X},{r,Y},{r,Z},{literal,P},Q,{l,R}}) when Q =< 255, Q >= 0 ->
//		[?uuuu(Z, Y, X, 223), ?xxxu(Q), {literal,P}, {l,R}];
case OP_BCHIP_I_R_R_R_T_N_L:
{
	not_implemented("bchip_i r, r, r, t, n, l");
}

//bchip_i r/chip, r/rest, r/binary, r/size, n/unit, n/options, l/too_short
//asm({'bchip_f',{r,X},{r,Y},{r,Z},{r,P},Q,R,{l,T}}) when Q =< 255, Q >= 0, R =< 255, R >= 0 ->
//		[?uuuu(Z, Y, X, 224), ?xuuu(R, Q, P), {l,T}];
case OP_BCHIP_F_R_R_R_R_N_N_L:
{
	not_implemented("bchip_f r, r, r, r, n, n, l");
}

//asm({'bchip_f',{r,X},{r,Y},{r,Z},P,Q,{l,R}})
//	when P =< 65535, P >= 0, Q =< 255, Q >= 0 -> [?uuuu(Z, Y, X, 222), ?xudd(Q, P), {l,R}];
case OP_BCHIP_F_R_R_R_U2_N_L:
{
	not_implemented("bchip_f r, r, r, u2, n, l");
}

//asm({'bchip_f',{r,X},{r,Y},{r,Z},P,Q,{l,R}})
//	when Q =< 255, Q >= 0 -> [?uuuu(Z, Y, X, 223), ?xxxu(Q), P, {l,R}];
case OP_BCHIP_F_R_R_R_T_N_L:
{
	not_implemented("bchip_f r, r, r, t, n, l");
}

//asm({'bchip_b',{r,X},{r,Y},{r,Z},{r,P},Q,R,{l,T}})
//	when Q =< 255, Q >= 0, R =< 255, R >= 0 -> [?uuuu(Z, Y, X, 227), ?xuuu(R, Q, P), {l,T}];
//bchip_b r/chip, r/rest, r/binary, r/size, n/unit, n/options, l/too_short
case OP_BCHIP_B_R_R_R_R_N_N_L:
{
	int rc = xxux(0);
	int rr = xuxx(0);
	int rb = uxxx(0);
	int rz = xxxu(1);
	int unit = xxux(1);
	int opts = xuxx(1);

	term_t s = get_reg(rz);
	int bit_size;

	term_t bin = get_reg(rb);
	term_box_t *bb = peel(bin);

	if (!is_int(s))
		bad_arg(s);
	bit_size = int_value(s) * unit;

	if (opts != 0)
		not_implemented("binary chipping with special options");

	if (bb->binary.bit_size < bit_size)
		jump(2);
	else
	{
		term_t chip;
		term_t rest;
		apr_byte_t *data = bb->binary.data;

		if (BIN_TRAILER_SIZE(bit_size) > 0)
			not_implemented("oddball binary chipping");

		chip = heap_binary_shared(proc->heap,
			bit_size,
			data,
			bin);
		rest = heap_binary_shared(proc->heap,
			bb->binary.bit_size - bit_size,
			data + BIN_BYTE_SIZE(bit_size),
			bin);

		set_reg(rc, chip);
		set_reg(rr, rest);
		next(3);
	}
}

//bchip_b r/chip, r/rest, r/binary, u2/size, n/options, l/too_short
//asm({'bchip_b',{r,X},{r,Y},{r,Z},P,Q,{l,R}})
//	when P =< 65535, P >= 0, Q =< 255, Q >= 0 -> [?uuuu(Z, Y, X, 225), ?xudd(Q, P), {l,R}];
case OP_BCHIP_B_R_R_R_U2_N_L:
{
	int rc = xxux(0);
	int rr = xuxx(0);
	int rb = uxxx(0);
	int bit_size = xxdd(1);
	int opts = xuxx(1);
	int bin = get_reg(rb);
	term_box_t *bb = peel(bin);

	if (opts != 0)
		not_implemented("binary chipping with special options");

	if (bb->binary.bit_size < bit_size)
		jump(2);
	else
	{
		term_t chip;
		term_t rest;
		apr_byte_t *data = bb->binary.data;

		if (BIN_TRAILER_SIZE(bit_size) > 0)
			not_implemented("oddball binary chipping");

		chip = heap_binary_shared(proc->heap,
			bit_size,
			data,
			bin);
		rest = heap_binary_shared(proc->heap,
			bb->binary.bit_size - bit_size,
			data + BIN_BYTE_SIZE(bit_size),
			bin);

		set_reg(rc, chip);
		set_reg(rr, rest);
		next(3);
	}
}

//asm({'bchip_b',{r,X},{r,Y},{r,Z},{literal,P},Q,{l,R}})
//	when Q =< 255, Q >= 0 -> [?uuuu(Z, Y, X, 229), ?xxxu(Q), {literal,P}, {l,R}];
case OP_BCHIP_B_R_R_R_T_N_L:
{
	not_implemented("bchip_b r, r, r, t, n, l");
}

//asm({'bchip_b_all',{r,X},{r,Y},Z}) when Z =< 255, Z >= 0 -> [?uuuu(Z, Y, X, 228)];
case OP_BCHIP_B_ALL_R_R_N:
{
	//TODO: looks like nop for, standard options at least; correct codegen?
	int rc = xxux(0);
	int rb = xuxx(0);
	int opts = uxxx(0);
	term_t v;

	if (opts != 0)
		not_implemented("bchip_b_all with non-standard options");

	v = get_reg(rb);
	set_reg(rc, v);
	next(1);
}

//asm({'is_empty_binary',{r,X},{l,Y}}) -> [?xxuu(X, 228), {l,Y}];
case OP_IS_EMPTY_BINARY_R_L:
{
	int r = xxux(0);
	term_t bin = get_reg(r);
	term_box_t *bb = peel(bin);
	if (bb->binary.bit_size > 0)
		jump(1);
	next(2);
}

//asm({'catch',{l,X}}) -> [?xxxu(229), {l,X}];
case OP_CATCH_L:
{
	catch_t *cat;
	cat = (catch_t *)apr_array_push(proc->catches);
	// NB: offset from the beginning of the stack node
	// is saved as stack_node may be reallocated
	cat->frame_offset = (char *)my_frame - (char *)proc->stack_node - APR_MEMNODE_T_SIZE;
	cat->on_exception = ip[1].l;
	next(2);
}

//asm('drop_catch') -> [?xxxu(230)];
case OP_DROP_CATCH:
{
	apr_array_pop(proc->catches);
	next(1);
}

default:
	return noval;
}

schedule:
{
	int i;
	//proc_t *saved_proc = proc;
	statistics_t *stats;

	if ((proc->result.what == SLICE_RESULT_ERROR ||
		proc->result.what == SLICE_RESULT_THROW) &&
		proc->catches->nelts > 0)
	{
		catch_t *cat = (catch_t *)apr_array_pop(proc->catches);
		term_t t;
		if (proc->result.what == SLICE_RESULT_ERROR)
			t = heap_tuple2(proc->heap, AEXIT__, proc->result.reason);
		else // throw
			t = proc->result.reason;
		set_reg(0, t);

		// unwind call stack; taking care of heap_needed counters
		while ((char *)my_frame
			- (char *)proc->stack_node
			- APR_MEMNODE_T_SIZE > cat->frame_offset)
		{
			char *waterline;

			heap_reclaim_unused(proc->heap, heap_needed);

			module = my_frame->saved_module;
			// ip ignored
			nslots = my_frame->saved_nslots;
			heap_needed = my_frame->saved_heap_needed;

			waterline = (char *)my_frame;
			my_frame = (frame_t *)((char *)my_frame
				- sizeof(term_t)*nslots
				- sizeof(frame_t));
			proc->stack_node->first_avail = waterline;
		}

		ip = cat->on_exception;
		goto dispatch;
	}

	//stats update
	stats = scheduler_stats(proc->teevm->scheduler);
	//NB: assumes that reductions == SLICE_REDUCTIONS
	//		in the beginning of a slice
	stats->reductions += (SLICE_REDUCTIONS - reductions);
	stats->reductions_slc += (SLICE_REDUCTIONS - reductions);

	proc = scheduler_next(proc->teevm->scheduler, proc);
	if (proc == 0)
	{
		// TODO: nothing else to run, some processes
		// may be waiting on the queues, report them

		exit(0);
	}

	for (i = 0; i < proc->capsule.arity; i++)
	{
		term_t v = proc->capsule.registers[i];
		set_reg(i, v);
	}

	module = proc->capsule.module;
	ip = proc->capsule.ip;

	my_frame = proc->capsule.my_frame;
	nslots = proc->capsule.nslots;
	heap_needed = proc->capsule.heap_needed;

	reductions = SLICE_REDUCTIONS;
	goto dispatch;
}

}

void proc_secure_stack_space(proc_t *proc, int nslots)
{
	// insures that stack_node has enough space for n slots
	apr_memnode_t *node = proc->stack_node;
	int requested_space = nslots*sizeof(term_t) + sizeof(frame_t);  // for faster 'call'
	int node_space = node->endp - node->first_avail;

	if (node_space < requested_space)
	{
		apr_allocator_t *allocator = apr_pool_allocator_get(proc->proc_pool);

		char *node_base = (char *)node + APR_MEMNODE_T_SIZE;
		apr_size_t node_size = node->first_avail - node_base;
		apr_size_t bigger_size = node_size + requested_space;
		apr_memnode_t *bigger = apr_allocator_alloc(allocator, bigger_size);
		char *bigger_base = (char *)bigger + APR_MEMNODE_T_SIZE;

		memcpy(bigger_base, node_base, node_size);
		bigger->first_avail += node_size;

		proc->stack_node = bigger;
		apr_allocator_free(allocator, node);
	}
}

const char *term2html(term_t t, atoms_t *atoms, apr_pool_t *pool);

void report_and_exit(proc_t *proc)
{
	FILE *f = fopen("teeterl_dump.html", "w");
	fprintf(f, "<html>");
	fprintf(f, "<head><link rel=\"stylesheet\" type=\"text/css\" href=\"navel/style.css\" /></head>");
	fprintf(f, "<body><p>====proc_main exits====</p>");
	
	switch (proc->result.what)
	{
	case SLICE_RESULT_NONE:
		fprintf(f, "<p>SLICE_RESULT_NONE</p>");
		break;
	case SLICE_RESULT_YIELD:
		fprintf(f, "<p>SLICE_RESULT_YIELD</p>");
		break;
	case SLICE_RESULT_WAIT:
		fprintf(f, "<p>SLICE_RESULT_WAIT</p>");
		break;
	case SLICE_RESULT_DONE:
		fprintf(f, "<p>returns %s</p>",
			term2html(proc->capsule.registers[0], proc->teevm->atoms, proc->proc_pool));
		break;
	case SLICE_RESULT_EXIT:
		fprintf(f, "<p>exits with reason %s</p>",
			term2html(proc->result.reason, proc->teevm->atoms, proc->proc_pool));
		break;
	case SLICE_RESULT_ERROR:
		fprintf(f, "<p>fails with reason %s</p>",
			term2html(proc->result.reason, proc->teevm->atoms, proc->proc_pool));
		break;
	case SLICE_RESULT_THROW:
		fprintf(f, "<p>throws %s</p>",
			term2html(proc->result.reason, proc->teevm->atoms, proc->proc_pool));
		break;
	}

	fclose(f);
	fprintf(stderr, "====proc_main exits====\nteeterl_dump.html written\n");
	exit(1);
}

//EOF
