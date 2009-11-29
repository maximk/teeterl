/// addi(int i)
	term_t a = pop();

	if (!is_int(a) && !is_bignum(a) && !is_float(a))
		exception(A_ERROR, make_tuple2(A_BADARG, a, proc->gc_cur));

	if (is_float(a))
		push(make_float(dbl_value(a) + (double)i, proc->gc_cur));
	else if (is_bignum(a))
	{
		bignum_t *r = bignum_add1(bn_value(a), i, proc->gc_cur);
		push(bignum_to_term(r, proc->gc_cur));
	}
	else // two ints
	{
		int_value_t r = int_value(a) + i;
		if (r > MAX_INT_VALUE || r < MIN_INT_VALUE)
			push(bignum(bignum_from_int_value(r, proc->gc_cur)));
		else
			push(intnum(r));
	}

/// subi(int i)
	term_t a = pop();

	if (!is_int(a) && !is_bignum(a) && !is_float(a))
		exception(A_ERROR, make_tuple2(A_BADARG, a, proc->gc_cur));

	if (is_float(a))
		push(make_float(dbl_value(a) - (double)i, proc->gc_cur));
	else if (is_bignum(a))
	{
		bignum_t *r = bignum_add1(bn_value(a), -i, proc->gc_cur);
		push(bignum_to_term(r, proc->gc_cur));
	}
	else // two ints
	{
		int_value_t r = int_value(a) - i;
		if (r > MAX_INT_VALUE || r < MIN_INT_VALUE)
			push(bignum(bignum_from_int_value(r, proc->gc_cur)));
		else
			push(intnum(r));
	}

/// add
	term_t a, b;
	b = pop();
	a = pop();

	if (!is_int(a) && !is_bignum(a) && !is_float(a))
		exception(A_ERROR, make_tuple2(A_BADARG, a, proc->gc_cur));
	if (!is_int(b) && !is_bignum(b) && !is_float(b))
		exception(A_ERROR, make_tuple2(A_BADARG, b, proc->gc_cur));

	if (is_float(a) || is_float(b))
	{
		double fa, fb;
		if (is_int(a))
			fa = (double)int_value(a);
		else if (is_bignum(a))
			fa = bignum_to_double(bn_value(a));
		else //is_float
			fa = dbl_value(a);
		if (is_int(b))
			fb = (double)int_value(b);
		else if (is_bignum(b))
			fb = bignum_to_double(bn_value(b));
		else //is_float
			fb = dbl_value(b);
		push(make_float(fa + fb, proc->gc_cur));
	}
	else if (is_bignum(a) || is_bignum(b))
	{
		bignum_t *r;
		if (is_int(a))
			r = bignum_add2(bn_value(b), int_value(a), proc->gc_cur);
		else if (is_int(b))
			r = bignum_add2(bn_value(a), int_value(b), proc->gc_cur);
		else
			r = bignum_add(bn_value(a), bn_value(b), proc->gc_cur);
		push(bignum_to_term(r, proc->gc_cur));
	}
	else // two ints
	{
		int_value_t r = int_value(a) + int_value(b);
		if (r > MAX_INT_VALUE || r < MIN_INT_VALUE)
			push(bignum(bignum_from_int_value(r, proc->gc_cur)));
		else
			push(intnum(r));
	}

/// sub
	term_t a, b;
	b = pop();
	a = pop();

	if (!is_int(a) && !is_bignum(a) && !is_float(a))
		exception(A_ERROR, make_tuple2(A_BADARG, a, proc->gc_cur));
	if (!is_int(b) && !is_bignum(b) && !is_float(b))
		exception(A_ERROR, make_tuple2(A_BADARG, b, proc->gc_cur));

	if (is_float(a) || is_float(b))
	{
		double fa, fb;
		if (is_int(a))
			fa = (double)int_value(a);
		else if (is_bignum(a))
			fa = bignum_to_double(bn_value(a));
		else //is_float
			fa = dbl_value(a);
		if (is_int(b))
			fb = (double)int_value(b);
		else if (is_bignum(b))
			fb = bignum_to_double(bn_value(b));
		else //is_float
			fb = dbl_value(b);
		push(make_float(fa - fb, proc->gc_cur));
	}
	else if (is_bignum(a) || is_bignum(b))
	{
		bignum_t *r;
		if (is_int(a))
		{
			r = bignum_add2(bn_value(b), -int_value(a), proc->gc_cur);
			bn_negate(r);
		}
		else if (is_int(b))
			r = bignum_add2(bn_value(a), -int_value(b), proc->gc_cur);
		else
			r = bignum_sub(bn_value(a), bn_value(b), proc->gc_cur);
		push(bignum_to_term(r, proc->gc_cur));
	}
	else // two ints
	{
		int_value_t r = int_value(a) - int_value(b);
		if (r > MAX_INT_VALUE || r < MIN_INT_VALUE)
			push(bignum(bignum_from_int_value(r, proc->gc_cur)));
		else
			push(intnum(r));
	}

/// or
	term_t a, b;
	int ok;
	b = pop();
	a = pop();
	if (!is_bool(a) || !is_bool(b))
		bad_arg();
	ok = (a == A_TRUE || b == A_TRUE);
	push(bool(ok));

/// bor
	term_t a, b, r;
	b = pop();
	a = pop();
	if (!is_int(a) && !is_bignum(a))
		bad_arg();
	if (!is_int(b) && !is_bignum(b))
		bad_arg();
	if (is_int(a) && is_int(b))
		r = intnum(int_value(a) | int_value(b));
	else
	{
		bignum_t *b1, *b2, *b3;
		if (is_int(a))
			b1 = bignum_from_int_value(int_value(a), proc->gc_cur);
		else
			b1 = bn_value(a);
		if (is_int(b))
			b2 = bignum_from_int_value(int_value(b), proc->gc_cur);
		else
			b2 = bn_value(b);
		b3 = bignum_or(b1, b2, proc->gc_cur);
		r = bignum_to_term(b3, proc->gc_cur);
	}
	push(r);

/// xor
	term_t a, b;
	int ok;
	b = pop();
	a = pop();
	if (!is_bool(a) || !is_bool(b))
		bad_arg();
	ok = ((a == A_TRUE) != (b == A_TRUE));
	push(bool(ok));

/// bxor
	term_t a, b, r;
	b = pop();
	a = pop();
	if (!is_int(a) && !is_bignum(a))
		bad_arg();
	if (!is_int(b) && !is_bignum(b))
		bad_arg();
	if (is_int(a) && is_int(b))
		r = intnum(int_value(a) ^ int_value(b));
	else
	{
		bignum_t *b1, *b2, *b3;
		if (is_int(a))
			b1 = bignum_from_int_value(int_value(a), proc->gc_cur);
		else
			b1 = bn_value(a);
		if (is_int(b))
			b2 = bignum_from_int_value(int_value(b), proc->gc_cur);
		else
			b2 = bn_value(b);
		b3 = bignum_xor(b1, b2, proc->gc_cur);
		r = bignum_to_term(b3, proc->gc_cur);
	}
	push(r);

/// bsl
	// a bsl b
	term_t a, b, r;
	int shifts;
	b = pop();
	a = pop();

	if (!is_int(a) && !is_bignum(a))
		bad_arg();
	if (!is_int(b))
		bad_arg();

	shifts = int_value2(b);

	if (is_bignum(a) || shifts > 0)
	{
		bignum_t *b1, *b2;

		if (shifts >= 100000000)
			exception(A_ERROR, A_SYSTEM_LIMIT);

		if (is_bignum(a))
			b1 = bn_value(a);
		else
			b1 = bignum_from_int_value(int_value(a), proc->gc_cur);

		b2 = bignum_bsl(b1, shifts, proc->gc_cur);
		r = bignum_to_term(b2, proc->gc_cur);
	}
	else
	{
		// shifts < 0 -> bsr		
		//TODO: bignums?

		if (-shifts >= 32) 	// should it be 64 for 64-bit OSes?
			r = intnum((int_value(a) > 0) ?0 :-1);
		else
			r = intnum(int_value(a) >> -shifts);
	}
	push(r);

/// bsr
	// a bsr b
	term_t a, b, r;
	int shifts;
	b = pop();
	a = pop();

	if (!is_int(a) && !is_bignum(a))
		bad_arg();
	if (!is_int(b))
		bad_arg();

	shifts = int_value2(b);

	if (is_bignum(a) || shifts < 0)
	{
		bignum_t *b1, *b2;

		if (-shifts >= 100000000)
			bad_arg();

		if (is_bignum(a))
			b1 = bn_value(a);
		else
			b1 = bignum_from_int_value(int_value(a), proc->gc_cur);

		b2 = bignum_bsr(b1, shifts, proc->gc_cur);
		r = bignum_to_term(b2, proc->gc_cur);
	}
	else
	{
		// shifts > 0
		//TODO: bignums?

		if (shifts >= 32)	// should it be 64 for 64-bit OSes?
			r = intnum((int_value(a) > 0) ?0 :-1);
		else
			r = intnum(int_value(a) >> shifts);
	}
	push(r);

/// mult
	term_t a, b;
	b = pop();
	a = pop();

	if (!is_int(a) && !is_bignum(a) && !is_float(a))
		exception(A_ERROR, make_tuple2(A_BADARG, a, proc->gc_cur));
	if (!is_int(b) && !is_bignum(b) && !is_float(b))
		exception(A_ERROR, make_tuple2(A_BADARG, b, proc->gc_cur));

	if (is_float(a) || is_float(b))
	{
		double fa, fb;
		if (is_int(a))
			fa = (double)int_value(a);
		else if (is_bignum(a))
			fa = bignum_to_double(bn_value(a));
		else //is_float
			fa = dbl_value(a);
		if (is_int(b))
			fb = (double)int_value(b);
		else if (is_bignum(b))
			fb = bignum_to_double(bn_value(b));
		else //is_float
			fb = dbl_value(b);
		push(make_float(fa * fb, proc->gc_cur));
	}
	else if (is_bignum(a) || is_bignum(b))
	{
		bignum_t *r;
		if (is_int(a))
		{
			int_value_t v = int_value(a);
			if (v < 0)
			{
				r = bignum_mult2(bn_value(b), -v, proc->gc_cur);
				bn_negate(r);
			}
			else
				r = bignum_mult2(bn_value(b), v, proc->gc_cur);
		}
		else if (is_int(b))
		{
			int_value_t v = int_value(b);
			if (v < 0)
			{
				r = bignum_mult2(bn_value(a), -v, proc->gc_cur);
				bn_negate(r);
			}
			else
				r = bignum_mult2(bn_value(a), v, proc->gc_cur);
		}
		else
			r = bignum_mult(bn_value(a), bn_value(b), proc->gc_cur);
		push(bignum_to_term(r, proc->gc_cur));
	}
	else // two ints
	{
		int_value_t va = int_value(a);
		int_value_t vb = int_value(b);
		
		if (va <= 0x7fffffff && va >= -0x80000000 &&
			vb <= 0x7fffffff && vb >= -0x80000000)
		{
			apr_int64_t r = (apr_int64_t)va * vb;
			if (r <= MAX_INT_VALUE && r >= MIN_INT_VALUE)
				push(intnum(r));
			else
				push(bignum(bignum_from64(r, proc->gc_cur)));
		}
		else
		{
			bignum_t *r;
			bignum_t *t = bignum_from_int_value(va, proc->gc_cur);
			r = bignum_mult2(t, vb, proc->gc_cur);
			push(bignum_to_term(r, proc->gc_cur));
		}
	}

/// div
	term_t a, b;
	double fa, fb;
	b = pop();
	a = pop();

	if (!is_int(a) && !is_bignum(a) && !is_float(a))
		exception(A_ERROR, make_tuple2(A_BADARG, a, proc->gc_cur));
	if (!is_int(b) && !is_bignum(b) && !is_float(b))
		exception(A_ERROR, make_tuple2(A_BADARG, b, proc->gc_cur));

	if (is_int(a))
		fa = (double)int_value(a);
	else if (is_bignum(a))
		fa = bignum_to_double(bn_value(a));
	else
		fa = dbl_value(a);

	if (is_int(b))
		fb = (double)int_value(b);
	else if (is_bignum(a))
		fb = bignum_to_double(bn_value(b));
	else
		fb = dbl_value(b);

	push(make_float(fa / fb, proc->gc_cur));

/// idiv
	term_t a, b;
	int_value_t r;
	b = pop();
	a = pop();

	if (!is_int(a) && !is_bignum(a))
		bad_arg();
	if (!is_int(b) && !is_bignum(b))
		bad_arg();

	if (is_bignum(a) || is_bignum(b))
	{
		bignum_t *ba = (is_bignum(a))? bn_value(a): bignum_from_int_value(int_value(a), proc->gc_cur);
		bignum_t *bb = (is_bignum(b))? bn_value(b): bignum_from_int_value(int_value(b), proc->gc_cur);
		
		bignum_t *q = bignum_div(ba, bb, 0, proc->gc_cur);
		push(bignum_to_term(q, proc->gc_cur));
	}
	else // two ints
	{
		r = int_value(a) / int_value(b);
		push(intnum(r));
	}

/// rem
	term_t a, b;
	int_value_t r;
	b = pop();
	a = pop();

	if (!is_int(a) && !is_bignum(a))
		bad_arg();
	if (!is_int(b) && !is_bignum(b))
		bad_arg();

	if (is_bignum(a) || is_bignum(b))
	{
		bignum_t *ba = (is_bignum(a))? bn_value(a): bignum_from_int_value(int_value(a), proc->gc_cur);
		bignum_t *bb = (is_bignum(b))? bn_value(b): bignum_from_int_value(int_value(b), proc->gc_cur);
		
		bignum_t *reminder;
		bignum_div(ba, bb, &reminder, proc->gc_cur); // ignore returned value
		push(bignum_to_term(reminder, proc->gc_cur));
	}
	else // two ints
	{
		r = int_value(a) % int_value(b);
		push(intnum(r));
	}

/// and
	term_t a, b;
	int ok;
	b = pop();
	a = pop();
	if (!is_bool(a) || !is_bool(b))
		bad_arg();
	ok = (a == A_TRUE && b == A_TRUE);
	push(bool(ok));

/// band
	term_t a, b, r;
	b = pop();
	a = pop();
	if (!is_int(a) && !is_bignum(a))
		bad_arg();
	if (!is_int(b) && !is_bignum(b))
		bad_arg();
	if (is_int(a) && is_int(b))
		r = intnum(int_value(a) & int_value(b));
	else
	{
		bignum_t *b1, *b2, *b3;
		if (is_int(a))
			b1 = bignum_from_int_value(int_value(a), proc->gc_cur);
		else
			b1 = bn_value(a);
		if (is_int(b))
			b2 = bignum_from_int_value(int_value(b), proc->gc_cur);
		else
			b2 = bn_value(b);
		b3 = bignum_and(b1, b2, proc->gc_cur);
		r = bignum_to_term(b3, proc->gc_cur);
	}
	push(r);

/// not
	term_t t = top();
	if (!is_bool(t))
		bad_arg();
	top() = (t == A_TRUE) ?A_FALSE :A_TRUE;

/// bnot
	term_t a = pop();
	if (!is_int(a) && !is_bignum(a))
		bad_arg();
	if (is_int(a))
	{
		int_value_t v = ~int_value(a);
		push(intnum(v));
	}
	else
	{
		bignum_t *v = bignum_not(bn_value(a), proc->gc_cur);
		push(bignum_to_term(v, proc->gc_cur));
	}

/// negate
	term_t n = top();
	if (is_int(n))
	{
		int_value_t v = int_value(n);
		if (v == MIN_INT_VALUE)
		{
			//a single value which becomes bignum when negated
			top() = bignum(bignum_from_int_value(-v, proc->gc_cur));
		}
		else
			top() = intnum(-v);
	}
	else if (is_float(n))
		top() = make_float(-dbl_value(n), proc->gc_cur);
	else if (is_bignum(n))
	{
		bignum_t *a = bn_value(n);

		//this may happen for a single value of a
		if (bn_size(a) == 1 && bn_sign(a) != 0 && a->digits[0] <= -MIN_INT_VALUE)
			top() = intnum(MIN_INT_VALUE);
		else
		{
			bignum_t *b = bignum_copy(a, proc->gc_cur);
			bn_negate(b);
			top() = bignum(b);
		}
	}
	else
		return atom(A_BADARG);
