/// beg_bin
	apr_pool_t *tmp;
	apr_pool_create(&tmp, 0);
	proc->bsite = bin_pad_make(tmp);

/// end_bin
	term_t size;
	apr_byte_t *data;
	if (!bin_pad_is_valid(proc->bsite))
		bad_arg();
	size = intnum(bin_pad_size(proc->bsite));
	data = bin_pad_data_dup(proc->bsite, proc->gc_cur);
	apr_pool_destroy(bin_pad_pool_get(proc->bsite));
	proc->bsite = 0;
	push(make_binary(size, data, proc->gc_cur));

/// reset_bin
	exception(A_ERROR, A_NOT_IMPLEMENTED);

/// bin_add_i_b(int unit)
	apr_byte_t buf[8];
	apr_int32_t nbits;
	term_t value;

	nbits = int_value(pop());
	value = pop();
	nbits *= unit;

	if (is_bignum(value))
	{
		bignum_t *bn = bn_value(value);
		int bn_size = bn_size(bn) * 32;	//TODO: negative bignum?

		if (nbits >= bn_size)
		{
			apr_uint32_t i;
			bin_pad_zeros(proc->bsite, nbits, bn_size);
			for (i = 0; i < bn_size(bn); i++)
			{
				PUT32(buf, bn->digits[i]);
				bin_pad_append(proc->bsite, buf, 32);
			}
		}
		else
		{
			apr_uint32_t i;

			// <<12345678999:48>> == <<0,2,223,220,28,151>>
			
			int o = nbits / 32; // offset in digits from the end of bn
			int s = nbits % 32; // the first digit treated differently

			// [n-o] .. [n-1] -- whole digits
			// [n-o-1] -- partial digit

			//partial digit
			if (s > 0)
			{
				PUT32(buf, bn->digits[bn_size(bn)-o-1] << (32-s));
				bin_pad_append(proc->bsite, buf, s);
			}

			//whole digits
			for (i = bn_size(bn)-o; i < bn_size(bn); i++)
			{
				PUT32(buf, bn->digits[i]);
				bin_pad_append(proc->bsite, buf, 32);
			}
		}
	}
	else if (is_int(value))
	{
		apr_int64_t v = int_value(value);

		if (nbits >= 64)
		{
			PUT64(buf, v);
			bin_pad_zeros(proc->bsite, nbits, 64);
			bin_pad_append(proc->bsite, buf, 64);
		}
		else
		{
			PUT64(buf, v << (64 - nbits));
			bin_pad_append(proc->bsite, buf, nbits);
		}
	}
	else
		exception(A_ERROR, A_BADARG);

/// bin_add_i_l(int unit)
	apr_byte_t buf[8];
	apr_int32_t nbits;
	term_t value;

	nbits = int_value(pop());
	value = pop();
	nbits *= unit;

	if (is_bignum(value))
	{
		bignum_t *bn = bn_value(value);
		int bn_size = bn_size(bn) * 32;	//TODO: negative bignum?

		if (nbits >= bn_size)
		{
			int i;
			for (i = bn_size(bn)-1; i >= 0; i--)
			{
				PUT32_LE(buf, bn->digits[i]);
				bin_pad_append(proc->bsite, buf, 32);
			}
			bin_pad_zeros(proc->bsite, nbits, bn_size);
		}
		else
		{
			apr_uint32_t i;

			// <<12345678999:48/little>> == <<151,28,220,223,2,0>>
			
			int o = nbits / 32; // # of digits from the end of bn
			int s = nbits % 32;

			// [n-1] .. [n-o] -- whole digits
			// [n-o-1] -- partial digit

			//whole digits
			for (i = bn_size(bn)-1; i >= bn_size(bn)-o; i--)
			{
				PUT32_LE(buf, bn->digits[i]);
				bin_pad_append(proc->bsite, buf, 32);
			}

			//partial digit
			if (s > 0)
			{
				PUT32_LE(buf, bn->digits[bn_size(bn)-o-1]);
				bin_pad_append(proc->bsite, buf, s);
			}
		}
	}
	else if (is_int(value))
	{
		apr_int64_t v = int_value(value);

		PUT64_LE(buf, v);
		if (nbits >= 64)
		{
			bin_pad_append(proc->bsite, buf, 64);
			bin_pad_zeros(proc->bsite, nbits-64, 0);
		}
		else
		{
			//adjust incomplete byte
			int o = nbits / 8;
			int s = nbits % 8;
			buf[o] <<= (8-s);
			bin_pad_append(proc->bsite, buf, nbits);
		}
	}
	else
		exception(A_ERROR, A_BADARG);

/// bin_add_i_n(int unit)
	exception(A_ERROR, A_NOT_IMPLEMENTED);

/// bin_add_f_b(int unit)
	apr_byte_t buf[8];
	apr_int32_t nbits = int_value(pop());
	term_t value = pop();

	union {
		apr_int64_t l;
		double d;
	} u;

	nbits *= unit;

	//XXX: size must be 64 here -- why?
	if (nbits != 64)
		exception(A_ERROR, A_BADARG);
	
	if (is_int(value))
		u.d = (double)int_value(value);
	else if (is_float(value))
		u.d = dbl_value(value);
	else // bignum
		u.d = bignum_to_double(bn_value(value));

	PUT64(buf, u.l);
	bin_pad_append(proc->bsite, buf, nbits);

/// bin_add_f_l(int unit)
	apr_byte_t buf[8];
	apr_int32_t nbits = int_value(pop());
	term_t value = pop();

	union {
		apr_int64_t l;
		double d;
	} u;

	nbits *= unit;

	//XXX: size must be 64 here -- why?
	if (nbits != 64)
		exception(A_ERROR, A_BADARG);
	
	if (is_int(value))
		u.d = (double)int_value(value);
	else if (is_float(value))
		u.d = dbl_value(value);
	else // bignum
		u.d = bignum_to_double(bn_value(value));

	PUT64_LE(buf, u.l);
	bin_pad_append(proc->bsite, buf, nbits);

/// bin_add_f_n(int unit)
	exception(A_ERROR, A_NOT_IMPLEMENTED);

/// bin_add_b(int unit)
	term_t size = pop();
	term_t bin = pop();
	apr_int32_t nbits;

	if (size == A_ALL)
		nbits = int_value(bin_size(bin))*8;
	else
	{
		nbits = int_value(size);
		nbits *= unit;

		if (nbits > int_value(bin_size(bin))*8)
			bad_arg();
	}

	bin_pad_append(proc->bsite, bin_data(bin), nbits);

/// bin_fetch_start
	proc->worm = pop();
	proc->marker = 0;

/// is_bin_consumed
	push(bool(proc->marker == int_value(bin_size(proc->worm))*8));
	proc->worm = AI_UNDEFINED;

/// bin_get_context
	push(proc->worm);

/// bin_size_is
	term_t sz = pop();
	term_t bin = pop();
	push(bool(int_value(bin_size(bin))*8 == int_value(sz)));

/// bin_size_is_at_least
	term_t sz = pop();
	term_t bin = pop();
	push(bool(int_value(bin_size(bin))*8 >= int_value(sz)));

/// bin_fetch_i_u_b(int unit)
	apr_byte_t buf[8];
	int nbits = int_value(pop()) * unit;
	int first_non_zero;
	static apr_array_header_t *digits = 0;
	int o, s;

	if (proc->marker + nbits > int_value(bin_size(proc->worm))*8)
		bad_arg();

	if (digits == 0)
    {
        apr_pool_t *poo;
        apr_pool_create(&poo, 0);       //TODO: never destroyed
        digits = apr_array_make(poo, 1, 4);
    }
 	else
		digits->nelts = 0;

	o = nbits / 32; // # of whole digits
	s = nbits % 32;

	//partial digit
	if (s > 0)
	{
		fetch_bits(bin_data(proc->worm), proc->marker, buf, s);
		proc->marker += s;
		*(digit_t *)apr_array_push(digits) = (apr_uint32_t)GET32(buf) >> (32-s);
	}

	//whole digits
	while (o > 0)
	{
		fetch_bits(bin_data(proc->worm), proc->marker, buf, 32);
		proc->marker += 32;
		*(digit_t *)apr_array_push(digits) = GET32(buf);
		o--;
	}

	// choose between intnum and bignum
	for (first_non_zero = 0; first_non_zero < digits->nelts; first_non_zero++)
	{
		if (((digit_t *)digits->elts)[first_non_zero] != 0)
			break;
	}

	if (first_non_zero == digits->nelts)
		push(intnum(0));
	else if (first_non_zero == digits->nelts-1)
	{
		digit_t dig = ((digit_t *)digits->elts)[first_non_zero];
		if (dig <= MAX_UINT_VALUE)
			push(intnum(dig));
		else
			push(bignum(bignum_make(0, 1, &dig, proc->gc_cur)));
	}
	else //use bignum
	{
		int n = digits->nelts - first_non_zero;
		bignum_t *bn = bignum_make(0, n, (digit_t *)digits->elts + first_non_zero, proc->gc_cur);
		push(bignum(bn));
	}

/// bin_fetch_i_u_l(int unit)
	apr_byte_t buf[8];
	int nbits = int_value(pop()) * unit;
	int first_non_zero;
	static apr_array_header_t *digits = 0;
	int i, o, s;

	if (proc->marker + nbits > int_value(bin_size(proc->worm))*8)
		bad_arg();

	if (digits == 0)
    {
        apr_pool_t *poo;
        apr_pool_create(&poo, 0);       //TODO: never destroyed
        digits = apr_array_make(poo, 1, 4);
    }
	else
		digits->nelts = 0;

	o = nbits / 32; // # of whole digits
	s = nbits % 32;
	
	//whole digits
	while (o > 0)
	{
		fetch_bits(bin_data(proc->worm), proc->marker, buf, 32);
		proc->marker += 32;
		*(digit_t *)apr_array_push(digits) = GET32_LE(buf);
		o--;
	}

	//partial digit
	if (s > 0)
	{
		int o2 = s / 8;	// last byte may require tweaking
		int s2 = s % 8;
		apr_uint32_t mask = ((1 << s) - 1);

		fetch_bits(bin_data(proc->worm), proc->marker, buf, s);
		proc->marker += s;

		if (s2 > 0)
			buf[o2] >>= (8-s2);

		*(digit_t *)apr_array_push(digits) = GET32_LE(buf) & mask;
	}

	//reverse digits
	for (i = 0; i < digits->nelts/2; i++)
	{
		digit_t temp = ((digit_t *)digits->elts)[i];
		((digit_t *)digits->elts)[i] = ((digit_t *)digits->elts)[digits->nelts-i-1];
		((digit_t *)digits->elts)[digits->nelts-i-1] = temp;
	}

	// choose between intnum and bignum
	for (first_non_zero = 0; first_non_zero < digits->nelts; first_non_zero++)
	{
		if (((digit_t *)digits->elts)[first_non_zero] != 0)
			break;
	}

	if (first_non_zero == digits->nelts)
		push(intnum(0));
	else if (first_non_zero == digits->nelts-1)
	{
		digit_t dig = ((digit_t *)digits->elts)[first_non_zero];
		if (dig <= MAX_UINT_VALUE)
			push(intnum(dig));
		else
			push(bignum(bignum_make(0, 1, &dig, proc->gc_cur)));
	}
	else //use bignum
	{
		int n = digits->nelts - first_non_zero;
		bignum_t *bn = bignum_make(0, n, (digit_t *)digits->elts + first_non_zero, proc->gc_cur);
		push(bignum(bn));
	}

/// bin_fetch_i_u_n(int unit)
	exception(A_ERROR, A_NOT_IMPLEMENTED);

/// bin_fetch_i_s_b(int unit)
	apr_byte_t buf[8];
	int nbits = int_value(pop()) * unit;
	int first_non_zero;
	static apr_array_header_t *digits = 0;
	int o, s;

	if (proc->marker + nbits > int_value(bin_size(proc->worm))*8)
		bad_arg();

	if (digits == 0)
    {
        apr_pool_t *poo;
        apr_pool_create(&poo, 0);       //TODO: never destroyed
        digits = apr_array_make(poo, 1, 4);
    }
	else
		digits->nelts = 0;

	o = nbits / 32; // # of whole digits
	s = nbits % 32;

	//partial digit
	if (s > 0)
	{
		fetch_bits(bin_data(proc->worm), proc->marker, buf, s);
		proc->marker += s;
		*(digit_t *)apr_array_push(digits) = GET32(buf) >> (32-s);
	}

	//whole digits
	while (o > 0)
	{
		fetch_bits(bin_data(proc->worm), proc->marker, buf, 32);
		proc->marker += 32;
		*(digit_t *)apr_array_push(digits) = GET32(buf);
		o--;
	}

	// choose between intnum and bignum
	for (first_non_zero = 0; first_non_zero < digits->nelts; first_non_zero++)
	{
		if (((digit_t *)digits->elts)[first_non_zero] != 0)
			break;
	}

	if (first_non_zero == digits->nelts)
		push(intnum(0));
	else if (first_non_zero == digits->nelts-1)
	{
		digit_t dig = ((digit_t *)digits->elts)[first_non_zero];
		if (dig <= MAX_UINT_VALUE)
			push(intnum(dig));
		else
			push(bignum(bignum_make(0, 1, &dig, proc->gc_cur)));
	}
	else //use bignum
	{
		int n = digits->nelts - first_non_zero;
		bignum_t *bn = bignum_make(0, n, (digit_t *)digits->elts + first_non_zero, proc->gc_cur);
		push(bignum(bn));
	}

/// bin_fetch_i_s_l(int unit)
	exception(A_ERROR, A_NOT_IMPLEMENTED);

/// bin_fetch_i_s_n(int unit)
	exception(A_ERROR, A_NOT_IMPLEMENTED);

/// bin_fetch_f_b(int unit)
	exception(A_ERROR, A_NOT_IMPLEMENTED);

/// bin_fetch_f_l(int unit)
	exception(A_ERROR, A_NOT_IMPLEMENTED);

/// bin_fetch_f_n(int unit)
	exception(A_ERROR, A_NOT_IMPLEMENTED);

/// bin_fetch_bin(int unit)
	term_t size = pop();
	int nbits, nbytes;
	apr_byte_t *data;
	term_t t;
	
	if (size == A_ALL)
		nbits = int_value(bin_size(proc->worm))*8 - proc->marker;
	else
		nbits = int_value(size) * unit;

	if (nbits % 8 != 0)
		bad_arg();

	nbytes = nbits / 8;

	if (proc->marker % 8 == 0)
		data = bin_data(proc->worm) + proc->marker / 8; //share data
	else
	{
		//fetch bits, copy data
		data = xalloc(proc->gc_cur, nbytes);
		fetch_bits(bin_data(proc->worm), proc->marker, data, nbits);
	}
	proc->marker += nbits;
	t = make_binary(intnum(nbytes), data, proc->gc_cur);
	push(t);
