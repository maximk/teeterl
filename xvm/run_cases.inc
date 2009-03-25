case 0: // is_integer (snip/recog.c)
{
        term_t t = top();
        top() = bool(is_int(t) || is_bignum(t));
	break;
}
case 1: // is_bignum (snip/recog.c)
{
        term_t t = top();
        top() = bool(is_bignum(t));
	break;
}
case 2: // is_atom (snip/recog.c)
{
        term_t t = top();
        top() = bool(is_atom(t));
	break;
}
case 3: // is_boolean (snip/recog.c)
{
        term_t t = top();
        top() = bool(t == A_FALSE || t == A_TRUE);
	break;
}
case 4: // is_float (snip/recog.c)
{
        term_t t = top();
        top() = bool(is_float(t));
	break;
}
case 5: // is_function (snip/recog.c)
{
        term_t t = top();
        top() = bool(is_fun(t));
	break;
}
case 6: // is_function2 (snip/recog.c)
{
        term_t n = pop();
        term_t fun = top();

        if (!is_int(n))
                bad_arg();

        if (!is_fun(fun))
                top() = A_FALSE;
        else
        {
                //TODO: will be gone when fridge becomes list
                term_t fridge = fun_fridge(fun);
                int nfree = (is_tuple(fridge))
                        ?int_value(tup_size(fun_fridge(fun)))
                        :0;
                int arity1 = int_value(fun_arity(fun)) - nfree;
                top() = bool(int_value(n) == arity1);
        }
	break;
}
case 7: // is_number (snip/recog.c)
{
        term_t t = pop();
        push(bool(is_number(t)));
	break;
}
case 8: // is_pid (snip/recog.c)
{
        term_t t = pop();
        push(bool(is_pid(t)));
	break;
}
case 9: // is_port (snip/recog.c)
{
        term_t t = pop();
        push(bool(is_port(t)));
	break;
}
case 10: // is_record (snip/recog.c)
{
        term_t n = pop();
        term_t a = pop();
        term_t t = top();
        if (!is_int(n) || !is_atom(a))
                bad_arg();
        top() = bool(is_tuple(t) && tup_elts(t)[0] == a && tup_size(t) == n);
	break;
}
case 11: // is_reference (snip/recog.c)
{
        term_t t = pop();
        push(bool(is_ref(t)));
	break;
}
case 12: // is_cons (snip/recog.c)
{
        term_t t = pop();
        push(bool(is_cons(t)));
	break;
}
case 13: // is_nil (snip/recog.c)
{
        term_t t = top();
        top() = bool(is_nil(t));
	break;
}
case 14: // is_list (snip/recog.c)
{
        term_t t = top();
        top() = bool(is_list(t));
	break;
}
case 15: // is_tuple (snip/recog.c)
{
        term_t t = top();
        top() = bool(is_tuple(t));
	break;
}
case 16: // is_tuple_of_arity (snip/recog.c)
{
	apr_int32_t arity = (apr_int32_t) *proc->ip++;
        term_t t = top();
        int ok = is_tuple(t) && (int_value(tup_size(t)) == arity);
        top() = bool(ok);
	break;
}
case 17: // is_binary (snip/recog.c)
{
        term_t t = top();
        top() = bool(is_binary(t));
	break;
}
case 18: // dup (snip/stack.c)
{
        term_t t = top();
        push(t);
	break;
}
case 19: // swap (snip/stack.c)
{
        term_t a, b;
        a = top();
        b = top1();
        top() = b;
        top1() = a;
	break;
}
case 20: // drop (snip/stack.c)
{
	apr_int32_t n = (apr_int32_t) *proc->ip++;
        ds->nelts -= n;
	break;
}
case 21: // reset_msgs (snip/msg.c)
{
	term_t per = pop();			
	if (per == AI_UNDEFINED || per == A_INFINITY)
		proc->timeout = INFINITY;
	else
	{
		apr_uint32_t period_ms;
		if (!is_int(per) && !is_bignum(per))
			exception(A_ERROR, make_tuple2(A_TIMEOUT_VALUE, per, proc->gc_cur));
		if (is_int(per))
		{
			if (int_value(per) < 0)
				exception(A_ERROR, make_tuple2(A_TIMEOUT_VALUE, per, proc->gc_cur));
			period_ms = int_value(per);
		}
		else //bignum
		{
			bignum_t *bn = bn_value(per);
			if (bn_sign(bn) || bn_size(bn) != 1)	//zero is always int
				exception(A_ERROR, make_tuple2(A_TIMEOUT_VALUE, per, proc->gc_cur));
			period_ms = bn->digits[0];
		}

		if (period_ms == 0)	//special case, no wait
			proc->timeout = TIMEOUT_ZERO;
		else
		{
			apr_time_t now = apr_time_now();
			proc->timeout = now + period_ms * 1000;
		}
	}
	
	msg_queue_reset(proc->mailbox);
	break;
}
case 22: // get_msg (snip/msg.c)
{
	apr_uint32_t *expired = (apr_uint32_t *) *proc->ip++;
	term_t msg = msg_queue_next(proc->mailbox);

	if (msg == AI_UNDEFINED)
	{
		//all message are matched -- check timeout
		apr_time_t now = apr_time_now();
		if (proc->timeout == TIMEOUT_ZERO ||
			(proc->timeout != INFINITY && now >= proc->timeout))
		{
			proc->ip = expired;	//no value on stack
			break;
		}
		else
		{
			//retry get_msg after timeout or new message
			proc->ip -= 2;	//get_arg now has an argument

			if (proc->timeout == INFINITY)
				*retval = A_INFINITY;
			else
			{
				*retval = make_tuple3(intnum(proc->timeout/1000000/1000000),
					intnum((proc->timeout/1000000) % 1000000),
					intnum(proc->timeout % 1000000),
					proc->gc_cur);
			}

			return AI_WAIT;
		}
	}
	else
		push(msg);
	break;
}
case 23: // drop_msg (snip/msg.c)
{
	msg_queue_drop(proc->mailbox);
	break;
}
case 24: // send_msg (snip/msg.c)
{
	exception(A_ERROR, A_NOT_IMPLEMENTED);
	break;
}
case 25: // get_arg (snip/vararg.c)
{
	apr_int32_t ano = (apr_int32_t) *proc->ip++;
	if (arg(ano) == AI_UNDEFINED)
	{
		int a = 1;
	}
	push(arg(ano));
	break;
}
case 26: // clear_arg (snip/vararg.c)
{
	apr_int32_t ano = (apr_int32_t) *proc->ip++;
	term_t t = arg(ano);
	//arg(ano) = AI_UNDEFINED;	//TODO: otherwise stack trace does not work

	push(t);
	break;
}
case 27: // set_arg (snip/vararg.c)
{
	apr_int32_t ano = (apr_int32_t) *proc->ip++;
	arg(ano) = pop();
	break;
}
case 28: // push_args (snip/vararg.c)
{
	apr_int32_t n = (apr_int32_t) *proc->ip++;
	while (n--)
		rpush(pop());
	break;
}
case 29: // set_args (snip/vararg.c)
{
	apr_int32_t n = (apr_int32_t) *proc->ip++;
	while (--n >= 0)
		arg(n) = pop();
	break;
}
case 30: // get_var (snip/vararg.c)
{
	apr_int32_t vno = (apr_int32_t) *proc->ip++;
	if (var(vno) == AI_UNDEFINED)
	{
		int a = 1;
	}
	push(var(vno));
	break;
}
case 31: // clear_var (snip/vararg.c)
{
	apr_int32_t vno = (apr_int32_t) *proc->ip++;
	term_t t = var(vno);
	if (t == AI_UNDEFINED)
	{
		int a = 1;
	}
	var(vno) = AI_UNDEFINED;
	push(t);
	break;
}
case 32: // set_var (snip/vararg.c)
{
	apr_int32_t vno = (apr_int32_t) *proc->ip++;
	var(vno) = pop();
	break;
}
case 33: // jump_if_not (snip/jump.c)
{
	apr_uint32_t *l = (apr_uint32_t *) *proc->ip++;
	if (pop() != A_TRUE)
		proc->ip = l;
	break;
}
case 34: // jump_if (snip/jump.c)
{
	apr_uint32_t *l = (apr_uint32_t *) *proc->ip++;
	if (pop() == A_TRUE)
		proc->ip = l;
	break;
}
case 35: // jump (snip/jump.c)
{
	apr_uint32_t *l = (apr_uint32_t *) *proc->ip++;
	proc->ip = l;
	break;
}
case 36: // 'catch' (snip/jump.c)
{
	apr_uint32_t *l = (apr_uint32_t *) *proc->ip++;
	catch_t *cat = apr_array_push(proc->catches);
	cat->csp = cs->nelts;
	cat->dsp = ds->nelts;
	cat->ebp = proc->ebp;
	cat->mod_index = proc->mod_index;
	cat->ip = l;
	break;
}
case 37: // drop_catch (snip/jump.c)
{
	apr_array_pop(proc->catches);
	break;
}
case 38: // raise (snip/jump.c)
{
	term_t re = pop();
	term_t cl = pop();
	exception(cl, re);
	break;
}
case 39: // lit (snip/literal.c)
{
	term_t t = (term_t) *proc->ip++;
	//TODO: copy is needed because term may be buried during gc
	term_t copy = marshal_term(t, proc->gc_cur);
	push(copy);
	break;
}
case 40: // car (snip/list.c)
{
	term_t l = pop();
	push(lst_value(l));
	break;
}
case 41: // cdr (snip/list.c)
{
	term_t l = pop();
	push(lst_next(l));
	break;
}
case 42: // make_cons (snip/list.c)
{
	term_t tail = pop();

	// wierd lists ok
	//if (!is_list(tail))
	//	bad_arg();

	top() = make_list2(top(), tail, proc->gc_cur);
	break;
}
case 43: // make_cons_nil (snip/list.c)
{
	top() = make_list(top(), proc->gc_cur);
	break;
}
case 44: // list_copy (snip/list.c)
{
	term_t lst = top();
	if (!is_list(lst))
		bad_arg();
	top() = copy_list(lst, proc->gc_cur); // shallow copy
	break;
}
case 45: // list_append (snip/list.c)
{
	// a ++ b
	term_t b = pop();
	term_t a = top();
	if (!is_list(a))	//b may not be cons: [a|b]
		bad_arg();
	if (a == nil)
		top() = b;
	else if (b != nil)
	{
		term_t a1;
		for (;;)
		{
			a1 = lst_next(a);
			if (!is_cons(a1))
				break;
			a = a1;
		}

		if (!is_nil(a1))
			bad_arg();	//wierd list not allowed here

		lst_next(a) = b;
	}
	break;
}
case 46: // list_subtract (snip/list.c)
{
	// a -- b
	term_t a, b;
	b = pop();
	a = pop();

	if (!is_proper_list(a) || !is_proper_list(b))
		bad_arg();

	if (a == nil || b == nil)
		push(a);
	else
	{
		while (is_cons(b))
		{
			term_t out = lst_value(b);
			term_t a1 = nil;
			term_t cons = nil;
			term_t l = a;

			while (is_cons(l))
			{
				term_t v = lst_value(l);
				if (terms_are_equal(v, out, 1))
					break;
				lst_add(a1, cons, v, proc->gc_cur);
				l = lst_next(l);
			}

			if (is_cons(l))
			{
				if (is_cons(cons))
					lst_next(cons) = lst_next(l);
				else
					a1 = lst_next(l);
			}

			a = a1;

			b = lst_next(b);
		}
		push(a);
	}
	break;
}
case 47: // pack_tuple (snip/list.c)
{
	apr_int32_t arity = (apr_int32_t) *proc->ip++;
	term_t t = make_tuple_elts(arity,
		((term_t *)ds->elts) + ds->nelts - arity, proc->gc_cur);
	ds->nelts -= arity;
	push(t);
	break;
}
case 48: // unpack_tuple (snip/list.c)
{
	apr_int32_t arity = (apr_int32_t) *proc->ip++;
	term_t t = pop();
	int i;
	for (i = 0; i < int_value(tup_size(t)); i++)
		push(tup_elts(t)[i]);
	break;
}
case 49: // tuple_elem (snip/list.c)
{
	apr_int32_t index = (apr_int32_t) *proc->ip++;
	push(tup_elts(pop())[index]);
	break;
}
case 50: // beg_bin (snip/binary.c)
{
	apr_pool_t *tmp;
	apr_pool_create(&tmp, 0);
	proc->bsite = bin_pad_make(tmp);
	break;
}
case 51: // end_bin (snip/binary.c)
{
	term_t size;
	apr_byte_t *data;
	if (!bin_pad_is_valid(proc->bsite))
		bad_arg();
	size = intnum(bin_pad_size(proc->bsite));
	data = bin_pad_data_dup(proc->bsite, proc->gc_cur);
	apr_pool_destroy(bin_pad_pool_get(proc->bsite));
	proc->bsite = 0;
	push(make_binary(size, data, proc->gc_cur));
	break;
}
case 52: // bin_add_i_b (snip/binary.c)
{
	apr_int32_t unit = (apr_int32_t) *proc->ip++;
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
	break;
}
case 53: // bin_add_i_l (snip/binary.c)
{
	apr_int32_t unit = (apr_int32_t) *proc->ip++;
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
	break;
}
case 54: // bin_add_i_n (snip/binary.c)
{
	apr_int32_t unit = (apr_int32_t) *proc->ip++;
	exception(A_ERROR, A_NOT_IMPLEMENTED);
	break;
}
case 55: // bin_add_f_b (snip/binary.c)
{
	apr_int32_t unit = (apr_int32_t) *proc->ip++;
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
	break;
}
case 56: // bin_add_f_l (snip/binary.c)
{
	apr_int32_t unit = (apr_int32_t) *proc->ip++;
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
	break;
}
case 57: // bin_add_f_n (snip/binary.c)
{
	apr_int32_t unit = (apr_int32_t) *proc->ip++;
	exception(A_ERROR, A_NOT_IMPLEMENTED);
	break;
}
case 58: // bin_add_b (snip/binary.c)
{
	apr_int32_t unit = (apr_int32_t) *proc->ip++;
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
	break;
}
case 59: // bin_fetch_start (snip/binary.c)
{
	proc->worm = pop();
	proc->marker = 0;
	break;
}
case 60: // is_bin_consumed (snip/binary.c)
{
	push(bool(proc->marker == int_value(bin_size(proc->worm))*8));
	proc->worm = AI_UNDEFINED;
	break;
}
case 61: // bin_get_context (snip/binary.c)
{
	push(proc->worm);
	break;
}
case 62: // bin_size_is (snip/binary.c)
{
	term_t sz = pop();
	term_t bin = pop();
	push(bool(int_value(bin_size(bin))*8 == int_value(sz)));
	break;
}
case 63: // bin_size_is_at_least (snip/binary.c)
{
	term_t sz = pop();
	term_t bin = pop();
	push(bool(int_value(bin_size(bin))*8 >= int_value(sz)));
	break;
}
case 64: // bin_fetch_i_u_b (snip/binary.c)
{
	apr_int32_t unit = (apr_int32_t) *proc->ip++;
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
	break;
}
case 65: // bin_fetch_i_u_l (snip/binary.c)
{
	apr_int32_t unit = (apr_int32_t) *proc->ip++;
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
	break;
}
case 66: // bin_fetch_i_u_n (snip/binary.c)
{
	apr_int32_t unit = (apr_int32_t) *proc->ip++;
	exception(A_ERROR, A_NOT_IMPLEMENTED);
	break;
}
case 67: // bin_fetch_i_s_b (snip/binary.c)
{
	apr_int32_t unit = (apr_int32_t) *proc->ip++;
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
	break;
}
case 68: // bin_fetch_i_s_l (snip/binary.c)
{
	apr_int32_t unit = (apr_int32_t) *proc->ip++;
	exception(A_ERROR, A_NOT_IMPLEMENTED);
	break;
}
case 69: // bin_fetch_i_s_n (snip/binary.c)
{
	apr_int32_t unit = (apr_int32_t) *proc->ip++;
	exception(A_ERROR, A_NOT_IMPLEMENTED);
	break;
}
case 70: // bin_fetch_f_b (snip/binary.c)
{
	apr_int32_t unit = (apr_int32_t) *proc->ip++;
	exception(A_ERROR, A_NOT_IMPLEMENTED);
	break;
}
case 71: // bin_fetch_f_l (snip/binary.c)
{
	apr_int32_t unit = (apr_int32_t) *proc->ip++;
	exception(A_ERROR, A_NOT_IMPLEMENTED);
	break;
}
case 72: // bin_fetch_f_n (snip/binary.c)
{
	apr_int32_t unit = (apr_int32_t) *proc->ip++;
	exception(A_ERROR, A_NOT_IMPLEMENTED);
	break;
}
case 73: // bin_fetch_bin (snip/binary.c)
{
	apr_int32_t unit = (apr_int32_t) *proc->ip++;
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
	break;
}
case 74: // self (snip/stack.c)
{
		// self is known to be local
		push(localpid(proc->serial));
        
		//term_t pid = make_pid(my_node, proc->serial, 0, proc->gc_cur);
        //push(pid);
	break;
}
case 75: // enter (snip/jump.c)
{
	apr_int32_t n = (apr_int32_t) *proc->ip++;
	rpush(intnum(proc->ebp));
	proc->ebp = cs->nelts;
	for (;n > 0; n--)
		rpush(AI_UNDEFINED);
	break;
}
case 76: // leave (snip/jump.c)
{
	cs->nelts = proc->ebp;
	proc->ebp = int_value(rpop());
	break;
}
case 77: // ret (snip/jump.c)
{
	apr_int32_t n = (apr_int32_t) *proc->ip++;
	apr_uint32_t off = int_value(rpop());
	apr_uint32_t mod_index = int_value(rpop());
	apr_uint32_t *code;
	
	if (mod_index == MOD_INDEX_NONE)
	{
		*retval = top();
		return AI_DONE;
	}

	code = code_base_starts(proc->base, mod_index);

	proc->mod_index = mod_index;
	proc->code = code;
	proc->ip = code + off;
	break;
}
case 78: // addi (snip/arith.c)
{
	apr_int32_t i = (apr_int32_t) *proc->ip++;
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
		int r = int_value(a) + i;
		if (r > MAX_INT_VALUE || r < MIN_INT_VALUE)
			push(bignum(bignum_from32(r, proc->gc_cur)));
		else
			push(intnum(r));
	}
	break;
}
case 79: // subi (snip/arith.c)
{
	apr_int32_t i = (apr_int32_t) *proc->ip++;
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
		int r = int_value(a) - i;
		if (r > MAX_INT_VALUE || r < MIN_INT_VALUE)
			push(bignum(bignum_from32(r, proc->gc_cur)));
		else
			push(intnum(r));
	}
	break;
}
case 80: // add (snip/arith.c)
{
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
			r = bignum_add1(bn_value(b), int_value(a), proc->gc_cur);
		else if (is_int(b))
			r = bignum_add1(bn_value(a), int_value(b), proc->gc_cur);
		else
			r = bignum_add(bn_value(a), bn_value(b), proc->gc_cur);
		push(bignum_to_term(r, proc->gc_cur));
	}
	else // two ints
	{
		int r = int_value(a) + int_value(b);
		if (r > MAX_INT_VALUE || r < MIN_INT_VALUE)
			push(bignum(bignum_from32(r, proc->gc_cur)));
		else
			push(intnum(r));
	}
	break;
}
case 81: // sub (snip/arith.c)
{
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
			r = bignum_add1(bn_value(b), -int_value(a), proc->gc_cur);
			bn_negate(r);
		}
		else if (is_int(b))
			r = bignum_add1(bn_value(a), -int_value(b), proc->gc_cur);
		else
			r = bignum_sub(bn_value(a), bn_value(b), proc->gc_cur);
		push(bignum_to_term(r, proc->gc_cur));
	}
	else // two ints
	{
		int r = int_value(a) - int_value(b);
		if (r > MAX_INT_VALUE || r < MIN_INT_VALUE)
			push(bignum(bignum_from32(r, proc->gc_cur)));
		else
			push(intnum(r));
	}
	break;
}
case 82: // 'or' (snip/arith.c)
{
	term_t a, b;
	int ok;
	b = pop();
	a = pop();
	if (!is_bool(a) || !is_bool(b))
		bad_arg();
	ok = (a == A_TRUE || b == A_TRUE);
	push(bool(ok));
	break;
}
case 83: // 'bor' (snip/arith.c)
{
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
			b1 = bignum_from32(int_value(a), proc->gc_cur);
		else
			b1 = bn_value(a);
		if (is_int(b))
			b2 = bignum_from32(int_value(b), proc->gc_cur);
		else
			b2 = bn_value(b);
		b3 = bignum_or(b1, b2, proc->gc_cur);
		r = bignum_to_term(b3, proc->gc_cur);
	}
	push(r);
	break;
}
case 84: // 'xor' (snip/arith.c)
{
	term_t a, b;
	int ok;
	b = pop();
	a = pop();
	if (!is_bool(a) || !is_bool(b))
		bad_arg();
	ok = ((a == A_TRUE) != (b == A_TRUE));
	push(bool(ok));
	break;
}
case 85: // 'bxor' (snip/arith.c)
{
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
			b1 = bignum_from32(int_value(a), proc->gc_cur);
		else
			b1 = bn_value(a);
		if (is_int(b))
			b2 = bignum_from32(int_value(b), proc->gc_cur);
		else
			b2 = bn_value(b);
		b3 = bignum_xor(b1, b2, proc->gc_cur);
		r = bignum_to_term(b3, proc->gc_cur);
	}
	push(r);
	break;
}
case 86: // 'bsl' (snip/arith.c)
{
	// a bsl b
	term_t a, b, r;
	int shifts;
	b = pop();
	a = pop();

	if (!is_int(a) && !is_bignum(a))
		bad_arg();
	if (!is_int(b))
		bad_arg();

	shifts = int_value(b);

	if (is_bignum(a) || shifts > 0)
	{
		bignum_t *b1, *b2;

		if (shifts >= 100000000)
			exception(A_ERROR, A_SYSTEM_LIMIT);

		if (is_bignum(a))
			b1 = bn_value(a);
		else
			b1 = bignum_from32(int_value(a), proc->gc_cur);

		b2 = bignum_bsl(b1, shifts, proc->gc_cur);
		r = bignum_to_term(b2, proc->gc_cur);
	}
	else
	{
		// shifts < 0 -> bsr

		if (-shifts >= 32)
			r = intnum((int_value(a) > 0) ?0 :-1);
		else
			r = intnum(int_value(a) >> -shifts);
	}
	push(r);
	break;
}
case 87: // 'bsr' (snip/arith.c)
{
	// a bsl b
	term_t a, b, r;
	int shifts;
	b = pop();
	a = pop();

	if (!is_int(a) && !is_bignum(a))
		bad_arg();
	if (!is_int(b))
		bad_arg();

	shifts = int_value(b);

	if (is_bignum(a) || shifts < 0)
	{
		bignum_t *b1, *b2;

		if (-shifts >= 100000000)
			bad_arg();

		if (is_bignum(a))
			b1 = bn_value(a);
		else
			b1 = bignum_from32(int_value(a), proc->gc_cur);

		b2 = bignum_bsr(b1, shifts, proc->gc_cur);
		r = bignum_to_term(b2, proc->gc_cur);
	}
	else
	{
		// shifts > 0

		if (shifts >= 32)
			r = intnum((int_value(a) > 0) ?0 :-1);
		else
			r = intnum(int_value(a) >> shifts);
	}
	push(r);
	break;
}
case 88: // mult (snip/arith.c)
{
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
			apr_int32_t v = int_value(a);
			if (v < 0)
			{
				r = bignum_mult1(bn_value(b), -v, proc->gc_cur);
				bn_negate(r);
			}
			else
				r = bignum_mult1(bn_value(b), v, proc->gc_cur);
		}
		else if (is_int(b))
		{
			apr_int32_t v = int_value(b);
			if (v < 0)
			{
				r = bignum_mult1(bn_value(a), -v, proc->gc_cur);
				bn_negate(r);
			}
			else
				r = bignum_mult1(bn_value(a), v, proc->gc_cur);
		}
		else
			r = bignum_mult(bn_value(a), bn_value(b), proc->gc_cur);
		push(bignum_to_term(r, proc->gc_cur));
	}
	else // two ints
	{
		apr_int64_t r = (apr_int64_t)int_value(a) * int_value(b);
		if (r <= MAX_INT_VALUE && r >= MIN_INT_VALUE)
			push(intnum(r));
		else
			push(bignum(bignum_from64(r, proc->gc_cur)));
	}
	break;
}
case 89: // 'div' (snip/arith.c)
{
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
	break;
}
case 90: // idiv (snip/arith.c)
{
	term_t a, b;
	int r;
	b = pop();
	a = pop();

	if (!is_int(a) && !is_bignum(a))
		bad_arg();
	if (!is_int(b) && !is_bignum(b))
		bad_arg();

	if (is_bignum(a) || is_bignum(b))
	{
		bignum_t *ba = (is_bignum(a))? bn_value(a): bignum_from32(int_value(a), proc->gc_cur);
		bignum_t *bb = (is_bignum(b))? bn_value(b): bignum_from32(int_value(b), proc->gc_cur);
		
		bignum_t *q = bignum_div(ba, bb, 0, proc->gc_cur);
		push(bignum_to_term(q, proc->gc_cur));
	}
	else // two ints
	{
		r = int_value(a) / int_value(b);
		push(intnum(r));
	}
	break;
}
case 91: // 'rem' (snip/arith.c)
{
	term_t a, b;
	int r;
	b = pop();
	a = pop();

	if (!is_int(a) && !is_bignum(a))
		bad_arg();
	if (!is_int(b) && !is_bignum(b))
		bad_arg();

	if (is_bignum(a) || is_bignum(b))
	{
		bignum_t *ba = (is_bignum(a))? bn_value(a): bignum_from32(int_value(a), proc->gc_cur);
		bignum_t *bb = (is_bignum(b))? bn_value(b): bignum_from32(int_value(b), proc->gc_cur);
		
		bignum_t *reminder;
		bignum_div(ba, bb, &reminder, proc->gc_cur); // ignore returned value
		push(bignum_to_term(reminder, proc->gc_cur));
	}
	else // two ints
	{
		r = int_value(a) % int_value(b);
		push(intnum(r));
	}
	break;
}
case 92: // 'and' (snip/arith.c)
{
	term_t a, b;
	int ok;
	b = pop();
	a = pop();
	if (!is_bool(a) || !is_bool(b))
		bad_arg();
	ok = (a == A_TRUE && b == A_TRUE);
	push(bool(ok));
	break;
}
case 93: // 'band' (snip/arith.c)
{
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
			b1 = bignum_from32(int_value(a), proc->gc_cur);
		else
			b1 = bn_value(a);
		if (is_int(b))
			b2 = bignum_from32(int_value(b), proc->gc_cur);
		else
			b2 = bn_value(b);
		b3 = bignum_and(b1, b2, proc->gc_cur);
		r = bignum_to_term(b3, proc->gc_cur);
	}
	push(r);
	break;
}
case 94: // equal (snip/compare.c)
{
	term_t a, b;
	term_t ok;
	b = pop();
	a = pop();
	ok = bool(terms_are_equal(a, b, 0));
	push(ok);
	break;
}
case 95: // neq (snip/compare.c)
{
	term_t a, b;
	int ok;
	b = pop();
	a = pop();
	ok = bool(!terms_are_equal(a, b, 0));
	push(ok);
	break;
}
case 96: // lesseq (snip/compare.c)
{
	term_t a, b;
	int ok;
	b = pop();
	a = pop();

	ok = bool(!terms_are_more(a, b, proc->atoms));
	push(ok);
	break;
}
case 97: // less (snip/compare.c)
{
	term_t a, b;
	int ok;
	b = pop();
	a = pop();

	ok = bool(terms_are_less(a, b, proc->atoms));
	push(ok);
	break;
}
case 98: // moreeq (snip/compare.c)
{
	term_t a, b;
	int ok;
	b = pop();
	a = pop();

	ok = bool(!terms_are_less(a, b, proc->atoms));
	push(ok);
	break;
}
case 99: // more (snip/compare.c)
{
	term_t a, b;
	int ok;
	b = pop();
	a = pop();

	ok = bool(terms_are_more(a, b, proc->atoms));
	push(ok);
	break;
}
case 100: // exeq (snip/compare.c)
{
	term_t a, b;
	term_t ok;
	b = pop();
	a = pop();
	ok = bool(terms_are_equal(a, b, 1));
	push(ok);
	break;
}
case 101: // nexeq (snip/compare.c)
{
	term_t a, b;
	term_t ok;
	b = pop();
	a = pop();
	ok = bool(!terms_are_equal(a, b, 1));
	push(ok);
	break;
}
case 102: // 'not' (snip/arith.c)
{
	term_t t = top();
	if (!is_bool(t))
		bad_arg();
	top() = (t == A_TRUE) ?A_FALSE :A_TRUE;
	break;
}
case 103: // 'bnot' (snip/arith.c)
{
	term_t a = pop();
	if (!is_int(a) && !is_bignum(a))
		bad_arg();
	if (is_int(a))
	{
		int v = ~int_value(a);
		push(intnum(v));
	}
	else
	{
		bignum_t *v = bignum_not(bn_value(a), proc->gc_cur);
		push(bignum_to_term(v, proc->gc_cur));
	}
	break;
}
case 104: // negate (snip/arith.c)
{
	term_t n = top();
	if (is_int(n))
	{
		apr_int32_t v = int_value(n);
		if (v == MIN_INT_VALUE)
		{
			//a single value which becomes bignum when negated
			top() = bignum(bignum_from32(-v, proc->gc_cur));
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
	break;
}
case 105: // equal_to (snip/compare.c)
{
	term_t t = (term_t) *proc->ip++;
	term_t ok;
	ok = bool(terms_are_equal(pop(), t, 0));
	push(ok);
	break;
}
case 106: // neq_to (snip/compare.c)
{
	term_t t = (term_t) *proc->ip++;
	int ok;
	ok = bool(!terms_are_equal(pop(), t, 0));
	push(ok);
	break;
}
case 107: // lesseq_than (snip/compare.c)
{
	term_t t = (term_t) *proc->ip++;
	int ok;
	ok = bool(!terms_are_more(pop(), t, proc->atoms));
	push(ok);
	break;
}
case 108: // less_than (snip/compare.c)
{
	term_t t = (term_t) *proc->ip++;
	int ok;
	ok = bool(terms_are_less(pop(), t, proc->atoms));
	push(ok);
	break;
}
case 109: // moreeq_than (snip/compare.c)
{
	term_t t = (term_t) *proc->ip++;
	int ok;
	ok = bool(!terms_are_less(pop(), t, proc->atoms));
	push(ok);
	break;
}
case 110: // more_than (snip/compare.c)
{
	term_t t = (term_t) *proc->ip++;
	int ok;
	ok = bool(terms_are_more(pop(), t, proc->atoms));
	push(ok);
	break;
}
case 111: // bif_call0 (snip/jump.c)
{
	bif0_t entry = (bif0_t) *proc->ip++;
	term_t r = entry(proc);
	if (r != AI_OK)
		exception(A_ERROR, r);
	reductions--;
	break;
}
case 112: // bif_call1 (snip/jump.c)
{
	bif1_t entry = (bif1_t) *proc->ip++;
	term_t a = pop();
	term_t r = entry(a, proc);
	if (r != AI_OK)
		exception(A_ERROR, r);
	reductions--;
	break;
}
case 113: // bif_call2 (snip/jump.c)
{
	bif2_t entry = (bif2_t) *proc->ip++;
	term_t b = pop();
	term_t a = pop();
	term_t r = entry(a, b, proc);
	if (r != AI_OK)
		exception(A_ERROR, r);
	reductions--;
	break;
}
case 114: // bif_call3 (snip/jump.c)
{
	bif3_t entry = (bif3_t) *proc->ip++;
	term_t c = pop();
	term_t b = pop();
	term_t a = pop();
	term_t r = entry(a, b, c, proc);
	if (r != AI_OK)
		exception(A_ERROR, r);
	reductions--;
	break;
}
case 115: // bif_call4 (snip/jump.c)
{
	bif4_t entry = (bif4_t) *proc->ip++;
	term_t d = pop();
	term_t c = pop();
	term_t b = pop();
	term_t a = pop();
	term_t r = entry(a, b, c, d, proc);
	if (r != AI_OK)
		exception(A_ERROR, r);
	reductions--;
	break;
}
case 116: // call (snip/jump.c)
{
	apr_uint32_t *l = (apr_uint32_t *) *proc->ip++;
	rpush(intnum(proc->mod_index));
	rpush(intnum(proc->ip - proc->code));

	proc->ip = l;
	reductions--;
	break;
}
case 117: // tail_call (snip/jump.c)
{
	apr_uint32_t *l = (apr_uint32_t *) *proc->ip++;
	cs->nelts = proc->ebp;	// drop all variables
	proc->ebp = int_value(rpop());		// redundant?

	proc->ip = l;
	reductions--;
	break;
}
case 118: // call_far (snip/jump.c)
{
	term_t amod = (term_t) *proc->ip++;
	term_t afun = (term_t) *proc->ip++;
	int n = (int) *proc->ip++;
	apr_uint32_t *l;

	apr_uint32_t mod_index = proc->mod_index;
	apr_uint32_t *code = proc->code;

	l = code_base_lookup(proc->base,
		amod, afun, n, &proc->mod_index, &proc->code);
	if (l == 0)
	{
		//repackage args
		term_t args = nil;
		term_t cons = nil;
		apr_uint32_t m = n;

		while (m-- > 0)
			lst_add(args, cons, rpop(), proc->gc_cur);

		rpush(args);
		rpush(afun);
		rpush(amod);

		rpush(intnum(mod_index));
		rpush(intnum(proc->ip - code));

		l = code_base_lookup(proc->base,
			A_ERROR_HANDLER, A_UNDEFINED_FUNCTION, 3, &proc->mod_index, &proc->code);
	}
	else
	{
		rpush(intnum(mod_index));
		rpush(intnum(proc->ip - code));
	}

	proc->ip = l;
	reductions--;
	break;
}
case 119: // tail_call_far (snip/jump.c)
{
	term_t amod = (term_t) *proc->ip++;
	term_t afun = (term_t) *proc->ip++;
	int n = (int) *proc->ip++;
	apr_uint32_t *l;

	cs->nelts = proc->ebp;	// drop all variables
	proc->ebp = int_value(rpop());

	l = code_base_lookup(proc->base,
		amod, afun, n, &proc->mod_index, &proc->code);
	if (l == 0)
	{
		term_t off = rpop();
		term_t mdi = rpop();

		//repackage args
		term_t args = nil;
		term_t cons = nil;
		apr_uint32_t m = n;

		while (m-- > 0)
			lst_add(args, cons, rpop(), proc->gc_cur);

		rpush(args);
		rpush(afun);
		rpush(amod);

		rpush(mdi);
		rpush(off);

		l = code_base_lookup(proc->base,
			A_ERROR_HANDLER, A_UNDEFINED_FUNCTION, 3, &proc->mod_index, &proc->code);
	}

	proc->ip = l;
	reductions--;
	break;
}
case 120: // apply (snip/jump.c)
{
	bifN_t entry;
	term_t mod, fun, args;
	int arity;

apply_me:	// label used by apply_fun

	// the operation may be compiled instead of apply BIF
	// thus extra checks are needed, because there probably
	// no checks typically made in BIFs

	args = pop();
	fun = pop();
	mod = pop();

	if (!is_atom(mod) || !is_atom(fun) || !is_list(args))
		exception(A_ERROR, A_BADARG);

	arity = lst_len(args);

	entry = code_base_bif(proc->base, mod, fun, arity);
	if (entry)
	{
		if (arity == 0)
		{
			bif0_t entry0 = (bif0_t) entry;
			term_t r = entry0(proc);
			if (r != AI_OK)
				exception(A_ERROR, r);
		}
		else if (arity == 1)
		{
			bif1_t entry1 = (bif1_t) entry;
			term_t a = lst_value(args);
			term_t r = entry1(a, proc);
			if (r != AI_OK)
				exception(A_ERROR, r);
		}
		else if (arity == 2)
		{
			bif2_t entry2 = (bif2_t) entry;
			term_t a = lst_value(args);
			term_t args1 = lst_next(args);
			term_t b = lst_value(args1);
			term_t r = entry2(a, b, proc);
			if (r != AI_OK)
				exception(A_ERROR, r);
		}
		else if (arity == 3)
		{
			bif3_t entry3 = (bif3_t) entry;
			term_t a = lst_value(args);
			term_t args1 = lst_next(args);
			term_t b = lst_value(args1);
			term_t args2 = lst_next(args1);
			term_t c = lst_value(args2);
			term_t r = entry3(a, b, c, proc);
			if (r != AI_OK)
				exception(A_ERROR, r);
		}
		else if (arity == 4)
		{
			bif4_t entry4 = (bif4_t) entry;
			term_t a = lst_value(args);
			term_t args1 = lst_next(args);
			term_t b = lst_value(args1);
			term_t args2 = lst_next(args1);
			term_t c = lst_value(args2);
			term_t args3 = lst_next(args2);
			term_t d = lst_value(args3);
			term_t r = entry4(a, b, c, d, proc);
			if (r != AI_OK)
				exception(A_ERROR, r);
		} 
	}
	else
		proc_apply(proc, mod, fun, args);

	reductions--;
	break;
}
case 121: // apply2 (snip/jump.c)
{
	term_t args = pop();
	term_t fun = pop();
	term_t mod = pop();
	int arity = lst_len(args);
	term_t l;
	int i;

	apr_uint32_t mod_index = proc->mod_index;
	apr_uint32_t *code = proc->code;

	apr_uint32_t *ip;

	ip = code_base_lookup(proc->base,
		mod, fun, arity, &proc->mod_index, &proc->code);
	if (ip == 0)
		exception(A_ERROR, make_tuple4(A_UNDEF, mod, fun, args, proc->gc_cur));

	// pushing args in reverse order: kludgy
	for (i = 0; i < arity; i++)
		rpush(AI_UNDEFINED);

	l = args;
	i = 0;
	while (l != nil)
	{
		(((term_t *)cs->elts)[cs->nelts-i-1]) = lst_value(l);
		l = lst_next(l);
		i++;
	}

	rpush(intnum(mod_index));
	rpush(intnum(proc->ip - code));
	
	proc->ip = ip;
	reductions--;

	// call to apply2 signifies loading a new module code
	// yielding to init means that source line info gets
	// incorporated correctly
	return AI_YIELD;
	break;
}
case 122: // apply_fun (snip/jump.c)
{
	term_t args = pop();
	term_t fun = pop();

	if (!is_list(args))
		bad_arg();

	if (is_fun(fun))
	{
		term_t args2 = nil;
		term_t cons = nil;
		term_t fridge = fun_fridge(fun);
		int nargs, arity, nfree;
		int i;

		nargs = lst_len(args);

		arity = int_value(fun_arity(fun));
		nfree = int_value(tup_size(fridge));

		if (nargs + nfree != arity)
			bad_arg();

		while (is_cons(args))
		{
			lst_add(args2, cons, lst_value(args), proc->gc_cur);
			args = lst_next(args);
		}
		for (i = 0; i < nfree; i++)
			lst_add(args2, cons, tup_elts(fridge)[i], proc->gc_cur);

		push(fun_amod(fun));
		push(fun_afun(fun));
		push(args2);
	}
	else if (is_tuple(fun) && tup_size(fun) == intnum(2))	//obsolete tuple funs
	{
		term_t m = tup_elts(fun)[0];
		term_t f = tup_elts(fun)[1];

		push(m);
		push(f);
		push(args);
	}
	else
		bad_arg();

	goto apply_me;
	break;
}
case 123: // level (snip/stack.c)
{
	apr_int32_t n = (apr_int32_t) *proc->ip++;
        if (ds->nelts != n)
                exception(A_ERROR, A_STACK_LEVEL);
	break;
}
case 124: // rlevel (snip/stack.c)
{
	apr_int32_t n = (apr_int32_t) *proc->ip++;
        if (cs->nelts != n)
                exception(A_ERROR, A_STACK_LEVEL);
	break;
}
case 125: // nop (snip/stack.c)
{
        // do nothing
	break;
}
case 126: // break (snip/jump.c)
{
	
	// ip is not moved back, taken into account when execution resumes
	apr_uint32_t offset = proc->ip - proc->code - 1;
	term_t mod;

	// capture call stack and local vars
	proc_trace_stack(proc);
	proc_trace_locals(proc);

	proc->stopped_on_breakpoint = 1;
	proc->breakpoint_command = code_base_breakpoint_command(proc->base, proc->mod_index, offset);

	// {module,offset}
	mod = code_base_mod_name(proc->base, proc->mod_index);
	*retval = make_tuple2(mod, intnum(offset), proc->gc_cur);
	return AI_BREAK;
	break;
}
