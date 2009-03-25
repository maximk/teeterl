/// reset_msgs
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

/// get_msg
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

/// drop_msg
	msg_queue_drop(proc->mailbox);

/// send_msg
	exception(A_ERROR, A_NOT_IMPLEMENTED);
