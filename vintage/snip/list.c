/// car
	term_t l = pop();
	push(lst_value(l));

/// cdr
	term_t l = pop();
	push(lst_next(l));

/// make_cons
	term_t tail = pop();

	// wierd lists ok
	//if (!is_list(tail))
	//	bad_arg();

	top() = make_list2(top(), tail, proc->gc_cur);

/// make_cons_nil
	top() = make_list(top(), proc->gc_cur);

/// list_copy
	term_t lst = top();
	if (!is_list(lst))
		bad_arg();
	top() = copy_list(lst, proc->gc_cur); // shallow copy

/// list_append
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

/// list_subtract
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

/// pack_tuple(uint arity)
	term_t t = make_tuple_elts(arity,
		((term_t *)ds->elts) + ds->nelts - arity, proc->gc_cur);
	ds->nelts -= arity;
	push(t);

/// unpack_tuple
	term_t t = pop();
	int i;
	for (i = 0; i < int_value2(tup_size(t)); i++)
		push(tup_elts(t)[i]);

/// tuple_elem(uint index)
	push(tup_elts(pop())[index]);
