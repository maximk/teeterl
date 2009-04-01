//
//	jumps and calls
//

//
// Stack frame layout:
//
//		argN
//		...
//		arg0
//		mod
//		off
//		saved_ebp
// ebp->var0
//		...
//		varM
// csp->

/// jump_if_not(codep l)
	if (pop() != A_TRUE)
		proc->ip = l;

/// jump_if(codep l)
	if (pop() == A_TRUE)
		proc->ip = l;

/// jump(codep l)
	proc->ip = l;

/// bif_call0(ent0 entry)
	term_t r = entry(proc);
	if (r != AI_OK)
		exception(A_ERROR, r);
	reductions--;

/// bif_call1(ent0 entry)
	term_t a = pop();
	term_t r = entry(a, proc);
	if (r != AI_OK)
		exception(A_ERROR, r);
	reductions--;

/// bif_call2(ent0 entry)
	term_t b = pop();
	term_t a = pop();
	term_t r = entry(a, b, proc);
	if (r != AI_OK)
		exception(A_ERROR, r);
	reductions--;

/// bif_call3(ent0 entry)
	term_t c = pop();
	term_t b = pop();
	term_t a = pop();
	term_t r = entry(a, b, c, proc);
	if (r != AI_OK)
		exception(A_ERROR, r);
	reductions--;

/// bif_call4(ent0 entry)
	term_t d = pop();
	term_t c = pop();
	term_t b = pop();
	term_t a = pop();
	term_t r = entry(a, b, c, d, proc);
	if (r != AI_OK)
		exception(A_ERROR, r);
	reductions--;

/// call(codep l)
	rpush(intnum(proc->mod_index));
	rpush(intnum(proc->ip - proc->code));

	proc->ip = l;
	reductions--;

/// tail_call(codep l)
	cs->nelts = proc->ebp;	// drop all variables
	proc->ebp = int_value2(rpop());		// redundant?

	proc->ip = l;
	reductions--;

/// call_far(term_t amod, term_t afun, uint n)
	celem_t *l;

	apr_uint32_t mod_index = proc->mod_index;
	celem_t *code = proc->code;

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

/// tail_call_far(uint amod, uint afun, uint n)
	celem_t *l;

	cs->nelts = proc->ebp;	// drop all variables
	proc->ebp = int_value2(rpop());

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

/// apply
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

/// apply2
	term_t args = pop();
	term_t fun = pop();
	term_t mod = pop();
	int arity = lst_len(args);
	term_t l;
	int i;

	apr_uint32_t mod_index = proc->mod_index;
	celem_t *code = proc->code;

	celem_t *ip;

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
	//
	// NB: this may cause a performance hit
	//
	return AI_YIELD;

/// apply_fun
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

		arity = int_value2(fun_arity(fun));
		nfree = int_value2(tup_size(fridge));

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

/// enter(uint n)
	rpush(intnum(proc->ebp));
	proc->ebp = cs->nelts;
	for (;n > 0; n--)
		rpush(AI_UNDEFINED);
/// leave
	cs->nelts = proc->ebp;
	proc->ebp = int_value2(rpop());

/// ret(uint n)
	apr_uint32_t off = (apr_uint32_t)int_value(rpop());
	apr_uint32_t mod_index = (apr_uint32_t)int_value(rpop());
	celem_t *code;
	
	if (mod_index == MOD_INDEX_NONE)
	{
		*retval = top();
		return AI_DONE;
	}

	code = code_base_starts(proc->base, mod_index);

	proc->mod_index = mod_index;
	proc->code = code;
	proc->ip = code + off;

/// catch(label l)
	catch_t *cat = apr_array_push(proc->catches);
	cat->csp = cs->nelts;
	cat->dsp = ds->nelts;
	cat->ebp = proc->ebp;
	cat->mod_index = proc->mod_index;
	cat->ip = l;

/// drop_catch
	apr_array_pop(proc->catches);

/// raise
	term_t re = pop();
	term_t cl = pop();
	exception(cl, re);

/// break
	
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
