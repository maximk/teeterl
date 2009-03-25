/// equal
	term_t a, b;
	term_t ok;
	b = pop();
	a = pop();
	ok = bool(terms_are_equal(a, b, 0));
	push(ok);

/// neq
	term_t a, b;
	int ok;
	b = pop();
	a = pop();
	ok = bool(!terms_are_equal(a, b, 0));
	push(ok);

/// lesseq
	term_t a, b;
	int ok;
	b = pop();
	a = pop();

	ok = bool(!terms_are_more(a, b, proc->atoms));
	push(ok);

/// less
	term_t a, b;
	int ok;
	b = pop();
	a = pop();

	ok = bool(terms_are_less(a, b, proc->atoms));
	push(ok);

/// moreeq
	term_t a, b;
	int ok;
	b = pop();
	a = pop();

	ok = bool(!terms_are_less(a, b, proc->atoms));
	push(ok);

/// more
	term_t a, b;
	int ok;
	b = pop();
	a = pop();

	ok = bool(terms_are_more(a, b, proc->atoms));
	push(ok);

/// exeq
	term_t a, b;
	term_t ok;
	b = pop();
	a = pop();
	ok = bool(terms_are_equal(a, b, 1));
	push(ok);

/// nexeq
	term_t a, b;
	term_t ok;
	b = pop();
	a = pop();
	ok = bool(!terms_are_equal(a, b, 1));
	push(ok);

/// equal_to(term t)
	term_t ok;
	ok = bool(terms_are_equal(pop(), t, 0));
	push(ok);

/// neq_to(term t)
	int ok;
	ok = bool(!terms_are_equal(pop(), t, 0));
	push(ok);

/// lesseq_than(term t)
	int ok;
	ok = bool(!terms_are_more(pop(), t, proc->atoms));
	push(ok);

/// less_than(term t)
	int ok;
	ok = bool(terms_are_less(pop(), t, proc->atoms));
	push(ok);

/// moreeq_than(term t)
	int ok;
	ok = bool(!terms_are_less(pop(), t, proc->atoms));
	push(ok);

/// more_than(term t)
	int ok;
	ok = bool(terms_are_more(pop(), t, proc->atoms));
	push(ok);
