//
//	variables and arguments
//

/// get_arg(uint ano)
	if (arg(ano) == AI_UNDEFINED)
	{
		int a = 1;
	}
	push(arg(ano));

/// clear_arg(uint ano)
	term_t t = arg(ano);
	//arg(ano) = AI_UNDEFINED;	//TODO: otherwise stack trace does not work

	push(t);

/// set_arg(uint ano)
	arg(ano) = pop();

/// push_args(uint n)
	while (n--)
		rpush(pop());

/// set_args(uint n)
	while (--n >= 0)
		arg(n) = pop();

/// get_var(uint vno)
	if (var(vno) == AI_UNDEFINED)
	{
		int a = 1;
	}
	push(var(vno));

/// clear_var(uint vno)
	term_t t = var(vno);
	if (t == AI_UNDEFINED)
	{
		int a = 1;
	}
	var(vno) = AI_UNDEFINED;
	push(t);

/// set_var(uint vno)
	var(vno) = pop();
