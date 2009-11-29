//
//	Literals
//

/// lit(term t)
	//TODO: copy is needed because term may be buried during gc
	term_t copy = marshal_term(t, proc->gc_cur);
	push(copy);
