//
//
//

#include "bifimpl.h"

// lists:member/2 [0]
term_t bif_member2(term_t Elem, term_t List, proc_t *proc)
{
	term_t cons = List;
	if (!is_list(List))
		bif_bad_arg(List);
	while (is_cons(cons))
	{
		term_box_t *box = peel(cons);
		if (Elem == box->cons.head || are_terms_equal(Elem, box->cons.head))
			return A_TRUE;
		cons = box->cons.tail;
	}
	if (cons != nil)
		bif_bad_arg(List);
	return A_FALSE;
}

term_t bif_keyfind3(term_t Key, term_t N, term_t TupleList, proc_t *proc)
{
	term_t cons = TupleList;
	int i;
	if (!is_int(N))
		bif_bad_arg(N);
	if (!is_list(TupleList))
		bif_bad_arg(TupleList);
	i = int_value(N);
	if (i < 1)
		bif_bad_arg(N);
	while (is_cons(cons))
	{
		term_box_t *cbox = peel(cons);
		term_t tuple = cbox->cons.head;
		if (is_tuple(tuple))
		{
			term_box_t *tbox = peel(tuple);
			if (tbox->tuple.size >= i &&
				(Key == tbox->tuple.elts[i-1] ||
					are_terms_equal(Key, tbox->tuple.elts[i-1])))
				return tuple;
		}
		cons = cbox->cons.tail;
	}
	if (cons != nil)
		bif_bad_arg(TupleList);
	return A_FALSE;
}

// lists:keysearch/3 [2]
term_t bif_keysearch3(term_t Key, term_t N, term_t TupleList, proc_t *proc)
{
	term_t cons = TupleList;
	int i;
	if (!is_int(N))
		bif_bad_arg(N);
	if (!is_list(TupleList))
		bif_bad_arg(TupleList);
	i = int_value(N);
	if (i < 1)
		bif_bad_arg(N);
	while (is_cons(cons))
	{
		term_box_t *cbox = peel(cons);
		term_t tuple = cbox->cons.head;
		if (is_tuple(tuple))
		{
			term_box_t *tbox = peel(tuple);
			if (tbox->tuple.size >= i &&
				(Key == tbox->tuple.elts[i-1] ||
					are_terms_equal(Key, tbox->tuple.elts[i-1])))
				return heap_tuple2(proc->heap, A_VALUE, tuple);
		}
		cons = cbox->cons.tail;
	}
	if (cons != nil)
		bif_bad_arg(TupleList);
	return A_FALSE;
}

// lists:keymember/3 [3]
term_t bif_keymember3(term_t Key, term_t N, term_t TupleList, proc_t *proc)
{
	term_t cons;
	int index;
	if (!is_int(N))
		bif_bad_arg(N);
	index = int_value(N);
	if (index < 1)
		bif_bad_arg(N);
	if (!is_list(TupleList))
		bif_bad_arg(TupleList);
	cons = TupleList;
	while (is_cons(cons))
	{
		term_box_t *cb = peel(cons);
		term_t t = cb->cons.head;
		if (is_tuple(t))
		{
			term_box_t *tb = peel(t);
			if (index <= tb->tuple.size)
			{
				if (Key == tb->tuple.elts[index-1] ||
						are_terms_equal(Key, tb->tuple.elts[index-1]))
					return A_TRUE;
			}
		}
		cons = cb->cons.tail;
	}

	if (!is_nil(cons))
		bif_bad_arg(TupleList);

	return A_FALSE;
}

// lists:reverse/2 [4]
term_t bif_reverse2(term_t Ls, term_t Hs, proc_t *proc)
{
	term_t cons = Ls;
	if (!is_list(Ls) || !is_list(Hs))
		bif_bad_arg0();
	while (is_cons(cons))
	{
		term_box_t *cb = peel(cons);
		Hs = heap_cons2(proc->heap, cb->cons.head, Hs);
		cons = cb->cons.tail;
	}
	if (!is_nil(cons))
		bif_bad_arg(Ls);
	return Hs;
}

//EOF
