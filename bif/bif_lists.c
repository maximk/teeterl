/*
* Copyright (c) 2009, Maxim Kharchenko
* All rights reserved.
*
* Redistribution and use in source and binary forms, with or without
* modification, are permitted provided that the following conditions are met:
*     * Redistributions of source code must retain the above copyright
*       notice, this list of conditions and the following disclaimer.
*     * Redistributions in binary form must reproduce the above copyright
*       notice, this list of conditions and the following disclaimer in the
*       documentation and/or other materials provided with the distribution.
*     * Neither the name of the author nor the names of his contributors
*	    may be used to endorse or promote products derived from this software
*		without specific prior written permission.
*
* THIS SOFTWARE IS PROVIDED BY Maxim Kharchenko ''AS IS'' AND ANY
* EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
* WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
* DISCLAIMED. IN NO EVENT SHALL Maxim Kharchenko BE LIABLE FOR ANY
* DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
* (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
* LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
* ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
* (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
* SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

#include "bif.h"

#include "tcomp.h"

term_t bif_reverse2(term_t List, term_t Tail, process_t *ctx)
{
	if (!is_list(List) || !is_list(Tail))
		return A_BADARG;

	while (is_cons(List))
	{
		Tail = make_list2(lst_value(List), Tail, proc_gc_pool(ctx));
		List = lst_next(List);
	}

	result(Tail);
	return AI_OK;
}

//Types: 
//
//Key = term()
//N = 1..size(Tuple)
//TupleList = [Tuple]
// Tuple = tuple()
//
//
//Returns true if there is a tuple in TupleList whose Nth element is Key,
//otherwise false. 
//
term_t bif_keymember3(term_t Key, term_t N, term_t TupleList, process_t *ctx)	// -> bool()
{
	int n;
	if (!is_int(N) || !is_list(TupleList))
		return A_BADARG;
	n = int_value2(N);
	if (n <= 0)
		return A_BADARG;
	while (TupleList != nil)
	{
		int size;
		term_t row = lst_value(TupleList);
		TupleList = lst_next(TupleList);
		if (!is_tuple(row))
			continue;
		size = int_value2(tup_size(row));
		if (n > size)
			continue;
		if (terms_are_equal(Key, tup_elts(row)[n-1], 0))
		{
			result(A_TRUE);
			return AI_OK;
		}
	}
	result(A_FALSE);
	return AI_OK;
}

//Types: 
//
//Key = term()
//N = 1..size(Tuple)
//TupleList = [Tuple]
//Tuple = tuple()
//
//
//Searches the list of the tuples TupleList for a tuple whose Nth element is
//Key. Returns {value, Tuple} if such a tuple is found, or false otherwise. 
//
term_t bif_keysearch3(term_t Key, term_t N, term_t TupleList, process_t *ctx)	// -> {value, Tuple} | false 
{
	int n;
	if (!is_int(N) || !is_list(TupleList))
		return A_BADARG;
	n = int_value2(N);
	if (n <= 0)
		return A_BADARG;
	while (TupleList != nil)
	{
		int size;
		term_t row = lst_value(TupleList);
		TupleList = lst_next(TupleList);
		if (!is_tuple(row))
			continue;
		size = int_value2(tup_size(row));
		if (n > size)
			continue;
		if (terms_are_equal(Key, tup_elts(row)[n-1], 0))
		{
			result(make_tuple2(A_VALUE, row, proc_gc_pool(ctx)));
			return AI_OK;
		}
	}
	result(A_FALSE);
	return AI_OK;
}

//Types: 
//
//List = [term()], term_t length(List)>0
//Last = term()
//
//
//Returns the last element in List. 
//
term_t bif_last1(term_t List, process_t *ctx)	// -> Last
{
	term_t next;
	if (!is_cons(List))
		return A_BADARG;
	for (;;)
	{
		next = lst_next(List);
		if (!is_cons(next))
			break;
		List = next;
	}

	if (!is_nil(next))
		return A_BADARG;	//no wierdness

	result(lst_value(List));
	return AI_OK;
}

term_t bif_member2(term_t Elem, term_t List, process_t *ctx)
{
	if (!is_list(List))
		return A_BADARG;

	while (is_cons(List))
	{
		if (terms_are_equal(Elem, lst_value(List), 0))
		{
			result(A_TRUE);
			return AI_OK;
		}
		List = lst_next(List);
	}
	result(A_FALSE);
	return AI_OK;
}

//EOF
