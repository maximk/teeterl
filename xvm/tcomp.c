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

#include "tcomp.h"

// to make local pids compare correctly
extern term_t my_node;
extern term_t my_prev_node;

extern apr_uint32_t my_creation;

int terms_are_equal(term_t a, term_t b, int exact)
{
	//Literals are coalesced on module load
	//if (is_pinned(a) && is_pinned(b))
	//	return a == b;

	//but,
	//not always, e.g. 52.00 and {52.00}
	//coalescing subterms may increase loading
	//time but let us revisit this some day

	//TODO: see above

	return terms_are_equal0(a, b, exact);
}

//same as above but with no regard to pinning
int terms_are_equal0(term_t a, term_t b, int exact)
{
	if (a == b)
		return 1;

	if (is_bignum(a) && is_bignum(b))
		return bignum_are_equal(bn_value(a), bn_value(b));
	if (is_float(a) && is_float(b))
		return dbl_value(a) == dbl_value(b);
	else if (is_cons(a) && is_cons(b))
	{
		//const char *s1;
		//const char *s2;
		//apr_pool_t *p;

		//apr_pool_create(&p, 0);
		//s1 = stringify_term(a, 0, p);
		//s2 = stringify_term(b, 0, p);

		do {
			if (terms_are_equal(lst_value(a), lst_value(b), exact) != 1)
				return 0;
			a = lst_next(a);
			b = lst_next(b);
		} while (is_cons(a) && is_cons(b));

		return is_nil(a) && is_nil(b);
	}
	else if (is_tuple(a) && is_tuple(b))
	{
		int i;
		if (tup_size(a) != tup_size(b))
			return 0;
		for (i = 0; i < int_value2(tup_size(a)); i++)
			if (terms_are_equal(tup_elts(a)[i], tup_elts(b)[i], exact) != 1)
				return 0;
		return 1;
	}
	else if (is_binary(a) && is_binary(b))
	{
		int d;
		if (bin_size(a) != bin_size(b))
			return 0;
		d = memcmp(bin_data(a), bin_data(b), int_value2(bin_size(a)));
		return d == 0;
	}
	else if (is_fun(a) && is_fun(b))
	{
		if (fun_amod(a) != fun_amod(b))
			return 0;
		if (fun_afun(a) != fun_afun(b))
			return 0;
		if (fun_arity(a) != fun_arity(b))
			return 0;
		return terms_are_equal(fun_fridge(a), fun_fridge(b), exact);
	}
	else if (is_pid(a) && is_pid(b))
	{
		term_t n1 = pid_node(a);
		term_t n2 = pid_node(b);

		if (n1 == my_node || n1 == my_prev_node) n1 = A_LOCAL;
		if (n2 == my_node || n2 == my_prev_node) n2 = A_LOCAL;

		if (n1 != n2)
			return 0;
		if (pid_serial(a) != pid_serial(b))
			return 0;
		return pid_creation(a) == pid_creation(b);
	}
	else if (is_ref(a) && is_ref(b) || is_port(a) && is_port(b))
	{
		term_t n1 = prp_node(a);
		term_t n2 = prp_node(b);

		if (n1 == my_node || n1 == my_prev_node) n1 = A_LOCAL;
		if (n2 == my_node || n2 == my_prev_node) n2 = A_LOCAL;

		if (n1 != n2)
			return 0;
		if (prp_serial(a) != prp_serial(b))
			return 0;
		return prp_creation(a) == prp_creation(b);
	}
	else if (exact)
		return 0;
	else if (is_int(a) && is_float(b))
		return (double)int_value(a) == dbl_value(b);
	else if (is_float(a) && is_int(b))
		return dbl_value(a) == (double)int_value(b);
	else if (is_bignum(a) && is_float(b))
		return bignum_to_double(bn_value(a)) == dbl_value(b);
	else if (is_float(a) && is_bignum(b))
		return dbl_value(a) == bignum_to_double(bn_value(b));
	else
		return 0;
}

int terms_are_more(term_t a, term_t b, atoms_t *atoms)
{
	return terms_are_less(b, a, atoms);
}

int terms_are_less(term_t a, term_t b, atoms_t *atoms)
{
	if (is_int(a) && is_bignum(b)) // int is always less than bignum (unless bignum is negative)
		return !bn_sign(bn_value(b)); 
	else if (is_bignum(a) && is_int(b))
		return bn_sign(bn_value(a));
	else if (is_bignum(a) && is_bignum(b))
		return bignum_are_less(bn_value(a), bn_value(b));
	else if (is_int(a) && is_int(b))
		return int_value(a) < int_value(b);
	else if (is_float(a) && is_float(b))
		return dbl_value(a) < dbl_value(b);
	else if (is_atom(a) && is_atom(b))
	{
		cstr_t *print1 = atoms_get(atoms, index(a));
		cstr_t *print2 = atoms_get(atoms, index(b));
		int short_len = (print1->size < print2->size)
			?print1->size
			:print2->size;
		int d = memcmp(print1->data, print2->data, short_len);
		if (d == 0)
			return print1->size < print2->size;
		return d < 0;
	}
	else if (is_cons(a) && is_cons(b))
	{
		while (a != nil && b != nil)
		{
			term_t r = terms_are_less(lst_value(a), lst_value(b), atoms);
			if (r == 1)
				return r;
			r = terms_are_more(lst_value(a), lst_value(b), atoms);
			if (r == 1)
				return 0;
			a = lst_next(a);
			b = lst_next(b);
		}
		return a == nil && b != nil;
	}
	else if (is_tuple(a) && is_tuple(b))
	{
		int i;
		int na = int_value2(tup_size(a));
		int nb = int_value2(tup_size(b));
		if (na < nb)
			return 1;
		if (na > nb)
			return 0;
		for (i = 0; i < na; i++)
		{
			term_t ea = tup_elts(a)[i];
			term_t eb = tup_elts(b)[i];
			if (terms_are_less(ea, eb, atoms))
				return 1;
			if (terms_are_more(ea, eb, atoms))
				return 0;
		}
		return 0;
	}
	else if (is_pid(a) && is_pid(b))
	{
		term_t n1 = pid_node(a);
		term_t n2 = pid_node(b);

		if (n1 == my_node || n1 == my_prev_node) n1 = A_LOCAL;
		if (n2 == my_node || n2 == my_prev_node) n2 = A_LOCAL;

		if (terms_are_less(n1, n2, atoms))
			return 1;
		if (terms_are_more(n1, n2, atoms))
			return 0;
		return pid_serial(a) < pid_serial(b);
	}
	else if (is_ref(a) && is_ref(b) || is_port(a) && is_port(b))
	{
		term_t n1 = prp_node(a);
		term_t n2 = prp_node(b);

		if (n1 == my_node || n1 == my_prev_node) n1 = A_LOCAL;
		if (n2 == my_node || n2 == my_prev_node) n2 = A_LOCAL;

		if (terms_are_less(n1, n2, atoms))
			return 1;
		if (terms_are_more(n1, n2, atoms))
			return 0;
		return prp_serial(a) < prp_serial(b);
	}
	else if (is_binary(a) && is_binary(b))
	{
		// binaries compared by values first
		// if one binary is prefix of the other
		// then sizes are compared
		
		int sa = int_value2(bin_size(a));
		int sb = int_value2(bin_size(b));
		int ss = (sa > sb) ?sb :sa;
		int cmp = memcmp(bin_data(a), bin_data(b), ss);
		
		if (cmp < 0)
			return 1;
		if (cmp > 0)
			return 0;
		return sa < sb;
	}
	else if (is_int(a) && is_float(b))
		return (double)int_value(a) < dbl_value(b);
	else if (is_float(a) && is_int(b))
		return dbl_value(a) < (double)int_value(b);
	else
	{
		//number < atom < reference < port < pid < tuple < empty_list < list < binary
		int oa = is_int(a)? 1:
			is_float(a)?	1:
			is_atom(a)?		2:
			is_ref(a)?		3:
			is_port(a)?		4:
			is_pid(a)?		5:
			is_tuple(a)?	6:
			is_nil(a)?		7:
			is_cons(a)?		8:
			is_binary(a)?	9:
			-1;
		int ob = is_int(b)?	1:
			is_float(b)?	1:
			is_atom(b)?		2:
			is_ref(b)?		3:
			is_port(b)?		4:
			is_pid(b)?		5:
			is_tuple(b)?	6:
			is_nil(b)?		7:
			is_cons(b)?		8:
			is_binary(b)?	9:
			-1;
		return oa < ob;
	}
}

//EOF
