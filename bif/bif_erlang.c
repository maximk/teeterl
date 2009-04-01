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

#include <apr_thread_proc.h>
#include <apr_time.h>
#include <apr_strings.h>
#include <apr_file_io.h>

#include "hash.h"
#include "exterm.h"
#include "port.h"
#include "errors.h"

term_t my_node = A_LOCAL;
term_t my_prev_node = A_LOCAL;

apr_uint32_t my_creation = 0;

term_t bif_display1(term_t Term, process_t *ctx)
{
	apr_pool_t *pool;
	const char *z;
	apr_pool_create(&pool, 0);
	z = stringify_term(Term, proc_atoms(ctx), pool);
	printf("%s\n", z);
	result(A_TRUE);
	apr_pool_destroy(pool);
	return AI_OK;
}

term_t bif_send_msg0_2(term_t Rcpt, term_t Msg, process_t *ctx)
{
	if (is_atom(Rcpt) || is_pid(Rcpt))
	{
		process_t *addressee;
		if (is_atom(Rcpt))
			addressee = proc_registered(Rcpt);
		else
			addressee = proc_lookup(pid_serial(Rcpt));
		if (addressee == 0)
			return A_BADARG;

		proc_new_mail(addressee, Msg);
	}
	else if (is_port(Rcpt))
	{
		port_t *port = port_lookup(prp_serial(Rcpt));
		if (port != 0)
			port_send(port, Msg);
		else
		{
			//port closed -- discard
		}
	}
	else
		return A_BADARG;

	result(Msg);
	return AI_OK;
}

term_t bif_make_fun3(term_t A, term_t B, term_t C, process_t *ctx)
{
	//TODO: wierd arguments + fridge is tuple
	term_t m, f, n, fridge;
	term_t fun;

	if (is_int(C))
	{
		// A=module
		// B=name
		// C=arity

		m = A;
		f = B;
		n = C;
		fridge = make_tuple(0, proc_gc_pool(ctx));
	}
	else
	{
		// A=name
		// B=arity
		// C=fridge

		m = proc_get_info(ctx, A_MODULE);	// no marshalling needed
		f = A;
		n = B;
		fridge = C;
	}

	fun = make_fun(fridge, m, f, n, proc_gc_pool(ctx));
	result(fun);
	return AI_OK;
}

term_t bif_fun_info1(term_t Fun, process_t *ctx)
{
	term_t e1, e2, e3, e4;
	term_t r = nil;
	term_t cons = nil;

	if (!is_fun(Fun))
		return A_BADARG;

	//TODO: see below, move to erlang.erl
	e1 = make_tuple2(A_MODULE, fun_amod(Fun), proc_gc_pool(ctx));
	e2 = make_tuple2(A_NAME, fun_afun(Fun), proc_gc_pool(ctx));
	e3 = make_tuple2(A_ARITY, fun_arity(Fun), proc_gc_pool(ctx));
	e4 = make_tuple2(A_ENV, fun_fridge(Fun), proc_gc_pool(ctx));

	lst_add(r, cons, e1, proc_gc_pool(ctx));
	lst_add(r, cons, e2, proc_gc_pool(ctx));
	lst_add(r, cons, e3, proc_gc_pool(ctx));
	lst_add(r, cons, e4, proc_gc_pool(ctx));

	result(r);
	return AI_OK;
}

term_t bif_fun_info2(term_t Fun, term_t What, process_t *ctx)
{
	term_t r;

	if (!is_fun(Fun) || !is_atom(What))
		return A_BADARG;

	if (What == A_MODULE)
		r = make_tuple2(A_MODULE, fun_amod(Fun), proc_gc_pool(ctx));
	else if (What == A_NAME)
		r = make_tuple2(A_NAME, fun_afun(Fun), proc_gc_pool(ctx));
	else if (What == A_ARITY)
	{
		//TODO: report arity without free vars
		int n = int_value2(fun_arity(Fun));
		int nfree = int_value2(tup_size(fun_fridge(Fun)));
	
		r = make_tuple2(A_ARITY, intnum(n-nfree), proc_gc_pool(ctx));
	}
	else if (What == A_ENV)
	{
		//TODO: will be gone when fridge is a list

		term_t fridge = fun_fridge(Fun);

		term_t f = nil;
		term_t cons = nil;
		int i, n;

		n = int_value2(tup_size(fridge));
		for (i = 0; i < n; i++)
			lst_add(f, cons, tup_elts(fridge)[i], proc_gc_pool(ctx));

		r = make_tuple2(A_ENV, f, proc_gc_pool(ctx));
	}
	else
		r = make_tuple2(What, A_UNDEFINED, proc_gc_pool(ctx));

	result(r);
	return AI_OK;
}

term_t bif_size1(term_t TupleOrBin, process_t *ctx)
{
	if (is_tuple(TupleOrBin))
		result(tup_size(TupleOrBin));
	else if (is_binary(TupleOrBin))
		result(bin_size(TupleOrBin));
	else
		return A_BADARG;
	return AI_OK;
}

term_t bif_bit_size1(term_t Bin, process_t *ctx)
{
	if (!is_binary(Bin))
		return A_BADARG;
	//TODO: may overflow, one day
	result(intnum(int_value(bin_size(Bin))*8));
	return AI_OK;
}

term_t bif_element2(term_t N, term_t Tuple, process_t *ctx)
{
	int i;
	if (!is_int(N) || !is_tuple(Tuple))
		return A_BADARG;
	i = int_value2(N);
	if (i <= 0 || i > int_value2(tup_size(Tuple)))
		return A_BADARG;
	
	result(tup_elts(Tuple)[i-1]);
	return AI_OK;
}

term_t bif_setelement3(term_t N, term_t Tuple, term_t Value, process_t *ctx)
{
	term_t t2;
	int i, j, arity;
	if (!is_int(N) || !is_tuple(Tuple))
		return A_BADARG;
	i = int_value2(N);
	arity = int_value2(tup_size(Tuple));
	if (i <= 0 || i > arity)
		return A_BADARG;
	
	t2 = make_tuple(arity, proc_gc_pool(ctx));
	for (j = 0; j < arity; j++)
		tup_elts(t2)[j] = tup_elts(Tuple)[j];
	tup_elts(t2)[i-1] = Value;

	result(t2);
	return AI_OK;
}

term_t bif_length1(term_t List, process_t *ctx)
{
	if (!is_list(List))
		return A_BADARG;
	result(intnum(lst_len(List)));
	return AI_OK;
}

term_t bif_tl1(term_t List, process_t *ctx)
{
	if (!is_cons(List))
		return A_BADARG;
	result(lst_next(List));
	return AI_OK;
}

term_t bif_hd1(term_t List, process_t *ctx)
{
	if (!is_cons(List))
		return A_BADARG;
	result(lst_value(List));
	return AI_OK;
}

term_t bif_float1(term_t Number, process_t *ctx)
{
	if (is_float(Number))
		result(Number);
	else if (is_int(Number))
		result(make_float((double)int_value(Number), proc_gc_pool(ctx)));
	else if (is_bignum(Number))
		result(make_float(bignum_to_double(bn_value(Number)), proc_gc_pool(ctx)));
	else
		return A_BADARG;
	return AI_OK;
}

term_t bif_round1(term_t Number, process_t *ctx)
{
	if (is_float(Number))
	{
		double d = dbl_value(Number);
		Number = (d >= 0)
			? intnum((int)(d + 0.5))
			: intnum((int)(d - 0.5));
	}
	else if (!is_int(Number))
		return A_BADARG;

	result(Number);
	return AI_OK;
}

term_t bif_trunc1(term_t Number, process_t *ctx)
{
	if (is_float(Number))
		Number = intnum((int)dbl_value(Number));
	else if (!is_int(Number))
		return A_BADARG;

	result(Number);
	return AI_OK;
}

term_t bif_atom_to_list1(term_t Atom, process_t *ctx)
{
	apr_pool_t *tmp;
	cstr_t *print_name;
	const char *z;
	if (!is_atom(Atom))
		return A_BADARG;
	apr_pool_create(&tmp, 0);
	print_name = atoms_get(proc_atoms(ctx), index(Atom));
	z = stoz(print_name, tmp);
	result(ztol(z, proc_gc_pool(ctx)));
	apr_pool_destroy(tmp);
	return AI_OK;
}

term_t bif_list_to_atom1(term_t List, process_t *ctx)
{
	int i, len;
	cstr_t *print_name;
	term_t l = List;
	if (!is_list(List))
		return A_BADARG;
	len = lst_len(List);
	if (len > 255)
		return A_BADARG;
	print_name = xalloc(proc_gc_pool(ctx), len+1);
	for (i = 0; i < len; i++)
	{
		term_t v = lst_value(l);
		int ch;
		if (!is_int(v))
			return A_BADARG;
		ch = int_value2(v);
		if (ch < 0 || ch > 255)
			return A_BADARG;
		print_name->data[i] = ch;
		l = lst_next(l);
	}
	print_name->size = len;
	result(atom(atoms_set(proc_atoms(ctx), print_name)));
	return AI_OK;
}

term_t bif_split_binary2(term_t Bin, term_t Index, process_t *ctx)
{
	int k;
	int size;
	term_t bin1, bin2;
	if (!is_binary(Bin) || !is_int(Index))
		return A_BADARG;
	k = int_value2(Index);
	size = int_value2(bin_size(Bin));
	if (k < 0 || k > size)
		return A_BADARG;

	bin1 = make_binary(intnum(k), bin_data(Bin), proc_gc_pool(ctx));
	bin2 = make_binary(intnum(size-k), bin_data(Bin)+k, proc_gc_pool(ctx));

	result(make_tuple2(bin1, bin2, proc_gc_pool(ctx)));
	return AI_OK;
}

term_t bif_binary_to_term1(term_t Bin, process_t *ctx)
{
	int size;
	term_t t;
	atom_cache_t *cache;
	if (!is_binary(Bin))
		return A_BADARG;
	cache = atom_cache_make(proc_gc_pool(ctx));
	size = int_value2(bin_size(Bin));
	t = unpack_term(bin_data(Bin), size, cache, proc_atoms(ctx), proc_gc_pool(ctx));
	if (t == AI_UNDEFINED)
		return A_BADARG;
	result(t);
	return AI_OK;
}

term_t bif_term_to_binary1(term_t Term, process_t *ctx)
{
	apr_byte_t *data;
	atom_cache_t *cache = atom_cache_make(proc_gc_pool(ctx));
	apr_array_header_t *pad = pack_term(Term, cache, proc_atoms(ctx));

	data = xalloc(proc_gc_pool(ctx), pad->nelts);
	memcpy(data, pad->elts, pad->nelts);

	result(make_binary(intnum(pad->nelts), data, proc_gc_pool(ctx)));
	apr_pool_destroy(pad->pool);
	return AI_OK;
}

term_t bif_binary_to_list3(term_t Bin, term_t BegInd, term_t EndInd, process_t *ctx)
{
	term_t r = nil;
	term_t cons = nil;
	int beg, end, size;
	apr_byte_t *data;
	int i;

	if (!is_binary(Bin))
		return A_BADARG;
	if (!is_int(BegInd) || !is_int(EndInd))
		return A_BADARG;

	beg = int_value2(BegInd);
	end = int_value2(EndInd);
	size = int_value2(bin_size(Bin));
	data = bin_data(Bin);

	if (beg < 1 || end > size || end < beg)
		return A_BADARG;

	for (i = beg-1; i < end; i++)
		lst_add(r, cons, intnum(data[i]), proc_gc_pool(ctx));

	result(r);
    return AI_OK;
}

//NB: Must work for iolists

int iolist_len(term_t list, int len)
{
	while (is_cons(list))
	{
		term_t v = lst_value(list);
		if (is_int(v))
		{
			if (int_value(v) < 0 || int_value(v) > 255)
				return -1;
			len++;
		}
		else if (is_binary(v))
		{
			len += int_value2(bin_size(v));
		}
		else if (is_list(v))
		{
			len = iolist_len(v, len);
			if (len < 0)
				return len;
		}
		else
			return -1;
		list = lst_next(list);
	}

	if (!is_nil(list))
		return -1;

	return len;
}

apr_byte_t *flatten_iolist(term_t list, apr_byte_t *data)
{
	while (is_cons(list))
	{
		term_t v = lst_value(list);
		if (is_int(v))
			*data++ = (apr_byte_t)int_value(v);
		else if (is_binary(v))
		{
			int size = int_value2(bin_size(v));
			memcpy(data, bin_data(v), size);
			data += size;
		}
		else if (is_list(v))
			data = flatten_iolist(v, data);
		list = lst_next(list);
	}
	return data;
}

term_t bif_list_to_binary1(term_t List, process_t *ctx)
{
	int len;
	apr_byte_t *data;
	if (!is_list(List))
		return A_BADARG;

	// Optimization: if the list is made of a single binary
	// then just return that binary
	if (!is_nil(List) &&
		 is_nil(lst_next(List)) &&
		 is_binary(lst_value(List)))
	{
		result(lst_value(List));
		return AI_OK;
	}

	len = iolist_len(List, 0);
	if (len < 0)
		return A_BADARG;

	data = xalloc(proc_gc_pool(ctx), len);
	flatten_iolist(List, data);
	result(make_binary(intnum(len), data, proc_gc_pool(ctx)));
	return AI_OK;
}

term_t bif_tuple_to_list1(term_t Tuple, process_t *ctx)
{
	term_t r = nil;
	term_t cons = nil;
	int i;

	if (!is_tuple(Tuple))
		return A_BADARG;

	for (i = 0; i < int_value2(tup_size(Tuple)); i++)
		lst_add(r, cons, tup_elts(Tuple)[i], proc_gc_pool(ctx));

	result(r);
	return AI_OK;
}

term_t bif_list_to_tuple1(term_t List, process_t *ctx)
{
	int i, n;
	term_t t;
	if (!is_list(List) || !is_proper_list(List))
		return A_BADARG;
	n = lst_len(List);
	t = make_tuple(n, proc_gc_pool(ctx));
	for (i = 0; i < n; i++)
	{
		tup_elts(t)[i] = lst_value(List);
		List = lst_next(List);
	}
	// TODO: wierd lists?
	result(t);
	return AI_OK;
}

// TODO: only small ints2
term_t bif_integer_to_list1(term_t N, process_t *ctx)
{
	apr_pool_t *tmp;
	char *buf;
	if (!is_int(N))
		return A_BADARG;
	apr_pool_create(&tmp, 0);
	buf = apr_itoa(tmp, int_value(N));
	result(ztol(buf, proc_gc_pool(ctx)));
	apr_pool_destroy(tmp);
	return AI_OK;
}

term_t bif_float_to_list1(term_t Number, process_t *ctx)
{
	apr_pool_t *tmp;
	char *p;
	if (!is_float(Number))
		return A_BADARG;
	apr_pool_create(&tmp, 0);
	p = apr_psprintf(tmp, "%.20e", dbl_value(Number));
	result(ztol(p, proc_gc_pool(ctx)));
	apr_pool_destroy(tmp);
	return AI_OK;
}

// XXX: small ints only
//term_t bif_list_to_integer1(term_t Chars, process_t *ctx)
//{
//	apr_pool_t *tmp;
//	const char *buf;
//	apr_int64_t value;
//	if (!is_list(Chars))
//		return A_BADARG;
//	apr_pool_create(&tmp, 0);
//	buf = ltoz(Chars, tmp);
//	value = apr_atoi64(buf);
//	if (value < MIN_INT_VALUE || value > MAX_INT_VALUE)
//	{
//		apr_pool_destroy(tmp);
//		return A_BADARG;
//	}
//	result(intnum(value));
//	apr_pool_destroy(tmp);
//	return AI_OK;
//}

term_t bif_list_to_float1(term_t Chars, process_t *ctx)
{
	apr_pool_t *tmp;
	const char *buf, *endptr;
	double d;

	if (!is_string(Chars))
		return A_BADARG;

	apr_pool_create(&tmp, 0);
	buf = ltoz(Chars, tmp);
	
	// decimal point must be there
	if (strchr(buf, '.') == 0)
	{
		apr_pool_destroy(tmp);
		return A_BADARG;
	}	
	
	d = strtod(buf, &endptr);
	if (endptr == buf)
	{
		apr_pool_destroy(tmp);
		return A_BADARG;
	}

	result(make_float(d, proc_gc_pool(ctx)));
	apr_pool_destroy(tmp);
    return AI_OK;
}

term_t bif_make_tuple2(term_t N, term_t InitValue, process_t *ctx)
{
	term_t t;
	int i, arity;
	if (!is_int(N))
		return A_BADARG;
	arity = int_value2(N);
	if (arity < 0)
		return A_BADARG;
	t = make_tuple(arity, proc_gc_pool(ctx));
	for (i = 0; i < arity; i++)
		tup_elts(t)[i] = InitValue;
	result(t);
	return AI_OK;
}

term_t bif_append_element2(term_t Tuple, term_t Elem, process_t *ctx)
{
	int i, n;
	term_t t;

	if (!is_tuple(Tuple))
		return A_BADARG;

	n = int_value2(tup_size(Tuple));
	t = make_tuple(n+1, proc_gc_pool(ctx));

	for (i = 0; i < n; i++)
		tup_elts(t)[i] = tup_elts(Tuple)[i];
	tup_elts(t)[n] = Elem;

	result(t);
	return AI_OK;
}

term_t bif_prp_triple1(term_t PidRefPort, process_t *ctx)
{
	term_t r;
	if (!is_prp(PidRefPort))
		return A_BADARG;

	if (is_pid(PidRefPort))
		r = make_tuple3(pid_node(PidRefPort),
			intnum(pid_serial(PidRefPort)),
			intnum(pid_creation(PidRefPort)), proc_gc_pool(ctx));
	else
		r = make_tuple3(prp_node(PidRefPort),
			intnum(prp_serial(PidRefPort)),
			intnum(prp_creation(PidRefPort)), proc_gc_pool(ctx));

	result(r);
	return AI_OK;
}

term_t bif_phash2(term_t Term, term_t Range, process_t *ctx)
{
	apr_uint32_t rng, h;
	
	if (!is_int(Range) && !is_bignum(Range))
		return A_BADARG;
	if (is_int(Range))
	{
		if (int_value(Range) > 0xffffffffl)
			return A_BADARG;
		rng = (apr_uint32_t)int_value(Range);
	}
	else
	{
		bignum_t *bn = bn_value(Range);
		if (bn_size(bn) > 1)
			return A_BADARG;
		rng = bn->digits[0];
	}

	h = hash_term(Term, rng, proc_atoms(ctx));

	// hash_term returns value within 0..rng-1
	// phash should return 1..rng

	if (h > MAX_INT_VALUE-1)
	{
		bignum_t *a = bignum_from32(h, proc_gc_pool(ctx));
		result(bignum(bignum_add1(a, 1, proc_gc_pool(ctx))));
	}
	else
		result(intnum(h+1));
	return AI_OK;
}

term_t bif_register2(term_t RegName, term_t Pid, process_t *ctx)
{
	process_t *proc;
	if (!is_atom(RegName) || !is_pid(Pid))
		return A_BADARG;
	proc = proc_lookup(pid_serial(Pid));
	if (proc == 0)
		return A_ENOPROC;
	if (!proc_register(proc, RegName))
		return A_BADARG;
	result(A_TRUE);
	return AI_OK;
}

term_t bif_unregister1(term_t RegName, process_t *ctx)
{
	if (!is_atom(RegName))
		return A_BADARG;
	if (!proc_unregister(RegName))
		return A_BADARG;
	result(A_TRUE);
	return AI_OK;
}

term_t bif_whereis1(term_t RegName, process_t *ctx)
{
	process_t *proc;
	
	if (!is_atom(RegName))
		return A_BADARG;
	proc = proc_registered(RegName);
	if (proc == 0)
		result(A_UNDEFINED);
	else
		result(proc_pid(proc, proc_gc_pool(ctx)));
	return AI_OK;
}

term_t bif_registered0(process_t *ctx)
{
	term_t r = proc_list_registered(proc_gc_pool(ctx));
	result(r);
	return AI_OK;
}

term_t bif_node1(term_t PidRefPort, process_t *ctx)
{
	if (!is_pid(PidRefPort) && !is_ref(PidRefPort) && !is_port(PidRefPort))
		return A_BADARG;
	if (is_pid(PidRefPort))
		result(pid_node(PidRefPort));
	else
		result(prp_node(PidRefPort));
	return AI_OK;
}

term_t bif_node0(process_t *ctx)
{
	result(my_node);
	return AI_OK;
}

// for the first time the node can be set to any atom (except 'local')
// subsequently, set_node may be set to 'local', next time it can
// only be set to the same value as the first time, and on it goes

term_t bif_set_node1(term_t Node, process_t *ctx)
{
	if (!is_atom(Node))
		return A_BADARG;

	if (my_node == A_LOCAL && my_prev_node == A_LOCAL)
	{
		// the first assignment
		if (Node == A_LOCAL)
			return A_BADARG;
		my_node = Node;
	}
	else
	{
		// subsequent assignments
		if (my_node == A_LOCAL && Node != my_prev_node)
			return A_BADARG;
		if (my_node != A_LOCAL && Node != A_LOCAL)
			return A_BADARG;

		my_prev_node = my_node;
		my_node = Node;
	}

	result(A_OK);
	return AI_OK;
}

term_t bif_is_local_node1(term_t Node, process_t *ctx)
{
	if (!is_atom(Node))
		return A_BADARG;
	if (Node == my_node || Node == my_prev_node)
		result(A_TRUE);
	else
		result(A_FALSE);
	return AI_OK;
}

term_t bif_open_port2(term_t Term, term_t Options, process_t *ctx)
{
	apr_pool_t *port_pool;
	port_t *port;
	term_t id;

	//TODO: check for incompatible options
	if (is_tuple(Term) && tup_size(Term) == intnum(2) && tup_elts(Term)[0] == A_SPAWN)
	{
		apr_status_t rs;
		const char *progname;
		apr_procattr_t *pa;
		apr_proc_t extproc;

		if (!is_string(tup_elts(Term)[1]))
			return A_BADARG;

		apr_pool_create(&port_pool, 0);

		progname = ltoz(tup_elts(Term)[1], port_pool);

		//set various options through procattr
		rs = apr_procattr_create(&pa, port_pool);

		//pipe back stdin and stdout
		if (rs == 0)
			rs = apr_procattr_io_set(pa, APR_CHILD_BLOCK, APR_CHILD_BLOCK, APR_NO_PIPE);

		//XXX: use APR_PROGRAM instead
		if (rs == 0)
			rs = apr_procattr_cmdtype_set(pa, APR_SHELLCMD);

		if (rs == 0)
			rs = apr_proc_create(&extproc, progname, 0, 0, pa, port_pool);
		
		if (rs != 0)
		{
			apr_pool_destroy(port_pool);
			return decipher_status(rs);
		}

		//create port and register it
		port = port_pipe_make(extproc.out, extproc.in, port_pool);

		//set initial port owner
		port->owner_in = port->owner_out = proc_pid(ctx, port->xp);

		//put port to polling ring
		port_register(port);

		id = make_port(my_node, port->key, my_creation, proc_gc_pool(ctx));
		result(id);
	}
	else
		return A_BADARG;

	return AI_OK;
}

term_t bif_set_port_option3(term_t Port, term_t Opt, term_t Value, process_t *ctx)
{
	apr_status_t rs;
	port_t *port;
	if (!is_port(Port) || !is_atom(Opt))
		return A_BADARG;
	port = port_lookup(prp_serial(Port));
	if (port)
	{
		rs = port_set_option(port, Opt, Value);
		if (rs != 0)
			return decipher_status(rs);
	}
	result(A_OK);
	return AI_OK;
}

term_t bif_ports0(process_t *ctx)
{
	term_t ports = port_get_list(proc_gc_pool(ctx));
	result(ports);
	return AI_OK;
}

term_t bif_close_port1(term_t Port, process_t *ctx)
{
	port_t *p;
	if (!is_port(Port))
		return A_BADARG;
	p = port_lookup(prp_serial(Port));
	if (p)
		port_close(p);
	result(A_TRUE);
	return AI_OK;

}

term_t bif_process_info2(term_t Pid, term_t What, process_t *ctx)
{
	process_t *proc;
	term_t info;
	if (!is_local_pid(Pid) || !is_atom(What))
		return A_BADARG;
	proc = proc_lookup(pid_serial(Pid));
	if (proc == 0)
		return A_ENOPROC;
	info = proc_get_info(proc, What);
	if (info == AI_UNDEFINED)
		result(A_UNDEFINED);
	else
	{
		term_t info2 = marshal_term(info, proc_gc_pool(ctx));
		result(make_tuple2(What, info2, proc_gc_pool(ctx)));
	}
	return AI_OK;
}

term_t bif_port_info2(term_t Port, term_t What, process_t *ctx)
{
	term_t node;
	term_t value;
	port_t *port;
	if (!is_port(Port) || !is_atom(What))
		return A_BADARG;
	node = prp_node(Port);
	if (node != my_node && node != my_prev_node)
		return A_BADARG;
	port = port_lookup(prp_serial(Port));
	if (port == 0)
		return A_ENOPROC;	// should it be enoport?
	value = port_get_info(port, What);
	if (value == A_UNDEFINED)
		result(A_UNDEFINED);
	else
	{
		term_t value2 = marshal_term(value, proc_gc_pool(ctx));
		result(make_tuple2(What, value2, proc_gc_pool(ctx)));
	}
	return AI_OK;
}

// marshal both ways
term_t bif_process_flag3(term_t Pid, term_t What, term_t Value, process_t *ctx)
{
	process_t *proc;
	term_t old;
	if (!is_pid(Pid) || !is_atom(What))
		return A_BADARG;
	if (is_ptr(Value))
		return A_BADARG;	//process flags are not gc'ed
	proc = proc_lookup(pid_serial(Pid));
	old = proc_set_flag(proc, What, marshal_term(Value, proc_gc_pool(proc)));
	if (old == AI_UNDEFINED)
		result(A_UNDEFINED);
	else
		result(marshal_term(old, proc_gc_pool(ctx)));
	return AI_OK;
}

term_t bif_garbage_collect0(process_t *ctx)
{
	proc_run_gc(ctx);
	result(A_TRUE);
	return AI_OK;
}

term_t bif_garbage_collect1(term_t Pid, process_t *ctx)
{
	process_t *proc;
	if (!is_pid(Pid))
		return A_BADARG;
	proc = proc_lookup(pid_serial(Pid));
	if (proc == 0)
		result(A_FALSE);
	else
	{
		proc_run_gc(proc);
		result(A_TRUE);
	}
	return AI_OK;
}

term_t bif_get_stacktrace0_1(term_t Pid, process_t *ctx)
{
	process_t *proc;
	if (!is_pid(Pid))
		return A_BADARG;
	proc = proc_lookup(pid_serial(Pid));
	if (proc == 0)
		result(A_FALSE);
	else
		result(proc_get_stacktrace(proc));
	return AI_OK;
}

term_t bif_get_locals1(term_t Pid, process_t *ctx)
{
	process_t *proc;
	if (!is_pid(Pid))
		return A_BADARG;
	proc = proc_lookup(pid_serial(Pid));
	if (proc == 0)
		result(A_FALSE);
	else
		result(proc_get_locals(proc));
	return AI_OK;
}

term_t bif_now0(process_t *ctx)
{
	//{MegaSecs, Secs, Microsecs}
	static apr_time_t last_now = 0; 
	apr_time_t now = apr_time_now();
	term_t t;

	if (now == last_now)
		now++;	// provide uniqueness of results
	last_now = now;

	t = make_tuple3(intnum(now/1000000/1000000),
		intnum((now/1000000) % 1000000),
		intnum(now % 1000000),
		proc_gc_pool(ctx));
	result(t);
    return AI_OK;
}

//date() -> {Year, Month, Day}
term_t bif_date0(process_t *ctx)
{
	apr_status_t rs;
	apr_time_t now = apr_time_now();
	apr_time_exp_t exp;

	rs = apr_time_exp_lt(&exp, now);
	if (rs != 0)
		return decipher_status(rs);

    /** (1-31) day of the month */
    //apr_int32_t tm_mday;
    /** (0-11) month of the year */
    //apr_int32_t tm_mon;
    /** year since 1900 */
    //apr_int32_t tm_year;

	result(make_tuple3(intnum(exp.tm_year+1900),
		intnum(exp.tm_mon+1), intnum(exp.tm_mday), proc_gc_pool(ctx)));

	return AI_OK;
}

//time() -> {Hour, Minute, Second}
term_t bif_time0(process_t *ctx)
{
	apr_status_t rs;
	apr_time_t now = apr_time_now();
	apr_time_exp_t exp;

	rs = apr_time_exp_lt(&exp, now);
	if (rs != 0)
		return decipher_status(rs);

	/** (0-61) seconds past tm_min */
    //apr_int32_t tm_sec;
    /** (0-59) minutes past tm_hour */
    //apr_int32_t tm_min;
    /** (0-23) hours past midnight */
    //apr_int32_t tm_hour;

	result(make_tuple3(intnum(exp.tm_hour),
		intnum(exp.tm_min), intnum(exp.tm_sec), proc_gc_pool(ctx)));

	return AI_OK;
}

//localtime() -> {Date, Time}
term_t bif_localtime0(process_t *ctx)
{
	apr_status_t rs;
	apr_time_t now = apr_time_now();
	apr_time_exp_t exp;
	term_t my_date, my_time;

	rs = apr_time_exp_lt(&exp, now);
	if (rs != 0)
		return decipher_status(rs);

	my_time = make_tuple3(intnum(exp.tm_hour),
		intnum(exp.tm_min), intnum(exp.tm_sec), proc_gc_pool(ctx));

	my_date = make_tuple3(intnum(exp.tm_year+1900),
		intnum(exp.tm_mon+1), intnum(exp.tm_mday), proc_gc_pool(ctx));

	result(make_tuple2(my_date, my_time, proc_gc_pool(ctx)));

	return AI_OK;
}

//universaltime() -> {Date, Time}
term_t bif_universaltime0(process_t *ctx)
{
	apr_status_t rs;
	apr_time_t now = apr_time_now();
	apr_time_exp_t exp;
	term_t my_date, my_time;

	rs = apr_time_exp_gmt(&exp, now);
	if (rs != 0)
		return decipher_status(rs);

	my_time = make_tuple3(intnum(exp.tm_hour),
		intnum(exp.tm_min), intnum(exp.tm_sec), proc_gc_pool(ctx));

	my_date = make_tuple3(intnum(exp.tm_year+1900),
		intnum(exp.tm_mon+1), intnum(exp.tm_mday), proc_gc_pool(ctx));

	result(make_tuple2(my_date, my_time, proc_gc_pool(ctx)));

	return AI_OK;
}

term_t bif_get0(process_t *ctx)
{
	term_t dict = proc_dict(ctx);
	result(dict);
	return AI_OK;
}

term_t bif_put1(term_t Dict, process_t *ctx)
{
	if (!is_list(Dict))
		return A_BADARG;
	proc_set_dict(ctx, Dict);
	result(Dict);
	return AI_OK;
}

term_t bif_make_ref0(process_t *ctx)
{
	static int next_ref = 0;
	term_t ref = make_ref(my_node, next_ref++, my_creation, proc_gc_pool(ctx));
	result(ref);
	return AI_OK;
}

term_t bif_make_ref3(term_t Node, term_t Serial, term_t Creation, process_t *ctx)
{
	if (!is_atom(Node) || !is_int(Serial) || !is_int(Creation))
		return A_BADARG;
	result(make_ref(Node, int_value(Serial), int_value(Creation), proc_gc_pool(ctx)));
	return AI_OK;
}

term_t bif_make_pid3(term_t Node, term_t Serial, term_t Creation, process_t *ctx)
{
	if (!is_atom(Node) || !is_int(Serial) || !is_int(Creation))
		return A_BADARG;
	result(make_pid(Node, int_value(Serial), int_value(Creation), proc_gc_pool(ctx)));
	return AI_OK;
}

term_t bif_make_port3(term_t Node, term_t Serial, term_t Creation, process_t *ctx)
{
	if (!is_atom(Node) || !is_int(Serial) || !is_int(Creation))
		return A_BADARG;
	result(make_port(Node, int_value(Serial), int_value(Creation), proc_gc_pool(ctx)));
	return AI_OK;
}

term_t bif_daemonize0(process_t *ctx)
{
	apr_proc_detach(APR_PROC_DETACH_DAEMONIZE);
	result(A_TRUE);
	return AI_OK;
}

//EOF
