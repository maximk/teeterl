//
//
//

#include "bifimpl.h"

#include <apr_strings.h>
#include <apr_thread_proc.h>

#include "binary.h"
#include "list.h"
#include "exterm.h"
#include "code_base.h"
#include "scheduler.h"

#include <stdlib.h>

const char *stringify_term(term_t t, atoms_t *atoms, apr_pool_t *pool);

int iolist_len(term_t list, int len);
apr_byte_t *flatten_iolist(term_t list, apr_byte_t *data);

// erlang:get/1 [13]
term_t bif_get1(term_t Key, proc_t *proc)
{
	dict_pair_t *ptr = (dict_pair_t *)proc->dictionary->elts;
	dict_pair_t *end = ptr + proc->dictionary->nelts;
	while (ptr < end)
	{
		if (are_terms_equal(ptr->key, Key))
			return ptr->value;
		ptr++;
	}

	return A_UNDEFINED;
}

// erlang:get/0 [14]
term_t bif_get0(proc_t *proc)
{
	term_t first = nil;
	term_t last = nil;

	dict_pair_t *ptr = (dict_pair_t *)proc->dictionary->elts;
	dict_pair_t *end = ptr + proc->dictionary->nelts;
	while (ptr < end)
	{
		term_t pair = heap_tuple2(proc->heap, ptr->key, ptr->value);
		cons_up(first, last, pair, proc->heap);
		ptr++;
	}

	return first;
}

// erlang:erase/1 [15]
term_t bif_erase1(term_t Key, proc_t *proc)
{
	dict_pair_t *ptr = (dict_pair_t *)proc->dictionary->elts;
	dict_pair_t *end = ptr + proc->dictionary->nelts;
	while (ptr < end)
	{
		if (are_terms_equal(ptr->key, Key))
		{
			term_t value = ptr->value;
			memmove(ptr, ptr+1, (end-ptr-1)*sizeof(dict_pair_t));
			proc->dictionary->nelts--;
			return value;
		}
		ptr++;
	}

	return A_UNDEFINED;
}

// erlang:erase/0 [16]
term_t bif_erase0(proc_t *proc)
{
	term_t first = nil;
	term_t last = nil;

	dict_pair_t *ptr = (dict_pair_t *)proc->dictionary->elts;
	dict_pair_t *end = ptr + proc->dictionary->nelts;
	while (ptr < end)
	{
		term_t pair = heap_tuple2(proc->heap, ptr->key, ptr->value);
		cons_up(first, last, pair, proc->heap);
		ptr++;
	}

	proc->dictionary->nelts = 0;
	return first;
}

// erlang:put/2 [17]
term_t bif_put2(term_t Key, term_t Value, proc_t *proc)
{
	dict_pair_t *ptr = (dict_pair_t *)proc->dictionary->elts;
	dict_pair_t *end = ptr + proc->dictionary->nelts;
	while (ptr < end)
	{
		if (are_terms_equal(ptr->key, Key))
		{
			term_t old_value = ptr->value;
			ptr->value = Value;
			return old_value;
		}
		ptr++;
	}

	ptr = (dict_pair_t *)apr_array_push(proc->dictionary);
	ptr->key = Key;
	ptr->value = Value;
	return A_UNDEFINED;
}

// erlang:make_tuple/3 [33]
term_t bif_make_tuple3(term_t N, term_t DefValue, term_t InitList, proc_t *proc)
{
	term_t cons;
	int i, size;
	term_t tuple;
	term_box_t *tb;
	if (!is_int(N))
		bif_bad_arg(N);
	if (!is_list(InitList))
		bif_bad_arg(InitList);
	size = int_value(N);
	tuple = heap_tuple(proc->heap, size);
	tb = peel(tuple);
	for (i = 0; i < size; i++)
		tb->tuple.elts[i] = DefValue;
	cons = InitList;
	while (is_cons(cons))
	{
		term_box_t *cb = peel(cons);
		term_box_t *ib;
		int index;
		if (!is_tuple(cb->cons.head))
			bif_bad_arg(InitList);
		ib = peel(cb->cons.head);
		if (ib->tuple.size != 2 || !is_int(ib->tuple.elts[0]))
			bif_bad_arg(InitList);
		index = int_value(ib->tuple.elts[0]);
		if (index < 1 || index > size)
			bif_bad_arg(InitList);
		tb->tuple.elts[index-1] = ib->tuple.elts[1];
		cons = cb->cons.tail;
	}

	if (!is_nil(cons))
		bif_bad_arg(InitList);

	return tuple;
}

// erlang:make_tuple/2 [34]
term_t bif_make_tuple2(term_t N, term_t InitValue, proc_t *proc)
{
	int i, size;
	term_t tuple;
	term_box_t *tb;
	if (!is_int(N))
		bif_bad_arg(N);
	size = int_value(N);
	tuple = heap_tuple(proc->heap, size);
	tb = peel(tuple);
	for (i = 0; i < size; i++)
		tb->tuple.elts[i] = InitValue;
	return tuple;
}

// erlang:list_to_tuple/1 [32]
term_t bif_list_to_tuple1(term_t List, proc_t *proc)
{
	int i, size;
	term_t tuple;
	term_box_t *tb;
	term_t cons;
	if (!is_list(List))
		bif_bad_arg(List);
	size = list_length(List);
	tuple = heap_tuple(proc->heap, size);
	tb = peel(tuple);
	cons = List;
	i = 0;
	while (is_cons(cons))
	{
		term_box_t *cb = peel(cons);
		tb->tuple.elts[i++] = cb->cons.head;
		cons = cb->cons.tail;
	}

	if (!is_nil(cons))
		bif_bad_arg(List);

	return tuple;
}

// erlang:tuple_to_list/1 [33]
term_t bif_tuple_to_list1(term_t Tuple, proc_t *proc)
{
	term_box_t *tb;
	term_t first = nil;
	term_t last = nil;
	int i;
	if (!is_tuple(Tuple))
		bif_bad_arg(Tuple);
	tb = peel(Tuple);
	for (i = 0; i < tb->tuple.size; i++)
		cons_up(first, last, tb->tuple.elts[i], proc->heap);
	return first;
}

// erlang:setelement/3 [?]
term_t bif_setelement3(term_t N, term_t Tuple, term_t Value, proc_t *proc)
{
	term_box_t *tb;
	term_box_t *cb;
	term_t copy;
	int index;
	if (!is_tuple(Tuple))
		bif_bad_arg(Tuple);
	tb = peel(Tuple);
	if (!is_int(N))
		bif_bad_arg(N);
	index = int_value(N);
	if (index < 1 || index > tb->tuple.size)
		bif_bad_arg(N);
	copy = heap_tuple(proc->heap, tb->tuple.size);
	cb = peel(copy);
	memcpy(cb->tuple.elts,
		tb->tuple.elts,
		tb->tuple.size*sizeof(term_t));
	cb->tuple.elts[index-1] = Value;
	return copy;
}

// erlang:append_element/2 [37]
term_t bif_append_element2(term_t Tuple, term_t Elem, proc_t *proc)
{
	term_t copy;
	term_box_t *tb1, *tb2;
	if (!is_tuple(Tuple))
		bif_bad_arg(Tuple);
	tb1 = peel(Tuple);
	copy = heap_tuple(proc->heap, tb1->tuple.size+1);
	tb2 = peel(copy);
	memcpy(tb2->tuple.elts, tb1->tuple.elts, tb1->tuple.size*sizeof(term_t));
	tb2->tuple.elts[tb1->tuple.size] = Elem;
	return copy;
}

// erlang:'++'/2 [18]
term_t bif_plusplus2(term_t List1, term_t List2, proc_t *proc)
{
	term_t first = nil;
	term_t last = nil;

	if (!is_list(List1))
		bif_bad_arg(List1);
	if (!is_list(List2))
		bif_bad_arg(List2);
	if (List1 == nil)
		return List2;
	if (List2 == nil)
		return List1;

	while (is_cons(List1))
	{
		term_box_t *cb = peel(List1);
		cons_up(first, last, cb->cons.head, proc->heap);
		List1 = cb->cons.tail;
	}

	if (!is_nil(List1))
		bif_bad_arg0();	//TODO: keep List1 value

	peel(last)->cons.tail = List2;
	return first;
}

// erlang:'--'/2 [37]
term_t bif_minusminus2(term_t List1, term_t List2, proc_t *proc)
{
	int n2;
	term_t *starred;
	int nstarred;
	term_t cons;
	term_t first = nil;
	term_t last = nil;

	// 1. find subset List2* of List2 found in List1
	// 2. put List2 into an array to remove elements easily
	// 3. scan List1 and remove (not copy) elements found in List2*

	if (!is_list(List1))
		bif_bad_arg(List1);
	if (!is_list(List2))
		bif_bad_arg(List2);
	if (List1 == nil || List2 == nil)
		return List1;

	n2 = list_length(List2);
	// allocate n2 spots for List2* probably not all of them will be in use
	starred = (term_t *)heap_alloc(proc->heap, n2*sizeof(term_t));
	nstarred = 0;

	cons = List2;
	while (is_cons(cons))
	{
		term_box_t *cb = peel(cons);
		term_t v = cb->cons.head;
		term_t cons1 = List1;
		while (is_cons(cons1))
		{
			term_box_t *cb1 = peel(cons1);
			if (are_terms_equal(cb1->cons.head, v))
			{
				starred[nstarred++] = v;
				break;
			}
			cons1 = cb1->cons.tail;
		}
		cons = cb->cons.tail;
	}

	if (!is_nil(cons))
		bif_bad_arg(List2);

	cons = List1;
	while (is_cons(cons))
	{
		term_box_t *cb = peel(cons);
		term_t v = cb->cons.head;
		int i;
		int found = 0;

		for (i = 0; i < nstarred; i++)
		{
			if (are_terms_equal(v, starred[i]))
			{
				if (i < nstarred-1)
					starred[i] = starred[nstarred-1];
				nstarred--;
				found = 1;
				break;
			}
		}

		if (!found)
			cons_up(first, last, v, proc->heap);

		cons = cb->cons.tail;
	}

	if (!is_nil(cons))
		bif_bad_arg(List1);

	assert(nstarred == 0);
	return first;
}

// erlang:term_to_binary/1 [42]
term_t bif_term_to_binary1(term_t Term, proc_t *proc)
{
	term_t bin = term_to_binary(Term, proc->teevm->atoms, proc->heap);
	if (bin == noval)
		bif_bad_arg(Term);
	return bin;
}

term_t bif_binary_to_term1(term_t Bin, proc_t *proc)
{
	term_t t;
	if (!is_binary(Bin))
		bif_bad_arg(Bin);
	t = binary_to_term(Bin, proc->teevm->atoms, proc->heap);
	if (t == noval)
		bif_bad_arg(Bin);
	return t;
}

// erlang:binary_to_list/3 [40]
term_t bif_binary_to_list3(term_t Bin, term_t Start, term_t Stop, proc_t *proc)
{
	term_box_t *bb;
	apr_byte_t *data;
	int i, size, from, to;
	term_t first = nil;
	term_t last = nil;
	if (!is_binary(Bin))
		bif_bad_arg(Bin);
	bb = peel(Bin);
	if (!is_int(Start))
		bif_bad_arg(Start);
	if (!is_int(Stop))
		bif_bad_arg(Stop);
	from = int_value(Start);
	to = int_value(Stop);
	if (BIN_TRAILER_SIZE(bb->binary.bit_size) > 0)
		bif_bad_arg(Bin);
	data = bb->binary.data;
	size = BIN_BYTE_SIZE(bb->binary.bit_size);
	if (from <= 0 || from > to)
		bif_bad_arg(Start);
	if (to > size)
		bif_bad_arg(Stop);
	for (i = from; i <= to; i++)
		cons_up(first, last, tag_int(data[i]), proc->heap);
	return first;
}

// erlang:binary_to_list/1 [15]
term_t bif_binary_to_list1(term_t Bin, proc_t *proc)
{
	term_box_t *bb;
	apr_byte_t *data;
	int i, size;
	term_t first = nil;
	term_t last = nil;
	if (!is_binary(Bin))
		bif_bad_arg(Bin);
	bb = peel(Bin);
	if (BIN_TRAILER_SIZE(bb->binary.bit_size) > 0)
		bif_bad_arg(Bin);
	data = bb->binary.data;
	size = BIN_BYTE_SIZE(bb->binary.bit_size);
	for (i = 0; i < size; i++)
		cons_up(first, last, tag_int(data[i]), proc->heap);
	return first;
}

// erlang:float_to_list/1 [22]
term_t bif_float_to_list1(term_t Float, proc_t *proc)
{
	char buf[64];
	if (!is_float(Float))
		bif_bad_arg(Float);
	apr_snprintf(buf, sizeof(buf), "%lf", float_value(Float));
	return ztol(buf, proc->heap);
}

// erlang:atom_to_list/1 [23]
term_t bif_atom_to_list1(term_t Atom, proc_t *proc)
{
	cstr_t *s;
	if (!is_atom(Atom))
		bif_bad_arg(Atom);
	s = atoms_get(proc->teevm->atoms, atom_index(Atom));
	return stol(s, proc->heap);
}

term_t bif_register2(term_t RegName, term_t PidOid, proc_t *proc)
{
	proc_t *reg_me;
	
	if (!is_atom(RegName) || RegName == A_UNDEFINED)
		bif_bad_arg(RegName);
	if (!is_short_pid(PidOid) && !is_short_oid(PidOid))
		bif_bad_arg(PidOid);
	if (is_short_oid(PidOid))
		not_implemented("registeration of outlets by name");

	if (scheduler_lookup_by_name(proc->teevm->scheduler, RegName) != 0)
		bif_bad_arg(PidOid);

	reg_me = scheduler_lookup(proc->teevm->scheduler, pid_serial(PidOid));
	if (reg_me == 0)
		bif_bad_arg(PidOid);
	if (reg_me->reg_name != noval)
		bif_bad_arg(PidOid);

	scheduler_register(proc->teevm->scheduler, reg_me, RegName);
	return A_TRUE;
}

// erlang:whereis/1 [34]
term_t bif_whereis1(term_t RegName, proc_t *proc)
{
	proc_t *registered;
	if (!is_atom(RegName))
		bif_bad_arg(RegName);
	registered =  scheduler_lookup_by_name(proc->teevm->scheduler, RegName);
	if (registered == NULL)
		return A_UNDEFINED;
	else
		return proc_id(registered);
}

term_t bif_unregister1(term_t RegName, proc_t *proc)
{
	if (!is_atom(RegName))
		bif_bad_arg(RegName);
	if (scheduler_lookup_by_name(proc->teevm->scheduler, RegName) == 0)
		bif_bad_arg(RegName);
	scheduler_unregister(proc->teevm->scheduler, RegName);
	return A_TRUE;
}

term_t bif_spawn3(term_t Module, term_t Function, term_t Args, proc_t *proc)
{
	module_t *m;
	codel_t *entry;
	proc_t *spawning;

	if (!is_atom(Module) || !is_atom(Function) || !is_list(Args))
		bif_bad_arg0();

	spawning = proc_make(proc->teevm);

	m = code_base_lookup(proc->teevm->base, Module);
	if (m == 0)
	{
		m = code_base_lookup(proc->teevm->base, A_ERROR_HANDLER);
		assert(m != 0);
		entry = module_lookup(m, A_UNDEFINED_FUNCTION, 3);
		assert(entry != 0);
		spawning->capsule.module = m;
		spawning->capsule.ip = entry;
		spawning->capsule.arity = 3;
		spawning->capsule.registers[0] = Module;
		spawning->capsule.registers[1] = Function;
		spawning->capsule.registers[2] = heap_marshal(Args, spawning->heap);
	}
	else
	{
		int arity = list_length(Args);
		codel_t *entry = module_lookup(m, Function, arity);
		if (entry == 0)
		{
			term_t reason = heap_tuple4(proc->heap, A_UNDEF, Module, Function, Args);
			bif_exception(reason);
		}

		spawning->capsule.module = m;
		spawning->capsule.ip = entry;
		spawning->capsule.arity = arity;

		if (Args != nil)
		{
			int i = 0;
			term_t cons = Args;
			while (is_cons(cons))
			{
				term_box_t *cb = peel(cons);
				term_t v = heap_marshal(cb->cons.head, spawning->heap);
				spawning->capsule.registers[i++] = v;
				cons = cb->cons.tail;
			}
		}
	}

	scheduler_enlist(proc->teevm->scheduler, spawning);
	return tag_short_pid(spawning->serial);
}

// erlang:spawn/1 [2]
term_t bif_spawn1(term_t Fun, proc_t *proc)
{
	term_box_t *fb;
	int nfree;
	module_t *m;
	proc_t *spawning;
	fun_slot_t *slot;

	if (!is_fun(Fun))
		bif_bad_arg(Fun);
	fb = peel(Fun);
	nfree = list_length(fb->fun.frozen);
	if (fb->fun.arity != nfree)
		bif_bad_arg(Fun);

	spawning = proc_make(proc->teevm);

	m = code_base_lookup(proc->teevm->base, fb->fun.module);
	if (m == 0)
		not_implemented("undefined_fun");
	//{
	//	m = code_base_lookup(proc->teevm->base, A_ERROR_HANDLER);
	//	assert(m != 0);
	//	entry = module_lookup(m, A_UNDEFINED_FUNCTION, 3);
	//	assert(entry != 0);
	//	spawning->capsule.module = m;
	//	spawning->capsule.ip = entry;
	//	spawning->capsule.arity = 3;
	//	spawning->capsule.registers[0] = Module;
	//	spawning->capsule.registers[1] = Function;
	//	spawning->capsule.registers[2] = heap_marshal(fb->fun.frozen, spawning.heap);
	//}

	slot = &m->funs[fb->fun.index];

	if (slot->uniq != fb->fun.uniq)
		bif_exception(A_STALE_FUN);

	spawning->capsule.module = m;
	spawning->capsule.ip = slot->entry;
	spawning->capsule.arity = nfree;

	if (fb->fun.frozen != nil)
	{
		int i = 0;
		term_t cons = fb->fun.frozen;
		while (is_cons(cons))
		{
			term_box_t *cb = peel(cons);
			term_t v = heap_marshal(cb->cons.head, spawning->heap);
			spawning->capsule.registers[i++] = v;
			cons = cb->cons.tail;
		}
	}

	scheduler_enlist(proc->teevm->scheduler, spawning);
	return tag_short_pid(spawning->serial);
}

// erlang:'!'/2 [2]
term_t bif_send_msg2(term_t PidOid, term_t Msg, proc_t *proc)
{
	int serial;
	proc_t *addressee;
	outlet_t *ol;

	if (is_short_pid(PidOid))
	{
		serial = pid_serial(PidOid);
		addressee = scheduler_lookup(proc->teevm->scheduler, serial);
		if (addressee != 0)
		{
			term_t m = heap_marshal(Msg, addressee->heap);
			scheduler_new_local_mail(proc->teevm->scheduler, addressee, m);
		}
	}
	else if (is_atom(PidOid))
	{
		addressee = scheduler_lookup_by_name(proc->teevm->scheduler, PidOid);
		if (addressee == 0)
			bif_bad_arg(PidOid);	// TODO: registered outlets
		else
		{
			term_t m = heap_marshal(Msg, addressee->heap);
			scheduler_new_local_mail(proc->teevm->scheduler, addressee, m);
		}
	}
	else if (is_short_oid(PidOid))
	{
		//TODO: Msg must be an io_list?

		serial = oid_serial(PidOid);
		ol = outlet_mall_lookup(proc->teevm->mall, serial);
		if (ol != 0)
			outlet_send(ol, Msg);
	}
	else if (is_long_pid(PidOid))
	{
		not_implemented("send_msg to long pid");
	}
	else if (is_long_oid(PidOid))
	{
		not_implemented("send_msg to long oid");
	}
	else
		bif_bad_arg(PidOid);

	return Msg;
}

// erlang:exit/2 [27]
term_t bif_exit2(term_t Pid, term_t Reason, proc_t *proc)
{
	scheduler_t *sched = proc->teevm->scheduler;
	proc_t *mate;
	if (!is_short_pid(Pid))
		bif_bad_arg(Pid);
	mate = scheduler_lookup(sched, pid_serial(Pid));
	if (mate == proc)
	{
		proc->result.what = SLICE_RESULT_EXIT;
		proc->result.reason = Reason;
		return noval;
	}
	else if (mate)
		scheduler_exit_process(sched, mate, Reason);
	return A_TRUE;
}

// erlang:exit/1 [5]
term_t bif_exit1(term_t Reason, proc_t *proc)
{
	proc->result.what = SLICE_RESULT_EXIT;
	proc->result.reason = Reason;
	return noval;
}

// erlang:throw/1 [6]
term_t bif_throw1(term_t Reason, proc_t *proc)
{
	proc->result.what = SLICE_RESULT_THROW;
	proc->result.reason = Reason;
	return noval;
}

// erlang:error/1 [7]
term_t bif_error1(term_t Reason, proc_t *proc)
{
	proc->result.what = SLICE_RESULT_ERROR;
	// TODO: add stack trace here
	proc->result.reason = heap_tuple2(proc->heap, Reason, nil);
	return noval;
}

// erlang:error/2 [7]
term_t bif_error2(term_t Reason, term_t Args, proc_t *proc)
{
	proc->result.what = SLICE_RESULT_ERROR;
	proc->result.reason = heap_tuple2(proc->heap, Reason, Args);
	// TODO: Args disregarded, should be used to pump up the stack trace
	return noval;
}

// erlang:close/1 [11]
term_t bif_close1(term_t Oid, proc_t *proc)
{
	outlet_t *ol;
	if (!is_short_oid(Oid))
		bif_bad_arg(Oid);

	ol = outlet_mall_lookup(proc->teevm->mall, oid_serial(Oid));
	if (ol != 0)
		outlet_close(ol);
	else
		return A_FALSE;

	return A_TRUE;
}

// erlang:property/3 [12]
term_t bif_property3(term_t Term, term_t Opt, term_t Val, proc_t *proc)
{
	if (!is_atom(Opt))
		bif_bad_arg(Opt);

	if (is_short_oid(Term))
	{
		outlet_t *ol = outlet_mall_lookup(proc->teevm->mall, oid_serial(Term));
		if (ol != 0)
		{
			outlet_set_option(ol, Opt, Val);	//TODO: rename set_option
			return A_TRUE;
		}
		else
			return A_FALSE;
	}
	else
		bif_bad_arg(Term);
}

// erlang:property/2 [13]
term_t bif_property2(term_t Term, term_t Opt, proc_t *proc)
{
	bif_bad_arg(Term);
}

// erlang:now/0 [15]
term_t bif_now0(proc_t *proc)
{
	//{MegaSecs, Secs, Microsecs}
	static apr_time_t last_now = 0; 
	apr_time_t now = apr_time_now();

	if (now == last_now)
		now++;	// provide uniqueness of results
	last_now = now;

	return heap_tuple3(proc->heap,
		tag_int(now/1000000/1000000),
		tag_int((now/1000000) % 1000000),
		tag_int(now % 1000000));
}

//date() -> {Year, Month, Day}
term_t bif_date0(proc_t *proc)
{
	apr_status_t rs;
	apr_time_t now = apr_time_now();
	apr_time_exp_t exp;

	rs = apr_time_exp_lt(&exp, now);
	if (rs != 0)
		bif_exception(decipher_status(rs));

    /** (1-31) day of the month */
    //apr_int32_t tm_mday;
    /** (0-11) month of the year */
    //apr_int32_t tm_mon;
    /** year since 1900 */
    //apr_int32_t tm_year;

	return heap_tuple3(proc->heap,
		tag_int(exp.tm_year+1900),
		tag_int(exp.tm_mon+1),
		tag_int(exp.tm_mday));
}

//time() -> {Hour, Minute, Second}
term_t bif_time0(proc_t *proc)
{
	apr_status_t rs;
	apr_time_t now = apr_time_now();
	apr_time_exp_t exp;

	rs = apr_time_exp_lt(&exp, now);
	if (rs != 0)
		bif_exception(decipher_status(rs));

	/** (0-61) seconds past tm_min */
    //apr_int32_t tm_sec;
    /** (0-59) minutes past tm_hour */
    //apr_int32_t tm_min;
    /** (0-23) hours past midnight */
    //apr_int32_t tm_hour;

	return heap_tuple3(proc->heap,
		tag_int(exp.tm_hour),
		tag_int(exp.tm_min),
		tag_int(exp.tm_sec));
}

//localtime() -> {Date, Time}
term_t bif_localtime0(proc_t *proc)
{
	apr_status_t rs;
	apr_time_t now = apr_time_now();
	apr_time_exp_t exp;
	term_t my_date, my_time;

	rs = apr_time_exp_lt(&exp, now);
	if (rs != 0)
		bif_exception(decipher_status(rs));

	my_time = heap_tuple3(proc->heap,
		tag_int(exp.tm_hour),
		tag_int(exp.tm_min),
		tag_int(exp.tm_sec));

	my_date = heap_tuple3(proc->heap,
		tag_int(exp.tm_year+1900),
		tag_int(exp.tm_mon+1),
		tag_int(exp.tm_mday));

	return heap_tuple2(proc->heap, my_date, my_time);
}

//universaltime() -> {Date, Time}
term_t bif_universaltime0(proc_t *proc)
{
	apr_status_t rs;
	apr_time_t now = apr_time_now();
	apr_time_exp_t exp;
	term_t my_date, my_time;

	rs = apr_time_exp_gmt(&exp, now);
	if (rs != 0)
		bif_exception(decipher_status(rs));

	my_time = heap_tuple3(proc->heap,
		tag_int(exp.tm_hour),
		tag_int(exp.tm_min),
		tag_int(exp.tm_sec));

	my_date = heap_tuple3(proc->heap,
		tag_int(exp.tm_year+1900),
		tag_int(exp.tm_mon+1),
		tag_int(exp.tm_mday));

	return heap_tuple2(proc->heap, my_date, my_time);
}

term_t bif_list_to_binary1(term_t List, proc_t *proc);

// erlang:display/1 [12]
term_t bif_display1(term_t Term, proc_t *proc)
{
	apr_pool_t *tmp;
	apr_pool_create(&tmp, 0);
	const char *str = stringify_term(Term, proc->teevm->atoms, tmp);
	fprintf(stderr, "%s\n", str);
	apr_pool_destroy(tmp);
	return A_OK;
}

// erlang:statistics/1
term_t bif_statistics1(term_t What, proc_t *proc)
{
	statistics_t *stats;
	if (!is_atom(What))
		bif_bad_arg(What);
	stats = scheduler_stats(proc->teevm->scheduler);
	switch (What)
	{
	case A_CONTEXT_SWITCHES:
		return heap_tuple2(proc->heap,
			tag_int(stats->context_switches),
			tag_int(0));
	case A_GARBAGE_COLLECTION:
		return heap_tuple3(proc->heap,
			tag_int(stats->gc_count),
			tag_int(stats->memory_reclaimed),
			tag_int(0));
	case A_IO:
	{
		term_t inp = heap_tuple2(proc->heap,
			A_INPUT, tag_int(stats->input_total));
		term_t out = heap_tuple2(proc->heap,
			A_OUTPUT, tag_int(stats->output_total));
		return heap_tuple2(proc->heap, inp, out);
	}
	case A_REDUCTIONS:
	{
		term_t t = heap_tuple2(proc->heap,
			tag_int(stats->reductions),
			tag_int(stats->reductions_slc));
		stats->reductions_slc = 0;
		return t;
	}
	case A_PROC_QUEUES:
	{
		int w, tw;
		int r = scheduler_proc_queues(proc->teevm->scheduler, &w, &tw);
		term_t run = heap_tuple2(proc->heap,
			A_RUN, tag_int(r));
		term_t wait = heap_tuple2(proc->heap,
			A_WAIT, tag_int(w));
		term_t timed_wait = heap_tuple2(proc->heap,
			A_TIMED_WAIT, tag_int(tw));
		return heap_tuple3(proc->heap, run, wait, timed_wait);
	}

	case A_RUNTIME:
	{
		apr_interval_time_t rt = stats->runtime;
		apr_interval_time_t rt_slc = stats->runtime_slc;
		stats->runtime_slc = 0;
		return heap_tuple2(proc->heap,
			int_to_term((long)rt, proc->heap), int_to_term((long)rt_slc, proc->heap));
	}
	case A_WALL_CLOCK:
	{
		apr_interval_time_t wc = stats->wall_clock;
		apr_interval_time_t wc_slc = stats->wall_clock_slc;
		stats->wall_clock_slc = 0;
		return heap_tuple2(proc->heap,
			int_to_term((long)wc, proc->heap), int_to_term((long)wc_slc, proc->heap));
	}
	default:
		bif_bad_arg(What);
	}
}

// erlang:list_to_binary/1 [22]
term_t bif_list_to_binary1(term_t List, proc_t *proc)
{
	int len;
	term_t bin;
	if (!is_list(List))
		bif_bad_arg(List);

	// Optimization: if the list is made of a single binary
	// then just return that binary
	if (is_cons(List))
	{
		term_box_t *cb = peel(List);
		if (is_nil(cb->cons.tail) && is_binary(cb->cons.head))
			return cb->cons.head;
	}

	len = iolist_len(List, 0);
	if (len < 0)
		bif_bad_arg(List);

	bin = heap_binary0(proc->heap, len*8);
	flatten_iolist(List, peel(bin)->binary.data);

	return bin;
}

int iolist_len(term_t list, int len)
{
	while (is_cons(list))
	{
		term_box_t *cb = peel(list);
		term_t v = cb->cons.head;
		if (is_int(v))
		{
			if (int_value(v) < 0 || int_value(v) > 255)
				return -1;
			len++;
		}
		else if (is_binary(v))
		{
			int bit_size = peel(v)->binary.bit_size;
			if (BIN_TRAILER_SIZE(bit_size) > 0)
				return -1;
			len += BIN_BYTE_SIZE(bit_size);
		}
		else if (is_list(v))
		{
			len = iolist_len(v, len);
			if (len < 0)
				return len;
		}
		else
			return -1;
		list = cb->cons.tail;
	}

	if (!is_nil(list))
		return -1;

	return len;
}

apr_byte_t *flatten_iolist(term_t list, apr_byte_t *data)
{
	while (is_cons(list))
	{
		term_box_t *cb = peel(list);
		term_t v = cb->cons.head;
		if (is_int(v))
			*data++ = (apr_byte_t)int_value(v);
		else if (is_binary(v))
		{
			term_box_t *bb = peel(v);
			int size = BIN_BYTE_SIZE(bb->binary.bit_size);
			memcpy(data, bb->binary.data, size);
			data += size;
		}
		else if (is_list(v))
			data = flatten_iolist(v, data);
		list = cb->cons.tail;
	}
	return data;
}

// erlang:daemonize/0
term_t bif_daemonize0(proc_t *proc)
{
	apr_proc_detach(APR_PROC_DETACH_DAEMONIZE);
	return A_TRUE;
}

//EOF
