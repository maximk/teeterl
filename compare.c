//
//	Term comparison: == > >= =< <
//

#include "compare.h"

#include "teeterl.h"
#include "mpi.h"
#include "binary.h"

int are_terms_equal(term_t a, term_t b)
{
	if (a == b)
		return 1;

	if (is_immed(a) || is_immed(b))
	{
		if (is_int(a) && is_float(b))
			return (double)int_value(a) == float_value(b);
		if (is_float(a) && is_int(b))
			return float_value(a) == (double)int_value(b);
		return 0;
	}

	// list - tuple - binary - fun - bignum - pid - float

	if (is_cons(a))
	{
		if (is_cons(b))
		{
			do {
				term_box_t *ba = peel(a);
				term_box_t *bb = peel(b);
				if (!are_terms_equal(ba->cons.head, bb->cons.head))
					return 0;
				a = ba->cons.tail;
				b = bb->cons.tail;
			} while (is_cons(a) && is_cons(b));
			return are_terms_equal(a, b);
		}
		else
			return 0;
	}
	else if (is_tuple(a))
	{
		if (is_tuple(b))
		{
			term_box_t *ba = peel(a);
			term_box_t *bb = peel(b);
			int i, n;
			if (ba->tuple.size != bb->tuple.size)
				return 0;
			n = ba->tuple.size;
			for (i = 0; i < n; i++)
				if (!are_terms_equal(ba->tuple.elts[i], bb->tuple.elts[i]))
					return 0;
			return 1;
		}
		else
			return 0;
	}
	else if (is_binary(a))
	{
		if (is_binary(b))
		{
			term_box_t *ba = peel(a);
			term_box_t *bb = peel(b);
			int whole_bytes;
			int trailer_size;
			int d;
			if (ba->binary.bit_size != bb->binary.bit_size)
				return 0;
			whole_bytes = BIN_WHOLE_BYTES(ba->binary.bit_size);
			trailer_size = BIN_TRAILER_SIZE(ba->binary.bit_size);
			d = memcmp(ba->binary.data, bb->binary.data, whole_bytes);
			if (d != 0)
				return 0;
			return (ba->binary.data[whole_bytes] >> (8 - trailer_size))
				 == (bb->binary.data[whole_bytes] >> (8 - trailer_size));
		}
		else
			return 0;
	}
	else if (is_fun(a))
	{
		term_box_t *ba = peel(a);
		term_box_t *bb = peel(b);
		if (is_fun(b))
		{
			if (ba->fun.uniq != bb->fun.uniq)
				return 0;
			if (ba->fun.index != bb->fun.index)
				return 0;
			return are_terms_equal(ba->fun.frozen, bb->fun.frozen);
		}
		else
			return 0;
	}
	else if (is_bignum(a))
	{
		if (is_bignum(b))
		{
			mp_int ma = bignum_to_mp(a);
			mp_int mb = bignum_to_mp(b);
			return mp_cmp(&ma, &mb) == 0;
		}
		else
			return 0;
	}
	else if (is_long_id(a))
	{
		term_box_t *ba = peel(a);
		term_box_t *bb = peel(b);
		if (is_long_id(b))
		{
			if (ba->long_id.node != bb->long_id.node)
				return 0;
			if (ba->long_id.serial != bb->long_id.serial)
				return 0;
			return ba->long_id.tag_creation == bb->long_id.tag_creation;
		}
		else
			return 0;
	}
	else // float
	{
		if (is_float(b))
			return (float_value(a) == float_value(b));
		else
			return 0;
	}
}

// number < atom < fun < pid < tuple < empty_list < list < binary

// list - tuple - binary - fun - bignum - pid - float

int is_term_smaller(term_t a, term_t b, atoms_t *atoms)
{
	if (a == b)
		return 0;

	if (are_both_immed_and_not_nil(a, b))
	{
		if (is_int(a))
		{
			if (is_int(b))
				return int_value(a) < int_value(b);		// or just a < b?
			else
				return 1;
		}

		if (is_atom(a))
		{
			if (is_int(b))
				return 0;
			else if (is_atom(b))
			{
				cstr_t *print1 = atoms_get(atoms, atom_index(a));
				cstr_t *print2 = atoms_get(atoms, atom_index(b));
				int short_len = (print1->size < print2->size)
					?print1->size
					:print2->size;
				int d = memcmp(print1->data, print2->data, short_len);
				if (d == 0)
					return print1->size < print2->size;
				return d < 0;
			}
			else
				return 1;
		}

		if (is_short_pid(a))
		{
			if (is_int(b) || is_atom(b))
				return 0;
			else // short_pid
				return pid_serial(a) < pid_serial(b);
		}
	}

	if (is_immed(a))
	{
		if (is_int(a))
		{
			if (is_bignum(b))
			{
				mp_int mb = bignum_to_mp(b);
				return mp_cmp_z(&mb) >= 0;
			}

			if (is_float(b))
				return int_value(a) < float_value(b);
			else
				return 1;
		}

		if (is_atom(a))
			return 1;

		if (is_short_pid(a))
			return 1;

		assert(is_nil(a));

		if (is_cons(b) || is_binary(b))
			return 1;
		else
			return 0;
	}

	if (is_cons(a))
	{
		if (is_cons(b))
		{
			term_box_t *ba = peel(a);
			term_box_t *bb = peel(b);
			do {
				if (is_term_smaller(ba->cons.head, bb->cons.head, atoms))
					return 1;
				if (is_term_bigger(ba->cons.head, bb->cons.head, atoms))
					return 0;
				a = ba->cons.tail;
				b = bb->cons.tail;
			} while (is_cons(a) && is_cons(b));

			return is_term_smaller(a, b, atoms);
		}
		else if (is_binary(b))
			return 1;
		else
			return 0;
	}
	
	if (is_tuple(a))
	{
		if (is_tuple(b))
		{
			term_box_t *ba = peel(a);
			term_box_t *bb = peel(b);
			int i;
			if (ba->tuple.size < bb->tuple.size)
				return 1;
			if (ba->tuple.size > bb->tuple.size)
				return 0;
			for (i = 0; i < ba->tuple.size; i++)
			{
				term_t ea = ba->tuple.elts[i];
				term_t eb = bb->tuple.elts[i];
				if (is_term_smaller(ea, eb, atoms))
					return 1;
				if (is_term_bigger(ea, eb, atoms))
					return 0;
			}
			return 0;
		}
		else if (is_list(b) || is_binary(b))
			return 1;
		else
			return 0;
	}
	
	if (is_binary(a))
	{
		if (is_binary(b))
		{
			// binaries compared by values first
			// if one binary is prefix of the other
			// then sizes are compared

			term_box_t *ba = peel(a);
			term_box_t *bb = peel(b);

			if (ba->binary.bit_size < bb->binary.bit_size)
			{
				uint trailera, trailerb;
				int whole_bytes = BIN_WHOLE_BYTES(ba->binary.bit_size);
				int trailer_size = BIN_TRAILER_SIZE(ba->binary.bit_size);
				int d = memcmp(ba->binary.data, bb->binary.data, whole_bytes);
				if (d < 0)
					return 1;
				if (d > 0)
					return 0;
				trailera = ba->binary.data[whole_bytes] >> (8 - trailer_size);
				trailerb = bb->binary.data[whole_bytes] >> (8 - trailer_size);
				if (trailera < trailerb)
					return 1;
				if (trailera > trailerb)
					return 0;
				return 1; // a is shorter
			}
			else	// size of b is smaller or equal to a's
			{
				uint trailera, trailerb;
				int whole_bytes = BIN_WHOLE_BYTES(bb->binary.bit_size);
				int trailer_size = BIN_TRAILER_SIZE(bb->binary.bit_size);
				int d = memcmp(ba->binary.data, bb->binary.data, whole_bytes);
				if (d < 0)
					return 1;
				if (d > 0)
					return 0;
				trailera = ba->binary.data[whole_bytes] >> (8 - trailer_size);
				trailerb = bb->binary.data[whole_bytes] >> (8 - trailer_size);
				if (trailera < trailerb)
					return 1;
				if (trailera > trailerb)
					return 0;
				return 0;
			}
		}
		else
			return 0;
	}

	if (is_fun(a))
	{
		if (is_fun(b))
		{
			term_box_t *ba = peel(a);
			term_box_t *bb = peel(b);

			if (ba->fun.uniq < bb->fun.uniq)
				return 1;
			if (ba->fun.uniq > bb->fun.uniq)
				return 0;
			if (ba->fun.index < bb->fun.index)
				return 1;
			if (ba->fun.index > bb->fun.index)
				return 0;
			return is_term_smaller(ba->fun.frozen, bb->fun.frozen, atoms);
		}
		else		
			return 1;
	}

	if (is_bignum(a))
	{
		mp_int ma = bignum_to_mp(a);

		if (is_int(b))
			return mp_cmp_z(&ma) < 0;
		if (is_bignum(b))
		{
			mp_int mb = bignum_to_mp(b);
			return mp_cmp(&ma, &mb) < 0;
		}
		if (is_float(b))
			return mp_get_double(&ma) < float_value(b);
		else
			return 1;
	}

	if (is_long_pid(a))
	{
		if (is_long_pid(b))
		{
			term_box_t *ba = peel(a);
			term_box_t *bb = peel(b);

			if (is_term_smaller(ba->long_id.node, bb->long_id.node, atoms))
				return 1;
			if (is_term_bigger(ba->long_id.node, bb->long_id.node, atoms))
				return 0;
			if (ba->long_id.serial < bb->long_id.serial)
				return 1;
			if (ba->long_id.serial > bb->long_id.serial)
				return 0;
			if (ba->long_id.tag_creation < bb->long_id.tag_creation)	// TODO: layout dependency
				return 1;
			else
				return 0;
		}

		// TODO: compare short_pid and long_pid

		if (is_fun(b) || is_oid(b))
			return 0;
		else
			return 1;
	}

	if (is_long_oid(a))
	{
		if (is_long_oid(b))
		{
			term_box_t *ba = peel(a);
			term_box_t *bb = peel(b);

			if (is_term_smaller(ba->long_id.node, bb->long_id.node, atoms))
				return 1;
			if (is_term_bigger(ba->long_id.node, bb->long_id.node, atoms))
				return 0;
			if (ba->long_id.serial < bb->long_id.serial)
				return 1;
			if (ba->long_id.serial > bb->long_id.serial)
				return 0;
			if (ba->long_id.tag_creation < bb->long_id.tag_creation)	// TODO: layout dependency
				return 1;
			else
				return 0;
		}

		// TODO: compare short_oid and long_oid

		if (is_fun(b))
			return 0;
		else
			return 1;
	}

	assert(is_float(a));

	if (is_int(b))
		return float_value(a) < (double)int_value(b);
	if (is_bignum(b))
	{
		mp_int mb = bignum_to_mp(b);
		return float_value(a) < mp_get_double(&mb);
	}
	if (is_float(b))
		return float_value(a) < float_value(b);
	else
			return 1;
}

//EOF
