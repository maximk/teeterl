//
//
//

#include <apr_strings.h>

#include <stdio.h>

#include "teeterl.h"
#include "term.h"
#include "heap.h"
#include "atom.h"
#include "cstr.h"
#include "binary.h"
#include "mpi.h"
#include "list.h"

term_t bin2term(apr_byte_t **data, int *bytes_left, atoms_t *atoms, heap_t *heap);
int term2bin(term_t t, atoms_t *atoms, apr_array_header_t *buf);

term_t binary_to_term(term_t bin, atoms_t *atoms, heap_t *hp)
{
	term_box_t *bbox;
	int bit_size;
	apr_byte_t *data;
	int bytes_left;

	bbox = peel(bin);
	bit_size = bbox->binary.bit_size;
	if (BIN_TRAILER_SIZE(bit_size) > 0)
		return noval;
	data = bbox->binary.data;
	bytes_left = BIN_BYTE_SIZE(bit_size);

	if (bytes_left < 1 || data[0] != 131)
		return noval;
	data++;	bytes_left--;

	return bin2term(&data, &bytes_left, atoms, hp);
}

term_t bin2term(apr_byte_t **data, int *bytes_left, atoms_t *atoms, heap_t *heap)
{

#define require(__n) \
	do { \
		if (*bytes_left < __n) \
			return noval; \
		(*bytes_left) -= __n; \
	} while (0)

#define get_byte() (*(*data)++)

	require(1);
	switch (get_byte())
	{
	case 97:
	{
		require(1);
		return tag_int(get_byte());
	}
	case 98:
	{
		int a, b, c, d;
		require(4);
		a = get_byte();
		b = get_byte();
		c = get_byte();
		d = get_byte();
		return int_to_term((a << 24) | (b << 16) | (c << 8) | d, heap);
	}
	case 99:
	{
		double value;
		require(31);
		sscanf((const char *)*data, "%lf", &value);
		(*data) += 31;
		return heap_float(heap, value);
	}
	case 100:
	{
		int a, b;
		int len;
		cstr_t *s;
		int index;
		require(2);
		a = get_byte();
		b = get_byte();
		len = ((a << 8) | b);
		if (len > 255)
			return noval;
		require(len);
		s = (cstr_t *)heap_alloc(heap, sizeof(cstr_t) + len);
		s->size = len;
		memcpy(s->data, *data, len);
		index = atoms_set(atoms, s);
		(*data) += len;
		return tag_atom(index);
	}
	case 104:
	{
		int arity, i;
		term_t tuple;
		term_box_t *tbox;
		require(1);
		arity = get_byte();
		tuple = heap_tuple(heap, arity);
		tbox = peel(tuple);
		for (i = 0; i < arity; i++)
		{
			term_t e = bin2term(data, bytes_left, atoms, heap);
			if (e == noval)
				return noval;
			tbox->tuple.elts[i] = e;
		}
		return tuple;
	}
	case 105:
	{
		int a, b, c, d;
		int arity, i;
		term_t tuple;
		term_box_t *tbox;
		require(4);
		a = get_byte();
		b = get_byte();
		c = get_byte();
		d = get_byte();
		arity = ((a << 24) | (b << 16) | (c << 8) | d);
		tuple = heap_tuple(heap, arity);
		tbox = peel(tuple);
		for (i = 0; i < arity; i++)
		{
			term_t e = bin2term(data, bytes_left, atoms, heap);
			if (e == noval)
				return noval;
			tbox->tuple.elts[i] = e;
		}
		return tuple;
	}
	case 106:
	{
		return nil;
	}
	case 107:
	{
		int a, b;
		int len, i;
		term_t cons = nil;
		require(2);
		a = get_byte();
		b = get_byte();
		len = ((a << 8) | b);
		require(len);
		i = len-1;
		while (i >= 0)
			cons = heap_cons2(heap, tag_int((*data)[i--]), cons);
		(*data) += len;
		return cons;
	}
	case 108:
	{
		int a, b, c, d;
		int len, i;
		term_t *es;
		term_t tail;
		require(4);
		a = get_byte();
		b = get_byte();
		c = get_byte();
		d = get_byte();
		len = ((a << 24) | (b << 16) | (c << 8) | d);
		es = (term_t *)heap_alloc(heap, len*sizeof(term_t));
		for (i = 0; i < len; i++)
		{
			term_t e = bin2term(data, bytes_left, atoms, heap);
			if (e == noval)
				return noval;
			es[i] = e;
		}
		tail = bin2term(data, bytes_left, atoms, heap);
		if (tail == noval)
			return noval;
		i = len-1;
		while (i >= 0)
			tail = heap_cons2(heap, es[i--], tail);
		return tail;
	}
	case 109:
	{
		int a, b, c, d;
		int len;
		term_t bin;
		require(4);
		a = get_byte();
		b = get_byte();
		c = get_byte();
		d = get_byte();
		len = ((a << 24) | (b << 16) | (c << 8) | d);
		require(len);
		bin = heap_binary(heap, len*8, (*data));
		(*data) += len;
		return bin;
	}
	case 110:
	{
		int len;
		int sign;
		mp_size prec;
		mp_int mp;
		mp_err rs;
		require(1);
		len = get_byte();
		sign = get_byte();
		require(len);
		prec = (len + (MP_DIGIT_SIZE-1)) / MP_DIGIT_SIZE;
		mp_init_size(&mp, prec, heap);
		//TODO: use mp_read_signed_bin
		rs = mp_read_unsigned_bin_lsb(&mp, *data, len, heap);
		if (rs != MP_OKAY)
			return noval;
		(*data) += len;
		if (sign == 1)
			mp_neg(&mp, &mp, heap);
		return mp_to_term(mp);
	}
	case 111:
	{
		int a, b, c, d;
		int len;
		int sign;
		mp_size prec;
		mp_int mp;
		mp_err rs;
		require(4);
		a = get_byte();
		b = get_byte();
		c = get_byte();
		d = get_byte();
		len = ((a << 24) | (b << 16) | (c << 8) | d);
		require(1);
		sign = get_byte();
		require(len);
		prec = (len + (MP_DIGIT_SIZE-1)) / MP_DIGIT_SIZE;
		mp_init_size(&mp, prec, heap);
		rs = mp_read_unsigned_bin_lsb(&mp, *data, len, heap);
		if (rs != MP_OKAY)
			return noval;
		(*data) += len;
		if (sign == 1)
			mp_neg(&mp, &mp, heap);
		return mp_to_term(mp);
	}
	default:
		return noval;	// only a subset of tags are supported; inspired by BERT
	}
}

term_t term_to_binary(term_t t, atoms_t *atoms, heap_t *hp)
{
	apr_pool_t *tmp;
	apr_array_header_t *buf;
	int result;
	term_t bin = noval;
	apr_pool_create(&tmp, 0);
	buf = apr_array_make(tmp, 1024, 1);
	APR_ARRAY_PUSH(buf, apr_byte_t) = 131;
	result = term2bin(t, atoms, buf);
	if (result == 0)
		bin = heap_binary(hp, buf->nelts*8, (apr_byte_t *)buf->elts);
	apr_pool_destroy(tmp);
	return bin;
}

// 97-100
// 104-111

int term2bin(term_t t, atoms_t *atoms, apr_array_header_t *buf)
{
	if (is_int(t))
	{
		int i = int_value(t);
		if (i >= 0 && i < 256)
		{
			APR_ARRAY_PUSH(buf, apr_byte_t) = 97;
			APR_ARRAY_PUSH(buf, apr_byte_t) = (apr_byte_t)i;
		}
		else
		{
			APR_ARRAY_PUSH(buf, apr_byte_t) = 98;
			APR_ARRAY_PUSH(buf, apr_byte_t) = (apr_byte_t)((i >> 24));
			APR_ARRAY_PUSH(buf, apr_byte_t) = (apr_byte_t)((i >> 16) & 255);
			APR_ARRAY_PUSH(buf, apr_byte_t) = (apr_byte_t)((i >> 8) & 255);
			APR_ARRAY_PUSH(buf, apr_byte_t) = (apr_byte_t)((i) & 255);
		}
	}
	else if (is_float(t))
	{
		const char *str = apr_psprintf(buf->pool, "%.20e", float_value(t));
		const char *ptr = str;
		int n = 0;

		APR_ARRAY_PUSH(buf, apr_byte_t) = 99;
		while (n < 31)
		{
			if (*ptr)
				APR_ARRAY_PUSH(buf, apr_byte_t) = *ptr++;
			else
				APR_ARRAY_PUSH(buf, apr_byte_t) = 0;
			n++;
		}
	}
	else if (is_atom(t))
	{
		cstr_t *s = atoms_get(atoms, atom_index(t));
		int i;
		APR_ARRAY_PUSH(buf, apr_byte_t) = 100;
		APR_ARRAY_PUSH(buf, apr_byte_t) = (apr_byte_t)(s->size >> 8);
		APR_ARRAY_PUSH(buf, apr_byte_t) = (apr_byte_t)s->size;
		for (i = 0; i < s->size; i++)
			APR_ARRAY_PUSH(buf, apr_byte_t) = (apr_byte_t)s->data[i];
	}
	else if (is_tuple(t))
	{
		term_box_t *tb = peel(t);
		int arity = tb->tuple.size;
		int i;
		if (arity < 256)
		{
			APR_ARRAY_PUSH(buf, apr_byte_t) = 104;
			APR_ARRAY_PUSH(buf, apr_byte_t) = (apr_byte_t)arity;
			for (i = 0; i < arity; i++)
			{
				int result = term2bin(tb->tuple.elts[i], atoms, buf);
				if (result != 0)
					return result;
			}
		}
		else
		{
			APR_ARRAY_PUSH(buf, apr_byte_t) = 105;
			APR_ARRAY_PUSH(buf, apr_byte_t) = (apr_byte_t)((arity >> 24));
			APR_ARRAY_PUSH(buf, apr_byte_t) = (apr_byte_t)((arity >> 16) & 255);
			APR_ARRAY_PUSH(buf, apr_byte_t) = (apr_byte_t)((arity >> 8) & 255);
			APR_ARRAY_PUSH(buf, apr_byte_t) = (apr_byte_t)((arity) & 255);
			for (i = 0; i < arity; i++)
			{
				int result = term2bin(tb->tuple.elts[i], atoms, buf);
				if (result != 0)
					return result;
			}
		}
	}
	else if (is_nil(t))
	{
		APR_ARRAY_PUSH(buf, apr_byte_t) = 106;
	}
	else if (is_list(t))
	{
		int len = 0;
		term_t cons = t;
		int is_string = 1;
		while (is_cons(cons) && len < 65536)
		{
			term_box_t *cb = peel(cons);
			term_t v = cb->cons.head;
			if (!is_int(v) || int_value(v) < 0 || int_value(v) > 255)
			{
				is_string = 0;
				break;
			}
			len++;
			cons = cb->cons.tail;
		}

		if ((is_string && !is_nil(cons)) || len >= 65536)
			is_string = 0;

		if (is_string)
		{
			APR_ARRAY_PUSH(buf, apr_byte_t) = 107;
			APR_ARRAY_PUSH(buf, apr_byte_t) = (apr_byte_t)((len >> 8));
			APR_ARRAY_PUSH(buf, apr_byte_t) = (apr_byte_t)((len) & 255);
			cons = t;
			while (is_cons(cons))
			{
				term_box_t *cb = peel(cons);
				term_t v = cb->cons.head;
				APR_ARRAY_PUSH(buf, apr_byte_t) = (apr_byte_t)int_value(v);
				cons = cb->cons.tail;
			}
		}
		else
		{
			int result;
			int len1 = list_length(t);

			APR_ARRAY_PUSH(buf, apr_byte_t) = 108;
			APR_ARRAY_PUSH(buf, apr_byte_t) = (apr_byte_t)((len1 >> 24));
			APR_ARRAY_PUSH(buf, apr_byte_t) = (apr_byte_t)((len1 >> 16) & 255);
			APR_ARRAY_PUSH(buf, apr_byte_t) = (apr_byte_t)((len1 >> 8) & 255);
			APR_ARRAY_PUSH(buf, apr_byte_t) = (apr_byte_t)((len1) & 255);
			cons = t;
			while (is_cons(cons))
			{
				term_box_t *cb = peel(cons);
				term_t v = cb->cons.head;
				result = term2bin(v, atoms, buf);
				if (result != 0)
					return result;
				cons = cb->cons.tail;
			}
			
			//tail
			result = term2bin(cons, atoms, buf);
			if (result != 0)
				return result;
		}
	}
	else if (is_binary(t))
	{
		term_box_t *bb = peel(t);
		int size = BIN_BYTE_SIZE(bb->binary.bit_size);
		apr_byte_t *data = bb->binary.data;
		int i;

		APR_ARRAY_PUSH(buf, apr_byte_t) = 109;
		APR_ARRAY_PUSH(buf, apr_byte_t) = (apr_byte_t)((size >> 24));
		APR_ARRAY_PUSH(buf, apr_byte_t) = (apr_byte_t)((size >> 16) & 255);
		APR_ARRAY_PUSH(buf, apr_byte_t) = (apr_byte_t)((size >> 8) & 255);
		APR_ARRAY_PUSH(buf, apr_byte_t) = (apr_byte_t)((size) & 255);
		for (i = 0; i < size; i++)
			APR_ARRAY_PUSH(buf, apr_byte_t) = data[i];
	}
	else if (is_bignum(t))
	{
		mp_int ma = bignum_to_mp(t);
		int sign = mp_cmp_z(&ma) >= 0 ?0 :1;
		int len = mp_unsigned_bin_size(&ma);
		int i;
		if (len < 256)
		{
			apr_byte_t *data;
			APR_ARRAY_PUSH(buf, apr_byte_t) = 110;
			APR_ARRAY_PUSH(buf, apr_byte_t) = (apr_byte_t)len;
			APR_ARRAY_PUSH(buf, apr_byte_t) = (apr_byte_t)sign;
			for (i = 0; i < len; i++)
				APR_ARRAY_PUSH(buf, apr_byte_t) = 0;
			data = (apr_byte_t *)buf->elts - len;
			mp_to_unsigned_bin_lsb(&ma, data);
		}
		else
		{
			apr_byte_t *data;
			APR_ARRAY_PUSH(buf, apr_byte_t) = 111;
			APR_ARRAY_PUSH(buf, apr_byte_t) = (apr_byte_t)((len >> 24));
			APR_ARRAY_PUSH(buf, apr_byte_t) = (apr_byte_t)((len >> 16) & 255);
			APR_ARRAY_PUSH(buf, apr_byte_t) = (apr_byte_t)((len >> 8) & 255);
			APR_ARRAY_PUSH(buf, apr_byte_t) = (apr_byte_t)((len) & 255);
			APR_ARRAY_PUSH(buf, apr_byte_t) = (apr_byte_t)sign;
			for (i = 0; i < len; i++)
				APR_ARRAY_PUSH(buf, apr_byte_t) = 0;
			data = (apr_byte_t *)buf->elts - len;
			mp_to_unsigned_bin_lsb(&ma, data);
		}
	}
	else
		return 1;

	return 0;
}

//EOF
