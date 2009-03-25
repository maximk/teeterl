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

#include "hash.h"

const apr_uint32_t C1 = 268440163;
const apr_uint32_t C2 = 268439161;
const apr_uint32_t C3 = 268435459;
const apr_uint32_t C4 = 268436141;
const apr_uint32_t C5 = 268438633;
const apr_uint32_t C6 = 268437017;
const apr_uint32_t C7 = 268438039;
const apr_uint32_t C8 = 268437511;
const apr_uint32_t C9 = 268439627;

static apr_uint32_t hash(term_t t, apr_uint32_t h, atoms_t *atoms);

apr_uint32_t hash_term(term_t t, apr_uint32_t max, atoms_t *atoms)
{
	return hash(t, 0, atoms) % max;
}

static apr_uint32_t hash(term_t t, apr_uint32_t h, atoms_t *atoms)
{
	if (is_atom(t))
	{
		cstr_t *print_name = atoms_get(atoms, index(t));
		int i;
		apr_uint32_t a = 0;
		for (i = 0; i < print_name->size; i++)
		{
			apr_uint32_t j = (a << 4) + print_name->data[i];
			a = (j & 0x0fffffff) ^ ((j >> 24) & 0xf0);
		}
		return C1 * h + a;
	}
	else if (is_int(t))
		return C2 * h + int_value(t);
	else if (is_nil(t))
		return C3 * h + 1;
	else if (is_binary(t))
	{
		int l = int_value(bin_size(t));
		apr_byte_t *data = bin_data(t); 
		int i;

		// TODO: phash uses the whole binary
		if (l > 15)
			l = 15;

		for (i = 0; i < l; i++)
			h = C1 * h + data[i];

		return C4 * h + int_value(bin_size(t));
	}
	else if (is_pid(t))
		return C5 * h; //TODO: + MagicPid(t)
	else if (is_port(t) || is_ref(t))
		return C9 * h; //TODO: + MagicPortRef(t)
	else if (is_float(t))
	{
		union {
			double d;
			apr_uint32_t w[2];
		} u;
		u.d = dbl_value(t);
		return C6 * h + (u.w[0] ^ u.w[1]);
	}
	else if (is_cons(t))
		return hash(lst_value(t), hash(lst_next(t), h, atoms), atoms);
	else if (is_tuple(t))
	{
		int n = int_value(tup_size(t));
		int i;
		for (i = 0; i < n; i++)
			h = hash(tup_elts(t)[i], h, atoms);
		return C9 * h + n;
	}
	else
		return 0;
}

//EOF
