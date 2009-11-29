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

#include <apr_general.h>
#include <apr_tables.h>
#include <apr_hash.h>
#include <apr_strings.h>

#include "atom.h"

typedef struct atom_info_t atom_info_t;
struct atom_info_t {
	atom_t index;
	cstr_t *str;
};

struct atoms_t {
	apr_pool_t *pool;
	apr_array_header_t *space;
	apr_hash_t *str_to_info;
};

apr_status_t atoms_create(atoms_t **new, apr_pool_t *pool)
{
	atom_info_t *ai;
	atoms_t *p = apr_palloc(pool, sizeof(atoms_t));
	p->pool = pool;
	p->space = apr_array_make(pool, 256, sizeof(atom_info_t));
	p->str_to_info = apr_hash_make(pool);

#define ATOM_INC_POOL (pool)
#define ATOM_INC_SPACE (p->space)
#define ATOM_INC_HASH (p->str_to_info)
#define ATOM_INC_PTR (ai)

#include "atoms.inc"

	*new = p;
	return 0;
}

atom_t atoms_set(atoms_t *self, cstr_t *str)
{
	atom_info_t *ai = apr_hash_get(self->str_to_info, str->data, str->size);
	if (ai)
		return ai->index;
	else
	{
		atom_t index = self->space->nelts;
		ai = (atom_info_t *)apr_array_push(self->space);
		ai->index = index;
		ai->str = scopy(str, self->pool);
		apr_hash_set(self->str_to_info, ai->str->data, ai->str->size, ai);
		return index;
	}
}

cstr_t *atoms_get(atoms_t *self, atom_t a)
{
	atom_info_t *ai;
	if ((int)a >= self->space->nelts)
		return 0;
	ai = (atom_info_t *)self->space->elts + a;
	return ai->str;
}

apr_byte_t *atoms_nonstd_space(atoms_t *self, int *sizep, apr_pool_t *pool)
{
	int i;
	int size = 0;
	apr_byte_t *nonstd, *p;
	for (i = ATOM_FIRST_NONSTD; i < self->space->nelts; i++)
	{
		atom_info_t *ai = (atom_info_t *)self->space->elts + i;
		size += ai->str->size + 1; //+1 for counter byte
	}
	if (size == 0)
		return 0;
	nonstd = apr_palloc(pool, size);
	p = nonstd;
	for (i = ATOM_FIRST_NONSTD; i < self->space->nelts; i++)
	{
		atom_info_t *ai = (atom_info_t *)self->space->elts + i;
		memmove(p, ai->str, ai->str->size+1);
		p += ai->str->size+1;
	}
	if (sizep)
		*sizep = size;
	return nonstd;
}

//EOF
