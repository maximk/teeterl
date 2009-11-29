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
	int index;
	cstr_t *str;
};

struct atoms_t {
	apr_array_header_t *space;
	apr_hash_t *str_to_info;
};

atoms_t *atoms_make(apr_pool_t *pool)
{
	atom_info_t *ai;
	atoms_t *p = apr_palloc(pool, sizeof(atoms_t));
	p->space = apr_array_make(pool, 256, sizeof(atom_info_t));
	p->str_to_info = apr_hash_make(pool);

#define ATOM_INC_POOL (pool)
#define ATOM_INC_SPACE (p->space)
#define ATOM_INC_HASH (p->str_to_info)
#define ATOM_INC_PTR (ai)

#include "atoms.inc"

	return p;
}

int atoms_set(atoms_t *self, cstr_t *str)
{
	atom_info_t *ai = apr_hash_get(self->str_to_info, str->data, str->size);
	if (ai)
		return ai->index;
	else
	{
		int index = self->space->nelts + ATOM_INDEX_BASE;
		ai = (atom_info_t *)apr_array_push(self->space);
		ai->index = index;
		ai->str = scopy2(str, self->space->pool);
		apr_hash_set(self->str_to_info, ai->str->data, ai->str->size, ai);
		return index;
	}
}

cstr_t *atoms_get(atoms_t *self, int a)
{
	atom_info_t *ai;
	if (a - ATOM_INDEX_BASE >= self->space->nelts)
		return NULL;
	ai = (atom_info_t *)self->space->elts + a - ATOM_INDEX_BASE;
	return ai->str;
}

//EOF
