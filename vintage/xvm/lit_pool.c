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

#include "lit_pool.h"

#include <apr_tables.h>

#include "tcomp.h"
#include "xpool.h"

struct lit_pool_t {
	//TODO: naive implementation
	apr_array_header_t *terms;
	xpool_t *xp;
};

lit_pool_t *lit_pool_make(apr_pool_t *p)
{
	lit_pool_t *lp = apr_palloc(p, sizeof(*lp));
	lp->terms = apr_array_make(p, 256, sizeof(term_t));
	lp->xp = xpool_make(p);
	return lp;
}

term_t lit_pool_store(lit_pool_t *self, term_t value)
{
	term_t *ptr;
	term_t *end;
	term_t value1;

	if (!is_ptr(value))
		return value;

	ptr = (term_t *)self->terms->elts;
	end = ptr + self->terms->nelts;
	while (ptr < end)
	{
		if (terms_are_equal0(value, *ptr, 1))
			return *ptr;
		ptr++;
	}

	value1 = marshal_term(value, self->xp);
	value1 = pin_term(value1);
	*(term_t *)apr_array_push(self->terms) = value1;
	return value1;
}

//EOF
