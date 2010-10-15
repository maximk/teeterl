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

#include "cstr.h"
#include "heap.h"

#include <apr_strings.h>

cstr_t *ztos(const char *z, heap_t *hp)
{
	int n = (int)strlen(z);
	cstr_t *s = (cstr_t *)heap_alloc(hp, n+1);
	s->size = n;
	memmove(s->data, z, n);
	return s;
}

const char *stoz(cstr_t *s, heap_t *hp)
{
	char *z = (char *)heap_alloc(hp, s->size+1);
	memmove(z, s->data, s->size);
	z[s->size] = 0;
	return z;
}

int scomp(cstr_t *s1, cstr_t *s2)
{
	if (s1->size != s2->size)
		return 0;
	return (memcmp(s1->data, s2->data, s1->size) == 0);
}

cstr_t *scopy(cstr_t *s, heap_t *hp)
{
	cstr_t *copy = (cstr_t *)heap_alloc(hp, s->size+1);
	memcpy(copy, s, s->size+1);
	return copy;
}

cstr_t *scopy2(cstr_t *s, apr_pool_t *pool)
{
	cstr_t *copy = (cstr_t *)apr_palloc(pool, s->size+1);
	memcpy(copy, s, s->size+1);
	return copy;
}

//EOF
