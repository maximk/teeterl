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

#include "xpool.h"

struct xpool_t {
	apr_pool_t *pool;
	apr_size_t size;
};

xpool_t *xpool_make(apr_pool_t *parent)
{
	apr_pool_t *p;
	xpool_t *xp;

	apr_pool_create(&p, parent);
	xp = apr_palloc(parent, sizeof(*xp));	//struct should survive clear
	xp->pool = p;
	xp->size = 0;
	return xp;
}

void *xalloc(xpool_t *xp, apr_size_t size)
{
	void *mem = apr_palloc(xp->pool, size);
	xp->size += APR_ALIGN_DEFAULT(size);
	return mem;
}

void xpool_clear(xpool_t *xp)
{
	apr_pool_clear(xp->pool);
	xp->size = 0;
}

void xpool_destroy(xpool_t *xp)
{
	apr_pool_destroy(xp->pool);
}

apr_size_t xpool_size(xpool_t *xp)
{
	return xp->size;
}

//EOF
