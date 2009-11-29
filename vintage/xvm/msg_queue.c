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

#include "msg_queue.h"

#include <apr_tables.h>

#include "atom.h"

struct msg_queue_t {
	apr_array_header_t *q;
	int hd;
	int cur;
	int end;
};

msg_queue_t *msg_queue_make(apr_pool_t *pool)
{
	msg_queue_t *p = apr_palloc(pool, sizeof(*p));
	p->q = apr_array_make(pool, MSGQ_INIT_SIZE, sizeof(term_t));
	p->hd = 0;
	p->cur = 0;
	p->end = 0;
	return p;
}

void msg_queue_push(msg_queue_t *self, term_t t)
{
	*(term_t *)apr_array_push(self->q) = t;
}

void msg_queue_reset(msg_queue_t *self)
{
	self->cur = self->hd - 1; //message just before the first
	self->end = self->q->nelts;
}

term_t msg_queue_next(msg_queue_t *self)
{
	if (self->cur == self->end) //call after new message
		self->end = self->q->nelts; //retry cur message
	else
		self->cur++;

	if (self->cur == self->end)
		return AI_UNDEFINED; //all messages matched -- check for timeout

	return ((term_t *)self->q->elts)[self->cur];
}

void msg_queue_drop(msg_queue_t *self)
{
	if (self->hd >= self->q->nelts)
	{
		int a = 1;
	}

	if (self->cur > self->hd)
		memmove((term_t *)self->q->elts + self->hd + 1,
			(term_t *)self->q->elts + self->hd,
			self->q->elt_size * (self->cur - self->hd));
	self->hd++;

	if (self->q->nelts > MSGQ_INIT_SIZE && self->hd >= (self->q->nelts+1)/2)
	{
		//move stuff to the beggining
		memmove(self->q->elts,
			(term_t *)self->q->elts + self->hd,
			self->q->elt_size * (self->q->nelts - self->hd));
		self->q->nelts -= self->hd;
		self->hd = 0;
	}
}

void msg_queue_gc_copy(msg_queue_t *self, xpool_t *xp)
{
	//self->q is not copied as maintains constant size
	term_t *ptr = (term_t *)self->q->elts + self->hd;
	term_t *end = (term_t *)self->q->elts + self->q->nelts;
	while (ptr < end)
	{
		*ptr = gc_copy_term(*ptr, xp);
		ptr++;
	}
}

int msg_queue_len(msg_queue_t *self)
{
	return self->q->nelts - self->hd;
}

term_t msg_queue_to_term(msg_queue_t *self, xpool_t *xp)
{
	term_t t = nil;
	term_t cons = nil;
	term_t *ptr = (term_t *)self->q->elts + self->hd;
	term_t *end = (term_t *)self->q->elts + self->q->nelts;
	while (ptr < end)
	{
		lst_add(t, cons, *ptr, xp);
		ptr++;
	}
	return t;
}

//EOF
