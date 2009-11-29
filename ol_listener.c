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

#include "outlet.h"

#include "proc.h"
#include "atom.h"
#include "scheduler.h"

typedef struct ol_listener_data_t ol_listener_data_t;
struct ol_listener_data_t {
	apr_socket_t *sock;
	int is_accepting;
	term_t reply_to_pid;
	teevm_t *teevm;			// owner_in and owner_out are NULL
};

int ol_listener_want_read(outlet_t *self);
int ol_listener_want_write(outlet_t *self);
int ol_listener_is_socket(outlet_t *self);
apr_socket_t *ol_listener_get_socket(outlet_t *self);
apr_status_t ol_listener_do_readable(outlet_t *self);
apr_status_t ol_listener_do_writable(outlet_t *self);
apr_status_t ol_listener_send(outlet_t *self, term_t io);

apr_status_t ol_listener_set_option(outlet_t *self, term_t opt, term_t value);
apr_status_t ol_listener_close(outlet_t *self);
apr_status_t ol_listener_close0(outlet_t *self);

outlet_t *ol_listener_make(apr_socket_t *sock, teevm_t *teevm)
{
	ol_listener_data_t *data;

	apr_pool_t *pool = apr_socket_pool_get(sock);
	outlet_t *outlet = apr_palloc(pool, sizeof(outlet_t));
	outlet->serial = (uint)-1;
	outlet->pool = pool;
	outlet->owner_in = outlet->owner_out = 0;

	outlet->want_read = ol_listener_want_read;
	outlet->want_write = ol_listener_want_write;
	outlet->is_socket = ol_listener_is_socket;
	outlet->get_socket = ol_listener_get_socket;
	outlet->is_file = 0;
	outlet->get_file = 0;

	outlet->try_read = 0;
	outlet->try_write = 0;

	outlet->do_readable = ol_listener_do_readable;
	outlet->do_writable = ol_listener_do_writable;
	outlet->send = ol_listener_send;

	outlet->read = 0;
	outlet->write = 0;

	outlet->set_option = ol_listener_set_option;
	outlet->close = ol_listener_close;
	outlet->close0 = ol_listener_close0;

	data = apr_palloc(pool, sizeof(ol_listener_data_t));
	data->sock = sock;
	data->is_accepting = 0;
	data->reply_to_pid = noval;
	data->teevm = teevm;

	outlet->data = data;
	return outlet;
}

int ol_listener_want_read(outlet_t *self)
{
	ol_listener_data_t *data = self->data;
	return data->is_accepting;
}

int ol_listener_want_write(outlet_t *self)
{
	return 0;
}

int ol_listener_is_socket(outlet_t *self)
{
	return 1;
}

apr_socket_t *ol_listener_get_socket(outlet_t *self)
{
	ol_listener_data_t *data = self->data;
	return data->sock;
}

apr_status_t ol_listener_do_readable(outlet_t *self)
{
	apr_status_t rs;
	ol_listener_data_t *data = self->data;
	proc_t *proc;
	
	proc = scheduler_lookup(data->teevm->scheduler, pid_serial(data->reply_to_pid));
	if (proc)
	{
		apr_socket_t *new_sock;
		outlet_t *new_outlet;
		term_t msg;

		apr_pool_t *p;
		apr_pool_create(&p, 0);

		rs = apr_socket_accept(&new_sock, data->sock, p);
		if (rs == 0)
			rs = apr_socket_opt_set(new_sock, APR_SO_NONBLOCK, 1);

		if (rs != 0)
		{
			apr_pool_destroy(p);
			return rs;
		}

		new_outlet = ol_socket_make(new_sock, 0);

		//put to poll ring
		outlet_mall_allot(data->teevm->mall, new_outlet);

		//messages delivered to caller of accept
		new_outlet->owner_in = new_outlet->owner_out = proc;

		msg = heap_tuple3(proc->heap, A_TCP_ACCEPTED, outlet_id(self), outlet_id(new_outlet));
		scheduler_new_local_mail(data->teevm->scheduler, proc, msg);
	}

	data->is_accepting = 0;

	return APR_SUCCESS;
}

apr_status_t ol_listener_do_writable(outlet_t *self)
{
	return APR_ENOSOCKET;
}

apr_status_t ol_listener_send(outlet_t *self, term_t io)
{
	return APR_ENOSOCKET;
}

apr_status_t ol_listener_set_option(outlet_t *self, term_t opt, term_t value)
{
	if (opt == A_ACCEPT)
	{
		ol_listener_data_t *data = self->data;

		if (!is_short_pid(value))
			return APR_BADARG;

		data->is_accepting = 1;
		data->reply_to_pid = value;
	}
	else
		return APR_BADARG;

	return APR_SUCCESS;
}

apr_status_t ol_listener_close(outlet_t *self)
{
	return ol_listener_close0(self);
}

apr_status_t ol_listener_close0(outlet_t *self)
{
	ol_listener_data_t *data = self->data;
	
	if (data->is_accepting)
	{
		proc_t *proc = scheduler_lookup(data->teevm->scheduler, pid_serial(data->reply_to_pid));
		term_t msg = heap_tuple2(proc->heap, A_TCP_CLOSED, outlet_id(self));
		scheduler_new_local_mail(data->teevm->scheduler, proc, msg);

		data->is_accepting = 0;
	}

	apr_socket_close(data->sock);
	outlet_mall_kick_out(data->teevm->mall, self);
	apr_pool_destroy(self->pool);

	return APR_SUCCESS;
}

// EOF
