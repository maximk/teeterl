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

#include "scheduler.h"
#include "binary.h"
#include "buffer.h"
#include "proc.h"
#include "atom.h"

#define SOCK_INBUF_LEN 4096
#define SOCK_OUTBUF_LEN 4096

typedef struct ol_socket_data_t ol_socket_data_t;
struct ol_socket_data_t {
	apr_socket_t *sock;
	int is_connecting;
	int is_closing;
	buffer_t *in_buf;
	buffer_t *out_buf;
	int packet_expected;
	int expected_size;
	int space_required;
	int required_size;
};

int ol_socket_want_read(outlet_t *self);
int ol_socket_want_write(outlet_t *self);
int ol_socket_is_socket(outlet_t *self);
apr_socket_t *ol_socket_get_socket(outlet_t *self);
apr_status_t ol_socket_do_readable(outlet_t *self);
apr_status_t ol_socket_do_writable(outlet_t *self);
apr_status_t ol_socket_send(outlet_t *self, term_t io);

apr_status_t ol_socket_set_option(outlet_t *self, term_t opt, term_t value);
apr_status_t ol_socket_close(outlet_t *self);
apr_status_t ol_socket_close0(outlet_t *self);

outlet_t *ol_socket_make(apr_socket_t *sock, int is_connecting)
{
	ol_socket_data_t *data;
	apr_pool_t *pool = apr_socket_pool_get(sock);

	outlet_t *outlet = apr_palloc(pool, sizeof(outlet_t));
	outlet->serial = (uint)-1;
	outlet->owner_in = outlet->owner_out = 0;
	outlet->pool = pool;

	outlet->want_read = ol_socket_want_read;
	outlet->want_write = ol_socket_want_write;
	outlet->is_socket = ol_socket_is_socket;
	outlet->get_socket = ol_socket_get_socket;
	outlet->is_file = 0;
	outlet->get_file = 0;

	outlet->try_read = 0;
	outlet->try_write = 0;

	outlet->do_readable = ol_socket_do_readable;
	outlet->do_writable = ol_socket_do_writable;
	outlet->send = ol_socket_send;

	outlet->read = 0;
	outlet->write = 0;

	outlet->set_option = ol_socket_set_option;
	outlet->close = ol_socket_close;
	outlet->close0 = ol_socket_close0;

	data = apr_palloc(pool, sizeof(ol_socket_data_t));
	data->sock = sock;
	data->is_connecting = is_connecting;
	data->is_closing = 0;
	data->in_buf = buffer_make(pool, SOCK_INBUF_LEN);
	data->out_buf = buffer_make(pool, SOCK_OUTBUF_LEN);
	data->packet_expected = 0;
	data->expected_size = 0;
	data->space_required = 0;
	data->required_size = 0;

	outlet->data = data;
	return outlet;
}

int ol_socket_want_read(outlet_t *self)
{
	ol_socket_data_t *data = self->data;
	if (data->is_closing || data->is_connecting)
		return 0;
	return buffer_available(data->in_buf) > 0;
}

int ol_socket_want_write(outlet_t *self)
{
	ol_socket_data_t *data = self->data;
	if (data->is_closing || data->is_connecting)
		return 1;
	return buffer_len(data->out_buf) > 0;
}

int ol_socket_is_socket(outlet_t *self)
{
	return 1;
}

apr_socket_t *ol_socket_get_socket(outlet_t *self)
{
	ol_socket_data_t *data = self->data;
	return data->sock;
}

apr_status_t ol_socket_do_readable(outlet_t *self)
{
	apr_status_t rs;
	ol_socket_data_t *data = self->data;

	rs = buffer_socket_recv(data->in_buf, data->sock);
	if (rs == 0)
	{
		if (data->packet_expected)
		{
			int len = buffer_len(data->in_buf);

			if (data->expected_size == 0 ||
				data->expected_size > 0 && len >= data->expected_size)
			{
				proc_t *proc = self->owner_in;

				if (data->expected_size > 0)
					len = data->expected_size;

				if (proc != 0)
				{
					term_t bin = heap_binary(proc->heap, len*8, buffer_ptr(data->in_buf));
					term_t msg = heap_tuple3(proc->heap, A_TCP, outlet_id(self), bin);
					scheduler_new_local_mail(proc->teevm->scheduler, proc, msg);
					buffer_consume(data->in_buf, len);
				}

				data->packet_expected = 0;
				data->expected_size = 0;
			}
		}
	}

	return rs;
}

apr_status_t ol_socket_do_writable(outlet_t *self)
{
	ol_socket_data_t *data = self->data;

	if (data->is_connecting)
	{
		proc_t *proc = self->owner_in;
		if (proc)
		{
			term_t msg = heap_tuple2(proc->heap, A_TCP_CONNECTED, outlet_id(self));
			scheduler_new_local_mail(proc->teevm->scheduler, proc, msg);
		}

		data->is_connecting = 0;
	}

	if (buffer_len(data->out_buf) > 0)
	{
		apr_status_t rs;
		rs = buffer_socket_send(data->out_buf, data->sock);
		if (rs != 0)
			return rs;
	}

    if (data->is_closing && buffer_len(data->out_buf) == 0)
		return APR_EOF;	//make poll close0 the socket

	if (data->space_required && buffer_available(data->out_buf) >= data->required_size)
	{
		proc_t *proc = self->owner_out;
		int avail = buffer_available(data->out_buf);
		term_t msg = heap_tuple3(proc->heap, A_TCP_SPACE, outlet_id(self), tag_int(avail));

		//TODO: insure that only owner can send to socket
		scheduler_new_local_mail(proc->teevm->scheduler, proc, msg);

		data->space_required = 0;
	}

	return APR_SUCCESS;
}

static apr_status_t outlet_buffer_send(buffer_t *buf, term_t io)
{
	int avail;

	if (is_nil(io))
		return 0;

	avail = buffer_available(buf);
	if (is_int(io))
	{
		if (avail < 1)
			return APR_EINCOMPLETE;
		else
			buffer_put_byte(buf, int_value(io));
	}
	else if (is_binary(io))
	{
		//TODO: odd-trailer binaries
		term_box_t *bb = peel(io);
		int size = BIN_BYTE_SIZE(bb->binary.bit_size);
		if (size > avail)
		{
			buffer_put_data(buf, bb->binary.data, avail);
			return APR_EINCOMPLETE;
		}
		else
			buffer_put_data(buf, bb->binary.data, size);
	}
	else if (is_list(io))
	{
		while (is_cons(io))
		{
			term_box_t *cb = peel(io);
			apr_status_t rs;
			rs = outlet_buffer_send(buf, cb->cons.head);
			if (rs != 0)
				return rs;
			io = cb->cons.tail;
		}
	}
	else
		return APR_BADARG;

	return APR_SUCCESS;
}

apr_status_t ol_socket_send(outlet_t *self, term_t io)
{
	ol_socket_data_t *data = self->data;
	if (data->is_closing)
		return APR_EOF;
	return outlet_buffer_send(data->out_buf, io);
}

apr_status_t ol_socket_set_option(outlet_t *self, term_t opt, term_t value)
{
	ol_socket_data_t *data = self->data;
	if (opt == A_EXPECT)
	{
		if (!is_int(value))
			return APR_BADARG;
		data->expected_size = int_value(value);
		if (data->expected_size < 0)
			return APR_BADARG;

		if (self->owner_in == 0)
			return APR_ENOPROC;

		//enough data may already be there
		if (data->expected_size == 0 && buffer_len(data->in_buf) > 0 ||
			data->expected_size > 0 && buffer_len(data->in_buf) >= data->expected_size)
		{
			proc_t *proc = self->owner_in;
			int len = (data->expected_size == 0)
				?buffer_len(data->in_buf)
				:data->expected_size;
			term_t bin = heap_binary(proc->heap, len*8, buffer_ptr(data->in_buf));
			term_t msg = heap_tuple3(proc->heap, A_TCP, outlet_id(self), bin);
			scheduler_new_local_mail(proc->teevm->scheduler, proc, msg);
			buffer_consume(data->in_buf, len);
		}
		else
			data->packet_expected = 1;
	}
	else if (opt == A_REQUIRE)
	{
		int size;
		if (!is_int(value))
			return APR_BADARG;
		size = int_value(value);
		if (size < 0 || size > SOCK_OUTBUF_LEN)
			return APR_BADARG;

		data->required_size = size;

		if (buffer_available(data->out_buf) >= size)
		{
			proc_t *proc = self->owner_out;
			int avail = buffer_available(data->out_buf);
			term_t msg = heap_tuple3(proc->heap, A_TCP_SPACE, outlet_id(self), tag_int(avail));

			//TODO: insure that only owner can send to socket
			scheduler_new_local_mail(proc->teevm->scheduler, proc, msg);

			data->space_required = 0;
		}
		else
			data->space_required = 1;
	}
	else
		return APR_BADARG;
	return APR_SUCCESS;
}

apr_status_t ol_socket_close(outlet_t *self)
{
	ol_socket_data_t *data = self->data;

	data->is_closing = 1;

	return APR_SUCCESS;
}

apr_status_t ol_socket_close0(outlet_t *self)
{
	ol_socket_data_t *data = self->data;
	proc_t *proc = self->owner_in;
	if (proc)
	{
		int len = buffer_len(data->in_buf);
		term_t msg;

		if (len > 0)
		{
			term_t bin = heap_binary(proc->heap, len*8, buffer_ptr(data->in_buf));
			msg = heap_tuple3(proc->heap, A_TCP, outlet_id(self), bin);
			scheduler_new_local_mail(proc->teevm->scheduler, proc, msg);
			buffer_clear(data->in_buf);
		}

		msg = heap_tuple2(proc->heap, A_TCP_CLOSED, outlet_id(self));
		scheduler_new_local_mail(proc->teevm->scheduler, proc, msg);
	}

	return apr_socket_close(data->sock);
}

// EOF
