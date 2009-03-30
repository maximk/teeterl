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

#include "port.h"

#include "proc.h"
#include "atom.h"
#include "buffer.h"

#define FILE_INBUF_LEN		4096
#define FILE_OUTBUF_LEN		4096

typedef struct port_pipe_data_t port_pipe_data_t;
struct port_pipe_data_t {
	apr_file_t *input;
	apr_file_t *output;
	int is_closing;
	buffer_t *in_buf;
	buffer_t *out_buf;
	int packet_expected;
	int expected_size;
	int space_required;
	int required_size;
};

int port_pipe_want_read(port_t *self);
int port_pipe_want_write(port_t *self);
int port_pipe_is_socket(port_t *self);
apr_socket_t *port_pipe_get_socket(port_t *self);
int port_pipe_is_file(port_t *self);
apr_file_t *port_pipe_get_file(port_t *self);
apr_status_t port_pipe_try_read(port_t *self, apr_interval_time_t tick);
apr_status_t port_pipe_try_write(port_t *self, apr_interval_time_t tick);
apr_status_t port_pipe_do_readable(port_t *self);
apr_status_t port_pipe_do_writable(port_t *self);
apr_status_t port_pipe_send(port_t *self, term_t io);

apr_status_t port_pipe_read(port_t *self, apr_byte_t *buf, apr_size_t *len);
apr_status_t port_pipe_write(port_t *self, apr_byte_t *buf, apr_size_t *len);

apr_status_t port_pipe_set_option(port_t *self, term_t opt, term_t value);
apr_status_t port_pipe_close(port_t *self);
apr_status_t port_pipe_close0(port_t *self);

port_t *port_pipe_make(apr_file_t *input, apr_file_t *output, apr_pool_t *pool)
{
	port_pipe_data_t *data;

	port_t *port = apr_palloc(pool, sizeof(port_t));
	port->key = get_next_port_key();
	port->owner_in = port->owner_out = AI_UNDEFINED;
	port->pool = pool;
	port->xp = xpool_make(pool);

	port->want_read = port_pipe_want_read;
	port->want_write = port_pipe_want_write;
	port->is_socket = port_pipe_is_socket;
	port->get_socket = port_pipe_get_socket;
	port->is_file = port_pipe_is_file;
	port->get_file = port_pipe_get_file;

	port->try_read = port_pipe_try_read;
	port->try_write = port_pipe_try_write;

	port->do_readable = port_pipe_do_readable;
	port->do_writable = port_pipe_do_writable;
	port->send = port_pipe_send;

	port->read = port_pipe_read;
	port->write = port_pipe_write;

	port->set_option = port_pipe_set_option;
	port->close = port_pipe_close;
	port->close0 = port_pipe_close0;

	data = xalloc(port->xp, sizeof(port_pipe_data_t));
	data->input = input;
	data->output = output;
	data->is_closing = 0;
	data->in_buf = buffer_make(port->xp, FILE_INBUF_LEN);
	data->out_buf = buffer_make(port->xp, FILE_OUTBUF_LEN);
	data->packet_expected = 0;
	data->expected_size = 0;
	data->space_required = 0;
	data->required_size = 0;

	port->data = data;
	return port;
}

int port_pipe_want_read(port_t *self)
{
	port_pipe_data_t *data = self->data;
	if (data->is_closing)
		return 0;
	return buffer_available(data->in_buf) > 0;
}

int port_pipe_want_write(port_t *self)
{
	port_pipe_data_t *data = self->data;
	if (data->is_closing)
		return 1;
	return buffer_len(data->out_buf) > 0;
}

int port_pipe_is_socket(port_t *self)
{
	return 0;
}

apr_socket_t *port_pipe_get_socket(port_t *self)
{
	return 0;
}

int port_pipe_is_file(port_t *self)
{
	return 1;
}

apr_file_t *port_pipe_get_file(port_t *self)
{
	port_pipe_data_t *data = self->data;
	return data->input;		//the same as data->output if this call is used
}

apr_status_t port_pipe_try_read(port_t *self, apr_interval_time_t tick)
{
	apr_status_t rs;
	port_pipe_data_t *data = self->data;
	apr_size_t len = buffer_available(data->in_buf);

	rs = apr_file_pipe_timeout_set(data->input, tick);
	if (rs == 0)
		rs = buffer_file_read(data->in_buf, data->input, len);
	if (rs == 0)
	{
		if (data->packet_expected)
		{
			process_t *proc;
			int len = buffer_len(data->in_buf);

			if (data->expected_size == 0 ||
				data->expected_size > 0 && len >= data->expected_size)
			{

				if (data->expected_size > 0)
					len = data->expected_size;

				proc = proc_lookup(pid_serial(self->owner_in));
				if (proc)
				{
					xpool_t *tmp = xpool_make(self->pool);
					term_t bin = make_binary(intnum(len), buffer_ptr(data->in_buf), tmp);
					term_t msg = make_tuple3(A_DATA, port_id(self, tmp), bin, tmp);
					proc_new_mail(proc, msg);
					buffer_consume(data->in_buf, len);
					xpool_destroy(tmp);
				}

				data->packet_expected = 0;
				data->expected_size = 0;
			}
		}
	}
	else if (APR_STATUS_IS_EAGAIN(rs))
		rs = 0;

	return rs;
}

apr_status_t port_pipe_try_write(port_t *self, apr_interval_time_t tick)
{
	apr_status_t rs;
	port_pipe_data_t *data = self->data;

	rs = apr_file_pipe_timeout_set(data->output, tick);
	if (rs == 0)
	{
		rs = buffer_file_write(data->out_buf, data->output);
		if (rs != 0)
			return rs;
	}

    if (data->is_closing && buffer_len(data->out_buf) == 0)
		return APR_EOF;	//make poll close0 the file

	if (data->space_required && buffer_available(data->out_buf) >= data->required_size)
	{
		xpool_t *tmp = xpool_make(self->pool);
		int avail = buffer_available(data->out_buf);
		term_t msg = make_tuple3(A_SPACE, port_id(self, tmp), intnum(avail), tmp);
		process_t *proc = proc_lookup(pid_serial(self->owner_out));

		//TODO: insure that only owner can send to file

		proc_new_mail(proc, msg);
		xpool_destroy(tmp);

		data->space_required = 0;
	}

	return APR_SUCCESS;
}

apr_status_t port_pipe_do_readable(port_t *self)
{
	return APR_ENOTIMPL;
}

apr_status_t port_pipe_do_writable(port_t *self)
{
	return APR_ENOTIMPL;
}

static apr_status_t port_buffer_send(buffer_t *buf, term_t io)
{
	int avail;

	if (is_nil(io))
		return 0;

	avail = buffer_available(buf);
	if (is_int(io))
	{
		if (avail == 0)
			return APR_EINCOMPLETE;
		else
			buffer_put_byte(buf, int_value(io));
	}
	else if (is_binary(io))
	{
		int size = int_value2(bin_size(io));
		if (size > avail)
		{
			buffer_put_data(buf, bin_data(io), avail);
			return APR_EINCOMPLETE;
		}
		else
			buffer_put_data(buf, bin_data(io), size);
	}
	if (is_list(io))
	{
		while (is_cons(io))
		{
			apr_status_t rs;
			rs = port_buffer_send(buf, lst_value(io));
			if (rs != 0)
				return rs;
			io = lst_next(io);
		}
	}

	return APR_SUCCESS;
}

apr_status_t port_pipe_send(port_t *self, term_t io)
{
	port_pipe_data_t *data = self->data;
	if (data->is_closing)
		return APR_EOF;
	return port_buffer_send(data->out_buf, io);
}

apr_status_t port_pipe_read(port_t *self, apr_byte_t *buf, apr_size_t *len)
{
	return APR_ENOTIMPL;
}

apr_status_t port_pipe_write(port_t *self, apr_byte_t *buf, apr_size_t *len)
{
	return APR_ENOTIMPL;
}

apr_status_t port_pipe_set_option(port_t *self, term_t opt, term_t value)
{
	port_pipe_data_t *data = self->data;
	if (opt == A_EXPECT)
	{
		if (!is_int(value))
			return APR_BADARG;
		data->expected_size = int_value2(value);
		if (data->expected_size < 0)
			return APR_BADARG;

		if (!is_pid(self->owner_in))
			return APR_ENOPROC;

		//enough data may already be there
		if (data->expected_size == 0 && buffer_len(data->in_buf) > 0 ||
			data->expected_size > 0 && buffer_len(data->in_buf) >= data->expected_size)
		{
			int len = (data->expected_size == 0)
				?buffer_len(data->in_buf)
				:data->expected_size;
			xpool_t *tmp = xpool_make(self->pool);
			term_t bin = make_binary(intnum(len), buffer_ptr(data->in_buf), tmp);
			term_t msg = make_tuple3(A_DATA, port_id(self, tmp), bin, tmp);
			process_t *proc = proc_lookup(pid_serial(self->owner_in));

			proc_new_mail(proc, msg);
			buffer_consume(data->in_buf, len);
			xpool_destroy(tmp);
		}
		else
			data->packet_expected = 1;
	}
	else if (opt == A_REQUIRE)
	{
		int size;
		if (!is_int(value))
			return APR_BADARG;
		size = int_value2(value);
		if (size < 0 || size > FILE_OUTBUF_LEN)
			return APR_BADARG;

		data->required_size = size;

		if (buffer_available(data->out_buf) >= size)
		{
			xpool_t *tmp = xpool_make(self->pool);
			int avail = buffer_available(data->out_buf);
			term_t msg = make_tuple3(A_SPACE, port_id(self, tmp), intnum(avail), tmp);
			process_t *proc = proc_lookup(pid_serial(self->owner_out));

			//TODO: insure that only owner can send to file

			proc_new_mail(proc, msg);
			xpool_destroy(tmp);

			data->space_required = 0;
		}
		else
			data->space_required = 1;
	}
	else
		return APR_BADARG;
	return APR_SUCCESS;
}

apr_status_t port_pipe_close(port_t *self)
{
	port_pipe_data_t *data = self->data;

	data->is_closing = 1;

	return APR_SUCCESS;
}

apr_status_t port_pipe_close0(port_t *self)
{
	port_pipe_data_t *data = self->data;
	process_t *proc = proc_lookup(pid_serial(self->owner_in));
	if (proc)
	{
		xpool_t *tmp = xpool_make(self->pool);
		int len = buffer_len(data->in_buf);
		term_t msg;

		if (len > 0)
		{
			term_t bin = make_binary(intnum(len), buffer_ptr(data->in_buf), tmp);

			msg = make_tuple3(A_DATA, port_id(self, tmp), bin, tmp);
			proc_new_mail(proc, msg);
			buffer_clear(data->in_buf);
		}

		msg = make_tuple2(A_CLOSED, port_id(self, tmp), tmp);
		proc_new_mail(proc, msg);
		xpool_destroy(tmp);
	}

	apr_file_close(data->input);
	apr_file_close(data->output);

	return APR_SUCCESS;
}

//EOF
