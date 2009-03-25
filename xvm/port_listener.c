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

typedef struct port_listener_data_t port_listener_data_t;
struct port_listener_data_t {
	apr_socket_t *sock;
	int is_accepting;
	apr_uint32_t reply_to_key;
};

int port_listener_want_read(port_t *self);
int port_listener_want_write(port_t *self);
int port_listener_is_socket(port_t *self);
apr_socket_t *port_listener_get_socket(port_t *self);
int port_listener_is_file(port_t *self);
apr_file_t *port_listener_get_file(port_t *self);
apr_status_t port_listener_try_read(port_t *self, apr_interval_time_t tick);
apr_status_t port_listener_try_write(port_t *self, apr_interval_time_t tick);
apr_status_t port_listener_do_readable(port_t *self);
apr_status_t port_listener_do_writable(port_t *self);
apr_status_t port_listener_send(port_t *self, term_t io);

apr_status_t port_listener_read(port_t *self, apr_byte_t *buf, apr_size_t *len);
apr_status_t port_listener_write(port_t *self, apr_byte_t *buf, apr_size_t *len);

apr_status_t port_listener_set_option(port_t *self, term_t opt, term_t value);
apr_status_t port_listener_close(port_t *self);
apr_status_t port_listener_close0(port_t *self);

port_t *port_listener_make(apr_socket_t *sock)
{
	port_listener_data_t *data;

	apr_pool_t *pool = apr_socket_pool_get(sock);
	port_t *port = apr_palloc(pool, sizeof(port_t));
	port->key = get_next_port_key();
	port->owner_in = port->owner_out = AI_UNDEFINED;
	port->pool = pool;
	port->xp = xpool_make(pool);

	port->want_read = port_listener_want_read;
	port->want_write = port_listener_want_write;
	port->is_socket = port_listener_is_socket;
	port->get_socket = port_listener_get_socket;
	port->is_file = port_listener_is_file;
	port->get_file = port_listener_get_file;

	port->try_read = port_listener_try_read;
	port->try_write = port_listener_try_write;

	port->do_readable = port_listener_do_readable;
	port->do_writable = port_listener_do_writable;
	port->send = port_listener_send;

	port->read = port_listener_read;
	port->write = port_listener_write;

	port->set_option = port_listener_set_option;
	port->close = port_listener_close;
	port->close0 = port_listener_close0;

	data = xalloc(port->xp, sizeof(port_listener_data_t));
	data->sock = sock;
	data->is_accepting = 0;
	data->reply_to_key = 0;

	port->data = data;
	return port;
}

int port_listener_want_read(port_t *self)
{
	port_listener_data_t *data = self->data;
	return data->is_accepting;
}

int port_listener_want_write(port_t *self)
{
	return 0;
}

int port_listener_is_socket(port_t *self)
{
	return 1;
}

apr_socket_t *port_listener_get_socket(port_t *self)
{
	port_listener_data_t *data = self->data;
	return data->sock;
}

int port_listener_is_file(port_t *self)
{
	return 0;
}

apr_file_t *port_listener_get_file(port_t *self)
{
	return 0;
}

apr_status_t port_listener_try_read(port_t *self, apr_interval_time_t tick)
{
	return APR_EOF;
}

apr_status_t port_listener_try_write(port_t *self, apr_interval_time_t tick)
{
	return APR_EOF;
}

apr_status_t port_listener_do_readable(port_t *self)
{
	apr_status_t rs;
	port_listener_data_t *data = self->data;

	apr_socket_t *new_sock;
	port_t *new_port;

	term_t msg;
	process_t *proc;

	proc = proc_lookup(data->reply_to_key);
	if (proc)
	{
		apr_pool_t *p;
		xpool_t *tmp;

		apr_pool_create(&p, 0);
		rs = apr_socket_accept(&new_sock, data->sock, p);
		if (rs == 0)
			rs = apr_socket_opt_set(new_sock, APR_SO_NONBLOCK, 1);

		if (rs != 0)
		{
			apr_pool_destroy(p);
			return rs;
		}

		new_port = port_socket_make(new_sock, 0);

		//put to poll ring
		port_register(new_port);

		//messages delivered to caller of accept
		new_port->owner_in = new_port->owner_out = proc_pid(proc, new_port->xp);

		tmp = xpool_make(p);
		msg = make_tuple3(A_TCP_ACCEPTED, port_id(self, tmp), port_id(new_port, tmp), tmp);
		proc_new_mail(proc, msg);
		xpool_destroy(tmp);
	}

	data->is_accepting = 0;

	return APR_SUCCESS;
}

apr_status_t port_listener_do_writable(port_t *self)
{
	return APR_ENOSOCKET;
}

apr_status_t port_listener_send(port_t *self, term_t io)
{
	return APR_ENOSOCKET;
}

apr_status_t port_listener_read(port_t *self, apr_byte_t *buf, apr_size_t *len)
{
	return APR_ENOTIMPL;
}

apr_status_t port_listener_write(port_t *self, apr_byte_t *buf, apr_size_t *len)
{
	return APR_ENOTIMPL;
}


apr_status_t port_listener_set_option(port_t *self, term_t opt, term_t value)
{
	if (opt == A_ACCEPT)
	{
		port_listener_data_t *data = self->data;

		if (!is_pid(value))
			return APR_BADARG;

		data->is_accepting = 1;
		data->reply_to_key = pid_serial(value);
	}
	else
		return APR_BADARG;

	return APR_SUCCESS;
}

apr_status_t port_listener_close(port_t *self)
{
	return port_listener_close0(self);
}

apr_status_t port_listener_close0(port_t *self)
{
	port_listener_data_t *data = self->data;
	process_t *proc = proc_lookup(pid_serial(self->owner_in));
	if (proc)
	{
		xpool_t *tmp = xpool_make(self->pool);
		term_t msg = make_tuple2(A_TCP_CLOSED, port_id(self, tmp), tmp);
		proc_new_mail(proc, msg);
		xpool_destroy(tmp);
	}

	return apr_socket_close(data->sock);
}

// EOF
