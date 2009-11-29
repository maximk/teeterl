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

typedef struct port_file_data_t port_file_data_t;
struct port_file_data_t {
	apr_file_t *io;
};

int port_file_want_read(port_t *self);
int port_file_want_write(port_t *self);
int port_file_is_socket(port_t *self);
apr_socket_t *port_file_get_socket(port_t *self);
int port_file_is_file(port_t *self);
apr_file_t *port_file_get_file(port_t *self);
apr_status_t port_file_try_read(port_t *self, apr_interval_time_t tick);
apr_status_t port_file_try_write(port_t *self, apr_interval_time_t tick);
apr_status_t port_file_do_readable(port_t *self);
apr_status_t port_file_do_writable(port_t *self);
apr_status_t port_file_send(port_t *self, term_t io);

apr_status_t port_file_read(port_t *self, apr_byte_t *buf, apr_size_t *len);
apr_status_t port_file_write(port_t *self, apr_byte_t *buf, apr_size_t *len);

apr_status_t port_file_set_option(port_t *self, term_t opt, term_t value);
apr_status_t port_file_close(port_t *self);
apr_status_t port_file_close0(port_t *self);

port_t *port_file_make(apr_file_t *io)
{
	port_file_data_t *data;
	apr_pool_t *pool = apr_file_pool_get(io);

	port_t *port = apr_palloc(pool, sizeof(port_t));
	port->key = get_next_port_key();
	port->owner_in = port->owner_out = AI_UNDEFINED;
	port->pool = pool;
	port->xp = xpool_make(pool);

	port->want_read = port_file_want_read;
	port->want_write = port_file_want_write;
	port->is_socket = port_file_is_socket;
	port->get_socket = port_file_get_socket;
	port->is_file = port_file_is_file;
	port->get_file = port_file_get_file;

	port->try_read = port_file_try_read;
	port->try_write = port_file_try_write;

	port->do_readable = port_file_do_readable;
	port->do_writable = port_file_do_writable;
	port->send = port_file_send;
	port->set_option = port_file_set_option;

	port->read = port_file_read;
	port->write = port_file_write;

	port->close = port_file_close;
	port->close0 = port_file_close0;

	data = xalloc(port->xp, sizeof(port_file_data_t));
	data->io = io;

	port->data = data;
	return port;
}

int port_file_want_read(port_t *self)
{
	return 0;
}

int port_file_want_write(port_t *self)
{
	return 0;
}

int port_file_is_socket(port_t *self)
{
	return 0;
}

apr_socket_t *port_file_get_socket(port_t *self)
{
	return 0;
}

int port_file_is_file(port_t *self)
{
	return 1;
}

apr_file_t *port_file_get_file(port_t *self)
{
	port_file_data_t *data = self->data;
	return data->io;
}

apr_status_t port_file_try_read(port_t *self, apr_interval_time_t tick)
{
	return APR_ENOTIMPL;
}

apr_status_t port_file_try_write(port_t *self, apr_interval_time_t tick)
{
	return APR_ENOTIMPL;
}

apr_status_t port_file_do_readable(port_t *self)
{
	return APR_ENOTIMPL;
}

apr_status_t port_file_do_writable(port_t *self)
{
	return APR_ENOTIMPL;
}

apr_status_t port_file_send(port_t *self, term_t io)
{
	return APR_ENOTIMPL;
}

apr_status_t port_file_read(port_t *self, apr_byte_t *buf, apr_size_t *len)
{
	port_file_data_t *data = self->data;
	return apr_file_read(data->io, buf, len);
}

apr_status_t port_file_write(port_t *self, apr_byte_t *buf, apr_size_t *len)
{
	port_file_data_t *data = self->data;
	return apr_file_write(data->io, buf, len);
}

apr_status_t port_file_set_option(port_t *self, term_t opt, term_t value)
{
	return APR_BADARG;
}

apr_status_t port_file_close(port_t *self)
{
	port_file_close0(self);
	port_unregister(self);
	apr_pool_destroy(self->pool);
	return APR_SUCCESS;
}

apr_status_t port_file_close0(port_t *self)
{
	port_file_data_t *data = self->data;
	return apr_file_close(data->io);
}

//EOF
