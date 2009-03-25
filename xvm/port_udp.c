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

#define SOCK_INBUF_LEN 4096
#define SOCK_OUTBUF_LEN 4096

typedef struct port_udp_data_t port_udp_data_t;
struct port_udp_data_t {
	apr_socket_t *sock;
	int dgram_expected;
	int expected_size;
};

int port_udp_want_read(port_t *self);
int port_udp_want_write(port_t *self);
int port_udp_is_socket(port_t *self);
apr_socket_t *port_udp_get_socket(port_t *self);
int port_udp_is_file(port_t *self);
apr_file_t *port_udp_get_file(port_t *self);
apr_status_t port_udp_do_readable(port_t *self);
apr_status_t port_udp_do_writable(port_t *self);
apr_status_t port_udp_send(port_t *self, term_t io);

apr_status_t port_udp_read(port_t *self, apr_byte_t *buf, apr_size_t *len);
apr_status_t port_udp_write(port_t *self, apr_byte_t *buf, apr_size_t *len);

apr_status_t port_udp_set_option(port_t *self, term_t opt, term_t value);
apr_status_t port_udp_close(port_t *self);
apr_status_t port_udp_close0(port_t *self);

port_t *port_udp_make(apr_socket_t *sock)
{
	port_udp_data_t *data;

	apr_pool_t *pool = apr_socket_pool_get(sock);
	port_t *port = apr_palloc(pool, sizeof(port_t));
	port->key = get_next_port_key();
	port->owner_in = port->owner_out = AI_UNDEFINED;
	port->pool = pool;
	port->xp = xpool_make(pool);

	port->want_read = port_udp_want_read;
	port->want_write = port_udp_want_write;
	port->is_socket = port_udp_is_socket;
	port->get_socket = port_udp_get_socket;
	port->is_file = port_udp_is_file;
	port->get_file = port_udp_get_file;
	port->try_read = 0;	//TODO: add stubs
	port->try_write = 0;
	port->do_readable = port_udp_do_readable;
	port->do_writable = port_udp_do_writable;
	port->send = port_udp_send;

	port->read = port_udp_read;
	port->write = port_udp_write;

	port->set_option = port_udp_set_option;
	port->close = port_udp_close;
	port->close0 = port_udp_close0;

	data = xalloc(port->xp, sizeof(port_udp_data_t));
	data->sock = sock;
	data->dgram_expected = 0;
	data->expected_size = 0;

	port->data = data;
	return port;
}

int port_udp_want_read(port_t *self)
{
	port_udp_data_t *data = self->data;
	return data->dgram_expected;
}

int port_udp_want_write(port_t *self)
{
	return 0;
}

int port_udp_is_socket(port_t *self)
{
	return 1;
}

apr_socket_t *port_udp_get_socket(port_t *self)
{
	port_udp_data_t *data = self->data;
	return data->sock;
}

int port_udp_is_file(port_t *self)
{
	return 0;
}

apr_file_t *port_udp_get_file(port_t *self)
{
	return 0;
}

apr_status_t port_udp_do_readable(port_t *self)
{
	apr_status_t rs;
	port_udp_data_t *data = self->data;
	apr_sockaddr_t from;
	apr_byte_t buf[65536];
	apr_size_t len;

	len = sizeof(buf);
	rs = apr_socket_recvfrom(&from, data->sock, 0, (char *)buf, &len);
	if (rs == 0)
	{
		process_t *proc = proc_lookup(pid_serial(self->owner_in));
		if (proc)
		{
			//{udp, Socket, IP, InPortNo, Packet}

			xpool_t *tmp = xpool_make(self->pool);
			term_t ip, port;
			term_t bin, msg;

			ip = A_UNDEFINED; //TODO: get from sa.sin_addr.s_addr

			port = intnum(from.port);
			bin = make_binary(intnum(len), buf, tmp);
			msg = make_tuple5(A_UDP, port_id(self, tmp), ip, port, bin, tmp);
			proc_new_mail(proc, msg);
			xpool_destroy(tmp);
		}

		data->dgram_expected = 0;
	}

	return rs;
}

apr_status_t port_udp_do_writable(port_t *self)
{
	return APR_EOF;
}

apr_status_t port_udp_send(port_t *self, term_t io)
{
	return APR_EOF;
}

apr_status_t port_udp_read(port_t *self, apr_byte_t *buf, apr_size_t *len)
{
	return APR_ENOTIMPL;
}

apr_status_t port_udp_write(port_t *self, apr_byte_t *buf, apr_size_t *len)
{
	return APR_ENOTIMPL;
}

apr_status_t port_udp_set_option(port_t *self, term_t opt, term_t value)
{
	port_udp_data_t *data = self->data;
	if (opt == A_EXPECT)
	{
		if (!is_int(value))
			return APR_BADARG;
		data->expected_size = int_value(value);	//ignored

		if (!is_pid(self->owner_in))
			return APR_ENOPROC;

		data->dgram_expected = 1;
	}
	else
		return APR_BADARG;
	return APR_SUCCESS;
}

apr_status_t port_udp_close(port_t *self)
{
	return port_udp_close0(self);
}

apr_status_t port_udp_close0(port_t *self)
{
	port_udp_data_t *data = self->data;
	process_t *proc = proc_lookup(pid_serial(self->owner_in));
	if (proc)
	{
		xpool_t *tmp = xpool_make(self->pool);
		term_t msg = make_tuple2(A_UDP_CLOSED, port_id(self, tmp), tmp);
		proc_new_mail(proc, msg);
		xpool_destroy(tmp);
	}

	return apr_socket_close(data->sock);
}

// EOF
