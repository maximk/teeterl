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

#include <apr_hash.h>
#include <apr_time.h>
#include <apr_poll.h>

#include "atom.h"

static apr_hash_t *ports = 0;
static apr_uint32_t next_port = 0;

apr_uint32_t get_next_port_key(void) {
	return next_port++;
}

void port_register(port_t *port)
{
	if (ports == 0)
	{
		apr_pool_t *p;
		apr_pool_create(&p, 0);
		ports = apr_hash_make(p);
	}

	apr_hash_set(ports, &port->key, sizeof(port->key), port);
}

void port_unregister(port_t *port)
{
	apr_hash_set(ports, &port->key, sizeof(port->key), 0);
}

port_t *port_lookup(apr_uint32_t serial)
{
	return apr_hash_get(ports, &serial, sizeof(serial));
}

term_t port_id(port_t *self, xpool_t *xp)
{
	return make_port(my_node, self->key, my_creation, xp);
}

term_t port_get_list(xpool_t *xp)
{
	term_t l = nil;
	term_t cons = nil;
	apr_hash_index_t *hi;

	if (ports == 0)
		return nil;

	for (hi = apr_hash_first(0, ports); hi; hi = apr_hash_next(hi))
	{
		port_t *port;
		apr_hash_this(hi, 0, 0, (void **)&port);

		lst_add(l, cons, port_id(port, xp), xp);
	}

	return l;
}

term_t port_get_info(port_t *port, term_t item)
{
	if (item == A_ID)
		return intnum(port->key);
	else if (item == A_CONNECTED)
		return port->owner_in;
	else
		return A_UNDEFINED;
}

apr_status_t ports_poll(apr_interval_time_t tick)
{
	apr_status_t rs = 0;

	apr_pool_t *tmp;
	apr_pollset_t *pollset;
	int file_count = 0;
	int poll_count = 0;

	apr_hash_index_t *hi;

	int num, i;
	const apr_pollfd_t *descriptors;

	if (ports == 0 || apr_hash_count(ports) == 0)
	{
		if (tick > 0)
			apr_sleep(tick);
		return 0;
	}

#if !APR_FILES_AS_SOCKETS
	for (hi = apr_hash_first(0, ports); hi; hi = apr_hash_next(hi))
	{
		apr_int16_t reqevents = 0;

		port_t *port;
		apr_hash_this(hi, 0, 0, &port);

		if (!port->is_file(port))
			continue;

		if (port->want_read(port))
		{
			file_count++;
			rs = port->try_read(port, tick/apr_hash_count(ports));
		}

		if (rs == 0 && port->want_write(port))
		{
			file_count++;
			rs = port->try_write(port, tick/apr_hash_count(ports));
		}

		if (rs != 0)
		{
			port->close0(port);
			apr_hash_set(ports, &port->key, sizeof(port->key), 0);
			apr_pool_destroy(port->pool);
		}
	}
#endif

	apr_pool_create(&tmp, 0);
	rs = apr_pollset_create(&pollset, apr_hash_count(ports), tmp, 0);
	if (rs != 0)
	{
		apr_pool_destroy(tmp);
		return rs;
	}

	for (hi = apr_hash_first(0, ports); hi; hi = apr_hash_next(hi))
	{
		apr_int16_t reqevents = 0;

		port_t *port;
		apr_hash_this(hi, 0, 0, (void **)&port);

#if !APR_FILES_AS_SOCKETS
		if (port->is_file(port))
			continue;
#endif

		if (port->want_read(port))
			reqevents |= APR_POLLIN;
		if (port->want_write(port))
			reqevents |= APR_POLLOUT;

		if (reqevents != 0)
		{
			apr_pollfd_t fd;

			if (port->is_socket(port))
			{
				fd.desc_type = APR_POLL_SOCKET;
				fd.desc.s = port->get_socket(port);
			}
			else if (port->is_file(port))
			{
				fd.desc_type = APR_POLL_FILE;
				fd.desc.f = port->get_file(port);
			}
			else
				return APR_ENOTIMPL;

			fd.reqevents = reqevents;
			fd.client_data = port;

			rs = apr_pollset_add(pollset, &fd);
			if (rs != 0)
			{
				apr_pool_destroy(tmp);
				return rs;
			}
			poll_count++;
		}
	}

	if (poll_count == 0)
	{
		if (file_count == 0 && tick > 0)
			apr_sleep(tick);
		apr_pool_destroy(tmp);
		return APR_SUCCESS;
	}

	rs = apr_pollset_poll(pollset, tick, &num, &descriptors);
	if (rs != 0)
	{
		apr_pool_destroy(tmp);
		return rs;
	}

	for (i = 0; i < num; i++)
	{
		port_t *port = descriptors[i].client_data;
		if ((descriptors[i].rtnevents & APR_POLLIN) != 0)
			rs = port->do_readable(port);
		if (rs == 0 && (descriptors[i].rtnevents & APR_POLLOUT) != 0)
			rs = port->do_writable(port);

		if (rs != 0)
		{
			port->close0(port);
			apr_hash_set(ports, &port->key, sizeof(port->key), 0);
			apr_pool_destroy(port->pool);
		}
	}

	apr_pool_destroy(tmp);
	return APR_SUCCESS;
}

apr_status_t port_send(port_t *self, term_t io)
{
	return self->send(self, io);
}

apr_status_t port_set_option(port_t *self, term_t opt, term_t value)
{
	return self->set_option(self, opt, value);
}

apr_status_t port_close(port_t *self)
{
	return self->close(self);
}

// EOF
