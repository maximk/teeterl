#ifndef _PORT_H
#define _PORT_H

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

#include <apr_network_io.h>
#include <apr_file_io.h>
#include <apr_time.h>

#include "term.h"
#include "xpool.h"

typedef struct port_t port_t;
struct port_t {
	apr_uint32_t key;		//unique serial number

	term_t owner_in;		// owner_in:	deliver messages to
	term_t owner_out;		// owner_out:	controls sending of messages

	apr_pool_t *pool;
	xpool_t *xp;

	int (*want_read)(port_t *self);
	int (*want_write)(port_t *self);

	int (*is_socket)(port_t *self);
	apr_socket_t *(*get_socket)(port_t *self);
	int (*is_file)(port_t *self);
	apr_file_t *(*get_file)(port_t *self);

	apr_status_t (*try_read)(port_t *self, apr_interval_time_t tick);
	apr_status_t (*try_write)(port_t *self, apr_interval_time_t tick);

	apr_status_t (*do_readable)(port_t *self);
	apr_status_t (*do_writable)(port_t *self);

	apr_status_t (*send)(port_t *self, term_t io);
	apr_status_t (*set_option)(port_t *self, term_t opt, term_t value);

	apr_status_t (*read)(port_t *self, apr_byte_t *buf, apr_size_t *len);
	apr_status_t (*write)(port_t *self, apr_byte_t *buf, apr_size_t *len);

	apr_status_t (*close)(port_t *self);
	apr_status_t (*close0)(port_t *self); 	//error, no lingering

	void *data;
};

extern term_t my_node;
extern term_t my_prev_node;

extern apr_uint32_t my_creation;

apr_uint32_t get_next_port_key(void);
void port_register(port_t *port);
void port_unregister(port_t *port);
port_t *port_lookup(apr_uint32_t serial);
term_t port_id(port_t *self, xpool_t *xp);
term_t port_get_list(xpool_t *xp);
term_t port_get_info(port_t *port, term_t item);
apr_status_t ports_poll(apr_interval_time_t tick);
apr_status_t port_send(port_t *self, term_t io);
apr_status_t port_set_option(port_t *self, term_t opt, term_t value);
apr_status_t port_close(port_t *self);

//socket constructors
port_t *port_socket_make(apr_socket_t *sock, int is_connecting);
port_t *port_listener_make(apr_socket_t *sock);
port_t *port_udp_make(apr_socket_t *sock);
port_t *port_pipe_make(apr_file_t *in, apr_file_t *out, apr_pool_t *pool);
port_t *port_file_make(apr_file_t *io);

#endif
