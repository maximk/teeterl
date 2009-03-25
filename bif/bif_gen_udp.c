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

#include "bif.h"

#include <apr_network_io.h>

#include "port.h"
#include "errors.h"

term_t bif_open_socket2(term_t LocIP, term_t LocPort, process_t *ctx)
{
	apr_status_t rs;
	apr_pool_t *p;
	apr_sockaddr_t *sa;
	apr_socket_t *socket;
	port_t *port;
	term_t id;

	const char *host;
	apr_port_t udp_port;

	if (LocIP != A_ANY && !is_binary(LocIP))
		return A_BADARG;
	if (!is_int(LocPort))
		return A_BADARG;

	host = (LocIP == A_ANY) ?0 :(const char *)bin_data(LocIP);
	udp_port = int_value(LocPort);

	apr_pool_create(&p, 0);

	rs = apr_sockaddr_info_get(&sa, host, APR_INET, udp_port, 0, p);
	if (rs == 0)
		rs = apr_socket_create(&socket,
			APR_INET, SOCK_DGRAM, APR_PROTO_UDP, p); //only APR_INET is supported, not APR_INET6
	if (rs == 0)
		rs = apr_socket_bind(socket, sa);
	if (rs == 0)
		rs = apr_socket_opt_set(socket, APR_SO_NONBLOCK, 1);

	if (rs != 0)
	{
		apr_pool_destroy(p);
		return decipher_status(rs);
	}

	port = port_udp_make(socket);	//takes care of pool p

	//add to poll ring
	port_register(port);

	//set initial port owner
	port->owner_in = port->owner_out = proc_pid(ctx, port->xp);

	id = make_port(my_node, port->key, my_creation, proc_gc_pool(ctx));
	result(id);
	return AI_OK;
}

term_t bif_sendto4(term_t Sock, term_t RemIP, term_t RemPort, term_t Bin, process_t *ctx)
{
	apr_status_t rs;
	port_t *port;
	apr_socket_t *sock;
	const char *host;
	int udp_port;
	apr_sockaddr_t *sa;
	apr_pool_t *p;

	if (!is_port(Sock))
		return A_BADARG;
	if (!is_binary(RemIP) || !is_int(RemPort))
		return A_BADARG;
	if (!is_binary(Bin))
		return A_BADARG;

	port = port_lookup(prp_serial(Sock));
	if (port == 0)
		return A_CLOSED;
	if (!port->is_socket(port))
		return A_BADARG;
	sock = port->get_socket(port);

	host = (const char *)bin_data(RemIP);
	udp_port = int_value(RemPort);

	apr_pool_create(&p, 0);
	rs = apr_sockaddr_info_get(&sa, host, APR_INET, udp_port, 0, p);
	if (rs == 0)
	{
		apr_size_t len = int_value(bin_size(Bin));
		rs = apr_socket_sendto(sock, sa, 0, (const char *)bin_data(Bin), &len);
	}

	if (rs != 0)
	{
		apr_pool_destroy(p);
		return decipher_status(rs);
	}

	result(A_OK);
	return AI_OK;
}

//EOF
