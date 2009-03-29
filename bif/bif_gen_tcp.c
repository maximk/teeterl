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

term_t bif_controlling_process2(term_t Sock, term_t Pid, process_t *ctx)
{
	// controlling process changes the process which can send to the socket
	// the other owner which receives closed messages, etc remains intact

	port_t *port;

	if (!is_port(Sock) || !is_pid(Pid))
		return A_BADARG;

	port = port_lookup(prp_serial(Sock));
	if (port != 0)
	{
		//TODO: check for permission
		port->owner_out = marshal_term(Pid, port->xp);
	}
	result(A_OK);
	return AI_OK;

	//return A_NOT_SUPPORTED;
}

term_t bif_connect_socket4(term_t RemIP, term_t RemPort, term_t LocIP, term_t LocPort, process_t *ctx)
{
	apr_status_t rs;
	apr_pool_t *p;
	apr_sockaddr_t *sa1, *sa2;
	apr_socket_t *socket;
	port_t *port;
	term_t id;

	const char *host1, *host2;
	apr_port_t port1, port2;

	if (!is_binary(RemIP) || !is_int(RemPort))
		return A_BADARG;
	if (LocIP != A_ANY && !is_binary(LocIP))
		return A_BADARG;
	if (!is_int(LocPort))
		return A_BADARG;

	host1 = (const char *)bin_data(RemIP);
	host2 = (LocIP == A_ANY) ?0 :(const char *)bin_data(LocIP);
	port1 = (apr_port_t)int_value(RemPort);
	port2 = (apr_port_t)int_value(LocPort);

	apr_pool_create(&p, 0);

	rs = apr_sockaddr_info_get(&sa1, host1, APR_INET, port1, 0, p);
	if (rs == 0)
		rs = apr_socket_create(&socket,
			APR_INET, SOCK_STREAM, APR_PROTO_TCP, p); //only APR_INET is supported, not APR_INET6
	if (rs == 0 && LocIP != A_ANY)
	{
		rs = apr_sockaddr_info_get(&sa2, host2, APR_INET, port2, 0, p);
		if (rs == 0)
			rs = apr_socket_bind(socket, sa2);
	}
	if (rs == 0)
		rs = apr_socket_opt_set(socket, APR_SO_NONBLOCK, 1);
	if (rs == 0)
		rs = apr_socket_connect(socket, sa1);

	if (rs != 0)
	{
		apr_pool_destroy(p);
		return decipher_status(rs);
	}

	port = port_socket_make(socket, 1);	//construct a connecting port, takes care of pool p

	//set initial port owner
	port->owner_in = port->owner_out = proc_pid(ctx, port->xp);

	//put port to polling ring
	port_register(port);

	id = make_port(my_node, port->key, my_creation, proc_gc_pool(ctx));
	result(id);
	return AI_OK;
}

term_t bif_listen_socket2(term_t LocIP, term_t LocPort, process_t *ctx)
{
	apr_status_t rs;
	apr_pool_t *p;
	apr_sockaddr_t *sa;
	apr_socket_t *socket;
	port_t *port;
	term_t id;

	const char *host;
	apr_port_t tcp_port;

	if (LocIP != A_ANY && !is_binary(LocIP))
		return A_BADARG;
	if (!is_int(LocPort))
		return A_BADARG;

	host = (LocIP == A_ANY) ?0 :(const char *)bin_data(LocIP);
	tcp_port = (apr_port_t)int_value(LocPort);

	apr_pool_create(&p, 0);

	rs = apr_sockaddr_info_get(&sa, host, APR_INET, tcp_port, 0, p);
	if (rs == 0)
		rs = apr_socket_create(&socket,
			APR_INET, SOCK_STREAM, APR_PROTO_TCP, p); //only APR_INET is supported, not APR_INET6
	if (rs == 0)
		rs = apr_socket_bind(socket, sa);
	if (rs == 0)
		rs = apr_socket_listen(socket, 8);
	if (rs == 0)
		rs = apr_socket_opt_set(socket, APR_SO_NONBLOCK, 1);

	if (rs != 0)
	{
		apr_pool_destroy(p);
		return decipher_status(rs);
	}

	port = port_listener_make(socket);	//takes care of pool p

	//add to poll ring
	port_register(port);

	//set initial port owner
	//
	//keep owners underfines so that socket does not close
	//when the process exits -- other processes may accept
	//connections using listening socket
	//
	//port->owner_in = port->owner_out = proc_pid(ctx, port->xp);
	port->owner_in = port->owner_out = A_UNDEFINED;

	id = make_port(my_node, port->key, my_creation, proc_gc_pool(ctx));
	result(id);
	return AI_OK;
}

//EOF
