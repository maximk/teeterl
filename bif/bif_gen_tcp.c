//
//
//

#include "bifimpl.h"

#include <apr_network_io.h>

#include "outlet_mall.h"
#include "scheduler.h"

term_t bif_controlling_process2(term_t Sock, term_t Pid, proc_t *proc)
{
	// controlling process changes the process which can send to the socket
	// the other owner which receives closed messages, etc remains intact

	outlet_t *outlet;

	if (!is_short_oid(Sock) || !is_short_pid(Pid))
		bif_bad_arg0();

	outlet = outlet_mall_lookup(proc->teevm->mall, oid_serial(Sock));
	if (outlet != 0)
	{
		//TODO: check for permission
		outlet->owner_out = scheduler_lookup(proc->teevm->scheduler, pid_serial(Pid));
	}

	return A_OK;
}

term_t bif_connect_socket4(term_t RemIP, term_t RemPort, term_t LocIP, term_t LocPort, proc_t *proc)
{
	apr_status_t rs;
	apr_pool_t *p;
	apr_sockaddr_t *sa1, *sa2;
	apr_socket_t *socket;
	outlet_t *outlet;

	const char *host1, *host2;
	apr_port_t port1, port2;

	if (!is_binary(RemIP) || !is_int(RemPort))
		bif_bad_arg0();
	if (LocIP != A_ANY && !is_binary(LocIP))
		bif_bad_arg(LocIP);
	if (!is_int(LocPort))
		bif_bad_arg(LocPort);

	host1 = (const char *)peel(RemIP)->binary.data;
	host2 = (LocIP == A_ANY) ?0 :(const char *)peel(LocIP)->binary.data;
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
		bif_exception(decipher_status(rs));
	}

	outlet = ol_socket_make(socket, 1);	//construct a connecting outlet, takes care of pool p

	//set initial outlet owner
	outlet->owner_in = outlet->owner_out = proc;

	//put outlet to polling ring
	outlet_mall_allot(proc->teevm->mall, outlet);

	return outlet_id(outlet);
}

term_t bif_listen_socket2(term_t LocIP, term_t LocPort, proc_t *proc)
{
	apr_status_t rs;
	apr_pool_t *p;
	apr_sockaddr_t *sa;
	apr_socket_t *socket;
	outlet_t *outlet;

	const char *host;
	apr_port_t port;

	if (LocIP != A_ANY && !is_binary(LocIP))
		return A_BADARG;
	if (!is_int(LocPort))
		return A_BADARG;

	host = (LocIP == A_ANY) ?0 :(const char *)peel(LocIP)->binary.data;
	port = (apr_port_t)int_value(LocPort);

	apr_pool_create(&p, 0);

	rs = apr_sockaddr_info_get(&sa, host, APR_INET, port, 0, p);
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
		bif_exception(decipher_status(rs));
	}

	outlet = ol_listener_make(socket, proc->teevm);	//takes care of pool p

	//add to poll ring
	outlet_mall_allot(proc->teevm->mall, outlet);

	//keep owners underfined so that socket does not close
	//when the process exits -- other processes may accept
	//connections using listening socket

	return outlet_id(outlet);
}

//EOF
