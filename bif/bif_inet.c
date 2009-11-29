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

#include "bifimpl.h"

#include <apr_network_io.h>

#include "outlet_mall.h"
#include "getput.h"
#include "list.h"

// inet:getaddrs0/2 [4]
term_t bif_getaddrs0_2(term_t Addr, term_t Family, proc_t *proc)
{
	apr_status_t rs;
	apr_pool_t *tmp;
	apr_sockaddr_t *sa;
	const char *host;
	term_t addrs = nil;

	if (!is_binary(Addr) || !is_atom(Family))
		bif_bad_arg0();

	if (Family != A_INET)
		bif_exception(A_NOT_SUPPORTED);

	host = (const char *)peel(Addr)->binary.data;	//null-terminated by caller

	apr_pool_create(&tmp, 0);
	rs = apr_sockaddr_info_get(&sa, host, APR_INET, 0, 0, tmp);
	if (rs == 0)
	{
		term_t first = nil;
		term_t last = nil;

		while (sa)
		{
			struct in_addr ia = *(struct in_addr *)sa->ipaddr_ptr;
			apr_byte_t qs[4];
			term_t ip;

#if APR_IS_BIGENDIAN
			PUT32(qs, ia.s_addr);
#else
			PUT32_LE(qs, ia.s_addr);
#endif

			ip = heap_tuple4(proc->heap,
				tag_int(qs[0]),
				tag_int(qs[1]),
				tag_int(qs[2]),
				tag_int(qs[3]));

			cons_up(first, last, ip, proc->heap);

			sa = sa->next;
		}

		addrs = first;
	}

	apr_pool_destroy(tmp);

	if (rs != 0)
		bif_exception(decipher_status(rs));

	return addrs;
}

//EOF
