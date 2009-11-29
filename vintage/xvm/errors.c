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

#include <apr_general.h>

#define APR_WANT_STDIO
#include <apr_want.h>

#include "atom.h"
#include "errors.h"

void exit(int status);

void fatal_err_(const char *file, int line, const char *msg)
{
	fprintf(stderr, "%s:%d:fatal error: %s\n", file, line, msg);
	exit(1);
}

void assert_(const char *file, int line, int e)
{
	if (!e)
	{
		fprintf(stderr, "%s:%d: assertion failed\n", file, line);
		exit(1);
	}
}

//convert APR status into an atom
term_t decipher_status(apr_status_t rs)
{
	if (APR_STATUS_IS_ANONYMOUS(rs))
		return A_ANONYMOUS;
	if (APR_STATUS_IS_BADARG(rs))
		return A_BADARG;
	if (APR_STATUS_IS_BADCH(rs))
		return A_BADCH;
	if (APR_STATUS_IS_CHILD_DONE(rs))
		return A_CHILD_DONE;
	if (APR_STATUS_IS_CHILD_NOTDONE(rs))
		return A_CHILD_NOTDONE;
	if (APR_STATUS_IS_DETACH(rs))
		return A_DETACH;
	if (APR_STATUS_IS_EABOVEROOT(rs))
		return A_EABOVEROOT;
	if (APR_STATUS_IS_EABSOLUTE(rs))
		return A_EABSOLUTE;
	if (APR_STATUS_IS_EACCES(rs))
		return A_EACCES;
	if (APR_STATUS_IS_EAGAIN(rs))
		return A_EAGAIN;
	if (APR_STATUS_IS_EBADDATE(rs))
		return A_EBADDATE;
	if (APR_STATUS_IS_EBADF(rs))
		return A_EBADF;
	if (APR_STATUS_IS_EBADIP(rs))
		return A_EBADIP;
	if (APR_STATUS_IS_EBADMASK(rs))
		return A_EBADMASK;
	if (APR_STATUS_IS_EBADPATH(rs))
		return A_EBADPATH;
	if (APR_STATUS_IS_EBUSY(rs))
		return A_EBUSY;
	if (APR_STATUS_IS_ECONNABORTED(rs))
		return A_ECONNABORTED;
	if (APR_STATUS_IS_ECONNREFUSED(rs))
		return A_ECONNREFUSED;
	if (APR_STATUS_IS_ECONNRESET(rs))
		return A_ECONNRESET;
	if (APR_STATUS_IS_EDSOOPEN(rs))
		return A_EDSOOPEN;
	if (APR_STATUS_IS_EEXIST(rs))
		return A_EEXIST;
	if (APR_STATUS_IS_EFTYPE(rs))
		return A_EFTYPE;
	if (APR_STATUS_IS_EGENERAL(rs))
		return A_EGENERAL;
	if (APR_STATUS_IS_EHOSTUNREACH(rs))
		return A_EHOSTUNREACH;
	if (APR_STATUS_IS_EINCOMPLETE(rs))
		return A_EINCOMPLETE;
	if (APR_STATUS_IS_EINIT(rs))
		return A_EINIT;
	if (APR_STATUS_IS_EINPROGRESS(rs))
		return A_EINPROGRESS;
	if (APR_STATUS_IS_EINTR(rs))
		return A_EINTR;
	if (APR_STATUS_IS_EINVAL(rs))
		return A_EINVAL;
	if (APR_STATUS_IS_EINVALSOCK(rs))
		return A_EINVALSOCK;
	if (APR_STATUS_IS_EMFILE(rs))
		return A_EMFILE;
	if (APR_STATUS_IS_EMISMATCH(rs))
		return A_EMISMATCH;
	if (APR_STATUS_IS_ENAMETOOLONG(rs))
		return A_ENAMETOOLONG;
	if (APR_STATUS_IS_ENETUNREACH(rs))
		return A_ENETUNREACH;
	if (APR_STATUS_IS_ENFILE(rs))
		return A_ENFILE;
	if (APR_STATUS_IS_ENODIR(rs))
		return A_ENODIR;
	if (APR_STATUS_IS_ENOENT(rs))
		return A_ENOENT;
	if (APR_STATUS_IS_ENOLOCK(rs))
		return A_ENOLOCK;
	if (APR_STATUS_IS_ENOMEM(rs))
		return A_ENOMEM;
	if (APR_STATUS_IS_ENOPOLL(rs))
		return A_ENOPOLL;
	if (APR_STATUS_IS_ENOPROC(rs))
		return A_ENOPROC;
	if (APR_STATUS_IS_ENOSHMAVAIL(rs))
		return A_ENOSHMAVAIL;
	if (APR_STATUS_IS_ENOSOCKET(rs))
		return A_ENOSOCKET;
	if (APR_STATUS_IS_ENOSPC(rs))
		return A_ENOSPC;
	if (APR_STATUS_IS_ENOSTAT(rs))
		return A_ENOSTAT;
	if (APR_STATUS_IS_ENOTDIR(rs))
		return A_ENOTDIR;
	if (APR_STATUS_IS_ENOTEMPTY(rs))
		return A_ENOTEMPTY;
	if (APR_STATUS_IS_ENOTENOUGHENTROPY(rs))
		return A_ENOTENOUGHENTROPY;
	if (APR_STATUS_IS_ENOTHDKEY(rs))
		return A_ENOTHDKEY;
	if (APR_STATUS_IS_ENOTHREAD(rs))
		return A_ENOTHREAD;
	if (APR_STATUS_IS_ENOTIME(rs))
		return A_ENOTIME;
	if (APR_STATUS_IS_ENOTIMPL(rs))
		return A_ENOTIMPL;
	if (APR_STATUS_IS_ENOTSOCK(rs))
		return A_ENOTSOCK;
	if (APR_STATUS_IS_EOF(rs))
		return A_EOF;
	if (APR_STATUS_IS_EPATHWILD(rs))
		return A_EPATHWILD;
	if (APR_STATUS_IS_EPIPE(rs))
		return A_EPIPE;
	if (APR_STATUS_IS_EPROC_UNKNOWN(rs))
		return A_EPROC_UNKNOWN;
	if (APR_STATUS_IS_ERELATIVE(rs))
		return A_ERELATIVE;
	if (APR_STATUS_IS_ESPIPE(rs))
		return A_ESPIPE;
	if (APR_STATUS_IS_ESYMNOTFOUND(rs))
		return A_ESYMNOTFOUND;
	if (APR_STATUS_IS_ETIMEDOUT(rs))
		return A_ETIMEDOUT;
	if (APR_STATUS_IS_EXDEV(rs))
		return A_EXDEV;
	if (APR_STATUS_IS_FILEBASED(rs))
		return A_FILEBASED;
	if (APR_STATUS_IS_INCHILD(rs))
		return A_INCHILD;
	if (APR_STATUS_IS_INCOMPLETE(rs))
		return A_INCOMPLETE;
	if (APR_STATUS_IS_INPARENT(rs))
		return A_INPARENT;
	if (APR_STATUS_IS_KEYBASED(rs))
		return A_KEYBASED;
	if (APR_STATUS_IS_NOTDETACH(rs))
		return A_NOTDETACH;
	if (APR_STATUS_IS_NOTFOUND(rs))
		return A_NOTFOUND;
	if (APR_STATUS_IS_TIMEUP(rs))
		return A_TIMEUP;
	return A_UNKNOWN;
}

//EOF
