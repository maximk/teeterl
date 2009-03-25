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

//aix
//beos
//dgux
//freebsd
//hpux
//irix
//linux
//macos
//netbsd
//netware
//openbsd
//os2
//qnx
//solaris
//sunos
//vms
//win32
//winnt
//unix

term_t bif_type0(process_t *ctx)
{
#if defined(_AIX)
	result(make_tuple2(A_UNIX, A_AIX, proc_gc_pool(ctx)));
#elif defined(__BEOS__)
	result(A_BEOS);
#elif defined(DGUX)
	result(make_tuple2(A_UNIX, A_DGUX, proc_gc_pool(ctx)));
#elif defined(__FreeBSD__)
	result(make_tuple2(A_UNIX, A_FREEBSD, proc_gc_pool(ctx)));
#elif defined(HPUX)
	result(make_tuple2(A_UNIX, A_HPUX, proc_gc_pool(ctx)));
#elif defined(sgi)
	result(make_tuple2(A_UNIX, A_IRIX, proc_gc_pool(ctx)));
#elif defined(linux)
	result(make_tuple2(A_UNIX, A_LINUX, proc_gc_pool(ctx)));
#elif defined(DARWIN)
	result(A_MACOS);
#elif defined(__NetBSD__)
	result(make_tuple2(A_UNIX, A_NETBSD, proc_gc_pool(ctx)));
#elif defined(__OpenBSD__)
	result(make_tuple2(A_UNIX, A_OPENBSD, proc_gc_pool(ctx)));
#elif defined(NETWARE)
	result(A_NETWARE);
#elif defined(OS2)
	result(A_OS2);
#elif defined(__QNX__)
	result(make_tuple2(A_UNIX, A_QNX, proc_gc_pool(ctx)));
#elif defined(__SVR4) || defined(__srv4__)
	result(make_tuple2(A_UNIX, A_SOLARIS, proc_gc_pool(ctx)));
#elif defined(sun)
	result(make_tuple2(A_UNIX, A_SUNOS, proc_gc_pool(ctx)));
#elif defined(VMS)
	result(A_VMS);
#elif defined(_WIN32) || defined(WIN32)
	result(make_tuple2(A_WIN32, A_WINNT, proc_gc_pool(ctx)));
#else
	result(A_UNIX);
#endif
	return AI_OK;
}

//EOF
