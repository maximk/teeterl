#ifndef GETPUT_H
#define GETPUT_H

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

#define PUT16(buf, n) \
	do { \
		(buf)[0] = (apr_byte_t)((n) >> 8); \
		(buf)[1] = (apr_byte_t)((n)); \
	} while (0)

#define PUT16_LE(buf, n) \
	do { \
		(buf)[1] = (apr_byte_t)((n) >> 8); \
		(buf)[0] = (apr_byte_t)((n)); \
	} while (0)

#define PUT32(buf, n) \
	do { \
		(buf)[0] = (apr_byte_t)((n) >> 24); \
		(buf)[1] = (apr_byte_t)((n) >> 16); \
		(buf)[2] = (apr_byte_t)((n) >> 8); \
		(buf)[3] = (apr_byte_t)((n)); \
	} while (0)

#define PUT32_LE(buf, n) \
	do { \
		(buf)[3] = (apr_byte_t)((n) >> 24); \
		(buf)[2] = (apr_byte_t)((n) >> 16); \
		(buf)[1] = (apr_byte_t)((n) >> 8); \
		(buf)[0] = (apr_byte_t)((n)); \
	} while (0)

#define PUT64(buf, n) \
	do { \
		(buf)[0] = (apr_byte_t)((n) >> 56); \
		(buf)[1] = (apr_byte_t)((n) >> 48); \
		(buf)[2] = (apr_byte_t)((n) >> 40); \
		(buf)[3] = (apr_byte_t)((n) >> 32); \
		(buf)[4] = (apr_byte_t)((n) >> 24); \
		(buf)[5] = (apr_byte_t)((n) >> 16); \
		(buf)[6] = (apr_byte_t)((n) >> 8); \
		(buf)[7] = (apr_byte_t)((n)); \
	} while (0)

#define PUT64_LE(buf, n) \
	do { \
		(buf)[7] = (apr_byte_t)((n) >> 56); \
		(buf)[6] = (apr_byte_t)((n) >> 48); \
		(buf)[5] = (apr_byte_t)((n) >> 40); \
		(buf)[4] = (apr_byte_t)((n) >> 32); \
		(buf)[3] = (apr_byte_t)((n) >> 24); \
		(buf)[2] = (apr_byte_t)((n) >> 16); \
		(buf)[1] = (apr_byte_t)((n) >> 8); \
		(buf)[0] = (apr_byte_t)((n)); \
	} while (0)

#define GET16(buf) \
	(((buf)[0] << 8) | \
	((buf)[1]))

#define GET16_LE(buf) \
	(((buf)[1] << 8) | \
	((buf)[0]))

#define GET32(buf) \
	(((buf)[0] << 24) | \
	((buf)[1] << 16) | \
	((buf)[2] << 8) | \
	((buf)[3]))

#define GET32_LE(buf) \
	(((buf)[3] << 24) | \
	((buf)[2] << 16) | \
	((buf)[1] << 8) | \
	((buf)[0]))

#define GET64(buf) \
	(((apr_uint64_t)(buf)[0] << 56) | \
	((apr_uint64_t)(buf)[1] << 48) | \
	((apr_uint64_t)(buf)[2] << 40) | \
	((apr_uint64_t)(buf)[3] << 32) | \
	((apr_uint64_t)(buf)[4] << 24) | \
	((apr_uint64_t)(buf)[5] << 16) | \
	((apr_uint64_t)(buf)[6] << 8) | \
	((apr_uint64_t)(buf)[7]))

#define GET64_LE(buf) \
	(((apr_uint64_t)(buf)[7] << 56) | \
	((apr_uint64_t)(buf)[6] << 48) | \
	((apr_uint64_t)(buf)[5] << 40) | \
	((apr_uint64_t)(buf)[4] << 32) | \
	((apr_uint64_t)(buf)[3] << 24) | \
	((apr_uint64_t)(buf)[2] << 16) | \
	((apr_uint64_t)(buf)[1] << 8) | \
	((apr_uint64_t)(buf)[0]))

#if APR_IS_BIGENDIAN
#define PUT16_NAT(buf, n) PUT16(buf, n)
#define PUT32_NAT(buf, n) PUT32(buf, n)
#define PUT64_NAT(buf, n) PUT64(buf, n)
#define GET16_NAT(buf) GET16(buf)
#define GET32_NAT(buf) GET32(buf)
#define GET64_NAT(buf) GET64(buf)
#else
#define PUT16_NAT(buf, n) PUT16_LE(buf, n)
#define PUT32_NAT(buf, n) PUT32_LE(buf, n)
#define PUT64_NAT(buf, n) PUT64_LE(buf, n)
#define GET16_NAT(buf) GET16_LE(buf)
#define GET32_NAT(buf) GET32_LE(buf)
#define GET64_NAT(buf) GET64_LE(buf)
#endif

#endif /*!GETPUT_H*/
