#ifndef MD5_H
#define MD5_H

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

#include "apr_general.h"

/** The MD5 digest size */
#define MD5_DIGESTSIZE 16

/** @see md5_ctx_t */
typedef struct md5_ctx_t md5_ctx_t;

/** MD5 context. */
struct md5_ctx_t {
    /** state (ABCD) */
    apr_uint32_t state[4];
    /** number of bits, modulo 2^64 (lsb first) */
    apr_uint32_t count[2];
    /** input buffer */
    unsigned char buffer[64];
};

void md5_init(md5_ctx_t *context);
void md5_update(md5_ctx_t *context, const void *vinput, apr_size_t inputLen);
void md5_final(unsigned char digest[MD5_DIGESTSIZE], md5_ctx_t *context);
void md5(unsigned char digest[MD5_DIGESTSIZE], const void *_input, apr_size_t inputLen);

#endif /* MD5_H */
