#ifndef SHA1_H
#define SHA1_H

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

/** size of the SHA1 DIGEST */
#define SHA1_DIGESTSIZE 20

/** @see apr_sha1_ctx_t */
typedef struct sha1_ctx_t sha1_ctx_t;

/** 
 * SHA1 context structure
 */
struct sha1_ctx_t {
    /** message digest */
    apr_uint32_t digest[5];
    /** 64-bit bit counts */
    apr_uint32_t count_lo, count_hi;
    /** SHA data buffer */
    apr_uint32_t data[16];
    /** unprocessed amount in data */
    int local;
};

void sha1_init(sha1_ctx_t *context);
void sha1_update(sha1_ctx_t *context, const unsigned char *input, unsigned int inputLen);
void sha1_final(unsigned char digest[SHA1_DIGESTSIZE], sha1_ctx_t *context);
void sha1(unsigned char digest[SHA1_DIGESTSIZE], const unsigned char *input, unsigned int inputLen);
#endif	/* SHA1_H */
