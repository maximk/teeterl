#ifndef BITS_H
#define BITS_H

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

#include "xpool.h"

// segment type
#define ST_INTEGER	0
#define ST_FLOAT	1
#define ST_BINARY	2

// signedness
#define SN_SIGNED	0
#define	SN_UNSIGNED	1

// endianness
#define	EN_BIG		0
#define	EN_LITTLE	1
#define	EN_NATIVE	2

typedef struct bin_pad_t bin_pad_t;

bin_pad_t *bin_pad_make(apr_pool_t *pool);
apr_pool_t *bin_pad_pool_get(bin_pad_t *self);

void bin_pad_append(bin_pad_t *self, apr_byte_t *buf, int nbits);
void bin_pad_zeros(bin_pad_t *self, int total, int dsize);
int bin_pad_is_valid(bin_pad_t *self);
apr_uint32_t bin_pad_size(bin_pad_t *self);
apr_byte_t *bin_pad_data_dup(bin_pad_t *self, xpool_t *xp);
void bin_pad_data_copy(bin_pad_t *self, apr_byte_t *data);

void fetch_bits(apr_byte_t *src, int boff, apr_byte_t *dst, int nbits);

#endif
