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

#include "bits.h"

#include <apr_tables.h>
#include <apr_strings.h>

#include "errors.h"

struct bin_pad_t {
	apr_array_header_t *data;
	int slack; //in bits
};

bin_pad_t *bin_pad_make(apr_pool_t *pool)
{
	bin_pad_t *pad = (bin_pad_t *)apr_palloc(pool, sizeof(*pad));
	pad->data = apr_array_make(pool, 16, 1);
	pad->slack = 0;
	return pad;
}

apr_pool_t *bin_pad_pool_get(bin_pad_t *self)
{
	return self->data->pool;
}

void bin_pad_append(bin_pad_t *self, apr_byte_t *buf, int nbits)
{
	apr_byte_t *src = buf;
	apr_byte_t *dst;
	while (nbits > 0)
	{
		int nb = (nbits <= 8)
			? nbits
			: 8;

		if (self->slack == 0)
		{
			//last byte is complete -- extend data
			dst = (apr_byte_t *)apr_array_push(self->data);
			*dst = *src;
		}
		else if (self->slack + nbits <= 8)
		{
			//data fits into the incomplete byte
			apr_byte_t mask = ((1 << nbits) - 1) << (8 - nbits); //nbits of 1's
			dst = (apr_byte_t *)self->data->elts + self->data->nelts - 1;
			mask >>= self->slack;
			*dst &= ~mask;
			*dst |= (*src >> self->slack);
		}
		else
		{
			apr_uint16_t mask, h, l, hl;

			//two bytes are needed to save 8 bits from src
			apr_array_push(self->data); //add another byte
			dst = (apr_byte_t *)self->data->elts + self->data->nelts - 2; //incomplete byte

			h = dst[0];
			l = dst[1];
			hl = (h << 8) | l;

			mask = ((1 << nb) - 1) << (16 - nb);
			mask >>= self->slack;
			hl &= ~mask;
			hl |= (*src << (8 - self->slack));

			dst[0] = hl >> 8;
			dst[1] = hl & 0xff;
		}

		self->slack += nb;
		self->slack %= 8;
		nbits -= nb;

		src++;
	}
}

void bin_pad_zeros(bin_pad_t *self, int total, int dsize)
{
	//append zeros until dsize bits is left out of total
	apr_byte_t buf[8] = {0, 0, 0, 0, 0, 0, 0, 0};
	while (total > dsize)
	{
		int pad_size = total - dsize;
		if (pad_size > sizeof(buf) * 8)
			pad_size = sizeof(buf) * 8;
		bin_pad_append(self, buf, pad_size);
		total -= pad_size;
	}
}

int bin_pad_is_valid(bin_pad_t *self)
{
	return (self->slack == 0);
}

apr_uint32_t bin_pad_size(bin_pad_t *self)
{
	return self->data->nelts;
}

apr_byte_t *bin_pad_data_dup(bin_pad_t *self, xpool_t *xp)
{
	apr_byte_t *data = xalloc(xp, self->data->nelts);
	memcpy(data, self->data->elts, self->data->nelts);
	return data;
}

void bin_pad_data_copy(bin_pad_t *self, apr_byte_t *data)
{
	memcpy(data, self->data->elts, self->data->nelts);
}

void fetch_bits(apr_byte_t *src, int boff, apr_byte_t *dst, int nbits)
{
	int slack = boff % 8;
	src += boff / 8; //points to the first byte which contains some required bits

	while (nbits > 0)
	{
		int nb = (nbits <= 8)
			? nb = nbits
			: 8;

		if (slack == 0)
		{
			*dst = *src;
		}
		else if (slack + nbits <= 8)
		{
			apr_byte_t mask = ((1 << nbits) - 1) << (8 - nbits);
			*dst = (*src << slack) & mask;
		}
		else
		{
			apr_uint16_t mask = ((1 << nb) - 1) << (16 - nb);
			apr_uint16_t h, l, hl;

			h = src[0];
			l = src[1];
			hl = (h << 8) | l;

			hl <<= slack;
			hl &= mask;
			dst[0] = (hl >> 8);
		}

		slack += nb;
		slack %= 8;
		nbits -= nb;

		src++; dst++;
	}
}

//void bit_tests()
//{
//	apr_pool_t *pool;
//	bin_pad_t *pad;
//	apr_byte_t buf[8];
//	apr_byte_t *data;
//
//	apr_pool_create(&pool, 0);
//	pad = bin_pad_make(pool);
//
//	//bin_pad_append(bin_pad_t *self, apr_byte_t *buf, int nbits)
//
//	buf[0] = 6 << 3;
//	bin_pad_append(pad, buf, 5);
//
//	buf[0] = 17;
//	buf[1] = 33;
//	buf[2] = 0;
//	bin_pad_append(pad, buf, 17);
//
//	buf[0] = 89;
//	buf[1] = 133;
//	buf[2] = 41;
//	buf[3] = 53;
//	bin_pad_append(pad, buf, 32);
//
//	buf[0] = 7;
//	buf[1] = 3 << 6;
//	bin_pad_append(pad, buf, 10);
//
//	data = bin_pad_data_dup(pad, pool);
//	
//	fetch_bits(data, 0, buf, 5);
//	assert(buf[0] >> 3 == 6);
//
//	fetch_bits(data, 5, buf, 17);
//	assert(buf[0] == 17);
//	assert(buf[1] == 33);
//	assert(buf[2] >> 7 == 0);
//
//	fetch_bits(data, 22, buf, 32);
//	assert(buf[0] == 89);
//	assert(buf[1] == 133);
//	assert(buf[2] == 41);
//	assert(buf[3] == 53);
//
//	fetch_bits(data, 54, buf, 10);
//	assert(buf[0] == 7);
//	assert(buf[1] >> 6 == 3);
//
//	apr_pool_destroy(pool);
//}

//EOF
