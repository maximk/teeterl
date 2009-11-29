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

#include "md5.h"
#include "sha1.h"

#include "errors.h"

term_t bif_md5_init0(process_t *ctx)
{
	term_t md5;

	// md5_ctx_t is safe for Erlang gc etc
	md5_ctx_t *data = xalloc(proc_gc_pool(ctx), sizeof(md5_ctx_t));
	md5_init(data);

	md5 = make_binary(intnum(sizeof(md5_ctx_t)),
			(apr_byte_t *)data, proc_gc_pool(ctx));
	result(md5);
	return AI_OK;
}

term_t bif_md5_update2(term_t Data, term_t Context, process_t *ctx)
{
	apr_size_t size;
	md5_ctx_t *tmp;
	if (!is_binary(Data) || !is_binary(Context))
		return A_BADARG;
	if (int_value2(bin_size(Context)) != sizeof(md5_ctx_t))
		return A_BADARG;
	size = (apr_size_t)int_value(bin_size(Data));
	
	tmp = xalloc(proc_gc_pool(ctx), sizeof(*tmp));
	memcpy(tmp, bin_data(Context), sizeof(*tmp));
	
	md5_update(tmp, bin_data(Data), size);
	result(make_binary(intnum(sizeof(*tmp)),
		(apr_byte_t *)tmp, proc_gc_pool(ctx)));
	return AI_OK;
}

term_t bif_md5_final1(term_t Context, process_t *ctx)
{
	apr_byte_t *data;
	if (!is_binary(Context) ||
			int_value2(bin_size(Context)) != sizeof(md5_ctx_t))
		return A_BADARG;
	data = xalloc(proc_gc_pool(ctx), 16);
	md5_final(data, (md5_ctx_t *)bin_data(Context));
	result(make_binary(intnum(MD5_DIGESTSIZE), data, proc_gc_pool(ctx)));
	return AI_OK;
}

term_t bif_md5_1(term_t Data, process_t *ctx)
{
	apr_byte_t *digest = xalloc(proc_gc_pool(ctx), MD5_DIGESTSIZE);
	if (!is_binary(Data))
		return A_BADARG;
	md5(digest, bin_data(Data), (apr_size_t)int_value(bin_size(Data)));
	result(make_binary(intnum(MD5_DIGESTSIZE), digest, proc_gc_pool(ctx)));
	return AI_OK;
}

term_t bif_sha1_init0(process_t *ctx)
{
	term_t sha1;

	// sha1_ctx_t is safe for Erlang gc etc
	sha1_ctx_t *data = xalloc(proc_gc_pool(ctx), sizeof(sha1_ctx_t));
	sha1_init(data);

	sha1 = make_binary(intnum(sizeof(sha1_ctx_t)),
			(apr_byte_t *)data, proc_gc_pool(ctx));
	result(sha1);
	return AI_OK;
}

term_t bif_sha1_update2(term_t Data, term_t Context, process_t *ctx)
{
	apr_size_t size;
	sha1_ctx_t *tmp;
	if (!is_binary(Data) || !is_binary(Context))
		return A_BADARG;
	if (int_value2(bin_size(Context)) != sizeof(sha1_ctx_t))
		return A_BADARG;
	size = (apr_size_t)int_value(bin_size(Data));
	
	tmp = xalloc(proc_gc_pool(ctx), sizeof(*tmp));
	memcpy(tmp, bin_data(Context), sizeof(*tmp));
	
	sha1_update(tmp, bin_data(Data), size);
	result(make_binary(intnum(sizeof(*tmp)),
		(apr_byte_t *)tmp, proc_gc_pool(ctx)));
	return AI_OK;
}

term_t bif_sha1_final1(term_t Context, process_t *ctx)
{
	apr_byte_t *data;
	if (!is_binary(Context) ||
			int_value2(bin_size(Context)) != sizeof(sha1_ctx_t))
		return A_BADARG;
	data = xalloc(proc_gc_pool(ctx), 16);
	sha1_final(data, (sha1_ctx_t *)bin_data(Context));
	result(make_binary(intnum(SHA1_DIGESTSIZE), data, proc_gc_pool(ctx)));
	return AI_OK;
}

term_t bif_sha1_1(term_t Data, process_t *ctx)
{
	apr_byte_t *digest = xalloc(proc_gc_pool(ctx), SHA1_DIGESTSIZE);
	if (!is_binary(Data))
		return A_BADARG;
	sha1(digest, bin_data(Data), (apr_size_t)int_value(bin_size(Data)));
	result(make_binary(intnum(SHA1_DIGESTSIZE), digest, proc_gc_pool(ctx)));
	return AI_OK;
}

term_t bif_rc4_init1(term_t Key, process_t *ctx)
{
	apr_byte_t *s;
	apr_byte_t i, j;
	apr_byte_t key_len;
	apr_byte_t *key_data;

	if (!is_binary(Key))
		return A_BADARG;
	s = xalloc(proc_gc_pool(ctx), 256+2);	//2 for i and j
	key_len = (apr_byte_t)int_value(bin_size(Key));
	key_data = bin_data(Key);

	i = 0;
	do {
		s[i] = i++;
	} while (i != 0);
	
	i = j = 0;
	do {
		apr_byte_t temp;
		j += key_data[i%key_len]+s[i];
		temp = s[i];
		s[i] = s[j];
		s[j] = temp;
		i++;
	} while (i != 0);

	s[256] = 0;
	s[257] = 0;

	result(make_binary(intnum(256+2), s, proc_gc_pool(ctx)));
	return AI_OK;
}

term_t bif_rc4_update2(term_t Text, term_t Opaque, process_t *ctx)
{
	apr_byte_t *text_data;
	apr_uint32_t text_size, k;
	apr_byte_t *s;
	apr_byte_t i, j;
	term_t Text1, Opaque1;

	if (!is_binary(Text) || !is_binary(Opaque) || bin_size(Opaque) != intnum(256+2))
		return A_BADARG;

	text_size = int_value2(bin_size(Text));
	text_data = xalloc(proc_gc_pool(ctx), text_size);
	memcpy(text_data, bin_data(Text), text_size);
	
	s = xalloc(proc_gc_pool(ctx), 256+2);
	memcpy(s, bin_data(Opaque), 256+2);

	i = s[256];
	j = s[257];

	for (k = 0; k < text_size; k++)
	{
		apr_byte_t temp;
		i++;
		j += s[i];
		temp = s[i];
		s[i] = s[j];
		s[j] = temp;

		text_data[k] ^= s[(s[i]+s[j]) & 255];
	}

	s[256] = i;
	s[257] = j;

	Text1 = make_binary(intnum(text_size), text_data, proc_gc_pool(ctx));
	Opaque1 = make_binary(intnum(256+2), s, proc_gc_pool(ctx));

	result(make_tuple2(Text1, Opaque1, proc_gc_pool(ctx)));
	return AI_OK;
}

//EOF
