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

#include "sha1.h"

/* a bit faster & bigger, if defined */
//#define UNROLL_LOOPS

/* NIST's proposed modification to SHA, 7/11/94 */
#define USE_MODIFIED_SHA

/* SHA f()-functions */
#define f1(x,y,z)	((x & y) | (~x & z))
#define f2(x,y,z)	(x ^ y ^ z)
#define f3(x,y,z)	((x & y) | (x & z) | (y & z))
#define f4(x,y,z)	(x ^ y ^ z)

/* SHA constants */
#define CONST1		0x5a827999L
#define CONST2		0x6ed9eba1L
#define CONST3		0x8f1bbcdcL
#define CONST4		0xca62c1d6L

/* 32-bit rotate */

#define ROT32(x,n)	((x << n) | (x >> (32 - n)))

#define FUNC(n,i)						\
    temp = ROT32(A,5) + f##n(B,C,D) + E + W[i] + CONST##n;	\
    E = D; D = C; C = ROT32(B,30); B = A; A = temp

#define SHA_BLOCKSIZE           64

/* do SHA transformation */
static void sha_transform(sha1_ctx_t *sha_info)
{
    int i;
    apr_uint32_t temp, A, B, C, D, E, W[80];

    for (i = 0; i < 16; ++i) {
	W[i] = sha_info->data[i];
    }
    for (i = 16; i < 80; ++i) {
	W[i] = W[i-3] ^ W[i-8] ^ W[i-14] ^ W[i-16];
#ifdef USE_MODIFIED_SHA
	W[i] = ROT32(W[i], 1);
#endif /* USE_MODIFIED_SHA */
    }
    A = sha_info->digest[0];
    B = sha_info->digest[1];
    C = sha_info->digest[2];
    D = sha_info->digest[3];
    E = sha_info->digest[4];
#ifdef UNROLL_LOOPS
    FUNC(1, 0);  FUNC(1, 1);  FUNC(1, 2);  FUNC(1, 3);  FUNC(1, 4);
    FUNC(1, 5);  FUNC(1, 6);  FUNC(1, 7);  FUNC(1, 8);  FUNC(1, 9);
    FUNC(1,10);  FUNC(1,11);  FUNC(1,12);  FUNC(1,13);  FUNC(1,14);
    FUNC(1,15);  FUNC(1,16);  FUNC(1,17);  FUNC(1,18);  FUNC(1,19);

    FUNC(2,20);  FUNC(2,21);  FUNC(2,22);  FUNC(2,23);  FUNC(2,24);
    FUNC(2,25);  FUNC(2,26);  FUNC(2,27);  FUNC(2,28);  FUNC(2,29);
    FUNC(2,30);  FUNC(2,31);  FUNC(2,32);  FUNC(2,33);  FUNC(2,34);
    FUNC(2,35);  FUNC(2,36);  FUNC(2,37);  FUNC(2,38);  FUNC(2,39);

    FUNC(3,40);  FUNC(3,41);  FUNC(3,42);  FUNC(3,43);  FUNC(3,44);
    FUNC(3,45);  FUNC(3,46);  FUNC(3,47);  FUNC(3,48);  FUNC(3,49);
    FUNC(3,50);  FUNC(3,51);  FUNC(3,52);  FUNC(3,53);  FUNC(3,54);
    FUNC(3,55);  FUNC(3,56);  FUNC(3,57);  FUNC(3,58);  FUNC(3,59);

    FUNC(4,60);  FUNC(4,61);  FUNC(4,62);  FUNC(4,63);  FUNC(4,64);
    FUNC(4,65);  FUNC(4,66);  FUNC(4,67);  FUNC(4,68);  FUNC(4,69);
    FUNC(4,70);  FUNC(4,71);  FUNC(4,72);  FUNC(4,73);  FUNC(4,74);
    FUNC(4,75);  FUNC(4,76);  FUNC(4,77);  FUNC(4,78);  FUNC(4,79);
#else /* !UNROLL_LOOPS */
    for (i = 0; i < 20; ++i) {
	FUNC(1,i);
    }
    for (i = 20; i < 40; ++i) {
	FUNC(2,i);
    }
    for (i = 40; i < 60; ++i) {
	FUNC(3,i);
    }
    for (i = 60; i < 80; ++i) {
	FUNC(4,i);
    }
#endif /* !UNROLL_LOOPS */
    sha_info->digest[0] += A;
    sha_info->digest[1] += B;
    sha_info->digest[2] += C;
    sha_info->digest[3] += D;
    sha_info->digest[4] += E;
}

union endianTest {
    long Long;
    char Char[sizeof(long)];
};

static char isLittleEndian(void)
{
    static union endianTest u;
    u.Long = 1;
    return (u.Char[0] == 1);
}

/* change endianness of data */

/* count is the number of bytes to do an endian flip */
static void maybe_byte_reverse(apr_uint32_t *buffer, int count)
{
    int i;
    apr_byte_t ct[4], *cp;

    if (isLittleEndian()) {	/* do the swap only if it is little endian */
	count /= sizeof(apr_uint32_t);
	cp = (apr_byte_t *) buffer;
	for (i = 0; i < count; ++i) {
	    ct[0] = cp[0];
	    ct[1] = cp[1];
	    ct[2] = cp[2];
	    ct[3] = cp[3];
	    cp[0] = ct[3];
	    cp[1] = ct[2];
	    cp[2] = ct[1];
	    cp[3] = ct[0];
	    cp += sizeof(apr_uint32_t);
	}
    }
}

/* initialize the SHA digest */

void sha1_init(sha1_ctx_t *sha_info)
{
    sha_info->digest[0] = 0x67452301L;
    sha_info->digest[1] = 0xefcdab89L;
    sha_info->digest[2] = 0x98badcfeL;
    sha_info->digest[3] = 0x10325476L;
    sha_info->digest[4] = 0xc3d2e1f0L;
    sha_info->count_lo = 0L;
    sha_info->count_hi = 0L;
    sha_info->local = 0;
}

/* update the SHA digest */

void sha1_update(sha1_ctx_t *sha_info, const unsigned char *buffer, unsigned int count)
{
    unsigned int i;

    if ((sha_info->count_lo + ((apr_uint32_t) count << 3)) < sha_info->count_lo) {
	++sha_info->count_hi;
    }
    sha_info->count_lo += (apr_uint32_t) count << 3;
    sha_info->count_hi += (apr_uint32_t) count >> 29;
    if (sha_info->local) {
	i = SHA_BLOCKSIZE - sha_info->local;
	if (i > count) {
	    i = count;
	}
	memcpy(((apr_byte_t *) sha_info->data) + sha_info->local, buffer, i);
	count -= i;
	buffer += i;
	sha_info->local += i;
	if (sha_info->local == SHA_BLOCKSIZE) {
	    maybe_byte_reverse(sha_info->data, SHA_BLOCKSIZE);
	    sha_transform(sha_info);
	}
	else {
	    return;
	}
    }
    while (count >= SHA_BLOCKSIZE) {
	memcpy(sha_info->data, buffer, SHA_BLOCKSIZE);
	buffer += SHA_BLOCKSIZE;
	count -= SHA_BLOCKSIZE;
	maybe_byte_reverse(sha_info->data, SHA_BLOCKSIZE);
	sha_transform(sha_info);
    }
    memcpy(sha_info->data, buffer, count);
    sha_info->local = count;
}

/* finish computing the SHA digest */

void sha1_final(unsigned char digest[SHA1_DIGESTSIZE],
                             sha1_ctx_t *sha_info)
{
    int count, i, j;
    apr_uint32_t lo_bit_count, hi_bit_count, k;

    lo_bit_count = sha_info->count_lo;
    hi_bit_count = sha_info->count_hi;
    count = (int) ((lo_bit_count >> 3) & 0x3f);
    ((apr_byte_t *) sha_info->data)[count++] = 0x80;
    if (count > SHA_BLOCKSIZE - 8) {
	memset(((apr_byte_t *) sha_info->data) + count, 0, SHA_BLOCKSIZE - count);
	maybe_byte_reverse(sha_info->data, SHA_BLOCKSIZE);
	sha_transform(sha_info);
	memset((apr_byte_t *) sha_info->data, 0, SHA_BLOCKSIZE - 8);
    }
    else {
	memset(((apr_byte_t *) sha_info->data) + count, 0,
	       SHA_BLOCKSIZE - 8 - count);
    }
    maybe_byte_reverse(sha_info->data, SHA_BLOCKSIZE);
    sha_info->data[14] = hi_bit_count;
    sha_info->data[15] = lo_bit_count;
    sha_transform(sha_info);

    for (i = 0, j = 0; j < SHA1_DIGESTSIZE; i++) {
	k = sha_info->digest[i];
	digest[j++] = (unsigned char) ((k >> 24) & 0xff);
	digest[j++] = (unsigned char) ((k >> 16) & 0xff);
	digest[j++] = (unsigned char) ((k >> 8) & 0xff);
	digest[j++] = (unsigned char) (k & 0xff);
    }
}

void sha1(unsigned char digest[SHA1_DIGESTSIZE],
		  const unsigned char *input, unsigned int inputLen)
{
	sha1_ctx_t context;
	sha1_init(&context);
	sha1_update(&context, input, inputLen);
	sha1_final(digest, &context);
}

//EOF
