#ifndef EXTERM_H
#define EXTERM_H

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
#include <apr_tables.h>

#include "term.h"
#include "atom_cache.h"
#include "atom.h"

#define EXTERM_MAGIC	131

#define EXTERM_BYTE			97
#define EXTERM_INT			98
#define EXTERM_FLOAT		99
#define EXTERM_ATOM			100
#define EXTERM_ATOM_UPDATE	78
#define EXTERM_ATOM_CACHED	67
#define EXTERM_REF			101
#define EXTERM_PORT			102
#define EXTERM_PID			103
#define EXTERM_SMALL_TUPLE	104
#define EXTERM_TUPLE		105
#define EXTERM_NIL			106
#define EXTERM_STRING		107
#define EXTERM_LIST			108
#define EXTERM_BINARY		109
#define EXTERM_BIGNUM_SMALL	110
#define EXTERM_BIGNUM		111

apr_array_header_t *pack_term(term_t t, atom_cache_t *cache, atoms_t *atoms);
term_t unpack_term(apr_byte_t *data, int size, atom_cache_t *cache, atoms_t *atoms, xpool_t *xp);

void exterm_test(void);

#endif
