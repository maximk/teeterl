#ifndef CODE_BASE_H
#define CODE_BASE_H

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

#include "bif.h"
#include "term.h"

#define MOD_INDEX_NONE		0
#define MOD_INDEX_STARTS	1

#ifndef _PROC_H
typedef struct code_base_t code_base_t;
#endif

code_base_t *code_base_make(apr_pool_t *pool);
celem_t *code_base_lookup(code_base_t *self,
	term_t amod, term_t afun, apr_byte_t arity, apr_uint32_t *new_mod_index, celem_t **new_code);
bifN_t code_base_bif(code_base_t *self, term_t mod, term_t fun, apr_byte_t arity);
celem_t *code_base_starts2(code_base_t *self, term_t mod);
celem_t *code_base_starts(code_base_t *self, apr_uint32_t index);
int code_base_load(code_base_t *self, term_t amod, term_t exports, term_t preloaded);
int code_base_load2(code_base_t *self, term_t code);

term_t code_base_mod_name(code_base_t *self, apr_uint32_t mod_index);
apr_uint32_t code_base_mod_index(code_base_t *self, term_t amod, int is_old);

term_t code_base_list_modules(code_base_t *self, xpool_t *xp);
int code_base_delete(code_base_t *self, term_t amod);
int code_base_purge(code_base_t *self, apr_uint32_t mod_index);

int code_base_breakpoint_set(code_base_t *self, apr_uint32_t mod_index, apr_uint32_t offset);
int code_base_breakpoint_unset(code_base_t *self, apr_uint32_t mod_index, apr_uint32_t offset);
int code_base_breakpoint_toggle(code_base_t *self, apr_uint32_t mod_index, apr_uint32_t offset);
int code_base_clear_breakpoints(code_base_t *self);
apr_byte_t code_base_breakpoint_command(code_base_t *self, apr_uint32_t mod_index, apr_uint32_t offset);

#endif
