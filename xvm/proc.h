#ifndef _PROC_H
#define _PROC_H

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

#include "term.h"

//#ifndef CODE_BASE_H
typedef struct code_base_t code_base_t;
//#endif

#define INFINITY		(-1)
#define TIMEOUT_ZERO	(0)

#define INIT_DSTACK_SIZE 32
#define INIT_CSTACK_SIZE 64

typedef struct process_t process_t;

extern term_t my_node;

term_t proc_main(process_t *proc, int reductions, term_t *retval);
term_t proc_get_info(process_t *proc, term_t what);
term_t proc_set_flag(process_t *proc, term_t what, term_t value);
xpool_t *proc_gc_pool(process_t *self);
code_base_t *proc_code_base(process_t *self);
atoms_t *proc_atoms(process_t *self);
apr_uint32_t proc_serial(process_t *self);
void proc_bif_result(process_t *self, term_t Result);
process_t *proc_lookup(apr_uint32_t serial);
void proc_destroy(process_t *proc);
process_t *proc_spawn(code_base_t *base,
	atoms_t *atoms,	term_t amod, term_t afun, term_t args);
int proc_register(process_t *self, term_t name);
process_t *proc_registered(term_t name);
int proc_unregister(term_t name);
term_t proc_list_registered(xpool_t *xp);
void proc_new_mail(process_t *self, term_t msg);
term_t proc_pid(process_t *self, xpool_t *xp);
void proc_run_gc(process_t *self);
term_t proc_trace_stack(process_t *self);
term_t proc_get_arguments(process_t *self, int n);
term_t proc_get_stacktrace(process_t *self);
term_t proc_get_locals(process_t *self);
int proc_is_lingering(apr_uint32_t mod_index);
term_t proc_dict(process_t *self);
void proc_set_dict(process_t *self, term_t d);

void proc_print_error(process_t *self, term_t e);
void proc_print_init_exits_unexpectedly(process_t *self, term_t result, term_t retval);

#endif
