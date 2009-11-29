#ifndef NAVEL_H
#define NAVEL_H

#include <apr_general.h>
#include <apr_tables.h>

#include "atom.h"
#include "proc.h"
#include "code_base.h"
#include "scheduler.h"

const char *term2html(term_t t, atoms_t *atoms, apr_pool_t *pool);

typedef struct navel_context_t navel_context_t;
struct navel_context_t {
	apr_pool_t *pool; // survives between requests
	heap_t *heap; // houses sample terms and all
	atoms_t *atoms; // for smoother comparison etc
	code_base_t *code_base;
	apr_array_header_t *sample_terms;
	teevm_t *teevm;
	proc_t *solitaire;
	module_t *module;
};

#endif
