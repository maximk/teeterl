#ifndef CODE_BASE_H
#define CODE_BASE_H

#include "term.h"
#include "proc.h"

#include <apr_hash.h>

typedef struct code_base_t code_base_t;

typedef struct builtin_t builtin_t;
struct builtin_t {
	struct bif_key_t {
		term_t module;
		term_t function;
		int arity;
	} key;
	bifN_t entry;
};

extern builtin_t builtins[];

typedef struct export_t export_t;
struct export_t {
	struct exp_key_t {
		term_t function;
		int arity;
	} key;
	codel_t *entry;
};

typedef struct fun_slot_t fun_slot_t;
struct fun_slot_t {
	uint uniq;
	codel_t *entry;
};

typedef struct source_ref_t source_ref_t;
struct source_ref_t {
	int file_index;
	int source_line;
	int off_starts;
	int off_ends;
};

typedef struct module_t module_t;
struct module_t {
	apr_pool_t *mod_pool;
	heap_t *literals;
	struct mod_key_t {
		term_t module;
		int is_old;
	} key;
	int code_size; // in codels
	codel_t *code;
	apr_hash_t *exports;
	int nfuns;
	fun_slot_t *funs;

	// source line info
	apr_array_header_t *files;		//const char *
	apr_array_header_t *source;		//source_ref_t
};

code_base_t *code_base_make(apr_pool_t *pool);
module_t *code_base_lookup(code_base_t *self, term_t module);
codel_t *module_lookup(module_t *m, term_t function, int arity);
bifN_t code_base_bif(code_base_t *self, term_t module, term_t function, int arity);
int code_base_load(code_base_t *self, named_tuples_t *nm_tuples,
	term_t module, term_t exports, term_t fun_table, term_t attrs, term_t preloaded, term_t misc);
int code_base_delete(code_base_t *self, term_t module);
//int code_base_purge(code_base_t *self, uint mod_id);

int module_source_from_offset(module_t *m, int offset, const char **file, int *line);
int module_offset_from_source(module_t *m, const char *file, int line, int *offset);

#endif
