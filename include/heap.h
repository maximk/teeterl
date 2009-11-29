#ifndef HEAP_H
#define HEAP_H

#include <apr_general.h>

#include "term.h"

typedef struct heap_t heap_t;
struct heap_t
{
	apr_allocator_t *allocator;
	apr_memnode_t *active;

	// char *htop;
	// htop must be first_avail; otherwise gc won't work

	char *hend;

	int alloc_size;

	apr_memnode_t *gc_last;
	apr_memnode_t *gc_spot;
};

#define heap_htop(hp)	((hp)->active->first_avail)

heap_t *heap_make(apr_pool_t *pool);
apr_byte_t *heap_alloc(heap_t *hp, int size);
int heap_chunk_count(heap_t *hp);
int heap_size(heap_t *hp);

void heap_anticipate_need(heap_t *hp, int size);
void heap_reclaim_unused(heap_t *hp, int size);
int heap_need_anticipated(heap_t *hp);	// mostly, for gc

// Oddball term allocations; in bifs?
term_t heap_float(heap_t *hp, double value);
term_t heap_bignum0(heap_t *hp, mp_sign sign, mp_size size);
term_t heap_bignum(heap_t *hp, mp_sign sign, mp_size size, mp_digit *digits);
term_t heap_binary0(heap_t *hp, int bit_size);
term_t heap_binary(heap_t *hp, int bit_size, apr_byte_t *data);
term_t heap_binary_shared(heap_t *hp, int bit_size, apr_byte_t *data, term_t parent);
term_t heap_fun(heap_t *hp, term_t module, term_t function, int arity, uint mod_id, int index, term_t frozen);
term_t heap_long_pid(heap_t *hp, term_t node, int serial, int creation);
term_t heap_long_oid(heap_t *hp, term_t node, int serial, int creation);
term_t heap_long_id(heap_t *hp, term_t node, int serial, int tag_creat);
term_t heap_tuple(heap_t *hp, int size);
term_t heap_tuple1(heap_t *hp, term_t e1);
term_t heap_tuple2(heap_t *hp, term_t e1, term_t e2);
term_t heap_tuple3(heap_t *hp, term_t e1, term_t e2, term_t e3);
term_t heap_tuple4(heap_t *hp, term_t e1, term_t e2, term_t e3, term_t e4);
term_t heap_tuple5(heap_t *hp, term_t e1, term_t e2, term_t e3, term_t e4, term_t e5);
term_t heap_tuple6(heap_t *hp, term_t e1, term_t e2, term_t e3, term_t e4, term_t e5, term_t e6);
term_t heap_tuple7(heap_t *hp, term_t e1, term_t e2, term_t e3, term_t e4, term_t e5, term_t e6, term_t e7);
term_t heap_tuple8(heap_t *hp, term_t e1, term_t e2, term_t e3, term_t e4, term_t e5, term_t e6, term_t e7, term_t e8);
term_t heap_cons(heap_t *hp, term_t head);
term_t heap_cons2(heap_t *hp, term_t head, term_t tail);

// copy term to another heap
term_t heap_marshal(term_t t, heap_t *hp);

#endif
