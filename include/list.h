#ifndef LIST_H
#define LIST_H

#include "cstr.h"
#include "term.h"
#include "heap.h"

int list_length(term_t list);

#define cons_up(__first, __last, __v, __heap) \
	do { \
		term_t __cons = heap_cons(__heap, __v); \
		if (__last != nil) \
			peel(__last)->cons.tail = __cons; \
		__last = __cons; \
		if (__first == nil) \
			__first = __cons; \
	} while (0)

const char *ltoz(term_t l, apr_pool_t *pool);
term_t ztol(const char *z, heap_t *hp);
term_t stol(cstr_t *s, heap_t *hp);

#endif
