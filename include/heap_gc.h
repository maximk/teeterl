#ifndef HEAP_GC_H
#define HEAP_GC_H

#include <apr_general.h>

#include "heap.h"

apr_status_t heap_gc(heap_t *hp, term_t *roots[], int root_sizes[], int nroots);

#endif
