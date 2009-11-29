#ifndef MIXED_H
#define MIXED_H

#include "term.h"
#include "heap.h"

term_t add_mixed(term_t a, term_t b, heap_t *heap);
term_t mult_mixed(term_t a, term_t b, heap_t *heap);
term_t sub_mixed(term_t a, term_t b, heap_t *heap);
term_t mod_mixed(term_t a, term_t b, heap_t *heap);
term_t idiv_mixed(term_t a, term_t b, heap_t *heap);
term_t div_mixed(term_t a, term_t b, heap_t *heap);
term_t bsl_mixed(term_t a, term_t b, heap_t *heap);
term_t bsr_mixed(term_t a, term_t b, heap_t *heap);
term_t bnot_mixed(term_t a, heap_t *heap);
term_t band_mixed(term_t a, term_t b, heap_t *heap);
term_t band_mixed2(term_t a, int i, heap_t *heap);
term_t bor_mixed(term_t a, term_t b, heap_t *heap);
term_t bor_mixed2(term_t a, int i, heap_t *heap);
term_t bxor_mixed(term_t a, term_t b, heap_t *heap);
term_t bxor_mixed2(term_t a, int i, heap_t *heap);

#endif
