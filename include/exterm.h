#ifndef EXtERM_H
#define EXTERM_H

#include "term.h"
#include "atom.h"
#include "heap.h"

term_t binary_to_term(term_t bin, atoms_t *atoms, heap_t *hp);
term_t term_to_binary(term_t t, atoms_t *atoms, heap_t *hp);

#endif
