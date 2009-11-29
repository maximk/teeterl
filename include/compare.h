//
//
//

#include "term.h"
#include "atom.h"

#define is_term_bigger(__a, __b, __atoms)	is_term_smaller((__b), (__a), (__atoms))

int are_terms_equal(term_t a, term_t b);
int is_term_smaller(term_t a, term_t b, atoms_t *atoms);

//EOF
