#ifndef NAMED_TUPLE
#define NAMED_TUPLE

#include <apr_general.h>

#include "term.h"

#ifndef NAMED_TUPLES_T_DEFINED
typedef struct named_tuples_t named_tuples_t;
#define NAMED_TUPLES_T_DEFINED
#endif

named_tuples_t *named_tuples_make(apr_pool_t *pool);
int named_tuples_set(named_tuples_t *self, term_t name, term_t field);
int named_tuples_arity(named_tuples_t *self, term_t name);

#endif
