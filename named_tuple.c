//
//
//

#include "named_tuple.h"

#include <apr_tables.h>
#include <apr_hash.h>

typedef struct nm_tuple_t nm_tuple_t;
struct nm_tuple_t {
	term_t name;					// key
	apr_array_header_t *fields;		// term_t (atoms)
};

struct named_tuples_t {
	apr_pool_t *pool;
	apr_hash_t *nm_tuples;			// key = name
};

named_tuples_t *named_tuples_make(apr_pool_t *pool)
{
	named_tuples_t *nts = apr_palloc(pool, sizeof(*nts));
	nts->pool = pool;
	nts->nm_tuples = apr_hash_make(pool);
	return nts;
}

int named_tuples_set(named_tuples_t *self, term_t name, term_t field)
{
	nm_tuple_t *nt = apr_hash_get(self->nm_tuples, &name, sizeof(name));
	if (nt != 0)
	{
		int i;
		for (i = 0; i < nt->fields->nelts; i++)
		{
			if (APR_ARRAY_IDX(nt->fields, i, term_t) == field)
				break;
		}
		
		if (i < nt->fields->nelts)
			return i+1;

		APR_ARRAY_PUSH(nt->fields, term_t) = field;
		return nt->fields->nelts;
	}
	else
	{
		nt = apr_palloc(self->pool, sizeof(*nt));
		nt->name = name;
		nt->fields = apr_array_make(self->pool, 4, sizeof(term_t));
		APR_ARRAY_PUSH(nt->fields, term_t) = field;
		apr_hash_set(self->nm_tuples, &nt->name, sizeof(nt->name), nt);
		return 1;
	}
}

//EOF
