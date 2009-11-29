#ifndef TEETERL_H
#define TEETERL_H

#include <apr_general.h>

#include "term.h"

#ifdef DEBUG
#define assert(x) \
	do { \
		if (!(x)) \
		{ \
			fprintf(stderr, "Assertion failed at %s:%d\n", __FILE__, __LINE__); \
			exit(1); \
		} \
	} while (0)
#else
#define assert(x)
#endif

#define not_implemented(what) \
	do { \
		fprintf(stderr, "not implemented: %s\n", what); \
		exit(1); \
	} while (0)

term_t decipher_status(apr_status_t rs);

#endif
