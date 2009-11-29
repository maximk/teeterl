#ifndef MODBIN_H
#define MODBIN_H

#include <apr_general.h>
#include "cstr.h"

typedef struct modbin_t modbin_t;
struct modbin_t {
	cstr_t *name;
	int is_preloaded;
	int size;
	apr_byte_t data[0];
};

extern modbin_t *modbins[];

#endif
