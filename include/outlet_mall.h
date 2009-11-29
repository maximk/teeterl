#ifndef OUTLET_MALL_H
#define OUTLET_MALL_H

#include <apr_time.h>

#include "outlet.h"

typedef struct outlet_mall_t outlet_mall_t;

outlet_mall_t *outlet_mall_make(apr_pool_t *pool);
void outlet_mall_allot(outlet_mall_t *mall, outlet_t *ol);
void outlet_mall_kick_out(outlet_mall_t *mall, outlet_t *ol);
outlet_t *outlet_mall_lookup(outlet_mall_t *mall, uint serial);
int outlet_mall_is_dark(outlet_mall_t *mall);
apr_status_t outlet_mall_poll(outlet_mall_t *mall, apr_interval_time_t for_how_long);

#endif
