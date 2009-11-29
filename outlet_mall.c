//
//
//

#include "outlet_mall.h"

#include <apr_hash.h>
#include <apr_poll.h>

#include "term.h"

struct outlet_mall_t {
	uint next_serial;
	apr_hash_t *shops;
};

outlet_mall_t *outlet_mall_make(apr_pool_t *pool)
{
	outlet_mall_t *mall = apr_palloc(pool, sizeof(*mall));
	mall->next_serial = 0;
	mall->shops = apr_hash_make(pool);
	return mall;
}

void outlet_mall_allot(outlet_mall_t *mall, outlet_t *ol)
{
	ol->serial = mall->next_serial++;
	apr_hash_set(mall->shops, &ol->serial, sizeof(ol->serial), ol);
}

void outlet_mall_kick_out(outlet_mall_t *mall, outlet_t *ol)
{
	apr_hash_set(mall->shops, &ol->serial, sizeof(ol->serial), 0);
}

outlet_t *outlet_mall_lookup(outlet_mall_t *mall, uint serial)
{
	return apr_hash_get(mall->shops, &serial, sizeof(serial));
}

int outlet_mall_is_dark(outlet_mall_t *mall)
{
	// TODO: there may be some passive outlets; so they will never wake up anybody
	return apr_hash_count(mall->shops) == 0;
}

apr_status_t outlet_mall_poll(outlet_mall_t *mall, apr_interval_time_t for_how_long)
{
	apr_status_t rs = 0;

	apr_pool_t *tmp;
	apr_pollset_t *pollset;
	int file_count = 0;
	int poll_count = 0;

	apr_hash_index_t *hi;

	int num, i;
	const apr_pollfd_t *descriptors;

	if (apr_hash_count(mall->shops) == 0)
	{
		if (for_how_long > 0)
			apr_sleep(for_how_long);
		return 0;
	}

#if !APR_FILES_AS_SOCKETS
	for (hi = apr_hash_first(0, mall->shops); hi; hi = apr_hash_next(hi))
	{
		apr_int16_t reqevents = 0;
		int nshops = apr_hash_count(mall->shops);

		outlet_t *outlet;
		apr_hash_this(hi, 0, 0, &outlet);

		if (!outlet_is_file(outlet))
			continue;

		if (outlet_want_read(outlet))
		{
			file_count++;
			rs = outlet_try_read(outlet, for_how_long/nshops);
		}

		if (rs == 0 && outlet_want_write(outlet))
		{
			file_count++;
			rs = outlet_try_write(outlet, for_how_long/nshops);
		}

		if (rs != 0)
		{
			outlet_close0(outlet);
			outlet_mall_kick_out(mall, outlet);
			apr_pool_destroy(outlet->pool);
		}
	}
#endif

	apr_pool_create(&tmp, 0);
	rs = apr_pollset_create(&pollset, apr_hash_count(mall->shops), tmp, 0);
	if (rs != 0)
	{
		apr_pool_destroy(tmp);
		return rs;
	}

	for (hi = apr_hash_first(0, mall->shops); hi; hi = apr_hash_next(hi))
	{
		apr_int16_t reqevents = 0;

		outlet_t *outlet;
		apr_hash_this(hi, 0, 0, (void **)&outlet);

#if !APR_FILES_AS_SOCKETS
		if (outlet_is_file(outlet))
			continue;
#endif

		if (outlet_want_read(outlet))
			reqevents |= APR_POLLIN;
		if (outlet_want_write(outlet))
			reqevents |= APR_POLLOUT;

		if (reqevents != 0)
		{
			apr_pollfd_t fd;

			if (outlet_is_socket(outlet))
			{
				fd.desc_type = APR_POLL_SOCKET;
				fd.desc.s = outlet_get_socket(outlet);
			}
			else if (outlet_is_file(outlet))
			{
				fd.desc_type = APR_POLL_FILE;
				fd.desc.f = outlet_get_file(outlet);
			}
			else
				return APR_ENOTIMPL;

			fd.reqevents = reqevents;
			fd.client_data = outlet;

			rs = apr_pollset_add(pollset, &fd);
			if (rs != 0)
			{
				apr_pool_destroy(tmp);
				return rs;
			}
			poll_count++;
		}
	}

	if (poll_count == 0)
	{
		if (file_count == 0 && for_how_long > 0)
			apr_sleep(for_how_long);
		apr_pool_destroy(tmp);
		return 0;
	}

	rs = apr_pollset_poll(pollset, for_how_long, &num, &descriptors);
	if (rs != 0 && !APR_STATUS_IS_TIMEUP(rs))
	{
		apr_pool_destroy(tmp);
		return rs;
	}

	for (i = 0; i < num; i++)
	{
		outlet_t *outlet = (outlet_t *)descriptors[i].client_data;
		if ((descriptors[i].rtnevents & APR_POLLIN) != 0)
			rs = outlet_do_readable(outlet);
		if (rs == 0 && (descriptors[i].rtnevents & APR_POLLOUT) != 0)
			rs = outlet_do_writable(outlet);

		if (rs != 0)
		{
			outlet_close0(outlet);
			outlet_mall_kick_out(mall, outlet);
			apr_pool_destroy(outlet->pool);
		}
	}

	apr_pool_destroy(tmp);
	return 0;
}

//EOF
