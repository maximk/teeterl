//
//
//

#include "proc_queue.h"

#include <apr_tables.h>
#include <apr_time.h>

#include "teeterl.h"

#include <stdio.h>
#include <stdlib.h>

struct proc_queue_t {
	apr_array_header_t *q;
	int hd;
};

typedef struct wl_item_t wl_item_t;
struct wl_item_t {
	proc_t *proc;
	apr_time_t expire;
};

struct wait_list_t {
	apr_array_header_t *w;
};

struct proc_list_t {
	apr_array_header_t *l;
};

// a process queue; a single queue for each process priority
proc_queue_t *proc_queue_make(apr_pool_t *pool)
{
	proc_queue_t *pq = apr_palloc(pool, sizeof(*pq));
	pq->q = apr_array_make(pool, 16, sizeof(proc_t *));
	pq->hd = 0;
	return pq;
}

void proc_queue_put(proc_queue_t *pq, proc_t *proc)
{
	APR_ARRAY_PUSH(pq->q, proc_t *) = proc;
}

int proc_queue_is_empty(proc_queue_t *pq)
{
	return pq->q->nelts == pq->hd;
}

int proc_queue_count(proc_queue_t *pq)
{
	return pq->q->nelts - pq->hd;
}

void proc_queue_remove(proc_queue_t *pq, proc_t *proc)
{
	proc_t **ptr = (proc_t **)pq->q->elts + pq->hd;
	proc_t **end = (proc_t **)pq->q->elts + pq->q->nelts;
	while (ptr < end)
	{
		if (*ptr == proc)
		{
			proc_t **last = (proc_t **)apr_array_pop(pq->q);
			memmove(ptr, ptr+1, (last-ptr)*sizeof(proc_t *));
			return;
		}
		ptr++;
	}
}

proc_t *proc_queue_get(proc_queue_t *pq)
{
	proc_t *proc;
	int n;
	if (pq->hd == pq->q->nelts)
		return 0;
	proc = ((proc_t **)pq->q->elts)[pq->hd++];
	n = pq->q->nelts - pq->hd;
	if (n > 16 && pq->hd > n/2)
	{
		proc_t **ps = (proc_t **)pq->q->elts;
		memcpy(ps, ps + pq->hd, n*sizeof(proc_t *));
		pq->q->nelts = n;
		pq->hd = 0;
	}
	assert(proc != NULL);
	return proc;
}

// a timed list of properties, e.g. processes waiting on receive with timeout
wait_list_t *wait_list_make(apr_pool_t *pool)
{
	wait_list_t *wlist = apr_palloc(pool, sizeof(*wlist));
	wlist->w = apr_array_make(pool, 4, sizeof(wl_item_t));
	return wlist;
}

void wait_list_put(wait_list_t *self, proc_t *proc, apr_time_t expire)
{
	wl_item_t *ptr = (wl_item_t *)self->w->elts;
	wl_item_t *end = ptr + self->w->nelts;

	while (ptr < end)
	{
		if (ptr->expire > expire)
		{
			wl_item_t *last = (wl_item_t *)apr_array_push(self->w);
			memmove(ptr+1, ptr, (last-ptr)*sizeof(wl_item_t));
			ptr->proc = proc;
			ptr->expire = expire;
			return;
		}
		ptr++;
	}

	ptr = (wl_item_t *)apr_array_push(self->w);
	ptr->proc = proc;
	ptr->expire = expire;
}

void wait_list_remove(wait_list_t *self, proc_t *proc)
{
	wl_item_t *ptr = (wl_item_t *)self->w->elts;
	wl_item_t *end = ptr + self->w->nelts;

	while (ptr < end)
	{
		if (ptr->proc == proc)
		{
			wl_item_t *last = (wl_item_t *)apr_array_pop(self->w);
			memmove(ptr, ptr+1, (last-ptr)*sizeof(wl_item_t));
			return;
		}
		ptr++;
	}
}

int wait_list_is_empty(wait_list_t *self)
{
	return apr_is_empty_array(self->w);
}

int wait_list_count(wait_list_t *self)
{
	return self->w->nelts;
}

proc_t *wait_list_expired(wait_list_t *self, apr_time_t now)
{
	if (apr_is_empty_array(self->w))
		return 0;
	else
	{
		wl_item_t *ptr = (wl_item_t *)self->w->elts;
		if (ptr->expire <= now)
		{
			wl_item_t *last = (wl_item_t *)apr_array_pop(self->w);
			proc_t *expired = ptr->proc;
			memmove(ptr, ptr+1, (last-ptr)*sizeof(wl_item_t));
			return expired;
		}
		else
			return 0;
	}
}

apr_interval_time_t wait_list_gap(wait_list_t *self, apr_time_t now)
{
	wl_item_t *ptr = (wl_item_t *)self->w->elts;
	return (ptr->expire - now);
}

// a simple list of processes, e.g. processes waiting for infinity
proc_list_t *proc_list_make(apr_pool_t *pool)
{
	proc_list_t *pl = apr_palloc(pool, sizeof(*pl));
	pl->l = apr_array_make(pool, 16, sizeof(proc_t *));
	return pl;
}

void proc_list_put(proc_list_t *self, proc_t *proc)
{
	APR_ARRAY_PUSH(self->l, proc_t *) = proc;
}

void proc_list_remove(proc_list_t *self, proc_t *proc)
{
	proc_t **ptr = (proc_t **)self->l->elts;
	proc_t **end = ptr + self->l->nelts;
	while (ptr < end)
	{
		if (*ptr == proc)
		{
			if (ptr < end-1)
				*ptr = *(end-1);
			self->l->nelts--;
			return;
		}
		ptr++;
	}
}

int proc_list_count(proc_list_t *self)
{
	return self->l->nelts;
}

//EOF
