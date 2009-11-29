#ifndef PROC_QUEUE_H
#define PROC_QUEUE_H

#include <apr_general.h>

#include "proc.h"

typedef struct proc_queue_t proc_queue_t;
typedef struct wait_list_t wait_list_t;
typedef struct proc_list_t proc_list_t;

// a process queue; a single queue for each process priority
proc_queue_t *proc_queue_make(apr_pool_t *pool);
void proc_queue_put(proc_queue_t *pq, proc_t *proc);
int proc_queue_is_empty(proc_queue_t *pq);
int proc_queue_count(proc_queue_t *pq);
proc_t *proc_queue_get(proc_queue_t *pq);
void proc_queue_remove(proc_queue_t *pq, proc_t *proc);

// a timed list of properties, e.g. processes waiting on receive with timeout
wait_list_t *wait_list_make(apr_pool_t *pool);
void wait_list_put(wait_list_t *self, proc_t *proc, apr_time_t expire);
void wait_list_remove(wait_list_t *self, proc_t *proc);
int wait_list_is_empty(wait_list_t *self);
int wait_list_count(wait_list_t *self);
proc_t *wait_list_expired(wait_list_t *self, apr_time_t now);
apr_interval_time_t wait_list_gap(wait_list_t *self, apr_time_t now);

// a simple list of processes, e.g. processes waiting for infinity
proc_list_t *proc_list_make(apr_pool_t *pool);
void proc_list_put(proc_list_t *self, proc_t *proc);
void proc_list_remove(proc_list_t *self, proc_t *proc);
int proc_list_count(proc_list_t *self);

#endif
