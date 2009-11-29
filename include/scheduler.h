#ifndef SCHEDULER_H
#define SCHEDULER_H

#include "proc_queue.h"
#include "outlet_mall.h"

#ifndef SUPPRESS_TRACES
#define TRACE_PROCESS_EXITS			1
#define TRACE_PROCESS_SPAWNS		2
#define TRACE_PROCESS_SWITCHES		4
#endif

typedef struct statistics_t statistics_t;
struct statistics_t {
	//context_switches
	int context_switches;

	//garbage_collection
	int gc_count;
	int memory_reclaimed;

	//io
	int input_total;
	int output_total;

	//reductions
	int reductions;
	int reductions_slc;

	//runtime
	apr_interval_time_t runtime;
	apr_interval_time_t runtime_slc;

	//wall_clock
	apr_interval_time_t wall_clock;
	apr_interval_time_t wall_clock_slc;
};

typedef struct scheduler_t scheduler_t;

scheduler_t *scheduler_make(outlet_mall_t *mall, apr_pool_t *pool);
#ifndef SUPPRESS_TRACES
void scheduler_trace(scheduler_t *self, uint mask, int yes);
#endif
void scheduler_enlist0(scheduler_t *self, proc_t *first_born);
void scheduler_enlist(scheduler_t *self, proc_t *spawning);
proc_t *scheduler_lookup(scheduler_t *self, uint serial);
void scheduler_new_local_mail(scheduler_t *self, proc_t *proc, term_t msg);
//void scheduler_enqueue(scheduler_t *self, proc_t *proc);
//void scheduler_remove(scheduler_t *self, proc_t *proc);
proc_t *scheduler_next(scheduler_t *self, proc_t *current);
void scheduler_exit_process(scheduler_t *self, proc_t *proc, term_t reason);

void scheduler_register(scheduler_t *self, proc_t *proc, term_t reg_name);
proc_t *scheduler_lookup_by_name(scheduler_t *self, term_t reg_name);
void scheduler_unregister(scheduler_t *self, term_t reg_name);

statistics_t *scheduler_stats(scheduler_t *self);
int scheduler_proc_queues(scheduler_t *self, int *wait_count, int *timed_wait_count);

#endif
