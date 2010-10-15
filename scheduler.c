//
//
//

#include <apr_hash.h>

#include "teeterl.h"
#include "proc.h"
#include "scheduler.h"

#include "proc_queue.h"
#include "outlet_mall.h"
#include "atom_defs.h"

#include "code_base.h"
#include "atom.h"
#include "cstr.h"
#include "list.h"

#include <stdlib.h>

#define INFINITY			(-1)

#define NORMAL_ADVANTAGE	8

const char *stringify_term(term_t t, atoms_t *atoms, apr_pool_t *pool);

void scheduler_park_runnable(scheduler_t *self, proc_t *proc);

#ifndef SUPPRESS_TRACES
void scheduler_trace_exit(scheduler_t *self, proc_t *proc);
#endif

void scheduler_dump_core(scheduler_t *self, proc_t *proc);

struct scheduler_t {
	uint next_serial;

	apr_hash_t *registry;
	apr_hash_t *named_processes;

	proc_queue_t *high_prio;
	proc_queue_t *normal_prio;
	proc_queue_t *low_prio;

	int normal_count;

	wait_list_t *on_timed_receive;
	proc_list_t *on_infinite_receive;

	outlet_mall_t *mall;

	statistics_t stats;

#ifndef SUPPRESS_TRACES
	uint trace_mask;
#endif
};

scheduler_t *scheduler_make(outlet_mall_t *mall, apr_pool_t *pool)
{
	scheduler_t *sched = apr_palloc(pool, sizeof(*sched));
	sched->next_serial = 0;
	sched->registry = apr_hash_make(pool);
	sched->named_processes = apr_hash_make(pool);
	sched->high_prio = proc_queue_make(pool);
	sched->normal_prio = proc_queue_make(pool);
	sched->low_prio = proc_queue_make(pool);
	sched->on_timed_receive = wait_list_make(pool);
	sched->on_infinite_receive = proc_list_make(pool);
	sched->mall = mall;
	sched->trace_mask = TRACE_PROCESS_EXITS | TRACE_PROCESS_SPAWNS;

	sched->stats.context_switches = 0;
	sched->stats.gc_count = 0;
	sched->stats.memory_reclaimed = 0;
	sched->stats.input_total = 0;
	sched->stats.output_total = 0;
	sched->stats.reductions = 0;
	sched->stats.reductions_slc = 0;
	sched->stats.runtime = 0;
	sched->stats.runtime_slc = 0;
	sched->stats.wall_clock = 0;
	sched->stats.wall_clock_slc = 0;

	return sched;
}

#ifndef SUPPRESS_TRACES
void scheduler_trace(scheduler_t *self, uint mask, int yes)
{
	if (yes)
		self->trace_mask |= mask;
	else
		self->trace_mask &= ~mask;
}
#endif

// to be used by first_born process only
void scheduler_enlist0(scheduler_t *self, proc_t *first_born)
{
	first_born->serial = self->next_serial++;
	apr_hash_set(self->registry, &first_born->serial, sizeof(first_born->serial), first_born);
}

void scheduler_enlist(scheduler_t *self, proc_t *spawning)
{
	spawning->serial = self->next_serial++;
	apr_hash_set(self->registry, &spawning->serial, sizeof(spawning->serial), spawning);
	// usually called by spawning BIFs thus the process should be put on queue
	scheduler_park_runnable(self, spawning);
}

proc_t *scheduler_lookup(scheduler_t *self, uint serial)
{
	return apr_hash_get(self->registry, &serial, sizeof(serial));
}

void scheduler_new_local_mail(scheduler_t *self, proc_t *proc, term_t msg)
{
	//term_t m = heap_marshal(msg, proc->heap); -- marshalling done by the caller
	msg_queue_push(proc->mailbox, msg);

	if (proc->my_queue == MY_QUEUE_NONE)
		return;
	if (proc->my_queue == MY_QUEUE_NORMAL ||
		proc->my_queue == MY_QUEUE_HIGH ||
		proc->my_queue == MY_QUEUE_LOW)
		return;

	if (proc->my_queue == MY_QUEUE_TIMED_WAIT)
		wait_list_remove(self->on_timed_receive, proc);
	else
	{
		assert(proc->my_queue == MY_QUEUE_INF_WAIT);
		proc_list_remove(self->on_infinite_receive, proc);
	}
	proc->my_queue = MY_QUEUE_NONE;

	scheduler_park_runnable(self, proc);
}

int scheduler_slim_process(scheduler_t *self, proc_t *proc)
{
	int times, reclaimed;
	int result;

	if (!proc_is_obese(proc))
		return 0;
	
	result = proc_burn_trash(proc, &times, &reclaimed);

	//stats update
	self->stats.gc_count += times;
	self->stats.memory_reclaimed += reclaimed;

	return result;
}

proc_t *scheduler_next(scheduler_t *self, proc_t *current)
{
	apr_status_t rs;
	proc_t *next_proc = NULL;

	assert(current->my_queue == MY_QUEUE_NONE);

	switch (current->result.what)
	{
	case SLICE_RESULT_YIELD:
		if (scheduler_slim_process(self, current) != 0)
			scheduler_exit_process(self, current, A_CANCER);
		else
			scheduler_park_runnable(self, current);
		break;
	case SLICE_RESULT_WAIT:
		if (scheduler_slim_process(self, current) != 0)
			scheduler_exit_process(self, current, A_CANCER);
		else if (current->result.until_when < 0)	// infinity
		{
			proc_list_put(self->on_infinite_receive, current);
			current->my_queue = MY_QUEUE_INF_WAIT;
		}
		else
		{
			wait_list_put(self->on_timed_receive, current, current->result.until_when);
			current->my_queue = MY_QUEUE_TIMED_WAIT;
		}
		break;
	case SLICE_RESULT_DONE:
		scheduler_exit_process(self, current, A_NORMAL);
		break;
	case SLICE_RESULT_EXIT:
		scheduler_exit_process(self, current, current->result.reason);
		// what about the returned value when main function just returns?
		break;
	case SLICE_RESULT_ERROR:
		scheduler_exit_process(self, current, current->result.reason);
		// how is this different from SLICE_RESULT_EXIT?
		break;
	case SLICE_RESULT_THROW:
		scheduler_exit_process(self, current, current->result.reason);
		// how is this different from SLICE_RESULT_EXIT?
		break;
	}

	rs = outlet_mall_poll(self->mall, 0);	// do not wait on outlets
	assert(rs == APR_SUCCESS);

	do	{
		// select_runnable
		if (!proc_queue_is_empty(self->high_prio))
			next_proc = proc_queue_get(self->high_prio);
		else if (self->normal_count < NORMAL_ADVANTAGE)
		{
			if (!proc_queue_is_empty(self->normal_prio))
				next_proc = proc_queue_get(self->normal_prio);
			else if (!proc_queue_is_empty(self->low_prio))
				next_proc = proc_queue_get(self->low_prio);
			self->normal_count++;
		}
		else
		{
			if (!proc_queue_is_empty(self->low_prio))
				next_proc = proc_queue_get(self->low_prio);
			else if (!proc_queue_is_empty(self->normal_prio))
				next_proc = proc_queue_get(self->normal_prio);
			self->normal_count = 0;
		}

		if (next_proc == NULL)
		{
			// select_expired
			apr_time_t now = apr_time_now();
			proc_t *expired;
			int ne = 0;
			while ((expired = wait_list_expired(self->on_timed_receive, now)) != NULL)
			{
				scheduler_park_runnable(self, expired);
				ne++;
			}

			if (ne > 0)
				continue;	// at least one waiting process expired

			if (!wait_list_is_empty(self->on_timed_receive))
			{
				apr_interval_time_t gap = wait_list_gap(self->on_timed_receive, now);

				outlet_mall_poll(self->mall, gap);
				continue;
			}
			else
			{
				if (!outlet_mall_is_dark(self->mall))
				{
					outlet_mall_poll(self->mall, INFINITY);
					continue;
				}
				else
					break;
			}
		}
		else
			next_proc->my_queue = MY_QUEUE_NONE;

	} while (next_proc == NULL);

	//stats update
	self->stats.context_switches++;

	return next_proc;
}

void scheduler_park_runnable(scheduler_t *self, proc_t *proc)
{
	term_t prio = proc->priority;
	if (prio == A_NORMAL)
	{
		proc_queue_put(self->normal_prio, proc);
		proc->my_queue = MY_QUEUE_NORMAL;
	}
	else if (prio == A_HIGH)
	{
		proc_queue_put(self->high_prio, proc);
		proc->my_queue = MY_QUEUE_HIGH;
	}
	else // PRIO_LOW
	{
		proc_queue_put(self->low_prio, proc);
		proc->my_queue = MY_QUEUE_LOW;
	}
}

const char *term2html(term_t t, atoms_t *atoms, apr_pool_t *pool);

void scheduler_exit_process(scheduler_t *self, proc_t *proc, term_t reason)
{
	// TODO linked processes and monitors
	// TODO trap_exit

	// forcibly exited processes do not have their result set
	if (proc->my_queue != MY_QUEUE_NONE)
	{
		proc->result.what = SLICE_RESULT_EXIT;
		proc->result.reason = reason;
	}
	
	switch (proc->my_queue)
	{
	case MY_QUEUE_NONE:
		break;
	case MY_QUEUE_NORMAL:
		proc_queue_remove(self->normal_prio, proc);
		proc->my_queue = MY_QUEUE_NONE;
		break;
	case MY_QUEUE_HIGH:
		proc_queue_remove(self->high_prio, proc);
		proc->my_queue = MY_QUEUE_NONE;
		break;
	case MY_QUEUE_LOW:
		proc_queue_remove(self->low_prio, proc);
		proc->my_queue = MY_QUEUE_NONE;
		break;
	case MY_QUEUE_TIMED_WAIT:
		wait_list_remove(self->on_timed_receive, proc);
		proc->my_queue = MY_QUEUE_NONE;
		break;
	case MY_QUEUE_INF_WAIT:
		proc_list_remove(self->on_infinite_receive, proc);
		proc->my_queue = MY_QUEUE_NONE;
		break;
	}

	if (proc->reg_name == A_INIT)
		scheduler_dump_core(self, proc);

#ifndef SUPPRESS_TRACES
	if (self->trace_mask & TRACE_PROCESS_EXITS)
		scheduler_trace_exit(self, proc);
#endif

	if (proc->reg_name != noval)
		apr_hash_set(self->named_processes, &proc->reg_name, sizeof(proc->reg_name), 0);
	apr_hash_set(self->registry, &proc->serial, sizeof(proc->serial), 0);

	proc_destroy(proc);
}

void scheduler_register(scheduler_t *self, proc_t *proc, term_t reg_name)
{
	proc->reg_name = reg_name;
	apr_hash_set(self->named_processes, &proc->reg_name, sizeof(proc->reg_name), proc);
}

proc_t *scheduler_lookup_by_name(scheduler_t *self, term_t reg_name)
{
	return apr_hash_get(self->named_processes, &reg_name, sizeof(reg_name));
}

void scheduler_unregister(scheduler_t *self, term_t reg_name)
{
	proc_t *proc = apr_hash_get(self->named_processes, &reg_name, sizeof(reg_name));
	if (proc != 0)
	{
		proc->reg_name = noval;
		apr_hash_set(self->named_processes, &reg_name, sizeof(reg_name), 0);
	}
}

statistics_t *scheduler_stats(scheduler_t *self)
{
	return &self->stats;
}

int scheduler_proc_queues(scheduler_t *self, int *wait_count, int *timed_wait_count)
{
	int q1 = proc_queue_count(self->high_prio);
	int q2 = proc_queue_count(self->normal_prio);
	int q3 = proc_queue_count(self->low_prio);

	if (wait_count != NULL)
		*wait_count = proc_list_count(self->on_infinite_receive);
	if (timed_wait_count != NULL)
		*timed_wait_count = wait_list_count(self->on_timed_receive);

	return q1 + q2 + q3;
}

#ifndef SUPPRESS_TRACES
void scheduler_trace_exit(scheduler_t *self, proc_t *proc)
{
	proc_t *init = scheduler_lookup_by_name(self, A_INIT);
	term_t pm = heap_tuple(init->heap, 12);		// NB: sync with trace.hrl
	term_box_t *tb = peel(pm);

	int offset = proc->capsule.ip - proc->capsule.module->code;
	const char *file;
	int line;
	term_t source = A_FALSE;

	if (module_source_from_offset(proc->capsule.module, offset, &file, &line))
		source = heap_tuple2(init->heap,
			ztol(file, init->heap),
			tag_int(line));

	tb->tuple.elts[0] = A_POSTMORTEM;
	tb->tuple.elts[1] = proc_id(proc);
	tb->tuple.elts[2] = (proc->reg_name != noval) ?proc->reg_name :A_UNDEFINED;
	tb->tuple.elts[3] = proc->capsule.module->key.module;
	tb->tuple.elts[4] = tag_int(proc->capsule.ip - proc->capsule.module->code);
	tb->tuple.elts[5] = source;
	tb->tuple.elts[6] = nil;	// TODO: call stack
	tb->tuple.elts[7] = nil;	// TODO: registers
	tb->tuple.elts[8] = nil;	// TODO: slots
	tb->tuple.elts[9] = tag_int(heap_size(proc->heap));
	tb->tuple.elts[10] = tag_int(proc->result.what);
	tb->tuple.elts[11] = heap_marshal(proc->result.reason, init->heap);

	scheduler_new_local_mail(self, init, pm);
}
#endif

void scheduler_dump_core(scheduler_t *self, proc_t *proc)
{
	cstr_t *s = atoms_get(proc->teevm->atoms, atom_index(proc->reg_name));
	module_t *m = proc->capsule.module;
	int offset = proc->capsule.ip - m->code;
	const char *file;
	int line;

	fprintf(stderr, "******\n");
	fprintf(stderr, "*** process '%*s' exits unexpectedly\n", s->size, s->data);
	if (module_source_from_offset(m, offset, &file, &line))
		fprintf(stderr, "***     at %s:%d\n", file, line);
	else
	{
		cstr_t *q = atoms_get(proc->teevm->atoms, atom_index(m->key.module));
		fprintf(stderr, "***     at mod %*s off %d\n", q->size, q->data, offset);
	}
	
	apr_pool_t *tmp;
	apr_pool_create(&tmp, 0);
	const char *r = stringify_term(proc->result.reason, proc->teevm->atoms, tmp);
	fprintf(stderr, "*** reason=%s\n", r);
	apr_pool_destroy(tmp);
	fprintf(stderr, "******\n");

	exit(1);
}

//EOF
