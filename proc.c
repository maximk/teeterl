//
//
//

#include "atom_defs.h"
#include "proc.h"
#include "teevm.h"
#include "heap_gc.h"

#define INIT_STACK_SIZE	8192

void burn_trash_once(proc_t *proc);

proc_t *proc_make(teevm_t *teevm)
{
	apr_pool_t *proc_pool;
	apr_allocator_t *allocator;
	proc_t *proc;
	frame_t *my_frame;

	apr_pool_create(&proc_pool, 0);
	allocator = apr_pool_allocator_get(proc_pool);

	proc = apr_palloc(proc_pool, sizeof(*proc));
	proc->serial = (uint)-1;
	proc->reg_name = noval;
	proc->proc_pool = proc_pool;
	proc->teevm = teevm;
	proc->heap = heap_make(proc_pool);

	proc->mailbox = msg_queue_make(proc_pool);

	proc->dictionary = apr_array_make(proc_pool, 0, sizeof(dict_pair_t));

	proc->ntimeouts = 0;

	proc->stack_node = apr_allocator_alloc(allocator, INIT_STACK_SIZE);
	// add a fake frame to stack to provide for smoother return; it may belong to proc_spawn?
	my_frame = (frame_t *)proc->stack_node->first_avail;
	my_frame->saved_ip = 0;
	my_frame->saved_module = 0;
	my_frame->saved_nslots = 0;
	my_frame->saved_heap_needed = 0;
	proc->stack_node->first_avail += sizeof(frame_t);

	proc->catches = apr_array_make(proc_pool, 4, sizeof(catch_t));

	proc->result.what = SLICE_RESULT_NONE;
	memset(&proc->capsule, 0, sizeof(proc->capsule));
	// scheduling code reads initial values from capsule which
	// are almost immediately ovetwritten by frame command
	// my_frame pointer should be valid however
	proc->capsule.my_frame = my_frame;

	proc->priority = A_NORMAL;
	proc->my_queue = MY_QUEUE_NONE;
	return proc;
}

int proc_is_obese(proc_t *proc)
{
	return (heap_size(proc->heap) > HEAP_SIZE_THRESH1);
}

int proc_burn_trash(proc_t *proc, int *times, int *reclaimed)
{
	term_t *roots[4096];
	int root_sizes[4096];
	int nroots = 0;

	frame_t *frame;
	int nslots;

	int mailbox_len;

	int slack = heap_need_anticipated(proc->heap);

	int hsize_before, hsize_after;

	//stack_node
	frame = proc->capsule.my_frame;
	nslots = proc->capsule.nslots;

	while (1)
	{
		roots[nroots] = frame->slots;
		root_sizes[nroots++] = nslots;

		if (frame->saved_ip == 0)
			break;

		nslots = frame->saved_nslots;
		frame = (frame_t *)((char *)frame -
			nslots*sizeof(term_t) - sizeof(frame_t));
	}

	//mailbox
	roots[nroots] = msg_queue_to_array(proc->mailbox, &mailbox_len);
	root_sizes[nroots++] = mailbox_len;

	//dictionary; somewhat of a hack; depends on layout of dictionary pairs
	roots[nroots] = (term_t *)proc->dictionary->elts;
	root_sizes[nroots++] = proc->dictionary->nelts*2;

	//capsule
	roots[nroots] = proc->capsule.registers;
	root_sizes[nroots++] = proc->capsule.arity;

	//result
	roots[nroots] = &proc->result.reason;
	root_sizes[nroots++] = 1;

	hsize_before = heap_size(proc->heap);
	if (hsize_before > HEAP_SIZE_THRESH2)
	{
		// try hard
		int max_tries = heap_chunk_count(proc->heap);
		int tries = 0;

		do {
			heap_gc(proc->heap, roots, root_sizes, nroots);
			hsize_after = heap_size(proc->heap);
			tries++;
		} while (tries < max_tries && hsize_after > HEAP_SIZE_THRESH2);

		if (times != NULL)
			*times = tries;
	}
	else
	{
		heap_gc(proc->heap, roots, root_sizes, nroots);
		hsize_after = heap_size(proc->heap);

		if (times != NULL)
			*times = 1;
	}

	heap_anticipate_need(proc->heap, slack);

	if (reclaimed != NULL)
		*reclaimed = hsize_before - hsize_after;

	if (hsize_after > HEAP_SIZE_THRESH2)
		return 1;

	return 0;
}

void proc_destroy(proc_t *proc)
{
	// the whole business to make this a one-liner
	apr_pool_destroy(proc->proc_pool);
}

//EOF
