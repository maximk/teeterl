#ifndef PROC_H
#define PROC_H

#include <apr_time.h>
#include <apr_tables.h>

#include "term.h"
#include "heap.h"
#include "teevm.h"
#include "msg_queue.h"

typedef struct proc_t proc_t;

#define NUM_ARGS	256

#define SLICE_REDUCTIONS	1000

// gc is run for heaps exceeding the threshold
#define HEAP_SIZE_THRESH1	(4096*1024)
#define HEAP_SIZE_THRESH2	(16384*1024)

// maximum nested levels or receives; it was 1 in teeterl and nobody noticed
#define RECV_IN_RECV		8

#define INFINITY		(-1)
#define TIMEOUT_ZERO	(0)

// a generic function pointer to avoid warnings
typedef term_t (*bifN_t)(proc_t *proc);

typedef term_t (*bif0_t)(proc_t *proc);
typedef term_t (*bif1_t)(term_t a, proc_t *proc);
typedef term_t (*bif2_t)(term_t a, term_t b, proc_t *proc);
typedef term_t (*bif3_t)(term_t a, term_t b, term_t c, proc_t *proc);
typedef term_t (*bif4_t)(term_t a, term_t b, term_t c, term_t d, proc_t *proc);
typedef term_t (*bif5_t)(term_t a, term_t b, term_t c, term_t d, term_t e, proc_t *proc);
typedef term_t (*bif6_t)(term_t a, term_t b, term_t c, term_t d, term_t e, term_t f, proc_t *proc);
typedef term_t (*bif7_t)(term_t a, term_t b, term_t c, term_t d, term_t e, term_t f, term_t g, proc_t *proc);
typedef term_t (*bif8_t)(term_t a, term_t b, term_t c, term_t d, term_t e, term_t f, term_t g, term_t h, proc_t *proc);

typedef union codel_t codel_t;
union codel_t {
	int i;
	uint n;
	term_t t;
	codel_t *l;
	bifN_t bif;
};

typedef struct module_t module_t;

typedef struct frame_t frame_t;
struct frame_t {
	codel_t *saved_ip;
	module_t *saved_module;
	int saved_nslots;
	int saved_heap_needed;
	term_t slots[0];
};

#define SLICE_RESULT_NONE	0
#define SLICE_RESULT_YIELD	100
#define SLICE_RESULT_WAIT	101
#define SLICE_RESULT_DONE	102
#define SLICE_RESULT_EXIT	103
#define SLICE_RESULT_ERROR	104
#define SLICE_RESULT_THROW	105

typedef struct slice_result_t slice_result_t;
struct slice_result_t {
	int what;
	union {
		term_t reason;
		apr_time_t until_when;
	};
};

//contains information needed to resume the process
typedef struct capsule_t capsule_t;
struct capsule_t
{
	module_t *module;
	codel_t *ip;
	int arity;
	term_t registers[NUM_ARGS];
	frame_t *my_frame;
	int nslots;
	int heap_needed;
};

#define MY_QUEUE_NONE			0
#define MY_QUEUE_HIGH			1
#define MY_QUEUE_NORMAL			2
#define MY_QUEUE_LOW			3
#define MY_QUEUE_TIMED_WAIT		4
#define MY_QUEUE_INF_WAIT		5

typedef struct catch_t catch_t;
struct catch_t {
	uint frame_offset;
	codel_t *on_exception;
};

typedef struct dict_pair_t dict_pair_t;
struct dict_pair_t {
	term_t key;
	term_t value;
};

struct proc_t {
	uint serial;
	term_t reg_name;
	teevm_t *teevm;			// refs to code_base, atoms, etc
	apr_pool_t *proc_pool;
	apr_memnode_t *stack_node;
	apr_array_header_t *catches;
	heap_t *heap;
	msg_queue_t *mailbox;
	apr_array_header_t *dictionary;
	int ntimeouts;
	apr_time_t timeouts[RECV_IN_RECV];	// TODO: do we really need more nesting levels?
	slice_result_t result;
	capsule_t capsule;
	term_t priority;
	int my_queue;	// save name of the current queue to facilitate waking up on mail
};

proc_t *proc_make(teevm_t *teevm);
term_t proc_main(proc_t *proc, module_t *module, codel_t *ip, int argc, term_t argv[]);

#define proc_id(__proc)		tag_short_pid((__proc)->serial)

int proc_is_obese(proc_t *proc);
int proc_burn_trash(proc_t *proc, int *times, int *reclaimed);

void proc_destroy(proc_t *proc);

#endif
