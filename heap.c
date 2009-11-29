//
//
//

#include "heap.h"

#include "teeterl.h"
#include "mpi.h"
#include "binary.h"
#include "list.h"

// size of the first chunk of memory allocated
// for the newly created heap
#define HEAP_INIT_SIZE	8192

apr_status_t heap_destroy(void *data);

/* coppied from apr_pools.c */

/* Node list management helper macros; list_insert() inserts 'node'
 * before 'point'. */
#define list_insert(node, point) do {           \
    node->ref = point->ref;                     \
    *node->ref = node;                          \
    node->next = point;                         \
    point->ref = &node->next;                   \
} while (0)

/* list_remove() removes 'node' from its list. */
#define list_remove(node) do {                  \
    *node->ref = node->next;                    \
    node->next->ref = node->ref;                \
} while (0)
	
heap_t *heap_make(apr_pool_t *pool)
{
	apr_memnode_t *node;
	heap_t *hp = apr_palloc(pool, sizeof(*hp));
	hp->allocator = apr_pool_allocator_get(pool);
	
	node = apr_allocator_alloc(hp->allocator, HEAP_INIT_SIZE);
	if (node == NULL)
		return NULL;
	node->next = node;
	node->ref = &node->next;

	hp->active = node;
	
	//hp->htop = node->first_avail;
	hp->hend = node->first_avail;

	hp->alloc_size = 0;

	// gc never run, on the first run active node will be cleansed
	hp->gc_last = NULL;
	hp->gc_spot = NULL;

	// make sure the memory is freed when the pool
	// is cleared/destroyed
	apr_pool_cleanup_register(pool, hp, heap_destroy, 0);

	return hp;
}

apr_status_t heap_destroy(void *data)
{
	heap_t *hp = (heap_t *)data;
	
	// break the memnode ring before freeing
	*hp->active->ref = NULL;

	apr_allocator_free(hp->allocator, hp->active);

	return APR_SUCCESS;
}

int heap_chunk_count(heap_t *hp)
{
	int count = 0;
	apr_memnode_t *node = hp->active;
	do {
		count++;
		node = node->next;
	} while (node != hp->active);
	return count;
}

void heap_secure_space(heap_t *hp, int size)
{
	apr_memnode_t *active = hp->active;

	if (hp->hend + size > active->endp)
	{
		int anticipated_size = hp->hend - heap_htop(hp);
		active = apr_allocator_alloc(hp->allocator, anticipated_size + size);
		list_insert(active, hp->active);
		hp->active = active;

		//hp->htop = active->first_avail;
		hp->hend = heap_htop(hp) + anticipated_size;
	}

	hp->hend += size;
}

apr_byte_t *heap_alloc(heap_t *hp, int size)
{
	apr_byte_t *mem;

	size = APR_ALIGN_DEFAULT(size);
	heap_secure_space(hp, size);
	mem = (apr_byte_t *)heap_htop(hp);
	heap_htop(hp) += size;

	hp->alloc_size += size;
	return mem;
}

int heap_size(heap_t *hp)
{
	return hp->alloc_size;
}

void heap_anticipate_need(heap_t *hp, int size)
{
	size = APR_ALIGN_DEFAULT(size);
	heap_secure_space(hp, size);
}

void heap_reclaim_unused(heap_t *hp, int size)
{
	hp->hend -= size; // watch alignment when using heap_needed
}

int heap_need_anticipated(heap_t *hp)
{
	return hp->hend - heap_htop(hp);
}

term_t heap_float(heap_t *hp, double value)
{
	float_value_t *f;
	int gap = sizeof(*f);
	f = (float_value_t *)heap_alloc(hp, gap);
	f->value = value;
	return tag_float(f);
}

term_t heap_bignum0(heap_t *hp, mp_sign sign, mp_size size)
{
	bignum_t *big;
	int gap = sizeof(*big) + size*sizeof(mp_digit);
	big = (bignum_t *)heap_alloc(hp, gap);
	
	big->sign = sign;
	big->alloc = size;

	big->used = 1;
	big->dp[0] = 0;
	return tag_bignum(big);
}

term_t heap_bignum(heap_t *hp, mp_sign sign, mp_size size, mp_digit *digits)
{
	bignum_t *big;
	int gap = sizeof(*big) + size*sizeof(mp_digit);
	big = (bignum_t *)heap_alloc(hp, gap);
	
	big->sign = sign;
	big->alloc = size;

	big->used = size;
	memmove(big->dp, digits, size*sizeof(mp_digit));
	return tag_bignum(big);
}

term_t heap_binary0(heap_t *hp, int bit_size)
{
	apr_byte_t *data;
	binary_t *bin;
	int bin_size = APR_ALIGN_DEFAULT(sizeof(*bin));
	int data_size = BIN_BYTE_SIZE(bit_size);
	int gap = bin_size + data_size;
	bin = (binary_t *)heap_alloc(hp, gap);
	data = (apr_byte_t *)bin + bin_size;
	bin->bit_size = bit_size;
	bin->data = data;
	bin->parent = noval;
	bin->offset = 0;
	return tag_binary(bin);
}

// NB: root binary should be allocated in the same node as its data

term_t heap_binary(heap_t *hp, int bit_size, apr_byte_t *data)
{
	apr_byte_t *data_copy;
	binary_t *bin;
	int bin_size = APR_ALIGN_DEFAULT(sizeof(*bin));
	int data_size = BIN_BYTE_SIZE(bit_size);
	int gap = bin_size + data_size;
	bin = (binary_t *)heap_alloc(hp, gap);
	data_copy = (apr_byte_t *)bin + bin_size;
	memmove(data_copy, data, data_size);
	bin->bit_size = bit_size;
	bin->data = data_copy;
	bin->parent = noval;
	bin->offset = 0;
	return tag_binary(bin);
}

term_t heap_binary_shared(heap_t *hp, int bit_size, apr_byte_t *data, term_t parent)
{
	binary_t *bin;
	int gap = sizeof(*bin);
	bin = (binary_t *)heap_alloc(hp, gap);
	bin->bit_size = bit_size;
	bin->data = data;
	bin->parent = parent;
	bin->offset = data - (peel(parent)->binary.data);
	return tag_binary(bin);
}

term_t heap_fun(heap_t *hp, term_t module, term_t function, int arity, uint uniq, int index, term_t frozen)
{
	fun_t *fun;
	int gap = sizeof(*fun);
	fun = (fun_t *)heap_alloc(hp, gap);
	fun->module = module;
	fun->function = function;
	fun->arity = arity;
	fun->uniq = uniq;
	fun->index = index;
	fun->frozen = frozen;
	return tag_fun(fun);
}

term_t heap_long_pid(heap_t *hp, term_t node, int serial, int creation)
{
	long_id_t *pid;
	int gap = sizeof(*pid);
	pid = (long_id_t *)heap_alloc(hp, gap);
	pid->node = node;
	pid->serial = serial;
	pid->tag_creation = tag_pid(creation);
	return tag_long_id(pid);
}

term_t heap_long_oid(heap_t *hp, term_t node, int serial, int creation)
{
	long_id_t *oid;
	int gap = sizeof(*oid);
	oid = (long_id_t *)heap_alloc(hp, gap);
	oid->node = node;
	oid->serial = serial;
	oid->tag_creation = tag_oid(creation);
	return tag_long_id(oid);
}

term_t heap_long_id(heap_t *hp, term_t node, int serial, int tag_creat)
{
	long_id_t *id;
	int gap = sizeof(*id);
	id = (long_id_t *)heap_alloc(hp, gap);
	id->node = node;
	id->serial = serial;
	id->tag_creation = tag_creat;
	return tag_long_id(id);
}

term_t heap_tuple(heap_t *hp, int size)
{
	tuple_t *tuple;
	int gap = sizeof(*tuple) + size*sizeof(term_t);
	tuple = (tuple_t *)heap_alloc(hp, gap);
	tuple->size = size;
	//NB: elements are undefined
	return tag_tuple(tuple);
}

term_t heap_tuple1(heap_t *hp, term_t e1)
{
	tuple_t *tuple;
	int gap = sizeof(*tuple) + 1*sizeof(term_t);
	tuple = (tuple_t *)heap_alloc(hp, gap);
	tuple->size = 1;
	tuple->elts[0] = e1;
	return tag_tuple(tuple);
}

term_t heap_tuple2(heap_t *hp, term_t e1, term_t e2)
{
	tuple_t *tuple;
	int gap = sizeof(*tuple) + 2*sizeof(term_t);
	tuple = (tuple_t *)heap_alloc(hp, gap);
	tuple->size = 2;
	tuple->elts[0] = e1;
	tuple->elts[1] = e2;
	return tag_tuple(tuple);
}

term_t heap_tuple3(heap_t *hp, term_t e1, term_t e2, term_t e3)
{
	tuple_t *tuple;
	int gap = sizeof(*tuple) + 3*sizeof(term_t);
	tuple = (tuple_t *)heap_alloc(hp, gap);
	tuple->size = 3;
	tuple->elts[0] = e1;
	tuple->elts[1] = e2;
	tuple->elts[2] = e3;
	return tag_tuple(tuple);
}

term_t heap_tuple4(heap_t *hp, term_t e1, term_t e2, term_t e3, term_t e4)
{
	tuple_t *tuple;
	int gap = sizeof(*tuple) + 4*sizeof(term_t);
	tuple = (tuple_t *)heap_alloc(hp, gap);
	tuple->size = 4;
	tuple->elts[0] = e1;
	tuple->elts[1] = e2;
	tuple->elts[2] = e3;
	tuple->elts[3] = e4;
	return tag_tuple(tuple);
}

term_t heap_tuple5(heap_t *hp, term_t e1, term_t e2, term_t e3, term_t e4, term_t e5)
{
	tuple_t *tuple;
	int gap = sizeof(*tuple) + 5*sizeof(term_t);
	tuple = (tuple_t *)heap_alloc(hp, gap);
	tuple->size = 5;
	tuple->elts[0] = e1;
	tuple->elts[1] = e2;
	tuple->elts[2] = e3;
	tuple->elts[3] = e4;
	tuple->elts[4] = e5;
	return tag_tuple(tuple);
}

term_t heap_tuple6(heap_t *hp, term_t e1, term_t e2, term_t e3, term_t e4, term_t e5, term_t e6)
{
	tuple_t *tuple;
	int gap = sizeof(*tuple) + 6*sizeof(term_t);
	tuple = (tuple_t *)heap_alloc(hp, gap);
	tuple->size = 6;
	tuple->elts[0] = e1;
	tuple->elts[1] = e2;
	tuple->elts[2] = e3;
	tuple->elts[3] = e4;
	tuple->elts[4] = e5;
	tuple->elts[5] = e6;
	return tag_tuple(tuple);
}

term_t heap_tuple7(heap_t *hp, term_t e1, term_t e2, term_t e3, term_t e4, term_t e5, term_t e6, term_t e7)
{
	tuple_t *tuple;
	int gap = sizeof(*tuple) + 7*sizeof(term_t);
	tuple = (tuple_t *)heap_alloc(hp, gap);
	tuple->size = 7;
	tuple->elts[0] = e1;
	tuple->elts[1] = e2;
	tuple->elts[2] = e3;
	tuple->elts[3] = e4;
	tuple->elts[4] = e5;
	tuple->elts[5] = e6;
	tuple->elts[6] = e7;
	return tag_tuple(tuple);
}

term_t heap_tuple8(heap_t *hp, term_t e1, term_t e2, term_t e3, term_t e4, term_t e5, term_t e6, term_t e7, term_t e8)
{
	tuple_t *tuple;
	int gap = sizeof(*tuple) + 8*sizeof(term_t);
	tuple = (tuple_t *)heap_alloc(hp, gap);
	tuple->size = 8;
	tuple->elts[0] = e1;
	tuple->elts[1] = e2;
	tuple->elts[2] = e3;
	tuple->elts[3] = e4;
	tuple->elts[4] = e5;
	tuple->elts[5] = e6;
	tuple->elts[6] = e7;
	tuple->elts[7] = e8;
	return tag_tuple(tuple);
}

term_t heap_cons(heap_t *hp, term_t head)
{
	cons_t *cons;
	int gap = sizeof(*cons);
	cons = (cons_t *)heap_alloc(hp, gap);
	cons->head = head;
	cons->tail = nil;
	return tag_list(cons);
}

term_t heap_cons2(heap_t *hp, term_t head, term_t tail)
{
	cons_t *cons;
	int gap = sizeof(*cons);
	cons = (cons_t *)heap_alloc(hp, gap);
	cons->head = head;
	cons->tail = tail;
	return tag_list(cons);
}

//cons - tuple - binary - fun
term_t heap_marshal(term_t t, heap_t *hp)
{
	term_box_t *box;
	if (is_immed(t))
		return t;
	box = peel(t);

	if (is_cons(t))
	{
		term_t first = nil;
		term_t last = nil;

		do {
			term_box_t *cb = peel(t);
			term_t v = heap_marshal(cb->cons.head, hp);
			cons_up(first, last, v, hp);
			t = cb->cons.tail;
		} while (is_cons(t));
		
		if (t != nil)
			peel(last)->cons.tail = heap_marshal(t, hp);

		return first;
	}
	else if (is_tuple(t))
	{
		int n = box->tuple.size;
		term_t tuple = heap_tuple(hp, n);
		term_box_t *tb = peel(tuple);
		int i;
		for (i = 0; i < n; i++)
			tb->tuple.elts[i] = heap_marshal(box->tuple.elts[i], hp);

		return tuple;
	}
	else if (is_binary(t))
	{
		//NB: for shared binaries parent not copied; shared becomes root

		term_t binary = heap_binary(hp, box->binary.bit_size, box->binary.data);
		return binary;
	}
	else if (is_bignum(t))
	{
		bignum_t *bb = (bignum_t *)peel(t);
		term_t biggie = heap_bignum(hp, bb->sign, bb->used, bb->dp);
		return biggie;
	}
	else if (is_float(t))
	{
		term_t f = heap_float(hp, float_value(t));
		return f;
	}
	else if (is_fun(t))
	{
		term_t fun = heap_fun(hp,
			box->fun.module,
			box->fun.function,
			box->fun.arity,
			box->fun.uniq,
			box->fun.index,
			heap_marshal(box->fun.frozen, hp));
		return fun;
	}
	else // long_id
	{
		term_t id;
		assert(is_long_id(t));
		id = heap_long_id(hp,
			box->long_id.node,
			box->long_id.serial,
			box->long_id.tag_creation);
		return id;
	}
}

//EOF
