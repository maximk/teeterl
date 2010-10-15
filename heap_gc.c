//
//
//

#include "heap_gc.h"

#include "mpi.h"

// GC algorithm:
// Single run of gc cleares single memnode
// Memnode constitute a list with the active memnode
// in its head
// Active memnode is the node allocations are being
// currently made from
// There are no references from older memnode to new
// memnodes (including binary data references)
// When gc is first run on the heap it acts on its
// active node, the next run acts on the node after
// the active and so on
// The node gc is cleansing from dead wood is called
// gc node
// When gc was run on the last node than the process
// started from the beginning
// GC starts with a list of root terms
// For pointer terms its location is determined
// The location can be one of the nodes starting from
// active node and ending just before gc node
// Such terms can contain further references to live
// terms in gc node
// The location can also be in the gc node, then the term
// is recreated
// Any other location indicates that the term belongs
// to older nodes or literal pool of the module
// Such terms are ignored
// GC node's size is determined in the beginning of
// the run and either a new node is created which
// in the end substitutes the gc node or the node
// before gc node is used for allocations if it has
// enough space available
// The size of recreated terms should not exceed
// the size of gc node; the size may have increased
// when binaries with shared data are recreated; such
// shared data should be kept shared

/* copied from apr_pools.c */

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

/* Returns the amount of free space in the given node. */
#define node_free_space(node_) ((int)(node_->endp - node_->first_avail))

#define node_alloc_size(node_)	(node_->first_avail - (char *)node_ - APR_MEMNODE_T_SIZE)

/* slight optimization - apr_memnode_t structure not skipped */
#define node_contains(node_, ptr_)	((char *)ptr_ >= (char *)node_ && (char *)ptr_ < node_->endp)

void seek_live(term_t *tp, apr_memnode_t *newest, heap_t *hp);
#ifdef DEBUG
void update_alloc_size(int what, int size, void *ptr, void *cont);
#endif

apr_status_t validate_heap(heap_t *hp);

apr_status_t heap_gc(heap_t *hp, term_t *roots[], int root_sizes[], int nroots)
{
	apr_memnode_t *saved_active;
	apr_memnode_t *gc_node, *copy_node;
	int node_size;
	int i, j;

	if (hp->gc_last == NULL)	// gc never run
		gc_node = hp->active;
	else
		gc_node = hp->gc_last->next;

	node_size = node_alloc_size(gc_node);

	// if gc_last node has enough space then use it for
	// live term copies, otherwise, create a new node
	// NB: gc_last may point to gc_node

	if (hp->gc_last != NULL && hp->gc_last != gc_node && node_free_space(hp->gc_last) >= node_size)
		copy_node = hp->gc_last;
	else
		copy_node = apr_allocator_alloc(hp->allocator, node_size);

	// temporarily make copy_node active; restore later
	saved_active = hp->active;
	hp->active = copy_node;
	hp->hend = heap_htop(hp);

	// save gc_node reference for seek_alive;
	// non-NULL gc_spot means gc in progress
	hp->gc_spot = gc_node;

	for (i = 0; i < nroots; i++)
		for (j = 0; j < root_sizes[i]; j++)
			seek_live(&roots[i][j], saved_active, hp);

	assert(hp->active == copy_node); // no overflow

	hp->gc_spot = NULL;

	// restore active node
	if (saved_active != gc_node)
		hp->active = saved_active;

	// insert copy_node into the ring:
	// if gc_node is the last node left
	// if copy_node is non-empty and was just created;
	// free copy_node if it was just created
	// and not put on the list

	if (gc_node->next == gc_node ||
		(node_alloc_size(copy_node) > 0 && copy_node != hp->gc_last))
	{
		list_insert(copy_node, gc_node);
		hp->gc_last = copy_node;
	}
	else if (copy_node != hp->gc_last)
	{
		if (hp->active == copy_node)
			hp->active = gc_node->next;
		apr_allocator_free(hp->allocator, copy_node);
	}

	hp->alloc_size -= node_alloc_size(gc_node);

	// reclaim memory
	list_remove(gc_node);
	gc_node->next = NULL;
	apr_allocator_free(hp->allocator, gc_node);

	// after gc is run, anticipated need is zero
	hp->hend = heap_htop(hp);

	return APR_SUCCESS;
}

void seek_live(term_t *tp, apr_memnode_t *newest, heap_t *hp)
{
	term_t t = *tp;
	apr_memnode_t *node;
	term_box_t *ptr;

	// newest node - the node last generation the term may belong to
	// the node chain starts with the newest and goes to hp->gc_spot

	if (is_immed(t))
		return;
	ptr = peel(t);

	node = newest;
	while (node != hp->gc_spot)
	{
		if (node_contains(node, ptr))
		{
			// the term belongs to the newer generation
			// of terms; recurse to find possible references
			// to live terms in hp->gc_spot

			// only tuples, conses, funs (frozen)
			// and binaries (data, parent) contain references

			// order of popularity:
			// cons - tuple - binary - fun

			if (is_cons(t))
			{
				seek_live(&ptr->cons.head, node, hp);
				seek_live(&ptr->cons.tail, node, hp);
			}
			else if (is_tuple(t))
			{
				int i;
				int n = ptr->tuple.size;
				for (i = 0; i < n; i++)
					seek_live(&ptr->tuple.elts[i], node, hp);
			}
			else if (is_binary(t))
			{
				if (ptr->binary.parent != noval)
				{
					term_box_t *parent;
					seek_live(&ptr->binary.parent, node, hp);
					parent = peel(ptr->binary.parent);
					ptr->binary.data = parent->binary.data + ptr->binary.offset;
				}
			}
			else if (is_fun(t))
			{
				seek_live(&ptr->fun.frozen, node, hp);
			}

			return;
		}
		node = node->next;
	}

	if (node_contains(hp->gc_spot, ptr))
	{
		// the term should be recreated

		// the term may have already been moved
		// and the term value has been replaced with
		// the buried reference to the new location

		if (is_grave(t))
		{
			*tp = ptr->grave.skeleton;
			return;
		}

		// list - tuple - binary - fun - bignum - pid - float

		if (is_list(t))
		{
			term_t cons = heap_cons2(hp, ptr->cons.head, ptr->cons.tail);
			term_box_t *box = peel(cons);
			seek_live(&box->cons.head, hp->gc_spot, hp);
			seek_live(&box->cons.tail, hp->gc_spot, hp);
			*tp = cons;
		}
		else if (is_tuple(t))
		{
			term_t tuple = heap_tuple(hp, ptr->tuple.size);
			term_box_t *box = peel(tuple);
			int i;
			for (i = 0; i < ptr->tuple.size; i++)
			{
				box->tuple.elts[i] = ptr->tuple.elts[i];
				seek_live(&box->tuple.elts[i], hp->gc_spot, hp);
			}
			*tp = tuple;
		}
		else if (is_binary(t))
		{
			term_t parent = ptr->binary.parent;
			term_t b;
			if (parent == noval)
				b = heap_binary(hp, ptr->binary.bit_size, ptr->binary.data);
			else
			{
				apr_byte_t *data;
				seek_live(&parent, hp->gc_spot, hp);
				data = peel(parent)->binary.data + ptr->binary.offset;
				b = heap_binary_shared(hp, ptr->binary.bit_size, data, parent);
			}
			*tp = b;
		}
		else if (is_fun(t))
		{
			term_t f = heap_fun(hp,
				ptr->fun.module, ptr->fun.function, ptr->fun.arity,
				ptr->fun.uniq, ptr->fun.index, ptr->fun.frozen);
			seek_live(&peel(f)->fun.frozen, hp->gc_spot, hp);
			*tp = f;
		}
		else if (is_bignum(t))
		{
			mp_int ma = bignum_to_mp(t);
			*tp = heap_bignum(hp, SIGN(&ma), USED(&ma), DIGITS(&ma));
		}
		else if (is_long_id(t))
		{
			*tp = heap_long_id(hp,
				ptr->long_id.node,
				ptr->long_id.serial,
				ptr->long_id.tag_creation);
		}
		else	// if (is_float(t))
		{
			assert(is_float(t));
			*tp = heap_float(hp, float_value(t));
		}

		// bury the term
		ptr->grave.cross = MAGIC_CROSS;
		ptr->grave.skeleton = *tp;

		return;
	}
	else
	{
		// the term belong to the older generation or
		// to the literal pool of the module -- ignore

		return;
	}
}

apr_status_t validate_heap(heap_t *hp)
{
	apr_memnode_t *node = hp->active;
	do {
		node = node->next;
	} while (node != 0 && node != hp->active);
	return (node != 0) ?0 :1;
}

//EOF
