//
//
//

#include "atom_defs.h"
#include "atom.h"
#include "named_tuple.h"
#include "heap.h"
#include "code_base.h"
#include "outlet_mall.h"
#include "scheduler.h"
#include "teevm.h"
#include "modbin.h"
#include "exterm.h"
#include "list.h"

extern modbin_t *modbins[];

void preload_module(code_base_t *base,
	cstr_t *name, atoms_t *atoms, named_tuples_t *nm_tuples, heap_t *hp);
term_t prepare_args(int argc, const char *argv[], heap_t *hp);

int main(int argc, const char *argv[])
{
	apr_pool_t *super_pool;
	atoms_t *atoms;
	named_tuples_t *nm_tuples;
	code_base_t *base;
	outlet_mall_t *mall;
	teevm_t *teevm;
	proc_t *first_born;
	module_t *init_module;
	codel_t *entry;
	term_t args;

	apr_pool_t *tmp;
	heap_t *hp;

	apr_initialize();
	atexit(apr_terminate);

	apr_pool_create(&super_pool, 0);
	atoms = atoms_make(super_pool);
	nm_tuples = named_tuples_make(super_pool);
	base = code_base_make(super_pool);
	mall = outlet_mall_make(super_pool);

	// preload some: init, error_handler, navel, prim_erlang, etc
	apr_pool_create(&tmp, super_pool);
	hp = heap_make(tmp);
	//preload_module(base, (ctsr_t *)"\013prim_erlang", atoms, hp);
	preload_module(base, (cstr_t *)"\015error_handler", atoms, nm_tuples, hp);
	preload_module(base, (cstr_t *)"\04init", atoms, nm_tuples, hp);
	apr_pool_destroy(tmp);

	teevm = apr_palloc(super_pool, sizeof(*teevm));
	teevm->atoms = atoms;
	teevm->nm_tuples = nm_tuples;
	teevm->base = base;
	teevm->scheduler = scheduler_make(mall, super_pool);
	teevm->mall = mall;

	first_born = proc_make(teevm);
	// scheduler_enlist0 does not put the process on the runnable queue
	scheduler_enlist0(teevm->scheduler, first_born);

	args = prepare_args(argc, argv, first_born->heap);

	init_module = code_base_lookup(base, A_INIT);
	entry = module_lookup(init_module, A_MAIN, 1);
	proc_main(first_born, init_module, entry, 1, &args);

	//NEVER REACHED

	return 0;
}

void preload_module(code_base_t *base,
	cstr_t *name, atoms_t *atoms, named_tuples_t *nm_tuples, heap_t *hp)
{
	modbin_t **mb = modbins;
	while (*mb)
	{
		if (scomp((*mb)->name, name))
		{
			term_t bin = heap_binary(hp, (*mb)->size*8, (*mb)->data);	//TODO: copies data
			term_t code = binary_to_term(bin, atoms, hp);
			term_box_t *tb = peel(code);
			code_base_load(base, nm_tuples,
				tb->tuple.elts[1],
				tb->tuple.elts[2],
				tb->tuple.elts[3],
				tb->tuple.elts[4],
				tb->tuple.elts[5],
				tb->tuple.elts[6]);
			(*mb)->is_preloaded = 1;
			break;
		}
		mb++;
	}
}

term_t prepare_args(int argc, const char *argv[], heap_t *hp)
{
	int i;
	term_t first = nil;
	term_t last = nil;
	for (i = 1; i < argc; i++)
	{
		term_t v = ztol(argv[i], hp);
		cons_up(first, last, v, hp);
	}
	return first;
}

//convert APR status into an atom
term_t decipher_status(apr_status_t rs)
{
	if (APR_STATUS_IS_ANONYMOUS(rs))
		return A_ANONYMOUS;
	if (APR_STATUS_IS_BADARG(rs))
		return A_BADARG;
	if (APR_STATUS_IS_BADCH(rs))
		return A_BADCH;
	if (APR_STATUS_IS_CHILD_DONE(rs))
		return A_CHILD_DONE;
	if (APR_STATUS_IS_CHILD_NOTDONE(rs))
		return A_CHILD_NOTDONE;
	if (APR_STATUS_IS_DETACH(rs))
		return A_DETACH;
	if (APR_STATUS_IS_EABOVEROOT(rs))
		return A_EABOVEROOT;
	if (APR_STATUS_IS_EABSOLUTE(rs))
		return A_EABSOLUTE;
	if (APR_STATUS_IS_EACCES(rs))
		return A_EACCES;
	if (APR_STATUS_IS_EAGAIN(rs))
		return A_EAGAIN;
	if (APR_STATUS_IS_EBADDATE(rs))
		return A_EBADDATE;
	if (APR_STATUS_IS_EBADF(rs))
		return A_EBADF;
	if (APR_STATUS_IS_EBADIP(rs))
		return A_EBADIP;
	if (APR_STATUS_IS_EBADMASK(rs))
		return A_EBADMASK;
	if (APR_STATUS_IS_EBADPATH(rs))
		return A_EBADPATH;
	if (APR_STATUS_IS_EBUSY(rs))
		return A_EBUSY;
	if (APR_STATUS_IS_ECONNABORTED(rs))
		return A_ECONNABORTED;
	if (APR_STATUS_IS_ECONNREFUSED(rs))
		return A_ECONNREFUSED;
	if (APR_STATUS_IS_ECONNRESET(rs))
		return A_ECONNRESET;
	if (APR_STATUS_IS_EDSOOPEN(rs))
		return A_EDSOOPEN;
	if (APR_STATUS_IS_EEXIST(rs))
		return A_EEXIST;
	if (APR_STATUS_IS_EFTYPE(rs))
		return A_EFTYPE;
	if (APR_STATUS_IS_EGENERAL(rs))
		return A_EGENERAL;
	if (APR_STATUS_IS_EHOSTUNREACH(rs))
		return A_EHOSTUNREACH;
	if (APR_STATUS_IS_EINCOMPLETE(rs))
		return A_EINCOMPLETE;
	if (APR_STATUS_IS_EINIT(rs))
		return A_EINIT;
	if (APR_STATUS_IS_EINPROGRESS(rs))
		return A_EINPROGRESS;
	if (APR_STATUS_IS_EINTR(rs))
		return A_EINTR;
	if (APR_STATUS_IS_EINVAL(rs))
		return A_EINVAL;
	if (APR_STATUS_IS_EINVALSOCK(rs))
		return A_EINVALSOCK;
	if (APR_STATUS_IS_EMFILE(rs))
		return A_EMFILE;
	if (APR_STATUS_IS_EMISMATCH(rs))
		return A_EMISMATCH;
	if (APR_STATUS_IS_ENAMETOOLONG(rs))
		return A_ENAMETOOLONG;
	if (APR_STATUS_IS_ENETUNREACH(rs))
		return A_ENETUNREACH;
	if (APR_STATUS_IS_ENFILE(rs))
		return A_ENFILE;
	if (APR_STATUS_IS_ENODIR(rs))
		return A_ENODIR;
	if (APR_STATUS_IS_ENOENT(rs))
		return A_ENOENT;
	if (APR_STATUS_IS_ENOLOCK(rs))
		return A_ENOLOCK;
	if (APR_STATUS_IS_ENOMEM(rs))
		return A_ENOMEM;
	if (APR_STATUS_IS_ENOPOLL(rs))
		return A_ENOPOLL;
	if (APR_STATUS_IS_ENOPROC(rs))
		return A_ENOPROC;
	if (APR_STATUS_IS_ENOSHMAVAIL(rs))
		return A_ENOSHMAVAIL;
	if (APR_STATUS_IS_ENOSOCKET(rs))
		return A_ENOSOCKET;
	if (APR_STATUS_IS_ENOSPC(rs))
		return A_ENOSPC;
	if (APR_STATUS_IS_ENOSTAT(rs))
		return A_ENOSTAT;
	if (APR_STATUS_IS_ENOTDIR(rs))
		return A_ENOTDIR;
	if (APR_STATUS_IS_ENOTEMPTY(rs))
		return A_ENOTEMPTY;
	if (APR_STATUS_IS_ENOTENOUGHENTROPY(rs))
		return A_ENOTENOUGHENTROPY;
	if (APR_STATUS_IS_ENOTHDKEY(rs))
		return A_ENOTHDKEY;
	if (APR_STATUS_IS_ENOTHREAD(rs))
		return A_ENOTHREAD;
	if (APR_STATUS_IS_ENOTIME(rs))
		return A_ENOTIME;
	if (APR_STATUS_IS_ENOTIMPL(rs))
		return A_ENOTIMPL;
	if (APR_STATUS_IS_ENOTSOCK(rs))
		return A_ENOTSOCK;
	if (APR_STATUS_IS_EOF(rs))
		return A_EOF;
	if (APR_STATUS_IS_EPATHWILD(rs))
		return A_EPATHWILD;
	if (APR_STATUS_IS_EPIPE(rs))
		return A_EPIPE;
	if (APR_STATUS_IS_EPROC_UNKNOWN(rs))
		return A_EPROC_UNKNOWN;
	if (APR_STATUS_IS_ERELATIVE(rs))
		return A_ERELATIVE;
	if (APR_STATUS_IS_ESPIPE(rs))
		return A_ESPIPE;
	if (APR_STATUS_IS_ESYMNOTFOUND(rs))
		return A_ESYMNOTFOUND;
	if (APR_STATUS_IS_ETIMEDOUT(rs))
		return A_ETIMEDOUT;
	if (APR_STATUS_IS_EXDEV(rs))
		return A_EXDEV;
	if (APR_STATUS_IS_FILEBASED(rs))
		return A_FILEBASED;
	if (APR_STATUS_IS_INCHILD(rs))
		return A_INCHILD;
	if (APR_STATUS_IS_INCOMPLETE(rs))
		return A_INCOMPLETE;
	if (APR_STATUS_IS_INPARENT(rs))
		return A_INPARENT;
	if (APR_STATUS_IS_KEYBASED(rs))
		return A_KEYBASED;
	if (APR_STATUS_IS_NOTDETACH(rs))
		return A_NOTDETACH;
	if (APR_STATUS_IS_NOTFOUND(rs))
		return A_NOTFOUND;
	if (APR_STATUS_IS_TIMEUP(rs))
		return A_TIMEUP;
	return A_UNKNOWN;
}

//EOF
