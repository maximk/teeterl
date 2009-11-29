#ifndef BIFIMPL_H
#define BIFIMPL_H

// the only include file needed for BIF implementation file

#include "teeterl.h"
#include "term.h"
#include "proc.h"
#include "atom.h"
#include "compare.h"
#include "list.h"
#include "mpi.h"

// TODO: rename bif_error -- confused with bif_error1

// TODO: Stack trace building
// TODO: Include stack trace into reason

#define bif_exception(__reason) \
	do { \
		proc->result.what = SLICE_RESULT_ERROR; \
		proc->result.reason = __reason; \
		return noval; \
	} while (0)

#define bif_bad_arg(__arg)		bif_exception(A_BADARG)
#define bif_bad_arg0()			bif_exception(A_BADARG)
#define bif_not_implemented()	bif_exception(A_NOT_IMPLEMENTED)

#endif
