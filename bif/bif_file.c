/*
* Copyright (c) 2009, Maxim Kharchenko
* All rights reserved.
*
* Redistribution and use in source and binary forms, with or without
* modification, are permitted provided that the following conditions are met:
*     * Redistributions of source code must retain the above copyright
*       notice, this list of conditions and the following disclaimer.
*     * Redistributions in binary form must reproduce the above copyright
*       notice, this list of conditions and the following disclaimer in the
*       documentation and/or other materials provided with the distribution.
*     * Neither the name of the author nor the names of his contributors
*	    may be used to endorse or promote products derived from this software
*		without specific prior written permission.
*
* THIS SOFTWARE IS PROVIDED BY Maxim Kharchenko ''AS IS'' AND ANY
* EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
* WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
* DISCLAIMED. IN NO EVENT SHALL Maxim Kharchenko BE LIABLE FOR ANY
* DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
* (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
* LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
* ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
* (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
* SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

#include "bifimpl.h"

#include <apr_file_io.h>

#include "outlet.h"
#include "outlet_mall.h"
#include "binary.h"

#define MAX_READ_LEN	8192

term_t bif_open0_3(term_t FileName, term_t Mode, term_t Perms, proc_t *proc)
{
	apr_status_t rs;
	const char *file_name;
	apr_pool_t *ol;
	apr_file_t *file;
	outlet_t *outlet;

	if (!is_binary(FileName) || !is_int(Mode) || !is_int(Perms))
		bif_bad_arg0();

	apr_pool_create(&ol, 0);
	file_name = peel(FileName)->binary.data;
	rs = apr_file_open(&file, file_name, (apr_uint32_t)int_value(Mode), (apr_uint32_t)int_value(Perms), ol);
	if (rs != 0)
	{
		apr_pool_destroy(ol);
		bif_exception(decipher_status(rs));
	}

	outlet = ol_file_make(file, proc->teevm);

	//put outlet to polling ring
	outlet_mall_allot(proc->teevm->mall, outlet);

	return outlet_id(outlet);
}

term_t bif_read0_2(term_t File, term_t Len, proc_t *proc)
{
	apr_status_t rs;
	apr_size_t size;
	outlet_t *ol;
	apr_byte_t buf[MAX_READ_LEN];

	if (!is_short_oid(File))
		bif_bad_arg(File);
	if (!is_int(Len) || int_value(Len) > MAX_READ_LEN)
		bif_bad_arg(Len);

	ol = outlet_mall_lookup(proc->teevm->mall, oid_serial(File));
	if (ol == 0)
		bif_bad_arg(File);

	size = (apr_size_t)int_value(Len);
	rs = outlet_read(ol, buf, &size);
	if (size == 0 && APR_STATUS_IS_EOF(rs))
		return A_EOF;
	if (size == 0 && rs != 0)
		bif_exception(decipher_status(rs));

	return heap_binary(proc->heap, size*8, buf);
}

term_t bif_write0_2(term_t File, term_t Bin, proc_t *proc)
{
	apr_status_t rs;
	apr_size_t size;
	outlet_t *ol;
	term_box_t *bb;
	if (!is_short_oid(File) || !is_binary(Bin))
		bif_bad_arg0();
	ol = outlet_mall_lookup(proc->teevm->mall, oid_serial(File));
	if (ol == 0)
		bif_bad_arg(File);

	bb = peel(Bin);
	if (BIN_TRAILER_SIZE(bb->binary.bit_size) > 0)
		bif_bad_arg(Bin);

	size = (apr_size_t)BIN_BYTE_SIZE(bb->binary.bit_size);
	rs = outlet_write(ol, bb->binary.data, &size);
	if (rs != 0)
		bif_exception(decipher_status(rs)); //TODO: something may still be written

	return tag_int(size);
}

//EOF
