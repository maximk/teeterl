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

#include "outlet.h"

#include "proc.h"
#include "atom.h"
#include "outlet_mall.h"

typedef struct ol_file_data_t ol_file_data_t;
struct ol_file_data_t {
	apr_file_t *io;
	teevm_t *teevm;
};

int ol_file_is_file(outlet_t *self);
apr_file_t *ol_file_get_file(outlet_t *self);

apr_status_t ol_file_read(outlet_t *self, apr_byte_t *buf, apr_size_t *len);
apr_status_t ol_file_write(outlet_t *self, apr_byte_t *buf, apr_size_t *len);

apr_status_t ol_file_set_option(outlet_t *self, term_t opt, term_t value);
apr_status_t ol_file_close(outlet_t *self);
apr_status_t ol_file_close0(outlet_t *self);

outlet_t *ol_file_make(apr_file_t *io, teevm_t *teevm)
{
	ol_file_data_t *data;
	apr_pool_t *pool = apr_file_pool_get(io);

	outlet_t *outlet = apr_palloc(pool, sizeof(outlet_t));
	outlet->serial = (uint)-1;
	outlet->owner_in = outlet->owner_out = 0;
	outlet->pool = pool;

	outlet->want_read = 0;
	outlet->want_write = 0;
	outlet->is_socket = 0;
	outlet->get_socket = 0;
	outlet->is_file = ol_file_is_file;
	outlet->get_file = ol_file_get_file;

	outlet->try_read = 0;
	outlet->try_write = 0;

	outlet->do_readable = 0;
	outlet->do_writable = 0;
	outlet->send = 0;
	outlet->set_option = ol_file_set_option;

	outlet->read = ol_file_read;
	outlet->write = ol_file_write;

	outlet->close = ol_file_close;
	outlet->close0 = ol_file_close0;

	data = apr_palloc(pool, sizeof(ol_file_data_t));
	data->io = io;
	data->teevm = teevm;

	outlet->data = data;
	return outlet;
}

int ol_file_is_file(outlet_t *self)
{
	return 1;
}

apr_file_t *ol_file_get_file(outlet_t *self)
{
	ol_file_data_t *data = self->data;
	return data->io;
}

apr_status_t ol_file_read(outlet_t *self, apr_byte_t *buf, apr_size_t *len)
{
	ol_file_data_t *data = self->data;
	return apr_file_read(data->io, buf, len);
}

apr_status_t ol_file_write(outlet_t *self, apr_byte_t *buf, apr_size_t *len)
{
	ol_file_data_t *data = self->data;
	return apr_file_write(data->io, buf, len);
}

apr_status_t ol_file_set_option(outlet_t *self, term_t opt, term_t value)
{
	return APR_BADARG;
}

apr_status_t ol_file_close(outlet_t *self)
{
	ol_file_data_t *data = self->data;
	ol_file_close0(self);
	outlet_mall_kick_out(data->teevm->mall, self);
	apr_pool_destroy(self->pool);
	return APR_SUCCESS;
}

apr_status_t ol_file_close0(outlet_t *self)
{
	ol_file_data_t *data = self->data;
	return apr_file_close(data->io);
}

//EOF
