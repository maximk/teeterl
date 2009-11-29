#ifndef BUFFER_H
#define BUFFER_H

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

#include <apr_general.h>
#include <apr_file_io.h>
#include <apr_network_io.h>

#include "xpool.h"

typedef struct buffer_t buffer_t;

buffer_t *buffer_make(xpool_t *xp, int max);
void buffer_resize(buffer_t *self, int avail, xpool_t *xp);
void buffer_clear(buffer_t *self);
void buffer_consume(buffer_t *self, int size);

int buffer_len(buffer_t *self);
apr_byte_t *buffer_ptr(buffer_t *self);
int buffer_available(buffer_t *self);

apr_uint32_t buffer_get32(buffer_t *self);
apr_uint16_t buffer_get16(buffer_t *self);
apr_byte_t buffer_get(buffer_t *self);

void buffer_put_byte(buffer_t *self, apr_byte_t b);
void buffer_put_data(buffer_t *self, apr_byte_t *data, int size);

apr_status_t buffer_file_read(buffer_t *self, apr_file_t *file, apr_size_t len);
apr_status_t buffer_file_write(buffer_t *self, apr_file_t *file);
apr_status_t buffer_socket_recv(buffer_t *self, apr_socket_t *sock);
apr_status_t buffer_socket_send(buffer_t *self, apr_socket_t *sock);

#endif
