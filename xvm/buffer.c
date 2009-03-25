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

#include "buffer.h"

#include "getput.h"

struct buffer_t
{
	apr_byte_t *data;
	int max;
	int size;
	int off;
};

static void buffer_tidy(buffer_t *self);

buffer_t *buffer_make(xpool_t *xp, int max)
{
	buffer_t *buf = xalloc(xp, sizeof(buffer_t));
	buf->data = xalloc(xp, max);
	buf->max = max;
	buf->size = 0;
	buf->off = 0;
	return buf;
}

void buffer_resize(buffer_t *self, int avail, xpool_t *xp)
{
	int new_size;
	apr_byte_t *new_data;

	if (buffer_available(self) >= avail)
		return;
	
	new_size = self->size + avail;
	new_data = xalloc(xp, new_size);
	memcpy(new_data, self->data + self->off, self->size);
	self->data = new_data;
	self->max = new_size;
	self->off = 0;
	//self->size stays the same
}

void buffer_clear(buffer_t *self)
{
	self->size = 0;
	self->off = 0;
}

void buffer_consume(buffer_t *self, int size)
{
	self->size -= size;
	self->off += size;
	buffer_tidy(self);
}

int buffer_len(buffer_t *self)
{
	return self->size;
}

apr_byte_t *buffer_ptr(buffer_t *self)
{
	return self->data + self->off;
}

int buffer_available(buffer_t *self)
{
	return self->max - self->off - self->size;
}

static void buffer_tidy(buffer_t *self)
{
	if (self->size == 0)
		self->off = 0;
	else if (self->off > (self->max+1)/2)
	{
		memcpy(self->data, self->data + self->off, self->size);
		self->off = 0;
	}
}

apr_uint32_t buffer_get32(buffer_t *self)
{
	apr_uint32_t v = GET32(self->data + self->off);
	self->off += 4;
	self->size -= 4;
	buffer_tidy(self);
	return v;
}

apr_uint16_t buffer_get16(buffer_t *self)
{
	apr_uint16_t v = GET16(self->data + self->off);
	self->off += 2;
	self->size -= 2;
	buffer_tidy(self);
	return v;
}

apr_byte_t buffer_get(buffer_t *self)
{
	apr_byte_t v = self->data[self->off];
	self->off++;
	self->size--;
	buffer_tidy(self);
	return v;
}

void buffer_put_byte(buffer_t *self, apr_byte_t b)
{
	self->data[self->off + self->size] = b;
	self->size++;
}

void buffer_put_data(buffer_t *self, apr_byte_t *data, int size)
{
	if (size > buffer_available(self))
		size = buffer_available(self);
	memcpy(self->data + self->off + self->size, data, size);
	self->size += size;
}

apr_status_t buffer_file_read(buffer_t *self, apr_file_t *file, apr_size_t len)
{
	apr_status_t rs;
	apr_byte_t *space = self->data + self->off + self->size;
	rs = apr_file_read(file, space, &len);
	if (rs != 0)
		return rs;
	if (len == 0)
		return EAGAIN; //to avoid empty message from port
	self->size += (int)len;
	return 0;
}

apr_status_t buffer_file_write(buffer_t *self, apr_file_t *file)
{
	apr_status_t rs;
	apr_size_t len = self->size;
	rs = apr_file_write(file, self->data + self->off, &len);
	if (rs != 0)
		return rs;
	self->off += (int)len;
	self->size -= (int)len;
	buffer_tidy(self);
	return 0;
}

apr_status_t buffer_socket_recv(buffer_t *self, apr_socket_t *sock)
{
	apr_status_t rs;
	apr_size_t len = buffer_available(self);
	apr_byte_t *space = self->data + self->off + self->size;
	rs = apr_socket_recv(sock, (char *)space, &len);
	if (rs != 0)
		return rs;
	self->size += (int)len;
	return 0;
}

apr_status_t buffer_socket_send(buffer_t *self, apr_socket_t *sock)
{
	apr_status_t rs;
	apr_size_t len = self->size;
	rs = apr_socket_send(sock, (char *)self->data + self->off, &len);
	if (rs != 0)
		return rs;
	self->off += (int)len;
	self->size -= (int)len;
	buffer_tidy(self);
	return 0;
}

//EOF
