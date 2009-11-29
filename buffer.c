//
//
//

#include "buffer.h"

struct buffer_t {
	apr_allocator_t *allocator;
	apr_memnode_t *node;
	int offset;
};

static void buffer_tidy(buffer_t *self)
{
	if (buffer_len(self) == 0)
		buffer_clear(self);
	else
	{
		int total = self->node->endp - (char *)self->node - APR_MEMNODE_T_SIZE;
		if (self->offset > (total + 1)/2)
		{
			memcpy((char *)self->node - APR_MEMNODE_T_SIZE, buffer_ptr(self), buffer_len(self));
			self->node->first_avail -= self->offset;
			self->offset = 0;
		}
	}
}

buffer_t *buffer_make(apr_pool_t *pool, int max)
{
	buffer_t *buf = apr_palloc(pool, sizeof(*buf));
	buf->allocator = apr_pool_allocator_get(pool);
	buf->node = apr_allocator_alloc(buf->allocator, max);
	buf->offset = 0;
	return buf;
}

void buffer_resize(buffer_t *self, int avail)
{
	int size, new_size;
	apr_memnode_t *new_node;

	if (buffer_available(self) >= avail)
		return;
	size = buffer_len(self);
	new_size = size + avail;
	new_node = apr_allocator_alloc(self->allocator, new_size);
	memcpy((char *)new_node + APR_MEMNODE_T_SIZE, buffer_ptr(self), size);
	apr_allocator_free(self->allocator, self->node);
	self->node = new_node;
	self->offset = 0;
}

void buffer_clear(buffer_t *self)
{
	self->node->first_avail = (char *)self->node + APR_MEMNODE_T_SIZE;
	self->offset = 0;
}

void buffer_consume(buffer_t *self, int size)
{
	self->offset += size;
	buffer_tidy(self);
}

int buffer_len(buffer_t *self)
{
	return self->node->first_avail - (char *)self->node - APR_MEMNODE_T_SIZE - self->offset;
}

apr_byte_t *buffer_ptr(buffer_t *self)
{
	return (char *)self->node + APR_MEMNODE_T_SIZE + self->offset;
}

int buffer_available(buffer_t *self)
{
	return (int)(self->node->endp - self->node->first_avail);
}

void buffer_put_byte(buffer_t *self, apr_byte_t b)
{
	*self->node->first_avail++ = b;
}

void buffer_put_data(buffer_t *self, apr_byte_t *data, int size)
{
	memcpy(self->node->first_avail, data, size);
	self->node->first_avail += size;
}

//apr_status_t buffer_file_read(buffer_t *self, apr_file_t *file, apr_size_t len);
//apr_status_t buffer_file_write(buffer_t *self, apr_file_t *file);

apr_status_t buffer_socket_recv(buffer_t *self, apr_socket_t *sock)
{
	apr_status_t rs;
	apr_size_t len = buffer_available(self);
	char *space = self->node->first_avail;
	rs = apr_socket_recv(sock, space, &len);
	if (rs != 0)
		return rs;
	self->node->first_avail += (int)len;
	return 0;
}

apr_status_t buffer_socket_send(buffer_t *self, apr_socket_t *sock)
{
	apr_status_t rs;
	apr_size_t len = buffer_len(self);
	rs = apr_socket_send(sock, buffer_ptr(self), &len);
	if (rs != 0)
		return rs;
	self->offset += (int)len;
	buffer_tidy(self);
	return 0;
}

//EOF
