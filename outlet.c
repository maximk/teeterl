//
//
//

#include "outlet.h"

term_t outlet_id(outlet_t *self)
{
	return tag_short_oid(self->serial);
}

int outlet_want_read(outlet_t *self)
{
	if (self->want_read == 0)
		return 0;
	return self->want_read(self);
}

int outlet_want_write(outlet_t *self)
{
	if (self->want_write == 0)
		return 0;
	return self->want_write(self);
}

int outlet_is_socket(outlet_t *self)
{
	if (self->is_socket == 0)
		return 0;
	return self->is_socket(self);
}

apr_socket_t *outlet_get_socket(outlet_t *self)
{
	if (self->get_socket == 0)
		return 0;
	return self->get_socket(self);
}

int outlet_is_file(outlet_t *self)
{
	if (self->is_file == 0)
		return 0;
	return self->is_file(self);
}

apr_file_t *outlet_get_file(outlet_t *self)
{
	if (self->get_file == 0)
		return 0;
	return self->get_file(self);
}

apr_status_t outlet_try_read(outlet_t *self, apr_interval_time_t tick)
{
	if (self->try_read == 0)
		return APR_EOF;
	return self->try_read(self, tick);
}

apr_status_t outlet_try_write(outlet_t *self, apr_interval_time_t tick)
{
	if (self->try_write == 0)
		return APR_EOF;
	return self->try_write(self, tick);
}

apr_status_t outlet_do_readable(outlet_t *self)
{
	if (self->do_readable == 0)
		return APR_ENOTIMPL;
	return self->do_readable(self);
}

apr_status_t outlet_do_writable(outlet_t *self)
{
	if (self->do_writable == 0)
		return APR_ENOTIMPL;
	return self->do_writable(self);
}

apr_status_t outlet_send(outlet_t *self, term_t io)
{
	if (self->send == 0)
		return APR_ENOTIMPL;
	return self->send(self, io);
}

apr_status_t outlet_set_option(outlet_t *self, term_t opt, term_t value)
{
	if (self->set_option == 0)
		return APR_ENOTIMPL;
	return self->set_option(self, opt, value);
}

apr_status_t outlet_read(outlet_t *self, apr_byte_t *buf, apr_size_t *len)
{
	if (self->read == 0)
		return APR_ENOTIMPL;
	return self->read(self, buf, len);
}

apr_status_t outlet_write(outlet_t *self, apr_byte_t *buf, apr_size_t *len)
{
	if (self->write == 0)
		return APR_ENOTIMPL;
	return self->write(self, buf, len);
}

apr_status_t outlet_close(outlet_t *self)
{
	if (self->close == 0)
		return APR_ENOTIMPL;
	return self->close(self);
}

apr_status_t outlet_close0(outlet_t *self) 	//error, no lingering
{
	if (self->close0 == 0)
		return APR_ENOTIMPL;
	return self->close0(self);
}

//EOF
