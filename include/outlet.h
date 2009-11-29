#ifndef OUTLET_H
#define OUTLET_H

#include <apr_network_io.h>

#include "proc.h"
#include "term.h"

typedef struct outlet_t outlet_t;
struct outlet_t {
	uint serial;
	apr_pool_t *pool;
	proc_t *owner_in;
	proc_t *owner_out;
	void *data;

	int (*want_read)(outlet_t *self);
	int (*want_write)(outlet_t *self);

	int (*is_socket)(outlet_t *self);
	apr_socket_t *(*get_socket)(outlet_t *self);
	int (*is_file)(outlet_t *self);
	apr_file_t *(*get_file)(outlet_t *self);

	apr_status_t (*try_read)(outlet_t *self, apr_interval_time_t tick);
	apr_status_t (*try_write)(outlet_t *self, apr_interval_time_t tick);

	apr_status_t (*do_readable)(outlet_t *self);
	apr_status_t (*do_writable)(outlet_t *self);

	apr_status_t (*send)(outlet_t *self, term_t io);
	apr_status_t (*set_option)(outlet_t *self, term_t opt, term_t value);

	apr_status_t (*read)(outlet_t *self, apr_byte_t *buf, apr_size_t *len);
	apr_status_t (*write)(outlet_t *self, apr_byte_t *buf, apr_size_t *len);

	apr_status_t (*close)(outlet_t *self);
	apr_status_t (*close0)(outlet_t *self); 	//error, no lingering
};

term_t outlet_id(outlet_t *self);

outlet_t *ol_socket_make(apr_socket_t *sock, int is_connecting);
outlet_t *ol_listener_make(apr_socket_t *sock, teevm_t *teevm);
outlet_t *ol_file_make(apr_file_t *io, teevm_t *teevm);

int outlet_want_read(outlet_t *self);
int outlet_want_write(outlet_t *self);

int outlet_is_socket(outlet_t *self);
apr_socket_t *outlet_get_socket(outlet_t *self);
int outlet_is_file(outlet_t *self);
apr_file_t *outlet_get_file(outlet_t *self);

apr_status_t outlet_try_read(outlet_t *self, apr_interval_time_t tick);
apr_status_t outlet_try_write(outlet_t *self, apr_interval_time_t tick);

apr_status_t outlet_do_readable(outlet_t *self);
apr_status_t outlet_do_writable(outlet_t *self);

apr_status_t outlet_send(outlet_t *self, term_t io);
apr_status_t outlet_set_option(outlet_t *self, term_t opt, term_t value);

apr_status_t outlet_read(outlet_t *self, apr_byte_t *buf, apr_size_t *len);
apr_status_t outlet_write(outlet_t *self, apr_byte_t *buf, apr_size_t *len);

apr_status_t outlet_close(outlet_t *self);
apr_status_t outlet_close0(outlet_t *self); 	//error, no lingering

#endif
