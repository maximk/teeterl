//
//
//

#include <apr_general.h>
#include <apr_file_io.h>
#include <apr_strings.h>

#include "navel.h"
#include "bif.h"

const char *exterm_test_sample(navel_context_t *cont, apr_pool_t *pool);

const char *exterm_test_handler(char *request, navel_context_t *cont, apr_pool_t *rp)
{
	if (strcmp(request, "/sample") == 0)
		return exterm_test_sample(cont, rp);
	else
		return apr_psprintf(rp, "Unknown exterm_test request: %s", request);
}

const char *exterm_test_sample(navel_context_t *cont, apr_pool_t *pool)
{
	const char *file_name = "test/sample.x";
	apr_file_t *bin_file;
	apr_off_t off;
	int len;
	apr_byte_t *data;
	term_t bin, exploded;

	if (cont->solitaire == 0)
		return "No spawned process to use";

	apr_file_open(&bin_file, file_name, APR_READ, 0, pool);
	off = 0;
	apr_file_seek(bin_file, APR_END, &off);
	len = (int)off;
	off = 0;
	apr_file_seek(bin_file, APR_SET, &off);
	data = apr_palloc(pool, len);
	apr_file_read(bin_file, data, &len);
	apr_file_close(bin_file);

	bin = heap_binary(cont->heap, 8*len, data);
	exploded = bif_binary_to_term1(bin, cont->solitaire);

	return apr_psprintf(pool, "%s", term2html(exploded, cont->atoms, pool));
}

//EOF
