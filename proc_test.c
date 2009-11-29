//
//
//

#include <apr_general.h>
#include <apr_strings.h>
#include <apr_file_io.h>

#include "navel.h"
#include "proc.h"
#include "code_base.h"
#include "bif.h"
#include "exterm.h"

const char *proc_test_run(navel_context_t *cont, apr_pool_t *pool);
const char *proc_test_load_sample_code(navel_context_t *cont, apr_pool_t *pool);

const char *proc_test_handler(char *request, navel_context_t *cont, apr_pool_t *rp)
{
	if (strcmp(request, "/run") == 0)
		return proc_test_run(cont, rp);
	else if (strcmp(request, "/load-sample-code") == 0)
		return proc_test_load_sample_code(cont, rp);
	else
		return apr_psprintf(rp, "Unknown proc_test request: %s", request);
}

const char *proc_test_run(navel_context_t *cont, apr_pool_t *pool)
{
	term_t retval;

	if (cont->module == 0)
		return "Sample code not loaded";

	retval = proc_main(cont->solitaire, cont->module, cont->module->code, 0, 0);

	cont->solitaire = 0;	// most probably, process was destroyed

	return apr_psprintf(pool, "proc_main returned: %s",
		term2html(retval, cont->atoms, pool));
}

const char *proc_test_load_sample_code(navel_context_t *cont, apr_pool_t *pool)
{
	const char *mod_name = "\06sample";
	const char *file_name = "test/sample.x";
	apr_file_t *file;
	apr_off_t off = 0;
	apr_size_t len;
	apr_byte_t *data;
	term_t bin, code;
	term_box_t *tbox;
	int retval;
	term_t mod;

	apr_file_open(&file, file_name, APR_READ, 0, pool);
	apr_file_seek(file, APR_END, &off);
	len = (apr_size_t)off;
	off = 0;
	apr_file_seek(file, APR_SET, &off);

	data = apr_palloc(pool, len);
	apr_file_read_full(file, data, len, NULL);
	apr_file_close(file);

	bin = heap_binary(cont->heap, len*8, data);
	code = binary_to_term(bin, cont->atoms, cont->heap);

	//-record(teeterl, {module,
	//	exports = [],
	//	funs = [],
	//	attrs = [],
	//	preloaded,
	//	misc = []
	//}).

	tbox = peel(code);
	retval = code_base_load(cont->code_base,
		tbox->tuple.elts[1],
		tbox->tuple.elts[2],
		tbox->tuple.elts[3],
		tbox->tuple.elts[4],
		tbox->tuple.elts[5],
		tbox->tuple.elts[6]);

	mod = tag_atom(atoms_set(cont->atoms, (cstr_t *)mod_name));
	cont->module = code_base_lookup(cont->code_base, mod);

	if (retval != 0)
		return "Unable to load sample code";
	else
		return "Sample code loaded successfully";
}

//const char *proc_test_load_sample_code(navel_context_t *cont, apr_pool_t *pool)
//{
//	const char *file_name = "test/sample.x";
//	char buf[256];
//	apr_array_header_t *code = apr_array_make(cont->pool, 256, sizeof(codel_t));
//
//	FILE *f = fopen(file_name, "r");
//	while (!feof(f))
//	{
//		if (fscanf(f, "%s", buf) == 1)
//		{
//			if (buf[0] == '@')
//				APR_ARRAY_PUSH(code, codel_t).l = (codel_t *)code->elts + atoi(buf+1);
//			else if (buf[0] == '&')
//			{
//				cstr_t *s = ztos(buf+1, cont->heap);
//				term_t a = tag_atom(atoms_set(cont->atoms, s));
//				APR_ARRAY_PUSH(code, codel_t).t = a;
//			}
//			else if (buf[0] == '*')
//			{
//				int bif_index = atoi(buf+1);
//				bifN_t entry = (bifN_t) builtins[bif_index].entry;
//				APR_ARRAY_PUSH(code, codel_t).bif = entry;
//			}
//			else
//				APR_ARRAY_PUSH(code, codel_t).i = atoi(buf);
//		}
//	}
//	fclose(f);
//	
//	cont->sample_code = (codel_t *)code->elts;
//	return apr_psprintf(pool, "Sample code loaded [%d codel(s)]", code->nelts);
//}

//EOF
