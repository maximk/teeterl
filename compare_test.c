//
//
//

#include <apr_tables.h>
#include <apr_strings.h>

#include "navel.h"
#include "compare.h"

const char *compare_test_add_samples(navel_context_t *cont, apr_pool_t *rp);
const char *compare_test_show_samples(navel_context_t *cont, apr_pool_t *rp);
const char *compare_test_do_some(const char *req, navel_context_t *cont, apr_pool_t *pool);

const char *compare_test_handler(char *request, navel_context_t *cont, apr_pool_t *rp)
{
	if (strcmp(request, "/add-samples") == 0)
		return compare_test_add_samples(cont, rp);
	else if (strcmp(request, "/show-samples") == 0)
		return compare_test_show_samples(cont, rp);
	else if (strncmp(request, "/values/", 8) == 0)
		return compare_test_do_some(request, cont, rp);
	else
		return "Unknown compare_test request";
}

const char *compare_test_add_samples(navel_context_t *cont, apr_pool_t *rp)
{
	mp_digit digs1[] = {1, 2, 3};
	mp_digit digs2[] = {8, 3, 7};
	apr_byte_t data1[] = {255};
	apr_byte_t data2[] = {120, 255};

	APR_ARRAY_PUSH(cont->sample_terms, term_t) = tag_int(0);
	APR_ARRAY_PUSH(cont->sample_terms, term_t) = tag_int(7);
	APR_ARRAY_PUSH(cont->sample_terms, term_t) = tag_int(-3);
	APR_ARRAY_PUSH(cont->sample_terms, term_t) = tag_int(MAX_INT_VALUE);
	APR_ARRAY_PUSH(cont->sample_terms, term_t) = tag_int(MIN_INT_VALUE);
	APR_ARRAY_PUSH(cont->sample_terms, term_t) = A_BADARG;
	APR_ARRAY_PUSH(cont->sample_terms, term_t) = A_TRUE;
	APR_ARRAY_PUSH(cont->sample_terms, term_t) = A_FALSE;
	APR_ARRAY_PUSH(cont->sample_terms, term_t) = tag_short_pid(100);
	APR_ARRAY_PUSH(cont->sample_terms, term_t) = tag_short_pid(101);
	APR_ARRAY_PUSH(cont->sample_terms, term_t) = nil;
	APR_ARRAY_PUSH(cont->sample_terms, term_t) = heap_float(cont->heap, 0.0);
	APR_ARRAY_PUSH(cont->sample_terms, term_t) = heap_float(cont->heap, 7.0);
	APR_ARRAY_PUSH(cont->sample_terms, term_t) = heap_float(cont->heap, -3.0);
	APR_ARRAY_PUSH(cont->sample_terms, term_t) = heap_float(cont->heap, -1.0);
	APR_ARRAY_PUSH(cont->sample_terms, term_t) = heap_float(cont->heap, -7.0);
	APR_ARRAY_PUSH(cont->sample_terms, term_t) = heap_float(cont->heap, 5.0);
	APR_ARRAY_PUSH(cont->sample_terms, term_t) = heap_float(cont->heap, 10.0);
	APR_ARRAY_PUSH(cont->sample_terms, term_t) = heap_float(cont->heap, -1.0);
	APR_ARRAY_PUSH(cont->sample_terms, term_t) = heap_float(cont->heap, 10.0);
	APR_ARRAY_PUSH(cont->sample_terms, term_t) = heap_bignum(cont->heap, 1, 3, digs1);
	APR_ARRAY_PUSH(cont->sample_terms, term_t) = heap_bignum(cont->heap, 0, 3, digs1);
	APR_ARRAY_PUSH(cont->sample_terms, term_t) = heap_bignum(cont->heap, 0, 3, digs2);
	APR_ARRAY_PUSH(cont->sample_terms, term_t) = heap_binary(cont->heap, 8, data1);
	APR_ARRAY_PUSH(cont->sample_terms, term_t) = heap_binary(cont->heap, 5, data1);
	APR_ARRAY_PUSH(cont->sample_terms, term_t) = heap_binary(cont->heap, 13, data2);
	APR_ARRAY_PUSH(cont->sample_terms, term_t) = heap_binary(cont->heap, 3, data2);
	APR_ARRAY_PUSH(cont->sample_terms, term_t) = heap_fun(cont->heap, A_TRUE, A_FALSE, 2, 28394, 3, nil);
	APR_ARRAY_PUSH(cont->sample_terms, term_t) = heap_long_pid(cont->heap, A_BADARG, 123, 5);
	APR_ARRAY_PUSH(cont->sample_terms, term_t) = heap_long_pid(cont->heap, A_BADARG, 127, 5);
	APR_ARRAY_PUSH(cont->sample_terms, term_t) = heap_long_pid(cont->heap, A_BADARG, 123, 3);
	APR_ARRAY_PUSH(cont->sample_terms, term_t) = heap_tuple(cont->heap, 0);
	APR_ARRAY_PUSH(cont->sample_terms, term_t) = heap_tuple2(cont->heap, tag_int(1), tag_int(2));
	APR_ARRAY_PUSH(cont->sample_terms, term_t) = heap_tuple2(cont->heap, tag_int(1), tag_int(3));
	APR_ARRAY_PUSH(cont->sample_terms, term_t) = heap_tuple3(cont->heap, tag_int(1), tag_int(3), tag_int(1));
	APR_ARRAY_PUSH(cont->sample_terms, term_t) = heap_tuple4(cont->heap, tag_int(0), tag_int(0), tag_int(0), tag_int(0));
	APR_ARRAY_PUSH(cont->sample_terms, term_t) = heap_tuple3(cont->heap, tag_int(1), tag_int(3), tag_int(1));
	APR_ARRAY_PUSH(cont->sample_terms, term_t) = heap_cons(cont->heap, tag_int(1));
	APR_ARRAY_PUSH(cont->sample_terms, term_t) = heap_cons2(cont->heap, tag_int(1), heap_cons(cont->heap, tag_int(2)));
	APR_ARRAY_PUSH(cont->sample_terms, term_t) = heap_cons2(cont->heap, tag_int(1), heap_cons2(cont->heap, tag_int(1), heap_cons(cont->heap, tag_int(2))));
	APR_ARRAY_PUSH(cont->sample_terms, term_t) = heap_cons2(cont->heap, tag_int(2), heap_cons2(cont->heap, tag_int(1), heap_cons(cont->heap, tag_int(2))));
	APR_ARRAY_PUSH(cont->sample_terms, term_t) = heap_cons2(cont->heap, tag_int(0), heap_cons(cont->heap, tag_int(2)));
	APR_ARRAY_PUSH(cont->sample_terms, term_t) = heap_cons2(cont->heap, tag_int(2), heap_cons2(cont->heap, tag_int(1), heap_cons(cont->heap, tag_int(2))));
	APR_ARRAY_PUSH(cont->sample_terms, term_t) = heap_cons2(cont->heap, tag_int(0), heap_cons(cont->heap, tag_int(2)));
	APR_ARRAY_PUSH(cont->sample_terms, term_t) = heap_cons2(cont->heap, A_ERROR, A_BADARG);
	APR_ARRAY_PUSH(cont->sample_terms, term_t) = heap_cons2(cont->heap, A_ERROR, A_BADARG);
	return "ok";
}

const char *compare_test_show_samples(navel_context_t *cont, apr_pool_t *rp)
{
	apr_array_header_t *bs = apr_array_make(rp, 8, sizeof(char *));
	int index = 0;

	term_t *ptr = (term_t *)cont->sample_terms->elts;
	term_t *end = ptr + cont->sample_terms->nelts;

	APR_ARRAY_PUSH(bs, char *) = "<ol>";
	while (ptr < end)
	{
		APR_ARRAY_PUSH(bs, char *) = apr_psprintf(rp,
			"<li id=\"%d\">%s</li>", index, term2html(*ptr, cont->atoms, rp));

		index++;
		ptr++;
	}
	APR_ARRAY_PUSH(bs, char *) = "</ol>";

	return apr_array_pstrcat(rp, bs, 0);
}

const char *compare_test_do_some(const char *req, navel_context_t *cont, apr_pool_t *pool)
{
	// /values/
	char *buf = apr_pstrdup(pool, req);
	char *ctx;
	int index1, index2;
	term_t a, b;
	int a_eq_b, b_eq_a,	a_lt_b,	b_lt_a;

	apr_strtok(buf+1, "/", &ctx); // values
	index1 = atoi(apr_strtok(NULL, "/", &ctx));
	index2 = atoi(apr_strtok(NULL, "/", &ctx));

	a = ((term_t *)cont->sample_terms->elts)[index1];
	b = ((term_t *)cont->sample_terms->elts)[index2];

	a_eq_b = are_terms_equal(a, b);
	b_eq_a = are_terms_equal(b, a);
	a_lt_b = is_term_smaller(a, b, cont->atoms);
	b_lt_a = is_term_smaller(b, a, cont->atoms);

#define true_false(x) ((x) ?"true" :"false")

	return apr_psprintf(pool,
		"<p>a == b &rarr; <b>%s</b></p>"
		"<p>b == a &rarr; <b>%s</b></p>"
		"<p>a &lt; b &rarr; <b>%s</b></p>"
		"<p>b &lt; a &rarr; <b>%s</b></p>",
			true_false(a_eq_b),
			true_false(b_eq_a),
			true_false(a_lt_b),
			true_false(b_lt_a));
}

//EOF
