//
//
//

#include "list.h"

int list_length(term_t list)
{
	int len = 0;
	while (is_cons(list))
	{
		len++;
		list = peel(list)->cons.tail;
	}
	return len;
}

const char *ltoz(term_t l, apr_pool_t *pool)
{
	int n = list_length(l);
	term_t x = l;
	char *buf, *p;

	if (n == 0)
		return "";

	buf = apr_palloc(pool, n+1); //+1 for null byte
	p = buf;

	x = l;
	while (is_cons(x))
	{
		term_box_t *cb = peel(x);
		term_t v = cb->cons.head;
		if (is_int(v))
			*p++ = int_value(v);
		x = cb->cons.tail;
	}
	*p = 0;

	return buf;
}


term_t ztol(const char *z, heap_t *hp)
{
	term_t first = nil;
	term_t last = nil;
	while (*z)
		cons_up(first, last, tag_int(*z++), hp);
	return first;
}

term_t stol(cstr_t *s, heap_t *hp)
{
	int i;
	term_t first = nil;
	term_t last = nil;
	for (i = 0; i < s->size; i++)
		cons_up(first, last, tag_int(s->data[i]), hp);
	return first;
}

//EOF
