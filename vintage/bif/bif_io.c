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

#include "bif.h"

//TODO: remove the whole file
#include <stdio.h>

static int write_one(term_t A);

term_t bif_print_iolist1(term_t IOList, process_t *ctx)
{
	if (!write_one(IOList))
		return A_BADARG;
	result(A_OK);
	return AI_OK;
}

static int write_one(term_t A)
{
	if (is_int(A))
	{
		apr_byte_t ch = (apr_byte_t)int_value(A);
		fwrite(&ch, 1, 1, stdout);
	}
	else if (is_cons(A))
	{
		term_t h = lst_value(A);
		term_t t = lst_next(A);
		if (!write_one(h))
			return 0;
		if (!write_one(t))
			return 0;
	}
	else if (is_nil(A))
	{
		//skip
	}
	else if (is_binary(A))
	{
		apr_byte_t *data = bin_data(A);
		int size = int_value2(bin_size(A));
		fwrite(data, 1, size, stdout);
	}
	else
		return 0;
	return 1;
}

// EOF
