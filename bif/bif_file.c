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

#include <apr_file_io.h>

#include "port.h"
#include "errors.h"

term_t bif_open0_3(term_t FileName, term_t Mode, term_t Perms, process_t *ctx)
{
	apr_status_t rs;
	apr_pool_t *p;
	apr_file_t *file;
	port_t *port;

	if (!is_binary(FileName) || !is_int(Mode) || !is_int(Perms))
		return A_BADARG;

	apr_pool_create(&p, 0);
	rs = apr_file_open(&file, (char *)bin_data(FileName), (apr_uint32_t)int_value(Mode), (apr_uint32_t)int_value(Perms), p);
	if (rs != 0)
	{
		apr_pool_destroy(p);
		return decipher_status(rs);
	}

	port = port_file_make(file);

	//set initial port owner
	//port->owner_in = port->owner_out = proc_pid(ctx, port->xp);
	port->owner_in = port->owner_out = A_UNDEFINED;

	//put port to polling ring
	port_register(port);

	result(port_id(port, proc_gc_pool(ctx)));
	return AI_OK;
}

term_t bif_read0_2(term_t Port, term_t Len, process_t *ctx)
{
	apr_status_t rs;
	apr_size_t size;
	apr_byte_t *buf;
	term_t bin;
	port_t *p;
	if (!is_port(Port) || !is_int(Len))
		return A_BADARG;
	p = port_lookup(prp_serial(Port));
	if (p == 0)
		return A_BADARG;
	size = (apr_size_t)int_value(Len);
	buf = xalloc(proc_gc_pool(ctx), size);
	rs = p->read(p, buf, &size);
	if (size == 0 && APR_STATUS_IS_EOF(rs))
		result(A_EOF);
	else if (size == 0 && rs != 0)
		return decipher_status(rs);
	else
	{
		bin = make_binary(intnum(size), buf, proc_gc_pool(ctx));
		result(bin);
	}
	return AI_OK;
}

term_t bif_write0_2(term_t Port, term_t Bin, process_t *ctx)
{
	apr_status_t rs;
	apr_size_t size;
	port_t *p;
	if (!is_port(Port) || !is_binary(Bin))
		return A_BADARG;
	p = port_lookup(prp_serial(Port));
	if (p == 0)
		return A_BADARG;
	size = (apr_size_t)int_value(bin_size(Bin));
	rs = p->write(p, bin_data(Bin), &size);
	if (rs != 0)
		return decipher_status(rs); //TODO: something may still be written
	result(intnum(size));
	return AI_OK;
}

//term_t bif_read_file1(term_t File, process_t *ctx)
//{
//	apr_status_t rs;
//	apr_pool_t *pool;
//	const char *file_name;
//	apr_finfo_t fi;
//	apr_file_t *input;
//	apr_size_t len;
//	apr_byte_t *data;
//
//	apr_pool_create(&pool, 0);
//	file_name = ltoz(File, pool);
//	rs = apr_file_open(&input, file_name, APR_READ, 0, pool);
//	if (rs == 0)
//		rs = apr_file_info_get(&fi, APR_FINFO_SIZE, input);
//	if (rs == 0)
//	{
//		len = (apr_size_t) fi.size;
//		data = xalloc(proc_gc_pool(ctx), len);
//		rs = apr_file_read(input, data, &len);
//	}
//	if (rs == 0)
//	{
//		term_t bin = make_binary(intnum(len), data, proc_gc_pool(ctx));
//		result(make_tuple2(A_OK, bin, proc_gc_pool(ctx)));
//	}
//	else
//		result(make_tuple2(A_NOENT, File, proc_gc_pool(ctx)));
//	apr_pool_destroy(pool);
//	return AI_OK;
//}

//term_t bif_write_file2(term_t File, term_t IOList, process_t *ctx)
//{
//	apr_status_t rs;
//	apr_pool_t *tmp;
//	const char *file_name;
//	apr_file_t *out;
//
//	apr_pool_create(&tmp, 0);
//	if (!is_string(File))
//		return A_BADARG;
//	file_name = ltoz(File, tmp);
//
//	rs = apr_file_open(&out, file_name, APR_WRITE | APR_CREATE | APR_TRUNCATE, 0, tmp);
//	if (rs == 0)
//	{
//		if (!write_one(out, IOList))
//		{
//			apr_pool_destroy(tmp);
//			return A_BADARG;
//		}
//		rs = apr_file_close(out);
//	}
//	if (rs == 0)
//		result(A_OK);
//	else
//		result(decipher_status(rs));
//	apr_pool_destroy(tmp);
//	return AI_OK;
//}
//
//static int write_one(apr_file_t *out, term_t A)
//{
//	apr_size_t len;
//	if (is_int(A))
//	{
//		apr_byte_t ch = int_value(A);
//		len = 1;
//		apr_file_write(out, &ch, &len);
//	}
//	else if (is_cons(A))
//	{
//		term_t cons = A;
//		do {
//			if (!write_one(out, lst_value(cons)))
//				return 0;
//			cons = lst_next(cons);
//		} while (is_cons(cons));
//	}
//	else if (is_nil(A))
//	{
//		//skip
//	}
//	else if (is_binary(A))
//	{
//		apr_byte_t *data = bin_data(A);
//		len = int_value(bin_size(A));
//		apr_file_write(out, data, &len);
//	}
//	else
//		return 0;
//	return 1;
//}

term_t bif_delete1(term_t File, process_t *ctx)
{
	apr_pool_t *tmp;
	apr_status_t rs;
	const char *path;
	if (!is_string(File))
		return A_BADARG;
	apr_pool_create(&tmp, 0);
	path = ltoz(File, tmp);
	rs = apr_file_remove(path, tmp);
	if (rs == 0)
		result(A_OK);
	else
		result(make_tuple2(A_ERROR, decipher_status(rs), proc_gc_pool(ctx)));
	apr_pool_destroy(tmp);
	return AI_OK;
}

term_t bif_rename2(term_t Src, term_t Dst, process_t *ctx)
{
	apr_status_t rs;
	apr_pool_t *tmp;
	const char *from_path, *to_path;
	if (!is_string(Src) || !is_string(Dst))
		return A_BADARG;
	apr_pool_create(&tmp, 0);
	from_path = ltoz(Src, tmp);
	to_path = ltoz(Dst, tmp);
	rs = apr_file_rename(from_path, to_path, tmp);
	if (rs == 0)
		result(A_OK);
	else
		result(make_tuple2(A_ERROR, decipher_status(rs), proc_gc_pool(ctx)));
	apr_pool_destroy(tmp);
	return AI_OK;
}

// set_cwd(Dir) -> ok | {error,Reason}
// get_cwd() -> {ok, Dir} | {error, Reason}
// --- get_cwd(Drive) -> {ok, Dir} | {error, Reason}
// list_dir(Dir) -> {ok, Filenames} | {error, Reason}
// make_dir(Dir) -> ok | {error, Reason}

term_t bif_set_cwd1(term_t Dir, process_t *ctx)
{
	apr_status_t rs;
	apr_pool_t *p;
	const char *path;
	if (!is_string(Dir))
		return A_BADARG;
	apr_pool_create(&p, 0);
	path = ltoz(Dir, p);
	rs = apr_filepath_set(path, p);
	apr_pool_destroy(p);
	if (rs == 0)
		result(A_OK);
	else
		result(make_tuple2(A_ERROR, decipher_status(rs), proc_gc_pool(ctx)));
	return AI_OK;
}

term_t bif_get_cwd0(process_t *ctx)
{
	apr_status_t rs;
	apr_pool_t *p;
	char *path;
	apr_pool_create(&p, 0);
	rs = apr_filepath_get(&path, 0, p);
	if (rs == 0)
		result(make_tuple2(A_OK, ztol(path, proc_gc_pool(ctx)), proc_gc_pool(ctx)));
	else
		result(make_tuple2(A_ERROR, decipher_status(rs), proc_gc_pool(ctx)));
	apr_pool_destroy(p);
	return AI_OK;
}

//drwxr-xr-x  5 root root 4096 2008-12-02 15:49 .
//drwxr-xr-x 21 root root 4096 2008-12-02 15:26 ..
//drwx------  2 root root 4096 2008-12-02 16:41 .aptitude
//-rw-------  1 root root 1498 2008-12-08 14:46 .bash_history
//-rw-r--r--  1 root root 2227 2007-05-15 16:07 .bashrc
//-rw-r--r--  1 root root  141 2007-05-15 16:07 .profile
//drwx------  2 root root 4096 2008-12-02 15:04 .ssh
//drwxr-xr-x  3 root root 4096 2008-12-02 15:49 .subversion

term_t long2term(apr_int64_t value, xpool_t *xp)
{
	if (value < MIN_INT_VALUE || value > MAX_INT_VALUE)
		return bignum(bignum_from64(value, xp));
	else
		return intnum(value);
}

term_t make_file_info(apr_finfo_t *fi, xpool_t *xp)
{
	apr_status_t rs;
	apr_pool_t *tmp;

	term_t file_info = make_tuple(10, xp);
	term_t *e = tup_elts(file_info);

	e[0] = A_FILE_INFO0;
	e[1] = (fi->valid & APR_FINFO_TYPE) ? intnum(fi->filetype) : A_UNDEFINED;
	e[2] = (fi->valid & APR_FINFO_NAME) ? ztol(fi->name, xp) : A_UNDEFINED;
	e[3] = (fi->valid & APR_FINFO_SIZE) ? long2term(fi->size, xp) : A_UNDEFINED;
	e[4] = (fi->valid & APR_FINFO_MTIME) ? long2term(fi->mtime, xp) : A_UNDEFINED;
	e[5] = (fi->valid & APR_FINFO_CTIME) ? long2term(fi->ctime, xp) : A_UNDEFINED;
	e[6] = (fi->valid & APR_FINFO_ATIME) ? long2term(fi->atime, xp) : A_UNDEFINED;

	e[7] = A_UNDEFINED;
	e[8] = A_UNDEFINED;

	apr_pool_create(&tmp, 0);
	if (fi->valid & APR_FINFO_USER)
	{
		char *user_name;
		rs = apr_uid_name_get(&user_name, fi->user, tmp);
		if (rs == 0)
			e[7] = ztol(user_name, xp);
	}
	if (fi->valid & APR_FINFO_GROUP)
	{
		char *group_name;
		rs = apr_gid_name_get(&group_name, fi->group, tmp);
		if (rs == 0)
			e[8] = ztol(group_name, xp);
	}
	apr_pool_destroy(tmp);

	//TODO: only user or group permissions may be known
	e[9] = (fi->valid & APR_FINFO_PROT)? intnum(fi->protection) : A_UNDEFINED;

	return file_info;
}

term_t bif_read_file_info0_1(term_t Filename, process_t *ctx)
{
	apr_status_t rs;
	apr_finfo_t fi;
	const char *file_name;
	apr_pool_t *tmp;
	apr_uint32_t wanted;

	if (!is_string(Filename))
		return A_BADARG;

	apr_pool_create(&tmp, 0);
	file_name = ltoz(Filename, tmp);

	wanted = APR_FINFO_MTIME | APR_FINFO_CTIME | APR_FINFO_ATIME |
		 APR_FINFO_NAME | APR_FINFO_SIZE | APR_FINFO_TYPE |
		 APR_FINFO_USER | APR_FINFO_GROUP | APR_FINFO_PROT;

	rs = apr_stat(&fi, file_name, wanted, tmp);
	if (rs == 0 || APR_STATUS_IS_INCOMPLETE(rs))
	{
		term_t finfo = make_file_info(&fi, proc_gc_pool(ctx));
		result(finfo);
	}
	else
		result(make_tuple2(A_ERROR, decipher_status(rs), proc_gc_pool(ctx)));

	apr_pool_destroy(tmp);
	return AI_OK;
}

term_t bif_list_dir1(term_t Dir, process_t *ctx)
{
	apr_status_t rs;
	apr_pool_t *p;
	
	const char *path;
	apr_dir_t *dir;

	term_t r = nil;
	term_t cons = nil;

	if (!is_string(Dir))
		return A_BADARG;

	apr_pool_create(&p, 0);

	path = ltoz(Dir, p);
	rs = apr_dir_open(&dir, path, p);
	if (rs == 0)
	{
		apr_finfo_t fi;
		for (;;)
		{
			rs = apr_dir_read(&fi, APR_FINFO_NAME | APR_FINFO_SIZE, dir);
			if (rs == 0 || APR_STATUS_IS_INCOMPLETE(rs))
			{
				term_t name = ztol(fi.name, proc_gc_pool(ctx));
				lst_add(r, cons, name, proc_gc_pool(ctx));
			}

			if (APR_STATUS_IS_ENOENT(rs))
			{
				rs = APR_SUCCESS;
				break;
			}
		}

		apr_dir_close(dir);
	}

	if (rs != 0 && !APR_STATUS_IS_ENOENT(rs))
		result(make_tuple2(A_ERROR, decipher_status(rs), proc_gc_pool(ctx)));
	else
		result(make_tuple2(A_OK, r, proc_gc_pool(ctx)));

	apr_pool_destroy(p);
	return AI_OK;
}

term_t bif_list_dir2_1(term_t Dir, process_t *ctx)
{
	apr_status_t rs;
	apr_pool_t *p;
	const char *path;
	apr_dir_t *dir;
	term_t r = nil;
	if (!is_string(Dir))
		return A_BADARG;
	apr_pool_create(&p, 0);
	path = ltoz(Dir, p);
	rs = apr_dir_open(&dir, path, p);
	if (rs == 0)
	{
		term_t cons = nil;
		apr_finfo_t fi;
		for (;;)
		{
			term_t v;
			term_t name;
			rs = apr_dir_read(&fi, APR_FINFO_NAME | APR_FINFO_SIZE, dir);
			if (rs != 0)
				break;
			name = ztol(fi.name, proc_gc_pool(ctx));
			if (fi.filetype == APR_DIR)
				v = make_tuple2(A_DIR, name, proc_gc_pool(ctx));
			else if (fi.filetype == APR_REG)
				v = make_tuple3(A_FILE, name, intnum(fi.size), proc_gc_pool(ctx));	//TODO: large size
			else
				v = make_tuple2(A_UNKNOWN, name, proc_gc_pool(ctx));
			lst_add(r, cons, v, proc_gc_pool(ctx));
		}

		apr_dir_close(dir);
	}

	if (rs != 0 && !APR_STATUS_IS_ENOENT(rs))
		result(make_tuple2(A_ERROR, decipher_status(rs), proc_gc_pool(ctx)));
	else
		result(make_tuple2(A_OK, r, proc_gc_pool(ctx)));

	apr_pool_destroy(p);
	return AI_OK;
}

term_t bif_list_dir3_0_1(term_t Dir, process_t *ctx)
{
	apr_status_t rs;
	apr_pool_t *p;
	const char *path;
	apr_dir_t *dir;
	term_t r = nil;
	term_t cons = nil;

	apr_uint32_t wanted = APR_FINFO_MTIME | APR_FINFO_CTIME | APR_FINFO_ATIME |
		 APR_FINFO_NAME | APR_FINFO_SIZE | APR_FINFO_TYPE |
		 APR_FINFO_USER | APR_FINFO_GROUP | APR_FINFO_PROT;

	if (!is_string(Dir))
		return A_BADARG;

	apr_pool_create(&p, 0);
	path = ltoz(Dir, p);
	rs = apr_dir_open(&dir, path, p);
	if (rs == 0)
	{
		apr_finfo_t fi;
		for (;;)
		{
			rs = apr_dir_read(&fi, wanted, dir);
			if (rs == 0 || APR_STATUS_IS_INCOMPLETE(rs))
			{
				term_t file_info = make_file_info(&fi, proc_gc_pool(ctx));
				lst_add(r, cons, file_info, proc_gc_pool(ctx));
			}
			else if (APR_STATUS_IS_ENOENT(rs))
			{
				rs = 0;
				break;
			}
		}
		apr_dir_close(dir);
	}

	if (rs != 0)
		result(make_tuple2(A_ERROR, decipher_status(rs), proc_gc_pool(ctx)));
	else
		result(make_tuple2(A_OK, r, proc_gc_pool(ctx)));

	apr_pool_destroy(p);
	return AI_OK;
}

term_t bif_make_dir1(term_t Dir, process_t *ctx)
{
	apr_status_t rs;
	apr_pool_t *tmp;
	const char *path;

	if (!is_string(Dir))
		return A_BADARG;

	apr_pool_create(&tmp, 0);
	path = ltoz(Dir, tmp);
	rs = apr_dir_make(path, APR_OS_DEFAULT, tmp);
	if (rs == 0)
		result(A_OK);
	else
		result(make_tuple2(A_ERROR, decipher_status(rs), proc_gc_pool(ctx)));

	apr_pool_destroy(tmp);
	return AI_OK;
}

term_t bif_del_dir1(term_t Dir, process_t *ctx)
{
	apr_status_t rs;
	apr_pool_t *tmp;
	const char *path;

	if (!is_string(Dir))
		return A_BADARG;

	apr_pool_create(&tmp, 0);
	path = ltoz(Dir, tmp);
	rs = apr_dir_remove(path, tmp);
	if (rs == 0)
		result(A_OK);
	else
		result(make_tuple2(A_ERROR, decipher_status(rs), proc_gc_pool(ctx)));

	apr_pool_destroy(tmp);
	return AI_OK;
}

//EOF
