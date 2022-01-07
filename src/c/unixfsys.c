/* -*- mode: c -*- */
/*
    unixfsys.c  -- Unix file system interface.
*/
/*
    Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
    Copyright (c) 1990, Giuseppe Attardi.
    Copyright (c) 2001, Juan Jose Garcia Ripoll.
    Copyright (c) 2010-2019,2021, Jean-Claude Beaudoin.

    MKCL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 3 of the License, or (at your option) any later version.

    See file '../../Copyright' for full details.
*/

#define _FILE_OFFSET_BITS 64

#include <mkcl/mkcl.h>
#include <mkcl/mkcl-inl.h>
#include <mkcl/internal.h>

#include <string.h>
#include <stdio.h>
#if MKCL_UNIX
# include <pwd.h>
#elif defined(_MSC_VER)
# include <io.h>
# define F_OK 0
#endif
#include <sys/types.h>
#include <sys/stat.h>
#include <stdlib.h>
#include <dirent.h>

#if MKCL_WINDOWS
# include <windows.h>
# include <direct.h>
# include <ctype.h>
#endif

#include <fcntl.h>
#include <errno.h>


struct OSpath
{
  mkcl_object pathname;
  mkcl_object reverse_directory_list_shadow;
};

static void init_absolute_OSpath(MKCL, struct OSpath * ospath)
{
  mkcl_object p = mkcl_alloc_raw_pathname(env);

  p->pathname.logical   = FALSE;
  p->pathname.complete  = FALSE;
  p->pathname.host      = mk_cl_Cnil;
  p->pathname.device    = mk_cl_Cnil;
  p->pathname.directory = mkcl_list1(env, MK_KEY_absolute);
  p->pathname.name      = mk_cl_Cnil;
  p->pathname.type      = mk_cl_Cnil;
  p->pathname.version   = MK_KEY_newest;
  p->pathname.namestring = mk_cl_Cnil;

  ospath->pathname = p;
  ospath->reverse_directory_list_shadow = mkcl_list1(env, p->pathname.directory);
}

static void init_relative_OSpath(MKCL, struct OSpath * ospath)
{
  mkcl_object p = mkcl_alloc_raw_pathname(env);

  p->pathname.logical   = FALSE;
  p->pathname.complete  = FALSE;
  p->pathname.host      = mk_cl_Cnil;
  p->pathname.device    = mk_cl_Cnil;
  p->pathname.directory = mkcl_list1(env, MK_KEY_relative);
  p->pathname.name      = mk_cl_Cnil;
  p->pathname.type      = mk_cl_Cnil;
  p->pathname.version   = MK_KEY_newest;
  p->pathname.namestring = mk_cl_Cnil;

  ospath->pathname = p;
  ospath->reverse_directory_list_shadow = mkcl_list1(env, p->pathname.directory);
}


static void copy_OSpath(MKCL, struct OSpath * new, struct OSpath * old)
{
  new->pathname = mkcl_alloc_raw_pathname(env);
  new->pathname->pathname = old->pathname->pathname;

  if (MKCL_CONSP(old->pathname->pathname.directory))
    {
      mkcl_object shadow = mk_cl_Cnil, directory;
      new->pathname->pathname.directory = mkcl_copy_proper_list(env, old->pathname->pathname.directory);

      for (directory = new->pathname->pathname.directory; !mkcl_Null(directory); directory = MKCL_CONS_CDR(directory))
	shadow = mkcl_cons(env, directory, shadow);
      new->reverse_directory_list_shadow = shadow;
    }
  else
    new->reverse_directory_list_shadow = mk_cl_Cnil;
}

static void OSpath_push_dir(MKCL, struct OSpath * path, mkcl_object dir)
{
  mkcl_object last_shadow = path->reverse_directory_list_shadow;

  mkcl_object last = MKCL_CONS_CAR(last_shadow);
  mkcl_object new = mkcl_list1(env, dir);
  
  MKCL_RPLACD(last, new);
  path->reverse_directory_list_shadow = mkcl_cons(env, new, last_shadow);
  path->pathname->pathname.namestring = mk_cl_Cnil; /* clear pathname namestring cache */
}

static mkcl_object OSpath_pop_dir(MKCL, struct OSpath * path)
{
  mkcl_object last_shadow = path->reverse_directory_list_shadow;

  mkcl_object last = MKCL_CONS_CAR(last_shadow);
  mkcl_object dir = MKCL_CONS_CAR(last);

  path->pathname->pathname.namestring = mk_cl_Cnil; /* clear pathname namestring cache */
  if (!(dir == MK_KEY_absolute || dir == MK_KEY_relative))
    {
      mkcl_object next_to_last_shadow = MKCL_CONS_CDR(last_shadow);
      mkcl_object next_to_last = MKCL_CONS_CAR(next_to_last_shadow);

      MKCL_RPLACD(next_to_last, mk_cl_Cnil);
      path->reverse_directory_list_shadow = next_to_last_shadow;
      return dir;
    }
  else
    return mk_cl_Cnil;
}



static int is_slash(int c) { return MKCL_IS_DIR_SEPARATOR(c); }


static int
safe_chdir(MKCL, mkcl_object path)
{
  mkcl_dynamic_extent_OSstring(env, os_path, path);
  int output;

#if MKCL_WINDOWS
  MKCL_LIBC_NO_INTR(env, output = _wchdir(mkcl_OSstring_self(os_path)));
#else
  MKCL_LIBC_NO_INTR(env, output = chdir((char *) mkcl_OSstring_self(os_path)));
#endif
  return output;
}

#if MKCL_WINDOWS
typedef struct __stat64 os_file_stat;
#else
typedef struct stat os_file_stat;
#endif

#if MKCL_UNIX
static int
safe_stat(MKCL, mkcl_object path, os_file_stat *sb)
{
  int output;
  mkcl_dynamic_extent_OSstring(env, os_path, path);

  MKCL_LIBC_NO_INTR(env, output = stat((char *) mkcl_OSstring_self(os_path), sb));
  return output;
}
#endif /* MKCL_UNIX */

#ifdef HAVE_LSTAT
static int
safe_lstat(MKCL, mkcl_object path, struct stat *sb)
{
  int output;
  mkcl_dynamic_extent_OSstring(env, os_path, path);

  MKCL_LIBC_NO_INTR(env, output = lstat((char *) mkcl_OSstring_self(os_path), sb));
  return output;
}
#endif

#if MKCL_UNIX
static mkcl_object safe_realpath(MKCL, mkcl_object orig_pathname, mkcl_object path)
{
  mkcl_dynamic_extent_OSstring(env, os_path, path);
  mkcl_OSstring_raw_type os_raw_path = mkcl_OSstring_self(os_path);
  char * output;

  MKCL_LIBC_NO_INTR(env, output = realpath((char *) os_raw_path, NULL));
  if (output == NULL)
    {
      /* errno could be: EACCES, EINVAL, EIO, ELOOP, ENAMETOOLONG, ENOENT, ENOTDIR. */
      mkcl_FElibc_file_error(env, orig_pathname, "realpath() failed in safe_realpath()", 0);
      return NULL;
    }
  else
    {
      mkcl_object str = mkcl_cstring_copy_to_OSstring(env, output);

      free(output); /* Yes this bypasses the GC since it was malloced by realpath(). */
      return str;
    }
}

#elif MKCL_WINDOWS

static mkcl_object safe_realpath(MKCL, mkcl_object orig_pathname, mkcl_object path)
{
  mkcl_dynamic_extent_OSstring(env, os_path, path);
  mkcl_OSstring_raw_type os_raw_path = mkcl_OSstring_self(os_path);
  DWORD size = MAX_PATH + 1;
  wchar_t * output;

  MKCL_LIBC_NO_INTR(env, output = malloc(size * sizeof(wchar_t)));
  if (output == NULL)
    {
      mkcl_FEerror(env, "malloc(): Out of memory", 0);
      return NULL;
    }

  MKCL_LIBC_NO_INTR(env, size = GetFullPathNameW(os_raw_path, size, output, NULL));
  if (size > MAX_PATH)
    { /* Not big enough */
      MKCL_LIBC_NO_INTR(env, (output = realloc(output, size * sizeof(wchar_t))));
      if (output == NULL)
	{
	  mkcl_FEerror(env, "realloc(): Out of memory", 0);
	  return NULL;
	}
      MKCL_LIBC_NO_INTR(env, size = GetFullPathNameW(os_raw_path, size, output, NULL));
    }
  if (size == 0)
    { /* Something went wrong. */
      mkcl_FEwin32_file_error(env, orig_pathname, "GetFullPathname() failed in safe_realpath().", 0);
      return NULL;
    }
  else
    {
      mkcl_object str = mkcl_cstring16_copy_to_utf_16(env, output);

      free(output); /* Yes this bypasses the GC since it was malloced by realpath(). */
      
      return str;    
    }
}
#endif


#if MKCL_WINDOWS
/*
 * Finds current directory by using _getdcwd() with an adjustable
 * string which grows until it can host the whole path.
 */
static mkcl_object
current_dir_on_drive(MKCL, int drive)
{
  mkcl_object output;
  const wchar_t *ok;
  mkcl_index size = 128;

  do {
    output = mkcl_alloc_OSstring(env, size);
    MKCL_LIBC_NO_INTR(env, ok = _wgetdcwd(drive, mkcl_OSstring_self(output), size));
    size += 256;
  } while (ok == NULL && errno == ERANGE);
  if (ok == NULL)
    mkcl_FElibc_error(env, "current_dir_on_drive() failed on _getdcwd()", 0);
  size = wcslen(mkcl_OSstring_self(output));
  mkcl_OSstring_set_fillp(output, size);
# ifdef _MSC_VER
#  error left as an exercise to the reader!
  {
    unsigned char *c;
    for (c = output->base_string.self; *c; c++)
      if (*c == '\\')
	*c = '/';
  }
# endif

  if (mkcl_OSstring_last(env, output) != '/')
    mkcl_OSstring_push_extend(env, output, '/');
  return mkcl_OSstring_to_string(env, output);
}
#endif /* MKCL_WINDOWS */

/*
 * Finds current directory by using getcwd() with an adjustable
 * string which grows until it can host the whole path.
 */
static mkcl_object
current_dir(MKCL)
{
  mkcl_object output;
#if MKCL_WINDOWS
  const wchar_t *ok;
  mkcl_index size = 256;

  do {
    output = mkcl_alloc_OSstring(env, size);
    MKCL_LIBC_NO_INTR(env, ok = _wgetcwd(mkcl_OSstring_self(output), size));
    size += 256;
  } while (ok == NULL && errno == ERANGE);
  if (ok == NULL)
    mkcl_FElibc_error(env, "current_dir() failed on _wgetcwd()", 0);
  size = wcslen(mkcl_OSstring_self(output));
  mkcl_OSstring_set_fillp(output, size);
# ifdef _MSC_VER
#  error left as an exercise to the reader!
  {
    unsigned char *c;
    for (c = output->base_string.self; *c; c++)
      if (*c == '\\')
	*c = '/';
  }
# endif
#else /*  MKCL_WINDOWS */
  const char *ok;
  mkcl_index size = 256;

  do {
    output = mkcl_alloc_OSstring(env, size);
    MKCL_LIBC_NO_INTR(env, ok = getcwd((char *) mkcl_OSstring_self(output), size));
    size += 256;
  } while (ok == NULL && errno == ERANGE);
  if (ok == NULL)
    mkcl_FElibc_error(env, "current_dir() failed on getcwd()", 0);
  size = strlen((char *) mkcl_OSstring_self(output)); /* Unix only! */
  mkcl_OSstring_set_fillp(output, size);
#endif

  if (mkcl_OSstring_last(env, output) != '/')
    mkcl_OSstring_push_extend(env, output, '/');
  return mkcl_OSstring_to_string(env, output);
}

/*
 * Using a certain path, query the type of the filesystem element it points to.
 */

static mkcl_object
file_kind(MKCL, mkcl_object os_filename, bool follow_links)
{
  mkcl_OSstring_raw_type raw_os_filename = mkcl_OSstring_self(os_filename);
  mkcl_object output;
#if MKCL_WINDOWS
  DWORD dw;
  MKCL_LIBC_NO_INTR(env, dw = GetFileAttributesW( raw_os_filename ));
  if (dw == INVALID_FILE_ATTRIBUTES)
    /* mkcl_FEwin32_file_error(env, os_filename, "GetFileAttributes() failed in file_kind()", 0); */
    output = mk_cl_Cnil; /* We prefer to have the caller handle the error just above us. */
  else if ( dw & FILE_ATTRIBUTE_DIRECTORY )
    output = MK_KEY_directory;
# ifdef FILE_ATTRIBUTE_REPARSE_POINT
  else if ( dw & FILE_ATTRIBUTE_REPARSE_POINT )
    output = MK_KEY_link;
# endif
  else if ( dw & FILE_ATTRIBUTE_DEVICE )
    output = MK_KEY_device;
  else
    output = MK_KEY_file;
#else /* !MKCL_WINDOWS */
  struct stat buf;
  int rc;

# ifdef HAVE_LSTAT
  MKCL_LIBC_NO_INTR(env, rc = (follow_links ? stat : lstat)((char *) mkcl_OSstring_self(os_filename), &buf));
# else
  MKCL_LIBC_NO_INTR(env, rc = stat((char *) mkcl_OSstring_self(os_filename), &buf));
# endif

  if (rc)
    /* Here errno could be EACCES, EBADF, EFAULT, ELOOP,
       ENAMETOOLONG, ENOENT, ENOMEM, ENOTDIR, EOVERFLOW. */
    /* ANSI requires us to raise a file-error on most of these. JCB */
    /* mkcl_FElibc_file_error(env, os_filename, "stat() failed in file_kind()", 0); */
    output = mk_cl_Cnil; /* We prefer to have the caller handle the error just above us. */
  else if (S_ISDIR(buf.st_mode))
    output = MK_KEY_directory;
  else if (S_ISREG(buf.st_mode))
    output = MK_KEY_file;
# ifdef HAVE_LSTAT
  else if (S_ISLNK(buf.st_mode))
    output = MK_KEY_link;
# endif
  else if (S_ISCHR(buf.st_mode))
    output = MK_KEY_device;
  else if (S_ISBLK(buf.st_mode))
    output = MK_KEY_device;
  else
    output = MK_KEY_special;
#endif /* !MKCL_WINDOWS */
  return output;
}


mkcl_object mk_si_file_kind(MKCL, mkcl_narg narg, mkcl_object filespec, ...)
{
  mkcl_call_stack_check(env);
  {
    mkcl_object follow_symlinks = mk_cl_Cnil;
    mkcl_object signal_error = mk_cl_Cnil;

    MKCL_RECEIVE_2_KEYWORD_ARGUMENTS(env, MK_SI_file_kind, narg, 1, filespec, MK_KEY_follow_symlinks, &follow_symlinks, MK_KEY_signal_error, &signal_error);

    mkcl_object filename = mk_si_coerce_to_filename(env, filespec);
    mkcl_dynamic_extent_OSstring(env, os_filename, filename);
    mkcl_object kind = file_kind(env, os_filename, !mkcl_Null(follow_symlinks));

    if (mkcl_Null(kind) && !mkcl_Null(signal_error))
      {
#if MKCL_WINDOWS
        mkcl_FEwin32_file_error(env, os_filename, "GetFileAttributes() failed in file_kind()", 0);
#else
        mkcl_FElibc_file_error(env, os_filename, "stat() failed in file_kind()", 0);
#endif
      }
    mkcl_return_value(kind);
  }
}



/*
 * Search the actual name of the directory of a pathname,
 * going through links if they exist. Default is
 * current directory
 */
mkcl_object
mk_cl_truename(MKCL, mkcl_object orig_pathname)
{
  mkcl_call_stack_check(env);
  mkcl_object pathname = orig_pathname;
  mkcl_object filename = mk_si_coerce_to_filename(env, pathname);
  if (mkcl_string_length(env, filename))
    {
      mkcl_object true_os_path = safe_realpath(env, filename, filename);

      if (MK_KEY_directory == file_kind(env, true_os_path, FALSE))
	if (!MKCL_IS_DIR_SEPARATOR(mkcl_OSstring_last(env, true_os_path)))
	  mkcl_OSstring_push_extend(env, true_os_path, MKCL_DIR_SEPARATOR);
      mkcl_return_value(mk_cl_pathname(env, mkcl_OSstring_to_string(env, true_os_path)));
    }
  else
    { /* This is a ridiculously nonsensical extension of the specification 
	 but it is implemented by a large number CL environments
	 (like ccl, clisp, cmulisp, lispworks and allegro)
	 and seems to be expected (even demanded) by a large number of users.
	 So, we are forced to join the crowd! JCB */
      mkcl_return_value(mk_cl_parse_namestring(env, 3, current_dir(env), mk_cl_Cnil, mk_cl_Cnil));
    }
}

int
mkcl_backup_open(MKCL, mkcl_object filename, int option, int mode)
{
  int fd;
  int error;
  static const mkcl_base_string_object(back_ext_str_obj, ".BAK");
  mkcl_dynamic_extent_OSstring(env, os_back_ext, (mkcl_object) &back_ext_str_obj);
  mkcl_dynamic_extent_OSstring(env, os_filename, filename);
  mkcl_object backupfilename = mkcl_alloc_OSstring(env, mkcl_length(env, filename) + mkcl_OSstring_size(os_back_ext));

  mkcl_OSstring_nconc(env, backupfilename, os_filename);
  mkcl_OSstring_nconc(env, backupfilename, os_back_ext);

#if MKCL_WINDOWS
  /* MS-Windows rename doesn't remove an existing file */
  MKCL_LIBC_NO_INTR(env, error = (_waccess(mkcl_OSstring_self(backupfilename), F_OK) == 0
				  && _wunlink(mkcl_OSstring_self(backupfilename))));
  if (error) {
    mkcl_object f = mkcl_OSstring_to_string(env, backupfilename);
    mkcl_FElibc_file_error(env, f, "Cannot remove the file ~S", 1, f);
  }
#endif
#if MKCL_WINDOWS
  MKCL_LIBC_NO_INTR(env, error = _wrename(mkcl_OSstring_self(os_filename), mkcl_OSstring_self(backupfilename)));
#else
  MKCL_LIBC_NO_INTR(env, error = rename((char *) mkcl_OSstring_self(os_filename), (char *) mkcl_OSstring_self(backupfilename)));
#endif
  if (error) {
    mkcl_object f = filename;
    mkcl_FElibc_file_error(env, f, "Cannot rename the file ~S to ~S.",
			   2, f, mkcl_OSstring_to_string(env, backupfilename));
  }
#if MKCL_WINDOWS
  MKCL_LIBC_NO_INTR(env, fd = _wopen(mkcl_OSstring_self(os_filename), option, mode));
#else
  MKCL_LIBC_NO_INTR(env, fd = open((char *) mkcl_OSstring_self(os_filename), option, mode));
#endif
  /* error value of fd must be handled by caller. */
  /* mkcl_dealloc(env, backupfilename); */
  return fd;
}

mkcl_object
mkcl_file_len(MKCL, int f)
{
  os_file_stat filestatus;
  int not_ok;

#if MKCL_WINDOWS
  MKCL_LIBC_NO_INTR(env, not_ok = _fstat64(f, &filestatus));
#else
  MKCL_LIBC_NO_INTR(env, not_ok = fstat(f, &filestatus));
#endif
  if (not_ok)
    mkcl_FElibc_error(env, "fstat() failed in mkcl_file_len().", 0);

  return mkcl_make_int64_t(env, filestatus.st_size);
}


mkcl_object mk_cl_rename_file(MKCL, mkcl_object old_filespec, mkcl_object new_name)
{
  mkcl_call_stack_check(env);
  /* 1) Get the old filename, and complain if it has wild components,
   *    or if it does not exist. Notice that the filename to be renamed
   *    is not the truename, because we might be renaming a symbolic link.
   */
  mkcl_object old_filename = mk_si_coerce_to_filename(env, old_filespec = mk_cl_pathname(env, old_filespec));
  mkcl_object old_truename = mk_cl_truename(env, old_filespec);
  mkcl_object new_filename;
  mkcl_interrupt_status old_intr;
  
  /* 2) Create the new file name. */
  new_name = mkcl_merge_pathnames(env, mk_cl_pathname(env, new_name), old_filespec, MK_KEY_newest);
  new_filename = mk_si_coerce_to_filename(env, new_name);

  mkcl_dynamic_extent_OSstring(env, new_os_filename, new_filename);
  mkcl_dynamic_extent_OSstring(env, old_os_filename, old_filename);
  
  mkcl_get_interrupt_status(env, &old_intr);
  mkcl_disable_interrupts(env);
  {
#if MKCL_WINDOWS
    int error_mode = SetErrorMode(0);

    if (MoveFileW(mkcl_OSstring_self(old_os_filename),
		  mkcl_OSstring_self(new_os_filename)))
      {
	SetErrorMode(error_mode);
	goto SUCCESS;
      }
    switch (GetLastError())
      {
      case ERROR_ALREADY_EXISTS:
      case ERROR_FILE_EXISTS:
	break;
      default:
	goto FAILURE_CLOBBER;
      };
    if (MoveFileExW(mkcl_OSstring_self(old_os_filename),
		    mkcl_OSstring_self(new_os_filename),
		    MOVEFILE_REPLACE_EXISTING))
      {
	SetErrorMode(error_mode);
	goto SUCCESS;
      }
    /* hack for win95/novell */
    _wchmod(mkcl_OSstring_self(old_os_filename), 0777); /* FIXME: Return value? JCB */
    _wchmod(mkcl_OSstring_self(new_os_filename), 0777); /* FIXME: Return value? JCB */
    SetFileAttributesW(mkcl_OSstring_self(new_os_filename), FILE_ATTRIBUTE_NORMAL); /* FIXME: Return value? JCB */
    SetFileAttributesW(mkcl_OSstring_self(new_os_filename), FILE_ATTRIBUTE_TEMPORARY); /* FIXME: Return value? JCB */
    if (MoveFileW(mkcl_OSstring_self(old_os_filename),
		  mkcl_OSstring_self(new_os_filename)))
      {
	SetErrorMode(error_mode);
	goto SUCCESS;
      }
    /* fallback on old behavior */
    (void)DeleteFileW(mkcl_OSstring_self(new_os_filename)); /* FIXME: Return value? JCB */
    if (MoveFileW(mkcl_OSstring_self(old_os_filename),
		  mkcl_OSstring_self(new_os_filename)))
      {
	SetErrorMode(error_mode);
	goto SUCCESS;
      }
    /* fall through */
#else /*  MKCL_WINDOWS */
    if (rename((char *) mkcl_OSstring_self(old_os_filename), /* FIXME, This will not work across filesystems. JCB */
	       (char *) mkcl_OSstring_self(new_os_filename)) == 0) {
      goto SUCCESS;
    }
    /* fall through */
#endif /*  MKCL_WINDOWS */
  }
 FAILURE_CLOBBER:
  mkcl_set_interrupt_status(env, &old_intr);
#if MKCL_WINDOWS
  mkcl_FEwin32_file_error(env, old_filespec, "Cannot rename the file ~S to ~S.", 2, old_filespec, new_name);
#else
  mkcl_FElibc_file_error(env, old_filespec, "Cannot rename the file ~S to ~S.", 2, old_filespec, new_name);
#endif
  
 SUCCESS:
  mkcl_set_interrupt_status(env, &old_intr);
  mkcl_return_3_values(new_name, old_truename, mk_cl_truename(env, new_name));
}

mkcl_object
mk_cl_delete_file(MKCL, mkcl_object file)
{
  mkcl_call_stack_check(env);
  mkcl_object filename = mk_si_coerce_to_filename(env, file);
  mkcl_dynamic_extent_OSstring(env, os_filename, filename);
  int ok;

#if MKCL_WINDOWS
  MKCL_LIBC_NO_INTR(env, ok = _wunlink(mkcl_OSstring_self(os_filename)));
#else
  MKCL_LIBC_NO_INTR(env, ok = unlink((char *) mkcl_OSstring_self(os_filename)));
#endif

  if (ok == -1)
    mkcl_FElibc_file_error(env, file, "Cannot delete file.", 0);
  mkcl_return_value(mk_cl_Ct);
}


bool mkcl_probe_file(MKCL, mkcl_object os_filename, bool follow_links)
{
#if MKCL_WINDOWS
  mkcl_OSstring_raw_type raw_os_filename = mkcl_OSstring_self(os_filename);
  DWORD dw;
  MKCL_LIBC_NO_INTR(env, dw = GetFileAttributesW( raw_os_filename ));
  if (dw == INVALID_FILE_ATTRIBUTES)
    {
      DWORD last_error = GetLastError();
      switch (last_error)
	{
	default:
	  mkcl_FEwin32_file_error(env, os_filename, "GetFileAttributes() failed in mkcl_probe_file()", 0);
	case ERROR_FILE_NOT_FOUND:
	case ERROR_PATH_NOT_FOUND:
	  return FALSE;
	}
    }
  else
    return TRUE;
#else /* !MKCL_WINDOWS */
  struct stat buf;
  int rc;

# ifdef HAVE_LSTAT
  MKCL_LIBC_NO_INTR(env, rc = (follow_links ? stat : lstat)((char *) mkcl_OSstring_self(os_filename), &buf));
# else
  MKCL_LIBC_NO_INTR(env, rc = stat((char *) mkcl_OSstring_self(os_filename), &buf));
# endif

  if (rc)
    {
      int this_errno = errno;

      switch (this_errno)
	{
	default: /* should be one of: EACCES, EBADF, EFAULT, ELOOP, ENAMETOOLONG, ENOMEM, EOVERFLOW. */
	  mkcl_FElibc_file_error(env, os_filename, "stat() failed in mkcl_probe_file()", 0);
	case ENOENT:
	case ENOTDIR:
	  return FALSE;
	}
    }
  else
    return TRUE;
#endif /* !MKCL_WINDOWS */
}

mkcl_object
mk_cl_probe_file(MKCL, mkcl_object filespec)
{
  mkcl_call_stack_check(env);
  /* INV: Both SI:FILE-KIND and TRUENAME complain if "file" has wildcards */
  mkcl_object filename = mk_si_coerce_to_filename(env, filespec);
  mkcl_dynamic_extent_OSstring(env, os_filename, filename);

  mkcl_return_value((mkcl_probe_file(env, os_filename, TRUE /* follow symlinks */) ? mk_cl_truename(env, filespec) : mk_cl_Cnil));
}

mkcl_object
mk_mkcl_stream_filename(MKCL, mkcl_object x)
{
  mkcl_call_stack_check(env);
 L:
  switch (mkcl_type_of(x)) {
  case mkcl_t_stream:
    switch ((enum mkcl_smmode)x->stream.mode) {
    case mkcl_smm_input:
    case mkcl_smm_output:
    case mkcl_smm_io:
      x = MKCL_IO_STREAM_FILENAME(x);
      break;
    case mkcl_smm_input_file:
    case mkcl_smm_output_file:
    case mkcl_smm_io_file:
    case mkcl_smm_probe:
      x = MKCL_IO_FILE_FILENAME(x);
      break;
    case mkcl_smm_synonym:
      x = MKCL_SYNONYM_STREAM_STREAM(env, x);
      goto L;
    default:
      ;/* Fall through to error message */
    }
  default:
    mkcl_FEwrong_type_argument(env, MK_CL_file_stream, x);
  }
  mkcl_return_value(x);
}

mkcl_object
mk_mkcl_probe_file_p(MKCL, mkcl_object filename)
{
  mkcl_call_stack_check(env);
  switch (mkcl_type_of(filename))
    {
    case mkcl_t_base_string: break;
    case mkcl_t_string: break;
    case mkcl_t_pathname:
      {
	mkcl_object path = filename;
	mkcl_object namestring;

	if (path->pathname.logical)
	  path = mk_cl_translate_logical_pathname(env, 1, path);
	namestring = mkcl_namestring(env, path, TRUE);
	if (namestring == mk_cl_Cnil)
	  mkcl_FEerror(env, "Pathname ~A does not have a physical namestring", 1, filename);
	else
	  filename = namestring;
      }
      break;
    case mkcl_t_stream: filename = mk_mkcl_stream_filename(env, filename); break;
    default:
      mkcl_FEwrong_type_argument(env,
				 mk_cl_list(env, 4, MK_CL_or, MK_CL_file_stream, MK_CL_string, MK_CL_pathname),
				 filename);
    }
  {
    mkcl_dynamic_extent_OSstring(env, os_filename, filename);

    mkcl_return_value((mkcl_probe_file(env, os_filename, TRUE /* follow symlinks */) ? mk_cl_Ct : mk_cl_Cnil));
  }
}

#if MKCL_WINDOWS
static mkcl_object mkcl_FILETIME_to_universal_time(MKCL, FILETIME file_time)
{
  ULARGE_INTEGER large_file_time;
  static ULARGE_INTEGER large_jan1st1900_ft = { 0, 0 };
  const static SYSTEMTIME jan1st1900_st = { 1900, 1, 1, 1, 0, 0, 0, 0};
  static FILETIME jan1st1900_ft = { 0, 0 };

  large_file_time.LowPart = file_time.dwLowDateTime;
  large_file_time.HighPart = file_time.dwHighDateTime;

  if (large_jan1st1900_ft.QuadPart == 0)
    {
      BOOL ok;
      MKCL_LIBC_NO_INTR(env, ok = SystemTimeToFileTime(&jan1st1900_st, &jan1st1900_ft));
      if (!ok)
	mkcl_FEwin32_error(env, "SystemTimetoFileTime failed to convert 1900/01/01 00:00:00 properly", 0);
      large_jan1st1900_ft.LowPart = jan1st1900_ft.dwLowDateTime;
      large_jan1st1900_ft.HighPart = jan1st1900_ft.dwHighDateTime;
    }
  
  /* FILETIME count is in 100 nanoseconds, so we divide it by 10000000 to get seconds. */
  __time64_t universal_time = (large_file_time.QuadPart - large_jan1st1900_ft.QuadPart) / (10 * 1000 * 1000);
  return mkcl_make_int64_t(env, universal_time);
}
#endif /*  MKCL_WINDOWS */

mkcl_object
mk_cl_file_write_date(MKCL, mkcl_object file)
{
  mkcl_call_stack_check(env);
  mkcl_object time, filename = mk_si_coerce_to_filename(env, file);

#if MKCL_UNIX
  os_file_stat filestatus;

  if (safe_stat(env, filename, &filestatus) < 0)
    mk_cl_error(env, 3, MK_CL_file_error, MK_KEY_pathname, file);
  else
    time = MKCL_UTC_time_to_universal_time(env, filestatus.st_mtime);
#elif MKCL_WINDOWS
  FILETIME ftWrite;
  HANDLE file_handle;
  BOOL ok;
  mkcl_dynamic_extent_OSstring(env, os_filename, filename);
  
  /* FILE_FLAG_BACKUP_SEMANTICS is required to obtain a handle on directories (see MicroSoft documentation on CreateFile).
     Hope it does not have extra unexpected consequences for regular files... */
  MKCL_LIBC_NO_INTR(env, file_handle = CreateFileW(mkcl_OSstring_self(os_filename),
						   GENERIC_READ, FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE,
						   NULL, OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS, NULL));
  if (file_handle == INVALID_HANDLE_VALUE)
    mkcl_FEwin32_file_error(env, file, "file-write-date failed to find file", 0);
  
  MKCL_LIBC_NO_INTR(env, ok = GetFileTime(file_handle, NULL, NULL, &ftWrite));
  CloseHandle(file_handle);
  if (!ok)
    mkcl_FEwin32_file_error(env, file, "file-write-date failed to retreive file write date", 0);
  
  time = mkcl_FILETIME_to_universal_time(env, ftWrite);
#else
#error mk_cl_file_write_date() not implemented on this platform.
#endif /* elif MKCL_WINDOWS */
  mkcl_return_value(time);
}

mkcl_object
mk_cl_file_author(MKCL, mkcl_object file)
{
  mkcl_call_stack_check(env);
#if MKCL_UNIX
  mkcl_object filename = mk_si_coerce_to_filename(env, file);
  mkcl_object output = mk_cl_Cnil;
  struct stat filestatus;

  if (safe_stat(env, filename, &filestatus) < 0)
    mkcl_FElibc_file_error(env, file, "mk_cl_file_author: Cannot get file status.", 0);

  {
    struct passwd pwd;
    struct passwd *result;
    const size_t bufsize = sysconf(_SC_GETPW_R_SIZE_MAX);
    char buf[bufsize]; /* a VLA. */
    int status;

    MKCL_LIBC_NO_INTR(env, (status = getpwuid_r(filestatus.st_uid, &pwd, buf, bufsize, &result)));
    if (result == NULL)
      {
	errno = status;
	mkcl_FElibc_file_error(env, file,
			       "mk_cl_file_author: Cannot get information for UID (~D).",
			       1, mkcl_make_unsigned_integer(env, filestatus.st_uid));
      }
    else
      output = mkcl_cstring_to_string(env, result->pw_name);
  }

  /* Beware that this is the file owner not its author. */
  mkcl_return_value(output);
#else /* MKCL_UNIX */
  /* If we want to duplicate the unix behavior on MS-Windows
     we could call GetFileSecurity() then do GetSecurityDescriptorOnwer()
     and finally LookupAccountSid to get the name of the file owner.
   */
  mkcl_return_value(mk_cl_Cnil);
#endif /* MKCL_UNIX */
}

mkcl_object
mkcl_homedir_pathname(MKCL, mkcl_object user)
{
  mkcl_object namestring;
  mkcl_OSstring_raw_type raw_os_home;
  mkcl_OSstring_raw_type raw_os_drive;

  if (!mkcl_Null(user))
    {
      mkcl_dynamic_extent_OSstring(env, os_user, user);
      mkcl_OSstring_raw_type raw_os_user = mkcl_OSstring_self(os_user);
      mkcl_index os_user_size = mkcl_OSstring_size(os_user);

      if (os_user_size > 0 && *raw_os_user == '~') {
	raw_os_user++;
	os_user_size--;
      }
      if (os_user_size == 0)
	return mkcl_homedir_pathname(env, mk_cl_Cnil);
      else
	{
#if MKCL_UNIX
	  struct passwd pwd;
	  struct passwd *result;
	  long bufsize = sysconf(_SC_GETPW_R_SIZE_MAX);
          
          if (bufsize <= -1) /* Has sysconf() failed to find the value of _SC_GETPW_R_SIZE_MAX? */
            bufsize = (32 * 1024); /* As a blind fallback we hope this will be large enough. */

          mkcl_ensure_call_stack(env, (bufsize * sizeof(char)) + sizeof(int));
	  {
	    char buf[bufsize]; /* a VLA. */
	    int status;

	    MKCL_LIBC_NO_INTR(env, status = getpwnam_r((char *) raw_os_user, &pwd, buf, bufsize, &result));
	    if (result == NULL)
	      {
		if (status)
		  {
		    errno = status;
		    mkcl_FElibc_error(env, "In mkcl_homedir_pathname(), failed on getpwnam_r()", 1, user);
		  }
		else
		  return mk_cl_Cnil;
	      }
	  }
	  namestring = mkcl_cstring_to_string(env, result->pw_dir);
#else /* MKCL_UNIX */
	  return mk_cl_Cnil;
#endif /* MKCL_UNIX */
	}
    }
  else
    {
      static const mkcl_base_string_object(home_string_obj, "HOME");

      namestring = mkcl_getenv(env, (mkcl_object)&home_string_obj);

      if (mkcl_Null(namestring))
	{
#if MKCL_WINDOWS
	  static const mkcl_base_string_object(homepath_string_obj, "HOMEPATH");
	  static const mkcl_base_string_object(homedrive_string_obj, "HOMEDRIVE");
	  mkcl_object homepath = mkcl_getenv(env, (mkcl_object)&homepath_string_obj);
	  mkcl_object homedrive = mkcl_getenv(env, (mkcl_object)&homedrive_string_obj);
	  
	  if (mkcl_Null(homepath) || mkcl_Null(homedrive))
	    namestring = mkcl_make_simple_base_string(env, "C:/");
	  else
	    {
	      namestring = mkcl_concatenate_2_strings(env, homedrive, homepath);
	      namestring = mkcl_coerce_to_adjustable_string(env, namestring);
	    }
	  
#else
	  namestring = mkcl_make_simple_base_string(env, "/");
#endif /*  MKCL_WINDOWS */
	}
    }

  if (mkcl_character_index_raw(env, namestring, 0) == '~')
    mkcl_FEerror(env, "Not a valid home pathname ~S", 1, namestring);

  {
    mkcl_character ch = mkcl_string_last(env, namestring);
    if (!MKCL_IS_DIR_SEPARATOR(ch))
      mkcl_string_push_extend(env, namestring, MKCL_DIR_SEPARATOR);
  }
  return mk_cl_parse_namestring(env, 3, namestring, mk_cl_Cnil, mk_cl_Cnil);
}

mkcl_object mk_cl_user_homedir_pathname(MKCL, mkcl_narg narg, ...)
{
  mkcl_call_stack_check(env);
  {
    mkcl_object host = mk_cl_Cnil;
    MKCL_RECEIVE_1_OPTIONAL_ARGUMENT(env, MK_CL_user_homedir_pathname, narg, 0, narg, &host);

    /* Ignore optional host argument. */
    mkcl_return_value(mkcl_homedir_pathname(env, mk_cl_Cnil));
  }
}

#if MKCL_UNIX
#define not_eq_ch(c1, c2) ((c1) != (c2))
#elif MKCL_WINDOWS
static inline bool not_eq_ch(wchar_t c1, wchar_t c2)
{
  return(towupper(c1) != towupper(c2)); /* towupper */
}
#endif /* MKCL_WINDOWS */


static bool
raw_string_match(mkcl_OSstring_raw_type s, mkcl_OSstring_raw_type p)
{ /* This code expects proper null termination of each raw string. JCB */
  while (*s) {
    switch (*p) {
    case '*':
      {
	/* Match any group of characters */
	mkcl_OSstring_raw_type next = p+1;
	if (*next != '?') {
	  if (*next == '\\')
	    next++;
	  while (*s && not_eq_ch(*s,*next)) s++;
	}
	if (raw_string_match(s,next))
	  return TRUE;
	/* starts back from the '*' */
	if (!*s)
	  return FALSE;
	s++;
      }
      break;
    case '?':
      /* Match any character */
      s++, p++;
      break;
    case '\\':
      /* Interpret a pattern character literally.
         Trailing slash is interpreted as a slash. */
      if (p[1]) p++;
      if (not_eq_ch(*s,*p))
	return FALSE;
      s++, p++;
      break;
    default:
      if (not_eq_ch(*s,*p))
	return FALSE;
      s++, p++;
      break;
    }
  }
  while (*p == '*')
    p++;
  return (*p == 0);
}

static bool
string_match(MKCL, mkcl_OSstring_raw_type s, mkcl_object mask)
{
  if (mask == MK_KEY_wild)
    return TRUE;
  else if (MKCL_STRINGP(mask))
    {
      mkcl_dynamic_extent_OSstring(env, os_mask, mask);
      mkcl_OSstring_raw_type p = mkcl_OSstring_self(os_mask);
      
      return raw_string_match(s, p);
    }
  else if (MKCL_OSSTRINGP(mask))
    {
      mkcl_OSstring_raw_type p = mkcl_OSstring_self(mask);
      
      return raw_string_match(s, p);
    }
  else
    return FALSE;
}

static bool
raw_string_is_wild_p(mkcl_OSstring_raw_type p)
{ /* This code expects proper null termination of raw string. JCB */
  for (; *p; p++)
    switch (*p)
      {
      case '*': return TRUE;
      case '?': return TRUE;
      case '\\': p++;
        if (*p == '\0')
          return FALSE; /* already at end, don't go any further. */
        break;
      default: break;
      }
  return FALSE;
}

bool mkcl_pathname_component_string_is_wild_p(MKCL, mkcl_object comp_str)
{
  if (MKCL_STRINGP(comp_str))
    {
      mkcl_dynamic_extent_OSstring(env, os_comp_str, comp_str);
      mkcl_OSstring_raw_type p = mkcl_OSstring_self(os_comp_str);

      return raw_string_is_wild_p(p);
    }
  else if (MKCL_OSSTRINGP(comp_str))
    {
      mkcl_OSstring_raw_type p = mkcl_OSstring_self(comp_str);

      return raw_string_is_wild_p(p);
    }
  else
    return FALSE;
}
/*
 * list_directory() lists the files and directories which are contained
 * in directory designated by 'path'. If ONLY_DIR is
 * true, the list is made of only the directories -- 
 * a propert which is checked by following the symlinks.
 */

#if MKCL_WINDOWS

static mkcl_object
list_directory(MKCL, struct OSpath * wd_path, mkcl_object mask, bool only_dir)
{
  mkcl_object out = mk_cl_Cnil;

  WIN32_FIND_DATAW fd;
  HANDLE hFind = NULL;
  BOOL found = FALSE;
  mkcl_object path_wild = mkcl_string_to_OSstring(env, mkcl_namestring(env, wd_path->pathname, FALSE));
  mkcl_UTF_16_object(star_obj, L"*");

  mkcl_OSstring_nconc(env, path_wild, (mkcl_object) &star_obj);

  MKCL_LIBC_NO_INTR(env, hFind = FindFirstFileW(mkcl_OSstring_self(path_wild), &fd));
  if (hFind == INVALID_HANDLE_VALUE) {
    return mk_cl_Cnil;
  }

  do
    {
      wchar_t * entry_text = fd.cFileName;

      if (entry_text[0] == L'.' &&
	  (entry_text[1] == L'\0' ||
	   (entry_text[1] == L'.' && entry_text[2] == L'\0')))
	goto CONTINUE_WITH_NEXT_ENTRY;
      if (!mkcl_Null(mask) && !string_match(env, entry_text, mask))
	goto CONTINUE_WITH_NEXT_ENTRY;
      if (only_dir && !(fd.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY))
	goto CONTINUE_WITH_NEXT_ENTRY;
      out = mkcl_cons(env, mkcl_cstring16_copy_to_OSstring(env, entry_text), out);
    CONTINUE_WITH_NEXT_ENTRY:
      MKCL_LIBC_NO_INTR(env, (found = FindNextFileW(hFind, &fd)));
    }
  while (found);

  if (GetLastError() != ERROR_NO_MORE_FILES)
    mkcl_FEwin32_file_error(env, wd_path->pathname, "list_directory failed on FindNextFile", 0);

  if (!FindClose(hFind))
    mkcl_FEwin32_file_error(env, wd_path->pathname, "list_directory failed on FindClose", 0);

  return mk_cl_nreverse(env, out);
}

#elif MKCL_UNIX

static mkcl_object
list_directory(MKCL, struct OSpath * wd_path, mkcl_object mask, bool only_dir)
{
  mkcl_object out = mk_cl_Cnil;
  struct dirent * entry;
  DIR *dir;
  int rc = 0;

  mkcl_object os_path = mkcl_string_to_OSstring(env, mkcl_namestring(env, wd_path->pathname, FALSE));

  if (!(mask == MK_KEY_wild || mkcl_pathname_component_string_is_wild_p(env, mask)))
    {
      struct stat buf;
      int rc;
      mkcl_object os_mask = mkcl_string_to_OSstring(env, mask); /* ??? */

      mkcl_OSstring_nconc(env, os_path, os_mask);
      MKCL_LIBC_NO_INTR(env, rc = stat((char *) mkcl_OSstring_self(os_path), &buf));
      if (rc)
        {
          switch (errno)
            {
            case ENOENT:
              return mk_cl_Cnil; /* ENOENT is expected but the others may need to be reported. */
            case EACCES: case ELOOP: case ENAMETOOLONG: case EOVERFLOW:
              return mk_cl_Cnil; /* something is wrong at the filesystem level. */
            case ENOTDIR: case ENOMEM: case EFAULT:
              return mk_cl_Cnil; /* something is very wrong with the OS. */
            default:
              return mk_cl_Cnil; /* Unexpected errno, something is wrong with the documentation of stat()? */
            }
        }
      else
        return mkcl_cons(env, os_mask, mk_cl_Cnil); /* return this single match. */
    }

  if (mkcl_OSstring_size(os_path) == 0)
    { /* 'path' is empty we assume we want the current directory. */
      mkcl_OSstring_push_extend(env, os_path, '.');
      mkcl_OSstring_push_extend(env, os_path, '/');
      MKCL_LIBC_NO_INTR(env, dir = opendir((char *) mkcl_OSstring_self(os_path)));
    }
  else
    { MKCL_LIBC_NO_INTR(env, dir = opendir((char *) mkcl_OSstring_self(os_path))); }
  if (dir == NULL) {
    /* opendir failed, errno should say why.
       One of EACCES, EBADF, EMFILE, ENFILE, ENOENT, ENOMEM, ENOTDIR.
     */
    mkcl_FElibc_file_error(env, wd_path->pathname, "list_directory failed on opendir: ~S", 1, os_path);
    return mk_cl_Cnil;
  }


  errno = 0;
  MKCL_LIBC_NO_INTR(env, entry = readdir(dir));
  rc = errno;
  while (rc == 0 && entry) {
    char * entry_text = entry->d_name;

    if (entry_text[0] == '.' &&
	(entry_text[1] == '\0' ||
	 (entry_text[1] == '.' && entry_text[2] == '\0')))
      goto CONTINUE_WITH_NEXT_ENTRY;
    if (!mkcl_Null(mask) && !string_match(env, (mkcl_OSstring_raw_type) entry_text, mask /*raw_os_mask*/))
      goto CONTINUE_WITH_NEXT_ENTRY;
    if (only_dir && entry->d_type != DT_DIR)
      {
	if (entry->d_type == DT_UNKNOWN || entry->d_type == DT_LNK)
	  {
	    const size_t entry_len = strlen(entry_text);
	    mkcl_object full_entry = mkcl_alloc_OSstring(env, mkcl_OSstring_size(os_path) + entry_len);
	    
	    mkcl_OSstring_nconc(env, full_entry, os_path);
	    mkcl_OSstring_nconc_cstring(env, full_entry, entry_text);
	    if (file_kind(env, full_entry, TRUE) != MK_KEY_directory)
	      goto CONTINUE_WITH_NEXT_ENTRY;
	  }
	else
	  goto CONTINUE_WITH_NEXT_ENTRY;
      }
    out = mkcl_cons(env, mkcl_cstring_copy_to_OSstring(env, entry_text), out);

  CONTINUE_WITH_NEXT_ENTRY:
    errno = 0;
    MKCL_LIBC_NO_INTR(env, entry = readdir(dir));
    rc = errno;
  }
  if (rc != 0)
    mkcl_FElibc_file_error(env, wd_path->pathname, "list_directory failed on readdir", 0);

  if (closedir(dir))
    mkcl_FElibc_file_error(env, wd_path->pathname, "list_directory failed on closedir", 0);

  return mkcl_nreverse_proper_list(env, out);
}

#endif /* elif MKCL_UNIX */

static mkcl_object list_all_in_directory(MKCL, struct OSpath * wd_path, mkcl_object mask)
{
  return list_directory(env, wd_path, mask, FALSE);
}

static mkcl_object list_directories_in_directory(MKCL, struct OSpath * wd_path, mkcl_object mask)
{
  return list_directory(env, wd_path, mask, TRUE);
}

static bool is_dot_pathname_p(MKCL, mkcl_object pathname)
{
  mkcl_object name = pathname->pathname.name;
  mkcl_object type = pathname->pathname.type;
  
  if (mkcl_Null(type)
      && !mkcl_Null(name)
      && name->base_string.fillp == 1
      && name->base_string.self[0] == '.')
    return true;

  return false;
}


static mkcl_object dir_real_pathname(MKCL, struct OSpath * wd_path)
{
  /* char * cur_dir = "./"; */
  mkcl_object real_path = safe_realpath(env, wd_path->pathname, mkcl_namestring(env, wd_path->pathname, FALSE));

  if (mkcl_OSstring_last(env, real_path) != MKCL_DIR_SEPARATOR)
    mkcl_OSstring_push_extend(env, real_path, MKCL_DIR_SEPARATOR);

  return mk_cl_pathname(env, mkcl_OSstring_to_string(env, real_path));
}

/*
 * dir_files() lists all files which are contained in the current directory and
 * which match the masks in PATHSPEC. This routine is essentially a wrapper for
 * list_directory(), which transforms the list of strings into a list
 * of pathnames. BASEDIR is the real name of the current directory and it is
 * used to build these pathnames.
 */
static mkcl_object
dir_files(MKCL, bool follow_symlinks, struct OSpath * wd_path, mkcl_object pathspec)
{
  mkcl_object all_files, output = mk_cl_Cnil;
  mkcl_object mask;
  mkcl_object name = pathspec->pathname.name;
  mkcl_object type = pathspec->pathname.type;
  mkcl_object basedir = dir_real_pathname(env, wd_path);

  if (name == mk_cl_Cnil && type == mk_cl_Cnil)
    {
      mkcl_object wd_path_namestring = mkcl_namestring(env, wd_path->pathname, FALSE);
      mkcl_dynamic_extent_OSstring(env, wd_path_os_namestring, wd_path_namestring);
      mkcl_object kind = file_kind(env, wd_path_os_namestring, FALSE);

      if (mkcl_Null(kind))
	{
#if MKCL_WINDOWS
	  mkcl_FEwin32_file_error(env, wd_path_os_namestring, "GetFileAttributes() failed in file_kind()", 0);
#else
	  mkcl_FElibc_file_error(env, wd_path_os_namestring, "stat() failed in file_kind()", 0);
#endif
	}
      else if (!follow_symlinks && MK_KEY_link == kind) /* This branch seems to be never taken! JCB */
	{
	  struct OSpath wd2_path;
	  copy_OSpath(env, &wd2_path, wd_path);
	  mkcl_object name = OSpath_pop_dir(env, &wd2_path);

	  mkcl_object true_pathname 
	    = mk_cl_make_pathname(env, 4,
				  MK_KEY_name, name,
				  MK_KEY_defaults, mk_cl_truename(env, wd2_path.pathname));

	  mkcl_object true_os_namestring = mkcl_string_to_OSstring(env, mkcl_namestring(env, true_pathname, TRUE));

	  if (MK_KEY_directory == file_kind(env, true_os_namestring, TRUE))
	    {
	      mkcl_OSstring_push_extend(env, true_os_namestring, MKCL_DIR_SEPARATOR);
	      return mk_cl_list(env, 1, mk_cl_pathname(env, mkcl_OSstring_to_string(env, true_os_namestring)));
	    }
	  else
	    return mk_cl_list(env, 1, true_pathname);
	}
      else 
	return mk_cl_list(env, 1, basedir);

    }
  if (is_dot_pathname_p(env, pathspec))
    {
      return mk_cl_list(env, 1, basedir);
    }

  if (name == MK_KEY_wild || name == MK_KEY_unspecific)
    mask = mkcl_cstring_copy_to_OSstring(env, "*");
  else if (mkcl_stringp(env, name))
    mask = mkcl_string_to_OSstring(env, name);
  else
    mask = mkcl_alloc_OSstring(env, 5);
  if (type == MK_KEY_wild || type == MK_KEY_unspecific)
    {
      if (!(name == MK_KEY_wild || name == MK_KEY_unspecific))
	{
	  mkcl_OSstring_push_extend(env, mask, '.');
	  mkcl_OSstring_push_extend(env, mask, '*');
	}
    }
  else if (mkcl_stringp(env, type))
    {
      mkcl_dynamic_extent_OSstring(env, os_type, type);
      mkcl_OSstring_push_extend(env, mask, '.');
      mkcl_OSstring_nconc(env, mask, os_type);
    }

  all_files = list_all_in_directory(env, wd_path, mask);
  mkcl_loop_for_in(env, all_files) {
    mkcl_object new_os_filename = MKCL_CAR(all_files);
    mkcl_object new_filename = mkcl_OSstring_to_string(env, new_os_filename);
    mkcl_object new_pathname;
    {
      mkcl_index s = 0, e = mkcl_string_length(env, new_filename), ep;
      new_pathname = mkcl_parse_namestring(env, new_filename, s, e, &ep, mkcl_core.localhost_string, mkcl_specific_namestring);
    }

    new_pathname->pathname.host = basedir->pathname.host;
    new_pathname->pathname.device = basedir->pathname.device;
    new_pathname->pathname.directory = basedir->pathname.directory;
    {
      mkcl_object os_namestring = mkcl_string_to_OSstring(env, mkcl_namestring(env, new_pathname, TRUE));
      
#ifdef HAVE_LSTAT
      /* Resolve symbolic links */
      while (follow_symlinks && file_kind(env, os_namestring, FALSE) == MK_KEY_link) 
	{
	  new_pathname = mk_cl_truename(env, new_pathname);
	  os_namestring = mkcl_string_to_OSstring(env, mkcl_namestring(env, new_pathname, TRUE));
	} 
#endif
      if (MK_KEY_directory == file_kind(env, os_namestring, follow_symlinks))
	{
	  mkcl_OSstring_push_extend(env, os_namestring, MKCL_DIR_SEPARATOR);
	  new_pathname = mk_cl_pathname(env, mkcl_OSstring_to_string(env, os_namestring));
	}
    }
    output = MKCL_CONS(env, new_pathname, output);
  } mkcl_end_loop_for_in;
  return output;
}

/*
 * dir_recursive() performs the dirty job of DIRECTORY. The routine moves
 * through the filesystem looking for files and directories which match
 * the masks in the arguments PATHSPEC and DIRECTORY, collecting them in a
 * list.
 */

static mkcl_object
dir_recursive(MKCL, bool follow_symlinks, struct OSpath * wd_path, mkcl_object pathspec, mkcl_object dir_spec)
{
  mkcl_object item, next_dirs, output = mk_cl_Cnil;

  /* There are several possibilities here:
   *
   * 1) The list of subdirectories DIR_SPEC is empty, and only PATHSPEC
   * remains to be inspected. If there is no file name or type, then
   * we simply output the truename of the current directory. Otherwise
   * we have to find a file which corresponds to the description.
   */
  if (dir_spec == mk_cl_Cnil) {
    return dir_files(env, follow_symlinks, wd_path, pathspec);
  }
  /*
   * 2) We have not yet exhausted the DIR_SPEC component of the
   * pathspec. We have to enter some subdirectory, determined by
   * MKCL_CAR(DIR_SPEC) and scan it.
   */
  item = MKCL_CAR(dir_spec);

  if (MKCL_STRINGP(item) || item == MK_KEY_wild) {
    /*
     * 2.1) If MKCL_CAR(DIR_SPEC) is a string or :WILD, we have to
     * enter & scan all subdirectories in our curent directory.
     */
    next_dirs = list_directories_in_directory(env, wd_path, item);
    mkcl_loop_for_in(env, next_dirs) {
      struct OSpath new_wd_path;
      mkcl_object os_dir = MKCL_CAR(next_dirs);
      mkcl_object dir = mkcl_OSstring_to_string(env, os_dir);

      copy_OSpath(env, &new_wd_path, wd_path);
      OSpath_push_dir(env, &new_wd_path, dir);

      item = dir_recursive(env, follow_symlinks, &new_wd_path, pathspec, MKCL_CDR(dir_spec));
      output = mkcl_nconc(env, item, output);
    } mkcl_end_loop_for_in;
  } else if (item == MK_KEY_up) {
    /*
     * 2.4) If MKCL_CAR(DIR_SPEC) is :UP, we have to scan the directory
     * which contains this one.
     */

    OSpath_push_dir(env, wd_path, MK_KEY_up);
    output = dir_recursive(env, follow_symlinks, wd_path, pathspec, MKCL_CDR(dir_spec));
  } else if (item == MK_KEY_back) {
    OSpath_pop_dir(env, wd_path);
    output = dir_recursive(env, follow_symlinks, wd_path, pathspec, MKCL_CDR(dir_spec));
  } else if (item == MK_KEY_wild_inferiors) {
    /*
     * 2.5) If MKCL_CAR(DIR_SPEC) is :WILD-INFERIORS, we have to do
     * scan all subdirectories from _all_ levels, looking for a
     * tree that matches the remaining part of DIR_SPEC.
     */
    next_dirs = list_directories_in_directory(env, wd_path, MK_KEY_wild);
    mkcl_loop_for_in(env, next_dirs) {
      struct OSpath new_wd_path;
      mkcl_object os_dir = MKCL_CAR(next_dirs);
      mkcl_object dir = mkcl_OSstring_to_string(env, os_dir);

      copy_OSpath(env, &new_wd_path, wd_path);
      OSpath_push_dir(env, &new_wd_path, dir);

      item = dir_recursive(env, follow_symlinks, &new_wd_path, pathspec, dir_spec);
      output = mkcl_nconc(env, item, output);
    } mkcl_end_loop_for_in;
    output = mkcl_nconc(env, output, dir_recursive(env, follow_symlinks, wd_path, pathspec, MKCL_CDR(dir_spec)));
  } else {
    mk_cl_error(env, 3, MK_CL_file_error, MK_KEY_pathname, pathspec);
  }
  return output;
}

mkcl_object mk_cl_directory(MKCL, mkcl_narg narg, mkcl_object pathspec, ...)
{
  mkcl_call_stack_check(env);
  {
    mkcl_object follow_symlinks = mk_cl_Cnil;
    MKCL_RECEIVE_1_KEYWORD_ARGUMENT(env, MK_CL_directory, narg, 1, pathspec, MK_KEY_follow_symlinks, &follow_symlinks);
    {
      mkcl_object path_spec = mkcl_coerce_to_file_pathname(env, pathspec);
      mkcl_object dir_spec = path_spec->pathname.directory;
      /* mkcl_object path = init_OSpath(env) */;
      struct OSpath wd_path;

      if (MKCL_PATHNAMEP(pathspec)
          && (mkcl_Null(pathspec->pathname.version) || pathspec->pathname.version == MK_KEY_unspecific))
        path_spec->pathname.version = MK_KEY_wild;

      if (MKCL_CAR(dir_spec) == MK_KEY_absolute)
        init_absolute_OSpath(env, &wd_path);
      else /* if (MKCL_CAR(dir_spec) == MK_KEY_relative) */
        init_relative_OSpath(env, &wd_path);

      wd_path.pathname->pathname.host = path_spec->pathname.host;
      wd_path.pathname->pathname.device = path_spec->pathname.device;

      mkcl_object output = dir_recursive(env, !mkcl_Null(follow_symlinks), &wd_path, path_spec, MKCL_CDR(dir_spec));
      mkcl_return_value(output);
    }
  }
}

mkcl_object mk_mkcl_getcwd(MKCL, mkcl_narg narg, ...)
{
  mkcl_call_stack_check(env);
  {
    mkcl_object output;
    mkcl_object change_default_pathname_defaults = mk_cl_Cnil;
    mkcl_object all_drives = mk_cl_Cnil;
  
    MKCL_RECEIVE_2_KEYWORD_ARGUMENTS(env, MK_MKCL_getcwd, narg, 0, narg, MK_KEY_change_default_pathname_defaults, &change_default_pathname_defaults, MK_KEY_all_drives, &all_drives);

    output = mk_cl_parse_namestring(env, 3, current_dir(env), mk_cl_Cnil, mk_cl_Cnil);
    if (!mkcl_Null(change_default_pathname_defaults)) {
      MKCL_SETQ(env, MK_CL_DYNVAR_default_pathname_defaults, output);
    }

#if MKCL_WINDOWS
    {
      mkcl_object all_cwd = mk_cl_Cnil;
      ULONG uDriveMask = _getdrives();
      int drive = 1;

      while (uDriveMask)
        {
          if (uDriveMask & 1)
            {
              if (!mkcl_Null(all_drives) || drive > 2) /* Skip floppies. */
                {
                  mkcl_object cwd = current_dir_on_drive(env, drive);
	      
                  cwd = mk_cl_parse_namestring(env, 3, cwd, mk_cl_Cnil, mk_cl_Cnil);
                  all_cwd = MKCL_CONS(env, cwd, all_cwd);
                }
            }
          drive++;
          uDriveMask >>= 1;
        }


      mkcl_return_2_values(output, mk_cl_nreverse(env, all_cwd));
    }
#else /*  MKCL_WINDOWS */
    mkcl_return_value(output);
#endif /*  MKCL_WINDOWS */
  }
}

static void warn_early(MKCL, char * msg)
{
  fprintf(stderr, "\nMKCL warning: %s\n", msg);
  fflush(stderr);
}

struct mkcl_cfun mk_si_get_SYS_library_pathname_cfunobj = MKCL_CFUN0(mk_si_get_SYS_library_pathname, MK_SI_get_SYS_library_pathname);

mkcl_object
mk_si_get_SYS_library_pathname(MKCL)
{
  mkcl_object SYS_libdir = mkcl_core.SYS_library_pathname;

  mkcl_call_stack_check(env);
  if (mkcl_Null(SYS_libdir))
    {
      static const mkcl_base_string_object(SYS_libdir_string_obj, "MKCL_SYS_LIBDIR");
      mkcl_object SYS_libdir_pathstring;
      static const mkcl_base_string_object(libdir_string_obj, "MKCL_LIBDIR"); /* deprecated in favor of MKCL_SYS_LIBDIR */
      mkcl_object libdir_pathstring;

      if (!mkcl_Null(SYS_libdir_pathstring = mkcl_getenv(env, (mkcl_object)&SYS_libdir_string_obj)))
	{
	  mkcl_character ch = mkcl_string_last(env, SYS_libdir_pathstring);

	  if (!MKCL_IS_DIR_SEPARATOR(ch))
	    mkcl_string_push_extend(env, SYS_libdir_pathstring, MKCL_DIR_SEPARATOR);
	  SYS_libdir = mk_cl_parse_namestring(env, 1, SYS_libdir_pathstring);
	}
      else if (!mkcl_Null(libdir_pathstring = mkcl_getenv(env, (mkcl_object)&libdir_string_obj)))
	{
	  mkcl_character ch = mkcl_string_last(env, libdir_pathstring);

	  if (!MKCL_IS_DIR_SEPARATOR(ch))
	    mkcl_string_push_extend(env, libdir_pathstring, MKCL_DIR_SEPARATOR);
	  SYS_libdir = mk_cl_parse_namestring(env, 1, libdir_pathstring);
	}
      else
	{
	  mkcl_object lib_dir_tip = mkcl_make_simple_base_string(env, "../lib/mkcl-" MKCL_VERSION_STRING "/");
	  mkcl_object lib_dir_root = mk_cl_make_pathname(env, 8,
							 MK_KEY_name, mk_cl_Cnil,
							 MK_KEY_type, mk_cl_Cnil,
							 MK_KEY_version, mk_cl_Cnil,
							 MK_KEY_defaults, mkcl_core.self_truename);
	  mkcl_object lib_pathname = mk_cl_merge_pathnames(env, 2, lib_dir_tip, lib_dir_root);

	  if (mkcl_Null(mk_mkcl_probe_file_p(env, lib_pathname)))
	    {
	      int i;
#if MKCL_WINDOWS
	      char os_dir0[] = "c:\\Program Files\\MKCL " MKCL_VERSION_STRING "\\lib\\mkcl-" MKCL_VERSION_STRING "\\";
	      char os_dir1[] = "c:\\Program Files\\MKCL " MKCL_MAJOR_VERSION_STRING "." MKCL_MINOR_VERSION_STRING
		                 "\\lib\\mkcl-" MKCL_VERSION_STRING "\\";
#else
	      char os_dir0[] = "/usr/local/lib/mkcl-" MKCL_VERSION_STRING "/";
	      char os_dir1[] = "/usr/lib/mkcl-" MKCL_VERSION_STRING "/";
#endif
	      mkcl_base_string_object(dir0_obj, os_dir0);
	      mkcl_base_string_object(dir1_obj, os_dir1);
	      mkcl_object dirnames[] = { (mkcl_object) &dir0_obj, (mkcl_object) &dir1_obj };

	      for (i = 0; i < MKCL_NB_ELEMS(dirnames); i++)
		if (!mkcl_Null(mk_mkcl_probe_file_p(env, dirnames[i])))
		  { SYS_libdir = mk_cl_pathname(env, dirnames[i]); break;}

	      if (mkcl_Null(SYS_libdir))
		{
		  mkcl_object default_SYS_libdir_pathstring = mkcl_cstring_to_string(env, MKCL_SYS_LIBDIR_DEFAULT "/");
		  SYS_libdir = mk_cl_parse_namestring(env, 1, default_SYS_libdir_pathstring);
		}
	    }
	  else
	    SYS_libdir = lib_pathname;
	}

      mkcl_core.SYS_library_pathname = SYS_libdir;
    }
  mkcl_return_value(SYS_libdir);
}

mkcl_object mk_mkcl_chdir(MKCL, mkcl_narg narg, mkcl_object directory, ...)
{
  mkcl_call_stack_check(env);
  {
  mkcl_object previous = mk_mkcl_getcwd(env, 0);
  mkcl_object namestring;
    mkcl_object change_d_p_d = mk_cl_Cnil;
    MKCL_RECEIVE_1_OPTIONAL_ARGUMENT(env, MK_MKCL_chdir, narg, 1, directory, &change_d_p_d);

    /* This will fail if the new directory does not exist */
    directory = mk_cl_pathname(env, directory);
    directory = mkcl_coerce_to_physical_pathname(env, directory);
    namestring = mkcl_namestring(env, directory, TRUE);
    if (namestring == mk_cl_Cnil) {
      mkcl_FEerror(env, "Pathname ~A does not have a physical namestring", 1, directory);
    }
    if (safe_chdir(env, namestring) == -1)
      mkcl_FElibc_file_error(env, namestring, "Can't change the current directory to ~A", 1, namestring);
    if (change_d_p_d != mk_cl_Cnil)
      MKCL_SETQ(env, MK_CL_DYNVAR_default_pathname_defaults, directory);
    mkcl_return_2_values(mk_mkcl_getcwd(env, 0), previous);
  }
}

mkcl_object
mk_mkcl_mkdir(MKCL, mkcl_object directory, mkcl_object mode)
{
  int ok;
  mkcl_call_stack_check(env);
  mkcl_object filename = mk_si_coerce_to_filename(env, directory);
  mkcl_dynamic_extent_OSstring(env, os_filename, filename);
#if MKCL_UNIX
  mkcl_index modeint = mkcl_fixnum_in_range(env, MK_MKCL_mkdir, "mode", mode, 0, 0777);
#endif

#if MKCL_WINDOWS
  MKCL_LIBC_NO_INTR(env, ok = _wmkdir(mkcl_OSstring_self(os_filename)));
#else
  MKCL_LIBC_NO_INTR(env, ok = mkdir((char *) mkcl_OSstring_self(os_filename), modeint));
#endif

  if (ok < 0)
    mkcl_FElibc_file_error(env, filename, "Could not create directory.", 0);
  mkcl_return_value(filename);
}

/* For #'si:mkstemp we follow the same keyword argument defaults as #'cl:open. */
mkcl_object mk_mkcl_mkstemp(MKCL, mkcl_narg narg, mkcl_object template, ...)
{
  mkcl_call_stack_check(env);
  {
    mkcl_object element_type = MK_CL_base_char;
    mkcl_object external_format = MK_KEY_default;
    MKCL_RECEIVE_2_KEYWORD_ARGUMENTS(env, MK_MKCL_mkstemp, narg, 1, template, MK_KEY_element_type, &element_type, MK_KEY_external_format, &external_format);

    {
      mkcl_object tmp_stream;
      mkcl_object tmp_filename;

#if MKCL_WINDOWS
      mkcl_object phys, dir, file;
      mkcl_char16 strTempDir[MAX_PATH];
      mkcl_char16 strTempFileName[MAX_PATH];
      /* char *s; */
      UINT unique_val = 0;
      bool first_try = TRUE;

      phys = mk_cl_translate_logical_pathname(env, 1, template);
      dir = mk_cl_make_pathname(env, 8,
                                MK_KEY_name, mk_cl_Cnil,
                                MK_KEY_type, mk_cl_Cnil,
                                MK_KEY_version, mk_cl_Cnil,
                                MK_KEY_defaults, phys);
      dir = mk_si_coerce_to_filename(env, dir);
      file = mk_cl_file_namestring(env, phys);
	
      {
        mkcl_dynamic_extent_OSstring(env, os_file, file);
        mkcl_dynamic_extent_OSstring(env, os_dir, dir);

      AGAIN:
        if (mkcl_OSstring_size(os_dir) <= (MAX_PATH - 14))
          MKCL_LIBC_NO_INTR(env, unique_val = GetTempFileNameW(mkcl_OSstring_self(os_dir),
                                                               mkcl_OSstring_self(os_file),
                                                               0,
                                                               strTempFileName));
        if (unique_val == 0) {
          tmp_stream = mk_cl_Cnil;
        } else {
          tmp_filename = mkcl_cstring16_to_string(env, strTempFileName);

          /* Yes, there is a gaping hole between name creation and file opening, but there does not
             seem to be any way provided by Microsoft to make this atomic. */
          int fd; /* This one should give better performance */

          MKCL_LIBC_NO_INTR(env, fd = _wopen(strTempFileName,
                                             _O_RDWR | _O_CREAT | /*_O_EXCL | */ _O_TRUNC | _O_BINARY | _O_SHORT_LIVED,
                                             _S_IREAD | _S_IWRITE));

          if (fd == -1)
            if ((errno == EEXIST) && first_try)
              { first_try = FALSE; goto AGAIN; }
            else
              { mkcl_return_3_values(mk_cl_Cnil, mk_cl_Cnil, MKCL_MAKE_FIXNUM(errno)); }

          tmp_stream = mkcl_make_stream_from_fd(env, tmp_filename, fd, mkcl_smm_io, element_type, external_format);
        }
      }
#else /*  MKCL_WINDOWS */
      int fd;
      int i;

      template = mk_si_coerce_to_filename(env, template);

      mkcl_object os_template = mkcl_string_to_OSstring(env, template);

      for (i = 0; i < 6; i++)
        mkcl_OSstring_push_extend(env, os_template, 'X'); /* add "XXXXXX" to the end of the template. */

      MKCL_LIBC_NO_INTR(env, fd = mkstemp((char *) mkcl_OSstring_self(os_template)));

      if (fd == -1) {
        mkcl_return_3_values(mk_cl_Cnil, mk_cl_Cnil, MKCL_MAKE_FIXNUM(errno));
      } else {
        tmp_filename = mkcl_OSstring_to_string(env, os_template);
        tmp_stream = mkcl_make_stream_from_fd(env, tmp_filename, fd, mkcl_smm_io, element_type, external_format);
      }
#endif /* else  MKCL_WINDOWS */

      mkcl_return_2_values(tmp_stream, tmp_filename);
    }
  }
}

mkcl_object
mk_mkcl_rmdir(MKCL, mkcl_object directory)
{
  int code;

  mkcl_call_stack_check(env);
  directory = mk_si_coerce_to_filename(env, directory);
  mkcl_dynamic_extent_OSstring(env, os_directory, directory);

#if MKCL_WINDOWS
  MKCL_LIBC_NO_INTR(env, code = _wrmdir(mkcl_OSstring_self(os_directory)));
#else
  MKCL_LIBC_NO_INTR(env, code = rmdir((char *) mkcl_OSstring_self(os_directory)));
#endif
  if (code != 0)
    mkcl_FElibc_file_error(env, directory, "Can't remove directory: ~S.", 1, directory);
  mkcl_return_value(mk_cl_Cnil);
}

mkcl_object
mk_mkcl_copy_file(MKCL, mkcl_object orig, mkcl_object dest)
{
  mkcl_interrupt_status old_intr;
  bool must_close_in = FALSE;
  bool must_close_out = FALSE;
  FILE * in = NULL, * out = NULL;

  mkcl_call_stack_check(env);
  mkcl_get_interrupt_status(env, &old_intr);
  mkcl_disable_interrupts(env);

  if (mkcl_type_of(orig) == mkcl_t_stream && !orig->stream.closed)
    {
      if (orig->stream.mode == mkcl_smm_io || orig->stream.mode == mkcl_smm_input)
	in = MKCL_IO_STREAM_FILE(orig);
      else if (orig->stream.mode == mkcl_smm_io_file || orig->stream.mode == mkcl_smm_input_file)
	{
	  int new_in_fd = dup(MKCL_IO_FILE_DESCRIPTOR(orig));

	  if ((new_in_fd == -1) || ((in = fdopen(new_in_fd, "rb")) == NULL)) /* we should dup() and setbuf() */
	    {
	      mkcl_set_interrupt_status(env, &old_intr);
	      mkcl_FElibc_file_error(env, orig, "In si::copy-file, failed to open for reading stream ~S.", 1, orig);
	    }
	  else
	    {
	      setvbuf(in, NULL, _IONBF, 0);
	      must_close_in = TRUE;
	    }
	}
    }
  if (in == NULL)
    {
      orig = mk_si_coerce_to_filename(env, orig);
      {
	mkcl_dynamic_extent_OSstring(env, os_orig, orig);
    
#if MKCL_WINDOWS
	in = _wfopen(mkcl_OSstring_self(os_orig), L"rbS");
#else
	in = fopen((char *) mkcl_OSstring_self(os_orig), "rb");
#endif
      }
      if (in == NULL)
	{
	  mkcl_set_interrupt_status(env, &old_intr);
	  mkcl_FElibc_file_error(env, orig, "In si::copy-file, failed to open for reading file ~S.", 1, orig);
	}
      else
	{
	  setvbuf(in, NULL, _IONBF, 0); /* the buffer would be in the way of our large block seqential read scan. */
	  must_close_in = TRUE;
	}
    }

  if (mkcl_type_of(dest) == mkcl_t_stream && !dest->stream.closed)
    {
      if (dest->stream.mode == mkcl_smm_io || dest->stream.mode == mkcl_smm_output)
	out = MKCL_IO_STREAM_FILE(dest);
      else if (dest->stream.mode == mkcl_smm_io_file || dest->stream.mode == mkcl_smm_output_file)
	{
	  int new_out_fd = dup(MKCL_IO_FILE_DESCRIPTOR(dest));

	  if ((new_out_fd == -1) || ((out = fdopen(new_out_fd, "wb")) == NULL))
	    {
	      mkcl_set_interrupt_status(env, &old_intr);
	      mkcl_FElibc_file_error(env, dest, "In si::copy-file, failed to open for reading stream ~S.", 1, dest);
	    }
	  else
	    {
	      setvbuf(out, NULL, _IONBF, 0); /* the buffer would be in the way of our large block seqential writes. */
	      must_close_out = TRUE;
	    }
	}
    }
  
  if (out == NULL)
    {
      dest = mk_si_coerce_to_filename(env, dest);
      {
	mkcl_dynamic_extent_OSstring(env, os_dest, dest);
	
#if MKCL_WINDOWS
	out = _wfopen(mkcl_OSstring_self(os_dest), L"wbS");
#else
	out = fopen((char *) mkcl_OSstring_self(os_dest), "wb");
#endif
      }
      if (out == NULL)
	{
	  int my_errno = errno;

	  if (must_close_in) mkcl_safe_fclose(env, in, orig);
	  mkcl_set_interrupt_status(env, &old_intr);
	  errno = my_errno;
	  mkcl_FElibc_file_error(env, dest, "In si::copy-file, failed to open for writing file ~S.", 1, dest);
	}
      else
	{
	  setvbuf(out, NULL, _IONBF, 0); /* the buffer would be in the way of our large block seqential writes. */
	  must_close_out = TRUE;
	}
    }
  
  {
    const size_t buffer_size = 64 * 1024; /* 64k is 8 times the usual cluster size (8k), yet unlikely to blow up the stack. */
    unsigned char buffer[buffer_size]; /* VLA */
    size_t out_size;
    do {
      size_t in_size = buffer_size;
      unsigned char * in_base = buffer;
      unsigned char * out_base = buffer;

      while ( in_size && !ferror(in) && !feof(in) )
	{
	  size_t size = fread(in_base, 1, in_size, in);

	  if (size < in_size && ferror(in) && errno == EINTR)
	    clearerr(in);
	  in_base += size;
	  in_size -= size;
	}

      out_size = buffer_size - in_size;
      while ( out_size && !ferror(out) )
	{
	  size_t size = fwrite(out_base, 1, out_size, out);

	  if (size < out_size && ferror(out) && errno == EINTR)
	    clearerr(out);
	  out_base += size;
	  out_size -= size;
	}
    } while ( !ferror(in) && !feof(in) && !out_size && !ferror(out) );
      
    {
      int error_on_in = ferror(in);
      int error_on_out = ferror(out) || out_size;
      int my_errno = errno;

      mkcl_set_interrupt_status(env, &old_intr);
      if (must_close_out) mkcl_safe_fclose(env, out, dest);
      if (must_close_in) mkcl_safe_fclose(env, in, orig);

      errno = my_errno;
      if (error_on_in)
	mkcl_FElibc_file_error(env, orig, "In si::copy-file, error while copying from ~S to ~S.", 2, orig, dest);
      else if (error_on_out)
	mkcl_FElibc_file_error(env, dest, "In si::copy-file, error while copying from ~S to ~S.", 2, orig, dest);
    }
  }

  mkcl_return_value(mk_cl_Ct);
}


