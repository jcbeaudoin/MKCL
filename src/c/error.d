/* -*- mode: c -*- */
/*
    error.c -- Error handling.
*/
/*
    Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
    Copyright (c) 1990, Giuseppe Attardi.
    Copyright (c) 2001, Juan Jose Garcia Ripoll.
    Copyright (c) 2010-2012, Jean-Claude Beaudoin.

    MKCL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 3 of the License, or (at your option) any later version.

    See file '../../Copyright' for full details.
*/

#include <mkcl/mkcl.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <signal.h>
#include <stdlib.h>
#if defined(MKCL_WINDOWS)
#include <windows.h>
#endif
#include <mkcl/internal.h>

void
mkcl_internal_C_error(MKCL, const char * const s, const char * const file, const int lineno)
{
  int mkcl_saved_errno = errno;

  mkcl_object thread = env->own_thread;

  if (thread)
    fprintf(stderr, "\nInternal error: (%s, %d) in [%s] %s.\n",
	    file, lineno, thread->thread.name->base_string.self, s); /* Unicode? JCB */

  if (mkcl_saved_errno)
    {
#ifdef MKCL_WINDOWS
#if 0
      wchar_t * _wcserror(int);  /* Do we need to go to __MSVCRT_VERSION__ >= 0x0700 ? */
      wchar_t * error_msg = _wcserror(mkcl_saved_errno);

      fwprintf(stderr, L"  [%d: %s]\n", mkcl_saved_errno, error_msg);
#else
      char * error_msg = strerror(mkcl_saved_errno);

      fprintf(stderr, "  [%d: %s]\n", mkcl_saved_errno, error_msg);
#endif
#else
      char * error_msg;
      char buf[2048];

# if _GNU_SOURCE
      error_msg = strerror_r(mkcl_saved_errno, buf, sizeof(buf)); /* GNU-specific version */
# else
      if (strerror_r(mkcl_saved_errno, buf, sizeof(buf))) buf[0] = '\0';
      error_msg = buf;
# endif
      fprintf(stderr, "  [%d: %s]\n", mkcl_saved_errno, error_msg);
#endif
    }

  fflush(stderr); /* This should be redundant, but just in case... JCB */
  if (mkcl_early_boot)
    mkcl_longjmp(mkcl_early_boot_error_handler, 1);
  else
    mk_mt_abandon_thread(env, @':aborted');
}

void mkcl_internal_error(MKCL, const char * const s, const char * const file, const int lineno)
{
  errno = 0;
  mkcl_internal_C_error(env, s, file, lineno);
}

/*****************************************************************************/
/*		Support for Lisp Error Handler				     */
/*****************************************************************************/

void
mkcl_FEerror(MKCL, const char *s, int narg, ...)
{
  mkcl_va_list args;
  mkcl_va_start(env, args, narg, narg, 0);
  mkcl_funcall3(env, @+'si::universal-error-handler',
		mk_cl_Cnil,                    /*  not correctable  */
		mkcl_make_simple_base_string(env, (char *) s),	 /*  condition text  */
		mkcl_grab_rest_args(env, args, FALSE));
  mkcl_lose(env, "Should not have returned from universal-error-handler");
}

mkcl_object
mkcl_CEerror(MKCL, mkcl_object c, const char *err, int narg, ...)
{
  mkcl_va_list args;
  mkcl_va_start(env, args, narg, narg, 0);
  return mkcl_funcall3(env, @+'si::universal-error-handler',
		       c,			/*  correctable  */
		       mkcl_make_simple_base_string(env, (char *) err), /* continue-format-string  */
		       mkcl_grab_rest_args(env, args, FALSE));
}

/***********************
 * Conditions signaler *
 ***********************/

void
mkcl_FEprogram_error(MKCL, const char *s, int narg, ...)
{
  mkcl_object real_args, text;
  mkcl_va_list args;
  mkcl_va_start(env, args, narg, narg, 0);
  text = mkcl_make_simple_base_string(env, (char *) s);
  real_args = mkcl_grab_rest_args(env, args, FALSE);
  if (mk_cl_boundp(env, @'si::*current-form*') != mk_cl_Cnil) {
    /* When mkcl_FEprogram_error is invoked from the compiler, we can
     * provide information about the offending form.
     */
    mkcl_object stmt = mkcl_symbol_value(env, @'si::*current-form*');
    if (stmt != mk_cl_Cnil) {
      real_args = @list(env, 3, stmt, text, real_args);
      text = mkcl_make_simple_base_string(env, "In form~%~S~%~?");
    }
  }
  mk_cl_error(env, 5, @'si::simple-program-error', @':format-control', text, @':format-arguments', real_args);
  mkcl_lose(env, "Returned from mk_cl_error()");
}

void
mkcl_FEcontrol_error(MKCL, const char *s, int narg, ...)
{
  mkcl_va_list args;
  mkcl_va_start(env, args, narg, narg, 0);
  mk_cl_error(env, 5,
	      @'si::simple-control-error',
	      @':format-control', mkcl_make_simple_base_string(env, (char *) s),
	      @':format-arguments', mkcl_grab_rest_args(env, args, FALSE));
  mkcl_lose(env, "Returned from mk_cl_error()");
}

void
mkcl_FEreader_error(MKCL, const char *s, mkcl_object stream, int narg, ...)
{
  mkcl_va_list args;
  mkcl_va_start(env, args, narg, narg, 0);
  mk_cl_error(env, 7,
	      @'si::simple-reader-error',
	      @':format-control', mkcl_make_simple_base_string(env, (char *) s),
	      @':format-arguments', mkcl_grab_rest_args(env, args, FALSE),
	      @':stream', stream);
  mkcl_lose(env, "Returned from mk_cl_error()");
}


void
mkcl_FEcannot_open(MKCL, mkcl_object fn)
{
  mkcl_FElibc_file_error(env, fn, "Cannot open file", 0);
}

void
mkcl_FEend_of_file(MKCL, mkcl_object strm)
{
  mk_cl_error(env, 3, @'end-of-file', @':stream', strm);
}

void
mkcl_FEclosed_stream(MKCL, mkcl_object strm)
{
  mk_cl_error(env, 3, @'si::closed-stream-error', @':stream', strm);
}

void
mkcl_FEwrong_type_argument(MKCL, mkcl_object type, mkcl_object value)
{
  mk_cl_error(env, 5, @'type-error', @':datum', value, @':expected-type', type);
}

void
mkcl_FEnot_fixnum_type(MKCL, mkcl_object value)
{
  mk_cl_error(env, 5, @'type-error', @':datum', value, @':expected-type', @'fixnum');
}

void
mkcl_FEnot_codeblock_type(MKCL, mkcl_object value)
{
  mk_cl_error(env, 5, @'type-error', @':datum', value, @':expected-type', @'si::code-block');
}

void
mkcl_FEunbound_variable(MKCL, mkcl_object sym)
{
  mk_cl_error(env, 3, @'unbound-variable', @':name', sym);
}

void
mkcl_FEundefined_function(MKCL, mkcl_object fname)
{
  mk_cl_error(env, 3, @'undefined-function', @':name', fname);
}

/*************
 * Shortcuts *
 *************/

void
mkcl_FEwrong_num_arguments(MKCL, mkcl_object fun)
{
  if (MKCL_FIXNUMP(fun)) {
    fun = (mkcl_object)(mkcl_root_symbols + mkcl_fixnum_to_word(fun));
  }
  mkcl_FEprogram_error(env, "Wrong number of arguments passed to function ~S", 1, fun);
}

void
mkcl_FEwrong_num_arguments_anonym(MKCL)
{
  mkcl_FEprogram_error(env, "Wrong number of arguments passed to an anonymous function", 0);
}

void
mkcl_FEinvalid_macro_call(MKCL, mkcl_object name)
{
  mkcl_FEerror(env, "Invalid macro call to ~S", 1, name);
}

void
mkcl_FEinvalid_variable(MKCL, const char *s, mkcl_object obj)
{
  mkcl_FEerror(env, s, 1, obj);
}

void
mkcl_FEassignment_to_constant(MKCL, mkcl_object v)
{
  mkcl_FEprogram_error(env, "SETQ: Tried to assign a value to the constant ~S", 1, v);
}

void
mkcl_FEillegal_variable_name(MKCL, mkcl_object v)
{
  mkcl_FEprogram_error(env, "Not a valid variable name ~S", 1, v);
}

void
mkcl_FEinvalid_function(MKCL, mkcl_object obj)
{
  mkcl_FEwrong_type_argument(env, @'function', obj);
}

void
mkcl_FEinvalid_function_name(MKCL, mkcl_object fname)
{
  mk_cl_error(env, 9, @'simple-type-error', @':format-control',
	      mkcl_make_simple_base_string(env, "Not a valid function name ~D"),
	      @':format-arguments', mk_cl_list(env, 1, fname),
	      @':expected-type', mk_cl_list(env, 2, @'satisfies', @'si::valid-function-name-p'),
	      @':datum', fname);
}

/*      bootstrap version                */
static mkcl_object
universal_error_handler(MKCL, mkcl_narg narg, mkcl_object c, mkcl_object err, mkcl_object args, ...)
{
  char * msg;

  if ( mkcl_t_base_string == mkcl_type_of(err) )
    msg = (char *) err->base_string.self; /* Could do better Unicode-wise. JCB */
  else
    msg = "Lisp initialization error";

  fprintf(stderr, "\n MKCL bootstrap error handler:\n\t%s.\n", msg);
  fflush(stderr);
  if (mkcl_early_boot)
    mkcl_longjmp(mkcl_early_boot_error_handler, 1);
  else
    mk_mt_abandon_thread(env, @':aborted');
}

void
mkcl_FEillegal_index(MKCL, mkcl_object x, mkcl_object i)
{
  mkcl_FEerror(env, "~S is an illegal index to ~S", 2, i, x);
}

void
mkcl_FEtype_error_symbol(MKCL, mkcl_object obj)
{
  mkcl_FEwrong_type_argument(env, @'symbol', obj);
}

void
mkcl_FEdivision_by_zero(MKCL, mkcl_object x, mkcl_object y)
{
  mk_cl_error(env, 5, @'division-by-zero', @':operation', @'/', @':operands', mk_cl_list(env, 2, x, y));
}


mkcl_object
mkcl_libc_error_string(MKCL, mkcl_word errno_value)
{
  mkcl_object errno_msg;

#ifdef MKCL_WINDOWS
  {
#if 0
    wchar_t * _wcserror(int);  /* Do we need to go to __MSVCRT_VERSION__ >= 0x0700 ? */
    errno_msg = mkcl_cstring16_to_string(env, _wcserror(errno_value));
#else
    errno_msg = mkcl_cstring_to_string(env, strerror(errno_value));
#endif
  }
#else
  {
    char * errno_msg_os_str;
    char buf[2048];

# if _GNU_SOURCE
    errno_msg_os_str = strerror_r(errno_value, buf, sizeof(buf)); /* GNU-specific version */
# else
    if (strerror_r(errno_value, buf, sizeof(buf))) buf[0] = '\0';
    errno_msg_os_str = buf;
# endif
    errno_msg = mkcl_cstring_to_string(env, errno_msg_os_str);
  }
#endif

  return errno_msg;
}

mkcl_object
mk_si_errno_string(MKCL)
{
  mkcl_call_stack_check(env);
  @(return mkcl_libc_error_string(env, errno));
}

mkcl_object
mk_si_libc_error_string(MKCL, mkcl_object errno_val)
{
  mkcl_object errno_msg;

  mkcl_call_stack_check(env);
  if (MKCL_FIXNUMP(errno_val))
    errno_msg = mkcl_libc_error_string(env, mkcl_fixnum_to_word(errno_val));
  else
    errno_msg = mk_cl_Cnil;

  @(return errno_msg);
}

/*************************************
 * Errors generated by the C library *
 *************************************/
/*
 * Interprets an error code from the C library according to the POSIX
 * standard, and produces a suitable error message by combining the user
 * supplied format with an explanation of the cause of the error.
 */
void
mkcl_FElibc_error(MKCL, const char *msg, int narg, ...)
{
  int mkcl_saved_errno = errno;
  mkcl_va_list args;
  mkcl_object rest;
  mkcl_object errno_msg = mkcl_libc_error_string(env, mkcl_saved_errno);

  mkcl_va_start(env, args, narg, narg, 0);
  rest = mkcl_grab_rest_args(env, args, FALSE);

  mkcl_FEerror(env, "~?~%OS Explanation: (errno == ~D) ~A",
	       4,
	       mkcl_make_simple_base_string(env, (char *) msg),
	       rest,
	       MKCL_MAKE_FIXNUM(mkcl_saved_errno),
	       errno_msg);
}

void
mkcl_FElibc_file_error(MKCL, mkcl_object pathname, const char *msg, int narg, ...)
{
  int mkcl_saved_errno = errno;
  mkcl_va_list args;
  mkcl_object rest;
  mkcl_object errno_msg = mkcl_libc_error_string(env, mkcl_saved_errno);

  mkcl_va_start(env, args, narg, narg, 0);
  rest = mkcl_grab_rest_args(env, args, FALSE);

  mk_cl_error(env, 7,
	      @'si::OS-file-error',
	      @':pathname',
	      pathname,
	      @':format-control',
	      mkcl_make_simple_base_string(env, "~?~%OS Explanation: (errno == ~D) ~A"),
	      @':format-arguments',
	      mk_cl_list(env, 4,
			 mkcl_make_simple_base_string(env, (char *) msg),
			 rest,
			 MKCL_MAKE_FIXNUM(mkcl_saved_errno),
			 errno_msg)
	      );
}

void
mkcl_FElibc_stream_error(MKCL, mkcl_object stream, const char *msg, int narg, ...)
{
  int mkcl_saved_errno = errno;
  mkcl_va_list args;
  mkcl_object rest;
  mkcl_object errno_msg = mkcl_libc_error_string(env, mkcl_saved_errno);

  mkcl_va_start(env, args, narg, narg, 0);
  rest = mkcl_grab_rest_args(env, args, FALSE);

  mk_cl_error(env, 7,
	      @'si::OS-stream-error',
	      @':stream',
	      stream,
	      @':format-control',
	      mkcl_make_simple_base_string(env, "~?~%OS Explanation: (errno == ~D) ~A"),
	      @':format-arguments',
	      mk_cl_list(env, 4,
			 mkcl_make_simple_base_string(env, (char *) msg),
			 rest,
			 MKCL_MAKE_FIXNUM(mkcl_saved_errno),
			 errno_msg)
	      );
}

#if defined(MKCL_WINDOWS)
void
mkcl_FEwin32_error(MKCL, const char *msg, int narg, ...)
{
  DWORD error_code = GetLastError();
  DWORD win_msg_size;
  mkcl_object rest, win_msg_obj;
  mkcl_va_list args;
  wchar_t *win_msg;

  mkcl_va_start(env, args, narg, narg, 0);
  rest = mkcl_grab_rest_args(env, args, FALSE);

  MKCL_LIBC_NO_INTR(env, (win_msg_size = FormatMessageW(FORMAT_MESSAGE_FROM_SYSTEM|FORMAT_MESSAGE_ALLOCATE_BUFFER,
						   0, error_code, 0, (void *)&win_msg, 0, NULL)));
  if (win_msg_size == 0)
    win_msg_obj = mkcl_make_simple_base_string(env, "[Unable to get OS error message]");
  else {
    win_msg_obj = mkcl_cstring16_to_string(env, win_msg);
    MKCL_LIBC_NO_INTR(env, LocalFree(win_msg));
  }

  mkcl_FEerror(env, "~?~%Explanation: ~A",
	       3,
	       mkcl_make_simple_base_string(env, (char *) msg),
	       rest,
	       win_msg_obj);
}

void
mkcl_FEwin32_file_error(MKCL, mkcl_object pathname, const char *msg, int narg, ...)
{
  DWORD error_code = GetLastError();
  mkcl_va_list args;
  mkcl_object rest, win_msg_obj;
  wchar_t *win_msg;

  mkcl_va_start(env, args, narg, narg, 0);
  rest = mkcl_grab_rest_args(env, args, FALSE);

  if (FormatMessageW(FORMAT_MESSAGE_FROM_SYSTEM|FORMAT_MESSAGE_ALLOCATE_BUFFER,
		     0, error_code, 0, (void*)&win_msg, 0, NULL) == 0)
    win_msg_obj = mkcl_make_simple_base_string(env, "[Unable to get error message]");
  else {
    win_msg_obj = mkcl_cstring16_to_string(env, win_msg);
    LocalFree(win_msg);
  }

  mk_cl_error(env, 7,
	      @'si::OS-file-error',
	      @':pathname',
	      pathname,
	      @':format-control',
	      mkcl_make_simple_base_string(env, "~?~%OS Explanation: ~A"),
	      @':format-arguments',
	      mk_cl_list(env, 3,
			 mkcl_make_simple_base_string(env, (char *) msg),
			 rest,
			 win_msg_obj)
	      );
}

void
mkcl_FEwin32_stream_error(MKCL, mkcl_object stream, const char *msg, int narg, ...)
{
  DWORD error_code = GetLastError();
  mkcl_va_list args;
  mkcl_object rest, win_msg_obj;
  wchar_t *win_msg;

  mkcl_va_start(env, args, narg, narg, 0);
  rest = mkcl_grab_rest_args(env, args, FALSE);

  if (FormatMessageW(FORMAT_MESSAGE_FROM_SYSTEM|FORMAT_MESSAGE_ALLOCATE_BUFFER,
		     0, error_code, 0, (void*)&win_msg, 0, NULL) == 0)
    win_msg_obj = mkcl_make_simple_base_string(env, "[Unable to get OS error message]");
  else {
    win_msg_obj = mkcl_cstring16_to_string(env, win_msg);
    LocalFree(win_msg);
  }

  mk_cl_error(env, 7,
	      @'si::OS-stream-error',
	      @':stream',
	      stream,
	      @':format-control',
	      mkcl_make_simple_base_string(env, "~?~%OS Explanation: ~A"),
	      @':format-arguments',
	      mk_cl_list(env, 3,
			 mkcl_make_simple_base_string(env, (char *) msg),
			 rest,
			 win_msg_obj)
	      );
}
#endif

/************************************
 * Higher level interface to errors *
 ************************************/

@(defun error (eformat &rest args)
@
  /* mkcl_enable_interrupts(env); */
  mkcl_funcall3(env,
		@+'si::universal-error-handler',
		mk_cl_Cnil,
		eformat,
		mkcl_grab_rest_args(env, args, FALSE));
  mkcl_lose(env, "Should not have returned from universal-error-handler");
@)

@(defun cerror (cformat eformat &rest args)
@
  /* mkcl_enable_interrupts(env); */
  return mkcl_funcall3(env,
		       @+'si::universal-error-handler',
		       cformat,
		       eformat,
		       mkcl_grab_rest_args(env, args, FALSE));
@)

void
mkcl_init_error(MKCL)
{
  mkcl_def_c_function_va(env, @'si::universal-error-handler', (mkcl_objectfn) universal_error_handler); /* bootstrap version. */
}
