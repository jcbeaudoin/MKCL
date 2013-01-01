/* -*- mode: c -*- */
/*
    main.c --
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

/******************************** GLOBALS *****************************/

#include <mkcl/mkcl.h>

#if defined(MKCL_WINDOWS)
# include <windows.h>
# include <shellapi.h>
# include <winbase.h>
#endif

#include <unistd.h>

#ifdef __unix
# include <langinfo.h>
# include <locale.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <mkcl/internal.h>


/******************************* EXPORTS ******************************/

struct mkcl_core_struct mkcl_core;


/************************ GLOBAL INITIALIZATION ***********************/

static mkcl_index ARGC;
static char **ARGV;

static mkcl_word option_values[MKCL_OPT_MAXIMUM] = {
  FALSE,	/* MKCL_OPT_INCREMENTAL_GC, a boolean flag */
  TRUE,		/* MKCL_OPT_CHAIN_SIGSEGV, a boolean flag */
  TRUE,		/* MKCL_OPT_CHAIN_SIGFPE, a boolean flag */
  TRUE,		/* MKCL_OPT_CHAIN_SIGINT, a boolean flag */
  TRUE,		/* MKCL_OPT_CHAIN_SIGILL, a boolean flag */
  TRUE,		/* MKCL_OPT_CHAIN_SIGBUS, a boolean flag */
  0,    	/* MKCL_OPT_THREAD_INTERRUPT_SIGNAL, a signal number */
  0,    	/* MKCL_OPT_THREAD_RESUME_SIGNAL, a signal number */
  0,    	/* MKCL_OPT_THREAD_WAKE_UP_SIGNAL, a signal number */
  0,    	/* MKCL_OPT_GC_THREAD_SUSPEND_SIGNAL, a signal number */
  0,    	/* MKCL_OPT_GC_THREAD_RESTART_SIGNAL, a signal number */
  TRUE,		/* MKCL_OPT_SET_GMP_MEMORY_FUNCTIONS, a boolean flag */
  FALSE,	/* MKCL_OPT_BOOTED, a boolean flag */
  128,  	/* MKCL_OPT_BINDING_STACK_INITIAL_SIZE, in nb. of bindings */
  128,  	/* MKCL_OPT_BINDING_STACK_OVERFLOW_SIZE, in nb. of bindings */
  16,   	/* MKCL_OPT_FRAME_STACK_INITIAL_SIZE, in nb. of frames */
  16,   	/* MKCL_OPT_FRAME_STACK_OVERFLOW_SIZE, in nb. of frames */
  1024,   	/* MKCL_OPT_LISP_TEMP_STACK_INITIAL_SIZE, in nb. of items */
  1024,   	/* MKCL_OPT_LISP_TEMP_STACK_OVERFLOW_SIZE, in nb. of items */
  1024*1024,    /* MKCL_OPT_INTERRUPT_THREAD_CALL_STACK_SIZE, in nb. of bytes */
#if 0 /* Does not work with Boehm's GC */
  128*1024,	/* MKCL_OPT_SIGALTSTACK_SIZE, in nb. of bytes */ /* we stack-allocate so much that 128K is barely safe. JCB */
#else
  128*1024,     /* MKCL_OPT_CALL_STACK_OVERFLOW_SIZE, in nb. of bytes */
#endif
  0,            /* MKCL_OPT_HEAP_SIZE, in nb. of bytes */ /* unlimited by default. JCB */
  1024*1024, 	/* MKCL_OPT_HEAP_SAFETY_AREA, in nb. of bytes */
};

#define NB_FEATURES (sizeof(feature_names)/sizeof(feature_names[0]))

static const char * const feature_names[] = {
        "MKCL", "COMMON", "COMMON-LISP", "ANSI-CL",
#if defined(__i386) || defined(__pentium)
	"X86",
	"LITTLE-ENDIAN",
#elif defined(__x86_64)
	"X86-64",
	"LITTLE-ENDIAN",
#endif
#ifdef MKCL_IEEE_FP
        "IEEE-FLOATING-POINT",
#endif
#ifdef __unix
	"UNIX",
#endif
#ifdef __linux
        "LINUX",
#endif
#ifdef __MINGW32__
	"WINDOWS",
# ifdef __MINGW64__
	"MINGW64",
# else
	"MINGW32",
# endif
#endif
#ifdef _MSC_VER
	"WINDOWS",
	"MSVC",
#endif
	"UNICODE",
#ifdef MKCL_RELATIVE_PACKAGE_NAMES
	"RELATIVE-PACKAGE-NAMES",
#endif
};

mkcl_word
mkcl_get_option(mkcl_option option)
{
  return option_values[option];
}

int
mkcl_set_option(mkcl_option option, mkcl_word value)
{
  if (option > MKCL_OPT_MAXIMUM || option < 0)
    return MKCL_BAD_OPTION;
  else if (option < MKCL_OPT_BOOTED && option_values[MKCL_OPT_BOOTED])
    return MKCL_IMMUTABLE_OPTION;
  else
    option_values[option] = value;
  return 0;
}


#include "iso_latin_names.h"




static mkcl_object mkcl_true_self(MKCL)
{
  mkcl_object self_truename;

#ifdef __linux
  {
    /* The value of 3 here is a rough upper bound of the number of decimal
       digits needed to print the value of 8 binary bits. A more precise
       value would have been 2.409 but we needed an integer. */
    static char buf[sizeof("/proc//exe") + (3 * sizeof(pid_t))];

    sprintf(buf, "/proc/%d/exe", getpid());

    self_truename = mkcl_make_base_string_copy(env, buf);
  }
#elif defined(MKCL_WINDOWS)
  {
    wchar_t buf[48 * 1024]; /* UNC paths are said in MS documentation to be more or less 32k long maximum. */
    DWORD nSize = MKCL_NB_ELEMS(buf);
    DWORD rc;

    MKCL_LIBC_NO_INTR(env, rc = GetModuleFileNameW(NULL, buf, nSize));
    if (rc == nSize)
      mkcl_lose(env, "mkcl_true_self(): Buffer overflow for GetModuleFileName.");
    else if (rc == 0)
      mkcl_FEwin32_error(env, "Could not get MKCL true name!", 0);

    self_truename = mkcl_cstring16_to_string(env, buf);
  }
#else
# error "This is almost sure to be wrong and may lead to a core dump! Fix it!"
  self_truename = mkcl_core.self;
#endif

  self_truename = mk_cl_probe_file(env, self_truename);
  return self_truename;
}

mkcl_object mk_si_self_truename(MKCL)
{
  @(return mkcl_core.self_truename);
}

static const mkcl_object const initial_thread_bindings[] =
  {
    @'*break-on-signals*',
    @'*compile-print*',
    @'*compile-verbose*',
    @'*debugger-hook*',
    @'*default-pathname-defaults*',
    @'*gensym-counter*',
    @'*load-print*',
    @'*load-verbose*',
    @'*macroexpand-hook*',
    @'*package*',
    @'*print-array*',
    @'*print-base*',
    @'*print-case*',
    @'*print-circle*',
    @'*print-escape*',
    @'*print-gensym*',
    @'*print-length*',
    @'*print-level*',
    @'*print-lines*',
    @'*print-miser-width*',
    @'*print-pprint-dispatch*',
    @'*print-pretty*',
    @'*print-radix*',
    @'*print-readably*',
    @'*print-right-margin*',
    @'*random-state*',
    @'*read-base*',
    @'*read-default-float-format*',
    @'*read-eval*',
    @'*read-suppress*',
    @'*readtable*',
    @'*standard-input*',
    @'*standard-output*',
    @'*error-output*',
    @'*trace-output*',
    @'*terminal-io*',
    @'*query-io*',
    @'*debug-io*',
    @'si::*dribble-closer*',
    @'mt::*thread*',
    @'si::*restart-clusters*',
    @'si::*condition-restarts*',
    @'si::*handler-clusters*',
    @'si::*dynamic-cons-stack*',
    @'si::*default-floating-point-exception-set*',
    @'si::*os-string-format*',
    @'si::*default-external-format*',
    @'mkcl::*current-working-directory*',
    @'mkcl::*all-current-working-directories*'
  };

static void setup_initial_thread_bindings(MKCL)
{
  int i;
  int nb_bindings = sizeof(initial_thread_bindings)/sizeof(initial_thread_bindings[0]);

  for (i = 0; i < nb_bindings; i++)
    mkcl_bds_push(env, initial_thread_bindings[i]);
}

static const mkcl_object mkcl_T = mk_cl_Ct; /* debugging convenience. */


static void _mkcl_boot_inner(MKCL)
{
  mkcl_init_early_unixint(env);
  mkcl_init_early_threads(env);
  mkcl_init_unixsys(env); /* Could be much later. */

#ifdef HASHTABLE_STATS /* JCB */
  mkcl_core.hashtables[mkcl_htt_eq] = mk_cl_Cnil;
  mkcl_core.hashtables[mkcl_htt_eql] = mk_cl_Cnil;
  mkcl_core.hashtables[mkcl_htt_equal] = mk_cl_Cnil;
  mkcl_core.hashtables[mkcl_htt_equalp] = mk_cl_Cnil;
  mkcl_core.hashtables[mkcl_htt_package] = mk_cl_Cnil;
#endif

  /*
   * 1) Initialize symbols and packages
   */

  mk_cl_Cnil_symbol->symbol.t = mkcl_t_symbol;
  mk_cl_Cnil_symbol->symbol.dynamic = 0;
  mk_cl_Cnil_symbol->symbol.special_index = MKCL_NOT_A_SPECIAL_INDEX;
  mk_cl_Cnil_symbol->symbol.value = mk_cl_Cnil;
  mk_cl_Cnil_symbol->symbol.name = mkcl_make_simple_base_string(env, "NIL");
  mk_cl_Cnil_symbol->symbol.gfdef = mk_cl_Cnil;
  mk_cl_Cnil_symbol->symbol.plist = mk_cl_Cnil;
  mk_cl_Cnil_symbol->symbol.sys_plist = mk_cl_Cnil;
  mk_cl_Cnil_symbol->symbol.hpack = mk_cl_Cnil;
  mk_cl_Cnil_symbol->symbol.properly_named_class = mk_cl_Cnil;
  mk_cl_Cnil_symbol->symbol.stype = mkcl_stp_constant;

  mk_cl_Ct->symbol.t = mkcl_t_symbol;
  mk_cl_Ct->symbol.dynamic = 0;
  mk_cl_Ct->symbol.special_index = MKCL_NOT_A_SPECIAL_INDEX;
  mk_cl_Ct->symbol.value = mk_cl_Ct;
  mk_cl_Ct->symbol.name = mkcl_make_simple_base_string(env, "T");
  mk_cl_Ct->symbol.gfdef = mk_cl_Cnil;
  mk_cl_Ct->symbol.plist = mk_cl_Cnil;
  mk_cl_Ct->symbol.sys_plist = mk_cl_Cnil;
  mk_cl_Ct->symbol.hpack = mk_cl_Cnil;
  mk_cl_Ct->symbol.properly_named_class = mk_cl_Cnil;
  mk_cl_Ct->symbol.stype = mkcl_stp_constant;


#if 1
  mkcl_core.packages = mk_cl_Cnil;
#endif
  mkcl_core.packages_to_be_created = mk_cl_Cnil;

  mkcl_core.lisp_package =
    mkcl_make_package(env, mkcl_make_simple_base_string(env, "COMMON-LISP"),
		     mk_cl_list(env, 2,
				mkcl_make_simple_base_string(env, "CL"),
				mkcl_make_simple_base_string(env, "LISP")),
		     mk_cl_Cnil);
  mkcl_core.user_package =
    mkcl_make_package(env, mkcl_make_simple_base_string(env, "COMMON-LISP-USER"),
		     mk_cl_list(env, 2,
				mkcl_make_simple_base_string(env, "CL-USER"),
				mkcl_make_simple_base_string(env, "USER")),
		     mkcl_list1(env, mkcl_core.lisp_package));
  mkcl_core.keyword_package =
    mkcl_make_package(env, mkcl_make_simple_base_string(env, "KEYWORD"), mk_cl_Cnil, mk_cl_Cnil);
  mkcl_core.system_package =
    mkcl_make_package(env, mkcl_make_simple_base_string(env, "SI"),
		      mk_cl_list(env, 2,
				 mkcl_make_simple_base_string(env, "SYSTEM"),
				 mkcl_make_simple_base_string(env, "SYS")),
		      mkcl_list1(env, mkcl_core.lisp_package));
  mkcl_core.mkcl_ext_package =
    mkcl_make_package(env, mkcl_make_simple_base_string(env, "MKCL"),
		      mk_cl_list(env, 2,
				 mkcl_make_simple_base_string(env, "MKCL-EXTENSIONS"),
				 /* mkcl_make_simple_base_string(env, "EXT"), */ /* temporary, for the transition period. */
				 mkcl_make_simple_base_string(env, "MK-EXT")),
		      mkcl_list1(env, mkcl_core.lisp_package));
  mkcl_core.clos_package =
    mkcl_make_package(env, mkcl_make_simple_base_string(env, "CLOS"),
		      mk_cl_Cnil, mkcl_list1(env, mkcl_core.lisp_package));
  mkcl_core.mt_package =
    mkcl_make_package(env, mkcl_make_simple_base_string(env, "MT"),
		      mk_cl_list(env, 3,
				 mkcl_make_simple_base_string(env, "MULTI-THREADING"),
				 mkcl_make_simple_base_string(env, "MP"),
				 mkcl_make_simple_base_string(env, "MULTIPROCESSING")),
		      mkcl_list1(env, mkcl_core.lisp_package));
  mkcl_core.gray_package = mkcl_make_package(env, mkcl_make_simple_base_string(env, "GRAY"),
					     mk_cl_Cnil,
					     mkcl_list1(env, mkcl_core.lisp_package));

  mk_cl_Cnil_symbol->symbol.hpack = mkcl_core.lisp_package;
  mkcl_import2(env, mk_cl_Cnil, mkcl_core.lisp_package);
  mkcl_export2(env, mk_cl_Cnil, mkcl_core.lisp_package);

  mk_cl_Ct->symbol.hpack = mkcl_core.lisp_package;
  mkcl_import2(env, mk_cl_Ct, mkcl_core.lisp_package);
  mkcl_export2(env, mk_cl_Ct, mkcl_core.lisp_package);

  /* These must come _after_ the packages and NIL/T have been created */
  mkcl_init_all_symbols(env);

  mk_si_gc_on(env);

  /*
   * Initialize constants (strings, numbers and time).
   */

  /* LIBRARIES is an adjustable vector of objects. It behaves as
     a vector of weak pointers thanks to the magic in
     gbc.d/alloc_2.d */
  mkcl_core.libraries = mk_cl_Cnil;
  mkcl_core.to_be_finalized = mk_cl_Cnil;
  mkcl_core.bytes_consed = mk_cl_Cnil;
  mkcl_core.gc_counter = mk_cl_Cnil;
  mkcl_core.gc_fast_counter = 0;
  mkcl_core.gc_stats = FALSE;

  mkcl_core.empty_string = mkcl_make_simple_base_string(env, "");
  mkcl_core.dot_string = mkcl_make_simple_base_string(env, ".");
  mkcl_core.dot_dot_string = mkcl_make_simple_base_string(env, "..");
  mkcl_core.localhost_string = mkcl_make_simple_base_string(env, "localhost");

  mkcl_core.null_stream = mk_cl_Cnil; /* Filled in file.d */

  mkcl_init_system_properties(env);

  mkcl_core.gensym_prefix = mkcl_make_simple_base_string(env, "G");
  mkcl_core.gentemp_prefix = mkcl_make_simple_base_string(env, "T");
  mkcl_core.gentemp_counter = MKCL_MAKE_FIXNUM(0);
  mkcl_init_gentemp(env);


  mkcl_init_error(env);

#ifdef MKCL_WINDOWS
  MKCL_SET(@'si::*os-string-format*', @':UTF-16LE'); /* This is in fact an immutable constant. */
  mkcl_core.default_default_external_format = @':ASCII'; /* Bootstrap value */
#else
  mkcl_object saved_locale = mkcl_make_base_string_copy(env, setlocale(LC_ALL, NULL));
  setlocale(LC_ALL, ""); /* Imports locale settings from environment. */
  {
    char * os_codeset = nl_langinfo(CODESET);
    size_t os_codeset_len = strlen(os_codeset);
    mkcl_base_string_object_sized(os_codeset_name_obj, os_codeset, os_codeset_len);
    int intern_flag;
    mkcl_object os_external_format = mkcl_intern(env, (mkcl_object) &os_codeset_name_obj, mkcl_core.keyword_package, &intern_flag);
   
    if (0 == strcmp(os_codeset, "ANSI_X3.4-1968"))
      os_external_format = @':US-ASCII';
    MKCL_SET(@'si::*os-string-format*', os_external_format);
    mkcl_core.default_default_external_format = os_external_format;
  }
  setlocale(LC_ALL, saved_locale->base_string.self); /* Puts locale back to its previous settings. */
#endif
  MKCL_SET(@'si::*default-external-format*', mkcl_core.default_default_external_format);


  /*
   * Initialize default pathnames
   */
  mkcl_core.pathname_translations = mk_cl_Cnil;
  mkcl_core.SYS_library_pathname = mk_cl_Cnil;
  mkcl_core.empty_default_pathname_defaults = mkcl_make_pathname(env,
								 mkcl_core.localhost_string, @':unspecific', mk_cl_Cnil,
								 mk_cl_Cnil, mk_cl_Cnil, mk_cl_Cnil);

  MKCL_SET(@'*default-pathname-defaults*', mkcl_core.empty_default_pathname_defaults);

  mkcl_core.self = mkcl_argv(env, 0);
  mkcl_core.self_truename = mkcl_true_self(env);

  mkcl_core.children = mk_cl_Cnil;
#if __unix
  mkcl_core.detached_children = mkcl_list1(env, mk_cl_Cnil); /* end of list sentinel. Needed by sigchld handler. */
#else
  mkcl_core.detached_children = mk_cl_Cnil; /* Should be unused in MS-Windows.*/
#endif

  /*
   * Load character names. The following hash table is a map
   * from names to character codes and viceversa. Note that we
   * need EQUALP because it has to be case insensitive.
   */
  {
    int i;
    mkcl_object char_names_table
      = mk_cl__make_hash_table(env, @'equalp', MKCL_MAKE_FIXNUM(128), /* size */
			       mkcl_make_singlefloat(env, 1.5f), /* rehash-size */
			       mkcl_make_singlefloat(env, 0.5f)); /* rehash-threshold */
    
    mkcl_core.char_names = char_names_table;
    for (i = 0; i < MKCL_NB_ELEMS(char_names); i++) {
      mkcl_object name = mkcl_make_simple_base_string(env, char_names[i]);
      mkcl_object code = MKCL_MAKE_FIXNUM(i);
      mkcl_sethash(env, name, char_names_table, code);
      mkcl_sethash(env, code, char_names_table, name);
    }
    {
      /* Linefeed is redundant with one of the names given
       * in iso_latin_names.h, but it can not be associated
       * to the code 10, because the default name must be
       * Newline. */
      static const mkcl_base_string_object(null_str_obj, "Null");
      static const mkcl_base_string_object(bell_str_obj, "Bell");
      static const mkcl_base_string_object(linefeed_str_obj, "Linefeed");
      static const mkcl_base_string_object(escape_str_obj, "Escape");
      
      mkcl_sethash(env, (mkcl_object) &null_str_obj, char_names_table, MKCL_MAKE_FIXNUM(0));
      mkcl_sethash(env, (mkcl_object) &bell_str_obj, char_names_table, MKCL_MAKE_FIXNUM(7));
      mkcl_sethash(env, (mkcl_object) &linefeed_str_obj, char_names_table, MKCL_MAKE_FIXNUM(10));
      mkcl_sethash(env, (mkcl_object) &escape_str_obj, char_names_table, MKCL_MAKE_FIXNUM(27));
    }
  }

  /*
   * Initialize logical pathname translations. This must come after
   * the character database has been filled.
   */
  @si::pathname-translations(env, 2, mkcl_make_simple_base_string(env, "SYS"),
			     mk_cl_list(env, 1,
					mk_cl_list(env, 2,
						   mkcl_make_simple_base_string(env, "**;*.*"),
						   mkcl_make_simple_base_string(env, "./**/*.*"))));

  /*
   * Ininitialize numbers
   */
  MKCL_SET(@'si::c-int-max', mkcl_make_integer(env, INT_MAX));
  MKCL_SET(@'si::c-int-min', mkcl_make_integer(env, INT_MIN));
  MKCL_SET(@'si::c-long-max', mkcl_make_integer(env, LONG_MAX));
  MKCL_SET(@'si::c-long-min', mkcl_make_integer(env, LONG_MIN));
  MKCL_SET(@'si::c-uint-max', mkcl_make_unsigned_integer(env, UINT_MAX));
  MKCL_SET(@'si::c-ulong-max', mkcl_make_unsigned_integer(env, ULONG_MAX));

  mkcl_init_number(env);

  MKCL_SET(@'si::c-long-long-min', mkcl_make_int64_t(env, INT64_MIN));
  MKCL_SET(@'si::c-long-long-max', mkcl_make_int64_t(env, INT64_MAX));
  MKCL_SET(@'si::c-ulong-long-max', mkcl_make_uint64_t(env, UINT64_MAX));

  mkcl_init_unixtime(env);

  MKCL_SET(@'mt::*thread*', env->own_thread);

  /*
   * Initialize I/O subsystem.
   */
  mkcl_init_file(env);
  mkcl_init_read(env);

  MKCL_SET(@'*print-case*', @':upcase');

  mkcl_core.shutdown_thread = mk_cl_Cnil;
  mkcl_core.shutdown_gate = mk_mt_make_lock(env, 4, @':name', @'si::+shutdown-gate+', @':recursive', mk_cl_Cnil);
  MKCL_SET(@'si::+shutdown-gate+', mkcl_core.shutdown_gate);

  /*
   * Set up hooks for LOAD, errors and macros.
   */
  MKCL_SET(@'mt::+forward-reference-lock+', mk_mt_make_lock(env, 2, @':name', @'mt::+forward-reference-lock+'));
  MKCL_SET(@'mt::+load-compile-lock+', mk_mt_make_lock(env, 4, @':name', @'mt::+load-compile-lock+', @':recursive', mk_cl_Ct));
  {
    mkcl_object load_hooks  = mk_cl_list(env, 11,
					 MKCL_CONS(env, mkcl_make_simple_base_string(env, "fasb"), @'si::load-binary'),
					 MKCL_CONS(env, mkcl_make_simple_base_string(env, "FASB"), @'si::load-binary'),
					 MKCL_CONS(env, mkcl_make_simple_base_string(env, "fas"), @'si::load-binary'),
					 MKCL_CONS(env, mkcl_make_simple_base_string(env, "FAS"), @'si::load-binary'),
					 MKCL_CONS(env, mkcl_make_simple_base_string(env, "fasl"), @'si::load-binary'),
					 MKCL_CONS(env, mkcl_make_simple_base_string(env, "FASL"), @'si::load-binary'),
					 MKCL_CONS(env, mkcl_make_simple_base_string(env, "lsp"), @'si::load-source'),
					 MKCL_CONS(env, mkcl_make_simple_base_string(env, "LSP"), @'si::load-source'),
					 MKCL_CONS(env, mkcl_make_simple_base_string(env, "lisp"), @'si::load-source'),
					 MKCL_CONS(env, mkcl_make_simple_base_string(env, "LISP"), @'si::load-source'),
					 MKCL_CONS(env, mk_cl_Cnil, @'si::load-source'));
    MKCL_SET(@'si::*load-hooks*', load_hooks);
  }
  MKCL_SET(@'*load-verbose*', mk_cl_Cnil);

  mkcl_init_macros(env);

  /*
   * Set up infrastructure for CLOS.
   */
  MKCL_SET(@'si::*class-name-hash-table*',
	   mk_cl__make_hash_table(env, @'eq', MKCL_MAKE_FIXNUM(1024), /* size */
				  mkcl_make_singlefloat(env, 1.5f), /* rehash-size */
				  mkcl_make_singlefloat(env, 0.75f))); /* rehash-threshold */
  /*
   * Features.
   */
  {
    int i;
    mkcl_object features;

    MKCL_SET(@'LAMBDA-LIST-KEYWORDS',
	     mk_cl_list(env, 8,
			@'&optional', @'&rest', @'&key', @'&allow-other-keys',
			@'&aux', @'&whole', @'&environment', @'&body'));
    
    for (i = 0, features = mk_cl_Cnil; i < NB_FEATURES; i++) {
      features = MKCL_CONS(env, mkcl_make_keyword(env, feature_names[i]),features);
    }
    
    MKCL_SET(@'*features*', features);
  }

  MKCL_SET(@'*package*', mkcl_core.lisp_package);

  {
    mkcl_object fpe_set = mk_si_initial_floating_point_exception_set(env);
    
    MKCL_SET(@'si::*default-floating-point-exception-set*', fpe_set); /* global */
  }

  setup_initial_thread_bindings(env);

 /* This initializes the dynamic cons stack and must be done right
    after the initial thread binding of si:*dynamic-cons-stack*. */
  mk_si_trim_dynamic_cons_stack(env);

  mkcl_read_VV(env, mk_cl_Cnil, mkcl_init_lib_LSP, mk_cl_Cnil);

  MKCL_SET(@'*package*', mkcl_core.user_package); /* global */
  MKCL_SETQ(env, @'*package*', mkcl_core.user_package); /* thread-local */

  /* At this point the full Common Lisp library is available. */
  mkcl_init_late_unixint(env);
  mkcl_init_late_threads(env);
  mkcl_init_late_file(env);
}

static void get_basic_OS_params(void)
{
#ifdef _PC_PATH_MAX
  mkcl_core.path_max = pathconf(".", _PC_PATH_MAX);
  mkcl_core.name_max = pathconf(".", _PC_NAME_MAX);
#else
# ifdef MKCL_WINDOWS
  mkcl_core.path_max = PATH_MAX;
# else
  mkcl_core.path_max = MAXPATHLEN;
# endif
#endif

#ifdef __unix
  {
    mkcl_index stack_addr = sysconf(_SC_THREAD_ATTR_STACKADDR);
    mkcl_index stack_size = sysconf(_SC_THREAD_ATTR_STACKSIZE);
    mkcl_index stack_min_size = sysconf(_SC_THREAD_STACK_MIN);

    mkcl_core.arg_max = sysconf(_SC_ARG_MAX);
    mkcl_core.pagesize = sysconf(_SC_PAGESIZE);
  }

  {
    pthread_attr_t main_attr;
    size_t stack_size;
    size_t guard_size;
    size_t stack_size_2;
    void * stack_addr;
    int rc;

    rc = pthread_getattr_np(pthread_self(), &main_attr);
    rc = pthread_attr_getstacksize(&main_attr, &stack_size);
    rc = pthread_attr_getstack(&main_attr, &stack_addr, &stack_size_2);
    rc = pthread_attr_getguardsize(&main_attr, &guard_size);

    usleep(1);
  }
#elif defined(MKCL_WINDOWS)
{
   SYSTEM_INFO siSysInfo;
  
   GetSystemInfo(&siSysInfo); 
 
#if 0 /* Debug. JCB */
   printf("\nHardware information: \n");  
   printf("  OEM ID: %u\n", siSysInfo.dwOemId);
   printf("  Number of processors: %u\n", siSysInfo.dwNumberOfProcessors); 
   printf("  Page size: %u\n", siSysInfo.dwPageSize); 
   printf("  Processor type: %u\n", siSysInfo.dwProcessorType); 
   printf("  Minimum application address: %lx\n", siSysInfo.lpMinimumApplicationAddress); 
   printf("  Maximum application address: %lx\n", siSysInfo.lpMaximumApplicationAddress); 
   printf("  Active processor mask: %u\n", siSysInfo.dwActiveProcessorMask); 
#endif

   mkcl_core.pagesize = siSysInfo.dwPageSize;
}

#endif
}

static bool inside_mkcl_boot_p = FALSE;
#ifdef MKCL_WINDOWS
static LONG volatile mkcl_boot_gate = FALSE;
#else
static pthread_mutex_t mkcl_boot_gate = PTHREAD_MUTEX_INITIALIZER;
#endif

bool mkcl_early_boot = TRUE;
mkcl_jmp_buf mkcl_early_boot_error_handler;


mkcl_env
mkcl_boot(int argc, char **argv, struct mkcl_thread_init_parameters * params)
{
  char stack_mark = 0;
  char * const stack_mark_address = ((params && params->stack_mark_address) ? params->stack_mark_address : &stack_mark);
  mkcl_env env;

  {
    bool already_booting_p = FALSE;
    bool booted = FALSE;
    int rc;

#ifdef MKCL_WINDOWS
    if (InterlockedCompareExchange(&mkcl_boot_gate, TRUE, FALSE))
      { errno = EAGAIN; return NULL; }
#else
    if (rc = pthread_mutex_lock(&mkcl_boot_gate))
      { errno = rc; return NULL; }
#endif
    already_booting_p = inside_mkcl_boot_p;
    if (!already_booting_p)
      {
	inside_mkcl_boot_p = TRUE;
	booted = option_values[MKCL_OPT_BOOTED];
      }
#ifdef MKCL_WINDOWS
    InterlockedExchange(&mkcl_boot_gate, FALSE);
#else
    if (rc = pthread_mutex_unlock(&mkcl_boot_gate))
      { inside_mkcl_boot_p = FALSE; errno = rc; return NULL; }
#endif

    if (already_booting_p)
      { inside_mkcl_boot_p = FALSE; errno = EAGAIN; return NULL; }
    else if (booted)
      { inside_mkcl_boot_p = FALSE; errno = EEXIST; return NULL; }
  }
      
  ARGC = ((((mkcl_index) argc) <= MKCL_MOST_POSITIVE_FIXNUM) ? argc : MKCL_MOST_POSITIVE_FIXNUM);
  ARGV = argv;

  get_basic_OS_params();

  if (mkcl_init_alloc())
    env = NULL; /* allocator initialization failed. */
  else
    env = _mkcl_alloc_env(NULL);

  if (env == NULL)
    {
      inside_mkcl_boot_p = FALSE;
      errno = -1;
      return NULL;
    }
  else
    {
      if (errno = mkcl_setjmp(mkcl_early_boot_error_handler))
	{ inside_mkcl_boot_p = FALSE; return NULL; } /* something went wrong with env initialization. */
      else
	{
	  env->cs_overflow_size = mkcl_get_option(MKCL_OPT_CALL_STACK_OVERFLOW_SIZE);
	  mkcl_init_call_stack_overflow_area(env, stack_mark_address);
	  mkcl_init_env(env, env, params);
	  mkcl_early_boot = FALSE;
	}

      MKCL_CATCH_ALL_BEGIN(env) {
	MKCL_SETUP_CALL_STACK_ROOT_GUARD(env);
	_mkcl_boot_inner(env);
	mkcl_set_option(MKCL_OPT_BOOTED, TRUE);
      } MKCL_CATCH_ALL_IF_CAUGHT {
	mkcl_object result_value = env->own_thread->thread.result_value;
	
	if (MKCL_FIXNUMP(result_value))
	  errno = mkcl_fixnum_to_word(result_value);
	else
	  errno = -1;
	inside_mkcl_boot_p = FALSE;
	return NULL;
      } MKCL_CATCH_ALL_END;

      /* The return from the CATCH_ALL just above restored the interrupt context it
	 sampled at its beginning (which was "interrupt disabled" until thread control
	 is in shape enough to have it enabled) and thus interrupts are disabled at
	 this point. We have to re-enable them later when appropriate. */
      /* mkcl_enable_interrupts(env); */
      inside_mkcl_boot_p = FALSE;
      return env;
    }
}

/************************* SHUTDOWN ROUTINES ***********************/

static void _mkcl_final_clean_up(MKCL)
{
#ifdef MKCL_WINDOWS
  while (InterlockedCompareExchange(&mkcl_boot_gate, TRUE, FALSE));
#else
  (void) pthread_mutex_lock(&mkcl_boot_gate);
#endif

  /* Here we clean up everything that is out of reach of the GC finalization. */
  /* These calls are in reverse order of what is done inside mkcl_boot() */
  mkcl_set_option(MKCL_OPT_BOOTED, FALSE);
  mkcl_clean_up_gentemp(env);
  mkcl_clean_up_system_properties(env);
  mkcl_clean_up_unixsys(env);
  mkcl_clean_up_threads(env);
  mkcl_clean_up_unixint(env);
  mkcl_clean_up_alloc(env);

  mkcl_early_boot = TRUE;

#ifdef MKCL_WINDOWS
  InterlockedExchange(&mkcl_boot_gate, FALSE);
#else
  (void) pthread_mutex_unlock(&mkcl_boot_gate);
#endif
}

long mkcl_exit_status(MKCL)
{
  mkcl_object result_value = env->own_thread->thread.result_value;
  
  if (MKCL_FIXNUMP(result_value))
    return mkcl_fixnum_to_word(result_value);
  else if (result_value == @':canceled')
    return MKCL_THREAD_CANCELED;
  else if (result_value == @':terminated')
    return MKCL_THREAD_TERMINATED;
  else if (result_value == @':invalid-value')
    return MKCL_THREAD_INVALID_VALUE;
  else if (result_value == @':imported')
    return MKCL_THREAD_INVALID_VALUE;
  else if (result_value == @':imported-and-gc-registered')
    return MKCL_THREAD_INVALID_VALUE;
  else if (result_value == @':aborted')
    return MKCL_THREAD_ABORTED;
  else if (result_value == @':gc-abort')
    return MKCL_GC_ABORT;
  else if (result_value == @':gc-exit')
    return MKCL_GC_EXIT;
  else
    return MKCL_THREAD_UNKNOWN_ERROR;
}

mkcl_object mk_si_shutdown_in_progress_p(MKCL) /* to be called with si::+shutdown-gate+ held. */
{
  @(return (mkcl_Null(mkcl_core.shutdown_thread) ? mk_cl_Cnil : mk_cl_Ct));
}

mkcl_object mk_si_register_shutdown_thread(MKCL, mkcl_object shutdown_thread) /* to be called with si::+shutdown-gate+ held. */
{
  if (mkcl_type_of(shutdown_thread) != mkcl_t_thread)
    mkcl_FEwrong_type_argument(env, @'mt::thread', shutdown_thread);
  mkcl_core.shutdown_thread = shutdown_thread;
  @(return shutdown_thread);
}

mkcl_object mk_si_register_shutdown_watchdog_thread(MKCL, mkcl_object watchdog_thread, mkcl_object will_clean_up)
{
  mkcl_call_stack_check(env);
  if (mkcl_type_of(watchdog_thread) != mkcl_t_thread)
    mkcl_FEwrong_type_argument(env, @'mt::thread', watchdog_thread);

  mk_mt_get_lock(env, 1, mkcl_core.shutdown_gate);
  mkcl_core.shutdown_watchdog_thread = watchdog_thread;
  mkcl_core.shutdown_watchdog_will_clean_up = will_clean_up;
  mk_mt_giveup_lock(env, mkcl_core.shutdown_gate);
  @(return watchdog_thread);
}

mkcl_object mk_si_shutdown_watchdog_thread(MKCL) /* to be called with si::+shutdown-gate+ held. */
{
  @(return mkcl_core.shutdown_watchdog_thread mkcl_core.shutdown_watchdog_will_clean_up);
}

static mkcl_object join_thread(MKCL, mkcl_object shutdown_thread)
{
  mkcl_os_thread_t os_thread = shutdown_thread->thread.thread;
  mkcl_object result_value = mk_cl_Cnil;
#if __unix
  void * status;
  int rc = 0;
  
  rc = pthread_join(os_thread, &status);  /* GC redirect */
  if (rc) { errno = rc; mkcl_FElibc_error(env, "mk_mt_thread_join failed on pthread_join", 0); }
#elif defined(MKCL_WINDOWS)
  DWORD exitCode;
  BOOL ok;
  DWORD wait_val;
  
  wait_val = WaitForSingleObject(os_thread, INFINITE);
  switch (wait_val)
    {
    case WAIT_OBJECT_0: break; /* The normal case. */
    case WAIT_TIMEOUT:
    case WAIT_FAILED:
    default:
      mkcl_FEwin32_error(env, "mk_mt_thread_join failed on WaitForSingleObject", 0);
      break;
    }
  ok = GetExitCodeThread(os_thread, &exitCode);
  if (!ok)
    mkcl_FEwin32_error(env, "mk_mt_thread_join failed on GetExitCodeThread", 0);
#endif

  mkcl_remove_thread_from_global_thread_list(env, shutdown_thread);

  result_value = shutdown_thread->thread.result_value;
  if (result_value == MKCL_OBJNULL)
    result_value = @':invalid-value';

#if defined(MKCL_WINDOWS)
  CloseHandle(os_thread);
  /* Make sure we do not close the handle again in the finalizer. */
  shutdown_thread->thread.base_thread = shutdown_thread->thread.thread = NULL;
#endif

  return result_value;
}

int mkcl_shutdown_watchdog(MKCL) /* We expect to run this function with interrupts disabled. */
{
  const mkcl_object own_thread = env->own_thread;
  mkcl_object result_value = own_thread->thread.result_value;

  own_thread->thread.status = mkcl_thread_done; /* Should already be the case but let's be sure */
  mkcl_remove_thread_from_global_thread_list(env, own_thread);

#if 0
  fprintf(stderr, "\n;; MKCL in shutdown watchdog.\n"); fflush(stderr);
#endif
  /* Shutdown all lisp threads. */
  if (!(result_value == @':gc-abort' || result_value == @':gc-exit')) /* Shutdown is pointless if GC is kaput. */
    {
      MKCL_CATCH_ALL_BEGIN(env) { /* Make sure we have minimal safety wrappers on in case of an abort. */
	MKCL_SETUP_CALL_STACK_ROOT_GUARD(env);
	mkcl_object shutdown_thread;
	mkcl_object watchdog_thread;
      
	mk_mt_get_lock(env, 1, mkcl_core.shutdown_gate);
	shutdown_thread = mkcl_core.shutdown_thread;
	if (mkcl_Null(shutdown_thread)) mkcl_core.shutdown_thread = mk_cl_Ct;
	watchdog_thread = mkcl_core.shutdown_watchdog_thread;
	mk_mt_giveup_lock(env, mkcl_core.shutdown_gate);
      
#if 0
	fprintf(stderr, "\n;; MKCL shutdown watchdog: about to join shutdown thread.\n"); fflush(stderr);
#endif
	if (mkcl_Null(shutdown_thread))
	  own_thread->thread.result_value
	    = mk_si_shutdown_mkcl(env, own_thread->thread.result_value, watchdog_thread, mk_cl_Ct, mk_cl_Ct);
	else
	  {
	    own_thread->thread.result_value = join_thread(env, shutdown_thread);
	  }
      } MKCL_CATCH_ALL_IF_CAUGHT {
	own_thread->thread.result_value = @':invalid-value';
      } MKCL_CATCH_ALL_END;
    }

  {
    int status = mkcl_exit_status(env);

#if 1
    if (!mkcl_Null(mkcl_core.threads))
      {
	fprintf(stderr, "\n;; MKCL shutdown watchdog: mkcl_core.threads is not NIL!\n"); fflush(stderr);
	mkcl_object threads = mkcl_core.threads;

	for (; MKCL_CONSP(threads); threads = threads->cons.cdr)
	  {
	    mkcl_object obj = threads->cons.car;

	    if (mkcl_type_of(obj) == mkcl_t_thread)
	      {
		fprintf(stderr, ";; MKCL: thread = %s\n", obj->thread.name->base_string.self);
		mkcl_println(env, obj, mkcl_core.error_output);
	      }
	    else
	      fprintf(stderr, ";; MKCL: non-thread object: %p, type = %d\n", obj, mkcl_type_of(obj));

	    fflush(stderr);
	  }
      }
#endif

    mkcl_library_close_all(env);
    _mkcl_final_clean_up(env);

#if 0
    fprintf(stderr, "\n;; MKCL shutdown watchdog: exit status = %d\n", status); fflush(stderr);
#endif
    return status;
  }
}


/************************* ENVIRONMENT ROUTINES ***********************/

mkcl_index mkcl_argc(void)
{
  return ARGC;
}

mkcl_object
mk_si_argc(MKCL)
{
  @(return MKCL_MAKE_FIXNUM(ARGC));
}

mkcl_object
mkcl_argv(MKCL, mkcl_index index)
{
  if (index < ARGC)
    {
#ifdef MKCL_WINDOWS
      char * const str = ARGV[index];
      const size_t len = strlen(str);
      mkcl_UTF_8_object_sized(utf_8_obj, str, len);

      return mkcl_utf_8_to_string(env, (mkcl_object) &utf_8_obj);
#else
      return mkcl_cstring_to_string(env, ARGV[index]);
#endif
    }
  else
    mkcl_FEerror(env, "Out of range command line argument index: ~S. Must be between 0 and ~D inclusively.",
		 2, index, MKCL_MAKE_FIXNUM(ARGC - 1));    
}

mkcl_object
mk_si_argv(MKCL, mkcl_object index)
{
  mkcl_call_stack_check(env);
  if (MKCL_FIXNUMP(index)) {
    @(return mkcl_argv(env, mkcl_fixnum_to_word(index)));
  }
  mkcl_FEerror(env, "Invalid type for command line argument index: ~S. Must be a positive integer.", 1, index);
}

#ifdef MKCL_WINDOWS
void mkcl_get_commandline_args_from_Windows(int * argc_ref, char *** argv_ref)
{
  LPWSTR *wArgs;
  int nArgs;

  wArgs = CommandLineToArgvW(GetCommandLineW(), &nArgs);
  if (wArgs == NULL)
    {
      static char * empty_argv[] = { "", NULL }; /* This is the minimum required by the C standard. */
      *argv_ref = empty_argv;
      *argc_ref = 1;
    }
  else
    {
      char * (*new_argv) = malloc(sizeof(char*)*(nArgs));
      int i;
      
      for (i=0; i < nArgs; i++)
	{
	  int utf_8_len = WideCharToMultiByte(CP_UTF8, 0, wArgs[i], -1, NULL, 0, NULL, NULL);
	  
	  if (utf_8_len)
	    {
	      new_argv[i] = malloc(utf_8_len);
	      if (new_argv[i] == NULL)
		new_argv[i] = "";
	      else
		if (0 == WideCharToMultiByte(CP_UTF8, 0, wArgs[i], -1, new_argv[i], utf_8_len, NULL, NULL))
		  new_argv[i][0] = '\0'; /* truncate to an empty string if conversion fails. */
	    }
	  else
	    new_argv[i] = "";
	}
      
      LocalFree(wArgs);
      *argv_ref = new_argv;
      *argc_ref = nArgs;
    }
}
#endif /* MKCL_WINDOWS */


typedef void (*mkcl_unlock_process_env)();
typedef mkcl_unlock_process_env (*mkcl_lock_process_env)();

static mkcl_lock_process_env process_lock_unlock_callback = NULL;

void mkcl_set_process_env_locking_callbacks(mkcl_lock_process_env locker_unlocker)
{
  if (process_lock_unlock_callback)
    {
      mkcl_unlock_process_env old_unlocker = process_lock_unlock_callback();

      process_lock_unlock_callback = locker_unlocker;
      old_unlocker();
    }
  else
    process_lock_unlock_callback = locker_unlocker;
}

void mkcl_get_process_env_locking_callbacks(mkcl_lock_process_env * lock_unlock_callback_ptr)
{
  *lock_unlock_callback_ptr = process_lock_unlock_callback;
}


mkcl_object mkcl_getenv(MKCL, mkcl_object var)
{
  mkcl_OSstring_raw_type raw_os_value;
  mkcl_unlock_process_env unlocker = NULL;
  mkcl_dynamic_extent_OSstring(env, os_var, var);

  if (process_lock_unlock_callback) unlocker = process_lock_unlock_callback();
#ifdef MKCL_WINDOWS
  raw_os_value = _wgetenv(mkcl_OSstring_self(os_var));
#else
  raw_os_value = getenv((char *) mkcl_OSstring_self(os_var));
#endif
  if (unlocker) unlocker();

  if (raw_os_value)
    return mkcl_rawOSstring_to_string(env, raw_os_value);
  else
    return mk_cl_Cnil;
}


mkcl_object
mk_mkcl_getenv(MKCL, mkcl_object var)
{
  mkcl_call_stack_check(env);
  while (!MKCL_STRINGP(var))
    var = mkcl_type_error(env, @'mkcl::getenv', "argument", var, @'string');
  @(return mkcl_getenv(env, var));
}

mkcl_object
mkcl_setenv(MKCL, mkcl_object var, mkcl_object value)
{
#if !(defined(HAVE_SETENV) || defined(HAVE_PUTENV))
  @(return mk_cl_Cnil);
#else
#ifndef HAVE_SETENV
  mkcl_object os_var = mkcl_string_to_OSstring(env, var);
#else
  mkcl_dynamic_extent_OSstring(env, os_var, var);
#endif
  int ret_val = 0;

  if (value == mk_cl_Cnil) {
# ifdef HAVE_SETENV
    /* Remove the variable when setting to nil, so that
     * (si:setenv "foo" nil), then (si:getenv "foo") returns
     * the right thing. */
    unsetenv((char*)mkcl_OSstring_self(os_var));
# else
#  ifdef MKCL_WINDOWS
    mkcl_setenv(env, var, mkcl_core.empty_string);
#  else
    putenv((char*)mkcl_OSstring_self(os_var));
#  endif
# endif
  } else {
  mkcl_dynamic_extent_OSstring(env, os_value, value);

# ifdef HAVE_SETENV
    ret_val = setenv((char*)mkcl_OSstring_self(os_var), (char*)mkcl_OSstring_self(os_value), TRUE);
# else
    mkcl_OSstring_push_extend(env, os_var, '=');
    mkcl_OSstring_nconc(env, os_var, os_value);
    ret_val = putenv((char*)mkcl_OSstring_self(os_var));
# endif
  }
  if (ret_val)
    mkcl_FElibc_error(env, "MKCL:SETENV failed: var = ~S, value = ~S.", 2, var, value);
  @(return value);
#endif
}

mkcl_object
mk_mkcl_setenv(MKCL, mkcl_object var, mkcl_object value)
{
  mkcl_call_stack_check(env);
  while (!MKCL_STRINGP(var))
    var = mkcl_type_error(env, @'mkcl::setenv', "argument", var, @'string');
  if (!mkcl_Null(value))
    while (!MKCL_STRINGP(value))
      value = mkcl_type_error(env, @'mkcl::setenv', "argument", value, @'string');
  @(return mkcl_setenv(env, var, value));
}

mkcl_object
mk_si_gdb(MKCL)
{
  /* A simple do-nothing function that allows for an easy escape
     from the REPL down into gdb. */
  @(return mk_cl_Cnil);
}

