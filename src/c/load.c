/* -*- mode: c -*- */
/*
    load.d -- Binary loader.
*/
/*
    Copyright (c) 1990, Giuseppe Attardi and William F. Schelter.
    Copyright (c) 2001, Juan Jose Garcia Ripoll.
    Copyright (c) 2011-2017,2021-2022, Jean-Claude Beaudoin.

    MKCL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 3 of the License, or (at your option) any later version.

    See file '../../Copyright' for full details.
*/

#include <mkcl/mkcl.h>
#include <mkcl/internal.h>
#include <string.h>
#include <stdio.h>

#if MKCL_UNIX && HAVE_DLFCN_H
# include <dlfcn.h>
# define INIT_PREFIX "mkcl_init_fas_"
#endif

#if MKCL_WINDOWS
# include <windows.h>
# include <windef.h>
# include <winbase.h>
# include <io.h>
# include <tlhelp32.h>
# define INIT_PREFIX "mkcl_init_fas_"
#endif

#include <mkcl/mkcl-inl.h>


static mkcl_object
copy_object_file(MKCL, mkcl_object original)
{
  mkcl_base_string_object(copy_string_obj, "TMP:MKCL");
  mkcl_object copy_template = mk_cl_translate_logical_pathname(env, 1, (mkcl_object) &copy_string_obj);
  mkcl_object copy_stream = mk_mkcl_mkstemp(env, 3, copy_template, (mkcl_object) &MK_KEY_element_type, (mkcl_object) &MK_CL_unsigned_byte);
  mkcl_object copy_filename;

  if (mkcl_Null(copy_stream))
    {
      if (env->nvalues > 2)
	errno = mkcl_fixnum_to_word(env->values[2]);
      mkcl_FElibc_file_error(env, original, "Unable to create temporary file ~S to copy file ~S", 2, copy_template, original);
    }
  if (env->nvalues == 2)
    copy_filename = env->values[1];
  else
    copy_filename = mkcl_namestring(env, copy_stream, FALSE);

  mk_si_set_buffering_mode(env, copy_stream, (mkcl_object) &MK_KEY_none);

  /*
   * We either have to make a full copy to convince the loader to load this object
   * file again, or we want to retain the possibility of overwriting later on the
   * object file we loaded (case of Windows, which locks files that are loaded).
   */
#if 0 /* MKCL_WINDOWS */ /* #'si:copy-file is as fast now. */
  {
    int err;
    mkcl_dynamic_extent_OSstring(env, os_copy_filename, copy_filename);
    mkcl_dynamic_extent_OSstring(env, os_original, original);

    MKCL_LIBC_NO_INTR(env, err = !CopyFileW(mkcl_OSstring_self(os_original), mkcl_OSstring_self(os_copy_filename), 0));

    if (err) {
      mk_cl_delete_file(env, copy_filename);
      mkcl_FEerror(env, "Unable to copy file ~A to ~A", 2, original, copy_filename);
    }
  }
#else
  mk_mkcl_copy_file(env, original, copy_stream);
#endif
  mk_cl_close(env, 1, copy_stream);

  return copy_filename;
}

static mkcl_object
mkcl_library_find_by_name(MKCL, mkcl_object filename)
{
  mkcl_object l;
  for (l = mkcl_core.libraries; l != mk_cl_Cnil; l = MKCL_CONS_CDR(l)) {
    mkcl_object other = MKCL_CONS_CAR(l);
    mkcl_object name = other->cblock.name;
    if (!mkcl_Null(name) && mkcl_string_E(env, name, filename)) {
      return other;
    }
  }
  return mk_cl_Cnil;
}

static mkcl_object
mkcl_library_find_by_handle(void *handle)
{
  mkcl_object l;
  for (l = mkcl_core.libraries; l != mk_cl_Cnil; l = MKCL_CONS_CDR(l)) {
    mkcl_object other = MKCL_CONS_CAR(l);
    if (handle == other->cblock.handle) {
      return other;
    }
  }
  return mk_cl_Cnil;
}

mkcl_object
mkcl_library_open(MKCL, mkcl_object filename, bool force_reload)
{
  mkcl_object block;
  bool self_destruct = FALSE;
  /* char *filename_string; */

  /* Coerces to a file name but does not merge with cwd */
  filename = mk_si_coerce_to_filename(env, filename);

  if (!force_reload) {
    /* When loading a foreign library, such as a dll or a
     * so, it cannot contain any executable top level
     * code. In that case force_reload == FALSE and there is no
     * need to reload it if it has already been loaded. */
    block = mkcl_library_find_by_name(env, filename);
    if (!mkcl_Null(block)) {
      return block;
    }
  } else {
    /* We are using shared libraries as modules and
     * force_reload == TRUE.  Here we have to face the problem
     * that many operating systems do not allow to load a
     * shared library twice, even if it has changed. Hence
     * we have to make a unique copy to be able to load
     * the same FASL twice. In Windows this copy is
     * _always_ made because otherwise it cannot be
     * overwritten. In Unix we need only do that when the
     * file has been previously loaded. */
#if MKCL_WINDOWS
    filename = copy_object_file(env, filename);
    self_destruct = TRUE;
#else
    block = mkcl_library_find_by_name(env, filename);
    if (!mkcl_Null(block)) {
      filename = copy_object_file(env, filename);
      self_destruct = TRUE;
    }
#endif
  }
  block = mkcl_alloc_raw_codeblock(env);
  block->cblock.self_destruct = self_destruct;
  block->cblock.locked = 0;
  block->cblock.handle = NULL;
  block->cblock.entry = NULL;
  block->cblock.data = NULL;
  block->cblock.data_size = 0;
  block->cblock.temp_data = NULL;
  block->cblock.temp_data_size = 0;
  block->cblock.data_text = NULL;
  block->cblock.data_text_size = 0;
  block->cblock.name = filename;
  block->cblock.next = mk_cl_Cnil;
  block->cblock.links = mk_cl_Cnil;
  block->cblock.cfuns_size = 0;
  block->cblock.cfuns = NULL;
  block->cblock.source = mk_cl_Cnil;
  block->cblock.fun_ref_syms = NULL;
  block->cblock.fun_refs = NULL;
  block->cblock.cfun_objs = NULL;

#if 0
  mkcl_object os_filename = mkcl_string_to_OSstring(env, filename);
#else
  mkcl_dynamic_extent_OSstring(env, os_filename, filename);
#endif

#if MKCL_UNIX
  MKCL_LIBC_NO_INTR(env, block->cblock.handle = dlopen((char *) mkcl_OSstring_self(os_filename), RTLD_NOW|RTLD_GLOBAL));
  if (block->cblock.handle == NULL)
    return block;
#elif MKCL_WINDOWS
  {
    mkcl_interrupt_status old_intr;

    mkcl_get_interrupt_status(env, &old_intr);
    mkcl_disable_interrupts(env);
    {
      UINT error = SetErrorMode(SetErrorMode(0) || SEM_NOOPENFILEERRORBOX);
      block->cblock.handle = LoadLibraryW(mkcl_OSstring_self(os_filename));
      SetErrorMode(error);
    }
    mkcl_set_interrupt_status(env, &old_intr);

    if (block->cblock.handle == NULL)
      return block;
  }
#else
# error "mkcl_library_open is imcomplete on this platform."
#endif

  /*
   * A second pass to ensure that the dlopen routine has not
   * returned a library that we had already loaded. If this is
   * the case, we close the new copy to ensure we do refcounting
   * right.
   *
   * INV: We can modify "libraries" in a multithread environment
   * because we have already taken the +load-compile-lock+
   */
  {
    mkcl_object other = mkcl_library_find_by_handle(block->cblock.handle);
    if (other != mk_cl_Cnil) {
      mkcl_library_close(env, block);
      block = other;
    } else {
      mk_si_set_finalizer(env, block, mk_cl_Ct);
    }
  }

  return block;
}

void *
mkcl_library_symbol(MKCL, mkcl_object block, const char *symbol, bool lock) /* must be called with mt::+load-compile-lock+ held! */
{
  void *p;

  if (block == ((mkcl_object) &MK_KEY_default))
    {
      mkcl_object l;
      for (l = mkcl_core.libraries; l != mk_cl_Cnil; l = MKCL_CONS_CDR(l)) {
	mkcl_object block = MKCL_CONS_CAR(l);
	p = mkcl_library_symbol(env, block, symbol, lock);
	if (p) return p;
      }
#if MKCL_WINDOWS
      {
	mkcl_interrupt_status old_intr;

	HANDLE hndSnap = NULL;
	HANDLE hnd = NULL;
      
	mkcl_get_interrupt_status(env, &old_intr);
	mkcl_disable_interrupts(env);
	hndSnap = CreateToolhelp32Snapshot(TH32CS_SNAPMODULE, GetCurrentProcessId());
	if (hndSnap != INVALID_HANDLE_VALUE)
	  {
	    MODULEENTRY32 me32;
	    me32.dwSize = sizeof(MODULEENTRY32);
	    if (Module32First(hndSnap, &me32))
	      {
		do
		  hnd = GetProcAddress(me32.hModule, symbol);
		while (hnd == NULL && Module32Next(hndSnap, &me32));
	      }
	    /* GetLastError() == ERROR_NO_MORE_FILES ?? JCB */
	    CloseHandle(hndSnap);
	    hndSnap = NULL;
	  }
	p = (void*)hnd;
	mkcl_set_interrupt_status(env, &old_intr);
      }
#elif MKCL_UNIX
      /* Is this a valid call? 0 looks suspicious. JCB */
      MKCL_LIBC_NO_INTR(env, (dlerror(), p = dlsym(0, symbol)));
#else /* !MKCL_WINDOWS && !MKCL_UNIX */
# error "mkcl_library_symbol is imcomplete on this platform."
      p = NULL;
#endif
    }
  else
    {
#if MKCL_UNIX
      MKCL_LIBC_NO_INTR(env, (dlerror(), p = dlsym(block->cblock.handle, symbol)));
#elif MKCL_WINDOWS
      {
	HMODULE h = (HMODULE)(block->cblock.handle);
	MKCL_LIBC_NO_INTR(env, p = GetProcAddress(h, symbol));
      }
#else
# error "mkcl_library_symbol is incomplete on this platform."
#endif
      /* Libraries whose symbols are being referenced by the FFI should not
       * get garbage collected. Until we find a better solution we simply lock
       * them for the rest of the runtime */
      if (p) {
	block->cblock.locked |= lock;
      }
    }
  return p;
}

mkcl_object
mkcl_library_error(MKCL, mkcl_object block) 
{
  mkcl_object output;
#if MKCL_UNIX
  MKCL_LIBC_NO_INTR(env, output = mkcl_cstring_to_string(env, dlerror()));
#elif MKCL_WINDOWS
  {
    mkcl_interrupt_status old_intr;
    void * message;
    DWORD error_code;
    DWORD msg_size;

    mkcl_get_interrupt_status(env, &old_intr);
    mkcl_disable_interrupts(env);
    error_code = GetLastError();
    if ((msg_size = FormatMessageW(FORMAT_MESSAGE_FROM_SYSTEM |
				   FORMAT_MESSAGE_ALLOCATE_BUFFER,
				   0, error_code, 0, (LPWSTR) &message, 0, NULL)))
      {
	wchar_t * msg_ctl = L"(%d) %s";
#if 1
	mkcl_VLA(env, wchar_t, buf, wcslen(message) + sizeof(msg_ctl) + 15);
#else
	wchar_t buf[wcslen(message) + sizeof(msg_ctl) + 15]; /* a VLA. */
#endif
	
	wsprintfW(buf, msg_ctl, error_code, message);
	output = mkcl_cstring16_to_string(env, buf);
	LocalFree(message);
      }
    else
      output = mkcl_make_simple_base_string(env, "");
    mkcl_set_interrupt_status(env, &old_intr);
  }
#else
# error "mkcl_library_error is incomplete on this platform."
#endif
  return output;
}

bool mkcl_library_close(MKCL, mkcl_object block)
{
  static const mkcl_base_string_object(anonymous_str_obj, "<anonymous>");
  mkcl_object block_name = block->cblock.name;
  /* const char *filename; */
  bool verbose = mkcl_symbol_value(env, (mkcl_object) &MK_SI_DYNVAR_gc_verbose) != mk_cl_Cnil;
  bool error = FALSE;

  if (mkcl_Null(block_name))
    block_name = (mkcl_object) &anonymous_str_obj;

#if 0
  mkcl_object os_block_name = mkcl_string_to_OSstring(env, block_name);
#else
  mkcl_dynamic_extent_OSstring(env, os_block_name, block_name);
#endif

  if (block->cblock.handle != NULL)
    {
      if (verbose) {
#if MKCL_WINDOWS
	fwprintf(stderr, L";;; Freeing library %s\n", mkcl_OSstring_self(os_block_name));
#else
	fprintf(stderr, ";;; Freeing library %s\n", mkcl_OSstring_self(os_block_name));
#endif
      }
#if MKCL_UNIX
      MKCL_LIBC_NO_INTR(env, error = dlclose(block->cblock.handle));
#elif MKCL_WINDOWS
      MKCL_LIBC_NO_INTR(env, error = !FreeLibrary(block->cblock.handle));
#else
# error "mkcl_library_close is incomplete on this platform."
#endif
      if (!error)
	block->cblock.handle = NULL; /* In an attempt to prevent closing the same library twice. JCB */
    }
  if (block->cblock.self_destruct)
    {
#if MKCL_WINDOWS
      if (verbose) 
	fwprintf(stderr, L";;; Removing library file %s\n", mkcl_OSstring_self(os_block_name));
      
      if (_wunlink(mkcl_OSstring_self(os_block_name)) && verbose)
	_wperror(L";;;*** Removal of library file failed");
#else
      if (verbose) {
	fprintf(stderr, ";;; Removing library file %s\n", mkcl_OSstring_self(os_block_name));
      }
      if (unlink((char *) mkcl_OSstring_self(os_block_name)) && verbose)
	perror(";;;*** Removal of library file failed");
#endif
    }
  return error;
}

void
mkcl_library_close_all(MKCL)
{
  mkcl_object l;
  for (l = mkcl_core.libraries; l != mk_cl_Cnil; l = MKCL_CONS_CDR(l)) {
    mkcl_object block = MKCL_CONS_CAR(l);
    (void) mkcl_library_close(env, block);
  }
}

struct mkcl_cfun mk_si_load_binary_cfunobj = MKCL_CFUN4(mk_si_load_binary, (mkcl_object) &MK_SI_load_binary);

mkcl_object
mk_si_load_binary(MKCL, mkcl_object filename, mkcl_object verbose, mkcl_object print, mkcl_object external_format)
{
  mkcl_object l_c_lock;
  volatile mkcl_object locked = mk_cl_Cnil;
  volatile mkcl_object output = mk_cl_Cnil;

  mkcl_call_stack_check(env);

  /* We need the full pathname */
  filename = mk_cl_truename(env, filename);

  /* Loading binary code is not thread safe. When another thread tries
     to load the same file, we may end up initializing twice the same
     module. */
  l_c_lock = mkcl_symbol_value(env, (mkcl_object) &MK_MT_CONSTANT_load_compile_lock);
  MKCL_UNWIND_PROTECT_BEGIN(env) {
    mkcl_object block;
    mkcl_object basename;
    mkcl_object prefix;

    MKCL_NO_INTR(env, locked = mk_mt_get_lock(env, 1, l_c_lock));

    /* Try to load shared object file */
    block = mkcl_library_open(env, filename, 1);
    if (block->cblock.handle == NULL) {
      output = mkcl_library_error(env, block);
      goto OUTPUT;
    } else
      mkcl_core.libraries = mkcl_adjoin_eq(env, block, mkcl_core.libraries);

    /* Fist try to call "mkcl_init_fas_CODE()" */
    block->cblock.entry = mkcl_library_symbol(env, block, INIT_PREFIX "CODE", 0);
    if (block->cblock.entry != NULL)
      goto GO_ON;

    /* Next try to call "init_FILE()" where FILE is the file name */
    prefix = mkcl_symbol_value(env, (mkcl_object) &MK_SI_DYNVAR_init_function_prefix);
    if (mkcl_Null(prefix))
      prefix = mkcl_make_simple_base_string(env, INIT_PREFIX);
    else
      prefix = mk_si_concatenate_base_strings(env, 3,
                                              mkcl_make_simple_base_string(env, INIT_PREFIX),
                                              prefix,
                                              mkcl_make_simple_base_string(env, "_"));
    basename = mk_cl_pathname_name(env, 1,filename); /* This code cannot work with Unicode file names! JCB */
    basename = mk_si_concatenate_base_strings(env, 2, 
                                              prefix,
                                              mk_cl_string_upcase(env, 1,
                                                                  mkcl_funcall3(env,
                                                                                MK_CL_nsubstitute.gfdef,
                                                                                MKCL_CODE_CHAR('_'),
                                                                                MKCL_CODE_CHAR('-'),
                                                                                basename)));
    block->cblock.entry = mkcl_library_symbol(env, block, (char*)basename->base_string.self, 0);

    if (block->cblock.entry == NULL) {
      static const mkcl_base_string_object(s_obj, "This file is probably not a proper MKCL binary file: ");
      output = mkcl_library_error(env, block);
      output = mkcl_concatenate_2_strings(env, (mkcl_object) &s_obj, output);
      mkcl_library_close(env, block);
      goto OUTPUT;
    }

    /* Finally, perform initialization */
  GO_ON:	
    mkcl_read_VV(env, block, block->cblock.entry, filename);
    output = mk_cl_Cnil;
  OUTPUT: ;
  } MKCL_UNWIND_PROTECT_EXIT {
    if (!mkcl_Null(locked)) mk_mt_giveup_lock(env, l_c_lock);
  } MKCL_UNWIND_PROTECT_END;
  mkcl_return_value(output);
}

struct mkcl_cfun mk_si_load_source_cfunobj = MKCL_CFUN4(mk_si_load_source, (mkcl_object) &MK_SI_load_source);

mkcl_object
mk_si_load_source(MKCL, mkcl_object source, mkcl_object verbose, mkcl_object print, mkcl_object external_format)
{
  mkcl_call_stack_check(env);
  volatile mkcl_object locked = mk_cl_Cnil;
  mkcl_object l_c_lock;
  mkcl_object x, strm;
  mkcl_object form_end_position = MKCL_MAKE_FIXNUM(0);
  mkcl_object location = MKCL_CONS(env, source, form_end_position);

  /* Source may be either a stream or a filename (a.k.a. pathname_designator) */
  if (MKCL_PATHNAMEP(source) || MKCL_STRINGP(source)) /* What about closed streams? They cause an error right now. Right? JCB */
    {
#if 0
      strm = mkcl_open_stream(env, source, mkcl_smm_input, mk_cl_Cnil, mk_cl_Cnil, mk_cl_Cnil,
			      MKCL_STREAM_TEXT | MKCL_STREAM_C_STDIO_STREAM, external_format);
#else
      strm = mkcl_open_stream(env, source, mkcl_smm_input, mk_cl_Cnil, mk_cl_Cnil, mk_cl_Cnil, external_format);
#endif
      if (mkcl_Null(strm))
	mkcl_return_value(mk_cl_Cnil); /* Silently?! JCB */
    }
  else
    strm = source;     /* INV: if "source" is not a valid stream, file.d will complain */
    
  l_c_lock = mkcl_symbol_value(env, (mkcl_object) &MK_MT_CONSTANT_load_compile_lock);
  MKCL_UNWIND_PROTECT_BEGIN(env) {
    mkcl_interrupt_status old_intr;

    mkcl_get_interrupt_status(env, &old_intr);
    mkcl_disable_interrupts(env);
    locked = mk_mt_get_lock(env, 1, l_c_lock);
    mkcl_set_interrupt_status(env, &old_intr);

    mkcl_bds_bind(env, (mkcl_object) &MK_SI_DYNVAR_source_location, location);
    for (;;) {
      x = mk_si_read_object_or_ignore(env, strm, MKCL_OBJNULL);
      form_end_position = mkcl_file_position(env, strm);
      MKCL_RPLACD(location, form_end_position);
      if (x == MKCL_OBJNULL)
	break;
      if (env->nvalues) {
	mk_si_eval_in_env(env, 1, x);
	if (print != mk_cl_Cnil) {
	  mk_cl_write(env, 1, x);
	  mk_cl_terpri(env, 0);
	}
      }
    }
    mkcl_bds_unwind1(env);
  } MKCL_UNWIND_PROTECT_EXIT {
    if (!mkcl_Null(locked)) mk_mt_giveup_lock(env, l_c_lock);
    /* We do not want to come back here if close_stream fails,
       therefore, first we frs_pop() current jump point, then
       try to close the stream, and then jump to next catch
       point */
    if (strm != source)
      mk_cl_close(env, 3, strm, (mkcl_object) &MK_KEY_abort, mk_cl_Ct);
  } MKCL_UNWIND_PROTECT_END;
  mkcl_return_value(mk_cl_Cnil);
}

struct mkcl_cfun mk_cl_load_cfunobj = MKCL_CFUN_VA(mk_cl_load, (mkcl_object) &MK_CL_load);

mkcl_object mk_cl_load(MKCL, mkcl_narg narg, mkcl_object source, ...)
{
  mkcl_call_stack_check(env);
  {
    mkcl_object pathname = mk_cl_Cnil;
    mkcl_object pntype = mk_cl_Cnil;
    mkcl_object hooks = mk_cl_Cnil;
    mkcl_object filename = mk_cl_Cnil;
    mkcl_object function = mk_cl_Cnil;
    mkcl_object ok = mk_cl_Cnil;
    bool not_a_filename = 0;

    mkcl_object verbose = mkcl_symbol_value(env, (mkcl_object) &MK_CL_DYNVAR_load_verbose);
    mkcl_object print = mkcl_symbol_value(env, (mkcl_object) &MK_CL_DYNVAR_load_print);
    mkcl_object if_does_not_exist = (mkcl_object) &MK_KEY_error;
    mkcl_object external_format = (mkcl_object) &MK_KEY_default;
    mkcl_object search_list = mkcl_symbol_value(env, (mkcl_object) &MK_SI_DYNVAR_load_search_list);
    struct mkcl_key_param_spec key_params[] =
      {
       { (mkcl_object) &MK_KEY_verbose, &verbose, false },
       { (mkcl_object) &MK_KEY_print, &print, false },
       { (mkcl_object) &MK_KEY_if_does_not_exist, &if_does_not_exist, false },
       { (mkcl_object) &MK_KEY_external_format, &external_format, false },
       { (mkcl_object) &MK_KEY_search_list, &search_list, false },
      };
    MKCL_RECEIVE_N_KEYWORD_ARGUMENTS(env, (mkcl_object) &MK_CL_load, narg, 1, source, key_params);

    ok = mk_cl_Cnil;
    /* If source is a stream, read conventional lisp code from it */
    if (mkcl_type_of(source) != mkcl_t_pathname && !mkcl_stringp(env, source)) {
      /* INV: if "source" is not a valid stream, file.d will complain */
      filename = source;
      function = mk_cl_Cnil;
      not_a_filename = 1;
      goto NOT_A_FILENAME;
    }
    /* INV: mkcl_coerce_to_file_pathname() creates a fresh new pathname object */
#if 0
    source   = mk_cl_merge_pathnames(env, 1, source); /* will be done by mkcl_coerce_to_file_pathname(). */
#endif
    pathname = mkcl_coerce_to_file_pathname(env, source);
    pntype   = pathname->pathname.type;
  
    filename = mk_cl_Cnil;
    hooks = mkcl_symbol_value(env, (mkcl_object) &MK_SI_DYNVAR_load_hooks);
    if (mkcl_Null(pathname->pathname.directory) &&
        (mkcl_Null(pathname->pathname.host) || mkcl_string_E(env, pathname->pathname.host, mkcl_core.localhost_string)) &&
        (mkcl_Null(pathname->pathname.device) || (pathname->pathname.device == ((mkcl_object) &MK_KEY_unspecific))) &&
        !mkcl_Null(search_list))
      {
        mkcl_loop_for_in(env, search_list) {
          mkcl_object d = MKCL_CAR(search_list);
          mkcl_object f = mk_cl_merge_pathnames(env, 2, pathname, d);
          mkcl_object ok = mk_cl_load(env, 11,
                                      f,
                                      (mkcl_object) &MK_KEY_verbose, verbose,
                                      (mkcl_object) &MK_KEY_print, print,
                                      (mkcl_object) &MK_KEY_if_does_not_exist, mk_cl_Cnil,
                                      (mkcl_object) &MK_KEY_external_format, external_format,
                                      (mkcl_object) &MK_KEY_search_list, mk_cl_Cnil);
          if (!mkcl_Null(ok)) {
            mkcl_return_value(ok);
          }
        } mkcl_end_loop_for_in;
      }
    if (!mkcl_Null(pntype) && (pntype != ((mkcl_object) &MK_KEY_wild))) {
      /* If filename already has an extension, make sure that the file exists */
      filename = mk_si_coerce_to_filename(env, pathname);
      if (mkcl_Null(mk_cl_probe_file(env, filename)))
        filename = mk_cl_Cnil;
      else
        {
          mkcl_object kind = mk_si_file_kind(env, 3, filename, (mkcl_object) &MK_KEY_follow_symlinks, mk_cl_Ct);
          if (kind != ((mkcl_object) &MK_KEY_file) && kind != ((mkcl_object) &MK_KEY_special)) {
            /* :special really!? What is hiding under that? A pipe, a socket maybe?
               :special is probably too broad. JCB */
            filename = mk_cl_Cnil;
          } else {
            function = mk_cl_cdr(env, mkcl_assoc(env, pathname->pathname.type, hooks));
          }
        }
    } else mkcl_loop_for_in(env, hooks) {
        /* Otherwise try with known extensions until a matching file is found */
        filename = pathname;
        filename->pathname.type = MKCL_CAAR(hooks);
        function = MKCL_CDAR(hooks);
        if (mkcl_Null(mk_cl_probe_file(env, filename)))
          filename = mk_cl_Cnil;
        else
          {
            mkcl_object kind = mk_si_file_kind(env, 3, filename, (mkcl_object) &MK_KEY_follow_symlinks, mk_cl_Ct);
            if (kind == ((mkcl_object) &MK_KEY_file) || kind == ((mkcl_object) &MK_KEY_special))
              /* :special really!? What is hiding under that? A pipe, a socket maybe?
                 :special is probably too broad. JCB */
              break;
            else
              filename = mk_cl_Cnil;
          }
      } mkcl_end_loop_for_in;
    if (mkcl_Null(filename)) {
      if (mkcl_Null(if_does_not_exist))
        { mkcl_return_value(mk_cl_Cnil); }
      else
        mkcl_FEcannot_open(env, source);
    }
  NOT_A_FILENAME:
    if (verbose != mk_cl_Cnil) {
      static const mkcl_base_string_object(loading_str_obj, "~&;;; Loading ~s~%");
      mk_cl_format(env, 3, mk_cl_Ct, (mkcl_object) &loading_str_obj, filename);
    }
    mkcl_bds_bind(env, (mkcl_object) &MK_CL_DYNVAR_package, mkcl_symbol_value(env, (mkcl_object) &MK_CL_DYNVAR_package));
    mkcl_bds_bind(env, (mkcl_object) &MK_CL_DYNVAR_readtable, mkcl_symbol_value(env, (mkcl_object) &MK_CL_DYNVAR_readtable));
    mkcl_bds_bind(env, (mkcl_object) &MK_CL_DYNVAR_load_pathname, not_a_filename ? mk_cl_Cnil : source);
    mkcl_bds_bind(env, (mkcl_object) &MK_CL_DYNVAR_load_truename, mk_cl_Cnil);
    mkcl_bds_push(env, (mkcl_object) &MK_SI_DYNVAR_dynamic_cons_stack);
    mkcl_bds_push(env, (mkcl_object) &MK_CL_DYNVAR_default_pathname_defaults);
    mkcl_bds_push(env, (mkcl_object) &MK_CLOS_DYNVAR_redefine_class_in_place);
    MKCL_SETQ(env, (mkcl_object) &MK_CL_DYNVAR_load_truename, (not_a_filename ? mk_cl_Cnil : (filename = mk_cl_truename(env, filename))));

    if (!mkcl_Null(function)) {
      mkcl_object l_c_lock = mkcl_symbol_value(env, (mkcl_object) &MK_MT_CONSTANT_load_compile_lock);
      volatile mkcl_object locked = mk_cl_Cnil;
      MKCL_UNWIND_PROTECT_BEGIN(env) {
        mkcl_interrupt_status old_intr;

        mkcl_get_interrupt_status(env, &old_intr);
        mkcl_disable_interrupts(env);
        locked = mk_mt_get_lock(env, 1, l_c_lock);
        mkcl_set_interrupt_status(env, &old_intr);

        ok = mkcl_funcall4(env, function, filename, verbose, print, external_format);
      } MKCL_UNWIND_PROTECT_EXIT {
        if (!mkcl_Null(locked)) mk_mt_giveup_lock(env, l_c_lock);
      } MKCL_UNWIND_PROTECT_END;
    } else {
      if (not_a_filename) {
        ok = mk_cl_Ct;
      } else {
        ok = mk_si_load_binary(env, filename, verbose, print, external_format);
      }
      if (!mkcl_Null(ok))
        ok = mk_si_load_source(env, filename, verbose, print, external_format);
    }
    mkcl_bds_unwind_n(env, 7);

    if (!mkcl_Null(ok))
      mkcl_FEerror(env, "LOAD: Could not load file ~S (Error: ~S)", 2, filename, ok);
    if (print != mk_cl_Cnil) {
      static const mkcl_base_string_object(loaded_str_obj, "~&;;; Loaded ~s~%");
      mk_cl_format(env, 3, mk_cl_Ct, (mkcl_object) &loaded_str_obj, filename);
    }
    mkcl_return_value(filename);
  }
}

struct mkcl_cfun mk_si_list_libraries_cfunobj = MKCL_CFUN0(mk_si_list_libraries, (mkcl_object) &MK_SI_list_libraries);

mkcl_object mk_si_list_libraries(MKCL)
{
  mkcl_call_stack_check(env);
  volatile mkcl_object output = mk_cl_Cnil;
  volatile mkcl_object locked = mk_cl_Cnil;
  mkcl_object l_c_lock = mkcl_symbol_value(env, (mkcl_object) &MK_MT_CONSTANT_load_compile_lock);

  MKCL_UNWIND_PROTECT_BEGIN(env) {
    MKCL_NO_INTR(env, locked = mk_mt_get_lock(env, 1, l_c_lock));
    output = mk_cl_copy_list(env, mkcl_core.libraries);
  } MKCL_UNWIND_PROTECT_EXIT {
    if (!mkcl_Null(locked)) mk_mt_giveup_lock(env, l_c_lock);
  } MKCL_UNWIND_PROTECT_END;

  mkcl_return_value(output);
}

