/* -*- mode: c -*- */
/*
    init.c  -- Lisp Initialization.
*/
/*
    Copyright (c) 1990, Giuseppe Attardi.
    Copyright (c) 2001, Juan Jose Garcia Ripoll.
    Copyright (c) 2011-2012, Jean-Claude Beaudoin.

    MKCL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 3 of the License, or (at your option) any later version.

    See file '../../Copyright' for full details.
*/

#include <mkcl/mkcl.h>
#include <mkcl/internal.h>

#include <stdio.h>
#include <stdlib.h>
/*
 * HOOKS.
 *
 * The following functions are only used to bootstrap MKCL. They divert
 * the calls to the interpreted code which is loaded by bare.lsp. Once
 * the whole of MKCL is built, the file cinit.o will be replaced by the
 * actual initialization code, and the compiled function will be
 * called instead.
 */

extern mkcl_object
mk_cl_upgraded_array_element_type(MKCL, mkcl_narg narg, mkcl_object type, ...)
{
  mkcl_call_stack_check(env);
  return mkcl_funcall1(env, @+'upgraded-array-element-type', type);
}

extern mkcl_object
mk_si_safe_eval(MKCL, mkcl_narg arg, mkcl_object form, mkcl_object lex_env, mkcl_object error_value, ...)
{
  mkcl_call_stack_check(env);
  return mkcl_funcall3(env, @+'si::safe-eval', form, lex_env, error_value);
}

extern mkcl_object
mk_cl_array_dimensions(MKCL, mkcl_object array)
{
  mkcl_call_stack_check(env);
  return mkcl_funcall1(env, @+'ARRAY-DIMENSIONS', array);
}

extern mkcl_object
mk_cl_vector_push_extend(MKCL, mkcl_narg narg, mkcl_object elt, mkcl_object vector, ...)
{
  mkcl_index fp;

  mkcl_call_stack_check(env);
  if (narg != 2) {
    mkcl_FEerror(env, "Too many arguments to interim mk_cl_vector_push_extend (cinit.d)", 0);
  }
  fp = vector->vector.fillp;
  if (fp < vector->vector.dim) {
    vector->vector.fillp = fp+1;
    vector->vector.self.t[fp+1] = elt;
    @(return MKCL_MAKE_FIXNUM(fp));
  }
  return mkcl_funcall2(env, @+'VECTOR-PUSH-EXTEND', elt, vector);
}

extern mkcl_object
mk_si_find_relative_package(MKCL, mkcl_narg narg, mkcl_object package, ...)
{
  @(return mk_cl_Cnil);
}

static mkcl_object mk_si_simple_toplevel(MKCL)
{
  mkcl_object output = mkcl_core.standard_output;
  mkcl_object sentence;
  int i;

  mkcl_call_stack_check(env);
  /* Simple minded top level loop */
  mkcl_write_cstr(env, "\n;*** MKCL core booted ****\n", output);
  mkcl_force_output(env, output);
  for (i = 1; i<mkcl_fixnum_to_word(mk_mkcl_argc(env)); i++) {
    mkcl_object arg = mk_mkcl_argv(env, MKCL_MAKE_FIXNUM(i));
#if 0
    mk_cl_load(env, 1, arg);
#else
    mk_cl_load(env, 3, arg, @':external-format', mk_cl_list(env, 2, @':ascii', @':lf'));
#endif
  }
  while (1) {
    mkcl_write_cstr(env, "\n> ", output);
    sentence = @read(env, 3, mk_cl_Cnil, mk_cl_Cnil, MKCL_OBJNULL);
    if (sentence == MKCL_OBJNULL)
      { @(return); }
    mkcl_prin1(env, mk_si_eval_in_env(env, 1, sentence), output);
  }
}

mkcl_object mk_si_shutdown_mkcl(MKCL, mkcl_object code, mkcl_object watchdog_thread, mkcl_object verbose, mkcl_object clean)
{ /* Bootstrap stub. */
  return mk_cl_Cnil;
}

int
main(int argc, char **args)
{
  mkcl_object top_level, features;
  struct mkcl_thread_init_parameters init_params = { 0 };

  const mkcl_env env = mkcl_boot(argc, args, &init_params);

  if (env == NULL)
    return(errno); /* boot failed */
  else
    {
      MKCL_CATCH_ALL_BEGIN(env) {
	MKCL_SETUP_CALL_STACK_ROOT_GUARD(env);
	MKCL_SETQ(env, @'*load-verbose*', mk_cl_Cnil);
	MKCL_SETQ(env, @'*package*', mkcl_core.system_package);
	
	features = mkcl_symbol_value(env, @'*features*');
	features = MKCL_CONS(env, mkcl_make_keyword(env, "MKCL-MIN"), features);
	MKCL_SET(@'*features*', features);

	MKCL_SETQ(env, @'clos::*redefine-class-in-place*', mk_cl_Ct); /* a concession to the old single-threaded days */

	_mkcl_intern(env, "INITIAL", mkcl_core.system_package);
	top_level = _mkcl_intern(env, "TOP-LEVEL", mkcl_core.system_package);
	mkcl_def_c_function(env, top_level, mk_si_simple_toplevel, 0);
	mkcl_funcall0(env, top_level);
      } MKCL_CATCH_ALL_IF_CAUGHT { /* The execution was abnormally terminated. */
	return mkcl_exit_status(env);
      } MKCL_CATCH_ALL_END;
      return(0);
    }
}

#ifdef __cplusplus
extern "C" void mkcl_init_lib_LSP(MKCL, mkcl_object, mkcl_object);
#endif

void mkcl_init_lib_LSP(MKCL, mkcl_object o, mkcl_object f) {}

