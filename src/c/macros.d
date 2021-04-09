/* -*- mode: c -*- */
/*
    macros.c -- Macros.
*/
/*
    Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
    Copyright (c) 1990, Giuseppe Attardi.
    Copyright (c) 2001, Juan Jose Garcia Ripoll.

    MKCL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 3 of the License, or (at your option) any later version.

    See file '../../Copyright' for full details.
*/

#include <mkcl/mkcl.h>
#include <mkcl/internal.h>

/******************************* ------- ******************************/
/*
 * The are two kinds of lisp environments. One of them is by the interpreter
 * when executing bytecode and it contains local variable and function
 * definitions.
 *
 * The other environment is shared by the bytecode compiler and by the C
 * compiler and it contains information for the compiler, including local
 * variable definitions, and local function and macro definitions. The
 * structure is as follows:
 *
 *	env -> ( var-list . fun-list )
 *	fun-list -> ( { definition | atomic-marker }* )
 *	definition -> ( macro-name SI::MACRO { extra-data }* )
 *		    | ( function-name FUNCTION { extra-data }* )
 *		    | ( a-symbol anything { extra-data }* )
 *	atomic-marker -> CB | LB
 *
 * The main difference between the bytecode and C compilers is on the extra
 * information. On the other hand, both environments are similar enough that
 * the functions MACROEXPAND-1, MACROEXPAND and MACRO-FUNCTION can find the
 * required information.
 */

static mkcl_object
search_symbol_macro(MKCL, mkcl_object name, mkcl_object lex_env)
{
  if (MKCL_CONSP(lex_env)) /* currently the environment object is a cons. */
    for (lex_env = MKCL_CAR(lex_env); MKCL_CONSP(lex_env); lex_env = MKCL_CDR(lex_env))
      {
        mkcl_object record = MKCL_CAR(lex_env);
        if (MKCL_CONSP(record) && MKCL_CAR(record) == name) {
          if (mk_cl_cadr(env, record) == @'si::symbol-macro')
            return mk_cl_caddr(env, record);
          return mk_cl_Cnil;
        }
      }
  return mk_si_get_sysprop(env, name, @'si::symbol-macro');
}

static mkcl_object
search_macro_function(MKCL, mkcl_object name, mkcl_object lex_env)
{
  int type = mkcl_symbol_type(env, name);
  if (MKCL_CONSP(lex_env)) {  /* currently the environment object is a cons. */
    for (lex_env = MKCL_CDR(lex_env); MKCL_CONSP(lex_env); lex_env = MKCL_CDR(lex_env))
      {
        mkcl_object record = MKCL_CAR(lex_env);
        /* When the environment has been produced by the
           compiler, there might be atoms/symbols signalling
           closure and block boundaries. */
        if (MKCL_CONSP(record) && MKCL_CAR(record) == name)
          {
            const mkcl_object tag = mk_cl_cadr(env, record);
            if (tag == @'si::macro')
              return mk_cl_caddr(env, record);
            else
              return mk_cl_Cnil;
          }
      }
  }
  if (type & mkcl_stp_macro)
    return MKCL_SYM_FUN(name);
  else
    return mk_cl_Cnil;
}

@(defun macro_function (sym &optional lex_env)
@
  mkcl_return_value((search_macro_function(env, sym, lex_env)));
@)

/*
	Analyze a form and expand it once if it is a macro form.
	MKCL_VALUES(0) contains either the expansion or the original form.
	MKCL_VALUES(1) is true when there was a macroexpansion.
*/

@(defun macroexpand_1 (form &optional (lex_env mk_cl_Cnil))
  mkcl_object exp_fun = mk_cl_Cnil;
@
  if (MKCL_ATOM(form))
    {
      if (MKCL_SYMBOLP(form))
	exp_fun = search_symbol_macro(env, form, lex_env);
    }
  else
    {
      mkcl_object head = MKCL_CAR(form);
      if (MKCL_SYMBOLP(head))
	exp_fun = search_macro_function(env, head, lex_env);
    }
  if (!mkcl_Null(exp_fun)) {
    mkcl_object hook = mkcl_symbol_value(env, @'*macroexpand-hook*');
    if (hook == @'funcall')
      form = mkcl_funcall2(env, exp_fun, form, lex_env);
    else
      form = mkcl_funcall3(env, hook, exp_fun, form, lex_env);
  }
  mkcl_return_2_values(form, exp_fun);
@)

/*
	Expands a form as many times as possible and returns the
	finally expanded form.
*/
@(defun macroexpand (form &optional lex_env)
  mkcl_object done, old_form;
@
  done = mk_cl_Cnil;
  do {
    form = mk_cl_macroexpand_1(env, 2, old_form = form, lex_env);
    if (MKCL_VALUES(1) == mk_cl_Cnil) {
      break;
    } else if (old_form == form) {
      mkcl_FEerror(env, "Infinite loop when expanding macro form ~A", 1, old_form);
    } else {
      done = mk_cl_Ct;
    }
  } while (1);
  mkcl_return_2_values(form, done);
@)

static mkcl_object
or_macro(MKCL, mkcl_object whole, mkcl_object lex_env)
{
  mkcl_object output = mk_cl_Cnil;
  whole = MKCL_CDR(whole);
  if (mkcl_Null(whole))	/* (OR) => NIL */
    mkcl_return_value(mk_cl_Cnil);
  while (!mkcl_Null(MKCL_CDR(whole))) {
    output = MKCL_CONS(env, MKCL_CONS(env, MKCL_CAR(whole), mk_cl_Cnil), output);
    whole = MKCL_CDR(whole);
  }
  if (mkcl_Null(output))	/* (OR form1) => form1 */
    mkcl_return_value(MKCL_CAR(whole));
  /* (OR form1 ... formn forml) => (COND (form1) ... (formn) (t forml)) */
  output = MKCL_CONS(env, mk_cl_list(env, 2, mk_cl_Ct, MKCL_CAR(whole)), output);
  mkcl_return_value(MKCL_CONS(env, @'cond', mk_cl_nreverse(env, output)));
}

static mkcl_object
expand_and(MKCL, mkcl_object whole)
{
  if (mkcl_Null(whole))
    return mk_cl_Ct;
  if (mkcl_Null(MKCL_CDR(whole)))
    return MKCL_CAR(whole);
  return mk_cl_list(env, 3, @'if', MKCL_CAR(whole), expand_and(env, MKCL_CDR(whole)));
}

static mkcl_object
and_macro(MKCL, mkcl_object whole, mkcl_object lex_env)
{
  mkcl_return_value(expand_and(env, MKCL_CDR(whole)));
}

static mkcl_object
when_macro(MKCL, mkcl_object whole, mkcl_object lex_env)
{
  mkcl_object args = MKCL_CDR(whole);
  if (mkcl_endp(env, args))
    mkcl_FEprogram_error(env, "Syntax error: ~S.", 1, whole);
  return mk_cl_list(env, 3, @'if', MKCL_CAR(args), MKCL_CONS(env, @'progn', MKCL_CDR(args)));
}

static mkcl_object
unless_macro(MKCL, mkcl_object whole, mkcl_object lex_env)
{
  mkcl_object args = MKCL_CDR(whole);
  if (mkcl_endp(env, args))
    mkcl_FEprogram_error(env, "Syntax error: ~S.", 1, whole);
  return mk_cl_list(env, 3, @'if', mk_cl_list(env, 2, @'not', MKCL_CAR(args)), MKCL_CONS(env, @'progn', MKCL_CDR(args)));
}

void
mkcl_init_macros(MKCL)
{
  MKCL_SET(@'*macroexpand-hook*', @'funcall');
  mkcl_def_c_macro(env, @'or', or_macro, 2);
  mkcl_def_c_macro(env, @'and', and_macro, 2);
  mkcl_def_c_macro(env, @'when', when_macro, 2);
  mkcl_def_c_macro(env, @'unless', unless_macro, 2);
}

