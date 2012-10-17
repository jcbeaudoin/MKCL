/* -*- mode: c -*- */
/*
    reference.c -- Reference in Constants and Variables.
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
#include <mkcl/mkcl-inl.h>

/*
	Symbol-function returns
                function-closure		for function
		(macro . function-closure)	for macros
		special				for special forms.
*/
mkcl_object
mk_cl_symbol_function(MKCL, mkcl_object sym)
{
  mkcl_call_stack_check(env);
  if (mkcl_Null(sym))
    mkcl_FEundefined_function(env, mk_cl_Cnil_symbol);
  else
    {
      int sym_type = mkcl_symbol_type(env, sym);
      mkcl_object output = MKCL_SYM_FUN(sym);

      if (sym_type & mkcl_stp_special_form) {
	output = @'special';
      } else if (mkcl_Null(output)) {
	mkcl_FEundefined_function(env, sym);
      } else if (sym_type & mkcl_stp_macro) {
	output = MKCL_CONS(env, @'si::macro', output);
      }
      @(return output);
    }
}

mkcl_object
mk_cl_fdefinition(MKCL, mkcl_object fname)
{
  mkcl_call_stack_check(env);
  @(return ((MKCL_SYMBOLP(fname)) ? mk_cl_symbol_function(env, fname) : mkcl_fdefinition(env, fname)));
}

mkcl_object
mk_cl_fboundp(MKCL, mkcl_object fname)
{
  mkcl_call_stack_check(env);
  if (mkcl_Null(fname))
    fname = mk_cl_Cnil_symbol;

  if (MKCL_SYMBOLP(fname)) {
    @(return (((fname->symbol.stype & mkcl_stp_special_form)
	       || MKCL_SYM_FUN(fname) != mk_cl_Cnil) ? mk_cl_Ct : mk_cl_Cnil));
  } else if (MKCL_LISTP(fname)) {
    if (MKCL_CAR(fname) == @'setf') {
      mkcl_object sym = MKCL_CDR(fname);
      if (MKCL_CONSP(sym) && MKCL_CDR(sym) == mk_cl_Cnil) {
	sym = MKCL_CAR(sym);
	if (MKCL_SYMBOLP(sym))
	  @(return mk_si_get_sysprop(env, sym, @'si::setf-symbol'));
      }
    }
  }
  mkcl_FEinvalid_function_name(env, fname);
}

mkcl_object
mkcl_fdefinition(MKCL, mkcl_object fun)
{
  mkcl_type t;
  mkcl_object output;

  if (mkcl_Null(fun))
    fun = mk_cl_Cnil_symbol;

  t = mkcl_type_of(fun);
  if (t == mkcl_t_symbol) {
    output = MKCL_SYM_FUN(fun);
    if (output == mk_cl_Cnil)
      mkcl_FEundefined_function(env, fun);
    if (fun->symbol.stype & (mkcl_stp_macro | mkcl_stp_special_form))
      mkcl_FEundefined_function(env, fun);
  } else if (t == mkcl_t_cons) {
    mkcl_object sym = MKCL_CDR(fun);
    if (!MKCL_CONSP(sym))
      mkcl_FEinvalid_function_name(env, fun);
    if (MKCL_CAR(fun) == @'setf') {
      if (MKCL_CDR(sym) != mk_cl_Cnil)
	mkcl_FEinvalid_function_name(env, fun);
      sym = MKCL_CAR(sym);
      if (mkcl_type_of(sym) != mkcl_t_symbol)
	mkcl_FEinvalid_function_name(env, fun);
      output = mk_si_get_sysprop(env, sym, @'si::setf-symbol');
      if (mkcl_Null(output))
	mkcl_FEundefined_function(env, fun);
    } else if (MKCL_CAR(fun) == @'lambda') {
      return mk_si_make_lambda(env, mk_cl_Cnil, sym);
    } else if (MKCL_CAR(fun) == @'si::lambda-block') {
      return mk_si_make_lambda(env, MKCL_CAR(sym), MKCL_CDR(sym));
    } else {
      mkcl_FEinvalid_function_name(env, fun);
    }
  } else {
    mkcl_FEinvalid_function_name(env, fun);
  }
  return output;
}

mkcl_object
mk_si_coerce_to_function(MKCL, mkcl_object fun)
{
  mkcl_type t = mkcl_type_of(fun);

  mkcl_call_stack_check(env);
  if (!(t == mkcl_t_cfun || t == mkcl_t_cclosure
	|| t == mkcl_t_bytecode || t == mkcl_t_bclosure
	|| (t == mkcl_t_instance && fun->instance.isgf)
	)) {
    fun = mkcl_fdefinition(env, fun);
  }
  @(return fun);
}

mkcl_object
mk_cl_symbol_value(MKCL, mkcl_object sym)
{
  mkcl_object value;

  mkcl_call_stack_check(env);
  if (mkcl_Null(sym)) {
    value = sym;
  } else {
    if (!MKCL_SYMBOLP(sym)) {
      mkcl_FEtype_error_symbol(env, sym);
    }
    value = MKCL_SYM_VAL(env, sym);
    if (value == MKCL_OBJNULL)
      mkcl_FEunbound_variable(env, sym);
  }
  @(return value);
}

mkcl_object
mk_cl_boundp(MKCL, mkcl_object sym)
{
  mkcl_object output;

  mkcl_call_stack_check(env);
  if (mkcl_Null(sym)) {
    output = mk_cl_Ct;
  } else {
    if (!MKCL_SYMBOLP(sym))
      mkcl_FEtype_error_symbol(env, sym);
    if (MKCL_SYM_VAL(env, sym) == MKCL_OBJNULL)
      output = mk_cl_Cnil;
    else
      output = mk_cl_Ct;
  }
  @(return output);
}

mkcl_object
mk_cl_special_operator_p(MKCL, mkcl_object form)
{
  int special;

  mkcl_call_stack_check(env);
  special = mkcl_symbol_type(env, form) & mkcl_stp_special_form;
  @(return (special ? mk_cl_Ct : mk_cl_Cnil));
}
