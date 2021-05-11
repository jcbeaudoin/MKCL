/* -*- mode: c -*- */
/*
    structure.c -- Structure interface.
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
#include <mkcl/mkcl-inl.h>
#include <string.h>


/******************************* ------- ******************************/

bool _mkcl_structure_subtypep(mkcl_object x, mkcl_object y)
{
  if (MKCL_CLASS_NAME(x) == y) {
    return TRUE;
  } else {
    mkcl_object superiors = MKCL_CLASS_SUPERIORS(x);
    mkcl_loop_for_on_unsafe(superiors) {
      if (_mkcl_structure_subtypep(MKCL_CONS_CAR(superiors), y))
	return TRUE;
    } mkcl_end_loop_for_on;
    return FALSE;
  }
}
#if 0 /* !CLOS */
bool _mkcl_structure_subtypep(mkcl_object x, mkcl_object y)
{
  do {
    if (!MKCL_SYMBOLP(x))
      return(FALSE);
    if (x == y)
      return(TRUE);
    x = mk_si_get_sysprop(x, MK_SI_structure_include);
  } while (x != mk_cl_Cnil);
  return(FALSE);
}
#endif /* !CLOS */

mkcl_object
mk_si_structure_subtype_p(MKCL, mkcl_object x, mkcl_object y)
{
  mkcl_call_stack_check(env);
  mkcl_return_value(((mkcl_type_of(x) == MKCL_T_STRUCTURE
	     && _mkcl_structure_subtypep(MKCL_STYPE(x), y)) ? mk_cl_Ct : mk_cl_Cnil));
}

mkcl_object mk_si_make_structure(MKCL, mkcl_narg narg, mkcl_object type, ...)
{
  mkcl_object x;
  int i;

  mkcl_call_stack_check(env);
  {
    mkcl_setup_for_rest(env, MK_SI_make_structure, 0, narg, type, args);

    x = mkcl_alloc_raw_structure(env, type, --narg);

    for (i = 0;  i < narg;  i++)
      MKCL_SLOT(x, i) = mkcl_va_arg(args);
    mkcl_va_end(args);
    mkcl_return_value(x);
  }
}

#define mkcl_copy_structure mk_si_copy_instance

#if 0 /* !CLOS */
mkcl_object
mkcl_copy_structure(MKCL, mkcl_object x)
{
  mkcl_index j, size;
  mkcl_object y;

  if (!mk_si_structurep(env, x))
    mkcl_FEwrong_type_argument(env, MK_CL_structure, x);
  y = mkcl_alloc_object(env, MKCL_T_STRUCTURE);
  MKCL_STYPE(y) = MKCL_STYPE(x);
  MKCL_SLENGTH(y) = j = MKCL_SLENGTH(x);
  size = sizeof(mkcl_object)*j;
  MKCL_SLOTS(y) = NULL;	/* for GC sake */
  MKCL_SLOTS(y) = (mkcl_object *)mkcl_alloc_align(env, size, sizeof(mkcl_object));
  memcpy(MKCL_SLOTS(y), MKCL_SLOTS(x), size);
  mkcl_return_value(y);
}
#endif /* !CLOS */

mkcl_object
mk_cl_copy_structure(MKCL, mkcl_object s)
{
  mkcl_call_stack_check(env);
  switch (mkcl_type_of(s)) {
  case mkcl_t_instance:
    if (_mkcl_structure_subtypep(MKCL_CLASS_OF(s), MK_CL_structure_object))
      s = mkcl_copy_structure(env, s);
    else
      mkcl_FEwrong_type_argument(env, MK_CL_structure, s);
    break;
  case mkcl_t_structure:
    s = mkcl_copy_structure(env, s);
    break;
  default:
    mkcl_FEwrong_type_argument(env, MK_CL_structure, s);
  }
  mkcl_return_value(s);
}


/* Kept only for compatibility. One should use class-of or type-of. */
mkcl_object
mk_si_structure_name(MKCL, mkcl_object s)
{
  mkcl_call_stack_check(env);
  if (!mk_si_structurep(env, s))
    mkcl_FEwrong_type_argument(env, MK_CL_structure, s);
  else
    { mkcl_return_value(MKCL_SNAME(s)); }
}

mkcl_object
mk_si_structure_length(MKCL, mkcl_object s)
{
  mkcl_call_stack_check(env);
  if (!mk_si_structurep(env, s))
    mkcl_FEwrong_type_argument(env, MK_CL_structure, s);
  else
    { mkcl_return_value(mkcl_make_unsigned_integer(env, MKCL_SLENGTH(s))); }
}

void
mkcl_FEtype_error_structure_index(MKCL, mkcl_object s, mkcl_object ndx)
{
  mk_cl_error(env, 5, MK_MKCL_invalid_slot, MK_KEY_name, ndx, MK_KEY_instance, s);
}


mkcl_object
mk_si_structure_ref(MKCL, mkcl_object x, mkcl_object name, mkcl_object index)
{
  mkcl_index i;
  
  mkcl_call_stack_check(env);
  if (mkcl_unlikely(!(mkcl_type_of(x) == MKCL_T_STRUCTURE
		      && (MKCL_SNAME(x) == name || _mkcl_structure_subtypep(MKCL_STYPE(x), name)))))
    mkcl_FEwrong_type_argument(env, name, x);
  else if (!MKCL_FIXNUMP(index) || (i = mkcl_fixnum_to_word(index)) >= MKCL_SLENGTH(x))
    mkcl_FEtype_error_structure_index(env, x, index);

  { mkcl_return_value(MKCL_SLOT(x, i)); }
}

mkcl_object
mk_si_structure_set(MKCL, mkcl_object x, mkcl_object name, mkcl_object index, mkcl_object val)
{
  mkcl_index i;
  
  mkcl_call_stack_check(env);
  if (mkcl_unlikely(!(mkcl_type_of(x) == MKCL_T_STRUCTURE
		      && (MKCL_SNAME(x) == name || _mkcl_structure_subtypep(MKCL_STYPE(x), name)))))
    mkcl_FEwrong_type_argument(env, name, x);
  else if (!MKCL_FIXNUMP(index) || (i = mkcl_fixnum_to_word(index)) >= MKCL_SLENGTH(x))
    mkcl_FEtype_error_structure_index(env, x, index);

  { MKCL_SLOT(x, i) = val; mkcl_return_value(val); }
}

mkcl_object
mk_si_structurep(MKCL, mkcl_object s)
{
  mkcl_call_stack_check(env);
  if (MKCL_INSTANCEP(s) && _mkcl_structure_subtypep(MKCL_CLASS_OF(s), MK_CL_structure_object))
    { mkcl_return_value(mk_cl_Ct); }
#if 0 /* !CLOS */
  if (mkcl_type_of(s) == mkcl_t_structure)
    { mkcl_return_value(mk_cl_Ct); }
#endif
  else
    { mkcl_return_value(mk_cl_Cnil); }
}
