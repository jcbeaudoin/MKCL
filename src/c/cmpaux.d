/* -*- mode: c -*- */
/*
    cmpaux.c -- Auxiliaries used in compiled Lisp code.
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

#include <mkcl/mkcl-cmp.h>
#include <mkcl/mkcl-inl.h>
#include <mkcl/internal.h>
#include <string.h>


mkcl_object
mk_si_specialp(MKCL, mkcl_object sym)
{
  @(return ((mkcl_symbol_type(env, sym) & mkcl_stp_special) ? mk_cl_Ct : mk_cl_Cnil));
}

mkcl_word
mkcl_ifloor(MKCL, mkcl_word x, mkcl_word y)
{
  if (y == 0)
    mkcl_FEerror(env, "Zero divizor", 0);
  else if (y > 0)
    if (x >= 0)
      return(x/y);
    else
      return(-((-x+y-1))/y);
  else
    if (x >= 0)
      return(-((x-y-1)/(-y)));
    else
      return((-x)/(-y));
}

mkcl_word
mkcl_imod(MKCL, mkcl_word x, mkcl_word y)
{
  return(x - mkcl_ifloor(env, x, y)*y);
}

/*
 * ----------------------------------------------------------------------
 *	Conversions to C
 * ----------------------------------------------------------------------
 */

char
mkcl_to_char(MKCL, mkcl_object x)
{
  switch (mkcl_type_of(x))
    {
    case mkcl_t_fixnum:
      return mkcl_fixnum_to_word(x);
    case mkcl_t_character:
      return MKCL_CHAR_CODE(x);
    default:
      mkcl_FEerror(env, "~S cannot be coerced to a C char.", 1, x);
    }
}

mkcl_word
mkcl_number_to_word(MKCL, mkcl_object x)
{
  switch (mkcl_type_of(x))
    {
    case mkcl_t_fixnum:
    case mkcl_t_bignum:
      return mkcl_integer_to_word(env, x);
    case mkcl_t_ratio:
      return (mkcl_word)mkcl_to_double(env, x);
    case mkcl_t_singlefloat:
      return (mkcl_word)mkcl_single_float(x);
    case mkcl_t_doublefloat:
      return (mkcl_word)mkcl_double_float(x);
#ifdef MKCL_LONG_FLOAT
    case mkcl_t_longfloat:
      return (mkcl_word)mkcl_long_float(x);
#endif
    default:
      mkcl_FEerror(env, "~S cannot be coerced to a C int.", 1, x);
    }
}

mkcl_index
mkcl_to_unsigned_integer(MKCL, mkcl_object x)
{
  switch (mkcl_type_of(x))
    {
    case mkcl_t_fixnum:
    case mkcl_t_bignum:
      return mkcl_integer_to_index(env, x);
    case mkcl_t_ratio:
      return (mkcl_index)mkcl_to_double(env, x);
    case mkcl_t_singlefloat:
      return (mkcl_index)mkcl_single_float(x);
    case mkcl_t_doublefloat:
      return (mkcl_index)mkcl_double_float(x);
#ifdef MKCL_LONG_FLOAT
    case mkcl_t_longfloat:
      return (mkcl_index)mkcl_long_float(x);
#endif
    default:
      mkcl_FEerror(env, "~S cannot be coerced to a C unsigned int.", 1, x);
    }
}


void
mkcl_throw(MKCL, mkcl_object tag)
{
  mkcl_call_stack_check(env);
  mkcl_frame_ptr fr = mkcl_frs_sch(env, tag);
  if (fr == NULL)
    mkcl_FEcontrol_error(env, "THROW: The catch ~S is undefined.", 1, tag);
  mkcl_unwind(env, fr);
}

void
mkcl_return_from(MKCL, mkcl_object block_id, mkcl_object block_name)
{
  mkcl_call_stack_check(env);
  mkcl_frame_ptr fr = mkcl_frs_sch(env, block_id);
  if (fr == NULL)
    mkcl_FEcontrol_error(env, "RETURN-FROM: The block ~S is missing.", 1, block_name);
  mkcl_unwind(env, fr);
}

void
mkcl_go(MKCL, mkcl_object tag_id, mkcl_index label_index)
{
  mkcl_call_stack_check(env);
  mkcl_frame_ptr fr = mkcl_frs_sch(env, tag_id);
  if (fr == NULL)
    mkcl_FEcontrol_error(env, "GO: The tagbody is missing for label ~S.", 1, MKCL_MAKE_FIXNUM(label_index));
  env->go_label_index = label_index;
  mkcl_unwind(env, fr);
}


mkcl_object
mkcl_grab_rest_args(MKCL, mkcl_va_list args, bool dynamic)
{
  mkcl_object rest = mk_cl_Cnil;
  mkcl_object *r = &rest;

  mkcl_object (*conser)(MKCL, mkcl_object, mkcl_object) = (dynamic ? mk_si_dyn_cons : mkcl_cons);

  mkcl_call_stack_check(env);
  while (args[0].narg) {
    *r = conser(env, mkcl_va_arg(args), mk_cl_Cnil);
    r = &MKCL_CONS_CDR(*r);
  }
  return rest;
}

void
mkcl_parse_key(MKCL,
		mkcl_va_list args,	/* actual args */
		int nkey,		/* number of keywords */
		mkcl_object *keys,	/* keywords for the function */
		mkcl_object *vars,	/* where to put values (vars[0..nkey-1])
					   and suppliedp (vars[nkey..2*nkey-1]) */
		mkcl_object *rest,	/* if rest != NULL, where to collect rest values */
		bool allow_other_keys,	/* whether other key are allowed */
		bool dynamic)
{
  int i;
  mkcl_object supplied_allow_other_keys = MKCL_OBJNULL;
  mkcl_object unknown_keyword = MKCL_OBJNULL;
  mkcl_object (*conser)(MKCL, mkcl_object, mkcl_object) = (dynamic ? mk_si_dyn_cons : mkcl_cons);

  if (rest != NULL) *rest = mk_cl_Cnil;

  for (i = 0; i < 2*nkey; i++)
    vars[i] = mk_cl_Cnil;             /* default values: NIL, supplied: NIL */
  if (args[0].narg <= 0) return;

  for (; args[0].narg > 1; ) {
    mkcl_object keyword = mkcl_va_arg(args);
    mkcl_object value = mkcl_va_arg(args);
    if (!MKCL_SYMBOLP(keyword))
      mkcl_FEprogram_error(env, "LAMBDA: Keyword expected, got ~S.", 1, keyword);
    if (rest != NULL) {
      rest = &MKCL_CONS_CDR(*rest = conser(env, keyword, mk_cl_Cnil));
      rest = &MKCL_CONS_CDR(*rest = conser(env, value, mk_cl_Cnil));
    }
    for (i = 0; i < nkey; i++) {
      if (keys[i] == keyword) {
	if (vars[nkey+i] == mk_cl_Cnil) {
	  vars[i] = value;
	  vars[nkey+i] = mk_cl_Ct;
	}
	goto goon;
      }
    }
    /* the key is a new one */
    if (keyword == @':allow-other-keys') {
      if (supplied_allow_other_keys == MKCL_OBJNULL)
	supplied_allow_other_keys = value;
    } else if (unknown_keyword == MKCL_OBJNULL)
      unknown_keyword = keyword;
  goon:;
  }
  if (args[0].narg != 0)
    mkcl_FEprogram_error(env, "Odd number of keys", 0);
  if (unknown_keyword != MKCL_OBJNULL && !allow_other_keys &&
      (supplied_allow_other_keys == mk_cl_Cnil ||
       supplied_allow_other_keys == MKCL_OBJNULL))
    mkcl_FEprogram_error(env, "Unknown keyword ~S", 1, unknown_keyword);
}

static mkcl_object convert_cmp_lexical_info(MKCL, mkcl_object cmp_env)
{
  mkcl_object lex_env = mk_cl_Cnil;
  unsigned long i;

  if (cmp_env != mk_cl_Cnil)
    {
      unsigned long nb_locations = cmp_env->cmp_dbg_lex.nb_locations;
      const struct mkcl_lex_var_info * var_desc = cmp_env->cmp_dbg_lex.var_descriptors;
      void * const * var_locs = cmp_env->cmp_dbg_lex.var_locations;

      lex_env = convert_cmp_lexical_info(env, cmp_env->cmp_dbg_lex.parent);
      
      for (i = 0; i < nb_locations; i++)
	{
#if 0
	  mkcl_object var_name = mkcl_make_simple_base_string(env, (char *) var_desc[i].name);
#else
	  size_t var_name_len = strlen(var_desc[i].name);
	  mkcl_UTF_8_object_sized(var_name_obj, (char *) var_desc[i].name, var_name_len);
	  mkcl_object var_name = mkcl_utf_8_to_string(env, (mkcl_object) &var_name_obj);
#endif
	  mkcl_object var_value;
	  
	  switch (var_desc[i].type)
	    {
	    case _mkcl_object_loc:
	      var_value = *((mkcl_object *) var_locs[i]);
	      break;
	    case _mkcl_word_loc:
	      var_value = MKCL_MAKE_FIXNUM(*((mkcl_word *) var_locs[i]));
	      break;
	    case _mkcl_base_char_loc:
	      var_value = MKCL_CODE_CHAR(*((mkcl_base_char *) var_locs[i]));
	      break;
	    case _mkcl_uni_char_loc:
	      var_value = MKCL_CODE_CHAR(*((mkcl_character *) var_locs[i]));
	      break;
	    case _mkcl_float_loc:
	      var_value = mkcl_make_singlefloat(env, *((float *) var_locs[i]));
	      break;
	    case _mkcl_double_loc:
	      var_value = mkcl_make_doublefloat(env, *((double *) var_locs[i]));
	      break;
	    case _mkcl_long_double_loc:
	      var_value = mkcl_make_longfloat(env, *((long double *) var_locs[i]));
	      break;
	    case _mkcl_closure_var_loc:
	      var_value = *((mkcl_object *) var_locs[i]);
	      break;
	    case _mkcl_float128_loc: /* not implemented yet. */
	    default:
	      mkcl_lose(env, "Invalid locative type");
	      break;
	    }
	  lex_env = MKCL_CONS(env, MKCL_CONS(env, var_name, var_value), lex_env);
	}
    }
  return lex_env;
}

mkcl_object mk_si_convert_cmp_lexical_info(MKCL, mkcl_object cmp_env)
{
  mkcl_call_stack_check(env);
  if (mkcl_Null(cmp_env))
    { @(return mk_cl_Cnil); }
  else if ( mkcl_type_of(cmp_env) != mkcl_t_cmp_dbg_lex_level )
    { mkcl_FEwrong_type_argument(env, @'si::compiled-debug-information', cmp_env); }
  else
    {
      mkcl_object lex_env = convert_cmp_lexical_info(env, cmp_env);
      @(return lex_env);
    }
}
