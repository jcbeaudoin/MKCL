/* -*- mode: c -*- */
/*
    typespec.c -- Type specifier routines.
*/
/*
    Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
    Copyright (c) 1990, Giuseppe Attardi.
    Copyright (c) 2001, Juan Jose Garcia Ripoll.
    Copyright (c) 2011-2017,2022, Jean-Claude Beaudoin.

    MKCL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 3 of the License, or (at your option) any later version.

    See file '../../Copyright' for full details.
*/

#include <mkcl/mkcl.h>
#include <mkcl/internal.h>

void
mkcl_FEtype_error_character(MKCL, mkcl_object x) {
  mkcl_FEwrong_type_argument(env, (mkcl_object) &MK_CL_character, x);
}

void
mkcl_FEtype_error_base_char(MKCL, mkcl_object x) {
  mkcl_FEwrong_type_argument(env, (mkcl_object) &MK_CL_base_char, x);
}

void
mkcl_FEtype_error_cons(MKCL, mkcl_object x) {
  mkcl_FEwrong_type_argument(env, (mkcl_object) &MK_CL_cons, x);
}

void
mkcl_FEtype_error_number(MKCL, mkcl_object x) {
  mkcl_FEwrong_type_argument(env, (mkcl_object) &MK_CL_number, x);
}

void
mkcl_FEtype_error_real(MKCL, mkcl_object x) {
  mkcl_FEwrong_type_argument(env, (mkcl_object) &MK_CL_real, x);
}

void
mkcl_FEtype_error_float(MKCL, mkcl_object x) {
  mkcl_FEwrong_type_argument(env, (mkcl_object) &MK_CL_float, x);
}

void
mkcl_FEtype_error_integer(MKCL, mkcl_object x) {
  mkcl_FEwrong_type_argument(env, (mkcl_object) &MK_CL_integer, x);
}

void
mkcl_FEtype_error_list(MKCL, mkcl_object x) {
  mkcl_FEwrong_type_argument(env, (mkcl_object) &MK_CL_list, x);
}

void
mkcl_FEtype_error_proper_list(MKCL, mkcl_object x) {
  mk_cl_error(env, 9, (mkcl_object) &MK_CL_simple_type_error,
	      (mkcl_object) &MK_KEY_format_control, mkcl_make_simple_base_string(env, "Not a proper list: ~S"),
	      (mkcl_object) &MK_KEY_format_arguments, mk_cl_list(env, 1, x),
	      (mkcl_object) &MK_KEY_expected_type, mkcl_fast_read_from_cstring(env, "si::proper-list"),
	      (mkcl_object) &MK_KEY_datum, x);
}

void
mkcl_FEtype_error_alist(MKCL, mkcl_object x)
{
  mk_cl_error(env, 9, (mkcl_object) &MK_CL_simple_type_error,
	      (mkcl_object) &MK_KEY_format_control, mkcl_make_simple_base_string(env, "Not a valid association list: ~S"),
	      (mkcl_object) &MK_KEY_format_arguments, mk_cl_list(env, 1, x),
	      (mkcl_object) &MK_KEY_expected_type, (mkcl_object) &MK_CL_list,
	      (mkcl_object) &MK_KEY_datum, x);
}

void
mkcl_FEcircular_list(MKCL, mkcl_object x)
{
  mkcl_bds_bind(env, (mkcl_object) &MK_CL_DYNVAR_print_circle, mk_cl_Ct);
  mk_cl_error(env, 9, (mkcl_object) &MK_CL_simple_type_error,
	      (mkcl_object) &MK_KEY_format_control, mkcl_make_simple_base_string(env, "Circular list: ~S"),
	      (mkcl_object) &MK_KEY_format_arguments, mk_cl_list(env, 1, x),
	      (mkcl_object) &MK_KEY_expected_type, (mkcl_object) &MK_CL_list,
	      (mkcl_object) &MK_KEY_datum, x);
  mkcl_bds_unwind1(env);
  mkcl_lose(env, "Should not have returned from universal-error-handler");
}

void
mkcl_FEtype_error_seq_index(MKCL, mkcl_object seq, mkcl_object ndx)
{
  mk_cl_error(env, 9, (mkcl_object) &MK_CL_simple_type_error,
	      (mkcl_object) &MK_KEY_format_control, mkcl_make_simple_base_string(env, "~S is not a valid index into the object ~S"),
	      (mkcl_object) &MK_KEY_format_arguments, mk_cl_list(env, 2, ndx, seq),
	      (mkcl_object) &MK_KEY_expected_type, mk_cl_list(env, 3,
					    (mkcl_object) &MK_CL_integer,
					    MKCL_MAKE_FIXNUM(0),
					    MKCL_MAKE_FIXNUM(mkcl_length(env, seq)-1)),
	      (mkcl_object) &MK_KEY_datum, ndx);
}

void
mkcl_FEtype_error_string(MKCL, mkcl_object s)
{
  mkcl_FEwrong_type_argument(env, (mkcl_object) &MK_CL_string, s);
}

void
mkcl_FEtype_error_string_with_fill_pointer(MKCL, mkcl_object x) {
  mkcl_object type = mkcl_fast_read_from_cstring(env, "(AND STRING (SATISFIES ARRAY-HAS-FILL-POINTER-P))");
  mkcl_FEwrong_type_argument(env, type, x);
}

void
mkcl_FEtype_error_base_string(MKCL, mkcl_object s)
{
  mkcl_FEwrong_type_argument(env, (mkcl_object) &MK_CL_base_string, s);
}

void
mkcl_FEtype_error_base_string_with_fill_pointer(MKCL, mkcl_object x) {
  mkcl_object type = mkcl_fast_read_from_cstring(env, "(AND BASE-STRING (SATISFIES ARRAY-HAS-FILL-POINTER-P))");
  mkcl_FEwrong_type_argument(env, type, x);
}

void
mkcl_FEtype_error_vector(MKCL, mkcl_object x) {
  mkcl_FEwrong_type_argument(env, (mkcl_object) &MK_CL_vector, x);
}

void
mkcl_FEtype_error_vector_with_fill_pointer(MKCL, mkcl_object x) {
  mkcl_object type = mkcl_fast_read_from_cstring(env, "(AND VECTOR (SATISFIES ARRAY-HAS-FILL-POINTER-P))");
  mkcl_FEwrong_type_argument(env, type, x);
}

void
mkcl_FEtype_error_sequence(MKCL, mkcl_object x) {
  mkcl_FEwrong_type_argument(env, (mkcl_object) &MK_CL_sequence, x);
}

void
mkcl_FEtype_error_proper_sequence(MKCL, mkcl_object x) {
  mk_cl_error(env, 9, (mkcl_object) &MK_CL_simple_type_error,
	      (mkcl_object) &MK_KEY_format_control, mkcl_make_simple_base_string(env, "Not a proper sequence: ~S"),
	      (mkcl_object) &MK_KEY_format_arguments, mk_cl_list(env, 1, x),
	      (mkcl_object) &MK_KEY_expected_type, mkcl_fast_read_from_cstring(env, "si::proper-sequence"),
	      (mkcl_object) &MK_KEY_datum, x);
}

void
mkcl_FEtype_error_array(MKCL, mkcl_object x) {
  mkcl_FEwrong_type_argument(env, (mkcl_object) &MK_CL_array, x);
}

void
mkcl_FEtype_error_stream(MKCL, mkcl_object strm)
{
  mkcl_FEwrong_type_argument(env, (mkcl_object) &MK_CL_stream, strm);
}

void
mkcl_FEtype_error_instance(MKCL, mkcl_object x) {
  mkcl_FEwrong_type_argument(env, (mkcl_object) &MK_SI_instance, x);
}

mkcl_object
mkcl_type_error(MKCL, mkcl_object function, const char *place, mkcl_object o, mkcl_object type)
{
  if (mkcl_Null(mk_cl_fboundp(env, (mkcl_object) &MK_SI_wrong_type_argument)))
    mkcl_FEwrong_type_argument(env, type, o);
  return mkcl_funcall4(env,
		       MK_SI_wrong_type_argument.gfdef,
		       o,
		       type,
		       (*place ? mkcl_make_simple_base_string(env, (char *) place) : mk_cl_Cnil),
		       function);
}

/**********************************************************************/

static mkcl_object
mkcl_type_to_symbol(MKCL, mkcl_type t)
{
  switch(t) {
  case mkcl_t_character:
    return (mkcl_object) &MK_CL_character;
  case mkcl_t_fixnum:
    return (mkcl_object) &MK_CL_fixnum;
  case mkcl_t_bignum:
    return (mkcl_object) &MK_CL_bignum;
  case mkcl_t_ratio:
    return (mkcl_object) &MK_CL_ratio;
  case mkcl_t_singlefloat:
    return (mkcl_object) &MK_CL_single_float;
  case mkcl_t_doublefloat:
    return (mkcl_object) &MK_CL_double_float;
#ifdef MKCL_LONG_FLOAT
  case mkcl_t_longfloat:
    return (mkcl_object) &MK_CL_long_float;
#endif
  case mkcl_t_complex:
    return (mkcl_object) &MK_CL_complex;
  case mkcl_t_symbol:
    return (mkcl_object) &MK_CL_symbol;
  case mkcl_t_package:
    return (mkcl_object) &MK_CL_package;
  case mkcl_t_null:
    return (mkcl_object) &MK_CL_null;
  case mkcl_t_cons:
    return (mkcl_object) &MK_CL_cons;
  case mkcl_t_hashtable:
    return (mkcl_object) &MK_CL_hash_table;
  case mkcl_t_array:
    return (mkcl_object) &MK_CL_array;
  case mkcl_t_vector:
    return (mkcl_object) &MK_CL_vector;
  case mkcl_t_bitvector:
    return (mkcl_object) &MK_CL_bit_vector;
  case mkcl_t_string:
    return (mkcl_object) &MK_CL_string;
  case mkcl_t_base_string:
    return (mkcl_object) &MK_CL_base_string;
  case mkcl_t_stream:
    return (mkcl_object) &MK_CL_stream;
  case mkcl_t_readtable:
    return (mkcl_object) &MK_CL_readtable;
  case mkcl_t_pathname:
    return (mkcl_object) &MK_CL_pathname;
  case mkcl_t_random:
    return (mkcl_object) &MK_CL_random_state;
  case mkcl_t_bytecode:
  case mkcl_t_bclosure:
  case mkcl_t_cfun:
  case mkcl_t_cclosure:
    return (mkcl_object) &MK_CL_compiled_function;
  case mkcl_t_thread:
    return (mkcl_object) &MK_MT_thread;
  case mkcl_t_lock:
    return (mkcl_object) &MK_MT_lock;
  case mkcl_t_rwlock:
    return (mkcl_object) &MK_MT_rwlock;
  case mkcl_t_semaphore:
    return (mkcl_object) &MK_MT_semaphore;
  case mkcl_t_condition_variable:
    return (mkcl_object) &MK_MT_condition_variable;
  case mkcl_t_codeblock:
    return (mkcl_object) &MK_SI_code_block;
  case mkcl_t_foreign:
    return (mkcl_object) &MK_SI_foreign;
  case mkcl_t_temp_stack_frame:
    return (mkcl_object) &MK_SI_temp_stack_frame;
  case mkcl_t_instance:
    return (mkcl_object) &MK_CL_standard_object;
  case mkcl_t_structure:
    return (mkcl_object) &MK_CL_structure_object;
  case mkcl_t_cdisplay:
    return (mkcl_object) &MK_SI_compiled_closure_display;
  case mkcl_t_clevel_block:
    return (mkcl_object) &MK_SI_compiled_closure_level;
  case mkcl_t_cmp_dbg_lex_level:
    return (mkcl_object) &MK_SI_compiled_debug_information;
  case mkcl_t_process:
    return (mkcl_object) &MK_MKCL_process;
  case mkcl_t_UTF_8:
    return (mkcl_object) &MK_SI_utf_8;
  case mkcl_t_UTF_16:
    return (mkcl_object) &MK_SI_utf_16;
  default:
    mkcl_lose(env, "not a lisp data object");
  }
}

mkcl_object
mkcl_check_cl_type(MKCL, mkcl_object fun, mkcl_object p, mkcl_type t)
{
  while (mkcl_type_of(p) != t) {
    p = mkcl_type_error(env, fun, "argument", p, mkcl_type_to_symbol(env, t));
  }
  return p;
}

mkcl_object
mkcl_check_type_string(MKCL, mkcl_object fun, mkcl_object p)
{
  mkcl_type t;
 AGAIN:
  t = mkcl_type_of(p);
  if (t != mkcl_t_base_string) {
    if (t != mkcl_t_string) 
      {
	p = mkcl_type_error(env, fun, "", p, (mkcl_object) &MK_CL_string);
	goto AGAIN;
      }
  }
  return p;
}


void
mkcl_assert_type_integer(MKCL, mkcl_object p)
{
  mkcl_type t = mkcl_type_of(p);
  if (t != mkcl_t_fixnum && t != mkcl_t_bignum)
    mkcl_FEtype_error_integer(env, p);
}

void
mkcl_assert_type_non_negative_integer(MKCL, mkcl_object p)
{
  mkcl_type t = mkcl_type_of(p);

  if (t == mkcl_t_fixnum) {
    if (MKCL_FIXNUM_PLUSP(p))
      return;
  } else if (t == mkcl_t_bignum) {
    if (_mkcl_big_sign(p) >= 0)
      return;
  }
  mkcl_FEwrong_type_argument(env, mk_cl_list(env, 3, (mkcl_object) &MK_CL_integer, MKCL_MAKE_FIXNUM(0), (mkcl_object) &MK_CL_X), p);
}

void
mkcl_assert_type_package(MKCL, mkcl_object p)
{
  if (mkcl_type_of(p) != mkcl_t_package)
    mkcl_FEwrong_type_argument(env, (mkcl_object) &MK_CL_package, p);
}

void
mkcl_assert_type_cons(MKCL, mkcl_object p)
{
  if (MKCL_ATOM(p))
    mkcl_FEtype_error_cons(env, p);
}

void
mkcl_assert_type_list(MKCL, mkcl_object p)
{
  if (MKCL_ATOM(p) && p != mk_cl_Cnil)
    mkcl_FEtype_error_list(env, p);
}

void
mkcl_assert_type_proper_list(MKCL, mkcl_object p)
{
  if (MKCL_ATOM(p) && p != mk_cl_Cnil)
    mkcl_FEtype_error_list(env, p);
  if (mk_cl_list_length(env, p) == mk_cl_Cnil)
    mkcl_FEcircular_list(env, p);
}

void
mkcl_assert_type_readtable(MKCL, mkcl_object p)
{
  if (mkcl_type_of(p) != mkcl_t_readtable)
    mkcl_FEwrong_type_argument(env, (mkcl_object) &MK_CL_readtable, p);
}

void
mkcl_assert_type_hash_table(MKCL, mkcl_object p)
{
  if (mkcl_type_of(p) != mkcl_t_hashtable)
    mkcl_FEwrong_type_argument(env, (mkcl_object) &MK_CL_hash_table, p);
}

void
mkcl_assert_type_array(MKCL, mkcl_object p)
{
  if (!MKCL_ARRAYP(p))
    mkcl_FEwrong_type_argument(env, (mkcl_object) &MK_CL_array, p);
}

void
mkcl_assert_type_vector(MKCL, mkcl_object p)
{
  if (!MKCL_VECTORP(p))
    mkcl_FEwrong_type_argument(env, (mkcl_object) &MK_CL_vector, p);
}

struct mkcl_cfun mk_cl_type_of_cfunobj = MKCL_CFUN1(mk_cl_type_of, (mkcl_object) &MK_CL_type_of);

mkcl_object
mk_cl_type_of(MKCL, mkcl_object x)
{
  mkcl_object t;
  mkcl_type tx = mkcl_type_of(x);

  mkcl_call_stack_check(env);
  switch (tx) {
  case mkcl_t_instance:
    {
      mkcl_object cl = MKCL_CLASS_OF(x);
      t = MKCL_CLASS_NAME(cl);
      if (mkcl_Null(t) || (MKCL_SYMBOLP(t) && cl != t->symbol.properly_named_class))
	t = cl;
    }
    break;
  case mkcl_t_fixnum:
  case mkcl_t_bignum:
    /* This is a pretty lame reading of the spec!
       it results in a nearly useless typespec. JCB
       Other CL (SBCL, clisp, ...) do better!
    */
    t = mk_cl_list(env, 3, (mkcl_object) &MK_CL_integer, x, x);
    break;
  case mkcl_t_character:
    {
      int i = MKCL_CHAR_CODE(x);
      if (mkcl_standard_char_p(i)) {
	t = (mkcl_object) &MK_CL_standard_char;
      } else if (mkcl_base_char_p(i)) {
	t = (mkcl_object) &MK_CL_base_char;
      } else {
	t = (mkcl_object) &MK_CL_character;
      }
    }
    break;
  case mkcl_t_null:
    t = (mkcl_object) &MK_CL_null;
    break;
  case mkcl_t_cons:
    t = (mkcl_object) &MK_CL_cons;
    break;
  case mkcl_t_symbol:
    if ( x == ((mkcl_object) &mk_cl_Cnil_symbol) )
      t = (mkcl_object) &MK_CL_null;
    else if (x == mk_cl_Ct)
      t = (mkcl_object) &MK_CL_boolean;
    else if (x->symbol.hpack == mkcl_core.keyword_package)
      t = (mkcl_object) &MK_CL_keyword;
    else
      t = (mkcl_object) &MK_CL_symbol;
    break;
  case mkcl_t_array:
    if (!(x->array.adjustable || !mkcl_Null(MKCL_CAR(x->array.displaced))))
      t = (mkcl_object) &MK_CL_simple_array;
    else
      t = (mkcl_object) &MK_CL_array;
    t = mk_cl_list(env, 3,
		   t,
		   mkcl_elttype_to_symbol(env, mkcl_array_elttype(env, x)),
#if 0
		   mk_cl_array_dimensions(env, 1, x)
#else
		   mk_cl_array_dimensions(env, x)
#endif
		   );
    break;
  case mkcl_t_vector:
    if (!(x->vector.hasfillp || x->vector.adjustable || !mkcl_Null(MKCL_CAR(x->vector.displaced)))
	&& x->vector.elttype == mkcl_aet_object)
      t = mk_cl_list(env, 2, (mkcl_object) &MK_CL_simple_vector, MKCL_MAKE_FIXNUM(x->vector.dim));
    else
      {
	t = mk_cl_list(env, 3,
		       (mkcl_object) &MK_CL_vector,
		       mkcl_elttype_to_symbol(env, mkcl_array_elttype(env, x)),
		       MKCL_MAKE_FIXNUM(x->vector.dim));
      }
    break;
  case mkcl_t_string:
    if (x->string.adjustable || x->string.hasfillp || !mkcl_Null(MKCL_CAR(x->string.displaced)))
      t = (mkcl_object) &MK_CL_string;
    else
      t = (mkcl_object) &MK_CL_simple_string;
    t = mk_cl_list(env, 2, t, MKCL_MAKE_FIXNUM(x->string.dim));
    break;
  case mkcl_t_base_string:
    if (x->base_string.adjustable || x->base_string.hasfillp || !mkcl_Null(MKCL_CAR(x->base_string.displaced)))
      t = (mkcl_object) &MK_CL_base_string;
    else
      t = (mkcl_object) &MK_CL_simple_base_string;
    t = mk_cl_list(env, 2, t, MKCL_MAKE_FIXNUM(x->base_string.dim));
    break;
  case mkcl_t_bitvector:
    if (x->vector.adjustable || x->vector.hasfillp || !mkcl_Null(MKCL_CAR(x->vector.displaced)))
      t = (mkcl_object) &MK_CL_bit_vector;
    else
      t = (mkcl_object) &MK_CL_simple_bit_vector;
    t = mk_cl_list(env, 2, t, MKCL_MAKE_FIXNUM(x->vector.dim));
    break;
  case mkcl_t_structure:
    t = x->str.name;
    break;
  case mkcl_t_stream:
    switch (x->stream.mode)
      {
      case mkcl_smm_synonym:	   t = (mkcl_object) &MK_CL_synonym_stream; break;
      case mkcl_smm_broadcast:	   t = (mkcl_object) &MK_CL_broadcast_stream; break;
      case mkcl_smm_concatenated:  t = (mkcl_object) &MK_CL_concatenated_stream; break;
      case mkcl_smm_two_way:	   t = (mkcl_object) &MK_CL_two_way_stream; break;
      case mkcl_smm_string_input:
      case mkcl_smm_string_output: t = (mkcl_object) &MK_CL_string_stream; break;
      case mkcl_smm_echo:	   t = (mkcl_object) &MK_CL_echo_stream; break;
      default:		           t = (mkcl_object) &MK_CL_file_stream; break;
      }
    break;
  case mkcl_t_pathname:
    t = x->pathname.logical ? ((mkcl_object) &MK_CL_logical_pathname) : ((mkcl_object) &MK_CL_pathname);
    break;
  default:
    t = mkcl_type_to_symbol(env, tx);
  }
  mkcl_return_value(t);
}
