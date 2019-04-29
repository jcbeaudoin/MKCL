/* -*- mode: c -*- */
/*
    typespec.c -- Type specifier routines.
*/
/*
    Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
    Copyright (c) 1990, Giuseppe Attardi.
    Copyright (c) 2001, Juan Jose Garcia Ripoll.
    Copyright (c) 2011-2017, Jean-Claude Beaudoin.

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
  mkcl_FEwrong_type_argument(env, @'character', x);
}

void
mkcl_FEtype_error_base_char(MKCL, mkcl_object x) {
  mkcl_FEwrong_type_argument(env, @'base-char', x);
}

void
mkcl_FEtype_error_cons(MKCL, mkcl_object x) {
  mkcl_FEwrong_type_argument(env, @'cons', x);
}

void
mkcl_FEtype_error_number(MKCL, mkcl_object x) {
  mkcl_FEwrong_type_argument(env, @'number', x);
}

void
mkcl_FEtype_error_real(MKCL, mkcl_object x) {
  mkcl_FEwrong_type_argument(env, @'real', x);
}

void
mkcl_FEtype_error_float(MKCL, mkcl_object x) {
  mkcl_FEwrong_type_argument(env, @'float', x);
}

void
mkcl_FEtype_error_integer(MKCL, mkcl_object x) {
  mkcl_FEwrong_type_argument(env, @'integer', x);
}

void
mkcl_FEtype_error_list(MKCL, mkcl_object x) {
  mkcl_FEwrong_type_argument(env, @'list', x);
}

void
mkcl_FEtype_error_proper_list(MKCL, mkcl_object x) {
  mk_cl_error(env, 9, @'simple-type-error',
	      @':format-control', mkcl_make_simple_base_string(env, "Not a proper list: ~S"),
	      @':format-arguments', mk_cl_list(env, 1, x),
	      @':expected-type', mkcl_fast_read_from_cstring(env, "si::proper-list"),
	      @':datum', x);
}

void
mkcl_FEtype_error_alist(MKCL, mkcl_object x)
{
  mk_cl_error(env, 9, @'simple-type-error',
	      @':format-control', mkcl_make_simple_base_string(env, "Not a valid association list: ~S"),
	      @':format-arguments', mk_cl_list(env, 1, x),
	      @':expected-type', @'list',
	      @':datum', x);
}

void
mkcl_FEcircular_list(MKCL, mkcl_object x)
{
  mkcl_bds_bind(env, @'*print-circle*', mk_cl_Ct);
  mk_cl_error(env, 9, @'simple-type-error',
	      @':format-control', mkcl_make_simple_base_string(env, "Circular list: ~S"),
	      @':format-arguments', mk_cl_list(env, 1, x),
	      @':expected-type', @'list',
	      @':datum', x);
  mkcl_bds_unwind1(env);
  mkcl_lose(env, "Should not have returned from universal-error-handler");
}

void
mkcl_FEtype_error_seq_index(MKCL, mkcl_object seq, mkcl_object ndx)
{
  mk_cl_error(env, 9, @'simple-type-error',
	      @':format-control', mkcl_make_simple_base_string(env, "~S is not a valid index into the object ~S"),
	      @':format-arguments', mk_cl_list(env, 2, ndx, seq),
	      @':expected-type', mk_cl_list(env, 3,
					    @'integer',
					    MKCL_MAKE_FIXNUM(0),
					    MKCL_MAKE_FIXNUM(mkcl_length(env, seq)-1)),
	      @':datum', ndx);
}

void
mkcl_FEtype_error_string(MKCL, mkcl_object s)
{
  mkcl_FEwrong_type_argument(env, @'string', s);
}

void
mkcl_FEtype_error_string_with_fill_pointer(MKCL, mkcl_object x) {
  mkcl_object type = mkcl_fast_read_from_cstring(env, "(AND STRING (SATISFIES ARRAY-HAS-FILL-POINTER-P))");
  mkcl_FEwrong_type_argument(env, type, x);
}

void
mkcl_FEtype_error_base_string(MKCL, mkcl_object s)
{
  mkcl_FEwrong_type_argument(env, @'base-string', s);
}

void
mkcl_FEtype_error_base_string_with_fill_pointer(MKCL, mkcl_object x) {
  mkcl_object type = mkcl_fast_read_from_cstring(env, "(AND BASE-STRING (SATISFIES ARRAY-HAS-FILL-POINTER-P))");
  mkcl_FEwrong_type_argument(env, type, x);
}

void
mkcl_FEtype_error_vector(MKCL, mkcl_object x) {
  mkcl_FEwrong_type_argument(env, @'vector', x);
}

void
mkcl_FEtype_error_vector_with_fill_pointer(MKCL, mkcl_object x) {
  mkcl_object type = mkcl_fast_read_from_cstring(env, "(AND VECTOR (SATISFIES ARRAY-HAS-FILL-POINTER-P))");
  mkcl_FEwrong_type_argument(env, type, x);
}

void
mkcl_FEtype_error_sequence(MKCL, mkcl_object x) {
  mkcl_FEwrong_type_argument(env, @'sequence', x);
}

void
mkcl_FEtype_error_proper_sequence(MKCL, mkcl_object x) {
  mk_cl_error(env, 9, @'simple-type-error',
	      @':format-control', mkcl_make_simple_base_string(env, "Not a proper sequence: ~S"),
	      @':format-arguments', mk_cl_list(env, 1, x),
	      @':expected-type', mkcl_fast_read_from_cstring(env, "si::proper-sequence"),
	      @':datum', x);
}

void
mkcl_FEtype_error_array(MKCL, mkcl_object x) {
  mkcl_FEwrong_type_argument(env, @'array', x);
}

void
mkcl_FEtype_error_stream(MKCL, mkcl_object strm)
{
  mkcl_FEwrong_type_argument(env, @'stream', strm);
}

void
mkcl_FEtype_error_instance(MKCL, mkcl_object x) {
  mkcl_FEwrong_type_argument(env, @'si::instance', x);
}

mkcl_object
mkcl_type_error(MKCL, mkcl_object function, const char *place, mkcl_object o, mkcl_object type)
{
  if (mkcl_Null(mk_cl_fboundp(env, @'si::wrong-type-argument')))
    mkcl_FEwrong_type_argument(env, type, o);
  return mkcl_funcall4(env,
		       @+'si::wrong-type-argument',
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
    return @'character';
  case mkcl_t_fixnum:
    return @'fixnum';
  case mkcl_t_bignum:
    return @'bignum';
  case mkcl_t_ratio:
    return @'ratio';
  case mkcl_t_singlefloat:
    return @'single-float';
  case mkcl_t_doublefloat:
    return @'double-float';
#ifdef MKCL_LONG_FLOAT
  case mkcl_t_longfloat:
    return @'long-float';
#endif
  case mkcl_t_complex:
    return @'complex';
  case mkcl_t_symbol:
    return @'symbol';
  case mkcl_t_package:
    return @'package';
  case mkcl_t_null:
    return @'null';
  case mkcl_t_cons:
    return @'cons';
  case mkcl_t_hashtable:
    return @'hash-table';
  case mkcl_t_array:
    return @'array';
  case mkcl_t_vector:
    return @'vector';
  case mkcl_t_bitvector:
    return @'bit-vector';
  case mkcl_t_string:
    return @'string';
  case mkcl_t_base_string:
    return @'base-string';
  case mkcl_t_stream:
    return @'stream';
  case mkcl_t_readtable:
    return @'readtable';
  case mkcl_t_pathname:
    return @'pathname';
  case mkcl_t_random:
    return @'random-state';
  case mkcl_t_bytecode:
  case mkcl_t_bclosure:
  case mkcl_t_cfun:
  case mkcl_t_cclosure:
    return @'compiled-function';
  case mkcl_t_thread:
    return @'mt::thread';
  case mkcl_t_lock:
    return @'mt::lock';
  case mkcl_t_rwlock:
    return @'mt::rwlock';
  case mkcl_t_semaphore:
    return @'mt::semaphore';
  case mkcl_t_condition_variable:
    return @'mt::condition-variable';
  case mkcl_t_codeblock:
    return @'si::code-block';
  case mkcl_t_foreign:
    return @'si::foreign';
  case mkcl_t_temp_stack_frame:
    return @'si::temp-stack-frame';
  case mkcl_t_instance:
    return @'standard-object';
  case mkcl_t_structure:
    return @'structure-object';
  case mkcl_t_cdisplay:
    return @'si::compiled-closure-display';
  case mkcl_t_clevel_block:
    return @'si::compiled-closure-level';
  case mkcl_t_cmp_dbg_lex_level:
    return @'si::compiled-debug-information';
  case mkcl_t_process:
    return @'mkcl::process';
  case mkcl_t_UTF_8:
    return @'si::UTF-8';
  case mkcl_t_UTF_16:
    return @'si::UTF-16';
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
	p = mkcl_type_error(env, fun,"",p,@'string');
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
  mkcl_FEwrong_type_argument(env, mk_cl_list(env, 3,@'integer',MKCL_MAKE_FIXNUM(0),@'*'), p);
}

void
mkcl_assert_type_package(MKCL, mkcl_object p)
{
  if (mkcl_type_of(p) != mkcl_t_package)
    mkcl_FEwrong_type_argument(env, @'package', p);
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
    mkcl_FEwrong_type_argument(env, @'readtable', p);
}

void
mkcl_assert_type_hash_table(MKCL, mkcl_object p)
{
  if (mkcl_type_of(p) != mkcl_t_hashtable)
    mkcl_FEwrong_type_argument(env, @'hash-table', p);
}

void
mkcl_assert_type_array(MKCL, mkcl_object p)
{
  if (!MKCL_ARRAYP(p))
    mkcl_FEwrong_type_argument(env, @'array', p);
}

void
mkcl_assert_type_vector(MKCL, mkcl_object p)
{
  if (!MKCL_VECTORP(p))
    mkcl_FEwrong_type_argument(env, @'vector', p);
}

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
    t = mk_cl_list(env, 3, @'integer', x, x);
    break;
  case mkcl_t_character:
    {
      int i = MKCL_CHAR_CODE(x);
      if (mkcl_standard_char_p(i)) {
	t = @'standard-char';
      } else if (mkcl_base_char_p(i)) {
	t = @'base-char';
      } else {
	t = @'character';
      }
    }
    break;
  case mkcl_t_null:
    t = @'null';
    break;
  case mkcl_t_cons:
    t = @'cons';
    break;
  case mkcl_t_symbol:
    if ( x == mk_cl_Cnil_symbol )
      t = @'null';
    else if (x == mk_cl_Ct)
      t = @'boolean';
    else if (x->symbol.hpack == mkcl_core.keyword_package)
      t = @'keyword';
    else
      t = @'symbol';
    break;
  case mkcl_t_array:
    if (!(x->array.adjustable || !mkcl_Null(MKCL_CAR(x->array.displaced))))
      t = @'simple-array';
    else
      t = @'array';
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
      t = mk_cl_list(env, 2, @'simple-vector', MKCL_MAKE_FIXNUM(x->vector.dim));
    else
      {
	t = mk_cl_list(env, 3,
		       @'vector',
		       mkcl_elttype_to_symbol(env, mkcl_array_elttype(env, x)),
		       MKCL_MAKE_FIXNUM(x->vector.dim));
      }
    break;
  case mkcl_t_string:
    if (x->string.adjustable || x->string.hasfillp || !mkcl_Null(MKCL_CAR(x->string.displaced)))
      t = @'string';
    else
      t = @'simple-string';
    t = mk_cl_list(env, 2, t, MKCL_MAKE_FIXNUM(x->string.dim));
    break;
  case mkcl_t_base_string:
    if (x->base_string.adjustable || x->base_string.hasfillp || !mkcl_Null(MKCL_CAR(x->base_string.displaced)))
      t = @'base-string';
    else
      t = @'simple-base-string';
    t = mk_cl_list(env, 2, t, MKCL_MAKE_FIXNUM(x->base_string.dim));
    break;
  case mkcl_t_bitvector:
    if (x->vector.adjustable || x->vector.hasfillp || !mkcl_Null(MKCL_CAR(x->vector.displaced)))
      t = @'bit-vector';
    else
      t = @'simple-bit-vector';
    t = mk_cl_list(env, 2, t, MKCL_MAKE_FIXNUM(x->vector.dim));
    break;
  case mkcl_t_structure:
    t = x->str.name;
    break;
  case mkcl_t_stream:
    switch (x->stream.mode)
      {
      case mkcl_smm_synonym:	   t = @'synonym-stream'; break;
      case mkcl_smm_broadcast:	   t = @'broadcast-stream'; break;
      case mkcl_smm_concatenated:  t = @'concatenated-stream'; break;
      case mkcl_smm_two_way:	   t = @'two-way-stream'; break;
      case mkcl_smm_string_input:
      case mkcl_smm_string_output: t = @'string-stream'; break;
      case mkcl_smm_echo:	   t = @'echo-stream'; break;
      default:		           t = @'file-stream'; break;
      }
    break;
  case mkcl_t_pathname:
    t = x->pathname.logical ? @'logical-pathname' : @'pathname';
    break;
  default:
    t = mkcl_type_to_symbol(env, tx);
  }
  @(return t);
}
