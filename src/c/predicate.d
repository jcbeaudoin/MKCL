/* -*- mode: c -*- */
/*
    predicate.c -- Predicates.
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
#include <mkcl/mkcl-math.h>
#include <string.h>

#include <mkcl/internal.h>

mkcl_object
mk_cl_identity(MKCL, mkcl_object x)
{
  @(return x);
}

mkcl_object
mk_cl_null(MKCL, mkcl_object x)
{
  @(return (mkcl_Null(x) ? mk_cl_Ct : mk_cl_Cnil));
}

mkcl_object
mk_cl_symbolp(MKCL, mkcl_object x)
{
  @(return (MKCL_SYMBOLP(x) ? mk_cl_Ct : mk_cl_Cnil));
}

mkcl_object
mk_cl_atom(MKCL, mkcl_object x)
{
  @(return (MKCL_ATOM(x) ? mk_cl_Ct : mk_cl_Cnil));
}

mkcl_object
mk_cl_consp(MKCL, mkcl_object x)
{
  @(return (MKCL_CONSP(x) ? mk_cl_Ct : mk_cl_Cnil));
}

mkcl_object
mk_cl_listp(MKCL, mkcl_object x)
{
  @(return ((MKCL_LISTP(x)) ? mk_cl_Ct : mk_cl_Cnil));
}

mkcl_object
mk_cl_numberp(MKCL, mkcl_object x)
{
  mkcl_type t = mkcl_type_of(x);
  @(return (MKCL_NUMBER_TYPE_P(t) ? mk_cl_Ct : mk_cl_Cnil));
}

mkcl_object
mk_cl_integerp(MKCL, mkcl_object x)
{
  mkcl_type t = mkcl_type_of(x);
  @(return ((t == mkcl_t_fixnum || t == mkcl_t_bignum) ? mk_cl_Ct : mk_cl_Cnil));
}

mkcl_object
mk_cl_rationalp(MKCL, mkcl_object x)
{
  @(return (mkcl_rationalp(env, x) ? mk_cl_Ct : mk_cl_Cnil));
}

mkcl_object
mk_cl_floatp(MKCL, mkcl_object x)
{
  @(return (mkcl_floatp(env, x) ? mk_cl_Ct : mk_cl_Cnil));
}

mkcl_object
mk_cl_realp(MKCL, mkcl_object x)
{
  @(return (mkcl_realp(env, x) ? mk_cl_Ct : mk_cl_Cnil));
}

mkcl_object
mk_cl_complexp(MKCL, mkcl_object x)
{
  @(return (MKCL_COMPLEXP(x) ? mk_cl_Ct : mk_cl_Cnil));
}

mkcl_object
mk_cl_characterp(MKCL, mkcl_object x)
{
  @(return (MKCL_CHARACTERP(x) ? mk_cl_Ct : mk_cl_Cnil));
}

mkcl_object
mk_mkcl_base_char_p(MKCL, mkcl_object c)
{
  @(return ((MKCL_CHARACTERP(c) && MKCL_BASE_CHAR_P(c)) ? mk_cl_Ct : mk_cl_Cnil));
}

mkcl_object
mk_cl_stringp(MKCL, mkcl_object x)
{
  @(return (mkcl_stringp(env, x)? mk_cl_Ct : mk_cl_Cnil));
}

mkcl_object
mk_cl_bit_vector_p(MKCL, mkcl_object x)
{
  @(return ((mkcl_type_of(x) == mkcl_t_bitvector) ? mk_cl_Ct : mk_cl_Cnil));
}

mkcl_object
mk_cl_vectorp(MKCL, mkcl_object x)
{
  mkcl_type t = mkcl_type_of(x);
  @(return (MKCL_VECTORP(x) ? mk_cl_Ct : mk_cl_Cnil));
}

mkcl_object
mk_cl_simple_string_p(MKCL, mkcl_object x)
{
  @(return (mkcl_simple_string_p(env, x) ? mk_cl_Ct : mk_cl_Cnil));
}

mkcl_object
mk_si_simple_base_string_p(MKCL, mkcl_object x)
{
  @(return (mkcl_simple_base_string_p(env, x) ? mk_cl_Ct : mk_cl_Cnil));
}

mkcl_object
mk_si_base_string_p(MKCL, mkcl_object x)
{
  @(return ((mkcl_type_of(x) == mkcl_t_base_string) ? mk_cl_Ct : mk_cl_Cnil));
}

mkcl_object
mk_cl_simple_bit_vector_p(MKCL, mkcl_object x)
{
  @(return (mkcl_simple_bit_vector_p(env, x) ? mk_cl_Ct : mk_cl_Cnil));
}

mkcl_object
mk_cl_simple_vector_p(MKCL, mkcl_object x)
{
  @(return (mkcl_simple_vector_p(env, x) ? mk_cl_Ct : mk_cl_Cnil));
}

mkcl_object
mk_cl_arrayp(MKCL, mkcl_object x)
{
  mkcl_type t = mkcl_type_of(x);
  @(return (MKCL_ARRAY_TYPE_P(t) ? mk_cl_Ct : mk_cl_Cnil));
}

mkcl_object
mk_cl_packagep(MKCL, mkcl_object x)
{
  @(return ((mkcl_type_of(x) == mkcl_t_package) ? mk_cl_Ct : mk_cl_Cnil));
}

mkcl_object
mk_cl_functionp(MKCL, mkcl_object x)
{
  @(return (mkcl_functionp(env, x) ? mk_cl_Ct : mk_cl_Cnil));
}

mkcl_object
mk_cl_compiled_function_p(MKCL, mkcl_object x)
{
  @(return (mkcl_compiled_function_p(env, x) ? mk_cl_Ct : mk_cl_Cnil));
}

mkcl_object
mk_si_bytecodep(MKCL, mkcl_object x)
{
  @(return (mkcl_bytecodep(env, x) ? mk_cl_Ct : mk_cl_Cnil));
}

mkcl_object
mk_cl_eq(MKCL, mkcl_object x, mkcl_object y)
{
  @(return ((x == y) ? mk_cl_Ct : mk_cl_Cnil));
}

/*
 * EQL-comparison of floats. If we are using signed zeros and NaNs,
 * numeric comparison of floating points is not equivalent to bit-wise
 * equality. In particular every two NaNs always give false
 *	(= #1=(/ 0.0 0.0) #1#) => NIL
 * and signed zeros always compare equal
 *	(= 0 -0.0) => T
 * which is not the same as what EQL should return
 *	(EQL #1=(/ 0.0 0.0) #1#) => T
 *	(EQL 0 -0.0) => NIL
 *
 * Furthermore, we can not use bit comparisons because in some platforms
 * long double has unused bits that makes two long floats be = but not eql.
 */
#if !defined(MKCL_SIGNED_ZERO) && !defined(MKCL_IEEE_FP)
# define FLOAT_EQL(a,b,type) return (a) == (b)
#else
# define FLOAT_EQL(a,b,type) {                          \
    type xa = (a), xb = (b);				\
    if (xa == xb) {					\
      return signbit(xa) == signbit(xb);		\
    } else if (isnan(xa) || isnan(xb)) {		\
      return !memcmp(&xa, &xb, sizeof(type));		\
    } else {						\
      return 0;						\
    } }
#endif

bool
mkcl_eql_unboxable_numbers(MKCL, mkcl_object x, mkcl_object y, mkcl_type t)
{
  switch (t) {
  case mkcl_t_bignum:
    return (_mkcl_big_compare(x, y) == 0);
  case mkcl_t_ratio:
    return (mkcl_eql(env, x->ratio.num, y->ratio.num) &&
	    mkcl_eql(env, x->ratio.den, y->ratio.den));
  case mkcl_t_singlefloat:
    FLOAT_EQL(mkcl_single_float(x), mkcl_single_float(y), float);
  case mkcl_t_doublefloat:
    FLOAT_EQL(mkcl_double_float(x), mkcl_double_float(y), double);
#ifdef MKCL_LONG_FLOAT
  case mkcl_t_longfloat:
    FLOAT_EQL(mkcl_long_float(x), mkcl_long_float(y), long double);
#endif
  case mkcl_t_complex:
    return (mkcl_eql(env, x->_complex.real, y->_complex.real) &&
	    mkcl_eql(env, x->_complex.imag, y->_complex.imag));
  default:
    return FALSE;
  }
}

#if 0 /* inlined */
bool
mkcl_eql(MKCL, mkcl_object x, mkcl_object y)
{
  mkcl_type t;

  if (x == y)
    return(TRUE);
  else if (MKCL_IMMEDIATE(x) || MKCL_IMMEDIATE(y) || mkcl_Null(x) ||  mkcl_Null(y))
    return(FALSE);
  else if ((t = x->d.t) != y->d.t)
    return(FALSE);
  else if (t > mkcl_t_reserved_bin0_7) /* last type tag for numbers */
    return(FALSE);
  else
    return(mkcl_eql_unboxable_numbers(env, x, y, t));
}
#endif

mkcl_object
mk_cl_eql(MKCL, mkcl_object x, mkcl_object y)
{
  mkcl_call_stack_check(env);
  @(return (mkcl_eql(env, x, y) ? mk_cl_Ct : mk_cl_Cnil))
}

bool
mkcl_equal(MKCL, register mkcl_object x, mkcl_object y)
{
  mkcl_type tx, ty;
 BEGIN:
  if (x==y)
    return(TRUE);
  tx = mkcl_type_of(x);
  ty = mkcl_type_of(y);
  switch (tx) {
  case mkcl_t_null:
    return FALSE;
  case mkcl_t_cons:
    if (tx != ty || !mkcl_equal(env, MKCL_CAR(x), MKCL_CAR(y)))
      return FALSE;
    x = MKCL_CDR(x);
    y = MKCL_CDR(y);
    goto BEGIN;
  case mkcl_t_symbol:
  case mkcl_t_vector:
  case mkcl_t_array:
  case mkcl_t_fixnum:
    return FALSE;
  case mkcl_t_bignum:
    return (tx == ty) && (_mkcl_big_compare(x,y) == 0);
  case mkcl_t_ratio:
    return (tx == ty) && mkcl_eql(env, x->ratio.num, y->ratio.num) &&
      mkcl_eql(env, x->ratio.den, y->ratio.den);
  case mkcl_t_singlefloat: {
    if (tx != ty) return 0;
    FLOAT_EQL(mkcl_single_float(x), mkcl_single_float(y), float);
  }
  case mkcl_t_doublefloat: {
    if (tx != ty) return 0;
    FLOAT_EQL(mkcl_double_float(x), mkcl_double_float(y), double);
  }
#ifdef MKCL_LONG_FLOAT
  case mkcl_t_longfloat: {
    if (tx != ty) return 0;
    FLOAT_EQL(mkcl_long_float(x), mkcl_long_float(y), long double);
  }
#endif
  case mkcl_t_complex:
    return (tx == ty) && mkcl_eql(env, x->_complex.real, y->_complex.real) &&
      mkcl_eql(env, x->_complex.imag, y->_complex.imag);
  case mkcl_t_character:
    return (tx == ty) && (MKCL_CHAR_CODE(x) == MKCL_CHAR_CODE(y));
  case mkcl_t_base_string:
  case mkcl_t_string:
    if (ty != mkcl_t_base_string && ty != mkcl_t_string)
      return FALSE;
    return mkcl_string_E(env, x, y);
  case mkcl_t_bitvector: {
    mkcl_index i, ox, oy;
    if (ty != tx)
      return FALSE;
    if (x->vector.fillp != y->vector.fillp)
      return(FALSE);
    ox = x->vector.bit_offset;
    oy = y->vector.bit_offset;
    for (i = 0;  i < x->vector.fillp;  i++)
      if ((mkcl_bit_bundle(x->vector.self.bit, i+ox) & mkcl_bundle_bit_mask(i+ox))
	  !=(mkcl_bit_bundle(y->vector.self.bit, i+oy) & mkcl_bundle_bit_mask(i+oy)))
	return(FALSE);
    return(TRUE);
  }
  case mkcl_t_pathname:
    return ty == tx &&
      (x->pathname.logical == y->pathname.logical) &&
      mkcl_equal(env, x->pathname.host, y->pathname.host) &&
      mkcl_equal(env, x->pathname.device, y->pathname.device) &&
      mkcl_equal(env, x->pathname.directory, y->pathname.directory) &&
      mkcl_equal(env, x->pathname.name, y->pathname.name) &&
      mkcl_equal(env, x->pathname.type, y->pathname.type) &&
      mkcl_equal(env, x->pathname.version, y->pathname.version);
  case mkcl_t_foreign:
    return (tx == ty) && (x->foreign.data == y->foreign.data);
  default:
    return FALSE;
  }
}

mkcl_object
mk_cl_equal(MKCL, mkcl_object x, mkcl_object y)
{
  mkcl_call_stack_check(env);
  @(return (mkcl_equal(env, x, y) ? mk_cl_Ct : mk_cl_Cnil));
}

bool
mkcl_equalp(MKCL, mkcl_object x, mkcl_object y)
{
  mkcl_type tx, ty;
  mkcl_index j;
 BEGIN:
  if (x == y)
    return TRUE;
  tx = mkcl_type_of(x);
  ty = mkcl_type_of(y);

  switch (tx) {
  case mkcl_t_fixnum:
  case mkcl_t_bignum:
  case mkcl_t_ratio:
  case mkcl_t_singlefloat:
  case mkcl_t_doublefloat:
#ifdef MKCL_LONG_FLOAT
  case mkcl_t_longfloat:
#endif
  case mkcl_t_complex:
    return MKCL_NUMBER_TYPE_P(ty) && mkcl_number_equalp(env, x, y);
  case mkcl_t_vector:
  case mkcl_t_base_string:
  case mkcl_t_bitvector:
  case mkcl_t_string:
    if (ty != mkcl_t_vector && ty != mkcl_t_base_string && ty != mkcl_t_bitvector && ty != mkcl_t_string)
      return FALSE;
    j = x->vector.fillp;
    if (j != y->vector.fillp)
      return FALSE;
    goto ARRAY;
  case mkcl_t_array:
    if (ty != mkcl_t_array || x->array.rank != y->array.rank)
      return FALSE;
    if (x->array.rank > 1) {
      mkcl_index i = 0;
      for (i = 0; i < x->array.rank; i++)
	if (x->array.dims[i] != y->array.dims[i])
	  return(FALSE);
    }
    if (x->array.dim != y->array.dim)
      return(FALSE);
    j=x->array.dim;
  ARRAY: {
      mkcl_index i;
      for (i = 0;  i < j;  i++)
	if (!mkcl_equalp(env, mkcl_aref_index(env, x, i), mkcl_aref_index(env, y, i)))
	  return(FALSE);
      return(TRUE);
    }
  case mkcl_t_character:
    return (ty == tx) && mkcl_char_equal(env, x, y);
  case mkcl_t_cons:
    if ( tx != ty )
      return FALSE;
    if (!mkcl_equalp(env, MKCL_CAR(x), MKCL_CAR(y)))
      return(FALSE);
    x = MKCL_CDR(x);
    y = MKCL_CDR(y);
    goto BEGIN;
  case mkcl_t_null:
    return FALSE; /* since they were not eq! */
  case mkcl_t_symbol:
    return FALSE; /* since they were not eq! */
  case mkcl_t_instance: {
    mkcl_index i;
    if ((ty != tx) || (MKCL_CLASS_OF(x) != MKCL_CLASS_OF(y))
	|| (x->instance.length != y->instance.length) /* should we force an instance update? JCB */
	)                                             /* most probably yes... FIXME! */
      return(FALSE);
    for (i = 0;  i < x->instance.length;  i++)
      if (!mkcl_equalp(env, x->instance.slots[i], y->instance.slots[i]))
	return(FALSE);
    return(TRUE);
  }
#if 0 /* !CLOS */
  case mkcl_t_structure: {
    mkcl_index i;
    if ((tx != ty) || (x->str.name != y->str.name))
      return(FALSE);
    for (i = 0;  i < x->str.length;  i++)
      if (!mkcl_equalp(x->str.self[i], y->str.self[i]))
	return(FALSE);
    return(TRUE);
  }
#endif /* !CLOS */
  case mkcl_t_pathname:
    return (tx == ty) && mkcl_equal(env, x, y);
  case mkcl_t_hashtable:
    {
      mkcl_index i;
      struct mkcl_hashtable_entry *ex, *ey;
      if (tx != ty ||
	  x->hash.entries != y->hash.entries ||
	  x->hash.test != y->hash.test)
	return(FALSE);
      for (i = 0; i < x->hash.size; i++)
	{
	  ex = x->hash.data[i];

	  for (; ex != NULL; ex = ex->next)
	    {
	      ey = mkcl_search_hash(env, ex->key, y);
	      if ( ey == NULL || !mkcl_equalp(env, ex->value, ey->value))
		return(FALSE);
	    }
	}
      return(TRUE);
    }
  case mkcl_t_random:
    return (tx == ty) && mkcl_equalp(env, x->random.value, y->random.value);
  default:
    return mkcl_eql(env, x,y);
  }
}

mkcl_object
mk_cl_equalp(MKCL, mkcl_object x, mkcl_object y)
{
  mkcl_call_stack_check(env);
  @(return (mkcl_equalp(env, x, y) ? mk_cl_Ct : mk_cl_Cnil))
}

mkcl_object
mk_si_fixnump(MKCL, mkcl_object x)
{
  @(return (MKCL_FIXNUMP(x) ? mk_cl_Ct : mk_cl_Cnil));
}

mkcl_object
mk_si_unbound_value_p(MKCL, mkcl_object val)
{
  if ( MKCL_OBJNULL == val )
    { @(return mk_cl_Ct); }
  else
    { @(return mk_cl_Cnil); }
}

