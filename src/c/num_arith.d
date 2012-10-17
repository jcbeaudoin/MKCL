/* -*- mode: c -*- */
/*
    num_arith.c  -- Arithmetic operations
*/
/*
    Copyright (c) 1990, Giuseppe Attardi.
    Copyright (c) 2001, Juan Jose Garcia Ripoll.
    Copyright (c) 2011, Jean-Claude Beaudoin.

    MKCL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 3 of the License, or (at your option) any later version.

    See file '../../Copyright' for full details.
*/

#include <mkcl/mkcl.h>

#include <mkcl/internal.h>

#include <stdlib.h>

/*  (*		)  */

@(defun * (&rest nums)
	mkcl_object prod = MKCL_MAKE_FIXNUM(1);
@
  /* INV: type check in mkcl_times() */
  while (narg--)
    prod = mkcl_times(env, prod, mkcl_va_arg(nums));
  @(return prod);
@)

mkcl_object
mkcl_word_times(MKCL, mkcl_word i, mkcl_word j)
{
  mkcl_object x = _mkcl_big_register0();

#if MKCL_LONG_BITS < MKCL_WORD_BITS
  mkcl_object y = _mkcl_big_register1();
  _mkcl_big_set_fixnum(x, i);
  _mkcl_big_set_fixnum(y, j);
  _mkcl_big_mul(x, x, y);
#else
  _mkcl_big_set_si(x, i);
  _mkcl_big_mul_si(x, x, j);
#endif
  return _mkcl_big_register_normalize(env, x);
}

static mkcl_object
_mkcl_big_times_big(MKCL, mkcl_object x, mkcl_object y);

static mkcl_object
_mkcl_big_times_fix(MKCL, mkcl_object b, mkcl_word i)
{
  mkcl_object z;

  if (i == 1)
    return b;
  z = _mkcl_big_register0();
  if (i == -1) {
    _mkcl_big_complement(z, b);
  } else {
#if MKCL_LONG_BITS < MKCL_WORD_BITS
    /* Maybe we could test for LONG_MIN and LONG_MAX but
       that would probably be a waist of time. JCB */
    z = _mkcl_big_register1();
    _mkcl_big_set_fixnum(z, i);
    return _mkcl_big_times_big(env, b, z);
#else
    _mkcl_big_mul_si(z, b, i);
#endif
  }
  return _mkcl_big_register_normalize(env, z);
}

static mkcl_object
_mkcl_big_times_big(MKCL, mkcl_object x, mkcl_object y)
{
  mkcl_object z;
  z = _mkcl_big_register0();
  _mkcl_big_mul(z, x, y);
  return _mkcl_big_register_normalize(env, z);
}

mkcl_object
mkcl_times(MKCL, mkcl_object x, mkcl_object y)
{
  mkcl_object z, z1;

  switch (mkcl_type_of(x)) {
  case mkcl_t_fixnum:
    switch (mkcl_type_of(y)) {
    case mkcl_t_fixnum:
      return mkcl_word_times(env, mkcl_fixnum_to_word(x),mkcl_fixnum_to_word(y));
    case mkcl_t_bignum:
      return _mkcl_big_times_fix(env, y, mkcl_fixnum_to_word(x));
    case mkcl_t_ratio:
      z = mkcl_times(env, x, y->ratio.num);
      z = mkcl_make_ratio(env, z, y->ratio.den);
      return(z);
    case mkcl_t_singlefloat:
      return mkcl_make_singlefloat(env, mkcl_fixnum_to_word(x) * mkcl_single_float(y));
    case mkcl_t_doublefloat:
      return mkcl_make_doublefloat(env, mkcl_fixnum_to_word(x) * mkcl_double_float(y));
#ifdef MKCL_LONG_FLOAT
    case mkcl_t_longfloat:
      return mkcl_make_longfloat(env, mkcl_fixnum_to_word(x) * mkcl_long_float(y));
#endif
    case mkcl_t_complex:
      goto COMPLEX;
    default:
      mkcl_FEtype_error_number(env, y);
    }
  case mkcl_t_bignum:
    switch (mkcl_type_of(y)) {
    case mkcl_t_fixnum:
      return _mkcl_big_times_fix(env, x, mkcl_fixnum_to_word(y));
    case mkcl_t_bignum:
      return _mkcl_big_times_big(env, x, y);
    case mkcl_t_ratio:
      z = mkcl_times(env, x, y->ratio.num);
      z = mkcl_make_ratio(env, z, y->ratio.den);
      return(z);
    case mkcl_t_singlefloat:
      return mkcl_make_singlefloat(env, mkcl_to_double(env, x) * mkcl_single_float(y));
    case mkcl_t_doublefloat:
      return mkcl_make_doublefloat(env, mkcl_to_double(env, x) * mkcl_double_float(y));
#ifdef MKCL_LONG_FLOAT
    case mkcl_t_longfloat:
      return mkcl_make_longfloat(env, mkcl_to_double(env, x) * mkcl_long_float(y));
#endif
    case mkcl_t_complex:
      goto COMPLEX;
    default:
      mkcl_FEtype_error_number(env, y);
    }
  case mkcl_t_ratio:
    switch (mkcl_type_of(y)) {
    case mkcl_t_fixnum:
    case mkcl_t_bignum:
      z = mkcl_times(env, x->ratio.num, y);
      z = mkcl_make_ratio(env, z, x->ratio.den);
      return(z);
    case mkcl_t_ratio:
      z = mkcl_times(env, x->ratio.num,y->ratio.num);
      z1 = mkcl_times(env, x->ratio.den,y->ratio.den);
      z = mkcl_make_ratio(env, z, z1);
      return(z);
    case mkcl_t_singlefloat:
      return mkcl_make_singlefloat(env, mkcl_to_double(env, x) * mkcl_single_float(y));
    case mkcl_t_doublefloat:
      return mkcl_make_doublefloat(env, mkcl_to_double(env, x) * mkcl_double_float(y));
#ifdef MKCL_LONG_FLOAT
    case mkcl_t_longfloat:
      return mkcl_make_longfloat(env, mkcl_to_double(env, x) * mkcl_long_float(y));
#endif
    case mkcl_t_complex:
      goto COMPLEX;
    default:
      mkcl_FEtype_error_number(env, y);
    }
  case mkcl_t_singlefloat: {
    float fx = mkcl_single_float(x);
    switch (mkcl_type_of(y)) {
    case mkcl_t_fixnum:
      return mkcl_make_singlefloat(env, fx * mkcl_fixnum_to_word(y));
    case mkcl_t_bignum:
    case mkcl_t_ratio:
      return mkcl_make_singlefloat(env, fx * mkcl_to_double(env, y));
    case mkcl_t_singlefloat:
      return mkcl_make_singlefloat(env, fx * mkcl_single_float(y));
    case mkcl_t_doublefloat:
      return mkcl_make_doublefloat(env, fx * mkcl_double_float(y));
#ifdef MKCL_LONG_FLOAT
    case mkcl_t_longfloat:
      return mkcl_make_longfloat(env, fx * mkcl_long_float(y));
#endif
    case mkcl_t_complex:
      goto COMPLEX;
    default:
      mkcl_FEtype_error_number(env, y);
    }
  }
  case mkcl_t_doublefloat: {
    switch (mkcl_type_of(y)) {
    case mkcl_t_fixnum:
      return mkcl_make_doublefloat(env, mkcl_double_float(x) * mkcl_fixnum_to_word(y));
    case mkcl_t_bignum:
    case mkcl_t_ratio:
      return mkcl_make_doublefloat(env, mkcl_double_float(x) * mkcl_to_double(env, y));
    case mkcl_t_singlefloat:
      return mkcl_make_doublefloat(env, mkcl_double_float(x) * mkcl_single_float(y));
    case mkcl_t_doublefloat:
      return mkcl_make_doublefloat(env, mkcl_double_float(x) * mkcl_double_float(y));
#ifdef MKCL_LONG_FLOAT
    case mkcl_t_longfloat:
      return mkcl_make_longfloat(env, mkcl_double_float(x) * mkcl_long_float(y));
#endif
    case mkcl_t_complex: {
      COMPLEX: /* INV: x is real, y is complex */
      return mkcl_make_complex(env, mkcl_times(env, x, y->_complex.real), mkcl_times(env, x, y->_complex.imag));
    }
    default:
      mkcl_FEtype_error_number(env, y);
    }
  }
#ifdef MKCL_LONG_FLOAT
  case mkcl_t_longfloat: {
    long double lx = mkcl_long_float(x);
    switch (mkcl_type_of(y)) {
    case mkcl_t_fixnum:
      return mkcl_make_longfloat(env, lx * mkcl_fixnum_to_word(y));
    case mkcl_t_bignum:
    case mkcl_t_ratio:
      return mkcl_make_longfloat(env, lx * mkcl_to_double(env, y));
    case mkcl_t_singlefloat:
      return mkcl_make_longfloat(env, lx * mkcl_single_float(y));
    case mkcl_t_doublefloat:
      return mkcl_make_longfloat(env, lx * mkcl_double_float(y));
    case mkcl_t_longfloat:
      {
	long double ly = mkcl_long_float(y);
	long double lz = lx * ly;
	return mkcl_make_longfloat(env, lz);
      }
    case mkcl_t_complex:
      goto COMPLEX;
    default:
      mkcl_FEtype_error_number(env, y);
    }
  }
#endif /* def MKCL_LONG_FLOAT */
  case mkcl_t_complex:
    {
      mkcl_object z11, z12, z21, z22;

      if (mkcl_type_of(y) != mkcl_t_complex) {
	mkcl_object aux = x;
	x = y; y = aux;
	goto COMPLEX;
      }
      z11 = mkcl_times(env, x->_complex.real, y->_complex.real);
      z12 = mkcl_times(env, x->_complex.imag, y->_complex.imag);
      z21 = mkcl_times(env, x->_complex.imag, y->_complex.real);
      z22 = mkcl_times(env, x->_complex.real, y->_complex.imag);
      return(mkcl_make_complex(env, 
			      mkcl_minus(env, z11, z12),
			      mkcl_plus(env, z21, z22)));
    }
  default:
    mkcl_FEtype_error_number(env, x);
  }
}

/* (+          )   */
@(defun + (&rest nums)
	mkcl_object sum = MKCL_MAKE_FIXNUM(0);
@
  /* INV: type check is in mkcl_plus() */
  while (narg--)
  sum = mkcl_plus(env, sum, mkcl_va_arg(nums));
  @(return sum);
@)

mkcl_object
mkcl_plus(MKCL, mkcl_object x, mkcl_object y)
{
  mkcl_word i, j;
  mkcl_object z, z1;

  switch (mkcl_type_of(x)) {
  case mkcl_t_fixnum:
    switch (mkcl_type_of(y)) {
    case mkcl_t_fixnum:
      return mkcl_make_integer(env, mkcl_fixnum_to_word(x) + mkcl_fixnum_to_word(y));
    case mkcl_t_bignum:
      if ((i = mkcl_fixnum_to_word(x)) == 0)
	return(y);
      z = _mkcl_big_register0();
      if (i > 0)
#if MKCL_LONG_BITS < MKCL_WORD_BITS
	{
	  mkcl_object x_big = _mkcl_big_register1();
	  _mkcl_big_set_fixnum(x_big, i);
	  _mkcl_big_add(z, y, x_big);
	}
#else
	_mkcl_big_add_ui(z, y, i);
#endif
      else
#if MKCL_LONG_BITS < MKCL_WORD_BITS
	{
	  mkcl_object x_big = _mkcl_big_register1();
	  _mkcl_big_set_fixnum(x_big, -i);
	  _mkcl_big_sub(z, y, x_big);
	}
#else
	_mkcl_big_sub_ui(z, y, -i);
#endif
      return _mkcl_big_register_normalize(env, z);
    case mkcl_t_ratio:
      z = mkcl_times(env, x, y->ratio.den);
      z = mkcl_plus(env, z, y->ratio.num);
      z = mkcl_make_ratio(env, z, y->ratio.den);
      return(z);
    case mkcl_t_singlefloat:
      return mkcl_make_singlefloat(env, mkcl_fixnum_to_word(x) + mkcl_single_float(y));
    case mkcl_t_doublefloat:
      return mkcl_make_doublefloat(env, mkcl_fixnum_to_word(x) + mkcl_double_float(y));
#ifdef MKCL_LONG_FLOAT
    case mkcl_t_longfloat:
      return mkcl_make_longfloat(env, mkcl_fixnum_to_word(x) + mkcl_long_float(y));
#endif
    case mkcl_t_complex:
    COMPLEX: /* INV: x is real, y is complex */
      return mkcl_make_complex(env,
			      mkcl_plus(env, x, y->_complex.real),
			      y->_complex.imag);
    default:
      mkcl_FEtype_error_number(env, y);
    }
  case mkcl_t_bignum:
    switch (mkcl_type_of(y)) {
    case mkcl_t_fixnum:
      if ((j = mkcl_fixnum_to_word(y)) == 0)
	return(x);
      z = _mkcl_big_register0();
      if (j > 0)
#if MKCL_LONG_BITS < MKCL_WORD_BITS
	{
	  mkcl_object y_big = _mkcl_big_register1();
	  _mkcl_big_set_fixnum(y_big, j);
	  _mkcl_big_add(z, x, y_big);
	}
#else
	_mkcl_big_add_ui(z, x, j);
#endif
      else
#if MKCL_LONG_BITS < MKCL_WORD_BITS
	{
	  mkcl_object y_big = _mkcl_big_register1();
	  _mkcl_big_set_fixnum(y_big, (-j));
	  _mkcl_big_sub(z, x, y_big);
	}
#else
	_mkcl_big_sub_ui(z, x, (-j));
#endif
      return _mkcl_big_register_normalize(env, z);
    case mkcl_t_bignum:
      z = _mkcl_big_register0();
      _mkcl_big_add(z, x, y);
      return _mkcl_big_register_normalize(env, z);
    case mkcl_t_ratio:
      z = mkcl_times(env, x, y->ratio.den);
      z = mkcl_plus(env, z, y->ratio.num);
      z = mkcl_make_ratio(env, z, y->ratio.den);
      return(z);
    case mkcl_t_singlefloat:
      return mkcl_make_singlefloat(env, mkcl_to_double(env, x) + mkcl_single_float(y));
    case mkcl_t_doublefloat:
      return mkcl_make_doublefloat(env, mkcl_to_double(env, x) + mkcl_double_float(y));
#ifdef MKCL_LONG_FLOAT
    case mkcl_t_longfloat:
      return mkcl_make_longfloat(env, mkcl_to_double(env, x) + mkcl_long_float(y));
#endif
    case mkcl_t_complex:
      goto COMPLEX;
    default:
      mkcl_FEtype_error_number(env, y);
    }
  case mkcl_t_ratio:
    switch (mkcl_type_of(y)) {
    case mkcl_t_fixnum:
    case mkcl_t_bignum:
      z = mkcl_times(env, x->ratio.den, y);
      z = mkcl_plus(env, x->ratio.num, z);
      z = mkcl_make_ratio(env, z, x->ratio.den);
      return(z);
    case mkcl_t_ratio:
      z1 = mkcl_times(env, x->ratio.num,y->ratio.den);
      z = mkcl_times(env, x->ratio.den,y->ratio.num);
      z = mkcl_plus(env, z1, z);
      z1 = mkcl_times(env, x->ratio.den,y->ratio.den);
      z = mkcl_make_ratio(env, z, z1);
      return(z);
    case mkcl_t_singlefloat:
      return mkcl_make_singlefloat(env, mkcl_to_double(env, x) + mkcl_single_float(y));
    case mkcl_t_doublefloat:
      return mkcl_make_doublefloat(env, mkcl_to_double(env, x) + mkcl_double_float(y));
#ifdef MKCL_LONG_FLOAT
    case mkcl_t_longfloat:
      return mkcl_make_longfloat(env, mkcl_to_double(env, x) + mkcl_long_float(y));
#endif
    case mkcl_t_complex:
      goto COMPLEX;
    default:
      mkcl_FEtype_error_number(env, y);
    }
  case mkcl_t_singlefloat:
    switch (mkcl_type_of(y)) {
    case mkcl_t_fixnum:
      return mkcl_make_singlefloat(env, mkcl_single_float(x) + mkcl_fixnum_to_word(y));
    case mkcl_t_bignum:
    case mkcl_t_ratio:
      return mkcl_make_singlefloat(env, mkcl_single_float(x) + mkcl_to_double(env, y));
    case mkcl_t_singlefloat:
      return mkcl_make_singlefloat(env, mkcl_single_float(x) + mkcl_single_float(y));
    case mkcl_t_doublefloat:
      return mkcl_make_doublefloat(env, mkcl_single_float(x) + mkcl_double_float(y));
#ifdef MKCL_LONG_FLOAT
    case mkcl_t_longfloat:
      return mkcl_make_longfloat(env, mkcl_single_float(x) + mkcl_long_float(y));
#endif
    case mkcl_t_complex:
      goto COMPLEX;
    default:
      mkcl_FEtype_error_number(env, y);
    }
  case mkcl_t_doublefloat:
    switch (mkcl_type_of(y)) {
    case mkcl_t_fixnum:
      return mkcl_make_doublefloat(env, mkcl_double_float(x) + mkcl_fixnum_to_word(y));
    case mkcl_t_bignum:
    case mkcl_t_ratio:
      return mkcl_make_doublefloat(env, mkcl_double_float(x) + mkcl_to_double(env, y));
    case mkcl_t_singlefloat:
      return mkcl_make_doublefloat(env, mkcl_double_float(x) + mkcl_single_float(y));
    case mkcl_t_doublefloat:
      return mkcl_make_doublefloat(env, mkcl_double_float(x) + mkcl_double_float(y));
#ifdef MKCL_LONG_FLOAT
    case mkcl_t_longfloat:
      return mkcl_make_longfloat(env, mkcl_double_float(x) + mkcl_long_float(y));
#endif
    case mkcl_t_complex:
      goto COMPLEX;
    default:
      mkcl_FEtype_error_number(env, y);
    }
#ifdef MKCL_LONG_FLOAT
  case mkcl_t_longfloat:
    switch (mkcl_type_of(y)) {
    case mkcl_t_fixnum:
      return mkcl_make_longfloat(env, mkcl_long_float(x) + mkcl_fixnum_to_word(y));
    case mkcl_t_bignum:
    case mkcl_t_ratio:
      return mkcl_make_longfloat(env, mkcl_long_float(x) + mkcl_to_double(env, y));
    case mkcl_t_singlefloat:
      return mkcl_make_longfloat(env, mkcl_long_float(x) + mkcl_single_float(y));
    case mkcl_t_doublefloat:
      return mkcl_make_longfloat(env, mkcl_long_float(x) + mkcl_double_float(y));
    case mkcl_t_longfloat:
      return mkcl_make_longfloat(env, mkcl_long_float(x) + mkcl_long_float(y));
    case mkcl_t_complex:
      goto COMPLEX;
    default:
      mkcl_FEtype_error_number(env, y);
    }
#endif /* def MKCL_LONG_FLOAT */
  case mkcl_t_complex:
    if (mkcl_type_of(y) != mkcl_t_complex) {
      mkcl_object aux = x;
      x = y; y = aux;
      goto COMPLEX;
    }
    z = mkcl_plus(env, x->_complex.real, y->_complex.real);
    z1 = mkcl_plus(env, x->_complex.imag, y->_complex.imag);
    z = mkcl_make_complex(env, z, z1);
    return(z);
  default:
    mkcl_FEtype_error_number(env, x);
  }
}

/*  (-		)  */
@(defun - (num &rest nums)
	mkcl_object diff;
@
  /* INV: argument type check in number_{negate,minus}() */
  if (narg == 1)
    { @(return mkcl_negate(env, num)); }
  for (diff = num;  --narg; )
    diff = mkcl_minus(env, diff, mkcl_va_arg(nums));
  @(return diff);
@)

mkcl_object
mkcl_minus(MKCL, mkcl_object x, mkcl_object y)
{
  mkcl_word i, j;
  mkcl_object z, z1;

  switch (mkcl_type_of(x)) {
  case mkcl_t_fixnum:
    switch(mkcl_type_of(y)) {
    case mkcl_t_fixnum:
      return mkcl_make_integer(env, mkcl_fixnum_to_word(x) - mkcl_fixnum_to_word(y));
    case mkcl_t_bignum:
      z = _mkcl_big_register0();
      i = mkcl_fixnum_to_word(x);
      if (i > 0)
#if MKCL_LONG_BITS < MKCL_WORD_BITS
	{
	  mkcl_object x_big = _mkcl_big_register1();
	  _mkcl_big_set_fixnum(x_big, i);
	  _mkcl_big_sub(z, y, x_big);
	}
#else
	_mkcl_big_sub_ui(z, y, i);
#endif
      else
#if MKCL_LONG_BITS < MKCL_WORD_BITS
	{
	  mkcl_object x_big = _mkcl_big_register1();
	  _mkcl_big_set_fixnum(x_big, -i);
	  _mkcl_big_add(z, y, x_big);
	}
#else
	_mkcl_big_add_ui(z, y, -i);
#endif
      _mkcl_big_complement(z, z);
      return _mkcl_big_register_normalize(env, z);
    case mkcl_t_ratio:
      z = mkcl_times(env, x, y->ratio.den);
      z = mkcl_minus(env, z, y->ratio.num);
      z = mkcl_make_ratio(env, z, y->ratio.den);
      return(z);
    case mkcl_t_singlefloat:
      return mkcl_make_singlefloat(env, mkcl_fixnum_to_word(x) - mkcl_single_float(y));
    case mkcl_t_doublefloat:
      return mkcl_make_doublefloat(env, mkcl_fixnum_to_word(x) - mkcl_double_float(y));
#ifdef MKCL_LONG_FLOAT
    case mkcl_t_longfloat:
      return mkcl_make_longfloat(env, mkcl_fixnum_to_word(x) - mkcl_long_float(y));
#endif
    case mkcl_t_complex:
      goto COMPLEX;
    default:
      mkcl_FEtype_error_number(env, y);
    }
  case mkcl_t_bignum:
    switch (mkcl_type_of(y)) {
    case mkcl_t_fixnum:
      if ((j = mkcl_fixnum_to_word(y)) == 0)
	return(x);
      z = _mkcl_big_register0();
      if (j > 0)
#if MKCL_LONG_BITS < MKCL_WORD_BITS
	{
	  mkcl_object y_big = _mkcl_big_register1();
	  _mkcl_big_set_fixnum(y_big, j);
	  _mkcl_big_sub(z, x, y_big);
	}
#else
	_mkcl_big_sub_ui(z, x, j);
#endif
      else
#if MKCL_LONG_BITS < MKCL_WORD_BITS
	{
	  mkcl_object y_big = _mkcl_big_register1();
	  _mkcl_big_set_fixnum(y_big, -j);
	  _mkcl_big_add(z, x, y_big);
	}
#else
	_mkcl_big_add_ui(z, x, -j);
#endif
      return _mkcl_big_register_normalize(env, z);
    case mkcl_t_bignum:
      z = _mkcl_big_register0();
      _mkcl_big_sub(z, x, y);
      return _mkcl_big_register_normalize(env, z);
    case mkcl_t_ratio:
      z = mkcl_times(env, x, y->ratio.den);
      z = mkcl_minus(env, z, y->ratio.num);
      z = mkcl_make_ratio(env, z, y->ratio.den);
      return(z);
    case mkcl_t_singlefloat:
      return mkcl_make_singlefloat(env, mkcl_to_double(env, x) - mkcl_single_float(y));
    case mkcl_t_doublefloat:
      return mkcl_make_doublefloat(env, mkcl_to_double(env, x) - mkcl_double_float(y));
#ifdef MKCL_LONG_FLOAT
    case mkcl_t_longfloat:
      return mkcl_make_longfloat(env, mkcl_to_double(env, x) - mkcl_long_float(y));
#endif
    case mkcl_t_complex:
      goto COMPLEX;
    default:
      mkcl_FEtype_error_number(env, y);
    }
  case mkcl_t_ratio:
    switch (mkcl_type_of(y)) {
    case mkcl_t_fixnum:
    case mkcl_t_bignum:
      z = mkcl_times(env, x->ratio.den, y);
      z = mkcl_minus(env, x->ratio.num, z);
      z = mkcl_make_ratio(env, z, x->ratio.den);
      return(z);
    case mkcl_t_ratio:
      z = mkcl_times(env, x->ratio.num,y->ratio.den);
      z1 = mkcl_times(env, x->ratio.den,y->ratio.num);
      z = mkcl_minus(env, z, z1);
      z1 = mkcl_times(env, x->ratio.den,y->ratio.den);
      z = mkcl_make_ratio(env, z, z1);
      return(z);
    case mkcl_t_singlefloat:
      return mkcl_make_singlefloat(env, mkcl_to_double(env, x) - mkcl_single_float(y));
    case mkcl_t_doublefloat:
      return mkcl_make_doublefloat(env, mkcl_to_double(env, x) - mkcl_double_float(y));
#ifdef MKCL_LONG_FLOAT
    case mkcl_t_longfloat:
      return mkcl_make_longfloat(env, mkcl_to_double(env, x) - mkcl_long_float(y));
#endif
    case mkcl_t_complex:
      goto COMPLEX;
    default:
      mkcl_FEtype_error_number(env, y);
    }
  case mkcl_t_singlefloat:
    switch (mkcl_type_of(y)) {
    case mkcl_t_fixnum:
      return mkcl_make_singlefloat(env, mkcl_single_float(x) - mkcl_fixnum_to_word(y));
    case mkcl_t_bignum:
    case mkcl_t_ratio:
      return mkcl_make_singlefloat(env, mkcl_single_float(x) - mkcl_to_double(env, y));
    case mkcl_t_singlefloat:
      return mkcl_make_singlefloat(env, mkcl_single_float(x) - mkcl_single_float(y));
    case mkcl_t_doublefloat:
      return mkcl_make_doublefloat(env, mkcl_single_float(x) - mkcl_double_float(y));
#ifdef MKCL_LONG_FLOAT
    case mkcl_t_longfloat:
      return mkcl_make_longfloat(env, mkcl_single_float(x) - mkcl_long_float(y));
#endif
    case mkcl_t_complex:
      goto COMPLEX;
    default:
      mkcl_FEtype_error_number(env, y);
    }
  case mkcl_t_doublefloat:
    switch (mkcl_type_of(y)) {
    case mkcl_t_fixnum:
      return mkcl_make_doublefloat(env, mkcl_double_float(x) - mkcl_fixnum_to_word(y));
    case mkcl_t_bignum:
    case mkcl_t_ratio:
      return mkcl_make_doublefloat(env, mkcl_double_float(x) - mkcl_to_double(env, y));
    case mkcl_t_singlefloat:
      return mkcl_make_doublefloat(env, mkcl_double_float(x) - mkcl_single_float(y));
    case mkcl_t_doublefloat:
      return mkcl_make_doublefloat(env, mkcl_double_float(x) - mkcl_double_float(y));
#ifdef MKCL_LONG_FLOAT
    case mkcl_t_longfloat:
      return mkcl_make_longfloat(env, mkcl_double_float(x) - mkcl_long_float(y));
#endif
    case mkcl_t_complex:
      goto COMPLEX;
    default:
      mkcl_FEtype_error_number(env, y);
    }
#ifdef MKCL_LONG_FLOAT
  case mkcl_t_longfloat:
    switch (mkcl_type_of(y)) {
    case mkcl_t_fixnum:
      return mkcl_make_longfloat(env, mkcl_long_float(x) - mkcl_fixnum_to_word(y));
    case mkcl_t_bignum:
    case mkcl_t_ratio:
      return mkcl_make_longfloat(env, mkcl_long_float(x) - mkcl_to_double(env, y));
    case mkcl_t_singlefloat:
      return mkcl_make_longfloat(env, mkcl_long_float(x) - mkcl_single_float(y));
    case mkcl_t_doublefloat:
      return mkcl_make_longfloat(env, mkcl_long_float(x) - mkcl_double_float(y));
    case mkcl_t_longfloat:
      return mkcl_make_longfloat(env, mkcl_long_float(x) - mkcl_long_float(y));
    case mkcl_t_complex:
      goto COMPLEX;
    default:
      mkcl_FEtype_error_number(env, y);
    }
#endif /* def MKCL_LONG_FLOAT */
  COMPLEX:
    return mkcl_make_complex(env, mkcl_minus(env, x, y->_complex.real), mkcl_negate(env, y->_complex.imag));
  case mkcl_t_complex:
    if (mkcl_type_of(y) != mkcl_t_complex) {
      z = mkcl_minus(env, x->_complex.real, y);
      z1 = x->_complex.imag;
    } else {
      z = mkcl_minus(env, x->_complex.real, y->_complex.real);
      z1 = mkcl_minus(env, x->_complex.imag, y->_complex.imag);
    }
    return mkcl_make_complex(env, z, z1);
  default:
    mkcl_FEtype_error_number(env, x);
  }
}

mkcl_object
mk_cl_conjugate(MKCL, mkcl_object c)
{
  mkcl_call_stack_check(env);
  switch (mkcl_type_of(c)) {
  case mkcl_t_complex:
    c = mkcl_make_complex(env, c->_complex.real, mkcl_negate(env, c->_complex.imag));
  case mkcl_t_fixnum:
  case mkcl_t_bignum:
  case mkcl_t_ratio:
  case mkcl_t_singlefloat:
  case mkcl_t_doublefloat:
#ifdef MKCL_LONG_FLOAT
  case mkcl_t_longfloat:
#endif
    break;
  default:
    mkcl_FEtype_error_number(env, c);
  }
  @(return c);
}

mkcl_object
mkcl_negate(MKCL, mkcl_object x)
{
  mkcl_object z, z1;

  switch (mkcl_type_of(x)) {
  case mkcl_t_fixnum:
#if MKCL_LONG_BITS < MKCL_WORD_BITS
    return mkcl_make_int64_t(env, -mkcl_fixnum_to_word(x));
#else
    return mkcl_make_integer(env, -mkcl_fixnum_to_word(x));
#endif
  case mkcl_t_bignum:
    z = _mkcl_big_register0();
    _mkcl_big_complement(z, x);
    return _mkcl_big_register_normalize(env, z);

  case mkcl_t_ratio:
    z1 = mkcl_negate(env, x->ratio.num);
    z = mkcl_alloc_raw_ratio(env);
    z->ratio.num = z1;
    z->ratio.den = x->ratio.den;
    return(z);

  case mkcl_t_singlefloat:
    z = mkcl_alloc_raw_singlefloat(env);
    mkcl_single_float(z) = -mkcl_single_float(x);
    return(z);

  case mkcl_t_doublefloat:
    z = mkcl_alloc_raw_doublefloat(env);
    mkcl_double_float(z) = -mkcl_double_float(x);
    return(z);
#ifdef MKCL_LONG_FLOAT
  case mkcl_t_longfloat:
    return mkcl_make_longfloat(env, -mkcl_long_float(x));
#endif
  case mkcl_t_complex:
    z = mkcl_negate(env, x->_complex.real);
    z1 = mkcl_negate(env, x->_complex.imag);
    z = mkcl_make_complex(env, z, z1);
    return(z);

  default:
    mkcl_FEtype_error_number(env, x);
  }
}

/*  (/		)  */
@(defun / (num &rest nums)
@
  /* INV: type check is in mkcl_divide() */
  if (narg == 0)
    mkcl_FEwrong_num_arguments(env, @'/');
  if (narg == 1)
    { @(return mkcl_divide(env, MKCL_MAKE_FIXNUM(1), num)); }
  while (--narg)
    num = mkcl_divide(env, num, mkcl_va_arg(nums));
  @(return num);
@)

mkcl_object
mkcl_divide(MKCL, mkcl_object x, mkcl_object y)
{
  mkcl_object z, z1, z2;

  switch (mkcl_type_of(x)) {
  case mkcl_t_fixnum:
  case mkcl_t_bignum:
    switch (mkcl_type_of(y)) {
    case mkcl_t_fixnum:
      if (y == MKCL_MAKE_FIXNUM(0))
	mkcl_FEdivision_by_zero(env, x, y);
    case mkcl_t_bignum:
      if (mkcl_minusp(env, y) == TRUE) {
	x = mkcl_negate(env, x);
	y = mkcl_negate(env, y);
      }
      z = mkcl_make_ratio(env, x, y);
      return(z);
    case mkcl_t_ratio:
      z = mkcl_times(env, x, y->ratio.den);
      z = mkcl_make_ratio(env, z, y->ratio.num);
      return(z);
    case mkcl_t_singlefloat:
      return mkcl_make_singlefloat(env, mkcl_to_double(env, x) / mkcl_single_float(y));
    case mkcl_t_doublefloat:
      return mkcl_make_doublefloat(env, mkcl_to_double(env, x) / mkcl_double_float(y));
#ifdef MKCL_LONG_FLOAT
    case mkcl_t_longfloat:
      return mkcl_make_longfloat(env, mkcl_to_double(env, x) / mkcl_long_float(y));
#endif
    case mkcl_t_complex:
      goto COMPLEX;
    default:
      mkcl_FEtype_error_number(env, y);
    }
  case mkcl_t_ratio:
    switch (mkcl_type_of(y)) {
    case mkcl_t_fixnum:
      if (y == MKCL_MAKE_FIXNUM(0))
	mkcl_FEdivision_by_zero(env, x, y);
    case mkcl_t_bignum:
      z = mkcl_times(env, x->ratio.den, y);
      z = mkcl_make_ratio(env, x->ratio.num, z);
      return(z);
    case mkcl_t_ratio:
      z = mkcl_times(env, x->ratio.num,y->ratio.den);
      z1 = mkcl_times(env, x->ratio.den,y->ratio.num);
      z = mkcl_make_ratio(env, z, z1);
      return(z);
    case mkcl_t_singlefloat:
      return mkcl_make_singlefloat(env, mkcl_to_double(env, x) / mkcl_single_float(y));
    case mkcl_t_doublefloat:
      return mkcl_make_doublefloat(env, mkcl_to_double(env, x) / mkcl_double_float(y));
#ifdef MKCL_LONG_FLOAT
    case mkcl_t_longfloat:
      return mkcl_make_longfloat(env, mkcl_to_double(env, x) / mkcl_long_float(y));
#endif
    case mkcl_t_complex:
      goto COMPLEX;
    default:
      mkcl_FEtype_error_number(env, y);
    }
  case mkcl_t_singlefloat:
    switch (mkcl_type_of(y)) {
    case mkcl_t_fixnum:
      return mkcl_make_singlefloat(env, mkcl_single_float(x) / mkcl_fixnum_to_word(y));
    case mkcl_t_bignum:
    case mkcl_t_ratio:
      return mkcl_make_singlefloat(env, mkcl_single_float(x) / mkcl_to_double(env, y));
    case mkcl_t_singlefloat:
      return mkcl_make_singlefloat(env, mkcl_single_float(x) / mkcl_single_float(y));
    case mkcl_t_doublefloat:
      return mkcl_make_doublefloat(env, mkcl_single_float(x) / mkcl_double_float(y));
#ifdef MKCL_LONG_FLOAT
    case mkcl_t_longfloat:
      return mkcl_make_longfloat(env, mkcl_single_float(x) / mkcl_long_float(y));
#endif
    case mkcl_t_complex:
      goto COMPLEX;
    default:
      mkcl_FEtype_error_number(env, y);
    }
  case mkcl_t_doublefloat:
    switch (mkcl_type_of(y)) {
    case mkcl_t_fixnum:
      return mkcl_make_doublefloat(env, mkcl_double_float(x) / mkcl_fixnum_to_word(y));
    case mkcl_t_bignum:
    case mkcl_t_ratio:
      return mkcl_make_doublefloat(env, mkcl_double_float(x) / mkcl_to_double(env, y));
    case mkcl_t_singlefloat:
      return mkcl_make_doublefloat(env, mkcl_double_float(x) / mkcl_single_float(y));
    case mkcl_t_doublefloat:
      return mkcl_make_doublefloat(env, mkcl_double_float(x) / mkcl_double_float(y));
#ifdef MKCL_LONG_FLOAT
    case mkcl_t_longfloat:
      return mkcl_make_longfloat(env, mkcl_double_float(x) / mkcl_long_float(y));
#endif
    case mkcl_t_complex:
      goto COMPLEX;
    default:
      mkcl_FEtype_error_number(env, y);
    }
#ifdef MKCL_LONG_FLOAT
  case mkcl_t_longfloat:
    switch (mkcl_type_of(y)) {
    case mkcl_t_fixnum:
      return mkcl_make_longfloat(env, mkcl_long_float(x) / mkcl_fixnum_to_word(y));
    case mkcl_t_bignum:
    case mkcl_t_ratio:
      return mkcl_make_longfloat(env, mkcl_long_float(x) / mkcl_to_double(env, y));
    case mkcl_t_singlefloat:
      return mkcl_make_longfloat(env, mkcl_long_float(x) / mkcl_single_float(y));
    case mkcl_t_doublefloat:
      return mkcl_make_longfloat(env, mkcl_long_float(x) / mkcl_double_float(y));
    case mkcl_t_longfloat:
      return mkcl_make_longfloat(env, mkcl_long_float(x) / mkcl_long_float(y));
    case mkcl_t_complex:
      goto COMPLEX;
    default:
      mkcl_FEtype_error_number(env, y);
    }
#endif /* def MKCL_LONG_FLOAT */
  case mkcl_t_complex:
    if (mkcl_type_of(y) != mkcl_t_complex) {
      z1 = mkcl_divide(env, x->_complex.real, y);
      z2 = mkcl_divide(env, x->_complex.imag, y);
      return mkcl_make_complex(env, z1, z2);
    } else if (1) {
      /* #C(z1 z2) = #C(xr xi) * #C(yr -yi) */
      z1 = mkcl_plus(env, mkcl_times(env, x->_complex.real, y->_complex.real),
		    mkcl_times(env, x->_complex.imag, y->_complex.imag));
      z2 = mkcl_minus(env, mkcl_times(env, x->_complex.imag, y->_complex.real),
		     mkcl_times(env, x->_complex.real, y->_complex.imag));
    } else {
    COMPLEX: /* INV: x is real, y is complex */
      /* #C(z1 z2) = x * #C(yr -yi) */
      z1 = mkcl_times(env, x, y->_complex.real);
      z2 = mkcl_negate(env, mkcl_times(env, x, y->_complex.imag));
    }
    z  = mkcl_plus(env, mkcl_times(env, y->_complex.real, y->_complex.real),
		  mkcl_times(env, y->_complex.imag, y->_complex.imag));
    z  = mkcl_make_complex(env, mkcl_divide(env, z1, z), mkcl_divide(env, z2, z));
    return(z);
  default:
    mkcl_FEtype_error_number(env, x);
  }
}

mkcl_object
mkcl_integer_divide(MKCL, mkcl_object x, mkcl_object y)
{
  mkcl_type tx, ty;

  tx = mkcl_type_of(x);
  ty = mkcl_type_of(y);
  if (tx == mkcl_t_fixnum) {
    if (ty == mkcl_t_fixnum) {
      if (y == MKCL_MAKE_FIXNUM(0))
	mkcl_FEdivision_by_zero(env, x, y);
      return MKCL_MAKE_FIXNUM(mkcl_fixnum_to_word(x) / mkcl_fixnum_to_word(y));
    }
    if (ty == mkcl_t_bignum) {
#if MKCL_LONG_BITS < MKCL_WORD_BITS
      mkcl_object x_big = _mkcl_big_register0();
      mkcl_object q = _mkcl_big_register1();
      _mkcl_big_set_fixnum(x_big, mkcl_fixnum_to_word(x));
      _mkcl_big_tdiv_q(q, x, y);
      return _mkcl_big_register_normalize(env, q);
#else /* MKCL_LONG_BITS < MKCL_WORD_BITS */
      /* The only number "x" which can be a bignum and be
       * as large as "-x" is -MKCL_MOST_NEGATIVE_FIXNUM. However
       * in newer versions of MKCL we will probably choose
       * MKCL_MOST_NEGATIVE_FIXNUM = - MKCL_MOST_POSITIVE_FIXNUM.
       */
      if (-MKCL_MOST_NEGATIVE_FIXNUM > MKCL_MOST_POSITIVE_FIXNUM) {
	if (_mkcl_big_cmp_si(y, -mkcl_fixnum_to_word(x)))
	  return MKCL_MAKE_FIXNUM(0);
	else
	  return MKCL_MAKE_FIXNUM(-1);
      } else {
	return MKCL_MAKE_FIXNUM(0);
      }
#endif /* MKCL_LONG_BITS < MKCL_WORD_BITS */
    }
    mkcl_FEtype_error_integer(env, y);
  }
  if (tx == mkcl_t_bignum) {
    mkcl_object q = _mkcl_big_register0();
    if (ty == mkcl_t_bignum) {
      _mkcl_big_tdiv_q(q, x, y);
    } else if (ty == mkcl_t_fixnum) {
#if MKCL_LONG_BITS < MKCL_WORD_BITS
      mkcl_object y_big = _mkcl_big_register1();
      _mkcl_big_set_fixnum(y_big, mkcl_fixnum_to_word(y));
      _mkcl_big_tdiv_q(q, x, y_big);
#else /* MKCL_LONG_BITS < MKCL_WORD_BITS */
      long j = mkcl_fixnum_to_word(y);
      _mkcl_big_tdiv_q_ui(q, x, labs(j));
      if (j < 0)
	_mkcl_big_complement(q, q);
#endif /* MKCL_LONG_BITS < MKCL_WORD_BITS */
    } else {
      mkcl_FEtype_error_integer(env, y);
    }
    return _mkcl_big_register_normalize(env, q);
  }
  mkcl_FEtype_error_integer(env, x);
}

@(defun gcd (&rest nums)
	mkcl_object gcd;
@
  if (narg == 0)
  @(return MKCL_MAKE_FIXNUM(0))
  /* INV: mkcl_gcd() checks types */
  gcd = mkcl_va_arg(nums);
  if (narg == 1) {
    mkcl_assert_type_integer(env, gcd);
    @(return (mkcl_minusp(env, gcd) ? mkcl_negate(env, gcd) : gcd));
  }
  while (--narg)
    gcd = mkcl_gcd(env, gcd, mkcl_va_arg(nums));
  @(return gcd);
@)

mkcl_object
mkcl_gcd(MKCL, mkcl_object x, mkcl_object y)
{
  mkcl_object gcd, x_big, y_big;

  switch (mkcl_type_of(x)) {
  case mkcl_t_fixnum:
    x_big = _mkcl_big_register0();
    _mkcl_big_set_fixnum(x_big, mkcl_fixnum_to_word(x));
    break;
  case mkcl_t_bignum:
    x_big = x;
    break;
  default:
    mkcl_FEtype_error_integer(env, x);
  }
  switch (mkcl_type_of(y)) {
  case mkcl_t_fixnum:
    y_big = _mkcl_big_register1();
    _mkcl_big_set_fixnum(y_big, mkcl_fixnum_to_word(y));
    break;
  case mkcl_t_bignum:
    y_big = y;
    break;
  default:
    mkcl_FEtype_error_integer(env, y);
  }
  gcd = _mkcl_big_register2();
  _mkcl_big_gcd(gcd, x_big, y_big);
  if (x != x_big) _mkcl_big_register_free(env, x_big);
  if (y != y_big) _mkcl_big_register_free(env, y_big);
  return _mkcl_big_register_normalize(env, gcd);
}

/*  (1+ x)  */
mkcl_object
@1+(MKCL, mkcl_object x)
{
  /* INV: type check is in mkcl_one_plus() */
  @(return mkcl_one_plus(env, x));
}


mkcl_object
mkcl_one_plus(MKCL, mkcl_object x)
{
  mkcl_object z;

  switch (mkcl_type_of(x)) {

  case mkcl_t_fixnum:
    if (x == MKCL_MAKE_FIXNUM(MKCL_MOST_POSITIVE_FIXNUM))
      return(mkcl_make_integer(env, MKCL_MOST_POSITIVE_FIXNUM+1));
    return (mkcl_object)((mkcl_word)x + ((mkcl_word)MKCL_MAKE_FIXNUM(1) - MKCL_FIXNUM_TAG));
  case mkcl_t_bignum:
    return(mkcl_plus(env, x, MKCL_MAKE_FIXNUM(1)));

  case mkcl_t_ratio:
    z = mkcl_plus(env, x->ratio.num, x->ratio.den);
    z = mkcl_make_ratio(env, z, x->ratio.den);
    return(z);

  case mkcl_t_singlefloat:
    z = mkcl_alloc_raw_singlefloat(env);
    mkcl_single_float(z) = mkcl_single_float(x) + 1.0F;
    return(z);

  case mkcl_t_doublefloat:
    z = mkcl_alloc_raw_doublefloat(env);
    mkcl_double_float(z) = mkcl_double_float(x) + 1.0;
    return(z);

#ifdef MKCL_LONG_FLOAT
  case mkcl_t_longfloat:
    return mkcl_make_longfloat(env, 1.0L + mkcl_long_float(x));
#endif

  case mkcl_t_complex:
    z = mkcl_one_plus(env, x->_complex.real);
    z = mkcl_make_complex(env, z, x->_complex.imag);
    return(z);

  default:
    mkcl_FEtype_error_number(env, x);
  }
}

/*  (1-	x)  */
mkcl_object
@1-(MKCL, mkcl_object x)
{	/* INV: type check is in mkcl_one_minus() */
  @(return mkcl_one_minus(env, x));
}

mkcl_object
mkcl_one_minus(MKCL, mkcl_object x)
{
  mkcl_object z;
	
  switch (mkcl_type_of(x)) {

  case mkcl_t_fixnum:
    if (x == MKCL_MAKE_FIXNUM(MKCL_MOST_NEGATIVE_FIXNUM))
      return(mkcl_make_integer(env, MKCL_MOST_NEGATIVE_FIXNUM-1));
    return (mkcl_object)((mkcl_word)x - ((mkcl_word)MKCL_MAKE_FIXNUM(1) - MKCL_FIXNUM_TAG));

  case mkcl_t_bignum:
    return(mkcl_minus(env, x, MKCL_MAKE_FIXNUM(1)));

  case mkcl_t_ratio:
    z = mkcl_minus(env, x->ratio.num, x->ratio.den);
    z = mkcl_make_ratio(env, z, x->ratio.den);
    return(z);

  case mkcl_t_singlefloat:
    z = mkcl_alloc_raw_singlefloat(env);
    mkcl_single_float(z) = mkcl_single_float(x) - 1.0F;
    return(z);

  case mkcl_t_doublefloat:
    z = mkcl_alloc_raw_doublefloat(env);
    mkcl_double_float(z) = mkcl_double_float(x) - 1.0;
    return(z);

#ifdef MKCL_LONG_FLOAT
  case mkcl_t_longfloat:
    return mkcl_make_longfloat(env, mkcl_long_float(x) - 1.0L);
#endif

  case mkcl_t_complex:
    z = mkcl_one_minus(env, x->_complex.real);
    z = mkcl_make_complex(env, z, x->_complex.imag);
    return(z);

  default:
    mkcl_FEtype_error_real(env, x);
  }
}

@(defun lcm (&rest nums)
	mkcl_object lcm;
@
  if (narg == 0)
    { @(return MKCL_MAKE_FIXNUM(1)); }
  /* INV: mkcl_gcd() checks types. By placing `numi' before `lcm' in
     this call, we make sure that errors point to `numi' */
  lcm = mkcl_va_arg(nums);
  mkcl_assert_type_integer(env, lcm);
  while (narg-- > 1) {
    mkcl_object numi = mkcl_va_arg(nums);
    mkcl_object t = mkcl_times(env, lcm, numi);
    mkcl_object g = mkcl_gcd(env, numi, lcm);
    if (g != MKCL_MAKE_FIXNUM(0))
      lcm = mkcl_divide(env, t, g);
  }
  @(return (mkcl_minusp(env, lcm) ? mkcl_negate(env, lcm) : lcm));
@)
