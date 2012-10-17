/* -*- mode: c -*- */
/*
    num_comp.c  -- Comparisons on numbers.
*/
/*
    Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
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

/*
 * In Common Lisp, comparisons between floats and integers are performed
 * via an intermediate rationalization of the floating point number. In C,
 * on the other hand, the comparison is performed by converting the integer
 * into a floating point number. However, if the double type is too small
 * this may lead to a loss of precision and two numbers being told equal
 * when, by Common Lisp standards, would not.
 */
static int
double_fix_compare(mkcl_word n, double d)
{
  if ((double)n < d) {
    return -1;
  } else if ((double)n > d) {
    return +1;
  } else if (sizeof(double) > sizeof(mkcl_word)) {
    return 0;
  } else {
    /* When we reach here, the double type has no
     * significant decimal part. However, as explained
     * above, the double type is too small and integers
     * may coerce to the same double number giving a false
     * positive. Hence we perform the comparison in
     * integer space. */
    mkcl_word m = d;
    if (n == m) {
      return 0;
    } else if (n > m) {
      return +1;
    } else {
      return -1;
    }
  }
}

#ifdef MKCL_LONG_FLOAT
static int
long_double_fix_compare(mkcl_word n, long double d)
{
  if ((long double)n < d) {
    return -1;
  } else if ((long double)n > d) {
    return +1;
  } else if (sizeof(long double) > sizeof(mkcl_word)) {
    return 0;
  } else {
    mkcl_word m = d;
    if (n == m) {
      return 0;
    } else if (n > m) {
      return +1;
    } else {
      return -1;
    }
  }
}
#endif

@(defun = (num &rest nums)
	int i;
@
  /* ANSI: Need not signal error for 1 argument */
  /* INV: For >= 2 arguments, mkcl_number_equalp() performs checks */
  for (i = 1; i < narg; i++)
    if (!mkcl_number_equalp(env, num, mkcl_va_arg(nums)))
      { @(return mk_cl_Cnil); }
  @(return mk_cl_Ct);
@)

/* Returns 1 if both numbers compare to equal */
int
mkcl_number_equalp(MKCL, mkcl_object x, mkcl_object y)
{
  double dx;
  /* INV: (= fixnum bignum) => 0 */
  /* INV: (= fixnum ratio) => 0 */
  /* INV: (= bignum ratio) => 0 */
 BEGIN:
  switch (mkcl_type_of(x)) {
  case mkcl_t_fixnum:
    switch (mkcl_type_of(y)) {
    case mkcl_t_fixnum:
      return x == y;
    case mkcl_t_bignum:
    case mkcl_t_ratio:
      return 0;
    case mkcl_t_singlefloat:
      return double_fix_compare(mkcl_fixnum_to_word(x), mkcl_single_float(y)) == 0;
    case mkcl_t_doublefloat:
      return double_fix_compare(mkcl_fixnum_to_word(x), mkcl_double_float(y)) == 0;
#ifdef MKCL_LONG_FLOAT
    case mkcl_t_longfloat:
      return long_double_fix_compare(mkcl_fixnum_to_word(x), mkcl_long_float(y)) == 0;
#endif
    case mkcl_t_complex:
      goto Y_COMPLEX;
    default:
      mkcl_FEtype_error_number(env, y);
    }
  case mkcl_t_bignum:
    switch (mkcl_type_of(y)) {
    case mkcl_t_fixnum:
      return 0;
    case mkcl_t_bignum:
      return _mkcl_big_compare(x, y)==0;
    case mkcl_t_ratio:
      return 0;
    case mkcl_t_singlefloat:
    case mkcl_t_doublefloat:
#ifdef MKCL_LONG_FLOAT
    case mkcl_t_longfloat:
#endif
      y = mk_cl_rational(env, y);
      goto BEGIN;
    case mkcl_t_complex:
      goto Y_COMPLEX;
    default:
      mkcl_FEtype_error_number(env, y);
    }
  case mkcl_t_ratio:
    switch (mkcl_type_of(y)) {
    case mkcl_t_fixnum:
    case mkcl_t_bignum:
      return 0;
    case mkcl_t_ratio:
      return (mkcl_number_equalp(env, x->ratio.num, y->ratio.num) &&
	      mkcl_number_equalp(env, x->ratio.den, y->ratio.den));
    case mkcl_t_singlefloat:
    case mkcl_t_doublefloat:
#ifdef MKCL_LONG_FLOAT
    case mkcl_t_longfloat:
#endif
      y = mk_cl_rational(env, y);
      goto BEGIN;
    case mkcl_t_complex:
      goto Y_COMPLEX;
    default:
      mkcl_FEtype_error_number(env, y);
    }
  case mkcl_t_singlefloat:
    dx = mkcl_single_float(x);
    goto FLOAT;
  case mkcl_t_doublefloat:
    dx = mkcl_double_float(x);
  FLOAT:
    switch (mkcl_type_of(y)) {
    case mkcl_t_fixnum:
      return double_fix_compare(mkcl_fixnum_to_word(y), dx) == 0;
    case mkcl_t_bignum:
    case mkcl_t_ratio:
      x = mk_cl_rational(env, x);
      goto BEGIN;
    case mkcl_t_singlefloat:
      return dx == mkcl_single_float(y);
    case mkcl_t_doublefloat:
      return dx == mkcl_double_float(y);
#ifdef MKCL_LONG_FLOAT
    case mkcl_t_longfloat:
      return dx == mkcl_long_float(y);
#endif
    case mkcl_t_complex:
      goto Y_COMPLEX;
    default:
      mkcl_FEtype_error_number(env, y);
    }
#ifdef MKCL_LONG_FLOAT
  case mkcl_t_longfloat: {
    long double ldx = mkcl_long_float(x);
    switch (mkcl_type_of(y)) {
    case mkcl_t_fixnum:
      return long_double_fix_compare(mkcl_fixnum_to_word(y), ldx) == 0;
    case mkcl_t_bignum:
    case mkcl_t_ratio:
      x = mk_cl_rational(env, x);
      goto BEGIN;
    case mkcl_t_singlefloat:
      return ldx == mkcl_single_float(y);
    case mkcl_t_doublefloat:
      return ldx == mkcl_double_float(y);
    case mkcl_t_longfloat:
      return ldx == mkcl_long_float(y);
    case mkcl_t_complex:
      goto Y_COMPLEX;
    default:
      mkcl_FEtype_error_number(env, y);
    }
  }
#endif
  Y_COMPLEX:
    if (!mkcl_zerop(env, y->_complex.imag))
      return 0;
    return mkcl_number_equalp(env, x, y->_complex.real);
  case mkcl_t_complex:
    if (mkcl_type_of(y) == mkcl_t_complex)
      return (mkcl_number_equalp(env, x->_complex.real, y->_complex.real) &&
	      mkcl_number_equalp(env, x->_complex.imag, y->_complex.imag));
    if (MKCL_REAL_TYPE_P(mkcl_type_of(y))) {
      if (mkcl_zerop(env, x->_complex.imag))
	return mkcl_number_equalp(env, x->_complex.real, y) != 0;
      else
	return 0;
    }
    mkcl_FEtype_error_number(env, y);
  default:
    mkcl_FEtype_error_number(env, x);
  }
}

/*
	The value of mkcl_number_compare(x, y) is

		-1	if	x < y
		0	if	x = y
		1	if	x > y.

	If x or y is not real, it fails.
*/
int
mkcl_number_compare(MKCL, mkcl_object x, mkcl_object y)
{
  mkcl_word ix, iy;
  double dx, dy;
#ifdef MKCL_LONG_FLOAT
  long double ldx, ldy;
#endif
  mkcl_type ty;

 BEGIN:
  ty = mkcl_type_of(y);
  switch (mkcl_type_of(x)) {
  case mkcl_t_fixnum:
    ix = mkcl_fixnum_to_word(x);
    switch (ty) {
    case mkcl_t_fixnum:
      iy = mkcl_fixnum_to_word(y);
      if (ix < iy)
	return(-1);
      else return(ix != iy);
    case mkcl_t_bignum:
      /* INV: (= x y) can't be zero since fixnum != bignum */
      return _mkcl_big_sign(y) < 0? 1 : -1;
    case mkcl_t_ratio:
      x = mkcl_times(env, x, y->ratio.den);
      y = y->ratio.num;
      return(mkcl_number_compare(env, x, y));
    case mkcl_t_singlefloat:
      return double_fix_compare(ix, mkcl_single_float(y));
    case mkcl_t_doublefloat:
      return double_fix_compare(ix, mkcl_double_float(y));
#ifdef MKCL_LONG_FLOAT
    case mkcl_t_longfloat:
      return long_double_fix_compare(ix, mkcl_long_float(y));
#endif
    default:
      mkcl_FEtype_error_real(env, y);
    }
  case mkcl_t_bignum:
    switch (ty) {
    case mkcl_t_fixnum:
      return _mkcl_big_sign(x) < 0 ? -1 : 1;
    case mkcl_t_bignum:
      return(_mkcl_big_compare(x, y));
    case mkcl_t_ratio:
      x = mkcl_times(env, x, y->ratio.den);
      y = y->ratio.num;
      return(mkcl_number_compare(env, x, y));
    case mkcl_t_singlefloat:
    case mkcl_t_doublefloat:
#ifdef MKCL_LONG_FLOAT
    case mkcl_t_longfloat:
#endif
      y = mk_cl_rational(env, y);
      goto BEGIN;
    default:
      mkcl_FEtype_error_real(env, y);
    }
  case mkcl_t_ratio:
    switch (ty) {
    case mkcl_t_fixnum:
    case mkcl_t_bignum:
      y = mkcl_times(env, y, x->ratio.den);
      x = x->ratio.num;
      return(mkcl_number_compare(env, x, y));
    case mkcl_t_ratio:
      return(mkcl_number_compare(env, 
				mkcl_times(env, x->ratio.num,
					  y->ratio.den),
				mkcl_times(env, y->ratio.num,
					  x->ratio.den)));
    case mkcl_t_singlefloat:
    case mkcl_t_doublefloat:
#ifdef MKCL_LONG_FLOAT
    case mkcl_t_longfloat:
#endif
      y = mk_cl_rational(env, y);
      goto BEGIN;
    default:
      mkcl_FEtype_error_real(env, y);
    }
  case mkcl_t_singlefloat:
    dx = (double)(mkcl_single_float(x));
    goto DOUBLEFLOAT0;
  case mkcl_t_doublefloat:
    dx = mkcl_double_float(x);
  DOUBLEFLOAT0:
    switch (ty) {
    case mkcl_t_fixnum:
      return -double_fix_compare(mkcl_fixnum_to_word(y), dx);
    case mkcl_t_bignum:
    case mkcl_t_ratio:
      x = mk_cl_rational(env, x);
      goto BEGIN;
    case mkcl_t_singlefloat:
      dy = (double)(mkcl_single_float(y));
      break;
    case mkcl_t_doublefloat:
      dy = mkcl_double_float(y);
      break;
#ifdef MKCL_LONG_FLOAT
    case mkcl_t_longfloat:
      ldx = dx;
      ldy = mkcl_long_float(y);
      goto LONGFLOAT;
#endif
    default:
      mkcl_FEtype_error_real(env, y);
    }
  /* DOUBLEFLOAT: */
    if (dx == dy)
      return(0);
    else if (dx < dy)
      return(-1);
    else
      return(1);
#ifdef MKCL_LONG_FLOAT
  case mkcl_t_longfloat:
    ldx = mkcl_long_float(x);
    switch (ty) {
    case mkcl_t_fixnum:
      return -long_double_fix_compare(mkcl_fixnum_to_word(y), ldx);
    case mkcl_t_bignum:
    case mkcl_t_ratio:
      x = mk_cl_rational(env, x);
      goto BEGIN;
    case mkcl_t_singlefloat:
      ldy = mkcl_single_float(y);
      break;
    case mkcl_t_doublefloat:
      ldy = mkcl_double_float(y);
      break;
    case mkcl_t_longfloat:
      ldy = mkcl_long_float(y);
      break;
    default:
      mkcl_FEtype_error_real(env, y);
    }
  LONGFLOAT:
    if (ldx == ldy)
      return 0;
    else if (ldx < ldy)
      return -1;
    else
      return 1;
    break;
#endif
  default:
    mkcl_FEtype_error_real(env, x);
  }
}

@(defun /= (&rest nums &aux numi)
  int i, j;
@
  if (narg == 0)
    mkcl_FEwrong_num_arguments_anonym(env);
  numi = mkcl_va_arg(nums);
  for (i = 2; i<=narg; i++) {
    mkcl_va_list numb;
    mkcl_va_start(env, numb, narg, narg, 0);
    numi = mkcl_va_arg(nums);
    for (j = 1; j<i; j++)
      if (mkcl_number_equalp(env, numi, mkcl_va_arg(numb)))
	{ @(return mk_cl_Cnil); }
  }
  @(return mk_cl_Ct);
@)

static mkcl_object
monotonic(MKCL, int s, int t, int narg, mkcl_va_list nums)
{
  mkcl_object c, d;
  
  if (narg == 0)
    mkcl_FEwrong_num_arguments_anonym(env);
  /* INV: type check occurs in mkcl_number_compare() */
  for (c = mkcl_va_arg(nums); --narg; c = d) {
    d = mkcl_va_arg(nums);
    if (s*mkcl_number_compare(env, d, c) < t)
      { mkcl_return1(mk_cl_Cnil); }
  }
  mkcl_return1(mk_cl_Ct);
}

#define MONOTONIC(i, j) (MKCL, mkcl_narg narg, ...) \
  { mkcl_va_list nums; mkcl_va_start(env, nums, narg, narg, 0);	\
    return monotonic(env, i, j, narg, nums); }

mkcl_object @<= MONOTONIC( 1, 0)
mkcl_object @>= MONOTONIC(-1, 0)
mkcl_object @<  MONOTONIC( 1, 1)
mkcl_object @>  MONOTONIC(-1, 1)

@(defun max (max &rest nums)
@
  /* INV: type check occurs in mkcl_number_compare() for the rest of
     numbers, but for the first argument it happens in mkcl_zerop(). */
  if (narg-- == 1) {
    mkcl_zerop(env, max);
  } else do {
    mkcl_object numi = mkcl_va_arg(nums);
    if (mkcl_number_compare(env, max, numi) < 0)
      max = numi;
  } while (--narg);
  @(return max);
@)

@(defun min (min &rest nums)
@
  /* INV: type check occurs in mkcl_number_compare() for the rest of
     numbers, but for the first argument it happens in mkcl_zerop(). */
  if (narg-- == 1) {
    mkcl_zerop(env, min);
  } else do {
    mkcl_object numi = mkcl_va_arg(nums);
    if (mkcl_number_compare(env, min, numi) > 0)
      min = numi;
  } while (--narg);
  @(return min);
@)

