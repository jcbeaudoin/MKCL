/* -*- mode: c -*- */
/*
    num_co.c -- Operations on floating-point numbers.
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

/*
	IMPLEMENTATION-DEPENDENT

	This file contains those functions
	that know the representation of floating-point numbers.
*/

#include <mkcl/mkcl.h>
#include <mkcl/mkcl-math.h>

#include <mkcl/internal.h>

#include <float.h>

#ifndef HAVE_ISOC99
# define floorf floor
# define ceilf ceil
# define fabsf fabs
# define frexpf frexp
# define ldexpf ldexp
# define cosf cos
# define coshf cosh
# define expf exp
# define logf log
# define sinf sin
# define sqrtf sqrt
# define tanf tan
# define tanhf tanh
#endif

static mkcl_object
number_remainder(MKCL, mkcl_object x, mkcl_object y, mkcl_object q)
{
  mkcl_object z;

  z = mkcl_times(env, q, y);
  z = mkcl_minus(env, x, z);
  return(z);
}

/* Coerce non-float X to single-float if one arg,
   otherwise coerce to same float type as second arg */

@(defun float (x &optional (y MKCL_OBJNULL))
  mkcl_type ty, tx;
@
  AGAIN:
  if (y != MKCL_OBJNULL) {
    ty = mkcl_type_of(y);
  } else {
    ty = mkcl_t_singlefloat;
  }
  switch (tx = mkcl_type_of(x)) {
  case mkcl_t_singlefloat:
  case mkcl_t_doublefloat:
#ifdef MKCL_LONG_FLOAT
  case mkcl_t_longfloat:
#endif
    if (y == MKCL_OBJNULL || ty == tx)
      break;
  case mkcl_t_fixnum:
  case mkcl_t_bignum:
  case mkcl_t_ratio:
    switch (ty) {
    case mkcl_t_singlefloat:
      x = mkcl_make_singlefloat(env, mkcl_to_float(env, x));
      /* x = mkcl_make_singlefloat(env, mkcl_to_double(env, x)); */
      break;
    case mkcl_t_doublefloat:
      x = mkcl_make_doublefloat(env, mkcl_to_double(env, x));
      break;
#ifdef MKCL_LONG_FLOAT
    case mkcl_t_longfloat:
      x = mkcl_make_longfloat(env, mkcl_to_long_double(env, x));
      break;
#endif
    default:
      y = mkcl_type_error(env, @'float',"prototype",y,@'float');
      goto AGAIN;
    }
    break;
  default:
    x = mkcl_type_error(env, @'float',"argument",x,@'real');
    goto AGAIN;
  }
  mkcl_return_value(x);
@)

mkcl_object
mk_cl_numerator(MKCL, mkcl_object x)
{
  mkcl_call_stack_check(env);
 AGAIN:
  switch (mkcl_type_of(x)) {
  case mkcl_t_ratio:
    x = x->ratio.num;
    break;
  case mkcl_t_fixnum:
  case mkcl_t_bignum:
    break;
  default:
    x = mkcl_type_error(env, @'numerator',"argument",x,@'rational');
    goto AGAIN;
  }
  mkcl_return_value(x);
}

mkcl_object
mk_cl_denominator(MKCL, mkcl_object x)
{
  mkcl_call_stack_check(env);
 AGAIN:
  switch (mkcl_type_of(x)) {
  case mkcl_t_ratio:
    x = x->ratio.den;
    break;
  case mkcl_t_fixnum:
  case mkcl_t_bignum:
    x = MKCL_MAKE_FIXNUM(1);
    break;
  default:
    x = mkcl_type_error(env, @'numerator',"argument",x,@'rational');
    goto AGAIN;
  }
  mkcl_return_value(x);
}

mkcl_object
mkcl_floor1(MKCL, mkcl_object x)
{
  mkcl_object v0, v1;
 AGAIN:
  switch (mkcl_type_of(x)) {
  case mkcl_t_fixnum:
  case mkcl_t_bignum:
    v0 = x;
    v1 = MKCL_MAKE_FIXNUM(0);
    break;
  case mkcl_t_ratio:
    v0 = mkcl_floor2(env, x->ratio.num, x->ratio.den);
    v1 = mkcl_make_ratio(env, MKCL_VALUES(1), x->ratio.den);
    break;
  case mkcl_t_singlefloat: {
    float d = mkcl_single_float(x);
    float y = floorf(d);
    v0 = mkcl_float_to_integer(env, y);
    v1 = mkcl_make_singlefloat(env, d - y);
    break;
  }
  case mkcl_t_doublefloat: {
    double d = mkcl_double_float(x);
    double y = floor(d);
    v0 = mkcl_double_to_integer(env, y);
    v1 = mkcl_make_doublefloat(env, d - y);
    break;
  }
#ifdef MKCL_LONG_FLOAT
  case mkcl_t_longfloat: {
    long double d = mkcl_long_float(x);
    long double y = floorl(d);
    v0 = mkcl_long_double_to_integer(env, y);
    v1 = mkcl_make_longfloat(env, d - y);
    break;
  }
#endif
  default:
    x = mkcl_type_error(env, @'floor', "argument", x, @'real');
    goto AGAIN;
  }
  mkcl_return_2_values(v0, v1);
}

mkcl_object
mkcl_floor2(MKCL, mkcl_object x, mkcl_object y)
{
  mkcl_object v0, v1;
  mkcl_type ty;
 AGAIN:
  while ((ty = mkcl_type_of(y), !MKCL_NUMBER_TYPE_P(ty))) {
    y = mkcl_type_error(env, @'floor',"divisor",y,@'real');
  }
  switch(mkcl_type_of(x)) {
  case mkcl_t_fixnum:
    switch(ty) {
    case mkcl_t_fixnum: {	/* FIX / FIX */
      mkcl_word a = mkcl_fixnum_to_word(x), b = mkcl_fixnum_to_word(y);
      mkcl_word q = a / b,  r = a % b;
      if ((r^b) < 0 && r) {	/* opposite sign and some remainder*/
	v0 = MKCL_MAKE_FIXNUM(q-1);
	v1 = MKCL_MAKE_FIXNUM(r+b);
      } else {
	v0 = MKCL_MAKE_FIXNUM(q);
	v1 = MKCL_MAKE_FIXNUM(r);
      }
      break;
    }
    case mkcl_t_bignum: {	/* FIX / BIG */
      /* We must perform the division because there is the
       * pathological case
       *	x = MKCL_MOST_NEGATIVE_FIXNUM
       *    y = - MKCL_MOST_NEGATIVE_FIXNUM
       */
      mkcl_object q = _mkcl_big_register0();
      mkcl_object r = _mkcl_big_register1();
      mkcl_object j = _mkcl_big_register2();
#if MKCL_LONG_BITS < MKCL_WORD_BITS
      _mkcl_big_set_fixnum(j, mkcl_fixnum_to_word(x));
#else
      mpz_set_si(j->big.big_num, mkcl_fixnum_to_word(x));
#endif
      mpz_fdiv_qr(q->big.big_num, r->big.big_num, j->big.big_num, y->big.big_num);
      v0 = _mkcl_big_register_normalize(env, q);
      v1 = _mkcl_big_register_normalize(env, r);
      break;
    }
    case mkcl_t_ratio:		/* FIX / RAT */
      v0 = mkcl_floor2(env, mkcl_times(env, x, y->ratio.den), y->ratio.num);
      v1 = mkcl_make_ratio(env, MKCL_VALUES(1), y->ratio.den);
      break;
    case mkcl_t_singlefloat: {	/* FIX / SF */
      float n = mkcl_single_float(y);
      float p = mkcl_fixnum_to_word(x) / n;
      float q = floorf(p);
      v0 = mkcl_float_to_integer(env, q);
      v1 = mkcl_make_singlefloat(env, (p - q)*n);
      break;
    }
    case mkcl_t_doublefloat: {	/* FIX / DF */
      double n = mkcl_double_float(y);
      double p = mkcl_fixnum_to_word(x) / n;
      double q = floor(p);
      v0 = mkcl_double_to_integer(env, q);
      v1 = mkcl_make_doublefloat(env, (p - q)*n);
      break;
    }
#ifdef MKCL_LONG_FLOAT
    case mkcl_t_longfloat: {	/* FIX / LF */
      long double n = mkcl_long_float(y);
      long double p = mkcl_fixnum_to_word(x) / n;
      long double q = floorl(p);
      v0 = mkcl_long_double_to_integer(env, q);
      v1 = mkcl_make_longfloat(env, (p - q)*n);
      break;
    }
#endif
    default:
      v0 = v1 = mk_cl_Cnil;
      (void)0; /* Never reached */
    }
    break;
  case mkcl_t_bignum:
    switch(ty) {
    case mkcl_t_fixnum: {	/* BIG / FIX */
      mkcl_object q = _mkcl_big_register0();
      mkcl_object r = _mkcl_big_register1();
      mkcl_object j = _mkcl_big_register2();
#if MKCL_LONG_BITS < MKCL_WORD_BITS
      _mkcl_big_set_fixnum(j, mkcl_fixnum_to_word(y));
#else
      mpz_set_si(j->big.big_num, mkcl_fixnum_to_word(y));
#endif
      mpz_fdiv_qr(q->big.big_num, r->big.big_num, x->big.big_num, j->big.big_num);
      v0 = _mkcl_big_register_normalize(env, q);
      v1 = _mkcl_big_register_normalize(env, r);
      break;
    }
    case mkcl_t_bignum: {	/* BIG / BIG */
      mkcl_object q = _mkcl_big_register0();
      mkcl_object r = _mkcl_big_register1();
      mpz_fdiv_qr(q->big.big_num, r->big.big_num, x->big.big_num, y->big.big_num);
      v0 = _mkcl_big_register_normalize(env, q);
      v1 = _mkcl_big_register_normalize(env, r);
      break;
    }
    case mkcl_t_ratio:		/* BIG / RAT */
      v0 = mkcl_floor2(env, mkcl_times(env, x, y->ratio.den), y->ratio.num);
      v1 = mkcl_make_ratio(env, MKCL_VALUES(1), y->ratio.den);
      break;
    case mkcl_t_singlefloat: {	/* BIG / SF */
      float n = mkcl_single_float(y);
#if 0
      float p = _mkcl_big_to_truncated_double(x) / n; /* does double truncation matter here? JCB */
#else
      float p = mkcl_to_double(env, x) / n; /* This one rounds the double. JCB */
#endif
      float q = floorf(p);
      v0 = mkcl_float_to_integer(env, q);
      v1 = mkcl_make_singlefloat(env, (p - q)*n);
      break;
    }
    case mkcl_t_doublefloat: {	/* BIG / DF */
      double n = mkcl_double_float(y);
#if 0
      double p = _mkcl_big_to_truncated_double(x) / n; /* does double truncation matter here? JCB */
#else
      double p = mkcl_to_double(env, x) / n; /* This one rounds the double. JCB */
#endif
      double q = floor(p);
      v0 = mkcl_double_to_integer(env, q);
      v1 = mkcl_make_doublefloat(env, (p - q)*n);
      break;
    }
#ifdef MKCL_LONG_FLOAT
    case mkcl_t_longfloat: {	/* BIG / LF */
      long double n = mkcl_long_float(y);
#if 0
      long double p = _mkcl_big_to_truncated_double(x) / n;
#else
      long double p = mkcl_to_long_double(env, x) / n; /* This one rounds the long double. JCB */
#endif
      long double q = floorl(p);
      v0 = mkcl_long_double_to_integer(env, q);
      v1 = mkcl_make_longfloat(env, (p - q)*n);
      break;
    }
#endif
    default:
      v0 = v1 = mk_cl_Cnil;
      ; /* Never reached */
    }
    break;
  case mkcl_t_ratio:
    switch(ty) {
    case mkcl_t_ratio:		/* RAT / RAT */
      v0 = mkcl_floor2(env, 
		      mkcl_times(env, x->ratio.num, y->ratio.den),
		      mkcl_times(env, x->ratio.den, y->ratio.num));
      v1 = mkcl_make_ratio(env, MKCL_VALUES(1), mkcl_times(env, x->ratio.den, y->ratio.den));
      break;
    default:		/* RAT / ANY */
      v0 = mkcl_floor2(env, x->ratio.num, mkcl_times(env, x->ratio.den, y));
      v1 = mkcl_divide(env, MKCL_VALUES(1), x->ratio.den);
    }
    break;
  case mkcl_t_singlefloat: {		/* SF / ANY */
    float n = mkcl_to_double(env, y);
    float p = mkcl_single_float(x)/n;
    float q = floorf(p);
    v0 = mkcl_float_to_integer(env, q);
    /* We cannot factor these two multiplications because
     * if we have signed zeros (1 - 1) * (-1) = -0 while
     * 1*(-1) - 1*(-1) = +0 */
    v1 = mkcl_make_singlefloat(env, p*n - q*n);
    break;
  }
  case mkcl_t_doublefloat: {		/* DF / ANY */
    double n = mkcl_to_double(env, y);
    double p = mkcl_double_float(x)/n;
    double q = floor(p);
    v0 = mkcl_double_to_integer(env, q);
    v1 = mkcl_make_doublefloat(env, p*n - q*n);
    break;
  }
#ifdef MKCL_LONG_FLOAT
  case mkcl_t_longfloat: {		/* LF / ANY */
    long double n = mkcl_to_long_double(env, y);
    long double p = mkcl_long_float(x)/n;
    long double q = floorl(p);
    v0 = mkcl_long_double_to_integer(env, q);
    v1 = mkcl_make_longfloat(env, p*n - q*n);
    break;
  }
#endif
  default:
    x = mkcl_type_error(env, @'floor',"argument",x,@'real');
    goto AGAIN;
  }
  mkcl_return_2_values(v0, v1);
}

@(defun floor (x &optional (y MKCL_OBJNULL))
@
  if (narg == 1)
    x = mkcl_floor1(env, x);
  else
    x = mkcl_floor2(env, x, y);
  mkcl_returnn(x);
@)

mkcl_object
mkcl_ceiling1(MKCL, mkcl_object x)
{
  mkcl_object v0, v1;
 AGAIN:
  switch (mkcl_type_of(x)) {
  case mkcl_t_fixnum:
  case mkcl_t_bignum:
    v0 = x;
    v1 = MKCL_MAKE_FIXNUM(0);
    break;
  case mkcl_t_ratio:
    v0 = mkcl_ceiling2(env, x->ratio.num, x->ratio.den);
    v1 = mkcl_make_ratio(env, MKCL_VALUES(1), x->ratio.den);
    break;
  case mkcl_t_singlefloat: {
    float d = mkcl_single_float(x);
    float y = ceilf(d);
    v0 = mkcl_float_to_integer(env, y);
    v1 = mkcl_make_singlefloat(env, d - y);
    break;
  }
  case mkcl_t_doublefloat: {
    double d = mkcl_double_float(x);
    double y = ceil(d);
    v0 = mkcl_double_to_integer(env, y);
    v1 = mkcl_make_doublefloat(env, d - y);
    break;
  }
#ifdef MKCL_LONG_FLOAT
  case mkcl_t_longfloat: {
    long double d = mkcl_long_float(x);
    long double y = ceill(d);
    v0 = mkcl_long_double_to_integer(env, y);
    v1 = mkcl_make_longfloat(env, d - y);
    break;
  }
#endif
  default:
    x = mkcl_type_error(env, @'ceiling',"argument",x,@'real');
    goto AGAIN;
  }
  mkcl_return_2_values(v0, v1);
}

mkcl_object
mkcl_ceiling2(MKCL, mkcl_object x, mkcl_object y)
{
  mkcl_object v0, v1;
  mkcl_type ty;
 AGAIN:
  while ((ty = mkcl_type_of(y), !MKCL_NUMBER_TYPE_P(ty))) {
    y = mkcl_type_error(env, @'ceiling',"divisor",y,@'real');
  }
  switch(mkcl_type_of(x)) {
  case mkcl_t_fixnum:
    switch(ty) {
    case mkcl_t_fixnum: {	/* FIX / FIX */
      mkcl_word a = mkcl_fixnum_to_word(x); mkcl_word b = mkcl_fixnum_to_word(y);
      mkcl_word q = a / b;  mkcl_word r = a % b;
      if ((r^b) > 0 && r) {	/* same signs and some remainder */
	v0 = MKCL_MAKE_FIXNUM(q+1);
	v1 = MKCL_MAKE_FIXNUM(r-b);
      } else {
	v0 = MKCL_MAKE_FIXNUM(q);
	v1 = MKCL_MAKE_FIXNUM(r);
      }
      break;
    }
    case mkcl_t_bignum: {	/* FIX / BIG */
      /* We must perform the division because there is the
       * pathological case
       *	x = MKCL_MOST_NEGATIVE_FIXNUM
       *    y = - MKCL_MOST_NEGATIVE_FIXNUM
       */
      mkcl_object q = _mkcl_big_register0();
      mkcl_object r = _mkcl_big_register1();
      mkcl_object j = _mkcl_big_register2();
#if MKCL_LONG_BITS < MKCL_WORD_BITS
      _mkcl_big_set_fixnum(j, mkcl_fixnum_to_word(x));
#else
      mpz_set_si(j->big.big_num, mkcl_fixnum_to_word(x));
#endif
      mpz_cdiv_qr(q->big.big_num, r->big.big_num, j->big.big_num, y->big.big_num);
      v0 = _mkcl_big_register_normalize(env, q);
      v1 = _mkcl_big_register_normalize(env, r);
      break;
    }
    case mkcl_t_ratio:		/* FIX / RAT */
      v0 = mkcl_ceiling2(env, mkcl_times(env, x, y->ratio.den), y->ratio.num);
      v1 = mkcl_make_ratio(env, MKCL_VALUES(1), y->ratio.den);
      break;
    case mkcl_t_singlefloat: {	/* FIX / SF */
      float n = mkcl_single_float(y);
      float p = mkcl_fixnum_to_word(x)/n;
      float q = ceilf(p);
      v0 = mkcl_float_to_integer(env, q);
      v1 = mkcl_make_singlefloat(env, p*n - q*n);
      break;
    }
    case mkcl_t_doublefloat: {	/* FIX / DF */
      double n = mkcl_double_float(y);
      double p = mkcl_fixnum_to_word(x)/n;
      double q = ceil(p);
      v0 = mkcl_double_to_integer(env, q);
      v1 = mkcl_make_doublefloat(env, p*n - q*n);
      break;
    }
#ifdef MKCL_LONG_FLOAT
    case mkcl_t_longfloat: {	/* FIX / LF */
      long double n = mkcl_long_float(y);
      long double p = mkcl_fixnum_to_word(x)/n;
      long double q = ceill(p);
      v0 = mkcl_long_double_to_integer(env, q);
      v1 = mkcl_make_longfloat(env, p*n - q*n);
      break;
    }
#endif
    default:
      v0 = v1 = mk_cl_Cnil;
      (void)0; /*Never reached */
    }
    break;
  case mkcl_t_bignum:
    switch(mkcl_type_of(y)) {
    case mkcl_t_fixnum: {	/* BIG / FIX */
      mkcl_object q = _mkcl_big_register0();
      mkcl_object r = _mkcl_big_register1();
      mkcl_object j = _mkcl_big_register2();
#if MKCL_LONG_BITS < MKCL_WORD_BITS
      _mkcl_big_set_fixnum(j, mkcl_fixnum_to_word(y));
#else
      mpz_set_si(j->big.big_num, mkcl_fixnum_to_word(y));
#endif
      mpz_cdiv_qr(q->big.big_num, r->big.big_num, x->big.big_num, j->big.big_num);
      v0 = _mkcl_big_register_normalize(env, q);
      v1 = _mkcl_big_register_normalize(env, r);
      break;
    }
    case mkcl_t_bignum: {	/* BIG / BIG */
      mkcl_object q = _mkcl_big_register0();
      mkcl_object r = _mkcl_big_register1();
      mpz_cdiv_qr(q->big.big_num, r->big.big_num, x->big.big_num, y->big.big_num);
      v0 = _mkcl_big_register_normalize(env, q);
      v1 = _mkcl_big_register_normalize(env, r);
      break;
    }
    case mkcl_t_ratio:		/* BIG / RAT */
      v0 = mkcl_ceiling2(env, mkcl_times(env, x, y->ratio.den), y->ratio.num);
      v1 = mkcl_make_ratio(env, MKCL_VALUES(1), y->ratio.den);
      break;
    case mkcl_t_singlefloat: {	/* BIG / SF */
      float n = mkcl_single_float(y);
#if 0
      float p = _mkcl_big_to_truncated_double(x)/n; /* does double truncation matter here? JCB */
#else
      float p = mkcl_to_double(env, x) / n; /* This one rounds the double. JCB */
#endif
      float q = ceilf(p);
      v0 = mkcl_float_to_integer(env, q);
      v1 = mkcl_make_singlefloat(env, p*n - q*n);
      break;
    }
    case mkcl_t_doublefloat: {	/* BIG / DF */
      double n = mkcl_double_float(y);
#if 0
      double p = _mkcl_big_to_truncated_double(x)/n; /* does double truncation matter here? JCB */
#else
      double p = mkcl_to_double(env, x) / n; /* This one rounds the double. JCB */
#endif
      double q = ceil(p);
      v0 = mkcl_double_to_integer(env, q);
      v1 = mkcl_make_doublefloat(env, p*n - q*n);
      break;
    }
#ifdef MKCL_LONG_FLOAT
    case mkcl_t_longfloat: {	/* BIG / LF */
      long double n = mkcl_long_float(y);
#if 0
      long double p = _mkcl_big_to_truncated_double(x)/n;
#else
      long double p = mkcl_to_long_double(env, x) / n; /* This one rounds the long double. JCB */
#endif
      long double q = ceill(p);
      v0 = mkcl_long_double_to_integer(env, q);
      v1 = mkcl_make_longfloat(env, p*n - q*n);
      break;
    }
#endif
    default:
      v0 = v1 = mk_cl_Cnil;
      ; /*Never reached */
    }
    break;
  case mkcl_t_ratio:
    switch(mkcl_type_of(y)) {
    case mkcl_t_ratio:		/* RAT / RAT */
      v0 = mkcl_ceiling2(env, 
			mkcl_times(env, x->ratio.num, y->ratio.den),
			mkcl_times(env, x->ratio.den, y->ratio.num));
      v1 = mkcl_make_ratio(env, MKCL_VALUES(1), mkcl_times(env, x->ratio.den, y->ratio.den));
      break;
    default:		/* RAT / ANY */
      v0 = mkcl_ceiling2(env, x->ratio.num, mkcl_times(env, x->ratio.den, y));
      v1 = mkcl_divide(env, MKCL_VALUES(1), x->ratio.den);
    }
    break;
  case mkcl_t_singlefloat: {		/* SF / ANY */
    float n = mkcl_to_double(env, y);
    float p = mkcl_single_float(x)/n;
    float q = ceilf(p);
    v0 = mkcl_float_to_integer(env, q);
    v1 = mkcl_make_singlefloat(env, p*n - q*n);
    break;
  }
  case mkcl_t_doublefloat: {		/* DF / ANY */
    double n = mkcl_to_double(env, y);
    double p = mkcl_double_float(x)/n;
    double q = ceil(p);
    v0 = mkcl_double_to_integer(env, q);
    v1 = mkcl_make_doublefloat(env, p*n - q*n);
    break;
  }
#ifdef MKCL_LONG_FLOAT
  case mkcl_t_longfloat: {		/* LF / ANY */
    long double n = mkcl_to_long_double(env, y);
    long double p = mkcl_long_float(x)/n;
    long double q = ceill(p);
    v0 = mkcl_long_double_to_integer(env, q);
    v1 = mkcl_make_longfloat(env, p*n - q*n);
    break;
  }
#endif
  default:
    x = mkcl_type_error(env, @'ceiling',"argument",x,@'real');
    goto AGAIN;
  }
  mkcl_return_2_values(v0, v1);
}

@(defun ceiling (x &optional (y MKCL_OBJNULL))
@
  if (narg == 1)
    x = mkcl_ceiling1(env, x);
  else
    x = mkcl_ceiling2(env, x, y);
  mkcl_returnn(x);
@)

mkcl_object
mkcl_truncate1(MKCL, mkcl_object x)
{
  mkcl_object v0, v1;
 AGAIN:
  switch (mkcl_type_of(x)) {
  case mkcl_t_fixnum:
  case mkcl_t_bignum:
    v0 = x;
    v1 = MKCL_MAKE_FIXNUM(0);
    break;
  case mkcl_t_ratio:
    v0 = mkcl_truncate2(env, x->ratio.num, x->ratio.den);
    v1 = mkcl_make_ratio(env, MKCL_VALUES(1), x->ratio.den);
    break;
  case mkcl_t_singlefloat: {
    float d = mkcl_single_float(x);
    float y = d > 0? floorf(d) : ceilf(d);
    v0 = mkcl_float_to_integer(env, y);
    v1 = mkcl_make_singlefloat(env, d - y);
    break;
  }
  case mkcl_t_doublefloat: {
    double d = mkcl_double_float(x);
    double y = d > 0? floor(d) : ceil(d);
    v0 = mkcl_double_to_integer(env, y);
    v1 = mkcl_make_doublefloat(env, d - y);
    break;
  }
#ifdef MKCL_LONG_FLOAT
  case mkcl_t_longfloat: {
    long double d = mkcl_long_float(x);
    long double y = d > 0? floorl(d) : ceill(d);
    v0 = mkcl_long_double_to_integer(env, y);
    v1 = mkcl_make_longfloat(env, d - y);
    break;
  }
#endif
  default:
    x = mkcl_type_error(env, @'truncate',"argument",x,@'real');
    goto AGAIN;
  }
  mkcl_return_2_values(v0, v1);
}

mkcl_object
mkcl_truncate2(MKCL, mkcl_object x, mkcl_object y)
{
  if (mkcl_plusp(env, x) != mkcl_plusp(env, y))
    return mkcl_ceiling2(env, x, y);
  else
    return mkcl_floor2(env, x, y);
}

@(defun truncate (x &optional (y MKCL_OBJNULL))
@
  if (narg == 1)
    x = mkcl_truncate1(env, x);
  else
    x = mkcl_truncate2(env, x, y);
  mkcl_returnn(x);
@)

static double
round_double(double d)
{
  if (d >= 0) {
    double q = floor(d + 0.5);
    d -= q;
    if (d == -0.5) {
      double x = q / 10;
      int i = (int)(10 * (x - floor(x)));
      if (i & 1) {
	return q-1;
      }
    }
    return q;
  } else {
    return -round_double(-d);
  }
}

#ifdef MKCL_LONG_FLOAT
static long double
round_long_double(long double d)
{
  if (d >= 0) {
    long double q = floorl(d + 0.5);
    d -= q;
    if (d == -0.5) {
      long double x = q / 10;
      int i = (int)(10 * (x - floorl(x)));
      if (i & 1) {
	return q-1;
      }
    }
    return q;
  } else {
    return -round_long_double(-d);
  }
}
#endif

mkcl_object
mkcl_round1(MKCL, mkcl_object x)
{
  mkcl_object v0, v1;
 AGAIN:
  switch (mkcl_type_of(x)) {
  case mkcl_t_fixnum:
  case mkcl_t_bignum:
    v0 = x;
    v1 = MKCL_MAKE_FIXNUM(0);
    break;
  case mkcl_t_ratio:
    v0 = mkcl_round2(env, x->ratio.num, x->ratio.den);
    v1 = mkcl_make_ratio(env, MKCL_VALUES(1), x->ratio.den);
    break;
  case mkcl_t_singlefloat: {
    float d = mkcl_single_float(x);
    float q = round_double(d);
    v0 = mkcl_float_to_integer(env, q);
    v1 = mkcl_make_singlefloat(env, d - q);
    break;
  }
  case mkcl_t_doublefloat: {
    double d = mkcl_double_float(x);
    double q = round_double(d);
    v0 = mkcl_double_to_integer(env, q);
    v1 = mkcl_make_doublefloat(env, d - q);
    break;
  }
#ifdef MKCL_LONG_FLOAT
  case mkcl_t_longfloat: {
    long double d = mkcl_long_float(x);
    long double q = round_long_double(d);
    v0 = mkcl_long_double_to_integer(env, q);
    v1 = mkcl_make_longfloat(env, d - q);
    break;
  }
#endif
  default:
    x = mkcl_type_error(env, @'round',"argument",x,@'real');
    goto AGAIN;
  }
  mkcl_return_2_values(v0, v1);
}

mkcl_object
mkcl_round2(MKCL, mkcl_object x, mkcl_object y)
{
  mkcl_object v0, v1;
  mkcl_object q;

  q = mkcl_divide(env, x, y);
  switch (mkcl_type_of(q)) {
  case mkcl_t_fixnum:
  case mkcl_t_bignum:
    v0 = q;
    v1 = MKCL_MAKE_FIXNUM(0);
    break;
  case mkcl_t_ratio: {
    mkcl_object q1 = mkcl_integer_divide(env, q->ratio.num, q->ratio.den);
    mkcl_object r = mkcl_minus(env, q, q1);
    if (mkcl_minusp(env, r)) {
      int c = mkcl_number_compare(env, mkcl_core.minus_half, r);
      if (c > 0 || (c == 0 && mkcl_oddp(env, q1))) {
	q1 = mkcl_one_minus(env, q1);
      }
    } else {
      int c = mkcl_number_compare(env, r, mkcl_core.plus_half);
      if (c > 0 || (c == 0 && mkcl_oddp(env, q1))) {
	q1 = mkcl_one_plus(env, q1);
      }
    }
    v0 = q1;
    v1 = number_remainder(env, x, y, q1);
    break;
  }
  default:
    v0 = q = mkcl_round1(env, q);
    v1 = number_remainder(env, x, y, q);
  }
  mkcl_return_2_values(v0, v1);
}

@(defun round (x &optional (y MKCL_OBJNULL))
@
  if (narg == 1)
    x = mkcl_round1(env, x);
  else
    x = mkcl_round2(env, x, y);
  mkcl_returnn(x);
@)


mkcl_object
mk_cl_mod(MKCL, mkcl_object x, mkcl_object y)
{
  mkcl_call_stack_check(env);
  /* INV: #'floor always outputs two values */
  @floor(env, 2, x, y);
  mkcl_return_value(MKCL_VALUES(1));
}

mkcl_object
mk_cl_rem(MKCL, mkcl_object x, mkcl_object y)
{
  mkcl_call_stack_check(env);
  @truncate(env, 2, x, y);
  mkcl_return_value(MKCL_VALUES(1));
}

mkcl_object
mk_cl_decode_float(MKCL, mkcl_object x)
{
  int e, s;
  mkcl_object sign;

  mkcl_call_stack_check(env);
 AGAIN:
  switch (mkcl_type_of(x)) {
  case mkcl_t_singlefloat: {
    float f = mkcl_single_float(x);
    if (f >= 0.0) {
      s = 1;
    } else {
      f = -f;
      s = -1;
    }
    f = frexpf(f, &e);
    x = mkcl_make_singlefloat(env, f);
    sign = mkcl_make_singlefloat(env, s);
    break;
  }
  case mkcl_t_doublefloat: {
    double d = mkcl_double_float(x);
    if (d >= 0.0) {
      s = 1;
    } else {
      d = -d;
      s = -1;
    }
    d = frexp(d, &e);
    x = mkcl_make_doublefloat(env, d);
    sign = mkcl_make_doublefloat(env, s);
    break;
  }
#ifdef MKCL_LONG_FLOAT
  case mkcl_t_longfloat: {
    long double d = mkcl_long_float(x);
    if (d >= 0.0)
      s = 1;
    else {
      d = -d;
      s = -1;
    }
    d = frexpl(d, &e);
    x = mkcl_make_longfloat(env, d);
    sign = mkcl_make_longfloat(env, s);
    break;
  }
#endif
  default:
    x = mkcl_type_error(env, @'decode-float',"argument",x,@'float');
    goto AGAIN;
  }
  mkcl_return_3_values(x, MKCL_MAKE_FIXNUM(e), sign);
}

mkcl_object
mk_cl_scale_float(MKCL, mkcl_object x, mkcl_object y)
{
  mkcl_word k;

  mkcl_call_stack_check(env);
 AGAIN:
  if (MKCL_FIXNUMP(y)) {
    k = mkcl_fixnum_to_word(y);
  } else {
    y = mkcl_type_error(env, @'scale-float',"exponent",y,@'fixnum');
    goto AGAIN;
  }
  switch (mkcl_type_of(x)) {
  case mkcl_t_singlefloat:
    x = mkcl_make_singlefloat(env, ldexpf(mkcl_single_float(x), k));
    break;
  case mkcl_t_doublefloat:
    x = mkcl_make_doublefloat(env, ldexp(mkcl_double_float(x), k));
    break;
#ifdef MKCL_LONG_FLOAT
  case mkcl_t_longfloat:
    x = mkcl_make_longfloat(env, ldexpl(mkcl_long_float(x), k));
    break;
#endif
  default:
    x = mkcl_type_error(env, @'scale-float', "argument", x, @'float');
    goto AGAIN;
  }
  mkcl_return_value(x);
}

mkcl_object
mk_cl_float_radix(MKCL, mkcl_object x)
{
  mkcl_call_stack_check(env);
  while (mk_cl_floatp(env, x) != mk_cl_Ct) {
    x = mkcl_type_error(env, @'float-radix', "argument", x, @'float');
  }
  mkcl_return_value(MKCL_MAKE_FIXNUM(FLT_RADIX));
}

@(defun float_sign (x &optional (y x yp))
  int negativep;
@
  if (!yp) {
    y = mk_cl_float(env, 2, MKCL_MAKE_FIXNUM(1), x);
  }
 AGAIN:
  switch (mkcl_type_of(x)) {
  case mkcl_t_singlefloat:
    negativep = signbit(mkcl_single_float(x));
    break;
  case mkcl_t_doublefloat:
    negativep = signbit(mkcl_double_float(x));
    break;
#ifdef MKCL_LONG_FLOAT
  case mkcl_t_longfloat:
    negativep = signbit(mkcl_long_float(x));
    break;
#endif
  default:
    x = mkcl_type_error(env, @'float-sign',"argument",x,@'float');
    goto AGAIN;
  }
  switch (mkcl_type_of(y)) {
  case mkcl_t_singlefloat:
    {
      float f = mkcl_single_float(y);
      if (signbit(f) != negativep) y = mkcl_make_singlefloat(env, -f);
    }
    break;
  case mkcl_t_doublefloat:
    {
      double f = mkcl_double_float(y);
      if (signbit(f) != negativep) y = mkcl_make_doublefloat(env, -f);
    }
    break;
#ifdef MKCL_LONG_FLOAT
  case mkcl_t_longfloat:
    {
      long double f = mkcl_long_float(y);
      if (signbit(f) != negativep) y = mkcl_make_longfloat(env, -f);
    }
    break;
#endif
  default:
    y = mkcl_type_error(env, @'float-sign', "prototype", y, @'float');
    goto AGAIN;
  }
  mkcl_return_value(y);
@)

mkcl_object
mk_cl_float_digits(MKCL, mkcl_object x)
{
  mkcl_call_stack_check(env);
 AGAIN:
  switch (mkcl_type_of(x)) {
  case mkcl_t_singlefloat:
    x = MKCL_MAKE_FIXNUM(FLT_MANT_DIG);
    break;
  case mkcl_t_doublefloat:
    x = MKCL_MAKE_FIXNUM(DBL_MANT_DIG);
    break;
#ifdef MKCL_LONG_FLOAT
  case mkcl_t_longfloat:
    x = MKCL_MAKE_FIXNUM(LDBL_MANT_DIG);
    break;
#endif
  default:
    x = mkcl_type_error(env, @'float-digits', "argument", x, @'float');
    goto AGAIN;
  }
  mkcl_return_value(x);
}

mkcl_object
mk_cl_float_precision(MKCL, mkcl_object x)
{
  int precision;

  mkcl_call_stack_check(env);
 AGAIN:
  switch (mkcl_type_of(x)) {
  case mkcl_t_singlefloat:
    {
      float f = mkcl_single_float(x);
      if (f == 0.0) {
	precision = 0;
      } else {
	int exp;
	frexpf(f, &exp);
	if (exp >= FLT_MIN_EXP) {
	  precision = FLT_MANT_DIG;
	} else {
	  precision = FLT_MANT_DIG - (FLT_MIN_EXP - exp);
	}
      }
    }
    break;
  case mkcl_t_doublefloat:
    {
      double f = mkcl_double_float(x);
      if (f == 0.0) {
	precision = 0;
      } else {
	int exp;
	frexp(f, &exp);
	if (exp >= DBL_MIN_EXP) {
	  precision = DBL_MANT_DIG;
	} else {
	  precision = DBL_MANT_DIG - (DBL_MIN_EXP - exp);
	}
      }
    }
    break;
#ifdef MKCL_LONG_FLOAT
  case mkcl_t_longfloat:
    {
      long double f = mkcl_long_float(x);
      if (f == 0.0) {
	precision = 0;
      } else {
	int exp;
	frexp(f, &exp);
	if (exp >= LDBL_MIN_EXP) {
	  precision = LDBL_MANT_DIG;
	} else {
	  precision = LDBL_MANT_DIG - (LDBL_MIN_EXP - exp);
	}
      }
    }
    break;
#endif
  default:
    x = mkcl_type_error(env, @'float-precision', "argument", x, @'float');
    goto AGAIN;
  }
  mkcl_return_value(MKCL_MAKE_FIXNUM(precision));
}

mkcl_object
mk_cl_integer_decode_float(MKCL, mkcl_object x)
{
  int e, s = 1;

  mkcl_call_stack_check(env);
 AGAIN:
  switch (mkcl_type_of(x)) {
#ifdef MKCL_LONG_FLOAT
  case mkcl_t_longfloat:
    {
      long double d = mkcl_long_float(x);
      if (signbit(d)) {
	s = -1;
	d = -d;
      }
      if (d == 0.0) {
	e = 0;
	x = MKCL_MAKE_FIXNUM(0);
      } else {
	d = frexpl(d, &e);
	/* FIXME! Loss of precision! */
	x = mkcl_double_to_integer(env, ldexpl(d, LDBL_MANT_DIG));
	e -= LDBL_MANT_DIG;
      }
    }
    break;
#endif /* def MKCL_LONG_FLOAT */
  case mkcl_t_doublefloat:
    {
      double d = mkcl_double_float(x);
      if (signbit(d)) {
	s = -1;
	d = -d;
      }
      if (d == 0.0) {
	e = 0;
	x = MKCL_MAKE_FIXNUM(0);
      } else {
	d = frexp(d, &e);
	x = mkcl_double_to_integer(env, ldexp(d, DBL_MANT_DIG));
	e -= DBL_MANT_DIG;
      }
    }
    break;
  case mkcl_t_singlefloat:
    {
      float d = mkcl_single_float(x);
      if (signbit(d)) {
	s = -1;
	d = -d;
      }
      if (d == 0.0) {
	e = 0;
	x = MKCL_MAKE_FIXNUM(0);
      } else {
	d = frexpf(d, &e);
	x = mkcl_double_to_integer(env, ldexp(d, FLT_MANT_DIG));
	e -= FLT_MANT_DIG;
      }
    }
    break;
  default:
    x = mkcl_type_error(env, @'integer-decode-float', "argument", x, @'float');
    goto AGAIN;
  }
  mkcl_return_3_values(x, MKCL_MAKE_FIXNUM(e), MKCL_MAKE_FIXNUM(s));
}


@(defun complex (r &optional (i MKCL_MAKE_FIXNUM(0)))
@	/* INV: mkcl_make_complex() checks types */
  mkcl_return_value(mkcl_make_complex(env, r, i));
@)

mkcl_object
mk_cl_realpart(MKCL, mkcl_object x)
{
  mkcl_call_stack_check(env);
 AGAIN:
  switch (mkcl_type_of(x)) {
  case mkcl_t_fixnum:
  case mkcl_t_bignum:
  case mkcl_t_ratio:
  case mkcl_t_singlefloat:
  case mkcl_t_doublefloat:
#ifdef MKCL_LONG_FLOAT
  case mkcl_t_longfloat:
#endif
    break;
  case mkcl_t_complex:
    x = x->_complex.real;
    break;
  default:
    x = mkcl_type_error(env, @'realpart', "argument", x, @'number');
    goto AGAIN;
  }
  mkcl_return_value(x);
}

mkcl_object
mk_cl_imagpart(MKCL, mkcl_object x)
{
  mkcl_call_stack_check(env);
 AGAIN:
  switch (mkcl_type_of(x)) {
  case mkcl_t_fixnum:
  case mkcl_t_bignum:
  case mkcl_t_ratio:
    x = MKCL_MAKE_FIXNUM(0);
    break;
  case mkcl_t_singlefloat:
    if (signbit(mkcl_single_float(x)))
      x = mkcl_core.singlefloat_minus_zero;
    else
      x = mkcl_core.singlefloat_zero;
    break;
  case mkcl_t_doublefloat:
    if (signbit(mkcl_double_float(x)))
      x = mkcl_core.doublefloat_minus_zero;
    else
      x = mkcl_core.doublefloat_zero;
    break;
#ifdef MKCL_LONG_FLOAT
  case mkcl_t_longfloat:
    if (signbit(mkcl_long_float(x)))
      x = mkcl_core.longfloat_minus_zero;
    else
      x = mkcl_core.longfloat_zero;
    break;
#endif /* def MKCL_LONG_FLOAT */
  case mkcl_t_complex:
    x = x->_complex.imag;
    break;
  default:
    x = mkcl_type_error(env, @'imagpart', "argument", x, @'number');
    goto AGAIN;
  }
  mkcl_return_value(x);
}

