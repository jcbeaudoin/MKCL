/* -*- mode: c -*- */
/*
    num_sfun.c  -- Trascendental functions.
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
#include <mkcl/mkcl-math.h>
#include <mkcl/internal.h>

#ifndef HAVE_LOG1P
double
log1p(double x)
{
	double u = 1.0 + x;
	if (u == 1) {
		return 0.0;
	} else {
		return (log(u) * x)/(u - 1.0);
	}
}
#endif

#ifndef HAVE_LOG1PF
float
log1pf(float x)
{
	float u = (float)1 + x;
	if (u == 1) {
		return (float)0;
	} else {
		return (logf(u) * x)/(u - (float)1);
	}
}
#endif

#if !defined(HAVE_LOG1PL) && defined(MKCL_LONG_FLOAT)
long double
log1pl(long double x)
{
	long double u = (long double)1 + x;
	if (u == 1) {
		return (long double)1;
	} else {
		return (logl(u) * x)/(u - (long double)1);
	}
}
#endif

mkcl_object
mkcl_abs(MKCL, mkcl_object x)
{
  if (mkcl_type_of(x) != mkcl_t_complex) {
    if (mkcl_minusp(env, x)) {
      x = mkcl_negate(env, x);
    }
  } else {
    /* Compute sqrt(r*r + i*i) carefully to prevent overflow.
     * Assume |i| >= |r|. Then sqrt(i*i + r*r) = |i|*sqrt(1 +(r/i)^2).
     */
    mkcl_object r = x->_complex.real;
    mkcl_object i = x->_complex.imag;
    int comparison;
    if (mkcl_minusp(env, r)) r = mkcl_negate(env, r);
    if (mkcl_minusp(env, i)) i = mkcl_negate(env, i);
    comparison = mkcl_number_compare(env, r, i);
    if (comparison == 0) {
      r = mkcl_times(env, r, r);
      x = mk_cl_sqrt(env, mkcl_plus(env, r, r));
    } else {
      if (comparison > 0) {
	mkcl_object aux = i;
	i = r; r = aux;
      }
      r = mkcl_divide(env, r, i);
      r = mkcl_plus(env, MKCL_MAKE_FIXNUM(1), mkcl_times(env, r, r));
      x = mkcl_times(env, mk_cl_sqrt(env, r), i);
    }
  }
  return x;
}

mkcl_object
mk_cl_abs(MKCL, mkcl_object x)
{
  mkcl_call_stack_check(env);
  @(return mkcl_abs(env, x));
}

mkcl_word
mkcl_word_expt(mkcl_word x, mkcl_word y)
{
  mkcl_word z = 1;
  while (y > 0)
    if (y % 2 == 0) {
      x *= x;
      y /= 2;
    } else {
      z *= x;
      --y;
    }
  return(z);
}

mkcl_object
mk_cl_exp(MKCL, mkcl_object x)
{
  mkcl_object output;

  mkcl_call_stack_check(env);
  MKCL_MATHERR_CLEAR;
 AGAIN:
  switch (mkcl_type_of(x)) {
  case mkcl_t_fixnum:
  case mkcl_t_bignum:
  case mkcl_t_ratio:
    output = mkcl_make_singlefloat(env, expf(mkcl_to_double(env, x))); break;
  case mkcl_t_singlefloat:
    output = mkcl_make_singlefloat(env, expf(mkcl_single_float(x))); break;
  case mkcl_t_doublefloat:
    output = mkcl_make_doublefloat(env, exp(mkcl_double_float(x))); break;
#ifdef MKCL_LONG_FLOAT
  case mkcl_t_longfloat:
    output = mkcl_make_longfloat(env, expl(mkcl_long_float(x))); break;
#endif
  case mkcl_t_complex: {
    mkcl_object y, y1;

    y = x->_complex.imag;
    output = mk_cl_exp(env, x->_complex.real);
    y1 = mk_cl_cos(env, y);
    y = mk_cl_sin(env, y);
    y = mkcl_make_complex(env, y1, y);
    output = mkcl_times(env, output, y);
    break;
  }
  default:
    x = mkcl_type_error(env, @'exp',"exponent",x,@'number');
    goto AGAIN;
  }
  MKCL_MATHERR_TEST(env);
  @(return output);
}

mkcl_object
mk_cl_expt(MKCL, mkcl_object x, mkcl_object y)
{
  mkcl_type ty, tx;
  mkcl_object z;

  mkcl_call_stack_check(env);
 /* AGAIN: */
  while ((ty = mkcl_type_of(y), !MKCL_NUMBER_TYPE_P(ty))) {
    y = mkcl_type_error(env, @'exp',"exponent",y,@'number');
  }
  while ((tx = mkcl_type_of(x), !MKCL_NUMBER_TYPE_P(tx))) {
    x = mkcl_type_error(env, @'exp',"basis",x,@'number');
  }
  if (mkcl_zerop(env, y)) {
    /* INV: The most specific numeric types come first. */
    switch ((ty > tx)? ty : tx) {
    case mkcl_t_fixnum:
    case mkcl_t_bignum:
    case mkcl_t_ratio:
      z = MKCL_MAKE_FIXNUM(1); break;
    case mkcl_t_singlefloat:
      z = mkcl_make_singlefloat(env, 1.0); break;
    case mkcl_t_doublefloat:
      z = mkcl_make_doublefloat(env, 1.0); break;
#ifdef MKCL_LONG_FLOAT
    case mkcl_t_longfloat:
      z = mkcl_make_longfloat(env, 1.0); break;
#endif
    case mkcl_t_complex:
      z = mk_cl_expt(env, ((tx == mkcl_t_complex) ? x->_complex.real : x), ((ty == mkcl_t_complex) ? y->_complex.real : y));
      z = mkcl_make_complex(env, z, MKCL_MAKE_FIXNUM(0));
      break;
    default:
      /* We will never reach this */
      z = mk_cl_Cnil; /* just to silence the compiler. */
      ;
    }
  } else if (mkcl_zerop(env, x)) {
    z = mkcl_times(env, x, y);
    if (!mkcl_plusp(env, ty == mkcl_t_complex ? y->_complex.real : y))
      z = mkcl_divide(env, MKCL_MAKE_FIXNUM(1), z);
  } else if (ty != mkcl_t_fixnum && ty != mkcl_t_bignum) {
    z = mkcl_log1(env, x);
    z = mkcl_times(env, z, y);
    z = mk_cl_exp(env, z);
  } else if (mkcl_minusp(env, y)) {
    z = mkcl_negate(env, y);
    z = mk_cl_expt(env, x, z);
    z = mkcl_divide(env, MKCL_MAKE_FIXNUM(1), z);
  } else {
    z = MKCL_MAKE_FIXNUM(1);
    do {
      /* INV: mkcl_integer_divide outputs an integer */
      if (!mkcl_evenp(env, y))
	z = mkcl_times(env, z, x);
      y = mkcl_integer_divide(env, y, MKCL_MAKE_FIXNUM(2));
      if (mkcl_zerop(env, y)) break;
      x = mkcl_times(env, x, x);
    } while (1);
  }
  @(return z);
}

static mkcl_object
mkcl_log1_complex(MKCL, mkcl_object r, mkcl_object i)
{
  mkcl_object a = mkcl_abs(env, r);
  mkcl_object p = mkcl_abs(env, i);
  int rel = mkcl_number_compare(env, a, p);
  if (rel > 0) {
    mkcl_object aux = p;
    p = a; a = aux;
  } else if (rel == 0) {
    /* if a == p, 
     * log(sqrt(a^2+p^2)) = log(2a^2)/2
     */
    a = mkcl_times(env, a, a);
    a = mkcl_divide(env, mkcl_log1(env, mkcl_plus(env, a, a)), MKCL_MAKE_FIXNUM(2));
    p = MKCL_MAKE_FIXNUM(0);
    goto OUTPUT;
  }
  /* For the real part of the output we use the formula
   *	log(sqrt(p^2 + a^2)) = log(sqrt(p^2*(1 + (a/p)^2)))
   *			     = log(p) + log(1 + (a/p)^2)/2; */
  a = mkcl_divide(env, a, p);
  a = mkcl_plus(env,
	       mkcl_divide(env,
			  mkcl_log1p(env, mkcl_times(env, a,a)),
			  MKCL_MAKE_FIXNUM(2)),
	       mkcl_log1(env, p));
  p = mkcl_atan2(env, i, r);
 OUTPUT:
  return mkcl_make_complex(env, a, p);
}

mkcl_object
mkcl_log1(MKCL, mkcl_object x)
{
  mkcl_object output;
  MKCL_MATHERR_CLEAR;
 AGAIN:
  switch (mkcl_type_of(x))
    {
    case mkcl_t_fixnum:
    case mkcl_t_ratio:
      {
	float f = mkcl_to_float(env, x);
	if (f < 0) goto COMPLEX;
	output = mkcl_make_singlefloat(env, logf(f));
	/* output = mkcl_make_singlefloat(env, logf(mkcl_to_double(env, x))); */
      }
      break;
    case mkcl_t_bignum:
      {
	mkcl_word l = mkcl_integer_length(env, x) - 1;
	mkcl_object r = mkcl_make_ratio(env, x, mkcl_ash(env, MKCL_MAKE_FIXNUM(1), l));
	float d = logf(mkcl_to_float(env, r)) + l * logf(2.0F);
	if (d < 0) goto COMPLEX;
	output = mkcl_make_singlefloat(env, d);
      }
      break;
    case mkcl_t_singlefloat:
      {
	float f = mkcl_single_float(x);
	if (isnan(f)) goto ISNAN;
	if (f < 0) goto COMPLEX;
	output = mkcl_make_singlefloat(env, logf(f));
      }
      break;
    case mkcl_t_doublefloat:
      {
	double f = mkcl_double_float(x);
	if (isnan(f)) goto ISNAN;
	if (f < 0) goto COMPLEX;
	output = mkcl_make_doublefloat(env, log(f));
      }
      break;
#ifdef MKCL_LONG_FLOAT
    case mkcl_t_longfloat:
      {
	long double f = mkcl_long_float(x);
	if (isnan(f)) goto ISNAN;
	if (f < 0) goto COMPLEX;
	output = mkcl_make_longfloat(env, logl(f));
      }
      break;
#endif
    case mkcl_t_complex:
      output = mkcl_log1_complex(env, x->_complex.real, x->_complex.imag);
      break;
    ISNAN:
      output = x;
      break;
    COMPLEX:
      output = mkcl_log1_complex(env, x, MKCL_MAKE_FIXNUM(0));
      break;
    default:
      x = mkcl_type_error(env, @'log', "argument", x, @'number');
      goto AGAIN;
      /* We do not reach here */
      ;
    }
  MKCL_MATHERR_TEST(env);
  return output;
}

mkcl_object
mkcl_log2(MKCL, mkcl_object x, mkcl_object y)
{
  return mkcl_divide(env, mkcl_log1(env, y), mkcl_log1(env, x));
}

mkcl_object
mkcl_log1p(MKCL, mkcl_object x)
{
  mkcl_object output;
  MKCL_MATHERR_CLEAR;
 AGAIN:
  switch (mkcl_type_of(x))
    {
    case mkcl_t_fixnum:
    case mkcl_t_bignum:
    case mkcl_t_ratio:
      {
	float f = mkcl_to_double(env, x);
	if (f < -1) goto COMPLEX;
	output = mkcl_make_singlefloat(env, log1pf(mkcl_to_double(env, x)));
      }
      break;
    case mkcl_t_singlefloat:
      {
	float f = mkcl_single_float(x);
	if (isnan(f)) goto ISNAN;
	if (f < -1) goto COMPLEX;
	output = mkcl_make_singlefloat(env, log1pf(f));
      }
      break;
    case mkcl_t_doublefloat:
      {
	double f = mkcl_double_float(x);
	if (isnan(f)) goto ISNAN;
	if (f < -1) goto COMPLEX;
	output = mkcl_make_doublefloat(env, log1p(f));
      }
      break;
#ifdef MKCL_LONG_FLOAT
    case mkcl_t_longfloat:
      {
	long double f = mkcl_long_float(x);
	if (isnan(f)) goto ISNAN;
	if (f < -1) goto COMPLEX;
	output = mkcl_make_longfloat(env, log1pl(f));
      }
      break;
#endif
    case mkcl_t_complex:
      output = mkcl_log1(env, mkcl_plus(env, MKCL_MAKE_FIXNUM(1), x));
      break;
    ISNAN:
      output = x;
      break;
    COMPLEX:
      output = mkcl_log1_complex(env, mkcl_plus(env, x, MKCL_MAKE_FIXNUM(1)), MKCL_MAKE_FIXNUM(0));
      break;
    default:
      x = mkcl_type_error(env, @'log', "argument", x, @'number');
      goto AGAIN;
      ;
    }
  MKCL_MATHERR_TEST(env);
  return output;
}

mkcl_object
mk_si_log1p(MKCL, mkcl_object x)
{
  mkcl_call_stack_check(env);
  @(return mkcl_log1p(env, x));
}

mkcl_object
mk_cl_sqrt(MKCL, mkcl_object x)
{
  mkcl_object z;
  mkcl_type tx;

  mkcl_call_stack_check(env);
  MKCL_MATHERR_CLEAR;
 AGAIN:
  tx = mkcl_type_of(x);
  if (!MKCL_NUMBER_TYPE_P(tx)) {
    x = mkcl_type_error(env, @'sqrt',"argument",x,@'number');
    goto AGAIN;
  }
  if (tx == mkcl_t_complex) {
    z = mkcl_make_ratio(env, MKCL_MAKE_FIXNUM(1), MKCL_MAKE_FIXNUM(2));
    z = mk_cl_expt(env, x, z);
  } else if (mkcl_minusp(env, x)) {
    z = mkcl_make_complex(env, MKCL_MAKE_FIXNUM(0), mk_cl_sqrt(env, mkcl_negate(env, x)));
  } else switch (mkcl_type_of(x)) {
    case mkcl_t_fixnum:
    case mkcl_t_bignum:
    case mkcl_t_ratio:
      z = mkcl_make_singlefloat(env, sqrtf(mkcl_to_double(env, x))); break;
    case mkcl_t_singlefloat:
      z = mkcl_make_singlefloat(env, sqrtf(mkcl_single_float(x))); break;
    case mkcl_t_doublefloat:
      z = mkcl_make_doublefloat(env, sqrt(mkcl_double_float(x))); break;
#ifdef MKCL_LONG_FLOAT
    case mkcl_t_longfloat:
      z = mkcl_make_longfloat(env, sqrtl(mkcl_long_float(x))); break;
#endif
    default:
      /* Never reaches this */
      z = mk_cl_Cnil; /* Just to silence the compiler. */
      ;
    }
  MKCL_MATHERR_TEST(env);
  @(return z);
}

static double
mkcl_atan2_double(double y, double x)
{
  if (signbit(x)) {
    if (signbit(y)) {
      return -MKCL_PI_D + atan(-y / -x);
    } else if (y == 0) {
      return MKCL_PI_D;
    } else {
      return MKCL_PI_D - atan(y / -x);
    }
  } else if (x == 0) {
    if (signbit(y)) {
      return -MKCL_PI2_D;
    } else if (y == 0) {
      return x / y;  /* Produces a NaN */
    } else {
      return MKCL_PI2_D;
    }
  } else {
    if (signbit(y)) {
      return -atan(-y / x);
    } else if (y == 0) {
      return (double)0;
    } else {
      return atan(y / x);
    }
  }
}

#ifdef MKCL_LONG_FLOAT
static long double
mkcl_atan2_long_double(long double y, long double x)
{
  if (signbit(x)) {
    if (signbit(y)) {
      return -MKCL_PI_L + atanl(-y / -x);
    } else if (y == 0) {
      return MKCL_PI_L;
    } else {
      return MKCL_PI_L - atanl(y / -x);
    }
  } else if (x == 0) {
    if (signbit(y)) {
      return -MKCL_PI2_L;
    } else if (y == 0) {
      return x / y;  /* Produces a NaN */
    } else {
      return MKCL_PI2_L;
    }
  } else {
    if (signbit(y)) {
      return -atanl(-y / x);
    } else if (y == 0) {
      return (long double)0;
    } else {
      return atanl(y / x);
    }
  }
}
#endif

mkcl_object
mkcl_atan2(MKCL, mkcl_object y, mkcl_object x)
{
  mkcl_object output;
  MKCL_MATHERR_CLEAR;
  {
#ifdef MKCL_LONG_FLOAT
    int tx = mkcl_type_of(x);
    int ty = mkcl_type_of(y);
    if (tx < ty)
      tx = ty;
    if (tx == mkcl_t_longfloat) {
      long double d = mkcl_atan2_long_double(mkcl_to_long_double(env, y),
					     mkcl_to_long_double(env, x));
      output = mkcl_make_longfloat(env, d);
    } else {
      double dx = mkcl_to_double(env, x);
      double dy = mkcl_to_double(env, y);
      double dz = mkcl_atan2_double(dy, dx);
      if (tx == mkcl_t_doublefloat) {
	output = mkcl_make_doublefloat(env, dz);
      } else {
	output = mkcl_make_singlefloat(env, dz);
      }
    }
#else /* def MKCL_LONG_FLOAT */
    double dy = mkcl_to_double(env, y);
    double dx = mkcl_to_double(env, x);
    double dz = mkcl_atan2_double(dy, dx);
    if (mkcl_type_of(x) == mkcl_t_doublefloat || mkcl_type_of(y) == mkcl_t_doublefloat) {
      output = mkcl_make_doublefloat(env, dz);
    } else {
      output = mkcl_make_singlefloat(env, dz);
    }
#endif /* def MKCL_LONG_FLOAT */
  }
  MKCL_MATHERR_TEST(env);
  return output;
}

mkcl_object
mkcl_atan1(MKCL, mkcl_object y)
{
  if (mkcl_type_of(y) == mkcl_t_complex) {
    mkcl_object z1, z = mkcl_times(env, mkcl_core.imag_unit, y);
    z = mkcl_one_plus(env, z);
    z1 = mkcl_times(env, y, y);
    z1 = mkcl_one_plus(env, z1);
    z1 = mk_cl_sqrt(env, z1);
    z = mkcl_divide(env, z, z1);
    z = mkcl_log1(env, z);
    z = mkcl_times(env, mkcl_core.minus_imag_unit, z);
    return z;
  } else {
    return mkcl_atan2(env, y, MKCL_MAKE_FIXNUM(1));
  }
}

mkcl_object
mk_cl_sin(MKCL, mkcl_object x)
{
  mkcl_object output;

  mkcl_call_stack_check(env);
  MKCL_MATHERR_CLEAR;
 AGAIN:
  switch (mkcl_type_of(x)) {
  case mkcl_t_fixnum:
  case mkcl_t_bignum:
  case mkcl_t_ratio:
    output = mkcl_make_singlefloat(env, sinf(mkcl_to_double(env, x))); break;
  case mkcl_t_singlefloat:
    output = mkcl_make_singlefloat(env, sinf(mkcl_single_float(x))); break;
  case mkcl_t_doublefloat:
    output = mkcl_make_doublefloat(env, sin(mkcl_double_float(x))); break;
#ifdef MKCL_LONG_FLOAT
  case mkcl_t_longfloat:
    output = mkcl_make_longfloat(env, sinl(mkcl_long_float(x))); break;
#endif
  case mkcl_t_complex: {
    /*
      z = x + I y
      z = x + I y
      sin(z) = sinh(I z) = sinh(-y + I x)
    */
    mkcl_object dx = x->_complex.real;
    mkcl_object dy = x->_complex.imag;
    mkcl_object a = mkcl_times(env, mk_cl_sin(env, dx), mk_cl_cosh(env, dy));
    mkcl_object b = mkcl_times(env, mk_cl_cos(env, dx), mk_cl_sinh(env, dy));
    output = mkcl_make_complex(env, a, b);
    break;
  }
  default:
    x = mkcl_type_error(env, @'sin',"argument",x,@'number');
    goto AGAIN;
  }
  MKCL_MATHERR_TEST(env);
  @(return output);
}

mkcl_object
mk_cl_cos(MKCL, mkcl_object x)
{
  mkcl_object output;

  mkcl_call_stack_check(env);
  MKCL_MATHERR_CLEAR;
 AGAIN:
  switch (mkcl_type_of(x)) {
  case mkcl_t_fixnum:
  case mkcl_t_bignum:
  case mkcl_t_ratio:
    output = mkcl_make_singlefloat(env, cosf(mkcl_to_double(env, x))); break;
  case mkcl_t_singlefloat:
    output = mkcl_make_singlefloat(env, cosf(mkcl_single_float(x))); break;
  case mkcl_t_doublefloat:
    output = mkcl_make_doublefloat(env, cos(mkcl_double_float(x))); break;
#ifdef MKCL_LONG_FLOAT
  case mkcl_t_longfloat:
    output = mkcl_make_longfloat(env, cosl(mkcl_long_float(x))); break;
#endif
  case mkcl_t_complex: {
    /*
      z = x + I y
      cos(z) = cosh(I z) = cosh(-y + I x)
    */
    mkcl_object dx = x->_complex.real;
    mkcl_object dy = x->_complex.imag;
    mkcl_object a = mkcl_times(env, mk_cl_cos(env, dx), mk_cl_cosh(env, dy));
    mkcl_object b = mkcl_times(env,
			    mkcl_negate(env, mk_cl_sin(env, dx)),
			    mk_cl_sinh(env, dy));
    output = mkcl_make_complex(env, a, b);
    break;
  }
  default:
    x = mkcl_type_error(env, @'cos',"argument",x,@'number');
    goto AGAIN;
  }
  MKCL_MATHERR_TEST(env);
  @(return output);
}

/*
 * As of 2006-10-13 I found this bug in GLIBC's tanf, which overflows
 * when the argument is pi/4. It is 2008 and this has not yet been
 * solved. Not only that, but if we use tan() on float, GCC automatically
 * and stupidly forces the use of tanf().
 */
#if defined(__amd64__) && defined(__GLIBC__)
static double safe_tanf(double x) { return tan(x); }
#else
# define safe_tanf(x) tanf(x)
#endif

mkcl_object
mk_cl_tan(MKCL, mkcl_object x)
{
  mkcl_object output;

  mkcl_call_stack_check(env);
  MKCL_MATHERR_CLEAR;
 AGAIN:
  switch (mkcl_type_of(x)) {
  case mkcl_t_fixnum:
  case mkcl_t_bignum:
  case mkcl_t_ratio:
    output = mkcl_make_singlefloat(env, safe_tanf(mkcl_to_double(env, x))); break;
  case mkcl_t_singlefloat:
    output = mkcl_make_singlefloat(env, safe_tanf(mkcl_single_float(x))); break;
  case mkcl_t_doublefloat:
    output = mkcl_make_doublefloat(env, tan(mkcl_double_float(x))); break;
#ifdef MKCL_LONG_FLOAT
  case mkcl_t_longfloat:
    output = mkcl_make_longfloat(env, tanl(mkcl_long_float(x))); break;
#endif
  case mkcl_t_complex: {
    mkcl_object a = mk_cl_sin(env, x);
    mkcl_object b = mk_cl_cos(env, x);
    output = mkcl_divide(env, a, b);
    break;
  }
  default:
    x = mkcl_type_error(env, @'tan',"argument",x,@'number');
    goto AGAIN;
  }
  MKCL_MATHERR_TEST(env);
  @(return output);
}

mkcl_object
mk_cl_sinh(MKCL, mkcl_object x)
{
  mkcl_object output;

  mkcl_call_stack_check(env);
  MKCL_MATHERR_CLEAR;
 AGAIN:
  switch (mkcl_type_of(x)) {
  case mkcl_t_fixnum:
  case mkcl_t_bignum:
  case mkcl_t_ratio:
    output = mkcl_make_singlefloat(env, sinhf(mkcl_to_double(env, x))); break;
  case mkcl_t_singlefloat:
    output = mkcl_make_singlefloat(env, sinhf(mkcl_single_float(x))); break;
  case mkcl_t_doublefloat:
    output = mkcl_make_doublefloat(env, sinh(mkcl_double_float(x))); break;
#ifdef MKCL_LONG_FLOAT
  case mkcl_t_longfloat:
    output = mkcl_make_longfloat(env, sinhl(mkcl_long_float(x))); break;
#endif
  case mkcl_t_complex: {
    /*
      z = x + I y
      sinh(z) = (exp(z)-exp(-z))/2
      = (exp(x)*(cos(y)+Isin(y))-exp(-x)*(cos(y)-Isin(y)))/2
      = sinh(x)*cos(y) + Icosh(x)*sin(y);
    */
    mkcl_object dx = x->_complex.real;
    mkcl_object dy = x->_complex.imag;
    mkcl_object a = mkcl_times(env, mk_cl_sinh(env, dx), mk_cl_cos(env, dy));
    mkcl_object b = mkcl_times(env, mk_cl_cosh(env, dx), mk_cl_sin(env, dy));
    output = mkcl_make_complex(env, a, b);
    break;
  }
  default:
    x = mkcl_type_error(env, @'sinh',"argument",x,@'number');
    goto AGAIN;
  }
  MKCL_MATHERR_TEST(env);
  @(return output);
}

mkcl_object
mk_cl_cosh(MKCL, mkcl_object x)
{
  mkcl_object output;

  mkcl_call_stack_check(env);
  MKCL_MATHERR_CLEAR;
 AGAIN:
  switch (mkcl_type_of(x)) {
  case mkcl_t_fixnum:
  case mkcl_t_bignum:
  case mkcl_t_ratio:
    output = mkcl_make_singlefloat(env, coshf(mkcl_to_double(env, x))); break;
  case mkcl_t_singlefloat:
    output = mkcl_make_singlefloat(env, coshf(mkcl_single_float(x))); break;
  case mkcl_t_doublefloat:
    output = mkcl_make_doublefloat(env, cosh(mkcl_double_float(x))); break;
#ifdef MKCL_LONG_FLOAT
  case mkcl_t_longfloat:
    output = mkcl_make_longfloat(env, coshl(mkcl_long_float(x))); break;
#endif
  case mkcl_t_complex: {
    /*
      z = x + I y
      cosh(z) = (exp(z)+exp(-z))/2
      = (exp(x)*(cos(y)+Isin(y))+exp(-x)*(cos(y)-Isin(y)))/2
      = cosh(x)*cos(y) + Isinh(x)*sin(y);
    */
    mkcl_object dx = x->_complex.real;
    mkcl_object dy = x->_complex.imag;
    mkcl_object a = mkcl_times(env, mk_cl_cosh(env, dx), mk_cl_cos(env, dy));
    mkcl_object b = mkcl_times(env, mk_cl_sinh(env, dx), mk_cl_sin(env, dy));
    output = mkcl_make_complex(env, a, b);
    break;
  }
  default:
    x = mkcl_type_error(env, @'cosh',"argument",x,@'number');
    goto AGAIN;
  }
  MKCL_MATHERR_TEST(env);
  @(return output);
}

mkcl_object
mk_cl_tanh(MKCL, mkcl_object x)
{
  mkcl_object output;

  mkcl_call_stack_check(env);
  MKCL_MATHERR_CLEAR;
 AGAIN:
  switch (mkcl_type_of(x)) {
  case mkcl_t_fixnum:
  case mkcl_t_bignum:
  case mkcl_t_ratio:
    output = mkcl_make_singlefloat(env, tanhf(mkcl_to_double(env, x))); break;
  case mkcl_t_singlefloat:
    output = mkcl_make_singlefloat(env, tanhf(mkcl_single_float(x))); break;
  case mkcl_t_doublefloat:
    output = mkcl_make_doublefloat(env, tanh(mkcl_double_float(x))); break;
#ifdef MKCL_LONG_FLOAT
  case mkcl_t_longfloat:
    output = mkcl_make_longfloat(env, tanhl(mkcl_long_float(x))); break;
#endif
  case mkcl_t_complex: {
    mkcl_object a = mk_cl_sinh(env, x);
    mkcl_object b = mk_cl_cosh(env, x);
    output = mkcl_divide(env, a, b);
    break;
  }
  default:
    x = mkcl_type_error(env, @'tanh',"argument",x,@'number');
    goto AGAIN;
  }
  MKCL_MATHERR_TEST(env);
  @(return output);
}

@(defun log (x &optional (y MKCL_OBJNULL))
@
  /* INV: type check in mkcl_log1() and mkcl_log2() */
  if (y == MKCL_OBJNULL)
    { @(return mkcl_log1(env, x)); }
  @(return mkcl_log2(env, y, x));
@)

@(defun atan (x &optional (y MKCL_OBJNULL))
@
  /* INV: type check in mkcl_atan() & mkcl_atan2() */
  /* FIXME mkcl_atan() and mkcl_atan2() produce generic errors
     without recovery and function information. */
  if (y == MKCL_OBJNULL)
    { @(return mkcl_atan1(env, x)); }
  @(return mkcl_atan2(env, x, y));
@)

