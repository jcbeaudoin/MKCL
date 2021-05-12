/* -*- mode: c -*- */
/*
    number.c -- Numeric constants.
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
#include <mkcl/mkcl-math.h>
#include <float.h>
#include <mkcl/internal.h>
#include <mkcl/mkcl-fenv.h>

#if defined(MKCL_IEEE_FP) && defined(HAVE_FEENABLEEXCEPT)
/*
 * We are using IEEE arithmetics and can rely on FPE exceptions
 * to be raised when invalid operations are performed.
 */
# define DO_DETECT_FPE(e, f) mkcl_detect_fpe()
#else
/*
 * Either we can not rely on C signals or we do not want IEEE NaNs and
 * infinities. The first case typically happens for instance under OS
 * X, where the status of the FPE control word is changed by
 * printf. We have two alternatives.
 */
# ifdef MKCL_IEEE_FP
#  if defined(HAVE_FENV_H) && !defined(MKCL_AVOID_FENV_H)
#   define DO_DETECT_FPE(e, f)						\
  if (isnan(f) || !isfinite(f))						\
    mkcl_deliver_fpe(e);
#  else
#   define DO_DETECT_FPE(e, f)						\
  if (isnan(f)) {							\
    if ((e)->fpe_control_bits & FE_INVALID)				\
      mk_cl_error(e,1, MK_CL_floating_point_invalid_operation);        \
  } else if (!isfinite(f)) {						\
    if ((e)->fpe_control_bits & FE_DIVBYZERO)				\
      mk_cl_error(e,1, MK_CL_division_by_zero);                        \
  }
#  endif
# else
#  define DO_DETECT_FPE(e, f)						\
  if (isnan(f)) {							\
    mk_cl_error(e, 1, MK_CL_floating_point_invalid_operation);		\
  } else if (!isfinite(f)) {						\
    mk_cl_error(e, 1, MK_CL_division_by_zero);				\
  }
# endif
#endif

mkcl_word
mkcl_integer_to_word(MKCL, mkcl_object x)
{
  if (MKCL_FIXNUMP(x))
    return mkcl_fixnum_to_word(x);
  if (mkcl_type_of(x) == mkcl_t_bignum) {
#if MKCL_LONG_BITS >= MKCL_WORD_BITS
    if (mpz_fits_slong_p(x->big.big_num)) {
      return mpz_get_si(x->big.big_num);
    }
#elif MKCL_WORD_BITS == 64
    return mkcl_to_int64_t(env, x);
#else
#error "Don't know how to convert from lisp bignum to native C integer."
#endif
  }
#if MKCL_WORD_BITS == 64
  mkcl_FEwrong_type_argument(env, MK_MKCL_integer64, x);
#elif MKCL_WORD_BITS == 32
  mkcl_FEwrong_type_argument(env, MK_MKCL_integer32, x);
#else
  mkcl_FEwrong_type_argument(env, MK_CL_integer, x);
#endif
}

mkcl_index
mkcl_integer_to_index(MKCL, mkcl_object x)
{
  if (MKCL_FIXNUMP(x)) {
    mkcl_word i = mkcl_fixnum_to_word(x);
    if (i >= 0)
      return i;
  } else if (mkcl_type_of(x) == mkcl_t_bignum) {
#if MKCL_LONG_BITS >= MKCL_WORD_BITS
    if (mpz_fits_ulong_p(x->big.big_num)) {
      return mpz_get_ui(x->big.big_num);
    }
#elif MKCL_WORD_BITS == 64
    return mkcl_to_uint64_t(env, x);
#else
#error "Don't know how to convert from lisp bignum to native C unsigned integer."
#endif
  }
  mk_cl_error(env, 9, MK_CL_simple_type_error, MK_KEY_format_control,
	      mkcl_make_simple_base_string(env, "Not a non-negative fixnum ~S"),
	      MK_KEY_format_arguments, mk_cl_list(env, 1,x),
	      MK_KEY_expected_type,
#if MKCL_WORD_BITS == 64
	      MK_MKCL_natural64,
#elif MKCL_WORD_BITS == 32
	      MK_MKCL_natural32,
#else
	      mk_cl_list(env, 3,
			 MK_CL_integer,
			 MKCL_MAKE_FIXNUM(0),
			 mkcl_one_minus(env, mkcl_ash(env,
						      MKCL_MAKE_FIXNUM(1),
						      MKCL_WORD_BITS))
			 ),
#endif
	      MK_KEY_datum, x);
}

mkcl_word
mkcl_fixnum_in_range(MKCL, mkcl_object fun, const char *what, mkcl_object value,
		     mkcl_word min, mkcl_word max)
{
  do {
    if (MKCL_FIXNUMP(value)) {
      mkcl_word output = mkcl_fixnum_to_word(value);
      if ((min <= output) && (output <= max)) {
	return output;
      }
    }
    value = mkcl_type_error(env, fun, what, value,
			    mk_cl_list(env, 3,
				       MK_CL_integer,
				       MKCL_MAKE_FIXNUM(min),
				       MKCL_MAKE_FIXNUM(max)));
  } while(1);
}

mkcl_object mkcl_make_big_integer(MKCL, mkcl_word l)
{
#if MKCL_WORD_BITS > MKCL_LONG_BITS
  return mkcl_make_int64_t(env, l);
#else
  mkcl_object z = _mkcl_big_register0();
  _mkcl_big_set_si(z, l);
  return _mkcl_big_register_copy(env, z);
#endif
}

mkcl_object mkcl_make_big_unsigned_integer(MKCL, mkcl_index l)
{
#if MKCL_WORD_BITS > MKCL_LONG_BITS
  return mkcl_make_uint64_t(env, l);
#else
  mkcl_object z = _mkcl_big_register0();
  _mkcl_big_set_ui(z, l);
  return _mkcl_big_register_copy(env, z);
#endif
}

mkcl_uint8_t
mkcl_to_uint8_t(MKCL, mkcl_object x) 
{
  do {
    if (MKCL_FIXNUMP(x)) {
      mkcl_word y = mkcl_fixnum_to_word(x);
      if (y >= 0 && y <= UINT8_MAX) {
	return (uint8_t)y;
      }
    }
    x = mkcl_type_error(env, MK_CL_coerce, "variable", x,
			mk_cl_list(env, 3, MK_CL_integer, MKCL_MAKE_FIXNUM(0), MKCL_MAKE_FIXNUM(UINT8_MAX)));
  } while(1);
}

mkcl_int8_t
mkcl_to_int8_t(MKCL, mkcl_object x)
{
  do {
    if (MKCL_FIXNUMP(x)) {
      mkcl_word y = mkcl_fixnum_to_word(x);
      if (y >= INT8_MIN && y <= INT8_MAX) {
	return (int8_t)y;
      }
    }
    x = mkcl_type_error(env, MK_CL_coerce, "variable", x,
			mk_cl_list(env, 3, MK_CL_integer, MKCL_MAKE_FIXNUM(INT8_MIN), MKCL_MAKE_FIXNUM(INT8_MAX)));
  } while(1);
}

#if MKCL_WORD_BITS < 32
# error "Unsupported platform with mkcl_word < mkcl_uint32_t"
#endif

mkcl_uint16_t
mkcl_to_uint16_t(MKCL, mkcl_object x)
{
  do {
    if (MKCL_FIXNUMP(x)) {
      mkcl_word y = mkcl_fixnum_to_word(x);
      if (y >= 0 && y <= UINT16_MAX) {
	return (mkcl_uint16_t)y;
      }
    }
    x = mkcl_type_error(env, MK_CL_coerce, "variable", x,
			mk_cl_list(env, 3, MK_CL_integer, MKCL_MAKE_FIXNUM(0), MKCL_MAKE_FIXNUM(UINT16_MAX)));
  } while(1);
}

mkcl_int16_t
mkcl_to_int16_t(MKCL, mkcl_object x)
{
  do {
    if (MKCL_FIXNUMP(x)) {
      mkcl_word y = mkcl_fixnum_to_word(x);
      if (y >= INT16_MIN && y <= INT16_MAX) {
	return (mkcl_int16_t)y;
      }
    }
    x = mkcl_type_error(env, MK_CL_coerce, "variable", x,
			mk_cl_list(env, 3, MK_CL_integer, MKCL_MAKE_FIXNUM(INT16_MIN), MKCL_MAKE_FIXNUM(INT16_MAX)));
  } while(1);
}

#if (MKCL_WORD_BITS > 32)
mkcl_uint32_t
mkcl_to_uint32_t(MKCL, mkcl_object x)
{
  do {
    if (MKCL_FIXNUMP(x)) {
      mkcl_word y = mkcl_fixnum_to_word(x);
      if (y >= 0 && y <= UINT32_MAX) {
	return (mkcl_uint32_t)y;
      }
    }
    x = mkcl_type_error(env, MK_CL_coerce, "variable", x,
			mk_cl_list(env, 3, MK_CL_integer, MKCL_MAKE_FIXNUM(0), mkcl_make_unsigned_integer(env, UINT32_MAX)));
  } while(1);
}

mkcl_int32_t
mkcl_to_int32_t(MKCL, mkcl_object x)
{
  do {
    if (MKCL_FIXNUMP(x)) {
      mkcl_word y = mkcl_fixnum_to_word(x);
      if (y >= INT32_MIN && y <= INT32_MAX) {
	return (mkcl_int32_t)y;
      }
    }
    x = mkcl_type_error(env, MK_CL_coerce, "variable", x,
			mk_cl_list(env, 3,
				   MK_CL_integer,
				   mkcl_make_integer(env, INT32_MIN),
				   mkcl_make_integer(env, INT32_MAX)));
  } while(1);
}
#endif /* (MKCL_WORD_BITS > 32) */

#if (MKCL_WORD_BITS < 64) || (MKCL_LONG_BITS < 64)
mkcl_uint64_t
mkcl_to_uint64_t(MKCL, mkcl_object x)
{
  do {
    if (!mkcl_minusp(env, x)) {
      if (MKCL_FIXNUMP(x)) {
	return  mkcl_fixnum_to_word(x);
      } else if (mkcl_type_of(x) != mkcl_t_bignum) {
	(void)0;
      } else if (mpz_fits_ulong_p(x->big.big_num)) {
	return mpz_get_ui(x->big.big_num);
      } else {
	mkcl_object copy = _mkcl_big_register0();
	mpz_fdiv_q_2exp(copy->big.big_num, x->big.big_num, 32);
	if (mpz_fits_ulong_p(copy->big.big_num)) {
	  mkcl_uint64_t output = mpz_get_ui(copy->big.big_num);
	  output = (output << 32) + mpz_get_ui(x->big.big_num);
	  return output;
	}
      }
    }
    x = mkcl_type_error(env, MK_CL_coerce, "variable", x,
			mk_cl_list(env, 3,MK_CL_integer,MKCL_MAKE_FIXNUM(0),
				   mkcl_one_minus(env, mkcl_ash(env, MKCL_MAKE_FIXNUM(1), 64))));
  } while(1);
}

mkcl_int64_t
mkcl_to_int64_t(MKCL, mkcl_object x)
{
  do {
    if (MKCL_FIXNUMP(x)) {
      return mkcl_fixnum_to_word(x);
    } else if (mkcl_type_of(x) != mkcl_t_bignum) {
      (void)0;
    } else if (mpz_fits_slong_p(x->big.big_num)) {
      return mpz_get_si(x->big.big_num);
    } else {
      mkcl_object copy = _mkcl_big_register0();
      mpz_fdiv_q_2exp(copy->big.big_num, x->big.big_num, 32);
      if (mpz_fits_slong_p(copy->big.big_num)) {
	mkcl_int64_t output =  mpz_get_si(copy->big.big_num);
	mpz_fdiv_r_2exp(copy->big.big_num, x->big.big_num, 32);
	return (output << 32) + mpz_get_ui(copy->big.big_num);
      }
    }
    x = mkcl_type_error(env, MK_CL_coerce, "variable", x,
			mk_cl_list(env, 3,MK_CL_integer,
				   mkcl_negate(env, mkcl_ash(env, MKCL_MAKE_FIXNUM(1), 63)),
				   mkcl_one_minus(env, mkcl_ash(env, MKCL_MAKE_FIXNUM(1), 63))));
  } while(1);
}

mkcl_object
mkcl_make_uint64_t(MKCL, mkcl_uint64_t i)
{
  if (i <= MKCL_MOST_POSITIVE_FIXNUM) {
    return MKCL_MAKE_FIXNUM(i);
  } else if (i <= ~((mkcl_uint32_t)0)) {
    return mkcl_make_uint32_t(env, i);
  } else {
    mkcl_object aux = mkcl_make_uint32_t(env, i >> 32);
    return mk_cl_logior(env, 2, mkcl_ash(env, aux, 32), mkcl_make_uint32_t(env, (mkcl_uint32_t)i));
  }
}

mkcl_object
mkcl_make_int64_t(MKCL, mkcl_int64_t i)
{
  if (i >= MKCL_MOST_NEGATIVE_FIXNUM && i <= MKCL_MOST_POSITIVE_FIXNUM) {
    return MKCL_MAKE_FIXNUM(i);
  } else {
    mkcl_object aux = mkcl_make_int32_t(env, i >> 32);
    mkcl_object aux0 = mkcl_ash(env, aux, 32);
    mkcl_object aux1 = mkcl_make_uint32_t(env, (mkcl_uint32_t)i);
    mkcl_object aux2 = mk_cl_logior(env, 2, aux0, aux1);

    return aux2;
  }
}
#endif /* (MKCL_WORD_BITS < 64) */

mkcl_ulong_long_t
mkcl_to_ulong_long(MKCL, mkcl_object x) {
  return (mkcl_ulong_long_t)mkcl_to_uint64_t(env, x);
}
mkcl_long_long_t
mkcl_to_long_long(MKCL, mkcl_object x) {
  return (mkcl_long_long_t)mkcl_to_int64_t(env, x);
}
mkcl_object
mkcl_make_ulong_long(MKCL, mkcl_ulong_long_t i) {
  return mkcl_make_uint64_t(env, i);
}
mkcl_object
mkcl_make_long_long(MKCL, mkcl_long_long_t i) {
  return mkcl_make_int64_t(env, i);
}


mkcl_object
mkcl_make_ratio(MKCL, mkcl_object num, mkcl_object den)
{
  mkcl_object g, r;

  /* INV: the arguments NUM & DEN are integers */
  if (den == MKCL_MAKE_FIXNUM(0))
    mkcl_FEdivision_by_zero(env, num, den);
  if (num == MKCL_MAKE_FIXNUM(0) || den == MKCL_MAKE_FIXNUM(1))
    return(num);
  if (mkcl_minusp(env, den)) {
    num = mkcl_negate(env, num);
    den = mkcl_negate(env, den);
  }
  g = mkcl_gcd(env, num, den);
  if (g != MKCL_MAKE_FIXNUM(1)) {
    num = mkcl_integer_divide(env, num, g);
    den = mkcl_integer_divide(env, den, g);
  }
  if (den == MKCL_MAKE_FIXNUM(1))
    return num;
  if (den == MKCL_MAKE_FIXNUM(-1))
    return mkcl_negate(env, num);
  r = mkcl_alloc_raw_ratio(env);
  r->ratio.num = num;
  r->ratio.den = den;
  return(r);
}

#if defined(HAVE_FENV_H) && !defined(MKCL_AVOID_FENV_H)
void
mkcl_deliver_fpe(MKCL)
{
  int bits = env->fpe_control_bits;
  if (fetestexcept(env->fpe_control_bits)) {
    mkcl_object condition;
    if (fetestexcept(bits & FE_DIVBYZERO))
      condition = MK_CL_division_by_zero;
    else if (fetestexcept(bits & FE_INVALID))
      condition = MK_CL_floating_point_invalid_operation;
    else if (fetestexcept(bits & FE_OVERFLOW))
      condition = MK_CL_floating_point_overflow;
    else if (fetestexcept(bits & FE_UNDERFLOW))
      condition = MK_CL_floating_point_underflow;
    else if (fetestexcept(bits & FE_INEXACT))
      condition = MK_CL_floating_point_inexact;
    else
      condition = MK_CL_arithmetic_error;
    feclearexcept(FE_ALL_EXCEPT);
    mk_cl_error(env, 1, condition);
  }
  feclearexcept(FE_ALL_EXCEPT);
}
#endif

mkcl_object
mkcl_make_singlefloat(MKCL, float f)
{
  mkcl_object x;

  DO_DETECT_FPE(env, f); /* What the F!!! for? JCB */
  if (f == 0.0F) {
#if defined(MKCL_SIGNED_ZERO)
    if (signbit(f))
      return mkcl_core.singlefloat_minus_zero;
#endif
    return mkcl_core.singlefloat_zero;
  }
  x = mkcl_alloc_raw_singlefloat(env);
  mkcl_single_float(x) = f;
  return(x);
}

mkcl_object
mkcl_make_doublefloat(MKCL, double f)
{
  mkcl_object x;

  DO_DETECT_FPE(env, f); /* What the F!!! for? JCB */
  if (f == 0.0) {
#if defined(MKCL_SIGNED_ZERO)
    if (signbit(f))
      return mkcl_core.doublefloat_minus_zero;
#endif
    return mkcl_core.doublefloat_zero;
  }
  x = mkcl_alloc_raw_doublefloat(env);
  mkcl_double_float(x) = f;
  return(x);
}

#ifdef MKCL_LONG_FLOAT
mkcl_object
mkcl_make_longfloat(MKCL, long double f)
{
  mkcl_object x;

  DO_DETECT_FPE(env, f); /* What the F!!! for? JCB */
  if (f == 0.0L) {
#if defined(MKCL_SIGNED_ZERO)
    if (signbit(f))
      return mkcl_core.longfloat_minus_zero;
#endif
    return mkcl_core.longfloat_zero;
  }
  x = mkcl_alloc_raw_longfloat(env);
  x->longfloat.value = f;
  return x;
}
#else
mkcl_object
mkcl_make_longfloat(MKCL, long double f)
{
  return mkcl_make_doublefloat(env, f); /* downcast. */
}
#endif

mkcl_object
mkcl_make_complex(MKCL, mkcl_object r, mkcl_object i)
{
  mkcl_object c;
  mkcl_type ti;
 AGAIN:
  ti = mkcl_type_of(i);
  /* Both R and I are promoted to a common type */
  switch (mkcl_type_of(r)) {
  case mkcl_t_fixnum:
  case mkcl_t_bignum:
  case mkcl_t_ratio:
    switch (ti) {
    case mkcl_t_fixnum:
      if (i == MKCL_MAKE_FIXNUM(0))
	return(r);
    case mkcl_t_bignum:
    case mkcl_t_ratio:
      break;
    case mkcl_t_singlefloat:
      r = mkcl_make_singlefloat(env, (float)mkcl_to_double(env, r));
      break;
    case mkcl_t_doublefloat:
      r = mkcl_make_doublefloat(env, mkcl_to_double(env, r));
      break;
#ifdef MKCL_LONG_FLOAT
    case mkcl_t_longfloat:
      r = mkcl_make_longfloat(env, mkcl_to_double(env, r));
      break;
#endif
    default:
      i = mkcl_type_error(env, MK_CL_complex,"imaginary part", i, MK_CL_real);
      goto AGAIN;
    }
    break;
  case mkcl_t_singlefloat:
    switch (ti) {
    case mkcl_t_fixnum:
    case mkcl_t_bignum:
    case mkcl_t_ratio:
      i = mkcl_make_singlefloat(env, (float)mkcl_to_double(env, i));
      break;
    case mkcl_t_singlefloat:
      break;
    case mkcl_t_doublefloat:
      r = mkcl_make_doublefloat(env, (double)(mkcl_single_float(r)));
      break;
#ifdef MKCL_LONG_FLOAT
    case mkcl_t_longfloat:
      r = mkcl_make_longfloat(env, (long double)mkcl_single_float(r));
      break;
#endif
    default:
      i = mkcl_type_error(env, MK_CL_complex,"imaginary part", i, MK_CL_real);
      goto AGAIN;
    }
    break;
  case mkcl_t_doublefloat:
    switch (ti) {
    case mkcl_t_fixnum:
    case mkcl_t_bignum:
    case mkcl_t_ratio:
    case mkcl_t_singlefloat:
      i = mkcl_make_doublefloat(env, mkcl_to_double(env, i));
    case mkcl_t_doublefloat:
      break;
#ifdef MKCL_LONG_FLOAT
    case mkcl_t_longfloat:
      r = mkcl_make_longfloat(env, (long double)mkcl_double_float(r));
      break;
#endif
    default:
      i = mkcl_type_error(env, MK_CL_complex,"imaginary part", i, MK_CL_real);
      goto AGAIN;
    }
    break;
#ifdef MKCL_LONG_FLOAT
  case mkcl_t_longfloat:
    if (ti != mkcl_t_longfloat)
      i = mkcl_make_longfloat(env, (long double)mkcl_to_double(env, i));
    break;
#endif
  default:
    r = mkcl_type_error(env, MK_CL_complex,"real part", r, MK_CL_real);
    goto AGAIN;

  }
  c = mkcl_alloc_raw_complex(env);
  c->_complex.real = r;
  c->_complex.imag = i;
  return(c);
}

static mkcl_object
into_bignum(MKCL, mkcl_object bignum, mkcl_object integer)
{
  if (MKCL_FIXNUMP(integer)) {
#if MKCL_LONG_BITS < MKCL_WORD_BITS
    _mkcl_big_set_fixnum(bignum, mkcl_fixnum_to_word(integer));
#else
    mpz_set_si(bignum->big.big_num, mkcl_fixnum_to_word(integer));
#endif
  } else if (MKCL_BIGNUMP(integer)) {
    mpz_set(bignum->big.big_num, integer->big.big_num);
  } else mkcl_FEtype_error_integer(env, integer);
  return bignum;
}

static mkcl_word
remove_zeros(MKCL, mkcl_object *integer)
{
  mkcl_object buffer = into_bignum(env, _mkcl_big_register0(), *integer);
  unsigned long den_twos = mpz_scan1(buffer->big.big_num, 0);
  if (den_twos < ULONG_MAX) {
    mpz_div_2exp(buffer->big.big_num, buffer->big.big_num, den_twos);
    *integer = _mkcl_big_register_normalize(env, buffer);
    return -den_twos;
  } else {
    _mkcl_big_register_free(env, buffer);
    return 0;
  }
}

static mkcl_object
prepare_ratio_to_float(MKCL, mkcl_object num, mkcl_object den,
		       int digits, mkcl_word *scaleout)
{
  /* We have to cook our own routine because GMP does not round.
   * The recipe is simple: we multiply the numberator by a large
   * enough number so that the division by the denominator fits
   * the floating point number. The result is scaled back by the
   * appropriate exponent.
   */
  /* Scale down the denominator, eliminating the zeros
   * so that we have smaller operands.
   */
  mkcl_word scale = remove_zeros(env, &den);
  mkcl_word num_size, delta;
  num_size = mkcl_integer_length(env, num);
  delta = mkcl_integer_length(env, den) - num_size;
  scale -= delta;
  {
    mkcl_word adjust = digits + delta + 1;

    if (adjust > 0)
      num = mkcl_ash(env, num, adjust);
    else if (adjust < 0)
      den = mkcl_ash(env, den, -adjust);
  }

  do {
    mkcl_object fraction = mkcl_truncate2(env, num, den);
    mkcl_object rem = MKCL_VALUES(1);
    mkcl_word len = mkcl_integer_length(env, fraction);
    if ((len - digits) == 1) {
      if (mkcl_oddp(env, fraction)) {
	mkcl_object one = mkcl_minusp(env, num) ? MKCL_MAKE_FIXNUM(-1) : MKCL_MAKE_FIXNUM(1);

	if (rem == MKCL_MAKE_FIXNUM(0)) {
	  if (!mkcl_Null(mk_cl_logbitp(env, MKCL_MAKE_FIXNUM(1), fraction)))
	    fraction = mkcl_plus(env, fraction, one);
	} else {
	  fraction = mkcl_plus(env, fraction, one);
	}
      }
      *scaleout = scale - (digits + 1);
      return fraction;
    }
    num = mkcl_ash(env, num, -1);
    scale++;
    --delta;
  } while (1);
}

static float
ratio_to_float(MKCL, mkcl_object num, mkcl_object den)
{
  mkcl_word scale;
  mkcl_object bits = prepare_ratio_to_float(env, num, den, FLT_MANT_DIG, &scale);
#if (MKCL_WORD_BITS - MKCL_TAG_BITS) >= FLT_MANT_DIG
  /* In this case the output of prepare_ratio_to_float() always fits within a fixnum. */
  float output = mkcl_fixnum_to_word(bits);
#else
  float output = MKCL_FIXNUMP(bits) ? mkcl_fixnum_to_word(bits) : _mkcl_big_to_truncated_double(bits);
#endif
  return ldexpf(output, scale);
}

static double
ratio_to_double(MKCL, mkcl_object num, mkcl_object den)
{
  mkcl_word scale;
  mkcl_object bits = prepare_ratio_to_float(env, num, den, DBL_MANT_DIG, &scale);
#if (MKCL_WORD_BITS - MKCL_TAG_BITS) >= DBL_MANT_DIG
  /* In this case the output of prepare_ratio_to_float() always fits within a fixnum. */
  double output = mkcl_fixnum_to_word(bits);
#else
  double output = MKCL_FIXNUMP(bits) ? mkcl_fixnum_to_word(bits) : _mkcl_big_to_truncated_double(bits);
#endif
  return ldexp(output, scale);
}

float
mkcl_to_float(MKCL, mkcl_object x)
{
  switch(mkcl_type_of(x)) {
  case mkcl_t_fixnum:
    return((float)(mkcl_fixnum_to_word(x)));
  case mkcl_t_bignum:
    return ratio_to_double(env, x, MKCL_MAKE_FIXNUM(1));
  case mkcl_t_ratio:
    return ratio_to_float(env, x->ratio.num, x->ratio.den);
  case mkcl_t_singlefloat:
    return mkcl_single_float(x);
  case mkcl_t_doublefloat:
    return((float) mkcl_double_float(x));
#ifdef MKCL_LONG_FLOAT
  case mkcl_t_longfloat:
    return (float) mkcl_long_float(x);
#endif
  default:
    mkcl_FEtype_error_real(env, x);
  }
}

double
mkcl_to_double(MKCL, mkcl_object x)
{
  switch(mkcl_type_of(x)) {
  case mkcl_t_fixnum:
    return((double)(mkcl_fixnum_to_word(x)));
  case mkcl_t_bignum:
    return ratio_to_double(env, x, MKCL_MAKE_FIXNUM(1));
  case mkcl_t_ratio:
    return ratio_to_double(env, x->ratio.num, x->ratio.den);
  case mkcl_t_singlefloat:
    return (double)mkcl_single_float(x);
  case mkcl_t_doublefloat:
    return(mkcl_double_float(x));
#ifdef MKCL_LONG_FLOAT
  case mkcl_t_longfloat:
    return (double)mkcl_long_float(x);
#endif
  default:
    mkcl_FEtype_error_real(env, x);
  }
}

#ifdef MKCL_LONG_FLOAT
static long double
ratio_to_long_double(MKCL, mkcl_object num, mkcl_object den)
{
  mkcl_word scale;
  mkcl_object bits = prepare_ratio_to_float(env, num, den, LDBL_MANT_DIG, &scale);
  long double output = mkcl_to_long_double(env, bits);
  return ldexpl(output, scale);
}
#endif

#ifdef MKCL_LONG_FLOAT
long double
mkcl_to_long_double(MKCL, mkcl_object x)
{
  switch(mkcl_type_of(x)) {
  case mkcl_t_fixnum:
    return (long double)mkcl_fixnum_to_word(x);
  case mkcl_t_bignum: {
    long double output = 0;
    int i, l = mpz_size(x->big.big_num);
    for (i = 0; i < l; i++) {
      output += mpz_getlimbn(x->big.big_num, i);
      output = ldexpl(output, -GMP_LIMB_BITS);
    }
    output = ldexpl(output, l * GMP_LIMB_BITS);
    return (mpz_sgn(x->big.big_num) < 0) ? -output : output;
  }
  case mkcl_t_ratio:
    return ratio_to_long_double(env, x->ratio.num, x->ratio.den);
  case mkcl_t_singlefloat:
    return (long double)mkcl_single_float(x);
  case mkcl_t_doublefloat:
    return (long double)mkcl_double_float(x);
  case mkcl_t_longfloat:
    return mkcl_long_float(x);
  default:
    mkcl_FEtype_error_real(env, x);
  }
}
#endif

mkcl_object
mk_cl_rational(MKCL, mkcl_object x)
{
  double d;

  mkcl_call_stack_check(env);
 AGAIN:
  switch (mkcl_type_of(x)) {
  case mkcl_t_fixnum:
  case mkcl_t_bignum:
  case mkcl_t_ratio:
    break;
  case mkcl_t_singlefloat:
    d = mkcl_single_float(x);
    goto GO_ON;
  case mkcl_t_doublefloat:
    d = mkcl_double_float(x);
  GO_ON:
    if (d == 0) {
      x = MKCL_MAKE_FIXNUM(0);
    } else {
      int e;
      d = frexp(d, &e);
      e -= DBL_MANT_DIG;
      x = mkcl_double_to_integer(env, ldexp(d, DBL_MANT_DIG));
      if (e != 0) {
	x = mkcl_times(env,
		       mk_cl_expt(env, MKCL_MAKE_FIXNUM(FLT_RADIX), MKCL_MAKE_FIXNUM(e)),
		       x);
      }
    }
    break;
#ifdef MKCL_LONG_FLOAT
  case mkcl_t_longfloat: {
    long double d = mkcl_long_float(x);
    if (d == 0) {
      x = MKCL_MAKE_FIXNUM(0);
    } else {
      int e;
      d = frexpl(d, &e);
      e -= LDBL_MANT_DIG;
      d = ldexpl(d, LDBL_MANT_DIG);
      x = mkcl_long_double_to_integer(env, d);
      if (e != 0) {
	x = mkcl_times(env,
		       mk_cl_expt(env, MKCL_MAKE_FIXNUM(FLT_RADIX), MKCL_MAKE_FIXNUM(e)),
		       x);
      }
    }
    break;
  }
#endif
  default:
    x = mkcl_type_error(env, MK_CL_rational,"argument",x,MK_CL_number);
    goto AGAIN;
  }
  mkcl_return_value(x);
}

mkcl_object
mkcl_long_double_to_integer(MKCL, long double d0)
{
  const int fb = MKCL_WORD_BITS - 3;
  int e;
  long double d = frexpl(d0, &e);
  if (e <= fb) {
    return MKCL_MAKE_FIXNUM((mkcl_word)d0);
  } else if (e > LDBL_MANT_DIG) {
    return mkcl_ash(env, mkcl_long_double_to_integer(env, ldexp(d, LDBL_MANT_DIG)),
		   e - LDBL_MANT_DIG);
  } else {
    long double d1 = floorl(d = ldexpl(d, fb));
    int newe = e - fb;
    mkcl_object o = mkcl_ash(env, mkcl_long_double_to_integer(env, d1), newe);
    long double d2 = ldexpl(d - d1, newe);
    if (d2) o = mkcl_plus(env, o, mkcl_long_double_to_integer(env, d2));
    return o;
  }
}

mkcl_object
mkcl_double_to_integer(MKCL, double d)
{
  if (d <= MKCL_MOST_POSITIVE_FIXNUM && d >= MKCL_MOST_NEGATIVE_FIXNUM)
    return MKCL_MAKE_FIXNUM((mkcl_word)d);
  else {
    mkcl_object z = _mkcl_big_register0();
    _mkcl_big_set_d(z, d);
    return _mkcl_big_register_copy(env, z);
  }
}

mkcl_object
mkcl_float_to_integer(MKCL, float d)
{
  if (d <= MKCL_MOST_POSITIVE_FIXNUM && d >= MKCL_MOST_NEGATIVE_FIXNUM)
    return MKCL_MAKE_FIXNUM((mkcl_word)d);
  else {
    mkcl_object z = _mkcl_big_register0();
    _mkcl_big_set_d(z, d);
    return _mkcl_big_register_copy(env, z);
  }
}

void
mkcl_init_number(MKCL)
{
  mkcl_object num;

  num = mkcl_make_singlefloat(env, FLT_MAX);
  MKCL_SET(MK_CL_MOST_POSITIVE_SHORT_FLOAT, num);
  MKCL_SET(MK_CL_MOST_POSITIVE_SINGLE_FLOAT, num);

  num = mkcl_make_singlefloat(env, -FLT_MAX);
  MKCL_SET(MK_CL_MOST_NEGATIVE_SHORT_FLOAT, num);
  MKCL_SET(MK_CL_MOST_NEGATIVE_SINGLE_FLOAT, num);

  num = mkcl_make_singlefloat(env, FLT_MIN);
  MKCL_SET(MK_CL_LEAST_POSITIVE_SHORT_FLOAT, num);
  MKCL_SET(MK_CL_LEAST_POSITIVE_SINGLE_FLOAT, num);
  MKCL_SET(MK_CL_LEAST_POSITIVE_NORMALIZED_SHORT_FLOAT, num);
  MKCL_SET(MK_CL_LEAST_POSITIVE_NORMALIZED_SINGLE_FLOAT, num);

  num = mkcl_make_singlefloat(env, -FLT_MIN);
  MKCL_SET(MK_CL_LEAST_NEGATIVE_SHORT_FLOAT, num);
  MKCL_SET(MK_CL_LEAST_NEGATIVE_SINGLE_FLOAT, num);
  MKCL_SET(MK_CL_LEAST_NEGATIVE_NORMALIZED_SHORT_FLOAT, num);
  MKCL_SET(MK_CL_LEAST_NEGATIVE_NORMALIZED_SINGLE_FLOAT, num);

  num = mkcl_make_doublefloat(env, DBL_MAX);
  MKCL_SET(MK_CL_MOST_POSITIVE_DOUBLE_FLOAT, num);

#ifdef MKCL_LONG_FLOAT
  num = mkcl_make_longfloat(env, LDBL_MAX);
#endif
  MKCL_SET(MK_CL_MOST_POSITIVE_LONG_FLOAT, num);

  num = mkcl_make_doublefloat(env, -DBL_MAX);
  MKCL_SET(MK_CL_MOST_NEGATIVE_DOUBLE_FLOAT, num);

#ifdef MKCL_LONG_FLOAT
  num = mkcl_make_longfloat(env, -LDBL_MAX);
#endif
  MKCL_SET(MK_CL_MOST_NEGATIVE_LONG_FLOAT, num);

  num = mkcl_make_doublefloat(env, DBL_MIN);
  MKCL_SET(MK_CL_LEAST_POSITIVE_DOUBLE_FLOAT, num);
  MKCL_SET(MK_CL_LEAST_POSITIVE_NORMALIZED_DOUBLE_FLOAT, num);

#ifdef MKCL_LONG_FLOAT
  num = mkcl_make_longfloat(env, LDBL_MIN);
#endif
  MKCL_SET(MK_CL_LEAST_POSITIVE_LONG_FLOAT, num);
  MKCL_SET(MK_CL_LEAST_POSITIVE_NORMALIZED_LONG_FLOAT, num);

  num = mkcl_make_doublefloat(env, -DBL_MIN);
  MKCL_SET(MK_CL_LEAST_NEGATIVE_DOUBLE_FLOAT, num);
  MKCL_SET(MK_CL_LEAST_NEGATIVE_NORMALIZED_DOUBLE_FLOAT, num);

#ifdef MKCL_LONG_FLOAT
  num = mkcl_make_longfloat(env, -LDBL_MIN);
#endif
  MKCL_SET(MK_CL_LEAST_NEGATIVE_LONG_FLOAT, num);
  MKCL_SET(MK_CL_LEAST_NEGATIVE_NORMALIZED_LONG_FLOAT, num);

  mkcl_core.singlefloat_zero = mkcl_alloc_raw_singlefloat(env);
  mkcl_single_float(mkcl_core.singlefloat_zero) = 0.0F;
  mkcl_core.doublefloat_zero = mkcl_alloc_raw_doublefloat(env);
  mkcl_double_float(mkcl_core.doublefloat_zero) = 0.0;
#ifdef MKCL_LONG_FLOAT
  mkcl_core.longfloat_zero = mkcl_alloc_raw_longfloat(env);
  mkcl_core.longfloat_zero->longfloat.value = 0.0L;
#else
  mkcl_core.longfloat_zero = mkcl_core.doublefloat_zero;
#endif
#ifdef MKCL_SIGNED_ZERO
  mkcl_core.singlefloat_minus_zero = mkcl_alloc_raw_singlefloat(env);
  mkcl_single_float(mkcl_core.singlefloat_minus_zero) = -0.0F;
  mkcl_core.doublefloat_minus_zero = mkcl_alloc_raw_doublefloat(env);
  mkcl_double_float(mkcl_core.doublefloat_minus_zero) = -0.0;

# ifdef MKCL_LONG_FLOAT
  mkcl_core.longfloat_minus_zero = mkcl_alloc_raw_longfloat(env);
  mkcl_core.longfloat_minus_zero->longfloat.value = -0.0L;
# else
  mkcl_core.longfloat_minus_zero = mkcl_core.doublefloat_minus_zero;
# endif
#else
  mkcl_core.singlefloat_minus_zero = mkcl_core.singlefloat_zero;
  mkcl_core.doublefloat_minus_zero = mkcl_core.doublefloat_zero;

  mkcl_core.longfloat_minus_zero = mkcl_core.longfloat_zero;
#endif
  mkcl_core.plus_half = mkcl_make_ratio(env, MKCL_MAKE_FIXNUM(1), MKCL_MAKE_FIXNUM(2));
  mkcl_core.minus_half = mkcl_make_ratio(env, MKCL_MAKE_FIXNUM(-1), MKCL_MAKE_FIXNUM(2));
  mkcl_core.imag_unit =
    mkcl_make_complex(env, mkcl_make_singlefloat(env, 0.0),
		     mkcl_make_singlefloat(env, 1.0));
  mkcl_core.minus_imag_unit =
    mkcl_make_complex(env, mkcl_make_singlefloat(env, 0.0),
		     mkcl_make_singlefloat(env, -1.0));
  mkcl_core.imag_two =
    mkcl_make_complex(env, mkcl_make_singlefloat(env, 0.0),
		     mkcl_make_singlefloat(env, 2.0));

#ifdef MKCL_LONG_FLOAT
  MKCL_SET(MK_CL_pi, mkcl_make_longfloat(env, MKCL_PI_L));
#else
  MKCL_SET(MK_CL_pi, mkcl_make_doublefloat(env, MKCL_PI_D));
#endif
  MKCL_SET(MK_CL_DYNVAR_random_state, mkcl_make_random_state(env, mk_cl_Ct));
}
