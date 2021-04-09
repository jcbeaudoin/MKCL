/* -*- mode: c -*- */
/*
    num_pred.c  -- Predicates on numbers.
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

int
mkcl_zerop(MKCL, mkcl_object x)
{
  switch (mkcl_type_of(x)) {
  case mkcl_t_fixnum:
    return(x == MKCL_MAKE_FIXNUM(0));

  case mkcl_t_bignum:
  case mkcl_t_ratio:
    return(0);
  case mkcl_t_singlefloat:
    return(mkcl_single_float(x) == 0.0F);

  case mkcl_t_doublefloat:
    return(mkcl_double_float(x) == 0.0);
#ifdef MKCL_LONG_FLOAT
  case mkcl_t_longfloat:
    return mkcl_long_float(x) == 0.0L;
#endif

  case mkcl_t_complex:
    return(mkcl_zerop(env, x->_complex.real) &&
	   mkcl_zerop(env, x->_complex.imag));

  default:
    mkcl_FEtype_error_number(env, x);
  }
}

int
mkcl_plusp(MKCL, mkcl_object x)
{
 RESTART:
  switch (mkcl_type_of(x)) {
  case mkcl_t_fixnum:
    return(mkcl_fixnum_to_word(x) > 0);

  case mkcl_t_bignum:
    return(_mkcl_big_sign(x) > 0);

  case mkcl_t_ratio:
    /* INV: rat_den is always positive */
    x = x->ratio.num;
    goto RESTART;
  case mkcl_t_singlefloat:
    return mkcl_single_float(x) > 0.0F;
  case mkcl_t_doublefloat:
    return mkcl_double_float(x) > 0.0;
#ifdef MKCL_LONG_FLOAT
  case mkcl_t_longfloat:
    return mkcl_long_float(x) > 0.0L;
#endif
  default:
    mkcl_FEtype_error_real(env, x);
  }
}

int
mkcl_minusp(MKCL, mkcl_object x)
{
 RESTART:
  switch (mkcl_type_of(x)) {
  case mkcl_t_fixnum:
    return(mkcl_fixnum_to_word(x) < 0);

  case mkcl_t_bignum:
    return(_mkcl_big_sign(x) < 0);

  case mkcl_t_ratio:
    /* INV: rat_den is always positive */
    x = x->ratio.num;
    goto RESTART;

  case mkcl_t_singlefloat:
    return mkcl_single_float(x) < 0.0F;

  case mkcl_t_doublefloat:
    return mkcl_double_float(x) < 0.0;
#ifdef MKCL_LONG_FLOAT
  case mkcl_t_longfloat:
    return mkcl_long_float(x) < 0.0L;
#endif
  default:
    mkcl_FEtype_error_real(env, x);
  }
}

int
mkcl_oddp(MKCL, mkcl_object x)
{
  if (MKCL_FIXNUMP(x))
    return mkcl_fixnum_to_word(x) & 1;
  if (mkcl_type_of(x) == mkcl_t_bignum)
    return _mkcl_big_odd_p(x);
  mkcl_FEtype_error_integer(env, x);
}

int
mkcl_evenp(MKCL, mkcl_object x)
{
  if (MKCL_FIXNUMP(x))
    return ~mkcl_fixnum_to_word(x) & 1;
  if (mkcl_type_of(x) == mkcl_t_bignum)
    return _mkcl_big_even_p(x);
  mkcl_FEtype_error_integer(env, x);
}

mkcl_object
mk_cl_zerop(MKCL, mkcl_object x)
{	/* INV: mkcl_zerop() checks type */
  mkcl_call_stack_check(env);
  mkcl_return_value((mkcl_zerop(env, x) ? mk_cl_Ct : mk_cl_Cnil));
}

mkcl_object
mk_cl_plusp(MKCL, mkcl_object x)
{	/* INV: mkcl_plusp()  checks type */
  mkcl_call_stack_check(env);
  mkcl_return_value((mkcl_plusp(env, x) ? mk_cl_Ct : mk_cl_Cnil));
}

mkcl_object
mk_cl_minusp(MKCL, mkcl_object x)
{	/* INV: mkcl_minusp() checks type */
  mkcl_call_stack_check(env);
  mkcl_return_value((mkcl_minusp(env, x) ? mk_cl_Ct : mk_cl_Cnil));
}

mkcl_object
mk_cl_oddp(MKCL, mkcl_object x)
{	/* INV: mkcl_oddp() checks type */
  mkcl_call_stack_check(env);
  mkcl_return_value((mkcl_oddp(env, x) ? mk_cl_Ct : mk_cl_Cnil));
}

mkcl_object
mk_cl_evenp(MKCL, mkcl_object x)
{	/* INV: mkcl_evenp() checks_type */
  mkcl_call_stack_check(env);
  mkcl_return_value((mkcl_evenp(env, x) ? mk_cl_Ct : mk_cl_Cnil));
}

mkcl_object
mk_si_float_nan_p(MKCL, mkcl_object x)
{
  mkcl_call_stack_check(env);
  mkcl_return_value((mkcl_float_nan_p(env, x) ? mk_cl_Ct : mk_cl_Cnil));
}

mkcl_object
mk_si_float_infinity_p(MKCL, mkcl_object x)
{
  mkcl_call_stack_check(env);
  mkcl_return_value((mkcl_float_infinity_p(env, x) ? mk_cl_Ct : mk_cl_Cnil));
}

bool
mkcl_float_nan_p(MKCL, mkcl_object x)
{
  switch (mkcl_type_of(x)) {
  case mkcl_t_singlefloat:
    return isnan(mkcl_single_float(x));
  case mkcl_t_doublefloat:
    return isnan(mkcl_double_float(x));
#ifdef MKCL_LONG_FLOAT
  case mkcl_t_longfloat:
    return isnan(mkcl_long_float(x));
#endif
  default:
    return FALSE;
  }
}

bool
mkcl_float_infinity_p(MKCL, mkcl_object x)
{
  switch (mkcl_type_of(x)) {
  case mkcl_t_singlefloat:
    return isinf(mkcl_single_float(x));
  case mkcl_t_doublefloat:
    return isinf(mkcl_double_float(x));
#ifdef MKCL_LONG_FLOAT
  case mkcl_t_longfloat:
    return isinf(mkcl_long_float(x));
#endif
  default:
    return FALSE;
  }
}
