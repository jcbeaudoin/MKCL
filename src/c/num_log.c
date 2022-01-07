/* -*- mode: c -*- */
/*
    num_log.c  -- Logical operations on numbers.
*/
/*
    Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
    Copyright (c) 1990, Giuseppe Attardi.
    Copyright (c) 2001, Juan Jose Garcia Ripoll.
    Copyright (c) 2012,2021, Jean-Claude Beaudoin.

    MKCL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 3 of the License, or (at your option) any later version.

    See file '../../Copyright' for full details.
*/

#include <mkcl/mkcl.h>
#include <stdlib.h>
#include <mkcl/internal.h>

/*
 * BIT OPERATIONS FOR FIXNUMS
 */

static mkcl_word
ior_op(mkcl_word i, mkcl_word j)
{
  return(i | j);
}

static void
mpz_ior_op(mkcl_object i, mkcl_object j)
{
  mpz_ior(i->big.big_num, i->big.big_num, j->big.big_num);
}

static mkcl_word
xor_op(mkcl_word i, mkcl_word j)
{
  return(i ^ j);
}

static void
mpz_xor_op(mkcl_object i, mkcl_object j)
{
  mpz_xor(i->big.big_num, i->big.big_num, j->big.big_num);
}

static mkcl_word
and_op(mkcl_word i, mkcl_word j)
{
  return(i & j);
}

static void
mpz_and_op(mkcl_object i, mkcl_object j)
{
  mpz_and(i->big.big_num, i->big.big_num, j->big.big_num);
}

static mkcl_word
eqv_op(mkcl_word i, mkcl_word j)
{
  return(~(i ^ j));
}

static void
mpz_eqv_op(mkcl_object i, mkcl_object j)
{
  mpz_xor(i->big.big_num, i->big.big_num, j->big.big_num);
  mpz_com(i->big.big_num, i->big.big_num);
}

static mkcl_word
nand_op(mkcl_word i, mkcl_word j)
{
  return(~(i & j));
}

static void
mpz_nand_op(mkcl_object i, mkcl_object j)
{
  mpz_and(i->big.big_num, i->big.big_num, j->big.big_num);
  mpz_com(i->big.big_num, i->big.big_num);
}

static mkcl_word
nor_op(mkcl_word i, mkcl_word j)
{
  return(~(i | j));
}

static void
mpz_nor_op(mkcl_object i, mkcl_object j)
{
  mpz_ior(i->big.big_num, i->big.big_num, j->big.big_num);
  mpz_com(i->big.big_num, i->big.big_num);
}

static mkcl_word
andc1_op(mkcl_word i, mkcl_word j)
{
  return((~i) & j);
}

static void
mpz_andc1_op(mkcl_object i, mkcl_object j)
{
  mpz_com(i->big.big_num, i->big.big_num);
  mpz_and(i->big.big_num, i->big.big_num, j->big.big_num);
}

static mkcl_word
andc2_op(mkcl_word i, mkcl_word j)
{
  return(i & (~j));
}

static void mpz_orc1_op(mkcl_object, mkcl_object);

static void
mpz_andc2_op(mkcl_object i, mkcl_object j)
{
	/* (i & ~j) = ~((~i) | j) */
  mpz_orc1_op(i, j);
  mpz_com(i->big.big_num, i->big.big_num);
}

static mkcl_word
orc1_op(mkcl_word i, mkcl_word j)
{
  return((~i) | j);
}

static void
mpz_orc1_op(mkcl_object i, mkcl_object j)
{
  mpz_com(i->big.big_num, i->big.big_num);
  mpz_ior(i->big.big_num, i->big.big_num, j->big.big_num);
}

static mkcl_word
orc2_op(mkcl_word i, mkcl_word j)
{
  return(i | (~j));
}

static void
mpz_orc2_op(mkcl_object i, mkcl_object j)
{
	/* (i | ~j) = ~((~i) & j) */
  mpz_andc1_op(i, j);
  mpz_com(i->big.big_num, i->big.big_num);
}

static mkcl_word
b_clr_op(mkcl_word i, mkcl_word j)
{
  return(0);
}

static void
mpz_b_clr_op(mkcl_object i, mkcl_object j)
{
  mpz_set_si(i->big.big_num, 0);
}

static mkcl_word
b_set_op(mkcl_word i, mkcl_word j)
{
  return(-1);
}

static void
mpz_b_set_op(mkcl_object i, mkcl_object j)
{
  mpz_set_si(i->big.big_num, -1);
}

static mkcl_word
b_1_op(mkcl_word i, mkcl_word j)
{
  return(i);
}

static void
mpz_b_1_op(mkcl_object i, mkcl_object j)
{
}

static mkcl_word
b_2_op(mkcl_word i, mkcl_word j)
{
  return(j);
}

static void
mpz_b_2_op(mkcl_object i, mkcl_object j)
{
  mpz_set(i->big.big_num, j->big.big_num);
}

static mkcl_word
b_c1_op(mkcl_word i, mkcl_word j)
{
  return(~i);
}

static void
mpz_b_c1_op(mkcl_object i, mkcl_object j)
{
  mpz_com(i->big.big_num, i->big.big_num);
}

static mkcl_word
b_c2_op(mkcl_word i, mkcl_word j)
{
  return(~j);
}

static void
mpz_b_c2_op(mkcl_object i, mkcl_object j)
{
  mpz_com(i->big.big_num, j->big.big_num);
}

typedef mkcl_word (*bit_operator)(mkcl_word, mkcl_word);
typedef void (*bignum_bit_operator)(mkcl_object, mkcl_object);

static const bit_operator fixnum_operations[16] = {
  b_clr_op,
  and_op,
  andc2_op,
  b_1_op,
  andc1_op,
  b_2_op,
  xor_op,
  ior_op,
  nor_op,
  eqv_op,
  b_c2_op,
  orc2_op,
  b_c1_op,
  orc1_op,
  nand_op,
  b_set_op
};

static const bignum_bit_operator bignum_operations[16] = {
  mpz_b_clr_op,
  mpz_and_op,
  mpz_andc2_op,
  mpz_b_1_op,
  mpz_andc1_op,
  mpz_b_2_op,
  mpz_xor_op,
  mpz_ior_op,
  mpz_nor_op,
  mpz_eqv_op,
  mpz_b_c2_op,
  mpz_orc2_op,
  mpz_b_c1_op,
  mpz_orc1_op,
  mpz_nand_op,
  mpz_b_set_op
};


static mkcl_object
log_op(MKCL, mkcl_narg narg, int op, mkcl_va_list ARGS)
{
  mkcl_object x, y;
  /* FIXME! This can be optimized */
  x = mkcl_va_arg(ARGS);
  if (narg-- == 1) {
    mkcl_assert_type_integer(env, x);
  } else {
    do {
      y = mkcl_va_arg(ARGS);
      x = mkcl_boole(env, op, x, y);
    } while (--narg);
  }
  return x;
}

mkcl_object
mkcl_boole(MKCL, int op, mkcl_object x, mkcl_object y)
{
  switch (mkcl_type_of(x)) {
  case mkcl_t_fixnum:
    switch (mkcl_type_of(y)) {
    case mkcl_t_fixnum: {
      mkcl_word z = fixnum_operations[op](mkcl_fixnum_to_word(x), mkcl_fixnum_to_word(y));
      return MKCL_MAKE_FIXNUM(z);
    }
    case mkcl_t_bignum: {
      mkcl_object x_copy = _mkcl_big_register0();
#if MKCL_LONG_BITS < MKCL_WORD_BITS
      _mkcl_big_set_fixnum(x_copy, mkcl_fixnum_to_word(x));
#else
      _mkcl_big_set_si(x_copy, mkcl_fixnum_to_word(x));
#endif
      bignum_operations[op](x_copy, y);
      return _mkcl_big_register_normalize(env, x_copy);
    }
    default:
      mkcl_FEtype_error_integer(env, y);
    }
    break;
  case mkcl_t_bignum: {
    mkcl_object x_copy = _mkcl_big_register0();
    _mkcl_big_set(x_copy, x);
    switch (mkcl_type_of(y)) {
    case mkcl_t_fixnum: {
      mkcl_object z = _mkcl_big_register1();
#if MKCL_LONG_BITS < MKCL_WORD_BITS
      _mkcl_big_set_fixnum(z, mkcl_fixnum_to_word(y));
#else
      _mkcl_big_set_si(z,mkcl_fixnum_to_word(y));
#endif
      bignum_operations[op](x_copy, z);
      _mkcl_big_register_free(env, z);
      break;
    }
    case mkcl_t_bignum:
      bignum_operations[op](x_copy, y);
      break;
    default:
      mkcl_FEtype_error_integer(env, y);
    }
    return _mkcl_big_register_normalize(env, x_copy);
  }
  default:
    mkcl_FEtype_error_integer(env, x);
  }
  return x;
}

mkcl_object
mk_cl_lognot(MKCL, mkcl_object x)
{
  mkcl_call_stack_check(env);
  return mk_cl_logxor(env, 2, x, MKCL_MAKE_FIXNUM(-1));
}

static mkcl_word
count_bits(MKCL, mkcl_object x)
{
  mkcl_word count;

  switch (mkcl_type_of(x)) {
  case mkcl_t_fixnum: {
    mkcl_word i = mkcl_fixnum_to_word(x);
    mkcl_word j = (i < 0) ? ~i : i;
    for (count=0 ; j ; j >>= 1)
      if (j & 1) count++;
    break;
  }
  case mkcl_t_bignum:
    if (_mkcl_big_sign(x) >= 0)
      count = mpz_popcount(x->big.big_num);
    else
      {
	mkcl_object z = _mkcl_big_register0();
	mpz_com(z->big.big_num, x->big.big_num);
	count = mpz_popcount(z->big.big_num);
	_mkcl_big_register_free(env, z);
      }
    break;
  default:
    mkcl_FEtype_error_integer(env, x);
  }
  return count;
}

/*
   Left shift if w > 0, right shift if w < 0.
 */
mkcl_object
mkcl_ash(MKCL, mkcl_object x, mkcl_word w)
{
  mkcl_object y;

  if (w == 0)
    return(x);
  y = _mkcl_big_register0();
  if (w < 0) {
    mkcl_index bits = -w;
    if (MKCL_FIXNUMP(x)) {
      /* The result of shifting a number further than the number
       * of digits it has is undefined in C.
       * Furthermore, shifting to the right negative numbers leads
       * to implementation-specific results.
       * Here we rely on the sign bit being duplicated by >>.
       */
      mkcl_word y = mkcl_fixnum_to_word(x);
      if (bits >= MKCL_WORD_BITS) {
	y = (y < 0) ? -1 : 0;
      } else {
	y >>= bits;
      }
      return MKCL_MAKE_FIXNUM(y);
    } else
      mpz_div_2exp(y->big.big_num, x->big.big_num, bits);
  } else {
    if (MKCL_FIXNUMP(x)) {
      const mkcl_word shift_out_mask = ((((mkcl_word) 1) << w) - 1) << (MKCL_WORD_BITS - 1 - w);

      if ((w >= (MKCL_WORD_BITS - 2)) || (((mkcl_word) x) & shift_out_mask))
	{ /* some bits will overflow into a bignum */
#if MKCL_LONG_BITS < MKCL_WORD_BITS
	  _mkcl_big_set_fixnum(y, mkcl_fixnum_to_word(x));
#else
	  mpz_set_si(y->big.big_num, mkcl_fixnum_to_word(x));
#endif
	  x = y;
	}
      else
	{ /* result stays confined within a fixnum. */
	  mkcl_word i = mkcl_fixnum_to_word(x);

	  i <<= w;
	  return MKCL_MAKE_FIXNUM(i);
	}
    }
    mpz_mul_2exp(y->big.big_num, x->big.big_num, (unsigned long)w);
  }
  return(_mkcl_big_register_normalize(env, y));
}

int
mkcl_word_bit_length(mkcl_word i)
{
  int count;
  if (i < 0)
    i = ~i;
  for (count = 0; i && (count < MKCL_WORD_BITS); i >>= 1, count++);
  return count;
}

mkcl_object mk_cl_logior(MKCL, mkcl_narg narg, ...)
{
  mkcl_call_stack_check(env);
  {
    mkcl_setup_for_rest(env, MK_CL_logior, 0, narg, narg, nums);

    if (narg == 0)
      { mkcl_va_end(nums); mkcl_return_value(MKCL_MAKE_FIXNUM(0)); }
    /* INV: log_op() checks types and outputs first argument as default. */
    mkcl_object val = log_op(env, narg, MKCL_BOOLIOR, nums);
    mkcl_va_end(nums);
    mkcl_return_value(val);
  }
}

mkcl_object mk_cl_logxor(MKCL, mkcl_narg narg, ...)
{
  mkcl_call_stack_check(env);
  {
    mkcl_setup_for_rest(env, MK_CL_logxor, 0, narg, narg, nums);

    if (narg == 0)
      { mkcl_va_end(nums); mkcl_return_value(MKCL_MAKE_FIXNUM(0)); }
    /* INV: log_op() checks types and outputs first argument as default. */
    mkcl_object val = log_op(env, narg, MKCL_BOOLXOR, nums);
    mkcl_va_end(nums);
    mkcl_return_value(val);
  }
}

mkcl_object mk_cl_logand(MKCL, mkcl_narg narg, ...)
{
  mkcl_call_stack_check(env);
  {
    mkcl_setup_for_rest(env, MK_CL_logand, 0, narg, narg, nums);

    if (narg == 0)
      { mkcl_va_end(nums); mkcl_return_value(MKCL_MAKE_FIXNUM(-1)); }
    /* INV: log_op() checks types and outputs first argument as default. */
    mkcl_object val = log_op(env, narg, MKCL_BOOLAND, nums);
    mkcl_va_end(nums);
    mkcl_return_value(val);
  }
}

mkcl_object mk_cl_logeqv(MKCL, mkcl_narg narg, ...)
{
  mkcl_call_stack_check(env);
  {
    mkcl_setup_for_rest(env, MK_CL_logeqv, 0, narg, narg, nums);

    if (narg == 0)
      { mkcl_va_end(nums); mkcl_return_value(MKCL_MAKE_FIXNUM(-1)); }
    /* INV: log_op() checks types and outputs first argument as default. */
    mkcl_object val = log_op(env, narg, MKCL_BOOLEQV, nums);
    mkcl_va_end(nums);
    mkcl_return_value(val);
  }
}

mkcl_object
mk_cl_lognand(MKCL, mkcl_object x, mkcl_object y)
{
  mkcl_call_stack_check(env);
  mkcl_return_value(mkcl_boole(env, MKCL_BOOLNAND, x, y));
}

mkcl_object
mk_cl_lognor(MKCL, mkcl_object x, mkcl_object y)
{
  mkcl_call_stack_check(env);
  mkcl_return_value(mkcl_boole(env, MKCL_BOOLNOR, x, y));
}

mkcl_object
mk_cl_logandc1(MKCL, mkcl_object x, mkcl_object y)
{
  mkcl_call_stack_check(env);
  mkcl_return_value(mkcl_boole(env, MKCL_BOOLANDC1, x, y));
}

mkcl_object
mk_cl_logandc2(MKCL, mkcl_object x, mkcl_object y)
{
  mkcl_call_stack_check(env);
  mkcl_return_value(mkcl_boole(env, MKCL_BOOLANDC2, x, y));
}

mkcl_object
mk_cl_logorc1(MKCL, mkcl_object x, mkcl_object y)
{
  mkcl_call_stack_check(env);
  mkcl_return_value(mkcl_boole(env, MKCL_BOOLORC1, x, y));
}

mkcl_object
mk_cl_logorc2(MKCL, mkcl_object x, mkcl_object y)
{
  mkcl_call_stack_check(env);
  mkcl_return_value(mkcl_boole(env, MKCL_BOOLORC2, x, y));
}

static int
coerce_to_logical_operator(MKCL, mkcl_object o)
{
  mkcl_word op;
  op = mkcl_integer_to_word(env, o);
  if (op < 0 || op > MKCL_BOOLSET)
    mkcl_FEerror(env, "~S is an invalid logical operator.", 1, o);
  return op;
}

struct mkcl_cfun mk_cl_boole_cfunobj = MKCL_CFUN3(mk_cl_boole, MK_CL_boole);

mkcl_object
mk_cl_boole(MKCL, mkcl_object o, mkcl_object x, mkcl_object y)
{
  mkcl_call_stack_check(env);
  /* INV: log_op2() checks types */
  mkcl_return_value(mkcl_boole(env, coerce_to_logical_operator(env, o), x, y))
}

mkcl_object
mk_cl_logbitp(MKCL, mkcl_object p, mkcl_object x)
{
  bool i;

  mkcl_call_stack_check(env);
  mkcl_assert_type_integer(env, x);
  if (MKCL_FIXNUMP(p)) {
    mkcl_index n = mkcl_integer_to_index(env, p);
    if (MKCL_FIXNUMP(x)) {
      mkcl_word y = mkcl_fixnum_to_word(x);
      if (n >= MKCL_WORD_BITS) {
	i = (y < 0);
      } else {
	i = ((y >> n) & 1);
      }
    } else {
      i = mpz_tstbit(x->big.big_num, n);
    }
  } else {
    mkcl_assert_type_non_negative_integer(env, p);
    if (MKCL_FIXNUMP(x))
      i = (mkcl_fixnum_to_word(x) < 0);
    else
      i = (_mkcl_big_sign(x) < 0);
  }
  mkcl_return_value((i ? mk_cl_Ct : mk_cl_Cnil));
}

mkcl_object
mk_cl_ash(MKCL, mkcl_object x, mkcl_object y)
{
  mkcl_object r;
  int sign_x;

  mkcl_call_stack_check(env);
  mkcl_assert_type_integer(env, x);
  mkcl_assert_type_integer(env, y);
  if (MKCL_FIXNUMP(y))
    r = mkcl_ash(env, x, mkcl_fixnum_to_word(y));
  else {
    /*
      bit position represented by bignum is probably
      out of our address space. So, result is returned
      according to sign of integer.
    */
    if (MKCL_FIXNUMP(x))
      if (MKCL_FIXNUM_MINUSP(x))
	sign_x = -1;
      else if (x == MKCL_MAKE_FIXNUM(0))
	sign_x = 0;
      else
	sign_x = 1;
    else
      sign_x = _mkcl_big_sign(x);
    if (_mkcl_big_sign(y) < 0)
      if (sign_x < 0)
	r = MKCL_MAKE_FIXNUM(-1);
      else
	r = MKCL_MAKE_FIXNUM(0);
    else if (sign_x == 0)
      r = x;
    else
      mkcl_FEerror(env, "Insufficient memory.", 0);
  }
  mkcl_return_value(r);
}

mkcl_object
mk_cl_logcount(MKCL, mkcl_object x)
{
  mkcl_call_stack_check(env);
  mkcl_return_value(MKCL_MAKE_FIXNUM(count_bits(env, x)));
}

mkcl_index
mkcl_integer_length(MKCL, mkcl_object x)
{
  int count;
  mkcl_word i;

  switch (mkcl_type_of(x)) {
  case mkcl_t_fixnum:
    i = mkcl_fixnum_to_word(x);
    count = mkcl_word_bit_length(i);
    break;
  case mkcl_t_bignum:
    if (_mkcl_big_sign(x) < 0)
      x = mk_cl_lognot(env, x);
    count = mpz_sizeinbase(x->big.big_num, 2);
    break;
  default:
    mkcl_FEtype_error_integer(env, x);
  }
  return count;
}

mkcl_object
mk_cl_integer_length(MKCL, mkcl_object x)
{
  mkcl_call_stack_check(env);
  mkcl_return_value(MKCL_MAKE_FIXNUM(mkcl_integer_length(env, x)));
}

struct mkcl_cfun mk_si_bit_array_op_cfunobj = MKCL_CFUN4(mk_si_bit_array_op, MK_SI_bit_array_op);

mkcl_object
mk_si_bit_array_op(MKCL, mkcl_object o, mkcl_object x, mkcl_object y, mkcl_object r)
{
  mkcl_word i, j, n, d;
  mkcl_object r0 = mk_cl_Cnil;
  bit_operator op;
  bool replace = FALSE;
  int xi, yi, ri;
  mkcl_byte *xp, *yp, *rp;
  int xo, yo, ro;

  mkcl_call_stack_check(env);
  if (mkcl_type_of(x) == mkcl_t_bitvector) {
    d = x->vector.dim;
    xp = x->vector.self.bit;
    xo = x->vector.bit_offset;
    if (mkcl_type_of(y) != mkcl_t_bitvector)
      goto _MKCL_ERROR;
    if (d != y->vector.dim)
      goto _MKCL_ERROR;
    yp = y->vector.self.bit;
    yo = y->vector.bit_offset;
    if (r == mk_cl_Ct)
      r = x;
    if (r != mk_cl_Cnil) {
      if (mkcl_type_of(r) != mkcl_t_bitvector)
	goto _MKCL_ERROR;
      if (r->vector.dim != d)
	goto _MKCL_ERROR;
      i = (r->vector.self.bit - xp)*8 + (r->vector.bit_offset - xo);
      if ((i > 0 && i < d) || (i < 0 && -i < d)) {
	r0 = r;
	r = mk_cl_Cnil;
	replace = TRUE;
	goto L1;
      }
      i = (r->vector.self.bit - yp)*8 + (r->vector.bit_offset - yo);
      if ((i > 0 && i < d) || (i < 0 && -i < d)) {
	r0 = r;
	r = mk_cl_Cnil;
	replace = TRUE;
      }
    }
  L1:
    if (mkcl_Null(r)) {
      r = mk_si_make_vector(env, MK_CL_bit, MKCL_MAKE_FIXNUM(d), mk_cl_Cnil, mk_cl_Cnil, mk_cl_Cnil, mk_cl_Cnil);
    }
  } else {
    if (mkcl_type_of(x) != mkcl_t_array)
      goto _MKCL_ERROR;
    if ((mkcl_elttype)x->array.elttype != mkcl_aet_bit)
      goto _MKCL_ERROR;
    d = x->array.dim;
    xp = x->vector.self.bit;
    xo = x->vector.bit_offset;
    if (mkcl_type_of(y) != mkcl_t_array)
      goto _MKCL_ERROR;
    if ((mkcl_elttype)y->array.elttype != mkcl_aet_bit)
      goto _MKCL_ERROR;
    if (x->array.rank != y->array.rank)
      goto _MKCL_ERROR;
    yp = y->vector.self.bit;
    yo = y->vector.bit_offset;
    for (i = 0;  i < x->array.rank;  i++)
      if (x->array.dims[i] != y->array.dims[i])
	goto _MKCL_ERROR;
    if (r == mk_cl_Ct)
      r = x;
    if (r != mk_cl_Cnil) {
      if (mkcl_type_of(r) != mkcl_t_array)
	goto _MKCL_ERROR;
      if ((mkcl_elttype)r->array.elttype != mkcl_aet_bit)
	goto _MKCL_ERROR;
      if (r->array.rank != x->array.rank)
	goto _MKCL_ERROR;
      for (i = 0;  i < x->array.rank;  i++)
	if (r->array.dims[i] != x->array.dims[i])
	  goto _MKCL_ERROR;
      i = (r->vector.self.bit - xp)*8 + (r->vector.bit_offset - xo);
      if ((i > 0 && i < d) || (i < 0 && -i < d)) {
	r0 = r;
	r = mk_cl_Cnil;
	replace = TRUE;
	goto L2;
      } 
      i = (r->vector.self.bit - yp)*8 + (r->vector.bit_offset - yo);
      if ((i > 0 && i < d) || (i < 0 && -i < d)) {
	r0 = r;
	r = mk_cl_Cnil;
	replace = TRUE;
      }
    }
  L2:
    if (mkcl_Null(r)) {
      r = mkcl_alloc_raw_array(env);
      r->array.self.t = NULL;
      r->array.displaced = mk_cl_Cnil;
      r->array.rank = x->array.rank;
      r->array.dims = x->array.dims;
      r->array.elttype = mkcl_aet_bit;
      r->array.dim = x->array.dim;
      r->array.adjustable = FALSE;
      mkcl_array_allocself(env, r);
    }
  }
  rp = r->vector.self.bit;
  ro = r->vector.bit_offset;
  op = fixnum_operations[coerce_to_logical_operator(env, o)];

#define	set_high(place, nbits, value)					\
  (place)=((place)&~(-0400>>(nbits)))|((value)&(-0400>>(nbits)))

#define	set_low(place, nbits, value)					\
  (place)=((place)&(-0400>>(8-(nbits))))|((value)&~(-0400>>(8-(nbits))))

#define	extract_byte(integer, pointer, index, offset)			\
  (integer) = (pointer)[(index)+1] & 0377;				\
  (integer) = ((pointer)[index]<<(offset))|((integer)>>(8-(offset)))

#define	store_byte(pointer, index, offset, value)		\
  set_low((pointer)[index], 8-(offset), (value)>>(offset));	\
  set_high((pointer)[(index)+1], offset, (value)<<(8-(offset)))

  if (xo == 0 && yo == 0 && ro == 0) {
    for (n = d/8, i = 0;  i < n;  i++)
      rp[i] = (*op)(xp[i], yp[i]);
    if ((j = d%8) > 0)
      set_high(rp[n], j, (*op)(xp[n], yp[n]));
    if (!replace)
      mkcl_return_value(r);
  } else {
    for (n = d/8, i = 0;  i <= n;  i++) {
      extract_byte(xi, xp, i, xo);
      extract_byte(yi, yp, i, yo);
      if (i == n) {
	if ((j = d%8) == 0)
	  break;
	extract_byte(ri, rp, n, ro);
	set_high(ri, j, (*op)(xi, yi));
      } else
	ri = (*op)(xi, yi);
      store_byte(rp, i, ro, ri);
    }
    if (!replace)
      mkcl_return_value(r);
  }
  rp = r0->vector.self.bit;
  ro = r0->vector.bit_offset;
  for (n = d/8, i = 0;  i <= n;  i++) {
    if (i == n) {
      if ((j = d%8) == 0)
	break;
      extract_byte(ri, rp, n, ro);
      set_high(ri, j, r->vector.self.bit[n]);
    } else
      ri = r->vector.self.bit[i];
    store_byte(rp, i, ro, ri);
  }
  mkcl_return_value(r0);
 _MKCL_ERROR:
  mkcl_FEerror(env, "Illegal arguments for bit-array operation.", 0);
}
