/* -*- mode: c -*- */
/*
    big.c -- Bignum routines.
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

#include <string.h>

/* 
 * Using GMP multiple precision integers.
 */
#define mkcl_big_dim    big_num->_mp_alloc
#define mkcl_big_size	big_num->_mp_size
#define mkcl_big_limbs	big_num->_mp_d

void
_mkcl_big_register_free(MKCL, mkcl_object x)
{
  if (mkcl_likely(MKCL_BIGNUMP(x))) {
    /* We only need to free the integer when it gets too large */
    if (x->big.mkcl_big_dim > 3 * MKCL_BIG_REGISTER_SIZE) {
      mpz_realloc2(x->big.big_num, MKCL_BIG_REGISTER_SIZE * GMP_LIMB_BITS);
    }
  }
}

static mkcl_object
_mkcl_big_copy(MKCL, mkcl_object old)
{
  mkcl_word size = old->big.mkcl_big_size;
  mkcl_index dim = (size < 0)? (-size) : size;
  mkcl_index bytes = dim * sizeof(mp_limb_t);
#if 0
  mkcl_object new_big = mkcl_alloc_compact_object(env, mkcl_t_bignum, bytes);
  new_big->big.mkcl_big_limbs = MKCL_COMPACT_OBJECT_EXTRA(new_big);
  new_big->big.mkcl_big_dim = dim;
#else
  mkcl_object new_big = mkcl_alloc_bignum_with_limbs(env, dim);
#endif
  new_big->big.mkcl_big_size = size;
  memcpy(new_big->big.mkcl_big_limbs, old->big.mkcl_big_limbs, bytes);
  return new_big;
}

mkcl_object
_mkcl_big_register_copy(MKCL, mkcl_object old)
{
  mkcl_object new_big = _mkcl_big_copy(env, old);
  _mkcl_big_register_free(env, old);
  return new_big;
}

mkcl_object
_mkcl_big_register_normalize(MKCL, mkcl_object x)
{
  int s = x->big.mkcl_big_size;
  if (s == 0)
    return(MKCL_MAKE_FIXNUM(0));
  if (s == 1) {
    mp_limb_t y = x->big.mkcl_big_limbs[0];
    if (y <= MKCL_MOST_POSITIVE_FIXNUM)
      return MKCL_MAKE_FIXNUM(y);
  } else if (s == -1) {
    mp_limb_t y = x->big.mkcl_big_limbs[0];
    if (y <= -MKCL_MOST_NEGATIVE_FIXNUM)
      return MKCL_MAKE_FIXNUM(-y);
  }
  return _mkcl_big_register_copy(env, x);
}

#if MKCL_LONG_BITS < MKCL_WORD_BITS
# undef _mkcl_big_set_fixnum
# undef _mkcl_big_set_index
# if GMP_LIMB_BITS >= MKCL_WORD_BITS
void _mkcl_big_set_fixnum(mkcl_object x, mkcl_word f)
{
  if (f == 0) {
    mpz_set_si(x->big.big_num, 0);
  } else if (f > 0) {
    x->big.mkcl_big_size = 1;
    x->big.mkcl_big_limbs[0] = f;
  } else if (f < 0) {
    x->big.mkcl_big_size = -1;
    x->big.mkcl_big_limbs[0] = -f;
  }
}

void _mkcl_big_set_index(mkcl_object x, mkcl_index f)
{
  /* This code is broken! JCB */
  if (f == 0)
    mpz_set_si(x->big.big_num, 0);
  else
    {
      x->big.mkcl_big_size = 1;
      x->big.mkcl_big_limbs[0] = f;
    }
}

# else
#  error "MKCL cannot be build with GMP when both long and mp_limb_t are smaller than mkcl_word"
# endif /* GMP_LIMB_BITS >= MKCL_WORD_BITS */
#endif /* MKCL_LONG_BITS < MKCL_WORD_BITS */

void
mkcl_init_bignums(MKCL)
{
}
