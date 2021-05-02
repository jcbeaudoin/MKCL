/* -*- mode: c -*- */
/*
    num_rand.c  -- Random numbers.
*/
/*
    Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
    Copyright (c) 1990, Giuseppe Attardi.
    Copyright (c) 2001, Juan Jose Garcia Ripoll.
    Copyright (c) 2011-2016,2021, Jean-Claude Beaudoin.

    MKCL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 3 of the License, or (at your option) any later version.

    See file '../../Copyright' for full details.
*/

#include <mkcl/mkcl.h>
#include <mkcl/internal.h>

#include <time.h>
#include <stdio.h>
#include <stdlib.h>


/*
 * Mersenne-Twister random number generator
 */

/* Period parameters */  
#define MT_N 624
#define MT_M 397
#define MATRIX_A 0x9908b0dfUL   /* constant vector a */
#define UPPER_MASK 0x80000000UL /* most significant w-r bits */
#define LOWER_MASK 0x7fffffffUL /* least significant r bits */

#define ulong unsigned long

mkcl_object
mkcl_init_random_state(MKCL)
{
  mkcl_index bytes = sizeof(ulong) * (MT_N + 1);
  mkcl_object a = mkcl_alloc_simple_base_string(env, bytes);
  ulong *mt = (ulong*)a->base_string.self;
  int j;
#if MKCL_UNIX
  static const char random_path[] = "/dev/urandom";
  static const mkcl_base_string_object(random_source_obj, random_path);
  static const mkcl_object random_source = (mkcl_object) &random_source_obj;
  FILE *fp = fopen(random_path /*"/dev/urandom"*/,"r");
  if (fp) {
    size_t nb = fread(mt, sizeof(*mt), MT_N, fp); /* FIXME: error ignored. */
    for (j=0; j < MT_N; j++){
      mt[j] &= 0xffffffffUL;
    }
    mkcl_safe_fclose(env, fp, random_source);
  } else
    /* FIXME: Why not use CryptGenRandom() on MS-Windows? JCB */
#endif	
    {
      /* cant get urandom, use crappy source */
      mt[0] = (rand() + time(0)) & 0xffffffffUL;
      for (j=1; j < MT_N; j++){
	mt[j] = (1812433253UL * (mt[j-1] ^ (mt[j-1] >> 30)) + j);
	mt[j] &= 0xffffffffUL;
      }
    }
  mt[MT_N] = MT_N+1;
  return a;
}

static ulong
generate_int32(mkcl_object state)
{
  static const ulong mag01[2]={0x0UL, MATRIX_A};
  ulong y;
  ulong *mt = (ulong*)state->base_string.self;
  if (mt[MT_N] >= MT_N){
    /* refresh data */
    int kk;
    for (kk=0; kk < (MT_N - MT_M); kk++) {
      y = (mt[kk] & UPPER_MASK) | (mt[kk+1] & LOWER_MASK);
      mt[kk] = mt[kk + MT_M] ^ (y >> 1) ^ mag01[y & 0x1UL];
    }
    for (; kk < (MT_N - 1); kk++) {
      y = (mt[kk] & UPPER_MASK) | (mt[kk+1] & LOWER_MASK);
      mt[kk] = mt[kk+(MT_M-MT_N)] ^ (y >> 1) ^ mag01[y & 0x1UL];
    }
    y = (mt[MT_N-1] & UPPER_MASK) | (mt[0] & LOWER_MASK);
    mt[MT_N-1] = mt[MT_M-1] ^ (y >> 1) ^ mag01[y & 0x1UL];
    mt[MT_N] = 0;
  }
  /* get random 32 bit num */
  y = mt[mt[MT_N]++];
  /* Tempering */
  y ^= (y >> 11);
  y ^= (y << 7) & 0x9d2c5680UL;
  y ^= (y << 15) & 0xefc60000UL;
  y ^= (y >> 18);
  return y;
}

static double
generate_double(mkcl_object state)
{
	return generate_int32(state) * (1.0 / 4294967296.0);
}


static mp_limb_t
generate_limb(mkcl_object state)
{
#if GMP_LIMB_BITS <= 32
  return generate_int32(state);
#else
# if GMP_LIMB_BITS <= 64
  mp_limb_t high = generate_int32(state);
  return (high << 32) | generate_int32(state);
# else
#  if GMP_LIMB_BITS <= 128
  mp_limb_t word0 = generate_int32(state);
  mp_limb_t word1 = generate_int32(state);
  mp_limb_t word2 = generate_int32(state);
  mp_limb_t word3 = generate_int32(state);
  return (word3 << 96) | (word3 << 64) | (word1 << 32) || word0;
#  endif
# endif
#endif
}

/* This does a peek inside GMP privates, brittle at best. JCB */
#define mkcl_big_limbs	big_num->_mp_d

static mkcl_object
random_integer(MKCL, mkcl_object limit, mkcl_object state)
{
  mkcl_index bit_length = mkcl_integer_length(env, limit);
  mkcl_object buffer;

  if (bit_length <= MKCL_WORD_BITS)
    bit_length = MKCL_WORD_BITS;
  buffer = mkcl_ash(env, MKCL_MAKE_FIXNUM(1), bit_length);
  for (bit_length = mpz_size(buffer->big.big_num); bit_length; ) {
    buffer->big.mkcl_big_limbs[--bit_length] = generate_limb(state);
  }
  return mk_cl_mod(env, buffer, limit);
}

static mkcl_object
rando(MKCL, mkcl_object x, mkcl_object rs)
{
  mkcl_object z;

 AGAIN:
  if (!mkcl_plusp(env, x)) {
    goto _MKCL_ERROR;
  }
  switch (mkcl_type_of(x)) {
  case mkcl_t_fixnum:
#if MKCL_WORD_BITS <= 32
    z = MKCL_MAKE_FIXNUM(generate_int32(rs->random.value)%mkcl_fixnum_to_word(x));
    break;
#endif
  case mkcl_t_bignum:
    z = random_integer(env, x, rs->random.value);
    break;
  case mkcl_t_singlefloat:
    z = mkcl_make_singlefloat(env, mkcl_single_float(x) * (float)generate_double(rs->random.value));
    break;
  case mkcl_t_doublefloat:
    z = mkcl_make_doublefloat(env, mkcl_double_float(x) * generate_double(rs->random.value));
    break;
#ifdef MKCL_LONG_FLOAT
  case mkcl_t_longfloat:
    z = mkcl_make_longfloat(env, 
			    mkcl_long_float(x) 
			    * (long double)generate_double(rs->random.value));
    break;
#endif
  default:
  _MKCL_ERROR:
    x = mkcl_type_error(env, @'random',"limit",x,
			mkcl_fast_read_from_cstring(env, "(OR (INTEGER (0) *) (FLOAT (0) *))"));
    goto AGAIN;
  }
  return z;
}

mkcl_object
mkcl_make_random_state(MKCL, mkcl_object rs)
{
  mkcl_object z = mkcl_alloc_raw_random(env);

  if (rs == mk_cl_Ct) {
    z->random.value = mkcl_init_random_state(env);
  } else {
    if (mkcl_Null(rs)) {
      rs = mkcl_symbol_value(env, @'*random-state*');
    }
    if (mkcl_type_of(rs) != mkcl_t_random) {
      mkcl_FEwrong_type_argument(env, @'random-state', rs);
    }
    z->random.value = mk_cl_copy_seq(env, rs->random.value);
  }
  return(z);
}

mkcl_object mk_cl_random(MKCL, mkcl_narg narg, mkcl_object x, ...)
{
  mkcl_call_stack_check(env);
  {
    mkcl_object rs = ((narg == 1) ? mkcl_symbol_value(env, @'*random-state*') : mk_cl_Cnil);
    MKCL_RECEIVE_1_OPTIONAL_ARGUMENT(env, @'random', narg, 1, x, &rs);

    rs = mkcl_check_cl_type(env, @'random', rs, mkcl_t_random);
    mkcl_return_value(rando(env, x, rs));
  }
}

mkcl_object mk_cl_make_random_state(MKCL, mkcl_narg narg, ...)
{
  mkcl_call_stack_check(env);
  {
    mkcl_object rs = mkcl_symbol_value(env, @'*random-state*');
    MKCL_RECEIVE_1_OPTIONAL_ARGUMENT(env, @'random', narg, 0, narg, &rs);

    mkcl_return_value(mkcl_make_random_state(env, rs));
  }
}

mkcl_object
mk_cl_random_state_p(MKCL, mkcl_object x)
{
  mkcl_call_stack_check(env);
  mkcl_return_value(((mkcl_type_of(x) == mkcl_t_random) ? mk_cl_Ct : mk_cl_Cnil));
}
