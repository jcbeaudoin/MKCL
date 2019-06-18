/* -*- mode: c -*- */
/*
    number.h  -- GMP interface.
*/
/*
    Copyright (c) 1995, Giuseppe Attardi.

    MKCL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 3 of the License, or (at your option) any later version.

    See file './Copyright' for full details.
*/

#ifndef MKCL_NUMBER_H
#define MKCL_NUMBER_H

#define MKCL_BIG_REGISTER_SIZE	32


#if MKCL_LONG_BITS >= MKCL_WORD_BITS
#define _mkcl_big_set_fixnum(x, f) mpz_set_si((x)->big.big_num,(f))
#define _mkcl_big_set_index(x, f) mpz_set_ui((x)->big.big_num,(f))
#else
extern MKCL_API void _mkcl_big_set_fixnum(mkcl_object x, mkcl_word f);
extern MKCL_API void _mkcl_big_set_index(mkcl_object x, mkcl_index f);
#endif
#define _mkcl_big_init2(x,size)	mpz_init2((x)->big.big_num,(size)*GMP_LIMB_BITS)
#define _mkcl_big_clear(x)	mpz_clear((x)->big.big_num)
#define _mkcl_big_set(x,y)	mpz_set((x)->big.big_num,(y)->big.big_num)
#define _mkcl_big_odd_p(x)      mpz_odd_p((x)->big.big_num)
#define _mkcl_big_even_p(x)     mpz_even_p((x)->big.big_num)
#define _mkcl_big_zerop(x)      (mpz_size((x)->big.big_num) == 0)
#define _mkcl_big_sign(x)	mpz_sgn((x)->big.big_num)
#define _mkcl_big_compare(x, y)	mpz_cmp(x->big.big_num, y->big.big_num)
#define _mkcl_big_complement(z, x) mpz_neg((z)->big.big_num,(x)->big.big_num)
#define _mkcl_big_add(z, x, y)	mpz_add((z)->big.big_num,(x)->big.big_num,(y)->big.big_num)
#define _mkcl_big_sub(z, x, y)	mpz_sub((z)->big.big_num,(x)->big.big_num,(y)->big.big_num)
#define _mkcl_big_mul(z, x, y)	mpz_mul((z)->big.big_num,(x)->big.big_num,(y)->big.big_num)
#define _mkcl_big_add_ui(z, x, i)	mpz_add_ui(z->big.big_num, x->big.big_num, i)
#define _mkcl_big_sub_ui(z, x, i)	mpz_sub_ui(z->big.big_num, x->big.big_num, i)
#define _mkcl_big_mul_ui(z, x, y)	mpz_mul_ui((z)->big.big_num,(x)->big.big_num,(y))
#define _mkcl_big_mul_si(z, x, y)	mpz_mul_si((z)->big.big_num,(x)->big.big_num,(y))
#define _mkcl_big_set_ui(x, i)	mpz_set_ui(x->big.big_num, i)
#define _mkcl_big_set_si(x, i)	mpz_set_si(x->big.big_num, i)
#define _mkcl_big_to_truncated_double(x)	mpz_get_d(x->big.big_num)
#define _mkcl_big_to_long(x)		mpz_get_si(x->big.big_num)
#define _mkcl_big_to_ulong(x)		mpz_get_ui(x->big.big_num)
#define _mkcl_big_cmp_si(x,y)		mpz_cmp_si((x)->big.big_num,(y))
#define _mkcl_big_tdiv_q(q, x, y)	mpz_tdiv_q((q)->big.big_num,(x)->big.big_num,(y)->big.big_num)
#define _mkcl_big_tdiv_q_ui(q, x, y)	mpz_tdiv_q_ui((q)->big.big_num, (x)->big.big_num, (y))
#define _mkcl_big_set_d(x, d)		mpz_set_d((x)->big.big_num, (d))
#define _mkcl_big_gcd(gcd, x, y)	mpz_gcd((gcd)->big.big_num, (x)->big.big_num, (y)->big.big_num)

#endif /* MKCL_NUMBER_H */
