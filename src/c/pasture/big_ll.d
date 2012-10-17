/* -*- mode: c -*- */
/* 
    big_ll.c -- Bignum emulation with long long.
 */
/*
    Copyright (c) 2005, Maciek Pasternacki.

    MECL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.
*/

#include <mecl/mecl.h>
#include <mecl/internal.h>

cl_object
big_register0_get(void)
{
	cl_env.big_register[0]->big.big_num = 0ll;
	return cl_env.big_register[0];
}

cl_object
big_register1_get(void)
{
	cl_env.big_register[1]->big.big_num = 0ll;
	return cl_env.big_register[1];
}

cl_object
big_register2_get(void)
{
	cl_env.big_register[2]->big.big_num = 0ll;
	return cl_env.big_register[2];
}

void
big_register_free(cl_object x) {}

cl_object
big_register_copy(cl_object old)
{
	cl_object new_big = ecl_alloc_object(t_bignum);
        new_big->big.big_num = old->big.big_num;
	return new_big;
}

cl_object
big_register_normalize(cl_object x)
{
	if (x->big.big_num == 0ll)
                return(MAKE_FIXNUM(0));
        if (x->big.big_num <= MOST_POSITIVE_FIXNUM && x->big.big_num >= MOST_NEGATIVE_FIXNUM)
                return(MAKE_FIXNUM(x->big.big_num));
	return big_register_copy(x);
}

static cl_object
big_alloc(int size)
{
	volatile cl_object x = ecl_alloc_object(t_bignum);
	if (size <= 0)
		ecl_internal_error("negative or zero size for bignum in big_alloc");
	x->big.big_num = 0ll;
	return x;
}


cl_object
bignum1(cl_fixnum val)
{
	volatile cl_object z = ecl_alloc_object(t_bignum);
	z->big.big_num = val;
	return(z);
}

cl_object
bignum2(cl_fixnum hi, cl_fixnum lo)
{
	cl_object z;

	z = big_alloc(2);
	z->big.big_num = hi<<32 + lo;
	return(z);
}

cl_object
big_copy(cl_object x)
{
	volatile cl_object y = ecl_alloc_object(t_bignum);
        y->big.big_num = x->big.big_num;
	return(y);
}

/*
	big_minus(x) returns the complement of bignum x.
*/
cl_object
big_minus(cl_object x)
{
	volatile cl_object y = big_copy(x);
        y->big.big_num = -x->big.big_num;
	return y;
}

cl_object
big_plus(cl_object x, cl_object y)
{
	volatile cl_object z = big_register0_get();
        z->big.big_num = x->big.big_num + y->big.big_num;
	return(big_register_copy(z));
}

cl_object
big_normalize(cl_object x)
{
	if (x->big.big_num == 0ll)
		return(MAKE_FIXNUM(0));
	if (x->big.big_num <= MOST_POSITIVE_FIXNUM && x->big.big_num >= MOST_NEGATIVE_FIXNUM)
		return(MAKE_FIXNUM(x->big.big_num));
	return(x);
}

int big_num_t_sgn(big_num_t x)
{
	return ( x == (big_num_t)0 ) ? 0 : (x < (big_num_t)0) ? -1 : 1;
}


void init_big_registers(void)
{
	int i;
	for (i = 0; i < 3; i++) {
		cl_env.big_register[i] = ecl_alloc_object(t_bignum);
                cl_env.big_register[i]->big.big_num = 0ll;
	}
}

void
init_big(void)
{
	init_big_registers();
}
