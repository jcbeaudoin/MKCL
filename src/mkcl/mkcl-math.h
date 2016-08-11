/*
    mkcl-math.h
*/
/*
    Copyright (c) 1990, Giuseppe Attardi.
    Copyright (c) 2001, Juan Jose Garcia Ripoll.
    Copyright (c) 2010-2011, Jean-Claude Beaudoin.

    MKCL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 3 of the License, or (at your option) any later version.

    See file './Copyright' for full details.
*/



#ifndef MKCL_MATH_H
#define MKCL_MATH_H

#ifndef MKCL_CONFIG_H
# error File <mkcl/config.h> must be included at some point before mkcl-math.h
#endif

#include <math.h>
#ifdef _MSC_VER
# define signbit(x) (copysign(1.0,(x)))
#endif

#include <mkcl/mkcl-fenv.h>

#if defined(HAVE_FENV_H) && !defined(HAVE_FEENABLEEXCEPT) && !defined(MKCL_AVOID_FPE_H)
# define MKCL_MATHERR_CLEAR feclearexcept(FE_ALL_EXCEPT)
# define MKCL_MATHERR_TEST(e) if (fetestexcept(FE_ALL_EXCEPT)) mkcl_deliver_fpe(e);
#else
# define MKCL_MATHERR_CLEAR
# define MKCL_MATHERR_TEST(e)
#endif

#endif /* MKCL_MATH_H */

