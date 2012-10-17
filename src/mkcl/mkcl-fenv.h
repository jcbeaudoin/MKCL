/*
    mkcl-fenv.h
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

#ifndef MKCL_FENV_H
#define MKCL_FENV_H

#if defined(MKCL_WINDOWS)
# define HAVE_FEENABLEEXCEPT
# include <float.h>
# define FE_DIVBYZERO _EM_ZERODIVIDE
# define FE_OVERFLOW  _EM_OVERFLOW
# define FE_UNDERFLOW _EM_UNDERFLOW
# define FE_INVALID   _EM_INVALID
# define FE_INEXACT   _EM_INEXACT
# define FE_ALL_EXCEPT FE_DIVBYZERO | FE_OVERFLOW | FE_UNDERFLOW | FE_INVALID | FE_INEXACT
  typedef int fenv_t;

# define feenableexcept(bits) mkcl_feenableexcept(bits)
# define fedisableexcept(bits) mkcl_fedisableexcept(bits)
# define feholdexcept(bits) mkcl_feholdexcept(bits)
int mkcl_feenableexcept(int);
int mkcl_fedisableexcept(int);
int mkcl_feholdexcept(int * fenv);

# define fegetexcept() mkcl_fegetexcept()
# define fesetenv(bits) mkcl_fesetenv(bits)
int mkcl_fegetexcept(void);
int mkcl_fesetenv(int * fenv);

# define feupdateenv(bits) fesetenv(bits)
# define fetestexcept(excepts) mkcl_fetestexcept(excepts)
# define feclearexcept(excepts) mkcl_feclearexcept(excepts)
int mkcl_fetestexcept(int excepts);
int mkcl_feclearexcept(int excepts);

#else /* !MKCL_WINDOWS */
# ifndef HAVE_FENV_H
#  define FE_INVALID 1
#  define FE_DIVBYZERO 2
#  define FE_INEXACT 0
#  define FE_OVERFLOW 0
#  define FE_UNDERFLOW 0
#  define FE_ALL_EXCEPT FE_DIVBYZERO | FE_OVERFLOW | FE_UNDERFLOW | FE_INVALID | FE_INEXACT
# else
#  include <fenv.h>
#  if __STDC__ && !__clang__
#   pragma STDC FENV_ACCESS ON
#  endif
# endif /* !HAVE_FENV_H */
#endif /* !MKCL_WINDOWS */

#endif /* MKCL_FENV_H */

