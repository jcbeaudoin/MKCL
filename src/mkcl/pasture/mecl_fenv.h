
#ifndef MECL_FENV_H
#define MECL_FENV_H

# if defined(_MSC_VER) || defined(mingw32)
#  define HAVE_FEENABLEEXCEPT
#  include <float.h>
#  if defined(_MSC_VER)
#   define FE_DIVBYZERO EM_ZERODIVIDE
#   define FE_OVERFLOW  EM_OVERFLOW
#   define FE_UNDERFLOW EM_UNDERFLOW
#   define FE_INVALID   EM_INVALID
#   define FE_INEXACT   EM_INEXACT
  typedef int fenv_t;
#  else
#   ifdef _MCW_EM
#    define MCW_EM _MCW_EM
#   else
#    define MCW_EM 0x0008001F
#   endif
#   define fenv_t int
#  endif
#  define feenableexcept(bits) { int cw = _controlfp(0,0); cw &= ~(bits); _controlfp(cw,MCW_EM); }
#  define fedisableexcept(bits) { int cw = _controlfp(0,0); cw |= (bits); _controlfp(cw,MCW_EM); }
#  define feholdexcept(bits) { *(bits) = _controlfp(0,0); _controlfp(0xffffffff, MCW_EM); }
#  define fesetenv(bits) _controlfp(*(bits), MCW_EM)
#  define feupdateenv(bits) fesetenv(bits)
# else /* !_MSC_VER */
#  ifndef HAVE_FENV_H
#   define FE_INVALID 1
#   define FE_DIVBYZERO 2
#   define FE_INEXACT 0
#   define FE_OVERFLOW 0
#   define FE_UNDERFLOW 0
#  endif /* !HAVE_FENV_H */
# endif /* !_MSC_VER */

#endif /* MECL_FENV_H */

