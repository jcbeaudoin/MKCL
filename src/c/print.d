/* -*- mode: c -*- */
/*
    print.d -- Print.
*/
/*
    Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
    Copyright (c) 1990, Giuseppe Attardi.
    Copyright (c) 2001, Juan Jose Garcia Ripoll.
    Copyright (c) 2011-2019,2021, Jean-Claude Beaudoin.

    MKCL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 3 of the License, or (at your option) any later version.

    See file '../../Copyright' for full details.
*/

#include <mkcl/mkcl.h>
#include <mkcl/mkcl-math.h>
#include <string.h>
#include <stdlib.h>
#include <errno.h>
#include <stdio.h>
#include <float.h>
#include <mkcl/internal.h>
#include <mkcl/mkcl-fenv.h>
#include <mkcl/bytecode.h>



/**********************************************************************/

#define	to_be_escaped(c) \
	(mkcl_core.standard_readtable->readtable.table[(c)&0377].syntax_type \
	 != mkcl_cat_constituent || \
	 mkcl_lower_case_p((c)&0377) || (c) == ':')

static bool object_will_print_as_hash(MKCL, mkcl_object x);
static mkcl_word search_print_circle(MKCL, mkcl_object x);
static bool potential_number_p(MKCL, mkcl_object s, mkcl_word base);

static void mkcl_FEprint_not_readable(MKCL, mkcl_object x) /*__attribute__((noreturn))*/;

static void
mkcl_FEprint_not_readable(MKCL, mkcl_object x)
{
  mkcl_bds_bind(env, MK_CL_DYNVAR_print_readably, mk_cl_Cnil);
  mk_cl_error(env, 3, MK_CL_print_not_readable, MK_KEY_object, x);
  mkcl_bds_unwind1(env);
}

static mkcl_object
stream_or_default_output(MKCL, mkcl_object stream)
{
  if (mkcl_Null(stream))
    return MKCL_SYM_VAL(env,MK_CL_DYNVAR_standard_output);
  else if (stream == mk_cl_Ct)
    return MKCL_SYM_VAL(env,MK_CL_DYNVAR_terminal_io);
  return stream;
}

mkcl_word
mkcl_print_base(MKCL)
{
  mkcl_object object = mkcl_symbol_value(env, MK_CL_DYNVAR_print_base);
  mkcl_word base;
  if (!MKCL_FIXNUMP(object) || (base = mkcl_fixnum_to_word(object)) < 2 || base > 36) {
    MKCL_SETQ(env, MK_CL_DYNVAR_print_base, MKCL_MAKE_FIXNUM(10));
    mkcl_FEerror(env, "~S is an illegal PRINT-BASE.", 1, object);
  }
  return base;
}

mkcl_word
mkcl_print_level(MKCL)
{
  mkcl_object object = mkcl_symbol_value(env, MK_CL_DYNVAR_print_level);
  mkcl_word level;
  if (object == mk_cl_Cnil) {
    level = MKCL_MOST_POSITIVE_FIXNUM;
  } else if (MKCL_FIXNUMP(object)) {
    level = mkcl_fixnum_to_word(object);
    if (level < 0) {
    _MKCL_ERROR: MKCL_SETQ(env, MK_CL_DYNVAR_print_level, mk_cl_Cnil);
      mkcl_FEerror(env, "~S is an illegal PRINT-LEVEL.", 1, object);
    }
  } else if (mkcl_type_of(object) != mkcl_t_bignum) {
    goto _MKCL_ERROR;
  } else {
    level = MKCL_MOST_POSITIVE_FIXNUM;
  }
  return level;
}

mkcl_word
mkcl_print_length(MKCL)
{
  mkcl_object object = mkcl_symbol_value(env, MK_CL_DYNVAR_print_length);
  mkcl_word length;
  if (object == mk_cl_Cnil) {
    length = MKCL_MOST_POSITIVE_FIXNUM;
  } else if (MKCL_FIXNUMP(object)) {
    length = mkcl_fixnum_to_word(object);
    if (length < 0) {
    _MKCL_ERROR: MKCL_SETQ(env, MK_CL_DYNVAR_print_length, mk_cl_Cnil);
      mkcl_FEerror(env, "~S is an illegal PRINT-LENGTH.", 1, object);
    }
  } else if (mkcl_type_of(object) != mkcl_t_bignum) {
    goto _MKCL_ERROR;
  } else {
    length = MKCL_MOST_POSITIVE_FIXNUM;
  }
  return length;
}

bool
mkcl_print_radix(MKCL)
{
  return mkcl_symbol_value(env, MK_CL_DYNVAR_print_radix) != mk_cl_Cnil;
}

mkcl_object
mkcl_print_case(MKCL)
{
  mkcl_object output = mkcl_symbol_value(env, MK_CL_DYNVAR_print_case);
  if (output != MK_KEY_upcase && output != MK_KEY_downcase &&
      output != MK_KEY_capitalize) {
    MKCL_SETQ(env, MK_CL_DYNVAR_print_case, MK_KEY_downcase);
    mkcl_FEerror(env, "~S is an illegal PRINT-CASE.", 1, output);
  }
  return output;
}

bool
mkcl_print_gensym(MKCL)
{
  return mkcl_symbol_value(env, MK_CL_DYNVAR_print_gensym) != mk_cl_Cnil;
}

bool
mkcl_print_array(MKCL)
{
  return mkcl_symbol_value(env, MK_CL_DYNVAR_print_array) != mk_cl_Cnil;
}

bool
mkcl_print_readably(MKCL)
{
  return mkcl_symbol_value(env, MK_CL_DYNVAR_print_readably) != mk_cl_Cnil;
}

bool
mkcl_print_escape(MKCL)
{
  return mkcl_symbol_value(env, MK_CL_DYNVAR_print_escape) != mk_cl_Cnil;
}

bool
mkcl_print_circle(MKCL)
{
  return mkcl_symbol_value(env, MK_CL_DYNVAR_print_circle) != mk_cl_Cnil;
}

static void
write_str(MKCL, const char *s, mkcl_object stream)
{
  while (*s != '\0')
    mkcl_write_char(env, *s++, stream);
}

static void
write_readable_pathname(MKCL, mkcl_object path, mkcl_object stream)
{
  mkcl_object l = mk_cl_list(env, 15,
			     MK_CL_make_pathname,
			     MK_KEY_host, path->pathname.host,
			     MK_KEY_device, path->pathname.device,
			     MK_KEY_directory, mkcl_funcall1(env, MK_SI_maybe_quote->symbol.gfdef, path->pathname.directory),
			     MK_KEY_name, path->pathname.name,
			     MK_KEY_type, path->pathname.type,
			     MK_KEY_version, path->pathname.version,
			     MK_KEY_defaults, mk_cl_Cnil);
  write_str(env, "#.", stream);
  mk_si_write_object(env, l, stream);
}

static void
write_pathname(MKCL, mkcl_object path, mkcl_object stream)
{
  mkcl_object namestring = mkcl_namestring(env, path, FALSE);
  bool readably = mkcl_print_readably(env);
  if (namestring == mk_cl_Cnil) {
    if (readably) {
      write_readable_pathname(env, path, stream);
      return;
    }
    namestring = mkcl_namestring(env, path, TRUE);
    if (namestring == mk_cl_Cnil) {
      write_str(env, "#<Unprintable pathname>", stream);
      return;
    }
  }
  if (readably || mkcl_print_escape(env))
    write_str(env, "#P", stream);
  mk_si_write_ugly_object(env, namestring, stream);
}

static void
write_positive_fixnum(MKCL, mkcl_index i, int base, mkcl_index len, mkcl_object stream)
{
  /* The maximum number of digits is achieved for base 2 and it
     is always < MKCL_WORD_BITS, since we use at least one bit for
     tagging */
  short digits[MKCL_WORD_BITS];
  int j = 0;
  if (i == 0) {
    digits[j++] = '0';
  } else do {
      digits[j++] = mkcl_digit_char(i % base, base);
      i /= base;
    } while (i > 0);
  while (len-- > j)
    mkcl_write_char(env, '0', stream);
  while (j-- > 0)
    mkcl_write_char(env, digits[j], stream);
}

static void
write_decimal(MKCL, mkcl_word i, mkcl_object stream)
{
  write_positive_fixnum(env, i, 10, 0, stream);
}

static void
write_addr(MKCL, mkcl_object x, mkcl_object stream)
{
  mkcl_word i, j;
  bool lead_zero = TRUE;

  i = (mkcl_index)x;
  for (j = sizeof(i)*8-4;  j >= 0;  j -= 4) {
    int k = (i>>j) & 0xf;
    if (!(k == 0 && lead_zero))
      {
        lead_zero = FALSE;
        if (k < 10)
          mkcl_write_char(env, '0' + k, stream);
        else
          mkcl_write_char(env, 'a' + k - 10, stream);
      }
  }
}

static void
write_base(MKCL, int base, mkcl_object stream)
{
  if (base == 2)
    write_str(env, "#b", stream);
  else if (base == 8)
    write_str(env, "#o", stream);
  else if (base == 16)
    write_str(env, "#x", stream);
  else if (base >= 10) {
    mkcl_write_char(env, '#', stream);
    mkcl_write_char(env, base/10+'0', stream);
    mkcl_write_char(env, base%10+'0', stream);
    mkcl_write_char(env, 'r', stream);
  } else {
    mkcl_write_char(env, '#', stream);
    mkcl_write_char(env, base+'0', stream);
    mkcl_write_char(env, 'r', stream);
  }
}

/* The floating point precision is required to make the
   most-positive-long-float printed expression readable.
   If this is too small, then the rounded off fraction, may be too big
   to read */

/* Maximum number of significant digits required to represent accurately
 * a double or single float. */
#define TIMES_LOG10_2(val) (((val)*3)/10)  /* an integral approximation good to 0.35% as long as there is no overflow */

#define DBL_SIG ((TIMES_LOG10_2(DBL_MANT_DIG) + 1))
#define FLT_SIG ((TIMES_LOG10_2(FLT_MANT_DIG) + 1))

/* This is the maximum number of decimal digits that our numbers will have.
 * Notice that we leave some extra margin, to ensure that reading the number
 * again will produce the same floating point number.
 */

#define LDBL_SIG ((TIMES_LOG10_2(LDBL_MANT_DIG) + 1))
#define LDBL_MAX_DIGITS (LDBL_SIG + 3)
#define LDBL_EXPONENT_SIZE (1 + 1 + 4)

#define DBL_MAX_DIGITS (DBL_SIG + 3)
#define DBL_EXPONENT_SIZE (1 + 1 + 3) /* Exponent marker 'e' + sign + digits .*/


/* The sinificant digits + the possible sign + the decimal dot. */
#define LDBL_MANTISSA_SIZE (LDBL_MAX_DIGITS + 1 + 1)
/* Total estimated size that a floating point number can take. */
#define LDBL_SIZE (LDBL_MANTISSA_SIZE + LDBL_EXPONENT_SIZE)

/* The sinificant digits + the possible sign + the decimal dot. */
#define DBL_MANTISSA_SIZE (DBL_MAX_DIGITS + 1 + 1)
/* Total estimated size that a floating point number can take. */
#define DBL_SIZE (DBL_MANTISSA_SIZE + DBL_EXPONENT_SIZE)

#define LONG_EXP_STRING "Le"
#define LONG_G_EXP_STRING "Lg"

#define EXP_STRING "e"

#define G_EXP_STRING "g"
#define DBL_TYPE double

static int edit_long_double(MKCL, int n, long double d, int *sp, char *s, int *ep)
{
  char *exponent;
  char buff[512] = { 0 };
  int length;

  if (isnan(d) || !isfinite(d))
    mkcl_FEerror(env, "Can't print a non-number.", 0);
  if (!isnormal(d))
    mkcl_FEerror(env, "Can't print a subnormal number.", 0);
  {
    long double ld_abs = fabsl(d);

    if ((ld_abs < LDBL_MIN) || (LDBL_MAX < ld_abs))
      mkcl_FEerror(env, "Can't print a denormalized number.", 0);
  }
  if (n < -LDBL_MAX_DIGITS)
    n = LDBL_MAX_DIGITS;
  if (n < 0) {
    long double aux;
    n = -n;
    do {
      snprintf(buff, sizeof(buff), "%- *.*" LONG_EXP_STRING,
               n + 1 + 1 + LDBL_EXPONENT_SIZE, n-1, d);
      errno = 0;
      aux = strtold(buff, NULL);
      if ( errno == ERANGE )
	{
	  char msg_pattern[] = "underflow or overflow: "
	                       "strtold in edit_long_double on: %s";
	  char msg[sizeof(buff) + sizeof(msg_pattern) + 16] = { 0 };
	  
	  snprintf(msg, sizeof(msg), msg_pattern, buff);
	  mkcl_C_lose(env, msg);
	}
      n++;
    } while (d != aux && n <= LDBL_MAX_DIGITS);
    n--;
  } else {
    snprintf(buff, sizeof(buff), "%- *.*" LONG_EXP_STRING, LDBL_SIZE,
             (n <= LDBL_MAX_DIGITS)? (n-1) : (LDBL_MAX_DIGITS-1), d);
  }
  exponent = strchr(buff, 'e');

  /* Get the exponent */
  *ep = strtol(exponent+1, NULL, 10);

  /* Get the sign */
  *sp = (buff[0] == '-') ? -1 : +1;

  /* Get the digits of the mantissa */
  buff[2] = buff[1];

  /* Get the actual number of digits in the mantissa */
  length = exponent - (buff + 2);

  /* The output consists of a string {d1,d2,d3,...,dn}
     with all N digits of the mantissa. If we ask for more
     digits than there are, the last ones are set to zero. */
  if (n <= length) {
    memcpy(s, buff+2, n);
  } else {
    mkcl_index i;
    memcpy(s, buff+2, length);
    for (i = length;  i < n;  i++)
      s[i] = '0';
  }
  s[n] = '\0';
  return length;
}

static int edit_double(MKCL, int n, double d, int *sp, char *s, int *ep)
{
  char *exponent;
  char buff[512] = { 0 };
  int length;

  if (isnan(d) || !isfinite(d))
    mkcl_FEerror(env, "Can't print a non-number.", 0);
  if (!isnormal(d))
    mkcl_FEerror(env, "Can't print a subnormal number.", 0);
  {
    double d_abs = fabs(d);

    if ((d_abs < DBL_MIN) || (DBL_MAX < d_abs))
      mkcl_FEerror(env, "Can't print a denormalized number.", 0);
  }
  if (n < -DBL_MAX_DIGITS)
    n = DBL_MAX_DIGITS;
  if (n < 0) {
    double aux = 0;
    n = -n + 1;
    do {
      snprintf(buff, sizeof(buff), "%- *.*" EXP_STRING, n + 1 + 1 + DBL_EXPONENT_SIZE, n-1, d);
      errno = 0;
      aux = strtod(buff, NULL);
      if ( errno == ERANGE )
	{
	  char msg_pattern[] = "underflow or overflow: "
	                       "strtod in edit_double on: %s";
	  char msg[sizeof(buff) + sizeof(msg_pattern) + 16] = { 0 };
	  
	  snprintf(msg, sizeof(msg), msg_pattern, buff);
	  mkcl_C_lose(env, msg);
	}
      n++;
    } while (d != aux && n <= DBL_MAX_DIGITS);
    n--;
  } else {
    snprintf(buff, sizeof(buff), "%- *.*" EXP_STRING, DBL_SIZE,
             (n <= DBL_MAX_DIGITS)? (n-1) : (DBL_MAX_DIGITS-1), d);
  }
  exponent = strchr(buff, 'e');

  /* Get the exponent */
  *ep = strtol(exponent+1, NULL, 10);

  /* Get the sign */
  *sp = (buff[0] == '-') ? -1 : +1;

  /* Get the digits of the mantissa */
  buff[2] = buff[1];

  /* Get the actual number of digits in the mantissa */
  length = exponent - (buff + 2);

  /* The output consists of a string {d1,d2,d3,...,dn}
     with all N digits of the mantissa. If we ask for more
     digits than there are, the last ones are set to zero. */
  if (n <= length) {
    memcpy(s, buff+2, n);
  } else {
    mkcl_index i;
    memcpy(s, buff+2, length);
    for (i = length;  i < n;  i++)
      s[i] = '0';
  }
  s[n] = '\0';

  return length;
}

static void
write_long_double(MKCL, long double d, mkcl_character e, int n, mkcl_object stream, mkcl_object o)
{
  int exp;
  bool fe_inexact_on = FALSE;
  mkcl_object print_exactly_p = mkcl_symbol_value(env, MK_SI_DYNVAR_print_float_exactly); 

  if (mkcl_Null(print_exactly_p))
    {
      fe_inexact_on = FE_INEXACT & fegetexcept();
      if (fe_inexact_on)
	{
	  /* printf("\nwrite_long_double: turning off FE_INEXACT!\n"); fflush(NULL); */
	  fedisableexcept(FE_INEXACT);
	}
    }
  else
    { printf("\nwrite_long_double told to print exactly!\n"); fflush(NULL); }

  if (!mkcl_Null(mk_cl_fboundp(env, MK_SI_output_float)))
    {
      mkcl_funcall2(env, MK_SI_output_float->symbol.gfdef, o, stream);
    }
  else if (isnan(d)) {
    if (mkcl_print_readably(env)) {
      mkcl_FEprint_not_readable(env, mkcl_make_longfloat(env, d));
    }
    mkcl_funcall2(env, MK_SI_output_float_nan->symbol.gfdef, o, stream);
  }
  else if (!isfinite(d)) {
    mkcl_funcall2(env, MK_SI_output_float_infinity->symbol.gfdef, o, stream);
  }
  else {
    if (d < 0) {
      mkcl_write_char(env, '-', stream);
      d = -d;
    }
    if (d == 0.0) {
#if defined(MKCL_SIGNED_ZERO) && defined(signbit)
      if (signbit(d))
	write_str(env, "-0.0", stream);
      else
#endif
	write_str(env, "0.0", stream);
      exp = 0;
    } else if (d < 1e-3 || d > 1e7) {
      int sign;
      char buff[LDBL_MANTISSA_SIZE + 1];
      n = edit_long_double(env, -n, d, &sign, buff, &exp);
      mkcl_write_char(env, buff[0], stream);
      mkcl_write_char(env, '.', stream);
      for (;  --n > 1; ) {
	if (buff[n] != '0') {
	  break;
	}
	buff[n] = '\0';
      }
      write_str(env, buff+1, stream);
    } else {
      char buff[512] = { 0 };
      int i;
      long double aux;
      /* Print in fixed point notation with enough number of
       * digits to preserve all information when reading again
       */
      do {
	snprintf(buff, sizeof(buff), "%0*.*" LONG_G_EXP_STRING, LDBL_MANTISSA_SIZE, n, d);
	aux = strtod(buff, NULL);

	if (n < LDBL_SIG) aux = (double)aux; /* What is the use of this? JCB */
	if (n < DBL_SIG) aux = (float)aux; /* What is the use of this? JCB */

	n++;
      } while (aux != d && n <= LDBL_MAX_DIGITS);
      n--;
      /* We look for the first nonzero character. There is
       * always one because our floating point number is not
       * zero.*/
      for (i = 0; buff[i] == '0' && buff[i+1] != '.'; i++)
	;
      write_str(env, buff + i, stream);
      if (strchr(buff, '.') == 0) {
	write_str(env, ".0", stream);
      }
      exp = 0;
    }
    if (exp || e) {
      if (e == 0)
	e = 'E';
      mkcl_write_char(env, e, stream);
      if (exp < 0) {
	mkcl_write_char(env, '-', stream);
	exp = -exp;
      }
      write_decimal(env, exp, stream);
    }
  }

  feclearexcept(FE_INEXACT); /* Clear leftovers from casting. */ /* should it be FE_ALL_EXCEPT? JCB*/
  if (fe_inexact_on)
    {
      /* printf("\nwrite_long_double: turning on FE_INEXACT!\n"); fflush(NULL); */
      feenableexcept(FE_INEXACT);
    }

}

static void
write_double(MKCL, double d, mkcl_character e, int n, mkcl_object stream, mkcl_object o)
{
  int exp;
  bool fe_inexact_on = FALSE;
  mkcl_object print_exactly_p = mkcl_symbol_value(env, MK_SI_DYNVAR_print_float_exactly); 

  if (mkcl_Null(print_exactly_p))
    {
      fe_inexact_on = FE_INEXACT & fegetexcept();
      if (fe_inexact_on)
	{
	  /* printf("\nwrite_double: turning off FE_INEXACT!\n"); fflush(NULL); */
	  fedisableexcept(FE_INEXACT);
	}
    }
  else
    { printf("\nwrite_double told to print exactly!\n"); fflush(NULL); }

  if (!mkcl_Null(mk_cl_fboundp(env, MK_SI_output_float)))
    {
      mkcl_funcall2(env, MK_SI_output_float->symbol.gfdef, o, stream);
    }
  else if (isnan(d)) {
    if (mkcl_print_readably(env)) {
      mkcl_FEprint_not_readable(env, mkcl_make_doublefloat(env, d));
    }
    mkcl_funcall2(env, MK_SI_output_float_nan->symbol.gfdef, o, stream);
  }
  else if (!isfinite(d)) {
    mkcl_funcall2(env, MK_SI_output_float_infinity->symbol.gfdef, o, stream);
  }
  else {
    if (d < 0) {
      mkcl_write_char(env, '-', stream);
      d = -d;
    }
    if (d == 0.0) {
#if defined(MKCL_SIGNED_ZERO) && defined(signbit)
      if (signbit(d))
	write_str(env, "-0.0", stream);
      else
#endif
	write_str(env, "0.0", stream);
      exp = 0;
    } else if (d < 1e-3 || d > 1e7) {
      int sign;
      char buff[DBL_MANTISSA_SIZE + 1];
      n = edit_double(env, -n, d, &sign, buff, &exp);
      mkcl_write_char(env, buff[0], stream);
      mkcl_write_char(env, '.', stream);
      for (;  --n > 1; ) {
	if (buff[n] != '0') {
	  break;
	}
	buff[n] = '\0';
      }
      write_str(env, buff+1, stream);
    } else {
#if 0
      char buff[DBL_MANTISSA_SIZE + 1] = { 0 };
      char buff[512] = { 0 };
#else
      char buff[512] = { 0 };
#endif
      int i;
      double aux;
      /* Print in fixed point notation with enough number of
       * digits to preserve all information when reading again
       */
      do {
	snprintf(buff, sizeof(buff), "%0*.*" G_EXP_STRING, DBL_MANTISSA_SIZE, n, d);
        aux = strtod(buff, NULL);
	if (n < DBL_SIG) aux = (float)aux; /* What is the use of this? JCB */

	n++;
      } while (aux != d && n <= DBL_MAX_DIGITS);
      n--;
      /* We look for the first nonzero character. There is
       * always one because our floating point number is not
       * zero.*/
      for (i = 0; buff[i] == '0' && buff[i+1] != '.'; i++)
	;
      write_str(env, buff + i, stream);
      if (strchr(buff, '.') == 0) {
	write_str(env, ".0", stream);
      }
      exp = 0;
    }
    if (exp || e) {
      if (e == 0)
	e = 'E';
      mkcl_write_char(env, e, stream);
      if (exp < 0) {
	mkcl_write_char(env, '-', stream);
	exp = -exp;
      }
      write_decimal(env, exp, stream);
    }
  }

  feclearexcept(FE_INEXACT); /* Clear leftovers from casting. */ /* should it be FE_ALL_EXCEPT? JCB*/
  if (fe_inexact_on)
    {
      /* printf("\nwrite_double: turning on FE_INEXACT!\n"); fflush(NULL); */
      feenableexcept(FE_INEXACT);
    }

}


struct powers {
  mkcl_object number;
  mkcl_index n_digits;
  int base;
};

static void
do_write_integer(MKCL, mkcl_object x, struct powers *powers, mkcl_index len,
		 mkcl_object stream)
{
  mkcl_object left;
  do {
    if (MKCL_FIXNUMP(x)) {
      write_positive_fixnum(env, mkcl_fixnum_to_word(x), powers->base, len, stream);
      return;
    }
    while (mkcl_number_compare(env, x, powers->number) < 0) {
      if (len)
	write_positive_fixnum(env, 0, powers->base, len, stream);
      powers--;
    }
    left = mkcl_floor2(env, x, powers->number);
    x = MKCL_VALUES(1);
    if (len) len -= powers->n_digits;
    do_write_integer(env, left, powers-1, len, stream);
    len = powers->n_digits;
    powers--;
  } while(1);
}

static void
write_bignum(MKCL, mkcl_object x, mkcl_object stream)
{
  int base = mkcl_print_base(env);
  mkcl_index str_size = mpz_sizeinbase(x->big.big_num, base);
  mkcl_word num_powers = mkcl_word_bit_length(str_size-1);
#if 0
  struct powers powers[num_powers]; /* VLA */
#else
  mkcl_VLA(env, struct powers, powers, num_powers);
#endif
    mkcl_object p;
    mkcl_index i, n_digits;
    powers[0].number = p = MKCL_MAKE_FIXNUM(base);
    powers[0].n_digits = n_digits = 1;
    powers[0].base = base;
    for (i = 1; i < num_powers; i++) {
      powers[i].number = p = mkcl_times(env, p, p);
      powers[i].n_digits = n_digits = 2*n_digits;
      powers[i].base = base;
    }
    if (mkcl_minusp(env, x)) {
      mkcl_write_char(env, '-', stream);
      x = mkcl_negate(env, x);
    }
    do_write_integer(env, x, &powers[num_powers-1], 0, stream);
}

static bool
all_dots(MKCL, mkcl_object s)
{
  mkcl_index i;
  mkcl_index len = mkcl_string_length(env, s);
  for (i = 0;  i < len;  i++)
    if (mkcl_char(env, s, i) != '.')
      return 0;
  return 1;
}

static bool
needs_to_be_escaped(MKCL, mkcl_object s, mkcl_object readtable, mkcl_object print_case)
{
  int action = readtable->readtable.read_case;
  mkcl_index i;
  if (potential_number_p(env, s, mkcl_print_base(env)))
    return 1;
  /* The value of *PRINT-ESCAPE* is T. We need to check whether the
   * symbol name S needs to be escaped. This will happen if it has some
   * strange character, or if it has a lowercase character (because such
   * a character cannot be read with the standard readtable) or if the
   * string has to be escaped according to readtable case and the rules
   * of 22.1.3.3.2. */
  mkcl_index len = mkcl_string_length(env, s);
  for (i = 0; i < len;  i++) {
    mkcl_character c = mkcl_char(env, s, i);
    enum mkcl_chattrib syntax = mkcl_readtable_get(env, readtable, c, 0);
    if (syntax != mkcl_cat_constituent || mkcl_invalid_constituent_character_p(c) || (c) == ':')
      return 1;
    if ((action == mkcl_case_downcase) && mkcl_upper_case_p(c))
      return 1;
    if (mkcl_lower_case_p(c))
      return 1;
  }
  return 0;
}

#define needs_to_be_inverted(s) (mkcl_string_case(s) != mkcl_mixedcase_string)

static void
write_symbol_string(MKCL, mkcl_object s, enum mkcl_readtable_case action, mkcl_object print_case,
		    mkcl_object stream, bool escape)
{
  mkcl_index i;
  bool capitalize;
  if (action == mkcl_case_invert) {
    if (!needs_to_be_inverted(s))
      action = mkcl_case_preserve;
  }
  if (escape)
    mkcl_write_char(env, '|', stream);
  capitalize = 1;

  mkcl_index len = mkcl_string_length(env, s);
  for (i = 0;  i < len;  i++) {
    int c = mkcl_char(env, s, i);
    if (escape) {
      if (c == '|' || c == '\\') {
	mkcl_write_char(env, '\\', stream);
      }
    } else if (action != mkcl_case_preserve) {
      if (mkcl_upper_case_p(c)) {
	if ((action == mkcl_case_invert) ||
	    ((action == mkcl_case_upcase) &&
	     ((print_case == MK_KEY_downcase) ||
	      ((print_case == MK_KEY_capitalize) && !capitalize))))
	  {
	    c = mkcl_char_downcase(c);
	  }
	capitalize = 0;
      } else if (mkcl_lower_case_p(c)) {
	if ((action == mkcl_case_invert) ||
	    ((action == mkcl_case_downcase) &&
	     ((print_case == MK_KEY_upcase) ||
	      ((print_case == MK_KEY_capitalize) && capitalize))))
	  {
	    c = mkcl_char_upcase(c);
	  }
	capitalize = 0;
      } else {
	capitalize = !mkcl_alphanumericp(c);
      }
    }
    mkcl_write_char(env, c, stream);
  }
  if (escape)
    mkcl_write_char(env, '|', stream);
}

static void
write_symbol(MKCL, mkcl_object x, mkcl_object stream)
{
  mkcl_object print_package = mkcl_symbol_value(env, MK_SI_DYNVAR_print_package);
  mkcl_object readtable = mkcl_current_readtable(env);
  mkcl_object print_case = mkcl_print_case(env);
  mkcl_object package;
  mkcl_object name;
  int intern_flag;
  bool print_readably = mkcl_print_readably(env);

  if (mkcl_Null(x)) {
    package = mk_cl_Cnil_symbol->symbol.hpack;
    name = mk_cl_Cnil_symbol->symbol.name;
  } else {
    package = x->symbol.hpack;
    name = x->symbol.name;
  }

  if (!print_readably && !mkcl_print_escape(env)) {
    write_symbol_string(env, name, readtable->readtable.read_case,
			print_case, stream, 0);
    return;
  }
  /* From here on, print-escape is true which means that it should
   * be possible to recover the same symbol by reading it with
   * the standard readtable (which has readtable-case = :UPCASE)
   */
  if (mkcl_Null(package)) {
    if (mkcl_print_gensym(env) || print_readably)
      write_str(env, "#:", stream);
  } else if (package == mkcl_core.keyword_package) {
    mkcl_write_char(env, ':', stream);
  } else if ((print_package != mk_cl_Cnil && package != print_package)
	     || (mkcl_find_symbol(env, x, mkcl_current_package(env), &intern_flag)!=x
		 && x != mk_cl_Cnil_symbol)
	     || intern_flag == 0)
    {
      mkcl_object name = package->pack.name;
      write_symbol_string(env, name, readtable->readtable.read_case,
			  print_case, stream,
			  needs_to_be_escaped(env, name, readtable, print_case));
      if ((mkcl_find_symbol(env, x, package, &intern_flag) != x) && x != mk_cl_Cnil_symbol)
	{
#if 0
	  mkcl_lose(env, "can't print symbol"); /* A bit too radical. JCB */
#else
	  mkcl_FEerror(env, "Corrupted symbol, symbol-name = ~S", 1, mkcl_symbol_name(env, x));
#endif
	}
      if ((print_package != mk_cl_Cnil && package != print_package)
	  || intern_flag == MKCL_SYMBOL_IS_INTERNAL) {
	write_str(env, "::", stream);
      } else if (intern_flag == MKCL_SYMBOL_IS_EXTERNAL) {
	mkcl_write_char(env, ':', stream);
      } else {
	mkcl_FEerror(env, "Pathological symbol --- cannot print.", 0);
      }
    }
  write_symbol_string(env, name, readtable->readtable.read_case, print_case, stream,
		      needs_to_be_escaped(env, name, readtable, print_case) ||
		      all_dots(env, name));
}

static void
write_character(MKCL, mkcl_character i, mkcl_object stream)
{
  if (!mkcl_print_escape(env) && !mkcl_print_readably(env)) {
    mkcl_write_char(env, i, stream);
  } else {
    write_str(env, "#\\", stream);
    if ((i > 0x020) && (i < 0x07F)) /* ASCII printable character? */
      {
	mkcl_write_char(env, i, stream);
      }
#if 0 /* This turns out to be a bad idea. JCB */
    else if ((0x0A0 < i) && (i <= 0x0FF)) /* Upper part of ISO-8859-1 printable character? */
      {
	mkcl_write_char(env, i, stream);
      }
#endif
    else if (i < MKCL_BASE_CHAR_CODE_LIMIT)
      {
	mkcl_object name = mk_cl_char_name(env, MKCL_CODE_CHAR(i));
	if (mkcl_Null(name))
	  write_str(env, "U????", stream); /* Somehow we don't know its name! Should never happen. */
	else
	  write_str(env, (char*)name->base_string.self, stream);
      } 
    else
      {
        mkcl_object ext_names = MKCL_SYM_VAL(env, MK_SI_DYNVAR_extended_character_names);
        mkcl_object output = ((ext_names != mk_cl_Cnil)
                              ? mkcl_gethash_safe(env, MKCL_MAKE_FIXNUM(i), ext_names, mk_cl_Cnil)
                              : mk_cl_Cnil);

        if (!mkcl_Null(output))
	  {
	    mkcl_bds_bind(env, MK_CL_DYNVAR_print_escape, mk_cl_Cnil);
	    mkcl_bds_bind(env, MK_CL_DYNVAR_print_readably, mk_cl_Cnil);
	    mk_si_write_ugly_object(env, output, stream);
	    mkcl_bds_unwind1(env);
	    mkcl_bds_unwind1(env);
	  }
        else
          {
            int  index = 0;
            char name[20] = { '\0' };
	
            if (i < 0x010000) /* Are we confined to 16 bits? */
              snprintf(name, sizeof(name), "U%04x", i);
            else if (i < 0x0110000) /* valid Unicode character? */
              snprintf(name, sizeof(name), "U%06x", i);
            else
              snprintf(name, sizeof(name), "U+????"); /* character is above valid Unicode range. */
            while(name[index])
              mkcl_write_char(env, name[index++], stream);
          }
      }
  }
}

static void
write_array(MKCL, mkcl_object x, mkcl_object stream)
{
  const mkcl_index *adims;
  mkcl_index subscripts[MKCL_ARANKLIM];
  mkcl_word n, j, m, k, i;
  mkcl_word print_length;
  mkcl_word print_level;
  const bool readably = mkcl_print_readably(env);

  adims = x->array.dims;
  n = x->array.rank;
  if (n > MKCL_ARANKLIM) n = MKCL_ARANKLIM; /* limit risks of array overflow. should we signal an error? */

  if (readably) {
    print_length = MKCL_MOST_POSITIVE_FIXNUM;
    print_level = MKCL_MOST_POSITIVE_FIXNUM;
  } else {
    if (!mkcl_print_array(env)) {
      write_str(env, "#<array ", stream);
      write_addr(env, x, stream);
      mkcl_write_char(env, '>', stream);
      return;
    }
    print_level = mkcl_print_level(env);
    print_length = mkcl_print_length(env);
  }
  mkcl_write_char(env, '#', stream);
  if (print_level == 0)
    return;
  if (readably) {
    mkcl_write_char(env, 'A', stream);
    mkcl_write_char(env, '(', stream);
    mk_si_write_object(env, mkcl_elttype_to_symbol(env, mkcl_array_elttype(env, x)), stream);
    mkcl_write_char(env, ' ', stream);
    if (n > 0) {
      mkcl_write_char(env, '(', stream);
      for (j=0; j<n; j++) {
	mk_si_write_object(env, MKCL_MAKE_FIXNUM(adims[j]), stream);
	if (j < n-1)
	  mkcl_write_char(env, ' ', stream);
      }
      mkcl_write_char(env, ')', stream);
    } else {
      mk_si_write_object(env, mk_cl_Cnil, stream);
    }
    mkcl_write_char(env, ' ', stream);
  } else {
    write_decimal(env, n, stream);
    mkcl_write_char(env, 'A', stream);
  }

  if (print_level >= n) {
    /* We can write the elements of the array */
    print_level -= n;
    mkcl_bds_bind(env, MK_CL_DYNVAR_print_level, MKCL_MAKE_FIXNUM(print_level));
  } else {
    /* The elements of the array are not printed */
    n = print_level;
    print_level = -1;
  }

  for (j = 0;  j < n;  j++)
    subscripts[j] = 0;
  if (n == 0)
    mk_si_write_object(env, mk_cl_Cnil, stream);
  else
    for (m = 0, j = 0;;) {
      for (i = j;  i < n;  i++) {
        if (subscripts[i] == 0) {
          mkcl_write_char(env, '(', stream);
          if (adims[i] == 0) {
            mkcl_write_char(env, ')', stream);
	  
            j = i-1;
            k = 0;
            goto INC;
          }
        }
        if (subscripts[i] > 0)
          mkcl_write_char(env, ' ', stream);
        if (subscripts[i] >= print_length) {
          write_str(env, "...)", stream);
          k=adims[i]-subscripts[i];
          subscripts[i] = 0;
          for (j = i+1;  j < n;  j++)
            k *= adims[j];
          j = i-1;
          goto INC;
        }
      }
      /* FIXME: This conses! */
      if (print_level >= 0)
        mk_si_write_object(env, mkcl_aref_index(env, x, m), stream);
      else
        mkcl_write_char(env, '#', stream);
      j = n-1;
      k = 1;

    INC:
      while (j >= 0) {
        if (++subscripts[j] < adims[j])
          break;
        subscripts[j] = 0;
        mkcl_write_char(env, ')', stream);
      
        --j;
      }
      if (j < 0)
        break;
      m += k;
    }
  if (print_level >= 0) {
    mkcl_bds_unwind1(env);
  }
  if (readably) {
    mkcl_write_char(env, ')', stream);
  }
}

static void
write_vector(MKCL, mkcl_object x, mkcl_object stream)
{
  const bool vector = TRUE;
  const mkcl_index adim = x->vector.fillp;
  const bool readably = mkcl_print_readably(env);
  mkcl_word print_length;
  mkcl_word print_level;

  if (readably) {
    print_length = MKCL_MOST_POSITIVE_FIXNUM;
    print_level = MKCL_MOST_POSITIVE_FIXNUM;
  } else {
    if (!mkcl_print_array(env)) {
      write_str(env, "#<vector ", stream);
      write_addr(env, x, stream);
      mkcl_write_char(env, '>', stream);
      return;
    }
    print_level = mkcl_print_level(env);
    print_length = mkcl_print_length(env);
  }
  mkcl_write_char(env, '#', stream);

  if (print_level == 0)
    return;

  if (readably) { /* special implementation-defined *readeable* syntax. */
    mkcl_write_char(env, 'A', stream);
    mkcl_write_char(env, '(', stream);
    mk_si_write_object(env, mkcl_elttype_to_symbol(env, mkcl_array_elttype(env, x)), stream);
    mkcl_write_char(env, ' ', stream);
      mkcl_write_char(env, '(', stream);
      mk_si_write_object(env, MKCL_MAKE_FIXNUM(adim), stream);
      mkcl_write_char(env, ')', stream);
    mkcl_write_char(env, ' ', stream);
  }

  mkcl_bds_bind(env, MK_CL_DYNVAR_print_level, MKCL_MAKE_FIXNUM(print_level - 1));

  mkcl_write_char(env, '(', stream);
  {
    mkcl_index i = 0;
    for (i = 0; i < adim; i++)
      {
        if (i >= print_length)
          {
            write_str(env, "...", stream);
            break;
          }
        if (i > 0)
          mkcl_write_char(env, ' ', stream);
        mk_si_write_object(env, mkcl_aref_index(env, x, i), stream);
      }
  }
  mkcl_write_char(env, ')', stream);

  mkcl_bds_unwind1(env);
  if (readably) { /* match special syntax initiated here above. */
    mkcl_write_char(env, ')', stream);
  }
}

mkcl_object
mk_si_write_ugly_object(MKCL, mkcl_object x, mkcl_object stream)
{
  mkcl_object r, y;
  mkcl_word i;
  mkcl_index ndx, k;

  mkcl_call_stack_check(env);
  if (x == MKCL_OBJNULL) {
    if (mkcl_print_readably(env)) mkcl_FEprint_not_readable(env, x);
    write_str(env, "#<OBJNULL>", stream);
    mkcl_return_value(x);
  }
  switch (mkcl_type_of(x)) {

  case mkcl_t_null:
    write_symbol(env, mk_cl_Cnil_symbol, stream);
    break;

  case mkcl_t_fixnum: {
    bool print_radix = mkcl_print_radix(env);
    int print_base = mkcl_print_base(env);
    if (print_radix && print_base != 10)
      write_base(env, print_base, stream);
    if (x == MKCL_MAKE_FIXNUM(0)) {
      mkcl_write_char(env, '0', stream);
    } else if (MKCL_FIXNUM_MINUSP(x)) {
      mkcl_write_char(env, '-', stream);
      write_positive_fixnum(env, -mkcl_fixnum_to_word(x), print_base, 0, stream);
    } else {
      write_positive_fixnum(env, mkcl_fixnum_to_word(x), print_base, 0, stream);
    }
    if (print_radix && print_base == 10) {
      mkcl_write_char(env, '.', stream);
    }
    break;
  }
  case mkcl_t_bignum: {
    bool print_radix = mkcl_print_radix(env);
    int print_base = mkcl_print_base(env);
    if (print_radix && print_base != 10)
      write_base(env, print_base, stream);
    write_bignum(env, x, stream);

    if (print_radix && print_base == 10)
      mkcl_write_char(env, '.', stream);
    break;
  }
  case mkcl_t_ratio: {
    if (mkcl_print_radix(env)) {
      write_base(env, mkcl_print_base(env), stream);
    }
    mkcl_bds_bind(env, MK_CL_DYNVAR_print_radix, mk_cl_Cnil);
    mk_si_write_ugly_object(env, x->ratio.num, stream);
    mkcl_write_char(env, '/', stream);
    mk_si_write_ugly_object(env, x->ratio.den, stream);
    mkcl_bds_unwind1(env);
    break;
  }
  case mkcl_t_singlefloat:
    r = mkcl_symbol_value(env, MK_CL_DYNVAR_read_default_float_format);
    write_double(env, mkcl_single_float(x), (r == MK_CL_single_float || r == MK_CL_short_float)? 0 : 's',
		 FLT_SIG, stream, x);
    break;
#ifdef MKCL_LONG_FLOAT
  case mkcl_t_doublefloat:
    r = mkcl_symbol_value(env, MK_CL_DYNVAR_read_default_float_format);
    write_double(env, mkcl_double_float(x), (r == MK_CL_double_float)? 0 : 'd', DBL_SIG, stream,
		 x);
    break;
  case mkcl_t_longfloat:
    r = mkcl_symbol_value(env, MK_CL_DYNVAR_read_default_float_format);
    write_long_double(env, mkcl_long_float(x), (r == MK_CL_long_float)? 0 : 'l',
		      LDBL_SIG, stream, x);
    break;
#else
  case mkcl_t_doublefloat:
    r = mkcl_symbol_value(env, MK_CL_DYNVAR_read_default_float_format);
    write_double(env, mkcl_double_float(x), (r == MK_CL_double_float || r == MK_CL_long_float)? 0 : 'd',
		 DBL_SIG, stream, x);
    break;
#endif
  case mkcl_t_complex:
    write_str(env, "#C(", stream);
    mk_si_write_ugly_object(env, x->_complex.real, stream);
    mkcl_write_char(env, ' ', stream);
    mk_si_write_ugly_object(env, x->_complex.imag, stream);
    mkcl_write_char(env, ')', stream);
    break;

  case mkcl_t_character: {
    write_character(env, MKCL_CHAR_CODE(x), stream);
  }
    break;

  case mkcl_t_symbol:
    write_symbol(env, x, stream);
    break;

  case mkcl_t_array:
    write_array(env, x, stream);
    break;

  case mkcl_t_vector:
    write_vector(env, x, stream);
    break;

  case mkcl_t_string:
    if (!mkcl_print_escape(env) && !mkcl_print_readably(env)) {
      for (ndx = 0;  ndx < x->string.fillp;  ndx++)
	mkcl_write_char(env, x->string.self[ndx], stream);
      break;
    }
    mkcl_write_char(env, '"', stream);
    for (ndx = 0;  ndx < x->string.fillp;  ndx++) {
      mkcl_character c = x->string.self[ndx];
      if (c == '"' || c == '\\')
	mkcl_write_char(env, '\\', stream);
      mkcl_write_char(env, c, stream);
    }
    mkcl_write_char(env, '"', stream);
    break;

  case mkcl_t_base_string:
    if (!mkcl_print_escape(env) && !mkcl_print_readably(env)) {
      for (ndx = 0;  ndx < x->base_string.fillp;  ndx++)
	mkcl_write_char(env, x->base_string.self[ndx], stream);
      break;
    }
    mkcl_write_char(env, '"', stream);
    for (ndx = 0;  ndx < x->base_string.fillp;  ndx++) {
      int c = x->base_string.self[ndx];
      if (c == '"' || c == '\\')
	mkcl_write_char(env, '\\', stream);
      mkcl_write_char(env, c, stream);
    }
    mkcl_write_char(env, '"', stream);
    break;

  case mkcl_t_bitvector:
    if (!mkcl_print_array(env) && !mkcl_print_readably(env)) {
      write_str(env, "#<bit-vector ", stream);
      write_addr(env, x, stream);
      mkcl_write_char(env, '>', stream);
      break;
    }
    write_str(env, "#*", stream);
    for (ndx = 0;  ndx < x->vector.fillp;  ndx++)
      {
	mkcl_index i = ndx + x->vector.bit_offset;

	if (mkcl_bit_bundle(x->vector.self.bit, i) & mkcl_bundle_bit_mask(i))
	  mkcl_write_char(env, '1', stream);
	else
	  mkcl_write_char(env, '0', stream);
      }
    break;

  case mkcl_t_cons: {
    bool circle;
    mkcl_word print_level, print_length;

    if (MKCL_CAR(x) == MK_SI_HASH_BANG) {
      write_str(env, "#!", stream);
      x = MKCL_CDR(x);
      return mk_si_write_object(env, x, stream);
    }
    if (MKCL_CONSP(MKCL_CDR(x)) && mkcl_Null(MKCL_CDDR(x))) {
      if (MKCL_CAR(x) == MK_CL_quote) {
	mkcl_write_char(env, '\'', stream);
	x = MKCL_CADR(x);
	return mk_si_write_object(env, x, stream);
      }
      if (MKCL_CAR(x) == MK_CL_function) {
	mkcl_write_char(env, '#', stream);
	mkcl_write_char(env, '\'', stream);
	x = MKCL_CADR(x);
	return mk_si_write_object(env, x, stream);
      }
      if (MKCL_CAR(x) == MK_SI_quasiquote) {
	mkcl_write_char(env, '`', stream);
	x = MKCL_CADR(x);
	return mk_si_write_object(env, x, stream);
      }
      if (MKCL_CAR(x) == MK_SI_unquote) {
	mkcl_write_char(env, ',', stream);
	x = MKCL_CADR(x);
	return mk_si_write_object(env, x, stream);
      }
      if (MKCL_CAR(x) == MK_SI_unquote_splice) {
	write_str(env, ",@@", stream);
	x = MKCL_CADR(x);
	return mk_si_write_object(env, x, stream);
      }
      if (MKCL_CAR(x) == MK_SI_unquote_nsplice) {
	write_str(env, ",.", stream);
	x = MKCL_CADR(x);
	return mk_si_write_object(env, x, stream);
      }
    }
    circle = mkcl_print_circle(env);
    if (mkcl_print_readably(env)) {
      print_level = MKCL_MOST_POSITIVE_FIXNUM;
      print_length = MKCL_MOST_POSITIVE_FIXNUM;
    } else {
      print_level = mkcl_print_level(env);
      print_length = mkcl_print_length(env);
    }
    if (print_level == 0) {
      mkcl_write_char(env, '#', stream);
      break;
    }

    mkcl_bds_bind(env, MK_CL_DYNVAR_print_level, MKCL_MAKE_FIXNUM(print_level-1));
    mkcl_write_char(env, '(', stream);
    for (i = 0;  ;  i++) {
      if (i >= print_length) {
	write_str(env, "...", stream);
	break;
      }
      y = MKCL_CAR(x);
      x = MKCL_CDR(x);
      mk_si_write_object(env, y, stream);
      /* FIXME! */
      if (x == MKCL_OBJNULL || MKCL_ATOM(x) ||
	  (circle && object_will_print_as_hash(env, x)))
	{
	  if (x != mk_cl_Cnil) {
	    mkcl_write_char(env, ' ', stream);
	    write_str(env, ". ", stream);
	    mk_si_write_object(env, x, stream);
	  }
	  break;
	}
#if 0 /* What is the purpose of this piece of code over the alternative below, I fail to see! JCB */
      if (i == 0 && y != MKCL_OBJNULL && mkcl_type_of(y) == mkcl_t_symbol)
	mkcl_write_char(env, ' ', stream);
      else
	mkcl_write_char(env, ' ', stream);
#else
      mkcl_write_char(env, ' ', stream);
#endif
    }
    /* RIGHT_PAREN: */
    mkcl_write_char(env, ')', stream);
    mkcl_bds_unwind1(env);
    break;
  }
  case mkcl_t_package:
    if (mkcl_print_readably(env)) mkcl_FEprint_not_readable(env, x);
#if 0
    write_str(env, x->pack.closed ? "#<closed package " : "#<package ", stream);
#else
    write_str(env, "#<package ", stream);
#endif
    mk_si_write_ugly_object(env, x->pack.name, stream);
    write_str(env, ">", stream);
    break;

  case mkcl_t_hashtable:
    if (mkcl_print_readably(env)) mkcl_FEprint_not_readable(env, x);
    write_str(env, "#<hash-table ", stream);
    write_addr(env, x, stream);
    mkcl_write_char(env, '>', stream);
    break;

  case mkcl_t_stream:
    if (mkcl_print_readably(env)) mkcl_FEprint_not_readable(env, x);
    write_str(env, x->stream.closed ? "#<closed " : "#<", stream);
    switch ((enum mkcl_smmode)x->stream.mode) {
    case mkcl_smm_input_file:
      write_str(env, "input stream ", stream);
      mk_si_write_ugly_object(env, MKCL_IO_STREAM_FILENAME(x), stream);
      break;
    case mkcl_smm_input:
      write_str(env, "input stream (C stdio) ", stream);
      mk_si_write_ugly_object(env, MKCL_IO_STREAM_FILENAME(x), stream);
      break;
    case mkcl_smm_output_file:
      write_str(env, "output stream ", stream);
      mk_si_write_ugly_object(env, MKCL_IO_STREAM_FILENAME(x), stream);
      break;
    case mkcl_smm_output:
      write_str(env, "output stream (C stdio) ", stream);
      mk_si_write_ugly_object(env, MKCL_IO_STREAM_FILENAME(x), stream);
      break;
    case mkcl_smm_io_file:
      write_str(env, "io stream ", stream);
      mk_si_write_ugly_object(env, MKCL_IO_STREAM_FILENAME(x), stream);
      break;
    case mkcl_smm_io:
      write_str(env, "io stream (C stdio) ", stream);
      mk_si_write_ugly_object(env, MKCL_IO_STREAM_FILENAME(x), stream);
      break;

    case mkcl_smm_input_socket:
      write_str(env, "input socket stream ", stream);
      mk_si_write_ugly_object(env, MKCL_IO_STREAM_FILENAME(x), stream);
      break;

    case mkcl_smm_output_socket:
      write_str(env, "output socket stream ", stream);
      mk_si_write_ugly_object(env, MKCL_IO_STREAM_FILENAME(x), stream);
      break;

    case mkcl_smm_io_socket:
      write_str(env, "i/o socket stream ", stream);
      mk_si_write_ugly_object(env, MKCL_IO_STREAM_FILENAME(x), stream);
      break;

    case mkcl_smm_probe:
      write_str(env, "probe stream ", stream);
      mk_si_write_ugly_object(env, MKCL_IO_STREAM_FILENAME(x), stream);
      break;

    case mkcl_smm_synonym:
      write_str(env, "synonym stream to ", stream);
      mk_si_write_ugly_object(env, MKCL_SYNONYM_STREAM_SYMBOL(x), stream);
      break;

    case mkcl_smm_broadcast:
      write_str(env, "broadcast stream ", stream);
      write_addr(env, x, stream);
      break;

    case mkcl_smm_concatenated:
      write_str(env, "concatenated stream ", stream);
      write_addr(env, x, stream);
      break;

    case mkcl_smm_two_way:
      write_str(env, "two-way stream ", stream);
      write_addr(env, x, stream);
      break;

    case mkcl_smm_echo:
      write_str(env, "echo stream ", stream);
      write_addr(env, x, stream);
      break;

    case mkcl_smm_string_input:
      write_str(env, "string-input stream from \"", stream);
      y = MKCL_STRING_INPUT_STREAM_STRING(x);
      k = mkcl_string_length(env, y);
      for (ndx = 0;  ndx < k && ndx < 16;  ndx++)
	mkcl_write_char(env, mkcl_char(env, y, ndx), stream);
      if (k > 16)
	write_str(env, "...", stream);
      mkcl_write_char(env, '"', stream);
      break;

    case mkcl_smm_string_output:
      write_str(env, "string-output stream ", stream);
      write_addr(env, x, stream);
      break;

    default:
      mkcl_lose(env, "si::write_ugly_object: illegal stream mode");
    }
    mkcl_write_char(env, '>', stream);
    break;

  case mkcl_t_random:
    if (mkcl_print_readably(env)) {
      write_str(env, "#$", stream);
      write_vector(env, x->random.value, stream);
    } else {
      write_str(env, "#<random-state ", stream);
      write_addr(env, x->random.value, stream);
      write_str(env, "#>", stream);
    }
    break;

#if 0 /* ndef CLOS */
  case mkcl_t_structure: {
    mkcl_object print_function;
    if (mkcl_type_of(x->str.name) != mkcl_t_symbol)
      mkcl_FEwrong_type_argument(MK_CL_symbol, x->str.name);
    print_function = mk_si_get_sysprop(x->str.name, MK_SI_structure_print_function);
    if (mkcl_Null(print_function) || !mkcl_print_structure()) /* Broken! JCB */
      {
	write_str("#S", stream);
	/* structure_to_list conses slot names and values into a list to be printed.
	 * print shouldn't allocate memory - Beppe
	 */
	x = structure_to_list(x); /* Broken! since at least 1993 AFAICT. JCB */
	mk_si_write_object(x, stream);
      } else {
#if 0
      call_structure_print_function(env, print_function, x, stream);
#else
      mkcl_funcall3(env, print_function, x, stream, MKCL_MAKE_FIXNUM(0));
#endif
    }
    break;
  }
#endif /* CLOS */
  case mkcl_t_readtable:
    if (mkcl_print_readably(env)) mkcl_FEprint_not_readable(env, x);
    write_str(env, "#<readtable ", stream);
    write_addr(env, x, stream);
    mkcl_write_char(env, '>', stream);
    break;

  case mkcl_t_pathname:
    write_pathname(env, x, stream);
    break;

  case mkcl_t_bclosure:
    if (mkcl_print_readably(env))
      mkcl_FEprint_not_readable(env, x);
    else
      {
	mkcl_object name = x->bclosure.code->bytecode.name;
	write_str(env, "#<bytecompiled-closure ", stream);
	if (name != mk_cl_Cnil)
	  mk_si_write_ugly_object(env, name, stream);
	else
	  write_addr(env, x, stream);
	mkcl_write_char(env, '>', stream);
      }
    break;
  case mkcl_t_bytecode:
    if (mkcl_print_readably(env))
      mkcl_FEprint_not_readable(env, x);
    else 
      {
	mkcl_object name = x->bytecode.name;
	write_str(env, "#<bytecompiled-function ", stream);
	if (name != mk_cl_Cnil)
	  mk_si_write_ugly_object(env, name, stream);
	else
	  write_addr(env, x, stream);
	mkcl_write_char(env, '>', stream);
      }
    break;
  case mkcl_t_cfun:
    if (mkcl_print_readably(env)) mkcl_FEprint_not_readable(env, x);
    write_str(env, "#<compiled-function ", stream);
    if (x->cfun.name != mk_cl_Cnil)
      mk_si_write_ugly_object(env, x->cfun.name, stream);
    else
      write_addr(env, x, stream);
    mkcl_write_char(env, '>', stream);
    break;
  case mkcl_t_codeblock:
    if (mkcl_print_readably(env)) mkcl_FEprint_not_readable(env, x);
    write_str(env, "#<codeblock ", stream);
    if (x->cblock.source == MK_KEY_foreign)
      write_str(env, "foreign ", stream);
    if (x->cblock.name != mk_cl_Cnil)
      mk_si_write_ugly_object(env, x->cblock.name, stream);
    else
      write_addr(env, x, stream);
    mkcl_write_char(env, '>', stream);
    break;
  case mkcl_t_cclosure:
    if (mkcl_print_readably(env)) mkcl_FEprint_not_readable(env, x);
    write_str(env, "#<compiled-closure ", stream);
    write_addr(env, x, stream);
    mkcl_write_char(env, '>', stream);
    break;
  case mkcl_t_instance:
    mkcl_funcall2(env, MK_CL_print_object->symbol.gfdef, x, stream);
    break;
  case mkcl_t_foreign:
    if (mkcl_print_readably(env)) mkcl_FEprint_not_readable(env, x);
    write_str(env, "#<foreign ", stream);
    mk_si_write_ugly_object(env, x->foreign.tag, stream);
    mkcl_write_char(env, ' ', stream);
    write_addr(env, (mkcl_object)x->foreign.data, stream);
    mkcl_write_char(env, '>', stream);
    break;
  case mkcl_t_temp_stack_frame:
    if (mkcl_print_readably(env)) mkcl_FEprint_not_readable(env, x);
    write_str(env, "#<temp-stack-frame ", stream);
    write_decimal(env, x->frame.size, stream);
    mkcl_write_char(env, ' ', stream);
    write_addr(env, (void*)x->frame.base, stream);
    mkcl_write_char(env, '>', stream);
    break;
  case mkcl_t_thread:
    if (mkcl_print_readably(env)) mkcl_FEprint_not_readable(env, x);
    write_str(env, "#<thread ", stream);
    /* mkcl_write_char(env, '"', stream); */
    mk_si_write_object(env, x->thread.name, stream);
    /* mkcl_write_char(env, '"', stream); */
#if 0
    if (!(x->thread.status))
      {
	mkcl_write_char(env, ' ', stream);
	write_str(env, "inactive", stream);
      }
#endif
    switch (x->thread.status)
      {
      case mkcl_thread_done:
	write_str(env, " done", stream);
	break;
      case mkcl_thread_initialized:
	write_str(env, " initialized", stream);
	break;
      case mkcl_thread_set:
	write_str(env, " set", stream);
	break;
      case mkcl_thread_active:
	write_str(env, " active", stream);
	break;
      default:
	write_str(env, " unknown status", stream);
	break;
      }
    if (x->thread.detached)
      {
	mkcl_write_char(env, ' ', stream);
	write_str(env, "detached", stream);
      }
    else if (x->thread.result_value == MK_KEY_imported)
      {
	mkcl_write_char(env, ' ', stream);
	write_str(env, "imported", stream);
      }
    else if (x->thread.result_value == MK_KEY_imported_and_gc_registered)
      {
	mkcl_write_char(env, ' ', stream);
	write_str(env, "imported-and-gc-registered", stream);
      }
    mkcl_write_char(env, ' ', stream);
    mkcl_write_char(env, '(', stream);
    mk_si_write_object(env, MKCL_MAKE_FIXNUM(x->thread.tid), stream);
    mkcl_write_char(env, ')', stream);
    mkcl_write_char(env, ' ', stream);
    {
      char buf[20] = { 0 };

#if MKCL_WINDOWS || __FreeBSD__
      snprintf(buf, sizeof(buf), "0x%p", x->thread.thread);
#else
      snprintf(buf, sizeof(buf), "0x%lx", x->thread.thread);
#endif
      write_str(env, buf, stream);
    }
    mkcl_write_char(env, ' ', stream);
    write_addr(env, x, stream);
    mkcl_write_char(env, '>', stream);
    break;
  case mkcl_t_lock:
    if (mkcl_print_readably(env)) mkcl_FEprint_not_readable(env, x);
    write_str(env, "#<lock ", stream);
    if (x->lock.recursive)
      write_str(env, "(recursive) ", stream);
    mk_si_write_object(env, x->lock.name, stream);
    mkcl_write_char(env, ' ', stream);
    write_addr(env, x, stream);
    mkcl_write_char(env, '>', stream);
    break;
  case mkcl_t_rwlock:
    if (mkcl_print_readably(env)) mkcl_FEprint_not_readable(env, x);
    write_str(env, "#<rwlock ", stream);
    write_addr(env, x, stream);
    mkcl_write_char(env, '>', stream);
    break;
  case mkcl_t_semaphore:
    if (mkcl_print_readably(env)) mkcl_FEprint_not_readable(env, x);
    write_str(env, "#<semaphore ", stream);
    mk_si_write_object(env, x->semaphore.name, stream);
    mkcl_write_char(env, ' ', stream);
    write_addr(env, x, stream);
    mkcl_write_char(env, '>', stream);
    break;
  case mkcl_t_condition_variable:
    if (mkcl_print_readably(env)) mkcl_FEprint_not_readable(env, x);
    write_str(env, "#<condition-variable ", stream);
    write_addr(env, x, stream);
    mkcl_write_char(env, '>', stream);
    break;
  case mkcl_t_cdisplay:
    if (mkcl_print_readably(env)) mkcl_FEprint_not_readable(env, x);
    write_str(env, "#<compiled-closure-display ", stream);
    write_addr(env, x, stream);
    mkcl_write_char(env, '>', stream);
    break;
  case mkcl_t_clevel_block:
    if (mkcl_print_readably(env)) mkcl_FEprint_not_readable(env, x);
    write_str(env, "#<compiled-closure-level ", stream);
    write_addr(env, x, stream);
    mkcl_write_char(env, '>', stream);
    break;
  case mkcl_t_cmp_dbg_lex_level:
    if (mkcl_print_readably(env)) mkcl_FEprint_not_readable(env, x);
    write_str(env, "#<compiled-debug-lexical-info ", stream);
    write_addr(env, x, stream);
    mkcl_write_char(env, '>', stream);
    break;
  case mkcl_t_process:
    if (mkcl_print_readably(env)) mkcl_FEprint_not_readable(env, x);
    write_str(env, "#<process ", stream);
    mk_si_write_ugly_object(env, x->process.command, stream);
    mkcl_write_char(env, ' ', stream);
    mk_si_write_ugly_object(env, mk_mkcl_process_id(env, x), stream);
    mkcl_write_char(env, ' ', stream);
    write_addr(env, x, stream);
    mkcl_write_char(env, '>', stream);
    break;
  case mkcl_t_UTF_8:
    if (mkcl_print_readably(env)) mkcl_FEprint_not_readable(env, x);
    write_str(env, "#<UTF-8 ", stream);
#if 0
    write_addr(env, x, stream);
#else
    mkcl_write_char(env, '"', stream);
    {
      const mkcl_index max = x->UTF_8.fillp;

      for (ndx = 0;  ndx < max;  ndx++) {
	int c = x->UTF_8.self[ndx];
	if (c == '"' || c == '\\')
	  mkcl_write_char(env, '\\', stream);
	mkcl_write_char(env, c, stream);
      }
    }
    mkcl_write_char(env, '"', stream);
#endif
    mkcl_write_char(env, '>', stream);
    break;
  case mkcl_t_UTF_16:
    if (mkcl_print_readably(env)) mkcl_FEprint_not_readable(env, x);
    write_str(env, "#<UTF-16 ", stream);
#if 0
    write_addr(env, x, stream);
#else
    mkcl_write_char(env, '"', stream);
    {
      const mkcl_index max = x->UTF_16.fillp;

      for (ndx = 0;  ndx < max;  ndx++) {
	int c = x->UTF_16.self[ndx];
	if (c == '"' || c == '\\')
	  mkcl_write_char(env, '\\', stream);
	mkcl_write_char(env, c, stream);
      }
    }
    mkcl_write_char(env, '"', stream);
#endif
    mkcl_write_char(env, '>', stream);
    break;
  default:
    if (mkcl_print_readably(env)) mkcl_FEprint_not_readable(env, x);
    write_str(env, "#<unknown object ", stream);
    write_addr(env, x, stream);
    mkcl_write_char(env, '>', stream);
    break;
  }
  mkcl_return_value(x);
}

mkcl_object
mk_si_write_object(MKCL, mkcl_object x, mkcl_object stream)
{
  bool circle;

  mkcl_call_stack_check(env);
  if (x == MKCL_OBJNULL) {
    if (mkcl_print_readably(env)) mkcl_FEprint_not_readable(env, x);
    write_str(env, "#<OBJNULL>", stream);
    mkcl_return_value(x);
  }

  if (mkcl_symbol_value(env, MK_CL_DYNVAR_print_pretty) != mk_cl_Cnil) {
    mkcl_object f = mkcl_funcall1(env, MK_CL_pprint_dispatch->symbol.gfdef, x);
    if (MKCL_VALUES(1) != mk_cl_Cnil) {
      mkcl_funcall2(env, f, stream, x);
      mkcl_return_value(x);
    }
  }
  circle = mkcl_print_circle(env);
  if (circle && !mkcl_Null(x) && !MKCL_FIXNUMP(x) && !MKCL_CHARACTERP(x) &&
      (MKCL_LISTP(x) || (x->d.t != mkcl_t_symbol) || (mkcl_Null(x->symbol.hpack))))
    {
      mkcl_object circle_counter;
      mkcl_word code;
      circle_counter = mkcl_symbol_value(env, MK_SI_DYNVAR_circle_counter);
      if (circle_counter == mk_cl_Cnil) {
	mkcl_object hash = mk_cl__make_hash_table(env, MK_CL_eq,
						  MKCL_MAKE_FIXNUM(1024),
						  mkcl_make_singlefloat(env, 1.5f),	
						  mkcl_make_singlefloat(env, 0.75f));
	mkcl_bds_bind(env, MK_SI_DYNVAR_circle_counter, mk_cl_Ct);
	mkcl_bds_bind(env, MK_SI_DYNVAR_circle_stack, hash);
	mk_si_write_object(env, x, mkcl_core.null_stream);
	MKCL_SETQ(env, MK_SI_DYNVAR_circle_counter, MKCL_MAKE_FIXNUM(0));
	mk_si_write_object(env, x, stream);
	mk_cl_clrhash(env, hash);
	mkcl_bds_unwind_n(env, 2);
	mkcl_return_value(x);
      }
      code = search_print_circle(env, x);
      if (!MKCL_FIXNUMP(circle_counter)) {
	/* We are only inspecting the object to be printed. */
	/* Only run X if it was not referenced before */
	if (code != 0) mkcl_return_value(x);
      } else if (code == 0) {
	/* Object is not referenced twice */
      } else if (code < 0) {
	/* Object is referenced twice. We print its definition */
	mkcl_write_char(env, '#', stream);
	write_decimal(env, -code, stream);
	mkcl_write_char(env, '=', stream);
      } else {
	/* Second reference to the object */
	mkcl_write_char(env, '#', stream);
	write_decimal(env, code, stream);
	mkcl_write_char(env, '#', stream);
	mkcl_return_value(x);
      }
    }
  mkcl_return_value(mk_si_write_ugly_object(env, x, stream));
}


static bool
object_will_print_as_hash(MKCL, mkcl_object x)
{
  mkcl_object circle_counter = mkcl_symbol_value(env, MK_SI_DYNVAR_circle_counter);
  mkcl_object circle_stack = mkcl_symbol_value(env, MK_SI_DYNVAR_circle_stack);
  mkcl_object code = mkcl_gethash_safe(env, x, circle_stack, MKCL_OBJNULL);
  if (MKCL_FIXNUMP(circle_counter)) {
    return !(code == MKCL_OBJNULL || code == mk_cl_Cnil);
  } else if (code == MKCL_OBJNULL) {
    /* Was not found before */
    mkcl_sethash(env, x, circle_stack, mk_cl_Cnil);
    return 0;
  } else {
    return 1;
  }
}

/* To print circular structures, we traverse the structure by adding
   a pair <element, flag> to the interpreter stack for each element visited.
   flag is initially NIL and becomes T if the element is visited again.
   After the visit we squeeze out all the non circular elements.
   The flags is used during printing to distinguish between the first visit
   to the element.
 */

static mkcl_word
search_print_circle(MKCL, mkcl_object x)
{
  mkcl_object circle_counter = mkcl_symbol_value(env, MK_SI_DYNVAR_circle_counter);
  mkcl_object circle_stack = mkcl_symbol_value(env, MK_SI_DYNVAR_circle_stack);
  mkcl_object code;

  if (!MKCL_FIXNUMP(circle_counter)) {
    code = mkcl_gethash_safe(env, x, circle_stack, MKCL_OBJNULL);
    if (code == MKCL_OBJNULL) {
      /* Was not found before */
      mkcl_sethash(env, x, circle_stack, mk_cl_Cnil);
      return 0;
    } else if (code == mk_cl_Cnil) {
      /* This object is referenced twice */
      mkcl_sethash(env, x, circle_stack, mk_cl_Ct);
      return 1;
    } else {
      return 2;
    }
  } else {
    code = mkcl_gethash_safe(env, x, circle_stack, MKCL_OBJNULL);
    if (code == MKCL_OBJNULL || code == mk_cl_Cnil) {
      /* Is not referenced or was not found before */
      /* mkcl_sethash(x, circle_stack, mk_cl_Cnil); */
      return 0;
    } else if (code == mk_cl_Ct) {
      /* This object is referenced twice, but has no code yet */
      mkcl_word new_code = mkcl_fixnum_to_word(circle_counter) + 1;
      circle_counter = MKCL_MAKE_FIXNUM(new_code);
      mkcl_sethash(env, x, circle_stack, circle_counter);
      MKCL_SETQ(env, MK_SI_DYNVAR_circle_counter,
	       circle_counter);
      return -new_code;
    } else {
      return mkcl_fixnum_to_word(code);
    }
  }
}

#define	mkcl_exponent_marker_p(i)				 \
  ((i) == 'e' || (i) == 'E' ||					 \
   (i) == 's' || (i) == 'S' || (i) == 'f' || (i) == 'F' ||	 \
   (i) == 'd' || (i) == 'D' || (i) == 'l' || (i) == 'L' ||	 \
   (i) == 'b' || (i) == 'B')

static bool
potential_number_p(MKCL, mkcl_object strng, mkcl_word base)
{
  /* See ANSI 2.3.1.1 */
  bool saw_a_digit = FALSE;
  mkcl_index l = mkcl_string_length(env, strng);
  mkcl_character c;

  if (l == 0)
    return FALSE;
  c = mkcl_char(env, strng, 0);

  /* A potential number must begin with a digit, sign or extension character (^ _) */
  if (mkcl_digitp(c, base) >= 0)
    saw_a_digit = TRUE;
  else if (c != '+' && c != '-' && c != '^' && c != '_' && c != '.')
    return FALSE;

  /* A potential number cannot end with a sign */
  c = mkcl_char(env, strng, l-1);
  if (c == '+' || c == '-')
    return FALSE;

  {
    mkcl_index i;

    for (i = 1;  i < l;  i++)
      {
        c = mkcl_char(env, strng, i);
        /* It can only contain digits, signs, ratio markers, extension characters and
         * number markers. Number markers are letters, but two adjacent letters fail
         * to be a number marker. */
        if (mkcl_digitp(c, base) >= 0)
          { saw_a_digit = TRUE; continue; }
        else if (c == '+' || c == '-' || c == '/' || c == '.' || c == '^' || c == '_')
          { continue; }
        if (mkcl_alpha_char_p(c) && (((i+1) >= l) || !mkcl_alpha_char_p(mkcl_char(env, strng, i+1))))
          { continue; }
        else
          return FALSE;
      }
  }
  return saw_a_digit;
}



mkcl_object mk_cl_write(MKCL, mkcl_narg narg, mkcl_object x, ...)
{
  mkcl_call_stack_check(env);
  {
    mkcl_object strm = mk_cl_Cnil;
    mkcl_object array = mkcl_symbol_value(env, MK_CL_DYNVAR_print_array);
    mkcl_object base = mkcl_symbol_value(env, MK_CL_DYNVAR_print_base);
    mkcl_object cas = mkcl_symbol_value(env, MK_CL_DYNVAR_print_case);
    mkcl_object circle = mkcl_symbol_value(env, MK_CL_DYNVAR_print_circle);
    mkcl_object escape = mkcl_symbol_value(env, MK_CL_DYNVAR_print_escape);
    mkcl_object gensym = mkcl_symbol_value(env, MK_CL_DYNVAR_print_gensym);
    mkcl_object length = mkcl_symbol_value(env, MK_CL_DYNVAR_print_length);
    mkcl_object level = mkcl_symbol_value(env, MK_CL_DYNVAR_print_level);
    mkcl_object lines = mkcl_symbol_value(env, MK_CL_DYNVAR_print_lines);
    mkcl_object miser_width = mkcl_symbol_value(env, MK_CL_DYNVAR_print_miser_width);
    mkcl_object pprint_dispatch = mkcl_symbol_value(env, MK_CL_DYNVAR_print_pprint_dispatch);
    mkcl_object pretty = mkcl_symbol_value(env, MK_CL_DYNVAR_print_pretty);
    mkcl_object radix = mkcl_symbol_value(env, MK_CL_DYNVAR_print_radix);
    mkcl_object readably = mkcl_symbol_value(env, MK_CL_DYNVAR_print_readably);
    mkcl_object right_margin = mkcl_symbol_value(env, MK_CL_DYNVAR_print_right_margin);
    struct mkcl_key_param_spec key_params[] =
      {
       { MK_KEY_stream, &strm, false },
       { MK_KEY_array, &array, false },
       { MK_KEY_base, &base, false },
       { MK_KEY_case, &cas, false },
       { MK_KEY_circle, &circle, false },
       { MK_KEY_escape, &escape, false },
       { MK_KEY_gensym, &gensym, false },
       { MK_KEY_length, &length, false },
       { MK_KEY_level, &level, false },
       { MK_KEY_lines, &lines, false },
       { MK_KEY_miser_width, &miser_width, false },
       { MK_KEY_pprint_dispatch, &pprint_dispatch, false },
       { MK_KEY_pretty, &pretty, false },
       { MK_KEY_radix, &radix, false },
       { MK_KEY_readably, &readably, false },
       { MK_KEY_right_margin, &right_margin, false }
      };
    MKCL_RECEIVE_N_KEYWORD_ARGUMENTS(env, MK_CL_write, narg, 1, x, key_params);

    mkcl_bds_bind(env, MK_CL_DYNVAR_print_array, array);
    mkcl_bds_bind(env, MK_CL_DYNVAR_print_base, base);
    mkcl_bds_bind(env, MK_CL_DYNVAR_print_case, cas);
    mkcl_bds_bind(env, MK_CL_DYNVAR_print_circle, circle);
    mkcl_bds_bind(env, MK_CL_DYNVAR_print_escape, escape);
    mkcl_bds_bind(env, MK_CL_DYNVAR_print_gensym, gensym);
    mkcl_bds_bind(env, MK_CL_DYNVAR_print_level, level);
    mkcl_bds_bind(env, MK_CL_DYNVAR_print_length, length);
    mkcl_bds_bind(env, MK_CL_DYNVAR_print_lines, lines);
    mkcl_bds_bind(env, MK_CL_DYNVAR_print_miser_width, miser_width);
    mkcl_bds_bind(env, MK_CL_DYNVAR_print_pprint_dispatch, pprint_dispatch);
    mkcl_bds_bind(env, MK_CL_DYNVAR_print_pretty, pretty);
    mkcl_bds_bind(env, MK_CL_DYNVAR_print_radix, radix);
    mkcl_bds_bind(env, MK_CL_DYNVAR_print_readably, readably);
    mkcl_bds_bind(env, MK_CL_DYNVAR_print_right_margin, right_margin);
  
    strm = stream_or_default_output(env, strm);
    mk_si_write_object(env, x, strm);
    
    /* The count here must match the number of mkcl_bds_bind just above. */
    mkcl_bds_unwind_n(env, 15);
    mkcl_return_value(x);
  }
}

mkcl_object mk_cl_prin1(MKCL, mkcl_narg narg, mkcl_object obj, ...)
{
  mkcl_call_stack_check(env);
  {
    mkcl_object strm = mk_cl_Cnil;
    MKCL_RECEIVE_1_OPTIONAL_ARGUMENT(env, MK_CL_prin1, narg, 1, obj, &strm);

    mkcl_prin1(env, obj, strm);
    mkcl_return_value(obj);
  }
}

mkcl_object mk_cl_print(MKCL, mkcl_narg narg, mkcl_object obj, ...)
{
  mkcl_call_stack_check(env);
  {
    mkcl_object strm = mk_cl_Cnil;
    MKCL_RECEIVE_1_OPTIONAL_ARGUMENT(env, MK_CL_print, narg, 1, obj, &strm);

    mkcl_print(env, obj, strm);
    mkcl_return_value(obj);
  }
}

mkcl_object mk_cl_pprint(MKCL, mkcl_narg narg, mkcl_object obj, ...)
{
  mkcl_call_stack_check(env);
  {
    mkcl_object strm = mk_cl_Cnil;
    MKCL_RECEIVE_1_OPTIONAL_ARGUMENT(env, MK_CL_pprint, narg, 1, obj, &strm);

    strm = stream_or_default_output(env, strm);
    mkcl_bds_bind(env, MK_CL_DYNVAR_print_escape, mk_cl_Ct);
    mkcl_bds_bind(env, MK_CL_DYNVAR_print_pretty, mk_cl_Ct);
    mkcl_write_char(env, '\n', strm);
    mk_si_write_object(env, obj, strm);
  
    mkcl_bds_unwind_n(env, 2);
    mkcl_return_no_value;
  }
}

mkcl_object mk_cl_princ(MKCL, mkcl_narg narg, mkcl_object obj, ...)
{
  mkcl_call_stack_check(env);
  {
    mkcl_object strm = mk_cl_Cnil;
    MKCL_RECEIVE_1_OPTIONAL_ARGUMENT(env, MK_CL_princ, narg, 1, obj, &strm);

    mkcl_princ(env, obj, strm);
    mkcl_return_value(obj);
  }
}

mkcl_object mk_cl_write_char(MKCL, mkcl_narg narg, mkcl_object c, ...)
{
  mkcl_call_stack_check(env);
  {
    mkcl_object strm = mk_cl_Cnil;
    MKCL_RECEIVE_1_OPTIONAL_ARGUMENT(env, MK_CL_write_char, narg, 1, c, &strm);

    /* INV: mkcl_char_code() checks the type of `c' */
    strm = stream_or_default_output(env, strm);
    mkcl_write_char(env, mkcl_char_code(env, c), strm);
    mkcl_return_value(c);
  }
}

mkcl_object mk_cl_write_string(MKCL, const mkcl_narg narg, mkcl_object strng, ...)
{
  mkcl_call_stack_check(env);
  {
    mkcl_object strm = mk_cl_Cnil;
    mkcl_object start = MKCL_MAKE_FIXNUM(0);
    mkcl_object end = mk_cl_Cnil;
    mkcl_check_minimal_arg_count(env, MK_CL_write_string, narg, 1);
    if (narg > 1) {
      mkcl_va_list ARGS;
      mkcl_va_start(env, ARGS, strng, narg, 1);
      strm = mkcl_va_arg(ARGS);
      if (narg > 2)
        mkcl_receive_2_keyword_arguments(env, MK_CL_write_string, ARGS, MK_KEY_start, &start, MK_KEY_end, &end);
      mkcl_va_end(ARGS);
    }
    strng = mkcl_check_type_string(env, MK_CL_write_string, strng);
    strm = stream_or_default_output(env, strm);
    if (mkcl_type_of(strm) != mkcl_t_stream)
      mkcl_funcall4(env, MK_GRAY_stream_write_string->symbol.gfdef, strm, strng, start, end);
    else
      mk_si_do_write_sequence(env, strng, strm, start, end);
    mkcl_return_value(strng);
  }
}

mkcl_object mk_cl_write_line(MKCL, const mkcl_narg narg, mkcl_object strng, ...)
{
  mkcl_call_stack_check(env);
  {
    mkcl_object strm = mk_cl_Cnil;
    mkcl_object start = MKCL_MAKE_FIXNUM(0);
    mkcl_object end = mk_cl_Cnil;
    mkcl_check_minimal_arg_count(env, MK_CL_write_line, narg, 1);
    if (narg > 1) {
      mkcl_va_list ARGS;
      mkcl_va_start(env, ARGS, strng, narg, 1);
      strm = mkcl_va_arg(ARGS);
      if (narg > 2)
        mkcl_receive_2_keyword_arguments(env, MK_CL_write_line, ARGS, MK_KEY_start, &start, MK_KEY_end, &end);
      mkcl_va_end(ARGS);
    }
  strng = mkcl_check_type_string(env, MK_CL_write_line, strng);
  strm = stream_or_default_output(env, strm);
  if (mkcl_type_of(strm) != mkcl_t_stream)
    mkcl_funcall4(env, MK_GRAY_stream_write_string->symbol.gfdef, strm, strng, start, end);
  else
    mk_si_do_write_sequence(env, strng, strm, start, end);
  mkcl_terpri(env, strm);
  mkcl_return_value(strng);
  }
}

mkcl_object mk_cl_terpri(MKCL, mkcl_narg narg, ...)
{
  mkcl_call_stack_check(env);
  {
    mkcl_object strm = mk_cl_Cnil;
    MKCL_RECEIVE_1_OPTIONAL_ARGUMENT(env, MK_CL_terpri, narg, 0, narg, &strm);

    mkcl_terpri(env, strm);
    mkcl_return_value(mk_cl_Cnil);
  }
}

mkcl_object mk_cl_fresh_line(MKCL, mkcl_narg narg, ...)
{
  mkcl_call_stack_check(env);
  {
    mkcl_object strm = mk_cl_Cnil;
    MKCL_RECEIVE_1_OPTIONAL_ARGUMENT(env, MK_CL_fresh_line, narg, 0, narg, &strm);

    strm = stream_or_default_output(env, strm);
    if (mkcl_type_of(strm) != mkcl_t_stream) {
      return mkcl_funcall1(env, MK_GRAY_stream_fresh_line->symbol.gfdef, strm);
    }
    if (mkcl_file_column(env, strm) == 0)
      { mkcl_return_value(mk_cl_Cnil); }
    mkcl_write_char(env, '\n', strm);
    mkcl_return_value(mk_cl_Ct);
  }
}

mkcl_object mk_cl_finish_output(MKCL, mkcl_narg narg, ...)
{
  mkcl_call_stack_check(env);
  {
    mkcl_object strm = mk_cl_Cnil;
    MKCL_RECEIVE_1_OPTIONAL_ARGUMENT(env, MK_CL_finish_output, narg, 0, narg, &strm);

    strm = stream_or_default_output(env, strm);
    if (mkcl_type_of(strm) != mkcl_t_stream) {
      return mkcl_funcall1(env, MK_GRAY_stream_finish_output->symbol.gfdef, strm);
    }
    mkcl_force_output(env, strm);
    mkcl_return_value(mk_cl_Cnil);
  }
}

mkcl_object mk_cl_force_output(MKCL, mkcl_narg narg, ...)
{
  mkcl_call_stack_check(env);
  {
    mkcl_object strm = mk_cl_Cnil;
    MKCL_RECEIVE_1_OPTIONAL_ARGUMENT(env, MK_CL_force_output, narg, 0, narg, &strm);

    strm = stream_or_default_output(env, strm);
    mkcl_force_output(env, strm);
    mkcl_return_value(mk_cl_Cnil);
  }
}

mkcl_object mk_cl_clear_output(MKCL, mkcl_narg narg, ...)
{
  mkcl_call_stack_check(env);
  {
    mkcl_object strm = mk_cl_Cnil;
    MKCL_RECEIVE_1_OPTIONAL_ARGUMENT(env, MK_CL_clear_output, narg, 0, narg, &strm);

    strm = stream_or_default_output(env, strm);
    mkcl_clear_output(env, strm);
    mkcl_return_value(mk_cl_Cnil);
  }
}

mkcl_object
mk_cl_write_byte(MKCL, mkcl_object integer, mkcl_object binary_output_stream)
{
  mkcl_call_stack_check(env);
  mkcl_write_byte(env, integer, binary_output_stream);
  mkcl_return_value(integer);
}

mkcl_object mk_cl_write_sequence(MKCL, mkcl_narg narg, mkcl_object sequence, mkcl_object stream, ...)
{
  mkcl_call_stack_check(env);
  {
    mkcl_object start = MKCL_MAKE_FIXNUM(0);
    mkcl_object end = mk_cl_Cnil;

    MKCL_RECEIVE_2_KEYWORD_ARGUMENTS(env, MK_CL_write_sequence, narg, 2, stream, MK_KEY_start, &start, MK_KEY_end, &end);

    if (mkcl_type_of(stream) != mkcl_t_stream)
      return mkcl_funcall4(env, MK_GRAY_stream_write_sequence->symbol.gfdef, stream, sequence, start, end);
    else
      return mk_si_do_write_sequence(env, sequence, stream, start, end);
  }
}


mkcl_object mkcl_princ(MKCL, mkcl_object obj, mkcl_object strm)
{
  const mkcl_env the_env = env;
  strm = stream_or_default_output(env, strm);
  mkcl_bds_bind(the_env, MK_CL_DYNVAR_print_escape, mk_cl_Cnil);
  mkcl_bds_bind(the_env, MK_CL_DYNVAR_print_readably, mk_cl_Cnil);
  mk_si_write_object(env, obj, strm);
  mkcl_bds_unwind_n(the_env, 2);
  return obj;
}

mkcl_object
mkcl_prin1(MKCL, mkcl_object obj, mkcl_object strm)
{
  const mkcl_env the_env = env;
  strm = stream_or_default_output(env, strm);
  mkcl_bds_bind(the_env, MK_CL_DYNVAR_print_escape, mk_cl_Ct);
  mk_si_write_object(env, obj, strm);

  mkcl_bds_unwind1(the_env);
  return obj;
}

mkcl_object
mkcl_print(MKCL, mkcl_object obj, mkcl_object strm)
{
  strm = stream_or_default_output(env, strm);
  mkcl_terpri(env, strm);
  mkcl_prin1(env, obj, strm);
  mkcl_princ_char(env, ' ', strm);
  return obj;
}

mkcl_object
mkcl_println(MKCL, mkcl_object obj, mkcl_object strm) /* Mostly a debugging tool to be called from the prompt of GDB. JCB */
{
  strm = stream_or_default_output(env, strm);
  mkcl_terpri(env, strm);
  mkcl_prin1(env, obj, strm);
  mkcl_terpri(env, strm);
  return obj;
}

mkcl_object
mkcl_println_T(MKCL, mkcl_object obj) /* Mostly a debugging tool to be called from the prompt of GDB. JCB */
{
  mkcl_object strm = stream_or_default_output(env, mk_cl_Ct);
  mkcl_terpri(env, strm);
  mkcl_prin1(env, obj, strm);
  mkcl_terpri(env, strm);
  return obj;
}


mkcl_object
mkcl_terpri(MKCL, mkcl_object strm)
{
  strm = stream_or_default_output(env, strm);
  if (mkcl_type_of(strm) != mkcl_t_stream) {
    return mkcl_funcall1(env, MK_GRAY_stream_terpri->symbol.gfdef, strm);
  }
  mkcl_write_char(env, '\n', strm);
  return(mk_cl_Cnil);
}

void
mkcl_write_string(MKCL, mkcl_object strng, mkcl_object strm)
{
  mkcl_index i;

  strm = stream_or_default_output(env, strm);
  switch(mkcl_type_of(strng)) {
  case mkcl_t_string:
    for (i = 0;  i < strng->string.fillp;  i++)
      mkcl_write_char(env, strng->string.self[i], strm);
    break;
  case mkcl_t_base_string:
    for (i = 0;  i < strng->base_string.fillp;  i++)
      mkcl_write_char(env, strng->base_string.self[i], strm);
    break;
  default:
    mkcl_FEtype_error_string(env, strng);
  }
}

void
mkcl_princ_str(MKCL, const char *s, mkcl_object strm)
{
  strm = stream_or_default_output(env, strm);
  mkcl_write_cstr(env, s, strm);
}

void
mkcl_princ_char(MKCL, int c, mkcl_object strm)
{
  strm = stream_or_default_output(env, strm);
  mkcl_write_char(env, c, strm);
#if 0 /* Turn this on if you do not believe in buffering. JCB */
  if (c == '\n') {
    mkcl_force_output(env, strm);
  }
#endif
}
