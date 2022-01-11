/* -*- mode: c -*- */
/*
    character.d -- Character routines.
*/
/*
    Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
    Copyright (c) 1990, Giuseppe Attardi.
    Copyright (c) 2012-2019,2021 Jean-Claude Beaudoin.

    MKCL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 3 of the License, or (at your option) any later version.

    See file '../../Copyright' for full details.
*/

#include <mkcl/mkcl.h>
#include <mkcl/internal.h>
#include <stdio.h>

mkcl_object
mk_cl_standard_char_p(MKCL, mkcl_object c)
{
  mkcl_call_stack_check(env);
  /* INV: mkcl_char_code() checks the type */
  mkcl_word i = mkcl_char_code(env, c);
  mkcl_return_value((mkcl_standard_char_p(i)? mk_cl_Ct : mk_cl_Cnil));
}

struct mkcl_cfun mk_cl_graphic_char_p_cfunobj = MKCL_CFUN1(mk_cl_graphic_char_p, MK_CL_graphic_char_p);

mkcl_object
mk_cl_graphic_char_p(MKCL, mkcl_object c)
{
  mkcl_call_stack_check(env);
  /* INV: mkcl_char_code() checks the type */
  mkcl_return_value((mkcl_graphic_char_p(mkcl_char_code(env, c))? mk_cl_Ct : mk_cl_Cnil));
}

struct mkcl_cfun mk_cl_alpha_char_p_cfunobj = MKCL_CFUN1(mk_cl_alpha_char_p, MK_CL_alpha_char_p);

mkcl_object
mk_cl_alpha_char_p(MKCL, mkcl_object c)
{
  mkcl_call_stack_check(env);
  /* INV: mkcl_char_code() checks the type */
  mkcl_return_value((mkcl_alpha_char_p(mkcl_char_code(env, c))? mk_cl_Ct : mk_cl_Cnil));
}

mkcl_object
mk_cl_upper_case_p(MKCL, mkcl_object c)
{
  mkcl_call_stack_check(env);
  /* INV: mkcl_char_code() checks the type */
  mkcl_return_value((mkcl_upper_case_p(mkcl_char_code(env, c))? mk_cl_Ct : mk_cl_Cnil));
}

mkcl_object
mk_cl_lower_case_p(MKCL, mkcl_object c)
{
  mkcl_call_stack_check(env);
  /* INV: mkcl_char_code() checks the type */
  mkcl_return_value((mkcl_lower_case_p(mkcl_char_code(env, c))? mk_cl_Ct : mk_cl_Cnil));
}

struct mkcl_cfun mk_cl_both_case_p_cfunobj = MKCL_CFUN1(mk_cl_both_case_p, MK_CL_both_case_p);

mkcl_object
mk_cl_both_case_p(MKCL, mkcl_object c)
{
  mkcl_call_stack_check(env);
  /* INV: mkcl_char_code() checks the type */
  mkcl_return_value((mkcl_both_case_p(mkcl_char_code(env, c))? mk_cl_Ct : mk_cl_Cnil));
}

enum mkcl_string_case
mkcl_string_case(const mkcl_object str)
{
  enum mkcl_string_case str_case = mkcl_mixedcase_string;
  mkcl_index i;

  if (MKCL_BASE_STRING_P(str))
    {
      const mkcl_base_char *text = str->base_string.self;
      const mkcl_index dim = str->base_string.dim;
      for (i = 0; i < dim; i++) {
        const mkcl_base_char ch = text[i];
        if (mkcl_upper_case_p(ch)) {
          if (str_case == mkcl_lowercase_string) 
            return mkcl_mixedcase_string;
          else
            str_case = mkcl_uppercase_string;
        } else if (mkcl_lower_case_p(ch)) {
          if (str_case == mkcl_uppercase_string)
            return mkcl_mixedcase_string;
          else
            str_case = mkcl_lowercase_string;
        }
      }
      return str_case;
    }
  else if (MKCL_CHARACTER_STRING_P(str))
    {
      const mkcl_character *text = str->string.self;
      const mkcl_index dim = str->string.dim;
      for (i = 0; i < dim; i++) {
        const mkcl_character ch = text[i];
        if (mkcl_upper_case_p(ch)) {
          if (str_case == mkcl_lowercase_string) 
            return mkcl_mixedcase_string;
          else
            str_case = mkcl_uppercase_string;
        } else if (mkcl_lower_case_p(ch)) {
          if (str_case == mkcl_uppercase_string)
            return mkcl_mixedcase_string;
          else
            str_case = mkcl_lowercase_string;
        }
      }
      return str_case;
    }
  else return str_case;
}

mkcl_object mk_cl_digit_char_p(MKCL, mkcl_narg narg, mkcl_object c, ...)
{
  mkcl_call_stack_check(env);
  {
    mkcl_object radix = MKCL_MAKE_FIXNUM(10);
    MKCL_RECEIVE_1_OPTIONAL_ARGUMENT(env, MK_CL_digit_char_p, narg, 1, c, &radix);
    {
      mkcl_word basis = mkcl_fixnum_in_range(env, MK_CL_digit_char_p,"radix", radix, 2, 36);
      mkcl_word value = mkcl_digitp(mkcl_char_code(env, c), basis);
      mkcl_return_value(((value < 0)? mk_cl_Cnil: MKCL_MAKE_FIXNUM(value)));
    }
  }
}

/*
	Mkcl_Digitp(i, r) returns the weight of code i
	as a digit of radix r, which must be 1 < r <= 36.
	If i is not a digit, -1 is returned.
*/
int
mkcl_digitp(mkcl_character i, int r)
{
  if (('0' <= i) && (i <= '9') && (i < '0' + r))
    return i - '0';
  if (('A' <= i) && (10 < r) && (i < 'A' + (r - 10)))
    return i - 'A' + 10;
  if (('a' <= i) && (10 < r) && (i < 'a' + (r - 10)))
    return i - 'a' + 10;
  if (i > 255) {
    int number = mkcl_ucd_decimal_digit(i);
    if (number < r)
      return number;
  }
  return -1;
}

struct mkcl_cfun mk_cl_alphanumericp_cfunobj = MKCL_CFUN1(mk_cl_alphanumericp, MK_CL_alphanumericp);

mkcl_object
mk_cl_alphanumericp(MKCL, mkcl_object c)
{
  mkcl_call_stack_check(env);
  /* INV: mkcl_char_code() checks type of `c' */
  mkcl_character i = mkcl_char_code(env, c);
  mkcl_return_value((mkcl_alphanumericp(i) ? mk_cl_Ct : mk_cl_Cnil));
}

mkcl_object mk_cl_charE(MKCL, mkcl_narg narg, mkcl_object c, ...)
{
  mkcl_call_stack_check(env);
  {
    mkcl_setup_for_rest(env, MK_CL_charE, 1, narg, c, cs);

    /* INV: mkcl_char_eq() checks types of `c' and `cs' */
    while (--narg)
      if (!mkcl_char_eq(env, c, mkcl_va_arg(cs)))
        { mkcl_va_end(cs); mkcl_return_value(mk_cl_Cnil); }
    mkcl_va_end(cs);
    mkcl_return_value(mk_cl_Ct);
  }
}

bool
mkcl_char_eq(MKCL, mkcl_object x, mkcl_object y)
{
  return mkcl_char_code(env, x) == mkcl_char_code(env, y);
}

mkcl_object mk_cl_charNE(MKCL, const mkcl_narg narg, ...)
{
  int i, j;
  mkcl_object c;

  mkcl_call_stack_check(env);
  {
    mkcl_setup_for_rest(env, MK_CL_charE, 0, narg, narg, cs);

    /* INV: mkcl_char_eq() checks types of its arguments */
    if (narg == 0)
      mkcl_FEwrong_num_arguments(env, MK_CL_charNE, 1, -1, 0);
    c = mkcl_va_arg(cs);
    for (i = 2; i<=narg; i++) {
      mkcl_va_list ds;
      mkcl_va_start(env, ds, narg, narg, 0);
      c = mkcl_va_arg(cs);
      for (j = 1; j<i; j++)
        if (mkcl_char_eq(env, mkcl_va_arg(ds), c))
          { mkcl_va_end(cs); mkcl_va_end(ds); mkcl_return_value(mk_cl_Cnil); }
      mkcl_va_end(ds);
    }
    mkcl_va_end(cs);
    mkcl_return_value(mk_cl_Ct);
  }
}

static mkcl_object
Lchar_cmp(MKCL, mkcl_narg narg, int s, int t, mkcl_va_list args)
{
  mkcl_object c, d;

  if (narg == 0)
    mkcl_FEwrong_num_arguments_anonym(env, 1, -1, narg);
  c = mkcl_va_arg(args);
  for (; --narg; c = d) {
    d = mkcl_va_arg(args);
    if (s*mkcl_char_cmp(env, d, c) < t)
      { mkcl_return_value(mk_cl_Cnil); }
  }
  mkcl_return_value(mk_cl_Ct);
}

int
mkcl_char_cmp(MKCL, mkcl_object x, mkcl_object y)
{
	/* mkcl_char_code(x) returns an integer which is well in the range
	 * of positive fixnums. Therefore, this subtraction never
	 * oveflows. */
  return mkcl_char_code(env, x) - mkcl_char_code(env, y);
}

mkcl_object mk_cl_charL(MKCL, const mkcl_narg narg, ...)
{
  mkcl_call_stack_check(env);
  {
    mkcl_setup_for_rest(env, MK_CL_charL, 0, narg, narg, args);

    mkcl_object val = Lchar_cmp(env, narg, 1, 1, args);
    mkcl_va_end(args);
    return val;
  }
}

mkcl_object mk_cl_charG(MKCL, const mkcl_narg narg, ...)
{
  mkcl_call_stack_check(env);
  {
    mkcl_setup_for_rest(env, MK_CL_charG, 0, narg, narg, args);

    mkcl_object val = Lchar_cmp(env, narg, -1, 1, args);
    mkcl_va_end(args);
    return val;
  }
}

mkcl_object mk_cl_charLE(MKCL, const mkcl_narg narg, ...)
{
  mkcl_call_stack_check(env);
  {
    mkcl_setup_for_rest(env, MK_CL_charLE, 0, narg, narg, args);

    mkcl_object val = Lchar_cmp(env, narg, 1, 0, args);
    mkcl_va_end(args);
    return val;
  }
}

mkcl_object mk_cl_charGE(MKCL, const mkcl_narg narg, ...)
{
  mkcl_call_stack_check(env);
  {
    mkcl_setup_for_rest(env, MK_CL_charGE, 0, narg, narg, args);

    mkcl_object val = Lchar_cmp(env, narg, -1, 0, args);
    mkcl_va_end(args);
    return val;
  }
}

mkcl_object mk_cl_char_equal(MKCL, mkcl_narg narg, mkcl_object c, ...)
{
  mkcl_narg i;
  mkcl_call_stack_check(env);
  {
    mkcl_setup_for_rest(env, MK_CL_char_equal, 1, narg, c, cs);

    /* INV: mkcl_char_equal() checks the type of its arguments */
    for (narg--, i = 0;  i < narg;  i++) {
      if (!mkcl_char_equal(env, c, mkcl_va_arg(cs)))
        { mkcl_va_end(cs); mkcl_return_value(mk_cl_Cnil); }
    }
    mkcl_va_end(cs);
    mkcl_return_value(mk_cl_Ct);
  }
}

#define char_equal_code(e, x) mkcl_char_upcase(mkcl_char_code(e, x))

bool
mkcl_char_equal(MKCL, mkcl_object x, mkcl_object y)
{
  return char_equal_code(env, x) == char_equal_code(env, y);
}

mkcl_object mk_cl_char_not_equal(MKCL, mkcl_narg narg, ...)
{
  int i, j;
  mkcl_object c;
  mkcl_call_stack_check(env);
  {
    mkcl_setup_for_rest(env, MK_CL_char_not_equal, 0, narg, narg, cs);

    /* INV: mkcl_char_equal() checks the type of its arguments */
    if (narg == 0)
      mkcl_FEwrong_num_arguments(env, MK_CL_char_not_equal, 1, -1, narg);
    c = mkcl_va_arg(cs);
    for (i = 2;  i<=narg;  i++) {
      mkcl_va_list ds;
      mkcl_va_start(env, ds, narg, narg, 0);
      c = mkcl_va_arg(cs);
      for (j=1;  j<i;  j++)
        if (mkcl_char_equal(env, c, mkcl_va_arg(ds)))
          { mkcl_va_end(cs); mkcl_va_end(ds); mkcl_return_value(mk_cl_Cnil); }
      mkcl_va_end(ds);
    }
    mkcl_va_end(cs);
    mkcl_return_value(mk_cl_Ct);
  }
}

static mkcl_object
Lchar_compare(MKCL, mkcl_narg narg, int s, int t, mkcl_va_list args)
{
  mkcl_object c, d;
  
  /* INV: mkcl_char_compare() checks the types of its arguments */
  if (narg == 0)
    mkcl_FEwrong_num_arguments_anonym(env, 1, -1, narg);
  c = mkcl_va_arg(args);
  for (; --narg; c = d) {
    d = mkcl_va_arg(args);
    if (s*mkcl_char_compare(env, d, c) < t)
      { mkcl_return_value(mk_cl_Cnil); }
  }
  mkcl_return_value(mk_cl_Ct);
}

int
mkcl_char_compare(MKCL, mkcl_object x, mkcl_object y)
{
  mkcl_word i = char_equal_code(env, x);
  mkcl_word j = char_equal_code(env, y);

  if (i < j)
    return(-1);
  else if (i == j)
    return(0);
  else
    return(1);
}

mkcl_object mk_cl_char_lessp(MKCL, mkcl_narg narg, ...)
{
  mkcl_call_stack_check(env);
  {
    mkcl_setup_for_rest(env, MK_CL_char_lessp, 0, narg, narg, args);

    mkcl_object val = Lchar_compare(env, narg, 1, 1, args);
    mkcl_va_end(args);
    return val;
  }
}

mkcl_object mk_cl_char_greaterp(MKCL, mkcl_narg narg, ...)
{
  mkcl_call_stack_check(env);
  {
    mkcl_setup_for_rest(env, MK_CL_char_greaterp, 0, narg, narg, args);

    mkcl_object val = Lchar_compare(env, narg,-1, 1, args);
    mkcl_va_end(args);
    return val;
  }
}

mkcl_object mk_cl_char_not_greaterp(MKCL, mkcl_narg narg, ...)
{
  mkcl_call_stack_check(env);
  {
    mkcl_setup_for_rest(env, MK_CL_char_not_greaterp, 0, narg, narg, args);

    mkcl_object val = Lchar_compare(env, narg, 1, 0, args);
    mkcl_va_end(args);
    return val;
  }
}

mkcl_object mk_cl_char_not_lessp(MKCL, mkcl_narg narg, ...)
{
  mkcl_call_stack_check(env);
  {
    mkcl_setup_for_rest(env, MK_CL_char_not_lessp, 0, narg, narg, args);

    mkcl_object val = Lchar_compare(env, narg,-1, 0, args);
    mkcl_va_end(args);
    return val;
  }
}


struct mkcl_cfun mk_cl_character_cfunobj = MKCL_CFUN1(mk_cl_character, MK_CL_character);

mkcl_object
mk_cl_character(MKCL, mkcl_object x)
{
  mkcl_call_stack_check(env);
 AGAIN:
  switch (mkcl_type_of(x)) {
  case mkcl_t_character:
    break;
  case mkcl_t_symbol:
    x = x->symbol.name;
    goto AGAIN;
  case mkcl_t_string:
    if (x->string.fillp == 1) {
      x = MKCL_CODE_CHAR(x->string.self[0]);
      break;
    }
    goto _MKCL_ERROR;
  case mkcl_t_base_string:
    if (x->base_string.fillp == 1) {
      x = MKCL_CODE_CHAR(x->base_string.self[0]);
      break;
    }
  _MKCL_ERROR:
  default:
    x = mkcl_type_error(env, 
			MK_CL_character,
			"character designator",
			x,
			mkcl_fast_read_from_cstring(env,("(OR CHARACTER SYMBOL"
							 " (ARRAY CHARACTER (1))"
							 " (ARRAY BASE-CHAR (1)))")));
    goto AGAIN;
  }
  mkcl_return_value(x);
}

struct mkcl_cfun mk_cl_char_code_cfunobj = MKCL_CFUN1(mk_cl_char_code, MK_CL_char_code);

mkcl_object
mk_cl_char_code(MKCL, mkcl_object c)
{
  mkcl_call_stack_check(env);
  /* INV: mkcl_char_code() checks the type of `c' */
  mkcl_return_value(MKCL_MAKE_FIXNUM(mkcl_char_code(env, c)));
}

struct mkcl_cfun mk_cl_code_char_cfunobj = MKCL_CFUN1(mk_cl_code_char, MK_CL_code_char);

mkcl_object
mk_cl_code_char(MKCL, mkcl_object c)
{
  mkcl_word fc;

  mkcl_call_stack_check(env);
  switch (mkcl_type_of(c)) {
  case mkcl_t_fixnum:
    fc = mkcl_fixnum_to_word(c);
    /* What about invalid codepoints like D800-DFFF or FFFE or FFFF? JCB */
    if ((0 <= fc && fc < 0x0D800) || (0x0DFFF < fc && fc != 0x0FFFE && fc != 0x0FFFF && fc < MKCL_CHAR_CODE_LIMIT))
      c = MKCL_CODE_CHAR(fc);
    else
      c = mk_cl_Cnil;
    break;
  case mkcl_t_bignum:
    c = mk_cl_Cnil;
    break;
  default:
    mkcl_FEtype_error_integer(env, c);
  }
  mkcl_return_value(c);
}

struct mkcl_cfun mk_cl_char_upcase_cfunobj = MKCL_CFUN1(mk_cl_char_upcase, MK_CL_char_upcase);

mkcl_object
mk_cl_char_upcase(MKCL, mkcl_object c)
{
  mkcl_call_stack_check(env);
  /* INV: mkcl_char_code() checks the type of `c' */
  mkcl_word code = mkcl_char_code(env, c);
  mkcl_return_value(MKCL_CODE_CHAR(mkcl_char_upcase(code)));
}

struct mkcl_cfun mk_cl_char_downcase_cfunobj = MKCL_CFUN1(mk_cl_char_downcase, MK_CL_char_downcase);

mkcl_object
mk_cl_char_downcase(MKCL, mkcl_object c)
{
  mkcl_call_stack_check(env);
  /* INV: mkcl_char_code() checks the type of `c' */
  mkcl_word code = mkcl_char_code(env, c);
  mkcl_return_value(MKCL_CODE_CHAR(mkcl_char_downcase(code)));
}

mkcl_object mk_cl_digit_char(MKCL, mkcl_narg narg, mkcl_object weight, ...)
{
  mkcl_call_stack_check(env);
  {
    mkcl_object radix = MKCL_MAKE_FIXNUM(10);
    MKCL_RECEIVE_1_OPTIONAL_ARGUMENT(env, MK_CL_digit_char, narg, 1, weight, &radix);

    {
      mkcl_word basis = mkcl_fixnum_in_range(env, MK_CL_digit_char,"radix",radix,2,36);
      mkcl_object output = mk_cl_Cnil;
    AGAIN:
      switch (mkcl_type_of(weight)) {
      case mkcl_t_fixnum: {
        mkcl_word value = mkcl_fixnum_to_word(weight);
        if (value >= 0) {
          int dw = mkcl_digit_char(value, basis);
          if (dw >= 0) {
            output = MKCL_CODE_CHAR(dw);
          }
        }
        break;
      }
      case mkcl_t_bignum:
        break;
      default:
        weight = mkcl_type_error(env, MK_CL_digit_char,"weight",weight,MK_CL_integer);
        goto AGAIN;
      }
      mkcl_return_value(output);
    }
  }
}

short
mkcl_digit_char(mkcl_word w, mkcl_word r)
{
  if (r < 2 || r > 36 || w < 0 || w >= r)
    return(-1);
  if (w < 10)
    return(w + '0');
  else
    return(w - 10 + 'A');
}

struct mkcl_cfun mk_cl_char_int_cfunobj = MKCL_CFUN1(mk_cl_char_int, MK_CL_char_int);

mkcl_object
mk_cl_char_int(MKCL, mkcl_object c)
{
  mkcl_call_stack_check(env);
  /* INV: mkcl_char_code() checks the type of `c' */
  mkcl_object val = MKCL_MAKE_FIXNUM(mkcl_char_code(env, c));
  mkcl_return1(val);
}

/* here we give every character an implicit name of the form 'u#' where # is a hexadecimal number,
   corresponding to a unicode code point.
   #\u14ea should work, for example
*/

struct mkcl_cfun mk_cl_char_name_cfunobj = MKCL_CFUN1(mk_cl_char_name, MK_CL_char_name);

mkcl_object
mk_cl_char_name(MKCL, mkcl_object c)
{
  mkcl_call_stack_check(env);
  mkcl_character code = mkcl_char_code(env, c);
  mkcl_object output = mk_cl_Cnil;

  if (/* (code >= 0) && */ (code <= MKCL_BASE_CHAR_CODE_LIMIT))
    output = mkcl_gethash_safe(env, MKCL_MAKE_FIXNUM(code), mkcl_core.base_char_names, mk_cl_Cnil);
  else
    {
      mkcl_object ext_names = MKCL_SYM_VAL(env, MK_SI_DYNVAR_extended_character_names);

      if (ext_names != mk_cl_Cnil)
        output =  mkcl_gethash_safe(env, MKCL_MAKE_FIXNUM(code), ext_names, mk_cl_Cnil);
      if (mkcl_Null(output))
        {
          char name[20] = { 0 };

          if (/* (code >= 0) && */ (code < 0x010000)) /* Are we confined to 16 bits? */
            snprintf(name, sizeof(name), "U%04x", code);
          else
            snprintf(name, sizeof(name), "U%06x", code);
          output = mkcl_make_base_string_copy(env, name);
        }
    }
  mkcl_return_value(output);
}

mkcl_object
mk_cl_name_char(MKCL, mkcl_object name)
{
  mkcl_object c;
  mkcl_index l;

  mkcl_call_stack_check(env);
  name = mk_cl_string(env, name);
  c = mkcl_gethash_safe(env, name, mkcl_core.base_char_names, mk_cl_Cnil);
  if (c != mk_cl_Cnil) {
    mkcl_return_value(MKCL_CODE_CHAR(mkcl_fixnum_to_word(c)));
  } else {
    mkcl_object ext_names = MKCL_SYM_VAL(env, MK_SI_DYNVAR_extended_character_names);

    if (ext_names != mk_cl_Cnil) c = mkcl_gethash_safe(env, name, ext_names, mk_cl_Cnil);
    if (c != mk_cl_Cnil) {
      mkcl_return_value(MKCL_CODE_CHAR(mkcl_fixnum_to_word(c)));
    } else if (mkcl_stringp(env, name) && (l = mkcl_length(env, name))) {
      c = mk_cl_char(env, name, MKCL_MAKE_FIXNUM(0));
      if (l == 1) {
        mkcl_return_value(mk_cl_Cnil);
      } else if (c != MKCL_CODE_CHAR('u') && c != MKCL_CODE_CHAR('U')) {
        mkcl_return_value(mk_cl_Cnil);
      } else {
        mkcl_index used_l;
        mkcl_index end = name->base_string.fillp;
        mkcl_index real_end = end;
        c = mkcl_parse_integer(env, name, 1, end, &real_end, 16);
        used_l = real_end;
        if (!MKCL_FIXNUMP(c) /* Bignum? */
            || (used_l < l)) /* Was character name cut short? JCB */
          { mkcl_return_value(mk_cl_Cnil); } 
        else
          {
            mkcl_character code = mkcl_fixnum_to_word(c);
            
            if (/* (code < 0) || */ (code >= MKCL_CHAR_CODE_LIMIT)) /* Outside valid character code range? */
              { mkcl_return_value(mk_cl_Cnil); }
            else
              { mkcl_return_value(MKCL_CODE_CHAR(code)); }
          }
      }
    }
  }
  mkcl_return_value(mk_cl_Cnil); /* Just in case. Should not be reached. JCB */
}
