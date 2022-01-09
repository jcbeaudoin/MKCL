/* -*- mode: c -*- */
/*
    string.d -- String routines.
*/
/*
    Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
    Copyright (c) 1990, Giuseppe Attardi.
    Copyright (c) 2001, Juan Jose Garcia Ripoll.
    Copyright (c) 2011-2016,2021, Jean-Claude Beaudoin.

    MKCL is free software; you can redistribute it and/or
    modify it under thep terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 3 of the License, or (at your option) any later version.

    See file '../../Copyright' for full details.
*/


#include <mkcl/mkcl.h>
#include <mkcl/internal.h>
#include <string.h>
#include <mkcl/mkcl-inl.h>

#if MKCL_WINDOWS
static inline mkcl_character * mkcl_wmemcpy(mkcl_character * dest, const mkcl_character * src, size_t len)
{
  size_t i = 0;

  for (i = 0; i < len; i++)
    dest[i] = src[i];

  return dest;
}

static inline mkcl_character * mkcl_wmemmove(mkcl_character * dest, const mkcl_character * src, size_t len)
{
  size_t i;

  if ( dest < src )
    for (i = 0; i < len; i++)
      dest[i] = src[i];
  else if ( dest > src )
    for (i = len; i; i--)
      dest[i-1] = src[i-1];

  return dest;
}

static inline mkcl_character * mkcl_wmemchr(const mkcl_character * ptr, mkcl_character val, size_t len)
{
  size_t i;

  for (i = 0; i < len; i++)
    if (ptr[i] == val)
      return (mkcl_character *) (ptr + i);

  return NULL;
}

static inline mkcl_character * mkcl_wmemset(mkcl_character * ptr, mkcl_character val, size_t len)
{
  size_t i;

  for (i = 0; i < len; i++)
    ptr[i] = val;
  return ptr;
}

static inline int mkcl_wmemcmp(const mkcl_character * ptr1, const mkcl_character * ptr2, size_t len)
{
  size_t i;

  for (i = 0; i < len; i++)
    if (ptr1[i] != ptr2[i])
      return ptr1[i] - ptr2[i];
  
  return 0;
}
#else /* !MKCL_WINDOWS */
# include <wchar.h>

# define mkcl_wmemcpy(d,s,l) ((mkcl_character *) wmemcpy((wchar_t *) d, (wchar_t *) s, l))
# define mkcl_wmemmove(d,s,l) ((mkcl_character *) wmemmove((wchar_t *) d, (wchar_t *) s, l))
# define mkcl_wmemchr(p,v,l) ((mkcl_character *) wmemchr((wchar_t *) p, (wchar_t) v, l))
# define mkcl_wmemset(p,v,l) ((mkcl_character *) wmemset((wchar_t *) p, (wchar_t) v, l))
# define mkcl_wmemcmp(p1,p2,l) wmemcmp((wchar_t *) p1, (wchar_t *) p2, l)
#endif /* !MKCL_WINDOWS */

typedef mkcl_character (*mkcl_casefun)(mkcl_character, bool *);

static mkcl_object
do_make_base_string(MKCL, mkcl_index s, mkcl_base_char code)
{
  mkcl_object x = mkcl_alloc_simple_base_string(env, s);
  mkcl_index i;
  for (i = 0;  i < s;  i++)
    x->base_string.self[i] = code;
  return x;
}

static mkcl_object
do_make_string(MKCL, mkcl_index s, mkcl_character code)
{
  mkcl_object x = mkcl_alloc_simple_character_string(env, s);
  mkcl_index i;
  for (i = 0;  i < s;  i++)
    x->string.self[i] = code;
  return x;
}

mkcl_object mk_cl_make_string(MKCL, mkcl_narg narg, mkcl_object size, ...)
{
  mkcl_call_stack_check(env);
  {

    mkcl_index s;
    mkcl_object x;
    mkcl_object initial_element = MKCL_CODE_CHAR(' ');
    mkcl_object element_type = MK_CL_character;

    MKCL_RECEIVE_2_KEYWORD_ARGUMENTS(env, MK_CL_make_string, narg, 1, size, MK_KEY_initial_element, &initial_element, MK_KEY_element_type, &element_type);

    s = mkcl_to_array_index(env, size);
    /* INV: mkcl_[base_]char_code() checks the type of initial_element() */
    if (element_type == MK_CL_base_char || element_type == MK_CL_standard_char) {
      int code = mkcl_base_char_code(env, initial_element);
      x = do_make_base_string(env, s, code);
    } else if (element_type == MK_CL_character) {
      mkcl_index code = mkcl_char_code(env, initial_element);
      x = do_make_string(env, s, code);
    } else if (mkcl_funcall2(env, MK_CL_subtypep->symbol.gfdef, element_type, MK_CL_base_char) == mk_cl_Ct) {
      int code = mkcl_base_char_code(env, initial_element);
      x = do_make_base_string(env, s, code);
    } else if (mkcl_funcall2(env, MK_CL_subtypep->symbol.gfdef, element_type, MK_CL_character) == mk_cl_Ct) {
      mkcl_index code = mkcl_char_code(env, initial_element);
      x = do_make_string(env, s, code);
    } else {
      mkcl_FEerror(env, "The type ~S is not a valid string char type.", 1, element_type);
    }
    mkcl_return_value(x);
  }
}

mkcl_object
mkcl_alloc_simple_base_string(MKCL, mkcl_index length)
{
  mkcl_object x;

  x = mkcl_alloc_raw_base_string(env);
  x->base_string.hasfillp     = FALSE;
  x->base_string.adjustable   = FALSE;
  x->base_string.displaced    = mk_cl_Cnil;
  x->base_string.dim          = (x->base_string.fillp = length);
  x->base_string.elem         = mkcl_base_char_index;
  x->base_string.set          = mkcl_base_char_set_index;
  x->base_string.self         = (mkcl_base_char *) mkcl_alloc_atomic(env, length + 1);
  x->base_string.self[length] = x->base_string.self[0] = 0;
  return x;
}

mkcl_object
mkcl_alloc_simple_character_string(MKCL, mkcl_index length)
{
  mkcl_object x;

  x = mkcl_alloc_raw_string(env);
  x->string.hasfillp   = FALSE;
  x->string.adjustable = FALSE;
  x->string.displaced  = mk_cl_Cnil;
  x->string.dim        = x->string.fillp = length;
  x->string.elem       = mkcl_character_index;
  x->string.set        = mkcl_character_set_index;
  x->string.self       = (mkcl_character *) mkcl_alloc_atomic(env, sizeof(mkcl_character)*(length + 1));
  return(x);
}

mkcl_object
mkcl_alloc_adjustable_base_string(MKCL, mkcl_index l)
{
  mkcl_object output = mkcl_alloc_simple_base_string(env, l);
  output->base_string.fillp = 0;
  output->base_string.hasfillp = TRUE;
  output->base_string.adjustable = TRUE;
  return output;
}

mkcl_object
mkcl_alloc_adjustable_character_string(MKCL, mkcl_index l)
{
  mkcl_object output = mkcl_alloc_simple_character_string(env, l);
  output->string.fillp = 0;
  output->string.hasfillp = TRUE;
  output->string.adjustable = TRUE;
  return output;
}

static void make_base_string_adjustable(MKCL, mkcl_object x)
{
  x->base_string.hasfillp = TRUE;
  x->base_string.adjustable = TRUE;
}

static void make_string_adjustable(MKCL, mkcl_object x)
{
  x->string.hasfillp = TRUE;
  x->string.adjustable = TRUE;
}

/*
  mkcl_make_simple_base_string(s) makes a simple-base string from C string s.
*/
mkcl_object
mkcl_make_simple_base_string(MKCL, char *s)
{
  mkcl_object x;
  mkcl_index l = (s ? strlen(s) : 0);

  x = mkcl_alloc_raw_base_string(env);
  x->base_string.hasfillp = FALSE;
  x->base_string.adjustable = FALSE;
  x->base_string.displaced = mk_cl_Cnil;
  x->base_string.dim = (x->base_string.fillp = l);
  x->base_string.elem = mkcl_base_char_index;
  x->base_string.set = mkcl_base_char_set_index;
  x->base_string.self = (mkcl_base_char *)s;
	
  return x;
}

mkcl_object
mkcl_make_adjustable_base_string(MKCL, char *s)
{
  mkcl_object x = mkcl_make_simple_base_string(env, s);

  make_base_string_adjustable(env, x);
  return x;
}

mkcl_object
mkcl_make_base_string_copy(MKCL, const char *s)
{
  mkcl_object x;
  mkcl_index l = (s ? strlen(s) : 0);
  
  x = mkcl_alloc_simple_base_string(env, l);
  if (s != NULL)
    memcpy(x->base_string.self, s, l);
  return x;
}

mkcl_object
mkcl_make_adjustable_base_string_copy(MKCL, const char *s)
{
  mkcl_object x = mkcl_make_base_string_copy(env, s);

  make_base_string_adjustable(env, x);
  return x;
}

mkcl_object mkcl_copy_base_string(MKCL, mkcl_object s)
{
  if (mkcl_unlikely(!MKCL_BASE_STRING_P(s))) mkcl_FEtype_error_base_string(env, s);

  mkcl_index len = s->base_string.fillp;
  mkcl_object new = mkcl_alloc_simple_base_string(env, len);
  if (len)
    memcpy(new->base_string.self, s->base_string.self, len);
  return new;
}

mkcl_object mkcl_copy_string(MKCL, mkcl_object s)
{
  mkcl_index len;
  mkcl_object new;

  if (mkcl_unlikely(!MKCL_STRINGP(s))) mkcl_FEtype_error_string(env, s);

  if (s->string.t == mkcl_t_string)
    {
      len = s->string.fillp;
      new = mkcl_alloc_simple_character_string(env, len);
      if (len)
	(void) mkcl_wmemcpy(new->string.self, s->string.self, len + 1);
    }
  else
    {
      len = s->base_string.fillp;
      new = mkcl_alloc_simple_base_string(env, len);
      if (len)
	memcpy(new->base_string.self, s->base_string.self, len + 1);
    }
  return new;
}

mkcl_object
mkcl_cstring_to_base_string(MKCL, const char *s)
{
  if (s == NULL)
    return mk_cl_Cnil;
  else
    return mkcl_make_base_string_copy(env, s);
}

bool
mkcl_fits_in_base_string(MKCL, mkcl_object s)
{
 AGAIN:
  switch (mkcl_type_of(s)) {
  case mkcl_t_string: {
    mkcl_index i;
    for (i = 0; i < s->string.fillp; i++) {
      if (!MKCL_BASE_CHAR_CODE_P(s->string.self[i]))
	return false;
    }
    return true;
  }
  case mkcl_t_base_string:
    return true;
  default:
    s = mkcl_type_error(env, MK_SI_copy_to_simple_base_string, "", s, MK_CL_string);
    goto AGAIN;
  }
}

static mkcl_object
mkcl_coerce_list_to_simple_base_string(MKCL, mkcl_object l)
{
  const mkcl_index len = mkcl_length(env, l);
  mkcl_object s = mkcl_alloc_simple_base_string(env, len);
  mkcl_base_char * elems = s->base_string.self;

  mkcl_loop_for_in(env, l) {
    mkcl_object maybe_char = MKCL_CAR(l);
    if (MKCL_BASE_CHAR_P(maybe_char))
      *(elems++) = MKCL_CHAR_CODE(maybe_char);
    else
      mkcl_FEtype_error_base_char(env, maybe_char);
  } mkcl_end_loop_for_in;
  return s;
}

static mkcl_object
mkcl_coerce_list_to_simple_string(MKCL, mkcl_object l)
{
  const mkcl_index len = mkcl_length(env, l);
  mkcl_object s = mkcl_alloc_simple_character_string(env, len);
  mkcl_character * elems = s->string.self;

  mkcl_loop_for_in(env, l) {
    mkcl_object maybe_char = MKCL_CAR(l);
    if (MKCL_CHARACTERP(maybe_char))
      *(elems++) = MKCL_CHAR_CODE(maybe_char);
    else
      mkcl_FEtype_error_character(env, maybe_char);
  } mkcl_end_loop_for_in;
  return s;
}

mkcl_object
mk_si_copy_to_simple_base_string(MKCL, mkcl_object x)
{
  mkcl_object y;

  mkcl_call_stack_check(env);
 AGAIN:
  switch(mkcl_type_of(x)) {
  case mkcl_t_symbol:
    x = x->symbol.name;
    goto AGAIN;
  case mkcl_t_character:
    x = mk_cl_string(env, x);
    goto AGAIN;
  case mkcl_t_string:
    {
      mkcl_index index, length = x->string.fillp;
      y = mkcl_alloc_simple_base_string(env, length);
      for (index=0; index < length; index++) {
	mkcl_character c = x->string.self[index];
	if (!MKCL_BASE_CHAR_CODE_P(c))
	  mkcl_FEerror(env, "Cannot coerce string ~A to a base-string", 1, x);
	y->base_string.self[index] = c;
      }
    }
    break;
  case mkcl_t_base_string:
    {
      mkcl_index length = x->base_string.fillp;
      y = mkcl_alloc_simple_base_string(env, length);
      memcpy(y->base_string.self, x->base_string.self, length + 1);
    }
    break;
  case mkcl_t_cons:
    y = mkcl_coerce_list_to_simple_base_string(env, x);
    break;
  case mkcl_t_null:
    x = mk_cl_Cnil_symbol.name;
    goto AGAIN;
  default:
    x = mkcl_type_error(env, MK_SI_copy_to_simple_base_string, "", x, MK_CL_string);
    goto AGAIN;
  }
  mkcl_return_value(y);
}

mkcl_object
mkcl_coerce_to_simple_base_string(MKCL, mkcl_object x) /* This one always returns a fresh base_string. */
{
  mkcl_object y;

  mkcl_call_stack_check(env);
 AGAIN:
  switch(mkcl_type_of(x)) {
  case mkcl_t_null: /* like cl::coerce */
    y = mkcl_core.empty_base_string;
    break;
  case mkcl_t_symbol: /* almost like cl::string */
    if (x != ((mkcl_object) &mk_cl_Cnil_symbol))
      {
        x = x->symbol.name;
        goto AGAIN;
      }
    else
      y = mkcl_core.empty_base_string;
    break;
  case mkcl_t_character: /* like cl::string */
    x = mk_cl_string(env, x);
    goto AGAIN;
  case mkcl_t_string:
    {
      mkcl_index index, length = x->string.fillp;
      y = mkcl_alloc_simple_base_string(env, length);
      for (index=0; index < length; index++) {
	mkcl_character c = x->string.self[index];
	if (!MKCL_BASE_CHAR_CODE_P(c))
	  mkcl_FEerror(env, "Cannot coerce string ~A to a base-string", 1, x);
	y->base_string.self[index] = c;
      }
    }
    break;
  case mkcl_t_base_string:
    {
      mkcl_index length = x->base_string.fillp;
      y = mkcl_alloc_simple_base_string(env, length);
      memcpy(y->base_string.self, x->base_string.self, length + 1);
    }
    break;
  case mkcl_t_vector:
    {
      const mkcl_index fp = x->vector.fillp;
      mkcl_index i = 0;

      y = mkcl_alloc_simple_base_string(env, fp);
      for (; i < fp; i++)
        {
          mkcl_object elem = mkcl_vref_index(env, x, i);
          y->base_string.self[i] = mkcl_base_char_code(env, elem);
        }
    }
    break;
  case mkcl_t_cons:
    y = mkcl_coerce_list_to_simple_base_string(env, x);
    break;
  default:
    x = mkcl_type_error(env, MK_SI_coerce_to_base_string, "", x, MK_CL_string);
    goto AGAIN;
  }
  mkcl_return_value(y);
}

mkcl_object mkcl_coerce_to_adjustable_base_string(MKCL, mkcl_object x)
{ 
  if ((mkcl_type_of(x) != mkcl_t_base_string) || !x->base_string.adjustable)
    {
      x = mkcl_coerce_to_simple_base_string(env, x);
      make_base_string_adjustable(env, x);
    }
  return x;
}

mkcl_object
mk_si_copy_to_simple_string(MKCL, mkcl_object x)
{
  mkcl_object y;

  mkcl_call_stack_check(env);
 AGAIN:
  switch(mkcl_type_of(x)) {
  case mkcl_t_symbol:
    x = x->symbol.name;
    goto AGAIN;
  case mkcl_t_character:
    x = mk_cl_string(env, x);
    goto AGAIN;
  case mkcl_t_string:
    {
      mkcl_index index, length = x->string.fillp;
      mkcl_character * from = x->string.self;
      mkcl_character * to;

      y = mkcl_alloc_simple_character_string(env, length);
      to = y->string.self;
      for (index = 0; index < length; index++)
	to[index] = from[index];
      to[index] = 0;
    }
    break;
  case mkcl_t_base_string:
    {
      mkcl_index index, length = x->base_string.fillp;
      mkcl_base_char * from = x->base_string.self;
      mkcl_character * to;

      y = mkcl_alloc_simple_character_string(env, length);
      to = y->string.self;
      for (index = 0; index < length; index++)
	to[index] = from[index];
      to[index] = 0;
    }
    break;
  case mkcl_t_cons:
    y = mkcl_coerce_list_to_simple_string(env, x);
    break;
  case mkcl_t_null:
    x = mk_cl_Cnil_symbol.name;
    goto AGAIN;
  default:
    x = mkcl_type_error(env, MK_SI_copy_to_simple_string, "", x, MK_CL_string);
    goto AGAIN;
  }
  mkcl_return_value(y);
}

mkcl_object mkcl_coerce_to_adjustable_string(MKCL, mkcl_object x)
{ 
  if ((mkcl_type_of(x) != mkcl_t_string) || !x->string.adjustable)
    {
      x = mkcl_coerce_to_simple_character_string(env, x);
      make_string_adjustable(env, x);
    }
  return x;
}


mkcl_object
mk_cl_string(MKCL, mkcl_object x)
{
  mkcl_call_stack_check(env);
 AGAIN:
  switch (mkcl_type_of(x)) {
  case mkcl_t_string:
  case mkcl_t_base_string:
    break;
  case mkcl_t_symbol:
    x = x->symbol.name;
    break;
  case mkcl_t_null:
    x = mk_cl_Cnil_symbol.name;
    break;
  case mkcl_t_UTF_8:
    x = mkcl_utf_8_to_string(env, x); /* Do we really want to drop the second value of this call? JCB */
    break;
  case mkcl_t_UTF_16:
    x = mkcl_utf_16_to_string(env, x); /* Do we really want to drop the second value of this call? JCB */
    break;
  case mkcl_t_character: {
    mkcl_object y;
    mkcl_character c = MKCL_CHAR_CODE(x);
    if (MKCL_BASE_CHAR_CODE_P(c)) {
      y = mkcl_alloc_simple_base_string(env, 1);
      y->base_string.self[0] = c;
      x = y;
    } else {
      y = mkcl_alloc_simple_character_string(env, 1);
      y->string.self[0] = c;
      x = y;
    }
    break;
  }
  default:
    x = mkcl_type_error(env, MK_CL_string, "", x, MK_CL_string);
    goto AGAIN;
  }
  mkcl_return_value(x);
}

mkcl_object
mk_si_coerce_to_base_string(MKCL, mkcl_object x)
{
  mkcl_call_stack_check(env);
#if 0
  if (mkcl_type_of(x) != mkcl_t_base_string) {
    x = mk_si_copy_to_simple_base_string(env, x);
  }
  mkcl_return_value(x);
#else
  mkcl_return_value(mkcl_coerce_to_base_string(env, x));
#endif
}


mkcl_object
mkcl_coerce_to_simple_character_string(MKCL, mkcl_object x) /* This one always returns a fresh string. */
{
  mkcl_object y;

  mkcl_call_stack_check(env);
 AGAIN:
  switch (mkcl_type_of(x))
    {
    case mkcl_t_base_string:
      {
        const mkcl_index length = x->base_string.fillp;
        y = mkcl_alloc_simple_character_string(env, length);
        {
          mkcl_base_char * from = x->base_string.self;
          mkcl_character * to = y->string.self;
          mkcl_index index;
          
          for (index = 0; index < length; index++)
            to[index] = from[index];
	}
      }
      break;
    case mkcl_t_string:
      {
        const mkcl_index length = x->string.fillp;
        y = mkcl_alloc_simple_character_string(env, length);
        {
          mkcl_character * from = x->string.self;
          mkcl_character * to = y->string.self;
          mkcl_index index;
          
          for (index = 0; index < length; index++)
            to[index] = from[index];
        }
      }
      break;
    case mkcl_t_character:
      y = mkcl_alloc_simple_character_string(env, 1);
      y->string.self[0] = MKCL_CHAR_CODE(x);
      break;
    case mkcl_t_symbol:
      if (x == ((mkcl_object) &mk_cl_Cnil_symbol))
        x = mkcl_core.empty_string;
      else
        x = x->symbol.name;
      goto AGAIN;
    case mkcl_t_null:
      x = mkcl_core.empty_string;
      goto AGAIN;
    case mkcl_t_vector:
      {
        const mkcl_index fp = x->vector.fillp;
        mkcl_index i = 0;
        
        y = mkcl_alloc_simple_character_string(env, fp);
        for (; i < fp; i++)
          {
            mkcl_object elem = mkcl_vref_index(env, x, i);
            y->string.self[i] = mkcl_char_code(env, elem);
          }
      }
      break;
    case mkcl_t_cons:
      y = mkcl_coerce_list_to_simple_string(env, x);
      break;
    default:
      x = mkcl_type_error(env, MK_SI_coerce_to_character_string, "", x, MK_CL_string);
      goto AGAIN;
    }
  mkcl_return_value(y);
}

mkcl_object
mk_si_coerce_to_character_string(MKCL, mkcl_object x)
{
  mkcl_object y;

  mkcl_call_stack_check(env);
  if (mkcl_type_of(x) != mkcl_t_string)
    y = mkcl_coerce_to_simple_character_string(env, x);
  else
    y = x;
  mkcl_return_value(y);
}

struct mkcl_cfun mk_cl_char_cfunobj = MKCL_CFUN2(mk_cl_char, MK_CL_char);
struct mkcl_cfun mk_cl_schar_cfunobj = MKCL_CFUN2(mk_cl_char, MK_CL_schar);

mkcl_object
mk_cl_char(MKCL, mkcl_object object, mkcl_object index)
{
  mkcl_index i;

  mkcl_call_stack_check(env);
  for (;;)
    switch(mkcl_type_of(object)) {
    case mkcl_t_string:
      if (MKCL_FIXNUMP(index) && ((i = mkcl_fixnum_to_word(index)) < object->string.dim))
	{ mkcl_return_value(MKCL_CODE_CHAR(object->string.self[i])); }
      else
	{
	  i = mkcl_fixnum_to_word(mkcl_ensure_valid_array_index_type(env, object, index));
	  mkcl_return_value(MKCL_CODE_CHAR(object->string.self[i]));
	}
    case mkcl_t_base_string:
      if (MKCL_FIXNUMP(index) && ((i = mkcl_fixnum_to_word(index)) < object->base_string.dim))
	{ mkcl_return_value(MKCL_CODE_CHAR(object->base_string.self[i])); }
      else
	{
	  i = mkcl_fixnum_to_word(mkcl_ensure_valid_array_index_type(env, object, index));
	  mkcl_return_value(MKCL_CODE_CHAR(object->base_string.self[i]));
	}
    default:
      object = mkcl_type_error(env, MK_CL_char,"",object,MK_CL_string);
      break;
    }
}

mkcl_character
mkcl_char(MKCL, mkcl_object object, mkcl_index index)
{
  /* CHAR bypasses fill pointers when accessing strings */

  for (;;)
    switch(mkcl_type_of(object)) {
    case mkcl_t_string:
      if (index >= object->string.dim)
	mkcl_FEillegal_index(env, object, MKCL_MAKE_FIXNUM(index));
      return object->string.self[index];
    case mkcl_t_base_string:
      if (index >= object->base_string.dim)
	mkcl_FEillegal_index(env, object, MKCL_MAKE_FIXNUM(index));
      return object->base_string.self[index];
    default:
      object = mkcl_type_error(env, MK_CL_char,"",object,MK_CL_string);
      break;
    }
}

struct mkcl_cfun mk_si_schar_set_cfunobj = MKCL_CFUN3(mk_si_char_set, MK_SI_schar_set);
struct mkcl_cfun mk_si_char_set_cfunobj = MKCL_CFUN3(mk_si_char_set, MK_SI_char_set);

mkcl_object
mk_si_char_set(MKCL, mkcl_object object, mkcl_object index, mkcl_object value)
{
  mkcl_index i;

  mkcl_call_stack_check(env);
  for (;;)
    switch(mkcl_type_of(object)) {
    case mkcl_t_string:
      {
	mkcl_character c;

	if (MKCL_CHARACTERP(value))
	  c = MKCL_CHAR_CODE(value);
	else
	  mkcl_FEtype_error_character(env, value);
      
	if (MKCL_FIXNUMP(index) && ((i = mkcl_fixnum_to_word(index)) < object->string.dim))
	  {
	    object->string.self[i] = c;
	    mkcl_return_value(value);
	  }
	else
	  {
	    i = mkcl_fixnum_to_word(mkcl_ensure_valid_array_index_type(env, object, index));
	    object->string.self[i] = c;
	    mkcl_return_value(value);
	  }
      }
    case mkcl_t_base_string:
      {
	mkcl_base_char c;

	if (MKCL_BASE_CHAR_P(value))
	  c = MKCL_CHAR_CODE(value);
	else
	  mkcl_FEwrong_type_argument(env, MK_CL_base_char, value);
	
	if (MKCL_FIXNUMP(index) && ((i = mkcl_fixnum_to_word(index)) < object->base_string.dim))
	  {
	    object->base_string.self[i] = c;
	    mkcl_return_value(value);
	  }
	else
	  {
	    i = mkcl_fixnum_to_word(mkcl_ensure_valid_array_index_type(env, object, index));
	    object->base_string.self[i] = c;
	    mkcl_return_value(value);
	  }
      }
    default:
      object = mkcl_type_error(env, MK_CL_char,"",object,MK_CL_string);
      break;
    }
}

mkcl_character
mkcl_char_set(MKCL, mkcl_object object, mkcl_index index, mkcl_character value)
{
 AGAIN:
  /* CHAR bypasses fill pointers when accessing strings */
  switch(mkcl_type_of(object)) {
  case mkcl_t_string:
    if (index >= object->string.dim)
      mkcl_FEillegal_index(env, object, MKCL_MAKE_FIXNUM(index));
    return object->string.self[index] = value;
  case mkcl_t_base_string:
    if (index >= object->base_string.dim)
      mkcl_FEillegal_index(env, object, MKCL_MAKE_FIXNUM(index));
    return object->base_string.self[index] = value;
  default:
    object = mkcl_type_error(env, MK_SI_char_set, "", object, MK_CL_string);
    goto AGAIN;
  }
}

void
mkcl_get_string_start_end(MKCL, mkcl_object string, mkcl_object start, mkcl_object end,
			  mkcl_index *ps, mkcl_index *pe)
{
  /* INV: works on both mkcl_t_base_string and mkcl_t_string */
  /* INV: Works with either string or symbol */
  if (!MKCL_FIXNUMP(start) || MKCL_FIXNUM_MINUSP(start))
    goto E;
  else
    *ps = mkcl_fixnum_to_word(start);
  if (mkcl_Null(end)) {
    *pe = string->vector.fillp;
    if (*pe < *ps)
      goto E;
  } else if (!MKCL_FIXNUMP(end) || MKCL_FIXNUM_MINUSP(end))
    goto E;
  else {
    *pe = mkcl_fixnum_to_word(end);
    if (*pe < *ps || *pe > string->vector.fillp)
      goto E;
  }
  return;

 E:
  mkcl_FEerror(env,
	       "~S and ~S are illegal as :START and :END~%"
	       "for the string designator ~S.",
	       3, start, end, string);
}

static int
compare_strings(MKCL, mkcl_object string1, mkcl_index s1, mkcl_index e1,
		mkcl_object string2, mkcl_index s2, mkcl_index e2,
		int case_sensitive, mkcl_index *m)
{
  mkcl_character c1, c2;
  for (; s1 < e1; s1++, s2++) {
    if (s2 >= e2) { /* s1 is longer than s2, therefore s2 < s1 */
      *m = s1;
      return +1;
    }
    c1 = mkcl_char(env, string1, s1);
    c2 = mkcl_char(env, string2, s2);
    if (!case_sensitive) {
      c1 = mkcl_char_upcase(c1);
      c2 = mkcl_char_upcase(c2);
    }
    if (c1 < c2) {
      *m = s1;
      return -1;
    } else if (c1 > c2) {
      *m = s1;
      return +1;
    }
  }
  *m = s1;
  if (s2 >= e2) {
    return 0;
  } else { /* s1 is shorter than s2, hence s1 < s2 */
    return -1;
  }
}

static int
compare_base(unsigned char *s1, mkcl_index l1, unsigned char *s2, mkcl_index l2,
	     int case_sensitive, mkcl_index *m)
{
  mkcl_index l;
  mkcl_character c1, c2;

  for (l = 0; l < l1; l++, s1++, s2++) {
    if (l == l2) { /* s1 is longer than s2, therefore s2 < s1 */
      *m = l;
      return +1;
    }
    c1 = *s1;
    c2 = *s2;
    if (!case_sensitive) {
      c1 = mkcl_char_upcase(c1);
      c2 = mkcl_char_upcase(c2);
    }
    if (c1 < c2) {
      *m = l;
      return -1;
    } else if (c1 > c2) {
      *m = l;
      return +1;
    }
  }
  *m = l;
  if (l1 == l2) 
    return 0;
  else { /* s1 is shorter than s2, hence s1 < s2 */
    return -1;
  }
}

mkcl_object mk_cl_stringE(MKCL, mkcl_narg narg, mkcl_object string1, mkcl_object string2, ...)
{
  mkcl_call_stack_check(env);
  {
    mkcl_index s1, e1, s2, e2;
    mkcl_object start1 = MKCL_MAKE_FIXNUM(0);
    mkcl_object end1 = mk_cl_Cnil;
    mkcl_object start2 = MKCL_MAKE_FIXNUM(0);
    mkcl_object end2 = mk_cl_Cnil;
    MKCL_RECEIVE_4_KEYWORD_ARGUMENTS(env, MK_CL_stringE, narg, 2, string2, MK_KEY_start1, &start1, MK_KEY_end1, &end1, MK_KEY_start2, &start2, MK_KEY_end2, &end2);

    /* AGAIN: */
    string1 = mk_cl_string(env, string1);
    string2 = mk_cl_string(env, string2);
    mkcl_get_string_start_end(env, string1, start1, end1, &s1, &e1);
    mkcl_get_string_start_end(env, string2, start2, end2, &s2, &e2);
    if (e1 - s1 != e2 - s2)
      { mkcl_return_value(mk_cl_Cnil); }
    switch(mkcl_type_of(string1)) {
    case mkcl_t_string:
      switch(mkcl_type_of(string2)) {
      case mkcl_t_string:
        while (s1 < e1)
          if (string1->string.self[s1++] != string2->string.self[s2++])
            { mkcl_return_value(mk_cl_Cnil); }
        mkcl_return_value(mk_cl_Ct);
      case mkcl_t_base_string:
        while (s1 < e1)
          if (string1->string.self[s1++] != string2->base_string.self[s2++])
            { mkcl_return_value(mk_cl_Cnil); }
        mkcl_return_value(mk_cl_Ct);
      default: break; /* a useless no-op to please clang. */
      }
      break;
    case mkcl_t_base_string:
      switch(mkcl_type_of(string2)) {
      case mkcl_t_string:
        while (s1 < e1)
          if (string1->base_string.self[s1++] != string2->string.self[s2++])
            { mkcl_return_value(mk_cl_Cnil); }
        mkcl_return_value(mk_cl_Ct);
      case mkcl_t_base_string:
        while (s1 < e1)
          if (string1->base_string.self[s1++] != string2->base_string.self[s2++])
            { mkcl_return_value(mk_cl_Cnil); }
        mkcl_return_value(mk_cl_Ct);
      default: break; /* a useless no-op to please clang. */
      }
      break;
    default: break; /* a useless no-op to please clang. */
    }
    mkcl_return_value(mk_cl_Ct);
  }
}

/*
	This correponds to string= (just the string equality).
*/
bool
mkcl_string_E(MKCL, mkcl_object x, mkcl_object y)
{
  mkcl_index i, j;
 AGAIN:
  switch(mkcl_type_of(x)) {
  case mkcl_t_base_string:
    switch(mkcl_type_of(y)) {
    case mkcl_t_base_string: {
      i = x->base_string.fillp;
      j = y->base_string.fillp;
      if (mkcl_unlikely(i != j)) return FALSE;
      return memcmp(x->base_string.self, y->base_string.self, i) == 0;
    }
    case mkcl_t_string: {
      mkcl_index index;
      i = x->base_string.fillp;
      j = y->string.fillp;
      if (mkcl_unlikely(i != j)) return FALSE;
      for(index=0; index<i; index++)
	if (x->base_string.self[index] != y->string.self[index])
	  return FALSE;
      return TRUE;
    }
    default:
      y = mkcl_type_error(env, MK_CL_stringE, "", y, MK_CL_string);
      goto AGAIN;
    }
    break;
  case mkcl_t_string:
    switch(mkcl_type_of(y)) {
    case mkcl_t_base_string: {
      mkcl_index index;
      i = x->string.fillp;
      j = y->base_string.fillp;
      if (mkcl_unlikely(i != j)) return FALSE;
      for(index=0; index<i; index++)
	if (x->string.self[index] != y->base_string.self[index])
	  return FALSE;
      return TRUE;
    }
    case mkcl_t_string: {
      i = x->string.fillp;
      j = y->string.fillp;
      if (mkcl_unlikely(i != j)) return FALSE;
      return mkcl_wmemcmp(x->string.self, y->string.self, i) == 0;
    }
    default:
      y = mkcl_type_error(env, MK_CL_stringE, "", y, MK_CL_string);
      goto AGAIN;
    }
    break;
  default:
    x = mkcl_type_error(env, MK_CL_stringE, "", x, MK_CL_string);
    goto AGAIN;
  }
}


mkcl_object mk_cl_string_equal(MKCL, mkcl_narg narg, mkcl_object string1, mkcl_object string2, ...)
{
  mkcl_call_stack_check(env);
  {

    mkcl_index s1, e1, s2, e2;
    int output;
    mkcl_object start1 = MKCL_MAKE_FIXNUM(0);
    mkcl_object end1 = mk_cl_Cnil;
    mkcl_object start2 = MKCL_MAKE_FIXNUM(0);
    mkcl_object end2 = mk_cl_Cnil;

    MKCL_RECEIVE_4_KEYWORD_ARGUMENTS(env, MK_CL_string_equal, narg, 2, string2, MK_KEY_start1, &start1, MK_KEY_end1, &end1, MK_KEY_start2, &start2, MK_KEY_end2, &end2);

    /* AGAIN: */
    string1 = mk_cl_string(env, string1);
    string2 = mk_cl_string(env, string2);
    mkcl_get_string_start_end(env, string1, start1, end1, &s1, &e1);
    mkcl_get_string_start_end(env, string2, start2, end2, &s2, &e2);
    if (e1 - s1 != e2 - s2)
      { mkcl_return_value(mk_cl_Cnil); }
    if (mkcl_type_of(string1) != mkcl_t_base_string || mkcl_type_of(string2) != mkcl_t_base_string) {
      output = compare_strings(env, string1, s1, e1, string2, s2, e2, 0, &e1);
    } else
      output = compare_base(string1->base_string.self + s1, e1 - s1,
                            string2->base_string.self + s2, e2 - s2,
                            0, &e1);
    mkcl_return_value(((output == 0) ? mk_cl_Ct : mk_cl_Cnil));
  }
}

static mkcl_object
string_compare(MKCL, mkcl_narg narg, int sign1, int sign2, int case_sensitive, mkcl_va_list ARGS)
{
  mkcl_object string1 = mkcl_va_arg(ARGS);
  mkcl_object string2 = mkcl_va_arg(ARGS);
  mkcl_index s1, e1, s2, e2;
  int output;
  mkcl_object result;
  mkcl_object KEYS[4];
#define start1 KEY_VARS[0]
#define end1 KEY_VARS[1]
#define start2 KEY_VARS[2]
#define end2 KEY_VARS[3]
#define start1p KEY_VARS[4]
#define start2p KEY_VARS[6]
  mkcl_object KEY_VARS[8];

  if (narg < 2) mkcl_FEwrong_num_arguments_anonym(env, 2, -1, narg);
  KEYS[0]=MK_KEY_start1;
  KEYS[1]=MK_KEY_end1;
  KEYS[2]=MK_KEY_start2;
  KEYS[3]=MK_KEY_end2;
  mkcl_parse_key(env, ARGS, 4, KEYS, KEY_VARS, NULL, FALSE, FALSE);

  string1 = mk_cl_string(env, string1);
  string2 = mk_cl_string(env, string2);
  if (start1p == mk_cl_Cnil) start1 = MKCL_MAKE_FIXNUM(0);
  if (start2p == mk_cl_Cnil) start2 = MKCL_MAKE_FIXNUM(0);
  mkcl_get_string_start_end(env, string1, start1, end1, &s1, &e1);
  mkcl_get_string_start_end(env, string2, start2, end2, &s2, &e2);
  if (mkcl_type_of(string1) != mkcl_t_base_string || mkcl_type_of(string2) != mkcl_t_base_string) {
    output = compare_strings(env, string1, s1, e1, string2, s2, e2, case_sensitive, &e1);
  } else {
    output = compare_base(string1->base_string.self + s1, e1 - s1,
			  string2->base_string.self + s2, e2 - s2,
			  case_sensitive, &e1);
    e1 += s1;
  }
  if (output == sign1 || output == sign2) {
    result = MKCL_MAKE_FIXNUM(e1);
  } else {
    result = mk_cl_Cnil;
  }
  mkcl_va_end(ARGS); 
  mkcl_return_value(result);
#undef start1p
#undef start2p
#undef start1
#undef end1
#undef start2
#undef end2
}

mkcl_object mk_cl_stringL(MKCL, mkcl_narg narg, ...)
{
  mkcl_call_stack_check(env);
  {
    mkcl_setup_for_rest(env, MK_CL_stringL, 0, narg, narg, args);

    return string_compare(env, narg, -1, -1, 1, args);
  }
}

mkcl_object mk_cl_stringG(MKCL, mkcl_narg narg, ...)
{
  mkcl_call_stack_check(env);
  {
    mkcl_setup_for_rest(env, MK_CL_stringG, 0, narg, narg, args);

    return string_compare(env, narg, +1, +1, 1, args);
  }
}

mkcl_object mk_cl_stringLE(MKCL, mkcl_narg narg, ...)
{
  mkcl_call_stack_check(env);
  {
    mkcl_setup_for_rest(env, MK_CL_stringLE, 0, narg, narg, args);

    return string_compare(env, narg, -1, 0, 1, args);
  }
}

mkcl_object mk_cl_stringGE(MKCL, mkcl_narg narg, ...)
{
  mkcl_call_stack_check(env);
  {
    mkcl_setup_for_rest(env, MK_CL_stringGE, 0, narg, narg, args);

    return string_compare(env, narg, 0, +1, 1, args);
  }
}


mkcl_object mk_cl_stringNE(MKCL, mkcl_narg narg, ...)
{
  mkcl_call_stack_check(env);
  {
    mkcl_setup_for_rest(env, MK_CL_stringNE, 0, narg, narg, args);

    return string_compare(env, narg, -1, +1, 1, args);
  }
}

mkcl_object mk_cl_string_lessp(MKCL, mkcl_narg narg, ...)
{
  mkcl_call_stack_check(env);
  {
    mkcl_setup_for_rest(env, MK_CL_string_lessp, 0, narg, narg, args);

    return string_compare(env, narg, -1, -1, 0, args);
  }
}

mkcl_object mk_cl_string_greaterp(MKCL, mkcl_narg narg, ...)
{
  mkcl_call_stack_check(env);
  {
    mkcl_setup_for_rest(env, MK_CL_string_greaterp, 0, narg, narg, args);

  return string_compare(env, narg, +1, +1, 0, args);
  }
}

mkcl_object mk_cl_string_not_greaterp(MKCL, mkcl_narg narg, ...)
{
  mkcl_call_stack_check(env);
  {
    mkcl_setup_for_rest(env, MK_CL_string_not_greaterp, 0, narg, narg, args);

    return string_compare(env, narg, -1, 0, 0, args);
  }
}

mkcl_object mk_cl_string_not_lessp(MKCL, mkcl_narg narg, ...)
{
  mkcl_call_stack_check(env);
  {
    mkcl_setup_for_rest(env, MK_CL_string_not_lessp, 0, narg, narg, args);

    return string_compare(env, narg, 0, +1, 0, args);
  }
}

mkcl_object mk_cl_string_not_equal(MKCL, mkcl_narg narg, ...)
{
  mkcl_call_stack_check(env);
  {
    mkcl_setup_for_rest(env, MK_CL_string_not_equal, 0, narg, narg, args);

    return string_compare(env, narg, -1, +1, 0, args);
  }
}


mkcl_object mkcl_base_string_L(MKCL, mkcl_object str1, mkcl_object str2)
{ /* returns the "mismatch-index" on success or ((mkcl_index)-1) on failure. */
  while (mkcl_unlikely(!MKCL_STRINGP(str1))) str1 = mkcl_type_error(env, MK_CL_stringE, "", str1, MK_CL_string);
  while (mkcl_unlikely(!MKCL_STRINGP(str2))) str2 = mkcl_type_error(env, MK_CL_stringE, "", str2, MK_CL_string);

  mkcl_index i = str1->base_string.fillp;
  mkcl_index j = str2->base_string.fillp;

  if (mkcl_unlikely(i != j))
    return mk_cl_Cnil;
  else
    return (memcmp(str1->base_string.self, str2->base_string.self, i) == 0) ? mk_cl_Ct : mk_cl_Cnil;
}

#if 0 /* Not implemented yet. */
mkcl_object mkcl_base_string_NE(MKCL, mkcl_object str1, mkcl_object str2)
{ /* returns the "mismatch-index" on success or ((mkcl_index)-1) on failure. */

}

mkcl_object mkcl_base_string_G(MKCL, mkcl_object str1, mkcl_object str2)
{ /* returns the "mismatch-index" on success or ((mkcl_index)-1) on failure. */

}

mkcl_object mkcl_base_string_LE(MKCL, mkcl_object str1, mkcl_object str2)
{ /* returns the "mismatch-index" on success or ((mkcl_index)-1) on failure. */

}

mkcl_object mkcl_base_string_GE(MKCL, mkcl_object str1, mkcl_object str2)
{ /* returns the "mismatch-index" on success or ((mkcl_index)-1) on failure. */

}
#endif


/* ***************************************** */


bool
mkcl_member_char(MKCL, mkcl_character c, mkcl_object char_bag)
{
  mkcl_index i, f;

  if ( mkcl_Null(char_bag) ) return(FALSE);

 AGAIN:
  switch (mkcl_type_of(char_bag)) {
  case mkcl_t_cons:
    mkcl_loop_for_in(env, char_bag) {
      mkcl_object other = MKCL_CAR(char_bag);
      while (!MKCL_CHARACTERP(other))
        other = mkcl_type_error(env, MK_CL_member, "", other, MK_CL_character);
      if (c == MKCL_CHAR_CODE(other))
	return(TRUE);
    } mkcl_end_loop_for_in;
    return(FALSE);
  case mkcl_t_vector:
    switch (char_bag->vector.elttype)
      {
      case mkcl_aet_object:
        for (i = 0, f = char_bag->vector.fillp;  i < f;  i++) {
          mkcl_object other = char_bag->vector.self.t[i];
          while (!MKCL_CHARACTERP(other))
            other = mkcl_type_error(env, MK_CL_member, "", other, MK_CL_character);
          if (c == MKCL_CHAR_CODE(other))
            return(TRUE);
        }
        return(FALSE);
      case mkcl_aet_ch:
        for (i = 0, f = char_bag->vector.fillp;  i < f;  i++) {
          if (c == char_bag->vector.self.c[i])
            return(TRUE);
        }
        return(FALSE);
      case mkcl_aet_bc:
        for (i = 0, f = char_bag->vector.fillp;  i < f;  i++) {
          if (c == char_bag->vector.self.bc[i])
            return(TRUE);
        }
        return(FALSE);
      default:
        char_bag = mkcl_type_error(env, MK_CL_member, "", char_bag, MK_CL_sequence);
        goto AGAIN;
      }
  case mkcl_t_string:
    for (i = 0, f = char_bag->string.fillp;  i < f;  i++) {
      if (c == char_bag->string.self[i])
	return(TRUE);
    }
    return(FALSE);
  case mkcl_t_base_string:
    for (i = 0, f = char_bag->base_string.fillp;  i < f;  i++) {
      if (c == char_bag->base_string.self[i])
	return(TRUE);
    }
    return(FALSE);
  case mkcl_t_bitvector:
    return(FALSE);
  default:
    char_bag = mkcl_type_error(env, MK_CL_member,"",char_bag,MK_CL_sequence);
    goto AGAIN;
  }
}

static mkcl_object
string_trim0(MKCL, bool left_trim, bool right_trim, mkcl_object char_bag, mkcl_object strng)
{
  mkcl_index i, j;

  strng = mk_cl_string(env, strng);
  i = 0;
  j = mkcl_length(env, strng);
  if (left_trim) {
    for (;  i < j;  i++) {
      mkcl_character c = mkcl_char(env, strng, i);
      if (!mkcl_member_char(env, c, char_bag))
	break;
    }
  }
  if (right_trim) {
    for (; j > i; j--) {
      mkcl_character c = mkcl_char(env, strng, j-1);
      if (!mkcl_member_char(env, c, char_bag)) {
	break;
      }
    }
  }
  return mk_cl_subseq(env, 3, strng, MKCL_MAKE_FIXNUM(i), MKCL_MAKE_FIXNUM(j));
}


struct mkcl_cfun mk_cl_string_trim_cfunobj = MKCL_CFUN2(mk_cl_string_trim, MK_CL_string_trim);

mkcl_object
mk_cl_string_trim(MKCL, mkcl_object char_bag, mkcl_object strng)
{
  mkcl_call_stack_check(env);
  return string_trim0(env, TRUE, TRUE, char_bag, strng);
}

struct mkcl_cfun mk_cl_string_left_trim_cfunobj = MKCL_CFUN2(mk_cl_string_left_trim, MK_CL_string_left_trim);

mkcl_object
mk_cl_string_left_trim(MKCL, mkcl_object char_bag, mkcl_object strng)
{
  mkcl_call_stack_check(env);
  return string_trim0(env, TRUE, FALSE, char_bag, strng);
}

struct mkcl_cfun mk_cl_string_right_trim_cfunobj = MKCL_CFUN2(mk_cl_string_right_trim, MK_CL_string_right_trim);

mkcl_object
mk_cl_string_right_trim(MKCL, mkcl_object char_bag, mkcl_object strng)
{
  mkcl_call_stack_check(env);
  return string_trim0(env, FALSE, TRUE, char_bag, strng);
}

static mkcl_object
string_case(MKCL, mkcl_narg narg, mkcl_casefun casefun, mkcl_va_list ARGS)
{
  mkcl_object strng = mkcl_va_arg(ARGS);
  mkcl_index s, e, i;
  bool b;
  mkcl_object KEYS[2];
#define start KEY_VARS[0]
#define end KEY_VARS[1]
#define startp KEY_VARS[2]
  mkcl_object conv;
  mkcl_object KEY_VARS[4];

  if (narg < 1) mkcl_FEwrong_num_arguments_anonym(env, 1, -1, narg);
  KEYS[0]=MK_KEY_start;
  KEYS[1]=MK_KEY_end;
  mkcl_parse_key(env, ARGS, 2, KEYS, KEY_VARS, NULL, FALSE, FALSE);

  strng = mk_cl_string(env, strng);
  conv  = mk_cl_copy_seq(env, strng);
  if (startp == mk_cl_Cnil)
    start = MKCL_MAKE_FIXNUM(0);
  mkcl_get_string_start_end(env, conv, start, end, &s, &e);
  b = TRUE;
  switch(mkcl_type_of(conv)) {
  case mkcl_t_string:
    for (i = s;  i < e;  i++)
      conv->string.self[i] = (*casefun)(conv->string.self[i], &b);
    break;
  case mkcl_t_base_string:
    for (i = s;  i < e;  i++)
      conv->base_string.self[i] = (*casefun)(conv->base_string.self[i], &b);
    break;
  default: break; /* a useless no-op to please clang. */
  }
  mkcl_va_end(ARGS); 
  mkcl_return_value(conv);
#undef startp
#undef start
#undef end
}

static mkcl_character
char_upcase(mkcl_character c, bool *bp)
{
  return mkcl_char_upcase(c);
}

mkcl_object mk_cl_string_upcase(MKCL, mkcl_narg narg, ...)
{
  mkcl_call_stack_check(env);
  {
    mkcl_setup_for_rest(env, MK_CL_string_upcase, 0, narg, narg, args);

    return string_case(env, narg, char_upcase, args);
  }
}

static mkcl_character
char_downcase(mkcl_character c, bool *bp)
{
  return mkcl_char_downcase(c);
}

mkcl_object mk_cl_string_downcase(MKCL, mkcl_narg narg, ...)
{
  mkcl_call_stack_check(env);
  {
    mkcl_setup_for_rest(env, MK_CL_string_downcase, 0, narg, narg, args);

    return string_case(env, narg, char_downcase, args);
  }
}

static mkcl_character
char_capitalize(mkcl_character c, bool *bp)
{
  if (mkcl_lower_case_p(c)) {
    if (*bp)
      c = mkcl_char_upcase(c);
    *bp = FALSE;
  } else if (mkcl_upper_case_p(c)) {
    if (!*bp)
      c = mkcl_char_downcase(c);
    *bp = FALSE;
  } else {
    *bp = !mkcl_alphanumericp(c);
  }
  return c;
}

mkcl_object mk_cl_string_capitalize(MKCL, mkcl_narg narg, ...)
{
  mkcl_call_stack_check(env);
  {
    mkcl_setup_for_rest(env, MK_CL_string_capitalize, 0, narg, narg, args);

    return string_case(env, narg, char_capitalize, args);
  }
}


static mkcl_object
nstring_case(MKCL, mkcl_narg narg, mkcl_object fun, mkcl_casefun casefun, mkcl_va_list ARGS)
{
  mkcl_object strng = mkcl_va_arg(ARGS);
  mkcl_index s, e, i;
  bool b;
  mkcl_object KEYS[2];
#define start KEY_VARS[0]
#define end KEY_VARS[1]
#define startp KEY_VARS[2]
  mkcl_object KEY_VARS[4];

  if (narg < 1) mkcl_FEwrong_num_arguments_anonym(env, 1, -1, narg);
  KEYS[0]=MK_KEY_start;
  KEYS[1]=MK_KEY_end;
  mkcl_parse_key(env, ARGS, 2, KEYS, KEY_VARS, NULL, FALSE, FALSE);

  strng = mkcl_check_type_string(env, fun,strng);
  if (startp == mk_cl_Cnil) start = MKCL_MAKE_FIXNUM(0);
  mkcl_get_string_start_end(env, strng, start, end, &s, &e);
  b = TRUE;
  if (mkcl_type_of(strng) == mkcl_t_string) {
    for (i = s;  i < e;  i++)
      strng->string.self[i] = (*casefun)(strng->string.self[i], &b);
  } else {
    for (i = s;  i < e;  i++)
      strng->base_string.self[i] = (*casefun)(strng->base_string.self[i], &b);
  }
  mkcl_va_end(ARGS);
  mkcl_return_value(strng);
#undef startp
#undef start
#undef end
}

mkcl_object mk_cl_nstring_upcase(MKCL, mkcl_narg narg, ...)
{
  mkcl_call_stack_check(env);
  {
    mkcl_setup_for_rest(env, MK_CL_nstring_upcase, 0, narg, narg, args);

    return nstring_case(env, narg, MK_CL_nstring_upcase, char_upcase, args);
  }
}

mkcl_object mk_cl_nstring_downcase(MKCL, mkcl_narg narg, ...)
{
  mkcl_call_stack_check(env);
  {
    mkcl_setup_for_rest(env, MK_CL_nstring_downcase, 0, narg, narg, args);

    return nstring_case(env, narg, MK_CL_nstring_downcase, char_downcase, args);
  }
}

mkcl_object mk_cl_nstring_capitalize(MKCL, mkcl_narg narg, ...)
{
  mkcl_call_stack_check(env);
  {
    mkcl_setup_for_rest(env, MK_CL_nstring_capitalize, 0, narg, narg, args);

    return nstring_case(env, narg, MK_CL_nstring_capitalize, char_capitalize, args);
  }
}

mkcl_object mk_si_concatenate_base_strings(MKCL, mkcl_narg narg, ...)
{
  mkcl_object output;

  mkcl_call_stack_check(env);
  {
    mkcl_setup_for_rest(env, MK_SI_concatenate_base_strings, 0, narg, narg, args);

    if (narg == 1) {
      mkcl_object arg = mkcl_va_arg(args);

      if (MKCL_BASE_STRING_P(arg))
        output = mkcl_copy_base_string(env, arg);
      else if (mkcl_Null(arg))
        output = mkcl_core.empty_base_string;
      else if (MKCL_STRINGP(arg) || MKCL_CONSP(arg) || MKCL_VECTORP(arg))
        output = mkcl_coerce_to_simple_base_string(env, arg);
      else
        mkcl_FEtype_error_sequence(env, arg);
    } else {
      mkcl_index l;
      mkcl_narg i;

      /* Compute final size and store NONEMPTY coerced strings. */
      for (i = 0, l = 0; i < narg; i++) {
        mkcl_object arg = mkcl_va_arg(args);
        mkcl_object s;

        if (MKCL_BASE_STRING_P(arg))
          s = arg;
        else if (mkcl_Null(arg))
          s = mkcl_core.empty_base_string;
        else if (MKCL_STRINGP(arg) || MKCL_CONSP(arg) || MKCL_VECTORP(arg))
          s = mkcl_coerce_to_simple_base_string(env, arg);
        else
          mkcl_FEtype_error_sequence(env, arg);

        if (s->base_string.fillp) {
          MKCL_TEMP_STACK_PUSH(env, s);
          l += s->base_string.fillp;
        }
      }
      /* Do actual copying by recovering those strings */
      output = mkcl_alloc_simple_base_string(env, l);
      while (l) {
        mkcl_object s = MKCL_TEMP_STACK_POP_UNSAFE(env);
        size_t bytes = s->base_string.fillp;
        l -= bytes;
        memcpy(output->base_string.self + l, s->base_string.self, bytes);
      }
    }
    mkcl_va_end(args);
    mkcl_return_value(output);
  }
}

mkcl_object mkcl_concatenate_2_base_strings(MKCL, mkcl_object str1, mkcl_object str2)
{
  if (mkcl_unlikely(!MKCL_BASE_STRING_P(str1))) mkcl_FEtype_error_base_string(env, str1);
  if (mkcl_unlikely(!MKCL_BASE_STRING_P(str2))) mkcl_FEtype_error_base_string(env, str2);

  mkcl_index len1 = str1->base_string.fillp;
  mkcl_index len2 = str2->base_string.fillp;

  mkcl_object new = mkcl_alloc_simple_base_string(env, len1 + len2);

  memcpy(new->base_string.self, str1->base_string.self, len1);
  memcpy(&(new->base_string.self[len1]), str2->base_string.self, len2);
  return new;
}

mkcl_object mkcl_concatenate_3_base_strings(MKCL, mkcl_object str1, mkcl_object str2, mkcl_object str3)
{
  if (mkcl_unlikely(!MKCL_BASE_STRING_P(str1))) mkcl_FEtype_error_base_string(env, str1);
  if (mkcl_unlikely(!MKCL_BASE_STRING_P(str2))) mkcl_FEtype_error_base_string(env, str2);
  if (mkcl_unlikely(!MKCL_BASE_STRING_P(str3))) mkcl_FEtype_error_base_string(env, str3);

  mkcl_index len1 = str1->base_string.fillp;
  mkcl_index len2 = str2->base_string.fillp;
  mkcl_index len3 = str3->base_string.fillp;

  mkcl_object new = mkcl_alloc_simple_base_string(env, len1 + len2 + len3);

  memcpy(new->base_string.self, str1->base_string.self, len1);
  memcpy(&(new->base_string.self[len1]), str2->base_string.self, len2);
  memcpy(&(new->base_string.self[len1 + len2]), str3->base_string.self, len3);
  return new;
}

mkcl_object mk_si_concatenate_strings(MKCL, mkcl_narg narg, ...)
{
  mkcl_object output;

  mkcl_call_stack_check(env);
  {
    mkcl_setup_for_rest(env, MK_SI_concatenate_strings, 0, narg, narg, args);

    if (narg == 1) {
      mkcl_object arg = mkcl_va_arg(args);

      if (MKCL_CHARACTER_STRING_P(arg))
        output = mkcl_copy_string(env, arg);
      else if (mkcl_Null(arg))
        output = mkcl_core.empty_string;
      else if (MKCL_BASE_STRING_P(arg) || MKCL_CONSP(arg) || MKCL_VECTORP(arg))
        output = mkcl_coerce_to_simple_character_string(env, arg);
      else
        mkcl_FEtype_error_sequence(env, arg);
    } else {
      mkcl_index l;
      mkcl_narg i;

      /* Compute final size and store NONEMPTY coerced strings. */
      for (i = 0, l = 0; i < narg; i++) {
        mkcl_object arg = mkcl_va_arg(args);
        mkcl_object s;

        if (MKCL_CHARACTER_STRING_P(arg))
          s = arg;
        else if (mkcl_Null(arg))
          s = mkcl_core.empty_string;
        else if (MKCL_BASE_STRING_P(arg) || MKCL_CONSP(arg) || MKCL_VECTORP(arg))
          s = mkcl_coerce_to_simple_character_string(env, arg);
        else
          mkcl_FEtype_error_sequence(env, arg);

        if (s->string.fillp) {
          MKCL_TEMP_STACK_PUSH(env, s);
          l += s->string.fillp;
        }
      }
      /* Do actual copying by recovering those strings */
      output = mkcl_alloc_simple_character_string(env, l);
      while (l) {
        mkcl_object s = MKCL_TEMP_STACK_POP_UNSAFE(env);
        size_t bytes = s->string.fillp;
        l -= bytes;
        (void) mkcl_wmemcpy((output->string.self + l), s->string.self, bytes);
      }
    }
    mkcl_va_end(args);
    mkcl_return_value(output);
  }
}

mkcl_object mkcl_concatenate_2_strings(MKCL, mkcl_object str1, mkcl_object str2)
{
  if (mkcl_unlikely(!MKCL_STRINGP(str1))) mkcl_FEtype_error_string(env, str1);
  if (mkcl_unlikely(!MKCL_STRINGP(str2))) mkcl_FEtype_error_string(env, str2);

  if (str1->d.t == mkcl_t_base_string) str1 = mkcl_coerce_to_simple_character_string(env, str1);
  if (str2->d.t == mkcl_t_base_string) str2 = mkcl_coerce_to_simple_character_string(env, str2);

  mkcl_index len1 = str1->string.fillp;
  mkcl_index len2 = str2->string.fillp;

  mkcl_object new = mkcl_alloc_simple_character_string(env, len1 + len2);

  (void) mkcl_wmemcpy(new->string.self, str1->string.self, len1);
  (void) mkcl_wmemcpy(&(new->string.self[len1]), str2->string.self, len2);
  return new;
}

mkcl_object mkcl_concatenate_3_strings(MKCL, mkcl_object str1, mkcl_object str2, mkcl_object str3)
{
  if (mkcl_unlikely(!MKCL_STRINGP(str1))) mkcl_FEtype_error_string(env, str1);
  if (mkcl_unlikely(!MKCL_STRINGP(str2))) mkcl_FEtype_error_string(env, str2);
  if (mkcl_unlikely(!MKCL_STRINGP(str3))) mkcl_FEtype_error_string(env, str3);

  if (str1->d.t == mkcl_t_base_string) str1 = mkcl_coerce_to_simple_character_string(env, str1);
  if (str2->d.t == mkcl_t_base_string) str2 = mkcl_coerce_to_simple_character_string(env, str2);
  if (str3->d.t == mkcl_t_base_string) str3 = mkcl_coerce_to_simple_character_string(env, str3);

  mkcl_index len1 = str1->string.fillp;
  mkcl_index len2 = str2->string.fillp;
  mkcl_index len3 = str3->string.fillp;

  mkcl_object new = mkcl_alloc_simple_character_string(env, len1 + len2 + len3);

  (void) mkcl_wmemcpy(new->string.self, str1->string.self, len1);
  (void) mkcl_wmemcpy(&(new->string.self[len1]), str2->string.self, len2);
  (void) mkcl_wmemcpy(&(new->string.self[len1 + len2]), str3->string.self, len3);
  return new;
}


mkcl_base_char * mkcl_extend_base_string(MKCL, mkcl_object s)
{
  mkcl_object other;
  mkcl_index new_length;
  const mkcl_index fillp = s->base_string.fillp;

  if (!s->base_string.adjustable)
    mkcl_FEerror(env, "string-push-extend: the string ~S is not adjustable.", 1, s);

  /* The following code expects that (2 * MKCL_ADIMLIM) fits inside a mkcl_index without overflow! JCB */
  if (s->base_string.dim >= MKCL_ADIMLIM)
    mkcl_FEerror(env, "Can't extend the string.", 0);
  new_length = 1 + s->base_string.dim + (s->base_string.dim / 2);
  if (new_length > MKCL_ADIMLIM)
    new_length = MKCL_ADIMLIM;

  other = mkcl_alloc_adjustable_base_string(env, new_length);
  if (fillp) memcpy(other->base_string.self, s->base_string.self, fillp + 1);
  other->base_string.fillp = fillp;

  s = mk_si_replace_array(env, s, other);
  return(s->base_string.self);
}

mkcl_character * mkcl_extend_string(MKCL, mkcl_object s)
{
  mkcl_object other;
  mkcl_index new_length;
  const mkcl_index fillp = s->string.fillp;

  if (!s->string.adjustable)
    mkcl_FEerror(env, "string-push-extend: the string ~S is not adjustable.", 1, s);

  /* The following code expects that (2 * MKCL_ADIMLIM) fits inside a mkcl_index without overflow! JCB */
  if (s->string.dim >= MKCL_ADIMLIM)
    mkcl_FEerror(env, "Can't extend the string.", 0);
  new_length = 1 + s->string.dim + (s->string.dim / 2);
  if (new_length > MKCL_ADIMLIM)
    new_length = MKCL_ADIMLIM;

  other = mkcl_alloc_adjustable_character_string(env, new_length);
  if (fillp) (void) mkcl_wmemcpy(other->string.self, s->string.self, fillp);
  other->string.fillp = fillp;
  s = mk_si_replace_array(env, s, other);

  return(s->string.self);  
}

union mkcl_array_data mkcl_extend_vector(MKCL, mkcl_object s)
{
  mkcl_object other;
  mkcl_index new_length;
  const mkcl_index fillp = s->vector.fillp;

  if (!s->vector.adjustable)
    mkcl_FEerror(env, "string-push-extend: the string ~S is not adjustable.", 1, s);

  /* The following code expects that (2 * MKCL_ADIMLIM) fits inside a mkcl_index without overflow! JCB */
  if (s->vector.dim >= MKCL_ADIMLIM)
    mkcl_FEerror(env, "Can't extend the string.", 0);
  new_length = 1 + s->vector.dim + (s->vector.dim / 2);
  if (new_length > MKCL_ADIMLIM)
    new_length = MKCL_ADIMLIM;

  other = mk_si_make_vector(env, mk_cl_array_element_type(env, s),
			    MKCL_MAKE_FIXNUM(new_length), mk_cl_Ct,
			    MKCL_MAKE_FIXNUM(s->vector.fillp),
			    mk_cl_Cnil, MKCL_MAKE_FIXNUM(0));
  mkcl_copy_subarray(env, other, 0, s, 0, s->vector.fillp);

  s = mk_si_replace_array(env, s, other);
  return(s->vector.self);
}


/********************************/

/* Sequence specializers for string and base-string. */

mkcl_object mkcl_base_substring(MKCL, mkcl_object str, mkcl_index start, mkcl_index end)
{
  if (mkcl_unlikely(!MKCL_BASE_STRING_P(str))) mkcl_FEtype_error_base_string(env, str);

  if (end == -1) end = str->base_string.fillp;

  if (start > str->base_string.fillp || start > end || end > str->base_string.fillp)
    mkcl_FEerror(env, "~S and ~S are invalid as :START and :END~%for the string sequence ~S.",
		 3, mkcl_make_unsigned_integer(env, start), mkcl_make_unsigned_integer(env, end), str);

  mkcl_index len = end - start;
  mkcl_object new = mkcl_alloc_simple_base_string(env, len);
  if (len)
    memcpy(new->base_string.self, &(str->base_string.self[start]), len);
  return new;
}

mkcl_object mkcl_substring(MKCL, mkcl_object str, mkcl_index start, mkcl_index end)
{
  mkcl_index len;
  mkcl_object new;

  if (mkcl_unlikely(!MKCL_STRINGP(str))) mkcl_FEtype_error_string(env, str);

  if (end == -1) end = str->base_string.fillp;

  if (start > str->base_string.fillp || start > end || end > str->base_string.fillp)
    mkcl_FEerror(env, "~S and ~S are invalid as :START and :END~%for the string sequence ~S.",
		 3, mkcl_make_unsigned_integer(env, start), mkcl_make_unsigned_integer(env, end), str);

  len = end - start;
  if (str->string.t == mkcl_t_string)
    {
      new = mkcl_alloc_simple_character_string(env, len);
      if (len)
	memcpy(new->string.self, &(str->string.self[start]), len * sizeof(new->string.self[0]));
    }
  else
    {
      new = mkcl_alloc_simple_base_string(env, len);
      if (len)
	memcpy(new->base_string.self, &(str->base_string.self[start]), len * sizeof(new->base_string.self[0]));
    }

  return new;
}


mkcl_object
mkcl_fill_base_string(MKCL, mkcl_object str, mkcl_base_char ch)
{
  mkcl_index start = 0;
  mkcl_index end;

  if (mkcl_unlikely(!MKCL_BASE_STRING_P(str))) mkcl_FEtype_error_base_string(env, str);

  end = str->base_string.fillp;

  if (end)
    memset(&(str->base_string.self[start]), ch, end);
  return str;
}

mkcl_object
mkcl_fill_base_string_k(MKCL, mkcl_object str, mkcl_base_char ch, mkcl_index start, mkcl_index end)
{
  if (mkcl_unlikely(!MKCL_BASE_STRING_P(str))) mkcl_FEtype_error_base_string(env, str);

  if (end == -1) end = str->base_string.fillp;

  if (start > str->base_string.fillp || start > end || end > str->base_string.fillp)
    mkcl_FEerror(env, "~S and ~S are invalid as :START and :END~%for the string sequence ~S.",
		 3, mkcl_make_unsigned_integer(env, start), mkcl_make_unsigned_integer(env, end), str);

  if (end)
    memset(&(str->base_string.self[start]), ch, end);
  return str;
}

mkcl_object
mkcl_fill_string(MKCL, mkcl_object str, mkcl_character ch)
{
  mkcl_index start = 0;
  mkcl_index end;

  if (mkcl_unlikely(!MKCL_STRINGP(str))) mkcl_FEtype_error_string(env, str);

  if (str->string.t == mkcl_t_string)
    {
      end = str->string.fillp;
      if (end)
	(void) mkcl_wmemset(&(str->string.self[start]), ch, end);
    }
  else
    {
      end = str->base_string.fillp;
      if (!MKCL_BASE_CHAR_CODE_P(ch))
	mkcl_FEerror(env, "Cannot insert character ~A into a base-string", 1, MKCL_CODE_CHAR(ch));
      if (end)
	memset(&(str->base_string.self[start]), ch, end);
    }

  return str;
}

mkcl_object
mkcl_fill_string_k(MKCL, mkcl_object str, mkcl_character ch, mkcl_index start, mkcl_index end)
{
  if (mkcl_unlikely(!MKCL_STRINGP(str))) mkcl_FEtype_error_string(env, str);

  if (end == -1) end = str->string.fillp;

  if (start > str->string.fillp || start > end || end > str->string.fillp)
    mkcl_FEerror(env, "~S and ~S are invalid as :START and :END~%for the string sequence ~S.",
		 3, mkcl_make_unsigned_integer(env, start), mkcl_make_unsigned_integer(env, end), str);

  if (str->string.t == mkcl_t_string)
    {
      if (end)
	(void) mkcl_wmemset(&(str->string.self[start]), ch, end);
    }
  else
    {
      end = str->base_string.fillp;
      if (!MKCL_BASE_CHAR_CODE_P(ch))
	mkcl_FEerror(env, "Cannot insert character ~A into a base-string", 1, MKCL_CODE_CHAR(ch));
      if (end)
	memset(&(str->base_string.self[start]), ch, end);
    }

  return str;
}


mkcl_object
mkcl_search_in_base_string(MKCL, mkcl_object str1, mkcl_object str2)
{
  if (mkcl_unlikely(!MKCL_BASE_STRING_P(str1))) mkcl_FEtype_error_base_string(env, str1);
  if (mkcl_unlikely(!MKCL_BASE_STRING_P(str2))) mkcl_FEtype_error_base_string(env, str2);
  
  const mkcl_index start1 = 0;
  const mkcl_index end1 = str1->base_string.fillp;
  mkcl_index start2 = 0;
  const mkcl_index end2 = str2->base_string.fillp;
  const mkcl_base_char * const self1 = str1->base_string.self;
  const mkcl_base_char * const self2 = str2->base_string.self;

  const mkcl_index len1 = end1 - start1;
  const mkcl_base_char ch0 = self1[start1];

  if (len1 == 0) return MKCL_MAKE_FIXNUM(start2);

  while (start2 + len1 <= end2)
    {
      mkcl_base_char * base = memchr(&(self2[start2]), ch0, end2 - start2);

      if (base && ((start2 = base - self2) + len1 <= end2))
	{
	  bool match = TRUE;
	  mkcl_index j = 0;
	  const mkcl_base_char * sub_self2 = self2 + ++start2;

	  if (0 == memcmp(self1 + 1, sub_self2, len1 - 1))
	    return MKCL_MAKE_FIXNUM(start2 - 1);
	}
      else
	return mk_cl_Cnil;
    }
  return mk_cl_Cnil;
}


mkcl_object
mkcl_search_in_string(MKCL, mkcl_object str1, mkcl_object str2)
{
  if (mkcl_unlikely(!MKCL_STRINGP(str1))) mkcl_FEtype_error_string(env, str1);
  if (mkcl_unlikely(!MKCL_STRINGP(str2))) mkcl_FEtype_error_string(env, str2);

  if (str2->string.t == mkcl_t_base_string)
    {
      if (str1->string.t == mkcl_t_string)
	str1 = mkcl_coerce_to_base_string(env, str1);
      return mkcl_search_in_base_string(env, str1, str2);
    }

  if (str1->string.t == mkcl_t_base_string)
    str1 = mk_si_coerce_to_character_string(env, str1);
  
  const mkcl_index start1 = 0;
  const mkcl_index end1 = str1->string.fillp;
  mkcl_index start2 = 0;
  const mkcl_index end2 = str2->string.fillp;
  const mkcl_character * const self1 = str1->string.self;
  const mkcl_character * const self2 = str2->string.self;

  const mkcl_index len1 = end1 - start1;
  const mkcl_character ch0 = self1[start1];

  if (len1 == 0) return MKCL_MAKE_FIXNUM(start2);

  while (start2 + len1 <= end2)
    {
      mkcl_character * base = mkcl_wmemchr(&(self2[start2]), ch0, end2 - start2);

      if (base && ((start2 = base - self2) + len1 <= end2))
	{
	  bool match = TRUE;
	  mkcl_index j = 0;
	  const mkcl_character * sub_self2 = self2 + ++start2;

	  if (0 == mkcl_wmemcmp((self1 + 1), sub_self2, len1 - 1))
	    return MKCL_MAKE_FIXNUM(start2 - 1);
	}
      else
	return mk_cl_Cnil;
    }
  return mk_cl_Cnil;
}


mkcl_object mkcl_replace_in_base_string(MKCL, mkcl_object str1, mkcl_object str2)
{
  const mkcl_index start1 = 0;
  const mkcl_index start2 = 0;

  if (mkcl_unlikely(!MKCL_BASE_STRING_P(str1))) mkcl_FEtype_error_base_string(env, str1);
  if (mkcl_unlikely(!MKCL_BASE_STRING_P(str2))) mkcl_FEtype_error_base_string(env, str2);

  const mkcl_index end1 = str1->base_string.fillp;
  const mkcl_index end2 = str2->base_string.fillp;
  const mkcl_index len1 = end1 - start1;
  const mkcl_index len2 = end2 - start2;

  if (len1 != 0 && len2 != 0)
    {
      if (len1 < len2)
	memmove(&(str1->base_string.self[start1]), &(str2->base_string.self[start2]), len1);
      else
	memmove(&(str1->base_string.self[start1]), &(str2->base_string.self[start2]), len2);
    }

  return str1;
}

mkcl_object mkcl_replace_in_base_string_k(MKCL, mkcl_object str1, mkcl_object str2,
					  mkcl_index start1, mkcl_index end1,
					  mkcl_index start2, mkcl_index end2)
{
  if (mkcl_unlikely(!MKCL_BASE_STRING_P(str1))) mkcl_FEtype_error_base_string(env, str1);
  if (mkcl_unlikely(!MKCL_BASE_STRING_P(str2))) mkcl_FEtype_error_base_string(env, str2);

  if (end1 == -1) end1 = str1->base_string.fillp;

  if (start1 > str1->base_string.fillp || start1 > end1 || end1 > str1->base_string.fillp)
    mkcl_FEerror(env, "~S and ~S are invalid as :START and :END~%for the string sequence ~S.",
		 3, mkcl_make_unsigned_integer(env, start1), mkcl_make_unsigned_integer(env, end1), str1);

  if (end2 == -1) end2 = str2->base_string.fillp;

  if (start2 > str2->base_string.fillp || start2 > end2 || end2 > str2->base_string.fillp)
    mkcl_FEerror(env, "~S and ~S are invalid as :START and :END~%for the string sequence ~S.",
		 3, mkcl_make_unsigned_integer(env, start2), mkcl_make_unsigned_integer(env, end2), str2);

  const mkcl_index len1 = end1 - start1;
  const mkcl_index len2 = end2 - start2;

  if (len1 != 0 && len2 != 0)
    {
      if (len1 < len2)
	memmove(&(str1->base_string.self[start1]), &(str2->base_string.self[start2]), len1);
      else
	memmove(&(str1->base_string.self[start1]), &(str2->base_string.self[start2]), len2);
    }

  return str1;
}

mkcl_object mkcl_replace_in_string(MKCL, mkcl_object str1, mkcl_object str2)
{
  const mkcl_index start1 = 0;
  const mkcl_index start2 = 0;

  if (mkcl_unlikely(!MKCL_STRINGP(str1))) mkcl_FEtype_error_string(env, str1);
  if (mkcl_unlikely(!MKCL_STRINGP(str2))) mkcl_FEtype_error_string(env, str2);

  if (str1->string.t != str2->string.t)
    {
      if (str1->string.t == mkcl_t_string)
	str2 = mk_si_coerce_to_character_string(env, str2);
      else
	str2 = mkcl_coerce_to_base_string(env, str2);
    }

  const mkcl_index end1 = str1->string.fillp;
  const mkcl_index end2 = str2->string.fillp;
  const mkcl_index len1 = end1 - start1;
  const mkcl_index len2 = end2 - start2;

  if (len1 != 0 && len2 != 0)
    {
      if (len1 < len2)
	{
	  if (str1->string.t == mkcl_t_string)
	    (void) mkcl_wmemmove(&(str1->string.self[start1]), &(str2->string.self[start2]), len1);
	  else
	    memmove(&(str1->base_string.self[start1]), &(str2->base_string.self[start2]), len1);
	}
      else
	{
	  if (str1->string.t == mkcl_t_string)
	    (void) mkcl_wmemmove(&(str1->string.self[start1]), &(str2->string.self[start2]), len2);
	  else
	    memmove(&(str1->base_string.self[start1]), &(str2->base_string.self[start2]), len2);
	}
    }

  return str1;
}

mkcl_object mkcl_replace_in_string_k(MKCL, mkcl_object str1, mkcl_object str2,
				     mkcl_index start1, mkcl_index end1,
				     mkcl_index start2, mkcl_index end2)
{
  if (mkcl_unlikely(!MKCL_STRINGP(str1))) mkcl_FEtype_error_string(env, str1);
  if (mkcl_unlikely(!MKCL_STRINGP(str2))) mkcl_FEtype_error_string(env, str2);

  if (str1->string.t != str2->string.t)
    {
      if (str1->string.t == mkcl_t_string)
	str2 = mk_si_coerce_to_character_string(env, str2);
      else
	str2 = mkcl_coerce_to_base_string(env, str2);
    }

  if (end1 == -1) end1 = str1->string.fillp;

  if (start1 > str1->string.fillp || start1 > end1 || end1 > str1->string.fillp)
    mkcl_FEerror(env, "~S and ~S are invalid as :START and :END~%for the string sequence ~S.",
		 3, mkcl_make_unsigned_integer(env, start1), mkcl_make_unsigned_integer(env, end1), str1);

  if (end2 == -1) end2 = str2->string.fillp;

  if (start2 > str2->string.fillp || start2 > end2 || end2 > str2->string.fillp)
    mkcl_FEerror(env, "~S and ~S are invalid as :START and :END~%for the string sequence ~S.",
		 3, mkcl_make_unsigned_integer(env, start2), mkcl_make_unsigned_integer(env, end2), str2);

  const mkcl_index len1 = end1 - start1;
  const mkcl_index len2 = end2 - start2;

  if (len1 != 0 && len2 != 0)
    {
      if (len1 < len2)
	{
	  if (str1->string.t == mkcl_t_string)
	    (void) mkcl_wmemmove(&(str1->string.self[start1]), &(str2->string.self[start2]), len1);
	  else
	    memmove(&(str1->base_string.self[start1]), &(str2->base_string.self[start2]), len1);
	}
      else
	{
	  if (str1->string.t == mkcl_t_string)
	    (void) mkcl_wmemmove(&(str1->string.self[start1]), &(str2->string.self[start2]), len2);
	  else
	    memmove(&(str1->base_string.self[start1]), &(str2->base_string.self[start2]), len2);
	}
    }
  
  return str1;
}


/* **************************** */

/* UTF-8 and UTF-16 */


static mkcl_index
utf_8_decoder(MKCL, unsigned char * in, mkcl_index max, mkcl_character * out, bool * invalid)
{
  /* In understanding this code:
   * 0x8 = 1000, 0xC = 1100, 0xE = 1110, 0xF = 1111
   * 0x1 = 0001, 0x3 = 0011, 0x7 = 0111, 0xF = 1111
   */
  mkcl_character cum = 0;
#if 0
  unsigned char buffer[5];
#endif
  mkcl_index nbytes, i = 0;

  unsigned char ch;

  if (i >= max)
    return (*out = cum, i);

  ch = in[i]; i++;

  if ((ch & 0x80) == 0) /* This was an ASCII char. */
    return (*out = ch, i);

  if ((ch & 0x40) == 0)
    return (*out = 0xFFFD, *invalid = TRUE, i); /*  malformed character */

  
  if ((ch & 0x20) == 0) {
    cum = ch & 0x1F;
    nbytes = 2;
  } else if ((ch & 0x10) == 0) {
    cum = ch & 0x0F;
    nbytes = 3;
  } else if ((ch & 0x08) == 0) {
    cum = ch & 0x07;
    nbytes = 4;
  } else {
    return (*out = 0xFFFD, *invalid = TRUE, i); /* unsupported character */
  }

  if (nbytes > max) /* Will overflow */
    {
      if (i < max)
	for (ch = in[i++]; ((ch & 0xC0) == 0x80) && i < max; ch = in[i++]); /* swallow tail bytes */
      return (*out = 0xFFFD, *invalid = TRUE, i); /*  malformed character */
    }

#if 0
  for (; i < nbytes; i++) {
    ch = in[i];
    /*printf(": %04x :", c);*/
    if ((ch & 0xC0) != 0x80) /* Not a valid tail byte. */
      return (*out = 0xFFFD, *invalid = TRUE, i); /*  malformed character */
    cum = (cum << 6) | (ch & 0x3F);
  }
#else
  switch (nbytes)
    {
    case 2:
      {
	unsigned char c = in[1];

	if ((c & 0xC0) != 0x80) /* not continuation byte? */
	  return (*out = 0xFFFD, *invalid = TRUE, nbytes);
	if (cum == 0)  /* illegal overlong sequence? */
	  return (*out = 0xFFFD, *invalid = TRUE, nbytes);
	cum = (cum << 6) | (c & 0x3F);
      }
      break;
    case 3:
      {
	unsigned char c1 = in[1];
	unsigned char c2 = in[2];

	if ((c1 & 0xC0) != 0x80) /* not continuation byte? */
	  return (*out = 0xFFFD, *invalid = TRUE, (nbytes - 1));
	if ((c2 & 0xC0) != 0x80) /* not continuation byte? */
	  return (*out = 0xFFFD, *invalid = TRUE, nbytes);
	if ((cum == 0) && ((c1 & 0x20) == 0))  /* illegal overlong sequence? */
	  return (*out = 0xFFFD, *invalid = TRUE, nbytes);
	cum = (cum << 12) | ((c1 & 0x3F) << 6) | (c2 & 0x3F);
      }
      break;
    case 4:
      {
	unsigned char c1 = in[1];
	unsigned char c2 = in[2];
	unsigned char c3 = in[3];

	if ((c1 & 0xC0) != 0x80) /* not continuation byte? */
	  return (*out = 0xFFFD, *invalid = TRUE, (nbytes - 2));
	if ((c2 & 0xC0) != 0x80) /* not continuation byte? */
	  return (*out = 0xFFFD, *invalid = TRUE, (nbytes - 1));
	if ((c3 & 0xC0) != 0x80) /* not continuation byte? */
	  return (*out = 0xFFFD, *invalid = TRUE, nbytes);
	if ((cum == 0) && ((c1 & 0x30) == 0))  /* illegal overlong sequence? */
	  return (*out = 0xFFFD, *invalid = TRUE, nbytes);
	cum = (cum << 18) | ((c1 & 0x3F) << 12) | ((c2 & 0x3F) << 6) | (c3 & 0x3F);
      }
      break;
    }
#endif
  if (cum == 0) return (*out = 0xFFFD, *invalid = TRUE, nbytes);
  if (cum >= 0xd800) {
    if (cum <= 0xdfff)
      return (*out = 0xFFFD, *invalid = TRUE, nbytes); /* invalid codepoint */
    if (cum >= 0xFFFE && cum <= 0xFFFF)
      return (*out = 0xFFFD, *invalid = TRUE, nbytes); /* invalid codepoint */
  }
  /*printf("; %04x ;", cum);*/
  return (*out = cum, nbytes);
}


static inline mkcl_index
utf_8_encoder(mkcl_char8 *buffer, mkcl_character c, bool * invalid)
{
  if (c <= 0x7F) {
    buffer[0] = c;
    return 1;
  } else if (c <= 0x7ff) {
    buffer[1] = (c & 0x3f) | 0x80; c >>= 6;
    buffer[0] = c | 0xC0;
    return 2;
  } else if (c <= 0xD7FF || (c > 0xDFFF && c < 0xFFFE)) {
    /* This here would have happily produced invalid UTF-8 for UFFF? or UD8??-UDF?? */
    buffer[2] = (c & 0x3f) | 0x80; c >>= 6;
    buffer[1] = (c & 0x3f) | 0x80; c >>= 6;
    buffer[0] = c | 0xE0;
    return 3;
  } else if (c <= 0xFFFF) { /* This must be an invalid 16-bits codepoint. */
    buffer[0] = 0xEF; buffer[1] = 0xBF; buffer[2] = 0xBD; /* This is UFFFD in UTF-8. */
    *invalid = TRUE;
    return 3;
  } else if (c <= 0x10FFFF) {
    buffer[3] = (c & 0x3f) | 0x80; c >>= 6;
    buffer[2] = (c & 0x3f) | 0x80; c >>= 6;
    buffer[1] = (c & 0x3f) | 0x80; c >>= 6;
    buffer[0] = c | 0xF0;
    return 4;
  } else { /* This must be an invalid codepoint. */
    buffer[0] = 0xEF; buffer[1] = 0xBF; buffer[2] = 0xBD; /* This is UFFFD in UTF-8. */
    *invalid = TRUE;
    return 3;
  } 
}

static inline mkcl_index
utf_8_size(mkcl_character c)
{
  if (c <= 0x7F) {
    return 1;
  } else if (c <= 0x7ff) {
    return 2;
  } else if (c <= 0xFFFF) {
    return 3;
  } else if (c <= 0x10FFFFL) {
    return 4;
  } else {
    return 3;
  }
}

#define UTF_8_MAX_PER_CHARACTER 4
#define UTF_8_MAX_PER_BASE_CHAR 2

void mkcl_fill_utf_8_from_string(MKCL, mkcl_object utf_8, mkcl_index prefix_size, mkcl_object str)
{
  bool invalid = FALSE;
  mkcl_index i;
  mkcl_index utf_8_dim = utf_8->UTF_8.dim;
  mkcl_index j = prefix_size;
  mkcl_index str_len = str->string.fillp;
  mkcl_character * str_self = str->string.self;
  unsigned char * utf_8_self = utf_8->UTF_8.self;

  for (i = 0; (i < str_len) && ((j + UTF_8_MAX_PER_CHARACTER) < utf_8_dim); i++)
    j += utf_8_encoder(utf_8_self + j, str_self[i], &invalid);

  for (; (i < str_len) && ((j + utf_8_size(str_self[i])) < utf_8_dim); i++)
    j += utf_8_encoder(utf_8_self + j, str_self[i], &invalid);

  if (i == str_len)
    {
      utf_8_self[j] = '\0'; /* needed by C! */
      utf_8->UTF_8.fillp = j;
    }
  else
    { /* overflow */
      mkcl_index old_i = i, old_j = j;
      /* mkcl_index old_utf_8_dim = utf_8_dim; */

      utf_8_dim = j;
      for (; i < str_len; i++)
	utf_8_dim += utf_8_size(str_self[i]);
  
      unsigned char * self = mkcl_alloc_atomic(env, utf_8_dim + 1);

      for (j = 0; j < old_j; j++) /* copy the part already encoded before the overflow */
	self[j] = utf_8_self[j];

      for (i = old_i; i < str_len; i++) /* encode the rest */
	j += utf_8_encoder(self + j, str_self[i], &invalid);

      self[j] = '\0'; /* needed by C! */
      utf_8->UTF_8.self = self;
      utf_8->UTF_8.fillp = utf_8->UTF_8.dim = utf_8_dim;
    }
}

void mkcl_fill_utf_8_from_base_string(MKCL, mkcl_object utf_8, mkcl_index prefix_size, mkcl_object str)
{
  bool invalid = FALSE;
  mkcl_index i;
  mkcl_index utf_8_dim = utf_8->UTF_8.dim;
  mkcl_index j = prefix_size;
  mkcl_index str_len = str->base_string.fillp;
  mkcl_base_char * str_self = str->base_string.self;
  unsigned char * utf_8_self = utf_8->UTF_8.self;

  for (i = 0; (i < str_len) && ((j + UTF_8_MAX_PER_BASE_CHAR) < utf_8_dim); i++)
    j += utf_8_encoder(utf_8_self + j, str_self[i], &invalid);

  for (; (i < str_len) && ((j + utf_8_size(str_self[i])) < utf_8_dim); i++)
    j += utf_8_encoder(utf_8_self + j, str_self[i], &invalid);

  if (i == str_len)
    {
      utf_8_self[j] = '\0'; /* needed by C! */
      utf_8->UTF_8.fillp = j;
    }
  else
    { /* overflow */
      mkcl_index old_i = i, old_j = j;
      /* mkcl_index old_utf_8_dim = utf_8_dim; */

      utf_8_dim = j;
      for (; i < str_len; i++)
	utf_8_dim += utf_8_size(str_self[i]);
  
      unsigned char * self = mkcl_alloc_atomic(env, utf_8_dim + 1);

      for (j = 0; j < old_j; j++) /* copy the part already encoded before the overflow */
	self[j] = utf_8_self[j];

      for (i = old_i; i < str_len; i++) /* encode the rest */
	j += utf_8_encoder(self + j, str_self[i], &invalid);

      self[j] = '\0'; /* needed by C! */
      utf_8->UTF_8.self = self;
      utf_8->UTF_8.fillp = utf_8->UTF_8.dim = utf_8_dim;
    }
}

mkcl_object mkcl_fill_base_string_from_string(MKCL, mkcl_object bstr, mkcl_index prefix_size, mkcl_object str)
{
  mkcl_index index, length = str->string.fillp;
  mkcl_base_char * bstr_self = bstr->base_string.self + prefix_size;
  mkcl_character * str_self = str->string.self;
  mkcl_character ch;

  if (mkcl_unlikely((bstr->base_string.dim - prefix_size) < length))
    length = bstr->base_string.dim - prefix_size;

  for (index = 0; index < length; index++)
    if ((ch = str_self[index]) > 0xFF)
      bstr_self[index] = 0xBF;
    else
      bstr_self[index] = ch;

  bstr_self[index] = '\0'; /* needed by C! */
  bstr->base_string.fillp = prefix_size + index;
  return bstr;
}

#if 0
int printf(const char *format, ...);
#endif

mkcl_object mk_si_utf_8(MKCL, mkcl_object string)
{
  mkcl_index i = 0;
  mkcl_index j = 0;

  mkcl_call_stack_check(env);
  for (;;)
    switch(mkcl_type_of(string)) {
    case mkcl_t_string:
      {
        mkcl_index utf_8_dim = 0;
	mkcl_index str_len = string->string.fillp;
	mkcl_character * str = string->string.self;
        bool invalid = FALSE;

	for (i = 0; i < str_len; i++)
	  utf_8_dim += utf_8_size(str[i]);
  
	unsigned char * self = mkcl_alloc_atomic(env, utf_8_dim + 1);

	for (i = 0, j = 0; i < str_len; i++)
	  j += utf_8_encoder(self + j, str[i], &invalid);
	self[j] = '\0'; /* needed by C! */

#if 0
	if (j != utf_8_dim) printf("\nIn mk_si_utf_8(): alloc botch: j = %lu and should be %lu.\n", j, utf_8_dim); /* debug! JCB */
#endif

	mkcl_object utf_8 = mkcl_alloc_raw_utf_8(env);
      
	utf_8->UTF_8.fillp = utf_8->UTF_8.dim = utf_8_dim;
	utf_8->UTF_8.self = self;

	mkcl_return_2_values(utf_8, (invalid ? mk_cl_Ct : mk_cl_Cnil));
      }
    case mkcl_t_base_string:
      {
        mkcl_index utf_8_dim = 0;
	mkcl_index str_len = string->base_string.fillp;
	mkcl_base_char * str = string->base_string.self;
        bool invalid = FALSE;

	for (i = 0; i < str_len; i++)
	  utf_8_dim += utf_8_size(str[i]);
  
	unsigned char * self = mkcl_alloc_atomic(env, utf_8_dim + 1);

	for (i = 0, j = 0; i < str_len; i++)
	  j += utf_8_encoder(self + j, str[i], &invalid);
	self[j] = '\0'; /* needed by C! */

#if 0
	if (j != utf_8_dim) printf("\nIn mk_si_utf_8(): alloc botch: j = %lu and should be %lu.\n", j, utf_8_dim); /* debug! JCB */
#endif

	mkcl_object utf_8 = mkcl_alloc_raw_utf_8(env);
      
	utf_8->UTF_8.fillp = utf_8->UTF_8.dim = utf_8_dim;
	utf_8->UTF_8.self = self;

	mkcl_return_2_values(utf_8, (invalid ? mk_cl_Ct : mk_cl_Cnil));
      }
    case mkcl_t_UTF_16:
      {
        mkcl_index utf_8_dim = 0;
	mkcl_index nb_char = 0;
	mkcl_index str_len = string->UTF_16.fillp;
	bool utf_8_invalid = FALSE;
	bool utf_16_invalid = FALSE;

	for (i = 0, nb_char = 0; i < str_len; nb_char++)
	  utf_8_dim += utf_8_size(mkcl_utf_16_char(env, string, i, &i, &utf_16_invalid));

	mkcl_char8 * self = mkcl_alloc_atomic(env, sizeof(mkcl_char8) * (utf_8_dim + 1));

	for (i = 0, j = 0; i < str_len; i++)
	  j += utf_8_encoder(self + j, mkcl_utf_16_char(env, string, i, &i, &utf_16_invalid), &utf_8_invalid);
	self[j] = 0; /* needed by C! */

	mkcl_object utf_8 = mkcl_alloc_raw_utf_8(env);
      
	utf_8->UTF_8.fillp = utf_8->UTF_8.dim = utf_8_dim;
	utf_8->UTF_8.self = self;
	
	mkcl_return_2_values(utf_8, ((utf_8_invalid || utf_16_invalid) ? mk_cl_Ct : mk_cl_Cnil));
      }
      break;
    case mkcl_t_UTF_8:
      { mkcl_return_2_values(string, mk_cl_Cnil); }
      break;
    case mkcl_t_vector:
      switch (string->vector.elttype)
	{
	case mkcl_aet_b8:
	  {
	    mkcl_index len = string->vector.fillp;
	    mkcl_base_char * str = string->vector.self.b8;
	    
	    unsigned char * self = mkcl_alloc_atomic(env, len + 1);
	    
	    mkcl_object utf_8 = mkcl_alloc_raw_utf_8(env);

	    memcpy(self, str, len);
	    utf_8->UTF_8.fillp = utf_8->UTF_8.dim = len;
	    utf_8->UTF_8.self = self;
	    
	    mkcl_return_2_values(utf_8, mk_cl_Cnil);
	  }
	  break;
	default:
	  string = mkcl_type_error(env, MK_SI_utf_8, "", string, mk_cl_list(env, 3, MK_CL_or, MK_CL_string, MK_MKCL_octets));
	  break;
	}
      break;
    default:
      string = mkcl_type_error(env, MK_SI_utf_8, "", string, MK_CL_string);
      break;
    }
}

mkcl_object mk_si_utf_8_p(MKCL, mkcl_object utf_8)
{
  mkcl_return_value((MKCL_UTF_8_P(utf_8) ? mk_cl_Ct : mk_cl_Cnil));
}

mkcl_object mk_si_utf_8_length(MKCL, mkcl_object utf_8)
{
  mkcl_call_stack_check(env);
  while(!MKCL_UTF_8_P(utf_8))
    utf_8 = mkcl_type_error(env, MK_SI_utf_8_length, "", utf_8, MK_SI_utf_8);

  mkcl_return_value(MKCL_MAKE_FIXNUM(utf_8->UTF_8.fillp));
}

mkcl_object mk_si_utf_8_as_is(MKCL, mkcl_object string)
{ /* This function seems to be of dubious value after all. JCB */
  mkcl_index i;

  mkcl_call_stack_check(env);
  for (;;)
    switch(mkcl_type_of(string)) {
    case mkcl_t_string:
      {
	mkcl_index str_len = string->string.fillp;
	mkcl_character * str = string->string.self;
	mkcl_object utf_8 = mkcl_alloc_utf_8(env, str_len);
	unsigned char * self = utf_8->UTF_8.self;

	for (i = 0; i < str_len; i++)
	  if ((self[i] = str[i]) > 0x7F) self[i] = 0x3F;
	self[i] = '\0';
	utf_8->UTF_8.fillp = i;
	mkcl_return_value(utf_8);
      }
    case mkcl_t_base_string:
      {
	mkcl_index str_len = string->base_string.fillp;
	mkcl_base_char * str = string->base_string.self;
	mkcl_object utf_8 = mkcl_alloc_utf_8(env, str_len);
	unsigned char * self = utf_8->UTF_8.self;

	for (i = 0; i < str_len; i++)
	  if ((self[i] = str[i]) > 0x7F) self[i] = 0x3F;
	self[i] = '\0';
	utf_8->UTF_8.fillp = i;
	mkcl_return_value(utf_8);
      }
    default:
      string = mkcl_type_error(env, MK_SI_utf_8_as_is, "", string, MK_CL_string);
      break;
    }
}

mkcl_object mkcl_utf_8_to_string(MKCL, mkcl_object utf_8)
{
  bool invalid = FALSE;
  mkcl_index dim = utf_8->UTF_8.fillp;
  unsigned char * self = utf_8->UTF_8.self;
  mkcl_index fillp = 0;
  mkcl_index i;

  {
#if 0
    mkcl_character data[dim + 1]; /* Yes, it's a VLA! */
#else
    mkcl_VLA(env, mkcl_character, data, dim + 1);
#endif

    for (i = 0; i < dim; fillp++)
      i += utf_8_decoder(env, &self[i], dim - i, &data[fillp], &invalid);
    data[fillp] = '\0';
 
    mkcl_object new = mkcl_alloc_adjustable_character_string(env, fillp);
    if (fillp)
      (void) mkcl_wmemcpy(new->string.self, data, fillp + 1);
    new->string.fillp = fillp;
    mkcl_return_2_values(new, (invalid ? mk_cl_Ct : mk_cl_Cnil));
  }
}

mkcl_object mkcl_utf_8_to_base_string(MKCL, mkcl_object utf_8)
{
  bool invalid = FALSE;
  mkcl_index dim = utf_8->UTF_8.fillp;
  unsigned char * self = utf_8->UTF_8.self;
  mkcl_index fillp = 0;
  mkcl_index i;

  {
    mkcl_character ch;
#if 0
    mkcl_base_char data[dim + 1]; /* Yes, it's a VLA! */
#else
    mkcl_VLA(env, mkcl_base_char, data, dim + 1);
#endif

    for (i = 0; i < dim; fillp++)
      {
	i += utf_8_decoder(env, &self[i], dim - i, &ch, &invalid);
	if (ch >= 0x0100)
	  { data[fillp] = 0x0BF; invalid = TRUE; }
	else
	  data[fillp] = ch;
      }
    data[fillp] = '\0';
 
    mkcl_object new = mkcl_alloc_simple_base_string(env, fillp);
    if (fillp)
      memcpy(new->base_string.self, data, fillp + 1);
    mkcl_return_2_values(new, (invalid ? mk_cl_Ct : mk_cl_Cnil));
  }
}

mkcl_character mkcl_utf_8_char(MKCL, mkcl_object utf_8, mkcl_index index, mkcl_index * next, bool * invalid)
{
  mkcl_character ch;
  mkcl_index size = utf_8_decoder(env, &(utf_8->UTF_8.self[index]), utf_8->UTF_8.fillp - index, &ch, invalid);
  
  *next = index + size;
  return ch;
}

struct mkcl_cfun mk_si_utf_8_char_cfunobj = MKCL_CFUN2(mk_si_utf_8_char, MK_SI_utf_8_char);

mkcl_object mk_si_utf_8_char(MKCL, mkcl_object utf_8, mkcl_object index_fix)
{
  mkcl_index next, index;
  bool invalid = FALSE;

  mkcl_call_stack_check(env);
  while(!MKCL_UTF_8_P(utf_8))
    utf_8 = mkcl_type_error(env, MK_SI_utf_8_char, "", utf_8, MK_SI_utf_8);

  mkcl_index dim = utf_8->UTF_8.fillp;

  while(!(MKCL_FIXNUMP(index_fix) && ((index = mkcl_fixnum_to_word(index_fix)) < dim)))
    index_fix = mkcl_out_of_bounds_error(env, MK_SI_utf_8_char, "index", index_fix, 0, dim);

  mkcl_return_3_values(MKCL_CODE_CHAR(mkcl_utf_8_char(env, utf_8, index, &next, &invalid)),
                       ((next < dim) ? MKCL_MAKE_FIXNUM(next) : mk_cl_Cnil),
                       (invalid ? mk_cl_Ct : mk_cl_Cnil));
}

mkcl_object mk_si_utf_8Plus(MKCL, mkcl_narg narg, ...)
{
  mkcl_index len;
  int i;
  mkcl_object output;

  mkcl_call_stack_check(env);
  {
    mkcl_setup_for_rest(env, MK_SI_utf_8P, 0, narg, narg, args);

    if (narg == 1) {
      mkcl_object s = mkcl_va_arg(args);
      
      if (MKCL_UTF_8_P(s))
        {
          mkcl_index dim = s->UTF_8.fillp;
	  
          output = mkcl_alloc_utf_8(env, dim);
          if (dim)
            memcpy(output->UTF_8.self, s->UTF_8.self, dim);
          output->UTF_8.self[dim] = '\0';
          output->UTF_8.fillp = dim;
        }
      else
        output = mk_si_utf_8(env, s);
    } else {
      /* Compute final size and store NONEMPTY coerced strings. */
      len = 0;
      for (i = 0; i < narg; i++) {
        mkcl_object arg = mkcl_va_arg(args);
        mkcl_object s = (MKCL_UTF_8_P(arg) ? arg : mk_si_utf_8(env, arg));
        mkcl_index dim = s->UTF_8.fillp;
        if (dim) {
          MKCL_TEMP_STACK_PUSH(env, s);
          len += dim;
        }
      }
      /* Do actual copying by recovering those strings */
      output = mkcl_alloc_utf_8(env, len);
      output->UTF_8.self[len] = '\0'; /* string terminator for C. */
      output->UTF_8.fillp = len;
      while (len > 0) {
        mkcl_object s = MKCL_TEMP_STACK_POP_UNSAFE(env);
        size_t bytes = s->UTF_8.fillp;
        len -= bytes;
        memcpy((output->UTF_8.self + len), s->UTF_8.self, bytes);
      }
    }
    mkcl_va_end(args);
    mkcl_return_value(output);
  }
}

struct mkcl_cfun mk_si_utf_8E_cfunobj = MKCL_CFUN2(mk_si_utf_8E, MK_SI_utf_8E);

mkcl_object mk_si_utf_8E(MKCL, mkcl_object u1, mkcl_object u2)
{
  mkcl_call_stack_check(env);
  while(!MKCL_UTF_8_P(u1))
    u1 = mkcl_type_error(env, MK_SI_utf_8E, "", u1, MK_SI_utf_8);

  while(!MKCL_UTF_8_P(u2))
    u2 = mkcl_type_error(env, MK_SI_utf_8E, "", u2, MK_SI_utf_8);

  mkcl_index dim = u1->UTF_8.fillp;

  if (dim != u2->UTF_8.fillp)
    { mkcl_return_value(mk_cl_Cnil); }
  else
    { mkcl_return_value((0 == memcmp(u1->UTF_8.self, u2->UTF_8.self, dim) ? mk_cl_Ct : mk_cl_Cnil)); }
}

/*******/

static mkcl_index
utf_16_decoder(MKCL, mkcl_char16 * in, mkcl_index max, mkcl_character * out, bool * invalid)
{
  mkcl_index i = 0;
  mkcl_char16 ch;

  if (i >= max)
    return (*out = 0, i);

  ch = in[i]; i++;

  if ((ch & 0xFC00) == 0xD800)
    {
      if (max < 2)
	return (*out = 0xFFFD, *invalid = TRUE, i); /* incomplete surrogate pair */
      else
	{
	  mkcl_char16 aux = in[i++];
	  
	  if ((aux & 0xFC00) == 0xDC00)
	    {
	      mkcl_character cum = ((ch & 0x3FF) << 10) + (aux & 0x3FF) + 0x10000;
	      return (*out = cum, i);
	    }
	  else
	    return (*out = 0xFFFD, *invalid = TRUE, i); /* invalid surrogate pair */
	}
    }
  else
    return (*out = ch, i);
}

static inline mkcl_index
utf_16_encoder(mkcl_char16 *buffer, mkcl_character c, bool * invalid)
{
  if (c <= 0xFFFF)  {
    buffer[0] = c;
    return 1;
  } else if (c <= 0x10FFFF) {
    c -= 0x10000;
    buffer[0] = ((c >> 10) & 0x3FF) | 0xD800;
    buffer[1] = (c & 0x3FF) | 0xDC00;
    return 2;
  } else {
    /* return encoding_error(env, stream, buffer, c); */
    buffer[0] = 0xFFFD;
    *invalid = TRUE;
    return 1;
  }
}

static inline mkcl_index
utf_16_size(mkcl_character c)
{
  if (c <= 0xFFFF) {
    return 1;
  } else if (c <= 0x10FFFF) {
    return 2;
  } else {
    return 1;
  }
}

#define UTF_16_MAX_PER_CHARACTER 2

void mkcl_fill_utf_16_from_string(MKCL, mkcl_object utf_16, mkcl_index prefix_size, mkcl_object str)
{
  bool invalid = FALSE;
  mkcl_index i;
  mkcl_index utf_16_dim = utf_16->UTF_16.dim;
  mkcl_index j = prefix_size;
  mkcl_index str_len = str->string.fillp;
  mkcl_character * str_self = str->string.self;
  mkcl_char16 * utf_16_self = utf_16->UTF_16.self;

  for (i = 0; (i < str_len) && ((j + UTF_16_MAX_PER_CHARACTER) < utf_16_dim); i++)
    j += utf_16_encoder(utf_16_self + j, str_self[i], &invalid);

  for (; (i < str_len) && ((j + utf_16_size(str_self[i])) < utf_16_dim); i++)
    j += utf_16_encoder(utf_16_self + j, str_self[i], &invalid);

  if (i == str_len)
    {
      utf_16_self[j] = 0; /* needed by C! */
      utf_16->UTF_16.fillp = j;
    }
  else
    { /* overflow */
      mkcl_index old_i = i, old_j = j;
      /* mkcl_index old_utf_16_dim = utf_16_dim; */

      utf_16_dim = j;
      for (; i < str_len; i++)
	utf_16_dim += utf_16_size(str_self[i]);
  
      mkcl_char16 * self = mkcl_alloc_atomic(env, sizeof(mkcl_char16) * (utf_16_dim + 1));

      for (j = 0; j < old_j; j++) /* copy the part already encoded before the overflow */
	self[j] = utf_16_self[j];

      for (i = old_i; i < str_len; i++) /* encode the rest */
	j += utf_16_encoder(self + j, str_self[i], &invalid);

      self[j] = 0; /* needed by C! */
      utf_16->UTF_16.self = self;
      utf_16->UTF_16.fillp = utf_16->UTF_16.dim = utf_16_dim;
  }
}

void mkcl_fill_utf_16_from_base_string(MKCL, mkcl_object utf_16, mkcl_index prefix_size, mkcl_object str)
{
  mkcl_index i;
  mkcl_index utf_16_dim = utf_16->UTF_16.dim;
  mkcl_index str_len = str->base_string.fillp;
  mkcl_base_char * str_self = str->base_string.self;
  mkcl_char16 * utf_16_self = utf_16->UTF_16.self;

  if ((str_len + prefix_size) > utf_16_dim)
    { /* overflow */
      mkcl_index new_utf_16_dim = str_len + prefix_size;
      mkcl_char16 * self = mkcl_alloc_atomic(env, sizeof(mkcl_char16) * (new_utf_16_dim + 1));

      for (i = 0; i < prefix_size; i++)
	self[i] = utf_16_self[i];
      utf_16->UTF_16.self = utf_16_self = self;
      utf_16->UTF_16.dim = new_utf_16_dim;
    }

  utf_16_self += prefix_size;
  for (i = 0; i < str_len; i++)
    utf_16_self[i] = str_self[i];
  utf_16_self[i] = 0; /* needed by C! */
  utf_16->UTF_16.fillp = i;
}

mkcl_object mk_si_utf_16_p(MKCL, mkcl_object utf_16)
{
  mkcl_return_value((MKCL_UTF_16_P(utf_16) ? mk_cl_Ct : mk_cl_Cnil));
}

mkcl_object mk_si_utf_16_length(MKCL, mkcl_object utf_16)
{
  mkcl_call_stack_check(env);
  while(!MKCL_UTF_16_P(utf_16))
    utf_16 = mkcl_type_error(env, MK_SI_utf_16_length, "", utf_16, MK_SI_utf_16);

  mkcl_return_value(MKCL_MAKE_FIXNUM(utf_16->UTF_16.fillp));
}


mkcl_object mk_si_utf_16(MKCL, mkcl_object string)
{
  mkcl_index i;
  mkcl_index utf_16_dim = 0;
  mkcl_index j = 0;

  mkcl_call_stack_check(env);
  for (;;)
    switch(mkcl_type_of(string)) {
    case mkcl_t_string:
      {
        bool invalid = FALSE;
	mkcl_index str_len = string->string.fillp;
	mkcl_character * str = string->string.self;

	for (i = 0; i < str_len; i++)
	  utf_16_dim += utf_16_size(str[i]);
  
	mkcl_char16 * self = mkcl_alloc_atomic(env, sizeof(mkcl_char16) * (utf_16_dim + 1));

	for (i = 0, j = 0; i < str_len; i++)
	  j += utf_16_encoder(self + j, str[i], &invalid);
	self[j] = 0; /* needed by C! */

	mkcl_object utf_16 = mkcl_alloc_raw_utf_16(env);
      
	utf_16->UTF_16.fillp = utf_16->UTF_16.dim = utf_16_dim;
	utf_16->UTF_16.self = self;

	mkcl_return_2_values(utf_16, (invalid ? mk_cl_Ct : mk_cl_Cnil));
      }
    case mkcl_t_base_string:
      {
#if 0
        bool invalid = FALSE;
#endif
	mkcl_index str_len = string->base_string.fillp;
	mkcl_base_char * str = string->base_string.self;

#if 0
	for (i = 0; i < str_len; i++)
	  utf_16_dim += utf_16_size(str[i]);
#else
	utf_16_dim = str_len;
#endif
  
	mkcl_char16 * self = mkcl_alloc_atomic(env, sizeof(mkcl_char16) * (utf_16_dim + 1));

#if 0
	for (i = 0, j = 0; i < str_len; i++)
	  j += utf_16_encoder(self + j, str[i], &invalid);
#else
	for (i = 0; i < str_len; i++)
	  self[i] = str[i];
#endif
	self[str_len] = 0; /* needed by C! */

	mkcl_object utf_16 = mkcl_alloc_raw_utf_16(env);
      
	utf_16->UTF_16.fillp = utf_16->UTF_16.dim = utf_16_dim;
	utf_16->UTF_16.self = self;

#if 0
	mkcl_return_2_values(utf_16, (invalid ? mk_cl_Ct : mk_cl_Cnil));
#else
	mkcl_return_2_values(utf_16, mk_cl_Cnil);
#endif
      }
    case mkcl_t_UTF_8:
      {
	mkcl_index nb_char;
	mkcl_index str_len = string->UTF_8.fillp;
	bool utf_8_invalid = FALSE;
	bool utf_16_invalid = FALSE;

	for (i = 0, nb_char = 0; i < str_len; nb_char++)
	  utf_16_dim += utf_16_size(mkcl_utf_8_char(env, string, i, &i, &utf_8_invalid));

	mkcl_char16 * self = mkcl_alloc_atomic(env, sizeof(mkcl_char16) * (utf_16_dim + 1));

	for (i = 0, j = 0; i < str_len; i++)
	  j += utf_16_encoder(self + j, mkcl_utf_8_char(env, string, i, &i, &utf_8_invalid), &utf_16_invalid);
	self[j] = 0; /* needed by C! */

	mkcl_object utf_16 = mkcl_alloc_raw_utf_16(env);
      
	utf_16->UTF_16.fillp = utf_16->UTF_16.dim = utf_16_dim;
	utf_16->UTF_16.self = self;
	
	mkcl_return_2_values(utf_16, ((utf_8_invalid || utf_16_invalid) ? mk_cl_Ct : mk_cl_Cnil));
      }
      break;
    case mkcl_t_UTF_16:
      { mkcl_return_2_values(string, mk_cl_Cnil); }
      break;
    case mkcl_t_vector:
      switch (string->vector.elttype)
	{
	case mkcl_aet_b16:
	  {
	    mkcl_index len = string->vector.fillp;
	    mkcl_uint16_t * str = string->vector.self.b16;
	    
	    mkcl_char16 * self = mkcl_alloc_atomic(env, sizeof(mkcl_char16) * (len + 1));
	    
	    mkcl_object utf_16 = mkcl_alloc_raw_utf_16(env);

	    for (i = 0; i < len; i++)
	      self[i] = str[i];
	    self[len] = 0;

	    memcpy(self, str, len);
	    utf_16->UTF_16.fillp = utf_16->UTF_16.dim = len;
	    utf_16->UTF_16.self = self;
	    
	    mkcl_return_2_values(utf_16, mk_cl_Cnil);
	  }
	  break;
	default:
	  string = mkcl_type_error(env, MK_SI_utf_16, "", string, mk_cl_list(env, 3, MK_CL_or, MK_CL_string, MK_MKCL_double_octets));
	  break;
	}
      break;
    default:
      string = mkcl_type_error(env, MK_SI_utf_16, "", string, MK_CL_string);
      break;
    }
}

mkcl_object mk_si_utf_16_as_is(MKCL, mkcl_object string)
{ /* This function seems to be of dubious value after all. JCB */
  mkcl_index i;

  mkcl_call_stack_check(env);
  for (;;)
    switch(mkcl_type_of(string)) {
    case mkcl_t_string:
      {
	mkcl_index str_len = string->string.fillp;
	mkcl_character * str = string->string.self;
	mkcl_object utf_16 = mkcl_alloc_utf_16(env, str_len);
	mkcl_char16 * self = utf_16->UTF_16.self;
	mkcl_character ch;

	for (i = 0; i < str_len; i++)
	  if ((self[i] = (ch = str[i])) > 0xD800)
	    {
	      if ((ch <= 0xDFFF) || (ch > 0xFFFF))
		self[i] = 0xFFFD; /* replacement char */
	    }
	utf_16->UTF_16.fillp = i;
	mkcl_return_value(utf_16);
      }
    case mkcl_t_base_string:
      {
	mkcl_index str_len = string->base_string.fillp;
	mkcl_base_char * str = string->base_string.self;
	mkcl_object utf_16 = mkcl_alloc_utf_16(env, str_len);
	mkcl_char16 * self = utf_16->UTF_16.self;

	for (i = 0; i < str_len; i++)
	  self[i] = str[i];
	utf_16->UTF_16.fillp = i;
	mkcl_return_value(utf_16);
      }
    default:
      string = mkcl_type_error(env, MK_SI_utf_16_as_is, "", string, MK_CL_string);
      break;
    }
}



mkcl_object mkcl_utf_16_to_string(MKCL, mkcl_object utf_16)
{
  bool invalid = FALSE;
  mkcl_index dim = utf_16->UTF_16.fillp;
  mkcl_char16 * self = utf_16->UTF_16.self;
  mkcl_index fillp = 0;
  mkcl_index i;

  {
#if 0
    mkcl_character data[dim + 1]; /* Yes, it's a VLA! */
#else
    mkcl_VLA(env, mkcl_character, data, dim + 1);
#endif

    for (i = 0; i < dim; fillp++)
      i += utf_16_decoder(env, &self[i], dim - i, &data[fillp], &invalid);
    data[fillp] = '\0';
 
    mkcl_object new = mkcl_alloc_adjustable_character_string(env, fillp);
    if (fillp)
      (void) mkcl_wmemcpy(new->string.self, data, fillp + 1);
    new->string.fillp = fillp;
    mkcl_return_2_values(new, (invalid ? mk_cl_Ct : mk_cl_Cnil));
  }
}

mkcl_object mkcl_utf_16_to_base_string(MKCL, mkcl_object utf_16)
{
  bool invalid = FALSE;
  mkcl_index dim = utf_16->UTF_16.fillp;
  mkcl_char16 * self = utf_16->UTF_16.self;
  mkcl_index fillp = 0;
  mkcl_index i;

  {
    mkcl_character ch;
#if 0
    mkcl_base_char data[dim + 1]; /* Yes, it's a VLA! */
#else
    mkcl_VLA(env, mkcl_base_char, data, dim + 1);
#endif

    for (i = 0; i < dim; fillp++)
      {
	i += utf_16_decoder(env, &self[i], dim - i, &ch, &invalid);
	if (ch >= 0x0100)
	  { data[fillp] = 0x0BF; invalid = TRUE; }
	else
	  data[fillp] = ch;
      }
    data[fillp] = '\0';
 
    mkcl_object new = mkcl_alloc_simple_base_string(env, fillp);
    if (fillp)
      memcpy(new->base_string.self, data, fillp + 1);
    mkcl_return_2_values(new, (invalid ? mk_cl_Ct : mk_cl_Cnil));
  }
}

mkcl_character mkcl_utf_16_char(MKCL, mkcl_object utf_16, mkcl_index index, mkcl_index * next, bool * invalid)
{
  mkcl_character ch;
  mkcl_index size = utf_16_decoder(env, &(utf_16->UTF_16.self[index]), utf_16->UTF_16.fillp - index, &ch, invalid);
  
  if (size > 0)
    { *next = index + size; *invalid = FALSE; }
  else
    { *next = index - size; *invalid = TRUE; }

  return ch;
}

struct mkcl_cfun mk_si_utf_16_char_cfunobj = MKCL_CFUN2(mk_si_utf_16_char, MK_SI_utf_16_char);

mkcl_object mk_si_utf_16_char(MKCL, mkcl_object utf_16, mkcl_object index_fix)
{
  mkcl_index next, index;
  bool invalid = FALSE;

  mkcl_call_stack_check(env);
  while(!MKCL_UTF_16_P(utf_16))
    utf_16 = mkcl_type_error(env, MK_SI_utf_16_char, "", utf_16, MK_SI_utf_16);

  mkcl_index dim = utf_16->UTF_16.dim;

  while(!(MKCL_FIXNUMP(index_fix) && ((index = mkcl_fixnum_to_word(index_fix)) < dim)))
    index_fix = mkcl_out_of_bounds_error(env, MK_SI_utf_16_char, "index", index_fix, 0, dim);

  mkcl_return_3_values(MKCL_CODE_CHAR(mkcl_utf_16_char(env, utf_16, index, &next, &invalid)),
                       MKCL_MAKE_FIXNUM(next),
                       (invalid ? mk_cl_Ct : mk_cl_Cnil));
}

#if 0
struct mkcl_cfun mk_si_utf_16Plus_cfunobj = MKCL_CFUN_VA(mk_si_utf_16Plus, MK_SI_utf_16Plus);
#endif

mkcl_object mk_si_utf_16Plus(MKCL, mkcl_narg narg, ...)
{
  mkcl_index len;
  int i;
  mkcl_object output;

  mkcl_call_stack_check(env);
  {
    mkcl_setup_for_rest(env, MK_SI_utf_16P, 0, narg, narg, args);

    if (narg == 1) {
      mkcl_object s = mkcl_va_arg(args);
      
      if (MKCL_UTF_16_P(s))
        {
          mkcl_index dim = s->UTF_16.fillp;
	  
          output = mkcl_alloc_utf_16(env, dim);
          if (dim)
            {
              mkcl_char16 * to = output->UTF_16.self, * from = s->UTF_16.self;
              mkcl_index i;
              for (i = 0; i < dim; i++) to[i] = from[i];
            }
          /* memcpy(output->UTF_16.self, s->UTF_16.self, dim); */
          output->UTF_16.self[dim] = '\0';
          output->UTF_16.fillp = dim;
        }
      else
        output = mk_si_utf_16(env, s);
    } else {
      /* Compute final size and store NONEMPTY coerced strings. */
      len = 0;
      for (i = 0; i < narg; i++) {
        mkcl_object arg = mkcl_va_arg(args);
        mkcl_object s = (MKCL_UTF_16_P(arg) ? arg : mk_si_utf_16(env, arg));
        mkcl_index dim = s->UTF_16.fillp;
        if (dim) {
          MKCL_TEMP_STACK_PUSH(env, s);
          len += dim;
        }
      }
      /* Do actual copying by recovering those strings */
      output = mkcl_alloc_utf_16(env, len);
      output->UTF_16.self[len] = 0; /* string terminator for C. */
      output->UTF_16.fillp = len;
      while (len > 0) {
        mkcl_object s = MKCL_TEMP_STACK_POP_UNSAFE(env);
        size_t bytes = s->UTF_16.fillp;
        len -= bytes;
        {
          mkcl_char16 * to = output->UTF_16.self + len, * from = s->UTF_16.self;
          mkcl_index i;
          for (i = 0; i < bytes; i++) to[i] = from[i];
        }
        /* memcpy((output->UTF_16.self + len), s->UTF_16.self, bytes); */
      }
    }
    mkcl_va_end(args);
    mkcl_return_value(output);
  }
}


mkcl_object mk_si_utf_16E(MKCL, mkcl_object u1, mkcl_object u2)
{
  mkcl_call_stack_check(env);
  while(!MKCL_UTF_16_P(u1))
    u1 = mkcl_type_error(env, MK_SI_utf_16E, "", u1, MK_SI_utf_16);

  while(!MKCL_UTF_16_P(u2))
    u2 = mkcl_type_error(env, MK_SI_utf_16E, "", u2, MK_SI_utf_16);

  mkcl_index dim = u1->UTF_16.fillp;

  if (dim != u2->UTF_16.fillp)
    { mkcl_return_value(mk_cl_Cnil); }
  else
    {
      mkcl_char16 * s1 = u1->UTF_16.self, * s2 = u2->UTF_16.self;
      mkcl_index i;

      for (i = 0; i < dim; i++)
	if (s1[i] != s2[i]) { mkcl_return_value(mk_cl_Cnil); }
      mkcl_return_value(mk_cl_Ct);
    }
    /* { mkcl_return_value((0 == memcmp(u1->UTF_16.self, u2->UTF_16.self, dim) ? mk_cl_Ct : mk_cl_Cnil)); } */
}

mkcl_object mkcl_cstring_to_utf_8(MKCL, const char * str)
{
  mkcl_object utf_8 = mkcl_alloc_raw_utf_8(env);

  utf_8->UTF_8.self = (mkcl_char8 *) str;
  utf_8->UTF_8.fillp = utf_8->UTF_8.dim = (str ? strlen(str) : 0);
  return utf_8;
}

mkcl_object mkcl_cstring_copy_to_utf_8(MKCL, const char * str)
{
  mkcl_object utf_8 = mkcl_alloc_raw_utf_8(env);
  mkcl_index len = (str ? strlen(str) : 0);
  mkcl_char8 * self = utf_8->UTF_8.self = mkcl_alloc_atomic(env, len + 1);

  if (str != NULL)
    memcpy(self, str, len);
  self[len] = '\0';
  utf_8->UTF_8.fillp = utf_8->UTF_8.dim = len;
  return utf_8;
}

mkcl_object mkcl_cstring_to_utf_16(MKCL, const char * str)
{
  const size_t len = (str ? strlen(str) : 0);
  mkcl_UTF_8_object_sized(utf_8_obj, (mkcl_char8 *) str, len);
  return mk_si_utf_16(env, (mkcl_object) &utf_8_obj);
}

mkcl_object mkcl_cstring16_to_utf_16(MKCL, const mkcl_char16 * str)
{
  mkcl_object utf_16 = mkcl_alloc_raw_utf_16(env);
  mkcl_index dim = 0;

  utf_16->UTF_16.self = (mkcl_char16 *) str;
  if (str != NULL) for (; str[dim]; dim++);
  utf_16->UTF_16.fillp = utf_16->UTF_16.dim = dim;
  return utf_16;
}

mkcl_object mkcl_cstring16_copy_to_utf_16(MKCL, const mkcl_char16 * str)
{
  mkcl_object utf_16 = mkcl_alloc_raw_utf_16(env);
  mkcl_index len = 0;
  mkcl_char16 * self;
  mkcl_index i;

  if (str != NULL) for (; str[len]; len++);
  self = utf_16->UTF_16.self = mkcl_alloc_atomic(env, sizeof(mkcl_char16) * (len + 1));
  for (i = 0; i < len; i++)
    self[i] = str[i];
  self[len] = 0;
  utf_16->UTF_16.fillp = utf_16->UTF_16.dim = len;
  return utf_16;
}

bool mkcl_os_string_format_is_utf_8_p(MKCL)
{
  return (MK_KEY_utf_8 == mkcl_symbol_value(env, MK_SI_DYNVAR_os_string_format));
}


#ifndef mkcl_alloc_OSstring
mkcl_object mkcl_alloc_OSstring(MKCL, mkcl_index size)
{
  mkcl_object external_format = mkcl_symbol_value(env, MK_SI_DYNVAR_os_string_format);

  if (external_format == MK_KEY_utf_8)
    return mkcl_alloc_utf_8(env, size);
  else
    return mkcl_alloc_adjustable_base_string(env, size);
}


mkcl_object mkcl_cstring_to_OSstring(MKCL, char * str)
{
  mkcl_object external_format = mkcl_symbol_value(env, MK_SI_DYNVAR_os_string_format);

  if (external_format == MK_KEY_utf_8)
    return mkcl_cstring_to_utf_8(env, str);
  else
    return mkcl_make_adjustable_base_string(env, str);
}

mkcl_object mkcl_cstring_copy_to_OSstring(MKCL, char * str)
{
  mkcl_object external_format = mkcl_symbol_value(env, MK_SI_DYNVAR_os_string_format);

  if (external_format == MK_KEY_utf_8)
    return mkcl_cstring_copy_to_utf_8(env, str);
  else
    return mkcl_make_adjustable_base_string_copy(env, str);
}

mkcl_object mkcl_string_to_OSstring(MKCL, mkcl_object string)
{
  mkcl_object external_format = mkcl_symbol_value(env, MK_SI_DYNVAR_os_string_format);

  if (external_format == MK_KEY_utf_8)
    return mk_si_utf_8(env, string);
  else
    return mkcl_coerce_to_adjustable_base_string(env, string);
}

mkcl_object mkcl_cstring_to_string(MKCL, char * str)
{
  mkcl_object external_format = mkcl_symbol_value(env, MK_SI_DYNVAR_os_string_format);

  if (external_format == MK_KEY_utf_8)
    {
      const size_t len = (str ? strlen(str) : 0);
      mkcl_UTF_8_object_sized(utf_8_obj, (mkcl_char8 *) str, len);
      return mkcl_utf_8_to_string(env, (mkcl_object) &utf_8_obj);
    }
  else
    return mkcl_make_adjustable_base_string_copy(env, str);
}

#else /* def mkcl_alloc_OSstring */

mkcl_object mkcl_cstring_to_string(MKCL, char * str)
{
  const size_t len = (str ? strlen(str) : 0);
  mkcl_UTF_8_object_sized(utf_8_obj, str, len);
  return mkcl_utf_8_to_string(env, (mkcl_object) &utf_8_obj);
}

#endif /* def mkcl_alloc_OSstring */

mkcl_object mkcl_cstring16_to_string(MKCL, mkcl_char16 * str)
{
  mkcl_index len;

  for (len = 0; str[len]; len++);
  mkcl_UTF_16_object_sized(utf_16_obj, str, len);
  return mkcl_utf_16_to_string(env, (mkcl_object) &utf_16_obj);
}

mkcl_char8 * mkcl_extend_utf_8(MKCL, mkcl_object s)
{
  /* The following code expects that (2 * MKCL_ADIMLIM) fits inside a mkcl_index without overflow! JCB */
  if (s->UTF_8.dim >= MKCL_ADIMLIM)
    mkcl_FEerror(env, "Can't extend the UTF-8 string.", 0);

  /* We grow by at least the size of the largest UTF-8 character (ie: 4) */
  mkcl_index new_dim = 4 + s->UTF_8.dim + (s->UTF_8.dim / 2);
  if (new_dim > MKCL_ADIMLIM)
    new_dim = MKCL_ADIMLIM + 3;

  mkcl_char8 * new_self = mkcl_alloc_atomic(env, new_dim + 1);
  const mkcl_index fillp = s->UTF_8.fillp;

  if (fillp) memcpy(new_self, s->UTF_8.self, fillp);
  new_self[fillp] = '\0';

  /* Because we do not call mk_si_replace_array here and rather do a direct assignement to "self"
     we cannot use UTF-8 objects as targets of displaced arrays. The array displacement update
     machinery is simply not setup.
  */
  s->UTF_8.self = new_self;
  s->UTF_8.dim = new_dim;
  return(new_self);
}

mkcl_index mkcl_utf_8_push_extend(MKCL, mkcl_object utf_8, mkcl_character ch, bool * invalid)
{
  bool my_invalid = FALSE;
  const mkcl_index fillp = utf_8->UTF_8.fillp;
  const mkcl_index new_fillp = fillp + utf_8_size(ch);
  mkcl_char8 * const self
    = (mkcl_unlikely(new_fillp > utf_8->UTF_8.dim) ? mkcl_extend_utf_8(env, utf_8) : utf_8->UTF_8.self);

  utf_8_encoder(&self[fillp], ch, &my_invalid);
  utf_8->UTF_8.fillp = new_fillp;
  self[new_fillp] = 0; /* Make sure the string is NULL terminated for C sake. */
  if (invalid) *invalid = my_invalid;
  return fillp;
}

struct mkcl_cfun mk_si_utf_8_push_extend_cfunobj = MKCL_CFUN2(mk_si_utf_8_push_extend, MK_SI_utf_8_push_extend);

mkcl_object mk_si_utf_8_push_extend(MKCL, mkcl_object utf_8, mkcl_object ch)
{
  mkcl_call_stack_check(env);
  while (mkcl_unlikely(!MKCL_UTF_8_P(utf_8)))
    utf_8 = mkcl_type_error(env, MK_SI_utf_8_push_extend, "utf-8", utf_8, MK_SI_utf_8);

  while (mkcl_unlikely(!MKCL_CHARACTERP(ch)))
    ch = mkcl_type_error(env, MK_SI_utf_8_push_extend, "ch", ch, MK_CL_character);

  mkcl_return_value(MKCL_MAKE_FIXNUM(mkcl_utf_8_push_extend(env, utf_8, MKCL_CHAR_CODE(ch), NULL)));
}

mkcl_object mkcl_utf_8_nconc(MKCL, mkcl_object base, mkcl_object new)
{
  mkcl_index new_fillp = new->UTF_8.fillp;
  mkcl_index base_fillp = base->UTF_8.fillp;
  mkcl_index new_base_fillp = base_fillp + new_fillp;
  mkcl_index base_dim;
  mkcl_char8 * base_self = base->UTF_8.self;

  if (new_fillp == 0) return base;

  while (new_base_fillp > (base_dim = base->UTF_8.dim))
    base_self = mkcl_extend_utf_8(env, base);

  memcpy(&(base_self[base_fillp]), new->UTF_8.self, new_fillp);
  base_self[new_base_fillp] = '\0';
  base->UTF_8.fillp = new_base_fillp;
  return base;
}

mkcl_character mkcl_utf_8_last(MKCL, mkcl_object utf_8, bool * invalid)
{
  bool my_invalid = FALSE;
  mkcl_index next; /* ignored */
  mkcl_character ch = 0;
  mkcl_index fillp = utf_8->UTF_8.fillp;
  if (fillp)
    {
      mkcl_index last = fillp - 1;

      ch = mkcl_utf_8_char(env, utf_8, last, &next, &my_invalid);
      if (my_invalid && (last > 0))
	{
	  ch = mkcl_utf_8_char(env, utf_8, last - 1, &next, &my_invalid);
	  if (my_invalid && (last > 1))
	    {
	      ch = mkcl_utf_8_char(env, utf_8, last - 2, &next, &my_invalid);
	      if (my_invalid && (last > 2))
		ch = mkcl_utf_8_char(env, utf_8, last - 3, &next, &my_invalid);
	    }
	}
    }
  *invalid = my_invalid;
  return ch;
}

mkcl_object mk_si_utf_8_last(MKCL, mkcl_object utf_8)
{
  bool invalid = FALSE;

  mkcl_call_stack_check(env);
  while (mkcl_unlikely(!MKCL_UTF_8_P(utf_8)))
    utf_8 = mkcl_type_error(env, MK_SI_utf_8_last, "utf-8", utf_8, MK_SI_utf_8);

  mkcl_return_2_values(MKCL_CODE_CHAR(mkcl_utf_8_last(env, utf_8, &invalid)), (invalid ? mk_cl_Ct : mk_cl_Cnil));
}

mkcl_char16 * mkcl_extend_utf_16(MKCL, mkcl_object s)
{
  /* The following code expects that (2 * MKCL_ADIMLIM) fits inside a mkcl_index without overflow! JCB */
  if (s->UTF_16.dim >= MKCL_ADIMLIM)
    mkcl_FEerror(env, "Can't extend the UTF-16 string.", 0);

  /* We grow by at least the size of the largest UTF-16 character (ie: 4) */
  mkcl_index new_dim = 4 + s->UTF_16.dim + (s->UTF_16.dim / 2);
  if (new_dim > MKCL_ADIMLIM)
    new_dim = MKCL_ADIMLIM + 3;

  mkcl_char16 * new_self = mkcl_alloc_atomic(env, sizeof(mkcl_char16) * (new_dim + 1));
  const mkcl_index fillp = s->UTF_16.fillp;

  if (fillp) memcpy(new_self, s->UTF_16.self, sizeof(mkcl_char16) * fillp);
  new_self[fillp] = 0;

  /* Because we do not call mk_si_replace_array here and rather do a direct assignement to "self"
     we cannot use UTF-16 objects as targets of displaced arrays. The array displacement update
     machinery is simply not setup.
  */
  s->UTF_16.self = new_self;
  s->UTF_16.dim = new_dim;
  return(new_self);
}

mkcl_index mkcl_utf_16_push_extend(MKCL, mkcl_object utf_16, mkcl_character ch, bool * invalid)
{
  bool my_invalid = FALSE;
  const mkcl_index fillp = utf_16->UTF_16.fillp;
  const mkcl_index new_fillp = fillp + utf_16_size(ch);
  mkcl_char16 * const self
    = (mkcl_unlikely(new_fillp > utf_16->UTF_16.dim) ? mkcl_extend_utf_16(env, utf_16) : utf_16->UTF_16.self);

  utf_16_encoder(&self[fillp], ch, &my_invalid);
  utf_16->UTF_16.fillp = new_fillp;
  self[new_fillp] = 0; /* Make sure the string is NULL terminated for C sake. */
  if (invalid) *invalid = my_invalid;
  return fillp;
}

struct mkcl_cfun mk_si_utf_16_push_extend_cfunobj = MKCL_CFUN2(mk_si_utf_16_push_extend, MK_SI_utf_16_push_extend);

mkcl_object mk_si_utf_16_push_extend(MKCL, mkcl_object utf_16, mkcl_object ch)
{
  mkcl_call_stack_check(env);
  while (mkcl_unlikely(!MKCL_UTF_16_P(utf_16)))
    utf_16 = mkcl_type_error(env, MK_SI_utf_16_push_extend, "utf-16", utf_16, MK_SI_utf_16);

  while (mkcl_unlikely(!MKCL_CHARACTERP(ch)))
    ch = mkcl_type_error(env, MK_SI_utf_16_push_extend, "ch", ch, MK_CL_character);

  mkcl_return_value(MKCL_MAKE_FIXNUM(mkcl_utf_16_push_extend(env, utf_16, MKCL_CHAR_CODE(ch), NULL)));
}

mkcl_object mkcl_utf_16_nconc(MKCL, mkcl_object base, mkcl_object new)
{
  mkcl_index new_fillp = new->UTF_16.fillp;
  mkcl_index base_fillp = base->UTF_16.fillp;
  mkcl_index new_base_fillp = base_fillp + new_fillp;
  mkcl_index base_dim;
  mkcl_char16 * base_self = base->UTF_16.self;

#if 0 /* debug */
  while (mkcl_unlikely(!MKCL_UTF_16_P(new)))
    new = mkcl_type_error(env, MK_SI_utf_16_push_extend, "utf-16", new, MK_SI_utf_16);

  while (mkcl_unlikely(!MKCL_UTF_16_P(base)))
    base = mkcl_type_error(env, MK_SI_utf_16_push_extend, "utf-16", base, MK_SI_utf_16);
#endif

  if (new_fillp == 0) return base;

  while (new_base_fillp > (base_dim = base->UTF_16.dim))
    base_self = mkcl_extend_utf_16(env, base);

  memcpy(&(base_self[base_fillp]), new->UTF_16.self, sizeof(mkcl_char16) * new_fillp);
  base_self[new_base_fillp] = 0;
  base->UTF_16.fillp = new_base_fillp;
  return base;
}

mkcl_character mkcl_utf_16_last(MKCL, mkcl_object utf_16, bool * invalid)
{
  bool my_invalid = FALSE;
  mkcl_index next; /* ignored */
  mkcl_character ch = 0;
  mkcl_index fillp = utf_16->UTF_16.fillp;
  if (fillp)
    {
      mkcl_index last = fillp - 1;

      ch = mkcl_utf_16_char(env, utf_16, last, &next, &my_invalid);
      if (my_invalid && (last > 0))
	ch = mkcl_utf_16_char(env, utf_16, last - 1, &next, &my_invalid);
    }
  *invalid = my_invalid;
  return ch;
}

mkcl_object mk_si_utf_16_last(MKCL, mkcl_object utf_16)
{
  bool invalid = FALSE;

  mkcl_call_stack_check(env);
  while (mkcl_unlikely(!MKCL_UTF_16_P(utf_16)))
    utf_16 = mkcl_type_error(env, MK_SI_utf_8_push_extend, "utf-16", utf_16, MK_SI_utf_8);

  mkcl_return_2_values(MKCL_CODE_CHAR(mkcl_utf_16_last(env, utf_16, &invalid)), (invalid ? mk_cl_Ct : mk_cl_Cnil));
}

mkcl_base_char mkcl_base_string_last(MKCL, mkcl_object str)
{
  if (MKCL_BASE_STRING_P(str) && str->base_string.fillp && str->base_string.self)
    return str->base_string.self[str->base_string.fillp - 1];
  else
    return '\0';
}

mkcl_character mkcl_string_last(MKCL, mkcl_object str)
{
  if (MKCL_BASE_STRING_P(str) && str->base_string.fillp && str->base_string.self)
    return str->base_string.self[str->base_string.fillp - 1];
  else if (MKCL_STRINGP(str) && str->string.fillp && str->string.self)
    return str->string.self[str->string.fillp - 1];
  else
    return 0;
}


mkcl_object mkcl_base_string_nconc(MKCL, mkcl_object base, mkcl_object new)
{
  mkcl_index new_fillp = new->base_string.fillp;
  mkcl_index base_fillp = base->base_string.fillp;
  mkcl_index new_base_fillp = base_fillp + new_fillp;
  mkcl_index base_dim;
  mkcl_base_char * base_self = base->base_string.self;

  if (new_fillp == 0) return base;

  while (new_base_fillp > (base_dim = base->base_string.dim))
    base_self = mkcl_extend_base_string(env, base);

  memcpy(&(base_self[base_fillp]), new->base_string.self, new_fillp);
  base_self[new_base_fillp] = '\0';
  base->base_string.fillp = new_base_fillp;
  return base;
}

#ifndef mkcl_OSstring_nconc
mkcl_object mkcl_OSstring_nconc(MKCL, mkcl_object base, mkcl_object new)
{
  if (MKCL_UTF_8_P(base))
    return mkcl_utf_8_nconc(env, base, new);
  else
    return mkcl_base_string_nconc(env, base, new);
}

mkcl_object mkcl_OSstring_nconc_cstring(MKCL, mkcl_object base, char * new)
{
  size_t new_len = (new ? strlen(new) : 0);
  
  if (MKCL_UTF_8_P(base))
    {
      mkcl_UTF_8_object_sized(new_utf_8_obj, (mkcl_char8 *) new, new_len);
      return mkcl_utf_8_nconc(env, base, (mkcl_object) &new_utf_8_obj);
    }
  else
    {
      mkcl_base_string_object_sized(new_bstr_obj, new, new_len);
      return mkcl_base_string_nconc(env, base, (mkcl_object) &new_bstr_obj);
    }
}

mkcl_index mkcl_OSstring_push_extend(MKCL, mkcl_object str, mkcl_character ch)
{
  if (MKCL_UTF_8_P(str))
    return mkcl_utf_8_push_extend(env, str, ch, NULL);
  else
    return mkcl_base_string_push_extend(env, str, ch);
}
#endif /* ndef mkcl_OSstring_nconc */


mkcl_object
mkcl_alloc_octets(MKCL, mkcl_index length)
{
  mkcl_object x;

  x = mkcl_alloc_raw_vector(env);
  x->vector.hasfillp     = FALSE;
  x->vector.adjustable   = FALSE;
  x->vector.displaced    = mk_cl_Cnil;
  x->vector.dim          = (x->vector.fillp = length);
  x->vector.elttype      = mkcl_aet_b8;
  x->vector.elem         = mkcl_aref_index_b8;
  x->vector.set          = mkcl_aset_index_b8;
  x->vector.self.b8      = (uint8_t *) mkcl_alloc_atomic(env, sizeof(uint8_t) * length);
  return x;
}

mkcl_object
mk_mkcl_octets(MKCL, mkcl_object obj)
{
  mkcl_object output;
  mkcl_index len;

  mkcl_call_stack_check(env);
 AGAIN:
  switch (mkcl_type_of(obj)) {
  case mkcl_t_UTF_8:
    len = obj->UTF_8.fillp;
    output = mkcl_alloc_octets(env, len);
    memcpy(output->vector.self.b8, obj->UTF_8.self, len);
    break;
  default:
    obj = mkcl_type_error(env, MK_MKCL_octets, "", obj, MK_SI_utf_8);
    goto AGAIN;
  }
  mkcl_return_value(output);
}

mkcl_object
mkcl_alloc_double_octets(MKCL, mkcl_index length)
{
  mkcl_object x;

  x = mkcl_alloc_raw_vector(env);
  x->vector.hasfillp     = FALSE;
  x->vector.adjustable   = FALSE;
  x->vector.displaced    = mk_cl_Cnil;
  x->vector.dim          = (x->vector.fillp = length);
  x->vector.elttype      = mkcl_aet_b16;
  x->vector.elem         = mkcl_aref_index_b16;
  x->vector.set          = mkcl_aset_index_b16;
  x->vector.self.b16     = (uint16_t *) mkcl_alloc_atomic(env, sizeof(uint16_t) * length);
  return x;
}

mkcl_object
mk_mkcl_double_octets(MKCL, mkcl_object obj)
{
  mkcl_object output;
  mkcl_index len;

  mkcl_call_stack_check(env);
 AGAIN:
  switch (mkcl_type_of(obj)) {
  case mkcl_t_UTF_16:
    len = obj->UTF_16.fillp;
    output = mkcl_alloc_double_octets(env, len);
    {
      mkcl_char16 * to = output->vector.self.b16, * from = obj->UTF_16.self;
      mkcl_index i;
      for (i = 0; i < len; i++) to[i] = from[i];
    }
    break;
  default:
    obj = mkcl_type_error(env, MK_MKCL_octets, "", obj, MK_SI_utf_16);
    goto AGAIN;
  }
  mkcl_return_value(output);
}

