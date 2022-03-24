/* -*- mode: c  -*- */
/*
    Copyright (c) 2010-2022, Jean-Claude Beaudoin.
    Copyright by a number of previous anonymous authors
              presumed to be the same as for the rest of MKCL.

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 3 of the License, or (at your option) any later version.

    See file '../../Copyright' for full details.
*/

#include <mkcl/mkcl.h>
#include <ctype.h>
#include <string.h>
#include <mkcl/internal.h>

#include <stdio.h>


static mkcl_index mangled_char_size(mkcl_character ch)
{
  mkcl_index size;

  if ('A' <= ch && ch <= 'Z')
    size = 1;
  else if ('a' <= ch && ch <= 'z')
    size = 1;
  else if ('0' <= ch && ch <= '9')
    size = 1;
  else if (ch == '_')
    size = 2;
  else if (ch == '-')
    size = 2;
  else if (ch == '&')
    size = 2;
  else if (ch == '*')
    size = 2;
  else if (ch == '+')
    size = 2;
  else if (ch == '<')
    size = 2;
  else if (ch == '>')
    size = 2;
  else if (ch == '=')
    size = 2;
  else if (ch < 0x100) /* all other extended ASCII */
    size = 4;
  else if (ch < 0x10000) /* BMP Unicode */
    size = 6;
  else /* all other Unicode */
    size = 8;

  return size;
}

static mkcl_index mangled_length(mkcl_object str)
{
  mkcl_index len = 0;

  if (MKCL_BASE_STRING_P(str))
    {
      char * src = (char *) str->base_string.self;
      mkcl_index fillp = str->base_string.fillp;
      mkcl_index i;

      for (i = 0; i < fillp; i++)
	len += mangled_char_size(src[i]);
    }
  else
    {
      mkcl_character * src = str->string.self;
      mkcl_index fillp = str->string.fillp;
      mkcl_index i;

      for (i = 0; i < fillp; i++)
	len += mangled_char_size(src[i]);
    }

  return len;
}

static mkcl_base_char * push_char_to_hex(char ch, mkcl_base_char * dest)
{
  int val = ((ch >> 4) & 0x0F);
  *dest++ = ((val < 10) ? '0' + val : 'A' + (val - 10));
  val = (ch & 0x0F);
  *dest++ = ((val < 10) ? '0' + val : 'A' + (val - 10));
  return dest;
}

static mkcl_base_char * push_mangled_char(mkcl_character ch, mkcl_base_char * dest)
{
  if ('A' <= ch && ch <= 'Z')
    *dest++ = ch;
  else if ('a' <= ch && ch <= 'z')
    *dest++ = ch;
  else if ('0' <= ch && ch <= '9')
    *dest++ = ch;
  else if (ch == '_')
    { *dest++ = '_'; *dest++ = 'S'; }
  else if (ch == '-')
    { *dest++ = '_'; *dest++ = '_'; }
  else if (ch == '&')
    { *dest++ = '_'; *dest++ = 'A'; }
  else if (ch == '*')
    { *dest++ = '_'; *dest++ = 'X'; }
  else if (ch == '+')
    { *dest++ = '_'; *dest++ = 'P'; }
  else if (ch == '<')
    { *dest++ = '_'; *dest++ = 'L'; }
  else if (ch == '>')
    { *dest++ = '_'; *dest++ = 'G'; }
  else if (ch == '=')
    { *dest++ = '_'; *dest++ = 'E'; }
  else if (ch < 0x100) /* all other extended ASCII */
    {
      *dest++ = '_'; *dest++ = 'a';
      dest = push_char_to_hex(ch, dest);
    }
  else if (ch < 0x10000) /* BMP Unicode */
    { 
      *dest++ = '_'; *dest++ = 'u';
      dest = push_char_to_hex((ch >> 8), dest);
      dest = push_char_to_hex(ch, dest);
    }
  else /* all other Unicode */
    { 
      *dest++ = '_'; *dest++ = 'U';
      dest = push_char_to_hex((ch >> 16), dest);
      dest = push_char_to_hex((ch >> 8), dest);
      dest = push_char_to_hex(ch, dest);
    }

  return dest;
}

static mkcl_base_char * push_mangled_string(mkcl_object str, mkcl_base_char * dest)
{
  mkcl_index i = 0;
  mkcl_index len;
  
  if (MKCL_BASE_STRING_P(str))
    {
      char * src = (char *) str->base_string.self;

      len = str->base_string.fillp;
      for (i = 0; i < len; i++)
	dest = push_mangled_char(src[i], dest);
    }
  else
    {
      mkcl_character * src = str->string.self;
      len = str->string.fillp;
      for (i = 0; i < len; i++)
	dest = push_mangled_char(src[i], dest);
    }

  return dest;
}

struct mkcl_cfun mk_si_mangle_string_cfunobj = MKCL_CFUN1(mk_si_mangle_string, (mkcl_object) &MK_SI_mangle_string);

mkcl_object mk_si_mangle_string(MKCL, mkcl_object string)
{
  mkcl_call_stack_check(env);
  if (MKCL_STRINGP(string))
    {
      mkcl_index output_length = mangled_length(string);
      mkcl_object output = mkcl_alloc_simple_base_string(env, output_length);
      mkcl_base_char * dest = output->base_string.self;
      
      dest = push_mangled_string(string, dest);
      
      mkcl_return_value(output);
    }
  else
    mkcl_return_value(mk_cl_Cnil);
}

/* Universal prefix for ManKai Common Lisp. JCB */
#define PREFIX "mk_"
#define PREFIX_SIZE (sizeof(PREFIX) - 1)

#define PKG_SEPARATOR "_8_"
#define PKG_SEPARATOR_SIZE (sizeof(PKG_SEPARATOR) - 1)

static const mkcl_base_string_object(cl_pkg_name_obj, "cl");
static const mkcl_base_string_object(si_pkg_name_obj, "si");
static const mkcl_base_string_object(key_pkg_name_obj, "key");
static const mkcl_base_string_object(nil_pkg_name_obj, "#");

static mkcl_object
mangle_full_symbol_name(MKCL, mkcl_object symbol, char * suffix)
{
  size_t suffix_length = strlen(suffix);
  mkcl_object symbol_name = mkcl_symbol_name(env, symbol);
  mkcl_object package = mkcl_symbol_package(env, symbol);
  mkcl_object package_name;

  if (mkcl_Null(package))
    package_name = (mkcl_object) &nil_pkg_name_obj;
  else if (package == mkcl_core.lisp_package)
    package_name = (mkcl_object) &cl_pkg_name_obj;
  else if (package == mkcl_core.system_package)
    package_name = (mkcl_object) &si_pkg_name_obj;
  else if (package == mkcl_core.keyword_package)
    package_name = (mkcl_object) &key_pkg_name_obj;
  else
    package_name = package->pack.name;


  mkcl_index output_length = (PREFIX_SIZE + mangled_length(package_name)
			      + PKG_SEPARATOR_SIZE + mangled_length(symbol_name)
			      + suffix_length);

  mkcl_object output = mkcl_alloc_simple_base_string(env, output_length);
  mkcl_base_char * dest = output->base_string.self;

  mkcl_index i;

  for (i = 0; i < PREFIX_SIZE; i++, dest++)
    *dest = PREFIX[i];

  dest = push_mangled_string(package_name, dest);

  for (i = 0; i < PKG_SEPARATOR_SIZE; i++, dest++)
    *dest = PKG_SEPARATOR[i];

  dest = push_mangled_string(symbol_name, dest);

  for (i = 0; i < suffix_length; i++, dest++)
    *dest = suffix[i];

  *dest = '\0'; /* C string termination */
  
  return output;
}

struct mkcl_cfun mk_si_mangle_symbol_cfunobj = MKCL_CFUN1(mk_si_mangle_symbol, (mkcl_object) &MK_SI_mangle_symbol);

mkcl_object mk_si_mangle_symbol(MKCL, mkcl_object symbol)
{
  mkcl_call_stack_check(env);
  if (MKCL_SYMBOLP(symbol))
    { mkcl_return_value(mangle_full_symbol_name(env, symbol, "")); }
  else
    { mkcl_return_value(mk_cl_Cnil); }
}


struct mkcl_cfun mk_si_mangle_name_cfunobj = MKCL_CFUN1(mk_si_mangle_name, (mkcl_object) &MK_SI_mangle_name);

mkcl_object mk_si_mangle_name(MKCL, mkcl_object symbol)
{
  mkcl_call_stack_check(env);
  if (MKCL_SYMBOLP(symbol))
    {
      mkcl_object output = symbol->symbol.C_name;

      if (!mkcl_Null(output))
	{ mkcl_return_2_values(mk_cl_Ct, output); }
      else
	{
	  output = mangle_full_symbol_name(env, symbol, "_symbol");
	  mkcl_return_2_values(mk_cl_Cnil, output);
	}
    }
  else
    mkcl_return_value(mk_cl_Cnil);
}

struct mkcl_cfun mk_si_mangle_function_name_cfunobj = MKCL_CFUN1(mk_si_mangle_function_name, (mkcl_object) &MK_SI_mangle_function_name);

mkcl_object mk_si_mangle_function_name(MKCL, mkcl_object symbol)
{
  mkcl_object found = mk_cl_Cnil;
  mkcl_object maxarg = MKCL_MAKE_FIXNUM(MKCL_CALL_ARGUMENTS_LIMIT);
  mkcl_object minarg = MKCL_MAKE_FIXNUM(0);

  mkcl_call_stack_check(env);
  if (mkcl_Null(symbol)) symbol = ((mkcl_object) &mk_cl_Cnil_symbol);
  else if (!MKCL_SYMBOLP(symbol)) { mkcl_return_2_values(mk_cl_Cnil, mk_cl_Cnil); }

  mkcl_object fun = symbol->symbol.gfdef;

  if (mkcl_type_of(fun) == mkcl_t_cfun)
    {
      mkcl_narg narg = fun->cfun.narg;
      const char * _C_name = fun->cfun._C_name;
      mkcl_object output = fun->cfun.C_name;

      if (mkcl_Null(output))
	{
	  if (_C_name)
	    {
	      output = mkcl_make_simple_base_string(env, (char *) _C_name);
	      fun->cfun.C_name = output;
	      found = mk_cl_Ct;
	      if (narg >= 0)
		minarg = maxarg = MKCL_MAKE_FIXNUM(narg);
	    }
	  else
	    found = mk_cl_Cnil;
	}
      else
	{
	  found = mk_cl_Ct;
	  if (narg >= 0)
	    minarg = maxarg = MKCL_MAKE_FIXNUM(narg);
	}

      mkcl_return_4_values(found, output, minarg, maxarg);
    }
  
  mkcl_object output = mangle_full_symbol_name(env, symbol, "");
  mkcl_return_4_values(found, output, minarg, maxarg);
}


