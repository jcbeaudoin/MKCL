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

#define PACKAGE_FIELD_BIT_OFFSET 5

#define CL_PACKAGE        (0 << PACKAGE_FIELD_BIT_OFFSET)
#define SI_PACKAGE        (1 << PACKAGE_FIELD_BIT_OFFSET)
#define KEYWORD_PACKAGE   (2 << PACKAGE_FIELD_BIT_OFFSET)
#define MT_PACKAGE        (3 << PACKAGE_FIELD_BIT_OFFSET)
#define CLOS_PACKAGE      (4 << PACKAGE_FIELD_BIT_OFFSET)
#define MKCL_EXT_PACKAGE  (5 << PACKAGE_FIELD_BIT_OFFSET)
#define FFI_PACKAGE       (6 << PACKAGE_FIELD_BIT_OFFSET)
#define GRAY_PACKAGE      (8 << PACKAGE_FIELD_BIT_OFFSET)

#define ORDINARY_SYMBOL 0
#define CONSTANT_SYMBOL 1
#define SPECIAL_SYMBOL 2
#define FORM_SYMBOL 4
#define MACRO_SYMBOL 8
#define SYMBOL_TYPE_MASK 15

#define EXPORT 16
#define EXPORT_MASK EXPORT

#define CL_ORDINARY	  CL_PACKAGE | EXPORT | ORDINARY_SYMBOL
#define CL_SPECIAL	  CL_PACKAGE | EXPORT | SPECIAL_SYMBOL
#define CL_CONSTANT	  CL_PACKAGE | EXPORT | CONSTANT_SYMBOL
#define CL_FORM		  CL_PACKAGE | EXPORT | ORDINARY_SYMBOL | FORM_SYMBOL
#define SI_ORDINARY	  SI_PACKAGE | EXPORT | ORDINARY_SYMBOL
#define SI_SPECIAL	  SI_PACKAGE | EXPORT | SPECIAL_SYMBOL
#define SI_CONSTANT	  SI_PACKAGE | EXPORT | CONSTANT_SYMBOL
#define MKCL_EXT_ORDINARY MKCL_EXT_PACKAGE | EXPORT | ORDINARY_SYMBOL
#define MKCL_EXT_SPECIAL  MKCL_EXT_PACKAGE | EXPORT | SPECIAL_SYMBOL
#define MKCL_EXT_CONSTANT MKCL_EXT_PACKAGE | EXPORT | CONSTANT_SYMBOL
#define MKCL_EXT_FORM	  MKCL_EXT_PACKAGE | EXPORT | ORDINARY_SYMBOL | FORM_SYMBOL
#define MT_ORDINARY	  MT_PACKAGE | EXPORT | ORDINARY_SYMBOL
#define MT_SPECIAL	  MT_PACKAGE | EXPORT | SPECIAL_SYMBOL
#define MT_CONSTANT	  MT_PACKAGE | EXPORT | CONSTANT_SYMBOL
#define FFI_ORDINARY	  FFI_PACKAGE | EXPORT | ORDINARY_SYMBOL
#define FFI_SPECIAL	  FFI_PACKAGE | EXPORT | SPECIAL_SYMBOL
#define FFI_CONSTANT	  FFI_PACKAGE | EXPORT | CONSTANT_SYMBOL
#define CLOS_ORDINARY	  CLOS_PACKAGE | EXPORT | ORDINARY_SYMBOL
#define CLOS_SPECIAL	  CLOS_PACKAGE | EXPORT | SPECIAL_SYMBOL
#define KEYWORD		  KEYWORD_PACKAGE | EXPORT | CONSTANT_SYMBOL
#define GRAY_ORDINARY	  GRAY_PACKAGE | EXPORT | ORDINARY_SYMBOL

#define SI_INTERNAL_ORDINARY	  SI_PACKAGE | ORDINARY_SYMBOL
#define SI_INTERNAL_SPECIAL	  SI_PACKAGE | SPECIAL_SYMBOL
#define SI_INTERNAL_CONSTANT	  SI_PACKAGE | CONSTANT_SYMBOL
#define FFI_INTERNAL_ORDINARY	  FFI_PACKAGE | ORDINARY_SYMBOL
#define FFI_INTERNAL_SPECIAL	  FFI_PACKAGE | SPECIAL_SYMBOL
#define MT_INTERNAL_ORDINARY	  MT_PACKAGE | ORDINARY_SYMBOL
#define CLOS_INTERNAL_ORDINARY	  CLOS_PACKAGE | ORDINARY_SYMBOL
#define CLOS_INTERNAL_SPECIAL	  CLOS_PACKAGE | SPECIAL_SYMBOL
#define GRAY_INTERNAL_ORDINARY    GRAY_PACKAGE | ORDINARY_SYMBOL

#define PACKAGE_MASK      (-1 << PACKAGE_FIELD_BIT_OFFSET)

#include "symbols_list.h"

#define NB_STATIC_SYMBOLS (sizeof(mkcl_root_symbol_inits)/sizeof(mkcl_root_symbol_inits[0]))
const mkcl_index mkcl_root_symbols_count = NB_STATIC_SYMBOLS - 1;

struct mkcl_symbol mkcl_root_symbols[NB_STATIC_SYMBOLS] = { 0 };


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

struct mkcl_cfun mk_si_mangle_string_cfunobj = MKCL_CFUN1(mk_si_mangle_string, MK_SI_mangle_string);

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

struct mkcl_cfun mk_si_mangle_symbol_cfunobj = MKCL_CFUN1(mk_si_mangle_symbol, MK_SI_mangle_symbol);

mkcl_object mk_si_mangle_symbol(MKCL, mkcl_object symbol)
{
  mkcl_call_stack_check(env);
  if (MKCL_SYMBOLP(symbol))
    { mkcl_return_value(mangle_full_symbol_name(env, symbol, "")); }
  else
    { mkcl_return_value(mk_cl_Cnil); }
}


struct mkcl_cfun mk_si_mangle_name_cfunobj = MKCL_CFUN1(mk_si_mangle_name, MK_SI_mangle_name);

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

struct mkcl_cfun mk_si_mangle_function_name_cfunobj = MKCL_CFUN1(mk_si_mangle_function_name, MK_SI_mangle_function_name);

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

#ifndef MKCL_MIN

struct mkcl_cfun mk_cl_adjust_array_cfunobj = MKCL_CFUN_VA(mk_cl_adjust_array, MK_CL_adjust_array);
struct mkcl_cfun mk_cl_apropos_cfunobj = MKCL_CFUN_VA(mk_cl_apropos, MK_CL_apropos);
struct mkcl_cfun mk_cl_apropos_list_cfunobj = MKCL_CFUN_VA(mk_cl_apropos_list, MK_CL_apropos_list);
struct mkcl_cfun mk_cl_array_in_bounds_p_cfunobj = MKCL_CFUN_VA(mk_cl_array_in_bounds_p, MK_CL_array_in_bounds_p);
struct mkcl_cfun mk_cl_assoc_if_cfunobj = MKCL_CFUN_VA(mk_cl_assoc_if, MK_CL_assoc_if);
struct mkcl_cfun mk_cl_assoc_if_not_cfunobj = MKCL_CFUN_VA(mk_cl_assoc_if_not, MK_CL_assoc_if_not);
struct mkcl_cfun mk_cl_bit_cfunobj = MKCL_CFUN_VA(mk_cl_bit, MK_CL_bit);
struct mkcl_cfun mk_cl_bit_and_cfunobj = MKCL_CFUN_VA(mk_cl_bit_and, MK_CL_bit_and);
struct mkcl_cfun mk_cl_bit_andc1_cfunobj = MKCL_CFUN_VA(mk_cl_bit_andc1, MK_CL_bit_andc1);
struct mkcl_cfun mk_cl_bit_andc2_cfunobj = MKCL_CFUN_VA(mk_cl_bit_andc2, MK_CL_bit_andc2);
struct mkcl_cfun mk_cl_bit_eqv_cfunobj = MKCL_CFUN_VA(mk_cl_bit_eqv, MK_CL_bit_eqv);
struct mkcl_cfun mk_cl_bit_ior_cfunobj = MKCL_CFUN_VA(mk_cl_bit_ior, MK_CL_bit_ior);
struct mkcl_cfun mk_cl_bit_nand_cfunobj = MKCL_CFUN_VA(mk_cl_bit_nand, MK_CL_bit_nand);
struct mkcl_cfun mk_cl_bit_nor_cfunobj = MKCL_CFUN_VA(mk_cl_bit_nor, MK_CL_bit_nor);
struct mkcl_cfun mk_cl_bit_not_cfunobj = MKCL_CFUN_VA(mk_cl_bit_not, MK_CL_bit_not);
struct mkcl_cfun mk_cl_bit_orc1_cfunobj = MKCL_CFUN_VA(mk_cl_bit_orc1, MK_CL_bit_orc1);
struct mkcl_cfun mk_cl_bit_orc2_cfunobj = MKCL_CFUN_VA(mk_cl_bit_orc2, MK_CL_bit_orc2);
struct mkcl_cfun mk_cl_bit_xor_cfunobj = MKCL_CFUN_VA(mk_cl_bit_xor, MK_CL_bit_xor);
struct mkcl_cfun mk_cl_cerror_cfunobj = MKCL_CFUN_VA(mk_cl_cerror, MK_CL_cerror);
struct mkcl_cfun mk_cl_concatenate_cfunobj = MKCL_CFUN_VA(mk_cl_concatenate, MK_CL_concatenate);
struct mkcl_cfun mk_cl_continue_cfunobj = MKCL_CFUN_VA(mk_cl_continue, MK_CL_continue);
struct mkcl_cfun mk_cl_copy_pprint_dispatch_cfunobj = MKCL_CFUN_VA(mk_cl_copy_pprint_dispatch, MK_CL_copy_pprint_dispatch);
struct mkcl_cfun mk_cl_count_cfunobj = MKCL_CFUN_VA(mk_cl_count, MK_CL_count);
struct mkcl_cfun mk_cl_count_if_cfunobj = MKCL_CFUN_VA(mk_cl_count_if, MK_CL_count_if);
struct mkcl_cfun mk_cl_count_if_not_cfunobj = MKCL_CFUN_VA(mk_cl_count_if_not, MK_CL_count_if_not);
struct mkcl_cfun mk_cl_decode_universal_time_cfunobj = MKCL_CFUN_VA(mk_cl_decode_universal_time, MK_CL_decode_universal_time);
struct mkcl_cfun mk_cl_delete_cfunobj = MKCL_CFUN_VA(mk_cl_delete, MK_CL_delete);
struct mkcl_cfun mk_cl_delete_duplicates_cfunobj = MKCL_CFUN_VA(mk_cl_delete_duplicates, MK_CL_delete_duplicates);
struct mkcl_cfun mk_cl_delete_if_cfunobj = MKCL_CFUN_VA(mk_cl_delete_if, MK_CL_delete_if);
struct mkcl_cfun mk_cl_delete_if_not_cfunobj = MKCL_CFUN_VA(mk_cl_delete_if_not, MK_CL_delete_if_not);
struct mkcl_cfun mk_cl_encode_universal_time_cfunobj = MKCL_CFUN_VA(mk_cl_encode_universal_time, MK_CL_encode_universal_time);
struct mkcl_cfun mk_cl_ensure_directories_exist_cfunobj = MKCL_CFUN_VA(mk_cl_ensure_directories_exist, MK_CL_ensure_directories_exist);
struct mkcl_cfun mk_cl_every_cfunobj = MKCL_CFUN_VA(mk_cl_every, MK_CL_every);
struct mkcl_cfun mk_cl_fceiling_cfunobj = MKCL_CFUN_VA(mk_cl_fceiling, MK_CL_fceiling);
struct mkcl_cfun mk_cl_ffloor_cfunobj = MKCL_CFUN_VA(mk_cl_ffloor, MK_CL_ffloor);
struct mkcl_cfun mk_cl_fill_cfunobj = MKCL_CFUN_VA(mk_cl_fill, MK_CL_fill);
struct mkcl_cfun mk_cl_find_cfunobj = MKCL_CFUN_VA(mk_cl_find, MK_CL_find);
struct mkcl_cfun mk_cl_find_if_cfunobj = MKCL_CFUN_VA(mk_cl_find_if, MK_CL_find_if);
struct mkcl_cfun mk_cl_find_if_not_cfunobj = MKCL_CFUN_VA(mk_cl_find_if_not, MK_CL_find_if_not);
struct mkcl_cfun mk_cl_fround_cfunobj = MKCL_CFUN_VA(mk_cl_fround, MK_CL_fround);
struct mkcl_cfun mk_cl_ftruncate_cfunobj = MKCL_CFUN_VA(mk_cl_ftruncate, MK_CL_ftruncate);
struct mkcl_cfun mk_cl_intersection_cfunobj = MKCL_CFUN_VA(mk_cl_intersection, MK_CL_intersection);
struct mkcl_cfun mk_cl_make_array_cfunobj = MKCL_CFUN_VA(mk_cl_make_array, MK_CL_make_array);
struct mkcl_cfun mk_cl_make_sequence_cfunobj = MKCL_CFUN_VA(mk_cl_make_sequence, MK_CL_make_sequence);
struct mkcl_cfun mk_cl_map_cfunobj = MKCL_CFUN_VA(mk_cl_map, MK_CL_map);
struct mkcl_cfun mk_cl_map_into_cfunobj = MKCL_CFUN_VA(mk_cl_map_into, MK_CL_map_into);
struct mkcl_cfun mk_cl_member_if_cfunobj = MKCL_CFUN_VA(mk_cl_member_if, MK_CL_member_if);
struct mkcl_cfun mk_cl_member_if_not_cfunobj = MKCL_CFUN_VA(mk_cl_member_if_not, MK_CL_member_if_not);
struct mkcl_cfun mk_cl_merge_cfunobj = MKCL_CFUN_VA(mk_cl_merge, MK_CL_merge);
struct mkcl_cfun mk_cl_mismatch_cfunobj = MKCL_CFUN_VA(mk_cl_mismatch, MK_CL_mismatch);
struct mkcl_cfun mk_cl_nintersection_cfunobj = MKCL_CFUN_VA(mk_cl_nintersection, MK_CL_nintersection);
struct mkcl_cfun mk_cl_notany_cfunobj = MKCL_CFUN_VA(mk_cl_notany, MK_CL_notany);
struct mkcl_cfun mk_cl_notevery_cfunobj = MKCL_CFUN_VA(mk_cl_notevery, MK_CL_notevery);
struct mkcl_cfun mk_cl_nset_difference_cfunobj = MKCL_CFUN_VA(mk_cl_nset_difference, MK_CL_nset_difference);
struct mkcl_cfun mk_cl_nset_exclusive_or_cfunobj = MKCL_CFUN_VA(mk_cl_nset_exclusive_or, MK_CL_nset_exclusive_or);
struct mkcl_cfun mk_cl_nsubst_if_cfunobj = MKCL_CFUN_VA(mk_cl_nsubst_if, MK_CL_nsubst_if);
struct mkcl_cfun mk_cl_nsubst_if_not_cfunobj = MKCL_CFUN_VA(mk_cl_nsubst_if_not, MK_CL_nsubst_if_not);
struct mkcl_cfun mk_cl_nsubstitute_cfunobj = MKCL_CFUN_VA(mk_cl_nsubstitute, MK_CL_nsubstitute);
struct mkcl_cfun mk_cl_nsubstitute_if_cfunobj = MKCL_CFUN_VA(mk_cl_nsubstitute_if, MK_CL_nsubstitute_if);
struct mkcl_cfun mk_cl_nsubstitute_if_not_cfunobj = MKCL_CFUN_VA(mk_cl_nsubstitute_if_not, MK_CL_nsubstitute_if_not);
struct mkcl_cfun mk_cl_nunion_cfunobj = MKCL_CFUN_VA(mk_cl_nunion, MK_CL_nunion);
struct mkcl_cfun mk_cl_position_cfunobj = MKCL_CFUN_VA(mk_cl_position, MK_CL_position);
struct mkcl_cfun mk_cl_position_if_cfunobj = MKCL_CFUN_VA(mk_cl_position_if, MK_CL_position_if);
struct mkcl_cfun mk_cl_position_if_not_cfunobj = MKCL_CFUN_VA(mk_cl_position_if_not, MK_CL_position_if_not);
struct mkcl_cfun mk_cl_pprint_dispatch_cfunobj = MKCL_CFUN_VA(mk_cl_pprint_dispatch, MK_CL_pprint_dispatch);
struct mkcl_cfun mk_cl_pprint_fill_cfunobj = MKCL_CFUN_VA(mk_cl_pprint_fill, MK_CL_pprint_fill);
struct mkcl_cfun mk_cl_pprint_indent_cfunobj = MKCL_CFUN_VA(mk_cl_pprint_indent, MK_CL_pprint_indent);
struct mkcl_cfun mk_cl_pprint_linear_cfunobj = MKCL_CFUN_VA(mk_cl_pprint_linear, MK_CL_pprint_linear);
struct mkcl_cfun mk_cl_pprint_newline_cfunobj = MKCL_CFUN_VA(mk_cl_pprint_newline, MK_CL_pprint_newline);
struct mkcl_cfun mk_cl_pprint_tab_cfunobj = MKCL_CFUN_VA(mk_cl_pprint_tab, MK_CL_pprint_tab);
struct mkcl_cfun mk_cl_pprint_tabular_cfunobj = MKCL_CFUN_VA(mk_cl_pprint_tabular, MK_CL_pprint_tabular);
struct mkcl_cfun mk_cl_rassoc_if_cfunobj = MKCL_CFUN_VA(mk_cl_rassoc_if, MK_CL_rassoc_if);
struct mkcl_cfun mk_cl_rassoc_if_not_cfunobj = MKCL_CFUN_VA(mk_cl_rassoc_if_not, MK_CL_rassoc_if_not);
struct mkcl_cfun mk_cl_read_from_string_cfunobj = MKCL_CFUN_VA(mk_cl_read_from_string, MK_CL_read_from_string);
struct mkcl_cfun mk_cl_reduce_cfunobj = MKCL_CFUN_VA(mk_cl_reduce, MK_CL_reduce);
struct mkcl_cfun mk_cl_remove_cfunobj = MKCL_CFUN_VA(mk_cl_remove, MK_CL_remove);
struct mkcl_cfun mk_cl_remove_duplicates_cfunobj = MKCL_CFUN_VA(mk_cl_remove_duplicates, MK_CL_remove_duplicates);
struct mkcl_cfun mk_cl_remove_if_cfunobj = MKCL_CFUN_VA(mk_cl_remove_if, MK_CL_remove_if);
struct mkcl_cfun mk_cl_remove_if_not_cfunobj = MKCL_CFUN_VA(mk_cl_remove_if_not, MK_CL_remove_if_not);
struct mkcl_cfun mk_cl_replace_cfunobj = MKCL_CFUN_VA(mk_cl_replace, MK_CL_replace);
struct mkcl_cfun mk_cl_require_cfunobj = MKCL_CFUN_VA(mk_cl_require, MK_CL_require);
struct mkcl_cfun mk_cl_sbit_cfunobj = MKCL_CFUN_VA(mk_cl_sbit, MK_CL_sbit);
struct mkcl_cfun mk_cl_search_cfunobj = MKCL_CFUN_VA(mk_cl_search, MK_CL_search);
struct mkcl_cfun mk_cl_set_difference_cfunobj = MKCL_CFUN_VA(mk_cl_set_difference, MK_CL_set_difference);
struct mkcl_cfun mk_cl_set_exclusive_or_cfunobj = MKCL_CFUN_VA(mk_cl_set_exclusive_or, MK_CL_set_exclusive_or);
struct mkcl_cfun mk_cl_set_pprint_dispatch_cfunobj = MKCL_CFUN_VA(mk_cl_set_pprint_dispatch, MK_CL_set_pprint_dispatch);
struct mkcl_cfun mk_cl_signum_cfunobj = MKCL_CFUN1(mk_cl_signum, MK_CL_signum);
struct mkcl_cfun mk_cl_some_cfunobj = MKCL_CFUN_VA(mk_cl_some, MK_CL_some);
struct mkcl_cfun mk_cl_sort_cfunobj = MKCL_CFUN_VA(mk_cl_sort, MK_CL_sort);
struct mkcl_cfun mk_cl_stable_sort_cfunobj = MKCL_CFUN_VA(mk_cl_stable_sort, MK_CL_stable_sort);
struct mkcl_cfun mk_cl_subsetp_cfunobj = MKCL_CFUN_VA(mk_cl_subsetp, MK_CL_subsetp);
struct mkcl_cfun mk_cl_subst_if_cfunobj = MKCL_CFUN_VA(mk_cl_subst_if, MK_CL_subst_if);
struct mkcl_cfun mk_cl_subst_if_not_cfunobj = MKCL_CFUN_VA(mk_cl_subst_if_not, MK_CL_subst_if_not);
struct mkcl_cfun mk_cl_substitute_cfunobj = MKCL_CFUN_VA(mk_cl_substitute, MK_CL_substitute);
struct mkcl_cfun mk_cl_substitute_if_cfunobj = MKCL_CFUN_VA(mk_cl_substitute_if, MK_CL_substitute_if);
struct mkcl_cfun mk_cl_substitute_if_not_cfunobj = MKCL_CFUN_VA(mk_cl_substitute_if_not, MK_CL_substitute_if_not);
struct mkcl_cfun mk_cl_subtypep_cfunobj = MKCL_CFUN_VA(mk_cl_subtypep, MK_CL_subtypep);
struct mkcl_cfun mk_cl_typep_cfunobj = MKCL_CFUN_VA(mk_cl_typep, MK_CL_typep);
struct mkcl_cfun mk_cl_union_cfunobj = MKCL_CFUN_VA(mk_cl_union, MK_CL_union);
struct mkcl_cfun mk_cl_upgraded_array_element_type_cfunobj = MKCL_CFUN_VA(mk_cl_upgraded_array_element_type, MK_CL_upgraded_array_element_type);
struct mkcl_cfun mk_cl_upgraded_complex_part_type_cfunobj = MKCL_CFUN_VA(mk_cl_upgraded_complex_part_type, MK_CL_upgraded_complex_part_type);
struct mkcl_cfun mk_cl_vector_cfunobj = MKCL_CFUN_VA(mk_cl_vector, MK_CL_vector);
struct mkcl_cfun mk_cl_vector_push_extend_cfunobj = MKCL_CFUN_VA(mk_cl_vector_push_extend, MK_CL_vector_push_extend);
struct mkcl_cfun mk_cl_write_to_string_cfunobj = MKCL_CFUN_VA(mk_cl_write_to_string, MK_CL_write_to_string);
struct mkcl_cfun mk_cl_y_or_n_p_cfunobj = MKCL_CFUN_VA(mk_cl_y_or_n_p, MK_CL_y_or_n_p);
struct mkcl_cfun mk_cl_yes_or_no_p_cfunobj = MKCL_CFUN_VA(mk_cl_yes_or_no_p, MK_CL_yes_or_no_p);
struct mkcl_cfun mk_cl_invalid_method_error_cfunobj = MKCL_CFUN_VA(mk_cl_invalid_method_error, MK_CL_invalid_method_error);
struct mkcl_cfun mk_cl_method_combination_error_cfunobj = MKCL_CFUN_VA(mk_cl_method_combination_error, MK_CL_method_combination_error);
struct mkcl_cfun mk_si_package_children_cfunobj = MKCL_CFUN_VA(mk_si_package_children, MK_SI_package_children);
struct mkcl_cfun mk_mkcl_prin1_to_base_string_cfunobj = MKCL_CFUN_VA(mk_mkcl_prin1_to_base_string, MK_MKCL_prin1_to_base_string);
struct mkcl_cfun mk_mkcl_princ_to_base_string_cfunobj = MKCL_CFUN_VA(mk_mkcl_princ_to_base_string, MK_MKCL_princ_to_base_string);
struct mkcl_cfun mk_mkcl_write_to_base_string_cfunobj = MKCL_CFUN_VA(mk_mkcl_write_to_base_string, MK_mkcl_write_to_base_string);
struct mkcl_cfun mk_mkcl_run_program_cfunobj = MKCL_CFUN_VA(mk_mkcl_run_program, MK_MKCL_run_program);

struct mkcl_cfun mk_cl_acos_cfunobj = MKCL_CFUN1(mk_cl_acos, MK_CL_acos);
struct mkcl_cfun mk_cl_acosh_cfunobj = MKCL_CFUN1(mk_cl_acosh, MK_CL_acosh);
struct mkcl_cfun mk_cl_array_dimensions_cfunobj = MKCL_CFUN1(mk_cl_array_dimensions, MK_CL_array_dimensions);
struct mkcl_cfun mk_cl_asin_cfunobj = MKCL_CFUN1(mk_cl_asin, MK_CL_asin);
struct mkcl_cfun mk_cl_asinh_cfunobj = MKCL_CFUN1(mk_cl_asinh, MK_CL_asinh);
struct mkcl_cfun mk_cl_atanh_cfunobj = MKCL_CFUN1(mk_cl_atanh, MK_CL_atanh);
struct mkcl_cfun mk_cl_byte_cfunobj = MKCL_CFUN2(mk_cl_byte, MK_CL_byte);
struct mkcl_cfun mk_cl_byte_position_cfunobj = MKCL_CFUN1(mk_cl_byte_position, MK_CL_byte_position);
struct mkcl_cfun mk_cl_byte_size_cfunobj = MKCL_CFUN1(mk_cl_byte_size, MK_CL_byte_size);
struct mkcl_cfun mk_cl_cis_cfunobj = MKCL_CFUN1(mk_cl_cis, MK_CL_cis);
struct mkcl_cfun mk_cl_coerce_cfunobj = MKCL_CFUN2(mk_cl_coerce, MK_CL_coerce);
struct mkcl_cfun mk_cl_complement_cfunobj = MKCL_CFUN1(mk_cl_complement, MK_CL_complement);
struct mkcl_cfun mk_cl_constantly_cfunobj = MKCL_CFUN1(mk_cl_constantly, MK_CL_constantly);
struct mkcl_cfun mk_cl_deposit_field_cfunobj = MKCL_CFUN3(mk_cl_deposit_field, MK_CL_deposit_field);
struct mkcl_cfun mk_cl_dpb_cfunobj = MKCL_CFUN3(mk_cl_dpb, MK_CL_dpb);
struct mkcl_cfun mk_cl_find_all_symbols_cfunobj = MKCL_CFUN1(mk_cl_find_all_symbols, MK_CL_find_all_symbols);
struct mkcl_cfun mk_cl_get_decoded_time_cfunobj = MKCL_CFUN0(mk_cl_get_decoded_time, MK_CL_get_decoded_time);
struct mkcl_cfun mk_cl_isqrt_cfunobj = MKCL_CFUN1(mk_cl_isqrt, MK_CL_isqrt);
struct mkcl_cfun mk_cl_ldb_cfunobj = MKCL_CFUN2(mk_cl_ldb, MK_CL_ldb);
struct mkcl_cfun mk_cl_ldb_test_cfunobj = MKCL_CFUN2(mk_cl_ldb_test, MK_CL_ldb_test);
struct mkcl_cfun mk_cl_load_logical_pathname_translations_cfunobj = MKCL_CFUN1(mk_cl_load_logical_pathname_translations, MK_CL_load_logical_pathname_translations);
struct mkcl_cfun mk_cl_logical_pathname_translations_cfunobj = MKCL_CFUN1(mk_cl_logical_pathname_translations, MK_CL_logical_pathname_translations);
struct mkcl_cfun mk_cl_logtest_cfunobj = MKCL_CFUN2(mk_cl_logtest, MK_CL_logtest);
struct mkcl_cfun mk_cl_mask_field_cfunobj = MKCL_CFUN2(mk_cl_mask_field, MK_CL_mask_field);
struct mkcl_cfun mk_cl_phase_cfunobj = MKCL_CFUN1(mk_cl_phase, MK_CL_phase);
struct mkcl_cfun mk_cl_prin1_to_string_cfunobj = MKCL_CFUN1(mk_cl_prin1_to_string, MK_CL_prin1_to_string);
struct mkcl_cfun mk_cl_princ_to_string_cfunobj = MKCL_CFUN1(mk_cl_princ_to_string, MK_CL_princ_to_string);
struct mkcl_cfun mk_cl_provide_cfunobj = MKCL_CFUN1(mk_cl_provide, MK_CL_provide);
struct mkcl_cfun mk_cl_vector_pop_cfunobj = MKCL_CFUN1(mk_cl_vector_pop, MK_CL_vector_pop);
struct mkcl_cfun mk_cl_vector_push_cfunobj = MKCL_CFUN2(mk_cl_vector_push, MK_CL_vector_push);
struct mkcl_cfun mk_si_safe_eval_cfunobj = MKCL_CFUN3(mk_si_safe_eval, MK_SI_safe_eval);
struct mkcl_cfun mk_si_subclassp_cfunobj = MKCL_CFUN2(mk_si_subclassp, MK_SI_subclassp);
struct mkcl_cfun mk_si_of_class_p_cfunobj = MKCL_CFUN2(mk_si_of_class_p, MK_SI_of_class_p);
struct mkcl_cfun mk_si_do_deftype_cfunobj = MKCL_CFUN3(mk_si_do_deftype, MK_SI_do_deftype);
struct mkcl_cfun mk_si_find_relative_package_cfunobj = MKCL_CFUN1(mk_si_find_relative_package, MK_SI_find_relative_package);
struct mkcl_cfun mk_si_package_parent_cfunobj = MKCL_CFUN1(mk_si_package_parent, MK_SI_package_parent);
struct mkcl_cfun mk_si_top_apply_cfunobj = MKCL_CFUN2(mk_si_top_apply, MK_SI_top_apply);
struct mkcl_cfun mk_si_mkcl_version_cfunobj = MKCL_CFUN0(mk_si_mkcl_version, MK_SI_mkcl_version);
struct mkcl_cfun mk_si_mkcl_major_version_cfunobj = MKCL_CFUN0(mk_si_mkcl_major_version, MK_SI_mkcl_major_version);
struct mkcl_cfun mk_si_mkcl_minor_version_cfunobj = MKCL_CFUN0(mk_si_mkcl_minor_version, MK_SI_mkcl_minor_version);
struct mkcl_cfun mk_si_mkcl_patch_level_cfunobj = MKCL_CFUN0(mk_si_mkcl_patch_level, MK_SI_mkcl_patch_level);
struct mkcl_cfun mk_si_shutdown_mkcl_threads_cfunobj = MKCL_CFUN4(mk_si_shutdown_mkcl_threads, MK_SI_shutdown_mkcl_threads);

#endif /* def MKCL_FINAL */



static void
make_this_symbol(MKCL, int i, struct mkcl_symbol * symbol, int code, const char * _name,
		 mkcl_objectfn fun, int narg, mkcl_object value, struct mkcl_cfun * funobj)
{
  enum mkcl_stype stp;
  mkcl_object package;
  bool form = FALSE;
  bool macro = FALSE;
  bool export = FALSE;

  switch (code & SYMBOL_TYPE_MASK) {
  case ORDINARY_SYMBOL: stp = mkcl_stp_ordinary; break;
  case SPECIAL_SYMBOL: stp = mkcl_stp_special; break;
  case CONSTANT_SYMBOL: stp = mkcl_stp_constant; break;
  case FORM_SYMBOL: form = TRUE; stp = mkcl_stp_ordinary; break;
  case MACRO_SYMBOL: macro = TRUE; stp = mkcl_stp_ordinary; break;
  default: mkcl_lose(env, "Unknown symbol type in make_this_symbol");
  }
  export = code & EXPORT_MASK;
  switch (code & PACKAGE_MASK) {
  case CL_PACKAGE: package = mkcl_core.lisp_package; break;
  case SI_PACKAGE: package = mkcl_core.system_package; break;
  case MKCL_EXT_PACKAGE: package = mkcl_core.mkcl_ext_package; break;
  case KEYWORD_PACKAGE: package = mkcl_core.keyword_package; break;
  case MT_PACKAGE: package = mkcl_core.mt_package; break;
  case CLOS_PACKAGE: package = mkcl_core.clos_package; break;
  case GRAY_PACKAGE: package = mkcl_core.gray_package; break;
  case FFI_PACKAGE: package = mkcl_core.ffi_package; /* return; */ break;
  default: mkcl_lose(env, "Unknown package in make_this_symbol");
  }

  {
    const char format[] = "mkcl_root_symbols[%d]";
#if 1
    const int _C_name_size = sizeof(format) + 5; /* should be good enough for 10k symbols. JCB */
#else
    const int _C_name_size = snprintf(NULL, 0, format, i); /* safer but slower. JCB */
#endif
    char * _C_name = mkcl_alloc(env, _C_name_size);
    snprintf(_C_name, _C_name_size, format, i);
    mkcl_object name = mkcl_make_simple_base_string(env, (char *) _name);
    mkcl_object C_name = mkcl_make_simple_base_string(env, _C_name);

    symbol->t = mkcl_t_symbol;
    symbol->special_index = MKCL_NOT_A_SPECIAL_INDEX;
    symbol->value = MKCL_OBJNULL;
    symbol->gfdef = mk_cl_Cnil;
    symbol->plist = mk_cl_Cnil;
    symbol->sys_plist = mk_cl_Cnil;
    symbol->hpack = mk_cl_Cnil;
    symbol->properly_named_class = mk_cl_Cnil;
    symbol->stype = stp;
    symbol->hpack = package;
    symbol->name = name;
    symbol->_name = (char *) _name;
    symbol->hashed_name = mkcl_hash_base_string(name->base_string.self, name->base_string.fillp, 0);
    symbol->_C_name = _C_name; /* just for now. */
    symbol->C_name = C_name; /* just for now. */
  }

  if (package == mkcl_core.keyword_package) {
    mkcl_package_sethash_new(env, symbol->name, package->pack.external, (mkcl_object) symbol, symbol->hashed_name);
    symbol->value = (mkcl_object) symbol;
  } else {
    mkcl_object htable = export ? package->pack.external : package->pack.internal;

    mkcl_package_sethash_new(env, symbol->name, htable, (mkcl_object) symbol, symbol->hashed_name);
    symbol->value = value;
  }
  if (form) {
    symbol->stype |= mkcl_stp_special_form;
  } else if (funobj) {
    symbol->gfdef = (mkcl_object) funobj;
    if (macro) symbol->stype |= mkcl_stp_macro;
  }
}

void
mkcl_init_all_symbols(MKCL)
{
  int i, code, narg;
  const char *name;
  mkcl_object s, value;
  mkcl_objectfn fun;

  /* We skip NIL and T and UNBOUND and PROTECT_TAG, thus we start at 4. */
  for (i = 4; mkcl_root_symbol_inits[i].name != NULL; i++) {
    struct mkcl_symbol * s = &(mkcl_root_symbols[i]);
    struct mkcl_cfun * funobj = mkcl_root_symbol_inits[i].funobj;
    code = mkcl_root_symbol_inits[i].type;
    name = mkcl_root_symbol_inits[i].name;
    fun = (mkcl_objectfn)mkcl_root_symbol_inits[i].fun;
    narg = mkcl_root_symbol_inits[i].narg;
    value = mkcl_root_symbol_inits[i].value;
    make_this_symbol(env, i, s, code, name, fun, narg, value, funobj);
  }
}

