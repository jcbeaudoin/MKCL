/* -*- mode: c  -*- */

#include <mkcl/mkcl.h>
#include <ctype.h>
#include <string.h>
#include <mkcl/internal.h>

#define PACKAGE_FIELD_BIT_OFFSET 3

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
#define FORM_SYMBOL 3
#define SYMBOL_TYPE_MASK 3

#define EXPORT 4
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

#define mkcl_root_symbol_inits mkcl_root_symbols_to_c_fun_name_map
#define mkcl_symbol_initializer mkcl_symbol_to_c_fun_name
#include "symbols_list2.h"
#undef mkcl_root_symbol_inits
#undef mkcl_symbol_initializer

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

mkcl_object mk_si_mangle_symbol(MKCL, mkcl_object symbol)
{
  mkcl_call_stack_check(env);
  if (MKCL_SYMBOLP(symbol))
    { mkcl_return_value(mangle_full_symbol_name(env, symbol, "")); }
  else
    { mkcl_return_value(mk_cl_Cnil); }
}


mkcl_object mk_si_mangle_name(MKCL, mkcl_object symbol)
{
  mkcl_call_stack_check(env);
  if (symbol == mk_cl_Cnil)
    { mkcl_return_2_values(mk_cl_Ct, mkcl_make_simple_base_string(env, "mk_cl_Cnil")); }
  else if (symbol == mk_cl_Ct)
    { mkcl_return_2_values(mk_cl_Ct, mkcl_make_simple_base_string(env, "mk_cl_Ct")); }

  mkcl_index p  = (struct mkcl_symbol *)symbol - mkcl_root_symbols;

  if (/* p >= 0 && */ p <= mkcl_root_symbols_count) {
    mkcl_object output = mk_cl_format(env, 4, mk_cl_Cnil,
				      mkcl_make_simple_base_string(env, "MKCL_SYM(~S,~D)"),
				      mkcl_symbol_name(env, symbol), MKCL_MAKE_FIXNUM(p));
    mkcl_return_2_values(mk_cl_Ct, output);
  }

  mkcl_object output = mangle_full_symbol_name(env, symbol, "_symbol");  
  mkcl_return_2_values(mk_cl_Cnil, output);
}

mkcl_object mk_si_mangle_function_name(MKCL, mkcl_object symbol)
{
  mkcl_object found = mk_cl_Cnil;
  mkcl_object maxarg = MKCL_MAKE_FIXNUM(MKCL_CALL_ARGUMENTS_LIMIT);
  mkcl_object minarg = MKCL_MAKE_FIXNUM(0);

  mkcl_call_stack_check(env);
  if (mkcl_Null(symbol)) symbol = mk_cl_Cnil_symbol;
  else if (!MKCL_SYMBOLP(symbol)) { mkcl_return_2_values(mk_cl_Cnil, mk_cl_Cnil); }

  if (((mkcl_object) &(mkcl_root_symbols[0])) <= symbol
      && symbol < ((mkcl_object) &(mkcl_root_symbols[NB_STATIC_SYMBOLS-1])))
    {
      mkcl_index i = (struct mkcl_symbol *)symbol - mkcl_root_symbols;

      const char * c_name = mkcl_root_symbols_to_c_fun_name_map[i].translation;
      int narg = mkcl_root_symbols_to_c_fun_name_map[i].narg;
      mkcl_object output = mk_cl_Cnil;

      if (c_name)
	{
	  found = mk_cl_Ct;
	  output = mkcl_make_simple_base_string(env, (char *) c_name);
	  if (narg >= 0)
	    minarg = maxarg = MKCL_MAKE_FIXNUM(narg);
	}
      else
	{
	  found = output = mk_cl_Cnil;
	}
      mkcl_return_4_values(found, output, minarg, maxarg);
    }
  
  mkcl_object output = mangle_full_symbol_name(env, symbol, "");
  mkcl_return_4_values(found, output, minarg, maxarg);
}


static void
make_this_symbol(MKCL, int i, struct mkcl_symbol * symbol, int code, const char *name,
		 mkcl_objectfn fun, int narg, mkcl_object value)
{
  enum mkcl_stype stp;
  mkcl_object package;
  bool form = FALSE;
  bool export = FALSE;

  switch (code & SYMBOL_TYPE_MASK) {
  case ORDINARY_SYMBOL: stp = mkcl_stp_ordinary; break;
  case SPECIAL_SYMBOL: stp = mkcl_stp_special; break;
  case CONSTANT_SYMBOL: stp = mkcl_stp_constant; break;
  case FORM_SYMBOL: form = 1; stp = mkcl_stp_ordinary; break;
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
  case FFI_PACKAGE: package = mkcl_core.ffi_package; break;
  default: mkcl_lose(env, "Unknown package in make_this_symbol");
  }

  {
    mkcl_object _name = mkcl_make_simple_base_string(env, (char *) name);
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
    symbol->name = _name;
    symbol->hashed_name = mkcl_hash_base_string(_name->base_string.self, _name->base_string.fillp, 0);
  }

  if (package == mkcl_core.keyword_package) {
    mkcl_package_sethash_new(env, symbol->name, package->pack.external, (mkcl_object) symbol, symbol->hashed_name);
    symbol->value = (mkcl_object) symbol;
  } else {
    int intern_flag;
    symbol->value = value;
    if (mkcl_find_symbol(env, symbol->name, package, &intern_flag) != mk_cl_Cnil
	&& intern_flag == MKCL_SYMBOL_IS_INHERITED) {
      mkcl_shadowing_import(env, (mkcl_object) symbol, package);
    } else {
      mkcl_import2(env, (mkcl_object) symbol, package);
    }
    if (export)
      mkcl_export2(env, (mkcl_object) symbol, package);
  }
  if (form) {
    symbol->stype |= mkcl_stp_special_form;
  } else if (fun) {
    mkcl_object f;
    if (narg >= 0) {
      f = mkcl_make_cfun(env, (mkcl_objectfn_fixed) fun, (mkcl_object) symbol, MKCL_OBJNULL, narg, NULL);
    } else {
      f = mkcl_make_cfun_va(env, fun, (mkcl_object) symbol, MKCL_OBJNULL, NULL);
    }
    symbol->gfdef = f;
  }
}

void
mkcl_init_all_symbols(MKCL)
{
  int i, code, narg;
  const char *name;
  mkcl_object s, value;
  mkcl_objectfn fun;

  /* We skip NIL and T, thus we start at 2. */
  for (i = 2; mkcl_root_symbol_inits[i].name != NULL; i++) {
    struct mkcl_symbol * s = &(mkcl_root_symbols[i]);
    code = mkcl_root_symbol_inits[i].type;
    name = mkcl_root_symbol_inits[i].name;
    fun = (mkcl_objectfn)mkcl_root_symbol_inits[i].fun;
    narg = mkcl_root_symbol_inits[i].narg;
    value = mkcl_root_symbol_inits[i].value;
    make_this_symbol(env, i, s, code, name, fun, narg, value);
  }
}

