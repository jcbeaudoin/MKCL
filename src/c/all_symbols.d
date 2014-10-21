/* -*- mode: c  -*- */

#include <mkcl/mkcl.h>
#include <ctype.h>
#include <string.h>
#include <mkcl/internal.h>

#define CL_PACKAGE 0        /* 0 << 2 */
#define SI_PACKAGE 4        /* 1 << 2 */
#define KEYWORD_PACKAGE 8   /* 2 << 2 */
#define MT_PACKAGE 12       /* 3 << 2 */
#define CLOS_PACKAGE 16     /* 4 << 2 */
#define MKCL_EXT_PACKAGE 20 /* 5 << 2 */
#define GRAY_PACKAGE 32     /* 8 << 2 */

#define ORDINARY_SYMBOL 0
#define CONSTANT_SYMBOL 1
#define SPECIAL_SYMBOL 2
#define FORM_SYMBOL 3

#define CL_ORDINARY	  CL_PACKAGE | ORDINARY_SYMBOL
#define CL_SPECIAL	  CL_PACKAGE | SPECIAL_SYMBOL
#define CL_CONSTANT	  CL_PACKAGE | CONSTANT_SYMBOL
#define CL_FORM		  CL_PACKAGE | ORDINARY_SYMBOL | FORM_SYMBOL
#define SI_ORDINARY	  SI_PACKAGE | ORDINARY_SYMBOL
#define SI_SPECIAL	  SI_PACKAGE | SPECIAL_SYMBOL
#define SI_CONSTANT	  SI_PACKAGE | CONSTANT_SYMBOL
#define MKCL_EXT_ORDINARY MKCL_EXT_PACKAGE | ORDINARY_SYMBOL
#define MKCL_EXT_SPECIAL  MKCL_EXT_PACKAGE | SPECIAL_SYMBOL
#define MKCL_EXT_CONSTANT MKCL_EXT_PACKAGE | CONSTANT_SYMBOL
#define MKCL_EXT_FORM	  MKCL_EXT_PACKAGE | ORDINARY_SYMBOL | FORM_SYMBOL
#define MT_ORDINARY	  MT_PACKAGE | ORDINARY_SYMBOL
#define MT_SPECIAL	  MT_PACKAGE | SPECIAL_SYMBOL
#define MT_CONSTANT	  MT_PACKAGE | CONSTANT_SYMBOL
#define CLOS_ORDINARY	  CLOS_PACKAGE | ORDINARY_SYMBOL
#define CLOS_SPECIAL	  CLOS_PACKAGE | SPECIAL_SYMBOL
#define KEYWORD		  KEYWORD_PACKAGE | CONSTANT_SYMBOL
#define GRAY_ORDINARY	  GRAY_PACKAGE | ORDINARY_SYMBOL

#include "symbols_list.h"

#define NB_STATIC_SYMBOLS (sizeof(mkcl_root_symbols)/sizeof(mkcl_root_symbols[0]))
const mkcl_index mkcl_root_symbols_count = NB_STATIC_SYMBOLS - 1;


#define mkcl_root_symbols mkcl_root_symbols_to_c_fun_name_map
#define mkcl_symbol_initializer mkcl_symbol_to_c_fun_name
#include "symbols_list2.h"
#undef mkcl_root_symbols
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
      char * src = str->base_string.self;
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
      char * src = str->base_string.self;

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
      
      @(return output);
    }
  else
    @(return mk_cl_Cnil);
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
    { @(return mangle_full_symbol_name(env, symbol, "")); }
  else
    { @(return mk_cl_Cnil); }
}


mkcl_object mk_si_mangle_name(MKCL, mkcl_object symbol)
{
  mkcl_call_stack_check(env);
  if (symbol == mk_cl_Cnil)
    { @(return mk_cl_Ct mkcl_make_simple_base_string(env, "mk_cl_Cnil")); }
  else if (symbol == mk_cl_Ct)
    { @(return mk_cl_Ct mkcl_make_simple_base_string(env, "mk_cl_Ct")); }

  mkcl_index p  = (mkcl_symbol_initializer*)symbol - mkcl_root_symbols;
  if (/* p >= 0 && */ p <= mkcl_root_symbols_count) {
    mkcl_object output = mk_cl_format(env, 4, mk_cl_Cnil,
				      mkcl_make_simple_base_string(env, "MKCL_SYM(~S,~D)"),
				      mkcl_symbol_name(env, symbol), MKCL_MAKE_FIXNUM(p));
    @(return mk_cl_Ct output);
  }

  mkcl_object output = mangle_full_symbol_name(env, symbol, "_symbol");  
  @(return mk_cl_Cnil output);
}

mkcl_object mk_si_mangle_function_name(MKCL, mkcl_object symbol)
{
  mkcl_object found = mk_cl_Cnil;
  mkcl_object maxarg = MKCL_MAKE_FIXNUM(MKCL_CALL_ARGUMENTS_LIMIT);
  mkcl_object minarg = MKCL_MAKE_FIXNUM(0);

  mkcl_call_stack_check(env);
  if (mkcl_Null(symbol)) symbol = mk_cl_Cnil_symbol;
  else if (!MKCL_SYMBOLP(symbol)) { @(return mk_cl_Cnil mk_cl_Cnil); }

  if (((mkcl_object) &(mkcl_root_symbols[0].data)) <= symbol
      && symbol < ((mkcl_object) &(mkcl_root_symbols[NB_STATIC_SYMBOLS-1].data)))
    {
      mkcl_index i = (mkcl_symbol_initializer*)symbol - mkcl_root_symbols;
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
      @(return found output minarg maxarg);
    }
  
  mkcl_object output = mangle_full_symbol_name(env, symbol, "");
  @(return found output minarg maxarg);
}


static void
make_this_symbol(MKCL, int i, mkcl_object s, int code, const char *name,
		 mkcl_objectfn fun, int narg, mkcl_object value)
{
  enum mkcl_stype stp;
  mkcl_object package;
  bool form = 0;

  switch (code & 3) {
  case ORDINARY_SYMBOL: stp = mkcl_stp_ordinary; break;
  case SPECIAL_SYMBOL: stp = mkcl_stp_special; break;
  case CONSTANT_SYMBOL: stp = mkcl_stp_constant; break;
  case FORM_SYMBOL: form = 1; stp = mkcl_stp_ordinary; break;
  default: mkcl_lose(env, "Unknown symbol type in make_this_symbol");
  }
  switch (code & ~(int)3) {
  case CL_PACKAGE: package = mkcl_core.lisp_package; break;
  case SI_PACKAGE: package = mkcl_core.system_package; break;
  case MKCL_EXT_PACKAGE: package = mkcl_core.mkcl_ext_package; break;
  case KEYWORD_PACKAGE: package = mkcl_core.keyword_package; break;
  case MT_PACKAGE: package = mkcl_core.mt_package; break;
  case CLOS_PACKAGE: package = mkcl_core.clos_package; break;
  case GRAY_PACKAGE: package = mkcl_core.gray_package; break;
  default: mkcl_lose(env, "Unknown package in make_this_symbol");
  }
  s->symbol.t = mkcl_t_symbol;
  s->symbol.dynamic = 0;
  s->symbol.special_index = MKCL_NOT_A_SPECIAL_INDEX;
  MKCL_SET(s, MKCL_OBJNULL);
  MKCL_SYM_FUN(s) = mk_cl_Cnil;
  s->symbol.plist = mk_cl_Cnil;
  s->symbol.sys_plist = mk_cl_Cnil;
  s->symbol.hpack = mk_cl_Cnil;
  s->symbol.properly_named_class = mk_cl_Cnil;
  s->symbol.stype = stp;
  s->symbol.hpack = package;
  s->symbol.name = mkcl_make_simple_base_string(env, (char *) name);
  if (package == mkcl_core.keyword_package) {
    mkcl_sethash(env, s->symbol.name, package->pack.external, s);
    MKCL_SET(s, s);
  } else {
    int intern_flag;
    MKCL_SET(s, value);
    if (mkcl_find_symbol(env, s->symbol.name, package, &intern_flag) != mk_cl_Cnil
	&& intern_flag == MKCL_SYMBOL_IS_INHERITED) {
      mkcl_shadowing_import(env, s, package);
    } else {
      mkcl_import2(env, s, package);
    }
    mkcl_export2(env, s, package);
  }
  if (form) {
    s->symbol.stype |= mkcl_stp_special_form;
  } else if (fun) {
    mkcl_object f;
    if (narg >= 0) {
      f = mkcl_make_cfun(env, (mkcl_objectfn_fixed) fun, s, MKCL_OBJNULL, narg, NULL);
    } else {
      f = mkcl_make_cfun_va(env, fun, s, MKCL_OBJNULL, NULL);
    }
    MKCL_SYM_FUN(s) = f;
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
  for (i = 2; mkcl_root_symbols[i].init.name != NULL; i++) {
    s = (mkcl_object)(mkcl_root_symbols + i);
    code = mkcl_root_symbols[i].init.type;
    name = mkcl_root_symbols[i].init.name;
    fun = (mkcl_objectfn)mkcl_root_symbols[i].init.fun;
    narg = mkcl_root_symbols[i].init.narg;
    value = mkcl_root_symbols[i].init.value;
    make_this_symbol(env, i, s, code, name, fun, narg, value);
  }
}

