/* -*- mode: c -*- */
/*
    symbol.d -- Symbols.
*/
/*
    Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
    Copyright (c) 1990, Giuseppe Attardi.
    Copyright (c) 2001, Juan Jose Garcia Ripoll.
    Copyright (c) 2011-2021, Jean-Claude Beaudoin.

    MKCL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 3 of the License, or (at your option) any later version.

    See file '../../Copyright' for full details.
*/

#include <mkcl/mkcl.h>
#include <mkcl/internal.h>
#include <mkcl/mkcl-inl.h>

/******************************* ------- ******************************/

mkcl_object
mkcl_symbol_package(MKCL, mkcl_object s)
{
  do {
    if (mkcl_Null(s))
      return mk_cl_Cnil_symbol->symbol.hpack;
    if (mkcl_type_of(s) == mkcl_t_symbol)
      return s->symbol.hpack;
    s = mkcl_type_error(env, MK_CL_symbol_package, "symbol", s, MK_CL_symbol);
  } while(1);
}

int
mkcl_symbol_type(MKCL, mkcl_object s)
{
  do {
    if (mkcl_Null(s))
      return mk_cl_Cnil_symbol->symbol.stype;
    if (mkcl_type_of(s) == mkcl_t_symbol)
      return s->symbol.stype;
    s = mkcl_type_error(env, MK_CL_symbol_name, "symbol", s, MK_CL_symbol);
  } while(1);
}

void
mkcl_symbol_type_set(MKCL, mkcl_object s, int type)
{
  do {
    if (mkcl_Null(s)) {
      mk_cl_Cnil_symbol->symbol.stype = type;
      return;
    }
    if (mkcl_type_of(s) == mkcl_t_symbol) {
      s->symbol.stype = type;
      return;
    }
    s = mkcl_type_error(env, MK_CL_symbol_name, "symbol", s, MK_CL_symbol);
  } while(1);
}

mkcl_object
mkcl_symbol_name(MKCL, mkcl_object s)
{
  do {
    if (mkcl_Null(s)) {
      return mk_cl_Cnil_symbol->symbol.name;
    }
    if (mkcl_type_of(s) == mkcl_t_symbol) {
      return s->symbol.name;
    }
    s = mkcl_type_error(env, MK_CL_symbol_name, "symbol", s, MK_CL_symbol);
  } while(1);
}

static mkcl_object *
mkcl_symbol_plist(MKCL, mkcl_object s)
{
  do {
    if (mkcl_Null(s)) {
      return &mk_cl_Cnil_symbol->symbol.plist;
    }
    if (mkcl_type_of(s) == mkcl_t_symbol) {
      return &s->symbol.plist;
    }
    s = mkcl_type_error(env, MK_CL_symbol_plist, "symbol", s, MK_CL_symbol);
  } while(1);
}

/**********************************************************************/

static void mkcl_FEtype_error_plist(MKCL, mkcl_object x) /*__attribute__((noreturn))*/;


#define MKCL_SYMBOL_INITIALIZER(name, name_hash_val, package, value, fun) { \
    mkcl_t_symbol, 0, mkcl_stp_ordinary, 0,                     \
      value,                                                    \
      fun,                                                      \
      mk_cl_Cnil,                                               \
      name,                                                     \
      package,                                                  \
      mk_cl_Cnil,                                               \
      mk_cl_Cnil,                                               \
      MKCL_NOT_A_SPECIAL_INDEX,					\
      name_hash_val						\
      }

static struct mkcl_symbol _symbol_proto = MKCL_SYMBOL_INITIALIZER(mk_cl_Cnil, 0, mk_cl_Cnil, MKCL_OBJNULL, mk_cl_Cnil);

mkcl_object
mk_cl_make_symbol(MKCL, mkcl_object str)
{
  mkcl_hash_value hash_val = 0;

  mkcl_call_stack_check(env);
 AGAIN:
  /* INV: In several places it is assumed that we copy the string! */
  switch (mkcl_type_of(str)) {
  case mkcl_t_string:
    if (!mkcl_fits_in_base_string(env, str)) {
      str = mk_cl_copy_seq(env, str);
    } else {
      str = mk_si_copy_to_simple_base_string(env, str);
    }
    hash_val = mkcl_hash_full_string(str->string.self, str->string.fillp, hash_val);
    break;
  case mkcl_t_base_string:
    str = mk_si_copy_to_simple_base_string(env, str);
    hash_val = mkcl_hash_base_string(str->base_string.self, str->base_string.fillp, hash_val);
    break;
  default:
    str = mkcl_type_error(env, MK_CL_make_symbol,"name",str,MK_CL_string);
    hash_val = 0;
    goto AGAIN;
  }
  {
    mkcl_object x = mkcl_alloc_raw_symbol(env);

    x->symbol.name = str;
    x->symbol.hashed_name = hash_val;
    x->symbol.special_index = MKCL_NOT_A_SPECIAL_INDEX;
    MKCL_SET(x,MKCL_OBJNULL);
    MKCL_SYM_FUN(x) = mk_cl_Cnil;
    x->symbol.plist = mk_cl_Cnil;
    x->symbol.hpack = mk_cl_Cnil;
    x->symbol.stype = mkcl_stp_ordinary;
    x->symbol.sys_plist = mk_cl_Cnil;
    x->symbol.properly_named_class = mk_cl_Cnil;

    mkcl_return_value(x);
  }
}

/*
	mkcl_make_keyword(s) makes a keyword from C string s.
*/
mkcl_object
mkcl_make_keyword(MKCL, const char *s)
{
  mkcl_object x = _mkcl_intern(env, s, mkcl_core.keyword_package);
  /* cl_export(x, keyword_package); this is implicit in _mkcl_intern() */
  return x;
}

#if 0 /* inlined */
mkcl_object
mkcl_symbol_value(MKCL, mkcl_object s)
{
  mkcl_object value = MKCL_SYM_VAL(env, s);

  if (value == MKCL_OBJNULL)
    mkcl_FEunbound_variable(env, s);
  return value;
}
#endif

static void
mkcl_FEtype_error_plist(MKCL, mkcl_object x)
{
  mk_cl_error(env, 9,
	      MK_CL_simple_type_error,
	      MK_KEY_format_control,
	      mkcl_make_simple_base_string(env, "Not a valid property list ~D"),
	      MK_KEY_format_arguments,
	      mk_cl_list(env, 1, x),
	      MK_KEY_expected_type,
	      MK_SI_property_list,
	      MK_KEY_datum,
	      x);
}

mkcl_object
mkcl_getf(MKCL, mkcl_object place, mkcl_object indicator, mkcl_object deflt)
{
  mkcl_word n;
  mkcl_object l, slow_l;

  for (n = 0, l = slow_l = place; MKCL_CONSP(l); n++) {
    mkcl_object cdr_l = MKCL_CONS_CDR(l);
    if (!MKCL_CONSP(cdr_l))
      break;
    if (MKCL_CONS_CAR(l) == indicator)
      return MKCL_CONS_CAR(cdr_l);
    if (slow_l == l && n != 0)
      mkcl_FEcircular_list(env, place);
    if (n & 1)
      slow_l = MKCL_CONS_CDR(MKCL_CONS_CDR(slow_l));
    l = MKCL_CONS_CDR(cdr_l);
  }
  if (l != mk_cl_Cnil)
    mkcl_FEtype_error_plist(env, place);
  return(deflt);
}

mkcl_object
mkcl_get(MKCL, mkcl_object s, mkcl_object p, mkcl_object d)
{
  return mkcl_getf(env, *mkcl_symbol_plist(env, s), p, d);
}

/*
	(SI:PUT-F plist value indicator)
	returns the new property list with value for property indicator.
	It will be used in SETF for GETF.
*/
mkcl_object
mk_si_put_f(MKCL, mkcl_object place, mkcl_object value, mkcl_object indicator)
{
  mkcl_word n;
  mkcl_object l, slow_l;

  mkcl_call_stack_check(env);
  for (n = 0, l = slow_l = place; MKCL_CONSP(l); n++) {
    mkcl_object cdr_l = MKCL_CONS_CDR(l);
    if (!MKCL_CONSP(cdr_l))
      break;
    if (MKCL_CONS_CAR(l) == indicator) {
      MKCL_RPLACA(cdr_l, value);
      mkcl_return_value(place);
    }
    if (slow_l == l && n != 0)
      mkcl_FEcircular_list(env, place);
    if (n & 1)
      slow_l = MKCL_CONS_CDR(MKCL_CONS_CDR(slow_l));
    l = MKCL_CONS_CDR(cdr_l);
  }
  if (l != mk_cl_Cnil)
    mkcl_FEtype_error_plist(env, place);
  place = MKCL_CONS(env, value, place);
  mkcl_return_value(MKCL_CONS(env, indicator, place));
}

/*
	Remf(p, i) removes property i
	from the property list pointed by p,
	which is a pointer to an mkcl_object.
	The returned value of remf(p, i) is:

		TRUE    if the property existed
		FALSE   otherwise.
*/
static bool
remf(MKCL, mkcl_object *place, mkcl_object indicator)
{
  mkcl_word n = 0;
  mkcl_object l = *place, tail = mk_cl_Cnil;
  mkcl_object slow_l = l;

  while (!mkcl_Null(l)) {
    mkcl_object ind;
    if (!MKCL_LISTP(l))
      mkcl_FEtype_error_plist(env, *place);
    if (slow_l == l && n != 0)
      mkcl_FEcircular_list(env, *place);

    ind = MKCL_CONS_CAR(l);
    l = MKCL_CONS_CDR(l);
    if (!MKCL_CONSP(l))
      mkcl_FEtype_error_plist(env, *place);
    if (ind == indicator) {
      l = MKCL_CONS_CDR(l);
      if (mkcl_Null(tail))
	*place = l;
      else
	MKCL_RPLACD(tail, l);
      return TRUE;
    }
    if (n & 1)
      slow_l = MKCL_CONS_CDR(MKCL_CONS_CDR(slow_l));
    n++;
    tail = l;
    l = MKCL_CONS_CDR(l);
  }
  return FALSE;
}

bool
mkcl_keywordp(mkcl_object s)
{
  return (mkcl_type_of(s) == mkcl_t_symbol) && (s->symbol.hpack == mkcl_core.keyword_package);
}

mkcl_object mk_cl_get(MKCL, mkcl_narg narg, mkcl_object sym, mkcl_object indicator, ...)
{
  mkcl_call_stack_check(env);
  {
    mkcl_object *plist;
    mkcl_object deflt = mk_cl_Cnil;
    MKCL_RECEIVE_1_OPTIONAL_ARGUMENT(env, MK_CL_get, narg, 2, indicator, &deflt);

    plist = mkcl_symbol_plist(env, sym);
    mkcl_return_value(mkcl_getf(env, *plist, indicator, deflt));
  }
}

mkcl_object
mk_cl_remprop(MKCL, mkcl_object sym, mkcl_object prop)
{
  mkcl_object *plist;

  mkcl_call_stack_check(env);
  plist = mkcl_symbol_plist(env, sym);
  mkcl_return_value((remf(env, plist, prop)? mk_cl_Ct: mk_cl_Cnil));
}

mkcl_object
mk_cl_symbol_plist(MKCL, mkcl_object sym)
{
  mkcl_call_stack_check(env);
  mkcl_return_value(*mkcl_symbol_plist(env, sym));
}

mkcl_object mk_cl_getf(MKCL, mkcl_narg narg, mkcl_object place, mkcl_object indicator, ...)
{
  mkcl_call_stack_check(env);
  {
    mkcl_object deflt = mk_cl_Cnil;
    MKCL_RECEIVE_1_OPTIONAL_ARGUMENT(env, MK_CL_getf, narg, 2, indicator, &deflt);

    mkcl_return_value(mkcl_getf(env, place, indicator, deflt));
  }
}

mkcl_object
mk_cl_get_properties(MKCL, mkcl_object place, mkcl_object indicator_list)
{
  mkcl_word n;
  mkcl_object l, slow_l;

  mkcl_call_stack_check(env);
  for (n = 0, l = slow_l = place;  MKCL_CONSP(l); n++) {
    mkcl_object cdr_l = MKCL_CONS_CDR(l);
    if (!MKCL_CONSP(cdr_l))
      break;
    if (mkcl_member_eq(env, MKCL_CONS_CAR(l), indicator_list))
      mkcl_return_3_values(MKCL_CONS_CAR(l), MKCL_CONS_CAR(cdr_l), l);
    if (slow_l == l && n != 0)
      mkcl_FEcircular_list(env, place);
    if (n & 1)
      slow_l = MKCL_CONS_CDR(MKCL_CONS_CDR(slow_l));
    l = MKCL_CONS_CDR(cdr_l);
  }
  if (l != mk_cl_Cnil)
    mkcl_FEtype_error_plist(env, place);
  mkcl_return_3_values(mk_cl_Cnil, mk_cl_Cnil, mk_cl_Cnil);
}

mkcl_object
mk_cl_symbol_name(MKCL, mkcl_object x)
{
  mkcl_call_stack_check(env);
  mkcl_return_value(mkcl_symbol_name(env, x));
}

mkcl_object mk_cl_copy_symbol(MKCL, mkcl_narg narg, mkcl_object sym, ...)
{
  mkcl_call_stack_check(env);
  {
    mkcl_object x = mk_cl_Cnil;
    mkcl_object cp = mk_cl_Cnil;
    MKCL_RECEIVE_1_OPTIONAL_ARGUMENT(env, MK_CL_copy_symbol, narg, 1, sym, &cp);

    if (mkcl_Null(sym))
      sym = mk_cl_Cnil_symbol;
    x = mk_cl_make_symbol(env, mkcl_symbol_name(env, sym));
    if (!mkcl_Null(cp)) {
      x->symbol.special_index = MKCL_NOT_A_SPECIAL_INDEX;
      x->symbol.stype = sym->symbol.stype;
      x->symbol.value = sym->symbol.value;
      x->symbol.gfdef = sym->symbol.gfdef;
      x->symbol.plist = mk_cl_copy_list(env, sym->symbol.plist);
      /* FIXME!!! We should also copy the system property list */
    }
    mkcl_return_value(x);
  }
}

mkcl_object mk_cl_gensym(MKCL, mkcl_narg narg, ...)
{
  mkcl_call_stack_check(env);
  {
    mkcl_type t;
    mkcl_object counter, output;
    bool increment;
    mkcl_object prefix = mkcl_core.gensym_prefix;
    MKCL_RECEIVE_1_OPTIONAL_ARGUMENT(env, MK_CL_gensym, narg, 0, narg, &prefix);
    {
    AGAIN:
      if (mkcl_stringp(env, prefix)) {
        counter = MKCL_SYM_VAL(env, MK_CL_DYNVAR_gensym_counter);
        increment = 1;
      } else if ((t = mkcl_type_of(prefix)) == mkcl_t_fixnum || t == mkcl_t_bignum) {
        counter = prefix;
        prefix = mkcl_core.gensym_prefix;
        increment = 0;
      } else {
        prefix = mkcl_type_error(env, MK_CL_gensym,"prefix",prefix,
                                 mk_cl_list(env, 3, MK_CL_or, MK_CL_string, MK_CL_integer));
        goto AGAIN;
      }
      output = mkcl_make_string_output_stream(env, 64, TRUE, MK_KEY_default);

      mkcl_bds_bind(env, MK_CL_DYNVAR_print_escape, mk_cl_Cnil);
      mkcl_bds_bind(env, MK_CL_DYNVAR_print_readably, mk_cl_Cnil);
      mkcl_bds_bind(env, MK_CL_DYNVAR_print_base, MKCL_MAKE_FIXNUM(10));
      mkcl_bds_bind(env, MK_CL_DYNVAR_print_radix, mk_cl_Cnil);
      mk_si_write_ugly_object(env, prefix, output);
      mk_si_write_ugly_object(env, MKCL_CODE_CHAR('-'), output);
      mk_si_write_ugly_object(env, MKCL_MAKE_FIXNUM(env->own_thread->thread.tid), output);
      mk_si_write_ugly_object(env, MKCL_CODE_CHAR('-'), output);
      mk_si_write_ugly_object(env, counter, output);
      mkcl_bds_unwind_n(env, 4);

      output = mk_cl_make_symbol(env, mk_cl_get_output_stream_string(env, output));
      if (increment)
        MKCL_SETQ(env, MK_CL_DYNVAR_gensym_counter,mkcl_one_plus(env, counter));
      mkcl_return_value(output);
    }
  }
}

#if MKCL_WINDOWS
static CRITICAL_SECTION gentemp_lock;
#elif MKCL_PTHREADS
static pthread_mutex_t gentemp_lock;
#endif

mkcl_object mk_cl_gentemp(MKCL, mkcl_narg narg, ...)
{
  mkcl_call_stack_check(env);
  {
    mkcl_object output, s;
    int intern_flag;
    mkcl_object prefix = mkcl_core.gentemp_prefix;
    mkcl_object pack = (((narg == 0) || (narg == 1)) ? mkcl_current_package(env) : mk_cl_Cnil);
    MKCL_RECEIVE_2_OPTIONAL_ARGUMENTS(env, MK_CL_gentemp, narg, 0, narg, &prefix, &pack);

    prefix = mkcl_check_type_string(env, MK_CL_gentemp, prefix);
    pack = mk_si_coerce_to_package(env, pack);
  ONCE_MORE:
    output = mkcl_make_string_output_stream(env, 64, TRUE, MK_KEY_default);

    mkcl_bds_bind(env, MK_CL_DYNVAR_print_escape, mk_cl_Cnil);
    mkcl_bds_bind(env, MK_CL_DYNVAR_print_readably, mk_cl_Cnil);
    mkcl_bds_bind(env, MK_CL_DYNVAR_print_base, MKCL_MAKE_FIXNUM(10));
    mkcl_bds_bind(env, MK_CL_DYNVAR_print_radix, mk_cl_Cnil);
    mk_si_write_ugly_object(env, prefix, output);

    {
      bool not_ok;
      volatile bool locked = false;

      MKCL_UNWIND_PROTECT_BEGIN(env) {
#if MKCL_WINDOWS
        EnterCriticalSection(&gentemp_lock);
#elif MKCL_PTHREADS
        MKCL_LIBC_NO_INTR(env, not_ok = pthread_mutex_lock(&gentemp_lock));
        if (not_ok)
          mkcl_lose(env, "Failed to acquire lock in mk_cl_gentemp().");
#endif
        locked = true;
        mk_si_write_ugly_object(env, mkcl_core.gentemp_counter, output);
        mkcl_core.gentemp_counter = mkcl_one_plus(env, mkcl_core.gentemp_counter);
      } MKCL_UNWIND_PROTECT_EXIT {
#if MKCL_WINDOWS
        if (locked)
          LeaveCriticalSection(&gentemp_lock);
#elif MKCL_PTHREADS
        if (locked)
          if (pthread_mutex_unlock(&gentemp_lock))
            mkcl_lose(env, "Failed to release lock in mk_cl_gentemp()");
#endif
      } MKCL_UNWIND_PROTECT_END;
    }

    mkcl_bds_unwind_n(env, 4);

    s = mkcl_intern(env, mkcl_copy_string(env, mk_cl_get_output_stream_string(env, output)), pack, &intern_flag);
    if (intern_flag != 0)
      goto ONCE_MORE;
    mkcl_return_value(s);
  }
}

mkcl_object
mk_cl_symbol_package(MKCL, mkcl_object sym)
{
  mkcl_call_stack_check(env);
  mkcl_return_value(mkcl_symbol_package(env, sym));
}

mkcl_object
mk_cl_keywordp(MKCL, mkcl_object sym)
{
  mkcl_call_stack_check(env);
  mkcl_return_value((mkcl_keywordp(sym) ? mk_cl_Ct : mk_cl_Cnil));
}

/*
	(SI:REM-F plist indicator) returns two values:

		* the new property list
		  in which property indcator is removed

		* T     if really removed
		  NIL   otherwise.

	It will be used for macro REMF.
*/
mkcl_object
mk_si_rem_f(MKCL, mkcl_object plist, mkcl_object indicator)
{
  mkcl_call_stack_check(env);
  bool found = remf(env, &plist, indicator);
  mkcl_return_2_values(plist, (found ? mk_cl_Ct : mk_cl_Cnil));
}

mkcl_object
mk_si_set_symbol_plist(MKCL, mkcl_object sym, mkcl_object plist)
{
  mkcl_call_stack_check(env);
  *mkcl_symbol_plist(env, sym) = plist;
  mkcl_return_value(plist);
}

mkcl_object
mk_si_putprop(MKCL, mkcl_object sym, mkcl_object value, mkcl_object indicator)
{
  mkcl_call_stack_check(env);
  mkcl_object *plist = mkcl_symbol_plist(env, sym);
  *plist = mk_si_put_f(env, *plist, value, indicator);
  mkcl_return_value(value);
}

/* Added for defstruct. Beppe */
mkcl_object mk_si_put_properties(MKCL, mkcl_narg narg, mkcl_object sym, ...)
{
  mkcl_call_stack_check(env);
  {
    mkcl_setup_for_rest(env, MK_SI_put_properties, 0, narg, sym, ind_values);

    while (--narg >= 2) {
      mkcl_object prop = mkcl_va_arg(ind_values);
      mk_si_putprop(env, sym, mkcl_va_arg(ind_values), prop);
      narg--;
    }
    mkcl_va_end(ind_values);
    mkcl_return_value(sym);
  }
}

mkcl_object
mk_si_Xmake_special(MKCL, mkcl_object sym)
{
  int type = mkcl_symbol_type(env, sym);
  if (type & mkcl_stp_constant)
    mkcl_FEerror(env, "~S is a constant.", 1, sym);
  mkcl_symbol_type_set(env, sym, type | mkcl_stp_special);
  mk_cl_remprop(env, sym, MK_SI_symbol_macro);
  mkcl_return_value(sym);
}

mkcl_object
mk_si_Xmake_constant(MKCL, mkcl_object sym, mkcl_object val)
{
  int type = mkcl_symbol_type(env, sym);
  if (type & mkcl_stp_special)
    mkcl_FEerror(env, "The argument ~S to DEFCONSTANT is a special variable.",
	    1, sym);
  mkcl_symbol_type_set(env, sym, type | mkcl_stp_constant);
  MKCL_SET(sym, val);
  mkcl_return_value(sym);
}

void
mkcl_init_gentemp(MKCL)
{
#if MKCL_WINDOWS
#if 0
  gentemp_lock = CreateMutex(NULL, FALSE, mkcl_handle_debug_name(env, "gentemp lock"));
  if ( gentemp_lock == NULL )
    mkcl_FEwin32_error(env, "mkcl_init_gentemp failed to create gentemp lock.", 0);
#else
  InitializeCriticalSection(&gentemp_lock);
#endif
#elif MKCL_PTHREADS
  {
    const pthread_mutexattr_t * const mutexattr = mkcl_normal_mutexattr;

    if (pthread_mutex_init(&gentemp_lock, mutexattr))
      mkcl_lose(env, "mkcl_init_read failed on pthread_mutex_init.");
  }
#else
# error Incomplete mkcl_init_gentemp().
#endif
}

void mkcl_clean_up_gentemp(MKCL)
{ /* Best effort only. We cannot raise an exception from here. */
#if MKCL_WINDOWS
  DeleteCriticalSection(&gentemp_lock);
#elif MKCL_PTHREADS
  (void) pthread_mutex_destroy(&gentemp_lock);
#else
# error Incomplete mkcl_clean_up_gentemp().
#endif
}

