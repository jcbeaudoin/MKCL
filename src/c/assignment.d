/* -*- mode: c -*- */
/*
    assignment.c  -- Assignment.
*/
/*
    Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
    Copyright (c) 1990, Giuseppe Attardi.
    Copyright (c) 2001, Juan Jose Garcia Ripoll.
    Copyright (c) 2010-2017, Jean-Claude Beaudoin.

    MKCL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 3 of the License, or (at your option) any later version.

    See file '../../Copyright' for full details.
*/

#include <mkcl/mkcl.h>
#include <mkcl/internal.h>
#include <string.h>

#if MKCL_WINDOWS
#define SYSTEM_PROPERTIES_LOCK() EnterCriticalSection(&system_properties_lock)
#define SYSTEM_PROPERTIES_UNLOCK() LeaveCriticalSection(&system_properties_lock)

static CRITICAL_SECTION system_properties_lock;
#elif MKCL_PTHREADS
#define SYSTEM_PROPERTIES_LOCK()				\
  if (pthread_mutex_lock(&system_properties_lock))		\
    mkcl_lose(env, "Failed in SYSTEM_PROPERTIES_LOCK()")
#define SYSTEM_PROPERTIES_UNLOCK()				\
  if (pthread_mutex_unlock(&system_properties_lock))		\
    mkcl_lose(env, "Failed in SYSTEM_PROPERTIES_UNLOCK()")

static pthread_mutex_t system_properties_lock;
#else
# error Incomplete definition of SYSTEM_PROPERTIES_LOCK().
#endif

mkcl_object
mk_cl_set(MKCL, mkcl_object var, mkcl_object val)
{
  mkcl_call_stack_check(env);
#if 0 /* This test is done by MKCL_SETQ now. JCB */
  if (mkcl_symbol_type(env, var) & mkcl_stp_constant)
    mkcl_FEinvalid_variable(env, "Cannot assign to the constant ~S.", var);
#endif
  val = MKCL_SETQ(env, var, val);
  mkcl_return1(val);
}

@(defun si::fset (fname def &optional macro pprint)
  mkcl_object sym = mk_si_function_block_name(env, fname);
  mkcl_object pack;
  bool mflag;
  int type;
@
  if (mkcl_Null(sym)) sym = mk_cl_Cnil_symbol;
  if (mkcl_Null(mk_cl_functionp(env, def))) mkcl_FEinvalid_function(env, def);
  pack = mkcl_symbol_package(env, sym);
  if (pack != mk_cl_Cnil && pack->pack.closed) {
    mkcl_CEpackage_error(env, pack,
			 "Attempt to redefine function ~S in closed package ~S.",
			 "Ignore closing and proceed", 2, fname, pack);
  }
  mflag = !mkcl_Null(macro);
  type = mkcl_symbol_type(env, sym);
  if ((type & mkcl_stp_special_form) && !mflag) {
    mkcl_FEerror(env, "Given that ~S is a special form, ~S cannot be defined as a function.",
		 2, sym, fname);
  }
  if (MKCL_SYMBOLP(fname)) {
    if (mflag) {
      type |= mkcl_stp_macro;
    } else {
      type &= ~mkcl_stp_macro;
    }
    mkcl_symbol_type_set(env, sym, type);
    MKCL_SYM_FUN(sym) = def;
    if (!MKCL_INSTANCEP(def) && mkcl_Null(mk_si_compiled_function_name(env, def)))
      mk_si_set_compiled_function_name(env, def, sym);
  } else {
    if (mflag)
      mkcl_FEerror(env, "~S is not a valid name for a macro.", 1, fname);
    mk_si_put_sysprop(env, sym, @'si::setf-symbol', def);
    mk_si_rem_sysprop(env, sym, @'si::setf-lambda');
    mk_si_rem_sysprop(env, sym, @'si::setf-method');
    mk_si_rem_sysprop(env, sym, @'si::setf-update');
  }
  mkcl_return_value(def);
@)

mkcl_object
mk_cl_makunbound(MKCL, mkcl_object sym)
{
  mkcl_call_stack_check(env);
  if (mkcl_symbol_type(env, sym) & mkcl_stp_constant)
    mkcl_FEinvalid_variable(env, "Cannot unbind the constant ~S.", sym);
  MKCL_SETQ(env, sym, MKCL_OBJNULL);
  mkcl_return_value(sym);
}

mkcl_object
mk_cl_fmakunbound(MKCL, mkcl_object fname)
{
  mkcl_call_stack_check(env);
  mkcl_object sym = mk_si_function_block_name(env, fname);
  mkcl_object pack = mkcl_symbol_package(env, sym);
  if (pack != mk_cl_Cnil && pack->pack.closed) {
    mkcl_CEpackage_error(env, pack,
			 "Attempt to redefine function ~S in closed package ~S.",
			 "Ignore lock and proceed", 2, fname, pack);
  }
  if (MKCL_SYMBOLP(fname)) {
    if (mkcl_Null(sym)) sym = mk_cl_Cnil_symbol;
    mkcl_symbol_type_set(env, sym, mkcl_symbol_type(env, sym) & ~mkcl_stp_macro);
    MKCL_SYM_FUN(sym) = mk_cl_Cnil;
  } else {
    mk_si_rem_sysprop(env, sym, @'si::setf-symbol');
    mk_si_rem_sysprop(env, sym, @'si::setf-lambda');
    mk_si_rem_sysprop(env, sym, @'si::setf-method');
    mk_si_rem_sysprop(env, sym, @'si::setf-update');
  }
  mkcl_return_value(fname);
}


mkcl_object
mk_si_get_sysprop(MKCL, mkcl_object sym, mkcl_object prop)
{
  mkcl_call_stack_check(env);
  if (mkcl_Null(sym))
    { sym = mk_cl_Cnil_symbol; }
  if (mkcl_type_of(sym) == mkcl_t_symbol)
    {
      mkcl_object plist = sym->symbol.sys_plist;
      mkcl_object val = mkcl_getf(env, plist, prop, MKCL_OBJNULL);
      if (val == MKCL_OBJNULL)
	{ mkcl_return_2_values(mk_cl_Cnil, mk_cl_Cnil); }
      else
	{ mkcl_return_2_values(val, mk_cl_Ct); }
    }
  else
    {
      mkcl_object plist = mkcl_gethash_safe(env, sym, mkcl_core.system_properties, mk_cl_Cnil);
      prop = mkcl_getf(env, plist, prop, MKCL_OBJNULL);
      if (prop == MKCL_OBJNULL) {
	mkcl_return_2_values(mk_cl_Cnil, mk_cl_Cnil);
      } else {
	mkcl_return_2_values(prop, mk_cl_Ct);
      }
    }
}

mkcl_object
mk_si_put_sysprop(MKCL, mkcl_object sym, mkcl_object prop, mkcl_object value)
{
  mkcl_call_stack_check(env);
  if (mkcl_Null(sym))
    { sym = mk_cl_Cnil_symbol; }
  if (mkcl_type_of(sym) == mkcl_t_symbol)
    {
      volatile bool locked = false;
      MKCL_UNWIND_PROTECT_BEGIN(env) {
	mkcl_object plist = sym->symbol.sys_plist;
	mkcl_interrupt_status old_intr;

	mkcl_get_interrupt_status(env, &old_intr);
	mkcl_disable_interrupts(env);
	SYSTEM_PROPERTIES_LOCK(); locked = true;
	mkcl_set_interrupt_status(env, &old_intr);
	sym->symbol.sys_plist = mk_si_put_f(env, plist, value, prop);
      } MKCL_UNWIND_PROTECT_EXIT {
	if (locked) SYSTEM_PROPERTIES_UNLOCK();
      } MKCL_UNWIND_PROTECT_END;
      mkcl_return_value(value);
    }
  else
    {
      volatile bool locked = false;
      mkcl_object plist;

      MKCL_UNWIND_PROTECT_BEGIN(env) {
	mkcl_interrupt_status old_intr;

	mkcl_get_interrupt_status(env, &old_intr);
	mkcl_disable_interrupts(env);
	SYSTEM_PROPERTIES_LOCK(); locked = true;
	mkcl_set_interrupt_status(env, &old_intr);
	plist = mkcl_gethash_safe(env, sym, mkcl_core.system_properties, mk_cl_Cnil);
	mkcl_sethash(env, sym, mkcl_core.system_properties, mk_si_put_f(env, plist, value, prop));
      } MKCL_UNWIND_PROTECT_EXIT {
	if (locked) SYSTEM_PROPERTIES_UNLOCK();
      } MKCL_UNWIND_PROTECT_END;
      mkcl_return_value(value);
    }
}

mkcl_object
mk_si_rem_sysprop(MKCL, mkcl_object sym, mkcl_object prop)
{
  mkcl_call_stack_check(env);
  if (mkcl_Null(sym))
    { sym = mk_cl_Cnil_symbol; }
  if (mkcl_type_of(sym) == mkcl_t_symbol)
    {
      volatile bool locked = false;
      mkcl_object found;
      MKCL_UNWIND_PROTECT_BEGIN(env) {
	mkcl_object plist = sym->symbol.sys_plist;
	mkcl_interrupt_status old_intr;

	mkcl_get_interrupt_status(env, &old_intr);
	mkcl_disable_interrupts(env);
	SYSTEM_PROPERTIES_LOCK(); locked = true;
	mkcl_set_interrupt_status(env, &old_intr);

	sym->symbol.sys_plist = mk_si_rem_f(env, plist, prop);
	found = MKCL_VALUES(1); /* extract the 2nd value from the rem_f call just above. */
      } MKCL_UNWIND_PROTECT_EXIT {
	if (locked) SYSTEM_PROPERTIES_UNLOCK();
      } MKCL_UNWIND_PROTECT_END;
      mkcl_return_value(found);
    }
  else
    {
      volatile bool locked = false;
      mkcl_object plist, found;

      MKCL_UNWIND_PROTECT_BEGIN(env) {
	mkcl_interrupt_status old_intr;

	mkcl_get_interrupt_status(env, &old_intr);
	mkcl_disable_interrupts(env);
	SYSTEM_PROPERTIES_LOCK(); locked = true;
	mkcl_set_interrupt_status(env, &old_intr);
	plist = mkcl_gethash_safe(env, sym, mkcl_core.system_properties, mk_cl_Cnil);
	plist = mk_si_rem_f(env, plist, prop);
	found = MKCL_VALUES(1);
	mkcl_sethash(env, sym, mkcl_core.system_properties, plist);
      } MKCL_UNWIND_PROTECT_EXIT {
	if (locked) SYSTEM_PROPERTIES_UNLOCK();
      } MKCL_UNWIND_PROTECT_END;
      mkcl_return_value(found);
    }
}


mkcl_object
mk_si_system_properties(MKCL)
{
  /* Maybe we should lock the system properties,
     then copy the hashtable and finally return the copy. JCB */
  mkcl_return_value(mkcl_core.system_properties);
}


void mkcl_init_system_properties(MKCL)
{
  mkcl_core.system_properties =
    mk_cl__make_hash_table(env, @'equal', MKCL_MAKE_FIXNUM(1024), /* size */
			   mkcl_make_singlefloat(env, 1.5f), /* rehash-size */
			   mkcl_make_singlefloat(env, 0.75f)); /* rehash-threshold */

#if MKCL_WINDOWS
#if 0
  system_properties_lock = CreateMutex(NULL, FALSE, mkcl_handle_debug_name(env, "system properties lock"));
  if ( system_properties_lock == NULL )
    mkcl_FEwin32_error(env, "mkcl_init_system_properties failed to create lock.", 0);
#else
  InitializeCriticalSection(&system_properties_lock);
#endif

#elif MKCL_PTHREADS
  {
    const pthread_mutexattr_t * const mutexattr = mkcl_normal_mutexattr;

    if (pthread_mutex_init(&system_properties_lock, mutexattr))
      mkcl_lose(env, "mkcl_init_system_properties failed on pthread_mutex_init.");
  }
#else
# error Incomplete mkcl_init_system_properties().
#endif
}

void mkcl_clean_up_system_properties(MKCL)
{ /* Best effort only. We cannot raise an exception from here. */
#if MKCL_WINDOWS
  DeleteCriticalSection(&system_properties_lock);
#elif MKCL_PTHREADS
  (void) pthread_mutex_destroy(&system_properties_lock);
#else
# error Incomplete mkcl_clean_up_system_properties().
#endif
}


