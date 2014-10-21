/* -*- mode: c -*- */
/*
    gfun.c -- Dispatch for generic functions.
*/
/*
    Copyright (c) 1990, Giuseppe Attardi.
    Copyright (c) 2010-2012, Jean-Claude Beaudoin.

    MKCL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 3 of the License, or (at your option) any later version.

    See file '../../Copyright' for full details.
*/

#include <mkcl/mkcl.h>
#include <mkcl/mkcl-inl.h>
#include <mkcl/internal.h>
#include "newhash.h"

#include <string.h>

static mkcl_object generic_function_dispatch_vararg(MKCL, mkcl_narg, ...);

mkcl_object
mkcl_FEnot_funcallable_fixed(MKCL)
{
  mkcl_object fun = env->function;
  mkcl_FEerror(env, "Not a funcallable instance ~A.", 1, fun);
  @(return);
}

mkcl_object
mkcl_FEnot_funcallable_vararg(MKCL, mkcl_narg narg, ...)
{
  return mkcl_FEnot_funcallable_fixed(env);
}

static mkcl_object
user_function_dispatch(MKCL, mkcl_narg narg, ...)
{
  int i;
  mkcl_object output;
  mkcl_object fun = env->function;
  struct mkcl_temp_stack_frame frame_aux;
  const mkcl_object frame = mkcl_temp_stack_frame_open(env, (mkcl_object)&frame_aux, narg);
  mkcl_va_list args;

  mkcl_va_start(env, args, narg, narg, 0);
  for (i = 0; i < narg; i++) {
    MKCL_TEMP_STACK_FRAME_SET(frame, i, mkcl_va_arg(args));
  }
  mkcl_va_end(args);
  fun = fun->instance.slots[fun->instance.length - 1];
  output = mkcl_apply_from_temp_stack_frame(env, frame, fun);
  mkcl_temp_stack_frame_close(env, frame);
  return output;
}

static mkcl_object
user_function_dispatch_f0(MKCL)
{
  return user_function_dispatch(env, 0);
}

static mkcl_object
user_function_dispatch_f1(MKCL, mkcl_object x1)
{
  return user_function_dispatch(env, 1, x1);
}

static mkcl_object
user_function_dispatch_f2(MKCL, mkcl_object x1, mkcl_object x2)
{
  return user_function_dispatch(env, 2, x1, x2);
}

static mkcl_object
user_function_dispatch_f3(MKCL, mkcl_object x1, mkcl_object x2, mkcl_object x3)
{
  return user_function_dispatch(env, 3, x1, x2, x3);
}

static mkcl_object
user_function_dispatch_f4(MKCL, mkcl_object x1, mkcl_object x2, mkcl_object x3, mkcl_object x4)
{
  return user_function_dispatch(env, 4, x1, x2, x3, x4);
}

static mkcl_object
generic_function_dispatch_vararg(MKCL, mkcl_narg narg, ...)
{
  mkcl_object output;
  MKCL_TEMP_STACK_FRAME_VARARGS_BEGIN(env, narg, narg, frame);
  output = _mkcl_standard_dispatch(env, frame, frame->frame.env->function);
  MKCL_TEMP_STACK_FRAME_VARARGS_END(frame);
  return output;
}

static mkcl_object
generic_function_dispatch_f0(MKCL)
{ /* this is not very good but will do for now.
     A more specialized version would be preferable... */
  return generic_function_dispatch_vararg(env, 0);
}

static mkcl_object
generic_function_dispatch_f1(MKCL, mkcl_object x1)
{ /* this is not very good but will do for now.
     A more specialized version would be preferable... */
  return generic_function_dispatch_vararg(env, 1, x1);
}

static mkcl_object
generic_function_dispatch_f2(MKCL, mkcl_object x1, mkcl_object x2)
{ /* this is not very good but will do for now.
     A more specialized version would be preferable... */
  return generic_function_dispatch_vararg(env, 2, x1, x2);
}

static mkcl_object
generic_function_dispatch_f3(MKCL, mkcl_object x1, mkcl_object x2, mkcl_object x3)
{ /* this is not very good but will do for now.
     A more specialized version would be preferable... */
  return generic_function_dispatch_vararg(env, 3, x1, x2, x3);
}

static mkcl_object
generic_function_dispatch_f4(MKCL, mkcl_object x1, mkcl_object x2, mkcl_object x3, mkcl_object x4)
{ /* this is not very good but will do for now.
     A more specialized version would be preferable... */
  return generic_function_dispatch_vararg(env, 4, x1, x2, x3, x4);
}

static void
reshape_instance(MKCL, mkcl_object x, int delta)
{
  mkcl_word size = x->instance.length + delta;
  mkcl_object aux = mkcl_allocate_instance(env, MKCL_CLASS_OF(x), size);
  memcpy(aux->instance.slots, x->instance.slots,
	 (delta < 0 ? aux->instance.length : x->instance.length) *
	 sizeof(mkcl_object));
  x->instance = aux->instance;
}

/* this turns any instance into a funcallable (apart from a builtin generic function)
   or back into an ordinary instance */

mkcl_object
mk_si_set_raw_funcallable(MKCL, mkcl_object instance, mkcl_object function)
{
  mkcl_call_stack_check(env);
  if (!MKCL_INSTANCEP(instance))
    mkcl_FEwrong_type_argument(env, @'si::instance', instance);
  if (mkcl_Null(function)) {
    if (instance->instance.isgf == 2) {
      int length = instance->instance.length-1;
      mkcl_object *slots = (mkcl_object*)mkcl_alloc(env, sizeof(mkcl_object)*(length));
      instance->instance.isgf    = 2;
      memcpy(slots, instance->instance.slots, sizeof(mkcl_object)*(length));
      instance->instance.slots   = slots;
      instance->instance.length  = length;
      instance->instance.isgf = 0; /* Could it be MKCL_NOT_FUNCALLABLE? */
      instance->instance.f.entry = mkcl_FEnot_funcallable_vararg;
      instance->instance.f._[0] = mkcl_FEnot_funcallable_fixed;
      instance->instance.f._[1] = mkcl_FEnot_funcallable_fixed;
      instance->instance.f._[2] = mkcl_FEnot_funcallable_fixed;
      instance->instance.f._[3] = mkcl_FEnot_funcallable_fixed;
      instance->instance.f._[4] = mkcl_FEnot_funcallable_fixed;
    }
  } else {
    if (instance->instance.isgf == 0) {
      int length = instance->instance.length+1;
      mkcl_object *slots = (mkcl_object*)mkcl_alloc(env, sizeof(mkcl_object)*length);
      memcpy(slots, instance->instance.slots, sizeof(mkcl_object)*(length-1));
      instance->instance.slots   = slots;
      instance->instance.length  = length;
      instance->instance.isgf    = 2; /* Could it be MKCL_USER_DISPATCH? */
      instance->instance.f.entry = user_function_dispatch;
      instance->instance.f._[0] = user_function_dispatch_f0;
      instance->instance.f._[1] = user_function_dispatch_f1;
      instance->instance.f._[2] = user_function_dispatch_f2;
      instance->instance.f._[3] = user_function_dispatch_f3;
      instance->instance.f._[4] = user_function_dispatch_f4;
    }
    instance->instance.slots[instance->instance.length-1] = function;
  }
  @(return instance);
}

mkcl_object
mk_clos_set_funcallable_instance_function(MKCL, mkcl_object x, mkcl_object function_or_t)
{
  mkcl_call_stack_check(env);
  if (!MKCL_INSTANCEP(x))
    mkcl_FEwrong_type_argument(env, @'si::instance', x);
  if (x->instance.isgf == MKCL_USER_DISPATCH) {
    reshape_instance(env, x, -1);
    x->instance.isgf = MKCL_NOT_FUNCALLABLE;
  }
  if (function_or_t == mk_cl_Ct) {
    x->instance.isgf = MKCL_STANDARD_DISPATCH;
    x->instance.f.entry = generic_function_dispatch_vararg;
    x->instance.f._[0] = generic_function_dispatch_f0;
    x->instance.f._[1] = generic_function_dispatch_f1;
    x->instance.f._[2] = generic_function_dispatch_f2;
    x->instance.f._[3] = generic_function_dispatch_f3;
    x->instance.f._[4] = generic_function_dispatch_f4;
  } else if (function_or_t == mk_cl_Cnil) {
    x->instance.isgf = MKCL_NOT_FUNCALLABLE;
    x->instance.f.entry = mkcl_FEnot_funcallable_vararg;
    x->instance.f._[0] = mkcl_FEnot_funcallable_fixed;
    x->instance.f._[1] = mkcl_FEnot_funcallable_fixed;
    x->instance.f._[2] = mkcl_FEnot_funcallable_fixed;
    x->instance.f._[3] = mkcl_FEnot_funcallable_fixed;
    x->instance.f._[4] = mkcl_FEnot_funcallable_fixed;
  } else if (mkcl_Null(mk_cl_functionp(env, function_or_t))) {
    mkcl_FEwrong_type_argument(env, @'function', function_or_t);
  } else {
    reshape_instance(env, x, +1);
    x->instance.slots[x->instance.length - 1] = function_or_t;
    x->instance.isgf = MKCL_USER_DISPATCH;
    x->instance.f.entry = user_function_dispatch;
    x->instance.f._[0] = user_function_dispatch_f0;
    x->instance.f._[1] = user_function_dispatch_f1;
    x->instance.f._[2] = user_function_dispatch_f2;
    x->instance.f._[3] = user_function_dispatch_f3;
    x->instance.f._[4] = user_function_dispatch_f4;
  }
  @(return x);
}

mkcl_object
mk_si_generic_function_p(MKCL, mkcl_object x)
{
  @(return ((MKCL_INSTANCEP(x) && (x->instance.isgf))? mk_cl_Ct : mk_cl_Cnil));
}

/**********************************************************************
 * METHOD HASH
 */

#define RECORD_KEY(e) ((e)[0])
#define RECORD_VALUE(e) ((e)[1])
#define RECORD_GEN(e) mkcl_fixnum_to_word((e+2)[0])
#define RECORD_GEN_SET(e,v) ((e+2)[0]=MKCL_MAKE_FIXNUM(v))

static void
do_clear_method_hash(struct mkcl_env_struct *env, mkcl_object target)
{
  mkcl_object table = env->method_hash;
  mkcl_index i, total_size = table->vector.dim;
  if (target == mk_cl_Ct) {
    env->method_generation = 0;
    for (i = 0; i < total_size; i+=3) {
      table->vector.self.t[i] = MKCL_OBJNULL;
      table->vector.self.t[i+1] = MKCL_OBJNULL;
      table->vector.self.word[i+2] = 0;
    }
    env->method_hash_clear_list = mk_cl_Cnil;
  } else {
    for (i = 0; i < total_size; i+=3) {
      mkcl_object key = table->vector.self.t[i];
      if (key != MKCL_OBJNULL) {
	if (target == key->vector.self.t[0]) {
	  table->vector.self.t[i] = MKCL_OBJNULL;
	  table->vector.self.word[i+2] = 0;
	}
      }
    }
  }
}

void
_mkcl_set_method_hash_size(struct mkcl_env_struct *env, mkcl_index size)
{
  env->method_spec_vector =
    mk_si_make_vector(env, mk_cl_Ct, /* element type */
		      MKCL_MAKE_FIXNUM(64), /* Maximum size */
		      mk_cl_Ct, /* adjustable */
		      MKCL_MAKE_FIXNUM(0), /* fill pointer */
		      mk_cl_Cnil, /* displaced */
		      mk_cl_Cnil);
  env->method_hash =
    mk_si_make_vector(env, mk_cl_Ct, /* element type */
		      MKCL_MAKE_FIXNUM(3*size), /* Maximum size */
		      mk_cl_Cnil, /* adjustable */
		      mk_cl_Cnil, /* fill pointer */
		      mk_cl_Cnil, /* displaced */
		      mk_cl_Cnil);
  do_clear_method_hash(env, mk_cl_Ct);
}

mkcl_object
mk_si_clear_gfun_cache(MKCL, mkcl_object what)
{
  /*
   * This function clears the generic function call hashes selectively.
   *	what = mk_cl_Ct means clear the hash completely
   *	what = generic function, means cleans only these entries
   * If we work on a multithreaded environment, we simply enqueue these
   * operations and wait for the destination thread to update its own hash.
   */
  mkcl_object list;
  volatile bool locked = false;

  mkcl_call_stack_check(env);
  MKCL_UNWIND_PROTECT_BEGIN(env) {
    MKCL_LIBC_NO_INTR(env, (MKCL_THREAD_LIST_LOCK(), locked = true));
    list = mkcl_core.threads;
    mkcl_loop_for_on_unsafe(list) {
      mkcl_object thread = MKCL_CONS_CAR(list);
      struct mkcl_env_struct *penv = thread->thread.env;
      penv->method_hash_clear_list = MKCL_CONS(env, what, penv->method_hash_clear_list);
    } mkcl_end_loop_for_on;
  } MKCL_UNWIND_PROTECT_EXIT {
    if (locked) MKCL_THREAD_LIST_UNLOCK();
  } MKCL_UNWIND_PROTECT_END;
  @(return);
}

static mkcl_index
vector_hash_key(mkcl_object keys)
{
  mkcl_index c, n, a = GOLDEN_RATIO, b = GOLDEN_RATIO;
  for (c = 0, n = keys->vector.fillp; n >= 3; ) {
    c += keys->vector.self.index[--n];
    b += keys->vector.self.index[--n];
    a += keys->vector.self.index[--n];
    mix(a, b, c);
  }
  switch (n) {
  case 2:	b += keys->vector.self.index[--n];
  case 1:	a += keys->vector.self.index[--n];
    c += keys->vector.dim;
    mix(a,b,c);
  }
  return c;
}


/*
 * variation of mkcl_gethash from hash.d, which takes an array of objects as key
 * It also assumes that entries are never removed except by clrhash.
 */

static mkcl_object *
search_method_hash(MKCL, mkcl_object keys)
{
  mkcl_object table = env->method_hash;
  mkcl_index argno = keys->vector.fillp;
  mkcl_index i = vector_hash_key(keys);
  mkcl_index total_size = table->vector.dim;
  mkcl_word min_gen, gen;
  mkcl_object *min_e;
  int k;
  i = i % total_size;
  i = i - (i % 3);
  min_gen = env->method_generation;
  min_e = 0;
  for (k = 20; k--; ) {
    mkcl_object *e = table->vector.self.t + i;
    mkcl_object hkey = RECORD_KEY(e);
    if (hkey == MKCL_OBJNULL) {
      min_gen = -1;
      min_e = e;
      if (RECORD_VALUE(e) == MKCL_OBJNULL) {
	/* This record is not only deleted but empty
	 * Hence we cannot find our method ahead */
	break;
      }
      /* Else we only know that the record has been
       * delete, but we might find our data ahead. */
    } else if (argno == hkey->vector.fillp) {
      mkcl_index n;
      for (n = 0; n < argno; n++) {
	if (keys->vector.self.t[n] !=
	    hkey->vector.self.t[n])
	  goto NO_MATCH;
      }
      min_e = e;
      goto FOUND;
    } else if (min_gen >= 0) {
    NO_MATCH:
      /* Unless we have found a deleted record, keep
       * looking for the oldest record that we can
       * overwrite with the new data. */
      gen = RECORD_GEN(e);
      if (gen < min_gen) {
	min_gen = gen;
	min_e = e;
      }
    }
    i += 3;
    if (i >= total_size) i = 0;
  }
  if (min_e == 0) {
    mkcl_lose(env, "search_method_hash");
  }
  RECORD_KEY(min_e) = MKCL_OBJNULL;
  env->method_generation++;
 FOUND:
  /*
   * Once we have reached here, we set the new generation of
   * this record and perform a global shift so that the total
   * generation number does not become too large and we can
   * expire some elements.
   */
  gen = env->method_generation;
  RECORD_GEN_SET(min_e, gen);
  if (gen >= total_size/2) {
    mkcl_object *e = table->vector.self.t;
    gen = 0.5*gen;
    env->method_generation -= gen;
    for (i = table->vector.dim; i; i-= 3, e += 3) {
      mkcl_word g = RECORD_GEN(e) - gen;
      if (g <= 0) {
	RECORD_KEY(e) = MKCL_OBJNULL;
	RECORD_VALUE(e) = mk_cl_Cnil;
	g = 0;
      }
      RECORD_GEN_SET(e, g);
    }
  }
  return min_e;
}

static mkcl_object
get_spec_vector(MKCL, mkcl_object frame, mkcl_object gf)
{
  mkcl_object *args = frame->frame.base;
  mkcl_index narg = frame->frame.size;
  mkcl_object spec_how_list = MKCL_GFUN_SPEC(gf);
  mkcl_object vector = env->method_spec_vector;
  mkcl_object *argtype = vector->vector.self.t;
  int spec_no = 1;
  argtype[0] = gf;
  mkcl_loop_for_on_unsafe(spec_how_list) {
    mkcl_object spec_how = MKCL_CONS_CAR(spec_how_list);
    mkcl_object spec_type = MKCL_CONS_CAR(spec_how);
    int spec_position = mkcl_fixnum_to_word(MKCL_CONS_CDR(spec_how));
    if (spec_position >= narg)
      mkcl_FEwrong_num_arguments(env, gf);
    argtype[spec_no++] =
      (MKCL_ATOM(spec_type) || mkcl_Null(mkcl_memql(env, args[spec_position], spec_type)))
      ? mk_cl_class_of(env, args[spec_position])
      : args[spec_position];
    if (spec_no > vector->vector.dim)
      return MKCL_OBJNULL; /* cache key size overflow. JCB */
  } mkcl_end_loop_for_on;
  vector->vector.fillp = spec_no;
  return vector;
}

static mkcl_object
compute_applicable_method(MKCL, mkcl_object frame, mkcl_object gf)
{
  /* method not cached */
  mkcl_object methods, arglist, func;
  mkcl_object *p;
  for (p = frame->frame.base + frame->frame.size, arglist = mk_cl_Cnil;
       p != frame->frame.base; ) {
    arglist = MKCL_CONS(env, *(--p), arglist);
  }
  return mkcl_funcall2(env, @+'clos::compute-effective-method-for-cache', gf, arglist);
}

mkcl_object
_mkcl_standard_dispatch(MKCL, mkcl_object frame, mkcl_object gf)
{
  mkcl_object func, vector;
  if ( env != frame->frame.env )
    mkcl_FEerror(env, "Incoherent environment on invocation of generic function ~A.", 1, gf);

  /*
   * We have to copy the frame because it might be stored in cl_env.values
   * which will be wiped out by the next function call. However this only
   * happens when we cannot reuse the values in the C stack.
   */
#if !defined(MKCL_USE_VARARG_AS_POINTER)
  struct mkcl_temp_stack_frame frame_aux;
  if (frame->frame.stack == (void*)0x1) {
    const mkcl_object new_frame = (mkcl_object)&frame_aux;
    MKCL_TEMP_STACK_FRAME_COPY(new_frame, frame);
    frame = new_frame;
  }
#endif
	
  /* See whether we have to clear the hash from some generic functions right now. */
  if (env->method_hash_clear_list != mk_cl_Cnil) {
    mkcl_object clear_list;
    mkcl_interrupt_status old_intr;

    mkcl_get_interrupt_status(env, &old_intr);
    mkcl_disable_interrupts(env);
    MKCL_THREAD_LIST_LOCK();
    clear_list = env->method_hash_clear_list;
    env->method_hash_clear_list = mk_cl_Cnil;
    MKCL_THREAD_LIST_UNLOCK();
    mkcl_set_interrupt_status(env, &old_intr);

    mkcl_loop_for_on_unsafe(clear_list) {
      do_clear_method_hash(env, MKCL_CONS_CAR(clear_list));
    } mkcl_end_loop_for_on;
  }
  vector = get_spec_vector(env, frame, gf);
  if (vector == MKCL_OBJNULL) {
    /* no cache because of key size overflow. JCB */
    func = compute_applicable_method(env, frame, gf);
  } else {
    mkcl_object *e = search_method_hash(env, vector);
    if (RECORD_KEY(e) != MKCL_OBJNULL) {
      func = RECORD_VALUE(e);
    } else {
      mkcl_object keys = mk_cl_copy_seq(env, vector);
      func = compute_applicable_method(env, frame, gf);
      if (RECORD_KEY(e) != MKCL_OBJNULL) {
	/* The cache might have changed while we
	 * computed applicable methods */
	e = search_method_hash(env, vector);
      }
      RECORD_KEY(e) = keys;
      RECORD_VALUE(e) = func;
    }
  }
  func = mkcl_funcall2(env, func, frame, mk_cl_Cnil);
  /* Only need to close the copy */
#if !defined(MKCL_USE_VARARG_AS_POINTER)
  if (frame == (mkcl_object)&frame_aux)
    mkcl_temp_stack_frame_close(env, frame);
#endif
  return func;
}

