/* -*- mode: c -*- */
/*
    stacks.c -- Binding/History/Frame stacks.
*/
/*
    Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
    Copyright (c) 1990, Giuseppe Attardi.
    Copyright (c) 2001, Juan Jose Garcia Ripoll.
    Copyright (c) 2010-2012, Jean-Claude Beaudoin.

    MKCL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 3 of the License, or (at your option) any later version.

    See file '../../Copyright' for full details.
*/

#include <mkcl/mkcl.h>

#include <unistd.h>

#include <errno.h>
#include <signal.h>
#include <string.h>
#ifdef HAVE_SYS_RESOURCE_H
# include <sys/time.h>
# include <sys/resource.h>
#endif
#include <mkcl/internal.h>


mkcl_object mk_si_disable_interrupts(MKCL)
{ /* Returns T if interrupts were enabled, NIL otherwise. */
  @(return ((env->disable_interrupts) ? mk_cl_Cnil : (mkcl_disable_interrupts(env), mk_cl_Ct)));
}

mkcl_object mk_si_enable_interrupts(MKCL)
{
  @(return (mkcl_enable_interrupts(env), mk_cl_Cnil));
}

mkcl_object mk_si_interrupt_status(MKCL)
{
#if MKCL_DEBUG_INTERRUPT_MASK
  mkcl_call_stack_check(env);
  @(return ((env->disable_interrupts) ? @':disabled' : @':enabled')
           mkcl_cstring_to_string(env, env->interrupt_disabler_file)
           mkcl_make_unsigned_integer(env, env->interrupt_disabler_lineno));
#else
  @(return ((env->disable_interrupts) ? @':disabled' : @':enabled'));
#endif
}

/************************ C STACK ***************************/

void
mkcl_call_stack_overflow(MKCL, char * const stack_mark_address)
{
  /* The "stack_mark_address" argument is in fact a dummy whose main purpose is to prevent the compiler from
     optimizing away the stack_mark by making its data flow analysis more difficult. */
  if (env->cs_overflowing)
    /* We should write a message to some log when this happens but right now we have nowhere to do it. JCB */
    mk_mt_abandon_thread(env, @':terminated'); /* Already overflowing and out of overflow space! must abort right now! */
  else
    {
      /* We could try 1024 instead of 4096 but less may not work because we need a call depth of at least 5. */
      const int call_stack_emergency_reserve = 4096;
      char * old_cs_limit = env->cs_limit;

      MKCL_UNWIND_PROTECT_BEGIN(env) {
#ifdef MKCL_DOWN_STACK
	env->cs_limit = env->cs_org + call_stack_emergency_reserve;
#else
	env->cs_limit = env->cs_org + env->cs_size - call_stack_emergency_reserve;
#endif
	env->cs_overflowing = TRUE;
	mk_cl_error(env, 5, @'mkcl::stack-overflow',
		    @':size', mkcl_make_unsigned_integer(env, env->cs_size),
		    @':type', @'si::call-stack');
      } MKCL_UNWIND_PROTECT_EXIT {
	env->cs_limit = old_cs_limit;
	env->cs_overflowing = FALSE;
      } MKCL_UNWIND_PROTECT_END;
    }
}

/* -------------------- LISP TEMPORARIES STACK -------------------- */


static void _resize_temp_stack(MKCL, mkcl_index new_size)
{
  mkcl_index size_limit = env->temp_stack_size_limit;
  mkcl_index old_size = env->temp_stack_size;
  mkcl_object * old_stack = env->temp_stack;
  mkcl_index top_index = env->temp_stack_top - old_stack;
  mkcl_object * new_stack;
  mkcl_interrupt_status old_intr;

  if (size_limit && (new_size >= size_limit))
    new_size = size_limit + env->temp_stack_overflow_size; /* ensure that we allocate the overflow area. */

#if 0
      printf("\nMKCL: Interpreter stack grown to %lu\n", new_size); /* debug. JCB */
#endif

  new_stack = mkcl_alloc(env, new_size * sizeof(mkcl_object));

  mkcl_get_interrupt_status(env, &old_intr);
  mkcl_disable_interrupts(env);

  if (old_stack != NULL)
    memcpy(new_stack, old_stack, old_size * sizeof(mkcl_object));
  env->temp_stack_size = new_size;
  env->temp_stack = new_stack;
  env->temp_stack_top = env->temp_stack + top_index;
  if (size_limit && (new_size >= size_limit))
    env->temp_stack_upper_bound = env->temp_stack + new_size - env->temp_stack_overflow_size;
  else
    env->temp_stack_upper_bound = env->temp_stack + new_size;

  /* A lisp temporaries stack always has at least one element. This is assumed by cl__va_start
   * and friends, which take a sp=0 to have no arguments.
   */
  if (top_index == 0) {
    *(env->temp_stack_top++) = MKCL_MAKE_FIXNUM(0);
  }
  mkcl_set_interrupt_status(env, &old_intr);
}

mkcl_object *
mkcl_grow_temp_stack(MKCL)
{
  if (env->temp_stack_overflowing)
    {
      mkcl_object * top = env->temp_stack_top;

      if (top >= env->temp_stack_upper_bound)
	mk_mt_abandon_thread(env, @':terminated'); /* Already overflowing and out of overflow space! must abort right now! */
      else
	return top;
    }
  else
    {
      const int TEMP_STACK_CHUNK_SIZE = 2048; /* in nb. of lisp objects */
      mkcl_index current_size = env->temp_stack_size;
      mkcl_index size_limit = env->temp_stack_size_limit;
      mkcl_index new_size;

      if (size_limit && (current_size >= size_limit))
	{
	  mkcl_object * old_upper_bound;
	  mkcl_index limit_plus_overflow = size_limit + env->temp_stack_overflow_size;

	  if (current_size < limit_plus_overflow)
	    _resize_temp_stack(env, size_limit + env->temp_stack_overflow_size);

	  MKCL_UNWIND_PROTECT_BEGIN(env) {
	    old_upper_bound = env->temp_stack_upper_bound;
	    env->temp_stack_upper_bound = env->temp_stack + env->temp_stack_size;
	    env->temp_stack_overflowing = TRUE;
	    mk_cl_error(env, 5, @'mkcl::stack-overflow',
			@':size', mkcl_make_unsigned_integer(env, size_limit),
			@':type', @'si::lisp-temp-stack');
	  } MKCL_UNWIND_PROTECT_EXIT {
	    env->temp_stack_upper_bound = old_upper_bound;
	    env->temp_stack_overflowing = FALSE;
	  } MKCL_UNWIND_PROTECT_END;
	}

      if (current_size < TEMP_STACK_CHUNK_SIZE)
	new_size = 2 * current_size;
      else
	new_size = current_size + TEMP_STACK_CHUNK_SIZE;

      _resize_temp_stack(env, new_size);
    }
  return env->temp_stack_top;
}





/********************* BINDING STACK ************************/

mkcl_index mkcl_alloc_new_special_index(MKCL, mkcl_object sym)
{
  mkcl_index index;
  mkcl_interrupt_status old_intr;

  mkcl_get_interrupt_status(env, &old_intr);
  mkcl_disable_interrupts(env);
#ifdef MKCL_WINDOWS
  EnterCriticalSection(&mkcl_core.special_index_lock);
#else
  if (pthread_mutex_lock(&mkcl_core.special_index_lock))
    mkcl_lose(env, "mkcl_alloc_new_special_index failed on pthread_mutex_lock().");
#endif

  index = mkcl_core.top_special_index;

  if (index == MKCL_NOT_A_SPECIAL_INDEX)
    mkcl_lose(env, "Out of special variables!");

  mkcl_core.top_special_index = index + 1;

#ifdef MKCL_WINDOWS
  LeaveCriticalSection(&mkcl_core.special_index_lock);
#else
  if (pthread_mutex_unlock(&mkcl_core.special_index_lock))
    mkcl_lose(env, "mkcl_alloc_new_special_index failed on pthread_mutex_unlock().");
#endif
  mkcl_set_interrupt_status(env, &old_intr);

  return index;
}

void mkcl_grow_specials(MKCL, mkcl_index new_size)
{
  mkcl_object * new = (mkcl_object *) mkcl_alloc(env, new_size * sizeof(mkcl_object));

  memcpy(new, env->specials, env->specials_size * sizeof(mkcl_object));

  {
    mkcl_index i;

    for (i = env->specials_size; i < new_size; i++)
      new[i] = MKCL_END_OF_BDS_CHAIN;
  }

  env->specials = new;
  env->specials_size = new_size;
}


void
mkcl_bds_push(MKCL, mkcl_object s)
{
  mkcl_bds_bind(env, s, _mkcl_sym_val(env, s));
}

bool mkcl_trace_specials = FALSE;

mkcl_object
mk_si_trace_specials(MKCL)
{
  mkcl_trace_specials = TRUE;
  @(return mk_cl_Ct);
}

mkcl_object
mk_si_untrace_specials(MKCL)
{
  mkcl_trace_specials = FALSE;
  @(return mk_cl_Cnil);
}


#if 0 /* inlined */
mkcl_object
mkcl_set_symbol_value(MKCL, mkcl_object s, mkcl_object value)
{
  mkcl_type type_of_s = mkcl_type_of(s);

  if (mkcl_unlikely(type_of_s != mkcl_t_symbol))
    mkcl_FEillegal_variable_name(env, s);
  else if (mkcl_unlikely(mkcl_Null(s) || s->symbol.stype & mkcl_stp_constant))
    mkcl_FEprogram_error(env, "Tried to bind a value to the constant ~S.", 1, s);
  else
    {
      mkcl_index index = s->symbol.special_index;
#ifdef MKCL_STATS
      extern bool mkcl_trace_specials;

      if (mkcl_trace_specials)
	printf("\nsetting special var: %s", s->symbol.name->base_string.self);
#endif

      if (mkcl_likely(index < env->specials_size))
	{
	  mkcl_object v = env->specials[index];
	  if (v != MKCL_END_OF_BDS_CHAIN)
	    return env->specials[index] = value;
	}
    }
  return s->symbol.value = value;
}
#endif


static void _resize_bds_stack(MKCL, mkcl_index new_size)
{
  mkcl_index size_limit = env->bds_size_limit;
  mkcl_index old_size = env->bds_size;
  struct mkcl_bds_bd * old_org = env->bds_org;
  mkcl_index top_index = env->bds_top - old_org;
  mkcl_bds_ptr org;
  mkcl_interrupt_status old_intr;

  if (size_limit && (new_size > size_limit))
    new_size = size_limit + env->bds_overflow_size; /* ensure that we allocate the overflow area. */

#if 0
  printf("\nMKCL: Binding stack grown to %lu\n", new_size); /* debug. JCB */
#endif

  org = mkcl_alloc(env, new_size * sizeof(*org));

  mkcl_get_interrupt_status(env, &old_intr);
  mkcl_disable_interrupts(env);

  memcpy(org, old_org, old_size * sizeof(*org));
  env->bds_size = new_size;
  env->bds_org = org;
  env->bds_top = org + top_index;
  if (size_limit && (new_size >= size_limit))
    env->bds_upper_bound = org + new_size - env->bds_overflow_size;
  else
    env->bds_upper_bound = org + new_size;
  mkcl_set_interrupt_status(env, &old_intr);

  /* mkcl_dealloc(env, old_org); */ /* Let's trust the GC instead! JCB */
}

void
mkcl_grow_bds_stack(MKCL)
{
  if (env->bds_overflowing)
    {
      struct mkcl_bds_bd * top = env->bds_top;

      if (top >= env->bds_upper_bound)
	mk_mt_abandon_thread(env, @':terminated'); /* Already overflowing and out of overflow space! must abort right now! */
      else
	return;
    }
  else
    {
      const int BINDING_STACK_CHUNK_SIZE = 512; /* in nb. of bindings. */
      mkcl_index current_size = env->bds_size;
      mkcl_index size_limit = env->bds_size_limit;
      mkcl_index new_size;

      if (size_limit && (current_size >= size_limit))
	{
	  struct mkcl_bds_bd * old_upper_bound;
	  mkcl_index limit_plus_overflow = size_limit + env->bds_overflow_size;

	  if (current_size < limit_plus_overflow)
	    _resize_bds_stack(env, size_limit + env->bds_overflow_size);

	  MKCL_UNWIND_PROTECT_BEGIN(env) {
	    old_upper_bound = env->bds_upper_bound;
	    env->bds_upper_bound = env->bds_org + env->bds_size;
	    env->bds_overflowing = TRUE;
	    mk_cl_error(env, 5, @'mkcl::stack-overflow',
			@':size', mkcl_make_unsigned_integer(env, size_limit),
			@':type', @'si::binding-stack');
	  } MKCL_UNWIND_PROTECT_EXIT {
	    env->bds_upper_bound = old_upper_bound;
	    env->bds_overflowing = FALSE;
	  } MKCL_UNWIND_PROTECT_END;
	}

      if (current_size < BINDING_STACK_CHUNK_SIZE)
	new_size = 2 * current_size;
      else
	new_size = current_size + BINDING_STACK_CHUNK_SIZE;

      _resize_bds_stack(env, new_size);
    }
}

void
mkcl_bds_unwind(MKCL, mkcl_index new_bds_top_index)
{
  mkcl_bds_ptr new_bds_top = new_bds_top_index + env->bds_org;
#if 0
  mkcl_bds_ptr bds = env->bds_top;
  for (;  bds > new_bds_top;  bds--)
    mkcl_bds_unwind1(env);
  env->bds_top = new_bds_top;   /* This could end up growing the stack instead! JCB */
#else
  while (env->bds_top > new_bds_top)
    mkcl_bds_unwind1(env);
#endif
}

static mkcl_bds_ptr
get_bds_ptr(MKCL, mkcl_object x)
{
  if (MKCL_FIXNUMP(x)) {
    mkcl_bds_ptr p = env->bds_org + mkcl_fixnum_to_word(x);
    if (env->bds_org <= p && p <= env->bds_top)
      return(p);
  }
  mkcl_FEerror(env, "~S is an illegal bds index. BDS_TOP = ~S.", 2, x, mk_si_bds_top(env));
}

mkcl_object
mk_si_bds_top(MKCL)
{
  @(return MKCL_MAKE_FIXNUM(env->bds_top - env->bds_org));
}

mkcl_object
mk_si_bds_var(MKCL, mkcl_object arg)
{
  mkcl_call_stack_check(env);
  @(return get_bds_ptr(env, arg)->symbol);
}

mkcl_object
mk_si_bds_val(MKCL, mkcl_object arg)
{
  mkcl_call_stack_check(env);
  mkcl_object v = get_bds_ptr(env, arg)->value;
  @(return ((v == MKCL_OBJNULL || v == MKCL_END_OF_BDS_CHAIN) ? MKCL_UNBOUND : v));
}

/******************** INVOCATION STACK **********************/

static mkcl_object
ihs_function_name(mkcl_object x)
{
  mkcl_object y;

  switch (mkcl_type_of(x))
    {
    case mkcl_t_symbol:
      return(x);

    case mkcl_t_bclosure:
      x = x->bclosure.code;
      goto mkcl_t_bytecode_case;  /* fallthrough is such bad taste! */
    case mkcl_t_bytecode:
    mkcl_t_bytecode_case:
      y = x->bytecode.name;
      if (mkcl_Null(y))
        return(@'lambda');
      else
        return y;

    case mkcl_t_cfun:
      return(x->cfun.name);
    case mkcl_t_cclosure:
      return (x->cclosure.name);
    default:
      return(mk_cl_Cnil);
    }
}

static mkcl_ihs_ptr
get_ihs_ptr(MKCL, mkcl_index n)
{
  mkcl_ihs_ptr p = env->ihs_top;
  if (n > p->index)
    mkcl_FEerror(env, "~D is an illegal IHS index. IHS_TOP = ~D.",
		 2, MKCL_MAKE_FIXNUM(n), MKCL_MAKE_FIXNUM(p->index));
  while (n < p->index)
    p = p->next;
  return p;
}

mkcl_object
mk_si_ihs_top_function_name(MKCL)
{
  mkcl_call_stack_check(env);
  @(return ihs_function_name(env->ihs_top->function));
}

mkcl_object
mk_si_ihs_top(MKCL)
{
  @(return MKCL_MAKE_FIXNUM(env->ihs_top->index));
}

mkcl_object
mk_si_ihs_prev(MKCL, mkcl_object x)
{
  mkcl_call_stack_check(env);
  @(return mk_cl_1M(env, x));
}

mkcl_object
mk_si_ihs_next(MKCL, mkcl_object x)
{
  mkcl_call_stack_check(env);
  @(return mk_cl_1P(env, x));
}

mkcl_object
mk_si_ihs_fun(MKCL, mkcl_object arg)
{
  mkcl_call_stack_check(env);
  @(return get_ihs_ptr(env, mkcl_integer_to_index(env, arg))->function);
}

mkcl_object
mk_si_ihs_env(MKCL, mkcl_object arg)
{
  mkcl_call_stack_check(env);
  @(return get_ihs_ptr(env, mkcl_integer_to_index(env, arg))->lex_env);
}

mkcl_object
mk_si_ihs_bds_marker(MKCL, mkcl_object arg)
{
  mkcl_call_stack_check(env);
  @(return MKCL_MAKE_FIXNUM(get_ihs_ptr(env, mkcl_integer_to_index(env, arg))->bds_marker));
}

/********************** FRAME STACK *************************/

static void _resize_frs_stack(MKCL, mkcl_index new_size)
{
  mkcl_index size_limit = env->frs_size_limit;
  mkcl_index old_size = env->frs_size;
  mkcl_frame_ptr old_org = env->frs_org;
  mkcl_index top_index = env->frs_top - old_org;
  mkcl_frame_ptr org;
  mkcl_interrupt_status old_intr;

  if (size_limit && (new_size > size_limit))
    new_size = size_limit + env->frs_overflow_size; /* ensure that we allocate the overflow area. */

#if 0
  printf("\nMKCL: Frame stack grown to %lu\n", new_size); /* debug. JCB */
#endif

  org = mkcl_alloc(env, new_size * sizeof(*org));

  mkcl_get_interrupt_status(env, &old_intr);
  mkcl_disable_interrupts(env);

  memcpy(org, old_org, old_size * sizeof(*org));
  env->frs_size = new_size;
  env->frs_org = org;
  env->frs_top = org + top_index;
  if (size_limit && (new_size >= size_limit))
    env->frs_upper_bound = org + new_size - env->frs_overflow_size;
  else
    env->frs_upper_bound = org + new_size;

  mkcl_set_interrupt_status(env, &old_intr);

  /* mkcl_dealloc(env, old_org); */ /* Let's trust the GC instead! JCB */
}

static void
grow_frs_stack(MKCL)
{
  if (env->frs_overflowing)
    {
      struct mkcl_frame * top = env->frs_top;

      if (top >= env->frs_upper_bound)
	mk_mt_abandon_thread(env, @':terminated'); /* Already overflowing and out of overflow space! must abort right now! */
      else
	return;
    }
  else
    {
      const int FRAME_STACK_CHUNK_SIZE = 64; /* in nb. of bindings. */
      mkcl_index current_size = env->frs_size;
      mkcl_index size_limit = env->frs_size_limit;
      mkcl_index new_size;

      if (size_limit && (current_size >= size_limit))
	{
	  struct mkcl_frame * old_upper_bound;
	  mkcl_index limit_plus_overflow = size_limit + env->frs_overflow_size;

	  if (current_size < limit_plus_overflow)
	    _resize_frs_stack(env, size_limit + env->frs_overflow_size);

	  MKCL_UNWIND_PROTECT_BEGIN(env) {
	    old_upper_bound = env->frs_upper_bound;
	    env->frs_upper_bound = env->frs_org + env->frs_size;
	    env->frs_overflowing = TRUE;
	    mk_cl_error(env, 5, @'mkcl::stack-overflow',
			@':size', mkcl_make_unsigned_integer(env, size_limit),
			@':type', @'si::frame-stack');
	  } MKCL_UNWIND_PROTECT_EXIT {
	    env->frs_upper_bound = old_upper_bound;
	    env->frs_overflowing = FALSE;
	  } MKCL_UNWIND_PROTECT_END;
	}

      if (current_size < FRAME_STACK_CHUNK_SIZE)
	new_size = 2 * current_size;
      else
	new_size = current_size + FRAME_STACK_CHUNK_SIZE;

      _resize_frs_stack(env, new_size);
    }
}

mkcl_frame_ptr
_mkcl_frs_push(MKCL, mkcl_object val)
{
  mkcl_frame_ptr output = ++env->frs_top;
  if (output >= env->frs_upper_bound) {
    grow_frs_stack(env);
    output = env->frs_top;
  }
  output->frs_bds_top_index = env->bds_top - env->bds_org;
  output->frs_val = val;
  output->frs_ihs = env->ihs_top;
  output->frs_sp = MKCL_TEMP_STACK_INDEX(env);

  mkcl_get_interrupt_status(env, &output->frs_intr);

  return output;
}

void
mkcl_unwind(MKCL, mkcl_frame_ptr fr)
{
  /* sleeping point can only be at leaf nodes of our call tree (ones calling long system calls). */
  env->sleeping_on = mk_cl_Cnil; /* since the target of unwind cannot be a leaf it follows that "sleeping_on" must be NIL. */
  mkcl_disable_interrupts(env); /* interrupts are re-enabled by the longjmp target. */

  {
    struct mkcl_frame * frs_org = env->frs_org;
    struct mkcl_frame * frs_top = env->frs_top;

    env->nlj_fr = fr;
    if (fr == NULL)
      frs_top = frs_org; /* jump directly all the way to the frs stack root guard! Used to abort everything. */
    else
      /* Search back on the stack. */
      for (; (frs_top != fr) && (frs_top->frs_val != MKCL_PROTECT_TAG) && (frs_top > frs_org); frs_top--);

    env->frs_top = frs_top;
    env->ihs_top = frs_top->frs_ihs;
    mkcl_bds_unwind(env, frs_top->frs_bds_top_index);
    MKCL_TEMP_STACK_SET_INDEX(env, frs_top->frs_sp);
    mkcl_longjmp(frs_top->frs_jmpbuf, 1);
    /* never reached */
  }
}

mkcl_frame_ptr
mkcl_frs_sch (MKCL, mkcl_object frame_id)
{
  mkcl_frame_ptr top;
  for (top = env->frs_top;  top >= env->frs_org;  top--)
    if (top->frs_val == frame_id)
      return(top);
  return(NULL);
}

static mkcl_frame_ptr
get_frame_ptr(MKCL, mkcl_object x)
{
  if (MKCL_FIXNUMP(x)) {
    mkcl_frame_ptr p = env->frs_org + mkcl_fixnum_to_word(x);
    if (env->frs_org <= p && p <= env->frs_top)
      return p;
  }
  mkcl_FEerror(env, "~S is an illegal frs index. FRS_TOP = ~S.", 2, x, mk_si_frs_top(env));
}

mkcl_object
mk_si_frs_top(MKCL)
{
  @(return MKCL_MAKE_FIXNUM(env->frs_top - env->frs_org)); /* Any overflow detection? JCB */
}

mkcl_object
mk_si_frs_bds(MKCL, mkcl_object arg)
{
  mkcl_call_stack_check(env);
  @(return MKCL_MAKE_FIXNUM(get_frame_ptr(env, arg)->frs_bds_top_index));
}

mkcl_object
mk_si_frs_tag(MKCL, mkcl_object arg)
{
  mkcl_call_stack_check(env);
  @(return get_frame_ptr(env, arg)->frs_val);
}

mkcl_object
mk_si_frs_ihs(MKCL, mkcl_object arg)
{
  mkcl_call_stack_check(env);
  @(return MKCL_MAKE_FIXNUM(get_frame_ptr(env, arg)->frs_ihs->index));
}

mkcl_object
mk_si_sch_frs_base(MKCL, mkcl_object fr, mkcl_object ihs)
{
  mkcl_call_stack_check(env);
  mkcl_frame_ptr x;
  mkcl_index y = mkcl_integer_to_index(env, ihs);

  for (x = get_frame_ptr(env, fr); 
       x <= env->frs_top && x->frs_ihs->index < y;
       x++);
  @(return ((x > env->frs_top) ? mk_cl_Cnil : MKCL_MAKE_FIXNUM(x - env->frs_org)));
}

/************************************************************/

mkcl_object *
_mkcl_va_sp(MKCL, mkcl_narg narg)
{
  return env->temp_stack_top - narg; /* FIXME: Possible unchecked underflow here! JCB */
}


/********************* INITIALIZATION ***********************/

mkcl_object
mk_si_set_binding_stack_limit(MKCL, mkcl_object size_limit)
{
  mkcl_call_stack_check(env);
  mkcl_index the_size_limit = mkcl_integer_to_index(env, size_limit);
  mkcl_index used_size = env->bds_top - env->bds_org;
  
  if (the_size_limit && (used_size > the_size_limit))
    the_size_limit = used_size;

  env->bds_size_limit = the_size_limit;
  env->bds_upper_bound = env->bds_org + the_size_limit;
  @(return mkcl_make_unsigned_integer(env, the_size_limit));
}

mkcl_object
mk_si_set_frame_stack_limit(MKCL, mkcl_object size_limit)
{
  mkcl_call_stack_check(env);
  mkcl_index the_size_limit = mkcl_integer_to_index(env, size_limit);
  mkcl_index used_size = env->frs_top - env->frs_org;
  
  if (the_size_limit && (used_size > the_size_limit))
    the_size_limit = used_size;

  env->frs_size_limit = the_size_limit;
  env->frs_upper_bound = env->frs_org + the_size_limit;
  @(return mkcl_make_unsigned_integer(env, the_size_limit));
}

mkcl_object
mk_si_set_lisp_temp_stack_limit(MKCL, mkcl_object size_limit)
{
  mkcl_call_stack_check(env);
  mkcl_index the_size_limit = mkcl_integer_to_index(env, size_limit);
  mkcl_index used_size = env->temp_stack_top - env->temp_stack;
  
  if (the_size_limit && (used_size > the_size_limit))
    the_size_limit = used_size;

  env->temp_stack_size_limit = the_size_limit;
  env->temp_stack_upper_bound = env->temp_stack + the_size_limit;
  @(return mkcl_make_unsigned_integer(env, the_size_limit));
}

mkcl_object
mk_si_get_call_stack_limit(MKCL)
{
  mkcl_index cs_size = env->cs_size;
#ifdef MKCL_DOWN_STACK
  mkcl_index cs_top_index = (((char *) env->cs_org) + cs_size) - ((char *) &cs_size);
#else
  mkcl_index cs_top_index = ((char *) &cs_size) - ((char *) env->cs_org);
#endif

  mkcl_call_stack_check(env);
  @(return mkcl_make_unsigned_integer(env, cs_size) mkcl_make_unsigned_integer(env, cs_top_index));
}

mkcl_object
mk_si_get_binding_stack_limit(MKCL)
{
  mkcl_call_stack_check(env);
  @(return mkcl_make_unsigned_integer(env, env->bds_size_limit) mk_si_bds_top(env));
}

mkcl_object
mk_si_get_frame_stack_limit(MKCL)
{
  mkcl_call_stack_check(env);
  @(return mkcl_make_unsigned_integer(env, env->frs_size_limit) mk_si_frs_top(env));
}

mkcl_object
mk_si_get_lisp_temp_stack_limit(MKCL)
{
  mkcl_index temp_stack_top_index = env->temp_stack_top - env->temp_stack;

  mkcl_call_stack_check(env);
  @(return mkcl_make_unsigned_integer(env, env->temp_stack_size_limit) mkcl_make_unsigned_integer(env, temp_stack_top_index));
}


void mkcl_init_call_stack_overflow_area(MKCL, char * const stack_mark_address)
{
  env->cs_overflowing = FALSE;
#if __unix
  pthread_attr_t attr_obj;
  pthread_attr_t * attr = &attr_obj;

  /* For MacOS X we will have to use pthread_get_stackaddr_np() and pthread_get_stacksizse_np(). */
  int rc = pthread_getattr_np(pthread_self(), attr); /* Linux specific */

  {
    size_t stack_size;
    void * stack_addr;
# if __linux
    int rc1 = pthread_attr_getstack(attr, &stack_addr, &stack_size);
# else
    int rc1 = pthread_attr_getstacksize(attr, &stack_size);
    int rc2 = pthread_attr_getstackaddr(attr, &stack_addr);
# endif

    /* On Linux, if "ulimit -Ss" return "unlimited" then the addresses you get from pthread_attr_getstack() overlap!
       The initial thread then has a very large stack space that it populates from one end and all threads created
       through pthread_create see their stack segment allocated successively from the other end of the "initial"
       thread stack space. Yes, the subsequent thread stack spaces are nested inside the "initial" thread stack space.
       This way, although that initial thread stack space is very large (about 2^48 on x86_64, nearly 2^31 on x86),
       there is a possibility that the initial thread may collide and overwrite on the stack of an other thread.
       Detecting such a collision seems extremely difficult since it requires tracking of the stack segment allocation
       and deallocation of every subsequent thread durign the existence of the process.
       Note that this happens only if "ulimit -Ss" returns "unlimited", so one can consider that such
       a user configuration is the equivalent of (declaim (optimize (safety 0))) as far as stack overflow
       detection is concerned. JCB 2012/05/26
    */
    env->cs_size = stack_size;
    env->cs_org = stack_addr;

# ifdef MKCL_DOWN_STACK
    env->cs_limit = env->cs_org + env->cs_overflow_size;
# else
    env->cs_limit = (env->cs_org + env->cs_size) - env->cs_overflow_size;
# endif

#if 0 /* debug JCB */
    printf("\nMKCL: thread params, tid = %d, stack_size = %lu, stack_addr = %p, mark = %p, offset = %lu\n",
	   (env->own_thread ? env->own_thread->thread.tid : 0), stack_size, stack_addr, stack_mark_address,
	   ((unsigned long) ((((char *)stack_addr) + stack_size) - stack_mark_address)));
    printf("MKCL: stack top = %p!\n", (((char *)stack_addr) + stack_size));
    if (!((((char *) stack_addr) <= stack_mark_address) && (stack_mark_address < (((char *)stack_addr) + stack_size))))
      printf("MKCL: mark is outside the stack!\n");
    fflush(NULL);
#endif
  }

# if 0
    {
      struct rlimit rl;
      mkcl_index size;
      if (getrlimit(RLIMIT_STACK, &rl))
	mkcl_C_lose(env, "mkcl_init_call_stack_overflow_area failed on getrlimit().");
      if (rl.rlim_cur != RLIM_INFINITY)
	size = rl.rlim_cur;
      else
	size = 0; /* the OS said this was unlimited and we believe it! JCB */

      if ((rl.rlim_cur == RLIM_INFINITY) && (rl.rlim_max == RLIM_INFINITY))
	printf("\nMKCL: thread params, tid = %d, getrlimit(RLIMIT_STACK, &rl)"
	       " said rl.rlim_cur = RLIM_INFINITY, rl.rlim_max = RLIM_INFINITY\n",
	       (env->own_thread ? env->own_thread->thread.tid : 0));
      else if (rl.rlim_max == RLIM_INFINITY)
	printf("\nMKCL: thread params, tid = %d, getrlimit(RLIMIT_STACK, &rl)"
	       " said rl.rlim_cur = %lu, rl.rlim_max = RLIM_INFINITY\n",
	       (env->own_thread ? env->own_thread->thread.tid : 0), rl.rlim_cur);	
      else
	printf("\nMKCL: thread params, tid = %d, getrlimit(RLIMIT_STACK, &rl) said rl.rlim_cur = %lu, rl.rlim_max = %lu\n",
	       (env->own_thread ? env->own_thread->thread.tid : 0), rl.rlim_cur, rl.rlim_max);
      fflush(NULL);

    }
# endif

# if 0 /* This is unusable with Boehm's GC. */
  {
    int rc;
    stack_t new_sig_stack;
    stack_t old_sig_stack;
    
    new_sig_stack.ss_size = env->altstack_size;
    new_sig_stack.ss_sp = env->altstack;
    new_sig_stack.ss_flags = 0;

    rc = sigaltstack(&new_sig_stack, &old_sig_stack);
    if (rc)
      mkcl_C_lose(env, "mkcl_init_stacks() failed on sigaltstack()");

    printf("\nMKCL: old_sigstack size = %lu, sp = %p, flags = %d\n",
	   old_sig_stack.ss_size, old_sig_stack.ss_sp, old_sig_stack.ss_flags);
    fflush(NULL);
  }
# endif
#elif defined(MKCL_WINDOWS)
  {
    mkcl_index a_var = 0;
    MEMORY_BASIC_INFORMATION mbi;

    SIZE_T mbi_size = VirtualQuery(&a_var, &mbi, sizeof(mbi));

    char * stack_top = mbi.BaseAddress + mbi.RegionSize;
    char * stack_base = mbi.AllocationBase;
    mkcl_index stack_size = stack_top - stack_base;

    if (mbi_size != sizeof(mbi))
      mkcl_C_lose(env, "mkcl_init_call_stack_overflow_area() failed on VirtualQuery");

# if 0
    printf("\ntid = %d, mbi info: BaseAddress = 0x%p, AllocationBase = 0x%p, RegionSize = %lu",
	   (env->own_thread ? env->own_thread->thread.tid : 0),
	   mbi.BaseAddress, mbi.AllocationBase, mbi.RegionSize);
    printf("\ntid = %d, Stack size = %lu, stack base = 0x%p, stack top = 0x%p, mark = 0x%p, offset = %lu\n",
	   (env->own_thread ? env->own_thread->thread.tid : 0),
	   stack_size, stack_base, stack_top, stack_mark_address, stack_top - stack_mark_address);
    fflush(NULL);
# endif

    env->cs_size = stack_size;
    env->cs_org = stack_base;

# ifdef MKCL_DOWN_STACK
    env->cs_limit = stack_base + env->cs_overflow_size;
# else
    env->cs_limit = stack_top - env->cs_overflow_size;
# endif

  }
#else
#error Do not know how to find call stack address and size!
#endif
}

static const struct mkcl_ihs_frame ihs_org = { NULL, mk_cl_Cnil, mk_cl_Cnil, 0, 0}; /* End of the IHS chains. */

void mkcl_reset_stacks(MKCL)
{
  env->ihs_top = (struct mkcl_ihs_frame *) &ihs_org;
  env->temp_stack_top = env->temp_stack + 1;
  env->frs_top = env->frs_org;
  env->bds_top = env->bds_org;
  env->cs_org = NULL;
  env->cs_size = 0;
}

void
mkcl_init_stacks(MKCL, mkcl_env new_env, struct mkcl_thread_init_parameters * params)
{
  mkcl_index size;
  mkcl_index limit;

  new_env->ihs_top = (struct mkcl_ihs_frame *) &ihs_org;

  /*--- Lisp temporaries stack ---*/
  size = (params ? params->lisp_temp_stack_initial_size : 0);
  limit = (params ? params->lisp_temp_stack_size_limit : MKCL_UNLIMITED);
  if (size == 0) size = mkcl_get_option(MKCL_OPT_LISP_TEMP_STACK_INITIAL_SIZE);

  new_env->temp_stack_size = size;
  new_env->temp_stack_size_limit = limit;
  new_env->temp_stack = (mkcl_object *) mkcl_alloc(env, size * sizeof(mkcl_object));

  /* A lisp temporaries stack always has at least one element. This is assumed by cl__va_start
   * and friends, which take a sp=0 to have no arguments.
   */
  *(new_env->temp_stack) = MKCL_MAKE_FIXNUM(0);
  new_env->temp_stack_top = new_env->temp_stack + 1;
  new_env->temp_stack_upper_bound = new_env->temp_stack + size;

  new_env->temp_stack_overflow_size = mkcl_get_option(MKCL_OPT_LISP_TEMP_STACK_OVERFLOW_SIZE);

  /*--- Frame stack ---*/
  size = (params ? params->frame_stack_initial_size : 0);
  limit = (params ? params->frame_stack_size_limit : MKCL_UNLIMITED);
  if (size == 0) size = mkcl_get_option(MKCL_OPT_FRAME_STACK_INITIAL_SIZE);

  new_env->frs_size = size;
  new_env->frs_size_limit = limit;
  new_env->frs_org = (mkcl_frame_ptr) mkcl_alloc(env, size * sizeof(*new_env->frs_org));

  new_env->frs_top = new_env->frs_org;
  new_env->frs_upper_bound = new_env->frs_org + size;

  new_env->frs_overflow_size = mkcl_get_option(MKCL_OPT_FRAME_STACK_OVERFLOW_SIZE);

  /*--- Binding stack ---*/
  size = (params ? params->binding_stack_initial_size : 0);
  limit = (params ? params->binding_stack_size_limit : MKCL_UNLIMITED);
  if (size == 0) size = mkcl_get_option(MKCL_OPT_BINDING_STACK_INITIAL_SIZE);

  new_env->bds_size = size;
  new_env->bds_size_limit = limit;
  new_env->bds_org = (mkcl_bds_ptr) mkcl_alloc(env, size * sizeof(*new_env->bds_org));

  new_env->bds_top = new_env->bds_org;
  /* The following dummy binding at the very bottom of the bds stack is in fact
     a place holder never to be poped, acting as a kind of underflow guard. JCB */
  new_env->bds_org->symbol = @'*package*';
  new_env->bds_org->value = MKCL_OBJNULL;
  new_env->bds_upper_bound = new_env->bds_org + size;

  new_env->bds_overflow_size = mkcl_get_option(MKCL_OPT_BINDING_STACK_OVERFLOW_SIZE);

#if 0 /* __unix */ /* This is unusable with Boehm's GC. */
  /*--- Call stack overflow ---*/
  size = (params ? params->sigaltstack_size : 0);
  if (size == 0) size = mkcl_get_option(MKCL_OPT_SIGALTSTACK_SIZE);
  if (size < SIGSTKSZ)
    size = SIGSTKSZ + mkcl_core.pagesize; /* We ask for 1 more page for a little bit more safety. */

  new_env->altstack_size = size;
  new_env->altstack = mkcl_alloc(env, size);
#endif

  /* The following two (cs_org_request, cs_size_request) are requests to be used at OS thread creation. */
  new_env->cs_org_request = (params ? params->call_stack_addr : NULL);
  new_env->cs_size_request = (params ? params->call_stack_size : 0);
  new_env->cs_overflow_size = mkcl_get_option(MKCL_OPT_CALL_STACK_OVERFLOW_SIZE);
}

