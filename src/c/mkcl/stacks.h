/* -*- mode: c -*- */
/*
    stacks.h -- Bind/Jump/Frame stacks.
*/
/*
    Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
    Copyright (c) 1990, Giuseppe Attardi.
    Copyright (c) 2000, Juan Jose Garcia-Ripoll
    Copyright (c) 2010-2012, Jean-Claude Beaudoin.

    MKCL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 3 of the License, or (at your option) any later version.

    See file './Copyright' for full details.
*/

#ifndef MKCL_STACKS_H
#define MKCL_STACKS_H

#ifdef __cplusplus
extern "C" {
#endif

/***********
 * C STACK
 ***********/

  extern MKCL_API void mkcl_call_stack_overflow(MKCL, char * const stack_mark_address);


#ifdef MKCL_DOWN_STACK
#define mkcl_call_stack_check(env)					\
  {									\
    int stack_mark = 0;							\
    char * const stack_mark_address = (char *) &stack_mark;		\
    if (mkcl_unlikely(stack_mark_address <= (env)->cs_limit))		\
      mkcl_call_stack_overflow(env, stack_mark_address);		\
  }
#else
#define mkcl_call_stack_check(env)					\
  {									\
    int stack_mark = 0;							\
    char * const stack_mark_address = (char *) &stack_mark;		\
    if (mkcl_unlikely(stack_mark_address >= (env)->cs_limit))		\
      mkcl_call_stack_overflow(env, stack_mark_address);		\
  }
#endif

#ifdef MKCL_DOWN_STACK
#define mkcl_ensure_call_stack(env, size)				\
  {									\
    int stack_mark = 0;							\
    char * const stack_mark_address = (char*) &stack_mark;		\
    char * const max_sp =  stack_mark_address - size;			\
    if (mkcl_unlikely((max_sp <= (env)->cs_limit) /* blew it */		\
		      || (max_sp > stack_mark_address))) /* wrapped */	\
      mkcl_call_stack_overflow(env, stack_mark_address);		\
  }
#else
#define mkcl_ensure_call_stack(env, size)				\
  {									\
    int stack_mark = 0;							\
    char * const stack_mark_address = (char*) &stack_mark;		\
    char * const max_sp =  stack_mark_address + size;			\
    if (mkcl_unlikely((max_sp >= (env)->cs_limit) /* blew it */		\
		      || (max_sp < stack_mark_address))) /* wrapped */	\
      mkcl_call_stack_overflow(env, stack_mark_address);		\
  }
#endif

  /* These two MKCL_XXX_STACK_DEPTH macro are estimate of the minimum stack
     depth required to safely (without stack overflow) perform a function
     in the XXX context/library. */
  /* They are somewhat rough guesses that try to err on the safe side.
     We could increase them if exprience show them to be too small. */
#define MKCL_LIBC_STACK_DEPTH (8 * 1024 * sizeof(mkcl_word))
#define MKCL_GC_STACK_DEPTH (16 * 1024 * sizeof(mkcl_word)) /* need to allow for a full GC collect phase. */



/**************
 * BIND STACK
 **************/

  typedef struct mkcl_bds_bd {
    mkcl_object symbol;	/*  symbol  */
    mkcl_object value;	/*  previous value of the symbol  */
  } *mkcl_bds_ptr;

/* This value is chosen because it cannot reference any valid lisp object
   and cannot be confused with MKCL_OBJNULL. */
#define MKCL_END_OF_BDS_CHAIN ((mkcl_object) 0x04)

  extern MKCL_API void mkcl_grow_bds_stack(MKCL);

#define	mkcl_bds_check(env) (((env)->bds_top >= (env)->bds_upper_bound) ? mkcl_grow_bds_stack() : (void)0)

  extern MKCL_API void mkcl_bds_push(MKCL, mkcl_object symbol);
#if 0
  extern MKCL_API mkcl_object mkcl_set_symbol_value(MKCL, mkcl_object s, mkcl_object v);
#endif

  static inline mkcl_object _mkcl_sym_val(MKCL, mkcl_object sym)
  {
    mkcl_index index;

    if (mkcl_Null(sym)) return mk_cl_Cnil;

    if ((index = sym->symbol.special_index) < env->specials_size)
      {
	mkcl_object val = (env->specials)[index];
#ifdef MKCL_STATS
	extern bool mkcl_trace_specials;

	if (mkcl_unlikely(mkcl_trace_specials)) /* debug JCB */
	  printf("\nreading special var: %s", sym->symbol.name->base_string.self);
#endif	
	if (val != MKCL_END_OF_BDS_CHAIN) return val;
      }
    return sym->symbol.value;
  }

  static inline void mkcl_bds_bind(MKCL, mkcl_object s, mkcl_object value)
  {
    struct mkcl_bds_bd * slot;
    mkcl_index index;

    if (mkcl_Null(s))
      mkcl_FEprogram_error(env, "Tried to bind a value to the constant NIL.", 0);

    if ((index = s->symbol.special_index) >= env->specials_size)
      {
	MKCL_API mkcl_index mkcl_alloc_new_special_index(MKCL, mkcl_object sym);
	MKCL_API void mkcl_grow_specials(MKCL, mkcl_index new_size);

	if (index == MKCL_NOT_A_SPECIAL_INDEX)
	  index = s->symbol.special_index = mkcl_alloc_new_special_index(env, s);
	if (index >= env->specials_size)
	  mkcl_grow_specials(env, index + 1);
      }

    slot = ++(env->bds_top);
    if (slot >= env->bds_upper_bound) {
      mkcl_grow_bds_stack(env);
      slot = env->bds_top;
    }

    {
      mkcl_object * specials = env->specials;

      slot->symbol = s;
      slot->value = specials[index];
   
      specials[index] = value;
    }
  }

  static inline void mkcl_bds_unwind1(MKCL)
  { /* FIXME: This could underflow! JCB */
    register struct mkcl_bds_bd *slot = ((env->bds_org < env->bds_top) ? env->bds_top-- : env->bds_org);

    (env->specials)[slot->symbol->symbol.special_index] = slot->value;
  }

  static inline void mkcl_bds_unwind_n(MKCL, int n)
  {
    while (n--) mkcl_bds_unwind1(env);
  }

#define MKCL_SYM_VAL(env,s) (_mkcl_sym_val(env,s))
#define MKCL_SET(s,v) ((s)->symbol.value=(v))
#define MKCL_SETQ(env,s,v) (mkcl_set_symbol_value(env,s,v))


  static inline mkcl_object mkcl_symbol_value(MKCL, mkcl_object s)
  {
    mkcl_object value = MKCL_SYM_VAL(env, s);

    if (mkcl_unlikely(value == MKCL_OBJNULL))
      mkcl_FEunbound_variable(env, s);
    return value;
  }

  static inline mkcl_object mkcl_set_symbol_value(MKCL, mkcl_object s, mkcl_object value)
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



/****************************
 * INVOCATION HISTORY STACK
 ****************************/

  typedef struct mkcl_ihs_frame {
    struct mkcl_ihs_frame *next;
    mkcl_object function;
    mkcl_object lex_env;
    mkcl_index index;
    mkcl_index bds_marker;
  } *mkcl_ihs_ptr;

#define mkcl_ihs_push(env,rec,fun,lisp_env) do {		     \
    const mkcl_env __the_env = (env);				     \
    struct mkcl_ihs_frame * const r = (rec);			     \
    r->next=__the_env->ihs_top;					     \
    r->function=(fun);						     \
    r->lex_env=(lisp_env);					     \
    r->index=(__the_env->ihs_top->index)+1;			     \
    r->bds_marker=__the_env->bds_top-__the_env->bds_org;	     \
    __the_env->ihs_top = r;					     \
  } while(0)

#define mkcl_ihs_pop(env) do {				\
    const mkcl_env __the_env = (env);			\
    struct mkcl_ihs_frame *r = __the_env->ihs_top;	\
    if (r) __the_env->ihs_top = r->next;		\
  } while(0)

  extern MKCL_API mkcl_object mk_si_ihs_top_function_name(MKCL);



/***************
 * FRAME STACK
 ***************/
/* Frames signal points in the code to which we can at any time jump.
 * Frames are established, for instance, by CATCH, BLOCK, TAGBODY,
 * LAMBDA, UNWIND-PROTECT, etc.
 *
 * Frames are established by mkcl_frs_push(). For each call to mkcl_frs_push()
 * there must be a corresponding mkcl_frs_pop(). More precisely, since our
 * frame mechanism relies on the C stack and on the setjmp/longjmp
 * functions, any function that creates a frame must also destroy it
 * with mkcl_frs_pop() before returning.
 *
 * Frames are identified by a value frs_val. This can be either a
 * unique identifier, created for each CATCH, BLOCK, etc, or a common
 * one MKCL_PROTECT_TAG, used by UNWIND-PROTECT forms. The first type
 * of frames can be target of a search mkcl_frs_sch() and thus one can jump
 * to them. The second type of frames are like barriers designed to
 * intercept the jumps to the outer frames and are called
 * automatically by the function unwind() whenever it jumps to a frame
 * which is beyond one of these barriers.
 */

  typedef struct mkcl_interrupt_status {
    volatile int disable_interrupts;
#if MKCL_DEBUG_INTERRUPT_MASK
    char * volatile interrupt_disabler_file;
    volatile size_t interrupt_disabler_lineno;
#endif
  } mkcl_interrupt_status;

#if MKCL_DEBUG_INTERRUPT_MASK
# if 0
# define mkcl_get_interrupt_status(the_env, status)			\
  ((status)->disable_interrupts = (the_env)->disable_interrupts,		\
   (status)->interrupt_disabler_file = (the_env)->interrupt_disabler_file, \
   (status)->interrupt_disabler_lineno = (the_env)->interrupt_disabler_lineno)

# define mkcl_set_interrupt_status(the_env, status)			\
  ((the_env)->interrupt_disabler_lineno = (status)->interrupt_disabler_lineno, \
   (the_env)->interrupt_disabler_file = (status)->interrupt_disabler_file, \
   (the_env)->disable_interrupts = (status)->disable_interrupts)
# else
  static inline void mkcl_get_interrupt_status(MKCL, mkcl_interrupt_status * const status_ptr)
  {
    (status_ptr)->disable_interrupts = (env)->disable_interrupts;
    (status_ptr)->interrupt_disabler_file = (env)->interrupt_disabler_file;
    (status_ptr)->interrupt_disabler_lineno = (env)->interrupt_disabler_lineno;
  }
  static inline int mkcl_set_interrupt_status(MKCL, const mkcl_interrupt_status * const status_ptr)
  {
    (env)->interrupt_disabler_lineno = (status_ptr)->interrupt_disabler_lineno;
    (env)->interrupt_disabler_file = (status_ptr)->interrupt_disabler_file;
    return (env)->disable_interrupts = (status_ptr)->disable_interrupts;
  }
# endif
#else
# define mkcl_get_interrupt_status(the_env, status)		\
  ((status)->disable_interrupts = (the_env)->disable_interrupts)
  
# define mkcl_set_interrupt_status(the_env, status)		\
  ((the_env)->disable_interrupts = (status)->disable_interrupts)
#endif

  typedef struct mkcl_frame {
    mkcl_jmp_buf frs_jmpbuf;
    mkcl_object	frs_val;
    mkcl_index	frs_bds_top_index;
    mkcl_ihs_ptr frs_ihs;
    mkcl_index	frs_sp;
    mkcl_interrupt_status frs_intr;
  } *mkcl_frame_ptr;

  extern MKCL_API mkcl_frame_ptr _mkcl_frs_push(MKCL, mkcl_object);

#define mkcl_frs_push(env,val)  mkcl_setjmp(_mkcl_frs_push(env,val)->frs_jmpbuf)

#if 0 /* MKCL_WINDOWS */ /* Will use it if we go back to hardware detection */
# define mkcl_maybe_reset_call_stack_overflow(env)			\
  if (mkcl_unlikely((env)->cs_has_overflowed))				\
    if (!_resetstkoflw())						\
      mkcl_FEwin32_error(env, "Stack overflow reset has failed", 0);
#else
# define mkcl_maybe_reset_call_stack_overflow(env)
#endif

#if 0
  /* This version will happily underflow! JCB */
# define mkcl_frs_pop(env)						\
  ((((env)->frs_top < (env)->frs_org)					\
    ? mkcl_disable_interrupts(env)					\
    : mkcl_set_interrupt_status(env, &(env)->frs_top->frs_intr)),	\
   (env)->frs_top--)
#else
# define mkcl_frs_pop(env)						\
  (mkcl_likely((env)->frs_top > (env)->frs_org)				\
   ? mkcl_set_interrupt_status(env, &(((env)->frs_top--)->frs_intr))	\
   : mkcl_disable_interrupts(env)) /* Shouldn't this case raise an internal error instead? JCB */
#endif


/*******************
 * ARGUMENTS STACK
 *******************
 * Here we define how we handle the incoming arguments for a
 * function. Our calling conventions specify that at most
 * MKCL_C_ARGUMENTS_LIMIT are pushed onto the C stack. If the function
 * receives more than this number of arguments it will keep a copy of
 * _all_ those arguments _plus_ the remaining ones in the lisp temporaries
 * stack. The caller is responsible for storing and removing such
 * values.
 *
 * Given this structure, we need our own object for handling variable
 * argument list, mkcl_va_list. This object joins the C data type for
 * handling vararg lists and a pointer to the lisp temp stack, in case the
 * arguments were passed there.
 *
 * Note that keeping a direct reference to the lisp temp stack effectively
 * locks it in memory, preventing the block from being garbage
 * collected if the stack grows -- at least until all references are
 * eliminated --. This is something we have to live with and which
 * is somehow unavoidable, given that function arguments have to be
 * stored somewhere.
 */

  extern MKCL_API mkcl_object *_mkcl_va_sp(MKCL, mkcl_narg narg);

#define mkcl_va_start(e,a,p,n,k) {					\
    a[0].narg = (n)-(k);						\
    va_start(a[0].args,p);						\
    a[0].sp = ((n) < MKCL_C_ARGUMENTS_LIMIT) ? 0 : _mkcl_va_sp(e, a[0].narg); \
  }

#define mkcl_va_arg(a)							\
  (a[0].narg--, (a[0].sp ? *(a[0].sp++) : va_arg(a[0].args,mkcl_object)))

#if 0
#define mkcl_va_copy(dest,orig) {	    \
    dest[0].narg = orig[0].narg;	    \
    dest[0].sp = orig[0].sp;		    \
    va_copy(dest[0].args, orig[0].args);    \
  }
#endif

#define mkcl_va_end(a) va_end(a[0].args)
#define	mkcl_check_arg(e,n) do { if (narg != (n)) mkcl_FEwrong_num_arguments_anonym(e, n, n, narg);} while(0)

/***********************
 * RETURN VALUES STACK
 ***********************/

#define MKCL_NVALUES    env->nvalues
#define MKCL_VALUES(n)  env->values[n]
#define mkcl_return0()  return ((MKCL_VALUES(0)=mk_cl_Cnil),(MKCL_NVALUES = 0),mk_cl_Cnil)
  static inline mkcl_object __mkcl_return1(MKCL, const mkcl_object x)
  { env->nvalues = 1; return (env->values[0] = x); }
#define mkcl_return1(x) return (__mkcl_return1(env, (x)))
#define mkcl_returnn(x) return x
#define mkcl_return_no_value { env->values[0] = mk_cl_Cnil; env->nvalues=0; return(mk_cl_Cnil); }
#define mkcl_return_value(val) { const mkcl_object _t__ = (val); env->values[0] = _t__; env->nvalues=1; return(_t__); }
#define mkcl_return_2_values(val0, val1)        \
  { const mkcl_object _t0__ = (val0);           \
    const mkcl_object _t1__ = (val1);           \
    env->values[0] = _t0__;                     \
    env->values[1] = _t1__;                     \
    env->nvalues=2;                             \
    return(_t0__); }
#define mkcl_return_3_values(val0, val1, val2)                          \
  { const mkcl_object _t0__ = (val0);                                   \
    const mkcl_object _t1__ = (val1);                                   \
    const mkcl_object _t2__ = (val2);                                   \
    env->values[0] = _t0__;                                             \
    env->values[1] = _t1__;                                             \
    env->values[2] = _t2__;                                             \
    env->nvalues=3;                                                     \
    return(_t0__); }
#define mkcl_return_4_values(val0, val1, val2, val3)                    \
  { const mkcl_object _t0__ = (val0);                                   \
    const mkcl_object _t1__ = (val1);                                   \
    const mkcl_object _t2__ = (val2);                                   \
    const mkcl_object _t3__ = (val3);                                   \
    env->values[0] = _t0__;                                             \
    env->values[1] = _t1__;                                             \
    env->values[2] = _t2__;                                             \
    env->values[3] = _t3__;                                             \
    env->nvalues=4;                                                     \
    return(_t0__); }


/*************
 * LISP TEMPORARIES STACK
 *************/

  extern MKCL_API mkcl_object * mkcl_grow_temp_stack(MKCL);


#define MKCL_TEMP_STACK_INDEX(env) ((env)->temp_stack_top - (env)->temp_stack)
  
#define MKCL_TEMP_STACK_PUSH(the_env,o) do {		\
    const mkcl_env __env = (the_env);			\
    mkcl_object *__new_top = __env->temp_stack_top;	\
    if (__new_top >= __env->temp_stack_upper_bound) {	\
      __new_top = mkcl_grow_temp_stack(__env);		\
    }							\
    *__new_top = (o);					\
    __env->temp_stack_top = __new_top+1; } while (0)
  
#define MKCL_TEMP_STACK_POP_UNSAFE(env) *(--((env)->temp_stack_top))

#define MKCL_TEMP_STACK_REF(env,n) ((env)->temp_stack_top[n]) /* quite unsafe. JCB */

#define MKCL_TEMP_STACK_SET_INDEX(the_env,ndx) do {	   \
    const mkcl_env __env = (the_env);			   \
    mkcl_object *__new_top = __env->temp_stack + (ndx);	   \
    if (__new_top > __env->temp_stack_top)		   \
      mkcl_FEtemp_stack_advance(__env);			   \
    __env->temp_stack_top = __new_top; } while (0)

#define MKCL_TEMP_STACK_POP_N(the_env,n) do {				\
    const mkcl_env __env = (the_env);					\
    mkcl_object *__new_top = __env->temp_stack_top - (n);		\
    if (__new_top < __env->temp_stack) mkcl_FEtemp_stack_underflow();	\
    __env->temp_stack_top = __new_top; } while (0)
  
#define MKCL_TEMP_STACK_POP_N_UNSAFE(the_env,n) ((the_env)->temp_stack_top -= (n))

#define MKCL_TEMP_STACK_PUSH_N(the_env,n) do {				\
    const mkcl_env __env = (the_env) ;					\
    mkcl_index __aux = (n);						\
    mkcl_object *__new_top = __env->temp_stack_top;			\
    while ((__env->temp_stack_upper_bound - __new_top) <= __aux) {	\
      __new_top = mkcl_grow_temp_stack(__env);				\
    }									\
    __env->temp_stack_top = __new_top + __aux; } while (0)
  
#define MKCL_TEMP_STACK_FRAME_COPY(dest,orig) do {			\
    mkcl_object __dest = (dest);					\
    mkcl_object __orig = (orig);					\
    mkcl_index __size = __orig->frame.size;				\
    mkcl_temp_stack_frame_open(__orig->frame.env, __dest, __size);	\
    memcpy(__dest->frame.base, __orig->frame.base, __size * sizeof(mkcl_object)); \
  } while (0);
  
#define MKCL_TEMP_STACK_FRAME_SET(f,ndx,o) do { (f)->frame.base[(ndx)] = (o); } while(0)
#define MKCL_TEMP_STACK_FRAME_REF(f,ndx) ((f)->frame.base[(ndx)])

/*********************************
 * HIGH LEVEL CONTROL STRUCTURES *
 *********************************/

  /* void mkcl_frs_stack_botch(MKCL); */

#define MKCL_NEWENV_BEGIN {			   \
  const mkcl_env env = MKCL_ENV();		   \
  mkcl_index __i = mkcl_stack_push_values(env);	   \

#define MKCL_NEWENV_END				\
  mkcl_stack_pop_values(env,__i); }

#define MKCL_UNWIND_PROTECT_BEGIN(the_env) do {			\
  volatile bool __unwinding;					\
  mkcl_frame_ptr __next_fr = NULL;				\
  const mkcl_env __the_env = (the_env);				\
  mkcl_index __nr;						\
  /* struct mkcl_frame * const frs_top = __the_env->frs_top; */	\
  if (mkcl_frs_push(__the_env,MKCL_PROTECT_TAG)) {		\
    mkcl_maybe_reset_call_stack_overflow(env);			\
    __unwinding=TRUE; __next_fr=__the_env->nlj_fr;		\
  } else {
  
#define MKCL_UNWIND_PROTECT_EXIT					\
  mkcl_disable_interrupts(__the_env); __unwinding=FALSE; }		\
  { const mkcl_interrupt_status __old_intr_stat = (__the_env)->frs_top->frs_intr; \
  mkcl_frs_pop(__the_env);						\
  /* if (frs_top != (__the_env)->frs_top) mkcl_frs_stack_botch(__the_env); */ \
  __nr = mkcl_stack_push_values(__the_env);
  
#define MKCL_UNWIND_PROTECT_END					\
  mkcl_stack_pop_values(__the_env,__nr);			\
  if (__unwinding) mkcl_unwind(__the_env,__next_fr);		\
  else mkcl_set_interrupt_status(__the_env, &__old_intr_stat);	\
  } } while(0)
  
#define MKCL_NEW_FRAME_ID(env) MKCL_CONS(env, mk_cl_Cnil, mk_cl_Cnil)

#define MKCL_BLOCK_BEGIN(the_env,id) do {   			\
  const mkcl_object __id = MKCL_NEW_FRAME_ID(the_env);		\
  const mkcl_env __the_env = (the_env);				\
  /* struct mkcl_frame * const frs_top = __the_env->frs_top; */	\
  if (mkcl_frs_push(__the_env,__id) == 0) {

#define MKCL_BLOCK_END	}						\
    mkcl_maybe_reset_call_stack_overflow(env);				\
    mkcl_set_interrupt_status(__the_env, &__the_env->frs_top->frs_intr); \
    mkcl_frs_pop(__the_env);						\
    /* if (frs_top != (__the_env)->frs_top) mkcl_frs_stack_botch(__the_env); */ \
} while(0)

#define MKCL_CL_CATCH_BEGIN(the_env,tag) do {			\
  const mkcl_env __the_env = (the_env);				\
  /* struct mkcl_frame * const frs_top = __the_env->frs_top; */	\
  if (mkcl_frs_push(__the_env,tag) == 0) {
  
#define MKCL_CL_CATCH_END }						\
    mkcl_maybe_reset_call_stack_overflow(env);				\
    mkcl_set_interrupt_status(__the_env, &__the_env->frs_top->frs_intr); \
    mkcl_frs_pop(__the_env);						\
  /* if (frs_top != (__the_env)->frs_top) mkcl_frs_stack_botch(__the_env); */ \
} while (0)

#if MKCL_WINDOWS
# define MKCL_CATCH_ALL_BEGIN(the_env) do {				\
  const mkcl_env __the_env = (the_env);					\
  /* struct mkcl_frame * const frs_top = __the_env->frs_top; */		\
  if (mkcl_frs_push(__the_env,MKCL_PROTECT_TAG) == 0) {

# define MKCL_CATCH_ALL_IF_CAUGHT   mkcl_disable_interrupts(__the_env); } else { mkcl_maybe_reset_call_stack_overflow(env);

# define MKCL_CATCH_ALL_END }						\
    mkcl_maybe_reset_call_stack_overflow(env);				\
    mkcl_set_interrupt_status(__the_env, &__the_env->frs_top->frs_intr); \
    mkcl_frs_pop(__the_env);						\
    /* if (frs_top != (__the_env)->frs_top) mkcl_frs_stack_botch(__the_env); */ \
} while(0)
#else /* !MKCL_WINDOWS */

# include <pthread.h>

# ifdef __ANDROID__
#  define pthread_setcancelstate(a, b) /* Android refused to implement pthread_cancel() et al. */
# endif

# define MKCL_CATCH_ALL_BEGIN(the_env) do {				\
  const mkcl_env __the_env = (the_env);					\
  int __old_cancel_state;						\
  pthread_setcancelstate(PTHREAD_CANCEL_DISABLE, &__old_cancel_state);	\
  /* struct mkcl_frame * const frs_top = __the_env->frs_top; */		\
  if (mkcl_frs_push(__the_env,MKCL_PROTECT_TAG) == 0) {

# define MKCL_CATCH_ALL_IF_CAUGHT   mkcl_disable_interrupts(__the_env); } else { mkcl_maybe_reset_call_stack_overflow(env);

# define MKCL_CATCH_ALL_END }						\
    mkcl_maybe_reset_call_stack_overflow(env);				\
    mkcl_set_interrupt_status(__the_env, &__the_env->frs_top->frs_intr); \
    mkcl_frs_pop(__the_env);						\
    pthread_setcancelstate(__old_cancel_state, &__old_cancel_state);	\
    /* if (frs_top != (__the_env)->frs_top) mkcl_frs_stack_botch(__the_env); */ \
} while(0)
#endif /* !MKCL_WINDOWS */

#define MKCL_SETUP_CALL_STACK_ROOT_GUARD(env) (*((env)->frs_org) = *((env)->frs_top))
#define MKCL_UNSET_CALL_STACK_ROOT_GUARD(env) ((env)->frs_org->frs_val = MKCL_OBJNULL) /* Marks an uninitialized call stack root guard. */


/******************************************************/

  extern MKCL_API mkcl_object mk_si_ihs_top(MKCL);
  extern MKCL_API mkcl_object mk_si_ihs_fun(MKCL, mkcl_object arg);
  extern MKCL_API mkcl_object mk_si_ihs_env(MKCL, mkcl_object arg);
  extern MKCL_API mkcl_object mk_si_ihs_next(MKCL, mkcl_object arg);
  extern MKCL_API mkcl_object mk_si_ihs_prev(MKCL, mkcl_object arg);
  extern MKCL_API mkcl_object mk_si_ihs_bds_marker(MKCL, mkcl_object arg);
  extern MKCL_API mkcl_object mk_si_frs_top(MKCL);
  extern MKCL_API mkcl_object mk_si_frs_bds(MKCL, mkcl_object arg);
  extern MKCL_API mkcl_object mk_si_frs_tag(MKCL, mkcl_object arg);
  extern MKCL_API mkcl_object mk_si_frs_ihs(MKCL, mkcl_object arg);
  extern MKCL_API mkcl_object mk_si_bds_top(MKCL);
  extern MKCL_API mkcl_object mk_si_bds_var(MKCL, mkcl_object arg);
  extern MKCL_API mkcl_object mk_si_bds_val(MKCL, mkcl_object arg);
  extern MKCL_API mkcl_object mk_si_sch_frs_base(MKCL, mkcl_object fr, mkcl_object ihs);

  extern MKCL_API mkcl_object mk_si_set_lisp_temp_stack_limit(MKCL, mkcl_object size_limit);
  extern MKCL_API mkcl_object mk_si_get_lisp_temp_stack_limit(MKCL);
  extern MKCL_API mkcl_object mk_si_set_binding_stack_limit(MKCL, mkcl_object size_limit);
  extern MKCL_API mkcl_object mk_si_get_binding_stack_limit(MKCL);
  extern MKCL_API mkcl_object mk_si_set_frame_stack_limit(MKCL, mkcl_object size_limit);
  extern MKCL_API mkcl_object mk_si_get_frame_stack_limit(MKCL);
  extern MKCL_API mkcl_object mk_si_get_call_stack_limit(MKCL);
  
  extern MKCL_API void mkcl_bds_unwind(MKCL, mkcl_index new_bds_top_index);
  extern MKCL_API void mkcl_unwind(MKCL, mkcl_frame_ptr fr) mkcl_noreturn;
  extern MKCL_API mkcl_frame_ptr mkcl_frs_sch(MKCL, mkcl_object frame_id);
  
  extern MKCL_API mkcl_object mk_si_disable_interrupts(MKCL);
  extern MKCL_API mkcl_object mk_si_enable_interrupts(MKCL);
  extern MKCL_API mkcl_object mk_si_interrupt_status(MKCL);

#ifdef __cplusplus
}
#endif

#endif /* MKCL_STACKS_H */
