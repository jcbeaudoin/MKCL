/* -*- mode: c -*- */
/*
    interpreter.c -- Bytecode interpreter.
*/
/*
    Copyright (c) 2001, Juan Jose Garcia Ripoll.
    Copyright (c) 2012,2021, Jean-Claude Beaudoin.

    MKCL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 3 of the License, or (at your option) any later version.

    See file '../../Copyright' for full details.
*/

#include <mkcl/mkcl.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <mkcl/mkcl-inl.h>
#include <mkcl/bytecode.h>
#include <mkcl/internal.h>

void
mkcl_FEtemp_stack_underflow(MKCL)
{
  mkcl_FEerror(env, "Internal error: lisp temporaries stack underflow.",0);
}

void
mkcl_FEtemp_stack_advance(MKCL)
{
  mkcl_FEerror(env, "Internal error: lisp temporaries stack advance beyond currently active region.",0);
}

mkcl_index
mkcl_stack_push_values(MKCL)
{
  mkcl_index i = env->nvalues;
  mkcl_object *b = env->temp_stack_top;
  mkcl_object *p = b + i;
  if (p >= env->temp_stack_upper_bound) {
    b = mkcl_grow_temp_stack(env); /* Can this garantee to grow enough? NO! FIXME! JCB */
    p = b + i;
  }
  env->temp_stack_top = p;
  memcpy(b, env->values, i * sizeof(mkcl_object));
  return i;
}

void
mkcl_stack_pop_values(MKCL, mkcl_index n)
{
  mkcl_object *p = env->temp_stack_top - n;
  if (p < env->temp_stack)
    mkcl_FEtemp_stack_underflow(env);
  env->nvalues = n;
  env->temp_stack_top = p;
  memcpy(env->values, p, n * sizeof(mkcl_object));
}

mkcl_object
mkcl_temp_stack_frame_open(MKCL, mkcl_object f, mkcl_index size)
{
  mkcl_object *base = env->temp_stack_top;
  if (size) {
    while ((env->temp_stack_upper_bound - base) < size) {
      base = mkcl_grow_temp_stack(env);
    }
  }
  f->frame.t = mkcl_t_temp_stack_frame;
  f->frame.stack = env->temp_stack;
  f->frame.base = base;
  f->frame.size = size;
  f->frame.env = env;
  env->temp_stack_top = (base + size);
  return f;
}

void
mkcl_temp_stack_frame_push(MKCL, mkcl_object f, mkcl_object o)
{
  if ( env != f->frame.env ) mkcl_lose(env, "corrupt environment");

  mkcl_object *top = env->temp_stack_top;
  if (top >= env->temp_stack_upper_bound) {
    top = mkcl_grow_temp_stack(env);
  }
  *top = o;
  env->temp_stack_top = ++top;
  f->frame.base = top - (++(f->frame.size));
  f->frame.stack = env->temp_stack;
}

void
mkcl_temp_stack_frame_push_values(MKCL, mkcl_object f)
{
  if ( env != f->frame.env ) mkcl_lose(env, "corrupt environment");

  mkcl_stack_push_values(env);
  f->frame.base = env->temp_stack_top - (f->frame.size += env->nvalues); 
  f->frame.stack = env->temp_stack;
}

mkcl_object
mkcl_temp_stack_frame_pop_values(MKCL, mkcl_object f)
{
  if ( env != f->frame.env ) mkcl_lose(env, "corrupt environment");

  mkcl_index n = f->frame.size % MKCL_MULTIPLE_VALUES_LIMIT;
  mkcl_object o;
  env->nvalues = n;
  env->values[0] = o = mk_cl_Cnil;
  while (n--) {
    env->values[n] = o = f->frame.base[n];
  }
  return o;
}

void
mkcl_temp_stack_frame_close(MKCL, mkcl_object f)
{
  if (f->frame.stack) {
    if ( env != f->frame.env ) mkcl_lose(env, "corrupt environment");

    MKCL_TEMP_STACK_SET_INDEX(env, f->frame.base - f->frame.stack);
  }
}

/* ------------------------------ LEXICAL ENV. ------------------------------ */

/*****************************
 * LEXICAL ENVIRONMENT STACK
 *****************************/
/*
 * A lexical environment is a list of pairs, each one containing either
 * a variable definition, a tagbody or block tag, or a local function
 * definition.
 *
 *	lex_env ---> ( { record }* )
 *	record = variable | function | block_tag | tagbody_tag
 *
 *	variable = ( var_name[symbol] . value )
 *	function = function[bytecode]
 *	block_tag = ( frame-tag[cons] . block_name[symbol] )
 *	tagbody_tag = ( frame-tag[cons] . labels )  
 *      labels = ( label[symbol] . index[fixnum] )
 */

#define bind_var(env, lenv, var, val)	MKCL_CONS(env, MKCL_CONS(env, var, val), (lenv))
#define bind_function(env, lenv, name, fun) 	MKCL_CONS(env, fun, (lenv))
#define bind_frame(env, lenv, id, name)	MKCL_CONS(env, MKCL_CONS(env, id, name), (lenv))

static mkcl_object
mkcl_lex_env_get_record(register mkcl_object lex_env, register int s)
{
  do {
    if (s-- == 0) return MKCL_CONS_CAR(lex_env);
    lex_env = MKCL_CONS_CDR(lex_env);
  } while(1);
}

#define mkcl_lex_env_get_var(lenv,x) MKCL_CONS_CDR(mkcl_lex_env_get_record(lenv,x))
#define mkcl_lex_env_set_var(lenv,x,v) MKCL_RPLACD(mkcl_lex_env_get_record(lenv,x),(v))
#define mkcl_lex_env_get_fun(lenv,x) mkcl_lex_env_get_record(lenv,x)
#define mkcl_lex_env_get_tag(lenv,x) MKCL_CONS_CAR(mkcl_lex_env_get_record(lenv,x))

/* -------------------- AIDS TO THE INTERPRETER -------------------- */

mkcl_object
_mkcl_bytecode_dispatch_vararg(MKCL, mkcl_narg narg, ...)
{
  mkcl_object output;
  MKCL_TEMP_STACK_FRAME_VARARGS_BEGIN(env, narg, narg, frame);
  output = mkcl_interpret(env, frame, mk_cl_Cnil, frame->frame.env->function);
  MKCL_TEMP_STACK_FRAME_VARARGS_END(frame);
  return output;
}

mkcl_object
_mkcl_bytecode_dispatch_f0(MKCL)
{
  return _mkcl_bytecode_dispatch_vararg(env, 0);
}

mkcl_object
_mkcl_bytecode_dispatch_f1(MKCL, mkcl_object x1)
{
  return _mkcl_bytecode_dispatch_vararg(env, 1, x1);
}

mkcl_object
_mkcl_bytecode_dispatch_f2(MKCL, mkcl_object x1, mkcl_object x2)
{
  return _mkcl_bytecode_dispatch_vararg(env, 2, x1, x2);
}

mkcl_object
_mkcl_bytecode_dispatch_f3(MKCL, mkcl_object x1, mkcl_object x2, mkcl_object x3)
{
  return _mkcl_bytecode_dispatch_vararg(env, 3, x1, x2, x3);
}

mkcl_object
_mkcl_bytecode_dispatch_f4(MKCL, mkcl_object x1, mkcl_object x2, mkcl_object x3, mkcl_object x4)
{
  return _mkcl_bytecode_dispatch_vararg(env, 4, x1, x2, x3, x4);
}

mkcl_object
_mkcl_bclosure_dispatch_vararg(MKCL, mkcl_narg narg, ...)
{
  mkcl_object output;
  MKCL_TEMP_STACK_FRAME_VARARGS_BEGIN(env, narg, narg, frame) {
    mkcl_object fun = frame->frame.env->function;
    output = mkcl_interpret(env, frame, fun->bclosure.lex, fun->bclosure.code);
  } MKCL_TEMP_STACK_FRAME_VARARGS_END(frame);
  return output;
}

mkcl_object
_mkcl_bclosure_dispatch_f0(MKCL)
{
  return _mkcl_bclosure_dispatch_vararg(env, 0);
}

mkcl_object
_mkcl_bclosure_dispatch_f1(MKCL, mkcl_object x1)
{
  return _mkcl_bclosure_dispatch_vararg(env, 1, x1);
}

mkcl_object
_mkcl_bclosure_dispatch_f2(MKCL, mkcl_object x1, mkcl_object x2)
{
  return _mkcl_bclosure_dispatch_vararg(env, 2, x1, x2);
}

mkcl_object
_mkcl_bclosure_dispatch_f3(MKCL, mkcl_object x1, mkcl_object x2, mkcl_object x3)
{
  return _mkcl_bclosure_dispatch_vararg(env, 3, x1, x2, x3);
}

mkcl_object
_mkcl_bclosure_dispatch_f4(MKCL, mkcl_object x1, mkcl_object x2, mkcl_object x3, mkcl_object x4)
{
  return _mkcl_bclosure_dispatch_vararg(env, 4, x1, x2, x3, x4);
}


static mkcl_object
close_around(MKCL, mkcl_object producer, mkcl_object fun, mkcl_object lex)
{
  mkcl_object v = mkcl_alloc_raw_bclosure(env);

  v->bclosure.code = fun;
  v->bclosure.lex = lex;
  v->bclosure.f.entry = _mkcl_bclosure_dispatch_vararg;
  v->bclosure.f._[0] = _mkcl_bclosure_dispatch_f0;
  v->bclosure.f._[1] = _mkcl_bclosure_dispatch_f1;
  v->bclosure.f._[2] = _mkcl_bclosure_dispatch_f2;
  v->bclosure.f._[3] = _mkcl_bclosure_dispatch_f3;
  v->bclosure.f._[4] = _mkcl_bclosure_dispatch_f4;
  v->bclosure.producer = producer;
  v->bclosure.name = mk_si_compiled_function_name(env, fun);

  return v;
}

mkcl_object
mkcl_clone_bclosure(MKCL, mkcl_object c0, mkcl_object new_lex)
{
  if (mkcl_Null(new_lex))
    new_lex = mk_cl_copy_list(env, c0->bclosure.lex);

  mkcl_return_value(close_around(env, c0->bclosure.producer, c0->bclosure.code, new_lex));
}

#define SETUP_ENV(the_env) { ihs.lex_env = lex_env; }

/*
 * INTERPRET-FUNCALL is one of the few ways to "exit" the interpreted
 * environment and get into the C/lisp world. Since almost all data
 * from the interpreter is kept in local variables, and frame stacks,
 * binding stacks, etc, are already handled by the C core, only the
 * lexical environment needs to be saved.
 */

#define INTERPRET_FUNCALL(reg0, the_env, frame, narg, fun) {            \
    mkcl_index __n = narg;						\
    SETUP_ENV(the_env);							\
    frame.stack = the_env->temp_stack;					\
    frame.base = the_env->temp_stack_top - (frame.size = __n);		\
    reg0 = mkcl_apply_from_temp_stack_frame(the_env, (mkcl_object)&frame, fun); \
    the_env->temp_stack_top -= __n; }

/* -------------------- THE INTERPRETER -------------------- */

mkcl_object
mkcl_interpret(MKCL, mkcl_object frame, mkcl_object lex_env, mkcl_object bytecode)
{
  MKCL_OFFSET_TABLE;
  const mkcl_env the_env = env;
  const mkcl_object this_func = env->function;
  volatile mkcl_index frame_index = 0;
  mkcl_opcode *vector = (mkcl_opcode*)bytecode->bytecode.code;
  mkcl_object *data = bytecode->bytecode.data;
  mkcl_object reg0, reg1;
  mkcl_index narg;
  struct mkcl_temp_stack_frame frame_aux;
  /* volatile */ struct mkcl_ihs_frame ihs;
  
  mkcl_index cached_nvalues; /* debug JCB */
  long cached_gc_fast_counter;

  /* INV: bytecode is of type mkcl_t_bytecode */

#if 1
  mkcl_call_stack_check(env);
#else
  mkcl_call_stack_check(env, ihs);
#endif
  mkcl_ihs_push(env, &ihs, bytecode, lex_env);
  frame_aux.t = mkcl_t_temp_stack_frame;
  frame_aux.stack = frame_aux.base = 0;
  frame_aux.size = 0;
  frame_aux.env = env;
 BEGIN: /* This label is need by the non-threaded bytecode interpreter. */
  BEGIN_SWITCH {
    CASE(OP_NOP); {
      reg0 = mk_cl_Cnil;
      env->nvalues = 0;
      THREAD_NEXT;
    }
    /* OP_QUOTE
       Sets REG0 to an immediate value.
    */
    CASE(OP_QUOTE); {
      GET_DATA(reg0, vector, data);
      THREAD_NEXT;
    }
    /* OP_VAR	n{arg}, var{symbol}
       Sets REG0 to the value of the n-th local.
       VAR is the name of the variable for readability purposes.
    */
    CASE(OP_VAR); {
      int lex_env_index;
      GET_OPARG(lex_env_index, vector);
      reg0 = mkcl_lex_env_get_var(lex_env, lex_env_index);
      THREAD_NEXT;
    }

    /* OP_VARS	var{symbol}
       Sets REG0 to the value of the symbol VAR.
       VAR should be either a special variable or a constant.
    */
    CASE(OP_VARS); {
      mkcl_object var_name;
      GET_DATA(var_name, vector, data);
      reg0 = MKCL_SYM_VAL(env, var_name);
      if (reg0 == MKCL_OBJNULL)
	mkcl_FEunbound_variable(env, var_name);
      THREAD_NEXT;
    }

    /* OP_CONS, OP_CAR, OP_CDR, etc
       Inlined forms for some functions which act on reg0 and stack.
    */

    CASE(OP_CONS); {
      mkcl_object car = MKCL_TEMP_STACK_POP_UNSAFE(env);
      reg0 = MKCL_CONS(env, car, reg0);
      THREAD_NEXT;
    }

    CASE(OP_CAR); {
      if (!MKCL_LISTP(reg0)) mkcl_FEtype_error_cons(env, reg0);
      reg0 = MKCL_CAR(reg0);
      THREAD_NEXT;
    }

    CASE(OP_CDR); {
      if (!MKCL_LISTP(reg0)) mkcl_FEtype_error_cons(env, reg0);
      reg0 = MKCL_CDR(reg0);
      THREAD_NEXT;
    }

    CASE(OP_LIST);
    reg0 = mkcl_list1(env, reg0);

    CASE(OP_LISTA);	{
      mkcl_index n;
      GET_OPARG(n, vector);
      while (--n) {
	reg0 = MKCL_CONS(env, MKCL_TEMP_STACK_POP_UNSAFE(env), reg0);
      }
      THREAD_NEXT;
    }

    CASE(OP_INT); {
      mkcl_word n;
      GET_OPARG(n, vector);
      reg0 = MKCL_MAKE_FIXNUM(n);
      THREAD_NEXT;
    }

    CASE(OP_PINT); {
      mkcl_word n;
      GET_OPARG(n, vector);
      MKCL_TEMP_STACK_PUSH(env, MKCL_MAKE_FIXNUM(n));
      THREAD_NEXT;
    }

    /* OP_PUSH
       Pushes the object in MKCL_VALUES(0).
    */
    CASE(OP_PUSH); {
      MKCL_TEMP_STACK_PUSH(env, reg0);
      THREAD_NEXT;
    }
    /* OP_PUSHV	n{arg}
       Pushes the value of the n-th local onto the stack.
    */
    CASE(OP_PUSHV); {
      int lex_env_index;
      GET_OPARG(lex_env_index, vector);
      MKCL_TEMP_STACK_PUSH(env, mkcl_lex_env_get_var(lex_env, lex_env_index));
      THREAD_NEXT;
    }

    /* OP_PUSHVS	var{symbol}
       Pushes the value of the symbol VAR onto the stack.
       VAR should be either a special variable or a constant.
    */
    CASE(OP_PUSHVS); {
      mkcl_object var_name, value;
      GET_DATA(var_name, vector, data);
      value = MKCL_SYM_VAL(env, var_name);
      if (value == MKCL_OBJNULL) mkcl_FEunbound_variable(env, var_name);
      MKCL_TEMP_STACK_PUSH(env, value);
      THREAD_NEXT;
    }

    /* OP_PUSHQ	value{object}
       Pushes "value" onto the stack.
    */
    CASE(OP_PUSHQ); {
      mkcl_object aux;
      GET_DATA(aux, vector, data);
      MKCL_TEMP_STACK_PUSH(env, aux);
      THREAD_NEXT;
    }

    CASE(OP_CALLG1); {
      mkcl_object s, fun;
      mkcl_objectfn_fixed f;
      GET_DATA(s, vector, data);
      fun = MKCL_SYM_FUN(s);
      f = fun->cfun.f._[1];
      SETUP_ENV(env);
      env->function = fun;
      reg0 = f(env, reg0);
      THREAD_NEXT;
    }

    CASE(OP_CALLG2); {
      mkcl_object s, fun;
      mkcl_objectfn_fixed f;
      GET_DATA(s, vector, data);
      fun = MKCL_SYM_FUN(s);
      f = fun->cfun.f._[2];
      SETUP_ENV(env);
      env->function = fun;
      reg0 = f(env, MKCL_TEMP_STACK_POP_UNSAFE(env), reg0);
      cached_nvalues = env->nvalues;
      cached_gc_fast_counter = mkcl_core.gc_fast_counter;
      THREAD_NEXT;
    }

    /* OP_CALL	n{arg}
       Calls the function in REG0 with N arguments which
       have been deposited in the stack. The first output value
       is pushed on the stack.
    */
    CASE(OP_CALL); {
      GET_OPARG(narg, vector);
      goto DO_CALL;
    }

    /* OP_CALLG	n{arg}, name{arg}
       Calls the function NAME with N arguments which have been
       deposited in the stack. The first output value is pushed on
       the stack.
    */
    CASE(OP_CALLG); {
      GET_OPARG(narg, vector);
      GET_DATA(reg0, vector, data);
      goto DO_CALL;
    }

    /* OP_FCALL	n{arg}
       Calls a function in the stack with N arguments which
       have been also deposited in the stack. The output values
       are left in MKCL_VALUES(...)
    */
    CASE(OP_FCALL); {
      GET_OPARG(narg, vector);
      reg0 = MKCL_TEMP_STACK_REF(env,-narg-1);
      goto DO_CALL;
    }

    /* OP_MCALL
       Similar to FCALL, but gets the number of arguments from
       the stack (They all have been deposited by OP_PUSHVALUES)
    */
    CASE(OP_MCALL); {
      narg = mkcl_fixnum_to_word(MKCL_TEMP_STACK_POP_UNSAFE(env));
      reg0 = MKCL_TEMP_STACK_REF(env,-narg-1);
      goto DO_CALL;
    }

  DO_CALL: {
      mkcl_object x = reg0;
      mkcl_object frame = (mkcl_object)&frame_aux;
      frame_aux.size = narg;
      frame_aux.base = env->temp_stack_top - narg;
      SETUP_ENV(env);
    AGAIN:
      if (reg0 == MKCL_OBJNULL ) {
	mkcl_FEundefined_function(env, x);
      }
      switch (mkcl_type_of(reg0)) {
      case mkcl_t_cfun:
	{
	  int func_narg = reg0->cfun.narg;

	  env->function = reg0;
	  if ( func_narg < 0 )
	    reg0 = mkcl_APPLY(env, narg, reg0, frame_aux.base);
	  else if ( narg != func_narg )
	    mkcl_FEwrong_num_arguments(env, reg0, func_narg, func_narg, narg);
	  else if ( narg < MKCL_MAX_FAST_FUNC_DISPATCH )
	    reg0 = mkcl_APPLY_fixed(env, narg, reg0->cfun.f._[narg], frame_aux.base);
	  else
	    reg0 = mkcl_APPLY_fixed(env, narg, reg0->cfun.old_entry_fixed, frame_aux.base);
	}
	break;
      case mkcl_t_cclosure:
	env->function = reg0;
	reg0 = mkcl_APPLY(env, narg, reg0, frame_aux.base);
	break;
      case mkcl_t_instance:
	switch (reg0->instance.isgf) {
	case MKCL_STANDARD_DISPATCH:
	  reg0 = _mkcl_standard_dispatch(env, frame, reg0);
	  break;
	case MKCL_USER_DISPATCH:
	  reg0 = reg0->instance.slots[reg0->instance.length - 1];
	  goto AGAIN;
	default:
	  mkcl_FEinvalid_function(env, reg0);
	}
	break;
      case mkcl_t_symbol:
	if (reg0->symbol.stype & mkcl_stp_macro)
	  mkcl_FEundefined_function(env, x);
	reg0 = MKCL_SYM_FUN(reg0);
	if (mkcl_Null(reg0))
	  mkcl_FEundefined_function(env, x);
	goto AGAIN;
      case mkcl_t_bytecode:
	env->function = reg0;
	reg0 = mkcl_interpret(env, frame, mk_cl_Cnil, reg0);
	break;
      case mkcl_t_bclosure:
	env->function = reg0->bclosure.code;
	reg0 = mkcl_interpret(env, frame, reg0->bclosure.lex, reg0->bclosure.code);
	break;
      case mkcl_t_null:
	reg0 = ((mkcl_object) &mk_cl_Cnil_symbol);
	goto AGAIN;
      default:
	mkcl_FEinvalid_function(env, reg0);
      }
      MKCL_TEMP_STACK_POP_N_UNSAFE(env, narg);
      THREAD_NEXT;
    }

    /* OP_POP
       Pops a singe value pushed by a OP_PUSH* operator.
    */
    CASE(OP_POP); {
      reg0 = MKCL_TEMP_STACK_POP_UNSAFE(env);
      THREAD_NEXT;
    }
    /* OP_POP1
       Pops a singe value pushed by a OP_PUSH* operator, ignoring it.
    */
    CASE(OP_POP1); {
      (void) MKCL_TEMP_STACK_POP_UNSAFE(env);
      THREAD_NEXT;
    }
    /* OP_POPREQ
       Checks the arguments list. If there are remaining arguments,
       REG0 = T and the value is on the stack, otherwise REG0 = NIL.
    */
    CASE(OP_POPREQ); {
      if (frame_index >= frame->frame.size) {
	mkcl_FEwrong_num_arguments(env, bytecode->bytecode.name,
                                   frame->frame.size, frame->frame.size, frame_index);
      }
      reg0 = frame->frame.base[frame_index++];
      THREAD_NEXT;
    }
    /* OP_POPOPT
       Checks the arguments list. If there are remaining arguments,
       REG0 = T and the value is on the stack, otherwise REG0 = NIL.
    */
    CASE(OP_POPOPT); {
      if (frame_index >= frame->frame.size) {
	reg0 = mk_cl_Cnil;
      } else {
	MKCL_TEMP_STACK_PUSH(env,frame->frame.base[frame_index++]);
	reg0 = mk_cl_Ct;
      }
      THREAD_NEXT;
    }
    /* OP_NOMORE
       No more arguments.
    */
    CASE(OP_NOMORE); {
      if (frame_index < frame->frame.size)
	mkcl_FEprogram_error(env, "Too many arguments passed to "
			"function ~A~&Argument list: ~S",
			2, bytecode, mk_cl_apply(env, 2, MK_CL_list, frame));
      THREAD_NEXT;
    }
    /* OP_POPREST
       Makes a list out of the remaining arguments.
    */
    CASE(OP_POPREST); {
      mkcl_object *first = frame->frame.base + frame_index;
      mkcl_object *last = frame->frame.base + frame->frame.size;
      for (reg0 = mk_cl_Cnil; last > first; ) {
	reg0 = MKCL_CONS(env, *(--last), reg0);
      }
      THREAD_NEXT;
    }
    /* OP_PUSHKEYS {names-list}
       Checks the stack frame for keyword arguments.
    */
    CASE(OP_PUSHKEYS); {
      mkcl_object keys_list, aok, *first, *last;
      mkcl_index count;
      GET_DATA(keys_list, vector, data);
      first = frame->frame.base + frame_index;
      count = frame->frame.size - frame_index;
      last = first + count;
      if (count & 1) {
	mkcl_FEprogram_error(env, "Function ~A called with odd number "
			"of keyword arguments.",
			1, bytecode);
      }
      aok = MKCL_CONS_CAR(keys_list);
      for (; (keys_list = MKCL_CONS_CDR(keys_list), !mkcl_Null(keys_list)); ) {
	mkcl_object name = MKCL_CONS_CAR(keys_list);
	mkcl_object flag = mk_cl_Cnil;
	mkcl_object value = mk_cl_Cnil;
	mkcl_object *p = first;
	for (; p != last; ++p) {
	  if (*(p++) == name) {
	    count -= 2;
	    if (flag == mk_cl_Cnil) {
	      flag = mk_cl_Ct;
	      value = *p;
	    }
	  }
	}
	if (flag != mk_cl_Cnil) MKCL_TEMP_STACK_PUSH(env, value);
	MKCL_TEMP_STACK_PUSH(env, flag);
      }
      if (count) {
	if (mkcl_Null(aok)) {
	  int aok = 0, mask = 1;
	  mkcl_object *p = first;
	  for (; p != last; ++p) {
	    if (*(p++) == MK_KEY_allow_other_keys) {
	      if (!mkcl_Null(*p)) aok |= mask;
	      mask <<= 1;
	      count -= 2;
	    }
	  }
	  if (count && (aok & 1) == 0) {
	    mkcl_FEprogram_error(env, "Unknown keyword argument "
				 "passed to function ~S.~&"
				 "Argument list: ~S",
				 2, bytecode,
				 mk_cl_apply(env, 2, MK_CL_list, frame));
	  }
	}
      }
      THREAD_NEXT;
    }
    /* OP_EXIT
       Marks the end of a high level construct (BLOCK, CATCH...)
       or a function.
    */
    CASE(OP_EXIT); {
      mkcl_ihs_pop(env);
      return env->values[0] = reg0;
    }
    /* OP_FLET	nfun{arg}, fun1{object}
       ...
       OP_UNBIND nfun
	   
       Executes the enclosed code in a lexical enviroment extended with
       the functions "fun1" ... "funn". Note that we only record the
       index of the first function: the others are after this one.
    */
    CASE(OP_FLET); {
      mkcl_index nfun, first;
      mkcl_object old_lex, *fun;
      GET_OPARG(nfun, vector);
      GET_OPARG(first, vector);
      fun = data + first;
      /* Copy the environment so that functions get it without references
	 to themselves, and then add new closures to the environment. */
      old_lex = lex_env;
      while (nfun--) {
	mkcl_object f = close_around(env, this_func, *(fun++), old_lex);
	lex_env = bind_function(env, lex_env, f->bytecode.name, f);
      }
      THREAD_NEXT;
    }
    /* OP_LABELS	nfun{arg}
       fun1{object}
       ...
       funn{object}
       ...
       OP_UNBIND n

       Executes the enclosed code in a lexical enviroment extended with
       the functions "fun1" ... "funn".
    */
    CASE(OP_LABELS); {
      mkcl_index i, nfun, first;
      mkcl_object *fun, l, new_lex;
      GET_OPARG(nfun, vector);
      GET_OPARG(first, vector);
      fun = data + first;
      /* Build up a new environment with all functions */
      for (new_lex = lex_env, i = nfun; i; i--) {
	mkcl_object f = *(fun++);
	new_lex = bind_function(env, new_lex, f->bytecode.name, f);
      }
      /* Update the closures so that all functions can call each other */
      ;
      for (l = new_lex, i = nfun; i; i--) {
	MKCL_RPLACA(l, close_around(env, this_func, MKCL_CONS_CAR(l), new_lex));
	l = MKCL_CONS_CDR(l);
      }
      lex_env = new_lex;
      THREAD_NEXT;
    }
    /* OP_LFUNCTION	n{arg}, function-name{symbol}
       Calls the local or global function with N arguments
       which have been deposited in the stack.
    */
    CASE(OP_LFUNCTION); {
      int lex_env_index;
      GET_OPARG(lex_env_index, vector);
      reg0 = mkcl_lex_env_get_fun(lex_env, lex_env_index);
      THREAD_NEXT;
    }

    /* OP_FUNCTION	name{symbol}
       Extracts the function associated to a symbol. The function
       may be defined in the global environment or in the local
       environment. This last value takes precedence.
    */
    CASE(OP_FUNCTION); {
      GET_DATA(reg0, vector, data);
      reg0 = mkcl_fdefinition(env, reg0);
      THREAD_NEXT;
    }

    /* OP_CLOSE	name{symbol}
       Extracts the function associated to a symbol. The function
       may be defined in the global environment or in the local
       environment. This last value takes precedence.
    */
    CASE(OP_CLOSE); {
      GET_DATA(reg0, vector, data);
      reg0 = close_around(env, this_func, reg0, lex_env);
      THREAD_NEXT;
    }
    /* OP_GO	n{arg}, tag-ndx{arg}
       Jumps to the tag which is defined for the tagbody
       frame registered at the n-th position in the lexical
       environment. TAG-NDX is the number of tag in the list.
    */
    CASE(OP_GO); {
      mkcl_index lex_env_index;
      mkcl_word tag_ndx;
      GET_OPARG(lex_env_index, vector);
      GET_OPARG(tag_ndx, vector);
      mkcl_go(env, mkcl_lex_env_get_tag(lex_env, lex_env_index), tag_ndx);
      THREAD_NEXT;
    }
    /* OP_RETURN	n{arg}
       Returns from the block whose record in the lexical environment
       occuppies the n-th position.
    */
    CASE(OP_RETURN); {
      int lex_env_index;
      mkcl_object block_record;
      GET_OPARG(lex_env_index, vector);
      /* record = (id . name) */
      block_record = mkcl_lex_env_get_record(lex_env, lex_env_index);
      env->values[0] = reg0;
      mkcl_return_from(env, MKCL_CONS_CAR(block_record),
			MKCL_CONS_CDR(block_record));
      THREAD_NEXT;
    }
    /* OP_THROW
       Jumps to an enclosing CATCH form whose tag matches the one
       of the THROW. The tag is taken from the stack, while the
       output values are left in MKCL_VALUES(...).
    */
    CASE(OP_THROW); {
      mkcl_object tag_name = MKCL_TEMP_STACK_POP_UNSAFE(env);
      env->values[0] = reg0;
      mkcl_throw(env, tag_name);
      THREAD_NEXT;
    }
    /* OP_JMP	label{arg}
       OP_JNIL	label{arg}
       OP_JT	label{arg}
       OP_JEQ	value{object}, label{arg}
       OP_JNEQ	value{object}, label{arg}
       Direct or conditional jumps. The conditional jumps are made
       comparing with the value of REG0.
    */
    CASE(OP_JMP); {
      mkcl_oparg jump;
      GET_OPARG(jump, vector);
      vector += jump - OPARG_SIZE;
      THREAD_NEXT;
    }
    CASE(OP_JNIL); {
      mkcl_oparg jump;
      GET_OPARG(jump, vector);
      if (mkcl_Null(reg0))
	vector += jump - OPARG_SIZE;
      THREAD_NEXT;
    }
    CASE(OP_JT); {
      mkcl_oparg jump;
      GET_OPARG(jump, vector);
      if (!mkcl_Null(reg0))
	vector += jump - OPARG_SIZE;
      THREAD_NEXT;
    }
    CASE(OP_JEQL); {
      mkcl_oparg value, jump;
      GET_OPARG(value, vector);
      GET_OPARG(jump, vector);
      if (mkcl_eql(env, reg0, data[value]))
	vector += jump - OPARG_SIZE;
      THREAD_NEXT;
    }
    CASE(OP_JNEQL); {
      mkcl_oparg value, jump;
      GET_OPARG(value, vector);
      GET_OPARG(jump, vector);
      if (!mkcl_eql(env, reg0, data[value]))
	vector += jump - OPARG_SIZE;
      THREAD_NEXT;
    }

    CASE(OP_ENDP); {
      if (!MKCL_LISTP(reg0)) mkcl_FEtype_error_list(env, reg0);
    }

    CASE(OP_NOT); {
      reg0 = (reg0 == mk_cl_Cnil)? mk_cl_Ct : mk_cl_Cnil;
      THREAD_NEXT;
    }

    /* OP_UNBIND	n{arg}
       Undo "n" local bindings.
    */
    CASE(OP_UNBIND); {
      mkcl_oparg n;
      GET_OPARG(n, vector);
      while (n--)
	lex_env = MKCL_CONS_CDR(lex_env);
      THREAD_NEXT;
    }
    /* OP_UNBINDS	n{arg}
       Undo "n" bindings of special variables.
    */
    CASE(OP_UNBINDS); {
      mkcl_oparg n;
      GET_OPARG(n, vector);
      mkcl_bds_unwind_n(env, n);
      THREAD_NEXT;
    }
    /* OP_BIND	name{symbol}
       OP_PBIND	name{symbol}
       OP_VBIND	nvalue{arg}, name{symbol}
       OP_BINDS	name{symbol}
       OP_PBINDS	name{symbol}
       OP_VBINDS	nvalue{arg}, name{symbol}
       Binds a lexical or special variable to the the
       value of REG0, the first value of the stack (PBIND) or
       to a given value in the values array.
    */
    CASE(OP_BIND); {
      mkcl_object var_name;
      GET_DATA(var_name, vector, data);
      lex_env = bind_var(env, lex_env, var_name, reg0);
      THREAD_NEXT;
    }
    CASE(OP_PBIND); {
      mkcl_object var_name;
      GET_DATA(var_name, vector, data);
      lex_env = bind_var(env, lex_env, var_name, MKCL_TEMP_STACK_POP_UNSAFE(env));
      THREAD_NEXT;
    }
    CASE(OP_VBIND);
    {
      mkcl_index n;
      mkcl_object var_name;
      mkcl_object value;
      GET_OPARG(n, vector);
      GET_DATA(var_name, vector, data);

      if (n < env->nvalues)
	value = env->values[n];
      else
	value = mk_cl_Cnil;
      lex_env = bind_var(env, lex_env, var_name, value);
    }
    THREAD_NEXT;
    CASE(OP_BINDS); {
      mkcl_object var_name;
      GET_DATA(var_name, vector, data);
      mkcl_bds_bind(env, var_name, reg0);
      THREAD_NEXT;
    }
    CASE(OP_PBINDS); {
      mkcl_object var_name;
      GET_DATA(var_name, vector, data);
      mkcl_bds_bind(env, var_name, MKCL_TEMP_STACK_POP_UNSAFE(env));
      THREAD_NEXT;
    }
    CASE(OP_VBINDS); {
      mkcl_index n;
      mkcl_object var_name;
      GET_OPARG(n, vector);
      GET_DATA(var_name, vector, data);
      mkcl_bds_bind(env, var_name, (n < env->nvalues) ? env->values[n] : mk_cl_Cnil);
      THREAD_NEXT;
    }
    /* OP_SETQ	n{arg}
       OP_PSETQ	n{arg}
       OP_SETQS	var-name{symbol}
       OP_PSETQS	var-name{symbol}
       OP_VSETQ	n{arg}, nvalue{arg}
       OP_VSETQS	var-name{symbol}, nvalue{arg}
       Sets either the n-th local or a special variable VAR-NAME,
       to either the value in REG0 (OP_SETQ[S]) or to the 
       first value on the stack (OP_PSETQ[S]), or to a given
       value from the multiple values array (OP_VSETQ[S]). Note
       that NVALUE > 0 strictly.
    */
    CASE(OP_SETQ); {
      int lex_env_index;
      GET_OPARG(lex_env_index, vector);
      mkcl_lex_env_set_var(lex_env, lex_env_index, reg0);
      THREAD_NEXT;
    }
    CASE(OP_SETQS); {
      mkcl_object var;
      GET_DATA(var, vector, data);
      /* INV: Not NIL, and of type mkcl_t_symbol */
      if (var->symbol.stype & mkcl_stp_constant)
	mkcl_FEassignment_to_constant(env, var);
      MKCL_SETQ(env, var, reg0);
      THREAD_NEXT;
    }
    CASE(OP_PSETQ); {
      int lex_env_index;
      GET_OPARG(lex_env_index, vector);
      mkcl_lex_env_set_var(lex_env, lex_env_index, MKCL_TEMP_STACK_POP_UNSAFE(env));
      THREAD_NEXT;
    }
    CASE(OP_PSETQS); {
      mkcl_object var;
      GET_DATA(var, vector, data);
      /* INV: Not NIL, and of type mkcl_t_symbol */
      MKCL_SETQ(env, var, MKCL_TEMP_STACK_POP_UNSAFE(env));
      THREAD_NEXT;
    }
    CASE(OP_VSETQ); {
      mkcl_index lex_env_index;
      mkcl_oparg index;
      GET_OPARG(lex_env_index, vector);
      GET_OPARG(index, vector);
      mkcl_lex_env_set_var(lex_env, lex_env_index,
			  (index >= env->nvalues)? mk_cl_Cnil : env->values[index]);
      THREAD_NEXT;
    }
    CASE(OP_VSETQS); {
      mkcl_object var, v;
      mkcl_oparg index;
      GET_DATA(var, vector, data);
      GET_OPARG(index, vector);
      v = (index >= env->nvalues)? mk_cl_Cnil : env->values[index];
      MKCL_SETQ(env, var, v);
      THREAD_NEXT;
    }
			
    /* OP_BLOCK	constant
       OP_DO
       OP_CATCH

       OP_FRAME	label{arg}
       ...
       OP_EXIT_FRAME
       label:
    */

    CASE(OP_BLOCK); {
      GET_DATA(reg0, vector, data);
      reg1 = MKCL_NEW_FRAME_ID(env);
      lex_env = bind_frame(env, lex_env, reg1, reg0);
      THREAD_NEXT;
    }
    CASE(OP_DO); {
      reg0 = mk_cl_Cnil;
      reg1 = MKCL_NEW_FRAME_ID(env);
      lex_env = bind_frame(env, lex_env, reg1, reg0);
      THREAD_NEXT;
    }
    CASE(OP_CATCH); {
      reg1 = reg0;
      lex_env = bind_frame(env, lex_env, reg1, reg0);
      THREAD_NEXT;
    }
    CASE(OP_FRAME); {
      mkcl_opcode *exit;
      GET_LABEL(exit, vector);
      MKCL_TEMP_STACK_PUSH(env, lex_env);
      MKCL_TEMP_STACK_PUSH(env, (mkcl_object)exit);
      if (mkcl_frs_push(env,reg1) == 0) {
	THREAD_NEXT;
      } else {
	mkcl_maybe_reset_call_stack_overflow(env);
	reg0 = env->values[0];
	vector = (mkcl_opcode *)MKCL_TEMP_STACK_REF(env,-1); /* FIXME! */
	lex_env = MKCL_TEMP_STACK_REF(env,-2);
	goto DO_EXIT_FRAME;
      }
    }
    /* OP_FRAMEID	0
       OP_TAGBODY	n{arg}
       label1
       ...
       labeln
       label1:
       ...
       labeln:
       ...
       OP_EXIT_TAGBODY

       High level construct for the TAGBODY form.
    */
    CASE(OP_TAGBODY); {
      int n;
      GET_OPARG(n, vector);
      MKCL_TEMP_STACK_PUSH(env, lex_env);
      MKCL_TEMP_STACK_PUSH(env, (mkcl_object)vector); /* FIXME! */
      vector += n * OPARG_SIZE;
      if (mkcl_frs_push(env,reg1) != 0) {
	mkcl_opcode *table = (mkcl_opcode *)MKCL_TEMP_STACK_REF(env,-1);
	lex_env = MKCL_TEMP_STACK_REF(env,-2);
	/* Wait here for gotos. Each goto sets
	   go_label_index to an integer which ranges from 0
	   to ntags-1, depending on the tag. These
	   numbers are indices into the jump table and
	   are computed at compile time. */
	table = table + env->go_label_index * OPARG_SIZE;
	vector = table + *(mkcl_oparg *)table;

	mkcl_maybe_reset_call_stack_overflow(env); /* JCB */
	mkcl_set_interrupt_status(env, &env->frs_top->frs_intr); /* JCB */
      }
      THREAD_NEXT;
    }
    CASE(OP_EXIT_TAGBODY); {
      reg0 = mk_cl_Cnil;
    }
    CASE(OP_EXIT_FRAME); {
    DO_EXIT_FRAME:
      mkcl_set_interrupt_status(env, &env->frs_top->frs_intr);
      mkcl_frs_pop(env);
      MKCL_TEMP_STACK_POP_N_UNSAFE(env, 2);
      lex_env = MKCL_CONS_CDR(lex_env);
      THREAD_NEXT;
    }
    CASE(OP_NIL); {
      reg0 = mk_cl_Cnil;
      THREAD_NEXT;
    }
    CASE(OP_PUSHNIL); {
      MKCL_TEMP_STACK_PUSH(env, mk_cl_Cnil);
      THREAD_NEXT;
    }
    CASE(OP_VALUEREG0); {
      env->nvalues = 1;
      THREAD_NEXT;
    }

    /* OP_PUSHVALUES
       Pushes the values output by the last form, plus the number
       of values.
    */
  PUSH_VALUES:
    CASE(OP_PUSHVALUES); {
      mkcl_index i = env->nvalues;
      MKCL_TEMP_STACK_PUSH_N(env, i+1);
      env->values[0] = reg0;
      memcpy(&MKCL_TEMP_STACK_REF(env, -(i+1)), env->values, i * sizeof(mkcl_object));
      MKCL_TEMP_STACK_REF(env, -1) = MKCL_MAKE_FIXNUM(env->nvalues);
      THREAD_NEXT;
    }
    /* OP_PUSHMOREVALUES
       Adds more values to the ones pushed by OP_PUSHVALUES.
    */
    CASE(OP_PUSHMOREVALUES); {
      mkcl_index n = mkcl_fixnum_to_word(MKCL_TEMP_STACK_REF(env,-1));
      mkcl_index i = env->nvalues;
      MKCL_TEMP_STACK_PUSH_N(env, i);
      env->values[0] = reg0;
      memcpy(&MKCL_TEMP_STACK_REF(env, -(i+1)), env->values, i * sizeof(mkcl_object));
      MKCL_TEMP_STACK_REF(env, -1) = MKCL_MAKE_FIXNUM(n + i);
      THREAD_NEXT;
    }
    /* OP_POPVALUES
       Pops all values pushed by a OP_PUSHVALUES operator.
    */
    CASE(OP_POPVALUES); {
      mkcl_object *dest = env->values;
      int n = env->nvalues = mkcl_fixnum_to_word(MKCL_TEMP_STACK_POP_UNSAFE(env));
      if (n == 0) {
	*dest = reg0 = mk_cl_Cnil;
	THREAD_NEXT;
      } else if (n == 1) {
	*dest = reg0 = MKCL_TEMP_STACK_POP_UNSAFE(env);
	THREAD_NEXT;
      } else {
	MKCL_TEMP_STACK_POP_N_UNSAFE(env,n);
	memcpy(dest, &MKCL_TEMP_STACK_REF(env,0), n * sizeof(mkcl_object));
	reg0 = *dest;
	THREAD_NEXT;
      }
    }
    /* OP_VALUES	n{arg}
       Pop N values from the stack and store them in MKCL_VALUES(...)
       Note that N is strictly > 0.
    */
    CASE(OP_VALUES); {
      mkcl_word n;
      GET_OPARG(n, vector);
      env->nvalues = n;
      MKCL_TEMP_STACK_POP_N_UNSAFE(env, n);
      memcpy(env->values, &MKCL_TEMP_STACK_REF(env, 0), n * sizeof(mkcl_object));
      reg0 = env->values[0];
      THREAD_NEXT;
    }
    /* OP_NTHVAL
       Set MKCL_VALUES(0) to the N-th value of the MKCL_VALUES(...) list.
       The index N-th is extracted from the top of the stack.
    */
    CASE(OP_NTHVAL); {
      mkcl_word n = mkcl_fixnum_to_word(MKCL_TEMP_STACK_POP_UNSAFE(env));
      if (n < 0) {
	mkcl_FEerror(env, "Wrong index passed to NTH-VAL", 1, MKCL_MAKE_FIXNUM(n));
      } else if ((mkcl_index)n >= env->nvalues) {
	reg0 = mk_cl_Cnil;
      } else if (n) {
	reg0 = env->values[n];
      }
      THREAD_NEXT;
    }
    /* OP_PROTECT	label
       ...	; code to be protected and whose value is output
       OP_PROTECT_NORMAL
       label:
       ...	; code executed at exit
       OP_PROTECT_EXIT

       High level construct for UNWIND-PROTECT. The first piece of code is
       executed and its output value is saved. Then the second piece of code
       is executed and the output values restored. The second piece of code
       is always executed, even if a THROW, RETURN or GO happen within the
       first piece of code.
    */
    CASE(OP_PROTECT); {
      mkcl_opcode *exit;
      GET_LABEL(exit, vector);
      MKCL_TEMP_STACK_PUSH(env, lex_env);
      MKCL_TEMP_STACK_PUSH(env, (mkcl_object)exit);
      if (mkcl_frs_push(env,MKCL_PROTECT_TAG) != 0) {
	const mkcl_interrupt_status old_intr_stat = (env)->frs_top->frs_intr; /* JCB */
	mkcl_maybe_reset_call_stack_overflow(env); /* JCB */
	mkcl_frs_pop(env);
	vector = (mkcl_opcode *)MKCL_TEMP_STACK_POP_UNSAFE(env);
	lex_env = MKCL_TEMP_STACK_POP_UNSAFE(env);
	reg0 = env->values[0];
	/* The following line pushes a negative value on the stack that marks abnormal exit. */
	MKCL_TEMP_STACK_PUSH(env, MKCL_MAKE_FIXNUM(env->nlj_fr - env->frs_top));
	MKCL_TEMP_STACK_PUSH(env, (mkcl_object) (intptr_t) old_intr_stat.disable_interrupts); /* JCB */
#if MKCL_DEBUG_INTERRUPT_MASK
	MKCL_TEMP_STACK_PUSH(env, (mkcl_object) old_intr_stat.interrupt_disabler_file); /* JCB */
	MKCL_TEMP_STACK_PUSH(env, (mkcl_object) old_intr_stat.interrupt_disabler_lineno); /* JCB */
#endif
	goto PUSH_VALUES;
      }
      THREAD_NEXT;
    }
    CASE(OP_PROTECT_NORMAL); {
      const mkcl_interrupt_status old_intr_stat = (env)->frs_top->frs_intr; /* JCB */
      mkcl_bds_unwind(env, env->frs_top->frs_bds_top_index);
      mkcl_frs_pop(env);
      (void) MKCL_TEMP_STACK_POP_UNSAFE(env);
      lex_env = MKCL_TEMP_STACK_POP_UNSAFE(env);
      MKCL_TEMP_STACK_PUSH(env, MKCL_MAKE_FIXNUM(1));  /* value of 1 marks normal exit. JCB */
      MKCL_TEMP_STACK_PUSH(env, (mkcl_object) (intptr_t) old_intr_stat.disable_interrupts); /* JCB */
#if MKCL_DEBUG_INTERRUPT_MASK
      MKCL_TEMP_STACK_PUSH(env, (mkcl_object) old_intr_stat.interrupt_disabler_file); /* JCB */
      MKCL_TEMP_STACK_PUSH(env, (mkcl_object) old_intr_stat.interrupt_disabler_lineno); /* JCB */
#endif
      goto PUSH_VALUES;
    }
    CASE(OP_PROTECT_EXIT); {
      mkcl_interrupt_status old_intr_stat;
      mkcl_word n = env->nvalues = mkcl_fixnum_to_word(MKCL_TEMP_STACK_POP_UNSAFE(env));
      while (n--)
	env->values[n] = MKCL_TEMP_STACK_POP_UNSAFE(env);
      reg0 = env->values[0];
#if MKCL_DEBUG_INTERRUPT_MASK
      old_intr_stat.interrupt_disabler_lineno = (size_t) MKCL_TEMP_STACK_POP_UNSAFE(env); /* JCB */
      old_intr_stat.interrupt_disabler_file = (char *) MKCL_TEMP_STACK_POP_UNSAFE(env); /* JCB */
#endif
      old_intr_stat.disable_interrupts= (int) (intptr_t) MKCL_TEMP_STACK_POP_UNSAFE(env); /* JCB */
      n = mkcl_fixnum_to_word(MKCL_TEMP_STACK_POP_UNSAFE(env)); /* exit type marker. */
      if (n <= 0)
	mkcl_unwind(env, env->frs_top + n);
      else
	mkcl_set_interrupt_status(env, &old_intr_stat);  /* JCB */
      THREAD_NEXT;
    }

    /* OP_PROGV	bindings{list}
       ...
       OP_EXIT
       Execute the code enclosed with the special variables in BINDINGS
       set to the values in the list which was passed in MKCL_VALUES(0).
    */
    CASE(OP_PROGV); {
      mkcl_object values = reg0;
      mkcl_object vars = MKCL_TEMP_STACK_POP_UNSAFE(env);
      mkcl_index n;
      for (n = 0; !mkcl_endp(env, vars); n++, vars = MKCL_CONS_CDR(vars)) {
	mkcl_object var = MKCL_CONS_CAR(vars);
	if (mkcl_type_of(var) != mkcl_t_symbol)
	  mkcl_FEinvalid_variable(env, "progv asked to bind ~s, which is not a symbol.", var);
	if (values == mk_cl_Cnil) {
	  mkcl_bds_bind(env, var, MKCL_OBJNULL);
	} else {
	  mkcl_bds_bind(env, var, mk_cl_car(env, values));
	  values = MKCL_CONS_CDR(values);
	}
      }
      MKCL_TEMP_STACK_PUSH(env, MKCL_MAKE_FIXNUM(n));
      THREAD_NEXT;
    }
    CASE(OP_EXIT_PROGV); {
      mkcl_index n = mkcl_fixnum_to_word(MKCL_TEMP_STACK_POP_UNSAFE(env));
      mkcl_bds_unwind_n(env, n);
      THREAD_NEXT;
    }

    CASE(OP_STEPIN); {
      mkcl_object form;
      mkcl_object a = MKCL_SYM_VAL(env, MK_SI_DYNVAR_step_action);
      mkcl_index n;
      GET_DATA(form, vector, data);
      SETUP_ENV(env);
      env->values[0] = reg0;
      n = mkcl_stack_push_values(env);
      if (a == mk_cl_Ct) {
	/* We are stepping in, but must first ask the user
	 * what to do. */
	MKCL_SETQ(env, MK_SI_DYNVAR_step_level,
		 mk_cl_1P(env, MKCL_SYM_VAL(env, MK_SI_DYNVAR_step_level)));
	MKCL_TEMP_STACK_PUSH(env, form);
	INTERPRET_FUNCALL(form, env, frame_aux, 1, MK_SI_stepper);
      } else if (a != mk_cl_Cnil) {
	/* The user told us to step over. *step-level* contains
	 * an integer number that, when it becomes 0, means
	 * that we have finished stepping over. */
	MKCL_SETQ(env, MK_SI_DYNVAR_step_action, mk_cl_1P(env, a));
      } else {
	/* We are not inside a STEP form. This should
	 * actually never happen. */
      }
      mkcl_stack_pop_values(env, n);
      reg0 = env->values[0];
      THREAD_NEXT;
    }
    CASE(OP_STEPCALL); {
      /* We are going to call a function. However, we would
       * like to step _in_ the function. STEPPER takes care of
       * that. */
      mkcl_word n;
      GET_OPARG(n, vector);
      SETUP_ENV(env);
      if (MKCL_SYM_VAL(env, MK_SI_DYNVAR_step_action) == mk_cl_Ct) {
	MKCL_TEMP_STACK_PUSH(env, reg0);
	INTERPRET_FUNCALL(reg0, env, frame_aux, 1, MK_SI_stepper);
      }
      INTERPRET_FUNCALL(reg0, env, frame_aux, n, reg0);
    }
    CASE(OP_STEPOUT); {
      mkcl_object a = MKCL_SYM_VAL(env, MK_SI_DYNVAR_step_action);
      mkcl_index n;
      SETUP_ENV(env);
      env->values[0] = reg0;
      n = mkcl_stack_push_values(env);
      if (a == mk_cl_Ct) {
	/* We exit one stepping level */
	MKCL_SETQ(env, MK_SI_DYNVAR_step_level,
		 mk_cl_1M(env, MKCL_SYM_VAL(env, MK_SI_DYNVAR_step_level)));
      } else if (a == MKCL_MAKE_FIXNUM(0)) {
	/* We are back to the level in which the user
	 * selected to step over. */
	MKCL_SETQ(env, MK_SI_DYNVAR_step_action, mk_cl_Ct);
      } else if (a != mk_cl_Cnil) {
	MKCL_SETQ(env, MK_SI_DYNVAR_step_action, mk_cl_1M(env, a));
      } else {
	/* Not stepping, nothing to be done. */
      }
      mkcl_stack_pop_values(env, n);
      reg0 = env->values[0];
      THREAD_NEXT;
    }
  }
}


