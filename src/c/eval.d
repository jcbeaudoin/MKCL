/* -*- mode: c -*- */
/*
    eval.c -- Eval.
*/
/*
    Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
    Copyright (c) 1990, Giuseppe Attardi.
    Copyright (c) 2001, Juan Jose Garcia Ripoll.
    Copyright (c) 2010-2012,2021 Jean-Claude Beaudoin.

    MKCL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 3 of the License, or (at your option) any later version.

    See file '../../Copyright' for full details.
*/


#include <mkcl/mkcl.h>
#include <mkcl/mkcl-inl.h>
#include <mkcl/internal.h>

mkcl_object
mkcl_apply_from_temp_stack_frame(MKCL, mkcl_object frame, mkcl_object x)
{
  mkcl_object *sp = frame->frame.base;
  mkcl_index narg = frame->frame.size;
  mkcl_object fun = x;
 AGAIN:
  env->function = fun;
  if (fun == MKCL_OBJNULL || fun == mk_cl_Cnil)
    mkcl_FEundefined_function(env, x);
  switch (mkcl_type_of(fun)) {
  case mkcl_t_cfun:
    return mkcl_APPLY(env, narg, fun, sp);
  case mkcl_t_cclosure:
    return mkcl_APPLY(env, narg, fun, sp);
  case mkcl_t_instance:
    switch (fun->instance.isgf) {
    case MKCL_STANDARD_DISPATCH:
      return _mkcl_standard_dispatch(env, frame, fun);
    case MKCL_USER_DISPATCH:
      fun = fun->instance.slots[fun->instance.length - 1];
      break;
    default:
      mkcl_FEinvalid_function(env, fun);
    }
    goto AGAIN;
  case mkcl_t_symbol:
    if (fun->symbol.stype & mkcl_stp_macro)
      mkcl_FEundefined_function(env, x);
    fun = MKCL_SYM_FUN(fun);
    goto AGAIN;
  case mkcl_t_bytecode:
    return mkcl_interpret(env, frame, mk_cl_Cnil, fun);
  case mkcl_t_bclosure:
    return mkcl_interpret(env, frame, fun->bclosure.lex, fun->bclosure.code);
  default:
    mkcl_FEinvalid_function(env, x);
  }
}


mkcl_object
mk_cl_funcall(MKCL, mkcl_narg narg, mkcl_object function, ...)
{
  mkcl_object output;
  const mkcl_narg n = --narg;

  mkcl_call_stack_check(env);
  {
    MKCL_TEMP_STACK_FRAME_VARARGS_BEGIN(env, narg, function, frame);
    {
      mkcl_object *sp = frame->frame.base;
      const mkcl_object fun = function;

      env->function = fun;
      if (fun == MKCL_OBJNULL || fun == mk_cl_Cnil)
	mkcl_FEundefined_function(env, function);
      else
	switch (mkcl_type_of(fun)) {
	case mkcl_t_cfun:
	  {
	    const mkcl_objectfn fn = fun->cfun.f.entry;

	    switch (n) {
	    case 0: output = fun->cfun.f._[0](env); break;
	    case 1: output = fun->cfun.f._[1](env, sp[0]); break;
	    case 2: output = fun->cfun.f._[2](env, sp[0],sp[1]); break;
	    case 3: output = fun->cfun.f._[3](env, sp[0],sp[1],sp[2]); break;
	    case 4: output = fun->cfun.f._[4](env, sp[0],sp[1],sp[2],sp[3]); break;
	    case 5: output = (*fn)(env, n, sp[0],sp[1],sp[2],sp[3],sp[4]); break;
	    case 6: output = (*fn)(env, n, sp[0],sp[1],sp[2],sp[3],sp[4],sp[5]); break;
	    case 7: output = (*fn)(env, n, sp[0],sp[1],sp[2],sp[3],sp[4],sp[5],sp[6]); break;
	    default: output = mkcl_APPLY(env, n, fun, sp); break;
	    }
	  }
	  break;
	case mkcl_t_cclosure:
	  {
	    const mkcl_objectfn fn = fun->cclosure.f.entry;

	    switch (n) {
	    case 0: output = fun->cclosure.f._[0](env); break;
	    case 1: output = fun->cclosure.f._[1](env, sp[0]); break;
	    case 2: output = fun->cclosure.f._[2](env, sp[0],sp[1]); break;
	    case 3: output = fun->cclosure.f._[3](env, sp[0],sp[1],sp[2]); break;
	    case 4: output = fun->cclosure.f._[4](env, sp[0],sp[1],sp[2],sp[3]); break;
	    case 5: output = (*fn)(env, n, sp[0],sp[1],sp[2],sp[3],sp[4]); break;
	    case 6: output = (*fn)(env, n, sp[0],sp[1],sp[2],sp[3],sp[4],sp[5]); break;
	    case 7: output = (*fn)(env, n, sp[0],sp[1],sp[2],sp[3],sp[4],sp[5],sp[6]); break;
	    default: output = mkcl_APPLY(env, n, fun, sp); break;
	    }
	  }
	  break;

	default:
	  output = mkcl_apply_from_temp_stack_frame(env, frame, function);
	  break;
	}
    }
    MKCL_TEMP_STACK_FRAME_VARARGS_END(frame);
  }
  return output;
}

mkcl_object mk_cl_apply(MKCL, mkcl_narg narg, mkcl_object fun, mkcl_object lastarg, ...)
{
  mkcl_call_stack_check(env);
  {
    mkcl_setup_for_rest(env, @'apply', 2, narg, lastarg, args);

    if (narg == 2 && mkcl_type_of(lastarg) == mkcl_t_temp_stack_frame) {
      {
        mkcl_object *sp = lastarg->frame.base;
        const mkcl_narg n = lastarg->frame.size;

        mkcl_va_end(args);
        env->function = fun;
        if (fun == MKCL_OBJNULL || fun == mk_cl_Cnil)
          mkcl_FEundefined_function(env, fun);
        else
          switch (mkcl_type_of(fun)) {
          case mkcl_t_cfun:
            {
              const mkcl_objectfn fn = fun->cfun.f.entry;

              switch (n) {
              case 0: return fun->cfun.f._[0](env); break;
              case 1: return fun->cfun.f._[1](env, sp[0]); break;
              case 2: return fun->cfun.f._[2](env, sp[0],sp[1]); break;
              case 3: return fun->cfun.f._[3](env, sp[0],sp[1],sp[2]); break;
              case 4: return fun->cfun.f._[4](env, sp[0],sp[1],sp[2],sp[3]); break;
              case 5: return (*fn)(env, n, sp[0],sp[1],sp[2],sp[3],sp[4]); break;
              case 6: return (*fn)(env, n, sp[0],sp[1],sp[2],sp[3],sp[4],sp[5]); break;
              case 7: return (*fn)(env, n, sp[0],sp[1],sp[2],sp[3],sp[4],sp[5],sp[6]); break;
              default: return mkcl_APPLY(env, n, fun, sp); break;
              }
            }
            break;
          case mkcl_t_cclosure:
            {
              const mkcl_objectfn fn = fun->cclosure.f.entry;

              switch (n) {
              case 0: return fun->cclosure.f._[0](env); break;
              case 1: return fun->cclosure.f._[1](env, sp[0]); break;
              case 2: return fun->cclosure.f._[2](env, sp[0],sp[1]); break;
              case 3: return fun->cclosure.f._[3](env, sp[0],sp[1],sp[2]); break;
              case 4: return fun->cclosure.f._[4](env, sp[0],sp[1],sp[2],sp[3]); break;
              case 5: return (*fn)(env, n, sp[0],sp[1],sp[2],sp[3],sp[4]); break;
              case 6: return (*fn)(env, n, sp[0],sp[1],sp[2],sp[3],sp[4],sp[5]); break;
              case 7: return (*fn)(env, n, sp[0],sp[1],sp[2],sp[3],sp[4],sp[5],sp[6]); break;
              default: return mkcl_APPLY(env, n, fun, sp); break;
              }
            }
            break;

          default:
            return mkcl_apply_from_temp_stack_frame(env, lastarg, fun);
          }
      }
    } else {
      mkcl_object out;
      mkcl_index i;
      struct mkcl_temp_stack_frame frame_aux;
      const mkcl_object frame = mkcl_temp_stack_frame_open(env, (mkcl_object)&frame_aux, narg -= 2);
    
      for (i = 0; i < narg; i++) {
        MKCL_TEMP_STACK_FRAME_SET(frame, i, lastarg);
        lastarg = mkcl_va_arg(args);
      }
      if (mkcl_type_of(lastarg) == mkcl_t_temp_stack_frame) {
        /* This could be replaced with a memcpy() */
        for (i = 0; i < lastarg->frame.size; i++) {
          mkcl_temp_stack_frame_push(env, frame, lastarg->frame.base[i]);
        }
      } else mkcl_loop_for_in (env, lastarg) {
          if (i >= MKCL_CALL_ARGUMENTS_LIMIT) {
            mkcl_temp_stack_frame_close(env, frame);
            mkcl_FEprogram_error(env, "CALL-ARGUMENTS-LIMIT exceeded",0);
          }
          mkcl_temp_stack_frame_push(env, frame, MKCL_CAR(lastarg));
          i++;
        } mkcl_end_loop_for_in;
      {
        mkcl_object *sp = frame->frame.base;
        const mkcl_narg n = frame->frame.size;

        env->function = fun;
        if (fun == MKCL_OBJNULL || fun == mk_cl_Cnil)
          mkcl_FEundefined_function(env, fun);
        else
          switch (mkcl_type_of(fun)) {
          case mkcl_t_cfun:
            {
              const mkcl_objectfn fn = fun->cfun.f.entry;

              switch (n) {
              case 0: out = fun->cfun.f._[0](env); break;
              case 1: out = fun->cfun.f._[1](env, sp[0]); break;
              case 2: out = fun->cfun.f._[2](env, sp[0],sp[1]); break;
              case 3: out = fun->cfun.f._[3](env, sp[0],sp[1],sp[2]); break;
              case 4: out = fun->cfun.f._[4](env, sp[0],sp[1],sp[2],sp[3]); break;
              case 5: out = (*fn)(env, n, sp[0],sp[1],sp[2],sp[3],sp[4]); break;
              case 6: out = (*fn)(env, n, sp[0],sp[1],sp[2],sp[3],sp[4],sp[5]); break;
              case 7: out = (*fn)(env, n, sp[0],sp[1],sp[2],sp[3],sp[4],sp[5],sp[6]); break;
              default: out = mkcl_APPLY(env, n, fun, sp); break;
              }
            }
            break;
          case mkcl_t_cclosure:
            {
              const mkcl_objectfn fn = fun->cclosure.f.entry;

              switch (n) {
              case 0: out = fun->cclosure.f._[0](env); break;
              case 1: out = fun->cclosure.f._[1](env, sp[0]); break;
              case 2: out = fun->cclosure.f._[2](env, sp[0],sp[1]); break;
              case 3: out = fun->cclosure.f._[3](env, sp[0],sp[1],sp[2]); break;
              case 4: out = fun->cclosure.f._[4](env, sp[0],sp[1],sp[2],sp[3]); break;
              case 5: out = (*fn)(env, n, sp[0],sp[1],sp[2],sp[3],sp[4]); break;
              case 6: out = (*fn)(env, n, sp[0],sp[1],sp[2],sp[3],sp[4],sp[5]); break;
              case 7: out = (*fn)(env, n, sp[0],sp[1],sp[2],sp[3],sp[4],sp[5],sp[6]); break;
              default: out = mkcl_APPLY(env, n, fun, sp); break;
              }
            }
            break;

          default:
            out = mkcl_apply_from_temp_stack_frame(env, frame, fun);
            break;
          }
      }

      mkcl_temp_stack_frame_close(env, frame);
      mkcl_va_end(args);
      return out;
    }
  }
}

mkcl_object
mk_cl_eval(MKCL, mkcl_object form)
{
  mkcl_call_stack_check(env);
  return mk_si_eval_in_env(env, 1, form);
}

@(defun constantp (arg &optional lex_env)
  mkcl_object flag;
@
  switch (mkcl_type_of(arg)) {
  case mkcl_t_cons:
    if (MKCL_CAR(arg) == @'quote') {
      flag = mk_cl_Ct;
    } else {
      flag = mk_cl_Cnil;
    }
    break;
  case mkcl_t_symbol:
    if (mkcl_Null(arg))
      flag = mk_cl_Ct;
    else
      flag = (arg->symbol.stype & mkcl_stp_constant) ? mk_cl_Ct : mk_cl_Cnil;
	break;
  default:
    flag = mk_cl_Ct;
  }
  mkcl_return_value(flag);
@)


extern inline MKCL_API mkcl_object mkcl_validate_function(MKCL, mkcl_object fun);
extern inline MKCL_API mkcl_object mkcl_validate_sym_fun(MKCL, mkcl_object sym);
