/* -*- mode: c  -*- */
/*
    Copyright (c) 2022, Jean-Claude Beaudoin.

    This program is under GNU LGPL, you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 3 of the License, or (at your option) any later version.

    See file '../../../Copyright' for full details.
*/

#include <mkcl/mkcl.h>
#include <mkcl/internal.h>

mkcl_object mkcl_eval_parsed_form(MKCL, mkcl_object proto_closure, mkcl_object pform)
{
  if ( mk_cl_consp(env, pform) )
    {
      mkcl_object value = mk_cl_apply(env, 2, mk_cl_car(env, pform), mk_cl_cdr(env, pform));
      return value; /* M-V passthrough. */
    }
  else if ( mkcl_type_of(pform) == mkcl_t_variable_info )
    {
      mkcl_object value = mk_cl_Cnil; /* evaluate variable here. */
      mkcl_return_value(value);
    }
  else
    mkcl_return_value(pform);
}

mkcl_object mk_si_parsed_block(MKCL, mkcl_object proto_closure,
                               mkcl_object block, mkcl_object body)
{
  mkcl_object pform = mk_cl_Cnil;

  mkcl_call_stack_check(env);
  mkcl_return_value(pform);
}

struct mkcl_cfun mk_si_parsed_block_cfunobj = MKCL_CFUN3(mk_si_parsed_block, (mkcl_object) &MK_SI_parsed_block);


mkcl_object mk_si_parsed_catch(MKCL, mkcl_object proto_closure,
                               mkcl_object tag, mkcl_object body)
{
  mkcl_object pform = mk_cl_Cnil;

  mkcl_call_stack_check(env);
  mkcl_return_value(pform);
}

struct mkcl_cfun mk_si_parsed_catch_cfunobj = MKCL_CFUN3(mk_si_parsed_catch, (mkcl_object) &MK_SI_parsed_catch);


mkcl_object mk_si_parsed_eval_when(MKCL, mkcl_object proto_closure,
                                   mkcl_object situation, mkcl_object body)
{
  mkcl_object pform = mk_cl_Cnil;

  mkcl_call_stack_check(env);
  mkcl_return_value(pform);
}

struct mkcl_cfun mk_si_parsed_eval_when_cfunobj = MKCL_CFUN3(mk_si_parsed_eval_when, (mkcl_object) &MK_SI_parsed_eval_when);


mkcl_object mk_si_parsed_flet(MKCL, mkcl_object proto_closure,
                              mkcl_object local_funs, mkcl_object body)
{
  mkcl_object pform = mk_cl_Cnil;

  mkcl_call_stack_check(env);
  mkcl_return_value(pform);
}

struct mkcl_cfun mk_si_parsed_flet_cfunobj = MKCL_CFUN3(mk_si_parsed_flet, (mkcl_object) &MK_SI_parsed_flet);


mkcl_object mk_si_parsed_function(MKCL, mkcl_object proto_closure,
                                  mkcl_object named_function)
{
  mkcl_object pform = mk_cl_Cnil;

  mkcl_call_stack_check(env);
  mkcl_return_value(pform);
}

struct mkcl_cfun mk_si_parsed_function_cfunobj = MKCL_CFUN2(mk_si_parsed_function, (mkcl_object) &MK_SI_parsed_function);


mkcl_object mk_si_parsed_go(MKCL, mkcl_object proto_closure,
                            mkcl_object tag)
{
  mkcl_object pform = mk_cl_Cnil;

  mkcl_call_stack_check(env);
  mkcl_return_value(pform);
}

struct mkcl_cfun mk_si_parsed_go_cfunobj = MKCL_CFUN2(mk_si_parsed_go, (mkcl_object) &MK_SI_parsed_go);


mkcl_object mk_si_parsed_if(MKCL, mkcl_object proto_closure,
                            mkcl_object test_form, mkcl_object then_form, mkcl_object else_form)
{
  mkcl_object pform = mk_cl_Cnil;

  mkcl_call_stack_check(env);
  mkcl_return_value(pform);
}

struct mkcl_cfun mk_si_parsed_if_cfunobj = MKCL_CFUN4(mk_si_parsed_if, (mkcl_object) &MK_SI_parsed_if);


mkcl_object mk_si_parsed_labels(MKCL, mkcl_object proto_closure,
                                mkcl_object local_funs, mkcl_object body)
{
  mkcl_object pform = mk_cl_Cnil;

  mkcl_call_stack_check(env);
  mkcl_return_value(pform);
}

struct mkcl_cfun mk_si_parsed_labels_cfunobj = MKCL_CFUN3(mk_si_parsed_labels, (mkcl_object) &MK_SI_parsed_labels);


mkcl_object mk_si_parsed_let(MKCL, mkcl_object proto_closure,
                             mkcl_object local_vars, mkcl_object body)
{
  mkcl_object pform = mk_cl_Cnil;

  mkcl_call_stack_check(env);
  mkcl_return_value(pform);
}

struct mkcl_cfun mk_si_parsed_let_cfunobj = MKCL_CFUN3(mk_si_parsed_let, (mkcl_object) &MK_SI_parsed_let);


mkcl_object mk_si_parsed_letX(MKCL, mkcl_object proto_closure,
                              mkcl_object local_vars, mkcl_object body)
{
  mkcl_object pform = mk_cl_Cnil;

  mkcl_call_stack_check(env);
  mkcl_return_value(pform);
}

struct mkcl_cfun mk_si_parsed_letX_cfunobj = MKCL_CFUN3(mk_si_parsed_letX, (mkcl_object) &MK_SI_parsed_letX);


mkcl_object mk_si_parsed_load_time_value(MKCL, mkcl_object proto_closure,
                                         mkcl_object form, mkcl_object read_only_p)
{
  mkcl_object body = mk_cl_Cnil;
  mkcl_object pform = mk_cl_Cnil;

  mkcl_call_stack_check(env);
  mkcl_return_value(pform);
}

struct mkcl_cfun mk_si_parsed_load_time_value_cfunobj = MKCL_CFUN3(mk_si_parsed_load_time_value, (mkcl_object) &MK_SI_parsed_load_time_value);


mkcl_object mk_si_parsed_locally(MKCL, mkcl_object proto_closure, mkcl_object body)
{
  mkcl_object pform = mk_cl_Cnil;

  mkcl_call_stack_check(env);
  mkcl_return_value(pform);
}

struct mkcl_cfun mk_si_parsed_locally_cfunobj = MKCL_CFUN2(mk_si_parsed_locally, (mkcl_object) &MK_SI_parsed_locally);


mkcl_object mk_si_parsed_macrolet(MKCL, mkcl_object proto_closure,
                                  mkcl_object local_macros, mkcl_object body)
{ /* does this function even need to exists? Isn't it entirely replaced by a 'parsed-progn' at parse time? */
  mkcl_object pform = mk_cl_Cnil;

  mkcl_call_stack_check(env);
  mkcl_return_value(pform);
}

struct mkcl_cfun mk_si_parsed_macrolet_cfunobj = MKCL_CFUN3(mk_si_parsed_macrolet, (mkcl_object) &MK_SI_parsed_macrolet);


mkcl_object mk_si_parsed_multiple_value_call(MKCL, mkcl_object proto_closure,
                                             mkcl_object function, mkcl_object body)
{
  mkcl_object pform = mk_cl_Cnil;

  mkcl_call_stack_check(env);
  mkcl_return_value(pform);
}

struct mkcl_cfun mk_si_parsed_multiple_value_call_cfunobj = MKCL_CFUN3(mk_si_parsed_multiple_value_call, (mkcl_object) &MK_SI_parsed_multiple_value_call);


mkcl_object mk_si_parsed_multiple_value_prog1(MKCL, mkcl_object proto_closure,
                                              mkcl_object first_form, mkcl_object body)
{
  mkcl_object pform = mk_cl_Cnil;

  mkcl_call_stack_check(env);
  mkcl_return_value(pform);
}

struct mkcl_cfun mk_si_parsed_multiple_value_prog1_cfunobj = MKCL_CFUN3(mk_si_parsed_multiple_value_prog1, (mkcl_object) &MK_SI_parsed_multiple_value_prog1);


mkcl_object mk_si_parsed_progn(MKCL, mkcl_object proto_closure,
                               mkcl_object body)
{
  mkcl_object pform = mk_cl_Cnil;

  mkcl_call_stack_check(env);
  mkcl_return_value(pform);
}

struct mkcl_cfun mk_si_parsed_progn_cfunobj = MKCL_CFUN2(mk_si_parsed_progn, (mkcl_object) &MK_SI_parsed_progn);


mkcl_object mk_si_parsed_progv(MKCL, mkcl_object proto_closure,
                               mkcl_object symbols_form, mkcl_object values_form, mkcl_object body)
{
  mkcl_object pform = mk_cl_Cnil;

  mkcl_call_stack_check(env);
  mkcl_return_value(pform);
}

struct mkcl_cfun mk_si_parsed_progv_cfunobj = MKCL_CFUN4(mk_si_parsed_progv, (mkcl_object) &MK_SI_parsed_progv);


mkcl_object mk_si_parsed_quote(MKCL, mkcl_object proto_closure,
                              mkcl_object object)
{
  mkcl_object body = mk_cl_Cnil;
  mkcl_object pform = mk_cl_Cnil;

  mkcl_call_stack_check(env);
  mkcl_return_value(pform);
}

struct mkcl_cfun mk_si_parsed_quote_cfunobj = MKCL_CFUN2(mk_si_parsed_quote, (mkcl_object) &MK_SI_parsed_quote);


mkcl_object mk_si_parsed_return_from(MKCL, mkcl_object proto_closure,
                                     mkcl_object name, mkcl_object result)
{
  mkcl_object body = mk_cl_Cnil;
  mkcl_object pform = mk_cl_Cnil;

  mkcl_call_stack_check(env);
  mkcl_return_value(pform);
}

struct mkcl_cfun mk_si_parsed_return_from_cfunobj = MKCL_CFUN3(mk_si_parsed_return_from, (mkcl_object) &MK_SI_parsed_return_from);


mkcl_object mk_si_parsed_setq(MKCL, mkcl_object proto_closure,
                              mkcl_object pairs)
{
  mkcl_object body = mk_cl_Cnil;
  mkcl_object pform = mk_cl_Cnil;

  mkcl_call_stack_check(env);
  mkcl_return_value(pform);
}

struct mkcl_cfun mk_si_parsed_setq_cfunobj = MKCL_CFUN2(mk_si_parsed_setq, (mkcl_object) &MK_SI_parsed_setq);


mkcl_object mk_si_parsed_symbol_macrolet(MKCL, mkcl_object proto_closure,
                                         mkcl_object symbol_macros, mkcl_object body)
{ /* does this function even need to exists? Isn't it entirely replaced by a 'parsed-progn' at parse time? */
  mkcl_object pform = mk_cl_Cnil;

  mkcl_call_stack_check(env);
  mkcl_return_value(pform);
}

struct mkcl_cfun mk_si_parsed_symbol_macrolet_cfunobj = MKCL_CFUN3(mk_si_parsed_symbol_macrolet, (mkcl_object) &MK_SI_parsed_symbol_macrolet);


mkcl_object mk_si_parsed_tagbody(MKCL, mkcl_object proto_closure, mkcl_object body)
{
  mkcl_object pform = mk_cl_Cnil;

  mkcl_call_stack_check(env);
  mkcl_return_value(pform);
}

struct mkcl_cfun mk_si_parsed_tagbody_cfunobj = MKCL_CFUN2(mk_si_parsed_tagbody, (mkcl_object) &MK_SI_parsed_tagbody);


mkcl_object mk_si_parsed_the(MKCL, mkcl_object proto_closure,
                             mkcl_object value_type, mkcl_object form)
{
  mkcl_object pform = mk_cl_Cnil;

  mkcl_call_stack_check(env);
  mkcl_return_value(pform);
}

struct mkcl_cfun mk_si_parsed_the_cfunobj = MKCL_CFUN3(mk_si_parsed_the, (mkcl_object) &MK_SI_parsed_the);


mkcl_object mk_si_parsed_throw(MKCL, mkcl_object proto_closure,
                               mkcl_object target_tag, mkcl_object result_form)
{
  mkcl_object pform = mk_cl_Cnil;

  mkcl_call_stack_check(env);
  mkcl_return_value(pform);
}

struct mkcl_cfun mk_si_parsed_throw_cfunobj = MKCL_CFUN3(mk_si_parsed_throw, (mkcl_object) &MK_SI_parsed_throw);


mkcl_object mk_si_parsed_unwind_protect(MKCL, mkcl_object proto_closure,
                                        mkcl_object protected_form, mkcl_object cleanup_body)
{
  mkcl_object pform = mk_cl_Cnil;

  mkcl_call_stack_check(env);
  mkcl_return_value(pform);
}

struct mkcl_cfun mk_si_parsed_unwind_protect_cfunobj = MKCL_CFUN3(mk_si_parsed_unwind_protect, (mkcl_object) &MK_SI_parsed_unwind_protect);


