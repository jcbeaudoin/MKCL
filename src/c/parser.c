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


mkcl_object mk_si_parse_block(MKCL, mkcl_narg narg,
                              mkcl_object spath, mkcl_object lex_env,
                              mkcl_object name, ...)
{
  mkcl_object body = mk_cl_Cnil;
  mkcl_object pform = mk_cl_Cnil;

  mkcl_call_stack_check(env);
  {
    mkcl_setup_for_rest(env, (mkcl_object) &MK_SI_parse_block, 3, narg, name, forms);

    if (narg--) {
      mkcl_object tail = body = mkcl_list1(env, mkcl_va_arg(forms));
      while (narg--) {
        mkcl_object cons = mkcl_list1(env, mkcl_va_arg(forms));
        MKCL_RPLACD(tail, cons);
        tail = cons;
      }
    }
    mkcl_va_end(forms);
  }
  mkcl_return_value(pform);
}

struct mkcl_cfun mk_si_parse_block_cfunobj = MKCL_CFUN_VA(mk_si_parse_block, (mkcl_object) &MK_SI_parse_block);


mkcl_object mk_si_parse_catch(MKCL, mkcl_narg narg,
                              mkcl_object spath, mkcl_object lex_env,
                              mkcl_object name, ...)
{
  mkcl_object body = mk_cl_Cnil;
  mkcl_object pform = mk_cl_Cnil;

  mkcl_call_stack_check(env);
  {
    mkcl_setup_for_rest(env, (mkcl_object) &MK_SI_parse_catch, 3, narg, name, forms);

    if (narg--) {
      mkcl_object tail = body = mkcl_list1(env, mkcl_va_arg(forms));
      while (narg--) {
        mkcl_object cons = mkcl_list1(env, mkcl_va_arg(forms));
        MKCL_RPLACD(tail, cons);
        tail = cons;
      }
    }
    mkcl_va_end(forms);
  }
  mkcl_return_value(pform);
}

struct mkcl_cfun mk_si_parse_catch_cfunobj = MKCL_CFUN_VA(mk_si_parse_catch, (mkcl_object) &MK_SI_parse_catch);


mkcl_object mk_si_parse_eval_when(MKCL, mkcl_narg narg,
                                  mkcl_object spath, mkcl_object lex_env,
                                  mkcl_object name, ...)
{
  mkcl_object body = mk_cl_Cnil;
  mkcl_object pform = mk_cl_Cnil;

  mkcl_call_stack_check(env);
  {
    mkcl_setup_for_rest(env, (mkcl_object) &MK_SI_parse_eval_when, 3, narg, name, forms);

    if (narg--) {
      mkcl_object tail = body = mkcl_list1(env, mkcl_va_arg(forms));
      while (narg--) {
        mkcl_object cons = mkcl_list1(env, mkcl_va_arg(forms));
        MKCL_RPLACD(tail, cons);
        tail = cons;
      }
    }
    mkcl_va_end(forms);
  }
  mkcl_return_value(pform);
}

struct mkcl_cfun mk_si_parse_eval_when_cfunobj = MKCL_CFUN_VA(mk_si_parse_eval_when, (mkcl_object) &MK_SI_parse_eval_when);


mkcl_object mk_si_parse_flet(MKCL, mkcl_narg narg,
                             mkcl_object spath, mkcl_object lex_env,
                             mkcl_object name, ...)
{
  mkcl_object body = mk_cl_Cnil;
  mkcl_object pform = mk_cl_Cnil;

  mkcl_call_stack_check(env);
  {
    mkcl_setup_for_rest(env, (mkcl_object) &MK_SI_parse_flet, 3, narg, name, forms);

    if (narg--) {
      mkcl_object tail = body = mkcl_list1(env, mkcl_va_arg(forms));
      while (narg--) {
        mkcl_object cons = mkcl_list1(env, mkcl_va_arg(forms));
        MKCL_RPLACD(tail, cons);
        tail = cons;
      }
    }
    mkcl_va_end(forms);
  }
  mkcl_return_value(pform);
}

struct mkcl_cfun mk_si_parse_flet_cfunobj = MKCL_CFUN_VA(mk_si_parse_flet, (mkcl_object) &MK_SI_parse_flet);


mkcl_object mk_si_parse_function(MKCL, mkcl_narg narg,
                                 mkcl_object spath, mkcl_object lex_env,
                                 mkcl_object name, ...)
{
  mkcl_object body = mk_cl_Cnil;
  mkcl_object pform = mk_cl_Cnil;

  mkcl_call_stack_check(env);
  {
    mkcl_setup_for_rest(env, (mkcl_object) &MK_SI_parse_function, 3, narg, name, forms);

    if (narg--) {
      mkcl_object tail = body = mkcl_list1(env, mkcl_va_arg(forms));
      while (narg--) {
        mkcl_object cons = mkcl_list1(env, mkcl_va_arg(forms));
        MKCL_RPLACD(tail, cons);
        tail = cons;
      }
    }
    mkcl_va_end(forms);
  }
  mkcl_return_value(pform);
}

struct mkcl_cfun mk_si_parse_function_cfunobj = MKCL_CFUN_VA(mk_si_parse_function, (mkcl_object) &MK_SI_parse_function);


mkcl_object mk_si_parse_go(MKCL, mkcl_narg narg,
                           mkcl_object spath, mkcl_object lex_env,
                           mkcl_object name, ...)
{
  mkcl_object body = mk_cl_Cnil;
  mkcl_object pform = mk_cl_Cnil;

  mkcl_call_stack_check(env);
  {
    mkcl_setup_for_rest(env, (mkcl_object) &MK_SI_parse_go, 3, narg, name, forms);

    if (narg--) {
      mkcl_object tail = body = mkcl_list1(env, mkcl_va_arg(forms));
      while (narg--) {
        mkcl_object cons = mkcl_list1(env, mkcl_va_arg(forms));
        MKCL_RPLACD(tail, cons);
        tail = cons;
      }
    }
    mkcl_va_end(forms);
  }
  mkcl_return_value(pform);
}

struct mkcl_cfun mk_si_parse_go_cfunobj = MKCL_CFUN_VA(mk_si_parse_go, (mkcl_object) &MK_SI_parse_go);


mkcl_object mk_si_parse_if(MKCL, mkcl_narg narg,
                           mkcl_object spath, mkcl_object lex_env,
                           mkcl_object name, ...)
{
  mkcl_object body = mk_cl_Cnil;
  mkcl_object pform = mk_cl_Cnil;

  mkcl_call_stack_check(env);
  {
    mkcl_setup_for_rest(env, (mkcl_object) &MK_SI_parse_if, 3, narg, name, forms);

    if (narg--) {
      mkcl_object tail = body = mkcl_list1(env, mkcl_va_arg(forms));
      while (narg--) {
        mkcl_object cons = mkcl_list1(env, mkcl_va_arg(forms));
        MKCL_RPLACD(tail, cons);
        tail = cons;
      }
    }
    mkcl_va_end(forms);
  }
  mkcl_return_value(pform);
}

struct mkcl_cfun mk_si_parse_if_cfunobj = MKCL_CFUN_VA(mk_si_parse_if, (mkcl_object) &MK_SI_parse_if);


mkcl_object mk_si_parse_labels(MKCL, mkcl_narg narg,
                               mkcl_object spath, mkcl_object lex_env,
                               mkcl_object name, ...)
{
  mkcl_object body = mk_cl_Cnil;
  mkcl_object pform = mk_cl_Cnil;

  mkcl_call_stack_check(env);
  {
    mkcl_setup_for_rest(env, (mkcl_object) &MK_SI_parse_labels, 3, narg, name, forms);

    if (narg--) {
      mkcl_object tail = body = mkcl_list1(env, mkcl_va_arg(forms));
      while (narg--) {
        mkcl_object cons = mkcl_list1(env, mkcl_va_arg(forms));
        MKCL_RPLACD(tail, cons);
        tail = cons;
      }
    }
    mkcl_va_end(forms);
  }
  mkcl_return_value(pform);
}

struct mkcl_cfun mk_si_parse_labels_cfunobj = MKCL_CFUN_VA(mk_si_parse_labels, (mkcl_object) &MK_SI_parse_labels);


mkcl_object mk_si_parse_let(MKCL, mkcl_narg narg,
                            mkcl_object spath, mkcl_object lex_env,
                            mkcl_object name, ...)
{
  mkcl_object body = mk_cl_Cnil;
  mkcl_object pform = mk_cl_Cnil;

  mkcl_call_stack_check(env);
  {
    mkcl_setup_for_rest(env, (mkcl_object) &MK_SI_parse_let, 3, narg, name, forms);

    if (narg--) {
      mkcl_object tail = body = mkcl_list1(env, mkcl_va_arg(forms));
      while (narg--) {
        mkcl_object cons = mkcl_list1(env, mkcl_va_arg(forms));
        MKCL_RPLACD(tail, cons);
        tail = cons;
      }
    }
    mkcl_va_end(forms);
  }
  mkcl_return_value(pform);
}

struct mkcl_cfun mk_si_parse_let_cfunobj = MKCL_CFUN_VA(mk_si_parse_let, (mkcl_object) &MK_SI_parse_let);


mkcl_object mk_si_parse_letX(MKCL, mkcl_narg narg,
                             mkcl_object spath, mkcl_object lex_env,
                             mkcl_object name, ...)
{
  mkcl_object body = mk_cl_Cnil;
  mkcl_object pform = mk_cl_Cnil;

  mkcl_call_stack_check(env);
  {
    mkcl_setup_for_rest(env, (mkcl_object) &MK_SI_parse_letX, 3, narg, name, forms);

    if (narg--) {
      mkcl_object tail = body = mkcl_list1(env, mkcl_va_arg(forms));
      while (narg--) {
        mkcl_object cons = mkcl_list1(env, mkcl_va_arg(forms));
        MKCL_RPLACD(tail, cons);
        tail = cons;
      }
    }
    mkcl_va_end(forms);
  }
  mkcl_return_value(pform);
}

struct mkcl_cfun mk_si_parse_letX_cfunobj = MKCL_CFUN_VA(mk_si_parse_letX, (mkcl_object) &MK_SI_parse_letX);


mkcl_object mk_si_parse_load_time_value(MKCL, mkcl_narg narg,
                                        mkcl_object spath, mkcl_object lex_env,
                                        mkcl_object name, ...)
{
  mkcl_object body = mk_cl_Cnil;
  mkcl_object pform = mk_cl_Cnil;

  mkcl_call_stack_check(env);
  {
    mkcl_setup_for_rest(env, (mkcl_object) &MK_SI_parse_load_time_value, 3, narg, name, forms);

    if (narg--) {
      mkcl_object tail = body = mkcl_list1(env, mkcl_va_arg(forms));
      while (narg--) {
        mkcl_object cons = mkcl_list1(env, mkcl_va_arg(forms));
        MKCL_RPLACD(tail, cons);
        tail = cons;
      }
    }
    mkcl_va_end(forms);
  }
  mkcl_return_value(pform);
}

struct mkcl_cfun mk_si_parse_load_time_value_cfunobj = MKCL_CFUN_VA(mk_si_parse_load_time_value, (mkcl_object) &MK_SI_parse_load_time_value);


mkcl_object mk_si_parse_locally(MKCL, mkcl_narg narg,
                                mkcl_object spath, mkcl_object lex_env,
                                mkcl_object name, ...)
{
  mkcl_object body = mk_cl_Cnil;
  mkcl_object pform = mk_cl_Cnil;

  mkcl_call_stack_check(env);
  {
    mkcl_setup_for_rest(env, (mkcl_object) &MK_SI_parse_locally, 3, narg, name, forms);

    if (narg--) {
      mkcl_object tail = body = mkcl_list1(env, mkcl_va_arg(forms));
      while (narg--) {
        mkcl_object cons = mkcl_list1(env, mkcl_va_arg(forms));
        MKCL_RPLACD(tail, cons);
        tail = cons;
      }
    }
    mkcl_va_end(forms);
  }
  mkcl_return_value(pform);
}

struct mkcl_cfun mk_si_parse_locally_cfunobj = MKCL_CFUN_VA(mk_si_parse_locally, (mkcl_object) &MK_SI_parse_locally);


mkcl_object mk_si_parse_macrolet(MKCL, mkcl_narg narg,
                                 mkcl_object spath, mkcl_object lex_env,
                                 mkcl_object name, ...)
{
  mkcl_object body = mk_cl_Cnil;
  mkcl_object pform = mk_cl_Cnil;

  mkcl_call_stack_check(env);
  {
    mkcl_setup_for_rest(env, (mkcl_object) &MK_SI_parse_macrolet, 3, narg, name, forms);

    if (narg--) {
      mkcl_object tail = body = mkcl_list1(env, mkcl_va_arg(forms));
      while (narg--) {
        mkcl_object cons = mkcl_list1(env, mkcl_va_arg(forms));
        MKCL_RPLACD(tail, cons);
        tail = cons;
      }
    }
    mkcl_va_end(forms);
  }
  mkcl_return_value(pform);
}

struct mkcl_cfun mk_si_parse_macrolet_cfunobj = MKCL_CFUN_VA(mk_si_parse_macrolet, (mkcl_object) &MK_SI_parse_macrolet);


mkcl_object mk_si_parse_multiple_value_call(MKCL, mkcl_narg narg,
                                            mkcl_object spath, mkcl_object lex_env,
                                            mkcl_object name, ...)
{
  mkcl_object body = mk_cl_Cnil;
  mkcl_object pform = mk_cl_Cnil;

  mkcl_call_stack_check(env);
  {
    mkcl_setup_for_rest(env, (mkcl_object) &MK_SI_parse_multiple_value_call, 3, narg, name, forms);

    if (narg--) {
      mkcl_object tail = body = mkcl_list1(env, mkcl_va_arg(forms));
      while (narg--) {
        mkcl_object cons = mkcl_list1(env, mkcl_va_arg(forms));
        MKCL_RPLACD(tail, cons);
        tail = cons;
      }
    }
    mkcl_va_end(forms);
  }
  mkcl_return_value(pform);
}

struct mkcl_cfun mk_si_parse_multiple_value_call_cfunobj = MKCL_CFUN_VA(mk_si_parse_multiple_value_call, (mkcl_object) &MK_SI_parse_multiple_value_call);


mkcl_object mk_si_parse_multiple_value_prog1(MKCL, mkcl_narg narg,
                                             mkcl_object spath, mkcl_object lex_env,
                                             mkcl_object name, ...)
{
  mkcl_object body = mk_cl_Cnil;
  mkcl_object pform = mk_cl_Cnil;

  mkcl_call_stack_check(env);
  {
    mkcl_setup_for_rest(env, (mkcl_object) &MK_SI_parse_multiple_value_prog1, 3, narg, name, forms);

    if (narg--) {
      mkcl_object tail = body = mkcl_list1(env, mkcl_va_arg(forms));
      while (narg--) {
        mkcl_object cons = mkcl_list1(env, mkcl_va_arg(forms));
        MKCL_RPLACD(tail, cons);
        tail = cons;
      }
    }
    mkcl_va_end(forms);
  }
  mkcl_return_value(pform);
}

struct mkcl_cfun mk_si_parse_multiple_value_prog1_cfunobj = MKCL_CFUN_VA(mk_si_parse_multiple_value_prog1, (mkcl_object) &MK_SI_parse_multiple_value_prog1);


mkcl_object mk_si_parse_progn(MKCL, mkcl_narg narg,
                              mkcl_object spath, mkcl_object lex_env,
                              mkcl_object name, ...)
{
  mkcl_object body = mk_cl_Cnil;
  mkcl_object pform = mk_cl_Cnil;

  mkcl_call_stack_check(env);
  {
    mkcl_setup_for_rest(env, (mkcl_object) &MK_SI_parse_progn, 3, narg, name, forms);

    if (narg--) {
      mkcl_object tail = body = mkcl_list1(env, mkcl_va_arg(forms));
      while (narg--) {
        mkcl_object cons = mkcl_list1(env, mkcl_va_arg(forms));
        MKCL_RPLACD(tail, cons);
        tail = cons;
      }
    }
    mkcl_va_end(forms);
  }
  mkcl_return_value(pform);
}

struct mkcl_cfun mk_si_parse_progn_cfunobj = MKCL_CFUN_VA(mk_si_parse_progn, (mkcl_object) &MK_SI_parse_progn);


mkcl_object mk_si_parse_progv(MKCL, mkcl_narg narg,
                              mkcl_object spath, mkcl_object lex_env,
                              mkcl_object name, ...)
{
  mkcl_object body = mk_cl_Cnil;
  mkcl_object pform = mk_cl_Cnil;

  mkcl_call_stack_check(env);
  {
    mkcl_setup_for_rest(env, (mkcl_object) &MK_SI_parse_progv, 3, narg, name, forms);

    if (narg--) {
      mkcl_object tail = body = mkcl_list1(env, mkcl_va_arg(forms));
      while (narg--) {
        mkcl_object cons = mkcl_list1(env, mkcl_va_arg(forms));
        MKCL_RPLACD(tail, cons);
        tail = cons;
      }
    }
    mkcl_va_end(forms);
  }
  mkcl_return_value(pform);
}

struct mkcl_cfun mk_si_parse_progv_cfunobj = MKCL_CFUN_VA(mk_si_parse_progv, (mkcl_object) &MK_SI_parse_progv);


mkcl_object mk_si_parse_quote(MKCL, mkcl_narg narg,
                              mkcl_object spath, mkcl_object lex_env,
                              mkcl_object name, ...)
{
  mkcl_object body = mk_cl_Cnil;
  mkcl_object pform = mk_cl_Cnil;

  mkcl_call_stack_check(env);
  {
    mkcl_setup_for_rest(env, (mkcl_object) &MK_SI_parse_quote, 3, narg, name, forms);

    if (narg--) {
      mkcl_object tail = body = mkcl_list1(env, mkcl_va_arg(forms));
      while (narg--) {
        mkcl_object cons = mkcl_list1(env, mkcl_va_arg(forms));
        MKCL_RPLACD(tail, cons);
        tail = cons;
      }
    }
    mkcl_va_end(forms);
  }
  mkcl_return_value(pform);
}

struct mkcl_cfun mk_si_parse_quote_cfunobj = MKCL_CFUN_VA(mk_si_parse_quote, (mkcl_object) &MK_SI_parse_quote);


mkcl_object mk_si_parse_return_from(MKCL, mkcl_narg narg,
                                    mkcl_object spath, mkcl_object lex_env,
                                    mkcl_object name, ...)
{
  mkcl_object body = mk_cl_Cnil;
  mkcl_object pform = mk_cl_Cnil;

  mkcl_call_stack_check(env);
  {
    mkcl_setup_for_rest(env, (mkcl_object) &MK_SI_parse_return_from, 3, narg, name, forms);

    if (narg--) {
      mkcl_object tail = body = mkcl_list1(env, mkcl_va_arg(forms));
      while (narg--) {
        mkcl_object cons = mkcl_list1(env, mkcl_va_arg(forms));
        MKCL_RPLACD(tail, cons);
        tail = cons;
      }
    }
    mkcl_va_end(forms);
  }
  mkcl_return_value(pform);
}

struct mkcl_cfun mk_si_parse_return_from_cfunobj = MKCL_CFUN_VA(mk_si_parse_return_from, (mkcl_object) &MK_SI_parse_return_from);


mkcl_object mk_si_parse_setq(MKCL, mkcl_narg narg,
                             mkcl_object spath, mkcl_object lex_env,
                             mkcl_object name, ...)
{
  mkcl_object body = mk_cl_Cnil;
  mkcl_object pform = mk_cl_Cnil;

  mkcl_call_stack_check(env);
  {
    mkcl_setup_for_rest(env, (mkcl_object) &MK_SI_parse_setq, 3, narg, name, forms);

    if (narg--) {
      mkcl_object tail = body = mkcl_list1(env, mkcl_va_arg(forms));
      while (narg--) {
        mkcl_object cons = mkcl_list1(env, mkcl_va_arg(forms));
        MKCL_RPLACD(tail, cons);
        tail = cons;
      }
    }
    mkcl_va_end(forms);
  }
  mkcl_return_value(pform);
}

struct mkcl_cfun mk_si_parse_setq_cfunobj = MKCL_CFUN_VA(mk_si_parse_setq, (mkcl_object) &MK_SI_parse_setq);


mkcl_object mk_si_parse_symbol_macrolet(MKCL, mkcl_narg narg,
                                        mkcl_object spath, mkcl_object lex_env,
                                        mkcl_object name, ...)
{
  mkcl_object body = mk_cl_Cnil;
  mkcl_object pform = mk_cl_Cnil;

  mkcl_call_stack_check(env);
  {
    mkcl_setup_for_rest(env, (mkcl_object) &MK_SI_parse_symbol_macrolet, 3, narg, name, forms);

    if (narg--) {
      mkcl_object tail = body = mkcl_list1(env, mkcl_va_arg(forms));
      while (narg--) {
        mkcl_object cons = mkcl_list1(env, mkcl_va_arg(forms));
        MKCL_RPLACD(tail, cons);
        tail = cons;
      }
    }
    mkcl_va_end(forms);
  }
  mkcl_return_value(pform);
}

struct mkcl_cfun mk_si_parse_symbol_macrolet_cfunobj = MKCL_CFUN_VA(mk_si_parse_symbol_macrolet, (mkcl_object) &MK_SI_parse_symbol_macrolet);


mkcl_object mk_si_parse_tagbody(MKCL, mkcl_narg narg,
                                mkcl_object spath, mkcl_object lex_env,
                                mkcl_object name, ...)
{
  mkcl_object body = mk_cl_Cnil;
  mkcl_object pform = mk_cl_Cnil;

  mkcl_call_stack_check(env);
  {
    mkcl_setup_for_rest(env, (mkcl_object) &MK_SI_parse_tagbody, 3, narg, name, forms);

    if (narg--) {
      mkcl_object tail = body = mkcl_list1(env, mkcl_va_arg(forms));
      while (narg--) {
        mkcl_object cons = mkcl_list1(env, mkcl_va_arg(forms));
        MKCL_RPLACD(tail, cons);
        tail = cons;
      }
    }
    mkcl_va_end(forms);
  }
  mkcl_return_value(pform);
}

struct mkcl_cfun mk_si_parse_tagbody_cfunobj = MKCL_CFUN_VA(mk_si_parse_tagbody, (mkcl_object) &MK_SI_parse_tagbody);


mkcl_object mk_si_parse_the(MKCL, mkcl_narg narg,
                            mkcl_object spath, mkcl_object lex_env,
                            mkcl_object name, ...)
{
  mkcl_object body = mk_cl_Cnil;
  mkcl_object pform = mk_cl_Cnil;

  mkcl_call_stack_check(env);
  {
    mkcl_setup_for_rest(env, (mkcl_object) &MK_SI_parse_the, 3, narg, name, forms);

    if (narg--) {
      mkcl_object tail = body = mkcl_list1(env, mkcl_va_arg(forms));
      while (narg--) {
        mkcl_object cons = mkcl_list1(env, mkcl_va_arg(forms));
        MKCL_RPLACD(tail, cons);
        tail = cons;
      }
    }
    mkcl_va_end(forms);
  }
  mkcl_return_value(pform);
}

struct mkcl_cfun mk_si_parse_the_cfunobj = MKCL_CFUN_VA(mk_si_parse_the, (mkcl_object) &MK_SI_parse_the);


mkcl_object mk_si_parse_throw(MKCL, mkcl_narg narg,
                              mkcl_object spath, mkcl_object lex_env,
                              mkcl_object name, ...)
{
  mkcl_object body = mk_cl_Cnil;
  mkcl_object pform = mk_cl_Cnil;

  mkcl_call_stack_check(env);
  {
    mkcl_setup_for_rest(env, (mkcl_object) &MK_SI_parse_throw, 3, narg, name, forms);

    if (narg--) {
      mkcl_object tail = body = mkcl_list1(env, mkcl_va_arg(forms));
      while (narg--) {
        mkcl_object cons = mkcl_list1(env, mkcl_va_arg(forms));
        MKCL_RPLACD(tail, cons);
        tail = cons;
      }
    }
    mkcl_va_end(forms);
  }
  mkcl_return_value(pform);
}

struct mkcl_cfun mk_si_parse_throw_cfunobj = MKCL_CFUN_VA(mk_si_parse_throw, (mkcl_object) &MK_SI_parse_throw);


mkcl_object mk_si_parse_unwind_protect(MKCL, mkcl_narg narg,
                                       mkcl_object spath, mkcl_object lex_env,
                                       mkcl_object name, ...)
{
  mkcl_object body = mk_cl_Cnil;
  mkcl_object pform = mk_cl_Cnil;

  mkcl_call_stack_check(env);
  {
    mkcl_setup_for_rest(env, (mkcl_object) &MK_SI_parse_unwind_protect, 3, narg, name, forms);

    if (narg--) {
      mkcl_object tail = body = mkcl_list1(env, mkcl_va_arg(forms));
      while (narg--) {
        mkcl_object cons = mkcl_list1(env, mkcl_va_arg(forms));
        MKCL_RPLACD(tail, cons);
        tail = cons;
      }
    }
    mkcl_va_end(forms);
  }
  mkcl_return_value(pform);
}

struct mkcl_cfun mk_si_parse_unwind_protect_cfunobj = MKCL_CFUN_VA(mk_si_parse_unwind_protect, (mkcl_object) &MK_SI_parse_unwind_protect);


