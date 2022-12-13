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


mkcl_object mkcl_evaluate_block(MKCL, mkcl_narg narg,
                                mkcl_object spath, mkcl_object proto_closure, mkcl_object lex_env,
                                mkcl_object name, ...)
{
  mkcl_return_value(mk_cl_Cnil);
}

struct mkcl_cfun mkcl_evaluate_block_cfunobj = MKCL_CFUN_VA(mkcl_evaluate_block, (mkcl_object) &MK_CL_block);

mkcl_object mkcl_evaluate_catch(MKCL, mkcl_narg narg,
                                mkcl_object spath, mkcl_object proto_closure, mkcl_object lex_env,
                                mkcl_object name, ...)
{
  mkcl_return_value(mk_cl_Cnil);
}

struct mkcl_cfun mkcl_evaluate_catch_cfunobj = MKCL_CFUN_VA(mkcl_evaluate_catch, (mkcl_object) &MK_CL_catch);

mkcl_object mkcl_evaluate_eval_when(MKCL, mkcl_narg narg,
                                    mkcl_object spath, mkcl_object proto_closure, mkcl_object lex_env,
                                    mkcl_object name, ...)
{
  mkcl_return_value(mk_cl_Cnil);
}

struct mkcl_cfun mkcl_evaluate_eval_when_cfunobj = MKCL_CFUN_VA(mkcl_evaluate_eval_when, (mkcl_object) &MK_CL_eval_when);

mkcl_object mkcl_evaluate_flet(MKCL, mkcl_narg narg,
                               mkcl_object spath, mkcl_object proto_closure, mkcl_object lex_env,
                               mkcl_object name, ...)
{
  mkcl_return_value(mk_cl_Cnil);
}

struct mkcl_cfun mkcl_evaluate_flet_cfunobj = MKCL_CFUN_VA(mkcl_evaluate_flet, (mkcl_object) &MK_CL_flet);

mkcl_object mkcl_evaluate_function(MKCL, mkcl_narg narg,
                                   mkcl_object spath, mkcl_object proto_closure, mkcl_object lex_env,
                                   mkcl_object name, ...)
{
  mkcl_return_value(mk_cl_Cnil);
}

struct mkcl_cfun mkcl_evaluate_function_cfunobj = MKCL_CFUN_VA(mkcl_evaluate_function, (mkcl_object) &MK_CL_function);

mkcl_object mkcl_evaluate_go(MKCL, mkcl_narg narg,
                             mkcl_object spath, mkcl_object proto_closure, mkcl_object lex_env,
                             mkcl_object name, ...)
{
  mkcl_return_value(mk_cl_Cnil);
}

struct mkcl_cfun mkcl_evaluate_go_cfunobj = MKCL_CFUN_VA(mkcl_evaluate_go, (mkcl_object) &MK_CL_go);

mkcl_object mkcl_evaluate_if(MKCL, mkcl_narg narg,
                             mkcl_object spath, mkcl_object proto_closure, mkcl_object lex_env,
                             mkcl_object name, ...)
{
  mkcl_return_value(mk_cl_Cnil);
}

struct mkcl_cfun mkcl_evaluate_if_cfunobj = MKCL_CFUN_VA(mkcl_evaluate_if, (mkcl_object) &MK_CL_if);

mkcl_object mkcl_evaluate_labels(MKCL, mkcl_narg narg,
                                 mkcl_object spath, mkcl_object proto_closure, mkcl_object lex_env,
                                 mkcl_object name, ...)
{
  mkcl_return_value(mk_cl_Cnil);
}

struct mkcl_cfun mkcl_evaluate_labels_cfunobj = MKCL_CFUN_VA(mkcl_evaluate_labels, (mkcl_object) &MK_CL_labels);

mkcl_object mkcl_evaluate_let(MKCL, mkcl_narg narg,
                              mkcl_object spath, mkcl_object proto_closure, mkcl_object lex_env,
                              mkcl_object name, ...)
{
  mkcl_return_value(mk_cl_Cnil);
}

struct mkcl_cfun mkcl_evaluate_let_cfunobj = MKCL_CFUN_VA(mkcl_evaluate_let, (mkcl_object) &MK_CL_let);

mkcl_object mkcl_evaluate_letX(MKCL, mkcl_narg narg,
                               mkcl_object spath, mkcl_object proto_closure, mkcl_object lex_env,
                               mkcl_object name, ...)
{
  mkcl_return_value(mk_cl_Cnil);
}

struct mkcl_cfun mkcl_evaluate_letX_cfunobj = MKCL_CFUN_VA(mkcl_evaluate_letX, (mkcl_object) &MK_CL_letX);

mkcl_object mkcl_evaluate_load_time_value(MKCL, mkcl_narg narg,
                                          mkcl_object spath, mkcl_object proto_closure, mkcl_object lex_env,
                                          mkcl_object name, ...)
{
  mkcl_return_value(mk_cl_Cnil);
}

struct mkcl_cfun mkcl_evaluate_load_time_value_cfunobj = MKCL_CFUN_VA(mkcl_evaluate_load_time_value, (mkcl_object) &MK_CL_load_time_value);

mkcl_object mkcl_evaluate_locally(MKCL, mkcl_narg narg,
                                  mkcl_object spath, mkcl_object proto_closure, mkcl_object lex_env,
                                  mkcl_object name, ...)
{
  mkcl_return_value(mk_cl_Cnil);
}

struct mkcl_cfun mkcl_evaluate_locally_cfunobj = MKCL_CFUN_VA(mkcl_evaluate_locally, (mkcl_object) &MK_CL_locally);

mkcl_object mkcl_evaluate_macrolet(MKCL, mkcl_narg narg,
                                   mkcl_object spath, mkcl_object proto_closure, mkcl_object lex_env,
                                   mkcl_object name, ...)
{
  mkcl_return_value(mk_cl_Cnil);
}

struct mkcl_cfun mkcl_evaluate_macrolet_cfunobj = MKCL_CFUN_VA(mkcl_evaluate_macrolet, (mkcl_object) &MK_CL_macrolet);

mkcl_object mkcl_evaluate_multiple_value_call(MKCL, mkcl_narg narg,
                                              mkcl_object spath, mkcl_object proto_closure, mkcl_object lex_env,
                                              mkcl_object name, ...)
{
  mkcl_return_value(mk_cl_Cnil);
}

struct mkcl_cfun mkcl_evaluate_multiple_value_call_cfunobj = MKCL_CFUN_VA(mkcl_evaluate_multiple_value_call, (mkcl_object) &MK_CL_multiple_value_call);

mkcl_object mkcl_evaluate_multiple_value_prog1(MKCL, mkcl_narg narg,
                                mkcl_object spath, mkcl_object proto_closure, mkcl_object lex_env,
                                mkcl_object name, ...)
{
  mkcl_return_value(mk_cl_Cnil);
}

struct mkcl_cfun mkcl_evaluate_multiple_value_prog1_cfunobj = MKCL_CFUN_VA(mkcl_evaluate_multiple_value_prog1, (mkcl_object) &MK_CL_multiple_value_prog1);

mkcl_object mkcl_evaluate_progn(MKCL, mkcl_narg narg,
                                mkcl_object spath, mkcl_object proto_closure, mkcl_object lex_env,
                                mkcl_object name, ...)
{
  mkcl_return_value(mk_cl_Cnil);
}

struct mkcl_cfun mkcl_evaluate_progn_cfunobj = MKCL_CFUN_VA(mkcl_evaluate_progn, (mkcl_object) &MK_CL_progn);

mkcl_object mkcl_evaluate_progv(MKCL, mkcl_narg narg,
                                mkcl_object spath, mkcl_object proto_closure, mkcl_object lex_env,
                                mkcl_object name, ...)
{
  mkcl_return_value(mk_cl_Cnil);
}

struct mkcl_cfun mkcl_evaluate_progv_cfunobj = MKCL_CFUN_VA(mkcl_evaluate_progv, (mkcl_object) &MK_CL_progv);

mkcl_object mkcl_evaluate_quote(MKCL, mkcl_narg narg,
                                mkcl_object spath, mkcl_object proto_closure, mkcl_object lex_env,
                                mkcl_object name, ...)
{
  mkcl_return_value(mk_cl_Cnil);
}

struct mkcl_cfun mkcl_evaluate_quote_cfunobj = MKCL_CFUN_VA(mkcl_evaluate_quote, (mkcl_object) &MK_CL_quote);

mkcl_object mkcl_evaluate_return_from(MKCL, mkcl_narg narg,
                                      mkcl_object spath, mkcl_object proto_closure, mkcl_object lex_env,
                                      mkcl_object name, ...)
{
  mkcl_return_value(mk_cl_Cnil);
}

struct mkcl_cfun mkcl_evaluate_return_from_cfunobj = MKCL_CFUN_VA(mkcl_evaluate_return_from, (mkcl_object) &MK_CL_return_from);

mkcl_object mkcl_evaluate_setq(MKCL, mkcl_narg narg,
                               mkcl_object spath, mkcl_object proto_closure, mkcl_object lex_env,
                               mkcl_object name, ...)
{
  mkcl_return_value(mk_cl_Cnil);
}

struct mkcl_cfun mkcl_evaluate_setq_cfunobj = MKCL_CFUN_VA(mkcl_evaluate_setq, (mkcl_object) &MK_CL_setq);

mkcl_object mkcl_evaluate_symbol_macrolet(MKCL, mkcl_narg narg,
                                          mkcl_object spath, mkcl_object proto_closure, mkcl_object lex_env,
                                          mkcl_object name, ...)
{
  mkcl_return_value(mk_cl_Cnil);
}

struct mkcl_cfun mkcl_evaluate_symbol_macrolet_cfunobj = MKCL_CFUN_VA(mkcl_evaluate_symbol_macrolet, (mkcl_object) &MK_CL_symbol_macrolet);

mkcl_object mkcl_evaluate_tagbody(MKCL, mkcl_narg narg,
                                  mkcl_object spath, mkcl_object proto_closure, mkcl_object lex_env,
                                  mkcl_object name, ...)
{
  mkcl_return_value(mk_cl_Cnil);
}

struct mkcl_cfun mkcl_evaluate_tagbody_cfunobj = MKCL_CFUN_VA(mkcl_evaluate_tagbody, (mkcl_object) &MK_CL_tagbody);

mkcl_object mkcl_evaluate_the(MKCL, mkcl_narg narg,
                              mkcl_object spath, mkcl_object proto_closure, mkcl_object lex_env,
                              mkcl_object name, ...)
{
  mkcl_return_value(mk_cl_Cnil);
}

struct mkcl_cfun mkcl_evaluate_the_cfunobj = MKCL_CFUN_VA(mkcl_evaluate_the, (mkcl_object) &MK_CL_the);

mkcl_object mkcl_evaluate_throw(MKCL, mkcl_narg narg,
                                mkcl_object spath, mkcl_object proto_closure, mkcl_object lex_env,
                                mkcl_object name, ...)
{
  mkcl_return_value(mk_cl_Cnil);
}

struct mkcl_cfun mkcl_evaluate_throw_cfunobj = MKCL_CFUN_VA(mkcl_evaluate_throw, (mkcl_object) &MK_CL_throw);

mkcl_object mkcl_evaluate_unwind_protect(MKCL, mkcl_narg narg,
                                         mkcl_object spath, mkcl_object proto_closure, mkcl_object lex_env,
                                         mkcl_object name, ...)
{
  mkcl_return_value(mk_cl_Cnil);
}

struct mkcl_cfun mkcl_evaluate_unwind_protect_cfunobj = MKCL_CFUN_VA(mkcl_evaluate_unwind_protect, (mkcl_object) &MK_CL_unwind_protect);


struct mkcl_special_operator mkcl_special_op_block =
  {
   mkcl_t_special_operator, 0, 0, 0,
   (mkcl_object) &MK_CL_block,
   (mkcl_object) &mkcl_evaluate_block_cfunobj,
   (mkcl_object) &mk_si_parse_block_cfunobj,
  };

struct mkcl_special_operator mkcl_special_op_catch = {
   mkcl_t_special_operator, 0, 0, 0,
   (mkcl_object) &MK_CL_catch,
   (mkcl_object) &mkcl_evaluate_catch_cfunobj,
   (mkcl_object) &mk_si_parse_catch_cfunobj,
};

struct mkcl_special_operator mkcl_special_op_eval_when = {
   mkcl_t_special_operator, 0, 0, 0,
   (mkcl_object) &MK_CL_eval_when,
   (mkcl_object) &mkcl_evaluate_eval_when_cfunobj,
   (mkcl_object) &mk_si_parse_eval_when_cfunobj,
};

struct mkcl_special_operator mkcl_special_op_flet = {
   mkcl_t_special_operator, 0, 0, 0,
   (mkcl_object) &MK_CL_flet,
   (mkcl_object) &mkcl_evaluate_flet_cfunobj,
   (mkcl_object) &mk_si_parse_flet_cfunobj,
};

struct mkcl_special_operator mkcl_special_op_function = {
   mkcl_t_special_operator, 0, 0, 0,
   (mkcl_object) &MK_CL_function,
   (mkcl_object) &mkcl_evaluate_function_cfunobj,
   (mkcl_object) &mk_si_parse_function_cfunobj,
};

struct mkcl_special_operator mkcl_special_op_go = {
   mkcl_t_special_operator, 0, 0, 0,
   (mkcl_object) &MK_CL_go,
   (mkcl_object) &mkcl_evaluate_go_cfunobj,
   (mkcl_object) &mk_si_parse_go_cfunobj,
};

struct mkcl_special_operator mkcl_special_op_if = {
   mkcl_t_special_operator, 0, 0, 0,
   (mkcl_object) &MK_CL_if,
   (mkcl_object) &mkcl_evaluate_if_cfunobj,
   (mkcl_object) &mk_si_parse_if_cfunobj,
};

struct mkcl_special_operator mkcl_special_op_labels = {
   mkcl_t_special_operator, 0, 0, 0,
   (mkcl_object) &MK_CL_labels,
   (mkcl_object) &mkcl_evaluate_labels_cfunobj,
   (mkcl_object) &mk_si_parse_labels_cfunobj,
};

struct mkcl_special_operator mkcl_special_op_let = {
   mkcl_t_special_operator, 0, 0, 0,
   (mkcl_object) &MK_CL_let,
   (mkcl_object) &mkcl_evaluate_let_cfunobj,
   (mkcl_object) &mk_si_parse_let_cfunobj,
};

struct mkcl_special_operator mkcl_special_op_letX = {
   mkcl_t_special_operator, 0, 0, 0,
   (mkcl_object) &MK_CL_letX,
   (mkcl_object) &mkcl_evaluate_letX_cfunobj,
   (mkcl_object) &mk_si_parse_letX_cfunobj,
};

struct mkcl_special_operator mkcl_special_op_load_time_value = {
   mkcl_t_special_operator, 0, 0, 0,
   (mkcl_object) &MK_CL_load_time_value,
   (mkcl_object) &mkcl_evaluate_load_time_value_cfunobj,
   (mkcl_object) &mk_si_parse_load_time_value_cfunobj,
};

struct mkcl_special_operator mkcl_special_op_locally = {
   mkcl_t_special_operator, 0, 0, 0,
   (mkcl_object) &MK_CL_locally,
   (mkcl_object) &mkcl_evaluate_locally_cfunobj,
   (mkcl_object) &mk_si_parse_locally_cfunobj,
};

struct mkcl_special_operator mkcl_special_op_macrolet = {
   mkcl_t_special_operator, 0, 0, 0,
   (mkcl_object) &MK_CL_macrolet,
   (mkcl_object) &mkcl_evaluate_macrolet_cfunobj,
   (mkcl_object) &mk_si_parse_macrolet_cfunobj,
};

struct mkcl_special_operator mkcl_special_op_multiple_value_call = {
   mkcl_t_special_operator, 0, 0, 0,
   (mkcl_object) &MK_CL_multiple_value_call,
   (mkcl_object) &mkcl_evaluate_multiple_value_call_cfunobj,
   (mkcl_object) &mk_si_parse_multiple_value_call_cfunobj,
};

struct mkcl_special_operator mkcl_special_op_multiple_value_prog1 = {
   mkcl_t_special_operator, 0, 0, 0,
   (mkcl_object) &MK_CL_multiple_value_prog1,
   (mkcl_object) &mkcl_evaluate_multiple_value_prog1_cfunobj,
   (mkcl_object) &mk_si_parse_multiple_value_prog1_cfunobj,
};

struct mkcl_special_operator mkcl_special_op_progn =
  {
   mkcl_t_special_operator, 0, 0, 0,
   (mkcl_object) &MK_CL_progn,
   (mkcl_object) &mkcl_evaluate_progn_cfunobj,
   (mkcl_object) &mk_si_parse_progn_cfunobj,
  };

struct mkcl_special_operator mkcl_special_op_progv = {
   mkcl_t_special_operator, 0, 0, 0,
   (mkcl_object) &MK_CL_progv,
   (mkcl_object) &mkcl_evaluate_progv_cfunobj,
   (mkcl_object) &mk_si_parse_progv_cfunobj,
};

struct mkcl_special_operator mkcl_special_op_quote = {
   mkcl_t_special_operator, 0, 0, 0,
   (mkcl_object) &MK_CL_quote,
   (mkcl_object) &mkcl_evaluate_quote_cfunobj,
   (mkcl_object) &mk_si_parse_quote_cfunobj,
};

struct mkcl_special_operator mkcl_special_op_return_from = {
   mkcl_t_special_operator, 0, 0, 0,
   (mkcl_object) &MK_CL_return_from,
   (mkcl_object) &mkcl_evaluate_return_from_cfunobj,
   (mkcl_object) &mk_si_parse_return_from_cfunobj,
};

struct mkcl_special_operator mkcl_special_op_setq = {
   mkcl_t_special_operator, 0, 0, 0,
   (mkcl_object) &MK_CL_setq,
   (mkcl_object) &mkcl_evaluate_setq_cfunobj,
   (mkcl_object) &mk_si_parse_setq_cfunobj,
};

struct mkcl_special_operator mkcl_special_op_symbol_macrolet = {
   mkcl_t_special_operator, 0, 0, 0,
   (mkcl_object) &MK_CL_symbol_macrolet,
   (mkcl_object) &mkcl_evaluate_symbol_macrolet_cfunobj,
   (mkcl_object) &mk_si_parse_symbol_macrolet_cfunobj,
};

struct mkcl_special_operator mkcl_special_op_tagbody = {
   mkcl_t_special_operator, 0, 0, 0,
   (mkcl_object) &MK_CL_tagbody,
   (mkcl_object) &mkcl_evaluate_tagbody_cfunobj,
   (mkcl_object) &mk_si_parse_tagbody_cfunobj,
};

struct mkcl_special_operator mkcl_special_op_the = {
   mkcl_t_special_operator, 0, 0, 0,
   (mkcl_object) &MK_CL_the,
   (mkcl_object) &mkcl_evaluate_the_cfunobj,
   (mkcl_object) &mk_si_parse_the_cfunobj,
};

struct mkcl_special_operator mkcl_special_op_throw = {
   mkcl_t_special_operator, 0, 0, 0,
   (mkcl_object) &MK_CL_throw,
   (mkcl_object) &mkcl_evaluate_throw_cfunobj,
   (mkcl_object) &mk_si_parse_throw_cfunobj,
};

struct mkcl_special_operator mkcl_special_op_unwind_protect = {
   mkcl_t_special_operator, 0, 0, 0,
   (mkcl_object) &MK_CL_unwind_protect,
   (mkcl_object) &mkcl_evaluate_unwind_protect_cfunobj,
   (mkcl_object) &mk_si_parse_unwind_protect_cfunobj,
};


