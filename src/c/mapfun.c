/* -*- mode: c -*- */
/*
    mapfun.c -- Mapping.
*/
/*
    Copyright (c) 1993, Giuseppe Attardi.
    Copyright (c) 2001, Juan Jose Garcia Ripoll.
    Copyright (c) 2021-2022, Jean-Claude Beaudoin.


    MKCL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 3 of the License, or (at your option) any later version.

    See file '../../Copyright' for full details.
*/


#include <mkcl/mkcl.h>
#include <mkcl/internal.h>
#include <string.h>

#define PREPARE_MAP(env, list, cdrs_frame, cars_frame, narg)		\
  struct mkcl_temp_stack_frame frames_aux[2];				\
  const mkcl_object cdrs_frame = (mkcl_object)frames_aux;		\
  const mkcl_object cars_frame = (mkcl_object)(frames_aux+1);		\
  MKCL_TEMP_STACK_FRAME_FROM_VA_LIST(env,cdrs_frame,list);              \
  MKCL_TEMP_STACK_FRAME_COPY(cars_frame, cdrs_frame);			\
  narg = cars_frame->frame.size;					\
  if (narg == 0) {							\
    mkcl_FEprogram_error(env, "MAP*: Too few arguments", 0);		\
  }

struct mkcl_cfun mk_cl_mapcar_cfunobj = MKCL_CFUN_VA(mk_cl_mapcar, (mkcl_object) &MK_CL_mapcar);

mkcl_object mk_cl_mapcar(MKCL, mkcl_narg narg, mkcl_object fun, ...)
{
  mkcl_object res, *val = &res;

  mkcl_call_stack_check(env);
  {
    mkcl_setup_for_rest(env, (mkcl_object) &MK_CL_mapcar, 1, narg, fun, lists);

    {
      PREPARE_MAP(env, lists, cdrs_frame, cars_frame, narg);
      res = mk_cl_Cnil;
      while (TRUE) {
        mkcl_index i;
        for (i = 0;  i < narg;  i++) {
          mkcl_object cdr = MKCL_TEMP_STACK_FRAME_REF(cdrs_frame, i);
          if (!MKCL_LISTP(cdr))
            mkcl_FEtype_error_list(env, cdr);
          if (mkcl_Null(cdr)) {
            mkcl_temp_stack_frame_close(env, cars_frame);
            mkcl_temp_stack_frame_close(env, cdrs_frame);
            mkcl_va_end(lists);
            mkcl_return_value(res);
          }
          MKCL_TEMP_STACK_FRAME_SET(cars_frame, i, MKCL_CONS_CAR(cdr));
          MKCL_TEMP_STACK_FRAME_SET(cdrs_frame, i, MKCL_CONS_CDR(cdr));
        }
        *val = mkcl_list1(env, mkcl_apply_from_temp_stack_frame(env, cars_frame, fun));
        val = &MKCL_CONS_CDR(*val);
      }
    }
  }
}

struct mkcl_cfun mk_cl_maplist_cfunobj = MKCL_CFUN_VA(mk_cl_maplist, (mkcl_object) &MK_CL_maplist);

mkcl_object mk_cl_maplist(MKCL, mkcl_narg narg, mkcl_object fun, ...)
{
  mkcl_object res, *val = &res;

  mkcl_call_stack_check(env);
  {
    mkcl_setup_for_rest(env, (mkcl_object) &MK_CL_maplist, 1, narg, fun, lists);

    {
      PREPARE_MAP(env, lists, cdrs_frame, cars_frame, narg);
      res = mk_cl_Cnil;
      while (TRUE) {
        mkcl_index i;
        for (i = 0;  i < narg;  i++) {
          mkcl_object cdr = MKCL_TEMP_STACK_FRAME_REF(cdrs_frame, i);
          if (!MKCL_LISTP(cdr))
            mkcl_FEtype_error_list(env, cdr);
          if (mkcl_Null(cdr)) {
            mkcl_temp_stack_frame_close(env, cars_frame);
            mkcl_temp_stack_frame_close(env, cdrs_frame);
            mkcl_va_end(lists);
            mkcl_return_value(res);
          }
          MKCL_TEMP_STACK_FRAME_SET(cars_frame, i, cdr);
          MKCL_TEMP_STACK_FRAME_SET(cdrs_frame, i, MKCL_CONS_CDR(cdr));
        }
        *val = mkcl_list1(env, mkcl_apply_from_temp_stack_frame(env, cars_frame, fun));
        val = &MKCL_CONS_CDR(*val);
      }
    }
  }
}

struct mkcl_cfun mk_cl_mapc_cfunobj = MKCL_CFUN_VA(mk_cl_mapc, (mkcl_object) &MK_CL_mapc);

mkcl_object mk_cl_mapc(MKCL, mkcl_narg narg, mkcl_object fun, ...)
{
  mkcl_object onelist;

  mkcl_call_stack_check(env);
  {
    mkcl_setup_for_rest(env, (mkcl_object) &MK_CL_mapc, 1, narg, fun, lists);

    {
      PREPARE_MAP(env, lists, cdrs_frame, cars_frame, narg);
      onelist = MKCL_TEMP_STACK_FRAME_REF(cdrs_frame, 0);
      while (TRUE) {
        mkcl_index i;
        for (i = 0;  i < narg;  i++) {
          mkcl_object cdr = MKCL_TEMP_STACK_FRAME_REF(cdrs_frame, i);
          if (!MKCL_LISTP(cdr))
            mkcl_FEtype_error_list(env, cdr);
          if (mkcl_Null(cdr)) {
            mkcl_temp_stack_frame_close(env, cars_frame);
            mkcl_temp_stack_frame_close(env, cdrs_frame);
            mkcl_va_end(lists);
            mkcl_return_value(onelist);
          }
          MKCL_TEMP_STACK_FRAME_SET(cars_frame, i, MKCL_CONS_CAR(cdr));
          MKCL_TEMP_STACK_FRAME_SET(cdrs_frame, i, MKCL_CONS_CDR(cdr));
        }
        mkcl_apply_from_temp_stack_frame(env, cars_frame, fun);
      }
    }
  }
}

struct mkcl_cfun mk_cl_mapl_cfunobj = MKCL_CFUN_VA(mk_cl_mapl, (mkcl_object) &MK_CL_mapl);

mkcl_object mk_cl_mapl(MKCL, mkcl_narg narg, mkcl_object fun, ...)
{
  mkcl_object onelist;

  mkcl_call_stack_check(env);
  {
    mkcl_setup_for_rest(env, (mkcl_object) &MK_CL_mapl, 1, narg, fun, lists);

    {
      PREPARE_MAP(env, lists, cdrs_frame, cars_frame, narg);
      onelist = MKCL_TEMP_STACK_FRAME_REF(cdrs_frame, 0);
      while (TRUE) {
        mkcl_index i;
        for (i = 0;  i < narg;  i++) {
          mkcl_object cdr = MKCL_TEMP_STACK_FRAME_REF(cdrs_frame, i);
          if (!MKCL_LISTP(cdr))
            mkcl_FEtype_error_list(env, cdr);
          if (mkcl_Null(cdr)) {
            mkcl_temp_stack_frame_close(env, cars_frame);
            mkcl_temp_stack_frame_close(env, cdrs_frame);
            mkcl_va_end(lists);
            mkcl_return_value(onelist);
          }
          MKCL_TEMP_STACK_FRAME_SET(cars_frame, i, cdr);
          MKCL_TEMP_STACK_FRAME_SET(cdrs_frame, i, MKCL_CONS_CDR(cdr));
        }
        mkcl_apply_from_temp_stack_frame(env, cars_frame, fun);
      }
    }
  }
}

struct mkcl_cfun mk_cl_mapcan_cfunobj = MKCL_CFUN_VA(mk_cl_mapcan, (mkcl_object) &MK_CL_mapcan);

mkcl_object mk_cl_mapcan(MKCL, mkcl_narg narg, mkcl_object fun, ...)
{
  mkcl_object res, *val = &res;

  mkcl_call_stack_check(env);
  {
    mkcl_setup_for_rest(env, (mkcl_object) &MK_CL_mapcan, 1, narg, fun, lists);

    {
      PREPARE_MAP(env, lists, cdrs_frame, cars_frame, narg);
      res = mk_cl_Cnil;
      while (TRUE) {
        mkcl_index i;
        for (i = 0;  i < narg;  i++) {
          mkcl_object cdr = MKCL_TEMP_STACK_FRAME_REF(cdrs_frame, i);
          if (!MKCL_LISTP(cdr))
            mkcl_FEtype_error_list(env, cdr);
          if (mkcl_Null(cdr)) {
            mkcl_temp_stack_frame_close(env, cars_frame);
            mkcl_temp_stack_frame_close(env, cdrs_frame);
            mkcl_va_end(lists);
            mkcl_return_value(res);
          }
          MKCL_TEMP_STACK_FRAME_SET(cars_frame, i, MKCL_CONS_CAR(cdr));
          MKCL_TEMP_STACK_FRAME_SET(cdrs_frame, i, MKCL_CONS_CDR(cdr));
        }
        *val = mkcl_apply_from_temp_stack_frame(env, cars_frame, fun);
        while (MKCL_CONSP(*val))
          val = &MKCL_CONS_CDR(*val);
      }
    }
  }
}

struct mkcl_cfun mk_cl_mapcon_cfunobj = MKCL_CFUN_VA(mk_cl_mapcon, (mkcl_object) &MK_CL_mapcon);

mkcl_object mk_cl_mapcon(MKCL, mkcl_narg narg, mkcl_object fun, ...)
{
  mkcl_object res, *val = &res;

  mkcl_call_stack_check(env);
  {
    mkcl_setup_for_rest(env, (mkcl_object) &MK_CL_mapcon, 1, narg, fun, lists);

    {
      PREPARE_MAP(env, lists, cdrs_frame, cars_frame, narg);
      res = mk_cl_Cnil;
      while (TRUE) {
        mkcl_index i;
        for (i = 0;  i < narg;  i++) {
          mkcl_object cdr = MKCL_TEMP_STACK_FRAME_REF(cdrs_frame, i);
          if (!MKCL_LISTP(cdr))
            mkcl_FEtype_error_list(env, cdr);
          if (mkcl_Null(cdr)) {
            mkcl_temp_stack_frame_close(env, cars_frame);
            mkcl_temp_stack_frame_close(env, cdrs_frame);
            mkcl_va_end(lists);
            mkcl_return_value(res);
          }
          MKCL_TEMP_STACK_FRAME_SET(cars_frame, i, cdr);
          MKCL_TEMP_STACK_FRAME_SET(cdrs_frame, i, MKCL_CONS_CDR(cdr));
        }
        *val = mkcl_apply_from_temp_stack_frame(env, cars_frame, fun);
        while (MKCL_CONSP(*val))
          val = &MKCL_CONS_CDR(*val);
      }
    }
  }
}

