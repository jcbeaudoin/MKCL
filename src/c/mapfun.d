/* -*- mode: c -*- */
/*
    mapfun.c -- Mapping.
*/
/*
    Copyright (c) 1993, Giuseppe Attardi.
    Copyright (c) 2001, Juan Jose Garcia Ripoll.

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

@(defun mapcar (fun &rest lists)
	mkcl_object res, *val = &res;
@ 
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
@)

@(defun maplist (fun &rest lists)
	mkcl_object res, *val = &res;
@
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
@)

@(defun mapc (fun &rest lists)
	mkcl_object onelist;
@ 
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
@)

@(defun mapl (fun &rest lists)
	mkcl_object onelist;
@ 
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
@)

@(defun mapcan (fun &rest lists)
	mkcl_object res, *val = &res;
@ 
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
@)

@(defun mapcon (fun &rest lists)
	mkcl_object res, *val = &res;
@ 
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
@)

