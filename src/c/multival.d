/* -*- mode: c -*- */
/*
    multival.c -- Multiple Values.
*/
/*
    Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
    Copyright (c) 1990, Giuseppe Attardi.

    ECoLisp is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 3 of the License, or (at your option) any later version.

    See file '../../Copyright' for full details.
*/


#include <mkcl/mkcl.h>

@(defun values (&rest args)
	int i;
@
  if (narg > MKCL_MULTIPLE_VALUES_LIMIT)
    mkcl_FEerror(env, "Too many values in VALUES",0);
  MKCL_NVALUES = narg;
  if (narg == 0)
    MKCL_VALUES(0) = mk_cl_Cnil;
  else for (i = 0; i < narg; i++)
	 MKCL_VALUES(i) = mkcl_va_arg(args);
  mkcl_va_end(args);
  mkcl_returnn(MKCL_VALUES(0));
@)

mkcl_object
mk_cl_values_list(MKCL, mkcl_object list)
{
  mkcl_index i = 0;

  mkcl_call_stack_check(env);
  MKCL_NVALUES=0;
  MKCL_VALUES(0) = mk_cl_Cnil;
  for (; !mkcl_Null(list); list=MKCL_CONS_CDR(list), i++)
    {
      if (!MKCL_LISTP(list)) mkcl_FEtype_error_list(env, list);
      
      if (i >= MKCL_MULTIPLE_VALUES_LIMIT)
	mkcl_FEerror(env, "Too many values in VALUES-LIST", 0);
      
      MKCL_NVALUES = i + 1;
      MKCL_VALUES(i) = MKCL_CONS_CAR(list);
    }
  mkcl_returnn(MKCL_VALUES(0));
}

