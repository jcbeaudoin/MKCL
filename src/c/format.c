/* -*- mode: c -*- */
/*
    format.c -- Format.
*/
/*
    Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
    Copyright (c) 1990, Giuseppe Attardi.
    Copyright (c) 2001,2021-2022, Juan Jose Garcia Ripoll.

    MKCL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 3 of the License, or (at your option) any later version.

    See file '../../Copyright' for full details.
*/

#include <mkcl/mkcl.h>
#include <mkcl/internal.h>

struct mkcl_cfun mk_cl_format_cfunobj = MKCL_CFUN_VA(mk_cl_format, (mkcl_object) &MK_CL_format);

mkcl_object mk_cl_format(MKCL, mkcl_narg narg, mkcl_object strm, mkcl_object string, ...)
{
  mkcl_object output = mk_cl_Cnil;
  int null_strm = 0;

  mkcl_call_stack_check(env);
  {
    mkcl_setup_for_rest(env, (mkcl_object) &MK_CL_format, 2, narg, string, args);

    if (mkcl_Null(strm)) {
      strm = mkcl_alloc_adjustable_character_string(env, 64);
      null_strm = 1;
    } else if (strm == mk_cl_Ct) {
      strm = mkcl_symbol_value(env, (mkcl_object) &MK_CL_DYNVAR_standard_output);
    }
    if (mkcl_stringp(env, strm)) {
      output = strm;
      if (!output->base_string.hasfillp) {
        mk_cl_error(env, 7,
                    (mkcl_object) &MK_SI_format_error,
                    (mkcl_object) &MK_KEY_format_control,
                    mkcl_make_simple_base_string(env, "Cannot output to a non adjustable string."),
                    (mkcl_object) &MK_KEY_control_string,
                    string,
                    (mkcl_object) &MK_KEY_offset,
                    MKCL_MAKE_FIXNUM(0));
      }
      strm = mk_si_make_string_output_stream_from_string(env, strm, (mkcl_object) &MK_KEY_default);
      if (null_strm == 0)
        output = mk_cl_Cnil;
    }
    if (!mkcl_Null(mk_cl_functionp(env, string))) {
      mk_cl_apply(env, 3, string, strm, mkcl_grab_rest_args(env, args, TRUE));
    } else {
      mkcl_funcall3(env, MK_SI_formatter_aux.gfdef, strm, string, mkcl_grab_rest_args(env, args, FALSE));
    }
    mkcl_va_end(args);
    mkcl_return_value(output);
  }
}
