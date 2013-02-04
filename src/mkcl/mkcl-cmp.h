/* -*- mode: c -*- */
/*
    mkcl-cmp.h  -- Include file for compiled code.
*/
/*
    Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
    Copyright (c) 1990, Giuseppe Attardi.
    Copyright (c) 2010-2013, Jean-Claude Beaudoin.

    MKCL is free software; you can redistribute it and/or modify it under
    the terms of the GNU General Lesser Public License as published by
    the Free Software Foundation; either version 3 of the License, or
    (at your option) any later version.

    See file './Copyright' for full details.
*/

#ifndef MKCL_CMP_H
#define MKCL_CMP_H

#include <mkcl/mkcl.h>

#include <math.h> /* for inline mathematics */

#include <mkcl/internal.h>


#define MKCL_TRAMPOLINK(narg, vv, lk, cblock)				\
  mkcl_va_list args; mkcl_va_start(args, narg, narg, 0);		\
  return(_mkcl_link_call(vv, (mkcl_objectfn *)lk, cblock, narg, args))

#define mkcl_def_constant_single_float(name,f)				\
  static const struct mkcl_doublefloat name ## __obj_ = {		\
    (uint8_t)mkcl_t_singlefloat, 0, 0, 0,				\
    (float)(f) };							\
  static const mkcl_object name = (mkcl_object)(& name ## __obj_)

#define mkcl_def_constant_double_float(name,f)				\
  static const struct mkcl_singlefloat name ## __obj_ = {		\
    (uint8_t)mkcl_t_doublefloat, 0, 0, 0,				\
    (double)(f) };							\
  static const mkcl_object name = (mkcl_object)(& name ## __obj_)

#define mkcl_def_constant_vector(name,type,raw,len)			\
  static const struct mkcl_vector name ## __obj_ = {			\
    (uint8_t)mkcl_t_vector, 0, FALSE, FALSE,				\
    mk_cl_Cnil, (mkcl_index)(len),					\
    (mkcl_index)(len), (mkcl_object*)(raw),				\
    mkcl_array_elem_accessor[type], mkcl_array_elem_setter[type],	\
    (type), 0 };							\
  static const mkcl_object name = (mkcl_object)(& name ## __obj_)
        
#define mkcl_def_ct_vector(name,type,raw,len,static,const)		\
  static const struct mkcl_vector name ## __obj_ = {			\
    (uint8_t)mkcl_t_vector, 0, FALSE, FALSE,				\
    mk_cl_Cnil, (mkcl_index)(len),					\
    (mkcl_index)(len), (mkcl_object*)(raw),				\
    mkcl_array_elem_accessor[type], mkcl_array_elem_setter[type],	\
    (type), 0 };							\
  static const mkcl_object name = (mkcl_object)(& name ## __obj_)

#define mkcl_cmp_dbg_lex_level(name, parent, descriptors, locations)	\
  struct mkcl_cmp_debug_lexical_level name = {				\
    (uint8_t) mkcl_t_cmp_dbg_lex_level, 0, 0, 0,			\
    parent,								\
    (sizeof(descriptors)/sizeof(descriptors[0])), descriptors,		\
    (sizeof(locations)/sizeof(locations[0])), locations }  

enum mkcl_locative_type {
  _mkcl_object_loc = 0,
  _mkcl_word_loc,
  _mkcl_base_char_loc,
  _mkcl_uni_char_loc,
  _mkcl_float_loc,
  _mkcl_double_loc,
  _mkcl_long_double_loc,
  _mkcl_float128_loc,
  _mkcl_closure_var_loc,
};

struct mkcl_lex_var_info {
  const char const * name;
  enum mkcl_locative_type type;
};

#define mkcl_cmp_fill_closure_level(disp, outer_disp)			\
  {									\
    mkcl_index i = 0;							\
    const mkcl_index max = outer_disp->display.nb_levels;		\
    mkcl_object * level = disp->display.level;				\
    mkcl_object * outer_level = outer_disp->display.level;		\
    for (; i < max; i++) level[i] = outer_level[i];			\
  }

#endif /* MKCL_CMP_H */
