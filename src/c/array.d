/* -*- mode: c -*- */
/*
    array.c --  Array routines
*/
/*
    Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
    Copyright (c) 1990, Giuseppe Attardi.
    Copyright (c) 2001, Juan Jose Garcia Ripoll.
    Copyright (c) 2011, Jean-Claude Beaudoin.

    MKCL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 3 of the License, or (at your option) any later version.

    See file '../../Copyright' for full details.
*/

#include <mkcl/mkcl.h>
#include <mkcl/internal.h>
#include <limits.h>
#include <string.h>

static const mkcl_index mkcl_aet_size[] = {
  sizeof(mkcl_object),        /* mkcl_aet_object */
  sizeof(mkcl_word),          /* mkcl_aet_fixnum */
  sizeof(float),              /* mkcl_aet_sf */
  sizeof(double),             /* mkcl_aet_df */
  0,                          /* mkcl_aet_bit: cannot be handled with this code */
  sizeof(mkcl_word),          /* mkcl_aet_word */
  sizeof(mkcl_index),         /* mkcl_aet_index */
  sizeof(uint8_t),            /* mkcl_aet_b8 */
  sizeof(int8_t),             /* mkcl_aet_i8 */
  sizeof(mkcl_uint16_t),
  sizeof(mkcl_int16_t),
  sizeof(mkcl_uint32_t),
  sizeof(mkcl_int32_t),
  sizeof(mkcl_uint64_t),
  sizeof(mkcl_int64_t),
  sizeof(mkcl_character),      /* mkcl_aet_ch */
  sizeof(mkcl_base_char)       /* mkcl_aet_bc */
};

mkcl_object (* const mkcl_array_elem_accessor[])(__MKCL, mkcl_object array, mkcl_index i) =
{
  mkcl_aref_index_object,
  mkcl_aref_index_fixnum,
  mkcl_aref_index_sf,
  mkcl_aref_index_df,
  mkcl_aref_index_bit,
  mkcl_aref_index_word,
  mkcl_aref_index_index,
  mkcl_aref_index_b8,
  mkcl_aref_index_i8,
  mkcl_aref_index_b16,
  mkcl_aref_index_i16,
  mkcl_aref_index_b32,
  mkcl_aref_index_i32,
  mkcl_aref_index_b64,
  mkcl_aref_index_i64,
  mkcl_aref_index_ch,
  mkcl_aref_index_bc
};

mkcl_object (* const mkcl_array_elem_setter[])(__MKCL, mkcl_object array, mkcl_index i, mkcl_object val) =
{
  mkcl_aset_index_object,
  mkcl_aset_index_fixnum,
  mkcl_aset_index_sf,
  mkcl_aset_index_df,
  mkcl_aset_index_bit,
  mkcl_aset_index_word,
  mkcl_aset_index_index,
  mkcl_aset_index_b8,
  mkcl_aset_index_i8,
  mkcl_aset_index_b16,
  mkcl_aset_index_i16,
  mkcl_aset_index_b32,
  mkcl_aset_index_i32,
  mkcl_aset_index_b64,
  mkcl_aset_index_i64,
  mkcl_aset_index_ch,
  mkcl_aset_index_bc

};

static void displace (MKCL, mkcl_object from, mkcl_object to, mkcl_object offset);

static void mkcl_FEbad_aet(MKCL) __attribute__((noreturn));

static void mkcl_FEbad_aet(MKCL)
{
  mkcl_FEerror(env, 
"A routine from MKCL got an object with a bad array element type.\n"
"If you are running a standard copy of MKCL, please report this bug.\n"
"If you are embedding MKCL into an application, please ensure you\n"
"passed the right value to the array creation routines.\n",0);
}

mkcl_object
mkcl_out_of_bounds_error(MKCL, mkcl_object fun, const char *place, mkcl_object value,
			 mkcl_index min, mkcl_index max)
{
  mkcl_object type = mk_cl_list(env, 3, @'integer', MKCL_MAKE_FIXNUM(min), MKCL_MAKE_FIXNUM(max - 1));
  return mkcl_type_error(env, fun, place, value, type);
}

mkcl_index
mkcl_to_array_index(MKCL, mkcl_object n)
{
  switch (mkcl_type_of(n)) {
  case mkcl_t_fixnum: {
    mkcl_word out = mkcl_fixnum_to_word(n);
#if MKCL_ADIMLIM < MKCL_MOST_POSITIVE_FIXNUM_RAW
    if (out < 0 || out >= MKCL_ADIMLIM)
      mkcl_FEtype_error_seq_index(env, mk_cl_Cnil, n);
#else
    if (out < 0)
      mkcl_FEtype_error_seq_index(env, mk_cl_Cnil, n);
#endif
    return out;
  }
  case mkcl_t_bignum:
    mkcl_FEtype_error_seq_index(env, mk_cl_Cnil, n);
  default:
    mkcl_FEtype_error_integer(env, n);
  }
}

mkcl_index mkcl_ensure_valid_array_index(MKCL, mkcl_object x, mkcl_index index)
{
  mkcl_index dim = x->array.dim;
  while (index >= dim)
    {
      mkcl_object i = mkcl_out_of_bounds_error(env, @'row-major-aref', "index", MKCL_MAKE_FIXNUM(index), 0, dim);
      index = mkcl_fixnum_to_word(i);
    }
  return index;
}

mkcl_object mkcl_ensure_valid_array_index_type(MKCL, mkcl_object x, mkcl_object index)
{
  mkcl_index dim = x->array.dim;
  while (!(MKCL_FIXNUMP(index) && mkcl_fixnum_to_word(index) < dim))
    index = mkcl_out_of_bounds_error(env, @'row-major-aref', "index", index, 0, dim);
  return index;
}


mkcl_object
mk_cl_row_major_aref(MKCL, mkcl_object x, mkcl_object indx)
{
  mkcl_index j;
 
  mkcl_call_stack_check(env);
  while (mkcl_unlikely(!MKCL_ARRAYP(x)))
    x = mkcl_type_error(env, @'row-major-aref', "argument", x, @'array');

  if (mkcl_unlikely(!(MKCL_FIXNUMP(indx) && ((j = mkcl_fixnum_to_word(indx)) < x->array.dim))))
    j = mkcl_fixnum_in_range(env, @'row-major-aref', "index", indx, 0, (mkcl_word)x->array.dim-1);

  @(return mkcl_aref_index(env, x, j));
}

mkcl_object
mk_si_row_major_aset(MKCL, mkcl_object x, mkcl_object indx, mkcl_object val)
{
  mkcl_index j;

  mkcl_call_stack_check(env);
  while (mkcl_unlikely(!MKCL_ARRAYP(x)))
    x = mkcl_type_error(env, @'si::row-major-aset', "argument", x, @'array');

  if (mkcl_unlikely(!(MKCL_FIXNUMP(indx) && ((j = mkcl_fixnum_to_word(indx)) < x->array.dim))))
    j = mkcl_fixnum_in_range(env, @'si::row-major-aset', "index", indx, 0, (mkcl_word)x->array.dim-1);

  @(return mkcl_aset_index(env, x, j, val));
}

@(defun array-row-major-index (x &rest indx)
@
  {
    mkcl_index i, j;
    mkcl_index r = narg - 1;
  AGAIN:
    switch (mkcl_type_of(x)) {
    case mkcl_t_array:
      if (r != x->array.rank)
	mkcl_FEerror(env, "Wrong number of indices.", 0);
      for (i = j = 0;  i < r;  i++) {
	mkcl_object index = mkcl_va_arg(indx);
	mkcl_index dim = x->array.dims[i];
	mkcl_index s;

	if (!(MKCL_FIXNUMP(index) && ((s = mkcl_fixnum_to_word(index)) < dim)))
	  s = mkcl_fixnum_in_range(env, @'array-row-major-index', "index", index, 0, (mkcl_word)dim-1);
	j = j*dim + s;
      }
      break;
    case mkcl_t_vector:
    case mkcl_t_string:
    case mkcl_t_base_string:
    case mkcl_t_bitvector:
      if (r != 1)
	mkcl_FEerror(env, "Wrong number of indices.", 0);
      {
	mkcl_object index = mkcl_va_arg(indx);

	if (!(MKCL_FIXNUMP(index) && ((j = mkcl_fixnum_to_word(index)) < x->vector.dim)))
	  j = mkcl_fixnum_in_range(env, @'array-row-major-index', "index", index, 0, (mkcl_word)x->vector.dim-1);
      }
      break;
    default:
      x = mkcl_type_error(env, @'aref',"argument",x,@'array');
      goto AGAIN;
    }
    /* By construction, "j" is a valid array index and should thus be within the range of fixnum. */
    @(return MKCL_MAKE_FIXNUM(j));
  } 
@)

mkcl_index mkcl_array_row_major_index_2_t(MKCL, mkcl_object a, mkcl_object i, mkcl_object j)
{
  mkcl_index i_i;
  mkcl_index i_j;

    while (mkcl_unlikely(!MKCL_ARRAYP(a))) a = mkcl_ensure_array_type(env, a);

  /* "a" must be an array but this function will survive being passed any
     sub-type of array for that argument. That is thanks to the fact that
     field "rank" has the same offset as field "hasfillp" of vectors and
     that "hasfillp" can have only two different values, either 1 (TRUE)
     of 0 (FALSE).
  */
    if (mkcl_unlikely(2 != a->array.rank))
    mkcl_FEerror(env, "Wrong number of indices.", 0);
    else if (mkcl_likely(MKCL_FIXNUMP(i) && MKCL_FIXNUMP(j)))
    {
      i_i = mkcl_fixnum_to_word(i);
      i_j = mkcl_fixnum_to_word(j);
      return mkcl_array_row_major_index_2_index(env, a, i_i, i_j);
    }
  else
    {
      i_i = mkcl_fixnum_in_range(env, @'array-row-major-index', "index", i, 0, (mkcl_word)a->array.dims[0]-1);
      i_j = mkcl_fixnum_in_range(env, @'array-row-major-index', "index", j, 0, (mkcl_word)a->array.dims[1]-1);
      return mkcl_array_row_major_index_2_index(env, a, i_i, i_j);
    }
}

mkcl_index mkcl_array_row_major_index_3_t(MKCL, mkcl_object a, mkcl_object i, mkcl_object j, mkcl_object k)
{
  mkcl_index i_i;
  mkcl_index i_j;
  mkcl_index i_k;

  while (mkcl_unlikely(!MKCL_ARRAYP(a))) a = mkcl_ensure_array_type(env, a);

  /* "a" must be an array but this function will survive being passed any
     sub-type of array for that argument. That is thanks to the fact that
     field "rank" has the same offset as field "hasfillp" of vectors and
     that "hasfillp" can have only two different values, either 1 (TRUE)
     of 0 (FALSE).
  */
  if (mkcl_unlikely(3 != a->array.rank))
    mkcl_FEerror(env, "Wrong number of indices.", 0);

  if (mkcl_likely(MKCL_FIXNUMP(i) && MKCL_FIXNUMP(j) && MKCL_FIXNUMP(k)))
    {
      i_i = mkcl_fixnum_to_word(i);
      i_j = mkcl_fixnum_to_word(j);
      i_k = mkcl_fixnum_to_word(k);
      return mkcl_array_row_major_index_3_index(env, a, i_i, i_j, i_k);
    }
  else
    {
      i_i = mkcl_fixnum_in_range(env, @'array-row-major-index', "index", i, 0, (mkcl_word)a->array.dims[0]-1);
      i_j = mkcl_fixnum_in_range(env, @'array-row-major-index', "index", j, 0, (mkcl_word)a->array.dims[1]-1);
      i_k = mkcl_fixnum_in_range(env, @'array-row-major-index', "index", k, 0, (mkcl_word)a->array.dims[2]-1);
      return mkcl_array_row_major_index_3_index(env, a, i_i, i_j, i_k);
    }
}

@(defun aref (x &rest indx)
@
  {
    mkcl_index i, j;
    mkcl_index r = narg - 1;
  AGAIN:
    switch (mkcl_type_of(x)) {
    case mkcl_t_array:
      if (r != x->array.rank)
	mkcl_FEerror(env, "Wrong number of indices.", 0);
      for (i = j = 0;  i < r;  i++) {
	mkcl_object index = mkcl_va_arg(indx);
	mkcl_index dim = x->array.dims[i];
	mkcl_index s;

	if (!(MKCL_FIXNUMP(index) && ((s = mkcl_fixnum_to_word(index)) < dim)))
	  s = mkcl_fixnum_in_range(env, @'aref', "index", index, 0, (mkcl_word)dim-1);
	j = j*dim + s;
      }
      break;
    case mkcl_t_vector:
    case mkcl_t_string:
    case mkcl_t_base_string:
    case mkcl_t_bitvector:
      if (r != 1)
	mkcl_FEerror(env, "Wrong number of indices.", 0);
      {
	mkcl_object index = mkcl_va_arg(indx);

	if (!(MKCL_FIXNUMP(index) && ((j = mkcl_fixnum_to_word(index)) < x->vector.dim)))
	  j = mkcl_fixnum_in_range(env, @'aref', "index", index, 0, (mkcl_word)x->vector.dim-1);
      }
      break;
    default:
      x = mkcl_type_error(env, @'aref',"argument",x,@'array');
      goto AGAIN;
    }
    @(return mkcl_aref_index(env, x, j));
  } 
@)

mkcl_object
mkcl_aref(MKCL, mkcl_object x, mkcl_object index)
{
  while (mkcl_unlikely(!MKCL_ARRAYP(x)))
    x = mkcl_type_error(env, @'aref',"argument",x,@'array');

  if (mkcl_likely(MKCL_FIXNUMP(index)))
    return mkcl_aref_index(env, x, mkcl_fixnum_to_word(index));
  else
    {
      index = mkcl_ensure_valid_array_index_type(env, x, index);
      return mkcl_aref_index(env, x, mkcl_fixnum_to_word(index));
    }
}

mkcl_object
mkcl_vref(MKCL, mkcl_object v, mkcl_object index)
{
  while (mkcl_unlikely(!MKCL_VECTORP(v))) v = mkcl_ensure_vector_type(env, v);

  if (mkcl_likely(MKCL_FIXNUMP(index)))
    return mkcl_vref_index(env, v, mkcl_fixnum_to_word(index));
  else
    {
      index = mkcl_ensure_valid_array_index_type(env, v, index);
      return mkcl_vref_index(env, v, mkcl_fixnum_to_word(index));
    }
}

/*
	Internal function for setting array elements:

		(si:aset value array dim0 ... dimN)
*/
@(defun si::aset (v x &rest dims)
@ 
  {
    mkcl_index i, j;
    mkcl_index r = narg - 2;
  AGAIN:
    switch (mkcl_type_of(x)) {
    case mkcl_t_array:
      if (r != x->array.rank)
	mkcl_FEerror(env, "Wrong number of indices.", 0);
      for (i = j = 0;  i < r;  i++) {
	mkcl_object index = mkcl_va_arg(dims);
	mkcl_index dim = x->array.dims[i];
	mkcl_index s;

	if (!(MKCL_FIXNUMP(index) && ((s = mkcl_fixnum_to_word(index)) < dim)))
	  s =mkcl_fixnum_in_range(env, @'si::aset', "index", index, 0, (mkcl_word)dim-1);
	j = j*dim + s;
      }
      break;
    case mkcl_t_vector:
    case mkcl_t_string:
    case mkcl_t_base_string:
    case mkcl_t_bitvector:
      if (r != 1)
	mkcl_FEerror(env, "Wrong number of indices.", 0);
      {
	mkcl_object index = mkcl_va_arg(dims);

	if (!(MKCL_FIXNUMP(index) && ((j = mkcl_fixnum_to_word(index)) < x->vector.dim)))
	  j = mkcl_fixnum_in_range(env, @'si::aset',"index", index, 0, (mkcl_word)x->vector.dim - 1);
      }
      break;
    default:
      x = mkcl_type_error(env, @'si::aset',"destination",v,@'array');
      goto AGAIN;
    }
    @(return mkcl_aset_index(env, x, j, v));
  } 
@)

mkcl_index mkcl_ensure_index_for_array_row_major_index(MKCL, mkcl_index i, mkcl_index dim)
{
  return mkcl_fixnum_in_range(env, @'array-row-major-index', "index", MKCL_MAKE_FIXNUM(i), 0, (mkcl_word)dim-1);
}

mkcl_object mkcl_ensure_bit_type_for_aset(MKCL, mkcl_object v)
{
  return MKCL_MAKE_FIXNUM(mkcl_fixnum_in_range(env, @'si::aset', "bit", v, 0, 1));
}

mkcl_object mkcl_ensure_string_type(MKCL, mkcl_object x)
{
  return mkcl_type_error(env, @'aref',"argument",x, @'string');
}

mkcl_object mkcl_ensure_base_string_type(MKCL, mkcl_object x)
{
  return mkcl_type_error(env, @'aref',"argument",x, @'base-string');
}

mkcl_object mkcl_ensure_vector_type(MKCL, mkcl_object x)
{
  return mkcl_type_error(env, @'aref',"argument",x, @'vector');
}

mkcl_object mkcl_ensure_specialized_vector_type(MKCL, mkcl_object x, mkcl_elttype elem_type)
{
  mkcl_object elem_type_name = mkcl_elttype_to_symbol(env, elem_type);

  return mkcl_type_error(env, @'aref',"argument",x, mk_cl_list(env, 2, @'vector', elem_type_name));
}
mkcl_object mkcl_ensure_array_type(MKCL, mkcl_object x)
{
  return mkcl_type_error(env, @'aref',"argument",x, @'array');
}

mkcl_object mkcl_ensure_specialized_array_type(MKCL, mkcl_object x, mkcl_elttype elem_type)
{
  mkcl_object elem_type_name = mkcl_elttype_to_symbol(env, elem_type);

  return mkcl_type_error(env, @'aref',"argument",x, mk_cl_list(env, 2, @'array', elem_type_name));
}

mkcl_object
mkcl_aset(MKCL, mkcl_object x, mkcl_object index, mkcl_object value)
{
  while (mkcl_unlikely(!MKCL_ARRAYP(x)))
    x = mkcl_type_error(env, @'aref',"argument",x,@'array');

  if (mkcl_likely(MKCL_FIXNUMP(index)))
    return x->array.set(env, x, mkcl_fixnum_to_word(index), value);
  else
    {
      index = mkcl_ensure_valid_array_index_type(env, x, index);
      return x->array.set(env, x, mkcl_fixnum_to_word(index), value);
    }
}


mkcl_object
mkcl_vset(MKCL, mkcl_object v, mkcl_object index, mkcl_object val)
{
  while (mkcl_unlikely(!MKCL_VECTORP(v))) v = mkcl_ensure_vector_type(env, v);

  if (mkcl_likely(MKCL_FIXNUMP(index)))
    return mkcl_vset_index(env, v, mkcl_fixnum_to_word(index), val);
  else
    {
      index = mkcl_ensure_valid_array_index_type(env, v, index);
      return mkcl_vset_index(env, v, mkcl_fixnum_to_word(index), val);
    }
}

mkcl_object mkcl_bvref(MKCL, mkcl_object v, mkcl_object index)
{
  while (mkcl_unlikely(!(MKCL_VECTORP(v) && v->vector.elttype == mkcl_aet_bit)))
    v = mkcl_ensure_specialized_vector_type(env, v, mkcl_aet_bit);

  if (mkcl_likely(MKCL_FIXNUMP(index)))
    return mkcl_bvref_index(env, v, mkcl_fixnum_to_word(index));
  else
    {
      index = mkcl_ensure_valid_array_index_type(env, v, index);
      return mkcl_bvref_index(env, v, mkcl_fixnum_to_word(index));
    }
}

mkcl_object mkcl_bvset(MKCL, mkcl_object v, mkcl_object index, mkcl_object val)
{
  while (mkcl_unlikely(!(MKCL_VECTORP(v) && v->vector.elttype == mkcl_aet_bit)))
    v = mkcl_ensure_specialized_vector_type(env, v, mkcl_aet_bit);

  if (mkcl_likely(MKCL_FIXNUMP(index)))
    return mkcl_bvset_index(env, v, mkcl_fixnum_to_word(index), val);
  else
    {
      index = mkcl_ensure_valid_array_index_type(env, v, index);
      return mkcl_bvset_index(env, v, mkcl_fixnum_to_word(index), val);
    }
}

mkcl_object mkcl_ensure_vector_type_for_vector_push(MKCL, mkcl_object vec)
{ return mkcl_type_error(env, @'vector-push', "vector", vec, @'vector'); }

mkcl_object mkcl_ensure_vector_type_for_vector_push_extend(MKCL, mkcl_object vec)
{ return mkcl_type_error(env, @'vector-push-extend', "vector", vec, @'vector'); }

mkcl_object mkcl_ensure_string_type_for_vector_push(MKCL, mkcl_object vec)
{ return mkcl_type_error(env, @'vector-push', "vector", vec, @'string'); }

mkcl_object mkcl_ensure_string_type_for_vector_push_extend(MKCL, mkcl_object vec)
{ return mkcl_type_error(env, @'vector-push-extend', "vector", vec, @'string'); }

mkcl_object mkcl_ensure_base_string_type_for_vector_push(MKCL, mkcl_object vec)
{ return mkcl_type_error(env, @'vector-push', "vector", vec, @'base-string'); }

mkcl_object mkcl_ensure_base_string_type_for_vector_push_extend(MKCL, mkcl_object vec)
{ return mkcl_type_error(env, @'vector-push-extend', "vector", vec, @'base-string'); }

mkcl_object
mk_cl_svref(MKCL, mkcl_object x, mkcl_object index)
{
  mkcl_index i;

  mkcl_call_stack_check(env);
  while (mkcl_unlikely(mkcl_type_of(x) != mkcl_t_vector ||
		       x->vector.adjustable ||
		       x->vector.hasfillp ||
		       MKCL_CAR(x->vector.displaced) != mk_cl_Cnil ||
		       (mkcl_elttype)x->vector.elttype != mkcl_aet_object))
    { x = mkcl_type_error(env, @'svref',"argument",x,@'simple-vector'); }

  if (mkcl_unlikely(!(MKCL_FIXNUMP(index) && ((i = mkcl_fixnum_to_word(index)) < x->vector.dim))))
    i = mkcl_fixnum_in_range(env, @'svref', "index", index, 0, (mkcl_word)x->vector.dim-1);

  @(return x->vector.self.t[i]);
}

mkcl_object
mk_si_svset(MKCL, mkcl_object x, mkcl_object index, mkcl_object v)
{
  mkcl_index i;

  mkcl_call_stack_check(env);
  while (mkcl_unlikely(mkcl_type_of(x) != mkcl_t_vector ||
		       x->vector.adjustable ||
		       x->vector.hasfillp ||
		       MKCL_CAR(x->vector.displaced) != mk_cl_Cnil ||
		       (mkcl_elttype)x->vector.elttype != mkcl_aet_object))
    { x = mkcl_type_error(env, @'si::svset',"argument",x,@'simple-vector'); }

  if (mkcl_unlikely(!(MKCL_FIXNUMP(index) && ((i = mkcl_fixnum_to_word(index)) < x->vector.dim))))
    i = mkcl_fixnum_in_range(env, @'svref', "index", index, 0, (mkcl_word)x->vector.dim-1);

  @(return (x->vector.self.t[i] = v));
}



/*
	Internal function for making arrays of more than one dimension:

		(si:make-pure-array dimension-list element-type adjustable
			            displaced-to displaced-index-offset)
*/
mkcl_object
mk_si_make_pure_array(MKCL, mkcl_object etype, mkcl_object dims, mkcl_object adj,
		      mkcl_object fillp, mkcl_object displ, mkcl_object disploff)
{
  mkcl_index r, s, i, j;
  mkcl_object x;
  if (MKCL_FIXNUMP(dims)) {
    return mk_si_make_vector(env, etype, dims, adj, fillp, displ, disploff);
  }
  r = mkcl_length(env, dims);
  if (r >= MKCL_ARANKLIM) {
    mkcl_FEerror(env, "The array rank, ~R, is too large.", 1, MKCL_MAKE_FIXNUM(r));
  } else if (r == 1) {
    return mk_si_make_vector(env, etype, MKCL_CONS_CAR(dims), adj, fillp, displ, disploff);
  } else if (!mkcl_Null(fillp)) {
    mkcl_FEerror(env, ":FILL-POINTER may not be specified for an array of rank ~D",
		 1, MKCL_MAKE_FIXNUM(r));
  }
  x = mkcl_alloc_raw_array(env);
  x->array.displaced = mk_cl_Cnil;
  x->array.self.t = NULL;		/* for GC sake */
  x->array.rank = r;
  x->array.elttype = mkcl_symbol_to_elttype(env, etype);
  x->array.bit_offset = 0;
  x->array.dims = mkcl_alloc_atomic_align(env, sizeof(mkcl_index)*r, sizeof(mkcl_index));
  for (i = 0, s = 1;  i < r;  i++, dims = MKCL_CONS_CDR(dims)) {
    j = mkcl_fixnum_in_range(env, @'make-array', "dimension", MKCL_CONS_CAR(dims), 0, MKCL_ADIMLIM);
    if ((MKCL_ATOTLIM / s) < j)
      mkcl_FEerror(env, "The array total size, ~D, is too large.", 1, mkcl_word_times(env, s, j));
    s *= (x->array.dims[i] = j);
  }

  x->array.dim = s;

  x->array.adjustable = adj != mk_cl_Cnil;

  x->array.elem = mkcl_array_elem_accessor[x->array.elttype];
  x->array.set = mkcl_array_elem_setter[x->array.elttype];

  if (mkcl_Null(etype))
    {
      if (s != 0)
  	mkcl_FEerror(env, "Cannot allocate non-empty array with element type NIL", 0);
    }
  else if (mkcl_Null(displ))
    mkcl_array_allocself(env, x);
  else
    displace(env, x, displ, disploff);
  @(return x);
}

/*
	Internal function for making vectors:

		(si:make-vector element-type dimension adjustable fill-pointer
				displaced-to displaced-index-offset)
*/
mkcl_object
mk_si_make_vector(MKCL, mkcl_object etype, mkcl_object dim, mkcl_object adj,
		  mkcl_object fillp, mkcl_object displ, mkcl_object disploff)
{
  mkcl_index d, f;
  mkcl_object x;
  mkcl_elttype aet;
 AGAIN:
  aet = mkcl_symbol_to_elttype(env, etype);
  d = mkcl_fixnum_in_range(env, @'make-array', "dimension", dim, 0, MKCL_ADIMLIM);
  if (aet == mkcl_aet_bc) {
    x = mkcl_alloc_raw_base_string(env);
    x->base_string.elem = mkcl_base_char_index;
    x->base_string.set = mkcl_base_char_set_index;
  } else if (aet == mkcl_aet_bit) {
    x = mkcl_alloc_raw_bitvector(env);
    x->vector.elem = mkcl_array_elem_accessor[aet];
    x->vector.set = mkcl_array_elem_setter[aet];
    x->vector.elttype = mkcl_aet_bit;
    x->vector.bit_offset = 0;
  } else if (aet == mkcl_aet_ch) {
    x = mkcl_alloc_raw_string(env);
    x->string.elem = mkcl_character_index;
    x->string.set = mkcl_character_set_index;
  } else {
    x = mkcl_alloc_raw_vector(env);
    x->vector.elem = mkcl_array_elem_accessor[aet];
    x->vector.set = mkcl_array_elem_setter[aet];
    x->vector.elttype = aet;
    x->vector.bit_offset = 0;
  }
  x->vector.self.t = NULL;
  x->vector.displaced = mk_cl_Cnil;
  x->vector.dim = d;
  x->vector.adjustable = adj != mk_cl_Cnil;
  if (mkcl_Null(fillp)) {
    x->vector.hasfillp = FALSE;
    f = d;
  } else if (fillp == mk_cl_Ct) {
    x->vector.hasfillp = TRUE;
    f = d;
  } else if (MKCL_FIXNUMP(fillp) && ((f = mkcl_fixnum_to_word(fillp)) <= d) && (f >= 0)) {
    x->vector.hasfillp = TRUE;
  } else {
    fillp = mkcl_type_error(env, @'make-array',"fill pointer",fillp,
			    mk_cl_list(env, 3,@'or',mk_cl_list(env, 3,@'member',mk_cl_Cnil,mk_cl_Ct),
				       mk_cl_list(env, 3,@'integer',MKCL_MAKE_FIXNUM(0), dim)));
    goto AGAIN;
  }
  x->vector.fillp = f;

  if (mkcl_Null(displ))
    mkcl_array_allocself(env, x);
  else
    displace(env, x, displ, disploff);
  @(return x);
}

mkcl_object
mkcl_alloc_simple_vector(MKCL, mkcl_index l, mkcl_elttype aet)
{
  mkcl_object x;

  switch (aet)
    {
    case mkcl_aet_bc:
      return mkcl_alloc_simple_base_string(env, l);
    case mkcl_aet_ch:
      return mkcl_alloc_simple_extended_string(env, l);
    case mkcl_aet_bit:
      x = mkcl_alloc_raw_bitvector(env);
      x->vector.hasfillp = FALSE;
      x->vector.adjustable = FALSE;
      x->vector.displaced = mk_cl_Cnil;
      x->vector.dim = x->vector.fillp = l;
      x->vector.elem = mkcl_array_elem_accessor[mkcl_aet_bit];
      x->vector.set = mkcl_array_elem_setter[mkcl_aet_bit];
      x->vector.self.bit = NULL;
      x->vector.elttype = mkcl_aet_bit;
      x->vector.bit_offset = 0;
      break;
    default:
      x = mkcl_alloc_raw_vector(env);
      x->vector.hasfillp = FALSE;
      x->vector.adjustable = FALSE;
      x->vector.displaced = mk_cl_Cnil;
      x->vector.dim = x->vector.fillp = l;
      x->vector.elem = mkcl_array_elem_accessor[aet];
      x->vector.set = mkcl_array_elem_setter[aet];
      x->vector.self.t = NULL;
      x->vector.elttype = aet;
      x->vector.bit_offset = 0;
      break;
    }
  mkcl_array_allocself(env, x);
  return(x);
}

void
mkcl_array_allocself(MKCL, mkcl_object x)
{
  mkcl_elttype t = mkcl_array_elttype(env, x);
  mkcl_index i, d = x->array.dim;
  switch (t) {
    /* assign self field only after it has been filled, for GC sake  */
  case mkcl_aet_object:
    {
      mkcl_object *elts;
      elts = (mkcl_object *)mkcl_alloc_align(env, sizeof(mkcl_object)*d, sizeof(mkcl_object));
      for (i = 0; i < d;  i++)
	elts[i] = mk_cl_Cnil;
      x->array.self.t = elts;
    }
    return;
  case mkcl_aet_fixnum:
    {
      mkcl_word *elts;
      elts = (mkcl_word *)mkcl_alloc_align(env, sizeof(mkcl_word)*d, sizeof(mkcl_word));
      for (i = 0; i < d;  i++)
	elts[i] = 0;
      x->array.self.word = elts;
    }
    return;
  case mkcl_aet_ch:
    {
      mkcl_character *elts;
      d += 1;
      d *= sizeof(mkcl_character);
      elts = mkcl_alloc_atomic_align(env, d, sizeof(mkcl_character));
      memset(elts, 0, d);
      x->string.self = elts;
    }
    return;
  case mkcl_aet_bit:
#if CHAR_BIT == 8
    d = (d + 7) >> 3; /* round up to next boundary of CHAR_BIT. */
#else
    d = (d + (CHAR_BIT-1)) / CHAR_BIT;
#endif
    x->vector.self.bit = (mkcl_byte *)mkcl_alloc_atomic(env, d);
    x->vector.bit_offset = 0;
    break;
  case mkcl_aet_bc:
    {
      mkcl_base_char * elts;
      d += 1;
      d *= sizeof(mkcl_base_char);
      elts = mkcl_alloc_atomic_align(env, d, sizeof(mkcl_base_char));
      memset(elts, 0, d);
      x->base_string.self = elts;
    }
    break;
  case mkcl_aet_nil: break;
  default:
    if (t < 0 || t >= MKCL_NB_ELEMS(mkcl_aet_size))
      mkcl_lose(env, "Out of bounds access to array mkcl_aet_size[].");
    else
      {
	mkcl_index elt_size = mkcl_aet_size[t];
	d *= elt_size;
	x->vector.self.bc = mkcl_alloc_align(env, d, elt_size);
      }
  }
}

mkcl_elttype
mkcl_symbol_to_elttype(MKCL, mkcl_object x)
{
 BEGIN:
  if (x == @'base-char')
    return(mkcl_aet_bc);
  else if (x == @'character')
    return(mkcl_aet_ch);
  else if (x == @'t')
    return(mkcl_aet_object);
  else if (x == @'bit')
    return(mkcl_aet_bit);
  else if (x == @'fixnum')
    return(mkcl_aet_fixnum);
  else if (x == @'mkcl::cl-word')
    return(mkcl_aet_word);
  else if (x == @'mkcl::cl-index')
    return(mkcl_aet_index);
  else if (x == @'single-float' || x == @'short-float')
    return(mkcl_aet_sf);
  else if (x == @'double-float')
    return(mkcl_aet_df);
  else if (x == @'long-float') {
#ifdef MKCL_LONG_FLOAT
    return(mkcl_aet_object);
#else
    return(mkcl_aet_df);
#endif
  } else if (x == @'mkcl::natural8')
    return(mkcl_aet_b8);
  else if (x == @'mkcl::integer8')
    return(mkcl_aet_i8);
  else if (x == @'mkcl::natural16')
    return(mkcl_aet_b16);
  else if (x == @'mkcl::integer16')
    return(mkcl_aet_i16);
  else if (x == @'mkcl::natural32')
    return(mkcl_aet_b32);
  else if (x == @'mkcl::integer32')
    return(mkcl_aet_i32);
  else if (x == @'mkcl::natural64')
    return(mkcl_aet_b64);
  else if (x == @'mkcl::integer64')
    return(mkcl_aet_i64);
  else if (x == mk_cl_Cnil) {
    return(mkcl_aet_nil);
  }
  x = mk_cl_upgraded_array_element_type(env, 1, x);
  goto BEGIN;
}

mkcl_object
mkcl_elttype_to_symbol(MKCL, mkcl_elttype aet)
{
  mkcl_object output;
  switch (aet)
    {
    case mkcl_aet_object:	output = mk_cl_Ct; break;
    case mkcl_aet_fixnum:       output = @'fixnum'; break;
    case mkcl_aet_ch:		output = @'character'; break;
    case mkcl_aet_bc:		output = @'base-char'; break;
    case mkcl_aet_bit:		output = @'bit'; break;
    case mkcl_aet_word:		output = @'mkcl::cl-word'; break;
    case mkcl_aet_index:	output = @'mkcl::cl-index'; break;
    case mkcl_aet_sf:		output = @'single-float'; break;
    case mkcl_aet_df:		output = @'double-float'; break;
    case mkcl_aet_b8:		output = @'mkcl::natural8'; break;
    case mkcl_aet_i8:		output = @'mkcl::integer8'; break;
    case mkcl_aet_b16:		output = @'mkcl::natural16'; break;
    case mkcl_aet_i16:		output = @'mkcl::integer16'; break;
    case mkcl_aet_b32:		output = @'mkcl::natural32'; break;
    case mkcl_aet_i32:		output = @'mkcl::integer32'; break;
    case mkcl_aet_b64:		output = @'mkcl::natural64'; break;
    case mkcl_aet_i64:		output = @'mkcl::integer64'; break;
    case mkcl_aet_nil:		output = mk_cl_Cnil; break;
    default: mkcl_lose(env, "Unknown elttype"); break;
    }
  return output;
}

static void *
address_inc(MKCL, void *address, mkcl_word inc, mkcl_elttype elt_type)
{
  union mkcl_array_data aux;
  aux.t = address;
  switch (elt_type) {
  case mkcl_aet_object:
    return aux.t + inc;
  case mkcl_aet_fixnum:
  case mkcl_aet_word:
    return aux.word + inc;
  case mkcl_aet_index:
    return aux.index + inc;
  case mkcl_aet_sf:
    return aux.sf + inc;
  case mkcl_aet_bc:
    return aux.bc + inc;
  case mkcl_aet_ch:
    return aux.c + inc;
  case mkcl_aet_df:
    return aux.df + inc;
  case mkcl_aet_b8:
  case mkcl_aet_i8:
    return aux.b8 + inc;
  case mkcl_aet_b16:
  case mkcl_aet_i16:
    return aux.b16 + inc;
  case mkcl_aet_b32:
  case mkcl_aet_i32:
    return aux.b32 + inc;
  case mkcl_aet_b64:
  case mkcl_aet_i64:
    return aux.b64 + inc;
  default:
    mkcl_FEbad_aet(env);
  }
}

static void *
array_address(MKCL, mkcl_object x, mkcl_index inc)
{
  return address_inc(env, x->array.self.t, inc, mkcl_array_elttype(env, x));
}

mkcl_object
mk_cl_array_element_type(MKCL, mkcl_object a)
{
  mkcl_call_stack_check(env);
  @(return mkcl_elttype_to_symbol(env, mkcl_array_elttype(env, a)))
}

/*
	Displace(from, to, offset) displaces the from-array
	to the to-array (the original array) by the specified offset.
	It changes the a_displaced field of both arrays.
	The field is a cons; the car of the from-array points to
	the to-array and the cdr of the to-array is a list of arrays
	displaced to the to-array, so the from-array is pushed to the
	cdr of the to-array's array.displaced.
*/
static void
displace(MKCL, mkcl_object from, mkcl_object to, mkcl_object offset)
{
  mkcl_index j;
  void *base;
  mkcl_elttype totype, fromtype;
  fromtype = mkcl_array_elttype(env, from);
  if (mkcl_type_of(to) == mkcl_t_foreign) {
    if (fromtype == mkcl_aet_bit || fromtype == mkcl_aet_object) {
      mkcl_FEerror(env, "Cannot displace arrays with element type T or BIT onto foreign data",0);
    }
    base = to->foreign.data;
    j = mkcl_fixnum_in_range(env, @'adjust-array',"array displacement", offset,
			     0, MKCL_MOST_POSITIVE_FIXNUM);
    /* FIXME: should check for foreign data size. JCB */
    from->array.displaced = mkcl_list1(env, to);
  } else {
    totype = mkcl_array_elttype(env, to);
    if (totype != fromtype)
      mkcl_FEerror(env, 
		   "Cannot displace the array,~%"
		   "because the element types don't match.", 0);
    if (from->array.dim > to->array.dim)
      mkcl_FEerror(env,
		   "Cannot displacqe the array,~%"
		   "because the total size of the to-array is too small.", 0);
    j = mkcl_fixnum_in_range(env, @'adjust-array',"array displacement",offset,
			     0, to->array.dim - from->array.dim);
    from->array.displaced = mkcl_list1(env, to);
    if (mkcl_Null(to->array.displaced))
      to->array.displaced = mkcl_list1(env, mk_cl_Cnil);
    MKCL_RPLACD(to->array.displaced, MKCL_CONS(env, from, MKCL_CDR(to->array.displaced)));
    if (fromtype == mkcl_aet_bit) {
      j += to->vector.bit_offset;
      from->vector.bit_offset = j%CHAR_BIT;
      from->vector.self.bit = to->vector.self.bit + j/CHAR_BIT;
      return;
    }
    base = to->array.self.t;
  }
  from->array.self.t = address_inc(env, base, j, fromtype);
}

mkcl_elttype
mkcl_array_elttype(MKCL, mkcl_object x)
{
  switch(mkcl_type_of(x)) 
    {
    case mkcl_t_array:
      return((mkcl_elttype)x->array.elttype);
    case mkcl_t_vector:
      return((mkcl_elttype)x->vector.elttype);
    case mkcl_t_string:
      return(mkcl_aet_ch);
    case mkcl_t_base_string:
      return(mkcl_aet_bc);
    case mkcl_t_bitvector:
      return(mkcl_aet_bit);
    default:
      mkcl_FEwrong_type_argument(env, @'array', x);
    }
}

mkcl_object
mk_cl_array_rank(MKCL, mkcl_object a)
{
  mkcl_call_stack_check(env);
  mkcl_assert_type_array(env, a);
  @(return ((mkcl_type_of(a) == mkcl_t_array)
	    ? MKCL_MAKE_FIXNUM(a->array.rank)
	    : MKCL_MAKE_FIXNUM(1)));
}

mkcl_object
mk_cl_array_dimension(MKCL, mkcl_object a, mkcl_object index)
{
  mkcl_index dim;

  mkcl_call_stack_check(env);
 AGAIN:
  switch (mkcl_type_of(a)) 
    {
    case mkcl_t_array:
      {
	int i = mkcl_fixnum_in_range(env, @'array-dimension',"dimension",index,0,a->array.rank);
	dim  = a->array.dims[i];
      }
      break;
    case mkcl_t_string:
      mkcl_fixnum_in_range(env, @'array-dimension',"dimension",index,0,0);
      dim = a->string.dim;
      break;
    case mkcl_t_base_string:
      mkcl_fixnum_in_range(env, @'array-dimension',"dimension",index,0,0);
      dim = a->base_string.dim;
      break;
    case mkcl_t_vector:
      mkcl_fixnum_in_range(env, @'array-dimension',"dimension",index,0,0);
      dim = a->vector.dim;
      break;
    case mkcl_t_bitvector:
      mkcl_fixnum_in_range(env, @'array-dimension',"dimension",index,0,0);
      dim = a->vector.dim;
      break;
    default:
      a = mkcl_type_error(env, @'array-dimension',"argument",a,@'array');
      goto AGAIN;
    }
  @(return MKCL_MAKE_FIXNUM(dim));
}

mkcl_object
mk_cl_array_total_size(MKCL, mkcl_object a)
{
  mkcl_call_stack_check(env);
  @(return MKCL_MAKE_FIXNUM(mkcl_array_total_size(env, a)));
}

mkcl_object
mk_cl_adjustable_array_p(MKCL, mkcl_object a)
{
  mkcl_call_stack_check(env);
  mkcl_assert_type_array(env, a);
  @(return (a->array.adjustable ? mk_cl_Ct : mk_cl_Cnil));
}

/*
	Internal function for checking if an array is displaced.
*/
mkcl_object
mk_cl_array_displacement(MKCL, mkcl_object a)
{
  mkcl_object to_array;
  mkcl_index offset;

  mkcl_call_stack_check(env);
  mkcl_assert_type_array(env, a);
  to_array = a->array.displaced;
  if (mkcl_Null(to_array)) {
    offset = 0; /* We're not displaced to and not displaced ourselves. */
  } else if (mkcl_Null(to_array = MKCL_CAR(a->array.displaced))) {
    offset = 0; /* We are displaced to but not displaced ourselves. */
  } else {
    /* We are a displaced array. */
    union mkcl_array_data self;

    switch (mkcl_type_of(to_array))
      {
      case mkcl_t_foreign:
	self.t = (mkcl_object *) to_array->foreign.data;
	break;
      case mkcl_t_array:
	self = to_array->array.self;
	break;
      case mkcl_t_bitvector:
      case mkcl_t_vector:
	self = to_array->vector.self;
	break;
      case mkcl_t_string:
	self.c = to_array->string.self;
	break;
      case mkcl_t_base_string:
	self.bc = to_array->base_string.self;
	break;
      default:
	mkcl_FEerror(env, "Corrupted internal array displacement information", 0);
      }

    switch (mkcl_array_elttype(env, a)) {
    case mkcl_aet_object:
      offset = a->array.self.t - self.t;
      break;
    case mkcl_aet_bc:
      offset = a->array.self.bc - self.bc;
      break;
    case mkcl_aet_ch:
      offset = a->array.self.c - self.c;
      break;
    case mkcl_aet_bit:
      offset = a->array.self.bit - self.bit;
      offset = offset * CHAR_BIT + a->array.bit_offset - to_array->array.bit_offset;
      break;
    case mkcl_aet_fixnum:
    case mkcl_aet_word:
      offset = a->array.self.word - self.word;
      break;
    case mkcl_aet_index:
      offset = a->array.self.word - self.word;
      break;
    case mkcl_aet_sf:
      offset = a->array.self.sf - self.sf;
      break;
    case mkcl_aet_df:
      offset = a->array.self.df - self.df;
      break;
    case mkcl_aet_b8:
    case mkcl_aet_i8:
      offset = a->array.self.b8 - self.b8;
      break;
    case mkcl_aet_b16:
    case mkcl_aet_i16:
      offset = a->array.self.b16 - self.b16;
      break;
    case mkcl_aet_b32:
    case mkcl_aet_i32:
      offset = a->array.self.b32 - self.b32;
      break;
    case mkcl_aet_b64:
    case mkcl_aet_i64:
      offset = a->array.self.b64 - self.b64;
      break;
    default:
      mkcl_FEbad_aet(env);
    }
  }
  @(return to_array MKCL_MAKE_FIXNUM(offset));
}

mkcl_object
mk_cl_array_has_fill_pointer_p(MKCL, mkcl_object a)
{
  mkcl_object r;

  mkcl_call_stack_check(env);
 AGAIN:
  switch (mkcl_type_of(a))
    {
    case mkcl_t_array:
      r = mk_cl_Cnil;
      break;
    case mkcl_t_vector:
    case mkcl_t_bitvector:
      r = a->vector.hasfillp ? mk_cl_Ct : mk_cl_Cnil;
      break;
    case mkcl_t_string:
      r = a->string.hasfillp ? mk_cl_Ct : mk_cl_Cnil;
      break;
    case mkcl_t_base_string:
      r = a->base_string.hasfillp ? mk_cl_Ct : mk_cl_Cnil;
      break;
    default:
      a = mkcl_type_error(env, @'array-has-fill-pointer-p',"argument", a, @'array');
      goto AGAIN;
    }
  @(return r);
}

mkcl_object
mk_cl_fill_pointer(MKCL, mkcl_object a)
{
  mkcl_call_stack_check(env);
  @(return MKCL_MAKE_FIXNUM(mkcl_vector_fill_pointer(env, a)));
}

/*
	Internal function for setting fill pointer.
*/

mkcl_index mkcl_ensure_index_for_fill_pointer_set(MKCL, mkcl_index fillp, mkcl_index dim)
{
  return mkcl_fixnum_in_range(env, @'si::fill-pointer-set', "fill pointer", MKCL_MAKE_FIXNUM(fillp), 0, dim);
}

mkcl_object
mk_si_fill_pointer_set(MKCL, mkcl_object a, mkcl_object fp)
{
  mkcl_index fillp;

  mkcl_call_stack_check(env);
  if (mkcl_likely(MKCL_FIXNUMP(fp)))
    fillp = mkcl_fixnum_to_word(fp);
  else
    fillp = mkcl_fixnum_in_range(env, @'si::fill-pointer-set', "fill pointer", fp, 0, MKCL_MOST_POSITIVE_FIXNUM);
  @(return MKCL_MAKE_FIXNUM(mkcl_vector_fill_pointer_set(env, a, fillp)));
}

/*
	Internal function for replacing the contents of arrays:

		(si:replace-array old-array new-array).

	Used in ADJUST-ARRAY.
*/
mkcl_object
mk_si_replace_array(MKCL, mkcl_object olda, mkcl_object newa)
{
  mkcl_object dlist;

  mkcl_call_stack_check(env);
  if (mkcl_type_of(olda) != mkcl_type_of(newa)
      || (mkcl_type_of(olda) == mkcl_t_array && olda->array.rank != newa->array.rank))
    goto CANNOT;
  if (!olda->array.adjustable) {
    /* When an array is not adjustable, we simply output the new array */
    olda = newa;
    goto OUTPUT;
  }
  for (dlist = MKCL_CDR(olda->array.displaced); dlist != mk_cl_Cnil; dlist = MKCL_CDR(dlist)) {
    mkcl_object other_array = MKCL_CAR(dlist);
    mkcl_object offset;
    mk_cl_array_displacement(env, other_array);
    offset = MKCL_VALUES(1);
    displace(env, other_array, newa, offset);
  }
  switch (mkcl_type_of(olda)) {
  case mkcl_t_array:
    olda->array = newa->array;
    break;
  case mkcl_t_vector:
  case mkcl_t_bitvector:
    olda->vector = newa->vector;
    break;
  case mkcl_t_string:
    olda->string = newa->string;
    break;
  case mkcl_t_base_string:
    olda->base_string = newa->base_string;
    break;
  default:
  CANNOT:
    mkcl_FEerror(env, "Cannot replace the array ~S by the array ~S.", 2, olda, newa);
  }
 OUTPUT:
  @(return olda);
}

void
mkcl_copy_subarray(MKCL, mkcl_object dest, mkcl_index i0, mkcl_object orig,
		   mkcl_index i1, mkcl_index l)
{
  mkcl_elttype t = mkcl_array_elttype(env, dest);
  if (i0 + l > dest->array.dim) {
    l = dest->array.dim - i0;
  }
  if (i1 + l > orig->array.dim) {
    l = orig->array.dim - i1;
  }
  if (t != mkcl_array_elttype(env, orig) || t == mkcl_aet_bit) {
    while (l--) {
      mkcl_aset_index(env, dest, i0++, mkcl_aref_index(env, orig, i1++));
    }
  } else if (t >= 0 && t <= mkcl_aet_last_type) {
    mkcl_index elt_size = mkcl_aet_size[t];
    memcpy(dest->array.self.bc + i0 * elt_size,
	   orig->array.self.bc + i1 * elt_size,
	   l * elt_size);
  } else {
    mkcl_FEbad_aet(env);
  }
}

void
mkcl_reverse_subarray(MKCL, mkcl_object x, mkcl_index i0, mkcl_index i1)
{
  mkcl_elttype t = mkcl_array_elttype(env, x);
  mkcl_index i, j;
  if (x->array.dim == 0) {
    return;
  }
  if (i1 >= x->array.dim) {
    i1 = x->array.dim;
  }
  switch (t) {
  case mkcl_aet_object:
  case mkcl_aet_fixnum:
  case mkcl_aet_word:
  case mkcl_aet_index:
    for (i = i0, j = i1-1;  i < j;  i++, --j) {
      mkcl_object y = x->vector.self.t[i];
      x->vector.self.t[i] = x->vector.self.t[j];
      x->vector.self.t[j] = y;
    }
    break;
  case mkcl_aet_sf:
    for (i = i0, j = i1-1;  i < j;  i++, --j) {
      float y = x->array.self.sf[i];
      x->array.self.sf[i] = x->array.self.sf[j];
      x->array.self.sf[j] = y;
    }
    break;
  case mkcl_aet_df:
    for (i = i0, j = i1-1;  i < j;  i++, --j) {
      double y = x->array.self.df[i];
      x->array.self.df[i] = x->array.self.df[j];
      x->array.self.df[j] = y;
    }
    break;
  case mkcl_aet_bc:
    for (i = i0, j = i1-1;  i < j;  i++, --j) {
      mkcl_base_char y = x->array.self.bc[i];
      x->array.self.bc[i] = x->array.self.bc[j];
      x->array.self.bc[j] = y;
    }
    break;
  case mkcl_aet_b8:
  case mkcl_aet_i8:
    for (i = i0, j = i1-1;  i < j;  i++, --j) {
      mkcl_uint8_t y = x->array.self.b8[i];
      x->array.self.b8[i] = x->array.self.b8[j];
      x->array.self.b8[j] = y;
    }
    break;
  case mkcl_aet_b16:
  case mkcl_aet_i16:
    for (i = i0, j = i1-1;  i < j;  i++, --j) {
      mkcl_uint16_t y = x->array.self.b16[i];
      x->array.self.b16[i] = x->array.self.b16[j];
      x->array.self.b16[j] = y;
    }
    break;
  case mkcl_aet_b32:
  case mkcl_aet_i32:
    for (i = i0, j = i1-1;  i < j;  i++, --j) {
      mkcl_uint32_t y = x->array.self.b32[i];
      x->array.self.b32[i] = x->array.self.b32[j];
      x->array.self.b32[j] = y;
    }
    break;
  case mkcl_aet_b64:
  case mkcl_aet_i64:
    for (i = i0, j = i1-1;  i < j;  i++, --j) {
      mkcl_uint64_t y = x->array.self.b64[i];
      x->array.self.b64[i] = x->array.self.b64[j];
      x->array.self.b64[j] = y;
    }
    break;
  case mkcl_aet_ch:
    for (i = i0, j = i1-1;  i < j;  i++, --j) {
      mkcl_character y = x->array.self.c[i];
      x->array.self.c[i] = x->array.self.c[j];
      x->array.self.c[j] = y;
    }
    break;
  case mkcl_aet_bit:
    for (i = i0 + x->vector.bit_offset,
	   j = i1 + x->vector.bit_offset - 1;
	 i < j;
	 i++, --j) {
      int k = mkcl_bit_bundle(x->array.self.bit, i) & mkcl_bundle_bit_mask(i);

      if (mkcl_bit_bundle(x->array.self.bit, j) & mkcl_bundle_bit_mask(j))
	mkcl_bit_bundle(x->array.self.bit, i) |= mkcl_bundle_bit_mask(i);
      else
	mkcl_bit_bundle(x->array.self.bit, i) &= ~(mkcl_bundle_bit_mask(i));
      if (k)
	mkcl_bit_bundle(x->array.self.bit, j) |= mkcl_bundle_bit_mask(j);
      else
	mkcl_bit_bundle(x->array.self.bit, j) &= ~(mkcl_bundle_bit_mask(j));
    }
    break;
  default:
    mkcl_FEbad_aet(env);
  }
}

mkcl_object
mk_si_fill_array_with_elt(MKCL, mkcl_object x, mkcl_object elt, mkcl_object start, mkcl_object end)
{
  mkcl_call_stack_check(env);
  mkcl_elttype t = mkcl_array_elttype(env, x);
  mkcl_index first = mkcl_integer_to_index(env, start);
  mkcl_index last = mkcl_Null(end) ? x->array.dim : mkcl_integer_to_index(env, end);
  if (first >= last) {
    goto END;
  }
  switch (t)
    {
    case mkcl_aet_object:
      {
	mkcl_object *p = x->vector.self.t + first;
	for (first = last - first; first; --first, ++p) { *p = elt; }
      }
      break;
    case mkcl_aet_bc:
      {
	mkcl_base_char e = mkcl_base_char_code(env, elt);
	mkcl_base_char *p = x->vector.self.bc + first;
	for (first = last - first; first; --first, ++p) { *p = e; }
      }
      break;
    case mkcl_aet_ch:
      {
	mkcl_character e = mkcl_char_code(env, elt);
	mkcl_character *p = x->vector.self.c + first;
	for (first = last - first; first; --first, ++p) { *p = e; }
      }
      break;
    case mkcl_aet_fixnum:
    case mkcl_aet_word:
      {
	mkcl_word e = mkcl_integer_to_word(env, elt);
	mkcl_word *p = x->vector.self.word + first;
	for (first = last - first; first; --first, ++p) { *p = e; }
      }
      break;
    case mkcl_aet_index:
      {
	mkcl_index e = mkcl_integer_to_index(env, elt);
	mkcl_index *p = x->vector.self.index + first;
	for (first = last - first; first; --first, ++p) { *p = e; }
      }
      break;
    case mkcl_aet_sf:
      {
	float e = mkcl_to_float(env, elt);
	float *p = x->vector.self.sf + first;
	for (first = last - first; first; --first, ++p) { *p = e; }
      }
      break;
    case mkcl_aet_df:
      {
      double e = mkcl_to_double(env, elt);
      double *p = x->vector.self.df + first;
      for (first = last - first; first; --first, ++p) { *p = e; }
      }
      break;
    case mkcl_aet_b8:
      {
	uint8_t e = mkcl_to_uint8_t(env, elt);
	uint8_t *p = x->vector.self.b8 + first;
	for (first = last - first; first; --first, ++p) { *p = e; }
      }
      break;
    case mkcl_aet_i8:
      {
	int8_t e = mkcl_to_int8_t(env, elt);
	int8_t *p = x->vector.self.i8 + first;
	for (first = last - first; first; --first, ++p) { *p = e; }
      }
      break;
    case mkcl_aet_b16:
      {
	mkcl_uint16_t e = mkcl_to_uint16_t(env, elt);
	mkcl_uint16_t *p = x->vector.self.b16 + first;
	for (first = last - first; first; --first, ++p) { *p = e; }
      }
      break;
    case mkcl_aet_i16:
      {
	mkcl_int16_t e = mkcl_to_int16_t(env, elt);
	mkcl_int16_t *p = x->vector.self.i16 + first;
	for (first = last - first; first; --first, ++p) { *p = e; }
      }
      break;
    case mkcl_aet_b32:
      {
	mkcl_uint32_t e = mkcl_to_uint32_t(env, elt);
	mkcl_uint32_t *p = x->vector.self.b32 + first;
	for (first = last - first; first; --first, ++p) { *p = e; }
      }
      break;
    case mkcl_aet_i32:
      {
	mkcl_int32_t e = mkcl_to_int32_t(env, elt);
	mkcl_int32_t *p = x->vector.self.i32 + first;
	for (first = last - first; first; --first, ++p) { *p = e; }
      }
      break;
    case mkcl_aet_b64:
      {
	mkcl_uint64_t e = mkcl_to_uint64_t(env, elt);
	mkcl_uint64_t *p = x->vector.self.b64 + first;
	for (first = last - first; first; --first, ++p) { *p = e; }
      }
      break;
    case mkcl_aet_i64:
      {
	mkcl_int64_t e = mkcl_to_int64_t(env, elt);
	mkcl_int64_t *p = x->vector.self.i64 + first;
	for (first = last - first; first; --first, ++p) { *p = e; }
      }
      break;
    case mkcl_aet_bit:
      {
	int i = mkcl_fixnum_in_range(env, @'si::aset',"bit",elt,0,1);
	for (last -= first, first += x->vector.bit_offset; last; --last, ++first) {
	  int mask = mkcl_bundle_bit_mask(first);
	  if (i == 0)
	    mkcl_bit_bundle(x->vector.self.bit, first) &= ~mask;
	  else
	    mkcl_bit_bundle(x->vector.self.bit, first) |= mask;
	}
      }
      break;
    default:
      mkcl_FEbad_aet(env);
    }
 END:
  @(return x);
}

