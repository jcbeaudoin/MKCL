/* -*- mode: c -*- */
/*
    sequence.d -- Sequence routines.
*/
/*
    Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
    Copyright (c) 1990, Giuseppe Attardi.
    Copyright (c) 2001, Juan Jose Garcia Ripoll.
    Copyright (c) 2011-2012, Jean-Claude Beaudoin.

    MKCL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 3 of the License, or (at your option) any later version.

    See file '../../Copyright' for full details.
*/

#include <mkcl/mkcl.h>
#include <mkcl/internal.h>
#include <mkcl/mkcl-inl.h>


mkcl_object
mk_cl_elt(MKCL, mkcl_object x, mkcl_object i)
{
  mkcl_call_stack_check(env);
  mkcl_return_value(mkcl_elt(env, x, mkcl_integer_to_word(env, i)));
}

mkcl_object
mkcl_elt(MKCL, mkcl_object seq, mkcl_word index)
{
  mkcl_word i;
  mkcl_object l;

  if (index < 0)
    goto E;
  if (mkcl_Null(seq)) goto E;
  switch (mkcl_type_of(seq))
    {
    case mkcl_t_cons:
      for (i = index, l = seq;  i > 0;  --i) {
	if (!MKCL_LISTP(l)) goto E0;
	if (mkcl_Null(l)) goto E;
	l = MKCL_CONS_CDR(l);
      }
      if (!MKCL_LISTP(l)) goto E0;
      if (mkcl_Null(l)) goto E;
      return MKCL_CONS_CAR(l);

    case mkcl_t_string:
      if (index >= seq->string.fillp)
	goto E;
      return(MKCL_CODE_CHAR(seq->string.self[index]));

    case mkcl_t_vector:
    case mkcl_t_bitvector:
      if (index >= seq->vector.fillp)
	goto E;
      return(mkcl_aref_index(env, seq, index));

    case mkcl_t_base_string:
      if (index >= seq->base_string.fillp)
	goto E;
      return(MKCL_CODE_CHAR(seq->base_string.self[index]));

    default:
    E0:
      mkcl_FEtype_error_sequence(env, seq);
    }
 E:
  mkcl_FEtype_error_seq_index(env, seq, MKCL_MAKE_FIXNUM(index));
}

mkcl_object
mk_si_elt_set(MKCL, mkcl_object seq, mkcl_object index, mkcl_object val)
{
  mkcl_call_stack_check(env);
  mkcl_return_value(mkcl_elt_set(env, seq, mkcl_integer_to_word(env, index), val));
}

mkcl_object
mkcl_elt_set(MKCL, mkcl_object seq, mkcl_word index, mkcl_object val)
{
  mkcl_word i;
  mkcl_object l;

  if (index < 0)
    goto E;
  if (mkcl_Null(seq)) goto E;
  switch (mkcl_type_of(seq))
    {
    case mkcl_t_cons:
      for (i = index, l = seq;  i > 0;  --i) {
	if (!MKCL_LISTP(l)) goto E0;
	if (mkcl_Null(l)) goto E;
	l = MKCL_CONS_CDR(l);
      }
      if (!MKCL_LISTP(l)) goto E0;
      if (mkcl_Null(l)) goto E;
      MKCL_RPLACA(l, val);
      return val;

    case mkcl_t_string:
      if (index >= seq->string.fillp)
	goto E;
      /* INV: mkcl_char_code() checks the type of `val' */
      seq->string.self[index] = mkcl_char_code(env, val);
      return(val);

    case mkcl_t_vector:
    case mkcl_t_bitvector:
      if (index >= seq->vector.fillp)
	goto E;
      return(mkcl_aset_index(env, seq, index, val));

    case mkcl_t_base_string:
      if (index >= seq->base_string.fillp)
	goto E;
      /* INV: mkcl_char_code() checks the type of `val' */
      seq->base_string.self[index] = mkcl_base_char_code(env, val);
      return(val);

    default:
    E0:
      mkcl_FEtype_error_sequence(env, seq);
    }
 E:
  mkcl_FEtype_error_seq_index(env, seq, MKCL_MAKE_FIXNUM(index));
}

@(defun subseq (sequence start &optional end)
  mkcl_object x = mk_cl_Cnil;
	mkcl_word s, e;
	mkcl_word i;
@
  s = mkcl_integer_to_index(env, start);
  if (mkcl_Null(end))
    e = -1;
  else
    e = mkcl_integer_to_index(env, end);
  
  if (mkcl_Null(sequence)) {
    if (s > 0)
      goto ILLEGAL_START_END;
    if (e > 0)
      goto ILLEGAL_START_END;
    mkcl_return_value(mk_cl_Cnil);
  }
  switch (mkcl_type_of(sequence))
    {
    case mkcl_t_cons:
      if (e >= 0)
	if ((e -= s) < 0)
	  goto ILLEGAL_START_END;
      while (s-- > 0) {
	if (MKCL_ATOM(sequence))
	  goto ILLEGAL_START_END;
	sequence = MKCL_CDR(sequence);
      }
      if (e < 0)
	return mk_cl_copy_list(env, sequence);
      { mkcl_object *z = &x;
	for (i = 0;  i < e;  i++) {
	  if (MKCL_ATOM(sequence))
	    goto ILLEGAL_START_END;
	  z = &MKCL_CONS_CDR(*z = mkcl_list1(env, MKCL_CAR(sequence)));
	  sequence = MKCL_CDR(sequence);
	}
      }
      mkcl_return_value(x);

    case mkcl_t_string:
      if (s > sequence->string.fillp)
	goto ILLEGAL_START_END;
      if (e < 0)
	e = sequence->string.fillp;
      else if (e < s || e > sequence->string.fillp)
	goto ILLEGAL_START_END;
      x = mkcl_alloc_simple_vector(env, e - s, mkcl_array_elttype(env, sequence));
      mkcl_copy_subarray(env, x, 0, sequence, s, e-s);
      mkcl_return_value(x);

    case mkcl_t_vector:
    case mkcl_t_bitvector:
      if (s > sequence->vector.fillp)
	goto ILLEGAL_START_END;
      if (e < 0)
	e = sequence->vector.fillp;
      else if (e < s || e > sequence->vector.fillp)
	goto ILLEGAL_START_END;
      x = mkcl_alloc_simple_vector(env, e - s, mkcl_array_elttype(env, sequence));
      mkcl_copy_subarray(env, x, 0, sequence, s, e-s);
      mkcl_return_value(x);
    case mkcl_t_base_string:
      if (s > sequence->base_string.fillp)
	goto ILLEGAL_START_END;
      if (e < 0)
	e = sequence->base_string.fillp;
      else if (e < s || e > sequence->base_string.fillp)
	goto ILLEGAL_START_END;
      x = mkcl_alloc_simple_vector(env, e - s, mkcl_array_elttype(env, sequence));
      mkcl_copy_subarray(env, x, 0, sequence, s, e-s);
      mkcl_return_value(x);
      
    default:
      mkcl_FEtype_error_sequence(env, sequence);
    }

 ILLEGAL_START_END:
  mkcl_FEerror(env,
	       "~S and ~S are illegal as :START and :END~%"
	       "for the sequence ~S.", 3, start, end, sequence);
@)

mkcl_object
mk_cl_copy_seq(MKCL, mkcl_object x)
{
  mkcl_call_stack_check(env);
  return @subseq(env, 2, x, MKCL_MAKE_FIXNUM(0));
}

mkcl_object
mk_cl_length(MKCL, mkcl_object x)
{
  mkcl_call_stack_check(env);
  mkcl_return_value(mkcl_make_unsigned_integer(env, mkcl_length(env, x)));
}

mkcl_index
mkcl_length(MKCL, mkcl_object x)
{
  if ( mkcl_Null(x) ) return(0);
  else
    switch (mkcl_type_of(x))
      {
      case mkcl_t_cons: {
	register mkcl_index i = 0; /* INV: A list's length always fits in a fixnum */

#if 0 
	mkcl_loop_for_in(env, x) { /* This is the unsafe version (on circular list). JCB */
	  i++;
	} mkcl_end_loop_for_in;
#else
        {
          register mkcl_object fast, slow;
          
          fast = slow = x;
          for (i = 0; !mkcl_Null(fast); i++, fast = MKCL_CONS_CDR(fast)) {
            if (!MKCL_CONSP(fast)) {
              mkcl_FEtype_error_proper_sequence(env, x);
            }
            if (slow == fast && i != 0)
              mkcl_FEtype_error_proper_sequence(env, x); /* Circular list! */
            if (i & 1) { /* move only on odd beat. */
              slow = MKCL_CONS_CDR(slow);
            }
          }
        }
#endif
        return(i);
      }
      case mkcl_t_string:
	return(x->string.fillp);
      case mkcl_t_base_string:
	return(x->base_string.fillp);
      case mkcl_t_vector:
      case mkcl_t_bitvector:
	return(x->vector.fillp);

      default:
	mkcl_FEtype_error_sequence(env, x);
      }
}

mkcl_index mkcl_base_string_length(MKCL, mkcl_object x)
{
  if (MKCL_BASE_STRING_P(x))
    return(x->base_string.fillp);
  else
    mkcl_FEtype_error_base_string(env, x);
}

mkcl_index mkcl_string_length(MKCL, mkcl_object x)
{
  if (MKCL_BASE_STRING_P(x))
    return(x->base_string.fillp);
  else if (MKCL_STRINGP(x))
    return(x->string.fillp);
  else
    mkcl_FEtype_error_string(env, x);
}


mkcl_object
mk_cl_reverse(MKCL, mkcl_object seq)
{
  mkcl_object output, x;

  if ( mkcl_Null(seq) ) return(mk_cl_Cnil);
  mkcl_call_stack_check(env);

  switch (mkcl_type_of(seq))
    {
    case mkcl_t_cons: {
      for (x = seq, output = mk_cl_Cnil; !mkcl_Null(x); x = MKCL_CONS_CDR(x)) {
	if (!MKCL_LISTP(x)) goto E;
	output = MKCL_CONS(env, MKCL_CONS_CAR(x), output);
      }
      break;
    }
    case mkcl_t_string:
      output = mkcl_alloc_simple_vector(env, seq->string.fillp, mkcl_array_elttype(env, seq));
      mkcl_copy_subarray(env, output, 0, seq, 0, seq->string.fillp);
      mkcl_reverse_subarray(env, output, 0, seq->string.fillp);
      break;
    case mkcl_t_base_string:
      output = mkcl_alloc_simple_vector(env, seq->base_string.fillp, mkcl_array_elttype(env, seq));
      mkcl_copy_subarray(env, output, 0, seq, 0, seq->base_string.fillp);
      mkcl_reverse_subarray(env, output, 0, seq->base_string.fillp);
      break;
    case mkcl_t_vector:
    case mkcl_t_bitvector:
      output = mkcl_alloc_simple_vector(env, seq->vector.fillp, mkcl_array_elttype(env, seq));
      mkcl_copy_subarray(env, output, 0, seq, 0, seq->vector.fillp);
      mkcl_reverse_subarray(env, output, 0, seq->vector.fillp);
      break;
    default:
    E:
      mkcl_FEtype_error_sequence(env, seq);
    }
  mkcl_return_value(output);
}

mkcl_object
mk_cl_nreverse(MKCL, mkcl_object seq)
{
  if ( mkcl_Null(seq) ) return(mk_cl_Cnil);

  mkcl_call_stack_check(env);
  switch (mkcl_type_of(seq))
    {
    case mkcl_t_cons:
      {
	mkcl_object x, y, z;
	for (x = seq, y = mk_cl_Cnil; !mkcl_Null(x); ) {
	  if (!MKCL_LISTP(x)) mkcl_FEtype_error_list(env, x);
	  z = x;
	  x = MKCL_CONS_CDR(x);
	  if (x == seq) mkcl_FEcircular_list(env, seq);
	  MKCL_RPLACD(z, y);
	  y = z;
	}
	seq = y;
      }
      break;
    case mkcl_t_string:
      mkcl_reverse_subarray(env, seq, 0, seq->string.fillp);
      break;
    case mkcl_t_base_string:
      mkcl_reverse_subarray(env, seq, 0, seq->base_string.fillp);
      break;
    case mkcl_t_vector:
    case mkcl_t_bitvector:
      mkcl_reverse_subarray(env, seq, 0, seq->vector.fillp);
      break;
    default:
      mkcl_FEtype_error_sequence(env, seq);
    }
  mkcl_return_value(seq);
}
