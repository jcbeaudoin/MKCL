/* -*- mode: c -*- */
/*
    instance.c -- CLOS interface.
*/
/*
    Copyright (c) 1990, Giuseppe Attardi.
    Copyright (c) 2001, Juan Jose Garcia Ripoll.
    Copyright (c) 2011-2017, Jean-Claude Beaudoin.

    MKCL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 3 of the License, or (at your option) any later version.

    See file '../../Copyright' for full details.
*/

#include <mkcl/mkcl.h>
#include <mkcl/internal.h>
#include <string.h>

mkcl_object
mkcl_allocate_instance(MKCL, mkcl_object clas, mkcl_index size)
{
  mkcl_object x = mkcl_alloc_raw_instance(env, size);
  mkcl_index i;
  MKCL_CLASS_OF(x) = clas;
  for (i = 0;  i < size;  i++)
    x->instance.slots[i] = MKCL_UNBOUND;
  return x;
}

mkcl_object
mk_si_allocate_raw_instance(MKCL, mkcl_object orig, mkcl_object clas, mkcl_object size)
{
  mkcl_call_stack_check(env);
  mkcl_object output = mkcl_allocate_instance(env, clas, mkcl_integer_to_index(env, size));

  if (mkcl_unlikely(!mkcl_Null(clas) && !(MKCL_SYMBOLP(clas) || MKCL_INSTANCEP(clas))))
    /* We have to accept symbols and NIL as valid values of clas because of
       some dubious hacks used during the CLOS bootstrap process. JCB */
    /* Should we check further that the instance is really a class? JCB */
    mkcl_FEtype_error_instance(env, clas);
  if (orig == mk_cl_Cnil) {
    orig = output;
  } else {
    if (mkcl_unlikely(!MKCL_INSTANCEP(orig)))
      mkcl_FEtype_error_instance(env, orig);
    orig->instance.clas = clas;
    orig->instance.length = output->instance.length;
    orig->instance.slots = output->instance.slots;
  }
  @(return orig);
}

mkcl_object
mk_si_instance_sig(MKCL, mkcl_object x)
{
  mkcl_call_stack_check(env);
  if (mkcl_unlikely(!MKCL_INSTANCEP(x)))
    mkcl_FEtype_error_instance(env, x);
  @(return x->instance.sig);
}

mkcl_object
mk_si_instance_sig_set(MKCL, mkcl_object x)
{
  mkcl_call_stack_check(env);
  if (mkcl_unlikely(!MKCL_INSTANCEP(x)))
    mkcl_FEtype_error_instance(env, x);
  @(return (x->instance.sig = MKCL_CLASS_SLOTS(MKCL_CLASS_OF(x))));
}

mkcl_object
mk_si_instance_sig_set2(MKCL, mkcl_object x, mkcl_object sig)
{
  mkcl_call_stack_check(env);
  if (mkcl_unlikely(!MKCL_INSTANCEP(x)))
    mkcl_FEtype_error_instance(env, x);
  @(return (x->instance.sig = sig));
}

mkcl_object
mk_si_instance_class(MKCL, mkcl_object x)
{
  mkcl_call_stack_check(env);
  if (!MKCL_INSTANCEP(x))
    mkcl_FEtype_error_instance(env, x);
  @(return MKCL_CLASS_OF(x));
}

mkcl_object
mk_si_instance_length(MKCL, mkcl_object x)
{
  mkcl_call_stack_check(env);
  if (!MKCL_INSTANCEP(x))
    mkcl_FEtype_error_instance(env, x);
  else
    { @(return mkcl_make_unsigned_integer(env, x->instance.length)); }
}

mkcl_object
mk_si_instance_class_set(MKCL, mkcl_object x, mkcl_object y)
{
  mkcl_call_stack_check(env);
  if (!MKCL_INSTANCEP(x))
    mkcl_FEtype_error_instance(env, x);
  if (!MKCL_INSTANCEP(y))
    mkcl_FEtype_error_instance(env, y);
  MKCL_CLASS_OF(x) = y;
  @(return x);
}

void
mkcl_FEtype_error_instance_index(MKCL, mkcl_object instance, mkcl_object ndx)
{
  mk_cl_error(env, 5, @'mkcl::invalid-slot', @':name', ndx, @':instance', instance);
}


#if 0 /* inlined */
mkcl_object
mkcl_instance_ref(MKCL, mkcl_object x, mkcl_word i)
{
  if (mkcl_unlikely(!MKCL_INSTANCEP(x)))
    mkcl_FEtype_error_instance(env, x);
  else if (mkcl_unlikely(i < 0 || i >= x->instance.length))
    mkcl_FEtype_error_instance_index(env, x, MKCL_MAKE_FIXNUM(i));

  return(x->instance.slots[i]);
}
#endif

mkcl_object
mk_si_instance_ref(MKCL, mkcl_object x, mkcl_object index)
{
  mkcl_word i;
  
  mkcl_call_stack_check(env);
  if (mkcl_unlikely(!MKCL_INSTANCEP(x)))
    mkcl_FEtype_error_instance(env, x);
  else if (mkcl_unlikely(!MKCL_FIXNUMP(index) ||
			 (i = mkcl_fixnum_to_word(index)) < 0 || i >= x->instance.length))
    mkcl_FEtype_error_instance_index(env, x, index);

  { @(return x->instance.slots[i]); }
}

mkcl_object
mk_si_instance_ref_safe(MKCL, mkcl_object x, mkcl_object index)
{
  mkcl_word i;

  mkcl_call_stack_check(env);
  if (mkcl_unlikely(!MKCL_INSTANCEP(x)))
    mkcl_FEtype_error_instance(env, x);
  if (mkcl_unlikely(!MKCL_FIXNUMP(index) ||
		    (i = mkcl_fixnum_to_word(index)) < 0 || i >= x->instance.length))
    mkcl_FEtype_error_instance_index(env, x, index);
  x = x->instance.slots[i];
  if (mkcl_unlikely(x == MKCL_UNBOUND))
    mk_cl_error(env, 5, @'unbound-slot', @':name', index, @':instance', x);
  @(return x);
}

#if 0 /* inlined */
mkcl_object
mkcl_instance_set(MKCL, mkcl_object x, mkcl_word i, mkcl_object v)
{
  if (mkcl_unlikely(!MKCL_INSTANCEP(x)))
    mkcl_FEtype_error_instance(env, x);
  else if (mkcl_unlikely(i < 0 || i >= x->instance.length))
    mkcl_FEtype_error_instance_index(env, x, MKCL_MAKE_FIXNUM(i));

  { x->instance.slots[i] = v; return(v); }
}
#endif

mkcl_object
mk_si_instance_set(MKCL, mkcl_object x, mkcl_object index, mkcl_object value)
{
  mkcl_word i;

  mkcl_call_stack_check(env);
  if (mkcl_unlikely(!MKCL_INSTANCEP(x)))
    mkcl_FEtype_error_instance(env, x);
  else if (mkcl_unlikely(!MKCL_FIXNUMP(index)
			 || (i = mkcl_fixnum_to_word(index)) >= (mkcl_word)x->instance.length
			 || i < 0))
    mkcl_FEtype_error_instance_index(env, x, index);

  { x->instance.slots[i] = value; @(return value); }
}

mkcl_object
mk_si_instancep(MKCL, mkcl_object x)
{
  @(return (MKCL_INSTANCEP(x) ? mk_cl_Ct : mk_cl_Cnil));
}

mkcl_object
mk_si_unbound(MKCL)
{
  /* Returns an object that cannot be read or written and which
     is used to represent an unitialized slot */
  @(return MKCL_UNBOUND);
}

mkcl_object
mk_si_sl_boundp(MKCL, mkcl_object x)
{
  @(return ((x == MKCL_UNBOUND) ? mk_cl_Cnil : mk_cl_Ct));
}

mkcl_object
mk_si_sl_makunbound(MKCL, mkcl_object x, mkcl_object index)
{
  mkcl_word i;

  mkcl_call_stack_check(env);
  if (!MKCL_INSTANCEP(x))
    mkcl_FEtype_error_instance(env, x);
  if (!MKCL_FIXNUMP(index) ||
      (i = mkcl_fixnum_to_word(index)) >= x->instance.length || i < 0)
    mkcl_FEtype_error_instance_index(env, x, index);
  x->instance.slots[i] = MKCL_UNBOUND;
  @(return x);
}

mkcl_object
mk_si_copy_instance(MKCL, mkcl_object x)
{
  mkcl_object y;

  mkcl_call_stack_check(env);
  if (!MKCL_INSTANCEP(x))
    mkcl_FEtype_error_instance(env, x);
  y = mkcl_allocate_instance(env, x->instance.clas, x->instance.length);
  y->instance.isgf = x->instance.isgf;
  y->instance.f = x->instance.f;
  y->instance.sig = x->instance.sig;
  memcpy(y->instance.slots, x->instance.slots,
	 x->instance.length * sizeof(mkcl_object));
  @(return y);
}

@(defun find-class (name &optional (errorp mk_cl_Ct) lex_env)
	mkcl_object class, hash;
@
  do {
    if (mkcl_Null(name)) {
      if (mkcl_Null(errorp))
        { @(return mk_cl_Cnil); }
      else
	mkcl_FEerror(env, "No class named ~S.", 1, name);
    }
    else if (mkcl_type_of(name) == mkcl_t_symbol)
      {
	class = name->symbol.properly_named_class;
	if (mkcl_Null(class))
	  {
	    if (mkcl_Null(errorp))
	      { @(return mk_cl_Cnil); }
	    else
	      mkcl_FEerror(env, "No class named ~S.", 1, name);
	  }
	else
	  { @(return class); }
      }
    else
      name = mkcl_type_error(env, @'find-class', "symbol", name, @'symbol');
  } while(1);
@)

mkcl_object mk_si_set_class_proper_name(MKCL, mkcl_object sym, mkcl_object class)
{
  mkcl_call_stack_check(env);
  if (mkcl_Null(class) || class == mk_cl_Cnil_symbol)
    class = mk_cl_Cnil;
  else if (!MKCL_INSTANCEP(class))
    mkcl_FEtype_error_instance(env, class);

  if (mkcl_Null(sym) || (mkcl_type_of(sym) != mkcl_t_symbol))
    mkcl_FEwrong_type_argument(env, @'symbol', sym);

  sym->symbol.properly_named_class = class;
  @(return class);
}

mkcl_object
mkcl_slot_value(MKCL, mkcl_object x, const char *slot)
{
  mkcl_object slot_name = mkcl_fast_read_from_cstring(env, (char *) slot);
  return mkcl_funcall2(env, @+'slot-value', x, slot_name);
}

mkcl_object
mkcl_slot_value_set(MKCL, mkcl_object x, const char *slot, mkcl_object value)
{
  mkcl_object slot_name = mkcl_fast_read_from_cstring(env, (char *) slot);
  mkcl_object slot_setter = mkcl_fast_read_from_cstring(env, "(SETF SLOT-VALUE)");
  return mkcl_funcall3(env, mkcl_fdefinition(env, slot_setter), value, x, slot_name);
}

/* This enum must match the definition of +builtin-classes+ in clos/builtin.lsp or else... JCB */
enum mkcl_built_in_classes {
  MKCL_BUILTIN_T = 0,
  MKCL_BUILTIN_SEQUENCE,
  MKCL_BUILTIN_LIST,
  MKCL_BUILTIN_CONS,
  MKCL_BUILTIN_ARRAY,
  MKCL_BUILTIN_VECTOR,
  MKCL_BUILTIN_STRING,
  MKCL_BUILTIN_BASE_STRING,
  MKCL_BUILTIN_BIT_VECTOR,
  MKCL_BUILTIN_STREAM,
  MKCL_BUILTIN_ANSI_STREAM,
  MKCL_BUILTIN_FILE_STREAM,
  MKCL_BUILTIN_ECHO_STREAM,
  MKCL_BUILTIN_STRING_STREAM,
  MKCL_BUILTIN_TWO_WAY_STREAM,
  MKCL_BUILTIN_SYNONYM_STREAM,
  MKCL_BUILTIN_BROADCAST_STREAM,
  MKCL_BUILTIN_CONCATENATED_STREAM,
  MKCL_BUILTIN_CHARACTER,
  MKCL_BUILTIN_NUMBER,
  MKCL_BUILTIN_REAL,
  MKCL_BUILTIN_RATIONAL,
  MKCL_BUILTIN_INTEGER,
  MKCL_BUILTIN_RATIO,
  MKCL_BUILTIN_FLOAT,
  MKCL_BUILTIN_COMPLEX,
  MKCL_BUILTIN_SYMBOL,
  MKCL_BUILTIN_NULL,
  MKCL_BUILTIN_KEYWORD,
  MKCL_BUILTIN_METHOD_COMBINATION,
  MKCL_BUILTIN_PACKAGE,
  MKCL_BUILTIN_FUNCTION,
  MKCL_BUILTIN_PATHNAME,
  MKCL_BUILTIN_LOGICAL_PATHNAME,
  MKCL_BUILTIN_HASH_TABLE,
  MKCL_BUILTIN_RANDOM_STATE,
  MKCL_BUILTIN_READTABLE,
  MKCL_BUILTIN_CODE_BLOCK,
  MKCL_BUILTIN_FOREIGN,
  MKCL_BUILTIN_TEMP_STACK_FRAME,
  MKCL_BUILTIN_COMPILED_CLOSURE_DISPLAY,
  MKCL_BUILTIN_COMPILED_CLOSURE_LEVEL,
  MKCL_BUILTIN_DEBUG_INFORMATION,
  MKCL_BUILTIN_THREAD,
  MKCL_BUILTIN_LOCK,
  MKCL_BUILTIN_RWLOCK,
  MKCL_BUILTIN_SEMAPHORE,
  MKCL_BUILTIN_CONDITION_VARIABLE,
  MKCL_BUILTIN_PROCESS,
  MKCL_BUILTIN_ENCODED_STRING,
  MKCL_BUILTIN_UTF_8,
  MKCL_BUILTIN_UTF_16
};

mkcl_object
mk_cl_class_of(MKCL, mkcl_object x)
{
  size_t index;
  mkcl_type tp = mkcl_type_of(x);

  mkcl_call_stack_check(env);
  if (tp == mkcl_t_instance)
    @(return MKCL_CLASS_OF(x));
  switch (tp) {
  case mkcl_t_fixnum:
  case mkcl_t_bignum:
    index = MKCL_BUILTIN_INTEGER; break;
  case mkcl_t_ratio:
    index = MKCL_BUILTIN_RATIO; break;
  case mkcl_t_singlefloat:
  case mkcl_t_doublefloat:
#ifdef MKCL_LONG_FLOAT
  case mkcl_t_longfloat:
#endif
    index = MKCL_BUILTIN_FLOAT; break;
    /* XXX index = MKCL_BUILTIN_long-float; break; */
  case mkcl_t_complex:
    index = MKCL_BUILTIN_COMPLEX; break;
  case mkcl_t_character:
    index = MKCL_BUILTIN_CHARACTER; break;
  case mkcl_t_symbol:
    if ( x == mk_cl_Cnil_symbol )
      index = MKCL_BUILTIN_NULL;
    else if (x->symbol.hpack == mkcl_core.keyword_package)
      index = MKCL_BUILTIN_KEYWORD;
    else
      index = MKCL_BUILTIN_SYMBOL;
    break;
  case mkcl_t_package:
    index = MKCL_BUILTIN_PACKAGE; break;
  case mkcl_t_null:
    index = MKCL_BUILTIN_NULL; break;
  case mkcl_t_cons:
    index = MKCL_BUILTIN_CONS; break;
  case mkcl_t_hashtable:
    index = MKCL_BUILTIN_HASH_TABLE; break;
  case mkcl_t_array:
    index = MKCL_BUILTIN_ARRAY; break;
  case mkcl_t_vector:
    index = MKCL_BUILTIN_VECTOR; break;
  case mkcl_t_string:
    index = MKCL_BUILTIN_STRING; break;
  case mkcl_t_base_string:
    index = MKCL_BUILTIN_BASE_STRING; break;
  case mkcl_t_bitvector:
    index = MKCL_BUILTIN_BIT_VECTOR; break;
  case mkcl_t_stream:
    switch (x->stream.mode) {
    case mkcl_smm_synonym:	index = MKCL_BUILTIN_SYNONYM_STREAM; break;
    case mkcl_smm_broadcast:	index = MKCL_BUILTIN_BROADCAST_STREAM; break;
    case mkcl_smm_concatenated:	index = MKCL_BUILTIN_CONCATENATED_STREAM; break;
    case mkcl_smm_two_way:	index =  MKCL_BUILTIN_TWO_WAY_STREAM; break;
    case mkcl_smm_string_input:
    case mkcl_smm_string_output:	index = MKCL_BUILTIN_STRING_STREAM; break;
    case mkcl_smm_echo:		index = MKCL_BUILTIN_ECHO_STREAM; break;
    default:		index = MKCL_BUILTIN_FILE_STREAM; break;
    }
    break;
  case mkcl_t_readtable:
    index = MKCL_BUILTIN_READTABLE; break;
  case mkcl_t_pathname:
    if (x->pathname.logical)
      index = MKCL_BUILTIN_LOGICAL_PATHNAME;
    else
      index = MKCL_BUILTIN_PATHNAME;
    break;
  case mkcl_t_random:
    index = MKCL_BUILTIN_RANDOM_STATE; break;
  case mkcl_t_bytecode:
  case mkcl_t_bclosure:
  case mkcl_t_cfun:
  case mkcl_t_cclosure:
    index = MKCL_BUILTIN_FUNCTION; break;
  case mkcl_t_thread:
    index = MKCL_BUILTIN_THREAD; break;
  case mkcl_t_lock:
    index = MKCL_BUILTIN_LOCK; break;
  case mkcl_t_rwlock:
    index = MKCL_BUILTIN_RWLOCK; break;
  case mkcl_t_semaphore:
    index = MKCL_BUILTIN_SEMAPHORE; break;
  case mkcl_t_condition_variable:
    index = MKCL_BUILTIN_CONDITION_VARIABLE; break;
  case mkcl_t_codeblock:
    index = MKCL_BUILTIN_CODE_BLOCK; break;
  case mkcl_t_foreign:
    index = MKCL_BUILTIN_FOREIGN; break;
  case mkcl_t_temp_stack_frame:
    index = MKCL_BUILTIN_TEMP_STACK_FRAME; break;
  case mkcl_t_cdisplay:
    index = MKCL_BUILTIN_COMPILED_CLOSURE_DISPLAY; break;
  case mkcl_t_clevel_block:
    index = MKCL_BUILTIN_COMPILED_CLOSURE_LEVEL; break;
  case mkcl_t_cmp_dbg_lex_level:
    index = MKCL_BUILTIN_DEBUG_INFORMATION; break;
  case mkcl_t_process:
    index = MKCL_BUILTIN_PROCESS; break;
  case mkcl_t_UTF_8:
    index = MKCL_BUILTIN_UTF_8; break;
  case mkcl_t_UTF_16:
    index = MKCL_BUILTIN_UTF_16; break;
  default:
    mkcl_lose(env, "not a lisp data object");
  }
  {
    mkcl_object output;
    x = MKCL_SYM_VAL(env, @'clos::*builtin-classes*');
    /* We have to be careful because *builtin-classes* might be empty! */
    if (mkcl_Null(x)) {
      output = mk_cl_find_class(env, 1,@'t');
    } else {
      output = mkcl_aref_index(env, x, index);
    }
    @(return output);
  }
}

