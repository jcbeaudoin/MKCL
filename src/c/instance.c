/* -*- mode: c -*- */
/*
    instance.c -- CLOS interface.
*/
/*
    Copyright (c) 1990, Giuseppe Attardi.
    Copyright (c) 2001, Juan Jose Garcia Ripoll.
    Copyright (c) 2011-2017,2021-2022, Jean-Claude Beaudoin.

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

struct mkcl_cfun mk_si_allocate_raw_instance_cfunobj = MKCL_CFUN3(mk_si_allocate_raw_instance, (mkcl_object) &MK_SI_allocate_raw_instance);

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
  mkcl_return_value(orig);
}

struct mkcl_cfun mk_si_instance_sig_cfunobj = MKCL_CFUN1(mk_si_instance_sig, (mkcl_object) &MK_SI_instance_sig);

mkcl_object
mk_si_instance_sig(MKCL, mkcl_object x)
{
  mkcl_call_stack_check(env);
  if (mkcl_unlikely(!MKCL_INSTANCEP(x)))
    mkcl_FEtype_error_instance(env, x);
  mkcl_return_value(x->instance.sig);
}

struct mkcl_cfun mk_si_instance_sig_set_cfunobj = MKCL_CFUN1(mk_si_instance_sig_set, (mkcl_object) &MK_SI_instance_sig_set);

mkcl_object
mk_si_instance_sig_set(MKCL, mkcl_object x)
{
  mkcl_call_stack_check(env);
  if (mkcl_unlikely(!MKCL_INSTANCEP(x)))
    mkcl_FEtype_error_instance(env, x);
  mkcl_return_value((x->instance.sig = MKCL_CLASS_SLOTS(MKCL_CLASS_OF(x))));
}

struct mkcl_cfun mk_si_instance_sig_set2_cfunobj = MKCL_CFUN2(mk_si_instance_sig_set2, (mkcl_object) &MK_SI_instance_sig_set2);

mkcl_object
mk_si_instance_sig_set2(MKCL, mkcl_object x, mkcl_object sig)
{
  mkcl_call_stack_check(env);
  if (mkcl_unlikely(!MKCL_INSTANCEP(x)))
    mkcl_FEtype_error_instance(env, x);
  mkcl_return_value((x->instance.sig = sig));
}

struct mkcl_cfun mk_si_instance_class_cfunobj = MKCL_CFUN1(mk_si_instance_class, (mkcl_object) &MK_SI_instance_class);

mkcl_object
mk_si_instance_class(MKCL, mkcl_object x)
{
  mkcl_call_stack_check(env);
  if (!MKCL_INSTANCEP(x))
    mkcl_FEtype_error_instance(env, x);
  mkcl_return_value(MKCL_CLASS_OF(x));
}

struct mkcl_cfun mk_si_instance_length_cfunobj = MKCL_CFUN1(mk_si_instance_length, (mkcl_object) &MK_SI_instance_length);

mkcl_object
mk_si_instance_length(MKCL, mkcl_object x)
{
  mkcl_call_stack_check(env);
  if (!MKCL_INSTANCEP(x))
    mkcl_FEtype_error_instance(env, x);
  else
    { mkcl_return_value(mkcl_make_unsigned_integer(env, x->instance.length)); }
}

struct mkcl_cfun mk_si_instance_class_set_cfunobj = MKCL_CFUN2(mk_si_instance_class_set, (mkcl_object) &MK_SI_instance_class_set);

mkcl_object
mk_si_instance_class_set(MKCL, mkcl_object x, mkcl_object y)
{
  mkcl_call_stack_check(env);
  if (!MKCL_INSTANCEP(x))
    mkcl_FEtype_error_instance(env, x);
  if (!MKCL_INSTANCEP(y))
    mkcl_FEtype_error_instance(env, y);
  MKCL_CLASS_OF(x) = y;
  mkcl_return_value(x);
}

void
mkcl_FEtype_error_instance_index(MKCL, mkcl_object instance, mkcl_object ndx)
{
  mk_cl_error(env, 5, (mkcl_object) &MK_MKCL_invalid_slot, (mkcl_object) &MK_KEY_name, ndx, (mkcl_object) &MK_KEY_instance, instance);
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

struct mkcl_cfun mk_si_instance_ref_cfunobj = MKCL_CFUN2(mk_si_instance_ref, (mkcl_object) &MK_SI_instance_ref);
struct mkcl_cfun mk_clos_funcallable_standard_instance_access_cfunobj = MKCL_CFUN2(mk_si_instance_ref, (mkcl_object) &MK_CLOS_funcallable_standard_instance_access);

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

  { mkcl_return_value(x->instance.slots[i]); }
}

struct mkcl_cfun mk_si_instance_ref_safe_cfunobj = MKCL_CFUN2(mk_si_instance_ref_safe, (mkcl_object) &MK_SI_instance_ref_safe);

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
    mk_cl_error(env, 5, (mkcl_object) &MK_CL_unbound_slot, (mkcl_object) &MK_KEY_name, index, (mkcl_object) &MK_KEY_instance, x);
  mkcl_return_value(x);
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

struct mkcl_cfun mk_si_instance_set_cfunobj = MKCL_CFUN3(mk_si_instance_set, (mkcl_object) &MK_SI_instance_set);

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

  { x->instance.slots[i] = value; mkcl_return_value(value); }
}

struct mkcl_cfun mk_si_instancep_cfunobj = MKCL_CFUN1(mk_si_instancep, (mkcl_object) &MK_SI_instancep);

mkcl_object
mk_si_instancep(MKCL, mkcl_object x)
{
  mkcl_return_value((MKCL_INSTANCEP(x) ? mk_cl_Ct : mk_cl_Cnil));
}

mkcl_object
mk_si_unbound(MKCL)
{
  /* Returns an object that cannot be read or written and which
     is used to represent an unitialized slot */
  mkcl_return_value(MKCL_UNBOUND);
}

struct mkcl_cfun mk_si_unbound_cfunobj = MKCL_CFUN0(mk_si_unbound, (mkcl_object) &MK_SI_CONSTANT_unbound);


struct mkcl_cfun mk_si_sl_boundp_cfunobj = MKCL_CFUN1(mk_si_sl_boundp, (mkcl_object) &MK_SI_sl_boundp);

mkcl_object
mk_si_sl_boundp(MKCL, mkcl_object x)
{
  mkcl_return_value(((x == MKCL_UNBOUND) ? mk_cl_Cnil : mk_cl_Ct));
}

struct mkcl_cfun mk_si_sl_makunbound_cfunobj = MKCL_CFUN2(mk_si_sl_makunbound, (mkcl_object) &MK_SI_sl_makunbound);

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
  mkcl_return_value(x);
}

struct mkcl_cfun mk_si_copy_instance_cfunobj = MKCL_CFUN1(mk_si_copy_instance, (mkcl_object) &MK_SI_copy_instance);

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
  mkcl_return_value(y);
}

struct mkcl_cfun mk_cl_find_class_cfunobj = MKCL_CFUN_VA(mk_cl_find_class, (mkcl_object) &MK_CL_find_class);

mkcl_object mk_cl_find_class(MKCL, mkcl_narg narg, mkcl_object name, ...)
{
  mkcl_call_stack_check(env);
  {
    mkcl_object class, hash;
    mkcl_object errorp = mk_cl_Ct;
    mkcl_object lex_env = mk_cl_Cnil;
    MKCL_RECEIVE_2_OPTIONAL_ARGUMENTS(env, (mkcl_object) &MK_CL_find_class, narg, 1, name, &errorp, &lex_env);

    do {
      if (mkcl_Null(name)) {
        if (mkcl_Null(errorp))
          { mkcl_return_value(mk_cl_Cnil); }
        else
          mkcl_FEerror(env, "No class named ~S.", 1, name);
      }
      else if (mkcl_type_of(name) == mkcl_t_symbol)
        {
          class = name->symbol.properly_named_class;
          if (mkcl_Null(class))
            {
              if (mkcl_Null(errorp))
                { mkcl_return_value(mk_cl_Cnil); }
              else
                mkcl_FEerror(env, "No class named ~S.", 1, name);
            }
          else
            { mkcl_return_value(class); }
        }
      else
        name = mkcl_type_error(env, (mkcl_object) &MK_CL_find_class, "symbol", name, (mkcl_object) &MK_CL_symbol);
    } while(1);
  }
}

struct mkcl_cfun mk_si_set_class_proper_name_cfunobj = MKCL_CFUN2(mk_si_set_class_proper_name, (mkcl_object) &MK_SI_set_class_proper_name);

mkcl_object mk_si_set_class_proper_name(MKCL, mkcl_object sym, mkcl_object class)
{
  mkcl_call_stack_check(env);
  if (mkcl_Null(class) || class == ((mkcl_object) &mk_cl_Cnil_symbol))
    class = mk_cl_Cnil;
  else if (!MKCL_INSTANCEP(class))
    mkcl_FEtype_error_instance(env, class);

  if (mkcl_Null(sym) || (mkcl_type_of(sym) != mkcl_t_symbol))
    mkcl_FEwrong_type_argument(env, (mkcl_object) &MK_CL_symbol, sym);

  sym->symbol.properly_named_class = class;
  mkcl_return_value(class);
}

mkcl_object
mkcl_slot_value(MKCL, mkcl_object x, const char *slot)
{
  mkcl_object slot_name = mkcl_fast_read_from_cstring(env, (char *) slot);
  return mkcl_funcall2(env, MK_CL_slot_value.gfdef, x, slot_name);
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

struct mkcl_cfun mk_cl_class_of_cfunobj = MKCL_CFUN1(mk_cl_class_of, (mkcl_object) &MK_CL_class_of);

mkcl_object
mk_cl_class_of(MKCL, mkcl_object x)
{
  size_t index;
  mkcl_type tp = mkcl_type_of(x);

  mkcl_call_stack_check(env);
  if (tp == mkcl_t_instance)
    mkcl_return_value(MKCL_CLASS_OF(x));
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
    if ( x == ((mkcl_object) &mk_cl_Cnil_symbol) )
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
    x = MKCL_SYM_VAL(env, (mkcl_object) &MK_CLOS_DYNVAR_builtin_classes);
    /* We have to be careful because *builtin-classes* might be empty! */
    if (mkcl_Null(x)) {
      output = mk_cl_find_class(env, 1,mk_cl_Ct);
    } else {
      output = mkcl_aref_index(env, x, index);
    }
    mkcl_return_value(output);
  }
}

