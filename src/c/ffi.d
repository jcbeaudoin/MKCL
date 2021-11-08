/* -*- mode: c -*- */
/*
    ffi.c -- User defined data types and foreign functions interface.
*/
/*
    Copyright (c) 2001, Juan Jose Garcia Ripoll.
    Copyright (c) 2011-2013, Jean-Claude Beaudoin.

    MKCL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 3 of the License, or (at your option) any later version.

    See file '../../Copyright' for full details.
*/

#include <mkcl/mkcl.h>
#include <mkcl/internal.h>
#include <string.h>

/* Note that mkcl_foreign_type_table[] and mkcl_foreign_type_size[]
   must have matching contents otherwise mayhem will result!
   Both must also match the order of declarations inside enum mkcl_ffi_tag!
   JCB
*/

static mkcl_object const mkcl_foreign_type_table[] = {
  @':char',
  @':unsigned-char',
  @':byte',
  @':unsigned-byte',
  @':short',
  @':unsigned-short',
  @':int',
  @':unsigned-int',
  @':long',
  @':unsigned-long',
  @':long-long',
  @':unsigned-long-long',
  @':pointer-void',
  @':cstring',
  @':object',
  @':float',
  @':double',
  @':long-double',
#if 0 /* We'll be C99 compliant one day! JCB */
  @':float-complex',
  @':double-complex',
  @':long-double-complex',
  @':float-imaginary',
  @':double-imaginary',
  @':long-double-imaginary',
#endif
  @':void'
};

static unsigned int const mkcl_foreign_type_size[] = {
  sizeof(char),
  sizeof(unsigned char),
  sizeof(mkcl_int8_t),
  sizeof(mkcl_uint8_t),
  sizeof(short),
  sizeof(unsigned short),
  sizeof(int),
  sizeof(unsigned int),
  sizeof(long),
  sizeof(unsigned long),
  sizeof(long long),
  sizeof(unsigned long long),
  sizeof(void *),
  sizeof(char *),
  sizeof(mkcl_object),
  sizeof(float),
  sizeof(double),
  sizeof(long double),
#if 0 /* We'll be C99 compliant one day! JCB */
  sizeof(float _Complex),
  sizeof(double _Complex),
  sizeof(long double _Complex),
  sizeof(float _Imaginary),
  sizeof(double _Imaginary),
  sizeof(long double _Imaginary),
#endif
  0 /* sizeof(void) */
};

#if 0
/* This array must match content of enum mkcl_ffi_calling_convention. */
static const mkcl_object mkcl_foreign_cc_table[] = {
  @':cdecl',
  @':stdcall'
};
#endif


mkcl_object
mk_si_pointer(MKCL, mkcl_object x)
{
  mkcl_call_stack_check(env);
  @(return mkcl_make_unsigned_integer(env, (mkcl_index)x));
}

mkcl_object mk_si_foreignp(MKCL, mkcl_object x)
{
  @(return (mkcl_foreignp(env, x) ? mk_cl_Ct : mk_cl_Cnil));
}

mkcl_object
mkcl_make_foreign(MKCL, mkcl_object C_type, mkcl_index data_size)
{

  mkcl_object output = mkcl_alloc_raw_foreign(env, data_size);
  output->foreign.C_type = C_type;
  return output;
}

mkcl_object
mkcl_allocate_foreign_data(MKCL, mkcl_object C_type, mkcl_index size)
{
  mkcl_object output = mkcl_alloc_raw_foreign(env, size);

  output->foreign.C_type = C_type;
  return output;
}

void *
mkcl_foreign_raw_pointer(MKCL, mkcl_object f)
{
  if (mkcl_type_of(f) != mkcl_t_foreign)
    mkcl_FEwrong_type_argument(env, @'si::foreign', f);
  return ((void **) f->foreign.data)[0];
}

char *
mkcl_base_string_raw_pointer(MKCL, mkcl_object f)
{
  unsigned char *s;

  f = mkcl_check_cl_type(env, @'si::make-foreign-data-from-array', f, mkcl_t_base_string);
  s = f->base_string.self;
  if (f->base_string.hasfillp && s[f->base_string.fillp] != 0) {
    mkcl_FEerror(env, "Cannot coerce a base-string with fill pointer to (char *)", 0); /* Is this still really true? JCB */
  }
  return (char *)s;
}

mkcl_object
mkcl_null_terminated_base_string(MKCL, mkcl_object f)
{
  /* FIXME! Is there a better function name? */
  f = mkcl_check_cl_type(env, @'si::make-foreign-data-from-array', f, mkcl_t_base_string);
  if (f->base_string.hasfillp && f->base_string.self[f->base_string.fillp] != 0) {
    return mk_cl_copy_seq(env, f);
  } else {
    return f;
  }
}

mkcl_object
mk_si_allocate_foreign_data(MKCL, mkcl_object tag, mkcl_object size)
{
  mkcl_call_stack_check(env);
  mkcl_index bytes = mkcl_integer_to_index(env, size);
  mkcl_object output = mkcl_allocate_foreign_data(env, tag, bytes);

  @(return output);
}

mkcl_object
mk_si_make_foreign_null_pointer(MKCL)
{
  mkcl_call_stack_check(env);
  mkcl_object output = mkcl_alloc_raw_foreign(env, sizeof(void *));

  output->foreign.C_type = mk_cl_list(env, 2, @'*', @':void');
  ((void **) output->foreign.data)[0] = NULL;
  @(return output);
}

#if 0
mkcl_object
mk_si_free_foreign_data(MKCL, mkcl_object f)
{
  mkcl_call_stack_check(env);
  if (mkcl_type_of(f) != mkcl_t_foreign) {
    mkcl_FEwrong_type_argument(env, @'si::foreign', f);
  }
  if (f->foreign.size) {
    /* See mk_si_allocate_foreign_data() */
    mkcl_free_uncollectable(env, f->foreign.data);
  }
  f->foreign.size = 0;
  f->foreign.data = NULL;
  @(return );
}
#endif


mkcl_object
mk_si_make_foreign_data_from_array(MKCL, mkcl_object array)
{
  mkcl_object elem_type = mk_cl_Cnil;

  mkcl_call_stack_check(env);
  if (mkcl_type_of(array) != mkcl_t_array && mkcl_type_of(array) != mkcl_t_vector) {
    mkcl_FEwrong_type_argument(env, @'array', array);
  }
  switch (array->array.elttype) {
  case mkcl_aet_sf: elem_type = @':float'; break;
  case mkcl_aet_df: elem_type = @':double'; break;
#if MKCL_WORD_BITS > MKCL_LONG_BITS
  case mkcl_aet_word: elem_type = @':long-long'; break;
  case mkcl_aet_index: elem_type = @':unsigned-long-long'; break;
#else
  case mkcl_aet_word: elem_type = @':long'; break;
  case mkcl_aet_index: elem_type = @':unsigned-long'; break;
#endif
    /* Why not permit other element types? JCB */
  default:
    mkcl_FEerror(env, "Cannot make foreign object from array with element type ~S.",
		 1, mkcl_elttype_to_symbol(env, array->array.elttype));
    break;
  }
  mkcl_object C_type = mkcl_cons(env, @'*', mkcl_cons(env, elem_type, mk_cl_Cnil));
  mkcl_object obj = mkcl_make_foreign(env, C_type, sizeof(void *));

  *((char **) obj->foreign.data) = array->array.self.bc;
  @(return obj);
}

mkcl_object
mk_si_foreign_address(MKCL, mkcl_object f)
{
  mkcl_call_stack_check(env);
  if (mkcl_type_of(f) != mkcl_t_foreign) {
    mkcl_FEwrong_type_argument(env, @'si::foreign', f);
  }
  @(return mkcl_make_unsigned_integer(env, (mkcl_index)f->foreign.data));
}


mkcl_object
mk_si_foreign_ref(MKCL, mkcl_object f, mkcl_object andx, mkcl_object asize)
{
  mkcl_call_stack_check(env);
  mkcl_index ndx = mkcl_integer_to_index(env, andx);
  mkcl_index size = mkcl_integer_to_index(env, asize);
  mkcl_object output;

  if (mkcl_type_of(f) != mkcl_t_foreign) {
    mkcl_FEwrong_type_argument(env, @'si::foreign', f);
  }
  if (ndx >= f->foreign.size || (f->foreign.size - ndx) < size) {
    mkcl_FEerror(env, "Out of bounds reference into foreign data type ~A.", 1, f);
  }

  mkcl_object C_type = mkcl_cons(env, @'*', f->foreign.C_type);
  output = mkcl_make_foreign(env, C_type, sizeof(void *));
  *((char **) output->foreign.data) = f->foreign.data + ndx;
  @(return output);
}

mkcl_object
mk_si_foreign_set(MKCL, mkcl_object f, mkcl_object andx, mkcl_object value)
{
  mkcl_call_stack_check(env);
  mkcl_index ndx = mkcl_integer_to_index(env, andx);
  mkcl_index size, limit;

  if (mkcl_type_of(f) != mkcl_t_foreign) {
    mkcl_FEwrong_type_argument(env, @'si::foreign', f);
  }
  if (mkcl_type_of(value) != mkcl_t_foreign) {
    mkcl_FEwrong_type_argument(env, @'si::foreign', value);
  }
  size = value->foreign.size;
  limit = f->foreign.size;
  if (ndx >= limit || (limit - ndx) < size) {
    mkcl_FEerror(env, "Out of bounds reference into foreign data type ~A.", 1, f);
  }
  memcpy(f->foreign.data + ndx, value->foreign.data, size);
  @(return value);
}

enum mkcl_ffi_tag
mkcl_foreign_type_code(MKCL, mkcl_object type)
{
  int i;
  for (i = 0; i <= MKCL_FFI_VOID; i++) {
    if (type == mkcl_foreign_type_table[i])
      return (enum mkcl_ffi_tag)i;
  }
  mkcl_FEerror(env, "~A does not denote an elementary foreign type.", 1, type);
  return MKCL_FFI_VOID;
}


mkcl_object
mkcl_foreign_ref_elt(MKCL, void *p, enum mkcl_ffi_tag tag)
{ /* Translate C basic typed data to a lisp object. */ /* to_lisp */
  switch (tag) {
  case MKCL_FFI_CHAR:
    return MKCL_CODE_CHAR(*(char *)p);
  case MKCL_FFI_UNSIGNED_CHAR:
    return MKCL_CODE_CHAR(*(unsigned char *)p);
  case MKCL_FFI_BYTE:
    return MKCL_MAKE_FIXNUM(*(int8_t *)p);
  case MKCL_FFI_UNSIGNED_BYTE:
    return MKCL_MAKE_FIXNUM(*(uint8_t *)p);
  case MKCL_FFI_SHORT:
    return MKCL_MAKE_FIXNUM(*(short *)p);
  case MKCL_FFI_UNSIGNED_SHORT:
    return MKCL_MAKE_FIXNUM(*(unsigned short *)p);
  case MKCL_FFI_INT:
    return mkcl_make_integer(env, *(int *)p);
  case MKCL_FFI_UNSIGNED_INT:
    return mkcl_make_unsigned_integer(env, *(unsigned int *)p);
  case MKCL_FFI_LONG:
    return mkcl_make_integer(env, *(long *)p);
  case MKCL_FFI_LONG_LONG:
    return mkcl_make_long_long(env, *(mkcl_long_long_t *)p);
  case MKCL_FFI_UNSIGNED_LONG_LONG:
    return mkcl_make_ulong_long(env, *(mkcl_ulong_long_t *)p);
  case MKCL_FFI_UNSIGNED_LONG:
    return mkcl_make_unsigned_integer(env, *(unsigned long *)p);
  case MKCL_FFI_POINTER_VOID:
    {
      mkcl_object C_type = mk_cl_list(env, 2, @'*', @':void');
      mkcl_object obj = mkcl_make_foreign(env, C_type, sizeof(void *));
      *((void **) obj->foreign.data) = *(void **)p;
      return obj;
    }
  case MKCL_FFI_CSTRING:
    return *(char **)p ? mkcl_make_simple_base_string(env, *(char **)p) : mk_cl_Cnil; /* external-format needed? JCB */
  case MKCL_FFI_OBJECT:
    return *(mkcl_object *)p;
  case MKCL_FFI_FLOAT:
    return mkcl_make_singlefloat(env, *(float *)p);
  case MKCL_FFI_DOUBLE:
    return mkcl_make_doublefloat(env, *(double *)p);
  case MKCL_FFI_LONG_DOUBLE:
#ifdef MKCL_LONG_FLOAT
    return mkcl_make_longfloat(env, *(long double *)p);
#else
    return mkcl_make_doublefloat(env, *(long double *)p);
#endif
  case MKCL_FFI_VOID:
    return mk_cl_Cnil;
  default:
    mkcl_lose(env, "Unknown foreign data type tag received in mkcl_foreign_ref_elt");
  }
}

void
mkcl_foreign_set_elt(MKCL, void *p, enum mkcl_ffi_tag tag, mkcl_object value)
{ /* Translate a lisp object to a said C basic type data. (unwrapped) */ /* to_foreign */
  switch (tag) {
  case MKCL_FFI_CHAR:
    *(char *)p = (char)mkcl_base_char_code(env, value);
    break;
  case MKCL_FFI_UNSIGNED_CHAR:
    *(unsigned char*)p = (unsigned char)mkcl_base_char_code(env, value);
    break;
  case MKCL_FFI_BYTE:
    *(int8_t *)p = mkcl_integer_to_word(env, value);
    break;
  case MKCL_FFI_UNSIGNED_BYTE:
    *(uint8_t *)p = mkcl_integer_to_index(env, value);
    break;
  case MKCL_FFI_SHORT:
    *(short *)p = mkcl_integer_to_word(env, value);
    break;
  case MKCL_FFI_UNSIGNED_SHORT:
    *(unsigned short *)p = mkcl_integer_to_index(env, value);
    break;
  case MKCL_FFI_INT:
    *(int *)p = mkcl_integer_to_word(env, value);
    break;
  case MKCL_FFI_UNSIGNED_INT:
    *(unsigned int *)p = mkcl_integer_to_index(env, value);
    break;
  case MKCL_FFI_LONG:
    *(long *)p = mkcl_integer_to_word(env, value);
    break;
  case MKCL_FFI_UNSIGNED_LONG:
    *(unsigned long *)p = mkcl_integer_to_index(env, value);
    break;
  case MKCL_FFI_LONG_LONG:
    *(mkcl_long_long_t *)p = mkcl_to_long_long(env, value);
    break;
  case MKCL_FFI_UNSIGNED_LONG_LONG:
    *(mkcl_ulong_long_t *)p = mkcl_to_ulong_long(env, value);
    break;
  case MKCL_FFI_POINTER_VOID:
    *(void **)p = mkcl_foreign_raw_pointer(env, value);
    break;
  case MKCL_FFI_CSTRING:
    *(char **)p = value == mk_cl_Cnil ? NULL : (char*)value->base_string.self; /* The only one that can cause a SIGSEGV! JCB */
    break;
  case MKCL_FFI_OBJECT:
    *(mkcl_object *)p = value;
    break;
  case MKCL_FFI_FLOAT:
    *(float *)p = mkcl_to_float(env, value);
    break;
  case MKCL_FFI_DOUBLE:
    *(double *)p = mkcl_to_double(env, value);
    break;
  case MKCL_FFI_LONG_DOUBLE:
#ifdef MKCL_LONG_FLOAT
    *(long double *)p = mkcl_to_long_double(env, value);
#else
    *(long double *)p = mkcl_to_double(env, value);
#endif
    break;
  case MKCL_FFI_VOID:
    break;
  default:
    mkcl_lose(env, "Unknown foreign data type tag received in mkcl_foreign_set_elt");
  }
}

mkcl_object
mk_si_foreign_ref_elt(MKCL, mkcl_object f, mkcl_object andx, mkcl_object type)
{
  mkcl_call_stack_check(env);
  mkcl_index ndx = mkcl_integer_to_index(env, andx);
  mkcl_index limit = f->foreign.size;
  enum mkcl_ffi_tag tag = mkcl_foreign_type_code(env, type);
  if (ndx >= limit || (ndx + mkcl_foreign_type_size[tag] > limit)) {
    mkcl_FEerror(env, "Out of bounds reference into foreign data type ~A.", 1, f);
  }
  if (mkcl_type_of(f) != mkcl_t_foreign) {
    mkcl_FEwrong_type_argument(env, @'si::foreign', f);
  }
  @(return mkcl_foreign_ref_elt(env, (void*)(f->foreign.data + ndx), tag));
}

mkcl_object
mk_si_foreign_set_elt(MKCL, mkcl_object f, mkcl_object andx, mkcl_object type, mkcl_object value)
{
  mkcl_call_stack_check(env);
  mkcl_index ndx = mkcl_integer_to_index(env, andx);
  mkcl_index limit = f->foreign.size;
  enum mkcl_ffi_tag tag = mkcl_foreign_type_code(env, type);
  if (ndx >= limit || ndx + mkcl_foreign_type_size[tag] > limit) {
    mkcl_FEerror(env, "Out of bounds reference into foreign data type ~A.", 1, f);
  }
  if (mkcl_type_of(f) != mkcl_t_foreign) {
    mkcl_FEwrong_type_argument(env, @'si::foreign', f);
  }
  mkcl_foreign_set_elt(env, (void*)(f->foreign.data + ndx), tag, value);
  @(return value);
}

mkcl_object
mk_si_size_of_foreign_elt_type(MKCL, mkcl_object type)
{
  mkcl_call_stack_check(env);
  enum mkcl_ffi_tag tag = mkcl_foreign_type_code(env, type);
  @(return MKCL_MAKE_FIXNUM(mkcl_foreign_type_size[tag]));
}

mkcl_object
mk_si_null_pointer_p(MKCL, mkcl_object f)
{
  mkcl_call_stack_check(env);
  if (mkcl_type_of(f) != mkcl_t_foreign)
    mkcl_FEwrong_type_argument(env, @'si::foreign', f);
  @(return ((((void **) f->foreign.data)[0] == NULL) ? mk_cl_Ct : mk_cl_Cnil));
}

mkcl_object
mk_si_foreign_recast(MKCL, mkcl_object f, mkcl_object size, mkcl_object C_type)
{
  mkcl_call_stack_check(env);
  if (mkcl_type_of(f) != mkcl_t_foreign)
    mkcl_FEwrong_type_argument(env, @'si::foreign', f);
  f->foreign.size = mkcl_integer_to_index(env, size);
  f->foreign.C_type = C_type;
  @(return f);
}

mkcl_object
mk_si_load_foreign_module(MKCL, mkcl_object filename)
{
  mkcl_call_stack_check(env);
  mkcl_object output = mk_cl_Cnil;
  mkcl_object l_c_lock = mkcl_symbol_value(env, @'mt::+load-compile-lock+');
  volatile mkcl_object locked = mk_cl_Cnil;

  MKCL_UNWIND_PROTECT_BEGIN(env) {
    mkcl_interrupt_status old_intr;

    mkcl_get_interrupt_status(env, &old_intr);
    mkcl_disable_interrupts(env);
    locked = mk_mt_get_lock(env, 1, l_c_lock);
    mkcl_set_interrupt_status(env, &old_intr);

    output = mkcl_library_open(env, filename, FALSE);
    if (output->cblock.handle == NULL) {
      mkcl_object msg = mkcl_library_error(env, output);
      mkcl_library_close(env, output);
      output = msg;
    } else
      mkcl_core.libraries = mkcl_adjoin_eq(env, output, mkcl_core.libraries);

  } MKCL_UNWIND_PROTECT_EXIT {
    if (!mkcl_Null(locked)) mk_mt_giveup_lock(env, l_c_lock);
  } MKCL_UNWIND_PROTECT_END;
  if (mkcl_type_of(output) != mkcl_t_codeblock) {
    mkcl_FEerror(env, "LOAD-FOREIGN-MODULE: Could not load foreign module ~S (Error: ~S)", 2, filename, output);
  }
  output->cblock.locked |= 1;
  output->cblock.source = @':foreign';
  @(return output);
}

mkcl_object
mk_si_unload_foreign_module(MKCL, mkcl_object module)
{
  mkcl_object output = mk_cl_Cnil;
  mkcl_call_stack_check(env);

  if ((mkcl_type_of(module) != mkcl_t_codeblock) || module->cblock.source != @':foreign')
    mkcl_FEerror(env, "UNLOAD-FOREIGN-MODULE: Argument is not a foreign module: ~S", 1, module);

  {
    mkcl_object l_c_lock = mkcl_symbol_value(env, @'mt::+load-compile-lock+');
    volatile mkcl_object locked = mk_cl_Cnil;

    MKCL_UNWIND_PROTECT_BEGIN(env) {
      mkcl_interrupt_status old_intr;

      mkcl_get_interrupt_status(env, &old_intr);
      mkcl_disable_interrupts(env);
      locked = mk_mt_get_lock(env, 1, l_c_lock);
      mkcl_set_interrupt_status(env, &old_intr);

      if (mkcl_library_close(env, module))
	{ output = mk_cl_Cnil; }
      else
	{
	  mkcl_core.libraries = mkcl_funcall2(env, @+'delete', module, mkcl_core.libraries);
	  output = mk_cl_Ct;
	}
    } MKCL_UNWIND_PROTECT_EXIT {
      if (!mkcl_Null(locked)) mk_mt_giveup_lock(env, l_c_lock);
    } MKCL_UNWIND_PROTECT_END;
  }

  @(return output);
}

mkcl_object
mk_si_find_foreign_symbol(MKCL, mkcl_object var, mkcl_object module, mkcl_object C_type, mkcl_object size)
{
  mkcl_call_stack_check(env);
  volatile mkcl_object locked = mk_cl_Cnil;
  mkcl_object l_c_lock = mkcl_symbol_value(env, @'mt::+load-compile-lock+');
  mkcl_object block = (module == @':default' ? module : mk_si_load_foreign_module(env, module));
  mkcl_object output = mk_cl_Cnil;
  void *sym;

  var = mkcl_null_terminated_base_string(env, var);
  MKCL_UNWIND_PROTECT_BEGIN(env) {
    MKCL_NO_INTR(env, locked = mk_mt_get_lock(env, 1, l_c_lock));
    sym = mkcl_library_symbol(env, block, (char*)var->base_string.self, 1);
    if (sym == NULL) {
      output = mkcl_library_error(env, block);
    }
  } MKCL_UNWIND_PROTECT_EXIT {
    if (!mkcl_Null(locked)) mk_mt_giveup_lock(env, l_c_lock);
  } MKCL_UNWIND_PROTECT_END;

  if (sym != NULL)
    {
      output = mkcl_make_foreign(env, C_type, sizeof(void *));
      /* *((void **) output->foreign.data) = sym; */
      ((void **) output->foreign.data)[0] = sym;
    }

  if (mkcl_type_of(output) != mkcl_t_foreign)
    mkcl_FEerror(env, "FIND-FOREIGN-SYMBOL: Could not load foreign symbol ~S from module ~S (Error: ~S)", 3, var, module, output);
  @(return output);
}

static void
mkcl_fficall_overflow(MKCL, size_t new_bytes)
{
  struct mkcl_fficall *fficall = env->fficall;
  size_t size = fficall->buffer_size;
  size_t new_size;

  if (size < MKCL_FFICALL_ARGS_STAGING_AREA_GROWTH_INCREMENT)
    if (new_bytes < size)
      new_size = size + size;
    else
      new_size = size + new_bytes;
  else
    if (new_bytes < MKCL_FFICALL_ARGS_STAGING_AREA_GROWTH_INCREMENT)
      new_size = size + MKCL_FFICALL_ARGS_STAGING_AREA_GROWTH_INCREMENT;
    else
      new_size = size + new_bytes;

  char * new_buffer = mkcl_alloc(env, new_size);
  char * new_buffer_sp = new_buffer + (fficall->buffer_sp - fficall->buffer);

  memcpy(new_buffer, fficall->buffer, fficall->buffer_size);

  fficall->buffer = new_buffer;
  fficall->buffer_sp = new_buffer_sp;
  fficall->buffer_size = new_size;
}

mkcl_object
mk_si_trim_ffi_arguments_staging_area(MKCL)
{
  struct mkcl_fficall *fficall = env->fficall;

  fficall->buffer_size = MKCL_FFICALL_ARGS_STAGING_AREA_INITIAL_SIZE;
  fficall->buffer = mkcl_alloc(env, MKCL_FFICALL_ARGS_STAGING_AREA_INITIAL_SIZE);
  fficall->buffer_sp = fficall->buffer;
  @(return mk_cl_Cnil);
}

mkcl_object
mk_si_release_ffi_area(MKCL)
{
  env->fficall = NULL;
  @(return mk_cl_Cnil);
}

struct mkcl_fficall *
mkcl_fficall_prepare(MKCL, mkcl_object return_type, mkcl_object arg_type)
{
  struct mkcl_fficall *fficall = env->fficall;

  if (fficall == NULL)
    {
      env->fficall = fficall = mkcl_alloc(env, sizeof(struct mkcl_fficall));
      fficall->buffer = mkcl_alloc(env, MKCL_FFICALL_ARGS_STAGING_AREA_INITIAL_SIZE);
      fficall->buffer_size = MKCL_FFICALL_ARGS_STAGING_AREA_INITIAL_SIZE;
      fficall->buffer_sp = fficall->buffer;
      fficall->registers = NULL;
      fficall->output.pc = NULL;
    }
  else
    fficall->buffer_sp = fficall->buffer;
  fficall->registers = mkcl_fficall_prepare_extra(env, fficall->registers);
  return fficall;
}

void
mkcl_fficall_push_bytes(MKCL, void *data, size_t bytes)
{
  struct mkcl_fficall *fficall = env->fficall;

  if (((fficall->buffer_sp + bytes) - fficall->buffer) > fficall->buffer_size)
    mkcl_fficall_overflow(env, bytes);
  memcpy(fficall->buffer_sp, (char*)data, bytes);
  fficall->buffer_sp += bytes;
}

void
mkcl_fficall_push_int(MKCL, int data)
{
  mkcl_fficall_push_bytes(env, &data, sizeof(int));
}


void mkcl_fficall_align4(MKCL)
{
  struct mkcl_fficall * const fficall = env->fficall;
  fficall->buffer_sp = (char *) (((intptr_t) (fficall->buffer_sp + 0x3)) & ~((uintptr_t)0x3));
}

void mkcl_fficall_align8(MKCL)
{
  struct mkcl_fficall * const fficall = env->fficall;
  fficall->buffer_sp = (char *) (((intptr_t) (fficall->buffer_sp + 0x7)) & ~((uintptr_t)0x7));
}

void mkcl_fficall_align16(MKCL)
{
  struct mkcl_fficall * const fficall = env->fficall;
  fficall->buffer_sp = (char *) (((intptr_t) (fficall->buffer_sp + 0xF)) & ~((uintptr_t)0xF));
}

mkcl_object mk_si_call_cfun(MKCL, mkcl_object fun, mkcl_object return_type, mkcl_object arg_types, mkcl_object args)
{
  void *cfun = mkcl_foreign_raw_pointer(env, fun);
  enum mkcl_ffi_tag return_type_tag = mkcl_foreign_type_code(env, return_type);
  struct mkcl_fficall *fficall = mkcl_fficall_prepare(env, return_type, arg_types);

  mkcl_call_stack_check(env);

  while (MKCL_CONSP(arg_types)) {
    mkcl_object object;
    enum mkcl_ffi_tag type;
    if (!MKCL_CONSP(args)) {
      mkcl_FEerror(env, "In SI:CALL-CFUN, mismatch between argument types and argument list: ~A vs ~A", 0);
    }
    type = mkcl_foreign_type_code(env, MKCL_CAR(arg_types));
    if (type == MKCL_FFI_CSTRING) {
      object = mkcl_null_terminated_base_string(env, MKCL_CAR(args));
    } else {
      object = MKCL_CAR(args);
    }
    mkcl_foreign_set_elt(env, &fficall->output, type, object);
    mkcl_fficall_push_arg(env, &fficall->output, type);
    arg_types = MKCL_CDR(arg_types);
    args = MKCL_CDR(args);
  }
  mkcl_fficall_execute(env, cfun, fficall, return_type_tag);

  if (return_type_tag == MKCL_FFI_VOID)
    { @(return); }
  else
    {
      mkcl_object return_value = mkcl_foreign_ref_elt(env, &fficall->output, return_type_tag);
  
      fficall->buffer_sp = fficall->buffer;  
      @(return return_value);
    }
}

mkcl_object mk_si_make_dynamic_callback(MKCL, mkcl_object fun, mkcl_object sym, mkcl_object rtype, mkcl_object argtypes)
{
  mkcl_call_stack_check(env);

  mkcl_object data = mk_cl_list(env, 3, fun, rtype, argtypes);
  void * ptr = mkcl_dynamic_callback_make(env, data);
  mkcl_object C_type = mk_cl_list(env, 2, @'*', @':void');
  mkcl_object cbk  = mkcl_make_foreign(env, C_type, sizeof(void *));

  *((void **) cbk->foreign.data) = ptr;
  mk_si_put_sysprop(env, sym, @':callback', MKCL_CONS(env, cbk, data));
  @(return cbk);
}

