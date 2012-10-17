/* -*- mode: c -*- */
/*
    ffi_x86.c -- Nonportable component of the FFI
*/
/*
    Copyright (c) 2005, Juan Jose Garcia Ripoll.
    Copyright (c) 2010-2012, Jean-Claude Beaudoin.

    MKCL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 3 of the License, or (at your option) any later version.

    See file '../../../Copyright' for full details.
*/

#include <mkcl/mkcl.h>
#include <string.h>
#include <mkcl/internal.h>

struct mkcl_fficall_reg *
mkcl_fficall_prepare_extra(MKCL, struct mkcl_fficall_reg *registers)
{
  /* No need to prepare registers */
  return 0;
}

void
mkcl_fficall_push_arg(MKCL, union mkcl_ffi_values *data, enum mkcl_ffi_tag type)
{
  int i;
  switch (type) {
  case MKCL_FFI_CHAR: i = data->c;	goto INT;
  case MKCL_FFI_UNSIGNED_CHAR: i = data->uc; goto INT;
  case MKCL_FFI_BYTE: i = data->b; goto INT;
  case MKCL_FFI_UNSIGNED_BYTE: i = data->ub; goto INT;
  case MKCL_FFI_SHORT: i = data->s; goto INT;
  case MKCL_FFI_UNSIGNED_SHORT: i = data->us; goto INT;
  case MKCL_FFI_INT16_T: i = data->i16; goto INT;
  case MKCL_FFI_UINT16_T: i = data->u16; goto INT;
  case MKCL_FFI_INT:
  case MKCL_FFI_LONG:
  case MKCL_FFI_UNSIGNED_INT:
  case MKCL_FFI_UNSIGNED_LONG:
  case MKCL_FFI_INT32_T:
  case MKCL_FFI_UINT32_T:
  case MKCL_FFI_POINTER_VOID:
  case MKCL_FFI_CSTRING:
  case MKCL_FFI_OBJECT:
    i = data->i;
  INT:
    mkcl_fficall_align4(env);
    mkcl_fficall_push_int(env, i);
    break;
  case MKCL_FFI_DOUBLE:
    mkcl_fficall_align4(env);
    mkcl_fficall_push_bytes(env, &data->d, sizeof(double));
    break;
  case MKCL_FFI_FLOAT:
    mkcl_fficall_align4(env);
    mkcl_fficall_push_bytes(env, &data->f, sizeof(float));
    break;
  case MKCL_FFI_LONG_DOUBLE:
    mkcl_fficall_align4(env);
    mkcl_fficall_push_bytes(env, &data->d, sizeof(long double));
    break;
  case MKCL_FFI_UINT64_T:
  case MKCL_FFI_INT64_T:
    mkcl_fficall_align4(env);
    mkcl_fficall_push_bytes(env, &data->ull, sizeof(mkcl_uint64_t));
    break;
  case MKCL_FFI_UNSIGNED_LONG_LONG:
  case MKCL_FFI_LONG_LONG:
    mkcl_fficall_align4(env);
    mkcl_fficall_push_bytes(env, &data->ull, sizeof(unsigned long long));
    break;
  case MKCL_FFI_VOID:
    mkcl_FEerror(env, "VOID is not a valid argument type for a C function", 0);
  default:
    mkcl_FEerror(env, "Unknown argument type for a C function", 0);
  }
}

void
mkcl_fficall_execute(MKCL, void *f_ptr, struct mkcl_fficall *fficall, enum mkcl_ffi_tag return_type)
{
  char* buf = fficall->buffer;
  char* stack_p;
  size_t bufsize;

  mkcl_fficall_align16(env); /* Size of a cache line. */
  bufsize = fficall->buffer_sp - fficall->buffer;

  /* Save current stack pointer and then push stack based arguments. */
#ifdef _MSC_VER
  __asm
    {
      mov	stack_p,esp
	sub	esp,bufsize
	mov	esi,buf
	mov	edi,esp
	mov	ecx,bufsize
	rep	movsb
	}
#else
  asm volatile (
		"movl	%%esp, %0\n\t"
		"subl	%1, %%esp\n\t"
		"movl	%2, %%esi\n\t"
		"movl	%%esp, %%edi\n\t"
		"rep\n\t"
		"movsb\n\t"
		: "=a" (stack_p) : "c" (bufsize), "d" (buf) : "%edi", "%esi", "%esp");
  /* We could use esi to store esp instead of putting in the stack frame. */
#endif

  if (return_type <= MKCL_FFI_UNSIGNED_LONG) {
    fficall->output.i = ((int (*)())f_ptr)();
  } else if (return_type == MKCL_FFI_POINTER_VOID) {
    fficall->output.pv = ((void * (*)())f_ptr)();
  } else if (return_type == MKCL_FFI_CSTRING) {
    fficall->output.pc = ((char * (*)())f_ptr)();
  } else if (return_type == MKCL_FFI_OBJECT) {
    fficall->output.o = ((mkcl_object (*)())f_ptr)();
  } else if (return_type == MKCL_FFI_FLOAT) {
    fficall->output.f = ((float (*)())f_ptr)();
  } else if (return_type == MKCL_FFI_DOUBLE) {
    fficall->output.d = ((double (*)())f_ptr)();
  } else if (return_type == MKCL_FFI_LONG_DOUBLE) {
    fficall->output.ld = ((long double (*)())f_ptr)();
  }
  else if (return_type == MKCL_FFI_VOID) {
    ((void (*)())f_ptr)();
  }
  else if (return_type == MKCL_FFI_INT16_T) {
    fficall->output.i16 = ((mkcl_int16_t (*)())f_ptr)();
  } else if (return_type == MKCL_FFI_UINT16_T) {
    fficall->output.u16 = ((mkcl_uint16_t (*)())f_ptr)();
  }
  else if (return_type == MKCL_FFI_INT32_T) {
    fficall->output.i32 = ((mkcl_int32_t (*)())f_ptr)();
  } else if (return_type == MKCL_FFI_UINT32_T) {
    fficall->output.u32 = ((mkcl_uint32_t (*)())f_ptr)();
  }
  else if (return_type == MKCL_FFI_INT64_T) {
    fficall->output.i64 = ((mkcl_int64_t (*)())f_ptr)();
  } else if (return_type == MKCL_FFI_UINT64_T) {
    fficall->output.u64 = ((mkcl_uint64_t (*)())f_ptr)();
  }
  else if (return_type == MKCL_FFI_LONG_LONG) {
    fficall->output.ll = ((mkcl_long_long_t (*)())f_ptr)();
  } else if (return_type == MKCL_FFI_UNSIGNED_LONG_LONG) {
    fficall->output.ull = ((mkcl_ulong_long_t (*)())f_ptr)();
  }
  else {
    mkcl_FEerror(env, "Unknown C function return type", 0);
  }

  /* restore saved stack pointer */
#ifdef _MSC_VER
  __asm mov esp,stack_p
#else
    asm volatile ("mov %0,%%esp" :: "a" (stack_p));
#endif
}


static const mkcl_base_string_object(mkcl_dynamic_callback_import_thread_name__obj_, "mkcl_dynamic_callback_execute");
static const mkcl_object mkcl_dynamic_callback_import_thread_name = (mkcl_object) &mkcl_dynamic_callback_import_thread_name__obj_;

static void
mkcl_dynamic_callback_execute(mkcl_object cbk_info, char *arg_buffer)
{
  char stack_mark = 0;
  mkcl_object fun, rtype, argtypes;
  mkcl_object result;
  mkcl_index i, size;
  union mkcl_ffi_values output;
  enum mkcl_ffi_tag tag;
  mkcl_env env = MKCL_ENV();
  mkcl_env imported_env = NULL;

  if (env == NULL)
    {
      env = imported_env = mkcl_import_current_thread(mkcl_dynamic_callback_import_thread_name, mk_cl_Cnil, NULL, NULL);
      if (imported_env == NULL)
	{ errno = ENOMEM; return; }
    }

  MKCL_BUILD_TEMP_STACK_FRAME(env, frame, aux);

  fun = MKCL_CAR(cbk_info);
  rtype = MKCL_CADR(cbk_info);
  argtypes = MKCL_CADDR(cbk_info);

  arg_buffer += 4; /* Skip return address */
  for (i=0; !mkcl_endp(env, argtypes); argtypes = MKCL_CDR(argtypes), i++) {
    tag = mkcl_foreign_type_code(env, MKCL_CAR(argtypes));
    size = mkcl_fixnum_to_word(mk_si_size_of_foreign_elt_type(env, MKCL_CAR(argtypes)));
    result = mkcl_foreign_ref_elt(env, arg_buffer, tag);
    mkcl_temp_stack_frame_push(env, frame,result);
    {
      mkcl_index sp = (size + 0x03) & ~((mkcl_index) 0x03);
      arg_buffer += (sp);
    }
  }

  if (imported_env)
    {
      mkcl_object thread = env->own_thread;

      MKCL_CATCH_ALL_BEGIN(env) {
	MKCL_SETUP_CALL_STACK_ROOT_GUARD(env);
	mkcl_setup_thread_lisp_context(env, &stack_mark);
	mkcl_register_thread_as_active(env, thread);
	mkcl_enable_interrupts(env);

	result = mkcl_apply_from_temp_stack_frame(env, frame, fun);

	mkcl_disable_interrupts(env);
#if 1
	mkcl_cleanup_thread_lisp_context(env);
#else
	mkcl_bds_unwind1(env);
#endif
      } MKCL_CATCH_ALL_END;
      thread->thread.status = mkcl_thread_done;
    }
  else
    result = mkcl_apply_from_temp_stack_frame(env, frame, fun);
  mkcl_temp_stack_frame_close(env, frame);

  tag = mkcl_foreign_type_code(env, rtype);
  memset(&output, 0, sizeof(output));
  mkcl_foreign_set_elt(env, &output, tag, result);

  if (imported_env)
    mkcl_release_current_thread(imported_env);

  errno = 0;
  switch (tag) {
  case MKCL_FFI_CHAR: i = output.c; goto INT;
  case MKCL_FFI_UNSIGNED_CHAR: i = output.uc; goto INT;
  case MKCL_FFI_BYTE: i = output.b; goto INT;
  case MKCL_FFI_UNSIGNED_BYTE: i = output.ub; goto INT;

  case MKCL_FFI_INT16_T:
  case MKCL_FFI_SHORT: i = output.s; goto INT;

  case MKCL_FFI_UINT16_T:
  case MKCL_FFI_UNSIGNED_SHORT: i = output.us; goto INT;

  case MKCL_FFI_POINTER_VOID:
  case MKCL_FFI_OBJECT:
  case MKCL_FFI_CSTRING:
  case MKCL_FFI_INT:
  case MKCL_FFI_UNSIGNED_INT:
  case MKCL_FFI_INT32_T:
  case MKCL_FFI_UINT32_T:
  case MKCL_FFI_LONG:
  case MKCL_FFI_UNSIGNED_LONG:
    i = output.i;
  INT:
#ifdef _MSC_VER
    __asm mov eax,i
#else
      {
	register int eax asm("eax");
	eax = i;
      }
#endif
    return;
  case MKCL_FFI_LONG_LONG:
  case MKCL_FFI_UNSIGNED_LONG_LONG:
  case MKCL_FFI_INT64_T:
  case MKCL_FFI_UINT64_T:
# ifdef _MSC_VER
    __asm mov eax,output.l2[0]
      __asm mov edx,output.l2[1]
# else
      {
	register int eax asm("eax");
	register int edx asm("edx");
	eax = output.l2[0];
	edx = output.l2[1];
      }
# endif
    return;

  case MKCL_FFI_DOUBLE: {
#ifdef _MSC_VER
    __asm fld output.d
#else
    {
      asm("fldl (%0)" :: "a" (&output.d));
    }
#endif
    return;
  }
  case MKCL_FFI_LONG_DOUBLE: {
#ifdef _MSC_VER
    __asm fld output.ld
#else
    {
      asm("fldt (%0)" :: "a" (&output.ld));
    }
#endif
    return;
  }
  case MKCL_FFI_FLOAT: {
#ifdef _MSC_VER
    __asm fld output.f
#else
    {
      asm("flds (%0)" :: "a" (&output.f));
    }
#endif
    return;
  }
  case MKCL_FFI_VOID:
    return;
  default:
    mkcl_FEerror(env, "Invalid C function callback return type", 0);
  }
}


void *
mkcl_dynamic_callback_make(MKCL, mkcl_object data, enum mkcl_ffi_calling_convention cc_type)
{
  /*
   *	push	%esp				54
   *	pushl	<data>				68 <addr32>
   *	call	mkcl_dynamic_callback_call	E8 <disp32>
   * [ Here we could use also lea 4(%esp), %esp, but %ecx seems to be free ]
   *	pop	%ecx				59
   *	pop	%ecx				59
   *	ret					c3
   *	nop					90
   *	nop					90
   */
  char *buf = (char*)mkcl_alloc_atomic_align(env, sizeof(char)*16, 4);
  *(char*) (buf+0)  = 0x54;
  *(char*) (buf+1)  = 0x68;
  *(long*) (buf+2)  = (long)data;
  *(unsigned char*) (buf+6)  = 0xE8;
  *(long*) (buf+7)  = (long)mkcl_dynamic_callback_execute - (long)(buf+11);
  *(char*) (buf+11) = 0x59;
  *(char*) (buf+12) = 0x59;
  if (cc_type == MKCL_FFI_CC_CDECL) {
    *(unsigned char*) (buf+13) = 0xc3;
    *(unsigned short*)(buf+14) = 0x9090;
  } else { /* This would be MKCL_FFI_CC_STDCALL. JCB */
    mkcl_object arg_types = MKCL_CADDR(data);
    int byte_size = 0;
    const unsigned int mask = 3;

    while (MKCL_CONSP(arg_types)) {
      int sz = mkcl_fixnum_to_word(mk_si_size_of_foreign_elt_type(env, MKCL_CAR(arg_types)));
      byte_size += ((sz+mask)&(~mask));
      arg_types = MKCL_CDR(arg_types);
    }

    *(unsigned char*) (buf+13) = 0xc2;
    *(unsigned short*)(buf+14) = (unsigned short)byte_size; /* This caps the value (of what?) to 65536! JCB */
  }

  return buf;
}
