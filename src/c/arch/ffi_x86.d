/* -*- mode: c -*- */
/*
    ffi_x86.c -- Nonportable component of the FFI
*/
/*
    Copyright (c) 2005, Juan Jose Garcia Ripoll.
    Copyright (c) 2010-2016, Jean-Claude Beaudoin.

    MKCL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 3 of the License, or (at your option) any later version.

    See file '../../../Copyright' for full details.
*/

#include <mkcl/mkcl.h>
#include <mkcl/internal.h>
#include <string.h>
#if MKCL_UNIX
#include <sys/mman.h>
#endif

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
  case MKCL_FFI_INT:
  case MKCL_FFI_LONG:
  case MKCL_FFI_UNSIGNED_INT:
  case MKCL_FFI_UNSIGNED_LONG:
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
        /* In the normal case we always set errno = 0, so this is clearly an error situation. */
	{ errno = ENOMEM; return; }
    }

  MKCL_BUILD_TEMP_STACK_FRAME(env, frame, aux);

  fun = MKCL_CAR(cbk_info);
  rtype = MKCL_CADR(cbk_info);
  argtypes = MKCL_CADDR(cbk_info);

  arg_buffer += sizeof(void *); /* Skip saved stack frame pointer */
  arg_buffer += sizeof(void *); /* Skip return address */

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
  case MKCL_FFI_SHORT: i = output.s; goto INT;
  case MKCL_FFI_UNSIGNED_SHORT: i = output.us; goto INT;

  case MKCL_FFI_POINTER_VOID:
  case MKCL_FFI_OBJECT:
  case MKCL_FFI_CSTRING:
  case MKCL_FFI_INT:
  case MKCL_FFI_UNSIGNED_INT:
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


#if MKCL_WINDOWS
void *
mkcl_stdcall_dynamic_callback_make(MKCL, mkcl_object data)
{
  unsigned char * buf = mkcl_alloc_callback_block(env);
  unsigned char * ip = buf; /* the instruction pointer (ip) */
  union { unsigned char b[4]; void * p; unsigned long l; unsigned short s; } imm; /* a staging buffer for immediate data */

#define i(byte) *(ip++) = (byte)
#define immed_ptr(val_ptr) imm.p = (val_ptr); i(imm.b[0]); i(imm.b[1]); i(imm.b[2]); i(imm.b[3]);
#define immed16(val_short) imm.s = (val_short);	i(imm.b[0]); i(imm.b[1]);
#define immed32(val_long) imm.l = (val_long); i(imm.b[0]); i(imm.b[1]); i(imm.b[2]); i(imm.b[3]);
    

  /* pushl  %ebp           */  i(0x55);                             /* build stack frame, step 1 of 2 */
  /* movl   %esp, %ebp     */  i(0x89); i(0xe5);                    /* build stack frame, step 2 of 2 */
  /* pushl  %esp           */  i(0x54);                             /* push arg_list pointer */
  /* movl   <addr32>, %eax */  i(0xb8); immed_ptr(data);
  /* pushl  %eax           */  i(0x50);                             /* push data */   
  /* movl   <addr32>, %eax */  i(0xb8); immed_ptr(mkcl_dynamic_callback_execute);
  /* call   *%eax          */  i(0xff); i(0xd0);                    /* call mkcl_dynamic_callback_execute() */
  /* addl   $16, %esp      */  i(0x83); i(0xc4); i(0x10);           /* cleanup arg list of previous call, 16 bytes. */
  /* leave                 */  i(0xc9);                             /* undo stack frame */

  { /* This would be MKCL_FFI_CC_STDCALL. JCB */
    mkcl_object arg_types = MKCL_CADDR(data);
    unsigned long arg_list_byte_size = 0;
    const unsigned long mask = 3;

    while (MKCL_CONSP(arg_types)) {
      unsigned int sz = mkcl_fixnum_to_word(mk_si_size_of_foreign_elt_type(env, MKCL_CAR(arg_types)));
      arg_list_byte_size += ((sz+mask)&(~mask));
      arg_types = MKCL_CDR(arg_types);
    }

    if (arg_list_byte_size > USHRT_MAX)
      {
	/* popl  %ecx                  */ i(0x59);                  /* get return %eip. */
	/* addl  <immed32>, %esp       */ i(0x81); i(0xc4); immed32(arg_list_byte_size); /* pop byte_size bytes. */
	/* jmp   *%ecx                 */ i(0xff); i(0xe1);         /* jump to return %eip. */
      }
    else
      {
	/* ret <immed16> */ i(0xc2); immed16(arg_list_byte_size);   /* return and pop byte_size bytes. */
      }
  }
  /* nop                   */  i(0x90);  /* Fill with nop until end of I-cache line (multiple of 16 bytes). */
  /* nop                   */  i(0x90);
  /* nop                   */  i(0x90);
  /* nop                   */  i(0x90);
  /* nop                   */  i(0x90);
  /* nop                   */  i(0x90);

  { /* By default on Win64 data is PAGE_READWRITE only and we would get
       an ACCESS_VIOLATION if we didn't set it to EXECUTE. */
    /* Not really needed on Win32 but we do it for uniformity with other platforms. */
    DWORD old_protection_flags;
    BOOL ok = VirtualProtect(buf, mkcl_core.pagesize, PAGE_EXECUTE_READ, &old_protection_flags);
    
    if (!ok)
      mkcl_FEwin32_error(env, "mkcl_dynamic_callback_make() failed on VirtualProtect()", 0);
  }

#if 0
  printf("\nIn mkcl_stdcall_dynamic_callback_make(), returning %p.\n", buf); fflush(NULL); /* debug JCB */
#endif
  return buf;
}
#endif

void *
mkcl_dynamic_callback_make(MKCL, mkcl_object data)
{
  unsigned char * buf = mkcl_alloc_callback_block(env);
  unsigned char * ip = buf; /* the instruction pointer (ip) */
  union { unsigned char b[4]; void * p; unsigned long l; unsigned short s; } imm; /* a staging buffer for immediate data */

#define i(byte) *(ip++) = (byte)
#define immed_ptr(val_ptr) imm.p = (val_ptr); i(imm.b[0]); i(imm.b[1]); i(imm.b[2]); i(imm.b[3]);
#define immed16(val_short) imm.s = (val_short);	i(imm.b[0]); i(imm.b[1]);
#define immed32(val_long) imm.l = (val_long); i(imm.b[0]); i(imm.b[1]); i(imm.b[2]); i(imm.b[3]);
    

  /* pushl  %ebp           */  i(0x55);                             /* build stack frame, step 1 of 2 */
  /* movl   %esp, %ebp     */  i(0x89); i(0xe5);                    /* build stack frame, step 2 of 2 */
  /* pushl  %esp           */  i(0x54);                             /* push arg_list pointer */
  /* movl   <addr32>, %eax */  i(0xb8); immed_ptr(data);
  /* pushl  %eax           */  i(0x50);                             /* push data */   
  /* movl   <addr32>, %eax */  i(0xb8); immed_ptr(mkcl_dynamic_callback_execute);
  /* call   *%eax          */  i(0xff); i(0xd0);                    /* call mkcl_dynamic_callback_execute() */
  /* addl   $16, %esp      */  i(0x83); i(0xc4); i(0x10);           /* cleanup arg list of previous call, 16 bytes. */
  /* leave                 */  i(0xc9);                             /* undo stack frame */
  /* ret                   */  i(0xc3);                             /* return */
  /* nop                   */  i(0x90);  /* Fill with nop until end of I-cache line (multiple of 16 bytes). */
  /* nop                   */  i(0x90);
  /* nop                   */  i(0x90);
  /* nop                   */  i(0x90);
  /* nop                   */  i(0x90);
  /* nop                   */  i(0x90);

#if MKCL_UNIX
  int rc = mprotect(buf, mkcl_core.pagesize, PROT_READ | /* PROT_WRITE | */ PROT_EXEC);
  if (rc)
    mkcl_FElibc_error(env, "mkcl_dynamic_callback_make() failed on mprotect()", 0);
#elif MKCL_WINDOWS
  { /* By default on Win64 data is PAGE_READWRITE only and we would get
       an ACCESS_VIOLATION if we didn't set it to EXECUTE. */
    /* Not really needed on Win32 but we do it for uniformity with other platforms. */
    DWORD old_protection_flags;
    BOOL ok = VirtualProtect(buf, mkcl_core.pagesize, PAGE_EXECUTE_READ, &old_protection_flags);
    
    if (!ok)
      mkcl_FEwin32_error(env, "mkcl_dynamic_callback_make() failed on VirtualProtect()", 0);
  }
#endif

#if 0
  printf("\nIn mkcl_dynamic_callback_make(), returning %p.\n", buf); fflush(NULL); /* debug JCB */
#endif
  return buf;
}
