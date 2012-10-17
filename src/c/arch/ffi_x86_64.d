/* -*- mode: c -*- */
/*
    ffi_x86.c -- Nonportable component of the FFI
*/
/*
    Copyright (c) 2005, Juan Jose Garcia Ripoll.
    Copyright (c) 2010-2012, Jean-Claude Beaudoin

    MKCL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 3 of the License, or (at your option) any later version.

    See file '../../../Copyright' for full details.
*/

#include <mkcl/mkcl.h>
#include <string.h>
#include <mkcl/internal.h>

#define MAX_INT_REGISTERS 6
#define MAX_FP_REGISTERS 8

struct mkcl_fficall_reg {
  long int_registers[MAX_INT_REGISTERS]; /* Shouldn't it be "unsigned" instead? JCB. */
  int int_registers_size;
  double fp_registers[MAX_FP_REGISTERS];
  int fp_registers_size;
};

struct mkcl_fficall_reg *
mkcl_fficall_prepare_extra(MKCL, struct mkcl_fficall_reg *registers)
{
  if (registers == 0) {
    registers = mkcl_alloc(env, sizeof(*registers));
  }
  registers->int_registers_size = 0;
  registers->fp_registers_size = 0;
  return registers;
}

void
mkcl_fficall_push_arg(MKCL, union mkcl_ffi_values *data, enum mkcl_ffi_tag type)
{
  long i;
  struct mkcl_fficall *fficall = env->fficall;
  struct mkcl_fficall_reg *registers = fficall->registers;
  switch (type) {
  case MKCL_FFI_CHAR: i = data->c;	goto INT;
  case MKCL_FFI_UNSIGNED_CHAR: i = data->uc; goto INT;
  case MKCL_FFI_BYTE: i = data->b; goto INT;
  case MKCL_FFI_UNSIGNED_BYTE: i = data->ub; goto INT;
  case MKCL_FFI_INT16_T: i = data->i16; goto INT;
  case MKCL_FFI_UINT16_T: i = data->u16; goto INT;
  case MKCL_FFI_SHORT: i = data->s; goto INT;
  case MKCL_FFI_UNSIGNED_SHORT: i = data->us; goto INT;
  case MKCL_FFI_INT32_T: i = data->i32; goto INT;
  case MKCL_FFI_UINT32_T: i = data->u32; goto INT;
  case MKCL_FFI_INT: i = data->i; goto INT;
  case MKCL_FFI_UNSIGNED_INT: i = data->ui; goto INT;
  case MKCL_FFI_LONG:
  case MKCL_FFI_UNSIGNED_LONG:
  case MKCL_FFI_INT64_T:
  case MKCL_FFI_UINT64_T:
  case MKCL_FFI_LONG_LONG:
  case MKCL_FFI_UNSIGNED_LONG_LONG:
  case MKCL_FFI_POINTER_VOID:
  case MKCL_FFI_CSTRING:
  case MKCL_FFI_OBJECT:
    i = data->l;
  INT:
    if (registers->int_registers_size < MAX_INT_REGISTERS) {
      registers->int_registers[registers->int_registers_size++] = i;
    } else {
      mkcl_fficall_align8(env);
      mkcl_fficall_push_bytes(env, &i, sizeof(long));
    }
    break;
  case MKCL_FFI_DOUBLE:
    if (registers->fp_registers_size < MAX_FP_REGISTERS) {
      registers->fp_registers[registers->fp_registers_size++] = data->d;
    } else {
      mkcl_fficall_align8(env);
      mkcl_fficall_push_bytes(env, &data->d, sizeof(double));
    }
    break;
  case MKCL_FFI_FLOAT:
    if (registers->fp_registers_size < MAX_FP_REGISTERS) {
      memset(&registers->fp_registers[registers->fp_registers_size], 0, sizeof(double));
      (*(float*)(&registers->fp_registers[registers->fp_registers_size++])) = (float)data->f;
    } else {
      i = 0;
      mkcl_fficall_align8(env);
      mkcl_fficall_push_bytes(env, &data->f, sizeof(float));
      mkcl_fficall_push_bytes(env, &i, sizeof(float));
    }
    break;
  case MKCL_FFI_LONG_DOUBLE:
    /* According to the specification, arguments of this type
       should be passed on the stack ("in memory" as they say).
     */
    mkcl_FEerror(env, "LONG DOUBLE is not implemented yet as argument type for a C function", 0);
    break;
  case MKCL_FFI_VOID:
    mkcl_FEerror(env, "VOID is not a valid argument type for a C function", 0);
  default:
    mkcl_FEerror(env, "Invalid argument type for a C function", 0);
  }
}

void
mkcl_fficall_execute(MKCL, void *_f_ptr, struct mkcl_fficall *fficall, enum mkcl_ffi_tag return_type)
{
  struct mkcl_fficall_reg *registers = fficall->registers;
  char* buf = fficall->buffer;
  void * stack_p;
  size_t bufsize;

  mkcl_fficall_align16(env); /* Size of a cache line. */
  bufsize = fficall->buffer_sp - fficall->buffer;
#if 0
  printf("\nIn mkcl_fficall_execute(), stack arguments size = %lu.\n", bufsize); fflush(NULL);
#endif

  /* Save current stack pointer and then push stack based arguments. */
  asm volatile ("mov	%%rsp, %0\n\t"
		"sub	%1, %%rsp\n\t"
		"mov	%2, %%rsi\n\t"
		"mov	%%rsp, %%rdi\n\t"
		"rep\n\t"
		"movsb\n\t"
		: "=r" (stack_p) 
		: "c" (bufsize), "d" (buf) 
		: "%rdi", "%rsi", "%rsp");


  asm volatile ("movsd	(%0), %%xmm0\n\t"
		"movsd	0x08(%0), %%xmm1\n\t"
		"movsd	0x10(%0), %%xmm2\n\t"
		"movsd	0x18(%0), %%xmm3\n\t"
		"movsd	0x20(%0), %%xmm4\n\t"
		"movsd	0x28(%0), %%xmm5\n\t"
		"movsd	0x30(%0), %%xmm6\n\t"
		"movsd	0x38(%0), %%xmm7\n\t"
		:: "r" (registers->fp_registers)
		: "%xmm0", "%xmm1", "%xmm2", "%xmm3",
		  "%xmm4", "%xmm5", "%xmm6", "%xmm7");

  /* Registers r10 and r11 are mentionned in the clobber list of the asm
     directives below in order to prevent GCC with -02 from using
     either register to cache the value of stack_p across the function invocation.
     Such a usage would be in violation of the x86_64 calling conventions.  JCB
   */

  if (return_type <= MKCL_FFI_OBJECT)
    {
      asm volatile ("mov  (%3), %%rdi\n\t"
		    "mov  0x08(%3), %%rsi\n\t"
		    "mov  0x10(%3), %%rdx\n\t"
		    "mov  0x18(%3), %%rcx\n\t"
		    "mov  0x20(%3), %%r8\n\t"
		    "mov  0x28(%3), %%r9\n\t"
		    "call *%%rbx\n\t"
		    : "=a" (fficall->output.ul) 
		    : "b" (_f_ptr), "a" (registers->fp_registers_size),
		      "r" (registers->int_registers)
		    : "%rdx", "%rcx", "%rdi", "%rsi",
		      "%r8", "%r9", "r10", "r11");
    }
  else if (return_type == MKCL_FFI_FLOAT)
    {
      asm volatile ("mov  (%2), %%rdi\n\t"
		    "mov  0x08(%2), %%rsi\n\t"
		    "mov  0x10(%2), %%rdx\n\t"
		    "mov  0x18(%2), %%rcx\n\t"
		    "mov  0x20(%2), %%r8\n\t"
		    "mov  0x28(%2), %%r9\n\t"
		    "call *%%rbx\n\t"
		    : /* see next asm */
		    : "b" (_f_ptr),
		      "a" (registers->fp_registers_size),
		      "r" (registers->int_registers)
		    : "%rdx", "%rcx", "%rdi", "%rsi",
		      "%r8", "%r9", "r10", "r11", "%xmm0");
      asm volatile ("movss %%xmm0, (%0)\n\t"
		    :: "r" (&(fficall->output.f)));
    }
  else if (return_type == MKCL_FFI_DOUBLE)
    {
      asm volatile ("mov  (%2), %%rdi\n\t"
		    "mov  0x08(%2), %%rsi\n\t"
		    "mov  0x10(%2), %%rdx\n\t"
		    "mov  0x18(%2), %%rcx\n\t"
		    "mov  0x20(%2), %%r8\n\t"
		    "mov  0x28(%2), %%r9\n\t"
		    "call *%%rbx\n\t"
		    : /* see next asm */
		    : "b" (_f_ptr),
		      "a" (registers->fp_registers_size),
		      "r" (registers->int_registers)
		    : "%rdx", "%rcx", "%rdi", "%rsi",
		      "%r8", "%r9", "r10", "r11", "%xmm0");
      asm volatile ("movsd %%xmm0, (%0)\n\t"
		    :: "r" (&(fficall->output.d)));
    }
  else if (return_type == MKCL_FFI_VOID)
    {
      asm volatile ("mov  (%2), %%rdi\n\t"
		    "mov  0x08(%2), %%rsi\n\t"
		    "mov  0x10(%2), %%rdx\n\t"
		    "mov  0x18(%2), %%rcx\n\t"
		    "mov  0x20(%2), %%r8\n\t"
		    "mov  0x28(%2), %%r9\n\t"
		    "call *%%rbx\n\t"
		    : /* no output */
		    : "b" (_f_ptr),
		      "a" (registers->fp_registers_size),
		      "r" (registers->int_registers)
		    : "%rdx", "%rcx", "%rdi", "%rsi",
		      "%r8", "%r9", "r10", "r11");
    }
  else
    mkcl_FEerror(env, "Invalid argument type for a C function", 0);

  /* restore saved stack pointer */
  asm volatile ("mov %0,%%rsp" :: "r" (stack_p): "%rsp");
}


static const mkcl_base_string_object(mkcl_dynamic_callback_import_thread_name__obj_, "mkcl_dynamic_callback_execute");
static const mkcl_object mkcl_dynamic_callback_import_thread_name = (mkcl_object) &mkcl_dynamic_callback_import_thread_name__obj_;


static void
mkcl_dynamic_callback_execute(long i1, long i2, long i3, long i4, long i5, long i6,
			      double f1, double f2, double f3, double f4,
			      double f5, double f6, double f7, double f8,
			      mkcl_object cbk_info, char *arg_buffer)
{
  char stack_mark = 0;
  mkcl_object fun, rtype, argtypes;
  mkcl_object result;
  mkcl_index /* i, */ size, i_reg_index, f_reg_index;
  union mkcl_ffi_values output;
  enum mkcl_ffi_tag tag;
  long i_reg[MAX_INT_REGISTERS];
  double f_reg[MAX_FP_REGISTERS];
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

  i_reg_index = f_reg_index = 0;
  i_reg[0] = i1;
  i_reg[1] = i2;
  i_reg[2] = i3;
  i_reg[3] = i4;
  i_reg[4] = i5;
  i_reg[5] = i6;
  f_reg[0] = f1;
  f_reg[1] = f2;
  f_reg[2] = f3;
  f_reg[3] = f4;
  f_reg[4] = f5;
  f_reg[5] = f6;
  f_reg[6] = f7;
  f_reg[7] = f8;

  arg_buffer += 2*sizeof(void*); /* Skip return address and base pointer */
  for (/* i=0 */; !mkcl_endp(env, argtypes); argtypes = MKCL_CDR(argtypes)/* , i++ */) {
    tag = mkcl_foreign_type_code(env, MKCL_CAR(argtypes));
    size = mkcl_fixnum_to_word(mk_si_size_of_foreign_elt_type(env, MKCL_CAR(argtypes)));
    if (tag <= MKCL_FFI_OBJECT) {
      if (i_reg_index < MAX_INT_REGISTERS)
	result = mkcl_foreign_ref_elt(env, &i_reg[i_reg_index++], tag);
      else
	goto ARG_FROM_STACK;
    } else if (tag <= MKCL_FFI_DOUBLE) {
      if (f_reg_index < MAX_FP_REGISTERS)
	result = mkcl_foreign_ref_elt(env, &f_reg[f_reg_index++], tag);
      else
	goto ARG_FROM_STACK;
    } else {
    ARG_FROM_STACK:
      result = mkcl_foreign_ref_elt(env, arg_buffer, tag);
      {
	mkcl_index sp = (size + 0x7) & ~((mkcl_index) 0x7);
	arg_buffer += (sp);
      }
    }
    mkcl_temp_stack_frame_push(env, frame, result);
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
    register unsigned long rax asm("rax");
  case MKCL_FFI_CHAR: rax = output.c; return;
  case MKCL_FFI_UNSIGNED_CHAR: rax = output.uc; return;
  case MKCL_FFI_BYTE: rax = output.b; return;
  case MKCL_FFI_UNSIGNED_BYTE: rax = output.ub; return;
  case MKCL_FFI_INT16_T: rax = output.i16; return;
  case MKCL_FFI_UINT16_T: rax = output.u16; return;
  case MKCL_FFI_SHORT: rax = output.s; return;
  case MKCL_FFI_UNSIGNED_SHORT: rax = output.us; return;
  case MKCL_FFI_INT32_T: rax = output.i32; return;
  case MKCL_FFI_UINT32_T: rax = output.u32; return;
  case MKCL_FFI_INT:
  case MKCL_FFI_UNSIGNED_INT:
  case MKCL_FFI_LONG:
  case MKCL_FFI_UNSIGNED_LONG:
  case MKCL_FFI_INT64_T:
  case MKCL_FFI_UINT64_T:
  case MKCL_FFI_LONG_LONG:
  case MKCL_FFI_UNSIGNED_LONG_LONG:
  case MKCL_FFI_POINTER_VOID:
  case MKCL_FFI_CSTRING:
  case MKCL_FFI_OBJECT: rax = output.ul; return;
  case MKCL_FFI_DOUBLE: asm("movsd (%0),%%xmm0" :: "a" (&output.d)); return;
  case MKCL_FFI_FLOAT: asm("movss (%0),%%xmm0" :: "a" (&output.f)); return;
  case MKCL_FFI_VOID: return;
  /* default: mkcl_FEerror(env, "Invalid return type for a C function callback", 0); */
  }
}



void*
mkcl_dynamic_callback_make(MKCL, mkcl_object data, enum mkcl_ffi_calling_convention cc_type)
{
  /*
   *	push    %rbp                    55
   *	push    %rsp                    54
   *	mov     <addr64>,%rax           48 b8 <addr64>
   *	push    %rax                    50
   *	mov     <addr64>,%rax           48 b8 <addr64>
   *	callq   *%rax                   48 ff d0
   *	pop     %rcx                    59
   *	pop     %rcx                    59
   *	pop     %rbp                    5d
   *	ret                             c3
   *	nop				90
   *	nop				90
   */
  char *buf = (char*)mkcl_alloc_atomic_align(env, 32, 8);
  *(char*) (buf+0)  = 0x55;
  *(char*) (buf+1)  = 0x54;
  *(short*)(buf+2)  = 0xb848;
  *(intptr_t*) (buf+4)  = (intptr_t)data;
  *(char*) (buf+12) = 0x50;
  *(short*)(buf+13) = 0xb848;
  *(intptr_t*) (buf+15) = (intptr_t)mkcl_dynamic_callback_execute;
  *(int*)  (buf+23) = (int)0x00d0ff48;	/* leading null byte is overwritten */
  *(char*) (buf+26) = 0x59;
  *(char*) (buf+27) = 0x59;
  *(char*) (buf+28) = 0x5d;
  *(char*) (buf+29) = 0xc3;
  *(short*)(buf+30) = 0x9090;

  return buf;
}
