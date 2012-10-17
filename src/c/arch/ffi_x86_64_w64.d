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

#define MAX_REGISTERS 4

struct mkcl_fficall_reg {
  union reg
  {
    int64_t i;
    float f;
    double d;
  } reg[MAX_REGISTERS];
  int reg_count;
};

struct mkcl_fficall_reg *
mkcl_fficall_prepare_extra(MKCL, struct mkcl_fficall_reg *registers)
{
  if (registers == 0) {
    registers = mkcl_alloc(env, sizeof(*registers));
  }
  registers->reg_count = 0;
  return registers;
}

void
mkcl_fficall_push_arg(MKCL, union mkcl_ffi_values *data, enum mkcl_ffi_tag type)
{
  int64_t i;
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
  case MKCL_FFI_LONG: i = data->l; goto INT;
  case MKCL_FFI_UNSIGNED_LONG: i = data->ul; goto INT;
  case MKCL_FFI_INT64_T: i = data->i64; goto INT;
  case MKCL_FFI_UINT64_T: i = data->u64; goto INT;
  case MKCL_FFI_LONG_LONG: i = data->ll; goto INT;
  case MKCL_FFI_UNSIGNED_LONG_LONG: i = data->ull; goto INT;
  case MKCL_FFI_POINTER_VOID: i = (intptr_t) data->pv; goto INT;
  case MKCL_FFI_CSTRING: i = (intptr_t) data->pc; goto INT;
  case MKCL_FFI_OBJECT: i = (intptr_t) data->o; goto INT;
  INT:
    if (registers->reg_count < MAX_REGISTERS) {
      registers->reg[registers->reg_count++].i = i;
    } else {
      mkcl_fficall_align8(env);
      mkcl_fficall_push_bytes(env, &i, sizeof(int64_t));
    }
    break;
  case MKCL_FFI_DOUBLE:
    if (registers->reg_count < MAX_REGISTERS) {
      registers->reg[registers->reg_count++].d = data->d;
    } else {
      mkcl_fficall_align8(env);
      mkcl_fficall_push_bytes(env, &data->d, sizeof(double));
    }
    break;
  case MKCL_FFI_FLOAT:
    if (registers->reg_count < MAX_REGISTERS) {
      registers->reg[registers->reg_count++].f = data->f;
    } else {
      long z = 0;
      mkcl_fficall_align8(env);
      mkcl_fficall_push_bytes(env, &data->f, sizeof(float));
      mkcl_fficall_push_bytes(env, &z, sizeof(float));
    }
    break;
  case MKCL_FFI_LONG_DOUBLE:
    /* According to the specification, arguments of this type
       should be passed on the stack ("in memory" as they say).
     */
    mkcl_FEerror(env, "LONG DOUBLE is not implemented yet as a C function argument type", 0);
    break;
  case MKCL_FFI_VOID:
    mkcl_FEerror(env, "VOID is not a valid C function argument type", 0);
  default:
    mkcl_FEerror(env, "Invalid C function argument type", 0);
  }
}

void
mkcl_fficall_execute(MKCL, void *_f_ptr, struct mkcl_fficall *fficall, enum mkcl_ffi_tag return_type)
{
  struct mkcl_fficall_reg *registers = fficall->registers;
  char* buf = fficall->buffer;
  /* void * volatile stack_p; */
  size_t bufsize;

  mkcl_fficall_align16(env); /* Size of a cache line. */
  bufsize = fficall->buffer_sp - fficall->buffer;

  /* Save current stack pointer and then push stack based arguments. */
  if (bufsize)
    asm volatile ("mov   %%rsp, %%rax\n\t"
		  "sub   %0, %%rsp\n\t"
		  "mov   %1, %%rsi\n\t"
		  "mov   %%rsp, %%rdi\n\t"
		  "rep movsb\n\t"
		  "mov   %%rax, %%rsi\n\t"
		  "sub    $32, %%rsp\n\t"
		  :: "c" (bufsize), "d" (buf)
		  : "%rax", "%rdi", "%rsi", "%rsp");
  else
    asm volatile ("mov   %%rsp, %%rsi\n\t"
		  "sub    $32, %%rsp\n\t"
		  ::: "%rsi", "%rsp");

  asm volatile ("movsd	(%0), %%xmm0\n\t"
		"movsd	0x08(%0), %%xmm1\n\t"
		"movsd	0x10(%0), %%xmm2\n\t"
		"movsd	0x18(%0), %%xmm3\n\t"
		:: "r" (registers->reg)
		: "%xmm0", "%xmm1", "%xmm2", "%xmm3");

  if (return_type <= MKCL_FFI_OBJECT)
    {
      asm volatile ("mov  (%2), %%rcx\n\t"
		    "mov  0x08(%2), %%rdx\n\t"
		    "mov  0x10(%2), %%r8\n\t"
		    "mov  0x18(%2), %%r9\n\t"
		    "call *%%rax\n\t"
		    : "=a" (fficall->output.ull) 
		    : "a" (_f_ptr),
		      "r" (registers->reg)
		    : "%rcx", "%rdx", "%r8", "%r9");
    }
  else if (return_type == MKCL_FFI_FLOAT)
    {
      asm volatile ("mov  (%1), %%rcx\n\t"
		    "mov  0x08(%1), %%rdx\n\t"
		    "mov  0x10(%1), %%r8\n\t"
		    "mov  0x18(%1), %%r9\n\t"
		    "call *%%rax\n\t"
		    : /* see next asm */
		    : "a" (_f_ptr),
		      "r" (registers->reg)
		    : "%rcx", "%rdx", "%r8", "%r9");
      asm volatile ("movss %%xmm0, (%0)\n\t"
		    :: "r" (&(fficall->output.f)));
    }
  else if (return_type == MKCL_FFI_DOUBLE)
    {
      asm volatile ("mov  (%1), %%rcx\n\t"
		    "mov  0x08(%1), %%rdx\n\t"
		    "mov  0x10(%1), %%r8\n\t"
		    "mov  0x18(%1), %%r9\n\t"
		    "call *%%rax\n\t"
		    : /* see next asm */
		    : "a" (_f_ptr),
		      "r" (registers->reg)
		    : "%rcx", "%rdx", "%r8", "%r9");
      asm volatile ("movsd %%xmm0, (%0)\n\t"
		    :: "r" (&(fficall->output.d)));
    }
  else if (return_type == MKCL_FFI_VOID)
    {
      asm volatile ("mov  (%1), %%rcx\n\t"
		    "mov  0x08(%1), %%rdx\n\t"
		    "mov  0x10(%1), %%r8\n\t"
		    "mov  0x18(%1), %%r9\n\t"
		    "call *%%rax\n\t"
		    : /* see next asm */
		    : "a" (_f_ptr),
		      "r" (registers->reg)
		    : "%rcx", "%rdx", "%r8", "%r9");
    }
  else
    mkcl_FEerror(env, "Unknown C function return type", 0);

  /* restore saved stack pointer */
  asm volatile ("mov %%rsi,%%rsp\n\t" ::: "%rsp");
}


static const mkcl_base_string_object(mkcl_dynamic_callback_import_thread_name__obj_, "mkcl_dynamic_callback_execute");
static const mkcl_object mkcl_dynamic_callback_import_thread_name = (mkcl_object) &mkcl_dynamic_callback_import_thread_name__obj_;


static void
mkcl_dynamic_callback_execute(int64_t i1, int64_t i2, int64_t i3, int64_t i4,
			      mkcl_object cbk_info, char *arg_buffer)
{
  char stack_mark = 0;
  mkcl_object fun, rtype, argtypes;
  mkcl_object result;
  mkcl_index size, reg_count;
  union mkcl_ffi_values output;
  enum mkcl_ffi_tag tag;
  union reg reg[MAX_REGISTERS];
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

  reg_count = 0;
  reg[0].i = i1;
  reg[1].i = i2;
  reg[2].i = i3;
  reg[3].i = i4;

  arg_buffer += 2*sizeof(void*); /* Skip return address and base pointer */
  arg_buffer += 4*sizeof(int64_t); /* Skip homes of the 4 register arguments */

  for (; !mkcl_endp(env, argtypes); argtypes = MKCL_CDR(argtypes))
    {
      tag = mkcl_foreign_type_code(env, MKCL_CAR(argtypes));
      size = mkcl_fixnum_to_word(mk_si_size_of_foreign_elt_type(env, MKCL_CAR(argtypes)));
      if (tag <= MKCL_FFI_OBJECT) {
	if (reg_count < MAX_REGISTERS)
	  result = mkcl_foreign_ref_elt(env, &(reg[reg_count++].i), tag);
	else
	  goto ARG_FROM_STACK;
      } else if (tag <= MKCL_FFI_DOUBLE) {
	if (reg_count < MAX_REGISTERS)
	  result = mkcl_foreign_ref_elt(env, &(reg[reg_count++].d), tag);
	else
	  goto ARG_FROM_STACK;
      } else {
      ARG_FROM_STACK:
	result = mkcl_foreign_ref_elt(env, arg_buffer, tag);
	{
	  mkcl_index sp = (size + 0x07) & ~((mkcl_index) 0x07);
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
  mkcl_foreign_set_elt(env, &output, tag, result);

  if (imported_env)
    mkcl_release_current_thread(imported_env);

  errno = 0;
  switch (tag) {
    register uint64_t rax asm("rax");
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
  case MKCL_FFI_INT: rax = output.i; return;
  case MKCL_FFI_UNSIGNED_INT: rax = output.ui; return;
  case MKCL_FFI_LONG: rax = output.l; return;
  case MKCL_FFI_UNSIGNED_LONG: rax = output.ul; return;

  case MKCL_FFI_INT64_T:
  case MKCL_FFI_UINT64_T:
  case MKCL_FFI_LONG_LONG:
  case MKCL_FFI_UNSIGNED_LONG_LONG:
  case MKCL_FFI_POINTER_VOID:
  case MKCL_FFI_CSTRING:
  case MKCL_FFI_OBJECT:
    rax = output.ull; return;
  case MKCL_FFI_DOUBLE:
    asm("movsd (%0),%%xmm0" :: "a" (&output.d));
    return;
  case MKCL_FFI_FLOAT:
    asm("movss (%0),%%xmm0" :: "a" (&output.f));
    return;
  case MKCL_FFI_VOID:
    return;
  default:
    mkcl_FEerror(env, "Invalid C function callback return type", 0);
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
   *    push    %r9                     41 51           ; push arg3
   *    push    %r8                     41 50           ; push arg2
   *    push    %rdx                    52              ; push arg1
   *    push    %rcx                    51              ; push arg0
   *	mov     <addr64>,%rax           48 b8 <addr64>
   *	callq   *%rax                   48 ff d0
   *	pop     %rcx                    59
   *	pop     %rcx                    59
   *	pop     %rbp                    5d
   *	ret                             c3
   *	nop				90
   *	nop				90
   *	nop				90
   *	nop				90
   *	nop				90
   *	nop				90
   *	nop				90
   *	nop				90
   *	nop				90
   *	nop				90
   *	nop				90
   *	nop				90
   */

  /*
   * we could also adjust the SP register this way
   * instead of pushing the 4 argument registers.
   *
   *    subq    $0x20,  %rsp            48 83 EC 20
   *
   */

  /* round the size of the routine to the next I-cache line boundary (48=3*16). */
  char *buf = (char*)mkcl_alloc_atomic_align(env, 48, 8);

  *(unsigned char*) (buf+0)  = 0x55;
  *(unsigned char*) (buf+1)  = 0x54;
  *(unsigned short*)(buf+2)  = 0xb848;
  *(uintptr_t*)     (buf+4)  = (uintptr_t)data;
  *(unsigned char*) (buf+12) = 0x50;
  *(unsigned short*)(buf+13) = 0x5141;
  *(unsigned short*)(buf+15) = 0x5041;
  *(unsigned char*) (buf+17) = 0x52;
  *(unsigned char*) (buf+18) = 0x51;
  *(unsigned short*)(buf+19) = 0xb848;
  *(uintptr_t*)     (buf+21) = (uintptr_t)mkcl_dynamic_callback_execute;
  *(unsigned int*)  (buf+29) = (unsigned int)0x00d0ff48;/* leading null byte is overwritten */
  *(unsigned char*) (buf+32) = 0x59;
  *(unsigned char*) (buf+33) = 0x59;
  *(unsigned char*) (buf+34) = 0x5d;
  *(unsigned char*) (buf+35) = 0xc3;
  *(unsigned long*) (buf+36) = 0x90909090;
  *(uint64_t*)(buf+40) = 0x9090909090909090LL;

  return buf;
}

