/* -*- mode: c -*- */
/*
    ffi_x86_64_w64.c -- Nonportable component of the FFI for Microsoft Win64 on x86_64.
*/
/*
    Copyright (c) 2005, Juan Jose Garcia Ripoll.
    Copyright (c) 2010-2013, Jean-Claude Beaudoin

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
  case MKCL_FFI_SHORT: i = data->s; goto INT;
  case MKCL_FFI_UNSIGNED_SHORT: i = data->us; goto INT;
  case MKCL_FFI_INT: i = data->i; goto INT;
  case MKCL_FFI_UNSIGNED_INT: i = data->ui; goto INT;
  case MKCL_FFI_LONG: i = data->l; goto INT;
  case MKCL_FFI_UNSIGNED_LONG: i = data->ul; goto INT;
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


static uint64_t
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

#if 0
  printf("\nIn mkcl_dynamic_callback_execute(), i1 = %llx, i2 = %llx, i3 = %llx, i4 = %llx,\n\t cbk_info = %p, arg_buffer = %p.\n", i1, i2, i3, i4, cbk_info, arg_buffer); fflush(NULL);
#endif

  if (env == NULL)
    {
      env = imported_env = mkcl_import_current_thread(mkcl_dynamic_callback_import_thread_name, mk_cl_Cnil, NULL, NULL);
      if (imported_env == NULL)
        /* In the normal case we always set errno = 0, so this is clearly an error situation.
           Returning -1 is just to please the compiler. */
	{ errno = ENOMEM; return -1; }
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
  arg_buffer += 4*sizeof(int64_t); /* Skip the spill-over homes of the 4 register arguments */

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
	mkcl_cleanup_thread_lisp_context(env);
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
  {
    volatile uint64_t val = 0;
    switch (tag) {
    case MKCL_FFI_CHAR: val = output.c; break;
    case MKCL_FFI_UNSIGNED_CHAR: val = output.uc; break;
    case MKCL_FFI_BYTE: val = output.b; break;
    case MKCL_FFI_UNSIGNED_BYTE: val = output.ub; break;
    case MKCL_FFI_SHORT: val = output.s; break;
    case MKCL_FFI_UNSIGNED_SHORT: val = output.us; break;
    case MKCL_FFI_INT: val = output.i; break;
    case MKCL_FFI_UNSIGNED_INT: val = output.ui; break;
    case MKCL_FFI_LONG: val = output.l; break;
    case MKCL_FFI_UNSIGNED_LONG: val = output.ul; break;

    case MKCL_FFI_LONG_LONG:
    case MKCL_FFI_UNSIGNED_LONG_LONG:
    case MKCL_FFI_POINTER_VOID:
    case MKCL_FFI_CSTRING:
    case MKCL_FFI_OBJECT:
      val = output.ull; break;
    case MKCL_FFI_DOUBLE:
      asm("movsd (%0),%%xmm0" :: "a" (&output.d));
      break;
    case MKCL_FFI_FLOAT:
      asm("movss (%0),%%xmm0" :: "a" (&output.f));
      break;
    case MKCL_FFI_VOID:
      break;
    default:
      mkcl_FEerror(env, "Invalid C function callback return type", 0);
    }
    {
#if 0
      register uint64_t rax asm("rax");
      rax = val;
#endif
      asm __volatile__ ("mov %0,%%rax\n\t" :: "m" (val));
      return val;
    }
  }
}



void*
mkcl_dynamic_callback_make(MKCL, mkcl_object data)
{
  unsigned char * buf = mkcl_alloc_callback_block(env);
  unsigned char * ip = buf; /* the instruction pointer (ip) */
  union { unsigned char b[8]; void * p; long long ll; long l; } imm; /* a staging buffer for immediate data */

#define i(byte) *(ip++) = (byte)
#define immed_ptr(val_ptr) imm.p = (val_ptr);	\
  i(imm.b[0]); i(imm.b[1]); i(imm.b[2]); i(imm.b[3]); i(imm.b[4]); i(imm.b[5]); i(imm.b[6]); i(imm.b[7]);
    

  /* pushq   %rbp           */  i(0x55);                             /* build stack frame, step 1 of 2 */
  /* movq    %rsp, %rbp     */  i(0x48); i(0x89); i(0xe5);           /* build stack frame, step 2 of 2 */
  /* pushq   %rsp           */  i(0x54);                             /* push arg_list pointer */
  /* movq    <addr64>, %rax */  i(0x48); i(0xb8); immed_ptr(data);
  /* pushq   %rax           */  i(0x50);                             /* push data */   
  /* pushq   %r9            */  i(0x41); i(0x51);                    /* push arg3 */
  /* pushq   %r8            */  i(0x41); i(0x50);                    /* push arg2 */
  /* pushq   %rdx           */  i(0x52);                             /* push arg1 */
  /* pushq   %rcx           */  i(0x51);                             /* push arg0 */
  /* movq    <addr64>, %rax */  i(0x48); i(0xb8); immed_ptr(mkcl_dynamic_callback_execute);
  /* callq   *%rax          */  i(0x48); i(0xff); i(0xd0);           /* call mkcl_dynamic_callback_execute() */
  /* addq    $48, %rsp      */  i(0x48); i(0x83); i(0xc4); i(0x30);  /* cleanup arg list of previous call, 48 bytes. */
  /* leave                  */  i(0xc9);                             /* undo stack frame */
  /* ret                    */  i(0xc3);                             /* return */
  /* nop                    */  i(0x90);  /* Fill with nop until end of I-cache line (multiple of 16 bytes). */
  /* nop                    */  i(0x90);
  /* nop                    */  i(0x90);
  /* nop                    */  i(0x90);
  /* nop                    */  i(0x90);
  /* nop                    */  i(0x90);
  /* nop                    */  i(0x90);
  /* nop                    */  i(0x90);

  /*
   * we could also adjust the SP register this way
   * instead of pushing the 4 argument registers.
   *
   *    subq    $0x20,  %rsp            48 83 EC 20
   *
   */

  { /* By default on Win64 data is PAGE_READWRITE only and we would get
       an ACCESS_VIOLATION if we didn't set it to EXECUTE. */
    DWORD old_protection_flags;
    BOOL ok = VirtualProtect(buf, mkcl_core.pagesize, PAGE_EXECUTE_READ, &old_protection_flags);
    
    if (!ok)
      mkcl_FEwin32_error(env, "mkcl_dynamic_callback_make() failed on VirtualProtect()", 0);
  }

#if 0
  printf("\nIn mkcl_dynamic_callback_make(), data = %p, returning = %p.\n", data, buf); fflush(NULL);
#endif
  return buf;
}

