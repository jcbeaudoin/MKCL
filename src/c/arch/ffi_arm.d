/* -*- mode: c -*- */
/*
    ffi_arm.c -- Nonportable component of the FFI
*/
/*
    Copyright (c) 2005, Juan Jose Garcia Ripoll.
    Copyright (c) 2020, Jean-Claude Beaudoin.

    MKCL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 3 of the License, or (at your option) any later version.

    See file '../../../Copyright' for full details.
*/

#include <mkcl/mkcl.h>
#include <mkcl/internal.h>
#include <string.h>
#include <sys/mman.h>


#define MAX_CORE_REGISTERS 4
#define MAX_VFP_REGISTERS 16

struct mkcl_fficall_reg {
  long r[MAX_CORE_REGISTERS];
  int core_register_count;
  union {
    float s[MAX_VFP_REGISTERS];
    double d[MAX_VFP_REGISTERS/2];
  } vfp;
  bool vfp_register_allocated[MAX_VFP_REGISTERS];
  int vfp_register_least_unallocated;
};

struct mkcl_fficall_reg *
mkcl_fficall_prepare_extra(MKCL, struct mkcl_fficall_reg *registers)
{
  if (registers == 0) {
    registers = mkcl_alloc(env, sizeof(*registers));
  }
  registers->core_register_count = 0;
  registers->vfp_register_least_unallocated = 0;
  {
    int i = 0;
    for (; i < MAX_VFP_REGISTERS; i++) registers->vfp_register_allocated[i] = FALSE;
  }
  return registers;
}

void
mkcl_fficall_push_arg(MKCL, union mkcl_ffi_values *data, enum mkcl_ffi_tag type)
{
  long word;
  struct mkcl_fficall *fficall = env->fficall;
  struct mkcl_fficall_reg *registers = fficall->registers;

  switch (type) {
  case MKCL_FFI_CHAR: word = data->c;	goto WORD_WIDE;
  case MKCL_FFI_UNSIGNED_CHAR: word = data->uc; goto WORD_WIDE;
  case MKCL_FFI_BYTE: word = data->b; goto WORD_WIDE;
  case MKCL_FFI_UNSIGNED_BYTE: word = data->ub; goto WORD_WIDE;
  case MKCL_FFI_SHORT: word = data->s; goto WORD_WIDE;
  case MKCL_FFI_UNSIGNED_SHORT: word = data->us; goto WORD_WIDE;
  case MKCL_FFI_INT:
  case MKCL_FFI_LONG:
  case MKCL_FFI_UNSIGNED_INT:
  case MKCL_FFI_UNSIGNED_LONG:
  case MKCL_FFI_POINTER_VOID:
  case MKCL_FFI_CSTRING:
  case MKCL_FFI_OBJECT:
    word = data->l;
  WORD_WIDE:
    if (registers->core_register_count < MAX_CORE_REGISTERS) {
      registers->r[registers->core_register_count++] = word;
    } else {
      mkcl_fficall_align4(env);
      mkcl_fficall_push_bytes(env, &word, sizeof(long));
    }
    break;
  case MKCL_FFI_LONG_DOUBLE:
  case MKCL_FFI_DOUBLE:
    if (registers->vfp_register_least_unallocated < (MAX_VFP_REGISTERS - 1)) {
      int least_unallocated_pair = registers->vfp_register_least_unallocated;
      if (least_unallocated_pair & 1) least_unallocated_pair++; /* round up to next pair */
      for (; (least_unallocated_pair < MAX_VFP_REGISTERS)
	     && (registers->vfp_register_allocated[least_unallocated_pair]
		 || registers->vfp_register_allocated[least_unallocated_pair + 1])
	     ; least_unallocated_pair += 2);
      if (least_unallocated_pair < MAX_VFP_REGISTERS) {
	registers->vfp.d[least_unallocated_pair / 2] = data->d;
	registers->vfp_register_allocated[least_unallocated_pair++] = TRUE;
	registers->vfp_register_allocated[least_unallocated_pair++] = TRUE;
	{
	  int i = least_unallocated_pair;

	  for (i++; (i < MAX_VFP_REGISTERS) && registers->vfp_register_allocated[i]; i++);
	  registers->vfp_register_least_unallocated = i;
	}
      } else {
	registers->vfp_register_least_unallocated = MAX_VFP_REGISTERS; /* this ends back-filling */
	mkcl_fficall_align8(env);
	mkcl_fficall_push_bytes(env, &data->d, sizeof(double));
      }
    } else {
      registers->vfp_register_least_unallocated = MAX_VFP_REGISTERS; /* this ends back-filling */
      mkcl_fficall_align8(env);
      mkcl_fficall_push_bytes(env, &data->d, sizeof(double));
    }
    break;
  case MKCL_FFI_FLOAT:
    if (registers->vfp_register_least_unallocated < MAX_VFP_REGISTERS) {
      int i = registers->vfp_register_least_unallocated;
      registers->vfp.s[i] = data->f;
      registers->vfp_register_allocated[i] = TRUE;
      for (i++; (i < MAX_VFP_REGISTERS) && registers->vfp_register_allocated[i]; i++);
      registers->vfp_register_least_unallocated = i;
    } else {
      mkcl_fficall_align4(env);
      mkcl_fficall_push_bytes(env, &data->f, sizeof(float));
    }
    break;
  case MKCL_FFI_UNSIGNED_LONG_LONG:
  case MKCL_FFI_LONG_LONG:
    if (registers->core_register_count < (MAX_CORE_REGISTERS - 1)) {
      registers->r[registers->core_register_count++] = data->l2[0];
      registers->r[registers->core_register_count++] = data->l2[1];
    } else {
      mkcl_fficall_align8(env);
      mkcl_fficall_push_bytes(env, &data->ull, sizeof(unsigned long long));
    }
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
  mkcl_word * const args_buffer = (mkcl_word *) fficall->buffer;
  char* stack_p;
  size_t bufsize;
  mkcl_index btop;
  struct mkcl_fficall_reg * regs = fficall->registers;

  mkcl_fficall_align16(env); /* Size of a cache line. */
  bufsize = fficall->buffer_sp - fficall->buffer;
  btop = bufsize / sizeof(mkcl_word);

  typedef long (*integral_ffunc)(long a0, long a1, long a2, long a3,
				 double d0, double d1, double d2, double d3,
				 double d4, double d5, double d6, double d7);

  typedef long long (*long_long_ffunc)(long a0, long a1, long a2, long a3,
				       double d0, double d1, double d2, double d3,
				       double d4, double d5, double d6, double d7);

  typedef void * (*ptr_ffunc)(long a0, long a1, long a2, long a3,
			      double d0, double d1, double d2, double d3,
			      double d4, double d5, double d6, double d7);

  typedef float (*float_ffunc)(long a0, long a1, long a2, long a3,
			       double d0, double d1, double d2, double d3,
			       double d4, double d5, double d6, double d7);

  typedef double (*double_ffunc)(long a0, long a1, long a2, long a3,
				 double d0, double d1, double d2, double d3,
				 double d4, double d5, double d6, double d7);

  typedef long double (*long_double_ffunc)(long a0, long a1, long a2, long a3,
					   double d0, double d1, double d2, double d3,
					   double d4, double d5, double d6, double d7);

  typedef void (*void_ffunc)(long a0, long a1, long a2, long a3,
			     double d0, double d1, double d2, double d3,
			     double d4, double d5, double d6, double d7);

  /* Save current stack pointer and then push stack based arguments. */
  {
    mkcl_word args_on_stack[btop]; /* VLA */

    for (; btop; btop--)
      args_on_stack[btop] = args_buffer[btop]; /* copy overflow args to the tip of the stack. */

    if (return_type <= MKCL_FFI_UNSIGNED_LONG) {
      fficall->output.l
	= ((integral_ffunc) f_ptr) (regs->r[0], regs->r[1], regs->r[2], regs->r[3],
				    regs->vfp.d[0], regs->vfp.d[1], regs->vfp.d[2], regs->vfp.d[3],
				    regs->vfp.d[4], regs->vfp.d[5], regs->vfp.d[6], regs->vfp.d[7]);
    } else if ((return_type == MKCL_FFI_POINTER_VOID)
	       || (return_type == MKCL_FFI_CSTRING)
	       ||  (return_type == MKCL_FFI_OBJECT)) {
      fficall->output.pv
	= ((ptr_ffunc) f_ptr) (regs->r[0], regs->r[1], regs->r[2], regs->r[3],
			       regs->vfp.d[0], regs->vfp.d[1], regs->vfp.d[2], regs->vfp.d[3],
			       regs->vfp.d[4], regs->vfp.d[5], regs->vfp.d[6], regs->vfp.d[7]);
    } else if (return_type == MKCL_FFI_FLOAT) {
      fficall->output.f
	= ((float_ffunc) f_ptr) (regs->r[0], regs->r[1], regs->r[2], regs->r[3],
				 regs->vfp.d[0], regs->vfp.d[1], regs->vfp.d[2], regs->vfp.d[3],
				 regs->vfp.d[4], regs->vfp.d[5], regs->vfp.d[6], regs->vfp.d[7]);
    } else if (return_type == MKCL_FFI_DOUBLE) {
      fficall->output.d
	= ((double_ffunc) f_ptr) (regs->r[0], regs->r[1], regs->r[2], regs->r[3],
				  regs->vfp.d[0], regs->vfp.d[1], regs->vfp.d[2], regs->vfp.d[3],
				  regs->vfp.d[4], regs->vfp.d[5], regs->vfp.d[6], regs->vfp.d[7]);
    } else if (return_type == MKCL_FFI_LONG_DOUBLE) {
      fficall->output.ld
	= ((long_double_ffunc) f_ptr) (regs->r[0], regs->r[1], regs->r[2], regs->r[3],
				       regs->vfp.d[0], regs->vfp.d[1], regs->vfp.d[2], regs->vfp.d[3],
				       regs->vfp.d[4], regs->vfp.d[5], regs->vfp.d[6], regs->vfp.d[7]);
    }
    else if (return_type == MKCL_FFI_VOID) {
      ((void_ffunc) f_ptr) (regs->r[0], regs->r[1], regs->r[2], regs->r[3],
			    regs->vfp.d[0], regs->vfp.d[1], regs->vfp.d[2], regs->vfp.d[3],
			    regs->vfp.d[4], regs->vfp.d[5], regs->vfp.d[6], regs->vfp.d[7]);
    }
    else if ((return_type == MKCL_FFI_LONG_LONG)
	     || (return_type == MKCL_FFI_UNSIGNED_LONG_LONG)) {
      fficall->output.ll
	= ((long_long_ffunc) f_ptr) (regs->r[0], regs->r[1], regs->r[2], regs->r[3],
				     regs->vfp.d[0], regs->vfp.d[1], regs->vfp.d[2], regs->vfp.d[3],
				     regs->vfp.d[4], regs->vfp.d[5], regs->vfp.d[6], regs->vfp.d[7]);
    }
    else {
      mkcl_FEerror(env, "Unknown C function return type", 0);
    }
  }
}


static const mkcl_base_string_object(mkcl_dynamic_callback_import_thread_name__obj_, "mkcl_dynamic_callback_execute");
static const mkcl_object mkcl_dynamic_callback_import_thread_name = (mkcl_object) &mkcl_dynamic_callback_import_thread_name__obj_;

static union mkcl_ffi_values
mkcl_dynamic_callback_execute(long i1, long i2, long i3, long i4,
			      double d0, double d1, double d2, double d3,
			      double d4, double d5, double d6, double d7,
			      mkcl_object cbk_info, char *arg_buffer)
{
  char stack_mark = 0;
  mkcl_object result;
  mkcl_env env = MKCL_ENV();
  mkcl_env imported_env = NULL;

  mkcl_object fun = MKCL_CAR(cbk_info);
  mkcl_object rtype = MKCL_CADR(cbk_info);
  mkcl_object argtypes = MKCL_CADDR(cbk_info);

  arg_buffer += sizeof(void *); /* Skip saved stack frame pointer */
  arg_buffer += sizeof(void *); /* Skip return address */

  if (env == NULL)
    {
      env = imported_env = mkcl_import_current_thread(mkcl_dynamic_callback_import_thread_name, mk_cl_Cnil, NULL, NULL);
      if (imported_env == NULL)
        /* In the normal case we always set errno = 0, so this is clearly an error situation. */
	{ union mkcl_ffi_values val; val.ll = 0; errno = ENOMEM; return val; }
    }

  MKCL_BUILD_TEMP_STACK_FRAME(env, frame, aux);

  for (; !mkcl_endp(env, argtypes); argtypes = MKCL_CDR(argtypes)) {
    enum mkcl_ffi_tag tag = mkcl_foreign_type_code(env, MKCL_CAR(argtypes));
    mkcl_index size = mkcl_fixnum_to_word(mk_si_size_of_foreign_elt_type(env, MKCL_CAR(argtypes)));
    result = mkcl_foreign_ref_elt(env, arg_buffer, tag);
    mkcl_temp_stack_frame_push(env, frame, result);
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
	mkcl_cleanup_thread_lisp_context(env);
      } MKCL_CATCH_ALL_END;
      thread->thread.status = mkcl_thread_done;
    }
  else
    result = mkcl_apply_from_temp_stack_frame(env, frame, fun);
  mkcl_temp_stack_frame_close(env, frame);

  {
    union mkcl_ffi_values output;
    enum mkcl_ffi_tag tag = mkcl_foreign_type_code(env, rtype);

    memset(&output, 0, sizeof(output));
    mkcl_foreign_set_elt(env, &output, tag, result);

    if (imported_env)
      mkcl_release_current_thread(imported_env);

    errno = 0; /* See above for the necessity of this. */
    return output;
  }
}

static void
mkcl_void_dynamic_callback_execute(long i1, long i2, long i3, long i4,
				   double d0, double d1, double d2, double d3,
				   double d4, double d5, double d6, double d7,
				   mkcl_object cbk_info, char *arg_buffer)
{
  (void) mkcl_dynamic_callback_execute(i1, i2, i3, i4, d0, d1, d2, d3, d4, d5, d6, d7, cbk_info, arg_buffer);
}

static mkcl_int8_t
mkcl_byte_dynamic_callback_execute(long i1, long i2, long i3, long i4,
				   double d0, double d1, double d2, double d3,
				   double d4, double d5, double d6, double d7,
				   mkcl_object cbk_info, char *arg_buffer)
{
  union mkcl_ffi_values val
    = mkcl_dynamic_callback_execute(i1, i2, i3, i4, d0, d1, d2, d3, d4, d5, d6, d7, cbk_info, arg_buffer);
  return val.b;
}

static short
mkcl_short_dynamic_callback_execute(long i1, long i2, long i3, long i4,
				   double d0, double d1, double d2, double d3,
				   double d4, double d5, double d6, double d7,
				   mkcl_object cbk_info, char *arg_buffer)
{
  union mkcl_ffi_values val
    = mkcl_dynamic_callback_execute(i1, i2, i3, i4, d0, d1, d2, d3, d4, d5, d6, d7, cbk_info, arg_buffer);
  return val.s;
}

static long
mkcl_long_dynamic_callback_execute(long i1, long i2, long i3, long i4,
				   double d0, double d1, double d2, double d3,
				   double d4, double d5, double d6, double d7,
				   mkcl_object cbk_info, char *arg_buffer)
{
  union mkcl_ffi_values val
    = mkcl_dynamic_callback_execute(i1, i2, i3, i4, d0, d1, d2, d3, d4, d5, d6, d7, cbk_info, arg_buffer);
  return val.l;
}

static long long
mkcl_long_long_dynamic_callback_execute(long i1, long i2, long i3, long i4,
				   double d0, double d1, double d2, double d3,
				   double d4, double d5, double d6, double d7,
				   mkcl_object cbk_info, char *arg_buffer)
{
  union mkcl_ffi_values val
    = mkcl_dynamic_callback_execute(i1, i2, i3, i4, d0, d1, d2, d3, d4, d5, d6, d7, cbk_info, arg_buffer);
  return val.ll;
}

static float
mkcl_float_dynamic_callback_execute(long i1, long i2, long i3, long i4,
				   double d0, double d1, double d2, double d3,
				   double d4, double d5, double d6, double d7,
				   mkcl_object cbk_info, char *arg_buffer)
{
  union mkcl_ffi_values val
    = mkcl_dynamic_callback_execute(i1, i2, i3, i4, d0, d1, d2, d3, d4, d5, d6, d7, cbk_info, arg_buffer);
  return val.f;
}

static double
mkcl_double_dynamic_callback_execute(long i1, long i2, long i3, long i4,
				   double d0, double d1, double d2, double d3,
				   double d4, double d5, double d6, double d7,
				   mkcl_object cbk_info, char *arg_buffer)
{
  union mkcl_ffi_values val
    = mkcl_dynamic_callback_execute(i1, i2, i3, i4, d0, d1, d2, d3, d4, d5, d6, d7, cbk_info, arg_buffer);
  return val.d;
}

static long double
mkcl_long_double_dynamic_callback_execute(long i1, long i2, long i3, long i4,
				   double d0, double d1, double d2, double d3,
				   double d4, double d5, double d6, double d7,
				   mkcl_object cbk_info, char *arg_buffer)
{
  union mkcl_ffi_values val
    = mkcl_dynamic_callback_execute(i1, i2, i3, i4, d0, d1, d2, d3, d4, d5, d6, d7, cbk_info, arg_buffer);
  return val.ld;
}




void *
mkcl_dynamic_callback_make(MKCL, mkcl_object data, enum mkcl_ffi_calling_convention cc_type)
{
  unsigned char * buf = mkcl_alloc_callback_block(env);
  unsigned char * ip = buf; /* the instruction pointer (ip) */
  union { unsigned char b[4]; void * p; unsigned long l; unsigned short s; } imm; /* a staging buffer for immediate data */

#define i(byte) *(ip++) = (byte)
#define immed_ptr(val_ptr) imm.p = (val_ptr); i(imm.b[0]); i(imm.b[1]); i(imm.b[2]); i(imm.b[3]);
#define immed16(val_short) imm.s = (val_short);	i(imm.b[0]); i(imm.b[1]);
#define immed32(val_long) imm.l = (val_long); i(imm.b[0]); i(imm.b[1]); i(imm.b[2]); i(imm.b[3]);
    

  enum mkcl_ffi_tag tag = mkcl_foreign_type_code(env, MKCL_CADR(data));
  void * fptr = 0;

  switch (tag) {
  case MKCL_FFI_CHAR:
  case MKCL_FFI_UNSIGNED_CHAR:
  case MKCL_FFI_BYTE:
  case MKCL_FFI_UNSIGNED_BYTE:
    fptr = mkcl_byte_dynamic_callback_execute;
    break;
  case MKCL_FFI_SHORT:
  case MKCL_FFI_UNSIGNED_SHORT:
    fptr = mkcl_short_dynamic_callback_execute;
    break;
  case MKCL_FFI_POINTER_VOID:
  case MKCL_FFI_OBJECT:
  case MKCL_FFI_CSTRING:
  case MKCL_FFI_INT:
  case MKCL_FFI_UNSIGNED_INT:
  case MKCL_FFI_LONG:
  case MKCL_FFI_UNSIGNED_LONG:
    fptr = mkcl_long_dynamic_callback_execute;
    break;
  case MKCL_FFI_LONG_LONG:
  case MKCL_FFI_UNSIGNED_LONG_LONG:
    fptr = mkcl_long_long_dynamic_callback_execute;
    break;
  case MKCL_FFI_DOUBLE:
    fptr = mkcl_double_dynamic_callback_execute;
    break;
  case MKCL_FFI_LONG_DOUBLE:
    fptr = mkcl_long_double_dynamic_callback_execute;
    break;
  case MKCL_FFI_FLOAT:
    fptr = mkcl_float_dynamic_callback_execute;
    break;
  case MKCL_FFI_VOID:
    fptr = mkcl_void_dynamic_callback_execute;
    break;
  default:
    mkcl_FEerror(env, "Invalid C function callback return type", 0);
  }

  i(0x7B); i(0x46);                   /*	mov	r3, pc        */
  i(0x03); i(0xF1); i(0x18); i(0x03); /*	add	r3, #24       */
  i(0x10); i(0xB5);                   /*	push	{r4, lr}      */
  i(0x1A); i(0x68);                   /*	ldr	r2, [r3]      */ /* pointer to trampoline */
  i(0x58); i(0x68);                   /*	ldr	r0, [r3, #4]  */ /* arg0 */
  i(0x69); i(0x46);                   /*	mov	r1, sp        */ /* arg1 */
  i(0x90); i(0x47);                   /*	blx	r2            */ /* call the callback trampoline */
  i(0x10); i(0xBD);                   /*	pop	{r4, pc}      */ /* return to caller. */
  i(0x00); i(0xBF);                   /*	nop                   */
  i(0x00); i(0xBF);                   /*	nop                   */
  i(0x00); i(0xBF);                   /*	.align	2             */
  immed_ptr(fptr); /* The code here above expect this pointer to be 24 bytes (0x18) after the first opcode. */
  immed_ptr(data); /* The code here above expect this pointer to be 4 bytes after the previous one. */

  {
    int rc = mprotect(buf, mkcl_core.pagesize, PROT_READ | /* PROT_WRITE | */ PROT_EXEC);
    if (rc)
      mkcl_FElibc_error(env, "mkcl_dynamic_callback_make() failed on mprotect()", 0);
  }

  return buf;
}
