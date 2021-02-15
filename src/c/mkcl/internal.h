/*
    internal.h -- Structures and functions that are not meant for the end user
*/
/*
    Copyright (c) 2001, Juan Jose Garcia Ripoll.
    Copyright (c) 2010-2016, Jean-Claude Beaudoin.

    MKCL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 3 of the License, or (at your option) any later version.

    See file './Copyright' for full details.
*/

#ifndef MKCL_INTERNAL_H
#define MKCL_INTERNAL_H

#if __unix || __MINGW32__ || cygwin
# include <unistd.h>
#endif

#if _POSIX_VERSION >= 200112L
# define MKCL_UNIX TRUE /* This is Unix enough for MKCL. */
# if _POSIX_THREADS >= 200112L
#  define MKCL_PTHREADS TRUE
# endif
#endif

#ifdef __cplusplus
extern "C" {
#endif

  /* -------------------------------------------------------------------- *
   *	FUNCTIONS, VARIABLES AND TYPES NOT FOR GENERAL USE		*
   * -------------------------------------------------------------------- */

  /* booting */
  extern int mkcl_init_alloc(void);
  extern void mkcl_init_system_properties(MKCL);
  extern void mkcl_init_all_symbols(MKCL);
  extern void mkcl_init_gentemp(MKCL);
  extern void mkcl_init_backq(MKCL);
  extern void mkcl_init_bignums(MKCL);
  extern void mkcl_init_big_registers(MKCL); /* Does not exist! */
  extern void mkcl_init_error(MKCL);
  extern void mkcl_init_file(MKCL);
  extern void mkcl_init_late_file(MKCL);
  extern void mkcl_init_macros(MKCL);
  extern void mkcl_init_number(MKCL);
  extern void mkcl_init_read(MKCL);
  extern void mkcl_init_call_stack_overflow_area(MKCL, char * const stack_mark_address);
  extern void mkcl_init_stacks(MKCL, mkcl_env, struct mkcl_thread_init_parameters *);
  extern void mkcl_init_unixsys(MKCL);
  extern void mkcl_init_early_unixint(MKCL);
  extern void mkcl_init_late_unixint(MKCL);
  extern void mkcl_init_unixtime(MKCL);
  extern void mkcl_init_early_threads(MKCL);
  extern void mkcl_init_late_threads(MKCL);
  extern void mkcl_init_env(MKCL, mkcl_env, struct mkcl_thread_init_parameters *);
  extern void mkcl_init_lib_LSP(MKCL, mkcl_object, mkcl_object);

  extern void mkcl_reset_stacks(MKCL);

  extern void mkcl_clean_up_alloc(MKCL);
  extern void mkcl_clean_up_system_properties(MKCL);
  extern void mkcl_clean_up_gentemp(MKCL);
  extern void mkcl_clean_up_unixsys(MKCL);
  extern void mkcl_clean_up_unixint(MKCL);
  extern void mkcl_clean_up_threads(MKCL);

  extern const mkcl_env _mkcl_alloc_env(MKCL);
  extern const mkcl_env _mkcl_alloc_raw_env(MKCL);
  extern void _mkcl_dealloc_env(MKCL);

  extern bool mkcl_early_boot;
  extern mkcl_jmp_buf mkcl_early_boot_error_handler;


  /* array.d */

  extern mkcl_object (* const mkcl_array_elem_accessor[])(__MKCL, mkcl_object array, mkcl_index i);
  extern mkcl_object (* const mkcl_array_elem_setter[])(__MKCL, mkcl_object array, mkcl_index i, mkcl_object val);


  /* alloc.d/alloc_2.d */

#define MKCL_COMPACT_OBJECT_EXTRA(x) ((void*)((x)->array.displaced))
  extern void _mkcl_set_max_heap_size(MKCL, mkcl_index new_size);



  /* compiler.d */

  struct mkcl_compiler_env {
    mkcl_object variables;	/* Variables, tags, functions, etc: the env. */
    mkcl_object macros;		/* Macros and function bindings */
    mkcl_word lexical_level;	/* =0 if toplevel form */
    mkcl_object constants;	/* Constants for this form */
    mkcl_object lex_env;		/* Lexical env. for eval-when */
    mkcl_index env_depth;
    mkcl_index env_size;
    int mode;
    bool coalesce;
    bool stepping;
  };

  typedef struct mkcl_compiler_env *mkcl_compiler_env_ptr;

  /* character.d */

#define MKCL_UCS_NONCHARACTER(c)				\
  (((c) >= 0xFDD0 && (c) <= 0xFDEF) ||				\
   (((c) & 0xFFFF) >= 0xFFFE && (((c) & 0xFFFF) <= 0xFFFF)))
#define MKCL_UCS_PRIVATE(c)			\
  (((c) >= 0xE000 && (c) <= 0xF8FF) ||		\
   ((c) >= 0xF0000 && (c) <= 0xFFFD) ||		\
   ((c) >= 0x100000 && (c) <= 0x10FFFD))
#define MKCL_UCS_HIGH_SURROGATE(c) ((c) >= 0xD800 && (c) <= 0xDBFF)
#define MKCL_UCS_LOW_SURROGATE(c) ((c) >= 0xDC00 && (c) <= 0xDFFF)


  /* interpreter.d */

#define MKCL_BUILD_TEMP_STACK_FRAME(env,name,frame)			\
  struct mkcl_temp_stack_frame frame;					\
  mkcl_object name = mkcl_temp_stack_frame_open(env, (mkcl_object)&frame, 0);

#ifdef MKCL_USE_VARARG_AS_POINTER
# define MKCL_TEMP_STACK_FRAME_FROM_VA_LIST(e,f,va) do {        \
    const mkcl_object __frame = (f);                            \
    __frame->frame.t = mkcl_t_temp_stack_frame;                 \
    __frame->frame.stack = 0;                                   \
    __frame->frame.env = (e);                                   \
    __frame->frame.size = va[0].narg;                           \
    __frame->frame.base = va[0].sp? va[0].sp :                  \
      (mkcl_object*)va[0].args;                                 \
  } while(0)
#else
# define MKCL_TEMP_STACK_FRAME_FROM_VA_LIST(e,f,va) do {	\
    const mkcl_object __frame = (f);                            \
    mkcl_index i, __nargs = va[0].narg;                         \
    mkcl_temp_stack_frame_open((e), __frame, __nargs);          \
    for (i = 0; i < __nargs; i++) {                             \
      __frame->frame.base[i] = mkcl_va_arg(va);                 \
    }                                                           \
  } while (0)
#endif

#ifdef MKCL_USE_VARARG_AS_POINTER
# define MKCL_TEMP_STACK_FRAME_VARARGS_BEGIN(env, narg,lastarg,frame)	\
  struct mkcl_temp_stack_frame __mkcl_frame;				\
  const mkcl_object frame = (mkcl_object)&__mkcl_frame;			\
  frame->frame.t = mkcl_t_temp_stack_frame;				\
  frame->frame.stack = 0;						\
  frame->frame.env = env;						\
  frame->frame.size = narg;						\
  if (narg < MKCL_C_ARGUMENTS_LIMIT) {					\
    va_list args;							\
    va_start(args, lastarg);						\
    frame->frame.base = (void*)args;					\
    va_end(args);                                                       \
  } else {								\
    frame->frame.base = env->temp_stack_top - narg;/* underflow? */	\
  }
# define MKCL_TEMP_STACK_FRAME_VARARGS_END(frame)	\
  /* No stack consumed, no need to close frame */
#else
# define MKCL_TEMP_STACK_FRAME_VARARGS_BEGIN(env,narg,lastarg,frame)	\
  struct mkcl_temp_stack_frame __mkcl_frame;				\
  const mkcl_object frame = (mkcl_object)&__mkcl_frame;			\
  frame->frame.t = mkcl_t_temp_stack_frame;				\
  frame->frame.env = env;						\
  frame->frame.size = narg;						\
  if (narg < MKCL_C_ARGUMENTS_LIMIT) {					\
    mkcl_index i;							\
    mkcl_object *p = frame->frame.base = env->values;			\
    va_list args;							\
    va_start(args, lastarg);						\
    while (narg--) {							\
      *p = va_arg(args, mkcl_object);					\
      ++p;								\
    }									\
    va_end(args);                                                       \
    frame->frame.stack = (void*)0x1;					\
  } else {								\
    frame->frame.base = env->temp_stack_top - narg;/* underflow? */	\
    frame->frame.stack = 0;						\
  }
# define MKCL_TEMP_STACK_FRAME_VARARGS_END(frame)	\
  /* No stack consumed, no need to close frame */
#endif

  extern mkcl_object _mkcl_bytecode_dispatch_vararg(MKCL, mkcl_narg narg, ...);
  mkcl_object _mkcl_bytecode_dispatch_f0(MKCL);
  mkcl_object _mkcl_bytecode_dispatch_f1(MKCL, mkcl_object x1);
  mkcl_object _mkcl_bytecode_dispatch_f2(MKCL, mkcl_object x1, mkcl_object x2);
  mkcl_object _mkcl_bytecode_dispatch_f3(MKCL, mkcl_object x1, mkcl_object x2, mkcl_object x3);
  mkcl_object _mkcl_bytecode_dispatch_f4(MKCL, mkcl_object x1, mkcl_object x2, mkcl_object x3, mkcl_object x4);

  extern mkcl_object _mkcl_bclosure_dispatch_vararg(MKCL, mkcl_narg narg, ...);
  mkcl_object _mkcl_bclosure_dispatch_f0(MKCL);
  mkcl_object _mkcl_bclosure_dispatch_f1(MKCL, mkcl_object x1);
  mkcl_object _mkcl_bclosure_dispatch_f2(MKCL, mkcl_object x1, mkcl_object x2);
  mkcl_object _mkcl_bclosure_dispatch_f3(MKCL, mkcl_object x1, mkcl_object x2, mkcl_object x3);
  mkcl_object _mkcl_bclosure_dispatch_f4(MKCL, mkcl_object x1, mkcl_object x2, mkcl_object x3, mkcl_object x4);

  mkcl_object mkcl_clone_bclosure(MKCL, mkcl_object c0, mkcl_object new_lex);

  /* ffi.d */

  enum mkcl_ffi_tag {
    MKCL_FFI_CHAR = 0,
    MKCL_FFI_UNSIGNED_CHAR,
    MKCL_FFI_BYTE,
    MKCL_FFI_UNSIGNED_BYTE,
    MKCL_FFI_SHORT,
    MKCL_FFI_UNSIGNED_SHORT,
    MKCL_FFI_INT,
    MKCL_FFI_UNSIGNED_INT,
    MKCL_FFI_LONG,
    MKCL_FFI_UNSIGNED_LONG,
    MKCL_FFI_LONG_LONG,
    MKCL_FFI_UNSIGNED_LONG_LONG,
    MKCL_FFI_POINTER_VOID,
    MKCL_FFI_CSTRING,
    MKCL_FFI_OBJECT, /* This tag must be the last one of the "integral" types. */
    MKCL_FFI_FLOAT,
    MKCL_FFI_DOUBLE,
    MKCL_FFI_LONG_DOUBLE,
#if 0
    /* The other C99 types. */
    MKCL_FFI_FLOAT_COMPLEX,
    MKCL_FFI_DOUBLE_COMPLEX,
    MKCL_FFI_LONG_DOUBLE_COMPLEX,
    MKCL_FFI_FLOAT_IMAGINARY,
    MKCL_FFI_DOUBLE_IMAGINARY,
    MKCL_FFI_LONG_DOUBLE_IMAGINARY,
#endif
    MKCL_FFI_VOID,
    MKCL_FFI_INT8_T = MKCL_FFI_BYTE,
    MKCL_FFI_UINT8_T = MKCL_FFI_UNSIGNED_BYTE,
    MKCL_FFI_INT16_T = MKCL_FFI_SHORT,
    MKCL_FFI_UINT16_T = MKCL_FFI_UNSIGNED_SHORT,
    MKCL_FFI_INT32_T  = MKCL_FFI_INT,
    MKCL_FFI_UINT32_T = MKCL_FFI_UNSIGNED_INT,
#if MKCL_LONG_BITS == 64
    MKCL_FFI_INT64_T = MKCL_FFI_LONG,
    MKCL_FFI_UINT64_T = MKCL_FFI_UNSIGNED_LONG
#else
    MKCL_FFI_INT64_T = MKCL_FFI_LONG_LONG,
    MKCL_FFI_UINT64_T = MKCL_FFI_UNSIGNED_LONG_LONG
#endif
  };
  /* enum MKCL_FFI_VOID is used as a limit marker in a number of locations, beware!
     Note also that the order of enums declared in enum mkcl_ffi_tag must
     carefully match the content of mkcl_foreign_type_table[] and mkcl_foreign_type_size[].
     JCB
   */


 /* These two fficall parameters are in bytes and should be multiples of 16. */
#define MKCL_FFICALL_ARGS_STAGING_AREA_INITIAL_SIZE 32
#define MKCL_FFICALL_ARGS_STAGING_AREA_GROWTH_INCREMENT 512 /* Exponential growth up to it, linear afterward. */


  union mkcl_ffi_values {
    char c;
    unsigned char uc;
    int8_t b;
    uint8_t ub;
    int i;
    unsigned int ui;
    short s;
    unsigned short us;
    long l;
    unsigned long ul;
    mkcl_int16_t i16;
    mkcl_uint16_t u16;
    mkcl_int32_t i32;
    mkcl_uint32_t u32;
    mkcl_int64_t i64;
    mkcl_uint64_t u64;
    mkcl_long_long_t ll;
    mkcl_ulong_long_t ull;
    unsigned long l2[2];
    void *pv;
    char *pc;
    mkcl_object o;
    float f;
    double d;
    long double ld;
  };

  struct mkcl_fficall {
    char *buffer_sp;
    size_t buffer_size;
    union mkcl_ffi_values output;
    struct mkcl_fficall_reg *registers;
    char * buffer;
    /* mkcl_object cstring; */
  };

  extern enum mkcl_ffi_tag mkcl_foreign_type_code(MKCL, mkcl_object type);
  extern enum mkcl_ffi_calling_convention mkcl_foreign_cc_code(MKCL, mkcl_object cc_type);
  extern struct mkcl_fficall * mkcl_fficall_prepare(MKCL, mkcl_object return_type, mkcl_object arg_types);
  extern void mkcl_fficall_push_bytes(MKCL, void *data, size_t bytes);
  extern void mkcl_fficall_push_int(MKCL, int word);
  extern void mkcl_fficall_align4(MKCL);
  extern void mkcl_fficall_align8(MKCL);
  extern void mkcl_fficall_align16(MKCL);

  extern struct mkcl_fficall_reg *mkcl_fficall_prepare_extra(MKCL, struct mkcl_fficall_reg *registers);
  extern void mkcl_fficall_push_arg(MKCL, union mkcl_ffi_values *data, enum mkcl_ffi_tag type);
  extern void mkcl_fficall_execute(MKCL, void *f_ptr, struct mkcl_fficall *fficall, enum mkcl_ffi_tag return_type);

  extern mkcl_object mkcl_make_foreign(MKCL, mkcl_object C_type, mkcl_index data_size);
  extern MKCL_API mkcl_object mkcl_allocate_foreign_data(MKCL, mkcl_object tag, mkcl_index size);
  extern MKCL_API char * mkcl_base_string_raw_pointer(MKCL, mkcl_object f);
  extern void* mkcl_dynamic_callback_make(MKCL, mkcl_object data);
#if MKCL_WINDOWS /* should be on Win32 only. JCB */
  extern void* mkcl_stdcall_dynamic_callback_make(MKCL, mkcl_object data);
#endif
  extern mkcl_object mkcl_foreign_ref_elt(MKCL, void *p, enum mkcl_ffi_tag type);
  extern void mkcl_foreign_set_elt(MKCL, void *p, enum mkcl_ffi_tag type, mkcl_object value);
  extern void * mkcl_foreign_raw_pointer(MKCL, mkcl_object f);
  extern mkcl_object mkcl_null_terminated_base_string(MKCL, mkcl_object s);

  static inline bool mkcl_foreignp(MKCL, mkcl_object x)
  {
    mkcl_type t = mkcl_type_of(x);
    return (t == mkcl_t_foreign);
  }

  /* load.d */

  /* The following mkcl_library_symbol() function must be called while holding the mt::+load-compile-lock+. */
  extern MKCL_API void * mkcl_library_symbol(MKCL, mkcl_object block, const char *symbol, bool lock);

  extern MKCL_API mkcl_object mkcl_library_open(MKCL, mkcl_object filename, bool force_reload);
  extern MKCL_API mkcl_object mkcl_library_error(MKCL, mkcl_object block);
  extern MKCL_API bool mkcl_library_close(MKCL, mkcl_object block);
  extern MKCL_API void mkcl_library_close_all(MKCL);


  /* file.d */

  enum mkcl_stream_flag {
    MKCL_STREAM_FORMAT_MASK = 0x0F,
    MKCL_STREAM_BINARY = 0,
    MKCL_STREAM_TEXT = 1,
    MKCL_STREAM_CR = 16, /* Text stream only */
    MKCL_STREAM_LF = 32, /* Text stream only */
    MKCL_STREAM_SIGNED_BYTES = 64, /* Binary stream only */
    MKCL_STREAM_LITTLE_ENDIAN = 128,
    MKCL_STREAM_C_STDIO_STREAM = 256,
    MKCL_STREAM_SEEKABLE = 512,
  };

  typedef int mkcl_stream_flag_set;

#define MKCL_BASIC_STREAM_P(strm) (mkcl_type_of(strm) == mkcl_t_stream && (strm)->stream.mode < mkcl_smm_synonym)
#define MKCL_COMPOSITE_STREAM_P(strm) (!MKCL_BASIC_STREAM_P(strm))
#define MKCL_STRING_OUTPUT_STREAM_STRING(strm) (strm)->stream.object0
#define MKCL_STRING_OUTPUT_STREAM_COLUMN(strm) (strm)->stream.int1
#define MKCL_STRING_INPUT_STREAM_STRING(strm) (strm)->stream.object0
#define MKCL_STRING_INPUT_STREAM_POSITION(strm) (strm)->stream.int0
#define MKCL_STRING_INPUT_STREAM_LIMIT(strm) (strm)->stream.int1
#define MKCL_TWO_WAY_STREAM_INPUT(strm) (strm)->stream.object0
#define MKCL_TWO_WAY_STREAM_OUTPUT(strm) (strm)->stream.object1
#define MKCL_SYNONYM_STREAM_SYMBOL(strm) (strm)->stream.object0
#define MKCL_SYNONYM_STREAM_STREAM(e,strm) mkcl_symbol_value(e, (strm)->stream.object0)
#define MKCL_BROADCAST_STREAM_LIST(strm) (strm)->stream.object0
#define MKCL_ECHO_STREAM_INPUT(strm) (strm)->stream.object0
#define MKCL_ECHO_STREAM_OUTPUT(strm) (strm)->stream.object1
#define MKCL_CONCATENATED_STREAM_LIST(strm) (strm)->stream.object0
#define MKCL_IO_STREAM_FILE(strm) ((strm)->stream.file.stream)
#define MKCL_IO_STREAM_COLUMN(strm) (strm)->stream.int1
#define MKCL_IO_STREAM_ELT_TYPE(strm) (strm)->stream.object0
#define MKCL_IO_STREAM_FILENAME(strm) (strm)->stream.object1
#define MKCL_IO_FILE_DESCRIPTOR(strm) (strm)->stream.file.descriptor
#define MKCL_IO_FILE_COLUMN(strm) (strm)->stream.int1
#define MKCL_IO_FILE_ELT_TYPE(strm) (strm)->stream.object0
#define MKCL_IO_FILE_FILENAME(strm) (strm)->stream.object1

  /* hash.d */
  extern void mkcl_extend_hashtable(MKCL, mkcl_object hashtable);
  extern struct mkcl_hashtable_entry * mkcl_search_hash_package(MKCL, mkcl_object key, mkcl_object hashtable);

  /* gfun.d, clos/kernel.lsp */

#define MKCL_GFUN_NAME(x) ((x)->instance.slots[0]) /* hardcoded from clos/kernel.lsp */
#define MKCL_GFUN_SPEC(x) ((x)->instance.slots[1]) /* hardcoded from clos/kernel.lsp */
#define MKCL_GFUN_COMB(x) ((x)->instance.slots[2]) /* hardcoded from clos/kernel.lsp */

  extern mkcl_object mkcl_FEnot_funcallable_vararg(MKCL, mkcl_narg narg, ...);
  extern mkcl_object mkcl_FEnot_funcallable_fixed();

  /* package.d */

  extern mkcl_object mkcl_find_symbol_nolock(MKCL, mkcl_object name, mkcl_object p, int *intern_flag);
  extern mkcl_object _mkcl_alloc_package(MKCL, mkcl_object name);

  /* print.d */

#define MKCL_PPRINT_QUEUE_SIZE			128
#define MKCL_PPRINT_INDENTATION_STACK_SIZE	256

  extern void mk_cl_write_object(MKCL, mkcl_object x, mkcl_object stream);

  /* global locks */

#if MKCL_WINDOWS

# define MKCL_THREAD_LIST_LOCK() EnterCriticalSection(&mkcl_core.thread_list_lock)
# define MKCL_THREAD_LIST_UNLOCK() LeaveCriticalSection(&mkcl_core.thread_list_lock)

# define MKCL_PACKAGE_LIST_LOCK() EnterCriticalSection(&mkcl_core.package_list_lock)
# define MKCL_PACKAGE_LIST_UNLOCK() LeaveCriticalSection(&mkcl_core.package_list_lock)

# define MKCL_PACKAGE_LOCK(p) EnterCriticalSection(&(p)->pack.lock)
# define MKCL_PACKAGE_UNLOCK(p) LeaveCriticalSection(&(p)->pack.lock)

#elif MKCL_PTHREADS

# include <pthread.h>

#define MKCL_THREAD_LIST_LOCK()					\
  (pthread_mutex_lock(&mkcl_core.thread_list_lock)		\
   && (mkcl_lose(env, "Failed in MKCL_THREAD_LIST_LOCK()"), 0))
#define MKCL_THREAD_LIST_UNLOCK()				\
  (pthread_mutex_unlock(&mkcl_core.thread_list_lock)		\
   && (mkcl_lose(env, "Failed in MKCL_THREAD_LIST_UNLOCK()"), 0))

#define MKCL_PACKAGE_LIST_LOCK()				\
  (pthread_mutex_lock(&mkcl_core.package_list_lock)		\
   && (mkcl_lose(env, "Failed in MKCL_PACKAGE_LIST_LOCK()"), 0))
#define MKCL_PACKAGE_LIST_UNLOCK()				\
  (pthread_mutex_unlock(&mkcl_core.package_list_lock)		\
   && (mkcl_lose(env, "Failed in MKCL_PACKAGE_LIST_UNLOCK()"), 0))

#define MKCL_PACKAGE_LOCK(p)				\
  (pthread_mutex_lock(&(p)->pack.lock)			\
   && (mkcl_lose(env, "Failed in MKCL_PACKAGE_LOCK()"), 0))
#define MKCL_PACKAGE_UNLOCK(p)					\
  (pthread_mutex_unlock(&(p)->pack.lock)			\
   && (mkcl_lose(env, "Failed in MKCL_PACKAGE_UNLOCK()"), 0))

#else
# error Incomplete thread support for this OS.
#endif



  /* read.d */
#define MKCL_RTABSIZE	MKCL_BASE_CHAR_CODE_LIMIT	/*  read table size  */

  /* time.d */

#if MKCL_WINDOWS
# define MKCL_UTC_time_to_universal_time(e, x)			\
   mkcl_plus(e, mkcl_make_int64_t(e, x), mkcl_core.Jan1st1970UT)
#else
# define MKCL_UTC_time_to_universal_time(e, x)			\
   mkcl_plus(e, mkcl_make_integer(e, x), mkcl_core.Jan1st1970UT)
#endif

  /* backq.d */

  extern int _mkcl_backq_car(MKCL, mkcl_object *px);


#if MKCL_PTHREADS

  /* threads.d */

# include <semaphore.h>

  extern mkcl_env mkcl_interrupted_thread_env;
  extern bool mkcl_interrupt_refused;
  extern bool mkcl_interrupt_forcefully;
  extern sem_t * mkcl_interrupted_thread_suspended;
  extern sem_t * mkcl_interrupted_thread_resumed;

  extern const pthread_mutexattr_t * mkcl_recursive_mutexattr;
  extern const pthread_mutexattr_t * mkcl_errorcheck_mutexattr;
  extern const pthread_mutexattr_t * mkcl_normal_mutexattr;

# include <signal.h>

  /* unixint.d */

  volatile extern int mkcl_terminal_signal_number;

  struct mkcl_signal_control
  {
    int installed;
    struct sigaction old_action; /* for chaining */
    int chainable; /* for chaining */
    sem_t sem_obj;
    sem_t * sem;
  };

# if __linux
#  define MKCL_SIGMAX 64 /* SIGRTMAX */
#  define MKCL_BASE_SIGMAX 31
# elif __FreeBSD__
#  define MKCL_SIGMAX 128 /* _SIG_MAXSIG ? */
#  define MKCL_BASE_SIGMAX 31
# endif

  extern struct mkcl_signal_control mkcl_signals[MKCL_SIGMAX + 1];

  extern void mkcl_create_signal_servicing_thread(MKCL, char * thread_cname, int sig, mkcl_object func_designator);

#elif MKCL_WINDOWS
# define MKCL_SIGMAX 0
# define MKCL_BASE_SIGMAX 0
#endif 



  /*************************************************/

#define MKCL_PI_D 3.14159265358979323846264338327950288
#define MKCL_PI_L 3.14159265358979323846264338327950288l
#define MKCL_PI2_D 1.57079632679489661923132169163975144
#define MKCL_PI2_L 1.57079632679489661923132169163975144l

  void mkcl_deliver_fpe(MKCL);

#ifdef __cplusplus
}
#endif

#endif /* MKCL_INTERNAL_H */
