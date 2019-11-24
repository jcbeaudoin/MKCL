/* -*- mode: c -*- */
/*
    threads.d -- Posix threads.
*/
/*
    Copyright (c) 2003, Juan Jose Garcia Ripoll.
    Copyright (c) 2010-2019, Jean-Claude Beaudoin.

    MKCL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 3 of the License, or (at your option) any later version.

    See file '../../Copyright' for full details.
*/

#include <mkcl/mkcl.h>
#include <mkcl/mkcl-gc.h>
#include <mkcl/mkcl-math.h>
#include <mkcl/mkcl-inl.h>
#include <mkcl/internal.h>

#include <string.h>
#include <errno.h>
#include <stdio.h>
#include <time.h>
#include <stdlib.h>
#include <signal.h>

#ifdef HAVE_GETTIMEOFDAY
# include <sys/time.h>
#endif
#ifdef HAVE_SCHED_YIELD
# include <sched.h>
#endif

#if MKCL_PTHREADS
/* This _true_pthread_join gives you direct access to the real pthread_join, not the Boehm's GC wrapped one. */
static int _true_pthread_join(pthread_t thread, void ** retval);
#endif /* MKCL_PTHREADS */

static void setup_thread_bindings(MKCL, mkcl_object initial_bindings);


#if MKCL_WINDOWS
char * mkcl_handle_debug_name(MKCL, char * prefix)
{
  static unsigned long count = 0;
  static const char * const format = "MKCL(%lu): %s %lu";
  DWORD pid = GetCurrentProcessId();
  const int sizeof_name = sizeof(format) + 12 + strlen(prefix) + 12;
  char * name = mkcl_alloc_atomic(env, sizeof_name);

  snprintf(name, sizeof_name, format, pid, prefix, count++);
  return name;
}
#endif

#if MKCL_WINDOWS
static DWORD cl_env_key;
#else
static pthread_key_t cl_env_key;
#endif

const mkcl_env mkcl_thread_env(void)
{
#if MKCL_WINDOWS
  return TlsGetValue(cl_env_key);
#else
  return pthread_getspecific(cl_env_key);
#endif
}

static void
mkcl_set_thread_env(MKCL)
{
#if MKCL_WINDOWS
  if (!TlsSetValue(cl_env_key, env))
    mkcl_FEwin32_error(env, "mkcl_set_thread_env failed on TlsSetValue", 0);
#else
  if (pthread_setspecific(cl_env_key, env))
    mkcl_FElibc_error(env, "mkcl_set_thread_env failed on pthread_setspecific()", 0);
#endif
}


/*----------------------------------------------------------------------
 */


static inline mkcl_object
mkcl_current_thread(MKCL)
{
  return env->own_thread;
}

mkcl_object mk_mt_current_thread(MKCL)
{
  @(return mkcl_current_thread(env));
}

/*----------------------------------------------------------------------
 * THREAD OBJECT
 */

inline static void
mkcl_assert_type_thread(MKCL, mkcl_object o)
{
  if (mkcl_type_of(o) != mkcl_t_thread)
    mkcl_FEwrong_type_argument(env, @'mt::thread', o);
}

static void
thread_final_cleanup(MKCL, mkcl_object thread)
{
  /* This routine performs some cleanup before a thread is completely
   * done. For instance, it has to remove the associated thread
   * object from the list, and it has to dealloc some memory.
   */
  /* const mkcl_env env = thread->thread.env; */

  thread->thread.status = mkcl_thread_done;
  env->disable_interrupts = 2; /* This prevents any interrupts, even forced. */
#if MKCL_DEBUG_INTERRUPT_MASK
  env->interrupt_disabler_lineno = __LINE__;
  env->interrupt_disabler_file = __FILE__;
#endif
  mkcl_reset_stacks(env);
  env->fpe_control_bits = 0;

  mkcl_set_thread_env(NULL);

  if (thread->thread.detached
      || thread->thread.result_value == @':imported'
      || thread->thread.result_value == @':imported-and-gc-registered')
    {
      mkcl_remove_thread_from_global_thread_list(env, thread);
    }

#if MKCL_PTHREADS
  if (thread->thread.running_lock) /* should not be here */
    {
      pthread_mutex_destroy(thread->thread.running_lock);
      thread->thread.running_lock = NULL;
    }
  sigemptyset(&thread->thread.saved_sigmask);
#endif

  /* cleanup the private state of the thread */
  thread->thread.interrupt = mk_cl_Cnil;
  thread->thread.sigmask_frs_marker = NULL;
  thread->thread.resume_handler_ran = FALSE;
  thread->thread.interrupt_count = 0;
  {
    int i, j;

    for (i = 0; i < MKCL_MAX_INTERRUPTS; i++)
      {
	thread->thread.interrupted_threads[i].thread_ident = 0;

	thread->thread.interrupted_threads[i].cs_org = NULL;
	thread->thread.interrupted_threads[i].cs_limit = NULL;
	thread->thread.interrupted_threads[i].cs_size = 0;
	thread->thread.interrupted_threads[i].cs_overflow_size = 0;
	thread->thread.interrupted_threads[i].cs_overflowing = FALSE;

	thread->thread.interrupted_threads[i].nvalues = 0;
	for (j = 0; j < MKCL_MULTIPLE_VALUES_LIMIT; j++)
	  thread->thread.interrupted_threads[i].values[j] = mk_cl_Cnil;
      }
  }
}

#if MKCL_WINDOWS
# if 0
/* For use with direct call to CreateThread */
typedef DWORD thread_value_t;
#  define CALL_CONV WINAPI
# else
/* For calls to _beginthreadex */
typedef unsigned thread_value_t;
#  define CALL_CONV __stdcall
# endif
#elif MKCL_PTHREADS
typedef void * thread_value_t;
# define CALL_CONV
#endif

static void push_on_global_thread_list(MKCL, mkcl_object thread)
{
  volatile bool locked = false;

  MKCL_UNWIND_PROTECT_BEGIN(env) {
    MKCL_LIBC_NO_INTR(env, (MKCL_THREAD_LIST_LOCK(), locked = true));
    mkcl_core.threads = MKCL_CONS(env, thread, mkcl_core.threads);
  } MKCL_UNWIND_PROTECT_EXIT {
    if (locked) MKCL_THREAD_LIST_UNLOCK();
  } MKCL_UNWIND_PROTECT_END;
}

void mkcl_remove_thread_from_global_thread_list(MKCL, mkcl_object thread)
{
  volatile bool locked = false;
  
  MKCL_UNWIND_PROTECT_BEGIN(env) {
    MKCL_LIBC_NO_INTR(env, (MKCL_THREAD_LIST_LOCK(), locked = true));
    mkcl_core.threads = mkcl_remove_eq(env, thread, mkcl_core.threads);
  } MKCL_UNWIND_PROTECT_EXIT {
    if (locked) MKCL_THREAD_LIST_UNLOCK();
  } MKCL_UNWIND_PROTECT_END;
}


void mkcl_register_thread_as_active(MKCL, mkcl_object thread)
{
  thread->thread.status = mkcl_thread_active;
  push_on_global_thread_list(env, thread);
}

void mkcl_setup_thread_lisp_context(MKCL, char * const stack_mark_address)
{
  mkcl_object thread = env->own_thread;

  mkcl_set_thread_env(env);
  thread->thread.tid = mkcl_gettid();
  mkcl_init_call_stack_overflow_area(env, stack_mark_address);
  
  mk_si_clear_all_fpe(env);
  mk_si_enable_fpe(env, @':default');
  
  if (!mkcl_Null(thread->thread.initial_bindings))
    setup_thread_bindings(env, thread->thread.initial_bindings);
  
  mkcl_bds_bind(env, @'mt::*thread*', thread);
  mkcl_bds_bind(env, @'mkcl::*current-working-directory*', mk_cl_Cnil);
  mkcl_bds_bind(env, @'mkcl::*all-current-working-directories*', mk_cl_Cnil);
  mkcl_bds_bind(env, @'si::*dynamic-cons-stack*', mk_cl_Cnil);
  mk_si_trim_dynamic_cons_stack(env);
}

void mkcl_cleanup_thread_lisp_context(MKCL)
{
#if 0
  mkcl_bds_unwind1(env); /* si::*dynamic-cons-stack* */
  mkcl_bds_unwind1(env); /* mkcl::*all-current-working-directories* */
  mkcl_bds_unwind1(env); /* mkcl::*current-working-directory* */
  mkcl_bds_unwind1(env); /* mt::*thread* */
#endif
}

mkcl_object
mkcl_top_apply(MKCL, mkcl_object function, mkcl_object args)
{
  static mkcl_object top_apply_fun = mk_cl_Cnil;
  mkcl_object value;

  if (mkcl_Null(top_apply_fun))
    {
      top_apply_fun = MKCL_SYM_FUN(@'si::top-apply');
      if (mkcl_Null(top_apply_fun))
	value = mk_cl_apply(env, 2, function, args);
      else
	value = mkcl_funcall2(env, top_apply_fun, function, args);
    }
  else
    value = mkcl_funcall2(env, top_apply_fun, function, args);
  return value;
}

static thread_value_t CALL_CONV thread_entry_point(void *arg)
{
  char stack_mark = 0;
  mkcl_object thread = (mkcl_object)arg;
  const mkcl_env env = thread->thread.env;
  thread_value_t status = (thread_value_t) MKCL_THREAD_NORMAL_EXIT;

  if (env->own_thread != thread) return (thread_value_t) MKCL_THREAD_ABORTED;

#if MKCL_PTHREADS
  if (pthread_mutex_lock(thread->thread.running_lock)) return (thread_value_t) MKCL_THREAD_UNKNOWN_ERROR;
  /* Insert some private os-level thread initialization here */
  if (pthread_mutex_unlock(thread->thread.running_lock)) return (thread_value_t) MKCL_THREAD_UNKNOWN_ERROR;
  if (pthread_mutex_destroy(thread->thread.running_lock)) return (thread_value_t) MKCL_THREAD_UNKNOWN_ERROR;
  thread->thread.running_lock = NULL;
#endif

  /*    The CATCH_ALL point provides us with an elegant way to
   *    exit the thread: we just do an unwind to frs_org.
   */
  MKCL_CATCH_ALL_BEGIN(env) {
    mkcl_object value;

    MKCL_SETUP_CALL_STACK_ROOT_GUARD(env);

    /* 1) Setup the environment for the execution of the thread */
    mkcl_setup_thread_lisp_context(env, &stack_mark);

    mkcl_register_thread_as_active(env, thread);
    mkcl_enable_interrupts(env);

    /* 2) Execute the code. */
    value = mkcl_top_apply(env, thread->thread.function, thread->thread.args);

    mkcl_cleanup_thread_lisp_context(env);
    if (thread->thread.result_value == MKCL_OBJNULL)
      thread->thread.result_value = value;
    status = (thread_value_t) MKCL_THREAD_NORMAL_EXIT;
    MKCL_UNSET_CALL_STACK_ROOT_GUARD(env);
  } MKCL_CATCH_ALL_IF_CAUGHT {
    MKCL_UNSET_CALL_STACK_ROOT_GUARD(env);
    /* mkcl_bds_unwind1(env); */
    /* mkcl_bds_unwind1(env); */
    /* mkcl_bds_unwind1(env); */
    /* mkcl_bds_unwind1(env); */
  } MKCL_CATCH_ALL_END;
  status = (thread_value_t) mkcl_exit_status(env);
  thread->thread.status = mkcl_thread_done;

  /* 3) Exit the thread through this point. */
  thread_final_cleanup(env, thread);
  return status;
}

#if MKCL_PTHREADS

static void * signal_servicing_thread_entry_point(void *arg)
{
  char stack_mark = 0;
  mkcl_object thread = MKCL_CAR((mkcl_object) arg);
  const mkcl_env env = thread->thread.env;
  int sig = mkcl_fixnum_to_word(MKCL_CDR((mkcl_object) arg));

  if (env->own_thread != thread) return (void *) MKCL_THREAD_ABORTED;

  if (pthread_mutex_lock(thread->thread.running_lock)) return (thread_value_t) MKCL_THREAD_UNKNOWN_ERROR;
  /* Insert some private thread initialization here */
  if (pthread_mutex_unlock(thread->thread.running_lock)) return (thread_value_t) MKCL_THREAD_UNKNOWN_ERROR;
  if (pthread_mutex_destroy(thread->thread.running_lock)) return (thread_value_t) MKCL_THREAD_UNKNOWN_ERROR;
  thread->thread.running_lock = NULL;

  /* The CATCH_ALL point provides us with an elegant way
   * to exit the thread: we just do an unwind to frs_org.
   */
  MKCL_CATCH_ALL_BEGIN(env) {
    MKCL_SETUP_CALL_STACK_ROOT_GUARD(env);

    /* 1) Setup the environment for the execution of the thread */
#if 1
    mkcl_setup_thread_lisp_context(env, &stack_mark);
#else
    mkcl_set_thread_env(env);
    thread->thread.tid = mkcl_gettid();
    mkcl_init_call_stack_overflow_area(env, &stack_mark);

    mk_si_clear_all_fpe(env);
    mk_si_enable_fpe(env, @':default');
    
    if (!mkcl_Null(thread->thread.initial_bindings))
      setup_thread_bindings(env, thread->thread.initial_bindings);
    
    mkcl_bds_bind(env, @'mt::*thread*', thread);
    mkcl_bds_bind(env, @'mkcl::*current-working-directory*', mk_cl_Cnil);
    mkcl_bds_bind(env, @'mkcl::*all-current-working-directories*', mk_cl_Cnil);
    mkcl_bds_bind(env, @'si::*dynamic-cons-stack*', mk_cl_Cnil);
    mk_si_trim_dynamic_cons_stack(env);
#endif

    mkcl_register_thread_as_active(env, thread);
    mkcl_enable_interrupts(env);

    /* 2) Execute the code. */
    for (;;)
      {
	int rc = 0;
	int received_signo;

	do {
	  MKCL_LIBC_Zzz(env, @':io', rc = sem_wait(mkcl_signals[sig].sem)); /* a sleeping point */
	} while ( rc && errno == EINTR );
	if ( rc ) mkcl_C_lose(env, "signal_servicing_thread_entry_point failed on sem_wait");
	mk_mt_test_for_thread_shutdown(env);

	received_signo = ((sig == 0) ? mkcl_terminal_signal_number : sig);
	mkcl_top_apply(env, thread->thread.function, MKCL_CONS(env, MKCL_MAKE_FIXNUM(received_signo), mk_cl_Cnil));
      }
    mkcl_cleanup_thread_lisp_context(env);
    MKCL_UNSET_CALL_STACK_ROOT_GUARD(env);
  } MKCL_CATCH_ALL_IF_CAUGHT {
    MKCL_UNSET_CALL_STACK_ROOT_GUARD(env);
    /* mkcl_bds_unwind1(env); */
    /* mkcl_bds_unwind1(env); */
    /* mkcl_bds_unwind1(env); */
    /* mkcl_bds_unwind1(env); */
    /* We could add some special action here to help stack unwinding
       but we don't need to do anything yet. */
  } MKCL_CATCH_ALL_END;
  thread->thread.status = mkcl_thread_done;

  thread_final_cleanup(env, thread);
  return NULL;
}

#endif /* MKCL_PTHREADS */

#if MKCL_PTHREADS
static pthread_mutex_t mkcl_interrupt_thread_lock;
static sigset_t mkcl_standard_sigmask;
#elif MKCL_WINDOWS
static HANDLE mkcl_interrupt_thread_lock;
#endif


#if MKCL_PTHREADS

static sem_t mkcl_run_interrupt_function_sem_obj;
static sem_t * mkcl_run_interrupt_function = &mkcl_run_interrupt_function_sem_obj;

static void *
interrupt_thread_entry_point(void *arg)
{
  char stack_mark = 0;
  int rc = 0;
  mkcl_object thread = (mkcl_object)arg;
  const mkcl_env env = thread->thread.env;

  if (env->own_thread != thread) return (void *) MKCL_THREAD_ABORTED;

  pthread_sigmask(SIG_SETMASK, &mkcl_standard_sigmask, NULL);

#if 1
  {
    struct timespec timeout;

    rc = clock_gettime(CLOCK_REALTIME, &timeout);
    if (rc)
      mkcl_FElibc_error(env, "interrupt_thread_entry_point() failed on clock_gettime", 0);

    timeout.tv_nsec += 5 * 1000000; /* 5ms */
    if (timeout.tv_nsec >= 1000000000) {
      timeout.tv_nsec -= 1000000000;
      timeout.tv_sec++;
    }

    do {
      rc = sem_timedwait(mkcl_run_interrupt_function, &timeout);
    } while ( rc && errno == EINTR );
    if ( rc )
      {
	if ( errno == ETIMEDOUT )
	  { fputs("\n;; MKCL internal error: interrupt synchronization timedout!\n", stderr); }
	else mkcl_C_lose(env, "mk_mt_interrupt_thread failed on sem_timedwait");
      }
  }
#else
  do {
    rc = sem_wait(mkcl_run_interrupt_function);
  } while ( rc && errno == EINTR );
  if ( rc ) mkcl_C_lose(env, "interrupt_thread_entry_point failed on sem_wait");
#endif

  /* The CATCH_ALL point provides us with an elegant way to
   * exit the thread: we just do an unwind to frs_org.
   */
  MKCL_CATCH_ALL_BEGIN(env) {
    /* 1) Setup the environment for the execution of the thread */
    mkcl_set_thread_env(env);
    mkcl_init_call_stack_overflow_area(env, &stack_mark);
    mk_si_clear_all_fpe(env);
    mkcl_reactivate_fpe_set(env);

    mkcl_enable_interrupts(env);
    /* 2) Execute the code. */
    mkcl_funcall0(env, thread->thread.interrupt);
    mkcl_disable_interrupts(env);
    env->nlj_fr = NULL; /* make sure to signal a normal exit */
#if 0
  } MKCL_CATCH_ALL_IF_CAUGHT {
    /* We could add some special action here to help stack unwinding
       but we don't need to do anything yet. */
#endif
  } MKCL_CATCH_ALL_END;

  if (pthread_mutex_lock(&mkcl_interrupt_thread_lock))
    mkcl_lose(env, "interrupt_thread_entry_point failed on pthread_mutex_lock");

  {
    int rc = 0;
    int sig = mkcl_get_option(MKCL_OPT_THREAD_RESUME_SIGNAL);
    int count = thread->thread.interrupt_count - 1;
    struct interrupted_thread_ctrl * p = &(thread->thread.interrupted_threads[count]);
	  
    thread->thread.thread = p->thread_ident;

    env->cs_org = p->cs_org;
    env->cs_limit = p->cs_limit;
    env->cs_size = p->cs_size;
    env->cs_overflow_size = p->cs_overflow_size;
    env->cs_overflowing = p->cs_overflowing;

    if ( env->nlj_fr == NULL )
      { /* This is a normal (not unwinding) return. */
	env->disable_interrupts = p->disable_interrupts;
#if MKCL_DEBUG_INTERRUPT_MASK	
	env->interrupt_disabler_file = p->interrupt_disabler_file;
	env->interrupt_disabler_lineno = p->interrupt_disabler_lineno;
#endif
      }

    /* Restore interrupted thread values. */
    env->nvalues = p->nvalues;
    {
      int j;
      const int max = p->nvalues;
      mkcl_object * const values = p->values;
      mkcl_object * const env_values = env->values;
      
      for (j = 0; j < max; j++) env_values[j] = values[j];
    }

    thread->thread.interrupt_count = count; /* interrupted_threads stack poped */

    mkcl_interrupted_thread_env = env;
    env->own_thread->thread.resume_handler_ran = FALSE;

    /* resume the interrupted thread */
    if ((rc = pthread_kill(thread->thread.thread, sig)))
      switch (rc)
	{
	case ESRCH: /* the interrupted thread died on us unexpectedly! */
	  /* Should we flag this untimely death or simply wimper away? */
	  /* Let's be quiet for now. */
	  break;
	case EINVAL:
	  mkcl_lose(env, "interrupt_thread_entry_point: invalid signal used with pthread_kill");
	  break;
	default:
	  mkcl_lose(env, "interrupt_thread_entry_point failed on pthread_kill");
	  break;
	}
    else
      {
	do {
	  rc = sem_wait(mkcl_interrupted_thread_resumed);
	} while ( rc && errno == EINTR );
	if ( rc ) mkcl_C_lose(env, "interrupt_thread_entry_point failed on sem_wait");
      }
    mkcl_interrupted_thread_env = NULL;
  }

  if (pthread_mutex_unlock(&mkcl_interrupt_thread_lock))
    mkcl_lose(env, "interrupt_thread_entry_point failed on pthread_mutex_unlock");

  pthread_detach(pthread_self()); /* no join for interrupt threads. */

  return NULL;
}

#elif MKCL_WINDOWS

static void resume_thread_unwinding(void)
{ /* This function is never called through normal means.
     Its call is the result a direct instruction pointer
     manipulation through the "CONTEXT" structure.
  */
  /* In this function we cannot assume that we have a valid
     stack frame to work with, so no automatic variable allowed!
  */
  const mkcl_env env = MKCL_ENV();

  mkcl_unwind(env, env->nlj_fr);
  /* We should never return from the line just above! */
  mkcl_lose(env, "resume_thread_unwinding failed!");
}

static thread_value_t CALL_CONV interrupt_thread_entry_point(void * arg)
{
  char stack_mark = 0;
  mkcl_object thread = (mkcl_object)arg;
  const mkcl_env env = thread->thread.env;

  if (env->own_thread != thread) return MKCL_THREAD_ABORTED;

  /* The CATCH_ALL point is the destination provides us with an elegant way
   * to exit the thread: we just do an unwind up to frs_top.
   */
  MKCL_CATCH_ALL_BEGIN(env) {
    /* 1) Setup the environment for the execution of the thread */
    mkcl_set_thread_env(env);
    mkcl_init_call_stack_overflow_area(env, &stack_mark);
    
    mk_si_clear_all_fpe(env);
    mkcl_reactivate_fpe_set(env);
    
    mkcl_enable_interrupts(env);
    /* 2) Execute the code. */
    mkcl_funcall0(env, thread->thread.interrupt);
    mkcl_disable_interrupts(env);
    env->nlj_fr = NULL; /* make sure to signal a normal exit */
#if 0
  } MKCL_CATCH_ALL_IF_CAUGHT {
    /* We could add some special action here to help stack unwinding
       but we don't need to do anything yet. */
#endif
  } MKCL_CATCH_ALL_END;

  {
    DWORD wait_val;

    MKCL_LIBC_NO_INTR(env, wait_val = WaitForSingleObject(mkcl_interrupt_thread_lock, INFINITE));
    switch (wait_val)
      {
      case WAIT_OBJECT_0: break;
      case WAIT_TIMEOUT:
      case WAIT_ABANDONED:
      case WAIT_FAILED:
      default:
	mkcl_FEwin32_error(env, "interrupt-thread failed to acquire lock", 0);
      }
  }

  {
    HANDLE os_thread;
    HANDLE old_os_thread;
    int count = thread->thread.interrupt_count - 1;
    struct interrupted_thread_ctrl * p = &(thread->thread.interrupted_threads[count]);
	  
    old_os_thread = thread->thread.thread;
    if (!CloseHandle(old_os_thread))
      mkcl_FEwin32_error(env, "Cannot call CloseHandle to dispose of old interrupt thread for thread ~A", 1, thread);
    os_thread = thread->thread.thread = p->thread_ident;
    p->thread_ident = NULL;

    env->cs_org = p->cs_org;
    env->cs_limit = p->cs_limit;
    env->cs_size = p->cs_size;
    env->cs_overflow_size = p->cs_overflow_size;
    env->cs_overflowing = p->cs_overflowing;

    if ( env->nlj_fr == NULL )
      { /* This is a normal (not unwinding) return. */
	env->disable_interrupts = p->disable_interrupts;
#if MKCL_DEBUG_INTERRUPT_MASK	
	env->interrupt_disabler_file = p->interrupt_disabler_file;
	env->interrupt_disabler_lineno = p->interrupt_disabler_lineno;
#endif
      }

    /* Restore interrupted thread values. */
    env->nvalues = p->nvalues;
    {
      int j;
      const int max = p->nvalues;
      mkcl_object * const values = p->values;
      mkcl_object * const env_values = env->values;
      
      for (j = 0; j < max; j++) env_values[j] = values[j];
    }
    
    thread->thread.interrupt_count = count; /* interrupted_threads stack poped */

    env->own_thread->thread.resume_handler_ran = FALSE; /* needed? No. */

    if ( env->nlj_fr != NULL )
      {
	CONTEXT context;
	context.ContextFlags = CONTEXT_CONTROL | CONTEXT_INTEGER;
	if (!GetThreadContext(os_thread, &context))
	  mkcl_FEwin32_error(env, "Cannot get context for thread ~A", 1, thread);
#if defined(_X86_)
	context.Eip = (DWORD) resume_thread_unwinding;
#elif defined(__x86_64)
	context.Rip = (DWORD64) resume_thread_unwinding;
#else
#error Unknown processor architecture
#endif
	if (!SetThreadContext(os_thread, &context))
	  mkcl_FEwin32_error(env, "Cannot set context for thread ~A", 1, thread);
      }

    {
      DWORD count;

      if ((count = ResumeThread(os_thread)) == (DWORD)-1)
	mkcl_FEwin32_error(env, "Cannot resume interrupted thread ~A", 1, thread);
    }
  }

  if (!ReleaseMutex(mkcl_interrupt_thread_lock))
    mkcl_FEwin32_error(env, "interrupt thread failed to release interrupt lock", 0);

  return 0;
}

#endif /* MKCL_WINDOWS */



static void setup_thread_bindings(MKCL, mkcl_object initial_bindings)
{
  mkcl_object l = initial_bindings;
  mkcl_object table = mk_cl__make_hash_table(env, @'eq',
					     MKCL_MAKE_FIXNUM(1024),
					     MKCL_MAKE_FIXNUM(1024),
					     MKCL_MAKE_FIXNUM(1));

  mkcl_loop_for_in(env, l)
    {
      mkcl_object cell = MKCL_CONS_CAR(l);

      mkcl_object sym = MKCL_CONS_CAR(cell);
      mkcl_object value = MKCL_CONS_CDR(cell);

      mkcl_check_symbol(env, sym);
      if (NULL == mkcl_search_hash(env, sym, table))
	{
	  mkcl_sethash(env, sym, table, value);
	  mkcl_bds_bind(env, sym, value);
	}
    }
  mkcl_end_loop_for_in;
}


static mkcl_object
_mkcl_specials_snapshot(MKCL)
{
  struct mkcl_bds_bd * bds_top = env->bds_top;
  struct mkcl_bds_bd * bds = env->bds_org;
  mkcl_object init_bind = mk_cl_Cnil;

  for (; bds <= bds_top; bds++)
    init_bind = mk_cl_acons(env, bds->symbol, MKCL_SYM_VAL(env, bds->symbol), init_bind);
  return(init_bind);
}

void
_mkcl_dealloc_env(MKCL)
{
  int i;
  for (i = 0; i < 3; i++)
    {
      _mkcl_big_clear(env->big_register[i]);
    }
  mkcl_dealloc(env, env);
}

const mkcl_env _mkcl_alloc_env(MKCL)
{
  /* This function is called so early in the life of a MKCL world that we cannot
     allow it to throw a CL condition under any circonstance,
     or to report any kind of error in any case.
  */
  const mkcl_env new_env = _mkcl_alloc_raw_env(env);

  if (new_env == NULL) /* alloc failed no point in going any further. */
    return(NULL);
  /*
   * An uninitialized environment _always_ disables interrupts. They
   * are activated later on by the thread entry point or mkcl_init_unixint().
   */
  mkcl_disable_interrupts(new_env);
  new_env->sleeping_on = mk_cl_Cnil;

  new_env->cs_org = NULL;
  new_env->cs_limit = NULL;
  new_env->cs_size = 0;
  new_env->cs_overflow_size = 0;
  new_env->cs_overflowing = FALSE;

  new_env->nvalues = 0;
  {
    int i;

    for (i = 0; i < MKCL_MULTIPLE_VALUES_LIMIT; i++)
      new_env->values[i] = mk_cl_Cnil;
  }

  new_env->function = mk_cl_Cnil;

  new_env->temp_stack_size = 0;
  new_env->temp_stack_size_limit = 0;
  new_env->temp_stack = NULL;
  new_env->temp_stack_top = NULL;
  new_env->temp_stack_upper_bound = NULL;
  new_env->temp_stack_overflow_size = 0;
  new_env->temp_stack_overflowing = FALSE;

  new_env->bds_size = 0;
  new_env->bds_size_limit = 0;
  new_env->bds_org = NULL;
  new_env->bds_top = NULL;
  new_env->bds_upper_bound = NULL;
  new_env->bds_overflow_size = 0;
  new_env->bds_overflowing = FALSE;

  new_env->specials_size = MKCL_STARTUP_SPECIALS_SIZE;
  new_env->specials = ((env == NULL) 
		       ? _mkcl_boot_alloc_unprotected(MKCL_STARTUP_SPECIALS_SIZE * sizeof(mkcl_object))
		       : mkcl_alloc(env, MKCL_STARTUP_SPECIALS_SIZE * sizeof(mkcl_object)));

  if (new_env->specials)
    {
      mkcl_index i;
      
      for (i = 0; i < MKCL_STARTUP_SPECIALS_SIZE; i++)
	new_env->specials[i] = MKCL_END_OF_BDS_CHAIN;
    }
  else
    return(NULL);

  new_env->ihs_top = NULL;

  new_env->frs_size = 0;
  new_env->frs_size_limit = 0;
  new_env->frs_org = NULL;
  new_env->frs_top = NULL;
  new_env->frs_upper_bound = NULL;
  new_env->nlj_fr = NULL;
  new_env->go_label_index = -1; /* Normally this should be an invalid index. */
  new_env->frs_overflow_size = 0;
  new_env->frs_overflowing = FALSE;

  new_env->string_pool = mk_cl_Cnil;

  new_env->c_env = NULL;

  new_env->fmt_aux_stream = mk_cl_Cnil;

  new_env->print_pretty = FALSE;
  new_env->queue = NULL;
  new_env->indent_stack = NULL;
  new_env->qh = new_env->qt = new_env->qc = new_env->isp = new_env->iisp = 0;

  new_env->big_register[0] = mk_cl_Cnil;
  new_env->big_register[1] = mk_cl_Cnil;
  new_env->big_register[2] = mk_cl_Cnil;

  new_env->own_thread = mk_cl_Cnil;

  new_env->method_hash_clear_list = mk_cl_Cnil;
  new_env->method_hash = mk_cl_Cnil;
  new_env->method_spec_vector = mk_cl_Cnil;
  new_env->method_generation = 0;

  new_env->fficall = NULL;

  new_env->altstack = NULL;
  new_env->altstack_size = 0;

  new_env->fpe_control_bits = 0;

  new_env->interrupt_disabler_file = NULL;
  new_env->interrupt_disabler_lineno = 0;

  new_env->fp_drone = 0;
  /* new_env->alloc = NULL; _mkcl_alloc_raw_env() takes care of this one. */

  new_env->cs_org_request = NULL;
  new_env->cs_size_request = 0;

  return new_env;
}

void
mkcl_init_env(MKCL, mkcl_env new_env, struct mkcl_thread_init_parameters * params)
{
  new_env->c_env = NULL;

  new_env->string_pool = mk_cl_Cnil;

  new_env->method_hash = mk_cl_Cnil;
  new_env->method_spec_vector = mk_cl_Cnil;
  new_env->method_generation = 0;
  _mkcl_set_method_hash_size(new_env, 4096);
  new_env->method_hash_clear_list = mk_cl_Cnil;

  mkcl_init_stacks(env, new_env, params);

  {
    int i;
    for (i = 0; i < 3; i++)
      {
	mkcl_object x = mkcl_alloc_raw_bignum(env);

	_mkcl_big_init2(x, MKCL_BIG_REGISTER_SIZE);
	new_env->big_register[i] = x;
      }
  }

  new_env->fpe_control_bits = 0;
}

mkcl_object
mkcl_make_thread(MKCL, mkcl_object name, mkcl_object initial_bindings, struct mkcl_thread_init_parameters * params)
{
  const mkcl_env new_env = _mkcl_alloc_env(env);
  mkcl_object thread = mkcl_alloc_raw_thread(env);

  thread->thread.status = mkcl_thread_initialized;
  thread->thread.name = name;
  thread->thread.function = mk_cl_Cnil;
  thread->thread.args = mk_cl_Cnil;
  thread->thread.result_value = MKCL_OBJNULL;
  thread->thread.detached = FALSE;
  thread->thread.thread = 0;
  thread->thread.base_thread = 0;
  thread->thread.tid = 0;
  thread->thread.interrupt = mk_cl_Cnil;
  thread->thread.plist = mk_cl_Cnil;
  thread->thread.shutdown_requested = false;
  thread->thread.env = new_env;

#if MKCL_PTHREADS
  thread->thread.running_lock = NULL;
  sigemptyset(&thread->thread.saved_sigmask);
#endif
  thread->thread.resume_handler_ran = FALSE;
  thread->thread.sigmask_frs_marker = NULL;
  thread->thread.interrupt_count = 0;
  {
    int i, j;

    for (i = 0; i < MKCL_MAX_INTERRUPTS; i++)
      {
	thread->thread.interrupted_threads[i].thread_ident = 0;

	thread->thread.interrupted_threads[i].cs_org = NULL;
	thread->thread.interrupted_threads[i].cs_limit = NULL;
	thread->thread.interrupted_threads[i].cs_size = 0;
	thread->thread.interrupted_threads[i].cs_overflow_size = 0;
	thread->thread.interrupted_threads[i].cs_overflowing = FALSE;

	thread->thread.interrupted_threads[i].disable_interrupts = FALSE;
	thread->thread.interrupted_threads[i].interrupt_disabler_file = NULL;
	thread->thread.interrupted_threads[i].interrupt_disabler_lineno = 0;

	thread->thread.interrupted_threads[i].nvalues = 0;

	mkcl_object * values = thread->thread.interrupted_threads[i].values;
	for (j = 0; j < MKCL_MULTIPLE_VALUES_LIMIT; j++) values[j] = mk_cl_Cnil;
      }   
  }

  thread->thread.initial_bindings = NULL;

  if (initial_bindings == mk_cl_Ct)
    { /* New thread inherits its initial bindings from its creator thread. */
      thread->thread.initial_bindings = _mkcl_specials_snapshot(env);
    }
  else
    thread->thread.initial_bindings = mk_cl_copy_alist(env, initial_bindings);

  new_env->own_thread = thread;
  mkcl_init_env(env, new_env, params);

  mk_si_set_finalizer(env, thread, mk_cl_Ct);

  return thread;
}

#if MKCL_WINDOWS
static CRITICAL_SECTION mkcl_imported_thread_pool_lock;
static HANDLE mkcl_imported_thread_pool_empty; /* Semaphore */
static HANDLE mkcl_imported_thread_pool_full; /* Semaphore */
#else
static pthread_mutex_t mkcl_imported_thread_pool_lock;
static sem_t mkcl_imported_thread_pool_empty_sem_obj;
static sem_t * mkcl_imported_thread_pool_empty = &mkcl_imported_thread_pool_empty_sem_obj;
static sem_t mkcl_imported_thread_pool_full_sem_obj;
static sem_t * mkcl_imported_thread_pool_full = &mkcl_imported_thread_pool_full_sem_obj;
#endif


static void fill_imported_thread_pool(MKCL)
{
  volatile bool locked = false;
  mkcl_object head;
  int i;

  MKCL_UNWIND_PROTECT_BEGIN(env) {
    mkcl_interrupt_status old_intr;
    struct mkcl_thread_init_parameters init_params = { 0 };

    mkcl_get_interrupt_status(env, &old_intr);
    mkcl_disable_interrupts(env);
#if MKCL_WINDOWS
    EnterCriticalSection(&mkcl_imported_thread_pool_lock);
#else
    if (pthread_mutex_lock(&mkcl_imported_thread_pool_lock))
      mkcl_lose(env, "fill_imported_thread_pool failed on pthread_mutex_lock");
#endif
    locked = true;
    mkcl_set_interrupt_status(env, &old_intr);

    head = mkcl_core.imported_thread_pool;
    for (i = 0; i < 5; i++)
      head = MKCL_CONS(env, mkcl_make_thread(env, mk_cl_Cnil, mk_cl_Cnil, &init_params), head);
    mkcl_core.imported_thread_pool = head;

  } MKCL_UNWIND_PROTECT_EXIT {
#if MKCL_WINDOWS
    if (locked)
      LeaveCriticalSection(&mkcl_imported_thread_pool_lock);
#else
    if (locked)
      if (pthread_mutex_unlock(&mkcl_imported_thread_pool_lock))
	mkcl_lose(env, "fill_imported_thread_pool failed on pthread_mutex_unlock");  
#endif
  } MKCL_UNWIND_PROTECT_END;
}

static thread_value_t CALL_CONV imported_thread_pool_filler(void * arg)
{
  char stack_mark = 0;
  mkcl_object thread = (mkcl_object) arg;
  const mkcl_env env = thread->thread.env;
  thread_value_t status = (thread_value_t) MKCL_THREAD_NORMAL_EXIT;

#if MKCL_PTHREADS
  if (pthread_mutex_lock(thread->thread.running_lock)) return (thread_value_t) MKCL_THREAD_UNKNOWN_ERROR;
  /* Insert some private thread initialization here */
  if (pthread_mutex_unlock(thread->thread.running_lock)) return (thread_value_t) MKCL_THREAD_UNKNOWN_ERROR;
  if (pthread_mutex_destroy(thread->thread.running_lock)) return (thread_value_t) MKCL_THREAD_UNKNOWN_ERROR;
  thread->thread.running_lock = NULL;
#endif

  MKCL_CATCH_ALL_BEGIN(env) {
    MKCL_SETUP_CALL_STACK_ROOT_GUARD(env);

#if 1
    mkcl_setup_thread_lisp_context(env, &stack_mark);
#else
    mkcl_set_thread_env(env);
    thread->thread.tid = mkcl_gettid();
    
    mkcl_init_call_stack_overflow_area(env, &stack_mark);
    
    mkcl_bds_bind(env, @'mt::*thread*', thread);
    mkcl_bds_bind(env, @'mkcl::*current-working-directory*', mk_cl_Cnil);
    mkcl_bds_bind(env, @'mkcl::*all-current-working-directories*', mk_cl_Cnil);
    mkcl_bds_bind(env, @'si::*dynamic-cons-stack*', mk_cl_Cnil);
    mk_si_trim_dynamic_cons_stack(env);
#endif

    mkcl_register_thread_as_active(env, thread);
    mk_mt_thread_detach(env, thread);
    mkcl_enable_interrupts(env);

    for (;;) /* forever until we're killed. */
      {
#if MKCL_WINDOWS
	BOOL ok;
	DWORD wait_val;

	do {
	  MKCL_LIBC_Zzz(env, @':io', wait_val = WaitForSingleObjectEx(mkcl_imported_thread_pool_empty, INFINITE, TRUE));
	} while (wait_val == WAIT_IO_COMPLETION);
	switch (wait_val)
	  {
	  case WAIT_OBJECT_0: break;
	  case WAIT_TIMEOUT:
	  case WAIT_ABANDONED:
	  case WAIT_FAILED:
	  default:
	    mkcl_FEwin32_error(env, ("imported_thread_pool_filler failed "
				     "on wait for empty semaphore"), 0);
	  }
	mk_mt_test_for_thread_shutdown(env);

	fill_imported_thread_pool(env);

	MKCL_LIBC_NO_INTR(env, ok = ReleaseSemaphore(mkcl_imported_thread_pool_full, 1, NULL));
	if (!ok)
	  mkcl_FEwin32_error(env, "imported_thread_pool_filler failed on ReleaseSemaphore", 0);

#else
	int rc;

	do {
	  MKCL_LIBC_Zzz(env, @':io', rc = sem_wait(mkcl_imported_thread_pool_empty));
	} while ( rc && errno == EINTR );
	if (rc)
	  mkcl_C_lose(env, "imported_thread_pool_filler failed on sem_wait");
	mk_mt_test_for_thread_shutdown(env);

	fill_imported_thread_pool(env);

	MKCL_LIBC_NO_INTR(env, rc = sem_post(mkcl_imported_thread_pool_full));
	if (rc)
	  mkcl_C_lose(env, "imported_thread_pool_filler failed on sem_post");
#endif
      }
#if 1
    mkcl_cleanup_thread_lisp_context(env);
#else
    /* mkcl_bds_unwind1(env); */
    /* mkcl_bds_unwind1(env); */
    /* mkcl_bds_unwind1(env); */
    /* mkcl_bds_unwind1(env); */
#endif
    MKCL_UNSET_CALL_STACK_ROOT_GUARD(env);
  } MKCL_CATCH_ALL_IF_CAUGHT {
    MKCL_UNSET_CALL_STACK_ROOT_GUARD(env);
  } MKCL_CATCH_ALL_END;
  status = (thread_value_t) mkcl_exit_status(env);
  thread->thread.status = mkcl_thread_done;

  thread_final_cleanup(env, thread);
  return status;
}

static void mkcl_create_imported_thread_pool_filler_thread(MKCL)
{
  static const mkcl_base_string_object(thread_name_obj, "Imported thread pool filler daemon");
  struct mkcl_thread_init_parameters init_params = { 0 };
  mkcl_object thread = mkcl_make_thread(env, (mkcl_object) &thread_name_obj, mk_cl_Cnil, &init_params);

  thread->thread.function = mk_cl_Cnil;
  thread->thread.args = mk_cl_Cnil;

#if MKCL_WINDOWS
  {
    unsigned threadId;
    uintptr_t code;

    /* GC redirect */
    MKCL_GC_NO_INTR(env, code = _beginthreadex(NULL, 0, imported_thread_pool_filler, thread, CREATE_SUSPENDED, &threadId));
    if (code == 0)
      mkcl_FElibc_error(env, "mt::thread-enable failed on " "_beginthreadex for thread ~S", 1, thread);
    else
      thread->thread.base_thread = thread->thread.thread = (HANDLE) code;
  }

  DWORD old_suspend_count;
  MKCL_LIBC_NO_INTR(env, old_suspend_count = ResumeThread(thread->thread.thread));
  if (old_suspend_count != 1) /* 1 is the only right answer here, everything else is an error of some kind. */
    mkcl_FEwin32_error(env, "create_interrupt_thread failed on ResumeThead for thread ~A", 1, thread);

  /* Trigger the initial fill-up. */
  BOOL ok;
  MKCL_LIBC_NO_INTR(env, ok = ReleaseSemaphore(mkcl_imported_thread_pool_empty, 1, NULL));
  if (!ok)
    mkcl_FEwin32_error(env, "mkcl_create_imported_thread_pool_filler failed on ReleaseSemaphore", 0);
#elif MKCL_PTHREADS
  int result;

  thread->thread.running_lock = mkcl_alloc_atomic(env, sizeof(pthread_mutex_t));
  pthread_mutex_init(thread->thread.running_lock, NULL);
  pthread_mutex_lock(thread->thread.running_lock);

  MKCL_GC_NO_INTR(env, result = pthread_create(&thread->thread.thread, NULL,  /* GC redirect */
					       imported_thread_pool_filler,
					       thread));
  thread->thread.base_thread = thread->thread.thread;
  pthread_mutex_unlock(thread->thread.running_lock);

  if ( result != 0 )
    { errno = result; mkcl_FElibc_error(env, "Cannot create imported thread pool filler thread", 0); }

  /* Trigger the initial fill-up. */
  MKCL_LIBC_NO_INTR(env, result = sem_post(mkcl_imported_thread_pool_empty));
  if (result)
    mkcl_FElibc_error(env, "mkcl_create_imported_thread_pool_filler_thread failed on sem_post", 0);
#else
# error Incomplete implementation of mkcl_create_imported_thread_pool_filler_thread().
#endif
}

static void push_in_imported_thread_pool(MKCL, mkcl_object thread)
{
  volatile bool locked = false;
  MKCL_UNWIND_PROTECT_BEGIN(env) {
    mkcl_interrupt_status old_intr;

    mkcl_get_interrupt_status(env, &old_intr);
    mkcl_disable_interrupts(env);
#if MKCL_WINDOWS
    EnterCriticalSection(&mkcl_imported_thread_pool_lock);
#else
    if (pthread_mutex_lock(&mkcl_imported_thread_pool_lock))
      mkcl_lose(env, "push_in_imported_thread_pool failed on pthread_mutex_lock");
#endif
    locked = true;
    mkcl_set_interrupt_status(env, &old_intr);

    mkcl_core.imported_thread_pool = MKCL_CONS(env, thread, mkcl_core.imported_thread_pool);
  } MKCL_UNWIND_PROTECT_EXIT {
#if MKCL_WINDOWS
    if (locked)
      LeaveCriticalSection(&mkcl_imported_thread_pool_lock);
#else
    if (locked)
      if (pthread_mutex_unlock(&mkcl_imported_thread_pool_lock))
	mkcl_lose(env, "push_in_imported_thread_pool failed on pthread_mutex_unlock");
#endif
  } MKCL_UNWIND_PROTECT_END;
}

static mkcl_object pop_from_imported_thread_pool(mkcl_thread_import_failure_handler * handler, void * handler_data)
{
  mkcl_object thread = mk_cl_Cnil;
  mkcl_object pool = mk_cl_Cnil;
  char * os_error = NULL;
  int rc = 0;
  bool pool_locked = FALSE;

#if MKCL_WINDOWS
  EnterCriticalSection(&mkcl_imported_thread_pool_lock);
#else
  if ((rc = pthread_mutex_lock(&mkcl_imported_thread_pool_lock)))
    { os_error = "pop_from_imported_thread_pool failed on pthread_mutex_lock"; goto LOSE; }
#endif
  pool_locked = TRUE;
  pool = mkcl_core.imported_thread_pool;

  if (mkcl_Null(pool))
    { /* the pool is empty. */
#if MKCL_WINDOWS
      DWORD wait_val;

      if (!ReleaseSemaphore(mkcl_imported_thread_pool_empty, 1, NULL))
	{ os_error ="pop_from_imported_thread_pool failed on ReleaseSemaphore"; goto LOSE; }

      wait_val = WaitForSingleObject(mkcl_imported_thread_pool_full, INFINITE);
      switch (wait_val)
	{
	case WAIT_OBJECT_0: break;
	case WAIT_TIMEOUT:
	case WAIT_ABANDONED:
	case WAIT_FAILED:
	default:
	  os_error = "pop_from_imported_thread_pool failed on wait for full semaphore";
	  goto LOSE;
	}
#else
      rc = sem_post(mkcl_imported_thread_pool_empty);
      if (rc)
	{ os_error = "pop_from_imported_thread_pool failed on sem_post"; goto LOSE; }

      do
	rc = sem_wait(mkcl_imported_thread_pool_full);
      while (rc && errno == EINTR);
      if (rc)
	{ os_error = "pop_from_imported_thread_pool failed on sem_wait"; goto LOSE; }

#endif

      pool = mkcl_core.imported_thread_pool;
      if (mkcl_Null(pool))
	{ os_error = "Failed to refill imported thread pool!"; rc = 0; goto LOSE; }
      else
	{
	  mkcl_core.imported_thread_pool = MKCL_CONS_CDR(pool);
	  thread = MKCL_CONS_CAR(pool);
	}
    }
  else
    {
      mkcl_core.imported_thread_pool = MKCL_CONS_CDR(pool);
      thread = MKCL_CONS_CAR(pool);
    }

  /* This function is called before we have a proper thread environment set up
     so we cannot wrap the lock/unlock in an unwind-protect.
     Hopefully the body of this function cannot raise any exception. JCB
   */
#if MKCL_WINDOWS
  LeaveCriticalSection(&mkcl_imported_thread_pool_lock);
#else
  if ((rc = pthread_mutex_unlock(&mkcl_imported_thread_pool_lock)))
    { os_error = "pop_from_imported_thread_pool failed on pthread_mutex_unlock"; goto LOSE; }
#endif
  
  return(thread);

 LOSE:
#if MKCL_WINDOWS
  LeaveCriticalSection(&mkcl_imported_thread_pool_lock);
  if (handler)
    (*handler)(handler_data, rc, os_error);
#else
  {
    int rc2 = 0;
    char * os_error2 = NULL;

    if (pool_locked)
      if ((rc2 = pthread_mutex_unlock(&mkcl_imported_thread_pool_lock)))
        { os_error2 = "pop_from_imported_thread_pool failed on pthread_mutex_unlock"; }
    if (handler)
      (*handler)(handler_data, rc, os_error);
    if (handler) /* should be unreachable */
      (*handler)(handler_data, rc2, os_error2);
  }
#endif
  return(mk_cl_Cnil); /* should be unreachable */
}

static mkcl_object assign_imported_thread(mkcl_object name, mkcl_object bindings,
					  mkcl_thread_import_failure_handler * handler, void * handler_data)
{
  mkcl_object thread = pop_from_imported_thread_pool(handler, handler_data);

  if (thread)
    {
      thread->thread.status = mkcl_thread_initialized;
      thread->thread.name = name;
      thread->thread.function = mk_cl_Cnil;
      thread->thread.args = mk_cl_Cnil;
      thread->thread.result_value = MKCL_OBJNULL;
      thread->thread.initial_bindings = (bindings == mk_cl_Ct ? mk_cl_Cnil : bindings);
      thread->thread.interrupt_count = 0;
      thread->thread.plist = mk_cl_Cnil;
      thread->thread.interrupt = mk_cl_Cnil;
      thread->thread.shutdown_requested = false;
    }
  return thread;
}


const mkcl_env
mkcl_import_current_thread(mkcl_object name, mkcl_object bindings, mkcl_thread_import_failure_handler * handler, void * handler_data)
{
  char stack_mark = 0;
  mkcl_object l;
  mkcl_os_thread_t current;

#if MKCL_WINDOWS
# define pthread_equal(a,b) ((a)==(b))
# define _close_handle(h) CloseHandle(h)
  DuplicateHandle(GetCurrentProcess(),
 		  GetCurrentThread(),
 		  GetCurrentProcess(),
 		  &current,
 		  0,
 		  FALSE,
 		  DUPLICATE_SAME_ACCESS);
#else
# define _close_handle(h)
  current = pthread_self();
#endif
  /* This walks (reading) the list of lisp threads without taking the lock for it! The ice is a bit thin here! JCB */
  for (l = mkcl_core.threads; l != mk_cl_Cnil; l = MKCL_CONS_CDR(l)) {
    mkcl_object p = MKCL_CONS_CAR(l);
    int i;
    if (pthread_equal(p->thread.thread, current)) {
      { _close_handle(current); errno = EEXIST; return(NULL); }
    }
    /* could be an interrupted thread. JCB */
    for (i = 0; i < p->thread.interrupt_count; i++)
      if (pthread_equal(current, p->thread.interrupted_threads[i].thread_ident))
	{ _close_handle(current); errno = EEXIST; return(NULL); }
  }
#if MKCL_WINDOWS
# if 0
  _close_handle(current); /* This is too early since it will be used in the thread object. */
# endif
# undef pthread_equal
# undef _close_handle
#endif

  {
    mkcl_object thread = assign_imported_thread(name, bindings, handler, handler_data);
    const mkcl_env env = (thread ? thread->thread.env : NULL);

    if (env == NULL)
      { errno = ENOMEM; return NULL; }
    else
      {
	struct MK_GC_stack_base sb;

	if (MK_GC_SUCCESS != MK_GC_get_stack_base(&sb))
	  { errno = ENOSYS; return(NULL); }

	/* cannot be interrupted since we are not known yet by the rest of the Lisp world. */
	switch (MK_GC_register_my_thread(&sb))
	  {
	  case MK_GC_SUCCESS:
	    thread->thread.thread = current;
	    thread->thread.base_thread = current;
	    /* imported threads are presumed not to be under lisp full (ultimate) control. */
	    thread->thread.result_value = @':imported-and-gc-registered';
	    return(env);
	  case MK_GC_DUPLICATE:
	    thread->thread.thread = current;
	    thread->thread.base_thread = current;
	    /* imported threads are presumed not to be under lisp full (ultimate) control. */
	    thread->thread.result_value = @':imported';
	    return(env);
	  default: /* This case should not be possible with Boehm GC 7.2 */
	    mkcl_release_current_thread(env);
	    errno = ENOSYS;
	    return NULL;
	  }
      }
  }
}

void
mkcl_release_current_thread(MKCL)
{
  mkcl_object thread = env->own_thread;
  bool must_unregister = thread->thread.result_value == @':imported-and-gc-registered';

  thread_final_cleanup(env, thread);

  thread->thread.status = mkcl_thread_initialized;
  push_in_imported_thread_pool(env, thread);
  
  if (must_unregister)
    MKCL_GC_NO_INTR(env, MK_GC_unregister_my_thread());
}


/*************************************************************/

#if MKCL_WINDOWS
static HANDLE mkcl_finalization_requested; /* an Event object */
#elif MKCL_PTHREADS
static mkcl_object mkcl_finalization_requested; /* a condition variable */
#endif

static thread_value_t CALL_CONV finalization_thread_entry_point(void * arg)
{
  char stack_mark = 0;
  mkcl_object thread = (mkcl_object) arg;
  const mkcl_env env = thread->thread.env;
  thread_value_t status = (thread_value_t) MKCL_THREAD_NORMAL_EXIT;

  if (env->own_thread != thread) return (thread_value_t) MKCL_THREAD_ABORTED;

#if MKCL_PTHREADS
  if (pthread_mutex_lock(thread->thread.running_lock)) return (thread_value_t) MKCL_THREAD_UNKNOWN_ERROR;
  /* Insert some private thread initialization here */
  if (pthread_mutex_unlock(thread->thread.running_lock)) return (thread_value_t) MKCL_THREAD_UNKNOWN_ERROR;
  if (pthread_mutex_destroy(thread->thread.running_lock)) return (thread_value_t) MKCL_THREAD_UNKNOWN_ERROR;
  thread->thread.running_lock = NULL;
#endif

  MKCL_CATCH_ALL_BEGIN(env) {
    MKCL_SETUP_CALL_STACK_ROOT_GUARD(env);

#if 1
    mkcl_setup_thread_lisp_context(env, &stack_mark);
#else
    mkcl_set_thread_env(env);
    thread->thread.tid = mkcl_gettid();
    mkcl_init_call_stack_overflow_area(env, &stack_mark);
    
    mkcl_bds_bind(env, @'mt::*thread*', thread);
    mkcl_bds_bind(env, @'mkcl::*current-working-directory*', mk_cl_Cnil);
    mkcl_bds_bind(env, @'mkcl::*all-current-working-directories*', mk_cl_Cnil);
    mkcl_bds_bind(env, @'si::*dynamic-cons-stack*', mk_cl_Cnil);
    mkcl_bds_bind(env, @'*debugger-hook*', @+'si::non-interactive-thread-debugger-trap');
    mk_si_trim_dynamic_cons_stack(env);
#endif

    mkcl_register_thread_as_active(env, thread);
    mk_mt_thread_detach(env, thread);
    mkcl_enable_interrupts(env);

#if MKCL_PTHREADS
    pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;

    /* pthread_mutex_init(&mutex, NULL); */
    if (pthread_mutex_lock(&mutex))
      {
        /* status = (thread_value_t) MKCL_THREAD_UNKNOWN_ERROR; */
        thread->thread.result_value = mk_cl_Cnil;
        goto CLEAN_UP_MUTEX_UNLOCKED;
      }
#endif

    for (;;) /* forever until we're shutdown. */
      {
#if MKCL_WINDOWS
	BOOL ok;
	DWORD wait_val;

	do {
	  MKCL_LIBC_Zzz(env, @':io', wait_val = WaitForSingleObjectEx(mkcl_finalization_requested, INFINITE, TRUE));
	} while (wait_val == WAIT_IO_COMPLETION);
	switch (wait_val)
	  {
	  case WAIT_OBJECT_0: break;
	  case WAIT_TIMEOUT:
	  case WAIT_ABANDONED:
	  case WAIT_FAILED:
	  default:
	    mkcl_FEwin32_error(env, "imported_thread_pool_filler failed on wait for empty semaphore", 0);
	  }
	mk_mt_test_for_thread_shutdown(env);
	env->nvalues = 0; /* Such that MK_GC_invoke_finalizers won't have to preserve any env->values[i]. */

	ResetEvent(mkcl_finalization_requested);

	int count;

	MKCL_GC_NO_INTR(env, count = MK_GC_invoke_finalizers()); /* returns the number of finalizers that were run, if we care... */
#if 0
	fprintf(stderr, "\n;; MKCL: Called MK_GC_invoke_finalizers() for a count of %d.\n", count);
	fflush(stderr);
#endif

#elif MKCL_PTHREADS
	int rc;

	do {
	  const mkcl_object cv = mkcl_finalization_requested;
	  MKCL_LIBC_Zzz(env, cv, rc = pthread_cond_wait(&cv->condition_variable.cv, &mutex));
#if 0
	  fprintf(stderr, "\n;; MKCL: mkcl_finalization_requested woke up with rc = %d.\n", rc);
	  fflush(stderr);
#endif
	} while ( rc == EINTR );

	if (rc)
	  { errno = rc; mkcl_C_lose(env, "finalization_thread_entry_point failed on pthread_cond_wait"); }
	mk_mt_test_for_thread_shutdown(env);
	env->nvalues = 0; /* Such that MK_GC_invoke_finalizers won't have to preserve any env->values[i]. */

	int count;

	MKCL_GC_NO_INTR(env, count = MK_GC_invoke_finalizers()); /* returns the number of finalizers that were run, if we care... */
#if 0
	fprintf(stderr, "\n;; MKCL: Called MK_GC_invoke_finalizers() for a count of %d.\n", count);
	fflush(stderr);
#endif
#else
# error Incomplete implementation of finalization_thread_entry_point().
#endif
      }

#if MKCL_PTHREADS
    if (pthread_mutex_unlock(&mutex))
      {
        /* status = (thread_value_t) MKCL_THREAD_UNKNOWN_ERROR; */
        thread->thread.result_value = mk_cl_Cnil;
      }
#endif /* MKCL_PTHREADS */

  CLEAN_UP_MUTEX_UNLOCKED:
#if 1
    mkcl_cleanup_thread_lisp_context(env);
#else
    /* mkcl_bds_unwind1(env); */
    /* mkcl_bds_unwind1(env); */
    /* mkcl_bds_unwind1(env); */
    /* mkcl_bds_unwind1(env); */
    /* mkcl_bds_unwind1(env); */
#endif
    MKCL_UNSET_CALL_STACK_ROOT_GUARD(env);
  } MKCL_CATCH_ALL_IF_CAUGHT {
    MKCL_UNSET_CALL_STACK_ROOT_GUARD(env);
  } MKCL_CATCH_ALL_END;
  status = (thread_value_t) mkcl_exit_status(env);
  thread->thread.status = mkcl_thread_done;

#if 0
  fprintf(stderr, "\n;; MKCL: Finalization thread is about to exit!\n");
  fflush(stderr);
#endif

#if MKCL_WINDOWS
  {
    HANDLE hnd = mkcl_finalization_requested;

    mkcl_finalization_requested = NULL;
    (void) CloseHandle(hnd);
  }
#endif
  thread_final_cleanup(env, thread);
  return status;
}


static void request_finalization(void)
{
#if MKCL_WINDOWS
  HANDLE hnd = mkcl_finalization_requested;
  if (hnd)
    SetEvent(hnd);
#else
  mkcl_object cond_var = mkcl_finalization_requested;
  if (!mkcl_Null(cond_var))
    pthread_cond_signal(&cond_var->condition_variable.cv);
#endif
}

static void mkcl_create_finalization_thread(MKCL)
{
  static const mkcl_base_string_object(thread_name_obj, "Finalization daemon");
  struct mkcl_thread_init_parameters init_params = { 0 };
  mkcl_object thread = mkcl_make_thread(env, (mkcl_object) &thread_name_obj, mk_cl_Cnil, &init_params);

  thread->thread.function = mk_cl_Cnil;
  thread->thread.args = mk_cl_Cnil;

#if MKCL_WINDOWS
  {
    unsigned threadId;
    uintptr_t code;

    mkcl_finalization_requested = CreateEvent(NULL, TRUE, FALSE, mkcl_handle_debug_name(env, "finalization requested event"));
    if (mkcl_finalization_requested == NULL)
      mkcl_FEwin32_error(env, "mkcl_create_finalization_thread failed to create finalization requested event", 0);

    /* GC redirect */
    MKCL_GC_NO_INTR(env, code = _beginthreadex(NULL, 0, finalization_thread_entry_point, thread, CREATE_SUSPENDED, &threadId));
    if (code == 0)
      mkcl_FElibc_error(env, "mt::thread-enable failed on " "_beginthreadex for thread ~S", 1, thread);
    else
      thread->thread.base_thread = thread->thread.thread = (HANDLE) code;

    DWORD old_suspend_count;
    MKCL_LIBC_NO_INTR(env, old_suspend_count = ResumeThread(thread->thread.thread));
    if (old_suspend_count != 1) /* 1 is the only right answer here, everything else is an error of some kind. */
      mkcl_FEwin32_error(env, "mkcl_create_finalization_thread failed on ResumeThead for thread ~A", 1, thread);    
  }
#elif MKCL_PTHREADS
  int result;

  mkcl_finalization_requested = mk_mt_make_condition_variable(env);

  thread->thread.running_lock = mkcl_alloc_atomic(env, sizeof(pthread_mutex_t));
  pthread_mutex_init(thread->thread.running_lock, NULL);
  pthread_mutex_lock(thread->thread.running_lock);

  MKCL_GC_NO_INTR(env, result = pthread_create(&thread->thread.thread, NULL,  /* GC redirect */
					       finalization_thread_entry_point,
					       thread));
  thread->thread.base_thread = thread->thread.thread;
  pthread_mutex_unlock(thread->thread.running_lock);

  if ( result != 0 )
    { errno = result; mkcl_FElibc_error(env, "Cannot create imported thread pool filler thread", 0); }
#else
# error Incomplete implementation of mkcl_create_finalization_thread().
#endif /* MKCL_PTHREADS */

  MK_GC_finalizer_notifier_proc old_notifier;

  MKCL_GC_NO_INTR(env, old_notifier = MK_GC_get_finalizer_notifier());

  MKCL_GC_NO_INTR(env, MK_GC_set_finalizer_notifier(/*(MK_GC_finalizer_notifier_proc)*/ request_finalization));
}




/*************************************************************/

@(defun mt::make-thread (&key
			 name
			 ((:initial-bindings initial_bindings) mk_cl_Ct)
			 call_stack_size
			 binding_stack_initial_size
			 binding_stack_size_limit
			 frame_stack_initial_size
			 frame_stack_size_limit
			 lisp_temp_stack_initial_size
			 lisp_temp_stack_size_limit
			 sigaltstack_size
			 )
  mkcl_object thread;
@
  struct mkcl_thread_init_parameters init_params = { 0 };

  if (!mkcl_Null(call_stack_size))
    init_params.call_stack_size = mkcl_integer_to_index(env, call_stack_size);
  if (!mkcl_Null(sigaltstack_size))
    init_params.sigaltstack_size = mkcl_integer_to_index(env, sigaltstack_size);
  if (!mkcl_Null(binding_stack_initial_size))
    init_params.binding_stack_initial_size = mkcl_integer_to_index(env, binding_stack_initial_size);
  if (!(mkcl_Null(binding_stack_size_limit) || (binding_stack_size_limit == @':unlimited')))
    init_params.binding_stack_size_limit = mkcl_integer_to_index(env, binding_stack_size_limit);
  if (!mkcl_Null(frame_stack_initial_size))
    init_params.frame_stack_initial_size = mkcl_integer_to_index(env, frame_stack_initial_size);
  if (!(mkcl_Null(frame_stack_size_limit) || (frame_stack_size_limit == @':unlimited')))
    init_params.frame_stack_size_limit = mkcl_integer_to_index(env, frame_stack_size_limit);
  if (!mkcl_Null(lisp_temp_stack_initial_size))
    init_params.lisp_temp_stack_initial_size = mkcl_integer_to_index(env, lisp_temp_stack_initial_size);
  if (!(mkcl_Null(lisp_temp_stack_size_limit) || (lisp_temp_stack_size_limit == @':unlimited')))
    init_params.lisp_temp_stack_size_limit = mkcl_integer_to_index(env, lisp_temp_stack_size_limit);

  thread = mkcl_make_thread(env, name, initial_bindings, &init_params);
  @(return thread);
@)

#if MKCL_PTHREADS
static void print_sig_mask(sigset_t * set)
{
  int i;

  fprintf(stderr, "Blocked: ");
  for (i = 1; i <= MKCL_SIGMAX; i++) {
    if (sigismember(set, i)) { fprintf(stderr, "%d ", i); }
  }
  fprintf(stderr, "\n");
  fflush(stderr);
}

static void print_thread_sig_mask(void)
{
  sigset_t blocked;

  pthread_sigmask(SIG_BLOCK, NULL, &blocked);
  print_sig_mask(&blocked);
}
#endif /* MKCL_PTHREADS */

mkcl_object
mk_mt_show_sigmask(MKCL)
{
#if MKCL_PTHREADS
  mkcl_call_stack_check(env);
  print_thread_sig_mask();
#endif
  @(return mk_cl_Cnil);
}

mkcl_object
mk_mt_reset_sigmask(MKCL)
{
#if MKCL_PTHREADS
  sigset_t all_signals;

  mkcl_call_stack_check(env);
  sigfillset(&all_signals);

  if (pthread_sigmask(SIG_UNBLOCK, &all_signals, NULL))
    mkcl_lose(env, "mk_mt_reset_sigmask failed on pthread_sigmask");
#endif
  @(return mk_cl_Cnil);
}


mkcl_object
mk_mt_block_signals(MKCL)
{
#if MKCL_PTHREADS
  mkcl_object this_thread = mkcl_current_thread(env);

  mkcl_call_stack_check(env);
  if ( this_thread->thread.sigmask_frs_marker == NULL )
    {
      int rc;
      sigset_t all_signals;

      sigfillset(&all_signals); /* block everything! */

      if ((rc = pthread_sigmask(SIG_SETMASK, &all_signals, &(this_thread->thread.saved_sigmask))))
	{ errno = rc; mkcl_FElibc_error(env, "mk_mt_block_signals failed on pthread_sigmask", 0); }
      
      this_thread->thread.sigmask_frs_marker = this_thread->thread.env->frs_top;
    }
#endif
  @(return mk_cl_Cnil);
}

mkcl_object
mk_mt_unblock_signals(MKCL)
{
#if MKCL_PTHREADS
  mkcl_object this_thread = mkcl_current_thread(env);

  mkcl_call_stack_check(env);
  if ( this_thread->thread.sigmask_frs_marker >= this_thread->thread.env->frs_top )
    {
      int rc;
      if ((rc = pthread_sigmask(SIG_SETMASK, &(this_thread->thread.saved_sigmask), NULL)))
	{ errno = rc; mkcl_FElibc_error(env, "mk_mt_unblock_signals failed on pthread_sigmask", 0); }

      this_thread->thread.sigmask_frs_marker = NULL;
    }
#endif
  @(return mk_cl_Cnil);
}

mkcl_object
mk_mt_thread_preset(MKCL, mkcl_narg narg, mkcl_object thread, mkcl_object function, ...)
{
  mkcl_call_stack_check(env);
  if (narg < 2)
    mkcl_FEwrong_num_arguments(env, @'mt::thread-preset', 2, -1, narg);
  mkcl_assert_type_thread(env, thread);

  if (thread->thread.detached)
    mkcl_FEerror(env, "Cannot preset detached thread ~A", 1, thread);

  switch (thread->thread.status)
    {
    case mkcl_thread_initialized:
    case mkcl_thread_set:
      break;
    case mkcl_thread_active:
      mkcl_FEerror(env, "Cannot preset active thread ~A", 1, thread);
      break;
    case mkcl_thread_done:
      mkcl_FEerror(env, "Cannot preset unjoinded thread ~A", 1, thread);
      break;
    default:
      mkcl_FEerror(env, "Cannot preset thread of unknown status: ~A", 1, thread);
      break;
    }

  thread->thread.function = function;
  thread->thread.status = mkcl_thread_set;
  {
    mkcl_va_list args;
    
    mkcl_va_start(env, args, function, narg, 2);
    thread->thread.args = mkcl_grab_rest_args(env, args, FALSE);
    mkcl_va_end(args);
  }
  @(return thread);
}

#if MKCL_PTHREADS
void mkcl_create_signal_servicing_thread(MKCL,
					 char * thread_cname, 
					 int sig,
					 mkcl_object func_designator)
{
  mkcl_object thread_name = mkcl_make_base_string_copy(env, thread_cname);
  struct mkcl_thread_init_parameters init_params = { 0 };
  mkcl_object thread = mkcl_make_thread(env, thread_name, mk_cl_Cnil, &init_params);
  int interrupt_sig = mkcl_get_option(MKCL_OPT_THREAD_INTERRUPT_SIGNAL);
  int wake_up_sig = mkcl_get_option(MKCL_OPT_THREAD_WAKE_UP_SIGNAL);
  sigset_t servicing_thread_sigmask, current_sigmask;
  mkcl_object pthread_arg = MKCL_CONS(env, thread, MKCL_MAKE_FIXNUM(sig));
  int result;

  sigfillset(&servicing_thread_sigmask); /* Block everything. */
  if (sigdelset(&servicing_thread_sigmask, interrupt_sig)) /* almost everything */
    mkcl_FElibc_error(env, "mkcl_create_signal_servicing_thread failed on sigdelset", 0);
  if (sigdelset(&servicing_thread_sigmask, wake_up_sig)) /* almost everything */
    mkcl_FElibc_error(env, "mkcl_create_signal_servicing_thread failed on sigdelset", 0);
  pthread_sigmask(SIG_SETMASK, &servicing_thread_sigmask, &current_sigmask);

  thread->thread.function = func_designator;
  thread->thread.args = mk_cl_Cnil;
  
  thread->thread.running_lock = mkcl_alloc_atomic(env, sizeof(pthread_mutex_t));
  pthread_mutex_init(thread->thread.running_lock, NULL);
  pthread_mutex_lock(thread->thread.running_lock);

  MKCL_GC_NO_INTR(env, result = pthread_create(&thread->thread.thread, NULL,  /* GC redirect */
					       signal_servicing_thread_entry_point,
					       pthread_arg));
  thread->thread.base_thread = thread->thread.thread;

  pthread_mutex_unlock(thread->thread.running_lock);

  pthread_sigmask(SIG_SETMASK, &current_sigmask, NULL);
  if ( result != 0 )
    { errno = result; mkcl_FElibc_error(env, "Cannot create signal servicing thread", 0); }
}
#endif /* MKCL_PTHREADS */

#if MKCL_WINDOWS
static HANDLE mkcl_sleeping_thread_interrupted; /* Event */
#elif MKCL_PTHREADS
mkcl_env mkcl_interrupted_thread_env = NULL;
bool mkcl_interrupt_refused;
bool mkcl_interrupt_forcefully;
static sem_t mkcl_sleeping_thread_interrupted_sem_obj;
static sem_t * mkcl_sleeping_thread_interrupted = &mkcl_sleeping_thread_interrupted_sem_obj;
static sem_t mkcl_interrupted_thread_suspended_sem_obj;
sem_t * mkcl_interrupted_thread_suspended = &mkcl_interrupted_thread_suspended_sem_obj;
static sem_t mkcl_interrupted_thread_resumed_sem_obj;
sem_t * mkcl_interrupted_thread_resumed = &mkcl_interrupted_thread_resumed_sem_obj;
#endif /* MKCL_PTHREADS */

#define MAX_INTERRUPT_RETRIES 50

#if MKCL_PTHREADS
static void create_interrupt_thread(MKCL, mkcl_object thread, mkcl_index os_call_stack_size)
{
  int result;
  pthread_attr_t attr_obj;
  pthread_attr_t * attr = &attr_obj;
  const mkcl_index minimum_cs_size = 2 * thread->thread.env->cs_overflow_size;
  const mkcl_index cs_size = thread->thread.env->cs_size;

  size_t stack_size = ((cs_size < minimum_cs_size) ? minimum_cs_size : cs_size);

  MKCL_LIBC_NO_INTR(env, result = pthread_attr_init(attr));
  if (result)
    { errno = result; mkcl_FElibc_error(env, "Interrupt thread attributes initialization failed for thread ~S", 1, thread); }


  MKCL_LIBC_NO_INTR(env, result = pthread_attr_setstacksize(attr, stack_size));
  if (result)
    { errno = result; mkcl_FElibc_error(env, "Interrupt thread stack size adjustment failed for thread ~S", 1, thread); }

  MKCL_GC_NO_INTR(env, result = pthread_create(&thread->thread.thread, attr,   /* GC redirect */
					       interrupt_thread_entry_point, thread));

  if ( result != 0 )
    { errno = result; mkcl_FElibc_error(env, "create_interrupt_thread failed on pthread_create", 0); }

  pthread_attr_destroy(attr);
  MKCL_LIBC_NO_INTR(env, result = sem_post(mkcl_run_interrupt_function));
  if (result)
    mkcl_FElibc_error(env, "create_interrupt_thread failed on sem_post", 0);
}

#elif MKCL_WINDOWS

static void create_interrupt_thread(MKCL, mkcl_object thread, mkcl_index os_call_stack_size)
{
  uintptr_t code;
  unsigned threadId;
  const mkcl_index cs_size = thread->thread.env->cs_size;
  const mkcl_index minimum_cs_size = 2 * thread->thread.env->cs_overflow_size;
  unsigned stack_size = ((cs_size > UINT_MAX) ? UINT_MAX : ((cs_size < minimum_cs_size) ? minimum_cs_size : cs_size));

 /* GC redirect */
  MKCL_GC_NO_INTR(env, code = _beginthreadex(NULL, stack_size, interrupt_thread_entry_point, thread, CREATE_SUSPENDED, &threadId));
  if (code == 0)
    mkcl_FElibc_error(env, "create_interrupt_thread failed on _beginthreadex for thread ~A", 1, thread);
  else
    thread->thread.thread = (HANDLE) code;

  DWORD old_suspend_count;
  MKCL_LIBC_NO_INTR(env, old_suspend_count = ResumeThread(thread->thread.thread));
  if (old_suspend_count != 1) /* 1 is the only right answer here, everything else is an error of some kind. */
    mkcl_FEwin32_error(env, "create_interrupt_thread failed on ResumeThead for thread ~A", 1, thread);    

  SwitchToThread();
}
#endif

#if MKCL_WINDOWS

/* interrupt_thread_internal() is to be run with interrupts disabled. */
static mkcl_object
interrupt_thread_internal(MKCL, mkcl_object thread, mkcl_object function, mkcl_index os_call_stack_size, mkcl_object force)
{
  if (mk_mt_thread_active_p(env, thread) == mk_cl_Cnil)
    mk_cl_error(env, 5, @'mt::invalid-thread', @':thread', thread, @':reason', @':dead');

  if ( thread == mkcl_current_thread(env) )
    {
      mkcl_funcall0(env, function);
      return mk_cl_Ct;
    }

  {
    bool success = FALSE, failure = FALSE;
    mkcl_object reason = mk_cl_Cnil;
    int retry_count = 0;

    do {
      volatile bool locked = false;
      DWORD wait_val;

      if (thread->thread.status != mkcl_thread_active)
	{
	  reason = @':dead';
	  failure = TRUE;
	  break;
	}

      MKCL_UNWIND_PROTECT_BEGIN(env) {

	MKCL_LIBC_NO_INTR(env, wait_val = WaitForSingleObject(mkcl_interrupt_thread_lock, 1000));
	switch (wait_val)
	  {
	  case WAIT_OBJECT_0: break;
	  case WAIT_TIMEOUT:
	    mkcl_FEwin32_error(env, "interrupt-thread: interrupt lock timeout", 0);
	  case WAIT_ABANDONED:
	  case WAIT_FAILED:
	  default:
	    mkcl_FEwin32_error(env, "interrupt-thread failed to acquire lock", 0);
	  }
	locked = TRUE;

	const int early_disable_interrupts = thread->thread.env->disable_interrupts;
      
	if ( !mkcl_Null(thread->thread.env->sleeping_on) )
	  {
	    if (!ResetEvent(mkcl_sleeping_thread_interrupted))
	      mkcl_FEwin32_error(env, "interrupt-thread failed on ResetEvent", 0);
	    thread->thread.interrupt = function;
	    if (mkcl_Null(mk_mt_try_to_wake_up_thread(env, thread)))
	      retry_count++;
	    else
	      {

		wait_val = WaitForSingleObject(mkcl_sleeping_thread_interrupted, 100); /* 100ms */
		switch (wait_val)
		  {
		  case WAIT_OBJECT_0:
		    success = TRUE;
		    break;
		  case WAIT_TIMEOUT:
		    if (mk_mt_thread_active_p(env, thread) == mk_cl_Cnil)
		      {
			reason = @':dead';
			failure = TRUE;
		      }
		    else if ( !mkcl_Null(thread->thread.env->sleeping_on) )
		      {
			reason = @'mt::thread-sleeping'; /* It's sleeping too hard, wouldn't wake up. */
			failure = TRUE;
		      }
		    else
		      retry_count++; /* like a refused interrupt */
		    break;
		  case WAIT_ABANDONED:
		  case WAIT_FAILED:
		  default:
		    mkcl_FEwin32_error(env, "interrupt-thread failed while waiting for sleeping thread interrupted event", 0);
		  }
	      }
	  }
	else if (early_disable_interrupts && ((early_disable_interrupts > 1) || (force == mk_cl_Cnil)))
	  {	/* The interruption is refused early. */
	    retry_count++;
	  }
	else if ( (thread->thread.status == mkcl_thread_active) && (thread->thread.interrupt_count < MKCL_MAX_INTERRUPTS) )
	  {
	    DWORD suspend_count = 0;
	    HANDLE os_thread = thread->thread.thread;

	    if (os_thread && (suspend_count = SuspendThread(os_thread)) == (DWORD)-1)
	      mkcl_FEwin32_error(env, "Cannot suspend thread ~A", 1, thread);

	    const mkcl_env target_env = thread->thread.env;

	    if ( suspend_count != 0 )
	      {
		/* The thread is already suspended!
		   What should we do? This should never happen!!
		*/
		DWORD count;
		failure = TRUE;
		reason = @':suspended';
		/* "resume" to set suspend_count back to its previous value. */
		if ((count = ResumeThread(os_thread)) == (DWORD)-1)
		  mkcl_FEwin32_error(env, "Cannot resume thread ~A", 1, thread);
	      }
	    else if ((target_env == NULL) /* The thread died unexpectedly! */
		     || (os_thread == NULL)
		     || (target_env->own_thread->thread.status == mkcl_thread_done) )
	      { /* The thread is dying on us! */
		DWORD count;
		reason = @':dead';
		failure = TRUE;
		/* "resume" to let thread complete its clean-up. */
		if ((count = ResumeThread(os_thread)) == (DWORD)-1)
		  mkcl_FEwin32_error(env, "Cannot resume thread ~A", 1, thread);
	      }
	    else if ((target_env->disable_interrupts > 1) /* being interrupted already! */
		     || (target_env->disable_interrupts && (force == mk_cl_Cnil)) )
	      {	/* The interruption is refused. */
		DWORD previous_suspend_count;
		retry_count++;
	      
		if ((previous_suspend_count = ResumeThread(os_thread)) == (DWORD)-1)
		  mkcl_FEwin32_error(env, "Cannot resume thread ~A", 1, thread);
	      }
	    else
	      {	/* The interruption is accepted. */
		/* Push interrupted thread on stack */
		int i = thread->thread.interrupt_count;
		struct interrupted_thread_ctrl * p 
		  = &(thread->thread.interrupted_threads[i]);

		p->thread_ident = thread->thread.thread;

		p->cs_org = target_env->cs_org;
		p->cs_limit = target_env->cs_limit;
		p->cs_size = target_env->cs_size;
		p->cs_overflow_size = target_env->cs_overflow_size;
		p->cs_overflowing = target_env->cs_overflowing;

		p->disable_interrupts = target_env->disable_interrupts;
#if MKCL_DEBUG_INTERRUPT_MASK
		p->interrupt_disabler_file = target_env->interrupt_disabler_file;
		p->interrupt_disabler_lineno = target_env->interrupt_disabler_lineno;
#endif

		thread->thread.interrupt_count = i + 1; /* Pushed on stack.*/

		thread->thread.interrupt = function;

		target_env->disable_interrupts = 2; /* This flags an interrupt in progress. */
#if MKCL_DEBUG_INTERRUPT_MASK
		target_env->interrupt_disabler_lineno = __LINE__;
		target_env->interrupt_disabler_file = __FILE__;
#endif

		/* Preserve interrupted thread values. */
		p->nvalues = target_env->nvalues;
		{
		  int j;
		  const int max = p->nvalues;
		  mkcl_object * const values = p->values;
		  mkcl_object * const tenv_values = target_env->values;

		  for (j = 0; j < max; j++) values[j] = tenv_values[j];
		}

		/* Create interruption sub-thread here */
		create_interrupt_thread(env, thread, os_call_stack_size);

		success = TRUE;
	      }
	  }
	else
	  {
	    retry_count++;
	  }

      } MKCL_UNWIND_PROTECT_EXIT {
	if (locked)
	  if (!ReleaseMutex(mkcl_interrupt_thread_lock))
	    mkcl_FEwin32_error(env, "interrupt-thread failed to release lock", 0);
      } MKCL_UNWIND_PROTECT_END;

      if ( !(success || failure) )
	SwitchToThread();
    } while ( !(retry_count >= MAX_INTERRUPT_RETRIES || success || failure) );

    if ( retry_count >= MAX_INTERRUPT_RETRIES)
      {
#if MKCL_DEBUG_INTERRUPT_MASK
	mk_cl_error(env, 7, @'mt::interrupt-refused',
		    @':thread', thread,
		    @':file', mkcl_cstring_to_string(env, (char *) thread->thread.env->interrupt_disabler_file),
		    @':lineno', MKCL_MAKE_FIXNUM(thread->thread.env->interrupt_disabler_lineno));
#else
	mk_cl_error(env, 3, @'mt::interrupt-refused', @':thread', thread);
#endif
	return mk_cl_Cnil;
      }
    else if ( failure )
      {
	if ( reason == @'mt::thread-sleeping')
	  {
#if MKCL_DEBUG_INTERRUPT_MASK
	    mk_cl_error(env, 7, @'mt::thread-sleeping',
			@':thread', thread,
			@':file', mkcl_cstring_to_string(env, (char *) thread->thread.env->interrupt_disabler_file),
			@':lineno', MKCL_MAKE_FIXNUM(thread->thread.env->interrupt_disabler_lineno));
#else
	    mk_cl_error(env, 3, @'mt::thread-sleeping', @':thread', thread);
#endif
	  }
	else
	  mk_cl_error(env, 5, @'mt::invalid-thread', @':thread', thread, @':reason', reason);
	return mk_cl_Cnil;
      }
    else if ( success )
      { 
	return mk_cl_Ct;
      }
    else
      mkcl_lose(env, "interrupt-thread is really confused");
  }
  return mk_cl_Ct;
}

#elif MKCL_PTHREADS

static void clear_semaphore(MKCL, sem_t * sem)
{
  int rc;

  do {
    MKCL_LIBC_NO_INTR(env, rc = sem_trywait(sem));
  } while ( rc == 0 || errno == EINTR);
  if (rc && errno != EAGAIN)
    mkcl_FElibc_error(env, "clear_semaphore failed on sem_trywait", 0);
}

/* interrupt_thread_internal() is to be run with interrupts disabled. */
static mkcl_object
interrupt_thread_internal(MKCL, mkcl_object thread, mkcl_object function, mkcl_index os_call_stack_size, mkcl_object force)
{
  if (mk_mt_thread_active_p(env, thread) == mk_cl_Cnil)
    /* mkcl_FEerror(env, "Cannot interrupt an inactive thread ~A", 1, thread); */
    mk_cl_error(env, 5, @'mt::invalid-thread', @':thread', thread, @':reason', @':dead');

  if ( thread == mkcl_current_thread(env) )
    {
      mkcl_funcall0(env, function);
      return mk_cl_Ct;
    }

  {
    int sig = mkcl_get_option(MKCL_OPT_THREAD_INTERRUPT_SIGNAL);
    sigset_t no_interrupt_sigmask, current_sigmask;
    bool success = FALSE, failure = FALSE;
    mkcl_object reason = mk_cl_Cnil;
    int retry_count = 0;

    mkcl_interrupt_forcefully = ((force == mk_cl_Cnil) ? FALSE : TRUE);

    sigemptyset(&no_interrupt_sigmask);
    if (sigaddset(&no_interrupt_sigmask, sig))
      mkcl_lose(env, "mk_mt_interrupt_thread failed on sigaddset");

    do {
      volatile bool locked = false;
      int rc; 

      if (mk_mt_thread_active_p(env, thread) == mk_cl_Cnil)
	{
	  reason = @':dead';
	  failure = TRUE;
	  break;
	}

      MKCL_UNWIND_PROTECT_BEGIN(env) {

	/* adjust sigmask to prevent interrupts during the critical section */
	pthread_sigmask(SIG_BLOCK, &no_interrupt_sigmask, &current_sigmask);

	MKCL_LIBC_NO_INTR(env, rc = pthread_mutex_lock(&mkcl_interrupt_thread_lock));
	if (rc)
	  mkcl_lose(env, "mk_mt_interrupt_thread failed on pthread_mutex_lock");
	locked = TRUE;

	if ( thread->thread.interrupt_count < MKCL_MAX_INTERRUPTS )
	  {
	    int rc = 0;
	    struct mkcl_env_struct *tenv = thread->thread.env;
	    mkcl_interrupted_thread_env = tenv;
	    mkcl_interrupt_refused = FALSE;

	    if ( !mkcl_Null(tenv->sleeping_on) )
	      {
		clear_semaphore(env, mkcl_sleeping_thread_interrupted);
		thread->thread.interrupt = function;
		if (mkcl_Null(mk_mt_try_to_wake_up_thread(env, thread)))
		  retry_count++;
		else
		  {
		    struct timespec timeout;

		    rc = clock_gettime(CLOCK_REALTIME, &timeout);
		    if (rc)
		      mkcl_FElibc_error(env, "mk_mt_interrupt_thread failed on clock_gettime", 0);

		    timeout.tv_nsec += 500 * 1000000; /* 100ms */
		    if (timeout.tv_nsec >= 1000000000) {
		      timeout.tv_nsec -= 1000000000;
		      timeout.tv_sec++;
		    }

		    do {
		      MKCL_LIBC_NO_INTR(env, rc = sem_timedwait(mkcl_sleeping_thread_interrupted, &timeout));
		    } while ( rc && errno == EINTR );
		    if ( rc == 0 )
		      success = TRUE;
		    else
		      if ( errno == ETIMEDOUT )
			{
			  if (mk_mt_thread_active_p(env, thread) == mk_cl_Cnil)
			    {
			      reason = @':dead';
			      failure = TRUE;
			    }
			  else if ( !mkcl_Null(tenv->sleeping_on) )
			    {
			      reason = @'mt::thread-sleeping'; /* It's sleeping too hard, wouldn't wake up. */
			      failure = TRUE;
			    }
			  /* else it is not really clear if this is an error case or not. JCB */
			  else
			    retry_count++; /* like a refused interrupt */

			}
		      else
			{
			  mkcl_C_lose(env, "mk_mt_interrupt_thread failed on sem_wait");
			}
		  }
		mkcl_interrupted_thread_env = NULL; /* done with it. clear it. */
	      }
	    else 

	      /* send signal to interrupt (suspend) thread. */
	      if ((rc = pthread_kill(thread->thread.thread, sig)))
		switch (rc /* errno */)
		  {
		  case ESRCH:
		    /* The targetted thread does not exist. */
		    reason = @':dead';
		    failure = TRUE;
		    mkcl_interrupted_thread_env = NULL; /* done with it. clear it. */
		    break;
		  case EINVAL:
		    mkcl_lose(env, "mk_mt_interrupt_thread: invalid signal used with pthread_kill");
		    break;
		  default:
		    mkcl_lose(env, "mk_mt_interrupt_thread failed on pthread_kill");
		    break;
		  }
	      else
		{
#if 1
		  struct timespec timeout;

		  rc = clock_gettime(CLOCK_REALTIME, &timeout);
		  if (rc)
		    mkcl_FElibc_error(env, "mk_mt_interrupt_thread failed on clock_gettime", 0);

		  timeout.tv_nsec += 10 * 1000000; /* 10ms */
		  if (timeout.tv_nsec >= 1000000000) {
		    timeout.tv_nsec -= 1000000000;
		    timeout.tv_sec++;
		  }

		  do {
		    MKCL_LIBC_NO_INTR(env, rc = sem_timedwait(mkcl_interrupted_thread_suspended, &timeout));
		  } while ( rc && errno == EINTR );
		  if ( rc )
		    {
		      if ( errno == ETIMEDOUT ) mkcl_interrupt_refused = TRUE;
		      else mkcl_C_lose(env, "mk_mt_interrupt_thread failed on sem_wait");
		    }
#else
		  do { /* should this be timed? What if the interruptee dies on us? JCB */
		    MKCL_LIBC_NO_INTR(env, rc = sem_wait(mkcl_interrupted_thread_suspended));
		  } while ( rc && errno == EINTR );
		  if ( rc ) mkcl_C_lose(env, "mk_mt_interrupt_thread failed on sem_wait");
#endif
		  mkcl_interrupted_thread_env = NULL; /* done with it. clear it. */
	  
		  if ( mkcl_interrupt_refused )
		    retry_count++;
		  else
		    { /* Push interrupted thread on stack */
		      int count = thread->thread.interrupt_count;
		      struct interrupted_thread_ctrl * p = &(thread->thread.interrupted_threads[count]);

		      p->thread_ident = thread->thread.thread;

		      p->cs_org = tenv->cs_org;
		      p->cs_limit = tenv->cs_limit;
		      p->cs_size = tenv->cs_size;
		      p->cs_overflow_size = tenv->cs_overflow_size;
		      p->cs_overflowing = tenv->cs_overflowing;

		      p->disable_interrupts = tenv->disable_interrupts;
#if MKCL_DEBUG_INTERRUPT_MASK
		      p->interrupt_disabler_file = tenv->interrupt_disabler_file;
		      p->interrupt_disabler_lineno = tenv->interrupt_disabler_lineno;
#endif

		      thread->thread.interrupt_count = count + 1; /* Pushed on stack.*/

		      thread->thread.interrupt = function;

		      tenv->disable_interrupts = 2; /* This flags an interrupt in progress. */
#if MKCL_DEBUG_INTERRUPT_MASK
		      tenv->interrupt_disabler_lineno = __LINE__;
		      tenv->interrupt_disabler_file = __FILE__;
#endif

		      /* Preserve interrupted thread values. */
		      p->nvalues = tenv->nvalues;
		      {
			int j;
			const int max = p->nvalues;
			mkcl_object * const values = p->values;
			mkcl_object * const tenv_values = tenv->values;
		    
			for (j = 0; j < max; j++) values[j] = tenv_values[j];
		      }
		  
		      /* Create interruption sub-thread here */
		      create_interrupt_thread(env, thread, os_call_stack_size);

		      success = TRUE;
		    }
		}
	  }
	else
	  {
	    retry_count++;
	  }

      } MKCL_UNWIND_PROTECT_EXIT {
	if (locked)
	  if (pthread_mutex_unlock(&mkcl_interrupt_thread_lock))
	    mkcl_lose(env, "mk_mt_interrupt_thread failed on pthread_mutex_unlock");

	MKCL_LIBC_NO_INTR(env, pthread_sigmask(SIG_SETMASK, &current_sigmask, NULL));
      } MKCL_UNWIND_PROTECT_END;

      if ( !(success || failure) )
	MKCL_LIBC_NO_INTR(env, sched_yield());
    } while ( !(retry_count >= MAX_INTERRUPT_RETRIES || success || failure) );

    if ( retry_count >= MAX_INTERRUPT_RETRIES)
      {
#if MKCL_DEBUG_INTERRUPT_MASK
	mk_cl_error(env, 7, @'mt::interrupt-refused',
		    @':thread', thread,
		    @':file', mkcl_cstring_to_string(env, (char *) thread->thread.env->interrupt_disabler_file),
		    @':lineno', MKCL_MAKE_FIXNUM(thread->thread.env->interrupt_disabler_lineno));
#else
	mk_cl_error(env, 3, @'mt::interrupt-refused', @':thread', thread);
#endif
	return mk_cl_Cnil;
      }
    else if ( failure )
      {
	if ( reason == @'mt::thread-sleeping')
	  {
#if MKCL_DEBUG_INTERRUPT_MASK
	    mk_cl_error(env, 7, @'mt::thread-sleeping',
			@':thread', thread,
			@':file', mkcl_cstring_to_string(env, (char *) thread->thread.env->interrupt_disabler_file),
			@':lineno', MKCL_MAKE_FIXNUM(thread->thread.env->interrupt_disabler_lineno));
#else
	    mk_cl_error(env, 3, @'mt::thread-sleeping', @':thread', thread);
#endif
	  }
	else
	  mk_cl_error(env, 5, @'mt::invalid-thread', @':thread', thread, @':reason', reason);
	return mk_cl_Cnil;
      }
    else if ( success )
      { 
	return mk_cl_Ct;
      }
    else
      mkcl_lose(env, "interrupt-thread is really confused");
  }
  return mk_cl_Ct;
}

#endif /* MKCL_PTHREADS */

@(defun mt::interrupt-thread (thread function &key (force mk_cl_Cnil) call_stack_size)
@
  mkcl_object val;
  mkcl_index os_call_stack_size;

  if (!mkcl_Null(call_stack_size))
    os_call_stack_size = mkcl_integer_to_index(env, call_stack_size);
  else
    os_call_stack_size = mkcl_get_option(MKCL_OPT_INTERRUPT_THREAD_CALL_STACK_SIZE);

  MKCL_UNWIND_PROTECT_BEGIN(env) {
    MKCL_LIBC_NO_INTR(env, val = interrupt_thread_internal(env, thread, function, os_call_stack_size, force));
  } MKCL_UNWIND_PROTECT_EXIT {
  } MKCL_UNWIND_PROTECT_END;
  @(return val);
@)

mkcl_object
mk_mt_thread_kill(MKCL, mkcl_object thread)
{
  mkcl_object val = MKCL_MAKE_FIXNUM(MKCL_THREAD_CANCELED);

  mkcl_call_stack_check(env);
  mkcl_assert_type_thread(env, thread);
  MKCL_UNWIND_PROTECT_BEGIN(env) {
    MKCL_LIBC_NO_INTR(env, val = interrupt_thread_internal(env, thread, @'mt::abort-thread', 0, mk_cl_Ct));
  } MKCL_UNWIND_PROTECT_EXIT {
  } MKCL_UNWIND_PROTECT_END;

#if 0   /* a thread removes itself from the threads list if it needs to. JCB */
  {
    volatile bool locked = false;

    MKCL_UNWIND_PROTECT_BEGIN(env) {
      MKCL_LIBC_NO_INTR(env, (MKCL_THREAD_LIST_LOCK(), locked = true));
      thread->thread.result_value = val; /* For whose sake? JCB */
      mkcl_core.threads = mkcl_remove_eq(env, thread, mkcl_core.threads);
    } MKCL_UNWIND_PROTECT_EXIT {
      if (locked) MKCL_THREAD_LIST_UNLOCK();
    } MKCL_UNWIND_PROTECT_END;
  }
#endif

  @(return val);
}

mkcl_object mk_mt_thread_detach(MKCL, mkcl_object thread)
{
  mkcl_call_stack_check(env);
  if (mkcl_Null(thread))
    thread = mkcl_current_thread(env);
  else if (mkcl_type_of(thread) != mkcl_t_thread)
    thread = mkcl_type_error(env, @'mt::thread-detach', "argument", thread, @'mt::thread');

  if ((thread->thread.status != mkcl_thread_active) || thread->thread.detached || (thread->thread.thread == 0))
    { @(return mk_cl_Cnil); }
  else
    {
#if MKCL_PTHREADS
# if 0 /* Disabled for the following reason: */
      /*
	In Pthreads the concept of thread ID seems to be severely under-specified,
	mainly when it comes to the "extent" of a thread ID when the said thread
	has been subject to a pthread_detach() call. Basically one has to
	understand that once pthread_detach() has been applied to a thread then
	its thread ID has henceforth "undefined extent" and can become invalid
	at any point in the future without any notice.

	This is even more so in pthread implementations (such as NPTL on Linux) where
	the thread ID and the stack segment are merely a cast away from each other.
	Since a detached thread is allowed (and expected) to deallocate its
	stack segment as soon as it terminates, and that termination can happen
	at any time, so goes the validity of the thread ID.
	To make matters worse, Linux NPTL recycles stack segments through an
	internal cache and thus recycles thread IDs without warning.

	Not only can you have severe thread identity crises with this but
	you can even get some SIGSEGV on pthread_XXXX() calls in some configurations
	if you happen unknowingly to pass the pthread call a now invalid thread ID.
       */
      volatile bool locked = false;
      mkcl_os_thread_t base_thread;
      int rc;

      MKCL_UNWIND_PROTECT_BEGIN(env) {
	mkcl_interrupt_status old_intr;

	mkcl_get_interrupt_status(env, &old_intr);
	mkcl_disable_interrupts(env);
	if ((rc = pthread_mutex_lock(&mkcl_interrupt_thread_lock)))
	  mkcl_lose(env, "mk_mt_thread_detach failed on pthread_mutex_lock");
	else
	  locked = true;
	
	if ( thread->thread.interrupt_count )
	  base_thread = thread->thread.interrupted_threads[0].thread_ident;
	else
	  base_thread = thread->thread.thread;
	
	rc = pthread_detach(base_thread);
	mkcl_set_interrupt_status(env, &old_intr);
	if (rc)
	  mkcl_FElibc_error(env, "mk_mt_thread_detach failed on pthread_detach()", 0);

      } MKCL_UNWIND_PROTECT_EXIT {
	if (locked)
	  if ((rc = pthread_mutex_unlock(&mkcl_interrupt_thread_lock)))
	    mkcl_lose(env, "mk_mt_thread_detach failed on pthread_mutex_unlock");
      } MKCL_UNWIND_PROTECT_END;
# endif
#elif MKCL_WINDOWS
#endif
      thread->thread.detached = TRUE;
    }

  if (thread->thread.status == mkcl_thread_done)
    mkcl_remove_thread_from_global_thread_list(env, thread);

  @(return mk_cl_Ct);
}

#if MKCL_PTHREADS

mkcl_object
mk_mt_thread_join(MKCL, mkcl_object thread)
{
  mkcl_os_thread_t base_thread;
  mkcl_object result_value = mk_cl_Cnil;
  void * status;
  int rc = 0;

  mkcl_call_stack_check(env);
  if (mk_mt_thread_active_p(env, thread) != mk_cl_Cnil )
    {
      mkcl_interrupt_status old_intr;

      mkcl_get_interrupt_status(env, &old_intr);
      mkcl_disable_interrupts(env);
      if ((rc = pthread_mutex_lock(&mkcl_interrupt_thread_lock)))
	mkcl_lose(env, "mk_mt_thread_join failed on pthread_mutex_lock");

      if ( thread->thread.interrupt_count )
	base_thread = thread->thread.interrupted_threads[0].thread_ident;
      else
	base_thread = thread->thread.thread;
      
      if ((rc = pthread_mutex_unlock(&mkcl_interrupt_thread_lock)))
	mkcl_lose(env, "mk_mt_thread_join failed on pthread_mutex_unlock");
      mkcl_set_interrupt_status(env, &old_intr);
    }
  else
    {
      base_thread = thread->thread.thread;
    }

  if ( thread->thread.detached )
    {
      if (thread->thread.status == mkcl_thread_done)
	mkcl_remove_thread_from_global_thread_list(env, thread);
      @(return @':detached');
    }
  else if (thread == mkcl_core.initial_thread)
    {
      /* Boehm's GC dumps core if you try to "join" the initial thread.
	 So we try to bypass it's pthread_join() redirection. */
      MKCL_LIBC_REALLY_NO_INTR(env, rc = _true_pthread_join(base_thread, &status));  /* Not GC redirect but playing it safe */
      if (rc) { errno = rc; mkcl_FElibc_error(env, "mk_mt_thread_join failed on _true_pthread_join", 0); }
    }
  else
    {
      MKCL_GC_NO_INTR(env, rc = pthread_join(base_thread, &status));  /* GC redirect */
      if (rc) { errno = rc; mkcl_FElibc_error(env, "mk_mt_thread_join failed on pthread_join", 0); }
    }

  result_value = thread->thread.result_value;
  if (result_value == MKCL_OBJNULL)
    result_value = @':invalid-value';

  mkcl_remove_thread_from_global_thread_list(env, thread);

  thread->thread.base_thread = thread->thread.thread = 0;
  thread->thread.tid = 0;
  thread->thread.status = mkcl_thread_initialized;

  @(return result_value);
}

#elif MKCL_WINDOWS

mkcl_object
mk_mt_thread_join(MKCL, mkcl_object thread)
{
  mkcl_os_thread_t handle;
  mkcl_object result_value;
  DWORD exitCode;

  mkcl_call_stack_check(env);
  if (mk_mt_thread_active_p(env, thread) != mk_cl_Cnil )
    {
      mkcl_interrupt_status old_intr;
      
      mkcl_get_interrupt_status(env, &old_intr);
      mkcl_disable_interrupts(env);
      switch (WaitForSingleObject(mkcl_interrupt_thread_lock, INFINITE))
	{
	case WAIT_OBJECT_0: break;
	case WAIT_TIMEOUT:
	case WAIT_ABANDONED:
	case WAIT_FAILED:
	default:
	  mkcl_FEwin32_error(env, "thread-join failed to acquire lock", 0);
	}

      if ( thread->thread.interrupt_count )
	{
	  handle = thread->thread.interrupted_threads[0].thread_ident;
	}
      else
	handle = thread->thread.thread;

      if (!ReleaseMutex(mkcl_interrupt_thread_lock))
	mkcl_FEwin32_error(env, "thread-join failed to release interrupt lock", 0);
      mkcl_set_interrupt_status(env, &old_intr);
    }
  else
    handle = thread->thread.thread;

  if ( thread->thread.detached )
    { 
      if (thread->thread.status == mkcl_thread_done)
	mkcl_remove_thread_from_global_thread_list(env, thread);
      @(return @':detached');
    }
  else
    {
      BOOL ok;
      DWORD wait_val;

      do {
	MKCL_LIBC_Zzz(env, @':io', wait_val = WaitForSingleObjectEx(handle, INFINITE, TRUE));
      } while (wait_val == WAIT_IO_COMPLETION);
      switch (wait_val)
	{
	case WAIT_OBJECT_0: break; /* The normal case. */
	case WAIT_TIMEOUT:
	case WAIT_FAILED:
	default:
	  mkcl_FEwin32_error(env, "mk_mt_thread_join failed on WaitForSingleObject", 0);
	  break;
	}
      mk_mt_test_for_thread_shutdown(env);

      MKCL_LIBC_NO_INTR(env, ok = GetExitCodeThread(handle, &exitCode));
      if (!ok)
	mkcl_FEwin32_error(env, "mk_mt_thread_join failed on GetExitCodeThread", 0);

#if 0
      if (exitCode == MKCL_THREAD_NORMAL_EXIT)
	result_value = thread->thread.result_value;
      else
	result_value = MKCL_MAKE_FIXNUM(-1);
#else
      result_value = thread->thread.result_value;
      if (result_value == MKCL_OBJNULL)
	result_value = @':invalid-value';
#endif
    }

  mkcl_remove_thread_from_global_thread_list(env, thread);

  CloseHandle(handle);
  /* Make sure we do not close the handle again in the finalizer. */
  thread->thread.interrupted_threads[0].thread_ident = thread->thread.base_thread = thread->thread.thread = NULL;
  thread->thread.tid = 0;

  thread->thread.status = mkcl_thread_initialized;

  @(return result_value);
}

#endif /* MKCL_WINDOWS */

mkcl_object
mk_mt_thread_yield(MKCL)
{
#if MKCL_WINDOWS
  MKCL_LIBC_NO_INTR(env, SwitchToThread());
#elif defined(HAVE_SCHED_YIELD)
  MKCL_LIBC_NO_INTR(env, sched_yield());
#else
  MKCL_LIBC_NO_INTR(env, sleep(0)); /* Use sleep(0) to yield to a >= priority thread */
#endif
  @(return);
}

#if MKCL_WINDOWS
mkcl_object
mk_mt_thread_enable(MKCL, mkcl_object thread)
{
  mkcl_object output;
  uintptr_t code;

  mkcl_call_stack_check(env);
  mkcl_assert_type_thread(env, thread);
  if ((thread->thread.status != mkcl_thread_set) || thread->thread.detached)
    mkcl_FEerror(env, "Cannot enable thread ~A", 1, thread);

  mk_mt_test_for_thread_shutdown(env);

  if (!mkcl_Null(mkcl_core.shutdown_thread) && (thread != mkcl_core.shutdown_thread)) /* There is a shutdown in progress */
    { @(return mk_cl_Cnil); }

  if (thread->thread.thread)  /* This thread was most probably reused. */
    MKCL_LIBC_NO_INTR(env, CloseHandle(thread->thread.thread));

  /* make sure the rest of the status info is not carried over. */
  thread->thread.shutdown_requested = FALSE;

  {
    unsigned threadId;
    const mkcl_index cs_size = thread->thread.env->cs_size_request;
    const mkcl_index minimum_cs_size = 2 * thread->thread.env->cs_overflow_size;
    unsigned stack_size = ((cs_size > UINT_MAX) ? UINT_MAX : ((cs_size < minimum_cs_size) ? minimum_cs_size : cs_size));

    /* GC redirect */
    MKCL_GC_NO_INTR(env, code = _beginthreadex(NULL, stack_size, thread_entry_point, thread, CREATE_SUSPENDED, &threadId));
    if (code == 0)
      mkcl_FElibc_error(env, "mt::thread-enable failed on _beginthreadex for thread ~S", 1, thread);
  }
  output = (thread->thread.thread = (HANDLE) code) ? thread : mk_cl_Cnil;

  DWORD old_suspend_count;
  MKCL_LIBC_NO_INTR(env, old_suspend_count = ResumeThread(thread->thread.thread));
  if (old_suspend_count != 1) /* 1 is the only right answer here, everything else is an error of some kind. */
    mkcl_FEwin32_error(env, "create_interrupt_thread failed on ResumeThead for thread ~A", 1, thread);    

  @(return output);
}

#elif MKCL_PTHREADS

mkcl_object
mk_mt_thread_enable(MKCL, mkcl_object thread)
{
  mkcl_object output;
  int create_code, code;

  mkcl_call_stack_check(env);
  mkcl_assert_type_thread(env, thread);
  if ((thread->thread.status != mkcl_thread_set) || thread->thread.detached)
    mkcl_FEerror(env, "Cannot enable thread ~A", 1, thread);

  mk_mt_test_for_thread_shutdown(env);

  if (!mkcl_Null(mkcl_core.shutdown_thread) && (thread != mkcl_core.shutdown_thread)) /* There is a shutdown in progress */
    { @(return mk_cl_Cnil); }

  /* make sure the rest of the status info is not carried over. */
  thread->thread.shutdown_requested = FALSE;

#if 0
  if (thread->thread.running_lock) /* This thread was most probably reused. Should never happen. */
    pthread_mutex_destroy(thread->thread.running_lock);
#endif
  thread->thread.running_lock = mkcl_alloc_atomic(env, sizeof(pthread_mutex_t));
  pthread_mutex_init(thread->thread.running_lock, NULL);

  {
#if 0
    pthread_attr_t * attr = mkcl_alloc_atomic(env, sizeof(pthread_attr_t));
#else
    pthread_attr_t attr_obj;
    pthread_attr_t * attr = &attr_obj;
#endif
    const mkcl_index minimum_cs_size = 2 * thread->thread.env->cs_overflow_size;
    const mkcl_index cs_size = thread->thread.env->cs_size_request;
    void * const cs_org = thread->thread.env->cs_org_request;

    MKCL_LIBC_NO_INTR(env, code = pthread_attr_init(attr));
    if (code)
      { errno = code; mkcl_FElibc_error(env, "Thread attributes initialization failed for thread ~S", 1, thread); }

#if 0
    printf("\nMKCL: about to create a threads with cs_size = %lu, cs_org = %p\n", cs_size, cs_org);
    fflush(NULL);
#endif

    if (cs_org && cs_size)
      { /* The CGC blows up with a SIGSEGV if the stack is not at least twice the phtread minimum! */
	size_t size = ((cs_size < minimum_cs_size) ? minimum_cs_size : cs_size);

#if 0 /* debug JCB */
	printf("\nMKCL: setting thread stack address to %p, and size to %lu", cs_org, size);
	fflush(NULL);
#endif

#if __linux
	MKCL_LIBC_NO_INTR(env, code = pthread_attr_setstack(attr, cs_org, size));
	if (code)
	  { errno = code; mkcl_FElibc_error(env, "Thread stack location and size adjustment failed for thread ~S", 1, thread); }
#else
	MKCL_LIBC_NO_INTR(env, code = pthread_attr_setstacksize(attr, size));
	if (code) 
	  { errno = code; mkcl_FElibc_error(env, "Thread stack size adjustment failed for thread ~S", 1, thread); }
	MKCL_LIBC_NO_INTR(env, code = pthread_attr_setstackaddr(attr, cs_org));
	if (code) 
	  { errno = code; mkcl_FElibc_error(env, "Thread stack location adjustment failed for thread ~S", 1, thread); }

#endif
      }
    else if (cs_size)
      {
	size_t size = ((cs_size < minimum_cs_size) ? minimum_cs_size : cs_size);

#if 0 /* debug JCB */
	printf("\nMKCL: setting thread stack size to %lu", size);
	fflush(NULL);
#endif

	MKCL_LIBC_NO_INTR(env, code = pthread_attr_setstacksize(attr, size));
	if (code)
	  { errno = code; mkcl_FElibc_error(env, "Thread stack size adjustment failed for thread ~S", 1, thread); }
      }

    MKCL_LIBC_NO_INTR(env, code = pthread_mutex_lock(thread->thread.running_lock));
    if (code)
      { errno = code; mkcl_FElibc_error(env, "Startup synchronization initiation failed for thread ~S", 1, thread); }

    thread->thread.thread = 0;
    MKCL_GC_NO_INTR(env, create_code = pthread_create(&thread->thread.thread, attr, thread_entry_point, thread)); /* GC redirect */
    if (create_code == 0) thread->thread.base_thread = thread->thread.thread;

    MKCL_LIBC_NO_INTR(env, code = pthread_mutex_unlock(thread->thread.running_lock));
    if (code)
      { errno = code; mkcl_FElibc_error(env, "Startup synchronization initiation failed for thread ~S", 1, thread); }
    MKCL_LIBC_NO_INTR(env, code = pthread_attr_destroy(attr));
    if (code)
      { errno = code; mkcl_FElibc_error(env, "Thread attributes cleanup failed for thread ~S", 1, thread); }
  }

  switch (create_code)
    {
    case 0:
      output = thread;
      break;
    case EAGAIN:
    case EINVAL:
    case EPERM: /* These should raise a condition instead, with more info. JCB */
      errno = create_code;
      mkcl_FElibc_error(env, "Thread creation failed for thread ~S", 1, thread);
      output = mk_cl_Cnil;
      break;
    default: /* maybe we should lose on this one. (its undocumented!) JCB */
      errno = create_code;
      mkcl_FElibc_error(env, "Thread creation failed for thread ~S", 1, thread);
      output = mk_cl_Cnil;
      break;
    }
  @(return output);
}
#endif /* MKCL_PTHREADS */

mkcl_object mk_mt_abandon_thread(MKCL, mkcl_object result_value)
{
  mkcl_call_stack_check(env);
  mkcl_disable_interrupts(env);

  {
    mkcl_object current_result_value = env->own_thread->thread.result_value;
      
    if (!(current_result_value == @':imported-and-gc-registered' || current_result_value == @':imported'))
      env->own_thread->thread.result_value = result_value;
  }

  mkcl_unwind(env, NULL); /* With NULL there is no stack unwinding done. We jump directly to the root of the stack. */

  /* If we get here it means that the thread failed to do its jump back to the stack root
     and is most probably damaged pretty much beyond repair!
     So, all we can really do at this point is to use
     very drastic means to bring this to an end. JCB
   */

#if 0 /* All of this is much too dangerous to be worth the risk. JCB */
  int i = env->own_thread->thread.interrupt_count;

  for (; i; --i)
    { /* These system calls here are strictly "fire and forget".
	 Testing for return status could send us into an infinite recursion. */
#if MKCL_PTHREADS
      pthread_cancel(env->own_thread->thread.interrupted_threads[i-1].thread_ident);
#elif MKCL_WINDOWS
      HANDLE handle = env->own_thread->thread.interrupted_threads[i-1].thread_ident;

      if (handle)
	{
	  TerminateThread(handle, 0);
	  CloseHandle(handle);
	  env->own_thread->thread.interrupted_threads[i-1].thread_ident = NULL;
	}
#endif
    }
#endif

  mkcl_thread_exit(env, MKCL_THREAD_ABORTED);
}

mkcl_object
mk_mt_exit_thread(MKCL, mkcl_object result_value)
{
  /* We simply undo the whole of the frame stack. This brings us
     back to the thread entry point, going through all possible
     UNWIND-PROTECT.
  */

  mkcl_call_stack_check(env);
  mkcl_disable_interrupts(env);
  {
    mkcl_object current_result_value = env->own_thread->thread.result_value;
      
    if (!(current_result_value == @':imported-and-gc-registered' || current_result_value == @':imported'))
      env->own_thread->thread.result_value = result_value;
  }

  mkcl_unwind(env, env->frs_org);

  /* If we get here it means that the thread failed to do a global unwind
     and is most probably damaged pretty much beyond repair!
     So, all we can really do at this point is to use
     very drastic means to bring this to an end. JCB
   */

#if 0 /* All of this is much too dangerous to be worth the risk. JCB */
  int i = env->own_thread->thread.interrupt_count;

  for (; i; --i)
    { /* These system calls here are strictly "fire and forget".
	 Testing for return status could send us into an infinite recursion. */
#if MKCL_PTHREADS
      pthread_detach(env->own_thread->thread.interrupted_threads[i-1].thread_ident);
      pthread_cancel(env->own_thread->thread.interrupted_threads[i-1].thread_ident);
#elif MKCL_WINDOWS
      HANDLE handle = env->own_thread->thread.interrupted_threads[i-1].thread_ident;
      if (handle)
	{
	  TerminateThread(handle, 0);
	  CloseHandle(handle);
	  env->own_thread->thread.interrupted_threads[i-1].thread_ident = NULL;
	}
#endif
    }
#endif

  mkcl_thread_exit(env, MKCL_THREAD_ABORTED);
}

mkcl_object mk_mt_terminate_thread(MKCL)
{
  mkcl_call_stack_check(env);
  mk_mt_exit_thread(env, @':terminated');
}

mkcl_object mk_mt_cancel_thread(MKCL)
{
  mkcl_call_stack_check(env);
  mk_mt_exit_thread(env, @':canceled');
}

mkcl_object mk_mt_abort_thread(MKCL)
{
  mkcl_call_stack_check(env);
  mk_mt_abandon_thread(env, @':aborted');
}



#if 0 /* Retired because much too dangerous to be used. */
mkcl_object mk_mt_scuttle_thread(MKCL)
{
  mkcl_call_stack_check(env);
  mkcl_disable_interrupts(env);

  env->own_thread->thread.result_value = @':aborted';

#if 0 /* All of this is much too dangerous to be worth the risk. JCB */
  int i = env->own_thread->thread.interrupt_count;

  for (; i; --i)
    { /* These system calls here are strictly "fire and forget".
	 Testing for return status could send us into an infinite recursion. */
#if MKCL_PTHREADS
      pthread_detach(env->own_thread->thread.interrupted_threads[i-1].thread_ident);
      pthread_cancel(env->own_thread->thread.interrupted_threads[i-1].thread_ident);
#elif MKCL_WINDOWS
      HANDLE handle = env->own_thread->thread.interrupted_threads[i-1].thread_ident;
      if (handle)
	{
	  TerminateThread(handle, 0);
	  CloseHandle(handle);
	  env->own_thread->thread.interrupted_threads[i-1].thread_ident = NULL;
	}
#endif
    }
#endif

  mkcl_thread_exit(env, MKCL_THREAD_ABORTED);
}
#endif

mkcl_object
mk_mt_all_threads(MKCL)
{
  /* No race condition here because this list is never destructively
   * modified. When we add or remove threads, we create new lists. */
  mkcl_call_stack_check(env);
  @(return mk_cl_copy_list(env, mkcl_core.threads));
}

mkcl_object
mk_mt_thread_name(MKCL, mkcl_object thread)
{
  mkcl_call_stack_check(env);
  mkcl_assert_type_thread(env, thread);
  @(return thread->thread.name);
}

mkcl_object
mk_mt_thread_active_p(MKCL, mkcl_object thread)
{
  mkcl_call_stack_check(env);
  mkcl_assert_type_thread(env, thread);
  @(return ((thread->thread.status == mkcl_thread_active) ? mk_cl_Ct : mk_cl_Cnil));
}

#if 0
mkcl_object
mk_mt_thread_whostate(MKCL, mkcl_object thread)
{
  mkcl_call_stack_check(env);
  mkcl_assert_type_thread(env, thread);
  @(return (mkcl_core.empty_base_string));
}
#endif

mkcl_object
mk_mt_thread_status(MKCL, mkcl_object thread)
{
  mkcl_object status;

  mkcl_call_stack_check(env);
  mkcl_assert_type_thread(env, thread);
  switch (thread->thread.status)
    {
    case mkcl_thread_active:
      status = @':active';
      break;
    case mkcl_thread_set:
      status = @':set';
      break;
    case mkcl_thread_initialized:
      status = @':initialized';
      break;
    case mkcl_thread_done:
      status = @':done';
      break;
    default:
      status = @':unknown';
      break;
    }
  @(return status);
}


mkcl_object
mk_mt_thread_run_function(MKCL, mkcl_narg narg, mkcl_object name, mkcl_object function, ...)
{
  mkcl_object thread;
  mkcl_va_list args;

  mkcl_call_stack_check(env);
  if (narg < 2)
    mkcl_FEwrong_num_arguments(env, @'mt::thread-run-function', 2, -1, narg);

  mkcl_va_start(env, args, function, narg, 2);
  mkcl_object rest = mkcl_grab_rest_args(env, args, FALSE);
  mkcl_va_end(args);

  if (MKCL_CONSP(name)) {
    /* This is useful to pass :initial-bindings to make-thread */
    thread = mk_cl_apply(env, 2, @'mt::make-thread', name);
  } else {
    thread = mk_mt_make_thread(env, 2, @':name', name);
  }
  mk_cl_apply(env, 4, @'mt::thread-preset', thread, function, rest);
  return mk_mt_thread_enable(env, thread);
}

/*----------------------------------------------------------------------
 * LOCKS or MUTEX
 */

@(defun mt::make-lock (&key name ((:recursive recursive) mk_cl_Cnil) fast)
  mkcl_object output;
@
#if MKCL_WINDOWS
  HANDLE mutex;

  MKCL_LIBC_NO_INTR(env, mutex = CreateMutex(NULL, FALSE, mkcl_handle_debug_name(env, "mutex")));
  if ( mutex == NULL )
    mkcl_FEwin32_error(env, "mt::make-lock failed to create lock: ~A", 1, name);

  output = mkcl_alloc_raw_lock(env);
  output->lock.name = name;
  output->lock.mutex = mutex;
  output->lock.holder = mk_cl_Cnil;
  output->lock.counter = 0;
  output->lock.recursive = (recursive != mk_cl_Cnil);
#else
  {
    const pthread_mutexattr_t * mutexattr = NULL;
    int rc;

    output = mkcl_alloc_raw_lock(env);
    output->lock.name = name;
    output->lock.holder = mk_cl_Cnil;
    output->lock.counter = 0;
    output->lock.mutex = &output->lock.mutex_obj;
    if (recursive == mk_cl_Cnil) {
      if (mkcl_Null(fast)) {
	mutexattr = mkcl_errorcheck_mutexattr;
      }
      output->lock.recursive = FALSE;
    } else {
      mutexattr = mkcl_recursive_mutexattr;
      output->lock.recursive = TRUE;
    }
    MKCL_LIBC_NO_INTR(env, rc = pthread_mutex_init(output->lock.mutex, mutexattr));
    if (rc)
      { errno = rc; mkcl_FElibc_error(env, "mk_mt_make_lock failed on pthread_mutex_init", 0); }
  }
#endif
  mk_si_set_finalizer(env, output, mk_cl_Ct);
  @(return output);
@)

mkcl_object
mk_mt_recursive_lock_p(MKCL, mkcl_object lock)
{
  mkcl_call_stack_check(env);
  if (mkcl_type_of(lock) != mkcl_t_lock)
    mkcl_FEwrong_type_argument(env, @'mt::lock', lock);
  @(return (lock->lock.recursive ? mk_cl_Ct : mk_cl_Cnil));
}

mkcl_object
mk_mt_lock_name(MKCL, mkcl_object lock)
{
  mkcl_call_stack_check(env);
  if (mkcl_type_of(lock) != mkcl_t_lock)
    mkcl_FEwrong_type_argument(env, @'mt::lock', lock);
  @(return lock->lock.name);
}

mkcl_object
mk_mt_lock_holder(MKCL, mkcl_object lock)
{
  mkcl_call_stack_check(env);
  if (mkcl_type_of(lock) != mkcl_t_lock)
    mkcl_FEwrong_type_argument(env, @'mt::lock', lock);
  @(return lock->lock.holder);
}

mkcl_object
mk_mt_giveup_lock(MKCL, mkcl_object lock)
{
  mkcl_object own_thread = mkcl_current_thread(env);
  mkcl_object lock_holder;

  mkcl_call_stack_check(env);
  if (mkcl_type_of(lock) != mkcl_t_lock)
    mkcl_FEwrong_type_argument(env, @'mt::lock', lock);
  if ((lock_holder = lock->lock.holder) != own_thread) {
    mkcl_FEerror(env, "Attempt to give up a lock ~S that is not owned by ~S", 2, lock, own_thread);
  }

  if (--lock->lock.counter == 0) { /* anticipated release side-effects */
    lock->lock.holder = mk_cl_Cnil;
  }

#if MKCL_WINDOWS
  int rc;

  MKCL_LIBC_NO_INTR(env, rc = ReleaseMutex(lock->lock.mutex));
  if (rc == 0)
#if 0 /* This is off only by curiosity on what is the error code emitted by Windows. JCB */
    if (lock->lock.recursive && (lock_holder == env->own_thread) && (env->own_thread->thread.interrupt_count != 0))
      { /* already held recursive lock in an interrupted thread */ }
    else
#endif
      {
	lock->lock.counter++; /* undo anticipated release side-effects */
	lock->lock.holder = lock_holder;
	mkcl_FEwin32_error(env, "giveup-lock: Unable to release Win32 Mutex: ~S", 1, lock);
      }
#elif MKCL_PTHREADS
  int rc;

  MKCL_LIBC_NO_INTR(env, rc = pthread_mutex_unlock(lock->lock.mutex));

  switch (rc)
    {
    case EPERM:
      if (lock->lock.recursive && (lock_holder == env->own_thread) && (env->own_thread->thread.interrupt_count != 0))
	{ /* already held recursive lock in an interrupted thread */ }
      else
	{
	  lock->lock.counter++; /* undo anticipated release side-effects */
	  lock->lock.holder = lock_holder;
	  mkcl_FEerror(env, "giveup-lock: Not owner of lock: ~S", 1, lock);
	}
      break;
    case EINVAL:
      lock->lock.counter++; /* undo anticipated release side-effects */
      lock->lock.holder = lock_holder;
      mkcl_FEerror(env, "giveup-lock: Invalid lock: ~S", 1, lock);
      break;
    default:
      lock->lock.counter++; /* undo anticipated release side-effects */
      lock->lock.holder = lock_holder;
      mkcl_lose(env, "mk_mt_giveup_lock failed on pthread_mutex_unlock");
      break;
    case 0: break;
    }
#else
# error Incomplete mk_mt_giveup_lock().
#endif
  @(return mk_cl_Ct);
}

static const mkcl_base_string_object(timeout_format_control_string_obj, "Timeout value ~S is not a positive real number.");
static const mkcl_object timeout_format_control_string = (mkcl_object) &timeout_format_control_string_obj;

@(defun mt::get-lock (lock &optional (timeout mk_cl_Ct))
@
  if (mkcl_type_of(lock) != mkcl_t_lock)
    mkcl_FEwrong_type_argument(env, @'mt::lock', lock);

  /* already held recursive lock in an interrupted thread */
  if (lock->lock.recursive && (lock->lock.holder == env->own_thread) && (env->own_thread->thread.interrupt_count != 0)) {
    lock->lock.counter++;
    @(return mk_cl_Ct);
  }

#if MKCL_WINDOWS
  {
    mkcl_object output = mk_cl_Cnil;
    DWORD delai = INFINITE;
    DWORD val;

    /* In Windows, all locks are recursive. We simulate the other case. */
    /* We complain if recursive=0 and this is an attempt to lock recursively. */
    /* FIXME: Should probably be moved after the lock has been acquired. JCB */
    if (!lock->lock.recursive && (lock->lock.holder == env->own_thread)) {
      mkcl_FEerror(env, "A recursive attempt was made to hold non-recursive lock ~S", 1, lock);
    }

    if (mkcl_Null(timeout) || (MKCL_MAKE_FIXNUM(0) == timeout))
      delai = 0;
    else if (timeout == mk_cl_Ct)
      delai = INFINITE;
    else
      {
	/* INV: mkcl_minusp() makes sure `timeout' is real */
	if (mkcl_minusp(env, timeout))
	  mk_cl_error(env, 9, @'simple-type-error', @':format-control',
		      timeout_format_control_string,
		      @':format-arguments', mkcl_list1(env, timeout),
		      @':expected-type', @'real', @':datum', timeout);

	{ 
	  double r = mkcl_to_double(env, timeout);

	  delai = floor(r * 1000.0); /* in milliseconds. */
	}
      }

    do {
      MKCL_LIBC_Zzz(env, @':io', val = WaitForSingleObjectEx(lock->lock.mutex, delai, TRUE));
    } while (val == WAIT_IO_COMPLETION);
    switch (val)
      {
      case WAIT_OBJECT_0:
	lock->lock.holder = env->own_thread;
	lock->lock.counter++;
	output = mk_cl_Ct;
	break;
      case WAIT_TIMEOUT:
	output = mk_cl_Cnil;
	break;
      case WAIT_ABANDONED:
	mkcl_FEwin32_error(env, "Mutex was abandoned in a locked state", 0);
	break;
      case WAIT_FAILED:
      default:
	mkcl_FEwin32_error(env, "Unable to lock Win32 Mutex: ~S", 1, lock);
	break;
      }
    mk_mt_test_for_thread_shutdown(env);

    @(return output);
  }
#elif MKCL_PTHREADS
  int rc;

  if (mkcl_Null(timeout) || (MKCL_MAKE_FIXNUM(0) == timeout))
    {
      MKCL_LIBC_NO_INTR(env, rc = pthread_mutex_trylock(lock->lock.mutex));

      if (rc == EBUSY)
	{ @(return mk_cl_Cnil); }
    }
  else if (timeout == mk_cl_Ct)
    {
      mkcl_os_mutex_ref mutex = lock->lock.mutex;

      MKCL_LIBC_NO_INTR(env, rc = pthread_mutex_lock(mutex));
      if (mutex != lock->lock.mutex)
	mk_mt_abandon_thread(env, @':aborted'); /* broken mutex! must abort right now! */
    }
  else
    {
      mkcl_os_mutex_ref mutex = lock->lock.mutex;
      struct timespec ts;
      int error;

      /* INV: mkcl_minusp() makes sure `timeout' is real */
      if (mkcl_minusp(env, timeout))
	mk_cl_error(env, 9, @'simple-type-error', @':format-control',
		    timeout_format_control_string,
		    @':format-arguments', mk_cl_list(env, 1, timeout),
		    @':expected-type', @'real', @':datum', timeout);

      MKCL_LIBC_NO_INTR(env, error = clock_gettime(CLOCK_REALTIME, &ts));
      if (error)
	mkcl_FElibc_error(env, "mk_mt_get_lock failed on clock_gettime", 0);

      /* Add `timeout' delta */
      { 
	double r = mkcl_to_double(env, timeout);
	ts.tv_sec += floor(r);
	ts.tv_nsec += ((r - floor(r)) * 1e9);
      }
      if (ts.tv_nsec >= 1000000000) {
	ts.tv_nsec -= 1000000000;
	ts.tv_sec++;
      }

      MKCL_LIBC_NO_INTR(env, rc = pthread_mutex_timedlock(mutex, &ts));
      if (mutex != lock->lock.mutex)
	mk_mt_abandon_thread(env, @':aborted'); /* broken mutex! must abort right now! */

      if (rc == ETIMEDOUT)
	{ @(return mk_cl_Cnil); }
    }

  switch (rc)
    {
    case EINVAL: mkcl_FEerror(env, "get-lock: Invalid lock: ~S", 1, lock); break;
    case EAGAIN: mkcl_FEerror(env, "get-lock: Out of available locks: ~S", 1, lock); break;
    case EDEADLK: mkcl_FEerror(env, "get-lock: Would deadlock: ~S", 1, lock); break;
    default: mkcl_lose(env, "mk_mt_get_lock failed on pthread_mutex_lock"); break;
    case 0:
      lock->lock.holder = env->own_thread;
      lock->lock.counter++;
      break;
    }
  @(return mk_cl_Ct);
#else
# error Incomplete mt::get-lock 
#endif /* MKCL_WINDOWS */
@)


/*----------------------------------------------------------------------
 * RWLOCKS
 */

mkcl_object mk_mt_make_rwlock(MKCL)
{
  mkcl_object output = mkcl_alloc_raw_rwlock(env);
  
  mkcl_call_stack_check(env);
  output->rwlock.name = mk_cl_Cnil;
#if MKCL_WINDOWS
# if defined(_MSC_VER) && (defined(WinVista) || defined(Win7))
  MKCL_LIBC_NO_INTR(env, InitializeSRWLock(&output->rwlock.rwlock));
# else
  mkcl_FEerror(env, "RWLocks are supported under Windows Vista and successors", 0);
# endif
#elif MKCL_PTHREADS
  int rc;

  output->rwlock.rwlock = &output->rwlock.rwlock_obj;
  MKCL_LIBC_NO_INTR(env, rc = pthread_rwlock_init(output->rwlock.rwlock, NULL));
  switch (rc)
    {
    case EAGAIN:
    case ENOMEM:
    case EPERM:
    case EBUSY:
    case EINVAL:
      mkcl_lose(env, "mk_mt_make_rwlock failed on pthread_rwlock_init");
      break;
    case 0: break;
    }
  mk_si_set_finalizer(env, output, mk_cl_Ct);
#else
# error Incomplete mk_mt_make_rwlock().
#endif
  @(return output);
}

@(defun mt::giveup-rwlock (rwlock &optional (read_or_write @':read'))
@
  {
    if (mkcl_type_of(rwlock) != mkcl_t_rwlock)
      mkcl_FEwrong_type_argument(env, @'mt::rwlock', rwlock);

#if MKCL_WINDOWS
# if defined(_MSC_VER) && (defined(WinVista) || defined(Win7))
    if (MKCL_EQ(read_or_write, @':read'))
      { MKCL_LIBC_NO_INTR(env, ReleaseSRWLockShared(&rwlock->rwlock.rwlock)); }
    else if (MKCL_EQ(read_or_write, @':write'))
      { MKCL_LIBC_NO_INTR(env, ReleaseSRWLockExclusive(&rwlock->rwlock.rwlock)); }
    else
      mkcl_FEerror(env, "Invalid second argument to mt::giveup-rwlock", 0);
# else
    mkcl_FEerror(env, "RWLocks are supported under Windows Vista and successors", 0);
# endif
#elif MKCL_PTHREADS
    int rc;

    MKCL_LIBC_NO_INTR(env, rc = pthread_rwlock_unlock(rwlock->rwlock.rwlock));
    switch (rc)
      {
      case EINVAL: mkcl_FEerror(env, "giveup-rwlock: Invalid lock: ~S", 1, rwlock); break;
      case EPERM: mkcl_FEerror(env, "giveup-rwlock: Not owner of lock: ~S", 1, rwlock); break;
      default: mkcl_lose(env, "mk_mt_giveup_rwlock failed on pthread_rwlock_unlock"); break;
      case 0: break;
      }
#else
# error Incomplete mt::giveup-rwlock.
#endif
    @(return mk_cl_Ct);
  }
@)


@(defun mt::get-read-rwlock (rwlock &optional (timeout mk_cl_Ct))
@
  if (mkcl_type_of(rwlock) != mkcl_t_rwlock)
    mkcl_FEwrong_type_argument(env, @'mt::rwlock', rwlock);

#if MKCL_WINDOWS
# if defined(_MSC_VER) && (defined(WinVista) || defined(Win7))
  MKCL_LIBC_NO_INTR(env, AcquireSRWLockShared(&rwlock->rwlock.rwlock));
# else
  mkcl_FEerror(env, "RWLocks are supported under Windows Vista and successors", 0);
# endif
#elif MKCL_PTHREADS
  int rc;

  if (mkcl_Null(timeout) || (MKCL_MAKE_FIXNUM(0) == timeout))
    {
      MKCL_LIBC_NO_INTR(env, rc = pthread_rwlock_tryrdlock(rwlock->rwlock.rwlock));

      if (rc == EBUSY)
	{ @(return mk_cl_Cnil); }
    }
  else if (timeout == mk_cl_Ct)
    {
      mkcl_os_rwlock_ref os_rwlock = rwlock->rwlock.rwlock;

      MKCL_LIBC_NO_INTR(env, rc = pthread_rwlock_rdlock(os_rwlock));
      if (os_rwlock != rwlock->rwlock.rwlock)
	mk_mt_abandon_thread(env, @':aborted'); /* broken rwlock! must abort right now! */
    }
  else
    {
      mkcl_os_rwlock_ref os_rwlock = rwlock->rwlock.rwlock;
      struct timespec   ts;

      /* INV: mkcl_minusp() makes sure `timeout' is real */
      if (mkcl_minusp(env, timeout))
	mk_cl_error(env, 9, @'simple-type-error', @':format-control',
		    timeout_format_control_string,
		    @':format-arguments', mk_cl_list(env, 1, timeout),
		    @':expected-type', @'real', @':datum', timeout);

      MKCL_LIBC_NO_INTR(env, rc = clock_gettime(CLOCK_REALTIME, &ts));
      if (rc)
	mkcl_FElibc_error(env, "mk_mt_get_read_rwlock failed on clock_gettime", 0);

      /* Add `timeout' delta */
      { 
	double r = mkcl_to_double(env, timeout);
	ts.tv_sec += floor(r);
	ts.tv_nsec += ((r - floor(r)) * 1e9);
      }
      if (ts.tv_nsec >= 1000000000) {
	ts.tv_nsec -= 1000000000;
	ts.tv_sec++;
      }

      MKCL_LIBC_NO_INTR(env, rc = pthread_rwlock_timedrdlock(os_rwlock, &ts));
      if (os_rwlock != rwlock->rwlock.rwlock)
	mk_mt_abandon_thread(env, @':aborted'); /* broken rwlock! must abort right now! */

      if (rc == ETIMEDOUT)
	{ @(return mk_cl_Cnil); }
    }

  switch (rc)
    {
    case EINVAL: mkcl_FEerror(env, "get-read-rwlock: Invalid lock: ~S", 1, rwlock); break;
    case EAGAIN: mkcl_FEerror(env, "get-read-rwlock: Out of available locks: ~S", 1, rwlock); break;
    case EDEADLK: mkcl_FEerror(env, "get-read-rwlock: Would deadlock: ~S", 1, rwlock); break;
    default: mkcl_lose(env, "mk_mt_get_read_rwlock failed on pthread_rwlock_rdlock"); break;
    case 0: break;
    }
#else
# error Incomplete mt::get-read-rwlock.
#endif
  @(return mk_cl_Ct);
@)

@(defun mt::get-write-rwlock (rwlock &optional (timeout mk_cl_Ct))
@
  if (mkcl_type_of(rwlock) != mkcl_t_rwlock)
    mkcl_FEwrong_type_argument(env, @'mt::rwlock', rwlock);

#if MKCL_WINDOWS
# if defined(_MSC_VER) && (defined(WinVista) || defined(Win7))
  MKCL_LIBC_NO_INTR(env, AcquireSRWLockExclusive(&rwlock->rwlock.rwlock));
# else
  mkcl_FEerror(env, "RWLocks are supported under Windows Vista and successors", 0);
# endif
#elif MKCL_PTHREADS
  int rc;

  if (mkcl_Null(timeout) || (MKCL_MAKE_FIXNUM(0) == timeout))
    {
      MKCL_LIBC_NO_INTR(env, rc = pthread_rwlock_trywrlock(rwlock->rwlock.rwlock));

      if (rc == EBUSY)
	{ @(return mk_cl_Cnil); }
    }
  else if (timeout == mk_cl_Ct)
    {
      mkcl_os_rwlock_ref os_rwlock = rwlock->rwlock.rwlock;

      MKCL_LIBC_NO_INTR(env, rc = pthread_rwlock_wrlock(os_rwlock));
      if (os_rwlock != rwlock->rwlock.rwlock)
	mk_mt_abandon_thread(env, @':aborted'); /* broken rwlock! must abort right now! */
    }
  else
    {
      mkcl_os_rwlock_ref os_rwlock = rwlock->rwlock.rwlock;
      struct timespec ts;

      /* INV: mkcl_minusp() makes sure `timeout' is real */
      if (mkcl_minusp(env, timeout))
	mk_cl_error(env, 9, @'simple-type-error', @':format-control',
		    timeout_format_control_string,
		    @':format-arguments', mk_cl_list(env, 1, timeout),
		    @':expected-type', @'real', @':datum', timeout);

      MKCL_LIBC_NO_INTR(env, rc = clock_gettime(CLOCK_REALTIME, &ts));
      if (rc)
	mkcl_FElibc_error(env, "mk_mt_get_write_rwlock failed on clock_gettime", 0);

      /* Add `timeout' delta */
      { 
	double r = mkcl_to_double(env, timeout);
	ts.tv_sec += floor(r);
	ts.tv_nsec += ((r - floor(r)) * 1e9);
      }
      if (ts.tv_nsec >= 1000000000) {
	ts.tv_nsec -= 1000000000;
	ts.tv_sec++;
      }

      MKCL_LIBC_NO_INTR(env, rc = pthread_rwlock_timedwrlock(os_rwlock, &ts));
      if (os_rwlock != rwlock->rwlock.rwlock)
	mk_mt_abandon_thread(env, @':aborted'); /* broken rwlock! must abort right now! */

      if (rc == ETIMEDOUT)
	{ @(return mk_cl_Cnil); }
    }

  switch (rc)
    {
    case EINVAL: mkcl_FEerror(env, "get-write-rwlock: Invalid lock: ~S", 1, rwlock); break;
    case EAGAIN: mkcl_FEerror(env, "get-write-rwlock: Out of available locks: ~S", 1, rwlock); break;
    case EDEADLK: mkcl_FEerror(env, "get-write-rwlock: Would deadlock: ~S", 1, rwlock); break;
    default: mkcl_lose(env, "mk_mt_get_write_rwlock failed on pthread_rwlock_wrlock"); break;
    case 0: break;
    }
#else
# error Incomplete mt::get-write-rwlock.
#endif
  @(return mk_cl_Ct);
@)


/*----------------------------------------------------------------------
 * SEMAPHORES
 */

@(defun mt::make-semaphore (&optional count)
@
  mkcl_object output = mkcl_alloc_raw_semaphore(env);
  unsigned int value = 0;

  if (!mkcl_Null(count))
    {
      if (!MKCL_FIXNUMP(count))
	count = mkcl_type_error(env, @'mt::make-semaphore', "count",
				count, @'integer'); /* FIXME: should be (unsigned-byte 32) instead. JCB */
      value = mkcl_fixnum_to_word(count);
    }

#if MKCL_WINDOWS
  {
    HANDLE sem;

    MKCL_LIBC_NO_INTR(env, sem = CreateSemaphore(NULL, value, LONG_MAX, mkcl_handle_debug_name(env, "semaphore")));
    if (sem == NULL)
      mkcl_FEwin32_error(env, "mt::make-semaphore failed on CreateSemaphore", 0);

    output->semaphore.sem = sem;
    output->semaphore.name = mk_cl_Cnil;
    output->semaphore.count = value;
    output->semaphore.max_count = LONG_MAX;
  }
#elif MKCL_PTHREADS
  int rc;

  output->semaphore.sem = &output->semaphore.sem_obj;

  MKCL_LIBC_NO_INTR(env, rc = sem_init(output->semaphore.sem, FALSE, value));
  if (rc) mkcl_FElibc_error(env, "mt::make-semaphore failed on sem_init", 0);
  output->semaphore.name = mk_cl_Cnil;
  output->semaphore.count = value;
  output->semaphore.max_count = SEM_VALUE_MAX;
#else
# error Incomplete mt::make-semaphore.
#endif

  mk_si_set_finalizer(env, output, mk_cl_Ct);

  @(return output);
@)

mkcl_object mk_mt_semaphore_count(MKCL, mkcl_object sem)
{
  mkcl_call_stack_check(env);
  if (mkcl_type_of(sem) != mkcl_t_semaphore)
    mkcl_FEwrong_type_argument(env, @'mt::semaphore', sem);
  
  {
#if MKCL_WINDOWS
    long count;
    DWORD wait_val;
    BOOL ok;
  
    /*  wait(timeout 0) followed by release if it was signaled */
    MKCL_LIBC_NO_INTR(env, wait_val = WaitForSingleObject(sem->semaphore.sem, 0));
    switch (wait_val)
      {
      case WAIT_OBJECT_0:
	MKCL_LIBC_NO_INTR(env, ok = ReleaseSemaphore(sem->semaphore.sem, 1, &count));
	if (!ok)
	  mkcl_FEwin32_error(env, "mk_mt_semaphore_count failed on ReleaseSemaphore", 0);
	count++;
	break;
      case WAIT_TIMEOUT: count = 0; break;
      case WAIT_FAILED:
      default:
	mkcl_FEwin32_error(env, "mk_mt_semaphore_count failed on WaitForSingleObject", 0);
      }
#elif MKCL_PTHREADS
    int rc, count;

    MKCL_LIBC_NO_INTR(env, rc = sem_getvalue(sem->semaphore.sem, &count));
    if (rc)
      mkcl_FElibc_error(env, "mk_mt_semaphore_count failed, invalid semaphore", 0);
#else
# error Incomplete mk_mt_semaphore_count.
#endif
    sem->semaphore.count = count;
    @(return mkcl_make_integer(env, count));
  }
}

@(defun mt::semaphore-signal (sem &optional count)
@
  int c_count = 0;

  if (mkcl_type_of(sem) != mkcl_t_semaphore)
    mkcl_FEwrong_type_argument(env, @'mt::semaphore', sem);
  
  if (mkcl_Null(count))
    c_count = 1;
  else if (!MKCL_FIXNUMP(count) || ((c_count = mkcl_fixnum_to_word(count)) < 0))
    mk_cl_error(env, 9, @'simple-type-error', @':format-control',
		timeout_format_control_string,
		@':format-arguments', mkcl_list1(env, count),
		@':expected-type', @'integer', @':datum', count);
  
#if MKCL_WINDOWS
  {
    BOOL ok;
    long prev_count;

    MKCL_LIBC_NO_INTR(env, ok = ReleaseSemaphore(sem->semaphore.sem, c_count, &prev_count));
    if (!ok)
      mkcl_FEwin32_error(env, "mk_mt_semaphore_count failed on ReleaseSemaphore", 0);
    sem->semaphore.count = prev_count;
  }
#elif MKCL_PTHREADS
  int i;

  for (i = 0; i < c_count; i++)
    {
      int rc;

      MKCL_LIBC_NO_INTR(env, rc = sem_post(sem->semaphore.sem));
      if (rc)
	mkcl_FElibc_error(env, "mk_mt_semaphore_signal failed, invalid semaphore", 0);
    }
#else
# error Incomplete mt::semaphore-signal.
#endif
  sem->semaphore.count += c_count;
  @(return mk_cl_Ct);
@)

@(defun mt::semaphore-wait (sem &optional (timeout mk_cl_Ct))
@
  if (mkcl_type_of(sem) != mkcl_t_semaphore)
    mkcl_FEwrong_type_argument(env, @'mt::semaphore', sem);
  
#if MKCL_WINDOWS
  {
    DWORD milliSeconds;
    DWORD wait_val;

    if (mkcl_Null(timeout))
      milliSeconds = 0;
    else if (timeout == mk_cl_Ct)
      milliSeconds = INFINITE;
    else
      {
	/* INV: mkcl_minusp() makes sure `timeout' is real */
	if (mkcl_minusp(env, timeout))
	  mk_cl_error(env, 9, @'simple-type-error', @':format-control',
		      timeout_format_control_string,
		      @':format-arguments', mkcl_list1(env, timeout),
		      @':expected-type', @'real', @':datum', timeout);
	
	double r = mkcl_to_double(env, timeout);
	milliSeconds = r * 1000;
      }

    do {
      MKCL_LIBC_Zzz(env, @':io', wait_val = WaitForSingleObjectEx(sem->semaphore.sem, milliSeconds, TRUE));
    } while (wait_val == WAIT_IO_COMPLETION);
    switch (wait_val)
      {
      case WAIT_OBJECT_0:
	break;
      case WAIT_TIMEOUT: 
	@(return mk_cl_Cnil);
	break;
      case WAIT_FAILED:
      default:
	mkcl_FEwin32_error(env, "mk_mt_semaphore_wait failed on WaitForSingleObject", 0);
      }
    mk_mt_test_for_thread_shutdown(env);
  }
#elif MKCL_PTHREADS
  int rc;

  if (mkcl_Null(timeout) || (MKCL_MAKE_FIXNUM(0) == timeout))
    {
      do
	{ MKCL_LIBC_NO_INTR(env, rc = sem_trywait(sem->semaphore.sem)); }
      while (rc && errno == EINTR);

      if (rc && errno == EAGAIN)
	{ @(return mk_cl_Cnil); }
    }
  else if (timeout == mk_cl_Ct)
    {
      do
	{ MKCL_LIBC_Zzz(env, @':io', rc = sem_wait(sem->semaphore.sem)); }
      while (rc && errno == EINTR);
      mk_mt_test_for_thread_shutdown(env);
    }
  else
    {
      struct timespec   ts;

      /* INV: mkcl_minusp() makes sure `timeout' is real */
      if (mkcl_minusp(env, timeout))
	mk_cl_error(env, 9, @'simple-type-error', @':format-control',
		    timeout_format_control_string,
		    @':format-arguments', mk_cl_list(env, 1, timeout),
		    @':expected-type', @'real', @':datum', timeout);
      
      MKCL_LIBC_NO_INTR(env, rc = clock_gettime(CLOCK_REALTIME, &ts));
      if (rc) 
	mkcl_FElibc_error(env, "mk_mt_semaphore_wait failed on clock_gettime", 0);

      /* Add `timeout' delta */
      { 
	double r = mkcl_to_double(env, timeout);
	double r_sec = floor(r);

	ts.tv_sec += r_sec;
	ts.tv_nsec += ((r - r_sec) * 1e9);
      }
      if (ts.tv_nsec >= 1000000000) {
	ts.tv_nsec -= 1000000000;
	ts.tv_sec++;
      }
      do
	{ MKCL_LIBC_Zzz(env, @':io', rc = sem_timedwait(sem->semaphore.sem, &ts)); }
      while (rc && (errno == EINTR));
      mk_mt_test_for_thread_shutdown(env);

      if (rc && errno == ETIMEDOUT)
	{ @(return mk_cl_Cnil); }
    }

  if (rc)
    switch (errno)
      {
      case EINVAL: mkcl_FEerror(env, "Invalid semaphore", 0); break;
      default: mkcl_C_lose(env, "mk_mt_semaphore_wait failed on sem_wait"); break;
      }
#else
# error Incomplete mt::semaphore-wait.
#endif

  @(return mk_cl_Ct);
@)



/*----------------------------------------------------------------------
 * CONDITION VARIABLES
 */

mkcl_object
mk_mt_make_condition_variable(MKCL)
{
  mkcl_call_stack_check(env);
#if MKCL_WINDOWS
  mkcl_FEerror(env, "Condition variables are not supported under Windows", 0);
  @(return mk_cl_Cnil);
#elif MKCL_PTHREADS
  mkcl_object output = mkcl_alloc_raw_condition_variable(env);

  output->condition_variable.name = mk_cl_Cnil;
#if 0
  pthread_condattr_t attr;
  pthread_condattr_init(&attr);
  pthread_cond_init(&output->condition_variable.cv, &attr);
  pthread_condattr_destroy(&attr);
#else
  MKCL_LIBC_NO_INTR(env, pthread_cond_init(&output->condition_variable.cv, NULL));
#endif

  mk_si_set_finalizer(env, output, mk_cl_Ct);
  @(return output);
#else
# error Incomplete mt::semaphore-wait().
#endif
}

@(defun mt::condition-wait (cv lock &optional (timeout mk_cl_Ct))
@
{
#if MKCL_WINDOWS
  mkcl_FEerror(env, "Condition variables are not supported under Windows", 0);
#elif MKCL_PTHREADS
  int rc;

  if (mkcl_type_of(cv) != mkcl_t_condition_variable)
    mkcl_FEwrong_type_argument(env, @'mt::condition-variable', cv);
  if (mkcl_type_of(lock) != mkcl_t_lock)
    mkcl_FEwrong_type_argument(env, @'mt::lock', lock);

  if (mkcl_Null(timeout))
    timeout = MKCL_MAKE_FIXNUM(0);

  if (timeout == mk_cl_Ct)
    {
      MKCL_LIBC_Zzz(env, cv, rc = pthread_cond_wait(&cv->condition_variable.cv, lock->lock.mutex));
      mk_mt_test_for_thread_shutdown(env);
      if (rc == 0)
	lock->lock.holder = mkcl_current_thread(env);
    }
  else
    {
      struct timespec   ts;
      /* INV: mkcl_minusp() makes sure `timeout' is real */
      if (mkcl_minusp(env, timeout))
	mk_cl_error(env, 9, @'simple-type-error', @':format-control',
		    timeout_format_control_string,
		    @':format-arguments', mk_cl_list(env, 1, timeout),
		    @':expected-type', @'real', @':datum', timeout);

      MKCL_LIBC_NO_INTR(env, rc = clock_gettime(CLOCK_REALTIME, &ts));
      if (rc)
	mkcl_FElibc_error(env, "mk_mt_condition_wait failed on clock_gettime", 0);

      /* Add `timeout' delta */
      {
	double r = mkcl_to_double(env, timeout);

	ts.tv_sec += floor(r);
	ts.tv_nsec += ((r - floor(r)) * 1e9);
      }
      if (ts.tv_nsec >= 1000000000) {
	ts.tv_nsec -= 1000000000;
	ts.tv_sec++;
      }
      
      MKCL_LIBC_Zzz(env, cv, rc = pthread_cond_timedwait(&cv->condition_variable.cv, lock->lock.mutex, &ts));
      mk_mt_test_for_thread_shutdown(env);

      if (rc == 0)
	lock->lock.holder = mkcl_current_thread(env);
      else if (rc == ETIMEDOUT || rc == EINTR) /* EINTR seems to be a Linux special, in direct contradiction to POSIX! */
	{ @(return mk_cl_Cnil); }
    }
  
  switch (rc)
    {
    case EINVAL: mkcl_FEerror(env, "condition-wait: Invalid condition variable or lock", 0); break;
    case EPERM: mkcl_FEerror(env, "condition-wait: Not lock owner: ~S", 1, lock); break;
    default: mkcl_lose(env, "mk_mt_condition_wait failed on pthread_cond_wait"); break;
    case 0: break;
    }
#else
# error Incomplete mt::semaphore-wait.
#endif
  @(return mk_cl_Ct);
}
@)

mkcl_object
mk_mt_condition_signal(MKCL, mkcl_object cv)
{
  mkcl_call_stack_check(env);
#if MKCL_WINDOWS
  mkcl_FEerror(env, "Condition variables are not supported under Windows", 0);
#elif MKCL_PTHREADS
  int rc;

  if (mkcl_type_of(cv) != mkcl_t_condition_variable)
    mkcl_FEwrong_type_argument(env, @'mt::condition-variable', cv);
  MKCL_LIBC_NO_INTR(env, rc = pthread_cond_signal(&cv->condition_variable.cv));

  switch (rc)
    {
    case EINVAL: mkcl_FEerror(env, "Invalid condition variable", 0); break;
    default: mkcl_lose(env, "mk_mt_condition_signal failed on pthread_cond_signal"); break;
    case 0: break;
    }
#else
# error Incomplete mk_mt_condition_signal().
#endif
  @(return mk_cl_Ct);
}

mkcl_object
mk_mt_condition_broadcast(MKCL, mkcl_object cv)
{
  mkcl_call_stack_check(env);
#if MKCL_WINDOWS
  mkcl_FEerror(env, "Condition variables are not supported under Windows", 0);
#elif MKCL_PTHREADS
  int rc;

  if (mkcl_type_of(cv) != mkcl_t_condition_variable)
    mkcl_FEwrong_type_argument(env, @'mt::condition-variable', cv);
  MKCL_LIBC_NO_INTR(env, rc = pthread_cond_broadcast(&cv->condition_variable.cv));

  switch (rc)
    {
    case EINVAL: mkcl_FEerror(env, "Invalid condition variable", 0); break;
    default: mkcl_lose(env, "mk_mt_condition_broadcast failed on pthread_cond_broadcast"); break;
    case 0: break;
    }
#else
# error Incomplete mk_mt_condition_broadcast().
#endif
  @(return mk_cl_Ct);
}

/*----------------------------------------------------------------------
 * INITIALIZATION
 */

#if MKCL_PTHREADS
static pthread_mutexattr_t recursive_mutexattr;
static pthread_mutexattr_t errorcheck_mutexattr;
static pthread_mutexattr_t normal_mutexattr;

const pthread_mutexattr_t * mkcl_recursive_mutexattr = NULL;
const pthread_mutexattr_t * mkcl_errorcheck_mutexattr = NULL;
const pthread_mutexattr_t * mkcl_normal_mutexattr = NULL;
#endif

void
mkcl_init_early_threads(MKCL)
{
  mkcl_object thread;
  mkcl_os_thread_t main_thread;

#if MKCL_WINDOWS
#if 0
  mkcl_core.global_lock = CreateMutex(NULL, FALSE, mkcl_handle_debug_name(env, "global lock"));
  if ( mkcl_core.global_lock == NULL )
    mkcl_FEwin32_error(env, "mkcl_init_thread failed to create global lock", 0);
#endif

#if 0
  mkcl_core.package_list_lock = CreateMutex(NULL, FALSE, mkcl_handle_debug_name(env, "package list lock"));
  if ( mkcl_core.package_list_lock == NULL )
    mkcl_FEwin32_error(env, "mkcl_init_thread failed to create package list lock", 0);
#else
  InitializeCriticalSection(&mkcl_core.package_list_lock);
#endif

#if 0
  mkcl_core.thread_list_lock = CreateMutex(NULL, FALSE, mkcl_handle_debug_name(env, "thread list lock"));
  if ( mkcl_core.thread_list_lock == NULL )
    mkcl_FEwin32_error(env, "mkcl_init_thread failed to create thread list lock", 0);
#else
  InitializeCriticalSection(&mkcl_core.thread_list_lock);
#endif

#if 0
  mkcl_core.special_index_lock = CreateMutex(NULL, FALSE, mkcl_handle_debug_name(env, "special index lock"));
  if ( mkcl_core.special_index_lock == NULL )
    mkcl_FEwin32_error(env, "mkcl_init_thread failed to create special's index lock", 0);
#else
  InitializeCriticalSection(&mkcl_core.special_index_lock);
#endif

#if 0
  mkcl_imported_thread_pool_lock = CreateMutex(NULL, FALSE, mkcl_handle_debug_name(env, "thread pool lock"));
  if ( mkcl_imported_thread_pool_lock == NULL )
    mkcl_FEwin32_error(env, "mkcl_init_thread failed to create imported thread pool lock", 0);
#else
  InitializeCriticalSection(&mkcl_imported_thread_pool_lock);
#endif

  EnterCriticalSection(&mkcl_imported_thread_pool_lock);
  mkcl_core.imported_thread_pool = mk_cl_Cnil;
  LeaveCriticalSection(&mkcl_imported_thread_pool_lock);

#elif MKCL_PTHREADS /*  MKCL_WINDOWS */

    if (pthread_mutexattr_init(&recursive_mutexattr))
      mkcl_lose(env, "mkcl_init_early_threads failed on pthread_mutexattr_init");
    if (pthread_mutexattr_settype(&recursive_mutexattr, PTHREAD_MUTEX_RECURSIVE))
      mkcl_lose(env, "mkcl_init_early_threads failed on pthread_mutexattr_settype");
    mkcl_recursive_mutexattr = &recursive_mutexattr;
    if (pthread_mutexattr_init(&errorcheck_mutexattr))
      mkcl_lose(env, "mkcl_init_early_threads failed on pthread_mutexattr_init");
    if (pthread_mutexattr_settype(&errorcheck_mutexattr, PTHREAD_MUTEX_ERRORCHECK))
      mkcl_lose(env, "mkcl_init_early_threads failed on pthread_mutexattr_settype");
    mkcl_errorcheck_mutexattr = &errorcheck_mutexattr;
    if (pthread_mutexattr_init(&normal_mutexattr))
      mkcl_lose(env, "mkcl_init_early_threads failed on pthread_mutexattr_init");
    if (pthread_mutexattr_settype(&normal_mutexattr, PTHREAD_MUTEX_NORMAL))
      mkcl_lose(env, "mkcl_init_early_threads failed on pthread_mutexattr_settype");
    mkcl_normal_mutexattr = &normal_mutexattr;

  {
    const pthread_mutexattr_t * const mutexattr = mkcl_normal_mutexattr;

#if 0
    if (pthread_mutex_init(&mkcl_core.global_lock, mutexattr))
      mkcl_lose(env, "mkcl_init_early_threads failed on pthread_mutex_init");
#endif
    if (pthread_mutex_init(&mkcl_core.package_list_lock, mutexattr))
      mkcl_lose(env, "mkcl_init_early_threads failed on pthread_mutex_init");
    if (pthread_mutex_init(&mkcl_core.thread_list_lock, mutexattr))
      mkcl_lose(env, "mkcl_init_early_threads failed on pthread_mutex_init");
    if (pthread_mutex_init(&mkcl_core.special_index_lock, mutexattr))
      mkcl_lose(env, "mkcl_init_early_threads failed on pthread_mutex_init");
    if (pthread_mutex_init(&mkcl_imported_thread_pool_lock, mutexattr))
      mkcl_lose(env, "mkcl_init_early_threads failed on pthread_mutex_init");
  }

  if (pthread_mutex_lock(&mkcl_imported_thread_pool_lock))
    mkcl_lose(env, "mkcl_init_early_threads failed on pthread_mutex_lock");
  else
    {
      mkcl_core.imported_thread_pool = mk_cl_Cnil;
      if (pthread_mutex_unlock(&mkcl_imported_thread_pool_lock))
        mkcl_lose(env, "fill_imported_thread_pool failed on pthread_mutex_unlock");
    }
#else
# error Incomplete mkcl_init_early_threads().
#endif /*  MKCL_WINDOWS */

  mkcl_core.threads = MKCL_OBJNULL;
  mkcl_core.top_special_index = 0;

#if MKCL_PTHREADS
  pthread_sigmask(SIG_SETMASK, NULL, &mkcl_standard_sigmask);

  if (sem_init(mkcl_sleeping_thread_interrupted, 0, 0))
    mkcl_lose(env, "mkcl_init_early_threads failed on sem_init");
  if (sem_init(mkcl_interrupted_thread_suspended, 0, 0))
    mkcl_lose(env, "mkcl_init_early_threads failed on sem_init");
  if (sem_init(mkcl_interrupted_thread_resumed, 0, 0))
    mkcl_lose(env, "mkcl_init_early_threads failed on sem_init");
  if (sem_init(mkcl_run_interrupt_function, 0, 0))
    mkcl_lose(env, "mkcl_init_early_threads failed on sem_init");
  if (sem_init(mkcl_imported_thread_pool_empty, 0, 0))
    mkcl_lose(env, "mkcl_init_early_threads failed on sem_init");
  if (sem_init(mkcl_imported_thread_pool_full, 0, 0))
    mkcl_lose(env, "mkcl_init_early_threads failed on sem_init");

  {
    const pthread_mutexattr_t * const mutexattr = mkcl_normal_mutexattr;

    if (pthread_mutex_init(&mkcl_interrupt_thread_lock, mutexattr))
      mkcl_lose(env, "mkcl_init_early_threads failed on pthread_mutex_init");
  }
#elif MKCL_WINDOWS
  mkcl_sleeping_thread_interrupted = CreateEvent(NULL, TRUE, FALSE, mkcl_handle_debug_name(env, "sleeping thread interrupted event"));
  if (mkcl_sleeping_thread_interrupted == NULL)
    mkcl_FEwin32_error(env, "mkcl_init_thread failed to create thread interrupted event", 0);

  mkcl_imported_thread_pool_empty = CreateSemaphore(NULL, 0, 1, mkcl_handle_debug_name(env, "imported thread pool empty semaphore"));
  if (mkcl_imported_thread_pool_empty == NULL)
    mkcl_FEwin32_error(env, "mkcl_init_thread failed to create imported thread pool empty semaphore", 0);

  mkcl_imported_thread_pool_full = CreateSemaphore(NULL, 0, 1, mkcl_handle_debug_name(env, "imported thread pool full semaphore"));
  if (mkcl_imported_thread_pool_full == NULL)
    mkcl_FEwin32_error(env, "mkcl_init_thread failed to create imported thread pool full semaphore", 0);

  mkcl_interrupt_thread_lock = CreateMutex(NULL, FALSE, mkcl_handle_debug_name(env, "interrupt thread lock"));
  if ( mkcl_interrupt_thread_lock == NULL )
    mkcl_FEwin32_error(env, "mkcl_init_thread failed to create interrupt thread lock", 0);
#endif /* MKCL_PTHREADS */

  /* We have to set the environment before any allocation takes place,
   * so that the interrupt handling code works. */
# if MKCL_WINDOWS
  cl_env_key = TlsAlloc(); /* handle error? JCB */
# elif MKCL_PTHREADS
  pthread_key_create(&cl_env_key, NULL); /* handle error? JCB */
# endif

  mkcl_set_thread_env(env);

#if MKCL_WINDOWS
  DuplicateHandle(GetCurrentProcess(),
 		  GetCurrentThread(),
 		  GetCurrentProcess(),
 		  &main_thread,
 		  0,
 		  FALSE,
 		  DUPLICATE_SAME_ACCESS);
#elif MKCL_PTHREADS
  main_thread = pthread_self();
#endif
  thread = mkcl_alloc_raw_thread(env);
  thread->thread.name = mkcl_make_simple_base_string(env, "Initial");
  thread->thread.function = mk_cl_Cnil;
  thread->thread.args = mk_cl_Cnil;
  thread->thread.result_value = MKCL_OBJNULL;
  thread->thread.detached = FALSE;
  thread->thread.thread = main_thread;
  thread->thread.base_thread = main_thread;
  thread->thread.tid = mkcl_gettid();
  thread->thread.env = env;
  thread->thread.shutdown_requested = false;
  thread->thread.interrupt = mk_cl_Cnil;
  thread->thread.plist = mk_cl_Cnil;
  thread->thread.initial_bindings = mk_cl_Cnil;
  thread->thread.sigmask_frs_marker = NULL;
#if MKCL_PTHREADS
  thread->thread.running_lock = NULL;
  sigemptyset(&thread->thread.saved_sigmask);
#endif
  thread->thread.resume_handler_ran = false;
  thread->thread.interrupt_count = 0;

  env->own_thread = thread;

  mkcl_core.initial_thread = thread;
  mkcl_core.shutdown_watchdog_thread = thread; /* By default the initial thread should also be the shutdown watchdog. */
  mkcl_object shutdown_watchdog_will_clean_up = mk_cl_Ct; /* By default we want the shutdown watchdog to do the best clean-up possible. */

  mk_si_set_finalizer(env, thread, mk_cl_Ct);

  thread->thread.status = mkcl_thread_active;
  mkcl_core.threads = mkcl_list1(env, thread);
}

void
mkcl_init_late_threads(MKCL)
{
  mkcl_create_imported_thread_pool_filler_thread(env);
  mkcl_create_finalization_thread(env);
}

void mkcl_clean_up_threads(MKCL)
{ /* Best effort only. We cannot raise an exception from here. */
#if MKCL_WINDOWS
  (void) TlsSetValue(cl_env_key, NULL);
  (void) TlsFree(cl_env_key);
  (void) CloseHandle(mkcl_interrupt_thread_lock);
  (void) CloseHandle(mkcl_imported_thread_pool_full);
  (void) CloseHandle(mkcl_imported_thread_pool_empty);
  (void) CloseHandle(mkcl_sleeping_thread_interrupted);
  DeleteCriticalSection(&mkcl_imported_thread_pool_lock);
  DeleteCriticalSection(&mkcl_core.special_index_lock);
  DeleteCriticalSection(&mkcl_core.thread_list_lock);
  DeleteCriticalSection(&mkcl_core.package_list_lock);
#elif MKCL_PTHREADS /*  MKCL_WINDOWS */
  (void) pthread_setspecific(cl_env_key, NULL);
  (void) pthread_key_delete(cl_env_key);
  (void) pthread_mutex_destroy(&mkcl_interrupt_thread_lock);
  (void) sem_destroy(mkcl_imported_thread_pool_full);
  (void) sem_destroy(mkcl_imported_thread_pool_empty);
  (void) sem_destroy(mkcl_run_interrupt_function);
  (void) sem_destroy(mkcl_interrupted_thread_resumed);
  (void) sem_destroy(mkcl_interrupted_thread_suspended);
  (void) sem_destroy(mkcl_sleeping_thread_interrupted);
  (void) pthread_mutex_destroy(&mkcl_imported_thread_pool_lock);
  (void) pthread_mutex_destroy(&mkcl_core.special_index_lock);
  (void) pthread_mutex_destroy(&mkcl_core.thread_list_lock);
  (void) pthread_mutex_destroy(&mkcl_core.package_list_lock);
  (void) pthread_mutexattr_destroy(&normal_mutexattr);
  mkcl_normal_mutexattr = NULL;
  (void) pthread_mutexattr_destroy(&errorcheck_mutexattr);
  mkcl_errorcheck_mutexattr = NULL;
  (void) pthread_mutexattr_destroy(&recursive_mutexattr);
  mkcl_recursive_mutexattr = NULL;
#else
# error Incomplete mkcl_init_early_threads().
#endif  /* elif MKCL_PTHREADS */

}


#ifdef __MINGW32__
MK_GC_API void MK_GC_CALL __MINGW_ATTRIB_NORETURN MK_GC_endthreadex(unsigned /* retval */);
#endif


void mkcl_thread_exit(MKCL, long status_code)
{
  if (env)
    {
      mkcl_object thread = env->own_thread;
      thread->thread.status = mkcl_thread_done;
      env->disable_interrupts = 2; /* This prevents any interrupts, even forced. */
      if ( thread == mkcl_core.shutdown_watchdog_thread )
        { /* this thread is about to bail out on its watchdog duties, so let's take act of that. */
          mk_mt_get_lock(env, 1, mkcl_core.shutdown_gate);
          if ( thread == mkcl_core.shutdown_watchdog_thread )
            {
              mkcl_core.shutdown_watchdog_thread = mk_cl_Cnil;
              mkcl_core.shutdown_watchdog_will_clean_up = mk_cl_Cnil;
            }
          mk_mt_giveup_lock(env, mkcl_core.shutdown_gate);
        }
    }

#if MKCL_PTHREADS
  pthread_exit((void *) status_code);
#elif MKCL_WINDOWS
  _endthreadex(status_code);
#else
# error "Missing implementation for mkcl_thread_exit()"
#endif
}

/* This here is to work around the wrapping Boehm's GC puts around pthread_join
   and thus get direct access to the real pthread version.
 */
#if MKCL_PTHREADS
#undef pthread_join
static int _true_pthread_join(pthread_t thread, void ** retval)
{
  return pthread_join(thread, retval);
}
#endif /* MKCL_PTHREADS */


mkcl_object mk_mt_thread_plist(MKCL, mkcl_object thread)
{
  mkcl_call_stack_check(env);
  if (mkcl_type_of(thread) != mkcl_t_thread)
    mkcl_FEwrong_type_argument(env, @'mt::thread', thread);
  @(return thread->thread.plist);
}

mkcl_object mk_mt_set_thread_plist(MKCL, mkcl_object thread, mkcl_object plist)
{
  mkcl_call_stack_check(env);
  if (mkcl_type_of(thread) != mkcl_t_thread)
    mkcl_FEwrong_type_argument(env, @'mt::thread', thread);
  @(return (thread->thread.plist = plist));
}

#if 0
void mkcl_initial_thread_cancelled(void * aux)
{
  mkcl_env env = aux;

  fprintf(stderr, "\n;; MKCL: Initial thread got cancelled!\n"); fflush(stderr);
}
#endif


mkcl_object mk_mt_test_for_thread_shutdown(MKCL)
{
  mkcl_object thread = env->own_thread;
  mkcl_object output = mk_cl_Cnil;

  mkcl_call_stack_check(env);
#if 0
  fprintf(stderr, "\n;; MKCL: In #'mt::test-for-thread-shutdown! errno = %d\n", errno); fflush(stderr);
#endif

  if (thread->thread.interrupt)
    {
      mkcl_object interrupt = thread->thread.interrupt;
      thread->thread.interrupt = mk_cl_Cnil; /* We do not want to handle the same interrupt twice. */

      {
#if MKCL_WINDOWS
	BOOL ok;

	MKCL_LIBC_NO_INTR(env, (ok = SetEvent(mkcl_sleeping_thread_interrupted)));
	if (!ok)
	  mkcl_FEwin32_error(env, "mk_mt_test_for_thread_shutdown failed on sem_post", 0);
#elif MKCL_PTHREADS
	int rc;

	MKCL_LIBC_NO_INTR(env, (rc = sem_post(mkcl_sleeping_thread_interrupted)));
	if (rc)
	  mkcl_FElibc_error(env, "mk_mt_test_for_thread_shutdown failed on sem_post", 0);
#else
# error Incomplete mk_mt_test_for_thread_shutdown().
#endif
      }

#if 0
      fprintf(stderr, "\n;; MKCL: In #'mt::test-for-thread-shutdown! errno = %d, About to run interrupt in [%s].\n",
	      errno, thread->thread.name->base_string.self);
      fflush(stderr);
#endif

      mkcl_funcall0(env, interrupt); /* We may be running with interrupts disabled. Is this a problem? JCB */
      output = @':interrupted';
    }

  if (thread->thread.shutdown_requested)
    {
#if 0
      fprintf(stderr, "\n;; MKCL: In #'mt::test-for-thread-shutdown! errno = %d, Thread [%s] will try to shutdown!\n",
	      errno, thread->thread.name->base_string.self);
      fflush(stderr);
#endif
      mk_mt_exit_thread(env, output = @':terminated');
    }

  @(return output);
}


mkcl_object
mk_mt_request_thread_shutdown(MKCL, mkcl_object thread)
{
  mkcl_call_stack_check(env);
  mkcl_assert_type_thread(env, thread);
  if (thread->thread.status == mkcl_thread_done)
    { @(return mk_cl_Cnil); }
  else
    {
#if 0
      fprintf(stderr, "\n;; MKCL: Requesting shutdown of thread [%s]!\n", thread->thread.name->base_string.self);
      fflush(stderr);
#endif
      thread->thread.shutdown_requested = TRUE;
      @(return mk_cl_Ct);
    }
}

mkcl_object
mk_mt_thread_shutdown_requested_p(MKCL, mkcl_object thread)
{
  mkcl_call_stack_check(env);
  mkcl_assert_type_thread(env, thread);
  @(return ((thread->thread.shutdown_requested == TRUE) ? mk_cl_Ct : mk_cl_Cnil));
}


mkcl_object
mk_si_non_interactive_thread_debugger_trap(MKCL, mkcl_object condition, mkcl_object old_hook)
{
  mkcl_object continue_fn = @+'continue';

  mkcl_call_stack_check(env);
  if (mkcl_functionp(env, continue_fn)) mkcl_funcall0(env, continue_fn);
  mk_cl_fresh_line(env, 1, mkcl_core.error_output);
  mkcl_write_cstr(env, "MKCL: Condition signaled in non-interactive thread: ", mkcl_core.error_output);
  mkcl_princ(env, mkcl_symbol_value(env, @'mt::*thread*'), mkcl_core.error_output);
  mkcl_terpri(env, mkcl_core.error_output);
  mkcl_write_cstr(env, "MKCL: Condition is: ", mkcl_core.error_output);
  mkcl_prin1(env, condition, mkcl_core.error_output);
  mkcl_princ(env, condition, mkcl_core.error_output);
  mkcl_terpri(env, mkcl_core.error_output);
  mkcl_finish_output(env, mkcl_core.error_output);
  mk_mt_abandon_thread(env, @':terminated');
  @(return); /* should never be reached! */
}
