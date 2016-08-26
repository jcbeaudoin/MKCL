/* -*- mode: c -*- */
/*
    unixint.c -- Unix interrupt interface.
*/
/*
    Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
    Copyright (c) 1990, Giuseppe Attardi.
    Copyright (c) 2001, Juan Jose Garcia Ripoll.
    Copyrignt (c) 2010-2016, Jean-Claude Beaudoin. (Completely rewritten 2010)

    MKCL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 3 of the License, or (at your option) any later version.

    See file '../../Copyright' for full details.
*/
#include <mkcl/mkcl.h>
#include <mkcl/internal.h>
#include <mkcl/mkcl-gc.h>

#include <errno.h>
#include <string.h>
#include <stdio.h>

#include <signal.h>

#if MKCL_WINDOWS
# include <malloc.h> /* for _resetstkoflw(). */
# include <windows.h>
#endif

#if MKCL_UNIX
# include <sys/types.h>
# include <sys/wait.h>
# include <dlfcn.h>
# include <ucontext.h>
#endif

#include <mkcl/mkcl-fenv.h>
#include <mkcl/mkcl-inl.h>

#if MKCL_UNIX
typedef void (*mkcl_sighandler_t)(int, siginfo_t *, void *);
#endif


#if MKCL_PTHREADS
# if MKCL_GC_7_2d
#  define MK_GC_SIG_SUSPEND MK_GC_suspend_signal()
#  define MK_GC_SIG_THR_RESTART MK_GC_thread_restart_signal()
# else
#  define MK_GC_SIG_SUSPEND MK_GC_get_suspend_signal()
#  define MK_GC_SIG_THR_RESTART MK_GC_get_thr_restart_signal()
# endif

# if 0
#  define DEBUG_SIGNALS
static int mkcl_lose_on_rogue_signal = 1;
# else
static int mkcl_lose_on_rogue_signal = 0;
# endif

static int interrupt_sig = 0;
static int resume_sig = 0;
static int wake_up_sig = 0;
static pid_t mkcl_pid;
#endif /* __linux */

/******************************* ------- ******************************/


#if MKCL_UNIX
static char * ltoad(long val, char * str)
{
  int i = 0, j = 0;
  long quotient = (val > 0) ? val : -val;
  char buf[24];
  
  if ( quotient == 0 )
    buf[i++] = '0';
  else
    do {
      char remainder = quotient % 10;
      quotient = quotient / 10;

      buf[i++] = remainder + '0';
    } while ( quotient != 0 );
  if ( val < 0 ) 
    buf[i++] = '-';

  while ( i > 0 )
    str[j++] = buf[--i];
  str[j] = '\0';
  return(str);
}
#endif /* MKCL_UNIX */

/******************************* ------- ******************************/

/* New fully POSIX compliant signal processing. JCB */

#if MKCL_UNIX

static int stderr_fd;

static
void sig_print(const char * const msg)
{
  /* This call may fail and it is just too bad. We do not want to do anything about it. */
  ssize_t count = write(stderr_fd, msg, strlen(msg));
}

#define signal_name(sig) #sig 

# if __linux
static const char * const signal_names[MKCL_BASE_SIGMAX + 1] =
  {
    "",
    signal_name(SIGHUP),
    signal_name(SIGINT),
    signal_name(SIGQUIT),
    signal_name(SIGILL),
    signal_name(SIGTRAP),
    signal_name(SIGABRT),
    signal_name(SIGBUS),
    signal_name(SIGFPE),
    signal_name(SIGKILL),
    signal_name(SIGUSR1),
    signal_name(SIGSEGV),
    signal_name(SIGUSR2),
    signal_name(SIGPIPE),
    signal_name(SIGALRM),
    signal_name(SIGTERM),
    signal_name(SIGSTKFLT),
    signal_name(SIGCHLD),
    signal_name(SIGCONT),
    signal_name(SIGSTOP),
    signal_name(SIGTSTP),
    signal_name(SIGTTIN),
    signal_name(SIGTTOU),
    signal_name(SIGURG),
    signal_name(SIGXCPU),
    signal_name(SIGXFSZ),
    signal_name(SIGVTALRM),
    signal_name(SIGPROF),
    signal_name(SIGWINCH),
    signal_name(SIGIO),
    signal_name(SIGPWR),
    signal_name(SIGSYS),
  };
# elif __FreeBSD__
static const char * const signal_names[MKCL_BASE_SIGMAX + 1] =
  {
    "",
    signal_name(SIGHUP),
    signal_name(SIGINT),
    signal_name(SIGQUIT),
    signal_name(SIGILL),
    signal_name(SIGTRAP),
    signal_name(SIGABRT),
    signal_name(SIGEMT),
    signal_name(SIGFPE),
    signal_name(SIGKILL),
    signal_name(SIGBUS),
    signal_name(SIGSEGV),
    signal_name(SIGSYS),
    signal_name(SIGPIPE),
    signal_name(SIGALRM),
    signal_name(SIGTERM),
    signal_name(SIGURG),
    signal_name(SIGSTOP),
    signal_name(SIGTSTP),
    signal_name(SIGCONT),
    signal_name(SIGCHLD),
    signal_name(SIGTTIN),
    signal_name(SIGTTOU),
    signal_name(SIGIO),
    signal_name(SIGXCPU),
    signal_name(SIGXFSZ),
    signal_name(SIGVTALRM),
    signal_name(SIGPROF),
    signal_name(SIGWINCH),
    signal_name(SIGINFO),
    signal_name(SIGUSR1),
    signal_name(SIGUSR2),
  };
# endif



struct mkcl_signal_control mkcl_signals[MKCL_SIGMAX + 1] = { { FALSE } };

volatile int mkcl_terminal_signal_number = -1;

static
void sig_perror(char * msg)
{
  int my_errno = errno;
  char errno_str[24];

  sig_print(msg);
  sig_print(" errno = ");
  sig_print(ltoad(my_errno, errno_str));
  sig_print("\n");
}

static void bark_about_signal(siginfo_t *info, const char * const msg)
{
  char buf[24];

  sig_print(msg);
  sig_print(" from pid = ");
  sig_print(ltoad(info->si_pid, buf));
  sig_print(" and uid = ");
  sig_print(ltoad(info->si_uid, buf));
  sig_print(", si_signo = ");
  sig_print(ltoad(info->si_signo, buf));
  sig_print(", si_code = ");
  sig_print(ltoad(info->si_code, buf));
  sig_print(".\n");
  return;
}

static void sig_print_sigmask(sigset_t * set)
{
    int i;
    char sig_num[24];

    sig_print("Blocked: ");
    for (i = 1; i <= MKCL_SIGMAX; i++) {
      if (sigismember(set, i))
	{ sig_print(ltoad(i, sig_num)); sig_print(" "); }
    }
    sig_print("\n");
}

static void show_sigmask(void)
{
  sigset_t mask;

  pthread_sigmask(SIG_SETMASK, NULL, &mask);
  sig_print_sigmask(&mask);
}

static
void posix_signal(MKCL, int sig, void (*handler)(int, siginfo_t *, void *))
{
  struct sigaction new_action;

  /* As a matter of style we should identify values SIG_DFL and SIG_IGN
     for handler and give them special treatment. */
  
  new_action.sa_sigaction = handler;
  sigemptyset(&new_action.sa_mask);

  /* We do not want our interrupt signal handler to interrupt in the 
     middle of its business another signal handler. Such nesting
     of signal handlers one on top of another would be a serious
     source of headaches.  JCB */
  if (sigaddset(&new_action.sa_mask, interrupt_sig))
    mkcl_FElibc_error(env, "posix_signal failed on sigaddset.", 0);

#if 1
  new_action.sa_flags = SA_SIGINFO;
#else
  new_action.sa_flags = SA_SIGINFO | SA_ONSTACK; /* Let's try the signal alternate stack. JCB */ /* Unusable with Boehm's GC */
#endif
  
  if (sigaction(sig, &new_action, &(mkcl_signals[sig].old_action)))
    mkcl_FElibc_error(env, "posix_signal failed on sigaction.", 0);

  mkcl_signals[sig].installed = TRUE;

#ifdef DEBUG_SIGNALS
  fprintf(stderr, "\nInstalled handler for signal %d.\n", sig); fflush(stderr);
  if (mkcl_signals[sig].old_action.sa_handler != SIG_DFL)
    {
      fprintf(stderr, "\nOld signal handler for signal %d is not default: %p\n",
	      sig, mkcl_signals[sig].old_action.sa_handler);
      fflush(stderr);
    }
#endif
}

static pid_t mkcl_debugged_by_process_id = 0; /* 0 is never a valid process id. */

#ifndef SI_TKILL
# ifdef SI_LWP
#  define SI_TKILL SI_LWP
# endif
#endif

void mkcl_resume_signal_handler(int sig, siginfo_t * info, void * aux)
{
  mkcl_env env = mkcl_interrupted_thread_env;

#ifdef DEBUG_SIGNALS
  sig_print("\nInside mkcl_resume_signal_handler.\n");
  show_sigmask();
#endif

  if ( (info->si_code != SI_TKILL && info->si_code != SI_USER)
       || (info->si_pid != mkcl_pid && info->si_pid != mkcl_debugged_by_process_id))
    { /* This is a rogue signal! */
      bark_about_signal(info, "\nMKCL: received an invalid resume signal");
      return;
    }

  env->own_thread->thread.resume_handler_ran = TRUE;

#if 0 /* moved to interrupt_signal_handler() */
  if (sem_post(mkcl_interrupted_thread_resumed))
    sig_perror("\nmkcl_resume_signal_handler failed on sem_post.");
#endif
}

static void mkcl_fix_sigmask(int sig)
{
  sigset_t interrupt_sigmask;

#ifdef DEBUG_SIGNALS
  sig_print("\nFixing sigmask!\n");
#endif

  sigemptyset(&interrupt_sigmask);
  if (sigaddset(&interrupt_sigmask, interrupt_sig))
    sig_perror("\nmkcl_fix_sigmask failed on sigaddset.");

  if ( sig != 0 )
    if (sigaddset(&interrupt_sigmask, sig))
      sig_perror("\nmkcl_fix_sigmask failed on sigaddset.");

  /* Strickly speaking this one is not POSIX async-signal-safe
     de jure but seems to be de facto on Linux (glibc 2.5)
     and Solaris (10 and some earlier). On Solaris the purist
     could use thr_sigsetmask() instead.
     The alternative would be to plug every call to mkcl_frs_pop()
     which is a quite annoying overhead. JCB */
  pthread_sigmask(SIG_UNBLOCK, &interrupt_sigmask, NULL);
}

void mkcl_interrupt_signal_handler(int sig, siginfo_t *info, void *aux)
{
  mkcl_env env = mkcl_interrupted_thread_env;

#ifdef DEBUG_SIGNALS
  sig_print("\nInside mkcl_interrupt_signal_handler.\n");
  show_sigmask();
#endif

  if ( (info->si_code != SI_TKILL && info->si_code != SI_USER)
       || (info->si_pid != mkcl_pid && info->si_pid != mkcl_debugged_by_process_id))
    { /* This is a rogue signal! */
      bark_about_signal(info, "\nMKCL: received an invalid interrupt signal");
      return;
    }

  if ( env == NULL /* The thread died unexpectedly! */
       || (env->own_thread->thread.status == mkcl_thread_done) /* The thread is dying on us! */
       || (env->disable_interrupts > 1) /* being interrupted already! */
       || (env->disable_interrupts && !mkcl_interrupt_forcefully) )
    {
      mkcl_interrupt_refused = TRUE;
      if (sem_post(mkcl_interrupted_thread_suspended))
	sig_perror("\nmkcl_interrupt_signal_handler failed on sem_post.");

#ifdef DEBUG_SIGNALS
      sig_print("\nmkcl_interrupt_signal_handler refused interrupt.\n");
      show_sigmask();
#endif
      return;
    }
  else
    if (sem_post(mkcl_interrupted_thread_suspended))
      sig_perror("\nmkcl_interrupt_signal_handler failed on sem_post.");

  {
    sigset_t suspend_sigmask;
   
    sigfillset(&suspend_sigmask);
    if (sigdelset(&suspend_sigmask, resume_sig))
      sig_perror("\nmkcl_interrupt_signal_handler failed on sigdelset.");
    if (sigdelset(&suspend_sigmask, MK_GC_SIG_SUSPEND)) /* Because of Boehm's GC */
      sig_perror("\nmkcl_interrupt_signal_handler failed on sigdelset.");

#ifdef DEBUG_SIGNALS
    /* We want to be able to get a core dump if need be. */
    if (sigdelset(&suspend_sigmask, SIGQUIT))
      sig_perror("\nmkcl_interrupt_signal_handler failed on sigdelset.");

    sig_print("\nAbout to sigsuspend on this mask:");
    sig_print_sigmask(&suspend_sigmask);
#endif

    do {
      sigsuspend(&suspend_sigmask);
      if ( errno != EINTR )
	sig_perror("\nmkcl_interrupt_signal_handler failed on sigsuspend.");
    } while ( env->own_thread->thread.resume_handler_ran != TRUE );

    /* Now that we have seen it let's clear it right away. */
    env->own_thread->thread.resume_handler_ran = FALSE;

    if (sem_post(mkcl_interrupted_thread_resumed))
      sig_perror("\nmkcl_resume_signal_handler failed on sem_post.");
  }

  if (env->nlj_fr)
    {/* resume unwinding initiated from the interrupt function. */

      mkcl_fix_sigmask(0);

#ifdef DEBUG_SIGNALS
      sig_print("\nResuming unwinding in interrupted thread.\n");
      show_sigmask();
#endif

      mkcl_unwind(env, env->nlj_fr);
    }
#ifdef DEBUG_SIGNALS
  else
    {
      sig_print("\nResuming interrupted thread normally.\n");
      show_sigmask();
    }
#endif
}

void mkcl_generic_signal_handler(int sig, siginfo_t *info, void *aux)
{
#ifdef DEBUG_SIGNALS
  sig_print("\nInside mkcl_generic_signal_handler for signal ");
  {
    char buf[24];
    sig_print(ltoad(sig, buf));
    sig_print(".\n");
  }
  show_sigmask();
#endif

  if (sem_post(mkcl_signals[sig].sem))
    sig_perror("\nmkcl_generic_signal_handler failed on sem_post.");
}

void mkcl_terminal_signal_handler(int sig, siginfo_t *info, void *aux)
{
#ifdef DEBUG_SIGNALS
  sig_print("\nInside mkcl_terminal_signal_handler for signal ");
  {
    char buf[24];
    sig_print(ltoad(sig, buf));
    sig_print(".\n");
  }
  show_sigmask();
#endif

  mkcl_terminal_signal_number = sig;

  if (sem_post(mkcl_signals[0].sem))
    sig_perror("\nmkcl_terminal_signal_handler failed on sem_post.");
}

void mkcl_wake_up_signal_handler(int sig, siginfo_t *info, void *aux)
{
#ifdef DEBUG_SIGNALS
  sig_print("\nInside mkcl_wake_up_signal_handler for signal ");
  {
    char buf[24];
    sig_print(ltoad(sig, buf));
    sig_print(".\n");
  }
  /* show_sigmask(); */
#endif
  struct sigaction * old = &(mkcl_signals[sig].old_action);
    
  /* SIGCONT resumes the process by default. */
  if (sig == SIGCONT && old->sa_handler != SIG_IGN)
    {
#ifdef DEBUG_SIGNALS
      sig_print("\nSIGCONT handler chaining.");
#endif
      if (old->sa_flags & SA_SIGINFO )
	(old->sa_sigaction)(sig, info, aux);
      else
	(old->sa_handler)(sig);
    }
}

static void install_wake_up_signal_handler(MKCL, int sig)
{
  posix_signal(env, sig, mkcl_wake_up_signal_handler);
}

static void install_resume_signal_handler(MKCL, int sig)
{
  posix_signal(env, sig, mkcl_resume_signal_handler);
}

static void install_interrupt_signal_handler(MKCL, int sig)
{
  posix_signal(env, sig, mkcl_interrupt_signal_handler);
}

static void 
install_lisp_signal_handler(MKCL, int signum, mkcl_object func_designator)
{
  /* Create the signal servicing thread */
  char sig_thread_name[128];

  if (signum <= MKCL_BASE_SIGMAX)
    sprintf(sig_thread_name, "%s handling daemon", signal_names[signum]);
  else
    sprintf(sig_thread_name, "SIG%d handling daemon", signum);


  mkcl_create_signal_servicing_thread(env, sig_thread_name, signum, func_designator);
}

static void 
install_lisp_terminal_signal_handler(MKCL)
{
  /* Create the signal servicing thread */
  char * sig_thread_name = "Terminal signal handling daemon";

  mkcl_create_signal_servicing_thread(env, sig_thread_name, 0, @'si::terminal-signal-handler');
}

/* End of new fully POSIX compliant signal code */
#endif /* MKCL_UNIX */

@(defun si::setup-for-gdb (&o pid)
@
  {
#if MKCL_UNIX
    if (mkcl_Null(pid))
      { @(return MKCL_MAKE_FIXNUM(mkcl_debugged_by_process_id = getppid())); }
    else
      { @(return MKCL_MAKE_FIXNUM(mkcl_debugged_by_process_id = mkcl_safe_fixnum_to_word(env, pid))); }
#else
    @(return MKCL_MAKE_FIXNUM(0));
#endif
  }
@)



/******************************* ------- ******************************/


#if MKCL_UNIX    /* Posix (linux) synchronous signal handlers.*/

static void
maybe_lose(char * msg)
{
#ifdef DEBUG_SIGNALS
  if ( mkcl_lose_on_rogue_signal )
    {
      fprintf(stderr, "\n%s\n", msg); fflush(stderr);
      abort();
    }
  else
#endif
    { sig_print("\n"); sig_print(msg); sig_print("\n"); }
}

void mkcl_sigfpe_handler(int sig, siginfo_t *info, void *aux)
{
#ifdef DEBUG_SIGNALS
  {
    char buf[24];
    sig_print("\nInside mkcl_sigfpe_handler, sig = ");
    sig_print(ltoad(sig, buf));
    sig_print(".\n");
  }
#endif

  const mkcl_env env = MKCL_ENV();

  if ( env == NULL )
    maybe_lose("MKCL: mkcl_sigfpe_handler called outside a lisp thread!");
  else if (!mkcl_get_option(MKCL_OPT_BOOTED)) {
    printf("For signal %d, %s\n", sig, sys_siglist[sig]);
    mkcl_lose(env, "In mkcl_sigfpe_handler. Got signal before environment was installed on our thread.");
  }
  else
    {
      mkcl_object condition = @'arithmetic-error';

      if (info) {
	char * siginfo_str;
	switch (info->si_code)
	  {
	  case FPE_INTDIV:
	    condition = @'division-by-zero';
	    siginfo_str = "FPE_INTDIV";
	    break;
	  case FPE_FLTDIV:
	    condition = @'division-by-zero';
	    siginfo_str = "FPE_FLTDIV";
	    break;
	  case FPE_FLTOVF:
	    condition = @'floating-point-overflow';
	    siginfo_str = "FPE_FLTOVF";
	    break;
	  case FPE_FLTUND:
	    condition = @'floating-point-underflow';
	    siginfo_str = "FPE_FLTUND";
	    break;
	  case FPE_FLTRES:
	    condition = @'floating-point-inexact';
	    siginfo_str = "FPE_FLTRES";
	    break;
	  case FPE_FLTINV:
	    condition = @'floating-point-invalid-operation';
	    siginfo_str = "FPE_FLTINV";
	    break;

	  case FPE_FLTSUB: /* Subscript out of range.  */
	    siginfo_str = "FPE_FLTSUB";
	    break;
	  case FPE_INTOVF: /* Integer overflow.  */
	    siginfo_str = "FPE_INTOVF";
	    /* What do we do with these? JCB */
	    break;
	  default:
	    sig_print("\nMKCL: received an unknown SIGFPE signal! Ignoring it.\n");
	    return;
	  }
      }
      MKCL_UNWIND_PROTECT_BEGIN(env) {
	mk_cl_error(env, 1, condition);
      } MKCL_UNWIND_PROTECT_EXIT {
	mkcl_reactivate_fpe_set(env);
	mkcl_fix_sigmask(sig);
      } MKCL_UNWIND_PROTECT_END;
    }

#ifdef DEBUG_SIGNALS
  sig_print("\nLeaving mkcl_sigfpe_handler.\n");
#endif
}

static void
unblock_signal(int signal)
{
  struct sigaction oact;
  sigset_t unblock_mask, current_mask;
  sigaction(signal, NULL, &oact);
  unblock_mask = oact.sa_mask;

  sigaddset(&unblock_mask, signal);
  pthread_sigmask(SIG_UNBLOCK, &unblock_mask, NULL);
  pthread_sigmask(SIG_SETMASK, NULL, &current_mask);
}

void mkcl_synchronous_signal_handler(int sig, siginfo_t *info, void *aux)
{
#ifdef DEBUG_SIGNALS
  {
    char buf[24];
    sig_print("\nInside mkcl_synchronous_signal_handler, sig = ");
    sig_print(ltoad(sig, buf));
    sig_print(".\n");
  }
#endif

  const mkcl_env env = MKCL_ENV();

  if ( env == NULL )
    maybe_lose("MKCL: mkcl_synchronous_signal_handler called outside a lisp thread!");
  else
    {
      if (!mkcl_get_option(MKCL_OPT_BOOTED)) {
	printf("For signal %d, %s\n", sig, sys_siglist[sig]);
	mkcl_lose(env, "In mkcl_synchronous_signal_handler. "
		  "Got signal before environment was installed"
		  " on our thread.");
      }

      mkcl_fix_sigmask(sig);
      mkcl_FEerror(env, "Synchronous signal ~D caught.", 1, MKCL_MAKE_FIXNUM(sig));
    }
#ifdef DEBUG_SIGNALS
  sig_print("\nLeaving mkcl_synchronous_signal_handler.\n");
#endif
}

#define MKCL_SIGSEGV_MAXIMUM_NESTING_DEPTH 5

void mkcl_sigsegv_handler(int sig, siginfo_t *info, void *aux)
{
  ucontext_t * ctx = aux;
#if defined(DEBUG_SIGNALS)
  {
    char buf[24];
    sig_print("\nInside mkcl_sigsegv_handler, sig = ");
    sig_print(ltoad(sig, buf));
    sig_print(".\n");
  }
#endif

  const mkcl_env env = MKCL_ENV();

  if ( env == NULL )
    {
      struct sigaction * old = &(mkcl_signals[sig].old_action);
      maybe_lose("MKCL: mkcl_sigsegv_handler called outside a lisp thread!");

      if (old->sa_handler != SIG_IGN && old->sa_handler != SIG_DFL)
	{
	  if (old->sa_flags & SA_SIGINFO )
	    (old->sa_sigaction)(sig, info, aux);
	  else
	    (old->sa_handler)(sig);
	}
      
    }
  else if (env->disable_interrupts >= 2)
    { /* The risk of reentering a locked region in the GC is just too high. */
      maybe_lose("MKCL: mkcl_sigsegv_handler called inside uninterruptable foreign code. Cannot re-enter lisp!");

      (void) sigaction(sig, &(mkcl_signals[sig].old_action), NULL);
      (void) kill(getpid(), sig); /* resending since we do not know what to do else! */
    }
  else
    {
      if (!mkcl_get_option(MKCL_OPT_BOOTED)) {
	printf("For signal %d, %s\n", sig, sys_siglist[sig]);
	mkcl_lose(env, "In mkcl_sigsegv_handler. "
		  "Got signal before environment was installed"
		  " on our thread.");
      }

      {
#if 0  /* This code assumes the use of sigaltstack() and sigaltstack() is incompatible with Boehm's GC */
	uintptr_t fault_address = (uintptr_t) info->si_addr;
#ifdef __x86_64
	mkcl_index fault_BP = ctx->uc_mcontext.gregs[REG_RBP]; /* REG_RSP == 10 on x86_64.*/
	mkcl_index fault_SP = ctx->uc_mcontext.gregs[REG_RSP]; /* REG_RSP == 15 on x86_64.*/
#else
	mkcl_index fault_BP = ctx->uc_mcontext.gregs[REG_EBP]; /* REG_EBP == 6 on x86.*/
	mkcl_index fault_SP = ctx->uc_mcontext.gregs[REG_ESP]; /* REG_ESP == 7 on x86.*/
#endif
	/* The following test assumes a stack that grows downward. */
	if ((fault_BP >= fault_address) && (fault_address >= (fault_SP - mkcl_core.pagesize)))
	  {
	    mkcl_fix_sigmask(sig);
	    mk_cl_error(env, 5, @'mkcl::stack-overflow',
			@':size', mkcl_make_unsigned_integer(env, env->cs_size),
			@':type', @'si::call-stack');
	  }
	else
#endif
	  {
	    char address_cstr[24];
	    mkcl_object address_str;

	    switch (info->si_code)
	      {
	      case SI_USER: /* Software generated (by kill or raise). */
		if (info->si_pid != mkcl_debugged_by_process_id) goto error; else goto normal;
	      case SEGV_MAPERR: /* Address not mapped to object.  */
	      case SEGV_ACCERR: /* Invalid permissions for mapped object.  */
              normal:
		sprintf(address_cstr, "%p", info->si_addr);
		break;
	      error:
	      default:
#ifdef DEBUG_SIGNALS
		sig_print("\nMKCL: received an invalid SIGSEGV signal from pid = ");
		sig_print(ltoad(info->si_pid, address_cstr));
		sig_print(" and uid = ");
		sig_print(ltoad(info->si_uid, address_cstr));
		sig_print(", si_code = ");
		sig_print(ltoad(info->si_code, address_cstr));
		if (info->si_code == SI_KERNEL) sig_print(" (SI_KERNEL)");
		sig_print(".\n");
#endif
		sprintf(address_cstr, "invalid address");
		break;
	      }
	    address_str =  mkcl_make_base_string_copy(env, address_cstr);
	    mkcl_fix_sigmask(sig);

	    mk_cl_error(env, 3, @'mkcl::segmentation-violation', @':address', address_str);
	  }
      }
    }

#ifdef DEBUG_SIGNALS
  sig_print("\nLeaving mkcl_sigsegv_handler.\n");
#endif
}


void mkcl_sigbus_handler(int sig, siginfo_t *info, void *aux)
{
#ifdef DEBUG_SIGNALS
  {
    char buf[24];
    sig_print("\nInside mkcl_sigbus_handler, sig = ");
    sig_print(ltoad(sig, buf));
    sig_print(".\n");
  }
#endif

  const mkcl_env env = MKCL_ENV();

  if ( env == NULL )
    maybe_lose("MKCL: mkcl_sigbus_handler called outside a lisp thread!");
  else
    {
      if (!mkcl_get_option(MKCL_OPT_BOOTED)) {
	printf("For signal %d, %s\n", sig, sys_siglist[sig]);
	mkcl_lose(env, "In mkcl_sigbus_handler. "
		  "Got signal before environment was installed"
		  " on our thread.");
      }

      {
	char address_cstr[24];
	mkcl_object address_str;
	    
	switch (info->si_code)
	  {
	  case BUS_ADRALN: /* Invalid address alignment.  */
	  case BUS_ADRERR: /* Non-existant physical address.  */
	  case BUS_OBJERR: /* Object specific hardware error.  */
	    sprintf(address_cstr, "%p", info->si_addr);
	    break;
	  default:
#ifdef DEBUG_SIGNALS
	    sig_print("\nMKCL: received an invalid SIGBUS signal from pid = ");
	    sig_print(ltoad(info->si_pid, address_cstr));
	    sig_print(" and uid = ");
	    sig_print(ltoad(info->si_uid, address_cstr));
	    sig_print(", si_code = ");
	    sig_print(ltoad(info->si_code, address_cstr));
	    sig_print(".\n");
#endif
	    sprintf(address_cstr, "invalid address");
	    break;
	  }
	address_str = mkcl_make_base_string_copy(env, address_cstr);
	mkcl_fix_sigmask(sig);
	mk_cl_error(env, 3, @'mkcl::segmentation-violation', @':address', address_str);
      }
    }

#ifdef DEBUG_SIGNALS
  sig_print("\nLeaving mkcl_sigbus_handler.\n");
#endif
}

void mkcl_sigchld_handler(int sig, siginfo_t *info, void *aux)
{
#ifdef DEBUG_SIGNALS
  {
    char buf[24];
    sig_print("\nInside mkcl_sigchld_handler, sig = ");
    sig_print(ltoad(sig, buf));
    sig_print(".\n");
  }
#endif

  pid_t dead_child_pid = info->si_pid;
  mkcl_object detached_children = mkcl_core.detached_children;
  long i = 0;

  mkcl_loop_for_on_unsafe(detached_children) {
    mkcl_object child_pid = MKCL_CONS_CAR(detached_children);
    if (MKCL_FIXNUMP(child_pid) && (mkcl_fixnum_to_word(child_pid) == dead_child_pid)) /* Was it ours? */
      {
	int status;

	while ((waitpid(dead_child_pid, &status, 0) < 0) && (errno == EINTR)); /* To prevent a zombie. */

	/* This splicing works because of the sentinel put at the end of the detached_children list. */
	MKCL_RPLACA(detached_children, MKCL_CONS_CAR(MKCL_CONS_CDR(detached_children)));
	MKCL_RPLACD(detached_children, MKCL_CONS_CDR(MKCL_CONS_CDR(detached_children)));

#ifdef DEBUG_SIGNALS
	{
	  char buf[24];
	  sig_print("\n");
	  sig_print(ltoad(i, buf));
	  sig_print(" nth, It was a detached one of ours! Leaving mkcl_sigchld_handler.\n");
	}
#endif
	return;
      }
    else if (MKCL_BIGNUMP(child_pid) && mpz_cmp_si(child_pid->big.big_num, dead_child_pid)) /* Was it ours? */
      {
	int status;

	while ((waitpid(dead_child_pid, &status, 0) < 0) && (errno == EINTR)); /* To prevent a zombie. */

	/* This splicing works because of the sentinel put at the end of the detached_children list. */
	MKCL_RPLACA(detached_children, MKCL_CONS_CAR(MKCL_CONS_CDR(detached_children)));
	MKCL_RPLACD(detached_children, MKCL_CONS_CDR(MKCL_CONS_CDR(detached_children)));

#ifdef DEBUG_SIGNALS
	sig_print("\nIt was a detached one (big) of ours! Leaving mkcl_sigchld_handler.\n");
#endif
	return;
      }
    i++;
  } mkcl_end_loop_for_on;

  mkcl_object children = mkcl_core.children;
  i = 0;
  mkcl_loop_for_on_unsafe(children) {
    mkcl_object child_pid = MKCL_CONS_CAR(children);
    if (MKCL_FIXNUMP(child_pid) && (mkcl_fixnum_to_word(child_pid) == dead_child_pid)) /* Was it ours? */
      {
#ifdef DEBUG_SIGNALS
	char buf[24];
	sig_print("\n");
	sig_print(ltoad(i, buf));
	sig_print(" nth, It was one of ours! Leaving mkcl_sigchld_handler.\n");
#endif
	return;
      }
    else if (MKCL_BIGNUMP(child_pid) && mpz_cmp_si(child_pid->big.big_num, dead_child_pid)) /* Was it ours? */
      {
#ifdef DEBUG_SIGNALS
	sig_print("\nIt was one (big) of ours! Leaving mkcl_sigchld_handler.\n");
#endif
	return;
      }
    /* else nice screw-up! */
    i++;
  } mkcl_end_loop_for_on;
  
  /* pass it on to the previous signal handler */
  {
    struct sigaction * old = &(mkcl_signals[sig].old_action);
    
    if (old->sa_handler == SIG_IGN)
      {
	int status;

	while (waitpid(dead_child_pid, &status, 0) < 0) /* To prevent a potential zombie. */
	  if (errno != EINTR) {
	    break;
	  }
#ifdef DEBUG_SIGNALS
	sig_print("\nIgnoring SIGCHLD signal.");
#endif
      }
    else if (old->sa_handler != SIG_DFL) /* SIGCHLD is ignored (with zombies preserved) by default. */
      {
#ifdef DEBUG_SIGNALS
	sig_print("\nSIGCHLD handler chaining.");
#endif
	if (old->sa_flags & SA_SIGINFO )
	  (old->sa_sigaction)(sig, info, aux);
	else
	  (old->sa_handler)(sig);
      }
  }

#ifdef DEBUG_SIGNALS
  sig_print("\nLeaving mkcl_sigchld_handler.\n");
#endif
}

void mkcl_sigpipe_handler(int sig, siginfo_t *info, void *aux)
{
  struct sigaction * old = &(mkcl_signals[sig].old_action);
    
  /* SIGPIPE is terminal by default and we cannot have that. */
  if (old->sa_handler != SIG_DFL && old->sa_handler != SIG_IGN)
    {
#ifdef DEBUG_SIGNALS
      sig_print("\nSIGPIPE handler chaining.");
#endif
      if (old->sa_flags & SA_SIGINFO )
	(old->sa_sigaction)(sig, info, aux);
      else
	(old->sa_handler)(sig);
    }
}

#endif /* __linux */

#if MKCL_WINDOWS

static volatile BOOL console_ctrl_event = FALSE;

BOOL mkcl_saw_console_ctrl_event(void)
{
  BOOL saw_it = console_ctrl_event;
  
  console_ctrl_event = FALSE;
  return saw_it;
}


static const mkcl_base_string_object(console_ctrl_event_handler_name_obj, "Console control event handler");
static const mkcl_object console_ctrl_event_handler_name = (mkcl_object) &console_ctrl_event_handler_name_obj;

static void handle_console_ctrl_event(mkcl_object lisp_handler, int signo)
{
  char stack_mark = 0;
  const mkcl_object name = console_ctrl_event_handler_name;
  const mkcl_env env = mkcl_import_current_thread(name, mk_cl_Cnil, NULL, NULL);

  if ( env == NULL )
    {
      printf("\nMKCL: Unable to create console control event thread!\n");
      fflush(NULL);
    }
  else
    {
      mkcl_object thread = env->own_thread;

      /* mk_mt_thread_detach(env, thread); */ /* probably much too strong. */

      MKCL_CATCH_ALL_BEGIN(env) {
	MKCL_SETUP_CALL_STACK_ROOT_GUARD(env);

	mkcl_setup_thread_lisp_context(env, &stack_mark);

	mkcl_register_thread_as_active(env, thread);

	mkcl_enable_interrupts(env);
	mkcl_funcall1(env, lisp_handler, MKCL_MAKE_FIXNUM(signo));
	mkcl_disable_interrupts(env);
	mkcl_cleanup_thread_lisp_context(env);
#if 0
      } MKCL_CATCH_ALL_IF_CAUGHT {
#endif
      } MKCL_CATCH_ALL_END;
      thread->thread.status = mkcl_thread_done;

      mkcl_release_current_thread(env);
    }
}

static LONG handle_fpe(EXCEPTION_POINTERS* ep)
{
  const mkcl_env env = MKCL_ENV();
  mkcl_object condition = @'arithmetic-error';
  char * cond_str;

  switch (ep->ExceptionRecord->ExceptionCode)
    {
      /* Catch all arithmetic exceptions */
    case EXCEPTION_FLT_DIVIDE_BY_ZERO:
      condition = @'division-by-zero';
      cond_str = "EXCEPTION_FLT_DIVIDE_BY_ZERO";
      break;
    case EXCEPTION_FLT_OVERFLOW:
      condition = @'floating-point-overflow';
      cond_str = "EXCEPTION_FLT_OVERFLOW";
      break;
    case EXCEPTION_FLT_UNDERFLOW:
      condition = @'floating-point-underflow';
      cond_str = "EXCEPTION_FLT_UNDERFLOW";
      break;
    case EXCEPTION_FLT_INVALID_OPERATION:
      condition = @'floating-point-invalid-operation';
      cond_str = "EXCEPTION_FLT_INVALID_OPERATION";
      break;
    case EXCEPTION_FLT_INEXACT_RESULT:
      condition = @'floating-point-inexact';
      cond_str = "EXCEPTION_FLT_INEXACT_RESULT";
      break;
    case EXCEPTION_FLT_DENORMAL_OPERAND:
      cond_str = "EXCEPTION_FLT_DENORMAL_OPERAND";
      break;
    case EXCEPTION_INT_DIVIDE_BY_ZERO:
      cond_str = "EXCEPTION_INT_DIVIDE_BY_ZERO";
      break;
    case EXCEPTION_INT_OVERFLOW:
      cond_str = "EXCEPTION_INT_OVERFLOW";
      break;
    case EXCEPTION_FLT_STACK_CHECK:
       /* Not really handled! JCB */
      cond_str = "EXCEPTION_FLT_STACK_CHECK";
      break;
    }

  unsigned int old_fp_status = _clearfp();

  mk_cl_error(env, 1, condition); 

  return EXCEPTION_CONTINUE_SEARCH;
}

static LONG handle_access_violation(EXCEPTION_POINTERS* ep)
{
  const mkcl_env env = MKCL_ENV();
  char address_cstr[24];
  mkcl_object address_str;

  sprintf(address_cstr, "%p", (void *) ep->ExceptionRecord->ExceptionInformation[1]);
  address_str =  mkcl_make_base_string_copy(env, address_cstr);

  mk_cl_error(env, 3, @'mkcl::segmentation-violation', @':address', address_str);

  return EXCEPTION_CONTINUE_SEARCH;
}

static LONG handle_stack_overflow(EXCEPTION_POINTERS* ep)
{
  const mkcl_env env = MKCL_ENV();
  int ok = _resetstkoflw();

#if 0
  printf("\nMKCL: Received EXCEPTION_STACK_OVERFLOW!\n");
  printf("env->disable_interrupts = %d\n", (env)->disable_interrupts);
  if (env->disable_interrupts)
    {
      printf("env->interrupt_disabler_file = %s\n", (env)->interrupt_disabler_file);
      printf("env->interrupt_disabler_lineno = %llu\n", (env)->interrupt_disabler_lineno);
    }
  printf("ExceptionRecord: %p\n", ep->ExceptionRecord->ExceptionRecord);
  printf("ExceptionAddress: %p\n", ep->ExceptionRecord->ExceptionAddress);
  printf("NumberParameters: %lu\n", ep->ExceptionRecord->NumberParameters);
  printf("ExceptionInformation[0]: %p\n", ep->ExceptionRecord->ExceptionInformation[0]);
  printf("ExceptionInformation[1]: %p\n", ep->ExceptionRecord->ExceptionInformation[1]);
  printf("thread name: %s\n", env->own_thread->thread.name->base_string.self);
  printf("tid = %d, Stack size = %lu, stack base = %p, stack top = %p.\n",
	 (env->own_thread ? env->own_thread->thread.tid : 0),
	 env->cs_size, env->cs_org, env->cs_org + env->cs_size);
  fflush(NULL);
#endif

  if (ok)
    {
#if 0
      printf("_resetstkoflw() said OK! Try to call the debugger...");
      fflush(NULL);
#endif
      env->cs_overflowing = TRUE;
      mk_cl_error(env, 5, @'mkcl::stack-overflow',
		  @':size', mkcl_make_unsigned_integer(env, env->cs_size),
		  @':type', @'si::call-stack');
    }
  else
    {
#if 0
      printf("_resetstkoflw() failed! All that is left to do is to sleep until we die...");
      fflush(NULL);
      Sleep(1000000);
#endif
    }
  return EXCEPTION_CONTINUE_SEARCH;
}


static LONG unhandled_exception(EXCEPTION_POINTERS* ep)
{
  printf("\nMKCL: Received an exception we cannot handle!\n");
  fflush(NULL);
  return EXCEPTION_CONTINUE_SEARCH;
}

static LONG handle_illegal_instruction(EXCEPTION_POINTERS* ep)
{
  printf("\nhandle_illegal_instruction is not implemented yet!\n");
  fflush(NULL);
  return EXCEPTION_CONTINUE_SEARCH;
}

static LONG WINAPI W32_exception_filter(EXCEPTION_POINTERS* ep)
{
  LONG excpt_result;

  excpt_result = EXCEPTION_CONTINUE_EXECUTION;
  switch (ep->ExceptionRecord->ExceptionCode)
    {
      /* Catch all arithmetic exceptions */
    case EXCEPTION_INT_DIVIDE_BY_ZERO:
    case EXCEPTION_INT_OVERFLOW:
    case EXCEPTION_FLT_DIVIDE_BY_ZERO:
    case EXCEPTION_FLT_OVERFLOW:
    case EXCEPTION_FLT_UNDERFLOW:
    case EXCEPTION_FLT_INEXACT_RESULT:
    case EXCEPTION_FLT_DENORMAL_OPERAND:
    case EXCEPTION_FLT_INVALID_OPERATION:
    case EXCEPTION_FLT_STACK_CHECK:
      excpt_result = handle_fpe(ep);
      break;
      /* Catch segmentation fault */
    case EXCEPTION_ACCESS_VIOLATION:
      excpt_result = handle_access_violation(ep);
      break;
      /* Catch illegal instruction */
    case EXCEPTION_ILLEGAL_INSTRUCTION:
      excpt_result = handle_illegal_instruction(ep);
      break;
    case EXCEPTION_PRIV_INSTRUCTION:
#if 0
      printf("\nMKCL: Received EXCEPTION_PRIV_INSTRUCTION!\n");
      fflush(NULL);
#endif
      excpt_result = unhandled_exception(ep);
      break;

      /* These are equivalent to a SIGBUS */
    case EXCEPTION_DATATYPE_MISALIGNMENT:
#if 0
      printf("\nMKCL: Received EXCEPTION_DATATYPE_MISALIGNMENT!\n");
      fflush(NULL);
#endif
      excpt_result = unhandled_exception(ep);
      break;
    case EXCEPTION_ARRAY_BOUNDS_EXCEEDED:
#if 0
      printf("\nMKCL: Received EXCEPTION_ARRAY_BOUNDS_EXCEEDED!\n");
      fflush(NULL);
#endif
      excpt_result = unhandled_exception(ep);
      break;
    case EXCEPTION_IN_PAGE_ERROR:
#if 0
      printf("\nMKCL: Received EXCEPTION_IN_PAGE_ERROR!\n");
      fflush(NULL);
#endif
      excpt_result = unhandled_exception(ep);
      break;
    case EXCEPTION_STACK_OVERFLOW:
      excpt_result = handle_stack_overflow(ep);
      break;

      /* Do not catch anything else */
    default:
      printf("\nMKCL: Received an unknown exception, (ExceptionCode = %lu)!\n", ep->ExceptionRecord->ExceptionCode);
      fflush(NULL);
      excpt_result = EXCEPTION_CONTINUE_SEARCH;
      break;
    }

  return excpt_result;
}

#ifdef __MINGW64__
typedef EXCEPTION_DISPOSITION
(* W64UnhandledExceptionFilter)(struct _EXCEPTION_RECORD *, void *,
				struct _CONTEXT *, void *);

static int SetMingW64UnhandledExceptionFilter(W64UnhandledExceptionFilter filter);
/* void register_UEF_WrapperCallbacks(void); */

static EXCEPTION_DISPOSITION
W64_exception_filter (struct _EXCEPTION_RECORD* ExceptionRecord,
		      void *EstablisherFrame  __attribute__ ((unused)),
		      struct _CONTEXT* ContextRecord,
		      void *DispatcherContext __attribute__ ((unused)))
{
  EXCEPTION_POINTERS ep;
  LONG excpt_result;

  ep.ExceptionRecord = ExceptionRecord;
  ep.ContextRecord = ContextRecord;

  excpt_result = W32_exception_filter(&ep);

  if (excpt_result == EXCEPTION_CONTINUE_EXECUTION)
    return ExceptionContinueExecution;
  else
    return ExceptionContinueSearch;
}
#endif /* __MINGW64__ */


static BOOL WINAPI W32_console_ctrl_handler(DWORD type)
{
  console_ctrl_event = TRUE;
  switch (type)
    {
      /* Catch CTRL-C */
    case CTRL_C_EVENT:
      handle_console_ctrl_event(@'si::sigint-handler', SIGINT);
      return TRUE;
    case CTRL_BREAK_EVENT: /* equivalent to SIGTERM or SIGHUP? */
      handle_console_ctrl_event(@'si::sigint-handler', SIGBREAK);
      return TRUE;
    case CTRL_CLOSE_EVENT:
      handle_console_ctrl_event(@'si::sighup-handler', SIGTERM);
      return TRUE;
    case CTRL_LOGOFF_EVENT:
      handle_console_ctrl_event(@'si::sighup-handler', SIGTERM);
      return TRUE;
    case CTRL_SHUTDOWN_EVENT:
      handle_console_ctrl_event(@'si::terminal-signal-handler', SIGTERM);
      return TRUE;
    }
  return FALSE;
}

#endif  /* MKCL_WINDOWS */

#if MKCL_WINDOWS
int mkcl_feenableexcept(int excepts)
{
  unsigned int cw = _controlfp(0,0);

  cw &= ~(excepts);

  int new_cw = _controlfp(cw,_MCW_EM);

  return _MCW_EM & ~new_cw;
}

int mkcl_fedisableexcept(int excepts)
{
  int cw = _controlfp(0,0);

  cw |= (excepts);

  int new_cw = _controlfp(cw,_MCW_EM);

  return _MCW_EM & ~new_cw;
}

int mkcl_feholdexcept(int * fenv)
{
  int cw = _controlfp(0,0);

  *fenv = cw;

  int new_cw = _controlfp(0xffffffff, _MCW_EM); 

  return _MCW_EM & ~new_cw;
}

int mkcl_fegetexcept(void)
{
  int cw = _controlfp(0,0);

  int excepts = _MCW_EM & ~ cw;

  return excepts;
}

int mkcl_fesetenv(int * fenv)
{
  int new_cw = _controlfp(*fenv, _MCW_EM);

  return 0;
}

int mkcl_fetestexcept(int excepts)
{
  int sw = _statusfp();

  excepts &= sw;

  return excepts;
}

int mkcl_feclearexcept(int excepts)
{
  _clearfp();
  return 0;
}
#endif /* MKCL_WINDOWS */


mkcl_object
mk_si_initial_floating_point_exception_set(MKCL)
{
  mkcl_object fpe_set = mk_cl_Cnil;

  mkcl_call_stack_check(env);
  fpe_set = MKCL_CONS(env, @'division-by-zero', fpe_set);
  fpe_set = MKCL_CONS(env, @'floating-point-overflow', fpe_set);
  fpe_set = MKCL_CONS(env, @'floating-point-underflow', fpe_set);
  fpe_set = MKCL_CONS(env, @'floating-point-invalid-operation', fpe_set);
#if 0
  fpe_set = MKCL_CONS(env, @'floating-point-inexact', fpe_set);
#endif
  
  @(return fpe_set);
}

static int default_fpe_mask(MKCL)
{
  mkcl_object default_set = MKCL_SYM_VAL(env, @'si::*default-floating-point-exception-set*');
  mkcl_object exception;
  int mask = 0;

  mkcl_loop_for_in(env, default_set) {
    exception = MKCL_CONS_CAR(default_set);

    if (exception == @'division-by-zero')
      mask |= FE_DIVBYZERO;
    else if (exception == @'floating-point-overflow')
      mask |= FE_OVERFLOW;
    else if (exception == @'floating-point-underflow')
      mask |= FE_UNDERFLOW;
    else if (exception == @'floating-point-invalid-operation')
      mask |= FE_INVALID;
    else if (exception == @'floating-point-inexact')
      mask |= FE_INEXACT;
    else
      mkcl_FEerror(env, "Unknown floating-point exception: ~S.", 1, exception);
  } mkcl_end_loop_for_in;
    
  return mask;
}


mkcl_object
mk_si_disable_fpe(MKCL, mkcl_object exception)
{
  int bits = 0;

  mkcl_call_stack_check(env);
  if (exception == @'division-by-zero')
    bits = FE_DIVBYZERO;
  else if (exception == @'floating-point-overflow')
    bits = FE_OVERFLOW;
  else if (exception == @'floating-point-underflow')
    bits = FE_UNDERFLOW;
  else if (exception == @'floating-point-invalid-operation')
    bits = FE_INVALID;
  else if (exception == @'floating-point-inexact')
    bits = FE_INEXACT;
  else if (exception == @':default')
    bits = default_fpe_mask(env);
  else if (exception == mk_cl_Ct)
    bits = FE_ALL_EXCEPT;
  else if (mkcl_Null(exception))
    bits = 0;
  else
    mkcl_FEerror(env, "Unknown floating-point exception: ~S.", 1, exception);

  fedisableexcept(bits);

  env->fpe_control_bits &= ~bits;

  @(return mk_cl_Cnil);
}

mkcl_object
mk_si_enable_fpe(MKCL, mkcl_object exception)
{
  int bits = 0;

  mkcl_call_stack_check(env);
  if (exception == @'division-by-zero')
    bits = FE_DIVBYZERO;
  else if (exception == @'floating-point-overflow')
    bits = FE_OVERFLOW;
  else if (exception == @'floating-point-underflow')
    bits = FE_UNDERFLOW;
  else if (exception == @'floating-point-invalid-operation')
    bits = FE_INVALID;
  else if (exception == @'floating-point-inexact')
    bits = FE_INEXACT;
  else if (exception == @':default')
    bits = default_fpe_mask(env);
  else if (exception == mk_cl_Ct)
    bits = FE_ALL_EXCEPT;
  else if (mkcl_Null(exception))
    bits = 0;
  else
    mkcl_FEerror(env, "Unknown floating-point exception: ~S.", 1, exception);

  feclearexcept(bits); /* We clear them because we have no idea where they came from. */
  feenableexcept(bits);
  
  env->fpe_control_bits |= bits;

  @(return mk_cl_Cnil);
}

void mkcl_reactivate_fpe_set(MKCL)
{
  feenableexcept(env->fpe_control_bits);
}

mkcl_object
mk_si_all_enabled_fpe(MKCL)
{
  mkcl_call_stack_check(env);
  mkcl_object fpe_set = mk_cl_Cnil;
  int enabled_except = fegetexcept();

  if (enabled_except & FE_DIVBYZERO)
    fpe_set = MKCL_CONS(env, @'division-by-zero', fpe_set);
  if (enabled_except & FE_OVERFLOW)
    fpe_set = MKCL_CONS(env, @'floating-point-overflow', fpe_set);
  if (enabled_except & FE_UNDERFLOW)
    fpe_set = MKCL_CONS(env, @'floating-point-underflow', fpe_set);
  if (enabled_except & FE_INVALID)
    fpe_set = MKCL_CONS(env, @'floating-point-invalid-operation', fpe_set);
  if (enabled_except & FE_INEXACT)
    fpe_set = MKCL_CONS(env, @'floating-point-inexact', fpe_set);

  @(return fpe_set);
}

mkcl_object
mk_si_fpe_enabled_p(MKCL, mkcl_object exception)
{
  mkcl_call_stack_check(env);
  int bits = 0;
  int enabled_except = fegetexcept();

  if (exception == @'division-by-zero')
    bits = FE_DIVBYZERO;
  else if (exception == @'floating-point-overflow')
    bits = FE_OVERFLOW;
  else if (exception == @'floating-point-underflow')
    bits = FE_UNDERFLOW;
  else if (exception == @'floating-point-invalid-operation')
    bits = FE_INVALID;
  else if (exception == @'floating-point-inexact')
    bits = FE_INEXACT;
  else
    mkcl_FEerror(env, "Unknown floating-point exception: ~S.", 1, exception);

  @(return ((enabled_except & bits) ? mk_cl_Ct : mk_cl_Cnil));
}

mkcl_object
mk_si_all_raised_fpe(MKCL)
{
  mkcl_call_stack_check(env);
  int raised_except = fetestexcept(FE_ALL_EXCEPT);
  mkcl_object fpe_set = mk_cl_Cnil;

  if (raised_except & FE_DIVBYZERO)
    fpe_set = MKCL_CONS(env, @'division-by-zero', fpe_set);
  if (raised_except & FE_OVERFLOW)
    fpe_set = MKCL_CONS(env, @'floating-point-overflow', fpe_set);
  if (raised_except & FE_UNDERFLOW)
    fpe_set = MKCL_CONS(env, @'floating-point-underflow', fpe_set);
  if (raised_except & FE_INVALID)
    fpe_set = MKCL_CONS(env, @'floating-point-invalid-operation', fpe_set);
  if (raised_except & FE_INEXACT)
    fpe_set = MKCL_CONS(env, @'floating-point-inexact', fpe_set);

  @(return fpe_set);
}

mkcl_object
mk_si_fpe_raised_p(MKCL, mkcl_object exception)
{
  mkcl_call_stack_check(env);
  int raised_except = fetestexcept(FE_ALL_EXCEPT);
  int bits = 0;

  if (exception == @'division-by-zero')
    bits = FE_DIVBYZERO;
  else if (exception == @'floating-point-overflow')
    bits = FE_OVERFLOW;
  else if (exception == @'floating-point-underflow')
    bits = FE_UNDERFLOW;
  else if (exception == @'floating-point-invalid-operation')
    bits = FE_INVALID;
  else if (exception == @'floating-point-inexact')
    bits = FE_INEXACT;
  else
    mkcl_FEerror(env, "Unknown floating-point exception: ~S.", 1, exception);

  @(return ((raised_except & bits) ? mk_cl_Ct : mk_cl_Cnil));
}

mkcl_object
mk_si_raise_fpe(MKCL, mkcl_object exception)
{
  int bits = 0;

  mkcl_call_stack_check(env);
  if (exception == @'division-by-zero')
    bits = FE_DIVBYZERO;
  else if (exception == @'floating-point-overflow')
    bits = FE_OVERFLOW;
  else if (exception == @'floating-point-underflow')
    bits = FE_UNDERFLOW;
  else if (exception == @'floating-point-invalid-operation')
    bits = FE_INVALID;
  else if (exception == @'floating-point-inexact')
    bits = FE_INEXACT;
  else
    mkcl_FEerror(env, "Unknown floating-point exception: ~S.", 1, exception);

  feraiseexcept(bits);

  @(return mk_cl_Cnil);
}

void mkcl_clear_fpe(MKCL, int except)
{
  feclearexcept(FE_ALL_EXCEPT & except);
}

mkcl_object
mk_si_clear_fpe(MKCL, mkcl_object exception)
{
  int bits = 0;

  mkcl_call_stack_check(env);
  if (exception == @'division-by-zero')
    bits = FE_DIVBYZERO;
  else if (exception == @'floating-point-overflow')
    bits = FE_OVERFLOW;
  else if (exception == @'floating-point-underflow')
    bits = FE_UNDERFLOW;
  else if (exception == @'floating-point-invalid-operation')
    bits = FE_INVALID;
  else if (exception == @'floating-point-inexact')
    bits = FE_INEXACT;
  else if (exception == @':default')
    bits = default_fpe_mask(env);
  else if (exception == mk_cl_Ct)
    bits = FE_ALL_EXCEPT;
  else if (mkcl_Null(exception))
    bits = 0;
  else
    mkcl_FEerror(env, "Unknown floating-point exception: ~S.", 1, exception);

  feclearexcept(bits);

  @(return mk_cl_Cnil);
}

mkcl_object
mk_si_clear_all_fpe(MKCL)
{
  mkcl_call_stack_check(env);
  feclearexcept(FE_ALL_EXCEPT);
  @(return mk_cl_Cnil);
}

#if MKCL_WINDOWS
static VOID CALLBACK dummy_apc_func(ULONG_PTR dwParam)
{
  /* This function is deliberately empty. */
}
#endif

mkcl_object
mk_mt_try_to_wake_up_thread(MKCL, mkcl_object thread)
{
  int rc, i;
  mkcl_object sleeping_on;

  mkcl_call_stack_check(env);
  if (mkcl_type_of(thread) != mkcl_t_thread)
    mkcl_FEwrong_type_argument(env, @'mt::thread', thread);

  if (thread->thread.status != mkcl_thread_active)
    { @(return mk_cl_Cnil); } /* There is no point in trying to wake up something that cannot be made to run. */

  sleeping_on = thread->thread.env->sleeping_on;
  if (mkcl_Null(sleeping_on))
    {
#if 0
      fprintf(stderr, "\n;; MKCL: Tried to wake up [%s] on NIL!\n", thread->thread.name->base_string.self);
      fflush(stderr);
#endif
      @(return mk_cl_Cnil);
    }
  else if (sleeping_on == @':io')
    {
#if 0
      fprintf(stderr, "\n;; MKCL: Tried to wake up [%s] on I/O!\n", thread->thread.name->base_string.self);
      fflush(stderr);
#endif
#if MKCL_WINDOWS
      if (thread->thread.thread)
	{
	  DWORD ok;
	  
	  MKCL_LIBC_NO_INTR(env, ok = QueueUserAPC(dummy_apc_func, thread->thread.thread, (ULONG_PTR) NULL));
	  if (!ok)
	    {
	      if (ERROR_INVALID_HANDLE == GetLastError())
		thread->thread.status = mkcl_thread_done; /* Let's prononce it dead. */
	      
	      @(return mk_cl_Cnil);
	    }
	}
      else
	{ @(return mk_cl_Cnil); }
#else
      for (i = 0; i < 3; i++) /* We knock 3 times because our wake-up call may be received before the sleep. */
	{
	  MKCL_LIBC_NO_INTR(env, (rc = pthread_kill(thread->thread.thread, wake_up_sig)));
	  if (rc)
	    {
	      if (rc == ESRCH)
		thread->thread.status = mkcl_thread_done; /* Let's prononce it dead. */

	      @(return mk_cl_Cnil); /* We muffle any error because this was just a try. */
	    }
	  pthread_yield();
	}
#endif
      @(return mk_cl_Ct);
    }
  else
    {
      mkcl_type sleep_type = mkcl_type_of(sleeping_on);

      switch (sleep_type)
	{
	case mkcl_t_condition_variable:
	  /* We deliberately shoot too wide but this should be harmless. */
#if 0
	  fprintf(stderr, "\n;; MKCL: Tried to wake up [%s] on a condition variable!\n", thread->thread.name->base_string.self);
	  fflush(stderr);
#endif
	  mk_mt_condition_broadcast(env, sleeping_on);
	  @(return mk_cl_Ct);
	  break;
	case mkcl_t_lock:
	case mkcl_t_rwlock: /* We're out of luck on these, thanks to POSIX. */
#if 0
	  fprintf(stderr, "\n;; MKCL: Tried to wake up [%s] on a lock!\n", thread->thread.name->base_string.self);
	  fflush(stderr);
#endif
	  @(return mk_cl_Cnil);
	  break;
	case mkcl_t_semaphore:
	  /* Messing with a semaphore count is a dangerous propostion at best. Caveat emptor. */
	  /* Sleeper must be able to handle graciously this kind of forced wake up. */
#if 0
	  fprintf(stderr, "\n;; MKCL: Tried to wake up [%s] on a semaphore!\n", thread->thread.name->base_string.self);
	  fflush(stderr);
#endif
	  mkcl_funcall1(env, @+'mt::semaphore-signal', sleeping_on);
	  @(return mk_cl_Ct);
	  break;
	case mkcl_t_cons: /* a hack. just in case. JCB */
	  {
	    void (*f)(void *) = (void (*)(void *)) MKCL_CONS_CAR(sleeping_on);
	    void * data = MKCL_CONS_CDR(sleeping_on);

#if 0
	    fprintf(stderr, "\n;; MKCL: Tried to wake up [%s] on a cons!\n", thread->thread.name->base_string.self);
	    fflush(stderr);
#endif
	    f(data);
	    @(return mk_cl_Ct);
	  }
	  break;
	case mkcl_t_cfun: case mkcl_t_cclosure:
	case mkcl_t_bytecode: case mkcl_t_bclosure:
#if 0
	  fprintf(stderr, "\n;; MKCL: Tried to wake up [%s] on a lisp function!\n", thread->thread.name->base_string.self);
	  fflush(stderr);
#endif
	  mkcl_funcall0(env, sleeping_on);
	  @(return mk_cl_Ct);
	  break;
	default:
#if 0
	  fprintf(stderr, "\n;; MKCL: Tried to wake up [%s] on an UNKNOWN objet!\n", thread->thread.name->base_string.self);
	  fflush(stderr);
#endif
	  @(return mk_cl_Cnil);
	}
    }
}


#if MKCL_UNIX

static void * signal_servicing_loop(void * arg)
{
  pthread_detach(pthread_self());
  for (;;) /* sleep forever! */
    pause();
  return NULL;
}

static mkcl_os_thread_t signal_servicing_thread;


struct mkcl_signal_disposition
{
  mkcl_sighandler_t c_handler; /* NULL or an apropriate C function pointer. */
  mkcl_object lisp_handler; /* nil means C handler decides everything.
                               T means a lisp handler specific to the C handler will be called.
			       A symbol names the lisp handler function to be called. 
			    */
};


# if __linux
static const struct mkcl_signal_disposition c_signal_disposition[MKCL_SIGMAX + 1] = {
  /* Linux signal ordering */
  /*  0 SIG0 */      { NULL, mk_cl_Cnil }, /* does not exist. */
  /*  1 SIGHUP */    { mkcl_generic_signal_handler, @'si::sighup-handler' },
  /*  2 SIGINT */    { mkcl_generic_signal_handler,  @'si::sigint-handler' },
  /*  3 SIGQUIT */   { mkcl_terminal_signal_handler, mk_cl_Ct },
  /*  4 SIGILL */    { mkcl_synchronous_signal_handler, mk_cl_Cnil },
  /*  5 SIGTRAP */   { mkcl_terminal_signal_handler, mk_cl_Ct },
  /*  6 SIGABRT */   { mkcl_terminal_signal_handler, mk_cl_Ct },
  /*  7 SIGBUS */    { mkcl_sigbus_handler, mk_cl_Cnil },
  /*  8 SIGFPE */    { mkcl_sigfpe_handler, mk_cl_Cnil },
  /*  9 SIGKILL */   { NULL, mk_cl_Cnil }, /* cannot be handled! */
  /* 10 SIGUSR1 */   { mkcl_terminal_signal_handler, mk_cl_Ct },
  /* 11 SIGSEGV */   { mkcl_sigsegv_handler, mk_cl_Cnil },
  /* 12 SIGUSR2 */   { mkcl_terminal_signal_handler, mk_cl_Ct },
  /* 13 SIGPIPE */   { mkcl_sigpipe_handler, mk_cl_Cnil },
  /* 14 SIGALRM */   { mkcl_terminal_signal_handler, mk_cl_Ct },
  /* 15 SIGTERM */   { mkcl_generic_signal_handler, @'si::sigterm-handler' },
  /* 16 SIGSTKFLT */ { mkcl_terminal_signal_handler, mk_cl_Ct },
  /* 17 SIGCHLD */   { mkcl_sigchld_handler, mk_cl_Cnil },
  /* 18 SIGCONT */   { NULL, mk_cl_Cnil }, /* continue by default */
  /* 19 SIGSTOP */   { NULL, mk_cl_Cnil }, /* stop by default */
  /* 20 SIGTSTP */   { NULL, mk_cl_Cnil }, /* stop by default */
  /* 21 SIGTTIN */   { NULL, mk_cl_Cnil }, /* stop by default */
  /* 22 SIGTTOU */   { NULL, mk_cl_Cnil }, /* stop by default */
  /* 23 SIGURG */    { NULL, mk_cl_Cnil }, /* ignored by default */
  /* 24 SIGXCPU */   { mkcl_terminal_signal_handler, mk_cl_Ct },
  /* 25 SIGXFSZ */   { mkcl_terminal_signal_handler, mk_cl_Ct },
  /* 26 SIGVTALRM */ { mkcl_terminal_signal_handler, mk_cl_Ct },
  /* 27 SIGPROF */   { NULL, mk_cl_Cnil }, /* used by gprof profiling. */
  /* 28 SIGWINCH */  { NULL, mk_cl_Cnil }, /* ignored by default */
  /* 29 SIGIO */     { mkcl_terminal_signal_handler, mk_cl_Ct },
  /* 30 SIGPWR */    { mkcl_terminal_signal_handler, mk_cl_Ct },
  /* 31 SIGSYS */    { mkcl_terminal_signal_handler, mk_cl_Ct },
  /* SIG32 */        { NULL, mk_cl_Cnil }, /* reserved by linux */
  /* SIG33 */        { NULL, mk_cl_Cnil }, /* reserved by linux */
  /* SIG34 */        { mkcl_terminal_signal_handler, mk_cl_Ct }, /* LinuxThreads: reserved */
  /* SIG35 */        { mkcl_terminal_signal_handler, mk_cl_Ct }, /* (default) resume */
  /* SIG36 */        { mkcl_terminal_signal_handler, mk_cl_Ct }, /* (default) interrupt */
  /* SIG37 */        { mkcl_terminal_signal_handler, mk_cl_Ct }, /* (default) wake-up */
  /* SIG38 */        { mkcl_terminal_signal_handler, mk_cl_Ct }, /* used by Boehm's GC */
  /* SIG39 */        { mkcl_terminal_signal_handler, mk_cl_Ct }, /* used by Boehm's GC */
  /* SIG40 */        { mkcl_terminal_signal_handler, mk_cl_Ct },
  /* SIG41 */        { mkcl_terminal_signal_handler, mk_cl_Ct },
  /* SIG42 */        { mkcl_terminal_signal_handler, mk_cl_Ct },
  /* SIG43 */        { mkcl_terminal_signal_handler, mk_cl_Ct },
  /* SIG44 */        { mkcl_terminal_signal_handler, mk_cl_Ct },
  /* SIG45 */        { mkcl_terminal_signal_handler, mk_cl_Ct },
  /* SIG46 */        { mkcl_terminal_signal_handler, mk_cl_Ct },
  /* SIG47 */        { mkcl_terminal_signal_handler, mk_cl_Ct },
  /* SIG48 */        { mkcl_terminal_signal_handler, mk_cl_Ct },
  /* SIG49 */        { mkcl_terminal_signal_handler, mk_cl_Ct },
  /* SIG50 */        { mkcl_terminal_signal_handler, mk_cl_Ct },
  /* SIG51 */        { mkcl_terminal_signal_handler, mk_cl_Ct },
  /* SIG52 */        { mkcl_terminal_signal_handler, mk_cl_Ct },
  /* SIG53 */        { mkcl_terminal_signal_handler, mk_cl_Ct },
  /* SIG54 */        { mkcl_terminal_signal_handler, mk_cl_Ct },
  /* SIG55 */        { mkcl_terminal_signal_handler, mk_cl_Ct },
  /* SIG56 */        { mkcl_terminal_signal_handler, mk_cl_Ct },
  /* SIG57 */        { mkcl_terminal_signal_handler, mk_cl_Ct },
  /* SIG58 */        { mkcl_terminal_signal_handler, mk_cl_Ct },
  /* SIG59 */        { mkcl_terminal_signal_handler, mk_cl_Ct },
  /* SIG60 */        { mkcl_terminal_signal_handler, mk_cl_Ct },
  /* SIG61 */        { mkcl_terminal_signal_handler, mk_cl_Ct },
  /* SIG62 */        { mkcl_terminal_signal_handler, mk_cl_Ct },
  /* SIG63 */        { mkcl_terminal_signal_handler, mk_cl_Ct },
  /* SIG64 */        { mkcl_terminal_signal_handler, mk_cl_Ct }
};

# elif __FreeBSD__

static const struct mkcl_signal_disposition c_signal_disposition[MKCL_SIGMAX + 1] = {
  /* FreeBSD signal ordering */
  /*  0 SIG0 */      { NULL, mk_cl_Cnil }, /* does not exist. */
  /*  1 SIGHUP */    { mkcl_generic_signal_handler, @'si::sighup-handler' },
  /*  2 SIGINT */    { mkcl_generic_signal_handler,  @'si::sigint-handler' },
  /*  3 SIGQUIT */   { mkcl_terminal_signal_handler, mk_cl_Ct },
  /*  4 SIGILL */    { mkcl_synchronous_signal_handler, mk_cl_Cnil },
  /*  5 SIGTRAP */   { mkcl_terminal_signal_handler, mk_cl_Ct },
  /*  6 SIGABRT */   { mkcl_terminal_signal_handler, mk_cl_Ct },
  /*  7 SIGEMT */    { mkcl_terminal_signal_handler, mk_cl_Ct },
  /*  8 SIGFPE */    { mkcl_sigfpe_handler, mk_cl_Cnil },
  /*  9 SIGKILL */   { NULL, mk_cl_Cnil }, /* cannot be handled! */
  /* 10 SIGBUS */    { mkcl_sigbus_handler, mk_cl_Cnil },
  /* 11 SIGSEGV */   { mkcl_sigsegv_handler, mk_cl_Cnil },
  /* 12 SIGSYS */    { mkcl_terminal_signal_handler, mk_cl_Ct },
  /* 13 SIGPIPE */   { mkcl_sigpipe_handler, mk_cl_Cnil },
  /* 14 SIGALRM */   { mkcl_terminal_signal_handler, mk_cl_Ct },
  /* 15 SIGTERM */   { mkcl_generic_signal_handler, @'si::sigterm-handler' },
  /* 16 SIGURG */    { NULL, mk_cl_Cnil }, /* ignored by default */
  /* 17 SIGSTOP */   { NULL, mk_cl_Cnil }, /* cannot be redefined! */
  /* 18 SIGTSTP */   { NULL, mk_cl_Cnil }, /* stop by default */
  /* 19 SIGCONT */   { NULL, mk_cl_Cnil }, /* continue by default */
  /* 20 SIGCHLD */   { mkcl_sigchld_handler, mk_cl_Cnil },
  /* 21 SIGTTIN */   { NULL, mk_cl_Cnil }, /* stop by default */
  /* 22 SIGTTOU */   { NULL, mk_cl_Cnil }, /* stop by default */
  /* 23 SIGIO */     { NULL, mk_cl_Cnil }, /* ignored by default */
  /* 24 SIGXCPU */   { mkcl_terminal_signal_handler, mk_cl_Ct },
  /* 25 SIGXFSZ */   { mkcl_terminal_signal_handler, mk_cl_Ct },
  /* 26 SIGVTALRM */ { mkcl_terminal_signal_handler, mk_cl_Ct },
  /* 27 SIGPROF */   { mkcl_terminal_signal_handler, mk_cl_Ct }, /* used by gprof profiling. */
  /* 28 SIGWINCH */  { NULL, mk_cl_Cnil }, /* ignored by default */
  /* 29 SIGINFO */   { NULL, mk_cl_Cnil }, /* ignored by default */
  /* 30 SIGUSR1 */   { mkcl_terminal_signal_handler, mk_cl_Ct },
  /* 31 SIGUSR2 */   { mkcl_terminal_signal_handler, mk_cl_Ct },

  /* 32 SIGTHR */    { NULL, mk_cl_Cnil }, /* reserved by FreeBSD, terminal? */
  /* 33 SIGLIBRT */  { NULL, mk_cl_Cnil }, /* reserved by FreeBSD, terminal? */

  /* SIG34 */        { mkcl_terminal_signal_handler, mk_cl_Ct },
  /* SIG35 */        { mkcl_terminal_signal_handler, mk_cl_Ct },
  /* SIG36 */        { mkcl_terminal_signal_handler, mk_cl_Ct },
  /* SIG37 */        { mkcl_terminal_signal_handler, mk_cl_Ct },
  /* SIG38 */        { mkcl_terminal_signal_handler, mk_cl_Ct },
  /* SIG39 */        { mkcl_terminal_signal_handler, mk_cl_Ct },
  /* SIG40 */        { mkcl_terminal_signal_handler, mk_cl_Ct },
  /* SIG41 */        { mkcl_terminal_signal_handler, mk_cl_Ct },
  /* SIG42 */        { mkcl_terminal_signal_handler, mk_cl_Ct },
  /* SIG43 */        { mkcl_terminal_signal_handler, mk_cl_Ct },
  /* SIG44 */        { mkcl_terminal_signal_handler, mk_cl_Ct },
  /* SIG45 */        { mkcl_terminal_signal_handler, mk_cl_Ct },
  /* SIG46 */        { mkcl_terminal_signal_handler, mk_cl_Ct },
  /* SIG47 */        { mkcl_terminal_signal_handler, mk_cl_Ct },
  /* SIG48 */        { mkcl_terminal_signal_handler, mk_cl_Ct },
  /* SIG49 */        { mkcl_terminal_signal_handler, mk_cl_Ct },
  /* SIG50 */        { mkcl_terminal_signal_handler, mk_cl_Ct },
  /* SIG51 */        { mkcl_terminal_signal_handler, mk_cl_Ct },
  /* SIG52 */        { mkcl_terminal_signal_handler, mk_cl_Ct },
  /* SIG53 */        { mkcl_terminal_signal_handler, mk_cl_Ct },
  /* SIG54 */        { mkcl_terminal_signal_handler, mk_cl_Ct },
  /* SIG55 */        { mkcl_terminal_signal_handler, mk_cl_Ct },
  /* SIG56 */        { mkcl_terminal_signal_handler, mk_cl_Ct },
  /* SIG57 */        { mkcl_terminal_signal_handler, mk_cl_Ct },
  /* SIG58 */        { mkcl_terminal_signal_handler, mk_cl_Ct },
  /* SIG59 */        { mkcl_terminal_signal_handler, mk_cl_Ct },
  /* SIG60 */        { mkcl_terminal_signal_handler, mk_cl_Ct },
  /* SIG61 */        { mkcl_terminal_signal_handler, mk_cl_Ct },
  /* SIG62 */        { mkcl_terminal_signal_handler, mk_cl_Ct },
  /* SIG63 */        { mkcl_terminal_signal_handler, mk_cl_Ct },
  /* SIG64 */        { mkcl_terminal_signal_handler, mk_cl_Ct },

  /* SIG65 */        { mkcl_terminal_signal_handler, mk_cl_Ct },
  /* SIG66 */        { mkcl_terminal_signal_handler, mk_cl_Ct }, /* (default) resume */
  /* SIG67 */        { mkcl_terminal_signal_handler, mk_cl_Ct }, /* (default) interrupt */
  /* SIG68 */        { mkcl_terminal_signal_handler, mk_cl_Ct }, /* (default) wake-up */
  /* SIG69 */        { mkcl_terminal_signal_handler, mk_cl_Ct }, /* used by Boehm's GC */
  /* SIG70 */        { mkcl_terminal_signal_handler, mk_cl_Ct }, /* used by Boehm's GC */
  /* SIG71 */        { mkcl_terminal_signal_handler, mk_cl_Ct },
  /* SIG72 */        { mkcl_terminal_signal_handler, mk_cl_Ct },
  /* SIG73 */        { mkcl_terminal_signal_handler, mk_cl_Ct },
  /* SIG74 */        { mkcl_terminal_signal_handler, mk_cl_Ct },
  /* SIG75 */        { mkcl_terminal_signal_handler, mk_cl_Ct },
  /* SIG76 */        { mkcl_terminal_signal_handler, mk_cl_Ct },
  /* SIG77 */        { mkcl_terminal_signal_handler, mk_cl_Ct },
  /* SIG78 */        { mkcl_terminal_signal_handler, mk_cl_Ct },
  /* SIG79 */        { mkcl_terminal_signal_handler, mk_cl_Ct },
  /* SIG80 */        { mkcl_terminal_signal_handler, mk_cl_Ct },
  /* SIG81 */        { mkcl_terminal_signal_handler, mk_cl_Ct },
  /* SIG82 */        { mkcl_terminal_signal_handler, mk_cl_Ct },
  /* SIG83 */        { mkcl_terminal_signal_handler, mk_cl_Ct },
  /* SIG84 */        { mkcl_terminal_signal_handler, mk_cl_Ct },
  /* SIG85 */        { mkcl_terminal_signal_handler, mk_cl_Ct },
  /* SIG86 */        { mkcl_terminal_signal_handler, mk_cl_Ct },
  /* SIG87 */        { mkcl_terminal_signal_handler, mk_cl_Ct },
  /* SIG88 */        { mkcl_terminal_signal_handler, mk_cl_Ct },
  /* SIG89 */        { mkcl_terminal_signal_handler, mk_cl_Ct },
  /* SIG90 */        { mkcl_terminal_signal_handler, mk_cl_Ct },
  /* SIG91 */        { mkcl_terminal_signal_handler, mk_cl_Ct },
  /* SIG92 */        { mkcl_terminal_signal_handler, mk_cl_Ct },
  /* SIG93 */        { mkcl_terminal_signal_handler, mk_cl_Ct },
  /* SIG94 */        { mkcl_terminal_signal_handler, mk_cl_Ct },
  /* SIG95 */        { mkcl_terminal_signal_handler, mk_cl_Ct },
  /* SIG96 */        { mkcl_terminal_signal_handler, mk_cl_Ct },
  /* SIG97 */        { mkcl_terminal_signal_handler, mk_cl_Ct },
  /* SIG98 */        { mkcl_terminal_signal_handler, mk_cl_Ct },
  /* SIG99 */        { mkcl_terminal_signal_handler, mk_cl_Ct },
  /* SIG100 */       { mkcl_terminal_signal_handler, mk_cl_Ct },
  /* SIG101 */       { mkcl_terminal_signal_handler, mk_cl_Ct },
  /* SIG102 */       { mkcl_terminal_signal_handler, mk_cl_Ct },
  /* SIG103 */       { mkcl_terminal_signal_handler, mk_cl_Ct },
  /* SIG104 */       { mkcl_terminal_signal_handler, mk_cl_Ct },
  /* SIG105 */       { mkcl_terminal_signal_handler, mk_cl_Ct },
  /* SIG106 */       { mkcl_terminal_signal_handler, mk_cl_Ct },
  /* SIG107 */       { mkcl_terminal_signal_handler, mk_cl_Ct },
  /* SIG108 */       { mkcl_terminal_signal_handler, mk_cl_Ct },
  /* SIG109 */       { mkcl_terminal_signal_handler, mk_cl_Ct },
  /* SIG110 */       { mkcl_terminal_signal_handler, mk_cl_Ct },
  /* SIG111 */       { mkcl_terminal_signal_handler, mk_cl_Ct },
  /* SIG112 */       { mkcl_terminal_signal_handler, mk_cl_Ct },
  /* SIG113 */       { mkcl_terminal_signal_handler, mk_cl_Ct },
  /* SIG114 */       { mkcl_terminal_signal_handler, mk_cl_Ct },
  /* SIG115 */       { mkcl_terminal_signal_handler, mk_cl_Ct },
  /* SIG116 */       { mkcl_terminal_signal_handler, mk_cl_Ct },
  /* SIG117 */       { mkcl_terminal_signal_handler, mk_cl_Ct },
  /* SIG118 */       { mkcl_terminal_signal_handler, mk_cl_Ct },
  /* SIG119 */       { mkcl_terminal_signal_handler, mk_cl_Ct },
  /* SIG120 */       { mkcl_terminal_signal_handler, mk_cl_Ct },
  /* SIG121 */       { mkcl_terminal_signal_handler, mk_cl_Ct },
  /* SIG122 */       { mkcl_terminal_signal_handler, mk_cl_Ct },
  /* SIG123 */       { mkcl_terminal_signal_handler, mk_cl_Ct },
  /* SIG124 */       { mkcl_terminal_signal_handler, mk_cl_Ct },
  /* SIG125 */       { mkcl_terminal_signal_handler, mk_cl_Ct },
  /* SIG126 */       { mkcl_terminal_signal_handler, mk_cl_Ct },
  /* SIG127 */       { mkcl_terminal_signal_handler, mk_cl_Ct },
  /* SIG128 */       { mkcl_terminal_signal_handler, mk_cl_Ct }
};
# endif /* __FreeBSD__ */

#endif /* MKCL_UNIX */

void mkcl_init_early_unixint(MKCL)
{
#if MKCL_PTHREADS
  int i;
  stderr_fd = fileno(stderr);

  mkcl_pid = getpid();

  /* Create a forever sleeping thread to ensure async-signals handlers will always find
     at least one thread stack to run on. */
  if (pthread_create(&signal_servicing_thread, NULL, signal_servicing_loop, NULL))
    mkcl_lose(env, "mkcl_init_unixint failed on pthread_create.");

# define DEFAULT_THREAD_RESUME_SIGNAL SIGRTMIN + 1
# define DEFAULT_THREAD_INTERRUPT_SIGNAL SIGRTMIN + 2
# define DEFAULT_THREAD_WAKE_UP_SIGNAL SIGRTMIN + 3


  wake_up_sig = mkcl_get_option(MKCL_OPT_THREAD_WAKE_UP_SIGNAL);
  if (wake_up_sig == 0) {
    wake_up_sig = DEFAULT_THREAD_WAKE_UP_SIGNAL;
    mkcl_set_option(MKCL_OPT_THREAD_WAKE_UP_SIGNAL, wake_up_sig);
  }
  resume_sig = mkcl_get_option(MKCL_OPT_THREAD_RESUME_SIGNAL);
  if (resume_sig == 0) {
    resume_sig = DEFAULT_THREAD_RESUME_SIGNAL;
    mkcl_set_option(MKCL_OPT_THREAD_RESUME_SIGNAL, resume_sig);
  }
  interrupt_sig = mkcl_get_option(MKCL_OPT_THREAD_INTERRUPT_SIGNAL);
  if (interrupt_sig == 0) {
    interrupt_sig = DEFAULT_THREAD_INTERRUPT_SIGNAL;
    mkcl_set_option(MKCL_OPT_THREAD_INTERRUPT_SIGNAL, interrupt_sig);
  }

  if (mkcl_get_option(MKCL_OPT_CHAIN_SIGSEGV)) {
    mkcl_signals[SIGSEGV].chainable = TRUE;
  }
  if (mkcl_get_option(MKCL_OPT_CHAIN_SIGBUS)) {
    mkcl_signals[SIGBUS].chainable = TRUE;
  }
  if (mkcl_get_option(MKCL_OPT_CHAIN_SIGINT)) {
    mkcl_signals[SIGINT].chainable = TRUE;
  }
  if (mkcl_get_option(MKCL_OPT_CHAIN_SIGFPE)) {
    mkcl_signals[SIGFPE].chainable = TRUE;
  }

  for (i = 0; i <= MKCL_SIGMAX; i++)
    {
      mkcl_signals[i].sem = &mkcl_signals[i].sem_obj;
      if (sem_init(mkcl_signals[i].sem, 0, 0))
	mkcl_C_lose(env, "mkcl_init_early_unixint failed on sem_init.");
    }
#elif MKCL_WINDOWS
#endif /* MKCL_UNIX */
}

void mkcl_init_late_unixint(MKCL)
{
#if MKCL_UNIX
#if MKCL_GC_7_2d
  int gc_thread_suspend_sig = MK_GC_suspend_signal();
  int gc_thread_restart_sig = MK_GC_thread_restart_signal();
#else
  int gc_thread_suspend_sig = MK_GC_get_suspend_signal();
  int gc_thread_restart_sig = MK_GC_get_thr_restart_signal();
#endif
  int i;

  install_lisp_terminal_signal_handler(env);

  install_wake_up_signal_handler(env, wake_up_sig);
#if 0
  c_signal_disposition[wake_up_sig].c_handler = NULL;
  c_signal_disposition[wake_up_sig].lisp_handler = mk_cl_Cnil;
#endif

  install_resume_signal_handler(env, resume_sig);
#if 0
  c_signal_disposition[resume_sig].c_handler = NULL;
  c_signal_disposition[resume_sig].lisp_handler = mk_cl_Cnil;
#endif

  install_interrupt_signal_handler(env, interrupt_sig);
#if 0
  c_signal_disposition[interrupt_sig].c_handler = NULL;
  c_signal_disposition[interrupt_sig].lisp_handler = mk_cl_Cnil;
#endif
  
  for (i = 1; i <= MKCL_SIGMAX; i++)
    if (!(i == wake_up_sig || i == resume_sig || i == interrupt_sig
	  || i == gc_thread_suspend_sig || i == gc_thread_restart_sig))
      {
	/* C side handler */
	if (NULL != c_signal_disposition[i].c_handler)
	  posix_signal(env, i, c_signal_disposition[i].c_handler);

	/* lisp side handler */
	if (mk_cl_Cnil != c_signal_disposition[i].lisp_handler
	    && mk_cl_Ct != c_signal_disposition[i].lisp_handler)
	  {
	    install_lisp_signal_handler(env, i, c_signal_disposition[i].lisp_handler);
	  }
      }

#if 0
  for (i = SIGRTMIN; i < NSIG; i++)
    if (!(i == wake_up_sig || i == resume_sig || i == interrupt_sig
	  || i == gc_thread_suspend_sig || i == gc_thread_restart_sig))
      {
	/* C side handler */
	if (NULL != c_signal_disposition[i].c_handler)
	  posix_signal(env, i, c_signal_disposition[i].c_handler);

	/* lisp side handler */
	if (mk_cl_Cnil != c_signal_disposition[i].lisp_handler
	    && mk_cl_Ct != c_signal_disposition[i].lisp_handler)
	  {
	    install_lisp_signal_handler(env, i, c_signal_disposition[i].lisp_handler);
	  }
      }
#endif

#elif MKCL_WINDOWS
  SetUnhandledExceptionFilter(W32_exception_filter);
  SetConsoleCtrlHandler(W32_console_ctrl_handler, TRUE);
#endif

  mk_si_enable_fpe(env, @':default');

  /* The initial thread is now fit for interrupts. */
  mkcl_enable_interrupts(env);
}

void mkcl_clean_up_unixint(MKCL)
{ /* Best effort only. We cannot raise an exception from here. */
#if MKCL_WINDOWS
#elif MKCL_PTHREADS
  int i;

  (void) pthread_cancel(signal_servicing_thread);
  /* We uninstall our signal handlers. */
  for (i = 1; i <= MKCL_SIGMAX; i++)
    {
      if (mkcl_signals[i].installed)
	(void) sigaction(i, &(mkcl_signals[i].old_action), NULL);
      (void) sem_destroy((mkcl_signals[i].sem));
    }
#endif
}



mkcl_object mkcl_signum_to_signal_name(MKCL, mkcl_word signum)
{
  if (signum > MKCL_SIGMAX)
    return mk_cl_Cnil;
  else
    {
      //int intern_flag;
      mkcl_object sig_name_string;

      if (signum > MKCL_BASE_SIGMAX)
        {
          const char format[] = "SIG%d";
          char sig_name_C_string_buffer[sizeof(format) + 5];

          snprintf(sig_name_C_string_buffer, sizeof(sig_name_C_string_buffer), format, (int) signum);
          sig_name_string = mkcl_make_base_string_copy(env, sig_name_C_string_buffer);
        }
      else
        sig_name_string = mkcl_make_base_string_copy(env, signal_names[signum]);

      //return mkcl_intern(env, sig_name_string, mkcl_core.keyword_package, &intern_flag);
      return sig_name_string;
    }
}

mkcl_object mk_si_signum_to_signal_name(MKCL, mkcl_object _signum)
{
  if (!MKCL_FIXNUMP(_signum))
    mkcl_FEtype_error_integer(env, _signum);
  else
    {
      mkcl_word signum = mkcl_fixnum_to_word(_signum);

      @(return mkcl_signum_to_signal_name(env, signum));
    }
}



/* Testing tool only. */
mkcl_object mk_si_do_sigsegv(MKCL)
{
#ifdef __x86_64
  @(return *((mkcl_object *) 0xffffffffffffdeadULL));
#else
  @(return *((mkcl_object *) 0xffffdead));
#endif
}

/* Testing tool only. */
mkcl_object mk_si_objnull(MKCL)
{
  @(return MKCL_OBJNULL);
}

#if MKCL_UNIX

static void _mkcl_display_signal_dispositions(void)
{
  int i;
  for (i = 1; i <= MKCL_SIGMAX; i++)
    {
      struct sigaction act;

      if (sigaction(i, NULL, &act))
        {
          const int errno_for_sigaction = errno;
          fflush(NULL);
          fprintf(stderr, "\nFor signal (%d): error = %d\n", i, errno_for_sigaction);
          perror("_mkcl_display_signal_dispositions failed on sigaction.");
        }
      else
        {
          if (i <= MKCL_BASE_SIGMAX)
            printf("\nsignal %s: action = ", signal_names[i]);
          else if (SIGRTMIN <= i && i <= SIGRTMAX)
            printf("\nsignal SIG%d: (REALTIME) action = ", i);
	  else
            printf("\nsignal SIG%d: action = ", i);

          if ( act.sa_handler == SIG_DFL )
            printf("SIG_DFL");
          else if ( act.sa_handler == SIG_IGN )
            printf("SIG_IGN");
          else if ( act.sa_handler == SIG_HOLD )
            printf("SIG_HOLD");
          else if ( act.sa_handler == SIG_ERR )
            printf("SIG_ERR");
          else
            {
              Dl_info info;

              if ( dladdr(act.sa_handler, &info) )
                {
                  printf("%p, %s from %s",
                         act.sa_handler,
                         info.dli_sname,
                         info.dli_fname);
                }
              else
                printf("%p", act.sa_handler);
            }
        }
    }

  printf("\n");
  fflush(stdout);
}

#else /* !MKCL_UNIX */
static void _mkcl_display_signal_dispositions() { }
#endif /* !MKCL_UNIX */

mkcl_object
mk_si_display_signal_dispositions(MKCL)
{
  mkcl_call_stack_check(env);
  _mkcl_display_signal_dispositions();
  @(return mk_cl_Cnil);
}

#if MKCL_UNIX

static struct sigaction foreign_sigsegv_sigaction;

void
mkcl_sigsegv_monitor(int sig, siginfo_t *info, void *aux)
{
#ifdef DEBUG_SIGNALS
  {
    char buf[24];
    sig_print("\nInside mkcl_sigsegv_monitor, sig = ");
    sig_print(ltoad(sig, buf));
    sig_print(".\n");
  }
#endif

  const mkcl_env env = MKCL_ENV();
  
  if ( env == NULL )
    maybe_lose("MKCL: mkcl_sigsegv_monitor called outside a lisp thread!");

  {
    char address_cstr[24];

    sprintf(address_cstr, "%p", info->si_addr);
    sig_print("\nMKCL: SIGSEGV monitor invoked on address: ");
    sig_print(address_cstr);
    sig_print("\n");

    {
      char buf[24];

      sig_print("\nMKCL: SIGSEGV pid = ");
      sig_print(ltoad(info->si_pid, buf));
      sig_print("\n");
      sig_print("\nMKCL: SIGSEGV si_code = ");
      sig_print(ltoad(info->si_code, buf));
      sig_print("\n");
    }

    if ( foreign_sigsegv_sigaction.sa_flags & SA_SIGINFO )
      (*foreign_sigsegv_sigaction.sa_sigaction)(sig, info, aux);
    else
      (*foreign_sigsegv_sigaction.sa_handler)(sig);
  }

#ifdef DEBUG_SIGNALS
  sig_print("\nLeaving mkcl_sigsegv_monitor.\n");
#endif
}

#endif /* __linux */

mkcl_object mk_si_install_sigsegv_monitor(MKCL)
{
  mkcl_call_stack_check(env);
#if MKCL_UNIX
  if (sigaction(SIGSEGV, NULL, &foreign_sigsegv_sigaction))
    perror("Failed on first sigaction in mk_si_install_sigsegv_monitor.");

  if ( foreign_sigsegv_sigaction.sa_sigaction != mkcl_sigsegv_handler )
    {
      struct sigaction monitor_action;

      monitor_action.sa_sigaction = mkcl_sigsegv_monitor;
      sigemptyset(&monitor_action.sa_mask);
      monitor_action.sa_flags = SA_SIGINFO;

      if (sigaction(SIGSEGV, &monitor_action, &foreign_sigsegv_sigaction))
	perror("Failed on second sigaction in mk_si_install_sigsegv_monitor.");
    }
#endif /* __linux */

  @(return);
}


#ifdef __MINGW64__

#define __ImageBase __MINGW_LSYMBOL(_image_base__)
/* This symbol is defined by the linker.  */
extern IMAGE_DOS_HEADER __ImageBase;


#pragma pack(push,1)
typedef struct _UNWIND_INFO {
  BYTE VersionAndFlags;
  BYTE PrologSize;
  BYTE CountOfUnwindCodes;
  BYTE FrameRegisterAndOffset;
  ULONG AddressOfExceptionHandler;
} UNWIND_INFO,*PUNWIND_INFO;
#pragma pack(pop)

PIMAGE_SECTION_HEADER _FindPESectionByName (const char *);
PIMAGE_SECTION_HEADER _FindPESectionExec (size_t);
PBYTE _GetPEImageBase (void);


#define MAX_PDATA_ENTRIES 32
static RUNTIME_FUNCTION emu_pdata[MAX_PDATA_ENTRIES];
static UNWIND_INFO emu_xdata[MAX_PDATA_ENTRIES];


/* Lifted from MingW64 (crt_handler.c). Used to be called __mingw_init_ehandler(). */
static int SetMingW64UnhandledExceptionFilter(W64UnhandledExceptionFilter filter)
{
  size_t e = 0;
  PIMAGE_SECTION_HEADER pSec;
  PBYTE _ImageBase = _GetPEImageBase ();
  
  if (_FindPESectionByName (".pdata") != NULL)
    { printf("\nMKCL: There is a .pdata section already!\n"); fflush(NULL);}

  /* Allocate # of e tables and entries.  */
  memset (emu_pdata, 0, sizeof (RUNTIME_FUNCTION) * MAX_PDATA_ENTRIES);
  memset (emu_xdata, 0, sizeof (UNWIND_INFO) * MAX_PDATA_ENTRIES);
    
  e = 0;
  /* Fill tables and entries.  */
  while (e < MAX_PDATA_ENTRIES && (pSec = _FindPESectionExec (e)) != NULL)
    {
      emu_xdata[e].VersionAndFlags = 9; /* UNW_FLAG_EHANDLER | UNW_VERSION */
      emu_xdata[e].AddressOfExceptionHandler =
	(DWORD)(size_t) ((LPBYTE)filter - _ImageBase);
      emu_pdata[e].BeginAddress = pSec->VirtualAddress;
      emu_pdata[e].EndAddress = pSec->VirtualAddress + pSec->Misc.VirtualSize;
      emu_pdata[e].UnwindData =
	(DWORD)(size_t)((LPBYTE)&emu_xdata[e] - _ImageBase);
      ++e;
    }
#ifdef _DEBUG_CRT
  if (!e || e > MAX_PDATA_ENTRIES)
    abort ();
#endif

  /* RtlAddFunctionTable.  */
  if (e != 0)
    RtlAddFunctionTable (emu_pdata, e, (DWORD64)_ImageBase);
  return 1;
}

#if __MINGW64_VERSION_MAJOR <= 1
int __mingw_init_ehandler(void)
{
  return SetMingW64UnhandledExceptionFilter(W64_exception_filter);
}
#endif


#if 0 /* Experimental */
static RUNTIME_FUNCTION emu2_pdata[MAX_PDATA_ENTRIES];
static UNWIND_INFO emu2_xdata[MAX_PDATA_ENTRIES];

size_t build_MingW64_UEF_Wrappers(void)
{
  size_t e = 0;
  PIMAGE_SECTION_HEADER pSec;
  PBYTE _ImageBase = _GetPEImageBase ();
  
  if (_FindPESectionByName (".pdata") != NULL)
    /* return 1; */ { printf("\nThere is a .pdata section already!\n"); fflush(NULL);}

  /* Allocate # of e tables and entries.  */
  memset (emu2_pdata, 0, sizeof (RUNTIME_FUNCTION) * MAX_PDATA_ENTRIES);
  memset (emu2_xdata, 0, sizeof (UNWIND_INFO) * MAX_PDATA_ENTRIES);
    
  e = 0;
  /* Fill tables and entries.  */
  while (e < MAX_PDATA_ENTRIES && (pSec = _FindPESectionExec (e)) != NULL)
    {
      emu2_xdata[e].VersionAndFlags = 9; /* UNW_FLAG_EHANDLER | UNW_VERSION */
#if 1
      emu2_xdata[e].AddressOfExceptionHandler =
	(DWORD)(size_t) ((LPBYTE) W64_exception_filter - _ImageBase);
      emu2_pdata[e].BeginAddress = pSec->VirtualAddress;
      emu2_pdata[e].EndAddress = pSec->VirtualAddress + pSec->Misc.VirtualSize;
      emu2_pdata[e].UnwindData =
	(DWORD)(size_t)((LPBYTE)&emu2_xdata[e] - _ImageBase);
#else
      emu2_xdata[e].AddressOfExceptionHandler =	(DWORD) (W64_exception_filter);
      emu2_pdata[e].BeginAddress = (DWORD)(pSec->VirtualAddress + _ImageBase);
      emu2_pdata[e].EndAddress = (DWORD)(pSec->VirtualAddress + _ImageBase + pSec->Misc.VirtualSize);
      emu2_pdata[e].UnwindData = (DWORD)(&emu2_xdata[e]);
#endif
      ++e;
    }
#ifdef _DEBUG_CRT
  if (!e || e > MAX_PDATA_ENTRIES)
    abort ();
#endif

  printf("\nInside build_MingW64_UEF_Wrappers, there was %lld PE sections.\n", e);
  fflush(NULL);

  return e;
}

PRUNTIME_FUNCTION MingW64_UEF_WrapperCallback0(DWORD64 ControlPc,PVOID Context)
{
  printf("\nInside MingW64_UEF_WrapperCallback0().\n"); fflush(NULL);
  return &(emu2_pdata[0]);
}

PRUNTIME_FUNCTION MingW64_UEF_WrapperCallback1(DWORD64 ControlPc,PVOID Context)
{
  printf("\nInside MingW64_UEF_WrapperCallback1().\n"); fflush(NULL);
  return &(emu2_pdata[1]);
}

PRUNTIME_FUNCTION MingW64_UEF_WrapperCallback2(DWORD64 ControlPc,PVOID Context)
{
  printf("\nInside MingW64_UEF_WrapperCallback2().\n"); fflush(NULL);
  return &(emu2_pdata[2]);
}

PRUNTIME_FUNCTION MingW64_UEF_WrapperCallback3(DWORD64 ControlPc,PVOID Context)
{
  printf("\nInside MingW64_UEF_WrapperCallback3().\n"); fflush(NULL);
  return &(emu2_pdata[3]);
}

void register_UEF_WrapperCallbacks(void)
{
  size_t nb_wrappers = build_MingW64_UEF_Wrappers();
  size_t e = 0;
  PBYTE _ImageBase = _GetPEImageBase ();

  for (e = 0; e < nb_wrappers; e++)
    {
      PRUNTIME_FUNCTION (*callback)(DWORD64, PVOID) = NULL;
      PIMAGE_SECTION_HEADER pSec = _FindPESectionExec(e);

      switch (e)
	{
	default: 
	case 3: callback = MingW64_UEF_WrapperCallback3; break;
	case 2: callback = MingW64_UEF_WrapperCallback2; break;
	case 1: callback = MingW64_UEF_WrapperCallback1; break;
	case 0: callback = MingW64_UEF_WrapperCallback0; break;
	}
      if (!RtlInstallFunctionTableCallback
	  (((DWORD64) (pSec->VirtualAddress + _ImageBase))|0x3,
	   ((DWORD64) (pSec->VirtualAddress + _ImageBase)),
	   pSec->Misc.VirtualSize,
	   callback,
	   NULL,
	   NULL))
	{
	  printf("\nRtlInstallFunctionTableCallback succeeded on %lld.\n", e);
	  fflush(NULL);
	}
      else
	{
	  printf("\nRtlInstallFunctionTableCallback succeeded on %lld.\n", e);
	  fflush(NULL);
	}
    }
}
#endif /* Experimental */

#endif /* __MINGW64__ */

