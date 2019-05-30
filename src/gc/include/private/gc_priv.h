/*
 * Copyright 1988, 1989 Hans-J. Boehm, Alan J. Demers
 * Copyright (c) 1991-1994 by Xerox Corporation.  All rights reserved.
 * Copyright (c) 1996-1999 by Silicon Graphics.  All rights reserved.
 * Copyright (c) 1999-2004 Hewlett-Packard Development Company, L.P.
 *
 *
 * THIS MATERIAL IS PROVIDED AS IS, WITH ABSOLUTELY NO WARRANTY EXPRESSED
 * OR IMPLIED.  ANY USE IS AT YOUR OWN RISK.
 *
 * Permission is hereby granted to use or copy this program
 * for any purpose,  provided the above notices are retained on all copies.
 * Permission to modify the code and to distribute modified code is granted,
 * provided the above notices are retained, and a notice that the code was
 * modified is included with the above copyright notice.
 */

/*
 * Copyright 2012, Jean-Claude Beaudoin
 *
 * MKCL cannot ever call abort() or exit(),
 * modifications have been done accordingly.
 */

#ifndef MK_GC_PRIVATE_H
#define MK_GC_PRIVATE_H

#ifdef HAVE_CONFIG_H
# include "config.h"
#endif

#ifndef MK_GC_BUILD
# define MK_GC_BUILD
#endif

#if (defined(__linux__) || defined(__GLIBC__) || defined(__GNU__)) \
    && !defined(_GNU_SOURCE)
  /* Can't test LINUX, since this must be defined before other includes. */
# define _GNU_SOURCE 1
#endif

#if (defined(DGUX) && defined(MK_GC_THREADS) || defined(DGUX386_THREADS) \
     || defined(MK_GC_DGUX386_THREADS)) && !defined(_USING_POSIX4A_DRAFT10)
# define _USING_POSIX4A_DRAFT10 1
#endif

# if defined(NO_DEBUGGING) && !defined(MK_GC_ASSERTIONS) && !defined(NDEBUG)
    /* To turn off assertion checking (in atomic_ops.h). */
#   define NDEBUG 1
# endif

#ifndef MK_GC_H
# include "../gc.h"
#endif

#include <stdlib.h>
#if !defined(sony_news)
# include <stddef.h>
#endif

#ifdef DGUX
# include <sys/types.h>
# include <sys/time.h>
# include <sys/resource.h>
#endif /* DGUX */

#ifdef BSD_TIME
# include <sys/types.h>
# include <sys/time.h>
# include <sys/resource.h>
#endif /* BSD_TIME */

#ifdef PARALLEL_MARK
# define MK_AO_REQUIRE_CAS
# if !defined(__GNUC__) && !defined(MK_AO_ASSUME_WINDOWS98)
#   define MK_AO_ASSUME_WINDOWS98
# endif
#endif

#ifndef MK_GC_TINY_FL_H
# include "../gc_tiny_fl.h"
#endif

#ifndef MK_GC_MARK_H
# include "../gc_mark.h"
#endif

typedef MK_GC_word word;
typedef MK_GC_signed_word signed_word;
typedef unsigned int unsigned32;

typedef int MK_GC_bool;
#define TRUE 1
#define FALSE 0

typedef char * ptr_t;   /* A generic pointer to which we can add        */
                        /* byte displacements and which can be used     */
                        /* for address comparisons.                     */

#ifndef GCCONFIG_H
# include "gcconfig.h"
#endif

#ifndef MK_GC_INNER
  /* This tagging macro must be used at the start of every variable     */
  /* definition which is declared with MK_GC_EXTERN.  Should be also used  */
  /* for the GC-scope function definitions and prototypes.  Must not be */
  /* used in gcconfig.h.  Shouldn't be used for the debugging-only      */
  /* functions.  Currently, not used for the functions declared in or   */
  /* called from the "dated" source files (pcr_interface.c and files    */
  /* located in the "extra" folder).                                    */
# if defined(MK_GC_DLL) && defined(__GNUC__) && !defined(MSWIN32) \
        && !defined(MSWINCE) && !defined(CYGWIN32)
#   if __GNUC__ >= 4
      /* See the corresponding MK_GC_API definition. */
#     define MK_GC_INNER __attribute__((__visibility__("hidden")))
#   else
      /* The attribute is unsupported. */
#     define MK_GC_INNER /* empty */
#   endif
# else
#   define MK_GC_INNER /* empty */
# endif

# define MK_GC_EXTERN extern MK_GC_INNER
  /* Used only for the GC-scope variables (prefixed with "MK_GC_")         */
  /* declared in the header files.  Must not be used for thread-local   */
  /* variables.  Must not be used in gcconfig.h.  Shouldn't be used for */
  /* the debugging-only or profiling-only variables.  Currently, not    */
  /* used for the variables accessed from the "dated" source files      */
  /* (pcr_interface.c, specific.c/h, and in the "extra" folder).        */
  /* The corresponding variable definition must start with MK_GC_INNER.    */
#endif /* !MK_GC_INNER */

#ifndef HEADERS_H
# include "gc_hdrs.h"
#endif

#ifndef MK_GC_ATTR_UNUSED
# if __GNUC__ > 3 || (__GNUC__ == 3 && __GNUC_MINOR__ >= 4)
#   define MK_GC_ATTR_UNUSED __attribute__((__unused__))
# else
#   define MK_GC_ATTR_UNUSED /* empty */
# endif
#endif /* !MK_GC_ATTR_UNUSED */

#if __GNUC__ >= 3 && !defined(LINT2)
# define EXPECT(expr, outcome) __builtin_expect(expr,outcome)
  /* Equivalent to (expr), but predict that usually (expr)==outcome. */
#else
# define EXPECT(expr, outcome) (expr)
#endif /* __GNUC__ */

#ifdef HAVE_CONFIG_H
  /* The "inline" keyword is determined by Autoconf AC_C_INLINE.    */
# define MK_GC_INLINE static inline
#elif defined(_MSC_VER) || defined(__INTEL_COMPILER) || defined(__DMC__) \
        || ((__GNUC__ >= 3) && defined(__STRICT_ANSI__)) \
        || defined(__WATCOMC__)
# define MK_GC_INLINE static __inline
#elif (__GNUC__ >= 3) || defined(__sun)
# define MK_GC_INLINE static inline
#else
# define MK_GC_INLINE static
#endif

#ifndef MK_GC_API_OSCALL
  /* This is used to identify GC routines called by name from OS.       */
# if defined(__GNUC__)
#   if __GNUC__ >= 4
      /* Same as MK_GC_API if MK_GC_DLL.      */
#     define MK_GC_API_OSCALL extern __attribute__((__visibility__("default")))
#   else
      /* The attribute is unsupported.  */
#     define MK_GC_API_OSCALL extern
#   endif
# else
#   define MK_GC_API_OSCALL MK_GC_API
# endif
#endif

#ifndef MK_GC_API_PRIV
# define MK_GC_API_PRIV MK_GC_API
#endif

#ifndef MK_GC_LOCKS_H
# include "gc_locks.h"
#endif

#define ONES ((word)(signed_word)(-1))

# ifdef STACK_GROWS_DOWN
#   define COOLER_THAN >
#   define HOTTER_THAN <
#   define MAKE_COOLER(x,y) if ((word)((x) + (y)) > (word)(x)) {(x) += (y);} \
                            else (x) = (ptr_t)ONES
#   define MAKE_HOTTER(x,y) (x) -= (y)
# else
#   define COOLER_THAN <
#   define HOTTER_THAN >
#   define MAKE_COOLER(x,y) if ((word)((x) - (y)) < (word)(x)) {(x) -= (y);} \
                            else (x) = 0
#   define MAKE_HOTTER(x,y) (x) += (y)
# endif

#if defined(AMIGA) && defined(__SASC)
#   define MK_GC_FAR __far
#else
#   define MK_GC_FAR
#endif


/*********************************/
/*                               */
/* Definitions for conservative  */
/* collector                     */
/*                               */
/*********************************/

/*********************************/
/*                               */
/* Easily changeable parameters  */
/*                               */
/*********************************/

/* #define STUBBORN_ALLOC */
                    /* Enable stubborn allocation, and thus a limited   */
                    /* form of incremental collection w/o dirty bits.   */

/* #define ALL_INTERIOR_POINTERS */
                    /* Forces all pointers into the interior of an      */
                    /* object to be considered valid.  Also causes the  */
                    /* sizes of all objects to be inflated by at least  */
                    /* one byte.  This should suffice to guarantee      */
                    /* that in the presence of a compiler that does     */
                    /* not perform garbage-collector-unsafe             */
                    /* optimizations, all portable, strictly ANSI       */
                    /* conforming C programs should be safely usable    */
                    /* with malloc replaced by MK_GC_malloc and free       */
                    /* calls removed.  There are several disadvantages: */
                    /* 1. There are probably no interesting, portable,  */
                    /*    strictly ANSI conforming C programs.          */
                    /* 2. This option makes it hard for the collector   */
                    /*    to allocate space that is not "pointed to"    */
                    /*    by integers, etc.  Under SunOS 4.X with a     */
                    /*    statically linked libc, we empirically        */
                    /*    observed that it would be difficult to        */
                    /*    allocate individual objects larger than 100K. */
                    /*    Even if only smaller objects are allocated,   */
                    /*    more swap space is likely to be needed.       */
                    /*    Fortunately, much of this will never be       */
                    /*    touched.                                      */
                    /* If you can easily avoid using this option, do.   */
                    /* If not, try to keep individual objects small.    */
                    /* This is now really controlled at startup,        */
                    /* through MK_GC_all_interior_pointers.                */


#ifndef MK_GC_NO_FINALIZATION
#  define MK_GC_INVOKE_FINALIZERS() MK_GC_notify_or_invoke_finalizers()
   MK_GC_INNER void MK_GC_notify_or_invoke_finalizers(void);
                        /* If MK_GC_finalize_on_demand is not set, invoke  */
                        /* eligible finalizers. Otherwise:              */
                        /* Call *MK_GC_finalizer_notifier if there are     */
                        /* finalizers to be run, and we haven't called  */
                        /* this procedure yet this GC cycle.            */

   MK_GC_INNER void MK_GC_push_finalizer_structures(void);
   MK_GC_INNER void MK_GC_finalize(void);
                        /* Perform all indicated finalization actions   */
                        /* on unmarked objects.                         */
                        /* Unreachable finalizable objects are enqueued */
                        /* for processing by MK_GC_invoke_finalizers.      */
                        /* Invoked with lock.                           */

#  ifndef SMALL_CONFIG
     MK_GC_INNER void MK_GC_print_finalization_stats(void);
#  endif
#else
#  define MK_GC_INVOKE_FINALIZERS() (void)0
#endif /* MK_GC_NO_FINALIZATION */

#if !defined(DONT_ADD_BYTE_AT_END)
# ifdef LINT2
    /* Explicitly instruct the code analysis tool that                  */
    /* MK_GC_all_interior_pointers is assumed to have only 0 or 1 value.   */
#   define EXTRA_BYTES (MK_GC_all_interior_pointers? 1 : 0)
# else
#   define EXTRA_BYTES MK_GC_all_interior_pointers
# endif
# define MAX_EXTRA_BYTES 1
#else
# define EXTRA_BYTES 0
# define MAX_EXTRA_BYTES 0
#endif


# ifndef LARGE_CONFIG
#   define MINHINCR 16   /* Minimum heap increment, in blocks of HBLKSIZE  */
                         /* Must be multiple of largest page size.         */
#   define MAXHINCR 2048 /* Maximum heap increment, in blocks              */
# else
#   define MINHINCR 64
#   define MAXHINCR 4096
# endif

# define BL_LIMIT MK_GC_black_list_spacing
                           /* If we need a block of N bytes, and we have */
                           /* a block of N + BL_LIMIT bytes available,   */
                           /* and N > BL_LIMIT,                          */
                           /* but all possible positions in it are       */
                           /* blacklisted, we just use it anyway (and    */
                           /* print a warning, if warnings are enabled). */
                           /* This risks subsequently leaking the block  */
                           /* due to a false reference.  But not using   */
                           /* the block risks unreasonable immediate     */
                           /* heap growth.                               */

/*********************************/
/*                               */
/* Stack saving for debugging    */
/*                               */
/*********************************/

#ifdef NEED_CALLINFO
    struct callinfo {
        word ci_pc;     /* Caller, not callee, pc       */
#       if NARGS > 0
            word ci_arg[NARGS]; /* bit-wise complement to avoid retention */
#       endif
#       if (NFRAMES * (NARGS + 1)) % 2 == 1
            /* Likely alignment problem. */
            word ci_dummy;
#       endif
    };
#endif

#ifdef SAVE_CALL_CHAIN
  /* Fill in the pc and argument information for up to NFRAMES of my    */
  /* callers.  Ignore my frame and my callers frame.                    */
  MK_GC_INNER void MK_GC_save_callers(struct callinfo info[NFRAMES]);
  MK_GC_INNER void MK_GC_print_callers(struct callinfo info[NFRAMES]);
#endif


/*********************************/
/*                               */
/* OS interface routines         */
/*                               */
/*********************************/

#ifdef BSD_TIME
# undef CLOCK_TYPE
# undef GET_TIME
# undef MS_TIME_DIFF
# define CLOCK_TYPE struct timeval
# define GET_TIME(x) \
                do { \
                  struct rusage rusage; \
                  getrusage(RUSAGE_SELF, &rusage); \
                  x = rusage.ru_utime; \
                } while (0)
# define MS_TIME_DIFF(a,b) ((unsigned long)(a.tv_sec - b.tv_sec) * 1000 \
                            + (unsigned long)(a.tv_usec - b.tv_usec) / 1000)
#elif defined(MSWIN32) || defined(MSWINCE)
# ifndef WIN32_LEAN_AND_MEAN
#   define WIN32_LEAN_AND_MEAN 1
# endif
# define NOSERVICE
# include <windows.h>
# include <winbase.h>
# define CLOCK_TYPE DWORD
# define GET_TIME(x) (void)(x = GetTickCount())
# define MS_TIME_DIFF(a,b) ((long)((a)-(b)))
#else /* !MSWIN32, !MSWINCE, !BSD_TIME */
# include <time.h>
# if defined(FREEBSD) && !defined(CLOCKS_PER_SEC)
#   include <machine/limits.h>
#   define CLOCKS_PER_SEC CLK_TCK
# endif
# if !defined(CLOCKS_PER_SEC)
#   define CLOCKS_PER_SEC 1000000
    /* This is technically a bug in the implementation.                 */
    /* ANSI requires that CLOCKS_PER_SEC be defined.  But at least      */
    /* under SunOS 4.1.1, it isn't.  Also note that the combination of  */
    /* ANSI C and POSIX is incredibly gross here.  The type clock_t     */
    /* is used by both clock() and times().  But on some machines       */
    /* these use different notions of a clock tick, CLOCKS_PER_SEC      */
    /* seems to apply only to clock.  Hence we use it here.  On many    */
    /* machines, including SunOS, clock actually uses units of          */
    /* microseconds (which are not really clock ticks).                 */
# endif
# define CLOCK_TYPE clock_t
# define GET_TIME(x) (void)(x = clock())
# define MS_TIME_DIFF(a,b) (CLOCKS_PER_SEC % 1000 == 0 ? \
        (unsigned long)((a) - (b)) / (unsigned long)(CLOCKS_PER_SEC / 1000) \
        : ((unsigned long)((a) - (b)) * 1000) / (unsigned long)CLOCKS_PER_SEC)
  /* Avoid using double type since some targets (like ARM) might        */
  /* require -lm option for double-to-long conversion.                  */
#endif /* !BSD_TIME && !MSWIN32 */

/* We use bzero and bcopy internally.  They may not be available.       */
# if defined(SPARC) && defined(SUNOS4)
#   define BCOPY_EXISTS
# endif
# if defined(M68K) && defined(AMIGA)
#   define BCOPY_EXISTS
# endif
# if defined(M68K) && defined(NEXT)
#   define BCOPY_EXISTS
# endif
# if defined(VAX)
#   define BCOPY_EXISTS
# endif
# if defined(AMIGA)
#   include <string.h>
#   define BCOPY_EXISTS
# endif
# if defined(DARWIN)
#   include <string.h>
#   define BCOPY_EXISTS
# endif
# if defined(MACOS) && defined(POWERPC)
#   include <MacMemory.h>
#   define bcopy(x,y,n) BlockMoveData(x, y, n)
#   define bzero(x,n) BlockZero(x, n)
#   define BCOPY_EXISTS
# endif

# ifndef BCOPY_EXISTS
#   include <string.h>
#   define BCOPY(x,y,n) memcpy(y, x, (size_t)(n))
#   define BZERO(x,n)  memset(x, 0, (size_t)(n))
# else
#   define BCOPY(x,y,n) bcopy((void *)(x),(void *)(y),(size_t)(n))
#   define BZERO(x,n) bzero((void *)(x),(size_t)(n))
# endif

/*
 * Stop and restart mutator threads.
 */
# ifdef PCR
#     include "th/PCR_ThCtl.h"
#     define STOP_WORLD() \
        PCR_ThCtl_SetExclusiveMode(PCR_ThCtl_ExclusiveMode_stopNormal, \
                                   PCR_allSigsBlocked, \
                                   PCR_waitForever)
#     define START_WORLD() \
        PCR_ThCtl_SetExclusiveMode(PCR_ThCtl_ExclusiveMode_null, \
                                   PCR_allSigsBlocked, \
                                   PCR_waitForever)
# else
#   if defined(MK_GC_WIN32_THREADS) || defined(MK_GC_PTHREADS)
      MK_GC_INNER void MK_GC_stop_world(void);
      MK_GC_INNER void MK_GC_start_world(void);
#     define STOP_WORLD() MK_GC_stop_world()
#     define START_WORLD() MK_GC_start_world()
#   else
        /* Just do a sanity check: we are not inside MK_GC_do_blocking().  */
#     define STOP_WORLD() MK_GC_ASSERT(MK_GC_blocked_sp == NULL)
#     define START_WORLD()
#   endif
# endif

/* Abandon ship */
#if 1 /* MKCL cannot ever call abort() or exit(). JCB */
MK_GC_API_PRIV MK_GC_abort_func MK_GC_on_abort;
# define ABORT(msg) MK_GC_on_abort(msg)
#else
# ifdef PCR
#   define ABORT(s) PCR_Base_Panic(s)
# else
#   if defined(MSWINCE) && !defined(DebugBreak) \
       && (!defined(UNDER_CE) || (defined(__MINGW32CE__) && !defined(ARM32)))
      /* This simplifies linking for WinCE (and, probably, doesn't      */
      /* hurt debugging much); use -DDebugBreak=DebugBreak to override  */
      /* this behavior if really needed.  This is also a workaround for */
      /* x86mingw32ce toolchain (if it is still declaring DebugBreak()  */
      /* instead of defining it as a macro).                            */
#     define DebugBreak() _exit(-1) /* there is no abort() in WinCE */
#   endif
#   ifdef SMALL_CONFIG
#     define MK_GC_on_abort(msg) (void)0 /* be silent on abort */
#   else
      MK_GC_API_PRIV MK_GC_abort_func MK_GC_on_abort;
#   endif /* !SMALL_CONFIG */
#   if defined(MSWIN32) && (defined(NO_DEBUGGING) || defined(LINT2))
      /* A more user-friendly abort after showing fatal message.        */
#     define ABORT(msg) (MK_GC_on_abort(msg), _exit(-1))
                /* Exit on error without running "at-exit" callbacks.   */
#   elif defined(MSWINCE) && defined(NO_DEBUGGING)
#     define ABORT(msg) (MK_GC_on_abort(msg), ExitProcess(-1))
#   elif defined(MSWIN32) || defined(MSWINCE)
#     define ABORT(msg) { MK_GC_on_abort(msg); DebugBreak(); }
                /* Note that: on a WinCE box, this could be silently    */
                /* ignored (i.e., the program is not aborted);          */
                /* DebugBreak is a statement in some toolchains.        */
#   else
#     define ABORT(msg) (MK_GC_on_abort(msg), abort())
#   endif /* !MSWIN32 */
# endif /* !PCR */
#endif

/* For abort message with 1-3 arguments.  C_msg and C_fmt should be     */
/* literals.  C_msg should not contain format specifiers.  Arguments    */
/* should match their format specifiers.                                */
#define ABORT_ARG1(C_msg, C_fmt, arg1) \
                do { \
                  MK_GC_COND_LOG_PRINTF(C_msg /* + */ C_fmt, arg1); \
                  ABORT(C_msg); \
                } while (0)
#define ABORT_ARG2(C_msg, C_fmt, arg1, arg2) \
                do { \
                  MK_GC_COND_LOG_PRINTF(C_msg /* + */ C_fmt, arg1, arg2); \
                  ABORT(C_msg); \
                } while (0)
#define ABORT_ARG3(C_msg, C_fmt, arg1, arg2, arg3) \
                do { \
                  MK_GC_COND_LOG_PRINTF(C_msg /* + */ C_fmt, arg1, arg2, arg3); \
                  ABORT(C_msg); \
                } while (0)

/* Same as ABORT but does not have 'no-return' attribute.       */
/* ABORT on a dummy condition (which is always true).           */
#define ABORT_RET(msg) \
              if ((signed_word)MK_GC_current_warn_proc == -1) {} else ABORT(msg)

/* Exit abnormally, but without making a mess (e.g. out of memory) */
# ifdef PCR
#   define EXIT() PCR_Base_Exit(1,PCR_waitForever)
# else
#  if 1 /* MKCL cannot ever call abort() or exit(). JCB */
#   define EXIT() (MK_GC_on_abort(NULL))
#  else
#   define EXIT() (MK_GC_on_abort(NULL), exit(1 /* EXIT_FAILURE */))
#  endif
# endif


/* Print warning message, e.g. almost out of memory.    */
/* The argument (if any) format specifier should be:    */
/* "%s", "%p" or "%"WARN_PRIdPTR.                       */
#define WARN(msg, arg) (*MK_GC_current_warn_proc)("GC Warning: " msg, \
                                               (MK_GC_word)(arg))
MK_GC_EXTERN MK_GC_warn_proc MK_GC_current_warn_proc;

/* Print format type macro for decimal signed_word value passed WARN(). */
/* This could be redefined for Win64 or LLP64, but typically should     */
/* not be done as the WARN format string is, possibly, processed on the */
/* client side, so non-standard print type modifiers (like MS "I64d")   */
/* should be avoided here if possible.                                  */
#ifndef WARN_PRIdPTR
  /* Assume sizeof(void *) == sizeof(long) (or a little-endian machine) */
# define WARN_PRIdPTR "ld"
#endif

/* Get environment entry */
#ifdef MK_GC_READ_ENV_FILE
  MK_GC_INNER char * MK_GC_envfile_getenv(const char *name);
# define GETENV(name) MK_GC_envfile_getenv(name)
#elif defined(NO_GETENV)
# define GETENV(name) NULL
#elif defined(EMPTY_GETENV_RESULTS)
  /* Workaround for a reputed Wine bug.   */
  MK_GC_INLINE char * fixed_getenv(const char *name)
  {
    char *value = getenv(name);
    return value != NULL && *value != '\0' ? value : NULL;
  }
# define GETENV(name) fixed_getenv(name)
#else
# define GETENV(name) getenv(name)
#endif

#if defined(DARWIN)
# ifndef MAC_OS_X_VERSION_MAX_ALLOWED
#   include <AvailabilityMacros.h>
                /* Include this header just to import the above macro.  */
# endif
# if defined(POWERPC)
#   if CPP_WORDSZ == 32
#     define MK_GC_THREAD_STATE_T          ppc_thread_state_t
#     define MK_GC_MACH_THREAD_STATE       PPC_THREAD_STATE
#     define MK_GC_MACH_THREAD_STATE_COUNT PPC_THREAD_STATE_COUNT
#   else
#     define MK_GC_THREAD_STATE_T          ppc_thread_state64_t
#     define MK_GC_MACH_THREAD_STATE       PPC_THREAD_STATE64
#     define MK_GC_MACH_THREAD_STATE_COUNT PPC_THREAD_STATE64_COUNT
#   endif
# elif defined(I386) || defined(X86_64)
#   if CPP_WORDSZ == 32
#     if defined(i386_THREAD_STATE_COUNT) && !defined(x86_THREAD_STATE32_COUNT)
        /* Use old naming convention for 32-bit x86.    */
#       define MK_GC_THREAD_STATE_T                i386_thread_state_t
#       define MK_GC_MACH_THREAD_STATE             i386_THREAD_STATE
#       define MK_GC_MACH_THREAD_STATE_COUNT       i386_THREAD_STATE_COUNT
#     else
#       define MK_GC_THREAD_STATE_T                x86_thread_state32_t
#       define MK_GC_MACH_THREAD_STATE             x86_THREAD_STATE32
#       define MK_GC_MACH_THREAD_STATE_COUNT       x86_THREAD_STATE32_COUNT
#     endif
#   else
#     define MK_GC_THREAD_STATE_T          x86_thread_state64_t
#     define MK_GC_MACH_THREAD_STATE       x86_THREAD_STATE64
#     define MK_GC_MACH_THREAD_STATE_COUNT x86_THREAD_STATE64_COUNT
#   endif
# else
#   if defined(ARM32)
#     define MK_GC_THREAD_STATE_T                  arm_thread_state_t
#     ifdef ARM_MACHINE_THREAD_STATE_COUNT
#       define MK_GC_MACH_THREAD_STATE             ARM_MACHINE_THREAD_STATE
#       define MK_GC_MACH_THREAD_STATE_COUNT       ARM_MACHINE_THREAD_STATE_COUNT
#     endif
#   else
#     error define MK_GC_THREAD_STATE_T
#   endif
# endif
# ifndef MK_GC_MACH_THREAD_STATE
#   define MK_GC_MACH_THREAD_STATE         MACHINE_THREAD_STATE
#   define MK_GC_MACH_THREAD_STATE_COUNT   MACHINE_THREAD_STATE_COUNT
# endif

# if CPP_WORDSZ == 32
#   define MK_GC_MACH_HEADER   mach_header
#   define MK_GC_MACH_SECTION  section
#   define MK_GC_GETSECTBYNAME getsectbynamefromheader
# else
#   define MK_GC_MACH_HEADER   mach_header_64
#   define MK_GC_MACH_SECTION  section_64
#   define MK_GC_GETSECTBYNAME getsectbynamefromheader_64
# endif

  /* Try to work out the right way to access thread state structure     */
  /* members.  The structure has changed its definition in different    */
  /* Darwin versions.  This now defaults to the (older) names           */
  /* without __, thus hopefully, not breaking any existing              */
  /* Makefile.direct builds.                                            */
# if __DARWIN_UNIX03
#   define THREAD_FLD(x) __ ## x
# else
#   define THREAD_FLD(x) x
# endif
#endif /* DARWIN */

/*********************************/
/*                               */
/* Word-size-dependent defines   */
/*                               */
/*********************************/

#if CPP_WORDSZ == 32
# define WORDS_TO_BYTES(x) ((x)<<2)
# define BYTES_TO_WORDS(x) ((x)>>2)
# define LOGWL             ((word)5) /* log[2] of CPP_WORDSZ    */
# define modWORDSZ(n) ((n) & 0x1f) /* n mod size of word        */
# if ALIGNMENT != 4
#   define UNALIGNED_PTRS
# endif
#endif

#if CPP_WORDSZ == 64
#  define WORDS_TO_BYTES(x)   ((x)<<3)
#  define BYTES_TO_WORDS(x)   ((x)>>3)
#  define LOGWL               ((word)6)    /* log[2] of CPP_WORDSZ */
#  define modWORDSZ(n) ((n) & 0x3f)        /* n mod size of word            */
#  if ALIGNMENT != 8
#       define UNALIGNED_PTRS
#  endif
#endif

/* The first TINY_FREELISTS free lists correspond to the first  */
/* TINY_FREELISTS multiples of GRANULE_BYTES, i.e. we keep      */
/* separate free lists for each multiple of GRANULE_BYTES       */
/* up to (TINY_FREELISTS-1) * GRANULE_BYTES.  After that they   */
/* may be spread out further.                                   */
#include "../gc_tiny_fl.h"
#define GRANULE_BYTES MK_GC_GRANULE_BYTES
#define TINY_FREELISTS MK_GC_TINY_FREELISTS

#define WORDSZ ((word)CPP_WORDSZ)
#define SIGNB  ((word)1 << (WORDSZ-1))
#define BYTES_PER_WORD      ((word)(sizeof (word)))
#define divWORDSZ(n) ((n) >> LOGWL)     /* divide n by size of word */

#if GRANULE_BYTES == 8
# define BYTES_TO_GRANULES(n) ((n)>>3)
# define GRANULES_TO_BYTES(n) ((n)<<3)
# if CPP_WORDSZ == 64
#   define GRANULES_TO_WORDS(n) (n)
# elif CPP_WORDSZ == 32
#   define GRANULES_TO_WORDS(n) ((n)<<1)
# else
#   define GRANULES_TO_WORDS(n) BYTES_TO_WORDS(GRANULES_TO_BYTES(n))
# endif
#elif GRANULE_BYTES == 16
# define BYTES_TO_GRANULES(n) ((n)>>4)
# define GRANULES_TO_BYTES(n) ((n)<<4)
# if CPP_WORDSZ == 64
#   define GRANULES_TO_WORDS(n) ((n)<<1)
# elif CPP_WORDSZ == 32
#   define GRANULES_TO_WORDS(n) ((n)<<2)
# else
#   define GRANULES_TO_WORDS(n) BYTES_TO_WORDS(GRANULES_TO_BYTES(n))
# endif
#else
# error Bad GRANULE_BYTES value
#endif

/*********************/
/*                   */
/*  Size Parameters  */
/*                   */
/*********************/

/* Heap block size, bytes. Should be power of 2.                */
/* Incremental GC with MPROTECT_VDB currently requires the      */
/* page size to be a multiple of HBLKSIZE.  Since most modern   */
/* architectures support variable page sizes down to 4K, and    */
/* X86 is generally 4K, we now default to 4K, except for        */
/*   Alpha: Seems to be used with 8K pages.                     */
/*   SMALL_CONFIG: Want less block-level fragmentation.         */
#ifndef HBLKSIZE
# if defined(LARGE_CONFIG) || !defined(SMALL_CONFIG)
#   ifdef ALPHA
#     define CPP_LOG_HBLKSIZE 13
#   else
#     define CPP_LOG_HBLKSIZE 12
#   endif
# else
#   define CPP_LOG_HBLKSIZE 10
# endif
#else
# if HBLKSIZE == 512
#   define CPP_LOG_HBLKSIZE 9
# elif HBLKSIZE == 1024
#   define CPP_LOG_HBLKSIZE 10
# elif HBLKSIZE == 2048
#   define CPP_LOG_HBLKSIZE 11
# elif HBLKSIZE == 4096
#   define CPP_LOG_HBLKSIZE 12
# elif HBLKSIZE == 8192
#   define CPP_LOG_HBLKSIZE 13
# elif HBLKSIZE == 16384
#   define CPP_LOG_HBLKSIZE 14
# else
    --> fix HBLKSIZE
# endif
# undef HBLKSIZE
#endif

# define CPP_HBLKSIZE (1 << CPP_LOG_HBLKSIZE)
# define LOG_HBLKSIZE   ((size_t)CPP_LOG_HBLKSIZE)
# define HBLKSIZE ((size_t)CPP_HBLKSIZE)


/*  max size objects supported by freelist (larger objects are  */
/*  allocated directly with allchblk(), by rounding to the next */
/*  multiple of HBLKSIZE.                                       */

#define CPP_MAXOBJBYTES (CPP_HBLKSIZE/2)
#define MAXOBJBYTES ((size_t)CPP_MAXOBJBYTES)
#define CPP_MAXOBJWORDS BYTES_TO_WORDS(CPP_MAXOBJBYTES)
#define MAXOBJWORDS ((size_t)CPP_MAXOBJWORDS)
#define CPP_MAXOBJGRANULES BYTES_TO_GRANULES(CPP_MAXOBJBYTES)
#define MAXOBJGRANULES ((size_t)CPP_MAXOBJGRANULES)

# define divHBLKSZ(n) ((n) >> LOG_HBLKSIZE)

# define HBLK_PTR_DIFF(p,q) divHBLKSZ((ptr_t)p - (ptr_t)q)
        /* Equivalent to subtracting 2 hblk pointers.   */
        /* We do it this way because a compiler should  */
        /* find it hard to use an integer division      */
        /* instead of a shift.  The bundled SunOS 4.1   */
        /* o.w. sometimes pessimizes the subtraction to */
        /* involve a call to .div.                      */

# define modHBLKSZ(n) ((n) & (HBLKSIZE-1))

# define HBLKPTR(objptr) ((struct hblk *)(((word) (objptr)) & ~(HBLKSIZE-1)))

# define HBLKDISPL(objptr) (((size_t) (objptr)) & (HBLKSIZE-1))

/* Round up byte allocation requests to integral number of words, etc. */
# define ROUNDED_UP_GRANULES(n) \
        BYTES_TO_GRANULES((n) + (GRANULE_BYTES - 1 + EXTRA_BYTES))
# if MAX_EXTRA_BYTES == 0
#  define SMALL_OBJ(bytes) EXPECT((bytes) <= (MAXOBJBYTES), TRUE)
# else
#  define SMALL_OBJ(bytes) \
            (EXPECT((bytes) <= (MAXOBJBYTES - MAX_EXTRA_BYTES), TRUE) \
             || (bytes) <= MAXOBJBYTES - EXTRA_BYTES)
        /* This really just tests bytes <= MAXOBJBYTES - EXTRA_BYTES.   */
        /* But we try to avoid looking up EXTRA_BYTES.                  */
# endif
# define ADD_SLOP(bytes) ((bytes) + EXTRA_BYTES)
# ifndef MIN_WORDS
#  define MIN_WORDS 2   /* FIXME: obsolete */
# endif

/*
 * Hash table representation of sets of pages.
 * Implements a map from aligned HBLKSIZE chunks of the address space to one
 * bit each.
 * This assumes it is OK to spuriously set bits, e.g. because multiple
 * addresses are represented by a single location.
 * Used by black-listing code, and perhaps by dirty bit maintenance code.
 */

# ifdef LARGE_CONFIG
#   if CPP_WORDSZ == 32
#     define LOG_PHT_ENTRIES 20 /* Collisions likely at 1M blocks,      */
                                /* which is >= 4GB.  Each table takes   */
                                /* 128KB, some of which may never be    */
                                /* touched.                             */
#   else
#     define LOG_PHT_ENTRIES 21 /* Collisions likely at 2M blocks,      */
                                /* which is >= 8GB.  Each table takes   */
                                /* 256KB, some of which may never be    */
                                /* touched.                             */
#   endif
# elif !defined(SMALL_CONFIG)
#   define LOG_PHT_ENTRIES  18   /* Collisions are likely if heap grows */
                                 /* to more than 256K hblks >= 1GB.     */
                                 /* Each hash table occupies 32K bytes. */
                                 /* Even for somewhat smaller heaps,    */
                                 /* say half that, collisions may be an */
                                 /* issue because we blacklist          */
                                 /* addresses outside the heap.         */
# else
#   define LOG_PHT_ENTRIES  15   /* Collisions are likely if heap grows */
                                 /* to more than 32K hblks = 128MB.     */
                                 /* Each hash table occupies 4K bytes.  */
# endif
# define PHT_ENTRIES ((word)1 << LOG_PHT_ENTRIES)
# define PHT_SIZE (PHT_ENTRIES >> LOGWL)
typedef word page_hash_table[PHT_SIZE];

# define PHT_HASH(addr) ((((word)(addr)) >> LOG_HBLKSIZE) & (PHT_ENTRIES - 1))

# define get_pht_entry_from_index(bl, index) \
                (((bl)[divWORDSZ(index)] >> modWORDSZ(index)) & 1)
# define set_pht_entry_from_index(bl, index) \
                (bl)[divWORDSZ(index)] |= (word)1 << modWORDSZ(index)
# define clear_pht_entry_from_index(bl, index) \
                (bl)[divWORDSZ(index)] &= ~((word)1 << modWORDSZ(index))
/* And a dumb but thread-safe version of set_pht_entry_from_index.      */
/* This sets (many) extra bits.                                         */
# define set_pht_entry_from_index_safe(bl, index) \
                (bl)[divWORDSZ(index)] = ONES


/********************************************/
/*                                          */
/*    H e a p   B l o c k s                 */
/*                                          */
/********************************************/

/*  heap block header */
#define HBLKMASK   (HBLKSIZE-1)

#define MARK_BITS_PER_HBLK (HBLKSIZE/GRANULE_BYTES)
           /* upper bound                                    */
           /* We allocate 1 bit per allocation granule.      */
           /* If MARK_BIT_PER_GRANULE is defined, we use     */
           /* every nth bit, where n is the number of        */
           /* allocation granules per object.  If            */
           /* MARK_BIT_PER_OBJ is defined, we only use the   */
           /* initial group of mark bits, and it is safe     */
           /* to allocate smaller header for large objects.  */

#ifdef PARALLEL_MARK
# include "atomic_ops.h"
# define counter_t volatile MK_AO_t
#else
  typedef size_t counter_t;
# if defined(THREADS) && (defined(MPROTECT_VDB) \
                || (defined(MK_GC_ASSERTIONS) && defined(THREAD_LOCAL_ALLOC)))
#   include "atomic_ops.h"
# endif
#endif /* !PARALLEL_MARK */

union word_ptr_ao_u {
  word w;
  signed_word sw;
  void *vp;
# ifdef MK_AO_HAVE_load
    volatile MK_AO_t ao;
# endif
};

/* We maintain layout maps for heap blocks containing objects of a given */
/* size.  Each entry in this map describes a byte offset and has the     */
/* following type.                                                       */
struct hblkhdr {
    struct hblk * hb_next;      /* Link field for hblk free list         */
                                /* and for lists of chunks waiting to be */
                                /* reclaimed.                            */
    struct hblk * hb_prev;      /* Backwards link for free list.        */
    struct hblk * hb_block;     /* The corresponding block.             */
    unsigned char hb_obj_kind;
                         /* Kind of objects in the block.  Each kind    */
                         /* identifies a mark procedure and a set of    */
                         /* list headers.  Sometimes called regions.    */
    unsigned char hb_flags;
#       define IGNORE_OFF_PAGE  1       /* Ignore pointers that do not  */
                                        /* point to the first page of   */
                                        /* this object.                 */
#       define WAS_UNMAPPED 2   /* This is a free block, which has      */
                                /* been unmapped from the address       */
                                /* space.                               */
                                /* MK_GC_remap must be invoked on it       */
                                /* before it can be reallocated.        */
                                /* Only set with USE_MUNMAP.            */
#       define FREE_BLK 4       /* Block is free, i.e. not in use.      */
#       ifdef ENABLE_DISCLAIM
#         define HAS_DISCLAIM 8
                                /* This kind has a callback on reclaim. */
#         define MARK_UNCONDITIONALLY 0x10
                                /* Mark from all objects, marked or     */
                                /* not.  Used to mark objects needed by */
                                /* reclaim notifier.                    */
#       endif
    unsigned short hb_last_reclaimed;
                                /* Value of MK_GC_gc_no when block was     */
                                /* last allocated or swept. May wrap.   */
                                /* For a free block, this is maintained */
                                /* only for USE_MUNMAP, and indicates   */
                                /* when the header was allocated, or    */
                                /* when the size of the block last      */
                                /* changed.                             */
    size_t hb_sz;  /* If in use, size in bytes, of objects in the block. */
                   /* if free, the size in bytes of the whole block      */
                   /* We assume that this is convertible to signed_word  */
                   /* without generating a negative result.  We avoid    */
                   /* generating free blocks larger than that.           */
    word hb_descr;              /* object descriptor for marking.  See  */
                                /* mark.h.                              */
#   ifdef MARK_BIT_PER_OBJ
      unsigned32 hb_inv_sz;     /* A good upper bound for 2**32/hb_sz.  */
                                /* For large objects, we use            */
                                /* LARGE_INV_SZ.                        */
#     define LARGE_INV_SZ (1 << 16)
#   else
      unsigned char hb_large_block;
      short * hb_map;           /* Essentially a table of remainders    */
                                /* mod BYTES_TO_GRANULES(hb_sz), except */
                                /* for large blocks.  See MK_GC_obj_map.   */
#   endif
    counter_t hb_n_marks;       /* Number of set mark bits, excluding   */
                                /* the one always set at the end.       */
                                /* Currently it is concurrently         */
                                /* updated and hence only approximate.  */
                                /* But a zero value does guarantee that */
                                /* the block contains no marked         */
                                /* objects.                             */
                                /* Ensuring this property means that we */
                                /* never decrement it to zero during a  */
                                /* collection, and hence the count may  */
                                /* be one too high.  Due to concurrent  */
                                /* updates, an arbitrary number of      */
                                /* increments, but not all of them (!)  */
                                /* may be lost, hence it may in theory  */
                                /* be much too low.                     */
                                /* The count may also be too high if    */
                                /* multiple mark threads mark the       */
                                /* same object due to a race.           */
                                /* Without parallel marking, the count  */
                                /* is accurate.                         */
#   ifdef USE_MARK_BYTES
#     define MARK_BITS_SZ (MARK_BITS_PER_HBLK + 1)
        /* Unlike the other case, this is in units of bytes.            */
        /* Since we force double-word alignment, we need at most one    */
        /* mark bit per 2 words.  But we do allocate and set one        */
        /* extra mark bit to avoid an explicit check for the            */
        /* partial object at the end of each block.                     */
      union {
        char _hb_marks[MARK_BITS_SZ];
                            /* The i'th byte is 1 if the object         */
                            /* starting at granule i or object i is     */
                            /* marked, 0 o.w.                           */
                            /* The mark bit for the "one past the       */
                            /* end" object is always set to avoid a     */
                            /* special case test in the marker.         */
        word dummy;     /* Force word alignment of mark bytes. */
      } _mark_byte_union;
#     define hb_marks _mark_byte_union._hb_marks
#   else
#     define MARK_BITS_SZ (MARK_BITS_PER_HBLK/CPP_WORDSZ + 1)
      word hb_marks[MARK_BITS_SZ];
#   endif /* !USE_MARK_BYTES */
};

# define ANY_INDEX 23   /* "Random" mark bit index for assertions */

/*  heap block body */

# define HBLK_WORDS (HBLKSIZE/sizeof(word))
# define HBLK_GRANULES (HBLKSIZE/GRANULE_BYTES)

/* The number of objects in a block dedicated to a certain size.        */
/* may erroneously yield zero (instead of one) for large objects.       */
# define HBLK_OBJS(sz_in_bytes) (HBLKSIZE/(sz_in_bytes))

struct hblk {
    char hb_body[HBLKSIZE];
};

# define HBLK_IS_FREE(hdr) (((hdr) -> hb_flags & FREE_BLK) != 0)

# define OBJ_SZ_TO_BLOCKS(sz) divHBLKSZ((sz) + HBLKSIZE-1)
    /* Size of block (in units of HBLKSIZE) needed to hold objects of   */
    /* given sz (in bytes).                                             */

/* Object free list link */
# define obj_link(p) (*(void  **)(p))

# define LOG_MAX_MARK_PROCS 6
# define MAX_MARK_PROCS (1 << LOG_MAX_MARK_PROCS)

/* Root sets.  Logically private to mark_rts.c.  But we don't want the  */
/* tables scanned, so we put them here.                                 */
/* MAX_ROOT_SETS is the maximum number of ranges that can be    */
/* registered as static roots.                                  */
# ifdef LARGE_CONFIG
#   define MAX_ROOT_SETS 8192
# elif !defined(SMALL_CONFIG)
#   define MAX_ROOT_SETS 2048
# else
#   define MAX_ROOT_SETS 512
# endif

# define MAX_EXCLUSIONS (MAX_ROOT_SETS/4)
/* Maximum number of segments that can be excluded from root sets.      */

/*
 * Data structure for excluded static roots.
 */
struct exclusion {
    ptr_t e_start;
    ptr_t e_end;
};

/* Data structure for list of root sets.                                */
/* We keep a hash table, so that we can filter out duplicate additions. */
/* Under Win32, we need to do a better job of filtering overlaps, so    */
/* we resort to sequential search, and pay the price.                   */
struct roots {
        ptr_t r_start;/* multiple of word size */
        ptr_t r_end;  /* multiple of word size and greater than r_start */
#       if !defined(MSWIN32) && !defined(MSWINCE) && !defined(CYGWIN32)
          struct roots * r_next;
#       endif
        MK_GC_bool r_tmp;
                /* Delete before registering new dynamic libraries */
};

#if !defined(MSWIN32) && !defined(MSWINCE) && !defined(CYGWIN32)
    /* Size of hash table index to roots.       */
#   define LOG_RT_SIZE 6
#   define RT_SIZE (1 << LOG_RT_SIZE) /* Power of 2, may be != MAX_ROOT_SETS */
#endif

#ifndef MAX_HEAP_SECTS
# ifdef LARGE_CONFIG
#   if CPP_WORDSZ > 32
#     define MAX_HEAP_SECTS 8192        /* overflows at roughly 128 GB  */
#   else
#     define MAX_HEAP_SECTS 768         /* Separately added heap sections. */
#   endif
# elif defined(SMALL_CONFIG) && !defined(USE_PROC_FOR_LIBRARIES)
#   if defined(PARALLEL_MARK) && (defined(MSWIN32) || defined(CYGWIN32))
#     define MAX_HEAP_SECTS 384
#   else
#     define MAX_HEAP_SECTS 128         /* Roughly 256MB (128*2048*1K)  */
#   endif
# elif CPP_WORDSZ > 32
#   define MAX_HEAP_SECTS 1024          /* Roughly 8GB                  */
# else
#   define MAX_HEAP_SECTS 512           /* Roughly 4GB                  */
# endif
#endif /* !MAX_HEAP_SECTS */

typedef struct MK_GC_ms_entry {
    ptr_t mse_start;    /* First word of object, word aligned.  */
    union word_ptr_ao_u mse_descr;
                        /* Descriptor; low order two bits are tags,     */
                        /* as described in gc_mark.h.                   */
} mse;

/* Lists of all heap blocks and free lists      */
/* as well as other random data structures      */
/* that should not be scanned by the            */
/* collector.                                   */
/* These are grouped together in a struct       */
/* so that they can be easily skipped by the    */
/* MK_GC_mark routine.                             */
/* The ordering is weird to make MK_GC_malloc      */
/* faster by keeping the important fields       */
/* sufficiently close together that a           */
/* single load of a base register will do.      */
/* Scalars that could easily appear to          */
/* be pointers are also put here.               */
/* The main fields should precede any           */
/* conditionally included fields, so that       */
/* gc_inl.h will work even if a different set   */
/* of macros is defined when the client is      */
/* compiled.                                    */

struct _MK_GC_arrays {
  word _heapsize;               /* Heap size in bytes.                  */
  word _requested_heapsize;     /* Heap size due to explicit expansion. */
  ptr_t _last_heap_addr;
  ptr_t _prev_heap_addr;
  word _large_free_bytes;
        /* Total bytes contained in blocks on large object free */
        /* list.                                                */
  word _large_allocd_bytes;
        /* Total number of bytes in allocated large objects blocks.     */
        /* For the purposes of this counter and the next one only, a    */
        /* large object is one that occupies a block of at least        */
        /* 2*HBLKSIZE.                                                  */
  word _max_large_allocd_bytes;
        /* Maximum number of bytes that were ever allocated in          */
        /* large object blocks.  This is used to help decide when it    */
        /* is safe to split up a large block.                           */
  word _bytes_allocd_before_gc;
                /* Number of bytes allocated before this        */
                /* collection cycle.                            */
# ifndef SEPARATE_GLOBALS
#   define MK_GC_bytes_allocd MK_GC_arrays._bytes_allocd
    word _bytes_allocd;
        /* Number of bytes allocated during this collection cycle.      */
# endif
  word _bytes_dropped;
        /* Number of black-listed bytes dropped during GC cycle */
        /* as a result of repeated scanning during allocation   */
        /* attempts.  These are treated largely as allocated,   */
        /* even though they are not useful to the client.       */
  word _bytes_finalized;
        /* Approximate number of bytes in objects (and headers) */
        /* that became ready for finalization in the last       */
        /* collection.                                          */
  word _bytes_freed;
        /* Number of explicitly deallocated bytes of memory     */
        /* since last collection.                               */
  word _finalizer_bytes_freed;
        /* Bytes of memory explicitly deallocated while         */
        /* finalizers were running.  Used to approximate mem.   */
        /* explicitly deallocated by finalizers.                */
  ptr_t _scratch_end_ptr;
  ptr_t _scratch_last_end_ptr;
        /* Used by headers.c, and can easily appear to point to */
        /* heap.                                                */
  mse *_mark_stack;
        /* Limits of stack for MK_GC_mark routine.  All ranges     */
        /* between MK_GC_mark_stack (incl.) and MK_GC_mark_stack_top  */
        /* (incl.) still need to be marked from.                */
  mse *_mark_stack_limit;
# ifdef PARALLEL_MARK
    mse *volatile _mark_stack_top;
        /* Updated only with mark lock held, but read asynchronously.   */
        /* TODO: Use union to avoid casts to MK_AO_t */
# else
    mse *_mark_stack_top;
# endif
  MK_GC_mark_proc _mark_procs[MAX_MARK_PROCS];
        /* Table of user-defined mark procedures.  There is     */
        /* a small number of these, which can be referenced     */
        /* by DS_PROC mark descriptors.  See gc_mark.h.         */
# ifndef SEPARATE_GLOBALS
#   define MK_GC_objfreelist MK_GC_arrays._objfreelist
    void *_objfreelist[MAXOBJGRANULES+1];
                          /* free list for objects */
#   define MK_GC_aobjfreelist MK_GC_arrays._aobjfreelist
    void *_aobjfreelist[MAXOBJGRANULES+1];
                          /* free list for atomic objs  */
# endif
  void *_uobjfreelist[MAXOBJGRANULES+1];
                          /* Uncollectible but traced objs      */
                          /* objects on this and auobjfreelist  */
                          /* are always marked, except during   */
                          /* garbage collections.               */
# ifdef ATOMIC_UNCOLLECTABLE
#   define MK_GC_auobjfreelist MK_GC_arrays._auobjfreelist
    void *_auobjfreelist[MAXOBJGRANULES+1];
                        /* Atomic uncollectible but traced objs */
# endif
  word _composite_in_use; /* Number of bytes in the accessible  */
                          /* composite objects.                 */
  word _atomic_in_use;    /* Number of bytes in the accessible  */
                          /* atomic objects.                    */
# ifdef USE_MUNMAP
#   define MK_GC_unmapped_bytes MK_GC_arrays._unmapped_bytes
    word _unmapped_bytes;
# else
#   define MK_GC_unmapped_bytes 0
# endif
  size_t _size_map[MAXOBJBYTES+1];
        /* Number of granules to allocate when asked for a certain      */
        /* number of bytes.                                             */

# ifdef STUBBORN_ALLOC
#   define MK_GC_sobjfreelist MK_GC_arrays._sobjfreelist
    ptr_t _sobjfreelist[MAXOBJGRANULES+1];
# endif
                          /* free list for immutable objects    */
# ifdef MARK_BIT_PER_GRANULE
#   define MK_GC_obj_map MK_GC_arrays._obj_map
    short * _obj_map[MAXOBJGRANULES+1];
                       /* If not NULL, then a pointer to a map of valid */
                       /* object addresses.                             */
                       /* _obj_map[sz_in_granules][i] is                */
                       /* i % sz_in_granules.                           */
                       /* This is now used purely to replace a          */
                       /* division in the marker by a table lookup.     */
                       /* _obj_map[0] is used for large objects and     */
                       /* contains all nonzero entries.  This gets us   */
                       /* out of the marker fast path without an extra  */
                       /* test.                                         */
#   define MAP_LEN BYTES_TO_GRANULES(HBLKSIZE)
# endif
# define VALID_OFFSET_SZ HBLKSIZE
  char _valid_offsets[VALID_OFFSET_SZ];
                                /* MK_GC_valid_offsets[i] == TRUE ==> i    */
                                /* is registered as a displacement.     */
  char _modws_valid_offsets[sizeof(word)];
                                /* MK_GC_valid_offsets[i] ==>                */
                                /* MK_GC_modws_valid_offsets[i%sizeof(word)] */
# ifdef STUBBORN_ALLOC
#   define MK_GC_changed_pages MK_GC_arrays._changed_pages
    page_hash_table _changed_pages;
        /* Stubborn object pages that were changes since last call to   */
        /* MK_GC_read_changed.                                             */
#   define MK_GC_prev_changed_pages MK_GC_arrays._prev_changed_pages
    page_hash_table _prev_changed_pages;
        /* Stubborn object pages that were changes before last call to  */
        /* MK_GC_read_changed.                                             */
# endif
# if defined(PROC_VDB) || defined(MPROTECT_VDB) \
     || defined(GWW_VDB) || defined(MANUAL_VDB)
#   define MK_GC_grungy_pages MK_GC_arrays._grungy_pages
    page_hash_table _grungy_pages; /* Pages that were dirty at last     */
                                   /* MK_GC_read_dirty.                    */
# endif
# if defined(MPROTECT_VDB) || defined(MANUAL_VDB)
#   define MK_GC_dirty_pages MK_GC_arrays._dirty_pages
    volatile page_hash_table _dirty_pages;
                        /* Pages dirtied since last MK_GC_read_dirty. */
# endif
# if defined(PROC_VDB) || defined(GWW_VDB)
#   define MK_GC_written_pages MK_GC_arrays._written_pages
    page_hash_table _written_pages;     /* Pages ever dirtied   */
# endif
# define MK_GC_heap_sects MK_GC_arrays._heap_sects
  struct HeapSect {
    ptr_t hs_start;
    size_t hs_bytes;
  } _heap_sects[MAX_HEAP_SECTS];        /* Heap segments potentially    */
                                        /* client objects.              */
# if defined(USE_PROC_FOR_LIBRARIES)
#   define MK_GC_our_memory MK_GC_arrays._our_memory
    struct HeapSect _our_memory[MAX_HEAP_SECTS];
                                        /* All GET_MEM allocated        */
                                        /* memory.  Includes block      */
                                        /* headers and the like.        */
# endif
# if defined(MSWIN32) || defined(MSWINCE) || defined(CYGWIN32)
#   define MK_GC_heap_bases MK_GC_arrays._heap_bases
    ptr_t _heap_bases[MAX_HEAP_SECTS];
                /* Start address of memory regions obtained from kernel. */
# endif
# ifdef MSWINCE
#   define MK_GC_heap_lengths MK_GC_arrays._heap_lengths
    word _heap_lengths[MAX_HEAP_SECTS];
                /* Committed lengths of memory regions obtained from kernel. */
# endif
  struct roots _static_roots[MAX_ROOT_SETS];
# if !defined(MSWIN32) && !defined(MSWINCE) && !defined(CYGWIN32)
#   define MK_GC_root_index MK_GC_arrays._root_index
    struct roots * _root_index[RT_SIZE];
# endif
  struct exclusion _excl_table[MAX_EXCLUSIONS];
  /* Block header index; see gc_headers.h */
  bottom_index * _all_nils;
  bottom_index * _top_index [TOP_SZ];
# ifdef ENABLE_TRACE
#   define MK_GC_trace_addr MK_GC_arrays._trace_addr
    ptr_t _trace_addr;
# endif
# ifdef SAVE_CALL_CHAIN
#   define MK_GC_last_stack MK_GC_arrays._last_stack
    struct callinfo _last_stack[NFRAMES];
                /* Stack at last garbage collection.  Useful for        */
                /* debugging mysterious object disappearances.  In the  */
                /* multi-threaded case, we currently only save the      */
                /* calling stack.                                       */
# endif
};

MK_GC_API_PRIV MK_GC_FAR struct _MK_GC_arrays MK_GC_arrays;

#define MK_GC_all_nils MK_GC_arrays._all_nils
#define MK_GC_atomic_in_use MK_GC_arrays._atomic_in_use
#define MK_GC_bytes_allocd_before_gc MK_GC_arrays._bytes_allocd_before_gc
#define MK_GC_bytes_dropped MK_GC_arrays._bytes_dropped
#define MK_GC_bytes_finalized MK_GC_arrays._bytes_finalized
#define MK_GC_bytes_freed MK_GC_arrays._bytes_freed
#define MK_GC_composite_in_use MK_GC_arrays._composite_in_use
#define MK_GC_excl_table MK_GC_arrays._excl_table
#define MK_GC_finalizer_bytes_freed MK_GC_arrays._finalizer_bytes_freed
#define MK_GC_heapsize MK_GC_arrays._heapsize
#define MK_GC_large_allocd_bytes MK_GC_arrays._large_allocd_bytes
#define MK_GC_large_free_bytes MK_GC_arrays._large_free_bytes
#define MK_GC_last_heap_addr MK_GC_arrays._last_heap_addr
#define MK_GC_mark_stack MK_GC_arrays._mark_stack
#define MK_GC_mark_stack_limit MK_GC_arrays._mark_stack_limit
#define MK_GC_mark_stack_top MK_GC_arrays._mark_stack_top
#define MK_GC_mark_procs MK_GC_arrays._mark_procs
#define MK_GC_max_large_allocd_bytes MK_GC_arrays._max_large_allocd_bytes
#define MK_GC_modws_valid_offsets MK_GC_arrays._modws_valid_offsets
#define MK_GC_prev_heap_addr MK_GC_arrays._prev_heap_addr
#define MK_GC_requested_heapsize MK_GC_arrays._requested_heapsize
#define MK_GC_scratch_end_ptr MK_GC_arrays._scratch_end_ptr
#define MK_GC_scratch_last_end_ptr MK_GC_arrays._scratch_last_end_ptr
#define MK_GC_size_map MK_GC_arrays._size_map
#define MK_GC_static_roots MK_GC_arrays._static_roots
#define MK_GC_top_index MK_GC_arrays._top_index
#define MK_GC_uobjfreelist MK_GC_arrays._uobjfreelist
#define MK_GC_valid_offsets MK_GC_arrays._valid_offsets

#define beginMK_GC_arrays ((ptr_t)(&MK_GC_arrays))
#define endMK_GC_arrays (((ptr_t)(&MK_GC_arrays)) + (sizeof MK_GC_arrays))
#define USED_HEAP_SIZE (MK_GC_heapsize - MK_GC_large_free_bytes)

/* Object kinds: */
#define MAXOBJKINDS 16

MK_GC_EXTERN struct obj_kind {
   void **ok_freelist;  /* Array of free listheaders for this kind of object */
                        /* Point either to MK_GC_arrays or to storage allocated */
                        /* with MK_GC_scratch_alloc.                            */
   struct hblk **ok_reclaim_list;
                        /* List headers for lists of blocks waiting to be */
                        /* swept.                                         */
                        /* Indexed by object size in granules.            */
   word ok_descriptor;  /* Descriptor template for objects in this      */
                        /* block.                                       */
   MK_GC_bool ok_relocate_descr;
                        /* Add object size in bytes to descriptor       */
                        /* template to obtain descriptor.  Otherwise    */
                        /* template is used as is.                      */
   MK_GC_bool ok_init;   /* Clear objects before putting them on the free list. */
#  ifdef ENABLE_DISCLAIM
     MK_GC_bool ok_mark_unconditionally;
                        /* Mark from all, including unmarked, objects   */
                        /* in block.  Used to protect objects reachable */
                        /* from reclaim notifiers.                      */
     int (MK_GC_CALLBACK *ok_disclaim_proc)(void * /*obj*/);
                        /* The disclaim procedure is called before obj  */
                        /* is reclaimed, but must also tolerate being   */
                        /* called with object from freelist.  Non-zero  */
                        /* exit prevents object from being reclaimed.   */
#    define OK_DISCLAIM_INITZ /* comma */, FALSE, 0
#  else
#    define OK_DISCLAIM_INITZ /* empty */
#  endif /* !ENABLE_DISCLAIM */
} MK_GC_obj_kinds[MAXOBJKINDS];

#define beginMK_GC_obj_kinds ((ptr_t)(&MK_GC_obj_kinds))
#define endMK_GC_obj_kinds (beginMK_GC_obj_kinds + (sizeof MK_GC_obj_kinds))

/* Variables that used to be in MK_GC_arrays, but need to be accessed by   */
/* inline allocation code.  If they were in MK_GC_arrays, the inlined      */
/* allocation code would include MK_GC_arrays offsets (as it did), which   */
/* introduce maintenance problems.                                      */

#ifdef SEPARATE_GLOBALS
  extern word MK_GC_bytes_allocd;
        /* Number of bytes allocated during this collection cycle.      */
  extern ptr_t MK_GC_objfreelist[MAXOBJGRANULES+1];
                          /* free list for NORMAL objects */
# define beginMK_GC_objfreelist ((ptr_t)(&MK_GC_objfreelist))
# define endMK_GC_objfreelist (beginMK_GC_objfreelist + sizeof(MK_GC_objfreelist))

  extern ptr_t MK_GC_aobjfreelist[MAXOBJGRANULES+1];
                          /* free list for atomic (PTRFREE) objs        */
# define beginMK_GC_aobjfreelist ((ptr_t)(&MK_GC_aobjfreelist))
# define endMK_GC_aobjfreelist (beginMK_GC_aobjfreelist + sizeof(MK_GC_aobjfreelist))
#endif /* SEPARATE_GLOBALS */

/* Predefined kinds: */
#define PTRFREE 0
#define NORMAL  1
#define UNCOLLECTABLE 2
#ifdef ATOMIC_UNCOLLECTABLE
# define AUNCOLLECTABLE 3
# define STUBBORN 4
# define IS_UNCOLLECTABLE(k) (((k) & ~1) == UNCOLLECTABLE)
#else
# define STUBBORN 3
# define IS_UNCOLLECTABLE(k) ((k) == UNCOLLECTABLE)
#endif

MK_GC_EXTERN unsigned MK_GC_n_kinds;

MK_GC_EXTERN word MK_GC_n_heap_sects; /* Number of separately added heap      */
                                /* sections.                            */

#ifdef USE_PROC_FOR_LIBRARIES
  MK_GC_EXTERN word MK_GC_n_memory;   /* Number of GET_MEM allocated memory   */
                                /* sections.                            */
#endif

MK_GC_EXTERN word MK_GC_page_size;

#if defined(MSWIN32) || defined(MSWINCE) || defined(CYGWIN32)
  struct _SYSTEM_INFO;
  MK_GC_EXTERN struct _SYSTEM_INFO MK_GC_sysinfo;
  MK_GC_INNER MK_GC_bool MK_GC_is_heap_base(ptr_t p);
#endif


MK_GC_EXTERN word MK_GC_black_list_spacing;
                        /* Average number of bytes between blacklisted  */
                        /* blocks. Approximate.                         */
                        /* Counts only blocks that are                  */
                        /* "stack-blacklisted", i.e. that are           */
                        /* problematic in the interior of an object.    */

#ifdef MK_GC_GCJ_SUPPORT
  extern struct hblk * MK_GC_hblkfreelist[];
                                        /* Remains visible to GNU GCJ. */
#endif

#ifdef MK_GC_DISABLE_INCREMENTAL
# define MK_GC_incremental FALSE
                        /* Hopefully allow optimizer to remove some code. */
# define TRUE_INCREMENTAL FALSE
#else
  MK_GC_EXTERN MK_GC_bool MK_GC_incremental;
                        /* Using incremental/generational collection. */
# define TRUE_INCREMENTAL \
        (MK_GC_incremental && MK_GC_time_limit != MK_GC_TIME_UNLIMITED)
        /* True incremental, not just generational, mode */
#endif /* !MK_GC_DISABLE_INCREMENTAL */

MK_GC_EXTERN word MK_GC_root_size; /* Total size of registered root sections. */

MK_GC_EXTERN MK_GC_bool MK_GC_debugging_started;
                                /* MK_GC_debug_malloc has been called.     */

/* This is used by MK_GC_do_blocking[_inner]().            */
struct blocking_data {
    MK_GC_fn_type fn;
    void * client_data; /* and result */
};

/* This is used by MK_GC_call_with_gc_active(), MK_GC_push_all_stack_sections(). */
struct MK_GC_traced_stack_sect_s {
  ptr_t saved_stack_ptr;
# ifdef IA64
    ptr_t saved_backing_store_ptr;
    ptr_t backing_store_end;
# endif
  struct MK_GC_traced_stack_sect_s *prev;
};

#ifdef THREADS
  /* Process all "traced stack sections" - scan entire stack except for */
  /* frames belonging to the user functions invoked by MK_GC_do_blocking.  */
  MK_GC_INNER void MK_GC_push_all_stack_sections(ptr_t lo, ptr_t hi,
                        struct MK_GC_traced_stack_sect_s *traced_stack_sect);
  MK_GC_EXTERN word MK_GC_total_stacksize; /* updated on every push_all_stacks */
#else
  MK_GC_EXTERN ptr_t MK_GC_blocked_sp;
  MK_GC_EXTERN struct MK_GC_traced_stack_sect_s *MK_GC_traced_stack_sect;
                        /* Points to the "frame" data held in stack by  */
                        /* the innermost MK_GC_call_with_gc_active().      */
                        /* NULL if no such "frame" active.              */
#endif /* !THREADS */

#ifdef IA64
  /* Similar to MK_GC_push_all_stack_sections() but for IA-64 registers store. */
  MK_GC_INNER void MK_GC_push_all_register_sections(ptr_t bs_lo, ptr_t bs_hi,
                  int eager, struct MK_GC_traced_stack_sect_s *traced_stack_sect);
#endif

/*  Marks are in a reserved area in                          */
/*  each heap block.  Each word has one mark bit associated  */
/*  with it. Only those corresponding to the beginning of an */
/*  object are used.                                         */

/* Mark bit operations */

/*
 * Retrieve, set, clear the nth mark bit in a given heap block.
 *
 * (Recall that bit n corresponds to nth object or allocation granule
 * relative to the beginning of the block, including unused words)
 */

#ifdef USE_MARK_BYTES
# define mark_bit_from_hdr(hhdr,n) ((hhdr)->hb_marks[n])
# define set_mark_bit_from_hdr(hhdr,n) ((hhdr)->hb_marks[n] = 1)
# define clear_mark_bit_from_hdr(hhdr,n) ((hhdr)->hb_marks[n] = 0)
#else
/* Set mark bit correctly, even if mark bits may be concurrently        */
/* accessed.                                                            */
# ifdef PARALLEL_MARK
    /* This is used only if we explicitly set USE_MARK_BITS.    */
#   define OR_WORD(addr, bits) MK_AO_or((volatile MK_AO_t *)(addr), (MK_AO_t)(bits))
# else
#   define OR_WORD(addr, bits) (void)(*(addr) |= (bits))
# endif
# define mark_bit_from_hdr(hhdr,n) \
              (((hhdr)->hb_marks[divWORDSZ(n)] >> modWORDSZ(n)) & (word)1)
# define set_mark_bit_from_hdr(hhdr,n) \
              OR_WORD((hhdr)->hb_marks+divWORDSZ(n), (word)1 << modWORDSZ(n))
# define clear_mark_bit_from_hdr(hhdr,n) \
              ((hhdr)->hb_marks[divWORDSZ(n)] &= ~((word)1 << modWORDSZ(n)))
#endif /* !USE_MARK_BYTES */

#ifdef MARK_BIT_PER_OBJ
#  define MARK_BIT_NO(offset, sz) (((unsigned)(offset))/(sz))
        /* Get the mark bit index corresponding to the given byte       */
        /* offset and size (in bytes).                                  */
#  define MARK_BIT_OFFSET(sz) 1
        /* Spacing between useful mark bits.                            */
#  define IF_PER_OBJ(x) x
#  define FINAL_MARK_BIT(sz) ((sz) > MAXOBJBYTES? 1 : HBLK_OBJS(sz))
        /* Position of final, always set, mark bit.                     */
#else /* MARK_BIT_PER_GRANULE */
#  define MARK_BIT_NO(offset, sz) BYTES_TO_GRANULES((unsigned)(offset))
#  define MARK_BIT_OFFSET(sz) BYTES_TO_GRANULES(sz)
#  define IF_PER_OBJ(x)
#  define FINAL_MARK_BIT(sz) \
                ((sz) > MAXOBJBYTES ? MARK_BITS_PER_HBLK \
                                : BYTES_TO_GRANULES((sz) * HBLK_OBJS(sz)))
#endif

/* Important internal collector routines */

MK_GC_INNER ptr_t MK_GC_approx_sp(void);

MK_GC_INNER MK_GC_bool MK_GC_should_collect(void);

void MK_GC_apply_to_all_blocks(void (*fn)(struct hblk *h, word client_data),
                            word client_data);
                        /* Invoke fn(hbp, client_data) for each         */
                        /* allocated heap block.                        */
MK_GC_INNER struct hblk * MK_GC_next_used_block(struct hblk * h);
                        /* Return first in-use block >= h       */
MK_GC_INNER struct hblk * MK_GC_prev_block(struct hblk * h);
                        /* Return last block <= h.  Returned block      */
                        /* is managed by GC, but may or may not be in   */
                        /* use.                                         */
MK_GC_INNER void MK_GC_mark_init(void);
MK_GC_INNER void MK_GC_clear_marks(void);
                        /* Clear mark bits for all heap objects.        */
MK_GC_INNER void MK_GC_invalidate_mark_state(void);
                                /* Tell the marker that marked          */
                                /* objects may point to unmarked        */
                                /* ones, and roots may point to         */
                                /* unmarked objects.  Reset mark stack. */
MK_GC_INNER MK_GC_bool MK_GC_mark_some(ptr_t cold_gc_frame);
                        /* Perform about one pages worth of marking     */
                        /* work of whatever kind is needed.  Returns    */
                        /* quickly if no collection is in progress.     */
                        /* Return TRUE if mark phase finished.          */
MK_GC_INNER void MK_GC_initiate_gc(void);
                                /* initiate collection.                 */
                                /* If the mark state is invalid, this   */
                                /* becomes full collection.  Otherwise  */
                                /* it's partial.                        */

MK_GC_INNER MK_GC_bool MK_GC_collection_in_progress(void);
                        /* Collection is in progress, or was abandoned. */

#ifndef MK_GC_DISABLE_INCREMENTAL
# define MK_GC_PUSH_CONDITIONAL(b, t, all) \
                MK_GC_push_conditional((ptr_t)(b), (ptr_t)(t), all)
                        /* Do either of MK_GC_push_all or MK_GC_push_selected */
                        /* depending on the third arg.                  */
#else
# define MK_GC_PUSH_CONDITIONAL(b, t, all) MK_GC_push_all((ptr_t)(b), (ptr_t)(t))
#endif

MK_GC_INNER void MK_GC_push_all_stack(ptr_t b, ptr_t t);
                                    /* As MK_GC_push_all but consider      */
                                    /* interior pointers as valid.      */
MK_GC_INNER void MK_GC_push_all_eager(ptr_t b, ptr_t t);
                                    /* Same as MK_GC_push_all_stack, but   */
                                    /* ensures that stack is scanned    */
                                    /* immediately, not just scheduled  */
                                    /* for scanning.                    */

  /* In the threads case, we push part of the current thread stack      */
  /* with MK_GC_push_all_eager when we push the registers.  This gets the  */
  /* callee-save registers that may disappear.  The remainder of the    */
  /* stacks are scheduled for scanning in *MK_GC_push_other_roots, which   */
  /* is thread-package-specific.                                        */

MK_GC_INNER void MK_GC_push_roots(MK_GC_bool all, ptr_t cold_gc_frame);
                                        /* Push all or dirty roots.     */

MK_GC_API_PRIV MK_GC_push_other_roots_proc MK_GC_push_other_roots;
                        /* Push system or application specific roots    */
                        /* onto the mark stack.  In some environments   */
                        /* (e.g. threads environments) this is          */
                        /* predefined to be non-zero.  A client         */
                        /* supplied replacement should also call the    */
                        /* original function.  Remains externally       */
                        /* visible as used by some well-known 3rd-party */
                        /* software (e.g., ECL) currently.              */

#ifdef THREADS
  void MK_GC_push_thread_structures(void);
#endif
MK_GC_EXTERN void (*MK_GC_push_typed_structures)(void);
                        /* A pointer such that we can avoid linking in  */
                        /* the typed allocation support if unused.      */

MK_GC_INNER void MK_GC_with_callee_saves_pushed(void (*fn)(ptr_t, void *),
                                          ptr_t arg);

#if defined(SPARC) || defined(IA64)
  /* Cause all stacked registers to be saved in memory.  Return a       */
  /* pointer to the top of the corresponding memory stack.              */
  ptr_t MK_GC_save_regs_in_stack(void);
#endif
                        /* Push register contents onto mark stack.      */

#if defined(MSWIN32) || defined(MSWINCE)
  void __cdecl MK_GC_push_one(word p);
#else
  void MK_GC_push_one(word p);
                              /* If p points to an object, mark it    */
                              /* and push contents on the mark stack  */
                              /* Pointer recognition test always      */
                              /* accepts interior pointers, i.e. this */
                              /* is appropriate for pointers found on */
                              /* stack.                               */
#endif

#if defined(PRINT_BLACK_LIST) || defined(KEEP_BACK_PTRS)
  MK_GC_INNER void MK_GC_mark_and_push_stack(ptr_t p, ptr_t source);
                                /* Ditto, omits plausibility test       */
#else
  MK_GC_INNER void MK_GC_mark_and_push_stack(ptr_t p);
#endif

MK_GC_INNER void MK_GC_clear_hdr_marks(hdr * hhdr);
                                    /* Clear the mark bits in a header */
MK_GC_INNER void MK_GC_set_hdr_marks(hdr * hhdr);
                                    /* Set the mark bits in a header */
MK_GC_INNER void MK_GC_set_fl_marks(ptr_t p);
                                    /* Set all mark bits associated with */
                                    /* a free list.                      */
#if defined(MK_GC_ASSERTIONS) && defined(THREADS) && defined(THREAD_LOCAL_ALLOC)
  void MK_GC_check_fl_marks(void **);
                                    /* Check that all mark bits         */
                                    /* associated with a free list are  */
                                    /* set.  Abort if not.              */
#endif
void MK_GC_add_roots_inner(ptr_t b, ptr_t e, MK_GC_bool tmp);
MK_GC_INNER void MK_GC_exclude_static_roots_inner(void *start, void *finish);
#if defined(DYNAMIC_LOADING) || defined(MSWIN32) || defined(MSWINCE) \
    || defined(CYGWIN32) || defined(PCR)
  MK_GC_INNER void MK_GC_register_dynamic_libraries(void);
                /* Add dynamic library data sections to the root set. */
#endif
MK_GC_INNER void MK_GC_cond_register_dynamic_libraries(void);
                /* Remove and reregister dynamic libraries if we're     */
                /* configured to do that at each GC.                    */

/* Machine dependent startup routines */
ptr_t MK_GC_get_main_stack_base(void);     /* Cold end of stack.           */
#ifdef IA64
  MK_GC_INNER ptr_t MK_GC_get_register_stack_base(void);
                                        /* Cold end of register stack.  */
#endif
void MK_GC_register_data_segments(void);

#ifdef THREADS
  MK_GC_INNER void MK_GC_thr_init(void);
  MK_GC_INNER void MK_GC_thr_uninit(void);
  MK_GC_INNER void MK_GC_init_parallel(void);
#else
  MK_GC_INNER MK_GC_bool MK_GC_is_static_root(ptr_t p);
                /* Is the address p in one of the registered static     */
                /* root sections?                                       */
#endif

/* Black listing: */
#ifdef PRINT_BLACK_LIST
  MK_GC_INNER void MK_GC_add_to_black_list_normal(word p, ptr_t source);
                        /* Register bits as a possible future false     */
                        /* reference from the heap or static data       */
# define MK_GC_ADD_TO_BLACK_LIST_NORMAL(bits, source) \
                if (MK_GC_all_interior_pointers) { \
                  MK_GC_add_to_black_list_stack((word)(bits), (source)); \
                } else \
                  MK_GC_add_to_black_list_normal((word)(bits), (source))
  MK_GC_INNER void MK_GC_add_to_black_list_stack(word p, ptr_t source);
# define MK_GC_ADD_TO_BLACK_LIST_STACK(bits, source) \
            MK_GC_add_to_black_list_stack((word)(bits), (source))
#else
  MK_GC_INNER void MK_GC_add_to_black_list_normal(word p);
# define MK_GC_ADD_TO_BLACK_LIST_NORMAL(bits, source) \
                if (MK_GC_all_interior_pointers) { \
                  MK_GC_add_to_black_list_stack((word)(bits)); \
                } else \
                  MK_GC_add_to_black_list_normal((word)(bits))
  MK_GC_INNER void MK_GC_add_to_black_list_stack(word p);
# define MK_GC_ADD_TO_BLACK_LIST_STACK(bits, source) \
            MK_GC_add_to_black_list_stack((word)(bits))
#endif /* PRINT_BLACK_LIST */

struct hblk * MK_GC_is_black_listed(struct hblk * h, word len);
                        /* If there are likely to be false references   */
                        /* to a block starting at h of the indicated    */
                        /* length, then return the next plausible       */
                        /* starting location for h that might avoid     */
                        /* these false references.  Remains externally  */
                        /* visible as used by GNU GCJ currently.        */

MK_GC_INNER void MK_GC_promote_black_lists(void);
                        /* Declare an end to a black listing phase.     */
MK_GC_INNER void MK_GC_unpromote_black_lists(void);
                        /* Approximately undo the effect of the above.  */
                        /* This actually loses some information, but    */
                        /* only in a reasonably safe way.               */

MK_GC_INNER ptr_t MK_GC_scratch_alloc(size_t bytes);
                                /* GC internal memory allocation for    */
                                /* small objects.  Deallocation is not  */
                                /* possible.  May return NULL.          */

/* Heap block layout maps: */
MK_GC_INNER MK_GC_bool MK_GC_add_map_entry(size_t sz);
                                /* Add a heap block map for objects of  */
                                /* size sz to obj_map.                  */
                                /* Return FALSE on failure.             */
MK_GC_INNER void MK_GC_register_displacement_inner(size_t offset);
                                /* Version of MK_GC_register_displacement  */
                                /* that assumes lock is already held.   */

/*  hblk allocation: */
MK_GC_INNER void MK_GC_new_hblk(size_t size_in_granules, int kind);
                                /* Allocate a new heap block, and build */
                                /* a free list in it.                   */

MK_GC_INNER ptr_t MK_GC_build_fl(struct hblk *h, size_t words, MK_GC_bool clear,
                           ptr_t list);
                                /* Build a free list for objects of     */
                                /* size sz in block h.  Append list to  */
                                /* end of the free lists.  Possibly     */
                                /* clear objects on the list.  Normally */
                                /* called by MK_GC_new_hblk, but also      */
                                /* called explicitly without GC lock.   */

MK_GC_INNER struct hblk * MK_GC_allochblk(size_t size_in_bytes, int kind,
                                    unsigned flags);
                                /* Allocate a heap block, inform        */
                                /* the marker that block is valid       */
                                /* for objects of indicated size.       */

MK_GC_INNER ptr_t MK_GC_alloc_large(size_t lb, int k, unsigned flags);
                        /* Allocate a large block of size lb bytes.     */
                        /* The block is not cleared.                    */
                        /* Flags is 0 or IGNORE_OFF_PAGE.               */
                        /* Calls MK_GC_allchblk to do the actual           */
                        /* allocation, but also triggers GC and/or      */
                        /* heap expansion as appropriate.               */
                        /* Does not update MK_GC_bytes_allocd, but does    */
                        /* other accounting.                            */

MK_GC_INNER void MK_GC_freehblk(struct hblk * p);
                                /* Deallocate a heap block and mark it  */
                                /* as invalid.                          */

/*  Misc GC: */
MK_GC_INNER MK_GC_bool MK_GC_expand_hp_inner(word n);
MK_GC_INNER void MK_GC_start_reclaim(MK_GC_bool abort_if_found);
                                /* Restore unmarked objects to free     */
                                /* lists, or (if abort_if_found is      */
                                /* TRUE) report them.                   */
                                /* Sweeping of small object pages is    */
                                /* largely deferred.                    */
MK_GC_INNER void MK_GC_continue_reclaim(size_t sz, int kind);
                                /* Sweep pages of the given size and    */
                                /* kind, as long as possible, and       */
                                /* as long as the corr. free list is    */
                                /* empty.  Sz is in granules.           */

MK_GC_INNER MK_GC_bool MK_GC_reclaim_all(MK_GC_stop_func stop_func, MK_GC_bool ignore_old);
                                /* Reclaim all blocks.  Abort (in a     */
                                /* consistent state) if f returns TRUE. */
MK_GC_INNER ptr_t MK_GC_reclaim_generic(struct hblk * hbp, hdr *hhdr, size_t sz,
                                  MK_GC_bool init, ptr_t list,
                                  signed_word *count);
                                /* Rebuild free list in hbp with        */
                                /* header hhdr, with objects of size sz */
                                /* bytes.  Add list to the end of the   */
                                /* free list.  Add the number of        */
                                /* reclaimed bytes to *count.           */
MK_GC_INNER MK_GC_bool MK_GC_block_empty(hdr * hhdr);
                                /* Block completely unmarked?   */
MK_GC_INNER int MK_GC_CALLBACK MK_GC_never_stop_func(void);
                                /* Always returns 0 (FALSE).            */
MK_GC_INNER MK_GC_bool MK_GC_try_to_collect_inner(MK_GC_stop_func f);

                                /* Collect; caller must have acquired   */
                                /* lock.  Collection is aborted if f    */
                                /* returns TRUE.  Returns TRUE if it    */
                                /* completes successfully.              */
#define MK_GC_gcollect_inner() \
                (void)MK_GC_try_to_collect_inner(MK_GC_never_stop_func)

MK_GC_EXTERN MK_GC_bool MK_GC_is_initialized; /* MK_GC_init() has been run. */

#if defined(MSWIN32) || defined(MSWINCE)
  void MK_GC_deinit(void);
                                /* Free any resources allocated by      */
                                /* MK_GC_init                              */
#endif

MK_GC_INNER void MK_GC_collect_a_little_inner(int n);
                                /* Do n units worth of garbage          */
                                /* collection work, if appropriate.     */
                                /* A unit is an amount appropriate for  */
                                /* HBLKSIZE bytes of allocation.        */

MK_GC_INNER void * MK_GC_generic_malloc_inner(size_t lb, int k);
                                /* Allocate an object of the given      */
                                /* kind but assuming lock already held. */
MK_GC_INNER void * MK_GC_generic_malloc_inner_ignore_off_page(size_t lb, int k);
                                /* Allocate an object, where            */
                                /* the client guarantees that there     */
                                /* will always be a pointer to the      */
                                /* beginning of the object while the    */
                                /* object is live.                      */

MK_GC_INNER ptr_t MK_GC_allocobj(size_t sz, int kind);
                                /* Make the indicated                   */
                                /* free list nonempty, and return its   */
                                /* head.  Sz is in granules.            */

#ifdef MK_GC_ADD_CALLER
  /* MK_GC_DBG_EXTRAS is used by GC debug API functions (unlike MK_GC_EXTRAS  */
  /* used by GC debug API macros) thus MK_GC_RETURN_ADDR_PARENT (pointing  */
  /* to client caller) should be used if possible.                      */
# ifdef MK_GC_RETURN_ADDR_PARENT
#  define MK_GC_DBG_EXTRAS MK_GC_RETURN_ADDR_PARENT, NULL, 0
# else
#  define MK_GC_DBG_EXTRAS MK_GC_RETURN_ADDR, NULL, 0
# endif
#else
# define MK_GC_DBG_EXTRAS "unknown", 0
#endif

/* We make the MK_GC_clear_stack() call a tail one, hoping to get more of  */
/* the stack.                                                           */
#define GENERAL_MALLOC(lb,k) \
    MK_GC_clear_stack(MK_GC_generic_malloc(lb, k))
#define GENERAL_MALLOC_IOP(lb,k) \
    MK_GC_clear_stack(MK_GC_generic_malloc_ignore_off_page(lb, k))

#ifdef MK_GC_COLLECT_AT_MALLOC
  extern size_t MK_GC_dbg_collect_at_malloc_min_lb;
                            /* variable visible outside for debugging   */
# define MK_GC_DBG_COLLECT_AT_MALLOC(lb) \
                (void)((lb) >= MK_GC_dbg_collect_at_malloc_min_lb ? \
                            (MK_GC_gcollect(), 0) : 0)
#else
# define MK_GC_DBG_COLLECT_AT_MALLOC(lb) (void)0
#endif /* !MK_GC_COLLECT_AT_MALLOC */

/* Allocation routines that bypass the thread local cache.      */
#ifdef THREAD_LOCAL_ALLOC
  MK_GC_INNER void * MK_GC_core_malloc(size_t);
  MK_GC_INNER void * MK_GC_core_malloc_atomic(size_t);
# ifdef MK_GC_GCJ_SUPPORT
    MK_GC_INNER void * MK_GC_core_gcj_malloc(size_t, void *);
# endif
#endif /* THREAD_LOCAL_ALLOC */

MK_GC_INNER void MK_GC_init_headers(void);
MK_GC_INNER struct hblkhdr * MK_GC_install_header(struct hblk *h);
                                /* Install a header for block h.        */
                                /* Return 0 on failure, or the header   */
                                /* otherwise.                           */
MK_GC_INNER MK_GC_bool MK_GC_install_counts(struct hblk * h, size_t sz);
                                /* Set up forwarding counts for block   */
                                /* h of size sz.                        */
                                /* Return FALSE on failure.             */
MK_GC_INNER void MK_GC_remove_header(struct hblk * h);
                                /* Remove the header for block h.       */
MK_GC_INNER void MK_GC_remove_counts(struct hblk * h, size_t sz);
                                /* Remove forwarding counts for h.      */
MK_GC_INNER hdr * MK_GC_find_header(ptr_t h);

MK_GC_INNER void MK_GC_add_to_heap(struct hblk *p, size_t bytes);
                        /* Add a HBLKSIZE aligned chunk to the heap.    */

#ifdef USE_PROC_FOR_LIBRARIES
  MK_GC_INNER void MK_GC_add_to_our_memory(ptr_t p, size_t bytes);
                        /* Add a chunk to MK_GC_our_memory.        */
                        /* If p == 0, do nothing.               */
#else
# define MK_GC_add_to_our_memory(p, bytes)
#endif

MK_GC_INNER void MK_GC_print_all_errors(void);
                        /* Print smashed and leaked objects, if any.    */
                        /* Clear the lists of such objects.             */

MK_GC_EXTERN void (*MK_GC_check_heap)(void);
                        /* Check that all objects in the heap with      */
                        /* debugging info are intact.                   */
                        /* Add any that are not to MK_GC_smashed list.     */
MK_GC_EXTERN void (*MK_GC_print_all_smashed)(void);
                        /* Print MK_GC_smashed if it's not empty.          */
                        /* Clear MK_GC_smashed list.                       */
MK_GC_EXTERN void (*MK_GC_print_heap_obj)(ptr_t p);
                        /* If possible print (using MK_GC_err_printf)      */
                        /* a more detailed description (terminated with */
                        /* "\n") of the object referred to by p.        */

#if defined(LINUX) && defined(__ELF__) && !defined(SMALL_CONFIG)
  void MK_GC_print_address_map(void);
                        /* Print an address map of the process.         */
#endif

#ifndef SHORT_DBG_HDRS
  MK_GC_EXTERN MK_GC_bool MK_GC_findleak_delay_free;
                        /* Do not immediately deallocate object on      */
                        /* free() in the leak-finding mode, just mark   */
                        /* it as freed (and deallocate it after GC).    */
  MK_GC_INNER MK_GC_bool MK_GC_check_leaked(ptr_t base); /* from dbg_mlc.c */
#endif

MK_GC_EXTERN MK_GC_bool MK_GC_have_errors; /* We saw a smashed or leaked object. */
                                  /* Call error printing routine        */
                                  /* occasionally.  It is OK to read it */
                                  /* without acquiring the lock.        */

#define VERBOSE 2
#ifndef SMALL_CONFIG
  /* MK_GC_print_stats should be visible to extra/MacOS.c. */
  extern int MK_GC_print_stats;    /* Nonzero generates basic GC log.      */
                                /* VERBOSE generates add'l messages.    */
#else /* SMALL_CONFIG */
# define MK_GC_print_stats 0
  /* Will this remove the message character strings from the executable? */
  /* With a particular level of optimizations, it should...              */
#endif

#ifdef KEEP_BACK_PTRS
  MK_GC_EXTERN long MK_GC_backtraces;
  MK_GC_INNER void MK_GC_generate_random_backtrace_no_gc(void);
#endif

MK_GC_EXTERN MK_GC_bool MK_GC_print_back_height;

#ifdef MAKE_BACK_GRAPH
  void MK_GC_print_back_graph_stats(void);
#endif

#ifdef THREADS
  MK_GC_INNER void MK_GC_free_inner(void * p);
#endif

/* Macros used for collector internal allocation.       */
/* These assume the collector lock is held.             */
#ifdef DBG_HDRS_ALL
  MK_GC_INNER void * MK_GC_debug_generic_malloc_inner(size_t lb, int k);
  MK_GC_INNER void * MK_GC_debug_generic_malloc_inner_ignore_off_page(size_t lb,
                                                                int k);
# define MK_GC_INTERNAL_MALLOC MK_GC_debug_generic_malloc_inner
# define MK_GC_INTERNAL_MALLOC_IGNORE_OFF_PAGE \
               MK_GC_debug_generic_malloc_inner_ignore_off_page
# ifdef THREADS
    MK_GC_INNER void MK_GC_debug_free_inner(void * p);
#   define MK_GC_INTERNAL_FREE MK_GC_debug_free_inner
# else
#   define MK_GC_INTERNAL_FREE MK_GC_debug_free
# endif
#else
# define MK_GC_INTERNAL_MALLOC MK_GC_generic_malloc_inner
# define MK_GC_INTERNAL_MALLOC_IGNORE_OFF_PAGE \
               MK_GC_generic_malloc_inner_ignore_off_page
# ifdef THREADS
#   define MK_GC_INTERNAL_FREE MK_GC_free_inner
# else
#   define MK_GC_INTERNAL_FREE MK_GC_free
# endif
#endif /* !DBG_HDRS_ALL */

#ifdef USE_MUNMAP
  /* Memory unmapping: */
  MK_GC_INNER void MK_GC_unmap_old(void);
  MK_GC_INNER void MK_GC_merge_unmapped(void);
  MK_GC_INNER void MK_GC_unmap(ptr_t start, size_t bytes);
  MK_GC_INNER void MK_GC_remap(ptr_t start, size_t bytes);
  MK_GC_INNER void MK_GC_unmap_gap(ptr_t start1, size_t bytes1, ptr_t start2,
                             size_t bytes2);
#endif

#ifdef CAN_HANDLE_FORK
  MK_GC_EXTERN int MK_GC_handle_fork;
                /* Fork-handling mode:                                  */
                /* 0 means no fork handling requested (but client could */
                /* anyway call fork() provided it is surrounded with    */
                /* MK_GC_atfork_prepare/parent/child calls);               */
                /* -1 means GC tries to use pthread_at_fork if it is    */
                /* available (if it succeeds then MK_GC_handle_fork value  */
                /* is changed to 1), client should nonetheless surround */
                /* fork() with MK_GC_atfork_prepare/parent/child (for the  */
                /* case of pthread_at_fork failure or absence);         */
                /* 1 (or other values) means client fully relies on     */
                /* pthread_at_fork (so if it is missing or failed then  */
                /* abort occurs in MK_GC_init), MK_GC_atfork_prepare and the  */
                /* accompanying routines are no-op in such a case.      */
#endif

#ifndef MK_GC_DISABLE_INCREMENTAL
  MK_GC_EXTERN MK_GC_bool MK_GC_dirty_maintained;
                                /* Dirty bits are being maintained,     */
                                /* either for incremental collection,   */
                                /* or to limit the root set.            */

  /* Virtual dirty bit implementation:            */
  /* Each implementation exports the following:   */
  MK_GC_INNER void MK_GC_read_dirty(void);
                        /* Retrieve dirty bits. */
  MK_GC_INNER MK_GC_bool MK_GC_page_was_dirty(struct hblk *h);
                        /* Read retrieved dirty bits.   */
  MK_GC_INNER void MK_GC_remove_protection(struct hblk *h, word nblocks,
                                   MK_GC_bool pointerfree);
                        /* h is about to be written or allocated.  Ensure   */
                        /* that it's not write protected by the virtual     */
                        /* dirty bit implementation.                        */

  MK_GC_INNER void MK_GC_dirty_init(void);
#endif /* !MK_GC_DISABLE_INCREMENTAL */

/* Same as MK_GC_base but excepts and returns a pointer to const object.   */
#define MK_GC_base_C(p) ((const void *)MK_GC_base((/* no const */ void *)(p)))

/* Stubborn objects: */
void MK_GC_read_changed(void); /* Analogous to MK_GC_read_dirty */
MK_GC_bool MK_GC_page_was_changed(struct hblk * h);
                                /* Analogous to MK_GC_page_was_dirty */
void MK_GC_clean_changing_list(void);
                                /* Collect obsolete changing list entries */
void MK_GC_stubborn_init(void);

/* Debugging print routines: */
void MK_GC_print_block_list(void);
void MK_GC_print_hblkfreelist(void);
void MK_GC_print_heap_sects(void);
void MK_GC_print_static_roots(void);
/* void MK_GC_dump(void); - declared in gc.h */

extern word MK_GC_fo_entries; /* should be visible in extra/MacOS.c */

#ifdef KEEP_BACK_PTRS
   MK_GC_INNER void MK_GC_store_back_pointer(ptr_t source, ptr_t dest);
   MK_GC_INNER void MK_GC_marked_for_finalization(ptr_t dest);
#  define MK_GC_STORE_BACK_PTR(source, dest) MK_GC_store_back_pointer(source, dest)
#  define MK_GC_MARKED_FOR_FINALIZATION(dest) MK_GC_marked_for_finalization(dest)
#else
#  define MK_GC_STORE_BACK_PTR(source, dest)
#  define MK_GC_MARKED_FOR_FINALIZATION(dest)
#endif

/* Make arguments appear live to compiler */
void MK_GC_noop6(word, word, word, word, word, word);

MK_GC_API void MK_GC_CALL MK_GC_noop1(word);

#ifndef MK_GC_ATTR_FORMAT_PRINTF
# if defined(__GNUC__) && __GNUC__ >= 3
#   define MK_GC_ATTR_FORMAT_PRINTF(spec_argnum, first_checked) \
        __attribute__((__format__(__printf__, spec_argnum, first_checked)))
# else
#   define MK_GC_ATTR_FORMAT_PRINTF(spec_argnum, first_checked)
# endif
#endif

/* Logging and diagnostic output:       */
/* MK_GC_printf is used typically on client explicit print requests.       */
/* For all MK_GC_X_printf routines, it is recommended to put "\n" at       */
/* 'format' string end (for output atomicity).                          */
MK_GC_API_PRIV void MK_GC_printf(const char * format, ...)
                        MK_GC_ATTR_FORMAT_PRINTF(1, 2);
                        /* A version of printf that doesn't allocate,   */
                        /* 1K total output length.                      */
                        /* (We use sprintf.  Hopefully that doesn't     */
                        /* allocate for long arguments.)                */
MK_GC_API_PRIV void MK_GC_err_printf(const char * format, ...)
                        MK_GC_ATTR_FORMAT_PRINTF(1, 2);

/* Basic logging routine.  Typically, MK_GC_log_printf is called directly  */
/* only inside various DEBUG_x blocks.                                  */
#if defined(__cplusplus) && defined(SYMBIAN)
  extern "C" {
#endif
MK_GC_API_PRIV void MK_GC_log_printf(const char * format, ...)
                        MK_GC_ATTR_FORMAT_PRINTF(1, 2);
#if defined(__cplusplus) && defined(SYMBIAN)
  }
#endif

#ifndef MK_GC_ANDROID_LOG
# define MK_GC_PRINT_STATS_FLAG (MK_GC_print_stats != 0)
# define MK_GC_INFOLOG_PRINTF MK_GC_COND_LOG_PRINTF
  /* MK_GC_verbose_log_printf is called only if MK_GC_print_stats is VERBOSE. */
# define MK_GC_verbose_log_printf MK_GC_log_printf
#else
  extern MK_GC_bool MK_GC_quiet;
# define MK_GC_PRINT_STATS_FLAG (!MK_GC_quiet)
  /* INFO/DBG loggers are enabled even if MK_GC_print_stats is off. */
# ifndef MK_GC_INFOLOG_PRINTF
#   define MK_GC_INFOLOG_PRINTF if (MK_GC_quiet) {} else MK_GC_info_log_printf
# endif
  MK_GC_INNER void MK_GC_info_log_printf(const char *format, ...)
                        MK_GC_ATTR_FORMAT_PRINTF(1, 2);
  MK_GC_INNER void MK_GC_verbose_log_printf(const char *format, ...)
                        MK_GC_ATTR_FORMAT_PRINTF(1, 2);
#endif /* MK_GC_ANDROID_LOG */

/* Convenient macros for MK_GC_[verbose_]log_printf invocation.    */
#define MK_GC_COND_LOG_PRINTF \
                if (EXPECT(!MK_GC_print_stats, TRUE)) {} else MK_GC_log_printf
#define MK_GC_VERBOSE_LOG_PRINTF \
    if (EXPECT(MK_GC_print_stats != VERBOSE, TRUE)) {} else MK_GC_verbose_log_printf
#ifndef MK_GC_DBGLOG_PRINTF
# define MK_GC_DBGLOG_PRINTF if (!MK_GC_PRINT_STATS_FLAG) {} else MK_GC_log_printf
#endif

void MK_GC_err_puts(const char *s);
                        /* Write s to stderr, don't buffer, don't add   */
                        /* newlines, don't ...                          */

/* Handy macro for logging size values (of word type) in KiB (rounding  */
/* to nearest value).                                                   */
#define TO_KiB_UL(v) ((unsigned long)(((v) + ((1 << 9) - 1)) >> 10))

MK_GC_EXTERN unsigned MK_GC_fail_count;
                        /* How many consecutive GC/expansion failures?  */
                        /* Reset by MK_GC_allochblk(); defined in alloc.c. */

MK_GC_EXTERN long MK_GC_large_alloc_warn_interval; /* defined in misc.c */

MK_GC_EXTERN signed_word MK_GC_bytes_found;
                /* Number of reclaimed bytes after garbage collection;  */
                /* protected by GC lock; defined in reclaim.c.          */

#ifndef MK_GC_GET_HEAP_USAGE_NOT_NEEDED
  MK_GC_EXTERN word MK_GC_reclaimed_bytes_before_gc;
                /* Number of bytes reclaimed before this        */
                /* collection cycle; used for statistics only.  */
#endif

#ifdef USE_MUNMAP
  MK_GC_EXTERN int MK_GC_unmap_threshold; /* defined in allchblk.c */
  MK_GC_EXTERN MK_GC_bool MK_GC_force_unmap_on_gcollect; /* defined in misc.c */
#endif

#ifdef MSWIN32
  MK_GC_EXTERN MK_GC_bool MK_GC_no_win32_dlls; /* defined in os_dep.c */
  MK_GC_EXTERN MK_GC_bool MK_GC_wnt;     /* Is Windows NT derivative;    */
                                /* defined and set in os_dep.c. */
#endif

#ifdef THREADS
# if defined(MSWIN32) || defined(MSWINCE)
    MK_GC_EXTERN CRITICAL_SECTION MK_GC_write_cs; /* defined in misc.c */
#   ifdef MK_GC_ASSERTIONS
      MK_GC_EXTERN MK_GC_bool MK_GC_write_disabled;
                                /* defined in win32_threads.c;  */
                                /* protected by MK_GC_write_cs.    */

#   endif
# endif
# ifdef MPROTECT_VDB
    MK_GC_EXTERN volatile MK_AO_TS_t MK_GC_fault_handler_lock;
                                        /* defined in os_dep.c */
# endif
# ifdef MSWINCE
    MK_GC_EXTERN MK_GC_bool MK_GC_dont_query_stack_min;
                                /* Defined and set in os_dep.c. */
# endif
#elif defined(IA64)
  MK_GC_EXTERN ptr_t MK_GC_save_regs_ret_val; /* defined in mach_dep.c. */
                        /* Previously set to backing store pointer.     */
#endif /* !THREADS */

#ifdef THREAD_LOCAL_ALLOC
  MK_GC_EXTERN MK_GC_bool MK_GC_world_stopped; /* defined in alloc.c */
  MK_GC_INNER void MK_GC_mark_thread_local_free_lists(void);
#endif

#ifdef MK_GC_GCJ_SUPPORT
# ifdef MK_GC_ASSERTIONS
    MK_GC_EXTERN MK_GC_bool MK_GC_gcj_malloc_initialized; /* defined in gcj_mlc.c */
# endif
  MK_GC_EXTERN ptr_t * MK_GC_gcjobjfreelist;
#endif

#if defined(GWW_VDB) && defined(MPROTECT_VDB)
  MK_GC_INNER MK_GC_bool MK_GC_gww_dirty_init(void);
  /* Defined in os_dep.c.  Returns TRUE if GetWriteWatch is available.  */
  /* May be called repeatedly.                                          */
#endif

#if defined(CHECKSUMS) || defined(PROC_VDB)
  MK_GC_INNER MK_GC_bool MK_GC_page_was_ever_dirty(struct hblk * h);
                        /* Could the page contain valid heap pointers?  */
#endif

MK_GC_INNER void MK_GC_default_print_heap_obj_proc(ptr_t p);

MK_GC_INNER void MK_GC_extend_size_map(size_t); /* in misc.c */

MK_GC_INNER void MK_GC_setpagesize(void);

MK_GC_INNER void MK_GC_initialize_offsets(void);      /* defined in obj_map.c */

MK_GC_INNER void MK_GC_bl_init(void);
MK_GC_INNER void MK_GC_bl_init_no_interiors(void);    /* defined in blacklst.c */

MK_GC_INNER void MK_GC_start_debugging(void); /* defined in dbg_mlc.c */

/* Store debugging info into p.  Return displaced pointer.      */
/* Assumes we don't hold allocation lock.                       */
MK_GC_INNER ptr_t MK_GC_store_debug_info(ptr_t p, word sz, const char *str,
                                   int linenum);

#ifdef REDIRECT_MALLOC
# ifdef MK_GC_LINUX_THREADS
    MK_GC_INNER MK_GC_bool MK_GC_text_mapping(char *nm, ptr_t *startp, ptr_t *endp);
                                                /* from os_dep.c */
# endif
#elif defined(USE_WINALLOC)
  MK_GC_INNER void MK_GC_add_current_malloc_heap(void);
#endif /* !REDIRECT_MALLOC */

#ifdef MAKE_BACK_GRAPH
  MK_GC_INNER void MK_GC_build_back_graph(void);
  MK_GC_INNER void MK_GC_traverse_back_graph(void);
#endif

#ifdef MSWIN32
  MK_GC_INNER void MK_GC_init_win32(void);
#endif

#if !defined(MSWIN32) && !defined(MSWINCE) && !defined(CYGWIN32)
  MK_GC_INNER void * MK_GC_roots_present(ptr_t);
        /* The type is a lie, since the real type doesn't make sense here, */
        /* and we only test for NULL.                                      */
#endif

#ifdef MK_GC_WIN32_THREADS
  MK_GC_INNER void MK_GC_get_next_stack(char *start, char * limit, char **lo,
                                  char **hi);
# ifdef MPROTECT_VDB
    MK_GC_INNER void MK_GC_set_write_fault_handler(void);
# endif
#endif /* MK_GC_WIN32_THREADS */

#ifdef THREADS
  MK_GC_INNER void MK_GC_reset_finalizer_nested(void);
  MK_GC_INNER unsigned char *MK_GC_check_finalizer_nested(void);
  MK_GC_INNER void MK_GC_do_blocking_inner(ptr_t data, void * context);
  MK_GC_INNER void MK_GC_push_all_stacks(void);
# ifdef USE_PROC_FOR_LIBRARIES
    MK_GC_INNER MK_GC_bool MK_GC_segment_is_thread_stack(ptr_t lo, ptr_t hi);
# endif
# ifdef IA64
    MK_GC_INNER ptr_t MK_GC_greatest_stack_base_below(ptr_t bound);
# endif
#endif /* THREADS */

#ifdef DYNAMIC_LOADING
  MK_GC_INNER MK_GC_bool MK_GC_register_main_static_data(void);
# ifdef DARWIN
    MK_GC_INNER void MK_GC_init_dyld(void);
# endif
#endif /* DYNAMIC_LOADING */

#ifdef SEARCH_FOR_DATA_START
  MK_GC_INNER void MK_GC_init_linux_data_start(void);
#endif

#if defined(NETBSD) && defined(__ELF__)
  MK_GC_INNER void MK_GC_init_netbsd_elf(void);
#endif

#ifdef UNIX_LIKE
  MK_GC_INNER void MK_GC_set_and_save_fault_handler(void (*handler)(int));
#endif

#ifdef NEED_PROC_MAPS
# if defined(DYNAMIC_LOADING) && defined(USE_PROC_FOR_LIBRARIES)
    MK_GC_INNER char *MK_GC_parse_map_entry(char *buf_ptr, ptr_t *start, ptr_t *end,
                                      char **prot, unsigned int *maj_dev,
                                      char **mapping_name);
# endif
  MK_GC_INNER char *MK_GC_get_maps(void); /* from os_dep.c */
#endif /* NEED_PROC_MAPS */

#ifdef MK_GC_ASSERTIONS
# define MK_GC_ASSERT(expr) \
              do { \
                if (!(expr)) { \
                  MK_GC_err_printf("Assertion failure: %s:%d\n", \
                                __FILE__, __LINE__); \
                  ABORT("assertion failure"); \
                } \
              } while (0)
  MK_GC_INNER word MK_GC_compute_large_free_bytes(void);
  MK_GC_INNER word MK_GC_compute_root_size(void);
#else
# define MK_GC_ASSERT(expr)
#endif

/* Check a compile time assertion at compile time.  The error   */
/* message for failure is a bit baroque, but ...                */
#if defined(mips) && !defined(__GNUC__)
/* DOB: MIPSPro C gets an internal error taking the sizeof an array type.
   This code works correctly (ugliness is to avoid "unused var" warnings) */
# define MK_GC_STATIC_ASSERT(expr) \
    do { if (0) { char j[(expr)? 1 : -1]; j[0]='\0'; j[0]=j[0]; } } while(0)
#else
# define MK_GC_STATIC_ASSERT(expr) (void)sizeof(char[(expr)? 1 : -1])
#endif

#define COND_DUMP_CHECKS \
          do { \
            MK_GC_ASSERT(MK_GC_compute_large_free_bytes() == MK_GC_large_free_bytes); \
            MK_GC_ASSERT(MK_GC_compute_root_size() == MK_GC_root_size); \
          } while (0)

#ifndef NO_DEBUGGING
  MK_GC_EXTERN MK_GC_bool MK_GC_dump_regularly;
                                /* Generate regular debugging dumps.    */
# define COND_DUMP if (EXPECT(MK_GC_dump_regularly, FALSE)) MK_GC_dump(); \
                        else COND_DUMP_CHECKS
#else
# define COND_DUMP COND_DUMP_CHECKS
#endif

#if defined(PARALLEL_MARK)
  /* We need additional synchronization facilities from the thread      */
  /* support.  We believe these are less performance critical           */
  /* than the main garbage collector lock; standard pthreads-based      */
  /* implementations should be sufficient.                              */

# define MK_GC_markers_m1 MK_GC_parallel
                        /* Number of mark threads we would like to have */
                        /* excluding the initiating thread.             */

  /* The mark lock and condition variable.  If the GC lock is also      */
  /* acquired, the GC lock must be acquired first.  The mark lock is    */
  /* used to both protect some variables used by the parallel           */
  /* marker, and to protect MK_GC_fl_builder_count, below.                 */
  /* MK_GC_notify_all_marker() is called when                              */
  /* the state of the parallel marker changes                           */
  /* in some significant way (see gc_mark.h for details).  The          */
  /* latter set of events includes incrementing MK_GC_mark_no.             */
  /* MK_GC_notify_all_builder() is called when MK_GC_fl_builder_count         */
  /* reaches 0.                                                         */

  MK_GC_INNER void MK_GC_acquire_mark_lock(void);
  MK_GC_INNER void MK_GC_release_mark_lock(void);
  MK_GC_INNER void MK_GC_notify_all_builder(void);
  MK_GC_INNER void MK_GC_wait_for_reclaim(void);

  MK_GC_EXTERN word MK_GC_fl_builder_count;   /* Protected by mark lock.      */

  MK_GC_INNER void MK_GC_notify_all_marker(void);
  MK_GC_INNER void MK_GC_wait_marker(void);
  MK_GC_EXTERN word MK_GC_mark_no;            /* Protected by mark lock.      */

  MK_GC_INNER void MK_GC_help_marker(word my_mark_no);
              /* Try to help out parallel marker for mark cycle         */
              /* my_mark_no.  Returns if the mark cycle finishes or     */
              /* was already done, or there was nothing to do for       */
              /* some other reason.                                     */
#endif /* PARALLEL_MARK */

#if defined(MK_GC_PTHREADS) && !defined(MK_GC_WIN32_THREADS) && !defined(NACL) \
    && !defined(SIG_SUSPEND)
  /* We define the thread suspension signal here, so that we can refer  */
  /* to it in the dirty bit implementation, if necessary.  Ideally we   */
  /* would allocate a (real-time?) signal using the standard mechanism. */
  /* unfortunately, there is no standard mechanism.  (There is one      */
  /* in Linux glibc, but it's not exported.)  Thus we continue to use   */
  /* the same hard-coded signals we've always used.                     */
# if defined(MK_GC_LINUX_THREADS) || defined(MK_GC_DGUX386_THREADS)
#   if defined(SPARC) && !defined(SIGPWR)
      /* SPARC/Linux doesn't properly define SIGPWR in <signal.h>.      */
      /* It is aliased to SIGLOST in asm/signal.h, though.              */
#     define SIG_SUSPEND SIGLOST
#   else
      /* Linuxthreads itself uses SIGUSR1 and SIGUSR2.                  */
#     define SIG_SUSPEND SIGPWR
#   endif
# elif defined(MK_GC_OPENBSD_THREADS)
#   ifndef MK_GC_OPENBSD_UTHREADS
#     define SIG_SUSPEND SIGXFSZ
#   endif
# elif !defined(MK_GC_DARWIN_THREADS)
#   if defined(_SIGRTMIN)
#     define SIG_SUSPEND _SIGRTMIN + 6
#   else
#     define SIG_SUSPEND SIGRTMIN + 6
#   endif
# endif
#endif /* MK_GC_PTHREADS && !SIG_SUSPEND */

#if defined(MK_GC_PTHREADS) && !defined(MK_GC_SEM_INIT_PSHARED)
# define MK_GC_SEM_INIT_PSHARED 0
#endif

#include <setjmp.h>

/* Some macros for setjmp that works across signal handlers     */
/* were possible, and a couple of routines to facilitate        */
/* catching accesses to bad addresses when that's               */
/* possible/needed.                                             */
#if (defined(UNIX_LIKE) || (defined(NEED_FIND_LIMIT) && defined(CYGWIN32))) \
    && !defined(MK_GC_NO_SIGSETJMP)
# if defined(SUNOS5SIGS) && !defined(FREEBSD) && !defined(LINUX)
#  include <sys/siginfo.h>
# endif
  /* Define SETJMP and friends to be the version that restores  */
  /* the signal mask.                                           */
# define SETJMP(env) sigsetjmp(env, 1)
# define LONGJMP(env, val) siglongjmp(env, val)
# define JMP_BUF sigjmp_buf
#else
# ifdef ECOS
#   define SETJMP(env) hal_setjmp(env)
# else
#   define SETJMP(env) setjmp(env)
# endif
# define LONGJMP(env, val) longjmp(env, val)
# define JMP_BUF jmp_buf
#endif /* !UNIX_LIKE || MK_GC_NO_SIGSETJMP */

/* Do we need the MK_GC_find_limit machinery to find the end of a  */
/* data segment.                                                */
#if defined(HEURISTIC2) || defined(SEARCH_FOR_DATA_START)
# define NEED_FIND_LIMIT
#endif

#if !defined(STACKBOTTOM) && defined(HEURISTIC2)
# define NEED_FIND_LIMIT
#endif

#if (defined(SVR4) || defined(AUX) || defined(DGUX) \
    || (defined(LINUX) && defined(SPARC))) && !defined(PCR)
# define NEED_FIND_LIMIT
#endif

#if defined(FREEBSD) && (defined(I386) || defined(X86_64) \
                        || defined(powerpc) || defined(__powerpc__))
# include <machine/trap.h>
# if !defined(PCR)
#   define NEED_FIND_LIMIT
# endif
#endif /* FREEBSD */

#if (defined(NETBSD) || defined(OPENBSD)) && defined(__ELF__) \
    && !defined(NEED_FIND_LIMIT)
  /* Used by MK_GC_init_netbsd_elf() in os_dep.c. */
# define NEED_FIND_LIMIT
#endif

#if defined(IA64) && !defined(NEED_FIND_LIMIT)
# define NEED_FIND_LIMIT
     /* May be needed for register backing store base. */
#endif

#if defined(NEED_FIND_LIMIT) \
     || (defined(USE_PROC_FOR_LIBRARIES) && defined(THREADS))
  JMP_BUF MK_GC_jmp_buf;

  /* Set up a handler for address faults which will longjmp to  */
  /* MK_GC_jmp_buf;                                                */
  MK_GC_INNER void MK_GC_setup_temporary_fault_handler(void);
  /* Undo the effect of MK_GC_setup_temporary_fault_handler.       */
  MK_GC_INNER void MK_GC_reset_fault_handler(void);
#endif /* NEED_FIND_LIMIT || USE_PROC_FOR_LIBRARIES */

/* Some convenience macros for cancellation support. */
#if defined(CANCEL_SAFE)
# if defined(MK_GC_ASSERTIONS) && (defined(USE_COMPILER_TLS) \
     || (defined(LINUX) && !defined(ARM32) \
              && (__GNUC__ > 3 || (__GNUC__ == 3 && __GNUC_MINOR__ >= 3)) \
     || defined(HPUX) /* and probably others ... */))
    extern __thread unsigned char MK_GC_cancel_disable_count;
#   define NEED_CANCEL_DISABLE_COUNT
#   define INCR_CANCEL_DISABLE() ++MK_GC_cancel_disable_count
#   define DECR_CANCEL_DISABLE() --MK_GC_cancel_disable_count
#   define ASSERT_CANCEL_DISABLED() MK_GC_ASSERT(MK_GC_cancel_disable_count > 0)
# else
#   define INCR_CANCEL_DISABLE()
#   define DECR_CANCEL_DISABLE()
#   define ASSERT_CANCEL_DISABLED() (void)0
# endif /* MK_GC_ASSERTIONS & ... */
# define DISABLE_CANCEL(state) \
        do { pthread_setcancelstate(PTHREAD_CANCEL_DISABLE, &state); \
          INCR_CANCEL_DISABLE(); } while (0)
# define RESTORE_CANCEL(state) \
        do { ASSERT_CANCEL_DISABLED(); \
          pthread_setcancelstate(state, NULL); \
          DECR_CANCEL_DISABLE(); } while (0)
#else /* !CANCEL_SAFE */
# define DISABLE_CANCEL(state) (void)0
# define RESTORE_CANCEL(state) (void)0
# define ASSERT_CANCEL_DISABLED() (void)0
#endif /* !CANCEL_SAFE */

#endif /* MK_GC_PRIVATE_H */
