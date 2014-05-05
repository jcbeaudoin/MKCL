/*
 * Copyright (c) 1991-1994 by Xerox Corporation.  All rights reserved.
 * Copyright (c) 1997 by Silicon Graphics.  All rights reserved.
 * Copyright (c) 2000 by Hewlett-Packard Company.  All rights reserved.
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

#include "private/gc_priv.h"

/* This used to be in dyn_load.c.  It was extracted into a separate     */
/* file to avoid having to link against libdl.{a,so} if the client      */
/* doesn't call dlopen.  Of course this fails if the collector is in    */
/* a dynamic library. -HB                                               */
#if defined(MK_GC_PTHREADS) && !defined(MK_GC_NO_DLOPEN)

#undef MK_GC_MUST_RESTORE_REDEFINED_DLOPEN
#if defined(dlopen) && !defined(MK_GC_USE_LD_WRAP)
  /* To support various threads pkgs, gc.h interposes on dlopen by      */
  /* defining "dlopen" to be "MK_GC_dlopen", which is implemented below.   */
  /* However, both MK_GC_FirstDLOpenedLinkMap() and MK_GC_dlopen() use the    */
  /* real system dlopen() in their implementation. We first remove      */
  /* gc.h's dlopen definition and restore it later, after MK_GC_dlopen().  */
# undef dlopen
# define MK_GC_MUST_RESTORE_REDEFINED_DLOPEN
#endif

/* Make sure we're not in the middle of a collection, and make sure we  */
/* don't start any.  This is invoked prior to a dlopen call to avoid    */
/* synchronization issues.  We can't just acquire the allocation lock,  */
/* since startup code in dlopen may try to allocate.  This solution     */
/* risks heap growth (or, even, heap overflow) in the presence of many  */
/* dlopen calls in either a multi-threaded environment, or if the       */
/* library initialization code allocates substantial amounts of GC'ed   */
/* memory.                                                              */
#ifndef USE_PROC_FOR_LIBRARIES
  static void disable_gc_for_dlopen(void)
  {
    DCL_LOCK_STATE;
    LOCK();
    while (MK_GC_incremental && MK_GC_collection_in_progress()) {
      MK_GC_collect_a_little_inner(1000);
    }
    ++MK_GC_dont_gc;
    UNLOCK();
  }
#endif

/* Redefine dlopen to guarantee mutual exclusion with           */
/* MK_GC_register_dynamic_libraries.  Should probably happen for   */
/* other operating systems, too.                                */

/* This is similar to WRAP/REAL_FUNC() in pthread_support.c.    */
#ifdef MK_GC_USE_LD_WRAP
# define WRAP_DLFUNC(f) __wrap_##f
# define REAL_DLFUNC(f) __real_##f
  void * REAL_DLFUNC(dlopen)(const char *, int);
#else
# define WRAP_DLFUNC(f) MK_GC_##f
# define REAL_DLFUNC(f) f
#endif

MK_GC_API void * WRAP_DLFUNC(dlopen)(const char *path, int mode)
{
  void * result;

# ifndef USE_PROC_FOR_LIBRARIES
    /* Disable collections.  This solution risks heap growth (or,       */
    /* even, heap overflow) but there seems no better solutions.        */
    disable_gc_for_dlopen();
# endif
  result = REAL_DLFUNC(dlopen)(path, mode);
# ifndef USE_PROC_FOR_LIBRARIES
    MK_GC_enable(); /* undoes disable_gc_for_dlopen */
# endif
  return(result);
}

#ifdef MK_GC_USE_LD_WRAP
  /* Define MK_GC_ function as an alias for the plain one, which will be   */
  /* intercepted.  This allows files which include gc.h, and hence      */
  /* generate references to the MK_GC_ symbol, to see the right symbol.    */
  MK_GC_API void *MK_GC_dlopen(const char *path, int mode)
  {
    return dlopen(path, mode);
  }
#endif /* MK_GC_USE_LD_WRAP */

#ifdef MK_GC_MUST_RESTORE_REDEFINED_DLOPEN
# define dlopen MK_GC_dlopen
#endif

#endif  /* MK_GC_PTHREADS && !MK_GC_NO_DLOPEN */
