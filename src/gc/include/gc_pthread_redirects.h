/*
 * Copyright (c) 1994 by Xerox Corporation.  All rights reserved.
 * Copyright (c) 1996 by Silicon Graphics.  All rights reserved.
 * Copyright (c) 1998 by Fergus Henderson.  All rights reserved.
 * Copyright (c) 2000-2010 by Hewlett-Packard Development Company.
 * All rights reserved.
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

/* Our pthread support normally needs to intercept a number of thread   */
/* calls.  We arrange to do that here, if appropriate.                  */

/* Included from gc.h only.  Included only if MK_GC_PTHREADS.              */
#if defined(MK_GC_H) && defined(MK_GC_PTHREADS)

/* We need to intercept calls to many of the threads primitives, so     */
/* that we can locate thread stacks and stop the world.                 */
/* Note also that the collector cannot always see thread specific data. */
/* Thread specific data should generally consist of pointers to         */
/* uncollectible objects (allocated with MK_GC_malloc_uncollectable,       */
/* not the system malloc), which are deallocated using the destructor   */
/* facility in thr_keycreate.  Alternatively, keep a redundant pointer  */
/* to thread specific data on the thread stack.                         */

#include <pthread.h>

#ifndef MK_GC_NO_DLOPEN
# include <dlfcn.h>
  MK_GC_API void *MK_GC_dlopen(const char * /* path */, int /* mode */);
#endif /* !MK_GC_NO_DLOPEN */

#ifndef MK_GC_NO_PTHREAD_SIGMASK
# include <signal.h>
  MK_GC_API int MK_GC_pthread_sigmask(int /* how */, const sigset_t *,
                                sigset_t * /* oset */);
#endif /* !MK_GC_NO_PTHREAD_SIGMASK */

#ifndef MK_GC_PTHREAD_CREATE_CONST
  /* This is used for pthread_create() only.    */
# define MK_GC_PTHREAD_CREATE_CONST const
#endif

MK_GC_API int MK_GC_pthread_create(pthread_t *,
                             MK_GC_PTHREAD_CREATE_CONST pthread_attr_t *,
                             void *(*)(void *), void * /* arg */);
MK_GC_API int MK_GC_pthread_join(pthread_t, void ** /* retval */);
MK_GC_API int MK_GC_pthread_detach(pthread_t);

#ifndef MK_GC_NO_PTHREAD_CANCEL
  MK_GC_API int MK_GC_pthread_cancel(pthread_t);
#endif

#if defined(MK_GC_PTHREAD_EXIT_ATTRIBUTE) && !defined(MK_GC_PTHREAD_EXIT_DECLARED)
# define MK_GC_PTHREAD_EXIT_DECLARED
  MK_GC_API void MK_GC_pthread_exit(void *) MK_GC_PTHREAD_EXIT_ATTRIBUTE;
#endif

#if !defined(MK_GC_NO_THREAD_REDIRECTS) && !defined(MK_GC_USE_LD_WRAP)
  /* Unless the compiler supports #pragma extern_prefix, the Tru64      */
  /* UNIX <pthread.h> redefines some POSIX thread functions to use      */
  /* mangled names.  Anyway, it's safe to undef them before redefining. */
# undef pthread_create
# undef pthread_join
# undef pthread_detach
# define pthread_create MK_GC_pthread_create
# define pthread_join MK_GC_pthread_join
# define pthread_detach MK_GC_pthread_detach

# ifndef MK_GC_NO_PTHREAD_SIGMASK
#   undef pthread_sigmask
#   define pthread_sigmask MK_GC_pthread_sigmask
# endif
# ifndef MK_GC_NO_DLOPEN
#   undef dlopen
#   define dlopen MK_GC_dlopen
# endif
# ifndef MK_GC_NO_PTHREAD_CANCEL
#   undef pthread_cancel
#   define pthread_cancel MK_GC_pthread_cancel
# endif
# ifdef MK_GC_PTHREAD_EXIT_ATTRIBUTE
#   undef pthread_exit
#   define pthread_exit MK_GC_pthread_exit
# endif
#endif /* !MK_GC_NO_THREAD_REDIRECTS */

#endif /* MK_GC_PTHREADS */
