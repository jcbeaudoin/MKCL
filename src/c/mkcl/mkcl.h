/* -*- mode: c -*- */
/*
    mkcl.h -- Main headers for use and/or development of MKCL
*/
/*
    Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
    Copyright (c) 1990, Giuseppe Attardi.
    Copyright (c) 2010-2019, Jean-Claude Beaudoin.

    MKCL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 3 of the License, or (at your option) any later version.

    See file './Copyright' for full details.
*/

#ifndef MKCL_H
#define MKCL_H

#ifdef __MINGW32__
/* These defines have to be done before any system include. */
# define _GNU_SOURCE 1
# define _XOPEN_SOURCE 700 /* The maximum useful value since 2008 it seems. JCB */
/* For WINVER, Windows XP is 0x0501, Windows XP_SP2 is 0x502,
               Windows Vista is 0x0600, Windows 7 is 0x0601, Windows 8 is 0x0602. */
# define WINVER 0x0502 /* Don't want to support below that. JCB */
# ifndef __MSVCRT_VERSION__
/*  High byte is the major version, low byte is the minor. */
#  define __MSVCRT_VERSION__ 0x0700 /* Matches Windows XP */ /* Don't want to support below that. JCB */
# endif
#endif

#include <sys/types.h>

#include <stddef.h>		/* NULL, ptrdiff_t */
#include <stdarg.h> 		/* va_list */
#include <setjmp.h> 		/* setjmp and buffers */
#include <errno.h>
#include <stdlib.h>
#include <signal.h>		/* sigset_t */


#include <mkcl/config.h>

#if MKCL_WINDOWS
# define WIN32_LEAN_AND_MEAN 1 /* Do not include winsock.h */
# ifndef WINVER
#  define WINVER 0x0502 /* We require at least Windows XP_SP2 or later. */
# endif
# ifndef _WIN32_WINNT
#  define _WIN32_WINNT WINVER
# endif
# include <winsock2.h>
# include <windows.h>
# include <malloc.h> /* for alloca() */
typedef HANDLE mkcl_os_process_t;
typedef DWORD mkcl_exit_code_t;
typedef HANDLE mkcl_os_thread_t;
typedef HANDLE mkcl_os_mutex_ref;
# if defined(_MSC_VER) && (defined(WinVista) || defined(Win7))
typedef SRWLOCK mkcl_os_rwlock_ref; /* MingW does not support this yet it seems. JCB 2010/02/15 */
# else
typedef HANDLE mkcl_os_rwlock_ref;
# endif
#else /* !MKCL_WINDOWS */
# include <semaphore.h>
typedef pthread_t mkcl_os_thread_t;
typedef pthread_mutex_t * mkcl_os_mutex_ref;
typedef pthread_rwlock_t * mkcl_os_rwlock_ref;
typedef pid_t mkcl_os_process_t;
typedef int mkcl_exit_code_t;
#endif /* !MKCL_WINDOWS */

#ifndef __GNUC__
# define mkcl_likely(expr) (expr)
# define mkcl_unlikely(expr) (expr)
# define mkcl_noreturn
#else
# define mkcl_likely(expr) __builtin_expect((expr), TRUE)
# define mkcl_unlikely(expr) __builtin_expect((expr), FALSE)
# define mkcl_noreturn __attribute__((noreturn))
# if (__GNUC__ < 4)
#  error MKCL expects GCC version 4.X.X or later
# endif
#endif


#include <mkcl/object.h>
#include <mkcl/number.h>
#include <mkcl/list.h>
#include <mkcl/external.h>
#include <mkcl/stacks.h>

#endif /* MKCL_H */
