/*
 * Copyright (c) 1991-1994 by Xerox Corporation.  All rights reserved.
 * Copyright (c) 1996-1999 by Silicon Graphics.  All rights reserved.
 * Copyright (c) 2003-2011 Hewlett-Packard Development Company, L.P.
 *
 *
 * THIS MATERIAL IS PROVIDED AS IS, WITH ABSOLUTELY NO WARRANTY EXPRESSED
 * OR IMPLIED.  ANY USE IS AT YOUR OWN RISK.
 *
 * Permission is hereby granted to use or copy this program
 * for any purpose, provided the above notices are retained on all copies.
 * Permission to modify the code and to distribute modified code is granted,
 * provided the above notices are retained, and a notice that the code was
 * modified is included with the above copyright notice.
 *
 */

/* The following implementation assumes GCC 4.7 or later.               */
/* For the details, see GNU Manual, chapter 6.52 (Built-in functions    */
/* for memory model aware atomic operations).                           */

/* TODO: Include this file for other targets if gcc 4.7+ */

#ifdef MK_AO_UNIPROCESSOR
  /* If only a single processor (core) is used, MK_AO_UNIPROCESSOR could   */
  /* be defined by the client to avoid unnecessary memory barrier.      */
  MK_AO_INLINE void
  MK_AO_nop_full(void)
  {
    MK_AO_compiler_barrier();
  }
# define MK_AO_HAVE_nop_full

#else
  MK_AO_INLINE void
  MK_AO_nop_read(void)
  {
    __atomic_thread_fence(__ATOMIC_ACQUIRE);
  }
# define MK_AO_HAVE_nop_read

# ifndef MK_AO_HAVE_nop_write
    MK_AO_INLINE void
    MK_AO_nop_write(void)
    {
      __atomic_thread_fence(__ATOMIC_RELEASE);
    }
#   define MK_AO_HAVE_nop_write
# endif

  MK_AO_INLINE void
  MK_AO_nop_full(void)
  {
    /* __sync_synchronize() could be used instead.      */
    __atomic_thread_fence(__ATOMIC_SEQ_CST);
  }
# define MK_AO_HAVE_nop_full
#endif /* !MK_AO_UNIPROCESSOR */

#include "generic-small.h"

#ifndef MK_AO_PREFER_GENERALIZED
# include "generic-arithm.h"

  MK_AO_INLINE MK_AO_TS_VAL_t
  MK_AO_test_and_set(volatile MK_AO_TS_t *addr)
  {
    return (MK_AO_TS_VAL_t)__atomic_test_and_set(addr, __ATOMIC_RELAXED);
  }
# define MK_AO_HAVE_test_and_set

  MK_AO_INLINE MK_AO_TS_VAL_t
  MK_AO_test_and_set_acquire(volatile MK_AO_TS_t *addr)
  {
    return (MK_AO_TS_VAL_t)__atomic_test_and_set(addr, __ATOMIC_ACQUIRE);
  }
# define MK_AO_HAVE_test_and_set_acquire

  MK_AO_INLINE MK_AO_TS_VAL_t
  MK_AO_test_and_set_release(volatile MK_AO_TS_t *addr)
  {
    return (MK_AO_TS_VAL_t)__atomic_test_and_set(addr, __ATOMIC_RELEASE);
  }
# define MK_AO_HAVE_test_and_set_release

  MK_AO_INLINE MK_AO_TS_VAL_t
  MK_AO_test_and_set_full(volatile MK_AO_TS_t *addr)
  {
    return (MK_AO_TS_VAL_t)__atomic_test_and_set(addr, __ATOMIC_SEQ_CST);
  }
# define MK_AO_HAVE_test_and_set_full
#endif /* !MK_AO_PREFER_GENERALIZED */

#ifdef MK_AO_HAVE_DOUBLE_PTR_STORAGE

# ifndef MK_AO_HAVE_double_load
    MK_AO_INLINE MK_AO_double_t
    MK_AO_double_load(const volatile MK_AO_double_t *addr)
    {
      MK_AO_double_t result;

      result.MK_AO_whole = __atomic_load_n(&addr->MK_AO_whole, __ATOMIC_RELAXED);
      return result;
    }
#   define MK_AO_HAVE_double_load
# endif

# ifndef MK_AO_HAVE_double_load_acquire
    MK_AO_INLINE MK_AO_double_t
    MK_AO_double_load_acquire(const volatile MK_AO_double_t *addr)
    {
      MK_AO_double_t result;

      result.MK_AO_whole = __atomic_load_n(&addr->MK_AO_whole, __ATOMIC_ACQUIRE);
      return result;
    }
#   define MK_AO_HAVE_double_load_acquire
# endif

# ifndef MK_AO_HAVE_double_store
    MK_AO_INLINE void
    MK_AO_double_store(volatile MK_AO_double_t *addr, MK_AO_double_t value)
    {
      __atomic_store_n(&addr->MK_AO_whole, value.MK_AO_whole, __ATOMIC_RELAXED);
    }
#   define MK_AO_HAVE_double_store
# endif

# ifndef MK_AO_HAVE_double_store_release
    MK_AO_INLINE void
    MK_AO_double_store_release(volatile MK_AO_double_t *addr, MK_AO_double_t value)
    {
      __atomic_store_n(&addr->MK_AO_whole, value.MK_AO_whole, __ATOMIC_RELEASE);
    }
#   define MK_AO_HAVE_double_store_release
# endif

# ifndef MK_AO_HAVE_double_compare_and_swap
    MK_AO_INLINE int
    MK_AO_double_compare_and_swap(volatile MK_AO_double_t *addr,
                               MK_AO_double_t old_val, MK_AO_double_t new_val)
    {
      return (int)__atomic_compare_exchange_n(&addr->MK_AO_whole,
                                  &old_val.MK_AO_whole /* p_expected */,
                                  new_val.MK_AO_whole /* desired */,
                                  0 /* is_weak: false */,
                                  __ATOMIC_RELAXED /* success */,
                                  __ATOMIC_RELAXED /* failure */);
    }
#   define MK_AO_HAVE_double_compare_and_swap
# endif

  /* TODO: Add double CAS _acquire/release/full primitives. */
#endif /* MK_AO_HAVE_DOUBLE_PTR_STORAGE */
