/*
 * Copyright (c) 2003-2011 Hewlett-Packard Development Company, L.P.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

/*
 * Generalize atomic operations for atomic_ops.h.
 * Should not be included directly.
 *
 * We make no attempt to define useless operations, such as
 * MK_AO_nop_acquire
 * MK_AO_nop_release
 *
 * We have also so far neglected to define some others, which
 * do not appear likely to be useful, e.g. stores with acquire
 * or read barriers.
 *
 * This file is sometimes included twice by atomic_ops.h.
 * All definitions include explicit checks that we are not replacing
 * an earlier definition.  In general, more desirable expansions
 * appear earlier so that we are more likely to use them.
 *
 * We only make safe generalizations, except that by default we define
 * the ...dd_acquire_read operations to be equivalent to those without
 * a barrier.  On platforms for which this is unsafe, the platform-specific
 * file must define MK_AO_NO_DD_ORDERING.
 */

#ifndef MK_AO_ATOMIC_OPS_H
# error This file should not be included directly.
#endif

/* Generate test_and_set_full, if necessary and possible.       */
#if !defined(MK_AO_HAVE_test_and_set) && !defined(MK_AO_HAVE_test_and_set_release) \
    && !defined(MK_AO_HAVE_test_and_set_acquire) \
    && !defined(MK_AO_HAVE_test_and_set_read) \
    && !defined(MK_AO_HAVE_test_and_set_full)

  /* Emulate MK_AO_compare_and_swap() via MK_AO_fetch_compare_and_swap().     */
# if defined(MK_AO_HAVE_fetch_compare_and_swap) \
     && !defined(MK_AO_HAVE_compare_and_swap)
    MK_AO_INLINE int
    MK_AO_compare_and_swap(volatile MK_AO_t *addr, MK_AO_t old_val, MK_AO_t new_val)
    {
      return MK_AO_fetch_compare_and_swap(addr, old_val, new_val) == old_val;
    }
#   define MK_AO_HAVE_compare_and_swap
# endif

# if defined(MK_AO_HAVE_fetch_compare_and_swap_full) \
     && !defined(MK_AO_HAVE_compare_and_swap_full)
    MK_AO_INLINE int
    MK_AO_compare_and_swap_full(volatile MK_AO_t *addr, MK_AO_t old_val, MK_AO_t new_val)
    {
      return MK_AO_fetch_compare_and_swap_full(addr, old_val, new_val)
               == old_val;
    }
#   define MK_AO_HAVE_compare_and_swap_full
# endif

# if defined(MK_AO_HAVE_fetch_compare_and_swap_acquire) \
     && !defined(MK_AO_HAVE_compare_and_swap_acquire)
    MK_AO_INLINE int
    MK_AO_compare_and_swap_acquire(volatile MK_AO_t *addr, MK_AO_t old_val,
                                MK_AO_t new_val)
    {
      return MK_AO_fetch_compare_and_swap_acquire(addr, old_val, new_val)
               == old_val;
    }
#   define MK_AO_HAVE_compare_and_swap_acquire
# endif

# if defined(MK_AO_HAVE_fetch_compare_and_swap_release) \
     && !defined(MK_AO_HAVE_compare_and_swap_release)
    MK_AO_INLINE int
    MK_AO_compare_and_swap_release(volatile MK_AO_t *addr, MK_AO_t old_val,
                                MK_AO_t new_val)
    {
      return MK_AO_fetch_compare_and_swap_release(addr, old_val, new_val)
               == old_val;
    }
#   define MK_AO_HAVE_compare_and_swap_release
# endif

# if MK_AO_CHAR_TS_T
#   define MK_AO_TS_COMPARE_AND_SWAP_FULL(a,o,n) \
                                MK_AO_char_compare_and_swap_full(a,o,n)
#   define MK_AO_TS_COMPARE_AND_SWAP_ACQUIRE(a,o,n) \
                                MK_AO_char_compare_and_swap_acquire(a,o,n)
#   define MK_AO_TS_COMPARE_AND_SWAP_RELEASE(a,o,n) \
                                MK_AO_char_compare_and_swap_release(a,o,n)
#   define MK_AO_TS_COMPARE_AND_SWAP(a,o,n) MK_AO_char_compare_and_swap(a,o,n)
# endif

# if MK_AO_MK_AO_TS_T
#   define MK_AO_TS_COMPARE_AND_SWAP_FULL(a,o,n) MK_AO_compare_and_swap_full(a,o,n)
#   define MK_AO_TS_COMPARE_AND_SWAP_ACQUIRE(a,o,n) \
                                MK_AO_compare_and_swap_acquire(a,o,n)
#   define MK_AO_TS_COMPARE_AND_SWAP_RELEASE(a,o,n) \
                                MK_AO_compare_and_swap_release(a,o,n)
#   define MK_AO_TS_COMPARE_AND_SWAP(a,o,n) MK_AO_compare_and_swap(a,o,n)
# endif

# if (MK_AO_MK_AO_TS_T && defined(MK_AO_HAVE_compare_and_swap_full)) \
     || (MK_AO_CHAR_TS_T && defined(MK_AO_HAVE_char_compare_and_swap_full))
    MK_AO_INLINE MK_AO_TS_VAL_t
    MK_AO_test_and_set_full(volatile MK_AO_TS_t *addr)
    {
      if (MK_AO_TS_COMPARE_AND_SWAP_FULL(addr, MK_AO_TS_CLEAR, MK_AO_TS_SET))
        return MK_AO_TS_CLEAR;
      else
        return MK_AO_TS_SET;
    }
#   define MK_AO_HAVE_test_and_set_full
# endif /* MK_AO_HAVE_compare_and_swap_full */

# if (MK_AO_MK_AO_TS_T && defined(MK_AO_HAVE_compare_and_swap_acquire)) \
     || (MK_AO_CHAR_TS_T && defined(MK_AO_HAVE_char_compare_and_swap_acquire))
    MK_AO_INLINE MK_AO_TS_VAL_t
    MK_AO_test_and_set_acquire(volatile MK_AO_TS_t *addr)
    {
      if (MK_AO_TS_COMPARE_AND_SWAP_ACQUIRE(addr, MK_AO_TS_CLEAR, MK_AO_TS_SET))
        return MK_AO_TS_CLEAR;
      else
        return MK_AO_TS_SET;
    }
#   define MK_AO_HAVE_test_and_set_acquire
# endif /* MK_AO_HAVE_compare_and_swap_acquire */

# if (MK_AO_MK_AO_TS_T && defined(MK_AO_HAVE_compare_and_swap_release)) \
     || (MK_AO_CHAR_TS_T && defined(MK_AO_HAVE_char_compare_and_swap_release))
    MK_AO_INLINE MK_AO_TS_VAL_t
    MK_AO_test_and_set_release(volatile MK_AO_TS_t *addr)
    {
      if (MK_AO_TS_COMPARE_AND_SWAP_RELEASE(addr, MK_AO_TS_CLEAR, MK_AO_TS_SET))
        return MK_AO_TS_CLEAR;
      else
        return MK_AO_TS_SET;
    }
#   define MK_AO_HAVE_test_and_set_release
# endif /* MK_AO_HAVE_compare_and_swap_release */

# if (MK_AO_MK_AO_TS_T && defined(MK_AO_HAVE_compare_and_swap)) \
     || (MK_AO_CHAR_TS_T && defined(MK_AO_HAVE_char_compare_and_swap))
    MK_AO_INLINE MK_AO_TS_VAL_t
    MK_AO_test_and_set(volatile MK_AO_TS_t *addr)
    {
      if (MK_AO_TS_COMPARE_AND_SWAP(addr, MK_AO_TS_CLEAR, MK_AO_TS_SET))
        return MK_AO_TS_CLEAR;
      else
        return MK_AO_TS_SET;
    }
#   define MK_AO_HAVE_test_and_set
# endif /* MK_AO_HAVE_compare_and_swap */
#endif /* No prior test and set */

/* Nop */
#if !defined(MK_AO_HAVE_nop)
  MK_AO_INLINE void MK_AO_nop(void) {}
# define MK_AO_HAVE_nop
#endif

#if defined(MK_AO_HAVE_test_and_set_full) && !defined(MK_AO_HAVE_nop_full)
  MK_AO_INLINE void
  MK_AO_nop_full(void)
  {
    MK_AO_TS_t dummy = MK_AO_TS_INITIALIZER;
    MK_AO_test_and_set_full(&dummy);
  }
# define MK_AO_HAVE_nop_full
#endif

#if defined(MK_AO_HAVE_nop_acquire)
# error MK_AO_nop_acquire is useless: dont define.
#endif
#if defined(MK_AO_HAVE_nop_release)
# error MK_AO_nop_release is useless: dont define.
#endif

#if defined(MK_AO_HAVE_nop_full) && !defined(MK_AO_HAVE_nop_read)
# define MK_AO_nop_read() MK_AO_nop_full()
# define MK_AO_HAVE_nop_read
#endif

#if defined(MK_AO_HAVE_nop_full) && !defined(MK_AO_HAVE_nop_write)
# define MK_AO_nop_write() MK_AO_nop_full()
# define MK_AO_HAVE_nop_write
#endif

/* Test_and_set */
#if defined(MK_AO_HAVE_test_and_set) && defined(MK_AO_HAVE_nop_full) \
    && !defined(MK_AO_HAVE_test_and_set_release)
# define MK_AO_test_and_set_release(addr) (MK_AO_nop_full(), MK_AO_test_and_set(addr))
# define MK_AO_HAVE_test_and_set_release
#endif

#if defined(MK_AO_HAVE_test_and_set) && defined(MK_AO_HAVE_nop_full) \
    && !defined(MK_AO_HAVE_test_and_set_acquire)
  MK_AO_INLINE MK_AO_TS_VAL_t
  MK_AO_test_and_set_acquire(volatile MK_AO_TS_t *addr)
  {
    MK_AO_TS_VAL_t result = MK_AO_test_and_set(addr);
    MK_AO_nop_full();
    return result;
  }
# define MK_AO_HAVE_test_and_set_acquire
#endif

#if defined(MK_AO_HAVE_test_and_set_full)
# if !defined(MK_AO_HAVE_test_and_set_release)
#   define MK_AO_test_and_set_release(addr) MK_AO_test_and_set_full(addr)
#   define MK_AO_HAVE_test_and_set_release
# endif
# if !defined(MK_AO_HAVE_test_and_set_acquire)
#   define MK_AO_test_and_set_acquire(addr) MK_AO_test_and_set_full(addr)
#   define MK_AO_HAVE_test_and_set_acquire
# endif
# if !defined(MK_AO_HAVE_test_and_set_write)
#   define MK_AO_test_and_set_write(addr) MK_AO_test_and_set_full(addr)
#   define MK_AO_HAVE_test_and_set_write
# endif
# if !defined(MK_AO_HAVE_test_and_set_read)
#   define MK_AO_test_and_set_read(addr) MK_AO_test_and_set_full(addr)
#   define MK_AO_HAVE_test_and_set_read
# endif
#endif /* MK_AO_HAVE_test_and_set_full */

#if !defined(MK_AO_HAVE_test_and_set) && defined(MK_AO_HAVE_test_and_set_release)
# define MK_AO_test_and_set(addr) MK_AO_test_and_set_release(addr)
# define MK_AO_HAVE_test_and_set
#endif
#if !defined(MK_AO_HAVE_test_and_set) && defined(MK_AO_HAVE_test_and_set_acquire)
# define MK_AO_test_and_set(addr) MK_AO_test_and_set_acquire(addr)
# define MK_AO_HAVE_test_and_set
#endif
#if !defined(MK_AO_HAVE_test_and_set) && defined(MK_AO_HAVE_test_and_set_write)
# define MK_AO_test_and_set(addr) MK_AO_test_and_set_write(addr)
# define MK_AO_HAVE_test_and_set
#endif
#if !defined(MK_AO_HAVE_test_and_set) && defined(MK_AO_HAVE_test_and_set_read)
# define MK_AO_test_and_set(addr) MK_AO_test_and_set_read(addr)
# define MK_AO_HAVE_test_and_set
#endif

#if defined(MK_AO_HAVE_test_and_set_acquire) && defined(MK_AO_HAVE_nop_full) \
    && !defined(MK_AO_HAVE_test_and_set_full)
# define MK_AO_test_and_set_full(addr) \
                        (MK_AO_nop_full(), MK_AO_test_and_set_acquire(addr))
# define MK_AO_HAVE_test_and_set_full
#endif

#if !defined(MK_AO_HAVE_test_and_set_release_write) \
    && defined(MK_AO_HAVE_test_and_set_write)
# define MK_AO_test_and_set_release_write(addr) MK_AO_test_and_set_write(addr)
# define MK_AO_HAVE_test_and_set_release_write
#endif
#if !defined(MK_AO_HAVE_test_and_set_release_write) \
    && defined(MK_AO_HAVE_test_and_set_release)
# define MK_AO_test_and_set_release_write(addr) MK_AO_test_and_set_release(addr)
# define MK_AO_HAVE_test_and_set_release_write
#endif
#if !defined(MK_AO_HAVE_test_and_set_acquire_read) \
    && defined(MK_AO_HAVE_test_and_set_read)
# define MK_AO_test_and_set_acquire_read(addr) MK_AO_test_and_set_read(addr)
# define MK_AO_HAVE_test_and_set_acquire_read
#endif
#if !defined(MK_AO_HAVE_test_and_set_acquire_read) \
    && defined(MK_AO_HAVE_test_and_set_acquire)
# define MK_AO_test_and_set_acquire_read(addr) MK_AO_test_and_set_acquire(addr)
# define MK_AO_HAVE_test_and_set_acquire_read
#endif

#ifdef MK_AO_NO_DD_ORDERING
# if defined(MK_AO_HAVE_test_and_set_acquire_read)
#   define MK_AO_test_and_set_dd_acquire_read(addr) \
                                        MK_AO_test_and_set_acquire_read(addr)
#   define MK_AO_HAVE_test_and_set_dd_acquire_read
# endif
#else
# if defined(MK_AO_HAVE_test_and_set)
#   define MK_AO_test_and_set_dd_acquire_read(addr) MK_AO_test_and_set(addr)
#   define MK_AO_HAVE_test_and_set_dd_acquire_read
# endif
#endif /* !MK_AO_NO_DD_ORDERING */

#include "generalize-small.h"

#include "generalize-arithm.h"

/* Compare_double_and_swap_double based on double_compare_and_swap.     */
#ifdef MK_AO_HAVE_DOUBLE_PTR_STORAGE
# if defined(MK_AO_HAVE_double_compare_and_swap) \
     && !defined(MK_AO_HAVE_compare_double_and_swap_double)
   MK_AO_INLINE int
   MK_AO_compare_double_and_swap_double(volatile MK_AO_double_t *addr,
                                     MK_AO_t old_val1, MK_AO_t old_val2,
                                     MK_AO_t new_val1, MK_AO_t new_val2)
   {
     MK_AO_double_t old_w;
     MK_AO_double_t new_w;
     old_w.MK_AO_val1 = old_val1;
     old_w.MK_AO_val2 = old_val2;
     new_w.MK_AO_val1 = new_val1;
     new_w.MK_AO_val2 = new_val2;
     return MK_AO_double_compare_and_swap(addr, old_w, new_w);
   }
#  define MK_AO_HAVE_compare_double_and_swap_double
# endif
# if defined(MK_AO_HAVE_double_compare_and_swap_full) \
     && !defined(MK_AO_HAVE_compare_double_and_swap_double_full)
    MK_AO_INLINE int
    MK_AO_compare_double_and_swap_double_full(volatile MK_AO_double_t *addr,
                                           MK_AO_t old_val1, MK_AO_t old_val2,
                                           MK_AO_t new_val1, MK_AO_t new_val2)
    {
      MK_AO_double_t old_w;
      MK_AO_double_t new_w;
      old_w.MK_AO_val1 = old_val1;
      old_w.MK_AO_val2 = old_val2;
      new_w.MK_AO_val1 = new_val1;
      new_w.MK_AO_val2 = new_val2;
      return MK_AO_double_compare_and_swap_full(addr, old_w, new_w);
    }
#   define MK_AO_HAVE_compare_double_and_swap_double_full
# endif
#endif /* MK_AO_HAVE_DOUBLE_PTR_STORAGE */

/* Compare_double_and_swap_double */
#if defined(MK_AO_HAVE_compare_double_and_swap_double) \
    && defined(MK_AO_HAVE_nop_full) \
    && !defined(MK_AO_HAVE_compare_double_and_swap_double_acquire)
  MK_AO_INLINE int
  MK_AO_compare_double_and_swap_double_acquire(volatile MK_AO_double_t *addr,
                                            MK_AO_t o1, MK_AO_t o2,
                                            MK_AO_t n1, MK_AO_t n2)
  {
    int result = MK_AO_compare_double_and_swap_double(addr, o1, o2, n1, n2);
    MK_AO_nop_full();
    return result;
  }
# define MK_AO_HAVE_compare_double_and_swap_double_acquire
#endif
#if defined(MK_AO_HAVE_compare_double_and_swap_double) \
    && defined(MK_AO_HAVE_nop_full) \
    && !defined(MK_AO_HAVE_compare_double_and_swap_double_release)
# define MK_AO_compare_double_and_swap_double_release(addr,o1,o2,n1,n2) \
      (MK_AO_nop_full(), MK_AO_compare_double_and_swap_double(addr,o1,o2,n1,n2))
# define MK_AO_HAVE_compare_double_and_swap_double_release
#endif
#if defined(MK_AO_HAVE_compare_double_and_swap_double_full)
# if !defined(MK_AO_HAVE_compare_double_and_swap_double_release)
#   define MK_AO_compare_double_and_swap_double_release(addr,o1,o2,n1,n2) \
                MK_AO_compare_double_and_swap_double_full(addr,o1,o2,n1,n2)
#   define MK_AO_HAVE_compare_double_and_swap_double_release
# endif
# if !defined(MK_AO_HAVE_compare_double_and_swap_double_acquire)
#   define MK_AO_compare_double_and_swap_double_acquire(addr,o1,o2,n1,n2) \
                MK_AO_compare_double_and_swap_double_full(addr,o1,o2,n1,n2)
#   define MK_AO_HAVE_compare_double_and_swap_double_acquire
# endif
# if !defined(MK_AO_HAVE_compare_double_and_swap_double_write)
#   define MK_AO_compare_double_and_swap_double_write(addr,o1,o2,n1,n2) \
                MK_AO_compare_double_and_swap_double_full(addr,o1,o2,n1,n2)
#   define MK_AO_HAVE_compare_double_and_swap_double_write
# endif
# if !defined(MK_AO_HAVE_compare_double_and_swap_double_read)
#   define MK_AO_compare_double_and_swap_double_read(addr,o1,o2,n1,n2) \
                MK_AO_compare_double_and_swap_double_full(addr,o1,o2,n1,n2)
#   define MK_AO_HAVE_compare_double_and_swap_double_read
# endif
#endif /* MK_AO_HAVE_compare_double_and_swap_double_full */

#if !defined(MK_AO_HAVE_compare_double_and_swap_double) \
    && defined(MK_AO_HAVE_compare_double_and_swap_double_release)
# define MK_AO_compare_double_and_swap_double(addr,o1,o2,n1,n2) \
                MK_AO_compare_double_and_swap_double_release(addr,o1,o2,n1,n2)
# define MK_AO_HAVE_compare_double_and_swap_double
#endif
#if !defined(MK_AO_HAVE_compare_double_and_swap_double) \
    && defined(MK_AO_HAVE_compare_double_and_swap_double_acquire)
# define MK_AO_compare_double_and_swap_double(addr,o1,o2,n1,n2) \
                MK_AO_compare_double_and_swap_double_acquire(addr,o1,o2,n1,n2)
# define MK_AO_HAVE_compare_double_and_swap_double
#endif
#if !defined(MK_AO_HAVE_compare_double_and_swap_double) \
    && defined(MK_AO_HAVE_compare_double_and_swap_double_write)
# define MK_AO_compare_double_and_swap_double(addr,o1,o2,n1,n2) \
                MK_AO_compare_double_and_swap_double_write(addr,o1,o2,n1,n2)
# define MK_AO_HAVE_compare_double_and_swap_double
#endif
#if !defined(MK_AO_HAVE_compare_double_and_swap_double) \
    && defined(MK_AO_HAVE_compare_double_and_swap_double_read)
# define MK_AO_compare_double_and_swap_double(addr,o1,o2,n1,n2) \
                MK_AO_compare_double_and_swap_double_read(addr,o1,o2,n1,n2)
# define MK_AO_HAVE_compare_double_and_swap_double
#endif

#if defined(MK_AO_HAVE_compare_double_and_swap_double_acquire) \
    && defined(MK_AO_HAVE_nop_full) \
    && !defined(MK_AO_HAVE_compare_double_and_swap_double_full)
# define MK_AO_compare_double_and_swap_double_full(addr,o1,o2,n1,n2) \
                (MK_AO_nop_full(), \
                 MK_AO_compare_double_and_swap_double_acquire(addr,o1,o2,n1,n2))
# define MK_AO_HAVE_compare_double_and_swap_double_full
#endif

#if !defined(MK_AO_HAVE_compare_double_and_swap_double_release_write) \
    && defined(MK_AO_HAVE_compare_double_and_swap_double_write)
# define MK_AO_compare_double_and_swap_double_release_write(addr,o1,o2,n1,n2) \
                MK_AO_compare_double_and_swap_double_write(addr,o1,o2,n1,n2)
# define MK_AO_HAVE_compare_double_and_swap_double_release_write
#endif
#if !defined(MK_AO_HAVE_compare_double_and_swap_double_release_write) \
    && defined(MK_AO_HAVE_compare_double_and_swap_double_release)
# define MK_AO_compare_double_and_swap_double_release_write(addr,o1,o2,n1,n2) \
                MK_AO_compare_double_and_swap_double_release(addr,o1,o2,n1,n2)
# define MK_AO_HAVE_compare_double_and_swap_double_release_write
#endif
#if !defined(MK_AO_HAVE_compare_double_and_swap_double_acquire_read) \
    && defined(MK_AO_HAVE_compare_double_and_swap_double_read)
# define MK_AO_compare_double_and_swap_double_acquire_read(addr,o1,o2,n1,n2) \
                MK_AO_compare_double_and_swap_double_read(addr,o1,o2,n1,n2)
# define MK_AO_HAVE_compare_double_and_swap_double_acquire_read
#endif
#if !defined(MK_AO_HAVE_compare_double_and_swap_double_acquire_read) \
    && defined(MK_AO_HAVE_compare_double_and_swap_double_acquire)
# define MK_AO_compare_double_and_swap_double_acquire_read(addr,o1,o2,n1,n2) \
                MK_AO_compare_double_and_swap_double_acquire(addr,o1,o2,n1,n2)
# define MK_AO_HAVE_compare_double_and_swap_double_acquire_read
#endif

#ifdef MK_AO_NO_DD_ORDERING
# if defined(MK_AO_HAVE_compare_double_and_swap_double_acquire_read)
#   define MK_AO_compare_double_and_swap_double_dd_acquire_read(addr,o1,o2,n1,n2) \
             MK_AO_compare_double_and_swap_double_acquire_read(addr,o1,o2,n1,n2)
#   define MK_AO_HAVE_compare_double_and_swap_double_dd_acquire_read
# endif
#else
# if defined(MK_AO_HAVE_compare_double_and_swap_double)
#   define MK_AO_compare_double_and_swap_double_dd_acquire_read(addr,o1,o2,n1,n2) \
                        MK_AO_compare_double_and_swap_double(addr,o1,o2,n1,n2)
#   define MK_AO_HAVE_compare_double_and_swap_double_dd_acquire_read
# endif
#endif /* !MK_AO_NO_DD_ORDERING */

/* Compare_and_swap_double */
#if defined(MK_AO_HAVE_compare_and_swap_double) && defined(MK_AO_HAVE_nop_full) \
    && !defined(MK_AO_HAVE_compare_and_swap_double_acquire)
  MK_AO_INLINE int
  MK_AO_compare_and_swap_double_acquire(volatile MK_AO_double_t *addr,
                                            MK_AO_t o1,
                                            MK_AO_t n1, MK_AO_t n2)
  {
    int result = MK_AO_compare_and_swap_double(addr, o1, n1, n2);
    MK_AO_nop_full();
    return result;
  }
# define MK_AO_HAVE_compare_and_swap_double_acquire
#endif
#if defined(MK_AO_HAVE_compare_and_swap_double) \
    && defined(MK_AO_HAVE_nop_full) \
    && !defined(MK_AO_HAVE_compare_and_swap_double_release)
# define MK_AO_compare_and_swap_double_release(addr,o1,n1,n2) \
                (MK_AO_nop_full(), MK_AO_compare_and_swap_double(addr,o1,n1,n2))
# define MK_AO_HAVE_compare_and_swap_double_release
#endif
#if defined(MK_AO_HAVE_compare_and_swap_double_full)
# if !defined(MK_AO_HAVE_compare_and_swap_double_release)
#   define MK_AO_compare_and_swap_double_release(addr,o1,n1,n2) \
                                MK_AO_compare_and_swap_double_full(addr,o1,n1,n2)
#   define MK_AO_HAVE_compare_and_swap_double_release
# endif
# if !defined(MK_AO_HAVE_compare_and_swap_double_acquire)
#   define MK_AO_compare_and_swap_double_acquire(addr,o1,n1,n2) \
                                MK_AO_compare_and_swap_double_full(addr,o1,n1,n2)
#   define MK_AO_HAVE_compare_and_swap_double_acquire
# endif
# if !defined(MK_AO_HAVE_compare_and_swap_double_write)
#   define MK_AO_compare_and_swap_double_write(addr,o1,n1,n2) \
                                MK_AO_compare_and_swap_double_full(addr,o1,n1,n2)
#   define MK_AO_HAVE_compare_and_swap_double_write
# endif
# if !defined(MK_AO_HAVE_compare_and_swap_double_read)
#   define MK_AO_compare_and_swap_double_read(addr,o1,n1,n2) \
                                MK_AO_compare_and_swap_double_full(addr,o1,n1,n2)
#   define MK_AO_HAVE_compare_and_swap_double_read
# endif
#endif /* MK_AO_HAVE_compare_and_swap_double_full */

#if !defined(MK_AO_HAVE_compare_and_swap_double) \
    && defined(MK_AO_HAVE_compare_and_swap_double_release)
# define MK_AO_compare_and_swap_double(addr,o1,n1,n2) \
                        MK_AO_compare_and_swap_double_release(addr,o1,n1,n2)
# define MK_AO_HAVE_compare_and_swap_double
#endif
#if !defined(MK_AO_HAVE_compare_and_swap_double) \
    && defined(MK_AO_HAVE_compare_and_swap_double_acquire)
# define MK_AO_compare_and_swap_double(addr,o1,n1,n2) \
                        MK_AO_compare_and_swap_double_acquire(addr,o1,n1,n2)
# define MK_AO_HAVE_compare_and_swap_double
#endif
#if !defined(MK_AO_HAVE_compare_and_swap_double) \
    && defined(MK_AO_HAVE_compare_and_swap_double_write)
# define MK_AO_compare_and_swap_double(addr,o1,n1,n2) \
                        MK_AO_compare_and_swap_double_write(addr,o1,n1,n2)
# define MK_AO_HAVE_compare_and_swap_double
#endif
#if !defined(MK_AO_HAVE_compare_and_swap_double) \
    && defined(MK_AO_HAVE_compare_and_swap_double_read)
# define MK_AO_compare_and_swap_double(addr,o1,n1,n2) \
                        MK_AO_compare_and_swap_double_read(addr,o1,n1,n2)
# define MK_AO_HAVE_compare_and_swap_double
#endif

#if defined(MK_AO_HAVE_compare_and_swap_double_acquire) \
    && defined(MK_AO_HAVE_nop_full) \
    && !defined(MK_AO_HAVE_compare_and_swap_double_full)
# define MK_AO_compare_and_swap_double_full(addr,o1,n1,n2) \
        (MK_AO_nop_full(), MK_AO_compare_and_swap_double_acquire(addr,o1,n1,n2))
# define MK_AO_HAVE_compare_and_swap_double_full
#endif

#if !defined(MK_AO_HAVE_compare_and_swap_double_release_write) \
    && defined(MK_AO_HAVE_compare_and_swap_double_write)
# define MK_AO_compare_and_swap_double_release_write(addr,o1,n1,n2) \
                        MK_AO_compare_and_swap_double_write(addr,o1,n1,n2)
# define MK_AO_HAVE_compare_and_swap_double_release_write
#endif
#if !defined(MK_AO_HAVE_compare_and_swap_double_release_write) \
    && defined(MK_AO_HAVE_compare_and_swap_double_release)
# define MK_AO_compare_and_swap_double_release_write(addr,o1,n1,n2) \
                        MK_AO_compare_and_swap_double_release(addr,o1,n1,n2)
# define MK_AO_HAVE_compare_and_swap_double_release_write
#endif
#if !defined(MK_AO_HAVE_compare_and_swap_double_acquire_read) \
    && defined(MK_AO_HAVE_compare_and_swap_double_read)
# define MK_AO_compare_and_swap_double_acquire_read(addr,o1,n1,n2) \
                        MK_AO_compare_and_swap_double_read(addr,o1,n1,n2)
# define MK_AO_HAVE_compare_and_swap_double_acquire_read
#endif
#if !defined(MK_AO_HAVE_compare_and_swap_double_acquire_read) \
    && defined(MK_AO_HAVE_compare_and_swap_double_acquire)
# define MK_AO_compare_and_swap_double_acquire_read(addr,o1,n1,n2) \
                        MK_AO_compare_and_swap_double_acquire(addr,o1,n1,n2)
# define MK_AO_HAVE_compare_and_swap_double_acquire_read
#endif

#ifdef MK_AO_NO_DD_ORDERING
# if defined(MK_AO_HAVE_compare_and_swap_double_acquire_read)
#   define MK_AO_compare_and_swap_double_dd_acquire_read(addr,o1,n1,n2) \
                        MK_AO_compare_and_swap_double_acquire_read(addr,o1,n1,n2)
#   define MK_AO_HAVE_compare_and_swap_double_dd_acquire_read
# endif
#else
# if defined(MK_AO_HAVE_compare_and_swap_double)
#   define MK_AO_compare_and_swap_double_dd_acquire_read(addr,o1,n1,n2) \
                        MK_AO_compare_and_swap_double(addr,o1,n1,n2)
#   define MK_AO_HAVE_compare_and_swap_double_dd_acquire_read
# endif
#endif

/* Convenience functions for MK_AO_double compare-and-swap which types and */
/* reads easier in code.                                                */
#if defined(MK_AO_HAVE_compare_double_and_swap_double) \
    && !defined(MK_AO_HAVE_double_compare_and_swap)
  MK_AO_INLINE int
  MK_AO_double_compare_and_swap(volatile MK_AO_double_t *addr,
                             MK_AO_double_t old_val, MK_AO_double_t new_val)
  {
    return MK_AO_compare_double_and_swap_double(addr,
                                        old_val.MK_AO_val1, old_val.MK_AO_val2,
                                        new_val.MK_AO_val1, new_val.MK_AO_val2);
  }
# define MK_AO_HAVE_double_compare_and_swap
#endif
#if defined(MK_AO_HAVE_compare_double_and_swap_double_release) \
    && !defined(MK_AO_HAVE_double_compare_and_swap_release)
  MK_AO_INLINE int
  MK_AO_double_compare_and_swap_release(volatile MK_AO_double_t *addr,
                                     MK_AO_double_t old_val, MK_AO_double_t new_val)
  {
    return MK_AO_compare_double_and_swap_double_release(addr,
                                          old_val.MK_AO_val1, old_val.MK_AO_val2,
                                          new_val.MK_AO_val1, new_val.MK_AO_val2);
  }
# define MK_AO_HAVE_double_compare_and_swap_release
#endif
#if defined(MK_AO_HAVE_compare_double_and_swap_double_acquire) \
    && !defined(MK_AO_HAVE_double_compare_and_swap_acquire)
  MK_AO_INLINE int
  MK_AO_double_compare_and_swap_acquire(volatile MK_AO_double_t *addr,
                                     MK_AO_double_t old_val, MK_AO_double_t new_val)
  {
    return MK_AO_compare_double_and_swap_double_acquire(addr,
                                          old_val.MK_AO_val1, old_val.MK_AO_val2,
                                          new_val.MK_AO_val1, new_val.MK_AO_val2);
  }
# define MK_AO_HAVE_double_compare_and_swap_acquire
#endif
#if defined(MK_AO_HAVE_compare_double_and_swap_double_read) \
    && !defined(MK_AO_HAVE_double_compare_and_swap_read)
  MK_AO_INLINE int
  MK_AO_double_compare_and_swap_read(volatile MK_AO_double_t *addr,
                                  MK_AO_double_t old_val, MK_AO_double_t new_val)
  {
    return MK_AO_compare_double_and_swap_double_read(addr,
                                          old_val.MK_AO_val1, old_val.MK_AO_val2,
                                          new_val.MK_AO_val1, new_val.MK_AO_val2);
  }
# define MK_AO_HAVE_double_compare_and_swap_read
#endif
#if defined(MK_AO_HAVE_compare_double_and_swap_double_write) \
    && !defined(MK_AO_HAVE_double_compare_and_swap_write)
  MK_AO_INLINE int
  MK_AO_double_compare_and_swap_write(volatile MK_AO_double_t *addr,
                                   MK_AO_double_t old_val, MK_AO_double_t new_val)
  {
    return MK_AO_compare_double_and_swap_double_write(addr,
                                          old_val.MK_AO_val1, old_val.MK_AO_val2,
                                          new_val.MK_AO_val1, new_val.MK_AO_val2);
  }
# define MK_AO_HAVE_double_compare_and_swap_write
#endif
#if defined(MK_AO_HAVE_compare_double_and_swap_double_release_write) \
    && !defined(MK_AO_HAVE_double_compare_and_swap_release_write)
  MK_AO_INLINE int
  MK_AO_double_compare_and_swap_release_write(volatile MK_AO_double_t *addr,
                                MK_AO_double_t old_val, MK_AO_double_t new_val)
  {
    return MK_AO_compare_double_and_swap_double_release_write(addr,
                                          old_val.MK_AO_val1, old_val.MK_AO_val2,
                                          new_val.MK_AO_val1, new_val.MK_AO_val2);
  }
# define MK_AO_HAVE_double_compare_and_swap_release_write
#endif
#if defined(MK_AO_HAVE_compare_double_and_swap_double_acquire_read) \
    && !defined(MK_AO_HAVE_double_compare_and_swap_acquire_read)
  MK_AO_INLINE int
  MK_AO_double_compare_and_swap_acquire_read(volatile MK_AO_double_t *addr,
                                MK_AO_double_t old_val, MK_AO_double_t new_val)
  {
    return MK_AO_compare_double_and_swap_double_acquire_read(addr,
                                          old_val.MK_AO_val1, old_val.MK_AO_val2,
                                          new_val.MK_AO_val1, new_val.MK_AO_val2);
  }
# define MK_AO_HAVE_double_compare_and_swap_acquire_read
#endif
#if defined(MK_AO_HAVE_compare_double_and_swap_double_full) \
    && !defined(MK_AO_HAVE_double_compare_and_swap_full)
  MK_AO_INLINE int
  MK_AO_double_compare_and_swap_full(volatile MK_AO_double_t *addr,
                                  MK_AO_double_t old_val, MK_AO_double_t new_val)
  {
    return MK_AO_compare_double_and_swap_double_full(addr,
                                          old_val.MK_AO_val1, old_val.MK_AO_val2,
                                          new_val.MK_AO_val1, new_val.MK_AO_val2);
  }
# define MK_AO_HAVE_double_compare_and_swap_full
#endif
