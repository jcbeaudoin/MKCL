/*
 * Copyright (c) 2003-2004 Hewlett-Packard Development Company, L.P.
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

#ifndef ATOMIC_OPS_H
# error Atomic_ops_generalize.h should not be included directly.
#endif

#if MK_AO_CHAR_TS_T
# define MK_AO_TS_COMPARE_AND_SWAP_FULL(a,o,n) \
                                MK_AO_char_compare_and_swap_full(a,o,n)
# define MK_AO_TS_COMPARE_AND_SWAP_ACQUIRE(a,o,n) \
                                MK_AO_char_compare_and_swap_acquire(a,o,n)
# define MK_AO_TS_COMPARE_AND_SWAP_RELEASE(a,o,n) \
                                MK_AO_char_compare_and_swap_release(a,o,n)
# define MK_AO_TS_COMPARE_AND_SWAP(a,o,n) MK_AO_char_compare_and_swap(a,o,n)
#endif

#if MK_AO_MK_AO_TS_T
# define MK_AO_TS_COMPARE_AND_SWAP_FULL(a,o,n) MK_AO_compare_and_swap_full(a,o,n)
# define MK_AO_TS_COMPARE_AND_SWAP_ACQUIRE(a,o,n) \
                                        MK_AO_compare_and_swap_acquire(a,o,n)
# define MK_AO_TS_COMPARE_AND_SWAP_RELEASE(a,o,n) \
                                        MK_AO_compare_and_swap_release(a,o,n)
# define MK_AO_TS_COMPARE_AND_SWAP(a,o,n) MK_AO_compare_and_swap(a,o,n)
#endif

/* Generate test_and_set_full, if necessary and possible.       */
#if !defined(MK_AO_HAVE_test_and_set) && !defined(MK_AO_HAVE_test_and_set_release) \
    && !defined(MK_AO_HAVE_test_and_set_acquire) \
    && !defined(MK_AO_HAVE_test_and_set_read) \
    && !defined(MK_AO_HAVE_test_and_set_full)
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

# if defined(MK_AO_HAVE_test_and_set) && defined(MK_AO_HAVE_nop_full) \
     && !defined(MK_AO_HAVE_test_and_set_acquire)
    MK_AO_INLINE MK_AO_TS_VAL_t
    MK_AO_test_and_set_acquire(volatile MK_AO_TS_t *addr)
    {
      MK_AO_TS_VAL_t result = MK_AO_test_and_set(addr);
      MK_AO_nop_full();
      return result;
    }
#   define MK_AO_HAVE_test_and_set_acquire
# endif
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

/* Load */
#if defined(MK_AO_HAVE_load_full) && !defined(MK_AO_HAVE_load_acquire)
# define MK_AO_load_acquire(addr) MK_AO_load_full(addr)
# define MK_AO_HAVE_load_acquire
#endif

#if defined(MK_AO_HAVE_load_acquire) && !defined(MK_AO_HAVE_load)
# define MK_AO_load(addr) MK_AO_load_acquire(addr)
# define MK_AO_HAVE_load
#endif

#if defined(MK_AO_HAVE_load_full) && !defined(MK_AO_HAVE_load_read)
# define MK_AO_load_read(addr) MK_AO_load_full(addr)
# define MK_AO_HAVE_load_read
#endif

#if !defined(MK_AO_HAVE_load_acquire_read) && defined(MK_AO_HAVE_load_acquire)
# define MK_AO_load_acquire_read(addr) MK_AO_load_acquire(addr)
# define MK_AO_HAVE_load_acquire_read
#endif

#if defined(MK_AO_HAVE_load) && defined(MK_AO_HAVE_nop_full) \
    && !defined(MK_AO_HAVE_load_acquire)
  MK_AO_INLINE MK_AO_t
  MK_AO_load_acquire(const volatile MK_AO_t *addr)
  {
    MK_AO_t result = MK_AO_load(addr);
    /* Acquire barrier would be useless, since the load could be delayed */
    /* beyond it.                                                        */
    MK_AO_nop_full();
    return result;
  }
# define MK_AO_HAVE_load_acquire
#endif

#if defined(MK_AO_HAVE_load) && defined(MK_AO_HAVE_nop_read) \
    && !defined(MK_AO_HAVE_load_read)
  MK_AO_INLINE MK_AO_t
  MK_AO_load_read(const volatile MK_AO_t *addr)
  {
    MK_AO_t result = MK_AO_load(addr);
    /* Acquire barrier would be useless, since the load could be delayed */
    /* beyond it.                                                        */
    MK_AO_nop_read();
    return result;
  }
# define MK_AO_HAVE_load_read
#endif

#if defined(MK_AO_HAVE_load_acquire) && defined(MK_AO_HAVE_nop_full) \
    && !defined(MK_AO_HAVE_load_full)
# define MK_AO_load_full(addr) (MK_AO_nop_full(), MK_AO_load_acquire(addr))
# define MK_AO_HAVE_load_full
#endif

#if !defined(MK_AO_HAVE_load_acquire_read) && defined(MK_AO_HAVE_load_read)
# define MK_AO_load_acquire_read(addr) MK_AO_load_read(addr)
# define MK_AO_HAVE_load_acquire_read
#endif

#if defined(MK_AO_HAVE_load_acquire_read) && !defined(MK_AO_HAVE_load)
# define MK_AO_load(addr) MK_AO_load_acquire_read(addr)
# define MK_AO_HAVE_load
#endif

#ifdef MK_AO_NO_DD_ORDERING
# if defined(MK_AO_HAVE_load_acquire_read)
#   define MK_AO_load_dd_acquire_read(addr) MK_AO_load_acquire_read(addr)
#   define MK_AO_HAVE_load_dd_acquire_read
# endif
#else
# if defined(MK_AO_HAVE_load)
#   define MK_AO_load_dd_acquire_read(addr) MK_AO_load(addr)
#   define MK_AO_HAVE_load_dd_acquire_read
# endif
#endif

/* Store */
#if defined(MK_AO_HAVE_store_full) && !defined(MK_AO_HAVE_store_release)
# define MK_AO_store_release(addr,val) MK_AO_store_full(addr,val)
# define MK_AO_HAVE_store_release
#endif

#if defined(MK_AO_HAVE_store_release) && !defined(MK_AO_HAVE_store)
# define MK_AO_store(addr,val) MK_AO_store_release(addr,val)
# define MK_AO_HAVE_store
#endif

#if defined(MK_AO_HAVE_store_full) && !defined(MK_AO_HAVE_store_write)
# define MK_AO_store_write(addr,val) MK_AO_store_full(addr,val)
# define MK_AO_HAVE_store_write
#endif

#if defined(MK_AO_HAVE_store_release) && !defined(MK_AO_HAVE_store_release_write)
# define MK_AO_store_release_write(addr,val) MK_AO_store_release(addr,val)
# define MK_AO_HAVE_store_release_write
#endif

#if defined(MK_AO_HAVE_store_write) && !defined(MK_AO_HAVE_store)
# define MK_AO_store(addr,val) MK_AO_store_write(addr,val)
# define MK_AO_HAVE_store
#endif

#if defined(MK_AO_HAVE_store) && defined(MK_AO_HAVE_nop_full) \
    && !defined(MK_AO_HAVE_store_release)
# define MK_AO_store_release(addr,val) (MK_AO_nop_full(), MK_AO_store(addr,val))
# define MK_AO_HAVE_store_release
#endif

#if defined(MK_AO_HAVE_nop_write) && defined(MK_AO_HAVE_store) \
    && !defined(MK_AO_HAVE_store_write)
# define MK_AO_store_write(addr,val) (MK_AO_nop_write(), MK_AO_store(addr,val))
# define MK_AO_HAVE_store_write
#endif

#if defined(MK_AO_HAVE_store_write) && !defined(MK_AO_HAVE_store_release_write)
# define MK_AO_store_release_write(addr,val) MK_AO_store_write(addr,val)
# define MK_AO_HAVE_store_release_write
#endif

#if defined(MK_AO_HAVE_store_release) && defined(MK_AO_HAVE_nop_full) \
    && !defined(MK_AO_HAVE_store_full)
# define MK_AO_store_full(addr,val) (MK_AO_store_release(addr,val), MK_AO_nop_full())
# define MK_AO_HAVE_store_full
#endif

/* NEC LE-IT: Test and set */
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

/* Fetch_and_add */
/* We first try to implement fetch_and_add variants in terms    */
/* of the corresponding compare_and_swap variants to minimize   */
/* adding barriers.                                             */
#if defined(MK_AO_HAVE_compare_and_swap_full) \
    && !defined(MK_AO_HAVE_fetch_and_add_full)
  MK_AO_INLINE MK_AO_t
  MK_AO_fetch_and_add_full(volatile MK_AO_t *addr, MK_AO_t incr)
  {
    MK_AO_t old;
    do
      {
        old = *addr;
      }
    while (!MK_AO_compare_and_swap_full(addr, old, old+incr));
    return old;
  }
# define MK_AO_HAVE_fetch_and_add_full
#endif

#if defined(MK_AO_HAVE_compare_and_swap_acquire) \
    && !defined(MK_AO_HAVE_fetch_and_add_acquire)
  MK_AO_INLINE MK_AO_t
  MK_AO_fetch_and_add_acquire(volatile MK_AO_t *addr, MK_AO_t incr)
  {
    MK_AO_t old;
    do
      {
        old = *addr;
      }
    while (!MK_AO_compare_and_swap_acquire(addr, old, old+incr));
    return old;
  }
# define MK_AO_HAVE_fetch_and_add_acquire
#endif

#if defined(MK_AO_HAVE_compare_and_swap_release) \
    && !defined(MK_AO_HAVE_fetch_and_add_release)
  MK_AO_INLINE MK_AO_t
  MK_AO_fetch_and_add_release(volatile MK_AO_t *addr, MK_AO_t incr)
  {
    MK_AO_t old;
    do
      {
        old = *addr;
      }
    while (!MK_AO_compare_and_swap_release(addr, old, old+incr));
    return old;
  }
# define MK_AO_HAVE_fetch_and_add_release
#endif

#if defined(MK_AO_HAVE_compare_and_swap) && !defined(MK_AO_HAVE_fetch_and_add)
  MK_AO_INLINE MK_AO_t
  MK_AO_fetch_and_add(volatile MK_AO_t *addr, MK_AO_t incr)
  {
    MK_AO_t old;
    do
      {
        old = *addr;
      }
    while (!MK_AO_compare_and_swap(addr, old, old+incr));
    return old;
  }
# define MK_AO_HAVE_fetch_and_add
#endif

#if defined(MK_AO_HAVE_fetch_and_add_full)
# if !defined(MK_AO_HAVE_fetch_and_add_release)
#   define MK_AO_fetch_and_add_release(addr,val) MK_AO_fetch_and_add_full(addr,val)
#   define MK_AO_HAVE_fetch_and_add_release
# endif
# if !defined(MK_AO_HAVE_fetch_and_add_acquire)
#   define MK_AO_fetch_and_add_acquire(addr,val) MK_AO_fetch_and_add_full(addr,val)
#   define MK_AO_HAVE_fetch_and_add_acquire
# endif
# if !defined(MK_AO_HAVE_fetch_and_add_write)
#   define MK_AO_fetch_and_add_write(addr,val) MK_AO_fetch_and_add_full(addr,val)
#   define MK_AO_HAVE_fetch_and_add_write
# endif
# if !defined(MK_AO_HAVE_fetch_and_add_read)
#   define MK_AO_fetch_and_add_read(addr,val) MK_AO_fetch_and_add_full(addr,val)
#   define MK_AO_HAVE_fetch_and_add_read
# endif
#endif /* MK_AO_HAVE_fetch_and_add_full */

#if !defined(MK_AO_HAVE_fetch_and_add) && defined(MK_AO_HAVE_fetch_and_add_release)
# define MK_AO_fetch_and_add(addr,val) MK_AO_fetch_and_add_release(addr,val)
# define MK_AO_HAVE_fetch_and_add
#endif
#if !defined(MK_AO_HAVE_fetch_and_add) && defined(MK_AO_HAVE_fetch_and_add_acquire)
# define MK_AO_fetch_and_add(addr,val) MK_AO_fetch_and_add_acquire(addr,val)
# define MK_AO_HAVE_fetch_and_add
#endif
#if !defined(MK_AO_HAVE_fetch_and_add) && defined(MK_AO_HAVE_fetch_and_add_write)
# define MK_AO_fetch_and_add(addr,val) MK_AO_fetch_and_add_write(addr,val)
# define MK_AO_HAVE_fetch_and_add
#endif
#if !defined(MK_AO_HAVE_fetch_and_add) && defined(MK_AO_HAVE_fetch_and_add_read)
# define MK_AO_fetch_and_add(addr,val) MK_AO_fetch_and_add_read(addr,val)
# define MK_AO_HAVE_fetch_and_add
#endif

#if defined(MK_AO_HAVE_fetch_and_add_acquire) && defined(MK_AO_HAVE_nop_full) \
    && !defined(MK_AO_HAVE_fetch_and_add_full)
# define MK_AO_fetch_and_add_full(addr,val) \
                        (MK_AO_nop_full(), MK_AO_fetch_and_add_acquire(addr,val))
# define MK_AO_HAVE_fetch_and_add_full
#endif

#if !defined(MK_AO_HAVE_fetch_and_add_release_write) \
    && defined(MK_AO_HAVE_fetch_and_add_write)
# define MK_AO_fetch_and_add_release_write(addr,val) \
                                        MK_AO_fetch_and_add_write(addr,val)
# define MK_AO_HAVE_fetch_and_add_release_write
#endif
#if !defined(MK_AO_HAVE_fetch_and_add_release_write) \
    && defined(MK_AO_HAVE_fetch_and_add_release)
# define MK_AO_fetch_and_add_release_write(addr,val) \
                                        MK_AO_fetch_and_add_release(addr,val)
# define MK_AO_HAVE_fetch_and_add_release_write
#endif
#if !defined(MK_AO_HAVE_fetch_and_add_acquire_read) \
    && defined(MK_AO_HAVE_fetch_and_add_read)
# define MK_AO_fetch_and_add_acquire_read(addr,val) \
                                        MK_AO_fetch_and_add_read(addr,val)
# define MK_AO_HAVE_fetch_and_add_acquire_read
#endif
#if !defined(MK_AO_HAVE_fetch_and_add_acquire_read) \
    && defined(MK_AO_HAVE_fetch_and_add_acquire)
# define MK_AO_fetch_and_add_acquire_read(addr,val) \
                                        MK_AO_fetch_and_add_acquire(addr,val)
# define MK_AO_HAVE_fetch_and_add_acquire_read
#endif

#ifdef MK_AO_NO_DD_ORDERING
# if defined(MK_AO_HAVE_fetch_and_add_acquire_read)
#   define MK_AO_fetch_and_add_dd_acquire_read(addr,val) \
                                MK_AO_fetch_and_add_acquire_read(addr,val)
#   define MK_AO_HAVE_fetch_and_add_dd_acquire_read
# endif
#else
# if defined(MK_AO_HAVE_fetch_and_add)
#   define MK_AO_fetch_and_add_dd_acquire_read(addr,val) \
                                        MK_AO_fetch_and_add(addr,val)
#   define MK_AO_HAVE_fetch_and_add_dd_acquire_read
# endif
#endif /* !MK_AO_NO_DD_ORDERING */

/* Fetch_and_add1 */

#if defined(MK_AO_HAVE_fetch_and_add_full) \
    && !defined(MK_AO_HAVE_fetch_and_add1_full)
# define MK_AO_fetch_and_add1_full(addr) MK_AO_fetch_and_add_full(addr,1)
# define MK_AO_HAVE_fetch_and_add1_full
#endif
#if defined(MK_AO_HAVE_fetch_and_add_release) \
    && !defined(MK_AO_HAVE_fetch_and_add1_release)
# define MK_AO_fetch_and_add1_release(addr) MK_AO_fetch_and_add_release(addr,1)
# define MK_AO_HAVE_fetch_and_add1_release
#endif
#if defined(MK_AO_HAVE_fetch_and_add_acquire) \
    && !defined(MK_AO_HAVE_fetch_and_add1_acquire)
# define MK_AO_fetch_and_add1_acquire(addr) MK_AO_fetch_and_add_acquire(addr,1)
# define MK_AO_HAVE_fetch_and_add1_acquire
#endif
#if defined(MK_AO_HAVE_fetch_and_add_write) \
    && !defined(MK_AO_HAVE_fetch_and_add1_write)
# define MK_AO_fetch_and_add1_write(addr) MK_AO_fetch_and_add_write(addr,1)
# define MK_AO_HAVE_fetch_and_add1_write
#endif
#if defined(MK_AO_HAVE_fetch_and_add_read) \
    && !defined(MK_AO_HAVE_fetch_and_add1_read)
# define MK_AO_fetch_and_add1_read(addr) MK_AO_fetch_and_add_read(addr,1)
# define MK_AO_HAVE_fetch_and_add1_read
#endif
#if defined(MK_AO_HAVE_fetch_and_add_release_write) \
    && !defined(MK_AO_HAVE_fetch_and_add1_release_write)
# define MK_AO_fetch_and_add1_release_write(addr) \
                                        MK_AO_fetch_and_add_release_write(addr,1)
# define MK_AO_HAVE_fetch_and_add1_release_write
#endif
#if defined(MK_AO_HAVE_fetch_and_add_acquire_read) \
    && !defined(MK_AO_HAVE_fetch_and_add1_acquire_read)
# define MK_AO_fetch_and_add1_acquire_read(addr) \
                                        MK_AO_fetch_and_add_acquire_read(addr,1)
# define MK_AO_HAVE_fetch_and_add1_acquire_read
#endif
#if defined(MK_AO_HAVE_fetch_and_add) && !defined(MK_AO_HAVE_fetch_and_add1)
# define MK_AO_fetch_and_add1(addr) MK_AO_fetch_and_add(addr,1)
# define MK_AO_HAVE_fetch_and_add1
#endif

#if defined(MK_AO_HAVE_fetch_and_add1_full)
# if !defined(MK_AO_HAVE_fetch_and_add1_release)
#   define MK_AO_fetch_and_add1_release(addr) MK_AO_fetch_and_add1_full(addr)
#   define MK_AO_HAVE_fetch_and_add1_release
# endif
# if !defined(MK_AO_HAVE_fetch_and_add1_acquire)
#   define MK_AO_fetch_and_add1_acquire(addr) MK_AO_fetch_and_add1_full(addr)
#   define MK_AO_HAVE_fetch_and_add1_acquire
# endif
# if !defined(MK_AO_HAVE_fetch_and_add1_write)
#   define MK_AO_fetch_and_add1_write(addr) MK_AO_fetch_and_add1_full(addr)
#   define MK_AO_HAVE_fetch_and_add1_write
# endif
# if !defined(MK_AO_HAVE_fetch_and_add1_read)
#   define MK_AO_fetch_and_add1_read(addr) MK_AO_fetch_and_add1_full(addr)
#   define MK_AO_HAVE_fetch_and_add1_read
# endif
#endif /* MK_AO_HAVE_fetch_and_add1_full */

#if !defined(MK_AO_HAVE_fetch_and_add1) \
    && defined(MK_AO_HAVE_fetch_and_add1_release)
# define MK_AO_fetch_and_add1(addr) MK_AO_fetch_and_add1_release(addr)
# define MK_AO_HAVE_fetch_and_add1
#endif
#if !defined(MK_AO_HAVE_fetch_and_add1) \
    && defined(MK_AO_HAVE_fetch_and_add1_acquire)
# define MK_AO_fetch_and_add1(addr) MK_AO_fetch_and_add1_acquire(addr)
# define MK_AO_HAVE_fetch_and_add1
#endif
#if !defined(MK_AO_HAVE_fetch_and_add1) && defined(MK_AO_HAVE_fetch_and_add1_write)
# define MK_AO_fetch_and_add1(addr) MK_AO_fetch_and_add1_write(addr)
# define MK_AO_HAVE_fetch_and_add1
#endif
#if !defined(MK_AO_HAVE_fetch_and_add1) && defined(MK_AO_HAVE_fetch_and_add1_read)
# define MK_AO_fetch_and_add1(addr) MK_AO_fetch_and_add1_read(addr)
# define MK_AO_HAVE_fetch_and_add1
#endif

#if defined(MK_AO_HAVE_fetch_and_add1_acquire) && defined(MK_AO_HAVE_nop_full) \
    && !defined(MK_AO_HAVE_fetch_and_add1_full)
# define MK_AO_fetch_and_add1_full(addr) \
                        (MK_AO_nop_full(), MK_AO_fetch_and_add1_acquire(addr))
# define MK_AO_HAVE_fetch_and_add1_full
#endif

#if !defined(MK_AO_HAVE_fetch_and_add1_release_write) \
    && defined(MK_AO_HAVE_fetch_and_add1_write)
# define MK_AO_fetch_and_add1_release_write(addr) MK_AO_fetch_and_add1_write(addr)
# define MK_AO_HAVE_fetch_and_add1_release_write
#endif
#if !defined(MK_AO_HAVE_fetch_and_add1_release_write) \
    && defined(MK_AO_HAVE_fetch_and_add1_release)
# define MK_AO_fetch_and_add1_release_write(addr) MK_AO_fetch_and_add1_release(addr)
# define MK_AO_HAVE_fetch_and_add1_release_write
#endif
#if !defined(MK_AO_HAVE_fetch_and_add1_acquire_read) \
    && defined(MK_AO_HAVE_fetch_and_add1_read)
# define MK_AO_fetch_and_add1_acquire_read(addr) MK_AO_fetch_and_add1_read(addr)
# define MK_AO_HAVE_fetch_and_add1_acquire_read
#endif
#if !defined(MK_AO_HAVE_fetch_and_add1_acquire_read) \
    && defined(MK_AO_HAVE_fetch_and_add1_acquire)
# define MK_AO_fetch_and_add1_acquire_read(addr) MK_AO_fetch_and_add1_acquire(addr)
# define MK_AO_HAVE_fetch_and_add1_acquire_read
#endif

#ifdef MK_AO_NO_DD_ORDERING
# if defined(MK_AO_HAVE_fetch_and_add1_acquire_read)
#   define MK_AO_fetch_and_add1_dd_acquire_read(addr) \
                                        MK_AO_fetch_and_add1_acquire_read(addr)
#   define MK_AO_HAVE_fetch_and_add1_dd_acquire_read
# endif
#else
# if defined(MK_AO_HAVE_fetch_and_add1)
#   define MK_AO_fetch_and_add1_dd_acquire_read(addr) MK_AO_fetch_and_add1(addr)
#   define MK_AO_HAVE_fetch_and_add1_dd_acquire_read
# endif
#endif /* !MK_AO_NO_DD_ORDERING */

/* Fetch_and_sub1 */
#if defined(MK_AO_HAVE_fetch_and_add_full) \
    && !defined(MK_AO_HAVE_fetch_and_sub1_full)
# define MK_AO_fetch_and_sub1_full(addr) MK_AO_fetch_and_add_full(addr,(MK_AO_t)(-1))
# define MK_AO_HAVE_fetch_and_sub1_full
#endif
#if defined(MK_AO_HAVE_fetch_and_add_release) \
    && !defined(MK_AO_HAVE_fetch_and_sub1_release)
# define MK_AO_fetch_and_sub1_release(addr) \
                                MK_AO_fetch_and_add_release(addr,(MK_AO_t)(-1))
# define MK_AO_HAVE_fetch_and_sub1_release
#endif
#if defined(MK_AO_HAVE_fetch_and_add_acquire) \
    && !defined(MK_AO_HAVE_fetch_and_sub1_acquire)
# define MK_AO_fetch_and_sub1_acquire(addr) \
                                MK_AO_fetch_and_add_acquire(addr,(MK_AO_t)(-1))
# define MK_AO_HAVE_fetch_and_sub1_acquire
#endif
#if defined(MK_AO_HAVE_fetch_and_add_write) \
    && !defined(MK_AO_HAVE_fetch_and_sub1_write)
# define MK_AO_fetch_and_sub1_write(addr) MK_AO_fetch_and_add_write(addr,(MK_AO_t)(-1))
# define MK_AO_HAVE_fetch_and_sub1_write
#endif
#if defined(MK_AO_HAVE_fetch_and_add_read) \
    && !defined(MK_AO_HAVE_fetch_and_sub1_read)
# define MK_AO_fetch_and_sub1_read(addr) MK_AO_fetch_and_add_read(addr,(MK_AO_t)(-1))
# define MK_AO_HAVE_fetch_and_sub1_read
#endif
#if defined(MK_AO_HAVE_fetch_and_add_release_write) \
    && !defined(MK_AO_HAVE_fetch_and_sub1_release_write)
# define MK_AO_fetch_and_sub1_release_write(addr) \
                        MK_AO_fetch_and_add_release_write(addr,(MK_AO_t)(-1))
# define MK_AO_HAVE_fetch_and_sub1_release_write
#endif
#if defined(MK_AO_HAVE_fetch_and_add_acquire_read) \
    && !defined(MK_AO_HAVE_fetch_and_sub1_acquire_read)
# define MK_AO_fetch_and_sub1_acquire_read(addr) \
                        MK_AO_fetch_and_add_acquire_read(addr,(MK_AO_t)(-1))
# define MK_AO_HAVE_fetch_and_sub1_acquire_read
#endif
#if defined(MK_AO_HAVE_fetch_and_add) && !defined(MK_AO_HAVE_fetch_and_sub1)
# define MK_AO_fetch_and_sub1(addr) MK_AO_fetch_and_add(addr,(MK_AO_t)(-1))
# define MK_AO_HAVE_fetch_and_sub1
#endif

#if defined(MK_AO_HAVE_fetch_and_sub1_full)
# if !defined(MK_AO_HAVE_fetch_and_sub1_release)
#   define MK_AO_fetch_and_sub1_release(addr) MK_AO_fetch_and_sub1_full(addr)
#   define MK_AO_HAVE_fetch_and_sub1_release
# endif
# if !defined(MK_AO_HAVE_fetch_and_sub1_acquire)
#   define MK_AO_fetch_and_sub1_acquire(addr) MK_AO_fetch_and_sub1_full(addr)
#   define MK_AO_HAVE_fetch_and_sub1_acquire
# endif
# if !defined(MK_AO_HAVE_fetch_and_sub1_write)
#   define MK_AO_fetch_and_sub1_write(addr) MK_AO_fetch_and_sub1_full(addr)
#   define MK_AO_HAVE_fetch_and_sub1_write
# endif
# if !defined(MK_AO_HAVE_fetch_and_sub1_read)
#   define MK_AO_fetch_and_sub1_read(addr) MK_AO_fetch_and_sub1_full(addr)
#   define MK_AO_HAVE_fetch_and_sub1_read
# endif
#endif /* MK_AO_HAVE_fetch_and_sub1_full */

#if !defined(MK_AO_HAVE_fetch_and_sub1) \
    && defined(MK_AO_HAVE_fetch_and_sub1_release)
# define MK_AO_fetch_and_sub1(addr) MK_AO_fetch_and_sub1_release(addr)
# define MK_AO_HAVE_fetch_and_sub1
#endif
#if !defined(MK_AO_HAVE_fetch_and_sub1) \
    && defined(MK_AO_HAVE_fetch_and_sub1_acquire)
# define MK_AO_fetch_and_sub1(addr) MK_AO_fetch_and_sub1_acquire(addr)
# define MK_AO_HAVE_fetch_and_sub1
#endif
#if !defined(MK_AO_HAVE_fetch_and_sub1) && defined(MK_AO_HAVE_fetch_and_sub1_write)
# define MK_AO_fetch_and_sub1(addr) MK_AO_fetch_and_sub1_write(addr)
# define MK_AO_HAVE_fetch_and_sub1
#endif
#if !defined(MK_AO_HAVE_fetch_and_sub1) && defined(MK_AO_HAVE_fetch_and_sub1_read)
# define MK_AO_fetch_and_sub1(addr) MK_AO_fetch_and_sub1_read(addr)
# define MK_AO_HAVE_fetch_and_sub1
#endif

#if defined(MK_AO_HAVE_fetch_and_sub1_acquire) && defined(MK_AO_HAVE_nop_full) \
    && !defined(MK_AO_HAVE_fetch_and_sub1_full)
# define MK_AO_fetch_and_sub1_full(addr) \
                        (MK_AO_nop_full(), MK_AO_fetch_and_sub1_acquire(addr))
# define MK_AO_HAVE_fetch_and_sub1_full
#endif

#if !defined(MK_AO_HAVE_fetch_and_sub1_release_write) \
    && defined(MK_AO_HAVE_fetch_and_sub1_write)
# define MK_AO_fetch_and_sub1_release_write(addr) MK_AO_fetch_and_sub1_write(addr)
# define MK_AO_HAVE_fetch_and_sub1_release_write
#endif
#if !defined(MK_AO_HAVE_fetch_and_sub1_release_write) \
    && defined(MK_AO_HAVE_fetch_and_sub1_release)
# define MK_AO_fetch_and_sub1_release_write(addr) MK_AO_fetch_and_sub1_release(addr)
# define MK_AO_HAVE_fetch_and_sub1_release_write
#endif
#if !defined(MK_AO_HAVE_fetch_and_sub1_acquire_read) \
    && defined(MK_AO_HAVE_fetch_and_sub1_read)
# define MK_AO_fetch_and_sub1_acquire_read(addr) MK_AO_fetch_and_sub1_read(addr)
# define MK_AO_HAVE_fetch_and_sub1_acquire_read
#endif
#if !defined(MK_AO_HAVE_fetch_and_sub1_acquire_read) \
    && defined(MK_AO_HAVE_fetch_and_sub1_acquire)
# define MK_AO_fetch_and_sub1_acquire_read(addr) MK_AO_fetch_and_sub1_acquire(addr)
# define MK_AO_HAVE_fetch_and_sub1_acquire_read
#endif

#ifdef MK_AO_NO_DD_ORDERING
# if defined(MK_AO_HAVE_fetch_and_sub1_acquire_read)
#   define MK_AO_fetch_and_sub1_dd_acquire_read(addr) \
                                        MK_AO_fetch_and_sub1_acquire_read(addr)
#   define MK_AO_HAVE_fetch_and_sub1_dd_acquire_read
# endif
#else
# if defined(MK_AO_HAVE_fetch_and_sub1)
#   define MK_AO_fetch_and_sub1_dd_acquire_read(addr) MK_AO_fetch_and_sub1(addr)
#   define MK_AO_HAVE_fetch_and_sub1_dd_acquire_read
# endif
#endif /* !MK_AO_NO_DD_ORDERING */

/* Atomic or */
#if defined(MK_AO_HAVE_compare_and_swap_full) && !defined(MK_AO_HAVE_or_full)
  MK_AO_INLINE void
  MK_AO_or_full(volatile MK_AO_t *addr, MK_AO_t incr)
  {
    MK_AO_t old;
    do
      {
        old = *addr;
      }
    while (!MK_AO_compare_and_swap_full(addr, old, old | incr));
  }
# define MK_AO_HAVE_or_full
#endif

#if defined(MK_AO_HAVE_or_full)
# if !defined(MK_AO_HAVE_or_release)
#   define MK_AO_or_release(addr,val) MK_AO_or_full(addr,val)
#   define MK_AO_HAVE_or_release
# endif
# if !defined(MK_AO_HAVE_or_acquire)
#   define MK_AO_or_acquire(addr,val) MK_AO_or_full(addr,val)
#   define MK_AO_HAVE_or_acquire
# endif
# if !defined(MK_AO_HAVE_or_write)
#   define MK_AO_or_write(addr,val) MK_AO_or_full(addr,val)
#   define MK_AO_HAVE_or_write
# endif
# if !defined(MK_AO_HAVE_or_read)
#   define MK_AO_or_read(addr,val) MK_AO_or_full(addr,val)
#   define MK_AO_HAVE_or_read
# endif
#endif /* MK_AO_HAVE_or_full */

#if !defined(MK_AO_HAVE_or) && defined(MK_AO_HAVE_or_release)
# define MK_AO_or(addr,val) MK_AO_or_release(addr,val)
# define MK_AO_HAVE_or
#endif
#if !defined(MK_AO_HAVE_or) && defined(MK_AO_HAVE_or_acquire)
# define MK_AO_or(addr,val) MK_AO_or_acquire(addr,val)
# define MK_AO_HAVE_or
#endif
#if !defined(MK_AO_HAVE_or) && defined(MK_AO_HAVE_or_write)
# define MK_AO_or(addr,val) MK_AO_or_write(addr,val)
# define MK_AO_HAVE_or
#endif
#if !defined(MK_AO_HAVE_or) && defined(MK_AO_HAVE_or_read)
# define MK_AO_or(addr,val) MK_AO_or_read(addr,val)
# define MK_AO_HAVE_or
#endif

#if defined(MK_AO_HAVE_or_acquire) && defined(MK_AO_HAVE_nop_full) \
    && !defined(MK_AO_HAVE_or_full)
# define MK_AO_or_full(addr,val) (MK_AO_nop_full(), MK_AO_or_acquire(addr,val))
# define MK_AO_HAVE_or_full
#endif

#if !defined(MK_AO_HAVE_or_release_write) && defined(MK_AO_HAVE_or_write)
# define MK_AO_or_release_write(addr,val) MK_AO_or_write(addr,val)
# define MK_AO_HAVE_or_release_write
#endif
#if !defined(MK_AO_HAVE_or_release_write) && defined(MK_AO_HAVE_or_release)
# define MK_AO_or_release_write(addr,val) MK_AO_or_release(addr,val)
# define MK_AO_HAVE_or_release_write
#endif
#if !defined(MK_AO_HAVE_or_acquire_read) && defined(MK_AO_HAVE_or_read)
# define MK_AO_or_acquire_read(addr,val) MK_AO_or_read(addr,val)
# define MK_AO_HAVE_or_acquire_read
#endif
#if !defined(MK_AO_HAVE_or_acquire_read) && defined(MK_AO_HAVE_or_acquire)
# define MK_AO_or_acquire_read(addr,val) MK_AO_or_acquire(addr,val)
# define MK_AO_HAVE_or_acquire_read
#endif

/* dd_aquire_read is meaningless.       */

/* Test_and_set */
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

/* Compare_and_swap */
#if defined(MK_AO_HAVE_compare_and_swap) && defined(MK_AO_HAVE_nop_full) \
    && !defined(MK_AO_HAVE_compare_and_swap_acquire)
  MK_AO_INLINE int
  MK_AO_compare_and_swap_acquire(volatile MK_AO_t *addr, MK_AO_t old, MK_AO_t new_val)
  {
    int result = MK_AO_compare_and_swap(addr, old, new_val);
    MK_AO_nop_full();
    return result;
  }
# define MK_AO_HAVE_compare_and_swap_acquire
#endif
#if defined(MK_AO_HAVE_compare_and_swap) && defined(MK_AO_HAVE_nop_full) \
    && !defined(MK_AO_HAVE_compare_and_swap_release)
# define MK_AO_compare_and_swap_release(addr,old,new_val) \
                        (MK_AO_nop_full(), MK_AO_compare_and_swap(addr,old,new_val))
# define MK_AO_HAVE_compare_and_swap_release
#endif
#if defined(MK_AO_HAVE_compare_and_swap_full)
# if !defined(MK_AO_HAVE_compare_and_swap_release)
#   define MK_AO_compare_and_swap_release(addr,old,new_val) \
                                MK_AO_compare_and_swap_full(addr,old,new_val)
#   define MK_AO_HAVE_compare_and_swap_release
# endif
# if !defined(MK_AO_HAVE_compare_and_swap_acquire)
#   define MK_AO_compare_and_swap_acquire(addr,old,new_val) \
                                MK_AO_compare_and_swap_full(addr,old,new_val)
#   define MK_AO_HAVE_compare_and_swap_acquire
# endif
# if !defined(MK_AO_HAVE_compare_and_swap_write)
#   define MK_AO_compare_and_swap_write(addr,old,new_val) \
                                MK_AO_compare_and_swap_full(addr,old,new_val)
#   define MK_AO_HAVE_compare_and_swap_write
# endif
# if !defined(MK_AO_HAVE_compare_and_swap_read)
#   define MK_AO_compare_and_swap_read(addr,old,new_val) \
                                MK_AO_compare_and_swap_full(addr,old,new_val)
#   define MK_AO_HAVE_compare_and_swap_read
# endif
#endif /* MK_AO_HAVE_compare_and_swap_full */

#if !defined(MK_AO_HAVE_compare_and_swap) \
    && defined(MK_AO_HAVE_compare_and_swap_release)
# define MK_AO_compare_and_swap(addr,old,new_val) \
                                MK_AO_compare_and_swap_release(addr,old,new_val)
# define MK_AO_HAVE_compare_and_swap
#endif
#if !defined(MK_AO_HAVE_compare_and_swap) \
    && defined(MK_AO_HAVE_compare_and_swap_acquire)
# define MK_AO_compare_and_swap(addr,old,new_val) \
                                MK_AO_compare_and_swap_acquire(addr,old,new_val)
# define MK_AO_HAVE_compare_and_swap
#endif
#if !defined(MK_AO_HAVE_compare_and_swap) \
    && defined(MK_AO_HAVE_compare_and_swap_write)
# define MK_AO_compare_and_swap(addr,old,new_val) \
                                MK_AO_compare_and_swap_write(addr,old,new_val)
# define MK_AO_HAVE_compare_and_swap
#endif
#if !defined(MK_AO_HAVE_compare_and_swap) \
    && defined(MK_AO_HAVE_compare_and_swap_read)
# define MK_AO_compare_and_swap(addr,old,new_val) \
                                MK_AO_compare_and_swap_read(addr,old,new_val)
# define MK_AO_HAVE_compare_and_swap
#endif

#if defined(MK_AO_HAVE_compare_and_swap_acquire) \
    && defined(MK_AO_HAVE_nop_full) && !defined(MK_AO_HAVE_compare_and_swap_full)
# define MK_AO_compare_and_swap_full(addr,old,new_val) \
                (MK_AO_nop_full(), MK_AO_compare_and_swap_acquire(addr,old,new_val))
# define MK_AO_HAVE_compare_and_swap_full
#endif

#if !defined(MK_AO_HAVE_compare_and_swap_release_write) \
    && defined(MK_AO_HAVE_compare_and_swap_write)
# define MK_AO_compare_and_swap_release_write(addr,old,new_val) \
                                MK_AO_compare_and_swap_write(addr,old,new_val)
# define MK_AO_HAVE_compare_and_swap_release_write
#endif
#if !defined(MK_AO_HAVE_compare_and_swap_release_write) \
    && defined(MK_AO_HAVE_compare_and_swap_release)
# define MK_AO_compare_and_swap_release_write(addr,old,new_val) \
                                MK_AO_compare_and_swap_release(addr,old,new_val)
# define MK_AO_HAVE_compare_and_swap_release_write
#endif
#if !defined(MK_AO_HAVE_compare_and_swap_acquire_read) \
    && defined(MK_AO_HAVE_compare_and_swap_read)
# define MK_AO_compare_and_swap_acquire_read(addr,old,new_val) \
                                MK_AO_compare_and_swap_read(addr,old,new_val)
# define MK_AO_HAVE_compare_and_swap_acquire_read
#endif
#if !defined(MK_AO_HAVE_compare_and_swap_acquire_read) \
    && defined(MK_AO_HAVE_compare_and_swap_acquire)
# define MK_AO_compare_and_swap_acquire_read(addr,old,new_val) \
                                MK_AO_compare_and_swap_acquire(addr,old,new_val)
# define MK_AO_HAVE_compare_and_swap_acquire_read
#endif

#ifdef MK_AO_NO_DD_ORDERING
# if defined(MK_AO_HAVE_compare_and_swap_acquire_read)
#   define MK_AO_compare_and_swap_dd_acquire_read(addr,old,new_val) \
                        MK_AO_compare_and_swap_acquire_read(addr,old,new_val)
#   define MK_AO_HAVE_compare_and_swap_dd_acquire_read
# endif
#else
# if defined(MK_AO_HAVE_compare_and_swap)
#   define MK_AO_compare_and_swap_dd_acquire_read(addr,old,new_val) \
                                MK_AO_compare_and_swap(addr,old,new_val)
#   define MK_AO_HAVE_compare_and_swap_dd_acquire_read
# endif
#endif /* !MK_AO_NO_DD_ORDERING */

#include "generalize-small.h"

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

/* NEC LE-IT: Convenience functions for MK_AO_double compare and swap which */
/* types and reads easier in code                                        */
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
