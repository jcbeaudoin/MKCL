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

/* char_compare_and_swap (based on fetch_compare_and_swap) */
#if defined(MK_AO_HAVE_char_fetch_compare_and_swap_full) \
    && !defined(MK_AO_HAVE_char_compare_and_swap_full)
  MK_AO_INLINE int
  MK_AO_char_compare_and_swap_full(volatile unsigned/**/char *addr, unsigned/**/char old_val,
                                 unsigned/**/char new_val)
  {
    return MK_AO_char_fetch_compare_and_swap_full(addr, old_val, new_val)
             == old_val;
  }
# define MK_AO_HAVE_char_compare_and_swap_full
#endif

#if defined(MK_AO_HAVE_char_fetch_compare_and_swap_acquire) \
    && !defined(MK_AO_HAVE_char_compare_and_swap_acquire)
  MK_AO_INLINE int
  MK_AO_char_compare_and_swap_acquire(volatile unsigned/**/char *addr, unsigned/**/char old_val,
                                    unsigned/**/char new_val)
  {
    return MK_AO_char_fetch_compare_and_swap_acquire(addr, old_val, new_val)
             == old_val;
  }
# define MK_AO_HAVE_char_compare_and_swap_acquire
#endif

#if defined(MK_AO_HAVE_char_fetch_compare_and_swap_release) \
    && !defined(MK_AO_HAVE_char_compare_and_swap_release)
  MK_AO_INLINE int
  MK_AO_char_compare_and_swap_release(volatile unsigned/**/char *addr, unsigned/**/char old_val,
                                    unsigned/**/char new_val)
  {
    return MK_AO_char_fetch_compare_and_swap_release(addr, old_val, new_val)
             == old_val;
  }
# define MK_AO_HAVE_char_compare_and_swap_release
#endif

#if defined(MK_AO_HAVE_char_fetch_compare_and_swap_write) \
    && !defined(MK_AO_HAVE_char_compare_and_swap_write)
  MK_AO_INLINE int
  MK_AO_char_compare_and_swap_write(volatile unsigned/**/char *addr, unsigned/**/char old_val,
                                  unsigned/**/char new_val)
  {
    return MK_AO_char_fetch_compare_and_swap_write(addr, old_val, new_val)
             == old_val;
  }
# define MK_AO_HAVE_char_compare_and_swap_write
#endif

#if defined(MK_AO_HAVE_char_fetch_compare_and_swap_read) \
    && !defined(MK_AO_HAVE_char_compare_and_swap_read)
  MK_AO_INLINE int
  MK_AO_char_compare_and_swap_read(volatile unsigned/**/char *addr, unsigned/**/char old_val,
                                 unsigned/**/char new_val)
  {
    return MK_AO_char_fetch_compare_and_swap_read(addr, old_val, new_val)
             == old_val;
  }
# define MK_AO_HAVE_char_compare_and_swap_read
#endif

#if defined(MK_AO_HAVE_char_fetch_compare_and_swap) \
    && !defined(MK_AO_HAVE_char_compare_and_swap)
  MK_AO_INLINE int
  MK_AO_char_compare_and_swap(volatile unsigned/**/char *addr, unsigned/**/char old_val,
                            unsigned/**/char new_val)
  {
    return MK_AO_char_fetch_compare_and_swap(addr, old_val, new_val) == old_val;
  }
# define MK_AO_HAVE_char_compare_and_swap
#endif

#if defined(MK_AO_HAVE_char_fetch_compare_and_swap_release_write) \
    && !defined(MK_AO_HAVE_char_compare_and_swap_release_write)
  MK_AO_INLINE int
  MK_AO_char_compare_and_swap_release_write(volatile unsigned/**/char *addr,
                                          unsigned/**/char old_val, unsigned/**/char new_val)
  {
    return MK_AO_char_fetch_compare_and_swap_release_write(addr, old_val,
                                                         new_val) == old_val;
  }
# define MK_AO_HAVE_char_compare_and_swap_release_write
#endif

#if defined(MK_AO_HAVE_char_fetch_compare_and_swap_acquire_read) \
    && !defined(MK_AO_HAVE_char_compare_and_swap_acquire_read)
  MK_AO_INLINE int
  MK_AO_char_compare_and_swap_acquire_read(volatile unsigned/**/char *addr,
                                         unsigned/**/char old_val, unsigned/**/char new_val)
  {
    return MK_AO_char_fetch_compare_and_swap_acquire_read(addr, old_val,
                                                        new_val) == old_val;
  }
# define MK_AO_HAVE_char_compare_and_swap_acquire_read
#endif

#if defined(MK_AO_HAVE_char_fetch_compare_and_swap_dd_acquire_read) \
    && !defined(MK_AO_HAVE_char_compare_and_swap_dd_acquire_read)
  MK_AO_INLINE int
  MK_AO_char_compare_and_swap_dd_acquire_read(volatile unsigned/**/char *addr,
                                            unsigned/**/char old_val, unsigned/**/char new_val)
  {
    return MK_AO_char_fetch_compare_and_swap_dd_acquire_read(addr, old_val,
                                                           new_val) == old_val;
  }
# define MK_AO_HAVE_char_compare_and_swap_dd_acquire_read
#endif

/* char_fetch_and_add */
/* We first try to implement fetch_and_add variants in terms of the     */
/* corresponding compare_and_swap variants to minimize adding barriers. */
#if defined(MK_AO_HAVE_char_compare_and_swap_full) \
    && !defined(MK_AO_HAVE_char_fetch_and_add_full)
  MK_AO_INLINE unsigned/**/char
  MK_AO_char_fetch_and_add_full(volatile unsigned/**/char *addr, unsigned/**/char incr)
  {
    unsigned/**/char old;

    do
      {
        old = *(unsigned/**/char *)addr;
      }
    while (MK_AO_EXPECT_FALSE(!MK_AO_char_compare_and_swap_full(addr, old,
                                                           old + incr)));
    return old;
  }
# define MK_AO_HAVE_char_fetch_and_add_full
#endif

#if defined(MK_AO_HAVE_char_compare_and_swap_acquire) \
    && !defined(MK_AO_HAVE_char_fetch_and_add_acquire)
  MK_AO_INLINE unsigned/**/char
  MK_AO_char_fetch_and_add_acquire(volatile unsigned/**/char *addr, unsigned/**/char incr)
  {
    unsigned/**/char old;

    do
      {
        old = *(unsigned/**/char *)addr;
      }
    while (MK_AO_EXPECT_FALSE(!MK_AO_char_compare_and_swap_acquire(addr, old,
                                                              old + incr)));
    return old;
  }
# define MK_AO_HAVE_char_fetch_and_add_acquire
#endif

#if defined(MK_AO_HAVE_char_compare_and_swap_release) \
    && !defined(MK_AO_HAVE_char_fetch_and_add_release)
  MK_AO_INLINE unsigned/**/char
  MK_AO_char_fetch_and_add_release(volatile unsigned/**/char *addr, unsigned/**/char incr)
  {
    unsigned/**/char old;

    do
      {
        old = *(unsigned/**/char *)addr;
      }
    while (MK_AO_EXPECT_FALSE(!MK_AO_char_compare_and_swap_release(addr, old,
                                                              old + incr)));
    return old;
  }
# define MK_AO_HAVE_char_fetch_and_add_release
#endif

#if defined(MK_AO_HAVE_char_compare_and_swap) \
    && !defined(MK_AO_HAVE_char_fetch_and_add)
  MK_AO_INLINE unsigned/**/char
  MK_AO_char_fetch_and_add(volatile unsigned/**/char *addr, unsigned/**/char incr)
  {
    unsigned/**/char old;

    do
      {
        old = *(unsigned/**/char *)addr;
      }
    while (MK_AO_EXPECT_FALSE(!MK_AO_char_compare_and_swap(addr, old,
                                                      old + incr)));
    return old;
  }
# define MK_AO_HAVE_char_fetch_and_add
#endif

#if defined(MK_AO_HAVE_char_fetch_and_add_full)
# if !defined(MK_AO_HAVE_char_fetch_and_add_release)
#   define MK_AO_char_fetch_and_add_release(addr, val) \
                                MK_AO_char_fetch_and_add_full(addr, val)
#   define MK_AO_HAVE_char_fetch_and_add_release
# endif
# if !defined(MK_AO_HAVE_char_fetch_and_add_acquire)
#   define MK_AO_char_fetch_and_add_acquire(addr, val) \
                                MK_AO_char_fetch_and_add_full(addr, val)
#   define MK_AO_HAVE_char_fetch_and_add_acquire
# endif
# if !defined(MK_AO_HAVE_char_fetch_and_add_write)
#   define MK_AO_char_fetch_and_add_write(addr, val) \
                                MK_AO_char_fetch_and_add_full(addr, val)
#   define MK_AO_HAVE_char_fetch_and_add_write
# endif
# if !defined(MK_AO_HAVE_char_fetch_and_add_read)
#   define MK_AO_char_fetch_and_add_read(addr, val) \
                                MK_AO_char_fetch_and_add_full(addr, val)
#   define MK_AO_HAVE_char_fetch_and_add_read
# endif
#endif /* MK_AO_HAVE_char_fetch_and_add_full */

#if defined(MK_AO_HAVE_char_fetch_and_add) && defined(MK_AO_HAVE_nop_full) \
    && !defined(MK_AO_HAVE_char_fetch_and_add_acquire)
  MK_AO_INLINE unsigned/**/char
  MK_AO_char_fetch_and_add_acquire(volatile unsigned/**/char *addr, unsigned/**/char incr)
  {
    unsigned/**/char result = MK_AO_char_fetch_and_add(addr, incr);
    MK_AO_nop_full();
    return result;
  }
# define MK_AO_HAVE_char_fetch_and_add_acquire
#endif
#if defined(MK_AO_HAVE_char_fetch_and_add) && defined(MK_AO_HAVE_nop_full) \
    && !defined(MK_AO_HAVE_char_fetch_and_add_release)
# define MK_AO_char_fetch_and_add_release(addr, incr) \
                (MK_AO_nop_full(), MK_AO_char_fetch_and_add(addr, incr))
# define MK_AO_HAVE_char_fetch_and_add_release
#endif

#if !defined(MK_AO_HAVE_char_fetch_and_add) \
    && defined(MK_AO_HAVE_char_fetch_and_add_release)
# define MK_AO_char_fetch_and_add(addr, val) \
                                MK_AO_char_fetch_and_add_release(addr, val)
# define MK_AO_HAVE_char_fetch_and_add
#endif
#if !defined(MK_AO_HAVE_char_fetch_and_add) \
    && defined(MK_AO_HAVE_char_fetch_and_add_acquire)
# define MK_AO_char_fetch_and_add(addr, val) \
                                MK_AO_char_fetch_and_add_acquire(addr, val)
# define MK_AO_HAVE_char_fetch_and_add
#endif
#if !defined(MK_AO_HAVE_char_fetch_and_add) \
    && defined(MK_AO_HAVE_char_fetch_and_add_write)
# define MK_AO_char_fetch_and_add(addr, val) \
                                MK_AO_char_fetch_and_add_write(addr, val)
# define MK_AO_HAVE_char_fetch_and_add
#endif
#if !defined(MK_AO_HAVE_char_fetch_and_add) \
    && defined(MK_AO_HAVE_char_fetch_and_add_read)
# define MK_AO_char_fetch_and_add(addr, val) \
                                MK_AO_char_fetch_and_add_read(addr, val)
# define MK_AO_HAVE_char_fetch_and_add
#endif

#if defined(MK_AO_HAVE_char_fetch_and_add_acquire) \
    && defined(MK_AO_HAVE_nop_full) && !defined(MK_AO_HAVE_char_fetch_and_add_full)
# define MK_AO_char_fetch_and_add_full(addr, val) \
                (MK_AO_nop_full(), MK_AO_char_fetch_and_add_acquire(addr, val))
# define MK_AO_HAVE_char_fetch_and_add_full
#endif

#if !defined(MK_AO_HAVE_char_fetch_and_add_release_write) \
    && defined(MK_AO_HAVE_char_fetch_and_add_write)
# define MK_AO_char_fetch_and_add_release_write(addr, val) \
                                MK_AO_char_fetch_and_add_write(addr, val)
# define MK_AO_HAVE_char_fetch_and_add_release_write
#endif
#if !defined(MK_AO_HAVE_char_fetch_and_add_release_write) \
    && defined(MK_AO_HAVE_char_fetch_and_add_release)
# define MK_AO_char_fetch_and_add_release_write(addr, val) \
                                MK_AO_char_fetch_and_add_release(addr, val)
# define MK_AO_HAVE_char_fetch_and_add_release_write
#endif

#if !defined(MK_AO_HAVE_char_fetch_and_add_acquire_read) \
    && defined(MK_AO_HAVE_char_fetch_and_add_read)
# define MK_AO_char_fetch_and_add_acquire_read(addr, val) \
                                MK_AO_char_fetch_and_add_read(addr, val)
# define MK_AO_HAVE_char_fetch_and_add_acquire_read
#endif
#if !defined(MK_AO_HAVE_char_fetch_and_add_acquire_read) \
    && defined(MK_AO_HAVE_char_fetch_and_add_acquire)
# define MK_AO_char_fetch_and_add_acquire_read(addr, val) \
                                MK_AO_char_fetch_and_add_acquire(addr, val)
# define MK_AO_HAVE_char_fetch_and_add_acquire_read
#endif

#ifdef MK_AO_NO_DD_ORDERING
# if defined(MK_AO_HAVE_char_fetch_and_add_acquire_read)
#   define MK_AO_char_fetch_and_add_dd_acquire_read(addr, val) \
                                MK_AO_char_fetch_and_add_acquire_read(addr, val)
#   define MK_AO_HAVE_char_fetch_and_add_dd_acquire_read
# endif
#else
# if defined(MK_AO_HAVE_char_fetch_and_add)
#   define MK_AO_char_fetch_and_add_dd_acquire_read(addr, val) \
                                MK_AO_char_fetch_and_add(addr, val)
#   define MK_AO_HAVE_char_fetch_and_add_dd_acquire_read
# endif
#endif /* !MK_AO_NO_DD_ORDERING */

/* char_fetch_and_add1 */
#if defined(MK_AO_HAVE_char_fetch_and_add_full) \
    && !defined(MK_AO_HAVE_char_fetch_and_add1_full)
# define MK_AO_char_fetch_and_add1_full(addr) \
                                MK_AO_char_fetch_and_add_full(addr, 1)
# define MK_AO_HAVE_char_fetch_and_add1_full
#endif
#if defined(MK_AO_HAVE_char_fetch_and_add_release) \
    && !defined(MK_AO_HAVE_char_fetch_and_add1_release)
# define MK_AO_char_fetch_and_add1_release(addr) \
                                MK_AO_char_fetch_and_add_release(addr, 1)
# define MK_AO_HAVE_char_fetch_and_add1_release
#endif
#if defined(MK_AO_HAVE_char_fetch_and_add_acquire) \
    && !defined(MK_AO_HAVE_char_fetch_and_add1_acquire)
# define MK_AO_char_fetch_and_add1_acquire(addr) \
                                MK_AO_char_fetch_and_add_acquire(addr, 1)
# define MK_AO_HAVE_char_fetch_and_add1_acquire
#endif
#if defined(MK_AO_HAVE_char_fetch_and_add_write) \
    && !defined(MK_AO_HAVE_char_fetch_and_add1_write)
# define MK_AO_char_fetch_and_add1_write(addr) \
                                MK_AO_char_fetch_and_add_write(addr, 1)
# define MK_AO_HAVE_char_fetch_and_add1_write
#endif
#if defined(MK_AO_HAVE_char_fetch_and_add_read) \
    && !defined(MK_AO_HAVE_char_fetch_and_add1_read)
# define MK_AO_char_fetch_and_add1_read(addr) \
                                MK_AO_char_fetch_and_add_read(addr, 1)
# define MK_AO_HAVE_char_fetch_and_add1_read
#endif
#if defined(MK_AO_HAVE_char_fetch_and_add_release_write) \
    && !defined(MK_AO_HAVE_char_fetch_and_add1_release_write)
# define MK_AO_char_fetch_and_add1_release_write(addr) \
                                MK_AO_char_fetch_and_add_release_write(addr, 1)
# define MK_AO_HAVE_char_fetch_and_add1_release_write
#endif
#if defined(MK_AO_HAVE_char_fetch_and_add_acquire_read) \
    && !defined(MK_AO_HAVE_char_fetch_and_add1_acquire_read)
# define MK_AO_char_fetch_and_add1_acquire_read(addr) \
                                MK_AO_char_fetch_and_add_acquire_read(addr, 1)
# define MK_AO_HAVE_char_fetch_and_add1_acquire_read
#endif
#if defined(MK_AO_HAVE_char_fetch_and_add) \
    && !defined(MK_AO_HAVE_char_fetch_and_add1)
# define MK_AO_char_fetch_and_add1(addr) MK_AO_char_fetch_and_add(addr, 1)
# define MK_AO_HAVE_char_fetch_and_add1
#endif

#if defined(MK_AO_HAVE_char_fetch_and_add1_full)
# if !defined(MK_AO_HAVE_char_fetch_and_add1_release)
#   define MK_AO_char_fetch_and_add1_release(addr) \
                                MK_AO_char_fetch_and_add1_full(addr)
#   define MK_AO_HAVE_char_fetch_and_add1_release
# endif
# if !defined(MK_AO_HAVE_char_fetch_and_add1_acquire)
#   define MK_AO_char_fetch_and_add1_acquire(addr) \
                                MK_AO_char_fetch_and_add1_full(addr)
#   define MK_AO_HAVE_char_fetch_and_add1_acquire
# endif
# if !defined(MK_AO_HAVE_char_fetch_and_add1_write)
#   define MK_AO_char_fetch_and_add1_write(addr) \
                                MK_AO_char_fetch_and_add1_full(addr)
#   define MK_AO_HAVE_char_fetch_and_add1_write
# endif
# if !defined(MK_AO_HAVE_char_fetch_and_add1_read)
#   define MK_AO_char_fetch_and_add1_read(addr) \
                                MK_AO_char_fetch_and_add1_full(addr)
#   define MK_AO_HAVE_char_fetch_and_add1_read
# endif
#endif /* MK_AO_HAVE_char_fetch_and_add1_full */

#if !defined(MK_AO_HAVE_char_fetch_and_add1) \
    && defined(MK_AO_HAVE_char_fetch_and_add1_release)
# define MK_AO_char_fetch_and_add1(addr) MK_AO_char_fetch_and_add1_release(addr)
# define MK_AO_HAVE_char_fetch_and_add1
#endif
#if !defined(MK_AO_HAVE_char_fetch_and_add1) \
    && defined(MK_AO_HAVE_char_fetch_and_add1_acquire)
# define MK_AO_char_fetch_and_add1(addr) MK_AO_char_fetch_and_add1_acquire(addr)
# define MK_AO_HAVE_char_fetch_and_add1
#endif
#if !defined(MK_AO_HAVE_char_fetch_and_add1) \
    && defined(MK_AO_HAVE_char_fetch_and_add1_write)
# define MK_AO_char_fetch_and_add1(addr) MK_AO_char_fetch_and_add1_write(addr)
# define MK_AO_HAVE_char_fetch_and_add1
#endif
#if !defined(MK_AO_HAVE_char_fetch_and_add1) \
    && defined(MK_AO_HAVE_char_fetch_and_add1_read)
# define MK_AO_char_fetch_and_add1(addr) MK_AO_char_fetch_and_add1_read(addr)
# define MK_AO_HAVE_char_fetch_and_add1
#endif

#if defined(MK_AO_HAVE_char_fetch_and_add1_acquire) \
    && defined(MK_AO_HAVE_nop_full) \
    && !defined(MK_AO_HAVE_char_fetch_and_add1_full)
# define MK_AO_char_fetch_and_add1_full(addr) \
                        (MK_AO_nop_full(), MK_AO_char_fetch_and_add1_acquire(addr))
# define MK_AO_HAVE_char_fetch_and_add1_full
#endif

#if !defined(MK_AO_HAVE_char_fetch_and_add1_release_write) \
    && defined(MK_AO_HAVE_char_fetch_and_add1_write)
# define MK_AO_char_fetch_and_add1_release_write(addr) \
                                MK_AO_char_fetch_and_add1_write(addr)
# define MK_AO_HAVE_char_fetch_and_add1_release_write
#endif
#if !defined(MK_AO_HAVE_char_fetch_and_add1_release_write) \
    && defined(MK_AO_HAVE_char_fetch_and_add1_release)
# define MK_AO_char_fetch_and_add1_release_write(addr) \
                                MK_AO_char_fetch_and_add1_release(addr)
# define MK_AO_HAVE_char_fetch_and_add1_release_write
#endif
#if !defined(MK_AO_HAVE_char_fetch_and_add1_acquire_read) \
    && defined(MK_AO_HAVE_char_fetch_and_add1_read)
# define MK_AO_char_fetch_and_add1_acquire_read(addr) \
                                MK_AO_char_fetch_and_add1_read(addr)
# define MK_AO_HAVE_char_fetch_and_add1_acquire_read
#endif
#if !defined(MK_AO_HAVE_char_fetch_and_add1_acquire_read) \
    && defined(MK_AO_HAVE_char_fetch_and_add1_acquire)
# define MK_AO_char_fetch_and_add1_acquire_read(addr) \
                                MK_AO_char_fetch_and_add1_acquire(addr)
# define MK_AO_HAVE_char_fetch_and_add1_acquire_read
#endif

#ifdef MK_AO_NO_DD_ORDERING
# if defined(MK_AO_HAVE_char_fetch_and_add1_acquire_read)
#   define MK_AO_char_fetch_and_add1_dd_acquire_read(addr) \
                                MK_AO_char_fetch_and_add1_acquire_read(addr)
#   define MK_AO_HAVE_char_fetch_and_add1_dd_acquire_read
# endif
#else
# if defined(MK_AO_HAVE_char_fetch_and_add1)
#   define MK_AO_char_fetch_and_add1_dd_acquire_read(addr) \
                                MK_AO_char_fetch_and_add1(addr)
#   define MK_AO_HAVE_char_fetch_and_add1_dd_acquire_read
# endif
#endif /* !MK_AO_NO_DD_ORDERING */

/* char_fetch_and_sub1 */
#if defined(MK_AO_HAVE_char_fetch_and_add_full) \
    && !defined(MK_AO_HAVE_char_fetch_and_sub1_full)
# define MK_AO_char_fetch_and_sub1_full(addr) \
                MK_AO_char_fetch_and_add_full(addr, (unsigned/**/char)(-1))
# define MK_AO_HAVE_char_fetch_and_sub1_full
#endif
#if defined(MK_AO_HAVE_char_fetch_and_add_release) \
    && !defined(MK_AO_HAVE_char_fetch_and_sub1_release)
# define MK_AO_char_fetch_and_sub1_release(addr) \
                MK_AO_char_fetch_and_add_release(addr, (unsigned/**/char)(-1))
# define MK_AO_HAVE_char_fetch_and_sub1_release
#endif
#if defined(MK_AO_HAVE_char_fetch_and_add_acquire) \
    && !defined(MK_AO_HAVE_char_fetch_and_sub1_acquire)
# define MK_AO_char_fetch_and_sub1_acquire(addr) \
                MK_AO_char_fetch_and_add_acquire(addr, (unsigned/**/char)(-1))
# define MK_AO_HAVE_char_fetch_and_sub1_acquire
#endif
#if defined(MK_AO_HAVE_char_fetch_and_add_write) \
    && !defined(MK_AO_HAVE_char_fetch_and_sub1_write)
# define MK_AO_char_fetch_and_sub1_write(addr) \
                MK_AO_char_fetch_and_add_write(addr, (unsigned/**/char)(-1))
# define MK_AO_HAVE_char_fetch_and_sub1_write
#endif
#if defined(MK_AO_HAVE_char_fetch_and_add_read) \
    && !defined(MK_AO_HAVE_char_fetch_and_sub1_read)
# define MK_AO_char_fetch_and_sub1_read(addr) \
                MK_AO_char_fetch_and_add_read(addr, (unsigned/**/char)(-1))
# define MK_AO_HAVE_char_fetch_and_sub1_read
#endif
#if defined(MK_AO_HAVE_char_fetch_and_add_release_write) \
    && !defined(MK_AO_HAVE_char_fetch_and_sub1_release_write)
# define MK_AO_char_fetch_and_sub1_release_write(addr) \
                MK_AO_char_fetch_and_add_release_write(addr, (unsigned/**/char)(-1))
# define MK_AO_HAVE_char_fetch_and_sub1_release_write
#endif
#if defined(MK_AO_HAVE_char_fetch_and_add_acquire_read) \
    && !defined(MK_AO_HAVE_char_fetch_and_sub1_acquire_read)
# define MK_AO_char_fetch_and_sub1_acquire_read(addr) \
                MK_AO_char_fetch_and_add_acquire_read(addr, (unsigned/**/char)(-1))
# define MK_AO_HAVE_char_fetch_and_sub1_acquire_read
#endif
#if defined(MK_AO_HAVE_char_fetch_and_add) \
    && !defined(MK_AO_HAVE_char_fetch_and_sub1)
# define MK_AO_char_fetch_and_sub1(addr) \
                MK_AO_char_fetch_and_add(addr, (unsigned/**/char)(-1))
# define MK_AO_HAVE_char_fetch_and_sub1
#endif

#if defined(MK_AO_HAVE_char_fetch_and_sub1_full)
# if !defined(MK_AO_HAVE_char_fetch_and_sub1_release)
#   define MK_AO_char_fetch_and_sub1_release(addr) \
                                MK_AO_char_fetch_and_sub1_full(addr)
#   define MK_AO_HAVE_char_fetch_and_sub1_release
# endif
# if !defined(MK_AO_HAVE_char_fetch_and_sub1_acquire)
#   define MK_AO_char_fetch_and_sub1_acquire(addr) \
                                MK_AO_char_fetch_and_sub1_full(addr)
#   define MK_AO_HAVE_char_fetch_and_sub1_acquire
# endif
# if !defined(MK_AO_HAVE_char_fetch_and_sub1_write)
#   define MK_AO_char_fetch_and_sub1_write(addr) \
                                MK_AO_char_fetch_and_sub1_full(addr)
#   define MK_AO_HAVE_char_fetch_and_sub1_write
# endif
# if !defined(MK_AO_HAVE_char_fetch_and_sub1_read)
#   define MK_AO_char_fetch_and_sub1_read(addr) \
                                MK_AO_char_fetch_and_sub1_full(addr)
#   define MK_AO_HAVE_char_fetch_and_sub1_read
# endif
#endif /* MK_AO_HAVE_char_fetch_and_sub1_full */

#if !defined(MK_AO_HAVE_char_fetch_and_sub1) \
    && defined(MK_AO_HAVE_char_fetch_and_sub1_release)
# define MK_AO_char_fetch_and_sub1(addr) MK_AO_char_fetch_and_sub1_release(addr)
# define MK_AO_HAVE_char_fetch_and_sub1
#endif
#if !defined(MK_AO_HAVE_char_fetch_and_sub1) \
    && defined(MK_AO_HAVE_char_fetch_and_sub1_acquire)
# define MK_AO_char_fetch_and_sub1(addr) MK_AO_char_fetch_and_sub1_acquire(addr)
# define MK_AO_HAVE_char_fetch_and_sub1
#endif
#if !defined(MK_AO_HAVE_char_fetch_and_sub1) \
    && defined(MK_AO_HAVE_char_fetch_and_sub1_write)
# define MK_AO_char_fetch_and_sub1(addr) MK_AO_char_fetch_and_sub1_write(addr)
# define MK_AO_HAVE_char_fetch_and_sub1
#endif
#if !defined(MK_AO_HAVE_char_fetch_and_sub1) \
    && defined(MK_AO_HAVE_char_fetch_and_sub1_read)
# define MK_AO_char_fetch_and_sub1(addr) MK_AO_char_fetch_and_sub1_read(addr)
# define MK_AO_HAVE_char_fetch_and_sub1
#endif

#if defined(MK_AO_HAVE_char_fetch_and_sub1_acquire) \
    && defined(MK_AO_HAVE_nop_full) \
    && !defined(MK_AO_HAVE_char_fetch_and_sub1_full)
# define MK_AO_char_fetch_and_sub1_full(addr) \
                        (MK_AO_nop_full(), MK_AO_char_fetch_and_sub1_acquire(addr))
# define MK_AO_HAVE_char_fetch_and_sub1_full
#endif

#if !defined(MK_AO_HAVE_char_fetch_and_sub1_release_write) \
    && defined(MK_AO_HAVE_char_fetch_and_sub1_write)
# define MK_AO_char_fetch_and_sub1_release_write(addr) \
                                MK_AO_char_fetch_and_sub1_write(addr)
# define MK_AO_HAVE_char_fetch_and_sub1_release_write
#endif
#if !defined(MK_AO_HAVE_char_fetch_and_sub1_release_write) \
    && defined(MK_AO_HAVE_char_fetch_and_sub1_release)
# define MK_AO_char_fetch_and_sub1_release_write(addr) \
                                MK_AO_char_fetch_and_sub1_release(addr)
# define MK_AO_HAVE_char_fetch_and_sub1_release_write
#endif
#if !defined(MK_AO_HAVE_char_fetch_and_sub1_acquire_read) \
    && defined(MK_AO_HAVE_char_fetch_and_sub1_read)
# define MK_AO_char_fetch_and_sub1_acquire_read(addr) \
                                MK_AO_char_fetch_and_sub1_read(addr)
# define MK_AO_HAVE_char_fetch_and_sub1_acquire_read
#endif
#if !defined(MK_AO_HAVE_char_fetch_and_sub1_acquire_read) \
    && defined(MK_AO_HAVE_char_fetch_and_sub1_acquire)
# define MK_AO_char_fetch_and_sub1_acquire_read(addr) \
                                MK_AO_char_fetch_and_sub1_acquire(addr)
# define MK_AO_HAVE_char_fetch_and_sub1_acquire_read
#endif

#ifdef MK_AO_NO_DD_ORDERING
# if defined(MK_AO_HAVE_char_fetch_and_sub1_acquire_read)
#   define MK_AO_char_fetch_and_sub1_dd_acquire_read(addr) \
                                MK_AO_char_fetch_and_sub1_acquire_read(addr)
#   define MK_AO_HAVE_char_fetch_and_sub1_dd_acquire_read
# endif
#else
# if defined(MK_AO_HAVE_char_fetch_and_sub1)
#   define MK_AO_char_fetch_and_sub1_dd_acquire_read(addr) \
                                MK_AO_char_fetch_and_sub1(addr)
#   define MK_AO_HAVE_char_fetch_and_sub1_dd_acquire_read
# endif
#endif /* !MK_AO_NO_DD_ORDERING */

/* char_and */
#if defined(MK_AO_HAVE_char_compare_and_swap_full) \
    && !defined(MK_AO_HAVE_char_and_full)
  MK_AO_INLINE void
  MK_AO_char_and_full(volatile unsigned/**/char *addr, unsigned/**/char value)
  {
    unsigned/**/char old;

    do
      {
        old = *(unsigned/**/char *)addr;
      }
    while (MK_AO_EXPECT_FALSE(!MK_AO_char_compare_and_swap_full(addr, old,
                                                           old & value)));
  }
# define MK_AO_HAVE_char_and_full
#endif

#if defined(MK_AO_HAVE_char_and_full)
# if !defined(MK_AO_HAVE_char_and_release)
#   define MK_AO_char_and_release(addr, val) MK_AO_char_and_full(addr, val)
#   define MK_AO_HAVE_char_and_release
# endif
# if !defined(MK_AO_HAVE_char_and_acquire)
#   define MK_AO_char_and_acquire(addr, val) MK_AO_char_and_full(addr, val)
#   define MK_AO_HAVE_char_and_acquire
# endif
# if !defined(MK_AO_HAVE_char_and_write)
#   define MK_AO_char_and_write(addr, val) MK_AO_char_and_full(addr, val)
#   define MK_AO_HAVE_char_and_write
# endif
# if !defined(MK_AO_HAVE_char_and_read)
#   define MK_AO_char_and_read(addr, val) MK_AO_char_and_full(addr, val)
#   define MK_AO_HAVE_char_and_read
# endif
#endif /* MK_AO_HAVE_char_and_full */

#if !defined(MK_AO_HAVE_char_and) && defined(MK_AO_HAVE_char_and_release)
# define MK_AO_char_and(addr, val) MK_AO_char_and_release(addr, val)
# define MK_AO_HAVE_char_and
#endif
#if !defined(MK_AO_HAVE_char_and) && defined(MK_AO_HAVE_char_and_acquire)
# define MK_AO_char_and(addr, val) MK_AO_char_and_acquire(addr, val)
# define MK_AO_HAVE_char_and
#endif
#if !defined(MK_AO_HAVE_char_and) && defined(MK_AO_HAVE_char_and_write)
# define MK_AO_char_and(addr, val) MK_AO_char_and_write(addr, val)
# define MK_AO_HAVE_char_and
#endif
#if !defined(MK_AO_HAVE_char_and) && defined(MK_AO_HAVE_char_and_read)
# define MK_AO_char_and(addr, val) MK_AO_char_and_read(addr, val)
# define MK_AO_HAVE_char_and
#endif

#if defined(MK_AO_HAVE_char_and_acquire) && defined(MK_AO_HAVE_nop_full) \
    && !defined(MK_AO_HAVE_char_and_full)
# define MK_AO_char_and_full(addr, val) \
                        (MK_AO_nop_full(), MK_AO_char_and_acquire(addr, val))
# define MK_AO_HAVE_char_and_full
#endif

#if !defined(MK_AO_HAVE_char_and_release_write) \
    && defined(MK_AO_HAVE_char_and_write)
# define MK_AO_char_and_release_write(addr, val) MK_AO_char_and_write(addr, val)
# define MK_AO_HAVE_char_and_release_write
#endif
#if !defined(MK_AO_HAVE_char_and_release_write) \
    && defined(MK_AO_HAVE_char_and_release)
# define MK_AO_char_and_release_write(addr, val) MK_AO_char_and_release(addr, val)
# define MK_AO_HAVE_char_and_release_write
#endif
#if !defined(MK_AO_HAVE_char_and_acquire_read) \
    && defined(MK_AO_HAVE_char_and_read)
# define MK_AO_char_and_acquire_read(addr, val) MK_AO_char_and_read(addr, val)
# define MK_AO_HAVE_char_and_acquire_read
#endif
#if !defined(MK_AO_HAVE_char_and_acquire_read) \
    && defined(MK_AO_HAVE_char_and_acquire)
# define MK_AO_char_and_acquire_read(addr, val) MK_AO_char_and_acquire(addr, val)
# define MK_AO_HAVE_char_and_acquire_read
#endif

/* char_or */
#if defined(MK_AO_HAVE_char_compare_and_swap_full) \
    && !defined(MK_AO_HAVE_char_or_full)
  MK_AO_INLINE void
  MK_AO_char_or_full(volatile unsigned/**/char *addr, unsigned/**/char value)
  {
    unsigned/**/char old;

    do
      {
        old = *(unsigned/**/char *)addr;
      }
    while (MK_AO_EXPECT_FALSE(!MK_AO_char_compare_and_swap_full(addr, old,
                                                           old | value)));
  }
# define MK_AO_HAVE_char_or_full
#endif

#if defined(MK_AO_HAVE_char_or_full)
# if !defined(MK_AO_HAVE_char_or_release)
#   define MK_AO_char_or_release(addr, val) MK_AO_char_or_full(addr, val)
#   define MK_AO_HAVE_char_or_release
# endif
# if !defined(MK_AO_HAVE_char_or_acquire)
#   define MK_AO_char_or_acquire(addr, val) MK_AO_char_or_full(addr, val)
#   define MK_AO_HAVE_char_or_acquire
# endif
# if !defined(MK_AO_HAVE_char_or_write)
#   define MK_AO_char_or_write(addr, val) MK_AO_char_or_full(addr, val)
#   define MK_AO_HAVE_char_or_write
# endif
# if !defined(MK_AO_HAVE_char_or_read)
#   define MK_AO_char_or_read(addr, val) MK_AO_char_or_full(addr, val)
#   define MK_AO_HAVE_char_or_read
# endif
#endif /* MK_AO_HAVE_char_or_full */

#if !defined(MK_AO_HAVE_char_or) && defined(MK_AO_HAVE_char_or_release)
# define MK_AO_char_or(addr, val) MK_AO_char_or_release(addr, val)
# define MK_AO_HAVE_char_or
#endif
#if !defined(MK_AO_HAVE_char_or) && defined(MK_AO_HAVE_char_or_acquire)
# define MK_AO_char_or(addr, val) MK_AO_char_or_acquire(addr, val)
# define MK_AO_HAVE_char_or
#endif
#if !defined(MK_AO_HAVE_char_or) && defined(MK_AO_HAVE_char_or_write)
# define MK_AO_char_or(addr, val) MK_AO_char_or_write(addr, val)
# define MK_AO_HAVE_char_or
#endif
#if !defined(MK_AO_HAVE_char_or) && defined(MK_AO_HAVE_char_or_read)
# define MK_AO_char_or(addr, val) MK_AO_char_or_read(addr, val)
# define MK_AO_HAVE_char_or
#endif

#if defined(MK_AO_HAVE_char_or_acquire) && defined(MK_AO_HAVE_nop_full) \
    && !defined(MK_AO_HAVE_char_or_full)
# define MK_AO_char_or_full(addr, val) \
                        (MK_AO_nop_full(), MK_AO_char_or_acquire(addr, val))
# define MK_AO_HAVE_char_or_full
#endif

#if !defined(MK_AO_HAVE_char_or_release_write) \
    && defined(MK_AO_HAVE_char_or_write)
# define MK_AO_char_or_release_write(addr, val) MK_AO_char_or_write(addr, val)
# define MK_AO_HAVE_char_or_release_write
#endif
#if !defined(MK_AO_HAVE_char_or_release_write) \
    && defined(MK_AO_HAVE_char_or_release)
# define MK_AO_char_or_release_write(addr, val) MK_AO_char_or_release(addr, val)
# define MK_AO_HAVE_char_or_release_write
#endif
#if !defined(MK_AO_HAVE_char_or_acquire_read) && defined(MK_AO_HAVE_char_or_read)
# define MK_AO_char_or_acquire_read(addr, val) MK_AO_char_or_read(addr, val)
# define MK_AO_HAVE_char_or_acquire_read
#endif
#if !defined(MK_AO_HAVE_char_or_acquire_read) \
    && defined(MK_AO_HAVE_char_or_acquire)
# define MK_AO_char_or_acquire_read(addr, val) MK_AO_char_or_acquire(addr, val)
# define MK_AO_HAVE_char_or_acquire_read
#endif

/* char_xor */
#if defined(MK_AO_HAVE_char_compare_and_swap_full) \
    && !defined(MK_AO_HAVE_char_xor_full)
  MK_AO_INLINE void
  MK_AO_char_xor_full(volatile unsigned/**/char *addr, unsigned/**/char value)
  {
    unsigned/**/char old;

    do
      {
        old = *(unsigned/**/char *)addr;
      }
    while (MK_AO_EXPECT_FALSE(!MK_AO_char_compare_and_swap_full(addr, old,
                                                           old ^ value)));
  }
# define MK_AO_HAVE_char_xor_full
#endif

#if defined(MK_AO_HAVE_char_xor_full)
# if !defined(MK_AO_HAVE_char_xor_release)
#   define MK_AO_char_xor_release(addr, val) MK_AO_char_xor_full(addr, val)
#   define MK_AO_HAVE_char_xor_release
# endif
# if !defined(MK_AO_HAVE_char_xor_acquire)
#   define MK_AO_char_xor_acquire(addr, val) MK_AO_char_xor_full(addr, val)
#   define MK_AO_HAVE_char_xor_acquire
# endif
# if !defined(MK_AO_HAVE_char_xor_write)
#   define MK_AO_char_xor_write(addr, val) MK_AO_char_xor_full(addr, val)
#   define MK_AO_HAVE_char_xor_write
# endif
# if !defined(MK_AO_HAVE_char_xor_read)
#   define MK_AO_char_xor_read(addr, val) MK_AO_char_xor_full(addr, val)
#   define MK_AO_HAVE_char_xor_read
# endif
#endif /* MK_AO_HAVE_char_xor_full */

#if !defined(MK_AO_HAVE_char_xor) && defined(MK_AO_HAVE_char_xor_release)
# define MK_AO_char_xor(addr, val) MK_AO_char_xor_release(addr, val)
# define MK_AO_HAVE_char_xor
#endif
#if !defined(MK_AO_HAVE_char_xor) && defined(MK_AO_HAVE_char_xor_acquire)
# define MK_AO_char_xor(addr, val) MK_AO_char_xor_acquire(addr, val)
# define MK_AO_HAVE_char_xor
#endif
#if !defined(MK_AO_HAVE_char_xor) && defined(MK_AO_HAVE_char_xor_write)
# define MK_AO_char_xor(addr, val) MK_AO_char_xor_write(addr, val)
# define MK_AO_HAVE_char_xor
#endif
#if !defined(MK_AO_HAVE_char_xor) && defined(MK_AO_HAVE_char_xor_read)
# define MK_AO_char_xor(addr, val) MK_AO_char_xor_read(addr, val)
# define MK_AO_HAVE_char_xor
#endif

#if defined(MK_AO_HAVE_char_xor_acquire) && defined(MK_AO_HAVE_nop_full) \
    && !defined(MK_AO_HAVE_char_xor_full)
# define MK_AO_char_xor_full(addr, val) \
                        (MK_AO_nop_full(), MK_AO_char_xor_acquire(addr, val))
# define MK_AO_HAVE_char_xor_full
#endif

#if !defined(MK_AO_HAVE_char_xor_release_write) \
    && defined(MK_AO_HAVE_char_xor_write)
# define MK_AO_char_xor_release_write(addr, val) MK_AO_char_xor_write(addr, val)
# define MK_AO_HAVE_char_xor_release_write
#endif
#if !defined(MK_AO_HAVE_char_xor_release_write) \
    && defined(MK_AO_HAVE_char_xor_release)
# define MK_AO_char_xor_release_write(addr, val) MK_AO_char_xor_release(addr, val)
# define MK_AO_HAVE_char_xor_release_write
#endif
#if !defined(MK_AO_HAVE_char_xor_acquire_read) \
    && defined(MK_AO_HAVE_char_xor_read)
# define MK_AO_char_xor_acquire_read(addr, val) MK_AO_char_xor_read(addr, val)
# define MK_AO_HAVE_char_xor_acquire_read
#endif
#if !defined(MK_AO_HAVE_char_xor_acquire_read) \
    && defined(MK_AO_HAVE_char_xor_acquire)
# define MK_AO_char_xor_acquire_read(addr, val) MK_AO_char_xor_acquire(addr, val)
# define MK_AO_HAVE_char_xor_acquire_read
#endif

/* char_and/or/xor_dd_acquire_read are meaningless.    */
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

/* short_compare_and_swap (based on fetch_compare_and_swap) */
#if defined(MK_AO_HAVE_short_fetch_compare_and_swap_full) \
    && !defined(MK_AO_HAVE_short_compare_and_swap_full)
  MK_AO_INLINE int
  MK_AO_short_compare_and_swap_full(volatile unsigned/**/short *addr, unsigned/**/short old_val,
                                 unsigned/**/short new_val)
  {
    return MK_AO_short_fetch_compare_and_swap_full(addr, old_val, new_val)
             == old_val;
  }
# define MK_AO_HAVE_short_compare_and_swap_full
#endif

#if defined(MK_AO_HAVE_short_fetch_compare_and_swap_acquire) \
    && !defined(MK_AO_HAVE_short_compare_and_swap_acquire)
  MK_AO_INLINE int
  MK_AO_short_compare_and_swap_acquire(volatile unsigned/**/short *addr, unsigned/**/short old_val,
                                    unsigned/**/short new_val)
  {
    return MK_AO_short_fetch_compare_and_swap_acquire(addr, old_val, new_val)
             == old_val;
  }
# define MK_AO_HAVE_short_compare_and_swap_acquire
#endif

#if defined(MK_AO_HAVE_short_fetch_compare_and_swap_release) \
    && !defined(MK_AO_HAVE_short_compare_and_swap_release)
  MK_AO_INLINE int
  MK_AO_short_compare_and_swap_release(volatile unsigned/**/short *addr, unsigned/**/short old_val,
                                    unsigned/**/short new_val)
  {
    return MK_AO_short_fetch_compare_and_swap_release(addr, old_val, new_val)
             == old_val;
  }
# define MK_AO_HAVE_short_compare_and_swap_release
#endif

#if defined(MK_AO_HAVE_short_fetch_compare_and_swap_write) \
    && !defined(MK_AO_HAVE_short_compare_and_swap_write)
  MK_AO_INLINE int
  MK_AO_short_compare_and_swap_write(volatile unsigned/**/short *addr, unsigned/**/short old_val,
                                  unsigned/**/short new_val)
  {
    return MK_AO_short_fetch_compare_and_swap_write(addr, old_val, new_val)
             == old_val;
  }
# define MK_AO_HAVE_short_compare_and_swap_write
#endif

#if defined(MK_AO_HAVE_short_fetch_compare_and_swap_read) \
    && !defined(MK_AO_HAVE_short_compare_and_swap_read)
  MK_AO_INLINE int
  MK_AO_short_compare_and_swap_read(volatile unsigned/**/short *addr, unsigned/**/short old_val,
                                 unsigned/**/short new_val)
  {
    return MK_AO_short_fetch_compare_and_swap_read(addr, old_val, new_val)
             == old_val;
  }
# define MK_AO_HAVE_short_compare_and_swap_read
#endif

#if defined(MK_AO_HAVE_short_fetch_compare_and_swap) \
    && !defined(MK_AO_HAVE_short_compare_and_swap)
  MK_AO_INLINE int
  MK_AO_short_compare_and_swap(volatile unsigned/**/short *addr, unsigned/**/short old_val,
                            unsigned/**/short new_val)
  {
    return MK_AO_short_fetch_compare_and_swap(addr, old_val, new_val) == old_val;
  }
# define MK_AO_HAVE_short_compare_and_swap
#endif

#if defined(MK_AO_HAVE_short_fetch_compare_and_swap_release_write) \
    && !defined(MK_AO_HAVE_short_compare_and_swap_release_write)
  MK_AO_INLINE int
  MK_AO_short_compare_and_swap_release_write(volatile unsigned/**/short *addr,
                                          unsigned/**/short old_val, unsigned/**/short new_val)
  {
    return MK_AO_short_fetch_compare_and_swap_release_write(addr, old_val,
                                                         new_val) == old_val;
  }
# define MK_AO_HAVE_short_compare_and_swap_release_write
#endif

#if defined(MK_AO_HAVE_short_fetch_compare_and_swap_acquire_read) \
    && !defined(MK_AO_HAVE_short_compare_and_swap_acquire_read)
  MK_AO_INLINE int
  MK_AO_short_compare_and_swap_acquire_read(volatile unsigned/**/short *addr,
                                         unsigned/**/short old_val, unsigned/**/short new_val)
  {
    return MK_AO_short_fetch_compare_and_swap_acquire_read(addr, old_val,
                                                        new_val) == old_val;
  }
# define MK_AO_HAVE_short_compare_and_swap_acquire_read
#endif

#if defined(MK_AO_HAVE_short_fetch_compare_and_swap_dd_acquire_read) \
    && !defined(MK_AO_HAVE_short_compare_and_swap_dd_acquire_read)
  MK_AO_INLINE int
  MK_AO_short_compare_and_swap_dd_acquire_read(volatile unsigned/**/short *addr,
                                            unsigned/**/short old_val, unsigned/**/short new_val)
  {
    return MK_AO_short_fetch_compare_and_swap_dd_acquire_read(addr, old_val,
                                                           new_val) == old_val;
  }
# define MK_AO_HAVE_short_compare_and_swap_dd_acquire_read
#endif

/* short_fetch_and_add */
/* We first try to implement fetch_and_add variants in terms of the     */
/* corresponding compare_and_swap variants to minimize adding barriers. */
#if defined(MK_AO_HAVE_short_compare_and_swap_full) \
    && !defined(MK_AO_HAVE_short_fetch_and_add_full)
  MK_AO_INLINE unsigned/**/short
  MK_AO_short_fetch_and_add_full(volatile unsigned/**/short *addr, unsigned/**/short incr)
  {
    unsigned/**/short old;

    do
      {
        old = *(unsigned/**/short *)addr;
      }
    while (MK_AO_EXPECT_FALSE(!MK_AO_short_compare_and_swap_full(addr, old,
                                                           old + incr)));
    return old;
  }
# define MK_AO_HAVE_short_fetch_and_add_full
#endif

#if defined(MK_AO_HAVE_short_compare_and_swap_acquire) \
    && !defined(MK_AO_HAVE_short_fetch_and_add_acquire)
  MK_AO_INLINE unsigned/**/short
  MK_AO_short_fetch_and_add_acquire(volatile unsigned/**/short *addr, unsigned/**/short incr)
  {
    unsigned/**/short old;

    do
      {
        old = *(unsigned/**/short *)addr;
      }
    while (MK_AO_EXPECT_FALSE(!MK_AO_short_compare_and_swap_acquire(addr, old,
                                                              old + incr)));
    return old;
  }
# define MK_AO_HAVE_short_fetch_and_add_acquire
#endif

#if defined(MK_AO_HAVE_short_compare_and_swap_release) \
    && !defined(MK_AO_HAVE_short_fetch_and_add_release)
  MK_AO_INLINE unsigned/**/short
  MK_AO_short_fetch_and_add_release(volatile unsigned/**/short *addr, unsigned/**/short incr)
  {
    unsigned/**/short old;

    do
      {
        old = *(unsigned/**/short *)addr;
      }
    while (MK_AO_EXPECT_FALSE(!MK_AO_short_compare_and_swap_release(addr, old,
                                                              old + incr)));
    return old;
  }
# define MK_AO_HAVE_short_fetch_and_add_release
#endif

#if defined(MK_AO_HAVE_short_compare_and_swap) \
    && !defined(MK_AO_HAVE_short_fetch_and_add)
  MK_AO_INLINE unsigned/**/short
  MK_AO_short_fetch_and_add(volatile unsigned/**/short *addr, unsigned/**/short incr)
  {
    unsigned/**/short old;

    do
      {
        old = *(unsigned/**/short *)addr;
      }
    while (MK_AO_EXPECT_FALSE(!MK_AO_short_compare_and_swap(addr, old,
                                                      old + incr)));
    return old;
  }
# define MK_AO_HAVE_short_fetch_and_add
#endif

#if defined(MK_AO_HAVE_short_fetch_and_add_full)
# if !defined(MK_AO_HAVE_short_fetch_and_add_release)
#   define MK_AO_short_fetch_and_add_release(addr, val) \
                                MK_AO_short_fetch_and_add_full(addr, val)
#   define MK_AO_HAVE_short_fetch_and_add_release
# endif
# if !defined(MK_AO_HAVE_short_fetch_and_add_acquire)
#   define MK_AO_short_fetch_and_add_acquire(addr, val) \
                                MK_AO_short_fetch_and_add_full(addr, val)
#   define MK_AO_HAVE_short_fetch_and_add_acquire
# endif
# if !defined(MK_AO_HAVE_short_fetch_and_add_write)
#   define MK_AO_short_fetch_and_add_write(addr, val) \
                                MK_AO_short_fetch_and_add_full(addr, val)
#   define MK_AO_HAVE_short_fetch_and_add_write
# endif
# if !defined(MK_AO_HAVE_short_fetch_and_add_read)
#   define MK_AO_short_fetch_and_add_read(addr, val) \
                                MK_AO_short_fetch_and_add_full(addr, val)
#   define MK_AO_HAVE_short_fetch_and_add_read
# endif
#endif /* MK_AO_HAVE_short_fetch_and_add_full */

#if defined(MK_AO_HAVE_short_fetch_and_add) && defined(MK_AO_HAVE_nop_full) \
    && !defined(MK_AO_HAVE_short_fetch_and_add_acquire)
  MK_AO_INLINE unsigned/**/short
  MK_AO_short_fetch_and_add_acquire(volatile unsigned/**/short *addr, unsigned/**/short incr)
  {
    unsigned/**/short result = MK_AO_short_fetch_and_add(addr, incr);
    MK_AO_nop_full();
    return result;
  }
# define MK_AO_HAVE_short_fetch_and_add_acquire
#endif
#if defined(MK_AO_HAVE_short_fetch_and_add) && defined(MK_AO_HAVE_nop_full) \
    && !defined(MK_AO_HAVE_short_fetch_and_add_release)
# define MK_AO_short_fetch_and_add_release(addr, incr) \
                (MK_AO_nop_full(), MK_AO_short_fetch_and_add(addr, incr))
# define MK_AO_HAVE_short_fetch_and_add_release
#endif

#if !defined(MK_AO_HAVE_short_fetch_and_add) \
    && defined(MK_AO_HAVE_short_fetch_and_add_release)
# define MK_AO_short_fetch_and_add(addr, val) \
                                MK_AO_short_fetch_and_add_release(addr, val)
# define MK_AO_HAVE_short_fetch_and_add
#endif
#if !defined(MK_AO_HAVE_short_fetch_and_add) \
    && defined(MK_AO_HAVE_short_fetch_and_add_acquire)
# define MK_AO_short_fetch_and_add(addr, val) \
                                MK_AO_short_fetch_and_add_acquire(addr, val)
# define MK_AO_HAVE_short_fetch_and_add
#endif
#if !defined(MK_AO_HAVE_short_fetch_and_add) \
    && defined(MK_AO_HAVE_short_fetch_and_add_write)
# define MK_AO_short_fetch_and_add(addr, val) \
                                MK_AO_short_fetch_and_add_write(addr, val)
# define MK_AO_HAVE_short_fetch_and_add
#endif
#if !defined(MK_AO_HAVE_short_fetch_and_add) \
    && defined(MK_AO_HAVE_short_fetch_and_add_read)
# define MK_AO_short_fetch_and_add(addr, val) \
                                MK_AO_short_fetch_and_add_read(addr, val)
# define MK_AO_HAVE_short_fetch_and_add
#endif

#if defined(MK_AO_HAVE_short_fetch_and_add_acquire) \
    && defined(MK_AO_HAVE_nop_full) && !defined(MK_AO_HAVE_short_fetch_and_add_full)
# define MK_AO_short_fetch_and_add_full(addr, val) \
                (MK_AO_nop_full(), MK_AO_short_fetch_and_add_acquire(addr, val))
# define MK_AO_HAVE_short_fetch_and_add_full
#endif

#if !defined(MK_AO_HAVE_short_fetch_and_add_release_write) \
    && defined(MK_AO_HAVE_short_fetch_and_add_write)
# define MK_AO_short_fetch_and_add_release_write(addr, val) \
                                MK_AO_short_fetch_and_add_write(addr, val)
# define MK_AO_HAVE_short_fetch_and_add_release_write
#endif
#if !defined(MK_AO_HAVE_short_fetch_and_add_release_write) \
    && defined(MK_AO_HAVE_short_fetch_and_add_release)
# define MK_AO_short_fetch_and_add_release_write(addr, val) \
                                MK_AO_short_fetch_and_add_release(addr, val)
# define MK_AO_HAVE_short_fetch_and_add_release_write
#endif

#if !defined(MK_AO_HAVE_short_fetch_and_add_acquire_read) \
    && defined(MK_AO_HAVE_short_fetch_and_add_read)
# define MK_AO_short_fetch_and_add_acquire_read(addr, val) \
                                MK_AO_short_fetch_and_add_read(addr, val)
# define MK_AO_HAVE_short_fetch_and_add_acquire_read
#endif
#if !defined(MK_AO_HAVE_short_fetch_and_add_acquire_read) \
    && defined(MK_AO_HAVE_short_fetch_and_add_acquire)
# define MK_AO_short_fetch_and_add_acquire_read(addr, val) \
                                MK_AO_short_fetch_and_add_acquire(addr, val)
# define MK_AO_HAVE_short_fetch_and_add_acquire_read
#endif

#ifdef MK_AO_NO_DD_ORDERING
# if defined(MK_AO_HAVE_short_fetch_and_add_acquire_read)
#   define MK_AO_short_fetch_and_add_dd_acquire_read(addr, val) \
                                MK_AO_short_fetch_and_add_acquire_read(addr, val)
#   define MK_AO_HAVE_short_fetch_and_add_dd_acquire_read
# endif
#else
# if defined(MK_AO_HAVE_short_fetch_and_add)
#   define MK_AO_short_fetch_and_add_dd_acquire_read(addr, val) \
                                MK_AO_short_fetch_and_add(addr, val)
#   define MK_AO_HAVE_short_fetch_and_add_dd_acquire_read
# endif
#endif /* !MK_AO_NO_DD_ORDERING */

/* short_fetch_and_add1 */
#if defined(MK_AO_HAVE_short_fetch_and_add_full) \
    && !defined(MK_AO_HAVE_short_fetch_and_add1_full)
# define MK_AO_short_fetch_and_add1_full(addr) \
                                MK_AO_short_fetch_and_add_full(addr, 1)
# define MK_AO_HAVE_short_fetch_and_add1_full
#endif
#if defined(MK_AO_HAVE_short_fetch_and_add_release) \
    && !defined(MK_AO_HAVE_short_fetch_and_add1_release)
# define MK_AO_short_fetch_and_add1_release(addr) \
                                MK_AO_short_fetch_and_add_release(addr, 1)
# define MK_AO_HAVE_short_fetch_and_add1_release
#endif
#if defined(MK_AO_HAVE_short_fetch_and_add_acquire) \
    && !defined(MK_AO_HAVE_short_fetch_and_add1_acquire)
# define MK_AO_short_fetch_and_add1_acquire(addr) \
                                MK_AO_short_fetch_and_add_acquire(addr, 1)
# define MK_AO_HAVE_short_fetch_and_add1_acquire
#endif
#if defined(MK_AO_HAVE_short_fetch_and_add_write) \
    && !defined(MK_AO_HAVE_short_fetch_and_add1_write)
# define MK_AO_short_fetch_and_add1_write(addr) \
                                MK_AO_short_fetch_and_add_write(addr, 1)
# define MK_AO_HAVE_short_fetch_and_add1_write
#endif
#if defined(MK_AO_HAVE_short_fetch_and_add_read) \
    && !defined(MK_AO_HAVE_short_fetch_and_add1_read)
# define MK_AO_short_fetch_and_add1_read(addr) \
                                MK_AO_short_fetch_and_add_read(addr, 1)
# define MK_AO_HAVE_short_fetch_and_add1_read
#endif
#if defined(MK_AO_HAVE_short_fetch_and_add_release_write) \
    && !defined(MK_AO_HAVE_short_fetch_and_add1_release_write)
# define MK_AO_short_fetch_and_add1_release_write(addr) \
                                MK_AO_short_fetch_and_add_release_write(addr, 1)
# define MK_AO_HAVE_short_fetch_and_add1_release_write
#endif
#if defined(MK_AO_HAVE_short_fetch_and_add_acquire_read) \
    && !defined(MK_AO_HAVE_short_fetch_and_add1_acquire_read)
# define MK_AO_short_fetch_and_add1_acquire_read(addr) \
                                MK_AO_short_fetch_and_add_acquire_read(addr, 1)
# define MK_AO_HAVE_short_fetch_and_add1_acquire_read
#endif
#if defined(MK_AO_HAVE_short_fetch_and_add) \
    && !defined(MK_AO_HAVE_short_fetch_and_add1)
# define MK_AO_short_fetch_and_add1(addr) MK_AO_short_fetch_and_add(addr, 1)
# define MK_AO_HAVE_short_fetch_and_add1
#endif

#if defined(MK_AO_HAVE_short_fetch_and_add1_full)
# if !defined(MK_AO_HAVE_short_fetch_and_add1_release)
#   define MK_AO_short_fetch_and_add1_release(addr) \
                                MK_AO_short_fetch_and_add1_full(addr)
#   define MK_AO_HAVE_short_fetch_and_add1_release
# endif
# if !defined(MK_AO_HAVE_short_fetch_and_add1_acquire)
#   define MK_AO_short_fetch_and_add1_acquire(addr) \
                                MK_AO_short_fetch_and_add1_full(addr)
#   define MK_AO_HAVE_short_fetch_and_add1_acquire
# endif
# if !defined(MK_AO_HAVE_short_fetch_and_add1_write)
#   define MK_AO_short_fetch_and_add1_write(addr) \
                                MK_AO_short_fetch_and_add1_full(addr)
#   define MK_AO_HAVE_short_fetch_and_add1_write
# endif
# if !defined(MK_AO_HAVE_short_fetch_and_add1_read)
#   define MK_AO_short_fetch_and_add1_read(addr) \
                                MK_AO_short_fetch_and_add1_full(addr)
#   define MK_AO_HAVE_short_fetch_and_add1_read
# endif
#endif /* MK_AO_HAVE_short_fetch_and_add1_full */

#if !defined(MK_AO_HAVE_short_fetch_and_add1) \
    && defined(MK_AO_HAVE_short_fetch_and_add1_release)
# define MK_AO_short_fetch_and_add1(addr) MK_AO_short_fetch_and_add1_release(addr)
# define MK_AO_HAVE_short_fetch_and_add1
#endif
#if !defined(MK_AO_HAVE_short_fetch_and_add1) \
    && defined(MK_AO_HAVE_short_fetch_and_add1_acquire)
# define MK_AO_short_fetch_and_add1(addr) MK_AO_short_fetch_and_add1_acquire(addr)
# define MK_AO_HAVE_short_fetch_and_add1
#endif
#if !defined(MK_AO_HAVE_short_fetch_and_add1) \
    && defined(MK_AO_HAVE_short_fetch_and_add1_write)
# define MK_AO_short_fetch_and_add1(addr) MK_AO_short_fetch_and_add1_write(addr)
# define MK_AO_HAVE_short_fetch_and_add1
#endif
#if !defined(MK_AO_HAVE_short_fetch_and_add1) \
    && defined(MK_AO_HAVE_short_fetch_and_add1_read)
# define MK_AO_short_fetch_and_add1(addr) MK_AO_short_fetch_and_add1_read(addr)
# define MK_AO_HAVE_short_fetch_and_add1
#endif

#if defined(MK_AO_HAVE_short_fetch_and_add1_acquire) \
    && defined(MK_AO_HAVE_nop_full) \
    && !defined(MK_AO_HAVE_short_fetch_and_add1_full)
# define MK_AO_short_fetch_and_add1_full(addr) \
                        (MK_AO_nop_full(), MK_AO_short_fetch_and_add1_acquire(addr))
# define MK_AO_HAVE_short_fetch_and_add1_full
#endif

#if !defined(MK_AO_HAVE_short_fetch_and_add1_release_write) \
    && defined(MK_AO_HAVE_short_fetch_and_add1_write)
# define MK_AO_short_fetch_and_add1_release_write(addr) \
                                MK_AO_short_fetch_and_add1_write(addr)
# define MK_AO_HAVE_short_fetch_and_add1_release_write
#endif
#if !defined(MK_AO_HAVE_short_fetch_and_add1_release_write) \
    && defined(MK_AO_HAVE_short_fetch_and_add1_release)
# define MK_AO_short_fetch_and_add1_release_write(addr) \
                                MK_AO_short_fetch_and_add1_release(addr)
# define MK_AO_HAVE_short_fetch_and_add1_release_write
#endif
#if !defined(MK_AO_HAVE_short_fetch_and_add1_acquire_read) \
    && defined(MK_AO_HAVE_short_fetch_and_add1_read)
# define MK_AO_short_fetch_and_add1_acquire_read(addr) \
                                MK_AO_short_fetch_and_add1_read(addr)
# define MK_AO_HAVE_short_fetch_and_add1_acquire_read
#endif
#if !defined(MK_AO_HAVE_short_fetch_and_add1_acquire_read) \
    && defined(MK_AO_HAVE_short_fetch_and_add1_acquire)
# define MK_AO_short_fetch_and_add1_acquire_read(addr) \
                                MK_AO_short_fetch_and_add1_acquire(addr)
# define MK_AO_HAVE_short_fetch_and_add1_acquire_read
#endif

#ifdef MK_AO_NO_DD_ORDERING
# if defined(MK_AO_HAVE_short_fetch_and_add1_acquire_read)
#   define MK_AO_short_fetch_and_add1_dd_acquire_read(addr) \
                                MK_AO_short_fetch_and_add1_acquire_read(addr)
#   define MK_AO_HAVE_short_fetch_and_add1_dd_acquire_read
# endif
#else
# if defined(MK_AO_HAVE_short_fetch_and_add1)
#   define MK_AO_short_fetch_and_add1_dd_acquire_read(addr) \
                                MK_AO_short_fetch_and_add1(addr)
#   define MK_AO_HAVE_short_fetch_and_add1_dd_acquire_read
# endif
#endif /* !MK_AO_NO_DD_ORDERING */

/* short_fetch_and_sub1 */
#if defined(MK_AO_HAVE_short_fetch_and_add_full) \
    && !defined(MK_AO_HAVE_short_fetch_and_sub1_full)
# define MK_AO_short_fetch_and_sub1_full(addr) \
                MK_AO_short_fetch_and_add_full(addr, (unsigned/**/short)(-1))
# define MK_AO_HAVE_short_fetch_and_sub1_full
#endif
#if defined(MK_AO_HAVE_short_fetch_and_add_release) \
    && !defined(MK_AO_HAVE_short_fetch_and_sub1_release)
# define MK_AO_short_fetch_and_sub1_release(addr) \
                MK_AO_short_fetch_and_add_release(addr, (unsigned/**/short)(-1))
# define MK_AO_HAVE_short_fetch_and_sub1_release
#endif
#if defined(MK_AO_HAVE_short_fetch_and_add_acquire) \
    && !defined(MK_AO_HAVE_short_fetch_and_sub1_acquire)
# define MK_AO_short_fetch_and_sub1_acquire(addr) \
                MK_AO_short_fetch_and_add_acquire(addr, (unsigned/**/short)(-1))
# define MK_AO_HAVE_short_fetch_and_sub1_acquire
#endif
#if defined(MK_AO_HAVE_short_fetch_and_add_write) \
    && !defined(MK_AO_HAVE_short_fetch_and_sub1_write)
# define MK_AO_short_fetch_and_sub1_write(addr) \
                MK_AO_short_fetch_and_add_write(addr, (unsigned/**/short)(-1))
# define MK_AO_HAVE_short_fetch_and_sub1_write
#endif
#if defined(MK_AO_HAVE_short_fetch_and_add_read) \
    && !defined(MK_AO_HAVE_short_fetch_and_sub1_read)
# define MK_AO_short_fetch_and_sub1_read(addr) \
                MK_AO_short_fetch_and_add_read(addr, (unsigned/**/short)(-1))
# define MK_AO_HAVE_short_fetch_and_sub1_read
#endif
#if defined(MK_AO_HAVE_short_fetch_and_add_release_write) \
    && !defined(MK_AO_HAVE_short_fetch_and_sub1_release_write)
# define MK_AO_short_fetch_and_sub1_release_write(addr) \
                MK_AO_short_fetch_and_add_release_write(addr, (unsigned/**/short)(-1))
# define MK_AO_HAVE_short_fetch_and_sub1_release_write
#endif
#if defined(MK_AO_HAVE_short_fetch_and_add_acquire_read) \
    && !defined(MK_AO_HAVE_short_fetch_and_sub1_acquire_read)
# define MK_AO_short_fetch_and_sub1_acquire_read(addr) \
                MK_AO_short_fetch_and_add_acquire_read(addr, (unsigned/**/short)(-1))
# define MK_AO_HAVE_short_fetch_and_sub1_acquire_read
#endif
#if defined(MK_AO_HAVE_short_fetch_and_add) \
    && !defined(MK_AO_HAVE_short_fetch_and_sub1)
# define MK_AO_short_fetch_and_sub1(addr) \
                MK_AO_short_fetch_and_add(addr, (unsigned/**/short)(-1))
# define MK_AO_HAVE_short_fetch_and_sub1
#endif

#if defined(MK_AO_HAVE_short_fetch_and_sub1_full)
# if !defined(MK_AO_HAVE_short_fetch_and_sub1_release)
#   define MK_AO_short_fetch_and_sub1_release(addr) \
                                MK_AO_short_fetch_and_sub1_full(addr)
#   define MK_AO_HAVE_short_fetch_and_sub1_release
# endif
# if !defined(MK_AO_HAVE_short_fetch_and_sub1_acquire)
#   define MK_AO_short_fetch_and_sub1_acquire(addr) \
                                MK_AO_short_fetch_and_sub1_full(addr)
#   define MK_AO_HAVE_short_fetch_and_sub1_acquire
# endif
# if !defined(MK_AO_HAVE_short_fetch_and_sub1_write)
#   define MK_AO_short_fetch_and_sub1_write(addr) \
                                MK_AO_short_fetch_and_sub1_full(addr)
#   define MK_AO_HAVE_short_fetch_and_sub1_write
# endif
# if !defined(MK_AO_HAVE_short_fetch_and_sub1_read)
#   define MK_AO_short_fetch_and_sub1_read(addr) \
                                MK_AO_short_fetch_and_sub1_full(addr)
#   define MK_AO_HAVE_short_fetch_and_sub1_read
# endif
#endif /* MK_AO_HAVE_short_fetch_and_sub1_full */

#if !defined(MK_AO_HAVE_short_fetch_and_sub1) \
    && defined(MK_AO_HAVE_short_fetch_and_sub1_release)
# define MK_AO_short_fetch_and_sub1(addr) MK_AO_short_fetch_and_sub1_release(addr)
# define MK_AO_HAVE_short_fetch_and_sub1
#endif
#if !defined(MK_AO_HAVE_short_fetch_and_sub1) \
    && defined(MK_AO_HAVE_short_fetch_and_sub1_acquire)
# define MK_AO_short_fetch_and_sub1(addr) MK_AO_short_fetch_and_sub1_acquire(addr)
# define MK_AO_HAVE_short_fetch_and_sub1
#endif
#if !defined(MK_AO_HAVE_short_fetch_and_sub1) \
    && defined(MK_AO_HAVE_short_fetch_and_sub1_write)
# define MK_AO_short_fetch_and_sub1(addr) MK_AO_short_fetch_and_sub1_write(addr)
# define MK_AO_HAVE_short_fetch_and_sub1
#endif
#if !defined(MK_AO_HAVE_short_fetch_and_sub1) \
    && defined(MK_AO_HAVE_short_fetch_and_sub1_read)
# define MK_AO_short_fetch_and_sub1(addr) MK_AO_short_fetch_and_sub1_read(addr)
# define MK_AO_HAVE_short_fetch_and_sub1
#endif

#if defined(MK_AO_HAVE_short_fetch_and_sub1_acquire) \
    && defined(MK_AO_HAVE_nop_full) \
    && !defined(MK_AO_HAVE_short_fetch_and_sub1_full)
# define MK_AO_short_fetch_and_sub1_full(addr) \
                        (MK_AO_nop_full(), MK_AO_short_fetch_and_sub1_acquire(addr))
# define MK_AO_HAVE_short_fetch_and_sub1_full
#endif

#if !defined(MK_AO_HAVE_short_fetch_and_sub1_release_write) \
    && defined(MK_AO_HAVE_short_fetch_and_sub1_write)
# define MK_AO_short_fetch_and_sub1_release_write(addr) \
                                MK_AO_short_fetch_and_sub1_write(addr)
# define MK_AO_HAVE_short_fetch_and_sub1_release_write
#endif
#if !defined(MK_AO_HAVE_short_fetch_and_sub1_release_write) \
    && defined(MK_AO_HAVE_short_fetch_and_sub1_release)
# define MK_AO_short_fetch_and_sub1_release_write(addr) \
                                MK_AO_short_fetch_and_sub1_release(addr)
# define MK_AO_HAVE_short_fetch_and_sub1_release_write
#endif
#if !defined(MK_AO_HAVE_short_fetch_and_sub1_acquire_read) \
    && defined(MK_AO_HAVE_short_fetch_and_sub1_read)
# define MK_AO_short_fetch_and_sub1_acquire_read(addr) \
                                MK_AO_short_fetch_and_sub1_read(addr)
# define MK_AO_HAVE_short_fetch_and_sub1_acquire_read
#endif
#if !defined(MK_AO_HAVE_short_fetch_and_sub1_acquire_read) \
    && defined(MK_AO_HAVE_short_fetch_and_sub1_acquire)
# define MK_AO_short_fetch_and_sub1_acquire_read(addr) \
                                MK_AO_short_fetch_and_sub1_acquire(addr)
# define MK_AO_HAVE_short_fetch_and_sub1_acquire_read
#endif

#ifdef MK_AO_NO_DD_ORDERING
# if defined(MK_AO_HAVE_short_fetch_and_sub1_acquire_read)
#   define MK_AO_short_fetch_and_sub1_dd_acquire_read(addr) \
                                MK_AO_short_fetch_and_sub1_acquire_read(addr)
#   define MK_AO_HAVE_short_fetch_and_sub1_dd_acquire_read
# endif
#else
# if defined(MK_AO_HAVE_short_fetch_and_sub1)
#   define MK_AO_short_fetch_and_sub1_dd_acquire_read(addr) \
                                MK_AO_short_fetch_and_sub1(addr)
#   define MK_AO_HAVE_short_fetch_and_sub1_dd_acquire_read
# endif
#endif /* !MK_AO_NO_DD_ORDERING */

/* short_and */
#if defined(MK_AO_HAVE_short_compare_and_swap_full) \
    && !defined(MK_AO_HAVE_short_and_full)
  MK_AO_INLINE void
  MK_AO_short_and_full(volatile unsigned/**/short *addr, unsigned/**/short value)
  {
    unsigned/**/short old;

    do
      {
        old = *(unsigned/**/short *)addr;
      }
    while (MK_AO_EXPECT_FALSE(!MK_AO_short_compare_and_swap_full(addr, old,
                                                           old & value)));
  }
# define MK_AO_HAVE_short_and_full
#endif

#if defined(MK_AO_HAVE_short_and_full)
# if !defined(MK_AO_HAVE_short_and_release)
#   define MK_AO_short_and_release(addr, val) MK_AO_short_and_full(addr, val)
#   define MK_AO_HAVE_short_and_release
# endif
# if !defined(MK_AO_HAVE_short_and_acquire)
#   define MK_AO_short_and_acquire(addr, val) MK_AO_short_and_full(addr, val)
#   define MK_AO_HAVE_short_and_acquire
# endif
# if !defined(MK_AO_HAVE_short_and_write)
#   define MK_AO_short_and_write(addr, val) MK_AO_short_and_full(addr, val)
#   define MK_AO_HAVE_short_and_write
# endif
# if !defined(MK_AO_HAVE_short_and_read)
#   define MK_AO_short_and_read(addr, val) MK_AO_short_and_full(addr, val)
#   define MK_AO_HAVE_short_and_read
# endif
#endif /* MK_AO_HAVE_short_and_full */

#if !defined(MK_AO_HAVE_short_and) && defined(MK_AO_HAVE_short_and_release)
# define MK_AO_short_and(addr, val) MK_AO_short_and_release(addr, val)
# define MK_AO_HAVE_short_and
#endif
#if !defined(MK_AO_HAVE_short_and) && defined(MK_AO_HAVE_short_and_acquire)
# define MK_AO_short_and(addr, val) MK_AO_short_and_acquire(addr, val)
# define MK_AO_HAVE_short_and
#endif
#if !defined(MK_AO_HAVE_short_and) && defined(MK_AO_HAVE_short_and_write)
# define MK_AO_short_and(addr, val) MK_AO_short_and_write(addr, val)
# define MK_AO_HAVE_short_and
#endif
#if !defined(MK_AO_HAVE_short_and) && defined(MK_AO_HAVE_short_and_read)
# define MK_AO_short_and(addr, val) MK_AO_short_and_read(addr, val)
# define MK_AO_HAVE_short_and
#endif

#if defined(MK_AO_HAVE_short_and_acquire) && defined(MK_AO_HAVE_nop_full) \
    && !defined(MK_AO_HAVE_short_and_full)
# define MK_AO_short_and_full(addr, val) \
                        (MK_AO_nop_full(), MK_AO_short_and_acquire(addr, val))
# define MK_AO_HAVE_short_and_full
#endif

#if !defined(MK_AO_HAVE_short_and_release_write) \
    && defined(MK_AO_HAVE_short_and_write)
# define MK_AO_short_and_release_write(addr, val) MK_AO_short_and_write(addr, val)
# define MK_AO_HAVE_short_and_release_write
#endif
#if !defined(MK_AO_HAVE_short_and_release_write) \
    && defined(MK_AO_HAVE_short_and_release)
# define MK_AO_short_and_release_write(addr, val) MK_AO_short_and_release(addr, val)
# define MK_AO_HAVE_short_and_release_write
#endif
#if !defined(MK_AO_HAVE_short_and_acquire_read) \
    && defined(MK_AO_HAVE_short_and_read)
# define MK_AO_short_and_acquire_read(addr, val) MK_AO_short_and_read(addr, val)
# define MK_AO_HAVE_short_and_acquire_read
#endif
#if !defined(MK_AO_HAVE_short_and_acquire_read) \
    && defined(MK_AO_HAVE_short_and_acquire)
# define MK_AO_short_and_acquire_read(addr, val) MK_AO_short_and_acquire(addr, val)
# define MK_AO_HAVE_short_and_acquire_read
#endif

/* short_or */
#if defined(MK_AO_HAVE_short_compare_and_swap_full) \
    && !defined(MK_AO_HAVE_short_or_full)
  MK_AO_INLINE void
  MK_AO_short_or_full(volatile unsigned/**/short *addr, unsigned/**/short value)
  {
    unsigned/**/short old;

    do
      {
        old = *(unsigned/**/short *)addr;
      }
    while (MK_AO_EXPECT_FALSE(!MK_AO_short_compare_and_swap_full(addr, old,
                                                           old | value)));
  }
# define MK_AO_HAVE_short_or_full
#endif

#if defined(MK_AO_HAVE_short_or_full)
# if !defined(MK_AO_HAVE_short_or_release)
#   define MK_AO_short_or_release(addr, val) MK_AO_short_or_full(addr, val)
#   define MK_AO_HAVE_short_or_release
# endif
# if !defined(MK_AO_HAVE_short_or_acquire)
#   define MK_AO_short_or_acquire(addr, val) MK_AO_short_or_full(addr, val)
#   define MK_AO_HAVE_short_or_acquire
# endif
# if !defined(MK_AO_HAVE_short_or_write)
#   define MK_AO_short_or_write(addr, val) MK_AO_short_or_full(addr, val)
#   define MK_AO_HAVE_short_or_write
# endif
# if !defined(MK_AO_HAVE_short_or_read)
#   define MK_AO_short_or_read(addr, val) MK_AO_short_or_full(addr, val)
#   define MK_AO_HAVE_short_or_read
# endif
#endif /* MK_AO_HAVE_short_or_full */

#if !defined(MK_AO_HAVE_short_or) && defined(MK_AO_HAVE_short_or_release)
# define MK_AO_short_or(addr, val) MK_AO_short_or_release(addr, val)
# define MK_AO_HAVE_short_or
#endif
#if !defined(MK_AO_HAVE_short_or) && defined(MK_AO_HAVE_short_or_acquire)
# define MK_AO_short_or(addr, val) MK_AO_short_or_acquire(addr, val)
# define MK_AO_HAVE_short_or
#endif
#if !defined(MK_AO_HAVE_short_or) && defined(MK_AO_HAVE_short_or_write)
# define MK_AO_short_or(addr, val) MK_AO_short_or_write(addr, val)
# define MK_AO_HAVE_short_or
#endif
#if !defined(MK_AO_HAVE_short_or) && defined(MK_AO_HAVE_short_or_read)
# define MK_AO_short_or(addr, val) MK_AO_short_or_read(addr, val)
# define MK_AO_HAVE_short_or
#endif

#if defined(MK_AO_HAVE_short_or_acquire) && defined(MK_AO_HAVE_nop_full) \
    && !defined(MK_AO_HAVE_short_or_full)
# define MK_AO_short_or_full(addr, val) \
                        (MK_AO_nop_full(), MK_AO_short_or_acquire(addr, val))
# define MK_AO_HAVE_short_or_full
#endif

#if !defined(MK_AO_HAVE_short_or_release_write) \
    && defined(MK_AO_HAVE_short_or_write)
# define MK_AO_short_or_release_write(addr, val) MK_AO_short_or_write(addr, val)
# define MK_AO_HAVE_short_or_release_write
#endif
#if !defined(MK_AO_HAVE_short_or_release_write) \
    && defined(MK_AO_HAVE_short_or_release)
# define MK_AO_short_or_release_write(addr, val) MK_AO_short_or_release(addr, val)
# define MK_AO_HAVE_short_or_release_write
#endif
#if !defined(MK_AO_HAVE_short_or_acquire_read) && defined(MK_AO_HAVE_short_or_read)
# define MK_AO_short_or_acquire_read(addr, val) MK_AO_short_or_read(addr, val)
# define MK_AO_HAVE_short_or_acquire_read
#endif
#if !defined(MK_AO_HAVE_short_or_acquire_read) \
    && defined(MK_AO_HAVE_short_or_acquire)
# define MK_AO_short_or_acquire_read(addr, val) MK_AO_short_or_acquire(addr, val)
# define MK_AO_HAVE_short_or_acquire_read
#endif

/* short_xor */
#if defined(MK_AO_HAVE_short_compare_and_swap_full) \
    && !defined(MK_AO_HAVE_short_xor_full)
  MK_AO_INLINE void
  MK_AO_short_xor_full(volatile unsigned/**/short *addr, unsigned/**/short value)
  {
    unsigned/**/short old;

    do
      {
        old = *(unsigned/**/short *)addr;
      }
    while (MK_AO_EXPECT_FALSE(!MK_AO_short_compare_and_swap_full(addr, old,
                                                           old ^ value)));
  }
# define MK_AO_HAVE_short_xor_full
#endif

#if defined(MK_AO_HAVE_short_xor_full)
# if !defined(MK_AO_HAVE_short_xor_release)
#   define MK_AO_short_xor_release(addr, val) MK_AO_short_xor_full(addr, val)
#   define MK_AO_HAVE_short_xor_release
# endif
# if !defined(MK_AO_HAVE_short_xor_acquire)
#   define MK_AO_short_xor_acquire(addr, val) MK_AO_short_xor_full(addr, val)
#   define MK_AO_HAVE_short_xor_acquire
# endif
# if !defined(MK_AO_HAVE_short_xor_write)
#   define MK_AO_short_xor_write(addr, val) MK_AO_short_xor_full(addr, val)
#   define MK_AO_HAVE_short_xor_write
# endif
# if !defined(MK_AO_HAVE_short_xor_read)
#   define MK_AO_short_xor_read(addr, val) MK_AO_short_xor_full(addr, val)
#   define MK_AO_HAVE_short_xor_read
# endif
#endif /* MK_AO_HAVE_short_xor_full */

#if !defined(MK_AO_HAVE_short_xor) && defined(MK_AO_HAVE_short_xor_release)
# define MK_AO_short_xor(addr, val) MK_AO_short_xor_release(addr, val)
# define MK_AO_HAVE_short_xor
#endif
#if !defined(MK_AO_HAVE_short_xor) && defined(MK_AO_HAVE_short_xor_acquire)
# define MK_AO_short_xor(addr, val) MK_AO_short_xor_acquire(addr, val)
# define MK_AO_HAVE_short_xor
#endif
#if !defined(MK_AO_HAVE_short_xor) && defined(MK_AO_HAVE_short_xor_write)
# define MK_AO_short_xor(addr, val) MK_AO_short_xor_write(addr, val)
# define MK_AO_HAVE_short_xor
#endif
#if !defined(MK_AO_HAVE_short_xor) && defined(MK_AO_HAVE_short_xor_read)
# define MK_AO_short_xor(addr, val) MK_AO_short_xor_read(addr, val)
# define MK_AO_HAVE_short_xor
#endif

#if defined(MK_AO_HAVE_short_xor_acquire) && defined(MK_AO_HAVE_nop_full) \
    && !defined(MK_AO_HAVE_short_xor_full)
# define MK_AO_short_xor_full(addr, val) \
                        (MK_AO_nop_full(), MK_AO_short_xor_acquire(addr, val))
# define MK_AO_HAVE_short_xor_full
#endif

#if !defined(MK_AO_HAVE_short_xor_release_write) \
    && defined(MK_AO_HAVE_short_xor_write)
# define MK_AO_short_xor_release_write(addr, val) MK_AO_short_xor_write(addr, val)
# define MK_AO_HAVE_short_xor_release_write
#endif
#if !defined(MK_AO_HAVE_short_xor_release_write) \
    && defined(MK_AO_HAVE_short_xor_release)
# define MK_AO_short_xor_release_write(addr, val) MK_AO_short_xor_release(addr, val)
# define MK_AO_HAVE_short_xor_release_write
#endif
#if !defined(MK_AO_HAVE_short_xor_acquire_read) \
    && defined(MK_AO_HAVE_short_xor_read)
# define MK_AO_short_xor_acquire_read(addr, val) MK_AO_short_xor_read(addr, val)
# define MK_AO_HAVE_short_xor_acquire_read
#endif
#if !defined(MK_AO_HAVE_short_xor_acquire_read) \
    && defined(MK_AO_HAVE_short_xor_acquire)
# define MK_AO_short_xor_acquire_read(addr, val) MK_AO_short_xor_acquire(addr, val)
# define MK_AO_HAVE_short_xor_acquire_read
#endif

/* short_and/or/xor_dd_acquire_read are meaningless.    */
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

/* int_compare_and_swap (based on fetch_compare_and_swap) */
#if defined(MK_AO_HAVE_int_fetch_compare_and_swap_full) \
    && !defined(MK_AO_HAVE_int_compare_and_swap_full)
  MK_AO_INLINE int
  MK_AO_int_compare_and_swap_full(volatile unsigned *addr, unsigned old_val,
                                 unsigned new_val)
  {
    return MK_AO_int_fetch_compare_and_swap_full(addr, old_val, new_val)
             == old_val;
  }
# define MK_AO_HAVE_int_compare_and_swap_full
#endif

#if defined(MK_AO_HAVE_int_fetch_compare_and_swap_acquire) \
    && !defined(MK_AO_HAVE_int_compare_and_swap_acquire)
  MK_AO_INLINE int
  MK_AO_int_compare_and_swap_acquire(volatile unsigned *addr, unsigned old_val,
                                    unsigned new_val)
  {
    return MK_AO_int_fetch_compare_and_swap_acquire(addr, old_val, new_val)
             == old_val;
  }
# define MK_AO_HAVE_int_compare_and_swap_acquire
#endif

#if defined(MK_AO_HAVE_int_fetch_compare_and_swap_release) \
    && !defined(MK_AO_HAVE_int_compare_and_swap_release)
  MK_AO_INLINE int
  MK_AO_int_compare_and_swap_release(volatile unsigned *addr, unsigned old_val,
                                    unsigned new_val)
  {
    return MK_AO_int_fetch_compare_and_swap_release(addr, old_val, new_val)
             == old_val;
  }
# define MK_AO_HAVE_int_compare_and_swap_release
#endif

#if defined(MK_AO_HAVE_int_fetch_compare_and_swap_write) \
    && !defined(MK_AO_HAVE_int_compare_and_swap_write)
  MK_AO_INLINE int
  MK_AO_int_compare_and_swap_write(volatile unsigned *addr, unsigned old_val,
                                  unsigned new_val)
  {
    return MK_AO_int_fetch_compare_and_swap_write(addr, old_val, new_val)
             == old_val;
  }
# define MK_AO_HAVE_int_compare_and_swap_write
#endif

#if defined(MK_AO_HAVE_int_fetch_compare_and_swap_read) \
    && !defined(MK_AO_HAVE_int_compare_and_swap_read)
  MK_AO_INLINE int
  MK_AO_int_compare_and_swap_read(volatile unsigned *addr, unsigned old_val,
                                 unsigned new_val)
  {
    return MK_AO_int_fetch_compare_and_swap_read(addr, old_val, new_val)
             == old_val;
  }
# define MK_AO_HAVE_int_compare_and_swap_read
#endif

#if defined(MK_AO_HAVE_int_fetch_compare_and_swap) \
    && !defined(MK_AO_HAVE_int_compare_and_swap)
  MK_AO_INLINE int
  MK_AO_int_compare_and_swap(volatile unsigned *addr, unsigned old_val,
                            unsigned new_val)
  {
    return MK_AO_int_fetch_compare_and_swap(addr, old_val, new_val) == old_val;
  }
# define MK_AO_HAVE_int_compare_and_swap
#endif

#if defined(MK_AO_HAVE_int_fetch_compare_and_swap_release_write) \
    && !defined(MK_AO_HAVE_int_compare_and_swap_release_write)
  MK_AO_INLINE int
  MK_AO_int_compare_and_swap_release_write(volatile unsigned *addr,
                                          unsigned old_val, unsigned new_val)
  {
    return MK_AO_int_fetch_compare_and_swap_release_write(addr, old_val,
                                                         new_val) == old_val;
  }
# define MK_AO_HAVE_int_compare_and_swap_release_write
#endif

#if defined(MK_AO_HAVE_int_fetch_compare_and_swap_acquire_read) \
    && !defined(MK_AO_HAVE_int_compare_and_swap_acquire_read)
  MK_AO_INLINE int
  MK_AO_int_compare_and_swap_acquire_read(volatile unsigned *addr,
                                         unsigned old_val, unsigned new_val)
  {
    return MK_AO_int_fetch_compare_and_swap_acquire_read(addr, old_val,
                                                        new_val) == old_val;
  }
# define MK_AO_HAVE_int_compare_and_swap_acquire_read
#endif

#if defined(MK_AO_HAVE_int_fetch_compare_and_swap_dd_acquire_read) \
    && !defined(MK_AO_HAVE_int_compare_and_swap_dd_acquire_read)
  MK_AO_INLINE int
  MK_AO_int_compare_and_swap_dd_acquire_read(volatile unsigned *addr,
                                            unsigned old_val, unsigned new_val)
  {
    return MK_AO_int_fetch_compare_and_swap_dd_acquire_read(addr, old_val,
                                                           new_val) == old_val;
  }
# define MK_AO_HAVE_int_compare_and_swap_dd_acquire_read
#endif

/* int_fetch_and_add */
/* We first try to implement fetch_and_add variants in terms of the     */
/* corresponding compare_and_swap variants to minimize adding barriers. */
#if defined(MK_AO_HAVE_int_compare_and_swap_full) \
    && !defined(MK_AO_HAVE_int_fetch_and_add_full)
  MK_AO_INLINE unsigned
  MK_AO_int_fetch_and_add_full(volatile unsigned *addr, unsigned incr)
  {
    unsigned old;

    do
      {
        old = *(unsigned *)addr;
      }
    while (MK_AO_EXPECT_FALSE(!MK_AO_int_compare_and_swap_full(addr, old,
                                                           old + incr)));
    return old;
  }
# define MK_AO_HAVE_int_fetch_and_add_full
#endif

#if defined(MK_AO_HAVE_int_compare_and_swap_acquire) \
    && !defined(MK_AO_HAVE_int_fetch_and_add_acquire)
  MK_AO_INLINE unsigned
  MK_AO_int_fetch_and_add_acquire(volatile unsigned *addr, unsigned incr)
  {
    unsigned old;

    do
      {
        old = *(unsigned *)addr;
      }
    while (MK_AO_EXPECT_FALSE(!MK_AO_int_compare_and_swap_acquire(addr, old,
                                                              old + incr)));
    return old;
  }
# define MK_AO_HAVE_int_fetch_and_add_acquire
#endif

#if defined(MK_AO_HAVE_int_compare_and_swap_release) \
    && !defined(MK_AO_HAVE_int_fetch_and_add_release)
  MK_AO_INLINE unsigned
  MK_AO_int_fetch_and_add_release(volatile unsigned *addr, unsigned incr)
  {
    unsigned old;

    do
      {
        old = *(unsigned *)addr;
      }
    while (MK_AO_EXPECT_FALSE(!MK_AO_int_compare_and_swap_release(addr, old,
                                                              old + incr)));
    return old;
  }
# define MK_AO_HAVE_int_fetch_and_add_release
#endif

#if defined(MK_AO_HAVE_int_compare_and_swap) \
    && !defined(MK_AO_HAVE_int_fetch_and_add)
  MK_AO_INLINE unsigned
  MK_AO_int_fetch_and_add(volatile unsigned *addr, unsigned incr)
  {
    unsigned old;

    do
      {
        old = *(unsigned *)addr;
      }
    while (MK_AO_EXPECT_FALSE(!MK_AO_int_compare_and_swap(addr, old,
                                                      old + incr)));
    return old;
  }
# define MK_AO_HAVE_int_fetch_and_add
#endif

#if defined(MK_AO_HAVE_int_fetch_and_add_full)
# if !defined(MK_AO_HAVE_int_fetch_and_add_release)
#   define MK_AO_int_fetch_and_add_release(addr, val) \
                                MK_AO_int_fetch_and_add_full(addr, val)
#   define MK_AO_HAVE_int_fetch_and_add_release
# endif
# if !defined(MK_AO_HAVE_int_fetch_and_add_acquire)
#   define MK_AO_int_fetch_and_add_acquire(addr, val) \
                                MK_AO_int_fetch_and_add_full(addr, val)
#   define MK_AO_HAVE_int_fetch_and_add_acquire
# endif
# if !defined(MK_AO_HAVE_int_fetch_and_add_write)
#   define MK_AO_int_fetch_and_add_write(addr, val) \
                                MK_AO_int_fetch_and_add_full(addr, val)
#   define MK_AO_HAVE_int_fetch_and_add_write
# endif
# if !defined(MK_AO_HAVE_int_fetch_and_add_read)
#   define MK_AO_int_fetch_and_add_read(addr, val) \
                                MK_AO_int_fetch_and_add_full(addr, val)
#   define MK_AO_HAVE_int_fetch_and_add_read
# endif
#endif /* MK_AO_HAVE_int_fetch_and_add_full */

#if defined(MK_AO_HAVE_int_fetch_and_add) && defined(MK_AO_HAVE_nop_full) \
    && !defined(MK_AO_HAVE_int_fetch_and_add_acquire)
  MK_AO_INLINE unsigned
  MK_AO_int_fetch_and_add_acquire(volatile unsigned *addr, unsigned incr)
  {
    unsigned result = MK_AO_int_fetch_and_add(addr, incr);
    MK_AO_nop_full();
    return result;
  }
# define MK_AO_HAVE_int_fetch_and_add_acquire
#endif
#if defined(MK_AO_HAVE_int_fetch_and_add) && defined(MK_AO_HAVE_nop_full) \
    && !defined(MK_AO_HAVE_int_fetch_and_add_release)
# define MK_AO_int_fetch_and_add_release(addr, incr) \
                (MK_AO_nop_full(), MK_AO_int_fetch_and_add(addr, incr))
# define MK_AO_HAVE_int_fetch_and_add_release
#endif

#if !defined(MK_AO_HAVE_int_fetch_and_add) \
    && defined(MK_AO_HAVE_int_fetch_and_add_release)
# define MK_AO_int_fetch_and_add(addr, val) \
                                MK_AO_int_fetch_and_add_release(addr, val)
# define MK_AO_HAVE_int_fetch_and_add
#endif
#if !defined(MK_AO_HAVE_int_fetch_and_add) \
    && defined(MK_AO_HAVE_int_fetch_and_add_acquire)
# define MK_AO_int_fetch_and_add(addr, val) \
                                MK_AO_int_fetch_and_add_acquire(addr, val)
# define MK_AO_HAVE_int_fetch_and_add
#endif
#if !defined(MK_AO_HAVE_int_fetch_and_add) \
    && defined(MK_AO_HAVE_int_fetch_and_add_write)
# define MK_AO_int_fetch_and_add(addr, val) \
                                MK_AO_int_fetch_and_add_write(addr, val)
# define MK_AO_HAVE_int_fetch_and_add
#endif
#if !defined(MK_AO_HAVE_int_fetch_and_add) \
    && defined(MK_AO_HAVE_int_fetch_and_add_read)
# define MK_AO_int_fetch_and_add(addr, val) \
                                MK_AO_int_fetch_and_add_read(addr, val)
# define MK_AO_HAVE_int_fetch_and_add
#endif

#if defined(MK_AO_HAVE_int_fetch_and_add_acquire) \
    && defined(MK_AO_HAVE_nop_full) && !defined(MK_AO_HAVE_int_fetch_and_add_full)
# define MK_AO_int_fetch_and_add_full(addr, val) \
                (MK_AO_nop_full(), MK_AO_int_fetch_and_add_acquire(addr, val))
# define MK_AO_HAVE_int_fetch_and_add_full
#endif

#if !defined(MK_AO_HAVE_int_fetch_and_add_release_write) \
    && defined(MK_AO_HAVE_int_fetch_and_add_write)
# define MK_AO_int_fetch_and_add_release_write(addr, val) \
                                MK_AO_int_fetch_and_add_write(addr, val)
# define MK_AO_HAVE_int_fetch_and_add_release_write
#endif
#if !defined(MK_AO_HAVE_int_fetch_and_add_release_write) \
    && defined(MK_AO_HAVE_int_fetch_and_add_release)
# define MK_AO_int_fetch_and_add_release_write(addr, val) \
                                MK_AO_int_fetch_and_add_release(addr, val)
# define MK_AO_HAVE_int_fetch_and_add_release_write
#endif

#if !defined(MK_AO_HAVE_int_fetch_and_add_acquire_read) \
    && defined(MK_AO_HAVE_int_fetch_and_add_read)
# define MK_AO_int_fetch_and_add_acquire_read(addr, val) \
                                MK_AO_int_fetch_and_add_read(addr, val)
# define MK_AO_HAVE_int_fetch_and_add_acquire_read
#endif
#if !defined(MK_AO_HAVE_int_fetch_and_add_acquire_read) \
    && defined(MK_AO_HAVE_int_fetch_and_add_acquire)
# define MK_AO_int_fetch_and_add_acquire_read(addr, val) \
                                MK_AO_int_fetch_and_add_acquire(addr, val)
# define MK_AO_HAVE_int_fetch_and_add_acquire_read
#endif

#ifdef MK_AO_NO_DD_ORDERING
# if defined(MK_AO_HAVE_int_fetch_and_add_acquire_read)
#   define MK_AO_int_fetch_and_add_dd_acquire_read(addr, val) \
                                MK_AO_int_fetch_and_add_acquire_read(addr, val)
#   define MK_AO_HAVE_int_fetch_and_add_dd_acquire_read
# endif
#else
# if defined(MK_AO_HAVE_int_fetch_and_add)
#   define MK_AO_int_fetch_and_add_dd_acquire_read(addr, val) \
                                MK_AO_int_fetch_and_add(addr, val)
#   define MK_AO_HAVE_int_fetch_and_add_dd_acquire_read
# endif
#endif /* !MK_AO_NO_DD_ORDERING */

/* int_fetch_and_add1 */
#if defined(MK_AO_HAVE_int_fetch_and_add_full) \
    && !defined(MK_AO_HAVE_int_fetch_and_add1_full)
# define MK_AO_int_fetch_and_add1_full(addr) \
                                MK_AO_int_fetch_and_add_full(addr, 1)
# define MK_AO_HAVE_int_fetch_and_add1_full
#endif
#if defined(MK_AO_HAVE_int_fetch_and_add_release) \
    && !defined(MK_AO_HAVE_int_fetch_and_add1_release)
# define MK_AO_int_fetch_and_add1_release(addr) \
                                MK_AO_int_fetch_and_add_release(addr, 1)
# define MK_AO_HAVE_int_fetch_and_add1_release
#endif
#if defined(MK_AO_HAVE_int_fetch_and_add_acquire) \
    && !defined(MK_AO_HAVE_int_fetch_and_add1_acquire)
# define MK_AO_int_fetch_and_add1_acquire(addr) \
                                MK_AO_int_fetch_and_add_acquire(addr, 1)
# define MK_AO_HAVE_int_fetch_and_add1_acquire
#endif
#if defined(MK_AO_HAVE_int_fetch_and_add_write) \
    && !defined(MK_AO_HAVE_int_fetch_and_add1_write)
# define MK_AO_int_fetch_and_add1_write(addr) \
                                MK_AO_int_fetch_and_add_write(addr, 1)
# define MK_AO_HAVE_int_fetch_and_add1_write
#endif
#if defined(MK_AO_HAVE_int_fetch_and_add_read) \
    && !defined(MK_AO_HAVE_int_fetch_and_add1_read)
# define MK_AO_int_fetch_and_add1_read(addr) \
                                MK_AO_int_fetch_and_add_read(addr, 1)
# define MK_AO_HAVE_int_fetch_and_add1_read
#endif
#if defined(MK_AO_HAVE_int_fetch_and_add_release_write) \
    && !defined(MK_AO_HAVE_int_fetch_and_add1_release_write)
# define MK_AO_int_fetch_and_add1_release_write(addr) \
                                MK_AO_int_fetch_and_add_release_write(addr, 1)
# define MK_AO_HAVE_int_fetch_and_add1_release_write
#endif
#if defined(MK_AO_HAVE_int_fetch_and_add_acquire_read) \
    && !defined(MK_AO_HAVE_int_fetch_and_add1_acquire_read)
# define MK_AO_int_fetch_and_add1_acquire_read(addr) \
                                MK_AO_int_fetch_and_add_acquire_read(addr, 1)
# define MK_AO_HAVE_int_fetch_and_add1_acquire_read
#endif
#if defined(MK_AO_HAVE_int_fetch_and_add) \
    && !defined(MK_AO_HAVE_int_fetch_and_add1)
# define MK_AO_int_fetch_and_add1(addr) MK_AO_int_fetch_and_add(addr, 1)
# define MK_AO_HAVE_int_fetch_and_add1
#endif

#if defined(MK_AO_HAVE_int_fetch_and_add1_full)
# if !defined(MK_AO_HAVE_int_fetch_and_add1_release)
#   define MK_AO_int_fetch_and_add1_release(addr) \
                                MK_AO_int_fetch_and_add1_full(addr)
#   define MK_AO_HAVE_int_fetch_and_add1_release
# endif
# if !defined(MK_AO_HAVE_int_fetch_and_add1_acquire)
#   define MK_AO_int_fetch_and_add1_acquire(addr) \
                                MK_AO_int_fetch_and_add1_full(addr)
#   define MK_AO_HAVE_int_fetch_and_add1_acquire
# endif
# if !defined(MK_AO_HAVE_int_fetch_and_add1_write)
#   define MK_AO_int_fetch_and_add1_write(addr) \
                                MK_AO_int_fetch_and_add1_full(addr)
#   define MK_AO_HAVE_int_fetch_and_add1_write
# endif
# if !defined(MK_AO_HAVE_int_fetch_and_add1_read)
#   define MK_AO_int_fetch_and_add1_read(addr) \
                                MK_AO_int_fetch_and_add1_full(addr)
#   define MK_AO_HAVE_int_fetch_and_add1_read
# endif
#endif /* MK_AO_HAVE_int_fetch_and_add1_full */

#if !defined(MK_AO_HAVE_int_fetch_and_add1) \
    && defined(MK_AO_HAVE_int_fetch_and_add1_release)
# define MK_AO_int_fetch_and_add1(addr) MK_AO_int_fetch_and_add1_release(addr)
# define MK_AO_HAVE_int_fetch_and_add1
#endif
#if !defined(MK_AO_HAVE_int_fetch_and_add1) \
    && defined(MK_AO_HAVE_int_fetch_and_add1_acquire)
# define MK_AO_int_fetch_and_add1(addr) MK_AO_int_fetch_and_add1_acquire(addr)
# define MK_AO_HAVE_int_fetch_and_add1
#endif
#if !defined(MK_AO_HAVE_int_fetch_and_add1) \
    && defined(MK_AO_HAVE_int_fetch_and_add1_write)
# define MK_AO_int_fetch_and_add1(addr) MK_AO_int_fetch_and_add1_write(addr)
# define MK_AO_HAVE_int_fetch_and_add1
#endif
#if !defined(MK_AO_HAVE_int_fetch_and_add1) \
    && defined(MK_AO_HAVE_int_fetch_and_add1_read)
# define MK_AO_int_fetch_and_add1(addr) MK_AO_int_fetch_and_add1_read(addr)
# define MK_AO_HAVE_int_fetch_and_add1
#endif

#if defined(MK_AO_HAVE_int_fetch_and_add1_acquire) \
    && defined(MK_AO_HAVE_nop_full) \
    && !defined(MK_AO_HAVE_int_fetch_and_add1_full)
# define MK_AO_int_fetch_and_add1_full(addr) \
                        (MK_AO_nop_full(), MK_AO_int_fetch_and_add1_acquire(addr))
# define MK_AO_HAVE_int_fetch_and_add1_full
#endif

#if !defined(MK_AO_HAVE_int_fetch_and_add1_release_write) \
    && defined(MK_AO_HAVE_int_fetch_and_add1_write)
# define MK_AO_int_fetch_and_add1_release_write(addr) \
                                MK_AO_int_fetch_and_add1_write(addr)
# define MK_AO_HAVE_int_fetch_and_add1_release_write
#endif
#if !defined(MK_AO_HAVE_int_fetch_and_add1_release_write) \
    && defined(MK_AO_HAVE_int_fetch_and_add1_release)
# define MK_AO_int_fetch_and_add1_release_write(addr) \
                                MK_AO_int_fetch_and_add1_release(addr)
# define MK_AO_HAVE_int_fetch_and_add1_release_write
#endif
#if !defined(MK_AO_HAVE_int_fetch_and_add1_acquire_read) \
    && defined(MK_AO_HAVE_int_fetch_and_add1_read)
# define MK_AO_int_fetch_and_add1_acquire_read(addr) \
                                MK_AO_int_fetch_and_add1_read(addr)
# define MK_AO_HAVE_int_fetch_and_add1_acquire_read
#endif
#if !defined(MK_AO_HAVE_int_fetch_and_add1_acquire_read) \
    && defined(MK_AO_HAVE_int_fetch_and_add1_acquire)
# define MK_AO_int_fetch_and_add1_acquire_read(addr) \
                                MK_AO_int_fetch_and_add1_acquire(addr)
# define MK_AO_HAVE_int_fetch_and_add1_acquire_read
#endif

#ifdef MK_AO_NO_DD_ORDERING
# if defined(MK_AO_HAVE_int_fetch_and_add1_acquire_read)
#   define MK_AO_int_fetch_and_add1_dd_acquire_read(addr) \
                                MK_AO_int_fetch_and_add1_acquire_read(addr)
#   define MK_AO_HAVE_int_fetch_and_add1_dd_acquire_read
# endif
#else
# if defined(MK_AO_HAVE_int_fetch_and_add1)
#   define MK_AO_int_fetch_and_add1_dd_acquire_read(addr) \
                                MK_AO_int_fetch_and_add1(addr)
#   define MK_AO_HAVE_int_fetch_and_add1_dd_acquire_read
# endif
#endif /* !MK_AO_NO_DD_ORDERING */

/* int_fetch_and_sub1 */
#if defined(MK_AO_HAVE_int_fetch_and_add_full) \
    && !defined(MK_AO_HAVE_int_fetch_and_sub1_full)
# define MK_AO_int_fetch_and_sub1_full(addr) \
                MK_AO_int_fetch_and_add_full(addr, (unsigned)(-1))
# define MK_AO_HAVE_int_fetch_and_sub1_full
#endif
#if defined(MK_AO_HAVE_int_fetch_and_add_release) \
    && !defined(MK_AO_HAVE_int_fetch_and_sub1_release)
# define MK_AO_int_fetch_and_sub1_release(addr) \
                MK_AO_int_fetch_and_add_release(addr, (unsigned)(-1))
# define MK_AO_HAVE_int_fetch_and_sub1_release
#endif
#if defined(MK_AO_HAVE_int_fetch_and_add_acquire) \
    && !defined(MK_AO_HAVE_int_fetch_and_sub1_acquire)
# define MK_AO_int_fetch_and_sub1_acquire(addr) \
                MK_AO_int_fetch_and_add_acquire(addr, (unsigned)(-1))
# define MK_AO_HAVE_int_fetch_and_sub1_acquire
#endif
#if defined(MK_AO_HAVE_int_fetch_and_add_write) \
    && !defined(MK_AO_HAVE_int_fetch_and_sub1_write)
# define MK_AO_int_fetch_and_sub1_write(addr) \
                MK_AO_int_fetch_and_add_write(addr, (unsigned)(-1))
# define MK_AO_HAVE_int_fetch_and_sub1_write
#endif
#if defined(MK_AO_HAVE_int_fetch_and_add_read) \
    && !defined(MK_AO_HAVE_int_fetch_and_sub1_read)
# define MK_AO_int_fetch_and_sub1_read(addr) \
                MK_AO_int_fetch_and_add_read(addr, (unsigned)(-1))
# define MK_AO_HAVE_int_fetch_and_sub1_read
#endif
#if defined(MK_AO_HAVE_int_fetch_and_add_release_write) \
    && !defined(MK_AO_HAVE_int_fetch_and_sub1_release_write)
# define MK_AO_int_fetch_and_sub1_release_write(addr) \
                MK_AO_int_fetch_and_add_release_write(addr, (unsigned)(-1))
# define MK_AO_HAVE_int_fetch_and_sub1_release_write
#endif
#if defined(MK_AO_HAVE_int_fetch_and_add_acquire_read) \
    && !defined(MK_AO_HAVE_int_fetch_and_sub1_acquire_read)
# define MK_AO_int_fetch_and_sub1_acquire_read(addr) \
                MK_AO_int_fetch_and_add_acquire_read(addr, (unsigned)(-1))
# define MK_AO_HAVE_int_fetch_and_sub1_acquire_read
#endif
#if defined(MK_AO_HAVE_int_fetch_and_add) \
    && !defined(MK_AO_HAVE_int_fetch_and_sub1)
# define MK_AO_int_fetch_and_sub1(addr) \
                MK_AO_int_fetch_and_add(addr, (unsigned)(-1))
# define MK_AO_HAVE_int_fetch_and_sub1
#endif

#if defined(MK_AO_HAVE_int_fetch_and_sub1_full)
# if !defined(MK_AO_HAVE_int_fetch_and_sub1_release)
#   define MK_AO_int_fetch_and_sub1_release(addr) \
                                MK_AO_int_fetch_and_sub1_full(addr)
#   define MK_AO_HAVE_int_fetch_and_sub1_release
# endif
# if !defined(MK_AO_HAVE_int_fetch_and_sub1_acquire)
#   define MK_AO_int_fetch_and_sub1_acquire(addr) \
                                MK_AO_int_fetch_and_sub1_full(addr)
#   define MK_AO_HAVE_int_fetch_and_sub1_acquire
# endif
# if !defined(MK_AO_HAVE_int_fetch_and_sub1_write)
#   define MK_AO_int_fetch_and_sub1_write(addr) \
                                MK_AO_int_fetch_and_sub1_full(addr)
#   define MK_AO_HAVE_int_fetch_and_sub1_write
# endif
# if !defined(MK_AO_HAVE_int_fetch_and_sub1_read)
#   define MK_AO_int_fetch_and_sub1_read(addr) \
                                MK_AO_int_fetch_and_sub1_full(addr)
#   define MK_AO_HAVE_int_fetch_and_sub1_read
# endif
#endif /* MK_AO_HAVE_int_fetch_and_sub1_full */

#if !defined(MK_AO_HAVE_int_fetch_and_sub1) \
    && defined(MK_AO_HAVE_int_fetch_and_sub1_release)
# define MK_AO_int_fetch_and_sub1(addr) MK_AO_int_fetch_and_sub1_release(addr)
# define MK_AO_HAVE_int_fetch_and_sub1
#endif
#if !defined(MK_AO_HAVE_int_fetch_and_sub1) \
    && defined(MK_AO_HAVE_int_fetch_and_sub1_acquire)
# define MK_AO_int_fetch_and_sub1(addr) MK_AO_int_fetch_and_sub1_acquire(addr)
# define MK_AO_HAVE_int_fetch_and_sub1
#endif
#if !defined(MK_AO_HAVE_int_fetch_and_sub1) \
    && defined(MK_AO_HAVE_int_fetch_and_sub1_write)
# define MK_AO_int_fetch_and_sub1(addr) MK_AO_int_fetch_and_sub1_write(addr)
# define MK_AO_HAVE_int_fetch_and_sub1
#endif
#if !defined(MK_AO_HAVE_int_fetch_and_sub1) \
    && defined(MK_AO_HAVE_int_fetch_and_sub1_read)
# define MK_AO_int_fetch_and_sub1(addr) MK_AO_int_fetch_and_sub1_read(addr)
# define MK_AO_HAVE_int_fetch_and_sub1
#endif

#if defined(MK_AO_HAVE_int_fetch_and_sub1_acquire) \
    && defined(MK_AO_HAVE_nop_full) \
    && !defined(MK_AO_HAVE_int_fetch_and_sub1_full)
# define MK_AO_int_fetch_and_sub1_full(addr) \
                        (MK_AO_nop_full(), MK_AO_int_fetch_and_sub1_acquire(addr))
# define MK_AO_HAVE_int_fetch_and_sub1_full
#endif

#if !defined(MK_AO_HAVE_int_fetch_and_sub1_release_write) \
    && defined(MK_AO_HAVE_int_fetch_and_sub1_write)
# define MK_AO_int_fetch_and_sub1_release_write(addr) \
                                MK_AO_int_fetch_and_sub1_write(addr)
# define MK_AO_HAVE_int_fetch_and_sub1_release_write
#endif
#if !defined(MK_AO_HAVE_int_fetch_and_sub1_release_write) \
    && defined(MK_AO_HAVE_int_fetch_and_sub1_release)
# define MK_AO_int_fetch_and_sub1_release_write(addr) \
                                MK_AO_int_fetch_and_sub1_release(addr)
# define MK_AO_HAVE_int_fetch_and_sub1_release_write
#endif
#if !defined(MK_AO_HAVE_int_fetch_and_sub1_acquire_read) \
    && defined(MK_AO_HAVE_int_fetch_and_sub1_read)
# define MK_AO_int_fetch_and_sub1_acquire_read(addr) \
                                MK_AO_int_fetch_and_sub1_read(addr)
# define MK_AO_HAVE_int_fetch_and_sub1_acquire_read
#endif
#if !defined(MK_AO_HAVE_int_fetch_and_sub1_acquire_read) \
    && defined(MK_AO_HAVE_int_fetch_and_sub1_acquire)
# define MK_AO_int_fetch_and_sub1_acquire_read(addr) \
                                MK_AO_int_fetch_and_sub1_acquire(addr)
# define MK_AO_HAVE_int_fetch_and_sub1_acquire_read
#endif

#ifdef MK_AO_NO_DD_ORDERING
# if defined(MK_AO_HAVE_int_fetch_and_sub1_acquire_read)
#   define MK_AO_int_fetch_and_sub1_dd_acquire_read(addr) \
                                MK_AO_int_fetch_and_sub1_acquire_read(addr)
#   define MK_AO_HAVE_int_fetch_and_sub1_dd_acquire_read
# endif
#else
# if defined(MK_AO_HAVE_int_fetch_and_sub1)
#   define MK_AO_int_fetch_and_sub1_dd_acquire_read(addr) \
                                MK_AO_int_fetch_and_sub1(addr)
#   define MK_AO_HAVE_int_fetch_and_sub1_dd_acquire_read
# endif
#endif /* !MK_AO_NO_DD_ORDERING */

/* int_and */
#if defined(MK_AO_HAVE_int_compare_and_swap_full) \
    && !defined(MK_AO_HAVE_int_and_full)
  MK_AO_INLINE void
  MK_AO_int_and_full(volatile unsigned *addr, unsigned value)
  {
    unsigned old;

    do
      {
        old = *(unsigned *)addr;
      }
    while (MK_AO_EXPECT_FALSE(!MK_AO_int_compare_and_swap_full(addr, old,
                                                           old & value)));
  }
# define MK_AO_HAVE_int_and_full
#endif

#if defined(MK_AO_HAVE_int_and_full)
# if !defined(MK_AO_HAVE_int_and_release)
#   define MK_AO_int_and_release(addr, val) MK_AO_int_and_full(addr, val)
#   define MK_AO_HAVE_int_and_release
# endif
# if !defined(MK_AO_HAVE_int_and_acquire)
#   define MK_AO_int_and_acquire(addr, val) MK_AO_int_and_full(addr, val)
#   define MK_AO_HAVE_int_and_acquire
# endif
# if !defined(MK_AO_HAVE_int_and_write)
#   define MK_AO_int_and_write(addr, val) MK_AO_int_and_full(addr, val)
#   define MK_AO_HAVE_int_and_write
# endif
# if !defined(MK_AO_HAVE_int_and_read)
#   define MK_AO_int_and_read(addr, val) MK_AO_int_and_full(addr, val)
#   define MK_AO_HAVE_int_and_read
# endif
#endif /* MK_AO_HAVE_int_and_full */

#if !defined(MK_AO_HAVE_int_and) && defined(MK_AO_HAVE_int_and_release)
# define MK_AO_int_and(addr, val) MK_AO_int_and_release(addr, val)
# define MK_AO_HAVE_int_and
#endif
#if !defined(MK_AO_HAVE_int_and) && defined(MK_AO_HAVE_int_and_acquire)
# define MK_AO_int_and(addr, val) MK_AO_int_and_acquire(addr, val)
# define MK_AO_HAVE_int_and
#endif
#if !defined(MK_AO_HAVE_int_and) && defined(MK_AO_HAVE_int_and_write)
# define MK_AO_int_and(addr, val) MK_AO_int_and_write(addr, val)
# define MK_AO_HAVE_int_and
#endif
#if !defined(MK_AO_HAVE_int_and) && defined(MK_AO_HAVE_int_and_read)
# define MK_AO_int_and(addr, val) MK_AO_int_and_read(addr, val)
# define MK_AO_HAVE_int_and
#endif

#if defined(MK_AO_HAVE_int_and_acquire) && defined(MK_AO_HAVE_nop_full) \
    && !defined(MK_AO_HAVE_int_and_full)
# define MK_AO_int_and_full(addr, val) \
                        (MK_AO_nop_full(), MK_AO_int_and_acquire(addr, val))
# define MK_AO_HAVE_int_and_full
#endif

#if !defined(MK_AO_HAVE_int_and_release_write) \
    && defined(MK_AO_HAVE_int_and_write)
# define MK_AO_int_and_release_write(addr, val) MK_AO_int_and_write(addr, val)
# define MK_AO_HAVE_int_and_release_write
#endif
#if !defined(MK_AO_HAVE_int_and_release_write) \
    && defined(MK_AO_HAVE_int_and_release)
# define MK_AO_int_and_release_write(addr, val) MK_AO_int_and_release(addr, val)
# define MK_AO_HAVE_int_and_release_write
#endif
#if !defined(MK_AO_HAVE_int_and_acquire_read) \
    && defined(MK_AO_HAVE_int_and_read)
# define MK_AO_int_and_acquire_read(addr, val) MK_AO_int_and_read(addr, val)
# define MK_AO_HAVE_int_and_acquire_read
#endif
#if !defined(MK_AO_HAVE_int_and_acquire_read) \
    && defined(MK_AO_HAVE_int_and_acquire)
# define MK_AO_int_and_acquire_read(addr, val) MK_AO_int_and_acquire(addr, val)
# define MK_AO_HAVE_int_and_acquire_read
#endif

/* int_or */
#if defined(MK_AO_HAVE_int_compare_and_swap_full) \
    && !defined(MK_AO_HAVE_int_or_full)
  MK_AO_INLINE void
  MK_AO_int_or_full(volatile unsigned *addr, unsigned value)
  {
    unsigned old;

    do
      {
        old = *(unsigned *)addr;
      }
    while (MK_AO_EXPECT_FALSE(!MK_AO_int_compare_and_swap_full(addr, old,
                                                           old | value)));
  }
# define MK_AO_HAVE_int_or_full
#endif

#if defined(MK_AO_HAVE_int_or_full)
# if !defined(MK_AO_HAVE_int_or_release)
#   define MK_AO_int_or_release(addr, val) MK_AO_int_or_full(addr, val)
#   define MK_AO_HAVE_int_or_release
# endif
# if !defined(MK_AO_HAVE_int_or_acquire)
#   define MK_AO_int_or_acquire(addr, val) MK_AO_int_or_full(addr, val)
#   define MK_AO_HAVE_int_or_acquire
# endif
# if !defined(MK_AO_HAVE_int_or_write)
#   define MK_AO_int_or_write(addr, val) MK_AO_int_or_full(addr, val)
#   define MK_AO_HAVE_int_or_write
# endif
# if !defined(MK_AO_HAVE_int_or_read)
#   define MK_AO_int_or_read(addr, val) MK_AO_int_or_full(addr, val)
#   define MK_AO_HAVE_int_or_read
# endif
#endif /* MK_AO_HAVE_int_or_full */

#if !defined(MK_AO_HAVE_int_or) && defined(MK_AO_HAVE_int_or_release)
# define MK_AO_int_or(addr, val) MK_AO_int_or_release(addr, val)
# define MK_AO_HAVE_int_or
#endif
#if !defined(MK_AO_HAVE_int_or) && defined(MK_AO_HAVE_int_or_acquire)
# define MK_AO_int_or(addr, val) MK_AO_int_or_acquire(addr, val)
# define MK_AO_HAVE_int_or
#endif
#if !defined(MK_AO_HAVE_int_or) && defined(MK_AO_HAVE_int_or_write)
# define MK_AO_int_or(addr, val) MK_AO_int_or_write(addr, val)
# define MK_AO_HAVE_int_or
#endif
#if !defined(MK_AO_HAVE_int_or) && defined(MK_AO_HAVE_int_or_read)
# define MK_AO_int_or(addr, val) MK_AO_int_or_read(addr, val)
# define MK_AO_HAVE_int_or
#endif

#if defined(MK_AO_HAVE_int_or_acquire) && defined(MK_AO_HAVE_nop_full) \
    && !defined(MK_AO_HAVE_int_or_full)
# define MK_AO_int_or_full(addr, val) \
                        (MK_AO_nop_full(), MK_AO_int_or_acquire(addr, val))
# define MK_AO_HAVE_int_or_full
#endif

#if !defined(MK_AO_HAVE_int_or_release_write) \
    && defined(MK_AO_HAVE_int_or_write)
# define MK_AO_int_or_release_write(addr, val) MK_AO_int_or_write(addr, val)
# define MK_AO_HAVE_int_or_release_write
#endif
#if !defined(MK_AO_HAVE_int_or_release_write) \
    && defined(MK_AO_HAVE_int_or_release)
# define MK_AO_int_or_release_write(addr, val) MK_AO_int_or_release(addr, val)
# define MK_AO_HAVE_int_or_release_write
#endif
#if !defined(MK_AO_HAVE_int_or_acquire_read) && defined(MK_AO_HAVE_int_or_read)
# define MK_AO_int_or_acquire_read(addr, val) MK_AO_int_or_read(addr, val)
# define MK_AO_HAVE_int_or_acquire_read
#endif
#if !defined(MK_AO_HAVE_int_or_acquire_read) \
    && defined(MK_AO_HAVE_int_or_acquire)
# define MK_AO_int_or_acquire_read(addr, val) MK_AO_int_or_acquire(addr, val)
# define MK_AO_HAVE_int_or_acquire_read
#endif

/* int_xor */
#if defined(MK_AO_HAVE_int_compare_and_swap_full) \
    && !defined(MK_AO_HAVE_int_xor_full)
  MK_AO_INLINE void
  MK_AO_int_xor_full(volatile unsigned *addr, unsigned value)
  {
    unsigned old;

    do
      {
        old = *(unsigned *)addr;
      }
    while (MK_AO_EXPECT_FALSE(!MK_AO_int_compare_and_swap_full(addr, old,
                                                           old ^ value)));
  }
# define MK_AO_HAVE_int_xor_full
#endif

#if defined(MK_AO_HAVE_int_xor_full)
# if !defined(MK_AO_HAVE_int_xor_release)
#   define MK_AO_int_xor_release(addr, val) MK_AO_int_xor_full(addr, val)
#   define MK_AO_HAVE_int_xor_release
# endif
# if !defined(MK_AO_HAVE_int_xor_acquire)
#   define MK_AO_int_xor_acquire(addr, val) MK_AO_int_xor_full(addr, val)
#   define MK_AO_HAVE_int_xor_acquire
# endif
# if !defined(MK_AO_HAVE_int_xor_write)
#   define MK_AO_int_xor_write(addr, val) MK_AO_int_xor_full(addr, val)
#   define MK_AO_HAVE_int_xor_write
# endif
# if !defined(MK_AO_HAVE_int_xor_read)
#   define MK_AO_int_xor_read(addr, val) MK_AO_int_xor_full(addr, val)
#   define MK_AO_HAVE_int_xor_read
# endif
#endif /* MK_AO_HAVE_int_xor_full */

#if !defined(MK_AO_HAVE_int_xor) && defined(MK_AO_HAVE_int_xor_release)
# define MK_AO_int_xor(addr, val) MK_AO_int_xor_release(addr, val)
# define MK_AO_HAVE_int_xor
#endif
#if !defined(MK_AO_HAVE_int_xor) && defined(MK_AO_HAVE_int_xor_acquire)
# define MK_AO_int_xor(addr, val) MK_AO_int_xor_acquire(addr, val)
# define MK_AO_HAVE_int_xor
#endif
#if !defined(MK_AO_HAVE_int_xor) && defined(MK_AO_HAVE_int_xor_write)
# define MK_AO_int_xor(addr, val) MK_AO_int_xor_write(addr, val)
# define MK_AO_HAVE_int_xor
#endif
#if !defined(MK_AO_HAVE_int_xor) && defined(MK_AO_HAVE_int_xor_read)
# define MK_AO_int_xor(addr, val) MK_AO_int_xor_read(addr, val)
# define MK_AO_HAVE_int_xor
#endif

#if defined(MK_AO_HAVE_int_xor_acquire) && defined(MK_AO_HAVE_nop_full) \
    && !defined(MK_AO_HAVE_int_xor_full)
# define MK_AO_int_xor_full(addr, val) \
                        (MK_AO_nop_full(), MK_AO_int_xor_acquire(addr, val))
# define MK_AO_HAVE_int_xor_full
#endif

#if !defined(MK_AO_HAVE_int_xor_release_write) \
    && defined(MK_AO_HAVE_int_xor_write)
# define MK_AO_int_xor_release_write(addr, val) MK_AO_int_xor_write(addr, val)
# define MK_AO_HAVE_int_xor_release_write
#endif
#if !defined(MK_AO_HAVE_int_xor_release_write) \
    && defined(MK_AO_HAVE_int_xor_release)
# define MK_AO_int_xor_release_write(addr, val) MK_AO_int_xor_release(addr, val)
# define MK_AO_HAVE_int_xor_release_write
#endif
#if !defined(MK_AO_HAVE_int_xor_acquire_read) \
    && defined(MK_AO_HAVE_int_xor_read)
# define MK_AO_int_xor_acquire_read(addr, val) MK_AO_int_xor_read(addr, val)
# define MK_AO_HAVE_int_xor_acquire_read
#endif
#if !defined(MK_AO_HAVE_int_xor_acquire_read) \
    && defined(MK_AO_HAVE_int_xor_acquire)
# define MK_AO_int_xor_acquire_read(addr, val) MK_AO_int_xor_acquire(addr, val)
# define MK_AO_HAVE_int_xor_acquire_read
#endif

/* int_and/or/xor_dd_acquire_read are meaningless.    */
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

/* compare_and_swap (based on fetch_compare_and_swap) */
#if defined(MK_AO_HAVE_fetch_compare_and_swap_full) \
    && !defined(MK_AO_HAVE_compare_and_swap_full)
  MK_AO_INLINE int
  MK_AO_compare_and_swap_full(volatile MK_AO_t *addr, MK_AO_t old_val,
                                 MK_AO_t new_val)
  {
    return MK_AO_fetch_compare_and_swap_full(addr, old_val, new_val)
             == old_val;
  }
# define MK_AO_HAVE_compare_and_swap_full
#endif

#if defined(MK_AO_HAVE_fetch_compare_and_swap_acquire) \
    && !defined(MK_AO_HAVE_compare_and_swap_acquire)
  MK_AO_INLINE int
  MK_AO_compare_and_swap_acquire(volatile MK_AO_t *addr, MK_AO_t old_val,
                                    MK_AO_t new_val)
  {
    return MK_AO_fetch_compare_and_swap_acquire(addr, old_val, new_val)
             == old_val;
  }
# define MK_AO_HAVE_compare_and_swap_acquire
#endif

#if defined(MK_AO_HAVE_fetch_compare_and_swap_release) \
    && !defined(MK_AO_HAVE_compare_and_swap_release)
  MK_AO_INLINE int
  MK_AO_compare_and_swap_release(volatile MK_AO_t *addr, MK_AO_t old_val,
                                    MK_AO_t new_val)
  {
    return MK_AO_fetch_compare_and_swap_release(addr, old_val, new_val)
             == old_val;
  }
# define MK_AO_HAVE_compare_and_swap_release
#endif

#if defined(MK_AO_HAVE_fetch_compare_and_swap_write) \
    && !defined(MK_AO_HAVE_compare_and_swap_write)
  MK_AO_INLINE int
  MK_AO_compare_and_swap_write(volatile MK_AO_t *addr, MK_AO_t old_val,
                                  MK_AO_t new_val)
  {
    return MK_AO_fetch_compare_and_swap_write(addr, old_val, new_val)
             == old_val;
  }
# define MK_AO_HAVE_compare_and_swap_write
#endif

#if defined(MK_AO_HAVE_fetch_compare_and_swap_read) \
    && !defined(MK_AO_HAVE_compare_and_swap_read)
  MK_AO_INLINE int
  MK_AO_compare_and_swap_read(volatile MK_AO_t *addr, MK_AO_t old_val,
                                 MK_AO_t new_val)
  {
    return MK_AO_fetch_compare_and_swap_read(addr, old_val, new_val)
             == old_val;
  }
# define MK_AO_HAVE_compare_and_swap_read
#endif

#if defined(MK_AO_HAVE_fetch_compare_and_swap) \
    && !defined(MK_AO_HAVE_compare_and_swap)
  MK_AO_INLINE int
  MK_AO_compare_and_swap(volatile MK_AO_t *addr, MK_AO_t old_val,
                            MK_AO_t new_val)
  {
    return MK_AO_fetch_compare_and_swap(addr, old_val, new_val) == old_val;
  }
# define MK_AO_HAVE_compare_and_swap
#endif

#if defined(MK_AO_HAVE_fetch_compare_and_swap_release_write) \
    && !defined(MK_AO_HAVE_compare_and_swap_release_write)
  MK_AO_INLINE int
  MK_AO_compare_and_swap_release_write(volatile MK_AO_t *addr,
                                          MK_AO_t old_val, MK_AO_t new_val)
  {
    return MK_AO_fetch_compare_and_swap_release_write(addr, old_val,
                                                         new_val) == old_val;
  }
# define MK_AO_HAVE_compare_and_swap_release_write
#endif

#if defined(MK_AO_HAVE_fetch_compare_and_swap_acquire_read) \
    && !defined(MK_AO_HAVE_compare_and_swap_acquire_read)
  MK_AO_INLINE int
  MK_AO_compare_and_swap_acquire_read(volatile MK_AO_t *addr,
                                         MK_AO_t old_val, MK_AO_t new_val)
  {
    return MK_AO_fetch_compare_and_swap_acquire_read(addr, old_val,
                                                        new_val) == old_val;
  }
# define MK_AO_HAVE_compare_and_swap_acquire_read
#endif

#if defined(MK_AO_HAVE_fetch_compare_and_swap_dd_acquire_read) \
    && !defined(MK_AO_HAVE_compare_and_swap_dd_acquire_read)
  MK_AO_INLINE int
  MK_AO_compare_and_swap_dd_acquire_read(volatile MK_AO_t *addr,
                                            MK_AO_t old_val, MK_AO_t new_val)
  {
    return MK_AO_fetch_compare_and_swap_dd_acquire_read(addr, old_val,
                                                           new_val) == old_val;
  }
# define MK_AO_HAVE_compare_and_swap_dd_acquire_read
#endif

/* fetch_and_add */
/* We first try to implement fetch_and_add variants in terms of the     */
/* corresponding compare_and_swap variants to minimize adding barriers. */
#if defined(MK_AO_HAVE_compare_and_swap_full) \
    && !defined(MK_AO_HAVE_fetch_and_add_full)
  MK_AO_INLINE MK_AO_t
  MK_AO_fetch_and_add_full(volatile MK_AO_t *addr, MK_AO_t incr)
  {
    MK_AO_t old;

    do
      {
        old = *(MK_AO_t *)addr;
      }
    while (MK_AO_EXPECT_FALSE(!MK_AO_compare_and_swap_full(addr, old,
                                                           old + incr)));
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
        old = *(MK_AO_t *)addr;
      }
    while (MK_AO_EXPECT_FALSE(!MK_AO_compare_and_swap_acquire(addr, old,
                                                              old + incr)));
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
        old = *(MK_AO_t *)addr;
      }
    while (MK_AO_EXPECT_FALSE(!MK_AO_compare_and_swap_release(addr, old,
                                                              old + incr)));
    return old;
  }
# define MK_AO_HAVE_fetch_and_add_release
#endif

#if defined(MK_AO_HAVE_compare_and_swap) \
    && !defined(MK_AO_HAVE_fetch_and_add)
  MK_AO_INLINE MK_AO_t
  MK_AO_fetch_and_add(volatile MK_AO_t *addr, MK_AO_t incr)
  {
    MK_AO_t old;

    do
      {
        old = *(MK_AO_t *)addr;
      }
    while (MK_AO_EXPECT_FALSE(!MK_AO_compare_and_swap(addr, old,
                                                      old + incr)));
    return old;
  }
# define MK_AO_HAVE_fetch_and_add
#endif

#if defined(MK_AO_HAVE_fetch_and_add_full)
# if !defined(MK_AO_HAVE_fetch_and_add_release)
#   define MK_AO_fetch_and_add_release(addr, val) \
                                MK_AO_fetch_and_add_full(addr, val)
#   define MK_AO_HAVE_fetch_and_add_release
# endif
# if !defined(MK_AO_HAVE_fetch_and_add_acquire)
#   define MK_AO_fetch_and_add_acquire(addr, val) \
                                MK_AO_fetch_and_add_full(addr, val)
#   define MK_AO_HAVE_fetch_and_add_acquire
# endif
# if !defined(MK_AO_HAVE_fetch_and_add_write)
#   define MK_AO_fetch_and_add_write(addr, val) \
                                MK_AO_fetch_and_add_full(addr, val)
#   define MK_AO_HAVE_fetch_and_add_write
# endif
# if !defined(MK_AO_HAVE_fetch_and_add_read)
#   define MK_AO_fetch_and_add_read(addr, val) \
                                MK_AO_fetch_and_add_full(addr, val)
#   define MK_AO_HAVE_fetch_and_add_read
# endif
#endif /* MK_AO_HAVE_fetch_and_add_full */

#if defined(MK_AO_HAVE_fetch_and_add) && defined(MK_AO_HAVE_nop_full) \
    && !defined(MK_AO_HAVE_fetch_and_add_acquire)
  MK_AO_INLINE MK_AO_t
  MK_AO_fetch_and_add_acquire(volatile MK_AO_t *addr, MK_AO_t incr)
  {
    MK_AO_t result = MK_AO_fetch_and_add(addr, incr);
    MK_AO_nop_full();
    return result;
  }
# define MK_AO_HAVE_fetch_and_add_acquire
#endif
#if defined(MK_AO_HAVE_fetch_and_add) && defined(MK_AO_HAVE_nop_full) \
    && !defined(MK_AO_HAVE_fetch_and_add_release)
# define MK_AO_fetch_and_add_release(addr, incr) \
                (MK_AO_nop_full(), MK_AO_fetch_and_add(addr, incr))
# define MK_AO_HAVE_fetch_and_add_release
#endif

#if !defined(MK_AO_HAVE_fetch_and_add) \
    && defined(MK_AO_HAVE_fetch_and_add_release)
# define MK_AO_fetch_and_add(addr, val) \
                                MK_AO_fetch_and_add_release(addr, val)
# define MK_AO_HAVE_fetch_and_add
#endif
#if !defined(MK_AO_HAVE_fetch_and_add) \
    && defined(MK_AO_HAVE_fetch_and_add_acquire)
# define MK_AO_fetch_and_add(addr, val) \
                                MK_AO_fetch_and_add_acquire(addr, val)
# define MK_AO_HAVE_fetch_and_add
#endif
#if !defined(MK_AO_HAVE_fetch_and_add) \
    && defined(MK_AO_HAVE_fetch_and_add_write)
# define MK_AO_fetch_and_add(addr, val) \
                                MK_AO_fetch_and_add_write(addr, val)
# define MK_AO_HAVE_fetch_and_add
#endif
#if !defined(MK_AO_HAVE_fetch_and_add) \
    && defined(MK_AO_HAVE_fetch_and_add_read)
# define MK_AO_fetch_and_add(addr, val) \
                                MK_AO_fetch_and_add_read(addr, val)
# define MK_AO_HAVE_fetch_and_add
#endif

#if defined(MK_AO_HAVE_fetch_and_add_acquire) \
    && defined(MK_AO_HAVE_nop_full) && !defined(MK_AO_HAVE_fetch_and_add_full)
# define MK_AO_fetch_and_add_full(addr, val) \
                (MK_AO_nop_full(), MK_AO_fetch_and_add_acquire(addr, val))
# define MK_AO_HAVE_fetch_and_add_full
#endif

#if !defined(MK_AO_HAVE_fetch_and_add_release_write) \
    && defined(MK_AO_HAVE_fetch_and_add_write)
# define MK_AO_fetch_and_add_release_write(addr, val) \
                                MK_AO_fetch_and_add_write(addr, val)
# define MK_AO_HAVE_fetch_and_add_release_write
#endif
#if !defined(MK_AO_HAVE_fetch_and_add_release_write) \
    && defined(MK_AO_HAVE_fetch_and_add_release)
# define MK_AO_fetch_and_add_release_write(addr, val) \
                                MK_AO_fetch_and_add_release(addr, val)
# define MK_AO_HAVE_fetch_and_add_release_write
#endif

#if !defined(MK_AO_HAVE_fetch_and_add_acquire_read) \
    && defined(MK_AO_HAVE_fetch_and_add_read)
# define MK_AO_fetch_and_add_acquire_read(addr, val) \
                                MK_AO_fetch_and_add_read(addr, val)
# define MK_AO_HAVE_fetch_and_add_acquire_read
#endif
#if !defined(MK_AO_HAVE_fetch_and_add_acquire_read) \
    && defined(MK_AO_HAVE_fetch_and_add_acquire)
# define MK_AO_fetch_and_add_acquire_read(addr, val) \
                                MK_AO_fetch_and_add_acquire(addr, val)
# define MK_AO_HAVE_fetch_and_add_acquire_read
#endif

#ifdef MK_AO_NO_DD_ORDERING
# if defined(MK_AO_HAVE_fetch_and_add_acquire_read)
#   define MK_AO_fetch_and_add_dd_acquire_read(addr, val) \
                                MK_AO_fetch_and_add_acquire_read(addr, val)
#   define MK_AO_HAVE_fetch_and_add_dd_acquire_read
# endif
#else
# if defined(MK_AO_HAVE_fetch_and_add)
#   define MK_AO_fetch_and_add_dd_acquire_read(addr, val) \
                                MK_AO_fetch_and_add(addr, val)
#   define MK_AO_HAVE_fetch_and_add_dd_acquire_read
# endif
#endif /* !MK_AO_NO_DD_ORDERING */

/* fetch_and_add1 */
#if defined(MK_AO_HAVE_fetch_and_add_full) \
    && !defined(MK_AO_HAVE_fetch_and_add1_full)
# define MK_AO_fetch_and_add1_full(addr) \
                                MK_AO_fetch_and_add_full(addr, 1)
# define MK_AO_HAVE_fetch_and_add1_full
#endif
#if defined(MK_AO_HAVE_fetch_and_add_release) \
    && !defined(MK_AO_HAVE_fetch_and_add1_release)
# define MK_AO_fetch_and_add1_release(addr) \
                                MK_AO_fetch_and_add_release(addr, 1)
# define MK_AO_HAVE_fetch_and_add1_release
#endif
#if defined(MK_AO_HAVE_fetch_and_add_acquire) \
    && !defined(MK_AO_HAVE_fetch_and_add1_acquire)
# define MK_AO_fetch_and_add1_acquire(addr) \
                                MK_AO_fetch_and_add_acquire(addr, 1)
# define MK_AO_HAVE_fetch_and_add1_acquire
#endif
#if defined(MK_AO_HAVE_fetch_and_add_write) \
    && !defined(MK_AO_HAVE_fetch_and_add1_write)
# define MK_AO_fetch_and_add1_write(addr) \
                                MK_AO_fetch_and_add_write(addr, 1)
# define MK_AO_HAVE_fetch_and_add1_write
#endif
#if defined(MK_AO_HAVE_fetch_and_add_read) \
    && !defined(MK_AO_HAVE_fetch_and_add1_read)
# define MK_AO_fetch_and_add1_read(addr) \
                                MK_AO_fetch_and_add_read(addr, 1)
# define MK_AO_HAVE_fetch_and_add1_read
#endif
#if defined(MK_AO_HAVE_fetch_and_add_release_write) \
    && !defined(MK_AO_HAVE_fetch_and_add1_release_write)
# define MK_AO_fetch_and_add1_release_write(addr) \
                                MK_AO_fetch_and_add_release_write(addr, 1)
# define MK_AO_HAVE_fetch_and_add1_release_write
#endif
#if defined(MK_AO_HAVE_fetch_and_add_acquire_read) \
    && !defined(MK_AO_HAVE_fetch_and_add1_acquire_read)
# define MK_AO_fetch_and_add1_acquire_read(addr) \
                                MK_AO_fetch_and_add_acquire_read(addr, 1)
# define MK_AO_HAVE_fetch_and_add1_acquire_read
#endif
#if defined(MK_AO_HAVE_fetch_and_add) \
    && !defined(MK_AO_HAVE_fetch_and_add1)
# define MK_AO_fetch_and_add1(addr) MK_AO_fetch_and_add(addr, 1)
# define MK_AO_HAVE_fetch_and_add1
#endif

#if defined(MK_AO_HAVE_fetch_and_add1_full)
# if !defined(MK_AO_HAVE_fetch_and_add1_release)
#   define MK_AO_fetch_and_add1_release(addr) \
                                MK_AO_fetch_and_add1_full(addr)
#   define MK_AO_HAVE_fetch_and_add1_release
# endif
# if !defined(MK_AO_HAVE_fetch_and_add1_acquire)
#   define MK_AO_fetch_and_add1_acquire(addr) \
                                MK_AO_fetch_and_add1_full(addr)
#   define MK_AO_HAVE_fetch_and_add1_acquire
# endif
# if !defined(MK_AO_HAVE_fetch_and_add1_write)
#   define MK_AO_fetch_and_add1_write(addr) \
                                MK_AO_fetch_and_add1_full(addr)
#   define MK_AO_HAVE_fetch_and_add1_write
# endif
# if !defined(MK_AO_HAVE_fetch_and_add1_read)
#   define MK_AO_fetch_and_add1_read(addr) \
                                MK_AO_fetch_and_add1_full(addr)
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
#if !defined(MK_AO_HAVE_fetch_and_add1) \
    && defined(MK_AO_HAVE_fetch_and_add1_write)
# define MK_AO_fetch_and_add1(addr) MK_AO_fetch_and_add1_write(addr)
# define MK_AO_HAVE_fetch_and_add1
#endif
#if !defined(MK_AO_HAVE_fetch_and_add1) \
    && defined(MK_AO_HAVE_fetch_and_add1_read)
# define MK_AO_fetch_and_add1(addr) MK_AO_fetch_and_add1_read(addr)
# define MK_AO_HAVE_fetch_and_add1
#endif

#if defined(MK_AO_HAVE_fetch_and_add1_acquire) \
    && defined(MK_AO_HAVE_nop_full) \
    && !defined(MK_AO_HAVE_fetch_and_add1_full)
# define MK_AO_fetch_and_add1_full(addr) \
                        (MK_AO_nop_full(), MK_AO_fetch_and_add1_acquire(addr))
# define MK_AO_HAVE_fetch_and_add1_full
#endif

#if !defined(MK_AO_HAVE_fetch_and_add1_release_write) \
    && defined(MK_AO_HAVE_fetch_and_add1_write)
# define MK_AO_fetch_and_add1_release_write(addr) \
                                MK_AO_fetch_and_add1_write(addr)
# define MK_AO_HAVE_fetch_and_add1_release_write
#endif
#if !defined(MK_AO_HAVE_fetch_and_add1_release_write) \
    && defined(MK_AO_HAVE_fetch_and_add1_release)
# define MK_AO_fetch_and_add1_release_write(addr) \
                                MK_AO_fetch_and_add1_release(addr)
# define MK_AO_HAVE_fetch_and_add1_release_write
#endif
#if !defined(MK_AO_HAVE_fetch_and_add1_acquire_read) \
    && defined(MK_AO_HAVE_fetch_and_add1_read)
# define MK_AO_fetch_and_add1_acquire_read(addr) \
                                MK_AO_fetch_and_add1_read(addr)
# define MK_AO_HAVE_fetch_and_add1_acquire_read
#endif
#if !defined(MK_AO_HAVE_fetch_and_add1_acquire_read) \
    && defined(MK_AO_HAVE_fetch_and_add1_acquire)
# define MK_AO_fetch_and_add1_acquire_read(addr) \
                                MK_AO_fetch_and_add1_acquire(addr)
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
#   define MK_AO_fetch_and_add1_dd_acquire_read(addr) \
                                MK_AO_fetch_and_add1(addr)
#   define MK_AO_HAVE_fetch_and_add1_dd_acquire_read
# endif
#endif /* !MK_AO_NO_DD_ORDERING */

/* fetch_and_sub1 */
#if defined(MK_AO_HAVE_fetch_and_add_full) \
    && !defined(MK_AO_HAVE_fetch_and_sub1_full)
# define MK_AO_fetch_and_sub1_full(addr) \
                MK_AO_fetch_and_add_full(addr, (MK_AO_t)(-1))
# define MK_AO_HAVE_fetch_and_sub1_full
#endif
#if defined(MK_AO_HAVE_fetch_and_add_release) \
    && !defined(MK_AO_HAVE_fetch_and_sub1_release)
# define MK_AO_fetch_and_sub1_release(addr) \
                MK_AO_fetch_and_add_release(addr, (MK_AO_t)(-1))
# define MK_AO_HAVE_fetch_and_sub1_release
#endif
#if defined(MK_AO_HAVE_fetch_and_add_acquire) \
    && !defined(MK_AO_HAVE_fetch_and_sub1_acquire)
# define MK_AO_fetch_and_sub1_acquire(addr) \
                MK_AO_fetch_and_add_acquire(addr, (MK_AO_t)(-1))
# define MK_AO_HAVE_fetch_and_sub1_acquire
#endif
#if defined(MK_AO_HAVE_fetch_and_add_write) \
    && !defined(MK_AO_HAVE_fetch_and_sub1_write)
# define MK_AO_fetch_and_sub1_write(addr) \
                MK_AO_fetch_and_add_write(addr, (MK_AO_t)(-1))
# define MK_AO_HAVE_fetch_and_sub1_write
#endif
#if defined(MK_AO_HAVE_fetch_and_add_read) \
    && !defined(MK_AO_HAVE_fetch_and_sub1_read)
# define MK_AO_fetch_and_sub1_read(addr) \
                MK_AO_fetch_and_add_read(addr, (MK_AO_t)(-1))
# define MK_AO_HAVE_fetch_and_sub1_read
#endif
#if defined(MK_AO_HAVE_fetch_and_add_release_write) \
    && !defined(MK_AO_HAVE_fetch_and_sub1_release_write)
# define MK_AO_fetch_and_sub1_release_write(addr) \
                MK_AO_fetch_and_add_release_write(addr, (MK_AO_t)(-1))
# define MK_AO_HAVE_fetch_and_sub1_release_write
#endif
#if defined(MK_AO_HAVE_fetch_and_add_acquire_read) \
    && !defined(MK_AO_HAVE_fetch_and_sub1_acquire_read)
# define MK_AO_fetch_and_sub1_acquire_read(addr) \
                MK_AO_fetch_and_add_acquire_read(addr, (MK_AO_t)(-1))
# define MK_AO_HAVE_fetch_and_sub1_acquire_read
#endif
#if defined(MK_AO_HAVE_fetch_and_add) \
    && !defined(MK_AO_HAVE_fetch_and_sub1)
# define MK_AO_fetch_and_sub1(addr) \
                MK_AO_fetch_and_add(addr, (MK_AO_t)(-1))
# define MK_AO_HAVE_fetch_and_sub1
#endif

#if defined(MK_AO_HAVE_fetch_and_sub1_full)
# if !defined(MK_AO_HAVE_fetch_and_sub1_release)
#   define MK_AO_fetch_and_sub1_release(addr) \
                                MK_AO_fetch_and_sub1_full(addr)
#   define MK_AO_HAVE_fetch_and_sub1_release
# endif
# if !defined(MK_AO_HAVE_fetch_and_sub1_acquire)
#   define MK_AO_fetch_and_sub1_acquire(addr) \
                                MK_AO_fetch_and_sub1_full(addr)
#   define MK_AO_HAVE_fetch_and_sub1_acquire
# endif
# if !defined(MK_AO_HAVE_fetch_and_sub1_write)
#   define MK_AO_fetch_and_sub1_write(addr) \
                                MK_AO_fetch_and_sub1_full(addr)
#   define MK_AO_HAVE_fetch_and_sub1_write
# endif
# if !defined(MK_AO_HAVE_fetch_and_sub1_read)
#   define MK_AO_fetch_and_sub1_read(addr) \
                                MK_AO_fetch_and_sub1_full(addr)
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
#if !defined(MK_AO_HAVE_fetch_and_sub1) \
    && defined(MK_AO_HAVE_fetch_and_sub1_write)
# define MK_AO_fetch_and_sub1(addr) MK_AO_fetch_and_sub1_write(addr)
# define MK_AO_HAVE_fetch_and_sub1
#endif
#if !defined(MK_AO_HAVE_fetch_and_sub1) \
    && defined(MK_AO_HAVE_fetch_and_sub1_read)
# define MK_AO_fetch_and_sub1(addr) MK_AO_fetch_and_sub1_read(addr)
# define MK_AO_HAVE_fetch_and_sub1
#endif

#if defined(MK_AO_HAVE_fetch_and_sub1_acquire) \
    && defined(MK_AO_HAVE_nop_full) \
    && !defined(MK_AO_HAVE_fetch_and_sub1_full)
# define MK_AO_fetch_and_sub1_full(addr) \
                        (MK_AO_nop_full(), MK_AO_fetch_and_sub1_acquire(addr))
# define MK_AO_HAVE_fetch_and_sub1_full
#endif

#if !defined(MK_AO_HAVE_fetch_and_sub1_release_write) \
    && defined(MK_AO_HAVE_fetch_and_sub1_write)
# define MK_AO_fetch_and_sub1_release_write(addr) \
                                MK_AO_fetch_and_sub1_write(addr)
# define MK_AO_HAVE_fetch_and_sub1_release_write
#endif
#if !defined(MK_AO_HAVE_fetch_and_sub1_release_write) \
    && defined(MK_AO_HAVE_fetch_and_sub1_release)
# define MK_AO_fetch_and_sub1_release_write(addr) \
                                MK_AO_fetch_and_sub1_release(addr)
# define MK_AO_HAVE_fetch_and_sub1_release_write
#endif
#if !defined(MK_AO_HAVE_fetch_and_sub1_acquire_read) \
    && defined(MK_AO_HAVE_fetch_and_sub1_read)
# define MK_AO_fetch_and_sub1_acquire_read(addr) \
                                MK_AO_fetch_and_sub1_read(addr)
# define MK_AO_HAVE_fetch_and_sub1_acquire_read
#endif
#if !defined(MK_AO_HAVE_fetch_and_sub1_acquire_read) \
    && defined(MK_AO_HAVE_fetch_and_sub1_acquire)
# define MK_AO_fetch_and_sub1_acquire_read(addr) \
                                MK_AO_fetch_and_sub1_acquire(addr)
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
#   define MK_AO_fetch_and_sub1_dd_acquire_read(addr) \
                                MK_AO_fetch_and_sub1(addr)
#   define MK_AO_HAVE_fetch_and_sub1_dd_acquire_read
# endif
#endif /* !MK_AO_NO_DD_ORDERING */

/* and */
#if defined(MK_AO_HAVE_compare_and_swap_full) \
    && !defined(MK_AO_HAVE_and_full)
  MK_AO_INLINE void
  MK_AO_and_full(volatile MK_AO_t *addr, MK_AO_t value)
  {
    MK_AO_t old;

    do
      {
        old = *(MK_AO_t *)addr;
      }
    while (MK_AO_EXPECT_FALSE(!MK_AO_compare_and_swap_full(addr, old,
                                                           old & value)));
  }
# define MK_AO_HAVE_and_full
#endif

#if defined(MK_AO_HAVE_and_full)
# if !defined(MK_AO_HAVE_and_release)
#   define MK_AO_and_release(addr, val) MK_AO_and_full(addr, val)
#   define MK_AO_HAVE_and_release
# endif
# if !defined(MK_AO_HAVE_and_acquire)
#   define MK_AO_and_acquire(addr, val) MK_AO_and_full(addr, val)
#   define MK_AO_HAVE_and_acquire
# endif
# if !defined(MK_AO_HAVE_and_write)
#   define MK_AO_and_write(addr, val) MK_AO_and_full(addr, val)
#   define MK_AO_HAVE_and_write
# endif
# if !defined(MK_AO_HAVE_and_read)
#   define MK_AO_and_read(addr, val) MK_AO_and_full(addr, val)
#   define MK_AO_HAVE_and_read
# endif
#endif /* MK_AO_HAVE_and_full */

#if !defined(MK_AO_HAVE_and) && defined(MK_AO_HAVE_and_release)
# define MK_AO_and(addr, val) MK_AO_and_release(addr, val)
# define MK_AO_HAVE_and
#endif
#if !defined(MK_AO_HAVE_and) && defined(MK_AO_HAVE_and_acquire)
# define MK_AO_and(addr, val) MK_AO_and_acquire(addr, val)
# define MK_AO_HAVE_and
#endif
#if !defined(MK_AO_HAVE_and) && defined(MK_AO_HAVE_and_write)
# define MK_AO_and(addr, val) MK_AO_and_write(addr, val)
# define MK_AO_HAVE_and
#endif
#if !defined(MK_AO_HAVE_and) && defined(MK_AO_HAVE_and_read)
# define MK_AO_and(addr, val) MK_AO_and_read(addr, val)
# define MK_AO_HAVE_and
#endif

#if defined(MK_AO_HAVE_and_acquire) && defined(MK_AO_HAVE_nop_full) \
    && !defined(MK_AO_HAVE_and_full)
# define MK_AO_and_full(addr, val) \
                        (MK_AO_nop_full(), MK_AO_and_acquire(addr, val))
# define MK_AO_HAVE_and_full
#endif

#if !defined(MK_AO_HAVE_and_release_write) \
    && defined(MK_AO_HAVE_and_write)
# define MK_AO_and_release_write(addr, val) MK_AO_and_write(addr, val)
# define MK_AO_HAVE_and_release_write
#endif
#if !defined(MK_AO_HAVE_and_release_write) \
    && defined(MK_AO_HAVE_and_release)
# define MK_AO_and_release_write(addr, val) MK_AO_and_release(addr, val)
# define MK_AO_HAVE_and_release_write
#endif
#if !defined(MK_AO_HAVE_and_acquire_read) \
    && defined(MK_AO_HAVE_and_read)
# define MK_AO_and_acquire_read(addr, val) MK_AO_and_read(addr, val)
# define MK_AO_HAVE_and_acquire_read
#endif
#if !defined(MK_AO_HAVE_and_acquire_read) \
    && defined(MK_AO_HAVE_and_acquire)
# define MK_AO_and_acquire_read(addr, val) MK_AO_and_acquire(addr, val)
# define MK_AO_HAVE_and_acquire_read
#endif

/* or */
#if defined(MK_AO_HAVE_compare_and_swap_full) \
    && !defined(MK_AO_HAVE_or_full)
  MK_AO_INLINE void
  MK_AO_or_full(volatile MK_AO_t *addr, MK_AO_t value)
  {
    MK_AO_t old;

    do
      {
        old = *(MK_AO_t *)addr;
      }
    while (MK_AO_EXPECT_FALSE(!MK_AO_compare_and_swap_full(addr, old,
                                                           old | value)));
  }
# define MK_AO_HAVE_or_full
#endif

#if defined(MK_AO_HAVE_or_full)
# if !defined(MK_AO_HAVE_or_release)
#   define MK_AO_or_release(addr, val) MK_AO_or_full(addr, val)
#   define MK_AO_HAVE_or_release
# endif
# if !defined(MK_AO_HAVE_or_acquire)
#   define MK_AO_or_acquire(addr, val) MK_AO_or_full(addr, val)
#   define MK_AO_HAVE_or_acquire
# endif
# if !defined(MK_AO_HAVE_or_write)
#   define MK_AO_or_write(addr, val) MK_AO_or_full(addr, val)
#   define MK_AO_HAVE_or_write
# endif
# if !defined(MK_AO_HAVE_or_read)
#   define MK_AO_or_read(addr, val) MK_AO_or_full(addr, val)
#   define MK_AO_HAVE_or_read
# endif
#endif /* MK_AO_HAVE_or_full */

#if !defined(MK_AO_HAVE_or) && defined(MK_AO_HAVE_or_release)
# define MK_AO_or(addr, val) MK_AO_or_release(addr, val)
# define MK_AO_HAVE_or
#endif
#if !defined(MK_AO_HAVE_or) && defined(MK_AO_HAVE_or_acquire)
# define MK_AO_or(addr, val) MK_AO_or_acquire(addr, val)
# define MK_AO_HAVE_or
#endif
#if !defined(MK_AO_HAVE_or) && defined(MK_AO_HAVE_or_write)
# define MK_AO_or(addr, val) MK_AO_or_write(addr, val)
# define MK_AO_HAVE_or
#endif
#if !defined(MK_AO_HAVE_or) && defined(MK_AO_HAVE_or_read)
# define MK_AO_or(addr, val) MK_AO_or_read(addr, val)
# define MK_AO_HAVE_or
#endif

#if defined(MK_AO_HAVE_or_acquire) && defined(MK_AO_HAVE_nop_full) \
    && !defined(MK_AO_HAVE_or_full)
# define MK_AO_or_full(addr, val) \
                        (MK_AO_nop_full(), MK_AO_or_acquire(addr, val))
# define MK_AO_HAVE_or_full
#endif

#if !defined(MK_AO_HAVE_or_release_write) \
    && defined(MK_AO_HAVE_or_write)
# define MK_AO_or_release_write(addr, val) MK_AO_or_write(addr, val)
# define MK_AO_HAVE_or_release_write
#endif
#if !defined(MK_AO_HAVE_or_release_write) \
    && defined(MK_AO_HAVE_or_release)
# define MK_AO_or_release_write(addr, val) MK_AO_or_release(addr, val)
# define MK_AO_HAVE_or_release_write
#endif
#if !defined(MK_AO_HAVE_or_acquire_read) && defined(MK_AO_HAVE_or_read)
# define MK_AO_or_acquire_read(addr, val) MK_AO_or_read(addr, val)
# define MK_AO_HAVE_or_acquire_read
#endif
#if !defined(MK_AO_HAVE_or_acquire_read) \
    && defined(MK_AO_HAVE_or_acquire)
# define MK_AO_or_acquire_read(addr, val) MK_AO_or_acquire(addr, val)
# define MK_AO_HAVE_or_acquire_read
#endif

/* xor */
#if defined(MK_AO_HAVE_compare_and_swap_full) \
    && !defined(MK_AO_HAVE_xor_full)
  MK_AO_INLINE void
  MK_AO_xor_full(volatile MK_AO_t *addr, MK_AO_t value)
  {
    MK_AO_t old;

    do
      {
        old = *(MK_AO_t *)addr;
      }
    while (MK_AO_EXPECT_FALSE(!MK_AO_compare_and_swap_full(addr, old,
                                                           old ^ value)));
  }
# define MK_AO_HAVE_xor_full
#endif

#if defined(MK_AO_HAVE_xor_full)
# if !defined(MK_AO_HAVE_xor_release)
#   define MK_AO_xor_release(addr, val) MK_AO_xor_full(addr, val)
#   define MK_AO_HAVE_xor_release
# endif
# if !defined(MK_AO_HAVE_xor_acquire)
#   define MK_AO_xor_acquire(addr, val) MK_AO_xor_full(addr, val)
#   define MK_AO_HAVE_xor_acquire
# endif
# if !defined(MK_AO_HAVE_xor_write)
#   define MK_AO_xor_write(addr, val) MK_AO_xor_full(addr, val)
#   define MK_AO_HAVE_xor_write
# endif
# if !defined(MK_AO_HAVE_xor_read)
#   define MK_AO_xor_read(addr, val) MK_AO_xor_full(addr, val)
#   define MK_AO_HAVE_xor_read
# endif
#endif /* MK_AO_HAVE_xor_full */

#if !defined(MK_AO_HAVE_xor) && defined(MK_AO_HAVE_xor_release)
# define MK_AO_xor(addr, val) MK_AO_xor_release(addr, val)
# define MK_AO_HAVE_xor
#endif
#if !defined(MK_AO_HAVE_xor) && defined(MK_AO_HAVE_xor_acquire)
# define MK_AO_xor(addr, val) MK_AO_xor_acquire(addr, val)
# define MK_AO_HAVE_xor
#endif
#if !defined(MK_AO_HAVE_xor) && defined(MK_AO_HAVE_xor_write)
# define MK_AO_xor(addr, val) MK_AO_xor_write(addr, val)
# define MK_AO_HAVE_xor
#endif
#if !defined(MK_AO_HAVE_xor) && defined(MK_AO_HAVE_xor_read)
# define MK_AO_xor(addr, val) MK_AO_xor_read(addr, val)
# define MK_AO_HAVE_xor
#endif

#if defined(MK_AO_HAVE_xor_acquire) && defined(MK_AO_HAVE_nop_full) \
    && !defined(MK_AO_HAVE_xor_full)
# define MK_AO_xor_full(addr, val) \
                        (MK_AO_nop_full(), MK_AO_xor_acquire(addr, val))
# define MK_AO_HAVE_xor_full
#endif

#if !defined(MK_AO_HAVE_xor_release_write) \
    && defined(MK_AO_HAVE_xor_write)
# define MK_AO_xor_release_write(addr, val) MK_AO_xor_write(addr, val)
# define MK_AO_HAVE_xor_release_write
#endif
#if !defined(MK_AO_HAVE_xor_release_write) \
    && defined(MK_AO_HAVE_xor_release)
# define MK_AO_xor_release_write(addr, val) MK_AO_xor_release(addr, val)
# define MK_AO_HAVE_xor_release_write
#endif
#if !defined(MK_AO_HAVE_xor_acquire_read) \
    && defined(MK_AO_HAVE_xor_read)
# define MK_AO_xor_acquire_read(addr, val) MK_AO_xor_read(addr, val)
# define MK_AO_HAVE_xor_acquire_read
#endif
#if !defined(MK_AO_HAVE_xor_acquire_read) \
    && defined(MK_AO_HAVE_xor_acquire)
# define MK_AO_xor_acquire_read(addr, val) MK_AO_xor_acquire(addr, val)
# define MK_AO_HAVE_xor_acquire_read
#endif

/* and/or/xor_dd_acquire_read are meaningless.    */
