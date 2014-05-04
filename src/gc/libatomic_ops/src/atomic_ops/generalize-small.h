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

/* char_load */
#if defined(MK_AO_HAVE_char_load_acquire) && !defined(MK_AO_HAVE_char_load)
# define MK_AO_char_load(addr) MK_AO_char_load_acquire(addr)
# define MK_AO_HAVE_char_load
#endif

#if defined(MK_AO_HAVE_char_load_full) && !defined(MK_AO_HAVE_char_load_acquire)
# define MK_AO_char_load_acquire(addr) MK_AO_char_load_full(addr)
# define MK_AO_HAVE_char_load_acquire
#endif

#if defined(MK_AO_HAVE_char_load_full) && !defined(MK_AO_HAVE_char_load_read)
# define MK_AO_char_load_read(addr) MK_AO_char_load_full(addr)
# define MK_AO_HAVE_char_load_read
#endif

#if !defined(MK_AO_HAVE_char_load_acquire_read) \
    && defined(MK_AO_HAVE_char_load_acquire)
# define MK_AO_char_load_acquire_read(addr) MK_AO_char_load_acquire(addr)
# define MK_AO_HAVE_char_load_acquire_read
#endif

#if defined(MK_AO_HAVE_char_load) && defined(MK_AO_HAVE_nop_full) \
    && !defined(MK_AO_HAVE_char_load_acquire)
  MK_AO_INLINE unsigned char
  MK_AO_char_load_acquire(const volatile unsigned char *addr)
  {
    unsigned char result = MK_AO_char_load(addr);
    /* Acquire barrier would be useless, since the load could be delayed    */
    /* beyond it.                                                           */
    MK_AO_nop_full();
    return result;
  }
# define MK_AO_HAVE_char_load_acquire
#endif

#if defined(MK_AO_HAVE_char_load) && defined(MK_AO_HAVE_nop_read) \
    && !defined(MK_AO_HAVE_char_load_read)
  MK_AO_INLINE unsigned char
  MK_AO_char_load_read(const volatile unsigned char *addr)
  {
    unsigned char result = MK_AO_char_load(addr);
    /* Acquire barrier would be useless, since the load could be delayed    */
    /* beyond it.                                                           */
    MK_AO_nop_read();
    return result;
  }
# define MK_AO_HAVE_char_load_read
#endif

#if defined(MK_AO_HAVE_char_load_acquire) && defined(MK_AO_HAVE_nop_full) \
    && !defined(MK_AO_HAVE_char_load_full)
# define MK_AO_char_load_full(addr) (MK_AO_nop_full(), MK_AO_char_load_acquire(addr))
# define MK_AO_HAVE_char_load_full
#endif

#if !defined(MK_AO_HAVE_char_load_acquire_read) \
    && defined(MK_AO_HAVE_char_load_read)
# define MK_AO_char_load_acquire_read(addr) MK_AO_char_load_read(addr)
# define MK_AO_HAVE_char_load_acquire_read
#endif

#if defined(MK_AO_HAVE_char_load_acquire_read) && !defined(MK_AO_HAVE_char_load)
# define MK_AO_char_load(addr) MK_AO_char_load_acquire_read(addr)
# define MK_AO_HAVE_char_load
#endif

#ifdef MK_AO_NO_DD_ORDERING
# if defined(MK_AO_HAVE_char_load_acquire_read)
#   define MK_AO_char_load_dd_acquire_read(addr) MK_AO_char_load_acquire_read(addr)
#   define MK_AO_HAVE_char_load_dd_acquire_read
# endif
#else
# if defined(MK_AO_HAVE_char_load)
#   define MK_AO_char_load_dd_acquire_read(addr) MK_AO_char_load(addr)
#   define MK_AO_HAVE_char_load_dd_acquire_read
# endif
#endif /* !MK_AO_NO_DD_ORDERING */

/* char_store */
#if defined(MK_AO_HAVE_char_store_release) && !defined(MK_AO_HAVE_char_store)
# define MK_AO_char_store(addr, val) MK_AO_char_store_release(addr,val)
# define MK_AO_HAVE_char_store
#endif

#if defined(MK_AO_HAVE_char_store_full) && !defined(MK_AO_HAVE_char_store_release)
# define MK_AO_char_store_release(addr,val) MK_AO_char_store_full(addr,val)
# define MK_AO_HAVE_char_store_release
#endif

#if defined(MK_AO_HAVE_char_store_full) && !defined(MK_AO_HAVE_char_store_write)
# define MK_AO_char_store_write(addr,val) MK_AO_char_store_full(addr,val)
# define MK_AO_HAVE_char_store_write
#endif

#if defined(MK_AO_HAVE_char_store_release) \
    && !defined(MK_AO_HAVE_char_store_release_write)
# define MK_AO_char_store_release_write(addr, val) \
                            MK_AO_char_store_release(addr,val)
# define MK_AO_HAVE_char_store_release_write
#endif

#if defined(MK_AO_HAVE_char_store_write) && !defined(MK_AO_HAVE_char_store)
# define MK_AO_char_store(addr, val) MK_AO_char_store_write(addr,val)
# define MK_AO_HAVE_char_store
#endif

#if defined(MK_AO_HAVE_char_store) && defined(MK_AO_HAVE_nop_full) \
    && !defined(MK_AO_HAVE_char_store_release)
# define MK_AO_char_store_release(addr,val) \
                                (MK_AO_nop_full(), MK_AO_char_store(addr,val))
# define MK_AO_HAVE_char_store_release
#endif

#if defined(MK_AO_HAVE_nop_write) && defined(MK_AO_HAVE_char_store) \
    && !defined(MK_AO_HAVE_char_store_write)
# define MK_AO_char_store_write(addr, val) \
                                (MK_AO_nop_write(), MK_AO_char_store(addr,val))
# define MK_AO_HAVE_char_store_write
#endif

#if defined(MK_AO_HAVE_char_store_write) \
    && !defined(MK_AO_HAVE_char_store_release_write)
# define MK_AO_char_store_release_write(addr, val) MK_AO_char_store_write(addr,val)
# define MK_AO_HAVE_char_store_release_write
#endif

#if defined(MK_AO_HAVE_char_store_release) && defined(MK_AO_HAVE_nop_full) \
    && !defined(MK_AO_HAVE_char_store_full)
# define MK_AO_char_store_full(addr, val) \
                        (MK_AO_char_store_release(addr, val), MK_AO_nop_full())
# define MK_AO_HAVE_char_store_full
#endif

/* char_fetch_and_add */
#if defined(MK_AO_HAVE_char_compare_and_swap_full) \
    && !defined(MK_AO_HAVE_char_fetch_and_add_full)
  MK_AO_INLINE unsigned char
  MK_AO_char_fetch_and_add_full(volatile unsigned char *addr,
                              unsigned char incr)
  {
    unsigned char old;
    do
      {
        old = *addr;
      }
    while (!MK_AO_char_compare_and_swap_full(addr, old, old+incr));
    return old;
  }
# define MK_AO_HAVE_char_fetch_and_add_full
#endif

#if defined(MK_AO_HAVE_char_compare_and_swap_acquire) \
    && !defined(MK_AO_HAVE_char_fetch_and_add_acquire)
  MK_AO_INLINE unsigned char
  MK_AO_char_fetch_and_add_acquire(volatile unsigned char *addr,
                                 unsigned char incr)
  {
    unsigned char old;
    do
      {
        old = *addr;
      }
    while (!MK_AO_char_compare_and_swap_acquire(addr, old, old+incr));
    return old;
  }
# define MK_AO_HAVE_char_fetch_and_add_acquire
#endif

#if defined(MK_AO_HAVE_char_compare_and_swap_release) \
    && !defined(MK_AO_HAVE_char_fetch_and_add_release)
  MK_AO_INLINE unsigned char
  MK_AO_char_fetch_and_add_release(volatile unsigned char *addr,
                                 unsigned char incr)
  {
    unsigned char old;
    do
      {
        old = *addr;
      }
    while (!MK_AO_char_compare_and_swap_release(addr, old, old+incr));
    return old;
  }
# define MK_AO_HAVE_char_fetch_and_add_release
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
                                MK_AO_char_fetch_and_add_full(addr,1)
# define MK_AO_HAVE_char_fetch_and_add1_full
#endif
#if defined(MK_AO_HAVE_char_fetch_and_add_release) \
    && !defined(MK_AO_HAVE_char_fetch_and_add1_release)
# define MK_AO_char_fetch_and_add1_release(addr) \
                                MK_AO_char_fetch_and_add_release(addr,1)
# define MK_AO_HAVE_char_fetch_and_add1_release
#endif
#if defined(MK_AO_HAVE_char_fetch_and_add_acquire) \
    && !defined(MK_AO_HAVE_char_fetch_and_add1_acquire)
# define MK_AO_char_fetch_and_add1_acquire(addr) \
                                MK_AO_char_fetch_and_add_acquire(addr,1)
# define MK_AO_HAVE_char_fetch_and_add1_acquire
#endif
#if defined(MK_AO_HAVE_char_fetch_and_add_write) \
    && !defined(MK_AO_HAVE_char_fetch_and_add1_write)
# define MK_AO_char_fetch_and_add1_write(addr) \
                                MK_AO_char_fetch_and_add_write(addr,1)
# define MK_AO_HAVE_char_fetch_and_add1_write
#endif
#if defined(MK_AO_HAVE_char_fetch_and_add_read) \
    && !defined(MK_AO_HAVE_char_fetch_and_add1_read)
# define MK_AO_char_fetch_and_add1_read(addr) \
                                MK_AO_char_fetch_and_add_read(addr,1)
# define MK_AO_HAVE_char_fetch_and_add1_read
#endif
#if defined(MK_AO_HAVE_char_fetch_and_add_release_write) \
    && !defined(MK_AO_HAVE_char_fetch_and_add1_release_write)
# define MK_AO_char_fetch_and_add1_release_write(addr) \
                                MK_AO_char_fetch_and_add_release_write(addr,1)
# define MK_AO_HAVE_char_fetch_and_add1_release_write
#endif
#if defined(MK_AO_HAVE_char_fetch_and_add_acquire_read) \
    && !defined(MK_AO_HAVE_char_fetch_and_add1_acquire_read)
# define MK_AO_char_fetch_and_add1_acquire_read(addr) \
                                MK_AO_char_fetch_and_add_acquire_read(addr,1)
# define MK_AO_HAVE_char_fetch_and_add1_acquire_read
#endif
#if defined(MK_AO_HAVE_char_fetch_and_add) \
    && !defined(MK_AO_HAVE_char_fetch_and_add1)
# define MK_AO_char_fetch_and_add1(addr) MK_AO_char_fetch_and_add(addr,1)
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
                MK_AO_char_fetch_and_add_full(addr,(unsigned char)(-1))
# define MK_AO_HAVE_char_fetch_and_sub1_full
#endif
#if defined(MK_AO_HAVE_char_fetch_and_add_release) \
    && !defined(MK_AO_HAVE_char_fetch_and_sub1_release)
# define MK_AO_char_fetch_and_sub1_release(addr) \
                MK_AO_char_fetch_and_add_release(addr,(unsigned char)(-1))
# define MK_AO_HAVE_char_fetch_and_sub1_release
#endif
#if defined(MK_AO_HAVE_char_fetch_and_add_acquire) \
    && !defined(MK_AO_HAVE_char_fetch_and_sub1_acquire)
# define MK_AO_char_fetch_and_sub1_acquire(addr) \
                MK_AO_char_fetch_and_add_acquire(addr,(unsigned char)(-1))
# define MK_AO_HAVE_char_fetch_and_sub1_acquire
#endif
#if defined(MK_AO_HAVE_char_fetch_and_add_write) \
    && !defined(MK_AO_HAVE_char_fetch_and_sub1_write)
# define MK_AO_char_fetch_and_sub1_write(addr) \
                MK_AO_char_fetch_and_add_write(addr,(unsigned char)(-1))
# define MK_AO_HAVE_char_fetch_and_sub1_write
#endif
#if defined(MK_AO_HAVE_char_fetch_and_add_read) \
    && !defined(MK_AO_HAVE_char_fetch_and_sub1_read)
# define MK_AO_char_fetch_and_sub1_read(addr) \
                MK_AO_char_fetch_and_add_read(addr,(unsigned char)(-1))
# define MK_AO_HAVE_char_fetch_and_sub1_read
#endif
#if defined(MK_AO_HAVE_char_fetch_and_add_release_write) \
    && !defined(MK_AO_HAVE_char_fetch_and_sub1_release_write)
# define MK_AO_char_fetch_and_sub1_release_write(addr) \
        MK_AO_char_fetch_and_add_release_write(addr,(unsigned char)(-1))
# define MK_AO_HAVE_char_fetch_and_sub1_release_write
#endif
#if defined(MK_AO_HAVE_char_fetch_and_add_acquire_read) \
    && !defined(MK_AO_HAVE_char_fetch_and_sub1_acquire_read)
# define MK_AO_char_fetch_and_sub1_acquire_read(addr) \
        MK_AO_char_fetch_and_add_acquire_read(addr,(unsigned char)(-1))
# define MK_AO_HAVE_char_fetch_and_sub1_acquire_read
#endif
#if defined(MK_AO_HAVE_char_fetch_and_add) \
    && !defined(MK_AO_HAVE_char_fetch_and_sub1)
# define MK_AO_char_fetch_and_sub1(addr) \
                MK_AO_char_fetch_and_add(addr,(unsigned char)(-1))
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

/* short_load */
#if defined(MK_AO_HAVE_short_load_acquire) && !defined(MK_AO_HAVE_short_load)
# define MK_AO_short_load(addr) MK_AO_short_load_acquire(addr)
# define MK_AO_HAVE_short_load
#endif

#if defined(MK_AO_HAVE_short_load_full) && !defined(MK_AO_HAVE_short_load_acquire)
# define MK_AO_short_load_acquire(addr) MK_AO_short_load_full(addr)
# define MK_AO_HAVE_short_load_acquire
#endif

#if defined(MK_AO_HAVE_short_load_full) && !defined(MK_AO_HAVE_short_load_read)
# define MK_AO_short_load_read(addr) MK_AO_short_load_full(addr)
# define MK_AO_HAVE_short_load_read
#endif

#if !defined(MK_AO_HAVE_short_load_acquire_read) \
    && defined(MK_AO_HAVE_short_load_acquire)
# define MK_AO_short_load_acquire_read(addr) MK_AO_short_load_acquire(addr)
# define MK_AO_HAVE_short_load_acquire_read
#endif

#if defined(MK_AO_HAVE_short_load) && defined(MK_AO_HAVE_nop_full) \
    && !defined(MK_AO_HAVE_short_load_acquire)
  MK_AO_INLINE unsigned short
  MK_AO_short_load_acquire(const volatile unsigned short *addr)
  {
    unsigned short result = MK_AO_short_load(addr);
    /* Acquire barrier would be useless, since the load could be delayed    */
    /* beyond it.                                                           */
    MK_AO_nop_full();
    return result;
  }
# define MK_AO_HAVE_short_load_acquire
#endif

#if defined(MK_AO_HAVE_short_load) && defined(MK_AO_HAVE_nop_read) \
    && !defined(MK_AO_HAVE_short_load_read)
  MK_AO_INLINE unsigned short
  MK_AO_short_load_read(const volatile unsigned short *addr)
  {
    unsigned short result = MK_AO_short_load(addr);
    /* Acquire barrier would be useless, since the load could be delayed    */
    /* beyond it.                                                           */
    MK_AO_nop_read();
    return result;
  }
# define MK_AO_HAVE_short_load_read
#endif

#if defined(MK_AO_HAVE_short_load_acquire) && defined(MK_AO_HAVE_nop_full) \
    && !defined(MK_AO_HAVE_short_load_full)
# define MK_AO_short_load_full(addr) (MK_AO_nop_full(), MK_AO_short_load_acquire(addr))
# define MK_AO_HAVE_short_load_full
#endif

#if !defined(MK_AO_HAVE_short_load_acquire_read) \
    && defined(MK_AO_HAVE_short_load_read)
# define MK_AO_short_load_acquire_read(addr) MK_AO_short_load_read(addr)
# define MK_AO_HAVE_short_load_acquire_read
#endif

#if defined(MK_AO_HAVE_short_load_acquire_read) && !defined(MK_AO_HAVE_short_load)
# define MK_AO_short_load(addr) MK_AO_short_load_acquire_read(addr)
# define MK_AO_HAVE_short_load
#endif

#ifdef MK_AO_NO_DD_ORDERING
# if defined(MK_AO_HAVE_short_load_acquire_read)
#   define MK_AO_short_load_dd_acquire_read(addr) MK_AO_short_load_acquire_read(addr)
#   define MK_AO_HAVE_short_load_dd_acquire_read
# endif
#else
# if defined(MK_AO_HAVE_short_load)
#   define MK_AO_short_load_dd_acquire_read(addr) MK_AO_short_load(addr)
#   define MK_AO_HAVE_short_load_dd_acquire_read
# endif
#endif /* !MK_AO_NO_DD_ORDERING */

/* short_store */
#if defined(MK_AO_HAVE_short_store_release) && !defined(MK_AO_HAVE_short_store)
# define MK_AO_short_store(addr, val) MK_AO_short_store_release(addr,val)
# define MK_AO_HAVE_short_store
#endif

#if defined(MK_AO_HAVE_short_store_full) && !defined(MK_AO_HAVE_short_store_release)
# define MK_AO_short_store_release(addr,val) MK_AO_short_store_full(addr,val)
# define MK_AO_HAVE_short_store_release
#endif

#if defined(MK_AO_HAVE_short_store_full) && !defined(MK_AO_HAVE_short_store_write)
# define MK_AO_short_store_write(addr,val) MK_AO_short_store_full(addr,val)
# define MK_AO_HAVE_short_store_write
#endif

#if defined(MK_AO_HAVE_short_store_release) \
    && !defined(MK_AO_HAVE_short_store_release_write)
# define MK_AO_short_store_release_write(addr, val) \
                            MK_AO_short_store_release(addr,val)
# define MK_AO_HAVE_short_store_release_write
#endif

#if defined(MK_AO_HAVE_short_store_write) && !defined(MK_AO_HAVE_short_store)
# define MK_AO_short_store(addr, val) MK_AO_short_store_write(addr,val)
# define MK_AO_HAVE_short_store
#endif

#if defined(MK_AO_HAVE_short_store) && defined(MK_AO_HAVE_nop_full) \
    && !defined(MK_AO_HAVE_short_store_release)
# define MK_AO_short_store_release(addr,val) \
                                (MK_AO_nop_full(), MK_AO_short_store(addr,val))
# define MK_AO_HAVE_short_store_release
#endif

#if defined(MK_AO_HAVE_nop_write) && defined(MK_AO_HAVE_short_store) \
    && !defined(MK_AO_HAVE_short_store_write)
# define MK_AO_short_store_write(addr, val) \
                                (MK_AO_nop_write(), MK_AO_short_store(addr,val))
# define MK_AO_HAVE_short_store_write
#endif

#if defined(MK_AO_HAVE_short_store_write) \
    && !defined(MK_AO_HAVE_short_store_release_write)
# define MK_AO_short_store_release_write(addr, val) MK_AO_short_store_write(addr,val)
# define MK_AO_HAVE_short_store_release_write
#endif

#if defined(MK_AO_HAVE_short_store_release) && defined(MK_AO_HAVE_nop_full) \
    && !defined(MK_AO_HAVE_short_store_full)
# define MK_AO_short_store_full(addr, val) \
                        (MK_AO_short_store_release(addr, val), MK_AO_nop_full())
# define MK_AO_HAVE_short_store_full
#endif

/* short_fetch_and_add */
#if defined(MK_AO_HAVE_short_compare_and_swap_full) \
    && !defined(MK_AO_HAVE_short_fetch_and_add_full)
  MK_AO_INLINE unsigned short
  MK_AO_short_fetch_and_add_full(volatile unsigned short *addr,
                              unsigned short incr)
  {
    unsigned short old;
    do
      {
        old = *addr;
      }
    while (!MK_AO_short_compare_and_swap_full(addr, old, old+incr));
    return old;
  }
# define MK_AO_HAVE_short_fetch_and_add_full
#endif

#if defined(MK_AO_HAVE_short_compare_and_swap_acquire) \
    && !defined(MK_AO_HAVE_short_fetch_and_add_acquire)
  MK_AO_INLINE unsigned short
  MK_AO_short_fetch_and_add_acquire(volatile unsigned short *addr,
                                 unsigned short incr)
  {
    unsigned short old;
    do
      {
        old = *addr;
      }
    while (!MK_AO_short_compare_and_swap_acquire(addr, old, old+incr));
    return old;
  }
# define MK_AO_HAVE_short_fetch_and_add_acquire
#endif

#if defined(MK_AO_HAVE_short_compare_and_swap_release) \
    && !defined(MK_AO_HAVE_short_fetch_and_add_release)
  MK_AO_INLINE unsigned short
  MK_AO_short_fetch_and_add_release(volatile unsigned short *addr,
                                 unsigned short incr)
  {
    unsigned short old;
    do
      {
        old = *addr;
      }
    while (!MK_AO_short_compare_and_swap_release(addr, old, old+incr));
    return old;
  }
# define MK_AO_HAVE_short_fetch_and_add_release
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
                                MK_AO_short_fetch_and_add_full(addr,1)
# define MK_AO_HAVE_short_fetch_and_add1_full
#endif
#if defined(MK_AO_HAVE_short_fetch_and_add_release) \
    && !defined(MK_AO_HAVE_short_fetch_and_add1_release)
# define MK_AO_short_fetch_and_add1_release(addr) \
                                MK_AO_short_fetch_and_add_release(addr,1)
# define MK_AO_HAVE_short_fetch_and_add1_release
#endif
#if defined(MK_AO_HAVE_short_fetch_and_add_acquire) \
    && !defined(MK_AO_HAVE_short_fetch_and_add1_acquire)
# define MK_AO_short_fetch_and_add1_acquire(addr) \
                                MK_AO_short_fetch_and_add_acquire(addr,1)
# define MK_AO_HAVE_short_fetch_and_add1_acquire
#endif
#if defined(MK_AO_HAVE_short_fetch_and_add_write) \
    && !defined(MK_AO_HAVE_short_fetch_and_add1_write)
# define MK_AO_short_fetch_and_add1_write(addr) \
                                MK_AO_short_fetch_and_add_write(addr,1)
# define MK_AO_HAVE_short_fetch_and_add1_write
#endif
#if defined(MK_AO_HAVE_short_fetch_and_add_read) \
    && !defined(MK_AO_HAVE_short_fetch_and_add1_read)
# define MK_AO_short_fetch_and_add1_read(addr) \
                                MK_AO_short_fetch_and_add_read(addr,1)
# define MK_AO_HAVE_short_fetch_and_add1_read
#endif
#if defined(MK_AO_HAVE_short_fetch_and_add_release_write) \
    && !defined(MK_AO_HAVE_short_fetch_and_add1_release_write)
# define MK_AO_short_fetch_and_add1_release_write(addr) \
                                MK_AO_short_fetch_and_add_release_write(addr,1)
# define MK_AO_HAVE_short_fetch_and_add1_release_write
#endif
#if defined(MK_AO_HAVE_short_fetch_and_add_acquire_read) \
    && !defined(MK_AO_HAVE_short_fetch_and_add1_acquire_read)
# define MK_AO_short_fetch_and_add1_acquire_read(addr) \
                                MK_AO_short_fetch_and_add_acquire_read(addr,1)
# define MK_AO_HAVE_short_fetch_and_add1_acquire_read
#endif
#if defined(MK_AO_HAVE_short_fetch_and_add) \
    && !defined(MK_AO_HAVE_short_fetch_and_add1)
# define MK_AO_short_fetch_and_add1(addr) MK_AO_short_fetch_and_add(addr,1)
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
                MK_AO_short_fetch_and_add_full(addr,(unsigned short)(-1))
# define MK_AO_HAVE_short_fetch_and_sub1_full
#endif
#if defined(MK_AO_HAVE_short_fetch_and_add_release) \
    && !defined(MK_AO_HAVE_short_fetch_and_sub1_release)
# define MK_AO_short_fetch_and_sub1_release(addr) \
                MK_AO_short_fetch_and_add_release(addr,(unsigned short)(-1))
# define MK_AO_HAVE_short_fetch_and_sub1_release
#endif
#if defined(MK_AO_HAVE_short_fetch_and_add_acquire) \
    && !defined(MK_AO_HAVE_short_fetch_and_sub1_acquire)
# define MK_AO_short_fetch_and_sub1_acquire(addr) \
                MK_AO_short_fetch_and_add_acquire(addr,(unsigned short)(-1))
# define MK_AO_HAVE_short_fetch_and_sub1_acquire
#endif
#if defined(MK_AO_HAVE_short_fetch_and_add_write) \
    && !defined(MK_AO_HAVE_short_fetch_and_sub1_write)
# define MK_AO_short_fetch_and_sub1_write(addr) \
                MK_AO_short_fetch_and_add_write(addr,(unsigned short)(-1))
# define MK_AO_HAVE_short_fetch_and_sub1_write
#endif
#if defined(MK_AO_HAVE_short_fetch_and_add_read) \
    && !defined(MK_AO_HAVE_short_fetch_and_sub1_read)
# define MK_AO_short_fetch_and_sub1_read(addr) \
                MK_AO_short_fetch_and_add_read(addr,(unsigned short)(-1))
# define MK_AO_HAVE_short_fetch_and_sub1_read
#endif
#if defined(MK_AO_HAVE_short_fetch_and_add_release_write) \
    && !defined(MK_AO_HAVE_short_fetch_and_sub1_release_write)
# define MK_AO_short_fetch_and_sub1_release_write(addr) \
        MK_AO_short_fetch_and_add_release_write(addr,(unsigned short)(-1))
# define MK_AO_HAVE_short_fetch_and_sub1_release_write
#endif
#if defined(MK_AO_HAVE_short_fetch_and_add_acquire_read) \
    && !defined(MK_AO_HAVE_short_fetch_and_sub1_acquire_read)
# define MK_AO_short_fetch_and_sub1_acquire_read(addr) \
        MK_AO_short_fetch_and_add_acquire_read(addr,(unsigned short)(-1))
# define MK_AO_HAVE_short_fetch_and_sub1_acquire_read
#endif
#if defined(MK_AO_HAVE_short_fetch_and_add) \
    && !defined(MK_AO_HAVE_short_fetch_and_sub1)
# define MK_AO_short_fetch_and_sub1(addr) \
                MK_AO_short_fetch_and_add(addr,(unsigned short)(-1))
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

/* int_load */
#if defined(MK_AO_HAVE_int_load_acquire) && !defined(MK_AO_HAVE_int_load)
# define MK_AO_int_load(addr) MK_AO_int_load_acquire(addr)
# define MK_AO_HAVE_int_load
#endif

#if defined(MK_AO_HAVE_int_load_full) && !defined(MK_AO_HAVE_int_load_acquire)
# define MK_AO_int_load_acquire(addr) MK_AO_int_load_full(addr)
# define MK_AO_HAVE_int_load_acquire
#endif

#if defined(MK_AO_HAVE_int_load_full) && !defined(MK_AO_HAVE_int_load_read)
# define MK_AO_int_load_read(addr) MK_AO_int_load_full(addr)
# define MK_AO_HAVE_int_load_read
#endif

#if !defined(MK_AO_HAVE_int_load_acquire_read) \
    && defined(MK_AO_HAVE_int_load_acquire)
# define MK_AO_int_load_acquire_read(addr) MK_AO_int_load_acquire(addr)
# define MK_AO_HAVE_int_load_acquire_read
#endif

#if defined(MK_AO_HAVE_int_load) && defined(MK_AO_HAVE_nop_full) \
    && !defined(MK_AO_HAVE_int_load_acquire)
  MK_AO_INLINE unsigned int
  MK_AO_int_load_acquire(const volatile unsigned int *addr)
  {
    unsigned int result = MK_AO_int_load(addr);
    /* Acquire barrier would be useless, since the load could be delayed    */
    /* beyond it.                                                           */
    MK_AO_nop_full();
    return result;
  }
# define MK_AO_HAVE_int_load_acquire
#endif

#if defined(MK_AO_HAVE_int_load) && defined(MK_AO_HAVE_nop_read) \
    && !defined(MK_AO_HAVE_int_load_read)
  MK_AO_INLINE unsigned int
  MK_AO_int_load_read(const volatile unsigned int *addr)
  {
    unsigned int result = MK_AO_int_load(addr);
    /* Acquire barrier would be useless, since the load could be delayed    */
    /* beyond it.                                                           */
    MK_AO_nop_read();
    return result;
  }
# define MK_AO_HAVE_int_load_read
#endif

#if defined(MK_AO_HAVE_int_load_acquire) && defined(MK_AO_HAVE_nop_full) \
    && !defined(MK_AO_HAVE_int_load_full)
# define MK_AO_int_load_full(addr) (MK_AO_nop_full(), MK_AO_int_load_acquire(addr))
# define MK_AO_HAVE_int_load_full
#endif

#if !defined(MK_AO_HAVE_int_load_acquire_read) \
    && defined(MK_AO_HAVE_int_load_read)
# define MK_AO_int_load_acquire_read(addr) MK_AO_int_load_read(addr)
# define MK_AO_HAVE_int_load_acquire_read
#endif

#if defined(MK_AO_HAVE_int_load_acquire_read) && !defined(MK_AO_HAVE_int_load)
# define MK_AO_int_load(addr) MK_AO_int_load_acquire_read(addr)
# define MK_AO_HAVE_int_load
#endif

#ifdef MK_AO_NO_DD_ORDERING
# if defined(MK_AO_HAVE_int_load_acquire_read)
#   define MK_AO_int_load_dd_acquire_read(addr) MK_AO_int_load_acquire_read(addr)
#   define MK_AO_HAVE_int_load_dd_acquire_read
# endif
#else
# if defined(MK_AO_HAVE_int_load)
#   define MK_AO_int_load_dd_acquire_read(addr) MK_AO_int_load(addr)
#   define MK_AO_HAVE_int_load_dd_acquire_read
# endif
#endif /* !MK_AO_NO_DD_ORDERING */

/* int_store */
#if defined(MK_AO_HAVE_int_store_release) && !defined(MK_AO_HAVE_int_store)
# define MK_AO_int_store(addr, val) MK_AO_int_store_release(addr,val)
# define MK_AO_HAVE_int_store
#endif

#if defined(MK_AO_HAVE_int_store_full) && !defined(MK_AO_HAVE_int_store_release)
# define MK_AO_int_store_release(addr,val) MK_AO_int_store_full(addr,val)
# define MK_AO_HAVE_int_store_release
#endif

#if defined(MK_AO_HAVE_int_store_full) && !defined(MK_AO_HAVE_int_store_write)
# define MK_AO_int_store_write(addr,val) MK_AO_int_store_full(addr,val)
# define MK_AO_HAVE_int_store_write
#endif

#if defined(MK_AO_HAVE_int_store_release) \
    && !defined(MK_AO_HAVE_int_store_release_write)
# define MK_AO_int_store_release_write(addr, val) \
                            MK_AO_int_store_release(addr,val)
# define MK_AO_HAVE_int_store_release_write
#endif

#if defined(MK_AO_HAVE_int_store_write) && !defined(MK_AO_HAVE_int_store)
# define MK_AO_int_store(addr, val) MK_AO_int_store_write(addr,val)
# define MK_AO_HAVE_int_store
#endif

#if defined(MK_AO_HAVE_int_store) && defined(MK_AO_HAVE_nop_full) \
    && !defined(MK_AO_HAVE_int_store_release)
# define MK_AO_int_store_release(addr,val) \
                                (MK_AO_nop_full(), MK_AO_int_store(addr,val))
# define MK_AO_HAVE_int_store_release
#endif

#if defined(MK_AO_HAVE_nop_write) && defined(MK_AO_HAVE_int_store) \
    && !defined(MK_AO_HAVE_int_store_write)
# define MK_AO_int_store_write(addr, val) \
                                (MK_AO_nop_write(), MK_AO_int_store(addr,val))
# define MK_AO_HAVE_int_store_write
#endif

#if defined(MK_AO_HAVE_int_store_write) \
    && !defined(MK_AO_HAVE_int_store_release_write)
# define MK_AO_int_store_release_write(addr, val) MK_AO_int_store_write(addr,val)
# define MK_AO_HAVE_int_store_release_write
#endif

#if defined(MK_AO_HAVE_int_store_release) && defined(MK_AO_HAVE_nop_full) \
    && !defined(MK_AO_HAVE_int_store_full)
# define MK_AO_int_store_full(addr, val) \
                        (MK_AO_int_store_release(addr, val), MK_AO_nop_full())
# define MK_AO_HAVE_int_store_full
#endif

/* int_fetch_and_add */
#if defined(MK_AO_HAVE_int_compare_and_swap_full) \
    && !defined(MK_AO_HAVE_int_fetch_and_add_full)
  MK_AO_INLINE unsigned int
  MK_AO_int_fetch_and_add_full(volatile unsigned int *addr,
                              unsigned int incr)
  {
    unsigned int old;
    do
      {
        old = *addr;
      }
    while (!MK_AO_int_compare_and_swap_full(addr, old, old+incr));
    return old;
  }
# define MK_AO_HAVE_int_fetch_and_add_full
#endif

#if defined(MK_AO_HAVE_int_compare_and_swap_acquire) \
    && !defined(MK_AO_HAVE_int_fetch_and_add_acquire)
  MK_AO_INLINE unsigned int
  MK_AO_int_fetch_and_add_acquire(volatile unsigned int *addr,
                                 unsigned int incr)
  {
    unsigned int old;
    do
      {
        old = *addr;
      }
    while (!MK_AO_int_compare_and_swap_acquire(addr, old, old+incr));
    return old;
  }
# define MK_AO_HAVE_int_fetch_and_add_acquire
#endif

#if defined(MK_AO_HAVE_int_compare_and_swap_release) \
    && !defined(MK_AO_HAVE_int_fetch_and_add_release)
  MK_AO_INLINE unsigned int
  MK_AO_int_fetch_and_add_release(volatile unsigned int *addr,
                                 unsigned int incr)
  {
    unsigned int old;
    do
      {
        old = *addr;
      }
    while (!MK_AO_int_compare_and_swap_release(addr, old, old+incr));
    return old;
  }
# define MK_AO_HAVE_int_fetch_and_add_release
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
                                MK_AO_int_fetch_and_add_full(addr,1)
# define MK_AO_HAVE_int_fetch_and_add1_full
#endif
#if defined(MK_AO_HAVE_int_fetch_and_add_release) \
    && !defined(MK_AO_HAVE_int_fetch_and_add1_release)
# define MK_AO_int_fetch_and_add1_release(addr) \
                                MK_AO_int_fetch_and_add_release(addr,1)
# define MK_AO_HAVE_int_fetch_and_add1_release
#endif
#if defined(MK_AO_HAVE_int_fetch_and_add_acquire) \
    && !defined(MK_AO_HAVE_int_fetch_and_add1_acquire)
# define MK_AO_int_fetch_and_add1_acquire(addr) \
                                MK_AO_int_fetch_and_add_acquire(addr,1)
# define MK_AO_HAVE_int_fetch_and_add1_acquire
#endif
#if defined(MK_AO_HAVE_int_fetch_and_add_write) \
    && !defined(MK_AO_HAVE_int_fetch_and_add1_write)
# define MK_AO_int_fetch_and_add1_write(addr) \
                                MK_AO_int_fetch_and_add_write(addr,1)
# define MK_AO_HAVE_int_fetch_and_add1_write
#endif
#if defined(MK_AO_HAVE_int_fetch_and_add_read) \
    && !defined(MK_AO_HAVE_int_fetch_and_add1_read)
# define MK_AO_int_fetch_and_add1_read(addr) \
                                MK_AO_int_fetch_and_add_read(addr,1)
# define MK_AO_HAVE_int_fetch_and_add1_read
#endif
#if defined(MK_AO_HAVE_int_fetch_and_add_release_write) \
    && !defined(MK_AO_HAVE_int_fetch_and_add1_release_write)
# define MK_AO_int_fetch_and_add1_release_write(addr) \
                                MK_AO_int_fetch_and_add_release_write(addr,1)
# define MK_AO_HAVE_int_fetch_and_add1_release_write
#endif
#if defined(MK_AO_HAVE_int_fetch_and_add_acquire_read) \
    && !defined(MK_AO_HAVE_int_fetch_and_add1_acquire_read)
# define MK_AO_int_fetch_and_add1_acquire_read(addr) \
                                MK_AO_int_fetch_and_add_acquire_read(addr,1)
# define MK_AO_HAVE_int_fetch_and_add1_acquire_read
#endif
#if defined(MK_AO_HAVE_int_fetch_and_add) \
    && !defined(MK_AO_HAVE_int_fetch_and_add1)
# define MK_AO_int_fetch_and_add1(addr) MK_AO_int_fetch_and_add(addr,1)
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
                MK_AO_int_fetch_and_add_full(addr,(unsigned int)(-1))
# define MK_AO_HAVE_int_fetch_and_sub1_full
#endif
#if defined(MK_AO_HAVE_int_fetch_and_add_release) \
    && !defined(MK_AO_HAVE_int_fetch_and_sub1_release)
# define MK_AO_int_fetch_and_sub1_release(addr) \
                MK_AO_int_fetch_and_add_release(addr,(unsigned int)(-1))
# define MK_AO_HAVE_int_fetch_and_sub1_release
#endif
#if defined(MK_AO_HAVE_int_fetch_and_add_acquire) \
    && !defined(MK_AO_HAVE_int_fetch_and_sub1_acquire)
# define MK_AO_int_fetch_and_sub1_acquire(addr) \
                MK_AO_int_fetch_and_add_acquire(addr,(unsigned int)(-1))
# define MK_AO_HAVE_int_fetch_and_sub1_acquire
#endif
#if defined(MK_AO_HAVE_int_fetch_and_add_write) \
    && !defined(MK_AO_HAVE_int_fetch_and_sub1_write)
# define MK_AO_int_fetch_and_sub1_write(addr) \
                MK_AO_int_fetch_and_add_write(addr,(unsigned int)(-1))
# define MK_AO_HAVE_int_fetch_and_sub1_write
#endif
#if defined(MK_AO_HAVE_int_fetch_and_add_read) \
    && !defined(MK_AO_HAVE_int_fetch_and_sub1_read)
# define MK_AO_int_fetch_and_sub1_read(addr) \
                MK_AO_int_fetch_and_add_read(addr,(unsigned int)(-1))
# define MK_AO_HAVE_int_fetch_and_sub1_read
#endif
#if defined(MK_AO_HAVE_int_fetch_and_add_release_write) \
    && !defined(MK_AO_HAVE_int_fetch_and_sub1_release_write)
# define MK_AO_int_fetch_and_sub1_release_write(addr) \
        MK_AO_int_fetch_and_add_release_write(addr,(unsigned int)(-1))
# define MK_AO_HAVE_int_fetch_and_sub1_release_write
#endif
#if defined(MK_AO_HAVE_int_fetch_and_add_acquire_read) \
    && !defined(MK_AO_HAVE_int_fetch_and_sub1_acquire_read)
# define MK_AO_int_fetch_and_sub1_acquire_read(addr) \
        MK_AO_int_fetch_and_add_acquire_read(addr,(unsigned int)(-1))
# define MK_AO_HAVE_int_fetch_and_sub1_acquire_read
#endif
#if defined(MK_AO_HAVE_int_fetch_and_add) \
    && !defined(MK_AO_HAVE_int_fetch_and_sub1)
# define MK_AO_int_fetch_and_sub1(addr) \
                MK_AO_int_fetch_and_add(addr,(unsigned int)(-1))
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
