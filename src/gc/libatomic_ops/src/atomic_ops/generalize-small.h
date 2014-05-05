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

/* char_fetch_compare_and_swap */
#if defined(MK_AO_HAVE_char_fetch_compare_and_swap) \
    && defined(MK_AO_HAVE_nop_full) \
    && !defined(MK_AO_HAVE_char_fetch_compare_and_swap_acquire)
  MK_AO_INLINE unsigned/**/char
  MK_AO_char_fetch_compare_and_swap_acquire(volatile unsigned/**/char *addr,
                                          unsigned/**/char old_val, unsigned/**/char new_val)
  {
    unsigned/**/char result = MK_AO_char_fetch_compare_and_swap(addr, old_val, new_val);
    MK_AO_nop_full();
    return result;
  }
# define MK_AO_HAVE_char_fetch_compare_and_swap_acquire
#endif
#if defined(MK_AO_HAVE_char_fetch_compare_and_swap) \
    && defined(MK_AO_HAVE_nop_full) \
    && !defined(MK_AO_HAVE_char_fetch_compare_and_swap_release)
# define MK_AO_char_fetch_compare_and_swap_release(addr, old_val, new_val) \
                (MK_AO_nop_full(), \
                 MK_AO_char_fetch_compare_and_swap(addr, old_val, new_val))
# define MK_AO_HAVE_char_fetch_compare_and_swap_release
#endif
#if defined(MK_AO_HAVE_char_fetch_compare_and_swap_full)
# if !defined(MK_AO_HAVE_char_fetch_compare_and_swap_release)
#   define MK_AO_char_fetch_compare_and_swap_release(addr, old_val, new_val) \
                MK_AO_char_fetch_compare_and_swap_full(addr, old_val, new_val)
#   define MK_AO_HAVE_char_fetch_compare_and_swap_release
# endif
# if !defined(MK_AO_HAVE_char_fetch_compare_and_swap_acquire)
#   define MK_AO_char_fetch_compare_and_swap_acquire(addr, old_val, new_val) \
                MK_AO_char_fetch_compare_and_swap_full(addr, old_val, new_val)
#   define MK_AO_HAVE_char_fetch_compare_and_swap_acquire
# endif
# if !defined(MK_AO_HAVE_char_fetch_compare_and_swap_write)
#   define MK_AO_char_fetch_compare_and_swap_write(addr, old_val, new_val) \
                MK_AO_char_fetch_compare_and_swap_full(addr, old_val, new_val)
#   define MK_AO_HAVE_char_fetch_compare_and_swap_write
# endif
# if !defined(MK_AO_HAVE_char_fetch_compare_and_swap_read)
#   define MK_AO_char_fetch_compare_and_swap_read(addr, old_val, new_val) \
                MK_AO_char_fetch_compare_and_swap_full(addr, old_val, new_val)
#   define MK_AO_HAVE_char_fetch_compare_and_swap_read
# endif
#endif /* MK_AO_HAVE_char_fetch_compare_and_swap_full */

#if !defined(MK_AO_HAVE_char_fetch_compare_and_swap) \
    && defined(MK_AO_HAVE_char_fetch_compare_and_swap_release)
# define MK_AO_char_fetch_compare_and_swap(addr, old_val, new_val) \
            MK_AO_char_fetch_compare_and_swap_release(addr, old_val, new_val)
# define MK_AO_HAVE_char_fetch_compare_and_swap
#endif
#if !defined(MK_AO_HAVE_char_fetch_compare_and_swap) \
    && defined(MK_AO_HAVE_char_fetch_compare_and_swap_acquire)
# define MK_AO_char_fetch_compare_and_swap(addr, old_val, new_val) \
            MK_AO_char_fetch_compare_and_swap_acquire(addr, old_val, new_val)
# define MK_AO_HAVE_char_fetch_compare_and_swap
#endif
#if !defined(MK_AO_HAVE_char_fetch_compare_and_swap) \
    && defined(MK_AO_HAVE_char_fetch_compare_and_swap_write)
# define MK_AO_char_fetch_compare_and_swap(addr, old_val, new_val) \
                MK_AO_char_fetch_compare_and_swap_write(addr, old_val, new_val)
# define MK_AO_HAVE_char_fetch_compare_and_swap
#endif
#if !defined(MK_AO_HAVE_char_fetch_compare_and_swap) \
    && defined(MK_AO_HAVE_char_fetch_compare_and_swap_read)
# define MK_AO_char_fetch_compare_and_swap(addr, old_val, new_val) \
                MK_AO_char_fetch_compare_and_swap_read(addr, old_val, new_val)
# define MK_AO_HAVE_char_fetch_compare_and_swap
#endif

#if defined(MK_AO_HAVE_char_fetch_compare_and_swap_acquire) \
    && defined(MK_AO_HAVE_nop_full) \
    && !defined(MK_AO_HAVE_char_fetch_compare_and_swap_full)
# define MK_AO_char_fetch_compare_and_swap_full(addr, old_val, new_val) \
            (MK_AO_nop_full(), \
             MK_AO_char_fetch_compare_and_swap_acquire(addr, old_val, new_val))
# define MK_AO_HAVE_char_fetch_compare_and_swap_full
#endif

#if !defined(MK_AO_HAVE_char_fetch_compare_and_swap_release_write) \
    && defined(MK_AO_HAVE_char_fetch_compare_and_swap_write)
# define MK_AO_char_fetch_compare_and_swap_release_write(addr,old_val,new_val) \
                MK_AO_char_fetch_compare_and_swap_write(addr, old_val, new_val)
# define MK_AO_HAVE_char_fetch_compare_and_swap_release_write
#endif
#if !defined(MK_AO_HAVE_char_fetch_compare_and_swap_release_write) \
    && defined(MK_AO_HAVE_char_fetch_compare_and_swap_release)
# define MK_AO_char_fetch_compare_and_swap_release_write(addr,old_val,new_val) \
            MK_AO_char_fetch_compare_and_swap_release(addr, old_val, new_val)
# define MK_AO_HAVE_char_fetch_compare_and_swap_release_write
#endif
#if !defined(MK_AO_HAVE_char_fetch_compare_and_swap_acquire_read) \
    && defined(MK_AO_HAVE_char_fetch_compare_and_swap_read)
# define MK_AO_char_fetch_compare_and_swap_acquire_read(addr,old_val,new_val) \
                MK_AO_char_fetch_compare_and_swap_read(addr, old_val, new_val)
# define MK_AO_HAVE_char_fetch_compare_and_swap_acquire_read
#endif
#if !defined(MK_AO_HAVE_char_fetch_compare_and_swap_acquire_read) \
    && defined(MK_AO_HAVE_char_fetch_compare_and_swap_acquire)
# define MK_AO_char_fetch_compare_and_swap_acquire_read(addr,old_val,new_val) \
            MK_AO_char_fetch_compare_and_swap_acquire(addr, old_val, new_val)
# define MK_AO_HAVE_char_fetch_compare_and_swap_acquire_read
#endif

#ifdef MK_AO_NO_DD_ORDERING
# if defined(MK_AO_HAVE_char_fetch_compare_and_swap_acquire_read)
#   define MK_AO_char_fetch_compare_and_swap_dd_acquire_read(addr,old_val,new_val) \
        MK_AO_char_fetch_compare_and_swap_acquire_read(addr, old_val, new_val)
#   define MK_AO_HAVE_char_fetch_compare_and_swap_dd_acquire_read
# endif
#else
# if defined(MK_AO_HAVE_char_fetch_compare_and_swap)
#   define MK_AO_char_fetch_compare_and_swap_dd_acquire_read(addr,old_val,new_val) \
                MK_AO_char_fetch_compare_and_swap(addr, old_val, new_val)
#   define MK_AO_HAVE_char_fetch_compare_and_swap_dd_acquire_read
# endif
#endif /* !MK_AO_NO_DD_ORDERING */

/* char_compare_and_swap */
#if defined(MK_AO_HAVE_char_compare_and_swap) && defined(MK_AO_HAVE_nop_full) \
    && !defined(MK_AO_HAVE_char_compare_and_swap_acquire)
  MK_AO_INLINE int
  MK_AO_char_compare_and_swap_acquire(volatile unsigned/**/char *addr, unsigned/**/char old,
                                    unsigned/**/char new_val)
  {
    int result = MK_AO_char_compare_and_swap(addr, old, new_val);
    MK_AO_nop_full();
    return result;
  }
# define MK_AO_HAVE_char_compare_and_swap_acquire
#endif
#if defined(MK_AO_HAVE_char_compare_and_swap) && defined(MK_AO_HAVE_nop_full) \
    && !defined(MK_AO_HAVE_char_compare_and_swap_release)
# define MK_AO_char_compare_and_swap_release(addr, old, new_val) \
                (MK_AO_nop_full(), MK_AO_char_compare_and_swap(addr, old, new_val))
# define MK_AO_HAVE_char_compare_and_swap_release
#endif
#if defined(MK_AO_HAVE_char_compare_and_swap_full)
# if !defined(MK_AO_HAVE_char_compare_and_swap_release)
#   define MK_AO_char_compare_and_swap_release(addr, old, new_val) \
                MK_AO_char_compare_and_swap_full(addr, old, new_val)
#   define MK_AO_HAVE_char_compare_and_swap_release
# endif
# if !defined(MK_AO_HAVE_char_compare_and_swap_acquire)
#   define MK_AO_char_compare_and_swap_acquire(addr, old, new_val) \
                MK_AO_char_compare_and_swap_full(addr, old, new_val)
#   define MK_AO_HAVE_char_compare_and_swap_acquire
# endif
# if !defined(MK_AO_HAVE_char_compare_and_swap_write)
#   define MK_AO_char_compare_and_swap_write(addr, old, new_val) \
                MK_AO_char_compare_and_swap_full(addr, old, new_val)
#   define MK_AO_HAVE_char_compare_and_swap_write
# endif
# if !defined(MK_AO_HAVE_char_compare_and_swap_read)
#   define MK_AO_char_compare_and_swap_read(addr, old, new_val) \
                MK_AO_char_compare_and_swap_full(addr, old, new_val)
#   define MK_AO_HAVE_char_compare_and_swap_read
# endif
#endif /* MK_AO_HAVE_char_compare_and_swap_full */

#if !defined(MK_AO_HAVE_char_compare_and_swap) \
    && defined(MK_AO_HAVE_char_compare_and_swap_release)
# define MK_AO_char_compare_and_swap(addr, old, new_val) \
                MK_AO_char_compare_and_swap_release(addr, old, new_val)
# define MK_AO_HAVE_char_compare_and_swap
#endif
#if !defined(MK_AO_HAVE_char_compare_and_swap) \
    && defined(MK_AO_HAVE_char_compare_and_swap_acquire)
# define MK_AO_char_compare_and_swap(addr, old, new_val) \
                MK_AO_char_compare_and_swap_acquire(addr, old, new_val)
# define MK_AO_HAVE_char_compare_and_swap
#endif
#if !defined(MK_AO_HAVE_char_compare_and_swap) \
    && defined(MK_AO_HAVE_char_compare_and_swap_write)
# define MK_AO_char_compare_and_swap(addr, old, new_val) \
                MK_AO_char_compare_and_swap_write(addr, old, new_val)
# define MK_AO_HAVE_char_compare_and_swap
#endif
#if !defined(MK_AO_HAVE_char_compare_and_swap) \
    && defined(MK_AO_HAVE_char_compare_and_swap_read)
# define MK_AO_char_compare_and_swap(addr, old, new_val) \
                MK_AO_char_compare_and_swap_read(addr, old, new_val)
# define MK_AO_HAVE_char_compare_and_swap
#endif

#if defined(MK_AO_HAVE_char_compare_and_swap_acquire) \
    && defined(MK_AO_HAVE_nop_full) \
    && !defined(MK_AO_HAVE_char_compare_and_swap_full)
# define MK_AO_char_compare_and_swap_full(addr, old, new_val) \
                (MK_AO_nop_full(), \
                 MK_AO_char_compare_and_swap_acquire(addr, old, new_val))
# define MK_AO_HAVE_char_compare_and_swap_full
#endif

#if !defined(MK_AO_HAVE_char_compare_and_swap_release_write) \
    && defined(MK_AO_HAVE_char_compare_and_swap_write)
# define MK_AO_char_compare_and_swap_release_write(addr, old, new_val) \
                MK_AO_char_compare_and_swap_write(addr, old, new_val)
# define MK_AO_HAVE_char_compare_and_swap_release_write
#endif
#if !defined(MK_AO_HAVE_char_compare_and_swap_release_write) \
    && defined(MK_AO_HAVE_char_compare_and_swap_release)
# define MK_AO_char_compare_and_swap_release_write(addr, old, new_val) \
                MK_AO_char_compare_and_swap_release(addr, old, new_val)
# define MK_AO_HAVE_char_compare_and_swap_release_write
#endif
#if !defined(MK_AO_HAVE_char_compare_and_swap_acquire_read) \
    && defined(MK_AO_HAVE_char_compare_and_swap_read)
# define MK_AO_char_compare_and_swap_acquire_read(addr, old, new_val) \
                MK_AO_char_compare_and_swap_read(addr, old, new_val)
# define MK_AO_HAVE_char_compare_and_swap_acquire_read
#endif
#if !defined(MK_AO_HAVE_char_compare_and_swap_acquire_read) \
    && defined(MK_AO_HAVE_char_compare_and_swap_acquire)
# define MK_AO_char_compare_and_swap_acquire_read(addr, old, new_val) \
                MK_AO_char_compare_and_swap_acquire(addr, old, new_val)
# define MK_AO_HAVE_char_compare_and_swap_acquire_read
#endif

#ifdef MK_AO_NO_DD_ORDERING
# if defined(MK_AO_HAVE_char_compare_and_swap_acquire_read)
#   define MK_AO_char_compare_and_swap_dd_acquire_read(addr, old, new_val) \
                MK_AO_char_compare_and_swap_acquire_read(addr, old, new_val)
#   define MK_AO_HAVE_char_compare_and_swap_dd_acquire_read
# endif
#else
# if defined(MK_AO_HAVE_char_compare_and_swap)
#   define MK_AO_char_compare_and_swap_dd_acquire_read(addr, old, new_val) \
                MK_AO_char_compare_and_swap(addr, old, new_val)
#   define MK_AO_HAVE_char_compare_and_swap_dd_acquire_read
# endif
#endif /* !MK_AO_NO_DD_ORDERING */

/* char_load */
#if defined(MK_AO_HAVE_char_load_full) && !defined(MK_AO_HAVE_char_load_acquire)
# define MK_AO_char_load_acquire(addr) MK_AO_char_load_full(addr)
# define MK_AO_HAVE_char_load_acquire
#endif

#if defined(MK_AO_HAVE_char_load_acquire) && !defined(MK_AO_HAVE_char_load)
# define MK_AO_char_load(addr) MK_AO_char_load_acquire(addr)
# define MK_AO_HAVE_char_load
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
  MK_AO_INLINE unsigned/**/char
  MK_AO_char_load_acquire(const volatile unsigned/**/char *addr)
  {
    unsigned/**/char result = MK_AO_char_load(addr);

    /* Acquire barrier would be useless, since the load could be delayed    */
    /* beyond it.                                                           */
    MK_AO_nop_full();
    return result;
  }
# define MK_AO_HAVE_char_load_acquire
#endif

#if defined(MK_AO_HAVE_char_load) && defined(MK_AO_HAVE_nop_read) \
    && !defined(MK_AO_HAVE_char_load_read)
  MK_AO_INLINE unsigned/**/char
  MK_AO_char_load_read(const volatile unsigned/**/char *addr)
  {
    unsigned/**/char result = MK_AO_char_load(addr);

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

#if defined(MK_AO_HAVE_char_compare_and_swap_read) \
    && !defined(MK_AO_HAVE_char_load_read)
# define MK_AO_char_CAS_BASED_LOAD_READ
  MK_AO_INLINE unsigned/**/char
  MK_AO_char_load_read(const volatile unsigned/**/char *addr)
  {
    unsigned/**/char result;

    do {
      result = *(const unsigned/**/char *)addr;
    } while (MK_AO_EXPECT_FALSE(!MK_AO_char_compare_and_swap_read(
                                                (volatile unsigned/**/char *)addr,
                                                result, result)));
    return result;
  }
# define MK_AO_HAVE_char_load_read
#endif

#if !defined(MK_AO_HAVE_char_load_acquire_read) \
    && defined(MK_AO_HAVE_char_load_read)
# define MK_AO_char_load_acquire_read(addr) MK_AO_char_load_read(addr)
# define MK_AO_HAVE_char_load_acquire_read
#endif

#if defined(MK_AO_HAVE_char_load_acquire_read) && !defined(MK_AO_HAVE_char_load) \
    && (!defined(MK_AO_char_CAS_BASED_LOAD_READ) \
        || !defined(MK_AO_HAVE_char_compare_and_swap))
# define MK_AO_char_load(addr) MK_AO_char_load_acquire_read(addr)
# define MK_AO_HAVE_char_load
#endif

#if defined(MK_AO_HAVE_char_compare_and_swap_full) \
    && !defined(MK_AO_HAVE_char_load_full)
  MK_AO_INLINE unsigned/**/char
  MK_AO_char_load_full(const volatile unsigned/**/char *addr)
  {
    unsigned/**/char result;

    do {
      result = *(const unsigned/**/char *)addr;
    } while (MK_AO_EXPECT_FALSE(!MK_AO_char_compare_and_swap_full(
                                                (volatile unsigned/**/char *)addr,
                                                result, result)));
    return result;
  }
# define MK_AO_HAVE_char_load_full
#endif

#if defined(MK_AO_HAVE_char_compare_and_swap_acquire) \
    && !defined(MK_AO_HAVE_char_load_acquire)
  MK_AO_INLINE unsigned/**/char
  MK_AO_char_load_acquire(const volatile unsigned/**/char *addr)
  {
    unsigned/**/char result;

    do {
      result = *(const unsigned/**/char *)addr;
    } while (MK_AO_EXPECT_FALSE(!MK_AO_char_compare_and_swap_acquire(
                                                (volatile unsigned/**/char *)addr,
                                                result, result)));
    return result;
  }
# define MK_AO_HAVE_char_load_acquire
#endif

#if defined(MK_AO_HAVE_char_compare_and_swap) && !defined(MK_AO_HAVE_char_load)
  MK_AO_INLINE unsigned/**/char
  MK_AO_char_load(const volatile unsigned/**/char *addr)
  {
    unsigned/**/char result;

    do {
      result = *(const unsigned/**/char *)addr;
    } while (MK_AO_EXPECT_FALSE(!MK_AO_char_compare_and_swap(
                                                (volatile unsigned/**/char *)addr,
                                                result, result)));
    return result;
  }
# define MK_AO_HAVE_char_load
#endif

#ifdef MK_AO_NO_DD_ORDERING
# if defined(MK_AO_HAVE_char_load_acquire_read)
#   define MK_AO_char_load_dd_acquire_read(addr) \
                                MK_AO_char_load_acquire_read(addr)
#   define MK_AO_HAVE_char_load_dd_acquire_read
# endif
#else
# if defined(MK_AO_HAVE_char_load)
#   define MK_AO_char_load_dd_acquire_read(addr) MK_AO_char_load(addr)
#   define MK_AO_HAVE_char_load_dd_acquire_read
# endif
#endif /* !MK_AO_NO_DD_ORDERING */

/* char_store */
#if defined(MK_AO_HAVE_char_store_full) && !defined(MK_AO_HAVE_char_store_release)
# define MK_AO_char_store_release(addr, val) MK_AO_char_store_full(addr, val)
# define MK_AO_HAVE_char_store_release
#endif

#if defined(MK_AO_HAVE_char_store_release) && !defined(MK_AO_HAVE_char_store)
# define MK_AO_char_store(addr, val) MK_AO_char_store_release(addr, val)
# define MK_AO_HAVE_char_store
#endif

#if defined(MK_AO_HAVE_char_store_full) && !defined(MK_AO_HAVE_char_store_write)
# define MK_AO_char_store_write(addr, val) MK_AO_char_store_full(addr, val)
# define MK_AO_HAVE_char_store_write
#endif

#if defined(MK_AO_HAVE_char_store_release) \
    && !defined(MK_AO_HAVE_char_store_release_write)
# define MK_AO_char_store_release_write(addr, val) \
                                MK_AO_char_store_release(addr, val)
# define MK_AO_HAVE_char_store_release_write
#endif

#if defined(MK_AO_HAVE_char_store_write) && !defined(MK_AO_HAVE_char_store)
# define MK_AO_char_store(addr, val) MK_AO_char_store_write(addr, val)
# define MK_AO_HAVE_char_store
#endif

#if defined(MK_AO_HAVE_char_store) && defined(MK_AO_HAVE_nop_full) \
    && !defined(MK_AO_HAVE_char_store_release)
# define MK_AO_char_store_release(addr, val) \
                                (MK_AO_nop_full(), MK_AO_char_store(addr, val))
# define MK_AO_HAVE_char_store_release
#endif

#if defined(MK_AO_HAVE_char_store) && defined(MK_AO_HAVE_nop_write) \
    && !defined(MK_AO_HAVE_char_store_write)
# define MK_AO_char_store_write(addr, val) \
                                (MK_AO_nop_write(), MK_AO_char_store(addr, val))
# define MK_AO_HAVE_char_store_write
#endif

#if defined(MK_AO_HAVE_char_compare_and_swap_write) \
    && !defined(MK_AO_HAVE_char_store_write)
  MK_AO_INLINE void
  MK_AO_char_store_write(volatile unsigned/**/char *addr, unsigned/**/char new_val)
  {
    unsigned/**/char old_val;

    do {
      old_val = *(unsigned/**/char *)addr;
    } while (MK_AO_EXPECT_FALSE(!MK_AO_char_compare_and_swap_write(addr, old_val,
                                                              new_val)));
  }
# define MK_AO_HAVE_char_store_write
#endif

#if defined(MK_AO_HAVE_char_store_write) \
    && !defined(MK_AO_HAVE_char_store_release_write)
# define MK_AO_char_store_release_write(addr, val) \
                                MK_AO_char_store_write(addr, val)
# define MK_AO_HAVE_char_store_release_write
#endif

#if defined(MK_AO_HAVE_char_store_release) && defined(MK_AO_HAVE_nop_full) \
    && !defined(MK_AO_HAVE_char_store_full)
# define MK_AO_char_store_full(addr, val) \
                                (MK_AO_char_store_release(addr, val), \
                                 MK_AO_nop_full())
# define MK_AO_HAVE_char_store_full
#endif

#if defined(MK_AO_HAVE_char_compare_and_swap) && !defined(MK_AO_HAVE_char_store)
  MK_AO_INLINE void
  MK_AO_char_store(volatile unsigned/**/char *addr, unsigned/**/char new_val)
  {
    unsigned/**/char old_val;

    do {
      old_val = *(unsigned/**/char *)addr;
    } while (MK_AO_EXPECT_FALSE(!MK_AO_char_compare_and_swap(addr,
                                                        old_val, new_val)));
  }
# define MK_AO_HAVE_char_store
#endif

#if defined(MK_AO_HAVE_char_compare_and_swap_release) \
    && !defined(MK_AO_HAVE_char_store_release)
  MK_AO_INLINE void
  MK_AO_char_store_release(volatile unsigned/**/char *addr, unsigned/**/char new_val)
  {
    unsigned/**/char old_val;

    do {
      old_val = *(unsigned/**/char *)addr;
    } while (MK_AO_EXPECT_FALSE(!MK_AO_char_compare_and_swap_release(addr, old_val,
                                                                new_val)));
  }
# define MK_AO_HAVE_char_store_release
#endif

#if defined(MK_AO_HAVE_char_compare_and_swap_full) \
    && !defined(MK_AO_HAVE_char_store_full)
  MK_AO_INLINE void
  MK_AO_char_store_full(volatile unsigned/**/char *addr, unsigned/**/char new_val)
  {
    unsigned/**/char old_val;

    do {
      old_val = *(unsigned/**/char *)addr;
    } while (MK_AO_EXPECT_FALSE(!MK_AO_char_compare_and_swap_full(addr, old_val,
                                                             new_val)));
  }
# define MK_AO_HAVE_char_store_full
#endif
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

/* short_fetch_compare_and_swap */
#if defined(MK_AO_HAVE_short_fetch_compare_and_swap) \
    && defined(MK_AO_HAVE_nop_full) \
    && !defined(MK_AO_HAVE_short_fetch_compare_and_swap_acquire)
  MK_AO_INLINE unsigned/**/short
  MK_AO_short_fetch_compare_and_swap_acquire(volatile unsigned/**/short *addr,
                                          unsigned/**/short old_val, unsigned/**/short new_val)
  {
    unsigned/**/short result = MK_AO_short_fetch_compare_and_swap(addr, old_val, new_val);
    MK_AO_nop_full();
    return result;
  }
# define MK_AO_HAVE_short_fetch_compare_and_swap_acquire
#endif
#if defined(MK_AO_HAVE_short_fetch_compare_and_swap) \
    && defined(MK_AO_HAVE_nop_full) \
    && !defined(MK_AO_HAVE_short_fetch_compare_and_swap_release)
# define MK_AO_short_fetch_compare_and_swap_release(addr, old_val, new_val) \
                (MK_AO_nop_full(), \
                 MK_AO_short_fetch_compare_and_swap(addr, old_val, new_val))
# define MK_AO_HAVE_short_fetch_compare_and_swap_release
#endif
#if defined(MK_AO_HAVE_short_fetch_compare_and_swap_full)
# if !defined(MK_AO_HAVE_short_fetch_compare_and_swap_release)
#   define MK_AO_short_fetch_compare_and_swap_release(addr, old_val, new_val) \
                MK_AO_short_fetch_compare_and_swap_full(addr, old_val, new_val)
#   define MK_AO_HAVE_short_fetch_compare_and_swap_release
# endif
# if !defined(MK_AO_HAVE_short_fetch_compare_and_swap_acquire)
#   define MK_AO_short_fetch_compare_and_swap_acquire(addr, old_val, new_val) \
                MK_AO_short_fetch_compare_and_swap_full(addr, old_val, new_val)
#   define MK_AO_HAVE_short_fetch_compare_and_swap_acquire
# endif
# if !defined(MK_AO_HAVE_short_fetch_compare_and_swap_write)
#   define MK_AO_short_fetch_compare_and_swap_write(addr, old_val, new_val) \
                MK_AO_short_fetch_compare_and_swap_full(addr, old_val, new_val)
#   define MK_AO_HAVE_short_fetch_compare_and_swap_write
# endif
# if !defined(MK_AO_HAVE_short_fetch_compare_and_swap_read)
#   define MK_AO_short_fetch_compare_and_swap_read(addr, old_val, new_val) \
                MK_AO_short_fetch_compare_and_swap_full(addr, old_val, new_val)
#   define MK_AO_HAVE_short_fetch_compare_and_swap_read
# endif
#endif /* MK_AO_HAVE_short_fetch_compare_and_swap_full */

#if !defined(MK_AO_HAVE_short_fetch_compare_and_swap) \
    && defined(MK_AO_HAVE_short_fetch_compare_and_swap_release)
# define MK_AO_short_fetch_compare_and_swap(addr, old_val, new_val) \
            MK_AO_short_fetch_compare_and_swap_release(addr, old_val, new_val)
# define MK_AO_HAVE_short_fetch_compare_and_swap
#endif
#if !defined(MK_AO_HAVE_short_fetch_compare_and_swap) \
    && defined(MK_AO_HAVE_short_fetch_compare_and_swap_acquire)
# define MK_AO_short_fetch_compare_and_swap(addr, old_val, new_val) \
            MK_AO_short_fetch_compare_and_swap_acquire(addr, old_val, new_val)
# define MK_AO_HAVE_short_fetch_compare_and_swap
#endif
#if !defined(MK_AO_HAVE_short_fetch_compare_and_swap) \
    && defined(MK_AO_HAVE_short_fetch_compare_and_swap_write)
# define MK_AO_short_fetch_compare_and_swap(addr, old_val, new_val) \
                MK_AO_short_fetch_compare_and_swap_write(addr, old_val, new_val)
# define MK_AO_HAVE_short_fetch_compare_and_swap
#endif
#if !defined(MK_AO_HAVE_short_fetch_compare_and_swap) \
    && defined(MK_AO_HAVE_short_fetch_compare_and_swap_read)
# define MK_AO_short_fetch_compare_and_swap(addr, old_val, new_val) \
                MK_AO_short_fetch_compare_and_swap_read(addr, old_val, new_val)
# define MK_AO_HAVE_short_fetch_compare_and_swap
#endif

#if defined(MK_AO_HAVE_short_fetch_compare_and_swap_acquire) \
    && defined(MK_AO_HAVE_nop_full) \
    && !defined(MK_AO_HAVE_short_fetch_compare_and_swap_full)
# define MK_AO_short_fetch_compare_and_swap_full(addr, old_val, new_val) \
            (MK_AO_nop_full(), \
             MK_AO_short_fetch_compare_and_swap_acquire(addr, old_val, new_val))
# define MK_AO_HAVE_short_fetch_compare_and_swap_full
#endif

#if !defined(MK_AO_HAVE_short_fetch_compare_and_swap_release_write) \
    && defined(MK_AO_HAVE_short_fetch_compare_and_swap_write)
# define MK_AO_short_fetch_compare_and_swap_release_write(addr,old_val,new_val) \
                MK_AO_short_fetch_compare_and_swap_write(addr, old_val, new_val)
# define MK_AO_HAVE_short_fetch_compare_and_swap_release_write
#endif
#if !defined(MK_AO_HAVE_short_fetch_compare_and_swap_release_write) \
    && defined(MK_AO_HAVE_short_fetch_compare_and_swap_release)
# define MK_AO_short_fetch_compare_and_swap_release_write(addr,old_val,new_val) \
            MK_AO_short_fetch_compare_and_swap_release(addr, old_val, new_val)
# define MK_AO_HAVE_short_fetch_compare_and_swap_release_write
#endif
#if !defined(MK_AO_HAVE_short_fetch_compare_and_swap_acquire_read) \
    && defined(MK_AO_HAVE_short_fetch_compare_and_swap_read)
# define MK_AO_short_fetch_compare_and_swap_acquire_read(addr,old_val,new_val) \
                MK_AO_short_fetch_compare_and_swap_read(addr, old_val, new_val)
# define MK_AO_HAVE_short_fetch_compare_and_swap_acquire_read
#endif
#if !defined(MK_AO_HAVE_short_fetch_compare_and_swap_acquire_read) \
    && defined(MK_AO_HAVE_short_fetch_compare_and_swap_acquire)
# define MK_AO_short_fetch_compare_and_swap_acquire_read(addr,old_val,new_val) \
            MK_AO_short_fetch_compare_and_swap_acquire(addr, old_val, new_val)
# define MK_AO_HAVE_short_fetch_compare_and_swap_acquire_read
#endif

#ifdef MK_AO_NO_DD_ORDERING
# if defined(MK_AO_HAVE_short_fetch_compare_and_swap_acquire_read)
#   define MK_AO_short_fetch_compare_and_swap_dd_acquire_read(addr,old_val,new_val) \
        MK_AO_short_fetch_compare_and_swap_acquire_read(addr, old_val, new_val)
#   define MK_AO_HAVE_short_fetch_compare_and_swap_dd_acquire_read
# endif
#else
# if defined(MK_AO_HAVE_short_fetch_compare_and_swap)
#   define MK_AO_short_fetch_compare_and_swap_dd_acquire_read(addr,old_val,new_val) \
                MK_AO_short_fetch_compare_and_swap(addr, old_val, new_val)
#   define MK_AO_HAVE_short_fetch_compare_and_swap_dd_acquire_read
# endif
#endif /* !MK_AO_NO_DD_ORDERING */

/* short_compare_and_swap */
#if defined(MK_AO_HAVE_short_compare_and_swap) && defined(MK_AO_HAVE_nop_full) \
    && !defined(MK_AO_HAVE_short_compare_and_swap_acquire)
  MK_AO_INLINE int
  MK_AO_short_compare_and_swap_acquire(volatile unsigned/**/short *addr, unsigned/**/short old,
                                    unsigned/**/short new_val)
  {
    int result = MK_AO_short_compare_and_swap(addr, old, new_val);
    MK_AO_nop_full();
    return result;
  }
# define MK_AO_HAVE_short_compare_and_swap_acquire
#endif
#if defined(MK_AO_HAVE_short_compare_and_swap) && defined(MK_AO_HAVE_nop_full) \
    && !defined(MK_AO_HAVE_short_compare_and_swap_release)
# define MK_AO_short_compare_and_swap_release(addr, old, new_val) \
                (MK_AO_nop_full(), MK_AO_short_compare_and_swap(addr, old, new_val))
# define MK_AO_HAVE_short_compare_and_swap_release
#endif
#if defined(MK_AO_HAVE_short_compare_and_swap_full)
# if !defined(MK_AO_HAVE_short_compare_and_swap_release)
#   define MK_AO_short_compare_and_swap_release(addr, old, new_val) \
                MK_AO_short_compare_and_swap_full(addr, old, new_val)
#   define MK_AO_HAVE_short_compare_and_swap_release
# endif
# if !defined(MK_AO_HAVE_short_compare_and_swap_acquire)
#   define MK_AO_short_compare_and_swap_acquire(addr, old, new_val) \
                MK_AO_short_compare_and_swap_full(addr, old, new_val)
#   define MK_AO_HAVE_short_compare_and_swap_acquire
# endif
# if !defined(MK_AO_HAVE_short_compare_and_swap_write)
#   define MK_AO_short_compare_and_swap_write(addr, old, new_val) \
                MK_AO_short_compare_and_swap_full(addr, old, new_val)
#   define MK_AO_HAVE_short_compare_and_swap_write
# endif
# if !defined(MK_AO_HAVE_short_compare_and_swap_read)
#   define MK_AO_short_compare_and_swap_read(addr, old, new_val) \
                MK_AO_short_compare_and_swap_full(addr, old, new_val)
#   define MK_AO_HAVE_short_compare_and_swap_read
# endif
#endif /* MK_AO_HAVE_short_compare_and_swap_full */

#if !defined(MK_AO_HAVE_short_compare_and_swap) \
    && defined(MK_AO_HAVE_short_compare_and_swap_release)
# define MK_AO_short_compare_and_swap(addr, old, new_val) \
                MK_AO_short_compare_and_swap_release(addr, old, new_val)
# define MK_AO_HAVE_short_compare_and_swap
#endif
#if !defined(MK_AO_HAVE_short_compare_and_swap) \
    && defined(MK_AO_HAVE_short_compare_and_swap_acquire)
# define MK_AO_short_compare_and_swap(addr, old, new_val) \
                MK_AO_short_compare_and_swap_acquire(addr, old, new_val)
# define MK_AO_HAVE_short_compare_and_swap
#endif
#if !defined(MK_AO_HAVE_short_compare_and_swap) \
    && defined(MK_AO_HAVE_short_compare_and_swap_write)
# define MK_AO_short_compare_and_swap(addr, old, new_val) \
                MK_AO_short_compare_and_swap_write(addr, old, new_val)
# define MK_AO_HAVE_short_compare_and_swap
#endif
#if !defined(MK_AO_HAVE_short_compare_and_swap) \
    && defined(MK_AO_HAVE_short_compare_and_swap_read)
# define MK_AO_short_compare_and_swap(addr, old, new_val) \
                MK_AO_short_compare_and_swap_read(addr, old, new_val)
# define MK_AO_HAVE_short_compare_and_swap
#endif

#if defined(MK_AO_HAVE_short_compare_and_swap_acquire) \
    && defined(MK_AO_HAVE_nop_full) \
    && !defined(MK_AO_HAVE_short_compare_and_swap_full)
# define MK_AO_short_compare_and_swap_full(addr, old, new_val) \
                (MK_AO_nop_full(), \
                 MK_AO_short_compare_and_swap_acquire(addr, old, new_val))
# define MK_AO_HAVE_short_compare_and_swap_full
#endif

#if !defined(MK_AO_HAVE_short_compare_and_swap_release_write) \
    && defined(MK_AO_HAVE_short_compare_and_swap_write)
# define MK_AO_short_compare_and_swap_release_write(addr, old, new_val) \
                MK_AO_short_compare_and_swap_write(addr, old, new_val)
# define MK_AO_HAVE_short_compare_and_swap_release_write
#endif
#if !defined(MK_AO_HAVE_short_compare_and_swap_release_write) \
    && defined(MK_AO_HAVE_short_compare_and_swap_release)
# define MK_AO_short_compare_and_swap_release_write(addr, old, new_val) \
                MK_AO_short_compare_and_swap_release(addr, old, new_val)
# define MK_AO_HAVE_short_compare_and_swap_release_write
#endif
#if !defined(MK_AO_HAVE_short_compare_and_swap_acquire_read) \
    && defined(MK_AO_HAVE_short_compare_and_swap_read)
# define MK_AO_short_compare_and_swap_acquire_read(addr, old, new_val) \
                MK_AO_short_compare_and_swap_read(addr, old, new_val)
# define MK_AO_HAVE_short_compare_and_swap_acquire_read
#endif
#if !defined(MK_AO_HAVE_short_compare_and_swap_acquire_read) \
    && defined(MK_AO_HAVE_short_compare_and_swap_acquire)
# define MK_AO_short_compare_and_swap_acquire_read(addr, old, new_val) \
                MK_AO_short_compare_and_swap_acquire(addr, old, new_val)
# define MK_AO_HAVE_short_compare_and_swap_acquire_read
#endif

#ifdef MK_AO_NO_DD_ORDERING
# if defined(MK_AO_HAVE_short_compare_and_swap_acquire_read)
#   define MK_AO_short_compare_and_swap_dd_acquire_read(addr, old, new_val) \
                MK_AO_short_compare_and_swap_acquire_read(addr, old, new_val)
#   define MK_AO_HAVE_short_compare_and_swap_dd_acquire_read
# endif
#else
# if defined(MK_AO_HAVE_short_compare_and_swap)
#   define MK_AO_short_compare_and_swap_dd_acquire_read(addr, old, new_val) \
                MK_AO_short_compare_and_swap(addr, old, new_val)
#   define MK_AO_HAVE_short_compare_and_swap_dd_acquire_read
# endif
#endif /* !MK_AO_NO_DD_ORDERING */

/* short_load */
#if defined(MK_AO_HAVE_short_load_full) && !defined(MK_AO_HAVE_short_load_acquire)
# define MK_AO_short_load_acquire(addr) MK_AO_short_load_full(addr)
# define MK_AO_HAVE_short_load_acquire
#endif

#if defined(MK_AO_HAVE_short_load_acquire) && !defined(MK_AO_HAVE_short_load)
# define MK_AO_short_load(addr) MK_AO_short_load_acquire(addr)
# define MK_AO_HAVE_short_load
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
  MK_AO_INLINE unsigned/**/short
  MK_AO_short_load_acquire(const volatile unsigned/**/short *addr)
  {
    unsigned/**/short result = MK_AO_short_load(addr);

    /* Acquire barrier would be useless, since the load could be delayed    */
    /* beyond it.                                                           */
    MK_AO_nop_full();
    return result;
  }
# define MK_AO_HAVE_short_load_acquire
#endif

#if defined(MK_AO_HAVE_short_load) && defined(MK_AO_HAVE_nop_read) \
    && !defined(MK_AO_HAVE_short_load_read)
  MK_AO_INLINE unsigned/**/short
  MK_AO_short_load_read(const volatile unsigned/**/short *addr)
  {
    unsigned/**/short result = MK_AO_short_load(addr);

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

#if defined(MK_AO_HAVE_short_compare_and_swap_read) \
    && !defined(MK_AO_HAVE_short_load_read)
# define MK_AO_short_CAS_BASED_LOAD_READ
  MK_AO_INLINE unsigned/**/short
  MK_AO_short_load_read(const volatile unsigned/**/short *addr)
  {
    unsigned/**/short result;

    do {
      result = *(const unsigned/**/short *)addr;
    } while (MK_AO_EXPECT_FALSE(!MK_AO_short_compare_and_swap_read(
                                                (volatile unsigned/**/short *)addr,
                                                result, result)));
    return result;
  }
# define MK_AO_HAVE_short_load_read
#endif

#if !defined(MK_AO_HAVE_short_load_acquire_read) \
    && defined(MK_AO_HAVE_short_load_read)
# define MK_AO_short_load_acquire_read(addr) MK_AO_short_load_read(addr)
# define MK_AO_HAVE_short_load_acquire_read
#endif

#if defined(MK_AO_HAVE_short_load_acquire_read) && !defined(MK_AO_HAVE_short_load) \
    && (!defined(MK_AO_short_CAS_BASED_LOAD_READ) \
        || !defined(MK_AO_HAVE_short_compare_and_swap))
# define MK_AO_short_load(addr) MK_AO_short_load_acquire_read(addr)
# define MK_AO_HAVE_short_load
#endif

#if defined(MK_AO_HAVE_short_compare_and_swap_full) \
    && !defined(MK_AO_HAVE_short_load_full)
  MK_AO_INLINE unsigned/**/short
  MK_AO_short_load_full(const volatile unsigned/**/short *addr)
  {
    unsigned/**/short result;

    do {
      result = *(const unsigned/**/short *)addr;
    } while (MK_AO_EXPECT_FALSE(!MK_AO_short_compare_and_swap_full(
                                                (volatile unsigned/**/short *)addr,
                                                result, result)));
    return result;
  }
# define MK_AO_HAVE_short_load_full
#endif

#if defined(MK_AO_HAVE_short_compare_and_swap_acquire) \
    && !defined(MK_AO_HAVE_short_load_acquire)
  MK_AO_INLINE unsigned/**/short
  MK_AO_short_load_acquire(const volatile unsigned/**/short *addr)
  {
    unsigned/**/short result;

    do {
      result = *(const unsigned/**/short *)addr;
    } while (MK_AO_EXPECT_FALSE(!MK_AO_short_compare_and_swap_acquire(
                                                (volatile unsigned/**/short *)addr,
                                                result, result)));
    return result;
  }
# define MK_AO_HAVE_short_load_acquire
#endif

#if defined(MK_AO_HAVE_short_compare_and_swap) && !defined(MK_AO_HAVE_short_load)
  MK_AO_INLINE unsigned/**/short
  MK_AO_short_load(const volatile unsigned/**/short *addr)
  {
    unsigned/**/short result;

    do {
      result = *(const unsigned/**/short *)addr;
    } while (MK_AO_EXPECT_FALSE(!MK_AO_short_compare_and_swap(
                                                (volatile unsigned/**/short *)addr,
                                                result, result)));
    return result;
  }
# define MK_AO_HAVE_short_load
#endif

#ifdef MK_AO_NO_DD_ORDERING
# if defined(MK_AO_HAVE_short_load_acquire_read)
#   define MK_AO_short_load_dd_acquire_read(addr) \
                                MK_AO_short_load_acquire_read(addr)
#   define MK_AO_HAVE_short_load_dd_acquire_read
# endif
#else
# if defined(MK_AO_HAVE_short_load)
#   define MK_AO_short_load_dd_acquire_read(addr) MK_AO_short_load(addr)
#   define MK_AO_HAVE_short_load_dd_acquire_read
# endif
#endif /* !MK_AO_NO_DD_ORDERING */

/* short_store */
#if defined(MK_AO_HAVE_short_store_full) && !defined(MK_AO_HAVE_short_store_release)
# define MK_AO_short_store_release(addr, val) MK_AO_short_store_full(addr, val)
# define MK_AO_HAVE_short_store_release
#endif

#if defined(MK_AO_HAVE_short_store_release) && !defined(MK_AO_HAVE_short_store)
# define MK_AO_short_store(addr, val) MK_AO_short_store_release(addr, val)
# define MK_AO_HAVE_short_store
#endif

#if defined(MK_AO_HAVE_short_store_full) && !defined(MK_AO_HAVE_short_store_write)
# define MK_AO_short_store_write(addr, val) MK_AO_short_store_full(addr, val)
# define MK_AO_HAVE_short_store_write
#endif

#if defined(MK_AO_HAVE_short_store_release) \
    && !defined(MK_AO_HAVE_short_store_release_write)
# define MK_AO_short_store_release_write(addr, val) \
                                MK_AO_short_store_release(addr, val)
# define MK_AO_HAVE_short_store_release_write
#endif

#if defined(MK_AO_HAVE_short_store_write) && !defined(MK_AO_HAVE_short_store)
# define MK_AO_short_store(addr, val) MK_AO_short_store_write(addr, val)
# define MK_AO_HAVE_short_store
#endif

#if defined(MK_AO_HAVE_short_store) && defined(MK_AO_HAVE_nop_full) \
    && !defined(MK_AO_HAVE_short_store_release)
# define MK_AO_short_store_release(addr, val) \
                                (MK_AO_nop_full(), MK_AO_short_store(addr, val))
# define MK_AO_HAVE_short_store_release
#endif

#if defined(MK_AO_HAVE_short_store) && defined(MK_AO_HAVE_nop_write) \
    && !defined(MK_AO_HAVE_short_store_write)
# define MK_AO_short_store_write(addr, val) \
                                (MK_AO_nop_write(), MK_AO_short_store(addr, val))
# define MK_AO_HAVE_short_store_write
#endif

#if defined(MK_AO_HAVE_short_compare_and_swap_write) \
    && !defined(MK_AO_HAVE_short_store_write)
  MK_AO_INLINE void
  MK_AO_short_store_write(volatile unsigned/**/short *addr, unsigned/**/short new_val)
  {
    unsigned/**/short old_val;

    do {
      old_val = *(unsigned/**/short *)addr;
    } while (MK_AO_EXPECT_FALSE(!MK_AO_short_compare_and_swap_write(addr, old_val,
                                                              new_val)));
  }
# define MK_AO_HAVE_short_store_write
#endif

#if defined(MK_AO_HAVE_short_store_write) \
    && !defined(MK_AO_HAVE_short_store_release_write)
# define MK_AO_short_store_release_write(addr, val) \
                                MK_AO_short_store_write(addr, val)
# define MK_AO_HAVE_short_store_release_write
#endif

#if defined(MK_AO_HAVE_short_store_release) && defined(MK_AO_HAVE_nop_full) \
    && !defined(MK_AO_HAVE_short_store_full)
# define MK_AO_short_store_full(addr, val) \
                                (MK_AO_short_store_release(addr, val), \
                                 MK_AO_nop_full())
# define MK_AO_HAVE_short_store_full
#endif

#if defined(MK_AO_HAVE_short_compare_and_swap) && !defined(MK_AO_HAVE_short_store)
  MK_AO_INLINE void
  MK_AO_short_store(volatile unsigned/**/short *addr, unsigned/**/short new_val)
  {
    unsigned/**/short old_val;

    do {
      old_val = *(unsigned/**/short *)addr;
    } while (MK_AO_EXPECT_FALSE(!MK_AO_short_compare_and_swap(addr,
                                                        old_val, new_val)));
  }
# define MK_AO_HAVE_short_store
#endif

#if defined(MK_AO_HAVE_short_compare_and_swap_release) \
    && !defined(MK_AO_HAVE_short_store_release)
  MK_AO_INLINE void
  MK_AO_short_store_release(volatile unsigned/**/short *addr, unsigned/**/short new_val)
  {
    unsigned/**/short old_val;

    do {
      old_val = *(unsigned/**/short *)addr;
    } while (MK_AO_EXPECT_FALSE(!MK_AO_short_compare_and_swap_release(addr, old_val,
                                                                new_val)));
  }
# define MK_AO_HAVE_short_store_release
#endif

#if defined(MK_AO_HAVE_short_compare_and_swap_full) \
    && !defined(MK_AO_HAVE_short_store_full)
  MK_AO_INLINE void
  MK_AO_short_store_full(volatile unsigned/**/short *addr, unsigned/**/short new_val)
  {
    unsigned/**/short old_val;

    do {
      old_val = *(unsigned/**/short *)addr;
    } while (MK_AO_EXPECT_FALSE(!MK_AO_short_compare_and_swap_full(addr, old_val,
                                                             new_val)));
  }
# define MK_AO_HAVE_short_store_full
#endif
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

/* int_fetch_compare_and_swap */
#if defined(MK_AO_HAVE_int_fetch_compare_and_swap) \
    && defined(MK_AO_HAVE_nop_full) \
    && !defined(MK_AO_HAVE_int_fetch_compare_and_swap_acquire)
  MK_AO_INLINE unsigned
  MK_AO_int_fetch_compare_and_swap_acquire(volatile unsigned *addr,
                                          unsigned old_val, unsigned new_val)
  {
    unsigned result = MK_AO_int_fetch_compare_and_swap(addr, old_val, new_val);
    MK_AO_nop_full();
    return result;
  }
# define MK_AO_HAVE_int_fetch_compare_and_swap_acquire
#endif
#if defined(MK_AO_HAVE_int_fetch_compare_and_swap) \
    && defined(MK_AO_HAVE_nop_full) \
    && !defined(MK_AO_HAVE_int_fetch_compare_and_swap_release)
# define MK_AO_int_fetch_compare_and_swap_release(addr, old_val, new_val) \
                (MK_AO_nop_full(), \
                 MK_AO_int_fetch_compare_and_swap(addr, old_val, new_val))
# define MK_AO_HAVE_int_fetch_compare_and_swap_release
#endif
#if defined(MK_AO_HAVE_int_fetch_compare_and_swap_full)
# if !defined(MK_AO_HAVE_int_fetch_compare_and_swap_release)
#   define MK_AO_int_fetch_compare_and_swap_release(addr, old_val, new_val) \
                MK_AO_int_fetch_compare_and_swap_full(addr, old_val, new_val)
#   define MK_AO_HAVE_int_fetch_compare_and_swap_release
# endif
# if !defined(MK_AO_HAVE_int_fetch_compare_and_swap_acquire)
#   define MK_AO_int_fetch_compare_and_swap_acquire(addr, old_val, new_val) \
                MK_AO_int_fetch_compare_and_swap_full(addr, old_val, new_val)
#   define MK_AO_HAVE_int_fetch_compare_and_swap_acquire
# endif
# if !defined(MK_AO_HAVE_int_fetch_compare_and_swap_write)
#   define MK_AO_int_fetch_compare_and_swap_write(addr, old_val, new_val) \
                MK_AO_int_fetch_compare_and_swap_full(addr, old_val, new_val)
#   define MK_AO_HAVE_int_fetch_compare_and_swap_write
# endif
# if !defined(MK_AO_HAVE_int_fetch_compare_and_swap_read)
#   define MK_AO_int_fetch_compare_and_swap_read(addr, old_val, new_val) \
                MK_AO_int_fetch_compare_and_swap_full(addr, old_val, new_val)
#   define MK_AO_HAVE_int_fetch_compare_and_swap_read
# endif
#endif /* MK_AO_HAVE_int_fetch_compare_and_swap_full */

#if !defined(MK_AO_HAVE_int_fetch_compare_and_swap) \
    && defined(MK_AO_HAVE_int_fetch_compare_and_swap_release)
# define MK_AO_int_fetch_compare_and_swap(addr, old_val, new_val) \
            MK_AO_int_fetch_compare_and_swap_release(addr, old_val, new_val)
# define MK_AO_HAVE_int_fetch_compare_and_swap
#endif
#if !defined(MK_AO_HAVE_int_fetch_compare_and_swap) \
    && defined(MK_AO_HAVE_int_fetch_compare_and_swap_acquire)
# define MK_AO_int_fetch_compare_and_swap(addr, old_val, new_val) \
            MK_AO_int_fetch_compare_and_swap_acquire(addr, old_val, new_val)
# define MK_AO_HAVE_int_fetch_compare_and_swap
#endif
#if !defined(MK_AO_HAVE_int_fetch_compare_and_swap) \
    && defined(MK_AO_HAVE_int_fetch_compare_and_swap_write)
# define MK_AO_int_fetch_compare_and_swap(addr, old_val, new_val) \
                MK_AO_int_fetch_compare_and_swap_write(addr, old_val, new_val)
# define MK_AO_HAVE_int_fetch_compare_and_swap
#endif
#if !defined(MK_AO_HAVE_int_fetch_compare_and_swap) \
    && defined(MK_AO_HAVE_int_fetch_compare_and_swap_read)
# define MK_AO_int_fetch_compare_and_swap(addr, old_val, new_val) \
                MK_AO_int_fetch_compare_and_swap_read(addr, old_val, new_val)
# define MK_AO_HAVE_int_fetch_compare_and_swap
#endif

#if defined(MK_AO_HAVE_int_fetch_compare_and_swap_acquire) \
    && defined(MK_AO_HAVE_nop_full) \
    && !defined(MK_AO_HAVE_int_fetch_compare_and_swap_full)
# define MK_AO_int_fetch_compare_and_swap_full(addr, old_val, new_val) \
            (MK_AO_nop_full(), \
             MK_AO_int_fetch_compare_and_swap_acquire(addr, old_val, new_val))
# define MK_AO_HAVE_int_fetch_compare_and_swap_full
#endif

#if !defined(MK_AO_HAVE_int_fetch_compare_and_swap_release_write) \
    && defined(MK_AO_HAVE_int_fetch_compare_and_swap_write)
# define MK_AO_int_fetch_compare_and_swap_release_write(addr,old_val,new_val) \
                MK_AO_int_fetch_compare_and_swap_write(addr, old_val, new_val)
# define MK_AO_HAVE_int_fetch_compare_and_swap_release_write
#endif
#if !defined(MK_AO_HAVE_int_fetch_compare_and_swap_release_write) \
    && defined(MK_AO_HAVE_int_fetch_compare_and_swap_release)
# define MK_AO_int_fetch_compare_and_swap_release_write(addr,old_val,new_val) \
            MK_AO_int_fetch_compare_and_swap_release(addr, old_val, new_val)
# define MK_AO_HAVE_int_fetch_compare_and_swap_release_write
#endif
#if !defined(MK_AO_HAVE_int_fetch_compare_and_swap_acquire_read) \
    && defined(MK_AO_HAVE_int_fetch_compare_and_swap_read)
# define MK_AO_int_fetch_compare_and_swap_acquire_read(addr,old_val,new_val) \
                MK_AO_int_fetch_compare_and_swap_read(addr, old_val, new_val)
# define MK_AO_HAVE_int_fetch_compare_and_swap_acquire_read
#endif
#if !defined(MK_AO_HAVE_int_fetch_compare_and_swap_acquire_read) \
    && defined(MK_AO_HAVE_int_fetch_compare_and_swap_acquire)
# define MK_AO_int_fetch_compare_and_swap_acquire_read(addr,old_val,new_val) \
            MK_AO_int_fetch_compare_and_swap_acquire(addr, old_val, new_val)
# define MK_AO_HAVE_int_fetch_compare_and_swap_acquire_read
#endif

#ifdef MK_AO_NO_DD_ORDERING
# if defined(MK_AO_HAVE_int_fetch_compare_and_swap_acquire_read)
#   define MK_AO_int_fetch_compare_and_swap_dd_acquire_read(addr,old_val,new_val) \
        MK_AO_int_fetch_compare_and_swap_acquire_read(addr, old_val, new_val)
#   define MK_AO_HAVE_int_fetch_compare_and_swap_dd_acquire_read
# endif
#else
# if defined(MK_AO_HAVE_int_fetch_compare_and_swap)
#   define MK_AO_int_fetch_compare_and_swap_dd_acquire_read(addr,old_val,new_val) \
                MK_AO_int_fetch_compare_and_swap(addr, old_val, new_val)
#   define MK_AO_HAVE_int_fetch_compare_and_swap_dd_acquire_read
# endif
#endif /* !MK_AO_NO_DD_ORDERING */

/* int_compare_and_swap */
#if defined(MK_AO_HAVE_int_compare_and_swap) && defined(MK_AO_HAVE_nop_full) \
    && !defined(MK_AO_HAVE_int_compare_and_swap_acquire)
  MK_AO_INLINE int
  MK_AO_int_compare_and_swap_acquire(volatile unsigned *addr, unsigned old,
                                    unsigned new_val)
  {
    int result = MK_AO_int_compare_and_swap(addr, old, new_val);
    MK_AO_nop_full();
    return result;
  }
# define MK_AO_HAVE_int_compare_and_swap_acquire
#endif
#if defined(MK_AO_HAVE_int_compare_and_swap) && defined(MK_AO_HAVE_nop_full) \
    && !defined(MK_AO_HAVE_int_compare_and_swap_release)
# define MK_AO_int_compare_and_swap_release(addr, old, new_val) \
                (MK_AO_nop_full(), MK_AO_int_compare_and_swap(addr, old, new_val))
# define MK_AO_HAVE_int_compare_and_swap_release
#endif
#if defined(MK_AO_HAVE_int_compare_and_swap_full)
# if !defined(MK_AO_HAVE_int_compare_and_swap_release)
#   define MK_AO_int_compare_and_swap_release(addr, old, new_val) \
                MK_AO_int_compare_and_swap_full(addr, old, new_val)
#   define MK_AO_HAVE_int_compare_and_swap_release
# endif
# if !defined(MK_AO_HAVE_int_compare_and_swap_acquire)
#   define MK_AO_int_compare_and_swap_acquire(addr, old, new_val) \
                MK_AO_int_compare_and_swap_full(addr, old, new_val)
#   define MK_AO_HAVE_int_compare_and_swap_acquire
# endif
# if !defined(MK_AO_HAVE_int_compare_and_swap_write)
#   define MK_AO_int_compare_and_swap_write(addr, old, new_val) \
                MK_AO_int_compare_and_swap_full(addr, old, new_val)
#   define MK_AO_HAVE_int_compare_and_swap_write
# endif
# if !defined(MK_AO_HAVE_int_compare_and_swap_read)
#   define MK_AO_int_compare_and_swap_read(addr, old, new_val) \
                MK_AO_int_compare_and_swap_full(addr, old, new_val)
#   define MK_AO_HAVE_int_compare_and_swap_read
# endif
#endif /* MK_AO_HAVE_int_compare_and_swap_full */

#if !defined(MK_AO_HAVE_int_compare_and_swap) \
    && defined(MK_AO_HAVE_int_compare_and_swap_release)
# define MK_AO_int_compare_and_swap(addr, old, new_val) \
                MK_AO_int_compare_and_swap_release(addr, old, new_val)
# define MK_AO_HAVE_int_compare_and_swap
#endif
#if !defined(MK_AO_HAVE_int_compare_and_swap) \
    && defined(MK_AO_HAVE_int_compare_and_swap_acquire)
# define MK_AO_int_compare_and_swap(addr, old, new_val) \
                MK_AO_int_compare_and_swap_acquire(addr, old, new_val)
# define MK_AO_HAVE_int_compare_and_swap
#endif
#if !defined(MK_AO_HAVE_int_compare_and_swap) \
    && defined(MK_AO_HAVE_int_compare_and_swap_write)
# define MK_AO_int_compare_and_swap(addr, old, new_val) \
                MK_AO_int_compare_and_swap_write(addr, old, new_val)
# define MK_AO_HAVE_int_compare_and_swap
#endif
#if !defined(MK_AO_HAVE_int_compare_and_swap) \
    && defined(MK_AO_HAVE_int_compare_and_swap_read)
# define MK_AO_int_compare_and_swap(addr, old, new_val) \
                MK_AO_int_compare_and_swap_read(addr, old, new_val)
# define MK_AO_HAVE_int_compare_and_swap
#endif

#if defined(MK_AO_HAVE_int_compare_and_swap_acquire) \
    && defined(MK_AO_HAVE_nop_full) \
    && !defined(MK_AO_HAVE_int_compare_and_swap_full)
# define MK_AO_int_compare_and_swap_full(addr, old, new_val) \
                (MK_AO_nop_full(), \
                 MK_AO_int_compare_and_swap_acquire(addr, old, new_val))
# define MK_AO_HAVE_int_compare_and_swap_full
#endif

#if !defined(MK_AO_HAVE_int_compare_and_swap_release_write) \
    && defined(MK_AO_HAVE_int_compare_and_swap_write)
# define MK_AO_int_compare_and_swap_release_write(addr, old, new_val) \
                MK_AO_int_compare_and_swap_write(addr, old, new_val)
# define MK_AO_HAVE_int_compare_and_swap_release_write
#endif
#if !defined(MK_AO_HAVE_int_compare_and_swap_release_write) \
    && defined(MK_AO_HAVE_int_compare_and_swap_release)
# define MK_AO_int_compare_and_swap_release_write(addr, old, new_val) \
                MK_AO_int_compare_and_swap_release(addr, old, new_val)
# define MK_AO_HAVE_int_compare_and_swap_release_write
#endif
#if !defined(MK_AO_HAVE_int_compare_and_swap_acquire_read) \
    && defined(MK_AO_HAVE_int_compare_and_swap_read)
# define MK_AO_int_compare_and_swap_acquire_read(addr, old, new_val) \
                MK_AO_int_compare_and_swap_read(addr, old, new_val)
# define MK_AO_HAVE_int_compare_and_swap_acquire_read
#endif
#if !defined(MK_AO_HAVE_int_compare_and_swap_acquire_read) \
    && defined(MK_AO_HAVE_int_compare_and_swap_acquire)
# define MK_AO_int_compare_and_swap_acquire_read(addr, old, new_val) \
                MK_AO_int_compare_and_swap_acquire(addr, old, new_val)
# define MK_AO_HAVE_int_compare_and_swap_acquire_read
#endif

#ifdef MK_AO_NO_DD_ORDERING
# if defined(MK_AO_HAVE_int_compare_and_swap_acquire_read)
#   define MK_AO_int_compare_and_swap_dd_acquire_read(addr, old, new_val) \
                MK_AO_int_compare_and_swap_acquire_read(addr, old, new_val)
#   define MK_AO_HAVE_int_compare_and_swap_dd_acquire_read
# endif
#else
# if defined(MK_AO_HAVE_int_compare_and_swap)
#   define MK_AO_int_compare_and_swap_dd_acquire_read(addr, old, new_val) \
                MK_AO_int_compare_and_swap(addr, old, new_val)
#   define MK_AO_HAVE_int_compare_and_swap_dd_acquire_read
# endif
#endif /* !MK_AO_NO_DD_ORDERING */

/* int_load */
#if defined(MK_AO_HAVE_int_load_full) && !defined(MK_AO_HAVE_int_load_acquire)
# define MK_AO_int_load_acquire(addr) MK_AO_int_load_full(addr)
# define MK_AO_HAVE_int_load_acquire
#endif

#if defined(MK_AO_HAVE_int_load_acquire) && !defined(MK_AO_HAVE_int_load)
# define MK_AO_int_load(addr) MK_AO_int_load_acquire(addr)
# define MK_AO_HAVE_int_load
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
  MK_AO_INLINE unsigned
  MK_AO_int_load_acquire(const volatile unsigned *addr)
  {
    unsigned result = MK_AO_int_load(addr);

    /* Acquire barrier would be useless, since the load could be delayed    */
    /* beyond it.                                                           */
    MK_AO_nop_full();
    return result;
  }
# define MK_AO_HAVE_int_load_acquire
#endif

#if defined(MK_AO_HAVE_int_load) && defined(MK_AO_HAVE_nop_read) \
    && !defined(MK_AO_HAVE_int_load_read)
  MK_AO_INLINE unsigned
  MK_AO_int_load_read(const volatile unsigned *addr)
  {
    unsigned result = MK_AO_int_load(addr);

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

#if defined(MK_AO_HAVE_int_compare_and_swap_read) \
    && !defined(MK_AO_HAVE_int_load_read)
# define MK_AO_int_CAS_BASED_LOAD_READ
  MK_AO_INLINE unsigned
  MK_AO_int_load_read(const volatile unsigned *addr)
  {
    unsigned result;

    do {
      result = *(const unsigned *)addr;
    } while (MK_AO_EXPECT_FALSE(!MK_AO_int_compare_and_swap_read(
                                                (volatile unsigned *)addr,
                                                result, result)));
    return result;
  }
# define MK_AO_HAVE_int_load_read
#endif

#if !defined(MK_AO_HAVE_int_load_acquire_read) \
    && defined(MK_AO_HAVE_int_load_read)
# define MK_AO_int_load_acquire_read(addr) MK_AO_int_load_read(addr)
# define MK_AO_HAVE_int_load_acquire_read
#endif

#if defined(MK_AO_HAVE_int_load_acquire_read) && !defined(MK_AO_HAVE_int_load) \
    && (!defined(MK_AO_int_CAS_BASED_LOAD_READ) \
        || !defined(MK_AO_HAVE_int_compare_and_swap))
# define MK_AO_int_load(addr) MK_AO_int_load_acquire_read(addr)
# define MK_AO_HAVE_int_load
#endif

#if defined(MK_AO_HAVE_int_compare_and_swap_full) \
    && !defined(MK_AO_HAVE_int_load_full)
  MK_AO_INLINE unsigned
  MK_AO_int_load_full(const volatile unsigned *addr)
  {
    unsigned result;

    do {
      result = *(const unsigned *)addr;
    } while (MK_AO_EXPECT_FALSE(!MK_AO_int_compare_and_swap_full(
                                                (volatile unsigned *)addr,
                                                result, result)));
    return result;
  }
# define MK_AO_HAVE_int_load_full
#endif

#if defined(MK_AO_HAVE_int_compare_and_swap_acquire) \
    && !defined(MK_AO_HAVE_int_load_acquire)
  MK_AO_INLINE unsigned
  MK_AO_int_load_acquire(const volatile unsigned *addr)
  {
    unsigned result;

    do {
      result = *(const unsigned *)addr;
    } while (MK_AO_EXPECT_FALSE(!MK_AO_int_compare_and_swap_acquire(
                                                (volatile unsigned *)addr,
                                                result, result)));
    return result;
  }
# define MK_AO_HAVE_int_load_acquire
#endif

#if defined(MK_AO_HAVE_int_compare_and_swap) && !defined(MK_AO_HAVE_int_load)
  MK_AO_INLINE unsigned
  MK_AO_int_load(const volatile unsigned *addr)
  {
    unsigned result;

    do {
      result = *(const unsigned *)addr;
    } while (MK_AO_EXPECT_FALSE(!MK_AO_int_compare_and_swap(
                                                (volatile unsigned *)addr,
                                                result, result)));
    return result;
  }
# define MK_AO_HAVE_int_load
#endif

#ifdef MK_AO_NO_DD_ORDERING
# if defined(MK_AO_HAVE_int_load_acquire_read)
#   define MK_AO_int_load_dd_acquire_read(addr) \
                                MK_AO_int_load_acquire_read(addr)
#   define MK_AO_HAVE_int_load_dd_acquire_read
# endif
#else
# if defined(MK_AO_HAVE_int_load)
#   define MK_AO_int_load_dd_acquire_read(addr) MK_AO_int_load(addr)
#   define MK_AO_HAVE_int_load_dd_acquire_read
# endif
#endif /* !MK_AO_NO_DD_ORDERING */

/* int_store */
#if defined(MK_AO_HAVE_int_store_full) && !defined(MK_AO_HAVE_int_store_release)
# define MK_AO_int_store_release(addr, val) MK_AO_int_store_full(addr, val)
# define MK_AO_HAVE_int_store_release
#endif

#if defined(MK_AO_HAVE_int_store_release) && !defined(MK_AO_HAVE_int_store)
# define MK_AO_int_store(addr, val) MK_AO_int_store_release(addr, val)
# define MK_AO_HAVE_int_store
#endif

#if defined(MK_AO_HAVE_int_store_full) && !defined(MK_AO_HAVE_int_store_write)
# define MK_AO_int_store_write(addr, val) MK_AO_int_store_full(addr, val)
# define MK_AO_HAVE_int_store_write
#endif

#if defined(MK_AO_HAVE_int_store_release) \
    && !defined(MK_AO_HAVE_int_store_release_write)
# define MK_AO_int_store_release_write(addr, val) \
                                MK_AO_int_store_release(addr, val)
# define MK_AO_HAVE_int_store_release_write
#endif

#if defined(MK_AO_HAVE_int_store_write) && !defined(MK_AO_HAVE_int_store)
# define MK_AO_int_store(addr, val) MK_AO_int_store_write(addr, val)
# define MK_AO_HAVE_int_store
#endif

#if defined(MK_AO_HAVE_int_store) && defined(MK_AO_HAVE_nop_full) \
    && !defined(MK_AO_HAVE_int_store_release)
# define MK_AO_int_store_release(addr, val) \
                                (MK_AO_nop_full(), MK_AO_int_store(addr, val))
# define MK_AO_HAVE_int_store_release
#endif

#if defined(MK_AO_HAVE_int_store) && defined(MK_AO_HAVE_nop_write) \
    && !defined(MK_AO_HAVE_int_store_write)
# define MK_AO_int_store_write(addr, val) \
                                (MK_AO_nop_write(), MK_AO_int_store(addr, val))
# define MK_AO_HAVE_int_store_write
#endif

#if defined(MK_AO_HAVE_int_compare_and_swap_write) \
    && !defined(MK_AO_HAVE_int_store_write)
  MK_AO_INLINE void
  MK_AO_int_store_write(volatile unsigned *addr, unsigned new_val)
  {
    unsigned old_val;

    do {
      old_val = *(unsigned *)addr;
    } while (MK_AO_EXPECT_FALSE(!MK_AO_int_compare_and_swap_write(addr, old_val,
                                                              new_val)));
  }
# define MK_AO_HAVE_int_store_write
#endif

#if defined(MK_AO_HAVE_int_store_write) \
    && !defined(MK_AO_HAVE_int_store_release_write)
# define MK_AO_int_store_release_write(addr, val) \
                                MK_AO_int_store_write(addr, val)
# define MK_AO_HAVE_int_store_release_write
#endif

#if defined(MK_AO_HAVE_int_store_release) && defined(MK_AO_HAVE_nop_full) \
    && !defined(MK_AO_HAVE_int_store_full)
# define MK_AO_int_store_full(addr, val) \
                                (MK_AO_int_store_release(addr, val), \
                                 MK_AO_nop_full())
# define MK_AO_HAVE_int_store_full
#endif

#if defined(MK_AO_HAVE_int_compare_and_swap) && !defined(MK_AO_HAVE_int_store)
  MK_AO_INLINE void
  MK_AO_int_store(volatile unsigned *addr, unsigned new_val)
  {
    unsigned old_val;

    do {
      old_val = *(unsigned *)addr;
    } while (MK_AO_EXPECT_FALSE(!MK_AO_int_compare_and_swap(addr,
                                                        old_val, new_val)));
  }
# define MK_AO_HAVE_int_store
#endif

#if defined(MK_AO_HAVE_int_compare_and_swap_release) \
    && !defined(MK_AO_HAVE_int_store_release)
  MK_AO_INLINE void
  MK_AO_int_store_release(volatile unsigned *addr, unsigned new_val)
  {
    unsigned old_val;

    do {
      old_val = *(unsigned *)addr;
    } while (MK_AO_EXPECT_FALSE(!MK_AO_int_compare_and_swap_release(addr, old_val,
                                                                new_val)));
  }
# define MK_AO_HAVE_int_store_release
#endif

#if defined(MK_AO_HAVE_int_compare_and_swap_full) \
    && !defined(MK_AO_HAVE_int_store_full)
  MK_AO_INLINE void
  MK_AO_int_store_full(volatile unsigned *addr, unsigned new_val)
  {
    unsigned old_val;

    do {
      old_val = *(unsigned *)addr;
    } while (MK_AO_EXPECT_FALSE(!MK_AO_int_compare_and_swap_full(addr, old_val,
                                                             new_val)));
  }
# define MK_AO_HAVE_int_store_full
#endif
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

/* fetch_compare_and_swap */
#if defined(MK_AO_HAVE_fetch_compare_and_swap) \
    && defined(MK_AO_HAVE_nop_full) \
    && !defined(MK_AO_HAVE_fetch_compare_and_swap_acquire)
  MK_AO_INLINE MK_AO_t
  MK_AO_fetch_compare_and_swap_acquire(volatile MK_AO_t *addr,
                                          MK_AO_t old_val, MK_AO_t new_val)
  {
    MK_AO_t result = MK_AO_fetch_compare_and_swap(addr, old_val, new_val);
    MK_AO_nop_full();
    return result;
  }
# define MK_AO_HAVE_fetch_compare_and_swap_acquire
#endif
#if defined(MK_AO_HAVE_fetch_compare_and_swap) \
    && defined(MK_AO_HAVE_nop_full) \
    && !defined(MK_AO_HAVE_fetch_compare_and_swap_release)
# define MK_AO_fetch_compare_and_swap_release(addr, old_val, new_val) \
                (MK_AO_nop_full(), \
                 MK_AO_fetch_compare_and_swap(addr, old_val, new_val))
# define MK_AO_HAVE_fetch_compare_and_swap_release
#endif
#if defined(MK_AO_HAVE_fetch_compare_and_swap_full)
# if !defined(MK_AO_HAVE_fetch_compare_and_swap_release)
#   define MK_AO_fetch_compare_and_swap_release(addr, old_val, new_val) \
                MK_AO_fetch_compare_and_swap_full(addr, old_val, new_val)
#   define MK_AO_HAVE_fetch_compare_and_swap_release
# endif
# if !defined(MK_AO_HAVE_fetch_compare_and_swap_acquire)
#   define MK_AO_fetch_compare_and_swap_acquire(addr, old_val, new_val) \
                MK_AO_fetch_compare_and_swap_full(addr, old_val, new_val)
#   define MK_AO_HAVE_fetch_compare_and_swap_acquire
# endif
# if !defined(MK_AO_HAVE_fetch_compare_and_swap_write)
#   define MK_AO_fetch_compare_and_swap_write(addr, old_val, new_val) \
                MK_AO_fetch_compare_and_swap_full(addr, old_val, new_val)
#   define MK_AO_HAVE_fetch_compare_and_swap_write
# endif
# if !defined(MK_AO_HAVE_fetch_compare_and_swap_read)
#   define MK_AO_fetch_compare_and_swap_read(addr, old_val, new_val) \
                MK_AO_fetch_compare_and_swap_full(addr, old_val, new_val)
#   define MK_AO_HAVE_fetch_compare_and_swap_read
# endif
#endif /* MK_AO_HAVE_fetch_compare_and_swap_full */

#if !defined(MK_AO_HAVE_fetch_compare_and_swap) \
    && defined(MK_AO_HAVE_fetch_compare_and_swap_release)
# define MK_AO_fetch_compare_and_swap(addr, old_val, new_val) \
            MK_AO_fetch_compare_and_swap_release(addr, old_val, new_val)
# define MK_AO_HAVE_fetch_compare_and_swap
#endif
#if !defined(MK_AO_HAVE_fetch_compare_and_swap) \
    && defined(MK_AO_HAVE_fetch_compare_and_swap_acquire)
# define MK_AO_fetch_compare_and_swap(addr, old_val, new_val) \
            MK_AO_fetch_compare_and_swap_acquire(addr, old_val, new_val)
# define MK_AO_HAVE_fetch_compare_and_swap
#endif
#if !defined(MK_AO_HAVE_fetch_compare_and_swap) \
    && defined(MK_AO_HAVE_fetch_compare_and_swap_write)
# define MK_AO_fetch_compare_and_swap(addr, old_val, new_val) \
                MK_AO_fetch_compare_and_swap_write(addr, old_val, new_val)
# define MK_AO_HAVE_fetch_compare_and_swap
#endif
#if !defined(MK_AO_HAVE_fetch_compare_and_swap) \
    && defined(MK_AO_HAVE_fetch_compare_and_swap_read)
# define MK_AO_fetch_compare_and_swap(addr, old_val, new_val) \
                MK_AO_fetch_compare_and_swap_read(addr, old_val, new_val)
# define MK_AO_HAVE_fetch_compare_and_swap
#endif

#if defined(MK_AO_HAVE_fetch_compare_and_swap_acquire) \
    && defined(MK_AO_HAVE_nop_full) \
    && !defined(MK_AO_HAVE_fetch_compare_and_swap_full)
# define MK_AO_fetch_compare_and_swap_full(addr, old_val, new_val) \
            (MK_AO_nop_full(), \
             MK_AO_fetch_compare_and_swap_acquire(addr, old_val, new_val))
# define MK_AO_HAVE_fetch_compare_and_swap_full
#endif

#if !defined(MK_AO_HAVE_fetch_compare_and_swap_release_write) \
    && defined(MK_AO_HAVE_fetch_compare_and_swap_write)
# define MK_AO_fetch_compare_and_swap_release_write(addr,old_val,new_val) \
                MK_AO_fetch_compare_and_swap_write(addr, old_val, new_val)
# define MK_AO_HAVE_fetch_compare_and_swap_release_write
#endif
#if !defined(MK_AO_HAVE_fetch_compare_and_swap_release_write) \
    && defined(MK_AO_HAVE_fetch_compare_and_swap_release)
# define MK_AO_fetch_compare_and_swap_release_write(addr,old_val,new_val) \
            MK_AO_fetch_compare_and_swap_release(addr, old_val, new_val)
# define MK_AO_HAVE_fetch_compare_and_swap_release_write
#endif
#if !defined(MK_AO_HAVE_fetch_compare_and_swap_acquire_read) \
    && defined(MK_AO_HAVE_fetch_compare_and_swap_read)
# define MK_AO_fetch_compare_and_swap_acquire_read(addr,old_val,new_val) \
                MK_AO_fetch_compare_and_swap_read(addr, old_val, new_val)
# define MK_AO_HAVE_fetch_compare_and_swap_acquire_read
#endif
#if !defined(MK_AO_HAVE_fetch_compare_and_swap_acquire_read) \
    && defined(MK_AO_HAVE_fetch_compare_and_swap_acquire)
# define MK_AO_fetch_compare_and_swap_acquire_read(addr,old_val,new_val) \
            MK_AO_fetch_compare_and_swap_acquire(addr, old_val, new_val)
# define MK_AO_HAVE_fetch_compare_and_swap_acquire_read
#endif

#ifdef MK_AO_NO_DD_ORDERING
# if defined(MK_AO_HAVE_fetch_compare_and_swap_acquire_read)
#   define MK_AO_fetch_compare_and_swap_dd_acquire_read(addr,old_val,new_val) \
        MK_AO_fetch_compare_and_swap_acquire_read(addr, old_val, new_val)
#   define MK_AO_HAVE_fetch_compare_and_swap_dd_acquire_read
# endif
#else
# if defined(MK_AO_HAVE_fetch_compare_and_swap)
#   define MK_AO_fetch_compare_and_swap_dd_acquire_read(addr,old_val,new_val) \
                MK_AO_fetch_compare_and_swap(addr, old_val, new_val)
#   define MK_AO_HAVE_fetch_compare_and_swap_dd_acquire_read
# endif
#endif /* !MK_AO_NO_DD_ORDERING */

/* compare_and_swap */
#if defined(MK_AO_HAVE_compare_and_swap) && defined(MK_AO_HAVE_nop_full) \
    && !defined(MK_AO_HAVE_compare_and_swap_acquire)
  MK_AO_INLINE int
  MK_AO_compare_and_swap_acquire(volatile MK_AO_t *addr, MK_AO_t old,
                                    MK_AO_t new_val)
  {
    int result = MK_AO_compare_and_swap(addr, old, new_val);
    MK_AO_nop_full();
    return result;
  }
# define MK_AO_HAVE_compare_and_swap_acquire
#endif
#if defined(MK_AO_HAVE_compare_and_swap) && defined(MK_AO_HAVE_nop_full) \
    && !defined(MK_AO_HAVE_compare_and_swap_release)
# define MK_AO_compare_and_swap_release(addr, old, new_val) \
                (MK_AO_nop_full(), MK_AO_compare_and_swap(addr, old, new_val))
# define MK_AO_HAVE_compare_and_swap_release
#endif
#if defined(MK_AO_HAVE_compare_and_swap_full)
# if !defined(MK_AO_HAVE_compare_and_swap_release)
#   define MK_AO_compare_and_swap_release(addr, old, new_val) \
                MK_AO_compare_and_swap_full(addr, old, new_val)
#   define MK_AO_HAVE_compare_and_swap_release
# endif
# if !defined(MK_AO_HAVE_compare_and_swap_acquire)
#   define MK_AO_compare_and_swap_acquire(addr, old, new_val) \
                MK_AO_compare_and_swap_full(addr, old, new_val)
#   define MK_AO_HAVE_compare_and_swap_acquire
# endif
# if !defined(MK_AO_HAVE_compare_and_swap_write)
#   define MK_AO_compare_and_swap_write(addr, old, new_val) \
                MK_AO_compare_and_swap_full(addr, old, new_val)
#   define MK_AO_HAVE_compare_and_swap_write
# endif
# if !defined(MK_AO_HAVE_compare_and_swap_read)
#   define MK_AO_compare_and_swap_read(addr, old, new_val) \
                MK_AO_compare_and_swap_full(addr, old, new_val)
#   define MK_AO_HAVE_compare_and_swap_read
# endif
#endif /* MK_AO_HAVE_compare_and_swap_full */

#if !defined(MK_AO_HAVE_compare_and_swap) \
    && defined(MK_AO_HAVE_compare_and_swap_release)
# define MK_AO_compare_and_swap(addr, old, new_val) \
                MK_AO_compare_and_swap_release(addr, old, new_val)
# define MK_AO_HAVE_compare_and_swap
#endif
#if !defined(MK_AO_HAVE_compare_and_swap) \
    && defined(MK_AO_HAVE_compare_and_swap_acquire)
# define MK_AO_compare_and_swap(addr, old, new_val) \
                MK_AO_compare_and_swap_acquire(addr, old, new_val)
# define MK_AO_HAVE_compare_and_swap
#endif
#if !defined(MK_AO_HAVE_compare_and_swap) \
    && defined(MK_AO_HAVE_compare_and_swap_write)
# define MK_AO_compare_and_swap(addr, old, new_val) \
                MK_AO_compare_and_swap_write(addr, old, new_val)
# define MK_AO_HAVE_compare_and_swap
#endif
#if !defined(MK_AO_HAVE_compare_and_swap) \
    && defined(MK_AO_HAVE_compare_and_swap_read)
# define MK_AO_compare_and_swap(addr, old, new_val) \
                MK_AO_compare_and_swap_read(addr, old, new_val)
# define MK_AO_HAVE_compare_and_swap
#endif

#if defined(MK_AO_HAVE_compare_and_swap_acquire) \
    && defined(MK_AO_HAVE_nop_full) \
    && !defined(MK_AO_HAVE_compare_and_swap_full)
# define MK_AO_compare_and_swap_full(addr, old, new_val) \
                (MK_AO_nop_full(), \
                 MK_AO_compare_and_swap_acquire(addr, old, new_val))
# define MK_AO_HAVE_compare_and_swap_full
#endif

#if !defined(MK_AO_HAVE_compare_and_swap_release_write) \
    && defined(MK_AO_HAVE_compare_and_swap_write)
# define MK_AO_compare_and_swap_release_write(addr, old, new_val) \
                MK_AO_compare_and_swap_write(addr, old, new_val)
# define MK_AO_HAVE_compare_and_swap_release_write
#endif
#if !defined(MK_AO_HAVE_compare_and_swap_release_write) \
    && defined(MK_AO_HAVE_compare_and_swap_release)
# define MK_AO_compare_and_swap_release_write(addr, old, new_val) \
                MK_AO_compare_and_swap_release(addr, old, new_val)
# define MK_AO_HAVE_compare_and_swap_release_write
#endif
#if !defined(MK_AO_HAVE_compare_and_swap_acquire_read) \
    && defined(MK_AO_HAVE_compare_and_swap_read)
# define MK_AO_compare_and_swap_acquire_read(addr, old, new_val) \
                MK_AO_compare_and_swap_read(addr, old, new_val)
# define MK_AO_HAVE_compare_and_swap_acquire_read
#endif
#if !defined(MK_AO_HAVE_compare_and_swap_acquire_read) \
    && defined(MK_AO_HAVE_compare_and_swap_acquire)
# define MK_AO_compare_and_swap_acquire_read(addr, old, new_val) \
                MK_AO_compare_and_swap_acquire(addr, old, new_val)
# define MK_AO_HAVE_compare_and_swap_acquire_read
#endif

#ifdef MK_AO_NO_DD_ORDERING
# if defined(MK_AO_HAVE_compare_and_swap_acquire_read)
#   define MK_AO_compare_and_swap_dd_acquire_read(addr, old, new_val) \
                MK_AO_compare_and_swap_acquire_read(addr, old, new_val)
#   define MK_AO_HAVE_compare_and_swap_dd_acquire_read
# endif
#else
# if defined(MK_AO_HAVE_compare_and_swap)
#   define MK_AO_compare_and_swap_dd_acquire_read(addr, old, new_val) \
                MK_AO_compare_and_swap(addr, old, new_val)
#   define MK_AO_HAVE_compare_and_swap_dd_acquire_read
# endif
#endif /* !MK_AO_NO_DD_ORDERING */

/* load */
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

#if !defined(MK_AO_HAVE_load_acquire_read) \
    && defined(MK_AO_HAVE_load_acquire)
# define MK_AO_load_acquire_read(addr) MK_AO_load_acquire(addr)
# define MK_AO_HAVE_load_acquire_read
#endif

#if defined(MK_AO_HAVE_load) && defined(MK_AO_HAVE_nop_full) \
    && !defined(MK_AO_HAVE_load_acquire)
  MK_AO_INLINE MK_AO_t
  MK_AO_load_acquire(const volatile MK_AO_t *addr)
  {
    MK_AO_t result = MK_AO_load(addr);

    /* Acquire barrier would be useless, since the load could be delayed    */
    /* beyond it.                                                           */
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

#if defined(MK_AO_HAVE_compare_and_swap_read) \
    && !defined(MK_AO_HAVE_load_read)
# define MK_AO_CAS_BASED_LOAD_READ
  MK_AO_INLINE MK_AO_t
  MK_AO_load_read(const volatile MK_AO_t *addr)
  {
    MK_AO_t result;

    do {
      result = *(const MK_AO_t *)addr;
    } while (MK_AO_EXPECT_FALSE(!MK_AO_compare_and_swap_read(
                                                (volatile MK_AO_t *)addr,
                                                result, result)));
    return result;
  }
# define MK_AO_HAVE_load_read
#endif

#if !defined(MK_AO_HAVE_load_acquire_read) \
    && defined(MK_AO_HAVE_load_read)
# define MK_AO_load_acquire_read(addr) MK_AO_load_read(addr)
# define MK_AO_HAVE_load_acquire_read
#endif

#if defined(MK_AO_HAVE_load_acquire_read) && !defined(MK_AO_HAVE_load) \
    && (!defined(MK_AO_CAS_BASED_LOAD_READ) \
        || !defined(MK_AO_HAVE_compare_and_swap))
# define MK_AO_load(addr) MK_AO_load_acquire_read(addr)
# define MK_AO_HAVE_load
#endif

#if defined(MK_AO_HAVE_compare_and_swap_full) \
    && !defined(MK_AO_HAVE_load_full)
  MK_AO_INLINE MK_AO_t
  MK_AO_load_full(const volatile MK_AO_t *addr)
  {
    MK_AO_t result;

    do {
      result = *(const MK_AO_t *)addr;
    } while (MK_AO_EXPECT_FALSE(!MK_AO_compare_and_swap_full(
                                                (volatile MK_AO_t *)addr,
                                                result, result)));
    return result;
  }
# define MK_AO_HAVE_load_full
#endif

#if defined(MK_AO_HAVE_compare_and_swap_acquire) \
    && !defined(MK_AO_HAVE_load_acquire)
  MK_AO_INLINE MK_AO_t
  MK_AO_load_acquire(const volatile MK_AO_t *addr)
  {
    MK_AO_t result;

    do {
      result = *(const MK_AO_t *)addr;
    } while (MK_AO_EXPECT_FALSE(!MK_AO_compare_and_swap_acquire(
                                                (volatile MK_AO_t *)addr,
                                                result, result)));
    return result;
  }
# define MK_AO_HAVE_load_acquire
#endif

#if defined(MK_AO_HAVE_compare_and_swap) && !defined(MK_AO_HAVE_load)
  MK_AO_INLINE MK_AO_t
  MK_AO_load(const volatile MK_AO_t *addr)
  {
    MK_AO_t result;

    do {
      result = *(const MK_AO_t *)addr;
    } while (MK_AO_EXPECT_FALSE(!MK_AO_compare_and_swap(
                                                (volatile MK_AO_t *)addr,
                                                result, result)));
    return result;
  }
# define MK_AO_HAVE_load
#endif

#ifdef MK_AO_NO_DD_ORDERING
# if defined(MK_AO_HAVE_load_acquire_read)
#   define MK_AO_load_dd_acquire_read(addr) \
                                MK_AO_load_acquire_read(addr)
#   define MK_AO_HAVE_load_dd_acquire_read
# endif
#else
# if defined(MK_AO_HAVE_load)
#   define MK_AO_load_dd_acquire_read(addr) MK_AO_load(addr)
#   define MK_AO_HAVE_load_dd_acquire_read
# endif
#endif /* !MK_AO_NO_DD_ORDERING */

/* store */
#if defined(MK_AO_HAVE_store_full) && !defined(MK_AO_HAVE_store_release)
# define MK_AO_store_release(addr, val) MK_AO_store_full(addr, val)
# define MK_AO_HAVE_store_release
#endif

#if defined(MK_AO_HAVE_store_release) && !defined(MK_AO_HAVE_store)
# define MK_AO_store(addr, val) MK_AO_store_release(addr, val)
# define MK_AO_HAVE_store
#endif

#if defined(MK_AO_HAVE_store_full) && !defined(MK_AO_HAVE_store_write)
# define MK_AO_store_write(addr, val) MK_AO_store_full(addr, val)
# define MK_AO_HAVE_store_write
#endif

#if defined(MK_AO_HAVE_store_release) \
    && !defined(MK_AO_HAVE_store_release_write)
# define MK_AO_store_release_write(addr, val) \
                                MK_AO_store_release(addr, val)
# define MK_AO_HAVE_store_release_write
#endif

#if defined(MK_AO_HAVE_store_write) && !defined(MK_AO_HAVE_store)
# define MK_AO_store(addr, val) MK_AO_store_write(addr, val)
# define MK_AO_HAVE_store
#endif

#if defined(MK_AO_HAVE_store) && defined(MK_AO_HAVE_nop_full) \
    && !defined(MK_AO_HAVE_store_release)
# define MK_AO_store_release(addr, val) \
                                (MK_AO_nop_full(), MK_AO_store(addr, val))
# define MK_AO_HAVE_store_release
#endif

#if defined(MK_AO_HAVE_store) && defined(MK_AO_HAVE_nop_write) \
    && !defined(MK_AO_HAVE_store_write)
# define MK_AO_store_write(addr, val) \
                                (MK_AO_nop_write(), MK_AO_store(addr, val))
# define MK_AO_HAVE_store_write
#endif

#if defined(MK_AO_HAVE_compare_and_swap_write) \
    && !defined(MK_AO_HAVE_store_write)
  MK_AO_INLINE void
  MK_AO_store_write(volatile MK_AO_t *addr, MK_AO_t new_val)
  {
    MK_AO_t old_val;

    do {
      old_val = *(MK_AO_t *)addr;
    } while (MK_AO_EXPECT_FALSE(!MK_AO_compare_and_swap_write(addr, old_val,
                                                              new_val)));
  }
# define MK_AO_HAVE_store_write
#endif

#if defined(MK_AO_HAVE_store_write) \
    && !defined(MK_AO_HAVE_store_release_write)
# define MK_AO_store_release_write(addr, val) \
                                MK_AO_store_write(addr, val)
# define MK_AO_HAVE_store_release_write
#endif

#if defined(MK_AO_HAVE_store_release) && defined(MK_AO_HAVE_nop_full) \
    && !defined(MK_AO_HAVE_store_full)
# define MK_AO_store_full(addr, val) \
                                (MK_AO_store_release(addr, val), \
                                 MK_AO_nop_full())
# define MK_AO_HAVE_store_full
#endif

#if defined(MK_AO_HAVE_compare_and_swap) && !defined(MK_AO_HAVE_store)
  MK_AO_INLINE void
  MK_AO_store(volatile MK_AO_t *addr, MK_AO_t new_val)
  {
    MK_AO_t old_val;

    do {
      old_val = *(MK_AO_t *)addr;
    } while (MK_AO_EXPECT_FALSE(!MK_AO_compare_and_swap(addr,
                                                        old_val, new_val)));
  }
# define MK_AO_HAVE_store
#endif

#if defined(MK_AO_HAVE_compare_and_swap_release) \
    && !defined(MK_AO_HAVE_store_release)
  MK_AO_INLINE void
  MK_AO_store_release(volatile MK_AO_t *addr, MK_AO_t new_val)
  {
    MK_AO_t old_val;

    do {
      old_val = *(MK_AO_t *)addr;
    } while (MK_AO_EXPECT_FALSE(!MK_AO_compare_and_swap_release(addr, old_val,
                                                                new_val)));
  }
# define MK_AO_HAVE_store_release
#endif

#if defined(MK_AO_HAVE_compare_and_swap_full) \
    && !defined(MK_AO_HAVE_store_full)
  MK_AO_INLINE void
  MK_AO_store_full(volatile MK_AO_t *addr, MK_AO_t new_val)
  {
    MK_AO_t old_val;

    do {
      old_val = *(MK_AO_t *)addr;
    } while (MK_AO_EXPECT_FALSE(!MK_AO_compare_and_swap_full(addr, old_val,
                                                             new_val)));
  }
# define MK_AO_HAVE_store_full
#endif
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

/* double_fetch_compare_and_swap */
#if defined(MK_AO_HAVE_double_fetch_compare_and_swap) \
    && defined(MK_AO_HAVE_nop_full) \
    && !defined(MK_AO_HAVE_double_fetch_compare_and_swap_acquire)
  MK_AO_INLINE MK_AO_double_t
  MK_AO_double_fetch_compare_and_swap_acquire(volatile MK_AO_double_t *addr,
                                          MK_AO_double_t old_val, MK_AO_double_t new_val)
  {
    MK_AO_double_t result = MK_AO_double_fetch_compare_and_swap(addr, old_val, new_val);
    MK_AO_nop_full();
    return result;
  }
# define MK_AO_HAVE_double_fetch_compare_and_swap_acquire
#endif
#if defined(MK_AO_HAVE_double_fetch_compare_and_swap) \
    && defined(MK_AO_HAVE_nop_full) \
    && !defined(MK_AO_HAVE_double_fetch_compare_and_swap_release)
# define MK_AO_double_fetch_compare_and_swap_release(addr, old_val, new_val) \
                (MK_AO_nop_full(), \
                 MK_AO_double_fetch_compare_and_swap(addr, old_val, new_val))
# define MK_AO_HAVE_double_fetch_compare_and_swap_release
#endif
#if defined(MK_AO_HAVE_double_fetch_compare_and_swap_full)
# if !defined(MK_AO_HAVE_double_fetch_compare_and_swap_release)
#   define MK_AO_double_fetch_compare_and_swap_release(addr, old_val, new_val) \
                MK_AO_double_fetch_compare_and_swap_full(addr, old_val, new_val)
#   define MK_AO_HAVE_double_fetch_compare_and_swap_release
# endif
# if !defined(MK_AO_HAVE_double_fetch_compare_and_swap_acquire)
#   define MK_AO_double_fetch_compare_and_swap_acquire(addr, old_val, new_val) \
                MK_AO_double_fetch_compare_and_swap_full(addr, old_val, new_val)
#   define MK_AO_HAVE_double_fetch_compare_and_swap_acquire
# endif
# if !defined(MK_AO_HAVE_double_fetch_compare_and_swap_write)
#   define MK_AO_double_fetch_compare_and_swap_write(addr, old_val, new_val) \
                MK_AO_double_fetch_compare_and_swap_full(addr, old_val, new_val)
#   define MK_AO_HAVE_double_fetch_compare_and_swap_write
# endif
# if !defined(MK_AO_HAVE_double_fetch_compare_and_swap_read)
#   define MK_AO_double_fetch_compare_and_swap_read(addr, old_val, new_val) \
                MK_AO_double_fetch_compare_and_swap_full(addr, old_val, new_val)
#   define MK_AO_HAVE_double_fetch_compare_and_swap_read
# endif
#endif /* MK_AO_HAVE_double_fetch_compare_and_swap_full */

#if !defined(MK_AO_HAVE_double_fetch_compare_and_swap) \
    && defined(MK_AO_HAVE_double_fetch_compare_and_swap_release)
# define MK_AO_double_fetch_compare_and_swap(addr, old_val, new_val) \
            MK_AO_double_fetch_compare_and_swap_release(addr, old_val, new_val)
# define MK_AO_HAVE_double_fetch_compare_and_swap
#endif
#if !defined(MK_AO_HAVE_double_fetch_compare_and_swap) \
    && defined(MK_AO_HAVE_double_fetch_compare_and_swap_acquire)
# define MK_AO_double_fetch_compare_and_swap(addr, old_val, new_val) \
            MK_AO_double_fetch_compare_and_swap_acquire(addr, old_val, new_val)
# define MK_AO_HAVE_double_fetch_compare_and_swap
#endif
#if !defined(MK_AO_HAVE_double_fetch_compare_and_swap) \
    && defined(MK_AO_HAVE_double_fetch_compare_and_swap_write)
# define MK_AO_double_fetch_compare_and_swap(addr, old_val, new_val) \
                MK_AO_double_fetch_compare_and_swap_write(addr, old_val, new_val)
# define MK_AO_HAVE_double_fetch_compare_and_swap
#endif
#if !defined(MK_AO_HAVE_double_fetch_compare_and_swap) \
    && defined(MK_AO_HAVE_double_fetch_compare_and_swap_read)
# define MK_AO_double_fetch_compare_and_swap(addr, old_val, new_val) \
                MK_AO_double_fetch_compare_and_swap_read(addr, old_val, new_val)
# define MK_AO_HAVE_double_fetch_compare_and_swap
#endif

#if defined(MK_AO_HAVE_double_fetch_compare_and_swap_acquire) \
    && defined(MK_AO_HAVE_nop_full) \
    && !defined(MK_AO_HAVE_double_fetch_compare_and_swap_full)
# define MK_AO_double_fetch_compare_and_swap_full(addr, old_val, new_val) \
            (MK_AO_nop_full(), \
             MK_AO_double_fetch_compare_and_swap_acquire(addr, old_val, new_val))
# define MK_AO_HAVE_double_fetch_compare_and_swap_full
#endif

#if !defined(MK_AO_HAVE_double_fetch_compare_and_swap_release_write) \
    && defined(MK_AO_HAVE_double_fetch_compare_and_swap_write)
# define MK_AO_double_fetch_compare_and_swap_release_write(addr,old_val,new_val) \
                MK_AO_double_fetch_compare_and_swap_write(addr, old_val, new_val)
# define MK_AO_HAVE_double_fetch_compare_and_swap_release_write
#endif
#if !defined(MK_AO_HAVE_double_fetch_compare_and_swap_release_write) \
    && defined(MK_AO_HAVE_double_fetch_compare_and_swap_release)
# define MK_AO_double_fetch_compare_and_swap_release_write(addr,old_val,new_val) \
            MK_AO_double_fetch_compare_and_swap_release(addr, old_val, new_val)
# define MK_AO_HAVE_double_fetch_compare_and_swap_release_write
#endif
#if !defined(MK_AO_HAVE_double_fetch_compare_and_swap_acquire_read) \
    && defined(MK_AO_HAVE_double_fetch_compare_and_swap_read)
# define MK_AO_double_fetch_compare_and_swap_acquire_read(addr,old_val,new_val) \
                MK_AO_double_fetch_compare_and_swap_read(addr, old_val, new_val)
# define MK_AO_HAVE_double_fetch_compare_and_swap_acquire_read
#endif
#if !defined(MK_AO_HAVE_double_fetch_compare_and_swap_acquire_read) \
    && defined(MK_AO_HAVE_double_fetch_compare_and_swap_acquire)
# define MK_AO_double_fetch_compare_and_swap_acquire_read(addr,old_val,new_val) \
            MK_AO_double_fetch_compare_and_swap_acquire(addr, old_val, new_val)
# define MK_AO_HAVE_double_fetch_compare_and_swap_acquire_read
#endif

#ifdef MK_AO_NO_DD_ORDERING
# if defined(MK_AO_HAVE_double_fetch_compare_and_swap_acquire_read)
#   define MK_AO_double_fetch_compare_and_swap_dd_acquire_read(addr,old_val,new_val) \
        MK_AO_double_fetch_compare_and_swap_acquire_read(addr, old_val, new_val)
#   define MK_AO_HAVE_double_fetch_compare_and_swap_dd_acquire_read
# endif
#else
# if defined(MK_AO_HAVE_double_fetch_compare_and_swap)
#   define MK_AO_double_fetch_compare_and_swap_dd_acquire_read(addr,old_val,new_val) \
                MK_AO_double_fetch_compare_and_swap(addr, old_val, new_val)
#   define MK_AO_HAVE_double_fetch_compare_and_swap_dd_acquire_read
# endif
#endif /* !MK_AO_NO_DD_ORDERING */

/* double_compare_and_swap */
#if defined(MK_AO_HAVE_double_compare_and_swap) && defined(MK_AO_HAVE_nop_full) \
    && !defined(MK_AO_HAVE_double_compare_and_swap_acquire)
  MK_AO_INLINE int
  MK_AO_double_compare_and_swap_acquire(volatile MK_AO_double_t *addr, MK_AO_double_t old,
                                    MK_AO_double_t new_val)
  {
    int result = MK_AO_double_compare_and_swap(addr, old, new_val);
    MK_AO_nop_full();
    return result;
  }
# define MK_AO_HAVE_double_compare_and_swap_acquire
#endif
#if defined(MK_AO_HAVE_double_compare_and_swap) && defined(MK_AO_HAVE_nop_full) \
    && !defined(MK_AO_HAVE_double_compare_and_swap_release)
# define MK_AO_double_compare_and_swap_release(addr, old, new_val) \
                (MK_AO_nop_full(), MK_AO_double_compare_and_swap(addr, old, new_val))
# define MK_AO_HAVE_double_compare_and_swap_release
#endif
#if defined(MK_AO_HAVE_double_compare_and_swap_full)
# if !defined(MK_AO_HAVE_double_compare_and_swap_release)
#   define MK_AO_double_compare_and_swap_release(addr, old, new_val) \
                MK_AO_double_compare_and_swap_full(addr, old, new_val)
#   define MK_AO_HAVE_double_compare_and_swap_release
# endif
# if !defined(MK_AO_HAVE_double_compare_and_swap_acquire)
#   define MK_AO_double_compare_and_swap_acquire(addr, old, new_val) \
                MK_AO_double_compare_and_swap_full(addr, old, new_val)
#   define MK_AO_HAVE_double_compare_and_swap_acquire
# endif
# if !defined(MK_AO_HAVE_double_compare_and_swap_write)
#   define MK_AO_double_compare_and_swap_write(addr, old, new_val) \
                MK_AO_double_compare_and_swap_full(addr, old, new_val)
#   define MK_AO_HAVE_double_compare_and_swap_write
# endif
# if !defined(MK_AO_HAVE_double_compare_and_swap_read)
#   define MK_AO_double_compare_and_swap_read(addr, old, new_val) \
                MK_AO_double_compare_and_swap_full(addr, old, new_val)
#   define MK_AO_HAVE_double_compare_and_swap_read
# endif
#endif /* MK_AO_HAVE_double_compare_and_swap_full */

#if !defined(MK_AO_HAVE_double_compare_and_swap) \
    && defined(MK_AO_HAVE_double_compare_and_swap_release)
# define MK_AO_double_compare_and_swap(addr, old, new_val) \
                MK_AO_double_compare_and_swap_release(addr, old, new_val)
# define MK_AO_HAVE_double_compare_and_swap
#endif
#if !defined(MK_AO_HAVE_double_compare_and_swap) \
    && defined(MK_AO_HAVE_double_compare_and_swap_acquire)
# define MK_AO_double_compare_and_swap(addr, old, new_val) \
                MK_AO_double_compare_and_swap_acquire(addr, old, new_val)
# define MK_AO_HAVE_double_compare_and_swap
#endif
#if !defined(MK_AO_HAVE_double_compare_and_swap) \
    && defined(MK_AO_HAVE_double_compare_and_swap_write)
# define MK_AO_double_compare_and_swap(addr, old, new_val) \
                MK_AO_double_compare_and_swap_write(addr, old, new_val)
# define MK_AO_HAVE_double_compare_and_swap
#endif
#if !defined(MK_AO_HAVE_double_compare_and_swap) \
    && defined(MK_AO_HAVE_double_compare_and_swap_read)
# define MK_AO_double_compare_and_swap(addr, old, new_val) \
                MK_AO_double_compare_and_swap_read(addr, old, new_val)
# define MK_AO_HAVE_double_compare_and_swap
#endif

#if defined(MK_AO_HAVE_double_compare_and_swap_acquire) \
    && defined(MK_AO_HAVE_nop_full) \
    && !defined(MK_AO_HAVE_double_compare_and_swap_full)
# define MK_AO_double_compare_and_swap_full(addr, old, new_val) \
                (MK_AO_nop_full(), \
                 MK_AO_double_compare_and_swap_acquire(addr, old, new_val))
# define MK_AO_HAVE_double_compare_and_swap_full
#endif

#if !defined(MK_AO_HAVE_double_compare_and_swap_release_write) \
    && defined(MK_AO_HAVE_double_compare_and_swap_write)
# define MK_AO_double_compare_and_swap_release_write(addr, old, new_val) \
                MK_AO_double_compare_and_swap_write(addr, old, new_val)
# define MK_AO_HAVE_double_compare_and_swap_release_write
#endif
#if !defined(MK_AO_HAVE_double_compare_and_swap_release_write) \
    && defined(MK_AO_HAVE_double_compare_and_swap_release)
# define MK_AO_double_compare_and_swap_release_write(addr, old, new_val) \
                MK_AO_double_compare_and_swap_release(addr, old, new_val)
# define MK_AO_HAVE_double_compare_and_swap_release_write
#endif
#if !defined(MK_AO_HAVE_double_compare_and_swap_acquire_read) \
    && defined(MK_AO_HAVE_double_compare_and_swap_read)
# define MK_AO_double_compare_and_swap_acquire_read(addr, old, new_val) \
                MK_AO_double_compare_and_swap_read(addr, old, new_val)
# define MK_AO_HAVE_double_compare_and_swap_acquire_read
#endif
#if !defined(MK_AO_HAVE_double_compare_and_swap_acquire_read) \
    && defined(MK_AO_HAVE_double_compare_and_swap_acquire)
# define MK_AO_double_compare_and_swap_acquire_read(addr, old, new_val) \
                MK_AO_double_compare_and_swap_acquire(addr, old, new_val)
# define MK_AO_HAVE_double_compare_and_swap_acquire_read
#endif

#ifdef MK_AO_NO_DD_ORDERING
# if defined(MK_AO_HAVE_double_compare_and_swap_acquire_read)
#   define MK_AO_double_compare_and_swap_dd_acquire_read(addr, old, new_val) \
                MK_AO_double_compare_and_swap_acquire_read(addr, old, new_val)
#   define MK_AO_HAVE_double_compare_and_swap_dd_acquire_read
# endif
#else
# if defined(MK_AO_HAVE_double_compare_and_swap)
#   define MK_AO_double_compare_and_swap_dd_acquire_read(addr, old, new_val) \
                MK_AO_double_compare_and_swap(addr, old, new_val)
#   define MK_AO_HAVE_double_compare_and_swap_dd_acquire_read
# endif
#endif /* !MK_AO_NO_DD_ORDERING */

/* double_load */
#if defined(MK_AO_HAVE_double_load_full) && !defined(MK_AO_HAVE_double_load_acquire)
# define MK_AO_double_load_acquire(addr) MK_AO_double_load_full(addr)
# define MK_AO_HAVE_double_load_acquire
#endif

#if defined(MK_AO_HAVE_double_load_acquire) && !defined(MK_AO_HAVE_double_load)
# define MK_AO_double_load(addr) MK_AO_double_load_acquire(addr)
# define MK_AO_HAVE_double_load
#endif

#if defined(MK_AO_HAVE_double_load_full) && !defined(MK_AO_HAVE_double_load_read)
# define MK_AO_double_load_read(addr) MK_AO_double_load_full(addr)
# define MK_AO_HAVE_double_load_read
#endif

#if !defined(MK_AO_HAVE_double_load_acquire_read) \
    && defined(MK_AO_HAVE_double_load_acquire)
# define MK_AO_double_load_acquire_read(addr) MK_AO_double_load_acquire(addr)
# define MK_AO_HAVE_double_load_acquire_read
#endif

#if defined(MK_AO_HAVE_double_load) && defined(MK_AO_HAVE_nop_full) \
    && !defined(MK_AO_HAVE_double_load_acquire)
  MK_AO_INLINE MK_AO_double_t
  MK_AO_double_load_acquire(const volatile MK_AO_double_t *addr)
  {
    MK_AO_double_t result = MK_AO_double_load(addr);

    /* Acquire barrier would be useless, since the load could be delayed    */
    /* beyond it.                                                           */
    MK_AO_nop_full();
    return result;
  }
# define MK_AO_HAVE_double_load_acquire
#endif

#if defined(MK_AO_HAVE_double_load) && defined(MK_AO_HAVE_nop_read) \
    && !defined(MK_AO_HAVE_double_load_read)
  MK_AO_INLINE MK_AO_double_t
  MK_AO_double_load_read(const volatile MK_AO_double_t *addr)
  {
    MK_AO_double_t result = MK_AO_double_load(addr);

    MK_AO_nop_read();
    return result;
  }
# define MK_AO_HAVE_double_load_read
#endif

#if defined(MK_AO_HAVE_double_load_acquire) && defined(MK_AO_HAVE_nop_full) \
    && !defined(MK_AO_HAVE_double_load_full)
# define MK_AO_double_load_full(addr) (MK_AO_nop_full(), MK_AO_double_load_acquire(addr))
# define MK_AO_HAVE_double_load_full
#endif

#if defined(MK_AO_HAVE_double_compare_and_swap_read) \
    && !defined(MK_AO_HAVE_double_load_read)
# define MK_AO_double_CAS_BASED_LOAD_READ
  MK_AO_INLINE MK_AO_double_t
  MK_AO_double_load_read(const volatile MK_AO_double_t *addr)
  {
    MK_AO_double_t result;

    do {
      result = *(const MK_AO_double_t *)addr;
    } while (MK_AO_EXPECT_FALSE(!MK_AO_double_compare_and_swap_read(
                                                (volatile MK_AO_double_t *)addr,
                                                result, result)));
    return result;
  }
# define MK_AO_HAVE_double_load_read
#endif

#if !defined(MK_AO_HAVE_double_load_acquire_read) \
    && defined(MK_AO_HAVE_double_load_read)
# define MK_AO_double_load_acquire_read(addr) MK_AO_double_load_read(addr)
# define MK_AO_HAVE_double_load_acquire_read
#endif

#if defined(MK_AO_HAVE_double_load_acquire_read) && !defined(MK_AO_HAVE_double_load) \
    && (!defined(MK_AO_double_CAS_BASED_LOAD_READ) \
        || !defined(MK_AO_HAVE_double_compare_and_swap))
# define MK_AO_double_load(addr) MK_AO_double_load_acquire_read(addr)
# define MK_AO_HAVE_double_load
#endif

#if defined(MK_AO_HAVE_double_compare_and_swap_full) \
    && !defined(MK_AO_HAVE_double_load_full)
  MK_AO_INLINE MK_AO_double_t
  MK_AO_double_load_full(const volatile MK_AO_double_t *addr)
  {
    MK_AO_double_t result;

    do {
      result = *(const MK_AO_double_t *)addr;
    } while (MK_AO_EXPECT_FALSE(!MK_AO_double_compare_and_swap_full(
                                                (volatile MK_AO_double_t *)addr,
                                                result, result)));
    return result;
  }
# define MK_AO_HAVE_double_load_full
#endif

#if defined(MK_AO_HAVE_double_compare_and_swap_acquire) \
    && !defined(MK_AO_HAVE_double_load_acquire)
  MK_AO_INLINE MK_AO_double_t
  MK_AO_double_load_acquire(const volatile MK_AO_double_t *addr)
  {
    MK_AO_double_t result;

    do {
      result = *(const MK_AO_double_t *)addr;
    } while (MK_AO_EXPECT_FALSE(!MK_AO_double_compare_and_swap_acquire(
                                                (volatile MK_AO_double_t *)addr,
                                                result, result)));
    return result;
  }
# define MK_AO_HAVE_double_load_acquire
#endif

#if defined(MK_AO_HAVE_double_compare_and_swap) && !defined(MK_AO_HAVE_double_load)
  MK_AO_INLINE MK_AO_double_t
  MK_AO_double_load(const volatile MK_AO_double_t *addr)
  {
    MK_AO_double_t result;

    do {
      result = *(const MK_AO_double_t *)addr;
    } while (MK_AO_EXPECT_FALSE(!MK_AO_double_compare_and_swap(
                                                (volatile MK_AO_double_t *)addr,
                                                result, result)));
    return result;
  }
# define MK_AO_HAVE_double_load
#endif

#ifdef MK_AO_NO_DD_ORDERING
# if defined(MK_AO_HAVE_double_load_acquire_read)
#   define MK_AO_double_load_dd_acquire_read(addr) \
                                MK_AO_double_load_acquire_read(addr)
#   define MK_AO_HAVE_double_load_dd_acquire_read
# endif
#else
# if defined(MK_AO_HAVE_double_load)
#   define MK_AO_double_load_dd_acquire_read(addr) MK_AO_double_load(addr)
#   define MK_AO_HAVE_double_load_dd_acquire_read
# endif
#endif /* !MK_AO_NO_DD_ORDERING */

/* double_store */
#if defined(MK_AO_HAVE_double_store_full) && !defined(MK_AO_HAVE_double_store_release)
# define MK_AO_double_store_release(addr, val) MK_AO_double_store_full(addr, val)
# define MK_AO_HAVE_double_store_release
#endif

#if defined(MK_AO_HAVE_double_store_release) && !defined(MK_AO_HAVE_double_store)
# define MK_AO_double_store(addr, val) MK_AO_double_store_release(addr, val)
# define MK_AO_HAVE_double_store
#endif

#if defined(MK_AO_HAVE_double_store_full) && !defined(MK_AO_HAVE_double_store_write)
# define MK_AO_double_store_write(addr, val) MK_AO_double_store_full(addr, val)
# define MK_AO_HAVE_double_store_write
#endif

#if defined(MK_AO_HAVE_double_store_release) \
    && !defined(MK_AO_HAVE_double_store_release_write)
# define MK_AO_double_store_release_write(addr, val) \
                                MK_AO_double_store_release(addr, val)
# define MK_AO_HAVE_double_store_release_write
#endif

#if defined(MK_AO_HAVE_double_store_write) && !defined(MK_AO_HAVE_double_store)
# define MK_AO_double_store(addr, val) MK_AO_double_store_write(addr, val)
# define MK_AO_HAVE_double_store
#endif

#if defined(MK_AO_HAVE_double_store) && defined(MK_AO_HAVE_nop_full) \
    && !defined(MK_AO_HAVE_double_store_release)
# define MK_AO_double_store_release(addr, val) \
                                (MK_AO_nop_full(), MK_AO_double_store(addr, val))
# define MK_AO_HAVE_double_store_release
#endif

#if defined(MK_AO_HAVE_double_store) && defined(MK_AO_HAVE_nop_write) \
    && !defined(MK_AO_HAVE_double_store_write)
# define MK_AO_double_store_write(addr, val) \
                                (MK_AO_nop_write(), MK_AO_double_store(addr, val))
# define MK_AO_HAVE_double_store_write
#endif

#if defined(MK_AO_HAVE_double_compare_and_swap_write) \
    && !defined(MK_AO_HAVE_double_store_write)
  MK_AO_INLINE void
  MK_AO_double_store_write(volatile MK_AO_double_t *addr, MK_AO_double_t new_val)
  {
    MK_AO_double_t old_val;

    do {
      old_val = *(MK_AO_double_t *)addr;
    } while (MK_AO_EXPECT_FALSE(!MK_AO_double_compare_and_swap_write(addr, old_val,
                                                              new_val)));
  }
# define MK_AO_HAVE_double_store_write
#endif

#if defined(MK_AO_HAVE_double_store_write) \
    && !defined(MK_AO_HAVE_double_store_release_write)
# define MK_AO_double_store_release_write(addr, val) \
                                MK_AO_double_store_write(addr, val)
# define MK_AO_HAVE_double_store_release_write
#endif

#if defined(MK_AO_HAVE_double_store_release) && defined(MK_AO_HAVE_nop_full) \
    && !defined(MK_AO_HAVE_double_store_full)
# define MK_AO_double_store_full(addr, val) \
                                (MK_AO_double_store_release(addr, val), \
                                 MK_AO_nop_full())
# define MK_AO_HAVE_double_store_full
#endif

#if defined(MK_AO_HAVE_double_compare_and_swap) && !defined(MK_AO_HAVE_double_store)
  MK_AO_INLINE void
  MK_AO_double_store(volatile MK_AO_double_t *addr, MK_AO_double_t new_val)
  {
    MK_AO_double_t old_val;

    do {
      old_val = *(MK_AO_double_t *)addr;
    } while (MK_AO_EXPECT_FALSE(!MK_AO_double_compare_and_swap(addr,
                                                        old_val, new_val)));
  }
# define MK_AO_HAVE_double_store
#endif

#if defined(MK_AO_HAVE_double_compare_and_swap_release) \
    && !defined(MK_AO_HAVE_double_store_release)
  MK_AO_INLINE void
  MK_AO_double_store_release(volatile MK_AO_double_t *addr, MK_AO_double_t new_val)
  {
    MK_AO_double_t old_val;

    do {
      old_val = *(MK_AO_double_t *)addr;
    } while (MK_AO_EXPECT_FALSE(!MK_AO_double_compare_and_swap_release(addr, old_val,
                                                                new_val)));
  }
# define MK_AO_HAVE_double_store_release
#endif

#if defined(MK_AO_HAVE_double_compare_and_swap_full) \
    && !defined(MK_AO_HAVE_double_store_full)
  MK_AO_INLINE void
  MK_AO_double_store_full(volatile MK_AO_double_t *addr, MK_AO_double_t new_val)
  {
    MK_AO_double_t old_val;

    do {
      old_val = *(MK_AO_double_t *)addr;
    } while (MK_AO_EXPECT_FALSE(!MK_AO_double_compare_and_swap_full(addr, old_val,
                                                             new_val)));
  }
# define MK_AO_HAVE_double_store_full
#endif
