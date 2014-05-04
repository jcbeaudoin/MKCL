/*
 * Copyright (c) 2003 by Hewlett-Packard Company.  All rights reserved.
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
 * This file specifies Itanimum primitives for use with the Intel (ecc)
 * compiler.  We use intrinsics instead of the inline assembly code in the
 * gcc file.
 */

#include "../all_atomic_load_store.h"

#include "../test_and_set_t_is_char.h"

#include <ia64intrin.h>

/* The acquire release semantics of volatile can be turned off.  And volatile   */
/* operations in icc9 don't imply ordering with respect to other nonvolatile    */
/* operations.                                                                  */

#define MK_AO_INTEL_PTR_t void *

MK_AO_INLINE MK_AO_t
MK_AO_load_acquire(const volatile MK_AO_t *p)
{
  return (MK_AO_t)(__ld8_acq((MK_AO_INTEL_PTR_t)p));
}
#define MK_AO_HAVE_load_acquire

MK_AO_INLINE void
MK_AO_store_release(volatile MK_AO_t *p, MK_AO_t val)
{
  __st8_rel((MK_AO_INTEL_PTR_t)p, (__int64)val);
}
#define MK_AO_HAVE_store_release

MK_AO_INLINE unsigned char
MK_AO_char_load_acquire(const volatile unsigned char *p)
{
  /* A normal volatile load generates an ld.acq         */
  return (__ld1_acq((MK_AO_INTEL_PTR_t)p));
}
#define MK_AO_HAVE_char_load_acquire

MK_AO_INLINE void
MK_AO_char_store_release(volatile unsigned char *p, unsigned char val)
{
  __st1_rel((MK_AO_INTEL_PTR_t)p, val);
}
#define MK_AO_HAVE_char_store_release

MK_AO_INLINE unsigned short
MK_AO_short_load_acquire(const volatile unsigned short *p)
{
  /* A normal volatile load generates an ld.acq         */
  return (__ld2_acq((MK_AO_INTEL_PTR_t)p));
}
#define MK_AO_HAVE_short_load_acquire

MK_AO_INLINE void
MK_AO_short_store_release(volatile unsigned short *p, unsigned short val)
{
  __st2_rel((MK_AO_INTEL_PTR_t)p, val);
}
#define MK_AO_HAVE_short_store_release

MK_AO_INLINE unsigned int
MK_AO_int_load_acquire(const volatile unsigned int *p)
{
  /* A normal volatile load generates an ld.acq         */
  return (__ld4_acq((MK_AO_INTEL_PTR_t)p));
}
#define MK_AO_HAVE_int_load_acquire

MK_AO_INLINE void
MK_AO_int_store_release(volatile unsigned int *p, unsigned int val)
{
  __st4_rel((MK_AO_INTEL_PTR_t)p, val);
}
#define MK_AO_HAVE_int_store_release

MK_AO_INLINE void
MK_AO_nop_full(void)
{
  __mf();
}
#define MK_AO_HAVE_nop_full

MK_AO_INLINE MK_AO_t
MK_AO_fetch_and_add1_acquire (volatile MK_AO_t *p)
{
  return __fetchadd8_acq((unsigned __int64 *)p, 1);
}
#define MK_AO_HAVE_fetch_and_add1_acquire

MK_AO_INLINE MK_AO_t
MK_AO_fetch_and_add1_release (volatile MK_AO_t *p)
{
  return __fetchadd8_rel((unsigned __int64 *)p, 1);
}

#define MK_AO_HAVE_fetch_and_add1_release

MK_AO_INLINE MK_AO_t
MK_AO_fetch_and_sub1_acquire (volatile MK_AO_t *p)
{
  return __fetchadd8_acq((unsigned __int64 *)p, -1);
}

#define MK_AO_HAVE_fetch_and_sub1_acquire

MK_AO_INLINE MK_AO_t
MK_AO_fetch_and_sub1_release (volatile MK_AO_t *p)
{
  return __fetchadd8_rel((unsigned __int64 *)p, -1);
}

#define MK_AO_HAVE_fetch_and_sub1_release

MK_AO_INLINE int
MK_AO_compare_and_swap_acquire(volatile MK_AO_t *addr,
                             MK_AO_t old, MK_AO_t new_val)
{
  MK_AO_t oldval;
  oldval = _InterlockedCompareExchange64_acq(addr, new_val, old);
  return (oldval == old);
}

#define MK_AO_HAVE_compare_and_swap_acquire

MK_AO_INLINE int
MK_AO_compare_and_swap_release(volatile MK_AO_t *addr,
                             MK_AO_t old, MK_AO_t new_val)
{
  MK_AO_t oldval;
  oldval = _InterlockedCompareExchange64_rel(addr, new_val, old);
  return (oldval == old);
}

#define MK_AO_HAVE_compare_and_swap_release

MK_AO_INLINE int
MK_AO_char_compare_and_swap_acquire(volatile unsigned char *addr,
                                 unsigned char old, unsigned char new_val)
{
  unsigned char oldval;
  oldval = _InterlockedCompareExchange8_acq(addr, new_val, old);
  return (oldval == old);
}

#define MK_AO_HAVE_char_compare_and_swap_acquire

MK_AO_INLINE int
MK_AO_char_compare_and_swap_release(volatile unsigned char *addr,
                            unsigned char old, unsigned char new_val)
{
  unsigned char oldval;
  oldval = _InterlockedCompareExchange8_rel(addr, new_val, old);
  return (oldval == old);
}

#define MK_AO_HAVE_char_compare_and_swap_release

MK_AO_INLINE int
MK_AO_short_compare_and_swap_acquire(volatile unsigned short *addr,
                                 unsigned short old, unsigned short new_val)
{
  unsigned short oldval;
  oldval = _InterlockedCompareExchange16_acq(addr, new_val, old);
  return (oldval == old);
}

#define MK_AO_HAVE_short_compare_and_swap_acquire

MK_AO_INLINE int
MK_AO_short_compare_and_swap_release(volatile unsigned short *addr,
                            unsigned short old, unsigned short new_val)
{
  unsigned short oldval;
  oldval = _InterlockedCompareExchange16_rel(addr, new_val, old);
  return (oldval == old);
}

#define MK_AO_HAVE_short_compare_and_swap_release

MK_AO_INLINE int
MK_AO_int_compare_and_swap_acquire(volatile unsigned int *addr,
                                 unsigned int old, unsigned int new_val)
{
  unsigned int oldval;
  oldval = _InterlockedCompareExchange_acq(addr, new_val, old);
  return (oldval == old);
}

#define MK_AO_HAVE_int_compare_and_swap_acquire

MK_AO_INLINE int
MK_AO_int_compare_and_swap_release(volatile unsigned int *addr,
                            unsigned int old, unsigned int new_val)
{
  unsigned int oldval;
  oldval = _InterlockedCompareExchange_rel(addr, new_val, old);
  return (oldval == old);
}

#define MK_AO_HAVE_int_compare_and_swap_release
