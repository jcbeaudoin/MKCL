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

#include "../all_atomic_load_store.h"

#include "../all_acquire_release_volatile.h"

#include "../test_and_set_t_is_char.h"

#ifdef _ILP32
  /* 32-bit HP/UX code. */
  /* This requires pointer "swizzling".  Pointers need to be expanded   */
  /* to 64 bits using the addp4 instruction before use.  This makes it  */
  /* hard to share code, but we try anyway.                             */
# define MK_AO_LEN "4"
  /* We assume that addr always appears in argument position 1 in asm   */
  /* code.  If it is clobbered due to swizzling, we also need it in     */
  /* second position.  Any later arguments are referenced symbolically, */
  /* so that we don't have to worry about their position.  This requires*/
  /* gcc 3.1, but you shouldn't be using anything older than that on    */
  /* IA64 anyway.                                                       */
  /* The MK_AO_MASK macro is a workaround for the fact that HP/UX gcc      */
  /* appears to otherwise store 64-bit pointers in ar.ccv, i.e. it      */
  /* doesn't appear to clear high bits in a pointer value we pass into  */
  /* assembly code, even if it is supposedly of type MK_AO_t.              */
# define MK_AO_IN_ADDR "1"(addr)
# define MK_AO_OUT_ADDR , "=r"(addr)
# define MK_AO_SWIZZLE "addp4 %1=0,%1;;\n"
# define MK_AO_MASK(ptr) __asm__ __volatile__("zxt4 %1=%1": "=r"(ptr) : "0"(ptr))
#else
# define MK_AO_LEN "8"
# define MK_AO_IN_ADDR "r"(addr)
# define MK_AO_OUT_ADDR
# define MK_AO_SWIZZLE
# define MK_AO_MASK(ptr) /* empty */
#endif /* !_ILP32 */

MK_AO_INLINE void
MK_AO_nop_full(void)
{
  __asm__ __volatile__("mf" : : : "memory");
}
#define MK_AO_HAVE_nop_full

#ifndef MK_AO_PREFER_GENERALIZED
MK_AO_INLINE MK_AO_t
MK_AO_fetch_and_add1_acquire (volatile MK_AO_t *addr)
{
  MK_AO_t result;

  __asm__ __volatile__ (MK_AO_SWIZZLE
                        "fetchadd" MK_AO_LEN ".acq %0=[%1],1":
                        "=r" (result) MK_AO_OUT_ADDR: MK_AO_IN_ADDR :"memory");
  return result;
}
#define MK_AO_HAVE_fetch_and_add1_acquire

MK_AO_INLINE MK_AO_t
MK_AO_fetch_and_add1_release (volatile MK_AO_t *addr)
{
  MK_AO_t result;

  __asm__ __volatile__ (MK_AO_SWIZZLE
                        "fetchadd" MK_AO_LEN ".rel %0=[%1],1":
                        "=r" (result) MK_AO_OUT_ADDR: MK_AO_IN_ADDR :"memory");
  return result;
}
#define MK_AO_HAVE_fetch_and_add1_release

MK_AO_INLINE MK_AO_t
MK_AO_fetch_and_sub1_acquire (volatile MK_AO_t *addr)
{
  MK_AO_t result;

  __asm__ __volatile__ (MK_AO_SWIZZLE
                        "fetchadd" MK_AO_LEN ".acq %0=[%1],-1":
                        "=r" (result) MK_AO_OUT_ADDR: MK_AO_IN_ADDR :"memory");
  return result;
}
#define MK_AO_HAVE_fetch_and_sub1_acquire

MK_AO_INLINE MK_AO_t
MK_AO_fetch_and_sub1_release (volatile MK_AO_t *addr)
{
  MK_AO_t result;

  __asm__ __volatile__ (MK_AO_SWIZZLE
                        "fetchadd" MK_AO_LEN ".rel %0=[%1],-1":
                        "=r" (result) MK_AO_OUT_ADDR: MK_AO_IN_ADDR :"memory");
  return result;
}
#define MK_AO_HAVE_fetch_and_sub1_release
#endif /* !MK_AO_PREFER_GENERALIZED */

MK_AO_INLINE MK_AO_t
MK_AO_fetch_compare_and_swap_acquire(volatile MK_AO_t *addr, MK_AO_t old, MK_AO_t new_val)
{
  MK_AO_t fetched_val;
  MK_AO_MASK(old);
  __asm__ __volatile__(MK_AO_SWIZZLE
                       "mov ar.ccv=%[old] ;; cmpxchg" MK_AO_LEN
                       ".acq %0=[%1],%[new_val],ar.ccv"
                       : "=r"(fetched_val) MK_AO_OUT_ADDR
                       : MK_AO_IN_ADDR, [new_val]"r"(new_val), [old]"r"(old)
                       : "memory");
  return fetched_val;
}
#define MK_AO_HAVE_fetch_compare_and_swap_acquire

MK_AO_INLINE MK_AO_t
MK_AO_fetch_compare_and_swap_release(volatile MK_AO_t *addr, MK_AO_t old, MK_AO_t new_val)
{
  MK_AO_t fetched_val;
  MK_AO_MASK(old);
  __asm__ __volatile__(MK_AO_SWIZZLE
                       "mov ar.ccv=%[old] ;; cmpxchg" MK_AO_LEN
                       ".rel %0=[%1],%[new_val],ar.ccv"
                       : "=r"(fetched_val) MK_AO_OUT_ADDR
                       : MK_AO_IN_ADDR, [new_val]"r"(new_val), [old]"r"(old)
                       : "memory");
  return fetched_val;
}
#define MK_AO_HAVE_fetch_compare_and_swap_release

MK_AO_INLINE unsigned char
MK_AO_char_fetch_compare_and_swap_acquire(volatile unsigned char *addr,
                                unsigned char old, unsigned char new_val)
{
  unsigned char fetched_val;
  __asm__ __volatile__(MK_AO_SWIZZLE
               "mov ar.ccv=%[old] ;; cmpxchg1.acq %0=[%1],%[new_val],ar.ccv"
               : "=r"(fetched_val) MK_AO_OUT_ADDR
               : MK_AO_IN_ADDR, [new_val]"r"(new_val), [old]"r"((MK_AO_t)old)
               : "memory");
  return fetched_val;
}
#define MK_AO_HAVE_char_fetch_compare_and_swap_acquire

MK_AO_INLINE unsigned char
MK_AO_char_fetch_compare_and_swap_release(volatile unsigned char *addr,
                                unsigned char old, unsigned char new_val)
{
  unsigned char fetched_val;
  __asm__ __volatile__(MK_AO_SWIZZLE
                "mov ar.ccv=%[old] ;; cmpxchg1.rel %0=[%1],%[new_val],ar.ccv"
                : "=r"(fetched_val) MK_AO_OUT_ADDR
                : MK_AO_IN_ADDR, [new_val]"r"(new_val), [old]"r"((MK_AO_t)old)
                : "memory");
  return fetched_val;
}
#define MK_AO_HAVE_char_fetch_compare_and_swap_release

MK_AO_INLINE unsigned short
MK_AO_short_fetch_compare_and_swap_acquire(volatile unsigned short *addr,
                                unsigned short old, unsigned short new_val)
{
  unsigned short fetched_val;
  __asm__ __volatile__(MK_AO_SWIZZLE
                "mov ar.ccv=%[old] ;; cmpxchg2.acq %0=[%1],%[new_val],ar.ccv"
                : "=r"(fetched_val) MK_AO_OUT_ADDR
                : MK_AO_IN_ADDR, [new_val]"r"(new_val), [old]"r"((MK_AO_t)old)
                : "memory");
  return fetched_val;
}
#define MK_AO_HAVE_short_fetch_compare_and_swap_acquire

MK_AO_INLINE unsigned short
MK_AO_short_fetch_compare_and_swap_release(volatile unsigned short *addr,
                                unsigned short old, unsigned short new_val)
{
  unsigned short fetched_val;
  __asm__ __volatile__(MK_AO_SWIZZLE
                "mov ar.ccv=%[old] ;; cmpxchg2.rel %0=[%1],%[new_val],ar.ccv"
                : "=r"(fetched_val) MK_AO_OUT_ADDR
                : MK_AO_IN_ADDR, [new_val]"r"(new_val), [old]"r"((MK_AO_t)old)
                : "memory");
  return fetched_val;
}
#define MK_AO_HAVE_short_fetch_compare_and_swap_release

#ifdef _ILP32

# define MK_AO_T_IS_INT

  /* TODO: Add compare_double_and_swap_double for the _ILP32 case.      */
#else

# ifndef MK_AO_PREFER_GENERALIZED
  MK_AO_INLINE unsigned int
  MK_AO_int_fetch_and_add1_acquire(volatile unsigned int *addr)
  {
    unsigned int result;
    __asm__ __volatile__("fetchadd4.acq %0=[%1],1"
                         : "=r" (result) : MK_AO_IN_ADDR
                         : "memory");
    return result;
  }
# define MK_AO_HAVE_int_fetch_and_add1_acquire

  MK_AO_INLINE unsigned int
  MK_AO_int_fetch_and_add1_release(volatile unsigned int *addr)
  {
    unsigned int result;
    __asm__ __volatile__("fetchadd4.rel %0=[%1],1"
                         : "=r" (result) : MK_AO_IN_ADDR
                         : "memory");
    return result;
  }
# define MK_AO_HAVE_int_fetch_and_add1_release

  MK_AO_INLINE unsigned int
  MK_AO_int_fetch_and_sub1_acquire(volatile unsigned int *addr)
  {
    unsigned int result;
    __asm__ __volatile__("fetchadd4.acq %0=[%1],-1"
                         : "=r" (result) : MK_AO_IN_ADDR
                         : "memory");
    return result;
  }
# define MK_AO_HAVE_int_fetch_and_sub1_acquire

  MK_AO_INLINE unsigned int
  MK_AO_int_fetch_and_sub1_release(volatile unsigned int *addr)
  {
    unsigned int result;
    __asm__ __volatile__("fetchadd4.rel %0=[%1],-1"
                         : "=r" (result) : MK_AO_IN_ADDR
                         : "memory");
    return result;
  }
# define MK_AO_HAVE_int_fetch_and_sub1_release
# endif /* !MK_AO_PREFER_GENERALIZED */

  MK_AO_INLINE unsigned int
  MK_AO_int_fetch_compare_and_swap_acquire(volatile unsigned int *addr,
                                        unsigned int old, unsigned int new_val)
  {
    unsigned int fetched_val;
    __asm__ __volatile__("mov ar.ccv=%3 ;; cmpxchg4.acq %0=[%1],%2,ar.ccv"
                         : "=r"(fetched_val)
                         : MK_AO_IN_ADDR, "r"(new_val), "r"((MK_AO_t)old)
                         : "memory");
    return fetched_val;
  }
# define MK_AO_HAVE_int_fetch_compare_and_swap_acquire

  MK_AO_INLINE unsigned int
  MK_AO_int_fetch_compare_and_swap_release(volatile unsigned int *addr,
                                        unsigned int old, unsigned int new_val)
  {
    unsigned int fetched_val;
    __asm__ __volatile__("mov ar.ccv=%3 ;; cmpxchg4.rel %0=[%1],%2,ar.ccv"
                         : "=r"(fetched_val)
                         : MK_AO_IN_ADDR, "r"(new_val), "r"((MK_AO_t)old)
                         : "memory");
    return fetched_val;
  }
# define MK_AO_HAVE_int_fetch_compare_and_swap_release
#endif /* !_ILP32 */

/* TODO: Add compare_and_swap_double as soon as there is widely         */
/* available hardware that implements it.                               */
