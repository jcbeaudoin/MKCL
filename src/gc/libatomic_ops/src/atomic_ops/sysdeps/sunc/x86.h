/*
 * Copyright (c) 1991-1994 by Xerox Corporation.  All rights reserved.
 * Copyright (c) 1996-1999 by Silicon Graphics.  All rights reserved.
 * Copyright (c) 1999-2003 by Hewlett-Packard Company. All rights reserved.
 *
 *
 * THIS MATERIAL IS PROVIDED AS IS, WITH ABSOLUTELY NO WARRANTY EXPRESSED
 * OR IMPLIED.  ANY USE IS AT YOUR OWN RISK.
 *
 * Permission is hereby granted to use or copy this program
 * for any purpose,  provided the above notices are retained on all copies.
 * Permission to modify the code and to distribute modified code is granted,
 * provided the above notices are retained, and a notice that the code was
 * modified is included with the above copyright notice.
 *
 * Some of the machine specific code was borrowed from our GC distribution.
 */

/* The following really assume we have a 486 or better.                 */

#include "../all_aligned_atomic_load_store.h"

#include "../test_and_set_t_is_char.h"

#if !defined(MK_AO_USE_PENTIUM4_INSTRS) && !defined(__i386)
  /* "mfence" (SSE2) is supported on all x86_64/amd64 chips.            */
# define MK_AO_USE_PENTIUM4_INSTRS
#endif

#if defined(MK_AO_USE_PENTIUM4_INSTRS)
  MK_AO_INLINE void
  MK_AO_nop_full(void)
  {
    __asm__ __volatile__ ("mfence" : : : "memory");
  }
# define MK_AO_HAVE_nop_full

#else
  /* We could use the cpuid instruction.  But that seems to be slower   */
  /* than the default implementation based on test_and_set_full.  Thus  */
  /* we omit that bit of misinformation here.                           */
#endif /* !MK_AO_USE_PENTIUM4_INSTRS */

/* As far as we can tell, the lfence and sfence instructions are not    */
/* currently needed or useful for cached memory accesses.               */

/* Really only works for 486 and later */
#ifndef MK_AO_PREFER_GENERALIZED
  MK_AO_INLINE MK_AO_t
  MK_AO_fetch_and_add_full (volatile MK_AO_t *p, MK_AO_t incr)
  {
    MK_AO_t result;

    __asm__ __volatile__ ("lock; xadd %0, %1"
                        : "=r" (result), "+m" (*p)
                        : "0" (incr)
                        : "memory");
    return result;
  }
# define MK_AO_HAVE_fetch_and_add_full
#endif /* !MK_AO_PREFER_GENERALIZED */

MK_AO_INLINE unsigned char
MK_AO_char_fetch_and_add_full (volatile unsigned char *p, unsigned char incr)
{
  unsigned char result;

  __asm__ __volatile__ ("lock; xaddb %0, %1"
                        : "=q" (result), "+m" (*p)
                        : "0" (incr)
                        : "memory");
  return result;
}
#define MK_AO_HAVE_char_fetch_and_add_full

MK_AO_INLINE unsigned short
MK_AO_short_fetch_and_add_full (volatile unsigned short *p, unsigned short incr)
{
  unsigned short result;

  __asm__ __volatile__ ("lock; xaddw %0, %1"
                        : "=r" (result), "+m" (*p)
                        : "0" (incr)
                        : "memory");
  return result;
}
#define MK_AO_HAVE_short_fetch_and_add_full

#ifndef MK_AO_PREFER_GENERALIZED
  /* Really only works for 486 and later */
  MK_AO_INLINE void
  MK_AO_and_full (volatile MK_AO_t *p, MK_AO_t value)
  {
    __asm__ __volatile__ ("lock; and %1, %0"
                        : "+m" (*p)
                        : "r" (value)
                        : "memory");
  }
# define MK_AO_HAVE_and_full

  MK_AO_INLINE void
  MK_AO_or_full (volatile MK_AO_t *p, MK_AO_t value)
  {
    __asm__ __volatile__ ("lock; or %1, %0"
                        : "+m" (*p)
                        : "r" (value)
                        : "memory");
  }
# define MK_AO_HAVE_or_full

  MK_AO_INLINE void
  MK_AO_xor_full (volatile MK_AO_t *p, MK_AO_t value)
  {
    __asm__ __volatile__ ("lock; xor %1, %0"
                        : "+m" (*p)
                        : "r" (value)
                        : "memory");
  }
# define MK_AO_HAVE_xor_full
#endif /* !MK_AO_PREFER_GENERALIZED */

MK_AO_INLINE MK_AO_TS_VAL_t
MK_AO_test_and_set_full (volatile MK_AO_TS_t *addr)
{
  MK_AO_TS_t oldval;
  /* Note: the "xchg" instruction does not need a "lock" prefix */
  __asm__ __volatile__ ("xchg %b0, %1"
                        : "=q" (oldval), "+m" (*addr)
                        : "0" (0xff)
                        : "memory");
  return (MK_AO_TS_VAL_t)oldval;
}
#define MK_AO_HAVE_test_and_set_full

#ifndef MK_AO_GENERALIZE_ASM_BOOL_CAS
  /* Returns nonzero if the comparison succeeded.       */
  MK_AO_INLINE int
  MK_AO_compare_and_swap_full(volatile MK_AO_t *addr, MK_AO_t old, MK_AO_t new_val)
  {
    char result;
    __asm__ __volatile__ ("lock; cmpxchg %2, %0; setz %1"
                        : "+m" (*addr), "=a" (result)
                        : "r" (new_val), "a" (old)
                        : "memory");
    return (int) result;
  }
# define MK_AO_HAVE_compare_and_swap_full
#endif /* !MK_AO_GENERALIZE_ASM_BOOL_CAS */

MK_AO_INLINE MK_AO_t
MK_AO_fetch_compare_and_swap_full(volatile MK_AO_t *addr, MK_AO_t old_val,
                               MK_AO_t new_val)
{
  MK_AO_t fetched_val;
  __asm__ __volatile__ ("lock; cmpxchg %2, %0"
                        : "+m" (*addr), "=a" (fetched_val)
                        : "r" (new_val), "a" (old_val)
                        : "memory");
  return fetched_val;
}
#define MK_AO_HAVE_fetch_compare_and_swap_full

#if defined(__i386)

# ifndef MK_AO_NO_CMPXCHG8B
#   include "../standard_ao_double_t.h"

    /* Reading or writing a quadword aligned on a 64-bit boundary is    */
    /* always carried out atomically (requires at least a Pentium).     */
#   define MK_AO_ACCESS_double_CHECK_ALIGNED
#   include "../loadstore/double_atomic_load_store.h"

    /* Returns nonzero if the comparison succeeded.     */
    /* Really requires at least a Pentium.              */
    MK_AO_INLINE int
    MK_AO_compare_double_and_swap_double_full(volatile MK_AO_double_t *addr,
                                           MK_AO_t old_val1, MK_AO_t old_val2,
                                           MK_AO_t new_val1, MK_AO_t new_val2)
    {
      char result;

      __asm__ __volatile__ ("lock; cmpxchg8b %0; setz %1"
                        : "+m" (*addr), "=a" (result)
                        : "d" (old_val2), "a" (old_val1),
                          "c" (new_val2), "b" (new_val1)
                        : "memory");
      return (int) result;
    }
#   define MK_AO_HAVE_compare_double_and_swap_double_full
# endif /* !MK_AO_NO_CMPXCHG8B */

# define MK_AO_T_IS_INT

#else /* x64 */

  MK_AO_INLINE unsigned int
  MK_AO_int_fetch_and_add_full (volatile unsigned int *p, unsigned int incr)
  {
    unsigned int result;

    __asm__ __volatile__ ("lock; xaddl %0, %1"
                        : "=r" (result), "+m" (*p)
                        : "0" (incr)
                        : "memory");
    return result;
  }
# define MK_AO_HAVE_int_fetch_and_add_full

# ifdef MK_AO_CMPXCHG16B_AVAILABLE
#   include "../standard_ao_double_t.h"

    /* Older AMD Opterons are missing this instruction (SIGILL should   */
    /* be thrown in this case).                                         */
    MK_AO_INLINE int
    MK_AO_compare_double_and_swap_double_full (volatile MK_AO_double_t *addr,
                                            MK_AO_t old_val1, MK_AO_t old_val2,
                                            MK_AO_t new_val1, MK_AO_t new_val2)
    {
      char result;
      __asm__ __volatile__ ("lock; cmpxchg16b %0; setz %1"
                        : "+m" (*addr), "=a" (result)
                        : "d" (old_val2), "a" (old_val1),
                          "c" (new_val2), "b" (new_val1)
                        : "memory");
      return (int) result;
    }
#   define MK_AO_HAVE_compare_double_and_swap_double_full
# endif /* !MK_AO_CMPXCHG16B_AVAILABLE */

#endif /* x64 */

/* Real X86 implementations, except for some old 32-bit WinChips,       */
/* appear to enforce ordering between memory operations, EXCEPT that    */
/* a later read can pass earlier writes, presumably due to the visible  */
/* presence of store buffers.                                           */
/* We ignore both the WinChips and the fact that the official specs     */
/* seem to be much weaker (and arguably too weak to be usable).         */
#include "../ordered_except_wr.h"
