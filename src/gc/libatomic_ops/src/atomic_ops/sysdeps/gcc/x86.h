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

/* The following really assume we have a 486 or better.  Unfortunately  */
/* gcc doesn't define a suitable feature test macro based on command    */
/* line options.                                                        */
/* We should perhaps test dynamically.                                  */

#include "../all_aligned_atomic_load_store.h"

#include "../test_and_set_t_is_char.h"

#if defined(__SSE2__) && !defined(MK_AO_USE_PENTIUM4_INSTRS)
  /* "mfence" is a part of SSE2 set (introduced on Intel Pentium 4).    */
# define MK_AO_USE_PENTIUM4_INSTRS
#endif

#if defined(MK_AO_USE_PENTIUM4_INSTRS)
  MK_AO_INLINE void
  MK_AO_nop_full(void)
  {
    __asm__ __volatile__("mfence" : : : "memory");
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

    __asm__ __volatile__ ("lock; xadd %0, %1" :
                        "=r" (result), "=m" (*p) : "0" (incr), "m" (*p)
                        : "memory");
    return result;
  }
# define MK_AO_HAVE_fetch_and_add_full
#endif /* !MK_AO_PREFER_GENERALIZED */

MK_AO_INLINE unsigned char
MK_AO_char_fetch_and_add_full (volatile unsigned char *p, unsigned char incr)
{
  unsigned char result;

  __asm__ __volatile__ ("lock; xaddb %0, %1" :
                        "=q" (result), "=m" (*p) : "0" (incr), "m" (*p)
                        : "memory");
  return result;
}
#define MK_AO_HAVE_char_fetch_and_add_full

MK_AO_INLINE unsigned short
MK_AO_short_fetch_and_add_full (volatile unsigned short *p, unsigned short incr)
{
  unsigned short result;

  __asm__ __volatile__ ("lock; xaddw %0, %1" :
                        "=r" (result), "=m" (*p) : "0" (incr), "m" (*p)
                        : "memory");
  return result;
}
#define MK_AO_HAVE_short_fetch_and_add_full

#ifndef MK_AO_PREFER_GENERALIZED
  /* Really only works for 486 and later */
  MK_AO_INLINE void
  MK_AO_and_full (volatile MK_AO_t *p, MK_AO_t value)
  {
    __asm__ __volatile__ ("lock; and %1, %0" :
                        "=m" (*p) : "r" (value), "m" (*p)
                        : "memory");
  }
# define MK_AO_HAVE_and_full

  MK_AO_INLINE void
  MK_AO_or_full (volatile MK_AO_t *p, MK_AO_t value)
  {
    __asm__ __volatile__ ("lock; or %1, %0" :
                        "=m" (*p) : "r" (value), "m" (*p)
                        : "memory");
  }
# define MK_AO_HAVE_or_full

  MK_AO_INLINE void
  MK_AO_xor_full (volatile MK_AO_t *p, MK_AO_t value)
  {
    __asm__ __volatile__ ("lock; xor %1, %0" :
                        "=m" (*p) : "r" (value), "m" (*p)
                        : "memory");
  }
# define MK_AO_HAVE_xor_full

  /* MK_AO_store_full could be implemented directly using "xchg" but it    */
  /* could be generalized efficiently as an ordinary store accomplished */
  /* with MK_AO_nop_full ("mfence" instruction).                           */
#endif /* !MK_AO_PREFER_GENERALIZED */

MK_AO_INLINE MK_AO_TS_VAL_t
MK_AO_test_and_set_full(volatile MK_AO_TS_t *addr)
{
  unsigned char oldval;
  /* Note: the "xchg" instruction does not need a "lock" prefix */
  __asm__ __volatile__ ("xchgb %0, %1"
                        : "=q" (oldval), "=m" (*addr)
                        : "0" ((unsigned char)0xff), "m" (*addr)
                        : "memory");
  return (MK_AO_TS_VAL_t)oldval;
}
#define MK_AO_HAVE_test_and_set_full

#ifndef MK_AO_GENERALIZE_ASM_BOOL_CAS
  /* Returns nonzero if the comparison succeeded.       */
  MK_AO_INLINE int
  MK_AO_compare_and_swap_full(volatile MK_AO_t *addr, MK_AO_t old, MK_AO_t new_val)
  {
#   ifdef MK_AO_USE_SYNC_CAS_BUILTIN
      return (int)__sync_bool_compare_and_swap(addr, old, new_val
                                               /* empty protection list */);
                /* Note: an empty list of variables protected by the    */
                /* memory barrier should mean all globally accessible   */
                /* variables are protected.                             */
#   else
      char result;
      __asm__ __volatile__ ("lock; cmpxchg %3, %0; setz %1"
                        : "=m" (*addr), "=a" (result)
                        : "m" (*addr), "r" (new_val), "a" (old)
                        : "memory");
      return (int)result;
#   endif
  }
# define MK_AO_HAVE_compare_and_swap_full
#endif /* !MK_AO_GENERALIZE_ASM_BOOL_CAS */

MK_AO_INLINE MK_AO_t
MK_AO_fetch_compare_and_swap_full(volatile MK_AO_t *addr, MK_AO_t old_val,
                               MK_AO_t new_val)
{
# ifdef MK_AO_USE_SYNC_CAS_BUILTIN
    return __sync_val_compare_and_swap(addr, old_val, new_val
                                       /* empty protection list */);
# else
    MK_AO_t fetched_val;
    __asm__ __volatile__ ("lock; cmpxchg %3, %4"
                        : "=a" (fetched_val), "=m" (*addr)
                        : "a" (old_val), "r" (new_val), "m" (*addr)
                        : "memory");
    return fetched_val;
# endif
}
#define MK_AO_HAVE_fetch_compare_and_swap_full

#if !defined(__x86_64__) && !defined(MK_AO_USE_SYNC_CAS_BUILTIN)
# include "../standard_ao_double_t.h"

  /* Reading or writing a quadword aligned on a 64-bit boundary is      */
  /* always carried out atomically on at least a Pentium according to   */
  /* Chapter 8.1.1 of Volume 3A Part 1 of Intel processor manuals.      */
# define MK_AO_ACCESS_double_CHECK_ALIGNED
# include "../loadstore/double_atomic_load_store.h"

  /* Returns nonzero if the comparison succeeded.       */
  /* Really requires at least a Pentium.                */
  MK_AO_INLINE int
  MK_AO_compare_double_and_swap_double_full(volatile MK_AO_double_t *addr,
                                         MK_AO_t old_val1, MK_AO_t old_val2,
                                         MK_AO_t new_val1, MK_AO_t new_val2)
  {
    char result;
#   ifdef __PIC__
      MK_AO_t saved_ebx;

      /* If PIC is turned on, we cannot use ebx as it is reserved for the */
      /* GOT pointer.  We should save and restore ebx.  The proposed      */
      /* solution is not so efficient as the older alternatives using     */
      /* push ebx or edi as new_val1 (w/o clobbering edi and temporary    */
      /* local variable usage) but it is more portable (it works even if  */
      /* ebx is not used as GOT pointer, and it works for the buggy GCC   */
      /* releases that incorrectly evaluate memory operands offset in the */
      /* inline assembly after push).                                     */
#     ifdef __OPTIMIZE__
        __asm__ __volatile__("mov %%ebx, %2\n\t" /* save ebx */
                             "lea %0, %%edi\n\t" /* in case addr is in ebx */
                             "mov %7, %%ebx\n\t" /* load new_val1 */
                             "lock; cmpxchg8b (%%edi)\n\t"
                             "mov %2, %%ebx\n\t" /* restore ebx */
                             "setz %1"
                        : "=m" (*addr), "=a" (result), "=m" (saved_ebx)
                        : "m" (*addr), "d" (old_val2), "a" (old_val1),
                          "c" (new_val2), "m" (new_val1)
                        : "%edi", "memory");
#     else
        /* A less-efficient code manually preserving edi if GCC invoked */
        /* with -O0 option (otherwise it fails while finding a register */
        /* in class 'GENERAL_REGS').                                    */
        MK_AO_t saved_edi;
        __asm__ __volatile__("mov %%edi, %3\n\t" /* save edi */
                             "mov %%ebx, %2\n\t" /* save ebx */
                             "lea %0, %%edi\n\t" /* in case addr is in ebx */
                             "mov %8, %%ebx\n\t" /* load new_val1 */
                             "lock; cmpxchg8b (%%edi)\n\t"
                             "mov %2, %%ebx\n\t" /* restore ebx */
                             "mov %3, %%edi\n\t" /* restore edi */
                             "setz %1"
                        : "=m" (*addr), "=a" (result),
                          "=m" (saved_ebx), "=m" (saved_edi)
                        : "m" (*addr), "d" (old_val2), "a" (old_val1),
                          "c" (new_val2), "m" (new_val1) : "memory");
#     endif
#   else
      /* For non-PIC mode, this operation could be simplified (and be   */
      /* faster) by using ebx as new_val1 (GCC would refuse to compile  */
      /* such code for PIC mode).                                       */
      __asm__ __volatile__ ("lock; cmpxchg8b %0; setz %1"
                        : "=m" (*addr), "=a" (result)
                        : "m" (*addr), "d" (old_val2), "a" (old_val1),
                          "c" (new_val2), "b" (new_val1)
                        : "memory");
#   endif
    return (int) result;
  }
# define MK_AO_HAVE_compare_double_and_swap_double_full

# define MK_AO_T_IS_INT

#elif defined(__ILP32__) || !defined(__x86_64__)
# include "../standard_ao_double_t.h"

  /* Reading or writing a quadword aligned on a 64-bit boundary is      */
  /* always carried out atomically (requires at least a Pentium).       */
# define MK_AO_ACCESS_double_CHECK_ALIGNED
# include "../loadstore/double_atomic_load_store.h"

  /* X32 has native support for 64-bit integer operations (MK_AO_double_t  */
  /* is a 64-bit integer and we could use 64-bit cmpxchg).              */
  /* This primitive is used by compare_double_and_swap_double_full.     */
  MK_AO_INLINE int
  MK_AO_double_compare_and_swap_full(volatile MK_AO_double_t *addr,
                                  MK_AO_double_t old_val, MK_AO_double_t new_val)
  {
    /* It is safe to use __sync CAS built-in here.      */
    return __sync_bool_compare_and_swap(&addr->MK_AO_whole,
                                        old_val.MK_AO_whole, new_val.MK_AO_whole
                                        /* empty protection list */);
  }
# define MK_AO_HAVE_double_compare_and_swap_full

# define MK_AO_T_IS_INT

#else /* 64-bit */

  MK_AO_INLINE unsigned int
  MK_AO_int_fetch_and_add_full (volatile unsigned int *p, unsigned int incr)
  {
    unsigned int result;

    __asm__ __volatile__ ("lock; xaddl %0, %1"
                        : "=r" (result), "=m" (*p)
                        : "0" (incr), "m" (*p)
                        : "memory");
    return result;
  }
# define MK_AO_HAVE_int_fetch_and_add_full

  /* The Intel and AMD Architecture Programmer Manuals state roughly    */
  /* the following:                                                     */
  /* - CMPXCHG16B (with a LOCK prefix) can be used to perform 16-byte   */
  /* atomic accesses in 64-bit mode (with certain alignment             */
  /* restrictions);                                                     */
  /* - SSE instructions that access data larger than a quadword (like   */
  /* MOVDQA) may be implemented using multiple memory accesses;         */
  /* - LOCK prefix causes an invalid-opcode exception when used with    */
  /* 128-bit media (SSE) instructions.                                  */
  /* Thus, currently, the only way to implement lock-free double_load   */
  /* and double_store on x86_64 is to use CMPXCHG16B (if available).    */

/* TODO: Test some gcc macro to detect presence of cmpxchg16b. */

# ifdef MK_AO_CMPXCHG16B_AVAILABLE
#   include "../standard_ao_double_t.h"

    /* NEC LE-IT: older AMD Opterons are missing this instruction.      */
    /* On these machines SIGILL will be thrown.                         */
    /* Define MK_AO_WEAK_DOUBLE_CAS_EMULATION to have an emulated (lock    */
    /* based) version available.                                        */
    /* HB: Changed this to not define either by default.  There are     */
    /* enough machines and tool chains around on which cmpxchg16b       */
    /* doesn't work.  And the emulation is unsafe by our usual rules.   */
    /* However both are clearly useful in certain cases.                */
    MK_AO_INLINE int
    MK_AO_compare_double_and_swap_double_full(volatile MK_AO_double_t *addr,
                                           MK_AO_t old_val1, MK_AO_t old_val2,
                                           MK_AO_t new_val1, MK_AO_t new_val2)
    {
      char result;
      __asm__ __volatile__("lock; cmpxchg16b %0; setz %1"
                        : "=m"(*addr), "=a"(result)
                        : "m"(*addr), "d" (old_val2), "a" (old_val1),
                          "c" (new_val2), "b" (new_val1)
                        : "memory");
      return (int) result;
    }
#   define MK_AO_HAVE_compare_double_and_swap_double_full

# elif defined(MK_AO_WEAK_DOUBLE_CAS_EMULATION)
#   include "../standard_ao_double_t.h"

    /* This one provides spinlock based emulation of CAS implemented in */
    /* atomic_ops.c.  We probably do not want to do this here, since it */
    /* is not atomic with respect to other kinds of updates of *addr.   */
    /* On the other hand, this may be a useful facility on occasion.    */
    int MK_AO_compare_double_and_swap_double_emulation(
                                                volatile MK_AO_double_t *addr,
                                                MK_AO_t old_val1, MK_AO_t old_val2,
                                                MK_AO_t new_val1, MK_AO_t new_val2);

    MK_AO_INLINE int
    MK_AO_compare_double_and_swap_double_full(volatile MK_AO_double_t *addr,
                                           MK_AO_t old_val1, MK_AO_t old_val2,
                                           MK_AO_t new_val1, MK_AO_t new_val2)
    {
      return MK_AO_compare_double_and_swap_double_emulation(addr,
                                old_val1, old_val2, new_val1, new_val2);
    }
#   define MK_AO_HAVE_compare_double_and_swap_double_full
# endif /* MK_AO_WEAK_DOUBLE_CAS_EMULATION && !MK_AO_CMPXCHG16B_AVAILABLE */

#endif /* x86_64 && !ILP32 */

/* Real X86 implementations, except for some old 32-bit WinChips,       */
/* appear to enforce ordering between memory operations, EXCEPT that    */
/* a later read can pass earlier writes, presumably due to the visible  */
/* presence of store buffers.                                           */
/* We ignore both the WinChips and the fact that the official specs     */
/* seem to be much weaker (and arguably too weak to be usable).         */
#include "../ordered_except_wr.h"
