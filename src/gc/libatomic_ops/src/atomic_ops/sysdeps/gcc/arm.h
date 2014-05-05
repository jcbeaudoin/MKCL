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
 */

#include "../test_and_set_t_is_ao_t.h" /* Probably suboptimal */

#if defined(__thumb__) && !defined(__thumb2__)
  /* Thumb One mode does not have ARM "mcr", "swp" and some load/store  */
  /* instructions, so we temporarily switch to ARM mode and go back     */
  /* afterwards (clobbering "r3" register).                             */
# define MK_AO_THUMB_GO_ARM \
           "       adr     r3, 4f\n" \
           "       bx      r3\n" \
           "      .align\n" \
           "      .arm\n" \
           "4:\n"
# define MK_AO_THUMB_RESTORE_MODE \
           "       adr     r3, 5f + 1\n" \
           "       bx      r3\n" \
           "       .thumb\n" \
           "5:\n"
# define MK_AO_THUMB_SWITCH_CLOBBERS "r3",
#else
# define MK_AO_THUMB_GO_ARM /* empty */
# define MK_AO_THUMB_RESTORE_MODE /* empty */
# define MK_AO_THUMB_SWITCH_CLOBBERS /* empty */
#endif /* !__thumb__ */

/* NEC LE-IT: gcc has no way to easily check the arm architecture       */
/* but it defines only one (or several) of __ARM_ARCH_x__ to be true.   */
#if !defined(__ARM_ARCH_2__) && !defined(__ARM_ARCH_3__) \
    && !defined(__ARM_ARCH_3M__) && !defined(__ARM_ARCH_4__) \
    && !defined(__ARM_ARCH_4T__) \
    && ((!defined(__ARM_ARCH_5__) && !defined(__ARM_ARCH_5E__) \
         && !defined(__ARM_ARCH_5T__) && !defined(__ARM_ARCH_5TE__) \
         && !defined(__ARM_ARCH_5TEJ__) && !defined(__ARM_ARCH_6M__)) \
        || defined(__ARM_ARCH_7__) || defined(__ARM_ARCH_7A__) \
        || defined(__ARM_ARCH_8A__))
# define MK_AO_ARM_HAVE_LDREX
# if !defined(__ARM_ARCH_6__) && !defined(__ARM_ARCH_6J__) \
     && !defined(__ARM_ARCH_6T2__)
    /* LDREXB/STREXB and LDREXH/STREXH are present in ARMv6K/Z+.        */
#   define MK_AO_ARM_HAVE_LDREXBH
# endif
# if !defined(__ARM_ARCH_6__) && !defined(__ARM_ARCH_6J__) \
     && !defined(__ARM_ARCH_6T2__) && !defined(__ARM_ARCH_6Z__) \
     && !defined(__ARM_ARCH_6ZT2__)
#   if !defined(__ARM_ARCH_6K__) && !defined(__ARM_ARCH_6ZK__)
      /* DMB is present in ARMv6M and ARMv7+.   */
#     define MK_AO_ARM_HAVE_DMB
#   endif
#   if (!defined(__thumb__) \
        || (defined(__thumb2__) && !defined(__ARM_ARCH_7__) \
            && !defined(__ARM_ARCH_7M__) && !defined(__ARM_ARCH_7EM__))) \
       && (!defined(__clang__) || (__clang_major__ > 3) \
            || (__clang_major__ == 3 && __clang_minor__ >= 3))
      /* LDREXD/STREXD present in ARMv6K/M+ (see gas/config/tc-arm.c).  */
      /* In the Thumb mode, this works only starting from ARMv7 (except */
      /* for the base and 'M' models).  Clang3.2 (and earlier) does not */
      /* allocate register pairs for LDREXD/STREXD properly (besides,   */
      /* Clang3.1 does not support "%H<r>" operand specification).      */
#     define MK_AO_ARM_HAVE_LDREXD
#   endif /* !thumb || ARMv7A || ARMv7R+ */
# endif /* ARMv7+ */
#endif /* ARMv6+ */

#if !defined(__ARM_ARCH_2__) && !defined(__ARM_ARCH_6M__) \
    && !defined(__ARM_ARCH_8A__) && !defined(__thumb2__)
# define MK_AO_ARM_HAVE_SWP
                /* Note: ARMv6M is excluded due to no ARM mode support. */
                /* Also, SWP is obsoleted for ARMv8+.                   */
#endif /* !__thumb2__ */

#ifdef MK_AO_UNIPROCESSOR
  /* If only a single processor (core) is used, MK_AO_UNIPROCESSOR could   */
  /* be defined by the client to avoid unnecessary memory barrier.      */
  MK_AO_INLINE void
  MK_AO_nop_full(void)
  {
    MK_AO_compiler_barrier();
  }
# define MK_AO_HAVE_nop_full

#elif defined(MK_AO_ARM_HAVE_DMB)
  /* ARMv7 is compatible to ARMv6 but has a simpler command for issuing */
  /* a memory barrier (DMB).  Raising it via CP15 should still work     */
  /* (but slightly less efficient because it requires the use of        */
  /* a general-purpose register).                                       */
  MK_AO_INLINE void
  MK_AO_nop_full(void)
  {
    /* MK_AO_THUMB_GO_ARM is empty. */
    __asm__ __volatile__("dmb" : : : "memory");
  }
# define MK_AO_HAVE_nop_full

  MK_AO_INLINE void
  MK_AO_nop_write(void)
  {
    /* MK_AO_THUMB_GO_ARM is empty. */
    __asm__ __volatile__("dmb st" : : : "memory");
  }
# define MK_AO_HAVE_nop_write

#elif defined(MK_AO_ARM_HAVE_LDREX)
  /* ARMv6 is the first architecture providing support for a simple     */
  /* LL/SC.  A data memory barrier must be raised via CP15 command.     */
  MK_AO_INLINE void
  MK_AO_nop_full(void)
  {
    unsigned dest = 0;

    /* Issue a data memory barrier (keeps ordering of memory    */
    /* transactions before and after this operation).           */
    __asm__ __volatile__("@MK_AO_nop_full\n"
      MK_AO_THUMB_GO_ARM
      "       mcr p15,0,%0,c7,c10,5\n"
      MK_AO_THUMB_RESTORE_MODE
      : "=&r"(dest)
      : /* empty */
      : MK_AO_THUMB_SWITCH_CLOBBERS "memory");
  }
# define MK_AO_HAVE_nop_full

#else
  /* MK_AO_nop_full() is emulated using MK_AO_test_and_set_full().    */
#endif /* !MK_AO_UNIPROCESSOR && !MK_AO_ARM_HAVE_LDREX */

#ifdef MK_AO_ARM_HAVE_LDREX

  /* MK_AO_t/char/short/int load is simple reading.                */
  /* Unaligned accesses are not guaranteed to be atomic.        */
# define MK_AO_ACCESS_CHECK_ALIGNED
# define MK_AO_ACCESS_short_CHECK_ALIGNED
# define MK_AO_ACCESS_int_CHECK_ALIGNED
# include "../all_atomic_only_load.h"

  /* "ARM Architecture Reference Manual" (chapter A3.5.3) says that the */
  /* single-copy atomic processor accesses are all byte accesses, all   */
  /* halfword accesses to halfword-aligned locations, all word accesses */
  /* to word-aligned locations.                                         */
  /* There is only a single concern related to AO store operations:     */
  /* a direct write (by STR[B/H] instruction) will not be recognized    */
  /* by the LL/SC construct on the same CPU (i.e., according to ARM     */
  /* documentation, e.g., see CortexA8 TRM reference, point 8.5,        */
  /* atomic "store" (using LDREX/STREX[B/H]) is the only safe way to    */
  /* set variables also used in LL/SC environment).                     */
  /* This is only a problem if interrupt handlers do not clear the      */
  /* reservation (by CLREX instruction or a dummy STREX one), as they   */
  /* almost certainly should (e.g., see restore_user_regs defined in    */
  /* arch/arm/kernel/entry-header.S of Linux.  Nonetheless, there is    */
  /* a doubt this was properly implemented in some ancient OS releases. */
# ifdef MK_AO_BROKEN_TASKSWITCH_CLREX
    MK_AO_INLINE void MK_AO_store(volatile MK_AO_t *addr, MK_AO_t value)
    {
      int flag;

      __asm__ __volatile__("@MK_AO_store\n"
        MK_AO_THUMB_GO_ARM
        "1:     ldrex %0, [%2]\n"
        "       strex %0, %3, [%2]\n"
        "       teq %0, #0\n"
        "       bne 1b\n"
        MK_AO_THUMB_RESTORE_MODE
        : "=&r" (flag), "+m" (*addr)
        : "r" (addr), "r" (value)
        : MK_AO_THUMB_SWITCH_CLOBBERS "cc");
    }
#   define MK_AO_HAVE_store

#   ifdef MK_AO_ARM_HAVE_LDREXBH
      MK_AO_INLINE void MK_AO_char_store(volatile unsigned char *addr,
                                   unsigned char value)
      {
        int flag;

        __asm__ __volatile__("@MK_AO_char_store\n"
          MK_AO_THUMB_GO_ARM
          "1:     ldrexb %0, [%2]\n"
          "       strexb %0, %3, [%2]\n"
          "       teq    %0, #0\n"
          "       bne 1b\n"
          MK_AO_THUMB_RESTORE_MODE
          : "=&r" (flag), "+m" (*addr)
          : "r" (addr), "r" (value)
          : MK_AO_THUMB_SWITCH_CLOBBERS "cc");
      }
#     define MK_AO_HAVE_char_store

      MK_AO_INLINE void MK_AO_short_store(volatile unsigned short *addr,
                                    unsigned short value)
      {
        int flag;

        __asm__ __volatile__("@MK_AO_short_store\n"
          MK_AO_THUMB_GO_ARM
          "1:     ldrexh %0, [%2]\n"
          "       strexh %0, %3, [%2]\n"
          "       teq    %0, #0\n"
          "       bne 1b\n"
          MK_AO_THUMB_RESTORE_MODE
          : "=&r" (flag), "+m" (*addr)
          : "r" (addr), "r" (value)
          : MK_AO_THUMB_SWITCH_CLOBBERS "cc");
      }
#     define MK_AO_HAVE_short_store
#   endif /* MK_AO_ARM_HAVE_LDREXBH */

# else
#   include "../loadstore/atomic_store.h"
    /* MK_AO_int_store is defined in ao_t_is_int.h.    */
# endif /* !MK_AO_BROKEN_TASKSWITCH_CLREX */

# ifndef MK_AO_HAVE_char_store
#   include "../loadstore/char_atomic_store.h"
#   include "../loadstore/short_atomic_store.h"
# endif

/* NEC LE-IT: replace the SWAP as recommended by ARM:
   "Applies to: ARM11 Cores
      Though the SWP instruction will still work with ARM V6 cores, it is
      recommended to use the new V6 synchronization instructions.  The SWP
      instruction produces 'locked' read and write accesses which are atomic,
      i.e. another operation cannot be done between these locked accesses which
      ties up external bus (AHB, AXI) bandwidth and can increase worst case
      interrupt latencies. LDREX, STREX are more flexible, other instructions
      can be done between the LDREX and STREX accesses."
*/
#ifndef MK_AO_PREFER_GENERALIZED
#if !defined(MK_AO_FORCE_USE_SWP) || !defined(MK_AO_ARM_HAVE_SWP)
  /* But, on the other hand, there could be a considerable performance  */
  /* degradation in case of a race.  Eg., test_atomic.c executing       */
  /* test_and_set test on a dual-core ARMv7 processor using LDREX/STREX */
  /* showed around 35 times lower performance than that using SWP.      */
  /* To force use of SWP instruction, use -D MK_AO_FORCE_USE_SWP option    */
  /* (the latter is ignored if SWP instruction is unsupported).         */
  MK_AO_INLINE MK_AO_TS_VAL_t
  MK_AO_test_and_set(volatile MK_AO_TS_t *addr)
  {
    MK_AO_TS_VAL_t oldval;
    int flag;

    __asm__ __volatile__("@MK_AO_test_and_set\n"
      MK_AO_THUMB_GO_ARM
      "1:     ldrex   %0, [%3]\n"
      "       strex   %1, %4, [%3]\n"
      "       teq     %1, #0\n"
      "       bne     1b\n"
      MK_AO_THUMB_RESTORE_MODE
      : "=&r"(oldval), "=&r"(flag), "+m"(*addr)
      : "r"(addr), "r"(1)
      : MK_AO_THUMB_SWITCH_CLOBBERS "cc");
    return oldval;
  }
# define MK_AO_HAVE_test_and_set
#endif /* !MK_AO_FORCE_USE_SWP */

MK_AO_INLINE MK_AO_t
MK_AO_fetch_and_add(volatile MK_AO_t *p, MK_AO_t incr)
{
  MK_AO_t result, tmp;
  int flag;

  __asm__ __volatile__("@MK_AO_fetch_and_add\n"
    MK_AO_THUMB_GO_ARM
    "1:     ldrex   %0, [%5]\n"         /* get original         */
    "       add     %2, %0, %4\n"       /* sum up in incr       */
    "       strex   %1, %2, [%5]\n"     /* store them           */
    "       teq     %1, #0\n"
    "       bne     1b\n"
    MK_AO_THUMB_RESTORE_MODE
    : "=&r"(result), "=&r"(flag), "=&r"(tmp), "+m"(*p) /* 0..3 */
    : "r"(incr), "r"(p)                                /* 4..5 */
    : MK_AO_THUMB_SWITCH_CLOBBERS "cc");
  return result;
}
#define MK_AO_HAVE_fetch_and_add

MK_AO_INLINE MK_AO_t
MK_AO_fetch_and_add1(volatile MK_AO_t *p)
{
  MK_AO_t result, tmp;
  int flag;

  __asm__ __volatile__("@MK_AO_fetch_and_add1\n"
    MK_AO_THUMB_GO_ARM
    "1:     ldrex   %0, [%4]\n"         /* get original */
    "       add     %1, %0, #1\n"       /* increment */
    "       strex   %2, %1, [%4]\n"     /* store them */
    "       teq     %2, #0\n"
    "       bne     1b\n"
    MK_AO_THUMB_RESTORE_MODE
    : "=&r"(result), "=&r"(tmp), "=&r"(flag), "+m"(*p)
    : "r"(p)
    : MK_AO_THUMB_SWITCH_CLOBBERS "cc");
  return result;
}
#define MK_AO_HAVE_fetch_and_add1

MK_AO_INLINE MK_AO_t
MK_AO_fetch_and_sub1(volatile MK_AO_t *p)
{
  MK_AO_t result, tmp;
  int flag;

  __asm__ __volatile__("@MK_AO_fetch_and_sub1\n"
    MK_AO_THUMB_GO_ARM
    "1:     ldrex   %0, [%4]\n"         /* get original */
    "       sub     %1, %0, #1\n"       /* decrement */
    "       strex   %2, %1, [%4]\n"     /* store them */
    "       teq     %2, #0\n"
    "       bne     1b\n"
    MK_AO_THUMB_RESTORE_MODE
    : "=&r"(result), "=&r"(tmp), "=&r"(flag), "+m"(*p)
    : "r"(p)
    : MK_AO_THUMB_SWITCH_CLOBBERS "cc");
  return result;
}
#define MK_AO_HAVE_fetch_and_sub1

MK_AO_INLINE void
MK_AO_and(volatile MK_AO_t *p, MK_AO_t value)
{
  MK_AO_t tmp, result;

  __asm__ __volatile__("@MK_AO_and\n"
    MK_AO_THUMB_GO_ARM
    "1:     ldrex   %0, [%4]\n"
    "       and     %1, %0, %3\n"
    "       strex   %0, %1, [%4]\n"
    "       teq     %0, #0\n"
    "       bne     1b\n"
    MK_AO_THUMB_RESTORE_MODE
    : "=&r" (tmp), "=&r" (result), "+m" (*p)
    : "r" (value), "r" (p)
    : MK_AO_THUMB_SWITCH_CLOBBERS "cc");
}
#define MK_AO_HAVE_and

MK_AO_INLINE void
MK_AO_or(volatile MK_AO_t *p, MK_AO_t value)
{
  MK_AO_t tmp, result;

  __asm__ __volatile__("@MK_AO_or\n"
    MK_AO_THUMB_GO_ARM
    "1:     ldrex   %0, [%4]\n"
    "       orr     %1, %0, %3\n"
    "       strex   %0, %1, [%4]\n"
    "       teq     %0, #0\n"
    "       bne     1b\n"
    MK_AO_THUMB_RESTORE_MODE
    : "=&r" (tmp), "=&r" (result), "+m" (*p)
    : "r" (value), "r" (p)
    : MK_AO_THUMB_SWITCH_CLOBBERS "cc");
}
#define MK_AO_HAVE_or

MK_AO_INLINE void
MK_AO_xor(volatile MK_AO_t *p, MK_AO_t value)
{
  MK_AO_t tmp, result;

  __asm__ __volatile__("@MK_AO_xor\n"
    MK_AO_THUMB_GO_ARM
    "1:     ldrex   %0, [%4]\n"
    "       eor     %1, %0, %3\n"
    "       strex   %0, %1, [%4]\n"
    "       teq     %0, #0\n"
    "       bne     1b\n"
    MK_AO_THUMB_RESTORE_MODE
    : "=&r" (tmp), "=&r" (result), "+m" (*p)
    : "r" (value), "r" (p)
    : MK_AO_THUMB_SWITCH_CLOBBERS "cc");
}
#define MK_AO_HAVE_xor
#endif /* !MK_AO_PREFER_GENERALIZED */

#ifdef MK_AO_ARM_HAVE_LDREXBH
  MK_AO_INLINE unsigned char
  MK_AO_char_fetch_and_add(volatile unsigned char *p, unsigned char incr)
  {
    unsigned result, tmp;
    int flag;

    __asm__ __volatile__("@MK_AO_char_fetch_and_add\n"
      MK_AO_THUMB_GO_ARM
      "1:     ldrexb  %0, [%5]\n"
      "       add     %2, %0, %4\n"
      "       strexb  %1, %2, [%5]\n"
      "       teq     %1, #0\n"
      "       bne     1b\n"
      MK_AO_THUMB_RESTORE_MODE
      : "=&r" (result), "=&r" (flag), "=&r" (tmp), "+m" (*p)
      : "r" ((unsigned)incr), "r" (p)
      : MK_AO_THUMB_SWITCH_CLOBBERS "cc");
    return (unsigned char)result;
  }
# define MK_AO_HAVE_char_fetch_and_add

  MK_AO_INLINE unsigned short
  MK_AO_short_fetch_and_add(volatile unsigned short *p, unsigned short incr)
  {
    unsigned result, tmp;
    int flag;

    __asm__ __volatile__("@MK_AO_short_fetch_and_add\n"
      MK_AO_THUMB_GO_ARM
      "1:     ldrexh  %0, [%5]\n"
      "       add     %2, %0, %4\n"
      "       strexh  %1, %2, [%5]\n"
      "       teq     %1, #0\n"
      "       bne     1b\n"
      MK_AO_THUMB_RESTORE_MODE
      : "=&r" (result), "=&r" (flag), "=&r" (tmp), "+m" (*p)
      : "r" ((unsigned)incr), "r" (p)
      : MK_AO_THUMB_SWITCH_CLOBBERS "cc");
    return (unsigned short)result;
  }
# define MK_AO_HAVE_short_fetch_and_add
#endif /* MK_AO_ARM_HAVE_LDREXBH */

#ifndef MK_AO_GENERALIZE_ASM_BOOL_CAS
  /* Returns nonzero if the comparison succeeded.       */
  MK_AO_INLINE int
  MK_AO_compare_and_swap(volatile MK_AO_t *addr, MK_AO_t old_val, MK_AO_t new_val)
  {
    MK_AO_t result, tmp;

    __asm__ __volatile__("@MK_AO_compare_and_swap\n"
      MK_AO_THUMB_GO_ARM
      "1:     mov     %0, #2\n"         /* store a flag */
      "       ldrex   %1, [%3]\n"       /* get original */
      "       teq     %1, %4\n"         /* see if match */
#     ifdef __thumb2__
        /* TODO: Eliminate warning: it blocks containing wide Thumb */
        /* instructions are deprecated in ARMv8.                    */
        "       it      eq\n"
#     endif
      "       strexeq %0, %5, [%3]\n"   /* store new one if matched */
      "       teq     %0, #1\n"
      "       beq     1b\n"             /* if update failed, repeat */
      MK_AO_THUMB_RESTORE_MODE
      : "=&r"(result), "=&r"(tmp), "+m"(*addr)
      : "r"(addr), "r"(old_val), "r"(new_val)
      : MK_AO_THUMB_SWITCH_CLOBBERS "cc");
    return !(result&2); /* if succeded, return 1, else 0 */
  }
# define MK_AO_HAVE_compare_and_swap
#endif /* !MK_AO_GENERALIZE_ASM_BOOL_CAS */

MK_AO_INLINE MK_AO_t
MK_AO_fetch_compare_and_swap(volatile MK_AO_t *addr, MK_AO_t old_val, MK_AO_t new_val)
{
  MK_AO_t fetched_val;
  int flag;

  __asm__ __volatile__("@MK_AO_fetch_compare_and_swap\n"
    MK_AO_THUMB_GO_ARM
    "1:     mov     %0, #2\n"           /* store a flag */
    "       ldrex   %1, [%3]\n"         /* get original */
    "       teq     %1, %4\n"           /* see if match */
#   ifdef __thumb2__
      "       it      eq\n"
#   endif
    "       strexeq %0, %5, [%3]\n"     /* store new one if matched */
    "       teq     %0, #1\n"
    "       beq     1b\n"               /* if update failed, repeat */
    MK_AO_THUMB_RESTORE_MODE
    : "=&r"(flag), "=&r"(fetched_val), "+m"(*addr)
    : "r"(addr), "r"(old_val), "r"(new_val)
    : MK_AO_THUMB_SWITCH_CLOBBERS "cc");
  return fetched_val;
}
#define MK_AO_HAVE_fetch_compare_and_swap

#ifdef MK_AO_ARM_HAVE_LDREXD
# include "../standard_ao_double_t.h"

  /* "ARM Architecture Reference Manual ARMv7-A/R edition" (chapter     */
  /* A3.5.3) says that memory accesses caused by LDREXD and STREXD      */
  /* instructions to doubleword-aligned locations are single-copy       */
  /* atomic; accesses to 64-bit elements by other instructions might    */
  /* not be single-copy atomic as they are executed as a sequence of    */
  /* 32-bit accesses.                                                   */
  MK_AO_INLINE MK_AO_double_t
  MK_AO_double_load(const volatile MK_AO_double_t *addr)
  {
    MK_AO_double_t result;

    /* MK_AO_THUMB_GO_ARM is empty. */
    __asm__ __volatile__("@MK_AO_double_load\n"
      "       ldrexd  %0, %H0, [%1]"
      : "=&r" (result.MK_AO_whole)
      : "r" (addr)
      /* : no clobber */);
    return result;
  }
# define MK_AO_HAVE_double_load

  MK_AO_INLINE void
  MK_AO_double_store(volatile MK_AO_double_t *addr, MK_AO_double_t new_val)
  {
    MK_AO_double_t old_val;
    int status;

    do {
      /* MK_AO_THUMB_GO_ARM is empty. */
      __asm__ __volatile__("@MK_AO_double_store\n"
        "       ldrexd  %0, %H0, [%3]\n"
        "       strexd  %1, %4, %H4, [%3]"
        : "=&r" (old_val.MK_AO_whole), "=&r" (status), "+m" (*addr)
        : "r" (addr), "r" (new_val.MK_AO_whole)
        : "cc");
    } while (MK_AO_EXPECT_FALSE(status));
  }
# define MK_AO_HAVE_double_store

  MK_AO_INLINE int
  MK_AO_double_compare_and_swap(volatile MK_AO_double_t *addr,
                             MK_AO_double_t old_val, MK_AO_double_t new_val)
  {
    double_ptr_storage tmp;
    int result = 1;

    do {
      /* MK_AO_THUMB_GO_ARM is empty. */
      __asm__ __volatile__("@MK_AO_double_compare_and_swap\n"
        "       ldrexd  %0, %H0, [%1]\n" /* get original to r1 & r2 */
        : "=&r"(tmp)
        : "r"(addr)
        /* : no clobber */);
      if (tmp != old_val.MK_AO_whole)
        break;
      __asm__ __volatile__(
        "       strexd  %0, %3, %H3, [%2]\n" /* store new one if matched */
        : "=&r"(result), "+m"(*addr)
        : "r" (addr), "r" (new_val.MK_AO_whole)
        : "cc");
    } while (MK_AO_EXPECT_FALSE(result));
    return !result;   /* if succeded, return 1 else 0 */
  }
# define MK_AO_HAVE_double_compare_and_swap
#endif /* MK_AO_ARM_HAVE_LDREXD */

#else
/* pre ARMv6 architectures ... */

/* I found a slide set that, if I read it correctly, claims that        */
/* Loads followed by either a Load or Store are ordered, but nothing    */
/* else is.                                                             */
/* It appears that SWP is the only simple memory barrier.               */
#include "../all_aligned_atomic_load_store.h"

/* The code should run correctly on a multi-core ARMv6+ as well.        */

#endif /* !MK_AO_ARM_HAVE_LDREX */

#if !defined(MK_AO_HAVE_test_and_set_full) && !defined(MK_AO_HAVE_test_and_set) \
    && defined (MK_AO_ARM_HAVE_SWP) && (!defined(MK_AO_PREFER_GENERALIZED) \
                                || !defined(MK_AO_HAVE_fetch_compare_and_swap))
  MK_AO_INLINE MK_AO_TS_VAL_t
  MK_AO_test_and_set_full(volatile MK_AO_TS_t *addr)
  {
    MK_AO_TS_VAL_t oldval;
    /* SWP on ARM is very similar to XCHG on x86.                       */
    /* The first operand is the result, the second the value            */
    /* to be stored.  Both registers must be different from addr.       */
    /* Make the address operand an early clobber output so it           */
    /* doesn't overlap with the other operands.  The early clobber      */
    /* on oldval is necessary to prevent the compiler allocating        */
    /* them to the same register if they are both unused.               */

    __asm__ __volatile__("@MK_AO_test_and_set_full\n"
      MK_AO_THUMB_GO_ARM
      "       swp %0, %2, [%3]\n"
                /* Ignore GCC "SWP is deprecated for this architecture" */
                /* warning here (for ARMv6+).                           */
      MK_AO_THUMB_RESTORE_MODE
      : "=&r"(oldval), "=&r"(addr)
      : "r"(1), "1"(addr)
      : MK_AO_THUMB_SWITCH_CLOBBERS "memory");
    return oldval;
  }
# define MK_AO_HAVE_test_and_set_full
#endif /* !MK_AO_HAVE_test_and_set[_full] && MK_AO_ARM_HAVE_SWP */

#define MK_AO_T_IS_INT
