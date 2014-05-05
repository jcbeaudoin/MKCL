/*
 * Copyright (c) 2005,2007  Thiemo Seufer <ths@networkno.de>
 *
 * THIS MATERIAL IS PROVIDED AS IS, WITH ABSOLUTELY NO WARRANTY EXPRESSED
 * OR IMPLIED.  ANY USE IS AT YOUR OWN RISK.
 *
 * Permission is hereby granted to use or copy this program
 * for any purpose,  provided the above notices are retained on all copies.
 * Permission to modify the code and to distribute modified code is granted,
 * provided the above notices are retained, and a notice that the code was
 * modified is included with the above copyright notice.
 */

/*
 * FIXME:  This should probably make finer distinctions.  SGI MIPS is
 * much more strongly ordered, and in fact closer to sequentially
 * consistent.  This is really aimed at modern embedded implementations.
 * It looks to me like this assumes a 32-bit ABI.  -HB
 */

#include "../all_aligned_atomic_load_store.h"

#include "../loadstore/acquire_release_volatile.h"

#include "../test_and_set_t_is_ao_t.h"

/* Data dependence does not imply read ordering.  */
#define MK_AO_NO_DD_ORDERING

#ifdef MK_AO_ICE9A1_LLSC_WAR
  /* ICE9 rev A1 chip (used in very few systems) is reported to */
  /* have a low-frequency bug that causes LL to fail.           */
  /* To workaround, just issue the second 'LL'.                 */
# define MK_AO_MIPS_LL_FIX(args_str) \
      "       ll   " args_str "\n"
#else
# define MK_AO_MIPS_LL_FIX(args_str) ""
#endif

MK_AO_INLINE void
MK_AO_nop_full(void)
{
  __asm__ __volatile__(
      "       .set push           \n"
      "       .set mips2          \n"
      "       .set noreorder      \n"
      "       .set nomacro        \n"
      "       sync                \n"
      "       .set pop              "
      : : : "memory");
}
#define MK_AO_HAVE_nop_full

#ifndef MK_AO_PREFER_GENERALIZED
MK_AO_INLINE MK_AO_t
MK_AO_fetch_and_add(volatile MK_AO_t *addr, MK_AO_t incr)
{
  register int result;
  register int temp;

  __asm__ __volatile__(
      "       .set push\n"
      "       .set mips2\n"
      "       .set noreorder\n"
      "       .set nomacro\n"
      "1:     ll   %0, %2\n"
      MK_AO_MIPS_LL_FIX("%0, %2")
      "       addu %1, %0, %3\n"
      "       sc   %1, %2\n"
      "       beqz %1, 1b\n"
      "       nop\n"
      "       .set pop "
      : "=&r" (result), "=&r" (temp), "+m" (*addr)
      : "Ir" (incr)
      : "memory");
  return (MK_AO_t)result;
}
#define MK_AO_HAVE_fetch_and_add

MK_AO_INLINE MK_AO_TS_VAL_t
MK_AO_test_and_set(volatile MK_AO_TS_t *addr)
{
  register int oldval;
  register int temp;

  __asm__ __volatile__(
      "       .set push\n"
      "       .set mips2\n"
      "       .set noreorder\n"
      "       .set nomacro\n"
      "1:     ll   %0, %2\n"
      MK_AO_MIPS_LL_FIX("%0, %2")
      "       move %1, %3\n"
      "       sc   %1, %2\n"
      "       beqz %1, 1b\n"
      "       nop\n"
      "       .set pop "
      : "=&r" (oldval), "=&r" (temp), "+m" (*addr)
      : "r" (1)
      : "memory");
  return (MK_AO_TS_VAL_t)oldval;
}
#define MK_AO_HAVE_test_and_set

  /* TODO: Implement MK_AO_and/or/xor primitives directly. */
#endif /* !MK_AO_PREFER_GENERALIZED */

#ifndef MK_AO_GENERALIZE_ASM_BOOL_CAS
  MK_AO_INLINE int
  MK_AO_compare_and_swap(volatile MK_AO_t *addr, MK_AO_t old, MK_AO_t new_val)
  {
    register int was_equal = 0;
    register int temp;

    __asm__ __volatile__(
        "       .set push           \n"
        "       .set mips2          \n"
        "       .set noreorder      \n"
        "       .set nomacro        \n"
        "1:     ll      %0, %1      \n"
        MK_AO_MIPS_LL_FIX("%0, %1")
        "       bne     %0, %4, 2f  \n"
        "        move   %0, %3      \n"
        "       sc      %0, %1      \n"
        "       .set pop            \n"
        "       beqz    %0, 1b      \n"
        "       li      %2, 1       \n"
        "2:                           "
        : "=&r" (temp), "+m" (*addr), "+r" (was_equal)
        : "r" (new_val), "r" (old)
        : "memory");
    return was_equal;
  }
# define MK_AO_HAVE_compare_and_swap
#endif /* !MK_AO_GENERALIZE_ASM_BOOL_CAS */

MK_AO_INLINE MK_AO_t
MK_AO_fetch_compare_and_swap(volatile MK_AO_t *addr, MK_AO_t old, MK_AO_t new_val)
{
  register int fetched_val;
  register int temp;

  __asm__ __volatile__(
      "       .set push\n"
      "       .set mips2\n"
      "       .set noreorder\n"
      "       .set nomacro\n"
      "1:     ll   %0, %2\n"
      MK_AO_MIPS_LL_FIX("%0, %2")
      "       bne  %0, %4, 2f\n"
      "       move %1, %3\n"
      "       sc   %1, %2\n"
      "       beqz %1, 1b\n"
      "       nop\n"
      "       .set pop\n"
      "2:"
      : "=&r" (fetched_val), "=&r" (temp), "+m" (*addr)
      : "r" (new_val), "Jr" (old)
      : "memory");
  return (MK_AO_t)fetched_val;
}
#define MK_AO_HAVE_fetch_compare_and_swap

/* #include "../standard_ao_double_t.h" */
/* TODO: Implement double-wide operations if available. */

/* CAS primitives with acquire, release and full semantics are  */
/* generated automatically (and MK_AO_int_... primitives are       */
/* defined properly after the first generalization pass).       */

/* FIXME: 32-bit ABI is assumed.    */
#define MK_AO_T_IS_INT
