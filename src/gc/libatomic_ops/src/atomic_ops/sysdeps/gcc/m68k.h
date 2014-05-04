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

/* The cas instruction causes an emulation trap for the */
/* 060 with a misaligned pointer, so let's avoid this.  */
#undef MK_AO_t
typedef unsigned long MK_AO_t __attribute__ ((aligned (4)));

/* FIXME.  Very incomplete.  */
#include "../all_aligned_atomic_load_store.h"

/* Are there any m68k multiprocessors still around?     */
/* AFAIK, Alliants were sequentially consistent.        */
#include "../ordered.h"

#include "../test_and_set_t_is_char.h"

/* Contributed by Tony Mantler or new.  Should be changed to MIT license? */
MK_AO_INLINE MK_AO_TS_VAL_t
MK_AO_test_and_set_full(volatile MK_AO_TS_t *addr) {
  MK_AO_TS_t oldval;

  /* The value at addr is semi-phony.   */
  /* 'tas' sets bit 7 while the return  */
  /* value pretends all bits were set,  */
  /* which at least matches MK_AO_TS_SET.  */
  __asm__ __volatile__(
                "tas %1; sne %0"
                : "=d" (oldval), "=m" (*addr)
                : "m" (*addr)
                : "memory");
  /* This cast works due to the above.  */
  return (MK_AO_TS_VAL_t)oldval;
}
#define MK_AO_HAVE_test_and_set_full

/* Returns nonzero if the comparison succeeded. */
MK_AO_INLINE int
MK_AO_compare_and_swap_full(volatile MK_AO_t *addr,
                         MK_AO_t old, MK_AO_t new_val)
{
  char result;

  __asm__ __volatile__(
                "cas.l %3,%4,%1; seq %0"
                : "=d" (result), "=m" (*addr)
                : "m" (*addr), "d" (old), "d" (new_val)
                : "memory");
  return -result;
}
#define MK_AO_HAVE_compare_and_swap_full

#include "../ao_t_is_int.h"
