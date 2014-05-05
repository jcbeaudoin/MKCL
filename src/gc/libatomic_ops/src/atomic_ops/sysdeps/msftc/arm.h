/*
 * Copyright (c) 2003 Hewlett-Packard Development Company, L.P.
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

#ifndef MK_AO_ASSUME_WINDOWS98
  /* CAS is always available */
# define MK_AO_ASSUME_WINDOWS98
#endif
#include "common32_defs.h"
/* FIXME: Do _InterlockedOps really have a full memory barrier?         */
/* (MSDN WinCE docs say nothing about it.)                              */

#include "../test_and_set_t_is_ao_t.h"
/* MK_AO_test_and_set_full() is emulated using CAS.                        */

/* Some ARM slide set, if it has been read correctly, claims that Loads */
/* followed by either a Load or a Store are ordered, but nothing else.  */
/* It is assumed that Windows interrupt handlers clear the LL/SC flag.  */
/* Unaligned accesses are not guaranteed to be atomic.                  */
#include "../all_aligned_atomic_load_store.h"

/* If only a single processor is used, we can define MK_AO_UNIPROCESSOR.   */
#ifdef MK_AO_UNIPROCESSOR
  MK_AO_INLINE void MK_AO_nop_full(void)
  {
    MK_AO_compiler_barrier();
  }
# define MK_AO_HAVE_nop_full
#else
  /* MK_AO_nop_full() is emulated using MK_AO_test_and_set_full().            */
#endif

#if _M_ARM >= 6
/* ARMv6 is the first architecture providing support for simple LL/SC.  */

/* #include "../standard_ao_double_t.h" */
/* TODO: implement double-wide operations (similar to x86).     */

#else /* _M_ARM < 6 */

/* TODO: implement MK_AO_test_and_set_full using SWP.      */

#endif /* _M_ARM < 6 */

#define MK_AO_T_IS_INT
