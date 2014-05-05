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

/*
 * This file specifies Itanimum primitives for use with the HP compiler
 * under HP/UX.  We use intrinsics instead of the inline assembly code in the
 * gcc file.
 */

#include "../all_atomic_load_store.h"

#include "../all_acquire_release_volatile.h"

#include "../test_and_set_t_is_char.h"

#include <machine/sys/inline.h>

#ifdef __LP64__
# define MK_AO_T_FASIZE _FASZ_D
# define MK_AO_T_SIZE _SZ_D
#else
# define MK_AO_T_FASIZE _FASZ_W
# define MK_AO_T_SIZE _SZ_W
#endif

MK_AO_INLINE void
MK_AO_nop_full(void)
{
  _Asm_mf();
}
#define MK_AO_HAVE_nop_full

#ifndef MK_AO_PREFER_GENERALIZED
MK_AO_INLINE MK_AO_t
MK_AO_fetch_and_add1_acquire (volatile MK_AO_t *p)
{
  return _Asm_fetchadd(MK_AO_T_FASIZE, _SEM_ACQ, p, 1,
                       _LDHINT_NONE, _DOWN_MEM_FENCE);
}
#define MK_AO_HAVE_fetch_and_add1_acquire

MK_AO_INLINE MK_AO_t
MK_AO_fetch_and_add1_release (volatile MK_AO_t *p)
{
  return _Asm_fetchadd(MK_AO_T_FASIZE, _SEM_REL, p, 1,
                       _LDHINT_NONE, _UP_MEM_FENCE);
}
#define MK_AO_HAVE_fetch_and_add1_release

MK_AO_INLINE MK_AO_t
MK_AO_fetch_and_sub1_acquire (volatile MK_AO_t *p)
{
  return _Asm_fetchadd(MK_AO_T_FASIZE, _SEM_ACQ, p, -1,
                       _LDHINT_NONE, _DOWN_MEM_FENCE);
}
#define MK_AO_HAVE_fetch_and_sub1_acquire

MK_AO_INLINE MK_AO_t
MK_AO_fetch_and_sub1_release (volatile MK_AO_t *p)
{
  return _Asm_fetchadd(MK_AO_T_FASIZE, _SEM_REL, p, -1,
                       _LDHINT_NONE, _UP_MEM_FENCE);
}
#define MK_AO_HAVE_fetch_and_sub1_release
#endif /* !MK_AO_PREFER_GENERALIZED */

MK_AO_INLINE MK_AO_t
MK_AO_fetch_compare_and_swap_acquire(volatile MK_AO_t *addr, MK_AO_t old_val,
                                  MK_AO_t new_val)
{
  _Asm_mov_to_ar(_AREG_CCV, old_val, _DOWN_MEM_FENCE);
  return _Asm_cmpxchg(MK_AO_T_SIZE, _SEM_ACQ, addr,
                      new_val, _LDHINT_NONE, _DOWN_MEM_FENCE);
}
#define MK_AO_HAVE_fetch_compare_and_swap_acquire

MK_AO_INLINE MK_AO_t
MK_AO_fetch_compare_and_swap_release(volatile MK_AO_t *addr, MK_AO_t old_val,
                                  MK_AO_t new_val)
{
  _Asm_mov_to_ar(_AREG_CCV, old_val, _UP_MEM_FENCE);
  return _Asm_cmpxchg(MK_AO_T_SIZE, _SEM_REL, addr,
                      new_val, _LDHINT_NONE, _UP_MEM_FENCE);

}
#define MK_AO_HAVE_fetch_compare_and_swap_release

MK_AO_INLINE unsigned char
MK_AO_char_fetch_compare_and_swap_acquire(volatile unsigned char *addr,
                                unsigned char old_val, unsigned char new_val)
{
  _Asm_mov_to_ar(_AREG_CCV, old_val, _DOWN_MEM_FENCE);
  return _Asm_cmpxchg(_SZ_B, _SEM_ACQ, addr,
                      new_val, _LDHINT_NONE, _DOWN_MEM_FENCE);

}
#define MK_AO_HAVE_char_fetch_compare_and_swap_acquire

MK_AO_INLINE unsigned char
MK_AO_char_fetch_compare_and_swap_release(volatile unsigned char *addr,
                                unsigned char old_val, unsigned char new_val)
{
  _Asm_mov_to_ar(_AREG_CCV, old_val, _UP_MEM_FENCE);
  return _Asm_cmpxchg(_SZ_B, _SEM_REL, addr,
                      new_val, _LDHINT_NONE, _UP_MEM_FENCE);

}
#define MK_AO_HAVE_char_fetch_compare_and_swap_release

MK_AO_INLINE unsigned short
MK_AO_short_fetch_compare_and_swap_acquire(volatile unsigned short *addr,
                                        unsigned short old_val,
                                        unsigned short new_val)
{
  _Asm_mov_to_ar(_AREG_CCV, old_val, _DOWN_MEM_FENCE);
  return _Asm_cmpxchg(_SZ_B, _SEM_ACQ, addr,
                      new_val, _LDHINT_NONE, _DOWN_MEM_FENCE);

}
#define MK_AO_HAVE_short_fetch_compare_and_swap_acquire

MK_AO_INLINE unsigned short
MK_AO_short_fetch_compare_and_swap_release(volatile unsigned short *addr,
                                        unsigned short old_val,
                                        unsigned short new_val)
{
  _Asm_mov_to_ar(_AREG_CCV, old_val, _UP_MEM_FENCE);
  return _Asm_cmpxchg(_SZ_B, _SEM_REL, addr,
                      new_val, _LDHINT_NONE, _UP_MEM_FENCE);

}
#define MK_AO_HAVE_short_fetch_compare_and_swap_release

#ifndef __LP64__
# define MK_AO_T_IS_INT
#endif
