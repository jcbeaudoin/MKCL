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
 * Ensure, if at all possible, that MK_AO_compare_and_swap_full() is
 * available.  The emulation should be brute-force signal-safe, even
 * though it actually blocks.
 * Including this file will generate an error if MK_AO_compare_and_swap_full()
 * cannot be made available.
 * This will be included from platform-specific atomic_ops files
 * if appropriate, and if MK_AO_REQUIRE_CAS is defined.  It should not be
 * included directly, especially since it affects the implementation
 * of other atomic update primitives.
 * The implementation assumes that only MK_AO_store_XXX and MK_AO_test_and_set_XXX
 * variants are defined, and that MK_AO_test_and_set_XXX is not used to
 * operate on compare_and_swap locations.
 */

#ifndef MK_AO_ATOMIC_OPS_H
# error This file should not be included directly.
#endif

#ifndef MK_AO_HAVE_double_t
# include "standard_ao_double_t.h"
#endif

MK_AO_t MK_AO_fetch_compare_and_swap_emulation(volatile MK_AO_t *addr, MK_AO_t old_val,
                                         MK_AO_t new_val);

int MK_AO_compare_double_and_swap_double_emulation(volatile MK_AO_double_t *addr,
                                                MK_AO_t old_val1, MK_AO_t old_val2,
                                                MK_AO_t new_val1, MK_AO_t new_val2);

void MK_AO_store_full_emulation(volatile MK_AO_t *addr, MK_AO_t val);

#ifndef MK_AO_HAVE_fetch_compare_and_swap_full
# define MK_AO_fetch_compare_and_swap_full(addr, old, newval) \
                MK_AO_fetch_compare_and_swap_emulation(addr, old, newval)
# define MK_AO_HAVE_fetch_compare_and_swap_full
#endif

#ifndef MK_AO_HAVE_compare_double_and_swap_double_full
# define MK_AO_compare_double_and_swap_double_full(addr, old1, old2, \
                                                newval1, newval2) \
        MK_AO_compare_double_and_swap_double_emulation(addr, old1, old2, \
                                                    newval1, newval2)
# define MK_AO_HAVE_compare_double_and_swap_double_full
#endif

#undef MK_AO_store
#undef MK_AO_HAVE_store
#undef MK_AO_store_write
#undef MK_AO_HAVE_store_write
#undef MK_AO_store_release
#undef MK_AO_HAVE_store_release
#undef MK_AO_store_full
#undef MK_AO_HAVE_store_full
#define MK_AO_store_full(addr, val) MK_AO_store_full_emulation(addr, val)
#define MK_AO_HAVE_store_full
