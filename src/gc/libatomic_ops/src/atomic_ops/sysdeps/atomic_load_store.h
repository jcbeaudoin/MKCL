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

/* Definitions for architectures on which loads and stores of MK_AO_t are  */
/* atomic for all legal alignments.                                     */

MK_AO_INLINE MK_AO_t
MK_AO_load(const volatile MK_AO_t *addr)
{
  /* Cast away the volatile for architectures like IA64 where   */
  /* volatile adds barrier semantics.                           */
  return (*(const MK_AO_t *)addr);
}
#define MK_AO_HAVE_load

MK_AO_INLINE void
MK_AO_store(volatile MK_AO_t *addr, MK_AO_t new_val)
{
  (*(MK_AO_t *)addr) = new_val;
}
#define MK_AO_HAVE_store
