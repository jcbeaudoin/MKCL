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
 * These are common definitions for architectures that provide processor
 * ordered memory operations except that a later read may pass an
 * earlier write.  Real x86 implementations seem to be in this category,
 * except apparently for some IDT WinChips, which we ignore.
 */

MK_AO_INLINE void
MK_AO_nop_read(void)
{
  MK_AO_compiler_barrier();
}
#define MK_AO_HAVE_nop_read

#ifdef MK_AO_HAVE_load
  MK_AO_INLINE MK_AO_t
  MK_AO_load_read(const volatile MK_AO_t *addr)
  {
    MK_AO_t result = MK_AO_load(addr);
    MK_AO_compiler_barrier();
    return result;
  }
# define MK_AO_HAVE_load_read

# define MK_AO_load_acquire(addr) MK_AO_load_read(addr)
# define MK_AO_HAVE_load_acquire
#endif /* MK_AO_HAVE_load */

#ifdef MK_AO_HAVE_char_load
  MK_AO_INLINE MK_AO_t
  MK_AO_char_load_read(const volatile unsigned char *addr)
  {
    MK_AO_t result = MK_AO_char_load(addr);
    MK_AO_compiler_barrier();
    return result;
  }
# define MK_AO_HAVE_char_load_read

# define MK_AO_char_load_acquire(addr) MK_AO_char_load_read(addr)
# define MK_AO_HAVE_char_load_acquire
#endif /* MK_AO_HAVE_char_load */

#ifdef MK_AO_HAVE_short_load
  MK_AO_INLINE MK_AO_t
  MK_AO_short_load_read(const volatile unsigned short *addr)
  {
    MK_AO_t result = MK_AO_short_load(addr);
    MK_AO_compiler_barrier();
    return result;
  }
# define MK_AO_HAVE_short_load_read

# define MK_AO_short_load_acquire(addr) MK_AO_short_load_read(addr)
# define MK_AO_HAVE_short_load_acquire
#endif /* MK_AO_HAVE_short_load */

#ifdef MK_AO_HAVE_int_load
  MK_AO_INLINE MK_AO_t
  MK_AO_int_load_read(const volatile unsigned int *addr)
  {
    MK_AO_t result = MK_AO_int_load(addr);
    MK_AO_compiler_barrier();
    return result;
  }
# define MK_AO_HAVE_int_load_read

# define MK_AO_int_load_acquire(addr) MK_AO_int_load_read(addr)
# define MK_AO_HAVE_int_load_acquire
#endif /* MK_AO_HAVE_int_load */
