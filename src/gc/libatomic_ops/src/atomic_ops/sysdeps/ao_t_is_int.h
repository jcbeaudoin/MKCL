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

/* Inclusion of this file signifies that MK_AO_t is in fact int.           */
/* Hence any MK_AO_... operation can also serve as MK_AO_int_... operation.   */

#if defined(MK_AO_HAVE_load) && !defined(MK_AO_HAVE_int_load)
# define MK_AO_int_load(addr) \
                (unsigned)MK_AO_load((const volatile MK_AO_t *)(addr))
# define MK_AO_HAVE_int_load
#endif

#if defined(MK_AO_HAVE_store) && !defined(MK_AO_HAVE_int_store)
# define MK_AO_int_store(addr, val) \
                MK_AO_store((volatile MK_AO_t *)(addr), (MK_AO_t)(val))
# define MK_AO_HAVE_int_store
#endif

#if defined(MK_AO_HAVE_fetch_and_add) \
    && !defined(MK_AO_HAVE_int_fetch_and_add)
# define MK_AO_int_fetch_and_add(addr, incr) \
                (unsigned)MK_AO_fetch_and_add((volatile MK_AO_t *)(addr), \
                                                (MK_AO_t)(incr))
# define MK_AO_HAVE_int_fetch_and_add
#endif

#if defined(MK_AO_HAVE_fetch_and_add1) \
    && !defined(MK_AO_HAVE_int_fetch_and_add1)
# define MK_AO_int_fetch_and_add1(addr) \
                (unsigned)MK_AO_fetch_and_add1((volatile MK_AO_t *)(addr))
# define MK_AO_HAVE_int_fetch_and_add1
#endif

#if defined(MK_AO_HAVE_fetch_and_sub1) \
    && !defined(MK_AO_HAVE_int_fetch_and_sub1)
# define MK_AO_int_fetch_and_sub1(addr) \
                (unsigned)MK_AO_fetch_and_sub1((volatile MK_AO_t *)(addr))
# define MK_AO_HAVE_int_fetch_and_sub1
#endif

#if defined(MK_AO_HAVE_and) && !defined(MK_AO_HAVE_int_and)
# define MK_AO_int_and(addr, val) \
                MK_AO_and((volatile MK_AO_t *)(addr), (MK_AO_t)(val))
# define MK_AO_HAVE_int_and
#endif

#if defined(MK_AO_HAVE_or) && !defined(MK_AO_HAVE_int_or)
# define MK_AO_int_or(addr, val) \
                MK_AO_or((volatile MK_AO_t *)(addr), (MK_AO_t)(val))
# define MK_AO_HAVE_int_or
#endif

#if defined(MK_AO_HAVE_xor) && !defined(MK_AO_HAVE_int_xor)
# define MK_AO_int_xor(addr, val) \
                MK_AO_xor((volatile MK_AO_t *)(addr), (MK_AO_t)(val))
# define MK_AO_HAVE_int_xor
#endif

#if defined(MK_AO_HAVE_fetch_compare_and_swap) \
    && !defined(MK_AO_HAVE_int_fetch_compare_and_swap)
# define MK_AO_int_fetch_compare_and_swap(addr, old, new_val) \
        (unsigned)MK_AO_fetch_compare_and_swap((volatile MK_AO_t *)(addr), \
                                                 (MK_AO_t)(old), (MK_AO_t)(new_val))
# define MK_AO_HAVE_int_fetch_compare_and_swap
#endif

#if defined(MK_AO_HAVE_compare_and_swap) \
    && !defined(MK_AO_HAVE_int_compare_and_swap)
# define MK_AO_int_compare_and_swap(addr, old, new_val) \
                MK_AO_compare_and_swap((volatile MK_AO_t *)(addr), \
                                         (MK_AO_t)(old), (MK_AO_t)(new_val))
# define MK_AO_HAVE_int_compare_and_swap
#endif
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

/* Inclusion of this file signifies that MK_AO_t is in fact int.           */
/* Hence any MK_AO_... operation can also serve as MK_AO_int_... operation.   */

#if defined(MK_AO_HAVE_load_full) && !defined(MK_AO_HAVE_int_load_full)
# define MK_AO_int_load_full(addr) \
                (unsigned)MK_AO_load_full((const volatile MK_AO_t *)(addr))
# define MK_AO_HAVE_int_load_full
#endif

#if defined(MK_AO_HAVE_store_full) && !defined(MK_AO_HAVE_int_store_full)
# define MK_AO_int_store_full(addr, val) \
                MK_AO_store_full((volatile MK_AO_t *)(addr), (MK_AO_t)(val))
# define MK_AO_HAVE_int_store_full
#endif

#if defined(MK_AO_HAVE_fetch_and_add_full) \
    && !defined(MK_AO_HAVE_int_fetch_and_add_full)
# define MK_AO_int_fetch_and_add_full(addr, incr) \
                (unsigned)MK_AO_fetch_and_add_full((volatile MK_AO_t *)(addr), \
                                                (MK_AO_t)(incr))
# define MK_AO_HAVE_int_fetch_and_add_full
#endif

#if defined(MK_AO_HAVE_fetch_and_add1_full) \
    && !defined(MK_AO_HAVE_int_fetch_and_add1_full)
# define MK_AO_int_fetch_and_add1_full(addr) \
                (unsigned)MK_AO_fetch_and_add1_full((volatile MK_AO_t *)(addr))
# define MK_AO_HAVE_int_fetch_and_add1_full
#endif

#if defined(MK_AO_HAVE_fetch_and_sub1_full) \
    && !defined(MK_AO_HAVE_int_fetch_and_sub1_full)
# define MK_AO_int_fetch_and_sub1_full(addr) \
                (unsigned)MK_AO_fetch_and_sub1_full((volatile MK_AO_t *)(addr))
# define MK_AO_HAVE_int_fetch_and_sub1_full
#endif

#if defined(MK_AO_HAVE_and_full) && !defined(MK_AO_HAVE_int_and_full)
# define MK_AO_int_and_full(addr, val) \
                MK_AO_and_full((volatile MK_AO_t *)(addr), (MK_AO_t)(val))
# define MK_AO_HAVE_int_and_full
#endif

#if defined(MK_AO_HAVE_or_full) && !defined(MK_AO_HAVE_int_or_full)
# define MK_AO_int_or_full(addr, val) \
                MK_AO_or_full((volatile MK_AO_t *)(addr), (MK_AO_t)(val))
# define MK_AO_HAVE_int_or_full
#endif

#if defined(MK_AO_HAVE_xor_full) && !defined(MK_AO_HAVE_int_xor_full)
# define MK_AO_int_xor_full(addr, val) \
                MK_AO_xor_full((volatile MK_AO_t *)(addr), (MK_AO_t)(val))
# define MK_AO_HAVE_int_xor_full
#endif

#if defined(MK_AO_HAVE_fetch_compare_and_swap_full) \
    && !defined(MK_AO_HAVE_int_fetch_compare_and_swap_full)
# define MK_AO_int_fetch_compare_and_swap_full(addr, old, new_val) \
        (unsigned)MK_AO_fetch_compare_and_swap_full((volatile MK_AO_t *)(addr), \
                                                 (MK_AO_t)(old), (MK_AO_t)(new_val))
# define MK_AO_HAVE_int_fetch_compare_and_swap_full
#endif

#if defined(MK_AO_HAVE_compare_and_swap_full) \
    && !defined(MK_AO_HAVE_int_compare_and_swap_full)
# define MK_AO_int_compare_and_swap_full(addr, old, new_val) \
                MK_AO_compare_and_swap_full((volatile MK_AO_t *)(addr), \
                                         (MK_AO_t)(old), (MK_AO_t)(new_val))
# define MK_AO_HAVE_int_compare_and_swap_full
#endif
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

/* Inclusion of this file signifies that MK_AO_t is in fact int.           */
/* Hence any MK_AO_... operation can also serve as MK_AO_int_... operation.   */

#if defined(MK_AO_HAVE_load_acquire) && !defined(MK_AO_HAVE_int_load_acquire)
# define MK_AO_int_load_acquire(addr) \
                (unsigned)MK_AO_load_acquire((const volatile MK_AO_t *)(addr))
# define MK_AO_HAVE_int_load_acquire
#endif

#if defined(MK_AO_HAVE_store_acquire) && !defined(MK_AO_HAVE_int_store_acquire)
# define MK_AO_int_store_acquire(addr, val) \
                MK_AO_store_acquire((volatile MK_AO_t *)(addr), (MK_AO_t)(val))
# define MK_AO_HAVE_int_store_acquire
#endif

#if defined(MK_AO_HAVE_fetch_and_add_acquire) \
    && !defined(MK_AO_HAVE_int_fetch_and_add_acquire)
# define MK_AO_int_fetch_and_add_acquire(addr, incr) \
                (unsigned)MK_AO_fetch_and_add_acquire((volatile MK_AO_t *)(addr), \
                                                (MK_AO_t)(incr))
# define MK_AO_HAVE_int_fetch_and_add_acquire
#endif

#if defined(MK_AO_HAVE_fetch_and_add1_acquire) \
    && !defined(MK_AO_HAVE_int_fetch_and_add1_acquire)
# define MK_AO_int_fetch_and_add1_acquire(addr) \
                (unsigned)MK_AO_fetch_and_add1_acquire((volatile MK_AO_t *)(addr))
# define MK_AO_HAVE_int_fetch_and_add1_acquire
#endif

#if defined(MK_AO_HAVE_fetch_and_sub1_acquire) \
    && !defined(MK_AO_HAVE_int_fetch_and_sub1_acquire)
# define MK_AO_int_fetch_and_sub1_acquire(addr) \
                (unsigned)MK_AO_fetch_and_sub1_acquire((volatile MK_AO_t *)(addr))
# define MK_AO_HAVE_int_fetch_and_sub1_acquire
#endif

#if defined(MK_AO_HAVE_and_acquire) && !defined(MK_AO_HAVE_int_and_acquire)
# define MK_AO_int_and_acquire(addr, val) \
                MK_AO_and_acquire((volatile MK_AO_t *)(addr), (MK_AO_t)(val))
# define MK_AO_HAVE_int_and_acquire
#endif

#if defined(MK_AO_HAVE_or_acquire) && !defined(MK_AO_HAVE_int_or_acquire)
# define MK_AO_int_or_acquire(addr, val) \
                MK_AO_or_acquire((volatile MK_AO_t *)(addr), (MK_AO_t)(val))
# define MK_AO_HAVE_int_or_acquire
#endif

#if defined(MK_AO_HAVE_xor_acquire) && !defined(MK_AO_HAVE_int_xor_acquire)
# define MK_AO_int_xor_acquire(addr, val) \
                MK_AO_xor_acquire((volatile MK_AO_t *)(addr), (MK_AO_t)(val))
# define MK_AO_HAVE_int_xor_acquire
#endif

#if defined(MK_AO_HAVE_fetch_compare_and_swap_acquire) \
    && !defined(MK_AO_HAVE_int_fetch_compare_and_swap_acquire)
# define MK_AO_int_fetch_compare_and_swap_acquire(addr, old, new_val) \
        (unsigned)MK_AO_fetch_compare_and_swap_acquire((volatile MK_AO_t *)(addr), \
                                                 (MK_AO_t)(old), (MK_AO_t)(new_val))
# define MK_AO_HAVE_int_fetch_compare_and_swap_acquire
#endif

#if defined(MK_AO_HAVE_compare_and_swap_acquire) \
    && !defined(MK_AO_HAVE_int_compare_and_swap_acquire)
# define MK_AO_int_compare_and_swap_acquire(addr, old, new_val) \
                MK_AO_compare_and_swap_acquire((volatile MK_AO_t *)(addr), \
                                         (MK_AO_t)(old), (MK_AO_t)(new_val))
# define MK_AO_HAVE_int_compare_and_swap_acquire
#endif
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

/* Inclusion of this file signifies that MK_AO_t is in fact int.           */
/* Hence any MK_AO_... operation can also serve as MK_AO_int_... operation.   */

#if defined(MK_AO_HAVE_load_release) && !defined(MK_AO_HAVE_int_load_release)
# define MK_AO_int_load_release(addr) \
                (unsigned)MK_AO_load_release((const volatile MK_AO_t *)(addr))
# define MK_AO_HAVE_int_load_release
#endif

#if defined(MK_AO_HAVE_store_release) && !defined(MK_AO_HAVE_int_store_release)
# define MK_AO_int_store_release(addr, val) \
                MK_AO_store_release((volatile MK_AO_t *)(addr), (MK_AO_t)(val))
# define MK_AO_HAVE_int_store_release
#endif

#if defined(MK_AO_HAVE_fetch_and_add_release) \
    && !defined(MK_AO_HAVE_int_fetch_and_add_release)
# define MK_AO_int_fetch_and_add_release(addr, incr) \
                (unsigned)MK_AO_fetch_and_add_release((volatile MK_AO_t *)(addr), \
                                                (MK_AO_t)(incr))
# define MK_AO_HAVE_int_fetch_and_add_release
#endif

#if defined(MK_AO_HAVE_fetch_and_add1_release) \
    && !defined(MK_AO_HAVE_int_fetch_and_add1_release)
# define MK_AO_int_fetch_and_add1_release(addr) \
                (unsigned)MK_AO_fetch_and_add1_release((volatile MK_AO_t *)(addr))
# define MK_AO_HAVE_int_fetch_and_add1_release
#endif

#if defined(MK_AO_HAVE_fetch_and_sub1_release) \
    && !defined(MK_AO_HAVE_int_fetch_and_sub1_release)
# define MK_AO_int_fetch_and_sub1_release(addr) \
                (unsigned)MK_AO_fetch_and_sub1_release((volatile MK_AO_t *)(addr))
# define MK_AO_HAVE_int_fetch_and_sub1_release
#endif

#if defined(MK_AO_HAVE_and_release) && !defined(MK_AO_HAVE_int_and_release)
# define MK_AO_int_and_release(addr, val) \
                MK_AO_and_release((volatile MK_AO_t *)(addr), (MK_AO_t)(val))
# define MK_AO_HAVE_int_and_release
#endif

#if defined(MK_AO_HAVE_or_release) && !defined(MK_AO_HAVE_int_or_release)
# define MK_AO_int_or_release(addr, val) \
                MK_AO_or_release((volatile MK_AO_t *)(addr), (MK_AO_t)(val))
# define MK_AO_HAVE_int_or_release
#endif

#if defined(MK_AO_HAVE_xor_release) && !defined(MK_AO_HAVE_int_xor_release)
# define MK_AO_int_xor_release(addr, val) \
                MK_AO_xor_release((volatile MK_AO_t *)(addr), (MK_AO_t)(val))
# define MK_AO_HAVE_int_xor_release
#endif

#if defined(MK_AO_HAVE_fetch_compare_and_swap_release) \
    && !defined(MK_AO_HAVE_int_fetch_compare_and_swap_release)
# define MK_AO_int_fetch_compare_and_swap_release(addr, old, new_val) \
        (unsigned)MK_AO_fetch_compare_and_swap_release((volatile MK_AO_t *)(addr), \
                                                 (MK_AO_t)(old), (MK_AO_t)(new_val))
# define MK_AO_HAVE_int_fetch_compare_and_swap_release
#endif

#if defined(MK_AO_HAVE_compare_and_swap_release) \
    && !defined(MK_AO_HAVE_int_compare_and_swap_release)
# define MK_AO_int_compare_and_swap_release(addr, old, new_val) \
                MK_AO_compare_and_swap_release((volatile MK_AO_t *)(addr), \
                                         (MK_AO_t)(old), (MK_AO_t)(new_val))
# define MK_AO_HAVE_int_compare_and_swap_release
#endif
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

/* Inclusion of this file signifies that MK_AO_t is in fact int.           */
/* Hence any MK_AO_... operation can also serve as MK_AO_int_... operation.   */

#if defined(MK_AO_HAVE_load_write) && !defined(MK_AO_HAVE_int_load_write)
# define MK_AO_int_load_write(addr) \
                (unsigned)MK_AO_load_write((const volatile MK_AO_t *)(addr))
# define MK_AO_HAVE_int_load_write
#endif

#if defined(MK_AO_HAVE_store_write) && !defined(MK_AO_HAVE_int_store_write)
# define MK_AO_int_store_write(addr, val) \
                MK_AO_store_write((volatile MK_AO_t *)(addr), (MK_AO_t)(val))
# define MK_AO_HAVE_int_store_write
#endif

#if defined(MK_AO_HAVE_fetch_and_add_write) \
    && !defined(MK_AO_HAVE_int_fetch_and_add_write)
# define MK_AO_int_fetch_and_add_write(addr, incr) \
                (unsigned)MK_AO_fetch_and_add_write((volatile MK_AO_t *)(addr), \
                                                (MK_AO_t)(incr))
# define MK_AO_HAVE_int_fetch_and_add_write
#endif

#if defined(MK_AO_HAVE_fetch_and_add1_write) \
    && !defined(MK_AO_HAVE_int_fetch_and_add1_write)
# define MK_AO_int_fetch_and_add1_write(addr) \
                (unsigned)MK_AO_fetch_and_add1_write((volatile MK_AO_t *)(addr))
# define MK_AO_HAVE_int_fetch_and_add1_write
#endif

#if defined(MK_AO_HAVE_fetch_and_sub1_write) \
    && !defined(MK_AO_HAVE_int_fetch_and_sub1_write)
# define MK_AO_int_fetch_and_sub1_write(addr) \
                (unsigned)MK_AO_fetch_and_sub1_write((volatile MK_AO_t *)(addr))
# define MK_AO_HAVE_int_fetch_and_sub1_write
#endif

#if defined(MK_AO_HAVE_and_write) && !defined(MK_AO_HAVE_int_and_write)
# define MK_AO_int_and_write(addr, val) \
                MK_AO_and_write((volatile MK_AO_t *)(addr), (MK_AO_t)(val))
# define MK_AO_HAVE_int_and_write
#endif

#if defined(MK_AO_HAVE_or_write) && !defined(MK_AO_HAVE_int_or_write)
# define MK_AO_int_or_write(addr, val) \
                MK_AO_or_write((volatile MK_AO_t *)(addr), (MK_AO_t)(val))
# define MK_AO_HAVE_int_or_write
#endif

#if defined(MK_AO_HAVE_xor_write) && !defined(MK_AO_HAVE_int_xor_write)
# define MK_AO_int_xor_write(addr, val) \
                MK_AO_xor_write((volatile MK_AO_t *)(addr), (MK_AO_t)(val))
# define MK_AO_HAVE_int_xor_write
#endif

#if defined(MK_AO_HAVE_fetch_compare_and_swap_write) \
    && !defined(MK_AO_HAVE_int_fetch_compare_and_swap_write)
# define MK_AO_int_fetch_compare_and_swap_write(addr, old, new_val) \
        (unsigned)MK_AO_fetch_compare_and_swap_write((volatile MK_AO_t *)(addr), \
                                                 (MK_AO_t)(old), (MK_AO_t)(new_val))
# define MK_AO_HAVE_int_fetch_compare_and_swap_write
#endif

#if defined(MK_AO_HAVE_compare_and_swap_write) \
    && !defined(MK_AO_HAVE_int_compare_and_swap_write)
# define MK_AO_int_compare_and_swap_write(addr, old, new_val) \
                MK_AO_compare_and_swap_write((volatile MK_AO_t *)(addr), \
                                         (MK_AO_t)(old), (MK_AO_t)(new_val))
# define MK_AO_HAVE_int_compare_and_swap_write
#endif
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

/* Inclusion of this file signifies that MK_AO_t is in fact int.           */
/* Hence any MK_AO_... operation can also serve as MK_AO_int_... operation.   */

#if defined(MK_AO_HAVE_load_read) && !defined(MK_AO_HAVE_int_load_read)
# define MK_AO_int_load_read(addr) \
                (unsigned)MK_AO_load_read((const volatile MK_AO_t *)(addr))
# define MK_AO_HAVE_int_load_read
#endif

#if defined(MK_AO_HAVE_store_read) && !defined(MK_AO_HAVE_int_store_read)
# define MK_AO_int_store_read(addr, val) \
                MK_AO_store_read((volatile MK_AO_t *)(addr), (MK_AO_t)(val))
# define MK_AO_HAVE_int_store_read
#endif

#if defined(MK_AO_HAVE_fetch_and_add_read) \
    && !defined(MK_AO_HAVE_int_fetch_and_add_read)
# define MK_AO_int_fetch_and_add_read(addr, incr) \
                (unsigned)MK_AO_fetch_and_add_read((volatile MK_AO_t *)(addr), \
                                                (MK_AO_t)(incr))
# define MK_AO_HAVE_int_fetch_and_add_read
#endif

#if defined(MK_AO_HAVE_fetch_and_add1_read) \
    && !defined(MK_AO_HAVE_int_fetch_and_add1_read)
# define MK_AO_int_fetch_and_add1_read(addr) \
                (unsigned)MK_AO_fetch_and_add1_read((volatile MK_AO_t *)(addr))
# define MK_AO_HAVE_int_fetch_and_add1_read
#endif

#if defined(MK_AO_HAVE_fetch_and_sub1_read) \
    && !defined(MK_AO_HAVE_int_fetch_and_sub1_read)
# define MK_AO_int_fetch_and_sub1_read(addr) \
                (unsigned)MK_AO_fetch_and_sub1_read((volatile MK_AO_t *)(addr))
# define MK_AO_HAVE_int_fetch_and_sub1_read
#endif

#if defined(MK_AO_HAVE_and_read) && !defined(MK_AO_HAVE_int_and_read)
# define MK_AO_int_and_read(addr, val) \
                MK_AO_and_read((volatile MK_AO_t *)(addr), (MK_AO_t)(val))
# define MK_AO_HAVE_int_and_read
#endif

#if defined(MK_AO_HAVE_or_read) && !defined(MK_AO_HAVE_int_or_read)
# define MK_AO_int_or_read(addr, val) \
                MK_AO_or_read((volatile MK_AO_t *)(addr), (MK_AO_t)(val))
# define MK_AO_HAVE_int_or_read
#endif

#if defined(MK_AO_HAVE_xor_read) && !defined(MK_AO_HAVE_int_xor_read)
# define MK_AO_int_xor_read(addr, val) \
                MK_AO_xor_read((volatile MK_AO_t *)(addr), (MK_AO_t)(val))
# define MK_AO_HAVE_int_xor_read
#endif

#if defined(MK_AO_HAVE_fetch_compare_and_swap_read) \
    && !defined(MK_AO_HAVE_int_fetch_compare_and_swap_read)
# define MK_AO_int_fetch_compare_and_swap_read(addr, old, new_val) \
        (unsigned)MK_AO_fetch_compare_and_swap_read((volatile MK_AO_t *)(addr), \
                                                 (MK_AO_t)(old), (MK_AO_t)(new_val))
# define MK_AO_HAVE_int_fetch_compare_and_swap_read
#endif

#if defined(MK_AO_HAVE_compare_and_swap_read) \
    && !defined(MK_AO_HAVE_int_compare_and_swap_read)
# define MK_AO_int_compare_and_swap_read(addr, old, new_val) \
                MK_AO_compare_and_swap_read((volatile MK_AO_t *)(addr), \
                                         (MK_AO_t)(old), (MK_AO_t)(new_val))
# define MK_AO_HAVE_int_compare_and_swap_read
#endif
