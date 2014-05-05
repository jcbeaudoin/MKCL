/*
 * The implementation of the routines described here is covered by the GPL.
 * This header file is covered by the following license:
 */

/*
 * Copyright (c) 2005 Hewlett-Packard Development Company, L.P.
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

/* Almost lock-free LIFO linked lists (linked stacks).  */
#ifndef MK_AO_STACK_H
#define MK_AO_STACK_H

#include "atomic_ops.h"

#if !defined(MK_AO_HAVE_compare_double_and_swap_double) \
    && !defined(MK_AO_HAVE_compare_double_and_swap) \
    && defined(MK_AO_HAVE_compare_and_swap)
# define MK_AO_USE_ALMOST_LOCK_FREE
#else
  /* If we have no compare-and-swap operation defined, we assume        */
  /* that we will actually be using CAS emulation.  If we do that,      */
  /* it's cheaper to use the version-based implementation.              */
# define MK_AO_STACK_IS_LOCK_FREE
#endif

/*
 * These are not guaranteed to be completely lock-free.
 * List insertion may spin under extremely unlikely conditions.
 * It cannot deadlock due to recursive reentry unless MK_AO_list_remove
 * is called while at least MK_AO_BL_SIZE activations of
 * MK_AO_list_remove are currently active in the same thread, i.e.
 * we must have at least MK_AO_BL_SIZE recursive signal handler
 * invocations.
 *
 * All operations take an MK_AO_list_aux argument.  It is safe to
 * share a single MK_AO_list_aux structure among all lists, but that
 * may increase contention.  Any given list must always be accessed
 * with the same MK_AO_list_aux structure.
 *
 * We make some machine-dependent assumptions:
 *   - We have a compare-and-swap operation.
 *   - At least _MK_AO_N_BITS low order bits in pointers are
 *     zero and normally unused.
 *   - size_t and pointers have the same size.
 *
 * We do use a fully lock-free implementation if double-width
 * compare-and-swap operations are available.
 */

#ifdef MK_AO_USE_ALMOST_LOCK_FREE
/* The number of low order pointer bits we can use for a small  */
/* version number.                                              */
# if defined(__LP64__) || defined(_LP64) || defined(_WIN64)
   /* WIN64 isn't really supported yet. */
#  define MK_AO_N_BITS 3
# else
#  define MK_AO_N_BITS 2
# endif

# define MK_AO_BIT_MASK ((1 << MK_AO_N_BITS) - 1)
/*
 * MK_AO_stack_aux should be treated as opaque.
 * It is fully defined here, so it can be allocated, and to facilitate
 * debugging.
 */
#ifndef MK_AO_BL_SIZE
#  define MK_AO_BL_SIZE 2
#endif

#if MK_AO_BL_SIZE > (1 << MK_AO_N_BITS)
#  error MK_AO_BL_SIZE too big
#endif

typedef struct MK_AO__stack_aux {
  volatile MK_AO_t MK_AO_stack_bl[MK_AO_BL_SIZE];
} MK_AO_stack_aux;

/* The stack implementation knows only about the location of    */
/* link fields in nodes, and nothing about the rest of the      */
/* stack elements.  Link fields hold an MK_AO_t, which is not      */
/* necessarily a real pointer.  This converts the MK_AO_t to a     */
/* real (MK_AO_t *) which is either o, or points at the link       */
/* field in the next node.                                      */
#define MK_AO_REAL_NEXT_PTR(x) (MK_AO_t *)((x) & ~MK_AO_BIT_MASK)

/* The following two routines should not normally be used directly.     */
/* We make them visible here for the rare cases in which it makes sense */
/* to share the an MK_AO_stack_aux between stacks.                         */
void
MK_AO_stack_push_explicit_aux_release(volatile MK_AO_t *list, MK_AO_t *x,
                                  MK_AO_stack_aux *);

MK_AO_t *
MK_AO_stack_pop_explicit_aux_acquire(volatile MK_AO_t *list, MK_AO_stack_aux *);

/* And now MK_AO_stack_t for the real interface:                           */

typedef struct MK_AO__stack {
  volatile MK_AO_t MK_AO_ptr;
  MK_AO_stack_aux MK_AO_aux;
} MK_AO_stack_t;

#define MK_AO_STACK_INITIALIZER {0,{{0}}}

MK_AO_INLINE void MK_AO_stack_init(MK_AO_stack_t *list)
{
# if MK_AO_BL_SIZE == 2
    list -> MK_AO_aux.MK_AO_stack_bl[0] = 0;
    list -> MK_AO_aux.MK_AO_stack_bl[1] = 0;
# else
    int i;
    for (i = 0; i < MK_AO_BL_SIZE; ++i)
      list -> MK_AO_aux.MK_AO_stack_bl[i] = 0;
# endif
  list -> MK_AO_ptr = 0;
}

/* Convert an MK_AO_stack_t to a pointer to the link field in      */
/* the first element.                                           */
#define MK_AO_REAL_HEAD_PTR(x) MK_AO_REAL_NEXT_PTR((x).MK_AO_ptr)

#define MK_AO_stack_push_release(l, e) \
        MK_AO_stack_push_explicit_aux_release(&((l)->MK_AO_ptr), e, &((l)->MK_AO_aux))
#define MK_AO_HAVE_stack_push_release

#define MK_AO_stack_pop_acquire(l) \
        MK_AO_stack_pop_explicit_aux_acquire(&((l)->MK_AO_ptr), &((l)->MK_AO_aux))
#define MK_AO_HAVE_stack_pop_acquire

# else /* Use fully non-blocking data structure, wide CAS       */

#ifndef MK_AO_HAVE_double_t
  /* Can happen if we're using CAS emulation, since we don't want to    */
  /* force that here, in case other atomic_ops clients don't want it.   */
# include "atomic_ops/sysdeps/standard_ao_double_t.h"
#endif

typedef volatile MK_AO_double_t MK_AO_stack_t;
/* MK_AO_val1 is version, MK_AO_val2 is pointer.      */

#define MK_AO_STACK_INITIALIZER MK_AO_DOUBLE_T_INITIALIZER

MK_AO_INLINE void MK_AO_stack_init(MK_AO_stack_t *list)
{
  list -> MK_AO_val1 = 0;
  list -> MK_AO_val2 = 0;
}

#define MK_AO_REAL_HEAD_PTR(x) (MK_AO_t *)((x).MK_AO_val2)
#define MK_AO_REAL_NEXT_PTR(x) (MK_AO_t *)(x)

void MK_AO_stack_push_release(MK_AO_stack_t *list, MK_AO_t *new_element);
#define MK_AO_HAVE_stack_push_release
MK_AO_t * MK_AO_stack_pop_acquire(MK_AO_stack_t *list);
#define MK_AO_HAVE_stack_pop_acquire

#endif /* Wide CAS case */

#if defined(MK_AO_HAVE_stack_push_release) && !defined(MK_AO_HAVE_stack_push)
# define MK_AO_stack_push(l, e) MK_AO_stack_push_release(l, e)
# define MK_AO_HAVE_stack_push
#endif

#if defined(MK_AO_HAVE_stack_pop_acquire) && !defined(MK_AO_HAVE_stack_pop)
# define MK_AO_stack_pop(l) MK_AO_stack_pop_acquire(l)
# define MK_AO_HAVE_stack_pop
#endif

#endif /* !MK_AO_STACK_H */
