/*
 * Copyright 1988, 1989 Hans-J. Boehm, Alan J. Demers
 * Copyright (c) 1991-1994 by Xerox Corporation.  All rights reserved.
 * Copyright 1996 Silicon Graphics.  All rights reserved.
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
 * Some simple primitives for allocation with explicit type information.
 * Facilities for dynamic type inference may be added later.
 * Should be used only for extremely performance critical applications,
 * or if conservative collector leakage is otherwise a problem (unlikely).
 * Note that this is implemented completely separately from the rest
 * of the collector, and is not linked in unless referenced.
 * This does not currently support MK_GC_DEBUG in any interesting way.
 */

#ifndef MK_GC_TYPED_H
#define MK_GC_TYPED_H

#ifndef MK_GC_H
# include "gc.h"
#endif

#ifdef __cplusplus
  extern "C" {
#endif

typedef MK_GC_word * MK_GC_bitmap;
        /* The least significant bit of the first word is one if        */
        /* the first word in the object may be a pointer.               */

#define MK_GC_WORDSZ (8 * sizeof(MK_GC_word))
#define MK_GC_get_bit(bm, index) \
            (((bm)[(index) / MK_GC_WORDSZ] >> ((index) % MK_GC_WORDSZ)) & 1)
#define MK_GC_set_bit(bm, index) \
            ((bm)[(index) / MK_GC_WORDSZ] |= (MK_GC_word)1 << ((index) % MK_GC_WORDSZ))
#define MK_GC_WORD_OFFSET(t, f) (offsetof(t,f) / sizeof(MK_GC_word))
#define MK_GC_WORD_LEN(t) (sizeof(t) / sizeof(MK_GC_word))
#define MK_GC_BITMAP_SIZE(t) ((MK_GC_WORD_LEN(t) + MK_GC_WORDSZ - 1) / MK_GC_WORDSZ)

typedef MK_GC_word MK_GC_descr;

MK_GC_API MK_GC_descr MK_GC_CALL MK_GC_make_descriptor(MK_GC_bitmap /* bm */,
                                           size_t /* len */);
                /* Return a type descriptor for the object whose layout */
                /* is described by the argument.                        */
                /* The least significant bit of the first word is one   */
                /* if the first word in the object may be a pointer.    */
                /* The second argument specifies the number of          */
                /* meaningful bits in the bitmap.  The actual object    */
                /* may be larger (but not smaller).  Any additional     */
                /* words in the object are assumed not to contain       */
                /* pointers.                                            */
                /* Returns a conservative approximation in the          */
                /* (unlikely) case of insufficient memory to build      */
                /* the descriptor.  Calls to MK_GC_make_descriptor         */
                /* may consume some amount of a finite resource.  This  */
                /* is intended to be called once per type, not once     */
                /* per allocation.                                      */

/* It is possible to generate a descriptor for a C type T with  */
/* word aligned pointer fields f1, f2, ... as follows:                  */
/*                                                                      */
/* MK_GC_descr T_descr;                                                    */
/* MK_GC_word T_bitmap[MK_GC_BITMAP_SIZE(T)] = {0};                           */
/* MK_GC_set_bit(T_bitmap, MK_GC_WORD_OFFSET(T,f1));                          */
/* MK_GC_set_bit(T_bitmap, MK_GC_WORD_OFFSET(T,f2));                          */
/* ...                                                                  */
/* T_descr = MK_GC_make_descriptor(T_bitmap, MK_GC_WORD_LEN(T));              */

MK_GC_API void * MK_GC_CALL MK_GC_malloc_explicitly_typed(size_t /* size_in_bytes */,
                                                 MK_GC_descr /* d */);
                /* Allocate an object whose layout is described by d.   */
                /* The resulting object MAY NOT BE PASSED TO REALLOC.   */
                /* The returned object is cleared.                      */

MK_GC_API void * MK_GC_CALL MK_GC_malloc_explicitly_typed_ignore_off_page(
                                        size_t /* size_in_bytes */,
                                        MK_GC_descr /* d */);

MK_GC_API void * MK_GC_CALL MK_GC_calloc_explicitly_typed(size_t /* nelements */,
                                        size_t /* element_size_in_bytes */,
                                        MK_GC_descr /* d */);
        /* Allocate an array of nelements elements, each of the */
        /* given size, and with the given descriptor.           */
        /* The element size must be a multiple of the byte      */
        /* alignment required for pointers.  E.g. on a 32-bit   */
        /* machine with 16-bit aligned pointers, size_in_bytes  */
        /* must be a multiple of 2.                             */
        /* Returned object is cleared.                          */

#ifdef MK_GC_DEBUG
# define MK_GC_MALLOC_EXPLICITLY_TYPED(bytes, d) MK_GC_MALLOC(bytes)
# define MK_GC_CALLOC_EXPLICITLY_TYPED(n, bytes, d) MK_GC_MALLOC((n) * (bytes))
#else
# define MK_GC_MALLOC_EXPLICITLY_TYPED(bytes, d) \
                        MK_GC_malloc_explicitly_typed(bytes, d)
# define MK_GC_CALLOC_EXPLICITLY_TYPED(n, bytes, d) \
                        MK_GC_calloc_explicitly_typed(n, bytes, d)
#endif

#ifdef __cplusplus
  } /* matches extern "C" */
#endif

#endif /* MK_GC_TYPED_H */
