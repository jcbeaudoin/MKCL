/*
 * Copyright 1988, 1989 Hans-J. Boehm, Alan J. Demers
 * Copyright (c) 1991-1995 by Xerox Corporation.  All rights reserved.
 * Copyright (c) 1997 by Silicon Graphics.  All rights reserved.
 * Copyright (c) 1999-2004 Hewlett-Packard Development Company, L.P.
 * Copyright (C) 2007 Free Software Foundation, Inc
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

#include "private/dbg_mlc.h"

#ifndef MSWINCE
# include <errno.h>
#endif
#include <string.h>

#ifndef SHORT_DBG_HDRS
  /* Check whether object with base pointer p has debugging info. */
  /* p is assumed to point to a legitimate object in our part     */
  /* of the heap.                                                 */
  /* This excludes the check as to whether the back pointer is    */
  /* odd, which is added by the MK_GC_HAS_DEBUG_INFO macro.          */
  /* Note that if DBG_HDRS_ALL is set, uncollectable objects      */
  /* on free lists may not have debug information set.  Thus it's */
  /* not always safe to return TRUE (1), even if the client does  */
  /* its part.  Return -1 if the object with debug info has been  */
  /* marked as deallocated.                                       */
  MK_GC_INNER int MK_GC_has_other_debug_info(ptr_t p)
  {
    ptr_t body = (ptr_t)((oh *)p + 1);
    word sz = MK_GC_size(p);

    if (HBLKPTR(p) != HBLKPTR((ptr_t)body)
        || sz < DEBUG_BYTES + EXTRA_BYTES) {
      return 0;
    }
    if (((oh *)p) -> oh_sf != (START_FLAG ^ (word)body)
        && ((word *)p)[BYTES_TO_WORDS(sz)-1] != (END_FLAG ^ (word)body)) {
      return 0;
    }
    if (((oh *)p)->oh_sz == sz) {
      /* Object may have had debug info, but has been deallocated     */
      return -1;
    }
    return 1;
  }
#endif /* !SHORT_DBG_HDRS */

#ifdef KEEP_BACK_PTRS

# include <stdlib.h>

# if defined(__GLIBC__) || defined(SOLARIS) \
     || defined(HPUX) || defined(IRIX5) || defined(OSF1)
#   define RANDOM() random()
# else
#   define RANDOM() (long)rand()
# endif

  /* Store back pointer to source in dest, if that appears to be possible. */
  /* This is not completely safe, since we may mistakenly conclude that    */
  /* dest has a debugging wrapper.  But the error probability is very      */
  /* small, and this shouldn't be used in production code.                 */
  /* We assume that dest is the real base pointer.  Source will usually    */
  /* be a pointer to the interior of an object.                            */
  MK_GC_INNER void MK_GC_store_back_pointer(ptr_t source, ptr_t dest)
  {
    if (MK_GC_HAS_DEBUG_INFO(dest)) {
      ((oh *)dest) -> oh_back_ptr = HIDE_BACK_PTR(source);
    }
  }

  MK_GC_INNER void MK_GC_marked_for_finalization(ptr_t dest)
  {
    MK_GC_store_back_pointer(MARKED_FOR_FINALIZATION, dest);
  }

  /* Store information about the object referencing dest in *base_p     */
  /* and *offset_p.                                                     */
  /*   source is root ==> *base_p = address, *offset_p = 0              */
  /*   source is heap object ==> *base_p != 0, *offset_p = offset       */
  /*   Returns 1 on success, 0 if source couldn't be determined.        */
  /* Dest can be any address within a heap object.                      */
  MK_GC_API MK_GC_ref_kind MK_GC_CALL MK_GC_get_back_ptr_info(void *dest, void **base_p,
                                                  size_t *offset_p)
  {
    oh * hdr = (oh *)MK_GC_base(dest);
    ptr_t bp;
    ptr_t bp_base;

#   ifdef LINT2
      /* Explicitly instruct the code analysis tool that                */
      /* MK_GC_get_back_ptr_info is not expected to be called with an      */
      /* incorrect "dest" value.                                        */
      if (!hdr) ABORT("Invalid MK_GC_get_back_ptr_info argument");
#   endif
    if (!MK_GC_HAS_DEBUG_INFO((ptr_t) hdr)) return MK_GC_NO_SPACE;
    bp = MK_GC_REVEAL_POINTER(hdr -> oh_back_ptr);
    if (MARKED_FOR_FINALIZATION == bp) return MK_GC_FINALIZER_REFD;
    if (MARKED_FROM_REGISTER == bp) return MK_GC_REFD_FROM_REG;
    if (NOT_MARKED == bp) return MK_GC_UNREFERENCED;
#   if ALIGNMENT == 1
      /* Heuristically try to fix off by 1 errors we introduced by      */
      /* insisting on even addresses.                                   */
      {
        ptr_t alternate_ptr = bp + 1;
        ptr_t target = *(ptr_t *)bp;
        ptr_t alternate_target = *(ptr_t *)alternate_ptr;

        if (alternate_target >= MK_GC_least_plausible_heap_addr
            && alternate_target <= MK_GC_greatest_plausible_heap_addr
            && (target < MK_GC_least_plausible_heap_addr
                || target > MK_GC_greatest_plausible_heap_addr)) {
            bp = alternate_ptr;
        }
      }
#   endif
    bp_base = MK_GC_base(bp);
    if (0 == bp_base) {
      *base_p = bp;
      *offset_p = 0;
      return MK_GC_REFD_FROM_ROOT;
    } else {
      if (MK_GC_HAS_DEBUG_INFO(bp_base)) bp_base += sizeof(oh);
      *base_p = bp_base;
      *offset_p = bp - bp_base;
      return MK_GC_REFD_FROM_HEAP;
    }
  }

  /* Generate a random heap address.            */
  /* The resulting address is in the heap, but  */
  /* not necessarily inside a valid object.     */
  MK_GC_API void * MK_GC_CALL MK_GC_generate_random_heap_address(void)
  {
    size_t i;
    word heap_offset = RANDOM();
    if (MK_GC_heapsize > RAND_MAX) {
        heap_offset *= RAND_MAX;
        heap_offset += RANDOM();
    }
    heap_offset %= MK_GC_heapsize;
        /* This doesn't yield a uniform distribution, especially if     */
        /* e.g. RAND_MAX = 1.5* MK_GC_heapsize.  But for typical cases,    */
        /* it's not too bad.                                            */
    for (i = 0; i < MK_GC_n_heap_sects; ++ i) {
        size_t size = MK_GC_heap_sects[i].hs_bytes;
        if (heap_offset < size) {
            return MK_GC_heap_sects[i].hs_start + heap_offset;
        } else {
            heap_offset -= size;
        }
    }
    ABORT("MK_GC_generate_random_heap_address: size inconsistency");
    /*NOTREACHED*/
    return 0;
  }

  /* Generate a random address inside a valid marked heap object. */
  MK_GC_API void * MK_GC_CALL MK_GC_generate_random_valid_address(void)
  {
    ptr_t result;
    ptr_t base;
    do {
      result = MK_GC_generate_random_heap_address();
      base = MK_GC_base(result);
    } while (base == 0 || !MK_GC_is_marked(base));
    return result;
  }

  /* Print back trace for p */
  MK_GC_API void MK_GC_CALL MK_GC_print_backtrace(void *p)
  {
    void *current = p;
    int i;
    MK_GC_ref_kind source;
    size_t offset;
    void *base;

    MK_GC_print_heap_obj(MK_GC_base(current));
    MK_GC_err_printf("\n");
    for (i = 0; ; ++i) {
      source = MK_GC_get_back_ptr_info(current, &base, &offset);
      if (MK_GC_UNREFERENCED == source) {
        MK_GC_err_printf("Reference could not be found\n");
        goto out;
      }
      if (MK_GC_NO_SPACE == source) {
        MK_GC_err_printf("No debug info in object: Can't find reference\n");
        goto out;
      }
      MK_GC_err_printf("Reachable via %d levels of pointers from ", i);
      switch(source) {
        case MK_GC_REFD_FROM_ROOT:
          MK_GC_err_printf("root at %p\n\n", base);
          goto out;
        case MK_GC_REFD_FROM_REG:
          MK_GC_err_printf("root in register\n\n");
          goto out;
        case MK_GC_FINALIZER_REFD:
          MK_GC_err_printf("list of finalizable objects\n\n");
          goto out;
        case MK_GC_REFD_FROM_HEAP:
          MK_GC_err_printf("offset %ld in object:\n", (unsigned long)offset);
          /* Take MK_GC_base(base) to get real base, i.e. header. */
          MK_GC_print_heap_obj(MK_GC_base(base));
          MK_GC_err_printf("\n");
          break;
        default:
          MK_GC_err_printf("INTERNAL ERROR: UNEXPECTED SOURCE!!!!\n");
          goto out;
      }
      current = base;
    }
    out:;
  }

  /* Force a garbage collection and generate a backtrace from a */
  /* random heap address.                                       */
  MK_GC_INNER void MK_GC_generate_random_backtrace_no_gc(void)
  {
    void * current;
    current = MK_GC_generate_random_valid_address();
    MK_GC_printf("\n****Chosen address %p in object\n", current);
    MK_GC_print_backtrace(current);
  }

  MK_GC_API void MK_GC_CALL MK_GC_generate_random_backtrace(void)
  {
    if (MK_GC_try_to_collect(MK_GC_never_stop_func) == 0) {
      MK_GC_err_printf("Cannot generate a backtrace: "
                    "garbage collection is disabled!\n");
      return;
    }
    MK_GC_generate_random_backtrace_no_gc();
  }

#endif /* KEEP_BACK_PTRS */

# define CROSSES_HBLK(p, sz) \
        (((word)(p + sizeof(oh) + sz - 1) ^ (word)p) >= HBLKSIZE)

/* Store debugging info into p.  Return displaced pointer.         */
/* This version assumes we do hold the allocation lock.            */
STATIC ptr_t MK_GC_store_debug_info_inner(ptr_t p, word sz, const char *string,
                                       int linenum)
{
    word * result = (word *)((oh *)p + 1);

    MK_GC_ASSERT(MK_GC_size(p) >= sizeof(oh) + sz);
    MK_GC_ASSERT(!(SMALL_OBJ(sz) && CROSSES_HBLK(p, sz)));
#   ifdef KEEP_BACK_PTRS
      ((oh *)p) -> oh_back_ptr = HIDE_BACK_PTR(NOT_MARKED);
#   endif
#   ifdef MAKE_BACK_GRAPH
      ((oh *)p) -> oh_bg_ptr = HIDE_BACK_PTR((ptr_t)0);
#   endif
    ((oh *)p) -> oh_string = string;
    ((oh *)p) -> oh_int = (word)linenum;
#   ifndef SHORT_DBG_HDRS
      ((oh *)p) -> oh_sz = sz;
      ((oh *)p) -> oh_sf = START_FLAG ^ (word)result;
      ((word *)p)[BYTES_TO_WORDS(MK_GC_size(p))-1] =
         result[SIMPLE_ROUNDED_UP_WORDS(sz)] = END_FLAG ^ (word)result;
#   endif
    return((ptr_t)result);
}

MK_GC_INNER ptr_t MK_GC_store_debug_info(ptr_t p, word sz, const char *string,
                                   int linenum)
{
    ptr_t result;
    DCL_LOCK_STATE;

    LOCK();
    result = MK_GC_store_debug_info_inner(p, sz, string, linenum);
    UNLOCK();
    return result;
}

#ifndef SHORT_DBG_HDRS
  /* Check the object with debugging info at ohdr.      */
  /* Return NULL if it's OK.  Else return clobbered     */
  /* address.                                           */
  STATIC ptr_t MK_GC_check_annotated_obj(oh *ohdr)
  {
    ptr_t body = (ptr_t)(ohdr + 1);
    word gc_sz = MK_GC_size((ptr_t)ohdr);
    if (ohdr -> oh_sz + DEBUG_BYTES > gc_sz) {
        return((ptr_t)(&(ohdr -> oh_sz)));
    }
    if (ohdr -> oh_sf != (START_FLAG ^ (word)body)) {
        return((ptr_t)(&(ohdr -> oh_sf)));
    }
    if (((word *)ohdr)[BYTES_TO_WORDS(gc_sz)-1] != (END_FLAG ^ (word)body)) {
        return((ptr_t)((word *)ohdr + BYTES_TO_WORDS(gc_sz)-1));
    }
    if (((word *)body)[SIMPLE_ROUNDED_UP_WORDS(ohdr -> oh_sz)]
        != (END_FLAG ^ (word)body)) {
        return((ptr_t)((word *)body + SIMPLE_ROUNDED_UP_WORDS(ohdr->oh_sz)));
    }
    return(0);
  }
#endif /* !SHORT_DBG_HDRS */

STATIC MK_GC_describe_type_fn MK_GC_describe_type_fns[MAXOBJKINDS] = {0};

MK_GC_API void MK_GC_CALL MK_GC_register_describe_type_fn(int kind,
                                                 MK_GC_describe_type_fn fn)
{
  MK_GC_describe_type_fns[kind] = fn;
}

/* Print a type description for the object whose client-visible address */
/* is p.                                                                */
STATIC void MK_GC_print_type(ptr_t p)
{
    hdr * hhdr = MK_GC_find_header(p);
    char buffer[MK_GC_TYPE_DESCR_LEN + 1];
    int kind = hhdr -> hb_obj_kind;

    if (0 != MK_GC_describe_type_fns[kind] && MK_GC_is_marked(MK_GC_base(p))) {
        /* This should preclude free list objects except with   */
        /* thread-local allocation.                             */
        buffer[MK_GC_TYPE_DESCR_LEN] = 0;
        (MK_GC_describe_type_fns[kind])(p, buffer);
        MK_GC_ASSERT(buffer[MK_GC_TYPE_DESCR_LEN] == 0);
        MK_GC_err_puts(buffer);
    } else {
        switch(kind) {
          case PTRFREE:
            MK_GC_err_puts("PTRFREE");
            break;
          case NORMAL:
            MK_GC_err_puts("NORMAL");
            break;
          case UNCOLLECTABLE:
            MK_GC_err_puts("UNCOLLECTABLE");
            break;
#         ifdef ATOMIC_UNCOLLECTABLE
            case AUNCOLLECTABLE:
              MK_GC_err_puts("ATOMIC UNCOLLECTABLE");
              break;
#         endif
          case STUBBORN:
            MK_GC_err_puts("STUBBORN");
            break;
          default:
            MK_GC_err_printf("kind=%d descr=0x%lx", kind,
                          (unsigned long)(hhdr -> hb_descr));
        }
    }
}

#define GET_OH_LINENUM(ohdr) ((int)(ohdr)->oh_int)

/* Print a human-readable description of the object to stderr. p points */
/* to somewhere inside an object with the debugging info.               */
STATIC void MK_GC_print_obj(ptr_t p)
{
    oh * ohdr = (oh *)MK_GC_base(p);

    MK_GC_ASSERT(I_DONT_HOLD_LOCK());
#   ifdef LINT2
      if (!ohdr) ABORT("Invalid MK_GC_print_obj argument");
#   endif
    MK_GC_err_printf("%p (", ((ptr_t)ohdr + sizeof(oh)));
    MK_GC_err_puts(ohdr -> oh_string);
#   ifdef SHORT_DBG_HDRS
      MK_GC_err_printf(":%d, ", GET_OH_LINENUM(ohdr));
#   else
      MK_GC_err_printf(":%d, sz=%lu, ",
                    GET_OH_LINENUM(ohdr), (unsigned long)(ohdr -> oh_sz));
#   endif
    MK_GC_print_type((ptr_t)(ohdr + 1));
    MK_GC_err_puts(")\n");
    PRINT_CALL_CHAIN(ohdr);
}

STATIC void MK_GC_debug_print_heap_obj_proc(ptr_t p)
{
    MK_GC_ASSERT(I_DONT_HOLD_LOCK());
    if (MK_GC_HAS_DEBUG_INFO(p)) {
        MK_GC_print_obj(p);
    } else {
        MK_GC_default_print_heap_obj_proc(p);
    }
}

#ifndef SHORT_DBG_HDRS
  /* Use MK_GC_err_printf and friends to print a description of the object */
  /* whose client-visible address is p, and which was smashed at        */
  /* clobbered_addr.                                                    */
  STATIC void MK_GC_print_smashed_obj(const char *msg, ptr_t p,
                                   ptr_t clobbered_addr)
  {
    oh * ohdr = (oh *)MK_GC_base(p);

    MK_GC_ASSERT(I_DONT_HOLD_LOCK());
#   ifdef LINT2
      if (!ohdr) ABORT("Invalid MK_GC_print_smashed_obj argument");
#   endif
    if (clobbered_addr <= (ptr_t)(&(ohdr -> oh_sz))
        || ohdr -> oh_string == 0) {
        MK_GC_err_printf(
                "%s %p in or near object at %p(<smashed>, appr. sz = %lu)\n",
                msg, clobbered_addr, p,
                (unsigned long)(MK_GC_size((ptr_t)ohdr) - DEBUG_BYTES));
    } else {
        MK_GC_err_printf("%s %p in or near object at %p (%s:%d, sz=%lu)\n",
                msg, clobbered_addr, p,
                (word)(ohdr -> oh_string) < HBLKSIZE ? "(smashed string)" :
                ohdr -> oh_string[0] == '\0' ? "EMPTY(smashed?)" :
                                                ohdr -> oh_string,
                GET_OH_LINENUM(ohdr), (unsigned long)(ohdr -> oh_sz));
        PRINT_CALL_CHAIN(ohdr);
    }
  }
#endif

#ifndef SHORT_DBG_HDRS
  STATIC void MK_GC_check_heap_proc (void);
  STATIC void MK_GC_print_all_smashed_proc (void);
#else
  STATIC void MK_GC_do_nothing(void) {}
#endif

MK_GC_INNER void MK_GC_start_debugging(void)
{
# ifndef SHORT_DBG_HDRS
    MK_GC_check_heap = MK_GC_check_heap_proc;
    MK_GC_print_all_smashed = MK_GC_print_all_smashed_proc;
# else
    MK_GC_check_heap = MK_GC_do_nothing;
    MK_GC_print_all_smashed = MK_GC_do_nothing;
# endif
  MK_GC_print_heap_obj = MK_GC_debug_print_heap_obj_proc;
  MK_GC_debugging_started = TRUE;
  MK_GC_register_displacement((word)sizeof(oh));
}

size_t MK_GC_debug_header_size = sizeof(oh);

MK_GC_API void MK_GC_CALL MK_GC_debug_register_displacement(size_t offset)
{
    MK_GC_register_displacement(offset);
    MK_GC_register_displacement((word)sizeof(oh) + offset);
}

MK_GC_API void * MK_GC_CALL MK_GC_debug_malloc(size_t lb, MK_GC_EXTRA_PARAMS)
{
    void * result;
    /* Note that according to malloc() specification, if size is 0 then */
    /* malloc() returns either NULL, or a unique pointer value that can */
    /* later be successfully passed to free(). We always do the latter. */
    result = MK_GC_malloc(lb + DEBUG_BYTES);

    if (result == 0) {
        MK_GC_err_printf("MK_GC_debug_malloc(%lu) returning NULL (",
                      (unsigned long) lb);
        MK_GC_err_puts(s);
        MK_GC_err_printf(":%ld)\n", (unsigned long)i);
        return(0);
    }
    if (!MK_GC_debugging_started) {
        MK_GC_start_debugging();
    }
    ADD_CALL_CHAIN(result, ra);
    return (MK_GC_store_debug_info(result, (word)lb, s, i));
}

MK_GC_API void * MK_GC_CALL MK_GC_debug_malloc_ignore_off_page(size_t lb,
                                                      MK_GC_EXTRA_PARAMS)
{
    void * result = MK_GC_malloc_ignore_off_page(lb + DEBUG_BYTES);

    if (result == 0) {
        MK_GC_err_printf("MK_GC_debug_malloc_ignore_off_page(%lu) returning NULL (",
                       (unsigned long) lb);
        MK_GC_err_puts(s);
        MK_GC_err_printf(":%lu)\n", (unsigned long)i);
        return(0);
    }
    if (!MK_GC_debugging_started) {
        MK_GC_start_debugging();
    }
    ADD_CALL_CHAIN(result, ra);
    return (MK_GC_store_debug_info(result, (word)lb, s, i));
}

MK_GC_API void * MK_GC_CALL MK_GC_debug_malloc_atomic_ignore_off_page(size_t lb,
                                                             MK_GC_EXTRA_PARAMS)
{
    void * result = MK_GC_malloc_atomic_ignore_off_page(lb + DEBUG_BYTES);

    if (result == 0) {
        MK_GC_err_printf("MK_GC_debug_malloc_atomic_ignore_off_page(%lu)"
                      " returning NULL (", (unsigned long)lb);
        MK_GC_err_puts(s);
        MK_GC_err_printf(":%lu)\n", (unsigned long)i);
        return(0);
    }
    if (!MK_GC_debugging_started) {
        MK_GC_start_debugging();
    }
    ADD_CALL_CHAIN(result, ra);
    return (MK_GC_store_debug_info(result, (word)lb, s, i));
}

#ifdef DBG_HDRS_ALL
  /* An allocation function for internal use.  Normally internally      */
  /* allocated objects do not have debug information.  But in this      */
  /* case, we need to make sure that all objects have debug headers.    */
  /* We assume debugging was started in collector initialization, and   */
  /* we already hold the GC lock.                                       */
  MK_GC_INNER void * MK_GC_debug_generic_malloc_inner(size_t lb, int k)
  {
    void * result = MK_GC_generic_malloc_inner(lb + DEBUG_BYTES, k);

    if (result == 0) {
        MK_GC_err_printf("GC internal allocation (%lu bytes) returning NULL\n",
                       (unsigned long) lb);
        return(0);
    }
    ADD_CALL_CHAIN(result, MK_GC_RETURN_ADDR);
    return (MK_GC_store_debug_info_inner(result, (word)lb, "INTERNAL", 0));
  }

  MK_GC_INNER void * MK_GC_debug_generic_malloc_inner_ignore_off_page(size_t lb,
                                                                int k)
  {
    void * result = MK_GC_generic_malloc_inner_ignore_off_page(
                                                lb + DEBUG_BYTES, k);

    if (result == 0) {
        MK_GC_err_printf("GC internal allocation (%lu bytes) returning NULL\n",
                       (unsigned long) lb);
        return(0);
    }
    ADD_CALL_CHAIN(result, MK_GC_RETURN_ADDR);
    return (MK_GC_store_debug_info_inner(result, (word)lb, "INTERNAL", 0));
  }
#endif /* DBG_HDRS_ALL */

#ifdef STUBBORN_ALLOC
  MK_GC_API void * MK_GC_CALL MK_GC_debug_malloc_stubborn(size_t lb, MK_GC_EXTRA_PARAMS)
  {
    void * result = MK_GC_malloc_stubborn(lb + DEBUG_BYTES);

    if (result == 0) {
        MK_GC_err_printf("MK_GC_debug_malloc(%lu) returning NULL (",
                      (unsigned long) lb);
        MK_GC_err_puts(s);
        MK_GC_err_printf(":%lu)\n", (unsigned long)i);
        return(0);
    }
    if (!MK_GC_debugging_started) {
        MK_GC_start_debugging();
    }
    ADD_CALL_CHAIN(result, ra);
    return (MK_GC_store_debug_info(result, (word)lb, s, i));
  }

  MK_GC_API void MK_GC_CALL MK_GC_debug_change_stubborn(void *p)
  {
    void * q = MK_GC_base(p);
    hdr * hhdr;

    if (q == 0) {
        MK_GC_err_printf("Bad argument: %p to MK_GC_debug_change_stubborn\n", p);
        ABORT("MK_GC_debug_change_stubborn: bad arg");
    }
    hhdr = HDR(q);
    if (hhdr -> hb_obj_kind != STUBBORN) {
        MK_GC_err_printf("MK_GC_debug_change_stubborn arg not stubborn: %p\n", p);
        ABORT("MK_GC_debug_change_stubborn: arg not stubborn");
    }
    MK_GC_change_stubborn(q);
  }

  MK_GC_API void MK_GC_CALL MK_GC_debug_end_stubborn_change(void *p)
  {
    void * q = MK_GC_base(p);
    hdr * hhdr;

    if (q == 0) {
        MK_GC_err_printf("Bad argument: %p to MK_GC_debug_end_stubborn_change\n", p);
        ABORT("MK_GC_debug_end_stubborn_change: bad arg");
    }
    hhdr = HDR(q);
    if (hhdr -> hb_obj_kind != STUBBORN) {
        MK_GC_err_printf("debug_end_stubborn_change arg not stubborn: %p\n", p);
        ABORT("MK_GC_debug_end_stubborn_change: arg not stubborn");
    }
    MK_GC_end_stubborn_change(q);
  }

#else /* !STUBBORN_ALLOC */

  MK_GC_API void * MK_GC_CALL MK_GC_debug_malloc_stubborn(size_t lb, MK_GC_EXTRA_PARAMS)
  {
    return MK_GC_debug_malloc(lb, OPT_RA s, i);
  }

  /*ARGSUSED*/
  MK_GC_API void MK_GC_CALL MK_GC_debug_change_stubborn(void *p) {}

  /*ARGSUSED*/
  MK_GC_API void MK_GC_CALL MK_GC_debug_end_stubborn_change(void *p) {}
#endif /* !STUBBORN_ALLOC */

MK_GC_API void * MK_GC_CALL MK_GC_debug_malloc_atomic(size_t lb, MK_GC_EXTRA_PARAMS)
{
    void * result = MK_GC_malloc_atomic(lb + DEBUG_BYTES);

    if (result == 0) {
        MK_GC_err_printf("MK_GC_debug_malloc_atomic(%lu) returning NULL (",
                      (unsigned long) lb);
        MK_GC_err_puts(s);
        MK_GC_err_printf(":%lu)\n", (unsigned long)i);
        return(0);
    }
    if (!MK_GC_debugging_started) {
        MK_GC_start_debugging();
    }
    ADD_CALL_CHAIN(result, ra);
    return (MK_GC_store_debug_info(result, (word)lb, s, i));
}

MK_GC_API char * MK_GC_CALL MK_GC_debug_strdup(const char *str, MK_GC_EXTRA_PARAMS)
{
  char *copy;
  size_t lb;
  if (str == NULL) {
    if (MK_GC_find_leak)
      MK_GC_err_printf("strdup(NULL) behavior is undefined\n");
    return NULL;
  }

  lb = strlen(str) + 1;
  copy = MK_GC_debug_malloc_atomic(lb, OPT_RA s, i);
  if (copy == NULL) {
#   ifndef MSWINCE
      errno = ENOMEM;
#   endif
    return NULL;
  }
# ifndef MSWINCE
    strcpy(copy, str);
# else
    /* strcpy() is deprecated in WinCE */
    memcpy(copy, str, lb);
# endif
  return copy;
}

MK_GC_API char * MK_GC_CALL MK_GC_debug_strndup(const char *str, size_t size,
                                       MK_GC_EXTRA_PARAMS)
{
  char *copy;
  size_t len = strlen(str); /* str is expected to be non-NULL  */
  if (len > size)
    len = size;
  copy = MK_GC_debug_malloc_atomic(len + 1, OPT_RA s, i);
  if (copy == NULL) {
#   ifndef MSWINCE
      errno = ENOMEM;
#   endif
    return NULL;
  }
  BCOPY(str, copy, len);
  copy[len] = '\0';
  return copy;
}

#ifdef MK_GC_REQUIRE_WCSDUP
# include <wchar.h> /* for wcslen() */

  MK_GC_API wchar_t * MK_GC_CALL MK_GC_debug_wcsdup(const wchar_t *str, MK_GC_EXTRA_PARAMS)
  {
    size_t lb = (wcslen(str) + 1) * sizeof(wchar_t);
    wchar_t *copy = MK_GC_debug_malloc_atomic(lb, OPT_RA s, i);
    if (copy == NULL) {
#     ifndef MSWINCE
        errno = ENOMEM;
#     endif
      return NULL;
    }
    BCOPY(str, copy, lb);
    return copy;
  }
#endif /* MK_GC_REQUIRE_WCSDUP */

MK_GC_API void * MK_GC_CALL MK_GC_debug_malloc_uncollectable(size_t lb,
                                                    MK_GC_EXTRA_PARAMS)
{
    void * result = MK_GC_malloc_uncollectable(lb + UNCOLLECTABLE_DEBUG_BYTES);

    if (result == 0) {
        MK_GC_err_printf("MK_GC_debug_malloc_uncollectable(%lu) returning NULL (",
                      (unsigned long) lb);
        MK_GC_err_puts(s);
        MK_GC_err_printf(":%lu)\n", (unsigned long)i);
        return(0);
    }
    if (!MK_GC_debugging_started) {
        MK_GC_start_debugging();
    }
    ADD_CALL_CHAIN(result, ra);
    return (MK_GC_store_debug_info(result, (word)lb, s, i));
}

#ifdef ATOMIC_UNCOLLECTABLE
  MK_GC_API void * MK_GC_CALL MK_GC_debug_malloc_atomic_uncollectable(size_t lb,
                                                             MK_GC_EXTRA_PARAMS)
  {
    void * result =
        MK_GC_malloc_atomic_uncollectable(lb + UNCOLLECTABLE_DEBUG_BYTES);

    if (result == 0) {
        MK_GC_err_printf(
                "MK_GC_debug_malloc_atomic_uncollectable(%lu) returning NULL (",
                (unsigned long) lb);
        MK_GC_err_puts(s);
        MK_GC_err_printf(":%lu)\n", (unsigned long)i);
        return(0);
    }
    if (!MK_GC_debugging_started) {
        MK_GC_start_debugging();
    }
    ADD_CALL_CHAIN(result, ra);
    return (MK_GC_store_debug_info(result, (word)lb, s, i));
  }
#endif /* ATOMIC_UNCOLLECTABLE */

#ifndef MK_GC_FREED_MEM_MARKER
# if CPP_WORDSZ == 32
#   define MK_GC_FREED_MEM_MARKER 0xdeadbeef
# else
#   define MK_GC_FREED_MEM_MARKER MK_GC_WORD_C(0xEFBEADDEdeadbeef)
# endif
#endif

MK_GC_API void MK_GC_CALL MK_GC_debug_free(void * p)
{
    ptr_t base;
    if (0 == p) return;

    base = MK_GC_base(p);
    if (base == 0) {
      MK_GC_err_printf("Attempt to free invalid pointer %p\n", p);
      ABORT("Invalid pointer passed to free()");
    }
    if ((ptr_t)p - (ptr_t)base != sizeof(oh)) {
      MK_GC_err_printf(
               "MK_GC_debug_free called on pointer %p w/o debugging info\n", p);
    } else {
#     ifndef SHORT_DBG_HDRS
        ptr_t clobbered = MK_GC_check_annotated_obj((oh *)base);
        word sz = MK_GC_size(base);
        if (clobbered != 0) {
          MK_GC_have_errors = TRUE;
          if (((oh *)base) -> oh_sz == sz) {
            MK_GC_print_smashed_obj(
                  "MK_GC_debug_free: found previously deallocated (?) object at",
                  p, clobbered);
            return; /* ignore double free */
          } else {
            MK_GC_print_smashed_obj("MK_GC_debug_free: found smashed location at",
                                 p, clobbered);
          }
        }
        /* Invalidate size (mark the object as deallocated) */
        ((oh *)base) -> oh_sz = sz;
#     endif /* SHORT_DBG_HDRS */
    }
    if (MK_GC_find_leak
#       ifndef SHORT_DBG_HDRS
          && ((ptr_t)p - (ptr_t)base != sizeof(oh) || !MK_GC_findleak_delay_free)
#       endif
        ) {
      MK_GC_free(base);
    } else {
      hdr * hhdr = HDR(p);
      if (hhdr -> hb_obj_kind == UNCOLLECTABLE
#         ifdef ATOMIC_UNCOLLECTABLE
            || hhdr -> hb_obj_kind == AUNCOLLECTABLE
#         endif
          ) {
        MK_GC_free(base);
      } else {
        size_t i;
        size_t obj_sz = BYTES_TO_WORDS(hhdr -> hb_sz - sizeof(oh));

        for (i = 0; i < obj_sz; ++i)
          ((word *)p)[i] = MK_GC_FREED_MEM_MARKER;
        MK_GC_ASSERT((word *)p + i == (word *)(base + hhdr -> hb_sz));
      }
    } /* !MK_GC_find_leak */
}

#if defined(THREADS) && defined(DBG_HDRS_ALL)
  /* Used internally; we assume it's called correctly.    */
  MK_GC_INNER void MK_GC_debug_free_inner(void * p)
  {
    ptr_t base = MK_GC_base(p);
    MK_GC_ASSERT((ptr_t)p - (ptr_t)base == sizeof(oh));
#   ifdef LINT2
      if (!base) ABORT("Invalid MK_GC_debug_free_inner argument");
#   endif
#   ifndef SHORT_DBG_HDRS
      /* Invalidate size */
      ((oh *)base) -> oh_sz = MK_GC_size(base);
#   endif
    MK_GC_free_inner(base);
  }
#endif

MK_GC_API void * MK_GC_CALL MK_GC_debug_realloc(void * p, size_t lb, MK_GC_EXTRA_PARAMS)
{
    void * base;
    void * result;
    hdr * hhdr;
    if (p == 0)
      return(MK_GC_debug_malloc(lb, OPT_RA s, i));

    base = MK_GC_base(p);
    if (base == 0) {
        MK_GC_err_printf("Attempt to reallocate invalid pointer %p\n", p);
        ABORT("Invalid pointer passed to realloc()");
    }
    if ((ptr_t)p - (ptr_t)base != sizeof(oh)) {
        MK_GC_err_printf(
              "MK_GC_debug_realloc called on pointer %p w/o debugging info\n", p);
        return(MK_GC_realloc(p, lb));
    }
    hhdr = HDR(base);
    switch (hhdr -> hb_obj_kind) {
#    ifdef STUBBORN_ALLOC
      case STUBBORN:
        result = MK_GC_debug_malloc_stubborn(lb, OPT_RA s, i);
        break;
#    endif
      case NORMAL:
        result = MK_GC_debug_malloc(lb, OPT_RA s, i);
        break;
      case PTRFREE:
        result = MK_GC_debug_malloc_atomic(lb, OPT_RA s, i);
        break;
      case UNCOLLECTABLE:
        result = MK_GC_debug_malloc_uncollectable(lb, OPT_RA s, i);
        break;
#    ifdef ATOMIC_UNCOLLECTABLE
      case AUNCOLLECTABLE:
        result = MK_GC_debug_malloc_atomic_uncollectable(lb, OPT_RA s, i);
        break;
#    endif
      default:
        result = NULL; /* initialized to prevent warning. */
        MK_GC_err_printf("MK_GC_debug_realloc: encountered bad kind\n");
        ABORT("Bad kind");
    }

    if (result != NULL) {
      size_t old_sz;
#     ifdef SHORT_DBG_HDRS
        old_sz = MK_GC_size(base) - sizeof(oh);
#     else
        old_sz = ((oh *)base) -> oh_sz;
#     endif
      BCOPY(p, result, old_sz < lb ? old_sz : lb);
      MK_GC_debug_free(p);
    }
    return(result);
}

#ifndef SHORT_DBG_HDRS

/* List of smashed (clobbered) locations.  We defer printing these,     */
/* since we can't always print them nicely with the allocation lock     */
/* held.  We put them here instead of in MK_GC_arrays, since it may be     */
/* useful to be able to look at them with the debugger.                 */
#ifndef MAX_SMASHED
# define MAX_SMASHED 20
#endif
STATIC ptr_t MK_GC_smashed[MAX_SMASHED] = {0};
STATIC unsigned MK_GC_n_smashed = 0;

STATIC void MK_GC_add_smashed(ptr_t smashed)
{
    MK_GC_ASSERT(MK_GC_is_marked(MK_GC_base(smashed)));
    /* FIXME: Prevent adding an object while printing smashed list.     */
    MK_GC_smashed[MK_GC_n_smashed] = smashed;
    if (MK_GC_n_smashed < MAX_SMASHED - 1) ++MK_GC_n_smashed;
      /* In case of overflow, we keep the first MAX_SMASHED-1   */
      /* entries plus the last one.                             */
    MK_GC_have_errors = TRUE;
}

/* Print all objects on the list.  Clear the list.      */
STATIC void MK_GC_print_all_smashed_proc(void)
{
    unsigned i;

    MK_GC_ASSERT(I_DONT_HOLD_LOCK());
    if (MK_GC_n_smashed == 0) return;
    MK_GC_err_printf("MK_GC_check_heap_block: found smashed heap objects:\n");
    for (i = 0; i < MK_GC_n_smashed; ++i) {
        MK_GC_print_smashed_obj("", (ptr_t)MK_GC_base(MK_GC_smashed[i]) + sizeof(oh),
                             MK_GC_smashed[i]);
        MK_GC_smashed[i] = 0;
    }
    MK_GC_n_smashed = 0;
    MK_GC_err_printf("\n");
}

/* Check all marked objects in the given block for validity     */
/* Avoid MK_GC_apply_to_each_object for performance reasons.       */
/*ARGSUSED*/
STATIC void MK_GC_check_heap_block(struct hblk *hbp, word dummy)
{
    struct hblkhdr * hhdr = HDR(hbp);
    size_t sz = hhdr -> hb_sz;
    size_t bit_no;
    char *p, *plim;

    p = hbp->hb_body;
    if (sz > MAXOBJBYTES) {
      plim = p;
    } else {
      plim = hbp->hb_body + HBLKSIZE - sz;
    }
    /* go through all words in block */
    for (bit_no = 0; p <= plim; bit_no += MARK_BIT_OFFSET(sz), p += sz) {
      if (mark_bit_from_hdr(hhdr, bit_no) && MK_GC_HAS_DEBUG_INFO((ptr_t)p)) {
        ptr_t clobbered = MK_GC_check_annotated_obj((oh *)p);
        if (clobbered != 0)
          MK_GC_add_smashed(clobbered);
      }
    }
}

/* This assumes that all accessible objects are marked, and that        */
/* I hold the allocation lock.  Normally called by collector.           */
STATIC void MK_GC_check_heap_proc(void)
{
  MK_GC_STATIC_ASSERT((sizeof(oh) & (GRANULE_BYTES - 1)) == 0);
  /* FIXME: Should we check for twice that alignment?   */
  MK_GC_apply_to_all_blocks(MK_GC_check_heap_block, 0);
}

MK_GC_INNER MK_GC_bool MK_GC_check_leaked(ptr_t base)
{
  size_t i;
  size_t obj_sz;
  word *p;

  if (
#     if defined(KEEP_BACK_PTRS) || defined(MAKE_BACK_GRAPH)
        (*(word *)base & 1) != 0 &&
#     endif
      MK_GC_has_other_debug_info(base) >= 0)
    return TRUE; /* object has leaked */

  /* Validate freed object's content. */
  p = (word *)(base + sizeof(oh));
  obj_sz = BYTES_TO_WORDS(HDR(base)->hb_sz - sizeof(oh));
  for (i = 0; i < obj_sz; ++i)
    if (p[i] != MK_GC_FREED_MEM_MARKER) {
        MK_GC_set_mark_bit(base); /* do not reclaim it in this cycle */
        MK_GC_add_smashed((ptr_t)(&p[i])); /* alter-after-free detected */
        break; /* don't report any other smashed locations in the object */
    }

  return FALSE; /* MK_GC_debug_free() has been called */
}

#endif /* !SHORT_DBG_HDRS */

struct closure {
    MK_GC_finalization_proc cl_fn;
    void * cl_data;
};

STATIC void * MK_GC_make_closure(MK_GC_finalization_proc fn, void * data)
{
    struct closure * result =
#   ifdef DBG_HDRS_ALL
      (struct closure *) MK_GC_debug_malloc(sizeof (struct closure),
                                         MK_GC_EXTRAS);
#   else
      (struct closure *) MK_GC_malloc(sizeof (struct closure));
#   endif
    if (result != 0) {
      result -> cl_fn = fn;
      result -> cl_data = data;
    }
    return((void *)result);
}

/* An auxiliary fns to make finalization work correctly with displaced  */
/* pointers introduced by the debugging allocators.                     */
STATIC void MK_GC_CALLBACK MK_GC_debug_invoke_finalizer(void * obj, void * data)
{
    struct closure * cl = (struct closure *) data;
    (*(cl -> cl_fn))((void *)((char *)obj + sizeof(oh)), cl -> cl_data);
}

/* Special finalizer_proc value to detect MK_GC_register_finalizer() failure. */
#define OFN_UNSET (MK_GC_finalization_proc)(signed_word)-1

/* Set ofn and ocd to reflect the values we got back.   */
static void store_old(void *obj, MK_GC_finalization_proc my_old_fn,
                      struct closure *my_old_cd, MK_GC_finalization_proc *ofn,
                      void **ocd)
{
    if (0 != my_old_fn) {
      if (my_old_fn == OFN_UNSET) {
        /* register_finalizer() failed; (*ofn) and (*ocd) are unchanged. */
        return;
      }
      if (my_old_fn != MK_GC_debug_invoke_finalizer) {
        MK_GC_err_printf("Debuggable object at %p had a non-debug finalizer\n",
                      obj);
        /* This should probably be fatal. */
      } else {
        if (ofn) *ofn = my_old_cd -> cl_fn;
        if (ocd) *ocd = my_old_cd -> cl_data;
      }
    } else {
      if (ofn) *ofn = 0;
      if (ocd) *ocd = 0;
    }
}

MK_GC_API void MK_GC_CALL MK_GC_debug_register_finalizer(void * obj,
                                        MK_GC_finalization_proc fn,
                                        void * cd, MK_GC_finalization_proc *ofn,
                                        void * *ocd)
{
    MK_GC_finalization_proc my_old_fn = OFN_UNSET;
    void * my_old_cd;
    ptr_t base = MK_GC_base(obj);
    if (0 == base) {
        /* We won't collect it, hence finalizer wouldn't be run. */
        if (ocd) *ocd = 0;
        if (ofn) *ofn = 0;
        return;
    }
    if ((ptr_t)obj - base != sizeof(oh)) {
        MK_GC_err_printf(
            "MK_GC_debug_register_finalizer called with non-base-pointer %p\n",
            obj);
    }
    if (0 == fn) {
      MK_GC_register_finalizer(base, 0, 0, &my_old_fn, &my_old_cd);
    } else {
      cd = MK_GC_make_closure(fn, cd);
      if (cd == 0) return; /* out of memory */
      MK_GC_register_finalizer(base, MK_GC_debug_invoke_finalizer,
                            cd, &my_old_fn, &my_old_cd);
    }
    store_old(obj, my_old_fn, (struct closure *)my_old_cd, ofn, ocd);
}

MK_GC_API void MK_GC_CALL MK_GC_debug_register_finalizer_no_order
                                    (void * obj, MK_GC_finalization_proc fn,
                                     void * cd, MK_GC_finalization_proc *ofn,
                                     void * *ocd)
{
    MK_GC_finalization_proc my_old_fn = OFN_UNSET;
    void * my_old_cd;
    ptr_t base = MK_GC_base(obj);
    if (0 == base) {
        /* We won't collect it, hence finalizer wouldn't be run. */
        if (ocd) *ocd = 0;
        if (ofn) *ofn = 0;
        return;
    }
    if ((ptr_t)obj - base != sizeof(oh)) {
        MK_GC_err_printf(
          "MK_GC_debug_register_finalizer_no_order called with "
          "non-base-pointer %p\n",
          obj);
    }
    if (0 == fn) {
      MK_GC_register_finalizer_no_order(base, 0, 0, &my_old_fn, &my_old_cd);
    } else {
      cd = MK_GC_make_closure(fn, cd);
      if (cd == 0) return; /* out of memory */
      MK_GC_register_finalizer_no_order(base, MK_GC_debug_invoke_finalizer,
                                     cd, &my_old_fn, &my_old_cd);
    }
    store_old(obj, my_old_fn, (struct closure *)my_old_cd, ofn, ocd);
}

MK_GC_API void MK_GC_CALL MK_GC_debug_register_finalizer_unreachable
                                    (void * obj, MK_GC_finalization_proc fn,
                                     void * cd, MK_GC_finalization_proc *ofn,
                                     void * *ocd)
{
    MK_GC_finalization_proc my_old_fn = OFN_UNSET;
    void * my_old_cd;
    ptr_t base = MK_GC_base(obj);
    if (0 == base) {
        /* We won't collect it, hence finalizer wouldn't be run. */
        if (ocd) *ocd = 0;
        if (ofn) *ofn = 0;
        return;
    }
    if ((ptr_t)obj - base != sizeof(oh)) {
        MK_GC_err_printf(
            "MK_GC_debug_register_finalizer_unreachable called with "
            "non-base-pointer %p\n",
            obj);
    }
    if (0 == fn) {
      MK_GC_register_finalizer_unreachable(base, 0, 0, &my_old_fn, &my_old_cd);
    } else {
      cd = MK_GC_make_closure(fn, cd);
      if (cd == 0) return; /* out of memory */
      MK_GC_register_finalizer_unreachable(base, MK_GC_debug_invoke_finalizer,
                                        cd, &my_old_fn, &my_old_cd);
    }
    store_old(obj, my_old_fn, (struct closure *)my_old_cd, ofn, ocd);
}

MK_GC_API void MK_GC_CALL MK_GC_debug_register_finalizer_ignore_self
                                    (void * obj, MK_GC_finalization_proc fn,
                                     void * cd, MK_GC_finalization_proc *ofn,
                                     void * *ocd)
{
    MK_GC_finalization_proc my_old_fn = OFN_UNSET;
    void * my_old_cd;
    ptr_t base = MK_GC_base(obj);
    if (0 == base) {
        /* We won't collect it, hence finalizer wouldn't be run. */
        if (ocd) *ocd = 0;
        if (ofn) *ofn = 0;
        return;
    }
    if ((ptr_t)obj - base != sizeof(oh)) {
        MK_GC_err_printf(
            "MK_GC_debug_register_finalizer_ignore_self called with "
            "non-base-pointer %p\n", obj);
    }
    if (0 == fn) {
      MK_GC_register_finalizer_ignore_self(base, 0, 0, &my_old_fn, &my_old_cd);
    } else {
      cd = MK_GC_make_closure(fn, cd);
      if (cd == 0) return; /* out of memory */
      MK_GC_register_finalizer_ignore_self(base, MK_GC_debug_invoke_finalizer,
                                        cd, &my_old_fn, &my_old_cd);
    }
    store_old(obj, my_old_fn, (struct closure *)my_old_cd, ofn, ocd);
}

MK_GC_API void * MK_GC_CALL MK_GC_debug_malloc_replacement(size_t lb)
{
    return MK_GC_debug_malloc(lb, MK_GC_DBG_RA "unknown", 0);
}

MK_GC_API void * MK_GC_CALL MK_GC_debug_realloc_replacement(void *p, size_t lb)
{
    return MK_GC_debug_realloc(p, lb, MK_GC_DBG_RA "unknown", 0);
}
