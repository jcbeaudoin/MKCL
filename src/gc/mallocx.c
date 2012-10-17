/*
 * Copyright 1988, 1989 Hans-J. Boehm, Alan J. Demers
 * Copyright (c) 1991-1994 by Xerox Corporation.  All rights reserved.
 * Copyright (c) 1996 by Silicon Graphics.  All rights reserved.
 * Copyright (c) 2000 by Hewlett-Packard Company.  All rights reserved.
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

#include "private/gc_priv.h"

/*
 * These are extra allocation routines which are likely to be less
 * frequently used than those in malloc.c.  They are separate in the
 * hope that the .o file will be excluded from statically linked
 * executables.  We should probably break this up further.
 */

#include <stdio.h>
#include <string.h>

#ifdef MSWINCE
# ifndef WIN32_LEAN_AND_MEAN
#   define WIN32_LEAN_AND_MEAN 1
# endif
# define NOSERVICE
# include <windows.h>
#else
# include <errno.h>
#endif

/* Some externally visible but unadvertised variables to allow access to */
/* free lists from inlined allocators without including gc_priv.h        */
/* or introducing dependencies on internal data structure layouts.       */
void ** const MK_GC_objfreelist_ptr = MK_GC_objfreelist;
void ** const MK_GC_aobjfreelist_ptr = MK_GC_aobjfreelist;
void ** const MK_GC_uobjfreelist_ptr = MK_GC_uobjfreelist;
# ifdef ATOMIC_UNCOLLECTABLE
    void ** const MK_GC_auobjfreelist_ptr = MK_GC_auobjfreelist;
# endif


STATIC void * MK_GC_generic_or_special_malloc(size_t lb, int knd)
{
    switch(knd) {
#     ifdef STUBBORN_ALLOC
        case STUBBORN:
            return(MK_GC_malloc_stubborn((size_t)lb));
#     endif
        case PTRFREE:
            return(MK_GC_malloc_atomic((size_t)lb));
        case NORMAL:
            return(MK_GC_malloc((size_t)lb));
        case UNCOLLECTABLE:
            return(MK_GC_malloc_uncollectable((size_t)lb));
#       ifdef ATOMIC_UNCOLLECTABLE
          case AUNCOLLECTABLE:
            return(MK_GC_malloc_atomic_uncollectable((size_t)lb));
#       endif /* ATOMIC_UNCOLLECTABLE */
        default:
            return(MK_GC_generic_malloc(lb,knd));
    }
}

/* Change the size of the block pointed to by p to contain at least   */
/* lb bytes.  The object may be (and quite likely will be) moved.     */
/* The kind (e.g. atomic) is the same as that of the old.             */
/* Shrinking of large blocks is not implemented well.                 */
MK_GC_API void * MK_GC_CALL MK_GC_realloc(void * p, size_t lb)
{
    struct hblk * h;
    hdr * hhdr;
    size_t sz;   /* Current size in bytes       */
    size_t orig_sz;      /* Original sz in bytes        */
    int obj_kind;

    if (p == 0) return(MK_GC_malloc(lb));  /* Required by ANSI */
    h = HBLKPTR(p);
    hhdr = HDR(h);
    sz = hhdr -> hb_sz;
    obj_kind = hhdr -> hb_obj_kind;
    orig_sz = sz;

    if (sz > MAXOBJBYTES) {
        /* Round it up to the next whole heap block */
          word descr;

          sz = (sz+HBLKSIZE-1) & (~HBLKMASK);
          hhdr -> hb_sz = sz;
          descr = MK_GC_obj_kinds[obj_kind].ok_descriptor;
          if (MK_GC_obj_kinds[obj_kind].ok_relocate_descr) descr += sz;
          hhdr -> hb_descr = descr;
#         ifdef MARK_BIT_PER_OBJ
            MK_GC_ASSERT(hhdr -> hb_inv_sz == LARGE_INV_SZ);
#         else
            MK_GC_ASSERT(hhdr -> hb_large_block &&
                      hhdr -> hb_map[ANY_INDEX] == 1);
#         endif
          if (IS_UNCOLLECTABLE(obj_kind)) MK_GC_non_gc_bytes += (sz - orig_sz);
          /* Extra area is already cleared by MK_GC_alloc_large_and_clear. */
    }
    if (ADD_SLOP(lb) <= sz) {
        if (lb >= (sz >> 1)) {
#           ifdef STUBBORN_ALLOC
                if (obj_kind == STUBBORN) MK_GC_change_stubborn(p);
#           endif
            if (orig_sz > lb) {
              /* Clear unneeded part of object to avoid bogus pointer */
              /* tracing.                                             */
              /* Safe for stubborn objects.                           */
                BZERO(((ptr_t)p) + lb, orig_sz - lb);
            }
            return(p);
        } else {
            /* shrink */
              void * result =
                        MK_GC_generic_or_special_malloc((word)lb, obj_kind);

              if (result == 0) return(0);
                  /* Could also return original object.  But this       */
                  /* gives the client warning of imminent disaster.     */
              BCOPY(p, result, lb);
#             ifndef IGNORE_FREE
                MK_GC_free(p);
#             endif
              return(result);
        }
    } else {
        /* grow */
          void * result =
                MK_GC_generic_or_special_malloc((word)lb, obj_kind);

          if (result == 0) return(0);
          BCOPY(p, result, sz);
#         ifndef IGNORE_FREE
            MK_GC_free(p);
#         endif
          return(result);
    }
}

# if defined(REDIRECT_MALLOC) && !defined(REDIRECT_REALLOC)
#   define REDIRECT_REALLOC MK_GC_realloc
# endif

# ifdef REDIRECT_REALLOC

/* As with malloc, avoid two levels of extra calls here.        */

# define MK_GC_debug_realloc_replacement(p, lb) \
        MK_GC_debug_realloc(p, lb, MK_GC_DBG_RA "unknown", 0)

void * realloc(void * p, size_t lb)
  {
    return(REDIRECT_REALLOC(p, lb));
  }

# undef MK_GC_debug_realloc_replacement
# endif /* REDIRECT_REALLOC */


/* Allocate memory such that only pointers to near the          */
/* beginning of the object are considered.                      */
/* We avoid holding allocation lock while we clear memory.      */
MK_GC_INNER void * MK_GC_generic_malloc_ignore_off_page(size_t lb, int k)
{
    void *result;
    size_t lg;
    size_t lb_rounded;
    word n_blocks;
    MK_GC_bool init;
    DCL_LOCK_STATE;

    if (SMALL_OBJ(lb))
        return(MK_GC_generic_malloc((word)lb, k));
    lg = ROUNDED_UP_GRANULES(lb);
    lb_rounded = GRANULES_TO_BYTES(lg);
    if (lb_rounded < lb)
        return((*MK_GC_get_oom_fn())(lb));
    n_blocks = OBJ_SZ_TO_BLOCKS(lb_rounded);
    init = MK_GC_obj_kinds[k].ok_init;
    if (MK_GC_have_errors) MK_GC_print_all_errors();
    MK_GC_INVOKE_FINALIZERS();
    LOCK();
    result = (ptr_t)MK_GC_alloc_large(ADD_SLOP(lb), k, IGNORE_OFF_PAGE);
    if (0 != result) {
        if (MK_GC_debugging_started) {
            BZERO(result, n_blocks * HBLKSIZE);
        } else {
#           ifdef THREADS
              /* Clear any memory that might be used for GC descriptors */
              /* before we release the lock.                          */
                ((word *)result)[0] = 0;
                ((word *)result)[1] = 0;
                ((word *)result)[GRANULES_TO_WORDS(lg)-1] = 0;
                ((word *)result)[GRANULES_TO_WORDS(lg)-2] = 0;
#           endif
        }
    }
    MK_GC_bytes_allocd += lb_rounded;
    if (0 == result) {
        MK_GC_oom_func oom_fn = MK_GC_oom_fn;
        UNLOCK();
        return((*oom_fn)(lb));
    } else {
        UNLOCK();
        if (init && !MK_GC_debugging_started) {
            BZERO(result, n_blocks * HBLKSIZE);
        }
        return(result);
    }
}

MK_GC_API void * MK_GC_CALL MK_GC_malloc_ignore_off_page(size_t lb)
{
    return((void *)MK_GC_generic_malloc_ignore_off_page(lb, NORMAL));
}

MK_GC_API void * MK_GC_CALL MK_GC_malloc_atomic_ignore_off_page(size_t lb)
{
    return((void *)MK_GC_generic_malloc_ignore_off_page(lb, PTRFREE));
}

/* Increment MK_GC_bytes_allocd from code that doesn't have direct access  */
/* to MK_GC_arrays.                                                        */
MK_GC_API void MK_GC_CALL MK_GC_incr_bytes_allocd(size_t n)
{
    MK_GC_bytes_allocd += n;
}

/* The same for MK_GC_bytes_freed.                         */
MK_GC_API void MK_GC_CALL MK_GC_incr_bytes_freed(size_t n)
{
    MK_GC_bytes_freed += n;
}

# ifdef PARALLEL_MARK
    STATIC volatile signed_word MK_GC_bytes_allocd_tmp = 0;
                        /* Number of bytes of memory allocated since    */
                        /* we released the GC lock.  Instead of         */
                        /* reacquiring the GC lock just to add this in, */
                        /* we add it in the next time we reacquire      */
                        /* the lock.  (Atomically adding it doesn't     */
                        /* work, since we would have to atomically      */
                        /* update it in MK_GC_malloc, which is too         */
                        /* expensive.)                                  */
# endif /* PARALLEL_MARK */

/* Return a list of 1 or more objects of the indicated size, linked     */
/* through the first word in the object.  This has the advantage that   */
/* it acquires the allocation lock only once, and may greatly reduce    */
/* time wasted contending for the allocation lock.  Typical usage would */
/* be in a thread that requires many items of the same size.  It would  */
/* keep its own free list in thread-local storage, and call             */
/* MK_GC_malloc_many or friends to replenish it.  (We do not round up      */
/* object sizes, since a call indicates the intention to consume many   */
/* objects of exactly this size.)                                       */
/* We assume that the size is a multiple of GRANULE_BYTES.              */
/* We return the free-list by assigning it to *result, since it is      */
/* not safe to return, e.g. a linked list of pointer-free objects,      */
/* since the collector would not retain the entire list if it were      */
/* invoked just as we were returning.                                   */
/* Note that the client should usually clear the link field.            */
MK_GC_API void MK_GC_CALL MK_GC_generic_malloc_many(size_t lb, int k, void **result)
{
    void *op;
    void *p;
    void **opp;
    size_t lw;      /* Length in words.     */
    size_t lg;      /* Length in granules.  */
    signed_word my_bytes_allocd = 0;
    struct obj_kind * ok = &(MK_GC_obj_kinds[k]);
    DCL_LOCK_STATE;

    MK_GC_ASSERT(lb != 0 && (lb & (GRANULE_BYTES-1)) == 0);
    if (!SMALL_OBJ(lb)) {
        op = MK_GC_generic_malloc(lb, k);
        if(0 != op) obj_link(op) = 0;
        *result = op;
        return;
    }
    lw = BYTES_TO_WORDS(lb);
    lg = BYTES_TO_GRANULES(lb);
    if (MK_GC_have_errors) MK_GC_print_all_errors();
    MK_GC_INVOKE_FINALIZERS();
    LOCK();
    if (!MK_GC_is_initialized) MK_GC_init();
    /* Do our share of marking work */
      if (MK_GC_incremental && !MK_GC_dont_gc) {
        ENTER_GC();
        MK_GC_collect_a_little_inner(1);
        EXIT_GC();
      }
    /* First see if we can reclaim a page of objects waiting to be */
    /* reclaimed.                                                  */
    {
        struct hblk ** rlh = ok -> ok_reclaim_list;
        struct hblk * hbp;
        hdr * hhdr;

        rlh += lg;
        while ((hbp = *rlh) != 0) {
            hhdr = HDR(hbp);
            *rlh = hhdr -> hb_next;
            MK_GC_ASSERT(hhdr -> hb_sz == lb);
            hhdr -> hb_last_reclaimed = (unsigned short) MK_GC_gc_no;
#           ifdef PARALLEL_MARK
              if (MK_GC_parallel) {
                  signed_word my_bytes_allocd_tmp = MK_GC_bytes_allocd_tmp;

                  MK_GC_ASSERT(my_bytes_allocd_tmp >= 0);
                  /* We only decrement it while holding the GC lock.    */
                  /* Thus we can't accidentally adjust it down in more  */
                  /* than one thread simultaneously.                    */
                  if (my_bytes_allocd_tmp != 0) {
                    (void)MK_AO_fetch_and_add(
                                (volatile void *)(&MK_GC_bytes_allocd_tmp),
                                (MK_AO_t)(-my_bytes_allocd_tmp));
                    MK_GC_bytes_allocd += my_bytes_allocd_tmp;
                  }
                  MK_GC_acquire_mark_lock();
                  ++ MK_GC_fl_builder_count;
                  UNLOCK();
                  MK_GC_release_mark_lock();
              }
#           endif
            op = MK_GC_reclaim_generic(hbp, hhdr, lb,
                                    ok -> ok_init, 0, &my_bytes_allocd);
            if (op != 0) {
              /* We also reclaimed memory, so we need to adjust         */
              /* that count.                                            */
              /* This should be atomic, so the results may be           */
              /* inaccurate.                                            */
              MK_GC_bytes_found += my_bytes_allocd;
#             ifdef PARALLEL_MARK
                if (MK_GC_parallel) {
                  *result = op;
                  (void)MK_AO_fetch_and_add(
                                (volatile MK_AO_t *)(&MK_GC_bytes_allocd_tmp),
                                (MK_AO_t)(my_bytes_allocd));
                  MK_GC_acquire_mark_lock();
                  -- MK_GC_fl_builder_count;
                  if (MK_GC_fl_builder_count == 0) MK_GC_notify_all_builder();
                  MK_GC_release_mark_lock();
                  (void) MK_GC_clear_stack(0);
                  return;
                }
#             endif
              MK_GC_bytes_allocd += my_bytes_allocd;
              goto out;
            }
#           ifdef PARALLEL_MARK
              if (MK_GC_parallel) {
                MK_GC_acquire_mark_lock();
                -- MK_GC_fl_builder_count;
                if (MK_GC_fl_builder_count == 0) MK_GC_notify_all_builder();
                MK_GC_release_mark_lock();
                LOCK();
                /* GC lock is needed for reclaim list access.   We      */
                /* must decrement fl_builder_count before reaquiring GC */
                /* lock.  Hopefully this path is rare.                  */
              }
#           endif
        }
    }
    /* Next try to use prefix of global free list if there is one.      */
    /* We don't refill it, but we need to use it up before allocating   */
    /* a new block ourselves.                                           */
      opp = &(MK_GC_obj_kinds[k].ok_freelist[lg]);
      if ( (op = *opp) != 0 ) {
        *opp = 0;
        my_bytes_allocd = 0;
        for (p = op; p != 0; p = obj_link(p)) {
          my_bytes_allocd += lb;
          if ((word)my_bytes_allocd >= HBLKSIZE) {
            *opp = obj_link(p);
            obj_link(p) = 0;
            break;
          }
        }
        MK_GC_bytes_allocd += my_bytes_allocd;
        goto out;
      }
    /* Next try to allocate a new block worth of objects of this size.  */
    {
        struct hblk *h = MK_GC_allochblk(lb, k, 0);
        if (h != 0) {
          if (IS_UNCOLLECTABLE(k)) MK_GC_set_hdr_marks(HDR(h));
          MK_GC_bytes_allocd += HBLKSIZE - HBLKSIZE % lb;
#         ifdef PARALLEL_MARK
            if (MK_GC_parallel) {
              MK_GC_acquire_mark_lock();
              ++ MK_GC_fl_builder_count;
              UNLOCK();
              MK_GC_release_mark_lock();

              op = MK_GC_build_fl(h, lw,
                        (ok -> ok_init || MK_GC_debugging_started), 0);

              *result = op;
              MK_GC_acquire_mark_lock();
              -- MK_GC_fl_builder_count;
              if (MK_GC_fl_builder_count == 0) MK_GC_notify_all_builder();
              MK_GC_release_mark_lock();
              (void) MK_GC_clear_stack(0);
              return;
            }
#         endif
          op = MK_GC_build_fl(h, lw, (ok -> ok_init || MK_GC_debugging_started), 0);
          goto out;
        }
    }

    /* As a last attempt, try allocating a single object.  Note that    */
    /* this may trigger a collection or expand the heap.                */
      op = MK_GC_generic_malloc_inner(lb, k);
      if (0 != op) obj_link(op) = 0;

  out:
    *result = op;
    UNLOCK();
    (void) MK_GC_clear_stack(0);
}

/* Note that the "atomic" version of this would be unsafe, since the    */
/* links would not be seen by the collector.                            */
MK_GC_API void * MK_GC_CALL MK_GC_malloc_many(size_t lb)
{
    void *result;
    MK_GC_generic_malloc_many((lb + EXTRA_BYTES + GRANULE_BYTES-1)
                           & ~(GRANULE_BYTES-1),
                           NORMAL, &result);
    return result;
}

/* Not well tested nor integrated.      */
/* Debug version is tricky and currently missing.       */
#include <limits.h>

MK_GC_API void * MK_GC_CALL MK_GC_memalign(size_t align, size_t lb)
{
    size_t new_lb;
    size_t offset;
    ptr_t result;

    if (align <= GRANULE_BYTES) return MK_GC_malloc(lb);
    if (align >= HBLKSIZE/2 || lb >= HBLKSIZE/2) {
        if (align > HBLKSIZE) {
          return (*MK_GC_get_oom_fn())(LONG_MAX-1024); /* Fail */
        }
        return MK_GC_malloc(lb <= HBLKSIZE? HBLKSIZE : lb);
            /* Will be HBLKSIZE aligned.        */
    }
    /* We could also try to make sure that the real rounded-up object size */
    /* is a multiple of align.  That would be correct up to HBLKSIZE.      */
    new_lb = lb + align - 1;
    result = MK_GC_malloc(new_lb);
    offset = (word)result % align;
    if (offset != 0) {
        offset = align - offset;
        if (!MK_GC_all_interior_pointers) {
            if (offset >= VALID_OFFSET_SZ) return MK_GC_malloc(HBLKSIZE);
            MK_GC_register_displacement(offset);
        }
    }
    result = (void *) ((ptr_t)result + offset);
    MK_GC_ASSERT((word)result % align == 0);
    return result;
}

/* This one exists largerly to redirect posix_memalign for leaks finding. */
MK_GC_API int MK_GC_CALL MK_GC_posix_memalign(void **memptr, size_t align, size_t lb)
{
  /* Check alignment properly.  */
  if (((align - 1) & align) != 0 || align < sizeof(void *)) {
#   ifdef MSWINCE
      return ERROR_INVALID_PARAMETER;
#   else
      return EINVAL;
#   endif
  }

  if ((*memptr = MK_GC_memalign(align, lb)) == NULL) {
#   ifdef MSWINCE
      return ERROR_NOT_ENOUGH_MEMORY;
#   else
      return ENOMEM;
#   endif
  }
  return 0;
}

#ifdef ATOMIC_UNCOLLECTABLE
  /* Allocate lb bytes of pointerfree, untraced, uncollectable data     */
  /* This is normally roughly equivalent to the system malloc.          */
  /* But it may be useful if malloc is redefined.                       */
  MK_GC_API void * MK_GC_CALL MK_GC_malloc_atomic_uncollectable(size_t lb)
  {
    void *op;
    void **opp;
    size_t lg;
    DCL_LOCK_STATE;

    if( SMALL_OBJ(lb) ) {
        if (EXTRA_BYTES != 0 && lb != 0) lb--;
                  /* We don't need the extra byte, since this won't be  */
                  /* collected anyway.                                  */
        lg = MK_GC_size_map[lb];
        opp = &(MK_GC_auobjfreelist[lg]);
        LOCK();
        if( (op = *opp) != 0 ) {
            *opp = obj_link(op);
            obj_link(op) = 0;
            MK_GC_bytes_allocd += GRANULES_TO_BYTES(lg);
            /* Mark bit was already set while object was on free list. */
            MK_GC_non_gc_bytes += GRANULES_TO_BYTES(lg);
            UNLOCK();
        } else {
            UNLOCK();
            op = (ptr_t)MK_GC_generic_malloc(lb, AUNCOLLECTABLE);
        }
        MK_GC_ASSERT(0 == op || MK_GC_is_marked(op));
        return((void *) op);
    } else {
        hdr * hhdr;

        op = (ptr_t)MK_GC_generic_malloc(lb, AUNCOLLECTABLE);
        if (0 == op) return(0);

        MK_GC_ASSERT(((word)op & (HBLKSIZE - 1)) == 0);
        hhdr = HDR(op);

        LOCK();
        set_mark_bit_from_hdr(hhdr, 0); /* Only object. */
#       ifndef THREADS
          MK_GC_ASSERT(hhdr -> hb_n_marks == 0);
#       endif
        hhdr -> hb_n_marks = 1;
        UNLOCK();
        return((void *) op);
    }
  }
#endif /* ATOMIC_UNCOLLECTABLE */

/* provide a version of strdup() that uses the collector to allocate the
   copy of the string */
MK_GC_API char * MK_GC_CALL MK_GC_strdup(const char *s)
{
  char *copy;
  size_t lb;
  if (s == NULL) return NULL;
  lb = strlen(s) + 1;
  if ((copy = MK_GC_malloc_atomic(lb)) == NULL) {
#   ifndef MSWINCE
      errno = ENOMEM;
#   endif
    return NULL;
  }
# ifndef MSWINCE
    strcpy(copy, s);
# else
    /* strcpy() is deprecated in WinCE */
    memcpy(copy, s, lb);
# endif
  return copy;
}

MK_GC_API char * MK_GC_CALL MK_GC_strndup(const char *str, size_t size)
{
  char *copy;
  size_t len = strlen(str); /* str is expected to be non-NULL  */
  if (len > size)
    len = size;
  copy = MK_GC_malloc_atomic(len + 1);
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

  MK_GC_API wchar_t * MK_GC_CALL MK_GC_wcsdup(const wchar_t *str)
  {
    size_t lb = (wcslen(str) + 1) * sizeof(wchar_t);
    wchar_t *copy = MK_GC_malloc_atomic(lb);
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
