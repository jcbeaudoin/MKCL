/*
 * Copyright 1988, 1989 Hans-J. Boehm, Alan J. Demers
 * Copyright (c) 1991-1994 by Xerox Corporation.  All rights reserved.
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

#include <stdio.h>

/* Data structure for list of root sets.                                */
/* We keep a hash table, so that we can filter out duplicate additions. */
/* Under Win32, we need to do a better job of filtering overlaps, so    */
/* we resort to sequential search, and pay the price.                   */
/* This is really declared in gc_priv.h:
struct roots {
        ptr_t r_start;
        ptr_t r_end;
#       if !defined(MSWIN32) && !defined(MSWINCE) && !defined(CYGWIN32)
          struct roots * r_next;
#       endif
        MK_GC_bool r_tmp;
                -- Delete before registering new dynamic libraries
};

struct roots MK_GC_static_roots[MAX_ROOT_SETS];
*/

int MK_GC_no_dls = 0;      /* Register dynamic library data segments.      */

static int n_root_sets = 0;
        /* MK_GC_static_roots[0..n_root_sets) contains the valid root sets. */

#if !defined(NO_DEBUGGING)
  /* For debugging:     */
  void MK_GC_print_static_roots(void)
  {
    int i;
    size_t total = 0;

    for (i = 0; i < n_root_sets; i++) {
        MK_GC_printf("From %p to %p%s\n",
                  MK_GC_static_roots[i].r_start,
                  MK_GC_static_roots[i].r_end,
                  MK_GC_static_roots[i].r_tmp ? " (temporary)" : "");
        total += MK_GC_static_roots[i].r_end - MK_GC_static_roots[i].r_start;
    }
    MK_GC_printf("Total size: %ld\n", (unsigned long) total);
    if (MK_GC_root_size != total) {
        MK_GC_err_printf("MK_GC_root_size incorrect: %ld!!\n",
                      (long) MK_GC_root_size);
    }
  }
#endif /* !NO_DEBUGGING */

#ifndef THREADS
  /* Primarily for debugging support:     */
  /* Is the address p in one of the registered static root sections?      */
  MK_GC_INNER MK_GC_bool MK_GC_is_static_root(ptr_t p)
  {
    static int last_root_set = MAX_ROOT_SETS;
    int i;

    if (last_root_set < n_root_sets
        && p >= MK_GC_static_roots[last_root_set].r_start
        && p < MK_GC_static_roots[last_root_set].r_end) return(TRUE);
    for (i = 0; i < n_root_sets; i++) {
        if (p >= MK_GC_static_roots[i].r_start
            && p < MK_GC_static_roots[i].r_end) {
            last_root_set = i;
            return(TRUE);
        }
    }
    return(FALSE);
  }
#endif /* !THREADS */

#if !defined(MSWIN32) && !defined(MSWINCE) && !defined(CYGWIN32)
/*
#   define LOG_RT_SIZE 6
#   define RT_SIZE (1 << LOG_RT_SIZE)  -- Power of 2, may be != MAX_ROOT_SETS

    struct roots * MK_GC_root_index[RT_SIZE];
        -- Hash table header.  Used only to check whether a range is
        -- already present.
        -- really defined in gc_priv.h
*/

  MK_GC_INLINE int rt_hash(ptr_t addr)
  {
    word result = (word) addr;
#   if CPP_WORDSZ > 8*LOG_RT_SIZE
        result ^= result >> 8*LOG_RT_SIZE;
#   endif
#   if CPP_WORDSZ > 4*LOG_RT_SIZE
        result ^= result >> 4*LOG_RT_SIZE;
#   endif
    result ^= result >> 2*LOG_RT_SIZE;
    result ^= result >> LOG_RT_SIZE;
    result &= (RT_SIZE-1);
    return(result);
  }

  /* Is a range starting at b already in the table? If so return a      */
  /* pointer to it, else NULL.                                          */
  MK_GC_INNER void * MK_GC_roots_present(ptr_t b)
  {
    int h = rt_hash(b);
    struct roots *p = MK_GC_root_index[h];

    while (p != 0) {
        if (p -> r_start == (ptr_t)b) return(p);
        p = p -> r_next;
    }
    return NULL;
  }

  /* Add the given root structure to the index. */
  MK_GC_INLINE void add_roots_to_index(struct roots *p)
  {
    int h = rt_hash(p -> r_start);

    p -> r_next = MK_GC_root_index[h];
    MK_GC_root_index[h] = p;
  }
#endif /* !MSWIN32 && !MSWINCE && !CYGWIN32 */

MK_GC_INNER word MK_GC_root_size = 0;

MK_GC_API void MK_GC_CALL MK_GC_add_roots(void *b, void *e)
{
    DCL_LOCK_STATE;

    if (!MK_GC_is_initialized) MK_GC_init();
    LOCK();
    MK_GC_add_roots_inner((ptr_t)b, (ptr_t)e, FALSE);
    UNLOCK();
}


/* Add [b,e) to the root set.  Adding the same interval a second time   */
/* is a moderately fast no-op, and hence benign.  We do not handle      */
/* different but overlapping intervals efficiently.  (We do handle      */
/* them correctly.)                                                     */
/* Tmp specifies that the interval may be deleted before                */
/* re-registering dynamic libraries.                                    */
void MK_GC_add_roots_inner(ptr_t b, ptr_t e, MK_GC_bool tmp)
{
    struct roots * old;

    MK_GC_ASSERT(b <= e);
    b = (ptr_t)(((word)b + (sizeof(word) - 1)) & ~(sizeof(word) - 1));
                                        /* round b up to word boundary */
    e = (ptr_t)((word)e & ~(sizeof(word) - 1));
                                        /* round e down to word boundary */
    if (b >= e) return; /* nothing to do */

#   if defined(MSWIN32) || defined(MSWINCE) || defined(CYGWIN32)
      /* Spend the time to ensure that there are no overlapping */
      /* or adjacent intervals.                                 */
      /* This could be done faster with e.g. a                  */
      /* balanced tree.  But the execution time here is         */
      /* virtually guaranteed to be dominated by the time it    */
      /* takes to scan the roots.                               */
      {
        register int i;
        old = 0; /* initialized to prevent warning. */
        for (i = 0; i < n_root_sets; i++) {
            old = MK_GC_static_roots + i;
            if (b <= old -> r_end && e >= old -> r_start) {
                if (b < old -> r_start) {
                    MK_GC_root_size += old->r_start - b;
                    old -> r_start = b;
                }
                if (e > old -> r_end) {
                    MK_GC_root_size += e - old->r_end;
                    old -> r_end = e;
                }
                old -> r_tmp &= tmp;
                break;
            }
        }
        if (i < n_root_sets) {
          /* merge other overlapping intervals */
            struct roots *other;

            for (i++; i < n_root_sets; i++) {
              other = MK_GC_static_roots + i;
              b = other -> r_start;
              e = other -> r_end;
              if (b <= old -> r_end && e >= old -> r_start) {
                if (b < old -> r_start) {
                    MK_GC_root_size += old->r_start - b;
                    old -> r_start = b;
                }
                if (e > old -> r_end) {
                    MK_GC_root_size += e - old->r_end;
                    old -> r_end = e;
                }
                old -> r_tmp &= other -> r_tmp;
                /* Delete this entry. */
                  MK_GC_root_size -= (other -> r_end - other -> r_start);
                  other -> r_start = MK_GC_static_roots[n_root_sets-1].r_start;
                  other -> r_end = MK_GC_static_roots[n_root_sets-1].r_end;
                  n_root_sets--;
              }
            }
          return;
        }
      }
#   else
      old = (struct roots *)MK_GC_roots_present(b);
      if (old != 0) {
        if (e <= old -> r_end) /* already there */ return;
        /* else extend */
        MK_GC_root_size += e - old -> r_end;
        old -> r_end = e;
        return;
      }
#   endif
    if (n_root_sets == MAX_ROOT_SETS) {
        ABORT("Too many root sets");
    }
    MK_GC_static_roots[n_root_sets].r_start = (ptr_t)b;
    MK_GC_static_roots[n_root_sets].r_end = (ptr_t)e;
    MK_GC_static_roots[n_root_sets].r_tmp = tmp;
#   if !defined(MSWIN32) && !defined(MSWINCE) && !defined(CYGWIN32)
      MK_GC_static_roots[n_root_sets].r_next = 0;
      add_roots_to_index(MK_GC_static_roots + n_root_sets);
#   endif
    MK_GC_root_size += e - b;
    n_root_sets++;
}

static MK_GC_bool roots_were_cleared = FALSE;

MK_GC_API void MK_GC_CALL MK_GC_clear_roots(void)
{
    DCL_LOCK_STATE;

    if (!MK_GC_is_initialized) MK_GC_init();
    LOCK();
    roots_were_cleared = TRUE;
    n_root_sets = 0;
    MK_GC_root_size = 0;
#   if !defined(MSWIN32) && !defined(MSWINCE) && !defined(CYGWIN32)
      BZERO(MK_GC_root_index, RT_SIZE * sizeof(void *));
#   endif
    UNLOCK();
}

/* Internal use only; lock held.        */
STATIC void MK_GC_remove_root_at_pos(int i)
{
    MK_GC_root_size -= (MK_GC_static_roots[i].r_end - MK_GC_static_roots[i].r_start);
    MK_GC_static_roots[i].r_start = MK_GC_static_roots[n_root_sets-1].r_start;
    MK_GC_static_roots[i].r_end = MK_GC_static_roots[n_root_sets-1].r_end;
    MK_GC_static_roots[i].r_tmp = MK_GC_static_roots[n_root_sets-1].r_tmp;
    n_root_sets--;
}

#if !defined(MSWIN32) && !defined(MSWINCE) && !defined(CYGWIN32)
  STATIC void MK_GC_rebuild_root_index(void)
  {
    int i;
    BZERO(MK_GC_root_index, RT_SIZE * sizeof(void *));
    for (i = 0; i < n_root_sets; i++)
        add_roots_to_index(MK_GC_static_roots + i);
  }
#endif

#if defined(DYNAMIC_LOADING) || defined(MSWIN32) || defined(MSWINCE) \
     || defined(PCR) || defined(CYGWIN32)
/* Internal use only; lock held.        */
STATIC void MK_GC_remove_tmp_roots(void)
{
    int i;

    for (i = 0; i < n_root_sets; ) {
        if (MK_GC_static_roots[i].r_tmp) {
            MK_GC_remove_root_at_pos(i);
        } else {
            i++;
        }
    }
#   if !defined(MSWIN32) && !defined(MSWINCE) && !defined(CYGWIN32)
      MK_GC_rebuild_root_index();
#   endif
}
#endif

#if !defined(MSWIN32) && !defined(MSWINCE) && !defined(CYGWIN32)
  STATIC void MK_GC_remove_roots_inner(ptr_t b, ptr_t e);

  MK_GC_API void MK_GC_CALL MK_GC_remove_roots(void *b, void *e)
  {
    DCL_LOCK_STATE;

    /* Quick check whether has nothing to do */
    if ((((word)b + (sizeof(word) - 1)) & ~(sizeof(word) - 1)) >=
        ((word)e & ~(sizeof(word) - 1)))
      return;

    LOCK();
    MK_GC_remove_roots_inner((ptr_t)b, (ptr_t)e);
    UNLOCK();
  }

  /* Should only be called when the lock is held */
  STATIC void MK_GC_remove_roots_inner(ptr_t b, ptr_t e)
  {
    int i;
    for (i = 0; i < n_root_sets; ) {
        if (MK_GC_static_roots[i].r_start >= b
            && MK_GC_static_roots[i].r_end <= e) {
            MK_GC_remove_root_at_pos(i);
        } else {
            i++;
        }
    }
    MK_GC_rebuild_root_index();
  }
#endif /* !defined(MSWIN32) && !defined(MSWINCE) && !defined(CYGWIN32) */

#if (defined(MSWIN32) || defined(MSWINCE) || defined(CYGWIN32)) \
    && !defined(NO_DEBUGGING)
  /* Not used at present (except for, may be, debugging purpose).       */
  /* Workaround for the OS mapping and unmapping behind our back:       */
  /* Is the address p in one of the temporary static root sections?     */
  MK_GC_bool MK_GC_is_tmp_root(ptr_t p)
  {
    static int last_root_set = MAX_ROOT_SETS;
    register int i;

    if (last_root_set < n_root_sets
        && p >= MK_GC_static_roots[last_root_set].r_start
        && p < MK_GC_static_roots[last_root_set].r_end)
        return MK_GC_static_roots[last_root_set].r_tmp;
    for (i = 0; i < n_root_sets; i++) {
        if (p >= MK_GC_static_roots[i].r_start
            && p < MK_GC_static_roots[i].r_end) {
            last_root_set = i;
            return MK_GC_static_roots[i].r_tmp;
        }
    }
    return(FALSE);
  }
#endif /* MSWIN32 || MSWINCE || CYGWIN32 */

MK_GC_INNER ptr_t MK_GC_approx_sp(void)
{
    volatile word sp;
    sp = (word)&sp;
                /* Also force stack to grow if necessary. Otherwise the */
                /* later accesses might cause the kernel to think we're */
                /* doing something wrong.                               */
    return((ptr_t)sp);
                /* GNU C: alternatively, we may return the value of     */
                /*__builtin_frame_address(0).                           */
}

/*
 * Data structure for excluded static roots.
 * Real declaration is in gc_priv.h.

struct exclusion {
    ptr_t e_start;
    ptr_t e_end;
};

struct exclusion MK_GC_excl_table[MAX_EXCLUSIONS];
                                        -- Array of exclusions, ascending
                                        -- address order.
*/

STATIC size_t MK_GC_excl_table_entries = 0;/* Number of entries in use.      */

/* Return the first exclusion range that includes an address >= start_addr */
/* Assumes the exclusion table contains at least one entry (namely the     */
/* GC data structures).                                                    */
STATIC struct exclusion * MK_GC_next_exclusion(ptr_t start_addr)
{
    size_t low = 0;
    size_t high = MK_GC_excl_table_entries - 1;
    size_t mid;

    while (high > low) {
        mid = (low + high) >> 1;
        /* low <= mid < high    */
        if ((word) MK_GC_excl_table[mid].e_end <= (word) start_addr) {
            low = mid + 1;
        } else {
            high = mid;
        }
    }
    if ((word) MK_GC_excl_table[low].e_end <= (word) start_addr) return 0;
    return MK_GC_excl_table + low;
}

/* Should only be called when the lock is held.  The range boundaries   */
/* should be properly aligned and valid.                                */
MK_GC_INNER void MK_GC_exclude_static_roots_inner(void *start, void *finish)
{
    struct exclusion * next;
    size_t next_index, i;

    MK_GC_ASSERT((word)start % sizeof(word) == 0);
    MK_GC_ASSERT(start < finish);

    if (0 == MK_GC_excl_table_entries) {
        next = 0;
    } else {
        next = MK_GC_next_exclusion(start);
    }
    if (0 != next) {
      if ((word)(next -> e_start) < (word) finish) {
        /* incomplete error check. */
        ABORT("Exclusion ranges overlap");
      }
      if ((word)(next -> e_start) == (word) finish) {
        /* extend old range backwards   */
          next -> e_start = (ptr_t)start;
          return;
      }
      next_index = next - MK_GC_excl_table;
      for (i = MK_GC_excl_table_entries; i > next_index; --i) {
        MK_GC_excl_table[i] = MK_GC_excl_table[i-1];
      }
    } else {
      next_index = MK_GC_excl_table_entries;
    }
    if (MK_GC_excl_table_entries == MAX_EXCLUSIONS) ABORT("Too many exclusions");
    MK_GC_excl_table[next_index].e_start = (ptr_t)start;
    MK_GC_excl_table[next_index].e_end = (ptr_t)finish;
    ++MK_GC_excl_table_entries;
}

MK_GC_API void MK_GC_CALL MK_GC_exclude_static_roots(void *b, void *e)
{
    DCL_LOCK_STATE;

    /* Adjust the upper boundary for safety (round down) */
    e = (void *)((word)e & ~(sizeof(word) - 1));

    if (b == e) return;  /* nothing to exclude? */

    LOCK();
    MK_GC_exclude_static_roots_inner(b, e);
    UNLOCK();
}

/* Invoke push_conditional on ranges that are not excluded. */
/*ARGSUSED*/
STATIC void MK_GC_push_conditional_with_exclusions(ptr_t bottom, ptr_t top,
                                                MK_GC_bool all)
{
    struct exclusion * next;
    ptr_t excl_start;

    while (bottom < top) {
        next = MK_GC_next_exclusion(bottom);
        if (0 == next || (excl_start = next -> e_start) >= top) {
            MK_GC_push_conditional(bottom, top, all);
            return;
        }
        if (excl_start > bottom) MK_GC_push_conditional(bottom, excl_start, all);
        bottom = next -> e_end;
    }
}

#ifdef IA64
  /* Similar to MK_GC_push_all_stack_sections() but for IA-64 registers store. */
  MK_GC_INNER void MK_GC_push_all_register_sections(ptr_t bs_lo, ptr_t bs_hi,
                  int eager, struct MK_GC_traced_stack_sect_s *traced_stack_sect)
  {
    while (traced_stack_sect != NULL) {
        ptr_t frame_bs_lo = traced_stack_sect -> backing_store_end;
        MK_GC_ASSERT(frame_bs_lo <= bs_hi);
        if (eager) {
            MK_GC_push_all_eager(frame_bs_lo, bs_hi);
        } else {
            MK_GC_push_all_stack(frame_bs_lo, bs_hi);
        }
        bs_hi = traced_stack_sect -> saved_backing_store_ptr;
        traced_stack_sect = traced_stack_sect -> prev;
    }
    MK_GC_ASSERT(bs_lo <= bs_hi);
    if (eager) {
        MK_GC_push_all_eager(bs_lo, bs_hi);
    } else {
        MK_GC_push_all_stack(bs_lo, bs_hi);
    }
  }
#endif /* IA64 */

#ifdef THREADS

MK_GC_INNER void MK_GC_push_all_stack_sections(ptr_t lo, ptr_t hi,
                        struct MK_GC_traced_stack_sect_s *traced_stack_sect)
{
    while (traced_stack_sect != NULL) {
        MK_GC_ASSERT(lo HOTTER_THAN (ptr_t)traced_stack_sect);
#       ifdef STACK_GROWS_UP
            MK_GC_push_all_stack((ptr_t)traced_stack_sect, lo);
#       else /* STACK_GROWS_DOWN */
            MK_GC_push_all_stack(lo, (ptr_t)traced_stack_sect);
#       endif
        lo = traced_stack_sect -> saved_stack_ptr;
        MK_GC_ASSERT(lo != NULL);
        traced_stack_sect = traced_stack_sect -> prev;
    }
    MK_GC_ASSERT(!(hi HOTTER_THAN lo));
#   ifdef STACK_GROWS_UP
        /* We got them backwards! */
        MK_GC_push_all_stack(hi, lo);
#   else /* STACK_GROWS_DOWN */
        MK_GC_push_all_stack(lo, hi);
#   endif
}

#else /* !THREADS */

# ifdef TRACE_BUF
    /* Defined in mark.c.       */
    void MK_GC_add_trace_entry(char *kind, word arg1, word arg2);
# endif

                        /* Similar to MK_GC_push_all_eager, but only the   */
                        /* part hotter than cold_gc_frame is scanned    */
                        /* immediately.  Needed to ensure that callee-  */
                        /* save registers are not missed.               */
/*
 * A version of MK_GC_push_all that treats all interior pointers as valid
 * and scans part of the area immediately, to make sure that saved
 * register values are not lost.
 * Cold_gc_frame delimits the stack section that must be scanned
 * eagerly.  A zero value indicates that no eager scanning is needed.
 * We don't need to worry about the MANUAL_VDB case here, since this
 * is only called in the single-threaded case.  We assume that we
 * cannot collect between an assignment and the corresponding
 * MK_GC_dirty() call.
 */
STATIC void MK_GC_push_all_stack_partially_eager(ptr_t bottom, ptr_t top,
                                              ptr_t cold_gc_frame)
{
  if (!NEED_FIXUP_POINTER && MK_GC_all_interior_pointers) {
    /* Push the hot end of the stack eagerly, so that register values   */
    /* saved inside GC frames are marked before they disappear.         */
    /* The rest of the marking can be deferred until later.             */
    if (0 == cold_gc_frame) {
        MK_GC_push_all_stack(bottom, top);
        return;
    }
    MK_GC_ASSERT(bottom <= cold_gc_frame && cold_gc_frame <= top);
#   ifdef STACK_GROWS_DOWN
        MK_GC_push_all(cold_gc_frame - sizeof(ptr_t), top);
        MK_GC_push_all_eager(bottom, cold_gc_frame);
#   else /* STACK_GROWS_UP */
        MK_GC_push_all(bottom, cold_gc_frame + sizeof(ptr_t));
        MK_GC_push_all_eager(cold_gc_frame, top);
#   endif /* STACK_GROWS_UP */
  } else {
    MK_GC_push_all_eager(bottom, top);
  }
# ifdef TRACE_BUF
      MK_GC_add_trace_entry("MK_GC_push_all_stack", bottom, top);
# endif
}

/* Similar to MK_GC_push_all_stack_sections() but also uses cold_gc_frame. */
STATIC void MK_GC_push_all_stack_part_eager_sections(ptr_t lo, ptr_t hi,
        ptr_t cold_gc_frame, struct MK_GC_traced_stack_sect_s *traced_stack_sect)
{
    MK_GC_ASSERT(traced_stack_sect == NULL || cold_gc_frame == NULL ||
                cold_gc_frame HOTTER_THAN (ptr_t)traced_stack_sect);

    while (traced_stack_sect != NULL) {
        MK_GC_ASSERT(lo HOTTER_THAN (ptr_t)traced_stack_sect);
#       ifdef STACK_GROWS_UP
            MK_GC_push_all_stack_partially_eager((ptr_t)traced_stack_sect, lo,
                                              cold_gc_frame);
#       else /* STACK_GROWS_DOWN */
            MK_GC_push_all_stack_partially_eager(lo, (ptr_t)traced_stack_sect,
                                              cold_gc_frame);
#       endif
        lo = traced_stack_sect -> saved_stack_ptr;
        MK_GC_ASSERT(lo != NULL);
        traced_stack_sect = traced_stack_sect -> prev;
        cold_gc_frame = NULL; /* Use at most once.      */
    }

    MK_GC_ASSERT(!(hi HOTTER_THAN lo));
#   ifdef STACK_GROWS_UP
        /* We got them backwards! */
        MK_GC_push_all_stack_partially_eager(hi, lo, cold_gc_frame);
#   else /* STACK_GROWS_DOWN */
        MK_GC_push_all_stack_partially_eager(lo, hi, cold_gc_frame);
#   endif
}

#endif /* !THREADS */

                        /* Push enough of the current stack eagerly to  */
                        /* ensure that callee-save registers saved in   */
                        /* GC frames are scanned.                       */
                        /* In the non-threads case, schedule entire     */
                        /* stack for scanning.                          */
                        /* The second argument is a pointer to the      */
                        /* (possibly null) thread context, for          */
                        /* (currently hypothetical) more precise        */
                        /* stack scanning.                              */
/*
 * In the absence of threads, push the stack contents.
 * In the presence of threads, push enough of the current stack
 * to ensure that callee-save registers saved in collector frames have been
 * seen.
 * FIXME: Merge with per-thread stuff.
 */
/*ARGSUSED*/
STATIC void MK_GC_push_current_stack(ptr_t cold_gc_frame, void * context)
{
#   if defined(THREADS)
        if (0 == cold_gc_frame) return;
#       ifdef STACK_GROWS_DOWN
          MK_GC_push_all_eager(MK_GC_approx_sp(), cold_gc_frame);
          /* For IA64, the register stack backing store is handled      */
          /* in the thread-specific code.                               */
#       else
          MK_GC_push_all_eager(cold_gc_frame, MK_GC_approx_sp());
#       endif
#   else
        MK_GC_push_all_stack_part_eager_sections(MK_GC_approx_sp(), MK_GC_stackbottom,
                                        cold_gc_frame, MK_GC_traced_stack_sect);
#       ifdef IA64
              /* We also need to push the register stack backing store. */
              /* This should really be done in the same way as the      */
              /* regular stack.  For now we fudge it a bit.             */
              /* Note that the backing store grows up, so we can't use  */
              /* MK_GC_push_all_stack_partially_eager.                     */
              {
                ptr_t bsp = MK_GC_save_regs_ret_val;
                ptr_t cold_gc_bs_pointer = bsp - 2048;
                if (MK_GC_all_interior_pointers &&
                    cold_gc_bs_pointer > BACKING_STORE_BASE) {
                  /* Adjust cold_gc_bs_pointer if below our innermost   */
                  /* "traced stack section" in backing store.           */
                  if (MK_GC_traced_stack_sect != NULL && cold_gc_bs_pointer <
                                MK_GC_traced_stack_sect->backing_store_end)
                    cold_gc_bs_pointer =
                                MK_GC_traced_stack_sect->backing_store_end;
                  MK_GC_push_all_register_sections(BACKING_STORE_BASE,
                        cold_gc_bs_pointer, FALSE, MK_GC_traced_stack_sect);
                  MK_GC_push_all_eager(cold_gc_bs_pointer, bsp);
                } else {
                  MK_GC_push_all_register_sections(BACKING_STORE_BASE, bsp,
                                TRUE /* eager */, MK_GC_traced_stack_sect);
                }
                /* All values should be sufficiently aligned that we    */
                /* don't have to worry about the boundary.              */
              }
#       endif
#   endif /* !THREADS */
}

MK_GC_INNER void (*MK_GC_push_typed_structures)(void) = 0;

                        /* Push GC internal roots.  These are normally  */
                        /* included in the static data segment, and     */
                        /* Thus implicitly pushed.  But we must do this */
                        /* explicitly if normal root processing is      */
                        /* disabled.                                    */
/*
 * Push GC internal roots.  Only called if there is some reason to believe
 * these would not otherwise get registered.
 */
STATIC void MK_GC_push_gc_structures(void)
{
    MK_GC_push_finalizer_structures();
#   if defined(THREADS)
      MK_GC_push_thread_structures();
#   endif
    if( MK_GC_push_typed_structures )
      MK_GC_push_typed_structures();
}

MK_GC_INNER void MK_GC_cond_register_dynamic_libraries(void)
{
# if defined(DYNAMIC_LOADING) || defined(MSWIN32) || defined(MSWINCE) \
     || defined(CYGWIN32) || defined(PCR)
    MK_GC_remove_tmp_roots();
    if (!MK_GC_no_dls) MK_GC_register_dynamic_libraries();
# else
    MK_GC_no_dls = TRUE;
# endif
}

STATIC void MK_GC_push_regs_and_stack(ptr_t cold_gc_frame)
{
    MK_GC_with_callee_saves_pushed(MK_GC_push_current_stack, cold_gc_frame);
}

/*
 * Call the mark routines (MK_GC_tl_push for a single pointer, MK_GC_push_conditional
 * on groups of pointers) on every top level accessible pointer.
 * If all is FALSE, arrange to push only possibly altered values.
 * Cold_gc_frame is an address inside a GC frame that
 * remains valid until all marking is complete.
 * A zero value indicates that it's OK to miss some
 * register values.
 */
MK_GC_INNER void MK_GC_push_roots(MK_GC_bool all, ptr_t cold_gc_frame)
{
    int i;
    unsigned kind;

    /*
     * Next push static data.  This must happen early on, since it's
     * not robust against mark stack overflow.
     */
     /* Re-register dynamic libraries, in case one got added.           */
     /* There is some argument for doing this as late as possible,      */
     /* especially on win32, where it can change asynchronously.        */
     /* In those cases, we do it here.  But on other platforms, it's    */
     /* not safe with the world stopped, so we do it earlier.           */
#      if !defined(REGISTER_LIBRARIES_EARLY)
         MK_GC_cond_register_dynamic_libraries();
#      endif

     /* Mark everything in static data areas                             */
       for (i = 0; i < n_root_sets; i++) {
         MK_GC_push_conditional_with_exclusions(
                             MK_GC_static_roots[i].r_start,
                             MK_GC_static_roots[i].r_end, all);
       }

     /* Mark all free list header blocks, if those were allocated from  */
     /* the garbage collected heap.  This makes sure they don't         */
     /* disappear if we are not marking from static data.  It also      */
     /* saves us the trouble of scanning them, and possibly that of     */
     /* marking the freelists.                                          */
       for (kind = 0; kind < MK_GC_n_kinds; kind++) {
         void *base = MK_GC_base(MK_GC_obj_kinds[kind].ok_freelist);
         if (0 != base) {
           MK_GC_set_mark_bit(base);
         }
       }

     /* Mark from GC internal roots if those might otherwise have       */
     /* been excluded.                                                  */
       if (MK_GC_no_dls || roots_were_cleared) {
           MK_GC_push_gc_structures();
       }

     /* Mark thread local free lists, even if their mark        */
     /* descriptor excludes the link field.                     */
     /* If the world is not stopped, this is unsafe.  It is     */
     /* also unnecessary, since we will do this again with the  */
     /* world stopped.                                          */
#      if defined(THREAD_LOCAL_ALLOC)
         if (MK_GC_world_stopped) MK_GC_mark_thread_local_free_lists();
#      endif

    /*
     * Now traverse stacks, and mark from register contents.
     * These must be done last, since they can legitimately overflow
     * the mark stack.
     * This is usually done by saving the current context on the
     * stack, and then just tracing from the stack.
     */
      MK_GC_push_regs_and_stack(cold_gc_frame);

    if (MK_GC_push_other_roots != 0) (*MK_GC_push_other_roots)();
        /* In the threads case, this also pushes thread stacks. */
        /* Note that without interior pointer recognition lots  */
        /* of stuff may have been pushed already, and this      */
        /* should be careful about mark stack overflows.        */
}
