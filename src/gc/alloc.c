/*
 * Copyright 1988, 1989 Hans-J. Boehm, Alan J. Demers
 * Copyright (c) 1991-1996 by Xerox Corporation.  All rights reserved.
 * Copyright (c) 1998 by Silicon Graphics.  All rights reserved.
 * Copyright (c) 1999-2004 Hewlett-Packard Development Company, L.P.
 *
 * THIS MATERIAL IS PROVIDED AS IS, WITH ABSOLUTELY NO WARRANTY EXPRESSED
 * OR IMPLIED.  ANY USE IS AT YOUR OWN RISK.
 *
 * Permission is hereby granted to use or copy this program
 * for any purpose,  provided the above notices are retained on all copies.
 * Permission to modify the code and to distribute modified code is granted,
 * provided the above notices are retained, and a notice that the code was
 * modified is included with the above copyright notice.
 *
 */

#include "private/gc_priv.h"

#include <stdio.h>
#if !defined(MACOS) && !defined(MSWINCE)
# include <signal.h>
# if !defined(__CC_ARM)
#   include <sys/types.h>
# endif
#endif

/*
 * Separate free lists are maintained for different sized objects
 * up to MAXOBJBYTES.
 * The call MK_GC_allocobj(i,k) ensures that the freelist for
 * kind k objects of size i points to a non-empty
 * free list. It returns a pointer to the first entry on the free list.
 * In a single-threaded world, MK_GC_allocobj may be called to allocate
 * an object of (small) size i as follows:
 *
 *            opp = &(MK_GC_objfreelist[i]);
 *            if (*opp == 0) MK_GC_allocobj(i, NORMAL);
 *            ptr = *opp;
 *            *opp = obj_link(ptr);
 *
 * Note that this is very fast if the free list is non-empty; it should
 * only involve the execution of 4 or 5 simple instructions.
 * All composite objects on freelists are cleared, except for
 * their first word.
 */

/*
 * The allocator uses MK_GC_allochblk to allocate large chunks of objects.
 * These chunks all start on addresses which are multiples of
 * HBLKSZ.   Each allocated chunk has an associated header,
 * which can be located quickly based on the address of the chunk.
 * (See headers.c for details.)
 * This makes it possible to check quickly whether an
 * arbitrary address corresponds to an object administered by the
 * allocator.
 */

word MK_GC_non_gc_bytes = 0;  /* Number of bytes not intended to be collected */

word MK_GC_gc_no = 0;

#ifndef MK_GC_DISABLE_INCREMENTAL
  MK_GC_INNER int MK_GC_incremental = 0;      /* By default, stop the world.  */
#endif

#ifdef THREADS
  int MK_GC_parallel = FALSE;      /* By default, parallel GC is off.      */
#endif

#ifndef MK_GC_FULL_FREQ
# define MK_GC_FULL_FREQ 19   /* Every 20th collection is a full   */
                           /* collection, whether we need it    */
                           /* or not.                           */
#endif

int MK_GC_full_freq = MK_GC_FULL_FREQ;

STATIC MK_GC_bool MK_GC_need_full_gc = FALSE;
                           /* Need full GC do to heap growth.   */

#ifdef THREAD_LOCAL_ALLOC
  MK_GC_INNER MK_GC_bool MK_GC_world_stopped = FALSE;
#endif

STATIC word MK_GC_used_heap_size_after_full = 0;

/* MK_GC_copyright symbol is externally visible. */
char * const MK_GC_copyright[] =
{"Copyright 1988,1989 Hans-J. Boehm and Alan J. Demers ",
"Copyright (c) 1991-1995 by Xerox Corporation.  All rights reserved. ",
"Copyright (c) 1996-1998 by Silicon Graphics.  All rights reserved. ",
"Copyright (c) 1999-2009 by Hewlett-Packard Company.  All rights reserved. ",
"THIS MATERIAL IS PROVIDED AS IS, WITH ABSOLUTELY NO WARRANTY",
" EXPRESSED OR IMPLIED.  ANY USE IS AT YOUR OWN RISK.",
"See source code for details." };

/* Version macros are now defined in gc_version.h, which is included by */
/* gc.h, which is included by gc_priv.h.                                */
#ifndef MK_GC_NO_VERSION_VAR
  const unsigned MK_GC_version = ((MK_GC_VERSION_MAJOR << 16) |
                        (MK_GC_VERSION_MINOR << 8) | MK_GC_TMP_ALPHA_VERSION);
#endif

MK_GC_API unsigned MK_GC_CALL MK_GC_get_version(void)
{
  return (MK_GC_VERSION_MAJOR << 16) | (MK_GC_VERSION_MINOR << 8) |
          MK_GC_TMP_ALPHA_VERSION;
}

/* some more variables */

#ifdef MK_GC_DONT_EXPAND
  MK_GC_bool MK_GC_dont_expand = TRUE;
#else
  MK_GC_bool MK_GC_dont_expand = FALSE;
#endif

#ifndef MK_GC_FREE_SPACE_DIVISOR
# define MK_GC_FREE_SPACE_DIVISOR 3 /* must be > 0 */
#endif

word MK_GC_free_space_divisor = MK_GC_FREE_SPACE_DIVISOR;

MK_GC_INNER int MK_GC_CALLBACK MK_GC_never_stop_func(void)
{
  return(0);
}

#ifndef MK_GC_TIME_LIMIT
# define MK_GC_TIME_LIMIT 50  /* We try to keep pause times from exceeding  */
                           /* this by much. In milliseconds.             */
#endif

unsigned long MK_GC_time_limit = MK_GC_TIME_LIMIT;

#ifndef NO_CLOCK
  STATIC CLOCK_TYPE MK_GC_start_time = 0;
                                /* Time at which we stopped world.      */
                                /* used only in MK_GC_timeout_stop_func.   */
#endif

STATIC int MK_GC_n_attempts = 0;   /* Number of attempts at finishing      */
                                /* collection within MK_GC_time_limit.     */

STATIC MK_GC_stop_func MK_GC_default_stop_func = MK_GC_never_stop_func;
                                /* accessed holding the lock.           */

MK_GC_API void MK_GC_CALL MK_GC_set_stop_func(MK_GC_stop_func stop_func)
{
  DCL_LOCK_STATE;
  MK_GC_ASSERT(stop_func != 0);
  LOCK();
  MK_GC_default_stop_func = stop_func;
  UNLOCK();
}

MK_GC_API MK_GC_stop_func MK_GC_CALL MK_GC_get_stop_func(void)
{
  MK_GC_stop_func stop_func;
  DCL_LOCK_STATE;
  LOCK();
  stop_func = MK_GC_default_stop_func;
  UNLOCK();
  return stop_func;
}

#if defined(MK_GC_DISABLE_INCREMENTAL) || defined(NO_CLOCK)
# define MK_GC_timeout_stop_func MK_GC_default_stop_func
#else
  STATIC int MK_GC_CALLBACK MK_GC_timeout_stop_func (void)
  {
    CLOCK_TYPE current_time;
    static unsigned count = 0;
    unsigned long time_diff;

    if ((*MK_GC_default_stop_func)())
      return(1);

    if ((count++ & 3) != 0) return(0);
    GET_TIME(current_time);
    time_diff = MS_TIME_DIFF(current_time,MK_GC_start_time);
    if (time_diff >= MK_GC_time_limit) {
        if (MK_GC_print_stats) {
          MK_GC_log_printf(
                "Abandoning stopped marking after %lu msecs (attempt %d)\n",
                time_diff, MK_GC_n_attempts);
        }
        return(1);
    }
    return(0);
  }
#endif /* !MK_GC_DISABLE_INCREMENTAL */

#ifdef THREADS
  MK_GC_INNER word MK_GC_total_stacksize = 0; /* updated on every push_all_stacks */
#endif

/* Return the minimum number of words that must be allocated between    */
/* collections to amortize the collection cost.                         */
static word min_bytes_allocd(void)
{
#   ifdef STACK_GROWS_UP
      word stack_size = MK_GC_approx_sp() - MK_GC_stackbottom;
            /* MK_GC_stackbottom is used only for a single-threaded case.  */
#   else
      word stack_size = MK_GC_stackbottom - MK_GC_approx_sp();
#   endif

    word total_root_size;       /* includes double stack size,  */
                                /* since the stack is expensive */
                                /* to scan.                     */
    word scan_size;             /* Estimate of memory to be scanned     */
                                /* during normal GC.                    */

#   ifdef THREADS
      if (MK_GC_need_to_lock) {
        /* We are multi-threaded... */
        stack_size = MK_GC_total_stacksize;
        /* For now, we just use the value computed during the latest GC. */
#       ifdef DEBUG_THREADS
          MK_GC_log_printf("Total stacks size: %lu\n",
                        (unsigned long)stack_size);
#       endif
      }
#   endif

    total_root_size = 2 * stack_size + MK_GC_root_size;
    scan_size = 2 * MK_GC_composite_in_use + MK_GC_atomic_in_use / 4
                + total_root_size;
    if (MK_GC_incremental) {
        return scan_size / (2 * MK_GC_free_space_divisor);
    } else {
        return scan_size / MK_GC_free_space_divisor;
    }
}

/* Return the number of bytes allocated, adjusted for explicit storage  */
/* management, etc..  This number is used in deciding when to trigger   */
/* collections.                                                         */
STATIC word MK_GC_adj_bytes_allocd(void)
{
    signed_word result;
    signed_word expl_managed = (signed_word)MK_GC_non_gc_bytes
                                - (signed_word)MK_GC_non_gc_bytes_at_gc;

    /* Don't count what was explicitly freed, or newly allocated for    */
    /* explicit management.  Note that deallocating an explicitly       */
    /* managed object should not alter result, assuming the client      */
    /* is playing by the rules.                                         */
    result = (signed_word)MK_GC_bytes_allocd
             + (signed_word)MK_GC_bytes_dropped
             - (signed_word)MK_GC_bytes_freed
             + (signed_word)MK_GC_finalizer_bytes_freed
             - expl_managed;
    if (result > (signed_word)MK_GC_bytes_allocd) {
        result = MK_GC_bytes_allocd;
        /* probably client bug or unfortunate scheduling */
    }
    result += MK_GC_bytes_finalized;
        /* We count objects enqueued for finalization as though they    */
        /* had been reallocated this round. Finalization is user        */
        /* visible progress.  And if we don't count this, we have       */
        /* stability problems for programs that finalize all objects.   */
    if (result < (signed_word)(MK_GC_bytes_allocd >> 3)) {
        /* Always count at least 1/8 of the allocations.  We don't want */
        /* to collect too infrequently, since that would inhibit        */
        /* coalescing of free storage blocks.                           */
        /* This also makes us partially robust against client bugs.     */
        return(MK_GC_bytes_allocd >> 3);
    } else {
        return(result);
    }
}


/* Clear up a few frames worth of garbage left at the top of the stack. */
/* This is used to prevent us from accidentally treating garbage left   */
/* on the stack by other parts of the collector as roots.  This         */
/* differs from the code in misc.c, which actually tries to keep the    */
/* stack clear of long-lived, client-generated garbage.                 */
STATIC void MK_GC_clear_a_few_frames(void)
{
#   ifndef CLEAR_NWORDS
#     define CLEAR_NWORDS 64
#   endif
    volatile word frames[CLEAR_NWORDS];
    BZERO((word *)frames, CLEAR_NWORDS * sizeof(word));
}

/* Heap size at which we need a collection to avoid expanding past      */
/* limits used by blacklisting.                                         */
STATIC word MK_GC_collect_at_heapsize = (word)(-1);

/* Have we allocated enough to amortize a collection? */
MK_GC_INNER MK_GC_bool MK_GC_should_collect(void)
{
    static word last_min_bytes_allocd;
    static word last_gc_no;
    if (last_gc_no != MK_GC_gc_no) {
      last_gc_no = MK_GC_gc_no;
      last_min_bytes_allocd = min_bytes_allocd();
    }
    return(MK_GC_adj_bytes_allocd() >= last_min_bytes_allocd
           || MK_GC_heapsize >= MK_GC_collect_at_heapsize);
}

/* STATIC */ MK_GC_start_callback_proc MK_GC_start_call_back = 0;
                        /* Called at start of full collections.         */
                        /* Not called if 0.  Called with the allocation */
                        /* lock held.  Not used by GC itself.           */

MK_GC_API void MK_GC_CALL MK_GC_set_start_callback(MK_GC_start_callback_proc fn)
{
    DCL_LOCK_STATE;
    LOCK();
    MK_GC_start_call_back = fn;
    UNLOCK();
}

MK_GC_API MK_GC_start_callback_proc MK_GC_CALL MK_GC_get_start_callback(void)
{
    MK_GC_start_callback_proc fn;
    DCL_LOCK_STATE;
    LOCK();
    fn = MK_GC_start_call_back;
    UNLOCK();
    return fn;
}

MK_GC_INLINE void MK_GC_notify_full_gc(void)
{
    if (MK_GC_start_call_back != 0) {
        (*MK_GC_start_call_back)();
    }
}

STATIC MK_GC_bool MK_GC_is_full_gc = FALSE;

STATIC MK_GC_bool MK_GC_stopped_mark(MK_GC_stop_func stop_func);
STATIC void MK_GC_finish_collection(void);

/*
 * Initiate a garbage collection if appropriate.
 * Choose judiciously
 * between partial, full, and stop-world collections.
 */
STATIC void MK_GC_maybe_gc(void)
{
    static int n_partial_gcs = 0;

    MK_GC_ASSERT(I_HOLD_LOCK());
    ASSERT_CANCEL_DISABLED();
    if (MK_GC_should_collect()) {
        if (!MK_GC_incremental) {
            /* FIXME: If possible, MK_GC_default_stop_func should be used here */
            MK_GC_try_to_collect_inner(MK_GC_never_stop_func);
            n_partial_gcs = 0;
            return;
        } else {
#         ifdef PARALLEL_MARK
            if (MK_GC_parallel)
              MK_GC_wait_for_reclaim();
#         endif
          if (MK_GC_need_full_gc || n_partial_gcs >= MK_GC_full_freq) {
            if (MK_GC_print_stats) {
              MK_GC_log_printf(
                  "***>Full mark for collection %lu after %ld allocd bytes\n",
                  (unsigned long)MK_GC_gc_no + 1, (long)MK_GC_bytes_allocd);
            }
            MK_GC_promote_black_lists();
            (void)MK_GC_reclaim_all((MK_GC_stop_func)0, TRUE);
            MK_GC_notify_full_gc();
            MK_GC_clear_marks();
            n_partial_gcs = 0;
            MK_GC_is_full_gc = TRUE;
          } else {
            n_partial_gcs++;
          }
        }
        /* We try to mark with the world stopped.       */
        /* If we run out of time, this turns into       */
        /* incremental marking.                 */
#       ifndef NO_CLOCK
          if (MK_GC_time_limit != MK_GC_TIME_UNLIMITED) { GET_TIME(MK_GC_start_time); }
#       endif
        /* FIXME: If possible, MK_GC_default_stop_func should be   */
        /* used instead of MK_GC_never_stop_func here.             */
        if (MK_GC_stopped_mark(MK_GC_time_limit == MK_GC_TIME_UNLIMITED?
                            MK_GC_never_stop_func : MK_GC_timeout_stop_func)) {
#           ifdef SAVE_CALL_CHAIN
                MK_GC_save_callers(MK_GC_last_stack);
#           endif
            MK_GC_finish_collection();
        } else {
            if (!MK_GC_is_full_gc) {
                /* Count this as the first attempt */
                MK_GC_n_attempts++;
            }
        }
    }
}


/*
 * Stop the world garbage collection.  Assumes lock held. If stop_func is
 * not MK_GC_never_stop_func then abort if stop_func returns TRUE.
 * Return TRUE if we successfully completed the collection.
 */
MK_GC_INNER MK_GC_bool MK_GC_try_to_collect_inner(MK_GC_stop_func stop_func)
{
#   ifndef SMALL_CONFIG
      CLOCK_TYPE start_time = 0; /* initialized to prevent warning. */
      CLOCK_TYPE current_time;
#   endif
    ASSERT_CANCEL_DISABLED();
    if (MK_GC_dont_gc || (*stop_func)()) return FALSE;
    if (MK_GC_incremental && MK_GC_collection_in_progress()) {
      if (MK_GC_print_stats) {
        MK_GC_log_printf(
            "MK_GC_try_to_collect_inner: finishing collection in progress\n");
      }
      /* Just finish collection already in progress.    */
        while(MK_GC_collection_in_progress()) {
            if ((*stop_func)()) return(FALSE);
            MK_GC_collect_a_little_inner(1);
        }
    }
    MK_GC_notify_full_gc();
#   ifndef SMALL_CONFIG
      if (MK_GC_print_stats) {
        GET_TIME(start_time);
        MK_GC_log_printf("Initiating full world-stop collection!\n");
      }
#   endif
    MK_GC_promote_black_lists();
    /* Make sure all blocks have been reclaimed, so sweep routines      */
    /* don't see cleared mark bits.                                     */
    /* If we're guaranteed to finish, then this is unnecessary.         */
    /* In the find_leak case, we have to finish to guarantee that       */
    /* previously unmarked objects are not reported as leaks.           */
#       ifdef PARALLEL_MARK
          if (MK_GC_parallel)
            MK_GC_wait_for_reclaim();
#       endif
        if ((MK_GC_find_leak || stop_func != MK_GC_never_stop_func)
            && !MK_GC_reclaim_all(stop_func, FALSE)) {
            /* Aborted.  So far everything is still consistent. */
            return(FALSE);
        }
    MK_GC_invalidate_mark_state();  /* Flush mark stack.   */
    MK_GC_clear_marks();
#   ifdef SAVE_CALL_CHAIN
        MK_GC_save_callers(MK_GC_last_stack);
#   endif
    MK_GC_is_full_gc = TRUE;
    if (!MK_GC_stopped_mark(stop_func)) {
      if (!MK_GC_incremental) {
        /* We're partially done and have no way to complete or use      */
        /* current work.  Reestablish invariants as cheaply as          */
        /* possible.                                                    */
        MK_GC_invalidate_mark_state();
        MK_GC_unpromote_black_lists();
      } /* else we claim the world is already still consistent.  We'll  */
        /* finish incrementally.                                        */
      return(FALSE);
    }
    MK_GC_finish_collection();
#   ifndef SMALL_CONFIG
      if (MK_GC_print_stats) {
        GET_TIME(current_time);
        MK_GC_log_printf("Complete collection took %lu msecs\n",
                      MS_TIME_DIFF(current_time,start_time));
      }
#   endif
    return(TRUE);
}

/*
 * Perform n units of garbage collection work.  A unit is intended to touch
 * roughly MK_GC_RATE pages.  Every once in a while, we do more than that.
 * This needs to be a fairly large number with our current incremental
 * GC strategy, since otherwise we allocate too much during GC, and the
 * cleanup gets expensive.
 */
#ifndef MK_GC_RATE
# define MK_GC_RATE 10
#endif
#ifndef MAX_PRIOR_ATTEMPTS
# define MAX_PRIOR_ATTEMPTS 1
#endif
        /* Maximum number of prior attempts at world stop marking       */
        /* A value of 1 means that we finish the second time, no matter */
        /* how long it takes.  Doesn't count the initial root scan      */
        /* for a full GC.                                               */

STATIC int MK_GC_deficit = 0;/* The number of extra calls to MK_GC_mark_some  */
                          /* that we have made.                         */

MK_GC_INNER void MK_GC_collect_a_little_inner(int n)
{
    int i;
    IF_CANCEL(int cancel_state;)

    if (MK_GC_dont_gc) return;
    DISABLE_CANCEL(cancel_state);
    if (MK_GC_incremental && MK_GC_collection_in_progress()) {
        for (i = MK_GC_deficit; i < MK_GC_RATE*n; i++) {
            if (MK_GC_mark_some((ptr_t)0)) {
                /* Need to finish a collection */
#               ifdef SAVE_CALL_CHAIN
                    MK_GC_save_callers(MK_GC_last_stack);
#               endif
#               ifdef PARALLEL_MARK
                    if (MK_GC_parallel)
                      MK_GC_wait_for_reclaim();
#               endif
                if (MK_GC_n_attempts < MAX_PRIOR_ATTEMPTS
                    && MK_GC_time_limit != MK_GC_TIME_UNLIMITED) {
#                 ifndef NO_CLOCK
                    GET_TIME(MK_GC_start_time);
#                 endif
                  if (!MK_GC_stopped_mark(MK_GC_timeout_stop_func)) {
                    MK_GC_n_attempts++;
                    break;
                  }
                } else {
                  /* FIXME: If possible, MK_GC_default_stop_func should be */
                  /* used here.                                         */
                  (void)MK_GC_stopped_mark(MK_GC_never_stop_func);
                }
                MK_GC_finish_collection();
                break;
            }
        }
        if (MK_GC_deficit > 0) MK_GC_deficit -= MK_GC_RATE*n;
        if (MK_GC_deficit < 0) MK_GC_deficit = 0;
    } else {
        MK_GC_maybe_gc();
    }
    RESTORE_CANCEL(cancel_state);
}

MK_GC_INNER void (*MK_GC_check_heap)(void) = 0;
MK_GC_INNER void (*MK_GC_print_all_smashed)(void) = 0;

MK_GC_API int MK_GC_CALL MK_GC_collect_a_little(void)
{
    int result;
    DCL_LOCK_STATE;

    LOCK();
    MK_GC_collect_a_little_inner(1);
    result = (int)MK_GC_collection_in_progress();
    UNLOCK();
    if (!result && MK_GC_debugging_started) MK_GC_print_all_smashed();
    return(result);
}

#ifndef SMALL_CONFIG
  /* Variables for world-stop average delay time statistic computation. */
  /* "divisor" is incremented every world-stop and halved when reached  */
  /* its maximum (or upon "total_time" oveflow).                        */
  static unsigned world_stopped_total_time = 0;
  static unsigned world_stopped_total_divisor = 0;
# ifndef MAX_TOTAL_TIME_DIVISOR
    /* We shall not use big values here (so "outdated" delay time       */
    /* values would have less impact on "average" delay time value than */
    /* newer ones).                                                     */
#   define MAX_TOTAL_TIME_DIVISOR 1000
# endif
#endif

/*
 * Assumes lock is held.  We stop the world and mark from all roots.
 * If stop_func() ever returns TRUE, we may fail and return FALSE.
 * Increment MK_GC_gc_no if we succeed.
 */
STATIC MK_GC_bool MK_GC_stopped_mark(MK_GC_stop_func stop_func)
{
    unsigned i;
#   ifndef SMALL_CONFIG
      CLOCK_TYPE start_time = 0; /* initialized to prevent warning. */
      CLOCK_TYPE current_time;
#   endif

#   if !defined(REDIRECT_MALLOC) && (defined(MSWIN32) || defined(MSWINCE))
        MK_GC_add_current_malloc_heap();
#   endif
#   if defined(REGISTER_LIBRARIES_EARLY)
        MK_GC_cond_register_dynamic_libraries();
#   endif

#   ifndef SMALL_CONFIG
      if (MK_GC_print_stats)
        GET_TIME(start_time);
#   endif

    STOP_WORLD();
#   ifdef THREAD_LOCAL_ALLOC
      MK_GC_world_stopped = TRUE;
#   endif
    if (MK_GC_print_stats) {
        /* Output blank line for convenience here */
        MK_GC_log_printf(
              "\n--> Marking for collection %lu after %lu allocated bytes\n",
              (unsigned long)MK_GC_gc_no + 1, (unsigned long) MK_GC_bytes_allocd);
    }
#   ifdef MAKE_BACK_GRAPH
      if (MK_GC_print_back_height) {
        MK_GC_build_back_graph();
      }
#   endif

    /* Mark from all roots.  */
        /* Minimize junk left in my registers and on the stack */
            MK_GC_clear_a_few_frames();
            MK_GC_noop(0,0,0,0,0,0);
        MK_GC_initiate_gc();
        for (i = 0;;i++) {
          if ((*stop_func)()) {
            if (MK_GC_print_stats) {
              MK_GC_log_printf("Abandoned stopped marking after %u iterations\n",
                            i);
            }
            MK_GC_deficit = i;     /* Give the mutator a chance.   */
#           ifdef THREAD_LOCAL_ALLOC
              MK_GC_world_stopped = FALSE;
#           endif
            START_WORLD();
            return(FALSE);
          }
          if (MK_GC_mark_some(MK_GC_approx_sp())) break;
        }

    MK_GC_gc_no++;
    if (MK_GC_print_stats) {
      MK_GC_log_printf(
             "Collection %lu reclaimed %ld bytes ---> heapsize = %lu bytes\n",
             (unsigned long)(MK_GC_gc_no - 1), (long)MK_GC_bytes_found,
             (unsigned long)MK_GC_heapsize);
    }

    /* Check all debugged objects for consistency */
        if (MK_GC_debugging_started) {
            (*MK_GC_check_heap)();
        }

#   ifdef THREAD_LOCAL_ALLOC
      MK_GC_world_stopped = FALSE;
#   endif
    START_WORLD();
#   ifndef SMALL_CONFIG
      if (MK_GC_print_stats) {
        unsigned long time_diff;
        unsigned total_time, divisor;
        GET_TIME(current_time);
        time_diff = MS_TIME_DIFF(current_time,start_time);

        /* Compute new world-stop delay total time */
        total_time = world_stopped_total_time;
        divisor = world_stopped_total_divisor;
        if ((int)total_time < 0 || divisor >= MAX_TOTAL_TIME_DIVISOR) {
          /* Halve values if overflow occurs */
          total_time >>= 1;
          divisor >>= 1;
        }
        total_time += time_diff < (((unsigned)-1) >> 1) ?
                        (unsigned)time_diff : ((unsigned)-1) >> 1;
        /* Update old world_stopped_total_time and its divisor */
        world_stopped_total_time = total_time;
        world_stopped_total_divisor = ++divisor;

        MK_GC_ASSERT(divisor != 0);
        MK_GC_log_printf(
                "World-stopped marking took %lu msecs (%u in average)\n",
                time_diff, total_time / divisor);
      }
#   endif
    return(TRUE);
}

/* Set all mark bits for the free list whose first entry is q   */
MK_GC_INNER void MK_GC_set_fl_marks(ptr_t q)
{
   struct hblk *h, *last_h;
   hdr *hhdr;
   IF_PER_OBJ(size_t sz;)
   unsigned bit_no;

   if (q != NULL) {
     h = HBLKPTR(q);
     last_h = h;
     hhdr = HDR(h);
     IF_PER_OBJ(sz = hhdr->hb_sz;)

     for (;;) {
        bit_no = MARK_BIT_NO((ptr_t)q - (ptr_t)h, sz);
        if (!mark_bit_from_hdr(hhdr, bit_no)) {
          set_mark_bit_from_hdr(hhdr, bit_no);
          ++hhdr -> hb_n_marks;
        }

        q = obj_link(q);
        if (q == NULL)
          break;

        h = HBLKPTR(q);
        if (h != last_h) {
          last_h = h;
          hhdr = HDR(h);
          IF_PER_OBJ(sz = hhdr->hb_sz;)
        }
     }
   }
}

#if defined(MK_GC_ASSERTIONS) && defined(THREADS) && defined(THREAD_LOCAL_ALLOC)
  /* Check that all mark bits for the free list whose first entry is    */
  /* (*pfreelist) are set.  Check skipped if points to a special value. */
  void MK_GC_check_fl_marks(void **pfreelist)
  {
#   ifdef MK_AO_HAVE_load_acquire_read
      MK_AO_t *list = (MK_AO_t *)MK_AO_load_acquire_read((MK_AO_t *)pfreelist);
                /* Atomic operations are used because the world is running. */
      MK_AO_t *prev;
      MK_AO_t *p;

      if ((word)list <= HBLKSIZE) return;

      prev = (MK_AO_t *)pfreelist;
      for (p = list; p != NULL;) {
        MK_AO_t *next;

        if (!MK_GC_is_marked((ptr_t)p)) {
          MK_GC_err_printf("Unmarked object %p on list %p\n",
                        (void *)p, (void *)list);
          ABORT("Unmarked local free list entry");
        }

        /* While traversing the free-list, it re-reads the pointer to   */
        /* the current node before accepting its next pointer and       */
        /* bails out if the latter has changed.  That way, it won't     */
        /* try to follow the pointer which might be been modified       */
        /* after the object was returned to the client.  It might       */
        /* perform the mark-check on the just allocated object but      */
        /* that should be harmless.                                     */
        next = (MK_AO_t *)MK_AO_load_acquire_read(p);
        if (MK_AO_load(prev) != (MK_AO_t)p)
          break;
        prev = p;
        p = next;
      }
#   else
      /* FIXME: Not implemented (just skipped). */
      (void)pfreelist;
#   endif
  }
#endif /* MK_GC_ASSERTIONS && THREAD_LOCAL_ALLOC */

/* Clear all mark bits for the free list whose first entry is q */
/* Decrement MK_GC_bytes_found by number of bytes on free list.    */
STATIC void MK_GC_clear_fl_marks(ptr_t q)
{
   struct hblk *h, *last_h;
   hdr *hhdr;
   size_t sz;
   unsigned bit_no;

   if (q != NULL) {
     h = HBLKPTR(q);
     last_h = h;
     hhdr = HDR(h);
     sz = hhdr->hb_sz;  /* Normally set only once. */

     for (;;) {
        bit_no = MARK_BIT_NO((ptr_t)q - (ptr_t)h, sz);
        if (mark_bit_from_hdr(hhdr, bit_no)) {
          size_t n_marks = hhdr -> hb_n_marks - 1;
          clear_mark_bit_from_hdr(hhdr, bit_no);
#         ifdef PARALLEL_MARK
            /* Appr. count, don't decrement to zero! */
            if (0 != n_marks || !MK_GC_parallel) {
              hhdr -> hb_n_marks = n_marks;
            }
#         else
            hhdr -> hb_n_marks = n_marks;
#         endif
        }
        MK_GC_bytes_found -= sz;

        q = obj_link(q);
        if (q == NULL)
          break;

        h = HBLKPTR(q);
        if (h != last_h) {
          last_h = h;
          hhdr = HDR(h);
          sz = hhdr->hb_sz;
        }
     }
   }
}

#if defined(MK_GC_ASSERTIONS) && defined(THREADS) && defined(THREAD_LOCAL_ALLOC)
  void MK_GC_check_tls(void);
#endif

/* Finish up a collection.  Assumes mark bits are consistent, lock is   */
/* held, but the world is otherwise running.                            */
STATIC void MK_GC_finish_collection(void)
{
#   ifndef SMALL_CONFIG
      CLOCK_TYPE start_time = 0; /* initialized to prevent warning. */
      CLOCK_TYPE finalize_time = 0;
      CLOCK_TYPE done_time;
#   endif

#   if defined(MK_GC_ASSERTIONS) && defined(THREADS) \
       && defined(THREAD_LOCAL_ALLOC) && !defined(DBG_HDRS_ALL)
        /* Check that we marked some of our own data.           */
        /* FIXME: Add more checks.                              */
        MK_GC_check_tls();
#   endif

#   ifndef SMALL_CONFIG
      if (MK_GC_print_stats)
        GET_TIME(start_time);
#   endif

    MK_GC_bytes_found = 0;
#   if defined(LINUX) && defined(__ELF__) && !defined(SMALL_CONFIG)
        if (GETENV("MK_GC_PRINT_ADDRESS_MAP") != 0) {
          MK_GC_print_address_map();
        }
#   endif
    COND_DUMP;
    if (MK_GC_find_leak) {
      /* Mark all objects on the free list.  All objects should be      */
      /* marked when we're done.                                        */
      word size;        /* current object size  */
      unsigned kind;
      ptr_t q;

      for (kind = 0; kind < MK_GC_n_kinds; kind++) {
        for (size = 1; size <= MAXOBJGRANULES; size++) {
          q = MK_GC_obj_kinds[kind].ok_freelist[size];
          if (q != 0) MK_GC_set_fl_marks(q);
        }
      }
      MK_GC_start_reclaim(TRUE);
        /* The above just checks; it doesn't really reclaim anything.   */
    }

    MK_GC_finalize();
#   ifdef STUBBORN_ALLOC
      MK_GC_clean_changing_list();
#   endif

#   ifndef SMALL_CONFIG
      if (MK_GC_print_stats)
        GET_TIME(finalize_time);
#   endif

    if (MK_GC_print_back_height) {
#     ifdef MAKE_BACK_GRAPH
        MK_GC_traverse_back_graph();
#     elif !defined(SMALL_CONFIG)
        MK_GC_err_printf("Back height not available: "
                      "Rebuild collector with -DMAKE_BACK_GRAPH\n");
#     endif
    }

    /* Clear free list mark bits, in case they got accidentally marked   */
    /* (or MK_GC_find_leak is set and they were intentionally marked).      */
    /* Also subtract memory remaining from MK_GC_bytes_found count.         */
    /* Note that composite objects on free list are cleared.             */
    /* Thus accidentally marking a free list is not a problem;  only     */
    /* objects on the list itself will be marked, and that's fixed here. */
    {
      word size;        /* current object size          */
      ptr_t q;          /* pointer to current object    */
      unsigned kind;

      for (kind = 0; kind < MK_GC_n_kinds; kind++) {
        for (size = 1; size <= MAXOBJGRANULES; size++) {
          q = MK_GC_obj_kinds[kind].ok_freelist[size];
          if (q != 0) MK_GC_clear_fl_marks(q);
        }
      }
    }

    if (MK_GC_print_stats == VERBOSE)
        MK_GC_log_printf("Bytes recovered before sweep - f.l. count = %ld\n",
                      (long)MK_GC_bytes_found);

    /* Reconstruct free lists to contain everything not marked */
    MK_GC_start_reclaim(FALSE);
    if (MK_GC_print_stats) {
      MK_GC_log_printf("Heap contains %lu pointer-containing "
                    "+ %lu pointer-free reachable bytes\n",
                    (unsigned long)MK_GC_composite_in_use,
                    (unsigned long)MK_GC_atomic_in_use);
    }
    if (MK_GC_is_full_gc) {
        MK_GC_used_heap_size_after_full = USED_HEAP_SIZE;
        MK_GC_need_full_gc = FALSE;
    } else {
        MK_GC_need_full_gc = USED_HEAP_SIZE - MK_GC_used_heap_size_after_full
                            > min_bytes_allocd();
    }

    if (MK_GC_print_stats == VERBOSE) {
#     ifdef USE_MUNMAP
        MK_GC_log_printf("Immediately reclaimed %ld bytes in heap"
                      " of size %lu bytes (%lu unmapped)\n",
                      (long)MK_GC_bytes_found, (unsigned long)MK_GC_heapsize,
                      (unsigned long)MK_GC_unmapped_bytes);
#     else
        MK_GC_log_printf(
                "Immediately reclaimed %ld bytes in heap of size %lu bytes\n",
                (long)MK_GC_bytes_found, (unsigned long)MK_GC_heapsize);
#     endif
    }

    /* Reset or increment counters for next cycle */
    MK_GC_n_attempts = 0;
    MK_GC_is_full_gc = FALSE;
    MK_GC_bytes_allocd_before_gc += MK_GC_bytes_allocd;
    MK_GC_non_gc_bytes_at_gc = MK_GC_non_gc_bytes;
    MK_GC_bytes_allocd = 0;
    MK_GC_bytes_dropped = 0;
    MK_GC_bytes_freed = 0;
    MK_GC_finalizer_bytes_freed = 0;

#   ifdef USE_MUNMAP
      MK_GC_unmap_old();
#   endif

#   ifndef SMALL_CONFIG
      if (MK_GC_print_stats) {
        GET_TIME(done_time);

        /* A convenient place to output finalization statistics. */
        MK_GC_print_finalization_stats();

        MK_GC_log_printf("Finalize plus initiate sweep took %lu + %lu msecs\n",
                      MS_TIME_DIFF(finalize_time,start_time),
                      MS_TIME_DIFF(done_time,finalize_time));
      }
#   endif
}

/* If stop_func == 0 then MK_GC_default_stop_func is used instead.         */
STATIC MK_GC_bool MK_GC_try_to_collect_general(MK_GC_stop_func stop_func,
                                         MK_GC_bool force_unmap)
{
    MK_GC_bool result;
#   ifdef USE_MUNMAP
      int old_unmap_threshold;
#   endif
    IF_CANCEL(int cancel_state;)
    DCL_LOCK_STATE;

    if (!MK_GC_is_initialized) MK_GC_init();
    if (MK_GC_debugging_started) MK_GC_print_all_smashed();
    MK_GC_INVOKE_FINALIZERS();
    LOCK();
    DISABLE_CANCEL(cancel_state);
#   ifdef USE_MUNMAP
      old_unmap_threshold = MK_GC_unmap_threshold;
      if (force_unmap ||
          (MK_GC_force_unmap_on_gcollect && old_unmap_threshold > 0))
        MK_GC_unmap_threshold = 1; /* unmap as much as possible */
#   endif
    ENTER_GC();
    /* Minimize junk left in my registers */
      MK_GC_noop(0,0,0,0,0,0);
    result = MK_GC_try_to_collect_inner(stop_func != 0 ? stop_func :
                                     MK_GC_default_stop_func);
    EXIT_GC();
#   ifdef USE_MUNMAP
      MK_GC_unmap_threshold = old_unmap_threshold; /* restore */
#   endif
    RESTORE_CANCEL(cancel_state);
    UNLOCK();
    if (result) {
        if (MK_GC_debugging_started) MK_GC_print_all_smashed();
        MK_GC_INVOKE_FINALIZERS();
    }
    return(result);
}

/* Externally callable routines to invoke full, stop-the-world collection. */
MK_GC_API int MK_GC_CALL MK_GC_try_to_collect(MK_GC_stop_func stop_func)
{
    MK_GC_ASSERT(stop_func != 0);
    return (int)MK_GC_try_to_collect_general(stop_func, FALSE);
}

MK_GC_API void MK_GC_CALL MK_GC_gcollect(void)
{
    /* 0 is passed as stop_func to get MK_GC_default_stop_func value       */
    /* while holding the allocation lock (to prevent data races).       */
    (void)MK_GC_try_to_collect_general(0, FALSE);
    if (MK_GC_have_errors) MK_GC_print_all_errors();
}

MK_GC_API void MK_GC_CALL MK_GC_gcollect_and_unmap(void)
{
    (void)MK_GC_try_to_collect_general(MK_GC_never_stop_func, TRUE);
}

MK_GC_INNER word MK_GC_n_heap_sects = 0;
                        /* Number of sections currently in heap. */

#ifdef USE_PROC_FOR_LIBRARIES
  MK_GC_INNER word MK_GC_n_memory = 0;
                        /* Number of GET_MEM allocated memory sections. */
#endif

#ifdef USE_PROC_FOR_LIBRARIES
  /* Add HBLKSIZE aligned, GET_MEM-generated block to MK_GC_our_memory. */
  /* Defined to do nothing if USE_PROC_FOR_LIBRARIES not set.       */
  MK_GC_INNER void MK_GC_add_to_our_memory(ptr_t p, size_t bytes)
  {
    if (0 == p) return;
    if (MK_GC_n_memory >= MAX_HEAP_SECTS)
      ABORT("Too many GC-allocated memory sections: Increase MAX_HEAP_SECTS");
    MK_GC_our_memory[MK_GC_n_memory].hs_start = p;
    MK_GC_our_memory[MK_GC_n_memory].hs_bytes = bytes;
    MK_GC_n_memory++;
  }
#endif

/*
 * Use the chunk of memory starting at p of size bytes as part of the heap.
 * Assumes p is HBLKSIZE aligned, and bytes is a multiple of HBLKSIZE.
 */
MK_GC_INNER void MK_GC_add_to_heap(struct hblk *p, size_t bytes)
{
    hdr * phdr;
    word endp;

    if (MK_GC_n_heap_sects >= MAX_HEAP_SECTS) {
        ABORT("Too many heap sections: Increase MAXHINCR or MAX_HEAP_SECTS");
    }
    while ((word)p <= HBLKSIZE) {
        /* Can't handle memory near address zero. */
        ++p;
        bytes -= HBLKSIZE;
        if (0 == bytes) return;
    }
    endp = (word)p + bytes;
    if (endp <= (word)p) {
        /* Address wrapped. */
        bytes -= HBLKSIZE;
        if (0 == bytes) return;
        endp -= HBLKSIZE;
    }
    phdr = MK_GC_install_header(p);
    if (0 == phdr) {
        /* This is extremely unlikely. Can't add it.  This will         */
        /* almost certainly result in a 0 return from the allocator,    */
        /* which is entirely appropriate.                               */
        return;
    }
    MK_GC_ASSERT(endp > (word)p && endp == (word)p + bytes);
    MK_GC_heap_sects[MK_GC_n_heap_sects].hs_start = (ptr_t)p;
    MK_GC_heap_sects[MK_GC_n_heap_sects].hs_bytes = bytes;
    MK_GC_n_heap_sects++;
    phdr -> hb_sz = bytes;
    phdr -> hb_flags = 0;
    MK_GC_freehblk(p);
    MK_GC_heapsize += bytes;
    if ((ptr_t)p <= (ptr_t)MK_GC_least_plausible_heap_addr
        || MK_GC_least_plausible_heap_addr == 0) {
        MK_GC_least_plausible_heap_addr = (void *)((ptr_t)p - sizeof(word));
                /* Making it a little smaller than necessary prevents   */
                /* us from getting a false hit from the variable        */
                /* itself.  There's some unintentional reflection       */
                /* here.                                                */
    }
    if ((ptr_t)p + bytes >= (ptr_t)MK_GC_greatest_plausible_heap_addr) {
        MK_GC_greatest_plausible_heap_addr = (void *)endp;
    }
}

#if !defined(NO_DEBUGGING)
  void MK_GC_print_heap_sects(void)
  {
    unsigned i;

    MK_GC_printf("Total heap size: %lu\n", (unsigned long)MK_GC_heapsize);
    for (i = 0; i < MK_GC_n_heap_sects; i++) {
      ptr_t start = MK_GC_heap_sects[i].hs_start;
      size_t len = MK_GC_heap_sects[i].hs_bytes;
      struct hblk *h;
      unsigned nbl = 0;

      for (h = (struct hblk *)start; h < (struct hblk *)(start + len); h++) {
        if (MK_GC_is_black_listed(h, HBLKSIZE)) nbl++;
      }
      MK_GC_printf("Section %d from %p to %p %lu/%lu blacklisted\n",
                i, start, start + len,
                (unsigned long)nbl, (unsigned long)(len/HBLKSIZE));
    }
  }
#endif

void * MK_GC_least_plausible_heap_addr = (void *)ONES;
void * MK_GC_greatest_plausible_heap_addr = 0;

MK_GC_INLINE word MK_GC_max(word x, word y)
{
    return(x > y? x : y);
}

MK_GC_INLINE word MK_GC_min(word x, word y)
{
    return(x < y? x : y);
}

MK_GC_API void MK_GC_CALL MK_GC_set_max_heap_size(MK_GC_word n)
{
    MK_GC_max_heapsize = n;
}

MK_GC_word MK_GC_max_retries = 0;

/*
 * this explicitly increases the size of the heap.  It is used
 * internally, but may also be invoked from MK_GC_expand_hp by the user.
 * The argument is in units of HBLKSIZE.
 * Tiny values of n are rounded up.
 * Returns FALSE on failure.
 */
MK_GC_INNER MK_GC_bool MK_GC_expand_hp_inner(word n)
{
    word bytes;
    struct hblk * space;
    word expansion_slop;        /* Number of bytes by which we expect the */
                                /* heap to expand soon.                   */

    if (n < MINHINCR) n = MINHINCR;
    bytes = n * HBLKSIZE;
    /* Make sure bytes is a multiple of MK_GC_page_size */
      {
        word mask = MK_GC_page_size - 1;
        bytes += mask;
        bytes &= ~mask;
      }

    if (MK_GC_max_heapsize != 0 && MK_GC_heapsize + bytes > MK_GC_max_heapsize) {
        /* Exceeded self-imposed limit */
        return(FALSE);
    }
    space = GET_MEM(bytes);
    MK_GC_add_to_our_memory((ptr_t)space, bytes);
    if (space == 0) {
        if (MK_GC_print_stats) {
            MK_GC_log_printf("Failed to expand heap by %ld bytes\n",
                          (unsigned long)bytes);
        }
        return(FALSE);
    }
    if (MK_GC_print_stats) {
      MK_GC_log_printf("Increasing heap size by %lu after %lu allocated bytes\n",
                    (unsigned long)bytes, (unsigned long)MK_GC_bytes_allocd);
    }
    /* Adjust heap limits generously for blacklisting to work better.   */
    /* MK_GC_add_to_heap performs minimal adjustment needed for            */
    /* correctness.                                                     */
    expansion_slop = min_bytes_allocd() + 4*MAXHINCR*HBLKSIZE;
    if ((MK_GC_last_heap_addr == 0 && !((word)space & SIGNB))
        || (MK_GC_last_heap_addr != 0 && MK_GC_last_heap_addr < (ptr_t)space)) {
        /* Assume the heap is growing up */
        word new_limit = (word)space + bytes + expansion_slop;
        if (new_limit > (word)space) {
          MK_GC_greatest_plausible_heap_addr =
            (void *)MK_GC_max((word)MK_GC_greatest_plausible_heap_addr,
                           (word)new_limit);
        }
    } else {
        /* Heap is growing down */
        word new_limit = (word)space - expansion_slop;
        if (new_limit < (word)space) {
          MK_GC_least_plausible_heap_addr =
            (void *)MK_GC_min((word)MK_GC_least_plausible_heap_addr,
                           (word)space - expansion_slop);
        }
    }
    MK_GC_prev_heap_addr = MK_GC_last_heap_addr;
    MK_GC_last_heap_addr = (ptr_t)space;
    MK_GC_add_to_heap(space, bytes);
    /* Force GC before we are likely to allocate past expansion_slop */
      MK_GC_collect_at_heapsize =
         MK_GC_heapsize + expansion_slop - 2*MAXHINCR*HBLKSIZE;
      if (MK_GC_collect_at_heapsize < MK_GC_heapsize /* wrapped */)
         MK_GC_collect_at_heapsize = (word)(-1);
    return(TRUE);
}

/* Really returns a bool, but it's externally visible, so that's clumsy. */
/* Arguments is in bytes.  Includes MK_GC_init() call.                      */
MK_GC_API int MK_GC_CALL MK_GC_expand_hp(size_t bytes)
{
    int result;
    DCL_LOCK_STATE;

    LOCK();
    if (!MK_GC_is_initialized) MK_GC_init();
    result = (int)MK_GC_expand_hp_inner(divHBLKSZ((word)bytes));
    if (result) MK_GC_requested_heapsize += bytes;
    UNLOCK();
    return(result);
}

MK_GC_INNER unsigned MK_GC_fail_count = 0;
                        /* How many consecutive GC/expansion failures?  */
                        /* Reset by MK_GC_allochblk.                       */

/* Collect or expand heap in an attempt make the indicated number of    */
/* free blocks available.  Should be called until the blocks are        */
/* available (seting retry value to TRUE unless this is the first call  */
/* in a loop) or until it fails by returning FALSE.                     */
MK_GC_INNER MK_GC_bool MK_GC_collect_or_expand(word needed_blocks,
                                      MK_GC_bool ignore_off_page,
                                      MK_GC_bool retry)
{
    MK_GC_bool gc_not_stopped = TRUE;
    word blocks_to_get;
    IF_CANCEL(int cancel_state;)

    DISABLE_CANCEL(cancel_state);
    if (!MK_GC_incremental && !MK_GC_dont_gc &&
        ((MK_GC_dont_expand && MK_GC_bytes_allocd > 0) || MK_GC_should_collect())) {
      /* Try to do a full collection using 'default' stop_func (unless  */
      /* nothing has been allocated since the latest collection or heap */
      /* expansion is disabled).                                        */
      gc_not_stopped = MK_GC_try_to_collect_inner(
                        MK_GC_bytes_allocd > 0 && (!MK_GC_dont_expand || !retry) ?
                        MK_GC_default_stop_func : MK_GC_never_stop_func);
      if (gc_not_stopped == TRUE || !retry) {
        /* Either the collection hasn't been aborted or this is the     */
        /* first attempt (in a loop).                                   */
        RESTORE_CANCEL(cancel_state);
        return(TRUE);
      }
    }

    blocks_to_get = MK_GC_heapsize/(HBLKSIZE*MK_GC_free_space_divisor)
                        + needed_blocks;
    if (blocks_to_get > MAXHINCR) {
      word slop;

      /* Get the minimum required to make it likely that we can satisfy */
      /* the current request in the presence of black-listing.          */
      /* This will probably be more than MAXHINCR.                      */
      if (ignore_off_page) {
        slop = 4;
      } else {
        slop = 2 * divHBLKSZ(BL_LIMIT);
        if (slop > needed_blocks) slop = needed_blocks;
      }
      if (needed_blocks + slop > MAXHINCR) {
        blocks_to_get = needed_blocks + slop;
      } else {
        blocks_to_get = MAXHINCR;
      }
    }

    if (!MK_GC_expand_hp_inner(blocks_to_get)
        && !MK_GC_expand_hp_inner(needed_blocks)) {
      if (gc_not_stopped == FALSE) {
        /* Don't increment MK_GC_fail_count here (and no warning).     */
        MK_GC_gcollect_inner();
        MK_GC_ASSERT(MK_GC_bytes_allocd == 0);
      } else if (MK_GC_fail_count++ < MK_GC_max_retries) {
        WARN("Out of Memory!  Trying to continue ...\n", 0);
        MK_GC_gcollect_inner();
      } else {
#       if !defined(AMIGA) || !defined(MK_GC_AMIGA_FASTALLOC)
          WARN("Out of Memory! Heap size: %" MK_GC_PRIdPTR " MiB."
               " Returning NULL!\n", (MK_GC_heapsize - MK_GC_unmapped_bytes) >> 20);
#       endif
        RESTORE_CANCEL(cancel_state);
        return(FALSE);
      }
    } else if (MK_GC_fail_count && MK_GC_print_stats) {
      MK_GC_log_printf("Memory available again...\n");
    }
    RESTORE_CANCEL(cancel_state);
    return(TRUE);
}

/*
 * Make sure the object free list for size gran (in granules) is not empty.
 * Return a pointer to the first object on the free list.
 * The object MUST BE REMOVED FROM THE FREE LIST BY THE CALLER.
 * Assumes we hold the allocator lock.
 */
MK_GC_INNER ptr_t MK_GC_allocobj(size_t gran, int kind)
{
    void ** flh = &(MK_GC_obj_kinds[kind].ok_freelist[gran]);
    MK_GC_bool tried_minor = FALSE;
    MK_GC_bool retry = FALSE;

    if (gran == 0) return(0);

    while (*flh == 0) {
      ENTER_GC();
      /* Do our share of marking work */
        if(TRUE_INCREMENTAL) MK_GC_collect_a_little_inner(1);
      /* Sweep blocks for objects of this size */
        MK_GC_continue_reclaim(gran, kind);
      EXIT_GC();
      if (*flh == 0) {
        MK_GC_new_hblk(gran, kind);
      }
      if (*flh == 0) {
        ENTER_GC();
        if (MK_GC_incremental && MK_GC_time_limit == MK_GC_TIME_UNLIMITED
            && !tried_minor) {
          MK_GC_collect_a_little_inner(1);
          tried_minor = TRUE;
        } else {
          if (!MK_GC_collect_or_expand(1, FALSE, retry)) {
            EXIT_GC();
            return(0);
          }
          retry = TRUE;
        }
        EXIT_GC();
      }
    }
    /* Successful allocation; reset failure count.      */
    MK_GC_fail_count = 0;

    return(*flh);
}
