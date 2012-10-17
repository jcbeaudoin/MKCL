/*
 * Copyright (c) 1991-1994 by Xerox Corporation.  All rights reserved.
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

#include "private/gc_pmark.h"  /* includes gc_priv.h */

#ifdef MK_GC_GCJ_SUPPORT

/*
 * This is an allocator interface tuned for gcj (the GNU static
 * java compiler).
 *
 * Each allocated object has a pointer in its first word to a vtable,
 * which for our purposes is simply a structure describing the type of
 * the object.
 * This descriptor structure contains a GC marking descriptor at offset
 * MARK_DESCR_OFFSET.
 *
 * It is hoped that this interface may also be useful for other systems,
 * possibly with some tuning of the constants.  But the immediate goal
 * is to get better gcj performance.
 *
 * We assume:
 *  1) Counting on explicit initialization of this interface is OK;
 *  2) FASTLOCK is not a significant win.
 */

#include "gc_gcj.h"
#include "private/dbg_mlc.h"

#ifdef MK_GC_ASSERTIONS
  MK_GC_INNER /* variable is also used in thread_local_alloc.c */
#else
  STATIC
#endif
MK_GC_bool MK_GC_gcj_malloc_initialized = FALSE;

int MK_GC_gcj_kind = 0;    /* Object kind for objects with descriptors     */
                        /* in "vtable".                                 */
int MK_GC_gcj_debug_kind = 0;
                        /* The kind of objects that is always marked    */
                        /* with a mark proc call.                       */

MK_GC_INNER ptr_t * MK_GC_gcjobjfreelist = NULL;

STATIC ptr_t * MK_GC_gcjdebugobjfreelist = NULL;

/*ARGSUSED*/
STATIC struct MK_GC_ms_entry * MK_GC_gcj_fake_mark_proc(word * addr,
                                        struct MK_GC_ms_entry *mark_stack_ptr,
                                        struct MK_GC_ms_entry *mark_stack_limit,
                                        word env)
{
    ABORT("No client gcj mark proc is specified");
    return mark_stack_ptr;
}

/* Caller does not hold allocation lock. */
MK_GC_API void MK_GC_CALL MK_GC_init_gcj_malloc(int mp_index,
                                       void * /* really MK_GC_mark_proc */mp)
{
    MK_GC_bool ignore_gcj_info;
    DCL_LOCK_STATE;

    if (mp == 0)        /* In case MK_GC_DS_PROC is unused.        */
      mp = (void *)(word)MK_GC_gcj_fake_mark_proc;

    MK_GC_init();  /* In case it's not already done.       */
    LOCK();
    if (MK_GC_gcj_malloc_initialized) {
      UNLOCK();
      return;
    }
    MK_GC_gcj_malloc_initialized = TRUE;
#   ifdef MK_GC_IGNORE_GCJ_INFO
      /* This is useful for debugging on platforms with missing getenv(). */
      ignore_gcj_info = 1;
#   else
      ignore_gcj_info = (0 != GETENV("MK_GC_IGNORE_GCJ_INFO"));
#   endif
    if (MK_GC_print_stats && ignore_gcj_info) {
        MK_GC_log_printf("Gcj-style type information is disabled!\n");
    }
    MK_GC_ASSERT(MK_GC_mark_procs[mp_index] == (MK_GC_mark_proc)0); /* unused */
    MK_GC_mark_procs[mp_index] = (MK_GC_mark_proc)(word)mp;
    if ((unsigned)mp_index >= MK_GC_n_mark_procs)
        ABORT("MK_GC_init_gcj_malloc: bad index");
    /* Set up object kind gcj-style indirect descriptor. */
      MK_GC_gcjobjfreelist = (ptr_t *)MK_GC_new_free_list_inner();
      if (ignore_gcj_info) {
        /* Use a simple length-based descriptor, thus forcing a fully   */
        /* conservative scan.                                           */
        MK_GC_gcj_kind = MK_GC_new_kind_inner((void **)MK_GC_gcjobjfreelist,
                                        (0 | MK_GC_DS_LENGTH),
                                        TRUE, TRUE);
      } else {
        MK_GC_gcj_kind = MK_GC_new_kind_inner(
                        (void **)MK_GC_gcjobjfreelist,
                        (((word)(-(signed_word)MARK_DESCR_OFFSET
                                 - MK_GC_INDIR_PER_OBJ_BIAS))
                         | MK_GC_DS_PER_OBJECT),
                        FALSE, TRUE);
      }
    /* Set up object kind for objects that require mark proc call.      */
      if (ignore_gcj_info) {
        MK_GC_gcj_debug_kind = MK_GC_gcj_kind;
        MK_GC_gcjdebugobjfreelist = MK_GC_gcjobjfreelist;
      } else {
        MK_GC_gcjdebugobjfreelist = (ptr_t *)MK_GC_new_free_list_inner();
        MK_GC_gcj_debug_kind = MK_GC_new_kind_inner(
                                (void **)MK_GC_gcjdebugobjfreelist,
                                MK_GC_MAKE_PROC(mp_index,
                                             1 /* allocated with debug info */),
                                FALSE, TRUE);
      }
    UNLOCK();
}

#define GENERAL_MALLOC_INNER(lb,k) \
    MK_GC_clear_stack(MK_GC_generic_malloc_inner(lb, k))

#define GENERAL_MALLOC_INNER_IOP(lb,k) \
    MK_GC_clear_stack(MK_GC_generic_malloc_inner_ignore_off_page(lb, k))

/* We need a mechanism to release the lock and invoke finalizers.       */
/* We don't really have an opportunity to do this on a rarely executed  */
/* path on which the lock is not held.  Thus we check at a              */
/* rarely executed point at which it is safe to release the lock.       */
/* We do this even where we could just call MK_GC_INVOKE_FINALIZERS,       */
/* since it's probably cheaper and certainly more uniform.              */
/* FIXME - Consider doing the same elsewhere?                           */
static void maybe_finalize(void)
{
   static word last_finalized_no = 0;
   DCL_LOCK_STATE;

   if (MK_GC_gc_no == last_finalized_no) return;
   if (!MK_GC_is_initialized) return;
   UNLOCK();
   MK_GC_INVOKE_FINALIZERS();
   LOCK();
   last_finalized_no = MK_GC_gc_no;
}

/* Allocate an object, clear it, and store the pointer to the   */
/* type structure (vtable in gcj).                              */
/* This adds a byte at the end of the object if MK_GC_malloc would.*/
#ifdef THREAD_LOCAL_ALLOC
  MK_GC_INNER void * MK_GC_core_gcj_malloc(size_t lb,
                                     void * ptr_to_struct_containing_descr)
#else
  MK_GC_API void * MK_GC_CALL MK_GC_gcj_malloc(size_t lb,
                                      void * ptr_to_struct_containing_descr)
#endif
{
    ptr_t op;
    ptr_t * opp;
    word lg;
    DCL_LOCK_STATE;

    if(SMALL_OBJ(lb)) {
        lg = MK_GC_size_map[lb];
        opp = &(MK_GC_gcjobjfreelist[lg]);
        LOCK();
        op = *opp;
        if(EXPECT(op == 0, FALSE)) {
            maybe_finalize();
            op = (ptr_t)GENERAL_MALLOC_INNER((word)lb, MK_GC_gcj_kind);
            if (0 == op) {
                MK_GC_oom_func oom_fn = MK_GC_oom_fn;
                UNLOCK();
                return((*oom_fn)(lb));
            }
        } else {
            *opp = obj_link(op);
            MK_GC_bytes_allocd += GRANULES_TO_BYTES(lg);
        }
        *(void **)op = ptr_to_struct_containing_descr;
        MK_GC_ASSERT(((void **)op)[1] == 0);
        UNLOCK();
    } else {
        LOCK();
        maybe_finalize();
        op = (ptr_t)GENERAL_MALLOC_INNER((word)lb, MK_GC_gcj_kind);
        if (0 == op) {
            MK_GC_oom_func oom_fn = MK_GC_oom_fn;
            UNLOCK();
            return((*oom_fn)(lb));
        }
        *(void **)op = ptr_to_struct_containing_descr;
        UNLOCK();
    }
    return((void *) op);
}

/* Similar to MK_GC_gcj_malloc, but add debug info.  This is allocated     */
/* with MK_GC_gcj_debug_kind.                                              */
MK_GC_API void * MK_GC_CALL MK_GC_debug_gcj_malloc(size_t lb,
                void * ptr_to_struct_containing_descr, MK_GC_EXTRA_PARAMS)
{
    void * result;
    DCL_LOCK_STATE;

    /* We're careful to avoid extra calls, which could          */
    /* confuse the backtrace.                                   */
    LOCK();
    maybe_finalize();
    result = MK_GC_generic_malloc_inner(lb + DEBUG_BYTES, MK_GC_gcj_debug_kind);
    if (result == 0) {
        MK_GC_oom_func oom_fn = MK_GC_oom_fn;
        UNLOCK();
        MK_GC_err_printf("MK_GC_debug_gcj_malloc(%ld, %p) returning NULL (",
                      (unsigned long)lb, ptr_to_struct_containing_descr);
        MK_GC_err_puts(s);
        MK_GC_err_printf(":%d)\n", i);
        return((*oom_fn)(lb));
    }
    *((void **)((ptr_t)result + sizeof(oh))) = ptr_to_struct_containing_descr;
    UNLOCK();
    if (!MK_GC_debugging_started) {
        MK_GC_start_debugging();
    }
    ADD_CALL_CHAIN(result, ra);
    return (MK_GC_store_debug_info(result, (word)lb, s, i));
}

/* There is no THREAD_LOCAL_ALLOC for MK_GC_gcj_malloc_ignore_off_page().  */
MK_GC_API void * MK_GC_CALL MK_GC_gcj_malloc_ignore_off_page(size_t lb,
                                     void * ptr_to_struct_containing_descr)
{
    ptr_t op;
    ptr_t * opp;
    word lg;
    DCL_LOCK_STATE;

    if(SMALL_OBJ(lb)) {
        lg = MK_GC_size_map[lb];
        opp = &(MK_GC_gcjobjfreelist[lg]);
        LOCK();
        if( (op = *opp) == 0 ) {
            maybe_finalize();
            op = (ptr_t)GENERAL_MALLOC_INNER_IOP(lb, MK_GC_gcj_kind);
            if (0 == op) {
                MK_GC_oom_func oom_fn = MK_GC_oom_fn;
                UNLOCK();
                return((*oom_fn)(lb));
            }
        } else {
            *opp = obj_link(op);
            MK_GC_bytes_allocd += GRANULES_TO_BYTES(lg);
        }
    } else {
        LOCK();
        maybe_finalize();
        op = (ptr_t)GENERAL_MALLOC_INNER_IOP(lb, MK_GC_gcj_kind);
        if (0 == op) {
            MK_GC_oom_func oom_fn = MK_GC_oom_fn;
            UNLOCK();
            return((*oom_fn)(lb));
        }
    }
    *(void **)op = ptr_to_struct_containing_descr;
    UNLOCK();
    return((void *) op);
}

#endif  /* MK_GC_GCJ_SUPPORT */
