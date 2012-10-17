/*
 * Copyright 1988, 1989 Hans-J. Boehm, Alan J. Demers
 * Copyright (c) 1991-1994 by Xerox Corporation.  All rights reserved.
 * Copyright (c) 1999-2001 by Hewlett-Packard Company. All rights reserved.
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

#include "private/gc_pmark.h"

#include <stdio.h>
#include <limits.h>
#include <stdarg.h>

#ifndef MSWINCE
# include <signal.h>
#endif

#ifdef MK_GC_SOLARIS_THREADS
# include <sys/syscall.h>
#endif
#if defined(MSWIN32) || defined(MSWINCE) \
    || (defined(CYGWIN32) && defined(MK_GC_READ_ENV_FILE))
# ifndef WIN32_LEAN_AND_MEAN
#   define WIN32_LEAN_AND_MEAN 1
# endif
# define NOSERVICE
# include <windows.h>
#endif

#if defined(UNIX_LIKE) || defined(CYGWIN32)
# include <fcntl.h>
# include <sys/types.h>
# include <sys/stat.h>
#endif

#ifdef NONSTOP
# include <floss.h>
#endif

#ifdef THREADS
# ifdef PCR
#   include "il/PCR_IL.h"
    MK_GC_INNER PCR_Th_ML MK_GC_allocate_ml;
# elif defined(SN_TARGET_PS3)
#   include <pthread.h>
    MK_GC_INNER pthread_mutex_t MK_GC_allocate_ml;
# endif
  /* For other platforms with threads, the lock and possibly            */
  /* MK_GC_lock_holder variables are defined in the thread support code.   */
#endif /* THREADS */

#ifdef DYNAMIC_LOADING
  /* We need to register the main data segment.  Returns  TRUE unless   */
  /* this is done implicitly as part of dynamic library registration.   */
# define MK_GC_REGISTER_MAIN_STATIC_DATA() MK_GC_register_main_static_data()
#else
  /* Don't unnecessarily call MK_GC_register_main_static_data() in case    */
  /* dyn_load.c isn't linked in.                                        */
# define MK_GC_REGISTER_MAIN_STATIC_DATA() TRUE
#endif

#ifdef NEED_CANCEL_DISABLE_COUNT
  __thread unsigned char MK_GC_cancel_disable_count = 0;
#endif

MK_GC_FAR struct _MK_GC_arrays MK_GC_arrays /* = { 0 } */;

MK_GC_INNER MK_GC_bool MK_GC_debugging_started = FALSE;
        /* defined here so we don't have to load debug_malloc.o */

ptr_t MK_GC_stackbottom = 0;

#ifdef IA64
  ptr_t MK_GC_register_stackbottom = 0;
#endif

MK_GC_bool MK_GC_dont_gc = 0;

MK_GC_bool MK_GC_dont_precollect = 0;

MK_GC_bool MK_GC_quiet = 0; /* used also in pcr_interface.c */

#ifndef SMALL_CONFIG
  MK_GC_bool MK_GC_print_stats = 0;
#endif

#ifdef MK_GC_PRINT_BACK_HEIGHT
  MK_GC_INNER MK_GC_bool MK_GC_print_back_height = TRUE;
#else
  MK_GC_INNER MK_GC_bool MK_GC_print_back_height = FALSE;
#endif

#ifndef NO_DEBUGGING
  MK_GC_INNER MK_GC_bool MK_GC_dump_regularly = FALSE;
                                /* Generate regular debugging dumps. */
#endif

#ifdef KEEP_BACK_PTRS
  MK_GC_INNER long MK_GC_backtraces = 0;
                /* Number of random backtraces to generate for each GC. */
#endif

#ifdef FIND_LEAK
  int MK_GC_find_leak = 1;
#else
  int MK_GC_find_leak = 0;
#endif

#ifndef SHORT_DBG_HDRS
# ifdef MK_GC_FINDLEAK_DELAY_FREE
    MK_GC_INNER MK_GC_bool MK_GC_findleak_delay_free = TRUE;
# else
    MK_GC_INNER MK_GC_bool MK_GC_findleak_delay_free = FALSE;
# endif
#endif /* !SHORT_DBG_HDRS */

#ifdef ALL_INTERIOR_POINTERS
  int MK_GC_all_interior_pointers = 1;
#else
  int MK_GC_all_interior_pointers = 0;
#endif

#ifdef MK_GC_FORCE_UNMAP_ON_GCOLLECT
  /* Has no effect unless USE_MUNMAP.                           */
  /* Has no effect on implicitly-initiated garbage collections. */
  MK_GC_INNER MK_GC_bool MK_GC_force_unmap_on_gcollect = TRUE;
#else
  MK_GC_INNER MK_GC_bool MK_GC_force_unmap_on_gcollect = FALSE;
#endif

#ifndef MK_GC_LARGE_ALLOC_WARN_INTERVAL
# define MK_GC_LARGE_ALLOC_WARN_INTERVAL 5
#endif
MK_GC_INNER long MK_GC_large_alloc_warn_interval = MK_GC_LARGE_ALLOC_WARN_INTERVAL;
                        /* Interval between unsuppressed warnings.      */

/*ARGSUSED*/
STATIC void * MK_GC_CALLBACK MK_GC_default_oom_fn(size_t bytes_requested)
{
    return(0);
}

/* All accesses to it should be synchronized to avoid data races.       */
MK_GC_oom_func MK_GC_oom_fn = MK_GC_default_oom_fn;

#ifdef CAN_HANDLE_FORK
# ifdef HANDLE_FORK
    MK_GC_INNER MK_GC_bool MK_GC_handle_fork = TRUE;
                        /* The value is examined by MK_GC_thr_init.        */
# else
    MK_GC_INNER MK_GC_bool MK_GC_handle_fork = FALSE;
# endif
#endif /* CAN_HANDLE_FORK */

/* Overrides the default handle-fork mode.  Non-zero value means GC     */
/* should install proper pthread_atfork handlers (or abort if not       */
/* supported).  Has effect only if called before MK_GC_INIT.               */
/*ARGSUSED*/
MK_GC_API void MK_GC_CALL MK_GC_set_handle_fork(int value)
{
# ifdef CAN_HANDLE_FORK
    if (!MK_GC_is_initialized)
      MK_GC_handle_fork = (MK_GC_bool)value;
# elif defined(THREADS) || (defined(DARWIN) && defined(MPROTECT_VDB))
    if (!MK_GC_is_initialized && value)
      ABORT("fork() handling disabled");
# else
    /* No at-fork handler is needed in the single-threaded mode.        */
# endif
}

/* Set things up so that MK_GC_size_map[i] >= granules(i),                 */
/* but not too much bigger                                              */
/* and so that size_map contains relatively few distinct entries        */
/* This was originally stolen from Russ Atkinson's Cedar                */
/* quantization algorithm (but we precompute it).                       */
STATIC void MK_GC_init_size_map(void)
{
    int i;

    /* Map size 0 to something bigger.                  */
    /* This avoids problems at lower levels.            */
      MK_GC_size_map[0] = 1;
    for (i = 1; i <= GRANULES_TO_BYTES(TINY_FREELISTS-1) - EXTRA_BYTES; i++) {
        MK_GC_size_map[i] = ROUNDED_UP_GRANULES(i);
#       ifndef _MSC_VER
          MK_GC_ASSERT(MK_GC_size_map[i] < TINY_FREELISTS);
          /* Seems to tickle bug in VC++ 2008 for AMD64 */
#       endif
    }
    /* We leave the rest of the array to be filled in on demand. */
}

/* Fill in additional entries in MK_GC_size_map, including the ith one */
/* We assume the ith entry is currently 0.                              */
/* Note that a filled in section of the array ending at n always    */
/* has length at least n/4.                                             */
MK_GC_INNER void MK_GC_extend_size_map(size_t i)
{
    size_t orig_granule_sz = ROUNDED_UP_GRANULES(i);
    size_t granule_sz = orig_granule_sz;
    size_t byte_sz = GRANULES_TO_BYTES(granule_sz);
                        /* The size we try to preserve.         */
                        /* Close to i, unless this would        */
                        /* introduce too many distinct sizes.   */
    size_t smaller_than_i = byte_sz - (byte_sz >> 3);
    size_t much_smaller_than_i = byte_sz - (byte_sz >> 2);
    size_t low_limit;   /* The lowest indexed entry we  */
                        /* initialize.                  */
    size_t j;

    if (MK_GC_size_map[smaller_than_i] == 0) {
        low_limit = much_smaller_than_i;
        while (MK_GC_size_map[low_limit] != 0) low_limit++;
    } else {
        low_limit = smaller_than_i + 1;
        while (MK_GC_size_map[low_limit] != 0) low_limit++;
        granule_sz = ROUNDED_UP_GRANULES(low_limit);
        granule_sz += granule_sz >> 3;
        if (granule_sz < orig_granule_sz) granule_sz = orig_granule_sz;
    }
    /* For these larger sizes, we use an even number of granules.       */
    /* This makes it easier to, for example, construct a 16byte-aligned */
    /* allocator even if GRANULE_BYTES is 8.                            */
        granule_sz += 1;
        granule_sz &= ~1;
    if (granule_sz > MAXOBJGRANULES) {
        granule_sz = MAXOBJGRANULES;
    }
    /* If we can fit the same number of larger objects in a block,      */
    /* do so.                                                   */
    {
        size_t number_of_objs = HBLK_GRANULES/granule_sz;
        granule_sz = HBLK_GRANULES/number_of_objs;
        granule_sz &= ~1;
    }
    byte_sz = GRANULES_TO_BYTES(granule_sz);
    /* We may need one extra byte;                      */
    /* don't always fill in MK_GC_size_map[byte_sz]        */
    byte_sz -= EXTRA_BYTES;

    for (j = low_limit; j <= byte_sz; j++) MK_GC_size_map[j] = granule_sz;
}


/*
 * The following is a gross hack to deal with a problem that can occur
 * on machines that are sloppy about stack frame sizes, notably SPARC.
 * Bogus pointers may be written to the stack and not cleared for
 * a LONG time, because they always fall into holes in stack frames
 * that are not written.  We partially address this by clearing
 * sections of the stack whenever we get control.
 */
# ifdef THREADS
#   define BIG_CLEAR_SIZE 2048  /* Clear this much now and then.        */
#   define SMALL_CLEAR_SIZE 256 /* Clear this much every time.          */
# else
  STATIC word MK_GC_stack_last_cleared = 0; /* MK_GC_no when we last did this */
  STATIC ptr_t MK_GC_min_sp = NULL;
                        /* Coolest stack pointer value from which       */
                        /* we've already cleared the stack.             */
  STATIC ptr_t MK_GC_high_water = NULL;
                        /* "hottest" stack pointer value we have seen   */
                        /* recently.  Degrades over time.               */
  STATIC word MK_GC_bytes_allocd_at_reset = 0;
#   define DEGRADE_RATE 50
# endif

# define CLEAR_SIZE 213  /* Granularity for MK_GC_clear_stack_inner */

#if defined(ASM_CLEAR_CODE)
  void *MK_GC_clear_stack_inner(void *, ptr_t);
#else
  /* Clear the stack up to about limit.  Return arg.  This function is  */
  /* not static because it could also be errorneously defined in .S     */
  /* file, so this error would be caught by the linker.                 */
  /*ARGSUSED*/
  void * MK_GC_clear_stack_inner(void *arg, ptr_t limit)
  {
    volatile word dummy[CLEAR_SIZE];

    BZERO((/* no volatile */ void *)dummy, sizeof(dummy));
    if ((word)MK_GC_approx_sp() COOLER_THAN (word)limit) {
        (void) MK_GC_clear_stack_inner(arg, limit);
    }
    /* Make sure the recursive call is not a tail call, and the bzero   */
    /* call is not recognized as dead code.                             */
    MK_GC_noop1((word)dummy);
    return(arg);
  }
#endif

/* Clear some of the inaccessible part of the stack.  Returns its       */
/* argument, so it can be used in a tail call position, hence clearing  */
/* another frame.                                                       */
MK_GC_API void * MK_GC_CALL MK_GC_clear_stack(void *arg)
{
    ptr_t sp = MK_GC_approx_sp();  /* Hotter than actual sp */
#   ifdef THREADS
        word dummy[SMALL_CLEAR_SIZE];
        static unsigned random_no = 0;
                                 /* Should be more random than it is ... */
                                 /* Used to occasionally clear a bigger  */
                                 /* chunk.                               */
#   endif
    ptr_t limit;

#   define SLOP 400
        /* Extra bytes we clear every time.  This clears our own        */
        /* activation record, and should cause more frequent            */
        /* clearing near the cold end of the stack, a good thing.       */
#   define MK_GC_SLOP 4000
        /* We make MK_GC_high_water this much hotter than we really saw    */
        /* saw it, to cover for GC noise etc. above our current frame.  */
#   define CLEAR_THRESHOLD 100000
        /* We restart the clearing process after this many bytes of     */
        /* allocation.  Otherwise very heavily recursive programs       */
        /* with sparse stacks may result in heaps that grow almost      */
        /* without bounds.  As the heap gets larger, collection         */
        /* frequency decreases, thus clearing frequency would decrease, */
        /* thus more junk remains accessible, thus the heap gets        */
        /* larger ...                                                   */
# ifdef THREADS
    if (++random_no % 13 == 0) {
        limit = sp;
        MAKE_HOTTER(limit, BIG_CLEAR_SIZE*sizeof(word));
        limit = (ptr_t)((word)limit & ~0xf);
                        /* Make it sufficiently aligned for assembly    */
                        /* implementations of MK_GC_clear_stack_inner.     */
        return MK_GC_clear_stack_inner(arg, limit);
    } else {
        BZERO(dummy, SMALL_CLEAR_SIZE*sizeof(word));
        return arg;
    }
# else
    if (MK_GC_gc_no > MK_GC_stack_last_cleared) {
        /* Start things over, so we clear the entire stack again */
        if (MK_GC_stack_last_cleared == 0) MK_GC_high_water = (ptr_t)MK_GC_stackbottom;
        MK_GC_min_sp = MK_GC_high_water;
        MK_GC_stack_last_cleared = MK_GC_gc_no;
        MK_GC_bytes_allocd_at_reset = MK_GC_bytes_allocd;
    }
    /* Adjust MK_GC_high_water */
        MAKE_COOLER(MK_GC_high_water, WORDS_TO_BYTES(DEGRADE_RATE) + MK_GC_SLOP);
        if (sp HOTTER_THAN MK_GC_high_water) {
            MK_GC_high_water = sp;
        }
        MAKE_HOTTER(MK_GC_high_water, MK_GC_SLOP);
    limit = MK_GC_min_sp;
    MAKE_HOTTER(limit, SLOP);
    if (sp COOLER_THAN limit) {
        limit = (ptr_t)((word)limit & ~0xf);
                        /* Make it sufficiently aligned for assembly    */
                        /* implementations of MK_GC_clear_stack_inner.     */
        MK_GC_min_sp = sp;
        return(MK_GC_clear_stack_inner(arg, limit));
    } else if (MK_GC_bytes_allocd - MK_GC_bytes_allocd_at_reset > CLEAR_THRESHOLD) {
        /* Restart clearing process, but limit how much clearing we do. */
        MK_GC_min_sp = sp;
        MAKE_HOTTER(MK_GC_min_sp, CLEAR_THRESHOLD/4);
        if (MK_GC_min_sp HOTTER_THAN MK_GC_high_water) MK_GC_min_sp = MK_GC_high_water;
        MK_GC_bytes_allocd_at_reset = MK_GC_bytes_allocd;
    }
    return(arg);
# endif
}


/* Return a pointer to the base address of p, given a pointer to a      */
/* an address within an object.  Return 0 o.w.                          */
MK_GC_API void * MK_GC_CALL MK_GC_base(void * p)
{
    ptr_t r;
    struct hblk *h;
    bottom_index *bi;
    hdr *candidate_hdr;
    ptr_t limit;

    r = p;
    if (!MK_GC_is_initialized) return 0;
    h = HBLKPTR(r);
    GET_BI(r, bi);
    candidate_hdr = HDR_FROM_BI(bi, r);
    if (candidate_hdr == 0) return(0);
    /* If it's a pointer to the middle of a large object, move it       */
    /* to the beginning.                                                */
        while (IS_FORWARDING_ADDR_OR_NIL(candidate_hdr)) {
           h = FORWARDED_ADDR(h,candidate_hdr);
           r = (ptr_t)h;
           candidate_hdr = HDR(h);
        }
    if (HBLK_IS_FREE(candidate_hdr)) return(0);
    /* Make sure r points to the beginning of the object */
        r = (ptr_t)((word)r & ~(WORDS_TO_BYTES(1) - 1));
        {
            size_t offset = HBLKDISPL(r);
            word sz = candidate_hdr -> hb_sz;
            size_t obj_displ = offset % sz;

            r -= obj_displ;
            limit = r + sz;
            if (limit > (ptr_t)(h + 1) && sz <= HBLKSIZE) {
                return(0);
            }
            if ((ptr_t)p >= limit) return(0);
        }
    return((void *)r);
}


/* Return the size of an object, given a pointer to its base.           */
/* (For small objects this also happens to work from interior pointers, */
/* but that shouldn't be relied upon.)                                  */
MK_GC_API size_t MK_GC_CALL MK_GC_size(const void * p)
{
    hdr * hhdr = HDR(p);

    return hhdr -> hb_sz;
}


/* These getters remain unsynchronized for compatibility (since some    */
/* clients could call some of them from a GC callback holding the       */
/* allocator lock).                                                     */
MK_GC_API size_t MK_GC_CALL MK_GC_get_heap_size(void)
{
    /* ignore the memory space returned to OS (i.e. count only the      */
    /* space owned by the garbage collector)                            */
    return (size_t)(MK_GC_heapsize - MK_GC_unmapped_bytes);
}

MK_GC_API size_t MK_GC_CALL MK_GC_get_free_bytes(void)
{
    /* ignore the memory space returned to OS */
    return (size_t)(MK_GC_large_free_bytes - MK_GC_unmapped_bytes);
}

MK_GC_API size_t MK_GC_CALL MK_GC_get_unmapped_bytes(void)
{
    return (size_t)MK_GC_unmapped_bytes;
}

MK_GC_API size_t MK_GC_CALL MK_GC_get_bytes_since_gc(void)
{
    return (size_t)MK_GC_bytes_allocd;
}

MK_GC_API size_t MK_GC_CALL MK_GC_get_total_bytes(void)
{
    return (size_t)(MK_GC_bytes_allocd + MK_GC_bytes_allocd_before_gc);
}

/* Return the heap usage information.  This is a thread-safe (atomic)   */
/* alternative for the five above getters.  NULL pointer is allowed for */
/* any argument.  Returned (filled in) values are of word type.         */
MK_GC_API void MK_GC_CALL MK_GC_get_heap_usage_safe(MK_GC_word *pheap_size,
                        MK_GC_word *pfree_bytes, MK_GC_word *punmapped_bytes,
                        MK_GC_word *pbytes_since_gc, MK_GC_word *ptotal_bytes)
{
  DCL_LOCK_STATE;

  LOCK();
  if (pheap_size != NULL)
    *pheap_size = MK_GC_heapsize - MK_GC_unmapped_bytes;
  if (pfree_bytes != NULL)
    *pfree_bytes = MK_GC_large_free_bytes - MK_GC_unmapped_bytes;
  if (punmapped_bytes != NULL)
    *punmapped_bytes = MK_GC_unmapped_bytes;
  if (pbytes_since_gc != NULL)
    *pbytes_since_gc = MK_GC_bytes_allocd;
  if (ptotal_bytes != NULL)
    *ptotal_bytes = MK_GC_bytes_allocd + MK_GC_bytes_allocd_before_gc;
  UNLOCK();
}


#ifdef THREADS
  MK_GC_API int MK_GC_CALL MK_GC_get_suspend_signal(void)
  {
#   ifdef SIG_SUSPEND
      return SIG_SUSPEND;
#   else
      return -1;
#   endif
  }
#endif /* THREADS */

#if defined(MK_GC_LINUX_THREADS) /* JCB */
static int suspend_signal = SIG_SUSPEND_DEFAULT;
static int thread_restart_signal = SIG_THR_RESTART_DEFAULT;

void MK_GC_set_suspend_signal(const int sig)
{
  if (MK_GC_is_initialized) return;
  suspend_signal = sig;
}

void MK_GC_set_thread_restart_signal(const int sig)
{
  if (MK_GC_is_initialized) return;
  thread_restart_signal = sig;
}

int MK_GC_suspend_signal(void) { return suspend_signal; }
int MK_GC_thread_restart_signal(void) { return thread_restart_signal; }
#endif /* defined(MK_GC_LINUX_THREADS) */ /* JCB */


#if !defined(_MAX_PATH) && (defined(MSWIN32) || defined(MSWINCE) \
                            || defined(CYGWIN32))
# define _MAX_PATH MAX_PATH
#endif

#ifdef MK_GC_READ_ENV_FILE
  /* This works for Win32/WinCE for now.  Really useful only for WinCE. */
  STATIC char *MK_GC_envfile_content = NULL;
                        /* The content of the GC "env" file with CR and */
                        /* LF replaced to '\0'.  NULL if the file is    */
                        /* missing or empty.  Otherwise, always ends    */
                        /* with '\0'.                                   */
  STATIC unsigned MK_GC_envfile_length = 0;
                        /* Length of MK_GC_envfile_content (if non-NULL).  */

# ifndef MK_GC_ENVFILE_MAXLEN
#   define MK_GC_ENVFILE_MAXLEN 0x4000
# endif

  /* The routine initializes MK_GC_envfile_content from the GC "env" file. */
  STATIC void MK_GC_envfile_init(void)
  {
#   if defined(MSWIN32) || defined(MSWINCE) || defined(CYGWIN32)
      HANDLE hFile;
      char *content;
      unsigned ofs;
      unsigned len;
      DWORD nBytesRead;
      TCHAR path[_MAX_PATH + 0x10]; /* buffer for path + ext */
      len = (unsigned)GetModuleFileName(NULL /* hModule */, path,
                                        _MAX_PATH + 1);
      /* If GetModuleFileName() has failed then len is 0. */
      if (len > 4 && path[len - 4] == (TCHAR)'.') {
        len -= 4; /* strip executable file extension */
      }
      memcpy(&path[len], TEXT(".gc.env"), sizeof(TEXT(".gc.env")));
      hFile = CreateFile(path, GENERIC_READ,
                         FILE_SHARE_READ | FILE_SHARE_WRITE,
                         NULL /* lpSecurityAttributes */, OPEN_EXISTING,
                         FILE_ATTRIBUTE_NORMAL, NULL /* hTemplateFile */);
      if (hFile == INVALID_HANDLE_VALUE)
        return; /* the file is absent or the operation is failed */
      len = (unsigned)GetFileSize(hFile, NULL);
      if (len <= 1 || len >= MK_GC_ENVFILE_MAXLEN) {
        CloseHandle(hFile);
        return; /* invalid file length - ignoring the file content */
      }
      /* At this execution point, MK_GC_setpagesize() and MK_GC_init_win32()  */
      /* must already be called (for GET_MEM() to work correctly).      */
      content = (char *)GET_MEM(len + 1);
      if (content == NULL) {
        CloseHandle(hFile);
        return; /* allocation failure */
      }
      ofs = 0;
      nBytesRead = (DWORD)-1L;
          /* Last ReadFile() call should clear nBytesRead on success. */
      while (ReadFile(hFile, content + ofs, len - ofs + 1, &nBytesRead,
                      NULL /* lpOverlapped */) && nBytesRead != 0) {
        if ((ofs += nBytesRead) > len)
          break;
      }
      CloseHandle(hFile);
      if (ofs != len || nBytesRead != 0)
        return; /* read operation is failed - ignoring the file content */
      content[ofs] = '\0';
      while (ofs-- > 0) {
       if (content[ofs] == '\r' || content[ofs] == '\n')
         content[ofs] = '\0';
      }
      MK_GC_envfile_length = len + 1;
      MK_GC_envfile_content = content;
#   endif
  }

  /* This routine scans MK_GC_envfile_content for the specified            */
  /* environment variable (and returns its value if found).             */
  MK_GC_INNER char * MK_GC_envfile_getenv(const char *name)
  {
    char *p;
    char *end_of_content;
    unsigned namelen;
#   ifndef NO_GETENV
      p = getenv(name); /* try the standard getenv() first */
      if (p != NULL)
        return *p != '\0' ? p : NULL;
#   endif
    p = MK_GC_envfile_content;
    if (p == NULL)
      return NULL; /* "env" file is absent (or empty) */
    namelen = strlen(name);
    if (namelen == 0) /* a sanity check */
      return NULL;
    for (end_of_content = p + MK_GC_envfile_length;
         p != end_of_content; p += strlen(p) + 1) {
      if (strncmp(p, name, namelen) == 0 && *(p += namelen) == '=') {
        p++; /* the match is found; skip '=' */
        return *p != '\0' ? p : NULL;
      }
      /* If not matching then skip to the next line. */
    }
    return NULL; /* no match found */
  }
#endif /* MK_GC_READ_ENV_FILE */

MK_GC_INNER MK_GC_bool MK_GC_is_initialized = FALSE;

#if (defined(MSWIN32) || defined(MSWINCE)) && defined(THREADS)
    MK_GC_INNER CRITICAL_SECTION MK_GC_write_cs;
#endif

STATIC void MK_GC_exit_check(void)
{
   MK_GC_gcollect();
}

#ifdef UNIX_LIKE
  static void looping_handler(int sig)
  {
    MK_GC_err_printf("Caught signal %d: looping in handler\n", sig);
    for (;;) {}
  }

  static MK_GC_bool installed_looping_handler = FALSE;

  static void maybe_install_looping_handler(void)
  {
    /* Install looping handler before the write fault handler, so we    */
    /* handle write faults correctly.                                   */
    if (!installed_looping_handler && 0 != GETENV("MK_GC_LOOP_ON_ABORT")) {
      MK_GC_set_and_save_fault_handler(looping_handler);
      installed_looping_handler = TRUE;
    }
  }

#else /* !UNIX_LIKE */
# define maybe_install_looping_handler()
#endif

#if !defined(OS2) && !defined(MACOS) && !defined(MSWIN32) && !defined(MSWINCE)
  STATIC int MK_GC_stdout = 1;
  STATIC int MK_GC_stderr = 2;
  STATIC int MK_GC_log = 2; /* stderr */
#endif

STATIC word MK_GC_parse_mem_size_arg(const char *str)
{
  char *endptr;
  word result = 0; /* bad value */
  char ch;

  if (*str != '\0') {
    result = (word)STRTOULL(str, &endptr, 10);
    ch = *endptr;
    if (ch != '\0') {
      if (*(endptr + 1) != '\0')
        return 0;
      /* Allow k, M or G suffix. */
      switch (ch) {
      case 'K':
      case 'k':
        result <<= 10;
        break;
      case 'M':
      case 'm':
        result <<= 20;
        break;
      case 'G':
      case 'g':
        result <<= 30;
        break;
      default:
        result = 0;
      }
    }
  }
  return result;
}

MK_GC_API void MK_GC_CALL MK_GC_init(void)
{
    /* LOCK(); -- no longer does anything this early. */
    word initial_heap_sz;
    IF_CANCEL(int cancel_state;)

    if (MK_GC_is_initialized) return;
#   ifdef REDIRECT_MALLOC
      {
        static MK_GC_bool init_started = FALSE;
        if (init_started)
          ABORT("Redirected malloc() called during GC init");
        init_started = TRUE;
      }
#   endif

#   ifdef MK_GC_INITIAL_HEAP_SIZE
      initial_heap_sz = divHBLKSZ(MK_GC_INITIAL_HEAP_SIZE);
#   else
      initial_heap_sz = (word)MINHINCR;
#   endif
    DISABLE_CANCEL(cancel_state);
    /* Note that although we are nominally called with the */
    /* allocation lock held, the allocation lock is now    */
    /* only really acquired once a second thread is forked.*/
    /* And the initialization code needs to run before     */
    /* then.  Thus we really don't hold any locks, and can */
    /* in fact safely initialize them here.                */
#   ifdef THREADS
      MK_GC_ASSERT(!MK_GC_need_to_lock);
#     ifdef SN_TARGET_PS3
        {
          pthread_mutexattr_t mattr;
          pthread_mutexattr_init(&mattr);
          pthread_mutex_init(&MK_GC_allocate_ml, &mattr);
          pthread_mutexattr_destroy(&mattr);
        }
#     endif
#   endif /* THREADS */
#   if defined(MK_GC_WIN32_THREADS) && !defined(MK_GC_PTHREADS)
     {
#     ifndef MSWINCE
        BOOL (WINAPI *pfn) (LPCRITICAL_SECTION, DWORD) = NULL;
        HMODULE hK32 = GetModuleHandle(TEXT("kernel32.dll"));
        if (hK32)
          pfn = (BOOL (WINAPI *) (LPCRITICAL_SECTION, DWORD))
                GetProcAddress (hK32,
                                "InitializeCriticalSectionAndSpinCount");
        if (pfn)
            pfn(&MK_GC_allocate_ml, 4000);
        else
#     endif /* !MSWINCE */
        /* else */ InitializeCriticalSection (&MK_GC_allocate_ml);
     }
#   endif /* MK_GC_WIN32_THREADS */
#   if (defined(MSWIN32) || defined(MSWINCE)) && defined(THREADS)
      InitializeCriticalSection(&MK_GC_write_cs);
#   endif
    MK_GC_setpagesize();
#   ifdef MSWIN32
      MK_GC_init_win32();
#   endif
#   ifdef MK_GC_READ_ENV_FILE
      MK_GC_envfile_init();
#   endif
#   ifndef SMALL_CONFIG
#     ifdef MK_GC_PRINT_VERBOSE_STATS
        /* This is useful for debugging and profiling on platforms with */
        /* missing getenv() (like WinCE).                               */
        MK_GC_print_stats = VERBOSE;
#     else
        if (0 != GETENV("MK_GC_PRINT_VERBOSE_STATS")) {
          MK_GC_print_stats = VERBOSE;
        } else if (0 != GETENV("MK_GC_PRINT_STATS")) {
          MK_GC_print_stats = 1;
        }
#     endif
#     if defined(UNIX_LIKE) || defined(CYGWIN32)
        {
          char * file_name = GETENV("MK_GC_LOG_FILE");
          if (0 != file_name) {
            int log_d = open(file_name, O_CREAT|O_WRONLY|O_APPEND, 0666);
            if (log_d < 0) {
              MK_GC_err_printf("Failed to open %s as log file\n", file_name);
            } else {
              char *str;
              MK_GC_log = log_d;
              str = GETENV("MK_GC_ONLY_LOG_TO_FILE");
#             ifdef MK_GC_ONLY_LOG_TO_FILE
                /* The similar environment variable set to "0"  */
                /* overrides the effect of the macro defined.   */
                if (str != NULL && *str == '0' && *(str + 1) == '\0')
#             else
                /* Otherwise setting the environment variable   */
                /* to anything other than "0" will prevent from */
                /* redirecting stdout/err to the log file.      */
                if (str == NULL || (*str == '0' && *(str + 1) == '\0'))
#             endif
              {
                MK_GC_stdout = log_d;
                MK_GC_stderr = log_d;
              }
            }
          }
        }
#     endif
#   endif /* !SMALL_CONFIG */
#   ifndef NO_DEBUGGING
      if (0 != GETENV("MK_GC_DUMP_REGULARLY")) {
        MK_GC_dump_regularly = TRUE;
      }
#   endif
#   ifdef KEEP_BACK_PTRS
      {
        char * backtraces_string = GETENV("MK_GC_BACKTRACES");
        if (0 != backtraces_string) {
          MK_GC_backtraces = atol(backtraces_string);
          if (backtraces_string[0] == '\0') MK_GC_backtraces = 1;
        }
      }
#   endif
    if (0 != GETENV("MK_GC_FIND_LEAK")) {
      MK_GC_find_leak = 1;
    }
#   ifndef SHORT_DBG_HDRS
      if (0 != GETENV("MK_GC_FINDLEAK_DELAY_FREE")) {
        MK_GC_findleak_delay_free = TRUE;
      }
#   endif
    if (0 != GETENV("MK_GC_ALL_INTERIOR_POINTERS")) {
      MK_GC_all_interior_pointers = 1;
    }
    if (0 != GETENV("MK_GC_DONT_GC")) {
      MK_GC_dont_gc = 1;
    }
    if (0 != GETENV("MK_GC_PRINT_BACK_HEIGHT")) {
      MK_GC_print_back_height = TRUE;
    }
    if (0 != GETENV("MK_GC_NO_BLACKLIST_WARNING")) {
      MK_GC_large_alloc_warn_interval = LONG_MAX;
    }
    {
      char * addr_string = GETENV("MK_GC_TRACE");
      if (0 != addr_string) {
#       ifndef ENABLE_TRACE
          WARN("Tracing not enabled: Ignoring MK_GC_TRACE value\n", 0);
#       else
          word addr = (word)STRTOULL(addr_string, NULL, 16);
          if (addr < 0x1000)
              WARN("Unlikely trace address: %p\n", addr);
          MK_GC_trace_addr = (ptr_t)addr;
#       endif
      }
    }
#   ifndef MK_GC_DISABLE_INCREMENTAL
      {
        char * time_limit_string = GETENV("MK_GC_PAUSE_TIME_TARGET");
        if (0 != time_limit_string) {
          long time_limit = atol(time_limit_string);
          if (time_limit < 5) {
            WARN("MK_GC_PAUSE_TIME_TARGET environment variable value too small "
                 "or bad syntax: Ignoring\n", 0);
          } else {
            MK_GC_time_limit = time_limit;
          }
        }
      }
#   endif
#   ifndef SMALL_CONFIG
      {
        char * full_freq_string = GETENV("MK_GC_FULL_FREQUENCY");
        if (full_freq_string != NULL) {
          int full_freq = atoi(full_freq_string);
          if (full_freq > 0)
            MK_GC_full_freq = full_freq;
        }
      }
#   endif
    {
      char * interval_string = GETENV("MK_GC_LARGE_ALLOC_WARN_INTERVAL");
      if (0 != interval_string) {
        long interval = atol(interval_string);
        if (interval <= 0) {
          WARN("MK_GC_LARGE_ALLOC_WARN_INTERVAL environment variable has "
               "bad value: Ignoring\n", 0);
        } else {
          MK_GC_large_alloc_warn_interval = interval;
        }
      }
    }
    {
        char * space_divisor_string = GETENV("MK_GC_FREE_SPACE_DIVISOR");
        if (space_divisor_string != NULL) {
          int space_divisor = atoi(space_divisor_string);
          if (space_divisor > 0)
            MK_GC_free_space_divisor = (MK_GC_word)space_divisor;
        }
    }
#   ifdef USE_MUNMAP
      {
        char * string = GETENV("MK_GC_UNMAP_THRESHOLD");
        if (string != NULL) {
          if (*string == '0' && *(string + 1) == '\0') {
            /* "0" is used to disable unmapping. */
            MK_GC_unmap_threshold = 0;
          } else {
            int unmap_threshold = atoi(string);
            if (unmap_threshold > 0)
              MK_GC_unmap_threshold = unmap_threshold;
          }
        }
      }
      {
        char * string = GETENV("MK_GC_FORCE_UNMAP_ON_GCOLLECT");
        if (string != NULL) {
          if (*string == '0' && *(string + 1) == '\0') {
            /* "0" is used to turn off the mode. */
            MK_GC_force_unmap_on_gcollect = FALSE;
          } else {
            MK_GC_force_unmap_on_gcollect = TRUE;
          }
        }
      }
      {
        char * string = GETENV("MK_GC_USE_ENTIRE_HEAP");
        if (string != NULL) {
          if (*string == '0' && *(string + 1) == '\0') {
            /* "0" is used to turn off the mode. */
            MK_GC_use_entire_heap = FALSE;
          } else {
            MK_GC_use_entire_heap = TRUE;
          }
        }
      }
#   endif
    maybe_install_looping_handler();
    /* Adjust normal object descriptor for extra allocation.    */
    if (ALIGNMENT > MK_GC_DS_TAGS && EXTRA_BYTES != 0) {
      MK_GC_obj_kinds[NORMAL].ok_descriptor = ((word)(-ALIGNMENT) | MK_GC_DS_LENGTH);
    }
    MK_GC_exclude_static_roots_inner(beginMK_GC_arrays, endMK_GC_arrays);
    MK_GC_exclude_static_roots_inner(beginMK_GC_obj_kinds, endMK_GC_obj_kinds);
#   ifdef SEPARATE_GLOBALS
      MK_GC_exclude_static_roots_inner(beginMK_GC_objfreelist, endMK_GC_objfreelist);
      MK_GC_exclude_static_roots_inner(beginMK_GC_aobjfreelist, endMK_GC_aobjfreelist);
#   endif
#   if defined(USE_PROC_FOR_LIBRARIES) && defined(MK_GC_LINUX_THREADS)
        WARN("USE_PROC_FOR_LIBRARIES + MK_GC_LINUX_THREADS performs poorly.\n", 0);
        /* If thread stacks are cached, they tend to be scanned in      */
        /* entirety as part of the root set.  This wil grow them to     */
        /* maximum size, and is generally not desirable.                */
#   endif
#   if defined(SEARCH_FOR_DATA_START)
        MK_GC_init_linux_data_start();
#   endif
#   if defined(NETBSD) && defined(__ELF__)
        MK_GC_init_netbsd_elf();
#   endif
#   if !defined(THREADS) || defined(MK_GC_PTHREADS) \
        || defined(MK_GC_WIN32_THREADS) || defined(MK_GC_SOLARIS_THREADS)
      if (MK_GC_stackbottom == 0) {
        MK_GC_stackbottom = MK_GC_get_main_stack_base();
#       if (defined(LINUX) || defined(HPUX)) && defined(IA64)
          MK_GC_register_stackbottom = MK_GC_get_register_stack_base();
#       endif
      } else {
#       if (defined(LINUX) || defined(HPUX)) && defined(IA64)
          if (MK_GC_register_stackbottom == 0) {
            WARN("MK_GC_register_stackbottom should be set with MK_GC_stackbottom\n", 0);
            /* The following may fail, since we may rely on             */
            /* alignment properties that may not hold with a user set   */
            /* MK_GC_stackbottom.                                          */
            MK_GC_register_stackbottom = MK_GC_get_register_stack_base();
          }
#       endif
      }
#   endif
    MK_GC_STATIC_ASSERT(sizeof (ptr_t) == sizeof(word));
    MK_GC_STATIC_ASSERT(sizeof (signed_word) == sizeof(word));
    MK_GC_STATIC_ASSERT(sizeof (struct hblk) == HBLKSIZE);
#   ifndef THREADS
      MK_GC_ASSERT(!((word)MK_GC_stackbottom HOTTER_THAN (word)MK_GC_approx_sp()));
#   endif
#   if !defined(_AUX_SOURCE) || defined(__GNUC__)
      MK_GC_STATIC_ASSERT((word)(-1) > (word)0);
      /* word should be unsigned */
#   endif
#   if !defined(__BORLANDC__) && !defined(__CC_ARM) \
       && !(defined(__clang__) && defined(X86_64)) /* Workaround */
      MK_GC_STATIC_ASSERT((ptr_t)(word)(-1) > (ptr_t)0);
      /* Ptr_t comparisons should behave as unsigned comparisons.       */
#   endif
    MK_GC_STATIC_ASSERT((signed_word)(-1) < (signed_word)0);
#   ifndef MK_GC_DISABLE_INCREMENTAL
      if (MK_GC_incremental || 0 != GETENV("MK_GC_ENABLE_INCREMENTAL")) {
        /* For GWW_VDB on Win32, this needs to happen before any        */
        /* heap memory is allocated.                                    */
        MK_GC_dirty_init();
        MK_GC_ASSERT(MK_GC_bytes_allocd == 0);
        MK_GC_incremental = TRUE;
      }
#   endif

    /* Add initial guess of root sets.  Do this first, since sbrk(0)    */
    /* might be used.                                                   */
      if (MK_GC_REGISTER_MAIN_STATIC_DATA()) MK_GC_register_data_segments();
    MK_GC_init_headers();
    MK_GC_bl_init();
    MK_GC_mark_init();
    {
        char * sz_str = GETENV("MK_GC_INITIAL_HEAP_SIZE");
        if (sz_str != NULL) {
          initial_heap_sz = MK_GC_parse_mem_size_arg(sz_str);
          if (initial_heap_sz <= MINHINCR * HBLKSIZE) {
            WARN("Bad initial heap size %s - ignoring it.\n", sz_str);
          }
          initial_heap_sz = divHBLKSZ(initial_heap_sz);
        }
    }
    {
        char * sz_str = GETENV("MK_GC_MAXIMUM_HEAP_SIZE");
        if (sz_str != NULL) {
          word max_heap_sz = MK_GC_parse_mem_size_arg(sz_str);
          if (max_heap_sz < initial_heap_sz * HBLKSIZE) {
            WARN("Bad maximum heap size %s - ignoring it.\n", sz_str);
          }
          if (0 == MK_GC_max_retries) MK_GC_max_retries = 2;
          MK_GC_set_max_heap_size(max_heap_sz);
        }
    }
    if (!MK_GC_expand_hp_inner(initial_heap_sz)) {
        MK_GC_err_printf("Can't start up: not enough memory\n");
        EXIT();
    }
    if (MK_GC_all_interior_pointers)
      MK_GC_initialize_offsets();
    MK_GC_register_displacement_inner(0L);
#   if defined(MK_GC_LINUX_THREADS) && defined(REDIRECT_MALLOC)
      if (!MK_GC_all_interior_pointers) {
        /* TLS ABI uses pointer-sized offsets for dtv. */
        MK_GC_register_displacement_inner(sizeof(void *));
      }
#   endif
    MK_GC_init_size_map();
#   ifdef PCR
      if (PCR_IL_Lock(PCR_Bool_false, PCR_allSigsBlocked, PCR_waitForever)
          != PCR_ERes_okay) {
          ABORT("Can't lock load state");
      } else if (PCR_IL_Unlock() != PCR_ERes_okay) {
          ABORT("Can't unlock load state");
      }
      PCR_IL_Unlock();
      MK_GC_pcr_install();
#   endif
    MK_GC_is_initialized = TRUE;
#   if defined(MK_GC_PTHREADS) || defined(MK_GC_WIN32_THREADS)
        MK_GC_thr_init();
#   endif
    COND_DUMP;
    /* Get black list set up and/or incremental GC started */
      if (!MK_GC_dont_precollect || MK_GC_incremental) MK_GC_gcollect_inner();
#   ifdef STUBBORN_ALLOC
        MK_GC_stubborn_init();
#   endif
    /* Convince lint that some things are used */
#   ifdef LINT
      {
          extern char * const MK_GC_copyright[];
          MK_GC_noop(MK_GC_copyright, MK_GC_find_header, MK_GC_push_one,
                  MK_GC_call_with_alloc_lock, MK_GC_dont_expand,
#                 ifndef NO_DEBUGGING
                    MK_GC_dump,
#                 endif
                  MK_GC_register_finalizer_no_order);
      }
#   endif

    if (MK_GC_find_leak) {
      /* This is to give us at least one chance to detect leaks.        */
      /* This may report some very benign leaks, but ...                */
      atexit(MK_GC_exit_check);
    }

    /* The rest of this again assumes we don't really hold      */
    /* the allocation lock.                                     */
#   if defined(PARALLEL_MARK) || defined(THREAD_LOCAL_ALLOC)
        /* Make sure marker threads are started and thread local */
        /* allocation is initialized, in case we didn't get      */
        /* called from MK_GC_init_parallel.                         */
        MK_GC_init_parallel();
#   endif /* PARALLEL_MARK || THREAD_LOCAL_ALLOC */

#   if defined(DYNAMIC_LOADING) && defined(DARWIN)
        /* This must be called WITHOUT the allocation lock held */
        /* and before any threads are created.                  */
        MK_GC_init_dyld();
#   endif
    RESTORE_CANCEL(cancel_state);
}

MK_GC_API void MK_GC_CALL MK_GC_enable_incremental(void)
{
# if !defined(MK_GC_DISABLE_INCREMENTAL) && !defined(KEEP_BACK_PTRS)
    DCL_LOCK_STATE;
    /* If we are keeping back pointers, the GC itself dirties all */
    /* pages on which objects have been marked, making            */
    /* incremental GC pointless.                                  */
    if (!MK_GC_find_leak && 0 == GETENV("MK_GC_DISABLE_INCREMENTAL")) {
      LOCK();
      if (!MK_GC_incremental) {
        MK_GC_setpagesize();
        /* if (MK_GC_no_win32_dlls) goto out; Should be win32S test? */
        maybe_install_looping_handler(); /* Before write fault handler! */
        MK_GC_incremental = TRUE;
        if (!MK_GC_is_initialized) {
          MK_GC_init();
        } else {
          MK_GC_dirty_init();
        }
        if (MK_GC_dirty_maintained && !MK_GC_dont_gc) {
                                /* Can't easily do it if MK_GC_dont_gc.    */
          if (MK_GC_bytes_allocd > 0) {
            /* There may be unmarked reachable objects. */
            MK_GC_gcollect_inner();
          }
            /* else we're OK in assuming everything's   */
            /* clean since nothing can point to an      */
            /* unmarked object.                         */
          MK_GC_read_dirty();
        }
      }
      UNLOCK();
      return;
    }
# endif
  MK_GC_init();
}

#if defined(MSWIN32) || defined(MSWINCE)

# if defined(_MSC_VER) && defined(_DEBUG) && !defined(MSWINCE)
#   include <crtdbg.h>
# endif

  STATIC HANDLE MK_GC_log = 0;

  void MK_GC_deinit(void)
  {
#   ifdef THREADS
      if (MK_GC_is_initialized) {
        DeleteCriticalSection(&MK_GC_write_cs);
      }
#   endif
  }

# ifdef THREADS
#   ifdef PARALLEL_MARK
#     define IF_NEED_TO_LOCK(x) if (MK_GC_parallel || MK_GC_need_to_lock) x
#   else
#     define IF_NEED_TO_LOCK(x) if (MK_GC_need_to_lock) x
#   endif
# else
#   define IF_NEED_TO_LOCK(x)
# endif /* !THREADS */

  STATIC HANDLE MK_GC_CreateLogFile(void)
  {
#   if !defined(NO_GETENV_WIN32) || !defined(OLD_WIN32_LOG_FILE)
      TCHAR logPath[_MAX_PATH + 0x10]; /* buffer for path + ext */
#   endif
    /* Use GetEnvironmentVariable instead of GETENV() for unicode support. */
#   ifndef NO_GETENV_WIN32
      if (GetEnvironmentVariable(TEXT("MK_GC_LOG_FILE"), logPath,
                                 _MAX_PATH + 1) - 1U >= (DWORD)_MAX_PATH)
#   endif
    {
      /* Env var not found or its value too long.       */
#     ifdef OLD_WIN32_LOG_FILE
        return CreateFile(TEXT("gc.log"), GENERIC_WRITE, FILE_SHARE_READ,
                          NULL /* lpSecurityAttributes */, CREATE_ALWAYS,
                          FILE_FLAG_WRITE_THROUGH, NULL /* hTemplateFile */);
#     else
        int len = (int)GetModuleFileName(NULL /* hModule */, logPath,
                                         _MAX_PATH + 1);
        /* If GetModuleFileName() has failed then len is 0. */
        if (len > 4 && logPath[len - 4] == (TCHAR)'.') {
          len -= 4; /* strip executable file extension */
        }
        /* strcat/wcscat() are deprecated on WinCE, so use memcpy()     */
        memcpy(&logPath[len], TEXT(".gc.log"), sizeof(TEXT(".gc.log")));
#     endif
    }
#   if !defined(NO_GETENV_WIN32) || !defined(OLD_WIN32_LOG_FILE)
      return CreateFile(logPath, GENERIC_WRITE, FILE_SHARE_READ,
                        NULL /* lpSecurityAttributes */, CREATE_ALWAYS,
                        MK_GC_print_stats == VERBOSE ? FILE_ATTRIBUTE_NORMAL :
                            /* immediately flush writes unless very verbose */
                            FILE_ATTRIBUTE_NORMAL | FILE_FLAG_WRITE_THROUGH,
                        NULL /* hTemplateFile */);
#   endif
  }

  STATIC int MK_GC_write(const char *buf, size_t len)
  {
      BOOL tmp;
      DWORD written;
      if (len == 0)
          return 0;
      IF_NEED_TO_LOCK(EnterCriticalSection(&MK_GC_write_cs));
#     ifdef THREADS
        MK_GC_ASSERT(!MK_GC_write_disabled);
#     endif
      if (MK_GC_log == INVALID_HANDLE_VALUE) {
          IF_NEED_TO_LOCK(LeaveCriticalSection(&MK_GC_write_cs));
          return -1;
      } else if (MK_GC_log == 0) {
        MK_GC_log = MK_GC_CreateLogFile();
        /* Ignore open log failure if the collector is built with       */
        /* print_stats always set on.                                   */
#       ifndef MK_GC_PRINT_VERBOSE_STATS
          if (MK_GC_log == INVALID_HANDLE_VALUE)
            ABORT("Open of log file failed");
#       endif
      }
      tmp = WriteFile(MK_GC_log, buf, (DWORD)len, &written, NULL);
      if (!tmp)
          DebugBreak();
#     if defined(_MSC_VER) && defined(_DEBUG)
#         ifdef MSWINCE
              /* There is no CrtDbgReport() in WinCE */
              {
                  WCHAR wbuf[1024];
                  /* Always use Unicode variant of OutputDebugString() */
                  wbuf[MultiByteToWideChar(CP_ACP, 0 /* dwFlags */,
                                buf, len, wbuf,
                                sizeof(wbuf) / sizeof(wbuf[0]) - 1)] = 0;
                  OutputDebugStringW(wbuf);
              }
#         else
              _CrtDbgReport(_CRT_WARN, NULL, 0, NULL, "%.*s", len, buf);
#         endif
#     endif
      IF_NEED_TO_LOCK(LeaveCriticalSection(&MK_GC_write_cs));
      return tmp ? (int)written : -1;
  }

  /* FIXME: This is pretty ugly ... */
# define WRITE(f, buf, len) MK_GC_write(buf, len)

#elif defined(OS2) || defined(MACOS)
  STATIC FILE * MK_GC_stdout = NULL;
  STATIC FILE * MK_GC_stderr = NULL;
  STATIC FILE * MK_GC_log = NULL;

  /* Initialize MK_GC_log (and the friends) passed to MK_GC_write().  */
  STATIC void MK_GC_set_files(void)
  {
    if (MK_GC_stdout == NULL) {
      MK_GC_stdout = stdout;
    }
    if (MK_GC_stderr == NULL) {
      MK_GC_stderr = stderr;
    }
    if (MK_GC_log == NULL) {
      MK_GC_log = stderr;
    }
  }

  MK_GC_INLINE int MK_GC_write(FILE *f, const char *buf, size_t len)
  {
    int res = fwrite(buf, 1, len, f);
    fflush(f);
    return res;
  }

# define WRITE(f, buf, len) (MK_GC_set_files(), MK_GC_write(f, buf, len))

#else
# if !defined(AMIGA) && !defined(__CC_ARM)
#   include <unistd.h>
# endif

  STATIC int MK_GC_write(int fd, const char *buf, size_t len)
  {
#   if defined(ECOS) || defined(NOSYS)
#     ifdef ECOS
        /* FIXME: This seems to be defined nowhere at present.  */
        /* _Jv_diag_write(buf, len); */
#     else
        /* No writing.  */
#     endif
      return len;
#   else
      int bytes_written = 0;
      int result;
      IF_CANCEL(int cancel_state;)

      DISABLE_CANCEL(cancel_state);
      while ((size_t)bytes_written < len) {
#        ifdef MK_GC_SOLARIS_THREADS
             result = syscall(SYS_write, fd, buf + bytes_written,
                                             len - bytes_written);
#        else
             result = write(fd, buf + bytes_written, len - bytes_written);
#        endif
         if (-1 == result) {
             RESTORE_CANCEL(cancel_state);
             return(result);
         }
         bytes_written += result;
      }
      RESTORE_CANCEL(cancel_state);
      return(bytes_written);
#   endif
  }

# define WRITE(f, buf, len) MK_GC_write(f, buf, len)
#endif /* !MSWIN32 && !OS2 && !MACOS */

#define BUFSZ 1024

#ifdef NO_VSNPRINTF
  /* In case this function is missing (eg., in DJGPP v2.0.3).   */
# define vsnprintf(buf, bufsz, format, args) vsprintf(buf, format, args)
#elif defined(_MSC_VER)
# ifdef MSWINCE
    /* _vsnprintf is deprecated in WinCE */
#   define vsnprintf StringCchVPrintfA
# else
#   define vsnprintf _vsnprintf
# endif
#endif
/* A version of printf that is unlikely to call malloc, and is thus safer */
/* to call from the collector in case malloc has been bound to MK_GC_malloc. */
/* Floating point arguments and formats should be avoided, since fp       */
/* conversion is more likely to allocate.                                 */
/* Assumes that no more than BUFSZ-1 characters are written at once.      */
void MK_GC_printf(const char *format, ...)
{
    va_list args;
    char buf[BUFSZ+1];

    if (MK_GC_quiet) return;
    va_start(args, format);
    buf[BUFSZ] = 0x15;
    (void) vsnprintf(buf, BUFSZ, format, args);
    va_end(args);
    if (buf[BUFSZ] != 0x15) ABORT("MK_GC_printf clobbered stack");
    if (WRITE(MK_GC_stdout, buf, strlen(buf)) < 0)
      ABORT("write to stdout failed");
}

void MK_GC_err_printf(const char *format, ...)
{
    va_list args;
    char buf[BUFSZ+1];

    va_start(args, format);
    buf[BUFSZ] = 0x15;
    (void) vsnprintf(buf, BUFSZ, format, args);
    va_end(args);
    if (buf[BUFSZ] != 0x15) ABORT("MK_GC_printf clobbered stack");
    if (WRITE(MK_GC_stderr, buf, strlen(buf)) < 0)
      ABORT("write to stderr failed");
}

void MK_GC_log_printf(const char *format, ...)
{
    va_list args;
    char buf[BUFSZ+1];

    va_start(args, format);
    buf[BUFSZ] = 0x15;
    (void) vsnprintf(buf, BUFSZ, format, args);
    va_end(args);
    if (buf[BUFSZ] != 0x15) ABORT("MK_GC_printf clobbered stack");
    if (WRITE(MK_GC_log, buf, strlen(buf)) < 0)
      ABORT("write to log failed");
}

/* This is equivalent to MK_GC_err_printf("%s",s). */
void MK_GC_err_puts(const char *s)
{
    if (WRITE(MK_GC_stderr, s, strlen(s)) < 0) ABORT("write to stderr failed");
}

STATIC void MK_GC_CALLBACK MK_GC_default_warn_proc(char *msg, MK_GC_word arg)
{
    MK_GC_err_printf(msg, arg);
}

MK_GC_INNER MK_GC_warn_proc MK_GC_current_warn_proc = MK_GC_default_warn_proc;

/* This is recommended for production code (release). */
MK_GC_API void MK_GC_CALLBACK MK_GC_ignore_warn_proc(char *msg, MK_GC_word arg)
{
    if (MK_GC_print_stats) {
      /* Don't ignore warnings if stats printing is on. */
      MK_GC_default_warn_proc(msg, arg);
    }
}

MK_GC_API void MK_GC_CALL MK_GC_set_warn_proc(MK_GC_warn_proc p)
{
    DCL_LOCK_STATE;
    MK_GC_ASSERT(p != 0);
#   ifdef MK_GC_WIN32_THREADS
#     ifdef CYGWIN32
        /* Need explicit MK_GC_INIT call */
        MK_GC_ASSERT(MK_GC_is_initialized);
#     else
        if (!MK_GC_is_initialized) MK_GC_init();
#     endif
#   endif
    LOCK();
    MK_GC_current_warn_proc = p;
    UNLOCK();
}

MK_GC_API MK_GC_warn_proc MK_GC_CALL MK_GC_get_warn_proc(void)
{
    MK_GC_warn_proc result;
    DCL_LOCK_STATE;
    LOCK();
    result = MK_GC_current_warn_proc;
    UNLOCK();
    return(result);
}

static MK_GC_abort_func abort_fn = NULL; /* JCB */

MK_GC_API void MK_GC_CALL MK_GC_set_abort_func(MK_GC_abort_func fn) /* JCB */
{
  abort_fn = fn;
}

#if !defined(PCR) && !defined(SMALL_CONFIG)
  /* Abort the program with a message. msg must not be NULL. */
  void MK_GC_abort(const char *msg)
  {
#   if defined(MSWIN32)
#     ifndef DONT_USE_USER32_DLL
        /* Use static binding to "user32.dll".  */
        (void)MessageBoxA(NULL, msg, "Fatal error in GC", MB_ICONERROR|MB_OK);
#     else
        /* This simplifies linking - resolve "MessageBoxA" at run-time. */
        HINSTANCE hU32 = LoadLibrary(TEXT("user32.dll"));
        if (hU32) {
          FARPROC pfn = GetProcAddress(hU32, "MessageBoxA");
          if (pfn)
            (void)(*(int (WINAPI *)(HWND, LPCSTR, LPCSTR, UINT))pfn)(
                                NULL /* hWnd */, msg, "Fatal error in GC",
                                MB_ICONERROR | MB_OK);
          (void)FreeLibrary(hU32);
        }
#     endif
      /* Also duplicate msg to GC log file.     */
#   endif
      /* Avoid calling MK_GC_err_printf() here, as MK_GC_abort() could be     */
      /* called from it.  Note 1: this is not an atomic output.         */
      /* Note 2: possible write errors are ignored.                     */
      if (WRITE(MK_GC_stderr, (void *)msg, strlen(msg)) >= 0)
        (void)WRITE(MK_GC_stderr, (void *)("\n"), 1);

    if (GETENV("MK_GC_LOOP_ON_ABORT") != NULL) {
            /* In many cases it's easier to debug a running process.    */
            /* It's arguably nicer to sleep, but that makes it harder   */
            /* to look at the thread if the debugger doesn't know much  */
            /* about threads.                                           */
            for(;;) {}
    }
#   ifndef LINT2
      if (!msg) return; /* to suppress compiler warnings in ABORT callers. */
#   endif
#   if defined(MSWIN32) && (defined(NO_DEBUGGING) || defined(LINT2))
      /* A more user-friendly abort after showing fatal message.        */
        _exit(-1); /* exit on error without running "at-exit" callbacks */
#   elif defined(MSWINCE) && defined(NO_DEBUGGING)
        ExitProcess(-1);
#   elif defined(MSWIN32) || defined(MSWINCE)
        DebugBreak();
                /* Note that on a WinCE box, this could be silently     */
                /* ignored (i.e., the program is not aborted).          */
#   else
        (void) abort();
#   endif
  }
#endif /* !SMALL_CONFIG */

static MK_GC_exit_func exit_fn = NULL; /* JCB */

void MK_GC_exit(int status) /* JCB */
{
  if (exit_fn) exit_fn(status);
  (void) exit(status);
}

MK_GC_API void MK_GC_CALL MK_GC_set_exit_func(MK_GC_exit_func fn) /* JCB */
{
  exit_fn = fn;
}

MK_GC_API void MK_GC_CALL MK_GC_enable(void)
{
    DCL_LOCK_STATE;
    LOCK();
    MK_GC_dont_gc--;
    UNLOCK();
}

MK_GC_API void MK_GC_CALL MK_GC_disable(void)
{
    DCL_LOCK_STATE;
    LOCK();
    MK_GC_dont_gc++;
    UNLOCK();
}

MK_GC_API int MK_GC_CALL MK_GC_is_disabled(void)
{
    return MK_GC_dont_gc != 0;
}

/* Helper procedures for new kind creation.     */
MK_GC_API void ** MK_GC_CALL MK_GC_new_free_list_inner(void)
{
    void *result = MK_GC_INTERNAL_MALLOC((MAXOBJGRANULES+1)*sizeof(ptr_t),
                                      PTRFREE);
    if (result == 0) ABORT("Failed to allocate freelist for new kind");
    BZERO(result, (MAXOBJGRANULES+1)*sizeof(ptr_t));
    return result;
}

MK_GC_API void ** MK_GC_CALL MK_GC_new_free_list(void)
{
    void *result;
    DCL_LOCK_STATE;
    LOCK();
    result = MK_GC_new_free_list_inner();
    UNLOCK();
    return result;
}

MK_GC_API unsigned MK_GC_CALL MK_GC_new_kind_inner(void **fl, MK_GC_word descr,
                                        int adjust, int clear)
{
    unsigned result = MK_GC_n_kinds++;

    if (MK_GC_n_kinds > MAXOBJKINDS) ABORT("Too many kinds");
    MK_GC_obj_kinds[result].ok_freelist = fl;
    MK_GC_obj_kinds[result].ok_reclaim_list = 0;
    MK_GC_obj_kinds[result].ok_descriptor = descr;
    MK_GC_obj_kinds[result].ok_relocate_descr = adjust;
    MK_GC_obj_kinds[result].ok_init = clear;
    return result;
}

MK_GC_API unsigned MK_GC_CALL MK_GC_new_kind(void **fl, MK_GC_word descr, int adjust,
                                    int clear)
{
    unsigned result;
    DCL_LOCK_STATE;
    LOCK();
    result = MK_GC_new_kind_inner(fl, descr, adjust, clear);
    UNLOCK();
    return result;
}

MK_GC_API unsigned MK_GC_CALL MK_GC_new_proc_inner(MK_GC_mark_proc proc)
{
    unsigned result = MK_GC_n_mark_procs++;

    if (MK_GC_n_mark_procs > MAX_MARK_PROCS) ABORT("Too many mark procedures");
    MK_GC_mark_procs[result] = proc;
    return result;
}

MK_GC_API unsigned MK_GC_CALL MK_GC_new_proc(MK_GC_mark_proc proc)
{
    unsigned result;
    DCL_LOCK_STATE;
    LOCK();
    result = MK_GC_new_proc_inner(proc);
    UNLOCK();
    return result;
}

MK_GC_API void * MK_GC_CALL MK_GC_call_with_stack_base(MK_GC_stack_base_func fn, void *arg)
{
    struct MK_GC_stack_base base;
    void *result;

    base.mem_base = (void *)&base;
#   ifdef IA64
      base.reg_base = (void *)MK_GC_save_regs_in_stack();
      /* Unnecessarily flushes register stack,          */
      /* but that probably doesn't hurt.                */
#   endif
    result = fn(&base, arg);
    /* Strongly discourage the compiler from treating the above */
    /* as a tail call.                                          */
    MK_GC_noop1((word)(&base));
    return result;
}

#ifndef THREADS

MK_GC_INNER ptr_t MK_GC_blocked_sp = NULL;
        /* NULL value means we are not inside MK_GC_do_blocking() call. */
# ifdef IA64
    STATIC ptr_t MK_GC_blocked_register_sp = NULL;
# endif

MK_GC_INNER struct MK_GC_traced_stack_sect_s *MK_GC_traced_stack_sect = NULL;

/* This is nearly the same as in win32_threads.c        */
MK_GC_API void * MK_GC_CALL MK_GC_call_with_gc_active(MK_GC_fn_type fn,
                                             void * client_data)
{
    struct MK_GC_traced_stack_sect_s stacksect;
    MK_GC_ASSERT(MK_GC_is_initialized);

    /* Adjust our stack base value (this could happen if        */
    /* MK_GC_get_main_stack_base() is unimplemented or broken for  */
    /* the platform).                                           */
    if (MK_GC_stackbottom HOTTER_THAN (ptr_t)(&stacksect))
      MK_GC_stackbottom = (ptr_t)(&stacksect);

    if (MK_GC_blocked_sp == NULL) {
      /* We are not inside MK_GC_do_blocking() - do nothing more.  */
      return fn(client_data);
    }

    /* Setup new "stack section".       */
    stacksect.saved_stack_ptr = MK_GC_blocked_sp;
#   ifdef IA64
      /* This is the same as in MK_GC_call_with_stack_base().      */
      stacksect.backing_store_end = MK_GC_save_regs_in_stack();
      /* Unnecessarily flushes register stack,          */
      /* but that probably doesn't hurt.                */
      stacksect.saved_backing_store_ptr = MK_GC_blocked_register_sp;
#   endif
    stacksect.prev = MK_GC_traced_stack_sect;
    MK_GC_blocked_sp = NULL;
    MK_GC_traced_stack_sect = &stacksect;

    client_data = fn(client_data);
    MK_GC_ASSERT(MK_GC_blocked_sp == NULL);
    MK_GC_ASSERT(MK_GC_traced_stack_sect == &stacksect);

    /* Restore original "stack section".        */
    MK_GC_traced_stack_sect = stacksect.prev;
#   ifdef IA64
      MK_GC_blocked_register_sp = stacksect.saved_backing_store_ptr;
#   endif
    MK_GC_blocked_sp = stacksect.saved_stack_ptr;

    return client_data; /* result */
}

/* This is nearly the same as in win32_threads.c        */
/*ARGSUSED*/
STATIC void MK_GC_do_blocking_inner(ptr_t data, void * context)
{
    struct blocking_data * d = (struct blocking_data *) data;
    MK_GC_ASSERT(MK_GC_is_initialized);
    MK_GC_ASSERT(MK_GC_blocked_sp == NULL);
#   ifdef SPARC
        MK_GC_blocked_sp = MK_GC_save_regs_in_stack();
#   else
        MK_GC_blocked_sp = (ptr_t) &d; /* save approx. sp */
#   endif
#   ifdef IA64
        MK_GC_blocked_register_sp = MK_GC_save_regs_in_stack();
#   endif

    d -> client_data = (d -> fn)(d -> client_data);

#   ifdef SPARC
        MK_GC_ASSERT(MK_GC_blocked_sp != NULL);
#   else
        MK_GC_ASSERT(MK_GC_blocked_sp == (ptr_t) &d);
#   endif
    MK_GC_blocked_sp = NULL;
}

#endif /* !THREADS */

/* Wrapper for functions that are likely to block (or, at least, do not */
/* allocate garbage collected memory and/or manipulate pointers to the  */
/* garbage collected heap) for an appreciable length of time.           */
/* In the single threaded case, MK_GC_do_blocking() (together              */
/* with MK_GC_call_with_gc_active()) might be used to make stack scanning  */
/* more precise (i.e. scan only stack frames of functions that allocate */
/* garbage collected memory and/or manipulate pointers to the garbage   */
/* collected heap).                                                     */
MK_GC_API void * MK_GC_CALL MK_GC_do_blocking(MK_GC_fn_type fn, void * client_data)
{
    struct blocking_data my_data;

    my_data.fn = fn;
    my_data.client_data = client_data;
    MK_GC_with_callee_saves_pushed(MK_GC_do_blocking_inner, (ptr_t)(&my_data));
    return my_data.client_data; /* result */
}

#if !defined(NO_DEBUGGING)
  MK_GC_API void MK_GC_CALL MK_GC_dump(void)
  {
    MK_GC_printf("***Static roots:\n");
    MK_GC_print_static_roots();
    MK_GC_printf("\n***Heap sections:\n");
    MK_GC_print_heap_sects();
    MK_GC_printf("\n***Free blocks:\n");
    MK_GC_print_hblkfreelist();
    MK_GC_printf("\n***Blocks in use:\n");
    MK_GC_print_block_list();
  }
#endif /* !NO_DEBUGGING */

/* Getter functions for the public Read-only variables.                 */

/* MK_GC_get_gc_no() is unsynchronized and should be typically called      */
/* inside the context of MK_GC_call_with_alloc_lock() to prevent data      */
/* races (on multiprocessors).                                          */
MK_GC_API MK_GC_word MK_GC_CALL MK_GC_get_gc_no(void)
{
    return MK_GC_gc_no;
}

#ifdef THREADS
  MK_GC_API int MK_GC_CALL MK_GC_get_parallel(void)
  {
    /* MK_GC_parallel is initialized at start-up.  */
    return MK_GC_parallel;
  }
#endif

/* Setter and getter functions for the public R/W function variables.   */
/* These functions are synchronized (like MK_GC_set_warn_proc() and        */
/* MK_GC_get_warn_proc()).                                                 */

MK_GC_API void MK_GC_CALL MK_GC_set_oom_fn(MK_GC_oom_func fn)
{
    MK_GC_ASSERT(fn != 0);
    DCL_LOCK_STATE;
    LOCK();
    MK_GC_oom_fn = fn;
    UNLOCK();
}

MK_GC_API MK_GC_oom_func MK_GC_CALL MK_GC_get_oom_fn(void)
{
    MK_GC_oom_func fn;
    DCL_LOCK_STATE;
    LOCK();
    fn = MK_GC_oom_fn;
    UNLOCK();
    return fn;
}

MK_GC_API void MK_GC_CALL MK_GC_set_finalizer_notifier(MK_GC_finalizer_notifier_proc fn)
{
    /* fn may be 0 (means no finalizer notifier). */
    DCL_LOCK_STATE;
    LOCK();
    MK_GC_finalizer_notifier = fn;
    UNLOCK();
}

MK_GC_API MK_GC_finalizer_notifier_proc MK_GC_CALL MK_GC_get_finalizer_notifier(void)
{
    MK_GC_finalizer_notifier_proc fn;
    DCL_LOCK_STATE;
    LOCK();
    fn = MK_GC_finalizer_notifier;
    UNLOCK();
    return fn;
}

/* Setter and getter functions for the public numeric R/W variables.    */
/* It is safe to call these functions even before MK_GC_INIT().            */
/* These functions are unsynchronized and should be typically called    */
/* inside the context of MK_GC_call_with_alloc_lock() (if called after     */
/* MK_GC_INIT()) to prevent data races (unless it is guaranteed the        */
/* collector is not multi-threaded at that execution point).            */

MK_GC_API void MK_GC_CALL MK_GC_set_find_leak(int value)
{
    /* value is of boolean type. */
    MK_GC_find_leak = value;
}

MK_GC_API int MK_GC_CALL MK_GC_get_find_leak(void)
{
    return MK_GC_find_leak;
}

MK_GC_API void MK_GC_CALL MK_GC_set_all_interior_pointers(int value)
{
    DCL_LOCK_STATE;

    MK_GC_all_interior_pointers = value ? 1 : 0;
    if (MK_GC_is_initialized) {
      /* It is not recommended to change MK_GC_all_interior_pointers value */
      /* after GC is initialized but it seems GC could work correctly   */
      /* even after switching the mode.                                 */
      LOCK();
      MK_GC_initialize_offsets(); /* NOTE: this resets manual offsets as well */
      if (!MK_GC_all_interior_pointers)
        MK_GC_bl_init_no_interiors();
      UNLOCK();
    }
}

MK_GC_API int MK_GC_CALL MK_GC_get_all_interior_pointers(void)
{
    return MK_GC_all_interior_pointers;
}

MK_GC_API void MK_GC_CALL MK_GC_set_finalize_on_demand(int value)
{
    MK_GC_ASSERT(value != -1);
    /* value is of boolean type. */
    MK_GC_finalize_on_demand = value;
}

MK_GC_API int MK_GC_CALL MK_GC_get_finalize_on_demand(void)
{
    return MK_GC_finalize_on_demand;
}

MK_GC_API void MK_GC_CALL MK_GC_set_java_finalization(int value)
{
    MK_GC_ASSERT(value != -1);
    /* value is of boolean type. */
    MK_GC_java_finalization = value;
}

MK_GC_API int MK_GC_CALL MK_GC_get_java_finalization(void)
{
    return MK_GC_java_finalization;
}

MK_GC_API void MK_GC_CALL MK_GC_set_dont_expand(int value)
{
    MK_GC_ASSERT(value != -1);
    /* value is of boolean type. */
    MK_GC_dont_expand = value;
}

MK_GC_API int MK_GC_CALL MK_GC_get_dont_expand(void)
{
    return MK_GC_dont_expand;
}

MK_GC_API void MK_GC_CALL MK_GC_set_no_dls(int value)
{
    MK_GC_ASSERT(value != -1);
    /* value is of boolean type. */
    MK_GC_no_dls = value;
}

MK_GC_API int MK_GC_CALL MK_GC_get_no_dls(void)
{
    return MK_GC_no_dls;
}

MK_GC_API void MK_GC_CALL MK_GC_set_non_gc_bytes(MK_GC_word value)
{
    MK_GC_non_gc_bytes = value;
}

MK_GC_API MK_GC_word MK_GC_CALL MK_GC_get_non_gc_bytes(void)
{
    return MK_GC_non_gc_bytes;
}

MK_GC_API void MK_GC_CALL MK_GC_set_free_space_divisor(MK_GC_word value)
{
    MK_GC_ASSERT(value > 0);
    MK_GC_free_space_divisor = value;
}

MK_GC_API MK_GC_word MK_GC_CALL MK_GC_get_free_space_divisor(void)
{
    return MK_GC_free_space_divisor;
}

MK_GC_API void MK_GC_CALL MK_GC_set_max_retries(MK_GC_word value)
{
    MK_GC_ASSERT(value != ~(MK_GC_word)0);
    MK_GC_max_retries = value;
}

MK_GC_API MK_GC_word MK_GC_CALL MK_GC_get_max_retries(void)
{
    return MK_GC_max_retries;
}

MK_GC_API void MK_GC_CALL MK_GC_set_dont_precollect(int value)
{
    MK_GC_ASSERT(value != -1);
    /* value is of boolean type. */
    MK_GC_dont_precollect = value;
}

MK_GC_API int MK_GC_CALL MK_GC_get_dont_precollect(void)
{
    return MK_GC_dont_precollect;
}

MK_GC_API void MK_GC_CALL MK_GC_set_full_freq(int value)
{
    MK_GC_ASSERT(value >= 0);
    MK_GC_full_freq = value;
}

MK_GC_API int MK_GC_CALL MK_GC_get_full_freq(void)
{
    return MK_GC_full_freq;
}

MK_GC_API void MK_GC_CALL MK_GC_set_time_limit(unsigned long value)
{
    MK_GC_ASSERT(value != (unsigned long)-1L);
    MK_GC_time_limit = value;
}

MK_GC_API unsigned long MK_GC_CALL MK_GC_get_time_limit(void)
{
    return MK_GC_time_limit;
}

MK_GC_API void MK_GC_CALL MK_GC_set_force_unmap_on_gcollect(int value)
{
    MK_GC_force_unmap_on_gcollect = (MK_GC_bool)value;
}

MK_GC_API int MK_GC_CALL MK_GC_get_force_unmap_on_gcollect(void)
{
    return (int)MK_GC_force_unmap_on_gcollect;
}
