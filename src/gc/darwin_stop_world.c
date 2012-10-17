/*
 * Copyright (c) 1994 by Xerox Corporation.  All rights reserved.
 * Copyright (c) 1996 by Silicon Graphics.  All rights reserved.
 * Copyright (c) 1998 by Fergus Henderson.  All rights reserved.
 * Copyright (c) 2000-2010 by Hewlett-Packard Development Company.
 * All rights reserved.
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

#include "private/pthread_support.h"

/* This probably needs more porting work to ppc64. */

#if defined(MK_GC_DARWIN_THREADS)

/* From "Inside Mac OS X - Mach-O Runtime Architecture" published by Apple
   Page 49:
   "The space beneath the stack pointer, where a new stack frame would normally
   be allocated, is called the red zone. This area as shown in Figure 3-2 may
   be used for any purpose as long as a new stack frame does not need to be
   added to the stack."

   Page 50: "If a leaf procedure's red zone usage would exceed 224 bytes, then
   it must set up a stack frame just like routines that call other routines."
*/
#ifdef POWERPC
# if CPP_WORDSZ == 32
#   define PPC_RED_ZONE_SIZE 224
# elif CPP_WORDSZ == 64
#   define PPC_RED_ZONE_SIZE 320
# endif
#endif

#ifndef DARWIN_DONT_PARSE_STACK

typedef struct StackFrame {
  unsigned long savedSP;
  unsigned long savedCR;
  unsigned long savedLR;
  unsigned long reserved[2];
  unsigned long savedRTOC;
} StackFrame;

MK_GC_INNER ptr_t MK_GC_FindTopOfStack(unsigned long stack_start)
{
  StackFrame *frame;

  if (stack_start == 0) {
# ifdef POWERPC
#   if CPP_WORDSZ == 32
      __asm__ __volatile__ ("lwz %0,0(r1)" : "=r" (frame));
#   else
      __asm__ __volatile__ ("ld %0,0(r1)" : "=r" (frame));
#   endif
# endif
  } else {
    frame = (StackFrame *)stack_start;
  }

# ifdef DEBUG_THREADS
    /* MK_GC_printf("FindTopOfStack start at sp = %p\n", frame); */
# endif
  while (frame->savedSP != 0) {
    /* if there are no more stack frames, stop */

    frame = (StackFrame*)frame->savedSP;

    /* we do these next two checks after going to the next frame
       because the LR for the first stack frame in the loop
       is not set up on purpose, so we shouldn't check it. */
    if ((frame->savedLR & ~0x3) == 0 || (frame->savedLR & ~0x3) == ~0x3)
      break; /* if the next LR is bogus, stop */
  }
# ifdef DEBUG_THREADS
    /* MK_GC_printf("FindTopOfStack finish at sp = %p\n", frame); */
# endif
  return (ptr_t)frame;
}

#endif /* !DARWIN_DONT_PARSE_STACK */

/* MK_GC_query_task_threads controls whether to obtain the list of */
/* the threads from the kernel or to use MK_GC_threads table.      */
#ifdef MK_GC_NO_THREADS_DISCOVERY
# define MK_GC_query_task_threads FALSE
#elif defined(MK_GC_DISCOVER_TASK_THREADS)
# define MK_GC_query_task_threads TRUE
#else
  STATIC MK_GC_bool MK_GC_query_task_threads = FALSE;
#endif /* !MK_GC_NO_THREADS_DISCOVERY */

/* Use implicit threads registration (all task threads excluding the GC */
/* special ones are stoped and scanned).  Should be called before       */
/* MK_GC_INIT() (or, at least, before going multi-threaded).  Deprecated.  */
MK_GC_API void MK_GC_CALL MK_GC_use_threads_discovery(void)
{
# if defined(MK_GC_NO_THREADS_DISCOVERY) || defined(DARWIN_DONT_PARSE_STACK)
    ABORT("Darwin task-threads-based stop and push unsupported");
# else
    MK_GC_ASSERT(!MK_GC_need_to_lock);
#   ifndef MK_GC_DISCOVER_TASK_THREADS
      MK_GC_query_task_threads = TRUE;
#   endif
    MK_GC_init_parallel(); /* just to be consistent with Win32 one */
# endif
}

/* Evaluates the stack range for a given thread.  Returns the lower     */
/* bound and sets *phi to the upper one.                                */
STATIC ptr_t MK_GC_stack_range_for(ptr_t *phi, thread_act_t thread, MK_GC_thread p,
                                MK_GC_bool thread_blocked, mach_port_t my_thread)
{
  ptr_t lo;
  if (thread == my_thread) {
    MK_GC_ASSERT(!thread_blocked);
    lo = MK_GC_approx_sp();
#   ifndef DARWIN_DONT_PARSE_STACK
      *phi = MK_GC_FindTopOfStack(0);
#   endif

  } else if (thread_blocked) {
    lo = p->stop_info.stack_ptr;
#   ifndef DARWIN_DONT_PARSE_STACK
      *phi = p->topOfStack;
#   endif

  } else {
    /* MACHINE_THREAD_STATE_COUNT does not seem to be defined       */
    /* everywhere.  Hence we use our own version.  Alternatively,   */
    /* we could use THREAD_STATE_MAX (but seems to be not optimal). */
    kern_return_t kern_result;
    mach_msg_type_number_t thread_state_count = MK_GC_MACH_THREAD_STATE_COUNT;
    MK_GC_THREAD_STATE_T state;

    /* Get the thread state (registers, etc) */
    kern_result = thread_get_state(thread, MK_GC_MACH_THREAD_STATE,
                                   (natural_t *)&state,
                                   &thread_state_count);
#   ifdef DEBUG_THREADS
      MK_GC_log_printf("thread_get_state returns value = %d\n", kern_result);
#   endif
    if (kern_result != KERN_SUCCESS)
      ABORT("thread_get_state failed");

#   if defined(I386)
      lo = (void *)state.THREAD_FLD(esp);
#     ifndef DARWIN_DONT_PARSE_STACK
        *phi = MK_GC_FindTopOfStack(state.THREAD_FLD(esp));
#     endif
      MK_GC_push_one(state.THREAD_FLD(eax));
      MK_GC_push_one(state.THREAD_FLD(ebx));
      MK_GC_push_one(state.THREAD_FLD(ecx));
      MK_GC_push_one(state.THREAD_FLD(edx));
      MK_GC_push_one(state.THREAD_FLD(edi));
      MK_GC_push_one(state.THREAD_FLD(esi));
      MK_GC_push_one(state.THREAD_FLD(ebp));

#   elif defined(X86_64)
      lo = (void *)state.THREAD_FLD(rsp);
#     ifndef DARWIN_DONT_PARSE_STACK
        *phi = MK_GC_FindTopOfStack(state.THREAD_FLD(rsp));
#     endif
      MK_GC_push_one(state.THREAD_FLD(rax));
      MK_GC_push_one(state.THREAD_FLD(rbx));
      MK_GC_push_one(state.THREAD_FLD(rcx));
      MK_GC_push_one(state.THREAD_FLD(rdx));
      MK_GC_push_one(state.THREAD_FLD(rdi));
      MK_GC_push_one(state.THREAD_FLD(rsi));
      MK_GC_push_one(state.THREAD_FLD(rbp));
      /* MK_GC_push_one(state.THREAD_FLD(rsp)); */
      MK_GC_push_one(state.THREAD_FLD(r8));
      MK_GC_push_one(state.THREAD_FLD(r9));
      MK_GC_push_one(state.THREAD_FLD(r10));
      MK_GC_push_one(state.THREAD_FLD(r11));
      MK_GC_push_one(state.THREAD_FLD(r12));
      MK_GC_push_one(state.THREAD_FLD(r13));
      MK_GC_push_one(state.THREAD_FLD(r14));
      MK_GC_push_one(state.THREAD_FLD(r15));

#   elif defined(POWERPC)
      lo = (void *)(state.THREAD_FLD(r1) - PPC_RED_ZONE_SIZE);
#     ifndef DARWIN_DONT_PARSE_STACK
        *phi = MK_GC_FindTopOfStack(state.THREAD_FLD(r1));
#     endif
      MK_GC_push_one(state.THREAD_FLD(r0));
      MK_GC_push_one(state.THREAD_FLD(r2));
      MK_GC_push_one(state.THREAD_FLD(r3));
      MK_GC_push_one(state.THREAD_FLD(r4));
      MK_GC_push_one(state.THREAD_FLD(r5));
      MK_GC_push_one(state.THREAD_FLD(r6));
      MK_GC_push_one(state.THREAD_FLD(r7));
      MK_GC_push_one(state.THREAD_FLD(r8));
      MK_GC_push_one(state.THREAD_FLD(r9));
      MK_GC_push_one(state.THREAD_FLD(r10));
      MK_GC_push_one(state.THREAD_FLD(r11));
      MK_GC_push_one(state.THREAD_FLD(r12));
      MK_GC_push_one(state.THREAD_FLD(r13));
      MK_GC_push_one(state.THREAD_FLD(r14));
      MK_GC_push_one(state.THREAD_FLD(r15));
      MK_GC_push_one(state.THREAD_FLD(r16));
      MK_GC_push_one(state.THREAD_FLD(r17));
      MK_GC_push_one(state.THREAD_FLD(r18));
      MK_GC_push_one(state.THREAD_FLD(r19));
      MK_GC_push_one(state.THREAD_FLD(r20));
      MK_GC_push_one(state.THREAD_FLD(r21));
      MK_GC_push_one(state.THREAD_FLD(r22));
      MK_GC_push_one(state.THREAD_FLD(r23));
      MK_GC_push_one(state.THREAD_FLD(r24));
      MK_GC_push_one(state.THREAD_FLD(r25));
      MK_GC_push_one(state.THREAD_FLD(r26));
      MK_GC_push_one(state.THREAD_FLD(r27));
      MK_GC_push_one(state.THREAD_FLD(r28));
      MK_GC_push_one(state.THREAD_FLD(r29));
      MK_GC_push_one(state.THREAD_FLD(r30));
      MK_GC_push_one(state.THREAD_FLD(r31));

#   elif defined(ARM32)
      lo = (void *)state.__sp;
#     ifndef DARWIN_DONT_PARSE_STACK
        *phi = MK_GC_FindTopOfStack(state.__sp);
#     endif
      MK_GC_push_one(state.__r[0]);
      MK_GC_push_one(state.__r[1]);
      MK_GC_push_one(state.__r[2]);
      MK_GC_push_one(state.__r[3]);
      MK_GC_push_one(state.__r[4]);
      MK_GC_push_one(state.__r[5]);
      MK_GC_push_one(state.__r[6]);
      MK_GC_push_one(state.__r[7]);
      MK_GC_push_one(state.__r[8]);
      MK_GC_push_one(state.__r[9]);
      MK_GC_push_one(state.__r[10]);
      MK_GC_push_one(state.__r[11]);
      MK_GC_push_one(state.__r[12]);
      /* MK_GC_push_one(state.__sp); */
      MK_GC_push_one(state.__lr);
      /* MK_GC_push_one(state.__pc); */
      MK_GC_push_one(state.__cpsr);

#   else
#     error FIXME for non-x86 || ppc || arm architectures
#   endif
  } /* thread != my_thread */

# ifdef DARWIN_DONT_PARSE_STACK
    /* p is guaranteed to be non-NULL regardless of MK_GC_query_task_threads. */
    *phi = (p->flags & MAIN_THREAD) != 0 ? MK_GC_stackbottom : p->stack_end;
# endif
# ifdef DEBUG_THREADS
    MK_GC_log_printf("Darwin: Stack for thread 0x%lx = [%p,%p)\n",
                  (unsigned long)thread, lo, *phi);
# endif
  return lo;
}

MK_GC_INNER void MK_GC_push_all_stacks(void)
{
  int i;
  ptr_t lo, hi;
  task_t my_task = current_task();
  mach_port_t my_thread = mach_thread_self();
  MK_GC_bool found_me = FALSE;
  int nthreads = 0;
  word total_size = 0;
  mach_msg_type_number_t listcount = (mach_msg_type_number_t)THREAD_TABLE_SZ;
  if (!MK_GC_thr_initialized)
    MK_GC_thr_init();

# ifndef DARWIN_DONT_PARSE_STACK
    if (MK_GC_query_task_threads) {
      kern_return_t kern_result;
      thread_act_array_t act_list = 0;

      /* Obtain the list of the threads from the kernel.  */
      kern_result = task_threads(my_task, &act_list, &listcount);
      if (kern_result != KERN_SUCCESS)
        ABORT("task_threads failed");

      for (i = 0; i < (int)listcount; i++) {
        thread_act_t thread = act_list[i];
        lo = MK_GC_stack_range_for(&hi, thread, NULL, FALSE, my_thread);
        MK_GC_ASSERT(lo <= hi);
        total_size += hi - lo;
        MK_GC_push_all_stack(lo, hi);
        nthreads++;
        if (thread == my_thread)
          found_me = TRUE;
        mach_port_deallocate(my_task, thread);
      } /* for (i=0; ...) */

      vm_deallocate(my_task, (vm_address_t)act_list,
                    sizeof(thread_t) * listcount);
    } else
# endif /* !DARWIN_DONT_PARSE_STACK */
  /* else */ {
    for (i = 0; i < (int)listcount; i++) {
      MK_GC_thread p;
      for (p = MK_GC_threads[i]; p != NULL; p = p->next)
        if ((p->flags & FINISHED) == 0) {
          thread_act_t thread = (thread_act_t)p->stop_info.mach_thread;
          lo = MK_GC_stack_range_for(&hi, thread, p, (MK_GC_bool)p->thread_blocked,
                                  my_thread);
          MK_GC_ASSERT(lo <= hi);
          total_size += hi - lo;
          MK_GC_push_all_stack_sections(lo, hi, p->traced_stack_sect);
          nthreads++;
          if (thread == my_thread)
            found_me = TRUE;
        }
    } /* for (i=0; ...) */
  }

  mach_port_deallocate(my_task, my_thread);
  if (MK_GC_print_stats == VERBOSE)
    MK_GC_log_printf("Pushed %d thread stacks\n", nthreads);
  if (!found_me && !MK_GC_in_thread_creation)
    ABORT("Collecting from unknown thread");
  MK_GC_total_stacksize = total_size;
}

#ifndef MK_GC_NO_THREADS_DISCOVERY

# ifdef MPROTECT_VDB
    STATIC mach_port_t MK_GC_mach_handler_thread = 0;
    STATIC MK_GC_bool MK_GC_use_mach_handler_thread = FALSE;

    MK_GC_INNER void MK_GC_darwin_register_mach_handler_thread(mach_port_t thread)
    {
      MK_GC_mach_handler_thread = thread;
      MK_GC_use_mach_handler_thread = TRUE;
    }
# endif /* MPROTECT_VDB */

# ifndef MK_GC_MAX_MACH_THREADS
#   define MK_GC_MAX_MACH_THREADS THREAD_TABLE_SZ
# endif

  struct MK_GC_mach_thread {
    thread_act_t thread;
    MK_GC_bool already_suspended;
  };

  struct MK_GC_mach_thread MK_GC_mach_threads[MK_GC_MAX_MACH_THREADS];
  STATIC int MK_GC_mach_threads_count = 0;
  /* FIXME: it is better to implement MK_GC_mach_threads as a hash set.  */

/* returns true if there's a thread in act_list that wasn't in old_list */
STATIC MK_GC_bool MK_GC_suspend_thread_list(thread_act_array_t act_list, int count,
                                      thread_act_array_t old_list,
                                      int old_count, mach_port_t my_thread)
{
  int i;
  int j = -1;
  MK_GC_bool changed = FALSE;

  for (i = 0; i < count; i++) {
    thread_act_t thread = act_list[i];
    MK_GC_bool found;
    struct thread_basic_info info;
    mach_msg_type_number_t outCount;
    kern_return_t kern_result;

    if (thread == my_thread
#       ifdef MPROTECT_VDB
          || (MK_GC_mach_handler_thread == thread && MK_GC_use_mach_handler_thread)
#       endif
        ) {
      /* Don't add our and the handler threads. */
      continue;
    }
#   ifdef PARALLEL_MARK
      if (MK_GC_is_mach_marker(thread))
        continue; /* ignore the parallel marker threads */
#   endif

#   ifdef DEBUG_THREADS
      MK_GC_log_printf("Attempting to suspend thread 0x%lx\n",
                    (unsigned long)thread);
#   endif
    /* find the current thread in the old list */
    found = FALSE;
    {
      int last_found = j; /* remember the previous found thread index */

      /* Search for the thread starting from the last found one first.  */
      while (++j < old_count)
        if (old_list[j] == thread) {
          found = TRUE;
          break;
        }
      if (!found) {
        /* If not found, search in the rest (beginning) of the list.    */
        for (j = 0; j < last_found; j++)
          if (old_list[j] == thread) {
            found = TRUE;
            break;
          }

        if (!found) {
          /* add it to the MK_GC_mach_threads list */
          if (MK_GC_mach_threads_count == MK_GC_MAX_MACH_THREADS)
            ABORT("Too many threads");
          MK_GC_mach_threads[MK_GC_mach_threads_count].thread = thread;
          /* default is not suspended */
          MK_GC_mach_threads[MK_GC_mach_threads_count].already_suspended = FALSE;
          changed = TRUE;
        }
      }
    }

    outCount = THREAD_INFO_MAX;
    kern_result = thread_info(thread, THREAD_BASIC_INFO,
                              (thread_info_t)&info, &outCount);
    if (kern_result != KERN_SUCCESS) {
      /* The thread may have quit since the thread_threads() call we  */
      /* mark already suspended so it's not dealt with anymore later. */
      if (!found)
        MK_GC_mach_threads[MK_GC_mach_threads_count++].already_suspended = TRUE;
      continue;
    }
#   ifdef DEBUG_THREADS
      MK_GC_log_printf("Thread state for 0x%lx = %d\n", (unsigned long)thread,
                    info.run_state);
#   endif
    if (info.suspend_count != 0) {
      /* thread is already suspended. */
      if (!found)
        MK_GC_mach_threads[MK_GC_mach_threads_count++].already_suspended = TRUE;
      continue;
    }

#   ifdef DEBUG_THREADS
      MK_GC_log_printf("Suspending 0x%lx\n", (unsigned long)thread);
#   endif
    kern_result = thread_suspend(thread);
    if (kern_result != KERN_SUCCESS) {
      /* The thread may have quit since the thread_threads() call we  */
      /* mark already suspended so it's not dealt with anymore later. */
      if (!found)
        MK_GC_mach_threads[MK_GC_mach_threads_count++].already_suspended = TRUE;
      continue;
    }
    if (!found)
      MK_GC_mach_threads_count++;
  }
  return changed;
}

#endif /* !MK_GC_NO_THREADS_DISCOVERY */

/* Caller holds allocation lock.        */
MK_GC_INNER void MK_GC_stop_world(void)
{
  unsigned i;
  task_t my_task = current_task();
  mach_port_t my_thread = mach_thread_self();
  kern_return_t kern_result;

# ifdef DEBUG_THREADS
    MK_GC_log_printf("Stopping the world from thread 0x%lx\n",
                  (unsigned long)my_thread);
# endif
# ifdef PARALLEL_MARK
    if (MK_GC_parallel) {
      /* Make sure all free list construction has stopped before we     */
      /* start.  No new construction can start, since free list         */
      /* construction is required to acquire and release the GC lock    */
      /* before it starts, and we have the lock.                        */
      MK_GC_acquire_mark_lock();
      MK_GC_ASSERT(MK_GC_fl_builder_count == 0);
      /* We should have previously waited for it to become zero. */
    }
# endif /* PARALLEL_MARK */

  if (MK_GC_query_task_threads) {
#   ifndef MK_GC_NO_THREADS_DISCOVERY
      MK_GC_bool changed;
      thread_act_array_t act_list, prev_list;
      mach_msg_type_number_t listcount, prevcount;

      /* Clear out the mach threads list table.  We do not need to      */
      /* really clear MK_GC_mach_threads[] as it is used only in the range */
      /* from 0 to MK_GC_mach_threads_count-1, inclusive.                  */
      MK_GC_mach_threads_count = 0;

      /* Loop stopping threads until you have gone over the whole list  */
      /* twice without a new one appearing.  thread_create() won't      */
      /* return (and thus the thread stop) until the new thread exists, */
      /* so there is no window whereby you could stop a thread,         */
      /* recognize it is stopped, but then have a new thread it created */
      /* before stopping show up later.                                 */
      changed = TRUE;
      prev_list = NULL;
      prevcount = 0;
      do {
        kern_result = task_threads(my_task, &act_list, &listcount);

        if (kern_result == KERN_SUCCESS) {
          changed = MK_GC_suspend_thread_list(act_list, listcount, prev_list,
                                           prevcount, my_thread);

          if (prev_list != NULL) {
            for (i = 0; i < prevcount; i++)
              mach_port_deallocate(my_task, prev_list[i]);

            vm_deallocate(my_task, (vm_address_t)prev_list,
                          sizeof(thread_t) * prevcount);
          }

          /* Repeat while having changes. */
          prev_list = act_list;
          prevcount = listcount;
        }
      } while (changed);

      MK_GC_ASSERT(prev_list != 0);
      for (i = 0; i < prevcount; i++)
        mach_port_deallocate(my_task, prev_list[i]);
      vm_deallocate(my_task, (vm_address_t)act_list,
                    sizeof(thread_t) * listcount);
#   endif /* !MK_GC_NO_THREADS_DISCOVERY */

  } else {
    for (i = 0; i < THREAD_TABLE_SZ; i++) {
      MK_GC_thread p;

      for (p = MK_GC_threads[i]; p != NULL; p = p->next) {
        if ((p->flags & FINISHED) == 0 && !p->thread_blocked &&
             p->stop_info.mach_thread != my_thread) {

          kern_result = thread_suspend(p->stop_info.mach_thread);
          if (kern_result != KERN_SUCCESS)
            ABORT("thread_suspend failed");
        }
      }
    }
  }

# ifdef MPROTECT_VDB
    if(MK_GC_incremental) {
      MK_GC_mprotect_stop();
    }
# endif
# ifdef PARALLEL_MARK
    if (MK_GC_parallel)
      MK_GC_release_mark_lock();
# endif

# ifdef DEBUG_THREADS
    MK_GC_log_printf("World stopped from 0x%lx\n", (unsigned long)my_thread);
# endif
  mach_port_deallocate(my_task, my_thread);
}

MK_GC_INLINE void MK_GC_thread_resume(thread_act_t thread)
{
  kern_return_t kern_result;
# if defined(DEBUG_THREADS) || defined(MK_GC_ASSERTIONS)
    struct thread_basic_info info;
    mach_msg_type_number_t outCount = THREAD_INFO_MAX;
    kern_result = thread_info(thread, THREAD_BASIC_INFO,
                              (thread_info_t)&info, &outCount);
    if (kern_result != KERN_SUCCESS)
      ABORT("thread_info failed");
# endif
# ifdef DEBUG_THREADS
    MK_GC_log_printf("Resuming thread 0x%lx with state %d\n",
                  (unsigned long)thread, info.run_state);
# endif
  /* Resume the thread */
  kern_result = thread_resume(thread);
  if (kern_result != KERN_SUCCESS)
    ABORT("thread_resume failed");
}

/* Caller holds allocation lock, and has held it continuously since     */
/* the world stopped.                                                   */
MK_GC_INNER void MK_GC_start_world(void)
{
  task_t my_task = current_task();
  int i;
# ifdef DEBUG_THREADS
    MK_GC_log_printf("World starting\n");
# endif
# ifdef MPROTECT_VDB
    if(MK_GC_incremental) {
      MK_GC_mprotect_resume();
    }
# endif

  if (MK_GC_query_task_threads) {
#   ifndef MK_GC_NO_THREADS_DISCOVERY
      int j = MK_GC_mach_threads_count;
      kern_return_t kern_result;
      thread_act_array_t act_list;
      mach_msg_type_number_t listcount;

      kern_result = task_threads(my_task, &act_list, &listcount);
      if (kern_result != KERN_SUCCESS)
        ABORT("task_threads failed");

      for (i = 0; i < (int)listcount; i++) {
        thread_act_t thread = act_list[i];
        int last_found = j;        /* The thread index found during the   */
                                   /* previous iteration (count value     */
                                   /* means no thread found yet).         */

        /* Search for the thread starting from the last found one first.  */
        while (++j < MK_GC_mach_threads_count) {
          if (MK_GC_mach_threads[j].thread == thread)
            break;
        }
        if (j >= MK_GC_mach_threads_count) {
          /* If not found, search in the rest (beginning) of the list.    */
          for (j = 0; j < last_found; j++) {
            if (MK_GC_mach_threads[j].thread == thread)
              break;
          }
        }

        if (j != last_found) {
          /* The thread is found in MK_GC_mach_threads.      */
          if (MK_GC_mach_threads[j].already_suspended) {
#           ifdef DEBUG_THREADS
              MK_GC_log_printf("Not resuming already suspended thread 0x%lx\n",
                            (unsigned long)thread);
#           endif
          } else {
            MK_GC_thread_resume(thread);
          }
        }

        mach_port_deallocate(my_task, thread);
      }
      vm_deallocate(my_task, (vm_address_t)act_list,
                    sizeof(thread_t) * listcount);
#   endif /* !MK_GC_NO_THREADS_DISCOVERY */

  } else {
    mach_port_t my_thread = mach_thread_self();

    for (i = 0; i < THREAD_TABLE_SZ; i++) {
      MK_GC_thread p;
      for (p = MK_GC_threads[i]; p != NULL; p = p->next) {
        if ((p->flags & FINISHED) == 0 && !p->thread_blocked &&
             p->stop_info.mach_thread != my_thread)
          MK_GC_thread_resume(p->stop_info.mach_thread);
      }
    }

    mach_port_deallocate(my_task, my_thread);
  }

# ifdef DEBUG_THREADS
    MK_GC_log_printf("World started\n");
# endif
}

#endif /* MK_GC_DARWIN_THREADS */
