/*
 * Copyright (c) 1994 by Xerox Corporation.  All rights reserved.
 * Copyright (c) 1996 by Silicon Graphics.  All rights reserved.
 * Copyright (c) 1998 by Fergus Henderson.  All rights reserved.
 * Copyright (c) 2000-2009 by Hewlett-Packard Development Company.
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

#ifndef MK_GC_DARWIN_STOP_WORLD_H
#define MK_GC_DARWIN_STOP_WORLD_H

#if !defined(MK_GC_DARWIN_THREADS)
# error darwin_stop_world.h included without MK_GC_DARWIN_THREADS defined
#endif

#include <mach/mach.h>
#include <mach/thread_act.h>

struct thread_stop_info {
  mach_port_t mach_thread;
  ptr_t stack_ptr; /* Valid only when thread is in a "blocked" state.   */
};

#ifndef DARWIN_DONT_PARSE_STACK
  MK_GC_INNER ptr_t MK_GC_FindTopOfStack(unsigned long);
#endif

#ifdef MPROTECT_VDB
  MK_GC_INNER void MK_GC_mprotect_stop(void);
  MK_GC_INNER void MK_GC_mprotect_resume(void);
#endif

#if defined(PARALLEL_MARK) && !defined(MK_GC_NO_THREADS_DISCOVERY)
  MK_GC_INNER MK_GC_bool MK_GC_is_mach_marker(thread_act_t);
#endif

#endif
