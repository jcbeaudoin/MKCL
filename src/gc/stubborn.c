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

#if defined(MANUAL_VDB)

  /* Stubborn object (hard to change, nearly immutable) allocation.     */
  /* This interface is deprecated.  We mostly emulate it using          */
  /* MANUAL_VDB.  But that imposes the additional constraint that       */
  /* written, but not yet MK_GC_dirty()ed objects must be referenced       */
  /* by a stack.                                                        */

  void MK_GC_dirty(ptr_t p);

  MK_GC_API void * MK_GC_CALL MK_GC_malloc_stubborn(size_t lb)
  {
    return(MK_GC_malloc(lb));
  }

  MK_GC_API void MK_GC_CALL MK_GC_end_stubborn_change(const void *p)
  {
    MK_GC_dirty((ptr_t)p);
  }

  MK_GC_API void MK_GC_CALL MK_GC_change_stubborn(const void *p MK_GC_ATTR_UNUSED)
  {
  }

#else /* !MANUAL_VDB */

  MK_GC_API void * MK_GC_CALL MK_GC_malloc_stubborn(size_t lb)
  {
    return(MK_GC_malloc(lb));
  }

  MK_GC_API void MK_GC_CALL MK_GC_end_stubborn_change(const void *p MK_GC_ATTR_UNUSED)
  {
  }

  MK_GC_API void MK_GC_CALL MK_GC_change_stubborn(const void *p MK_GC_ATTR_UNUSED)
  {
  }

#endif /* !MANUAL_VDB */
