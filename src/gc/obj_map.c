/*
 * Copyright 1988, 1989 Hans-J. Boehm, Alan J. Demers
 * Copyright (c) 1991, 1992 by Xerox Corporation.  All rights reserved.
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

#include "private/gc_priv.h"

/* Routines for maintaining maps describing heap block
 * layouts for various object sizes.  Allows fast pointer validity checks
 * and fast location of object start locations on machines (such as SPARC)
 * with slow division.
 */

/* Consider pointers that are offset bytes displaced from the beginning */
/* of an object to be valid.                                            */

MK_GC_API void MK_GC_CALL MK_GC_register_displacement(size_t offset)
{
    DCL_LOCK_STATE;

    LOCK();
    MK_GC_register_displacement_inner(offset);
    UNLOCK();
}

MK_GC_INNER void MK_GC_register_displacement_inner(size_t offset)
{
    if (offset >= VALID_OFFSET_SZ) {
        ABORT("Bad argument to MK_GC_register_displacement");
    }
    if (!MK_GC_valid_offsets[offset]) {
      MK_GC_valid_offsets[offset] = TRUE;
      MK_GC_modws_valid_offsets[offset % sizeof(word)] = TRUE;
    }
}

#ifdef MARK_BIT_PER_GRANULE
  /* Add a heap block map for objects of size granules to obj_map.      */
  /* Return FALSE on failure.                                           */
  /* A size of 0 granules is used for large objects.                    */
  MK_GC_INNER MK_GC_bool MK_GC_add_map_entry(size_t granules)
  {
    unsigned displ;
    short * new_map;

    if (granules > BYTES_TO_GRANULES(MAXOBJBYTES)) granules = 0;
    if (MK_GC_obj_map[granules] != 0) {
        return(TRUE);
    }
    new_map = (short *)MK_GC_scratch_alloc(MAP_LEN * sizeof(short));
    if (new_map == 0) return(FALSE);
    MK_GC_COND_LOG_PRINTF(
                "Adding block map for size of %u granules (%u bytes)\n",
                (unsigned)granules, (unsigned)GRANULES_TO_BYTES(granules));
    if (granules == 0) {
      for (displ = 0; displ < BYTES_TO_GRANULES(HBLKSIZE); displ++) {
        new_map[displ] = 1;  /* Nonzero to get us out of marker fast path. */
      }
    } else {
      for (displ = 0; displ < BYTES_TO_GRANULES(HBLKSIZE); displ++) {
        new_map[displ] = (short)(displ % granules);
      }
    }
    MK_GC_obj_map[granules] = new_map;
    return(TRUE);
  }
#endif /* MARK_BIT_PER_GRANULE */

MK_GC_INNER void MK_GC_initialize_offsets(void)
{
  unsigned i;
  if (MK_GC_all_interior_pointers) {
    for (i = 0; i < VALID_OFFSET_SZ; ++i)
      MK_GC_valid_offsets[i] = TRUE;
  } else {
    BZERO(MK_GC_valid_offsets, sizeof(MK_GC_valid_offsets));
    for (i = 0; i < sizeof(word); ++i)
      MK_GC_modws_valid_offsets[i] = FALSE;
  }
}
