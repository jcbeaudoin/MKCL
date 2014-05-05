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

/*
 * We maintain several hash tables of hblks that have had false hits.
 * Each contains one bit per hash bucket;  If any page in the bucket
 * has had a false hit, we assume that all of them have.
 * See the definition of page_hash_table in gc_private.h.
 * False hits from the stack(s) are much more dangerous than false hits
 * from elsewhere, since the former can pin a large object that spans the
 * block, even though it does not start on the dangerous block.
 */

/*
 * Externally callable routines are:

 * MK_GC_add_to_black_list_normal
 * MK_GC_add_to_black_list_stack
 * MK_GC_promote_black_lists
 * MK_GC_is_black_listed
 *
 * All require that the allocator lock is held.
 */

/* Pointers to individual tables.  We replace one table by another by   */
/* switching these pointers.                                            */
STATIC word * MK_GC_old_normal_bl = NULL;
                /* Nonstack false references seen at last full          */
                /* collection.                                          */
STATIC word * MK_GC_incomplete_normal_bl = NULL;
                /* Nonstack false references seen since last            */
                /* full collection.                                     */
STATIC word * MK_GC_old_stack_bl = NULL;
STATIC word * MK_GC_incomplete_stack_bl = NULL;

STATIC word MK_GC_total_stack_black_listed = 0;
                        /* Number of bytes on stack blacklist.  */

MK_GC_INNER word MK_GC_black_list_spacing = MINHINCR * HBLKSIZE;
                        /* Initial rough guess. */

STATIC void MK_GC_clear_bl(word *);

MK_GC_INNER void MK_GC_default_print_heap_obj_proc(ptr_t p)
{
    ptr_t base = MK_GC_base(p);
    int kind = HDR(base)->hb_obj_kind;

    MK_GC_err_printf("object at %p of appr. %lu bytes (%s)\n",
                  base, (unsigned long)MK_GC_size(base),
                  kind == PTRFREE ? "atomic" :
                    IS_UNCOLLECTABLE(kind) ? "uncollectable" : "composite");
}

MK_GC_INNER void (*MK_GC_print_heap_obj)(ptr_t p) = MK_GC_default_print_heap_obj_proc;

#ifdef PRINT_BLACK_LIST
  STATIC void MK_GC_print_blacklisted_ptr(word p, ptr_t source,
                                       const char *kind_str)
  {
    ptr_t base = MK_GC_base(source);

    if (0 == base) {
        MK_GC_err_printf("Black listing (%s) %p referenced from %p in %s\n",
                      kind_str, (ptr_t)p, source,
                      NULL != source ? "root set" : "register");
    } else {
        /* FIXME: We can't call the debug version of MK_GC_print_heap_obj  */
        /* (with PRINT_CALL_CHAIN) here because the lock is held and    */
        /* the world is stopped.                                        */
        MK_GC_err_printf("Black listing (%s) %p referenced from %p in"
                      " object at %p of appr. %lu bytes\n",
                      kind_str, (ptr_t)p, source,
                      base, (unsigned long)MK_GC_size(base));
    }
  }
#endif /* PRINT_BLACK_LIST */

MK_GC_INNER void MK_GC_bl_init_no_interiors(void)
{
  if (MK_GC_incomplete_normal_bl == 0) {
    MK_GC_old_normal_bl = (word *)MK_GC_scratch_alloc(sizeof(page_hash_table));
    MK_GC_incomplete_normal_bl = (word *)MK_GC_scratch_alloc(
                                                  sizeof(page_hash_table));
    if (MK_GC_old_normal_bl == 0 || MK_GC_incomplete_normal_bl == 0) {
      MK_GC_err_printf("Insufficient memory for black list\n");
      EXIT();
    }
    MK_GC_clear_bl(MK_GC_old_normal_bl);
    MK_GC_clear_bl(MK_GC_incomplete_normal_bl);
  }
}

MK_GC_INNER void MK_GC_bl_init(void)
{
    if (!MK_GC_all_interior_pointers) {
      MK_GC_bl_init_no_interiors();
    }
    MK_GC_old_stack_bl = (word *)MK_GC_scratch_alloc(sizeof(page_hash_table));
    MK_GC_incomplete_stack_bl = (word *)MK_GC_scratch_alloc(sizeof(page_hash_table));
    if (MK_GC_old_stack_bl == 0 || MK_GC_incomplete_stack_bl == 0) {
        MK_GC_err_printf("Insufficient memory for black list\n");
        EXIT();
    }
    MK_GC_clear_bl(MK_GC_old_stack_bl);
    MK_GC_clear_bl(MK_GC_incomplete_stack_bl);
}

STATIC void MK_GC_clear_bl(word *doomed)
{
    BZERO(doomed, sizeof(page_hash_table));
}

STATIC void MK_GC_copy_bl(word *old, word *new)
{
    BCOPY(old, new, sizeof(page_hash_table));
}

static word total_stack_black_listed(void);

/* Signal the completion of a collection.  Turn the incomplete black    */
/* lists into new black lists, etc.                                     */
MK_GC_INNER void MK_GC_promote_black_lists(void)
{
    word * very_old_normal_bl = MK_GC_old_normal_bl;
    word * very_old_stack_bl = MK_GC_old_stack_bl;

    MK_GC_old_normal_bl = MK_GC_incomplete_normal_bl;
    MK_GC_old_stack_bl = MK_GC_incomplete_stack_bl;
    if (!MK_GC_all_interior_pointers) {
      MK_GC_clear_bl(very_old_normal_bl);
    }
    MK_GC_clear_bl(very_old_stack_bl);
    MK_GC_incomplete_normal_bl = very_old_normal_bl;
    MK_GC_incomplete_stack_bl = very_old_stack_bl;
    MK_GC_total_stack_black_listed = total_stack_black_listed();
    MK_GC_VERBOSE_LOG_PRINTF(
                "%lu bytes in heap blacklisted for interior pointers\n",
                (unsigned long)MK_GC_total_stack_black_listed);
    if (MK_GC_total_stack_black_listed != 0) {
        MK_GC_black_list_spacing =
                HBLKSIZE*(MK_GC_heapsize/MK_GC_total_stack_black_listed);
    }
    if (MK_GC_black_list_spacing < 3 * HBLKSIZE) {
        MK_GC_black_list_spacing = 3 * HBLKSIZE;
    }
    if (MK_GC_black_list_spacing > MAXHINCR * HBLKSIZE) {
        MK_GC_black_list_spacing = MAXHINCR * HBLKSIZE;
        /* Makes it easier to allocate really huge blocks, which otherwise */
        /* may have problems with nonuniform blacklist distributions.      */
        /* This way we should always succeed immediately after growing the */
        /* heap.                                                           */
    }
}

MK_GC_INNER void MK_GC_unpromote_black_lists(void)
{
    if (!MK_GC_all_interior_pointers) {
      MK_GC_copy_bl(MK_GC_old_normal_bl, MK_GC_incomplete_normal_bl);
    }
    MK_GC_copy_bl(MK_GC_old_stack_bl, MK_GC_incomplete_stack_bl);
}

/* P is not a valid pointer reference, but it falls inside      */
/* the plausible heap bounds.                                   */
/* Add it to the normal incomplete black list if appropriate.   */
#ifdef PRINT_BLACK_LIST
  MK_GC_INNER void MK_GC_add_to_black_list_normal(word p, ptr_t source)
#else
  MK_GC_INNER void MK_GC_add_to_black_list_normal(word p)
#endif
{
  if (MK_GC_modws_valid_offsets[p & (sizeof(word)-1)]) {
    word index = PHT_HASH((word)p);

    if (HDR(p) == 0 || get_pht_entry_from_index(MK_GC_old_normal_bl, index)) {
#     ifdef PRINT_BLACK_LIST
        if (!get_pht_entry_from_index(MK_GC_incomplete_normal_bl, index)) {
          MK_GC_print_blacklisted_ptr(p, source, "normal");
        }
#     endif
      set_pht_entry_from_index(MK_GC_incomplete_normal_bl, index);
    } /* else this is probably just an interior pointer to an allocated */
      /* object, and isn't worth black listing.                         */
  }
}

/* And the same for false pointers from the stack. */
#ifdef PRINT_BLACK_LIST
  MK_GC_INNER void MK_GC_add_to_black_list_stack(word p, ptr_t source)
#else
  MK_GC_INNER void MK_GC_add_to_black_list_stack(word p)
#endif
{
  word index = PHT_HASH((word)p);

  if (HDR(p) == 0 || get_pht_entry_from_index(MK_GC_old_stack_bl, index)) {
#   ifdef PRINT_BLACK_LIST
      if (!get_pht_entry_from_index(MK_GC_incomplete_stack_bl, index)) {
        MK_GC_print_blacklisted_ptr(p, source, "stack");
      }
#   endif
    set_pht_entry_from_index(MK_GC_incomplete_stack_bl, index);
  }
}

/*
 * Is the block starting at h of size len bytes black listed?   If so,
 * return the address of the next plausible r such that (r, len) might not
 * be black listed.  (R may not actually be in the heap.  We guarantee only
 * that every smaller value of r after h is also black listed.)
 * If (h,len) is not black listed, return 0.
 * Knows about the structure of the black list hash tables.
 */
struct hblk * MK_GC_is_black_listed(struct hblk *h, word len)
{
    word index = PHT_HASH((word)h);
    word i;
    word nblocks;

    if (!MK_GC_all_interior_pointers
        && (get_pht_entry_from_index(MK_GC_old_normal_bl, index)
            || get_pht_entry_from_index(MK_GC_incomplete_normal_bl, index))) {
      return (h+1);
    }

    nblocks = divHBLKSZ(len);
    for (i = 0;;) {
        if (MK_GC_old_stack_bl[divWORDSZ(index)] == 0
            && MK_GC_incomplete_stack_bl[divWORDSZ(index)] == 0) {
            /* An easy case */
          i += WORDSZ - modWORDSZ(index);
        } else {
          if (get_pht_entry_from_index(MK_GC_old_stack_bl, index)
              || get_pht_entry_from_index(MK_GC_incomplete_stack_bl, index)) {
            return(h+i+1);
          }
          i++;
        }
        if (i >= nblocks) break;
        index = PHT_HASH((word)(h+i));
    }
    return(0);
}

/* Return the number of blacklisted blocks in a given range.    */
/* Used only for statistical purposes.                          */
/* Looks only at the MK_GC_incomplete_stack_bl.                    */
STATIC word MK_GC_number_stack_black_listed(struct hblk *start,
                                         struct hblk *endp1)
{
    register struct hblk * h;
    word result = 0;

    for (h = start; (word)h < (word)endp1; h++) {
        word index = PHT_HASH((word)h);

        if (get_pht_entry_from_index(MK_GC_old_stack_bl, index)) result++;
    }
    return(result);
}

/* Return the total number of (stack) black-listed bytes. */
static word total_stack_black_listed(void)
{
    register unsigned i;
    word total = 0;

    for (i = 0; i < MK_GC_n_heap_sects; i++) {
        struct hblk * start = (struct hblk *) MK_GC_heap_sects[i].hs_start;
        struct hblk * endp1 = start + MK_GC_heap_sects[i].hs_bytes/HBLKSIZE;

        total += MK_GC_number_stack_black_listed(start, endp1);
    }
    return(total * HBLKSIZE);
}
