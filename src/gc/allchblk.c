/*
 * Copyright 1988, 1989 Hans-J. Boehm, Alan J. Demers
 * Copyright (c) 1991-1994 by Xerox Corporation.  All rights reserved.
 * Copyright (c) 1998-1999 by Silicon Graphics.  All rights reserved.
 * Copyright (c) 1999 by Hewlett-Packard Company. All rights reserved.
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

#ifdef MK_GC_USE_ENTIRE_HEAP
  int MK_GC_use_entire_heap = TRUE;
#else
  int MK_GC_use_entire_heap = FALSE;
#endif

/*
 * Free heap blocks are kept on one of several free lists,
 * depending on the size of the block.  Each free list is doubly linked.
 * Adjacent free blocks are coalesced.
 */


# define MAX_BLACK_LIST_ALLOC (2*HBLKSIZE)
                /* largest block we will allocate starting on a black   */
                /* listed block.  Must be >= HBLKSIZE.                  */


# define UNIQUE_THRESHOLD 32
        /* Sizes up to this many HBLKs each have their own free list    */
# define HUGE_THRESHOLD 256
        /* Sizes of at least this many heap blocks are mapped to a      */
        /* single free list.                                            */
# define FL_COMPRESSION 8
        /* In between sizes map this many distinct sizes to a single    */
        /* bin.                                                         */

# define N_HBLK_FLS ((HUGE_THRESHOLD - UNIQUE_THRESHOLD) / FL_COMPRESSION \
                     + UNIQUE_THRESHOLD)

#ifndef MK_GC_GCJ_SUPPORT
  STATIC
#endif
  struct hblk * MK_GC_hblkfreelist[N_HBLK_FLS+1] = { 0 };
                                /* List of completely empty heap blocks */
                                /* Linked through hb_next field of      */
                                /* header structure associated with     */
                                /* block.  Remains externally visible   */
                                /* as used by GNU GCJ currently.        */

#ifndef MK_GC_GCJ_SUPPORT
  STATIC
#endif
  word MK_GC_free_bytes[N_HBLK_FLS+1] = { 0 };
        /* Number of free bytes on each list.  Remains visible to GCJ.  */

/* Return the largest n such that the number of free bytes on lists     */
/* n .. N_HBLK_FLS is greater or equal to MK_GC_max_large_allocd_bytes     */
/* minus MK_GC_large_allocd_bytes.  If there is no such n, return 0.       */
MK_GC_INLINE int MK_GC_enough_large_bytes_left(void)
{
    int n;
    word bytes = MK_GC_large_allocd_bytes;

    MK_GC_ASSERT(MK_GC_max_large_allocd_bytes <= MK_GC_heapsize);
    for (n = N_HBLK_FLS; n >= 0; --n) {
        bytes += MK_GC_free_bytes[n];
        if (bytes >= MK_GC_max_large_allocd_bytes) return n;
    }
    return 0;
}

/* Map a number of blocks to the appropriate large block free list index. */
STATIC int MK_GC_hblk_fl_from_blocks(word blocks_needed)
{
    if (blocks_needed <= UNIQUE_THRESHOLD) return (int)blocks_needed;
    if (blocks_needed >= HUGE_THRESHOLD) return N_HBLK_FLS;
    return (int)(blocks_needed - UNIQUE_THRESHOLD)/FL_COMPRESSION
                                        + UNIQUE_THRESHOLD;

}

# define PHDR(hhdr) HDR((hhdr) -> hb_prev)
# define NHDR(hhdr) HDR((hhdr) -> hb_next)

# ifdef USE_MUNMAP
#   define IS_MAPPED(hhdr) (((hhdr) -> hb_flags & WAS_UNMAPPED) == 0)
# else
#   define IS_MAPPED(hhdr) TRUE
# endif /* !USE_MUNMAP */

#if !defined(NO_DEBUGGING) || defined(MK_GC_ASSERTIONS)
  /* Should return the same value as MK_GC_large_free_bytes.       */
  MK_GC_INNER word MK_GC_compute_large_free_bytes(void)
  {
      struct hblk * h;
      hdr * hhdr;
      word total_free = 0;
      unsigned i;

      for (i = 0; i <= N_HBLK_FLS; ++i) {
        for (h = MK_GC_hblkfreelist[i]; h != 0; h = hhdr->hb_next) {
          hhdr = HDR(h);
          total_free += hhdr->hb_sz;
        }
      }
      return total_free;
  }
#endif /* !NO_DEBUGGING || MK_GC_ASSERTIONS */

# if !defined(NO_DEBUGGING)
void MK_GC_print_hblkfreelist(void)
{
    struct hblk * h;
    hdr * hhdr;
    unsigned i;
    word total;

    for (i = 0; i <= N_HBLK_FLS; ++i) {
      h = MK_GC_hblkfreelist[i];
      if (0 != h) MK_GC_printf("Free list %u (total size %lu):\n",
                            i, (unsigned long)MK_GC_free_bytes[i]);
      while (h != 0) {
        hhdr = HDR(h);
        MK_GC_printf("\t%p size %lu %s black listed\n",
                (void *)h, (unsigned long) hhdr -> hb_sz,
                MK_GC_is_black_listed(h, HBLKSIZE) != 0 ? "start" :
                MK_GC_is_black_listed(h, hhdr -> hb_sz) != 0 ? "partially" :
                                                        "not");
        h = hhdr -> hb_next;
      }
    }
    MK_GC_printf("MK_GC_large_free_bytes: %lu\n",
              (unsigned long)MK_GC_large_free_bytes);

    if ((total = MK_GC_compute_large_free_bytes()) != MK_GC_large_free_bytes)
          MK_GC_err_printf("MK_GC_large_free_bytes INCONSISTENT!! Should be: %lu\n",
                        (unsigned long)total);
}

/* Return the free list index on which the block described by the header */
/* appears, or -1 if it appears nowhere.                                 */
static int free_list_index_of(hdr *wanted)
{
    struct hblk * h;
    hdr * hhdr;
    int i;

    for (i = 0; i <= N_HBLK_FLS; ++i) {
      h = MK_GC_hblkfreelist[i];
      while (h != 0) {
        hhdr = HDR(h);
        if (hhdr == wanted) return i;
        h = hhdr -> hb_next;
      }
    }
    return -1;
}

void MK_GC_dump_regions(void)
{
    unsigned i;
    ptr_t start, end;
    ptr_t p;
    size_t bytes;
    hdr *hhdr;
    for (i = 0; i < MK_GC_n_heap_sects; ++i) {
        start = MK_GC_heap_sects[i].hs_start;
        bytes = MK_GC_heap_sects[i].hs_bytes;
        end = start + bytes;
        /* Merge in contiguous sections.        */
          while (i+1 < MK_GC_n_heap_sects && MK_GC_heap_sects[i+1].hs_start == end) {
            ++i;
            end = MK_GC_heap_sects[i].hs_start + MK_GC_heap_sects[i].hs_bytes;
          }
        MK_GC_printf("***Section from %p to %p\n", start, end);
        for (p = start; (word)p < (word)end; ) {
            hhdr = HDR(p);
            if (IS_FORWARDING_ADDR_OR_NIL(hhdr)) {
                MK_GC_printf("\t%p Missing header!!(%p)\n", p, (void *)hhdr);
                p += HBLKSIZE;
                continue;
            }
            if (HBLK_IS_FREE(hhdr)) {
                int correct_index = MK_GC_hblk_fl_from_blocks(
                                        divHBLKSZ(hhdr -> hb_sz));
                int actual_index;

                MK_GC_printf("\t%p\tfree block of size 0x%lx bytes%s\n", p,
                          (unsigned long)(hhdr -> hb_sz),
                          IS_MAPPED(hhdr) ? "" : " (unmapped)");
                actual_index = free_list_index_of(hhdr);
                if (-1 == actual_index) {
                    MK_GC_printf("\t\tBlock not on free list %d!!\n",
                              correct_index);
                } else if (correct_index != actual_index) {
                    MK_GC_printf("\t\tBlock on list %d, should be on %d!!\n",
                              actual_index, correct_index);
                }
                p += hhdr -> hb_sz;
            } else {
                MK_GC_printf("\t%p\tused for blocks of size 0x%lx bytes\n", p,
                          (unsigned long)(hhdr -> hb_sz));
                p += HBLKSIZE * OBJ_SZ_TO_BLOCKS(hhdr -> hb_sz);
            }
        }
    }
}

# endif /* NO_DEBUGGING */

/* Initialize hdr for a block containing the indicated size and         */
/* kind of objects.                                                     */
/* Return FALSE on failure.                                             */
static MK_GC_bool setup_header(hdr * hhdr, struct hblk *block, size_t byte_sz,
                            int kind, unsigned flags)
{
    word descr;
#   ifndef MARK_BIT_PER_OBJ
      size_t granules;
#   endif

#   ifdef ENABLE_DISCLAIM
      if (MK_GC_obj_kinds[kind].ok_disclaim_proc)
        flags |= HAS_DISCLAIM;
      if (MK_GC_obj_kinds[kind].ok_mark_unconditionally)
        flags |= MARK_UNCONDITIONALLY;
#   endif

    /* Set size, kind and mark proc fields */
      hhdr -> hb_sz = byte_sz;
      hhdr -> hb_obj_kind = (unsigned char)kind;
      hhdr -> hb_flags = (unsigned char)flags;
      hhdr -> hb_block = block;
      descr = MK_GC_obj_kinds[kind].ok_descriptor;
      if (MK_GC_obj_kinds[kind].ok_relocate_descr) descr += byte_sz;
      hhdr -> hb_descr = descr;

#   ifdef MARK_BIT_PER_OBJ
     /* Set hb_inv_sz as portably as possible.                          */
     /* We set it to the smallest value such that sz * inv_sz > 2**32    */
     /* This may be more precision than necessary.                      */
      if (byte_sz > MAXOBJBYTES) {
         hhdr -> hb_inv_sz = LARGE_INV_SZ;
      } else {
        word inv_sz;

#       if CPP_WORDSZ == 64
          inv_sz = ((word)1 << 32)/byte_sz;
          if (((inv_sz*byte_sz) >> 32) == 0) ++inv_sz;
#       else  /* 32 bit words */
          MK_GC_ASSERT(byte_sz >= 4);
          inv_sz = ((unsigned)1 << 31)/byte_sz;
          inv_sz *= 2;
          while (inv_sz*byte_sz > byte_sz) ++inv_sz;
#       endif
        hhdr -> hb_inv_sz = inv_sz;
      }
#   else /* MARK_BIT_PER_GRANULE */
      hhdr -> hb_large_block = (unsigned char)(byte_sz > MAXOBJBYTES);
      granules = BYTES_TO_GRANULES(byte_sz);
      if (EXPECT(!MK_GC_add_map_entry(granules), FALSE)) {
        /* Make it look like a valid block. */
        hhdr -> hb_sz = HBLKSIZE;
        hhdr -> hb_descr = 0;
        hhdr -> hb_large_block = TRUE;
        hhdr -> hb_map = 0;
        return FALSE;
      } else {
        size_t index = (hhdr -> hb_large_block? 0 : granules);
        hhdr -> hb_map = MK_GC_obj_map[index];
      }
#   endif /* MARK_BIT_PER_GRANULE */

    /* Clear mark bits */
    MK_GC_clear_hdr_marks(hhdr);

    hhdr -> hb_last_reclaimed = (unsigned short)MK_GC_gc_no;
    return(TRUE);
}

/* Remove hhdr from the free list (it is assumed to specified by index). */
STATIC void MK_GC_remove_from_fl_at(hdr *hhdr, int index)
{
    MK_GC_ASSERT(((hhdr -> hb_sz) & (HBLKSIZE-1)) == 0);
    if (hhdr -> hb_prev == 0) {
        MK_GC_ASSERT(HDR(MK_GC_hblkfreelist[index]) == hhdr);
        MK_GC_hblkfreelist[index] = hhdr -> hb_next;
    } else {
        hdr *phdr;
        GET_HDR(hhdr -> hb_prev, phdr);
        phdr -> hb_next = hhdr -> hb_next;
    }
    /* We always need index to maintain free counts.    */
    MK_GC_ASSERT(MK_GC_free_bytes[index] >= hhdr -> hb_sz);
    MK_GC_free_bytes[index] -= hhdr -> hb_sz;
    if (0 != hhdr -> hb_next) {
        hdr * nhdr;
        MK_GC_ASSERT(!IS_FORWARDING_ADDR_OR_NIL(NHDR(hhdr)));
        GET_HDR(hhdr -> hb_next, nhdr);
        nhdr -> hb_prev = hhdr -> hb_prev;
    }
}

/* Remove hhdr from the appropriate free list (we assume it is on the   */
/* size-appropriate free list).                                         */
MK_GC_INLINE void MK_GC_remove_from_fl(hdr *hhdr)
{
  MK_GC_remove_from_fl_at(hhdr, MK_GC_hblk_fl_from_blocks(divHBLKSZ(hhdr->hb_sz)));
}

/* Return a pointer to the free block ending just before h, if any.     */
STATIC struct hblk * MK_GC_free_block_ending_at(struct hblk *h)
{
    struct hblk * p = h - 1;
    hdr * phdr;

    GET_HDR(p, phdr);
    while (0 != phdr && IS_FORWARDING_ADDR_OR_NIL(phdr)) {
        p = FORWARDED_ADDR(p,phdr);
        phdr = HDR(p);
    }
    if (0 != phdr) {
        if(HBLK_IS_FREE(phdr)) {
            return p;
        } else {
            return 0;
        }
    }
    p = MK_GC_prev_block(h - 1);
    if (0 != p) {
      phdr = HDR(p);
      if (HBLK_IS_FREE(phdr) && (ptr_t)p + phdr -> hb_sz == (ptr_t)h) {
        return p;
      }
    }
    return 0;
}

/* Add hhdr to the appropriate free list.               */
/* We maintain individual free lists sorted by address. */
STATIC void MK_GC_add_to_fl(struct hblk *h, hdr *hhdr)
{
    int index = MK_GC_hblk_fl_from_blocks(divHBLKSZ(hhdr -> hb_sz));
    struct hblk *second = MK_GC_hblkfreelist[index];
    hdr * second_hdr;
#   if defined(MK_GC_ASSERTIONS) && !defined(USE_MUNMAP)
      struct hblk *next = (struct hblk *)((word)h + hhdr -> hb_sz);
      hdr * nexthdr = HDR(next);
      struct hblk *prev = MK_GC_free_block_ending_at(h);
      hdr * prevhdr = HDR(prev);
      MK_GC_ASSERT(nexthdr == 0 || !HBLK_IS_FREE(nexthdr)
                || (signed_word)MK_GC_heapsize < 0);
                /* In the last case, blocks may be too large to merge. */
      MK_GC_ASSERT(prev == 0 || !HBLK_IS_FREE(prevhdr)
                || (signed_word)MK_GC_heapsize < 0);
#   endif

    MK_GC_ASSERT(((hhdr -> hb_sz) & (HBLKSIZE-1)) == 0);
    MK_GC_hblkfreelist[index] = h;
    MK_GC_free_bytes[index] += hhdr -> hb_sz;
    MK_GC_ASSERT(MK_GC_free_bytes[index] <= MK_GC_large_free_bytes);
    hhdr -> hb_next = second;
    hhdr -> hb_prev = 0;
    if (0 != second) {
      GET_HDR(second, second_hdr);
      second_hdr -> hb_prev = h;
    }
    hhdr -> hb_flags |= FREE_BLK;
}

#ifdef USE_MUNMAP

#   ifndef MUNMAP_THRESHOLD
#     define MUNMAP_THRESHOLD 6
#   endif

MK_GC_INNER int MK_GC_unmap_threshold = MUNMAP_THRESHOLD;

/* Unmap blocks that haven't been recently touched.  This is the only way */
/* way blocks are ever unmapped.                                          */
MK_GC_INNER void MK_GC_unmap_old(void)
{
    struct hblk * h;
    hdr * hhdr;
    int i;

    if (MK_GC_unmap_threshold == 0)
      return; /* unmapping disabled */

    for (i = 0; i <= N_HBLK_FLS; ++i) {
      for (h = MK_GC_hblkfreelist[i]; 0 != h; h = hhdr -> hb_next) {
        hhdr = HDR(h);
        if (!IS_MAPPED(hhdr)) continue;

        if ((unsigned short)MK_GC_gc_no - hhdr -> hb_last_reclaimed >
                (unsigned short)MK_GC_unmap_threshold) {
          MK_GC_unmap((ptr_t)h, hhdr -> hb_sz);
          hhdr -> hb_flags |= WAS_UNMAPPED;
        }
      }
    }
}

/* Merge all unmapped blocks that are adjacent to other free            */
/* blocks.  This may involve remapping, since all blocks are either     */
/* fully mapped or fully unmapped.                                      */
MK_GC_INNER void MK_GC_merge_unmapped(void)
{
    struct hblk * h, *next;
    hdr * hhdr, *nexthdr;
    word size, nextsize;
    int i;

    for (i = 0; i <= N_HBLK_FLS; ++i) {
      h = MK_GC_hblkfreelist[i];
      while (h != 0) {
        GET_HDR(h, hhdr);
        size = hhdr->hb_sz;
        next = (struct hblk *)((word)h + size);
        GET_HDR(next, nexthdr);
        /* Coalesce with successor, if possible */
          if (0 != nexthdr && HBLK_IS_FREE(nexthdr)
              && (signed_word) (size + (nextsize = nexthdr->hb_sz)) > 0
                 /* no pot. overflow */) {
            /* Note that we usually try to avoid adjacent free blocks   */
            /* that are either both mapped or both unmapped.  But that  */
            /* isn't guaranteed to hold since we remap blocks when we   */
            /* split them, and don't merge at that point.  It may also  */
            /* not hold if the merged block would be too big.           */
            if (IS_MAPPED(hhdr) && !IS_MAPPED(nexthdr)) {
              /* make both consistent, so that we can merge */
                if (size > nextsize) {
                  MK_GC_remap((ptr_t)next, nextsize);
                } else {
                  MK_GC_unmap((ptr_t)h, size);
                  MK_GC_unmap_gap((ptr_t)h, size, (ptr_t)next, nextsize);
                  hhdr -> hb_flags |= WAS_UNMAPPED;
                }
            } else if (IS_MAPPED(nexthdr) && !IS_MAPPED(hhdr)) {
              if (size > nextsize) {
                MK_GC_unmap((ptr_t)next, nextsize);
                MK_GC_unmap_gap((ptr_t)h, size, (ptr_t)next, nextsize);
              } else {
                MK_GC_remap((ptr_t)h, size);
                hhdr -> hb_flags &= ~WAS_UNMAPPED;
                hhdr -> hb_last_reclaimed = nexthdr -> hb_last_reclaimed;
              }
            } else if (!IS_MAPPED(hhdr) && !IS_MAPPED(nexthdr)) {
              /* Unmap any gap in the middle */
                MK_GC_unmap_gap((ptr_t)h, size, (ptr_t)next, nextsize);
            }
            /* If they are both unmapped, we merge, but leave unmapped. */
            MK_GC_remove_from_fl_at(hhdr, i);
            MK_GC_remove_from_fl(nexthdr);
            hhdr -> hb_sz += nexthdr -> hb_sz;
            MK_GC_remove_header(next);
            MK_GC_add_to_fl(h, hhdr);
            /* Start over at beginning of list */
            h = MK_GC_hblkfreelist[i];
          } else /* not mergable with successor */ {
            h = hhdr -> hb_next;
          }
      } /* while (h != 0) ... */
    } /* for ... */
}

#endif /* USE_MUNMAP */

/*
 * Return a pointer to a block starting at h of length bytes.
 * Memory for the block is mapped.
 * Remove the block from its free list, and return the remainder (if any)
 * to its appropriate free list.
 * May fail by returning 0.
 * The header for the returned block must be set up by the caller.
 * If the return value is not 0, then hhdr is the header for it.
 */
STATIC struct hblk * MK_GC_get_first_part(struct hblk *h, hdr *hhdr,
                                       size_t bytes, int index)
{
    word total_size = hhdr -> hb_sz;
    struct hblk * rest;
    hdr * rest_hdr;

    MK_GC_ASSERT((total_size & (HBLKSIZE-1)) == 0);
    MK_GC_remove_from_fl_at(hhdr, index);
    if (total_size == bytes) return h;
    rest = (struct hblk *)((word)h + bytes);
    rest_hdr = MK_GC_install_header(rest);
    if (0 == rest_hdr) {
        /* FIXME: This is likely to be very bad news ... */
        WARN("Header allocation failed: Dropping block.\n", 0);
        return(0);
    }
    rest_hdr -> hb_sz = total_size - bytes;
    rest_hdr -> hb_flags = 0;
#   ifdef MK_GC_ASSERTIONS
      /* Mark h not free, to avoid assertion about adjacent free blocks. */
        hhdr -> hb_flags &= ~FREE_BLK;
#   endif
    MK_GC_add_to_fl(rest, rest_hdr);
    return h;
}

/*
 * H is a free block.  N points at an address inside it.
 * A new header for n has already been set up.  Fix up h's header
 * to reflect the fact that it is being split, move it to the
 * appropriate free list.
 * N replaces h in the original free list.
 *
 * Nhdr is not completely filled in, since it is about to allocated.
 * It may in fact end up on the wrong free list for its size.
 * That's not a disaster, since n is about to be allocated
 * by our caller.
 * (Hence adding it to a free list is silly.  But this path is hopefully
 * rare enough that it doesn't matter.  The code is cleaner this way.)
 */
STATIC void MK_GC_split_block(struct hblk *h, hdr *hhdr, struct hblk *n,
                           hdr *nhdr, int index /* Index of free list */)
{
    word total_size = hhdr -> hb_sz;
    word h_size = (word)n - (word)h;
    struct hblk *prev = hhdr -> hb_prev;
    struct hblk *next = hhdr -> hb_next;

    /* Replace h with n on its freelist */
      nhdr -> hb_prev = prev;
      nhdr -> hb_next = next;
      nhdr -> hb_sz = total_size - h_size;
      nhdr -> hb_flags = 0;
      if (0 != prev) {
        HDR(prev) -> hb_next = n;
      } else {
        MK_GC_hblkfreelist[index] = n;
      }
      if (0 != next) {
        HDR(next) -> hb_prev = n;
      }
      MK_GC_ASSERT(MK_GC_free_bytes[index] > h_size);
      MK_GC_free_bytes[index] -= h_size;
#   ifdef USE_MUNMAP
      hhdr -> hb_last_reclaimed = (unsigned short)MK_GC_gc_no;
#   endif
    hhdr -> hb_sz = h_size;
    MK_GC_add_to_fl(h, hhdr);
    nhdr -> hb_flags |= FREE_BLK;
}

STATIC struct hblk *
MK_GC_allochblk_nth(size_t sz /* bytes */, int kind, unsigned flags, int n,
                 int may_split);
#define AVOID_SPLIT_REMAPPED 2

/*
 * Allocate (and return pointer to) a heap block
 *   for objects of size sz bytes, searching the nth free list.
 *
 * NOTE: We set obj_map field in header correctly.
 *       Caller is responsible for building an object freelist in block.
 *
 * The client is responsible for clearing the block, if necessary.
 */
MK_GC_INNER struct hblk *
MK_GC_allochblk(size_t sz, int kind, unsigned flags/* IGNORE_OFF_PAGE or 0 */)
{
    word blocks;
    int start_list;
    struct hblk *result;
    int may_split;
    int split_limit; /* Highest index of free list whose blocks we      */
                     /* split.                                          */

    MK_GC_ASSERT((sz & (GRANULE_BYTES - 1)) == 0);
    blocks = OBJ_SZ_TO_BLOCKS(sz);
    if ((signed_word)(blocks * HBLKSIZE) < 0) {
      return 0;
    }
    start_list = MK_GC_hblk_fl_from_blocks(blocks);
    /* Try for an exact match first. */
    result = MK_GC_allochblk_nth(sz, kind, flags, start_list, FALSE);
    if (0 != result) return result;

    may_split = TRUE;
    if (MK_GC_use_entire_heap || MK_GC_dont_gc
        || USED_HEAP_SIZE < MK_GC_requested_heapsize
        || MK_GC_incremental || !MK_GC_should_collect()) {
        /* Should use more of the heap, even if it requires splitting. */
        split_limit = N_HBLK_FLS;
    } else if (MK_GC_finalizer_bytes_freed > (MK_GC_heapsize >> 4)) {
          /* If we are deallocating lots of memory from         */
          /* finalizers, fail and collect sooner rather         */
          /* than later.                                        */
          split_limit = 0;
    } else {
          /* If we have enough large blocks left to cover any   */
          /* previous request for large blocks, we go ahead     */
          /* and split.  Assuming a steady state, that should   */
          /* be safe.  It means that we can use the full        */
          /* heap if we allocate only small objects.            */
          split_limit = MK_GC_enough_large_bytes_left();
#         ifdef USE_MUNMAP
            if (split_limit > 0)
              may_split = AVOID_SPLIT_REMAPPED;
#         endif
    }
    if (start_list < UNIQUE_THRESHOLD) {
      /* No reason to try start_list again, since all blocks are exact  */
      /* matches.                                                       */
      ++start_list;
    }
    for (; start_list <= split_limit; ++start_list) {
        result = MK_GC_allochblk_nth(sz, kind, flags, start_list, may_split);
        if (0 != result)
            break;
    }
    return result;
}

STATIC long MK_GC_large_alloc_warn_suppressed = 0;
                        /* Number of warnings suppressed so far.        */

/* The same, but with search restricted to nth free list.  Flags is     */
/* IGNORE_OFF_PAGE or zero.  sz is in bytes.  The may_split flag        */
/* indicates whether it is OK to split larger blocks (if set to         */
/* AVOID_SPLIT_REMAPPED then memory remapping followed by splitting     */
/* should be generally avoided).                                        */
STATIC struct hblk *
MK_GC_allochblk_nth(size_t sz, int kind, unsigned flags, int n, int may_split)
{
    struct hblk *hbp;
    hdr * hhdr;                 /* Header corr. to hbp */
    struct hblk *thishbp;
    hdr * thishdr;              /* Header corr. to thishbp */
    signed_word size_needed;    /* number of bytes in requested objects */
    signed_word size_avail;     /* bytes available in this block        */

    size_needed = HBLKSIZE * OBJ_SZ_TO_BLOCKS(sz);

    /* search for a big enough block in free list */
        for (hbp = MK_GC_hblkfreelist[n];; hbp = hhdr -> hb_next) {
            if (NULL == hbp) return NULL;
            GET_HDR(hbp, hhdr); /* set hhdr value */
            size_avail = hhdr->hb_sz;
            if (size_avail < size_needed) continue;
            if (size_avail != size_needed) {
              signed_word next_size;

              if (!may_split) continue;
              /* If the next heap block is obviously better, go on.     */
              /* This prevents us from disassembling a single large     */
              /* block to get tiny blocks.                              */
              thishbp = hhdr -> hb_next;
              if (thishbp != 0) {
                GET_HDR(thishbp, thishdr);
                next_size = (signed_word)(thishdr -> hb_sz);
                if (next_size < size_avail
                    && next_size >= size_needed
                    && !MK_GC_is_black_listed(thishbp, (word)size_needed)) {
                    continue;
                }
              }
            }
            if (!IS_UNCOLLECTABLE(kind) && (kind != PTRFREE
                        || size_needed > (signed_word)MAX_BLACK_LIST_ALLOC)) {
              struct hblk * lasthbp = hbp;
              ptr_t search_end = (ptr_t)hbp + size_avail - size_needed;
              signed_word orig_avail = size_avail;
              signed_word eff_size_needed = (flags & IGNORE_OFF_PAGE) != 0 ?
                                                (signed_word)HBLKSIZE
                                                : size_needed;

              while ((word)lasthbp <= (word)search_end
                     && (thishbp = MK_GC_is_black_listed(lasthbp,
                                            (word)eff_size_needed)) != 0) {
                lasthbp = thishbp;
              }
              size_avail -= (ptr_t)lasthbp - (ptr_t)hbp;
              thishbp = lasthbp;
              if (size_avail >= size_needed) {
                if (thishbp != hbp) {
#                 ifdef USE_MUNMAP
                    /* Avoid remapping followed by splitting.   */
                    if (may_split == AVOID_SPLIT_REMAPPED && !IS_MAPPED(hhdr))
                      continue;
#                 endif
                  thishdr = MK_GC_install_header(thishbp);
                  if (0 != thishdr) {
                  /* Make sure it's mapped before we mangle it. */
#                   ifdef USE_MUNMAP
                      if (!IS_MAPPED(hhdr)) {
                        MK_GC_remap((ptr_t)hbp, hhdr -> hb_sz);
                        hhdr -> hb_flags &= ~WAS_UNMAPPED;
                      }
#                   endif
                  /* Split the block at thishbp */
                      MK_GC_split_block(hbp, hhdr, thishbp, thishdr, n);
                  /* Advance to thishbp */
                      hbp = thishbp;
                      hhdr = thishdr;
                      /* We must now allocate thishbp, since it may     */
                      /* be on the wrong free list.                     */
                  }
                }
              } else if (size_needed > (signed_word)BL_LIMIT
                         && orig_avail - size_needed
                            > (signed_word)BL_LIMIT) {
                /* Punt, since anything else risks unreasonable heap growth. */
                if (++MK_GC_large_alloc_warn_suppressed
                    >= MK_GC_large_alloc_warn_interval) {
                  WARN("Repeated allocation of very large block "
                       "(appr. size %" WARN_PRIdPTR "):\n"
                       "\tMay lead to memory leak and poor performance.\n",
                       size_needed);
                  MK_GC_large_alloc_warn_suppressed = 0;
                }
                size_avail = orig_avail;
              } else if (size_avail == 0 && size_needed == HBLKSIZE
                         && IS_MAPPED(hhdr)) {
                if (!MK_GC_find_leak) {
                  static unsigned count = 0;

                  /* The block is completely blacklisted.  We need      */
                  /* to drop some such blocks, since otherwise we spend */
                  /* all our time traversing them if pointer-free       */
                  /* blocks are unpopular.                              */
                  /* A dropped block will be reconsidered at next GC.   */
                  if ((++count & 3) == 0) {
                    /* Allocate and drop the block in small chunks, to  */
                    /* maximize the chance that we will recover some    */
                    /* later.                                           */
                      word total_size = hhdr -> hb_sz;
                      struct hblk * limit = hbp + divHBLKSZ(total_size);
                      struct hblk * h;
                      struct hblk * prev = hhdr -> hb_prev;

                      MK_GC_large_free_bytes -= total_size;
                      MK_GC_bytes_dropped += total_size;
                      MK_GC_remove_from_fl_at(hhdr, n);
                      for (h = hbp; (word)h < (word)limit; h++) {
                        if (h != hbp) {
                          hhdr = MK_GC_install_header(h);
                        }
                        if (NULL != hhdr) {
                          (void)setup_header(hhdr, h, HBLKSIZE, PTRFREE, 0);
                                                    /* Can't fail. */
                          if (MK_GC_debugging_started) {
                            BZERO(h, HBLKSIZE);
                          }
                        }
                      }
                    /* Restore hbp to point at free block */
                      hbp = prev;
                      if (0 == hbp) {
                        return MK_GC_allochblk_nth(sz, kind, flags, n, may_split);
                      }
                      hhdr = HDR(hbp);
                  }
                }
              }
            }
            if( size_avail >= size_needed ) {
#               ifdef USE_MUNMAP
                  if (!IS_MAPPED(hhdr)) {
                    MK_GC_remap((ptr_t)hbp, hhdr -> hb_sz);
                    hhdr -> hb_flags &= ~WAS_UNMAPPED;
                    /* Note: This may leave adjacent, mapped free blocks. */
                  }
#               endif
                /* hbp may be on the wrong freelist; the parameter n    */
                /* is important.                                        */
                hbp = MK_GC_get_first_part(hbp, hhdr, size_needed, n);
                break;
            }
        }

    if (0 == hbp) return 0;

    /* Add it to map of valid blocks */
        if (!MK_GC_install_counts(hbp, (word)size_needed)) return(0);
        /* This leaks memory under very rare conditions. */

    /* Set up header */
        if (!setup_header(hhdr, hbp, sz, kind, flags)) {
            MK_GC_remove_counts(hbp, (word)size_needed);
            return(0); /* ditto */
        }
#   ifndef MK_GC_DISABLE_INCREMENTAL
        /* Notify virtual dirty bit implementation that we are about to */
        /* write.  Ensure that pointer-free objects are not protected   */
        /* if it is avoidable.  This also ensures that newly allocated  */
        /* blocks are treated as dirty.  Necessary since we don't       */
        /* protect free blocks.                                         */
        MK_GC_ASSERT((size_needed & (HBLKSIZE-1)) == 0);
        MK_GC_remove_protection(hbp, divHBLKSZ(size_needed),
                             (hhdr -> hb_descr == 0) /* pointer-free */);
#   endif
    /* We just successfully allocated a block.  Restart count of        */
    /* consecutive failures.                                            */
    MK_GC_fail_count = 0;

    MK_GC_large_free_bytes -= size_needed;
    MK_GC_ASSERT(IS_MAPPED(hhdr));
    return( hbp );
}

/*
 * Free a heap block.
 *
 * Coalesce the block with its neighbors if possible.
 *
 * All mark words are assumed to be cleared.
 */
MK_GC_INNER void MK_GC_freehblk(struct hblk *hbp)
{
    struct hblk *next, *prev;
    hdr *hhdr, *prevhdr, *nexthdr;
    word size;

    GET_HDR(hbp, hhdr);
    size = HBLKSIZE * OBJ_SZ_TO_BLOCKS(hhdr->hb_sz);
    if ((signed_word)size <= 0)
      ABORT("Deallocating excessively large block.  Too large an allocation?");
      /* Probably possible if we try to allocate more than half the address */
      /* space at once.  If we don't catch it here, strange things happen   */
      /* later.                                                             */
    MK_GC_remove_counts(hbp, size);
    hhdr->hb_sz = size;
#   ifdef USE_MUNMAP
      hhdr -> hb_last_reclaimed = (unsigned short)MK_GC_gc_no;
#   endif

    /* Check for duplicate deallocation in the easy case */
      if (HBLK_IS_FREE(hhdr)) {
        ABORT_ARG1("Duplicate large block deallocation",
                   " of %p", (void *)hbp);
      }

    MK_GC_ASSERT(IS_MAPPED(hhdr));
    hhdr -> hb_flags |= FREE_BLK;
    next = (struct hblk *)((ptr_t)hbp + size);
    GET_HDR(next, nexthdr);
    prev = MK_GC_free_block_ending_at(hbp);
    /* Coalesce with successor, if possible */
      if(0 != nexthdr && HBLK_IS_FREE(nexthdr) && IS_MAPPED(nexthdr)
         && (signed_word)(hhdr -> hb_sz + nexthdr -> hb_sz) > 0
         /* no overflow */) {
        MK_GC_remove_from_fl(nexthdr);
        hhdr -> hb_sz += nexthdr -> hb_sz;
        MK_GC_remove_header(next);
      }
    /* Coalesce with predecessor, if possible. */
      if (0 != prev) {
        prevhdr = HDR(prev);
        if (IS_MAPPED(prevhdr)
            && (signed_word)(hhdr -> hb_sz + prevhdr -> hb_sz) > 0) {
          MK_GC_remove_from_fl(prevhdr);
          prevhdr -> hb_sz += hhdr -> hb_sz;
#         ifdef USE_MUNMAP
            prevhdr -> hb_last_reclaimed = (unsigned short)MK_GC_gc_no;
#         endif
          MK_GC_remove_header(hbp);
          hbp = prev;
          hhdr = prevhdr;
        }
      }
    /* FIXME: It is not clear we really always want to do these merges  */
    /* with USE_MUNMAP, since it updates ages and hence prevents        */
    /* unmapping.                                                       */

    MK_GC_large_free_bytes += size;
    MK_GC_add_to_fl(hbp, hhdr);
}
