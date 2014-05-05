/*
 * Copyright (c) 1991-1994 by Xerox Corporation.  All rights reserved.
 * Copyright (c) 1996-1999 by Silicon Graphics.  All rights reserved.
 * Copyright (c) 2003-2011 Hewlett-Packard Development Company, L.P.
 *
 *
 * THIS MATERIAL IS PROVIDED AS IS, WITH ABSOLUTELY NO WARRANTY EXPRESSED
 * OR IMPLIED.  ANY USE IS AT YOUR OWN RISK.
 *
 * Permission is hereby granted to use or copy this program
 * for any purpose, provided the above notices are retained on all copies.
 * Permission to modify the code and to distribute modified code is granted,
 * provided the above notices are retained, and a notice that the code was
 * modified is included with the above copyright notice.
 *
 */

MK_AO_INLINE unsigned/**/char
MK_AO_char_load(const volatile unsigned/**/char *addr)
{
  return __atomic_load_n(addr, __ATOMIC_RELAXED);
}
#define MK_AO_HAVE_char_load

MK_AO_INLINE unsigned/**/char
MK_AO_char_load_acquire(const volatile unsigned/**/char *addr)
{
  return __atomic_load_n(addr, __ATOMIC_ACQUIRE);
}
#define MK_AO_HAVE_char_load_acquire

/* char_load_full is generalized using load and nop_full, so that      */
/* char_load_read is defined using load and nop_read.                  */
/* char_store_full definition is omitted similar to load_full reason.  */

MK_AO_INLINE void
MK_AO_char_store(volatile unsigned/**/char *addr, unsigned/**/char value)
{
  __atomic_store_n(addr, value, __ATOMIC_RELAXED);
}
#define MK_AO_HAVE_char_store

MK_AO_INLINE void
MK_AO_char_store_release(volatile unsigned/**/char *addr, unsigned/**/char value)
{
  __atomic_store_n(addr, value, __ATOMIC_RELEASE);
}
#define MK_AO_HAVE_char_store_release

MK_AO_INLINE unsigned/**/char
MK_AO_char_fetch_compare_and_swap(volatile unsigned/**/char *addr,
                                unsigned/**/char old_val, unsigned/**/char new_val)
{
  return __sync_val_compare_and_swap(addr, old_val, new_val
                                     /* empty protection list */);
}
#define MK_AO_HAVE_char_fetch_compare_and_swap

/* TODO: Add CAS _acquire/release/full primitives. */

#ifndef MK_AO_GENERALIZE_ASM_BOOL_CAS
  MK_AO_INLINE int
  MK_AO_char_compare_and_swap(volatile unsigned/**/char *addr,
                            unsigned/**/char old_val, unsigned/**/char new_val)
  {
    return __sync_bool_compare_and_swap(addr, old_val, new_val
                                        /* empty protection list */);
  }
# define MK_AO_HAVE_char_compare_and_swap
#endif /* !MK_AO_GENERALIZE_ASM_BOOL_CAS */
/*
 * Copyright (c) 1991-1994 by Xerox Corporation.  All rights reserved.
 * Copyright (c) 1996-1999 by Silicon Graphics.  All rights reserved.
 * Copyright (c) 2003-2011 Hewlett-Packard Development Company, L.P.
 *
 *
 * THIS MATERIAL IS PROVIDED AS IS, WITH ABSOLUTELY NO WARRANTY EXPRESSED
 * OR IMPLIED.  ANY USE IS AT YOUR OWN RISK.
 *
 * Permission is hereby granted to use or copy this program
 * for any purpose, provided the above notices are retained on all copies.
 * Permission to modify the code and to distribute modified code is granted,
 * provided the above notices are retained, and a notice that the code was
 * modified is included with the above copyright notice.
 *
 */

MK_AO_INLINE unsigned/**/short
MK_AO_short_load(const volatile unsigned/**/short *addr)
{
  return __atomic_load_n(addr, __ATOMIC_RELAXED);
}
#define MK_AO_HAVE_short_load

MK_AO_INLINE unsigned/**/short
MK_AO_short_load_acquire(const volatile unsigned/**/short *addr)
{
  return __atomic_load_n(addr, __ATOMIC_ACQUIRE);
}
#define MK_AO_HAVE_short_load_acquire

/* short_load_full is generalized using load and nop_full, so that      */
/* short_load_read is defined using load and nop_read.                  */
/* short_store_full definition is omitted similar to load_full reason.  */

MK_AO_INLINE void
MK_AO_short_store(volatile unsigned/**/short *addr, unsigned/**/short value)
{
  __atomic_store_n(addr, value, __ATOMIC_RELAXED);
}
#define MK_AO_HAVE_short_store

MK_AO_INLINE void
MK_AO_short_store_release(volatile unsigned/**/short *addr, unsigned/**/short value)
{
  __atomic_store_n(addr, value, __ATOMIC_RELEASE);
}
#define MK_AO_HAVE_short_store_release

MK_AO_INLINE unsigned/**/short
MK_AO_short_fetch_compare_and_swap(volatile unsigned/**/short *addr,
                                unsigned/**/short old_val, unsigned/**/short new_val)
{
  return __sync_val_compare_and_swap(addr, old_val, new_val
                                     /* empty protection list */);
}
#define MK_AO_HAVE_short_fetch_compare_and_swap

/* TODO: Add CAS _acquire/release/full primitives. */

#ifndef MK_AO_GENERALIZE_ASM_BOOL_CAS
  MK_AO_INLINE int
  MK_AO_short_compare_and_swap(volatile unsigned/**/short *addr,
                            unsigned/**/short old_val, unsigned/**/short new_val)
  {
    return __sync_bool_compare_and_swap(addr, old_val, new_val
                                        /* empty protection list */);
  }
# define MK_AO_HAVE_short_compare_and_swap
#endif /* !MK_AO_GENERALIZE_ASM_BOOL_CAS */
/*
 * Copyright (c) 1991-1994 by Xerox Corporation.  All rights reserved.
 * Copyright (c) 1996-1999 by Silicon Graphics.  All rights reserved.
 * Copyright (c) 2003-2011 Hewlett-Packard Development Company, L.P.
 *
 *
 * THIS MATERIAL IS PROVIDED AS IS, WITH ABSOLUTELY NO WARRANTY EXPRESSED
 * OR IMPLIED.  ANY USE IS AT YOUR OWN RISK.
 *
 * Permission is hereby granted to use or copy this program
 * for any purpose, provided the above notices are retained on all copies.
 * Permission to modify the code and to distribute modified code is granted,
 * provided the above notices are retained, and a notice that the code was
 * modified is included with the above copyright notice.
 *
 */

MK_AO_INLINE unsigned
MK_AO_int_load(const volatile unsigned *addr)
{
  return __atomic_load_n(addr, __ATOMIC_RELAXED);
}
#define MK_AO_HAVE_int_load

MK_AO_INLINE unsigned
MK_AO_int_load_acquire(const volatile unsigned *addr)
{
  return __atomic_load_n(addr, __ATOMIC_ACQUIRE);
}
#define MK_AO_HAVE_int_load_acquire

/* int_load_full is generalized using load and nop_full, so that      */
/* int_load_read is defined using load and nop_read.                  */
/* int_store_full definition is omitted similar to load_full reason.  */

MK_AO_INLINE void
MK_AO_int_store(volatile unsigned *addr, unsigned value)
{
  __atomic_store_n(addr, value, __ATOMIC_RELAXED);
}
#define MK_AO_HAVE_int_store

MK_AO_INLINE void
MK_AO_int_store_release(volatile unsigned *addr, unsigned value)
{
  __atomic_store_n(addr, value, __ATOMIC_RELEASE);
}
#define MK_AO_HAVE_int_store_release

MK_AO_INLINE unsigned
MK_AO_int_fetch_compare_and_swap(volatile unsigned *addr,
                                unsigned old_val, unsigned new_val)
{
  return __sync_val_compare_and_swap(addr, old_val, new_val
                                     /* empty protection list */);
}
#define MK_AO_HAVE_int_fetch_compare_and_swap

/* TODO: Add CAS _acquire/release/full primitives. */

#ifndef MK_AO_GENERALIZE_ASM_BOOL_CAS
  MK_AO_INLINE int
  MK_AO_int_compare_and_swap(volatile unsigned *addr,
                            unsigned old_val, unsigned new_val)
  {
    return __sync_bool_compare_and_swap(addr, old_val, new_val
                                        /* empty protection list */);
  }
# define MK_AO_HAVE_int_compare_and_swap
#endif /* !MK_AO_GENERALIZE_ASM_BOOL_CAS */
/*
 * Copyright (c) 1991-1994 by Xerox Corporation.  All rights reserved.
 * Copyright (c) 1996-1999 by Silicon Graphics.  All rights reserved.
 * Copyright (c) 2003-2011 Hewlett-Packard Development Company, L.P.
 *
 *
 * THIS MATERIAL IS PROVIDED AS IS, WITH ABSOLUTELY NO WARRANTY EXPRESSED
 * OR IMPLIED.  ANY USE IS AT YOUR OWN RISK.
 *
 * Permission is hereby granted to use or copy this program
 * for any purpose, provided the above notices are retained on all copies.
 * Permission to modify the code and to distribute modified code is granted,
 * provided the above notices are retained, and a notice that the code was
 * modified is included with the above copyright notice.
 *
 */

MK_AO_INLINE MK_AO_t
MK_AO_load(const volatile MK_AO_t *addr)
{
  return __atomic_load_n(addr, __ATOMIC_RELAXED);
}
#define MK_AO_HAVE_load

MK_AO_INLINE MK_AO_t
MK_AO_load_acquire(const volatile MK_AO_t *addr)
{
  return __atomic_load_n(addr, __ATOMIC_ACQUIRE);
}
#define MK_AO_HAVE_load_acquire

/* load_full is generalized using load and nop_full, so that      */
/* load_read is defined using load and nop_read.                  */
/* store_full definition is omitted similar to load_full reason.  */

MK_AO_INLINE void
MK_AO_store(volatile MK_AO_t *addr, MK_AO_t value)
{
  __atomic_store_n(addr, value, __ATOMIC_RELAXED);
}
#define MK_AO_HAVE_store

MK_AO_INLINE void
MK_AO_store_release(volatile MK_AO_t *addr, MK_AO_t value)
{
  __atomic_store_n(addr, value, __ATOMIC_RELEASE);
}
#define MK_AO_HAVE_store_release

MK_AO_INLINE MK_AO_t
MK_AO_fetch_compare_and_swap(volatile MK_AO_t *addr,
                                MK_AO_t old_val, MK_AO_t new_val)
{
  return __sync_val_compare_and_swap(addr, old_val, new_val
                                     /* empty protection list */);
}
#define MK_AO_HAVE_fetch_compare_and_swap

/* TODO: Add CAS _acquire/release/full primitives. */

#ifndef MK_AO_GENERALIZE_ASM_BOOL_CAS
  MK_AO_INLINE int
  MK_AO_compare_and_swap(volatile MK_AO_t *addr,
                            MK_AO_t old_val, MK_AO_t new_val)
  {
    return __sync_bool_compare_and_swap(addr, old_val, new_val
                                        /* empty protection list */);
  }
# define MK_AO_HAVE_compare_and_swap
#endif /* !MK_AO_GENERALIZE_ASM_BOOL_CAS */
