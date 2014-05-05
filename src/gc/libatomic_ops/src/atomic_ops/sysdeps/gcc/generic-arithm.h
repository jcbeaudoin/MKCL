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
MK_AO_char_fetch_and_add(volatile unsigned/**/char *addr, unsigned/**/char incr)
{
  return __atomic_fetch_add(addr, incr, __ATOMIC_RELAXED);
}
#define MK_AO_HAVE_char_fetch_and_add

MK_AO_INLINE void
MK_AO_char_and(volatile unsigned/**/char *addr, unsigned/**/char value)
{
  (void)__atomic_and_fetch(addr, value, __ATOMIC_RELAXED);
}
#define MK_AO_HAVE_char_and

MK_AO_INLINE void
MK_AO_char_or(volatile unsigned/**/char *addr, unsigned/**/char value)
{
  (void)__atomic_or_fetch(addr, value, __ATOMIC_RELAXED);
}
#define MK_AO_HAVE_char_or

MK_AO_INLINE void
MK_AO_char_xor(volatile unsigned/**/char *addr, unsigned/**/char value)
{
  (void)__atomic_xor_fetch(addr, value, __ATOMIC_RELAXED);
}
#define MK_AO_HAVE_char_xor
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
MK_AO_short_fetch_and_add(volatile unsigned/**/short *addr, unsigned/**/short incr)
{
  return __atomic_fetch_add(addr, incr, __ATOMIC_RELAXED);
}
#define MK_AO_HAVE_short_fetch_and_add

MK_AO_INLINE void
MK_AO_short_and(volatile unsigned/**/short *addr, unsigned/**/short value)
{
  (void)__atomic_and_fetch(addr, value, __ATOMIC_RELAXED);
}
#define MK_AO_HAVE_short_and

MK_AO_INLINE void
MK_AO_short_or(volatile unsigned/**/short *addr, unsigned/**/short value)
{
  (void)__atomic_or_fetch(addr, value, __ATOMIC_RELAXED);
}
#define MK_AO_HAVE_short_or

MK_AO_INLINE void
MK_AO_short_xor(volatile unsigned/**/short *addr, unsigned/**/short value)
{
  (void)__atomic_xor_fetch(addr, value, __ATOMIC_RELAXED);
}
#define MK_AO_HAVE_short_xor
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
MK_AO_int_fetch_and_add(volatile unsigned *addr, unsigned incr)
{
  return __atomic_fetch_add(addr, incr, __ATOMIC_RELAXED);
}
#define MK_AO_HAVE_int_fetch_and_add

MK_AO_INLINE void
MK_AO_int_and(volatile unsigned *addr, unsigned value)
{
  (void)__atomic_and_fetch(addr, value, __ATOMIC_RELAXED);
}
#define MK_AO_HAVE_int_and

MK_AO_INLINE void
MK_AO_int_or(volatile unsigned *addr, unsigned value)
{
  (void)__atomic_or_fetch(addr, value, __ATOMIC_RELAXED);
}
#define MK_AO_HAVE_int_or

MK_AO_INLINE void
MK_AO_int_xor(volatile unsigned *addr, unsigned value)
{
  (void)__atomic_xor_fetch(addr, value, __ATOMIC_RELAXED);
}
#define MK_AO_HAVE_int_xor
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
MK_AO_fetch_and_add(volatile MK_AO_t *addr, MK_AO_t incr)
{
  return __atomic_fetch_add(addr, incr, __ATOMIC_RELAXED);
}
#define MK_AO_HAVE_fetch_and_add

MK_AO_INLINE void
MK_AO_and(volatile MK_AO_t *addr, MK_AO_t value)
{
  (void)__atomic_and_fetch(addr, value, __ATOMIC_RELAXED);
}
#define MK_AO_HAVE_and

MK_AO_INLINE void
MK_AO_or(volatile MK_AO_t *addr, MK_AO_t value)
{
  (void)__atomic_or_fetch(addr, value, __ATOMIC_RELAXED);
}
#define MK_AO_HAVE_or

MK_AO_INLINE void
MK_AO_xor(volatile MK_AO_t *addr, MK_AO_t value)
{
  (void)__atomic_xor_fetch(addr, value, __ATOMIC_RELAXED);
}
#define MK_AO_HAVE_xor
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
MK_AO_char_fetch_and_add_acquire(volatile unsigned/**/char *addr, unsigned/**/char incr)
{
  return __atomic_fetch_add(addr, incr, __ATOMIC_ACQUIRE);
}
#define MK_AO_HAVE_char_fetch_and_add_acquire

MK_AO_INLINE void
MK_AO_char_and_acquire(volatile unsigned/**/char *addr, unsigned/**/char value)
{
  (void)__atomic_and_fetch(addr, value, __ATOMIC_ACQUIRE);
}
#define MK_AO_HAVE_char_and_acquire

MK_AO_INLINE void
MK_AO_char_or_acquire(volatile unsigned/**/char *addr, unsigned/**/char value)
{
  (void)__atomic_or_fetch(addr, value, __ATOMIC_ACQUIRE);
}
#define MK_AO_HAVE_char_or_acquire

MK_AO_INLINE void
MK_AO_char_xor_acquire(volatile unsigned/**/char *addr, unsigned/**/char value)
{
  (void)__atomic_xor_fetch(addr, value, __ATOMIC_ACQUIRE);
}
#define MK_AO_HAVE_char_xor_acquire
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
MK_AO_short_fetch_and_add_acquire(volatile unsigned/**/short *addr, unsigned/**/short incr)
{
  return __atomic_fetch_add(addr, incr, __ATOMIC_ACQUIRE);
}
#define MK_AO_HAVE_short_fetch_and_add_acquire

MK_AO_INLINE void
MK_AO_short_and_acquire(volatile unsigned/**/short *addr, unsigned/**/short value)
{
  (void)__atomic_and_fetch(addr, value, __ATOMIC_ACQUIRE);
}
#define MK_AO_HAVE_short_and_acquire

MK_AO_INLINE void
MK_AO_short_or_acquire(volatile unsigned/**/short *addr, unsigned/**/short value)
{
  (void)__atomic_or_fetch(addr, value, __ATOMIC_ACQUIRE);
}
#define MK_AO_HAVE_short_or_acquire

MK_AO_INLINE void
MK_AO_short_xor_acquire(volatile unsigned/**/short *addr, unsigned/**/short value)
{
  (void)__atomic_xor_fetch(addr, value, __ATOMIC_ACQUIRE);
}
#define MK_AO_HAVE_short_xor_acquire
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
MK_AO_int_fetch_and_add_acquire(volatile unsigned *addr, unsigned incr)
{
  return __atomic_fetch_add(addr, incr, __ATOMIC_ACQUIRE);
}
#define MK_AO_HAVE_int_fetch_and_add_acquire

MK_AO_INLINE void
MK_AO_int_and_acquire(volatile unsigned *addr, unsigned value)
{
  (void)__atomic_and_fetch(addr, value, __ATOMIC_ACQUIRE);
}
#define MK_AO_HAVE_int_and_acquire

MK_AO_INLINE void
MK_AO_int_or_acquire(volatile unsigned *addr, unsigned value)
{
  (void)__atomic_or_fetch(addr, value, __ATOMIC_ACQUIRE);
}
#define MK_AO_HAVE_int_or_acquire

MK_AO_INLINE void
MK_AO_int_xor_acquire(volatile unsigned *addr, unsigned value)
{
  (void)__atomic_xor_fetch(addr, value, __ATOMIC_ACQUIRE);
}
#define MK_AO_HAVE_int_xor_acquire
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
MK_AO_fetch_and_add_acquire(volatile MK_AO_t *addr, MK_AO_t incr)
{
  return __atomic_fetch_add(addr, incr, __ATOMIC_ACQUIRE);
}
#define MK_AO_HAVE_fetch_and_add_acquire

MK_AO_INLINE void
MK_AO_and_acquire(volatile MK_AO_t *addr, MK_AO_t value)
{
  (void)__atomic_and_fetch(addr, value, __ATOMIC_ACQUIRE);
}
#define MK_AO_HAVE_and_acquire

MK_AO_INLINE void
MK_AO_or_acquire(volatile MK_AO_t *addr, MK_AO_t value)
{
  (void)__atomic_or_fetch(addr, value, __ATOMIC_ACQUIRE);
}
#define MK_AO_HAVE_or_acquire

MK_AO_INLINE void
MK_AO_xor_acquire(volatile MK_AO_t *addr, MK_AO_t value)
{
  (void)__atomic_xor_fetch(addr, value, __ATOMIC_ACQUIRE);
}
#define MK_AO_HAVE_xor_acquire
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
MK_AO_char_fetch_and_add_release(volatile unsigned/**/char *addr, unsigned/**/char incr)
{
  return __atomic_fetch_add(addr, incr, __ATOMIC_RELEASE);
}
#define MK_AO_HAVE_char_fetch_and_add_release

MK_AO_INLINE void
MK_AO_char_and_release(volatile unsigned/**/char *addr, unsigned/**/char value)
{
  (void)__atomic_and_fetch(addr, value, __ATOMIC_RELEASE);
}
#define MK_AO_HAVE_char_and_release

MK_AO_INLINE void
MK_AO_char_or_release(volatile unsigned/**/char *addr, unsigned/**/char value)
{
  (void)__atomic_or_fetch(addr, value, __ATOMIC_RELEASE);
}
#define MK_AO_HAVE_char_or_release

MK_AO_INLINE void
MK_AO_char_xor_release(volatile unsigned/**/char *addr, unsigned/**/char value)
{
  (void)__atomic_xor_fetch(addr, value, __ATOMIC_RELEASE);
}
#define MK_AO_HAVE_char_xor_release
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
MK_AO_short_fetch_and_add_release(volatile unsigned/**/short *addr, unsigned/**/short incr)
{
  return __atomic_fetch_add(addr, incr, __ATOMIC_RELEASE);
}
#define MK_AO_HAVE_short_fetch_and_add_release

MK_AO_INLINE void
MK_AO_short_and_release(volatile unsigned/**/short *addr, unsigned/**/short value)
{
  (void)__atomic_and_fetch(addr, value, __ATOMIC_RELEASE);
}
#define MK_AO_HAVE_short_and_release

MK_AO_INLINE void
MK_AO_short_or_release(volatile unsigned/**/short *addr, unsigned/**/short value)
{
  (void)__atomic_or_fetch(addr, value, __ATOMIC_RELEASE);
}
#define MK_AO_HAVE_short_or_release

MK_AO_INLINE void
MK_AO_short_xor_release(volatile unsigned/**/short *addr, unsigned/**/short value)
{
  (void)__atomic_xor_fetch(addr, value, __ATOMIC_RELEASE);
}
#define MK_AO_HAVE_short_xor_release
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
MK_AO_int_fetch_and_add_release(volatile unsigned *addr, unsigned incr)
{
  return __atomic_fetch_add(addr, incr, __ATOMIC_RELEASE);
}
#define MK_AO_HAVE_int_fetch_and_add_release

MK_AO_INLINE void
MK_AO_int_and_release(volatile unsigned *addr, unsigned value)
{
  (void)__atomic_and_fetch(addr, value, __ATOMIC_RELEASE);
}
#define MK_AO_HAVE_int_and_release

MK_AO_INLINE void
MK_AO_int_or_release(volatile unsigned *addr, unsigned value)
{
  (void)__atomic_or_fetch(addr, value, __ATOMIC_RELEASE);
}
#define MK_AO_HAVE_int_or_release

MK_AO_INLINE void
MK_AO_int_xor_release(volatile unsigned *addr, unsigned value)
{
  (void)__atomic_xor_fetch(addr, value, __ATOMIC_RELEASE);
}
#define MK_AO_HAVE_int_xor_release
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
MK_AO_fetch_and_add_release(volatile MK_AO_t *addr, MK_AO_t incr)
{
  return __atomic_fetch_add(addr, incr, __ATOMIC_RELEASE);
}
#define MK_AO_HAVE_fetch_and_add_release

MK_AO_INLINE void
MK_AO_and_release(volatile MK_AO_t *addr, MK_AO_t value)
{
  (void)__atomic_and_fetch(addr, value, __ATOMIC_RELEASE);
}
#define MK_AO_HAVE_and_release

MK_AO_INLINE void
MK_AO_or_release(volatile MK_AO_t *addr, MK_AO_t value)
{
  (void)__atomic_or_fetch(addr, value, __ATOMIC_RELEASE);
}
#define MK_AO_HAVE_or_release

MK_AO_INLINE void
MK_AO_xor_release(volatile MK_AO_t *addr, MK_AO_t value)
{
  (void)__atomic_xor_fetch(addr, value, __ATOMIC_RELEASE);
}
#define MK_AO_HAVE_xor_release
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
MK_AO_char_fetch_and_add_full(volatile unsigned/**/char *addr, unsigned/**/char incr)
{
  return __atomic_fetch_add(addr, incr, __ATOMIC_SEQ_CST);
}
#define MK_AO_HAVE_char_fetch_and_add_full

MK_AO_INLINE void
MK_AO_char_and_full(volatile unsigned/**/char *addr, unsigned/**/char value)
{
  (void)__atomic_and_fetch(addr, value, __ATOMIC_SEQ_CST);
}
#define MK_AO_HAVE_char_and_full

MK_AO_INLINE void
MK_AO_char_or_full(volatile unsigned/**/char *addr, unsigned/**/char value)
{
  (void)__atomic_or_fetch(addr, value, __ATOMIC_SEQ_CST);
}
#define MK_AO_HAVE_char_or_full

MK_AO_INLINE void
MK_AO_char_xor_full(volatile unsigned/**/char *addr, unsigned/**/char value)
{
  (void)__atomic_xor_fetch(addr, value, __ATOMIC_SEQ_CST);
}
#define MK_AO_HAVE_char_xor_full
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
MK_AO_short_fetch_and_add_full(volatile unsigned/**/short *addr, unsigned/**/short incr)
{
  return __atomic_fetch_add(addr, incr, __ATOMIC_SEQ_CST);
}
#define MK_AO_HAVE_short_fetch_and_add_full

MK_AO_INLINE void
MK_AO_short_and_full(volatile unsigned/**/short *addr, unsigned/**/short value)
{
  (void)__atomic_and_fetch(addr, value, __ATOMIC_SEQ_CST);
}
#define MK_AO_HAVE_short_and_full

MK_AO_INLINE void
MK_AO_short_or_full(volatile unsigned/**/short *addr, unsigned/**/short value)
{
  (void)__atomic_or_fetch(addr, value, __ATOMIC_SEQ_CST);
}
#define MK_AO_HAVE_short_or_full

MK_AO_INLINE void
MK_AO_short_xor_full(volatile unsigned/**/short *addr, unsigned/**/short value)
{
  (void)__atomic_xor_fetch(addr, value, __ATOMIC_SEQ_CST);
}
#define MK_AO_HAVE_short_xor_full
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
MK_AO_int_fetch_and_add_full(volatile unsigned *addr, unsigned incr)
{
  return __atomic_fetch_add(addr, incr, __ATOMIC_SEQ_CST);
}
#define MK_AO_HAVE_int_fetch_and_add_full

MK_AO_INLINE void
MK_AO_int_and_full(volatile unsigned *addr, unsigned value)
{
  (void)__atomic_and_fetch(addr, value, __ATOMIC_SEQ_CST);
}
#define MK_AO_HAVE_int_and_full

MK_AO_INLINE void
MK_AO_int_or_full(volatile unsigned *addr, unsigned value)
{
  (void)__atomic_or_fetch(addr, value, __ATOMIC_SEQ_CST);
}
#define MK_AO_HAVE_int_or_full

MK_AO_INLINE void
MK_AO_int_xor_full(volatile unsigned *addr, unsigned value)
{
  (void)__atomic_xor_fetch(addr, value, __ATOMIC_SEQ_CST);
}
#define MK_AO_HAVE_int_xor_full
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
MK_AO_fetch_and_add_full(volatile MK_AO_t *addr, MK_AO_t incr)
{
  return __atomic_fetch_add(addr, incr, __ATOMIC_SEQ_CST);
}
#define MK_AO_HAVE_fetch_and_add_full

MK_AO_INLINE void
MK_AO_and_full(volatile MK_AO_t *addr, MK_AO_t value)
{
  (void)__atomic_and_fetch(addr, value, __ATOMIC_SEQ_CST);
}
#define MK_AO_HAVE_and_full

MK_AO_INLINE void
MK_AO_or_full(volatile MK_AO_t *addr, MK_AO_t value)
{
  (void)__atomic_or_fetch(addr, value, __ATOMIC_SEQ_CST);
}
#define MK_AO_HAVE_or_full

MK_AO_INLINE void
MK_AO_xor_full(volatile MK_AO_t *addr, MK_AO_t value)
{
  (void)__atomic_xor_fetch(addr, value, __ATOMIC_SEQ_CST);
}
#define MK_AO_HAVE_xor_full
