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

/* This should never be included directly; it is included only from gc.h. */
#if defined(MK_GC_H)

/* The version here should match that in configure/configure.ac */
/* Eventually this one may become unnecessary.  For now we need */
/* it to keep the old-style build process working.              */
#define MK_GC_TMP_VERSION_MAJOR 7
#define MK_GC_TMP_VERSION_MINOR 2
#define MK_GC_TMP_ALPHA_VERSION MK_GC_NOT_ALPHA

#ifndef MK_GC_NOT_ALPHA
# define MK_GC_NOT_ALPHA 0xff
#endif

#ifdef MK_GC_VERSION_MAJOR
# if MK_GC_TMP_VERSION_MAJOR != MK_GC_VERSION_MAJOR \
     || MK_GC_TMP_VERSION_MINOR != MK_GC_VERSION_MINOR \
     || defined(MK_GC_ALPHA_VERSION) != (MK_GC_TMP_ALPHA_VERSION != MK_GC_NOT_ALPHA) \
     || (defined(MK_GC_ALPHA_VERSION) && MK_GC_TMP_ALPHA_VERSION != MK_GC_ALPHA_VERSION)
#   error Inconsistent version info.  Check doc/README, include/gc_version.h, and configure.ac.
# endif
#else
# define MK_GC_VERSION_MAJOR MK_GC_TMP_VERSION_MAJOR
# define MK_GC_VERSION_MINOR MK_GC_TMP_VERSION_MINOR
# define MK_GC_ALPHA_VERSION MK_GC_TMP_ALPHA_VERSION
#endif /* !MK_GC_VERSION_MAJOR */

#endif
