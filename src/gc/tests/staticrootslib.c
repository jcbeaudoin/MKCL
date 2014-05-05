
/* This test file is intended to be compiled into a DLL. */

#ifndef MK_GC_DEBUG
# define MK_GC_DEBUG
#endif

#include "gc.h"

#ifndef MK_GC_TEST_EXPORT_API
# if defined(MK_GC_VISIBILITY_HIDDEN_SET) \
     && !defined(__CEGCC__) && !defined(__CYGWIN__) && !defined(__MINGW32__)
#   define MK_GC_TEST_EXPORT_API \
                        extern __attribute__((__visibility__("default")))
# else
#   define MK_GC_TEST_EXPORT_API extern
# endif
#endif

struct treenode {
    struct treenode *x;
    struct treenode *y;
};

static struct treenode *root[10] = { 0 };
static struct treenode *root_nz[10] = { (void *)(MK_GC_word)2 };

#ifdef STATICROOTSLIB2
# define libsrl_getpelem libsrl_getpelem2
#else

  MK_GC_TEST_EXPORT_API struct treenode * libsrl_mktree(int i)
  {
    struct treenode * r = MK_GC_MALLOC(sizeof(struct treenode));
    if (0 == i) return 0;
    if (1 == i) r = MK_GC_MALLOC_ATOMIC(sizeof(struct treenode));
    if (r) {
      r -> x = libsrl_mktree(i-1);
      r -> y = libsrl_mktree(i-1);
    }
    return r;
  }

  MK_GC_TEST_EXPORT_API void * libsrl_init(void)
  {
#   ifndef STATICROOTSLIB_INIT_IN_MAIN
      MK_GC_INIT();
#   endif
    return MK_GC_MALLOC(sizeof(struct treenode));
  }

#endif /* !STATICROOTSLIB2 */

MK_GC_TEST_EXPORT_API struct treenode ** libsrl_getpelem(int i, int j)
{
  return &((j & 1) != 0 ? root_nz : root)[i];
}
