
/* This test file is intended to be compiled into a DLL. */

#include <stdio.h>

#ifndef MK_GC_DEBUG
# define MK_GC_DEBUG
#endif

#include "gc.h"

struct treenode {
    struct treenode *x;
    struct treenode *y;
} * root[10];

struct treenode * libsrl_mktree(int i)
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

void * libsrl_init(void)
{
  MK_GC_INIT();
  return MK_GC_MALLOC(sizeof(struct treenode));
}
