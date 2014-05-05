/*
 * Test at the boundary between small and large objects.
 * Inspired by a test case from Zoltan Varga.
 */
#include "gc.h"
#include <stdio.h>

int main (void)
{
  int i;

  MK_GC_set_all_interior_pointers(0);
  MK_GC_INIT();

  for (i = 0; i < 20000; ++i) {
    (void)MK_GC_malloc_atomic(4096);
    (void)MK_GC_malloc(4096);
  }
  for (i = 0; i < 20000; ++i) {
    (void)MK_GC_malloc_atomic(2048);
    (void)MK_GC_malloc(2048);
  }
  printf("Final heap size is %lu\n", (unsigned long)MK_GC_get_heap_size());
  return 0;
}
