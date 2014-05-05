/* FIXME.  This is only a placeholder for the AIX compiler.             */
/* It doesn't work.  Please send a patch.                               */
/* Memory model documented at http://www-106.ibm.com/developerworks/    */
/* eserver/articles/archguide.html and (clearer)                        */
/* http://www-106.ibm.com/developerworks/eserver/articles/powerpc.html. */
/* There appears to be no implicit ordering between any kind of         */
/* independent memory references.                                       */
/* Architecture enforces some ordering based on control dependence.     */
/* I don't know if that could help.                                     */
/* Data-dependent loads are always ordered.                             */
/* Based on the above references, eieio is intended for use on          */
/* uncached memory, which we don't support.  It does not order loads    */
/* from cached memory.                                                  */
/* Thanks to Maged Michael, Doug Lea, and Roger Hoover for helping to   */
/* track some of this down and correcting my misunderstandings. -HB     */

#include "../all_aligned_atomic_load_store.h"

void MK_AO_sync(void);
#pragma mc_func MK_AO_sync { "7c0004ac" }

#ifdef __NO_LWSYNC__
# define MK_AO_lwsync MK_AO_sync
#else
  void MK_AO_lwsync(void);
#pragma mc_func MK_AO_lwsync { "7c2004ac" }
#endif

#define MK_AO_nop_write() MK_AO_lwsync()
#define MK_AO_HAVE_nop_write

#define MK_AO_nop_read() MK_AO_lwsync()
#define MK_AO_HAVE_nop_read

/* We explicitly specify load_acquire and store_release, since these    */
/* rely on the fact that lwsync is also a LoadStore barrier.            */
MK_AO_INLINE MK_AO_t
MK_AO_load_acquire(const volatile MK_AO_t *addr)
{
  MK_AO_t result = *addr;
  MK_AO_lwsync();
  return result;
}
#define MK_AO_HAVE_load_acquire

MK_AO_INLINE void
MK_AO_store_release(volatile MK_AO_t *addr, MK_AO_t value)
{
  MK_AO_lwsync();
  *addr = value;
}
#define MK_AO_HAVE_store_release

#ifndef MK_AO_PREFER_GENERALIZED
/* This is similar to the code in the garbage collector.  Deleting      */
/* this and having it synthesized from compare_and_swap would probably  */
/* only cost us a load immediate instruction.                           */
/*MK_AO_INLINE MK_AO_TS_VAL_t
MK_AO_test_and_set(volatile MK_AO_TS_t *addr) {
# error FIXME Implement me
}
#define MK_AO_HAVE_test_and_set*/

MK_AO_INLINE MK_AO_TS_VAL_t
MK_AO_test_and_set_acquire(volatile MK_AO_TS_t *addr) {
  MK_AO_TS_VAL_t result = MK_AO_test_and_set(addr);
  MK_AO_lwsync();
  return result;
}
#define MK_AO_HAVE_test_and_set_acquire

MK_AO_INLINE MK_AO_TS_VAL_t
MK_AO_test_and_set_release(volatile MK_AO_TS_t *addr) {
  MK_AO_lwsync();
  return MK_AO_test_and_set(addr);
}
#define MK_AO_HAVE_test_and_set_release

MK_AO_INLINE MK_AO_TS_VAL_t
MK_AO_test_and_set_full(volatile MK_AO_TS_t *addr) {
  MK_AO_TS_VAL_t result;
  MK_AO_lwsync();
  result = MK_AO_test_and_set(addr);
  MK_AO_lwsync();
  return result;
}
#define MK_AO_HAVE_test_and_set_full
#endif /* !MK_AO_PREFER_GENERALIZED */

/*MK_AO_INLINE MK_AO_t
MK_AO_fetch_compare_and_swap(volatile MK_AO_t *addr, MK_AO_t old_val, MK_AO_t new_val)
{
# error FIXME Implement me
}
#define MK_AO_HAVE_fetch_compare_and_swap*/

MK_AO_INLINE MK_AO_t
MK_AO_fetch_compare_and_swap_acquire(volatile MK_AO_t *addr, MK_AO_t old_val,
                                  MK_AO_t new_val)
{
  MK_AO_t result = MK_AO_fetch_compare_and_swap(addr, old_val, new_val);
  MK_AO_lwsync();
  return result;
}
#define MK_AO_HAVE_fetch_compare_and_swap_acquire

MK_AO_INLINE MK_AO_t
MK_AO_fetch_compare_and_swap_release(volatile MK_AO_t *addr, MK_AO_t old_val,
                                  MK_AO_t new_val)
{
  MK_AO_lwsync();
  return MK_AO_fetch_compare_and_swap(addr, old_val, new_val);
}
#define MK_AO_HAVE_fetch_compare_and_swap_release

MK_AO_INLINE MK_AO_t
MK_AO_fetch_compare_and_swap_full(volatile MK_AO_t *addr, MK_AO_t old_val,
                               MK_AO_t new_val)
{
  MK_AO_t result;
  MK_AO_lwsync();
  result = MK_AO_fetch_compare_and_swap(addr, old_val, new_val);
  MK_AO_lwsync();
  return result;
}
#define MK_AO_HAVE_fetch_compare_and_swap_full

/* TODO: Implement MK_AO_fetch_and_add, MK_AO_and/or/xor directly.    */
