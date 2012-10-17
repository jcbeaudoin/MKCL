
#ifdef HAVE_CONFIG_H
# include "private/config.h"
#endif

#ifndef MK_GC_THREADS
# define MK_GC_THREADS
#endif

#define MK_GC_NO_THREAD_REDIRECTS 1

#include "gc.h"

#if (!defined(MK_GC_PTHREADS) || defined(MK_GC_SOLARIS_THREADS) \
     || defined(__native_client__)) && !defined(SKIP_THREADKEY_TEST)
  /* FIXME: Skip this test on Solaris for now.  The test may fail on    */
  /* other targets as well.  Currently, tested only on Linux, Cygwin    */
  /* and Darwin.                                                        */
# define SKIP_THREADKEY_TEST
#endif

#ifdef SKIP_THREADKEY_TEST

#include <stdio.h>

int main (void)
{
  printf("threadkey_test skipped\n");
  return 0;
}

#else

#include <pthread.h>

pthread_key_t key;

#ifdef MK_GC_SOLARIS_THREADS
  /* pthread_once_t key_once = { PTHREAD_ONCE_INIT }; */
#else
  pthread_once_t key_once = PTHREAD_ONCE_INIT;
#endif

void * entry (void *arg)
{
  pthread_setspecific(key,
                      (void *)MK_GC_HIDE_POINTER(MK_GC_STRDUP("hello, world")));
  return arg;
}

void * MK_GC_CALLBACK on_thread_exit_inner (struct MK_GC_stack_base * sb, void * arg)
{
  int res = MK_GC_register_my_thread (sb);
  pthread_t t;
  int creation_res;     /* Used to suppress a warning about     */
                        /* unchecked pthread_create() result.   */

  creation_res = MK_GC_pthread_create (&t, NULL, entry, NULL);
  if (res == MK_GC_SUCCESS)
    MK_GC_unregister_my_thread ();

  return (void*)(MK_GC_word)creation_res;
}

void on_thread_exit (void *v)
{
  MK_GC_call_with_stack_base (on_thread_exit_inner, NULL);
}

void make_key (void)
{
  pthread_key_create (&key, on_thread_exit);
}

#ifndef LIMIT
# define LIMIT 30
#endif

int main (void)
{
  int i;
  MK_GC_INIT ();

# ifdef MK_GC_SOLARIS_THREADS
    pthread_key_create (&key, on_thread_exit);
# else
    pthread_once (&key_once, make_key);
# endif
  for (i = 0; i < LIMIT; i++) {
    pthread_t t;
    void *res;
    if (MK_GC_pthread_create (&t, NULL, entry, NULL) == 0
        && (i & 1) != 0)
      MK_GC_pthread_join (t, &res);
  }
  return 0;
}

#endif /* !SKIP_THREADKEY_TEST */
