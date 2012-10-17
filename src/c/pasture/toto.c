
#include <stdio.h>
#include <pthread.h>
#include <errno.h>

pthread_mutex_t mutex;


void * foo (void * arg)
{  
  int rc = 0;

  printf("doing lock in foo.\n");
  if (rc = pthread_mutex_lock(&mutex))
    printf("In foo, pthread_mutex_lock failed! errno = %d, rc = %d\n", errno, rc);

  return (void *) 0xdeadbeef;
}

void * bar (void * arg)
{
  int rc = 0;

  sleep(10);

  printf("doing unlock in bar.\n");
  if (rc = pthread_mutex_unlock(&mutex))
    printf("pthread_mutex_unlock failed! errno = %d, rc = %d\n", errno, rc);

  return (void *) 2;
}

int main(int argc, char *argv[])
{
  int i, rc = 0;
  pthread_t t1, t2;
  void * t1_val, * t2_val;

  pthread_mutexattr_t mutexattr;
/*   pthread_mutex_t mutex; */

  int attr_type;

  pthread_mutexattr_init(&mutexattr);

  pthread_mutexattr_gettype(&mutexattr, &attr_type);

/*   pthread_mutexattr_settype(&mutexattr, PTHREAD_MUTEX_RECURSIVE); */
  pthread_mutexattr_settype(&mutexattr, PTHREAD_MUTEX_ERRORCHECK);

  printf("attr_type = %d\n", attr_type);

  pthread_mutex_init(&mutex, &mutexattr);

  pthread_create(&t1, NULL, foo, NULL);
  pthread_create(&t2, NULL, bar, NULL);

  pthread_join(t1, &t1_val);

  printf("joined with t1, t1_val = %p\n", t1_val);

  for (i = 0; i < 5; i++)
    {
      printf("doing lock.\n");
      if (rc = pthread_mutex_lock(&mutex))
	printf("pthread_mutex_lock failed! errno = %d, rc = %d\n", errno, rc);
    }

  for (i = 0; i < 10; i++)
    {
      printf("doing unlock.\n");
      if (rc = pthread_mutex_unlock(&mutex))
	printf("pthread_mutex_unlock failed! errno = %d, rc = %d\n", errno, rc);
    }
 
  fflush(NULL);
  return 0;
}
