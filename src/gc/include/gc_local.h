
/*
 * Copyright (c) 2012, Jean-Claude Beaudoin
 *
 * This file has been added to the original code of Boehm's GC to give access to
 * a facility to configure the suspend/restart signals used to
 * stop/resume the world on Linux.
 * A pair of callback hooks to intercept calls to exit() or abort()
 * has also been declared here.
 * Both these improvements have been submitted for inclusion
 * in the development branch of the GC.
 * 
 */

typedef void (MK_GC_CALLBACK * MK_GC_exit_func)(const int status);

void MK_GC_set_exit_func(MK_GC_exit_func fn);

typedef void (MK_GC_CALLBACK * MK_GC_abort_func)(const char * const msg);

void MK_GC_set_abort_func(MK_GC_abort_func fn);

#if defined(MK_GC_LINUX_THREADS)
void MK_GC_set_suspend_signal(const int sig);
void MK_GC_set_thread_restart_signal(const int sig);

int MK_GC_suspend_signal(void);
int MK_GC_thread_restart_signal(void);
#endif

