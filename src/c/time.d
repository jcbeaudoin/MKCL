/* -*- mode: c -*- */
/*
    time.c -- Time routines.
*/
/*
    Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
    Copyright (c) 1990, Giuseppe Attardi.
    Copyright (c) 2001, Juan Jose Garcia Ripoll.
    Copyright (c) 2010-2016, Jean-Claude Beaudoin.

    MKCL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 3 of the License, or (at your option) any later version.

    See file '../../Copyright' for full details.
*/

#include <mkcl/mkcl.h>
#include <mkcl/mkcl-math.h>
#include <mkcl/internal.h>

#include <time.h>
#include <errno.h>
#ifdef HAVE_TIMES
# include <sys/times.h>
#endif
#ifdef HAVE_GETRUSAGE
# include <sys/time.h>
# include <sys/resource.h>
#endif
#ifdef HAVE_GETTIMEOFDAY
# include <sys/time.h>
#endif

#include <stdlib.h>

#ifdef darwin
#undef HAVE_NANOSLEEP
#endif

#if MKCL_WINDOWS && !defined(__MINGW64_VERSION_MAJOR)
struct timespec {
  time_t tv_sec;
  long tv_nsec;
};
#endif

static struct timespec beginning;

static void
get_real_time(MKCL, struct timespec *ts)
{
#if MKCL_UNIX
  int rc;

  MKCL_LIBC_NO_INTR(env, rc = clock_gettime(CLOCK_REALTIME, ts));
  if (rc)
    mkcl_FElibc_error(env, "get_real_time() failed on clock_gettime()", 0);

#elif MKCL_WINDOWS
  /* This version will not wrap-around before year 30827 and has 100 nanoseconds resolution! */
  FILETIME system_time;
  ULARGE_INTEGER uli_system_time;

  MKCL_LIBC_NO_INTR(env, GetSystemTimeAsFileTime(&system_time));
  uli_system_time.LowPart = system_time.dwLowDateTime;
  uli_system_time.HighPart = system_time.dwHighDateTime;
#if 0 /* There seems to be a bug in GCC 4.7.0 on MinGW that mis-inlines lldiv! */
  {
    lldiv_t sec_and_100nsec = lldiv(uli_system_time.QuadPart, 1000 * 1000 * 10);

    ts->tv_sec = sec_and_100nsec.quot;
    ts->tv_nsec = sec_and_100nsec.rem * 100;
  }
#else
  {
    time_t sec = uli_system_time.QuadPart / ((long long) (1000 * 1000 * 10));
    long nsec = 100 * (uli_system_time.QuadPart - (((long long) sec) * ((long long) 1000 * 1000 * 10)));

    ts->tv_sec = sec;
    ts->tv_nsec = nsec;
  }
#endif
#elif defined(HAVE_GETTIMEOFDAY)
  struct timezone tz;
  gettimeofday(ts, &tz);
  ts->tv_nsec = ts->tv_nsec * 1000;
#else
  time_t t = time(0);
  ts->tv_sec = t;
  ts->tv_usec = 0;
#endif
}

static void
get_run_time(MKCL, struct timespec *ts)
{
#if __linux
  int rc;

  MKCL_LIBC_NO_INTR(env, rc = clock_gettime(CLOCK_THREAD_CPUTIME_ID, ts));
  if (rc)
    mkcl_FElibc_error(env, "get_run_time() failed on clock_gettime()", 0);

#elif MKCL_WINDOWS
  FILETIME creation_time;
  FILETIME exit_time;
  FILETIME kernel_time;
  FILETIME user_time;
  ULARGE_INTEGER uli_kernel_time;
  ULARGE_INTEGER uli_user_time;
  BOOL ok;

  MKCL_LIBC_NO_INTR(env, ok = GetThreadTimes(GetCurrentThread(), &creation_time, &exit_time, &kernel_time, &user_time));
  if (!ok)
    mkcl_FEwin32_error(MKCL_ENV(), "GetThreadTimes() failed", 0);
  uli_kernel_time.HighPart = kernel_time.dwHighDateTime;
  uli_kernel_time.LowPart  = kernel_time.dwLowDateTime;
  uli_user_time.HighPart = user_time.dwHighDateTime;
  uli_user_time.LowPart  = user_time.dwLowDateTime;
  uli_kernel_time.QuadPart += uli_user_time.QuadPart;
#if 0 /* There seems to be a bug in GCC 4.7.0 on MinGW that mis-inlines lldiv! */
  {
    lldiv_t sec_and_100nsec = lldiv(uli_kernel_time.QuadPart, 1000 * 1000 * 10);

    ts->tv_sec = sec_and_100nsec.quot;
    ts->tv_nsec = sec_and_100nsec.rem * 100;
  }
#else
  {
    time_t sec = uli_kernel_time.QuadPart / ((long long) (1000 * 1000 * 10));
    long nsec = 100 * (uli_kernel_time.QuadPart - (((long long) sec) * ((long long) 1000 * 1000 * 10)));

    ts->tv_sec = sec;
    ts->tv_nsec = nsec;
  }
#endif
#elif defined(HAVE_GETRUSAGE)
  struct rusage r;
  getrusage(RUSAGE_SELF, &r);
  ts->tv_sec = r.ru_utime.tv_sec;
  ts->tv_nsec = r.ru_utime.tv_usec * 1000;
#elif defined(HAVE_TIMES)
  struct tms buf;
  times(&buf);
  ts->tv_sec = buf.tms_utime / CLK_TCK;
  ts->tv_nsec = (buf.tms_utime % CLK_TCK) * (1000 * 1000 * 1000);
#else
  get_real_time(ts);
#endif
}

mkcl_object
mk_cl_sleep(MKCL, mkcl_object z)
{
  mkcl_call_stack_check(env);
  bool fe_inexact_on = FE_INEXACT & fegetexcept();

  /* INV: mkcl_minusp() makes sure `z' is real */
  if (mkcl_minusp(env, z))
    mk_cl_error(env, 9, @'simple-type-error', @':format-control',
                mkcl_make_simple_base_string(env, "Not a non-negative number ~S"),
                @':format-arguments', mk_cl_list(env, 1, z),
                @':expected-type', @'real', @':datum', z);

  if (fe_inexact_on)
    fedisableexcept(FE_INEXACT);

#ifdef HAVE_NANOSLEEP
  {
    struct timespec tm;
    double r = mkcl_to_double(env, z);
    tm.tv_sec = (time_t)floor(r);
    tm.tv_nsec = (long)((r - floor(r)) * 1e9);

    if (fe_inexact_on)
      {
	feclearexcept(FE_INEXACT);
	feenableexcept(FE_INEXACT);
      }

    {
      int rc;
      struct timespec rem = tm;
      do
	{
	  struct timespec req = rem;
	  MKCL_LIBC_Zzz(env, @':io', rc = nanosleep(&req, &rem)); /* We should use clock_nanosleep() instead. JCB */
	}
      while (rc && errno == EINTR);
      mk_mt_test_for_thread_shutdown(env);
      if (rc)
	mkcl_FElibc_error(env, "mk_cl_sleep() failed on nanosleep().", 0);
    }
  }
#elif MKCL_WINDOWS
  {
    double r = mkcl_to_double(env, z) * 1000;
#if 0
    DWORD duration = (DWORD) r; /* This limits sleep duration to about 49,7 days */

    if (fe_inexact_on)
      {
	feclearexcept(FE_INEXACT);
	feenableexcept(FE_INEXACT);
      }
#endif

#if 0
    Sleep(duration); /* This one sleeps hard and cannot be interrupted. JCB */
#else
    {
      long long now_ms;
      long long target_ms;
      FILETIME system_time;
      ULARGE_INTEGER uli_system_time; /* in multiples of 100 nanoseconds. */
      DWORD val;

      MKCL_LIBC_NO_INTR(env, GetSystemTimeAsFileTime(&system_time));
      uli_system_time.LowPart = system_time.dwLowDateTime;
      uli_system_time.HighPart = system_time.dwHighDateTime;

      now_ms = uli_system_time.QuadPart / 10000;
#if 0
      target_ms = now_ms + llround(r);
#else
      target_ms = now_ms + ((long long) round(r)); /* used to be llround() but mingw with gcc 4.8.1 foobared on it. */
#endif

      if (fe_inexact_on)
	{
	  feclearexcept(FE_INEXACT);
	  feenableexcept(FE_INEXACT);
	}

      do {
	DWORD duration;

	if (now_ms >= target_ms)
	  duration = 0;
	if ((target_ms - now_ms) > MAXDWORD)
	  duration = MAXDWORD;
	else
	  duration = target_ms - now_ms;

#if 0
	fprintf(stderr, "\nMKCL: sleep duration = %lu ms.\n", duration); fflush(stderr);
#endif

	MKCL_LIBC_Zzz(env, @':io', val = SleepEx(duration, TRUE)); /* The "alertable" version. */

	MKCL_LIBC_NO_INTR(env, GetSystemTimeAsFileTime(&system_time));
	uli_system_time.LowPart = system_time.dwLowDateTime;
	uli_system_time.HighPart = system_time.dwHighDateTime;
	
	now_ms = uli_system_time.QuadPart / 10000;
	
      } while ((val == WAIT_IO_COMPLETION) && (now_ms < target_ms));
      mk_mt_test_for_thread_shutdown(env);
    }
#endif
  }
#else
  z = mkcl_round1(z);
  if (fe_inexact_on)
    {
      feclearexcept(FE_INEXACT);
      feenableexcept(FE_INEXACT);
    }
  if (MKCL_FIXNUMP(z))
    sleep(mkcl_fixnum_to_word(z)); /* FIXME! sleep() takes an "unsigned int", beware of truncation here. JCB */
#endif
  @(return mk_cl_Cnil);
}

static mkcl_object
timespec_to_time(MKCL, time_t sec, long nsec)
{
#if MKCL_WORD_BITS < 64
  if (sec < (MKCL_MOST_POSITIVE_FIXNUM / (1000 * 1000)))
    {
      mkcl_word x = (sec * (1000 * 1000)) + (nsec / 1000);
      return MKCL_MAKE_FIXNUM(x);
    }
  else
    {
      mkcl_object time = mkcl_make_integer(env, sec);

      time = mkcl_times(env, time , MKCL_MAKE_FIXNUM(1000 * 1000));
      time = mkcl_plus(env, time, MKCL_MAKE_FIXNUM(nsec / 1000));

      return time;
    }
#else
  if ( sec < (MKCL_MOST_POSITIVE_FIXNUM / (1000 * 1000 * 1000)))
    {
      mkcl_word x = (sec * (1000 * 1000 * 1000)) + nsec;
      return MKCL_MAKE_FIXNUM((mkcl_word)x);
    }
  else
    {
      mkcl_object time = mkcl_make_integer(env, sec);

      time = mkcl_times(env, time , MKCL_MAKE_FIXNUM(1000 * 1000 * 1000));
      time = mkcl_plus(env, time, MKCL_MAKE_FIXNUM(nsec));

      return time;
    }    
#endif
}

mkcl_object
mk_cl_get_internal_run_time(MKCL) /* Should take a "thread" as optional argument. JCB */
{
  struct timespec ts;
  get_run_time(env, &ts);
  @(return timespec_to_time(env, ts.tv_sec, ts.tv_nsec));
}

mkcl_object
mk_cl_get_internal_real_time(MKCL)
{
  struct timespec ts;

  mkcl_call_stack_check(env);
  get_real_time(env, &ts);
  @(return timespec_to_time(env, ts.tv_sec - beginning.tv_sec, ts.tv_nsec - beginning.tv_nsec));
}

mkcl_object
mk_cl_get_universal_time(MKCL)
{ /* On 32 bits POSIX machines this code will fail around January 18, 2038
     because of the value returned by time() will have wrapped-around. JCB */
  mkcl_call_stack_check(env);
  mkcl_object utc = mkcl_make_integer(env, time(0));
  @(return mkcl_plus(env, utc, mkcl_core.Jan1st1970UT));
}


mkcl_object
mk_si_get_local_time_zone(MKCL)
{
  int zone_bias_in_minutes;
  
  mkcl_call_stack_check(env);
  {
#if MKCL_WINDOWS
    TIME_ZONE_INFORMATION tzi;
    DWORD dwRet;
 
    MKCL_LIBC_NO_INTR(env, dwRet = GetTimeZoneInformation(&tzi));

    if (dwRet == TIME_ZONE_ID_INVALID)
      mkcl_FEwin32_error(env, "GetTimeZoneInformation() failed", 0);

    zone_bias_in_minutes = tzi.Bias;
#else
    struct tm ltm, gtm;
    const time_t when = 0L;
    void * ok_p = NULL;

    MKCL_LIBC_NO_INTR(env, ok_p = localtime_r(&when, &ltm));
    if (ok_p == NULL)
      mkcl_FElibc_error(env, "mk_si_get_local_time_zone() failed on localtime_r()", 0);
    MKCL_LIBC_NO_INTR(env, ok_p = gmtime_r(&when, &gtm));
    if (ok_p == NULL)
      mkcl_FElibc_error(env, "mk_si_get_local_time_zone() failed on gmtime_r()", 0);

    zone_bias_in_minutes = (gtm.tm_min + 60 * gtm.tm_hour) - (ltm.tm_min + 60 * ltm.tm_hour);

    if ((gtm.tm_wday + 1) % 7 == ltm.tm_wday)
      zone_bias_in_minutes -= 24*60;
    else if (gtm.tm_wday == (ltm.tm_wday + 1) % 7)
      zone_bias_in_minutes += 24*60;
#endif
  }
  @(return mkcl_make_ratio(env, MKCL_MAKE_FIXNUM(zone_bias_in_minutes), MKCL_MAKE_FIXNUM(60)));
}


void
mkcl_init_unixtime(MKCL)
{
  get_real_time(env, &beginning);
  
#if MKCL_WORD_BITS < 64
  MKCL_SET(@'internal-time-units-per-second', MKCL_MAKE_FIXNUM(1000 * 1000)); /* microseconds */
#else
  MKCL_SET(@'internal-time-units-per-second', MKCL_MAKE_FIXNUM(1000 * 1000 * 1000)); /* nanoseconds */
#endif
  
  /* This is the number of seconds between 00:00:00 January 1st, 1900 GMT and 00:00:00 January 1st, 1970 GMT. */
  mkcl_core.Jan1st1970UT = mkcl_times(env, MKCL_MAKE_FIXNUM(24 * 60 * 60), MKCL_MAKE_FIXNUM(17 + 365 * 70));
}
