dnl -*- autoconf -*-

dnl --------------------------------------------------------------
dnl http://autoconf-archive.cryp.to/ac_c_long_long_.html
dnl Provides a test for the existance of the long long int type and defines HAVE_LONG_LONG if it is found.
AC_DEFUN([AC_C_LONG_LONG],
[AC_MSG_CHECKING(size of long long)
if test "$GCC" = yes; then
  ac_cv_c_long_long=yes
else
  AC_TRY_COMPILE(,[long long int i;],
  ac_cv_c_long_long=yes,
  ac_cv_c_long_long=no)
fi
if test $ac_cv_c_long_long = yes; then
  AC_RUN_IFELSE([AC_LANG_SOURCE([[#include <stdio.h>
#include <stdlib.h>
int main() {
  const char *int_type;
  int bits;
  unsigned long long x = 1;
  FILE *f=fopen("conftestval", "w");
  if (!f) exit(1);
  for (bits = 0; x; bits++) {
    x <<= 1;
  }
  fprintf(f,"MKCL_LONG_LONG_BITS='%d'",bits);
  exit(0);
}]])],[eval "`cat conftestval`"],[],[])
fi
if test -z "$MKCL_LONG_LONG_BITS"; then
  AC_MSG_RESULT(not available)
else
  AC_MSG_RESULT([$MKCL_LONG_LONG_BITS])
  AC_DEFINE(mkcl_long_long_t, long long, [compiler understands long long])
  AC_DEFINE(mkcl_ulong_long_t, unsigned long long, [compiler understands long long])
  AC_DEFINE_UNQUOTED([MKCL_LONG_LONG_BITS],[$MKCL_LONG_LONG_BITS])
fi
])

dnl --------------------------------------------------------------
dnl Add *feature* for conditional compilation.
AC_DEFUN([MKCL_ADD_FEATURE], [
dnl LSP_FEATURES="(cons :$1 ${LSP_FEATURES})"
LSP_FEATURES=":$1 ${LSP_FEATURES}"
])

dnl --------------------------------------------------------------
dnl Add lisp module to compile; if second argument is given,
dnl compile module into Lisp library if we don't support shared
dnl libraries.
dnl
AC_DEFUN([MKCL_ADD_LISP_MODULE], [
  MKCL_ADD_FEATURE([wants-$1])
])

dnl --------------------------------------------------------------
dnl Add lisp module and build it into the compiler.
dnl
AC_DEFUN([MKCL_ADD_BUILTIN_MODULE], [
  MKCL_ADD_FEATURE([builtin-$1])
])

dnl --------------------------------------------------------------
dnl Set up a configuration file for the case when we are cross-
dnl compiling
dnl
AC_DEFUN(MKCL_CROSS_CONFIG,[
if test "x${cross_compiling}" = "xyes"; then
  if test -n "${with_cross_config}" -a -f "${with_cross_config}"; then
    . ${with_cross_config}
  elif test -f ./cross_config; then
    . ./cross_config
  elif test -n "${srcdir}" -a -f ${srcdir}/cross_config; then
    . ${srcdir}/cross_config
  else
    test -z ${with_cross_config} && cross_config=`pwd`/cross_config
    cat > ${with_cross_config} <<EOF
###
### YOU ARE TRYING TO CROSS COMPILE MKCL.
### PLEASE FOLLOW THESE INSTRUCTIONS:
###
### 1) Vital information cannot be determined at configuration time
### because we are not able to run test programs. A file called
###		${cross_config}
### has been created, that you will have to fill out. Please do
### it before invoking "configure" again.

### 1.1) Direction of growth of the stack
MKCL_STACK_DIR=up

### 1.2) Choose an integer datatype which is large enough to host a pointer
CL_FIXNUM_TYPE=int
CL_FIXNUM_BITS=32
CL_FIXNUM_MAX=536870911L
CL_FIXNUM_MIN=-536870912L

### 1.3) Order of bytes within a word
MKCL_BIGENDIAN=no

### 1.4) What characters signal an end of line. May be LF (Linefeed or \\n)
###      CR (Carriage return or \\r), and CRLF (CR followed by LF).
MKCL_NEWLINE=LF

### 1.5) Can we guess how many characters are available for reading from
###      the FILE structure?
###          0 = no
###          1 = (f)->_IO_read_end - (f)->_IO_read_ptr
###          2 = (f)->_r
###          3 = (f)->_cnt
MKCL_FILE_CNT=0

### 2) To cross-compile MKCL so that it runs on the system
###		${host}
### you need to first compile MKCL on the system in which you are building
### the cross-compiled files, that is
###		${build}
### By default we assume that MKCL can be accessed from some directory in
### the path.
MKCL_TO_RUN=`which mkcl`
EOF
    cat ${with_cross_config}
    AC_MSG_ERROR(Configuration aborted)
  fi
  if test "${MKCL_TO_RUN}" = "failed"; then
    AC_MSG_ERROR(The program MKCL is not installed in your system)
  fi
  MKCL_MIN_TO_RUN=`${MKCL_TO_RUN} -eval '(progn (print (truename "sys:mkcl_min")) (si:quit))' \
	| grep '\#\P' | sed 's,#P"\(.*\)",\1,'`
  if test -z "${MKCL_MIN_TO_RUN}" -o "${MKCL_MIN_TO_RUN}" = "failed"  ; then
    AC_MSG_ERROR(The program MKCL-MIN is not installed in your system)
  fi
  DPP_TO_RUN=`${MKCL_TO_RUN} -eval '(progn (print (truename "sys:dpp")) (si:quit))' \
	| grep '\#\P' | sed 's,#P"\(.*\)",\1,'`
  if test -z "${DPP_TO_RUN}" -o "${DPP_TO_RUN}" = "failed"  ; then
    AC_MSG_ERROR(The program DPP is not installed in your system)
  fi
  dnl (echo '#!/bin/sh'; echo exec ${MKCL_TO_RUN} -eval "'"'(push :cross *features*)'"'" '$''*') > CROSS-COMPILER
  (echo '#!/bin/sh'; echo exec ${MKCL_MIN_TO_RUN} '$''*') > CROSS-COMPILER
  (echo '#!/bin/sh'; echo exec ${DPP_TO_RUN} '$''*') > CROSS-DPP
  chmod +x CROSS-COMPILER CROSS-DPP
  MKCL_ADD_FEATURE([cross])
fi
])

dnl --------------------------------------------------------------
dnl Make srcdir absolute, if it isn't already.  It's important to
dnl avoid running the path through pwd unnecessarily, since pwd can
dnl give you automounter prefixes, which can go away.
dnl
AC_DEFUN(MKCL_MAKE_ABSOLUTE_SRCDIR,[
AC_SUBST(true_srcdir)
AC_SUBST(true_builddir)
PWDCMD="pwd";
case "${srcdir}" in
  /* | ?:/* ) ;;
  *  ) srcdir="`(cd ${srcdir}; ${PWDCMD})`";
esac
if uname -a | grep -i 'mingw32' > /dev/null; then
  true_srcdir=`(cd ${srcdir}; pwd -W)`
  true_builddir=`pwd -W`
else
  true_srcdir=`(cd ${srcdir}; pwd)`
  true_builddir=`pwd`
fi
])

dnl
dnl --------------------------------------------------------------
dnl Define a name for this operating system and set some defaults
dnl
AC_DEFUN(MKCL_GUESS_HOST_OS,[
#### Some command variations:
AC_SUBST(CP)
AC_SUBST(RM)
AC_SUBST(MV)
AC_SUBST(EXE_SUFFIX)
RM="rm -f"
CP="cp"
MV="mv"

### Guess the operating system
AC_SUBST(ARCHITECTURE)dnl	Type of processor for which this is compiled
AC_SUBST(SOFTWARE_TYPE)dnl	Type of operating system
AC_SUBST(SOFTWARE_VERSION)dnl	Version number of operating system
AC_SUBST(MACHINE_VERSION)dnl	Version of the machine

AC_SUBST(LDRPATH)dnl	Sometimes the path for finding DLLs must be hardcoded.
AC_SUBST(LIBPREFIX)dnl	Name components of a statically linked library
AC_SUBST(LIBEXT)
AC_SUBST(SHAREDEXT)dnl	Name components of a dynamically linked library
AC_SUBST(SHAREDPREFIX)
AC_SUBST(OBJEXT)dnl	These are set by autoconf
AC_SUBST(EXEEXT)
AC_SUBST(INSTALL_TARGET)dnl Which type of installation: flat directory or unix like.
AC_SUBST(thehost)
CFLAGS=''
LDRPATH='~*'
SHAREDEXT='so'
SHAREDPREFIX='lib'
LIBPREFIX='lib'
LIBEXT='a'
PICFLAG='-fPIC'
THREAD_CFLAGS=''
THREAD_LIBS=''
THREAD_GC_FLAGS='--enable-threads=posix'
INSTALL_TARGET='install'
THREAD_OBJ='threads'
#clibs=''
CORE_OS_LIBS=''
SONAME=''
SONAME_LDFLAGS=''
case "${host_os}" in
	# libdir may have a dollar expression inside
	linux*)
		thehost='linux'
		THREAD_CFLAGS='-pthread'
		THREAD_LIBS='-lrt'
		SHARED_LDFLAGS="-shared ${LDFLAGS}"
		BUNDLE_LDFLAGS="-shared ${LDFLAGS}"
		LDRPATH='-Wl,--rpath,~A'
		#clibs="-ldl"
		CORE_OS_LIBS="-pthread -ldl"
		MKCL_CFLAGS="-D_GNU_SOURCE -fno-strict-aliasing"
		SONAME="${SHAREDPREFIX}mkcl.${SHAREDEXT}.SOVERSION"
		SONAME_LDFLAGS="-Wl,-soname,SONAME"
		;;
	gnu*)
		thehost='gnu'
		THREAD_CFLAGS='-D_THREAD_SAFE'
		THREAD_LIBS='-lpthread'
		SHARED_LDFLAGS="-shared ${LDFLAGS}"
		BUNDLE_LDFLAGS="-shared ${LDFLAGS}"
		LDRPATH='-Wl,--rpath,~A'
		#clibs="-ldl"
		CORE_OS_LIBS="-pthread -ldl"
		MKCL_CFLAGS="-D_GNU_SOURCE"
		SONAME="${SHAREDPREFIX}mkcl.${SHAREDEXT}.SOVERSION"
		SONAME_LDFLAGS="-Wl,-soname,SONAME"
		;;
	kfreebsd*-gnu)
		thehost='kfreebsd'
		THREAD_CFLAGS='-D_THREAD_SAFE'
		THREAD_LIBS='-lpthread'
		SHARED_LDFLAGS="-shared ${LDFLAGS}"
		BUNDLE_LDFLAGS="-shared ${LDFLAGS}"
		LDRPATH='-Wl,--rpath,~A'
		#clibs="-ldl"
		CORE_OS_LIBS="-pthread -ldl"
		MKCL_CFLAGS="-D_GNU_SOURCE"
		SONAME="${SHAREDPREFIX}mkcl.${SHAREDEXT}.SOVERSION"
		SONAME_LDFLAGS="-Wl,-soname,SONAME"
		;;
	freebsd*)
		thehost='freebsd'
		THREAD_CFLAGS='-pthread'
		THREAD_LIBS='-lpthread'
		SHARED_LDFLAGS="-shared ${LDFLAGS}"
		BUNDLE_LDFLAGS="-shared ${LDFLAGS}"
		LDRPATH="-Wl,--rpath,~A"
		clibs=""
                CORE_OS_LIBS="-pthread"
		MKCL_CFLAGS="-D_GNU_SOURCE -fno-strict-aliasing"
		SONAME="${SHAREDPREFIX}mkcl.${SHAREDEXT}.SOVERSION"
		SONAME_LDFLAGS="-Wl,-soname,SONAME"
		;;
	netbsd*)
		thehost='netbsd'
		THREAD_LIBS='-lpthread'
		SHARED_LDFLAGS="-shared ${LDFLAGS}"
		BUNDLE_LDFLAGS="-shared ${LDFLAGS}"
		LDRPATH="-Wl,--rpath,~A"
		#clibs=""
                CORE_OS_LIBS=""
		SONAME="${SHAREDPREFIX}mkcl.${SHAREDEXT}.SOVERSION"
		SONAME_LDFLAGS="-Wl,-soname,SONAME"
		;;
	openbsd*)
		thehost='openbsd'
		THREAD_CFLAGS=''
		THREAD_LIBS='-lpthread'
		SHARED_LDFLAGS="-shared ${LDFLAGS}"
		BUNDLE_LDFLAGS="-shared ${LDFLAGS}"
		LDRPATH="-Wl,--rpath,~A"
		#clibs=""
                CORE_OS_LIBS=""
		SONAME="${SHAREDPREFIX}mkcl.${SHAREDEXT}.SOVERSION"
		SONAME_LDFLAGS="-Wl,-soname,SONAME"
		;;
	solaris*)
		thehost='sun4sol2'
		SHARED_LDFLAGS="-dy -G ${LDFLAGS}"
		BUNDLE_LDFLAGS="-dy -G ${LDFLAGS}"
		LDRPATH='-Wl,-R,~A'
		TCP_LIBS='-lsocket -lnsl -lintl'
		#clibs='-ldl'
		CORE_OS_LIBS='-ldl'
		;;
	cygwin*)
		thehost='cygwin'
		shared='yes'
		THREAD_CFLAGS='-D_THREAD_SAFE'
		THREAD_LIBS='-lpthread'
		SHARED_LDFLAGS="-shared ${LDFLAGS}"
		BUNDLE_LDFLAGS="-shared ${LDFLAGS}"
		SHAREDPREFIX=''
		SHAREDEXT='dll'
		PICFLAG=''
		;;
	mingw*)
		thehost='mingw32'
		#clibs=''
		CORE_OS_LIBS=''
		shared='yes'
		MKCL_CFLAGS="-fno-strict-aliasing"
		THREAD_CFLAGS='-mthreads -D_MT'
		THREAD_GC_FLAGS='--enable-threads=win32'
		SHARED_LDFLAGS="-shared ${LDFLAGS}"
		BUNDLE_LDFLAGS="-shared ${LDFLAGS}"
		SHAREDPREFIX=''
		SHAREDEXT='dll'
		SONAME="libmkcl.SOVERSION.${SHAREDEXT}"
		PICFLAG=''
		INSTALL_TARGET='flatinstall'
		TCP_LIBS='-lws2_32'
		case "$(cmd //C ver)" in
		   *Windows\ NT*) WINVER='WinNT' ;;
		   *Windows\ 2000*) WINVER='Win2000' ;;
		   *Windows\ XP*) WINVER='WinXP' ;;
		   *version\ 6.0*) WINVER='WinVista' ;;
		   *version\ 6.1*) WINVER='Win7' ;;
                   *) WINVER='unknown';;
		esac
		;;
	darwin*)
		thehost='darwin'
		shared='yes'
		SHAREDEXT='dylib'
		PICFLAG='-fPIC -fno-common'
		SHARED_LDFLAGS="-dynamiclib -flat_namespace -undefined suppress ${LDFLAGS}"
		BUNDLE_LDFLAGS="-bundle ${LDFLAGS}"
		LDRPATH=''
		THREAD_CFLAGS='-D_THREAD_SAFE'
		THREAD_LIBS='-lpthread'
		# The GMP library has not yet been ported to Intel-OSX
		if test "`uname -m`" = i386; then
		  gmp_build=none-apple-${host_os}
		else
		  export ABI=mode32
		fi
                # The Boehm-Weiser GC library shipped with Fink does not work
                # well with our signal handler.
                enable_boehm=included
		# MKCL, due to some of the libraries, does not build on
		# 64 bit mode on OSX. We prevent GMP using that mode.
		SONAME="${SHAREDPREFIX}mkcl.SOVERSION.${SHAREDEXT}"
		SONAME_LDFLAGS="-Wl,-install_name,SONAME -Wl,-compatibility_version,${PACKAGE_VERSION}"
		;;
	nsk*)
		# HP Non-Stop platform
		thehost='nonstop'
		shared='yes'
		PICFLAG='-call_shared'
		THREAD_CFLAGS='-spthread'
		SHARED_LDFLAGS="-shared ${LDFLAGS}"
		BUNDLE_LDFLAGS="-shared ${LDFLAGS}"
		LDRPATH='-Wld=\"-rld_l ~A\"'
		#clibs="-Wld=-lrld"
		CORE_OS_LIBS="-Wld=-lrld"
		;;
	*)
		thehost="$host_os"
		shared="no"
		;;
esac
case "${host_cpu}" in
	alpha*)
		CFLAGS="${CFLAGS} -mieee";;
esac
#MKCL_CFLAGS="-D${thehost}"
case "${WINVER}" in
     WinNT|Win2000|WinXP|WinVista|Win7)
      MKCL_CFLAGS="${MKCL_CFLAGS} -D${WINVER}";;
     *) ;;
esac
if test "${enable_debug+set}" = set; then
MKCL_OPTIM_CFLAGS="${DEBUG_CFLAGS}"
else
MKCL_OPTIM_CFLAGS="-O2"
fi
AC_MSG_CHECKING(for ld flags when building shared libraries)
if test "${enable_shared}" = "yes"; then
AC_MSG_RESULT([${SHARED_LDFLAGS}])
MKCL_CFLAGS="${MKCL_CFLAGS} ${PICFLAG}"
else
shared="no";
AC_MSG_RESULT(cannot build)
fi
#LIBS="${clibs} ${LIBS}"
AC_MSG_CHECKING(for required libraries)
#AC_MSG_RESULT([${clibs}])
AC_MSG_RESULT([${CORE_OS_LIBS}])
AC_MSG_CHECKING(for architecture)
ARCHITECTURE=`echo "${host_cpu}" | tr a-z A-Z` # i386 -> I386
AC_MSG_RESULT([${ARCHITECTURE}])
AC_MSG_CHECKING(for software type)
SOFTWARE_TYPE="$thehost"
case "${WINVER}" in
  WinNT) SOFTWARE_TYPE="${SOFTWARE_TYPE} on Windows NT";;
  Win2000) SOFTWARE_TYPE="${SOFTWARE_TYPE} on Windows 2000";;
  WinXP) SOFTWARE_TYPE="${SOFTWARE_TYPE} on Windows XP";;
  WinVista) SOFTWARE_TYPE="${SOFTWARE_TYPE} on Windows Vista";;
  Win7) SOFTWARE_TYPE="${SOFTWARE_TYPE} on Windows 7";;
  *) ;;
esac
SOFTWARE_VERSION=""
AC_MSG_RESULT([${SOFTWARE_TYPE} / ${SOFTWARE_VERSION}])
])

dnl
dnl --------------------------------------------------------------
dnl Check whether the FILE structure has a field with the number of
dnl characters left in the buffer.
dnl
AC_DEFUN(MKCL_FILE_STRUCTURE,[
AC_SUBST(MKCL_FILE_CNT)
if test -z "${MKCL_FILE_CNT}"; then
MKCL_FILE_CNT=0
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[#include <stdio.h>]], [[
int main() {
  FILE *f = fopen("conftestval","w");
  if ((f)->_IO_read_end - (f)->_IO_read_ptr)
    return 1;
}]])],[MKCL_FILE_CNT=1],[])
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[#include <stdio.h>]], [[
int main() {
  FILE *f = fopen("conftestval","w");
  if ((f)->_r)
    return 1;
}]])],[MKCL_FILE_CNT=2],[])
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[#include <stdio.h>]], [[
int main() {
  FILE *f = fopen("conftestval","w");
  if ((f)->_cnt)
    return 1;
}]])],[MKCL_FILE_CNT=3],[])
fi
])
dnl
dnl --------------------------------------------------------------
dnl Check the existence of different integer types and that they
dnl have the right size;
dnl
AC_DEFUN(MKCL_INTEGER_TYPES,[
MKCL_STDINT_HEADER=""
MKCL_UINT16_T=""
MKCL_UINT32_T=""
MKCL_UINT64_T=""
MKCL_INT16_T=""
MKCL_INT32_T=""
MKCL_INT64_T=""
AC_SUBST(MKCL_STDINT_HEADER)
AC_CHECK_HEADER([stdint.h],[MKCL_STDINT_HEADER="#include <stdint.h>"],[])
if test -z "${MKCL_STDINT_HEADER}"; then
AC_CHECK_HEADER([inttypes.h],[MKCL_STDINT_HEADER="#include <inttypes.h>"],[])
fi
if test -n "${MKCL_STDINT_HEADER}" -a -z "${MKCL_UINT8_T}"; then
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[#ifdef HAVE_STDINT_H
#include <inttypes.h>
#else
#ifdef HAVE_STDINT_H
#include <stdint.h>
#endif
#endif]], [[
{
  uint8_t i = 0x80;
  if (i == 0)
    return 0;
  if ((i << 1))
    return 0;
  if ((i - 1) != 0x7F)
    return 0;
  return 1;
}]])],[MKCL_UINT8_T=uint8_t;MKCL_INT8_T=int8_t],[])
fi
if test -z "${MKCL_UINT8_T}"; then
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([], [[
{
  unsigned char c = 0x80;
  if (i == 0)
    return 0;
  if ((i << 1))
    return 0;
  if ((i - 1) != 0x7F)
    return 0;
  return 1;
}]])],[MKCL_UINT8_T="unsigned char";MKCL_INT8_T="signed char"],[])
fi
if test -n "${MKCL_STDINT_HEADER}" -a -z "${MKCL_UINT16_T}"; then
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[#ifdef HAVE_STDINT_H
#include <inttypes.h>
#else
#ifdef HAVE_STDINT_H
#include <stdint.h>
#endif
#endif]], [[
{
  uint16_t i = 0x8000UL;
  if (i == 0)
    return 0;
  if ((i << 1))
    return 0;
  if ((i - 1) != 0x7FFFUL)
    return 0;
  return 1;
}]])],[MKCL_UINT16_T=uint16_t;MKCL_INT16_T=int16_t],[])
fi
if test -n "${MKCL_STDINT_HEADER}" -a -z "${MKCL_UINT32_T}"; then
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[#ifdef HAVE_STDINT_H
#include <inttypes.h>
#else
#ifdef HAVE_STDINT_H
#include <stdint.h>
#endif
#endif]], [[
{
  uint32_t i = 0x80000000UL;
  if (i == 0)
    return 0;
  if ((i << 1))
    return 0;
  if ((i - 1) != 0x7FFFFFFFUL)
    return 0;
  return 1;
}]])],[MKCL_UINT32_T=uint32_t;MKCL_INT32_T=int32_t],[])
fi
if test -n "${MKCL_STDINT_HEADER}" -a -z "${MKCL_UINT64_T}"; then
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[#ifdef HAVE_STDINT_H
#include <inttypes.h>
#else
#ifdef HAVE_STDINT_H
#include <stdint.h>
#endif
#endif]], [[
{
  uint64_t i = 1;
  i <<= 63; if (i == 0) return 0;
  i <<= 1;  if (i) return 0;
  return 1;
}]])],[MKCL_UINT64_T=uint64_t;MKCL_INT64_T=int64_t],[])
fi
if test "${MKCL_UINT16_T}${CL_FIXNUM_BITS}" = "16"; then
  MKCL_UINT16_T="cl_index"
  MKCL_INT16_T="cl_fixnum"
fi
if test "${MKCL_UINT32_T}${CL_FIXNUM_BITS}" = "32"; then
  MKCL_UINT32_T="cl_index"
  MKCL_INT32_T="cl_fixnum"
fi
if test "${MKCL_UINT64_T}${CL_FIXNUM_BITS}" = "64"; then
  MKCL_UINT64_T="cl_index"
  MKCL_INT64_T="cl_fixnum"
fi
AC_MSG_CHECKING(uint8_t type)
if test -n "${MKCL_UINT8_T}"; then
  AC_DEFINE_UNQUOTED([mkcl_uint8_t],[$MKCL_UINT8_T])
  AC_DEFINE_UNQUOTED([mkcl_int8_t],[$MKCL_INT8_T])
  AC_MSG_RESULT(${MKCL_UINT8_T})
else
  AC_MSG_RESULT(none)
  AC_MSG_ERROR(Can not build MKCL without byte types)
fi
AC_MSG_CHECKING(uint16_t type)
if test -n "${MKCL_UINT16_T}"; then
  AC_DEFINE_UNQUOTED([mkcl_uint16_t],[$MKCL_UINT16_T])
  AC_DEFINE_UNQUOTED([mkcl_int16_t],[$MKCL_INT16_T])
  AC_MSG_RESULT(${MKCL_UINT16_T})
else
  AC_MSG_RESULT(none)
fi
AC_MSG_CHECKING(uint32_t type)
if test -n "${MKCL_UINT32_T}"; then
  AC_DEFINE_UNQUOTED([mkcl_uint32_t],[$MKCL_UINT32_T])
  AC_DEFINE_UNQUOTED([mkcl_int32_t],[$MKCL_INT32_T])
  AC_MSG_RESULT(${MKCL_UINT32_T})
else
  AC_MSG_RESULT(none)
fi
AC_MSG_CHECKING(uint64_t type)
if test -n "${MKCL_UINT64_T}"; then
  AC_DEFINE_UNQUOTED([mkcl_uint64_t],[$MKCL_UINT64_T])
  AC_DEFINE_UNQUOTED([mkcl_int64_t],[$MKCL_INT64_T])
  AC_MSG_RESULT(${MKCL_UINT64_T})
else
  AC_MSG_RESULT(none)
fi
])
dnl
dnl --------------------------------------------------------------
dnl Check the direction to which the stack grows (for garbage
dnl collection).
dnl
AC_DEFUN(MKCL_STACK_DIRECTION,[
  AC_MSG_CHECKING(whether stack grows downwards)
if test -z "${MKCL_STACK_DIR}" ; then
  AC_RUN_IFELSE([AC_LANG_SOURCE([[
#include <stdio.h>
/* This code is in antique K&R style, deliberately. */

#if __GNUC__ >= 4
int f2() __attribute__((noinline));
#else
int f2();
#endif

int f1() {
  extern int (* f_ptr)(); /* Try really hard to prevent inlining of next call. */
  char d = 0;
  return f_ptr(&d);
}

int f2(mark)
  char * mark;
{
  char c = 0;
  /* The purpose of this printf call is to prevent the compiler
     from inlining this function (like gcc 4.7.0 does).
     This is hoped to be portable, otherwise compiler specific
     pragmas or attributes would be needed. (like gcc __attribute__((noinline)))
   */
  printf("callee frame = 0x%lx, caller frame = 0x%lx, ", (unsigned long) &c, (unsigned long) mark);
  return &c - mark;
}

int (* f_ptr)() = NULL;

int main() {
  f_ptr = f2;
  if (f1() > 0)
    return 1;
  else
    return 0;
}
]])],[MKCL_STACK_DIR=down],[MKCL_STACK_DIR=up],[])
fi
case "${MKCL_STACK_DIR}" in
  down|DOWN) AC_MSG_RESULT(yes); AC_DEFINE(MKCL_DOWN_STACK, [1], [Stack grows downwards]) ;;
  up|UP) AC_MSG_RESULT(no) ;;
  *) AC_MSG_ERROR(Unable to determine stack growth direction)
esac])
dnl
dnl ------------------------------------------------------------
dnl Find out a setjmp() that does not save signals. It is called
dnl in several architectures.
AC_DEFUN(MKCL_FIND_SETJMP,[
AC_SUBST(MKCL_SETJMP)
AC_SUBST(MKCL_LONGJMP)
AC_CHECK_FUNC(_longjmp,
MKCL_SETJMP="_setjmp";MKCL_LONGJMP="_longjmp",
MKCL_SETJMP="setjmp";MKCL_LONGJMP="longjmp")])

dnl
dnl --------------------------------------------------------------
dnl Guess the right type and size for cl_fixnum. It must be large
dnl enough that convertion back and forth to pointer implies no
dnl loss of information.
AC_DEFUN(MKCL_FIXNUM_TYPE,[
AC_SUBST(CL_FIXNUM_TYPE)
AC_SUBST(CL_FIXNUM_BITS)
AC_SUBST(CL_FIXNUM_MAX)
AC_SUBST(CL_FIXNUM_MIN)
AC_SUBST(CL_INT_BITS)
AC_SUBST(CL_LONG_BITS)
AC_MSG_CHECKING(appropriate type for fixnums)
if test -z "${CL_FIXNUM_TYPE}" ; then
  AC_RUN_IFELSE([AC_LANG_SOURCE([[#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
int main() {
  const char *int_type;
  int bits;
  FILE *f=fopen("conftestval", "w");
  if (!f) exit(1);
  if (sizeof(int) >= sizeof(void*)) {
    unsigned int t = 1;
    signed int l = 0;
    int_type="int";
    for (bits=1; ((t << 1) >> 1) == t; bits++, t <<= 1);
    l = (~l) << (bits - 3);
    fprintf(f,"CL_FIXNUM_MIN='%d';",l);
    fprintf(f,"CL_FIXNUM_MAX='%d';",-(l+1));
  } else if (sizeof(long) >= sizeof(void*)) {
    unsigned long int t = 1;
    signed long int l = 0;
    int_type="long int";
    for (bits=1; ((t << 1) >> 1) == t; bits++, t <<= 1);
    l = (~l) << (bits - 3);
    fprintf(f,"CL_FIXNUM_MIN='%ld';",l);
    fprintf(f,"CL_FIXNUM_MAX='%ld';",-(l+1));
  } else if (sizeof(long long) >= sizeof(void*)) {
    unsigned long long int t = 1;
    signed long long int l = 0;
    int_type="long long int";
    for (bits=1; ((t << 1) >> 1) == t; bits++, t <<= 1);
    l = (~l) << (bits - 3);
    fprintf(f,"CL_FIXNUM_MIN='%lldLL';",l);
    fprintf(f,"CL_FIXNUM_MAX='%lldLL';",-(l+1));
  } else
    exit(1);
  fprintf(f,"CL_FIXNUM_TYPE='%s';",int_type);
  fprintf(f,"CL_FIXNUM_BITS='%d';",bits);
  {
    unsigned int x = 1;
    for (bits = 0; x; bits++) {
      x <<= 1;
    }
    fprintf(f,"CL_INT_BITS='%d';",bits);
  }
  {
    unsigned long x = 1;
    for (bits = 0; x; bits++) {
      x <<= 1;
    }
    fprintf(f,"CL_LONG_BITS='%d'",bits);
  }
  exit(0);
}]])],[eval "`cat conftestval`"],[],[])
fi
if test -z "${CL_FIXNUM_TYPE}" ; then
AC_MSG_ERROR(There is no appropriate integer type for the cl_fixnum type)
fi
AC_MSG_RESULT([${CL_FIXNUM_TYPE}])])

dnl
dnl ------------------------------------------------------------
dnl Find out what is written for every '\n' character, when
dnl opening a text file.
dnl
AC_DEFUN(MKCL_LINEFEED_MODE,[
AC_MSG_CHECKING(character sequence for end of line)
if test -z "${MKCL_NEWLINE}" ; then
AC_RUN_IFELSE([AC_LANG_SOURCE([[#include <stdio.h>
#include <stdlib.h>
int main() {
  FILE *f = fopen("conftestval","w");
  int c1, c2;
  char *output;
  if (f == NULL) exit(1);
  fprintf(f, "\n");
  fclose(f);
  f = fopen("conftestval","rb");
  if (f == NULL) exit(1);
  c1 = fgetc(f);
  c2 = fgetc(f);
  fclose(f);
  f = fopen("conftestval","w");
  if (f == NULL) exit(1);
  if (c1 == '\r')
    if (c2 == EOF)
      output="CR";
    else
      output="CRLF";
  else
    output="LF";
  fclose(f);
  f = fopen("conftestval","w");
  if (f == NULL) exit(1);
#if 0
  fprintf(f, output); /* This one seems to spook some security settings now. */
#else
  fputs(output, f);
#endif
  fclose(f);
  exit(0);
}
]])],[MKCL_NEWLINE=`cat conftestval`],[],[])
fi
case "${MKCL_NEWLINE}" in
  LF) AC_MSG_RESULT(lf) ;;
  CR) AC_MSG_RESULT(cr); AC_DEFINE(MKCL_NEWLINE_IS_CR, [1], [Define if your newline is CR]) ;;
  CRLF) AC_MSG_RESULT(cr+lf); AC_DEFINE(MKCL_NEWLINE_IS_CRLF, [1], [Define if your newline is CRLF]) ;;
  *) AC_MSG_ERROR(Unable to determine linefeed mode) ;;
esac
])

dnl
dnl ------------------------------------------------------------
dnl Find out which program we can use to install INFO files
dnl
AC_DEFUN(MKCL_INSTALL_INFO,[
AC_SUBST(INSTALL_INFO)
AC_PATH_PROG(INSTALL_INFO, install-info, [/sbin/install-info],
[$PATH:/usr/bin:/usr/sbin:/usr/etc:/usr/libexec])
])

dnl
dnl ------------------------------------------------------------
dnl Use the configuration scripts in the GMP library for
dnl configuring MKCL in a compatible way.
dnl
AC_DEFUN(MKCL_GMP_BASED_CONFIG,[
AC_MSG_CHECKING([Using the GMP library to guess good compiler/linker flags])
(rm -rf tmp; \
 mkdir tmp; \
 aux=`cd ${srcdir}/gmp; pwd`;
 cd tmp; \
 ${aux}/configure --srcdir=${aux} --prefix=${builddir} >/dev/null 2>&1)
GMP_CFLAGS=`grep '^s,@CFLAGS@' tmp/config.status| sed 's&s,@CFLAGS@,\(.*\),;t t&\1&'`
GMP_LDFLAGS=`grep '^s,@GMP_LDFLAGS@' tmp/config.status| sed 's&s,@GMP_LDFLAGS@,\(.*\),;t t&\1&'`;
rm -rf tmp
# Notice that GMP_LDFLAGS is designed to be passed to libtool, and therefore
# some options could be prefixed by -Wc, which means "flag for the compiler".
LDFLAGS=`grep '^s,@LDFLAGS@' config.status| sed 's&s,@LDFLAGS@,\(.*\),;t t&\1&'`;
LDFLAGS=`echo ${LDFLAGS} ${GMP_LDFLAGS} | sed 's%-Wc,%%'`
CFLAGS="${CFLAGS} ${GMP_CFLAGS}"
#host=`grep '^s,@host@' config.status | sed 's&s,@host@,\(.*\),;t t&\1&'`
AC_MSG_CHECKING([C/C++ compiler flags])
AC_MSG_RESULT([${CFLAGS}])
AC_MSG_CHECKING([Linker flags])
AC_MSG_RESULT([${LDFLAGS}])
])

dnl
dnl ------------------------------------------------------------
dnl Do we have a non-portable implementation of calls to foreign
dnl functions?
dnl
AC_DEFUN([MKCL_FFI],[
AC_MSG_CHECKING([whether we can dynamically build calls to C functions])
case "${host_cpu}" in
   i686 | i586 | pentium* | athlon* )
	EXTRA_OBJS="${EXTRA_OBJS} ffi_x86.o"
	if test "${enable_asmapply}" = "yes" ; then
		EXTRA_OBJS="${EXTRA_OBJS} apply_x86.o"
		AC_DEFINE(MKCL_ASM_APPLY)
	fi
        AC_DEFINE(MKCL_USE_VARARG_AS_POINTER)
	dynamic_ffi=yes
	;;
   x86_64 )
        if test "${CL_FIXNUM_BITS}" = 32 ; then
	  EXTRA_OBJS="${EXTRA_OBJS} ffi_x86.o"
	else
            case "${host_os}" in
                mingw*)
	            EXTRA_OBJS="${EXTRA_OBJS} ffi_x86_64_w64.o"
                    ;;
                *)
	            EXTRA_OBJS="${EXTRA_OBJS} ffi_x86_64.o"
                    ;;
            esac
	fi
	dynamic_ffi=yes
	;;
   *)
	dynamic_ffi=no
	;;
esac
AC_MSG_RESULT([${dynamic_ffi}])
if test "$dynamic_ffi" = "yes" ; then
  AC_DEFINE(MKCL_DYNAMIC_FFI, 1, [we can build calls to foreign functions])
fi
])

dnl --------------------------------------------------------------
dnl Provides a test for the existance of the __thread declaration and
dnl defines WITH___THREAD if it is found
AC_DEFUN([MKCL___THREAD],
[AC_CACHE_CHECK(for __thread local data, ac_cv_mkcl___thread,
AC_TRY_COMPILE(,[static __thread void *data;],
   ac_cv_mkcl___thread=yes,
   ac_cv_mkcl___thread=no))
dnl We deactivate this test because it seems to slow down MKCL A LOT!!!
])

dnl ----------------------------------------------------------------------
dnl Choose the type of code to detect floating point exceptions and
dnl raise them.
dnl
AC_DEFUN([MKCL_FPE_MODEL],
[AC_MSG_CHECKING([for code to detect FP exceptions])
case "${host_cpu}" in
   i686 |i586 | pentium* | athlon* )
	MKCL_FPE_CODE="arch/fpe_x86.c"
	AC_MSG_RESULT([x86])
	;;
   x86_64* )
	MKCL_FPE_CODE="arch/fpe_x86.c"
	AC_MSG_RESULT([x86_64])
	;;
   *)
        MKCL_FPE_CODE="arch/fpe_none.c"
	AC_MSG_RESULT([not available])
	;;
esac
AC_SUBST(MKCL_FPE_CODE)
])
