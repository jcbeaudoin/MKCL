/* -*- mode: c  -*- */
/*
    Copyright (c) 2022, Jean-Claude Beaudoin.

    This program is under GNU LGPL, you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 3 of the License, or (at your option) any later version.

    See file '../../../Copyright' for full details.
*/


#ifdef MKCL_MIN
#define MKCL_IN_LISP(x) NULL
#else
#define MKCL_IN_LISP(x) x
#endif

#ifdef MKCL_PACKAGE_BUILDER
#define SYMBOL_NAME(it) {				\
    (int8_t)mkcl_t_base_string, 0, FALSE, FALSE,	\
      mk_cl_Cnil,					\
      (sizeof(it)-1),					\
      (sizeof(it)-1),					\
      (it),						\
      NULL, NULL,					\
      }
#else
# define SYMBOL_NAME(it) MKCL_BASE_STRING_INIT(it)
#endif

static struct mkcl_base_string const mkcl_mkcl_external_symbol_names[] = {
  SYMBOL_NAME("!"),
  SYMBOL_NAME("*ALL-CURRENT-WORKING-DIRECTORIES*"),
  SYMBOL_NAME("*CURRENT-WORKING-DIRECTORY*"),
  SYMBOL_NAME("*MODULE-PROVIDER-FUNCTIONS*"),
  SYMBOL_NAME("ARGC"),
  SYMBOL_NAME("ARGV"),
  SYMBOL_NAME("CHDIR"),
  SYMBOL_NAME("FIXNUMP"),
  SYMBOL_NAME("GETENV"),
  SYMBOL_NAME("GETCWD"),
  SYMBOL_NAME("GETPID"),
  SYMBOL_NAME("GETTID"),
  SYMBOL_NAME("LOGICAL-PATHNAME-P"),
  SYMBOL_NAME("MKDIR"),
  SYMBOL_NAME("MKSTEMP"),
  SYMBOL_NAME("RMDIR"),
  SYMBOL_NAME("MAKE-PIPE"),
  SYMBOL_NAME("RUN-COMMAND"),
  SYMBOL_NAME("RUN-PROGRAM-1"),
  SYMBOL_NAME("SETENV"),
  SYMBOL_NAME("SYSTEM"),
  SYMBOL_NAME("BYTE8"),
  SYMBOL_NAME("BYTE16"),
  SYMBOL_NAME("BYTE32"),
  SYMBOL_NAME("BYTE64"),
  SYMBOL_NAME("COMPILER-LET"),
  SYMBOL_NAME("INTEGER8"),
  SYMBOL_NAME("INTEGER16"),
  SYMBOL_NAME("INTEGER32"),
  SYMBOL_NAME("INTEGER64"),
  SYMBOL_NAME("QUIT"),
  SYMBOL_NAME("CL-WORD"),
  SYMBOL_NAME("CL-INDEX"),
  SYMBOL_NAME("INTERACTIVE-INTERRUPT"),
  SYMBOL_NAME("STORAGE-EXHAUSTED"),
  SYMBOL_NAME("STACK-OVERFLOW"),
  SYMBOL_NAME("STACK-OVERFLOW-SIZE"),
  SYMBOL_NAME("STACK-OVERFLOW-TYPE"),
  SYMBOL_NAME("SEGMENTATION-VIOLATION"),
  SYMBOL_NAME("SEGMENTATION-VIOLATION-ADDRESS"),
  SYMBOL_NAME("CHARACTER-STRING"),
  SYMBOL_NAME("COPY-FILE"),
  SYMBOL_NAME("BAD-FASL-FILE"),
  SYMBOL_NAME("INVALID-SLOT"),
  SYMBOL_NAME("GETUID"),
  SYMBOL_NAME("WITH-UNIQUE-NAMES"),
  SYMBOL_NAME("PROCESS"),
  SYMBOL_NAME("JOIN-PROCESS"),
  SYMBOL_NAME("TERMINATE-PROCESS"),
  SYMBOL_NAME("PROCESS-P"),
  SYMBOL_NAME("PROCESS-INPUT"),
  SYMBOL_NAME("PROCESS-OUTPUT"),
  SYMBOL_NAME("PROCESS-ERROR"),
  SYMBOL_NAME("PROCESS-PLIST"),
  SYMBOL_NAME("PROCESS-TO-WORKER"),
  SYMBOL_NAME("PROCESS-FROM-WORKER"),
  SYMBOL_NAME("PROCESS-ERROR-FROM-WORKER"),
  SYMBOL_NAME("SET-PROCESS-PLIST"),
  SYMBOL_NAME("SET-PROCESS-TO-WORKER"),
  SYMBOL_NAME("SET-PROCESS-FROM-WORKER"),
  SYMBOL_NAME("SET-PROCESS-ERROR-FROM-WORKER"),
  SYMBOL_NAME("PROCESS-STATUS"),
  SYMBOL_NAME("PROCESS-EXIT-CODE"),
  SYMBOL_NAME("PROCESS-COMMAND"),
  SYMBOL_NAME("PROCESS-ARGV"),
  SYMBOL_NAME("PROCESS-ID"),
  SYMBOL_NAME("PROCESS-DETACHED-P"),
  SYMBOL_NAME("DETACH-PROCESS"),
  SYMBOL_NAME("SIMPLE-STYLE-WARNING"),
  SYMBOL_NAME("DOLIST!"),
  SYMBOL_NAME("PROBE-FILE-P"),
  SYMBOL_NAME("STREAM-FILENAME"),
  SYMBOL_NAME("PRIN1-TO-BASE-STRING"),
  SYMBOL_NAME("PRINC-TO-BASE-STRING"),
  SYMBOL_NAME("WRITE-TO-BASE-STRING"),
  SYMBOL_NAME("C-EXPORT-FNAME"),
  SYMBOL_NAME("STREAM-ENCODING-ERROR"),
  SYMBOL_NAME("STREAM-DECODING-ERROR"),
  SYMBOL_NAME("BASE-CHAR-P"),
  SYMBOL_NAME("PATHNAME-COMPLETE-P"),
  SYMBOL_NAME("MELD-PATHNAMES"),
  SYMBOL_NAME("NATURAL8"),
  SYMBOL_NAME("NATURAL16"),
  SYMBOL_NAME("NATURAL32"),
  SYMBOL_NAME("NATURAL64"),
  SYMBOL_NAME("OCTET"),
  SYMBOL_NAME("DOUBLE-OCTET"),
  SYMBOL_NAME("OCTETS"),
  SYMBOL_NAME("DOUBLE-OCTETS"),
  SYMBOL_NAME("RELATIVE-PATHNAME"),
  SYMBOL_NAME("RELATIVE-NAMESTRING"),
  SYMBOL_NAME("ABSOLUTE-LOGICAL-PATHNAME"),
  SYMBOL_NAME("ABSOLUTE-PATHNAME"),
  SYMBOL_NAME("ABSOLUTE-PATHNAME-P"),
  SYMBOL_NAME("RELATIVE-PATHNAME-P"),
  SYMBOL_NAME("DIRECTORY-P"),
  SYMBOL_NAME("COMPLETE-PATHNAME"),
  SYMBOL_NAME("PHYSICALLY-COMPLETE-PATHNAME"),
  SYMBOL_NAME("FILE-PATHNAME"),
  SYMBOL_NAME("FULL-DIRECTORY-PATHNAME"),
  SYMBOL_NAME("FULL-DIRECTORY-NAMESTRING"),
  SYMBOL_NAME("PATHNAME+"),
  SYMBOL_NAME("COPY-PATHNAME"),
  SYMBOL_NAME("RUN-PROGRAM"),
  SYMBOL_NAME("DEFAULT-MODULE-PROVIDER"),
  SYMBOL_NAME("GIT-DESCRIBE-THIS-MKCL"),
  SYMBOL_NAME("HELP"),
  SYMBOL_NAME("STR+"),
  SYMBOL_NAME("BSTR+"),
  SYMBOL_NAME("SPLIT-STRING"),
  SYMBOL_NAME("+PATHNAME-CLOSER+"),
  SYMBOL_NAME("LAUNCH-TO-SUBPROCESS-WORKER"),
  SYMBOL_NAME("LAUNCH-FROM-SUBPROCESS-WORKER"),
  SYMBOL_NAME("LAUNCH-ERROR-FROM-SUBPROCESS-WORKER"),
  SYMBOL_NAME("FILE-CHARACTER-POSITION"),
  SYMBOL_NAME("REASSERT-FILE-CHARACTER-POSITION"),
  SYMBOL_NAME("HELP*"),
  SYMBOL_NAME("SHORT-FLOAT-POSITIVE-INFINITY"),
  SYMBOL_NAME("SINGLE-FLOAT-POSITIVE-INFINITY"),
  SYMBOL_NAME("DOUBLE-FLOAT-POSITIVE-INFINITY"),
  SYMBOL_NAME("LONG-FLOAT-POSITIVE-INFINITY"),
  SYMBOL_NAME("SHORT-FLOAT-NEGATIVE-INFINITY"),
  SYMBOL_NAME("SINGLE-FLOAT-NEGATIVE-INFINITY"),
  SYMBOL_NAME("DOUBLE-FLOAT-NEGATIVE-INFINITY"),
  SYMBOL_NAME("LONG-FLOAT-NEGATIVE-INFINITY"),
  SYMBOL_NAME("BASE-CHAR-CODE-LIMIT"),
};


struct mkcl_base_string const mkcl_mkcl_internal_symbol_names[] = {
};


#define internal_count MKCL_NB_ELEMS(mkcl_mkcl_internal_symbol_names)
#define external_count MKCL_NB_ELEMS(mkcl_mkcl_external_symbol_names)
#define internal_size 17
#define external_size 191

static struct mkcl_hashtable_entry internal_entries[];
static struct mkcl_hashtable_entry external_entries[];

static struct mkcl_hashtable_entry * internal_vector[];
static struct mkcl_hashtable_entry * external_vector[];

static struct mkcl_singlefloat rehash_size_factor = { mkcl_t_singlefloat, 0, 0, 0, 1.5f };
static struct mkcl_singlefloat rehash_threshold = { mkcl_t_singlefloat, 0, 0, 0, 0.75f };

static struct mkcl_hashtable internal_ht = {
  mkcl_t_hashtable, 0, mkcl_htt_package, 0, /* MKCL_HEADER2(test,lockable) */
  internal_vector, /* data */
#ifndef MKCL_PACKAGE_BUILDER
  mkcl_search_hash_package, /* search_fun */
  mkcl_hash_equal_package, /* hash_fun */
  mkcl_equality_fun_package, /* equality_fun */
#else
  NULL, /* search_fun */
  NULL, /* hash_fun */
  NULL, /* equality_fun */
#endif
  internal_count, /* entries */
  internal_size, /* size */
  (mkcl_object) &rehash_size_factor, /* rehash_size */
  (mkcl_object) &rehash_threshold, /* threshold */
  12, /* factor_of_16th */
  NULL /* free_bucket */
};

static struct mkcl_hashtable external_ht = {
  mkcl_t_hashtable, 0, mkcl_htt_package, 0, /* MKCL_HEADER2(test,lockable) */
  external_vector, /* data */
#ifndef MKCL_PACKAGE_BUILDER
  mkcl_search_hash_package, /* search_fun */
  mkcl_hash_equal_package, /* hash_fun */
  mkcl_equality_fun_package, /* equality_fun */
#else
  NULL, /* search_fun */
  NULL, /* hash_fun */
  NULL, /* equality_fun */
#endif
  external_count, /* entries */
  external_size, /* size */
  (mkcl_object) &rehash_size_factor, /* rehash_size */
  (mkcl_object) &rehash_threshold, /* threshold */
  12, /* factor_of_16th */
  NULL /* free_bucket */
};

static struct mkcl_base_string mkcl_package_name = SYMBOL_NAME("MKCL");

struct mkcl_package mkcl_package_mkcl = {
  mkcl_t_package, 0, 0, 0, /* MKCL_HEADER1(closed) */
  (mkcl_object) &mkcl_package_name, /* name */
  mk_cl_Cnil, /* nicknames */
  mk_cl_Cnil, /* shadowings */
  mk_cl_Cnil, /* uses */
  mk_cl_Cnil, /* usedby */
  (mkcl_object) &internal_ht, /* internal */
  (mkcl_object) &external_ht, /* external */
#ifndef MKCL_WINDOWS
  PTHREAD_MUTEX_INITIALIZER /* lock */
#endif
};
