/* -*- mode: c  -*- */
/*
    Copyright (c) 2022, Jean-Claude Beaudoin.

    This program is under GNU LGPL, you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 3 of the License, or (at your option) any later version.

    See file '../../../Copyright' for full details.
*/

#ifdef MKCL_PACKAGE_BUILDER
# define SYMBOL_NAME(it) {				\
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

static struct mkcl_base_string const mkcl_mt_external_symbol_names[] = {
  SYMBOL_NAME("THREAD"),
  SYMBOL_NAME("LOCK"),
  SYMBOL_NAME("RWLOCK"),
  SYMBOL_NAME("SEMAPHORE"),
  SYMBOL_NAME("CONDITION-VARIABLE"),
  SYMBOL_NAME("*THREAD*"),
  SYMBOL_NAME("ALL-THREADS"),
  SYMBOL_NAME("EXIT-THREAD"),
  SYMBOL_NAME("TERMINATE-THREAD"),
  SYMBOL_NAME("MAKE-THREAD"),
  SYMBOL_NAME("THREAD-ACTIVE-P"),
  SYMBOL_NAME("THREAD-ENABLE"),
  SYMBOL_NAME("THREAD-YIELD"),
  SYMBOL_NAME("THREAD-KILL"),
  SYMBOL_NAME("THREAD-DETACH"),
  SYMBOL_NAME("THREAD-JOIN"),
  SYMBOL_NAME("THREAD-NAME"),
  SYMBOL_NAME("THREAD-PRESET"),
  SYMBOL_NAME("SHOW-SIGMASK"),
  SYMBOL_NAME("RESET-SIGMASK"),
  SYMBOL_NAME("BLOCK-SIGNALS"),
  SYMBOL_NAME("UNBLOCK-SIGNALS"),
  SYMBOL_NAME("THREAD-RUN-FUNCTION"),
  SYMBOL_NAME("MAKE-LOCK"),
  SYMBOL_NAME("RECURSIVE-LOCK-P"),
  SYMBOL_NAME("LOCK-NAME"),
  SYMBOL_NAME("LOCK-HOLDER"),
  SYMBOL_NAME("GET-LOCK"),
  SYMBOL_NAME("GIVEUP-LOCK"),
  SYMBOL_NAME("MAKE-RWLOCK"),
  SYMBOL_NAME("GIVEUP-RWLOCK"),
  SYMBOL_NAME("GET-READ-RWLOCK"),
  SYMBOL_NAME("GET-WRITE-RWLOCK"),
  SYMBOL_NAME("MAKE-SEMAPHORE"),
  SYMBOL_NAME("SEMAPHORE-COUNT"),
  SYMBOL_NAME("SEMAPHORE-SIGNAL"),
  SYMBOL_NAME("SEMAPHORE-WAIT"),
  SYMBOL_NAME("MAKE-CONDITION-VARIABLE"),
  SYMBOL_NAME("CONDITION-WAIT"),
  SYMBOL_NAME("CONDITION-SIGNAL"),
  SYMBOL_NAME("CONDITION-BROADCAST"),
  SYMBOL_NAME("INTERRUPT-THREAD"),
  SYMBOL_NAME("+LOAD-COMPILE-LOCK+"),
  SYMBOL_NAME("WITH-LOCK"),
  SYMBOL_NAME("WITHOUT-LOCK"),
  SYMBOL_NAME("WITHOUT-INTERRUPTS"),
  SYMBOL_NAME("INTERRUPT-ERROR"),
  SYMBOL_NAME("THREAD-SLEEPING"),
  SYMBOL_NAME("INTERRUPT-REFUSED"),
  SYMBOL_NAME("INVALID-THREAD"),
  SYMBOL_NAME("+FORWARD-REFERENCE-LOCK+"),
  SYMBOL_NAME("JOIN-THREAD"),
  SYMBOL_NAME("DETACH-THREAD"),
  SYMBOL_NAME("THREAD-PLIST"),
  SYMBOL_NAME("SET-THREAD-PLIST"),
  SYMBOL_NAME("ABANDON-THREAD"),
  SYMBOL_NAME("CURRENT-THREAD"),
  SYMBOL_NAME("CANCEL-THREAD"),
  SYMBOL_NAME("TRY-TO-WAKE-UP-THREAD"),
  SYMBOL_NAME("REQUEST-THREAD-SHUTDOWN"),
  SYMBOL_NAME("THREAD-SHUTDOWN-REQUESTED-P"),
  SYMBOL_NAME("THREAD-STATUS"),
  SYMBOL_NAME("ABORT-THREAD"),
};


struct mkcl_base_string const mkcl_mt_internal_symbol_names[] = {
  SYMBOL_NAME("MAYBE-WITH-INTERRUPTS"),
  SYMBOL_NAME("BODY"),
  SYMBOL_NAME("WERE-ENABLED-SYM"),
  SYMBOL_NAME("WITHOUT-ANY-INTERRUPTS"),
  SYMBOL_NAME("FILE"),
  SYMBOL_NAME("LINENO"),
  SYMBOL_NAME("INTERRUPT-ERROR-THREAD"),
  SYMBOL_NAME("THREAD-SLEEPING-IN-FILE"),
  SYMBOL_NAME("THREAD-SLEEPING-ON-LINENO"),
  SYMBOL_NAME("INTERRUPT-DISABLER-FILE"),
  SYMBOL_NAME("INTERRUPT-DISABLER-LINENO"),
  SYMBOL_NAME("INVALID-THREAD-THREAD"),
  SYMBOL_NAME("INVALID-THREAD-REASON"),
  SYMBOL_NAME("REASON"),
  SYMBOL_NAME("INTERRUPT-ERROR-THREAD__INTERRUPT-ERROR"),
  SYMBOL_NAME("INTERRUPT-ERROR-THREAD__THREAD-SLEEPING"),
  SYMBOL_NAME("THREAD-SLEEPING-IN-FILE__THREAD-SLEEPING"),
  SYMBOL_NAME("THREAD-SLEEPING-ON-LINENO__THREAD-SLEEPING"),
  SYMBOL_NAME("PRINT-OBJECT__THREAD-SLEEPING_T"),
  SYMBOL_NAME("INTERRUPT-ERROR-THREAD__INTERRUPT-REFUSED"),
  SYMBOL_NAME("INTERRUPT-DISABLER-FILE__INTERRUPT-REFUSED"),
  SYMBOL_NAME("INTERRUPT-DISABLER-LINENO__INTERRUPT-REFUSED"),
  SYMBOL_NAME("PRINT-OBJECT__INTERRUPT-REFUSED_T"),
  SYMBOL_NAME("INVALID-THREAD-THREAD__INVALID-THREAD"),
  SYMBOL_NAME("INVALID-THREAD-REASON__INVALID-THREAD"),
};


#define internal_count MKCL_NB_ELEMS(mkcl_mt_internal_symbol_names)
#define external_count MKCL_NB_ELEMS(mkcl_mt_external_symbol_names)
#define internal_size 41
#define external_size 89

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

static struct mkcl_base_string mt_package_name = SYMBOL_NAME("MT");

struct mkcl_package mkcl_package_mt = {
  mkcl_t_package, 0, 0, 0, /* MKCL_HEADER1(closed) */
  (mkcl_object) &mt_package_name, /* name */
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
