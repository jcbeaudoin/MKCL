/* -*- mode: c  -*- */
/*
    Copyright (c) 2022, Jean-Claude Beaudoin.

    This program is under GNU LGPL, you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 3 of the License, or (at your option) any later version.

    See file '../../../Copyright' for full details.
*/

#define SYMBOL_NAME(it) {				\
    (int8_t)mkcl_t_base_string, 0, FALSE, FALSE,	\
      mk_cl_Cnil,					\
      (sizeof(it)-1),					\
      (sizeof(it)-1),					\
      (it),						\
      NULL, NULL,					\
      }

#if 0
static struct mkcl_base_string const mkcl_user_external_symbol_names[] = {
};


struct mkcl_base_string const mkcl_user_internal_symbol_names[] = {
};

#define internal_count MKCL_NB_ELEMS(mkcl_user_internal_symbol_names)
#define external_count MKCL_NB_ELEMS(mkcl_user_external_symbol_names)

static struct mkcl_hashtable_entry internal_entries[];
static struct mkcl_hashtable_entry external_entries[];

#endif

#define internal_count 129
#define external_count 129


static struct mkcl_hashtable_entry * internal_vector[internal_count] = { NULL, };
static struct mkcl_hashtable_entry * external_vector[external_count] = { NULL, };

static struct mkcl_singlefloat rehash_size_factor = { mkcl_t_singlefloat, 0, 0, 0, 1.5f };
static struct mkcl_singlefloat rehash_threshold = { mkcl_t_singlefloat, 0, 0, 0, 0.75f };

static struct mkcl_hashtable internal_ht = {
  mkcl_t_hashtable, 0, mkcl_htt_package, 0, /* MKCL_HEADER2(test,lockable) */
  internal_vector, /* data */
  NULL, /* search_fun */
  NULL, /* hash_fun */
  NULL, /* equality_fun */
  0, /* entries */
  internal_count, /* size */
  (mkcl_object) &rehash_size_factor, /* rehash_size */
  (mkcl_object) &rehash_threshold, /* threshold */
  12, /* factor_of_16th */
  NULL /* free_bucket */
};

static struct mkcl_hashtable external_ht = {
  mkcl_t_hashtable, 0, mkcl_htt_package, 0, /* MKCL_HEADER2(test,lockable) */
  external_vector, /* data */
  NULL, /* search_fun */
  NULL, /* hash_fun */
  NULL, /* equality_fun */
  0, /* entries */
  external_count, /* size */
  (mkcl_object) &rehash_size_factor, /* rehash_size */
  (mkcl_object) &rehash_threshold, /* threshold */
  12, /* factor_of_16th */
  NULL /* free_bucket */
};

static struct mkcl_base_string user_package_name = SYMBOL_NAME("COMMON-LISP-USER");

struct mkcl_package mkcl_package_user = {
  mkcl_t_package, 0, 0, 0, /* MKCL_HEADER1(closed) */
  (mkcl_object) &user_package_name, /* name */
  mk_cl_Cnil, /* nicknames */
  mk_cl_Cnil, /* shadowings */
  mk_cl_Cnil, /* uses */
  mk_cl_Cnil, /* usedby */
  (mkcl_object) &internal_ht, /* internal */
  (mkcl_object) &external_ht, /* external */
  PTHREAD_MUTEX_INITIALIZER /* lock */
};
