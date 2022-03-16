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

#define SYMBOL_NAME(it) {				\
    (int8_t)mkcl_t_base_string, 0, FALSE, FALSE,	\
      mk_cl_Cnil,					\
      (sizeof(it)-1),					\
      (sizeof(it)-1),					\
      (it),						\
      NULL, NULL,					\
      }

struct mkcl_base_string const mkcl_ffi_external_symbol_names[] = {
  SYMBOL_NAME("DEF-CONSTANT"),
  SYMBOL_NAME("FOREIGN"),
  SYMBOL_NAME("DEF-FOREIGN-TYPE"),
  SYMBOL_NAME("DEF-TYPE"),
  SYMBOL_NAME("SIZE-OF-FOREIGN-TYPE"),
  SYMBOL_NAME("ALLOCATE-FOREIGN-OBJECT"),
  SYMBOL_NAME("FREE-FOREIGN-OBJECT"),
  SYMBOL_NAME("DEF-ENUM"),
  SYMBOL_NAME("DEF-STRUCT"),
  SYMBOL_NAME("GET-SLOT-VALUE"),
  SYMBOL_NAME("GET-SLOT-POINTER"),
  SYMBOL_NAME("DEF-ARRAY-POINTER"),
  SYMBOL_NAME("DEREF-ARRAY"),
  SYMBOL_NAME("DEF-UNION"),
  SYMBOL_NAME("POINTER-ADDRESS"),
  SYMBOL_NAME("DEREF-POINTER"),
  SYMBOL_NAME("MAKE-NULL-POINTER"),
  SYMBOL_NAME("MAKE-POINTER"),
  SYMBOL_NAME("NULL-CHAR-P"),
  SYMBOL_NAME("ENSURE-CHAR-CHARACTER"),
  SYMBOL_NAME("ENSURE-CHAR-INTEGER"),
  SYMBOL_NAME("ENSURE-CHAR-STORABLE"),
  SYMBOL_NAME("CHAR-ARRAY-TO-POINTER"),
  SYMBOL_NAME("CONVERT-FROM-CSTRING"),
  SYMBOL_NAME("CONVERT-TO-CSTRING"),
  SYMBOL_NAME("FREE-CSTRING"),
  SYMBOL_NAME("WITH-CSTRING"),
  SYMBOL_NAME("WITH-CSTRINGS"),
  SYMBOL_NAME("FOREIGN-STRING-LENGTH"),
  SYMBOL_NAME("CONVERT-FROM-FOREIGN-STRING"),
  SYMBOL_NAME("CONVERT-TO-FOREIGN-STRING"),
  SYMBOL_NAME("ALLOCATE-FOREIGN-STRING"),
  SYMBOL_NAME("WITH-FOREIGN-STRING"),
  SYMBOL_NAME("WITH-FOREIGN-STRINGS"),
  SYMBOL_NAME("WITH-FOREIGN-OBJECT"),
  SYMBOL_NAME("WITH-FOREIGN-OBJECTS"),
  SYMBOL_NAME("WITH-CAST-POINTER"),
  SYMBOL_NAME("DEF-FUNCTION"),
  SYMBOL_NAME("C-INLINE"),
  SYMBOL_NAME("DEF-FOREIGN-VAR"),
  SYMBOL_NAME("FIND-FOREIGN-LIBRARY"),
  SYMBOL_NAME("LOAD-FOREIGN-LIBRARY"),
  SYMBOL_NAME("DEFCALLBACK"),
  SYMBOL_NAME("CALLBACK"),
  SYMBOL_NAME("CLINES"),
  SYMBOL_NAME("DEFINLINE"),
  SYMBOL_NAME("DEFCBODY"),
  SYMBOL_NAME("DEFLA"),
  SYMBOL_NAME("DEFENTRY"),
  SYMBOL_NAME("DEF-ARRAY"),
  SYMBOL_NAME("+NULL-CSTRING-POINTER+"),
};


struct mkcl_base_string const mkcl_ffi_internal_symbol_names[] = {
  SYMBOL_NAME("FOREIGN-ELT-TYPE-P"),
  SYMBOL_NAME("%CONVERT-TO-FFI-TYPE"),
  SYMBOL_NAME("%ALIGN-DATA"),
  SYMBOL_NAME("%FOREIGN-DATA-SET"),
  SYMBOL_NAME("%FOREIGN-DATA-REF"),
  SYMBOL_NAME("SLOT-POSITION"),
  SYMBOL_NAME("MAP-NAME-FROM-C-TO-LISP"),
  SYMBOL_NAME("%CONVERT-TO-ARG-TYPE"),
  SYMBOL_NAME("%CONVERT-TO-RETURN-TYPE"),
  SYMBOL_NAME("PRODUCE-FUNCTION-CALL"),
  SYMBOL_NAME("DEF-LIB-FUNCTION"),
  SYMBOL_NAME("C-FUN"),
  SYMBOL_NAME("FFI-FOREIGN-VAR"),
  SYMBOL_NAME("P"),
  SYMBOL_NAME("DO-LOAD-FOREIGN-LIBRARY"),
  SYMBOL_NAME("DEF-INLINE"),
  SYMBOL_NAME("*FFI-TYPES*"),
  SYMBOL_NAME("*REFERENCED-LIBRARIES*"),
};


#define internal_count MKCL_NB_ELEMS(mkcl_ffi_internal_symbol_names)
#define external_count MKCL_NB_ELEMS(mkcl_ffi_external_symbol_names)
#define internal_size 31
#define external_size 73

static struct mkcl_hashtable_entry internal_entries[];
static struct mkcl_hashtable_entry external_entries[];

static struct mkcl_hashtable_entry * internal_vector[];
static struct mkcl_hashtable_entry * external_vector[];

static struct mkcl_singlefloat rehash_size_factor = { mkcl_t_singlefloat, 0, 0, 0, 1.5f };
static struct mkcl_singlefloat rehash_threshold = { mkcl_t_singlefloat, 0, 0, 0, 0.75f };

static struct mkcl_hashtable internal_ht = {
  mkcl_t_hashtable, 0, mkcl_htt_package, 0, /* MKCL_HEADER2(test,lockable) */
  internal_vector, /* data */
  NULL, /* search_fun */
  NULL, /* hash_fun */
  NULL, /* equality_fun */
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
  NULL, /* search_fun */
  NULL, /* hash_fun */
  NULL, /* equality_fun */
  external_count, /* entries */
  external_size, /* size */
  (mkcl_object) &rehash_size_factor, /* rehash_size */
  (mkcl_object) &rehash_threshold, /* threshold */
  12, /* factor_of_16th */
  NULL /* free_bucket */
};

static struct mkcl_base_string ffi_package_name = SYMBOL_NAME("FFI");

struct mkcl_package mkcl_package_ffi = {
  mkcl_t_package, 0, 0, 0, /* MKCL_HEADER1(closed) */
  (mkcl_object) &ffi_package_name, /* name */
  mk_cl_Cnil, /* nicknames */
  mk_cl_Cnil, /* shadowings */
  mk_cl_Cnil, /* uses */
  mk_cl_Cnil, /* usedby */
  (mkcl_object) &internal_ht, /* internal */
  (mkcl_object) &external_ht, /* external */
  PTHREAD_MUTEX_INITIALIZER /* lock */
};
