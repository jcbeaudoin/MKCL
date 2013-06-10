/* -*- mode: c -*- */
/*
    hash.d  -- Hash tables.
*/
/*
    Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
    Copyright (c) 1990, Giuseppe Attardi.
    Copyright (c) 2001, Juan Jose Garcia Ripoll.
    Copyright (c) 2010-2011, Jean-Claude Beaudoin 

    MKCL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 3 of the License, or (at your option) any later version.

    See file '../../Copyright' for full details.
*/

#include <mkcl/mkcl.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include <mkcl/mkcl-inl.h>
#include <mkcl/internal.h>
#include <mkcl/mkcl-fenv.h>
#include "newhash.h"

static void corrupted_hash(MKCL, mkcl_object hashtable) /*__attribute__((noreturn))*/;

#define SYMBOL_NAME(x) (mkcl_Null(x)? mk_cl_Cnil_symbol->symbol.name : (x)->symbol.name)

static void
corrupted_hash(MKCL, mkcl_object hashtable)
{
  mkcl_FEerror(env, "internal error, corrupted hashtable ~S", 1, hashtable);
}

/* These do a peek inside GMP privates, brittle at best. JCB */
#define mkcl_big_size	big_num->_mp_size
#define mkcl_big_limbs	big_num->_mp_d

#if __i386 || __x86_64
# define MKCL_LONG_DOUBLE_REAL_SIZE 10
#else
# define MKCL_LONG_DOUBLE_REAL_SIZE sizeof(long double)
#endif

static mkcl_hashkey
_hash_eql(mkcl_hashkey h, mkcl_object x)
{
  switch (mkcl_type_of(x)) {
  case mkcl_t_bignum:
    return hash_string(h, (unsigned char*)x->big.mkcl_big_limbs,
		       labs(x->big.mkcl_big_size) * sizeof(mp_limb_t));
  case mkcl_t_ratio:
    h = _hash_eql(h, x->ratio.num);
    return _hash_eql(h, x->ratio.den);
  case mkcl_t_singlefloat:
    return hash_string(h, (unsigned char*)&mkcl_single_float(x), sizeof(mkcl_single_float(x)));
  case mkcl_t_doublefloat:
    return hash_string(h, (unsigned char*)&mkcl_double_float(x), sizeof(mkcl_double_float(x)));
#ifdef MKCL_LONG_FLOAT
  case mkcl_t_longfloat:
    return hash_string(h, (unsigned char*)&mkcl_long_float(x), MKCL_LONG_DOUBLE_REAL_SIZE);
#endif
  case mkcl_t_complex:
    h = _hash_eql(h, x->_complex.real);
    return _hash_eql(h, x->_complex.imag);
  case mkcl_t_character:
    return hash_word(h, MKCL_CHAR_CODE(x));
  default:
    return hash_word(h, ((mkcl_hashkey)x >> 2));
  }
}

static mkcl_hashkey
_hash_equal(int depth, mkcl_hashkey h, mkcl_object x)
{
  switch (mkcl_type_of(x)) {
  case mkcl_t_cons:
    if (--depth == 0) {
      return h;
    } else {
      h = _hash_equal(depth, h, MKCL_CONS_CAR(x));
      return _hash_equal(depth, h, MKCL_CONS_CDR(x));
    }
  case mkcl_t_symbol:
    if (mkcl_Null(x)) {
      return _hash_equal(depth, h, mk_cl_Cnil_symbol->symbol.name);
    }
    else
      return _hash_equal(depth, h, x->symbol.name);
  case mkcl_t_base_string:
    return hash_base_string((mkcl_base_char *)x->base_string.self, x->base_string.fillp, h);
  case mkcl_t_string:
    return hash_full_string(x->string.self, x->string.fillp, h);
  case mkcl_t_pathname:
    h = _hash_equal(0, h, x->pathname.directory);
    h = _hash_equal(0, h, x->pathname.name);
    h = _hash_equal(0, h, x->pathname.type);
    h = _hash_equal(0, h, x->pathname.host);
    h = _hash_equal(0, h, x->pathname.device);
    return _hash_equal(0, h, x->pathname.version);
  case mkcl_t_bitvector:
    /* Notice that we may round out some bits. We must do this
     * because the fill pointer may be set in the middle of a byte.
     * If so, the extra bits _must_ _not_ take part in the hash,
     * because otherwise two bit arrays which are EQUAL might
     * have different hash keys. */
    return hash_string(h, x->vector.self.bc, x->vector.fillp / 8);
  case mkcl_t_random:
    return _hash_equal(0, h, x->random.value);
#ifdef MKCL_SIGNED_ZERO
  case mkcl_t_singlefloat: {
    float f = mkcl_single_float(x);
    /* if (f == 0.0) f = 0.0; */
    return hash_string(h, (unsigned char*)&f, sizeof(f));
  }
  case mkcl_t_doublefloat: {
    double f = mkcl_double_float(x);
    /* if (f == 0.0) f = 0.0; */
    return hash_string(h, (unsigned char*)&f, sizeof(f));
  }
#ifdef MKCL_LONG_FLOAT
  case mkcl_t_longfloat: {
    long double f = mkcl_long_float(x);
    /* if (f == 0.0) f = 0.0; */
    return hash_string(h, (unsigned char*)&f, MKCL_LONG_DOUBLE_REAL_SIZE);
  }
#endif
  case mkcl_t_complex: {
    h = _hash_equal(depth, h, x->_complex.real);
    return _hash_equal(depth, h, x->_complex.imag);
  }
#endif
  default:
    return _hash_eql(h, x);
  }
}

static mkcl_hashkey
_hash_equalp(MKCL, int depth, mkcl_hashkey h, mkcl_object x)
{
  mkcl_index i, len;

  if (mkcl_Null(x)) {
    return _hash_equalp(env, depth, h, mk_cl_Cnil_symbol->symbol.name);
  }

  switch (mkcl_type_of(x)) {
  case mkcl_t_character:
    return hash_word(h, mkcl_char_upcase(MKCL_CHAR_CODE(x)));
  case mkcl_t_cons:
    if (--depth == 0) {
      return h;
    } else {
      h = _hash_equalp(env, depth, h, MKCL_CONS_CAR(x));
      return _hash_equalp(env, depth, h, MKCL_CONS_CDR(x));
    }
  case mkcl_t_string:
    len = x->string.fillp;
    goto SCAN;
  case mkcl_t_base_string:
    len = x->base_string.fillp;
    goto SCAN;
  case mkcl_t_vector:
  case mkcl_t_bitvector:
    len = x->vector.fillp;
    goto SCAN;
  case mkcl_t_array:
    len = x->array.dim;
  SCAN:	if (--depth) {
      for (i = 0; i < len; i++) {
	h = _hash_equalp(env, depth, h, mkcl_aref_index(env, x, i));
      }
    }
    return h;
  case mkcl_t_fixnum:
    return hash_word(h, mkcl_fixnum_to_word(x));
  case mkcl_t_singlefloat:
    /* FIXME! We should be more precise here! */
    return hash_word(h, (mkcl_index)mkcl_single_float(x));
  case mkcl_t_doublefloat:
    /* FIXME! We should be more precise here! */
    return hash_word(h, (mkcl_index)mkcl_double_float(x));
  case mkcl_t_bignum:
    /* FIXME! We should be more precise here! */
    return _hash_equal(depth, h, x);
  case mkcl_t_ratio:
    h = _hash_equalp(env, 0, h, x->ratio.num);
    return _hash_equalp(env, 0, h, x->ratio.den);
  case mkcl_t_complex:
    h = _hash_equalp(env, 0, h, x->_complex.real);
    return _hash_equalp(env, 0, h, x->_complex.imag);
  case mkcl_t_instance:
  case mkcl_t_hashtable:
    /* FIXME! We should be more precise here! */
    return hash_word(h, 42); /* Is this the meaning of life by any chance? JCB */
  default:
    return _hash_equal(depth, h, x);
  }
}


struct mkcl_hashtable_entry *
mkcl_search_hash_eq(MKCL, mkcl_object key, mkcl_object hashtable)
{
  struct mkcl_hashtable_entry *e;
  mkcl_hashkey h;
  mkcl_index hsize = hashtable->hash.size;

#ifdef HASHTABLE_STATS
  hashtable->hash.nb_searches++;
#endif

  h = (mkcl_hashkey)key >> 2;

  /* A power of 2 for hsize would not be good here! */
  e = hashtable->hash.data[h % hsize]; 
  
  if ( e != NULL )
    {
#ifdef HASHTABLE_STATS
      long probes = 0;
#endif

      do {
#ifdef HASHTABLE_STATS
	probes++;
#endif

	if ( key == e->key )
	  {
#ifdef HASHTABLE_STATS
	    hashtable->hash.probes += probes;
	    if ( probes < hashtable->hash.shortest_probe_chain )
	      hashtable->hash.shortest_probe_chain = probes;
	    if ( hashtable->hash.longest_probe_chain < probes )
	      hashtable->hash.longest_probe_chain = probes;
#endif
	    return(e);
	  }
	else
	  e = e->next;
      } while ( e != NULL );
#ifdef HASHTABLE_STATS
      hashtable->hash.probes += probes;
      if ( probes < hashtable->hash.shortest_failed_probe_chain )
	hashtable->hash.shortest_failed_probe_chain = probes;
      if ( hashtable->hash.longest_failed_probe_chain < probes )
	hashtable->hash.longest_failed_probe_chain = probes;
#endif
      return(NULL); /* we got to the end of the chain without a match. */
    }
  else
    {
#ifdef HASHTABLE_STATS
      long probes = 1;
      hashtable->hash.probes += probes;
      if ( probes < hashtable->hash.shortest_failed_probe_chain )
	hashtable->hash.shortest_failed_probe_chain = probes;
      if ( hashtable->hash.longest_failed_probe_chain < probes )
	hashtable->hash.longest_failed_probe_chain = probes;
#endif
      
      return(NULL); /* The chain was empty */
    }
}

struct mkcl_hashtable_entry *
mkcl_search_hash_eql(MKCL, mkcl_object key, mkcl_object hashtable)
{
  struct mkcl_hashtable_entry *e;
  mkcl_hashkey h;
  mkcl_index hsize = hashtable->hash.size;

#ifdef HASHTABLE_STATS
  hashtable->hash.nb_searches++;
#endif

  h = _hash_eql(0, key); 

  /* A power of 2 for hsize would not be good here! */
  e = hashtable->hash.data[h % hsize]; 
  
  if ( e != NULL )
    {
#ifdef HASHTABLE_STATS
      long probes = 0;
#endif

      do {
#ifdef HASHTABLE_STATS
	probes++;
#endif
	if ( mkcl_eql(env, key, e->key) )
	  {
#ifdef HASHTABLE_STATS
	    hashtable->hash.probes += probes;
	    if ( probes < hashtable->hash.shortest_probe_chain )
	      hashtable->hash.shortest_probe_chain = probes;
	    if ( hashtable->hash.longest_probe_chain < probes )
	      hashtable->hash.longest_probe_chain = probes;
#endif
	    return(e);
	  }
	else
	  e = e->next;
      } while ( e != NULL );
#ifdef HASHTABLE_STATS
      hashtable->hash.probes += probes;
      if ( probes < hashtable->hash.shortest_failed_probe_chain )
	hashtable->hash.shortest_failed_probe_chain = probes;
      if ( hashtable->hash.longest_failed_probe_chain < probes )
	hashtable->hash.longest_failed_probe_chain = probes;
#endif
      return(NULL); /* we got to the end of the chain without a match. */
    }
  else
    {
#ifdef HASHTABLE_STATS
      long probes = 1;
      hashtable->hash.probes += probes;
      if ( probes < hashtable->hash.shortest_failed_probe_chain )
	hashtable->hash.shortest_failed_probe_chain = probes;
      if ( hashtable->hash.longest_failed_probe_chain < probes )
	hashtable->hash.longest_failed_probe_chain = probes;
#endif
      
      return(NULL); /* The chain was empty */
    }
}

struct mkcl_hashtable_entry *
mkcl_search_hash_equal(MKCL, mkcl_object key, mkcl_object hashtable)
{
  struct mkcl_hashtable_entry *e;
  mkcl_hashkey h;
  mkcl_index hsize = hashtable->hash.size;

#ifdef HASHTABLE_STATS
  hashtable->hash.nb_searches++;
#endif

  h = _hash_equal(3, 0, key);

  /* A power of 2 for hsize would not be good here! */
  e = hashtable->hash.data[h % hsize]; 
  
  if ( e != NULL )
    {
#ifdef HASHTABLE_STATS
      long probes = 0;
#endif

      do {
#ifdef HASHTABLE_STATS
	probes++;
#endif

	if ( mkcl_equal(env, key, e->key) )
	  {
#ifdef HASHTABLE_STATS
	    hashtable->hash.probes += probes;
	    if ( probes < hashtable->hash.shortest_probe_chain )
	      hashtable->hash.shortest_probe_chain = probes;
	    if ( hashtable->hash.longest_probe_chain < probes )
	      hashtable->hash.longest_probe_chain = probes;
#endif
	    return(e);
	  }
	else
	  e = e->next;
      } while ( e != NULL );
#ifdef HASHTABLE_STATS
      hashtable->hash.probes += probes;
      if ( probes < hashtable->hash.shortest_failed_probe_chain )
	hashtable->hash.shortest_failed_probe_chain = probes;
      if ( hashtable->hash.longest_failed_probe_chain < probes )
	hashtable->hash.longest_failed_probe_chain = probes;
#endif
      return(NULL); /* we got to the end of the chain without a match. */
    }
  else
    {
#ifdef HASHTABLE_STATS
      long probes = 1;
      hashtable->hash.probes += probes;
      if ( probes < hashtable->hash.shortest_failed_probe_chain )
	hashtable->hash.shortest_failed_probe_chain = probes;
      if ( hashtable->hash.longest_failed_probe_chain < probes )
	hashtable->hash.longest_failed_probe_chain = probes;
#endif
      
      return(NULL); /* The chain was empty */
    }
}

struct mkcl_hashtable_entry *
mkcl_search_hash_equalp(MKCL, mkcl_object key, mkcl_object hashtable)
{
  struct mkcl_hashtable_entry *e;
  mkcl_hashkey h;
  mkcl_index hsize = hashtable->hash.size;

#ifdef HASHTABLE_STATS
  hashtable->hash.nb_searches++;
#endif

  h = _hash_equalp(env, 3, 0, key);

  /* A power of 2 for hsize would not be good here! */
  e = hashtable->hash.data[h % hsize]; 
  
  if ( e != NULL )
    {
#ifdef HASHTABLE_STATS
      long probes = 0;
#endif

      do {
#ifdef HASHTABLE_STATS
	probes++;
#endif

	if ( mkcl_equalp(env, key, e->key) )
	  {
#ifdef HASHTABLE_STATS
	    hashtable->hash.probes += probes;
	    if ( probes < hashtable->hash.shortest_probe_chain )
	      hashtable->hash.shortest_probe_chain = probes;
	    if ( hashtable->hash.longest_probe_chain < probes )
	      hashtable->hash.longest_probe_chain = probes;
#endif
	    return(e);
	  }
	else
	  e = e->next;
      } while ( e != NULL );
#ifdef HASHTABLE_STATS
      hashtable->hash.probes += probes;
      if ( probes < hashtable->hash.shortest_failed_probe_chain )
	hashtable->hash.shortest_failed_probe_chain = probes;
      if ( hashtable->hash.longest_failed_probe_chain < probes )
	hashtable->hash.longest_failed_probe_chain = probes;
#endif
      return(NULL); /* we got to the end of the chain without a match. */
    }
  else
    {
#ifdef HASHTABLE_STATS
      long probes = 1;
      hashtable->hash.probes += probes;
      if ( probes < hashtable->hash.shortest_failed_probe_chain )
	hashtable->hash.shortest_failed_probe_chain = probes;
      if ( hashtable->hash.longest_failed_probe_chain < probes )
	hashtable->hash.longest_failed_probe_chain = probes;
#endif
      
      return(NULL); /* The chain was empty */
    }
}

struct mkcl_hashtable_entry *
mkcl_search_hash_package(MKCL, mkcl_object key, mkcl_object hashtable)
{
  struct mkcl_hashtable_entry *e;
  mkcl_hashkey h;
  mkcl_object ho;
  mkcl_index hsize = hashtable->hash.size;

#ifdef HASHTABLE_STATS
  hashtable->hash.nb_searches++;
#endif

  h = _hash_equal(3, 0, key);
  ho = MKCL_MAKE_FIXNUM(h & 0xFFFFFFF);

  /* A power of 2 for hsize would not be good here! */
  e = hashtable->hash.data[h % hsize]; 
  
  if ( e != NULL )
    {
#ifdef HASHTABLE_STATS
      long probes = 0;
#endif

      do {
#ifdef HASHTABLE_STATS
	probes++;
#endif

	if ( (ho == e->key) && mkcl_string_E(env, key,SYMBOL_NAME(e->value)) )
	  {
#ifdef HASHTABLE_STATS
	    hashtable->hash.probes += probes;
	    if ( probes < hashtable->hash.shortest_probe_chain )
	      hashtable->hash.shortest_probe_chain = probes;
	    if ( hashtable->hash.longest_probe_chain < probes )
	      hashtable->hash.longest_probe_chain = probes;
#endif
	    return(e);
	  }
	else
	  e = e->next;
      } while ( e != NULL );
#ifdef HASHTABLE_STATS
      hashtable->hash.probes += probes;
      if ( probes < hashtable->hash.shortest_failed_probe_chain )
	hashtable->hash.shortest_failed_probe_chain = probes;
      if ( hashtable->hash.longest_failed_probe_chain < probes )
	hashtable->hash.longest_failed_probe_chain = probes;
#endif
      return(NULL); /* we got to the end of the chain without a match. */
    }
  else
    {
#ifdef HASHTABLE_STATS
      long probes = 1;
      hashtable->hash.probes += probes;
      if ( probes < hashtable->hash.shortest_failed_probe_chain )
	hashtable->hash.shortest_failed_probe_chain = probes;
      if ( hashtable->hash.longest_failed_probe_chain < probes )
	hashtable->hash.longest_failed_probe_chain = probes;
#endif
      
      return(NULL); /* The chain was empty */
    }
}

#if 0 /* inlined */
/* The content of mkcl_search_hash is largely duplicated inside
   mkcl_remhash. If you change one you need to also change the other. */
struct mkcl_hashtable_entry *
mkcl_search_hash(mkcl_object key, mkcl_object hashtable)
{
  struct mkcl_hashtable_entry *e;
  mkcl_hashkey h;
  mkcl_object ho;
  mkcl_index hsize = hashtable->hash.size;
  enum mkcl_httest htest = hashtable->hash.test;

#ifdef HASHTABLE_STATS
  hashtable->hash.nb_searches++;
#endif

  switch (htest) {
  case mkcl_htt_eq:	  h = (mkcl_hashkey)key >> 2; break; /* you must be kidding! */
  case mkcl_htt_eql:	  h = _hash_eql(0, key); break;
  case mkcl_htt_equal:	  h = _hash_equal(3, 0, key); break;
  case mkcl_htt_equalp:  h = _hash_equalp(3, 0, key); break;
  case mkcl_htt_package: h = _hash_equal(3, 0, key);
    ho = MKCL_MAKE_FIXNUM(h & 0xFFFFFFF);
    break;
  default:	  corrupted_hash(hashtable);
  }

  /* A power of 2 for hsize would not be good here! */
  e = hashtable->hash.data[h % hsize]; 
  
  if ( e != NULL )
    {
#ifdef HASHTABLE_STATS
      long probes = 0;
#endif

      do {
	bool b;
	mkcl_object hkey = e->key;
#ifdef HASHTABLE_STATS
	probes++;
#endif

	switch (htest)
	  {
	  case mkcl_htt_eq:	b = (key == hkey); break;
	  case mkcl_htt_eql:	b = mkcl_eql(key, hkey); break;
	  case mkcl_htt_equal: b = mkcl_equal(key, hkey); break;
	  case mkcl_htt_equalp:  b = mkcl_equalp(key, hkey); break;
	  case mkcl_htt_package: 
	    b = (ho == hkey) && mkcl_string_E(key,SYMBOL_NAME(e->value));
	    break;
	  }
	if ( b )
	  {
#ifdef HASHTABLE_STATS
	    hashtable->hash.probes += probes;
	    if ( probes < hashtable->hash.shortest_probe_chain )
	      hashtable->hash.shortest_probe_chain = probes;
	    if ( hashtable->hash.longest_probe_chain < probes )
	      hashtable->hash.longest_probe_chain = probes;
#endif
	    return(e);
	  }
	else
	  e = e->next;
      } while ( e != NULL );
#ifdef HASHTABLE_STATS
      hashtable->hash.probes += probes;
      if ( probes < hashtable->hash.shortest_failed_probe_chain )
	hashtable->hash.shortest_failed_probe_chain = probes;
      if ( hashtable->hash.longest_failed_probe_chain < probes )
	hashtable->hash.longest_failed_probe_chain = probes;
#endif
      return(NULL); /* we got to the end of the chain without a match. */
    }
  else
    {
#ifdef HASHTABLE_STATS
      long probes = 1;
      hashtable->hash.probes += probes;
      if ( probes < hashtable->hash.shortest_failed_probe_chain )
	hashtable->hash.shortest_failed_probe_chain = probes;
      if ( hashtable->hash.longest_failed_probe_chain < probes )
	hashtable->hash.longest_failed_probe_chain = probes;
#endif
      
      return(NULL); /* The chain was empty */
    }
}
#else
extern inline struct mkcl_hashtable_entry *mkcl_search_hash(MKCL, mkcl_object key, mkcl_object hashtable);
#endif



mkcl_object
mkcl_gethash_safe(MKCL, mkcl_object key, mkcl_object hashtable, mkcl_object def)
{
  struct mkcl_hashtable_entry *e;

  mkcl_assert_type_hash_table(env, hashtable);
  e = mkcl_search_hash(env, key, hashtable);
  if (e != NULL)
    def = e->value;
  return def;
}

static void
add_new_to_hash(MKCL, mkcl_object key, mkcl_object hashtable, mkcl_object value)
{
  enum mkcl_httest htest;
  mkcl_hashkey h;
  mkcl_index hsize;
  struct mkcl_hashtable_entry ** root;

  /* INV: hashtable has the right type */
  htest = hashtable->hash.test;
  hsize = hashtable->hash.size;
  switch (htest) {
  case mkcl_htt_eq:	h = (mkcl_hashkey)key >> 2; break;
  case mkcl_htt_eql:	h = _hash_eql(0, key); break;
  case mkcl_htt_equal:	h = _hash_equal(3, 0, key); break;
  case mkcl_htt_equalp:  h = _hash_equalp(env, 3, 0, key); break;
  case mkcl_htt_package: h = _hash_equal(3, 0, key); break;
  default:	corrupted_hash(env, hashtable);
  }

  root = &(hashtable->hash.data[h % hsize]);
  
  {
    struct mkcl_hashtable_entry * e;
    struct mkcl_hashtable_entry * free_bucket = hashtable->hash.free_bucket;

    if (free_bucket)
      { e = free_bucket; hashtable->hash.free_bucket = e->next; /* e->next = NULL; */ }
    else
      e = (struct mkcl_hashtable_entry *)
	mkcl_alloc(env, sizeof(struct mkcl_hashtable_entry));

    hashtable->hash.entries++;

    if (htest == mkcl_htt_package)
      e->key = MKCL_MAKE_FIXNUM(h & 0xFFFFFFF);
    else
      e->key = key;
    e->value = value;
    e->next = *root;

    *root = e;
  }
}

void
mkcl_sethash(MKCL, mkcl_object key, mkcl_object hashtable, mkcl_object value)
{
  mkcl_index i;
  struct mkcl_hashtable_entry *e;

  mkcl_assert_type_hash_table(env, hashtable);
  e = mkcl_search_hash(env, key, hashtable);
  if (e != NULL) {
    e->value = value;
    return;
  }

  i = hashtable->hash.entries + 1;
  if (i >= hashtable->hash.size ||
      /* This version is all integral ops. */
      (!(i & (((mkcl_word) -1) << (MKCL_WORD_BITS - 4))) /* make sure i will not overflow. */
       && (i * 16) >= (hashtable->hash.size * hashtable->hash.factor_of_16th)))
    mkcl_extend_hashtable(env, hashtable);

  add_new_to_hash(env, key, hashtable, value);
  return;
}

void
mkcl_extend_hashtable(MKCL, mkcl_object hashtable)
{
  mkcl_object old, key;
  mkcl_index old_size, new_size, i;
  mkcl_object new_size_obj;

  mkcl_assert_type_hash_table(env, hashtable);
  old_size = hashtable->hash.size;
  /* We do the computation with lisp datatypes, just in case the sizes contain
   * weird numbers */
  if (MKCL_FIXNUMP(hashtable->hash.rehash_size)) {
    new_size_obj = mkcl_plus(env, hashtable->hash.rehash_size,
			     MKCL_MAKE_FIXNUM(old_size));
  } else {
    /* if we get here rehash_size is most likely a real (float) number. */
    bool fe_inexact_on = FE_INEXACT & fegetexcept();

    if (fe_inexact_on)
      {
	fedisableexcept(FE_INEXACT);
      }
    
    new_size_obj = mkcl_times(env, hashtable->hash.rehash_size, MKCL_MAKE_FIXNUM(old_size));
    new_size_obj = mkcl_ceiling1(env, new_size_obj);

    feclearexcept(FE_INEXACT); /* Clear leftovers from casting. */ /* should it be FE_ALL_EXCEPT? JCB*/
    if (fe_inexact_on)
      {
	feenableexcept(FE_INEXACT);
      }
  }
  if (!MKCL_FIXNUMP(new_size_obj)) {
    /* New size is too large */
    new_size = old_size * 2;
  } else {
    new_size = mkcl_fixnum_to_word(new_size_obj);
  }
  old = mkcl_alloc_raw_hashtable(env);

  old->hash = hashtable->hash; /* To copy entries, size, data. */

  hashtable->hash.entries = 0;
  hashtable->hash.size = new_size;
  hashtable->hash.data = (struct mkcl_hashtable_entry **)
    mkcl_alloc(env, new_size * sizeof(struct mkcl_hashtable_entry *));

  for (i = 0;  i < new_size;  i++) {
    hashtable->hash.data[i] = NULL;
  }

  for (i = 0;  i < old_size;  i++)
    {
      struct mkcl_hashtable_entry * e = old->hash.data[i];

      for (; e != NULL; e = e->next)
	{
	  if ( hashtable->hash.test == mkcl_htt_package )
	    key = SYMBOL_NAME(e->value);
	  else
	    key = e->key;
	  add_new_to_hash(env, key, hashtable, e->value);
	}
    }
}


@(defun make_hash_table (&key 
			 (test @'eql')
			 (size MKCL_MAKE_FIXNUM(1024))
			 (rehash_size mkcl_make_singlefloat(env, 1.5))
			 (rehash_threshold mkcl_make_singlefloat(env, 0.7))
			 )
@
  @(return mk_cl__make_hash_table(env, test, size, rehash_size, rehash_threshold));
@)

static void
do_clrhash(mkcl_object ht)
{
  /*
   * Fill a hash with null pointers and ensure it does not have
   * any entry. We separate this routine because it is needed
   * both by clrhash and hash table initialization.
   */
  mkcl_index i;
  const mkcl_index hsize = ht->hash.size;
  struct mkcl_hashtable_entry ** data = ht->hash.data;

  ht->hash.entries = 0;
  for(i = 0; i < hsize; i++) {
    data[i] = NULL;
  }
}

mkcl_object
mk_cl__make_hash_table(MKCL, mkcl_object test, mkcl_object size,
		       mkcl_object rehash_size, mkcl_object rehash_threshold)
{
  enum mkcl_httest htt;
  struct mkcl_hashtable_entry * (*search_fun)(MKCL, mkcl_object key, mkcl_object hashtable);  
  mkcl_index hsize;
  mkcl_object h;
  /*
   * Argument checking
   */
  if (test == @'eq' || test == MKCL_SYM_FUN(@'eq'))
    { htt = mkcl_htt_eq; search_fun = mkcl_search_hash_eq; }
  else if (test == @'eql' || test == MKCL_SYM_FUN(@'eql'))
    { htt = mkcl_htt_eql; search_fun = mkcl_search_hash_eql; }
  else if (test == @'equal' || test == MKCL_SYM_FUN(@'equal'))
    { htt = mkcl_htt_equal; search_fun = mkcl_search_hash_equal; }
  else if (test == @'equalp' || test == MKCL_SYM_FUN(@'equalp'))
    { htt = mkcl_htt_equalp; search_fun = mkcl_search_hash_equalp; }
  else
    mkcl_FEerror(env, "~S is an illegal hash-table test function.", 1, test);

  hsize = mkcl_fixnum_in_range(env, @'make-hash-table', "size", size, 0, MKCL_ATOTLIM);;
  if (hsize < 16) {
    hsize = 16;
  }
 AGAIN:
  if (!mkcl_plusp(env, rehash_size)) {
  ERROR1:
    rehash_size =
      mkcl_type_error(env, @'make-hash-table',"rehash-size",
		      rehash_size,
		      mkcl_fast_read_from_cstring(env, "(OR (INTEGER 1 *) (FLOAT (1.0) *))"));
    goto AGAIN;
  }
  if (mkcl_floatp(env, rehash_size)) {
    if (mkcl_number_compare(env, rehash_size, MKCL_MAKE_FIXNUM(1)) <= 0) {
      goto ERROR1;
    }
    rehash_size = mkcl_make_doublefloat(env, mkcl_to_double(env, rehash_size));
  } else if (!MKCL_FIXNUMP(rehash_size)) {
    goto ERROR1;
  }
  while (!mkcl_realp(env, rehash_threshold) ||
	 mkcl_minusp(env, rehash_threshold) ||
	 mkcl_number_compare(env, rehash_threshold, MKCL_MAKE_FIXNUM(1)) > 0)
    {
      rehash_threshold =
	mkcl_type_error(env, @'make-hash-table',"rehash-threshold",
			rehash_threshold,
			mkcl_fast_read_from_cstring(env, "(REAL 0 1)"));
    }
  /*
   * Build actual hash.
   */

  h = mkcl_alloc_raw_hashtable(env);
  h->hash.test = htt;
  h->hash.search_fun = search_fun;
  h->hash.size = hsize;
  h->hash.entries = 0;
  h->hash.data = NULL;	/* for GC sake */

  h->hash.data = (struct mkcl_hashtable_entry **) mkcl_alloc(env, hsize * sizeof(struct mkcl_hashtable_entry *));
  do_clrhash(h);

  h->hash.free_bucket = NULL;

  h->hash.rehash_size = rehash_size;
  h->hash.threshold = rehash_threshold;
  {
    fenv_t old_fenv;

    bool fe_inexact_on = FE_INEXACT & fegetexcept();
    if (fe_inexact_on)
      {
	/* printf("\nwrite_long_double: turning off FE_INEXACT!\n"); fflush(NULL); */
	fedisableexcept(FE_INEXACT);
      }

    /* fegetenv(&old_fenv); */ /* should this be feholdexcept() instead? JCB */
    /* fedisableexcept(FE_INEXACT); */

    mkcl_object factor = mkcl_round1(env,
				     mkcl_times(env,
						rehash_threshold,
						MKCL_MAKE_FIXNUM(16)));
    /* fesetenv(&old_fenv); */
    feclearexcept(FE_INEXACT); /* Clear leftovers from rounding. */
    if (fe_inexact_on)
      {
	/* printf("\nwrite_long_double: turning on FE_INEXACT!\n"); fflush(NULL); */
	feenableexcept(FE_INEXACT);
      }

    h->hash.factor_of_16th = mkcl_integer_to_index(env, factor);
  }
  if (h->hash.factor_of_16th == 0)
    h->hash.factor_of_16th = 1;


#ifdef HASHTABLE_STATS
  h->hash.nb_searches = 0;
  h->hash.probes = 0;
  h->hash.shortest_probe_chain = LONG_MAX;
  h->hash.longest_probe_chain = 0;
  h->hash.shortest_failed_probe_chain = LONG_MAX;
  h->hash.longest_failed_probe_chain = 0;
  h->hash.longest_static_chain = 0;

  mkcl_core.hashtables[htt] = mkcl_cons(h, mkcl_core.hashtables[htt]);
#endif

  return h;
}

static void
print_stats_for_hset(mkcl_object hset)
{
#ifdef HASHTABLE_STATS
  mkcl_loop_for_in(hset) {
    mkcl_object h = MKCL_CONS_CAR(hset);
    long chain_count[8] = { 0, 0, 0, 0, 0, 0, 0, 0 };

    printf("Hash table size = %d, entries = %d, alpha = %f\n",
	   h->hash.size, h->hash.entries, (float) h->hash.entries / (float)h->hash.size);
    printf("searches = %d\n", h->hash.nb_searches);
    printf("total probes = %d\n", h->hash.probes);
    printf("average probe chain = %f\n",
	   (float) h->hash.probes / (float) h->hash.nb_searches);
    printf("shortest probe chain = %d\n", h->hash.shortest_probe_chain);
    printf("longest probe chain = %d\n", h->hash.longest_probe_chain);
    printf("shortest failed probe chain = %d\n", h->hash.shortest_failed_probe_chain);
    printf("longest failed probe chain = %d\n", h->hash.longest_failed_probe_chain);

    {
      long size = h->hash.size;
      long i;

      for (i = 0; i < size; i++)
	{
	  long count = 0;
	  struct mkcl_hashtable_entry * e = h->hash.data[i];
	  
	  for (; e != NULL; e = e->next)
	    count++;

	  if ( h->hash.longest_static_chain < count )
	    h->hash.longest_static_chain = count;

	  if ( count >= 7 )
	    chain_count[7]++;
	  else
	    chain_count[count]++;
	}
    }
    printf("longest static chain = %d\n", h->hash.longest_static_chain);
    if ( h->hash.longest_static_chain > 2 )
      {
	printf("\tchains of length 0: %d\n", chain_count[0]);
	printf("\tchains of length 1: %d\n", chain_count[1]);
	printf("\tchains of length 2: %d\n", chain_count[2]);
	printf("\tchains of length 3: %d\n", chain_count[3]);
	printf("\tchains of length 4: %d\n", chain_count[4]);
	printf("\tchains of length 5: %d\n", chain_count[5]);
	printf("\tchains of length 6: %d\n", chain_count[6]);
	printf("\tchains of length 7: %d\n", chain_count[7]);
      }
    printf("\n");
  } mkcl_end_loop_for_in;
#endif
}

mkcl_object
mk_si_hash_tables_statistics(MKCL)
{
  mkcl_call_stack_check(env);
#ifdef HASHTABLE_STATS
  printf("\nHash tables of type EQ:\n");
  print_stats_for_hset(mkcl_core.hashtables[mkcl_htt_eq]);
  printf("\nHash tables of type EQL:\n");
  print_stats_for_hset(mkcl_core.hashtables[mkcl_htt_eql]);
  printf("\nHash tables of type EQUAL:\n");
  print_stats_for_hset(mkcl_core.hashtables[mkcl_htt_equal]);
  printf("\nHash tables of type EQUALP:\n");
  print_stats_for_hset(mkcl_core.hashtables[mkcl_htt_equalp]);
  printf("\nHash tables of type PACKAGE:\n");
  print_stats_for_hset(mkcl_core.hashtables[mkcl_htt_package]);
#else
  printf("\nThere is no hashtable statistics!\n");
#endif
  @(return mk_cl_Cnil);
}

mkcl_object
mk_cl_hash_table_p(MKCL, mkcl_object ht)
{
  mkcl_call_stack_check(env);
  @(return ((mkcl_type_of(ht) == mkcl_t_hashtable) ? mk_cl_Ct : mk_cl_Cnil));
}

@(defun gethash (key ht &optional (no_value mk_cl_Cnil))
	struct mkcl_hashtable_entry *e;
@
  mkcl_assert_type_hash_table(env, ht);
  e = mkcl_search_hash(env, key, ht);
  if (e != NULL)
    { @(return e->value mk_cl_Ct); }
  else
    { @(return no_value mk_cl_Cnil); }
@)

mkcl_object
mk_si_hash_set(MKCL, mkcl_object key, mkcl_object ht, mkcl_object val)
{
  mkcl_call_stack_check(env);
  /* INV: mkcl_sethash() checks the type of hashtable */
  mkcl_sethash(env, key, ht, val);
  @(return val);
}

/* The content of mkcl_search_hash is largely duplicated inside
   mkcl_remhash. If you change one you need to also change the other. */
bool
mkcl_remhash(MKCL, mkcl_object key, mkcl_object hashtable)
{
  mkcl_hashkey h;
  mkcl_object ho;
  struct mkcl_hashtable_entry * e;
  struct mkcl_hashtable_entry ** root;
  mkcl_index hsize;
  enum mkcl_httest htest;

  mkcl_assert_type_hash_table(env, hashtable);

  hsize = hashtable->hash.size;
  htest = hashtable->hash.test;

#ifdef HASHTABLE_STATS
  hashtable->hash.nb_searches++;
#endif

  switch (htest) {
  case mkcl_htt_eq:	  h = (mkcl_hashkey)key >> 2; break; /* you must be kidding! */
  case mkcl_htt_eql:	  h = _hash_eql(0, key); break;
  case mkcl_htt_equal:	  h = _hash_equal(3, 0, key); break;
  case mkcl_htt_equalp:  h = _hash_equalp(env, 3, 0, key); break;
  case mkcl_htt_package: h = _hash_equal(3, 0, key);
    ho = MKCL_MAKE_FIXNUM(h & 0xFFFFFFF);
    break;
  default:	  corrupted_hash(env, hashtable);
  }

  /* A power of 2 for hsize would not be good here! */
  root = &(hashtable->hash.data[h % hsize]);
  e = *root;
  
  if ( e != NULL )
    {
      bool b;
#ifdef HASHTABLE_STATS
      long probes = 0;
#endif

      do {
	mkcl_object hkey = e->key;

#ifdef HASHTABLE_STATS
	probes++;
#endif

	switch (htest)
	  {
	  case mkcl_htt_eq:	b = key == hkey; break;
	  case mkcl_htt_eql:	b = mkcl_eql(env, key, hkey); break;
	  case mkcl_htt_equal: b = mkcl_equal(env, key, hkey); break;
	  case mkcl_htt_equalp:  b = mkcl_equalp(env, key, hkey); break;
	  case mkcl_htt_package: 
	    b = (ho == hkey) && mkcl_string_E(env, key,SYMBOL_NAME(e->value));
	    break;
	  }
	if ( b )
	  {
	    *root = e->next;
	    hashtable->hash.entries--;

	    /* push the now removed entry into the free bucket. */
	    e->next = hashtable->hash.free_bucket;
	    e->key = MKCL_OBJNULL;
	    e->value = MKCL_OBJNULL;
	    hashtable->hash.free_bucket = e;

#ifdef HASHTABLE_STATS
	    hashtable->hash.probes += probes;
	    if ( probes < hashtable->hash.shortest_probe_chain )
	      hashtable->hash.shortest_probe_chain = probes;
	    if ( hashtable->hash.longest_probe_chain < probes )
	      hashtable->hash.longest_probe_chain = probes;
#endif

	    return(TRUE);
	  }
	else
	  { root = &(e->next); e = *root; }
      } while ( e != NULL );

#ifdef HASHTABLE_STATS
      hashtable->hash.probes += probes;
      if ( probes < hashtable->hash.shortest_failed_probe_chain )
	hashtable->hash.shortest_failed_probe_chain = probes;
      if ( hashtable->hash.longest_failed_probe_chain < probes )
	hashtable->hash.longest_failed_probe_chain = probes;
#endif

      return(FALSE); /* we got to the end of the chain without a match. */
    }
  else
    {
#ifdef HASHTABLE_STATS
      long probes = 1;
      hashtable->hash.probes += probes;
      if ( probes < hashtable->hash.shortest_failed_probe_chain )
	hashtable->hash.shortest_failed_probe_chain = probes;
      if ( hashtable->hash.longest_failed_probe_chain < probes )
	hashtable->hash.longest_failed_probe_chain = probes;
#endif

      return(FALSE); /* The chain was empty */
    }
}

mkcl_object
mk_cl_remhash(MKCL, mkcl_object key, mkcl_object ht)
{
  mkcl_call_stack_check(env);
  /* INV: mkcl_search_hash() checks the type of hashtable */
  @(return (mkcl_remhash(env, key, ht)? mk_cl_Ct : mk_cl_Cnil));
}

mkcl_object
mk_cl_clrhash(MKCL, mkcl_object ht)
{
  mkcl_call_stack_check(env);
  mkcl_assert_type_hash_table(env, ht);
  if (ht->hash.entries) {
    do_clrhash(ht);
  }
  @(return ht);
}

mkcl_object
mk_cl_hash_table_test(MKCL, mkcl_object ht)
{
  mkcl_call_stack_check(env);
  mkcl_object output;
  mkcl_assert_type_hash_table(env, ht);
  switch(ht->hash.test) {
  case mkcl_htt_eq: output = @'eq'; break;
  case mkcl_htt_eql: output = @'eql'; break;
  case mkcl_htt_equal: output = @'equal'; break;
  case mkcl_htt_equalp: output = @'equalp'; break;
  case mkcl_htt_package:
  default: output = @'equal';
  }
  @(return output);
}

mkcl_object
mk_cl_hash_table_size(MKCL, mkcl_object ht)
{
  mkcl_call_stack_check(env);
  mkcl_assert_type_hash_table(env, ht);
  @(return MKCL_MAKE_FIXNUM(ht->hash.size));
}

mkcl_object
mk_cl_hash_table_count(MKCL, mkcl_object ht)
{
  mkcl_call_stack_check(env);
  mkcl_assert_type_hash_table(env, ht);
  @(return (MKCL_MAKE_FIXNUM(ht->hash.entries)));
}

static mkcl_object
mk_si_hash_table_iterate(MKCL, mkcl_narg narg)
{
  mkcl_object closure_display = env->function->cclosure.cenv;
  mkcl_object chain_index = closure_display->display.level[0]->lblock.var[2];
  mkcl_object index = closure_display->display.level[0]->lblock.var[1];
  mkcl_object ht = closure_display->display.level[0]->lblock.var[0];

  mkcl_call_stack_check(env);
  if (!mkcl_Null(chain_index)) {
    mkcl_word i = mkcl_fixnum_to_word(chain_index);
    mkcl_word j = mkcl_fixnum_to_word(index);
	  
    if (i < 0)
      { i = 0; }

    for (; i < ht->hash.size; i++, j = -1) {
      mkcl_word k = 0;
      struct mkcl_hashtable_entry * e = ht->hash.data[i];
	    
      for (; k <= j && e != NULL; k++, e = e->next);
	    
      for (; e != NULL; k++, e = e->next)
	if (e->key != MKCL_OBJNULL) {
	  mkcl_object chain_ndx = MKCL_MAKE_FIXNUM(i);
	  mkcl_object ndx = MKCL_MAKE_FIXNUM(k);
	  closure_display->display.level[0]->lblock.var[2] = chain_ndx;
	  closure_display->display.level[0]->lblock.var[1] = ndx;
	  @(return mk_cl_Ct e->key e->value);
	}
    }
    closure_display->display.level[0]->lblock.var[2] = mk_cl_Cnil;
  }
  @(return mk_cl_Cnil mk_cl_Cnil mk_cl_Cnil);
}

mkcl_object
mk_si_hash_table_iterator(MKCL, mkcl_object ht)
{
  mkcl_call_stack_check(env);
  mkcl_assert_type_hash_table(env, ht);

  {
    mkcl_object closure_block = mkcl_alloc_clevel_block(env, mk_cl_Cnil, mk_cl_Cnil, 3);
    mkcl_object closure_syms_block = mkcl_alloc_clevel_block(env, mk_cl_Cnil, mk_cl_Cnil, 3);

    closure_block->lblock.producer = mk_cl_Cnil;
    closure_block->lblock.var[0] = ht;
    closure_block->lblock.var[1] = MKCL_MAKE_FIXNUM(-1);
    closure_block->lblock.var[2] = MKCL_MAKE_FIXNUM(-1);

    closure_syms_block->lblock.producer = mk_cl_Cnil;
    closure_syms_block->lblock.var[0] = mkcl_make_simple_base_string(env, "si::htable");
    closure_syms_block->lblock.var[1] = mkcl_make_simple_base_string(env, "si::bin-nth");
    closure_syms_block->lblock.var[2] = mkcl_make_simple_base_string(env, "si::index");

    @(return mkcl_make_cclosure_va(env, mk_cl_Cnil, (mkcl_objectfn)mk_si_hash_table_iterate, 1,
				   closure_syms_block, closure_block,
				   @'si::hash-table-iterator', -1));
  }
}

mkcl_object
mk_cl_hash_table_rehash_size(MKCL, mkcl_object ht)
{
  mkcl_call_stack_check(env);
  mkcl_assert_type_hash_table(env, ht);
  @(return ht->hash.rehash_size);
}

mkcl_object
mk_cl_hash_table_rehash_threshold(MKCL, mkcl_object ht)
{
  mkcl_call_stack_check(env);
  mkcl_assert_type_hash_table(env, ht);
  @(return ht->hash.threshold);
}

mkcl_object
mk_cl_sxhash(MKCL, mkcl_object key)
{
  mkcl_call_stack_check(env);
  mkcl_index output = _hash_equal(3, 0, key);
  const mkcl_index mask = ((mkcl_index)1 << (MKCL_WORD_BITS - 3)) - 1;
  @(return MKCL_MAKE_FIXNUM(output & mask));
}

@(defun si::hash-eql (&rest args)
  mkcl_index h;
@
  for (h = 0; narg; narg--) {
    mkcl_object o = mkcl_va_arg(args);
    h = _hash_eql(h, o);
  }
  @(return MKCL_MAKE_FIXNUM(h))
@)

@(defun si::hash-equal (&rest args)
  mkcl_index h;
@
  for (h = 0; narg; narg--) {
    mkcl_object o = mkcl_va_arg(args);
    h = _hash_equal(3, h, o);
  }
  @(return MKCL_MAKE_FIXNUM(h))
@)

@(defun si::hash-equalp (&rest args)
  mkcl_index h;
@
  for (h = 0; narg; narg--) {
    mkcl_object o = mkcl_va_arg(args);
    h = _hash_equalp(env, 3, h, o);
  }
  @(return MKCL_MAKE_FIXNUM(h));
@)

mkcl_object
mk_cl_maphash(MKCL, mkcl_object fun, mkcl_object ht)
{
  mkcl_index i;

  mkcl_call_stack_check(env);
  mkcl_assert_type_hash_table(env, ht);
  for (i = 0;  i < ht->hash.size;  i++) {
    struct mkcl_hashtable_entry * e = ht->hash.data[i];

    for (; e != NULL; e = e->next)
      if(e->key != MKCL_OBJNULL)
	mkcl_funcall2(env, fun, e->key, e->value);
  }
  @(return mk_cl_Cnil);
}

static struct mkcl_hashtable_entry *
copy_hash_table_chain(MKCL, struct mkcl_hashtable_entry * chain)
{
  struct mkcl_hashtable_entry * root = NULL;

  if ( chain != NULL )
    {
      struct mkcl_hashtable_entry * head;
      head = (struct mkcl_hashtable_entry *) 
	mkcl_alloc(env, sizeof(struct mkcl_hashtable_entry));

      head->next = NULL;
      head->key = chain->key;
      head->value = chain->value;

      root = head;

      chain = chain->next;

      for (; chain != NULL; chain = chain->next)
	{
	  struct mkcl_hashtable_entry * this =
	    (struct mkcl_hashtable_entry *) 
	    mkcl_alloc(env, sizeof(struct mkcl_hashtable_entry));

	  this->next = NULL;
	  this->key = chain->key;
	  this->value = chain->value;

	  head->next = this;
	  head = this;
	}
    }

  return(root);
}

mkcl_object
mk_si_copy_hash_table(MKCL, mkcl_object orig)
{
  mkcl_object hash;
  long i, size;

  mkcl_call_stack_check(env);
  hash = mk_cl__make_hash_table(env, mk_cl_hash_table_test(env, orig),
				mk_cl_hash_table_size(env, orig),
				mk_cl_hash_table_rehash_size(env, orig),
				mk_cl_hash_table_rehash_threshold(env, orig));
  size = orig->hash.size;
  for (i = 0; i < size; i++)
    hash->hash.data[i] = copy_hash_table_chain(env, orig->hash.data[i]);
  hash->hash.entries = orig->hash.entries;

  @(return hash);
}
