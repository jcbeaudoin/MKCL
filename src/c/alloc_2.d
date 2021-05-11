/* -*- mode: c -*- */
/*
    alloc_2.c -- Memory allocation based on the Boehmn GC.
*/
/*
    Copyright (c) 2001, Juan Jose Garcia Ripoll.
    Copyright (c) 2011-2016,2021, Jean-Claude Beaudoin.

    MKCL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 3 of the License, or (at your option) any later version.

    See file '../../Copyright' for full details.
*/


#include <mkcl/mkcl.h>
#include <mkcl/internal.h>
#include <mkcl/mkcl-gc.h>
#include <mkcl/mkcl-inl.h>

#if MKCL_UNIX
# include <sys/mman.h> /* For mprotect() */
#endif

#include <stdlib.h> /* for access to native malloc */
#include <stdio.h> /* mainly to support debug messages. */



#define MKCL_STATS 1 /* Hardcoded just for now. JCB */

#if MKCL_STATS

struct mkcl_alloc_stats
{
  size_t bignum;
  size_t ratio;
  size_t singlefloat;
  size_t doublefloat;
  size_t longfloat;
  size_t complex;
  size_t symbol;
  size_t cons;
  size_t package;
  size_t array;
  size_t vector;
  size_t string;
  size_t base_string;
  size_t bitvector;
  size_t cfun;
  size_t cclosure;
  size_t bytecode;
  size_t bclosure;
  size_t instance;
  size_t hashtable;
  size_t stream;
  size_t random;
  size_t readtable;
  size_t pathname;
  size_t structure;
  size_t thread;
  size_t lock;
  size_t rwlock;
  size_t semaphore;
  size_t condition_variable;
  size_t codeblock;
  size_t foreign;
  /* size_t frame; */ /* always allocated as a C automatic variable, thus strictly of dynamic extent. JCB */
  size_t cdisplay;
  size_t clevel_block;
  size_t UTF_8;
  size_t UTF_16;
  size_t process;
};

# define COLLECT_STATS(env, type) (env->alloc->type++)
#else
# define COLLECT_STATS(env, type)
#endif

static void mkcl_count_GC_collections(void);

#if MKCL_WINDOWS
static CRITICAL_SECTION oom_handler_lock;

#define OUT_OF_MEMORY_LOCK() EnterCriticalSection(&oom_handler_lock)
#define OUT_OF_MEMORY_UNLOCK() LeaveCriticalSection(&oom_handler_lock)
#elif MKCL_PTHREADS
static pthread_mutex_t oom_handler_lock;

#define OUT_OF_MEMORY_LOCK() if (pthread_mutex_lock(&oom_handler_lock)) mkcl_lose(env, "Failed in OUT_OF_MEMORY_LOCK()")
#define OUT_OF_MEMORY_UNLOCK() if (pthread_mutex_unlock(&oom_handler_lock)) mkcl_lose(env, "Failed in OUT_OF_MEMORY_UNLOCK()")
#else
# error Incomplete definition of OUT_OF_MEMORY_LOCK().
#endif


/**********************************************************
 *		OBJECT ALLOCATION			  *
 **********************************************************/

mkcl_object
mk_si_set_heap_size_limit(MKCL, mkcl_object size_limit) /* This function should acquire the OOM lock. */
{
  mkcl_index the_size_limit = mkcl_integer_to_index(env, size_limit);
  mkcl_interrupt_status old_intr;

  mkcl_get_interrupt_status(env, &old_intr);
  mkcl_disable_interrupts(env);
  OUT_OF_MEMORY_LOCK();
  MK_GC_set_max_heap_size(mkcl_core.max_heap_size = the_size_limit);
  OUT_OF_MEMORY_UNLOCK();
  mkcl_set_interrupt_status(env, &old_intr);
  if (the_size_limit == 0) {
    mkcl_index size = mkcl_get_option(MKCL_OPT_HEAP_SAFETY_AREA);
    mkcl_core.safety_region = mkcl_alloc_atomic(env, size);
  } else if (mkcl_core.safety_region) {
    MKCL_GC_NO_INTR(env, MK_GC_FREE(mkcl_core.safety_region));
    mkcl_core.safety_region = NULL;
  }
  mkcl_return_value(size_limit);
}

mkcl_object
mk_si_get_heap_size_limit(MKCL) /* This function should acquire the OOM lock. */
{
  mkcl_return_value(mkcl_make_unsigned_integer(env, mkcl_core.max_heap_size));
}

static void
no_warnings(char *msg, MK_GC_word arg)
{
}



static void grow_memory(MKCL)
{
  mkcl_interrupt_status old_intr;

  mkcl_get_interrupt_status(env, &old_intr);
  mkcl_disable_interrupts(env);

  OUT_OF_MEMORY_LOCK();

  if (mkcl_core.max_heap_size == 0) {
    /* We did not set any limit in the amount of memory,
     * yet we failed, or we had some limits but we have
     * not reached them. */
    OUT_OF_MEMORY_UNLOCK();
    mkcl_set_interrupt_status(env, &old_intr);
    if (mkcl_core.safety_region) {
      /* We can free our safety region and hope it will be enough to let us signal a storage-exhausted exception */
      MK_GC_FREE(mkcl_core.safety_region);
      mkcl_core.safety_region = NULL;
      /* env->string_pool = mk_cl_Cnil; */
      mk_cl_error(env, 1, MK_MKCL_storage_exhausted);
    } else {
      /* No possibility of continuing */
      mkcl_lose(env, "Memory exhausted, quitting program.");
    }
  } else {
    static const mkcl_base_string_object(extend_str_obj, "Extend heap");

    mkcl_core.max_heap_size += mkcl_get_option(MKCL_OPT_HEAP_SAFETY_AREA);
    MK_GC_set_max_heap_size(mkcl_core.max_heap_size);
    OUT_OF_MEMORY_UNLOCK();
    mkcl_set_interrupt_status(env, &old_intr);
    mk_cl_cerror(env, 2, (mkcl_object) &extend_str_obj, MK_MKCL_storage_exhausted); /* Ask for extension */
  }

  mkcl_disable_interrupts(env);
  OUT_OF_MEMORY_LOCK();
  MK_GC_set_max_heap_size(mkcl_core.max_heap_size += mkcl_core.max_heap_size / 2);
  OUT_OF_MEMORY_UNLOCK();
  mkcl_set_interrupt_status(env, &old_intr);
}

static inline void * MKCL_GC_MALLOC(MKCL, mkcl_index size)
{
  void * new;

  MKCL_GC_NO_INTR(env, new = MK_GC_MALLOC(size));

  if (mkcl_likely(new != NULL))
    return new;
  else
    {
      grow_memory(env);
      MKCL_GC_NO_INTR(env, new = MK_GC_MALLOC(size));
      if (mkcl_likely(new != NULL))
	return new;
      else
	mkcl_lose(env, "Memory exhausted, quitting program.");
    }
}

static inline void * MKCL_GC_MALLOC_ATOMIC(MKCL, mkcl_index size)
{
  void * new;

  MKCL_GC_NO_INTR(env, new = MK_GC_MALLOC_ATOMIC(size));

  if (mkcl_likely(new != NULL))
    return new;
  else
    {
      grow_memory(env);
      MKCL_GC_NO_INTR(env, new = MK_GC_MALLOC_ATOMIC(size));
      if (mkcl_likely(new != NULL))
	return new;
      else
	mkcl_lose(env, "Memory exhausted, quitting program.");
    }
}

static inline void * MKCL_GC_MALLOC_UNCOLLECTABLE(MKCL, mkcl_index size)
{
  void * new;

  MKCL_GC_NO_INTR(env, new = MK_GC_MALLOC_UNCOLLECTABLE(size));

  if (mkcl_likely(new != NULL))
    return new;
  else
    {
      grow_memory(env);
      MKCL_GC_NO_INTR(env, new = MK_GC_MALLOC_UNCOLLECTABLE(size));
      if (mkcl_likely(new != NULL))
	return new;
      else
	mkcl_lose(env, "Memory exhausted, quitting program.");
    }
}

static inline void * MKCL_GC_MALLOC_IGNORE_OFF_PAGE(MKCL, mkcl_index size)
{
  void * new;

  MKCL_GC_NO_INTR(env, new = MK_GC_MALLOC_IGNORE_OFF_PAGE(size));

  if (mkcl_likely(new != NULL))
    return new;
  else
    {
      grow_memory(env);
      MKCL_GC_NO_INTR(env, new = MK_GC_MALLOC_IGNORE_OFF_PAGE(size));
      if (mkcl_likely(new != NULL))
	return new;
      else
	mkcl_lose(env, "Memory exhausted, quitting program.");
    }
}

static inline void * MKCL_GC_MALLOC_ATOMIC_IGNORE_OFF_PAGE(MKCL, mkcl_index size)
{
  void * new;

  MKCL_GC_NO_INTR(env, new = MK_GC_MALLOC_ATOMIC_IGNORE_OFF_PAGE(size));

  if (mkcl_likely(new != NULL))
    return new;
  else
    {
      grow_memory(env);
      MKCL_GC_NO_INTR(env, new = MK_GC_MALLOC_ATOMIC_IGNORE_OFF_PAGE(size));
      if (mkcl_likely(new != NULL))
	return new;
      else
	mkcl_lose(env, "Memory exhausted, quitting program.");
    }
}



static inline void * MKCL_GC_MEMALIGN(MKCL, mkcl_index alignment, mkcl_index size)
{
  void * new;

  MKCL_GC_NO_INTR(env, new = MK_GC_memalign(alignment, size));

  if (mkcl_likely(new != NULL))
    return new;
  else
    {
      grow_memory(env);
      MKCL_GC_NO_INTR(env, new = MK_GC_memalign(alignment, size));
      if (mkcl_likely(new != NULL))
	return new;
      else
	mkcl_lose(env, "Memory exhausted, quitting program.");
    }
}

void * mkcl_alloc_pages(MKCL, mkcl_index nb_pages)
{
  long pagesize = mkcl_core.pagesize;

  return MKCL_GC_MEMALIGN(env, pagesize, nb_pages * pagesize);
}

static void restore_block_access_permissions(void * obj, void * client_data)
{
#if MKCL_UNIX
  int rc = mprotect(obj, mkcl_core.pagesize, PROT_READ | PROT_WRITE);
  if (rc)
    {
      mkcl_env env = MKCL_ENV();

      if (env)
        mkcl_FElibc_error(env, "restore_block_access_permissions() failed on mprotect()", 0);
    }
#elif MKCL_WINDOWS
  { /* By default on Win64 data is PAGE_READWRITE only and we would get
       an ACCESS_VIOLATION if we didn't set it to EXECUTE. */
    DWORD old_protection_flags;
    BOOL ok = VirtualProtect(obj, mkcl_core.pagesize, PAGE_READWRITE, &old_protection_flags);
    
    if (!ok)
      {
        mkcl_env env = MKCL_ENV();
        
        if (env)
          mkcl_FEwin32_error(env, "restore_block_access_permissions() failed on VirtualProtect()", 0);
      }
  }
#else
# error "Function restore_block_access_permissions() for callback blocks is not implemented properly."
#endif

#if 0 /* debug */
  printf("\n!!! Ran restore_block_access_permissions() for a callback block!\n"); fflush(NULL);
#endif
}

void * mkcl_alloc_callback_block(MKCL)
{ /* An entire page (usually 4096 bytes) for a single callback! That is quite some waste. FIXME. JCB */
  long pagesize = mkcl_core.pagesize;
  void * block = MKCL_GC_MEMALIGN(env, pagesize, pagesize);

  MK_GC_register_finalizer_no_order(block, restore_block_access_permissions, NULL, NULL, NULL);
  return block;
}

/****************************************************/

mkcl_object mkcl_alloc_cdisplay(MKCL, mkcl_index nb_levels)
{
  mkcl_object cd;

  COLLECT_STATS(env, cdisplay);
  cd = MKCL_GC_MALLOC(env, sizeof(struct mkcl_cdisplay) + nb_levels * sizeof(mkcl_object));
  cd->display.t = mkcl_t_cdisplay;
  cd->display.nb_levels = nb_levels;
  return cd;
}

mkcl_object mkcl_alloc_clevel_block(MKCL, mkcl_object producer, const union mkcl_lispunion * const outer, const mkcl_index nb_vars)
{
  mkcl_object cb;

  COLLECT_STATS(env, clevel_block);
  cb = MKCL_GC_MALLOC(env, sizeof(struct mkcl_clevel_block) + nb_vars * sizeof(mkcl_object));
  cb->lblock.t = mkcl_t_clevel_block;
  cb->lblock.producer = producer;
  cb->lblock.outer = (mkcl_object) outer;
  cb->lblock.nb_vars = nb_vars;
  return cb;
}


mkcl_object
mkcl_alloc_bignum_with_limbs(MKCL, int nb_limbs)
{
  mkcl_index size = sizeof(struct mkcl_bignum) + (nb_limbs * sizeof(mp_limb_t));
  mkcl_object x;

  COLLECT_STATS(env, bignum);
  x = MKCL_GC_MALLOC_ATOMIC(env, size);
  x->big.t = mkcl_t_bignum;

  {
    __mpz_struct * b = x->big.big_num;
    b->_mp_d = (mp_limb_t *)(((char*)x) + sizeof(struct mkcl_bignum));
    b->_mp_alloc = nb_limbs;
    b->_mp_size = 0;
  }

  return x;
}

mkcl_object
mkcl_cons(MKCL, mkcl_object a, mkcl_object d)
{
  struct mkcl_cons *obj;
  
  COLLECT_STATS(env, cons);
  obj = MKCL_GC_MALLOC(env, sizeof(struct mkcl_cons));
  obj->t = mkcl_t_cons;
  obj->car = a;
  obj->cdr = d;
  return (mkcl_object)obj;
}

mkcl_object
mkcl_list1(MKCL, mkcl_object a)
{
  struct mkcl_cons *obj;

  COLLECT_STATS(env, cons);
  obj = MKCL_GC_MALLOC(env, sizeof(struct mkcl_cons));
  obj->t = mkcl_t_cons;
  obj->car = a;
  obj->cdr = mk_cl_Cnil;
  return (mkcl_object)obj;
}

mkcl_object mkcl_alloc_raw_structure(MKCL, mkcl_object type, mkcl_index nb_slots)
{
  mkcl_object s;

#if (MKCL_T_STRUCTURE == mkcl_t_instance)
  COLLECT_STATS(env, instance);
  s = MKCL_GC_MALLOC(env, sizeof(struct mkcl_instance));
  s->instance.sig = MKCL_UNBOUND; /* This special value prevent structure from being seen as updatable objects. */
#else
  COLLECT_STATS(env, structure);
  s = MKCL_GC_MALLOC(env, sizeof(struct mkcl_structure));
#endif

  s->d.t = MKCL_T_STRUCTURE;
  MKCL_STYPE(s) = type;
  if (nb_slots >= MKCL_SLOTS_LIMIT)
    mkcl_FEerror(env, "Limit on structure size exceeded: ~S slots requested.", 1, MKCL_MAKE_FIXNUM(nb_slots));
  MKCL_SLOTS(s) = MKCL_GC_MALLOC_IGNORE_OFF_PAGE(env, sizeof(mkcl_object) * nb_slots);
  MKCL_SLENGTH(s) = nb_slots;
  return s;
}

mkcl_object
mkcl_alloc_raw_instance(MKCL, mkcl_index nb_slots)
{
  mkcl_object i;

  COLLECT_STATS(env, instance);
  i = MKCL_GC_MALLOC(env, sizeof(struct mkcl_instance));
  i->instance.t = mkcl_t_instance;
  i->instance.isgf = MKCL_NOT_FUNCALLABLE;

  i->instance.slots = MKCL_GC_MALLOC_IGNORE_OFF_PAGE(env, sizeof(mkcl_object) * nb_slots);
  i->instance.length = nb_slots;

  i->instance.clas = mk_cl_Cnil; /* dummy */
  i->instance.sig = mk_cl_Ct; /* sure to never be a valid signature since it cannot be a list. */

  i->instance.f.entry = mkcl_FEnot_funcallable_vararg;
  i->instance.f._[0] = mkcl_FEnot_funcallable_fixed;
  i->instance.f._[1] = mkcl_FEnot_funcallable_fixed;
  i->instance.f._[2] = mkcl_FEnot_funcallable_fixed;
  i->instance.f._[3] = mkcl_FEnot_funcallable_fixed;
  i->instance.f._[4] = mkcl_FEnot_funcallable_fixed;
  return i;
}

mkcl_object
mkcl_alloc_raw_base_string(MKCL)
{
  mkcl_object s;

  COLLECT_STATS(env, base_string);
  s = MKCL_GC_MALLOC(env, sizeof(struct mkcl_base_string));
  s->base_string.t = mkcl_t_base_string;
  return s;
}

mkcl_object
mkcl_alloc_raw_string(MKCL)
{
  mkcl_object s;

  COLLECT_STATS(env, string);
  s = MKCL_GC_MALLOC(env, sizeof(struct mkcl_string));
  s->string.t = mkcl_t_string;
  return s;
}

mkcl_object
mkcl_alloc_raw_symbol(MKCL)
{
  mkcl_object s;

  COLLECT_STATS(env, symbol);
  s = MKCL_GC_MALLOC(env, sizeof(struct mkcl_symbol));
  s->symbol.t = mkcl_t_symbol;
  return s;
}

mkcl_object
mkcl_alloc_raw_bytecode(MKCL)
{
  mkcl_object s;

  COLLECT_STATS(env, bytecode);
  s = MKCL_GC_MALLOC(env, sizeof(struct mkcl_bytecode));
  s->bytecode.t = mkcl_t_bytecode;
  return s;
}

mkcl_object
mkcl_alloc_raw_bclosure(MKCL)
{
  mkcl_object s;

  COLLECT_STATS(env, bclosure);
  s = MKCL_GC_MALLOC(env, sizeof(struct mkcl_bclosure));
  s->bclosure.t = mkcl_t_bclosure;
  return s;
}

mkcl_object
mkcl_alloc_raw_cfun(MKCL)
{
  mkcl_object s;

  COLLECT_STATS(env, cfun);
  s = MKCL_GC_MALLOC(env, sizeof(struct mkcl_cfun));
  s->cfun.t = mkcl_t_cfun;
  return s;
}

mkcl_object
mkcl_alloc_raw_cclosure(MKCL)
{
  mkcl_object s;

  COLLECT_STATS(env, cclosure);
  s = MKCL_GC_MALLOC(env, sizeof(struct mkcl_cclosure));
  s->cclosure.t = mkcl_t_cclosure;
  return s;
}

mkcl_object
mkcl_alloc_raw_vector(MKCL)
{
  mkcl_object s;

  COLLECT_STATS(env, vector);
  s = MKCL_GC_MALLOC(env, sizeof(struct mkcl_vector));
  s->vector.t = mkcl_t_vector;
  return s;
}

mkcl_object
mkcl_alloc_raw_bitvector(MKCL)
{
  mkcl_object s;

  COLLECT_STATS(env, bitvector);
  s = MKCL_GC_MALLOC(env, sizeof(struct mkcl_vector));
  s->vector.t = mkcl_t_bitvector;
  return s;
}

mkcl_object
mkcl_alloc_raw_array(MKCL)
{
  mkcl_object s;

  COLLECT_STATS(env, array);
  s = MKCL_GC_MALLOC(env, sizeof(struct mkcl_array));
  s->array.t = mkcl_t_array;
  return s;
}

mkcl_object
mkcl_alloc_raw_bignum(MKCL)
{
  mkcl_object s;

  COLLECT_STATS(env, bignum);
  s = MKCL_GC_MALLOC(env, sizeof(struct mkcl_bignum));
  s->big.t = mkcl_t_bignum;
  return s;
}

mkcl_object
mkcl_alloc_raw_ratio(MKCL)
{
  mkcl_object s;

  COLLECT_STATS(env, ratio);
  s = MKCL_GC_MALLOC(env, sizeof(struct mkcl_ratio));
  s->ratio.t = mkcl_t_ratio;
  return s;
}

mkcl_object
mkcl_alloc_raw_singlefloat(MKCL)
{
  mkcl_object s;

  COLLECT_STATS(env, singlefloat);
  s = MKCL_GC_MALLOC_ATOMIC(env, sizeof(struct mkcl_singlefloat));
  s->SF.t = mkcl_t_singlefloat;
  return s;
}

mkcl_object
mkcl_alloc_raw_doublefloat(MKCL)
{
  mkcl_object s;

  COLLECT_STATS(env, doublefloat);
  s = MKCL_GC_MALLOC_ATOMIC(env, sizeof(struct mkcl_doublefloat));
  s->DF.t = mkcl_t_doublefloat;
  return s;
}

mkcl_object
mkcl_alloc_raw_longfloat(MKCL)
{
  mkcl_object s;

  COLLECT_STATS(env, longfloat);
  s = MKCL_GC_MALLOC_ATOMIC(env, sizeof(struct mkcl_long_float));
  s->longfloat.t = mkcl_t_longfloat;
  return s;
}

mkcl_object
mkcl_alloc_raw_complex(MKCL)
{
  mkcl_object s;

  COLLECT_STATS(env, complex);
  s = MKCL_GC_MALLOC(env, sizeof(struct mkcl_complex));
  s->_complex.t = mkcl_t_complex;
  return s;
}

mkcl_object
mkcl_alloc_raw_hashtable(MKCL)
{
  mkcl_object s;

  COLLECT_STATS(env, hashtable);
  s = MKCL_GC_MALLOC(env, sizeof(struct mkcl_hashtable));
  s->hash.t = mkcl_t_hashtable;
  return s;
}

mkcl_object
mkcl_alloc_raw_codeblock(MKCL)
{
  mkcl_object s;

  COLLECT_STATS(env, codeblock);
  s = MKCL_GC_MALLOC(env, sizeof(struct mkcl_codeblock));
  s->cblock.t = mkcl_t_codeblock;
  return s;
}

mkcl_object
mkcl_alloc_raw_random(MKCL)
{
  mkcl_object s;

  COLLECT_STATS(env, random);
  s = MKCL_GC_MALLOC(env, sizeof(struct mkcl_random));
  s->random.t = mkcl_t_random;
  return s;
}

mkcl_object
mkcl_alloc_raw_package(MKCL)
{
  mkcl_object s;

  COLLECT_STATS(env, package);
  s = MKCL_GC_MALLOC(env, sizeof(struct mkcl_package));
  s->pack.t = mkcl_t_package;
  return s;
}

mkcl_object
mkcl_alloc_raw_pathname(MKCL)
{
  mkcl_object s;

  COLLECT_STATS(env, pathname);
  s = MKCL_GC_MALLOC(env, sizeof(struct mkcl_pathname));
  s->pathname.t = mkcl_t_pathname;
  return s;
}

mkcl_object
mkcl_alloc_raw_readtable(MKCL)
{
  mkcl_object s;

  COLLECT_STATS(env, readtable);
  s = MKCL_GC_MALLOC(env, sizeof(struct mkcl_readtable));
  s->readtable.t = mkcl_t_readtable;
  return s;
}

mkcl_object
mkcl_alloc_raw_thread(MKCL)
{
  mkcl_object s;

  COLLECT_STATS(env, thread);
  s = MKCL_GC_MALLOC(env, sizeof(struct mkcl_thread));
  s->thread.t = mkcl_t_thread;
  return s;
}

mkcl_object
mkcl_alloc_raw_lock(MKCL)
{
  mkcl_object s;

  COLLECT_STATS(env, lock);
  s = MKCL_GC_MALLOC(env, sizeof(struct mkcl_lock));
  s->lock.t = mkcl_t_lock;
  return s;
}

mkcl_object
mkcl_alloc_raw_rwlock(MKCL)
{
  mkcl_object s;

  COLLECT_STATS(env, rwlock);
  s = MKCL_GC_MALLOC(env, sizeof(struct mkcl_rwlock));
  s->rwlock.t = mkcl_t_rwlock;
  return s;
}

mkcl_object
mkcl_alloc_raw_semaphore(MKCL)
{
  mkcl_object s;

  COLLECT_STATS(env, semaphore);
  s = MKCL_GC_MALLOC(env, sizeof(struct mkcl_semaphore));
  s->semaphore.t = mkcl_t_semaphore;
  return s;
}

mkcl_object
mkcl_alloc_raw_condition_variable(MKCL)
{
  mkcl_object s;

  COLLECT_STATS(env, condition_variable);
  s = MKCL_GC_MALLOC(env, sizeof(struct mkcl_condition_variable));
  s->condition_variable.t = mkcl_t_condition_variable;
  return s;
}

mkcl_object
mkcl_alloc_raw_foreign(MKCL)
{
  mkcl_object s;

  COLLECT_STATS(env, foreign);
  s = MKCL_GC_MALLOC(env, sizeof(struct mkcl_foreign));
  s->foreign.t = mkcl_t_foreign;
  return s;
}

mkcl_object
mkcl_alloc_raw_stream(MKCL)
{
  mkcl_object s;

  COLLECT_STATS(env, stream);
  s = MKCL_GC_MALLOC(env, sizeof(struct mkcl_stream));
  s->stream.t = mkcl_t_stream;
  return s;
}

mkcl_object
mkcl_alloc_raw_process(MKCL)
{
  mkcl_object s;

  COLLECT_STATS(env, process);
  s = MKCL_GC_MALLOC(env, sizeof(struct mkcl_process));
  s->process.t = mkcl_t_process;
  return s;
}

mkcl_object
mkcl_alloc_utf_8(MKCL, mkcl_index length)
{
  mkcl_object s;
  unsigned char * self;

  COLLECT_STATS(env, UTF_8);
  s = MKCL_GC_MALLOC(env, sizeof(struct mkcl_UTF_8));
  s->UTF_8.t = mkcl_t_UTF_8;
  self = MKCL_GC_MALLOC_ATOMIC_IGNORE_OFF_PAGE(env, (length + 1) * sizeof(mkcl_char8));
  s->UTF_8.self = self;
  s->UTF_8.fillp = 0;
  s->UTF_8.dim = length;
  self[0] = self[length] = 0;
  return s;
}

mkcl_object
mkcl_alloc_raw_utf_8(MKCL)
{
  mkcl_object s;

  COLLECT_STATS(env, UTF_8);
  s = MKCL_GC_MALLOC(env, sizeof(struct mkcl_UTF_8));
  s->UTF_8.t = mkcl_t_UTF_8;
  s->UTF_8.self = NULL;
  s->UTF_8.fillp = s->UTF_8.dim = 0;
  return s;
}

mkcl_object
mkcl_alloc_utf_16(MKCL, mkcl_index length)
{
  mkcl_object s;
  mkcl_char16 * self;

  COLLECT_STATS(env, UTF_16);
  s = MKCL_GC_MALLOC(env, sizeof(struct mkcl_UTF_16));
  s->UTF_16.t = mkcl_t_UTF_16;
  self = MKCL_GC_MALLOC_ATOMIC_IGNORE_OFF_PAGE(env, (length + 1) * sizeof(mkcl_char16));  
  s->UTF_16.self = self;
  s->UTF_16.fillp = 0;
  s->UTF_16.dim = length;
  self[0] = self[length] = 0;
  return s;
}

mkcl_object
mkcl_alloc_raw_utf_16(MKCL)
{
  mkcl_object s;

  COLLECT_STATS(env, UTF_16);
  s = MKCL_GC_MALLOC(env, sizeof(struct mkcl_UTF_16));
  s->UTF_16.t = mkcl_t_UTF_16;
  s->UTF_16.self = NULL;
  s->UTF_16.fillp = s->UTF_16.dim = 0;
  return s;
}




void *
mkcl_alloc_uncollectable(MKCL, size_t size)
{
  void *output;
  output = MKCL_GC_MALLOC_UNCOLLECTABLE(env, size);
  return output;
}

void
mkcl_free_uncollectable(MKCL, void *pointer)
{
  MKCL_GC_NO_INTR(env, MK_GC_FREE(pointer));
}

void *
_mkcl_boot_alloc_unprotected(mkcl_index n)
{
  return MK_GC_MALLOC(n); /* This one is more conservative. JCB */
}

void *
mkcl_alloc(MKCL, mkcl_index n)
{
  void *output;

  output = MKCL_GC_MALLOC(env, n); /* This one is more conservative. JCB */
  return output;
}

void *
mkcl_alloc_atomic(MKCL, mkcl_index n)
{
  void *output;

  output = MKCL_GC_MALLOC_ATOMIC(env, n); /* This one is more conservative. JCB */
  return output;
}

void
mkcl_dealloc(MKCL, void *ptr)
{
  /* Intentionally left empty! Broken on any platform after all... */
  /* Beside, we believe that the GC will do the right thing, eventually... */
}

void *
mkcl_foreign_malloc(MKCL, size_t size)
{
  void *output;
  MKCL_LIBC_REALLY_NO_INTR(env, output = malloc(size));

  if ((output == NULL) && (size != 0))
    mkcl_FElibc_error(env, "Foreign memory exhausted!", 0);
  return output;
}

void
mkcl_foreign_free(MKCL, void *pointer)
{
  MKCL_LIBC_REALLY_NO_INTR(env, free(pointer));
}

#if MKCL_STATS
static const struct mkcl_alloc_stats blank_alloc_stats = { 0 };

static struct mkcl_alloc_stats * mkcl_alloc_alloc_stats(MKCL)
{
  struct mkcl_alloc_stats * stats = ((env == NULL) 
				     ? _mkcl_boot_alloc_unprotected(sizeof(struct mkcl_alloc_stats))
				     : mkcl_alloc(env, sizeof(struct mkcl_alloc_stats)));

  if (stats)
    *stats = blank_alloc_stats;
  return stats;
}

mkcl_object mk_si_reset_allocation_statistics(MKCL)
{
  if (env->alloc) *(env->alloc) = blank_alloc_stats;
  mkcl_return_value(mk_cl_Cnil);
}
#endif

const mkcl_env _mkcl_alloc_raw_env(MKCL)
{
  const mkcl_env new_env = ((env == NULL) 
			    ? _mkcl_boot_alloc_unprotected(sizeof(struct mkcl_env_struct))
			    : mkcl_alloc(env, sizeof(struct mkcl_env_struct)));

  if (new_env)
    {
#if MKCL_STATS
      new_env->alloc = mkcl_alloc_alloc_stats(env);
#else
      new_env->alloc = NULL;
#endif  
    }
  return new_env;
}



/******************************/

static void (*old_MK_GC_start_call_back)(void);
extern void (*MK_GC_start_call_back)(void);  /* Internal and private to Boehm's GC. */ /* Not thread protected! */

static void mkcl_GC_abort(const char * const msg);
#if MKCL_GC_7_2d
static void mkcl_GC_exit(const int status);
#endif

static void * customize_GC(void * client_data)
{
  MK_GC_set_no_dls(FALSE); /* This prevent GC from scanning dynamic library data segments */

  old_MK_GC_start_call_back = MK_GC_start_call_back;
  MK_GC_start_call_back = mkcl_count_GC_collections; /* We should do proper chaining of GC start callbacks! */

#if MKCL_GC_7_2d
  MK_GC_java_finalization = 1; /* not thread-safe! Ok as long as nobody want that topological sort stuff. */
#else
  MK_GC_set_java_finalization(TRUE);
#endif

  {
    int old_finalize_on_demand = MK_GC_get_finalize_on_demand();

    MK_GC_set_finalize_on_demand(TRUE);
  }

  MK_GC_set_abort_func(mkcl_GC_abort);
#if MKCL_GC_7_2d
  MK_GC_set_exit_func(mkcl_GC_exit);
#endif
  return NULL;
}

static int alloc_initialized = FALSE;

#if __ANDROID__ && (__arm__ || __i386__)
/* Signal mask support for realtime signals is broken in Android 32bits. */
# define DEFAULT_GC_THREAD_SUSPEND_SIGNAL SIGPWR
# define DEFAULT_GC_THREAD_RESTART_SIGNAL SIGXCPU
#elif MKCL_PTHREADS
# define DEFAULT_GC_THREAD_SUSPEND_SIGNAL SIGRTMIN + 5
# define DEFAULT_GC_THREAD_RESTART_SIGNAL SIGRTMIN + 4
#elif MKCL_WINDOWS
#else
# error Default GC signals!
#endif /* MKCL_PTHREADS */

int
mkcl_init_alloc(void)
{
  /* This function is called so early in the life of a MKCL world that we cannot
     allow it to throw a CL condition under any circonstance.
     Error reporting must be done by returning an int error code
     other than 0 (a bit a la pthread_xxx()) choosing that value
     to be coherent with the already predefined libc errno values.
   */
  if (alloc_initialized) return 0; /* Not really thread-safe. */  /* 0 indicates success, sort of. */

#if MKCL_WINDOWS
  InitializeCriticalSection(&oom_handler_lock);
#elif MKCL_PTHREADS
  {
    int rc;

    if ((rc = pthread_mutex_init(&oom_handler_lock, NULL)))
      return rc;
  }
#endif

  /*
   * Garbage collector restrictions: we set up the garbage collector
   * library to work as follows:
   *
   * 1) The garbage collector shall not scan shared libraries explicitly.
   * 2) We only detect objects that are referenced by a pointer to
   *    the begining or to the first byte.
   */
  MK_GC_set_all_interior_pointers(0);
  MK_GC_set_time_limit(MK_GC_TIME_UNLIMITED);

#if MKCL_PTHREADS
  int gc_thread_suspend_sig = mkcl_get_option(MKCL_OPT_GC_THREAD_SUSPEND_SIGNAL);
  if (gc_thread_suspend_sig == 0) {
    gc_thread_suspend_sig = DEFAULT_GC_THREAD_SUSPEND_SIGNAL;
    mkcl_set_option(MKCL_OPT_GC_THREAD_SUSPEND_SIGNAL, gc_thread_suspend_sig);
  }
  int gc_thread_restart_sig = mkcl_get_option(MKCL_OPT_GC_THREAD_RESTART_SIGNAL);
  if (gc_thread_restart_sig == 0) {
    gc_thread_restart_sig = DEFAULT_GC_THREAD_RESTART_SIGNAL;
    mkcl_set_option(MKCL_OPT_GC_THREAD_RESTART_SIGNAL, gc_thread_restart_sig);
  }

  MK_GC_set_suspend_signal(gc_thread_suspend_sig);
#if MKCL_GC_7_2d
  MK_GC_set_thread_restart_signal(gc_thread_restart_sig);
#else
  MK_GC_set_thr_restart_signal(gc_thread_restart_sig);
#endif
#endif /* MKCL_PTHREADS */

  MK_GC_init();

  MK_GC_disable();

#if MKCL_WINDOWS
  EnterCriticalSection(&oom_handler_lock);
#elif MKCL_PTHREADS
  { int rc; if ((rc = pthread_mutex_lock(&oom_handler_lock))) return rc; }
#else
# error Incomplete mkcl_init_alloc().
#endif
  MK_GC_set_max_heap_size(mkcl_core.max_heap_size = mkcl_get_option(MKCL_OPT_HEAP_SIZE));
#if MKCL_WINDOWS
  LeaveCriticalSection(&oom_handler_lock);
#elif MKCL_PTHREADS
  { int rc; if ((rc = pthread_mutex_unlock(&oom_handler_lock))) return rc; }
#else
# error Incomplete mkcl_init_alloc().
#endif
  /* Save some memory in case we get tight. */
  if (mkcl_core.max_heap_size == 0) {
    mkcl_index size = mkcl_get_option(MKCL_OPT_HEAP_SAFETY_AREA);
    mkcl_core.safety_region = MK_GC_MALLOC(size);
  } else {
    mkcl_core.safety_region = NULL;
  }

  MK_GC_call_with_alloc_lock(customize_GC, NULL);

  MK_GC_clear_roots();
  MK_GC_add_roots(&mkcl_core, (&mkcl_core + 1));
  MK_GC_add_roots(mkcl_root_symbols, (mkcl_root_symbols + mkcl_root_symbols_count));

  MK_GC_set_warn_proc(no_warnings);
  alloc_initialized = TRUE;
  return 0; /* 0 indicates success. */
}

void mkcl_clean_up_alloc(MKCL)
{ /* Best effort only. We cannot raise an exception from here. */
  MK_GC_uninit();
#if MKCL_WINDOWS
  DeleteCriticalSection(&oom_handler_lock);
#elif MKCL_PTHREADS
  (void) pthread_mutex_destroy(&oom_handler_lock);
#else
# error Incomplete mkcl_clean_up_alloc().
#endif
}

/**********************************************************
 *		FINALIZATION				  *
 **********************************************************/




#if 0 /* DEBUG */
static void say_what_final(char * typename)
{
  fprintf(stderr, "\nMKCL: finalization of a %s.", typename);
  fflush(stderr);
}
#else
#define say_what_final(typename)
#endif


static void
standard_finalizer(MKCL, mkcl_object o)
{
  switch (mkcl_type_of(o))
    {
    case mkcl_t_stream:
      say_what_final("stream");
      mk_cl_close(env, 1, o);
      break;
    case mkcl_t_lock:
      {
	say_what_final("lock");
#if MKCL_WINDOWS
	if (o->lock.mutex)
	  {
	    MKCL_LIBC_NO_INTR(env, CloseHandle(o->lock.mutex));  /* FIXME! return status? JCB */
	    o->lock.mutex = NULL;
	  }
#elif MKCL_PTHREADS
	MKCL_LIBC_NO_INTR(env, pthread_mutex_destroy(o->lock.mutex));  /* FIXME! return status? JCB */
#else
# error Incomplete standard_finalizer().
#endif
      }
      break;
    case mkcl_t_rwlock:
      {
      say_what_final("rwlock");
#if MKCL_WINDOWS
	if (o->rwlock.rwlock)
	  {
	    MKCL_LIBC_NO_INTR(env, CloseHandle(o->rwlock.rwlock));  /* FIXME! return status? JCB */
	    o->rwlock.rwlock = NULL;
	  }
#elif MKCL_PTHREADS
	MKCL_LIBC_NO_INTR(env, pthread_rwlock_destroy(o->rwlock.rwlock));  /* FIXME! return status? JCB */
#else
# error Incomplete standard_finalizer().
#endif
      }
      break;
    case mkcl_t_semaphore:
      {
      say_what_final("semaphore");
#if MKCL_WINDOWS
	if (o->semaphore.sem)
	  {
	    MKCL_LIBC_NO_INTR(env, CloseHandle(o->semaphore.sem));  /* FIXME! return status? JCB */
	    o->semaphore.sem = NULL;
	  }
#elif MKCL_PTHREADS
	MKCL_LIBC_NO_INTR(env, sem_destroy(o->semaphore.sem));  /* FIXME! return status? JCB */
#else
# error Incomplete standard_finalizer().
#endif
      }
      break;
    case mkcl_t_condition_variable:
      {
	say_what_final("condition variable");
#if MKCL_WINDOWS
	if (o->condition_variable.event)
	  {
	    MKCL_LIBC_NO_INTR(env, CloseHandle(o->condition_variable.event));  /* FIXME! return status? JCB */
	    o->condition_variable.event = NULL;
	  }
#elif MKCL_PTHREADS
	MKCL_LIBC_NO_INTR(env, pthread_cond_destroy(&o->condition_variable.cv));  /* FIXME! return status? JCB */
#else
# error Incomplete standard_finalizer().
#endif
      }
      break;
    case mkcl_t_readtable:
      {
	say_what_final("readtable");
#if MKCL_WINDOWS
	MKCL_LIBC_NO_INTR(env, DeleteCriticalSection(&o->readtable.lock));
#elif MKCL_PTHREADS
	MKCL_LIBC_NO_INTR(env, pthread_mutex_destroy(&o->readtable.lock));  /* FIXME! return status? JCB */
#else
# error Incomplete standard_finalizer().
#endif
      }
      break;
    case mkcl_t_process:
      say_what_final("process");
      mkcl_finalize_process(env, o);
      break;
    case mkcl_t_thread:
      {
	say_what_final("thread");
#if MKCL_WINDOWS
	if (o->thread.thread)
	  {
	    o->thread.tid = 0;
	    MKCL_LIBC_NO_INTR(env, CloseHandle(o->thread.thread));  /* FIXME! return status? JCB */
	    o->thread.base_thread = o->thread.thread = NULL;
	  }
#elif MKCL_PTHREADS
	if (o->thread.thread)
	  {
	    o->thread.tid = 0;
	    /* This (the finalizer) is the only safe place from which to call pthread_detach(). */
	    MKCL_LIBC_NO_INTR(env, pthread_detach(o->thread.thread));  /* FIXME! return status? JCB */
	    o->thread.base_thread = o->thread.thread = 0;
	  }
	if (o->thread.running_lock)
	  {
	    pthread_mutex_destroy(o->thread.running_lock);
	    o->thread.running_lock = NULL;
	  }
#else
# error Incomplete standard_finalizer().
#endif
      }
      break;
    case mkcl_t_package:
      {
	say_what_final("package");
#if MKCL_WINDOWS
	DeleteCriticalSection(&(o->pack.lock));
#elif MKCL_PTHREADS
	MKCL_LIBC_NO_INTR(env, pthread_mutex_destroy(&o->pack.lock));  /* FIXME! return status? JCB */
#else
# error Incomplete standard_finalizer().
#endif
      }
      break;
    case mkcl_t_codeblock:
      say_what_final("codeblock");
      mkcl_library_close(env, o);
      break;
    default:;
    }
}


/* GC's source code says that this callback is called with the world running and the GC's lock NOT held. */
static void call_finalizer_on_mkcl_object(void * obj, void * client_data)
{
  mkcl_object o = (mkcl_object) obj;
  mkcl_object finalizer = (mkcl_object) client_data;

  if (finalizer != mk_cl_Cnil && finalizer != NULL)
    {
      mkcl_env env = MKCL_ENV();
      mkcl_env imported_env = NULL;
#if 1
      mkcl_index saved_nvalues;
      mkcl_object saved_value0;
#endif
    
      if (env == NULL)
	{
	  static const mkcl_base_string_object(thread_name_obj, "Imported thread finalization");

	  imported_env = env = mkcl_import_current_thread((mkcl_object)&thread_name_obj, mk_cl_Cnil, NULL, NULL);
	}
      else
	{
	  /* printf("\nIn call_finalizer_on_mkcl_object() in already assigned thread.\n"); fflush(NULL); */
#if 1
	  saved_nvalues = env->nvalues;
	  saved_value0 = env->values[0];
#endif
	}

      if (env)
	{
	  char stack_mark = 0;
	  mkcl_index i, nvalues = MKCL_MULTIPLE_VALUES_LIMIT + 1;
	  mkcl_object values[MKCL_MULTIPLE_VALUES_LIMIT];

	  MKCL_CATCH_ALL_BEGIN(env) {
	    if ((i = env->nvalues) > MKCL_MULTIPLE_VALUES_LIMIT)
	      i = MKCL_MULTIPLE_VALUES_LIMIT;
	    for (nvalues = 0; nvalues < i; nvalues++)
	      values[nvalues] = env->values[nvalues];

	    if (imported_env)
	      {
		MKCL_SETUP_CALL_STACK_ROOT_GUARD(env);
		mkcl_setup_thread_lisp_context(env, &stack_mark);

		mkcl_register_thread_as_active(env, env->own_thread);
		mkcl_enable_interrupts(env);
	      }
      
	    /* The result value of finalizer is purposely ignored. */
	    if (finalizer == mk_cl_Ct) {
	      standard_finalizer(env, o);
	    } else if (mkcl_functionp(env, finalizer) || MKCL_SYMBOLP(finalizer)) {
#if 0
	      mkcl_funcall1(env, finalizer, obj);
#else
	      mkcl_top_apply(env, finalizer, mk_si_dyn_cons(env, obj, mk_cl_Cnil));
#endif
	    } /* else we simply ignore the whole thing. */

	    if (imported_env)
	      {
		mkcl_cleanup_thread_lisp_context(env);
		mkcl_disable_interrupts(env);
                MKCL_UNSET_CALL_STACK_ROOT_GUARD(env);
	      }

	  } MKCL_CATCH_ALL_IF_CAUGHT {
	    if (imported_env)
	      { MKCL_UNSET_CALL_STACK_ROOT_GUARD(env); }
	    if (finalizer == mk_cl_Ct)
	      fprintf(stderr, "\nMKCL: standard finalizer has crashed!\nMKCL: object ");
	    else
	      fprintf(stderr, "\nMKCL: a custom finalizer has crashed!\nMKCL: object ");
	    fflush(stderr);
	    mkcl_princ(env, mk_cl_type_of(env, o), mkcl_core.error_output);
	    mkcl_write_char(env, ' ', mkcl_core.error_output);
	    mkcl_prin1(env, o, mkcl_core.error_output);
	    mkcl_terpri(env, mkcl_core.error_output);
	    mkcl_write_cstr(env, "MKCL: Thread value = ", mkcl_core.error_output);
	    mkcl_prin1(env, env->own_thread->thread.result_value, mkcl_core.error_output);
	    mkcl_terpri(env, mkcl_core.error_output);
	    mkcl_finish_output(env, mkcl_core.error_output);
	    fflush(stderr);
	  } MKCL_CATCH_ALL_END;

	  if (nvalues <= MKCL_MULTIPLE_VALUES_LIMIT)
	    {
	      env->nvalues = nvalues;
	      for (i = 0; i < nvalues; i++) env->values[i] = values[i];
	    }

	  if (imported_env != NULL)
	    {
	      /* mkcl_bds_unwind1(env); */
	      /* mkcl_bds_unwind1(env); */
	      /* mkcl_bds_unwind1(env); */
	      /* mkcl_bds_unwind1(env); */
	      /* mkcl_bds_unwind1(env); */
	      mkcl_release_current_thread(imported_env);
	    }
	  else
	    {
#if 1
	      if (env->nvalues != saved_nvalues)
		fprintf(stderr, "\nIn call_finalizer_on_mkcl_object(): Corrupted nvalues, old = %lu, new = %lu\n",
			(unsigned long) saved_nvalues, (unsigned long) env->nvalues);
	      if (saved_nvalues != 0 && env->values[0] != saved_value0)
		fprintf(stderr, "\nIn call_finalizer_on_mkcl_object(): Corrupted value0, old = %p, new = %p\n",
			saved_value0, env->values[0]);
	      fflush(stderr);
#endif
	    }
	}
    }
}

mkcl_object
mk_si_get_finalizer(MKCL, mkcl_object o)
{
  mkcl_object output;
  MK_GC_finalization_proc ofn, ofn2;
  void *odata, *odata2;
  mkcl_interrupt_status old_intr;

  mkcl_call_stack_check(env);
  mkcl_get_interrupt_status(env, &old_intr);
  mkcl_disable_interrupts(env);
  MK_GC_register_finalizer_no_order(o, 0, NULL, &ofn, &odata);
  MK_GC_register_finalizer_no_order(o, ofn, odata, &ofn2, &odata2);
  mkcl_set_interrupt_status(env, &old_intr);

  if (ofn == 0) {
    output = mk_cl_Cnil;
  } else if (ofn == call_finalizer_on_mkcl_object) {
    output = (mkcl_object)odata;
  } else {
    output = mk_cl_Cnil;
  }
  mkcl_return_value(output);
}

mkcl_object
mk_si_set_finalizer(MKCL, mkcl_object obj, mkcl_object finalizer)
{
  MK_GC_finalization_proc ofn;
  void *odata;

  mkcl_call_stack_check(env);
  if (finalizer == mk_cl_Cnil) {
    MKCL_GC_NO_INTR(env, MK_GC_register_finalizer_no_order(obj, 0, NULL, &ofn, &odata)); /* cancel finalization */
  } else {
    MKCL_GC_NO_INTR(env, MK_GC_register_finalizer_no_order(obj, call_finalizer_on_mkcl_object, finalizer, &ofn, &odata));
  }
  mkcl_return_no_value;
}

mkcl_object
mk_si_gc_stats(MKCL, mkcl_object enable)
{
  mkcl_object old_status = mkcl_core.gc_stats ? mk_cl_Ct : mk_cl_Cnil;

  mkcl_call_stack_check(env);
  mkcl_core.gc_stats = (enable != mk_cl_Cnil);
  if (mkcl_core.bytes_consed == mk_cl_Cnil) {
    mkcl_core.bytes_consed = mkcl_alloc_raw_bignum(env);
    mpz_init2(mkcl_core.bytes_consed->big.big_num, 128);
    mkcl_core.gc_counter = mkcl_alloc_raw_bignum(env);
    mpz_init2(mkcl_core.gc_counter->big.big_num, 128);
  }

  {
    /* This used to be in the finalizer but
       calls to GC routines from the finalizer do not work
       anymore since 7.2alpha4. JCB
    */
    /* This is not accurate and may wrap around. We try
       to detect this assuming that an overflow in an
       unsigned integer will produce a smaller
       integer. */
    static size_t bytes = 0;
    size_t new_bytes = MK_GC_get_total_bytes();

    if (bytes > new_bytes) {
      mkcl_index before_wrap = ~((mkcl_index)0) - bytes;
#if MKCL_LONG_BITS >= MKCL_WORD_BITS
      _mkcl_big_add_ui(mkcl_core.bytes_consed, mkcl_core.bytes_consed, before_wrap);
#else
      {
	mkcl_object x = _mkcl_big_register0();
	_mkcl_big_set_fixnum(x, before_wrap);
	_mkcl_big_add(mkcl_core.bytes_consed, mkcl_core.bytes_consed, x);
      }
#endif
    }
#if MKCL_LONG_BITS >= MKCL_WORD_BITS
    _mkcl_big_add_ui(mkcl_core.bytes_consed, mkcl_core.bytes_consed, new_bytes - bytes);
#else
    {
      mkcl_object x = _mkcl_big_register0();
      _mkcl_big_set_fixnum(x, new_bytes - bytes);
      _mkcl_big_add(mkcl_core.bytes_consed, mkcl_core.bytes_consed, x);
    }
#endif
    bytes = new_bytes;
  }
  
  mkcl_return_3_values(_mkcl_big_register_normalize(env, mkcl_core.bytes_consed),
                       _mkcl_big_register_normalize(env, mkcl_core.gc_counter),
                       old_status);
}

mkcl_object
mk_si_mem_stats(MKCL)
{
  mkcl_call_stack_check(env);
  size_t heap_size = MK_GC_get_heap_size();
  size_t free_bytes = MK_GC_get_free_bytes();

  mkcl_return_3_values(mkcl_make_unsigned_integer(env, heap_size),
                       mkcl_make_unsigned_integer(env, free_bytes),
                       (MK_GC_get_parallel() ? mk_cl_Ct : mk_cl_Cnil));
}

size_t mkcl_GC_get_total_bytes(void)
{
  return MK_GC_get_total_bytes();
}

/*
 * This procedure is invoked after garbage collection. It invokes
 * finalizers for all objects that are to be reclaimed by the
 * colector. Note that we cannot cons because this procedure is
 * invoked with the garbage collection lock on.
 */
static void mkcl_count_GC_collections(void)
{
  if (mkcl_core.gc_stats) {
    mpz_add_ui(mkcl_core.gc_counter->big.big_num, mkcl_core.gc_counter->big.big_num, 1);
  }
  mkcl_core.gc_fast_counter++;

  if (old_MK_GC_start_call_back) old_MK_GC_start_call_back();
}


/**********************************************************
 *		GARBAGE COLLECTION			  *
 **********************************************************/

mkcl_object mk_si_scrub_values(MKCL)
{
  register mkcl_index i;

  env->nvalues = MKCL_MULTIPLE_VALUES_LIMIT;
  for (i = 0; i < MKCL_MULTIPLE_VALUES_LIMIT; i++)
    env->values[i] = mk_cl_Cnil;
  mkcl_return_no_value;
}

mkcl_object mk_si_gc(MKCL, mkcl_narg narg, ...)
{
  mkcl_call_stack_check(env);
  {
    mkcl_object area = mk_cl_Cnil;
    MKCL_RECEIVE_1_OPTIONAL_ARGUMENT(env, MK_SI_gc, narg, 0, narg, &area);
    mk_si_trim_dynamic_cons_stack(env);
    mk_si_scrub_values(env);
    MKCL_GC_NO_INTR(env, MK_GC_gcollect());
    mkcl_return_no_value;
  }
}

mkcl_object
mk_si_gc_dump(MKCL)
{
  MKCL_GC_NO_INTR(env, MK_GC_dump());
  mkcl_return_no_value;
}

mkcl_object
mk_si_gc_off(MKCL)
{
  MKCL_GC_NO_INTR(env, MK_GC_disable());
  mkcl_return_no_value;
}

mkcl_object
mk_si_gc_on(MKCL)
{
  MKCL_GC_NO_INTR(env, MK_GC_enable());
  mkcl_return_no_value;
}

static void mkcl_GC_abort(const char * const msg)
{
  mkcl_env env = MKCL_ENV();

#if 0
  fprintf(stderr, "\nMKCL: MK_GC_abort called with msg = %s.\n", msg);
  fflush(stderr);
#endif
  if (env)
    mk_mt_abandon_thread(env, MK_KEY_gc_abort);
  else
    mkcl_thread_exit(env, MKCL_GC_ABORT); /* This one should never be called unless we're really confused. */
}

#if MKCL_GC_7_2d
static void mkcl_GC_exit(const int status)
{
  mkcl_env env = MKCL_ENV();

#if 0
  fprintf(stderr, "\nMKCL: MK_GC_exit called with status = %d.\n", status);
  fflush(stderr);
#endif
  if (env)
    mk_mt_abandon_thread(env, MK_KEY_gc_exit);
  else
    mkcl_thread_exit(env, MKCL_GC_EXIT); /* This one should never be called unless we're really confused. */
}
#endif

/******************************************************************************************/

#if 0 /* experimental */
mkcl_object mkcl_alloc_pin_bag(MKCL)
{
  mkcl_object bag;

  bag = MKCL_GC_MALLOC(env, sizeof(struct mkcl_pin_bag));

  bag->pin_bag.t = mkcl_t_pin_bag;
  bag->pin_bag.pins = mk_cl_Cnil;
  return bag;
}

mkcl_object mkcl_pin(MKCL, mkcl_object bag, mkcl_object obj)
{
  mkcl_object pin;

  pin = MKCL_GC_MALLOC(env, sizeof(struct mkcl_pin));

  pin->pin.t = mkcl_t_pin;
  pin->pin.this = obj;
  pin->pin.bag = bag;
  pin->pin.left = bag;
  pin->pin.right = bag->pin_bag.pins;

  bag->pin_bag.pins->pin.left = pin;
  bag->pin_bag.pins = pin;

  return pin;
}


mkcl_object mkcl_unpin(MKCL, mkcl_object pin)
{

  if (mkcl_type_of(pin->pin.left) == mkcl_t_pin_bag)
    {
      mkcl_object bag = pin->pin.left;
      mkcl_object right = pin->pin.right;

      bag->pin_bag.pins = right;
      right->pin.left = bag;
    }
  else
    {
      mkcl_object left = pin->pin.left;
      mkcl_object right = pin->pin.right;

      left->pin.right = right;
      right->pin.left = left;
    }

  return pin->pin.this;
}
#endif


mkcl_object mk_si_sample_allocation_statistics(MKCL)
{
  mkcl_object stats = mk_cl_Cnil;

  mkcl_call_stack_check(env);
  if (env->alloc) {
    struct mkcl_alloc_stats alloc = *(env->alloc); /* snapshot */

    if (alloc.process)
      stats = mkcl_cons(env, mkcl_cons(env, MK_MKCL_process, mkcl_make_unsigned_integer(env, alloc.process)), stats);
    if (alloc.UTF_16)
      stats = mkcl_cons(env, mkcl_cons(env, MK_SI_utf_16, mkcl_make_unsigned_integer(env, alloc.UTF_16)), stats);
    if (alloc.UTF_8)
      stats = mkcl_cons(env, mkcl_cons(env, MK_SI_utf_8, mkcl_make_unsigned_integer(env, alloc.UTF_8)), stats);
    if (alloc.clevel_block)
      stats = mkcl_cons(env, mkcl_cons(env, MK_SI_compiled_closure_level, mkcl_make_unsigned_integer(env, alloc.clevel_block)), stats);
    if (alloc.cdisplay)
      stats = mkcl_cons(env, mkcl_cons(env, MK_SI_compiled_closure_display, mkcl_make_unsigned_integer(env, alloc.cdisplay)), stats);
#if 0
    if (alloc.frame)
      stats = mkcl_cons(env, mkcl_cons(env, MK_SI_temp_stack_frame, mkcl_make_unsigned_integer(env, alloc.frame)), stats);
#endif
    if (alloc.foreign)
      stats = mkcl_cons(env, mkcl_cons(env, MK_SI_foreign, mkcl_make_unsigned_integer(env, alloc.foreign)), stats);
    if (alloc.codeblock)
      stats = mkcl_cons(env, mkcl_cons(env, MK_SI_code_block, mkcl_make_unsigned_integer(env, alloc.codeblock)), stats);
    if (alloc.condition_variable)
      stats = mkcl_cons(env, mkcl_cons(env, MK_MT_condition_variable, mkcl_make_unsigned_integer(env, alloc.condition_variable)), stats);
    if (alloc.semaphore)
      stats = mkcl_cons(env, mkcl_cons(env, MK_MT_semaphore, mkcl_make_unsigned_integer(env, alloc.semaphore)), stats);
    if (alloc.rwlock)
      stats = mkcl_cons(env, mkcl_cons(env, MK_MT_rwlock, mkcl_make_unsigned_integer(env, alloc.rwlock)), stats);
    if (alloc.lock)
      stats = mkcl_cons(env, mkcl_cons(env, MK_MT_lock, mkcl_make_unsigned_integer(env, alloc.lock)), stats);
    if (alloc.thread)
      stats = mkcl_cons(env, mkcl_cons(env, MK_MT_thread, mkcl_make_unsigned_integer(env, alloc.thread)), stats);
    if (alloc.structure)
      stats = mkcl_cons(env, mkcl_cons(env, MK_CL_structure_object, mkcl_make_unsigned_integer(env, alloc.structure)), stats);
    if (alloc.pathname)
      stats = mkcl_cons(env, mkcl_cons(env, MK_CL_pathname, mkcl_make_unsigned_integer(env, alloc.pathname)), stats);
    if (alloc.readtable)
      stats = mkcl_cons(env, mkcl_cons(env, MK_CL_readtable, mkcl_make_unsigned_integer(env, alloc.readtable)), stats);
    if (alloc.random)
      stats = mkcl_cons(env, mkcl_cons(env, MK_CL_random_state, mkcl_make_unsigned_integer(env, alloc.random)), stats);
    if (alloc.stream)
      stats = mkcl_cons(env, mkcl_cons(env, MK_CL_stream, mkcl_make_unsigned_integer(env, alloc.stream)), stats);
    if (alloc.hashtable)
      stats = mkcl_cons(env, mkcl_cons(env, MK_CL_hash_table, mkcl_make_unsigned_integer(env, alloc.hashtable)), stats);
    if (alloc.instance)
      stats = mkcl_cons(env, mkcl_cons(env, MK_CL_standard_object, mkcl_make_unsigned_integer(env, alloc.instance)), stats);
    if (alloc.bclosure)
      stats = mkcl_cons(env, mkcl_cons(env, MK_SI_bytecode_closure, mkcl_make_unsigned_integer(env, alloc.bclosure)), stats);
    if (alloc.bytecode)
      stats = mkcl_cons(env, mkcl_cons(env, MK_SI_bytecode, mkcl_make_unsigned_integer(env, alloc.bytecode)), stats);
    if (alloc.cclosure)
      stats = mkcl_cons(env, mkcl_cons(env, MK_SI_compiled_closure, mkcl_make_unsigned_integer(env, alloc.cclosure)), stats);
    if (alloc.cfun)
      stats = mkcl_cons(env, mkcl_cons(env, MK_CL_compiled_function, mkcl_make_unsigned_integer(env, alloc.cfun)), stats);
    if (alloc.bitvector)
      stats = mkcl_cons(env, mkcl_cons(env, MK_CL_bit_vector, mkcl_make_unsigned_integer(env, alloc.bitvector)), stats);
    if (alloc.base_string)
      stats = mkcl_cons(env, mkcl_cons(env, MK_CL_base_string, mkcl_make_unsigned_integer(env, alloc.base_string)), stats);
    if (alloc.string)
      stats = mkcl_cons(env, mkcl_cons(env, MK_CL_string, mkcl_make_unsigned_integer(env, alloc.string)), stats);
    if (alloc.vector)
      stats = mkcl_cons(env, mkcl_cons(env, MK_CL_vector, mkcl_make_unsigned_integer(env, alloc.vector)), stats);
    if (alloc.array)
      stats = mkcl_cons(env, mkcl_cons(env, MK_CL_array, mkcl_make_unsigned_integer(env, alloc.array)), stats);
    if (alloc.package)
      stats = mkcl_cons(env, mkcl_cons(env, MK_CL_package, mkcl_make_unsigned_integer(env, alloc.package)), stats);
    if (alloc.symbol)
      stats = mkcl_cons(env, mkcl_cons(env, MK_CL_symbol, mkcl_make_unsigned_integer(env, alloc.symbol)), stats);
    if (alloc.complex)
      stats = mkcl_cons(env, mkcl_cons(env, MK_CL_complex, mkcl_make_unsigned_integer(env, alloc.complex)), stats);
    if (alloc.longfloat)
      stats = mkcl_cons(env, mkcl_cons(env, MK_CL_long_float, mkcl_make_unsigned_integer(env, alloc.longfloat)), stats);
    if (alloc.doublefloat)
      stats = mkcl_cons(env, mkcl_cons(env, MK_CL_double_float, mkcl_make_unsigned_integer(env, alloc.doublefloat)), stats);
    if (alloc.singlefloat)
      stats = mkcl_cons(env, mkcl_cons(env, MK_CL_single_float, mkcl_make_unsigned_integer(env, alloc.singlefloat)), stats);
    if (alloc.ratio)
      stats = mkcl_cons(env, mkcl_cons(env, MK_CL_ratio, mkcl_make_unsigned_integer(env, alloc.ratio)), stats);
    if (alloc.bignum)
      stats = mkcl_cons(env, mkcl_cons(env, MK_CL_bignum, mkcl_make_unsigned_integer(env, alloc.bignum)), stats);
    if (alloc.cons)
      stats = mkcl_cons(env, mkcl_cons(env, MK_CL_cons, mkcl_make_unsigned_integer(env, alloc.cons)), stats);
  }
  mkcl_return_value(stats);
}


mkcl_object
mk_si_room_report(MKCL, mkcl_object label)
{
  mkcl_call_stack_check(env);
  if (mkcl_type_of(label) == mkcl_t_string)
    label = mkcl_coerce_to_base_string(env, label);
  if (mkcl_type_of(label) == mkcl_t_base_string)
      fprintf(stderr, "\n%s:\n", label->base_string.self);
  
  if (env->alloc) {
    struct mkcl_alloc_stats alloc = *(env->alloc); /* snapshot */

    if (alloc.process)
      fprintf(stderr, "\tprocess: %lu\n", (unsigned long) alloc.process);
    if (alloc.UTF_16)
      fprintf(stderr, "\tUTF-16: %lu\n", (unsigned long) alloc.UTF_16);
    if (alloc.UTF_8)
      fprintf(stderr, "\tUTF-8: %lu\n", (unsigned long) alloc.UTF_8);
    if (alloc.clevel_block)
      fprintf(stderr, "\tcompiled-closure-level: %lu\n", (unsigned long) alloc.clevel_block);
    if (alloc.cdisplay)
      fprintf(stderr, "\tcompiled-closure-display: %lu\n", (unsigned long) alloc.cdisplay);
#if 0
    if (alloc.frame)
      fprintf(stderr, "\tframe: %lu\n", (unsigned long) alloc.frame);
#endif
    if (alloc.foreign)
      fprintf(stderr, "\tforeign: %lu\n", (unsigned long) alloc.foreign);
    if (alloc.codeblock)
      fprintf(stderr, "\tcode-block: %lu\n", (unsigned long) alloc.codeblock);
    if (alloc.condition_variable)
      fprintf(stderr, "\tcondition-variable: %lu\n", (unsigned long) alloc.condition_variable);
    if (alloc.semaphore)
      fprintf(stderr, "\tsemaphore: %lu\n", (unsigned long) alloc.semaphore);
    if (alloc.rwlock)
      fprintf(stderr, "\trwlock: %lu\n", (unsigned long) alloc.rwlock);
    if (alloc.lock)
      fprintf(stderr, "\tlock: %lu\n", (unsigned long) alloc.lock);
    if (alloc.thread)
      fprintf(stderr, "\tthread: %lu\n", (unsigned long) alloc.thread);
    if (alloc.structure)
      fprintf(stderr, "\tstructure: %lu\n", (unsigned long) alloc.structure);
    if (alloc.pathname)
      fprintf(stderr, "\tpathname: %lu\n", (unsigned long) alloc.pathname);
    if (alloc.readtable)
      fprintf(stderr, "\treadtable: %lu\n", (unsigned long) alloc.readtable);
    if (alloc.random)
      fprintf(stderr, "\trandom: %lu\n", (unsigned long) alloc.random);
    if (alloc.stream)
      fprintf(stderr, "\tstream: %lu\n", (unsigned long) alloc.stream);
    if (alloc.hashtable)
      fprintf(stderr, "\thashtable: %lu\n", (unsigned long) alloc.hashtable);
    if (alloc.instance)
      fprintf(stderr, "\tstandard-object: %lu\n", (unsigned long) alloc.instance);
    if (alloc.bclosure)
      fprintf(stderr, "\tbytecode-closure: %lu\n", (unsigned long) alloc.bclosure);
    if (alloc.bytecode)
      fprintf(stderr, "\tbytecode-function: %lu\n", (unsigned long) alloc.bytecode);
    if (alloc.cclosure)
      fprintf(stderr, "\tcompiled-closure: %lu\n", (unsigned long) alloc.cclosure);
    if (alloc.cfun)
      fprintf(stderr, "\tcompiled-function: %lu\n", (unsigned long) alloc.cfun);
    if (alloc.bitvector)
      fprintf(stderr, "\tbit-vector: %lu\n", (unsigned long) alloc.bitvector);
    if (alloc.base_string)
      fprintf(stderr, "\tbase-string: %lu\n", (unsigned long) alloc.base_string);
    if (alloc.string)
      fprintf(stderr, "\tstring: %lu\n", (unsigned long) alloc.string);
    if (alloc.vector)
      fprintf(stderr, "\tvector: %lu\n", (unsigned long) alloc.vector);
    if (alloc.array)
      fprintf(stderr, "\tarray: %lu\n", (unsigned long) alloc.array);
    if (alloc.package)
      fprintf(stderr, "\tpackage: %lu\n", (unsigned long) alloc.package);
    if (alloc.symbol)
      fprintf(stderr, "\tsymbol: %lu\n", (unsigned long) alloc.symbol);
    if (alloc.complex)
      fprintf(stderr, "\tcomplex: %lu\n", (unsigned long) alloc.complex);
    if (alloc.longfloat)
      fprintf(stderr, "\tlong-float: %lu\n", (unsigned long) alloc.longfloat);
    if (alloc.doublefloat)
      fprintf(stderr, "\tdouble-float: %lu\n", (unsigned long) alloc.doublefloat);
    if (alloc.singlefloat)
      fprintf(stderr, "\tsingle-float: %lu\n", (unsigned long) alloc.singlefloat);
    if (alloc.ratio)
      fprintf(stderr, "\tratio: %lu\n", (unsigned long) alloc.ratio);
    if (alloc.bignum)
      fprintf(stderr, "\tbignum: %lu\n", (unsigned long) alloc.bignum);
    if (alloc.cons)
      fprintf(stderr, "\tcons: %lu\n", (unsigned long) alloc.cons);
    fflush(stderr);
  }
  mkcl_return_value(mk_cl_Cnil);
}


