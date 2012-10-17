/* -*- mode: c -*- */
/*
    package.d -- Packages.
*/
/*
    Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
    Copyright (c) 1990, Giuseppe Attardi.
    Copyright (c) 2001, Juan Jose Garcia Ripoll.
    Copyright (c) 2011-2012, Jean-Claude Beaudoin.

    MKCL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 3 of the License, or (at your option) any later version.

    See file '../../Copyright' for full details.
*/

#include <mkcl/mkcl.h>
#include <mkcl/internal.h>
#include <mkcl/mkcl-inl.h>

#include <limits.h>

/******************************* ------- ******************************/
/*
 * NOTE 1: we only need to use the package locks when reading/writing the hash
 * tables, or changing the fields of a package.  We do not need the locks to
 * read lists from the packages (i.e. list of shadowing symbols, used
 * packages, etc), or from the global environment (mkcl_core.packages_list) if
 * we do not destructively modify them (For instance, use mkcl_remove_eq
 * instead of mkcl_delete_eq).
 */
/*
 * NOTE 2: Operations between locks must be guaranteed not fail, or, if
 * they signal an error, they should undo all locks they had before.
 */

#define	MKCL_SYMBOL_IS_INTERNAL	1
#define	MKCL_SYMBOL_IS_EXTERNAL	2
#define	MKCL_SYMBOL_IS_INHERITED	3

static void
mkcl_FEpackage_error(MKCL, mkcl_object package, char *message, int narg, ...)
{
  mkcl_va_list args;
  mkcl_va_start(env, args, narg, narg, 0);
  mk_cl_error(env, 7,
	      @'si::simple-package-error',
	      @':format-control', mkcl_make_simple_base_string(env, message),
	      @':format-arguments', (narg ? mkcl_grab_rest_args(env, args, FALSE) : mkcl_list1(env, package)),
	      @':package', package);
}

void
mkcl_CEpackage_error(MKCL, mkcl_object package, char *message, char *continue_message, int narg, ...)
{
  mkcl_va_list args;
  mkcl_va_start(env, args, narg, narg, 0);
  mk_cl_cerror(env, 8,
	       mkcl_make_simple_base_string(env, continue_message),
	       @'si::simple-package-error',
	       @':format-control', mkcl_make_simple_base_string(env, message),
	       @':format-arguments', (narg ? mkcl_grab_rest_args(env, args, FALSE) : mkcl_list1(env, package)),
	       @':package', package);
}

static bool
member_string_eq(MKCL, mkcl_object x, mkcl_object l)
{
  /* INV: l is a proper list */
  mkcl_loop_for_on_unsafe(l) {
    if (mkcl_string_E(env, x, MKCL_CONS_CAR(l)))
      return TRUE;
  } mkcl_end_loop_for_on;
  return FALSE;
}

static inline void
symbol_remove_package(mkcl_object s, mkcl_object p)
{
  if (mkcl_Null(s))
    s = mk_cl_Cnil_symbol;
  if (s->symbol.hpack == p)
    s->symbol.hpack = mk_cl_Cnil;
}

static inline void
symbol_add_package(mkcl_object s, mkcl_object p)
{
  if (mkcl_Null(s))
    s = mk_cl_Cnil_symbol;
  if (s->symbol.hpack == mk_cl_Cnil)
    s->symbol.hpack = p;
}

static mkcl_object
make_package_hashtable(MKCL)
{
  mkcl_index i;
  mkcl_object h;
  mkcl_index hsize = 128;

  h = mkcl_alloc_raw_hashtable(env);
  h->hash.lockable = 0;
  h->hash.test = mkcl_htt_package;
  h->hash.size = hsize;
  h->hash.rehash_size = mkcl_make_singlefloat(env, 1.5f);
  h->hash.threshold = mkcl_make_singlefloat(env, 0.75f);
  h->hash.factor_of_16th = 12; /* that is (round (* 0.75 16)). */
  h->hash.data = NULL; /* for GC sake */
  h->hash.data = (struct mkcl_hashtable_entry **)
    mkcl_alloc(env, hsize * sizeof(struct mkcl_hashtable_entry *));

  /* do clrhash */
  h->hash.entries = 0;
  for(i = 0; i < hsize; i++) {
    h->hash.data[i] = NULL;
  }

  h->hash.search_fun = mkcl_search_hash_package;

#ifdef HASHTABLE_STATS
  h->hash.nb_searches = 0;
  h->hash.probes = 0;
  h->hash.shortest_probe_chain = LONG_MAX;
  h->hash.longest_probe_chain = 0;
  h->hash.shortest_failed_probe_chain = LONG_MAX;
  h->hash.longest_failed_probe_chain = 0;

  mkcl_core.hashtables[mkcl_htt_package] = mkcl_cons(h, mkcl_core.hashtables[mkcl_htt_package]);
#endif

  return h;
}


mkcl_object _mkcl_alloc_package(MKCL, mkcl_object name)
{
  mkcl_object x = mkcl_alloc_raw_package(env);

  x->pack.internal = make_package_hashtable(env);
  x->pack.external = make_package_hashtable(env);
#if defined(MKCL_WINDOWS)
#if 0
  x->pack.lock = CreateMutex(NULL, FALSE, mkcl_handle_debug_name(env, "package lock")); /* FIXME! return status of this? JCB */
#else
  InitializeCriticalSection(&(x->pack.lock));
#endif
#else
  {
    const pthread_mutexattr_t * const mutexattr = mkcl_normal_mutexattr;

    pthread_mutex_init(&x->pack.lock, mutexattr);  /* FIXME! return status of this? JCB */
  }
#endif /* defined(MKCL_WINDOWS) */

  x->pack.name = name;
  x->pack.nicknames = mk_cl_Cnil;
  x->pack.shadowings = mk_cl_Cnil;
  x->pack.uses = mk_cl_Cnil;
  x->pack.usedby = mk_cl_Cnil;
  x->pack.closed = FALSE;

  mk_si_set_finalizer(env, x, mk_cl_Ct);

  return x;
}

/*
	mkcl_make_package(n, ns, ul) makes a package with name n,
	which must be a string or a symbol,
	and nicknames ns, which must be a list of strings or symbols,
	and uses packages in list ul, which must be a list of packages
	or package names i.e. strings or symbols.
*/
mkcl_object
mkcl_make_package(MKCL, mkcl_object name, mkcl_object nicknames, mkcl_object use_list)
{
  mkcl_object x, other = mk_cl_Cnil;
  volatile bool locked = false;

  name = mk_cl_string(env, name);
  mkcl_assert_type_proper_list(env, nicknames);
  mkcl_assert_type_proper_list(env, use_list);

  MKCL_UNWIND_PROTECT_BEGIN(env) {
    MKCL_LIBC_NO_INTR(env, (MKCL_PACKAGE_LIST_LOCK(), locked = true));

    /* 1) Find a similarly named package in the list of packages to be created and use it. */
    if (mkcl_core.packages_to_be_created != MKCL_OBJNULL) {
      mkcl_object l = mkcl_core.packages_to_be_created;
      mkcl_object tail = l;
      while (MKCL_CONSP(l)) {
	mkcl_object pair = MKCL_CONS_CAR(l);
	mkcl_object other_name = MKCL_CONS_CAR(pair);
	if (mkcl_equal(env, other_name, name)
	    || 
	    !mkcl_Null(mkcl_funcall4(env, @+'member', other_name, nicknames, @':test', @+'string=')))
	  {
	    mkcl_object next = MKCL_CONS_CDR(l);
	    x = MKCL_CONS_CDR(pair);
	    if (l == tail) {
	      mkcl_core.packages_to_be_created = next;
	    } else {
	      MKCL_RPLACD(tail, next);
	    }
	    goto INTERN;
	  }
	tail = l;
	l = MKCL_CONS_CDR(l);
      }
    }

    /* 2) Otherwise, try to build a new package */
    if ((other = mkcl_find_package_nolock(env, name)) != mk_cl_Cnil) {
      goto _MKCL_ERROR;
    }

    x = _mkcl_alloc_package(env, name);

  INTERN:;
    mkcl_object good_nicknames = mk_cl_Cnil;

    mkcl_loop_for_in(env, nicknames) {
      mkcl_object nick = mk_cl_string(env, MKCL_CONS_CAR(nicknames));
      if ((other = mkcl_find_package_nolock(env, nick)) != mk_cl_Cnil) {
	name = nick;
	goto _MKCL_ERROR;
      }
      good_nicknames = MKCL_CONS(env, nick, good_nicknames);
    } mkcl_end_loop_for_in;

    x->pack.nicknames = good_nicknames;

    mkcl_loop_for_in(env, use_list) {
      mkcl_object y = mk_si_coerce_to_package(env, MKCL_CONS_CAR(use_list));
      x->pack.uses = MKCL_CONS(env, y, x->pack.uses);
      y->pack.usedby = MKCL_CONS(env, x, y->pack.usedby);
    } mkcl_end_loop_for_in;

    /* 3) Finally, add it to the list of packages */
    mkcl_core.packages = MKCL_CONS(env, x, mkcl_core.packages);
  _MKCL_ERROR:;
  } MKCL_UNWIND_PROTECT_EXIT {
    if (locked) MKCL_PACKAGE_LIST_UNLOCK();
  } MKCL_UNWIND_PROTECT_END;

  if (mkcl_Null(other))
    return(x);
  else
    {
      mkcl_CEpackage_error(env, other,
			   "A package with the name ~A already exists.",
			   "Return existing package",
			   1, name);
      return other;
    }
}

mkcl_object
mkcl_rename_package(MKCL, mkcl_object x, mkcl_object name, mkcl_object nicknames)
{
  mkcl_object y = mk_cl_Cnil;
  volatile bool locked = false;

  name = mk_cl_string(env, name);
  x = mk_si_coerce_to_package(env, x);
  if (x->pack.closed)
    mkcl_CEpackage_error(env, x, "Cannot rename closed package ~S.", "Ignore package closing and proceed", 0);

  mkcl_assert_type_proper_list(env, nicknames);

  MKCL_UNWIND_PROTECT_BEGIN(env) {
    MKCL_LIBC_NO_INTR(env, (MKCL_PACKAGE_LIST_LOCK(), locked = true));

    y = mkcl_find_package_nolock(env, name);
    if ((y != mk_cl_Cnil) && (y != x)) {
      goto _MKCL_ERROR;
    }
    else
      y = mk_cl_Cnil;
    x->pack.name = name;
    x->pack.nicknames = mk_cl_Cnil;
    while (!mkcl_Null(nicknames)) {
      mkcl_object nick = MKCL_CONS_CAR(nicknames);
      if (mkcl_find_package_nolock(env, nick) != x)
	x->pack.nicknames = MKCL_CONS(env, mk_cl_string(env, nick), x->pack.nicknames);
      nicknames = MKCL_CONS_CDR(nicknames);
    }
  _MKCL_ERROR:;
  } MKCL_UNWIND_PROTECT_EXIT {
    if (locked) MKCL_PACKAGE_LIST_UNLOCK();
  } MKCL_UNWIND_PROTECT_END;
  if (!mkcl_Null(y))
    mkcl_FEpackage_error(env, x, "A package with name ~S already exists.", 1, name);
  return x;
}

/*
	mkcl_find_package_nolock(n) seaches for a package with name n, where n is
	a valid string designator, or simply outputs n if it is a
	package.

	This is not a locking routine and someone may replace the list of
	packages while we are scanning it. Nevertheless, the list IS NOT
	be destructively modified, which means that we are on the safe side.
	Routines which need to ensure that the package list remains constant
	should enforce a global lock with MKCL_PACKAGE_LIST_LOCK().
*/
mkcl_object
mkcl_find_package_nolock(MKCL, mkcl_object name)
{
  mkcl_object l, p;

  if (mkcl_type_of(name) == mkcl_t_package)
    return name;
  name = mk_cl_string(env, name);
  l = mkcl_core.packages;
  mkcl_loop_for_on_unsafe(l) {
    p = MKCL_CONS_CAR(l);
    if (mkcl_string_E(env, name, p->pack.name))
      return p;
    if (member_string_eq(env, name, p->pack.nicknames))
      return p;
  } mkcl_end_loop_for_on;
#ifdef MKCL_RELATIVE_PACKAGE_NAMES
  /* Note that this function may actually be called _before_ symbols are set up
   * and bound! */
  if (mkcl_get_option(MKCL_OPT_BOOTED)
      && MKCL_SYM_FUN(@'si::find-relative-package') != mk_cl_Cnil
      && MKCL_SYM_VAL(env, @'si::*relative-package-names*') != mk_cl_Cnil) {
    return mk_si_find_relative_package(env, 1, name);
  }
#endif
  return mk_cl_Cnil;
}

mkcl_object
mk_si_coerce_to_package(MKCL, mkcl_object p)
{
  mkcl_call_stack_check(env);
  /* INV: mkcl_find_package_nolock() signals an error if "p" is neither a package
     nor a string */
  mkcl_object pp = mkcl_find_package_nolock(env, p);
  if (mkcl_Null(pp)) {
    mkcl_FEpackage_error(env, p, "There exists no package with name ~S", 0);
  }
  @(return pp);
}

mkcl_object
mkcl_current_package(MKCL)
{
  mkcl_object x = mkcl_symbol_value(env, @'*package*');
  if (mkcl_type_of(x) != mkcl_t_package) {
    MKCL_SETQ(env, @'*package*', mkcl_core.user_package);
    mkcl_FEerror(env, "The value of *PACKAGE*, ~S, was not a package", 1, x);
  }
  return x;
}

/*
	Mkcl_Intern(st, p) interns string st in package p.
*/
mkcl_object
_mkcl_intern(MKCL, const char *s, mkcl_object p)
{
  int intern_flag;
  mkcl_object str = mkcl_make_simple_base_string(env, (char *) s);
  return mkcl_intern(env, str, p, &intern_flag);
}

mkcl_object
mkcl_intern(MKCL, mkcl_object name, mkcl_object p, int *intern_flag)
{
  mkcl_object s, ul;
  volatile bool locked = false;

  name = mkcl_check_type_string(env, @'intern', name);
  p = mk_si_coerce_to_package(env, p);

  MKCL_UNWIND_PROTECT_BEGIN(env) {
    MKCL_LIBC_NO_INTR(env, (MKCL_PACKAGE_LOCK(p), locked = true));

  TRY_AGAIN_LABEL:
    s = mkcl_gethash_safe(env, name, p->pack.external, MKCL_OBJNULL);
    if (s != MKCL_OBJNULL) {
      *intern_flag = MKCL_SYMBOL_IS_EXTERNAL;
      goto OUTPUT;
    }
    /* Keyword package has no internal section nor can it be used */
    if (p == mkcl_core.keyword_package) goto INTERN;
    s = mkcl_gethash_safe(env, name, p->pack.internal, MKCL_OBJNULL);
    if (s != MKCL_OBJNULL) {
      *intern_flag = MKCL_SYMBOL_IS_INTERNAL;
      goto OUTPUT;
    }
    ul = p->pack.uses;
    mkcl_loop_for_on_unsafe(ul) {
      s = mkcl_gethash_safe(env, name, MKCL_CONS_CAR(ul)->pack.external, MKCL_OBJNULL);
      if (s != MKCL_OBJNULL) {
	*intern_flag = MKCL_SYMBOL_IS_INHERITED;
	goto OUTPUT;
      }
    } mkcl_end_loop_for_on;
  INTERN:
    if (p->pack.closed) {
      volatile bool unlocked = false;
      MKCL_UNWIND_PROTECT_BEGIN(env) {
	MKCL_LIBC_NO_INTR(env, (MKCL_PACKAGE_UNLOCK(p), unlocked = true));

	mkcl_CEpackage_error(env, p,
			     "Cannot intern symbol ~S in closed package ~S.",
			     "Ignore package closing and proceed", 2, name, p);
      } MKCL_UNWIND_PROTECT_EXIT {
	if (unlocked) MKCL_PACKAGE_LOCK(p);
      } MKCL_UNWIND_PROTECT_END;
      goto TRY_AGAIN_LABEL;
    }
    s = mk_cl_make_symbol(env, name);
    s->symbol.hpack = p;
    *intern_flag = 0;
    if (p == mkcl_core.keyword_package) {
      mkcl_symbol_type_set(env, s, mkcl_symbol_type(env, s) | mkcl_stp_constant);
      MKCL_SET(s, s);
      mkcl_sethash(env, name, p->pack.external, s);
    } else {
      mkcl_sethash(env, name, p->pack.internal, s);
    }
  OUTPUT:;
  } MKCL_UNWIND_PROTECT_EXIT {
    if (locked) MKCL_PACKAGE_UNLOCK(p);
  } MKCL_UNWIND_PROTECT_END;
   return s;
}


mkcl_object
mkcl_find_symbol_nolock(MKCL, mkcl_object name, mkcl_object p, int *intern_flag)
{
  mkcl_object s, ul;

  name = mkcl_check_type_string(env, @'find-symbol', name);
  s = mkcl_gethash_safe(env, name, p->pack.external, MKCL_OBJNULL);
  if (s != MKCL_OBJNULL) {
    *intern_flag = MKCL_SYMBOL_IS_EXTERNAL;
    goto OUTPUT;
  }
  if (p == mkcl_core.keyword_package)
    goto NOTHING;
  s = mkcl_gethash_safe(env, name, p->pack.internal, MKCL_OBJNULL);
  if (s != MKCL_OBJNULL) {
    *intern_flag = MKCL_SYMBOL_IS_INTERNAL;
    goto OUTPUT;
  }
  ul = p->pack.uses;
  mkcl_loop_for_on_unsafe(ul) {
    s = mkcl_gethash_safe(env, name, MKCL_CONS_CAR(ul)->pack.external, MKCL_OBJNULL);
    if (s != MKCL_OBJNULL) {
      *intern_flag = MKCL_SYMBOL_IS_INHERITED;
      goto OUTPUT;
    }
  } mkcl_end_loop_for_on;
 NOTHING:
  *intern_flag = 0;
  s = mk_cl_Cnil;
 OUTPUT:
  return s;
}

mkcl_object
mkcl_find_symbol(MKCL, mkcl_object n, mkcl_object p, int *intern_flag)
{
  volatile bool locked = false;

  n = mk_cl_string(env, n); /* This is an extension of the standard that calls for a string, not a string designator. JCB */
  p = mk_si_coerce_to_package(env, p);
  MKCL_UNWIND_PROTECT_BEGIN(env) {
    MKCL_LIBC_NO_INTR(env, (MKCL_PACKAGE_LOCK(p), locked = true));
    n = mkcl_find_symbol_nolock(env, n, p, intern_flag);
  } MKCL_UNWIND_PROTECT_EXIT {
    if (locked) MKCL_PACKAGE_UNLOCK(p);
  } MKCL_UNWIND_PROTECT_END;
  return n;
}

bool
mkcl_unintern(MKCL, mkcl_object s, mkcl_object p)
{
  mkcl_object x, y, l, hash;
  bool output = FALSE;
  mkcl_object name = mkcl_symbol_name(env, s);
  volatile bool locked = false;

  p = mk_si_coerce_to_package(env, p);

  MKCL_UNWIND_PROTECT_BEGIN(env) {
    MKCL_LIBC_NO_INTR(env, (MKCL_PACKAGE_LOCK(p), locked = true));

  TRY_AGAIN_LABEL:
    hash = p->pack.internal;
    x = mkcl_gethash_safe(env, name, hash, MKCL_OBJNULL);
    if (x == s)
      goto UNINTERN;
    hash = p->pack.external;
    x = mkcl_gethash_safe(env, name, hash, MKCL_OBJNULL);
    if (x != s)
      goto OUTPUT;
  UNINTERN:
    if (p->pack.closed) {
      volatile bool unlocked = false;

      MKCL_UNWIND_PROTECT_BEGIN(env) {
	MKCL_LIBC_NO_INTR(env, (MKCL_PACKAGE_UNLOCK(p), unlocked = true));
	mkcl_CEpackage_error(env, p,
			     "Cannot unintern symbol ~S from closed package ~S.",
			     "Ignore package closing and proceed", 2, s, p);
      } MKCL_UNWIND_PROTECT_EXIT {
	if (unlocked) MKCL_PACKAGE_LOCK(p);
      } MKCL_UNWIND_PROTECT_END;
      goto TRY_AGAIN_LABEL;
    }
    if (!mkcl_member_eq(env, s, p->pack.shadowings))
      goto NOT_SHADOW;
    x = MKCL_OBJNULL;
    l = p->pack.uses;
    mkcl_loop_for_on_unsafe(l) {
      y = mkcl_gethash_safe(env, name, MKCL_CONS_CAR(l)->pack.external, MKCL_OBJNULL);
      if (y != MKCL_OBJNULL) {
	if (x == MKCL_OBJNULL)
	  x = y;
	else if (x != y) {
	  volatile bool unlocked = false;

	  MKCL_UNWIND_PROTECT_BEGIN(env) {
	    MKCL_LIBC_NO_INTR(env, (MKCL_PACKAGE_UNLOCK(p), unlocked = true));
	    mkcl_FEpackage_error(env, p,
				 "Cannot unintern the shadowing symbol ~S~%"
				 "from ~S,~%"
				 "because ~S and ~S will cause~%"
				 "a name conflict.", 4, s, p, x, y);
	  } MKCL_UNWIND_PROTECT_EXIT {
	    if (unlocked) MKCL_PACKAGE_LOCK(p);
	  } MKCL_UNWIND_PROTECT_END;
	}
      }
    } mkcl_end_loop_for_on;
    p->pack.shadowings = mkcl_remove_eq(env, s, p->pack.shadowings);
  NOT_SHADOW:
    mkcl_remhash(env, name, hash);
    symbol_remove_package(s, p);
    output = TRUE;
  OUTPUT:;
  } MKCL_UNWIND_PROTECT_EXIT {
    if (locked) MKCL_PACKAGE_UNLOCK(p);
  } MKCL_UNWIND_PROTECT_END;
  return output;
}

void
mkcl_export2(MKCL, mkcl_object s, mkcl_object p)
{
  volatile bool locked = false;
  mkcl_object x, l, hash = MKCL_OBJNULL;
  int intern_flag;
  mkcl_object name = mkcl_symbol_name(env, s);
  p = mk_si_coerce_to_package(env, p);
  if (p->pack.closed)
    mkcl_CEpackage_error(env, p,
			 "Cannot export symbol ~S from closed package ~S.",
			 "Ignore package closing and proceed", 2, s, p);

  MKCL_UNWIND_PROTECT_BEGIN(env) {
    MKCL_LIBC_NO_INTR(env, (MKCL_PACKAGE_LOCK(p), locked = true));

  TRY_AGAIN_LABEL:
    x = mkcl_find_symbol_nolock(env, name, p, &intern_flag);
    if (!intern_flag) {
      volatile bool unlocked = false;

      MKCL_UNWIND_PROTECT_BEGIN(env) {
	MKCL_LIBC_NO_INTR(env, (MKCL_PACKAGE_UNLOCK(p), unlocked = true));
	mkcl_CEpackage_error(env, p,
			     "The symbol ~S is not accessible from ~S and cannot be exported.",
			     "Import the symbol in the package and proceed.",
			     2, s, p);
	/* Where is the import mentionned just above? JCB */
      } MKCL_UNWIND_PROTECT_EXIT {
	if (unlocked) MKCL_PACKAGE_LOCK(p);
      } MKCL_UNWIND_PROTECT_END;
    }
    if (x != s) {
      volatile bool unlocked = false;
      
      MKCL_UNWIND_PROTECT_BEGIN(env) {
	MKCL_LIBC_NO_INTR(env, (MKCL_PACKAGE_UNLOCK(p), unlocked = true));
	mkcl_FEpackage_error(env, p,
			     "Cannot export the symbol ~S from ~S,~%"
			     "because there is already a symbol with the same name~%"
			     "in the package.",
			     2, s, p);
      } MKCL_UNWIND_PROTECT_EXIT {
	if (unlocked) MKCL_PACKAGE_LOCK(p);
      } MKCL_UNWIND_PROTECT_END;
    }
    if (intern_flag == MKCL_SYMBOL_IS_EXTERNAL)
      goto OUTPUT;
    if (intern_flag == MKCL_SYMBOL_IS_INTERNAL)
      hash = p->pack.internal;
    l = p->pack.usedby;
    mkcl_loop_for_on_unsafe(l) {
      x = mkcl_find_symbol_nolock(env, name, MKCL_CONS_CAR(l), &intern_flag);
      if (intern_flag && s != x &&
	  !mkcl_member_eq(env, x, MKCL_CAR(l)->pack.shadowings)) {
	volatile bool unlocked = false;

	MKCL_UNWIND_PROTECT_BEGIN(env) {
	  MKCL_LIBC_NO_INTR(env, (MKCL_PACKAGE_UNLOCK(p), unlocked = true));
	  mkcl_FEpackage_error(env, p,
			       "Cannot export the symbol ~S~%"
			       "from ~S,~%"
			       "because it will cause a name conflict~%"
			       "in ~S.",
			       3, s, p, MKCL_CAR(l));
	} MKCL_UNWIND_PROTECT_EXIT {
	  if (unlocked) MKCL_PACKAGE_LOCK(p);
	} MKCL_UNWIND_PROTECT_END;
      }
    } mkcl_end_loop_for_on;
    if (hash != MKCL_OBJNULL)
      mkcl_remhash(env, name, hash);
    mkcl_sethash(env, name, p->pack.external, s);
  OUTPUT:;
  } MKCL_UNWIND_PROTECT_EXIT {
    if (locked) MKCL_PACKAGE_UNLOCK(p);
  } MKCL_UNWIND_PROTECT_END;
}

mkcl_object
mk_cl_delete_package(MKCL, mkcl_object p)
{
  mkcl_object hash, list;
  mkcl_index i;
  volatile bool locked = false;

  mkcl_call_stack_check(env);
  /* 1) Try to remove the package from the global list */
  p = mkcl_find_package_nolock(env, p);
  if (mkcl_Null(p)) {
    mkcl_CEpackage_error(env, p,
			 "Package ~S not found. Cannot delete it.",
			 "Ignore error and continue", 0);
    @(return mk_cl_Cnil);
  }
  if (p->pack.closed)
    mkcl_CEpackage_error(env, p,
			 "Cannot delete closed package ~S.",
			 "Ignore package closing and proceed", 0);
  if (p == mkcl_core.lisp_package || p == mkcl_core.keyword_package) {
    mkcl_FEpackage_error(env, p, "Cannot remove package ~S", 0);
  }

  /* 2) Now remove the package from the other packages that use it
   *    and empty the package.
   */
  if (mkcl_Null(p->pack.name)) {
    @(return mk_cl_Cnil);
  }
  list = p->pack.uses;
  mkcl_loop_for_on_unsafe(list) {
    mkcl_unuse_package(env, MKCL_CONS_CAR(list), p);
  } mkcl_end_loop_for_on;
  list = p->pack.usedby;
  if (!mkcl_Null(list))
    {
      mkcl_CEpackage_error(env, p, "Cannot delete package ~S because it is used by these other packages: ~S.",
			   "Unuse this package from each of its users and then delete it", 1, p, list);
      mkcl_loop_for_on_unsafe(list) {
	mkcl_unuse_package(env, p, MKCL_CONS_CAR(list));
      } mkcl_end_loop_for_on;
    }
  MKCL_UNWIND_PROTECT_BEGIN(env) {
    MKCL_LIBC_NO_INTR(env, (MKCL_PACKAGE_LOCK(p), locked = true));

    for (hash = p->pack.internal, i = 0; i < hash->hash.size; i++)
      {
	struct mkcl_hashtable_entry * e = hash->hash.data[i];

	for (; e != NULL; e = e->next)
	  symbol_remove_package(e->value, p);
      }
    mk_cl_clrhash(env, p->pack.internal);
    for (hash = p->pack.external, i = 0; i < hash->hash.size; i++)
      {
	struct mkcl_hashtable_entry * e = hash->hash.data[i];

	for (; e != NULL; e = e->next)
	  symbol_remove_package(e->value, p);
      }
    mk_cl_clrhash(env, p->pack.external);
    p->pack.shadowings = mk_cl_Cnil;
    p->pack.name = mk_cl_Cnil;
  } MKCL_UNWIND_PROTECT_EXIT {
    if (locked) MKCL_PACKAGE_UNLOCK(p);
  } MKCL_UNWIND_PROTECT_END;

  /* 2) Only at the end, remove the package from the list of packages. */
  locked = false;
  MKCL_UNWIND_PROTECT_BEGIN(env) {
    MKCL_LIBC_NO_INTR(env, (MKCL_PACKAGE_LIST_LOCK(), locked = true));
    mkcl_core.packages = mkcl_remove_eq(env, p, mkcl_core.packages);
  } MKCL_UNWIND_PROTECT_EXIT {
    if (locked) MKCL_PACKAGE_LIST_UNLOCK();
  } MKCL_UNWIND_PROTECT_END;
  @(return mk_cl_Ct);
}

void
mkcl_unexport2(MKCL, mkcl_object s, mkcl_object p)
{
  volatile bool locked = false;
  int intern_flag;
  mkcl_object x;
  mkcl_object name = mkcl_symbol_name(env, s);
  p = mk_si_coerce_to_package(env, p);
  if (p == mkcl_core.keyword_package)
    mkcl_FEpackage_error(env, mkcl_core.keyword_package, 
			 "Cannot unexport a symbol from the keyword package.", 0);
  if (p->pack.closed)
    mkcl_CEpackage_error(env, p,
			 "Cannot unexport symbol ~S from closed package ~S.",
			 "Ignore package closing and proceed", 2, s, p);

  MKCL_UNWIND_PROTECT_BEGIN(env) {
    MKCL_LIBC_NO_INTR(env, (MKCL_PACKAGE_LOCK(p), locked = true));

    x = mkcl_find_symbol_nolock(env, name, p, &intern_flag);
    if (intern_flag == 0 || x != s) {
      volatile bool unlocked = false;

      MKCL_UNWIND_PROTECT_BEGIN(env) {
	MKCL_LIBC_NO_INTR(env, (MKCL_PACKAGE_UNLOCK(p), unlocked = true));
	mkcl_FEpackage_error(env, p,
			     "Cannot unexport ~S because it does not belong to package ~S.",
			     2, s, p);
      } MKCL_UNWIND_PROTECT_EXIT {
	if (unlocked) MKCL_PACKAGE_LOCK(p);
      } MKCL_UNWIND_PROTECT_END;
    }
    if (intern_flag != MKCL_SYMBOL_IS_EXTERNAL) {
      /* According to ANSI & Cltl, internal symbols are
	 ignored in unexport */
      (void)0;
    } else {
      mkcl_remhash(env, name, p->pack.external);
      mkcl_sethash(env, name, p->pack.internal, s);
    }
  } MKCL_UNWIND_PROTECT_EXIT {
    if (locked) MKCL_PACKAGE_UNLOCK(p);
  } MKCL_UNWIND_PROTECT_END;
}

void
mkcl_import2(MKCL, mkcl_object s, mkcl_object p)
{
  volatile bool locked = false;
  int intern_flag;
  mkcl_object x;
  mkcl_object name = mkcl_symbol_name(env, s);
  p = mk_si_coerce_to_package(env, p);
  if (p->pack.closed)
    mkcl_CEpackage_error(env, p,
			 "Cannot import symbol ~S into closed package ~S.",
			 "Ignore package closing and proceed", 2, s, p);

  MKCL_UNWIND_PROTECT_BEGIN(env) {
    MKCL_LIBC_NO_INTR(env, (MKCL_PACKAGE_LOCK(p), locked = true));

    x = mkcl_find_symbol_nolock(env, name, p, &intern_flag);
    if (intern_flag) {
      if (x != s) {
	volatile bool unlocked = false;

	MKCL_UNWIND_PROTECT_BEGIN(env) {
	  MKCL_LIBC_NO_INTR(env, (MKCL_PACKAGE_UNLOCK(p), unlocked = true));
	  mkcl_CEpackage_error(env, p,
			       "Cannot import the symbol ~S "
			       "into package ~A,~%"
			       "because there is already a symbol with the same name~%"
			       "in the package.",
			       "Ignore conflict and proceed", 2, s, p);
	} MKCL_UNWIND_PROTECT_EXIT {
	  if (unlocked) MKCL_PACKAGE_LOCK(p);
	} MKCL_UNWIND_PROTECT_END;
      }
      if (intern_flag == MKCL_SYMBOL_IS_INTERNAL || intern_flag == MKCL_SYMBOL_IS_EXTERNAL)
	goto OUTPUT;
    }
    mkcl_sethash(env, name, p->pack.internal, s);
    symbol_add_package(s, p);
  OUTPUT:;
  } MKCL_UNWIND_PROTECT_EXIT {
    if (locked) MKCL_PACKAGE_UNLOCK(p);
  } MKCL_UNWIND_PROTECT_END;
}

void
mkcl_shadowing_import(MKCL, mkcl_object s, mkcl_object p)
{
  volatile bool locked = false;
  int intern_flag;
  mkcl_object x;
  mkcl_object name = mkcl_symbol_name(env, s);
  p = mk_si_coerce_to_package(env, p);
  if (p->pack.closed)
    mkcl_CEpackage_error(env, p,
			 "Cannot shadowing-import symbol ~S into closed package ~S.",
			 "Ignore package closing and proceed", 2, s, p);

  MKCL_UNWIND_PROTECT_BEGIN(env) {
    MKCL_LIBC_NO_INTR(env, (MKCL_PACKAGE_LOCK(p), locked = true));

    x = mkcl_find_symbol_nolock(env, name, p, &intern_flag);
    if (intern_flag && intern_flag != MKCL_SYMBOL_IS_INHERITED) {
      if (x == s) {
	if (!mkcl_member_eq(env, x, p->pack.shadowings))
	  p->pack.shadowings
	    = MKCL_CONS(env, x, p->pack.shadowings);
	goto OUTPUT;
      }
      if(mkcl_member_eq(env, x, p->pack.shadowings))
	p->pack.shadowings = mkcl_remove_eq(env, x, p->pack.shadowings);
      if (intern_flag == MKCL_SYMBOL_IS_INTERNAL)
	mkcl_remhash(env, name, p->pack.internal);
      else
	mkcl_remhash(env, name, p->pack.external);
      symbol_remove_package(x, p);
    }
    p->pack.shadowings = MKCL_CONS(env, s, p->pack.shadowings);
    mkcl_sethash(env, name, p->pack.internal, s);
  OUTPUT:;
  } MKCL_UNWIND_PROTECT_EXIT {
    if (locked) MKCL_PACKAGE_UNLOCK(p);
  } MKCL_UNWIND_PROTECT_END;
}

void
mkcl_shadow(MKCL, mkcl_object s, mkcl_object p)
{
  volatile bool locked = false;
  int intern_flag;
  mkcl_object x;

  /* Contrary to CLTL, in ANSI CL, SHADOW operates on strings. */
  s = mk_cl_string(env, s);
  p = mk_si_coerce_to_package(env, p);
  if (p->pack.closed)
    mkcl_CEpackage_error(env, p,
			 "Cannot shadow symbol ~S in closed package ~S.",
			 "Ignore package closing and proceed", 2, s, p);

  MKCL_UNWIND_PROTECT_BEGIN(env) {
    MKCL_LIBC_NO_INTR(env, (MKCL_PACKAGE_LOCK(p), locked = true));

    x = mkcl_find_symbol_nolock(env, s, p, &intern_flag);
    if (intern_flag != MKCL_SYMBOL_IS_INTERNAL && intern_flag != MKCL_SYMBOL_IS_EXTERNAL) {
      x = mk_cl_make_symbol(env, s);
      mkcl_sethash(env, s, p->pack.internal, x);
      x->symbol.hpack = p;
    }
    p->pack.shadowings = MKCL_CONS(env, x, p->pack.shadowings);
  } MKCL_UNWIND_PROTECT_EXIT {
    if (locked) MKCL_PACKAGE_UNLOCK(p);
  } MKCL_UNWIND_PROTECT_END;
}

void
mkcl_use_package(MKCL, mkcl_object x, mkcl_object p)
{
  struct mkcl_hashtable_entry *hash_entries;
  mkcl_index i, hash_length;
  int intern_flag;
  volatile bool locked = false;

  x = mk_si_coerce_to_package(env, x);
  if (x == mkcl_core.keyword_package)
    mkcl_FEpackage_error(env, mkcl_core.keyword_package, "Cannot use keyword package.", 0);
  p = mk_si_coerce_to_package(env, p);
  if (p->pack.closed)
    mkcl_CEpackage_error(env, p,
			 "Cannot use package ~S in closed package ~S.",
			 "Ignore package closing and proceed", 2, x, p);
  if (p == mkcl_core.keyword_package)
    mkcl_FEpackage_error(env, mkcl_core.keyword_package, "Cannot use in keyword package.", 0);
  if (p == x)
    return;
  if (mkcl_member_eq(env, x, p->pack.uses))
    return;

  MKCL_UNWIND_PROTECT_BEGIN(env) {
    /* This has *deadlock* written all over it! JCB */
    MKCL_LIBC_NO_INTR(env, (MKCL_PACKAGE_LOCK(x), MKCL_PACKAGE_LOCK(p), locked = true));

    hash_length = x->pack.external->hash.size;
    for (i = 0;  i < hash_length;  i++)
      {
	hash_entries = x->pack.external->hash.data[i];

	for (; hash_entries != NULL; hash_entries = hash_entries->next)
	  {
	    mkcl_object here = hash_entries->value;
	    mkcl_object name = mkcl_symbol_name(env, here);
	    mkcl_object there = mkcl_find_symbol_nolock(env, name, p, &intern_flag);
	    if (intern_flag && here != there
		&& ! mkcl_member_eq(env, there, p->pack.shadowings)) {
	      volatile bool unlocked = false;
	      MKCL_UNWIND_PROTECT_BEGIN(env) {
		MKCL_LIBC_NO_INTR(env, (MKCL_PACKAGE_UNLOCK(p), MKCL_PACKAGE_UNLOCK(x), unlocked = true));
		mkcl_FEpackage_error(env, p,
				     "Cannot use ~S~%"
				     "from ~S,~%"
				     "because ~S and ~S will cause~%"
				     "a name conflict.",
				     4, x, p, here, there);
	      } MKCL_UNWIND_PROTECT_EXIT {
		if (unlocked) {
		  MKCL_PACKAGE_LOCK(x);
		  MKCL_PACKAGE_LOCK(p);
		}
	      } MKCL_UNWIND_PROTECT_END;
	    }
	  }
      }

    p->pack.uses = MKCL_CONS(env, x, p->pack.uses);
    x->pack.usedby = MKCL_CONS(env, p, x->pack.usedby);
  } MKCL_UNWIND_PROTECT_EXIT {
    if (locked) {
      MKCL_PACKAGE_UNLOCK(p);
      MKCL_PACKAGE_UNLOCK(x);
    }
  } MKCL_UNWIND_PROTECT_END;
}

void
mkcl_unuse_package(MKCL, mkcl_object x, mkcl_object p)
{
  volatile bool locked = false;

  x = mk_si_coerce_to_package(env, x);
  p = mk_si_coerce_to_package(env, p);
  if (p->pack.closed)
    mkcl_CEpackage_error(env, p, "Cannot unuse package ~S from closed package ~S.",
			 "Ignore package closing and proceed", 2, x, p);

  MKCL_UNWIND_PROTECT_BEGIN(env) {
    /* This has *deadlock* written all over it! JCB */
    MKCL_LIBC_NO_INTR(env, (MKCL_PACKAGE_LOCK(x), MKCL_PACKAGE_LOCK(p), locked = true));

    p->pack.uses = mkcl_remove_eq(env, x, p->pack.uses);
    x->pack.usedby = mkcl_remove_eq(env, p, x->pack.usedby);
  } MKCL_UNWIND_PROTECT_EXIT {
    if (locked) {
      MKCL_PACKAGE_UNLOCK(p);
      MKCL_PACKAGE_UNLOCK(x);
    }
  } MKCL_UNWIND_PROTECT_END;
}

@(defun make_package (pack_name &key nicknames (use MKCL_CONS(env, mkcl_core.lisp_package, mk_cl_Cnil)))
@
  /* INV: mkcl_make_package() performs type checking */
  @(return mkcl_make_package(env, pack_name, nicknames, use))
@)

mkcl_object
mk_si_select_package(MKCL, mkcl_object pack_name)
{
  mkcl_call_stack_check(env);
  mkcl_object p = mk_si_coerce_to_package(env, pack_name);
  @(return (MKCL_SETQ(env, @'*package*', p)));
}

mkcl_object
mk_cl_find_package(MKCL, mkcl_object p)
{
  mkcl_object package = mk_cl_Cnil;
  bool locked = false;

  mkcl_call_stack_check(env);
  MKCL_UNWIND_PROTECT_BEGIN(env) {
    MKCL_LIBC_NO_INTR(env, (MKCL_PACKAGE_LIST_LOCK(), locked = true));
    package = mkcl_find_package_nolock(env, p);
  } MKCL_UNWIND_PROTECT_EXIT {
    if (locked) MKCL_PACKAGE_LIST_UNLOCK();
  } MKCL_UNWIND_PROTECT_END;

  @(return package);
}

mkcl_object
mk_cl_package_name(MKCL, mkcl_object p)
{
  mkcl_call_stack_check(env);
  p = mk_si_coerce_to_package(env, p);
  @(return mkcl_copy_string(env, p->pack.name));
}

mkcl_object
mk_cl_package_nicknames(MKCL, mkcl_object p)
{
  mkcl_call_stack_check(env);
  /* FIXME: list should be a fresh one */
  p = mk_si_coerce_to_package(env, p);
  @(return mk_cl_copy_list(env, p->pack.nicknames));
}

@(defun rename_package (pack new_name &o new_nicknames)
@
  /* INV: mkcl_rename_package() type checks and coerces pack to package */
  @(return mkcl_rename_package(env, pack, new_name, new_nicknames))
@)

mkcl_object
mk_cl_package_use_list(MKCL, mkcl_object p)
{
  mkcl_call_stack_check(env);
  return mk_cl_copy_list(env, mk_si_coerce_to_package(env, p)->pack.uses);
}

mkcl_object
mk_cl_package_used_by_list(MKCL, mkcl_object p)
{
  mkcl_call_stack_check(env);
  return mk_cl_copy_list(env, mk_si_coerce_to_package(env, p)->pack.usedby);
}

mkcl_object
mk_cl_package_shadowing_symbols(MKCL, mkcl_object p)
{
  mkcl_call_stack_check(env);
  return mk_cl_copy_list(env, mk_si_coerce_to_package(env, p)->pack.shadowings);
}

mkcl_object
mk_si_close_package(MKCL, mkcl_object p)
{
  mkcl_call_stack_check(env);
  p = mk_si_coerce_to_package(env, p);
  p->pack.closed = TRUE;
  @(return p);
}

mkcl_object
mk_si_reopen_package(MKCL, mkcl_object p)
{
  mkcl_call_stack_check(env);
  p = mk_si_coerce_to_package(env, p);
  p->pack.closed = FALSE;
  @(return p);
}

mkcl_object
mk_si_package_closed_p(MKCL, mkcl_object p)
{
  mkcl_call_stack_check(env);
  p = mk_si_coerce_to_package(env, p);
  @(return (p->pack.closed ? mk_cl_Ct : mk_cl_Cnil));
}

mkcl_object
mk_cl_list_all_packages(MKCL)
{
  mkcl_object packages = mk_cl_Cnil;
  bool locked = false;

  mkcl_call_stack_check(env);
  MKCL_UNWIND_PROTECT_BEGIN(env) {
    MKCL_LIBC_NO_INTR(env, (MKCL_PACKAGE_LIST_LOCK(), locked = true));
    packages = mk_cl_copy_list(env, mkcl_core.packages);
  } MKCL_UNWIND_PROTECT_EXIT {
    if (locked) MKCL_PACKAGE_LIST_UNLOCK();
  } MKCL_UNWIND_PROTECT_END;

  @(return packages);
}

@(defun intern (strng &optional (p mkcl_current_package(env)) &aux sym)
	int intern_flag;
@
  sym = mkcl_intern(env, strng, p, &intern_flag);
  if (intern_flag == MKCL_SYMBOL_IS_INTERNAL)
    { @(return sym @':internal'); }
  else if (intern_flag == MKCL_SYMBOL_IS_EXTERNAL)
    { @(return sym @':external'); }
  else if (intern_flag == MKCL_SYMBOL_IS_INHERITED)
    { @(return sym @':inherited'); }
  else
    { @(return sym mk_cl_Cnil); }
@)

@(defun find_symbol (strng &optional (p mkcl_current_package(env)))
	mkcl_object x;
	int intern_flag;
@
  x = mkcl_find_symbol(env, strng, p, &intern_flag);
  if (intern_flag == MKCL_SYMBOL_IS_INTERNAL)
    { @(return x @':internal'); }
  else if (intern_flag == MKCL_SYMBOL_IS_EXTERNAL)
    { @(return x @':external'); }
  else if (intern_flag == MKCL_SYMBOL_IS_INHERITED)
    { @(return x @':inherited'); }
  else
    { @(return mk_cl_Cnil mk_cl_Cnil); }
@)

@(defun unintern (symbl &optional (p mkcl_current_package(env)))
@
  @(return (mkcl_unintern(env, symbl, p) ? mk_cl_Ct : mk_cl_Cnil));
@)

@(defun export (symbols &o (pack mkcl_current_package(env)))
@
BEGIN:
  switch (mkcl_type_of(symbols)) {
  case mkcl_t_symbol:
    mkcl_export2(env, symbols, pack);
    break;
    
  case mkcl_t_null:
  case mkcl_t_cons:
    pack = mk_si_coerce_to_package(env, pack);
    mkcl_loop_for_in(env, symbols) {
      mkcl_export2(env, MKCL_CONS_CAR(symbols), pack);
    } mkcl_end_loop_for_in;
    break;
    
  default:
    symbols = mkcl_type_error(env, @'export', "argument", symbols,
			     mk_cl_list(env, 3, @'or', @'symbol', @'list'));
    goto BEGIN;
  }
  @(return mk_cl_Ct);
@)

@(defun unexport (symbols &o (pack mkcl_current_package(env)))
@
BEGIN:
  switch (mkcl_type_of(symbols)) {
  case mkcl_t_symbol:
    mkcl_unexport2(env, symbols, pack);
    break;
    
  case mkcl_t_null:
  case mkcl_t_cons:
    pack = mk_si_coerce_to_package(env, pack);
    mkcl_loop_for_in(env, symbols) {
      mkcl_unexport2(env, MKCL_CONS_CAR(symbols), pack);
    } mkcl_end_loop_for_in;
    break;
    
  default:
    symbols = mkcl_type_error(env, @'unexport', "argument", symbols,
			     mk_cl_list(env, 3, @'or', @'symbol', @'list'));
    goto BEGIN;
  }
  @(return mk_cl_Ct);
@)

@(defun import (symbols &o (pack mkcl_current_package(env)))
@
BEGIN:
  switch (mkcl_type_of(symbols)) {
  case mkcl_t_symbol:
    mkcl_import2(env, symbols, pack);
    break;
    
  case mkcl_t_null:
  case mkcl_t_cons:
    pack = mk_si_coerce_to_package(env, pack);
    mkcl_loop_for_in(env, symbols) {
      mkcl_import2(env, MKCL_CONS_CAR(symbols), pack);
    } mkcl_end_loop_for_in;
    break;
    
  default:
    symbols = mkcl_type_error(env, @'import', "argument", symbols,
			     mk_cl_list(env, 3, @'or', @'symbol', @'list'));
    goto BEGIN;
  }
  @(return mk_cl_Ct);
@)

@(defun shadowing_import (symbols &o (pack mkcl_current_package(env)))
@
BEGIN:
  switch (mkcl_type_of(symbols)) {
  case mkcl_t_symbol:
    mkcl_shadowing_import(env, symbols, pack);
    break;
    
  case mkcl_t_null:
  case mkcl_t_cons:
    pack = mk_si_coerce_to_package(env, pack);
    mkcl_loop_for_in(env, symbols) {
      mkcl_shadowing_import(env, MKCL_CONS_CAR(symbols), pack);
    } mkcl_end_loop_for_in;
    break;
    
  default:
    symbols = mkcl_type_error(env, @'shadowing-import', "argument", symbols,
			     mk_cl_list(env, 3, @'or', @'symbol', @'list'));
    goto BEGIN;
  }
  @(return mk_cl_Ct);
@)

@(defun shadow (symbols &o (pack mkcl_current_package(env)))
@
BEGIN:
  switch (mkcl_type_of(symbols)) {
  case mkcl_t_string:
  case mkcl_t_base_string:
  case mkcl_t_symbol:
  case mkcl_t_character:
    /* Arguments to SHADOW may be: string designators ... */
    mkcl_shadow(env, symbols, pack);
    break;
  case mkcl_t_null:
  case mkcl_t_cons:
    /* ... or lists of string designators */
    pack = mk_si_coerce_to_package(env, pack);
    mkcl_loop_for_in(env, symbols) {
      mkcl_shadow(env, MKCL_CONS_CAR(symbols), pack);
    } mkcl_end_loop_for_in;
    break;
  default:
    symbols = mkcl_type_error(env, @'shadow', "", symbols,
			     mk_cl_list(env, 3, @'or', @'symbol', @'list'));
    goto BEGIN;
  }
  @(return mk_cl_Ct);
@)

@(defun use_package (pack &o (pa mkcl_current_package(env)))
@
BEGIN:
  switch (mkcl_type_of(pack)) {
  case mkcl_t_symbol:
  case mkcl_t_character:
  case mkcl_t_base_string:
  case mkcl_t_package:
    mkcl_use_package(env, pack, pa);
    break;
    
  case mkcl_t_null:
  case mkcl_t_cons:
    pa = mk_si_coerce_to_package(env, pa);
    mkcl_loop_for_in(env, pack) {
      mkcl_use_package(env, MKCL_CONS_CAR(pack), pa);
    } mkcl_end_loop_for_in;
    break;
    
  default:
    mkcl_assert_type_package(env, pack);
    goto BEGIN;
  }
  @(return mk_cl_Ct);
@)

@(defun unuse_package (pack &o (pa mkcl_current_package(env)))
@
BEGIN:
  switch (mkcl_type_of(pack)) {
  case mkcl_t_symbol:
  case mkcl_t_character:
  case mkcl_t_base_string:
  case mkcl_t_package:
    mkcl_unuse_package(env, pack, pa);
    break;
    
  case mkcl_t_null:
  case mkcl_t_cons:
    pa = mk_si_coerce_to_package(env, pa);
    mkcl_loop_for_in(env, pack) {
      mkcl_unuse_package(env, MKCL_CONS_CAR(pack), pa);
    } mkcl_end_loop_for_in;
    break;
    
  default:
    mkcl_assert_type_package(env, pack);
    goto BEGIN;
  }
  @(return mk_cl_Ct);
@)

mkcl_object
mk_si_package_hash_tables(MKCL, mkcl_object p)
{
  volatile bool locked = false;
  mkcl_object he, hi, u;

  mkcl_call_stack_check(env);
  mkcl_assert_type_package(env, p);
  MKCL_UNWIND_PROTECT_BEGIN(env) {
    MKCL_LIBC_NO_INTR(env, (MKCL_PACKAGE_LOCK(p), locked = true));

    he = mk_si_copy_hash_table(env, p->pack.external);
    hi = mk_si_copy_hash_table(env, p->pack.internal);
    u = mk_cl_copy_list(env, p->pack.uses);
  } MKCL_UNWIND_PROTECT_EXIT {
    if (locked) MKCL_PACKAGE_UNLOCK(p);
  } MKCL_UNWIND_PROTECT_END;
  @(return he hi u);
}

mkcl_object mk_si_packages_in_waiting(MKCL)
{
  mkcl_object x = mk_cl_Cnil;
  volatile bool locked = false;

  mkcl_call_stack_check(env);
  MKCL_UNWIND_PROTECT_BEGIN(env) {
    MKCL_LIBC_NO_INTR(env, (MKCL_PACKAGE_LIST_LOCK(), locked = true));
    if (mkcl_core.packages_to_be_created != MKCL_OBJNULL)
      x = mk_cl_copy_alist(env, mkcl_core.packages_to_be_created);
  } MKCL_UNWIND_PROTECT_EXIT {
    if (locked) MKCL_PACKAGE_LIST_UNLOCK();
  } MKCL_UNWIND_PROTECT_END;
  @(return x);
}

