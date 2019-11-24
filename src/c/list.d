/* -*- mode: c -*- */
/*
    list.d -- List manipulating routines.
*/
/*
    Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
    Copyright (c) 1990, Giuseppe Attardi.
    Copyright (c) 2001, Juan Jose Garcia Ripoll.
    Copyright (c) 2010-2012, Jean-Claude Beaudoin

    MKCL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 3 of the License, or (at your option) any later version.

    See file '../../Copyright' for full details.
*/

#include <mkcl/mkcl.h>
#include <mkcl/mkcl-inl.h>

struct cl_test {
  bool (*test_c_function)(MKCL, struct cl_test *, mkcl_object);
  mkcl_object (*key_c_function)(MKCL, struct cl_test *, mkcl_object);
  mkcl_env env;
  mkcl_object key_function;
  mkcl_objectfn_fixed key_fn;
  mkcl_object test_function;
  mkcl_objectfn_fixed test_fn;
  mkcl_object item_compared;
};

#define TEST(e,t,k) ((t)->test_c_function)((e),(t),(k))
#define KEY(e,t,x) ((t)->key_c_function)((e),(t),(x))
#define close_test(t)

static bool
test_compare(MKCL, struct cl_test *t, mkcl_object x)
{
  x = KEY(env, t, x);
  t->env->function = t->test_function;
  return t->test_fn(env, t->item_compared, x) != mk_cl_Cnil;
}

static bool
test_compare_not(MKCL, struct cl_test *t, mkcl_object x)
{
  x = KEY(env, t, x);
  t->env->function = t->test_function;
  return t->test_fn(env, t->item_compared, x) == mk_cl_Cnil;
}

static bool
test_eq(MKCL, struct cl_test *t, mkcl_object x)
{
  return (t->item_compared == KEY(env, t, x));
}

static bool
test_eql(MKCL, struct cl_test *t, mkcl_object x)
{
  return mkcl_eql(env, t->item_compared, KEY(env, t, x));
}

static bool
test_equal(MKCL, struct cl_test *t, mkcl_object x)
{
  return mkcl_equal(env, t->item_compared, KEY(env, t, x));
}

static bool
test_equalp(MKCL, struct cl_test *t, mkcl_object x)
{
  return mkcl_equalp(env, t->item_compared, KEY(env, t, x));
}

static mkcl_object
key_function(MKCL, struct cl_test *t, mkcl_object x)
{
  t->env->function = t->key_function;
  return t->key_fn(env, x);
}

static mkcl_object
key_identity(MKCL, struct cl_test *t, mkcl_object x)
{
  return x;
}

static void
setup_test(MKCL, struct cl_test *t, mkcl_object item, mkcl_object test,
	   mkcl_object test_not, mkcl_object key)
{
  t->env = env;
  t->item_compared = item;
  if (test != mk_cl_Cnil) {
    if (test_not != mk_cl_Cnil)
      mkcl_FEerror(env, "Both :TEST and :TEST-NOT are specified.", 0);
    t->test_function = test = mk_si_coerce_to_function(env, test);
    if (test == MKCL_SYM_FUN(@'eq')) {
      t->test_c_function = test_eq;
    } else if (test == MKCL_SYM_FUN(@'eql')) {
      t->test_c_function = test_eql;
    } else if (test == MKCL_SYM_FUN(@'equal')) {
      t->test_c_function = test_equal;
    } else if (test == MKCL_SYM_FUN(@'equalp')) {
      t->test_c_function = test_equalp;
    } else {
      t->test_c_function = test_compare;
      t->test_function = mkcl_validate_function(env, test);
      t->test_fn = t->test_function->cfun.f._[2];
    }
  } else if (test_not != mk_cl_Cnil) {
    t->test_c_function = test_compare_not;
    test_not = mk_si_coerce_to_function(env, test_not);
    t->test_function = mkcl_validate_function(env, test_not);
    t->test_fn = t->test_function->cfun.f._[2];
  } else {
    t->test_c_function = test_eql;
  }
  if (key != mk_cl_Cnil) {
    key = mk_si_coerce_to_function(env, key);
    t->key_function = mkcl_validate_function(env, key);
    t->key_fn = t->key_function->cfun.f._[1];
    t->key_c_function = key_function;
  } else {
    t->key_c_function = key_identity;
  }
}

extern inline mkcl_object mk_cl_car(MKCL, mkcl_object x);

extern inline mkcl_object mk_cl_cdr(MKCL, mkcl_object x);

@(defun list (&rest args)
  mkcl_object head = mk_cl_Cnil;
@
  if (narg--) {
    mkcl_object tail = head = mkcl_list1(env, mkcl_va_arg(args));
    while (narg--) {
      mkcl_object cons = mkcl_list1(env, mkcl_va_arg(args));
      MKCL_RPLACD(tail, cons);
      tail = cons;
    }
  }
  mkcl_va_end(args);
  @(return head);
@)

@(defun list* (&rest args)
  mkcl_object head;
@
  if (narg == 0)
  mkcl_FEwrong_num_arguments(env, @'list*', 1, -1, narg);
  head = mkcl_va_arg(args);
  if (--narg) {
    mkcl_object tail = head = mkcl_list1(env, head);
    while (--narg) {
      mkcl_object cons = mkcl_list1(env, mkcl_va_arg(args));
      MKCL_RPLACD(tail, cons);
      tail = cons;
    }
    MKCL_RPLACD(tail, mkcl_va_arg(args));
  }
  mkcl_va_end(args);
  @(return head);
@)


static mkcl_object copy_proper_tail(MKCL, mkcl_object * cursor_ptr)
{
  mkcl_object cursor = *cursor_ptr;
  mkcl_object next;
  mkcl_object root;

  if (MKCL_CONSP(cursor))
    {
      mkcl_object new = mkcl_cons(env, MKCL_CONS_CAR(cursor), MKCL_CONS_CDR(cursor));
      root = cursor = new;
    }
  else
    mkcl_FEtype_error_list(env, cursor);

  for (next = MKCL_CONS_CDR(cursor); MKCL_CONSP(next); next = MKCL_CONS_CDR(cursor))
    {
      mkcl_object new = mkcl_cons(env, MKCL_CONS_CAR(next), MKCL_CONS_CDR(next));
      MKCL_RPLACD(cursor, new);
      cursor = new;
    }
  
  if (!mkcl_Null(next))
    mkcl_FEtype_error_proper_list(env, root); /* (APPEND (QUOTE (1 . 2)) 3) */
  else
    *cursor_ptr = cursor;

  return root;
}

@(defun append (&rest rest)
  mkcl_object head = mk_cl_Cnil;
@
  if ( narg == 1 )
    head = mkcl_va_arg(rest);
  else if ( narg > 1 )
    {
      mkcl_object tail = mk_cl_Cnil;

      while (--narg)
	{
	  mkcl_object arg = mkcl_va_arg(rest);
	  if (mkcl_Null(arg))
	    continue;
	  else if (mkcl_Null(tail))
	    {
	      tail = arg;
	      head = copy_proper_tail(env, &tail);
	    }
	  else
	    {
	      MKCL_RPLACD(tail, copy_proper_tail(env, &arg));
	      tail = arg;
	    }
	}

      if (mkcl_Null(tail))
	head = mkcl_va_arg(rest);
      else
	MKCL_RPLACD(tail, mkcl_va_arg(rest));

    }
  mkcl_va_end(rest);
  @(return head);
@)


mkcl_object
mkcl_append(MKCL, mkcl_object x, mkcl_object y)
{
  mkcl_object head = mk_cl_Cnil;
  
  if (mkcl_Null(x))
    return y;

  head = copy_proper_tail(env, &x);
  MKCL_RPLACD(x, y);

  return head;
}


/* Open coded CARs and CDRs */
#define car(foo)				\
  (void) foo;					\
  if (!MKCL_LISTP(x)) goto E;			\
  if (!mkcl_Null(x)) x = MKCL_CONS_CAR(x);
#define cdr(foo)				\
  (void) foo;					\
  if (!MKCL_LISTP(x)) goto E;			\
  if (!mkcl_Null(x)) x = MKCL_CONS_CDR(x);
#define defcxr(name, arg, code)				\
  mkcl_object mk_cl_##name(MKCL, mkcl_object foo) {	\
    register mkcl_object arg = foo;			\
    code; mkcl_return1(arg);				\
  E:	mkcl_FEtype_error_list(env, arg);}

defcxr(caar, x, car(car(x)))
defcxr(cadr, x, car(cdr(x)))
defcxr(cdar, x, cdr(car(x)))
defcxr(cddr, x, cdr(cdr(x)))
defcxr(caaar, x, car(car(car(x))))
defcxr(caadr, x, car(car(cdr(x))))
defcxr(cadar, x, car(cdr(car(x))))
defcxr(caddr, x, car(cdr(cdr(x))))
defcxr(cdaar, x, cdr(car(car(x))))
defcxr(cdadr, x, cdr(car(cdr(x))))
defcxr(cddar, x, cdr(cdr(car(x))))
defcxr(cdddr, x, cdr(cdr(cdr(x))))
defcxr(caaaar, x, car(car(car(car(x)))))
defcxr(caaadr, x, car(car(car(cdr(x)))))
defcxr(caadar, x, car(car(cdr(car(x)))))
defcxr(caaddr, x, car(car(cdr(cdr(x)))))
defcxr(cadaar, x, car(cdr(car(car(x)))))
defcxr(cadadr, x, car(cdr(car(cdr(x)))))
defcxr(caddar, x, car(cdr(cdr(car(x)))))
defcxr(cadddr, x, car(cdr(cdr(cdr(x)))))
defcxr(cdaaar, x, cdr(car(car(car(x)))))
defcxr(cdaadr, x, cdr(car(car(cdr(x)))))
defcxr(cdadar, x, cdr(car(cdr(car(x)))))
defcxr(cdaddr, x, cdr(car(cdr(cdr(x)))))
defcxr(cddaar, x, cdr(cdr(car(car(x)))))
defcxr(cddadr, x, cdr(cdr(car(cdr(x)))))
defcxr(cdddar, x, cdr(cdr(cdr(car(x)))))
defcxr(cddddr, x, cdr(cdr(cdr(cdr(x)))))
#undef car
#undef cdr
#undef defcxr


#define LENTH(n) (MKCL, mkcl_object x) {		\
    mkcl_object val = mkcl_nth(env, n, x);		\
    mkcl_return1(val);					\
  }
mkcl_object @fifth	LENTH(4)
mkcl_object @sixth	LENTH(5)
mkcl_object @seventh	LENTH(6)
mkcl_object @eighth	LENTH(7)
mkcl_object @ninth	LENTH(8)
mkcl_object @tenth	LENTH(9)
#undef LENTH

static bool
tree_equal(MKCL, struct cl_test *t, mkcl_object x, mkcl_object y)
{
 BEGIN:
  if (MKCL_CONSP(x)) {
    if (MKCL_CONSP(y)) {
      if (tree_equal(env, t, MKCL_CONS_CAR(x), MKCL_CONS_CAR(y))) {
	x = MKCL_CONS_CDR(x);
	y = MKCL_CONS_CDR(y);
	goto BEGIN;
      } else {
	return(FALSE);
      }
    } else {
      return(FALSE);
    }
  } else {
    t->item_compared = x;
    if (TEST(env, t, y))
      return(TRUE);
    else
      return(FALSE);
  }
}

@(defun tree_equal (x y &key test test_not)
	struct cl_test t;
	mkcl_object output;
@
  setup_test(env, &t, mk_cl_Cnil, test, test_not, mk_cl_Cnil);
  output = tree_equal(env, &t, x, y) ? mk_cl_Ct : mk_cl_Cnil;
  close_test(&t);
  @(return output);
@)

mkcl_object
mk_cl_endp(MKCL, mkcl_object x)
{
  mkcl_call_stack_check(env);
  if (mkcl_Null(x))
    { @(return mk_cl_Ct); }
  else if (MKCL_LISTP(x))
    { @(return mk_cl_Cnil); }
  else
    mkcl_FEtype_error_list(env, x);
}

bool
mkcl_endp(MKCL, mkcl_object x)
{
  if (mkcl_Null(x))
    return(TRUE);
  else if (MKCL_LISTP(x))
    return(FALSE);
  else
    mkcl_FEtype_error_list(env, x);
}

mkcl_object
mk_cl_list_length(MKCL, mkcl_object x)
{
  mkcl_word n;
  mkcl_object fast, slow;

  mkcl_call_stack_check(env);
  /* INV: A list's length always fits in a fixnum */
  fast = slow = x;
  for (n = 0; !mkcl_Null(fast); n++, fast = MKCL_CONS_CDR(fast)) {
    if (!MKCL_LISTP(fast)) {
      mkcl_FEtype_error_list(env, fast);
    }
    if (slow == fast && n != 0) { @(return mk_cl_Cnil); } /* Circular list! */
    if (n & 1) { /* move only on odd beat. */
      slow = MKCL_CONS_CDR(slow);
    }
  }
  @(return MKCL_MAKE_FIXNUM(n));
}

mkcl_object
mk_cl_nth(MKCL, mkcl_object n, mkcl_object x)
{
  mkcl_call_stack_check(env);
  @(return mkcl_nth(env, mkcl_safe_fixnum_to_word(env, n), x));
}

mkcl_object
mkcl_nth(MKCL, mkcl_word n, mkcl_object x)
{
  if (n < 0)
    mkcl_FEtype_error_seq_index(env, x, MKCL_MAKE_FIXNUM(n));
  /* INV: No need to check for circularity since we visit
     at most `n' conses */
  for (; n > 0 && MKCL_CONSP(x); n--)
    x = MKCL_CONS_CDR(x);

  if (mkcl_Null(x))
    return mk_cl_Cnil;
  else if (MKCL_CONSP(x))
    return MKCL_CONS_CAR(x);
  else
    mkcl_FEtype_error_list(env, x);
}

mkcl_object
mk_cl_nthcdr(MKCL, mkcl_object n, mkcl_object x)
{
  mkcl_call_stack_check(env);
  @(return mkcl_nthcdr(env, mkcl_safe_fixnum_to_word(env, n), x));
}

mkcl_object
mkcl_nthcdr(MKCL, mkcl_word n, mkcl_object x)
{
  if (n < 0)
    mkcl_FEtype_error_seq_index(env, x, MKCL_MAKE_FIXNUM(n));
  while (n-- > 0 && !mkcl_Null(x)) {
    if (MKCL_LISTP(x)) {
      x = MKCL_CONS_CDR(x);
    } else {
      mkcl_FEtype_error_list(env, x);
    }
  }
  return x;
}

mkcl_object
mkcl_last(MKCL, mkcl_object l, mkcl_index n)
{
  /* The algorithm is very simple. We run over the list with
   * two pointers, "l" and "r". The separation between both
   * must be "n", so that when "l" finds no more conses, "r"
   * contains the output. */
  mkcl_object r;

  for (r = l; n && MKCL_CONSP(r); n--, r = MKCL_CONS_CDR(r));
  /* If "l" has not moved, we have to ensure that it is a list */
  if (r == l) {
    if (!MKCL_LISTP(r)) mkcl_FEtype_error_list(env, l);
    while (MKCL_CONSP(r)) {
      r = MKCL_CONS_CDR(r);
    }
    return r;
  } else if (n == 0) {
    while (MKCL_CONSP(r)) {
      r = MKCL_CONS_CDR(r);
      l = MKCL_CONS_CDR(l);
    }
    return l;
  } else {
    return l;
  }
}

@(defun last (l &optional (k MKCL_MAKE_FIXNUM(1)))
@
  if (mkcl_type_of(k) == mkcl_t_bignum)
    { @(return l); }
  else
    { @(return mkcl_last(env, l, mkcl_integer_to_index(env, k))); }
@)

@(defun make_list (size &key initial_element &aux x)
	mkcl_word i;
@
  /* INV: mkcl_integer_to_index() signals a type-error if SIZE is not a integer >=0 */
  i = mkcl_integer_to_index(env, size);
  while (i-- > 0)
    x = MKCL_CONS(env, initial_element, x);
  @(return x);
@)

mkcl_object
mkcl_copy_proper_list(MKCL, mkcl_object x)
{
  mkcl_object copy;

  if (mkcl_Null(x))
    copy = mk_cl_Cnil;
  else
    {
      mkcl_object tail = copy = mkcl_list1(env, MKCL_CONS_CAR(x));
      x = MKCL_CONS_CDR(x);
      while (!mkcl_Null(x))
	{
	  mkcl_object cons = mkcl_list1(env, MKCL_CONS_CAR(x));
	  MKCL_RPLACD(tail, cons);
	  tail = cons;
	  x = MKCL_CONS_CDR(x);
	}
    }
  return copy;
}

mkcl_object
mk_cl_copy_list(MKCL, mkcl_object x)
{
  mkcl_object copy;

  mkcl_call_stack_check(env);
  if (!MKCL_LISTP(x)) {
    mkcl_FEtype_error_list(env, x);
  }
  copy = mk_cl_Cnil;
  if (!mkcl_Null(x)) {
    mkcl_object tail = copy = mkcl_list1(env, MKCL_CAR(x));
    while (x = MKCL_CONS_CDR(x), MKCL_CONSP(x)) {
      mkcl_object cons = mkcl_list1(env, MKCL_CONS_CAR(x));
      MKCL_RPLACD(tail, cons);
      tail = cons;
    }
    MKCL_RPLACD(tail, x);
  }
  @(return copy);
}

mkcl_object mkcl_reverse_proper_list(MKCL, mkcl_object x)
{
  mkcl_object output = mk_cl_Cnil;

  for (; !mkcl_Null(x); x = MKCL_CONS_CDR(x)) {
    output = MKCL_CONS(env, MKCL_CONS_CAR(x), output);
  }
  return output;
}

mkcl_object mkcl_nreverse_proper_list(MKCL, mkcl_object old)
{
  mkcl_object this, new = mk_cl_Cnil;

  while (!mkcl_Null(old))
    {
      this = old;
      old = MKCL_CONS_CDR(old);
      MKCL_RPLACD(this, new);
      new = this;
    }
  return new;
}

static mkcl_object
duplicate_pairs(MKCL, mkcl_object x)
{
  mkcl_object p = MKCL_CONS_CAR(x);
  if (MKCL_CONSP(p))
    p = MKCL_CONS(env, MKCL_CONS_CAR(p), MKCL_CONS_CDR(p));
  return mkcl_list1(env, p);
}

mkcl_object
mk_cl_copy_alist(MKCL, mkcl_object x)
{
  mkcl_object copy;

  mkcl_call_stack_check(env);
  if (!MKCL_LISTP(x)) {
    mkcl_FEtype_error_list(env, x);
  }
  copy = mk_cl_Cnil;
  if (!mkcl_Null(x)) {
    mkcl_object tail = copy = duplicate_pairs(env, x);
    while (x = MKCL_CONS_CDR(x), !mkcl_Null(x)) {
      if (!MKCL_LISTP(x)) {
	mkcl_FEtype_error_list(env, x);
      } else {
	mkcl_object cons = duplicate_pairs(env, x);
	tail = MKCL_RPLACD(tail, cons);
	tail = cons;
      }
    }
  }
  @(return copy);
}

static mkcl_object
do_copy_tree(MKCL, mkcl_object x)
{
  if (MKCL_CONSP(x)) {
    x = MKCL_CONS(env,
	     do_copy_tree(env, MKCL_CONS_CAR(x)),
	     do_copy_tree(env, MKCL_CONS_CDR(x)));
  }
  return x;
}

mkcl_object
mk_cl_copy_tree(MKCL, mkcl_object x)
{
  mkcl_call_stack_check(env);
  @(return do_copy_tree(env, x));
}

mkcl_object
mk_cl_revappend(MKCL, mkcl_object x, mkcl_object y)
{
  mkcl_call_stack_check(env);
  mkcl_loop_for_in(env, x) {
    y = MKCL_CONS(env, MKCL_CONS_CAR(x),y);
  } mkcl_end_loop_for_in;
  @(return y);
}

@(defun nconc (&rest lists)
  mkcl_object head = mk_cl_Cnil, tail = mk_cl_Cnil;
@	
  while (narg--) {
    mkcl_object new_tail, other = mkcl_va_arg(lists);
    if (mkcl_Null(other)) {
      new_tail = tail;
    } else if (MKCL_CONSP(other)) {
      new_tail = mkcl_last(env, other, 1);
    } else {
      if (narg) mkcl_FEtype_error_list(env, other);
      new_tail = tail;
    }
    if (mkcl_Null(head)) {
      head = other;
    } else {
      MKCL_RPLACD(tail, other);
    }
    tail = new_tail;
  }
  mkcl_va_end(lists);
  @(return head);
@)

mkcl_object
mkcl_nconc(MKCL, mkcl_object l, mkcl_object y)
{
  if (mkcl_Null(l)) {
    return y;
  } else {
    MKCL_RPLACD(mkcl_last(env, l, 1), y);
    return l;
  }
}

mkcl_object
mk_cl_nreconc(MKCL, mkcl_object l, mkcl_object y)
{
  mkcl_object x, z;
  /* INV: when a circular list is "reconc'ed", the pointer ends
     up at the beginning of the original list, hence we need no
     slow pointer */

  mkcl_call_stack_check(env);
  for (x = l; !mkcl_Null(x); ) {
    if (!MKCL_LISTP(x)) mkcl_FEtype_error_list(env, x);
    z = x;
    x = MKCL_CONS_CDR(x);
    if (x == l) mkcl_FEcircular_list(env, l);
    MKCL_RPLACD(z, y);
    y = z;
  }
  @(return y);
}

mkcl_object
mkcl_butlast(MKCL, mkcl_object l, mkcl_index n)
{
  /* See LAST for details on this algorithm */
  mkcl_object r;

  for (r = l; n && MKCL_CONSP(r); n--, r = MKCL_CONS_CDR(r));

  if (mkcl_Null(r)) {
    return mk_cl_Cnil;
  } else if (!MKCL_LISTP(r)) {
    /* We reach here either because l is shorter than n conses,
     * or because it is not a list */
    if (r == l) mkcl_FEtype_error_list(env, r);
    return mk_cl_Cnil;
  } else {
    /* We reach here because l has at least n conses and
     * thus we can take MKCL_CAR(l) */
    mkcl_object head, tail;
    head = tail = mkcl_list1(env, MKCL_CAR(l));
    while (l = MKCL_CONS_CDR(l), r = MKCL_CONS_CDR(r), MKCL_CONSP(r)) {
      mkcl_object cons = mkcl_list1(env, MKCL_CONS_CAR(l));
      MKCL_RPLACD(tail, cons);
      tail = cons;
    }
    return head;
  }
}

@(defun butlast (lis &optional (nn MKCL_MAKE_FIXNUM(1)))
@
  /* INV: No list has more than MKCL_MOST_POSITIVE_FIXNUM elements */
  if (mkcl_type_of(nn) == mkcl_t_bignum)
    { @(return mk_cl_Cnil); }
  else
    /* INV: mkcl_integer_to_index() signals a type-error if NN is not an integer >=0 */
    { @(return mkcl_butlast(env, lis, mkcl_integer_to_index(env, nn))); }
@)

mkcl_object
mkcl_nbutlast(MKCL, mkcl_object l, mkcl_index n)
{
  mkcl_object r;

  if (!MKCL_LISTP(l))
    mkcl_FEtype_error_list(env, l);
  for (n++, r = l; n && MKCL_CONSP(r); n--, r = MKCL_CONS_CDR(r));

  if (n == 0) {
    mkcl_object tail = l;

    while (MKCL_CONSP(r)) {
      tail = MKCL_CONS_CDR(tail);
      r = MKCL_CONS_CDR(r);
    }
    MKCL_RPLACD(tail, mk_cl_Cnil);
    return l;
  }
  return mk_cl_Cnil;
}

@(defun nbutlast (lis &optional (nn MKCL_MAKE_FIXNUM(1)))
@
  /* INV: No list has more than MKCL_MOST_POSITIVE_FIXNUM elements */
  if (mkcl_type_of(nn) == mkcl_t_bignum)
    { @(return mk_cl_Cnil); }
  else
    /* INV: mkcl_integer_to_index() signas a type-error if NN is not an integer >=0 */
    { @(return mkcl_nbutlast(env, lis, mkcl_integer_to_index(env, nn))); }
@)

mkcl_object
mk_cl_ldiff(MKCL, mkcl_object x, mkcl_object y)
{
  mkcl_object head = mk_cl_Cnil;

  mkcl_call_stack_check(env);
  if (!MKCL_LISTP(x)) {
    mkcl_FEtype_error_list(env, x);
  }
  /* Here we use that, if X or Y are CONS, then (EQL X Y)
   * only when X == Y */
  if (!mkcl_Null(x) && (x != y)) {
    mkcl_object tail = head = mkcl_list1(env, MKCL_CONS_CAR(x));
    while (1) {
      x = MKCL_CONS_CDR(x);
      if (!MKCL_CONSP(x)) {
	if (!mkcl_eql(env, x, y)) {
	  MKCL_RPLACD(tail, x);
	}
	break;
      } else if (x == y) {
	break;
      } else {
	mkcl_object cons = mkcl_list1(env, MKCL_CONS_CAR(x));
	MKCL_RPLACD(tail, cons);
	tail = cons;
      }
    }
  }
  @(return head);
}

mkcl_object
mk_cl_rplaca(MKCL, mkcl_object x, mkcl_object v)
{
  mkcl_call_stack_check(env);
  mkcl_assert_type_cons(env, x);
  MKCL_RPLACA(x, v);
  @(return x);
}

mkcl_object
mk_cl_rplacd(MKCL, mkcl_object x, mkcl_object v)
{
  mkcl_call_stack_check(env);
  mkcl_assert_type_cons(env, x);
  MKCL_RPLACD(x, v);
  @(return x);
}


/**********************************/


static mkcl_object
do_assoc(MKCL, struct cl_test *t, mkcl_object a_list)
{
  mkcl_loop_for_in(env, a_list) {
    mkcl_object pair = MKCL_CONS_CAR(a_list);
    if (!mkcl_Null(pair)) {
      if (!MKCL_LISTP(pair))
	mkcl_FEtype_error_list(env, pair);
      if (TEST(env, t, MKCL_CONS_CAR(pair)))
	return pair;
    }
  } mkcl_end_loop_for_in;
  return mk_cl_Cnil;
}


@(defun assoc (item a_list &key test test_not key)
  struct cl_test t;
@
  setup_test(env, &t, item, test, test_not, key);
  a_list = do_assoc(env, &t, a_list);
  close_test(&t);
  @(return a_list);
@)


static mkcl_object
subst(MKCL, struct cl_test *t, mkcl_object new_obj, mkcl_object tree)
{
  if (TEST(env, t, tree)) {
    return new_obj;
  } else if (MKCL_ATOM(tree)) {
    return tree;
  } else {
    mkcl_object head, tail = mk_cl_Cnil;
    do {
      mkcl_object cons = subst(env, t, new_obj, MKCL_CONS_CAR(tree));

      cons = mkcl_cons(env, cons, tree = MKCL_CONS_CDR(tree));
      if (mkcl_Null(tail)) {
	head = cons;
      } else {
	MKCL_RPLACD(tail, cons);
      }
      tail = cons;
      if (TEST(env, t, tree)) {
	MKCL_RPLACD(tail, new_obj);
	return head;
      }
    } while (MKCL_CONSP(tree));
    return head;
  }
}

@(defun subst (new_obj old_obj tree &key test test_not key)
	struct cl_test t;
	mkcl_object output;
@
  setup_test(env, &t, old_obj, test, test_not, key);
  output = subst(env, &t, new_obj, tree);
  close_test(&t);
  @(return output);
@)


static mkcl_object
nsubst_cons(MKCL, struct cl_test *t, mkcl_object new_obj, mkcl_object tree)
{
  mkcl_object l = tree;
  do {
    mkcl_object o = MKCL_CONS_CAR(l);
    if (TEST(env, t, o)) {
      MKCL_RPLACA(l, new_obj);
    } else if (MKCL_CONSP(o)) {
      nsubst_cons(env, t, new_obj, o);
    }
    o = MKCL_CONS_CDR(l);
    if (TEST(env, t, o)) {
      MKCL_RPLACD(l, new_obj);
      return tree;
    }
    l = o;
  } while (MKCL_CONSP(l));
  return tree;
}

static mkcl_object
nsubst(MKCL, struct cl_test *t, mkcl_object new_obj, mkcl_object tree)
{
  if (TEST(env, t, tree))
    return new_obj;
  if (MKCL_CONSP(tree))
    return nsubst_cons(env, t, new_obj, tree);
  return tree;
}

@(defun nsubst (new_obj old_obj tree &key test test_not key)
  struct cl_test t;
@
  setup_test(env, &t, old_obj, test, test_not, key);
  tree = nsubst(env, &t, new_obj, tree);
  close_test(&t);
  @(return tree);
@)

/*
	Sublis(alist, tree) returns
	result of substituting tree by alist.
*/
static mkcl_object
sublis(MKCL, struct cl_test *t, mkcl_object alist, mkcl_object tree)
{
  mkcl_object node;

  t[1].item_compared = KEY(env, t, tree);
  node = do_assoc(env, t+1, alist);
  if (!mkcl_Null(node)) {
    return MKCL_CONS_CDR(node);
  }
  if (MKCL_CONSP(tree)) {
    tree = MKCL_CONS(env,
		sublis(env, t, alist, MKCL_CONS_CAR(tree)),
		sublis(env, t, alist, MKCL_CONS_CDR(tree)));
  }
  return tree;
}

@(defun sublis (alist tree &key test test_not key)
	/* t[0] is the test for the objects in the tree, configured
	   with test, test_not and key. t[1] is the test for searching
	   in the association list.
	 */
  struct cl_test t[2];
@
  setup_test(env, t, mk_cl_Cnil, mk_cl_Cnil, mk_cl_Cnil, key);
  setup_test(env, t+1, mk_cl_Cnil, test, test_not, mk_cl_Cnil);
  tree = sublis(env, t, alist, tree);
  close_test(t+1);
  close_test(t);
  @(return tree);
@)

/*
	Nsublis(alist, treep) stores
	the result of substiting *treep by alist
	to *treep.
*/
static mkcl_object
nsublis(MKCL, struct cl_test *t, mkcl_object alist, mkcl_object tree)
{
  mkcl_object node;

  t[1].item_compared = KEY(env, t, tree);
  node = do_assoc(env, t+1, alist);
  if (!mkcl_Null(node)) {
    return MKCL_CONS_CDR(node);
  }
  if (MKCL_CONSP(tree)) {
    MKCL_RPLACA(tree, nsublis(env, t, alist, MKCL_CONS_CAR(tree)));
    MKCL_RPLACD(tree, nsublis(env, t, alist, MKCL_CONS_CDR(tree)));
  }
  return tree;
}

@(defun nsublis (alist tree &key test test_not key)
  /* t[0] is the test for the objects in the tree, configured
     with test, test_not and key. t[1] is the test for searching
     in the association list.
  */
  struct cl_test t[2];
@
  setup_test(env, t, mk_cl_Cnil, mk_cl_Cnil, mk_cl_Cnil, key);
  setup_test(env, t+1, mk_cl_Cnil, test, test_not, mk_cl_Cnil);
  tree = nsublis(env, t, alist, tree);
  close_test(t+1);
  close_test(t);
  @(return tree);
@)

@(defun member (item list &key test test_not key)
  struct cl_test t;
@
  setup_test(env, &t, item, test, test_not, key);
  mkcl_loop_for_in(env, list) {
    if (TEST(env, &t, MKCL_CONS_CAR(list)))
      break;
  } mkcl_end_loop_for_in;
  close_test(&t);
  @(return list);
@)

bool
mkcl_member_eq(MKCL, mkcl_object x, mkcl_object l)
{
  mkcl_loop_for_in(env, l) {
    if (x == MKCL_CONS_CAR(l))
      return(TRUE);
  } mkcl_end_loop_for_in;
  return(FALSE);
}

mkcl_object
mkcl_memq(MKCL, mkcl_object x, mkcl_object l)
{
  mkcl_loop_for_in(env, l) {
    if (x == MKCL_CONS_CAR(l))
      return(l);
  } mkcl_end_loop_for_in;
  return(mk_cl_Cnil);
}

mkcl_object
mk_si_memq(MKCL, mkcl_object x, mkcl_object l)
{
  mkcl_call_stack_check(env);
  mkcl_loop_for_in(env, l) {
    if (x == MKCL_CONS_CAR(l))
      { @(return l); }
  } mkcl_end_loop_for_in;
  @(return mk_cl_Cnil);
}

/* Added for use by the compiler, instead of open coding them. Beppe */
mkcl_object
mkcl_memql(MKCL, mkcl_object x, mkcl_object l)
{
  mkcl_loop_for_in(env, l) {
    if (mkcl_eql(env, x, MKCL_CONS_CAR(l)))
      return(l);
  } mkcl_end_loop_for_in;
  return(mk_cl_Cnil);
}

mkcl_object
mkcl_member(MKCL, mkcl_object x, mkcl_object l)
{
  mkcl_loop_for_in(env, l) {
    if (mkcl_equal(env, x, MKCL_CONS_CAR(l)))
      return(l);
  } mkcl_end_loop_for_in;
  return(mk_cl_Cnil);
}


mkcl_object
mk_si_member1(MKCL,
	      mkcl_object item, mkcl_object list,
	      mkcl_object test, mkcl_object test_not,
	      mkcl_object key)
{
  struct cl_test t;
  
  if (key != mk_cl_Cnil)
    item = mkcl_funcall1(env, key, item);
  setup_test(env, &t, item, test, test_not, key);
  mkcl_loop_for_in(env, list) {
    if (TEST(env, &t, MKCL_CONS_CAR(list)))
      break;
  } mkcl_end_loop_for_in;
  close_test(&t);
  @(return list);
}

mkcl_object
mk_cl_tailp(MKCL, mkcl_object y, mkcl_object x)
{
  mkcl_call_stack_check(env);
  mkcl_loop_for_on(env, x) {
    if (mkcl_eql(env, x, y))
      { @(return mk_cl_Ct); }
  } mkcl_end_loop_for_on;
  return mk_cl_eql(env, x, y);
}

mkcl_object
mkcl_adjoin_eq(MKCL, mkcl_object item, mkcl_object list)
{
  mkcl_object output = mkcl_memq(env, item, list);

  if (mkcl_Null(output))
    output = MKCL_CONS(env, item, list);
  else
    output = list;
  return output;
}

mkcl_object
mkcl_adjoin(MKCL, mkcl_object item, mkcl_object list)
{
  mkcl_object output = mkcl_memql(env, item, list);

  if (mkcl_Null(output))
    output = MKCL_CONS(env, item, list);
  else
    output = list;
  return output;
}

@(defun adjoin (item list &key test test_not key)
  mkcl_object output;
@
  {
    if (narg == 2)
      output = mkcl_adjoin(env, item, list);
    else if (narg < 2)
      mkcl_FEwrong_num_arguments(env, @'adjoin', 2, -1, narg);
    else
      {
	output = @si::member1(env, item, list, test, test_not, key);
	if (mkcl_Null(output))
	  output = MKCL_CONS(env, item, list);
	else
	  output = list;
      }
    @(return output);
  }
@)

mkcl_object
mk_cl_cons(MKCL, mkcl_object x, mkcl_object y)
{
  mkcl_call_stack_check(env);
  @(return MKCL_CONS(env, x, y));
}

mkcl_object
mk_cl_acons(MKCL, mkcl_object x, mkcl_object y, mkcl_object z)
{
  mkcl_call_stack_check(env);
  @(return MKCL_CONS(env, MKCL_CONS(env, x, y), z));
}

@(defun pairlis (keys data &optional a_list)
  mkcl_object k, d;
@
  k = keys;
  d = data;
  mkcl_loop_for_in(env, k) {
    if (mkcl_endp(env, d))
      goto error;
    a_list = MKCL_CONS(env, MKCL_CONS(env, MKCL_CONS_CAR(k), MKCL_CONS_CAR(d)), a_list);
    d = MKCL_CDR(d);
  } mkcl_end_loop_for_in;
  if (!mkcl_endp(env, d))
error:	    mkcl_FEerror(env, "The keys ~S and the data ~S are not of the same length",
		    2, keys, data);
  @(return a_list);
@)


@(defun rassoc (item a_list &key test test_not key)
  struct cl_test t;
@
  setup_test(env, &t, item, test, test_not, key);
  mkcl_loop_for_in(env, a_list) {
    mkcl_object pair = MKCL_CONS_CAR(a_list);
    if (!mkcl_Null(pair)) {
      if (!MKCL_LISTP(pair))
	mkcl_FEtype_error_list(env, pair);
      if (TEST(env, &t, MKCL_CONS_CDR(pair))) {
	a_list = pair;
	break;
      }
    }
  } mkcl_end_loop_for_in;
  close_test(&t);
  @(return a_list);
@)

mkcl_object
mkcl_remove_eq(MKCL, mkcl_object x, mkcl_object l)
{
  mkcl_object head = mk_cl_Cnil, tail = mk_cl_Cnil;

  mkcl_loop_for_on_unsafe(l) {
    if (MKCL_CONS_CAR(l) != x) {
      mkcl_object cons = mkcl_list1(env, MKCL_CONS_CAR(l));
      if (mkcl_Null(tail)) {
	head = tail = cons;
      } else {
	MKCL_RPLACD(tail, cons);
	tail = cons;
      }
    }
  } mkcl_end_loop_for_on;
  return head;
}

/* Added for use by the compiler, instead of open coding them. Beppe */
mkcl_object
mkcl_assq(MKCL, mkcl_object x, mkcl_object l)
{
  mkcl_loop_for_in(env, l) {
    mkcl_object pair = MKCL_CONS_CAR(l);
    if (x == MKCL_CAR(pair))
      return pair;
  } mkcl_end_loop_for_in;
  return(mk_cl_Cnil);
}

mkcl_object
mkcl_assql(MKCL, mkcl_object x, mkcl_object l)
{
  mkcl_loop_for_in(env, l) {
    mkcl_object pair = MKCL_CONS_CAR(l);
    if (mkcl_eql(env, x, MKCL_CAR(pair)))
      return pair;
  } mkcl_end_loop_for_in;
  return(mk_cl_Cnil);
}

mkcl_object
mkcl_assoc(MKCL, mkcl_object x, mkcl_object l)
{
  mkcl_loop_for_in(env, l) {
    mkcl_object pair = MKCL_CONS_CAR(l);
    if (mkcl_equal(env, x, MKCL_CAR(pair)))
      return pair;
  } mkcl_end_loop_for_in;
  return(mk_cl_Cnil);
}

mkcl_object
mkcl_assqlp(MKCL, mkcl_object x, mkcl_object l)
{
  mkcl_loop_for_in(env, l) {
    mkcl_object pair = MKCL_CONS_CAR(l);
    if (mkcl_equalp(env, x, MKCL_CAR(pair)))
      return pair;
  } mkcl_end_loop_for_in;
  return(mk_cl_Cnil);
}


/********************************/

mkcl_object
mk_si_dyn_cons(MKCL, mkcl_object car, mkcl_object cdr)
{
  mkcl_call_stack_check(env);
  mkcl_object stack_cell = MKCL_SYM_VAL(env, @'si::*dynamic-cons-stack*');

  if (mkcl_Null(stack_cell))
    {
      stack_cell = mkcl_cons(env, mkcl_cons(env, mk_cl_Cnil, mk_cl_Cnil), mk_cl_Cnil);
      MKCL_SETQ(env, @'si::*dynamic-cons-stack*', stack_cell);
    }
  {
    mkcl_object next_stack_cell = MKCL_CONS_CDR(stack_cell);
    mkcl_object it;

    if (mkcl_Null(next_stack_cell))
      {
	mkcl_object new_cells = mk_cl_Cnil;
	int i;

	for (i = 0; i < 5; i++) /* allocate a group of 5 new cells. */
	  new_cells = mkcl_cons(env, mkcl_cons(env, mk_cl_Cnil, mk_cl_Cnil), new_cells);

	next_stack_cell = new_cells;
	MKCL_RPLACD(stack_cell, next_stack_cell);
      }
    MKCL_SETQ(env, @'si::*dynamic-cons-stack*', next_stack_cell);
    it = MKCL_CONS_CAR(stack_cell);
    MKCL_RPLACA(it, car);
    MKCL_RPLACD(it, cdr);

    @(return it);
  }
}

mkcl_object mk_si_trim_dynamic_cons_stack(MKCL)
{
  mkcl_call_stack_check(env);
  mkcl_object stack_cell = MKCL_SYM_VAL(env, @'si::*dynamic-cons-stack*');

  if (mkcl_Null(stack_cell))
    {
      stack_cell = mkcl_cons(env, mkcl_cons(env, mk_cl_Cnil, mk_cl_Cnil), mk_cl_Cnil);
      MKCL_SETQ(env, @'si::*dynamic-cons-stack*', stack_cell);      
    }
  else
    MKCL_RPLACD(stack_cell, mk_cl_Cnil);

  @(return mk_cl_Cnil);
}

