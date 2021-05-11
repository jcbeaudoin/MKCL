/* -*- mode: c -*- */
/*
    backq.c -- Backquote mechanism.
*/
/*
    Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
    Copyright (c) 1990, Giuseppe Attardi.
    Copyright (c) 2001, Juan Jose Garcia Ripoll.

    MKCL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 3 of the License, or (at your option) any later version.

    See file '../../Copyright' for full details.
*/

#include <mkcl/mkcl.h>
#include <mkcl/internal.h>

/******************************* ------- ******************************/

#define	QUOTE	1
#define	EVAL	2
#define	LIST	3
#define	LISTX	4
#define	APPEND	5
#define	NCONC	6

/* extern int _mkcl_backq_car(MKCL, mkcl_object *px); */
static mkcl_object backq(MKCL, mkcl_object x);

static mkcl_object
kwote(MKCL, mkcl_object x)
{
  mkcl_type t = mkcl_type_of(x);
  if ((t == mkcl_t_symbol && !mkcl_Null(x) && !mkcl_keywordp(x)) ||
      t == mkcl_t_cons || t == mkcl_t_vector)
    x = MKCL_CONS(env, MK_CL_quote, mkcl_list1(env, x));
  return x;
}

/*
	_mkcl_backq_cdr(&x) puts result into x and returns one of

		QUOTE		the form should be quoted
		EVAL		the form should be evaluated
		LIST		the form should be applied to LIST
		LISTX		the form should be applied to LIST*
		APPEND		the form should be applied to APPEND
		NCONC		the form should be applied to NCONC
*/
static int
_mkcl_backq_cdr(MKCL, mkcl_object *px)
{
  mkcl_object x = *px, ax, dx;
  int a, d, out;

  if (MKCL_ATOM(x))
    return(QUOTE);
  if (MKCL_CAR(x) == MK_SI_unquote) {
    *px = MKCL_CADR(x);
    return(EVAL);
  }
  if (MKCL_CAR(x) == MK_SI_unquote_splice || MKCL_CAR(x) == MK_SI_unquote_nsplice)
    mkcl_FEerror(env, ",@@ or ,. has appeared in an illegal position.", 0);

  ax = MKCL_CAR(x); dx = MKCL_CDR(x);
  a = _mkcl_backq_car(env, &ax);
  d = _mkcl_backq_cdr(env, &dx);
  if (d == QUOTE) {
    switch (a) {
    case QUOTE:
      return(QUOTE);

    case EVAL:
      if (mkcl_Null(dx)) {
	out = LIST;
      } else if (MKCL_CONSP(dx) && mkcl_Null(MKCL_CDR(dx))) {
	dx = mkcl_list1(env, kwote(env, MKCL_CAR(dx)));
	out = LIST;
      } else {
	dx = mkcl_list1(env, kwote(env, dx));
	out = LISTX;
      }
      break;
    case APPEND:
    case NCONC:
      if (mkcl_Null(dx)) {
	*px = ax;
	return EVAL;
      } else {
	dx = mkcl_list1(env, kwote(env, dx));
	out = a;
      }
      break;
    default:
      mkcl_lose(env, "backquote botch");
    }
  } else if (d == EVAL) {
    switch (a) {
    case QUOTE:
      ax = kwote(env, ax);
      dx = mkcl_list1(env, dx);
      out = LISTX;
      break;
    case EVAL:
      dx = mkcl_list1(env, dx);
      out = LISTX;
      break;
    case APPEND:
    case NCONC:
      dx = mkcl_list1(env, dx);
      out = a;
      break;
    default:
      mkcl_lose(env, "backquote botch");
    }
  } else if (d == a) {
    out = d;
  } else {
    switch (d) {
    case LIST:
      if (a == QUOTE) {
	ax = kwote(env, ax);
	out = LIST;
	goto OUTPUT;
      } else if (a == EVAL) {
	out = LIST;
	goto OUTPUT;
      }
      dx = MKCL_CONS(env, MK_CL_list, dx);
      break;
    case LISTX:
      if (a == QUOTE) {
	ax = kwote(env, ax);
	out = LISTX;
	goto OUTPUT;
      } else if (a == EVAL) {
	out = LISTX;
	goto OUTPUT;
      }
      dx = MKCL_CONS(env, MK_CL_listX, dx);
      break;
    case APPEND:
      dx = MKCL_CONS(env, MK_CL_append, dx);
      break;
    case NCONC:
      dx = MKCL_CONS(env, MK_CL_nconc, dx);
      break;
    default:
      mkcl_lose(env, "backquote botch");
    }
    switch (a) {
    case QUOTE:
      ax = kwote(env, ax);
      dx = mkcl_list1(env, dx);
      out = LISTX;
      break;
    case EVAL:
      dx = mkcl_list1(env, dx);
      out = LISTX;
      break;
    case APPEND:
    case NCONC:
      dx = mkcl_list1(env, dx);
      out = a;
      break;
    default:
      mkcl_lose(env, "backquote botch");
    }
  }
 OUTPUT:
  *px = MKCL_CONS(env, ax, dx);
  return out;
}

/*
	_mkcl_backq_car(&x) puts result into x and returns one of

		QUOTE		the form should be quoted
		EVAL		the form should be evaluated
		APPEND		the form should be appended
				into the outer form
		NCONC		the form should be nconc'ed
				into the outer form
*/
int
_mkcl_backq_car(MKCL, mkcl_object *px)
{
  mkcl_object x = *px;
  int d;
 AGAIN:
  if (MKCL_ATOM(x))
    return(QUOTE);
  if (MKCL_CAR(x) == MK_SI_quasiquote) {
    x = *px = backq(env, MKCL_CADR(x));
    goto AGAIN;
  }
  if (MKCL_CAR(x) == MK_SI_unquote) {
    *px = MKCL_CADR(x);
    return EVAL;
  }
  if (MKCL_CAR(x) == MK_SI_unquote_splice) {
    *px = MKCL_CADR(x);
    return APPEND;
  }
  if (MKCL_CAR(x) == MK_SI_unquote_nsplice) {
    *px = MKCL_CADR(x);
    return NCONC;
  }
  d = _mkcl_backq_cdr(env, px);
  switch (d) {
  case QUOTE:
  case EVAL:
    return(d);

  case LIST:
    *px = MKCL_CONS(env, MK_CL_list, *px);
    break;

  case LISTX:
    *px = MKCL_CONS(env, MK_CL_listX, *px);
    break;

  case APPEND:
    *px = MKCL_CONS(env, MK_CL_append, *px);
    break;

  case NCONC:
    *px = MKCL_CONS(env, MK_CL_nconc, *px);
    break;

  default:
    mkcl_lose(env, "backquote botch");
  }
  return(EVAL);
}

static mkcl_object
backq(MKCL, mkcl_object x)
{
  int a;

  a = _mkcl_backq_car(env, &x);
  if (a == APPEND || a == NCONC)
    mkcl_FEerror(env, ",@@ or ,. has appeared in an illegal position.", 0);
  if (a == QUOTE)
    return(kwote(env, x));
  return(x);
}

static mkcl_object
quasiquote_macro(MKCL, mkcl_object whole, mkcl_object lex_env)
{
  if (mkcl_length(env, whole) != 2)
    mkcl_FEprogram_error(env, "Syntax error: ~S.", 1, whole);
  mkcl_return_value(backq(env, MKCL_CADR(whole)));
}

void
mkcl_init_backq(MKCL)
{
  mkcl_def_c_macro(env, MK_SI_quasiquote, quasiquote_macro, 2);
}

