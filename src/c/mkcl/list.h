/* -*- mode: c -*- */
/*
    eval.h -- Macros and parameters..
*/
/*
    Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
    Copyright (c) 1990, Giuseppe Attardi.

    MKCL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 3 of the License, or (at your option) any later version.

    See file './Copyright' for full details.
*/

#ifndef MKCL_LIST_H
#define MKCL_LIST_H

#define MKCL_CONS(env, a,d)	mkcl_cons((env),(a),(d))
#define MKCL_ACONS(env, a,b,c)	mkcl_cons((env),mkcl_cons((env),(a),(b)),(c))
#define MKCL_CAR(x)		(mkcl_Null(x) ? (x) : MKCL_CONS_CAR(x))
#define MKCL_CDR(x)		(mkcl_Null(x) ? (x) : MKCL_CONS_CDR(x))

#define MKCL_CAAR(x)	MKCL_CAR(MKCL_CAR(x))
#define MKCL_CADR(x)	MKCL_CAR(MKCL_CDR(x))
#define MKCL_CAAAR(x)	MKCL_CAR(MKCL_CAAR(x))
#define MKCL_CAADR(x)	MKCL_CAR(MKCL_CADR(x))
#define MKCL_CADAR(x)	MKCL_CAR(MKCL_CDAR(x))
#define MKCL_CADDR(x)	MKCL_CAR(MKCL_CDDR(x))
#define MKCL_CAAAAR(x)	MKCL_CAR(MKCL_CAAAR(x))
#define MKCL_CAAADR(x)	MKCL_CAR(MKCL_CAADR(x))
#define MKCL_CAADAR(x)	MKCL_CAR(MKCL_CADAR(x))
#define MKCL_CAADDR(x)	MKCL_CAR(MKCL_CADDR(x))
#define MKCL_CADAAR(x)	MKCL_CAR(MKCL_CDAAR(x))
#define MKCL_CADADR(x)	MKCL_CAR(MKCL_CDADR(x))
#define MKCL_CADDAR(x)	MKCL_CAR(MKCL_CDDAR(x))
#define MKCL_CADDDR(x)	MKCL_CAR(MKCL_CDDDR(x))

#define MKCL_CDAR(x)	MKCL_CDR(MKCL_CAR(x))
#define MKCL_CDDR(x)	MKCL_CDR(MKCL_CDR(x))
#define MKCL_CDAAR(x)	MKCL_CDR(MKCL_CAAR(x))
#define MKCL_CDADR(x)	MKCL_CDR(MKCL_CADR(x))
#define MKCL_CDDAR(x)	MKCL_CDR(MKCL_CDAR(x))
#define MKCL_CDDDR(x)	MKCL_CDR(MKCL_CDDR(x))
#define MKCL_CDAAAR(x)	MKCL_CDR(MKCL_CAAAR(x))
#define MKCL_CDAADR(x)	MKCL_CDR(MKCL_CAADR(x))
#define MKCL_CDADAR(x)	MKCL_CDR(MKCL_CADAR(x))
#define MKCL_CDADDR(x)	MKCL_CDR(MKCL_CADDR(x))
#define MKCL_CDDAAR(x)	MKCL_CDR(MKCL_CDAAR(x))
#define MKCL_CDDADR(x)	MKCL_CDR(MKCL_CDADR(x))
#define MKCL_CDDDAR(x)	MKCL_CDR(MKCL_CDDAR(x))
#define MKCL_CDDDDR(x)	MKCL_CDR(MKCL_CDDDR(x))

#endif /* MKCL_LIST_H */

