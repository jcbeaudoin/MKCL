/* -*- mode: c -*- */

#ifndef MKCL_INL_H
#define MKCL_INL_H

/*
 * Loops over a proper list. Complains on circularity
 */
#define mkcl_loop_for_in_no_circle(env, list) {				\
  mkcl_object __slow;							\
  bool __flag = TRUE;							\
  for (__slow = list; !mkcl_endp(list); list = MKCL_CONS_CDR(list)) {	\
  if ((__flag = !__flag)) {						\
    if (__slow == list) mkcl_FEcircular_list(env, list);		\
    __slow = MKCL_CONS_CDR(__slow);					\
  }

/*
 * Loops over a proper list
 */
#define mkcl_loop_for_in(env, list) {					\
  const mkcl_object __mkcl_l0 = list;					\
  for (; list != mk_cl_Cnil; list = MKCL_CONS_CDR(list)) {		\
  if (!MKCL_F_CONSP(list)) mkcl_FEtype_error_proper_list(env, __mkcl_l0);

#define mkcl_end_loop_for_in }}

/*
 * Loops over a dotted list. Complains on circularity.
 */
#define mkcl_loop_for_on_no_circle(list)				\
  if (!MKCL_CONSP(list)) {						\
    if (list != mk_cl_Cnil) mkcl_FEtype_error_list(list);		\
  } else {								\
  mkcl_object __slow;							\
  bool __flag = TRUE;							\
  for (__slow = list; MKCL_CONSP(list); list = MKCL_CONS_CDR(list)) {	\
  if ((__flag = !__flag)) {						\
    if (__slow == list) mkcl_FEcircular_list(list);			\
    __slow = MKCL_CDR(__slow);						\
  }

/*
 * Loops over a list. Ignores errors.
 */
#define mkcl_loop_for_on_unsafe(list) {				\
  for (; MKCL_CONSP(list); list = MKCL_CONS_CDR(list)) {

/*
 * Loops over a dotted list
 */
#define mkcl_loop_for_on(env, list)				\
  if (!MKCL_CONSP(list)) {					\
    if (list != mk_cl_Cnil) mkcl_FEtype_error_list(env, list);	\
  } else {							\
  for (; MKCL_CONSP(list); list = MKCL_CONS_CDR(list)) {

#define mkcl_end_loop_for_on }}


#endif /* MKCL_INL_H */
