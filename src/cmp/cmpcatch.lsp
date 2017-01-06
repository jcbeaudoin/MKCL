;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: C -*-
;;;;
;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;  Copyright (c) 2012-2017, Jean-Claude Beaudoin.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Lesser General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 3 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../../Copyright' for full details.

;;;; CMPCATCH  Catch, Unwind-protect, and Throw.

(in-package "COMPILER")

(defun c1catch (args &aux (*load-control-flow-is-linear* nil))
  (check-args-number 'CATCH args 1)
  (incf *setjmps*)
  (make-c1form* 'CATCH :sp-change t :type t :args (c1expr (first args))
		(c1progn (rest args))))

(defun c2catch (tag body)
  (let* ((new-destination (tmp-destination *destination*)))
    (let* ((*destination* 'VALUE0))
      (c2expr* tag))
    (let* ((*destination* new-destination)
	   (code (incf *last-label*))
	   (*exit* (next-label))
	   (*unwind-exit* (list* *exit* 'FRAME *unwind-exit*)))
      (if (member new-destination '(TRASH VALUES))
	  (progn
	    (wt-nl "if (mkcl_frs_push(env," 'VALUE0 ")==0) {")
	    (wt-comment "BEGIN CATCH ~A" code)
	    (c2expr body)
	    (wt-nl "}")
	    (wt-nl "else { mkcl_maybe_reset_call_stack_overflow(env); mkcl_set_interrupt_status(env, &env->frs_top->frs_intr); }") ;; JCB
	    )
	  (progn
	    (wt-nl "if (mkcl_frs_push(env," 'VALUE0 ")) {")
	    (wt-comment "BEGIN CATCH ~A" code)
	    (wt-nl "  mkcl_maybe_reset_call_stack_overflow(env);") ;; JCB
	    (wt-nl "  mkcl_set_interrupt_status(env, &env->frs_top->frs_intr);") ;; as precaution JCB
	    (unwind-exit 'VALUES t)
	    (wt-nl "}")
	    (c2expr body)))
      (wt-label *exit*)
      (wt-nl "mkcl_frs_pop(env);") 
      (wt-comment "END CATCH ~A" code)
      )
    (unwind-exit new-destination)))

(defun c1unwind-protect (args)
  (check-args-number 'UNWIND-PROTECT args 1)
  (incf *setjmps*)
  (let (form)
    (let ((*cmp-env* (cmp-env-mark 'UNWIND-PROTECT)))
      (setq form (c1expr (first args))))
    (make-c1form* 'UNWIND-PROTECT :type (c1form-type form) :sp-change t
		  :args form (c1progn (rest args)))))

(defun c2unwind-protect (form body)
  (let* ((sp (make-lcl-var :rep-type :cl-index))
	 (nargs (make-lcl-var :rep-type :cl-index))
	 (*unwind-exit* `((STACK ,sp) ,@*unwind-exit*)))
    (wt-nl "{")
    (wt-nl "volatile bool __unwinding = FALSE;")
    (wt-nl "mkcl_index " sp "=MKCL_TEMP_STACK_INDEX(env)," nargs ";")
    (wt-nl "mkcl_frame_ptr next_fr;")
    ;; Here we compile the form which is protected. When this form
    ;; is aborted, it continues at the mkcl_frs_pop() with unwinding=TRUE.
    (wt-nl "if (mkcl_frs_push(env,MKCL_PROTECT_TAG)) {")
    (wt-nl "  mkcl_maybe_reset_call_stack_overflow(env);") ;; JCB
    (wt-nl "  __unwinding = TRUE; next_fr=env->nlj_fr;")
    (wt-nl "} else {")
    (let ((*unwind-exit* (cons 'FRAME *unwind-exit*))
	  (*destination* 'VALUES))
      (c2expr* form))
    (wt-nl "env->disable_interrupts = TRUE; __unwinding = FALSE; }")
    (wt-nl "{ const mkcl_interrupt_status __old_intr_stat = (env)->frs_top->frs_intr;")
    (wt-nl "mkcl_frs_pop(env);")
    ;; Here we save the values of the form which might have been
    ;; aborted, and execute some cleanup code. This code may also
    ;; be aborted by some control structure, but is not protected.
    (wt-nl nargs "=mkcl_stack_push_values(env);")
    (let ((*destination* 'TRASH))
      (c2expr* body))
    (wt-nl "mkcl_stack_pop_values(env," nargs ");")
    ;; Finally, if the protected form was aborted, jump to the
    ;; next catch point...
    (wt-nl "if (__unwinding) mkcl_unwind(env,next_fr);")
    (wt-nl "else mkcl_set_interrupt_status(env, &__old_intr_stat);")
    ;; ... or simply return the values of the protected form.
    (unwind-exit 'VALUES)
    (wt "}")
    (wt "}")))

(defun c1throw (args)
  (check-args-number 'THROW args 2 2)
  (make-c1form* 'THROW :args (c1expr (first args)) (c1expr (second args))))

(defun c2throw (tag val &aux loc)
  (case (c1form-name tag)
    ((VAR LOCATION) (setq loc (c1form-arg 0 tag)))
    (t (setq loc (make-temp-var))
       (let ((*destination* loc)) (c2expr* tag))))
  (let ((*destination* 'VALUES)) (c2expr* val))
  (wt-nl "mkcl_throw(env, " loc ");"))

;;; ----------------------------------------------------------------------

(put-sysprop 'CATCH 'C1SPECIAL 'c1catch)
(put-sysprop 'CATCH 'C2 'c2catch)
(put-sysprop 'UNWIND-PROTECT 'C1SPECIAL 'c1unwind-protect)
(put-sysprop 'UNWIND-PROTECT 'C2 'c2unwind-protect)
(put-sysprop 'THROW 'C1SPECIAL 'c1throw)
(put-sysprop 'THROW 'C2 'c2throw)
