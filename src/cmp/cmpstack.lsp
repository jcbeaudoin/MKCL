;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: C -*-
;;;;
;;;;  Copyright (c) 2006, Juan Jose Garcia-Ripoll
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Lesser General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 3 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../../Copyright' for full details.

;;;; CMPSTACK Manipulation of the lisp stack from C code
;;;;
;;;; Following special forms are provided:
;;;;
;;;;	(WITH-TEMP-STACK {form}*)
;;;;		Executes given forms, restoring the lisp stack on output.
;;;;	(TEMP-STACK-PUSH form)
;;;;	(TEMP-STACK-PUSH-VALUES form)
;;;;	(TEMP-STACK-POP nvalues)
;;;;

(in-package "COMPILER")

(defconstant +mkcl-temp-stack-frame-variable+ "_mkcl_inner_frame")

(defconstant +mkcl-local-temp-stack-frame-variable+ "__frame")

(defconstant +mkcl-local-stack-variable+ "__frame_sp")

(defun c1with-temp-stack (forms)
  (let* ((var (pop forms))
	 (body (c1expr `(let ((,var (innermost-temp-stack-frame))) ,@forms))))
      (make-c1form* 'WITH-TEMP-STACK
		    :type (c1form-type body)
		    :args body)))

(defun c2with-temp-stack (body)
  (let* ((new-destination (tmp-destination *destination*))
	 (*temp* *temp*))
    (wt-nl "{ struct mkcl_temp_stack_frame _mkcl_inner_frame_aux;")
    (wt-nl *volatile* "mkcl_object _mkcl_inner_frame = mkcl_temp_stack_frame_open(env,(mkcl_object)&_mkcl_inner_frame_aux,0);")
    (let* ((*destination* new-destination)
	   (*unwind-exit* `((STACK ,+mkcl-temp-stack-frame-variable+) ,@*unwind-exit*)))
      (c2expr* body))
    (wt-nl "mkcl_temp_stack_frame_close(env, _mkcl_inner_frame);}")
    (unwind-exit new-destination)))

(defun c1innermost-temp-stack-frame (args)
  (declare (ignore args))
  (c1expr `(c-inline () () :object ,+mkcl-temp-stack-frame-variable+
	    :one-liner t :side-effects nil)))

(defun c1temp-stack-push (args)
  (c1expr `(progn
	     (c-inline ,args (t t) :void "mkcl_temp_stack_frame_push(env, #0,#1)"
		       :one-liner t :side-effects t)
	     1)))

(defun c1temp-stack-push-values (args)
  (let ((frame-var (pop args))
	(form (pop args)))
    (make-c1form* 'TEMP-STACK-PUSH-VALUES :type '(VALUES)
		  :args
		  (c1expr form)
		  (c1expr `(c-inline (,frame-var) (t) :void "mkcl_temp_stack_frame_push_values(env, #0)"
				     :one-liner t :side-effects t)))))

(defun c2temp-stack-push-values (form push-statement)
  (let ((*destination* 'VALUES))
    (c2expr* form))
  (c2expr push-statement))

(defun c1temp-stack-pop (args)
  (c1expr `(c-inline ,args (t) (values &rest t)
		     "env->values[0]=mkcl_temp_stack_frame_pop_values(env, #0);" ;; mkcl_temp_stack_frame_pop_values sets env->nvalues properly
		     :one-liner nil :side-effects t)))

(defun c1apply-from-temp-stack-frame (args)
  (c1expr `(c-inline ,args (t t) (values &rest t)
		     "env->values[0]=mkcl_apply_from_temp_stack_frame(env, #0,#1); /* JCB value -2 */" ;; this one is incorrect.
		     :one-liner nil :side-effects t)))

(put-sysprop 'with-temp-stack 'C1 #'c1with-temp-stack)
(put-sysprop 'with-temp-stack 'c2 #'c2with-temp-stack)
(put-sysprop 'innermost-temp-stack-frame 'C1 #'c1innermost-temp-stack-frame)
(put-sysprop 'temp-stack-push 'C1 #'c1temp-stack-push)
(put-sysprop 'temp-stack-push-values 'C1 #'c1temp-stack-push-values)
(put-sysprop 'temp-stack-push-values 'C2 #'c2temp-stack-push-values)
(put-sysprop 'temp-stack-pop 'C1 #'c1temp-stack-pop)
(put-sysprop 'si::apply-from-temp-stack-frame 'c1 #'c1apply-from-temp-stack-frame)
