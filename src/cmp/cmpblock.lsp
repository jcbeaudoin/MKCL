;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: C -*-
;;;;
;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;  Copyright (c) 2012, Jean-Claude Beaudoin.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Lesser General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 3 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../../Copyright' for full details.

;;;; CMPBLOCK  Block and Return-from.

(in-package "COMPILER")

;;; A dummy variable is created to hold the block identifier.  When a
;;; reference to the block (via return-from) is found, the var-ref
;;; count for that variable is incremented only if the reference
;;; appears across a boundary (CB, LB or UNWIND-PROTECT), while the
;;; blk-ref is always incremented.  Therefore blk-ref represents
;;; whether the block is used at all and var-ref for the dummy
;;; variable represents whether a block identifier must be created and
;;; stored in such variable.

(defun c1block (args &aux (*load-control-flow-is-linear* nil))
  (check-args-number 'BLOCK args 1)
  (let ((block-name (first args)))
    (unless (symbolp block-name)
      (cmperr "The block name ~s is not a symbol." block-name))
    (let* ((blk-var (make-var :name block-name :kind 'LEXICAL))
	   (blk (make-blk :var blk-var :name block-name))
	   (body (let ((*cmp-env* (cmp-env-copy)))
		   (cmp-env-register-block blk)
		   (c1progn (rest args)))))
      (when (or (blk-ref-ccb blk) (blk-ref-clb blk))
	(incf *setjmps*))
      (if (plusp (blk-ref blk))
	  ;; FIXME! By simplifying the type of a BLOCK form so much (it is
	  ;; either NIL or T), we lose a lot of information.
	  (make-c1form* 'BLOCK
			:local-vars (list blk-var)
			:type (type-or (blk-type blk) (c1form-type body))
			:args blk body)
	  body))))

(defun c2block (blk body)
  (if (plusp (var-ref (blk-var blk)))
      (let* ((blk-var (blk-var blk))
	     (*env-lvl* *env-lvl*)
	     closure-block-id
	     (*closure-levels* *closure-levels*))
	(setf (blk-exit blk) *exit*
	      (blk-destination blk) *destination*)
	(wt "{")
	(unless (or (blk-ref-ccb blk) (blk-ref-clb blk))
	  (setf (var-kind blk-var) :object
		(var-loc blk-var) (next-lcl))
	  (wt " mkcl_object " blk-var ";"))
	(when (env-grows (blk-ref-ccb blk))
	  (push 0 *closure-levels*)
	  (setq closure-block-id (incf *closure-block-id*) *cenv0-used* t)
	  (let ((env-lvl *env-lvl*))
	    (incf *env-lvl*)
	    (wt-nl "mkcl_object cenv" *env-lvl* " = mkcl_alloc_clevel_block(env, "
		   (if *written-function* "this_func" "mk_cl_Cnil")
		   ", cenv" env-lvl ", "
		   (written-function-cname) "_cblock_" closure-block-id ");")
	    (when (>= *debug-fun* 2)
	      (wt-nl "mkcl_object syms_cenv" *env-lvl* " = mkcl_alloc_clevel_block(env, "
		     (if *written-function* "this_func" "mk_cl_Cnil")
		     ", syms_cenv" env-lvl ", "
		     (written-function-cname) "_cblock_" closure-block-id ");")
	      )
	    )
	  )
	(bind "MKCL_NEW_FRAME_ID(env)" blk-var) (wt-comment "BLOCK")
	(wt-nl "if (mkcl_frs_push(env," blk-var ")!=0) {")
	(wt-nl "  mkcl_maybe_reset_call_stack_overflow(env);") ;; JCB
	(wt-nl "  mkcl_set_interrupt_status(env, &env->frs_top->frs_intr);") ;; JCB
	(let ((*unwind-exit* (cons 'FRAME *unwind-exit*)))
	  (unwind-exit 'VALUES)
	  (wt-nl "} else {")
	  (c2expr body)
	  (wt "}"))
	(when (blk-ref-ccb blk) (decf *env*))
	(when closure-block-id
	  (wt-nl-h "#define "
		   (written-function-cname)
		   "_cblock_" closure-block-id " "
		   (first *closure-levels*))
	  )
	(wt-nl "}"))
      (progn
	(setf (blk-exit blk) *exit*)
	(setf (blk-destination blk) *destination*)
	(c2expr body)))
  )

(defun c1return-from (args)
  (check-args-number 'RETURN-FROM args 1 2)
  (let ((name (first args)))
    (unless (symbolp name)
      (cmperr "The block name ~s is not a symbol." name))
    (multiple-value-bind (blk ccb clb unw)
	(cmp-env-search-block name)
      (unless blk
	(cmperr "The block ~s is undefined." name))
      (let* ((val (c1expr (second args)))
	     (var (blk-var blk))
	     (type T))
	(cond (ccb (setf (blk-ref-ccb blk) t
			 type 'CCB
			 (var-kind var) 'CLOSURE
			 (var-ref-ccb var) T)
		   (incf (var-ref var)))
	      (clb (setf (blk-ref-clb blk) t
			 type 'CLB)
		   (incf (var-ref var)))
	      (unw (setf type 'UNWIND-PROTECT)
		   (incf (var-ref var))))
	(incf (blk-ref blk))
	(setf (blk-type blk) (type-or (blk-type blk) (c1form-primary-type val)))
	(add-to-read-nodes var (make-c1form* 'RETURN-FROM :type 'T
					     :args blk type val))))))

(defun c2return-from (blk type val)
  (case type
    (CCB
     (let ((*destination* 'VALUES)) (c2expr* val))
     (wt-nl "mkcl_return_from(env, " (blk-var blk) "," (add-symbol (blk-name blk)) ");"))
    ((CLB UNWIND-PROTECT)
     (let ((*destination* 'VALUES)) (c2expr* val))
     (wt-nl "mkcl_return_from(env, " (blk-var blk) ",mk_cl_Cnil);"))
    (T (let ((*destination* (blk-destination blk))
	     (*exit* (blk-exit blk)))
	 (c2expr val))))
  )

;;; ----------------------------------------------------------------------

(put-sysprop 'BLOCK 'C1SPECIAL 'c1block)
(put-sysprop 'BLOCK 'C2 'c2block)

(put-sysprop 'RETURN-FROM 'C1SPECIAL 'c1return-from)
(put-sysprop 'RETURN-FROM 'C2 'c2return-from)
