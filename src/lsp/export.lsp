;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: SYSTEM -*-
;;;;
;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;  Copyright (c) 2010-2013, Jean-Claude Beaudoin.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Lesser General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 3 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../../Copyright' for full details.
;;;;                    Exporting external symbols of LISP package

;;; Bootstrap Note:  Because of our load-time evaluation of 
;;; compound form operators we are limited at this point
;;; to making references only to functions already defined
;;; and we have to avoid making forward function references.
;;; In the context here this limits us to using only functions
;;; already defined in our C runtime.
;;; This limitation holds until #'generate-forward-fun-ref-handler gets
;;; properly defined somewhere down here and thus becomes available
;;; to handle those forward function references. JCB

(eval-when (eval compile load)
  (si::select-package "SI"))

;;; ----------------------------------------------------------------------
;;;
#+(or)
(progn
  (*make-special '*dump-defun-definitions*) ;; usefulness? JCB
  (setq *dump-defun-definitions* nil)
  (*make-special '*dump-defmacro-definitions*) ;; usefulness? JCB
  (setq *dump-defmacro-definitions* *dump-defun-definitions*)
  )

;; This is needed only when bootstrapping MKCL using MKCL-MIN
(eval-when (eval)
  (si::fset 'si:register-with-pde
	  #'(si::lambda-block si:register-with-pde (whole env)
	       (let* ((definition (second whole))
		      (output-form (third whole)))
		 `(if si:*register-with-pde-hook*
		      (funcall si:*register-with-pde-hook*
			       (copy-tree *source-location*)
			       ,definition
			       ,output-form)
		      ,output-form)))
	  t)
  (si::fset 'defun
	  #'(si::lambda-block defun (def env)
	      (let* ((name (second def))
		     (function `#'(si::lambda-block ,@(cdr def))))
		#+(or)
		(when *dump-defun-definitions*
		  (print function)
		  (setq function `(si::bc-disassemble ,function)))
		(si:register-with-pde def `(si::fset ',name ,function))))
	  t)
 (si::fset 'in-package
 	  #'(si::lambda-block in-package (def env)
	      `(eval-when (eval compile load)
		(si::select-package ,(string (second def)))))
 	  t)
)

(let ((f #'(si::lambda-block si::define-when (whole env)
	     (declare (ignore env))
	     (let* ((phase (second whole))
		    (body (nthcdr 2 whole))
		    (compile-or-load nil)
		    (top-body nil)
		    (execute (or (member 'eval phase) (member :execute phase))))
	       (when (or (member 'load phase) (member :load-toplevel phase))
		 (setq compile-or-load (cons :load-toplevel compile-or-load)))
	       (when (or (member 'compile phase) (member :compile-toplevel phase))
		 (setq compile-or-load (cons :compile-toplevel compile-or-load)))
	       (when execute 
		 (setq top-body (cons `(eval-when (:execute)
						  ;;(if (not (member :mkcl-min *features*))
						  ;;   (progn (print "!! Eval time LOAD-COMPILE-LOCK for ") (princ ',body)))
						  (mt:with-lock (mt:+load-compile-lock+) ,@body)) top-body)))
	       (when compile-or-load 
		 (setq top-body (cons `(eval-when ,compile-or-load ,@body) top-body)))
	       `(progn
		  ,@top-body)))))
  (si::fset 'si::define-when f t))


(let ((f #'(si::lambda-block mt:with-lock (whole env) (declare (ignore env)) `(progn ,@(cddr whole)))))
  (si::fset 'mt:with-lock f t)) ;; bootstrap version. redefined in mp.lsp

;;;
;;;

(*make-special '*warn-on-forward-reference*)
(setq *warn-on-forward-reference* nil)

;;; function generate-forward-fun-ref-handler has to be the very first defun
;;; loaded by MKCL if we want the load-time function reference vector
;;; to be generated properly.
;;
;;; If you ever change the structure of this closure you will need to adjust
;;; accordingly the code of mkcl_fun_ref_fdefinition() in cfun.d. JCB
(defun generate-forward-fun-ref-handler (fun-or-cblock 
					 index
					 &aux 
					 (fun-ref nil)
					 (fname (get-fun-ref-sym fun-or-cblock index)))
  (declare (optimize (debug 0))) ;; hide it from stack trace. JCB

  (if (and *warn-on-forward-reference* (not (eq 'si::code-block (type-of fun-or-cblock))))
      (warn "Forward reference to function named ~A from function ~S" fname fun-or-cblock))
  #'(si::lambda-block forward-fun-ref-handler (&rest args)
      (declare (dynamic-extent args))
      (if fun-ref
	  (apply fun-ref args)
	(progn
	  (mt:with-lock (mt:+forward-reference-lock+)
	    (if (null fun-ref)
		(if (fboundp fname)
		    (setq fun-ref (fdefinition fname)))))
	  (if fun-ref
	      (apply fun-ref args)
	    (error 'undefined-function :name fname))))))

;;
;; This is also needed for booting MKCL. In particular it is required in
;; defmacro.lsp.
;;
(let ((f #'(si::lambda-block dolist (whole env)
	   (declare (ignore env))
	   (let (body pop finished control var expr exit)
	     (setq body (rest whole))
	     (when (endp body)
	       (simple-program-error "Syntax error in ~A:~%~A" 'DOLIST whole))
	     (setq control (first body) body (rest body))
	     (when (endp control)
	       (simple-program-error "Syntax error in ~A:~%~A" 'DOLIST whole))
	     (setq var (first control) control (rest control))
	     (if (<= 1 (length control) 2)
		 (setq expr (first control) exit (rest control))
		 (simple-program-error "Syntax error in ~A:~%~A" 'DOLIST whole))
	     (multiple-value-bind (declarations body)
		 (process-declarations body nil)
	       `(block nil
		 (let* ((%dolist-var ,expr)
			,var)
		   (declare ,@declarations)
		   (si::while %dolist-var
		      (setq ,var (first %dolist-var))
		      ,@body
		      (setq %dolist-var (rest %dolist-var)))
		   ,(when exit `(setq ,var nil))
		   ,@exit)))))))
  (si::fset 'dolist f t))

;; This version of dolist removes constraints imposed on the iteration variable by ANSI CL,
;; and thus makes dolist much more useful for optimization through declarations.
(let ((f #'(si::lambda-block mkcl:dolist! (whole env)
	   (declare (ignore env))
	   (let (body pop finished control var expr exit)
	     (setq body (rest whole))
	     (when (endp body)
	       (simple-program-error "Syntax error in ~A:~%~A" 'MKCL:DOLIST! whole))
	     (setq control (first body) body (rest body))
	     (when (endp control)
	       (simple-program-error "Syntax error in ~A:~%~A" 'MKCL:DOLIST! whole))
	     (setq var (first control) control (rest control))
	     (if (<= 1 (length control) 2)
		 (setq expr (first control) exit (rest control))
		 (simple-program-error "Syntax error in ~A:~%~A" 'MKCL:DOLIST! whole))
	     (multiple-value-bind (declarations body)
		 (process-declarations body nil)
	       `(block nil
		 (let ((%dolist!-var ,expr))
		   (si::while (consp %dolist!-var)
		      (let* ((%dolist!-inner-var %dolist!-var)
			     (,var (first %dolist!-inner-var)))
			(declare (type cons %dolist!-inner-var))
			(declare ,@declarations)
			,@body
			(setq %dolist!-var (rest %dolist!-inner-var))))
		   ,@exit)))))))
  (si::fset 'mkcl:dolist! f t))



(let ((f #'(si::lambda-block dotimes (whole env)
           (declare (ignore env))
	   (let (body pop finished control var expr exit)
	     (setq body (rest whole))
	     (when (endp body)
	       (simple-program-error "Syntax error in ~A:~%~A" 'DOTIMES whole))
	     (setq control (first body) body (rest body))
	     (when (endp control)
	       (simple-program-error "Syntax error in ~A:~%~A" 'DOTIMES whole))
	     (setq var (first control) control (rest control))
	     (if (<= 1 (length control) 2)
		 (setq expr (first control) exit (rest control))
		 (simple-program-error "Syntax error in ~A:~%~A" 'DOTIMES whole))
	     (multiple-value-bind (declarations body)
		 (process-declarations body nil)
	       `(block nil
		 (let* ((%dotimes-var ,expr)
			(,var 0))
		   (declare ,@declarations)
		   (si::while (< ,var %dotimes-var)
		     ,@body
		     (setq ,var (1+ ,var)))
		   ,@exit)))))))
  (si::fset 'dotimes f t))

(let ((f #'(si::lambda-block do/do*-expand (whole env)
	   (declare (ignore env))
           (let (do/do* control test result vl step let psetq body)
	     (setq do/do* (first whole) body (rest whole))
	     (if (eq do/do* 'do)
		 (setq let 'LET psetq 'PSETQ)
		 (setq let 'LET* psetq 'SETQ))
	     (when (endp body)
	       (simple-program-error "Syntax error in ~A:~%~A" do/do* whole))
	     (setq control (first body) body (rest body))
	     (when (endp body)
	       (simple-program-error "Syntax error in ~A:~%~A" do/do* whole))
	     (setq test (first body) body (rest body))
	     (when (endp test)
	       (simple-program-error "Syntax error in ~A:~%~A" do/do* whole))
	     (setq result (rest test) test (first test))
	     (dolist (c control)
	       (when (symbolp c) (setq c (list c)))
	       (case (length c)
		 ((1 2)
		  (setq vl (cons c vl)))
		 ((3)
		  (setq vl (cons (butlast c) vl)
			step (list* (third c) (first c) step)))
		 (t
		  (simple-program-error "Syntax error in ~A:~%~A" do/do* whole))))
	     (multiple-value-bind (declarations real-body)
		 (process-declarations body nil)
	       `(BLOCK NIL
		 (,let ,(nreverse vl)
		   (declare ,@declarations)
		   (sys::until ,test
		      ,@real-body
		      ,@(when step (list (cons psetq (nreverse step)))))
		   ,@(or result '(nil)))))))))
  (si::fset 'do f t)
  (si::fset 'do* f t))

(defun eval-feature (x &aux operator)
  (cond ((symbolp x)
	 (and (member x *features* :test #'eq) t))
	((atom x) (error "~ is not allowed as a feature" x))
	((not (symbolp (setq operator (first x))))
	 (error "~S is not a valid feature expression." x))
        ((eql operator :AND)
         (dolist (x (cdr x) t) (when (not (eval-feature x)) (return nil))))
        ((eql operator :OR)
         (dolist (x (cdr x) nil) (when (eval-feature x) (return t))))
        ((eql operator :NOT)
	 (not (eval-feature (second x))))
	(t (error "~S is not a valid feature expression." x))))

(sys:*make-constant 'keyword-package (find-package "KEYWORD"))

(defun do-read-feature (stream subchar arg test)
  (when arg
    (error "Reading from ~S: no number should appear between # and ~A"
	   stream subchar))
  (let ((feature (let ((*package* keyword-package #|(find-package "KEYWORD")|#))
		   (read stream t nil t))))
    (if (and (not *read-suppress*) (eq (eval-feature feature) test))
	(read stream t nil t)
	(let ((*read-suppress* t)) (read stream t nil t) (values)))))

(defun sharp-+-reader (stream subchar arg)
  (do-read-feature stream subchar arg T))

(defun sharp---reader (stream subchar arg)
  (do-read-feature stream subchar arg NIL))

(set-dispatch-macro-character #\# #\+ 'sharp-+-reader)
(set-dispatch-macro-character #\# #\+ 'sharp-+-reader (sys::standard-readtable))

(set-dispatch-macro-character #\# #\- 'sharp---reader)
(set-dispatch-macro-character #\# #\- 'sharp---reader (sys::standard-readtable))



