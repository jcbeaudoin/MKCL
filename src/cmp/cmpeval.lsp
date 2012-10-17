;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: C -*-
;;;;
;;;; CMPEVAL --  The Expression Dispatcher.

;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;
;;;;    ECoLisp is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Lesser General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 3 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../../Copyright' for full details.

(in-package "COMPILER")

(defun c1expr (form)
  (setq form (catch *cmperr-tag*
    (cond ((symbolp form)
	   (setq form (chk-symbol-macrolet form))
	   (cond ((not (symbolp form))
		  (c1expr form))
		 ((eq form nil) (c1nil))
		 ((eq form t) (c1t))
		 ((keywordp form)
		  (make-c1form* 'LOCATION :type (object-type form)
				:args (add-symbol form)))
		 ((constantp form)
		  (or (c1constant-value (symbol-value form) :only-small-values t)
		      (c1var form)))
		 (t (c1var form))))
	  ((consp form)
	   (let ((fun (car form)))
	     (cond ((symbolp fun)
		    (c1call-symbol fun (cdr form)))
		   ((and (consp fun) (eq (car fun) 'LAMBDA))
		    (c1funcall form))
		   (t (cmperr "~s is not a legal function name." fun)))))
	  (t (c1constant-value form :always t)))))
  (if (eq form '*cmperr-tag*)
      (c1nil)
      form))

(defvar *c1nil* (make-c1form* 'LOCATION :type (object-type nil) :args nil))
(defun c1nil () *c1nil*)
(defvar *c1t* (make-c1form* 'LOCATION :type (object-type t) :args t))
(defun c1t () *c1t*)

(defun c1call-symbol (fname args &aux fd)
  (cond ((setq fd (get-sysprop fname 'c1special)) (funcall fd args))
	((c1call-local fname args))
	((setq fd (cmp-env-search-macro fname))
	 (c1expr (cmp-expand-macro fd (list* fname args))))
	((and (setq fd (get-sysprop fname 'C1))
	      (inline-possible fname))
	 (funcall fd args))
	((and (setq fd (get-sysprop fname 'C1CONDITIONAL))
	      (inline-possible fname)
	      (funcall fd args)))
	((and (setq fd (compiler-macro-function fname))
	      (inline-possible fname)
	      (let ((success nil))
		(multiple-value-setq (fd success)
		  (cmp-expand-macro fd (list* fname args)))
		success))
	 (c1expr fd))
	((setq fd (macro-function fname))
	 (c1expr (cmp-expand-macro fd (list* fname args))))
	(t (c1call-global fname args))))

(defun c1call-local (fname args)
  (let ((fun (local-function-ref fname)))
    (when fun
      (when (> (length args) si::c-arguments-limit)
	(return-from c1call-local (unoptimized-long-call `#',fname args)))
      (let* ((forms (c1args* args))
	     (lambda-form (fun-lambda fun))
	     (return-type (or (get-local-return-type fun) 'T))
	     (arg-types (get-local-arg-types fun)))
	(declare (ignore lambda-form))
	;; Add type information to the arguments.
	(when arg-types
	  (let ((fl nil))
	    (dolist (form forms)
	      (cond ((endp arg-types) (push form fl))
		    (t (push (and-form-type (car arg-types) form (car args)
					    :safe "In a call to ~a" fname)
			     fl)
		       (pop arg-types)
		       (pop args))))
	    (setq forms (nreverse fl))))
	(make-c1form* 'CALL-LOCAL :sp-change t :type return-type
		      :args fun forms)))))

(defun c1call-global (fname args)
  (let ((l (length args))
	)
    (cond ((> l si::c-arguments-limit)
	   (unoptimized-long-call `#',fname args))
	  ((maybe-optimize-structure-access fname args))
	  ((maybe-optimize-generic-function fname args))
	  (t
	   (let* ((forms (c1args* args))
		  (return-type (propagate-types fname forms args)))
	     (make-c1form* 'CALL-GLOBAL
			   :sp-change (function-may-change-sp fname)
			   :type return-type
			   :args fname forms
                           ;; loc and type are filled by c2expr
                           ))))))

(defun c2expr (form)
  (let* ((name (c1form-name form))
         (args (c1form-args form))
         (dispatch (get-sysprop name 'C2)))
    (if (or (eq name 'LET) (eq name 'LET*))
        (let ((*volatile* (c1form-volatile* form)))
          (declare (special *volatile*))
          (apply dispatch args))
        (apply dispatch args))))

(defun c2expr* (form)
  (let* ((*exit* (next-label))
	 (*unwind-exit* (cons *exit* *unwind-exit*))
	 (*lcl* *lcl*)
	 (*temp* *temp*))
    (c2expr form)
    (wt-label *exit*))
  )

(defun c1progn (forms)
  (cond ((endp forms) (t1/c1expr 'NIL))
	((endp (cdr forms)) (t1/c1expr (car forms)))
	(t (let* ((fl (mapcar #'t1/c1expr forms))
		  (output-form (first (last fl)))
		  (output-type (and output-form (c1form-type output-form))))
	     (make-c1form* 'PROGN :type output-type :args fl)))))

(defun c2progn (forms)
  ;; c1progn ensures that the length of forms is not less than 1.
  (do ((l forms (cdr l))
       (lex *lex*))
      ((endp (cdr l))
       (c2expr (car l)))
    (let* ((this-form (first l))
	   (name (c1form-name this-form)))
      (let ((*destination* 'TRASH))
	(c2expr* (car l)))
      (setq *lex* lex)	; recycle lex locations
      ;; Since PROGN does not have tags, any transfer of control means
      ;; leaving the current PROGN statement.
      (when (or (eq name 'GO) (eq name 'RETURN-FROM))
	(cmpnote "Eliminating unreachable code")
	(return)))))

(defun c1args* (forms)
  (mapcar #'(lambda (form) (c1expr form)) forms))

;;; ----------------------------------------------------------------------

#|
(defvar *compiler-temps*
	'(tmp0 tmp1 tmp2 tmp3 tmp4 tmp5 tmp6 tmp7 tmp8 tmp9))

(defmacro sys::define-inline-function (name vars &body body)
  (let ((temps nil)
	(*compiler-temps* *compiler-temps*))
    (dolist (var vars)
      (if (and (symbolp var)
	       (not (member var '(&OPTIONAL &REST &KEY &AUX) :test #'eq)))
	(push (or (pop *compiler-temps*)
		  (gentemp "TMP" (find-package 'COMPILER)))
	      temps)
	(error "The parameter ~s for the inline function ~s is illegal."
	       var name)))
    (let ((binding (cons 'LIST (mapcar
				#'(lambda (var temp) `(list ',var ,temp))
				vars temps))))
      `(progn
	 (defun ,name ,vars ,@body)
	 (define-compiler-macro ,name ,temps (list* 'LET ,binding ',body))))))
|#

;;; ----------------------------------------------------------------------

(put-sysprop 'PROGN 'C1SPECIAL 'c1progn)
(put-sysprop 'PROGN 'C2 'c2progn)
