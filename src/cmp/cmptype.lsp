;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: C -*-
;;;;
;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;  Copyright (c) 2011, Jean-Claude Beaudoin.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Lesser General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 3 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../../Copyright' for full details.

;;;; CMPTYPE  Type information.

(in-package "COMPILER")

;;; CL-TYPE is any valid type specification of Common Lisp.
;;;
;;; TYPE is a representation type used by MKCL.  TYPE is one of:
;;;
;;;				T(BOOLEAN)
;;;
;;;	FIXNUM  CHARACTER  SINGLE-FLOAT  DOUBLE-FLOAT
;;;	(VECTOR T)  STRING  BIT-VECTOR  (VECTOR FIXNUM)
;;;	(VECTOR SINGLE-FLOAT)  (VECTOR DOUBLE-FLOAT)
;;;	(ARRAY T)  (ARRAY BASE-CHAR)  (ARRAY BIT)
;;;	(ARRAY FIXNUM)
;;;	(ARRAY SINGLE-FLOAT)  (ARRAY DOUBLE-FLOAT)
;;;	STANDARD-OBJECT STRUCTURE-OBJECT
;;;	SYMBOL
;;;	UNKNOWN
;;;
;;;				NIL
;;;
;;;
;;; immediate-type:
;;;	FIXNUM		int
;;;	CHARACTER	char
;;;	SINGLE-FLOAT	float
;;;	DOUBLE-FLOAT	double

(defun member-type (type disjoint-supertypes)
  (member type disjoint-supertypes :test #'subtypep))

;;; Check if THING is an object of the type TYPE.
;;; Depends on the implementation of TYPE-OF.
;;; (only used for saving constants?)
;; (defun object-type (thing)
;;   (let ((type (if thing (type-of thing) 'SYMBOL)))
;;     (case type
;;       ((FIXNUM SHORT-FLOAT SINGLE-FLOAT DOUBLE-FLOAT LONG-FLOAT SYMBOL NULL CONS) type)
;;       ((BASE-CHAR STANDARD-CHAR CHARACTER EXTENDED-CHAR) 'CHARACTER)
;;       ((STRING BASE-STRING BIT-VECTOR) type)
;;       (VECTOR (list 'VECTOR (array-element-type thing)))
;;       (ARRAY (list 'ARRAY (array-element-type thing)))
;;       (STANDARD-OBJECT 'STANDARD-OBJECT)
;;       (STRUCTURE-OBJECT 'STRUCTURE-OBJECT)
;;       (t t))))

(defun object-type (thing)
  (let ((type (if thing (type-of thing) 'SYMBOL)))
    (typecase thing
      (FIXNUM 'FIXNUM)
      ((or SHORT-FLOAT SINGLE-FLOAT DOUBLE-FLOAT LONG-FLOAT RATIO COMPLEX SYMBOL NULL CONS) type)
      ((or STANDARD-CHAR BASE-CHAR #+unicode EXTENDED-CHAR CHARACTER) type)
      (SIMPLE-BASE-STRING 'SIMPLE-BASE-STRING) (SIMPLE-STRING 'SIMPLE-STRING)
      (BASE-STRING 'BASE-STRING) (STRING 'STRING)
      (SIMPLE-BIT-VECTOR 'SIMPLE-BIT-VECTOR)
      (BIT-VECTOR 'BIT-VECTOR)
      (SIMPLE-VECTOR 'SIMPLE-VECTOR)
      (VECTOR (list 'VECTOR (array-element-type thing)))
      (SIMPLE-ARRAY (list 'SIMPLE-ARRAY (array-element-type thing)))
      (ARRAY (list 'ARRAY (array-element-type thing)))
      (STANDARD-OBJECT 'STANDARD-OBJECT)
      (STRUCTURE-OBJECT 'STRUCTURE-OBJECT)
      (t t))))




(defun known-type-p (type)
  (subtypep type 'T))

(defun type-and (t1 t2)
  ;; FIXME! Should we allow "*" as type name???
  (when (or (eq t1 t2) (eq t2 '*))
    (return-from type-and t1))
  (when (eq t1 '*)
    (return-from type-and t2))
  (let* ((si::*highest-type-tag* si::*highest-type-tag*)
	 (si::*save-types-database* t)
	 (si::*member-types* si::*member-types*)
	 (si::*elementary-types* si::*elementary-types*)
	 (tag1 (si::safe-canonical-type t1))
	 (tag2 (si::safe-canonical-type t2)))
    (cond ((and (numberp tag1) (numberp tag2))
	   (setf tag1 (si::safe-canonical-type t1)
		 tag2 (si::safe-canonical-type t2))
	   (cond ((zerop (logand tag1 tag2)) ; '(AND t1 t2) = NIL
		  NIL)
		 ((zerop (logandc2 tag1 tag2)) ; t1 <= t2
		  t1)
		 ((zerop (logandc2 tag2 tag1)) ; t2 <= t1
		  t2)
		 (t
		  `(AND ,t1 ,t2))))
	  ((eq tag1 'CONS)
	   (cmpwarn-style "Unsupported CONS type ~S. Replacing it with T." t1)
	   t2)
	  ((eq tag2 'CONS)
	   (cmpwarn-style "Unsupported CONS type ~S. Replacing it with T." t2)
	   t1)
	  ((null tag1)
	   (unless (and (consp t1) (eq 'values (first t1)))
	     ;; handling of (values ...) is just broken right now.
	     ;; So there is no point complaining about it. JCB
	     (cmpwarn-style "Unknown type ~S. Assuming it is T." t1))
	   t2)
	  (t
	   (unless (and (consp t2) (eq 'values (first t2)))
	     ;; handling of (values ...) is just broken right now.
	     ;; So there is no point complaining about it. JCB
	     (cmpwarn-style "Unknown type ~S. Assuming it is T." t2))
	   t1))))

(defun type-or (t1 t2)
  ;; FIXME! Should we allow "*" as type name???
  (when (or (eq t1 t2) (eq t2 '*))
    (return-from type-or t1))
  (when (eq t1 '*)
    (return-from type-or t2))
  (let* ((si::*highest-type-tag* si::*highest-type-tag*)
	 (si::*save-types-database* t)
	 (si::*member-types* si::*member-types*)
	 (si::*elementary-types* si::*elementary-types*)
	 (tag1 (si::safe-canonical-type t1))
	 (tag2 (si::safe-canonical-type t2)))
    (cond ((and (numberp tag1) (numberp tag2))
	   (setf tag1 (si::safe-canonical-type t1)
		 tag2 (si::safe-canonical-type t2))
	   (cond ((zerop (logandc2 tag1 tag2)) ; t1 <= t2
		  t2)
		 ((zerop (logandc2 tag2 tag1)) ; t2 <= t1
		  t1)
		 (t
		  `(OR ,t1 ,t2))))
	  ((eq tag1 'CONS)
	   (cmpwarn-style "Unsupported CONS type ~S. Replacing it with T." t1)
	   T)
	  ((eq tag2 'CONS)
	   (cmpwarn-style "Unsupported CONS type ~S. Replacing it with T." t2)
	   T)
	  ((null tag1)
	   (unless (and (consp t1) (eq 'values (first t1)))
	     ;; handling of (values ...) is just broken right now.
	     ;; So there is no point complaining about it. JCB
	     (cmpwarn "Unknown type ~S" t1))
	   T)
	  (t
	   (unless (and (consp t2) (eq 'values (first t2)))
	     ;; handling of (values ...) is just broken right now.
	     ;; So there is no point complaining about it. JCB
	     (cmpwarn "Unknown type ~S" t2))
	   T))))

(defvar *trace-type>=* nil)
(defun type>= (type1 type2)
  (let (val)
    (when *trace-type>=*
      (format t "~&In type>=: type1 = ~S, type2 = ~S.~%" type1 type2)
      (finish-output)
      )
    (setq val (subtypep type2 type1))
    (when *trace-type>=*
      (format t "~&In type>=: val = ~S.~%" val)
      (finish-output)
      )
    val
    )
  )

;;;
;;; and-form-type
;;;   returns a copy of form whose type is the type-and of type and the form's
;;;   type
;;;
(defun and-form-type (type form original-form &optional (mode :safe)
		      (format-string "") &rest format-args)
  (let* ((type2 (c1form-primary-type form))
	 (type1 (type-and type type2)))
    ;; We only change the type if it is not NIL. Is this wise?
    (if type1
	(setf (c1form-type form) type1)
	(funcall (if (eq mode :safe) #'cmperr #'cmpwarn)
		 "~?, the type of the form ~s is ~s, not ~s." format-string
		 format-args original-form type2 type))
    form))

(defun default-init (var &optional warn)
  (declare (ignore warn))
  (let ((new-value (cdr (assoc (var-type var)
			       '((fixnum . 0) (character . #\space)
                                 #+long-float (long-float 0.0L1)
				 (double-float . 0.0D1) (single-float . 0.0F1))
			       :test #'subtypep))))
    (if new-value
	(c1constant-value new-value :only-small-values t)
        (c1nil))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; TYPE PROPAGATORS
;;;

(in-package "COMPILER")

(defun simple-type-propagator (fname &rest form-types)
  (declare (ignore form-types))
  (let ((arg-types (get-arg-types fname))
	(return-type (or (get-return-type fname) '(VALUES &REST T))))
    (values arg-types return-type)))

(defun propagate-types (fname forms lisp-forms)
  (multiple-value-bind (arg-types return-type)
      (apply (or (get-sysprop fname 'C1TYPE-PROPAGATOR)
		 #'simple-type-propagator)
	     fname
	     forms)
    (when arg-types
      (do* ((types arg-types (rest types))
	    (fl forms (rest fl))
	    (al lisp-forms (rest al))
	    (i 1 (1+ i))
	    (in-optionals nil))
	   ((endp types)
	    (when fl
	      (cmpwarn "Too many arguments passed to ~A" fname)))
	(tagbody
          again
	  (let ((expected-type (first types)))
	    (when (member expected-type '(* &rest &key &allow-other-keys) :test #'eq)
	      (return))
	    (when (eq expected-type '&optional)
	      (when in-optionals
		(cmpwarn "Syntax error in type proclamation for function ~A.~&~A" fname arg-types))
	      (setq types (rest types))
	      (setq in-optionals t)
	      (go again))
	    (when (endp fl)
	      (unless in-optionals
		(cmperr #|cmpwarn|# "Too few arguments for proclaimed function ~A" fname)) ;; JCB
	      (return))
	    (let* ((form (first fl))
		   (lisp-form (first al))
		   (old-type (c1form-type form)))
	      (and-form-type expected-type form lisp-form
			     :safe "In the argument ~d of a call to ~a" i fname)
	      ;; In safe mode, we cannot assume that the type of the
	      ;; argument is going to be the right one.
	      (unless (zerop (cmp-env-optimization 'safety))
		(setf (c1form-type form) old-type)))))))
    return-type))

(defmacro def-type-propagator (fname lambda-list &body body)
  `(put-sysprop ',fname 'C1TYPE-PROPAGATOR
    #'(si::lambda-block ,fname ,lambda-list ,@body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; TYPE CHECKING
;;

(defun remove-function-types (type)
  ;; We replace this type by an approximate one that contains no function
  ;; types. This function may not produce the best approximation. Hence,
  ;; it is only used for optional type checks where we do not want to pass
  ;; TYPEP a complex type.
  (flet ((simplify-type (type)
           (cond #|((subtypep type '(NOT FUNCTION)) ;; JCB
                  type)|#
                 ((subtypep type 'FUNCTION)
                  'FUNCTION)
                 (t
                  type #|T|#)))) ;; JCB
    (if (atom type)
        (simplify-type type)
        (case (first type)
          ((OR AND NOT)
           (cons (first type)
                 (loop for i in (rest type) collect (remove-function-types i))))
          (FUNCTION 'FUNCTION)
          (otherwise (simplify-type type))))))

(defmacro optional-check-type (&whole whole var-name type &environment env)
  "Generates a type check that is only activated for the appropriate
safety settings and when the type is not trivial."
  (unless (policy-automatic-check-type-p env)
    (cmpnote "Unable to emit check for variable ~A" whole))
  (when (policy-automatic-check-type-p env)
    ;;(format t "~&About to insert check-type for var ~S of type ~S.~%" var-name type) ;; debug JCB
    (setf type (remove-function-types type))
    `(check-type ,var-name ,type)))

