;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: SYSTEM -*-
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

;;----------------------------------------------------------------------
;; (FUNCTION ...) types. This code is a continuation of predlib.lsp.
;; It implements function types and a SUBTYPEP relationship between them.
;;
;; This code was moved away from cmptype.lsp (2011/10/19)
;;

(in-package "SYSTEM")

(defstruct function-type
  required
  optional
  rest
  key-p
  keywords
  keyword-types
  allow-other-keys-p
  output)

(defun canonical-function-type (ftype)
  (when (function-type-p ftype)
    (return-from canonical-function-type ftype))
  (flet ((ftype-error ()
	   (error "Syntax error in FUNCTION type definition ~S" ftype))) ;; Should this be a cmperr instead? JCB
    (let (o k k-t values)
      (unless (and (= (length ftype) 3) (eql (first ftype) 'FUNCTION))
	(ftype-error))
      (multiple-value-bind (requireds nb_reqs optionals nb_opts rest key-flag keywords nb_keys
				      allow-other-keys-p auxs)
	  (si::process-lambda-list (second ftype) 'FTYPE)
	(declare (ignore nb_reqs auxs))
	(dotimes (i nb_opts)
	  (let ((type (first optionals))
		(init (second optionals))
		(flag (third optionals)))
	    (setq optionals (cdddr optionals))
	    (when (or init flag) (ftype-error))
	    (push type o)))
	(dotimes (i nb_keys)
	  (let ((keyword (first keywords))
		(var (second keywords))
		(type (third keywords))
		(flag (fourth keywords)))
	    (setq keywords (cddddr keywords))
	    (when (or var flag) (ftype-error))
	    (push keyword k)
	    (push type k-t)))
	(setf values (third ftype))
	(cond ((atom values) (setf values (list 'VALUES values)))
	      ((and (listp values) (eql (first values) 'VALUES)))
	      (t (ftype-error)))
	(when (and rest key-flag
		   (not (subtypep 'keyword rest)))
	  (ftype-error))
	(make-function-type :required requireds
			    :optional o
			    :rest rest
			    :key-p key-flag
			    :keywords k
			    :keyword-types k-t
			    :allow-other-keys-p allow-other-keys-p
			    :output (canonical-values-type values))))))

(defconstant +function-type-tag+ (cdr (assoc 'FUNCTION *elementary-types*)))

(defun register-function-type (type &aux ftype)
  (or (find-registered-tag type)
      (find-registered-tag (setq ftype (canonical-function-type type)))
      (let ((tag (register-type ftype #'function-type-p #'function-type-<=)))
	(update-types +function-type-tag+ tag)
	tag)))

(defun function-type-<= (f1 f2)
  (unless (and (every* #'subtypep
		       (function-type-required f2)
		       (function-type-required f1))
	       (do* ((o1 (function-type-optional f1) (cdr o1))
		     (o2 (function-type-optional f2) (cdr o2))
		     (r1 (function-type-rest f1))
		     (r2 (function-type-rest f2))
		     t1 t2)
		    ((and (endp o1) (endp o2)) t)
		 (setf t1 (cond ((consp o1) (first o1))
				(r1 r1)
				(t (return nil)))
		       t2 (cond ((consp o2) (first o2))
				(r2 r2)
				(t (return nil))))
		 (unless (subtypep t1 t2)
		   (return nil)))
	       (subtypep (function-type-output f1)
			 (function-type-output f2))
	       (eql (function-type-key-p f1) (function-type-key-p f2))
	       (or (function-type-allow-other-keys-p f2)
		   (not (function-type-allow-other-keys-p f1))))
    (return-from function-type-<= nil))
  (do* ((k2 (function-type-keywords f2))
	(k-t2 (function-type-keyword-types f2))
	(k1 (function-type-keywords f1) (cdr k1))
	(k-t1 (function-type-keyword-types f1) (cdr k1)))
       ((endp k1)
	t)
    (unless
	(let* ((n (position (first k1) k2)))
	  (when n
	    (let ((t2 (nth n k-t2)))
	      (subtypep (first k-t1) t2))))
      (return-from function-type-<= nil))))

;;----------------------------------------------------------------------
;; (VALUES ...) type

(defstruct values-type
  min-values
  max-values
  required
  optional
  rest)

(defun register-values-type (vtype)
  (or (find-registered-tag vtype)
      (find-registered-tag (setf vtype (canonical-values-type vtype)))
      (register-type vtype #'values-type-p #'values-type-<=)))

(defun canonical-values-type (vtype)
  (when (values-type-p vtype)
    (return-from canonical-values-type vtype))
  (flet ((vtype-error ()
	   (error "Syntax error in VALUES type definition ~S" vtype))) ;; Should this be a cmperr instead? JCB
    (unless (and (listp vtype) (eql (pop vtype) 'VALUES))
      (vtype-error))
    (let ((required '())
	  (optional '())
	  (rest nil))
      (do ()
	  ((endp vtype)
	   (make-values-type :min-values (length required)
			     :max-values (if rest multiple-values-limit
					     (+ (length required)
						(length optional)))
			     :required (nreverse required)
			     :optional (nreverse optional)
			     :rest rest))

	(let ((type (pop vtype)))
	  (if (eql type '&optional)
	      (do ()
		  ((endp vtype))
		(let ((type (pop vtype)))
		  (if (eql type '&rest)
		      (if (endp vtype)
			  (ftype-error)
			  (setf rest (first vtype)))
		      (push type optional))))
	      (push type required)))))))

(defun values-type-<= (v1 v2)
  (and (= (values-type-min-values v1) (values-type-min-values v2))
       (= (values-type-max-values v1) (values-type-max-values v2))
       (every* #'subtypep (values-type-required v1) (values-type-required v2))
       (every* #'subtypep (values-type-optional v1) (values-type-optional v2))
       (subtypep (values-type-rest v1) (values-type-rest v2))))

