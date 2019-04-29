;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: C -*-
;;;;
;;;;  CMPCLOS. CLOS related optimizations.

;;;;  Copyright (c) 2008. Juan Jose Garcia-Ripol
;;;;  Copyright (c) 2014, Jean-Claude Beaudoin.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Lesser General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 3 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../../Copyright' for full details.

(in-package "COMPILER")

;;;
;;; GENERIC OPTIMIZATION
;;;

(defun maybe-optimize-generic-function (fname args)
  (declare (ignorable fname args))

  #+(and)
  nil ;; simply return nil to say that the optimization could not be done. JCB

  ;; There is doubts whether these optimizations really work. Let's err on the safe side. JCB
  ;; There isn't much of a doubt anymore. It's wrong. JCB
  #-(and)
  (when (fboundp fname)
    (let ((gf (fdefinition fname)))
      (when (typep gf 'standard-generic-function)
	(when (policy-inline-slot-access-p)
	  (maybe-optimize-slot-accessor fname gf args)))))
  )

#|  ;; This stuff is just wrong! JCB

;;;
;;; PRECOMPUTE APPLICABLE METHODS
;;;
;;; Computes a list of methods that would apply given what we know
;;; about their arguments. Since the types are not exact, we have to
;;; use subtypep.  We could speed this up if we could precompute the
;;; classes for the c-args.
;;;

(defun precompute-applicable-methods (methods c-args)
  (flet ((applicable-method-p (m)
	   (loop for specializer in (clos::method-specializers m)
	      for arg in c-args
	      always (let ((arg-type (c1form-type arg)))
		       (subtypep arg-type (if (consp specializer)
					      `(member ,(second specializer))
					      specializer))))))
    (delete-if-not #'applicable-method-p methods)))

;;;
;;; SLOT ACCESSORS
;;;
;;; The following functions deal with an MKCL extension, which are
;;; sealed slots.  These slots have a fixed location which is
;;; inherited by subclasses. They normally appear when you add the
;;; option (:sealedp t) to a class definition.
;;;
;;; When MKCL detects that you call an accessor to such a slot, it can
;;; optimize the operation, using a direct access based on the
;;; position of the slot. This optimization is only active when the
;;; safety levels are low, because it prevents you from changing the
;;; class hierarchy.
;;;

(defun find-slot-accessors (gf)
  (loop for method in (clos::generic-function-methods gf)
     with readers = '()
     with writers = '()
     with reader-class = (find-class 'clos::standard-reader-method)
     with writer-class = (find-class 'clos::standard-writer-method)
     do (let ((method-class (class-of method)))
	  (cond ((si::subclassp method-class reader-class)
		 (push method readers))
		((si::subclassp method-class writer-class)
		 (push method writers))))
     finally (return (values readers writers))))

(defun maybe-optimize-slot-accessor (fname gf args)
  (multiple-value-bind (readers writers)
      (find-slot-accessors gf)
    (cond ((and readers writers)
	   (cmpwarn-style "When analyzing generic function ~A found both slot reader and writer methods" fname))
	  ((not (or readers writers))
	   nil)
	  ((/= (length args) (length (clos::generic-function-spec-list gf)))
	   (cmpwarn-style "Too many arguments for generic function ~A" fname)
	   nil)
	  (readers
	   (try-optimize-slot-reader readers args))
	  (writers
	   (try-optimize-slot-writer writers args)))))

(defun try-optimize-slot-reader (readers args)
  (let* ((object (first args))
	 (c-object (c1expr object))
	 (readers (precompute-applicable-methods readers (list c-object))))
    (when (= (length readers) 1)
      (let ((reader (first readers)))
	(when (typep reader 'clos::standard-reader-method)
	  (let* ((slotd (clos::accessor-method-slot-definition reader))
		 (index (clos::safe-slot-definition-location slotd)))
	    (when (clos::accessor-method-class-sealedp reader)
	      (c1expr `(clos::safe-instance-ref (ensure-up-to-date-instance ,object) ,index)))))))))

(defun try-optimize-slot-writer (orig-writers args)
  (let* ((c-args (mapcar #'c1expr args))
	 (writers (precompute-applicable-methods orig-writers c-args)))
    (when (= (length writers) 1)
      (let ((writer (first writers)))
	(when (typep writer 'clos::standard-writer-method)
	  (let* ((slotd (clos::accessor-method-slot-definition writer))
		 (index (clos::safe-slot-definition-location slotd)))
	    (when (clos::accessor-method-class-sealedp writer)
	      (c1expr `(si::instance-set (ensure-up-to-date-instance ,(second args)) ,index ,(first args))))))))))

|#

