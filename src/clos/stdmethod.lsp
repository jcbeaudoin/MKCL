;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: CLOS -*-
;;;;
;;;;  Copyright (c) 1992, Giuseppe Attardi.
;;;;  Copyright (c) 2010-2017, Jean-Claude Beaudoin.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Lesser General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 3 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../../Copyright' for full details.

(in-package "CLOS")


;;; ----------------------------------------------------------------------
;;; Generic Functions
;;; ----------------------------------------------------------------------

(defclass generic-function (function standard-object) ())

(defclass standard-generic-function (generic-function)
  #.+standard-generic-function-slots+)

;;;----------------------------------------------------------------------
;;; Method
;;; ----------------------------------------------------------------------

(defclass method () ())

(defclass standard-method (method)
  #.+standard-method-slots+)

(defmethod migrate-method ((method standard-method) old-class new-class)
  (let ((name (generic-function-name (method-generic-function method)))
	(qualif (method-qualifiers method))
	(spec (substitute new-class old-class (method-specializers method))))
    (multiple-value-call
     #'install-method 
     name
     qualif
     spec
     (method-lambda-list method)
     (method-documentation method)
     (method-plist method)
     (values-list (si::clone-closure (method-function method) (method-fun-context-setter method)))
     nil			  ;; method-class
     :source (method-source method) ;; options, a &rest p-list.
     )
    )
  )


(defmethod function-keywords ((method standard-method))
  (multiple-value-bind (reqs nb_reqs opts nb_opts rest-var key-flag keywords nb_keys allow-other-keys)
      (si::process-lambda-list (method-lambda-list method) 'function)
    (declare (ignore reqs nb_reqs opts nb_opts rest-var nb_keys))
    (if key-flag
        (do* ((output '())
              (l keywords (cddddr l)))
            ((endp l)
             (values (nreverse output) allow-other-keys))
          (push (first l) output))
      (values nil nil))))


(eval-when (compile eval)
  (defparameter +standard-accessor-method-slots+
    (append +standard-method-slots+
	    '(
	      (slot-definition :initarg :slot-definition :initform nil
	       :accessor accessor-method-slot-definition)
#|
	      (class-sealedp :initarg :class-sealedp :initform nil
	       :accessor accessor-method-class-sealedp)
|#
	      ))))

#.(create-accessors +standard-accessor-method-slots+ 'standard-accessor-method)

(defclass standard-accessor-method (standard-method)
  #.+standard-accessor-method-slots+)


(defclass standard-reader-method (standard-accessor-method) ())

(defclass standard-writer-method (standard-accessor-method) ())


(defmethod migrate-method ((method standard-accessor-method) old-class new-class)
  (declare (ignore old-class new-class))
  ;; This method is empty becase standard-accessor-methods are
  ;; automatically generated upon class creation.
  )

