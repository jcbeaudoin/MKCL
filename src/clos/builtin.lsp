;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: CLOS -*-
;;;;
;;;;  Copyright (c) 1992, Giuseppe Attardi.
;;;;  Copyright (c) 2012, Jean-Claude Beaudoin.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Lesser General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 3 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../../Copyright' for full details.

(in-package "CLOS")

;;; ----------------------------------------------------------------------
;;; Methods

;;; ======================================================================
;;; Built-in classes
;;; ----------------------------------------------------------------------
;;;
;;; IMPORTANT!
;;; This class did not exist until now. This was no problem, because it is
;;; not used anywhere in MKCL. However, we have to define and we have to
;;; ensure that "T" becomes an instance of BUILT-IN-CLASS.

;;; We have to build the class manually, because
;;;	(ENSURE-CLASS-USING-CLASS NIL ...)
;;; does not work yet, since the class NULL does not exist.
;;;

;; Create class built-in-class.
(setf (find-class 'built-in-class)
      (make-instance (find-class 'standard-class)
		     :name 'built-in-class
		     :direct-superclasses (list (find-class 'class))
		     :direct-slots nil))

;; Change class of class T to built-in-class
(let ((the-t (find-class T)))
  (si:instance-class-set the-t (find-class 'built-in-class))
  (si::instance-sig-set the-t))

(defmethod make-instance ((class built-in-class) &rest initargs)
  (declare (ignore initargs))
  (error "The built-in class (~A) cannot be instantiated" class))

(eval-when (:compile-toplevel :execute)
  (defconstant +builtin-classes+  ;; if you modify this also adjust c/instance.d
	 '(;(t object)
	    (sequence)
	      (list sequence)
	        (cons list)
	    (array)
	      (vector array sequence)
	        (string vector)
                #+unicode
	        (base-string string vector)
	        (bit-vector vector)
	    (stream)
	      (ansi-stream stream)
		(file-stream ansi-stream)
		(echo-stream ansi-stream)
		(string-stream ansi-stream)
		(two-way-stream ansi-stream)
		(synonym-stream ansi-stream)
		(broadcast-stream ansi-stream)
		(concatenated-stream ansi-stream)
	    (character)
	    (number)
	      (real number)
	        (rational real)
		  (integer rational)
		  (ratio rational)
	        (float real)
	      (complex number)
	    (symbol)
	      (null symbol list)
	      (keyword symbol)
	    (method-combination)
	    (package)
	    (function)
	    (pathname)
	      (logical-pathname pathname)
	    (hash-table)
	    (random-state)
	    (readtable)
            (si::code-block)
	    (si::foreign)
	    (si::temp-stack-frame)
	    (si::compiled-closure-display)
	    (si::compiled-closure-level)
	    (si::compiled-debug-information)
	    (mt::thread)
	    (mt::lock)
	    (mt::rwlock)
	    (mt::semaphore)
	    (mt::condition-variable)
	    (mkcl:process)
	    (si::encoded-string)
	      (si::utf-8 si::encoded-string)
	      (si::utf-16 si::encoded-string))))

(loop for (name . rest) in '#.+builtin-classes+
   with index = 1
   with built-in-class = (find-class 'built-in-class)
   with array = (setf *builtin-classes* (make-array #.(1+ (length +builtin-classes+))
						    :initial-element (find-class 't)))
   do (let* ((direct-superclasses (mapcar #'find-class (or rest '(t))))
	     (class (make-instance built-in-class :name name
				   :direct-superclasses direct-superclasses
				   :direct-slots nil)))
	(setf (find-class name) class
	      (aref array index) class
	      index (1+ index))))

;;; Now that class NULL exists we can define this:
(defmethod ensure-class-using-class ((class null) name &rest rest &key &allow-other-keys)
  (multiple-value-bind (metaclass direct-superclasses options)
      (apply #'help-ensure-class rest)
    (declare (ignore direct-superclasses))
    (apply #'make-instance metaclass :name name options)))


(defmethod change-class ((instance t) (new-class symbol) &rest initargs)
  (apply #'change-class instance (find-class new-class) initargs))

(defmethod make-instances-obsolete ((class symbol))
  (make-instances-obsolete (find-class class))
  class)

(defmethod make-instance ((class-name symbol) &rest initargs)
  (declare (dynamic-extent initargs))
  (apply #'make-instance (find-class class-name) initargs))

(defmethod slot-makunbound-using-class ((class built-in-class) self slotd)
  (declare (ignore self slotd))
  (error "SLOT-MAKUNBOUND-USING-CLASS cannot be applied on built-in objects"))

(defmethod slot-boundp-using-class ((class built-in-class) self slotd)
  (declare (ignore self slotd))
  (error "SLOT-BOUNDP-USING-CLASS cannot be applied on built-in objects"))

(defmethod slot-value-using-class ((class built-in-class) self slotd)
  (declare (ignore self slotd))
  (error "SLOT-VALUE-USING-CLASS cannot be applied on built-in objects"))

(defmethod (setf slot-value-using-class) (val (class built-in-class) self slotd)
  (declare (ignore val self slotd))
  (error "SLOT-VALUE-USING-CLASS cannot be applied on built-in objects"))

(defmethod slot-exists-p-using-class ((class built-in-class) self slotd)
  (declare (ignore self slotd))
  nil)

;;; ======================================================================
;;; STRUCTURES
;;;

(defclass structure-class (class)
  (slot-descriptions
   initial-offset
   defstruct-form
   constructors
   documentation
   copier
   predicate
   print-function))

;;; structure-classes cannot be instantiated
(defmethod make-instance ((class structure-class) &rest initargs)
  (declare (ignore initargs))
  (error "The structure-class (~A) cannot be instantiated" class))

(defmethod finalize-inheritance ((class structure-class))
  (call-next-method)
  (dolist (slot (class-slots class))
    (unless (eq :INSTANCE (slot-definition-allocation slot))
      (error "The structure class ~S cannot have shared slots" (class-name class)))))

;;; ----------------------------------------------------------------------
;;; Structure-object
;;;
;;; Structure-object has no slots and inherits only from t:

(defclass structure-object (t) ()
  (:metaclass structure-class))

(defmethod make-load-form ((object structure-object) &optional environment)
  (declare (ignore environment))
  (make-load-form-saving-slots object))

(defun print-object-as-struct (obj stream)
  (ensure-up-to-date-instance obj) ;; let's make sure the object length is up-to-date.
  (let* ((class (si:instance-class obj))
	 (slotds (class-slots class))
	 (obj-length (si:instance-length obj)))
    (declare (:read-only class))
    (when (and slotds
	       *print-level*
	       ;; *p-readably* effectively disables *p-level*
	       (not *print-readably*)
	       (zerop *print-level*))
      (write-string "#" stream)
      (return-from print-object-as-struct obj))
    (write-string "#S(" stream)
    (ignore-errors (prin1 (class-name class) stream))
    (do ((scan slotds (cdr scan))
	 (i 0 (1+ i))
	 (limit (or *print-length* most-positive-fixnum))
	 (sv))
	((null scan) (when (< i obj-length) (write-string " +++" stream)))
      (declare (fixnum i))
      (when (>= i limit)
	(write-string " ..." stream)
	(return))
      (when (>= i obj-length)
	(write-string " ---" stream)
	(return))
      (setq sv (si:instance-ref obj i))
      (write-string " :" stream)
      (ignore-errors (prin1 (slot-definition-name (car scan)) stream))
      (write-string " " stream)
      ;;(prin1 sv stream)
      (if (si:sl-boundp sv)
	  (if (si:unbound-value-p sv)
	      (prin1 "Unbound value" stream)
	    (ignore-errors (prin1 sv stream)))
	(prin1 "Unbound slot" stream))
      )
    (write-string ")" stream)
    obj))

(defmethod print-object ((obj structure-object) stream)
  (print-object-as-struct obj stream)
  )

