;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: CLOS -*-
;;;;
;;;;  Copyright (c) 1992, Giuseppe Attardi.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Lesser General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 3 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../../Copyright' for full details.

(in-package "CLOS")

;;; ----------------------------------------------------------------------
;;; Building the classes T, CLASS, STANDARD-OBJECT and STANDARD-CLASS.
;;;
;;; We cannot use the functions CREATE-STANDARD-CLASS and others because SLOTS,
;;; DIRECT-SLOTS, etc are empty and therefore SLOT-VALUE does not work.

(defun make-empty-standard-class (name metaclass)
  (let ((class (si:allocate-raw-instance nil metaclass #.(length +standard-class-slots+))))
    (unless metaclass
      (si:instance-class-set class class))
    (setf (class-id                  class) name
	  (class-direct-superclasses class) nil
	  (class-direct-subclasses   class) nil
	  (class-slots               class) nil
	  (class-size                class) 0
	  (class-direct-slots        class) nil
	  (class-direct-default-initargs class) nil
	  (class-default-initargs    class) nil
	  (class-precedence-list     class) nil
	  (class-finalized-p         class) nil
	  (class-cached-make-instance class) nil
	  (class-sealedp             class) nil
	  (class-documentation       class) nil
	  (class-version             class) 0
	  (class-previous            class) nil
	  (class-source              class) nil
	  (class-spec-users          class) nil
	  (find-class name) class)
    (unless (eq name 'T)
      (setf (slot-table class) (make-hash-table :size 2)))
    (setf (class-optimize-slot-access class) nil)
    class))

;; 1) Create the classes
;;
;; Notice that, due to circularity in the definition, STANDARD-CLASS has
;; itself as metaclass. MAKE-EMPTY-CLASS takes care of that.
;;
(let* ((standard-class (make-empty-standard-class 'STANDARD-CLASS nil))
       (standard-object (make-empty-standard-class 'STANDARD-OBJECT standard-class))
       (forward-referenced-class
	(make-empty-standard-class 'FORWARD-REFERENCED-CLASS standard-class))
       (the-class (make-empty-standard-class 'CLASS standard-class))
       (the-t (make-empty-standard-class 'T the-class))
       ;; It does not matter that we pass NIL instead of a class object,
       ;; because CANONICAL-SLOT-TO-DIRECT-SLOT will make simple slots.
       (class-slots (loop for s in (parse-slots '#.+class-slots+)
			  collect (canonical-slot-to-direct-slot nil s)))
       (standard-slots (loop for s in (parse-slots '#.+standard-class-slots+)
			     collect (canonical-slot-to-direct-slot nil s)))
       (hash-table (make-hash-table :size 24)))

  ;; 2) STANDARD-CLASS and CLASS are the only classes with slots. Create a
  ;; hash table for them, so that SLOT-VALUE works. Notice that we
  ;; make a intentional mistake: CLASS and STANDARD-CLASS share the same
  ;; hashtable!!
  (do* ((i 0 (1+ i))
	(slots standard-slots (cdr slots)))
       ((endp slots))
    (let ((slotd (first slots)))
      (setf (slot-definition-location slotd) i)
      (setf (gethash (slot-definition-name slotd) hash-table) slotd)))
  (dolist (slotd class-slots)
    (setf (slot-definition-location slotd)
	  (slot-definition-location (gethash (slot-definition-name slotd) hash-table))))
  (setf (class-slots               the-class) (copy-list class-slots)
	(class-size                the-class) (length class-slots)
	(slot-table                the-class) hash-table
	(class-direct-slots        the-class) class-slots
	(class-slots               standard-class) standard-slots
	(class-size                standard-class) (length standard-slots)
	(slot-table                standard-class) hash-table
	(class-direct-slots        standard-class) (set-difference standard-slots class-slots))
  (setf (class-slots        forward-referenced-class) (copy-list class-slots)
	(class-size         forward-referenced-class) (length class-slots)
	(slot-table         forward-referenced-class) hash-table
	(class-direct-slots forward-referenced-class) (copy-list class-slots))

  ;; 3) Fix the class hierarchy
  (setf (class-direct-superclasses the-t) nil
	(class-direct-subclasses the-t) (list standard-object)
	(class-direct-superclasses standard-object) (list the-t)
	(class-direct-subclasses standard-object) (list the-class)
	(class-direct-superclasses the-class) (list standard-object)
	(class-direct-subclasses the-class) (list standard-class)
	(class-direct-superclasses standard-class) (list the-class))
  (setf (class-direct-superclasses forward-referenced-class) (list the-class))
  (push forward-referenced-class (class-direct-subclasses the-class))

  (si::instance-sig-set the-class)
  (si::instance-sig-set standard-class)
  (si::instance-sig-set standard-object)
  (si::instance-sig-set the-t)
  (si::instance-sig-set forward-referenced-class)

  ;; 4) Fix the class precedence list
  (let ((cpl (list standard-class the-class standard-object the-t)))
    (setf (class-precedence-list standard-class) cpl
	  (class-precedence-list the-class) (cdr cpl)
	  (class-precedence-list standard-object) (cddr cpl)
	  (class-precedence-list the-t) (cdddr cpl))
    (setf (class-precedence-list forward-referenced-class)
	  (list* forward-referenced-class (cdr cpl))))

  (setf (class-finalized-p the-t) t
	(class-finalized-p standard-object) t
	(class-finalized-p the-class) t
	(class-finalized-p standard-class) t)
  (setf (class-finalized-p forward-referenced-class) t)

  ;; 5) Generate accessors (In macros.lsp)
)

(defconstant +the-standard-class+ (find-class 'standard nil))

(defmethod class-prototype ((class class))
  (unless (slot-boundp class 'prototype)
    (setf (slot-value class 'prototype) (allocate-instance class)))
  (slot-value class 'prototype)
  )

;;; ----------------------------------------------------------------------
;;; SLOTS READING AND WRITING
;;;

;;;
;;; 3) Error messages related to slot access
;;;

(defmethod slot-missing ((class t) object slot-name operation 
			 &optional new-value)
  (declare (ignore operation new-value))
  (error "~A is not a slot of ~A" slot-name object))

(defmethod slot-unbound ((class t) object slot-name)
  (error 'unbound-slot :instance object :name slot-name))

;;;
;;; 2) Overloadable methods on which the previous functions are based
;;;

(defun standard-instance-get (instance slotd)
  (ensure-up-to-date-instance instance)
  (let* (;;(class (si:instance-class instance))
	 (location (slot-definition-location slotd)))
    (cond ((mkcl:fixnump location)
	   ;; local slot
	   (if (< location (si:instance-length instance))
	       (si:instance-ref instance (the fixnum location))
	     (si:unbound))) ;; should we do this or rather let an exception be raised? JCB
	  ((consp location)
	   ;; shared slot
	   (car location))
	  (t
 	   (error "Effective slot definition lacks a valid location:~%~A" slotd)))))

(defun standard-instance-set (val instance slotd)
  (ensure-up-to-date-instance instance)
  (let* (;;(class (si:instance-class instance))
	 (location (slot-definition-location slotd)))
    (cond ((mkcl:fixnump location)
	   ;; local slot
	   (si:instance-set instance (the fixnum location) val))
	  ((consp location)
	   ;; shared slot
	   (setf (car location) val))
	  (t
	   (error "Effective slot definition lacks a valid location:~%~A" slotd)))
    val))

(defmethod slot-value-using-class ((class class) self slotd)
  (let ((value (standard-instance-get self slotd)))
    (if (si:sl-boundp value)
	value
	(values (slot-unbound class self (slot-definition-name slotd))))))

(defmethod slot-boundp-using-class ((class class) self slotd)
  (si::sl-boundp (standard-instance-get self slotd)))

(defmethod (setf slot-value-using-class) (val (class class) self slotd)
  (standard-instance-set val self slotd))

(defmethod slot-makunbound-using-class ((class class) instance slotd)
  (ensure-up-to-date-instance instance)
  (let* ((location (slot-definition-location slotd)))
    (cond ((mkcl:fixnump location)
	   ;; local slot
	   (si:sl-makunbound instance (the fixnum location)))
	  ((consp location)
	   ;; shared slot
	   (setf (car location) (unbound)))
	  (t
	   (error "Effective slot definition lacks a valid location:~%~A" slotd))))
  instance)

;;;
;;; 1) Functional interface
;;;

(defun find-slot-definition (class slot-name)
  (if (eq (si:instance-class class) +the-standard-class+)
      (gethash (class-slot-table class) slot-name nil)
      (find slot-name (class-slots class) :key #'slot-definition-name))) ;; linear search.

(defun slot-value (self slot-name)
  (let* ((class (class-of self))
	 (slotd (find-slot-definition class slot-name)))
    (if slotd
	(slot-value-using-class class self slotd)
	(values (slot-missing class self slot-name 'SLOT-VALUE)))))

(defun slot-boundp (self slot-name)
  (let* ((class (class-of self))
	 (slotd (find-slot-definition class slot-name)))
    (if slotd
	(slot-boundp-using-class class self slotd)
	(values (slot-missing class self slot-name 'SLOT-BOUNDP)))))

(defun (setf slot-value) (value self slot-name)
  (let* ((class (class-of self))
	 (slotd (find-slot-definition class slot-name)))
    (if slotd
	(funcall #'(setf slot-value-using-class) value class self slotd)
	(slot-missing class self slot-name 'SETF value))
    value))

(defun slot-makunbound (self slot-name)
  (let* ((class (class-of self))
	 (slotd (find-slot-definition class slot-name)))
    (if slotd
	(slot-makunbound-using-class class self slotd)
	(slot-missing class self slot-name 'SLOT-MAKUNBOUND))
    self))

(defun slot-exists-p (self slot-name)
  (and (find-slot-definition (class-of self) slot-name)
       t))

;;;
;;; For the next accessor we define a method.
;;;

(defmethod class-name ((class class))
  (class-id class))

(defmethod (setf class-name) (new-value (class class))
  (setf (class-id class) new-value))
