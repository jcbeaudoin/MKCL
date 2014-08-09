;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: CLOS -*-
;;;;
;;;;  Copyright (c) 1992, Giuseppe Attardi.
;;;;  Copyright (c) 2010, Jean-Claude Beaudoin.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Lesser General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 3 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../../Copyright' for full details.

(in-package "CLOS")

;;; The mechanism for updating classes.

;;; ----------------------------------------------------------------------
;;; INSTANCE UPDATE PROTOCOL
;;;
;;;
;;; PART 1: CHANGING THE CLASS OF AN INSTANCE
;;;
;;; The method CHANGE-CLASS performs most of the work.
;;;
;;;	a) The structure of the instance is changed to match the new
;;;	   number of local slots.
;;;	b) The new local slots are filled with the value of the old
;;;	   slots. Only the name is used, so that a new local slot may
;;;	   get the value of old slots that were eithe local or shared.
;;;	c) Finally, UPDATE-INSTANCE-FOR-DIFFERENT-CLASS is invoked
;;;	   with a copy of the instance as it looked before the change,
;;;	   the changed instance and enough information to perform any
;;;	   extra processing.
;;;

(defmethod update-instance-for-different-class
    ((old-data standard-object) (new-data standard-object) &rest initargs)
    ;;(declare (dynamic-extent initargs))
  (let ((old-local-slotds (si::instance-sig old-data))
	(new-local-slotds (remove :instance (si::instance-sig new-data)
				  :test-not #'eq :key #'slot-definition-allocation))
	added-slots)
    (setf added-slots (set-difference (mapcar #'slot-definition-name new-local-slotds)
				      (mapcar #'slot-definition-name old-local-slotds)))
  (multiple-value-bind (method-initargs allow-other-keys)
      (valid-keywords-from-methods (append (compute-applicable-methods
					    #'update-instance-for-different-class
					    (list old-data new-data))
					   (compute-applicable-methods
					    #'shared-initialize (list new-data added-slots))))
      (unless allow-other-keys
	(check-initargs (class-of new-data) initargs method-initargs)))
    (apply #'shared-initialize new-data added-slots initargs)))

(defmethod change-class ((instance standard-object) (new-class standard-class)
			 &rest initargs)
  ;;(declare (dynamic-extent initargs))
  (let* ((old-instance (si::copy-instance instance))
	 (new-size (class-size new-class))
	 (instance (si::allocate-raw-instance instance new-class new-size)))
    (declare (type standard-object instance))
    (si::instance-sig-set instance)
    ;; "The values of local slots specified by both the class Cto and
    ;; Cfrom are retained.  If such a local slot was unbound, it remains
    ;; unbound."
    ;; "The values of slots specified as shared in the class Cfrom and
    ;; as local in the class Cto are retained."
    (let* ((old-local-slotds (class-slots (class-of old-instance)))
	   (new-local-slotds (class-slots (class-of instance))))
      (declare (ignore old-local-slotds))
      (dolist (new-slot new-local-slotds)
	;; CHANGE-CLASS can only operate on the value of local slots.
	(when (eq (slot-definition-allocation new-slot) :INSTANCE)
	  (let ((name (slot-definition-name new-slot)))
	    (if (and (slot-exists-p old-instance name)
		     (slot-boundp old-instance name))
		(setf (slot-value instance name) (slot-value old-instance name))
		(slot-makunbound instance name))))))
    (apply #'update-instance-for-different-class old-instance instance initargs)
    instance))

(defmethod change-class ((instance class) new-class &rest initargs)
  (declare (ignore new-class initargs))
  (if (forward-referenced-class-p instance)
      (call-next-method)
      (error "The metaclass of a class metaobject cannot be changed.")))

;;;
;;; PART 2: UPDATING AN INSTANCE THAT BECAME OBSOLETE
;;;
;;; Each instance has a hidden field (readable with SI::INSTANCE-SIG), which
;;; contains the list of slots of its class. This field is updated every time
;;; the class is initialized or reinitialized. Generally
;;;	(EQ (SI::INSTANCE-SIG x) (CLASS-SLOTS (CLASS-OF x)))
;;; returns NIL whenever the class became obsolete.
;;;
;;; There are two circumstances under which a instance may become obsolete:
;;; either the class has been modified using REDEFINE-INSTANCE (and thus the
;;; list of slots changed), or MAKE-INSTANCES-OBSOLETE has been used.
;;;
;;; The function UPDATE-INSTANCE (hidden to the user) does the job of
;;; updating an instance that has become obsolete.
;;;
;;;	a) A copy of the instance is saved to check the old values.
;;;	b) The structure of the instance is changed to match the new
;;;	   number of local slots.
;;;	c) The new local slots are filled with the value of the old
;;;	   local slots.
;;;	d) Finally, UPDATE-INSTANCE-FOR-REDEFINED-CLASS is invoked
;;;	   with enough information to perform any extra initialization,
;;;	   for instance of new slots.
;;;
;;; It is not clear when the function UPDATE-INSTANCE is invoked. At least
;;; this will happen whenever the functions SLOT-VALUE, (SETF SLOT-VALUE),
;;; SLOT-BOUNDP or SLOT-EXISTS-P are used.
;;;

(defmethod update-instance-for-redefined-class
    ((instance standard-object) added-slots discarded-slots property-list
     &rest initargs)
    ;;(declare (dynamic-extent initargs))
  (multiple-value-bind (method-initargs allow-other-keys)
      (valid-keywords-from-methods (append (compute-applicable-methods
					    #'update-instance-for-redefined-class
					    (list instance added-slots discarded-slots property-list))
					   (compute-applicable-methods
					    #'shared-initialize
					    (list instance added-slots))))
      (unless allow-other-keys
	(check-initargs (class-of instance) initargs method-initargs)))
  (apply #'shared-initialize instance added-slots initargs))

(defun update-instance (instance)
  (declare (type standard-object instance))
  (let* ((class (class-of instance))
	 (old-slotds (si::instance-sig instance))
	 (new-slotds (class-slots class))
	 (old-instance (si::copy-instance instance))
	 (discarded-slots '())
	 (added-slots '())
	 (property-list '()))
    (unless (equal old-slotds new-slotds)
      (setf instance (si::allocate-raw-instance instance class (class-size class)))
      (si::instance-sig-set instance)
      (let* ((new-i 0)
	     (old-local-slotds (remove :instance old-slotds :test-not #'eq
				       :key #'slot-definition-allocation))
	     (new-local-slotds (remove :instance new-slotds :test-not #'eq
				       :key #'slot-definition-allocation)))
	(declare (fixnum new-i))
	(setq discarded-slots
	      (set-difference (mapcar #'slot-definition-name old-local-slotds)
			      (mapcar #'slot-definition-name new-local-slotds)))
	(dolist (slot-name discarded-slots)
	  (let* ((ndx (position slot-name old-local-slotds :key #'slot-definition-name)))
	    ;; (push (cons slot-name (si::instance-ref old-instance ndx))
	    ;; 	  property-list) ;; This is building an a-list! JCB
	    (setq property-list (list* slot-name (si::instance-ref old-instance ndx) property-list)) ;; This builds a p-list.
	    ))
	(dolist (new-slot new-local-slotds)
	  (let* ((name (slot-definition-name new-slot))
		 (old-i (position name old-local-slotds :key #'slot-definition-name)))
	    (if old-i
		(si::instance-set instance new-i
				  (si::instance-ref old-instance old-i))
		(push name added-slots))
	    (incf new-i))))
      (update-instance-for-redefined-class instance added-slots
					   discarded-slots property-list))))

;;; ----------------------------------------------------------------------
;;; CLASS REDEFINITION PROTOCOL

(defun unfinalize-class (class)
  (setf (class-finalized-p class) nil)
  (dolist (subclass (class-direct-subclasses class))
    (unfinalize-class subclass)))

(ensure-generic-function 'reinitialize-instance
			 :lambda-list '(class &rest initargs &key &allow-other-keys))

(defmethod reinitialize-instance ((class class) &rest initargs
				  &key
                                  (direct-superclasses nil direct-superclasses-p)
                                  (direct-slots nil direct-slots-p)
                                  &allow-other-keys)
  (declare (ignore initargs))
  (let ((name (class-name class)))
    (when (member name '(CLASS BUILT-IN-CLASS) :test #'eq) ;; Too weak! This restriction should be a lot broader. JCB
      (error "The kernel CLOS class ~S cannot be changed." name)))

  (let ((was-already-finalized-p (class-finalized-p class))
        (checked-direct-superclasses (when direct-superclasses-p
                                       ;; checking that class direct hierarchy makes sense.
                                       (check-direct-superclasses class direct-superclasses))))
    (when was-already-finalized-p
      (unfinalize-class class))      

    (when direct-superclasses-p
      ;; Remove old subclass relationships since direct class hierarchy is about to change.
      (dolist (l (class-direct-superclasses class))
        (remove-direct-subclass l class)))

    (call-next-method)

    ;; the list of direct slots is converted to direct-slot-definitions
    (when direct-slots-p
      (setf (class-direct-slots class)
            (loop for s in direct-slots
                  collect (canonical-slot-to-direct-slot class s))))

    ;; assure coherence of class hierarchy.
    (when direct-superclasses-p
      ;; Setup new subclass relationships since direct class hierarchy has changed.    
      (setf (class-direct-superclasses class) checked-direct-superclasses)
      (dolist (l checked-direct-superclasses)
        (add-direct-subclass l class)))

    #+(and) ;; if there are no forward references, we can just finalize the class here
    (finalize-unless-forward class)  ;; somewhat eager. JCB
    #-(and) ;; This here should replace the line just above but there is something foobar with our finalization. JCB
    (when was-already-finalized-p
      (finalize-inheritance class)
      ;; Here AMOP says we should also update dependents, but that protocol isn't implemented yet. JCB
      )
    )
  class)

(defmethod make-instances-obsolete ((class class))
  (setf (class-slots class) (copy-list (class-slots class)))
  class)

