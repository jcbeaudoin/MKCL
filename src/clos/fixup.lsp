;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: CLOS -*-
;;;;
;;;;  Copyright (c) 1992, Giuseppe Attardi.
;;;;  Copyright (c) 2010-2014, Jean-Claude Beaudoin.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Lesser General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 3 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../../Copyright' for full details.

(in-package "CLOS")

;;; ----------------------------------------------------------------------
;;;                                                                  slots


(defun no-primary-method (gf &rest args)
  (error "Generic function: ~A. No primary method given arguments: ~S"
	 (generic-function-name gf) (apply #'list (car args))))

(defun convert-one-class (class)
  (let* ((direct-slots (class-direct-slots class))
	 (effective-slots (class-slots class))
	 (new-direct-slots
	  (loop for x in direct-slots
		collect (if (consp x)
			    (apply #'make-instance 'standard-direct-slot-definition
				   (slot-definition-to-plist x))
			    x)))
	 (new-effective-slots
	  (loop for x in effective-slots
		collect (if (consp x)
			    (apply #'make-instance 'standard-effective-slot-definition
				   (slot-definition-to-plist x))
			    x))))
    (map-into direct-slots #'identity new-direct-slots)
    (map-into effective-slots #'identity new-effective-slots)
    (when (typep class 'standard-class)
      (std-create-slots-table class)
      (std-class-generate-accessors class) ;; JCB
      ))
  (mapc #'convert-one-class (class-direct-subclasses class)))

;;;
;;; We cannot redefine the class for slot definitions because this
;;; causes an infinite loop. Hence, we avoid evaluating the following
;;; forms at compile time.
;;;
(eval-when (:load-toplevel :execute)
(let ((si:*source-location* '#.(copy-list si:*source-location*)))
  (eval
   `(progn
      (defclass slot-definition ()
	,(mapcar #'(lambda (x) (butlast x 2)) +slot-definition-slots+)) ;; strip out :accessor! JCB
      (defclass standard-slot-definition (slot-definition) ())
      (defclass direct-slot-definition (slot-definition) ())
      (defclass effective-slot-definition (slot-definition) ())
      (defclass standard-direct-slot-definition (standard-slot-definition direct-slot-definition) ())
      (defclass standard-effective-slot-definition (standard-slot-definition effective-slot-definition) ())))
  (make-instances-obsolete (find-class 't))
  (convert-one-class (find-class 't))))


(defmethod reader-method-class ((class standard-class)
				(direct-slot direct-slot-definition)
				&rest initargs)
  (declare (ignore initargs))
  (find-class 'standard-reader-method))

(defmethod writer-method-class ((class standard-class)
				(direct-slot direct-slot-definition)
				&rest initargs)
  (declare (ignore initargs))
  (find-class 'standard-writer-method))

(defmethod reader-method-class ((class standard-class)
				(direct-slot standard-slot-definition)
				&rest initargs)
  (declare (ignore initargs))
  (find-class 'standard-reader-method))

(defmethod writer-method-class ((class standard-class)
				(direct-slot standard-slot-definition)
				&rest initargs)
  (declare (ignore initargs))
  (find-class 'standard-writer-method))

;;; ----------------------------------------------------------------------
;;; Fixup

(dolist (method-info *early-methods*)
  (let* ((method-name (car method-info))
	 (gfun (fdefinition method-name))
	 (standard-method-class (find-class 'standard-method)))
    (when (eq 'T (class-id (si:instance-class gfun)))
      ;; complete the generic function object
      (si:instance-class-set gfun (find-class 'STANDARD-GENERIC-FUNCTION))
      (si::instance-sig-set gfun)
      (setf (generic-function-method-class gfun) standard-method-class)
      (setf (slot-value gfun 'documentation) nil)
      )
    (dolist (method (cdr method-info))
      ;; complete the method object
      (let ((old-class (si::instance-class method)))
	(si::instance-class-set method
				(cond ((null old-class)
				       (find-class 'standard-method))
				      ((symbolp old-class)
				       (find-class old-class))
				      (t
				       old-class))))
      (si::instance-sig-set method)
      )
    )
  )

(makunbound '*EARLY-METHODS*)
;;; Because we just unbound *early-methods* now add-method is broken
;;; such that we cannot call defmethod until it gets fixed again somewhere down below here.
;;; No defmethod possible from here to down there.

;;; ----------------------------------------------------------------------
;;;                                                              redefined

(defun method-p (method) (typep method 'METHOD))

(defun make-method (method-class qualifiers specializers arglist doc
		    function fun-context-setter plist options)
  (apply #'make-instance
	 method-class
	 :generic-function nil
	 :qualifiers qualifiers
	 :lambda-list arglist
	 :specializers specializers
	 :function function
	 :fun-context-setter fun-context-setter
	 :documentation doc
	 :plist plist
	 :allow-other-keys t
	 options))



(defun congruent-lambda-lists-p (gf-ll method-ll)
  (multiple-value-bind (r1 nb_r1 opts1 nb_opts1 rest1 key-flag1 keywords1 nb_keys1 a-o-k1)
      (si::process-lambda-list gf-ll 'FUNCTION)
    (declare (ignore r1 opts1 nb_keys1 a-o-k1))
    (multiple-value-bind (r2 nb_r2 opts2 nb_opts2 rest2 key-flag2 keywords2 nb_keys2 a-o-k2)
	(si::process-lambda-list method-ll 'FUNCTION)
      (declare (ignore r2 opts2 nb_keys2))
      (and (eql nb_r2 nb_r1)
	   (eql nb_opts1 nb_opts2)
           (flet ((gf-keys-are-subset-of-method-keys-p (gf-key-specs m-key-specs)
                    (flet ((all-keywords (keyword-specs)
                             (do ((l keyword-specs (cddddr l))
                                  (all-keys '()))
                                 ((null l) all-keys)
                                 (push (first l) all-keys))))
                      (null (set-difference (all-keywords gf-key-specs) (all-keywords m-key-specs))))))
             (if key-flag1
                 (if key-flag2
                     (or a-o-k2 (gf-keys-are-subset-of-method-keys-p keywords1 keywords2)) ;; as per CLHS 7.6.4, item 4.
                   rest2 ;; &rest of method swallows gf keyword args
                   )
               (if (or rest1 rest2 key-flag2)
                   (and rest1 (or rest2 key-flag2)) ;; as per CLHS 7.6.4, item 3.
                 t) ;; there is no &rest nor &key anywhere in the lambda-lists, therefore that part is congruent.
               ))
	   t))))



(defun add-method (gf method)
  (when (generic-function-closed-p gf)
    (error "Cannot add the method ~A to the closed generic function ~A." method gf))
  ;;
  ;; 1) The method must not be already installed in another generic function.
  ;;
  (let ((other-gf (method-generic-function method)))
    (unless (or (null other-gf) (eq other-gf gf))
      (error "The method ~A belongs to the generic function ~A ~
               and cannot be added to ~A." method other-gf gf)))
  ;;
  ;; 2) The method and the generic function should have congruent lambda
  ;;    lists. That is, it should accept the same number of required and
  ;;    optional arguments, and only accept keyword arguments when the generic
  ;;    function does.
  ;;
  (let ((method-lambda-list (method-lambda-list method)))
    (if (slot-boundp gf 'lambda-list)
	(let ((gf-lambda-list (generic-function-lambda-list gf)))
	  (unless (congruent-lambda-lists-p gf-lambda-list method-lambda-list)
	    (error "Cannot add a method to generic function ~A because~%~
                     their respective lambda lists ~A and ~A are not congruent."
                   (generic-function-name gf) method-lambda-list gf-lambda-list)))
      (reinitialize-instance gf :lambda-list (convert-to-implicit-generic-function-lambda-list method-lambda-list))))
  ;;
  ;; 3) Finally, it is inserted in the list of methods, and the method is
  ;;    marked as belonging to a generic function.
  ;;
  (when (generic-function-methods gf)
    (let* ((method-qualifiers (method-qualifiers method)) 
	   (specializers (method-specializers method))
	   found)
      (when (setq found (find-method gf method-qualifiers specializers nil))
	(remove-method gf found))))
  ;;
  ;; We install the method by:
  ;;  i) Adding it to the list of methods
  (push method (generic-function-methods gf))
  (setf (method-generic-function method) gf)
  ;;  ii) Updating the specializers list of the generic function. Notice that
  ;;  we should call add-direct-method for each specializer but specializer
  ;;  objects are not yet implemented
  #+(or)
  (dolist (spec (method-specializers method))
    (add-direct-method spec method))
  ;;  iii) Computing a new discriminating function... Well, since the core
  ;;  MKCL does not need the discriminating function because we always use
  ;;  the same one, we just update the spec-how list of the generic function.
  (compute-g-f-spec-list gf)
  (maybe-clear-cached-make-instance gf method)
  (dolist (spec (method-specializers method))
    (register-method-as-spec-user spec method))
  gf)

;;; defmethod works again from this point on.

(setf (method-function (eval '(defmethod boot-add-method ((gf standard-generic-function)
							  (method standard-method)))))
      #'add-method)
(setf (fdefinition 'add-method) #'boot-add-method)
(setf (generic-function-name #'add-method) 'add-method)



(defmethod unregister-method-as-spec-user ((spec T) method) ;; empty default method.
  (declare (ignore method))
  )

(defmethod unregister-method-as-spec-user ((spec standard-class) method)
  (setf (class-spec-users spec) (delete method (class-spec-users spec))))

(defmethod unregister-method-as-spec-user ((spec forward-referenced-class) method)
  (setf (class-spec-users spec) (delete method (class-spec-users spec))))

(defun remove-method (gf method)
  (with-metadata-lock
  (when (generic-function-closed-p gf)
    (error "Cannot remove the method ~A to the closed generic function ~A." method gf))
   (maybe-clear-cached-make-instance gf method)
   (si:clear-gfun-cache gf)
   (dolist (spec (method-specializers method))
     (unregister-method-as-spec-user spec method))
   (setf (generic-function-methods gf)
	 (delete method (generic-function-methods gf))
	 (method-generic-function method) nil)
   gf))

(setf (method-function (eval '(defmethod boot-remove-method ((gf standard-generic-function)
							     (method standard-method)))))
      #'remove-method)
(setf (fdefinition 'remove-method) #'boot-remove-method)
(setf (generic-function-name #'remove-method) 'remove-method)

(setf (method-function (eval '(defmethod boot-find-method ((gf standard-generic-function)
							   method-qualifiers 
							   specializers
							   &optional (errorp t)))))
      #'find-method)
(setf (fdefinition 'find-method) #'boot-find-method)
(setf (generic-function-name #'find-method) 'find-method)


(setf (method-function (eval '(defmethod boot-register-method-as-spec-user ((spec standard-class)
									    method))))
      #'register-method-as-spec-user)
(setf (fdefinition 'register-method-as-spec-user) #'boot-register-method-as-spec-user)
(setf (generic-function-name #'register-method-as-spec-user) 'register-method-as-spec-user)


(defmethod register-method-as-spec-user ((spec T) method) ;; empty default method.
  (declare (ignore method))
  )

(defmethod register-method-as-spec-user ((spec forward-referenced-class) method)
  (push method (class-spec-users spec)))


;;; ----------------------------------------------------------------------
;;;

(defclass locking-generic-function (standard-generic-function)
  ((lock :initform nil :accessor generic-function-lock)))

;;; Since we cannot turn compute-effective-method into a generic-function
;;; without the gf-cache code going into a tail-spin ending with a crash,
;;; we have to resort to this kind of somewhat primitive wrapping! JCB
(let ((old-compute-effective-method #'compute-effective-method))
  (defun compute-effective-method (gf method-combination applicable-methods)
    (let ((gf-lock)
	  (unlocked-effective-method (funcall old-compute-effective-method
					      gf method-combination applicable-methods)))
      (if (typep gf 'locking-generic-function)
	  (progn
	    (setq gf-lock (generic-function-lock gf))
	    #'(lambda (args next-methods)
		(mt:with-lock (gf-lock)
		  (funcall unlocked-effective-method args next-methods))))
	unlocked-effective-method)
      )
    )
  )


(defgeneric change-class (instance new-class &rest initargs)
  (:generic-function-class locking-generic-function))
(setf (generic-function-lock #'change-class) +metadata-lock+)

(defgeneric add-method (gf method)
  (:generic-function-class locking-generic-function))
(setf (generic-function-lock #'add-method) +metadata-lock+)

(defgeneric remove-method (gf method)
  (:generic-function-class locking-generic-function))
(setf (generic-function-lock #'remove-method) +metadata-lock+)

(defgeneric find-method (gf method-qualifiers specializers &optional errorp)
  (:generic-function-class locking-generic-function))
(setf (generic-function-lock #'find-method) +metadata-lock+)

(defgeneric reinitialize-instance (instance &rest initargs &key &allow-other-keys)
  (:generic-function-class locking-generic-function))
(setf (generic-function-lock #'reinitialize-instance) +metadata-lock+)

(defgeneric make-instances-obsolete (class)
  (:generic-function-class locking-generic-function))
(setf (generic-function-lock #'make-instances-obsolete) +metadata-lock+)



;;; ----------------------------------------------------------------------
;;; Error messages

(defmethod no-applicable-method (gf &rest args)
  ;;(declare (ignore args))
  (error "Generic function: ~A. No applicable method given arguments: ~S"
	 (generic-function-name gf) args))

(defmethod no-next-method (gf method &rest args)
  (declare (ignore gf))
  (error "In method ~A. No next method given arguments: ~S" method args))



;;; Now we protect classes from redefinition:
(eval-when (compile load)
(defun setf-find-class (new-value name &optional errorp env)
  (declare (ignore errorp env))
  (with-metadata-lock
   (let ((old-class (find-class name nil)))
     (cond
      ((typep old-class 'built-in-class)
       (error "The class associated to the CL specifier ~S cannot be changed."
	      name))
      ((member name '(CLASS BUILT-IN-CLASS) :test #'eq)
       (error "The kernel CLOS class ~S cannot be changed." name))
      ((classp new-value)
       (si:set-class-proper-name name new-value))
      ((null new-value)
       (si:set-class-proper-name name new-value))
      (t (error "~A is not a class." new-value)))))
  new-value)
) ;; eval-when

;;
;; Stub out a few MOP functions used by slime.  JCB 2009/08/28
;;

(defgeneric specializer-direct-methods (specializer))

(defmethod specializer-direct-methods ((specializer t))
  nil)

(defgeneric compute-applicable-methods-using-classes (gf classes))

(defmethod compute-applicable-methods-using-classes ((gf t) classes)
  (declare (ignore classes))
  (values nil nil))

(defclass eql-specializer ()
  ((object :initarg :object :reader eql-specializer-object)))

;;;
;;; End of MOP stubs.
;;;

(defgeneric make-instance (class &rest initargs &key &allow-other-keys))

(defgeneric allocate-instance (class &rest initargs &key &allow-other-keys))

(defgeneric initialize-instance (instance &rest initargs &key &allow-other-keys))

(defgeneric shared-initialize (instance slot-names &rest initargs &key &allow-other-keys))

;;(defgeneric reinitialize-instance (instance &rest initargs &key &allow-other-keys)) ;; already done in clos-change.lsp

(defgeneric update-instance-for-different-class (previous current &rest initargs &key &allow-other-keys))

(defgeneric update-instance-for-redefined-class (instance added-slots discarded-slots property-list &rest initargs &key &allow-other-keys))

