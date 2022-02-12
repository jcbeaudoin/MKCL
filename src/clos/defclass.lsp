;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: CLOS -*-
;;;;
;;;;  Copyright (c) 1992, Giuseppe Attardi.
;;;;  Copyright (c) 2010-2015,2022, Jean-Claude Beaudoin.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Lesser General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 3 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../../Copyright' for full details.

(in-package "CLOS")

;;; ----------------------------------------------------------------------
;;; DEFCLASS

(defun self-evaluating-p (form)
  ;; Output T when the form has the same value if it appears quoted
  ;; or unquoted. It is used to check whether we can transform
  ;; (LIST form) into '(form ...)
  (and (atom form)
       (or (not (symbolp form))
	   (member form '(t nil +initform-unsupplied+))
	   (and (boundp form) (constantp form) (eq (symbol-value form) form)))))

(defun parse-default-initargs (default-initargs)
  (do* ((output-list nil)
	(scan default-initargs (cddr scan))
	(already-supplied '()))
       ((endp scan) `(list ,@(nreverse output-list)))
    (when (endp (rest scan))
      (si::simple-program-error "Wrong number of elements in :DEFAULT-INITARGS option."))
    (let ((slot-name (first scan))
	  (initform (second scan)))
      (if (member slot-name already-supplied)
	  (si::simple-program-error "~S is duplicated in :DEFAULT-INITARGS form ~S"
				    slot-name default-initargs)
	  (push slot-name already-supplied))
      (push `(list ',slot-name ',initform ,(make-function-initform initform))
	    output-list))))

(defmacro defclass (&whole form &rest args)
  (let* (name superclasses slots options
	 metaclass-name default-initargs documentation
	 (processed-options '()))
    (unless (>= (length args) 3)
      (si::simple-program-error "Illegal defclass form: the class name, the superclasses ~
                                 and the slots should always be provided"))
    (setq name (first args)
	  superclasses (second args)
	  slots (third args)
	  args (cdddr args))
    (unless (and (listp superclasses) (listp slots))
      (si::simple-program-error "Illegal defclass form: superclasses and slots should be lists"))
    (unless (and (symbolp name) (every #'symbolp superclasses))
      (si::simple-program-error "Illegal defclass form: superclasses and class name are not valid"))
    ;;
    ;; Here we compose the final form. The slots list, and the default initargs
    ;; may contain object that need to be evaluated. Hence, it cannot be always
    ;; quoted.
    ;;
    (do ((l (setf slots (parse-slots slots)) (rest l)))
	((endp l)
	 (setf slots
	       (if (every #'constantp slots)
		   (si:maybe-quote (mapcar #'si:maybe-unquote slots))
		   `(list ,@slots))))
      (let* ((slotd (first l))
	     (initfun (getf slotd :initfunction nil)))
	(if initfun
	    (progn
	      (remf slotd :initfunction)
	      (setf slotd (list* 'list :initfunction initfun (mapcar #'si:maybe-quote slotd))))
	    (setf slotd (si:maybe-quote slotd)))
	(setf (first l) slotd)))
    (dolist (option args)
      (let ((option-name (first option))
	    option-value)
	(if (member option-name processed-options)
	    (si:simple-program-error
	     "Option ~s for DEFCLASS specified more than once"
	     option-name)
	    (push option-name processed-options))
	(setq option-value
	      (case option-name
		((:metaclass :documentation :source)
		 (si:maybe-quote (second option)))
		(:default-initargs
		 (setf option-name :direct-default-initargs)
		 (parse-default-initargs (rest option)))
		(otherwise
                 (let ((maybe-value (rest option)))
                   (si:maybe-quote (if (rest maybe-value) maybe-value (first maybe-value)))))))
	(setf options (list* (si:maybe-quote option-name) option-value options))))
    (unless (member :source options)
      (setf options (list* :source (list 'quote (copy-list si:*source-location*)) options)))
    `(si::define-when (compile load eval)
       ,(si:register-with-pde form `(ensure-class ',name
						   :direct-superclasses ',superclasses
						   :direct-slots ,slots
						   ,@options)))))

;;; ----------------------------------------------------------------------
;;; ENSURE-CLASS
;;;
(defun ensure-class (name &rest initargs)
  (with-metadata-lock
   (let* ((old-class nil)
	  new-class)
     ;; Only classes which have a PROPER name are redefined. If a class
     ;; with the same name is register, but the name of the class does not
     ;; correspond to the registered name, a new class is returned.
     ;; [Hyperspec 7.7 for DEFCLASS]
     (when name
       (when (and (setf old-class (find-class name nil))
		  (not (eq (class-name old-class) name)))
	 (setf old-class nil)))
     (setf new-class (apply #'ensure-class-using-class old-class name initargs))
     (when name
       (si:validate-type-name name)
       (setf (find-class name) new-class))
     new-class)))


;;; ----------------------------------------------------------------------
;;; ORDERING OF CLASSES
;;;
;;; We have two implementations of the algorithm described in Sect. 4.3.5
;;; of the Common Lisp Hyperspec. The first implementation is a literal
;;; transcription of that algorithm. The second implementation does not
;;; create the list of pairs for describing the order, it is not recursive
;;; and conses less
#+(or)
(defun compute-clos-class-precedence-list (new-class superclasses)
  (labels ((pair-list (l)
	     (if (or (null l) (endp (cdr l)))
		 nil
		 (cons (cons (first l) (second l))
		       (pair-list (rest l)))))
	   (walk-supers (parent superclasses class-list precedence-alist)
	     (let ((new-alist (pair-list (if parent
					     (list* parent superclasses)
					     superclasses))))
	       (setf precedence-alist (nconc new-alist precedence-alist)
		     class-list (union superclasses class-list)))
	     (dolist (c superclasses)
	       (multiple-value-setq (class-list precedence-alist)
		 (walk-supers c (class-direct-superclasses c) class-list precedence-alist)))
	     (values class-list precedence-alist))
	   (cycle-error (new-class)
	     (error "A cycle has been detected in the class precedence list for ~A."
		    (class-name new-class)))
	   (free-elements (class-list precedence-alist)
	     (set-difference class-list
			     (delete-duplicates (mapcar #'cdr precedence-alist))))
	   (next-element (free-list cpl)
	     (if (or (null cpl) (endp free-list) (endp (rest free-list)))
		 (first free-list)
		 (dolist (i cpl nil)
		   (dolist (j (class-direct-superclasses i))
		     (when (member j free-list)
		       (return-from next-element j)))))))
  (if (endp (rest superclasses))
      (let ((class (first superclasses)))
	(list* new-class (class-precedence-list class)))
      (multiple-value-bind (class-list precedence-alist)
	  (walk-supers nil superclasses nil nil)
	  (do ((cpl (list new-class)))
	      ((null class-list)
	       (if precedence-alist (cycle-error new-class) (nreverse cpl)))
	    (let* ((candidates (free-elements class-list precedence-alist))
		   (next (next-element candidates cpl)))
	      (unless next
		(cycle-error new-class))
	      (setf precedence-alist (delete next precedence-alist :key #'car)
		    class-list (delete next class-list)
		    cpl (cons next cpl))))))))

(defun compute-clos-class-precedence-list (new-class superclasses)
  (labels ((walk-supers (superclasses)
	     ;; Creates two lists, one with all the superclasses of a class to be created,
	     ;; and a second list with lists (c1 c2 c3 ... cn) that represent a partial
	     ;; ordering of the classes (c1 > c2), (c2 > c3), etc."
	     (let ((class-list '())
		   (precedence-lists (list superclasses)))
	       (loop (unless superclasses
		       (return (values class-list precedence-lists)))
		  (let ((next-class (pop superclasses)))
		    (unless (member next-class class-list :test 'eql)
		      (let ((more-classes (class-direct-superclasses next-class)))
			(setf class-list (list* next-class class-list)
			      precedence-lists (list* (list* next-class more-classes)
						      precedence-lists)
			      superclasses (append more-classes superclasses))))))))
	   (cycle-error (class)
	     (error "A cycle has been detected in the class precedence list for ~A."
		    (class-name class)))
	   (has-no-precedent (class precedence-lists)
	     ;; Check if CLASS is not preceded by any other class in the partial order.
	     (dolist (partial-order precedence-lists t)
	       (when (member class (rest partial-order) :test 'eql)
		 (return nil))))
	   (free-elements (class-list precedence-lists)
	     ;; Return classes that are not preceded by anyone
	     (let ((output '()))
	       (dolist (class class-list)
		 (when (has-no-precedent class precedence-lists)
		   (push class output)))
	       output))
	   (next-element (free-list cpl)
	     ;; Compute the next element that we will add to the class precedence list.
	     (if (or (null cpl) (endp free-list) (endp (rest free-list)))
		 (first free-list)
		 (dolist (i cpl nil)
		   (dolist (j (class-direct-superclasses i))
		     (when (member j free-list :test 'eql)
		       (return-from next-element j))))))
	   (delete-class (class precedence-lists)
	     (do ((l precedence-lists (rest l)))
		 ((null l)
		  (delete nil precedence-lists))
	       (let ((one-list (first l)))
		 (when (eq class (first one-list))
		   (setf (first l) (rest one-list)))))))
    (if (endp (rest superclasses))
	(let ((class (first superclasses)))
	  (list* new-class (class-precedence-list class)))
	(multiple-value-bind (class-list precedence-lists)
	    (walk-supers superclasses)
	  (do ((cpl (list new-class)))
	      ((null class-list)
	       (if precedence-lists (cycle-error new-class) (nreverse cpl)))
	    (let* ((candidates (free-elements class-list precedence-lists))
		   (next (next-element candidates cpl)))
	      (unless next
		(cycle-error new-class))
	      (setf precedence-lists (delete-class next precedence-lists)
		    class-list (delete next class-list)
		    cpl (cons next cpl))))))))

;;; ----------------------------------------------------------------------
