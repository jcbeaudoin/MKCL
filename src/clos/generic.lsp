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

;;; ----------------------------------------------------------------------
;;; DEFGENERIC
;;;

(defmacro defgeneric (&whole whole &rest args)
  (multiple-value-bind (function-specifier lambda-list options)
    (parse-defgeneric args)
    (parse-lambda-list lambda-list)
    ;; process options
    (multiple-value-bind (option-list method-list)
	(parse-generic-options options lambda-list)
	(unless (member :source option-list)
	  (setf option-list 
		(list* :source (list 'quote (copy-list si:*source-location*))
		       option-list))
	  )
      (let* ((output `(si::define-when (:load-toplevel :execute)
			(ensure-generic-function ',function-specifier
						 :delete-methods t ,@option-list))))
	(when method-list
	  (setf method-list (mapcar #'(lambda (m) `(defmethod ,function-specifier ,@m))
				    method-list)
		output `(si::define-when (:load-toplevel :execute)
			  (associate-methods-to-gfun ,output ,@method-list))))
	(si:register-with-pde whole output)))))

(defun parse-defgeneric (args)
  (let (function-specifier)
    (unless args
      (simple-program-error "Illegal defgeneric form: missing generic function name"))
    (setq function-specifier (pop args))
    (unless args
      (simple-program-error "Illegal defgeneric form: missing lambda-list"))
    (values function-specifier (first args) (rest args))))

(defun parse-generic-options (options lambda-list)
  (let* ((processed-options '())
	 (method-list '())
	 (declarations '())
	 arg-list)
    (dolist (option options)
      (let ((option-name (first option))
	    option-value)
	(cond ((eq option-name :method)
	       ;; We do not need to check the validity of this
	       ;; because DEFMETHOD will do it.
	       (push (rest option) method-list))
	      ((eq option-name 'declare)
	       (setf declarations (append (rest option) declarations)))
	      ((member option-name processed-options)
	       (simple-program-error "Option ~s specified more than once"
				     option-name))
	      (t
	       (push option-name processed-options)
	       ;; We leave much of the type checking for SHARED-INITIALIZE
	       (setq option-value
		     (case option-name
		       (:argument-precedence-order
			(rest option))
		       (:method-combination
			(rest option))
		       ((:documentation :generic-function-class :method-class)
			(unless (endp (cddr option))
			  (simple-program-error "Too many arguments for option ~A"
						option-name))
			(second option))
		       (otherwise
			(simple-program-error "~S is not a legal defgeneric option"
					      option-name))))
	       (setf arg-list `(',option-name ',option-value ,@arg-list))))))
    (values `(:lambda-list ',lambda-list ,@arg-list
	      ,@(when declarations `(:declarations ',declarations)))
	    method-list)))

(defun parse-lambda-list (lambda-list &optional post-keyword)
  (let ((arg (car lambda-list)))
    (cond ((null lambda-list))
	  ((eq arg '&AUX)
	   (simple-program-error "&aux is not allowed in a generic function lambda-list"))
	  ((member arg lambda-list-keywords)
	   (parse-lambda-list (cdr lambda-list) t))
	  (post-keyword
	   ;; After a lambda-list-keyword there can be no specializers.
	   (parse-lambda-list (cdr lambda-list) t))
	  (t
	   (if (listp arg)
	       (simple-program-error "the parameters cannot be specialized in generic function lambda-list")
	       (parse-lambda-list (cdr lambda-list)))))))

(defun valid-declaration-p (decl)
  (unless (eq (first decl) 'OPTIMIZE)
	  (simple-program-error "The only declaration allowed is optimize"))
  (dolist (first (rest decl))
    (when (atom first)
      (setq first (cons first 3)))
    (unless (member (car first) '(SPEED SPACE COMPILATION-SPEED DEBUG SAFETY))
      (simple-program-error "The only qualities allowed are speed and space")))
  decl)

;;; ----------------------------------------------------------------------
;;; GENERIC FUNCTION (RE)INITIALIZATION PROTOCOL
;;

(defun lambda-list-required-arguments (lambda-list)
  (si::process-lambda-list lambda-list t))

(defmethod shared-initialize ((gfun generic-function) slot-names &rest initargs
			      &key (lambda-list nil l-l-p)
			      (argument-precedence-order nil a-o-p)
			      (documentation nil)
			      (declarations nil)
			      method-combination
			      (method-class (find-class 'method))
			      )
  ;;
  ;; Check the validity of several fields.
  ;;
  (when a-o-p
    (unless l-l-p
      (simple-program-error "Supplied :argument-precedence-order, but :lambda-list is missing"))
    (let ((reqs (lambda-list-required-arguments lambda-list)))
      (dolist (l reqs)
        (unless (= (count l argument-precedence-order) 1)
          (si::simple-program-error "The required argument ~A does not appear ~
                                     exactly once in the ARGUMENT-PRECEDENCE-ORDER list ~A"
                                    l argument-precedence-order)))
      (unless (= (length reqs) (length argument-precedence-order))
        (si::simple-program-error "Unknown argument(s) ~A in the ARGUMENT-PRECEDENCE-ORDER list ~A"
                                  (set-difference argument-precedence-order reqs) argument-precedence-order))
      ))
  (unless (every #'valid-declaration-p declarations)
    (simple-program-error "Not a valid declaration list: ~A" declarations))
  (unless (or (null documentation) (stringp documentation))
    (error 'simple-type-error
	   :format-control "Not a valid documentation object ~"
	   :format-arguments (list documentation)
	   :datum documentation
	   :expected-type '(or nil string)))
  (unless (or (null method-combination)
	      (and (listp method-combination)
		   (member (first method-combination) *method-combinations*)))
    (error 'simple-type-error
	   :format-control "Not a valid method combination, ~A"
	   :format-arguments (list method-combination)
	   :datum method-combination
	   :expected-type 'list))
  (unless (si::subclassp method-class (find-class 'method))
    (error 'simple-type-error
	   :format-control "Not a valid method class, ~A"
	   :format-arguments (list method-class)
	   :datum method-class
	   :expected-type 'method))
  ;;
  ;; When supplying a new lambda-list, ensure that it is compatible with
  ;; the old list of methods.
  ;;
  (when (and l-l-p (slot-boundp gfun 'methods))
    (unless (every #'(lambda (method-ll)
		       (congruent-lambda-lists-p lambda-list method-ll))
		   (mapcar #'method-lambda-list (generic-function-methods gfun)))
      (simple-program-error "Cannot replace the lambda list of ~A with ~A ~
                             because it is not congruent with some of the methods"
			    gfun lambda-list)))

  (when (generic-function-source gfun)
    (remf initargs :source))

  (apply #'call-next-method gfun slot-names initargs) ;; JCB

  (when (and l-l-p (not a-o-p))
    (setf (generic-function-argument-precedence-order gfun)
	  (lambda-list-required-arguments lambda-list)))
  gfun)

(defmethod shared-initialize ((gfun standard-generic-function) slot-names &rest initargs)
  (declare (ignore slot-names initargs))
  (call-next-method)
  (compute-g-f-spec-list gfun)
  gfun)

(defun associate-methods-to-gfun (gfun &rest methods)
  (dolist (method methods)
    (setf (getf (method-plist method) :method-from-defgeneric-p) t))
  gfun)

(defmethod ensure-generic-function-using-class
    ((gfun generic-function) name &rest args &key (method-class 'STANDARD-METHOD)
     (generic-function-class (class-of gfun))
     (delete-methods nil))
    (declare (dynamic-extent args))
  ;; modify the existing object
  (setf args (copy-list args))
  (remf args :generic-function-class) ;; handled
  (remf args :declare) ;; dropped
  (remf args :environment) ;; dropped
  (remf args :delete-methods) ;; handled
  ;; FIXME! We should check that the class GENERIC-FUNCTION-CLASS is compatible
  ;; with the old one. In what sense "compatible" is meant, I do not know!
  ;; (See ANSI DEFGENERIC entry)
  (when (symbolp generic-function-class)
    (setf generic-function-class (find-class generic-function-class)))
  (unless (si::subclassp generic-function-class (find-class 'generic-function))
    (error "~A is not a valid :GENERIC-FUNCTION-CLASS argument for ENSURE-GENERIC-FUNCTION."
	   generic-function-class))
  (when delete-methods
    (dolist (m (copy-list (generic-function-methods gfun)))
      (when (getf (method-plist m) :method-from-defgeneric-p)
	(remove-method gfun m))))
  (unless (classp method-class)
    (setf args (list* :method-class (find-class method-class) args)))
  (when (generic-function-closed-p gfun)
    (let ((closed-gfun gfun)
	  (traced (get-sysprop name 'SI::TRACED)))
      (setq gfun (si:copy-instance gfun)) ;; create a clone of this generic-function. JCB
      (setf (generic-function-closed-p gfun) nil)
      (setf (generic-function-previous gfun) closed-gfun)
      (setf (fdefinition (or traced name)) gfun) ;; install clone on global name
      )
    (dolist (method (generic-function-methods gfun))
      (setf (method-generic-function method) gfun))
    )
  (if (eq (class-of gfun) generic-function-class)
      (progn
	(apply #'reinitialize-instance gfun args))
      (apply #'change-class gfun generic-function-class args)))

(defmethod ensure-generic-function-using-class
    ((gfun null) name &rest args &key (method-class 'STANDARD-METHOD)
     (generic-function-class 'STANDARD-GENERIC-FUNCTION)
     (delete-methods nil))
  (declare (ignorable delete-methods))
  (declare (dynamic-extent args))
  ;; else create a new generic function object
  (setf args (copy-list args))
  (remf args :generic-function-class)
  (remf args :declare)
  (remf args :environment)
  (remf args :delete-methods)
  (unless (classp method-class)
    (setf args (list* :method-class (find-class method-class) args)))
  (setf (fdefinition name)
        (set-funcallable-instance-function
         (apply #'make-instance generic-function-class :name name args)
         t)))

(defun ensure-generic-function (name &rest args &key &allow-other-keys)
  ;;(declare (dynamic-extent args))
  (with-metadata-lock
   (let ((gfun nil)
	 (traced nil))
     (when (setf traced (get-sysprop name 'SI::TRACED))
       (setf gfun (fdefinition traced)))
     (cond ((not (legal-generic-function-name-p name))
	    (simple-program-error "~A is not a valid generic function name" name))
	   ((not (fboundp name))
	    (setf (fdefinition (or traced name))
		  (apply #'ensure-generic-function-using-class gfun name args)))
	   ((si::generic-function-p (or gfun (setf gfun (fdefinition name))))
	    (apply #'ensure-generic-function-using-class gfun name args))
	   ((special-operator-p name)
	    (simple-program-error "The special operator ~A is not a valid name ~
                                  for a generic function" name))
	   ((macro-function name)
	    (simple-program-error "The symbol ~A is bound to a macro and is not a valid name ~
                                  for a generic function" name))
	   (t
	    (simple-program-error "The symbol ~A is bound to an ordinary function and ~
                                  is not a valid name for a generic function" name))
	   ))))

(defgeneric generic-function-declarations (gf))

(defmethod generic-function-declarations ((gf t))
  nil)

