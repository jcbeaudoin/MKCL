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

;;; This holds fake methods created during bootstrap.
;;; It is  an alist of:
;;;	(method-name {method}+)
(defvar *early-methods* nil)

;;;
;;; This is used by combined methods to communicate the next methods to
;;; the methods they call.
;;;
(defvar *next-methods* nil)


;;; ----------------------------------------------------------------------
;;; DEFMETHOD
;;;

(defmacro defmethod (&whole whole &rest args &environment env)
  (multiple-value-bind (name qualifiers specialized-lambda-list body)
      (parse-defmethod args)
    (multiple-value-bind (lambda-list required-parameters specializers)
	(parse-specialized-lambda-list specialized-lambda-list)
      (multiple-value-bind (fn-form doc plist)
	  (expand-defmethod name qualifiers 
			    specialized-lambda-list lambda-list
			    required-parameters specializers body env)
	(si:register-with-pde 
	 whole
	 `(si::define-when (:load-toplevel :execute)
	    (multiple-value-call #'install-method 
			       ',name ',qualifiers
			       ,(list 'si::quasiquote specializers)
			       ',lambda-list ',doc
			       ',plist 
			       (let (.this-generic-func. .this-method.)
				 (values ,fn-form
					 #'(lambda (gf method) 
					     (setq .this-generic-func. gf .this-method. method))))
			       nil
			       :source ',(copy-list si:*source-location*)
			       ))
	 )))))


;;; ----------------------------------------------------------------------
;;;                                                  method body expansion

(defun expand-defmethod (generic-function-name qualifiers 
			 specialized-lambda-list lambda-list
			 required-parameters specializers body env)
  (declare (ignore qualifiers))
  (multiple-value-bind (declarations real-body documentation)
      (sys::find-declarations body)
    ;; FIXME!! This deactivates the checking of keyword arguments
    ;; inside methods. The reason is that this checking must be
    ;; supplemented the knowledge of the keyword arguments of all
    ;; applicable methods (X3J13 7.6.5). Therefore, we should insert
    ;; that check, either in the method itself so that it is done
    ;; incrementally, or in COMPUTE-EFFECTIVE-METHOD.
    (when (and (member '&key lambda-list)
	       (not (member '&allow-other-keys lambda-list)))
      (let ((x (position '&aux lambda-list)))
	(setf lambda-list
		(append (subseq lambda-list 0 x)
			'(&allow-other-keys)
			(and x (subseq lambda-list x))))))
    (let* ((class-declarations
	    (nconc (loop for name in required-parameters
		      for type in specializers
		      when (and (not (eq type t)) (symbolp type))
		      nconc `((type ,type ,name)
			      (si::no-check-type ,name)))
		   (cdar declarations)))
	   (ignorables
	    (loop for specialized-arg in specialized-lambda-list
	       until (member specialized-arg '(&optional &rest &key &aux &allow-other-keys))
	       when (listp specialized-arg)
	       nconc (cons (car specialized-arg) nil)
		 )
	     )
	   (method-lambda
	    ;; Remove the documentation string and insert the
	    ;; appropriate class declarations.  The documentation
	    ;; string is removed to make it easy for us to insert
	    ;; new declarations later, they will just go after the
	    ;; second of the method lambda.  The class declarations
	    ;; are inserted to communicate the class of the method's
	    ;; arguments to the code walk.
	    `(si::lambda-block ,generic-function-name
	      ;;,(mangle-internal-method-name generic-function-name qualifiers specializers)
	      ,lambda-list
	      ,@(and ignorables `((declare (ignorable ,@ignorables))))
	      ,@(and class-declarations `((declare ,@class-declarations)))
	      ,@real-body))
	   
	   (aux-bindings ())
	   (plist ()))
      (multiple-value-bind (call-next-method-p next-method-p-p in-closure-p)
	  (walk-method-lambda method-lambda required-parameters env)

	(when (or call-next-method-p next-method-p-p)
	  (setf plist '(:needs-next-method-p t))) ;; any use? JCB

	(if in-closure-p
	    (progn
	      (setf plist '(:needs-next-method-p FUNCTION)) ;; any use? JCB
	      (setf real-body
		`((let* ((.closed-combined-method-args.
			  (locally 
			   (declare (special .combined-method-args.))
			   (if (listp .combined-method-args.)
			       .combined-method-args.
			     (apply #'list .combined-method-args.))))
			 (.next-methods. *next-methods*))
		    (flet ((call-next-method (&rest args)
			      (declare (dynamic-extent args))
			    (if .next-methods.
				(funcall (car .next-methods.)
					 (or args .closed-combined-method-args.)
					 (rest .next-methods.))
			      (apply #'no-next-method .this-generic-func. .this-method. args)
			      )
			    )
			   (next-method-p ()
			     (not (null .next-methods.))))
			  ,@real-body))))
	      )
	  (progn
	    (setf real-body
		  `((macrolet
		     (
		      ,@(and 
			 call-next-method-p
			 '((call-next-method (&rest cnm-args)
			     `(if *next-methods*
				  (funcall
				   (car *next-methods*)
				   ,(if cnm-args `(list ,@cnm-args) 
				      '(locally (declare (special .combined-method-args.))
						.combined-method-args.))
				   (rest *next-methods*))
				(funcall #'no-next-method .this-generic-func. .this-method. ,@cnm-args)
				)
			     ))
			 )
		      ,@(and
			 next-method-p-p
			 '((next-method-p ()
			     `(not (null *next-methods*))))
			 )
		      )
		     ,@real-body)))
	    )
	  )
	(values
	 `(si::lambda-block ,generic-function-name 
	      ,lambda-list
	      ,@(and ignorables `((declare (ignorable ,@ignorables))))
	      ,@(and class-declarations `((declare ,@class-declarations)))
	      ,@real-body)
	 documentation
	 plist)))))

(defun environment-contains-closure (env)
  ;;
  ;; As explained in compiler.d (make_lambda()), we use a symbol with name
  ;; "FUNCTION" to mark the beginning of a function. If we find that symbol
  ;; twice, it is quite likely that this form will end up in a closure.
  ;;
  (flet ((function-boundary (s)
	   (and (consp s)
		(symbolp (setf s (first s)))
		(null (symbol-package s))
		(equal (symbol-name s) "FUNCTION"))))
    (> (count-if #'function-boundary (car env)) 1)))

(defun walk-method-lambda (method-lambda required-parameters env)
  (declare (ignore required-parameters env))
  (let ((call-next-method-p nil)
	(next-method-p-p nil)
	(in-closure-p nil))
    (flet ((code-walker (form env)
	     (unless (atom form)
	       (let ((name (first form)))
		 (case name
		   (CALL-NEXT-METHOD
		    (setf call-next-method-p
			  (or call-next-method-p T)
			  in-closure-p
			  (or in-closure-p (environment-contains-closure env))))
		   (NEXT-METHOD-P
		    (setf next-method-p-p t
			  in-closure-p (or in-closure-p (environment-contains-closure env))))
		   (FUNCTION
		    (when (eq (second form) 'CALL-NEXT-METHOD)
		      (setf in-closure-p t
			    call-next-method-p 'FUNCTION))
		    (when (eq (second form) 'NEXT-METHOD-P)
		      (setf next-method-p-p 'FUNCTION
			    in-closure-p t))))))
	     form))
      (let ((si::*code-walker* #'code-walker))
	;; si::*code-walker* is a special hook built into the bytecompiler,
	;; see ../c/compiler.d in function compile_form().
	;; The following "coerce" forces the byte-compilation
	;; of the form bound to "method-lambda".
	(coerce method-lambda 'function)))
    (values call-next-method-p
	    next-method-p-p
	    in-closure-p)))

;;; ----------------------------------------------------------------------
;;;                                                                parsing

(defun legal-generic-function-name-p (name)
  (si::valid-function-name-p name))

(defun parse-defmethod (args)
  ;; This function has to extract the name of the method, a list of
  ;; possible qualifiers (identified by not being lists), the lambda
  ;; list of the method (which might be empty!) and the body of the
  ;; function.
  (let* (name)
    (unless args
      (error "Illegal defmethod form: missing method name"))
    (setq name (pop args))
    (unless (legal-generic-function-name-p name)
      (error "~A cannot be a generic function specifier.~%~
             It must be either a non-nil symbol or ~%~
             a list whose car is setf and whose second is a non-nil symbol."
	     name))
    (do ((qualifiers '()))
	((progn
	   (when (endp args)
	     (error "Illegal defmethod form: missing lambda-list"))
	   (listp (first args)))
	 (values name (nreverse qualifiers) (first args) (rest args)))
      (push (pop args) qualifiers))))

(defun parse-specialized-lambda-list (specialized-lambda-list)
  "This function takes a method lambda list and outputs the list of required
arguments, the list of specializers and a new lambda list where the specializer
have disappeared."

  ;; SI:PROCESS-LAMBDA-LIST will ensure that the lambda list is
  ;; syntactically correct and will output as a first argument the
  ;; list of required arguments. We use this list to extract the
  ;; specializers and build a lambda list without specializers.
  (do* ((arglist (si::process-lambda-list specialized-lambda-list 'METHOD)
		 (rest arglist))
	(lambda-list (copy-list specialized-lambda-list))
	(ll lambda-list (rest ll))
	(required-parameters '())
	(specializers '())
	arg variable specializer)
       ((null arglist)
	(values lambda-list
		(nreverse required-parameters)
		(nreverse specializers)))
    (setf arg (first arglist))
    (cond
      ;; Just a variable
      ((atom arg)
       (setf variable arg specializer T))
      ;; List contains more elements than variable and specializer
      ((not (endp (cddr arg)))
       (si::simple-program-error "Syntax error in method specializer ~A" arg))
      ;; Specializer is NIL
      ((null (setf variable (first arg)
		   specializer (second arg)))
       (si::simple-program-error
	"NIL is not a valid specializer in a method lambda list"))
      ;; Specializer is a class name
      ((atom specializer))
      ;; Specializer is (EQL value)
      ((and (eql (first specializer) 'EQL)
	    (endp (cddr specializer)))
       (let ((value (second specializer)))
	 (setf specializer
	       `(eql ,(if (constantp value)
			  (eval value)
			  (list 'si::unquote value))))))
      ;; Otherwise, syntax error
      (t
       (si::simple-program-error "Syntax error in method specializer ~A" arg)))
    (setf (first ll) variable)
    (push variable required-parameters)
    (push specializer specializers)))

(defun declaration-specializers (arglist declarations)
  (do ((argscan arglist (cdr argscan))
       (declist (when declarations (cdr declarations))))
      ((or
	(null argscan)
	(member (first argscan) '(&OPTIONAL &REST &KEY &ALLOW-OTHER-KEYS &AUX)))
       `(DECLARE ,@declist))
      (when (listp (first argscan))
	    (push `(TYPE ,(cadar argscan) ,(caar argscan)) declist))))


;;; ----------------------------------------------------------------------
;;;                                                             operations

;;; early version
(defun make-method (method-class qualifiers specializers lambda-list doc
				 fun fun-context-setter plist options)
  (let* ((instance-size (+ #.(length +standard-method-slots+)
			   (if (eq method-class 'standard-method)
			       0 2))) ;; probably because of standard-accessor-method. JCB
	 (method (si:allocate-raw-instance nil method-class instance-size))
	 (source (when (member :source options) (getf options :source))))
    (setf (method-generic-function method) nil
	  (method-lambda-list method) lambda-list
	  (method-function method) fun
	  (method-fun-context-setter method) fun-context-setter
	  (method-specializers method) specializers
	  (method-qualifiers method) qualifiers
	  (method-documentation method) doc
	  (method-plist method) plist
	  (method-version method) 0
	  (method-previous method) nil
	  (method-source method) source
	  )
    method))


;;; early version used during bootstrap
(defun register-method-as-spec-user (spec method)
  (when (typep spec 'standard-class)
    (push method (class-spec-users spec))))


;;; early version used during bootstrap
(defun method-p (x)
  (si::instancep x))

(defun method-needs-next-methods-p (method)
  (getf (method-plist method) :needs-next-methods-p))

(defun convert-to-implicit-generic-function-lambda-list (lambda-list)
  (multiple-value-bind (reqs nreq opts nopt rest keyp keys nkey allow-other-keys)
      (si::process-lambda-list lambda-list 'FUNCTION)
    (declare (ignore nreq nopt keys nkey allow-other-keys))
    (let ((new-opts nil))
      (do () ((null opts))
	(push (car opts) new-opts)
	(setq opts (cdddr opts)))
      (let* ((new-lambda-list (copy-list reqs)) (last (last new-lambda-list)))
	(when new-opts (rplacd last (cons '&optional (nreverse new-opts))) (setq last (last last)))
	(when rest (rplacd last (list '&rest rest)) (setq last (last last)))
        (when keyp (rplacd last (cons '&key nil)))
	new-lambda-list))))

;;; early version used during bootstrap
(defun add-method (gf method)
  (let* ((name (generic-function-name gf))
	 (method-entry (assoc name *early-methods*)))
    (unless method-entry
      (setq method-entry (list name))
      (push method-entry *early-methods*))
    (push method (cdr method-entry))
    (push method (generic-function-methods gf))
    (setf (method-generic-function method) gf)
    (unless (si::sl-boundp (generic-function-lambda-list gf))
      (setf (generic-function-lambda-list gf)
            (convert-to-implicit-generic-function-lambda-list (method-lambda-list method)))
      (setf (generic-function-argument-precedence-order gf)
	    (si::process-lambda-list (method-lambda-list method) t)))
    (compute-g-f-spec-list gf)
    (maybe-clear-cached-make-instance gf method)
    (dolist (spec (method-specializers method))
      (register-method-as-spec-user spec method))
    method))

(defun find-method (gf qualifiers specializers &optional (errorp t))
  (let* ((method-list (generic-function-methods gf))
	 (required-args (subseq (generic-function-lambda-list gf) 0
				(length specializers))) ;; This one is inexact. JCB
	 found)
    (declare (ignore required-args))
    (dolist (method method-list)
      (when (and (equal qualifiers (method-qualifiers method))
		 (equal specializers (method-specializers method)))
	(return-from find-method method)))
    ;; If we did not find any matching method, then the list of
    ;; specializers might have the wrong size and we must signal
    ;; an error.
    (cond ((/= (length specializers)
	       (length (generic-function-argument-precedence-order gf)))
	   (error
	    "The specializers list~%~A~%does not match the number of required arguments in ~A"
	    specializers (generic-function-name gf)))
	  (errorp
	   (error "There is no method on the generic function ~S ~
                   that agrees on qualifiers ~S and specializers ~S"
		  (generic-function-name gf)
		  qualifiers specializers)))
    nil))


;;; ----------------------------------------------------------------------
;;;                                                             with-slots

(defmacro with-slots (slot-entries instance-form &body body)
  (let* ((temp (gensym))
	 (accessors
	  (do ((scan slot-entries (cdr scan))
	       (res))
	      ((null scan) (nreverse res))
	      (if (symbolp (first scan))
		  (push `(,(first scan) (slot-value ,temp ',(first scan))) res)
		(push `(,(caar scan)
			(slot-value ,temp ',(cadar scan))) res)))))
    `(let ((,temp ,instance-form))
       (symbol-macrolet ,accessors ,@body))))

;;(with-slots (x (y2 y)) inst (setq x y2))

;;; ----------------------------------------------------------------------
;;;                                                         with-accessors

(defmacro with-accessors (slot-accessor-pairs instance-form &body body)
  (let* ((temp (gensym))
	 (accessors (do ((scan slot-accessor-pairs (cdr scan))
			(res))
		       ((null scan) (nreverse res))
		       (push `(,(caar scan) (,(cadar scan) ,temp)) res))))
    `(let ((,temp ,instance-form))
       (symbol-macrolet ,accessors ,@body))))

;;; Force the compiler into optimizing use of gethash inside methods:
(setf (symbol-function 'SLOT-INDEX) (symbol-function 'GETHASH))
