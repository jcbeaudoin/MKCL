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

(defpackage "CLOS"
  (:use "CL")
  (:import-from "SI"
		"UNBOUND" "GET-SYSPROP" "PUT-SYSPROP" "REM-SYSPROP"
		"SIMPLE-PROGRAM-ERROR"))

(in-package "CLOS")

(do-external-symbols (sym) (unexport sym)) ;; clear the package exports. JCB

;;; During bootstrap these need to be redefined at some point
;;; and thus cannot be load-time evaluated.
#+mkcl-bootstrap
(proclaim '(notinline clos::find-slot-definition
		      cl::ensure-generic-function
		      clos::method-p
		      cl::make-method
		      cl::add-method
		      cl::find-method
		      cl::remove-method
		      clos::register-method-as-spec-user
		      clos::compute-effective-method
		      clos::setf-find-class
		      cl::warn
		      si::universal-error-handler))

;;;
;;;

(defvar *warn-on-redefinition* #|'(:class :generic-function :method :function)|# nil)

;;;
;;;

(defvar +inside-make-instance+ nil)

(defvar +metadata-lock+ (mt:make-lock :name '+metadata-lock+ :recursive t))

(defmacro with-metadata-lock (&body body)
  `(mt:with-lock (+metadata-lock+) ,@body)
  )

(defmacro without-metadata-lock (&body body)
  `(mt:without-lock (+metadata-lock+) ,@body)
  )

;;;----------------------------------------------------------------------
;;; BOOTSTRAP FUNCTIONS TO ACCESS SLOTS
;;;
;;; MKCL has some restictions regarding the basic classes CLASS,
;;; STANDARD-CLASS and STANDARD-GENERIC-FUNCTION. These are that, certain
;;; slots must have pre-defined positions which cannot change. That means
;;; that a user can extend these classes, but they must be the first ones
;;; in the class hierarchy, and the position of their slots must not change.

(eval-when (compile eval)
(defun create-accessors (slotds type)
  (let ((i 0)
	(output '())
	name)	
    (dolist (s slotds `(progn ,@output))
      (when (setf name (getf (cdr s) :accessor))
	(remf (cdr s) :accessor)
	(setf output
	      (append output
		      `((defun ,name (obj)
			  (si:instance-ref obj ,i))
			(defsetf ,name (obj) (x)
			  `(si:instance-set ,obj ,,i ,x))
			#+nil
			(define-compiler-macro ,name (obj)
			  `(si:instance-ref ,obj ,,i))
			))))
      (incf i))))
)

;;; ----------------------------------------------------------------------
;;; Class CLASS

;; The position of the first 5 slots of this object is hardcoded in object.h
(eval-when (compile eval)
  (defparameter +class-slots+
    '((name :initarg :name :initform nil :accessor class-id)
      (direct-superclasses :initarg :direct-superclasses :accessor class-direct-superclasses)
      (direct-subclasses :initform nil :accessor class-direct-subclasses)
      (slots :accessor class-slots)
      (precedence-list :accessor class-precedence-list)
      (direct-slots :initarg :direct-slots :accessor class-direct-slots)
      (direct-default-initargs :initarg :direct-default-initargs
       :initform nil :accessor class-direct-default-initargs)
      (default-initargs :accessor class-default-initargs)
      (finalized :initform nil :accessor class-finalized-p)
      (documentation :initarg :documentation :initform nil :accessor class-documentation)
      (size :accessor class-size)
      (sealedp :initarg :sealedp :initform nil :accessor class-sealedp)
      (cached-make-instance :initform nil :accessor class-cached-make-instance)
      (prototype)
      (version :initarg :version :initform 0 :accessor class-version)
      (previous :initarg :previous :initform nil :accessor class-previous)
      (source :initarg :source :initform nil :accessor class-source)
      (spec-users :initarg :spec-users :initform nil :accessor class-spec-users)
      )))

#.(create-accessors +class-slots+ 'class)

;;; ----------------------------------------------------------------------
;;; STANDARD-CLASS

(eval-when (compile eval)
  (defparameter +standard-class-slots+
    (append +class-slots+
	    '((slot-table :accessor slot-table)  ;; Not MOP! JCB
	      (optimize-slot-access :accessor class-optimize-slot-access)  ;; Not MOP either! JCB
	      ))))

#.(create-accessors +standard-class-slots+ 'standard-class)

;;; ----------------------------------------------------------------------
;;; STANDARD-GENERIC-FUNCTION

;; The position of the first 3 slots of this object are hardcoded in gfun.d and internal.h
(eval-when (compile eval)
  (defparameter +standard-generic-function-slots+
    '((name :initarg :name :initform nil
       :accessor generic-function-name)
      (spec-list :initform nil :accessor generic-function-spec-list)
      (method-combination 
       :initarg :method-combination :initform '(standard)
       :accessor generic-function-method-combination)
      (lambda-list :initarg :lambda-list
       :accessor generic-function-lambda-list)
      (argument-precedence-order 
       :initarg :argument-precedence-order
       :initform nil
       :accessor generic-function-argument-precedence-order)
      (method-class
       :initarg :method-class
       :initform (find-class 'standard-method)
       :accessor generic-function-method-class)
      (documentation :initarg :documentation :initform nil :accessor generic-function-documentation)
      (methods :initform nil :accessor generic-function-methods)
      (a-p-o-function :initform nil :accessor generic-function-a-p-o-function)
      (version :initarg :version :initform 0 :accessor generic-function-version)
      (previous :initarg :previous :initform nil :accessor generic-function-previous)
      (source :initarg :source :initform nil :accessor generic-function-source)
      (closed-p :initarg :closed-p :initform nil :accessor generic-function-closed-p)
      )))

#.(create-accessors +standard-generic-function-slots+ 'standard-generic-function)

;;; ----------------------------------------------------------------------
;;; STANDARD-METHOD

(eval-when (compile eval)
  (defparameter +standard-method-slots+
    '((generic-function :initarg :generic-function :initform nil
       :accessor method-generic-function)
      (lambda-list :initarg :lambda-list :initform t
       :accessor method-lambda-list)
      (specializers :initarg :specializers :initform t :accessor method-specializers)
      (qualifiers :initarg :qualifiers :initform nil :accessor method-qualifiers)
      (function :initarg :function :accessor method-function)
      (fun-context-setter :initarg :fun-context-setter :accessor method-fun-context-setter)
      (documentation :initarg :documentation :initform nil :accessor method-documentation)
      (plist :initarg :plist :initform nil :accessor method-plist)
      (version :initarg :version :initform 0 :accessor method-version)
      (previous :initarg :previous :initform nil :accessor method-previous)
      (source :initarg :source :initform nil :accessor method-source)
      )))

#.(create-accessors +standard-method-slots+ 'standard-method)

;;; ----------------------------------------------------------------------
;;;
;;; FIND-CLASS  naming classes.
;;;
;;;
;;; (FIND-CLASS <name>) returns the class named <name>.  setf can be used
;;; with find-class to set the class named <name>.  These are "extrinsic"
;;; names.  Neither find-class nor setf of find-class do anything with the
;;; name slot of the class, they only lookup and change the association from
;;; name to class.
;;; 
;;; This is only used during boot. The real one is in built-in.
(eval-when (compile)
  (defun setf-find-class (new-value class &optional errorp env)
    (declare (ignore new-value errorp env))

    #+mkcl-bootstrap 
    (format t "~&;;; Bootstrap note: Ignoring class definition for ~S" class)
    #-mkcl-bootstrap
    (warn "Ignoring class definition for ~S" class)))

(defun setf-find-class (new-value name &optional errorp env)
  (declare (ignore errorp env))
  (with-metadata-lock
   (let ((old-class (find-class name nil)))
     (cond
      ((and old-class
	    (or (typep old-class 'built-in-class)
		(member name '(class built-in-class) :test #'eq)))
       (error "The class associated to the CL specifier ~S cannot be changed." name))
      ((classp new-value)
       (si:set-class-proper-name name new-value))
      ((null new-value)
       (si:set-class-proper-name name nil))
      (t (error "~A is not a class." new-value)))))
  new-value)

(defsetf find-class (&rest x) (v) `(setf-find-class ,v ,@x))

(defun classp (obj)
  (and (si:instancep obj)
       (let ((topmost (find-class 'CLASS nil)))
	 ;; All instances can be classes until the class CLASS has
	 ;; been installed. Otherwise, we check the parents.
	 (or (null topmost)
	     (si::subclassp (si::instance-class obj) topmost)))
       t))

;;; ----------------------------------------------------------------------
;;; Methods

(defun mangle-internal-method-name (name qualifiers specializers)
  (let ((name-str (cond ((symbolp name) (symbol-name name))
			((consp name) (concatenate 'base-string 
						   "setf-" (symbol-name (cadr name))))
			(t (setq name ""))))
	qual-str
	spec-str
	mangled
	)
    (dolist (qual qualifiers)
      (setq qual-str
	    (concatenate 'base-string
			 qual-str "_"
			 (if (symbolp qual)
			     (symbol-name qual)
			   "???")))
      )
    (dolist (spec specializers)
      (setq spec-str
	    (concatenate 'base-string
			 spec-str "_"
			 (cond ((null spec) "nil")
			       ((consp spec) "eql")
			       ((si::instancep spec) (symbol-name (class-name spec)))
			       (t (symbol-name spec)))))
      )
    (setq mangled name-str)
    (when spec-str
      (setq mangled (concatenate 'base-string mangled "_" spec-str)))
    (when qual-str
      (setq mangled (concatenate 'base-string mangled "_" qual-str)))

    (let ((internal-name (intern mangled)))
      internal-name)
    )
  )

(defun install-method (name qualifiers specializers lambda-list doc plist fun fun-context-setter
		       &optional method-class &rest options)
  (declare (notinline ensure-generic-function))
  (with-metadata-lock
   (let* ((gf (ensure-generic-function name :source (getf options :source)))
	  (internal-name (mangle-internal-method-name name qualifiers specializers))
	  (specializers (mapcar #'(lambda (x)  ;; here class names are turned into class objects.
				    (cond ((null x) x)
					  ((consp x) x)
					  ((si::instancep x) x)
					  (t (find-class x))))
				specializers))
	  (method (make-method (or method-class
				   (generic-function-method-class gf))
			       qualifiers specializers lambda-list doc
			       fun fun-context-setter plist options)))
     (si:set-compiled-function-name fun internal-name)
     (si:set-compiled-function-owner fun method)
     (when fun-context-setter (funcall fun-context-setter gf method))
     (add-method gf method)
     method)))

;;; ----------------------------------------------------------------------
;;;                                                         early versions

;;; early version used during bootstrap
(defun ensure-generic-function (name &key (lambda-list (si::unbound) l-l-p) source)
  (if (and (fboundp name) (si::instancep (fdefinition name)))
      (fdefinition name)
    ;; create a fake standard-generic-function object:
    (let ((gfun (si:allocate-raw-instance nil (find-class 't)
					  #.(length +standard-generic-function-slots+))))
      (declare (type standard-object gfun))
      ;; create a new gfun
      (si::instance-sig-set gfun)
      (setf (generic-function-name gfun) name
	    (generic-function-lambda-list gfun) lambda-list
	    (generic-function-method-combination gfun) '(standard)
	    (generic-function-methods gfun) nil
	    (generic-function-spec-list gfun) nil
	    (generic-function-method-class gfun) 'standard-method)
      (setf (generic-function-documentation gfun) nil
	    (generic-function-methods gfun) nil
	    (generic-function-a-p-o-function gfun) nil
	    (generic-function-version gfun) 0
	    (generic-function-previous gfun) nil
	    (generic-function-source gfun) source
	    (generic-function-closed-p gfun) nil)
      (when l-l-p
	(setf (generic-function-argument-precedence-order gfun)
	      (si::process-lambda-list lambda-list t)))
      (set-funcallable-instance-function gfun t)
      (setf (fdefinition name) gfun)
      gfun)))


;;; ----------------------------------------------------------------------
;;; COMPUTE-APPLICABLE-METHODS
;;;
;;; FIXME! This should be split int an internal function, like
;;; raw-compute-... and a higher level interface, because the current
;;; version does not check _any_ of the arguments but it is
;;; nevertheless exported by the ANSI specification!
;;;
(defun compute-applicable-methods (gf args)
  (with-metadata-lock
   (let* ((methods (generic-function-methods gf))
	  (f (generic-function-a-p-o-function gf))
	  applicable-list
	  args-specializers)
     ;; first compute the applicable method list
     (dolist (method methods)
       ;; for each method in the list
       (do* ((scan-args args (cdr scan-args))
	     (scan-specializers (method-specializers method)
				(cdr scan-specializers))
	     (arg)
	     (spec))
	    ;; check if the method is applicable verifying
	    ;; if each argument satisfies the corresponding
	    ;; parameter specializers
	    ((null scan-args) (push method applicable-list))
	    (setq arg (first scan-args)
		  spec (first scan-specializers))
	    (cond ((null spec))
		  ((listp spec)
		   (unless (eql arg (second spec))
		     (return)))
		  ((not (si::of-class-p arg spec))
		   (return)))))
     (dolist (arg args)
       (push (class-of arg) args-specializers))
     (setq args-specializers (nreverse args-specializers))
     ;; reorder args to match the precedence order
     (when f
       (setf args-specializers
	     (funcall f (subseq args-specializers 0
				(length (generic-function-argument-precedence-order gf))))))
     ;; then order the list
     (do* ((scan applicable-list)
	   (most-specific (first scan) (first scan))
	   (ordered-list))
	  ((null (cdr scan)) (when most-specific
			       ;; at least one method
			       ;;(print (mapcar #'method-specializers
			       ;;		     (reverse (cons most-specific ordered-list))))
			       (nreverse
				(push most-specific ordered-list))))
	  (dolist (meth (cdr scan))
	    (when (eq (compare-methods most-specific
				       meth args-specializers f) 2)
	      (setq most-specific meth)))
	  (setq scan (delete most-specific scan))
	  (push most-specific ordered-list)))))

;;; ----------------------------------------------------------------------
;;;                                                      method comparison

(defun compare-methods (method-1 method-2 args-specializers f)
  (let* ((specializers-list-1 (method-specializers method-1))
	 (specializers-list-2 (method-specializers method-2)))
    (compare-specializers-lists (if f (funcall f specializers-list-1) specializers-list-1)
				(if f (funcall f specializers-list-2) specializers-list-2)
				args-specializers)))

(defun compare-specializers-lists (spec-list-1 spec-list-2 args-specializers)
  (when (or spec-list-1 spec-list-2)
    (ecase (compare-specializers (first spec-list-1)
				 (first spec-list-2)
				 (first args-specializers))
      (1 '1)
      (2 '2)
      (= 
       (compare-specializers-lists (cdr spec-list-1)
				   (cdr spec-list-2)
				   (cdr args-specializers)))
      ((nil)
       (error "The type specifiers ~S and ~S can not be disambiguated~
                  with respect to the argument specializer: ~S"
	      (or (car spec-list-1) t)
	      (or (car spec-list-2) t)
	      (car args-specializers)))))
  )

(defun fast-subtypep (spec1 spec2)
  ;; Specialized version of subtypep which uses the fact that spec1
  ;; and spec2 are either classes or of the form (EQL x)
  (if (atom spec1)
      (if (atom spec2)
	  (si::subclassp spec1 spec2)
	  ;; There is only one class with a single element, which
	  ;; is NIL = (MEMBER NIL).
	  (and (null (second spec2))
	       (eq (class-name (first spec1)) 'nil)))
      (if (atom spec2)
	  (si::of-class-p (second spec1) spec2)
	  (eql (second spec1) (second spec2)))))

(defun compare-specializers (spec-1 spec-2 arg-class)
  (let* ((cpl (class-precedence-list arg-class)))
    (cond ((equal spec-1 spec-2) '=)
	  ((null spec-1) '2)
	  ((null spec-2) '1)
	  ((fast-subtypep spec-1 spec-2) '1)
	  ((fast-subtypep spec-2 spec-1) '2)
	  ((and (listp spec-1) (eq (car spec-1) 'eql)) '1) ; is this engough?
	  ((and (listp spec-2) (eq (car spec-2) 'eql)) '2) ; Beppe
	  ((member spec-1 (member spec-2 cpl)) '2)
	  ((member spec-2 (member spec-1 cpl)) '1)
	  ;;(t (error "Complex type specifiers are not yet supported."))
	  ;; This will force an error in the caller
	  (t nil))))

(defun compute-g-f-spec-list (gf)
  (flet ((nupdate-spec-how-list (spec-how-list specializers gf)
	   ;; FIXME! This check should have happened before, shouldn't it???
	   (let ((l (length specializers)))
	     (if spec-how-list
		 (unless (= (length spec-how-list) l)
		   (error "The generic function ~A~%has ~D required arguments, ~
                           but the new specialization provides ~D."
			  gf (length spec-how-list) l))
	       (setf spec-how-list (make-list l))))
	   ;; update the spec-how of the gfun 
	   ;; computing the or of the previous value and the new one
	   (do* ((l specializers (cdr l))
		 (l2 spec-how-list (cdr l2))
		 (spec-how)
		 (spec-how-old))
		((null l))
	     (setq spec-how (first l) spec-how-old (first l2))
	     (setf (first l2)
		   (if (consp spec-how)	; an eql list
		       (if (consp spec-how-old)
			   (list* (second spec-how) spec-how-old)
			   (cdr spec-how))
		       (if (consp spec-how-old)
			   spec-how-old
			   (or spec-how spec-how-old)))))
	   spec-how-list))
  (let* ((spec-how-list nil)
	 (function nil)
	 (a-p-o (generic-function-argument-precedence-order gf)))
    (dolist (method (generic-function-methods gf))
      (setf spec-how-list
	    (nupdate-spec-how-list spec-how-list (method-specializers method) gf)))
    (setf (generic-function-spec-list gf)
	  (loop for type in spec-how-list
		for name in (generic-function-lambda-list gf)
		for i from 0
		when type collect (cons type 
					i ;; fix by A. Gavrilov used to be: (position name a-p-o)
					)))
    (let* ((g-f-l-l (generic-function-lambda-list gf)))
      (when (consp g-f-l-l)
	(let ((required-arguments (si::process-lambda-list g-f-l-l t)))
	  (unless (equal a-p-o required-arguments)
	    (setf function
		  (coerce `(lambda (%list)
			    (destructuring-bind ,required-arguments %list
			      (list ,@a-p-o)))
			  'function))))))
    (setf (generic-function-a-p-o-function gf) function)
    (si:clear-gfun-cache gf))))


(defun clear-cached-make-instance (class)
  (setf (class-cached-make-instance class) nil)
  (dolist (subclass (class-direct-subclasses class))
    (clear-cached-make-instance subclass)))

(defun maybe-clear-cached-make-instance (gf method)
  (when (case (generic-function-name gf)
	      ((shared-initialize initialize-instance allocate-instance) t)
	      (t nil))
    (clear-cached-make-instance (car (method-specializers method))))
  )

