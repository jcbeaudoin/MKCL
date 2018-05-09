;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: SYSTEM -*-
;;;;
;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Lesser General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 3 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../../Copyright' for full details.
;;;;        The structure routines.

(in-package "SYSTEM")

(defun si::structure-type-error (value slot-type struct-name slot-name)
  (error 'simple-type-error
	 :format-control "Slot ~A in structure ~A only admits values of type ~A."
	 :format-arguments (list slot-name struct-name slot-type)
	 :datum value
	 :expected-type slot-type))

(defun make-access-function (name conc-name type named slot-descr)
  (declare (ignore named))
  (let* ((slot-name (nth 0 slot-descr))
	 (read-only (nth 3 slot-descr))
	 (offset (nth 4 slot-descr))
	 (access-function (if conc-name
			      (intern (concatenate-base-strings (string conc-name) (string slot-name)))
			      slot-name)))
    (if (eql access-function (sixth slot-descr))
	(return-from make-access-function nil)
	(setf (sixth slot-descr) access-function))
    (cond ((null type)
           ;; If TYPE is NIL,
           ;;  the slot is at the offset in the structure-body.
	   (fset access-function #'(lambda (x)
				     (sys:structure-ref x name offset))))
          ((subtypep type '(OR LIST VECTOR))
	   ;; If TYPE is VECTOR, (VECTOR ... ) or LIST, ELT is used.
           (fset access-function
		 #'(lambda (x) (elt x offset))))
          (t (error "~S is an illegal structure type." type)))
    (if read-only
	(progn
	  (rem-sysprop access-function 'SETF-UPDATE-FN)
	  (rem-sysprop access-function 'SETF-LAMBDA)
	  (rem-sysprop access-function 'SETF-SYMBOL)
	  (set-documentation access-function 'SETF nil))
	(progn
	  ;; The following is used by the compiler to expand inline
	  ;; the accessor
	  (put-sysprop access-function 'STRUCTURE-ACCESS (cons (or type name) offset)))
	))
  )

(defun process-boa-lambda-list (slot-names slot-descriptions boa-list assertions)
  (let ((mentioned-slots '())
	(aux))
    ;; With a call to PROCESS-LAMBDA-LIST we ensure that the lambda list is
    ;; syntactically correct. This simplifies notably the code in the loop.
    (process-lambda-list (setq boa-list (copy-list boa-list)) 'FUNCTION)
    ;; Search for &optional or &key arguments without initialization.  Also,
    ;; record all slot names which are initialized by means of the BOA call.
    (do* ((i boa-list (rest i))
	  (slot (first i) (first i))
	  (modify nil))
	 ((endp i))
      (cond ((or (eq slot '&optional) (eq slot '&key))
	     (setq modify t))
	    ((eq slot '&rest)
	     (setq modify nil))
	    ((eq slot '&aux)
	     (setq aux t modify nil))
	    ((eq slot '&allow-other-keys)
	     )
	    ((atom slot)
	     (push slot mentioned-slots)
	     (when modify
	       (setf (first i)
		     (list slot (second (assoc slot slot-descriptions)))))
	     (when aux
	       (setf assertions (delete slot assertions :key 'cadadr))))
	    (t
	     (let ((slot-name (first slot)))
	       (when (consp slot-name)
		 (setq slot-name (second slot-name)))
	       (push slot-name mentioned-slots)
	       (when (endp (rest slot))
		 (when modify
		   (setf (rest slot)
			 (list (second (assoc slot-name slot-descriptions)))))
		 (when aux
		   (setf assertions (delete slot assertions :key 'cadadr))))))))
    ;; For all slots not mentioned above, add the default values from
    ;; the DEFSTRUCT slot description.
    (let ((other-slots (nset-difference
			(delete-if #'consp (copy-list slot-names))
			mentioned-slots)))
      (do ((l other-slots (cdr l)))
	  ((endp l))
	(let* ((slot (assoc (car l) slot-descriptions))
	       (slot-init (second slot)))
	  (when slot-init
	    (setf (car l) (list (car l) slot-init)))))
      (when other-slots
	(unless aux
	  (push '&aux other-slots))
	(setf boa-list (append boa-list other-slots)))
      (values boa-list assertions))))

(defvar *defstruct-as-defclass* nil)

(defun make-constructor (name constructor type named slot-descriptions)
  (declare (ignore named))
  ;; CONSTRUCTOR := constructor-name | (constructor-name boa-lambda-list)
  (let* ((boa-constructor-p (consp constructor))
	 (keys (unless boa-constructor-p (list '&key)))
	 (constructor-name (if boa-constructor-p (first constructor) constructor))
	 (slot-names '())
	 (assertions '()))
    (dolist (slot slot-descriptions
	     (setq slot-names (nreverse slot-names) keys (nreverse keys)))
      (push
       (cond ((null slot)
	      ;; If slot-description is NIL, it is padding for initial-offset.
	      nil)
	     ((eql (first slot) 'TYPED-STRUCTURE-NAME)
	      ;; This slot is the name of a typed structure with name.
	      (list 'QUOTE (second slot)))
	     (t
	      (let* ((slot-name (first slot))
		     (slot-type (third slot))
		     (offset (fifth slot))
		     (init-form (second slot))
		     (var-name slot-name))
		(declare (ignore offset))
		;; Unless BOA constructors are used, we should avoid using
		;; slot names as lambda variables in the constructor.
		(unless boa-constructor-p
		  (setq var-name (copy-symbol slot-name))
		  (push (if init-form (list var-name init-form) var-name)
			keys))
		;; We insert type checks for every slot and only in the
		;; case of BOA lists we remove some of these checks for
		;; uninitialized slots.
		(unless (eq 'T slot-type)
		  (push `(unless (typep ,var-name ',slot-type)
			   (si::structure-type-error ,var-name ',slot-type ',name ',slot-name))
			assertions))
		var-name)))
       slot-names))
    (when boa-constructor-p
      (setf (values keys assertions)
	    (process-boa-lambda-list slot-names slot-descriptions
				     (second constructor) assertions)))
    (cond ((null type)
           `(defun ,constructor-name ,keys
	      ,@assertions
	      #|
	      #-CLOS
              (sys:make-structure ',name ,@slot-names)
	      |#
	      ;; the .structure-constructor-class. is defined by an enclosing LET form
	      ,(if *defstruct-as-defclass*
		   (let ((obj-var (gensym)))
		     `(let ((,obj-var (make-instance .structure-constructor-class.)))
			,@(do* ((i 0 (1+ i))
				(slot-names slot-names (rest slot-names))
				(slot-name (first slot-names) (first slot-names))
				slot-inits
				)
			       ((null slot-names) (nreverse slot-inits))
			       (push `(si:instance-set ,obj-var ,i ,slot-name) slot-inits)
			       )
			,obj-var)
		     )
		 `(sys:make-structure .structure-constructor-class. ,@slot-names))
	      ))
	  ((subtypep type '(VECTOR T))
	   `(defun ,constructor-name ,keys
	     (vector ,@slot-names)))
          ((subtypep type 'VECTOR)
           `(defun ,constructor-name ,keys
              (make-array ',(list (length slot-names))
			  :element-type ',(closest-sequence-type type)
	       		  :initial-contents (list ,@slot-names))))
          ((eq type 'LIST)
           `(defun ,constructor-name ,keys
              (list ,@slot-names)))
          ((error "~S is an illegal structure type" type)))))


(defun make-predicate (name type named name-offset)
  (cond ((null type)
	 #'(lambda (x)
	     (structure-subtype-p x name)))
        ((or (eq type 'VECTOR)
             (and (consp type) (eq (car type) 'VECTOR)))
         ;; The name is at the NAME-OFFSET in the vector.
         (unless named (error "The structure should be named."))
	 #'(lambda (x)
	     (and (vectorp x)
		  (> (length x) name-offset)
		  (eq (elt x name-offset) name))))
        ((eq type 'LIST)
         ;; The name is at the NAME-OFFSET in the list.
         (unless named (error "The structure should be named."))
         (if (= name-offset 0)
	     #'(lambda (x)
		 (and (consp x) (eq (car x) name)))
	     #'(lambda (x)
		 (do ((i name-offset (1- i))
		      (y x (cdr y)))
		     ((= i 0) (and (consp y) (eq (car y) name)))
		   (declare (fixnum i))
		   (unless (consp y) (return nil))))))
        (t (error "~S is an illegal structure type." type))))


;;; PARSE-SLOT-DESCRIPTION parses the given slot-description
;;;  and returns a list of the form:
;;;        (slot-name default-init slot-type read-only offset accessor-name)

(defun parse-slot-description (slot-description offset &optional read-only)
  (let* ((slot-type 'T)
	 slot-name default-init)
    (cond ((atom slot-description)
           (setq slot-name slot-description))
          ((endp (cdr slot-description))
           (setq slot-name (car slot-description)))
          (t
           (setq slot-name (car slot-description))
           (setq default-init (cadr slot-description))
           (do ((os (cddr slot-description) (cddr os)) (o) (v))
               ((endp os))
             (setq o (car os))
             (when (endp (cdr os))
                   (error "~S is an illegal structure slot option." os))
             (setq v (cadr os))
             (case o
               (:TYPE (setq slot-type v))
               (:READ-ONLY (setq read-only v))
               (t
                (error "~S is an illegal structure slot option." os))))))
    (list slot-name default-init slot-type read-only offset nil)))


;;; OVERWRITE-SLOT-DESCRIPTIONS overwrites the old slot-descriptions
;;;  with the new descriptions which are specified in the
;;;  :include defstruct option.

(defun overwrite-slot-descriptions (new-slots old-slots)
  (do* ((output '())
        (old-slots old-slots (rest old-slots)))
       ((null old-slots)
        (nreverse output))
    (let* ((old-slot (first old-slots))
           (slot-name (first old-slot))
           (new-slot (first (member slot-name new-slots :key #'car))))
      (if (null new-slot)
          (setf new-slot old-slot)
          (let* ((old-read-only (fourth old-slot))
                 (new-read-only (fourth new-slot)))
            (cond ((and (null new-read-only)
                        old-read-only)
                   (error "Tried to turn a read only slot ~A into writtable." slot-name))
                  ((eq new-read-only :unknown)
                   (setf new-read-only old-read-only)))
            (setf new-slot (copy-list new-slot)
                  (fourth new-slot) new-read-only
                  (fifth new-slot) (fifth old-slot) ; preserve offset
                  (sixth new-slot) (sixth old-slot))))
      (push new-slot output))))

(defun structure-default-print-function (obj stream level)
  (declare (ignore level))
  (clos::print-object-as-struct obj stream))

(defun define-structure (name conc-name type named slots slot-descriptions
			 copier include print-function print-object constructors
			 offset name-offset documentation predicate source)
  (create-type-name name)
  ;; We are going to modify this list!!!
  (setf slot-descriptions (copy-tree slot-descriptions))
  ;; FIXME! We could do the same with ENSURE-CLASS!
  (unless type
    (eval `(defclass ,name ,(and include (list include))
	     ,(mapcar
	       #'(lambda (sd)
		   (if sd
		       (list* (first sd)
			      :initform (second sd)
			      :initarg 
			      (intern (symbol-name (first sd))
				      (find-package 'KEYWORD))
			      (when (third sd) (list :type (third sd))))
		       nil))		; for initial offset slots
	       slot-descriptions)
	     (:source ,source)
	     ,@(unless *defstruct-as-defclass*
                 `((:metaclass structure-class)))))
    (when *defstruct-as-defclass*
      (eval `(defmethod print-object ((obj ,name) stream)
	       (clos::print-object-as-struct obj stream))))
    )
  ;; FIXME! We can do the same with INSTALL-METHOD!
  (when print-function
    (eval `(defmethod print-object ((obj ,name) stream)
	     (,print-function obj stream 0)
	     )
	  ))
  (when print-object
    (eval `(defmethod print-object ((obj ,name) stream)
	    (,print-object obj stream))))
  (when predicate
    (fset predicate (make-predicate name type named name-offset)))
  (put-sysprop name 'DEFSTRUCT-FORM `(defstruct ,name ,@slots))
  (put-sysprop name 'IS-A-STRUCTURE t)
  (put-sysprop name 'STRUCTURE-SLOT-DESCRIPTIONS slot-descriptions)
  (put-sysprop name 'STRUCTURE-INCLUDE include)
  (put-sysprop name 'STRUCTURE-PRINT-FUNCTION print-function)
  (put-sysprop name 'STRUCTURE-TYPE type)
  (put-sysprop name 'STRUCTURE-NAMED named)
  (put-sysprop name 'STRUCTURE-OFFSET offset)
  (put-sysprop name 'STRUCTURE-CONSTRUCTORS constructors)
  (when *keep-documentation*
    (set-documentation name 'STRUCTURE documentation))
  (dolist (x slot-descriptions)
    (and x
	 (not (eql (car x) 'TYPED-STRUCTURE-NAME))
	 (funcall #'make-access-function name conc-name type named x)))
  (when copier
    (fset copier (if type #'copy-seq #'copy-structure)))
  (unless type
    (find-class name)))

;;; The DEFSTRUCT macro.

(defmacro defstruct (&whole whole name&opts &rest slots)
  "Syntax: (defstruct
         {name | (name {:conc-name | (:conc-name prefix-string) |
                        :constructor | (:constructor symbol [lambda-list]) |
                        :copier | (:copier symbol) |
                        :predicate | (:predicate symbol) |
                        (:include symbol) |
                        (:print-function function) |
                        (:print-object function) |
                        (:type {vector | (vector type) | list}) |
                        :named |
                        (:initial-offset number)}*)}
         [doc]
         {slot-name |
          (slot-name [default-value-form] {:type type | :read-only flag}*) }*
         )
Defines a structure named by NAME.  The doc-string DOC, if supplied, is saved
as a STRUCTURE doc and can be retrieved by (documentation 'NAME 'structure)."

  (let*((slot-descriptions slots)
	(name (if (consp name&opts) (first name&opts) name&opts))
        (options (when (consp name&opts) (rest name&opts)))
        (conc-name (concatenate-base-strings (string name) "-"))
	(default-constructor (intern (concatenate-base-strings "MAKE-" (string name))))
	(copier (intern (concatenate-base-strings "COPY-" (string name))))
	predicate predicate-specified
        constructors no-constructor
        include
        print-function print-object
        type named initial-offset
        offset name-offset
        documentation)

    ;; Parse the defstruct options.
    (do ((os options (cdr os)) (o) (v))
        ((endp os))
      (cond ((and (consp (car os)) (not (endp (cdar os))))
             (setq o (caar os) v (cadar os))
             (case o
               (:CONC-NAME
                (if (null v)
                    (setq conc-name nil)
                    (setq conc-name v)))
               (:CONSTRUCTOR
                (if (null v)
                    (setq no-constructor t)
                    (if (endp (cddar os))
                        (setq constructors (cons v constructors))
                        (setq constructors (cons (cdar os) constructors)))))
               (:COPIER (setq copier v))
               (:PREDICATE
                (unless (symbolp v)
                  (error "~S must be a symbol to be a valid value for defstruct option :PREDICATE" v))
                (setq predicate v)
                (setq predicate-specified t))
               (:INCLUDE
                (setq include (cdar os))
                (unless (get-sysprop v 'IS-A-STRUCTURE)
                        (error "~S is an illegal included structure." v)))
               (:PRINT-FUNCTION (setq print-function v))
	       (:PRINT-OBJECT (setq print-object v))
               (:TYPE (setq type v))
               (:INITIAL-OFFSET (setq initial-offset v))
               (t (error "~S is an illegal defstruct option." o))))
            (t
             (if (consp (car os))
                 (setq o (caar os))
                 (setq o (car os)))
             (case o
               (:CONSTRUCTOR
                (setq constructors
                      (adjoin default-constructor constructors)))
	       (:CONC-NAME
		(setq conc-name nil))
               ((:COPIER :PREDICATE))
               (:PRINT-FUNCTION (setq print-function 'structure-default-print-function))
               (:PRINT-OBJECT (setq print-object 'clos::print-object-as-struct))
               (:NAMED (setq named t))
               (t (error "~S is an illegal defstruct option." o))))))

    ;; Skip the documentation string.
    (when (and (not (endp slot-descriptions))
               (stringp (car slot-descriptions)))
          (setq documentation (car slot-descriptions))
          (setq slot-descriptions (cdr slot-descriptions)))

    ;; Check the include option.
    (when include
          (unless (equal type (get-sysprop (car include) 'STRUCTURE-TYPE))
                  (error "~S is an illegal structure include." (car include))))

    ;; Set OFFSET.
    (setq offset (if include
		     (get-sysprop (car include) 'STRUCTURE-OFFSET)
		   0))

    ;; Increment OFFSET.
    (when (and type initial-offset)
          (setq offset (+ offset initial-offset)))
    (when (and type named)
	  (unless (or (subtypep '(vector symbol) type)
		      (subtypep type 'list))
	    (error "Structure cannot have type ~S and be :NAMED." type))
          (setq name-offset offset)
          (setq offset (1+ offset)))

    ;; Parse slot-descriptions, incrementing OFFSET for each one.
    (do ((ds slot-descriptions (cdr ds))
         (sds nil))
        ((endp ds)
         (setq slot-descriptions (nreverse sds)))
      (push (parse-slot-description (car ds) offset) sds)
      (setq offset (1+ offset)))

    ;; If TYPE is non-NIL and structure is named,
    ;;  add the slot for the structure-name to the slot-descriptions.
    (when (and type named)
          (setq slot-descriptions
                (cons (list 'TYPED-STRUCTURE-NAME name) slot-descriptions)))

    ;; Pad the slot-descriptions with the initial-offset number of NILs.
    (when (and type initial-offset)
          (setq slot-descriptions
                (append (make-list initial-offset) slot-descriptions)))

    ;; Append the slot-descriptions of the included structure.
    ;; The slot-descriptions in the include option are also counted.
    (cond ((null include))
          ((endp (cdr include))
           (setq slot-descriptions
                 (append (get-sysprop (car include) 'STRUCTURE-SLOT-DESCRIPTIONS)
                         slot-descriptions)))
          (t
           (setq slot-descriptions
                 (append (overwrite-slot-descriptions
                          (mapcar #'(lambda (sd)
                                      (parse-slot-description sd 0 :unknown))
                                  (cdr include))
                          (get-sysprop (car include) 'STRUCTURE-SLOT-DESCRIPTIONS))
                         slot-descriptions))))

    (cond (no-constructor
           ;; If a constructor option is NIL,
           ;;  no constructor should have been specified.
           (when constructors
                 (error "Contradictory constructor options.")))
          ((null constructors)
           ;; If no constructor is specified,
           ;;  the default-constructor is made.
           (setq constructors (list default-constructor))))

    ;; Check the named option and set the predicate.
    (if (and type (not named))
        (progn
;;        (when predicate-specified
          (unless (or (not predicate-specified) (null predicate))
            (error "~S is an illegal structure predicate." predicate))
          (setq predicate nil))
      (unless predicate-specified
        (setq predicate (intern (concatenate-base-strings (string name) "-P")))))

    (when include (setq include (car include)))

    ;; Check the print-function.
    (when (and (or print-function print-object) type)
      (error "An print function is supplied to a typed structure."))

    (let ((core `(define-structure ',name ',conc-name ',type ',named ',slots
		   ',slot-descriptions ',copier ',include
		   ',print-function ',print-object ',constructors
		   ',offset ',name-offset
		   ',documentation ',predicate
		   ',(copy-list si:*source-location*)
		   ))
	  (constructors (mapcar #'(lambda (constructor)
				    (make-constructor name constructor type named slot-descriptions))
				constructors)))
      `(progn
         (define-when (:compile-toplevel :load-toplevel :execute)
           ,core)
	 ,(si::register-with-pde whole)
         (let ,(unless type (when constructors `((.structure-constructor-class. (find-class ',name)))))
           ,@constructors)
	 ',name)
      )))

