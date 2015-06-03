;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: C -*-
;;;;
;;;; CMPUTIL  --  Miscellaneous Functions.

;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;  Copyright (c) 2010-2012, Jean-Claude Beaudoin.
;;;;
;;;;    ECoLisp is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Lesser General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 3 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../../Copyright' for full details.

(in-package "COMPILER")

(define-condition compiler-message (simple-condition)
  ((prefix :initform "Note" :accessor compiler-message-prefix)
   (file :initarg :file :initform *compile-file-pathname*
	 :accessor compiler-message-file)
   (end-position :initarg :end-position :initform *compile-file-end-position*
		 :accessor compiler-message-file-end-position)
   (form :initarg :form :initform *original-current-form*
	 :accessor compiler-message-form))
  (:REPORT
   (lambda (c stream)
     (let ((position (compiler-message-file-end-position c)))
       (if (< 0 position)
	   (let ((*print-length* 3)
		 (*print-level* 2)
		 )
	     (format stream "~A: in file ~A, end position ~D,~%    and form: ~A~%"
		     (compiler-message-prefix c)
		     (compiler-message-file c) position (compiler-message-form c)))
	   (format stream "~A: " (compiler-message-prefix c)))
       (format stream "  ~?"
	       (simple-condition-format-control c)
	       (simple-condition-format-arguments c))))))

(define-condition compiler-note (compiler-message) ())

(define-condition compiler-warning (compiler-message warning)
  ((prefix :initform "Warning")))

(define-condition compiler-style-warning (compiler-message style-warning)
  ((prefix :initform "Style Warning")))

(define-condition compiler-error (compiler-message error)
  ((prefix :initform "Error")))

(define-condition compiler-fatal-error (compiler-error) ())

(define-condition compiler-internal-error (compiler-fatal-error)
  ((prefix :initform "Internal error")))

(define-condition compiler-undefined-variable (compiler-style-warning)
  ((prefix :initform "Style Warning")
   (variable :initarg :name :initform nil))
  (:report
   (lambda (condition stream)
     (let ((position (compiler-message-file-end-position condition)))
       (if (< 0 position)
	   (let ((*print-length* 3)
		 (*print-level* 2)
		 )
	     (format stream "~A: in file ~A, end position ~D,~%   and form: ~A~%"
		     (compiler-message-prefix condition)
		     (compiler-message-file condition)
		     position 
		     (compiler-message-form condition)))
	 (format stream "~A: " (compiler-message-prefix condition)))
       (format stream "Variable ~A was undefined. Compiler assumes it is a global."
	       (slot-value condition 'variable))))))

(defun print-compiler-message (c stream)
  (unless (typep c *suppress-compiler-messages*)
    (format stream "~&~@<;;; ~@;~A~:>" c)
    ;;(format stream "~&;;; ~A" c)
    ;;(print ";;; " stream) (princ c stream)
    ))

(defun handle-note (c)
  (declare (ignore c))
  nil)

(defun handle-warning/error (c)
  (push c *compiler-conditions*)
  nil)

(defun handle-internal-error (c)
  (push c *compiler-conditions*)
  (invoke-debugger c) ;; give *debugger-hook* a chance to handle this
  (unless (typep c 'compiler-error)
    (signal 'compiler-internal-error
	    :format-control "~A"
	    :format-arguments (list c))
    (print-compiler-message c t)
    (abort)))

(defun do-compilation-unit (closure &key override)
  (cond (override
	 (let* ((*active-protection* nil))
	   (do-compilation-unit closure)))
	((null *active-protection*)
	 (let* ((*active-protection* t)
		(*pending-actions* nil))
	   (unwind-protect (do-compilation-unit closure)
	     (loop for action in *pending-actions*
		do (funcall action)))))
	(t
	 (funcall closure))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (si::reopen-package :cl))
(defmacro cl:with-compilation-unit ((&rest options) &body body)
 `(si:without-fpe *compiler-floating-point-exclusion-set*
     (do-compilation-unit #'(lambda () ,@body) ,@options)))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (si::close-package :cl))

(defun compiler-debugger (outer-hook condition old-hook)
  (declare (ignore old-hook))
  (when *compiler-break-enable*
    (when outer-hook
      (funcall outer-hook condition outer-hook))
    (si::default-debugger condition))
  (cmperr "~A" condition) ;; Wrap condition in a compiler-error to give it source context.
  (abort))

(defmacro with-compiler-env ((compiler-conditions) &body body)
  `(let ((*compiler-conditions* nil))
     (declare (special *compiler-conditions*))
     (restart-case
	 (handler-bind (((and error (not compiler-message)) #'handle-internal-error))
	   (handler-bind ((compiler-note #'handle-note)
			  (warning #'handle-warning/error)
			  (compiler-error #'handle-warning/error)
			  ((and compiler-message
				(not (or compiler-note warning compiler-error)))
			   #'handle-note) ;; not really expected...
			  )
             (mt:with-lock (mt::+load-compile-lock+)
	        (let ,+init-env-form+
                  (with-compilation-unit ()
                     ,@body)))))
       (abort (&optional c)
	      :report "Abort compilation."
	      (push (if c c :aborted) *compiler-conditions*)
	      (format t "~&;;; Compilation failed!~%")))
     (setf ,compiler-conditions *compiler-conditions*)))

(defvar *c1form-level* 0)
(defun print-c1forms (form)
  (cond ((consp form)
	 (let ((*c1form-level* (1+ *c1form-level*)))
	   (mapc #'print-c1forms form)))
	((c1form-p form)
	 (format t "~% ~D > ~A, parent ~A" *c1form-level* form (c1form-parent form))
	 (print-c1forms (c1form-args form))
	 form
	 )))

(defun print-ref (ref-object stream)
  (let ((name (ref-name ref-object)))
    (if name
	(format stream "#<a ~A: ~A>" (type-of ref-object) name)
	(format stream "#<a ~A>" (type-of ref-object)))))

(defun print-var (var-object stream)
  (format stream "#<a VAR: ~A KIND: ~A>" (var-name var-object) (var-kind var-object)))

(defun cmpprogress (&rest args)
  (when *compile-verbose*
    (apply #'format t args)))

(defun cmperr (string &rest args)
  (let ((c (make-condition 'compiler-error
			   :format-control string
			   :format-arguments args)))
    (signal c)
    (print-compiler-message c t)
    (abort)))

(defun check-args-number (operator args &optional (min 0) (max nil))
  (let ((l (length args)))
    (when (< l min)
      (too-few-args operator min l))
    (when (and max (> l max))
      (too-many-args operator max l))))

(defun too-many-args (name upper-bound n &aux (*print-case* :upcase))
  (cmperr "~S requires at most ~R argument~:p, but ~R ~:*~[were~;was~:;were~] supplied.~%"
          name
          upper-bound
          n))

(defun too-few-args (name lower-bound n)
  (cmperr "~S requires at least ~R argument~:p, but only ~R ~:*~[were~;was~:;were~] supplied.~%"
          name
          lower-bound
          n))

(defun do-cmpwarn (&rest args)
  (let ((condition (apply #'make-condition args)))
    (restart-case (signal condition)
      (muffle-warning ()
	:REPORT "Skip warning"
	(return-from do-cmpwarn nil)))
    (print-compiler-message condition t)))

(defun cmpwarn-style (string &rest args)
  (unless *suppress-compiler-warnings*
    (do-cmpwarn 'compiler-style-warning :format-control string :format-arguments args)))

(defun cmpwarn (string &rest args)
  (unless *suppress-compiler-warnings*
    (do-cmpwarn 'compiler-warning :format-control string :format-arguments args)))

(defun cmpnote (string &rest args)
  (unless *suppress-compiler-notes*
    (do-cmpwarn 'compiler-note :format-control string :format-arguments args)))

(defun print-current-form ()
  (when *compile-print*
    (let ((*print-length* 2)
	  (*print-level* 2))
      (format t "~&;;; Compiling ~s.~%" *original-current-form*)))
  nil)

(defun print-emitting (f)
  (when *compile-print*
    (let* ((name (or (fun-name f) (fun-description f))))
      (when name
	(format t "~&;;; Emitting code for ~s.~%" name)))))

(defun undefined-variable (sym)
  (do-cmpwarn 'compiler-undefined-variable :name sym))
  
(defun baboon (&aux (*print-case* :upcase))
  (signal 'compiler-internal-error
	  :format-control "A bug was found in the compiler."
	  :format-arguments nil))
  
(defmacro with-cmp-protection (main-form error-form)
  `(let* ((si::*break-enable* *compiler-break-enable*)
          (throw-flag t))
     (unwind-protect
	 (multiple-value-prog1
             (if *compiler-break-enable*
                 (handler-bind (((and error (not compiler-error)) #'invoke-debugger))
                   ,main-form)
                 ,main-form)
           (setf throw-flag nil))
       (when throw-flag ,error-form)))) ;; this intercepts any non-local transfer! JCB

(defun cmp-eval (form)
  (with-cmp-protection (eval form)
    (cmperr "The form ~s was not evaluated successfully.~
~&You are recommended to compile again."
	    form)))

(defun cmp-macroexpand (form &optional (env *cmp-env*))
  (with-cmp-protection 
    (handler-case (macroexpand form env)
      (condition #|program-error|# (condition) ;; program-error was much too weak. JCB
	 (cmperr "Macro expansion of form: ~S~
                  ~%failed with this condition: ~A." form condition)))
    (cmperr "The macro form ~S was not expanded successfully.~
             ~%You are recommended to compile again." form)))
  
(defun cmp-expand-macro (fd form &optional (env *cmp-env*))
  (with-cmp-protection
    (let ((new-form 
	   (handler-case
	       (funcall *macroexpand-hook* fd form env)
	     (condition #|program-error|# (condition) ;; program-error was much too weak. JCB
	       (cmperr "Macro expansion of form: ~S~
                        ~%failed with this condition: ~A." form condition)))))
      (values new-form (not (eql new-form form))))
    (cmperr "The macro form ~S was not expanded successfully.~
             ~%You are recommended to compile again." form)))

#-(or mkcl-min mkcl-bootstrap mkcl-cmp-bootstrap)
(defun clear-compiler-properties (symbol)
  (rem-sysprop symbol 't1)
  (rem-sysprop symbol 't2)
  (rem-sysprop symbol 't3)
  (rem-sysprop symbol 'c1)
  (rem-sysprop symbol 'c2)
  (rem-sysprop symbol 'c1conditional)
  (rem-sysprop symbol ':inline-always)
  (rem-sysprop symbol ':inline-unsafe)
  (rem-sysprop symbol ':inline-safe)
  (rem-sysprop symbol 'lfun))
  
(defun lisp-to-c-function-name (obj)
  "Translate Lisp function name object prin1 representation to valid C function name"
  #+(or)
  (and obj 
       (map 'string 
            #'(lambda (c)
                (let ((cc (char-code c)))
                  (if (or (<= #.(char-code #\a) cc #.(char-code #\z))
                          (<= #.(char-code #\0) cc #.(char-code #\9))) ;; And you call that mangling! LOL JCB
                      c #\_)))
            (string-downcase (prin1-to-string obj))))
  #+(and)
  (typecase obj
    (string (si:mangle-string obj))
    (symbol (si:mangle-symbol obj))
    (t (si:mangle-string (prin1-to-string obj)))) ;; the (setf foo) case.
  )

(defun proper-list-p (x &optional test)
  #+(or)
  (and (listp x)
       (handler-case (list-length x) (type-error (c) nil))
       (or (null test) (every test x)))
  ;; This version traverses the list only once and does not create a closure. JCB
  (and (listp x)
       (do ((tail x (cdr tail)))
	   ((not (consp tail)) (null tail))
	 (if test (if (not (funcall test (car tail))) (return nil)))
	 )
       )
  )

