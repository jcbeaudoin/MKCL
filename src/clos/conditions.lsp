;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: CLOS -*-
;;;;
;;;;  Copyright (c) 2010-2014, Jean-Claude Beaudoin.
;;;;  Copyright (c) 2001, Juan Jose Garcia-Ripoll
;;;;  Copyright (c) 1992, Giuseppe Attardi.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Lesser General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 3 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../../Copyright' for full details.
;;;
;;; conditions.lsp
;;;
;;; Originally written by Kent M. Pitman of Symbolics, Inc. and
;;; distributed without any copyright.
;;; This is Version 18.
;;;
;;; KMP's disclaimer:
;;;
;;; This is a sample implementation. It is not in any way intended as the
;;; definition of any aspect of the condition system. It is simply an existence
;;; proof that the condition system can be implemented.
;;;

(in-package "SYSTEM")

;;; ----------------------------------------------------------------------
;;; Unique Ids

(defmacro unique-id (obj)
  "Generates a unique integer ID for its argument."
  `(si:pointer ,obj))


;;; Restarts

(defvar *restart-clusters* ())
(defvar *condition-restarts* ())

(defun restart-print (restart stream depth)
  (declare (ignore depth))
  (if *print-escape*
      (format stream "#<~s.~d>" (type-of restart) (unique-id restart))
      (restart-report restart stream)))

(defstruct (restart (:PRINT-FUNCTION restart-print))
  name
  function
  report-function
  interactive-function
  (test-function (constantly t)))

(defun restart-report (restart stream)
  (let ((fn (restart-report-function restart)))
    (if fn
	(funcall fn stream)
	(format stream "~s" (or (restart-name restart) restart)))))

;;; Function compute-restarts is a key component of the debugger. Therefore it cannot
;;; afford the risk of itself signaling a unhandled condition that would then trigger
;;; a recursive invocation of the debugger since this would most probably be the start
;;; of an infinite recursion. Exhaustion of resources and a core dump are then the most
;;; likely outcome. JCB
(defun compute-restarts (&optional condition)
  (let* ((assoc-restart ())
	 (other ())
	 (output ())
         (output-last nil))
    #+(or) ;; This is the naive optimist version. JCB
    (when condition
      (dolist (i *condition-restarts*)
	(if (eq (first i) condition)
	    (setq assoc-restart (append (rest i) assoc-restart))
	  (setq other (append (rest i) other)))))
    #+(and) ;; This is the more defensive version. JCB
    (when (and condition (consp *condition-restarts*))
      (do ((sub-set *condition-restarts* (and (consp sub-set) (cdr sub-set)))
	   )
	  ((or (null sub-set) (not (consp sub-set))))
	(let ((i (car sub-set)))
	  (when (consp i)
	    (if (eq (first i) condition)
		(setq assoc-restart (append (rest i) assoc-restart))
	      (setq other (append (rest i) other)))))))

    #+(or) ;; This is the naive optimist version. JCB
    (dolist (restart-cluster *restart-clusters* (setq output (nreverse output)))
      (dolist (restart restart-cluster)
	(when (and (or (not condition)
		       (member restart assoc-restart)
		       (not (member restart other)))
		   (funcall (restart-test-function restart) condition))
	  (push restart output))))
    #+(and) ;; This is the more defensive version. JCB
    (when (consp *restart-clusters*)
      (do* ((restart-clusters *restart-clusters* (and (consp restart-clusters) (cdr restart-clusters))))
	   ((or (null restart-clusters) (not (consp restart-clusters))))
	(let ((restart-cluster (car restart-clusters)))
	  (when (consp restart-cluster)
	    (do* ((sub-cluster restart-cluster (and (consp sub-cluster) (cdr sub-cluster)))
                  (cluster-output ())
                  (cluster-output-last nil))
		 ((or (null sub-cluster) (not (consp sub-cluster)))
                  (when cluster-output
                    (if output-last
                        (setf (cdr output-last) cluster-output)
                      (setq output cluster-output))
                    (setq output-last cluster-output-last)))
	      (let ((restart (car sub-cluster)))
		(when (and (restart-p restart)
			   (or (not condition)
			       (member restart assoc-restart)
			       (not (member restart other)))
			   (funcall (restart-test-function restart) condition))
                  (let ((it (cons restart nil)))
                    (if cluster-output-last
                        (rplacd cluster-output-last it)
                      (setq cluster-output it))
                    (setq cluster-output-last it))))))))) 
    output))

(defmacro restart-bind (bindings &body forms)
  `(let* ((*dynamic-cons-stack* *dynamic-cons-stack*)
	  (*restart-clusters*
	   (dyn-cons (dyn-list ,@(mapcar #'(lambda (binding)
					     `(make-restart
					       :NAME     ',(car binding)
					       :FUNCTION ,(cadr binding)
					       ,@(cddr binding)))
					 bindings))
		     *restart-clusters*)))
     (declare (special *restart-clusters*))
     ,@forms))


(defun find-restart (name &optional condition)
  (dolist (restart (compute-restarts condition))
    (when (or (eq restart name) (eq (restart-name restart) name))
      (return-from find-restart restart))))

(defun find-restart-never-fail (restart &optional condition)
  (or (find-restart restart condition)
      (error 'simple-control-error :format-control "Restart ~S is not active." :format-arguments (list restart))))

(defun invoke-restart (restart &rest values)
  (let ((real-restart (find-restart-never-fail restart)))
    (apply (restart-function real-restart) values)))

(defun invoke-restart-interactively (restart)
  (let ((real-restart (find-restart-never-fail restart)))
    (apply (restart-function real-restart)
	   (let ((interactive-function
		   (restart-interactive-function real-restart)))
	     (if interactive-function
		 (funcall interactive-function)
		 '())))))


(defmacro restart-case (expression &body clauses &environment env)
  (flet ((transform-keywords (&key report interactive test)
	   (let ((keywords '()))
	     (when test
	       (setq keywords (list :TEST-FUNCTION `#',test)))				    
	     (when interactive
	       (setq keywords (list :INTERACTIVE-FUNCTION
				    `#',interactive)))
	     (when report
	       (setq keywords (list* :REPORT-FUNCTION
				     (if (stringp report)
					 `#'(lambda (stream)
					      (write-string ,report stream))
					 `#',report)
				     keywords)))
	     keywords)))
    (let*((block-tag (gensym))
	  (temp-var  (gensym))
	  (data (mapcar #'(lambda (clause)
			    (let (keywords (forms (cddr clause)))
			      (do ()
				  ((null forms))
				(if (keywordp (car forms))
				    (setq keywords (list* (car forms)
							  (cadr forms)
							  keywords)
					  forms (cddr forms))
				    (return)))
			      (list (car clause) 		;Name=0
				    (gensym) 			;Tag=1
				    (apply #'transform-keywords ;Keywords=2
					   keywords)
				    (cadr clause)		;BVL=3
				    forms))) 			;Body=4
			clauses)))
      (let ((expression2 (macroexpand expression env)))
	(when (consp expression2)
	  (let* ((condition-form nil)
		 (condition-var (gensym))
		 (name (first expression2)))
	    (case name
	      (SIGNAL
	       (setq condition-form `(coerce-to-condition ,(second expression2)
                                      (list ,@(cddr expression2))
                                      'SIMPLE-CONDITION 'SIGNAL)))
	      (ERROR
	       (setq condition-form `(coerce-to-condition ,(second expression2)
				      (list ,@(cddr expression2))
				      'SIMPLE-ERROR 'ERROR)))
	      (CERROR
	       (setq condition-form `(coerce-to-condition ,(third expression2)
				      (list ,@(cdddr expression2))
				      'SIMPLE-ERROR 'CERROR)))
	      (WARN
	       (setq condition-form `(coerce-to-condition ,(second expression2)
				      (list ,@(cddr expression2))
				      'SIMPLE-WARNING 'WARN))))
	    (when condition-form
	      (setq expression
		    `(let ((,condition-var ,condition-form))
		      (with-condition-restarts ,condition-var
			(first *restart-clusters*)
			,(if (eq name 'CERROR)
			     `(cerror ,(second expression2) ,condition-var)
			     (list name condition-var)))))
	      ))))
      `(block ,block-tag
	 (let ((,temp-var nil))
	   (tagbody
	     (restart-bind
	       ,(mapcar #'(lambda (datum)
			    (let*((name (nth 0 datum))
				  (tag  (nth 1 datum))
				  (keys (nth 2 datum)))
			      `(,name #'(lambda (&rest temp)
					  (setq ,temp-var temp)
					  (go ,tag))
				,@keys)))
			data)
	       (return-from ,block-tag ,expression))
	     ,@(mapcan #'(lambda (datum)
			   (let*((tag  (nth 1 datum))
				 (bvl  (nth 3 datum))
				 (body (nth 4 datum)))
			     (list tag
				   `(return-from ,block-tag
				      (apply #'(lambda ,bvl ,@body)
					     ,temp-var)))))
		       data)))))))

(defmacro with-simple-restart ((restart-name format-control
					     &rest format-arguments)
			       &body forms)
  `(restart-case (progn ,@forms)
     (,restart-name ()
        :REPORT (lambda (stream)
		  (format stream ,format-control ,@format-arguments))
      (values nil t))))

(defmacro with-condition-restarts (condition restarts &body forms)
  `(let ((*condition-restarts* (cons (cons ,condition ,restarts)
				     *condition-restarts*)))
    ,@forms))


;;; ----------------------------------------------------------------------
;;; Condition Data Type

(defclass condition () ())

(defmacro define-condition (name parent-list slot-specs &rest options)
  (let* ((report-function nil)
	 (documentation nil)
	 (default-initargs nil))
    (dolist (option options)
      (case (car option)
	(:DEFAULT-INITARGS (push option default-initargs))
	(:REPORT (setq report-function (cadr option)))
	(:DOCUMENTATION (setq documentation (cadr option)))
	(otherwise (cerror "Ignore this DEFINE-CONDITION option."
			   "Invalid DEFINE-CONDITION option: ~S" option))))
    `(PROGN
      (DEFCLASS ,name ,(or parent-list '(CONDITION)) ,slot-specs ,@default-initargs)
      ,@(when report-function
	      `((defmethod print-object ((X ,name) stream)
		    (if *print-escape*
		      (call-next-method)
		      ,(if (stringp report-function)
			   `(write-string ,report-function stream)
			   `(,report-function x stream))))))
      ,@(when documentation
	      `((EVAL-WHEN (COMPILE LOAD EVAL)
		  (SETF (DOCUMENTATION ',name 'TYPE) ',documentation))))
      ',NAME)))

(defun find-subclasses-of-type (type class)
  ;; Find all subclasses of CLASS that are subtypes of the given TYPE.
  (if (subtypep class type)
      (list class)
      (loop for c in (clos::class-direct-subclasses class)
	    nconc (find-subclasses-of-type type c))))

(defun make-condition (type &rest slot-initializations)
  ;;(declare (dynamic-extent slot-initializations))
  (let ((class (or (and (symbolp type) (find-class type nil))
		   (first (last (sort (find-subclasses-of-type type (find-class 'condition))
				      #'si::subclassp))))))
    (unless class
      (error 'SIMPLE-TYPE-ERROR
	     :DATUM type
	     :EXPECTED-TYPE 'CONDITION
	     :FORMAT-CONTROL "Not a condition type: ~S"
	     :FORMAT-ARGUMENTS (list type)))
    (apply #'make-instance class slot-initializations)))



(defvar *handler-clusters* nil)

(defmacro handler-bind (bindings &body forms)
  (unless (every #'(lambda (x) (and (listp x) (= (length x) 2))) bindings)
    (error "Ill-formed handler bindings."))
  `(let* ((*dynamic-cons-stack* *dynamic-cons-stack*)
	  (*handler-clusters*
	   (dyn-cons (dyn-list ,@(mapcar #'(lambda (x) `(cons ',(car x) ,(cadr x)))
					 bindings))
		     *handler-clusters*)))
     ,@forms))



(defun signal (datum &rest arguments)
  (let* ((condition (coerce-to-condition datum arguments 'SIMPLE-CONDITION 'SIGNAL))
	 (*handler-clusters* *handler-clusters*))
    (if (typep condition *break-on-signals*)
	(break "~A~%Break entered because of *BREAK-ON-SIGNALS*."
	       condition))
    (loop (unless *handler-clusters* (return))
          (let ((cluster (pop *handler-clusters*)))
	    (dolist (handler cluster)
	      (when (typep condition (car handler))
		(funcall (cdr handler) condition)
		))))
    nil))



;;; COERCE-TO-CONDITION
;;;  Internal routine used in ERROR, CERROR, BREAK, and WARN for parsing the
;;;  hairy argument conventions into a single argument that's directly usable 
;;;  by all the other routines.

(defun coerce-to-condition (datum arguments default-type function-name)
  (cond ((typep datum 'CONDITION)
	 (when arguments
	   (cerror "Ignore the additional arguments."
		   'SIMPLE-TYPE-ERROR
		   :DATUM arguments
		   :EXPECTED-TYPE 'NULL
		   :FORMAT-CONTROL "You may not supply additional arguments ~
				     when giving ~S to ~S."
		   :FORMAT-ARGUMENTS (list datum function-name)))
	 datum)
        ((symbolp datum)                  ;roughly, (subtypep datum 'CONDITION)
         (apply #'make-condition datum arguments))
        ((or (stringp datum) (functionp datum))
	 (make-condition default-type ;; must be a subclass of simple-condition
                         :FORMAT-CONTROL datum
                         :FORMAT-ARGUMENTS arguments))
        (t
         (error 'SIMPLE-TYPE-ERROR
		:DATUM datum
		:EXPECTED-TYPE '(OR SYMBOL STRING)
		:FORMAT-CONTROL "Bad argument to ~S: ~S"
		:FORMAT-ARGUMENTS (list function-name datum)))))

(defun break (&optional (format-control "Break") &rest format-arguments)
  "Enters a break loop.  The execution of the program can be resumed by typing
:CONTINUE at the break loop.  Type :HELP to see the break-loop commands list.
If FORMAT-STRING is non-NIL, it is used as the format string to be output to
*ERROR-OUTPUT* before entering the break loop.  ARGs are arguments to the
format string."
  (with-simple-restart (continue "Return from BREAK.")
    (invoke-debugger
      (make-condition 'SIMPLE-CONDITION
		      :FORMAT-CONTROL    format-control
		      :FORMAT-ARGUMENTS format-arguments)))
  nil)

(defun warn (datum &rest arguments)
  "Args: (format-string &rest args)
Formats FORMAT-STRING and ARGs to *ERROR-OUTPUT* as a warning message.  Enters
a break level if the value of *BREAK-ON-WARNINGS* is non-NIL.  Otherwise,
returns with NIL."
  (let ((condition
	  (coerce-to-condition datum arguments 'SIMPLE-WARNING 'WARN)))
    (check-type condition warning "a warning condition")
    (restart-case (signal condition)
      (muffle-warning ()
	  :REPORT "Skip warning."
	(return-from warn nil)))
    (format *error-output* "~&;;; Warning: ~A~%" condition)
    nil))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-condition warning () ())

(define-condition serious-condition () ())

(define-condition error (serious-condition) ())

(define-condition simple-condition ()
  ((format-control :INITARG :FORMAT-CONTROL :INITFORM ""
		   :ACCESSOR simple-condition-format-control)
   (format-arguments :INITARG :FORMAT-ARGUMENTS :INITFORM NIL
		     :ACCESSOR simple-condition-format-arguments))
  (:REPORT
   (lambda (condition stream)
     (format stream "~?" (simple-condition-format-control condition)
	     (simple-condition-format-arguments condition)))))

(define-condition simple-warning (simple-condition warning) ())

(define-condition style-warning (warning) ())

(define-condition mkcl:simple-style-warning (style-warning simple-condition) ())

(define-condition simple-error (simple-condition error) ())

(define-condition storage-condition (serious-condition) ())

(define-condition mkcl:segmentation-violation (storage-condition)
  ((address :INITARG :ADDRESS :INITFORM "0x?????????"
	    :READER mkcl:segmentation-violation-address))
  (:REPORT
   (lambda (condition stream)
     (format stream 
	     "Detected access to an invalid or protected memory address at (~A)."
	     (mkcl:segmentation-violation-address condition)))))


(define-condition mkcl:stack-overflow (storage-condition)
  ((size :initarg :size :initform 0 :reader mkcl:stack-overflow-size)
   (type :initarg :type :initform nil :reader mkcl:stack-overflow-type))
  (:REPORT
   (lambda (condition stream)
     (let* ((type (mkcl::stack-overflow-type condition))
	    (size (mkcl::stack-overflow-size condition)))
       (if size
	   (format stream "~A overflow, stack size = ~D." type size)
	 (format stream "~A stack overflow." type))))))

(define-condition mkcl:storage-exhausted (storage-condition) ()
  (:REPORT
   (lambda (condition stream)
     (format stream "Memory limit reached. Please jump to an outer pointer, quit program and enlarge the
memory limits before executing the program again."))))

(define-condition type-error (error)
  ((datum :INITARG :DATUM :initform :invalid-value :READER type-error-datum)
   (expected-type :INITARG :EXPECTED-TYPE :initform t :READER type-error-expected-type))
  (:REPORT
   (lambda (condition stream)
     (format stream "~S is not of type ~S."
	     ;; There is a circularity on access to slot datum if ever
	     ;; it gets assigned UNBOUND
	     (if (slot-boundp condition 'datum)
		 (type-error-datum condition)
	       si:unbound)
	     (type-error-expected-type condition)))))

(define-condition simple-type-error (simple-condition type-error) ())

(define-condition case-failure (type-error)
  ((name :INITARG :NAME :READER case-failure-name)
   (possibilities :INITARG :POSSIBILITIES :READER case-failure-possibilities))
  (:REPORT
   (lambda (condition stream)
     (format stream "~S fell through ~S expression.~%Wanted one of ~:S."
	     (type-error-datum condition)
	     (case-failure-name condition)
	     (case-failure-possibilities condition)))))

(define-condition program-error (error) ())

(define-condition simple-program-error (simple-error program-error) ())

(define-condition control-error (error) ())

(define-condition simple-control-error (simple-error control-error) ())

(define-condition stream-error (error)
  ((stream :INITARG :STREAM :READER stream-error-stream)))

(define-condition end-of-file (stream-error)
  ()
  (:REPORT (lambda (condition stream)
	     (format stream "Stream error: Unexpected end of file on ~S."
		     (stream-error-stream condition)))))

(define-condition si::closed-stream-error (stream-error)
  ()
  (:REPORT (lambda (condition stream)
	     (format stream "Stream error: Attempt to operate on closed stream ~S."
		     (stream-error-stream condition)))))

(define-condition si::OS-stream-error (simple-condition stream-error) ;; JCB
  ()
  (:REPORT
   (lambda (condition stream)
     (let ((*print-circle* nil))
       (format stream "Stream error: OS level error on stream ~S.~%" (stream-error-stream condition)))
     (format stream "~?" (simple-condition-format-control condition)
	     (simple-condition-format-arguments condition))
     )))

(define-condition file-error (error)
  ((pathname :INITARG :PATHNAME :READER file-error-pathname))
  (:REPORT (lambda (condition stream)
	     (format stream "Filesystem error with pathname ~S.~%Either
 1) the file does not exist, or
 2) you are not allowed to access the file, or
 3) the pathname designates a broken symbolic link."
		     (file-error-pathname condition)))))

(define-condition si::OS-file-error (simple-condition file-error) ;; JCB
  ()
  (:REPORT
   (lambda (condition stream)
     (let ((*print-circle* nil))
       (format stream "Filesystem error with pathname: ~S.~%" (file-error-pathname condition)))
     (format stream "~?" (simple-condition-format-control condition)
	     (simple-condition-format-arguments condition))
     )))

(define-condition package-error (error)
  ((package :INITARG :PACKAGE :READER package-error-package)))

(define-condition simple-package-error (simple-error package-error) ())

(define-condition cell-error (error)
  ((name :INITARG :NAME :READER cell-error-name)))

(define-condition unbound-variable (cell-error)
  ()
  (:REPORT (lambda (condition stream)
	     (format stream "The variable ~S is unbound."
		     (cell-error-name condition)))))
  
(define-condition unbound-slot (cell-error)
  ((instance :INITARG :INSTANCE :READER unbound-slot-instance))
  (:REPORT (lambda (condition stream)
	     (format stream "The slot ~S in the object of class ~S is unbound."
		     (cell-error-name condition)
		     (class-name (class-of (unbound-slot-instance condition)))))))

(define-condition mkcl:invalid-slot (cell-error)
  ((instance :INITARG :INSTANCE :READER invalid-slot-instance))
  (:REPORT (lambda (condition stream)
	     (format stream
		     "The index ~S (max: ~S) is not a valid slot index in the object ~S."
		     (cell-error-name condition)
		     (1- (si:instance-length (invalid-slot-instance condition)))
		     (invalid-slot-instance condition)))))

(define-condition undefined-function (cell-error)
  ()
  (:REPORT (lambda (condition stream)
	     (format stream "The function ~S is undefined."
		     (cell-error-name condition)))))

(define-condition arithmetic-error (error)
  ((operation :INITARG :OPERATION :READER arithmetic-error-operation)
   (operands :INITARG :OPERANDS :INITFORM '() :READER arithmetic-error-operands)))

(define-condition division-by-zero         (arithmetic-error) ())

(define-condition floating-point-overflow  (arithmetic-error) ())

(define-condition floating-point-underflow (arithmetic-error) ())

(define-condition floating-point-inexact (arithmetic-error) ())

(define-condition floating-point-invalid-operation (arithmetic-error) ())

(define-condition abort-failure (control-error) ()
  (:REPORT (lambda (c s) (declare (ignore c))
	     (write-string "Abort failed." s))))

(define-condition print-not-readable (error)
  ((object :INITARG :OBJECT :READER print-not-readable-object))
  (:REPORT (lambda (condition stream)
	     (format stream "Cannot print object ~A readably."
		     (print-not-readable-object condition)))))

(define-condition parse-error (error) ())

(define-condition reader-error (parse-error stream-error) ())

;;(define-condition simple-reader-error (simple-error reader-error) ())
(define-condition simple-reader-error (simple-condition reader-error) ()) ;; JCB


(define-condition format-error (simple-error)
  ((format-control :initarg :complaint)
   (format-arguments :initarg :arguments)
   (control-string :reader format-error-control-string
		   :initarg :control-string
		   :initform *default-format-error-control-string*) 
   (offset :reader format-error-offset :initarg :offset
	   :initform *default-format-error-offset*)
   (print-banner :reader format-error-print-banner :initarg :print-banner
		 :initform t))
  (:report (lambda (condition stream)
	     (cl:format stream
			"~:[~;Error in format: ~]~
			 ~?~@[~%  ~A~%  ~V@T^~]"
			(format-error-print-banner condition)
			(simple-condition-format-control condition)
			(simple-condition-format-arguments condition)
			(format-error-control-string condition)
			(format-error-offset condition)))))

(define-condition mkcl:interactive-interrupt (serious-condition)
  ()
  (:report (lambda (condition stream)
	     (declare (ignore condition))
	     (format stream "~&~@<Console interrupt~:@>")
	     ;(format stream "~&Console interrupt")
	     )))

(define-condition mkcl:bad-fasl-file (file-error)
  ((reason :initarg :reason :reader bad-fasl-file-reason))
  (:report (lambda (condition stream)
	     (format stream "FASL error (~A), pathname: ~S."
		     (bad-fasl-file-reason condition)
		     (file-error-pathname condition)))))


;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	   


(defmacro handler-case (form &rest cases)
  (let ((no-error-clause (assoc ':NO-ERROR cases)))
    (if no-error-clause
	(let* ((normal-return (make-symbol "NORMAL-RETURN"))
	       (error-return  (make-symbol "ERROR-RETURN")))
	  `(block ,error-return
	    (multiple-value-call #'(lambda ,@(cdr no-error-clause))
	      (block ,normal-return
		(return-from ,error-return
		  (handler-case (return-from ,normal-return ,form)
		     ,@(remove no-error-clause cases)))))))
	(let* ((tag (gensym))
	       (var (gensym))
	       (annotated-cases (mapcar #'(lambda (case) (cons (gensym) case))
					cases)))
	  `(block ,tag
	     (let ((,var nil))
	       (declare (ignorable ,var))
	       (tagbody
		 (handler-bind 
		  ,(mapcar 
		    #'(lambda (annotated-case)
			(list (cadr annotated-case)
			      `#'(lambda (temp)
				   (setq ,var temp)
				   (go ,(car annotated-case)))))
		    annotated-cases)
		  (return-from ,tag ,form))
		 ,@(mapcan #'(lambda (annotated-case)
			       (list (car annotated-case)
				     (let ((body (cdddr annotated-case)))
				       `(return-from ,tag
					  ,(if (caddr annotated-case)
					       `(let ((,(caaddr annotated-case)
						       ,var))
						 ,@body)
					       ;; We must allow declarations!
					       `(locally ,@body))))))
			   annotated-cases))))))))

(defmacro ignore-errors (&rest forms)
  `(handler-case (progn ,@forms)
     (error (condition) (values nil condition))))

(defun abort (&optional c)
  (invoke-restart (find-restart-never-fail 'ABORT c))
  (error 'ABORT-FAILURE))

(defun continue (&optional c)
  (let ((restart (find-restart 'CONTINUE c)))
    (and restart (invoke-restart restart))))

(defun muffle-warning (&optional c)
  (invoke-restart (find-restart-never-fail 'MUFFLE-WARNING c)))

(defun store-value (value &optional c)
  (let ((restart (find-restart 'STORE-VALUE c)))
    (and restart (invoke-restart restart value))))

(defun use-value (value &optional c)
  (let ((restart (find-restart 'USE-VALUE c)))
    (and restart (invoke-restart restart value))))


;;; ----------------------------------------------------------------------
;;; MKCL's interface to the toplevel and debugger

(defun try-to-invoke-debugger (condition)
  (if (fboundp 'invoke-debugger)
      (invoke-debugger condition)
    (progn
      (finish-output)
      (let ((stderr *error-output*))
	(terpri stderr)
	(terpri stderr)
	(princ "Debugger called in: " stderr)
	(prin1 mt:*thread* stderr)
	(write-char #\. stderr)
	(terpri stderr)
	(princ "MKCL Fatal: the debugger is missing, bailing out!" stderr)
	(terpri stderr)
	(princ condition stderr)
	(terpri stderr))
      (finish-output)
      #|
      ;; mt:exit-thread would be the thing to do here if "deamon" threads were not
      ;; likely to keep us hung up lifeless. We do not distinguish "daemon" threads
      ;; from normal threads YET. Fix it one day... JCB
      (mt:exit-thread :killed-by-debugger)
      |#
      (if (fboundp 'mkcl::quit)
	  (mkcl::quit :exit-code 1)
	(mt:abandon-thread 1))
      )
    )
  )

(defvar *default-universal-error-handler-maximum-depth* 10)
(defvar *universal-error-handler-stack* nil)
(defvar *universal-error-handler-level* 0)

(defun sys::universal-error-handler (datum args
                                     &aux (*universal-error-handler-level* (1+ *universal-error-handler-level*)))
  "Args: (datum args)
MKCL specific.
Starts the error handler of MKCL.
When an error is detected, MKCL calls this function with the specified
arguments.  To change the error handler of MKCL, redefine this function.
"
  (let* ((condition (coerce-to-condition datum args 'simple-error 'error))
	 (*universal-error-handler-stack* (cons condition *universal-error-handler-stack*))
	 )
    (when (<= *default-universal-error-handler-maximum-depth* *universal-error-handler-level*)
      (ignore-errors
       (format *error-output*
	       "~&Excessive universal-error-handler depth! Probable infinite recursion!~%~
             Quitting thread: ~S.~%" mt:*thread*)
       (dolist (c *universal-error-handler-stack*)
	 (format *error-output* "~A~%" c)
	 )
       )
      (when (<= (+ *default-universal-error-handler-maximum-depth* 3) *universal-error-handler-level*)
	;; we tried to be polite but it does not seem to work.
	(if (fboundp 'mkcl::quit)
	    (mkcl::quit :exit-code 1)
	  (mt:abandon-thread 1)))
      #|
      ;; mt:exit-thread would be the thing to do here if "deamon" threads were not
      ;; likely to keep us hung up lifeless. We do not distinguish "daemon" threads
      ;; from normal threads YET. Fix it one day... JCB
      (mt:exit-thread :killed-by-debugger)
      |#
      (if (fboundp 'mkcl::quit)
	  (mkcl::quit :exit-code 1)
	(mt:abandon-thread 1))
      )
    (signal condition)
    (try-to-invoke-debugger condition)))

(defun cerror (continue-format-control datum &rest args)
  ;;(declare (:c-export "mk_cl_cerror"))
  (if (or (stringp continue-format-control) (functionp continue-format-control))
      (with-simple-restart
       (continue "~A" (apply #'format nil continue-format-control args))
       (apply #'error datum args))
    (progn
      (warn "In cerror: Ignoring what is not a proper format-control value for continue-format-control: ~S" continue-format-control)
      (apply #'error datum args)
      )
    )
  nil)



(defun sys::tpl-continue-command (&rest any) ;; any use? JCB
  (apply #'invoke-restart 'continue any))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; Switch to new class redefinition semantics from this point on.
;;;
;;#-mkcl-min (setq clos::*redefine-class-in-place* nil)

