;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: SYSTEM -*-
;;;;
;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;  Copyright (c) 2010-2014, Jean-Claude Beaudoin
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Lesser General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 3 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../../Copyright' for full details.
;;;                           DESCRIBE and INSPECT

(in-package "SYSTEM")

(defvar *inspect-level* 0)
(defvar *inspect-history* nil)
(defvar *inspect-mode* nil)

(defvar *old-print-level* nil)
(defvar *old-print-length* nil)


;; Either the inspector reads and writes everything on *standard-output*,
;; or reads and writes everything on *query-io* but not a mix of each!
;; If this rule is not followed only severe confusion can result. JCB

(defun inspect-read-line ()
  (do ((char (read-char *query-io*) (read-char *query-io*)))
      ((or (char= char #\Newline) (char= char #\Return)))))

(defun select-P (object)
  (let* ((*print-pretty* t) (*print-level* nil) (*print-length* nil))
       (prin1 object)
       (terpri)))

(defun select-E (object)
  (let ((mkcl:! object))
    (dolist (x (multiple-value-list
		(multiple-value-prog1
		 (eval (read-preserving-whitespace *query-io*))
		 (inspect-read-line))))
      (write x
	     :level *old-print-level*
	     :length *old-print-length*)
      (terpri))))

(defun select-U ()
  (prog1
    (eval (read-preserving-whitespace *query-io*))
    (inspect-read-line)))

(defun select-? ()
  (terpri)
  (format t
	  "Inspect commands:~%~
                n (or N or Newline):    inspects the field (recursively).~%~
                s (or S):               skips the field.~%~
                p (or P):               pretty-prints the field.~%~
                a (or A):               aborts the inspection ~
                                        of the rest of the fields.~%~
                u (or U) form:          updates the field ~
                                        with the value of the form.~%~
                e (or E) form:          evaluates and prints the form ~
                                        with ! bound to the inspected object.~%~
                q (or Q):               quits the inspection.~%~
                ?:                      prints this.~%~%"))

(defun read-inspect-command (label object allow-recursive)
  (unless *inspect-mode*
    ;; This is "describe" mode. So we stay non-interactive.
    (inspect-indent-1)
    (if allow-recursive
        (progn (princ label) (inspect-object object))
        (format t label object))
    (return-from read-inspect-command nil))
  (let* ((*quit-tags* (cons *quit-tag* *quit-tag*)) ;; as seen in top.lsp
	 (*quit-tag* *quit-tags*))
    (declare (special *quit-tag* *quit-tags*))
    (loop
       (when
	   (catch *quit-tag* ;; as seen in top.lsp
	     (with-simple-restart (inspect "Go back to inspector.")
	       (inspect-indent-1)
	       (if allow-recursive
		   (progn (princ label)
			  (inspect-indent)
			  (prin1 object))
		   (format t label object))
	       (terpri)
	       (princ " I>> ")
	       (finish-output)
	       (case (do ((char (read-char *query-io*) (read-char *query-io*)))
			 ((and (char/= char #\Space) (char/= char #\Tab)) 
			  (cond
			    ((char= char #\Newline) char)
			    ((char= char #\Return) char)
			    ((alphanumericp (peek-char)) #\!) ;; Invalid command on purpose.
			    (t char))
			  ))
		 ((#\Newline #\Return)
		  (when allow-recursive (inspect-object object))
		  (return nil))
		 ((#\n #\N)
		  (inspect-read-line)
		  (when allow-recursive (inspect-object object))
		  (return nil))
		 ((#\s #\S)
		  (inspect-read-line)
		  (return nil))
		 ((#\p #\P)
		  (inspect-read-line)
		  (select-P object))
		 ((#\a #\A)
		  (inspect-read-line)
		  (throw 'ABORT-INSPECT nil))
		 ((#\u #\U)
		  (return (values t (select-U))))
		 ((#\e #\E)
		  (select-E object))
		 ((#\q #\Q)
		  (inspect-read-line)
		  (throw 'QUIT-INSPECT nil))
		 ((#\?)
		  (inspect-read-line)
		  (select-?))
		 (t
		  (inspect-read-line)
		  (inspect-indent)
		  (format t "Unknown inspector command. ~
                       Type ? followed by #\\Newline for help."))
		 )
	       )
	     nil
	     )
	 (format t "~&Back to Inspection mode: ~
                      Type ? followed by #\\Newline for help.~%")
	 ))))

#+mkcl-min
(defmacro inspect-recursively (label object &optional place)
  (if place
      `(multiple-value-bind (update-flag new-value)
            (read-inspect-command ,label ,object t)
         (when update-flag (setf ,place new-value)))
      `(when (read-inspect-command ,label ,object t)
             (princ "Not updated.")
             (terpri))))

#+mkcl-min
(defmacro inspect-print (label object &optional place)
  (if place
      `(multiple-value-bind (update-flag new-value)
           (read-inspect-command ,label ,object nil)
         (when update-flag (setf ,place new-value)))
      `(when (read-inspect-command ,label ,object nil)
             (princ "Not updated.")
             (terpri))))

(defun inspect-indent ()
  (fresh-line)
  (format t "~V@T"
          (* 4 (if (< *inspect-level* 8) *inspect-level* 8))))

(defun inspect-indent-1 ()
  (fresh-line)
  (format t "~V@T"
          (- (* 4 (if (< *inspect-level* 8) *inspect-level* 8)) 3)))


(defun inspect-symbol (symbol)
  (let* ((p (symbol-package symbol)))
    (cond ((null p)
           (format t "~:@(~S~) - uninterned symbol" symbol))
          ((eq p (find-package "KEYWORD"))
           (format t "~:@(~S~) - keyword" symbol))
          (t
           (format t "~:@(~S~) - ~:[internal~;external~] symbol in ~A package"
                   symbol
                   (multiple-value-bind (b f)
                                        (find-symbol (symbol-name symbol) p)
                     (declare (ignore b))
                     (eq f :external))
                   (package-name p)))))

  (when (print-doc symbol t)
        (format t "~&-----------------------------------------------------------------------------~%~%"))
  
  (if (or (eq t symbol) (eq nil symbol) (keywordp symbol))
      (progn (inspect-indent-1) (format t "value: ~S" (symbol-value symbol)))
      (when (boundp symbol)
        (if *inspect-mode*
            (inspect-recursively "value:"
                                 (symbol-value symbol)
                                 (symbol-value symbol))
            (inspect-print "value:~%   ~S"
                           (symbol-value symbol)
                           (symbol-value symbol)))))

  (do ((pl (symbol-plist symbol) (cddr pl)))
      ((endp pl))
    (unless (and (symbolp (car pl))
                 (or (eq (symbol-package (car pl)) (find-package 'SYSTEM))
                     (eq (symbol-package (car pl)) (find-package 'COMPILER))))
      (if *inspect-mode*
          (inspect-recursively (format nil "property ~S:" (car pl))
                               (cadr pl)
                               (get symbol (car pl)))
          (inspect-print (format nil "property ~:@(~S~):~%   ~~S" (car pl))
                         (cadr pl)
                         (get symbol (car pl))))))

  )

(defun inspect-package (package)
  (if (si:package-closed-p package)
      (format t "~S - closed package" package)
    (format t "~S - package" package)
    )    
  (when (package-nicknames package)
    (inspect-print "nicknames:  ~S" (package-nicknames package)))
  (when (package-use-list package)
    (inspect-print "use list:  ~S" (package-use-list package)))
  (when  (package-used-by-list package)
    (inspect-print "used-by list:  ~S" (package-used-by-list package)))
  (when (package-shadowing-symbols package)
    (inspect-print "shadowing symbols:  ~S" (package-shadowing-symbols package))))

(defun inspect-character (character)
  (format t
          (cond ((standard-char-p character) "~S - standard character")
                (t "~S - character"))
          character)
  (inspect-print "code:  #x~X" (char-code character)))

(defun inspect-number (number)
  (let ((type (type-of number)))
    (when (consp type) ;; Range types, as (INTEGER 0 0)
     (setf type (first type)))
    (format t "~S - ~a" number (string-downcase type))
    (case type
      (INTEGER)
      (RATIO
       (inspect-recursively "numerator:" (numerator number))
       (inspect-recursively "denominator:" (denominator number)))
      (COMPLEX
       (inspect-recursively "real part:" (realpart number))
       (inspect-recursively "imaginary part:" (imagpart number)))
      ((SHORT-FLOAT SINGLE-FLOAT LONG-FLOAT DOUBLE-FLOAT)
       (multiple-value-bind (signif expon sign)
	   (integer-decode-float number)
	 (declare (ignore sign))
	 (inspect-print "exponent:  ~D" expon)
	 (inspect-print "mantissa:  ~D" signif))))))

(defun inspect-cons (cons)
  (format t "~S - cons" cons)
  (when *inspect-mode*
        (do ((i 0 (1+ i))
             (l cons (cdr l)))
            ((atom l)
             (case l
	       ((t nil) ;; no point in inspecting recursively t nor nil.
		(inspect-print (format nil "nthcdr ~D: ~~S" i) l))
	       (t
		(inspect-recursively (format nil "nthcdr ~D:" i)
				     l (cdr (nthcdr (1- i) cons))))))
          (inspect-recursively (format nil "nth ~D:" i)
                               (car l) (nth i cons)))))


(defun inspect-utf-8 (utf-8)
  (format t "~S - UTF-8" utf-8)
  (inspect-print  "dimension:  ~D" (si:utf-8-length utf-8))
  (when *inspect-mode*
	(do ((i 0)) ((null i))
	  (multiple-value-bind (ch next invalid) (si:utf-8-char utf-8 i)
	    (inspect-recursively (format nil (if invalid "si:utf-8-char ~D - invalid character" "si:utf-8-char ~D") i) ch ch)
	    (setq i next)))))

(defun inspect-utf-16 (utf-16)
  (format t "~S - UTF-16" utf-16)
  (inspect-print  "dimension:  ~D" (si:utf-16-length utf-16))
  (when *inspect-mode*
	(do ((i 0)) ((null i))
	  (multiple-value-bind (ch next invalid) (si:utf-16-char utf-16 i)
	    (inspect-recursively (format nil (if invalid "si:utf-16-char ~D - invalid character" "si:utf-16-char ~D") i) ch ch)
	    (setq i next)))))

(defun inspect-string (string)
  (ignore-errors (format t (if (simple-string-p string) "~S - simple string" "~S - string") string))
  (inspect-print  "dimension:  ~D"(array-dimension string 0))
  (when (array-has-fill-pointer-p string)
        (inspect-print "fill pointer:  ~D"
                       (fill-pointer string)
                       (fill-pointer string)))
  (when *inspect-mode*
        (dotimes (i (array-dimension string 0))
                 (inspect-recursively (format nil "char ~D:" i)
                                      (char string i)
                                      (char string i)))))

(defun inspect-vector (vector)
  (ignore-errors (format t (if (simple-vector-p vector) "~S - simple vector" "~S - vector") vector))
  (inspect-print  "dimension:  ~D" (array-dimension vector 0))
  (when (array-has-fill-pointer-p vector)
        (inspect-print "fill pointer:  ~D"
                       (fill-pointer vector)
                       (fill-pointer vector)))
  (when *inspect-mode*
        (dotimes (i (array-dimension vector 0))
                 (inspect-recursively (format nil "aref ~D:" i)
                                      (aref vector i)
                                      (aref vector i)))))

(defun inspect-array (array)
  (format t (if (adjustable-array-p array)
                "~S - adjustable aray"
                "~S - array")
          array)
  (inspect-print "rank:  ~D" (array-rank array))
  (inspect-print "dimensions:  ~D" (array-dimensions array))
  (inspect-print "total size:  ~D" (array-total-size array)))

(defun select-ht-N (hashtable)
  (incf *inspect-level*)
  (maphash #'(lambda (key val)
	       (inspect-indent-1)
	       (format t "key  : ~S" key)
	       (inspect-recursively "value:" val (gethash key hashtable)))
	   hashtable)
  (decf *inspect-level*))

(defun select-ht-L (hashtable)
  (terpri)
  (format t "The keys of the hash table are:~%")
  (maphash #'(lambda (key val)
	       (declare (ignore val))
	       (format t "  ~S~%" key))
	   hashtable)
  (terpri))

(defun select-ht-J (hashtable)
  (let* ((key (prog1
		(read-preserving-whitespace *query-io*)
		(inspect-read-line)))
	 (val (gethash key hashtable)))
        (if val
	    (progn
	      (incf *inspect-level*)
	      (inspect-indent-1)
	      (format t "key  : ~S" key)
	      (inspect-recursively "value:" val (gethash key hashtable))
	      (decf *inspect-level*))
	    (progn
	      (terpri)
	      (format t "The key ~S is not present or the value associated is NIL." key)
	      (terpri)
	      (terpri)))))

(defun select-ht-? ()
  (terpri)
  (format t
	  "Inspect commands for hash tables:~%~
n (or N or #\\Newline):  inspects the keys/values of the hashtable (recursively).~%~
s (or S):             skips the field.~%~
p (or P):             pretty-prints the field.~%~
a (or A):             aborts the inspection of the rest of the fields.~%~
e (or E) form:        evaluates and prints the form.~%~
l (or L):             show the keys of the hash table.~%~
j (or J) key:         inspect the value associated to the key requested.~%~
q (or Q):             quits the inspection.~%~
?:                    prints this help message.~%~%"
	  ))

(defun inspect-hashtable (hashtable)
  (if *inspect-mode*
      (progn
	(decf *inspect-level*)
        (loop
          (format t "~S - hash table: " hashtable)
	  (finish-output)
          (case (do ((char (read-char *query-io*) (read-char *query-io*)))
	            ((and (char/= char #\Space) (char/= #\Tab)) char))
	        ((#\Newline #\Return)
		 (select-ht-N hashtable)
		 (return nil))
	        ((#\n #\N)
	         (inspect-read-line)
		 (select-ht-N hashtable)
		 (return nil))
	        ((#\s #\S)
	         (inspect-read-line)
	         (return nil))
		((#\p #\P)
		 (inspect-read-line)
		 (select-P hashtable))
		((#\a #\A)
		 (inspect-read-line)
		 (throw 'ABORT-INSPECT nil))
		((#\e #\E)
		 (select-E hashtable))
		((#\q #\Q)
		 (inspect-read-line)
		 (throw 'QUIT-INSPECT nil))
		((#\l #\L)
		 (inspect-read-line)
		 (select-ht-L hashtable))
		((#\j #\J)
		 (select-ht-J hashtable))
		((#\?)
		 (inspect-read-line)
		 (select-ht-?)))
          (inspect-indent)))
      (progn
	(format t "~S - hash table: " hashtable)
	(maphash #'(lambda (key val)
		     (inspect-indent-1)
		     (format t "key  : ~S" key)
		     (inspect-indent-1)
		     (format t "value:")
		     (inspect-object val))
	         hashtable))))

(defun inspect-instance (instance)
  (if *inspect-mode*
      (clos::inspect-obj instance)
      (clos::describe-object instance *standard-output*)))

(defun inspect-function (func)
  (multiple-value-bind (lambda-expr closure-p name)
       (function-lambda-expression func)
    (multiple-value-bind (file end-position)
	(si:compiled-function-file func)
      (format t "~S - function" func)
      (when lambda-expr
	(inspect-print "lambda-expression: ~S" lambda-expr)
	)
      (if name
	  (inspect-recursively "name: " name)
	(progn (inspect-indent) (format t "name: nil"))
	)
      (when file
	(inspect-indent)
	(format t "source: ~S ending at ~D." file end-position)
	)
      (when closure-p
	(multiple-value-bind (env csyms)
	    (si:closure-env func)
	    (if (listp env)
		(let ((nb-vars (length env)))
		  (inspect-indent)
		  (format t "bytecode closure closed over ~D variables." nb-vars)
		  (dotimes (i nb-vars)
		    (inspect-recursively
		     (format nil "var ~S:" (car (nth i env)))
		     (cdr (nth i env)) (cdr (nth i env)))
		    )
		  )
	      (let ((nb-levels (si:closure-depth env)))
		(inspect-indent)
		(format t "compiled closure closed over ~D lexical levels." nb-levels)
		(loop
		 for level-index downfrom (1- nb-levels) to 0 do
		 (let* ((level (si:closure-level env level-index))
			(nb-vars (si:closure-level-size level)))
		   (unless (zerop nb-vars)
		     (inspect-indent)
		     (if (< 1 nb-vars)
			 (format t "lexical level ~D has ~D variables." level-index nb-vars)
		       (format t "lexical level ~D has ~D variable." level-index nb-vars))

		     (dotimes (i nb-vars)
		       (inspect-recursively 
			(if csyms
			    (format nil "var ~S:" 
				    (handler-case (read-from-string (si:closure-level-var csyms i))
				      (si::simple-package-error (c)
					(let ((package (package-error-package c)))
					  (when (si:package-closed-p package)
					    (unwind-protect 
						(progn (si:reopen-package package)
						       (read-from-string (si:closure-level-var csyms i)))
					      (si:close-package package)))))))
			  (format nil "var ~D:" i))
			(si:closure-level-var level i)
			(si:closure-level-var level i)
			))
		     )
		   (when csyms (setq csyms (si:closure-level-outer-level csyms)))
		   )
		 )
		)
	      )
	  )))))

(defun inspect-pathname (it)
  (if (mkcl:logical-pathname-p it)
      (format t "~S - LOGICAL PATHNAME" it)
    (format t "~S - PATHNAME" it))
  (if *inspect-mode*
      (progn
	(inspect-recursively "host:" (pathname-host it))
	(inspect-recursively "device:" (pathname-device it))
	(inspect-recursively "directory:" (pathname-directory it))
	(inspect-recursively "name:" (pathname-name it))
	(inspect-recursively "type:" (pathname-type it))
	(inspect-recursively "version:" (pathname-version it))
	)
    (progn
      (inspect-print "host: ~S" (pathname-host it))
      (inspect-print "device: ~S" (pathname-device it))
      (inspect-print "directory: ~S" (pathname-directory it))
      (inspect-print "name: ~S" (pathname-name it))
      (inspect-print "type: ~S" (pathname-type it))
      (inspect-print "version: ~S" (pathname-version it))
      )
    )
  )

(defun inspect-object (object &aux (*inspect-level* *inspect-level*))
  (inspect-indent)
  (when (and (not *inspect-mode*)
             (or (> *inspect-level* 5)
                 (member object *inspect-history*)))
        (prin1 object)
        (return-from inspect-object))
  (incf *inspect-level*)
  (push object *inspect-history*)
  (catch 'ABORT-INSPECT
         (cond
	       ((symbolp object) (inspect-symbol object))
               ((packagep object) (inspect-package object))
               ((characterp object) (inspect-character object))
               ((numberp object) (inspect-number object))
               ((consp object) (inspect-cons object))
	       ((si:utf-8-p object) (inspect-utf-8 object))
	       ((si:utf-16-p object) (inspect-utf-16 object))
               ((stringp object) (inspect-string object))
               ((vectorp object) (inspect-vector object))
               ((arrayp object) (inspect-array object))
               ((hash-table-p object) (inspect-hashtable object))
	       ((sys:instancep object) (inspect-instance object))
	       ((functionp object) (inspect-function object))
	       ((pathnamep object) (inspect-pathname object))
               (t (format t "~S - ~S" object (type-of object))))))


(defun inspect (object &aux (*inspect-mode* t)
                            (*inspect-level* 0)
                            (*inspect-history* nil)
                            (*old-print-level* *print-level*)
                            (*old-print-length* *print-length*)
                            (*print-level* 3)
                            (*print-length* 3))
  "Args: (object)
Shows the information about OBJECT interactively.  See the MKCL Report for the
inspect commands, or type '?' to the inspector."
  ;;(read-line)
  (terpri)
  (princ "Inspection mode: Type ? followed by #\\Newline for help.")
  (terpri)
  (terpri)
  (catch 'QUIT-INSPECT (inspect-object object))
  (terpri)
  (values))

(defun describe (object &optional (stream *standard-output*)
			&aux (*inspect-mode* nil)
                             (*inspect-level* 0)
                             (*inspect-history* nil)
                             (*print-level* nil)
                             (*print-length* nil)
			     (*standard-output* (cond ((streamp stream) stream)
			                              ((null stream) *standard-output*)
						      ((eq stream t) *terminal-io*)
						      (t (error 'type-error
						                :datum stream
								:expected-type '(or stream t nil))))))
  "Args: (object &optional (stream *standard-output*))
Prints information about OBJECT to STREAM."
  (terpri)
  (catch 'QUIT-INSPECT (inspect-object object))
  (terpri)
  (values))

(defun print-doc (symbol &optional (called-from-apropos-doc-p nil)
                         &aux (f nil) x)
  (flet ((doc1 (doc ind)
           (setq f t)
           (format t
                   "~&-----------------------------------------------------------------------------~%~53S~24@A~%~A"
                   symbol ind doc))
         (good-package ()
           (if (eq (symbol-package symbol) (find-package "CL"))
               (find-package "SYSTEM")
               *package*)))

    (cond ((special-operator-p symbol)
           (doc1 (or (si::get-documentation symbol 'FUNCTION) "")
                 (if (macro-function symbol)
                     "[Special form and Macro]"
                     "[Special form]")))
          ((macro-function symbol)
           (doc1 (or (si::get-documentation symbol 'FUNCTION) "") "[Macro]"))
          ((fboundp symbol)
           (doc1 (or (si::get-documentation symbol 'FUNCTION) "") "[Function]"))
          ((setq x (si::get-documentation symbol 'FUNCTION))
           (doc1 x "[Macro or Function]")))

    (cond ((constantp symbol)
           (unless (and (eq (symbol-package symbol) (find-package "KEYWORD"))
                        (null (si::get-documentation symbol 'VARIABLE)))
             (doc1 (or (si::get-documentation symbol 'VARIABLE) "") "[Constant]")))
          ((sys:specialp symbol)
           (doc1 (or (si::get-documentation symbol 'VARIABLE) "")
                 "[Special variable]"))
          ((or (setq x (si::get-documentation symbol 'VARIABLE)) (boundp symbol))
           (doc1 (or x "") "[Variable]")))

    (cond ((setq x (si::get-documentation symbol 'TYPE))
           (doc1 x "[Type]"))
          ((setq x (get-sysprop symbol 'DEFTYPE-FORM))
           (let ((*package* (good-package)))
             (doc1 (format nil "~%Defined as: ~S~%See the doc of DEFTYPE." x)
                   "[Type]"))))

    (cond ((setq x (si::get-documentation symbol 'STRUCTURE))
           (doc1 x "[Structure]"))
          ((setq x (get-sysprop symbol 'DEFSTRUCT-FORM))
           (doc1 (format nil "~%Defined as: ~S~%See the doc of DEFSTRUCT." x)
                 "[Structure]")))

    (cond ((setq x (si::get-documentation symbol 'SETF))
           (doc1 x "[Setf]"))
          ((setq x (get-sysprop symbol 'SETF-UPDATE-FN))
           (let ((*package* (good-package)))
             (doc1 (format nil "~%Defined as: ~S~%See the doc of DEFSETF."
                           `(defsetf ,symbol ,(get-sysprop symbol 'SETF-UPDATE-FN)))
                   "[Setf]")))
          ((setq x (get-sysprop symbol 'SETF-LAMBDA))
           (let ((*package* (good-package)))
             (doc1 (format nil "~%Defined as: ~S~%See the doc of DEFSETF."
                           `(defsetf ,symbol ,@(get-sysprop symbol 'SETF-LAMBDA)))
                   "[Setf]")))
          ((setq x (get-sysprop symbol 'SETF-METHOD))
           (let ((*package* (good-package)))
             (doc1
              (format nil
                "~@[~%Defined as: ~S~%See the doc of DEFINE-SETF-EXPANDER.~]"
                (if (consp x)
                    (case (car x)
                          (LAMBDA `(define-setf-expander ,@(cdr x)))
                          (SI::LAMBDA-BLOCK `(define-setf-expander ,@(cddr x)))
                          (t nil))
                    nil))
            "[Setf]"))))
    )
  (if called-from-apropos-doc-p
      f
      (progn (if f
                 (format t "~&-----------------------------------------------------------------------------")
                 (format t "~&No documentation for ~:@(~S~)." symbol))
             (values))))

(defun mkcl::help* (string &optional (package "COMMON-LISP"))
  "Args: (string &optional (package-spec \"COMMON-LISP\"))
MKCL specific.
Prints the documentation associated with those symbols in the specified
package whose print names contain STRING as substring.  STRING may be a
symbol, in which case the print-name of that symbol is used.  If PACKAGE is
NIL, then all packages are searched."
  (do* ((f nil)
	(l (apropos-list string package) (cdr l)))
      ((endp l)
       (format t (if f
		     "~&-----------------------------------------------------------------------------"
		     "~&No documentation for ~S in ~:[any~;~A~] package.")
	       string package (and package (package-name (coerce-to-package package)))))
    (when (print-doc (first l) t)
      (setf f t)))
  (values))

(export 'mkcl::help* :mkcl)



