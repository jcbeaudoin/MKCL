;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: C -*-
;;;;
;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;  Copyright (c) 2011-2016,2021-2022, Jean-Claude Beaudoin.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Lesser General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 3 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../../Copyright' for full details.

;;;; CMPENV  Environments of the Compiler.

(in-package "COMPILER")

;;; Only these flags are set by the user.
;;; If (safe-compile) is ON, some kind of run-time checks are not
;;; included in the compiled code.  The default value is OFF.

(defconstant +init-env-form+
  '((*gensym-counter* 0)
    (*compiler-in-use* t)
    (*compiler-phase* 't1)
    (*callbacks* nil)
    (*max-temp* 0)
    (*temp* 0)
    (*next-cmacro* 0)
    (*next-cfun* 0)
    (*last-label* 0)
    (*load-objects* (make-hash-table :size 128 :test #'equal))
    (*make-forms* nil)
    (*static-constants* nil)
    (*permanent-objects* nil)
    (*temporary-objects* nil)
    (*data-storage-frozen* nil) ;; JCB
    (*local-funs* nil)
    (*global-var-objects* nil)
    (*global-vars* nil)
    (*global-funs* nil)
    (*global-cfuns-array* (make-array 128 :fill-pointer 0 :adjustable t)) ;; JCB
    (*lex-local-funs* nil)
    (*linking-calls* nil)
    (*undefined-vars* nil)
    (*reservations* nil)
    (*top-level-forms* nil)
    (*compile-time-too* nil)
    (*clines-string-list* '())
    (*inline-functions* nil)
    (*inline-blocks* 0)
    (*notinline* nil)
    (*debugger-hook*  (let ((outer-hook *debugger-hook*))
			#'(lambda (condition hook)
			  (compiler-debugger outer-hook condition hook))))
    (*readtable* (copy-readtable))   ;; for thread safety. JCB
    (si::*debug* si::*debug*)   ;; JCB
    (si::*safety* si::*safety*) ;; JCB
    (si::*space* si::*space*)   ;; JCB
    (si::*speed* si::*speed*)   ;; JCB
    (si::*compilation-speed* si::*compilation-speed*) ;; JCB
    (*compiler-floating-point-exclusion-set* *compiler-floating-point-exclusion-set*) ;; JCB
    (ffi::*referenced-libraries* ffi::*referenced-libraries*) ;; JCB
    ))

(defun next-lcl () (list 'LCL (incf *lcl*)))

(defun next-cfun (&optional (prefix "L~D~A") (lisp-name nil))
  (let ((code (incf *next-cfun*)))
    (format nil prefix code (lisp-to-c-function-name lisp-name))))

(defun next-temp ()
  (prog1 *temp*
         (incf *temp*)
         (setq *max-temp* (max *temp* *max-temp*))))

(defun next-lex ()
  (prog1 (cons *level* *lex*)
         (incf *lex*)
         (setq *max-lex* (max *lex* *max-lex*))))

(defun next-env () (prog1 *env*
		     (incf *env*)
		     (incf (car *closure-levels*))
		     (setq *max-env* (max *env* *max-env*))))


(defun function-arg-types (arg-types &aux (types nil))
  (do ((al arg-types (cdr al)))
      ((or (endp al)
           (member (car al) '(&optional &rest &key)))
       (nreverse types))
      ;;(declare (object al))
      (push (si::type-filter (car al)) types)))

(defun add-function-declaration (fname arg-types return-types)
  (if (si::valid-function-name-p fname)
      (let ((fun (cmp-env-search-function fname)))
	(if (functionp fun)
	    (warn "Found function declaration for local macro ~A" fname)
	    (push (list fun
			(function-arg-types arg-types)
			(si::function-return-type return-types))
		  *function-declarations*)))
      (warn "In (DECLARE (FTYPE ...)): ~s is not a valid function name" fname)))

(defun get-arg-types (fname)
  (let ((x (assoc fname *function-declarations*)))
    (if x
	(values (second x) t)
      (let ((finfo (si::get-sysprop fname 'SI::PROCLAIMED-FUNCTION-INFORMATION)))
	(if finfo
 	    (values (si::proclaimed-function-arg-types finfo) t)
	  (values '* nil))))))

(defun get-return-type (fname)
  (let ((x (assoc fname *function-declarations*)))
    (if x
	(values (third x) t)
      (let ((finfo (si::get-sysprop fname 'SI::PROCLAIMED-FUNCTION-INFORMATION)))
	(if finfo
	    (values (si::proclaimed-function-return-type finfo) t)
	  (values '* nil))))))

(defun get-local-arg-types (fun &aux x)
  (if (setq x (assoc fun *function-declarations*))
      (values (second x) t)
      (values '* nil)))

(defun get-local-return-type (fun &aux x)
  (if (setq x (assoc fun *function-declarations*))
      (values (caddr x) t)
      (values '* nil)))

(defun get-proclaimed-narg (fun-name)
  (multiple-value-bind (arg-list found)
      (get-arg-types fun-name)
    (if found
	(loop for type in arg-list
	   with minarg = 0
	   and maxarg = 0
	   and in-optionals = nil
	   do (cond ((member type '(* &rest &key &allow-other-keys) :test #'eq)
		     (return (values found minarg call-arguments-limit)))
		    ((eq type '&optional)
		     (setf in-optionals t maxarg minarg))
		    (in-optionals
		     (incf maxarg))
		    (t
		     (incf minarg)
		     (incf maxarg)))
	   finally (return (values found minarg maxarg)))
      #+(and) ;; JCB
      (multiple-value-bind (found cname minarg maxarg) (si::mangle-function-name fun-name)
	(declare (ignore cname))
	(values found minarg maxarg)
	)
      #+(or)(values found 0 call-arguments-limit)))) ;; JCB

;;; Proclamation and declaration handling.

(defun declared-notinline-p (fname)
  (let ((local-decl (assoc fname *notinline* :test #'same-fname-p)))
    (if local-decl
	(eq (cdr local-decl) 'NOTINLINE)
      (get-sysprop fname 'NOTINLINE)
      )
    )
  )

(defun inline-possible (fname)
  (not (or (declared-notinline-p fname)
           (<= 3 (cmp-env-optimization 'debug))
           )))


(defun c1body (body doc-p &aux
	            (all-declarations nil)
		    (ss nil)		; special vars
		    (is nil)		; ignored vars
		    (iables nil)        ; ignorable vars
		    (ts nil)		; typed vars (var . type)
		    (dyns nil)          ; dynamic-extent vars
		    (others nil)	; all other vars
	            doc form)
  (loop
    (when (endp body) (return))
    (setq form (cmp-macroexpand (car body)))
    (cond
     ((stringp form)
      (when (or (null doc-p) (endp (cdr body)) doc) (return))
      (setq doc form))
     ((and (consp form) (eq (car form) 'DECLARE))
      (push form all-declarations)
      (dolist (decl (cdr form))
        (cmpassert (and (proper-list-p decl) (si::typespecp (first decl)))
		   "Syntax error in declaration ~s" form)
	(let* ((decl-name (first decl))
	       (decl-args (rest decl)))
	  (flet ((declare-variables (type var-list)
		   (cmpassert (proper-list-p var-list #'symbolp)
			      "Syntax error in declaration ~s" decl)
		   (when type
		     (dolist (var var-list)
		       (push (cons var type) ts)))))
	    (case decl-name
	      (SPECIAL
	       (cmpassert (proper-list-p decl-args #'symbolp)
			  "Syntax error in declaration ~s" decl)
	       (setf ss (append decl-args ss)))
	      (IGNORE
	       (cmpassert (proper-list-p decl-args
                                         #'(lambda (x) 
                                             (or (symbolp x)
                                                 (and (consp x) (eq (car x) 'function) 
                                                      (si::valid-function-name-p (cadr x)))))
                                         )
			  "Syntax error in declaration ~s" decl)
	       (setf is (append decl-args is)))
	      (IGNORABLE
	       (cmpassert (proper-list-p decl-args
                                         #'(lambda (x) 
                                             (or (symbolp x)
                                                 (and (consp x) (eq (car x) 'function) 
                                                      (si::valid-function-name-p (cadr x)))))
                                         )
			  "Syntax error in declaration ~s" decl)
	       (setf iables (append decl-args iables)))
	      (TYPE
	       (cmpassert decl-args "Syntax error in declaration ~s" decl)
	       (declare-variables (first decl-args) (rest decl-args)))
	      (DYNAMIC-EXTENT
	       (cmpassert (proper-list-p decl-args
                                         #'(lambda (x)
                                             (or (symbolp x)
                                                 (and (consp x) (eq (car x) 'function)
                                                      (si::valid-function-name-p (cadr x))))))
			  "Syntax error in declaration ~s" decl)
	       (setf dyns (append decl-args dyns))
	       )
	      (OBJECT
	       (declare-variables 'OBJECT decl-args))
	      ;; read-only variable treatment. obsolete!
	      (:READ-ONLY
	       (push decl others))
	      ((OPTIMIZE FTYPE INLINE NOTINLINE DECLARATION
		VALUES SI::C-GLOBAL SI::NO-CHECK-TYPE)
	       (push decl others))
	      (ANNUL #| do some processing of declarations to annul. |#)
	      (otherwise
	       (if (member decl-name si::*alien-declarations*)
		   (push decl others)
		 (multiple-value-bind (ok type)
		     (si::valid-type-specifier decl-name)
		   (if ok  
		       (declare-variables type decl-args)
		     (cmpwarn "The declaration specifier ~s is unknown." decl-name)) ;; JCB
		   )))
	      )))))
     (t (return)))
    (pop body)
    )
  (values body ss ts is others doc all-declarations iables dyns)
  )

(defun default-optimization (optimization)
  (ecase optimization
    (speed si::*speed*)
    (safety si::*safety*)
    (space si::*space*)
    (debug si::*debug*)
    (compilation-speed si::*compilation-speed*)))

(defun search-optimization-quality (declarations what)
  (dolist (i (reverse declarations) (cmp-env-optimization what))
    (when (and (consp i) (eq (first i) 'optimize))
      (dolist (j (rest i))
	(cond ((consp j)
	       (when (eq (first j) what)
		 (return-from search-optimization-quality (second j))))
	      ((eq j what)
	       (return-from search-optimization-quality 3)))))))

(defun c1add-declarations (decls &aux (dl nil) (optimizations))
  (dolist (decl decls)
    (case (car decl)
      (OPTIMIZE
       (push decl dl)
       (dolist (x (cdr decl))
	 (when (symbolp x) (setq x (list x 3)))
	 (unless optimizations
	   (setq optimizations (cmp-env-all-optimizations)))
	 (if (or (not (consp x))
		 (not (consp (cdr x)))
		 (not (numberp (second x)))
		 (not (<= 0 (second x) 3)))
	   (cmpwarn "The OPTIMIZE proclamation ~s is illegal." x)
	   (let ((value (second x)))
	     (case (car x)
	       (DEBUG (setf (first optimizations) value))
	       (SAFETY (setf (second optimizations) value))
	       (SPACE (setf (third optimizations) value))
	       (SPEED (setf (fourth optimizations) value))
	       (COMPILATION-SPEED (setf (fifth optimizations) value))
	       (t (cmpwarn "The OPTIMIZE quality ~s is unknown." (car x))))))))
      (FTYPE
       (if (atom (rest decl))
	   (cmpwarn "Syntax error in declaration ~a" decl)
	   (multiple-value-bind (type-name args)
	       (si::normalize-type (second decl))
	     (if (eq type-name 'FUNCTION)
		 (dolist (v (cddr decl))
		   (add-function-declaration v (first args) (rest args)))
		 (cmpwarn "In an FTYPE declaration, found ~A which is not a function type." (second decl))))))
      (INLINE
       (push decl dl)
       (dolist (fname (cdr decl))
	 (if (si::valid-function-name-p fname)
             (push (cons fname 'INLINE) *notinline*)
	   (cmperr "Not a valid function name ~s in declaration ~s" fname decl))))
      (NOTINLINE
       (push decl dl)
       (dolist (fname (cdr decl))
	 (if (si::valid-function-name-p fname)
	     (push (cons fname 'NOTINLINE) *notinline*)
	   (cmperr "Not a valid function name ~s in declaration ~s" fname decl))))
      (DECLARATION
       (do-declaration (rest decl) #'cmperr))
      ((SI::C-GLOBAL SI::NO-CHECK-TYPE))
      ((DYNAMIC-EXTENT IGNORABLE)
       ;; These need no action here and are handled elsewhere.
       )
      (:READ-ONLY)
      (otherwise
       (unless (member (car decl) si:*alien-declarations*)
	 (cmpwarn "The declaration specifier ~s is unknown." (car decl))))))
  (when optimizations
    (setf *cmp-env*
	  (cons (cons `(:declare optimize ,@optimizations)
		      (car *cmp-env*))
		(cdr *cmp-env*))))
  dl)

(defun c1decl-body (decls body)
  (if (null decls)
      (c1progn body)
      (let* ((*function-declarations* *function-declarations*)
	     (si:*alien-declarations* si:*alien-declarations*)
	     (*notinline* *notinline*)
	     (*cmp-env* *cmp-env*)
	     (dl (c1add-declarations decls)))
	(setq body (c1progn body))
	(make-c1form 'DECL-BODY body dl body))))

(put-sysprop 'decl-body 'c2 'c2decl-body)

(defun c2decl-body (decls body)
  (let ((*cmp-env* *cmp-env*)
        (*notinline* *notinline*))
    (c1add-declarations decls)
    (c2expr body)))

(defun check-vdecl (specials ignored-vars)
  (dolist (svar specials)
    (when (member svar ignored-vars)
      (cmpwarn-style "Variable ~s declared special while explicitly ignored." svar))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; COMPILER ENVIRONMENT
;;;

(defmacro cmp-env-new ()
  '(cons nil nil))

(defun cmp-env-copy (&optional (env *cmp-env*))
  (cons (car env) (cdr env)))

(defmacro cmp-env-variables (&optional (env '*cmp-env*))
  `(car ,env))

(defmacro cmp-env-functions (&optional (env '*cmp-env*))
  `(cdr ,env))

(defun cmp-env-register-var (var &optional (env *cmp-env*) (boundp t))
  ;;(declare (ignore env))
  (push (list (var-name var)
	      (if (member (var-kind var) '(special global))
		  :special
		  t)
	      boundp
	      var)
	;;(cmp-env-variables)
	(cmp-env-variables env)
	))

(defun cmp-env-declare-special (name &optional (env *cmp-env*))
  (cmp-env-register-var (c1make-global-variable name :warn nil :kind 'SPECIAL)
			env nil))

(defun cmp-env-register-function (fun &optional (env *cmp-env*))
  (push (list (fun-name fun) 'function fun)
	(cmp-env-functions env)))

(defun cmp-env-register-macro (name function &optional (env *cmp-env*))
  (push (list name 'si::macro function)
	(cmp-env-functions env)))

(defun cmp-env-register-symbol-macro (name form &optional (env *cmp-env*))
  (push (list name
	      'si::symbol-macro
	      #'(lambda (whole env) (declare (ignore whole env)) form))
	(cmp-env-variables env)))

(defun cmp-env-register-block (blk &optional (env *cmp-env*))
  (push (list :block (blk-name blk) blk)
	(cmp-env-variables env)))

(defun cmp-env-register-tag (tag &optional (env *cmp-env*))
  (push (list :tag (list (tag-name tag)) tag)
	(cmp-env-variables env)))

(defun cmp-env-search-function (name &optional (env *cmp-env*))
  (let ((ccb nil)
	(clb nil)
	(unw nil)
	(found nil))
    (dolist (record (cmp-env-functions env))
      (cond ((eq record 'CB)
	     (setf ccb t))
	    ((eq record 'LB)
	     (setf clb t))
	    ((eq record 'UNWIND-PROTECT)
	     (setf unw t))
	    ((atom record)
	     (baboon))
	    ;; We have to use EQUAL because the name can be a list (SETF whatever)
	    ((equal (first record) name)
	     (setf found (first (last record)))
	     (return))))
    (values found ccb clb unw)))

(defun cmp-env-search-variables (type name env)
  (let ((ccb nil)
	(clb nil)
	(unw nil)
	(found nil))
    (dolist (record (cmp-env-variables env))
      (cond ((eq record 'CB)
	     (setf ccb t))
	    ((eq record 'LB)
	     (setf clb t))
	    ((eq record 'UNWIND-PROTECT)
	     (setf unw t))
	    ((atom record)
	     (baboon))
	    ((not (eq (first record) type)))
	    ((eq type :block)
	     (when (eq name (second record))
	       (setf found record)
	       (return)))
	    ((eq type :tag)
	     (when (member name (second record) :test #'eql)
	       (setf found record)
	       (return)))
	    ((eq (second record) 'si::symbol-macro)
	     (when (eq name 'si::symbol-macro)
	       (setf found record))
	     (return))
	    (t
	     (setf found record)
	     (return))))
    (values (first (last found)) ccb clb unw)))

(defun cmp-env-search-block (name &optional (env *cmp-env*))
  (cmp-env-search-variables :block name env))

(defun cmp-env-search-tag (name &optional (env *cmp-env*))
  (cmp-env-search-variables :tag name env))

(defun cmp-env-search-symbol-macro (name &optional (env *cmp-env*))
  (cmp-env-search-variables name 'si::symbol-macro env))

(defun cmp-env-search-var (name &optional (env *cmp-env*))
  (cmp-env-search-variables name t env))

(defun cmp-env-search-macro (name &optional (env *cmp-env*))
  (let ((f (cmp-env-search-function name env)))
    (if (functionp f) f nil)))

(defun cmp-env-mark (mark &optional (env *cmp-env*))
  (cons (cons mark (car env))
	(cons mark (cdr env))))

(defun cmp-env-new-variables (new-env old-env)
  (declare (ignore new-env))
  (loop for i in (ldiff (cmp-env-variables *cmp-env*)
			(cmp-env-variables old-env))
	when (and (consp i) (var-p (fourth i)))
	collect (fourth i)))

(defun cmp-env-all-optimizations (&optional (env *cmp-env*))
  (loop for i in (car env)
     when (and (consp i)
	       (eq (first i) :declare)
	       (eq (second i) 'optimize))
     do (return (cddr i))
     finally (return (list si::*debug* si::*safety* si::*space* si::*speed* si::*compilation-speed*))))

(defun cmp-env-optimization (property &optional (env *cmp-env*))
  (let ((x (cmp-env-all-optimizations env)))
    (case property
      (debug (first x))
      (safety (second x))
      (space (third x))
      (speed (fourth x))
      (compilation-speed (fifth x)))))

(defun policy-assume-right-type (&optional (env *cmp-env*)) ;; used in funcall optimization
  (< (cmp-env-optimization 'safety env) 2))

#+(and)
(defun policy-check-call-stack-overflow (&optional (env *cmp-env*))
  "Do we add a stack check to every function?"
  (>= (cmp-env-optimization 'safety env) 1))

(defun policy-inline-slot-access-p (&optional (env *cmp-env*))
  "Do we inline access to structures and sealed classes?"
  (or (< (cmp-env-optimization 'safety env) 2)
       (<= (cmp-env-optimization 'safety env) (cmp-env-optimization 'speed env))))

(defun policy-check-all-arguments-p (&optional (env *cmp-env*))
  "Do we assume that arguments are the right type?"
  (>= (cmp-env-optimization 'safety env) 1))

(defun policy-automatic-check-type-p (&optional (env *cmp-env*))
  "Do we generate CHECK-TYPE forms for function arguments with type declarations?"
  (let ((safety (cmp-env-optimization 'safety env)))
    ;;(format t "~& In policy-automatic-check-type-p, safety is ~S for env: ~S.~%" safety env) (finish-output) ;; debug JCB
    (and *automatic-check-type-in-lambda*
	 (>= safety 3 #|1|#))) ;; JCB
  )

(defun policy-assume-types-dont-change-p (&optional (env *cmp-env*))
  "Do we assume that type and class definitions will not change?"
  (< (cmp-env-optimization 'safety env) 1))


;;;
;;;
#|

;;; This is a useful way to trace declaim. JCB
(define-compiler-macro declaim (&whole form &rest decl-specs &environment env)
  (format t "~&Applying compiler-macro declaim on: ~S.~%" form)
  form
  )

|#

