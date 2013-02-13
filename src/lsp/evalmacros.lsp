;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: SYSTEM -*-
;;;;
;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;  Copyright (c) 2011-2012, Jean-Claude Beaudoin.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Lesser General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 3 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../../Copyright' for full details.

(in-package "SYSTEM")


(defmacro unless (pred &rest body)
  "Syntax: (unless test {form}*)
If TEST evaluates to NIL, then evaluates FORMs and returns all values of the
last FORM.  If not, simply returns NIL."
  `(IF (NOT ,pred) (PROGN ,@body)))

(defmacro defun (&whole whole name vl &body body &aux doc-string)
  "Syntax: (defun name lambda-list {decl | doc}* {form}*)
Defines a global function named by NAME.
The complete syntax of a lambda-list is:
	({var}*
	 [&optional {var | (var [init [svar]])}*]
	 [&rest var]
	 [&key {var | ({var | (keyword var)} [init [svar]])}*
	       [&allow-other-keys]]
	 [&aux {var | (var [init])}*])
The doc-string DOC, if supplied, is saved as a FUNCTION doc and can be
retrieved by (documentation 'NAME 'function)."
  (multiple-value-setq (body doc-string) (remove-documentation body))
  (let* ((function `#'(si::lambda-block ,name ,vl ,@body))
	 (global-function `#'(si::lambda-block ,name ,vl
			       (declare (si::c-global))
			       ,@body)))
    #+(or)
    (when *dump-defun-definitions*
      (print function)
      (setq function `(si::bc-disassemble ,function)))
  `(define-when (:load-toplevel :execute)
;;      (eval-when (:execute)
;;        (si::fset ',name ,global-function)
;; ;;       (si::fset ',name ,function)
;;        )
;;      (eval-when (:load-toplevel)
;;        ,(si:register-with-pde whole `(si::fset ',name ,global-function)))
     ,(si:register-with-pde whole `(si::fset ',name ,global-function))
     ,@(si::expand-set-documentation name 'function doc-string)
    ',name)))

(defmacro defmacro (name vl &body body &aux doc-string)
  "Syntax: (defmacro name defmacro-lambda-list {decl | doc}* {form}*)
Defines a global macro named by NAME.  The complete syntax of DEFMACRO-LAMBDA-
LIST is:
	( [&whole var] [&environment var] . pvar )
where PVAR may be a symbol,
	( {pvar}* [&optional {var | (pvar [init [pvar]])}*] . var )
or
	( {pvar}*
	  [&optional {var | (pvar [init [pvar]])}*]
	  [{&rest | &body} pvar]
	  [&key {var | ({var | (keyword pvar)} [init [pvar]])}*
	        [&allow-other-keys]]
	  [&aux {var | (pvar [init])}*] )
The doc-string DOC, if supplied, is saved as a FUNCTION doc and can be
retrieved by (documentation 'NAME 'function).  See LIST for the backquote
macro useful for defining macros."
  (multiple-value-bind (function pprint doc-string)
      (sys::expand-defmacro name vl body)
    (setq function `(function ,function))
    #+(or)
    (when *dump-defun-definitions*
      (print function)
      (setq function `(si::bc-disassemble ,function)))
    `(define-when (:compile-toplevel :load-toplevel :execute)
       (si::fset ',name ,function t ,pprint)
       ,@(si::expand-set-documentation name 'function doc-string)
       ',name)))

(defun register-global (var)
  (let ((cmp-pkg (find-package "COMPILER")))
    (when cmp-pkg (funcall (find-symbol (symbol-name 'register-global) cmp-pkg) var))))

(defmacro defvar (&whole whole var &optional (form nil form-sp) doc-string)
  "Syntax: (defvar name [form [doc]])
Declares the variable named by NAME as a special variable.  If the variable
does not have a value, then evaluates FORM and assigns the value to the
variable.  FORM defaults to NIL.  The doc-string DOC, if supplied, is saved
as a VARIABLE doc and can be retrieved by (documentation 'NAME 'variable)."
  `(LOCALLY (DECLARE (SPECIAL ,var))
    (SYS:*MAKE-SPECIAL ',var)
    ,@(when form-sp
	  `((UNLESS (BOUNDP ',var)
	      (SETQ ,var ,form))))
    ,@(si::expand-set-documentation var 'variable doc-string)
    ,(si:register-with-pde whole)
    ,(unless *bytecode-compiler*
       `(eval-when (:compile-toplevel)
          (si::register-global ',var)))
    ',var))

(defmacro defparameter (&whole whole var form &optional doc-string)
  "Syntax: (defparameter name form [doc])
Declares the global variable named by NAME as a special variable and assigns
the value of FORM to the variable.  The doc-string DOC, if supplied, is saved
as a VARIABLE doc and can be retrieved by (documentation 'NAME 'variable)."
  `(LOCALLY (DECLARE (SPECIAL ,var))
    (SYS:*MAKE-SPECIAL ',var)
    (SETQ ,var ,form)
    ,@(si::expand-set-documentation var 'variable doc-string)
    ,(si:register-with-pde whole)
    ,(unless *bytecode-compiler*
       `(eval-when (:compile-toplevel)
          (si::register-global ',var)))
    ',var))

(defmacro defconstant (&whole whole var form &optional doc-string)
  "Syntax: (defconstant symbol form [doc])

Declares that the global variable named by SYMBOL is a constant with the value
of FORM as its constant value.  The doc-string DOC, if supplied, is saved as a
VARIABLE doc and can be retrieved by (DOCUMENTATION 'SYMBOL 'VARIABLE)."
  `(PROGN
     (SYS:*MAKE-CONSTANT ',var ,form)
    ,@(si::expand-set-documentation var 'variable doc-string)
    ,(si:register-with-pde whole)
    ,(unless *bytecode-compiler*
       `(eval-when (:compile-toplevel)
          (si::register-global ',var)))
    ',var))

;;;
;;; This is a no-op unless the compiler is installed
;;;
(defmacro define-compiler-macro (&whole whole name vl &rest body)
  (multiple-value-bind (function pprint doc-string)
      (sys::expand-defmacro name vl body)
    (declare (ignore pprint))
    (setq function `(function ,function))
    #+(or)
    (when *dump-defun-definitions*
      (print function)
      (setq function `(si::bc-disassemble ,function)))
    `(define-when (:load-toplevel :execute) ;;progn
       (put-sysprop ',name 'sys::compiler-macro ,function)
       ,@(si::expand-set-documentation name 'function doc-string)
       ,(si:register-with-pde whole)
       ',name)))

(defun compiler-macro-function (name &optional env)
  (declare (ignore env))
  (get-sysprop name 'sys::compiler-macro))

;;; Each of the following macros is also defined as a special form,
;;; as required by CLtL. Some of them are used by the compiler (e.g.
;;; dolist), some not at all (e.g. defun).
;;; Thus their names need not be exported.

(let ()
  ;; We enclose the macro in a LET form so that it is no longer
  ;; a toplevel form. This solves the problem of this simple LOOP
  ;; replacing the more complex form in loop2.lsp when evalmacros.lsp
  ;; gets compiled.
(defmacro loop (&rest body &aux (tag (gensym)))
  "Syntax: (loop {form}*)
Establishes a NIL block and executes FORMs repeatedly.
The loop is normally terminated by a non-local exit."
  `(BLOCK NIL (TAGBODY ,tag (PROGN ,@body) (GO ,tag))))
)

(defmacro lambda (&rest body)
  `(function (lambda ,@body)))

(defmacro si:lambda-block (name lambda-list &rest lambda-body)
  (multiple-value-bind (decl body doc)
      (si::process-declarations lambda-body)
    (when decl (setq decl (list (cons 'declare decl))))
    `(lambda ,lambda-list ,@doc ,@decl
      (block ,(si::function-block-name name) ,@body))))

; assignment

(defmacro psetq (&rest args)
  "Syntax: (psetq {var form}*)
Similar to SETQ, but evaluates all FORMs first, and then assigns each value to
the corresponding VAR.  Returns NIL."
   (do ((l args (cddr l))
        (forms nil)
        (bindings nil))
       ((endp l) (list* 'LET* (nreverse bindings) (nreverse (cons nil forms))))
       (let ((sym (gensym)))
            (push (list sym (cadr l)) bindings)
            (push (list 'setq (car l) sym) forms)))
   )

; conditionals

(defmacro cond (&rest clauses &aux (form nil))
  "Syntax: (cond {(test {form}*)}*)
Evaluates TESTs in order until one evaluates to non-NIL.  Then evaluates FORMs
in order that follow the TEST and returns all values of the last FORM.  If no
forms follow the TEST, then returns the value of the TEST.  Returns NIL, if no
TESTs evaluates to non-NIL."
  (dolist (l (reverse clauses) form)	; don't use nreverse here
    (if (endp (cdr l))
	(if (eq (car l) 't)
	    (setq form 't)
	    (let ((sym (gensym)))
	      (setq form `(LET ((,sym ,(car l)))
			   (IF ,sym ,sym ,form)))))
	(if (eq (car l) 't)
	    (setq form (if (endp (cddr l))
			   (cadr l)
			   `(PROGN ,@(cdr l))))
	    (setq form (if (endp (cddr l))
			   `(IF ,(car l) ,(cadr l) ,form)
			   `(IF ,(car l) (PROGN ,@(cdr l)) ,form))))))
  )

; program feature

(defmacro prog (vl &rest body &aux (decl nil))
  "Syntax: (prog ({var | (var [init])}*) {decl}* {tag | statement}*)
Establishes a NIL block, binds each VAR to the value of INIT (which defaults
to NIL) in parallel, and executes STATEMENTs.  Returns NIL."
  (multiple-value-setq (decl body)
    (find-declarations body))
  `(BLOCK NIL (LET ,vl ,@decl (TAGBODY ,@body)))
  )

(defmacro prog* (vl &rest body &aux (decl nil))
  "Syntax: (prog* ({var | (var [init])}*) {decl}* {tag | statement}*)
Establishes a NIL block, binds each VAR to the value of INIT (which defaults
to NIL) sequentially, and executes STATEMENTs.  Returns NIL."
  (multiple-value-setq (decl body)
    (find-declarations body))
  `(BLOCK NIL (LET* ,vl ,@decl (TAGBODY ,@body)))
  )

; sequencing

(defmacro prog1 (first &rest body &aux (sym (gensym)))
  "Syntax: (prog1 first-form {form}*)
Evaluates FIRST-FORM and FORMs in order.  Returns the value of FIRST-FORM."
  (if (null body) first
  `(LET ((,sym ,first))
    ,@body ,sym)))

(defmacro prog2 (first second &rest body &aux (sym (gensym)))
  "Syntax: (prog2 first-form second-form {forms}*)
Evaluates FIRST-FORM, SECOND-FORM, and FORMs in order.  Returns the value of
SECOND-FORM."
  `(PROGN ,first (LET ((,sym ,second))
		       ,@body ,sym)))

; multiple values

(defmacro multiple-value-list (form)
  "Evaluates FORM and returns a list of all values FORM returns."
  `(MULTIPLE-VALUE-CALL #'LIST ,form))

(defmacro multiple-value-setq (vars form)
  "Syntax: (multiple-value-setq {var}* form)

Evaluates FORM and binds the N-th VAR to the N-th value of FORM or, if FORM
returns less than N values, to NIL.  Returns the first value of FORM or, if
FORM returns no value, NIL."
  (do ((vl vars (cdr vl))
       (sym (gensym))
       (forms nil)
       (n 0 (1+ n)))
      ((endp vl) `(LET ((,sym (MULTIPLE-VALUE-LIST ,form))) ,@forms))
      (declare (fixnum n))
      (push `(SETQ ,(car vl) (NTH ,n ,sym)) forms))
  )

;; We do not use this macroexpanso, and thus we do not care whether
;; it is efficiently compiled by MKCL or not.
(defmacro multiple-value-bind (vars form &rest body)
  "Syntax: (multiple-value-bind ({var}*) init {decl}* {form}*)

Evaluates INIT and binds the N-th VAR to the N-th value of INIT or, if INIT
returns less than N values, to NIL.  Then evaluates FORMs, and returns all
values of the last FORM.  If no FORM is given, returns NIL."
  `(multiple-value-call #'(lambda (&optional ,@(mapcar #'list vars)) ,@body) ,form))

(defun while-until (test body jmp-op)
  (let ((label (gensym))
	(exit (gensym)))
    `(TAGBODY
        (GO ,exit)
      ,label
        ,@body
      ,exit
	(,jmp-op ,test (GO ,label)))))

(defmacro sys::while (test &body body)
  (while-until test body 'when))

(defmacro sys::until (test &body body)
  (while-until test body 'unless))

#|
(defmacro case (keyform &rest clauses &aux (form nil) (key (gensym)))
  (dolist (clause (reverse clauses)
	   `(LET ((,key ,keyform))
	     ,form))
    (if (or (eq (car clause) 'T) (eq (car clause) 'OTHERWISE))
	(setq form `(PROGN ,@(cdr clause)))
	(if (consp (car clause))
	    (setq form `(IF (MEMBER ,key ',(car clause))
			 (PROGN ,@(cdr clause))
			 ,form))
	    (if (car clause)
		(setq form `(IF (EQL ,key ',(car clause))
			     (PROGN ,@(cdr clause))
			     ,form))))))
  )
|#

#|
(defun choose-test (key)
  (if (and (numberp key) (not (si:fixnump key)))
      'EQL
    'EQ)
  )

(defun build-case-member (test-key keys &aux (form nil))
  (dolist (key keys `(not (and ,@(nreverse form))))
    (push `(not (,(choose-test key) ,test-key ',key)) form)
    )
  )
|#

(defmacro case (keyform &rest clauses &aux (reverse-clauses (reverse clauses)) (form nil) (test-key (gensym)))
  (labels ((choose-test (key)
	     (if (and (numberp key) (not (si:fixnump key))) 'EQL 'EQ))
	   (build-case-member (test-key keys &aux (form nil))
	     (dolist (key keys `(not (and ,@(nreverse form))))
	       (push `(not (,(choose-test key) ,test-key ',key)) form)))
	   )
    (dolist (clause reverse-clauses
		    `(LET ((,test-key ,keyform))
			  ,form))
      (let ((keys (car clause)))
	(if (or (eq keys 'T) (eq keys 'OTHERWISE))
	    (progn
	      (unless (eq clause (first reverse-clauses))
		(si:simple-program-error "CASE: otherwise-clause must appear last in list of clauses"))
	      (setq form `(PROGN ,@(cdr clause))))
	  (if (consp keys)
	      (setq form `(IF ,(build-case-member test-key keys)
			      (PROGN ,@(cdr clause))
			      ,form))
	    (if keys
		(setq form `(IF (,(choose-test keys) ,test-key ',keys)
				(PROGN ,@(cdr clause))
				,form))))))))
  )


(defmacro return (&optional (val nil)) `(RETURN-FROM NIL ,val))

;; Declarations
(defmacro declaim (&rest decl-specs)
  (if (cdr decl-specs)
    `(eval-when (:compile-toplevel :execute)
       (mapcar #'proclaim ',decl-specs))
    `(eval-when (:load-toplevel :execute)
       (proclaim ',(car decl-specs)))))


(defmacro in-package (name)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (si::select-package ,(string name))))

;; FIXME!
(defmacro the (type value)
  (declare (ignore type))
  value)

#+mkcl-min
(defmacro ignore-errors (&rest forms) ;; bootstrap version.
  ;;(format t "~&Expanding ignore-errors stub! Error!~%") (finish-output)
  `(progn ;;(format t "~&Executing ignore-errors stub. This is a macro expansion error!~%") (finish-output)
	  ,@forms))


(defmacro define-symbol-macro (&whole whole symbol expansion)
  (cond ((not (symbolp symbol))
	 (simple-program-error "DEFINE-SYMBOL-MACRO: ~A is not a symbol" symbol))
	((specialp symbol)
	 (simple-program-error "DEFINE-SYMBOL-MACRO: cannot redefine a special variable, ~A" symbol))
	(t
	 `(define-when (:load-toplevel :execute) ;;progn
	   (put-sysprop ',symbol 'si::symbol-macro
			(lambda (form env) (declare (ignore form env)) ',expansion))
	   ,(si:register-with-pde whole)
	   ',symbol))))

(defmacro nth-value (n expr)
  `(nth ,n (multiple-value-list ,expr)))

(defun maybe-unquote (form)
  (if (and (consp form) (eq (car form) 'quote))
      (second form)
      form))

(defun maybe-quote (form)
  ;; Quotes a form only if strictly required. This happens only when FORM is
  ;; either a symbol and not a keyword
  (if (if (atom form)
	  (typep form '(and symbol (not keyword) (not boolean)))
	  (not (eq (first form) 'quote)))
      (list 'quote form)
      form))


(defmacro dyn-list (&rest args)
  `(dyn-cons ,(car args) ,(and (cdr args) `(dyn-list ,@(cdr args)))))

(defmacro dyn-list* (&rest args)
  (if (cdr args)
      `(dyn-cons ,(car args) (dyn-list* ,@(cdr args)))
    (car args)))

(defun dyn-append (&rest args)
  (declare (dynamic-extent args))
  (let (root head (tail (car (last args))) (lists (nbutlast args)))
    (si:while (and lists (null (car args))) (pop args))
    (when (null lists) (return-from dyn-append tail))
    (let ((arg (car lists)))
      (setq root (setq head (si:dyn-cons (car arg) nil)))
      (dolist (it (cdr arg)) 
	(let ((new (si:dyn-cons it nil))) (rplacd head new) (setq head new))))
    (dolist (arg (cdr lists))
      (dolist (it arg)
	(let ((new (si:dyn-cons it nil))) (rplacd head new) (setq head new))))
    (rplacd head tail)
    root
    )
  )

(export '(dyn-list dyn-list* dyn-append))

