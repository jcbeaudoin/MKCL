;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: SYSTEM -*-
;;;;
;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;  Copyright (c) 2010-2015, Jean-Claude Beaudoin
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Lesser General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 3 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../../Copyright' for full details.
;;;;         defines SYS:DEFMACRO*, the defmacro preprocessor

(in-package "SYSTEM")

#-mkcl-min
(defvar *dl*)
#-mkcl-min
(defvar *key-check*)
#-mkcl-min
(defvar *arg-check*)

#+mkcl-min
(sys:*make-special '*dl*)
#+mkcl-min
(sys:*make-special '*key-check*)
#+mkcl-min
(sys:*make-special '*arg-check*)

#+mkcl-min
(si::fset 'push
	  #'(si::lambda-block push (args env)
	      (let* ((what (second args))
		     (where (caddr args)))
		`(setq ,where (cons ,what ,where))))
	  t)

#+mkcl-min
(si::fset 'pop
	  #'(si::lambda-block pop (args env)
	      (let ((where (cadr args)))
		`(let* ((l ,where)
			(v (car l)))
		  (setq ,where (cdr l))
		  v)))
	  t)

#+mkcl-min
(si::fset 'incf
	  #'(si::lambda-block incf (args env)
	      (let* ((where (second args))
		     (what (caddr args)))
		(if what
		  `(setq ,where (+ ,where ,what))
		  `(setq ,where (1+ ,where)))))
	  t)

#+mkcl-min
(si::fset 'decf
	  #'(si::lambda-block decf (args env)
	      (let* ((where (second args))
		     (what (caddr args)))
		(if what
		  `(setq ,where (- ,where ,what))
		  `(setq ,where (1- ,where)))))
	  t)

(defun sys::search-keyword (list key)
  (cond ((atom list) 'failed)
	((atom (cdr list)) 'failed)
	((eq (car list) key) (cadr list))
	(t (search-keyword (cddr list) key))))

(defun check-keyword (name key-arg-list keywords &optional (allow-other-keys nil aok-flag))
  (do ((tail key-arg-list)
       head
       arg
       (err 42)) ;; The 42 here is simply a value that is not a symbol and can never be one.
      ((null tail)
       (unless (or (eql 42 err) allow-other-keys)
         (simple-program-error
          "The key ~s is not allowed in this call to macro ~A. Expected one of: ~S" err name keywords)))
    (if (atom tail)
        (simple-program-error
         "In call to macro ~A, keyword list is not properly formed. Keyword list is: ~S" name key-arg-list)
      (setq head (car tail) tail (cdr tail)))
    (if (atom tail)
        (simple-program-error
         "In call to macro ~A, keyword list is not properly formed. Keyword list is: ~S" name key-arg-list)
      (setq arg (car tail) tail (cdr tail)))
    (cond ((eq head :allow-other-keys)
           (when (not aok-flag)
             (setq allow-other-keys tail aok-flag t)))
          ((not (symbolp head))
           (simple-program-error
            "In keyword list, element ~S is not a symbol as required. Keyword list is: ~S" head key-arg-list))
          ((not (member head keywords)) ;; FIXME: this reports only the last unknown keyword.
           (setq err head)))))

(defun check-arg-length (name list max-length)
  (when (> (length list) max-length)
    (simple-program-error "Too many arguments supplied in call to macro ~A." name)))

(defun dm-bad-key (key)
  (simple-program-error "Defmacro-lambda-list contains illegal use of ~s." key))

(defun dm-too-few-arguments ()
  (simple-program-error "Too few arguments supplied to a macro or a destructuring-bind form."))

(defun sys::destructure (vl macro)
  (labels ((dm-vl (vl whole macro)
	     (let* ((n (if macro 1 0))
		    (ppn 0)
		    (no-check nil)
		    all-keywords)
	       (multiple-value-bind (reqs nb_reqs opts nb_opts rest key-flag keys nb_keys
					  allow-other-keys auxs nb_auxs)
		   (si::process-lambda-list vl (if macro 'macro 'destructuring-bind))
		 (declare (ignore nb_reqs nb_auxs))

		 (dolist (v reqs)
		   (dm-v v `(if ,(dm-nth-cdr n whole)
			     ,(dm-nth n whole)
			     (dm-too-few-arguments)))
		   (incf n))
		 (dotimes (i nb_opts)
		   (let* ((x (first opts))
			  (init (second opts))
			  (sv (third opts)))
		     (setq opts (cdddr opts))
		     (dm-v x `(if ,(dm-nth-cdr n whole) ,(dm-nth n whole) ,init))
		     (when sv (dm-v sv `(not (null ,(dm-nth-cdr n whole)))))
		     (incf n)))
		 (when rest
		   (dm-v rest (dm-nth-cdr n whole))
		   (setq no-check t
			 rest nil)
		   (when (and (null (last vl 0)) (member '&body vl))
		     (setq ppn (if macro (1- n) n))))
		 (dotimes (i nb_keys)
		   (when (null rest)
		     (setq rest (gensym))
		     (dm-v rest (dm-nth-cdr n whole))
		     (setq no-check t))
		   (let* ((temp (gensym))
			  (k (first keys))
			  (v (second keys))
			  (init (third keys))
			  (sv (fourth keys)))
		     (setq keys (cddddr keys))
		     (dm-v temp `(search-keyword ,rest ',k))
		     (dm-v v `(if (eq ,temp 'failed) ,init ,temp))
		     (when sv (dm-v sv `(not (eq ,temp 'failed))))
		     (push k all-keywords)))
		 (do ((l auxs (cddr l))) ((endp l))
		   (let* ((v (first l))
			  (init (second l)))
		     (dm-v v init)))
		 (cond (key-flag
			(push `(check-keyword ',(or macro 'destructuring-bind) ,rest ',all-keywords
				,@(if allow-other-keys '(t) '()))
			      *key-check*))
		       ((not no-check)
			(push `(check-arg-length ',(or macro 'destructuring-bind) ,whole ,n) *arg-check*))))
	       ppn))

	   (dm-v (v init)
	     (cond ((and v (symbolp v))
		    (push (if init (list v init) v) *dl*))
		   ((and v (atom v))
		    (error "destructure: ~A is not a list nor a symbol" v))
		   ((eq (first v) '&whole)
		    (let ((whole-var (second v)))
		      (if (listp whole-var)
			  (let ((new-whole (gensym)))
			    (dm-v new-whole init)
			    (dm-vl whole-var new-whole nil)
			    (setq whole-var new-whole))
			  (dm-v whole-var init))
		      (dm-vl (cddr v) whole-var nil)))
		   (t
		    (let ((temp (gensym)))
		      (push (if init (list temp init) temp) *dl*)
		      (dm-vl v temp nil)))))

	   (dm-nth (n v)
	     (multiple-value-bind (q r) (floor n 4)
	       (declare (fixnum q r))
	       (dotimes (i q) (setq v (list 'CDDDDR v)))
	       (case r
		 (0 (list 'CAR v))
		 (1 (list 'CADR v))
		 (2 (list 'CADDR v))
		 (3 (list 'CADDDR v))
		 )))

	   (dm-nth-cdr (n v)
	     (multiple-value-bind (q r) (floor n 4)
	       (declare (fixnum q r))
	       (dotimes (i q) (setq v (list 'CDDDDR v)))
	       (case r
		 (0 v)
		 (1 (list 'CDR v))
		 (2 (list 'CDDR v))
		 (3 (list 'CDDDR v))
		 ))))

    (let* ((whole nil)
	   (*dl* nil)
	   (*key-check* nil)
	   (*arg-check* nil))
      (cond ((listp vl)
	     (when (eq (first vl) '&whole)
	       (setq whole (second vl) vl (cddr vl))
	       (when (listp whole)
		 (let ((new-whole (gensym)))
		   (dm-vl whole new-whole nil)
		   (setq whole new-whole)))))
	    ((symbolp vl)
	     (setq vl (list '&rest vl)))
	    (t (error "The destructuring-lambda-list ~s is not a list." vl)))
      (if (null whole) (setq whole (gensym)))
      (values (dm-vl vl whole macro) whole (nreverse *dl*) *key-check* *arg-check*))))

;;; valid lambda-list to DEFMACRO is:
;;;
;;;	( [ &whole sym ]
;;;	  [ &environment sym ]
;;;	  { v }*
;;;	  [ &optional { sym | ( v [ init [ v ] ] ) }* ]
;;;	  {  [ { &rest | &body } v ]
;;;	     [ &key { sym | ( { sym | ( key v ) } [ init [ v ]] ) }*
;;;		    [ &allow-other-keys ]]
;;;	     [ &aux { sym | ( v [ init ] ) }* ]
;;;	  |  . sym }
;;;	 )
;;;
;;; where v is short for { defmacro-lambda-list | sym }.
;;; A symbol may be accepted as a DEFMACRO lambda-list, in which case
;;; (DEFMACRO <name> <symbol> ... ) is equivalent to
;;; (DEFMACRO <name> (&REST <symbol>) ...).
;;; Defmacro-lambda-list is defined as:
;;;
;;;	( { v }*
;;;	  [ &optional { sym | ( v [ init [ v ] ] ) }* ]
;;;	  {  [ { &rest | &body } v ]
;;;	     [ &key { sym | ( { sym | ( key v ) } [ init [ v ]] ) }*
;;;		    [ &allow-other-keys ]]
;;;	     [ &aux { sym | ( v [ init ] ) }* ]
;;;	  |  . sym }
;;;	 )
(defun find-documentation (body)
  (nth-value 3 (process-declarations body t)))

(defun remove-documentation (body)
  (multiple-value-bind (decls body doc)
      (process-declarations body t)
    (when decls (push `(declare ,@decls) body))
    (values body doc)))

(defun find-declarations (body &optional (doc t))
  (multiple-value-bind (decls body doc)
      (process-declarations body doc)
    (values (if decls `((declare ,@decls)) nil)
	    body doc)))

(defun sys::expand-defmacro (name vl body
			     &aux *dl* *key-check* *arg-check*
			     doc decls ppn env)
  (multiple-value-setq (decls body doc)
    (find-declarations body))
  ;; We turn (a . b) into (a &rest b)
  ;; This is required because MEMBER (used below) does not like improper lists
  (let ((cell (last vl)))
    (when (rest cell)
      (setq vl (nconc (butlast vl 0) (list '&rest (rest cell))))))
  ;; If we find an &environment variable in the lambda list, we take not of the
  ;; name and remove it from the list so that DESTRUCTURE does not get confused
  (if (setq env (member '&environment vl :test #'eq))
      (setq vl (nconc (ldiff vl env) (cddr env))
	    env (second env))
      (progn
	(setq env (gensym))
	(push `(declare (ignore ,env)) decls)))
  (multiple-value-bind (ppn whole *dl* *key-check* *arg-check*)
      (destructure vl name)
    (setq body (nconc decls (append *arg-check* *key-check* body)))
    (values `(si::lambda-block ,name (,whole ,env &aux ,@*dl*) ,@body)
	    ppn
	    doc)))

(si::fset 'defmacro ;; bootstrap version. redefined in evalmacros.lsp
	  #'(si::lambda-block defmacro (def env)
	      (declare (ignore env))
	      ;;(print "!! Bootstrap defmacro for ") (princ def) ;; debug! JCB
	      (let* ((name (second def))
		     (vl (third def))
		     (body (cdddr def))
		     (function))
		(multiple-value-bind (function pprint doc)
		    (sys::expand-defmacro name vl body)
		  (declare (ignore doc))
		  (setq function `(function ,function))
		  #+(or)
		  (when *dump-defmacro-definitions*
		    (print function)
		    (setq function `(si::bc-disassemble ,function)))
		  (si:register-with-pde def `(si::fset ',name ,function t ,pprint)))))
	  t)

;;; valid lambda-list to DESTRUCTURING-BIND is:
;;;
;;;	( [ &whole sym ]
;;;	  { v }*
;;;	  [ &optional { sym | ( v [ init [ v ] ] ) }* ]
;;;	  {  [ { &rest | &body } v ]
;;;	     [ &key { sym | ( { sym | ( key v ) } [ init [ v ]] ) }*
;;;		    [ &allow-other-keys ]]
;;;	     [ &aux { sym | ( v [ init ] ) }* ]
;;;	  |  . sym }
;;;	 )
;;;
;;; where v is short for { destructuring-bind-lambda-list | sym }.
;;; A symbol may be accepted as a DESTRUCTURING-BIND lambda-list, in which case
;;; (DESTRUCTURING-BIND <name> <symbol> ... ) is equivalent to
;;; (DESTRUCTURING-BIND <name> (&REST <symbol>) ...).
;;; Destructuring-bind-lambda-list is defined as:
;;;
;;;	( [ &whole sym ]
;;;	  { v }*
;;;	  [ &optional { sym | ( v [ init [ v ] ] ) }* ]
;;;	  {  [ { &rest | &body } v ]
;;;	     [ &key { sym | ( { sym | ( key v ) } [ init [ v ]] ) }*
;;;		    [ &allow-other-keys ]]
;;;	     [ &aux { sym | ( v [ init ] ) }* ]
;;;	  |  . sym }
;;;	 )

(defmacro destructuring-bind (vl list &body body &aux (decls nil))
  (multiple-value-setq (decls body) (find-declarations body))
  (multiple-value-bind (ppn whole *dl* *key-check* *arg-check*)
      (destructure vl nil)
    (declare (ignore ppn))
    (setq body (nconc decls (append *arg-check* *key-check* body)))
    (list* 'let* (cons (list whole list) *dl*) body)))

(defun warn (&rest foo) ;; This muffles warnings until clos/conditions.lsp
  (declare (ignore foo))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; MACROLET HELPER
;;;

(defun cmp-env-for-bytecode (old-env)
#|  "Produce an environment which is safe to pass to the bytecode
compiler. We remove all blocks and tags and ensure that
references to local variables will cause an error. This
environment can be used to bytecompile the functions in MACROLET
or SYMBOL-MACRO forms, and also to evaluate other forms." |#
  (flet ((local-var-error-function (name)
	  #'(lambda (whole env)
	      (declare (ignore whole env))
	      (error
"In a MACROLET function you tried to access a local variable, ~A,
from the function in which it appears." name)))
	 (local-fun-error-function (name)
	  #'(lambda (whole env)
	      (declare (ignore whole env))
	      (error
"In a MACROLET function you tried to access a local function, ~A,
from the function in which it appears." name))))
    (cons (do ((env (car old-env) (cdr env))
	       (variables '()))
	      ((endp env) (nreverse variables))
	    (let ((i (car env)))
	      (if (consp i)
		(let ((name (first i)))
		  (if (not (keywordp name))
		      (push (if (second i)
				i
			      (list name 'si::symbol-macro (local-var-error-function name)))
			    variables))))))
	  (do ((env (cdr old-env) (cdr env))
	       (macros '()))
	      ((endp env) (nreverse macros))
	    (let ((i (car env)))
	      (if (consp i)
		(push (if (eq (second i) 'SI::MACRO)
			  i
			(list (first i) 'SI:MACRO (local-fun-error-function (first i))))
		      macros)))))))

(defun macrolet-functions (definitions old-env)
  (let ((env (cmp-env-for-bytecode old-env)))
    (si::eval-in-env
     (cons 'list
	   (mapcar #'(lambda (x)
		       (let* ((name (first x))
			      (llist (second x))
			      (def (cddr x)))
			 `(list ',name ,(si::expand-defmacro name llist def))))
		   definitions))
     env nil t)))

(defun cmp-env-register-macrolet (definitions old-env)
  (let ((macros (cdr old-env)))
    (dolist (record (macrolet-functions definitions old-env))
      (push (list (first record) 'si::macro (second record))
	    macros))
    (rplacd old-env macros)))


