;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: C -*-
;;;;
;;;;  CMPFUN  Library functions.

;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi and William F. Schelter.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Lesser General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 3 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../../Copyright' for full details.


(in-package "COMPILER")

(defvar *princ-string-limit* 80)

(defun c1princ (args)
  (check-args-number 'PRINC args 1 2)
  (let ((object (first args))
	(stream (if (endp (rest args))
		    (c1nil)
		    (c1expr (second args)))))
    (if (and (or (and (stringp object)
		      (<= (length object) *princ-string-limit*))
		 (characterp object))
	     (or (endp (rest args))
		 (eq (c1form-name stream) 'VAR)))
	(make-c1form* 'C2PRINC :args object (c1form-arg 0 stream) stream)
	(c1call-global 'PRINC args))))

(defun c2princ (string stream-var stream)
  (cond ((eq *destination* 'TRASH)
	 (cond ((characterp string)
		(wt-nl "mkcl_princ_char(env, " (char-code string) "," stream-var ");"))
	       ((= (length string) 1)
		(wt-nl "mkcl_princ_char(env, " (char-code (aref string 0)) ","
		       stream-var ");"))
	       (t
		(wt-nl "mkcl_princ_str(env, \"")
		(dotimes (n (length string))
		  (declare (fixnum n))
		  (let ((char (schar string n)))
		       (cond ((char= char #\\) (wt "\\\\"))
			     ((char= char #\") (wt "\\\""))
			     ((char= char #\Newline) (wt "\\n"))
			     (t (wt char)))))
		(wt "\"," stream-var ");")))
	 (unwind-exit nil))
	;;((eql string #\Newline) (c2call-global 'TERPRI (list stream) t)) ;; bad optim, JCB
	(t (c2call-global 'PRINC
                          (list (make-c1form 'LOCATION *info* (add-object string))
                                stream)
                          t))))

#|
;; This optimizer for terpri fails in some obscure cases and buys us nothing! JCB
(defun c1terpri (args &aux stream)
  (check-args-number 'TERPRI args 0 1)
  (setq stream (if (endp args)
		   (c1nil)
		   (c1expr (first args))))
  (if (or (endp args)
	  (and (eq (c1form-name stream) 'VAR)
	       (member (var-kind (c1form-arg 0 stream)) '(GLOBAL SPECIAL))))
      (make-c1form* 'C2PRINC :args  #\Newline
		    (if (endp args) nil (c1form-arg 0 stream))
		    stream)
      (c1call-global 'TERPRI args)))
|#

(defun c1apply (args)
  (check-args-number 'APPLY args 2)
  (let* ((fun (first args))
	 (arguments (rest args)))
    (cond ((and (consp fun)
		(eq (first fun) 'LAMBDA))
	   (c1expr (optimize-funcall/apply-lambda (cdr fun) arguments t)))
	  ((and (consp fun)
		(eq (first fun) 'SI::LAMBDA-BLOCK))
	   (setf fun (macroexpand-1 fun))
	   (c1expr (optimize-funcall/apply-lambda (cdr fun) arguments t)))
	  ((and (consp fun)
		(eq (first fun) 'FUNCTION)
		(consp (second fun))
		(member (caadr fun) '(LAMBDA SI::LAMBDA-BLOCK)))
	   (c1apply (list* (second fun) arguments)))
	  (t
	   (c1funcall (list* '#'APPLY args))))))

(defun c1rplaca (args)
  (check-args-number 'RPLACA args 2 2)
  (make-c1form* 'RPLACA :args (c1args* args)))

(defun c2rplaca (args &aux (*inline-blocks* 0) x y)
  (setq args (coerce-locs (inline-args args))
	x (first args)
	y (second args))
  (when (safe-compile)
   (wt-nl "if(MKCL_ATOM(" x "))"
	  "mkcl_FEtype_error_cons(env, " x ");"))
  (wt-nl "MKCL_CONS_CAR(" x ") = " y ";")
  (unwind-exit x)
  (close-inline-blocks))

(defun c1rplacd (args)
  (check-args-number 'RPLACD args 2 2)
  (make-c1form* 'RPLACD :args (c1args* args)))

(defun c2rplacd (args &aux (*inline-blocks* 0) x y)
  (setq args (coerce-locs (inline-args args))
	x (first args)
	y (second args))
  (when (safe-compile)
   (wt-nl "if(MKCL_ATOM(" x "))"
	  "mkcl_FEtype_error_cons(env, " x ");"))
  (wt-nl "MKCL_CONS_CDR(" x ") = " y ";")
  (unwind-exit x)
  (close-inline-blocks))

(defun c1member (args)
  (check-args-number 'MEMBER args 2)
  (cond ((endp (cddr args))
	 (make-c1form* 'MEMBER!2 :args 'EQL (c1args* args)))
	((and (eq (third args) :test)
	      (= (length args) 4)       ; Beppe
	      (member (fourth args) '('EQ #'EQ 'EQUAL #'EQUAL 'EQL #'EQL)
		      :test #'EQUAL))	; arg4 = (QUOTE EQ)
	 (make-c1form* 'MEMBER!2 :args (second (fourth args))
		       (c1args* (list (car args) (second args)))))
	(t
	 (c1call-global 'MEMBER args))))

(defun c2member!2 (fun args
		       &aux (*inline-blocks* 0))
  (unwind-exit
   (produce-inline-loc (inline-args args) '(T T) '(:object)
	 (case fun
	   (EQ "mkcl_memq(env, #0,#1)")
	   (EQL "mkcl_memql(env, #0,#1)")
	   (EQUAL "mkcl_member(env, #0,#1)"))
	 nil ; side effects?
	 t)) ; one liner?
  (close-inline-blocks))

(defun c1assoc (args)
  (check-args-number 'ASSOC args 2)
  (cond ((endp (cddr args))
	 (make-c1form* 'ASSOC!2 :args 'EQL (c1args* args)))
	((and (eq (third args) ':TEST)
	      (= (length args) 4)       ; Beppe
	      (member (fourth args) '('EQ #'EQ 'EQUAL #'EQUAL
				      'EQUALP #'EQUALP 'EQL #'EQL)
		      :test 'EQUAL))
	 (make-c1form* 'ASSOC!2 :args (second (fourth args))
		       (c1args* (list (car args) (second args)))))
	(t
	 (c1call-global 'ASSOC args))))

(defun c2assoc!2 (fun args
		      &aux (*inline-blocks* 0))
  (unwind-exit
   (produce-inline-loc (inline-args args) '(T T) '(:object)
	 (case fun
	   (eq "mkcl_assq(env, #0,#1)")
	   (eql "mkcl_assql(env, #0,#1)")
	   (equal "mkcl_assoc(env, #0,#1)")
	   (equalp "mkcl_assqlp(env, #0,#1)"))
	 nil ; side effects?
	 t
	 ))
  (close-inline-blocks))

(defun co1nth (args)
  (and (not (endp args))
       (not (endp (cdr args)))
       (endp (cddr args))
       (numberp (car args))
       (<= 0 (car args) 7)
       (c1expr (case (car args)
		     (0 (cons 'CAR (cdr args)))
		     (1 (cons 'CADR (cdr args)))
		     (2 (cons 'CADDR (cdr args)))
		     (3 (cons 'CADDDR (cdr args)))
		     (4 (list 'CAR (cons 'CDDDDR (cdr args))))
		     (5 (list 'CADR (cons 'CDDDDR (cdr args))))
		     (6 (list 'CADDR (cons 'CDDDDR (cdr args))))
		     (7 (list 'CADDDR (cons 'CDDDDR (cdr args))))
		     ))))

(defun co1nthcdr (args)
  (and (not (endp args))
       (not (endp (cdr args)))
       (endp (cddr args))
       (numberp (car args))
       (<= 0 (car args) 7)
       (c1expr (case (car args)
		 (0 (second args))
		 (1 (cons 'CDR (cdr args)))
		 (2 (cons 'CDDR (cdr args)))
		 (3 (cons 'CDDDR (cdr args)))
		 (4 (cons 'CDDDDR (cdr args)))
		 (5 (list 'CDR (cons 'CDDDDR (cdr args))))
		 (6 (list 'CDDR (cons 'CDDDDR (cdr args))))
		 (7 (list 'CDDDR (cons 'CDDDDR (cdr args))))))))

;;----------------------------------------------------------------------
;; We transform BOOLE into the individual operations, which have
;; inliners
;;

(define-compiler-macro boole (&whole form op-code op1 op2)
  (or (and (constantp op-code)
	   (case (eval op-code)
	     (#. boole-clr `(progn ,op1 ,op2 0))
	     (#. boole-set `(progn ,op1 ,op2 -1))
	     (#. boole-1 `(prog1 ,op1 ,op2))
	     (#. boole-2 `(progn ,op1 ,op2))
	     (#. boole-c1 `(prog1 (lognot ,op1) ,op2))
	     (#. boole-c2 `(progn ,op1 (lognot ,op2)))
	     (#. boole-and `(logand ,op1 ,op2))
	     (#. boole-ior `(logior ,op1 ,op2))
	     (#. boole-xor `(logxor ,op1 ,op2))
	     (#. boole-eqv `(logeqv ,op1 ,op2))
	     (#. boole-nand `(lognand ,op1 ,op2))
	     (#. boole-nor `(lognor ,op1 ,op2))
	     (#. boole-andc1 `(logandc1 ,op1 ,op2))
	     (#. boole-andc2 `(logandc2 ,op1 ,op2))
	     (#. boole-orc1 `(logorc1 ,op1 ,op2))
	     (#. boole-orc2 `(logorc2 ,op1 ,op2))))
      form))

;----------------------------------------------------------------------

(defun co1coerce (args &aux expr type (info (make-info)))
  (declare (ignore info))
  (and args (cdr args) (endp (cddr args))
       (let ((expr (first args))
	     (type (second args)))
	 (and (listp type)
	      (eq (car type) 'QUOTE)
	      (case (second type)
                (BASE-CHAR (c1expr `(let* ((object ,expr)
                                           (new (character object)))
                                      (if (mkcl:base-char-p new) new (si::error-coerce object ,type)))))
		(CHARACTER (c1expr `(CHARACTER ,expr)))
		(FLOAT (c1expr `(FLOAT ,expr)))
		(SHORT-FLOAT (c1expr `(FLOAT ,expr 0.0S0)))
		(SINGLE-FLOAT (c1expr `(FLOAT ,expr 0.0F0)))
		(DOUBLE-FLOAT (c1expr `(FLOAT ,expr 0.0D0)))
		(LONG-FLOAT (c1expr `(FLOAT ,expr 0.0L0)))
                )))))

;----------------------------------------------------------------------
;; turn repetitious cons's into a list*

(defun co1cons (args &aux temp)
  (labels ((cons-to-lista (x)
	     (let ((tem (last x)))
	       (if (and (consp tem)
			(consp (car tem))
			(eq (caar tem) 'CONS)
			(eql (length (cdar tem)) 2))
		   (cons-to-lista (append (butlast x) (cdar tem)))
		   x))))
    (and (eql (length args) 2)
	 (not (eq args (setq temp (cons-to-lista args))))
	 (c1expr (if (equal '(nil) (last temp))
		     (cons 'LIST (butlast temp))
		     (cons 'LIST* temp))))))

;----------------------------------------------------------------------

;; Return the most particular type we can EASILY obtain from x.  
(defun result-type (x)
  (cond ((symbolp x)
	 (c1form-primary-type (c1expr x)))
	((constantp x)
	 (si::type-filter (type-of x)))
	((and (consp x) (eq (car x) 'the))
	 (si::type-filter (second x)))
	(t t)))

;----------------------------------------------------------------------

;;; Look for inline expansion of LDB1 in sysfun.lsp

(defun co1ldb (args &aux (arg1 (first args))
		    (len (integer-length most-positive-fixnum))
		    size pos)
    (and (consp arg1)
	 (eq 'BYTE (car arg1))
	 (integerp (setq size (second arg1)))
	 (integerp (setq pos (third arg1)))
	 (<= (+ size pos) len)
	 (subtypep (result-type (second args)) 'FIXNUM)
	 (c1expr `(the fixnum (ldb1 ,size ,pos ,(second args))))))

;;; ----------------------------------------------------------------------

(put-sysprop 'princ 'C1 'c1princ)
(put-sysprop 'c2princ 'C2 'c2princ)
;;(put-sysprop 'terpri 'C1 'c1terpri) ;; JCB

(put-sysprop 'apply 'C1 'c1apply)

(put-sysprop 'rplaca 'C1 'c1rplaca)
(put-sysprop 'rplaca 'C2 'c2rplaca)
(put-sysprop 'rplacd 'C1 'c1rplacd)
(put-sysprop 'rplacd 'C2 'c2rplacd)

(put-sysprop 'member 'C1 'c1member)
(put-sysprop 'member!2 'C2 'c2member!2)
(put-sysprop 'assoc 'C1 'c1assoc)
(put-sysprop 'assoc!2 'C2 'c2assoc!2)

(put-sysprop 'nth 'C1CONDITIONAL 'co1nth)
(put-sysprop 'nthcdr 'C1CONDITIONAL 'co1nthcdr)

(put-sysprop 'coerce 'C1CONDITIONAL 'co1coerce)
(put-sysprop 'cons 'C1CONDITIONAL 'co1cons)
(put-sysprop 'ldb 'C1CONDITIONAL 'co1ldb)

