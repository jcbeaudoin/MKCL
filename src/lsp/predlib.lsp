;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: SYSTEM -*-
;;;;
;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;  Copyright (c) 2012, Jean-Claude Beaudoin.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Lesser General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 3 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../../Copyright' for full details.

;;;;                              predicate routines

(in-package "SYSTEM")

(defvar *subtypep-cache* (si:make-vector t 256 nil nil nil 0))

(defvar *upgraded-array-element-type-cache* (si:make-vector t 128 nil nil nil 0))

(defun subtypep-clear-cache ()
  (si:fill-array-with-elt *subtypep-cache* nil 0 nil)
  (si:fill-array-with-elt *upgraded-array-element-type-cache* nil 0 nil))

(defun create-type-name (name)
  (when (member name *alien-declarations*)
    (error "Symbol ~s is a declaration specifier and cannot be used to name a new type" name)))

(defun do-deftype (name form function)
  (unless (symbolp name)
    (error "~s is not a valid type specifier" name))
  (create-type-name name)
  (put-sysprop name 'DEFTYPE-FORM form)
  (put-sysprop name 'DEFTYPE-DEFINITION function)
  (subtypep-clear-cache)
  name)

;;; DEFTYPE macro.
(defmacro deftype (name lambda-list &rest body)
  "Syntax: (deftype name lambda-list {decl | doc}* {form}*)
Defines a new type-specifier abbreviation in terms of an 'expansion' function
	(lambda lambda-list1 {DECL}* {FORM}*)
where LAMBDA-LIST1 is identical to LAMBDA-LIST except that all optional
parameters with no default value specified in LAMBDA-LIST defaults to the
symbol '*', but not to NIL.  When the type system of MKCL encounters a type
specifier (NAME arg1 ... argn), it calls the expansion function with the
arguments ARG1 ... ARGn, and uses the returned value instead of the original
type specifier.  When the symbol NAME is used as a type specifier, the
expansion function is called with no argument.
The doc-string DOC, if supplied, is saved as a TYPE doc and can be retrieved
by (documentation 'NAME 'type)."
  (multiple-value-bind (body doc)
      (remove-documentation body)
    (setf lambda-list (copy-list lambda-list))
    (dolist (x '(&optional &key))
      (do ((l (rest (member x lambda-list)) (rest l)))
	  ((null l))
	(let ((variable (first l)))
	  (when (and (symbolp variable)
		     (not (member variable lambda-list-keywords)))
	    (setf (first l) `(,variable '*))))))
    `(define-when (:compile-toplevel :load-toplevel :execute)
       ,@(si::expand-set-documentation name 'type doc)
       (do-deftype ',name '(DEFTYPE ,name ,lambda-list ,@body)
		   #'(si::LAMBDA-BLOCK ,name ,lambda-list ,@body)))))


;;; Some DEFTYPE definitions.
(deftype boolean ()
  "A BOOLEAN is an object which is either NIL or T."
  `(member nil t))

(deftype index ()
  `(INTEGER 0 (#.array-dimension-limit)))

(deftype fixnum ()
  "A FIXNUM is an integer between MOST-NEGATIVE-FIXNUM and
MOST-POSITIVE-FIXNUM inclusive.  Other integers are bignums."
  `(INTEGER #.most-negative-fixnum #.most-positive-fixnum))
(deftype bignum ()
  '(OR (INTEGER * (#.most-negative-fixnum)) (INTEGER (#.most-positive-fixnum) *)))

(deftype bit ()
  "A BIT is either integer 0 or 1."
  '(INTEGER 0 1))

(deftype mod (n)
  `(INTEGER 0 ,(1- n)))

(deftype signed-byte (&optional s)
  "As a type specifier, (SIGNED-BYTE n) specifies those integers that can be
represented with N bits in 2's complement representation."
  (if (or (null s) (eq s '*))
      '(INTEGER * *)
      `(INTEGER ,(- (expt 2 (1- s))) ,(1- (expt 2 (1- s))))))

(deftype unsigned-byte (&optional s)
  "As a type specifier, (UNSIGNED-BYTE n) specifies non-negative integers that
can be represented with N bits."
  (if (or (null s) (eq s '*))
      '(INTEGER 0 *)
      `(INTEGER 0 ,(1- (expt 2 s)))))

;; (deftype mkcl::byte8 () '(INTEGER 0 255))
;; (deftype mkcl::integer8 () '(INTEGER -128 127))
;; (deftype mkcl::byte16 () '(INTEGER 0 #xFFFF))
;; (deftype mkcl::integer16 () '(INTEGER #x-8000 #x7FFF))
;; (deftype mkcl::byte32 () '(INTEGER 0 #xFFFFFFFF))
;; (deftype mkcl::integer32 () '(INTEGER #x-80000000 #x7FFFFFFF))
;; (deftype mkcl::byte64 () '(INTEGER 0 #xFFFFFFFFFFFFFFFF))
;; (deftype mkcl::integer64 () '(INTEGER #x-8000000000000000 #x7FFFFFFFFFFFFFFF))
(deftype mkcl::byte8 () '(UNSIGNED-BYTE 8)) ;; deprecated, use natural8
(deftype mkcl::natural8 () '(UNSIGNED-BYTE 8))
(deftype mkcl::octet () '(UNSIGNED-BYTE 8))
(deftype mkcl::integer8 () '(SIGNED-BYTE 8))
(deftype mkcl::byte16 () '(UNSIGNED-BYTE 16)) ;; deprecated, use natural16
(deftype mkcl::natural16 () '(UNSIGNED-BYTE 16))
(deftype mkcl::double-octet () '(UNSIGNED-BYTE 16))
(deftype mkcl::integer16 () '(SIGNED-BYTE 16))
(deftype mkcl::byte32 () '(UNSIGNED-BYTE 32)) ;; deprecated, use natural32
(deftype mkcl::natural32 () '(UNSIGNED-BYTE 32))
(deftype mkcl::integer32 () '(SIGNED-BYTE 32))
(deftype mkcl::byte64 () '(UNSIGNED-BYTE 64)) ;; deprecated, use natural64
(deftype mkcl::natural64 () '(UNSIGNED-BYTE 64))
(deftype mkcl::integer64 () '(SIGNED-BYTE 64))
(deftype mkcl::cl-word () '(SIGNED-BYTE #.CL-WORD-BITS))
(deftype mkcl::cl-index () '(UNSIGNED-BYTE #.CL-WORD-BITS))

(deftype mkcl::octets () '(vector mkcl::octet))
(deftype mkcl::double-octets () '(vector mkcl::double-octet))

(deftype real (&optional (start '* start-p) (end '*))
  (if start-p
      (let (rat-start
	    real-start
	    rat-end
	    real-end)
	(cond ((consp start)
	       (setf start (first start)
		     rat-start (list (rational start))
		     real-start (list (float start))))
	      ((numberp start)
	       (setf rat-start (rational start)
		     real-start (float start)))
	      (t
	       (setf rat-start start
		     real-start start)))
	(cond ((consp end)
	       (setf end (first end)
		     rat-end (list (rational end))
		     real-end (list (float end))))
	      ((numberp end)
	       (setf rat-end (rational end)
		     real-end (float end)))
	      (t
	       (setf rat-end end
		     real-end end)))
	`(OR (RATIONAL ,rat-start ,rat-end) (FLOAT ,real-start ,real-end)))
      '(OR RATIONAL FLOAT)))

#-short-float
(deftype short-float (&rest args)
  (if args
      `(single-float ,@args)
      'single-float))

#-long-float
(deftype long-float (&rest args)
  (if args
      `(double-float ,@args)
      'double-float))

(deftype null ()
  "The type to which only NIL belongs."
  '(MEMBER NIL))

(deftype sequence ()
  "A sequence is either a list or a vector."
  ;;'(OR CONS NULL (ARRAY * (*)))
  '(OR CONS NULL VECTOR)
  )

(deftype list ()
  "As a type specifier, LIST is used to specify the type consisting of NIL and
cons objects.  In our ordinary life with Lisp, however, a list is either NIL
or a cons whose cdr is a list, and is notated by its elements surrounded with
parentheses.
The backquote macro is sometimes useful to construct a complicated list
structure.  When evaluating `(...)
	,form embeds the value of FORM,
	,@form and ,.form embed all elements of the list value of FORM,
	and other things embed itself
into the structure at their position.  For example,
	`(a b ,c d e) expands to (list* 'a 'b c '(d e))
	`(a b ,@c d e) expands to (list* 'a 'b (append c '(d e)))
	`(a b ,.c d e) expands to (list* 'a 'b (nconc c '(d e)))"
  '(OR CONS NULL))

(deftype proper-list ()
  '(OR (CONS T PROPER-LIST) NULL))

(deftype property-list ()
  '(OR (CONS T (CONS T PROPERTY-LIST)) NULL))

(deftype atom ()
  "An ATOM is an object that is not a CONS."
  '(NOT CONS))

(deftype vector (&optional (element-type '*) (size '*))
  "A vector is a one-dimensional array.  Strings and bit-vectors are kinds of
vectors.  Other vectors are called general vectors and are notated as
	#(elem ... elem)
Some vectors may be displaced to another array, may have a fill-pointer, or
may be adjustable.  Other vectors are called simple-vectors."
  `(array ,element-type (,size)))

(deftype extended-char ()
  "A character which is not of type BASE-CHAR."
  '(and character (not base-char)))

(deftype string (&optional size)
  "A string is a vector of characters.  A string is notated by surrounding the
characters with double quotes.  Some strings may be displaced to another
string, may have a fill-pointer, or may be adjustable.  Other strings are
called simple-strings."
  #-unicode
  (if size `(array character (,size)) '(array character (*)))
  #+unicode
  (if size
      `(or (array base-char (,size))
	   (array character (,size)))
      '(or (array base-char (*)) (array character (*)))))

(deftype base-string (&optional size)
  "A string which is made of BASE-CHAR."
  (if size `(array base-char (,size)) '(array base-char (*))))

(deftype extended-string (&optional size)
  "A string which is nt a base string"
  #-unicode (declare (ignore size))
  #-unicode
  NIL
  #+unicode
  (if size `(array character (,size)) '(array character (*))))

(deftype bit-vector (&optional size)
  "A bit-vector is a vector of bits.  A bit-vector is notated by '#*' followed
by its elements (0 or 1).  Bit-vectors may be displaced to another array, may
have a fill-pointer, or may be adjustable.  Other bit-vectors are called
simple-bit-vectors.  Only simple-bit-vectors can be input in the above format
using '#*'."
  (if size `(array bit (,size)) '(array bit (*))))

(deftype simple-vector (&optional size)
  "A simple-vector is a vector that is not displaced to another array, has no
fill-pointer, and is not adjustable."
  (if size `(simple-array t (,size)) '(simple-array t (*))))

(deftype simple-string (&optional size)
  "A simple-string is a string that is not displaced to another array, has no
fill-pointer, and is not adjustable."
  #-unicode
  (if size
    `(simple-array character (,size))
    '(simple-array character (*)))
  #+unicode
  (if size
      `(or (simple-array base-char (,size))
	   (simple-array character (,size)))
      '(or (simple-array base-char (*)) (simple-array character (*)))))

(deftype simple-base-string (&optional size)
  "A base-string which cannot be adjusted nor displaced."
  (if size `(simple-array base-char (,size)) '(simple-array base-char (*))))

(deftype simple-bit-vector (&optional size)
  "A bit-vector that is not displaced to another array, has no fill-pointer,
and is not adjustable."
  (if size `(simple-array bit (,size)) '(simple-array bit (*))))


(deftype encoded-string ()
  `(or utf-8 utf-16))


;;************************************************************
;;			TYPEP
;;************************************************************

(defun constantly-t (&rest foo)
  (declare (ignore foo))
  t)

(defun constantly-nil (&rest foo)
  (declare (ignore foo))
  nil)

(defun simple-array-p (x)
  (and (arrayp x)
       (not (adjustable-array-p x))
       (not (array-has-fill-pointer-p x))
       (not (array-displacement x))))

(eval-when (:execute :load-toplevel :compile-toplevel)
  (defconstant +known-typep-predicates+
    '((ARRAY . ARRAYP)
      (ATOM . ATOM)
      (BASE-CHAR . BASE-CHAR-P)
      (BASE-STRING . BASE-STRING-P)
      ;(BIGNUM . SI::BIGNUMP)
      (BIT-VECTOR . BIT-VECTOR-P)
      (CHARACTER . CHARACTERP)
      (COMPILED-FUNCTION . COMPILED-FUNCTION-P)
      (COMPLEX . COMPLEXP)
      (CONS . CONSP)
      ;(DOUBLE-FLOAT . SI::DOUBLE-FLOAT-P)
      #-unicode
      (EXTENDED-CHAR . CONSTANTLY-NIL)
      (FLOAT . FLOATP)
      (FUNCTION . FUNCTIONP)
      (HASH-TABLE . HASH-TABLE-P)
      (INTEGER . INTEGERP)
      (FIXNUM . SI::FIXNUMP)
      (KEYWORD . KEYWORDP)
      (LIST . LISTP)
      (LOGICAL-PATHNAME . LOGICAL-PATHNAME-P)
      ;#+long-float
      ;(LONG-FLOAT . SI::LONG-FLOAT-P)
      (NIL . CONSTANTLY-NIL)
      (NULL . NULL)
      (NUMBER . NUMBERP)
      (PACKAGE . PACKAGEP)
      (PATHNAME . PATHNAMEP)
      (RANDOM-STATE . RANDOM-STATE-P)
      ;(RATIO . SI::RATIOP)
      (RATIONAL . RATIONALP)
      (READTABLE . READTABLEP)
      (REAL . REALP)
      (SIMPLE-ARRAY . SIMPLE-ARRAY-P)
      (SIMPLE-BASE-STRING . SIMPLE-BASE-STRING-P)
      (SIMPLE-STRING . SIMPLE-STRING-P)
      (SIMPLE-VECTOR . SIMPLE-VECTOR-P)
      (SIMPLE-BIT-VECTOR . SIMPLE-BIT-VECTOR-P)
      ;(SINGLE-FLOAT . SI::SINGLE-FLOAT-P)
      (STREAM . STREAMP)
      (STRING . STRINGP)
      (STRUCTURE . SYS:STRUCTUREP)
      (SYMBOL . SYMBOLP)
      (T . CONSTANTLY-T)
      (VECTOR . VECTORP))))

(dolist (l +known-typep-predicates+)
  (put-sysprop (car l) 'TYPE-PREDICATE (cdr l)))

(defconstant +upgraded-array-element-types+
  '#.(append '(NIL BASE-CHAR #+unicode CHARACTER BIT MKCL:INTEGER8 MKCL:NATURAL8)
             '(MKCL:INTEGER16 MKCL:NATURAL16)
             '(MKCL:INTEGER32 MKCL:NATURAL32)
             (when (< 32 cl-word-bits 64) '(MKCL::CL-WORD MKCL::CL-INDEX))
             '(MKCL:INTEGER64 MKCL:NATURAL64)
             (when (< 64 cl-word-bits) '(MKCL::CL-WORD MKCL::CL-INDEX))
             '(SINGLE-FLOAT DOUBLE-FLOAT T)))

(defconstant +fixed-array-element-types+
  '#.(append '(FIXNUM)
	     (unless (< 64 cl-word-bits) '(MKCL::CL-WORD MKCL::CL-INDEX))))

(defun upgraded-array-element-type (element-type &optional env)
  (declare (ignore env))
  (let* ((hash (logand 127 (si:hash-eql element-type)))
	 (record (aref *upgraded-array-element-type-cache* hash)))
    (declare (type (integer 0 127) hash))
    (if (and record (eq (car record) element-type))
	(cdr record)
	(let ((answer (cond ((member element-type +fixed-array-element-types+ :test #'eq)
			     element-type)
			    ((member element-type +upgraded-array-element-types+ :test #'eq)
			     element-type)
			    (t
			     (dolist (v +upgraded-array-element-types+ 'T)
			       (when (subtypep element-type v)
				 (return v)))))))
	  (setf (aref *upgraded-array-element-type-cache* hash)
		(cons element-type answer))
	  answer))))

(defun upgraded-complex-part-type (real-type &optional env)
  (declare (ignore env))
  ;; MKCL does not have specialized complex types. If we had them, the
  ;; code would look as follows
  ;;   (dolist (v '(INTEGER RATIO RATIONAL SINGLE-FLOAT DOUBLE-FLOAT FLOAT REAL)
  ;; 	   (error "~S is not a valid part type for a complex." real-type))
  ;;     (when (subtypep real-type v)
  ;;       (return v))))
  (unless (subtypep real-type 'REAL)
    (error "~S is not a valid part type for a complex." real-type))
  'REAL)

(defun in-interval-p (x interval)
  (let* (low high)
    (if (endp interval)
        (setq low '* high '*)
        (if (endp (cdr interval))
            (setq low (car interval) high '*)
            (setq low (car interval) high (second interval))))
    (cond ((eq low '*))
          ((consp low)
           (when (<= x (car low)) (return-from in-interval-p nil)))
          ((when (< x low) (return-from in-interval-p nil))))
    (cond ((eq high '*))
          ((consp high)
           (when (>= x (car high)) (return-from in-interval-p nil)))
          ((when (> x high) (return-from in-interval-p nil))))
    (return-from in-interval-p t)))

(defun error-type-specifier (type)
  (error "~S is not a valid type specifier." type))

(defun match-dimensions (array pat)
  (or (eq pat '*)
      (let ((rank (array-rank array)))
	(cond ((numberp pat) (= rank pat))
	      ((listp pat)
	       (dotimes (i rank (null pat))
		 (unless (and (consp pat)
			      (or (eq (car pat) '*)
				  (eql (array-dimension array i) (car pat))))
		   (return nil))
		 (setq pat (cdr pat))))
	      ((atom pat)
	       (error "~S does not describe array dimensions." pat))))))

#|
(defun typep (object type &optional env &aux tp i c)
  "Args: (object type)
Returns T if X belongs to TYPE; NIL otherwise."
  (declare (ignore env))
  (cond ((symbolp type)
	 (let ((f (get-sysprop type 'TYPE-PREDICATE)))
	   (cond (f (return-from typep (funcall f object)))
		 ((eq (type-of object) type) (return-from typep t))
		 (t (setq tp type i nil)))))
	((consp type)
	 (setq tp (car type) i (cdr type)))
	((sys:instancep type)
	 (return-from typep (si::subclassp (class-of object) type)))
	(t
	 (error-type-specifier type)))
  (case tp
    ((EQL MEMBER) (and (member object i) t))
    (NOT (not (typep object (car i))))
    (OR (dolist (e i)
	  (when (typep object e) (return t))))
    (AND (dolist (e i t)
	   (unless (typep object e) (return nil))))
    (SATISFIES (funcall (car i) object))
    ((T) t)
    ((NIL) nil)
    (BIGNUM (and (integerp object) (not (si::fixnump object))))
    (RATIO (eq (type-of object) 'RATIO))
    (STANDARD-CHAR
     (and (characterp object) (standard-char-p object)))
    (INTEGER
     (and (integerp object) (in-interval-p object i)))
    (RATIONAL
     (and (rationalp object) (in-interval-p object i)))
    (FLOAT
     (and (floatp object) (in-interval-p object i)))
    (REAL
     (and (or (rationalp object) (floatp object)) (in-interval-p object i)))
    ((SINGLE-FLOAT SHORT-FLOAT)
     (and (eq (type-of object) 'SINGLE-FLOAT) (in-interval-p object i)))
    ((DOUBLE-FLOAT #-long-float LONG-FLOAT)
     (and (eq (type-of object) 'DOUBLE-FLOAT) (in-interval-p object i)))
    #+long-float
    (LONG-FLOAT
     (and (eq (type-of object) 'LONG-FLOAT) (in-interval-p object i)))
    (COMPLEX
     (and (complexp object)
          (or (null i)
	      (and (typep (realpart object) (car i))
		   ;;wfs--should only have to check one.
		   ;;Illegal to mix real and imaginary types!
		   (typep (imagpart object) (car i))))
	   ))
    (SEQUENCE (or (listp object) (vectorp object)))
    (CONS (and (consp object)
	       (or (endp i)
		   (let ((car-type (first i)))
		     (or (eq car-type '*) (typep (car object) car-type))))
	       (or (endp (cdr i))
		   (let ((cdr-type (second i)))
		     (or (eq cdr-type '*) (typep (cdr object) cdr-type))))))

    ;;(CONS (and (consp object)
    ;;           (or (endp i) (typep (car object) (first i)))
    ;;           (or (endp (cdr i)) (typep (cdr object) (second i)))))
    (BASE-STRING
     (and (base-string-p object)
          (or (null i) (match-dimensions object i))))
    (STRING
     (and (stringp object)
          (or (null i) (match-dimensions object i))))
    (BIT-VECTOR
     (and (bit-vector-p object)
          (or (null i) (match-dimensions object i))))
    (SIMPLE-BASE-STRING
     (and (base-string-p object)
          (simple-string-p object)
	  (or (null i) (match-dimensions object i))))
    (SIMPLE-STRING
     (and (simple-string-p object)
          (or (null i) (match-dimensions object i))))
    (SIMPLE-BIT-VECTOR
     (and (simple-bit-vector-p object)
          (or (null i) (match-dimensions object i))))
    (SIMPLE-VECTOR
     (and (simple-vector-p object)
          (or (null i) (match-dimensions object i))))
    (SIMPLE-ARRAY
     (and (simple-array-p object)
          (or (endp i) (eq (car i) '*)
	      ;; (car i) needs expansion
	      (eq (array-element-type object)
		  (upgraded-array-element-type (car i))))
          (or (endp (cdr i)) (match-dimensions object (second i)))))
    (ARRAY
     (and (arrayp object)
          (or (endp i) (eq (car i) '*)
              ;; Or the element type of object should be EQUAL to (car i).
              ;; Is this too strict?
              (eq (array-element-type object)
		  (upgraded-array-element-type (car i))))
          (or (endp (cdr i)) (match-dimensions object (second i)))))
    (t
     (cond
           ((get-sysprop tp 'DEFTYPE-DEFINITION)
            (typep object (apply (get-sysprop tp 'DEFTYPE-DEFINITION) i)))
	   ((consp i)
	    (error-type-specifier type))
	   ((setq c (find-class type nil))
	    ;; Follow the inheritance chain
	    (si::subclassp (class-of object) c))
#|
	   #-clos
	   ((get-sysprop tp 'IS-A-STRUCTURE)
            (when (sys:structurep object)
	      ;; Follow the chain of structure-include.
	      (do ((stp (sys:structure-name object)
			(get-sysprop stp 'STRUCTURE-INCLUDE)))
		  ((eq tp stp) t)
		(when (null (get-sysprop stp 'STRUCTURE-INCLUDE))
		  (return nil)))))
|#
	   (t
	    (error-type-specifier type))))))
|#

;;; The implementation of this key performance critical predicate, that is #'typep, is ridiculously naive!
;;; We must soon do better than this! JCB
(defun typep-in-env (object type env &aux tp i c)
  "Args: (object type env)
Returns T if X belongs to TYPE; NIL otherwise."
  ;;(declare (ignore env))
  (cond ((symbolp type)
	 (let ((f (get-sysprop type 'TYPE-PREDICATE)))
	   (cond (f (return-from typep-in-env (funcall f object)))
		 ((eq (type-of object) type) (return-from typep-in-env t))
		 (t (setq tp type i nil)))))
	((consp type)
	 (setq tp (car type) i (cdr type)))
	((sys:instancep type)
	 (return-from typep-in-env (si::subclassp (class-of object) type)))
	(t
	 (error-type-specifier type)))
  (case tp
    (EQL (if (cdr i) (error-type-specifier type) (eql object (car i))))
    (MEMBER (and (member object i) t))
    (NOT (not (typep-in-env object (car i) env)))
    (OR (dolist (e i)
	  (when (typep-in-env object e env) (return t))))
    (AND (dolist (e i t)
	   (unless (typep-in-env object e env) (return nil))))
    (SATISFIES (if (funcall (car i) object) t nil))
    ((T) t)
    ((NIL) nil)
    (BIGNUM (and (integerp object) (not (si::fixnump object))))
    (RATIO (eq (type-of object) 'RATIO))
    (STANDARD-CHAR
     (and (characterp object) (standard-char-p object)))
    (CHARACTER (characterp object))
    (INTEGER
     (and (integerp object) (in-interval-p object i)))
    (RATIONAL
     (and (rationalp object) (in-interval-p object i)))
    (FLOAT
     (and (floatp object) (in-interval-p object i)))
    (REAL
     (and (or (rationalp object) (floatp object)) (in-interval-p object i)))
    ((SINGLE-FLOAT SHORT-FLOAT)
     (and (eq (type-of object) 'SINGLE-FLOAT) (in-interval-p object i)))
    ((DOUBLE-FLOAT #-long-float LONG-FLOAT)
     (and (eq (type-of object) 'DOUBLE-FLOAT) (in-interval-p object i)))
    #+long-float
    (LONG-FLOAT
     (and (eq (type-of object) 'LONG-FLOAT) (in-interval-p object i)))
    (COMPLEX
     (and (complexp object)
          (or (null i)
	      (and (typep-in-env (realpart object) (car i) env)
		   ;;wfs--should only have to check one.
		   ;;Illegal to mix real and imaginary types!
		   (typep-in-env (imagpart object) (car i) env)))
	   ))
    (SEQUENCE (or (listp object) (vectorp object)))
#+(and)
    (CONS (and (consp object)
	       (or (endp i)
		   (let ((car-type (first i)))
		     (or (eq car-type '*) (typep-in-env (car object) car-type env))))
	       (or (endp (cdr i))
		   (let ((cdr-type (second i)))
		     (or (eq cdr-type '*) (typep-in-env (cdr object) cdr-type env))))))
#+(or)
    (CONS (and (consp object)
    	       (or (endp i) (typep-in-env (car object) (first i) env))
    	       (or (endp (cdr i)) (typep-in-env (cdr object) (second i) env))))
    (BASE-STRING
     (and (base-string-p object)
          (or (null i) (match-dimensions object i))))
    (STRING
     (and (stringp object)
          (or (null i) (match-dimensions object i))))
    (BIT-VECTOR
     (and (bit-vector-p object)
          (or (null i) (match-dimensions object i))))
    (SIMPLE-BASE-STRING
     (and (base-string-p object)
          (simple-string-p object)
	  (or (null i) (match-dimensions object i))))
    (SIMPLE-STRING
     (and (simple-string-p object)
          (or (null i) (match-dimensions object i))))
    (SIMPLE-BIT-VECTOR
     (and (simple-bit-vector-p object)
          (or (null i) (match-dimensions object i))))
    (SIMPLE-VECTOR
     (and (simple-vector-p object)
          (or (null i) (match-dimensions object i))))
    (SIMPLE-ARRAY
     (and (simple-array-p object)
          (or (endp i) (eq (car i) '*)
	      ;; (car i) needs expansion
	      (eq (array-element-type object)
		  (upgraded-array-element-type (car i))))
          (or (endp (cdr i)) (match-dimensions object (second i)))))
    (ARRAY
     (and (arrayp object)
          (or (endp i) (eq (car i) '*)
              ;; Or the element type of object should be EQUAL to (car i).
              ;; Is this too strict?
              (eq (array-element-type object)
		  (upgraded-array-element-type (car i))))
          (or (endp (cdr i)) (match-dimensions object (second i)))))
    (t
     (let ((deftype-def (get-sysprop tp 'DEFTYPE-DEFINITION)))
       (cond (deftype-def
	       (typep-in-env object (apply deftype-def i) env))
	     ((consp i)
	      (error-type-specifier type))
	     ((setq c (find-class type nil))
	      ;; Follow the inheritance chain
	      (si::subclassp (class-of object) c))
	     #|
	     #-clos
	     ((get-sysprop tp 'IS-A-STRUCTURE)
	     (when (sys:structurep object)
	     ;; Follow the chain of structure-include. ;
	     (do ((stp (sys:structure-name object)
	     (get-sysprop stp 'STRUCTURE-INCLUDE)))
	     ((eq tp stp) t)
	     (when (null (get-sysprop stp 'STRUCTURE-INCLUDE))
	     (return nil)))))
	     |#
	     (t
	      (error-type-specifier type)))))))

(defun typep (object type &optional env)
  (typep-in-env object type env))


(declaim (ftype (function (class class) boolean) subclassp))
(defun subclassp (low high)
  (or (eq low high)
      (dolist (class (sys:instance-ref low 1)) ; (class-superiors low)
	(when (subclassp class high) (return t)))))

(defun of-class-p (object class)
  (declare (optimize (speed 3)))
  (macrolet ((class-precedence-list (x)
	       `(instance-ref ,x 4))
	     (class-name (x)
	       `(instance-ref ,x 0)))
    (let* ((x-class (class-of object)))
      (declare (class x-class))
      (if (eq x-class class)
	  t
	  (let ((x-cpl (class-precedence-list x-class)))
	    (if (instancep class)
		(member class x-cpl :test #'eq)
		(dolist (c x-cpl nil)
		  (declare (type (or null class) c))
		  (when (eq (class-name c) class)
		    (return t)))))))))


;;************************************************************
;;			NORMALIZE-TYPE
;;************************************************************
;; NORMALIZE-TYPE normalizes the type using the DEFTYPE definitions.
;; The result is a pair of values
;;  VALUE-1 = normalized type name or object
;;  VALUE-2 = normalized type arguments or nil
(defun normalize-type (type &aux tp i fd)
  ;; Loops until the car of type has no DEFTYPE definition.
  (cond ((symbolp type)
	 (if (setq fd (get-sysprop type 'DEFTYPE-DEFINITION))
	   (normalize-type (funcall fd))
	   (values type nil)))
	((clos::classp type) (values type nil))
	((atom type)
	 (error-type-specifier type))
	((progn
	   (setq tp (car type) i (cdr type))
	   (setq fd (get-sysprop tp 'DEFTYPE-DEFINITION)))
	 (normalize-type (apply fd i)))
	((and (eq tp 'INTEGER) (consp (cadr i)))
	 (values tp (list (car i) (1- (caadr i)))))
	(t (values tp i))))

(defun expand-deftype (type)
  (cond ((symbolp type)
	 (let ((fd (get-sysprop type 'DEFTYPE-DEFINITION)))
	   (if fd
	       (expand-deftype (funcall fd))
	       type)))
	((and (consp type)
	      (symbolp type))
	 (let ((fd (get-sysprop (first type) 'DEFTYPE-DEFINITION)))
	   (if fd
	       (expand-deftype (funcall fd (rest type)))
	       type)))
	(t
	 type)))

;;************************************************************
;;			COERCE
;;************************************************************

(defun error-coerce (object type)
  (error "Cannot coerce ~S to type ~S." object type))

(defun coerce (object type &aux aux)
  "Args: (x type)
Coerces X to an object of the specified type, if possible.  Signals an error
if not possible."
  (when (typep-in-env object type nil)
    ;; Just return as it is.
    (return-from coerce object))
  (setq type (expand-deftype type))
  (cond ((atom type)
	 (case type
	   ((T) object)
	   (LIST
	    (do ((io (make-seq-iterator object) (seq-iterator-next object io))
	         (l nil (cons (seq-iterator-ref object io) l)))
	        ((null io) l)))
	   ((CHARACTER BASE-CHAR) (character object))
	   (FLOAT (float object))
	   (SINGLE-FLOAT (float object 0.0F0))
	   (SHORT-FLOAT (float object 0.0S0))
	   (DOUBLE-FLOAT (float object 0.0D0))
	   (LONG-FLOAT (float object 0.0L0))
	   (COMPLEX (complex (realpart object) (imagpart object)))
	   (FUNCTION (coerce-to-function object))
	   ((VECTOR SIMPLE-VECTOR
	     #+unicode SIMPLE-BASE-STRING SIMPLE-STRING
	     #+unicode BASE-STRING STRING
	     BIT-VECTOR SIMPLE-BIT-VECTOR)
	    (concatenate type object))
	   (t
	    (if (or (listp object) (vector object))
		(concatenate type object)
		(error-coerce object type)))))
	((eq (setq aux (first type)) 'COMPLEX)
	 (if type
	     (complex (coerce (realpart object) (second type))
		      (coerce (imagpart object) (second type)))
	     (complex (realpart object) (imagpart object))))
	((member aux '(SINGLE-FLOAT SHORT-FLOAT DOUBLE-FLOAT LONG-FLOAT FLOAT))
	 (setq aux (coerce object aux))
	 (unless (typep-in-env aux type nil)
	   (error-coerce object type))
	 aux)
	((eq aux 'AND)
	 (dolist (type (rest type))
	   (setq aux (coerce aux type)))
	 (unless (typep-in-env aux type nil)
	   (error-coerce object type))
	 aux)
	((or (listp object) (vector object))
	 (concatenate type object))
	(t
	 (error-coerce object type))))

;;************************************************************
;;			SUBTYPEP
;;************************************************************
;;
;; TYPES LATTICE (Following Henry Baker's paper)
;;
;; The algorithm works as follows. Types are identified with sets. Some sets
;; are elementary, in the sense that other types may be expressed as
;; combination of them. We partition these sets into FAMILIES
;;
;;	Built-in objects --- Hash tables, etc
;;	Intervals --- (INTEGER a b), (REAL a b), etc
;;	Arrays --- (ARRAY * (2)), etc
;;	Classes
;;
;; When passed a type specifier, MKCL canonicalizes it: it decomposes the
;; type into the most elementary sets, assigns a unique bit pattern (TAG) to
;; each of these sets, and builds a composite tag for the type by LOGIOR.
;; Operations between these sets reduce to logical operations between these
;; bit patterns. Given types T1, T2 and a function which produces tags f(T)
;;
;;	f((AND T1 T2)) = (LOGIAND f(T1) f(T2))
;;	f((OR T1 T2)) = (LOGIOR f(T1) f(T2))
;;	f((NOT T1)) = (LOGNOT f(T2))
;;
;; However, tags are not permanent: whenever a new type is registered, the
;; tag associated to a type may be changed (for instance, because new
;; elementary sets are discovered, which also belong to existing types).

(defparameter *save-types-database* nil)

(defparameter *highest-type-tag*
  #+mkcl-min #B1
  #-mkcl-min '#.*highest-type-tag*)

(defparameter *member-types*
  #+mkcl-min NIL
  #-mkcl-min '#.*member-types*)

(defparameter *intervals-mask* #B1)

(defparameter *elementary-types*
  #+mkcl-min
  '()
  #-mkcl-min
  '#.*elementary-types*)

(defun new-type-tag ()
  (prog1 *highest-type-tag*
    (setq *highest-type-tag* (ash *highest-type-tag* 1))))

;; Find out the tag for a certain type, if it has been already registered.
;;
#|
	(defun find-registered-tag (type &optional (test #'equal))
(let* ((pos (assoc type *elementary-types* :test test)))
(and pos (cdr pos))))
	|#

(defun find-registered-tag (type)
  (let* ((pos (assoc type *elementary-types* :test #'equal)))
    (and pos (cdr pos))))

(defun find-registered-tag-equalp (type)
  (let* ((pos (assoc type *elementary-types* :test #'equalp)))
    (and pos (cdr pos))))

;; We are going to make changes in the types database. Save a copy if this
;; will cause trouble.
;;
(defun maybe-save-types ()
  (when *save-types-database*
    (setf *save-types-database* nil
	  *elementary-types* (copy-tree *elementary-types*)
	  *member-types* (copy-tree *member-types*))))

;; We have created and tagged a new type (NEW-TAG). However, there are
;; composite and synonym types registered around which are supertypes of
;; this type and need to be tagged. TYPE-MASK is a bit pattern which helps
;; us in recognizing these supertypes.
;;
(defun update-types (type-mask new-tag)
  (maybe-save-types)
  (dolist (i *elementary-types*)
    (declare (list i))
    (unless (zerop (logand (cdr i) type-mask))
      (setf (cdr i) (logior new-tag (cdr i))))))

;; FIND-TYPE-BOUNDS => (VALUES TAG-SUPER TAG-SUB)
;;
;; This function outputs two values: TAG-SUB, the tag for the union-type of all
;; types which are subtypes of the supplied one; and TAG-SUPER, which is either
;; the tag for the union-type of all types which a supertype of the supplied
;; one (MINIMIZE-SUPER = NIL) or the tag for the smallest type which is a
;; supertype of the given one (MINIMIZE-SUPER = TRUE). The search process is
;; restricted to types in the same family class.
;;
;; A value of MINIMIZE-SUPER = TRUE only makes sense for some families (such
;; as semi-infinite intervals), for which (SUBTYPEP T1 T2) = T and (SUBTYPEP T1
;; T3) = T implies either (SUBTYPEP T2 T3) = T or (SUBTYPEP T3 T2) = T.
;;
(defun find-type-bounds (type in-our-family-p type-<= minimize-super)
  (declare (function in-our-family-p type-<=)) 
  (let* ((subtype-tag 0)
	 (disjoint-tag 0)
	 (supertype-tag (if minimize-super -1 0)))
    (dolist (i *elementary-types*)
      (declare (list i))
      (let ((other-type (car i))
	    (other-tag (cdr i)))
	(when (funcall in-our-family-p other-type)
	  (cond ((funcall type-<= type other-type)
		 (if minimize-super
		     (when (zerop (logandc2 other-tag supertype-tag))
		       (setq supertype-tag other-tag))
		     (setq supertype-tag (logior other-tag supertype-tag))))
		((funcall type-<= other-type type)
		 (setq subtype-tag (logior other-tag subtype-tag)))
		(t
		 (setq disjoint-tag (logior disjoint-tag other-tag)))))))
    (values (if (= supertype-tag -1) 0
		(logandc2 supertype-tag (logior disjoint-tag subtype-tag)))
	    subtype-tag)))

;; A new type is to be registered, which is not simply a composition of
;; previous types. A new tag has to be created, and all supertypes are to be
;; tagged. Here we have to distinguish two possibilities: first, a supertype
;; may belong to the same family (intervals, arrays, etc); second, some
;; supertypes may be basic types (NUMBER is a supertype for (INTEGER 0 2),
;; for instance). The first possibility is detected with the comparison
;; procedure, TYPE-<=; the second possibility is detected by means of tags.
;;
(defun register-type (type in-our-family-p type-<=)
  (declare (function in-our-family-p type-<=))
  (or (find-registered-tag type)
      (multiple-value-bind (tag-super tag-sub)
	  (find-type-bounds type in-our-family-p type-<= nil)
	(let ((tag (new-type-tag)))
	  (update-types (logandc2 tag-super tag-sub) tag)
	  (setf tag (logior tag tag-sub))
	  (push-type type tag)
	  tag))))

;;----------------------------------------------------------------------
;; MEMBER types. We register this object in a separate list, *MEMBER-TYPES*,
;; and tag all types to which it belongs. We need to treat three cases
;; separately
;;	- Ordinary types, via simple-member-type, check the objects
;;	  against all pre-registered types, adding their tags.
;;	- Ordinary numbers, are translated into intervals.
;;	- Floating point zeros, have to be treated separately. This
;;	  is done by assigning a special tag to -0.0 and translating
;;	  (MEMBER 0.0) = (AND (float-type 0.0 0.0) (NOT (MEMBER -0.0)))
;;
(defun register-member-type (object)
  (let ((pos (assoc object *member-types*)))
    (cond ((and pos (cdr pos)))
	  ((not (realp object))
	   (simple-member-type object))
	  ((and (floatp object) (zerop object))
	   #.(if (eql (- 0.0) 0.0)
		 '(number-member-type object)
		 '(if (minusp (float-sign object))
		      (simple-member-type object)
		      (logandc2 (number-member-type object)
			        (register-member-type (- object))))))
	  (t
	   (number-member-type object)))))

(defun simple-member-type (object)
  (let* ((tag (new-type-tag)))
    (maybe-save-types)
    (setq *member-types* (acons object tag *member-types*))
    (dolist (i *elementary-types*)
      (let ((type (car i)))
	(when (typep-in-env object type nil)
	  (setf (cdr i) (logior tag (cdr i))))))
    tag))

;; We convert number into intervals, so that (AND INTEGER (NOT (EQL
;; 10))) is detected as a subtype of (OR (INTEGER * 9) (INTEGER 11
;; *)).
(defun number-member-type (object)
  (let* ((base-type (if (integerp object) 'INTEGER (type-of object)))
	 (type (list base-type object object)))
    (or (find-registered-tag type)
	(register-interval-type type))))

(defun push-type (type tag)
  (dolist (i *member-types*)
    (declare (list i))
    (when (typep-in-env (car i) type nil)
      (setq tag (logior tag (cdr i)))))
  (push (cons type tag) *elementary-types*))

;;----------------------------------------------------------------------
;; SATISFIES types. Here we should signal some error which is caught
;; somewhere up, to denote failure of the decision procedure.
;;
(defun register-satisfies-type (type)
  (declare (ignore type))
  (throw '+canonical-type-failure+ 'satisfies))

;;----------------------------------------------------------------------
;; CLOS classes and structures.
;;
(defun register-class (class)
  (declare (notinline class-name))
  (or (find-registered-tag class)
      ;; We do not need to register classes which belong to the core type
      ;; system of LISP (ARRAY, NUMBER, etc).
      (let* ((name (class-name class)))
	(and name
	     (eq class (find-class name 'nil))
	     (or (find-registered-tag name)
		 (find-built-in-tag name))))
      (register-type class
		     #'(lambda (c) (or (si::instancep c) (symbolp c)))
		     #'(lambda (c1 c2)
			 (when (symbolp c1)
			   (setq c1 (find-class c1 nil)))
			 (when (symbolp c2)
			   (setq c2 (find-class c2 nil)))
			 (and c1 c2 (si::subclassp c1 c2))))))

;;----------------------------------------------------------------------
;; ARRAY types.
;;
(defun register-array-type (type)
  (multiple-value-bind (array-class elt-type dimensions)
      (parse-array-type type)
    (cond ((eq elt-type '*)
	   (canonical-type `(OR ,@(mapcar #'(lambda (type) `(,array-class ,type ,dimensions))
					  +upgraded-array-element-types+)
				,@(mapcar #'(lambda (type) `(,array-class ,type ,dimensions))
					  +fixed-array-element-types+)
				)))
	  ((find-registered-tag (setq type (list array-class elt-type dimensions))))
	  (t
	   #+nil
	   (when (and (consp dimensions) (> (count-if #'numberp dimensions) 1))
	     (dotimes (i (length dimensions))
	       (when (numberp (elt dimensions i))
		 (let ((dims (make-list (length dimensions) :initial-element '*)))
		   (setf (elt dims i) (elt dimensions i))
		   (register-type (list array-class elt-type dims)
				  #'array-type-p #'array-type-<=)))))
	   (register-type type #'array-type-p #'array-type-<=)))))

;;
;; We look for the most specialized type which is capable of containing
;; this object. LIST always contains 'T, so that this procedure never
;; fails. It is faster than UPGRADED-... because we use the tags of types
;; that have been already registered.
;;
(defun fast-upgraded-array-element-type (type)
  (cond ((eql type '*) '*)
	((member type +fixed-array-element-types+ :test #'eq)
	 type)
	((member type +upgraded-array-element-types+ :test #'eq)
	 type)
	(t
	 (dolist (other-type +upgraded-array-element-types+ 'T)
	   (when (fast-subtypep type other-type)
	     (return other-type))))))

;;
;; This canonicalizes the array type into the form
;;	({ARRAY | SIMPLE-ARRAY} {elt-type | '*} {'* | (['*]*)})
;;
;; ELT-TYPE is the upgraded element type of the input.
;;
(defun parse-array-type (input)
  (let* ((type input)
	 (name (pop type))
	 (elt-type (fast-upgraded-array-element-type (if type (pop type) '*)))
	 (dims (if type (pop type) '*)))
    (when type
      (error "Wrong array type designator ~S." input))
    (cond ((numberp dims)
	   (unless (< -1 dims array-rank-limit)
	     (error "Wrong rank size array type ~S." input))
	   (setq dims (nthcdr (- array-rank-limit dims)
			      '#.(make-list array-rank-limit :initial-element '*))))
	  ((consp dims)
	   (dolist (i dims)
	     (unless (or (eq i '*)
			 (and (integerp i) (< -1 i array-dimension-limit)))
	       (error "Wrong dimension size in array type ~S." input)))))
    (values name elt-type dims)))

;;
;; This function checks whether the array type T1 is a subtype of the array
;; type T2.
;;
(defun array-type-<= (t1 t2)
  (unless (and (or (eq (first t1) (first t2))
		   (eq (first t2) 'ARRAY))
	       (eq (second t1) (second t2)))
    (return-from array-type-<= nil))
  (let ((dim (third t1))
	(pat (third t2)))
    (cond ((eq pat '*) t)
	  ((eq dim '*) nil)
	  (t (do ((a dim (cdr a))
		  (b pat (cdr b)))
		 ((or (endp a)
		      (endp b)
		      (not (or (eq (car b) '*)
			       (eql (car b) (car a)))))
		  (and (null a) (null b)))
	       )))))

(defun array-type-p (type)
  (and (consp type)
       ;;(member (first type) '(ARRAY SIMPLE-ARRAY))
       (case (first type) ((ARRAY SIMPLE-ARRAY) t) (t nil))
       ))

;;----------------------------------------------------------------------
;; INTERVALS:
;;
;; Arbitrary intervals may be defined as the union or intersection of
;; semi-infinite intervals, of the form (number-type b *), where B is
;; either a real number, a list with one real number or *.
;; Any other interval, may be defined using these. For instance
;;  (INTEGER 0 2) = (AND (INTEGER 0 *) (NOT (INTEGER (2) *)))
;;  (SHORT-FLOAT (0.2) (2)) = (AND (SHORT-FLOAT (0.2) *) (NOT (SHORT-FLOAT 2 *)))

(defun register-elementary-interval (type b)
  (setq type (list type b))
  (or (find-registered-tag-equalp type) ;;(find-registered-tag type #'equalp)
      (multiple-value-bind (tag-super tag-sub)
	  (find-type-bounds type
			    #'(lambda (other-type)
				(and (consp other-type)
				     (null (cddr other-type))))
			    #'(lambda (i1 i2)
				(and (eq (first i1) (first i2))
				     (bounds-<= (second i2) (second i1))))
			    t)
	(let ((tag (new-type-tag)))
	  (update-types (logandc2 tag-super tag-sub) tag)
	  (setq tag (logior tag tag-sub))
	  (push-type type tag)
	  tag))))

(defun register-interval-type (interval)
  (let* ((i interval)
	 (type (pop i))
	 (low (if i (pop i) '*))
	 (high (if i (pop i) '*))
	 (tag-high (cond ((eq high '*)
			  0)
			 ((eq type 'INTEGER)
			  (setq high (if (consp high)
					 (ceiling (first high))
					 (floor (1+ high))))
			  (register-elementary-interval type high))
			 ((consp high)
			  (register-elementary-interval type (first high)))
			 (t
			  (register-elementary-interval type (list high)))))
	 (tag-low (register-elementary-interval type
		    (cond ((or (eq '* low) (not (eq type 'INTEGER)) (integerp low))
			   low)
			  ((consp low)
			   (floor (1+ (first low))))
			  (t
			   (ceiling low)))))
	 (tag (logandc2 tag-low tag-high)))
    (unless (eq high '*)
      (push-type interval tag))
    tag))

;; All comparisons between intervals operations may be defined in terms of
;;
;;	(BOUNDS-<= b1 b2)	and	(BOUNDS-< b1 b2)
;;
;; The first one checks whether (REAL b2 *) is contained in (REAL b1 *). The
;; second one checks whether (REAL b2 *) is strictly contained in (REAL b1 *)
;; (i.e., (AND (REAL b1 *) (NOT (REAL b2 *))) is not empty).
;;
(defun bounds-<= (b1 b2)
  (cond ((eq b1 '*) t)
	((eq b2 '*) nil)
	((consp b1)
	 (if (consp b2)
	     (<= (first b1) (first b2))
	     (< (first b1) b2)))
	((consp b2)
	 (<= b1 (first b2)))
	(t
	 (<= b1 b2))))

(defun bounds-< (b1 b2)
  (cond ((eq b1 '*) (not (eq b2 '*)))
	((eq b2 '*) nil)
	((consp b1)
	 (if (consp b2)
	     (< (first b1) (first b2))
	     (< (first b1) b2)))
	((consp b2)
	 (<= b1 (first b2)))
	(t
	 (< b1 b2))))

;;----------------------------------------------------------------------
;; COMPLEX types. We do not need to register anything, because all
;; possibilities have been covered by the definitions above. We only have to
;; bring the type to canonical form, which is a union of all specialized
;; complex types that can store an element of the corresponding type.
;;
(defun canonical-complex-type (real-type)
  ;; UPGRADE-COMPLEX-PART-TYPE will signal an error if REAL-TYPE
  ;; is not a subtype of REAL.
  (unless (eq real-type '*)
    (upgraded-complex-part-type real-type))
  (or (find-registered-tag '(COMPLEX REAL))
      (let ((tag (new-type-tag)))
	(push-type '(COMPLEX REAL) tag)
	tag))
  #+(or)
  (case real-type
    ((SINGLE-FLOAT DOUBLE-FLOAT INTEGER RATIO LONG-FLOAT)
     (let ((tag (new-type-tag)))
       (push-type `(COMPLEX ,real-type) tag)
       tag))
    ((RATIONAL) (canonical-type '(OR (COMPLEX INTEGER) (COMPLEX RATIO))))
    ((FLOAT) (canonical-type '(OR (COMPLEX SINGLE-FLOAT) (COMPLEX DOUBLE-FLOAT)
				  (COMPLEX LONG-FLOAT))))
    ((* NIL REAL) (canonical-type
		   '(OR (COMPLEX INTEGER) (COMPLEX RATIO)
		        (COMPLEX SINGLE-FLOAT) (COMPLEX DOUBLE-FLOAT)
			(COMPLEX LONG-FLOAT))))
    (otherwise (canonical-complex-type (upgraded-complex-part-type real-type)))))

;;----------------------------------------------------------------------
;; CONS types. Only (CONS T T) and variants, as well as (CONS NIL *), etc
;; are strictly supported.
;;
(defun register-cons-type (&optional (car-type '*) (cdr-type '*))
  (let ((car-tag (if (eq car-type '*) -1 (canonical-type car-type)))
	(cdr-tag (if (eq cdr-type '*) -1 (canonical-type cdr-type))))
    (cond ((or (zerop car-tag) (zerop cdr-tag))
	   0)
	  ((and (= car-tag -1) (= cdr-tag -1))
	   (canonical-type 'CONS))
	  (t
	   (throw '+canonical-type-failure+ 'CONS)))))

;;----------------------------------------------------------------------
;; FIND-BUILT-IN-TAG
;;
;; This function computes the tags for all builtin types. We used to
;; do this computation and save it. However, for most cases it seems
;; faster if we just repeat it every time we need it, because the list of
;; *elementary-types* is kept smaller and *highest-type-tag* may be just
;; a fixnum.
;;
;; Note 1: There is some redundancy between this and the built-in
;; classes definitions. REGISTER-CLASS knows this and calls
;; FIND-BUILT-IN-TAG, which has priority. This is because some built-in
;; classes are also interpreted as intervals, arrays, etc.
;;
;; Note 2: All built in types listed here have to be symbols.
;;
#+mkcl-min
(defconstant +built-in-types+
	     '((SYMBOL)
	       (KEYWORD NIL SYMBOL)
	       (PACKAGE)
	       (COMPILED-FUNCTION)
	       (FUNCTION (OR COMPILED-FUNCTION GENERIC-FUNCTION))

	       (INTEGER (INTEGER * *))
	       
	       (SINGLE-FLOAT (SINGLE-FLOAT * *))
	       (DOUBLE-FLOAT (DOUBLE-FLOAT * *))
	       (LONG-FLOAT (LONG-FLOAT * *))

	       (RATIO (RATIO * *))

	       (RATIONAL (OR INTEGER RATIO))
	       (FLOAT (OR SINGLE-FLOAT DOUBLE-FLOAT LONG-FLOAT))
	       (REAL (OR INTEGER SINGLE-FLOAT DOUBLE-FLOAT RATIO))
	       (COMPLEX (COMPLEX REAL))

	       (NUMBER (OR REAL COMPLEX))

	       (CHARACTER)
               #-unicode
	       (BASE-CHAR CHARACTER)
               #+unicode
	       (BASE-CHAR NIL CHARACTER)
	       (STANDARD-CHAR NIL BASE-CHAR)

	       (CONS)
	       (NULL (MEMBER NIL))
	       (LIST (OR CONS (MEMBER NIL)))

	       (ARRAY (ARRAY * *))
 	       (SIMPLE-ARRAY (SIMPLE-ARRAY * *))
	       (SIMPLE-VECTOR (SIMPLE-ARRAY T (*)))
	       (SIMPLE-BIT-VECTOR (SIMPLE-ARRAY BIT (*)))
	       (VECTOR (ARRAY * (*)))
	       (STRING (ARRAY CHARACTER (*)))
               #+unicode
	       (BASE-STRING (ARRAY BASE-CHAR (*)))
	       (SIMPLE-STRING (SIMPLE-ARRAY CHARACTER (*)))
               #+unicode
	       (SIMPLE-BASE-STRING (SIMPLE-ARRAY BASE-CHAR (*)))
	       (BIT-VECTOR (ARRAY BIT (*)))

	       (SEQUENCE (OR CONS (MEMBER NIL) (ARRAY * (*))))

	       (HASH-TABLE)
	       (PATHNAME)
	       (LOGICAL-PATHNAME NIL PATHNAME)

	       (BROADCAST-STREAM)
	       (CONCATENATED-STREAM)
	       (ECHO-STREAM)
	       (FILE-STREAM)
	       (STRING-STREAM)
	       (SYNONYM-STREAM)
 	       (TWO-WAY-STREAM)
	       (STREAM (OR BROADCAST-STREAM CONCATENATED-STREAM ECHO-STREAM
			   FILE-STREAM STRING-STREAM SYNONYM-STREAM TWO-WAY-STREAM
			   GRAY:FUNDAMENTAL-STREAM))

	       (READTABLE)
	       (MT::THREAD)
	       (MT::LOCK)
	       (FOREIGN)
               (CODE-BLOCK)
	       ))

(defun find-built-in-tag (name)
  (when (eq name T)
    (return-from find-built-in-tag -1))
  (dolist (i '#.+built-in-types+)
    (declare (list i))
    (when (eq name (first i))
      (let* ((alias (second i))
	     (strict-supertype (or (third i) 'T))
	     (tag))
	(if alias
	    (setq tag (canonical-type alias))
	    (let* ((strict-supertype-tag (canonical-type strict-supertype)))
	      (setq tag (new-type-tag))
	      (unless (eq strict-supertype 't)
		(extend-type-tag tag strict-supertype-tag))))
	(push-type name tag)
	(return-from find-built-in-tag tag)
	)))
  nil)

(defun extend-type-tag (tag minimal-supertype-tag)
  (dolist (type *elementary-types*)
    (let ((other-tag (cdr type)))
      (when (zerop (logandc2 minimal-supertype-tag other-tag))
	(setf (cdr type) (logior tag other-tag))))))

;;----------------------------------------------------------------------
;; (CANONICAL-TYPE TYPE)
;;
;; This function registers all types mentioned in the given expression,
;; and outputs a code corresponding to the represented type. This
;; function has side effects: it destructively modifies the content of
;; *ELEMENTARY-TYPES* and *MEMBER-TYPES*.
;;
(defun canonical-type (type)
  (cond ((find-registered-tag type))
	((eq type 'T) -1)
	((eq type 'NIL) 0)
        ((symbolp type)
	 (let ((expander (get-sysprop type 'DEFTYPE-DEFINITION)))
	   (cond (expander
		  (canonical-type (funcall expander)))
		 ((find-built-in-tag type))
		 (t (let ((class (find-class type nil)))
		      (if class
			  (register-class class)
			  (throw '+canonical-type-failure+ nil)))))))
	((consp type)
	 (case (first type)
	   (AND (apply #'logand (mapcar #'canonical-type (rest type))))
	   (OR (apply #'logior (mapcar #'canonical-type (rest type))))
	   (NOT (lognot (canonical-type (second type))))
	   ((EQL MEMBER) (apply #'logior (mapcar #'register-member-type (rest type))))
	   (SATISFIES (register-satisfies-type type))
	   ((INTEGER SINGLE-FLOAT DOUBLE-FLOAT RATIO #+long-float LONG-FLOAT)
	    (register-interval-type type))
	   ((FLOAT)
	    (canonical-type `(OR (SINGLE-FLOAT ,@(rest type))
			      (DOUBLE-FLOAT ,@(rest type)))))
	   ((REAL)
	    (canonical-type `(OR (INTEGER ,@(rest type))
			      (RATIO ,@(rest type))
			      (SINGLE-FLOAT ,@(rest type))
			      (DOUBLE-FLOAT ,@(rest type)))))
	   ((RATIONAL)
	    (canonical-type `(OR (INTEGER ,@(rest type))
			      (RATIO ,@(rest type)))))
	   (COMPLEX
	    (or (find-built-in-tag type)
		(canonical-complex-type (second type))))
	   (CONS (apply #'register-cons-type (rest type)))
	   ((ARRAY SIMPLE-ARRAY) (register-array-type type))
	   ;;(FUNCTION (register-function-type type))
	   ;;(VALUES (register-values-type type))
	   (FUNCTION (canonical-type 'FUNCTION))
	   (t (let ((expander (get-sysprop (first type) 'DEFTYPE-DEFINITION)))
		(if expander
		    (canonical-type (apply expander (rest type)))
		    (unless (assoc (first type) *elementary-types*)
		      (throw '+canonical-type-failure+ nil)))))))
	((clos::classp type)
	 (register-class type))
	((and (fboundp 'function-type-p) (function-type-p type))
	 (register-function-type type))
	((and (fboundp 'values-type-p) (values-type-p type))
	 (register-values-type type))
	(t
	 (error-type-specifier type))))

(defun safe-canonical-type (type)
  (catch '+canonical-type-failure+
    (canonical-type type)))

(defun fast-subtypep (t1 t2)
  (when (eq t1 t2)
    (return-from fast-subtypep (values t t)))
  (let* ((tag1 (safe-canonical-type t1))
	 (tag2 (safe-canonical-type t2)))
    (cond ((and (numberp tag1) (numberp tag2))
	   (values (zerop (logandc2 (safe-canonical-type t1)
				    (safe-canonical-type t2)))
		   t))
	  #+nil
	  ((null tag1)
	   (error "Unknown type specifier ~S." t1))
	  #+nil
	  ((null tag2)
	   (error "Unknown type specifier ~S." t2))
	  (t
	   (values nil nil)))))

(defun subtypep-in-env (t1 t2 &optional env)
  (declare (ignore env))
  ;; One easy case: types are equal
  (when (eq t1 t2)
    (return-from subtypep-in-env (values t t)))
  ;; Another easy case: types are classes.
  (when (and (instancep t1) (instancep t2)
	     (clos::classp t1) (clos::classp t2))
    (return-from subtypep-in-env (values (subclassp t1 t2) t)))
  ;; Finally, cached results.
  (let* ((cache *subtypep-cache*)
	 (hash (logand (hash-eql t1 t2) 255))
	 (elt (aref cache hash)))
    (declare (type (integer 0 255) hash))
    (when (and elt (eq (caar elt) t1) (eq (cdar elt) t2))
      (setf elt (cdr elt))
      (return-from subtypep-in-env (values (car elt) (cdr elt))))
    (let* ((*highest-type-tag* *highest-type-tag*)
	   (*save-types-database* t)
	   (*member-types* *member-types*)
	   (*elementary-types* *elementary-types*))
      (multiple-value-bind (test confident)
	  (fast-subtypep t1 t2)
	(setf (aref cache hash) (cons (cons t1 t2) (cons test confident)))
	(values test confident)))))

(defun subtypep (t1 t2 &optional env)
  (subtypep-in-env t1 t2 env))

(defun fast-type= (t1 t2)
  (when (eq t1 t2)
    (return-from fast-type= (values t t)))
  (let* ((tag1 (safe-canonical-type t1))
	 (tag2 (safe-canonical-type t2)))
    (cond ((and (numberp tag1) (numberp tag2))
	   (values (= (safe-canonical-type t1) (safe-canonical-type t2))
		   t))
	  #+nil
	  ((null tag1)
	   (error "Unknown type specifier ~S." t1))
	  #+nil
	  ((null tag2)
	   (error "Unknown type specifier ~S." t2))
	  (t
	   (values nil nil)))))

(defun type= (t1 t2)
  (let ((*highest-type-tag* *highest-type-tag*)
	(*save-types-database* t)
	(*member-types* *member-types*)
	(*elementary-types* *elementary-types*))
    (fast-type= t1 t2)))


;;;;

;;;-------------------------------------------------------------------------------
;;;
;;; 




