;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: C -*-
;;;;
;;;;  CMPARRAY. Optimizations related to arrays

;;;;  Copyright (c) 2008. Juan Jose Garcia-Ripol.
;;;;  Copyright (c) 2011. Jean-Claude Beaudoin.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Lesser General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 3 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../../Copyright' for full details.

(in-package "COMPILER")

;;;
;;; MAKE-ARRAY
;;;

;; FIXME! Clashes with another compiler-macro in cmpprop.lsp!
(define-compiler-macro make-array (&whole form dimensions &key (element-type t)
					  (initial-element nil initial-element-supplied-p)
					  (initial-contents nil initial-contents-supplied-p)
					  adjustable fill-pointer
					  displaced-to (displaced-index-offset 0)
					  &environment env)
  (declare (ignore initial-element initial-contents))
  ;; This optimization is always done unless we provide content. There
  ;; is no speed, debug or space reason not to do it, unless the user
  ;; specifies not to inline MAKE-ARRAY, but in that case the compiler
  ;; macro should not be used.
  (unless (or initial-element-supplied-p
	      initial-contents-supplied-p)
    ;; If the type is known and we can assume it will not change, we
    ;; replace it with the upgraded form.
    (when (and (constantp element-type env)
	       (policy-assume-types-dont-change-p env))
      (let ((new-type (cmp-eval element-type)))
	(when (known-type-p new-type)
	  (setf element-type `',(upgraded-array-element-type new-type)))))
    ;; Finally, we choose between making a vector or making a general array.
    ;; It only saves some time, since MAKE-PURE-ARRAY will call MAKE-VECTOR
    ;; if a one-dimensional array is to be created.
    (let ((function 'si::make-pure-array))
      (when (constantp dimensions env)
	(let ((d (cmp-eval dimensions)))
	  (when (or (integerp d) (and (listp d) (= (length d) 1) (setf d (first d))))
	    (setf function 'si::make-vector
		  dimensions `',d)))
	(setf form
	      `(,function ,element-type ,dimensions ,adjustable ,fill-pointer
			  ,displaced-to ,displaced-index-offset)))))
  form)


;;;
;;; AREF/ASET
;;;

(define-compiler-macro aref (&whole form array &rest indices &environment env)
  (declare (ignore env))
  (if (<= (length indices) 1)
      form
    (if (symbolp array) ;; What about symbol-macros? JCB
	`(row-major-aref ,array (the fixnum (array-row-major-index ,array ,@indices)))
      (with-clean-symbols (.array.)
        `(let ((.array. ,array))
	   (row-major-aref .array. (the fixnum (array-row-major-index .array. ,@indices))))))))


(define-compiler-macro si:aset (&whole form value array &rest indices &environment env)
  (declare (ignore env))
  (if (<= (length indices) 1)
      form
    (if (symbolp array) ;; What about symbol-macros? JCB
	`(si:row-major-aset ,array (the fixnum (array-row-major-index ,array ,@indices)) ,value)
      (with-clean-symbols (.array.)
	`(let ((.array. ,array))
	   (si:row-major-aset .array. (the fixnum (array-row-major-index .array. ,@indices)) ,value))))))


(define-compiler-macro bit (&whole form array &rest indices &environment env)
  (declare (ignore env))
  `(aref ,array ,@indices))

(define-compiler-macro sbit (&whole form array &rest indices &environment env)
  (declare (ignore env))
  `(aref ,array ,@indices))

(define-compiler-macro si:bit-set (&whole form value array &rest indices &environment env)
  (declare (ignore env))
  `(si::aset ,value ,array ,@indices))

(define-compiler-macro si:sbit-set (&whole form value array &rest indices &environment env)
  (declare (ignore env))
  `(si::aset ,value ,array ,@indices))


(define-compiler-macro concatenate (&whole form target-type-form &rest sequences)
  (cond ((constantp target-type-form)
	 (let ((target-type (eval target-type-form)))
	   (case target-type
		 ((base-string #-unicode string) `(si:concatenate-base-strings ,@sequences))
		 #+unicode
		 (string `(si:concatenate-strings ,@sequences))
		 (t 
		  (cond ((subtypep target-type 'base-string)
			 `(si:concatenate-base-strings ,@sequences))
			#+unicode
			((subtypep target-type 'string)
			 `(si:concatenate-strings ,@sequences))
			(t form))))))
	(t form)))

