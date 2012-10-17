;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: C -*-
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

;;;; CMPPROP Type propagation.

(in-package "COMPILER")

(defun type-from-array-elt (array)
  "Input is a lisp type representing a valid subtype of ARRAY. Output is
either the array element type or NIL, denoting that we are not able to
compute it. This version only handles the simplest cases."
  (cond ((eq array 'string)
         'character)
        ((eq array 'base-string)
         'base-char)
        ((member array '(array vector simple-vector simple-array))
         t)
        ((atom array)
         nil)
        ((not (member (first array) '(array vector simple-vector simple-array)))
         nil)
        ((null (rest array))
         t)
        (t
         (second array))))

(defun get-constant-value (form default)
  (if (constantp form)
      (cmp-eval form)
      default))

(def-type-propagator si::aset (fname obj array &rest indices)
  (declare (ignore fname obj))
  (let* ((array-type (c1form-primary-type array))
         (elt-type (or (type-from-array-elt array) t)))
    (values (list* elt-type array-type (make-list (length indices) :initial-element 'si::index))
            elt-type)))

(def-type-propagator aref (fname array &rest indices)
  (declare (ignore fname))
  (let* ((array-type (c1form-primary-type array))
         (elt-type (or (type-from-array-elt array) t)))
    (values (list* array-type (make-list (length indices) :initial-element 'si::index))
            elt-type)))

;; This compiler-macro is clashes with an other one in cmparray.lsp! We should make up our mind...
(define-compiler-macro make-array (&whole form dimensions
                                          &key (element-type t)
                                          (initial-element nil initial-element-supplied-p)
                                          (initial-contents nil initial-contents-supplied-p)
                                          adjustable fill-pointer
                                          displaced-to (displaced-index-offset 0))
  (let* ((type (if (or (get-constant-value adjustable t)
                       (get-constant-value fill-pointer t)
                       (get-constant-value displaced-to t))
                   'array
                   'simple-array))
         (upgraded-type (get-constant-value element-type '*))
         (guess-dims (get-constant-value dimensions '*))
         (form (list 'si::make-pure-array element-type dimensions adjustable
                     fill-pointer displaced-to displaced-index-offset)))
    (unless (eq upgraded-type '*)
      ;; Known type?
      (if (nth-value 1 (subtypep t upgraded-type))
          (setf upgraded-type (upgraded-array-element-type upgraded-type))
          (cmpnote "Unknown element type ~A passed to MAKE-ARRAY" upgraded-type)))
    (unless (eq guess-dims '*)
      (if (listp guess-dims)
          (setf guess-dims (make-list (length guess-dims) :initial-element '*))
          (setf guess-dims '(*))))
    (setf type (list type upgraded-type guess-dims))
    (cond (initial-element-supplied-p
           (when initial-contents-supplied-p
             (cmpwarn "In MAKE-ARRAY, both :INITIAL-ELEMENT and :INITIAL-CONTENTS were supplied."))
           (setf form `(si::fill-array-with-elt ,form ,initial-element 0 nil)))
          (initial-contents-supplied-p
           (setf form `(si::fill-array-with-seq ,form ,initial-contents))))
    `(the ,type ,form)))


