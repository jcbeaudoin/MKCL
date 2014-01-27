;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: C -*-
;;;;
;;;; CMPCT --  Optimizer for several constant values

;;;;  Copyright (c) 2003, Juan Jose Garcia Ripoll.
;;;;
;;;;    ECoLisp is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Lesser General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 3 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../../Copyright' for full details.

(in-package "COMPILER")

(defparameter +optimizable-constants+ '())

(defun c1constant-value (val &key always only-small-values)
  (cond
   ((let ((x (assoc val +optimizable-constants+)))
      (when x
       (pushnew "#include <float.h>" *clines-string-list*)
       (setf x (cdr x))
       (if (listp x)
           (c1expr x)
           x))))
   ((eq val nil) (c1nil))
   ((eq val t) (c1t))
   ((mkcl:fixnump val)
    (make-c1form* 'LOCATION :type 'FIXNUM :args (list 'FIXNUM-VALUE val)))
   ((mkcl:base-char-p val)
    (make-c1form* 'LOCATION :type (type-of val)
		  :args (list 'BASE-CHAR-VALUE (char-code val))))
   ((characterp val)
    (make-c1form* 'LOCATION :type (type-of val)
		  :args (list 'CHARACTER-VALUE (char-code val))))
   ((typep val 'SINGLE-FLOAT)
    (make-c1form* 'LOCATION :type 'SINGLE-FLOAT
		  :args (list 'SINGLE-FLOAT-VALUE val (add-object val))))
   ((typep val 'DOUBLE-FLOAT)
    (make-c1form* 'LOCATION :type 'DOUBLE-FLOAT
		  :args (list 'DOUBLE-FLOAT-VALUE val (add-object val))))
   ((typep val 'LONG-FLOAT)
    (make-c1form* 'LOCATION :type 'LONG-FLOAT
		  :args (list 'LONG-FLOAT-VALUE val (add-object val))))
   (always
    (make-c1form* 'LOCATION :type (object-type val)
		  :args (list 'VV (add-object val))))
   (only-small-values nil)
   (t nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; OPTIMIZABLE FLOAT CONSTANTS
;;;

(mapc
 #'(lambda (record)
     (let* ((name (first record))
	    (c-value (second record)))
       (push 
        (cond ((symbolp name)
               (let* ((value (symbol-value name))
                      (type (lisp-type->rep-type (type-of value))))
                 (cons value `(c-inline () () ,type ,c-value
                                        :one-liner t :side-effects nil))))
              ((floatp name)
               (let* ((value name)
                      (type (type-of value))
                      (loc-type (case type
                                  (single-float 'single-float-value)
                                  (double-float 'double-float-value)
                                  (long-float 'long-float-value)))
                      (location `(VV ,c-value)))
                (cons value (make-c1form* 'LOCATION :type type
                                          :args (list loc-type value location)))))
              (t
               (baboon)))
        +optimizable-constants+)))
 (reverse
 '((MOST-POSITIVE-SHORT-FLOAT "FLT_MAX")
   (MOST-POSITIVE-SINGLE-FLOAT "FLT_MAX")

   (MOST-NEGATIVE-SHORT-FLOAT "-FLT_MAX")
   (MOST-NEGATIVE-SINGLE-FLOAT "-FLT_MAX")

   (LEAST-POSITIVE-SHORT-FLOAT "FLT_MIN")
   (LEAST-POSITIVE-SINGLE-FLOAT "FLT_MIN")
   (LEAST-POSITIVE-NORMALIZED-SHORT-FLOAT "FLT_MIN")
   (LEAST-POSITIVE-NORMALIZED-SINGLE-FLOAT" FLT_MIN")

   (LEAST-NEGATIVE-SHORT-FLOAT "-FLT_MIN")
   (LEAST-NEGATIVE-SINGLE-FLOAT "-FLT_MIN")
   (LEAST-NEGATIVE-NORMALIZED-SHORT-FLOAT "-FLT_MIN")
   (LEAST-NEGATIVE-NORMALIZED-SINGLE-FLOAT "-FLT_MIN")

   (MOST-POSITIVE-DOUBLE-FLOAT "DBL_MAX")
   (MOST-NEGATIVE-DOUBLE-FLOAT "-DBL_MAX")
   (LEAST-POSITIVE-DOUBLE-FLOAT "DBL_MIN")
   (LEAST-POSITIVE-NORMALIZED-DOUBLE-FLOAT "DBL_MIN")
   (LEAST-NEGATIVE-DOUBLE-FLOAT "-DBL_MIN")
   (LEAST-NEGATIVE-NORMALIZED-DOUBLE-FLOAT "-DBL_MIN")

   ;; Order is important: on platforms where 0.0 and -0.0 are the same
   ;; the last one is prioritized.
   (#.(coerce 0 'single-float) "mkcl_core.singlefloat_zero")
   (#.(coerce 0 'double-float) "mkcl_core.doublefloat_zero")
   (#.(coerce -0.0 'single-float) "mkcl_core.singlefloat_minus_zero")
   (#.(coerce -0.0 'double-float) "mkcl_core.doublefloat_minus_zero")

   .
   #-long-float
   NIL
   #+long-float
   (
    (MOST-POSITIVE-LONG-FLOAT "LDBL_MAX")
    (MOST-NEGATIVE-LONG-FLOAT "-LDBL_MAX")
    (LEAST-POSITIVE-LONG-FLOAT "LDBL_MIN")
    (LEAST-POSITIVE-NORMALIZED-LONG-FLOAT" LDBL_MIN")
    (LEAST-NEGATIVE-LONG-FLOAT "-LDBL_MIN")
    (LEAST-NEGATIVE-NORMALIZED-LONG-FLOAT "-LDBL_MIN")
    (#.(coerce -0.0 'long-float) "mkcl_core.longfloat_minus_zero")
    (#.(coerce 0 'long-float) "mkcl_core.longfloat_zero")
    )
   )))
