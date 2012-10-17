;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: C -*-
;;;;
;;;; CMPNUM -- Optimizer for numerical expressions.

;;;;  Copyright (c) 2005, Juan Jose Garcia Ripoll
;;;;
;;;;    ECoLisp is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Lesser General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 3 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../../Copyright' for full details.

(in-package "COMPILER")

(defun simplify-arithmetic (operator args whole)
  (let ((l (length args)))
    (cond ((every #'numberp args)
	   (apply operator args))
          ((> l 2)
	   (simplify-arithmetic
	    operator
	    (list* (simplify-arithmetic operator (list (first args) (second args)) nil)
		   (cddr args))
	    nil))
	  ((= l 2)
	   (or whole (list* operator args)))
	  ((= l 1)
	   (if (or (eq operator '*) (eq operator '+))
	       (first args)
	       (or whole (list* operator args))))
	  ((eq operator '*)
	   1)
	  ((eq operator '+)
	   0)
	  (t
	   (error 'si:simple-program-error
		  :format-error "Wrong number of arguments for operator ~a in ~a"
		  :format-arguments (list operator (or whole (list* operator args))))))))

(define-compiler-macro * (&whole all &rest args)
  (simplify-arithmetic '* args all))

(define-compiler-macro + (&whole all &rest args)
  (simplify-arithmetic '+ args all))

(define-compiler-macro / (&whole all &rest args)
  (simplify-arithmetic '/ args all))

(define-compiler-macro - (&whole all &rest args)
  (simplify-arithmetic '- args all))

(define-compiler-macro byte (size position)
  (if (and (integerp size) (integerp position))
      `(quote (,size . ,position))
    `(cons ,size ,position)))


