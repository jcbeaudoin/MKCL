;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: C -*-
;;;;
;;;;  CMPSTRUCT. STRUCTURE related optimizations.

;;;;  Copyright (c) 2008. Juan Jose Garcia-Ripol
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Lesser General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 3 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../../Copyright' for full details.

(in-package "COMPILER")

;;;
;;; GET-SLOT-TYPE
;;;
;;; Given a structure type and a slot index, infer the type of the output.
;;;
(defun get-slot-type (name index)
  ;; default is t
  (type-filter
   (or (third (nth index (get-sysprop name 'SYS::STRUCTURE-SLOT-DESCRIPTIONS))) 'T)))

;;;
;;; STRUCTURE SLOT READING
;;;
;;; By looking at the name of a function we may infer whether it is a
;;; reader for a structure slot. If this is the case and the policy
;;; allows us, we will inline the slot access and infer the type of
;;; the output.
;;;

(defun maybe-optimize-structure-access (fname args)
  (let* ((slot-description (get-sysprop fname 'SYS::STRUCTURE-ACCESS)))
    (when (and slot-description
	       (inline-possible fname)
	       (policy-inline-slot-access-p))
      (let (structure-type slot-index)
	(unless (and (consp slot-description)
		     (setf structure-type (car slot-description)
			   slot-index (cdr slot-description))
		     (typep slot-index 'fixnum))
	  (cmpwarn "Unable to inline access to structure slot ~A because index is corrupt: ~A"
		   fname slot-index)
	  (return-from maybe-optimize-structure-access nil))
	(unless (= (length args) 1)
	  (cmpwarn "Too many arguments for structure slot accessor ~A" fname)
	  (return-from maybe-optimize-structure-access nil))
	(setf args (first args))
	(cond
	  ((eq structure-type 'list)
	   (c1expr `(nth ,slot-index ,args))
	   )
	  ((eq structure-type 'vector)
	   (c1expr `(svref ,args ,slot-index)))
	  ((and (consp structure-type) (eq (first structure-type) 'vector))
	   (c1expr `(aref (the ,structure-type ,args) ,slot-index)))
	  (t
           (c1structure-ref `(,args ',structure-type ,slot-index))))))))

(defun c1structure-ref (args)
  (check-args-number 'sys:structure-ref args 3)
  ;(format t "~%;;; Optimizing structure-ref for ~A" args)
  (let* ((form (first args))
	 (c-form (c1expr form))
	 (name (second args))
	 (index (third args)))
    (if (and (constantp name)
	     (constantp index))
	(let* ((name (cmp-eval name))
	       (index (cmp-eval index))
	       (type (get-slot-type name index)))
	  (make-c1form* 'SYS:STRUCTURE-REF :type type
			:args c-form (add-symbol name) index
			(if (safe-compile) nil :unsafe)))
      (c1call-global 'sys:structure-ref args))))

(defun c2structure-ref (form name-vv index unsafe)
  (let* ((*inline-blocks* 0)
	 (loc (first (coerce-locs (inline-args (list form))))))
    (unwind-exit (list 'SYS:STRUCTURE-REF loc name-vv index unsafe))
    (close-inline-blocks)))

(defun wt-structure-ref (loc name-vv index unsafe)
  (if unsafe
      (progn
	(wt "(" loc ")->instance.slots[" `(COERCE-LOC :fixnum ,index) "]")
	#| #-clos (wt "(" loc ")->str.self[" `(COERCE-LOC :fixnum ,index) "]") |#
	)
    (wt "mkcl_structure_ref(env, " loc "," name-vv "," `(COERCE-LOC :fixnum ,index) ")")))

(defun c1structure-set (args)
  (if (and ;;(not (safe-compile))         ; Beppe
	   (not (endp args))
	   (not (endp (cdr args)))
	   (consp (second args))
	   (eq (caadr args) 'QUOTE)
	   (not (endp (cdadr args)))
	   (symbolp (cadadr args))
	   (endp (cddadr args))
	   (not (endp (cddr args)))
	   (sys::fixnump (third args))
	   (not (endp (cdddr args)))
	   (endp (cddddr args)))
      (let ((x (c1expr (car args)))
	    (y (c1expr (fourth args)))
	    (name (cadadr args)))       ; remove QUOTE.
	;; Beppe. Type check added:
	(let* ((slot-type (get-slot-type name (third args)))
	       (new-type (type-and slot-type (c1form-primary-type y))))
	  (if (null new-type)
	      (cmpwarn "The type of the form ~s is not ~s."
		       (fourth args) slot-type)
	      (progn
		(when (eq 'VAR (c1form-name y))
		  ;; it's a variable, propagate type
		  (setf (var-type (c1form-arg 0 y)) new-type))
		(setf (c1form-type y) new-type))))
	(make-c1form* 'SYS:STRUCTURE-SET :type (c1form-primary-type y)
		      :args x (add-symbol name) (third args) y))
      (c1call-global 'SYS:STRUCTURE-SET args)))

(defun c2structure-set (x name-vv index y
			  &aux locs (*inline-blocks* 0))
  ;; the third argument here *c1t* is just a hack to ensure that
  ;; a variable is introduced for y if it is an expression with side effects
  (setq locs (inline-args (list x y *c1t*)))
  (setq x (second (first locs)))
  (setq y `(coerce-loc :object ,(second (second locs))))
  (if (safe-compile)
      (wt-nl "mkcl_structure_set(env, " x "," name-vv "," index "," y ");")
    (wt-nl "(" x ")->instance.slots[" index "]= " y ";")
    #| #-clos (wt-nl "(" x ")->str.self[" index "]= " y ";") |#
    )
  (unwind-exit y)
  (close-inline-blocks)
  )

(put-sysprop 'SYS:STRUCTURE-REF 'C1 'c1structure-ref)
(put-sysprop 'SYS:STRUCTURE-REF 'C2 'c2structure-ref)
(put-sysprop 'SYS:STRUCTURE-REF 'WT-LOC 'wt-structure-ref)
(put-sysprop 'SYS:STRUCTURE-SET 'C1 'c1structure-set)
(put-sysprop 'SYS:STRUCTURE-SET 'C2 'c2structure-set)
