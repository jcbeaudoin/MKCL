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

;;;; CMPINLINE  Open coding optimizer.

(in-package "COMPILER")

;;; Valid property names for open coded functions are:
;;;  :INLINE-ALWAYS
;;;  :INLINE-SAFE	safe-compile only
;;;  :INLINE-UNSAFE	non-safe-compile only
;;;
;;; Each property is a list of 'inline-info's, where each inline-info is:
;;; ( types { type | boolean } { string | function } ).
;;;
;;; For each open-codable function, open coding will occur only if there exits
;;; an appropriate property with the argument types equal to 'types' and with
;;; the return-type equal to 'type'.  The third element
;;; is T if and only if side effects may occur by the call of the function.
;;; Even if *DESTINATION* is TRASH, open code for such a function with side
;;; effects must be included in the compiled code.
;;; The forth element is T if and only if the result value is a new Lisp
;;; object, i.e., it must be explicitly protected against GBC.

;;;
;;; inline-args:
;;;   returns a list of pairs (type loc)
;;;   side effects: emits code for temporary variables
;;;
;;; Whoever calls inline-args must bind *inline-blocks* to 0 and afterwards
;;; call close-inline-blocks
;;;
(defun inline-args (forms &optional types)
  (declare (ignore types))
  (flet ((all-locations (args &aux (res t))
	   (dolist (arg args res)
	     (unless (member (c1form-name arg)
			     '(LOCATION VAR SYS:STRUCTURE-REF
			       SYS:INSTANCE-REF)
			     :test #'eq)
	       (setq res nil)))))

    (do ((forms forms (cdr forms))
	 (form) (locs))
	((endp forms) (nreverse locs))
      (setq form (car forms))
      (case (c1form-name form)
	(LOCATION
	 (push (list (c1form-primary-type form) (c1form-arg 0 form)) locs))
	(VAR
	 (let ((var (c1form-arg 0 form)))
	   (if (var-changed-in-form-list var (cdr forms))
	       (let* ((var-rep-type (var-rep-type var))
		      (lcl (make-lcl-var :rep-type var-rep-type :type (var-type var))))
		 (wt-nl "{" (rep-type-name var-rep-type) " " lcl "= " var ";")
		 (push (list (c1form-primary-type form) lcl) locs)
		 (incf *inline-blocks*))
	       (push (list (c1form-primary-type form) var) locs))))

	(CALL-GLOBAL
	 (let* ((fname (c1form-arg 0 form))
		(args (c1form-arg 1 form))
		(return-type (c1form-primary-type form))
		(arg-locs (inline-args args))
		(loc (inline-function fname arg-locs return-type)))
	   (if loc
	       ;; If there are side effects, we may not move the C form
	       ;; around and we have to save its value in a variable.
	       ;; We use a variable of type out-type to save the value
	       ;; if (return-type >= out-type)
	       ;; then
	       ;;   coerce the value to out-type
	       ;; otherwise
	       ;;   save the value without coercion and return the
	       ;;   variable tagged with and-type,
	       ;;   so that whoever uses it may coerce it to such type
	       (let* ((and-type (type-and return-type (loc-type loc)))
		      (out-rep-type (loc-representation-type loc))
		      (var (make-lcl-var :rep-type out-rep-type :type and-type)))
		 (wt-nl "{" " const " (rep-type-name out-rep-type) " " var "= " loc ";")
		 (incf *inline-blocks*)
		 (setq loc var)
		 (push (list (loc-type loc) loc) locs))
	       (let* ((temp (make-temp-var)) ;; output value
		      ;; bindings like c2expr*
		      (*exit* (next-label))
		      (*unwind-exit* (cons *exit* *unwind-exit*))
		      (*lcl* *lcl*)
		      (*temp* *temp*)
		      (*destination* temp))
		 (unwind-exit (call-global-loc fname nil arg-locs return-type :object))
		 (wt-label *exit*)
		 (push
		  (list (if (subtypep 'T return-type)
			    (or (get-return-type fname) 'T)
			    return-type)
			temp)
		  locs)))))

	(SYS:STRUCTURE-REF
	 (let ((type (c1form-primary-type form)))
	   (if (args-cause-side-effect (cdr forms))
	       (let* ((temp (make-temp-var))
		      (*destination* temp))
		 (c2expr* form)
		 (push (list type temp) locs))
	       (push (list type
			   (list 'SYS:STRUCTURE-REF
				 (first (coerce-locs
					  (inline-args (list (c1form-arg 0 form)))))
				 (c1form-arg 1 form)
				 (c1form-arg 2 form)
				 (c1form-arg 3 form)))
		     locs))))
	(SYS:INSTANCE-REF
	 (let ((type (c1form-primary-type form)))
	   (if (args-cause-side-effect (cdr forms))
	       (let* ((temp (make-temp-var))
		      (*destination* temp))
		 (c2expr* form)
		 (push (list type temp) locs))
	       (push (list type
			   (list 'SYS:INSTANCE-REF
				 (first (coerce-locs
					  (inline-args (list (c1form-arg 0 form)))))
				 (c1form-arg 1 form)))
		     locs))))
	(SETQ
	 (let ((vref (c1form-arg 0 form))
	       (form1 (c1form-arg 1 form)))
	   (let ((*destination* vref)) (c2expr* form1))
	   (if (eq (c1form-name form1) 'LOCATION)
	       (push (list (c1form-primary-type form1) (c1form-arg 0 form1)) locs)
	       (setq forms (list* nil	; discarded at iteration
				  (make-c1form 'VAR form vref)
				  (cdr forms))
		     ))))

	(t (let ((temp (make-temp-var)))
	     (let ((*destination* temp)) (c2expr* form))
	     (push (list (c1form-primary-type form) temp) locs))))))
  )

(defun destination-type ()
  (rep-type->lisp-type (loc-representation-type *destination*))
)

;;;
;;; inline-function:
;;;   locs are typed locs as produced by inline-args
;;;   returns NIL if inline expansion of the function is not possible
;;;
(defun inline-function (fname inlined-locs return-type &optional (return-rep-type 'any))
  ;; Those functions that use INLINE-FUNCTION must rebind
  ;; the variable *INLINE-BLOCKS*.
  (and (inline-possible fname)
       (not (get-sysprop fname 'C2))
       (let* ((dest-rep-type (loc-representation-type *destination*))
              (dest-type (rep-type->lisp-type dest-rep-type))
              (ii (get-inline-info fname (mapcar #'first inlined-locs)
				   return-type return-rep-type)))
	 (declare (ignore dest-type))
	 (when ii
	   (let* ((arg-types (inline-info-arg-types ii))
		  (out-rep-type (inline-info-return-rep-type ii))
		  (out-type (inline-info-return-type ii))
		  (side-effects-p (function-may-have-side-effects fname))
		  (fun (inline-info-expansion ii))
		  (one-liner (inline-info-one-liner ii)))
	     (declare (ignore out-type))
	     (produce-inline-loc inlined-locs arg-types (list out-rep-type)
				 fun side-effects-p one-liner))))))

(defun choose-inline-info (ia ib return-type return-rep-type)
  (declare (ignore return-type))
  (cond
    ;; Only accept inliners that have the right rep type
    ((not (or (eq return-rep-type 'any)
              (eq return-rep-type :void)
              (let ((info-type (inline-info-return-rep-type ib)))
                (or (eq return-rep-type info-type)
                    ;; :bool can be coerced to any other location type
                    (eq info-type :bool)))))
     ia)
    ((null ia)
     ib)
    ;; Keep the first one, which is typically the least safe but fastest. 
    ((equal (inline-info-arg-types ia) (inline-info-arg-types ib))
     ia)
    ;; More specific?
    ((every #'type>= (inline-info-arg-types ia) (inline-info-arg-types ib))
     ib)
    ;; Keep the first one, which is typically the least safe but fastest. 
    (t
     ia)))

(defun get-inline-info (fname types return-type return-rep-type)
  (let ((output nil))
    (dolist (x *inline-functions*)
      (when (eq (car x) fname)
        (let ((other (inline-type-matches (cdr x) types return-type)))
          (setf output (choose-inline-info output other return-type return-rep-type)))))
    (unless (safe-compile)
      (dolist (x (get-sysprop fname ':INLINE-UNSAFE))
        (let ((other (inline-type-matches x types return-type)))
          (when other
            (setf output (choose-inline-info output other return-type return-rep-type))))))
    (dolist (x (get-sysprop fname ':INLINE-SAFE))
      (let ((other (inline-type-matches x types return-type)))
        (when other
          (setf output (choose-inline-info output other return-type return-rep-type)))))
    (dolist (x (get-sysprop fname ':INLINE-ALWAYS))
      (let ((other (inline-type-matches x types return-type)))
        (when other
          (setf output (choose-inline-info output other return-type return-rep-type)))))
    output))

(defun to-fixnum-float-type (type)
  (dolist (i '(FIXNUM DOUBLE-FLOAT SINGLE-FLOAT #+long-float LONG-FLOAT) nil)
    (when (type>= i type)
      (return i))))

(defun maximum-float-type (t1 t2)
  (cond ((null t1)
         t2)
	#+long-float
        ((or (eq t1 'LONG-FLOAT) (eq t2 'LONG-FLOAT))
         'LONG-FLOAT)
        ((or (eq t1 'DOUBLE-FLOAT) (eq t2 'DOUBLE-FLOAT))
         'DOUBLE-FLOAT)
        ((or (eq t1 'SINGLE-FLOAT) (eq t2 'SINGLE-FLOAT))
         'SINGLE-FLOAT)
        (T
         'FIXNUM)))

(defun inline-type-matches (inline-info arg-types return-type)
  (let* ((rts nil)
         (number-max nil))
    ;;
    ;; Check that the argument types match those of the inline expression
    ;;
    (do* ((arg-types arg-types (cdr arg-types))
          (types (inline-info-arg-types inline-info) (cdr types)))
         ((or (endp arg-types) (endp types))
          (when (or arg-types types)
            (return-from inline-type-matches nil)))
      (let* ((arg-type (first arg-types))
             (type (first types)))
        (cond ((eq type 'FIXNUM-FLOAT)
               (let ((new-type (to-fixnum-float-type arg-type)))
                 (unless new-type
                   (return-from inline-type-matches nil))
                 (push new-type rts)
                 (setq number-max (maximum-float-type number-max new-type))))
              ((type>= type arg-type)
               (push type rts))
              (t (return-from inline-type-matches nil)))))
    ;;
    ;; Now there is an optional check of the return type. This check is
    ;; only used when enforced by the inliner.
    ;;
    (when (or (eq (inline-info-return-rep-type inline-info) :bool)
              (null (inline-info-exact-return-type inline-info))
              (let ((inline-return-type (inline-info-return-type inline-info)))
                (if number-max
                    ;; for arithmetic operators we take the maximal
                    ;; type as possible result type. Note that FIXNUM
                    ;; is not an option, because the product, addition
                    ;; or difference of fixnums may be a larger
                    ;; integer.
                    (and (setf number-max (if (eq number-max 'fixnum)
                                              'integer
                                              number-max))
                         (type>= inline-return-type number-max)
                         (type>= number-max return-type))
                    ;; no contravariance
                    (type>= inline-return-type return-type))))
      (let ((inline-info (copy-structure inline-info)))
        (setf (inline-info-arg-types inline-info)
              (nreverse rts))
        inline-info))))

(defun need-to-protect (forms &aux ii)
  (do ((forms forms (cdr forms))
       (res nil))
      ((or res (endp forms)) res)
    (let ((form (car forms)))
      (declare (object form))
      (case (c1form-name form)
	(LOCATION)
	(VAR
	 (when (var-changed-in-form-list (c1form-arg 0 form) (cdr forms))
	   (setq res t)))
	(CALL-GLOBAL
	 (let ((fname (c1form-arg 0 form))
	       (args (c1form-arg 1 form)))
	   (or (function-may-have-side-effects fname)
	       (need-to-protect args))))
	(SYS:STRUCTURE-REF
	 (when (need-to-protect (list (c1form-arg 0 form)))
	   (setq res t)))
	(t (setq res t)))))
  )

(defun close-inline-blocks ()
  (dotimes (i *inline-blocks*) (declare (fixnum i)) (wt #\})))

(defun form-causes-side-effect (form)
  (case (c1form-name form)
    ((LOCATION VAR SYS:STRUCTURE-REF SYS:INSTANCE-REF)
     nil)
    (CALL-GLOBAL
     (let ((fname (c1form-arg 0 form))
	   (args (c1form-arg 1 form)))
       (or (function-may-have-side-effects fname)
	   (args-cause-side-effect args))))
    (t t)))

(defun args-cause-side-effect (forms)
  (some #'form-causes-side-effect forms))

(defun function-may-have-side-effects (fname)
  (not (get-sysprop fname 'no-side-effects)))

(defun function-may-change-sp (fname)
  (not (or (get-sysprop fname 'no-side-effects)
	   (get-sysprop fname 'no-sp-change))))

