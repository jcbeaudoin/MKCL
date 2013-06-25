;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: C -*-
;;;;
;;;;  CMPLET  Let and Let*.
;;;;
;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;
;;;;    MKCL is free software; you can redistribute it and/or modify it
;;;;    under the terms of the GNU Lesser General Public License as
;;;;    published by the Free Software Foundation; either version 3 of
;;;;    the License, or (at your option) any later version.
;;;;
;;;;    See file '../../Copyright' for full details.

(in-package "COMPILER")

(defun c1let (args &aux	(setjmps *setjmps*)
                        (forms nil) (vars nil)
                        ss is ts body other-decls aa1 aa2 doc all-decls iables dyns
			(*cmp-env* (cmp-env-copy)))
  (check-args-number 'LET args 1)

  (multiple-value-setq (body ss ts is other-decls doc all-decls iables dyns) 
		       (c1body (cdr args) nil))

  (dolist (x (car args))
    (cond ((symbolp x)
           (let ((v (c1make-var x ss is ts iables dyns)))
             (push v vars)
             (push (default-init v) forms)))
          (t (cmpck (not (and (consp x) (or (endp (cdr x)) (endp (cddr x)))))
                    "The variable binding ~s is illegal." x)
             (let* ((vname (car x))
		    (v (c1make-var vname ss is ts iables dyns))
		    (form (if (endp (cdr x))
                            (default-init v)
                            (and-form-type (var-type v)
                                           (c1expr (second x))
                                           (second x)
					   :unsafe
					   "In LET bindings"))))
	       ;; :read-only variable handling. Beppe
	       (when (read-only-variable-p vname other-decls)
	         (setf (var-type v) (c1form-primary-type form)))
	       (push v vars)
	       (push form forms)))))

  (setf vars (nreverse vars) forms (nreverse forms))

  (mapc #'push-vars vars)

  (check-vdecl ss is)

  (c1declare-specials ss)
  (setq body (c1decl-body other-decls body))

  ;; since the body may produce type constraints on variables:
  ;; (let (x) (foo x)) where (type (function (fixnum) fixnum) foo)
  ;; do it again
  (do ((vars vars (cdr vars))
       (forms forms (cdr forms))
       (all-vars vars)
       (used-vars '())
       (used-forms '()))
      ((null vars)
       (setf used-vars (nreverse used-vars))
       (make-c1form* 'LET :type (c1form-type body)
		     :volatile (not (eql setjmps *setjmps*))
		     :local-vars used-vars
		     :args used-vars (nreverse used-forms) body))
    (let* ((var (first vars))
	   (form (and-form-type (var-type var) (first forms) (var-name var)
				:unsafe "In LET body"))
	   (form-type (c1form-primary-type form)))
      (declare (type var var))
      ;; Automatic treatement for READ-ONLY variables which are not
      ;; closed over in other functions.
      (unless (or (var-changed-in-form-list var (list body))
		  (var-functions-reading var)
		  (var-functions-setting var))
	(setf (var-type var) form-type)
	(update-var-type var form-type body)
	;; * (let ((v2 e2)) e3 e4) => (let () e3 e4)
	;;   provided
	;;   - v2 does not appear in body
	;;   - e2 produces no side effects
	(when (and (= 0 (var-ref var))
		   (not (member (var-kind var) '(special global)))
		   (not (form-causes-side-effect form)))
	  (cmpnote "Removing unused variable ~A" (var-name var))
	  (go continue))
	;;  (let ((v1 e1) (v2 e2) (v3 e3)) (expr e4 v2 e5))
	;;  can become
	;;  (let ((v1 e1) (v3 e3)) (expr e4 e2 e5))
	;;  provided
	;;  - v2 appears only once
	;;  - v2 appears only in body
	;;  - e2 does not affect v1 nor e3, e3 does not affect e2
	;;  - e4 does not affect e2
	(when (and (= 1 (var-ref var))
		   (var-referenced-in-form var body)
		   (not (form-causes-side-effect form))
		   ;; it does not refer to special variables which
		   ;; are changed in the LET form
		   (notany #'(lambda (v) (var-referenced-in-form v form)) all-vars)
		   (catch var
		     (replaceable var body)))
	  (cmpnote "Replacing variable ~A by its value ~A" (var-name var) form)
	  (nsubst-var var form)
	  (go continue))
	)
      #+(or)
      (when (member-type form-type '(FIXNUM CHARACTER DOUBLE-FLOAT SINGLE-FLOAT))
	(incf (var-ref var)))		; force unboxing
      (check-vref var)
      (push var used-vars)
      (push form used-forms))
    continue))

(defun update-var-type (var type x)
  (cond ((consp x)
	 (dolist (e x)
	   (update-var-type var type e)))
	((not (c1form-p x)))
	((eq (c1form-name x) 'VAR)
	 (when (eq var (c1form-arg 0 x))
	   (setf (c1form-type x) (type-and (c1form-type x) type))))
	(t
	 (update-var-type var type (c1form-args x)))))

(defun read-only-variable-p (v other-decls)
  (dolist (i other-decls nil)
    (when (and (eq (car i) :READ-ONLY)
	       (member v (rest i)))
      (return t))))

(defun c2let (vars forms body
                   &aux (block-p nil) (bindings nil)
                   initials
                   (*unwind-exit* *unwind-exit*)
		   (*env* *env*)
		   (*closure-levels* *closure-levels*)
		   closure-block-id
                   (*env-lvl* *env-lvl*) env-grows)
  (declare (type boolean block-p))

  ;; Allocation is needed for:
  ;; 1. each variable which is LOCAL and which is not REPLACED
  ;;    or whose value is not DISCARDED
  ;; 2. each init form for the remaining variables except last

  ;; Determine which variables are really necessary and create list of init's
  ;; and list of bindings. Bindings for specials must be done after all inits.
  (labels ((do-decl (var)
	     (declare (type var var))
	     (wt-nl)
	     (unless block-p
	       (wt "{") (setq block-p t))
	     (wt *volatile* (rep-type-name (var-rep-type var)) " " var ";")
	     (when (local var)
	       (wt-comment (var-name var))))
	   (do-init (var form fl)
	     (if (and (local var)
		      (not (args-cause-side-effect (cdr fl))))
		 ;; avoid creating temporary for init
		 (push (cons var form) initials)
		 (let* ((loc (make-lcl-var :rep-type (var-rep-type var)
					   :type (var-type var))))
		   (do-decl loc)
		   (push (cons loc form) initials)
		   (push (cons var loc) bindings)))))

    (do ((vl vars (rest vl))
         (fl forms (rest fl))
         (prev-ss nil) (used t t))
        ((endp vl))
      (let ((form (first fl))
            (var (first vl)))
      (declare (type var var))
      (if (local var)
	  (if (setq used (not (discarded var form body)))
	    (progn
	      (setf (var-loc var) (next-lcl))
	      (do-decl var))
	    ;; The variable is discared, we simply replace it with
	    ;; a dummy value that will not get used.
	    (setf (var-kind var) 'REPLACED
		  (var-loc var) NIL)))
      (when used
	(if (unboxed var)
	    (push (cons var form) initials)	; nil (ccb)
	    ;; LEXICAL, SPECIAL, GLOBAL or :OBJECT
	    (case (c1form-name form)
	      (LOCATION
	       (if (can-be-replaced var body)
		   (setf (var-kind var) 'REPLACED
			 (var-loc var) (c1form-arg 0 form))
		   (push (cons var (c1form-arg 0 form)) bindings)))
	      (VAR
	       (let* ((var1 (c1form-arg 0 form)))
		 (cond ((or (var-changed-in-form-list var1 (cdr fl))
			    (and (member (var-kind var1) '(SPECIAL GLOBAL))
				 (member (var-name var1) prev-ss)))
			(do-init var form fl))
		       ((and ;; Fixme! We should be able to replace variable
			     ;; even if they are referenced across functions.
			     ;; We just need to keep track of their uses.
			     (member (var-kind var1) '(REPLACED :OBJECT))
			     (can-be-replaced var body)
			     (not (var-changed-in-form var1 body)))
			(setf (var-kind var) 'REPLACED
			      (var-loc var) var1))
		       (t (push (cons var var1) bindings)))))
	      (t (do-init var form fl))))
	(unless env-grows
	  (setq env-grows (var-ref-ccb var))))
      (when (eq (var-kind var) 'SPECIAL) (push (var-name var) prev-ss)))))

  (when (env-grows env-grows)
    (unless block-p
      (wt-nl "{ ") (setq block-p t))
    (push 0 *closure-levels*)
    (setq closure-block-id (incf *closure-block-id*) *cenv0-used* t)
    (let ((env-lvl *env-lvl*))
      (incf *env-lvl*)
      (wt-nl "mkcl_object cenv" *env-lvl* " = mkcl_alloc_clevel_block(env, "
	     (if *written-function* "this_func" "mk_cl_Cnil")
	     ", cenv" env-lvl ", "
	     (written-function-cname) "_cblock_" closure-block-id ");")
      (when (>= *debug-fun* 2)
	(wt-nl "mkcl_object syms_cenv" *env-lvl* " = mkcl_alloc_clevel_block(env, "
	       (if *written-function* "this_func" "mk_cl_Cnil")
	       ", syms_cenv" env-lvl ", "
	       (written-function-cname) "_cblock_" closure-block-id ");")
	)
      )
    )

  ;; eval INITFORM's and bind variables
  (dolist (init (nreverse initials))
    (let ((*destination* (car init))
	  (*lcl* *lcl*))
      (c2expr* (cdr init))))
  ;; bind LET variables
  (dolist (binding (nreverse bindings))
    (bind (cdr binding) (car binding)))

  (if (and *debug-fun* (>= *debug-fun* 3))
    (let ((*unwind-exit* *unwind-exit*))
      (wt-nl "{")
      (let* ((env (build-debug-lexical-env (reverse vars))))
        (when env (push 'IHS-ENV *unwind-exit*))
        (c2expr body)
        (wt-nl "}")
        (when env (pop-debug-lexical-env))))
    (c2expr body))

  (when closure-block-id
    (wt-nl-h "#define " (written-function-cname) "_cblock_" closure-block-id " "
	     (first *closure-levels*))
    )

  (when block-p (wt-nl "}"))
  )

(defun env-grows (possibly)
  ;; if additional closure variables are introduced and this is not
  ;; last form, we must use a new env.
  (and possibly
       (or (when *compile-toplevel*
	     t)
	   (dolist (exit *unwind-exit*)
	     (case exit
		   (RETURN (return NIL))
		   (BDS-BIND)
		   (t (return T)))))))

(defun c1let* (args &aux (forms nil) (vars nil)
                    (setjmps *setjmps*)
                    ss is ts body other-decls doc all-decls iables dyns
                    (*cmp-env* (cmp-env-copy)))
  (check-args-number 'LET* args 1)

  (multiple-value-setq (body ss ts is other-decls doc all-decls iables dyns)
		       (c1body (cdr args) nil))

  (dolist (x (car args))
    (cond ((symbolp x)
           (let ((v (c1make-var x ss is ts iables dyns)))
	     (push (default-init v) forms)
	     (push v vars)
	     (push-vars v)))
          ((not (and (consp x) (or (endp (cdr x)) (endp (cddr x)))))
           (cmperr "The variable binding ~s is illegal." x))
          (t (let* ((v (c1make-var (car x) ss is ts iables dyns))
		    (form (if (endp (cdr x))
			      (default-init v)
			      (and-form-type (var-type v)
					     (c1expr (second x))
					     (second x)
					     :unsafe
					     "In LET* bindings"))))
	       ;; :read-only variable handling.
	       (when (read-only-variable-p (car x) other-decls)
		 (setf (var-type v) (c1form-primary-type form)))
	       (push form forms)
	       (push v vars)
	       (push-vars v)))))

  (c1declare-specials ss)
  (check-vdecl ss is)
  (setq body (c1decl-body other-decls body))

  ;; since the body may produce type constraints on variables,
  ;; do it again:
  (do ((vs (setq vars (nreverse vars)) (cdr vs))
       (fs (nreverse forms) (cdr fs))
       (used-vars '())
       (used-forms '()))
      ((null vs)
       (setf used-vars (nreverse used-vars))
       (make-c1form* 'LET* :type (c1form-type body)
		     :volatile (not (eql setjmps *setjmps*))
		     :local-vars used-vars
		     :args used-vars (nreverse used-forms) body))
    (let* ((var (first vs))
	   (form (and-form-type (var-type var) (car fs) (cadar args)
				:unsafe "In LET* body"))
	   (form-type (c1form-primary-type form))
	   (rest-forms (cons body (rest fs))))
      ;; Automatic treatement for READ-ONLY variables:
      (unless (or (var-changed-in-form-list var rest-forms)
		  (var-functions-reading var)
		  (var-functions-setting var))
	(setf (var-type var) form-type)
	(update-var-type var form-type rest-forms)
	;; * (let* ((v2 e2)) e3 e4) => (let () e3 e4)
	;;   provided
	;;   - v2 does not appear in body
	;;   - e2 produces no side effects
	(when (and (= 0 (var-ref var))
		   (not (member (var-kind var) '(SPECIAL GLOBAL)))
		   (not (form-causes-side-effect form)))
	  (cmpnote "Removing unused variable ~A" (var-name var))
	  (go continue))
	;;  (let* ((v1 e1) (v2 e2) (v3 e3)) (expr e4 v2 e5))
	;;  can become
	;;  (let* ((v1 e1) (v3 e3)) (expr e4 e2 e5))
	;;  provided
	;;  - v2 appears only once
	;;  - v2 appears only in body
	;;  - e2 does not affect v1 nor e3, e3 does not affect e2
	;;  - e4 does not affect e2
	(when (and (= 1 (var-ref var))
		   (var-referenced-in-form var body)
		   (not (form-causes-side-effect form))
		   ;; it does not refer to special variables which
		   ;; are changed in later assignments
		   (notany #'(lambda (v)
			       (var-referenced-in-form v form))
			   (rest vs))
		   (or (and (null (rest vs))	; last variable
			    ;; its form does not affect previous variables
			    (let ((tforms (list form)))
			      (dolist (v vars)
				(when (eq v var) (return t))
				(when (var-changed-in-form-list v tforms)
				  (return nil)))))
		       (not (args-cause-side-effect fs)))
		   (catch var
		     (replaceable var body)))
	  (cmpnote "Replacing variable ~A by its value ~a" (var-name var) form)
	  (nsubst-var var form)
	  (go continue))
	)
      #+(or)
      ;; Force unboxing
      (when (member-type (c1form-primary-type form)
			 '(FIXNUM CHARACTER DOUBLE-FLOAT SINGLE-FLOAT))
	(incf (var-ref var)))
      (check-vref var)
      (push var used-vars)
      (push form used-forms))
    continue))

;; should check whether a form before var causes a side-effect
;; exactly one occurrence of var is present in forms
(defun replaceable (var form &aux (args (c1form-args form)))
  (case (c1form-name form)
    (VAR
     (if (eq var (first args))
	 (throw var T)
	 T))
    ((LOCATION SYS:STRUCTURE-REF) T)
    (CALL-GLOBAL
     (dolist (subform (second args) T)
       (when (or (not (replaceable var subform))
		 (form-causes-side-effect subform))
	 (return nil))))
    (SETQ (replaceable var (second args)))))

(defun c2let* (vars forms body
                    &aux (block-p nil)
                    (*unwind-exit* *unwind-exit*)
		    (*env* *env*)
		    (*closure-levels* *closure-levels*)
		    closure-block-id
		    (*env-lvl* *env-lvl*) env-grows)
  (declare (type boolean block-p))

  (do ((vl vars (cdr vl))
       (fl forms (cdr fl)))
      ((endp vl))
    (let* ((form (car fl))
	   (var (car vl))
	   (kind (local var))
	   )
      (declare (type var var))
      (unless (unboxed var)
	;; LEXICAL, CLOSURE, SPECIAL, GLOBAL or OBJECT
	(case (c1form-name form)
	      (LOCATION
	       (when (can-be-replaced* var body (cdr fl))
		 (cmpnote "Replacing variable ~a by its value" (var-name var))
		 (setf (var-kind var) 'REPLACED
		       (var-loc var) (c1form-arg 0 form))))
	      (VAR
	       (let* ((var1 (c1form-arg 0 form)))
		 (declare (type var var1))
		 (when (and ;; Fixme! We should be able to replace variable
			;; even if they are referenced across functions.
			;; We just need to keep track of their uses.
			(member (var-kind var1) '(REPLACED :OBJECT))
			(can-be-replaced* var body (cdr fl))
			(not (var-changed-in-form-list var1 (rest fl)))
			(not (var-changed-in-form var1 body)))
		   (cmpnote "Replacing variable ~a by its value" (var-name var))
		   (setf (var-kind var) 'REPLACED
			 (var-loc var) var1)))))
	(unless env-grows
	  (setq env-grows (var-ref-ccb var))))
      (when (and kind (not (eq (var-kind var) 'REPLACED)))
	(bind (next-lcl) var)
	(wt-nl) (unless block-p (wt "{") (setq block-p t))
	(wt *volatile* (rep-type-name kind) " " var ";")
	(wt-comment (var-name var))))
    )

  (when (env-grows env-grows)
    (unless block-p
      (wt-nl "{ ") (setq block-p t))
    (push 0 *closure-levels*)
    (setq closure-block-id (incf *closure-block-id*) *cenv0-used* t)
    (let ((env-lvl *env-lvl*))
      (incf *env-lvl*)
      (wt-nl "mkcl_object cenv" *env-lvl* " = mkcl_alloc_clevel_block(env, "
	     (if *written-function* "this_func" "mk_cl_Cnil")
	     ", cenv" env-lvl ", "
	     (written-function-cname) "_cblock_" closure-block-id ");")
      (when (>= *debug-fun* 2)
	(wt-nl "mkcl_object syms_cenv" *env-lvl* " = mkcl_alloc_clevel_block(env, "
	       (if *written-function* "this_func" "mk_cl_Cnil")
	       ", syms_cenv" env-lvl ", "
	       (written-function-cname) "_cblock_" closure-block-id ");")
	)
      )
    )

  (do ((vl vars (cdr vl))
       (fl forms (cdr fl)))
      ((null vl))
    (let ((var (car vl))
	  (form (car fl)))
      (declare (type var var))
      (case (var-kind var)
	    (REPLACED)
	    ((LEXICAL CLOSURE SPECIAL GLOBAL)
	     (case (c1form-name form)
		   (LOCATION (bind (c1form-arg 0 form) var))
		   (VAR (bind (c1form-arg 0 form) var))
		   (t (bind-init form var))))
	    (t				    ; local var
	     (let ((*destination* var))	    ; nil (ccb)
	       (c2expr* form)))
	    )
      )
    )
  (if (and *debug-fun* (>= *debug-fun* 3))
    (let ((*unwind-exit* *unwind-exit*))
      (wt-nl "{")
      (let* ((env (build-debug-lexical-env (reverse vars))))
        (when env (push 'IHS-ENV *unwind-exit*))
        (c2expr body)
        (wt-nl "}")
        (when env (pop-debug-lexical-env))))
    (c2expr body))

  (when closure-block-id
    (wt-nl-h "#define " (written-function-cname) "_cblock_" closure-block-id " "
	     (first *closure-levels*))
    )

  (when block-p (wt-nl "}"))
  )

(defun last-form (x &aux (args (c1form-args x)))
  (case (c1form-name x)
	(PROGN
	 (last-form (car (last (first args)))))
	((LET LET* FLET LABELS BLOCK CATCH)
	 (last-form (car (last args))))
	(VAR (c1form-arg 0 x))
	(t x)))

(defun discarded (var form body &aux last)
  (labels (#+(or) ;; generates too many closures. JCB
	     (last-form (x &aux (args (c1form-args x)))
	     (case (c1form-name x)
	       (PROGN
		 (last-form (car (last (first args)))))
	       ((LET LET* FLET LABELS BLOCK CATCH)
		(last-form (car (last args))))
	       (VAR (c1form-arg 0 x))
	       (t x))))
    (and (not (form-causes-side-effect form))
	 (or (< (var-ref var) 1)
	     (and (= (var-ref var) 1)
		  (eq var (last-form body))
		  (eq 'TRASH *destination*))))))

(defun can-be-replaced (var body)
  (declare (type var var))
  (and (eq (var-kind var) :OBJECT)
       (not (var-changed-in-form var body))))

(defun can-be-replaced* (var body forms)
  (declare (type var var))
  (and (can-be-replaced var body)
       (not (var-changed-in-form-list var forms))))

(defun nsubst-var (var form)
  (when (var-set-nodes var)
    (baboon "Cannot replace a variable that is to be changed"))
  (when (var-functions-reading var)
    (baboon "Cannot replace a variable that is closed over"))
  (when (> (length (var-read-nodes var)) 1)
    (baboon "Cannot replace a variable that is used more than once"))
  ;; FIXME!!!!
  ;; Only take the first value out of the form
  #+(or)
  (setf form (make-c1form* 'VALUES :args (list form)))
  (dolist (where (var-read-nodes var))
    (cond ((and (eql (c1form-name where) 'VAR)
		(eql (c1form-arg 0 where) var))
	   (setf (c1form-type where) (c1form-type form)
		 (c1form-sp-change where) (c1form-sp-change form)
		 (c1form-volatile where) (c1form-volatile form)
		 (c1form-name where) (c1form-name form)
		 (c1form-args where) (c1form-args form))
	   (c1form-add-info where (c1form-args where))
	   )
	  (t
	   (baboon "VAR-SET-NODES are only C1FORMS of type VAR")))))

(defun member-var (var list)
  (let ((kind (var-kind var)))
    (if (member kind '(SPECIAL GLOBAL))
	(member var list :test
		#'(lambda (v1 v2)
		    (and (member (var-kind v2) '(SPECIAL GLOBAL))
			 (eql (var-name v1) (var-name v2)))))
	(member var list))))

;;; ----------------------------------------------------------------------

(put-sysprop 'LET 'C1SPECIAL 'c1let)
(put-sysprop 'LET 'C2 'c2let)
(put-sysprop 'LET* 'C1SPECIAL 'c1let*)
(put-sysprop 'LET* 'C2 'c2let*)
