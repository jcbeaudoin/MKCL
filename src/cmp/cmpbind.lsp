;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: C -*-
;;;;
;;;;  CMPBIND  Variable Binding.
;;;;
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;  Copyright (c) 2010-2011, Jean-Claude Beaudoin.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Lesser General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 3 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../../Copyright' for full details.

(in-package "COMPILER")

;;; bind must be called for each variable in a lambda or let, once the value
;;; to be bound has been placed in loc.
;;; bind takes care of setting var-loc.

(defun print-full-name (var-name)
  (let ((*package* keyword-package))
    (prin1-to-string var-name)))

(defun bind (loc var)
  ;; loc can be either (LCL n), 'VA-ARGS, (KEYVARS n), (CAR n),
  ;; a constant, or (VAR var) from a let binding. ; ccb
  (declare (type var var))
  (case (var-kind var)
    (CLOSURE
     (let ((var-loc (var-loc var)))
       (unless (sys:fixnump var-loc)
	 ;; first binding: assign location
	 (setq var-loc (next-env))
	 (setf (var-loc var) var-loc)
	 (setf (var-cloc var) (cons (length *closure-levels*) (car *closure-levels*)))
	 )

       (setq *cenv0-used* t)
       (wt-nl "cenv" *env-lvl* "->lblock.var[" (1- (cdr (var-cloc var))) "] = ")
       (wt-coerce-loc :object loc) (wt ";")

       (wt-comment (var-name var))
       (when (>= *debug-fun* 2)
	 ;; (wt-nl "syms_cenv" *env-lvl* "->lblock.var[" (1- (cdr (var-cloc var))) "] = "
	 ;; 	"mkcl_make_simple_base_string(env, \"" (print-full-name (var-name var)) "\");")
	 (wt-nl "{")
	 (wt-nl "mkcl_UTF_8_object(raw_var_name_obj, \"" (print-full-name (var-name var)) "\");")
	 (wt-nl "syms_cenv" *env-lvl* "->lblock.var[" (1- (cdr (var-cloc var))) "] = "
		"mkcl_utf_8_to_string(env, (mkcl_object) &raw_var_name_obj);")
	 (wt-nl "}")
	 )
       )
     )
    (LEXICAL
     (let ((var-loc (var-loc var)))
       (unless (consp var-loc)
	 ;; first binding: assign location
	 (setq var-loc (next-lex))
	 (setf (var-loc var) var-loc))
       (wt-nl) (wt-lex var-loc) (wt "= ")
       (wt-coerce-loc :object loc)
       (wt ";"))
       (wt-comment (var-name var)))
    ((SPECIAL GLOBAL)
     (bds-bind loc var))
    (t
     (cond ((not (eq (var-loc var) 'OBJECT))
	    ;; already has location (e.g. optional in lambda list)
	    ;; check they are not the same
	    (unless (equal (var-loc var) loc)
	      (wt-nl var "= ")
	      (wt-coerce-loc (var-rep-type var) loc)
	      (wt ";")))
	   ((and (consp loc) (eql (car loc) 'LCL))
	    ;; set location for lambda list requireds
	    (setf (var-loc var) loc))
	   (t
	    (baboon)))
	 )))

;;; Used by let*, defmacro and lambda's &aux, &optional, &rest, &keyword
(defun bind-init (form var)
  (let ((kind (var-kind var)))
    (if (member kind '(CLOSURE LEXICAL SPECIAL GLOBAL))
	;; Binding these variables is complicated and involves lexical
	;; environments, global environments, etc. If we use `(BIND var)
	;; as destination, BIND might receive the wrong environment.
	(let* ((*inline-blocks* 0)
	       (*temp* *temp*)
	       (locs (coerce-locs (inline-args (list form)))))
	  (bind (first locs) var)
	  (close-inline-blocks)
	  ;; Notice that we do not need to update *UNWIND-EXIT*
	  ;; because BIND does it for us.
	  )
	;; The simple case of a variable which is local to a function.
	(let ((*destination* `(BIND ,var)))
	  (c2expr* form)))))

(defun bds-bind (loc var)
  ;; Optimize the case (let ((*special-var* *special-var*)) ...)
  (cond ((and (var-p loc)
	      (member (var-kind loc) '(global special))
	      (eq (var-name loc) (var-name var)))
	 (wt-nl "mkcl_bds_push(env," (var-loc var) ");"))
	(t
	 (wt-nl "mkcl_bds_bind(env," (var-loc var) ",")
	 (wt-coerce-loc :object loc)
	 (wt ");")))
  (push 'BDS-BIND *unwind-exit*)
  (wt-comment (var-name var)))

(put-sysprop 'BIND 'SET-LOC 'bind)
