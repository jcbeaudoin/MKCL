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

;;;; CMPVAR  Variables.

(in-package "COMPILER")

(defun make-var (&rest args)
  (let ((var (apply #'%make-var args)))
    (unless (member (var-kind var) '(SPECIAL GLOBAL))
      (when *current-function*
	(push var (fun-local-vars *current-function*))))
    var))

(defun var-referenced-in-form-list (var form-list)
  (dolist (f form-list nil)
    (when (var-referenced-in-form var f)
      (return t))))

(defun var-changed-in-form-list (var form-list)
  (dolist (f form-list nil)
    (when (var-changed-in-form var f)
      (return t))))

;;; FIXME! VAR-REFERENCED-IN-FORM and VAR-CHANGED-IN-FORM are too
;;; pessimistic. One should check whether the functions reading/setting the
;;; variable are actually called from the given node.  The problem arises when
;;; we create a closure of a function, as in
;;;
;;;	(let* ((a 1) (b #'(lambda () (incf a)))) ...)
;;;
;;; To know whether A is changed or read, we would have to track where B is
;;; actually used.

(defun var-referenced-in-form (var form)
  (declare (type var var))
  (if (eq (var-kind var) 'REPLACED)
      (let ((loc (var-loc var)))
	(when (var-p loc)
	  (var-referenced-in-forms loc form)))
      (or (find-node-in-list form (var-read-nodes var))
	  (var-functions-reading var))))

(defun var-changed-in-form (var form)
  (declare (type var var))
  (let ((kind (var-kind var)))
    (if (eq (var-kind var) 'REPLACED)
	(let ((loc (var-loc var)))
	  (when (var-p loc)
	    (var-changed-in-form loc form)))
	(or (find-node-in-list form (var-set-nodes var))
	    (if (or (eq kind 'SPECIAL) (eq kind 'GLOBAL))
		(c1form-sp-change form)
		(var-functions-setting var))))))

(defun add-to-read-nodes (var form)
  (push form (var-read-nodes var))
  (when *current-function*
    (unless (eq *current-function* (var-function var))
      (pushnew *current-function* (var-functions-reading var))
      (pushnew var (fun-referred-vars *current-function*))))
  form)

(defun add-to-set-nodes (var form)
  (push form (var-set-nodes var))
  (when *current-function*
    (unless (eq *current-function* (var-function var))
      (pushnew *current-function* (var-functions-setting var))
      (pushnew var (fun-referred-vars *current-function*))))
  form)

(defun add-to-set-nodes-of-var-list (var-list form)
  (dolist (v var-list)
    (add-to-set-nodes v form))
  form)

;;; A special binding creates a var object with the kind field SPECIAL,
;;; whereas a special declaration without binding creates a var object with
;;; the kind field GLOBAL.  Thus a reference to GLOBAL may need to make sure
;;; that the variable has a value.

;;;  Bootstrap problem: proclaim needs this function:
(defun sch-global (name)
  (dolist (var *undefined-vars* nil)
    (declare (type (or null var) var))
    (when (eq (var-name var) name)
      (return-from sch-global var))))

;;;
;;; Check if a variable has been declared as a special variable with a global
;;; value.

(defun check-global (name)
  (member name *global-vars* :test #'eq :key #'var-name))

;;;
;;; Check if the symbol has a symbol macro
;;;
(defun chk-symbol-macrolet (form)
  (loop
   (when (not (symbolp form))
     (return form))
   (let ((new-form (macroexpand-1 form *cmp-env*)))
     (when (eq new-form form)
       (return form))
     (setf form new-form))))

(defun c1make-var (name specials ignores types ignorables dynamics)
  (cmpck (not (symbolp name)) "The variable ~s is not a symbol." name)
  (cmpck (constantp name) "The constant ~s is being bound." name)
  (let (type)
    (if (setq type (assoc name types))
	(setq type (si::type-filter (cdr type)))
	(setq type 'T))
    (cond ((or (member name specials)
	       (sys:specialp name)
               (check-global name))
           (when (member name ignores)
             (cmpwarn-style "Variable ~s declared special while explicitly ignored." name))
           (unless type
	     (setf type (or (get-sysprop name 'TYPE) 'T)))
	   (c1make-global-variable name :kind 'SPECIAL :type type))
          (t
	   (make-var :name name :type type :loc 'OBJECT
		     :kind 'LEXICAL ; we rely on check-vref to fix it
		     :ref (if (member name ignores) -1 0)
		     :ignorable (if (member name ignorables) t nil)
		     :extent (if (member name dynamics) 'DYNAMIC nil))))))

(defun check-vref (var)
  (when (eq (var-kind var) 'LEXICAL)
    (when (zerop (var-ref var)) ;;; This field may be -1 (IGNORE). Beppe
      (unless (var-ignorable var)
        (cmpwarn-style "The variable ~s is not used." (var-name var))))
    (when (not (var-ref-clb var))
      ;; if the variable can be stored locally, set it var-kind to its type
      (setf (var-kind var)
	    (if (plusp (var-ref var))
		(lisp-type->rep-type (var-type var))
		:OBJECT)))))

(defun c1var (name)
  (let ((vref (c1vref name)))
    (unless (var-p vref)
      ;; This might be the case if there is a symbol macrolet
      (return-from c1var vref))
    (let ((output (make-c1form* 'VAR :type (var-type vref)
				:args vref)))
      (add-to-read-nodes vref output)
      output)
    #+nil
    (add-to-read-nodes vref (make-c1form* 'VAR :type (var-type vref)
					  :args vref))))

(defun make-lcl-var (&key rep-type (type 'T))
  (unless rep-type
    (setq rep-type (if type (lisp-type->rep-type type) :object)))
  (unless type
    (setq type 'T))
  (make-var :kind rep-type :type type :loc `(LCL ,(incf *lcl*))))

(defun make-temp-var (&optional (type 'T))
  (make-var :kind :object :type type :loc `(TEMP ,(next-temp))))

;;; A variable reference (vref for short) is a list: pair
;;;	( var-object ) Beppe(ccb) ccb-reference )

(defun c1vref (name)
  (multiple-value-bind (var ccb clb unw)
      (cmp-env-search-var name)
    (declare (ignore unw))
    (cond ((null var)
	   (c1make-global-variable name :warn t
				   :type (or (get-sysprop name 'TYPE) t)))
	  ((not (var-p var))
	   ;; symbol-macrolet
	   (baboon))
	  (t
           (when (minusp (var-ref var)) ; IGNORE.
             (cmpwarn-style "The ignored variable ~s is used." name)
             (setf (var-ref var) 0))
	   (when (eq (var-kind var) 'LEXICAL)
	     (cond (ccb (setf (var-ref-clb var) nil ; replace a previous 'CLB
			      (var-ref-ccb var) t
			      (var-kind var) 'CLOSURE
			      (var-loc var) 'OBJECT))
		   (clb (setf (var-ref-clb var) t
			      (var-loc var) 'OBJECT))))
           (incf (var-ref var))
	   var))))

(defun push-vars (v)
  (setf (var-index v) (length (cmp-env-variables)))
  (cmp-env-register-var v))

(defun unboxed (var)
  (not (eq (var-rep-type var) :object)))

(defun local (var)
  (and (not (member (var-kind var) '(LEXICAL CLOSURE SPECIAL GLOBAL REPLACED)))
       (var-kind var)))

(defun c2var (vref) (unwind-exit vref))

(defun c2location (loc) (unwind-exit loc))

(defun wt-var (var &aux (var-loc (var-loc var))) ; ccb
  (declare (type var var))
  (case (var-kind var)
    (CLOSURE 
     (let* ((cloc (var-cloc var))
	    (level (car cloc))
	    (index (cdr cloc))
	    (var-fun (var-function var)))
       (if var-fun
	   (if (eq *written-function* var-fun)
	       (progn
		 (let ((local-level (- level (fun-closure-depth var-fun))))
		   (wt "cenv" (1- local-level) "->lblock.var[" (1- index) "]")
		   )
		 )
	     (progn
	       (wt "closure_display->display.level[" (1- level) "]->lblock.var[" (1- index) "]")
	       )
	     )
	 (if *written-function*
	     (progn
	       (wt "closure_display->display.level[" (1- level) "]->lblock.var[" (1- index) "]")
	       )
	   (progn
	     (wt "cenv" (1- level) "->lblock.var[" (1- index) "]")
	     )
	   )
	 )
       )
     )
    (LEXICAL (wt-lex var-loc))
    (REPLACED (wt var-loc))
    ((SPECIAL GLOBAL)
     (if (safe-compile)
	 (wt "mkcl_symbol_value(env, " var-loc ")")
	 (wt "MKCL_SYM_VAL(env," var-loc ")")))
    (t (wt var-loc))
    ))

(defun var-rep-type (var)
  (case (var-kind var)
    ((LEXICAL CLOSURE SPECIAL GLOBAL) :object)
    (REPLACED (loc-representation-type (var-loc var)))
    (t (var-kind var))))

(defun set-var (loc var &aux (var-loc (var-loc var))) ;  ccb
  (if (var-p var)
    (case (var-kind var)
      (CLOSURE
       (wt-nl)(wt-var var)(wt "= ")
       (wt-coerce-loc (var-rep-type var) loc)
       (wt #\;))
      (LEXICAL
       (wt-nl)(wt-lex var-loc)(wt "= ")
       (wt-coerce-loc (var-rep-type var) loc)
       (wt #\;))
      ((SPECIAL GLOBAL)
;;       (if (safe-compile)
;;	   (wt-nl "mk_cl_set(env, " var-loc ",")
	   (wt-nl "MKCL_SETQ(env," var-loc ",") ;; I believe this is always safe now. JCB
;;	   )
       (wt-coerce-loc (var-rep-type var) loc)
       (wt ");"))
      (t
       (wt-nl var-loc "= ")
       (wt-coerce-loc (var-rep-type var) loc)
       (wt #\;))
    )
    (baboon)))

(defun wt-lex (lex)
  (if (consp lex)
    (wt "lex" (car lex) "[" (cdr lex) "]")
    (wt-lcl lex)))


;;; ----------------------------------------------------------------------

(defun c1make-global-variable (name &key (type t) (kind 'GLOBAL) (warn nil))
  (let ((var (find name *global-var-objects* :key #'var-name)))
    (unless var
      (setf var (make-var :name name :kind kind :type type :loc (add-symbol name))))
    (push var *global-var-objects*)
    (when warn
      (unless (or (sys:specialp name) (constantp name) (check-global name))
	(undefined-variable name)
	(push var *undefined-vars*)))
    var))

(defun c1declare-specials (globals)
  (mapc #'cmp-env-declare-special globals))

(defun register-global (name)
  (unless (check-global name)
    (push (c1make-global-variable name :kind 'GLOBAL
				  :type (or (get-sysprop name 'TYPE) 'T))
	  *global-vars*))
  (values))

(defun c1setq (args)
  (let ((l (length args)))
    (declare (fixnum l))
    (cmpck (oddp l) "SETQ requires an even number of arguments.")
    (cond ((zerop l) (c1nil))
	  ((= l 2) (c1setq1 (first args) (second args)))
	  (t
	   (do ((pairs args (cddr pairs))
		(forms nil))
	       ((endp pairs)
		(make-c1form* 'PROGN
			      :type (c1form-type (first forms))
			      :args (nreverse forms)))
             (push (c1setq1 (first pairs) (second pairs)) forms)
             )))))

(defun c1setq1 (name form)
  (cmpck (not (symbolp name)) "The variable ~s is not a symbol." name)
  (cmpck (constantp name) "The constant ~s is being assigned a value." name)
  (setq name (chk-symbol-macrolet name))
  (unless (symbolp name)
    (return-from c1setq1 (c1expr `(setf ,name ,form))))
  (let* ((name1 (c1vref name))
	 (form1 (c1expr form))
	 (type (type-and (var-type name1) (c1form-primary-type form1))) ;; This type check is too weak! JCB
	 )
    (unless type
      (cmpwarn "Type mismatch between ~s and ~s." name form)
      (setq type T) ;; And what is this supposed to accomplish, destroy further type-checking? JCB
      )
    ;; Is this justified????
    (add-to-set-nodes name1 (make-c1form* 'SETQ :type type :args name1 form1))))

(defun c2setq (vref form)
  (let ((*destination* vref)) (c2expr* form))
  (if (eq (c1form-name form) 'LOCATION)
    (c2location (c1form-arg 0 form))
    (unwind-exit vref))
  )

(defun c1progv (args)
  (check-args-number 'PROGV args 2)
  (let ((symbols (c1expr (first args)))
	(values (c1expr (second args)))
	(forms (c1progn (cddr args))))
    (make-c1form* 'PROGV :type (c1form-type forms)
		  :args symbols values forms)))

(defun c2progv (symbols values body
                &aux (*unwind-exit* *unwind-exit*))
  (let* ((*lcl* *lcl*)
         (lcl (next-lcl))
         (sym-loc (make-lcl-var))
         (val-loc (make-lcl-var)))
    (wt-nl "{mkcl_object " sym-loc "," val-loc ";")
    (wt-nl "mkcl_index " lcl " = env->bds_top - env->bds_org;")
    (push lcl *unwind-exit*)
    
    (let ((*destination* sym-loc)) (c2expr* symbols))
    
    (let ((*destination* val-loc)) (c2expr* values))
    
    (wt-nl "while(!mkcl_endp(env, " sym-loc ")) {")
    (when (safe-compile)
      (wt-nl "if(mkcl_type_of(MKCL_CAR(" sym-loc "))!=mkcl_t_symbol)")
      (wt-nl "mkcl_FEinvalid_variable(env, \"progv asked to bind ~s, which is not a symbol.\",MKCL_CAR(" sym-loc "));"))
    (wt-nl "if(mkcl_endp(env, " val-loc "))mkcl_bds_bind(env,MKCL_CAR(" sym-loc "),MKCL_OBJNULL);")
    (wt-nl "else{mkcl_bds_bind(env,MKCL_CAR(" sym-loc "),MKCL_CAR(" val-loc "));")
    (wt-nl val-loc "=MKCL_CDR(" val-loc ");}")
    (wt-nl sym-loc "=MKCL_CDR(" sym-loc ");}")

    (c2expr body)
    (wt "}")
    )
  )

(defun c1psetq (old-args &aux (args nil) (use-psetf nil))
  (do (var (l old-args (cddr l)))
      ((endp l))
      (declare (object l))
      (setq var (car l))
      (cmpck (not (symbolp var))
             "The variable ~s is not a symbol." var)
      (cmpck (endp (cdr l))
             "No form was given for the value of ~s." var)
      (setq var (chk-symbol-macrolet var))
      (setq args (nconc args (list var (second l))))
      (if (symbolp var)
	(cmpck (constantp var)
	       "The constant ~s is being assigned a value." var)
	(setq use-psetf t)))
  (when use-psetf
    (return-from c1psetq (c1expr `(psetf ,@args))))
  (do ((l args (cddr l))
       (vrefs '())
       (forms '()))
      ((endp l)
       (add-to-set-nodes-of-var-list
	vrefs (make-c1form* 'PSETQ :type '(MEMBER NIL)
			    :args (reverse vrefs) (nreverse forms))))
    (let* ((vref (c1vref (first l)))
	   (form (c1expr (second l)))
	   (type (type-and (var-type vref) (c1form-primary-type form)))) ;; This type check is too weak! JCB
      (unless type
	(cmpwarn "Type mismatch between ~s and ~s." (var-name vref) form)
	(setq type T))
	;; Is this justified????
	#+nil(setf (c1form-type form) type)
	(push vref vrefs)
	(push form forms))))

(defun c2psetq (vrefs forms &aux (*lcl* *lcl*) (saves nil) (blocks 0))
  ;; similar to inline-args
  (do ((vrefs vrefs (cdr vrefs))
       (forms forms (cdr forms))
       (var) (form))
      ((null vrefs))
    (setq var (first vrefs)
	  form (car forms))
    (if (or (var-changed-in-form-list var (rest forms))
	    (var-referenced-in-form-list var (rest forms)))
        (case (c1form-name form)
          (LOCATION (push (cons var (c1form-arg 0 form)) saves))
          (otherwise
            (if (local var)
                (let* ((rep-type (var-rep-type var))
		       (rep-type-name (rep-type-name rep-type))
		       (temp (make-lcl-var :rep-type rep-type)))
                  (wt-nl "{" *volatile* rep-type-name " " temp ";")
                  (incf blocks)
                  (let ((*destination* temp)) (c2expr* form))
                  (push (cons var temp) saves))
                (let ((*destination* (make-temp-var)))
                  (c2expr* form)
                  (push (cons var *destination*) saves)))))
        (let ((*destination* var)) (c2expr* form))))
  (dolist (save saves) (set-var (cdr save) (car save)))
  (dotimes (i blocks) (wt "}"))
  (unwind-exit nil)
  )

;;; ----------------------------------------------------------------------

(put-sysprop 'VAR 'C2 'c2var)
(put-sysprop 'LOCATION 'C2 'c2location)
(put-sysprop 'SETQ 'c1special 'c1setq)
(put-sysprop 'SETQ 'C2 'c2setq)
(put-sysprop 'PROGV 'c1special 'c1progv)
(put-sysprop 'PROGV 'C2 'c2progv)
(put-sysprop 'PSETQ 'c1 'c1psetq)
(put-sysprop 'PSETQ 'C2 'c2psetq)

