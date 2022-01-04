;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: C -*-
;;;;
;;;;  CMPTOP  --  Compiler top-level.

;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;  Copyright (c) 2010-2015, Jean-Claude Beaudoin.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Lesser General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 3 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../../Copyright' for full details.

(in-package "COMPILER")

(defun t1expr (form)
  (let ((*cmp-env* (cmp-env-new))
	(*original-current-form* form))
    (push (t1expr* form) *top-level-forms*)))

(defvar *toplevel-forms-to-print*
  '(defun defmacro defvar defparameter defclass defmethod defgeneric))

(defun t1expr* (form &aux (*current-form* form) (*first-error* t) (*setjmps* 0))
  (catch *cmperr-tag*
    (when (consp form)
      (let ((fun (car form)) (args (cdr form)) fd)
	(when (member fun *toplevel-forms-to-print*)
	  (print-current-form))
	(cond
            ((consp fun) (t1ordinary form))
            ((not (symbolp fun))
	     (cmperr "~s is illegal function." fun))
	    ((eq fun 'QUOTE)
	     (t1ordinary 'NIL))
	    ((setq fd (get-sysprop fun 'T1))
	     (funcall fd args))
	    ((or (get-sysprop fun 'C1) (get-sysprop fun 'C1SPECIAL))
             (t1ordinary form))
	    ((and (setq fd (compiler-macro-function fun))
		  (inline-possible fun)
		  (let ((success nil))
		    (multiple-value-setq (fd success)
		      (cmp-expand-macro fd form))
		    success))
	     (t1expr* fd))
	    ((setq fd (cmp-macro-function fun))
	     (t1expr* (cmp-expand-macro fd form)))
	    (t (t1ordinary form))
	   )))))

(defun t1/c1expr (form)
  (cond ((not *compile-toplevel*)
	 (c1expr form))
	((atom form)
	 (t1ordinary form))
	(t
	 (t1expr* form))))	

(defun t2expr (form)
  (when form
    (let ((def (get-sysprop (c1form-name form) 'T2)))
      (when def (apply def (c1form-args form))))))



(defun make-global-fun-refs-vector ()
  (make-array 10 :fill-pointer 0 :adjustable t))

(defun add-global-fun-ref (fname loc)
  (vector-push-extend (cons fname loc) *global-fun-refs*))

(defun find-global-fun-ref (fname)
  (let ((pos (position fname *global-fun-refs* :key #'car :test #'same-fname-p)))
    #+(or)
    (when pos
      (format t "~&In find-global-fun-ref: pos = ~S, fname = ~S, loc = ~S.~%"
	      pos fname (cdr (elt *global-fun-refs* pos)))
      (finish-output)
      )
    pos
    )
  )



(defvar *emitted-local-funs* nil)

(defun emit-local-funs ()
  (let* ((disassemble-bindings *disassemble-bindings*)
	 (d-b-symbols (car disassemble-bindings))
	 (d-b-values (cdr disassemble-bindings)))
    ;; Local functions and closure functions
    (do ((*compile-time-too* nil)
	 (*compile-toplevel* nil))
	;; repeat until t3local-fun generates no more
	((eq *emitted-local-funs* *local-funs*))
	;; scan *local-funs* backwards
	(do ((lfs *local-funs* (cdr lfs)))
	    ((eq (cdr lfs) *emitted-local-funs*)
	     (setq *emitted-local-funs* lfs)
	     (if disassemble-bindings
		 (progv d-b-symbols d-b-values (t3local-fun (first lfs)))
	       (t3local-fun (first lfs))))))))

(defun emit-fun-refs-for-local-funs (funs)
  (dolist (fun funs)
    (let ((global-fun-refs (fun-global-fun-refs fun)))
      (unless (or (eq (fun-closure fun) 'CLOSURE))
	;; Declare fun-refs here! JCB

	(wt-nl-h "static mkcl_object " (fun-cfun fun) "_fun_ref_sym_locs[] = {")
	(unless (fun-lex-local-p fun)
	  (dotimes (i (length global-fun-refs))
	    (let ((loc (cdr (aref global-fun-refs i))))
	      (if (consp loc)
		  (wt-nl-h "MKCL_MAKE_FIXNUM(" (second loc) "),")
		(wt-nl-h  loc ","))
	      (wt-h "/* " (princ-to-string (car (aref global-fun-refs i))) " */") ;; debug JCB
	      )
	    )
	  )
	(wt-nl-h "};")
	)
      )
    )
  )


(defun file-basename (pathname)
  ;;(setq pathname (pathname pathname))
  (let ((file-type (pathname-type pathname)))
    (if (and file-type (not (eq file-type :unspecific))) ;; :unspecific is equivalent to NIL here.
	(concatenate 'string (pathname-name pathname) "." (pathname-type pathname))
      (pathname-name pathname))))

(defun ctop-write (name h-pathname data-pathname
			&aux
			def
			top-output-string
			(*global-fun-refs* (make-global-fun-refs-vector)) ;; JCB
			(*volatile* " volatile "))
  (declare (ignore h-pathname data-pathname))
  (setq *top-level-forms* (nreverse *top-level-forms*))
  ;; All lines from CLINES statements are grouped at the beginning of the header
  ;; Notice that it does not make sense to guarantee that c-lines statements
  ;; are produced in-between the function definitions, because two functions
  ;; might be collapsed into one, or we might not produce that function at all
  ;; and rather inline it.
  (do ()
      ((null *clines-string-list*))
    (wt-nl-h (pop *clines-string-list*)))
  (wt-nl-h "#ifdef __cplusplus")
  (wt-nl-h "extern \"C\" {")
  (wt-nl-h "#endif")

  ;;; Initialization function.
  (let* ((*lcl* 0) (*lex* 0) (*max-lex* 0) (*max-env* 0) (*max-temp* 0)
	 (*closure-block-id* 0)
         (*aux-closure* nil)
	 (*reservation-cmacro* (next-cmacro))
	 (c-output-file *compiler-output1*)
	 (*compiler-output1* (make-string-output-stream :element-type 'base-char :encoding :UTF-8))
	 (*emitted-local-funs* nil)
	 (*compiler-declared-globals* (make-hash-table)))
    (wt-nl1 "#ifdef __cplusplus")
    (wt-nl1 "extern \"C\"")
    (wt-nl1 "#endif")
    (wt-comment-nl "optimization: speed ~D, space ~D, safety ~D, debug ~D "
		   (cmp-env-optimization 'speed)
		   (cmp-env-optimization 'space)
		   (cmp-env-optimization 'safety)
		   (cmp-env-optimization 'debug))
    (wt-comment-nl "This file external format: ~A" (stream-external-format *compiler-output1*))
    (wt-nl1 "MKCL_DLLEXPORT void " name "(MKCL, mkcl_object flag, mkcl_object this_filename)")
    (wt-nl1 "{ VT" *reservation-cmacro*
	    " VLEX" *reservation-cmacro*
            " CLSR" *reservation-cmacro*
	    " STCK" *reservation-cmacro*)

    ;; This closure block is in fact a dummy but it is usefull as environment chain termination.
    (wt-nl "const mkcl_object cenv0 = mkcl_alloc_clevel_block(env, mk_cl_Cnil,  mk_cl_Cnil, 0);")
    (wt-nl "const mkcl_object syms_cenv0"
	   " = mkcl_alloc_clevel_block(env, mk_cl_Cnil, mk_cl_Cnil, 0);")
    (wt-nl "mkcl_object value0;")
    (wt-nl "mkcl_object *VVtemp;")

    (wt-nl "if (mkcl_type_of(flag) == mkcl_t_codeblock){")
    (wt-nl "Cblock=flag;")
    (when *self-destructing-fasl*
      (wt-nl "flag->cblock.self_destruct=TRUE;"))
    (wt-nl "flag->cblock.data_size = VM;")
    (wt-nl "flag->cblock.temp_data_size = VMtemp;")
    (wt-nl "flag->cblock.data_text = compiler_data_text;")
    (wt-nl "flag->cblock.data_text_size = compiler_data_text_size;")
    (wt-nl "flag->cblock.cfuns_size = compiler_cfuns_size;")
    (wt-nl "flag->cblock.cfuns = compiler_cfuns;")
    (when *compile-file-truename*
      (wt-nl "flag->cblock.source = mkcl_make_simple_base_string(env, \""
             (preserve-escapes (namestring *compile-file-truename*)) "\");"))
    (wt-nl "return;}")
    (wt-nl "else if (flag != mk_cl_Cnil) mkcl_FEnot_codeblock_type(env, flag);")
    (wt-nl "VV = Cblock->cblock.data;")
    ;; With this we ensure creating a constant with the tag
    ;; and the initialization file
    (wt-nl "Cblock->cblock.data_text = \"" (init-name-tag name) "\";")
    
    (when si::*compiler-constants*
      (wt-nl "{ int i; mkcl_object data = mkcl_symbol_value(env, ((mkcl_object) &"
	     (nth-value 1 (si::mangle-name 'si::*compiler-constants*))
	     "));")
      (wt-nl "for (i = 0; i < VM; i++) VV[i] = data->vector.self.t[i]; }")
      ) 

    ;; build top-level fun-refs here! JCB
    (wt-nl "Cblock->cblock.nb_fun_refs = MKCL_NB_ELEMS(_mkcl_toplevel_fun_ref_sym_locs);")
    (wt-nl "Cblock->cblock.fun_ref_syms = mkcl_build_fun_ref_syms_from_locs(env, VV, _mkcl_toplevel_fun_ref_sym_locs, MKCL_NB_ELEMS(_mkcl_toplevel_fun_ref_sym_locs));")
    (wt-nl "Cblock->cblock.fun_refs = mkcl_build_fun_refs_from_syms(env, Cblock, Cblock->cblock.fun_ref_syms, MKCL_NB_ELEMS(_mkcl_toplevel_fun_ref_sym_locs));")
    (wt-nl "{ const mkcl_object * const fun_refs = Cblock->cblock.fun_refs;")

    (when (>= (cmp-env-optimization 'debug) 2)
      (wt-nl "mkcl_object _mkcl_debug_env = mk_cl_Cnil;")
      (wt-nl "struct mkcl_ihs_frame ihs;"))

    (wt-nl "VVtemp = Cblock->cblock.temp_data;")

    ;; Fix fun-refs of top-level lex-local function objects here. JCB
    (dolist (lex-local-fun *lex-local-funs*)
      (unless (eq (fun-closure lex-local-fun) 'CLOSURE)
	  (wt-nl "mkcl_fix_lex_local_fun_refs(env, "
		 ;;loc
		 "Cblock"
		 ", "
		 ;;lex-local-loc
		 "Cblock->cblock.cfun_objs[" (fun-block-index lex-local-fun) "]"
		 ");")
	  (wt-comment (fun-name lex-local-fun))
	)
      )

    (setq *compiler-phase* 't2)

    (dolist (form (nconc (nreverse *make-forms*) *top-level-forms*))
      (let* (;;(*compile-to-linking-call* nil)
             (*compile-file-truename* (and form (c1form-file form)))
             (*compile-file-end-position* (and form (c1form-file-end-position form)))
             (*env* 0) (*level* 0) (*temp* 0) (*env-lvl* 0)
	     (*closure-levels* (list 0))
	     (*debug-fun* (cmp-env-optimization 'debug))
	     )
	  (t2expr form)
	  )
      (let ((*compiler-output1* c-output-file))
	(emit-local-funs)))
    (wt-function-epilogue)
    (wt-nl "}")
    (wt-nl1 "} /* End of FAS file init function */")
    (setq top-output-string (get-output-stream-string *compiler-output1*))

    ;; emit fun-refs here!
    (emit-fun-refs-for-local-funs *emitted-local-funs*)
    )

  ;; Declarations in h-file.
  (wt-nl-h "static mkcl_object Cblock;")

  ;; Declare top-level fun-refs here! JCB
  (wt-nl-h "static mkcl_object _mkcl_toplevel_fun_ref_sym_locs[] = {")
  (dotimes (i (length *global-fun-refs*))
    (let ((loc (cdr (aref *global-fun-refs* i))))
      (if (consp loc)
	  (wt-nl-h "MKCL_MAKE_FIXNUM(" (second loc) "),")
	(wt-nl-h  loc ","))
      (wt-h "/* " (princ-to-string (car (aref *global-fun-refs* i))) " */") ;; debug JCB
      )
    )
  (wt-nl-h "};")

  (dolist (x *reservations*)
    (wt-nl-h "#define VM" (car x) " " (cdr x)))
  (freeze-data-storage) ;; JCB
  (let ((num-objects (data-size)))
    (if (zerop num-objects)
	(progn
	  (wt-nl-h "#define VM 0")
	  (wt-nl-h "#define VMtemp 0")
	  (wt-nl-h "static mkcl_object *VV = NULL;")
	  )
      (progn
	(wt-nl-h "#define VM " (data-permanent-storage-size))
	(wt-nl-h "#define VMtemp "  (data-temporary-storage-size))
	(wt-nl-h "static mkcl_object *VV;")
	)))

  (dolist (l *linking-calls*)
    (let* ((c-name (fourth l))
	   (var-name (fifth l)))
      (wt-nl-h "static mkcl_object " c-name "(mkcl_narg, ...);")
      (wt-nl-h "static mkcl_object (*" var-name ")(mkcl_narg, ...)=" c-name ";")))


  ;;; Initial functions for linking calls.
  (dolist (l *linking-calls*)
    (let* ((var-name (fifth l))
	   (c-name (fourth l))
	   (lisp-name (third l)))
      (wt-nl1 "static mkcl_object " c-name "(mkcl_narg narg, ...)"
	      "{MKCL_TRAMPOLINK(narg," lisp-name ",&" var-name ",Cblock);}")))

  (wt-nl-h "#ifdef __cplusplus")
  (wt-nl-h "}")
  (wt-nl-h "#endif")

  (when (and (listp *static-constants*)
             (setf *static-constants* (nreverse *static-constants*)))
    (wt-nl-h "/*")
    (wt-nl-h " * Statically defined constants")
    (wt-nl-h " */")
    (loop for (value name builder) in (reverse *static-constants*)
          do (terpri *compiler-output2*)
          do (funcall builder name value *compiler-output2*)))

  (output-cfuns *compiler-output2*)

  (setq *compiler-phase* 't3)

  ;;; Callbacks
  (when *callbacks*
    (wt-nl-h "#include <mkcl/internal.h>")
    (dolist (x *callbacks*)
      (apply #'t3-defcallback x)))

  (wt-nl top-output-string)
  )

(defun c1eval-when (args)
  (check-args-number 'EVAL-WHEN args 1)
  (let ((load-flag nil)
	(compile-flag nil)
	(execute-flag nil))
    (dolist (situation (car args))
      (case situation
	((LOAD :LOAD-TOPLEVEL) (setq load-flag t))
	((COMPILE :COMPILE-TOPLEVEL) (setq compile-flag t))
	((EVAL :EXECUTE)
	 (if *compile-toplevel*
	     (setq compile-flag (or *compile-time-too* compile-flag))
	     (setq execute-flag t)))
	(otherwise (cmperr "The EVAL-WHEN situation ~s is illegal." situation))))
    (cond ((not *compile-toplevel*)
	   (c1progn (and execute-flag (rest args))))
	  (load-flag
	   (let ((*compile-time-too* compile-flag))
	     (c1progn (rest args))))
	  (compile-flag
	   (cmp-eval (cons 'PROGN (rest args)))
	   (c1progn 'NIL))
	  (t
	   (c1progn 'NIL)))))

(defun t2compiler-let (symbols values body)
  (progv symbols values (c2expr body)))

(defun t2progn (args)
  (mapcar #'t2expr args))

(defun exported-fname (name)
  (let (cname found)
    (if (and (symbolp name) (setf cname (get-sysprop name 'si::Lfun)))
        (values cname t)
      (if (multiple-value-setq (found cname) (si::mangle-function-name name))
	  (values cname t)
        (values (next-cfun "L~D~A" name) nil)))))

(defun new-defun (new)
  (push new *global-funs*))

(defun print-function (x)
  (format t "~%<a FUN: ~A, CLOSURE: ~A, LEVEL: ~A, ENV: ~A>"
	  (fun-name x) (fun-closure x) (fun-level x) (fun-env x)))


(defun wt-function-prolog (&optional sp local-entry) ;; not used anymore? JCB
  (declare (ignore local-entry))
  (wt " VT" *reservation-cmacro*
      " VLEX" *reservation-cmacro*
      " CLSR" *reservation-cmacro*
      " STCK" *reservation-cmacro*)
  (wt-nl "mkcl_object value0;")
  (when sp (wt-nl "mkcl_bds_check(env);"))
  )

(defun wt-function-epilogue (&optional fun)
  (push (cons *reservation-cmacro* *max-temp*) *reservations*)
  ;; FIXME! Are we careful enough with temporary variables that
  ;; we need not make them volatile?
  (wt-nl-h "#define VT" *reservation-cmacro*)
  (when (plusp *max-temp*)
    (wt-h " mkcl_object ")
    (dotimes (i *max-temp*)
      (wt-h "T" i)
      (unless (= (1+ i) *max-temp*) (wt-h ",")))
    (wt-h ";"))
  (wt-nl-h "#define VLEX" *reservation-cmacro*)
  ;; There should be no need to mark lex as volatile, since we
  ;; are going to pass pointers of this array around and the compiler
  ;; should definitely keep this in memory.
  (when (plusp *max-lex*)
    (wt-h " volatile mkcl_object lex" *level* "[" *max-lex* "];"))
  (wt-nl-h "#define CLSR" *reservation-cmacro*)
  (wt-nl-h "#define STCK" *reservation-cmacro*)
  (when (plusp *max-env*)
    (when *aux-closure*
      (wt-h " struct mkcl_cclosure aux_closure;"))
    )
  (when (and fun (eq (fun-closure fun) 'CLOSURE))
    (wt-nl-h "#define " (fun-cfun fun) "_closure_depth " (fun-closure-depth fun))
    )
  )


(defun t1ordinary (form)
  (when *compile-time-too* (cmp-eval form))
  (let ((*compile-toplevel* nil)
	(*compile-time-too* nil))
    (add-load-time-values (make-c1form* 'ORDINARY :args (c1expr form)))))

(defun t2ordinary (form)
  (let* ((*exit* (next-label))
	 (*unwind-exit* (list *exit*))
         (*destination* 'TRASH))
    (c2expr form)
    (wt-label *exit*)))

(defun add-load-time-values (form)
  (let ((previous (append (and (consp *load-time-values*)
			       (nreverse *load-time-values*))
			  (nreverse *make-forms*))))
    (when previous
      (setf *load-time-values* nil
	    *make-forms* nil)
      (setf form (make-c1form* 'PROGN :args (nconc previous (list form))))))
  form)

(defun c1load-time-value (args)
  (check-args-number 'LOAD-TIME-VALUE args 1 2)
  (let ((form (first args))
	loc)
    (cond ((not (listp *load-time-values*))
	   ;; When using COMPILE, we set *load-time-values* to 'VALUES and
	   ;; thus signal that we do not want to compile these forms, but
	   ;; just to retain their value.
	   (return-from c1load-time-value (c1constant-value (cmp-eval form) :always t)))
          ((typep form '(or list symbol))
	   (setf loc (data-empty-loc))
	   (push (make-c1form* 'LOAD-TIME-VALUE :args loc (c1expr form))
		 *load-time-values*))
	  (t
	   (setf loc (add-object (cmp-eval form)))))
    (make-c1form* 'LOCATION :type t :args loc)))

(defun t2load-time-value (vv-loc form)
  (let* ((*exit* (next-label)) (*unwind-exit* (list *exit*))
         (*destination* vv-loc))
    (c2expr form)
    (wt-label *exit*)))

(defun t2make-form (vv-loc form)
  (let* ((*exit* (next-label)) (*unwind-exit* (list *exit*))
         (*destination* vv-loc))
    (c2expr form)
    (wt-label *exit*)))

(defun t2init-form (vv-loc form)
  (declare (ignore vv-loc))
  (let* ((*exit* (next-label)) (*unwind-exit* (list *exit*))
         (*destination* 'TRASH))
    (c2expr form)
    (wt-label *exit*)))

(defun t2decl-body (decls body)
  (let ((*cmp-env* *cmp-env*)
        (*notinline* *notinline*))
    (c1add-declarations decls)
    (t2expr body)))

(defun parse-cvspecs (x &aux (cvspecs nil))
  (dolist (cvs x (nreverse cvspecs))
    (cond ((symbolp cvs)
           (push (list :OBJECT (string-downcase (symbol-name cvs))) cvspecs))
          ((stringp cvs) (push (list :OBJECT cvs) cvspecs))
          ((and (consp cvs)
                (member (car cvs) '(OBJECT CHAR INT FLOAT DOUBLE)))
           (dolist (name (cdr cvs))
             (push (list (car cvs)
                         (cond ((symbolp name)
                                (string-downcase (symbol-name name)))
                               ((stringp name) name)
                               (t (cmperr "The C variable name ~s is illegal." name))))
                   cvspecs)))
          (t (cmperr "The C variable specification ~s is illegal." cvs))))
  )

;;(defvar *debug-fun* nil)

(defun locative-type-from-var-kind (kind)
  (cdr (assoc kind '((:object . "_mkcl_object_loc")
		     (:fixnum . "_mkcl_word_loc")
		     (:char . "_mkcl_base_char_loc") ;; FIXME, how about :wchar? JCB
		     (:float . "_mkcl_float_loc")
		     (:double . "_mkcl_double_loc")
		     (:long-float . "_mkcl_long_double_loc")
		     (closure . "_mkcl_closure_var_loc")
		     (lexical . "_mkcl_closure_var_loc")
		     ((special global closure replaced lexical) . NIL)))))

(defvar *show-var-loc* nil)

(defun closure-lcl-name (var) 
  (with-output-to-string (*compiler-output1*)
     (wt-var var)
     )
  )


(defun build-debug-lexical-env (vars)
  (let* ((var-locations '())
         (info-local-vars '())
	 )
    ;; Filter out variables that we know how to store in the
    ;; debug information table. This excludes among other things
    ;; closures and special variables.
    (loop for var in vars
          for name = (let ((*package* keyword-package))
		       (prin1-to-string (var-name var))
		       )
	  for kind = (var-kind var)
          for locative-type = (locative-type-from-var-kind kind)
          for loc = (var-loc var)
          when (and locative-type (or (and (consp loc) (eq (first loc) 'LCL)) (eq kind 'closure) (eq kind 'lexical)))
          do (progn
	       (when *show-var-loc* (format t "~&Var ~S locative-type: ~S loc: ~S~%" name locative-type loc))
               (push (cons name locative-type) info-local-vars)
	       (if (or (eq kind 'closure) (eq kind 'lexical))
		   (push (closure-lcl-name var) var-locations)
		 (push (lcl-name (second loc)) var-locations))
               ))

    ;; Generate two tables, a static one with information about the
    ;; variables, including name and type, and a dynamic one, which is
    ;; a vector of pointer to the variables.
    (when info-local-vars
      (wt-nl "static const struct mkcl_lex_var_info _mkcl_var_descriptors[]={")
      (let ((comma ""))
	(loop for (name . locative-type) in info-local-vars
	      do 
	      (wt-nl comma "{\"" name "\"," locative-type "}")
	      (setq comma ",")
	      )
	(wt "};")
	)
      (wt-nl "void * const _mkcl_var_locations[]={")
      (let ((comma ""))
	(dolist (var-loc var-locations)
	   (wt comma "(void *)(&" var-loc ")")
	   (setq comma ",")
	   )
	(wt "};")
	)
      (wt-nl "mkcl_cmp_dbg_lex_level(_mkcl_debug_env__obj_, _mkcl_debug_env, "
	     "_mkcl_var_descriptors, _mkcl_var_locations);")
      (wt-nl "mkcl_object _mkcl_debug_env = (mkcl_object) (&_mkcl_debug_env__obj_);")

      (wt-nl "ihs.lex_env=_mkcl_debug_env;")
      )
    (if info-local-vars t nil)))

(defun pop-debug-lexical-env ()
  (wt-nl "ihs.lex_env=_mkcl_debug_env;"))

(defun outer-closure (fun)
  (and (eq (fun-closure fun) 'CLOSURE)
       (do ((pfun (fun-parent fun) (fun-parent pfun)))
	   ((null pfun) nil)
	 (when (eq (fun-closure pfun) 'CLOSURE) (return pfun))
	 )
       )
  )

(defun t3local-fun (fun &aux  (lambda-expr (fun-lambda fun))
			      (level (if (eq (fun-closure fun) 'LEXICAL)
					 (fun-level fun)
					 0))
			      (cfun (fun-cfun fun))
		    	      (minarg (fun-minarg fun))
		    	      (maxarg (fun-maxarg fun))
		    	      (narg (fun-needs-narg fun))
			      (nenvs level)
			      (*volatile* (c1form-volatile* lambda-expr))
			      (*tail-recursion-info* fun)
                              (lambda-list (c1form-arg 0 lambda-expr))
                              (requireds (car lambda-list))
			      (*compile-file-truename* (fun-file fun))
			      (*compile-file-pathname* *compile-file-truename*)
			      (*compile-file-end-position* (fun-file-end-position fun))
                              (*debug-fun* (if (>= (fun-debug fun) 2) (fun-debug fun) *debug-fun*))
			      (*written-function* fun) ;; JCB
			      (*global-fun-refs*
			       (if (or (fun-lex-local-p fun) (fun-closure fun))
				   (if (fun-parent fun)
				       (progn
					 (fun-global-fun-refs (fun-parent fun)) ;; extend parent's
					 )
				     (progn
				       *global-fun-refs*)) ;; extend top-level's
				 (make-global-fun-refs-vector))) ;; JCB
			      )
  (declare (fixnum level nenvs) (ignore minarg maxarg))

  (setf (fun-global-fun-refs fun) *global-fun-refs*)

  (print-emitting fun)
  (wt-comment-nl (cond ((fun-global fun) "function definition for ~a")
                       ((eq (fun-closure fun) 'CLOSURE) "closure ~a")
                       (t "local function ~a"))
                 (or (fun-name fun) (fun-description fun) 'CLOSURE))
  (wt-comment-nl "optimization: speed ~D, space ~D, safety ~D, debug ~D "
                 (cmp-env-optimization 'speed)
                 (cmp-env-optimization 'space)
                 (cmp-env-optimization 'safety)
                 (cmp-env-optimization 'debug))

  (unless (and (eq (fun-closure fun) 'CLOSURE) (not (fun-global fun)))
    (wt-nl1 "static mkcl_object " cfun "_mkcl_cfun_object_ = mk_cl_Cnil;")
    (wt-nl-h "static mkcl_object " cfun "_mkcl_cfun_object_;")
    )
  (cond ((fun-exported fun)
	 (wt-nl-h "MKCL_DLLEXPORT mkcl_object " cfun "(MKCL")
	 (wt-nl1 "mkcl_object " cfun "(MKCL"))
	(t
	 (wt-nl-h "static mkcl_object " cfun "(MKCL")
	 (wt-nl1 "static mkcl_object " cfun "(MKCL")))
  (let ((comma ", "))
    (when narg
      (wt-h comma *volatile* "mkcl_narg")
      (wt comma *volatile* "mkcl_narg narg")
      (setf comma ", "))
    (dotimes (n level)
      (wt-h comma "volatile mkcl_object  *")
      (wt comma "volatile mkcl_object *lex" n)
      (setf comma ", "))
    (let ((lcl 0))
      (declare (fixnum lcl))
      (dolist (var requireds)
	(wt-h comma "mkcl_object " *volatile*)
	(wt comma "mkcl_object " *volatile*) (wt-lcl (incf lcl))
	(setf comma ", ")))
    (when narg
      (wt-h ", ...")
      (wt ", ..."))
    (wt-h ");")
    (wt ")"))

  (let* ((*lcl* 0) (*temp* 0) (*max-temp* 0)
	 (*lex* 0) (*max-lex* 0)
	 (*env* (fun-env fun))		; continue growing env
	 (*closure-levels* (if (fun-closure-levels fun)
			       (progn
				 (cons 0 (fun-closure-levels fun))  ; continue growing env
				 )
			     (progn
			       (list 0) ; seed a closure root in case this needs to be a closure.
			       )
			     )
			   )
	 (*closure-block-id* 0)
	 (*cenv0-used* nil)
	 (*max-env* *env*) (*env-lvl* 0)
         (*aux-closure* nil)
	 (*level* level)
	 (*exit* 'RETURN) (*unwind-exit* '(RETURN))
	 (*destination* 'RETURN)
	 (*reservation-cmacro* (next-cmacro))
	 (*inline-blocks* 1))
    (wt-nl1 "{")
    (wt " VT" *reservation-cmacro*
	" VLEX" *reservation-cmacro*
	" CLSR" *reservation-cmacro*
	" STCK" *reservation-cmacro*)
    (if (eq (fun-closure fun) 'CLOSURE)
	(progn
	  (wt-nl "const mkcl_object this_func = env->function;")
	  (wt-nl "const union mkcl_lispunion * const closure_display = this_func->cclosure.cenv;")
	  (when (>= *debug-fun* 2)
	    (wt-nl "const union mkcl_lispunion * const closure_syms = this_func->cclosure.syms_cenv;"))
	  (wt-nl "const mkcl_object * const fun_refs = this_func->cclosure.fun_refs;")
	  )
      (progn
	(wt-nl "const mkcl_object this_func = " cfun "_mkcl_cfun_object_;")
	(if (fun-parent fun)
	     ;; not top-level
	    (wt-nl "const mkcl_object * const fun_refs = this_func->cfun.fun_refs;")
	  ;; top-level
	  (if (fun-lex-local-p fun)
	      ;; top-level and lex-local
	      (wt-nl "const mkcl_object * const fun_refs = Cblock->cblock.fun_refs;")
	    ;; top-level and not lex-local
	    (wt-nl "const mkcl_object * const fun_refs = this_func->cfun.fun_refs;")))
	)
      )

    (let ((outer-closure (outer-closure fun)))
      (declare (ignorable outer-closure))
      (wt-nl "#ifdef " cfun "_cblock_0")
      (wt-nl "const mkcl_object cenv0 ="
	     " mkcl_alloc_clevel_block(env, this_func, ")
      (wt "mk_cl_Cnil, ")
      (wt cfun "_cblock_0);")
      (when (>= *debug-fun* 2)
	(wt-nl "const mkcl_object syms_cenv0 = mkcl_alloc_clevel_block(env, this_func, "
	       (if ;;outer-closure
		   (eq (fun-closure fun) 'CLOSURE)
		   "closure_syms, "
		 "mk_cl_Cnil, ")
	       cfun "_cblock_0);"))
      (wt-nl "#endif")
      )
    (wt-nl *volatile* "mkcl_object value0;")
    (when (>= *debug-fun* 2)
      (wt-nl "mkcl_object _mkcl_debug_env = mk_cl_Cnil;")
      (wt-nl "struct mkcl_ihs_frame ihs;"))
    #+(and)
    (when (policy-check-call-stack-overflow)
      ;;(wt-nl "mkcl_call_stack_check(env,value0);"))
      (wt-nl "mkcl_call_stack_check(env);"))
    (when (eq (fun-closure fun) 'CLOSURE)
      (let ((clv-used (remove-if
		       #'(lambda (x)
			   (or (not (ref-ref-ccb x)) ;; non closure variable
			       ;; special variable
			       (eq (var-kind x) 'special)
			       ;; not actually referenced
			       (and (not (var-referenced-in-form x (fun-lambda fun)))
				    (not (var-changed-in-form x (fun-lambda fun))))
			       ;; parameter of this closure
			       ;; (not yet bound, therefore var-loc is OBJECT)
			       (eq (var-loc x) 'OBJECT)))
		       (fun-referred-vars fun)))
	    l)
	(when clv-used
	  (setf clv-used (sort clv-used #'> :key #'var-loc))
	  )
	(wt-nl "/* Scanning closure data ... */")
	(when (>= *debug-fun* 3)
	  (wt-nl "{")
	  (build-debug-lexical-env clv-used)
	  (incf *inline-blocks*)
	  )
	(wt-nl "{ /* ... closure scanning finished */")
	(incf *inline-blocks*)))

    (when (>= *debug-fun* 2)
      (wt-nl "mkcl_ihs_push(env, &ihs, this_func, _mkcl_debug_env);")
      )

    (c2lambda-expr (c1form-arg 0 lambda-expr)
		   (c1form-arg 2 lambda-expr)
		   cfun (fun-name fun)
		   narg
                   (>= *debug-fun* 2)
		   (fun-closure fun))
    (wt-nl1)
    (close-inline-blocks)
    (wt-function-epilogue fun)

    (when *cenv0-used*
      (wt-nl-h "#define " cfun "_cblock_0 " (first *closure-levels*))
      )
    )
  )

;;; ----------------------------------------------------------------------
;;; Optimizer for FSET. Removes the need for a special handling of DEFUN as a
;;; toplevel form and also allows optimizing calls to DEFUN or DEFMACRO which
;;; are not toplevel, but which create no closures.
;;;
;;; The idea is as follows: when the function or macro to be defined is not a
;;; closure, we can use the auxiliary C functions c_def_c_*() instead of
;;; creating a closure and invoking mk_si_fset(). However until the C2 phase of
;;; the compiler we do not know whether a function is a closure, hence the need
;;; for a c2fset.
;;;
(defun c1fset (args)
  (destructuring-bind (fname def &optional (macro nil) (pprint nil))
      args
    (let* ((fun-form (c1expr def)))
      (if (and (eq (c1form-name fun-form) 'FUNCTION)
	       (not (eq (c1form-arg 0 fun-form) 'GLOBAL)))
	  (let ((fun-object (c1form-arg 2 fun-form)))
	    (when (and (typep macro 'boolean)
		       (typep pprint '(or integer null))
		       (consp fname)
		       (eq (first fname) 'quote))
	      (return-from c1fset
		(make-c1form* 'SI:FSET
                              :type 'FUNCTION
                              :args
			      fun-object ;; Function object
			      macro
			      pprint
			      ;; The c1form, when we do not optimize
			      (list (c1expr fname)
				    fun-form
				    (c1expr macro)
				    (c1expr pprint))))))))
    (c1call-global 'SI:FSET (list fname def macro pprint))))

(defun c2fset (fun macro pprint c1forms)
  (declare (ignore pprint))
  (unless (and (not macro)
               (not (fun-closure fun))
	       (eq *destination* 'TRASH))
    (return-from c2fset
      (c2call-global 'SI:FSET c1forms (c1form-primary-type (second c1forms)))))
  (let ((*inline-blocks* 0))
    ;; Process lex-local functions here! JCB
    ;; FIXME! Look at c2function!
    (new-local fun)
    (wt-nl (if macro "mkcl_cmp_defmacro(env, " "mkcl_cmp_defun(env, ")
	   "Cblock->cblock.cfun_objs[" (fun-block-index fun) "]"
	   ");")
    (wt-comment (fun-name fun))
    ;; Fix fun-refs of lex-local functions here! JCB
    (dolist (lex-local-fun (fun-lex-local-funs fun))
      (unless (eq (fun-closure lex-local-fun) 'CLOSURE)
	(let (;;(lex-local-loc (data-empty-loc))
	      )
	  #+(or)
	  (push (list lex-local-loc (add-symbol (fun-name lex-local-fun)) lex-local-fun)
		*global-cfuns-array*)
	  (wt-nl "mkcl_fix_lex_local_fun_refs(env, "
		 "Cblock->cblock.cfun_objs[" (fun-block-index fun) "]"
		 ", "
		 "Cblock->cblock.cfun_objs[" (fun-block-index lex-local-fun) "]"
		 ");")
	  (wt-comment (fun-name lex-local-fun))
	  )
	)
      )
    (close-inline-blocks))
  )

(defun output-cfuns (stream)
  (let ((n-cfuns (length *global-cfuns-array*)))
    (wt-nl-h "/*")
    (wt-nl-h " * Exported Lisp functions")
    (wt-nl-h " */")
    (wt-nl-h "#define compiler_cfuns_size " n-cfuns)
    (if (zerop n-cfuns)
        (wt-nl-h "#define compiler_cfuns NULL")
        (progn
          (format stream "~%static const struct mkcl_cfun compiler_cfuns[] = {~
~%~t/*t,m,padding[0],padding[1],f.entry,f._[0],f._[1],f._[2],f._[3],f._[4],name,block,old_entry_fixed,file,file_position,narg,anchor,nb_fun_refs,fun_ref_syms*/");
          (loop for fun across *global-cfuns-array*
                do (let* ((cfun (fun-cfun fun))
                          (minarg (fun-minarg fun))
                          (maxarg (fun-maxarg fun))
                          (narg (if (and (= minarg maxarg) (<= maxarg si:c-arguments-limit)) maxarg -1)))
		     (if (eq (fun-closure fun) 'CLOSURE)
			 (format stream
				 "~%{0,0,0,0,{NULL,NULL,NULL,NULL,NULL,NULL},~
                                  mk_cl_Cnil,mk_cl_Cnil,NULL,~
                                  mk_cl_Cnil,mk_cl_Cnil,0,NULL,~
                                  0, NULL}, /* ~A */"
				 (fun-name fun)
				 )
		       (format stream 
			       "~%{0,0,0,0,{(mkcl_objectfn)~A,NULL,NULL,NULL,NULL,NULL},~
                                MKCL_MAKE_FIXNUM(~D),MKCL_MAKE_FIXNUM(~D),NULL,~
                                mk_cl_Cnil,MKCL_MAKE_FIXNUM(~D),~D, &~A~A,~
                                MKCL_NB_ELEMS(~A~A), ~A~A}, ~
                                /* minarg = ~D, maxarg = ~D */"
			       cfun
			       (fun-block-index fun)
			       (second (fun-name-loc fun))
			       (fun-file-end-position fun) narg
			       cfun "_mkcl_cfun_object_"
			       cfun "_fun_ref_sym_locs"
			       cfun "_fun_ref_sym_locs"
			       minarg maxarg
			       )
		       )
		     ))
          (format stream "~%};")))))

;;; ----------------------------------------------------------------------

;;; Pass 1 top-levels.

(put-sysprop 'COMPILER-LET 'T1 #'c1compiler-let)
(put-sysprop 'EVAL-WHEN 'T1 #'c1eval-when)
(put-sysprop 'PROGN 'T1 #'c1progn)
(put-sysprop 'MACROLET 'T1 #'c1macrolet)
(put-sysprop 'LOCALLY 'T1 #'c1locally)
(put-sysprop 'SYMBOL-MACROLET 'T1 #'c1symbol-macrolet)
(put-sysprop 'LOAD-TIME-VALUE 'C1SPECIAL 'c1load-time-value)
(put-sysprop 'SI:FSET 'C1 'c1fset)

;;; Pass 2 initializers.

(put-sysprop 'COMPILER-LET 'T2 #'t2compiler-let)
(put-sysprop 'DECL-BODY 't2 #'t2decl-body)
(put-sysprop 'PROGN 'T2 #'t2progn)
(put-sysprop 'ORDINARY 'T2 #'t2ordinary)
(put-sysprop 'LOAD-TIME-VALUE 'T2 't2load-time-value)
(put-sysprop 'MAKE-FORM 'T2 't2make-form)
(put-sysprop 'INIT-FORM 'T2 't2init-form)
(put-sysprop 'SI:FSET 'C2 'c2fset)

