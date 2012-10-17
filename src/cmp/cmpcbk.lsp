;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: C -*-
;;;;
;;;;  CMPCBK --  Callbacks: lisp functions that can be called from the C world

;;;;  Copyright (c) 2003, Juan Jose Garcia-Ripoll.
;;;;  Copyright (c) 2012, Jean-Claude Beaudoin.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Lesser General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 3 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../../Copyright' for full details.

(in-package "COMPILER")

(defun c1-defcallback (args)
  (destructuring-bind (name return-type arg-list &rest body)
      args
    (let ((arg-types '())
	  (arg-type-constants '())
	  (arg-variables '())
	  (c-name (format nil "mkcl_callback_~d" (length *callbacks*)))
	  (name (if (consp name) (first name) name))
	  (call-type (if (consp name) (second name) :cdecl)))
      (dolist (i arg-list)
	(unless (consp i)
	  (cmperr "Syntax error in CALLBACK form: C type is missing in argument ~A "i))
	(push (first i) arg-variables)
	(let ((type (second i)))
	  (push (second i) arg-types)
	  (push (if (ffi::foreign-elt-type-p type)
		    (foreign-elt-type-code type)
		    (add-object type))
		arg-type-constants)))
      (push (list name c-name (add-object name)
		  return-type (reverse arg-types) (reverse arg-type-constants) call-type)
	    *callbacks*)
      (c1expr
       `(progn
	 ;; defun does not seem to work very well when not at toplevel!
	 ;;(defun ,name ,(reverse arg-variables) ,@body)
	 (setf (symbol-function ',name)
	       #'(si::lambda-block ,name ,(reverse arg-variables) ,@body))
	 (si::put-sysprop ',name :callback
	  (list
	  (ffi:c-inline () () :object
	   ,(format nil "mkcl_make_foreign(env,@':pointer-void,0,~a)" c-name)
	   :one-liner t)))))
      )))

(defconstant +foreign-elt-type-codes+
  '((:char . "MKCL_FFI_CHAR")
    (:unsigned-char . "MKCL_FFI_UNSIGNED_CHAR")
    (:byte . "MKCL_FFI_BYTE")
    (:unsigned-byte . "MKCL_FFI_UNSIGNED_BYTE")
    (:short . "MKCL_FFI_SHORT")
    (:unsigned-short . "MKCL_FFI_UNSIGNED_SHORT")
    (:int . "MKCL_FFI_INT")
    (:unsigned-int . "MKCL_FFI_UNSIGNED_INT")
    (:long . "MKCL_FFI_LONG")
    (:unsigned-long . "MKCL_FFI_UNSIGNED_LONG")
    (:long-long . "MKCL_FFI_LONG_LONG")
    (:unsigned-long-long . "MKCL_FFI_UNSIGNED_LONG_LONG")
    (:pointer-void . "MKCL_FFI_POINTER_VOID")
    (:cstring . "MKCL_FFI_CSTRING")
    (:object . "MKCL_FFI_OBJECT")
    (:float . "MKCL_FFI_FLOAT")
    (:double . "MKCL_FFI_DOUBLE")
    (:long-double . "MKCL_FFI_LONG_DOUBLE")
    (:void . "MKCL_FFI_VOID")))

(defun foreign-elt-type-code (type)
  (let ((x (assoc type +foreign-elt-type-codes+)))
    (unless x
      (cmperr "~a is not a valid elementary FFI type" x))
    (cdr x)))

(defun t3-defcallback (lisp-name c-name c-name-constant return-type
		       arg-types arg-type-constants call-type &aux (return-p t))
  (cond ((ffi::foreign-elt-type-p return-type))
	((member return-type '(nil :void))
	 (setf return-p nil))
	((and (consp return-type)
	      (member (first return-type) '(* array)))
	 (setf return-type :pointer-void))
	(t
	 (cmperr "DEFCALLBACK does not support complex return types such as ~A" return-type)))
  (let ((return-type-name (rep-type-name (ffi::%convert-to-arg-type return-type)))
	(fmod (case call-type
		(:cdecl "")
		(:stdcall "__stdcall ")
		(t (cmperr "DEFCALLBACK does not support ~A as calling convention" call-type)))))
    (wt-nl1 "static " return-type-name " " fmod c-name "(")
    (wt-nl-h "static " return-type-name " " fmod c-name "(")
    (if arg-types
	(loop for n from 0
	      and type in arg-types
	      with comma = ""
	      do
	      (progn
		(wt comma (rep-type-name (ffi::%convert-to-arg-type type)) " var" n)
		(wt-h comma (rep-type-name (ffi::%convert-to-arg-type type)) " var" n)
		(setf comma ",")))
      (progn (wt "void") (wt-h "void"))
      )
    (wt ")") (wt-h ");")
    (wt-nl1 "{")
    (when return-p
      (wt-nl return-type-name " output;"))
    (wt-nl "const mkcl_env env = mkcl_thread_env();")
    (wt-nl "if (env == NULL) {")
    (wt-nl "  return " c-name "_import_wrapper(")
    (when arg-types
      (let ((comma ""))
	(dotimes (n (length arg-types))
	  (wt comma "var" n)
	  (setf comma ","))))
    (wt-nl ");")
    (wt-nl "} else {")
    (wt-nl "const mkcl_object this_func = env->function;")
    (wt-nl "mkcl_object aux;")
    (wt-nl "MKCL_BUILD_TEMP_STACK_FRAME(env, frame, helper)")
    (loop for n from 0
	  and type in arg-types
	  and ct in arg-type-constants
	  do
	  (if (stringp ct)
	      (wt-nl "mkcl_temp_stack_frame_push(env, frame,mkcl_foreign_ref_elt(env, &var"
                     n "," ct "));")
	      (wt-nl "mkcl_temp_stack_frame_push(env, frame,mkcl_make_foreign(env, &var"
                     n "," ct "," (ffi:size-of-foreign-type type) "));")))

    ;; here is the lisp callback function invocation.
    (wt-nl "aux = mkcl_apply_from_temp_stack_frame(env, frame, mkcl_fdefinition(env, " c-name-constant "));")

    (wt-nl "mkcl_temp_stack_frame_close(env, frame);")

    (when return-p
      (wt-nl "mkcl_foreign_set_elt(env, &output,"
	     (foreign-elt-type-code return-type) ",aux);")
      (wt-nl "{ errno = 0; return output; }"))
    (wt-nl "}")
    (wt-nl1 "}")

    ;; emit thread import wrapper. JCB
    (wt-nl1 "static const mkcl_base_string_object(" c-name "_import_thread_name__obj_,")
    (wt-nl #\" (string lisp-name) " callback\");")
    (wt-nl1 "static const mkcl_object " c-name "_import_thread_name = (mkcl_object) &" c-name "_import_thread_name__obj_;")
    (wt-nl1 "static " return-type-name " " fmod c-name "_import_wrapper(")
    (wt-nl-h "static " return-type-name " " fmod c-name "_import_wrapper(")
    (if arg-types
	(loop for n from 0
	      and type in arg-types
	      with comma = ""
	      do
	      (progn
		(wt comma (rep-type-name (ffi::%convert-to-arg-type type)) " var" n)
		(wt-h comma (rep-type-name (ffi::%convert-to-arg-type type)) " var" n)
		(setf comma ",")))
      (progn (wt "void") (wt-h "void"))
      )
    (wt ")") (wt-h ");")
    (wt-nl1 "{")
    (when return-p
      (wt-nl return-type-name " output = 0;"))

    (wt-nl "const mkcl_env env = mkcl_import_current_thread(" c-name "_import_thread_name, mk_cl_Cnil, NULL, NULL);")
    (wt-nl "if (env==NULL)")
    (if return-p
	(wt-nl "{ errno = ENOMEM; return(output); }")
      (wt-nl "return;"))
    (wt-nl "else {")
    (wt-nl "char stack_mark = 0;")
    (wt-nl "mkcl_object thread = env->own_thread;")
    (wt-nl "MKCL_CATCH_ALL_BEGIN(env) {")
    (wt-nl "MKCL_SETUP_CALL_STACK_ROOT_GUARD(env);")

    (wt-nl "mkcl_setup_thread_lisp_context(env, &stack_mark);");
    (wt-nl "mkcl_register_thread_as_active(env, thread);")
    (wt-nl "mkcl_enable_interrupts(env);")

    ;; Here is where the effective callback is called.
    (when return-p (wt-nl "output = "))
    (wt-nl c-name "(")
    (when arg-types
      (let ((comma ""))
	(dotimes (n (length arg-types))
	  (wt comma "var" n)
	  (setf comma ",")))
      )
    (wt-nl ");")

    (wt-nl "mkcl_cleanup_thread_lisp_context(env);")
    (wt-nl "mkcl_disable_interrupts(env);")
    (wt-nl "} MKCL_CATCH_ALL_END;")
    (wt-nl "thread->thread.status = mkcl_thread_done;")
    (wt-nl "mkcl_release_current_thread(env);")
    (wt-nl "}")

    (when return-p (wt-nl "{ errno = 0; return(output); }"))

    (wt-nl1 "}")
    ;; end of thread import wrapper. JCB
    )
  )

(put-sysprop 'FFI:DEFCALLBACK 'C1 #'c1-defcallback)
