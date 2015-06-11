;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: C -*-
;;;;
;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;  Copyright (c) 2001, Juan Jose Garcia Ripoll.
;;;;  Copyright (c) 2010-2013, Jean-Claude Beaudoin.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Lesser General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 3 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../../Copyright' for full details.

;;;; CMPMAIN  Compiler main program.

(in-package "COMPILER")

(defvar *trace-cc* nil)

(defun run-command (string directory &key real-name)
  ;; STRING, DIRECTORY and REAL-NAME must be strings.
  (cmpnote "Doing: ~A" string)
  (when *trace-cc*
    (format t "~&Doing: ~A, in ~S~%" string directory)
    (finish-output))
  (multiple-value-bind (result output)
      (if real-name
	  (mkcl:run-command string directory :real-name real-name)
	(mkcl:run-command string directory))
    (when (and *trace-cc* output)
      (format t "~{~A~}~%" output)
      (finish-output)
      )
    (unless (zerop result)
      (cerror "Continue anyway."
	      "(mkcl:run-command ~S) returned non-zero value ~D~%~
                 Command output:~%~{~A~}~%"
	      string result output))
    (values result output))
  )

(defun compile-file-internal-pathname (name type)
  (let (extension)
    (case type
      (:c (setf extension "c"))
      (:h (setf extension "h"))
      (:object (setf extension +object-file-extension+))
      (:data (setf extension "data"))
      ((:fasl :fas) (setf extension "fas"))
      ;;(:sdata (setf extension "sdat"))
      (t (error "In compile-file-internal-pathname. Do not know how to handler file type: ~S" type))
      )
    #-(or mingw32 mingw64)
    (make-pathname :type extension :defaults name)
    #+(or mingw32 mingw64)
    (make-pathname :type extension :name (si:mangle-string (pathname-name name)) :defaults name)    
    #+(or)
    (multiple-value-bind (stream tmp-name errno)
        (mkcl:mkstemp (make-pathname :type nil :defaults name))
      (unless stream
	(error 'si:OS-file-error
	       :pathname name
	       :format-control "Unable to create temporay file~%~AXXXXXX~%OS Explanation: (errno == ~D) ~S."
	       :format-arguments (list name errno (si::libc-error-string errno)))
	)
      (close stream)
      (rename-file tmp-name (make-pathname :type extension :defaults tmp-name));
      )
    )
  )

(defun builder-internal-pathname (pathspec target-type)
  (unless (pathnamep pathspec) (setq pathspec (pathname pathspec)))
  (let ((name (pathname-name pathspec))
	(type (pathname-type pathspec))
	extension
	format)
    (case target-type
      ((:fasl :fas) (setq format "~a.fas" extension "fas"))
      (:fasb (setq format "~a.fasb" extension "fasb"))
      (:program (setq format +executable-file-format+ extension #+unix "" #+windows "exe"))
      ((:shared-library :dll) (setq format +shared-library-format+ extension +shared-library-extension+))
      ((:static-library :library :lib) (setq format +static-library-format+ extension +static-library-extension+))
      #+msvc
      (:import-library (setq format "~a.implib" extension "implib"))
      (t (error "In builder-internal-pathname. Do not know how to handler file type: ~S" type))
      )
    (if (or #+unix (string= type extension) #+windows (string-equal type extension))
	pathspec
      (merge-pathnames (format nil format name) pathspec))
    )
  )


#+msvc
(defun delete-msvc-generated-files (output-pathname)
  (loop for i in '("lib" "exp" "ilk" "pdb")
        do (let ((the-pathname (merge-pathnames (make-pathname :type i) output-pathname)))
	     (when (mkcl:probe-file-p the-pathname)
	       (cmp-delete-file the-pathname)))))

(defun cmp-delete-file (file)
  (cond ((null *delete-compiler-internal-files*))
	(*debug-compiler*
	 (cmpprogress "~%Postponing deletion of ~A" file)
	 (push file *files-to-be-deleted-on-shutdown*))
	(t
	 (and (mkcl:probe-file-p file)
	      (ignore-errors (delete-file file))))))

(push #'(lambda () (mapc #'(lambda (f) (ignore-errors (delete-file f))) *files-to-be-deleted-on-shutdown*))
      si::*exit-hooks*)


(let* ((bin-dir (make-pathname :name nil :type nil :version nil :defaults (si:self-truename)))
       (inc-dir (merge-pathnames #P"../include/" bin-dir))
       (inc-dir-probe (merge-pathnames #P"mkcl/mkcl.h" inc-dir))
       )
  (defun mkcl-include-directory ()
    "Finds the directory in which the header files were installed."
    (cond (*mkcl-include-directory*)
          ((mkcl:probe-file-p inc-dir-probe) inc-dir)
          (*mkcl-default-include-directory*)
	  ((error "Unable to find include directory")))))

(let* ((bin-dir (make-pathname :name nil :type nil :version nil :defaults (si:self-truename)))
       (lib-dir (merge-pathnames "../lib/" bin-dir))
       (shared-lib-pathname-name (make-pathname :name (mkcl:bstr+ "mkcl_" (si:mkcl-version))))
       (lib-dir-probe
	(merge-pathnames (builder-internal-pathname shared-lib-pathname-name :shared-library) lib-dir))
       )
  (defun mkcl-library-directory ()
    "Finds the directory in which the MKCL core library was installed."
    (cond (*mkcl-library-directory*)
          ((mkcl:probe-file-p lib-dir-probe) lib-dir)
          (*mkcl-default-library-directory*)
	  ((error "Unable to find library directory")))))

(defun libs-ld-flags (libraries mkcl-libraries mkcl-shared external-shared)
  (declare (ignorable mkcl-shared))
  (let ((mkcl-libdir (namestring (mkcl-library-directory)))
	out)

    (dolist (lib-set (si:dyn-list libraries ffi::*referenced-libraries*))
      (dolist (lib-spec lib-set)
	(if (pathnamep lib-spec)
	    (push (mkcl:str+ (namestring lib-spec) " ") out)
	  (let ((lib-spec-as-path (pathname lib-spec)))
	    (if (or (pathname-directory lib-spec-as-path) (pathname-type lib-spec-as-path))
		(push (mkcl:str+ (namestring lib-spec-as-path) " ") out)
	      (push (mkcl:str+ "-l" lib-spec " ") out))))))

    #-mkcl-bootstrap
    (unless mkcl-shared (setq mkcl-libdir (mkcl:bstr+ mkcl-libdir "mkcl-" (si:mkcl-version) "/")))
    (dolist (lib mkcl-libraries)
      (push (mkcl:bstr+ "\"" mkcl-libdir lib "\" ") out)
      )
    (unless external-shared
      (push "-Wl,-Bstatic " out)
      )
    (push *external-ld-flags* out)
    (apply #'concatenate 'base-string (nreverse out))
    )
  )

(defun link-program (out-pathname extra-ld-flags o-files libraries mkcl-shared external-shared &optional (working-directory "."))
  (run-command (format nil
		       *ld-format*
		       *ld*
		       out-pathname
		       (or extra-ld-flags "")
		       o-files
		       *program-ld-flags*
		       (libs-ld-flags libraries (if mkcl-shared *mkcl-shared-libs* *mkcl-static-libs*) mkcl-shared external-shared)
		       )
	       (namestring working-directory)))

(defun link-shared-lib (out-pathname extra-ld-flags o-files libraries mkcl-shared external-shared &optional (working-directory "."))
  (run-command (format nil
		       *ld-format*
		       *ld*
		       (merge-pathnames out-pathname (builder-internal-pathname out-pathname :dll))
		       (or extra-ld-flags "")
		       #+msvc o-files
		       #-msvc (cons "-Wl,--whole-archive" (nconc o-files '("-Wl,--no-whole-archive")))
		       *shared-ld-flags*
		       ;; during bootstrap the only shared lib we will build is MKCL's main lib which cannot depend on itself.
		       (libs-ld-flags libraries
				      (and #+unix nil #+mkcl-bootstrap nil *mkcl-shared-libs*)
				      mkcl-shared external-shared))
	       (namestring working-directory)))

(defun link-fasl (out-pathname init-name extra-ld-flags o-files libraries mkcl-shared external-shared &optional (working-directory "."))
  (declare (ignorable init-name))
  (run-command (format nil
		       *ld-format*
		       *ld*
		       out-pathname
		       (or extra-ld-flags "")
		       o-files
		       *bundle-ld-flags*
		       #-msvc (libs-ld-flags libraries
					     (if mkcl-shared 
						 (and #+unix nil *mkcl-shared-libs*)
					       *mkcl-static-libs*)
					     mkcl-shared external-shared)
		       #+msvc (concatenate 'string
					   " /EXPORT:"
					   init-name
					   " /LIBPATH:"
					   (mkcl-library-directory)
					   " /IMPLIB:"
					   (builder-internal-pathname out-pathname :import-library)))
	       (namestring working-directory)))

(defun preserve-escapes (string)
  (let ((new (prin1-to-string string)))
    (subseq new 1 (1- (length new))))
  )

(defun compiler-cc (c-pathname o-pathname &optional (working-directory "."))
  (run-command (format nil
		       *cc-format*
		       *cc* *cc-flags* (>= (cmp-env-optimization 'speed) 2) *cc-optimize*
		       (preserve-escapes (namestring (mkcl-include-directory)))
		       c-pathname
		       o-pathname
		       )
	       (namestring working-directory)))


(defconstant +lisp-program-full-header+ "~
/* MKCL: Initialization code for file ~A */
/* Date: ~D/~D/~D ~2,'0D:~2,'0D:~2,'0D (yyyy/mm/dd) */
/* Compiler: ~A ~A */
/* Machine: ~A ~A ~A */

#include <mkcl/mkcl.h>
#include <mkcl/internal.h>

#ifdef __cplusplus
#define MKCL_CPP_TAG \"C\"
#else
#define MKCL_CPP_TAG
#endif

~{	extern MKCL_CPP_TAG void ~A(MKCL, mkcl_object, mkcl_object);~%~}

")

(defconstant +lisp-program-fast-header+ "~
/* MKCL: Initialization code for file ~A */
/* Date: ~D/~D/~D ~2,'0D:~2,'0D:~2,'0D (yyyy/mm/dd) */
/* Compiler: ~A ~A */
/* Machine: ~A ~A ~A */

#define mkcl_t_codeblock  (( 35 << 2 ) | 1) /* see mkcl/object.h */

  struct mkcl_env_struct;
#define MKCL struct mkcl_env_struct * const env
  typedef union mkcl_lispunion * mkcl_object;

#define NULL ((void *) 0)
/* #define MKCL_OBJNULL         ((mkcl_object) ~~0x03) */
#if __WIN64__
#define MKCL_REF_P(o) ((((unsigned long long) (o)) & 0x03) == 0)
#else
#define MKCL_REF_P(o) ((((unsigned long) (o)) & 0x03) == 0)
#endif
#define mk_cl_Cnil ((mkcl_object) NULL)

typedef unsigned char uint8_t;
typedef signed char int8_t;
#define MKCL_HEADER2(field1,field2)	uint8_t t, m; int8_t field1, field2

  /* This duplicate structure definition must match the official version in mkcl/object.h */
  struct mkcl_codeblock {
    MKCL_HEADER2(self_destruct,locked);
    mkcl_object next;
    const char *data_text;
    int	data_text_size;
    mkcl_object *data;
    int	data_size;
    mkcl_object *temp_data;
    int	temp_data_size;
    void *handle;
    void (*entry)(MKCL, mkcl_object, mkcl_object);
    void * __pad[9];
  };

  union mkcl_lispunion {
    struct mkcl_codeblock   cblock;
  };

  extern /* MKCL_API */ mkcl_object mkcl_read_VV(MKCL, mkcl_object, void (*)(MKCL, mkcl_object, mkcl_object), mkcl_object);
  extern /* MKCL_API */ void mkcl_FEnot_codeblock_type(MKCL, mkcl_object value);

#ifdef __cplusplus
#define MKCL_CPP_TAG \"C\"
#else
#define MKCL_CPP_TAG
#endif

~{	extern MKCL_CPP_TAG void ~A(MKCL, mkcl_object, mkcl_object);~%~}

")



(defconstant +lisp-program-init-export+ "
#ifdef __cplusplus
extern \"C\"
#else
extern
#endif
__declspec(dllexport) void ~A(MKCL, mkcl_object cblock, mkcl_object fasl_filename);
~%")

;;
;; This format string contains the structure of the code that initializes
;; a program, a library, a module, etc. Basically, it processes a codeblock
;; just like in a normal compiled file, but then adds all the codeblocks of
;; its corresponding modules.
;;
;; IMPORTANT: Notice how the modules are linked to the parent forming a
;; circular chain. This disables the garbage collection of the library until
;; _ALL_ functions in all modules are unlinked.
;;
(defconstant +lisp-program-init+ "
#ifdef __cplusplus
extern \"C\"
#endif
void ~A(MKCL, mkcl_object cblock, mkcl_object fasl_filename)
{
	static mkcl_object Cblock = mk_cl_Cnil; /* root for GC. */
        if (cblock != mk_cl_Cnil) {
                if (MKCL_REF_P(cblock)) {
                        if (cblock->cblock.t != mkcl_t_codeblock)
                                mkcl_FEnot_codeblock_type(env, cblock);
		        Cblock = cblock;
		        cblock->cblock.data_text = compiler_data_text;
		        cblock->cblock.data_text_size = compiler_data_text_size;
		        cblock->cblock.data_size = VM;
		        return;
	        } else mkcl_FEnot_codeblock_type(env, cblock);
        }
        if ((Cblock == mk_cl_Cnil) || !MKCL_REF_P(Cblock) || (Cblock->cblock.t != mkcl_t_codeblock))
                mkcl_FEnot_codeblock_type(env, Cblock);

	~A
   {
	mkcl_object current, next = Cblock;
  ~{	current = mkcl_read_VV(env, mk_cl_Cnil, ~A, fasl_filename);
        current->cblock.next = next; next = current; ~%~}
	Cblock->cblock.next = current;
   }
	~A
}~%")

(defconstant +lisp-program-main+ "
int
main(int argc, char **argv)
{
  ~A

  struct mkcl_thread_init_parameters params = { 0 };

  const mkcl_env env = mkcl_boot(argc, argv, &params);

  if (env == NULL)
    return(errno); /* boot failed */
  else
    {
      /* mk_si_register_shutdown_watchdog_thread(env, env->own_thread, mk_cl_Ct); */ /* already done by mkcl_boot() */

      MKCL_CATCH_ALL_BEGIN(env) {
	MKCL_SETUP_CALL_STACK_ROOT_GUARD(env);
        mkcl_enable_interrupts(env);
        mkcl_read_VV(env, mk_cl_Cnil, ~A, mk_cl_Cnil);
        ~A

      } MKCL_CATCH_ALL_IF_CAUGHT {
        /* watchdog here? */
        /* return mkcl_exit_status(env); */
      } MKCL_CATCH_ALL_END;

      env->own_thread->thread.status = mkcl_thread_done;
      /* MKCL's shutdown watchdog should be inserted here. */
      return mkcl_shutdown_watchdog(env);
    }
}~%")

#+windows
(defconstant +lisp-program-winmain+ "
#include <windows.h>
int WINAPI
WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, LPSTR lpCmdLine, int nCmdShow)
{ 
  char **argv;
  int argc;
  struct mkcl_thread_init_parameters params = { 0 };

  ~A
  mkcl_get_commandline_args_from_Windows(&argc, &argv);

  const mkcl_env env = mkcl_boot(argc, argv, &params);

  if (env == NULL)
    return(errno); /* boot failed */
  else
    {
      /* mk_si_register_shutdown_watchdog_thread(env, env->own_thread, mk_cl_Ct); */ /* already done by mkcl_boot() */
      MKCL_CATCH_ALL_BEGIN(env) {
	MKCL_SETUP_CALL_STACK_ROOT_GUARD(env);
        mkcl_enable_interrupts(env);
        mkcl_read_VV(env, mk_cl_Cnil, ~A, mk_cl_Cnil);
        ~A
      } MKCL_CATCH_ALL_IF_CAUGHT {
        /* watchdog here? */
        /* return mkcl_exit_status(env); */
      } MKCL_CATCH_ALL_END;
      env->own_thread->thread.status = mkcl_thread_done;
      return mkcl_shutdown_watchdog(env);
    }
}~%")

(defun build-full-epilogue (epilogue-code program-p &aux (epilogue-p t))
  (cond ((null epilogue-code)
	 (setq epilogue-p nil epilogue-code "")
	 )
	((stringp epilogue-code)
	 )
	(t
	 (with-standard-io-syntax
	  (setq epilogue-code
		(with-output-to-string (stream)
		  (princ "{" stream) (terpri stream)
		  (princ "          static char lisp_code[] = " stream)
		  (wt-filtered-data #-unicode (write-to-string epilogue-code)
				    #+unicode (mkcl:write-to-base-string epilogue-code :encoding :utf-8)
				    stream :one-liner)
		  (princ ";
          static const mkcl_base_string_object(lisp_code_bstr_obj, lisp_code);
          static const mkcl_base_string_object(safe_eval_bstr_obj, \"si::safe-eval\");
          static const mkcl_base_string_object(cl_user_bstr_obj, \"CL-USER\");
          mkcl_object output = mk_cl_Cnil;

          mk_si_select_package(env, (mkcl_object) &cl_user_bstr_obj);
          output = mk_si_top_apply(env,
                                   mk_si_fast_read_from_base_string(env, (mkcl_object) &safe_eval_bstr_obj),
                                   mk_cl_list(env, 3,
                                              mk_si_fast_read_from_base_string(env, (mkcl_object) &lisp_code_bstr_obj),
                                              mk_cl_Cnil,
                                              MKCL_MAKE_FIXNUM(MKCL_THREAD_UNKNOWN_ERROR)));
"
			 stream)
		  (when program-p
		    (princ "
          env->own_thread->thread.result_value = output;
" 
			   stream))
		  (princ "        }" stream))))))
  (values epilogue-code epilogue-p)
  )

(defun guess-object-file-kind (pathname)
  "Given a file name, guess whether it is an object file, a library, a program
or a loadable module."
  (let ((record (assoc (pathname-type pathname)
		       '(("o" :object)
			 #+windows ("obj" :object)
			 ;;("c" :c)
			 #+windows ("lib" :static-library)
			 ("a" :static-library)
			 #+windows ("dll" :shared-library)
			 ("so" :shared-library)
			 ("fas" :fasl)
			 ("fasb" :fasb))
		       :test #'string-equal)))
    (if record
	(second record)
      (progn
	(warn "File ~s is not a known object file type. Assuming it is an basic object file." pathname)
	:object))))


(defun map-symbol-to-library (library)
  "Given a symbol, try to find a library that matches it, either by looking in the
filesystem or in the database of ASDF modules."
  (let ((asdf (find-package "ASDF"))
        system)
    (labels ((asdfsym (x) (find-symbol (string x) asdf))
             (asdfcall (fun &rest rest) (apply (asdfsym fun) rest))
             (system-output (system type)
               (let ((build (make-instance (asdfsym :build-op) :type type)))
                 (first (asdfcall :output-files build system))))
             (existing-system-output (system type)
               (let ((o (system-output system type)))
                 (and o (setf o (probe-file o)) (namestring o))))
             (find-archive (system)
                 (or (existing-system-output system :library)
                     (existing-system-output system :shared-library)))
             (fallback () (format nil #-msvc +static-library-format+ #+msvc "~A.lib" (string-downcase library))))
      (or (and asdf
               (setf system (asdfcall :find-system library nil))
               (find-archive system))
        (fallback)))))

(defvar *builder-to-delete*)

(defun filename-is-ascii-only-p (filespec)
  (let ((filename (namestring filespec)))
    (dotimes (i (length filename) T)
      ;; break out on anything not ASCII. JCB
      (when (<= 128 (char-code (char filename i))) (return-from filename-is-ascii-only-p nil)))))

(defun ensure-palatable-to-linker (filespec)
  #+(or mingw32 mingw64)
  (let ((template #P"TMP:MKBUILD"))
    (when (filename-is-ascii-only-p filespec) (return-from ensure-palatable-to-linker filespec))
    (multiple-value-bind (stream name errno)
	(mkcl:mkstemp template)
      (unless stream
	(error 'si:OS-file-error
	       :pathname template
	       :format-control "Unable to create temporay file~%~AXXXXXX~%OS Explanation: (errno == ~D) ~S."
	       :format-arguments (list template errno (si::libc-error-string errno)))
	(return-from ensure-palatable-to-linker nil)
	)
      ;;(format t "~&;;MKCL: In ensure-palatable-to-linker: Had to substitute file ~S for ~S.~%" name filespec) ;; debug JCB.
      (when (mkcl:probe-file-p filespec) (mkcl:copy-file filespec stream))
      (close stream)
      (setq name (make-pathname :type (pathname-type filespec) :defaults name))
      (rename-file stream name)
      (push name *builder-to-delete*)
      name
      )
    )
  #-(or mingw32 mingw64)
  filespec
  )

(defun collect-submodule-initializers (lisp-object-files object-files &aux submodule-inits)
  (dolist (item (reverse lisp-object-files))
    (etypecase item
      (symbol
       (push (ensure-palatable-to-linker (mkcl:physically-complete-pathname (map-symbol-to-library item))) object-files)
       (push (init-function-name item :kind :lib) submodule-inits))
      ((or string pathname)
       (let* ((o-path (ensure-palatable-to-linker (mkcl:physically-complete-pathname item))) ;; make item physical and absolute path
	      (kind (guess-object-file-kind o-path)))
	 (unless (member kind '(:object :static-library :lib :shared-library :dll))
	   (error "COMPILER::BUILDER does not accept a file ~s of kind ~s" item kind))
	 (let ((init-fn (guess-init-name o-path))
	       #+(or)(o-file (validate-object-file o-path)))
	     (push o-path object-files)
	     (push init-fn submodule-inits))))))
  (values submodule-inits object-files)
  )

(defun build-init-c-file-header (header-style c-file output-name submodules)
  (let ((template (if (eq header-style :fast) +lisp-program-fast-header+ +lisp-program-full-header+)))
    (multiple-value-bind (second minute hour day month year)
        (get-decoded-time)
      (format c-file template
	      output-name
	      year month day hour minute second
	      (lisp-implementation-type) (lisp-implementation-version)
	      (software-type) (software-version) (machine-type)
	      submodules))
    (format c-file "~@
                    #define compiler_data_text NULL~@
                    #define compiler_data_text_size 0~@
                    static mkcl_object * VV = NULL;~@
                    #define VM 0~%"
		   c-file)))

(defvar *builder-default-libraries* nil)

(defun builder (target output-name
		       &key
		       lisp-object-files
		       object-files
		       extra-ld-flags
		       (init-name nil)
		       (libraries *builder-default-libraries*) ;; a list of strings, each naming a library
		       (use-mkcl-shared-libraries t)
		       (use-external-shared-libraries t)
		       #+windows (subsystem :console) ;; only for :program target on :windows
		       (prologue-code "" prologue-p)
		       (epilogue-code (when (and (eq target :program) #+windows (eq subsystem :console)) '(SI::TOP-LEVEL)))
		       &aux
		       (*builder-to-delete* nil)
		       output-internal-name
		       (epilogue-p t)
		       (mkcl:*current-working-directory* (or mkcl:*current-working-directory* (mkcl:getcwd)))
		       (cwd mkcl:*current-working-directory*)
		       (*suppress-compiler-messages* (or *suppress-compiler-messages*
							 (not *compile-verbose*))))

;;(format t "~&In compiler::builder: target= ~S, output-name= ~S, lisp-object-files= ~S, object-files= ~S, extra-ld-flags= ~S.~%" target output-name lisp-object-files object-files extra-ld-flags)

  (when *suppress-compiler-notes*
    (setf *suppress-compiler-messages*
	  `(or ,*suppress-compiler-messages* compiler-note)))
  (when *suppress-compiler-warnings*
    (setf *suppress-compiler-messages*
	  `(or ,*suppress-compiler-messages* compiler-warning)))

  ;;
  ;; The epilogue-code can be either a string made of C code, or a
  ;; lisp form.  In the latter case we add some additional C code to
  ;; clean up, and the lisp form is stored in a text representation,
  ;; to avoid using the compiler.
  ;;
  (multiple-value-setq (epilogue-code epilogue-p) (build-full-epilogue epilogue-code (eq target :program)))
  (when (null prologue-code) (setq prologue-code "" prologue-p nil))
  (unless (stringp prologue-code)
    (error ";;; MKCL In compiler::builder, Keyword argument :prologue-code is not a string, invalid value is: ~S" prologue-code))

  ;;
  ;; When a module is built out of several object files, we have to
  ;; create an additional object file that initializes those ones.
  ;; This routine is responsible for creating this file.
  ;;
  ;; To avoid name clashes, this object file will have a temporary
  ;; file name (tmp-name).
  ;;
  (if (or (pathnamep output-name) (stringp output-name) (subtypep output-name 'file-stream))
      (setf output-name (builder-internal-pathname output-name target)) 
    (error ";;; MKCL: In compiler::builder, argument 'output-name' is not a valid pathname specifier: ~S." output-name))
  (setq output-name (mkcl:physically-complete-pathname output-name)) ;; make output-name absolute path
  (setq output-internal-name (ensure-palatable-to-linker output-name))

  (unless init-name (setq init-name (compute-init-name output-name :kind target)))

  (let* ((template "MKCLINIT")
	 tmp-name
	 (tmp-file (multiple-value-bind (stream name errno)
		       (mkcl:mkstemp (make-pathname :name template :type nil :defaults output-internal-name) :external-format :utf-8)
		     (unless stream
		       (error 'si:OS-file-error
			      :pathname template
			      :format-control "Unable to create temporay file~%~AXXXXXX~%OS Explanation: (errno == ~D) ~S."
			      :format-arguments (list template errno (si::libc-error-string errno)))
		       (return-from builder nil)
		       )
		     (setq tmp-name name)
		     stream
		     )
		   )
	 (c-pathname (compile-file-internal-pathname tmp-name :c))
	 (o-pathname (compile-file-internal-pathname tmp-name :object))
	 (c-basename (file-namestring c-pathname))
	 (o-basename (file-namestring o-pathname))
	 (work-dir (mkcl:full-directory-namestring output-internal-name))
	 (c-file tmp-file)
	 submodules
	 )

    (push tmp-name *builder-to-delete*)
    (push c-pathname *builder-to-delete*)
    (push o-pathname *builder-to-delete*)
    (unwind-protect
	(progn
	  (let (new-object-files)
	    (dolist (o-file object-files (setq object-files (nreverse new-object-files)))
	      (push (merge-pathnames o-file cwd) new-object-files)))

	  (multiple-value-setq (submodules object-files)
	    (collect-submodule-initializers lisp-object-files object-files))
	  
	  (if (or (eq target :program) prologue-p epilogue-p) 
	      (build-init-c-file-header :full c-file output-internal-name submodules)
	    (build-init-c-file-header :fast c-file output-internal-name submodules))
	  
	  (ecase target
	    (:program
	     (format c-file +lisp-program-init+ init-name "" submodules "")
	     (format c-file #+windows (ecase subsystem
                                             (:console +lisp-program-main+)
					     (:windows +lisp-program-winmain+))
		     #-windows +lisp-program-main+
		     prologue-code init-name epilogue-code)
	     (close c-file)
	     (rename-file c-file c-pathname)
	     (compiler-cc c-basename o-basename work-dir)
	     #+(or mingw32 mingw64)
	     (ecase subsystem
		    (:console (push "-mconsole" object-files))
		    (:windows (push "-mwindows" object-files)))
	     (link-program output-internal-name extra-ld-flags (cons (namestring o-pathname) object-files)
			   libraries use-mkcl-shared-libraries use-external-shared-libraries cwd))
	    ((:static-library :library :lib)
	     (let ((output-filename output-internal-name))
	       (format c-file +lisp-program-init+ init-name prologue-code submodules epilogue-code)
	       (close c-file)
	       (rename-file c-file c-pathname)
	       (compiler-cc c-basename o-basename work-dir)
	       (when (mkcl:probe-file-p output-filename) (delete-file output-filename))
	       #-msvc
	       (progn
		 (run-command (format nil "\"~A\" cr \"~A\" \"~A\" ~{\"~A\" ~}" *ar* output-filename o-pathname object-files) work-dir)
		 (run-command (format nil "\"~A\" \"~A\"" *ranlib* output-filename) work-dir))
	       #+msvc
	       (unwind-protect
		   (progn
		     (with-open-file (f "static_lib.tmp" :direction :output :if-does-not-exist :create :if-exists :supersede)
				     (format f "/DEBUGTYPE:CV /OUT:~A ~A ~{~&\"~A\"~}"
					     output-filename o-pathname object-files))
		     (run-command "link -lib @static_lib.tmp" work-dir))
		 (when (mkcl:probe-file-p "static_lib.tmp")
		   (cmp-delete-file "static_lib.tmp")))
	       )
	     )
	    ((:shared-library :dll)
	     #+windows (format c-file +lisp-program-init-export+ init-name)
	     (format c-file +lisp-program-init+ init-name prologue-code submodules epilogue-code)
	     (close c-file)
	     (rename-file c-file c-pathname)
	     (compiler-cc c-basename o-basename work-dir)
	     (link-shared-lib output-internal-name extra-ld-flags (cons o-pathname object-files)
			      libraries use-mkcl-shared-libraries use-external-shared-libraries cwd))
	    ((:fasl :fasb)
	     #+windows (format c-file +lisp-program-init-export+ init-name)
	     (format c-file +lisp-program-init+ init-name prologue-code submodules epilogue-code)
	     (close c-file)
	     (rename-file c-file c-pathname)
	     (compiler-cc c-basename o-basename work-dir)
	     (link-fasl output-internal-name init-name extra-ld-flags (cons o-pathname object-files)
			libraries use-mkcl-shared-libraries use-external-shared-libraries cwd))
	    )

	  (unless (equal output-name output-internal-name)
	    ;;(format t "~&MKCL;; builder had to rename its output from ~A to ~A.~%" output-internal-name output-name) (finish-output)
	    (rename-file output-internal-name output-name))
	  )
      ;; start of unwind-protected region. Cleaning up our workspace
      (close c-file) ;; in case it wasn't done already.
      (mapc #'cmp-delete-file *builder-to-delete*)
      )
    )
  output-name
  )

(defun build-fasl (&rest args)
  (declare (dynamic-extent args))
  (handler-bind (((and condition (not style-warning))
		  #'(lambda (condition)
		      (format t "~&build-fasl failed: ~A~%" condition)
		      (when *compiler-break-enable*
			(invoke-debugger condition))
		      (return-from build-fasl))))
   (apply #'builder :fasl args)))

(defun build-bundle (&rest args)
  (declare (dynamic-extent args))
  (handler-bind (((and condition (not style-warning))
		  #'(lambda (condition)
		      (format t "~&build-bundle failed: ~A~%" condition)
		      (when *compiler-break-enable*
			(invoke-debugger condition))
		      (return-from build-bundle))))
   (apply #'builder :fasb args)))

(defun build-program (&rest args)
  (declare (dynamic-extent args))
  (handler-bind (((and condition (not style-warning))
		  #'(lambda (condition)
		      (format t "~&build-program failed: ~A~%" condition)
		      (when *compiler-break-enable*
			(invoke-debugger condition))
		      (return-from build-program))))
   (apply #'builder :program args)))

(defun build-static-library (&rest args)
  (declare (dynamic-extent args))
  (handler-bind (((and condition (not style-warning))
		  #'(lambda (condition)
		      (format t "~&build-static-library failed: ~A~%" condition)
		      (when *compiler-break-enable*
			(invoke-debugger condition))
		      (return-from build-static-library))))
   (apply #'builder :static-library args)))

(defun build-shared-library (&rest args)
  (declare (dynamic-extent args))
  (handler-bind (((and condition (not style-warning))
		  #'(lambda (condition)
		      (format t "~&build-shared-library failed: ~A~%" condition)
		      (when *compiler-break-enable*
			(invoke-debugger condition)
			)
		      (return-from build-shared-library))))
   (apply #'builder :shared-library args)))


(defvar *debug-stream-decoding-error* nil)

(defun read-safely (input eof)
  (handler-bind ((mkcl:stream-decoding-error
		  #'(lambda (condition)
		      (if *debug-stream-decoding-error*
			  (invoke-debugger condition)
			(progn
			  (cmpwarn "~A Using default replacement character instead. ~
                                    You should consider another :external-format." condition)
			  (continue)
			  )
			)
		      ))
		 (condition
		  #'(lambda (condition)
		      (format t "~&;;; MKCL Compiler Read Error: in file ~S, near position ~S,~@
                                   ;;; MKCL Compiler Read Error: ~A~%"
			      (pathname input) (mkcl::file-character-position input) condition)
		      (handle-warning/error condition)
		      nil))
		 )
    (si::read-object-or-ignore input eof)))


(defun compiler-pass2 (c-pathname h-pathname data-pathname init-name &key input-designator)
  (with-open-file (*compiler-output1* c-pathname :direction :output :external-format :utf-8) ;; JCB
    (wt-comment-nl "Compiler: ~A ~A" (lisp-implementation-type) (lisp-implementation-version))
    (multiple-value-bind (second minute hour day month year)
        (get-decoded-time)
      (wt-comment-nl "Date: ~D/~D/~D ~2,'0D:~2,'0D:~2,'0D (yyyy/mm/dd)" year month day hour minute second)
      (wt-comment-nl "Machine: ~A ~A ~A" (software-type) (software-version) (machine-type)))
    (wt-comment-nl "Source: ~A" input-designator)
    (wt-comment-nl "This file external format: ~A" (stream-external-format *compiler-output1*)) ;; JCB debug
    (with-open-file (*compiler-output2* h-pathname :direction :output :external-format :utf-8) ;; JCB
      (wt-nl1 "#include " *cmpinclude*)
      (catch *cmperr-tag*
	(ctop-write init-name h-pathname data-pathname))
      (terpri *compiler-output1*)
      (terpri *compiler-output2*))))


(defun compiler-output-values (main-value conditions)
  (loop for i in conditions
     with style-warning-p = nil
     with failure-p = nil
     do (cond ((typep i 'style-warning)
	       (setf style-warning-p t))
	      ((typep i 'warning)
	       (setf failure-p t))
	      ((typep i 'error)
	       (setf failure-p t))
	      ((eq i :aborted)
	       (setf failure-p t))
	      )
     finally (return (values (and #|(not failure-p)|# main-value) (or style-warning-p failure-p) failure-p))))


(defvar *c-file* nil) ;; default value of the :c-file argument to compile-file
(defvar *h-file* nil) ;; default value of the :h-file argument to compile-file
(defvar *data-file* nil) ;; default value of the :data-file argument to compile-file
  

(si:reopen-package :cl)

(defun cl:compile-file-pathname (name &key (output-file T) verbose print external-format ;; standard args
				      c-file h-file data-file (fasl-p t) ;; compile-file extension args
				      )
  (declare (ignore verbose print external-format c-file h-file data-file))
  (if (or (eq output-file T) (eq output-file nil))
      (make-pathname :type (if fasl-p "fas" +object-file-extension+) :defaults name)
    (pathname output-file))
  )

;;(defvar *trace-compiler-memory* nil)
(defvar *compile-default-libraries* nil)
  
(defun cl:compile-file (input-pathname 
			&key
			;; standard keyword arguments
			output-file
			((:verbose *compile-verbose*) *compile-verbose*)
			((:print *compile-print*) *compile-print*)
			(external-format :default)
			;; extension arguments
			(c-file *c-file*)
			(h-file *h-file*)
			(data-file *data-file*)
			(fasl-p t)
			(libraries *compile-default-libraries*) ;; a list of strings, each naming a foreign library
			&aux
			(*standard-output* *standard-output*)
			(*error-output* *error-output*)
			(*compiler-in-use* *compiler-in-use*)
			(*package* *package*)
			(*print-pretty* nil)
			(*compile-file-pathname* nil)
			(*compile-file-truename* nil)
			(*suppress-compiler-messages*
			 (or *suppress-compiler-messages* (not *compile-verbose*)))
			(mkcl:*current-working-directory* (or mkcl:*current-working-directory* (mkcl:getcwd)))
			(cwd mkcl:*current-working-directory*)
			input-file
			(original-output-file output-file)
			init-name
			(*disassemble-bindings* nil) ;; turn disassemble redirection off.
			(si:*compiler-constants* nil))
  ;;(declare (ignore args))
  "Compiles the file specified by INPUT-PATHNAME and generates a compiled file.
The produced compiled file is in FASL format if :FASL-P is true (the default),
otherwise its format is the native system binary object file format. 
If the filetype is not specified in INPUT-PATHNAME, then \".lsp\" is used
as the default file type for the source file.
The :OUTPUT-FILE, :C-FILE, :H-FILE, and :DATA-FILE keyword parameters allow you to
control the intermediate files generated by the MKCL compiler. If the file was
compiled successfully, returns the pathname of the compiled file."

  ;; (when *trace-compiler-memory*
  ;;   (si:reset-allocation-statistics)
  ;;   (format t "~&compile-file start: ~S.~%" (si:sample-allocation-statistics)))

  (when *suppress-compiler-notes*
    (setf *suppress-compiler-messages*
	  `(or ,*suppress-compiler-messages* compiler-note)))
  (when *suppress-compiler-warnings*
    (setf *suppress-compiler-messages*
	  `(or ,*suppress-compiler-messages* compiler-warning)))

  ;; pick up *default-pathname-defaults* as per standard
  (let ((input-real-path (mkcl:physically-complete-pathname (merge-pathnames input-pathname))))
    (unless (mkcl:probe-file-p input-real-path)
      (if (pathname-type input-real-path)
	  (error 'file-error :pathname input-pathname)
	(dolist (ext '("lsp" "LSP" "lisp" "LISP") (error 'file-error :pathname input-pathname))
	  (let ((a-path (make-pathname :type ext :defaults input-real-path)))
	    (when (mkcl:probe-file-p a-path)
	      (setq input-real-path a-path)
	      (return))))))
    (setq input-file (truename input-real-path)
	  *compile-file-pathname* input-real-path
	  *compile-file-truename* input-file))

  ;; the compiler needs absolute pathnames.
  (when (and output-file (not (eq output-file T)))
    (setq output-file (mkcl:physically-complete-pathname output-file))
    )
  
  (cmpprogress "~&;;; Compiling ~a." (namestring input-pathname))

  (let* ((eof '(NIL))
	 (*compiler-in-use* *compiler-in-use*)
	 (*load-time-values* nil) ;; Load time values are compiled
         (output-file (compile-file-pathname input-file :output-file output-file :fasl-p fasl-p))
	 (tool-wd (mkcl:full-directory-pathname output-file))
         (c-pathname (if (or (eq c-file T) (eq c-file nil))
			 (compile-file-internal-pathname output-file :c)
		       (mkcl:absolute-pathname c-file cwd)))
         (h-pathname (if (or (eq h-file T) (eq h-file nil))
			 (compile-file-internal-pathname output-file :h)
		       (mkcl:absolute-pathname h-file cwd)))
         (data-pathname (if (or (eq data-file T) (eq data-file nil))
			    (compile-file-internal-pathname output-file :data)
			  (mkcl:absolute-pathname data-file cwd)))
	 (compiler-conditions nil)
         (to-delete (nconc (unless c-file (list c-pathname))
                           (unless h-file (list h-pathname))
                           (unless data-file (list data-pathname)))))

    (unwind-protect
	(with-compiler-env
	 (compiler-conditions)

	 ;;(when *trace-compiler-memory* (format t "~&compile-file before read: ~S.~%" (si:sample-allocation-statistics)))

	 (data-init)

	 ;; Read in the source code.
	 (with-open-file 
	  (*compiler-input* input-file #|:direction :input|# :external-format external-format)
	  (do* ((si:*source-location* (cons *compile-file-pathname* 0))
		(form (read-safely *compiler-input* eof) (read-safely *compiler-input* eof))
		(*compile-file-end-position* (mkcl::file-character-position *compiler-input*)
                                             (mkcl::file-character-position *compiler-input*))
		)
	       ((eq form eof))
	       (when form
		 (setf (cdr si:*source-location*) *compile-file-end-position*)
		 (t1expr form))))

	 ;;(when *trace-compiler-memory* (format t "~&compile-file after read: ~S.~%" (si:sample-allocation-statistics)))

	 ;; Generate intermediate C code.
	 (progn
	   (setf init-name (compute-init-name output-file :kind (if fasl-p :fasl :object)))
	   (compiler-pass2 c-pathname h-pathname data-pathname init-name :input-designator (namestring input-pathname))
	   (data-dump data-pathname)
	   )

	 ;;(when *trace-compiler-memory* (format t "~&compile-file before CC: ~S.~%" (si:sample-allocation-statistics)))

	 ;; Emit final object code.
	 (let* ((o-pathname (compile-file-internal-pathname output-file :object))
		(o-basename (file-namestring o-pathname))
		tmp-output)
	   (compiler-cc (mkcl:file-pathname c-pathname) o-basename tool-wd)
	   (setq tmp-output o-pathname)
	   (when fasl-p
	     (push o-pathname to-delete)
	     (setq tmp-output (compile-file-internal-pathname output-file :fasl))
	     (link-fasl (mkcl:file-pathname tmp-output) init-name "" (list o-basename) libraries t t tool-wd))
	   (unless (equal output-file tmp-output)
	     ;;(format t "~&MKCL;; compiler had to rename its output from ~A to ~A.~%" tmp-output output-file) (finish-output)
	     (rename-file tmp-output output-file))
	   )
	 
	 ;;(when *trace-compiler-memory* (format t "~&compile-file after CC: ~S.~%" (si:sample-allocation-statistics)))
	 
	 (if (setq output-file (probe-file output-file)) ;; obtain truename or nil
	     (cmpprogress "~&;;; Finished compiling ~a.~%" (namestring input-pathname))
	   (cmperr "The C compiler failed to compile the intermediate file."))
	 
	 ) ;; with-compiler-env
      
      ;; Start of unwind-protected region: Clean-up our working space.
      (mapc #'cmp-delete-file to-delete)
      )

    ;;(when *trace-compiler-memory* (format t "~&compile-file end: ~S.~%" (si:sample-allocation-statistics)))

    (compiler-output-values output-file compiler-conditions)
    )
  )

(defun cl:compile (name &optional (definition nil definition-supplied-p)
			&key (libraries *compile-default-libraries*) ;; a list of strings, each naming a foreign library
			&aux 
			form
			data-pathname
			data-file
			GAZONK-name
			(*suppress-compiler-messages* (or *suppress-compiler-messages* (not *compile-verbose*)))
			(*compiler-in-use* *compiler-in-use*)
			(*standard-output* *standard-output*)
			(*error-output* *error-output*)
			(*package* *package*)
			(*compile-print* nil)
			(*print-pretty* nil)
			(*disassemble-bindings* nil) ;; turn disassemble redirection off.
			(si:*compiler-constants* t))
  "Args: (name &optional definition)

If DEFINITION is NIL, NAME must be the name of a not-yet-compiled function.
In this case, COMPILE compiles the function, installs the compiled function as
the global function definition of NAME, and returns NAME.  If DEFINITION is
non-NIL, it must be a lambda expression and NAME must be a valid function name.
COMPILE compiles the lambda expression, installs the compiled function as the
function definition of NAME, and returns NAME.  There is only one exception for
this: If NAME is NIL, then the compiled function is not installed but is simply
returned as the value of COMPILE."

  (unless (si::valid-function-name-p name)
    (error "In #'compile, required argument NAME has value ~s, and is not a valid function name." name))

  (when *suppress-compiler-notes*
    (setf *suppress-compiler-messages*
	  `(or ,*suppress-compiler-messages* compiler-note)))
  (when *suppress-compiler-warnings*
    (setf *suppress-compiler-messages*
	  `(or ,*suppress-compiler-messages* compiler-warning)))

  (cond ((and definition-supplied-p definition)
	 (when (functionp definition)
	   (unless (function-lambda-expression definition)
	     (return-from compile (values definition nil nil)))
	   (setf definition (function-lambda-expression definition)))
         (setq form (if name
                        ;;`(si:fset ',name #',definition)
                        `(setf (fdefinition ',name) #',definition)
                        `(set ',(setq GAZONK-name (gensym)) #',definition))))
	((not (fboundp name))
	 (error "Function name ~s is unbound." name))
	((null (setq form (function-lambda-expression (setq definition (fdefinition name)))))
	 (warn "We have lost the original function definition for ~s. ~
                Compilation to C failed" name)
	 (return-from compile (values definition t nil)))
	(t
	 (setq form `(setf (fdefinition ',name) #',form))))

  (let* ((template #P"TMP:MKCL")
	 errno)
    (multiple-value-setq (data-file data-pathname errno) (mkcl:mkstemp template :external-format :utf-8))
    (unless data-file
      (error 'si:OS-file-error
	     :pathname template
	     :format-control "Unable to create temporay file~%~AXXXXXX~%OS Explanation: (errno == ~D) ~S."
	     :format-arguments (list template errno (si::libc-error-string errno)))
      (return-from compile (values nil t t))))

  (let* ((*load-time-values* 'values) ;; Only the value is kept
	 (c-pathname (compile-file-internal-pathname data-pathname :c))
	 (h-pathname (compile-file-internal-pathname data-pathname :h))
	 (o-pathname (compile-file-internal-pathname data-pathname :object))
	 (so-pathname (compile-file-internal-pathname data-pathname :fasl))
	 (tool-wd (mkcl:full-directory-pathname data-pathname))
	 (init-name (compute-init-name so-pathname :kind :fasl))
	 (compiler-conditions nil)
	 new-function)

    (unwind-protect
	(with-compiler-env
	 (compiler-conditions)
	 (data-init)
	 (t1expr form)
	 (let (#+(or mingw32 mingw64 msvc cygwin)(*self-destructing-fasl* t))
	   (compiler-pass2 c-pathname h-pathname data-pathname init-name :input-designator (format nil "~A" definition)))
	 (setf si:*compiler-constants* (data-dump data-file #|data-pathname|# :close-when-done t))

	 (compiler-cc (file-namestring c-pathname) (file-namestring o-pathname) tool-wd)
	 (link-fasl (file-namestring so-pathname) init-name "" (list (file-namestring o-pathname)) libraries t t tool-wd)
	 
	 (cond ((mkcl:probe-file-p so-pathname)
		(load so-pathname :verbose nil)
		(setf new-function (or name (symbol-value GAZONK-name)))
		)
	       (t (cmperr "The C compiler failed to compile the intermediate code for ~s." name)))
	 ) ; with-compiler-env

      (cmp-delete-file c-pathname)
      (cmp-delete-file h-pathname)
      (cmp-delete-file o-pathname)
      (cmp-delete-file data-pathname)
      ;;#-(or mingw32 msvc cygwin)
      (cmp-delete-file so-pathname)
      #+msvc (delete-msvc-generated-files so-pathname)
      )
    (compiler-output-values new-function compiler-conditions)
    )
  )


(defun cl:disassemble (thing &key (h-file nil) (data-file nil)
			     &aux
			     ;;def ;; any use? JCB
			     disassembled-form
			     (mkcl:*current-working-directory* (or mkcl:*current-working-directory* (mkcl:getcwd)))
			     (cwd mkcl:*current-working-directory*)
			     (*compiler-in-use* *compiler-in-use*)
			     (*print-pretty* nil)
			     (si:*compiler-constants* nil))
  "Compiles the form specified by THING and prints the intermediate C language
code for that form.  But does not install the result of compilation.  If THING
is NIL, then the previously DISASSEMBLEd form is re-DISASSEMBLEd.  If THING is
a symbol that names a function not yet compiled, the function definition is
disassembled.  If THING is a lambda expression, it is disassembled as a
function definition.  Otherwise, THING itself is disassembled as a top-level
form.  H-FILE and DATA-FILE specify intermediate files to build a fasl file
from the C language code.  NIL means \"do not create the file\"."
  
  (when h-file (setq h-file (merge-pathnames h-file cwd)))
  (when data-file (setq data-file (merge-pathnames data-file cwd)))
  (when (si::valid-function-name-p thing)
    (setq thing (fdefinition thing)))
  (cond ((null thing))
	((functionp thing)
	 (unless (si::bc-disassemble thing)
	   (warn "Cannot disassemble the binary function ~S because I do not have its source code." thing)
	   (return-from disassemble nil))
	 ;;(setq disassembled-form (function-lambda-expression thing))
	 (multiple-value-bind (def closurep name)
	     (function-lambda-expression thing)
           (declare (ignore closurep))
	   (setq disassembled-form `(defun ,(or name 'gazonk) 
				      ,@(if (eq (car def) 'si::lambda-block) (cddr def) (cdr def))))
	   )
	 )
	((atom thing)
	 (error 'simple-type-error
		:datum thing
		:expected-type '(OR FUNCTION (SATISFIES SI:VALID-FUNCTION-NAME-P))
		:format-control "DISASSEMBLE cannot accept ~A"
		:format-arguments (list thing)))
	((eq (car thing) 'LAMBDA)
	 (setq disassembled-form `(defun gazonk ,@(cdr thing))))
	((eq (car thing) 'SI::LAMBDA-BLOCK)
	 (setq disassembled-form `(defun ,@(rest thing))))
	(t
	 (error 'simple-type-error
		:datum thing
		:expected-type '(OR FUNCTION (SATISFIES SI:VALID-FUNCTION-NAME-P))
		:format-control "DISASSEMBLE cannot accept ~A"
		:format-arguments (list thing))))

  (let* ((null-stream (make-broadcast-stream))
         (*compiler-output1* null-stream)
         (*compiler-output2* (if h-file
				 (open h-file :direction :output :external-format :utf-8 #|:default|#)
				 null-stream))
         ;;(t3local-fun (symbol-function 'T3LOCAL-FUN))
	 (*disassemble-bindings* (cons '(*compiler-output1*) (list *standard-output*))) ;; ( symbols . values ) for progv
	 (compiler-conditions nil))
    (with-compiler-env (compiler-conditions)
      (unwind-protect
	   (progn
	     (data-init)
	     (t1expr disassembled-form)
	     (wt-nl1 "#include " *cmpinclude*)
	     (ctop-write (compute-init-name "foo" :kind :fasl)
			 (if h-file h-file "")
			 (if data-file data-file ""))
	     (data-dump data-file))
	(finish-output *standard-output*)
	(when h-file (close *compiler-output2*)))))
  nil)


(si::close-package :cl)


(proclaim '(notinline builder)) ;; for debugging

;;; Since CMP is an autoloaded optional module these cannot be load-time evaluated
(proclaim '(notinline cl::proclaim 
		      cl::compile-file-pathname
		      cl::compile-file
		      cl::compile
		      cl::disassemble))

