;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: SYSTEM -*-
;;;;
;;;;  cmdline.lsp -- command line processing
;;;;
;;;;  Copyright (c) 2005, Juan Jose Garcia-Ripoll
;;;;  Copyright (c) 2010-2012, Jean-Claude Beaudoin.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Lesser General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 3 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../../Copyright' for full details.
;;;;

(in-package "SYSTEM")

(export '(*lisp-init-file-list*
          *help-message*
          +default-command-arg-rules+
          command-args
          process-command-args))

(defvar *lisp-init-file-list* '("~/.mkcl" "~/.mkclrc")
  "List of files automatically loaded when MKCL is invoked.")

(defvar *help-message* "
Usage: mkcl [-? | --help]
            [--version]
            [-lib-dir dir]
            [-load file]
            [-shell file]
            [-eval expr]
            [-norc]
            [-hp | -nohp]
            [-debug | -nodebug]
            [--lisp-temp-stack-limit size]
            [--frame-stack-limit size]
            [--binding-stack-limit size]
            [--heap-size-limit size]
            [-q | --quiet | --silent]
            [--external-format external-format-spec]
            [[-o ofile] [-c [cfile]] [-h [hfile]] [-data [datafile]] [-not-fasl-p] -compile file]
            [[-o ofile] -link file+]
            [-- args* ]
"
 "Prints a help message about command line arguments of MKCL")

(defun command-args ()
  "Returns the command line arguments as list"
  (loop for i from 0 below (argc)
	collect (argv i)))

(defun command-arg-error (str &rest fmt-args)
  ;; Format an error message and quit
  (apply #'format *error-output* str fmt-args)
  (princ *help-message* *error-output*)
  (mkcl:quit :exit-code 1))

(defconstant +default-command-arg-rules+
  '(("--help" 0 #0=(progn (princ *help-message* *standard-output*) (mkcl:quit)) :noloadrc)
    ("-?" 0 #0# :noloadrc)
    ("-h" 0 #0# :noloadrc)
    ("-norc" 0 nil :noloadrc)
    ("--" 0 nil :stop)
    ("--version" 0
     (progn (setq quit 0)
	    (format *standard-output* "~&MKCL ~A" (lisp-implementation-version))
	    (print-copyright))
     :noloadrc)
    ("-debug" 0 (setf si::*break-enable* t))
    ("-nodebug" 0 (setf si::*break-enable* nil))
    ("-eval" 1 (eval (read-from-string 1)))
    ("-shell" 1 (progn (setq quit 0) (load 1 :verbose nil)))
    ("-load" 1 (load 1 :verbose verbose :external-format external-format))
    ("-lib-dir" 1 (setf (logical-pathname-translations "SYS")
			`(("**;*.*" ,(merge-pathnames "**/*.*" (truename 1))))))
    ("--heap-size-limit" 1 (si:set-heap-size-limit (read-from-string 1)))
    ("--lisp-temp-stack-limit" 1 (si:set-lisp-temp-stack-limit (read-from-string 1)))
    ("--frame-stack-limit" 1 (si:set-frame-stack-limit (read-from-string 1)))
    ("--binding-stack-limit" 1 (si:set-binding-stack-limit (read-from-string 1)))
    ("--trap-fpe" 0 (si::trap-fpe t t))
    ("--no-trap-fpe" 0 (si::trap-fpe t nil))
    ("-compile" 1
     (progn
       (setq quit
	     (if (nth-value 3
		     (compile-file 1 :output-file output-file :c-file c-file
				   :h-file h-file :data-file data-file
				   :verbose verbose :fasl-p fasl-p :external-format external-format))
		 1
		 0)
	     output-file t
	     c-file nil
	     h-file nil
	     data-file nil
	     fasl-p t)))
    ("-link" &rest
     (progn
       (require 'cmp)
       (funcall (read-from-string "compiler::build-program")
		(or output-file "lisp.exe") :lisp-files '&rest)
       (setq output-file t quit t)))
    ("-o" 1 (setq output-file 1))
    ("-c" 1 (setq c-file 1))
    ("-h" 1 (setq h-file 1))
    ("-data" 1 (setq data-file 1))
    ("-q" 0 #1=(setq verbose nil *display-banner* nil
		     *load-verbose* nil *compile-verbose* nil
		     *load-print* nil *compile-print* nil))
    ("--quiet" 0 #1#)
    ("--silent" 0 #1#)
    ("-hp" 0 (setf *relative-package-names* t))
    ("-nohp" 0 (setf *relative-package-names* nil))
    ("--external-format" 1 (setq external-format (let ((*package* (find-package :keyword))) (read-from-string 1))))
    ("-not-fasl-p" 0 (setq fasl-p nil))))

(defun produce-init-code (option-list rules)
  (do* ((commands '())
        (stop nil)
	(loadrc t))
       ((or stop (null option-list))
	(values `(let ((output-file t)
		       (c-file nil)
		       (h-file nil)
		       (data-file nil)
		       (external-format :default)
		       (verbose t)
		       (fasl-p t)
		       (quit nil))
		   ,@(nreverse commands)
		   (when quit (mkcl:quit :exit-code 0)))
		loadrc
		option-list))
    (let* ((option (pop option-list))
	   (rule (assoc option rules :test #'string=)))
      (unless rule
	;; If there is a default rule, group all remaining arguments
	;; including the unmatched one, and pass them to this rule.
	(setf rule (assoc "*DEFAULT*" rules :test #'string=)
	      option-list `('(,option ,@option-list))
	      stop t)
	(unless rule
	  (command-arg-error "~&Unknown command line option ~A.~%" option)))
      (progn
	(case (fourth rule)
	  (:noloadrc (setf loadrc nil))
	  (:loadrc (setf loadrc t))
	  (:stop (setf option-list nil)))
	(let ((pattern (copy-tree (third rule)))
              (noptions (second rule)))
	  (unless (equal noptions 0)
	    (when (null option-list)
	      (command-arg-error
	       "~&Missing argument after command line option ~A.~%"
	       option))
            (if (or (eq noptions 'rest) (eq noptions '&rest))
		(progn (nsubst option-list noptions pattern)
		       (setf option-list nil))
		(nsubst (pop option-list) noptions pattern)))
	  (push pattern commands))))))

(defun process-command-args (&key
			     (args (rest (command-args)))
			     (rules +default-command-arg-rules+))
"PROCESS-COMMAND-ARGS takes a list of arguments and processes according
to a set of rules. These rules are of the format

	(option-name nargs template [ :stop | :noloadrc | :loadrc ] )

OPTION-NAME is a string containing the command line option. NARGS is
the number of arguments that this option takes. TEMPLATE is a lisp
form where numbers from 0 to NARGS will be substituted by the
arguments, and which will be evaluated afterwards. The flags :STOP,
:NOLOADRC and :LOADRC denote whether to stop processing the command
line after this option and whether the initialization file will be
loaded before evaluating all forms.

An excerpt of the rules used by MKCL:
'((\"--help\" 0 #0=(progn (princ *help-message* *standard-output*) (mkcl:quit))
               :noloadrc)
  (\"-?\" 0 #0# :noloadrc)
  (\"-h\" 0 #0# :noloadrc)
  (\"-norc\" 0 nil :noloadrc)
  (\"--\" 0 nil :stop)
  (\"-eval\" 1 (eval (read-from-string 1))))
"
  (multiple-value-bind (commands loadrc)
      (produce-init-code args rules)

    (when loadrc
      (dolist (file *lisp-init-file-list*)
	(handler-bind
	 ((error
	   #'(lambda (c)
	       (if *break-enable*
		   (invoke-debugger c)
		 (progn
		   (format *error-output*
			   "~&Error during processing of configuration file: ~A~%~A.~%"
			   file c)
		   (return)
		   ))))
	  ((and condition (not warning))
	   #'(lambda (c)
	       (if *break-enable*
		   (invoke-debugger c)
		 (progn
		   (format *error-output*
			   "~&Unhandled condition signaled during processing of configuration file: ~A~%~A.~%"
			   file c)
		   (return)
		   )))))
	 (when (load file :if-does-not-exist nil :search-list nil :verbose nil)
	   (return))
	 )
	)
      )
    (handler-bind 
     ((error
       #'(lambda (c)
	   (if *break-enable*
	       (invoke-debugger c)
	     (progn
	       (format *error-output*
		       "~&Error during command line arguments processing:~%~A.~%"
		       c)
	       (mkcl:quit :exit-code 1)
	       ))))
      ((and condition (not warning))
       #'(lambda (c)
	   (if *break-enable*
	       (invoke-debugger c)
	     (progn
	       (format *error-output*
		       "~&Unhandled condition signaled during command line arguments processing:~%~A.~%"
		       c)
	       (mkcl:quit :exit-code 1)
	       )))))
     (eval commands)
     )
    )
  )

