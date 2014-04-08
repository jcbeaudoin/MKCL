;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: SYSTEM -*-
;;;;
;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;  Copyright (c) 2012, Jean-Claude Beaudoin.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Lesser General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 3 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../../Copyright' for full details.



;;; Program Development Environment

(in-package "SYSTEM")

(defun lisp-implementation-type ()
  "Args: ()
Returns the string \"MKCL\"."
  "MKCL")

;;; Compiler functions.

(defun autoload (module &rest function-names)
  (dolist (fname function-names)
    (let ((thename fname))
      (fset fname #'(lambda (&rest args)
		      (require module)
		      (apply thename args))))))

(unless (fboundp 'compile)
#|
  (defun proclaim (d)
    "Args: (decl-spec)
Gives a global declaration.  See DECLARE for possible DECL-SPECs."
    (when (eq (car d) 'SPECIAL) (mapc #'sys::*make-special (cdr d))))
|#
  
  (autoload "cmp" 'compile-file 'compile 'compile-file-pathname 'disassemble)
  )



;;;
;;; These default settings are equivalent to (optimize (speed 3) (space 0) (safety 2) (debug 2))
;;;
(defvar *safety* 2)
(defvar *speed* 3)
(defvar *space* 0)
(defvar *debug* 2)
(defvar *compilation-speed* 0)

(defvar *compilation-unit-environment* nil) ;; NIL stands for the null lexical environment.

(defvar *compiler-floating-point-exclusion-set* '(floating-point-inexact
						  floating-point-invalid-operation
						  floating-point-underflow
						  floating-point-overflow
						  division-by-zero))

;; This definition of with-compilation-unit is a dummy place holder
;; to be redefined by a more specific version when the compiler is loaded.
(defmacro with-compilation-unit ((&rest options) &rest body)
  (declare (ignore options))
  `(progn ,@body))


;;; Editor.

(defun ed (&optional filename)
  "Args: (&optional filename)
Invokes the editor.  The exact action depends on the version of MKCL."
  (let ((command (or (mkcl::getenv "EDITOR") #+unix "vi" #+windows "notepad")))
    (unless command (error "Function cl:ed does not know which editor to invoke."))
    (when filename
      (setq command (format nil "~S \"~A\"" command filename)))
    (mkcl::system command)))


;;; Allocator.

(defun room (&optional x)
  (declare (ignore x))
  "Args: (&optional (x t))
Displays information about storage allocation.  The optional X is simply ignored."

  (multiple-value-bind (total-mem free-mem gc_is_parallel)
       (si:mem-stats)
     (format t "~2%Total dynamic space: ~D bytes.~%~
                   Free dynamic space:  ~D bytes.~%"
	     total-mem free-mem)
     (if gc_is_parallel
       (format t "GC marking is parallel.~%")
       (format t "GC marking is sequential.~%")
       )
     )
  (values)
  )


;;; Help.

(defun mkcl::help (&optional (symbol 'mkcl::help))
  "Args: (&optional symbol)
MKCL specific.
Prints the documentation associated with SYMBOL.  With no args, it prints this
greeting message to MKCL beginners:

Welcome to MKCL. Here are the few functions you should learn first.

	(MKCL:HELP symbol) prints the online documentation associated with the
	symbol.  For example, (HELP 'CONS) will print the useful information
	about the CONS function, the CONS data type, and so on.

	(MKCL:HELP* string) prints the online documentation associated with those
	symbols whose print-names have the string as substring.  For example,
	(HELP* \"PROG\") will print the documentation of the symbols such as
	PROG, PROGN, and MULTIPLE-VALUE-PROG1.

	(MKCL:QUIT) ends the current MKCL session.

For the precise language specification, refer to the ANSI standard document
ANSI INCITS 226-1994 (R2004) or to its hypertext version known as
the Common Lisp HyperSpec (CLHS), accessible at this URL:
http://www.lispworks.com/documentation/HyperSpec/Front/index.htm

"
  (print-doc symbol))

(export 'mkcl::help :mkcl)

;;; Import functions which are useful for user interaction

;;(import '(sys::help sys::help* mkcl::quit) (find-package "CL-USER"))
