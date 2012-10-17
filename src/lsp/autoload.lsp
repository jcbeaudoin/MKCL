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

(defun autoload (pathname &rest function-names)
  (dolist (fname function-names)
    (let ((thename fname))
      (fset fname #'(lambda (&rest args)
		      (load pathname)
		      (apply thename args))))))

(unless (fboundp 'compile)
  (defun proclaim (d)
    "Args: (decl-spec)
Gives a global declaration.  See DECLARE for possible DECL-SPECs."
    (when (eq (car d) 'SPECIAL) (mapc #'sys::*make-special (cdr d))))
  
  (autoload "SYS:cmp" 'compile-file 'compile 'compile-file-pathname 'disassemble)
  )

;; This definition of with-compilation-unit is a dummy place holder
;; to be redefined by a more specific version when the compiler is loaded.
(defmacro with-compilation-unit ((&rest options) &rest body)
  (declare (ignore options))
  `(progn ,@body))


;;; Editor.

(defun ed (&optional filename)
  "Args: (&optional filename)
Invokes the editor.  The action depends on the version of MKCL.  See the MKCL
Report for details."
  (si:system (format nil "~S ~A" (or (mkcl::getenv "EDITOR") "vi") filename)))


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

(defun help (&optional (symbol 'help))
  "Args: (&optional symbol)
MKCL specific.
Prints the documentation associated with SYMBOL.  With no args, prints the
greeting message to MKCL beginners.

Welcome to MKCL. Here are the few functions you should learn first.

	(HELP symbol) prints the online documentation associated with the
	symbol.  For example, (HELP 'CONS) will print the useful information
	about the CONS function, the CONS data type, and so on.

	(HELP* string) prints the online documentation associated with those
	symbols whose print-names have the string as substring.  For example,
	(HELP* \"PROG\") will print the documentation of the symbols such as
	PROG, PROGN, and MULTIPLE-VALUE-PROG1.

	(QUIT) ends the current MKCL session.

For the precise language specification, refer to Guy Steele's \"Common Lisp,
the Language\" and our \"MKCL Manual\".  \"MKCL Dictionary\", the hard-copied
version of MKCL online documentation, will be useful as a handbook.

Good luck!
"
  (print-doc symbol))


;;; Import functions which are useful for user interaction

;; (in-package "CL-USER")
;; (import '(sys::help sys::help* mkcl::quit))
(import '(sys::help sys::help* mkcl::quit) (find-package "CL-USER"))