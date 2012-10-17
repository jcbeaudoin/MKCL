;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: C -*-
;;;;
;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;  Copyright (c) 2001, Juan Jose Garcia Ripoll
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

;;;; BYTECMP Fake compiler which is used as a replacement when we do not
;;;;         want or can have the real native compiler.

(defpackage "BYTE-COMPILER")

(in-package "BYTE-COMPILER")
(si:reopen-package (find-package :cl))

(defun cl:compile (name &optional (def nil supplied-p) &aux form)
 (cond ((and supplied-p def)
        (when (functionp def)
	  (unless (function-lambda-expression def)
	    (return-from compile (values def nil nil)))
	  (setf def (function-lambda-expression def)))
        (setq form (if name
		     `(setf (symbol-function ',name) #',def)
		     `(set 'GAZONK #',def))))
       ((not (fboundp name))
	(error "Symbol ~s is unbound." name))
       ((typep (setf def (symbol-function name)) 'standard-generic-function)
	(warn "COMPILE can not compile generic functions yet")
	(return-from compile (values def t nil)))
       ((null (setq form (function-lambda-expression def)))
	(warn "We have lost the original function definition for ~s. Compilation failed"
	      name)
	(return-from compile (values def t nil)))
       (t
	(setq form `(setf (symbol-function ',name) #',form))))
 (eval form)
 (values name nil nil))

(defun cl:compile-file-pathname (name &key (output-file name) verbose print external-format ;; standard args
				 #+(or) (type :fasl)
				 c-file h-file data-file fasl-p) ;; extension args
  (declare (ignore verbose print external-format c-file h-file data-file fasl-p))
  (let ((extension "fasb"))
    #+(or)
    (case type
      ((:fasl :fas) (setf extension "fasb"))
      (t (error "In COMPILE-FILE-PATHNAME, the type ~A is unsupported." type)))
    (make-pathname :type extension :defaults output-file)))

(defun cl:compile-file (input-pathname
			&key
			((:verbose *compile-verbose*) *compile-verbose*)
			((:print *compile-print*) *compile-print*)
			(load nil)
			((:output-file output-pathname) 't)
			&allow-other-keys)
  (setf output-pathname (if (eq output-pathname 't)
			  (compile-file-pathname input-pathname)
			  (pathname output-pathname)))
  (when *compile-verbose*
    (format t "~&;;; Compiling ~A" input-pathname))
  (with-open-file (sin input-pathname :direction :input)
    (with-open-file (sout output-pathname :direction :output :if-exists :supersede :if-does-not-exist :create)
      (loop with *package* = *package*
         and si:*bytecode-compiler* = t
         for form = (read sin nil :EOF)
         until (eq form :EOF)
         do (let ((bytecode (si:eval-in-env form nil nil nil)))
              (with-standard-io-syntax
                (write `(FUNCALL ,bytecode) :stream sout :circle t
                       :escape t :readably t :pretty nil)
                (terpri sout))))))
  (when load
    (load output-pathname :verbose *compile-verbose*))
  (values output-pathname nil nil))

(si::close-package (find-package :cl))

(pushnew :ecl-bytecmp *features*)

(provide :BYTECMP)

(in-package :cl-user) ;; put something safe into *package* before we nuke our transient package.

(eval-when (:compile-toplevel :load-toplevel :execute) (delete-package "BYTE-COMPILER"))

