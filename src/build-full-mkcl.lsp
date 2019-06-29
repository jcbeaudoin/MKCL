;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: SYSTEM -*-
;;;;
;;;;  Copyright (c) 2010-2019, Jean-Claude Beaudoin.
;;;;
;;;;  This program is free software; you can redistribute it and/or
;;;;  modify it under the terms of the GNU Lesser General Public
;;;;  License as published by the Free Software Foundation; either
;;;;  version 3 of the License, or (at your option) any later version.
;;;;
;;;;  See file '../Copyright' for full details.

;;;
;;;
(push :mkcl-bootstrap *features*)

(load "cmp/load.lsp" :external-format '(:ascii :lf))

;;(setq compiler::*trace-cc* t)

(setq compiler::*mkcl-include-directory* (truename (pathname "./c")) ;; truename is needed by MS-Windows
      compiler::*mkcl-library-directory* (truename (pathname "."))
      )

(defparameter *build-modules* (list "cmp/CMP.a"))

(do ((i 8 (1+ i))
     (max (mkcl:argc))
     )
    ((>= i max) (setq *build-modules* (nreverse *build-modules*)))
    (push (mkcl:argv i) *build-modules*)
    ;;(format t "~&i = ~D, arg = ~S~%" i (mkcl:argv i))
    )

;;(format t "~&*build-modules* = ~S~%" *build-modules*) (finish-output)

(unless (compiler::build-program
	 "mkcl-full"
	 :lisp-object-files *build-modules* ;; list of built-ins.
	 :use-mkcl-shared-libraries nil ;; force static linking
	 )
  (mkcl:quit :exit-code 1))

(mkcl:quit :exit-code 0)
