;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: SYSTEM -*-
;;;;
;;;;  Copyright (c) 2010-2012, Jean-Claude Beaudoin.
;;;;
;;;;  This program is free software; you can redistribute it and/or
;;;;  modify it under the terms of the GNU Lesser General Public
;;;;  License as published by the Free Software Foundation; either
;;;;  version 3 of the License, or (at your option) any later version.
;;;;
;;;;  See file '../Copyright' for full details.

;;;
;;;

;;(setq compiler::*trace-cc* t)

(setq compiler::*mkcl-include-directory* (truename (pathname "./c")) ;; truename is needed by MS-Windows
      compiler::*mkcl-library-directory* (truename (pathname "."))
      )

(defparameter *modules* nil)

(do ((i 6 (1+ i))
     (max (si:argc))
     )
    ((>= i max) (setq *modules* (nreverse *modules*)))
    (push (mkcl:argv i) *modules*)
    ;;(format t "~&i = ~D, arg = ~S~%" i (mkcl:argv i))
    )

(unless (compiler::build-program
	 "mkcl-full"
	 :lisp-object-files (list* "cmp/cmp.a" *modules*) ;; list of built-ins.
	 :use-mkcl-shared-libraries nil ;; force static linking
	 )
  (mkcl:quit :exit-code 1))

(mkcl:quit :exit-code 0)
