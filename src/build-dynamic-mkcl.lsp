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

;;(si::pathname-translations "SRC" `(("**;*.*.*" "./**/*.*")))
;;(load "cmp/load.lsp" :external-format '(:ascii :lf))
(load "cmp.fasb")

(setq compiler::*mkcl-include-directory* (truename (pathname ".")) ;; truename is needed by MS-Windows
      compiler::*mkcl-library-directory* (truename (pathname "."))
      )

(let (;;(compiler::*suppress-compiler-messages* nil)
      ;;(compiler::*suppress-compiler-warnings* nil)
      ;;(compiler::*suppress-compiler-notes* nil)
      ;;(*compile-verbose* t)
      ;;(*compile-print* t)
      (compiler::*ld-flags* compiler::*external-ld-flags*)
      (mkcl-lib-name (concatenate 'base-string
				  compiler::+shared-library-prefix+
				  "mkcl."
				  compiler::+shared-library-extension+))
      )
  (unless (compiler::build-program
	   #+msvc "mkcl2"
	   #+unix "bin/mkcl-dyn"
	   #+mingw32 "bin/mkcl"
	   ;;:object-files '( "@SHAREDPREFIX@mkcl.@SHAREDEXT@" )
	   :object-files (list mkcl-lib-name)
	   )
    (mkcl:quit :exit-code 1))
  )

(mkcl:quit :exit-code 0)
