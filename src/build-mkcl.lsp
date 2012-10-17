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

(load "cmp.fasb")

;;(proclaim '(optimize (debug 1))) ;; faster, no debug info.
(proclaim '(optimize (debug 0))) ;; faster, no debug info.
#-(and)
(progn
  (setq compiler::*compiler-break-enable* t) ;; enter debugger on compiler internal error
  (setq compiler::*delete-compiler-internal-files* nil)
  (setq *compile-extra-options* '(:c-file t :data-file t :h-file t))
  (proclaim '(optimize (debug 3))) ;; full debug info
  ;;(proclaim '(optimize (safety 3))) ;; full safety checks
  (setq compiler::*trace-cc* t)
  )

;;; -H traces include files in gcc.
;;(setq compiler::*cc-flags* (concatenate 'base-string "-H " compiler::*cc-flags*))



(setq compiler::*mkcl-include-directory* (truename (pathname ".")) ;; truename is needed by MS-Windows
      compiler::*mkcl-library-directory* (truename (pathname "."))
      )

#+unix
(let (;;(compiler::*suppress-compiler-messages* nil)
      ;;(compiler::*suppress-compiler-warnings* nil)
      ;;(compiler::*suppress-compiler-notes* nil)
      ;;(*compile-verbose* t)
      ;;(*compile-print* t)
      #+(or)
      (compiler::*ld-flags* (concatenate 
		      'base-string
		      " -rdynamic libmkcltop.a liblsp.a "
		      ;;"libmkclmin.a libmkclgc.a "
		      "libmkclmin.a "
		      compiler::*support-libraries*
		      compiler::*external-ld-flags*
		      ))
      (compiler::*ld-flags* compiler::*external-ld-flags*) ;; prevent -lmkcl from showing up.
      (object-files (list "libmkcltop.a" "liblsp.a" "libmkclmin.a" compiler::*support-libraries*))
      )
  (unless (compiler::build-program
	   "bin/mkcl"
	   :lisp-object-files '( "libcmp.a" ) ;; list of built-ins.
	   :object-files object-files
	   :extra-ld-flags "-rdynamic" ;; export main executable symbols to loaded modules and fasls.
;;	   :extra-ld-flags "-pg -rdynamic"  ;; for profiling
	   )
    (mkcl:quit :exit-code 1))
  )


#+windows
(let (;;(compiler::*suppress-compiler-messages* nil)
      ;;(compiler::*suppress-compiler-warnings* nil)
      ;;(compiler::*suppress-compiler-notes* nil)
      ;;(*compile-verbose* t)
      ;;(*compile-print* t)
      (compiler::*ld-flags* compiler::*external-ld-flags*)  ;; prevent -lmkcl from showing up.
      (mkcl-lib-name (concatenate 'base-string
				  compiler::+shared-library-prefix+
				  "mkcl_" (si:mkcl-version) "."
				  compiler::+shared-library-extension+))
      )
  (unless (compiler::build-program
	   "bin/mkcl"
	   :extra-ld-flags "-Wl,--stack,0x800000" ;; Stack of 8MB.
	   :object-files (list mkcl-lib-name)
	   :epilogue-code '(PROGN (REQUIRE "CMP") (SI::TOP-LEVEL))
	   )
    (mkcl:quit :exit-code 1))
  )

(mkcl:quit :exit-code 0) ;; tell make that all is well.

