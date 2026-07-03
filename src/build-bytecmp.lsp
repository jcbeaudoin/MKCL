;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: SYSTEM -*-
;;;;
;;;;  Copyright (c) 2010-2012, 2026, Jean-Claude Beaudoin.
;;;;
;;;;  This program is free software; you can redistribute it and/or
;;;;  modify it under the terms of the GNU Lesser General Public
;;;;  License as published by the Free Software Foundation; either
;;;;  version 3 of the License, or (at your option) any later version.
;;;;
;;;;  See file '../Copyright' for full details.

;;;
;;;

(load "cmp/CMP.fasb")

(load "compile-utils" :external-format '(:ascii :lf))

;;(setq cl:*compile-verbose* nil cl:*load-verbose* nil)

;;(setq compiler::*trace-cc* t)


;;;
;;; * BYTECMP
;;;
(defun build-bytecmp-module ()
  (build-module "BYTECMP"
		'("../contrib/bytecmp/bytecmp.lsp")
		:destdir (ensure-directories-exist (pathname "./ext/bytecmp/"))
		)
  )

(defun compile-BYTECMP-lib ()
  (build-module "BYTECMP"
		'("../contrib/bytecmp/bytecmp.lsp")
		:destdir (ensure-directories-exist (pathname "./ext/bytecmp/"))
		)
  )

(defun build-BYTECMP-fasb ()
  (build-fasb "BYTECMP"
	      '("./ext/BYTECMP.a")
	      :destdir (ensure-directories-exist (pathname "./ext/bytecmp/"))
	      )
  )

(defun build-BYTECMP-asd-file ()
  (build-substitute-asd-file "bytecmp" "BYTECMP" (pathname "./ext/bytecmp/") nil))

(defun build-BYTECMP-as2-file ()
  (build-substitute-as2-file "bytecmp" "BYTECMP" (pathname "./ext/bytecmp/") nil))

