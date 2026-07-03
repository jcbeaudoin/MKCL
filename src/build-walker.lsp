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
(defun build-walker-module ()
  (build-module "WALKER"
		'("../contrib/walker/walk.lsp")
		:destdir (ensure-directories-exist (pathname "./ext/walker/"))
		)
  )

(defun compile-WALKER-lib ()
  (build-module "WALKER"
		'("../contrib/walker/walk.lsp")
		:destdir (ensure-directories-exist (pathname "./ext/walker/"))
		)
  )

(defun build-WALKER-fasb ()
  (build-fasb "WALKER"
	      '("./ext/WALKER.a")
	      :destdir (ensure-directories-exist (pathname "./ext/walker/"))
	      )
  )

(defun build-WALKER-asd-file ()
  (build-substitute-asd-file "walker" "WALKER" (pathname "./ext/walker/") nil))

(defun build-WALKER-as2-file ()
  (build-substitute-as2-file "walker" "WALKER" (pathname "./ext/walker/") nil))


