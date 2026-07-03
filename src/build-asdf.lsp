;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: SYSTEM -*-
;;;;
;;;;  Copyright (c) 2010-2014, 2026, Jean-Claude Beaudoin.
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

(setq cl:*compile-verbose* t cl:*load-verbose* t)

;;(setq compiler::*trace-cc* t)

;;;
;;; * ASDF
;;;
(defun build-asdf-module ()
  (build-module "asdf"
		'("../contrib/asdf/build/asdf.lisp")
		:destdir (ensure-directories-exist (pathname "./ext/asdf/"))
		)
  )


(defun compile-asdf-lib ()
  (compile-and-build-lib "asdf"
			 '("../contrib/asdf/build/asdf.lisp")
			 :destdir (ensure-directories-exist (pathname "./ext/asdf/"))
			 )
  )


(defun build-asdf-fasb ()
  (build-fasb "asdf"
	      '("./ext/asdf.a")
	      :destdir (ensure-directories-exist (pathname "./ext/asdf/"))
	      )
  )


