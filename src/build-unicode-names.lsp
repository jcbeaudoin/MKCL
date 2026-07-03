;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: SYSTEM -*-
;;;;
;;;;  Copyright (c) 2010-2016, 2026, Jean-Claude Beaudoin.
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
;;; * DEFSYSTEM
;;;
(defun build-unicode-names-module ()
(build-module "UNICODE-NAMES"
              '("../contrib/unicode/unicode-names.lsp")
              :destdir (ensure-directories-exist (pathname "./ext/unicode-names/"))
	      )
)


(defun compile-UNICODE-NAMES-lib ()
  (build-module "UNICODE-NAMES"
		'("../contrib/unicode/unicode-names.lsp")
		:destdir (ensure-directories-exist (pathname "./ext/unicode-names/"))
		)
  )

(defun build-UNICODE-NAMES-fasb ()
  (build-fasb "UNICODE-NAMES"
	      '("./ext/UNICODE-NAMES.a")
	      :destdir (ensure-directories-exist (pathname "./ext/unicode-names/"))
	      )
  )

(defun build-UNICODE-NAMES-asd-file ()
  (build-substitute-asd-file "unicode-names" "UNICODE-NAMES" (pathname "./ext/unicode-names/") nil))

(defun build-UNICODE-NAMES-as2-file ()
  (build-substitute-as2-file "unicode-names" "UNICODE-NAMES" (pathname "./ext/unicode-names/") nil))


