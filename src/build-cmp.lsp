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

;;(setq compiler::*trace-cc* t)

(load "cmp/load.lsp" :external-format '(:ascii :lf))
(load "parallel-compile-utils" :external-format '(:ascii :lf))

(defvar cmp-dir (pathname "./cmp/"))

(unless (probe-file cmp-dir)  ;; ensure destdir exists.
  (si::mkdir cmp-dir #o0777))

(defun compile-and-build-cmp-lib ()
  (compile-and-build-lib "CMP" +cmp-module-files+
			 :destdir cmp-dir
			 )
  )

(defun build-cmp-fasb ()
  (build-fasb "CMP" (list "./cmp/CMP.a")
		:destdir cmp-dir
		)
  )

;; (defun build-CMP-asdf-support-file ()
;;   (build-module-asdf-support-file "CMP" :destdir cmp-dir))

;; (defun build-CMP-asdf2-support-file ()
;;   (build-module-asdf2-support-file "CMP" :destdir cmp-dir))

;;;
