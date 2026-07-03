;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: SYSTEM -*-
;;;;
;;;;  Copyright (c) 2016, 2026, Jean-Claude Beaudoin.
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
;;; * Sockets library.
;;;
(defun build-mk-unix-module ()
(build-module "mk-unix"
	      '("../contrib/mk-unix/mk-unix.lsp")
	      :destdir (ensure-directories-exist (pathname "./ext/mk-unix/"))
	      )
)


(defun compile-mk-unix-lib ()
  (build-module "mk-unix"
		'("../contrib/mk-unix/mk-unix.lsp")
		:destdir (ensure-directories-exist (pathname "./ext/mk-unix/"))
		)
  )

(defun build-mk-unix-fasb ()
  (build-fasb "mk-unix"
	      '("./ext/mk-unix.a")
	      :destdir (ensure-directories-exist (pathname "./ext/mk-unix/"))
	      )
  )

(defun build-mk-unix-asd-file ()
  (build-substitute-asd-file "mk-unix" "mk-unix" (pathname "./ext/mk-unix/") nil))

(defun build-mk-unix-as2-file ()
  (build-substitute-as2-file "mk-unix" "mk-unix" (pathname "./ext/mk-unix/") nil))

