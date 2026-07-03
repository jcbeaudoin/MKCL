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
;;; * Sockets library.
;;;
(defparameter sources '("../contrib/sockets/package.lisp"
                        "../contrib/sockets/sockets.lisp"))

(clean-up "./ext/" sources)

(defun build-sockets-module ()
(build-module "SOCKETS"
	      sources
	      :destdir (ensure-directories-exist (pathname "./ext/sockets/"))
	      )
)

(defun compile-SOCKETS-lib ()
  (build-module "SOCKETS"
		sources
		:destdir (ensure-directories-exist (pathname "./ext/sockets/"))
		)
  )

(defun build-SOCKETS-fasb ()
  (build-fasb "SOCKETS"
	      '("./ext/SOCKETS.a")
	      :destdir (ensure-directories-exist (pathname "./ext/sockets/"))
	      )
  )

(defun build-SOCKETS-asd-file ()
  (build-substitute-asd-file "sockets" "SOCKETS" (pathname "./ext/sockets/") nil))

(defun build-SOCKETS-as2-file ()
  (build-substitute-as2-file "sockets" "SOCKETS" (pathname "./ext/sockets/") nil))


