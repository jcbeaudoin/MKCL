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

(defun build-serve-event-module ()
  (build-module "SERVE-EVENT"
		'("../contrib/serve-event/serve-event.lisp")
		:destdir (ensure-directories-exist (pathname "./ext/serve-event/"))
		)
  )

(defun compile-SERVE-EVENT-lib ()
  (build-module "SERVE-EVENT"
		'("../contrib/serve-event/serve-event.lisp")
		:destdir (ensure-directories-exist (pathname "./ext/serve-event/"))
		)
  )

(defun build-SERVE-EVENT-fasb ()
  (build-fasb "SERVE-EVENT"
	      '("./ext/SERVE-EVENT.a")
	      :destdir (ensure-directories-exist (pathname "./ext/serve-event/"))
	      )
  )

(defun build-SERVE-EVENT-asd-file ()
  (build-substitute-asd-file "serve-event" "SERVE-EVENT" (pathname "./ext/serve-event/") nil))

(defun build-SERVE-EVENT-as2-file ()
  (build-substitute-as2-file "serve-event" "SERVE-EVENT" (pathname "./ext/serve-event/") nil))


