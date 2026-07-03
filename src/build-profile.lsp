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
;;; * PROFILE
;;;
(defun build-profile-module ()
  (build-module "profile"
		'("../contrib/profile/profile.lisp")
		:destdir (ensure-directories-exist (pathname "./ext/profile/"))
		)
  )


(defun compile-profile-lib ()
  (build-module "profile"
		'("../contrib/profile/profile.lisp")
		:destdir (ensure-directories-exist (pathname "./ext/profile/"))
		)
  )

(defun build-profile-fasb ()
  (build-fasb "profile"
	      '("./ext/profile.a")
	      :destdir (ensure-directories-exist (pathname "./ext/profile/"))
	      )
  )

(defun build-profile-asd-file ()
  (build-substitute-asd-file "profile" "profile" (pathname "./ext/profile/") nil))

(defun build-profile-as2-file ()
  (build-substitute-as2-file "profile" "profile" (pathname "./ext/profile/") nil))


