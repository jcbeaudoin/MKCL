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

(load "compile-utils" :external-format '(:ascii :lf))

;;(setq cl:*compile-verbose* nil cl:*load-verbose* nil)

;;(setq compiler::*trace-cc* t)


;;;
;;; * BYTECMP
;;;
(build-module "bytecmp"
              '("../contrib/bytecmp/bytecmp.lsp")
              :destdir "./ext/" #|:prefix "EXT"|#
	      )

;;;
;;; * DEFSYSTEM
;;;
(build-module "defsystem"
              '("../contrib/defsystem/defsystem.lisp")
              :destdir "./ext/" #|:prefix "EXT"|#
	      )

;;;
;;; * ASDF
;;;
(build-module "asdf"
              '("../contrib/asdf/asdf.lisp"
		"../contrib/asdf/asdf-mkcl.lisp")
              :destdir "./ext/" #|:prefix "EXT"|#
	      )

;;;
;;; * PROFILE
;;;
(build-module "profile"
              '("../contrib/profile/profile.lisp")
              :destdir "./ext/" #|:prefix "EXT"|#
	      )

;;;
;;; * Sockets library.
;;;
(build-module "sockets"
	      '("../contrib/sockets/package.lisp"
		"../contrib/sockets/sockets.lisp")
	      :destdir "./ext/" #|:prefix "EXT"|#
	      )

(build-module "serve-event"
	      '("../contrib/serve-event/serve-event.lisp")
	      :destdir "./ext/" #|:prefix "EXT"|#
	      )

;;;
;;; * Test suite
;;;
(build-module "rt" '("../contrib/rt/rt.lisp") :destdir "./ext/" #|:prefix "EXT"|#)

(mkcl:quit :exit-code 0) ;; signal to "make" that all is well.
