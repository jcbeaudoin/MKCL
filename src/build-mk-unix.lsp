;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: SYSTEM -*-
;;;;
;;;;  Copyright (c) 2016, Jean-Claude Beaudoin.
;;;;
;;;;  This program is free software; you can redistribute it and/or
;;;;  modify it under the terms of the GNU Lesser General Public
;;;;  License as published by the Free Software Foundation; either
;;;;  version 3 of the License, or (at your option) any later version.
;;;;
;;;;  See file '../Copyright' for full details.

;;;
;;;

(load "cmp/cmp.fasb")

(load "compile-utils" :external-format '(:ascii :lf))

;;(setq cl:*compile-verbose* nil cl:*load-verbose* nil)

;;(setq compiler::*trace-cc* t)

;;;
;;; * Sockets library.
;;;
(build-module "mk-unix"
	      '("../contrib/mk-unix/mk-unix.lsp")
	      :dir "./ext/" #|:prefix "EXT"|#
	      )

(mkcl:quit :exit-code 0) ;; signal to "make" that all is well.

