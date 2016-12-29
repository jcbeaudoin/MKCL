;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: SYSTEM -*-
;;;;
;;;;  Copyright (c) 2010-2014, Jean-Claude Beaudoin.
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
(build-module "asdf3"
              '("../contrib/asdf3/build/asdf3.lisp")
              :destdir "./ext/"
	      )

(mkcl:quit :exit-code 0) ;; signal to "make" that all is well.

