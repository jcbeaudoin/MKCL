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

;;(setq compiler::*trace-cc* t)

(load "cmp/load.lsp" :external-format '(:ascii :lf))

(load "compile-utils" :external-format '(:ascii :lf))

(build-module "cmp/cmp" +cmp-module-files+ 
	      :dir (pathname "./cmp/")
	      )

(mkcl:quit :exit-code 0) ;; signal to "make" that all is well.

