;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: SYSTEM -*-
;;;;
;;;;  Copyright (c) 2010-2019, Jean-Claude Beaudoin.
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

;;(setq cl:*compile-verbose* t cl:*load-verbose* t)

;;(setq compiler::*trace-cc* t)

;;;;;;;;;;;;;;;

(defconstant +uiop-module-files+
'(
  "../contrib/asdf/uiop/package.lisp"
  "../contrib/asdf/uiop/common-lisp.lisp"
  "../contrib/asdf/uiop/utility.lisp"
  "../contrib/asdf/uiop/version.lisp"
  "../contrib/asdf/uiop/os.lisp"
  "../contrib/asdf/uiop/pathname.lisp"
  "../contrib/asdf/uiop/filesystem.lisp"
  "../contrib/asdf/uiop/stream.lisp"
  "../contrib/asdf/uiop/image.lisp"
  "../contrib/asdf/uiop/lisp-build.lisp"
  "../contrib/asdf/uiop/launch-program.lisp"
  "../contrib/asdf/uiop/run-program.lisp"
  "../contrib/asdf/uiop/configuration.lisp"
  "../contrib/asdf/uiop/backward-driver.lisp"
  "../contrib/asdf/uiop/driver.lisp"
  ))

(build-module "uiop" +uiop-module-files+ :version "3.3.3"
              :destdir (ensure-directories-exist (pathname "./ext/uiop/"))
	      )

(mkcl:quit :exit-code 0) ;; signal to "make" that all is well.
