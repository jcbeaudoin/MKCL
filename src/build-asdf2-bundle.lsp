;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: SYSTEM -*-
;;;;
;;;;  Copyright (c) 2014, Jean-Claude Beaudoin.
;;;;
;;;;  This program is free software; you can redistribute it and/or
;;;;  modify it under the terms of the GNU Lesser General Public
;;;;  License as published by the Free Software Foundation; either
;;;;  version 3 of the License, or (at your option) any later version.
;;;;
;;;;  See file '../Copyright' for full details.

;;;
;;;


(si::pathname-translations "CONTRIB" `(("**;*.*.*" ,(merge-pathnames "./../contrib/**/*.*" (mkcl:getcwd)))))

(load "cmp/CMP.fasb")

(load "compile-utils" :external-format '(:ascii :lf))

;;(setq cl:*compile-verbose* t cl:*load-verbose* t)

;;(setq compiler::*trace-cc* t)

(multiple-value-bind (result bundling-error)
    (ignore-errors (load "ext/asdf2.fasb")) ;; load the local ASDF 2 which in turn load/build asdf2-bundle.
  (unless result
    (format *error-output*
            "~&ASDF failed to load package asdf2-bundle! Reason is: ~S ~A.~%"
            bundling-error
            bundling-error)
    (mkcl:quit :exit-code 1)
    ))

(mkcl:quit :exit-code 0)

