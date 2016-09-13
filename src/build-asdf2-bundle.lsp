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

(load "cmp/cmp.fasb")

(load "compile-utils" :external-format '(:ascii :lf))

(setq cl:*compile-verbose* t cl:*load-verbose* t)

;;(setq compiler::*trace-cc* t)

(load "ext/asdf2.fasb") ;; load the local ASDF 2.

;; Let's get rid of the compiler output cache!
(asdf:disable-output-translations)

(setq asdf::*asdf-verbose* nil)
;; If you want to watch ASDF 2 activity uncomment this.
;;(setq asdf::*verbose-out* t)


(let ((asdf:*central-registry* (cons (make-pathname :directory '(:relative "asdf2-bundle")
						    :defaults (translate-logical-pathname #P"CONTRIB:"))
				     asdf:*central-registry*)))
  (asdf::clear-system :asdf2-bundle) ;; we hope to force a reload.
  (multiple-value-bind (result bundling-error)
      (ignore-errors (asdf:oos 'asdf:compile-op :asdf2-bundle :force t))
    (unless result
      (format *error-output*
	      "~&ASDF failed to load package asdf2-bundle! Reason is: ~S ~A.~%"
	      bundling-error
	      bundling-error)
      (mkcl:quit :exit-code 1)
      )))

(mkcl:quit :exit-code 0)

