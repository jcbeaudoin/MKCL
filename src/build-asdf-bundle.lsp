;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: SYSTEM -*-
;;;;
;;;;  Copyright (c) 2012, Jean-Claude Beaudoin.
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

(load "cmp.fasb")

(load "compile-utils" :external-format '(:ascii :lf))

(setq cl:*compile-verbose* t cl:*load-verbose* nil)

;;(setq compiler::*trace-cc* t)

;(defparameter arg-base 5)
(defparameter arg-base 7)

(load "asdf.fasb") ;; load the local one.

;; Let's get rid of the compiler output cache!
#+asdf2
;;(asdf:initialize-output-translations "/:") ;; it would be "/;" on MS-windows.
(asdf:disable-output-translations)

#+asdf2 (setq asdf::*asdf-verbose* nil)

#|
;; If you want to watch ASDF activity uncomment this.
(progn
  #+asdf2 (setq asdf::*asdf-verbose* t)
  (setq asdf::*verbose-out* t) ;; needed to trace ASDF activity outside asdf:operate.
  )
|#

(let ((asdf:*central-registry* (cons (translate-logical-pathname #P"CONTRIB:asdf-bundle;")
				     asdf:*central-registry*)))
  (asdf::clear-system :asdf-bundle) ;; we hope to force a reload.
  (multiple-value-bind (result bundling-error)
      (ignore-errors (asdf:oos 'asdf:compile-op :asdf-bundle :force t))
    (unless result
      (format *error-output*
	      "~&ASDF failed to load package asdf-bundle! Reason is: ~S ~A.~%"
	      bundling-error
	      bundling-error)
      (mkcl:quit :exit-code 1)
      )))

(mkcl:quit :exit-code 0)

