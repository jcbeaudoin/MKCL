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

(load "cmp.fasb")

(load "compile-utils" :external-format '(:ascii :lf))

(setq cl:*compile-verbose* t cl:*load-verbose* t)

;;(setq compiler::*trace-cc* t)

;;;
;;; * ASDF
;;;
#-(and)
(build-module "asdf"
              '("../contrib/asdf/build/asdf.lisp")
              :dir "./ext/"
	      )

(load "../contrib/asdf/build/asdf.lisp") ;; run in bytecode-compiled form.

(asdf:oos 'asdf:load-op :asdf) ;; For some strange reason this is needed by ASDF 3 to force it to register itself.

;;(push '(mkcl:getcwd) asdf:*central-registry*) ;; ASDF 1 old style

;;(push sys-dir asdf:*central-registry*) ;; ASDF 1 old style

#+asdf2
(let* ((current-dir (mkcl:getcwd))
       (src-dir (truename (merge-pathnames "../contrib/" current-dir)))
       (asdf-src-dir (truename (merge-pathnames "../contrib/asdf/" current-dir)))
       (target-dir (merge-pathnames "./asdf-stage/" current-dir))
       )
  (ensure-directories-exist target-dir)
  ;; If you want to watch ASDF activity uncomment this.
  ;;(setq asdf::*verbose-out* t) ;; needed to trace ASDF activity outside asdf:operate.
  (setq asdf::*asdf-verbose* nil)

  (asdf::clear-source-registry)
  (asdf::initialize-source-registry `(:source-registry (:tree ,(namestring asdf-src-dir))
                                                       ;;(:directory ,(namestring sys-dir))
                                                       ;;(:tree ,(namestring current-dir))
                                                       :ignore-inherited-configuration))
  (asdf::clear-output-translations)
  (asdf::initialize-output-translations `(:output-translations
                                          (,(namestring src-dir)
                                           ,(namestring target-dir))
                                          :ignore-inherited-configuration))
  );;#+asdf2

;;(format t "~&About to call asdf:find-system.")(finish-output)

(setq si::*default-external-format* '(:utf-8 :lf))

(format t "~&About to build UIOP.~%")(finish-output)
(unless (ignore-errors (asdf:bundle-system "uiop"))
  (format t "~%asdf:bundle-system failed.~%")
  (finish-output)
  (mkcl:quit :exit-code 1)
  )

(format t "~&About to compile ASDF proper.~%")(finish-output)
(unless (ignore-errors (asdf:bundle-system "asdf"))
  (format t "~%asdf:bundle-system failed.~%")
  (finish-output)
  (mkcl:quit :exit-code 1)
  )



(mkcl:quit :exit-code 0) ;; signal to "make" that all is well.

