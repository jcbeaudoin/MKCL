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

(load "cmp/cmp.fasb")

(load "compile-utils" :external-format '(:ascii :lf))

(setq cl:*compile-verbose* t cl:*load-verbose* t)

;;(setq compiler::*trace-cc* t)

(load "ext/asdf.fasb")

#+asdf2
(let* ((current-dir (mkcl:getcwd))
       ;;(src-dir (truename (merge-pathnames "../contrib/" current-dir)))
       (uiop-src-dir (truename (merge-pathnames "../contrib/asdf/uiop/" current-dir)))
       (target-dir (merge-pathnames "./asdf-stage/asdf/uiop/" current-dir))
       )
  (ensure-directories-exist target-dir)
  ;; If you want to watch ASDF activity uncomment this.
  ;;(setq asdf::*verbose-out* t) ;; needed to trace ASDF activity outside asdf:operate.
  (setq asdf::*asdf-verbose* nil)

  (asdf::clear-source-registry)
  (asdf::initialize-source-registry `(:source-registry (:tree ,(namestring uiop-src-dir))
                                                       ;;(:directory ,(namestring sys-dir))
                                                       ;;(:tree ,(namestring current-dir))
                                                       :ignore-inherited-configuration))
  (asdf::clear-output-translations)
  (asdf::initialize-output-translations `(:output-translations
                                          (,(namestring uiop-src-dir)
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

(mkcl:quit :exit-code 0) ;; signal to "make" that all is well.

