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

(defparameter arg-base 7)

(defparameter sys-name (mkcl:argv (incf arg-base)))

;;(format t "~&sys-name = ~S~%" sys-name)

(defparameter sys-dir (truename (mkcl:argv (incf arg-base))))

;;(format t "sys-dir = ~S~%" sys-dir)

;; Guard against environment interference.
(mkcl:setenv "ASDF_OUTPUT_TRANSLATIONS" nil)
(mkcl:setenv "CL_SOURCE_REGISTRY" nil)

(load "ext/ASDF.fasb") ;; load the local one.

;;(push '(mkcl:getcwd) asdf:*central-registry*) ;; ASDF 1 old style

;;(push sys-dir asdf:*central-registry*) ;; ASDF 1 old style

#+asdf2
(let* ((current-dir (mkcl:getcwd))
       (src-dir (truename (merge-pathnames "../contrib/" current-dir)))
       (target-dir (merge-pathnames "./asdf-stage/" current-dir))
       )
  (ensure-directories-exist target-dir)
  ;; If you want to watch ASDF activity uncomment this.
  ;;(setq asdf::*verbose-out* t) ;; needed to trace ASDF activity outside asdf:operate.
  (setq asdf::*asdf-verbose* nil)

  (asdf::clear-source-registry)
  (asdf::initialize-source-registry `(:source-registry (:directory ,(namestring sys-dir))
                                                       (:directory ,(namestring target-dir))
                                                       :ignore-inherited-configuration))
  (asdf::clear-output-translations)
  (asdf::initialize-output-translations `(:output-translations
                                          (,(namestring src-dir)
                                           ,(namestring target-dir))
                                          :ignore-inherited-configuration))
  );;#+asdf2


;;(format t "~&Done with asdf setup.")(finish-output)



(defparameter sys nil)

;;(format t "~&About to call asdf:find-system.")(finish-output)

(setq si::*default-external-format* '(:utf-8 :lf))

(multiple-value-bind (result bundling-error)
    (ignore-errors (asdf:bundle-system sys-name))
  (unless result
    (format t "~%asdf:bundle-system for ~S failed! Reason is: ~S ~A.~%" sys-name bundling-error bundling-error)
    (finish-output)
    (mkcl:quit :exit-code 1)))

(mkcl:quit :exit-code 0)

