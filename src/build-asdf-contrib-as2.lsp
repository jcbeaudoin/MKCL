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

(si::pathname-translations "CONTRIB" `(("**;*.*.*" ,(merge-pathnames "./../contrib/**/*.*" (mkcl:getcwd)))))

(load "cmp/cmp.fasb")

(load "compile-utils" :external-format '(:ascii :lf))

;;(setq cl:*compile-verbose* t cl:*load-verbose* t)

;;(setq compiler::*trace-cc* t)

(defparameter arg-base 7)

(defparameter sys-name (mkcl:argv (incf arg-base)))

;;(format t "~&sys-name = ~S~%" sys-name)

(defparameter sys-dir (truename (mkcl:argv (incf arg-base))))

;;(format t "sys-dir = ~S~%" sys-dir)

(defparameter dest-dir (truename (mkcl:argv (incf arg-base))))

;;(format t "dest-dir = ~S~%" dest-dir)

(load "ext/asdf2.fasb") ;; load the local one.

;;(push '(mkcl:getcwd) asdf:*central-registry*) ;; ASDF 1 old style

;;(push sys-dir asdf:*central-registry*) ;; ASDF 1 old style

#+asdf2
(let ((current-dir (mkcl:getcwd)))
  ;; If you want to watch ASDF activity uncomment this.
  ;;(setq asdf::*verbose-out* t) ;; needed to trace ASDF activity outside asdf:operate.
  (setq asdf::*asdf-verbose* nil)

  (asdf::clear-source-registry)
  (asdf::initialize-source-registry `(:source-registry (:directory ,(namestring sys-dir))
                                                       (:tree ,(namestring current-dir))
                                                       :ignore-inherited-configuration))
  );;#+asdf2


;;(format t "~&Done with asdf setup.")(finish-output)



(defparameter sys nil)

;;(format t "~&About to call asdf:find-system.")(finish-output)

(setq si::*default-external-format* '(:utf-8 :lf))

;; This stuff was needed by ASDF 2.
;; handler-bind could be better than ignore-errors here. JCB
(multiple-value-bind (result bundling-error)
    (ignore-errors (asdf:find-system sys-name))
  (unless result
    (format t "~%asdf:find-system failed to find system: ~S! Reason is: ~S ~A.~%"
            sys-name bundling-error bundling-error)
    (format t "asdf:*central-registry* = ~S~%" asdf:*central-registry*)
    (finish-output)
    (mkcl:quit :exit-code 1))
  (setq sys result))

;;(format t "~&Done with asdf:find-system.")(finish-output)

(defun asdf-system-depends-on (sys)
  (reverse (cdr (assoc 'asdf:load-op (asdf::component-depends-on 'asdf:load-op sys)))))

(let ((depends-on (asdf-system-depends-on sys))
      (version (ignore-errors (asdf:component-version sys)))
      (description (ignore-errors (asdf:system-description sys)))
      (long-description (ignore-errors (asdf:system-long-description sys)))
      (author (ignore-errors (asdf:system-author sys)))
      (maintainer (ignore-errors (asdf:system-maintainer sys)))
      (licence (ignore-errors (asdf:system-licence sys)))
      sys-attr
      )

  (when depends-on (setq sys-attr (list* :depends-on depends-on sys-attr)))
  (when version (setq sys-attr (list* :version version sys-attr)))
  (when description (setq sys-attr (list* :description description sys-attr)))
  (when long-description (setq sys-attr (list* :long-description long-description sys-attr)))
  (when author (setq sys-attr (list* :author author sys-attr)))
  (when maintainer (setq sys-attr (list* :maintainer maintainer sys-attr)))
  (when licence (setq sys-attr (list* :licence licence sys-attr)))

  (build-substitute-as2-file sys-name dest-dir sys-attr)
  )

(mkcl:quit :exit-code 0)

