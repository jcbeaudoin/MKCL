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

;;(format t "~&Done with asdf setup.")(finish-output)

(defparameter sys-name (si:argv (incf arg-base)))

;;(format t "~&sys-name = ~S~%" sys-name)

(defparameter sys-dir (pathname (si:argv (incf arg-base))))

;;(format t "sys-dir = ~S~%" sys-dir)

(push '(mkcl:getcwd) asdf:*central-registry*)

(push sys-dir asdf:*central-registry*)

(defparameter sys nil)

;;(format t "~&About to call asdf:find-system.")(finish-output)

(setq si:*default-external-format* '(:utf-8 :lf))

;; handler-bind could be better than ignore-errors here. JCB
(unless (setq sys (ignore-errors (asdf:find-system sys-name)))
  (format t "~%asdf:find-system failed to find system: ~S~%" sys-name)
  (format t "asdf:*central-registry* = ~S~%" asdf:*central-registry*)
  (finish-output)
  (mkcl:quit :exit-code 1))

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

  (build-substitute-asd-file sys-name sys-attr)
  )

(unless (progn #|ignore-errors|# (asdf:bundle-system sys))
  (format t "~%asdf:bundle-system failed.~%")
  (finish-output)
  (mkcl:quit :exit-code 1))

(mkcl:quit :exit-code 0)

