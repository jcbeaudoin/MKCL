;;;
;;;

(terpri) (princ ";;; Generating supplemental character encoding external support files.") (terpri)

(si::pathname-translations "BUILD" `(("**;*.*.*" "./**/*.*")))

(defvar *generate-verbose*)
(setq *generate-verbose* nil) ;; Makes the generate.lisp script more verbose.

(defvar *update-mapping*)
(setq *update-mapping* nil) ;; Setting this to something other than nil may result in network activity.

#+UNICODE
(load "../contrib/encodings/generate.lisp" :external-format '(:ascii :lf))

(terpri)

(mkcl:quit :exit-code 0) ;; signal to "make" that all is well.

