;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: SYSTEM -*-
;;;;
;;;;
;;;;
;;;;  Copyright (c) 2026, Jean-Claude Beaudoin.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Lesser General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 3 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.


(defun build-substitute-as2-file (asdf-module-name name destdir system-attribs) ;; This is for ASDF 2
  (with-open-file (*standard-output* (make-pathname :name asdf-module-name :type "as2" :defaults destdir)
                   :direction :output :if-exists :supersede :if-does-not-exist :create)
    (pprint `(defsystem ,asdf-module-name
               :components ((:bundle ,(string name)))
	       ,@system-attribs
	       ))
    (terpri)))

(defun build-substitute-asd-file (asdf-module-name name destdir system-attribs) ;; This is for ASDF 3 (and later hopefully)
  (with-open-file (*standard-output* (make-pathname :name asdf-module-name :type "asd" :defaults destdir)
                   :direction :output :if-exists :supersede :if-does-not-exist :create)
    (format *standard-output* "(defsystem ~S~%~
                                 ~12T:class ASDF/BUNDLE::PREBUILT-SYSTEM~%~
                                 ~12T:depends-on nil~%~
                                 ~12T:components ((:compiled-file ~S~:*))~%~
                                 ~12T:lib \"~A.a\"~%~
                                 ~12T~{ ~S~})~%" asdf-module-name (string name) system-attribs)
    (terpri)))


(defun build-module-asdf-support-file (name &key destdir (version nil))
  (let* ((asdf-module-name (string-downcase name)) ;; ASDF is hell-bent on downcasing system names by default.
         (system-attribs (when version (list :version version)))
         )
    (build-substitute-asd-file asdf-module-name name destdir system-attribs)))

(defun build-module-asdf2-support-file (name &key destdir (version nil))
   (let* ((asdf-module-name (string-downcase name)) ;; ASDF is hell-bent on downcasing system names by default.
          (system-attribs (when version (list :version version)))
          )
     (build-substitute-as2-file asdf-module-name name destdir system-attribs)))


;;;
