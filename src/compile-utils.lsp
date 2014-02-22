;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: SYSTEM -*-
;;;;
;;;;  Copyright (c) 2010-2013, Jean-Claude Beaudoin.
;;;;  Copyright by a number of previous anonymous authors
;;;;            presumed to be the same as for the rest of MKCL.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Lesser General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 3 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

(defparameter *compile-extra-options* nil)
;;;
;;; If you want to debug the CMP compiler or the lisp part of the runtime
;;; then uncomment this.
;;;
;;(proclaim '(optimize (debug 1))) ;; faster, no debug info.
(proclaim '(optimize (debug 0))) ;; faster, no debug info.
#-(and)
(progn
  ;;(setq compiler::*compiler-break-enable* t) ;; enter debugger on compiler internal error
  (setq compiler::*delete-compiler-internal-files* nil)
  (setq *compile-extra-options* '(:c-file t :data-file t :h-file t))
  (proclaim '(optimize (debug 3))) ;; full debug info
  ;;(proclaim '(optimize (safety 3))) ;; full safety checks
  (setq compiler::*trace-cc* t)
  )
;;(setq *compile-verbose* t)

;;; -H traces include files in gcc.
;;(setq compiler::*cc-flags* (concatenate 'base-string "-H " compiler::*cc-flags*))


#+windows (setq *compile-extra-options* (append *compile-extra-options* '(:external-format (:ascii :lf))))

;;;
;;; * Add include path to not yet installed headers.

(setq compiler::*mkcl-include-directory* (truename (pathname ".")) ;; truename is needed by MS-Windows
      compiler::*mkcl-library-directory* (truename (pathname "."))
      )

;;;
;;;
;;;

(defun object-file-pathname (destdir source)
  (let* ((defaults (compile-file-pathname source :fasl-p nil #|:type :object|#))
	 (path (make-pathname :host (pathname-host destdir)
			      :device (pathname-device destdir)
			      :directory (pathname-directory destdir)
			      ;;:version nil
			      :defaults defaults)))
    #+(or)
    (format t "~&In object-file-pathname, in ~S from ~S to ~S to ~S.~%" destdir source defaults path)
    path))

(defun clean-up (destdir sources)
  (dolist (source sources)
    (let ((object (object-file-pathname destdir source)))
      (when (probe-file object)
	(handler-bind ((condition #'identity))
	  (format t "~&Removing: ~S~%" object) (finish-output)
	  (delete-file object))))
    )
  )

;;;
;;; * Timed compilation facility.
;;;
(defun compile-if-old (destdir sources &rest options)
  (unless (probe-file destdir)
    (si::mkdir destdir #o0777))
  (with-compilation-unit ()
    (mapcar #'(lambda (source &aux (orig-source source))
		#+(or)
		(format t "~&In compile-if-old in ~S for ~S~%" destdir source)
		(setq source (translate-logical-pathname source))
		(let ((object (object-file-pathname destdir source))
		      (*print-pretty* nil))
		  (unless (and (probe-file object)
			       (>= (file-write-date object) (file-write-date source))
			       (>= (file-write-date object) 
				   (file-write-date "./mkcl/mkcl-cmp.h")))
		    (format t "~&(compile-file ~S :output-file ~S~{ ~S~})~%"
			    source object (append options *compile-extra-options*))
		    (multiple-value-bind (output-truename warnings-p failure-p)
		        (apply #'compile-file source
			       :output-file object
			       :fasl-p nil
			       (append options *compile-extra-options*))
		      (declare (ignorable output-truename warnings-p))
		      (when failure-p
			(clean-up destdir sources)
			#+(or)
			(format t "~&Bailing out from compile-if-old!~%") (finish-output)
			(mkcl:quit :exit-code 1) ;; exit if compilation failed
			)
		      )
		    )
		  object))
	    sources)))


(defun build-substitute-as2-file (name system-attribs) ;; This is for ASDF 2
  (with-open-file (*standard-output* (make-pathname :name name :type "as2")
                   :direction :output :if-exists :supersede :if-does-not-exist :create)
    (pprint `(defsystem ,name
               :components ((:bundle ,(string name)))
	       ,@system-attribs
	       ))
    (terpri)))

(defun build-substitute-asd-file (name system-attribs) ;; This is for ASDF 3 (and later hopefully)
  (with-open-file (*standard-output* (make-pathname :name name :type "asd")
                   :direction :output :if-exists :supersede :if-does-not-exist :create)
    (format *standard-output* "(defsystem ~S~:*~%~
                                 ~12T:class ASDF/BUNDLE::PREBUILT-SYSTEM~%~
                                 ~12T:depends-on nil~%~
                                 ~12T:components ((:compiled-file ~S~:*))~%~
                                 ~12T:lib \"~A.a\"~%~
                                 ~12T~{ ~S~})~%" (string name) system-attribs)
    (terpri)))

(defun build-module (name sources &key
			  (builtin nil) ;; deprecated! JCB
			  (dir "")
			  ((:prefix si::*init-function-prefix*) "EXT")
			  &aux (*break-enable* t)
			  )
  (handler-bind 
     ((condition #'(lambda (c)
		     (unless (subtypep (type-of c) 'warning)
		       (format t "~&build-module failed on condition: ~A~%" c)
		       (break)
		       (finish-output)
		       (clean-up dir sources)
		       (format t "~&Bailing out from build-module condition handler!~%") (finish-output)
		       (mkcl:quit :exit-code 1)))))
   (let* ((name (string-downcase name)))
     (unless (or (equalp name "asdf") (equalp name "asdf2"))
       (build-substitute-as2-file name nil)
       (build-substitute-asd-file name nil))
     (if builtin
	 (let* ((objects (compile-if-old dir sources)))
	   (unless (compiler::build-static-library name :lisp-object-files objects)
	     (clean-up dir sources)
	     (mkcl:quit :exit-code 1))
	   )
       (let* ((objects (compile-if-old dir sources))
	      )
	 (let (result)
	   (format t "~&(compiler::build-bundle ~S :lisp-object-files ~S)" name objects)
	   (setq result (compiler::build-bundle name :lisp-object-files objects))
	   (unless result
	     (clean-up dir sources)
	     (format t "~&Bailing out from build-module fasl step!~%") (finish-output)
	     (mkcl:quit :exit-code 1)) ;; exit if fasl build failed.
	   )
	 #+unix
	 (progn
	   (format t "~&(compiler::build-static-library ~S :lisp-object-files ~S)" name objects)
	   (unless (compiler::build-static-library name :lisp-object-files objects)
	     (clean-up dir sources)
	     (format t "~&Bailing out from build-module static library step!~%") (finish-output)
	     (mkcl:quit :exit-code 1)))
	 (terpri))))))


