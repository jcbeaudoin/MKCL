;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: SYSTEM -*-
;;;;
;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;  Copyright (c) 2012-2016, Jean-Claude Beaudoin.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Lesser General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 3 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../../Copyright' for full details.

;;;;	module routines

;; Parts of this were originally taken from SBCL's code/module.lisp which is in the public
;; domain.

(in-package "SYSTEM")

;;;; exported specials

(defvar *modules* ()
  "This is a list of module names that have been loaded into Lisp so far.
It is used by PROVIDE and REQUIRE.")

(defvar mkcl:*module-provider-functions* nil
  "See function documentation for REQUIRE")

;;;; PROVIDE and REQUIRE

(defun provide (module-name)
  "Adds a new module name to *MODULES* indicating that it has been loaded.
Module-name is a string designator"
  (pushnew (string module-name) *modules* :test #'string=)
  t)

(defvar *requiring* nil)

(defun require-error (control &rest arguments)
  (error "Module error: ~?" control arguments))

(defun require (module-name &optional pathnames)
  "Loads a module, unless it already has been loaded. PATHNAMES, if supplied,
is a designator for a list of pathnames to be loaded if the module
needs to be. If PATHNAMES is not supplied, functions from the list
MKCL:*MODULE-PROVIDER-FUNCTIONS* are called in order with MODULE-NAME
as an argument, until one of them returns non-NIL.  User code is
responsible for calling PROVIDE to indicate a successful load of the
module."
  (let ((name (string module-name)))
    (when (member name *requiring* :test #'string=)
      (require-error "~@<Could not ~S ~A: circularity detected. Please check ~
                     your configuration.~:@>" 'require module-name))
    (let ((saved-modules (copy-list *modules*))
	  (*requiring* (cons name *requiring*)))
      (unless (member name *modules* :test #'string=)
	(cond (pathnames
	       (unless (listp pathnames) (setf pathnames (list pathnames))) ;; for CLTL2 compatibility. JCB
	       ;; ambiguity in standard: should we try all pathnames in the
	       ;; list, or should we stop as soon as one of them calls PROVIDE?
               ;; CLTL2 says to load all in order. JCB
	       (dolist (ele pathnames t)
		 (load ele)))
	      (t
	       (unless (some 
			(lambda (p)
			  (handler-case
			      (funcall p module-name)
			    ((and condition (not warning)) (condition)
			      (require-error "Error while loading module ~A: ~A"
					     module-name condition))))
			mkcl:*module-provider-functions*)
		 (require-error "Don't know how to ~S ~A"
				'require module-name))
	       )))
      (set-difference *modules* saved-modules))))


(defun mkcl:default-module-provider (module)
  (flet ((try-load (path)
                   (handler-case
                    (load path :if-does-not-exist nil)
                    ((and condition (not warning)) (condition)
                     (error "Error loading file: ~A, Condition: ~A" path condition)))))
    (let* ((sysdir (translate-logical-pathname #P"SYS:"))
           (contribdir (translate-logical-pathname #P"CONTRIB:"))
           (module (string module)))
      (or
       (try-load (merge-pathnames (make-pathname :name module) sysdir))
       (try-load (merge-pathnames (make-pathname :name (string-downcase module)) sysdir))
       (try-load (merge-pathnames (make-pathname :name module) contribdir))
       (try-load (merge-pathnames (make-pathname :name (string-downcase module)) contribdir))
       ))))

(pushnew #'mkcl:default-module-provider mkcl:*module-provider-functions*)

