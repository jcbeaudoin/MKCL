;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: CLOS -*-
;;;;
;;;;  Copyright (c) 1992, Giuseppe Attardi.
;;;;  Copyright (c) 2010, Jean-Claude Beaudoin.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Lesser General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 3 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../../Copyright' for full details.

(in-package "CLOS")

(defmacro mapappend (fun &rest args)
  `(reduce #'append (mapcar ,fun ,@args)))

(defmacro ensure-up-to-date-instance (instance)
  ;; The up-to-date status of a class is determined by
  ;; instance.sig. This slot of the C structure contains a list of
  ;; slot definitions that was used to create the instance. When the
  ;; class is updated, the list is newly created. Structures are also
  ;; "instances" but have an instance.sig value of MKCL_UNBOUND instead of the list.
  `(let* ((i ,instance)
          (s (si::instance-sig i)))
     (declare (:read-only i s))
     (when (si:sl-boundp s) ;; rule structures out.
       (unless (or (eq s (class-slots (si::instance-class i)))
		   +inside-make-instance+) ;; prevent updates during make-instance calls. JCB
	 #+(or)
	 (format t "~&Need to update-instance ~S. s = ~S. slots = ~S.~%"
		 i s (class-slots (si::instance-class i)))
         (update-instance i)))
     i))
