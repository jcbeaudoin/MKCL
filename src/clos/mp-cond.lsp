;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: SYSTEM -*-
;;;;
;;;;  MP-COND.LSP  -- Multiprocessing conditions.

;;;;  Copyright (c) 2010-2012, Jean-Claude Beaudoin.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Lesser General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 3 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../../Copyright' for full details.



(in-package "MT")

(define-condition interrupt-error (error)
  ((thread :initarg :thread :reader interrupt-error-thread)))

(define-condition thread-sleeping (interrupt-error) 
  ((file :initarg :file :initform nil :reader thread-sleeping-in-file)
   (lineno :initarg :lineno :initform nil :reader thread-sleeping-on-lineno))
  (:report
   (lambda (condition stream)
     (format stream "Sleeping thread: ~S" (interrupt-error-thread condition))
     (when (thread-sleeping-in-file condition)
       (format stream ", in file ~A at line ~D"
	       (thread-sleeping-in-file condition)
	       (thread-sleeping-on-lineno condition))))))

(define-condition interrupt-refused (interrupt-error)
  ((file :initarg :file :initform nil :reader interrupt-disabler-file)
   (lineno :initarg :lineno :initform nil :reader interrupt-disabler-lineno))
  (:report
   (lambda (condition stream)
     (format stream "Interrupt refused in thread: ~S" (interrupt-error-thread condition))
     (when (interrupt-disabler-file condition)
       (format stream ", disabled in file ~A at line ~D"
	       (interrupt-disabler-file condition)
	       (interrupt-disabler-lineno condition))))))

(define-condition invalid-thread (error)
  ((thread :initarg :thread :reader invalid-thread-thread)
   (reason :initarg :reason :reader invalid-thread-reason)))

