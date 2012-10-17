;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: SYSTEM -*-
;;;;
;;;;  MP.LSP  -- Multiprocessing capabilities.

;;;;  Copyright (c) 2003, Juan Jose Garcia-Ripoll
;;;;  Copyright (c) 2010-2012, Jean-Claude Beaudoin.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Lesser General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 3 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../../Copyright' for full details.

(in-package "MT")

(defmacro without-interrupts (&body body)
  (let ((were-enabled-sym (gensym "%lisp-interrupts-were-enabled-")))
    `(let (,were-enabled-sym)
       (macrolet ((maybe-with-interrupts (&body body)
		    `(unwind-protect
			 (progn
			   (if ,were-enabled-sym (si:enable-interrupts))
			   ,@body)
		       (if ,were-enabled-sym (si:disable-interrupts)))))
	 (unwind-protect
	     (progn 
	       (setq ,were-enabled-sym (si:disable-interrupts))
	       ,@body)
	   (if ,were-enabled-sym (si:enable-interrupts)))))))


(defmacro with-lock ((lock) &body body)
  (let ((lock-sym (gensym "%the-lock-"))
	(lock-acquired-sym (gensym "%lock-acquired-")))
    `(let ((,lock-sym ,lock)
	   (,lock-acquired-sym nil))
       (unwind-protect
	   (progn 
	     (without-interrupts 
	      (get-lock ,lock-sym)
	      (setq ,lock-acquired-sym t))
	     ,@body)
	 (without-interrupts
	  (when ,lock-acquired-sym
	    (giveup-lock ,lock-sym)))))))

(defmacro without-lock ((lock) &body body)
  (let ((lock-sym (gensym "%the-lock-"))
	(lock-released-sym (gensym "%lock-released-")))
    `(let ((,lock-sym ,lock)
	   (,lock-released-sym nil))
       (unwind-protect
	   (progn 
	     (without-interrupts 
	      (giveup-lock ,lock-sym)
	      (setq ,lock-released-sym t))
	     ,@body)
	 (without-interrupts
	  (when ,lock-released-sym
	    (get-lock ,lock-sym)))))))


(defmacro without-any-interrupts (&body body)
  `(without-interrupts 
    (unwind-protect
	(progn (block-signals)
	       ,@body)
      (unblock-signals))))


(export 'without-any-interrupts)


(defsetf thread-plist set-thread-plist)



