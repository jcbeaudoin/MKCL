;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: SYSTEM -*-
;;;;
;;; -*- Package: PRETTY-PRINT -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
;;; CMU Common Lisp pretty printer.
;;; Written by William Lott.  Algorithm stolen from Richard Waters' XP.
;;;

(in-package "SI")


(defmacro pprint-logical-block
	  ((stream-symbol object &key (prefix "" prefix-p)
			  (per-line-prefix "" per-line-prefix-p)
			  (suffix "" suffix-p))
	   &body body)
  "Group some output into a logical block.  STREAM-SYMBOL should be either a
   stream, T (for *TERMINAL-IO*), or NIL (for *STANDARD-OUTPUT*).  The printer
   control variable *PRINT-LEVEL* is automatically handled."
  (when per-line-prefix-p
    (when prefix-p
      (error "Cannot specify both a prefix and a per-line-prefix."))
    (setf prefix per-line-prefix))
  (let* ((object-var (gensym))
	 (block-name (gensym "PPRINT-LOGICAL-BLOCK-"))
	 (count-name (gensym "PPRINT-LOGICAL-BLOCK-LENGTH-"))
	 (stream-var (case stream-symbol
		       ((nil) '*standard-output*)
		       ((t) '*terminal-io*)
		       (t stream-symbol)))
	 (function
	  `(si::lambda-block ,block-name (,object-var ,stream-var
					   &aux (,count-name 0))
			      (declare (ignorable ,object-var ,count-name))
	    (macrolet ((pprint-pop ()
			 '(progn
			   (unless (pprint-pop-helper ,object-var ,count-name
						      ,stream-var)
			     (return-from ,block-name nil))
			   (incf ,count-name)
			   ,(if object `(pop ,object-var) nil)))
		       (pprint-exit-if-list-exhausted ()
			 ,(if object
			      `'(when (null ,object-var)
				 (return-from ,block-name nil))
			      `'(return-from ,block-name nil))))
	      ,@body))))
      `(pprint-logical-block-helper #',function ,object ,stream-symbol
				    ,prefix ,per-line-prefix-p ,suffix)))

(defmacro pprint-exit-if-list-exhausted ()
  "Cause the closest enclosing use of PPRINT-LOGICAL-BLOCK to return
   if it's list argument is exhausted.  Can only be used inside
   PPRINT-LOGICAL-BLOCK, and only when the LIST argument to
   PPRINT-LOGICAL-BLOCK is supplied."
  (error "PPRINT-EXIT-IF-LIST-EXHAUSTED must be lexically inside ~
	  PPRINT-LOGICAL-BLOCK."))

(defmacro pprint-pop ()
  "Return the next element from LIST argument to the closest enclosing
   use of PPRINT-LOGICAL-BLOCK, automatically handling *PRINT-LENGTH*
   and *PRINT-CIRCLE*.  Can only be used inside PPRINT-LOGICAL-BLOCK.
   If the LIST argument to PPRINT-LOGICAL-BLOCK was NIL, then nothing
   is poped, but the *PRINT-LENGTH* testing still happens."
  (error "PPRINT-POP must be lexically inside PPRINT-LOGICAL-BLOCK."))
