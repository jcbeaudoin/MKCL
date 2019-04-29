;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: C -*-
;;;;
;;;;  Copyright (c) 2007, Juan Jose Garcia Ripoll.
;;;;  Copyright (c) 2012-2016, Jean-Claude Beaudoin.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Lesser General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 3 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../../Copyright' for full details.

;;;; CMPNAME Unambiguous init names for object files
;;;;
;;;; Every object file in a lisp library or combined FASL (such as the
;;;; compiler), needs a function that creates its data and installs the
;;;; functions. This initialization function has a C name which needs
;;;; to be unique. This file has functions to create such names.

(in-package "COMPILER")

(defvar *counter* 0)
(defconstant +standard-char-set-vector+ "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz")

(defun encode-number-in-name (number &aux (given-number number))
  ;; Encode a number in an alphanumeric identifier which is a valid C name.
  (cond ((zerop number) "0")
	((minusp number) (encode-number-in-name (- number)))
	(t
	 (let* ((code +standard-char-set-vector+)
	       (base (length code))
	       )
	   ;;(format t "~&encode-number-in-name: base = ~S, code = ~S.~%" base code)
	   (do* ((output '())
		 (digit 0))
		((zerop number) 
		 (let ((it (coerce (setq output (nreverse output)) 'base-string)))
		   ;;(format t "~&encode-number-in-name: ~S turned into ~S and coerced into ~S.~%" given-number output it)
		   ;;(format t "~&encode-number-in-name: base = ~S, code = ~S.~%" base code)
		   it
		   )
		 )
		(multiple-value-setq (number digit) (floor number base))
		;;(format t "~&encode-number-in-name: number = ~S, digit = ~S.~%" number digit)
		(push (char code digit) output)
		;;(format t "~&encode-number-in-name: (char code digit) = ~S.~%" (char code digit))
		;;(format t "~&encode-number-in-name: output = ~S.~%" output)
		)))))

(defun unique-init-name (file)
  "Create a unique name for this initialization function. The current algorithm
relies only on the name of the source file and the time at which it is built. This
should be enough to prevent name collisions for object files built in the same
machine."
  (let* ((path (pathname file))
	 (path-hash (logxor (ash (sxhash path) 8)
			    (ash (sxhash (cddr (pathname-directory path))) 16)
			    (sxhash (pathname-name path))))
	 (seconds (get-universal-time))
	 (ms (+ (* seconds 1000)
		(mod (floor (* 1000 (get-internal-real-time))
			    internal-time-units-per-second)
		     1000)))
	 (tag (concatenate 'base-string
			   "_mkcl"
			   (let ((it (encode-number-in-name path-hash)))
			     ;;(format t "~&unique-init-name: path-hash transformed into ~S.~%" it)
			     it)
			   "_"
			   (let ((it (encode-number-in-name ms)))
			     ;;(format t "~&unique-init-name: ms transformed into ~S.~%" it)
			     it))))
    (cmpnote "Creating tag: ~S for ~S" tag file)
    ;;(format t "~&unique-init-name: Creating tag: ~S (type ~S) for ~S~%" tag (type-of tag) file)
    ;;(finish-output)
    tag))

(defun init-name-tag (init-name)
  (concatenate 'base-string "@mKcLtAg" ":" init-name "@"))

(defun search-tag (stream tag)
  (do* ((eof nil)
	(key (concatenate 'list tag ":"))
	(string key))
       (nil)
    (let ((c (read-byte stream nil nil)))
      (cond ((null c) (return nil))
	    ((not (= c (char-code (pop string))))
	     (setf string key))
	    ((null string)
	     (return t))))))

(defun read-name (stream)
  ;;(format t "~&") ;; debug JCB
  (let* ((raw-name (loop with c = t
			 until (or (null (setf c (read-byte stream nil nil)))
				   (= c #.(char-code #\@)))
			 collect (code-char c)))
	 (name (concatenate 'string raw-name)))
    ;;(format t "found name: ~S, raw: ~S.~%" name raw-name) ;; debug JCB
    name))

(defun find-init-name (file &key (tag "@mKcLtAg"))
  "Search for the initialization function in an object file. Since the
initialization function in object files have more or less unpredictable
names, we store them in a string in the object file. This string is recognized
by the TAG it has at the beginning This function searches that tag and retrieves
the function name it precedes."
  (with-open-file (stream file :direction :input :element-type '(unsigned-byte 8))
    (cmpnote "Scanning ~S" file)
    (when (search-tag stream tag)
      (let ((name (read-name stream)))
	(cmpnote "Found tag: ~S for ~A" name file)
	name))))

(defun remove-prefix (prefix name)
  (if (equal 0 (search prefix name))
      (subseq name (length prefix) nil)
      name))

(defun guess-init-name (pathname on-missing-lisp-object-initializer &aux (kind (guess-object-file-kind pathname)))
  (if (eq kind :object)
    (or (and (mkcl:probe-file-p pathname)
	     (find-init-name pathname))
        (when on-missing-lisp-object-initializer
          (error on-missing-lisp-object-initializer
                 :format-control "Cannot find out entry point for binary file ~A"
                 :format-arguments (list pathname))))
    (compute-init-name pathname :kind kind)))

(defun init-function-name (s &key (kind :object))
  (flet ((translate-char (c)
	   (cond ((and (char>= c #\a) (char<= c #\z))
		  (char-upcase c))
		 ((and (char>= c #\A) (char<= c #\Z))
		  c)
		 ((or (eq c #\-) (eq c #\_))
		  #\_)
		 ((eq c #\*)
		  #\x)
		 ((eq c #\?)
		  #\a)
		 ((digit-char-p c)
		  c)
		 (t
		  #\p)))
	 (disambiguation (c)
	   (declare (ignore c))
	   (case kind
	     (:object "")
             ((:program :static-program) "exe_")
	     ((:fasl :fas) "fas_")
	     ((:library :shared-library :dll :static-library :lib) "lib_")
	     (otherwise (error "Not a valid argument to INIT-FUNCTION-NAME: kind = ~S"
			       kind)))))
    ;;(setq s (map 'string #'translate-char (string s)))
    (concatenate 'string
		 "mkcl_init_"
		 (disambiguation kind)
		 (map 'string #'translate-char (string s)))))


(defun compute-init-name (pathname &key (kind (guess-object-file-kind pathname)))
  (let ((filename (pathname-name pathname)))
    (case kind
      ((:object #|:c|#)
       (unique-init-name pathname))
      ((:fasl :fas :fasb)
       (init-function-name "CODE" :kind :fas))
      ((:static-library :lib :library)
       (init-function-name (remove-prefix +static-library-prefix+ filename) :kind :lib))
      ((:shared-library :dll)
       (init-function-name (remove-prefix +shared-library-prefix+ filename) :kind :dll))
      ((:program :static-program)
       "mkcl_init_PROGRAM")
      (otherwise
       (error "COMPILER::BUILDER cannot accept files of kind ~s" kind)))))

