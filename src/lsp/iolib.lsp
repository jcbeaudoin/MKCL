;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: SYSTEM -*-
;;;;
;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;  Copyright (c) 2011-2014, Jean-Claude Beaudoin.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Lesser General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 3 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../../Copyright' for full details.
;;;;        The IO library.

(in-package "SYSTEM")

(defmacro with-open-stream ((var stream) &rest body)
  "Syntax: (with-open-stream (var stream-form) {decl}* {form}*)
Evaluates FORMs with VAR bound to the value of STREAM-FORM.  The stream is
automatically closed on exit."
  (multiple-value-bind (ds b)
      (find-declarations body)
    `(LET ((,var ,stream))
       ,@ds
       (UNWIND-PROTECT
         (PROGN ,@b)
         (CLOSE ,var)))))

(defmacro with-input-from-string ((var string &key index (start 0) end (encoding :default)) &rest body)
  "Syntax: (with-input-from-string (var string-form {keyword value}*)
           {decl}* {form}*)
Evaluates FORMs with VAR bound to a string input stream from the string that
is the value of STRING-FORM.  The stream is automatically closed on exit.
Possible keywords are :INDEX, :START, and :END."
  (if index
      (multiple-value-bind (ds b)
          (find-declarations body)
        `(LET ((,var (MAKE-STRING-INPUT-STREAM ,string ,start ,end :encoding ,encoding)))
           ,@ds
           (UNWIND-PROTECT
             (MULTIPLE-VALUE-PROG1
	      (PROGN ,@b)
	      (SETF ,index (FILE-POSITION ,var)))
             (CLOSE ,var))))
      `(LET ((,var (MAKE-STRING-INPUT-STREAM ,string ,start ,end :encoding ,encoding)))
         ,@body)))

(defmacro with-output-to-string ((var &optional string &rest r &key element-type (encoding :default)) &rest body)
  "Syntax: (with-output-to-string (var [string-form]) {decl}* {form}*)
Evaluates FORMs with VAR bound to a string output stream to the string that is
the value of STRING-FORM.  If STRING-FORM is not given, a new string is used.
The stream is automatically closed on exit and the string is returned."
  (if string
      `(LET* ((,var (MAKE-STRING-OUTPUT-STREAM-FROM-STRING ,string ,encoding))
	      (,(gensym) ,element-type))
	;; We must evaluate element-type if it has been supplied by the user.
	;; Even if we ignore the value afterwards.
         ,@body)
      `(LET ((,var (MAKE-STRING-OUTPUT-STREAM ,@r)))
         ,@body
         (GET-OUTPUT-STREAM-STRING ,var))))

(defun read-from-string (string
                         &optional (eof-error-p t) eof-value
                         &key (start 0) (end (length string))
			 preserve-whitespace (encoding :default))
  "Args: (string &optional (eof-error-p t) (eof-value nil)
              &key (start 0) (end (length string)) (preserve-whitespace nil))
Reads an object from STRING and returns the object.  As the second value,
returns the index to the character next to the object's representation.
PRESERVE-WHITESPACE specifies whether to leave the character next to the
object's representation."
  (let ((stream (make-string-input-stream string start end :encoding encoding)))
    (if preserve-whitespace
        (values (read-preserving-whitespace stream eof-error-p eof-value)
                (file-position stream))
        (values (read stream eof-error-p eof-value)
                (file-position stream)))))

(defun write-to-string (object &rest rest
                        &aux (stream (make-string-output-stream)))
  "Args: (object &key (escape *print-escape*) (radix *print-radix*)
                   (base *print-base*) (circle *print-circle*)
                   (pretty *print-pretty*) (level *print-level*)
                   (length *print-length*) (case *print-case*)
                   (array *print-array*) (gensym *print-gensym*))
Returns as a string the printed representation of OBJECT in the specified
mode.  See the variable docs of *PRINT-...* for the mode."
  (apply #'write object :stream stream rest)
  (get-output-stream-string stream))

(defun prin1-to-string (object
                        &aux (stream (make-string-output-stream)))
  "Args: (object)
PRIN1s OBJECT to a new string and returns the result.  Equivalent to
(WRITE-TO-STRING OBJECT :ESCAPE T)."
   (prin1 object stream)
   (get-output-stream-string stream))

(defun princ-to-string (object
                        &aux (stream (make-string-output-stream)))
  "Args: (object)
PRINCs OBJECT to a new string and returns the result.  Equivalent to
(WRITE-TO-STRING OBJECT :ESCAPE NIL)."
  (princ object stream)
  (get-output-stream-string stream))

(defun mkcl:write-to-base-string (object &rest rest &key (encoding :default) &allow-other-keys)
  "Args: (object &key (escape *print-escape*) (radix *print-radix*)
                   (base *print-base*) (circle *print-circle*)
                   (pretty *print-pretty*) (level *print-level*)
                   (length *print-length*) (case *print-case*)
                   (array *print-array*) (gensym *print-gensym*)
                   (encoding :default))
Returns as a base-string the printed representation of OBJECT in the specified
mode.  See the variable docs of *PRINT-...* for the mode."
  (let ((stream (make-string-output-stream :element-type 'base-char :encoding encoding)))
    (remf rest :encoding)
    (apply #'write object :stream stream rest)
    (get-output-stream-string stream)))

(defun mkcl:prin1-to-base-string (object &key (encoding :default))
  "Args: (object &key (encoding :default))
PRIN1s OBJECT to a new base-string and returns the result.
Equivalent to (WRITE-TO-STRING OBJECT :ESCAPE T)."
  (let ((stream (make-string-output-stream :element-type 'base-char :encoding encoding)))
    (prin1 object stream)
    (get-output-stream-string stream)))

(defun mkcl:princ-to-base-string (object &key (encoding :default))
  "Args: (object &key (encoding :default))
PRINCs OBJECT to a new base-string and returns the result.
Equivalent to (WRITE-TO-STRING OBJECT :ESCAPE NIL)."
  (let ((stream (make-string-output-stream :element-type 'base-char :encoding encoding)))
    (princ object stream)
    (get-output-stream-string stream)))


(defmacro with-open-file ((stream . filespec) &rest body)
  "Syntax: (with-open-file (var filespec-form {options}*) {decl}* {form}*)
Opens the specified file using OPTIONs, and evaluates FORMs with VAR bound to
a stream to/from the file.  The file is automatically closed on exit.  See
OPEN for the options."
  (multiple-value-bind (ds b)
      (find-declarations body)
    `(LET ((,stream (OPEN ,@filespec)))
       ,@ds
       (UNWIND-PROTECT
         (MULTIPLE-VALUE-PROG1 (PROGN ,@b) (WHEN ,stream (CLOSE ,stream)))
         (WHEN ,stream (CLOSE ,stream :ABORT T))))))

(defun y-or-n-p (&optional string &rest args)
  "Args: (&optional format-string &rest args)
Asks the user a Y-or-N question.  Does FRESH-LINE, prints a message as if
FORMAT-STRING and ARGs were given to FORMAT, and then prints \"(Y or N)\" is
printed.  If FORMAT-STRING is NIL, however, no prompt will appear."
  (do ((reply))
      (nil)
    (when string (format *query-io* "~&~?  (Y or N) " string args))
    (setq reply (read *query-io*))
    (cond ((string-equal (symbol-name reply) "Y")
           (return-from y-or-n-p t))
          ((string-equal (symbol-name reply) "N")
           (return-from y-or-n-p nil)))))

(defun yes-or-no-p (&optional string &rest args)
  "Args: (&optional format-string &rest args)
Asks the user an YES-or-NO question.  Does FRESH-LINE, prints a message as if
FORMAT-STRING and ARGs were given to FORMAT, and then prints \"(Y or N)\" is
printed.  If FORMAT-STRING is NIL, however, no prompt will appear."
  (do ((reply))
      (nil)
    (when string (format *query-io* "~&~?  (Yes or No) " string args))
    (setq reply (read *query-io*))
    (cond ((string-equal (symbol-name reply) "YES")
           (return-from yes-or-no-p t))
          ((string-equal (symbol-name reply) "NO")
           (return-from yes-or-no-p nil)))))

(defun sharp-a-reader (stream subchar arg)
  (declare (ignore subchar))
  (let ((initial-contents (read-preserving-whitespace stream nil nil t)))
    (cond
      (*read-suppress* nil)
      ((null arg)
        ;; readably-pretty-printed array: #A(type dims initial-contents)
        (let ((elt-type (car initial-contents))
	      (dims (cadr initial-contents))
	      (initial-contents (caddr initial-contents)))
	  (make-array dims :element-type elt-type :initial-contents initial-contents)))
      (t
        (do* ((i 0 (1+ i))
	      (d nil (cons (length ic) d))
	      (ic initial-contents (if (zerop (length ic)) ic (elt ic 0))))
            ((>= i arg)
             (make-array (nreverse d) :initial-contents initial-contents))
	  (declare (fixnum i)))))))

(set-dispatch-macro-character #\# #\a 'sharp-a-reader)
(set-dispatch-macro-character #\# #\A 'sharp-a-reader)

(defun sharp-s-reader (stream subchar arg)
  (declare (ignore subchar))
  (when (and arg (null *read-suppress*))
        (error "~S is an extra argument for the #s readmacro." arg))
  (let ((l (read-preserving-whitespace stream)))
    (when *read-suppress*
      (return-from sharp-s-reader nil))
    (unless (get-sysprop (car l) 'is-a-structure)
            (error "~S is not a structure." (car l)))
    ;; Intern keywords in the keyword package.
    (do ((ll (cdr l) (cddr ll)))
        ((endp ll)
         ;; Find an appropriate construtor.
         (do ((cs (get-sysprop (car l) 'structure-constructors) (cdr cs)))
             ((endp cs)
              (error "The structure ~S has no structure constructor."
                     (car l)))
           (when (symbolp (car cs))
                 (return (apply (car cs) (cdr l))))))
      (rplaca ll (intern (string (car ll)) 'keyword)))))

(set-dispatch-macro-character #\# #\s 'sharp-s-reader)
(set-dispatch-macro-character #\# #\S 'sharp-s-reader)

#|
(defvar *dribble-stream* nil)
(defvar *dribble-io* nil)
(defvar *dribble-namestring* nil)
(defvar *dribble-saved-terminal-io* nil)

(defun dribble (&optional (pathname "DRIBBLE.LOG" psp)) ;; Old (and broken) version of dribble. Worked back in the CLTL1 days.
  "Args: (&optional filespec)
If FILESPEC is given, starts recording the interaction to the specified file.
FILESPEC may be a symbol, a string, a pathname, or a file stream.  If FILESPEC
is not given, ends the recording."
  (cond ((not psp)
         (when (null *dribble-stream*) (error "Not in dribble."))
         (if (eq *dribble-io* *terminal-io*)
             (setq *terminal-io* *dribble-saved-terminal-io*)
             (warn "*TERMINAL-IO* was rebound while DRIBBLE is on.~%~
                   You may miss some dribble output."))
         (close *dribble-stream*)
         (setq *dribble-stream* nil)
         (format t "~&Finished dribbling to ~A." *dribble-namestring*))
        (*dribble-stream*
         (error "Already in dribble (to ~A)." *dribble-namestring*))
        (t
         (let* ((namestring (namestring pathname))
                (stream (open pathname :direction :output
                                       :if-exists :supersede
                                       :if-does-not-exist :create)))
           (setq *dribble-namestring* namestring
                 *dribble-stream* stream
                 *dribble-saved-terminal-io* *terminal-io*
                 *dribble-io* (make-two-way-stream
                               (make-echo-stream *terminal-io* stream)
                               (make-broadcast-stream *terminal-io* stream))
                 *terminal-io* *dribble-io*)
           (multiple-value-bind (sec min hour day month year)
               (get-decoded-time)
             (format t "~&Start dribbling to ~A (~d/~d/~d, ~d:~d:~d)."
                     namestring year month day hour min sec))))))
|#

(defun dribble (&optional (dribble-pathname "DRIBBLE.LOG" dribble-on-p)) ;; This version of dribble is recursive. JCB
  "Args: (&optional filespec)
If FILESPEC is given, starts recording the interaction to the specified file.
FILESPEC may be a symbol, a string, a pathname, or a file stream.  If FILESPEC
is not given, ends the recording."

  (cond (dribble-on-p
         (let* ((dribble-namestring (namestring dribble-pathname))
                (dribble-stream (open dribble-pathname :direction :output
				      :if-exists :supersede
				      :if-does-not-exist :create))
		;; Common Lisp set of 7 basic IO streams.
		(standard-input *standard-input*)
		(standard-output *standard-output*)
		(error-output *error-output*)
		(trace-output *trace-output*)
		(terminal-io *terminal-io*)
		(query-io *query-io*)
		(debug-io *debug-io*)
		;; Closer closures are link-stacked through this binding.
		(dribble-closer si::*dribble-closer*)
		)
	   (let ((new-standard-input (make-echo-stream standard-input dribble-stream))
		 (new-standard-output (make-broadcast-stream standard-output dribble-stream))
		 (new-error-output (make-broadcast-stream error-output dribble-stream))
		 (new-trace-output (make-broadcast-stream trace-output dribble-stream))
		 (new-terminal-io (make-two-way-stream (make-echo-stream terminal-io dribble-stream)
						       (make-broadcast-stream terminal-io dribble-stream)))
		 (new-query-io (if (and (typep query-io 'synonym-stream) (eq '*terminal-io* (synonym-stream-symbol query-io)))
				   query-io ;; terminal-io is already dribbling, no need to do more.
				 (make-two-way-stream (make-echo-stream query-io dribble-stream)
						    (make-broadcast-stream query-io dribble-stream))))
		 (new-debug-io (if (and (typep debug-io 'synonym-stream) (eq '*terminal-io* (synonym-stream-symbol debug-io)))
				   debug-io ;; terminal-io is already dribbling, no need to do more.
				 (make-two-way-stream (make-echo-stream debug-io dribble-stream)
						    (make-broadcast-stream debug-io dribble-stream))))
		 )
	     (setq si::*dribble-closer* ;; push on the dribble stack.
		   #'(lambda ()
		       (ignore-errors
			(format t "~&;;; End of dribbling to ~A.~%" dribble-namestring)
			(finish-output *standard-output*)
			(finish-output *error-output*)
			(finish-output *trace-output*)
			(finish-output *terminal-io*)
			(finish-output *query-io*)
			(finish-output *debug-io*)
			(close dribble-stream)
			)
		       ;; Should we check for input/output stream rebinding?
		       (setq *standard-input* standard-input
			     *standard-output* standard-output
			     *error-output* error-output
			     *trace-output* trace-output
			     *terminal-io* terminal-io
			     *query-io* query-io
			     *debug-io* debug-io
			     ;; pop the dribble stack.
			     si::*dribble-closer* dribble-closer)))
	     ;; Now that all memory allocation for this dribbling is done, and
	     ;; that no more exceptions can be raised, we force redirection.
	     (setq *standard-input* new-standard-input
		   *standard-output* new-standard-output
		   *error-output* new-error-output
		   *trace-output* new-trace-output
		   *terminal-io* new-terminal-io
		   *query-io* new-query-io
		   *debug-io* new-debug-io)
	     )
		  
	   (multiple-value-bind (sec min hour day month year)
               (get-decoded-time)
             (format t "~&;;; Dribbling to ~A started on (~d/~d/~d, ~d:~d:~d).~%"
                     dribble-namestring year month day hour min sec))
	   (finish-output)
	   )
	 )
	(t (when si::*dribble-closer* (funcall si::*dribble-closer*)))
	)
  (values)
  )

(defmacro with-standard-io-syntax (&body body)
  "Syntax: ({forms}*)
The forms of the body are executed in a print environment that corresponds to
the one defined in the ANSI standard. *print-base* is 10, *print-array* is t,
*package* is \"CL-USER\", etc."
  `(let*((*package* (find-package :cl-user))
	 (*print-array* t)
	 (*print-base* 10)
	 (*print-case* :upcase)
	 (*print-circle* nil)
	 (*print-escape* t)
	 (*print-gensym* t)
	 (*print-length* nil)
	 (*print-level* nil)
	 (*print-lines* nil)
	 (*print-miser-width* nil)
         (*print-pprint-dispatch* (copy-pprint-dispatch nil))
	 (*print-pretty* nil)
	 (*print-radix* nil)
	 (*print-readably* t)
	 (*print-right-margin* nil)
	 (*read-base* 10)
	 (*read-default-float-format* 'single-float)
	 (*read-eval* t)
	 (*read-suppress* nil)
	 (*readtable* (copy-readtable (si::standard-readtable))))
    ,@body))


(defun print-unreadable-object-function (object stream type identity function)
  (if *print-readably*
    (error 'print-not-readable :object object)
    (when (and *print-level* (zerop *print-level*))
      (write-string "#" stream)
      (return-from print-unreadable-object-function nil)))
  (write-string "#<" stream)
  (when type
    (prin1 (type-of object) stream)
    (write-string " " stream))
  (when function (funcall function))
  (when identity
    (when (or function (not type)) (write-string " " stream))
    (princ (si:pointer object) stream))
  (write-string ">" stream)
  nil)

(defmacro print-unreadable-object
	  ((object stream &key type identity) &body body)
  (if body
      `(flet ((.print-unreadable-object-body. () ,@body))
	 (print-unreadable-object-function
	   ,object ,stream ,type ,identity #'.print-unreadable-object-body.))
    `(print-unreadable-object-function ,object ,stream ,type ,identity nil)))

(let* ((basic-encodings
        #+unicode
         '(:UTF-8
	   :UTF-16 :UTF-16BE :UTF-16LE
	   :UTF-32 :UTF-32BE :UTF-32LE
           :ISO-8859-1 :US-ASCII :DEFAULT)
         #-unicode
         '(:DEFAULT))
       (all-encodings nil))
  (defun all-encodings ()
    (or all-encodings
        (progn
          (setf all-encodings basic-encodings)
          #+unicode
	  (progn
	    (dolist (i (directory "SYS:ENCODINGS;*"))
	      (push (intern (pathname-name i) "KEYWORD") all-encodings))
	    (dolist (i (directory "SYS:ENCODINGS;*.BIN"))
	      (push (intern (pathname-name i) "KEYWORD") all-encodings)))
          all-encodings))))

(defun load-encoding (name)
  #-unicode
  (values nil (format nil "Cannot load encoding ~A because this MKCL instance does not have Unicode support" name))
  #+unicode
  (let ((filename (make-pathname :name (symbol-name name) :defaults "SYS:ENCODINGS;")))
    (cond ((mkcl:probe-file-p filename)
	   (load filename :verbose nil)
	   name)
	  ((mkcl:probe-file-p (setf filename (make-pathname :type "BIN" :defaults filename)))
	   (with-open-file (in filename :element-type '(unsigned-byte 16)
			       :external-format :big-endian)
	     (let* ((l (read-byte in))
		    (s (make-array l :element-type '(unsigned-byte 16) :initial-element 0)))
	       (read-sequence s in)
	       s)))
	  (t
	   (values nil (format nil "Unable to find mapping file ~A for encoding ~A" filename name))))))

(defun make-encoding (mapping)
  #-unicode
  (values nil (format nil "Not a valid external format ~A" mapping))
  #+unicode
  (cond
    ((symbolp mapping)
     (if (member mapping
		 #+unicode '(:UTF-8 :UTF-16 :UTF-16BE :UTF-16LE :UTF-32 :UTF-32BE :UTF-32LE :ISO-8859-1 :US-ASCII :DEFAULT)
		 #-unicode '(:DEFAULT)
		 :test #'string=)
	 (values (intern (symbol-name mapping) keyword-package #|(find-package "KEYWORD")|#)) ;; This is a built-in mapping.
       (let* ((mk-ext-pkg (find-package "MK-EXT"))
	      (var (find-symbol (symbol-name mapping) mk-ext-pkg)))
	 (unless var
	   (let ((mk-ext-was-closed (si:package-closed-p mk-ext-pkg))
		 encoding)
	     (unwind-protect
		 (progn
		   (when mk-ext-was-closed (reopen-package mk-ext-pkg))
		   (setq var (intern (symbol-name mapping) mk-ext-pkg))
		   (multiple-value-bind (array-map failure-reason)
		       (load-encoding mapping)
		     (when array-map
			 (multiple-value-setq (encoding failure-reason) (make-encoding array-map)))
                     (unless encoding
                       (unintern var mk-ext-pkg)
		       (return-from make-encoding (values nil failure-reason))))
		   (set var encoding)
		   )
	       (when mk-ext-was-closed (close-package mk-ext-pkg)))))
	 (symbol-value var))))
    ((consp mapping)
     (let ((output (make-hash-table :size 512 :test 'eq)))
       (dolist (record mapping output)
	 (let* ((byte (car record))
		(unicode (cdr record))
		(unicode-char (code-char unicode)))
	   (when (> byte #xFF)
	     (setf (gethash (ash byte -8) output) t))
	   (setf (gethash byte output) unicode-char)
	   (setf (gethash unicode-char output) byte)))))
    ((arrayp mapping)
      (do* ((l (array-total-size mapping))
	    (output (make-hash-table :size (floor (* 1.5 l)) :test 'eq))
	    (i 0 (+ 2 i)))
	   ((>= i l) output)
	(let* ((byte (aref mapping i))
	       (unicode (aref mapping (1+ i)))
	       (unicode-char (code-char unicode)))
	  (when (> byte #xFF)
	    (setf (gethash (ash byte -8) output) t))
	  (setf (gethash byte output) unicode-char)
	  (setf (gethash unicode-char output) byte))))
    (t
     (values nil (format nil "Not a valid external format ~A" mapping)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;

;;(in-package "SYSTEM")

(defun   logical-pathname-translations (p) (si:pathname-translations p))
(defsetf logical-pathname-translations si:pathname-translations)

(defun load-logical-pathname-translations (host)
  "Search for a logical pathname named host, if not already defined. If already
defined no attempt to find or load a definition is attempted and NIL is
returned. If host is not already defined, but definition is found and loaded
successfully, T is returned, else error."
  (declare (type string host))
  (let ((*autoload-translations* nil))
    (unless (or (string-equal host "sys")
                (si::pathname-translations host))
      (with-open-file (in-str (make-pathname :defaults (translate-logical-pathname #P"SYS:")
                                             :name (string-downcase host)
                                             :type "translations"))
        (if *load-verbose*
            (format *error-output*
                    ";; Loading pathname translations from ~A~%"
                    (namestring (truename in-str))))
        (setf (logical-pathname-translations host) (read in-str)))
      t)))


(defun ensure-directories-exist (pathname &key verbose)
"Args: (ensure-directories pathname &key :verbose)
Creates tree of directories specified by the given pathname. Outputs
	(VALUES pathname created)
where CREATED is true only if we succeeded on creating all directories."
  (let* ((created nil)
	 (full-pathname (merge-pathnames pathname))
	 d)
    (when (or (wild-pathname-p full-pathname :directory)
	      (wild-pathname-p full-pathname :host)
	      (wild-pathname-p full-pathname :device))
      (error 'file-error :pathname pathname))
    (dolist (item (pathname-directory full-pathname))
      (setf d (nconc d (list item)))
      (let ((p (make-pathname :name nil :type nil :directory d
						  :defaults full-pathname)))
	;; (unless (or (symbolp item) (si::file-kind p nil))
	;;   (setf created t)
	;;   (when verbose
	;;     (format t "~%;;; Making directory ~A" p))
	;;   (si::mkdir p #o777))
	(unless (symbolp item)
	  (if (mkcl:probe-file-p p)
	      (unless (eq :directory (si::file-kind p :follow-symlinks t))
		(error 'file-error :pathname p :format-control "~S is not a directory" :format-arguments (list item)))
	    (progn
	      (when verbose
		(format t "~%;;; Making directory ~A" p))
	      (si::mkdir p #o777)
	      (setf created t))))
	))
    (values pathname created)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "MKCL")

(defun directory-p (pathspec)
  (setq pathspec (pathname pathspec))
  (let ((name (pathname-name pathspec))
	(type (pathname-type pathspec))
	#-(or unix windows) (version (pathname-version pathspec)) ;; who cares about versions anymore.
	#-unix (device (pathname-device pathspec)))
    (and (or (null name) (eq name :unspecific))
	 (or (null type) (eq type :unspecific))
	 #-(or unix windows) (or (null version) (eq version :unspecific)) ;; who cares about versions anymore.
	 #-unix (and device (not (eq device :unspecific))) ;; on Unix the device never matters.
	 (pathname-host pathspec)
	 (consp (pathname-directory pathspec)))))


(defun copy-pathname (pathspec)
  (if (pathnamep pathspec)
      (make-pathname :defaults pathspec)
    (pathname pathspec)))

(defun pathname+ (pathspec &key
			   (host nil hostp) (device nil devicep)
			   (directory nil directoryp) (root-directory nil rootp)
			   (name nil namep) (type nil typep) (version nil versionp))
  (let ((path (pathname pathspec)))
    (let ((host (if hostp host (pathname-host path)))
	  (device (if devicep device (pathname-device path)))
	  (directory (if directoryp directory (pathname-directory path)))
	  (name (if namep name (pathname-name path)))
	  (type (if typep type (pathname-type path)))
	  (version (if versionp version (pathname-version path))))
      (when (and rootp (consp root-directory))
	(if (and (consp directory) (eq :relative (car directory)))
	    (setq directory (append root-directory (cdr directory)))
	  (setq directory root-directory)))
      (make-pathname :host host :device device :directory directory :name name :type type :version version))))

(defun absolute-logical-pathname (pathspec &optional cwd)
  (let ((path (if (pathnamep pathspec) pathspec (pathname pathspec))))
    (when (pathname-complete-p path)
      (return-from absolute-logical-pathname path)) ;; already an absolute pathname

    (let ((host (pathname-host path))
	  (directory (pathname-directory path))
	  (cwd (or cwd mkcl:*current-working-directory*))
	  (acwd mkcl:*all-current-working-directories*)
	  wd-host
	  root-dir
	  )

      (when (and (consp directory) (eq :absolute (car directory)))
	(return-from absolute-logical-pathname path)) ;; already an absolute pathname

      (if host
	  (if (and cwd (progn (setq wd-host (pathname-host cwd))
			      (and (logical-pathname-p cwd) (stringp wd-host) (string-equal host wd-host))))
	      (setq root-dir (pathname-directory cwd))
	    (dolist (wd acwd nil)
	      (setq wd-host (pathname-host wd))
	      (when (and (logical-pathname-p wd) (stringp wd-host) (string-equal host wd-host))
		(setq root-dir (pathname-directory wd))
		(return))))
	(if (and cwd (progn (setq wd-host (pathname-host cwd))
			    (and (logical-pathname-p cwd) (stringp wd-host))))
	    (setq root-dir (pathname-directory cwd))
	  (dolist (wd acwd nil)
	    (setq wd-host (pathname-host wd))
	    (when (and (logical-pathname-p wd) (stringp wd-host))
	      (setq root-dir (pathname-directory wd))
	      (return)))))

      (cond ((consp root-dir)
	     (if (and (consp directory) (eq :relative (car directory)))
		 (setq directory (append root-dir (cdr directory)))
	       (unless directory
		 (setq directory root-dir))))
	    ((stringp root-dir)
	     (if (and (consp directory) (eq :relative (car directory)))
		 (setq directory (list* :absolute root-dir (cdr directory)))
	       (unless directory
		 (setq directory (list :absolute root-dir))))))

      (make-pathname :host host :directory directory :defaults path))))

(defun absolute-pathname-p (pathspec)
  (let ((path (if (pathnamep pathspec) pathspec (pathname pathspec))))
    (when (pathname-complete-p path)
      (return-from absolute-pathname-p path)) ;; already an absolute pathname

    (let ((host (pathname-host path))
	  (device (pathname-device path))
	  (directory (pathname-directory path)))
      (when (and host (stringp device)
		 (or (and (consp directory) (eq :absolute (car directory)))
		     (stringp directory)))
	(return-from absolute-pathname-p path)))))

(defun relative-pathname-p (pathspec)
  (let ((path (if (pathnamep pathspec) pathspec (pathname pathspec))))
    (when (pathname-complete-p path)
      (return-from relative-pathname-p nil)) ;; an absolute pathname

    (let ((directory (pathname-directory path)))
      (when (or (and (consp directory) (eq :relative (car directory)))
		(null directory))
	(return-from relative-pathname-p path)))))


(defun absolute-pathname (pathspec &optional cwd)
  (let ((path (if (pathnamep pathspec) pathspec (pathname pathspec))))
    (when (pathname-complete-p path)
      (return-from absolute-pathname path)) ;; already an absolute pathname

    (when (logical-pathname-p path)
      (return-from absolute-pathname (absolute-logical-pathname path cwd)))

    (let ((host (pathname-host path))
	  (device (pathname-device path))
	  (directory (pathname-directory path))
	  (cwd (or cwd mkcl:*current-working-directory*))
	  (acwd mkcl:*all-current-working-directories*)
	  wd-host
	  wd-device
	  root-dir
	  )

      (when (and host (stringp device) (consp directory) (eq :absolute (car directory)))
	(return-from absolute-pathname path)) ;; already an absolute pathname

      (when (and (stringp host) (string= host "localhost")
		 (not (stringp device))
		 (or (null directory) (and (consp directory) (eq :relative (car directory)))))
	(setq host nil))

      (if host
	  (cond ((eq device :unspecific)
		 (if (and cwd (progn (setq wd-host (pathname-host cwd)
					   wd-device (pathname-device cwd))
				     (and (stringp wd-host) (string= host wd-host)
					  (eq device wd-device))))
		     (setq root-dir (pathname-directory cwd))
		   (dolist (wd acwd nil)
		     (setq wd-host (pathname-host wd)
			   wd-device (pathname-device wd))
		     (when (and (stringp wd-host) (string= host wd-host))
		       (when (eq device wd-device)
			 (setq root-dir (pathname-directory wd))
			 (return))))))
		((stringp device)
		 (if (and cwd (progn (setq wd-host (pathname-host cwd)
					   wd-device (pathname-device cwd))
				     (and (stringp wd-host) (string= host wd-host)
					  (stringp wd-device) (string= device wd-device))))
		     (setq root-dir (pathname-directory cwd))
		   (dolist (wd acwd nil)
		     (setq wd-host (pathname-host wd)
			   wd-device (pathname-device wd))
		     (when (and (stringp wd-host) (string= host wd-host))
		       (when (and #+windows (and (stringp wd-device) (string= device wd-device)))
			 (setq root-dir (pathname-directory wd))
			 (return))))))
		(t
		 (if (and cwd (progn (setq wd-host (pathname-host cwd)
					   wd-device (pathname-device cwd))
				     (and (stringp wd-host) (string= host wd-host) (stringp wd-device))))
		     (setq root-dir (pathname-directory cwd) device wd-device)
		   (dolist (wd acwd nil)
		     (setq wd-host (pathname-host wd)
			   wd-device (pathname-device wd))
		     (when (and (stringp wd-host) (string= host wd-host))
		       (when (and #+windows (stringp wd-device))
			 (setq root-dir (pathname-directory wd) device wd-device)
			 (return)))))))
	(cond ((eq device :unspecific)
	       (if (and cwd (progn (setq wd-host (pathname-host cwd)
					 wd-device (pathname-device cwd))
				   (and (stringp wd-host)
					(eq device wd-device))))
		   (setq root-dir (pathname-directory cwd) host wd-host)
		 (dolist (wd acwd nil)
		   (setq wd-host (pathname-host wd)
			 wd-device (pathname-device wd))
		   (when (stringp wd-host)
		     (when (eq device wd-device)
		       (setq root-dir (pathname-directory wd) host wd-host)
		       (return))))))
	      ((stringp device)
	       (if (and cwd (progn (setq wd-host (pathname-host cwd)
					 wd-device (pathname-device cwd))
				   (and (stringp wd-host)
					(stringp wd-device) (string= device wd-device))))
		   (setq root-dir (pathname-directory cwd) host wd-host)
		 (dolist (wd acwd nil)
		   (setq wd-host (pathname-host wd)
			 wd-device (pathname-host wd))
		   (when (stringp wd-host)
		     (when (and #+windows (and (stringp wd-device) (string= device wd-device)))
		       (setq root-dir (pathname-directory wd) host wd-host)
		       (return))))))
	      (t
	       (if (and cwd (progn (setq wd-host (pathname-host cwd)
					 wd-device (pathname-device cwd))
				   (and (stringp wd-host) (stringp wd-device))))
		   (setq root-dir (pathname-directory cwd) host wd-host device wd-device)
		 (dolist (wd acwd nil)
		   (setq wd-host (pathname-host wd)
			 wd-device (pathname-host wd))
		   (when (stringp wd-host)
		     (when (and #+windows (stringp wd-device))
		       (setq root-dir (pathname-directory wd) host wd-host device wd-device)
		       (return))))))))

      (cond ((consp root-dir)
	     (if (and (consp directory) (eq :relative (car directory)))
		 (setq directory (append root-dir (cdr directory)))
	       (unless directory (setq directory root-dir))))
	    ((stringp root-dir)
	     (if (and (consp directory) (eq :relative (car directory)))
		 (setq directory (list* :absolute root-dir (cdr directory)))
	       (unless directory (setq directory (list :absolute root-dir))))))

      (make-pathname :host host :device device :directory directory :defaults path))))


(defconstant +pathname-closer+ (make-pathname :host "localhost"
					      :device :unspecific
					      :directory :unspecific
					      :name :unspecific
					      :type :unspecific
					      :version :unspecific))
				 
(defun complete-pathname (path &optional (defaults *default-pathname-defaults*))
  (unless (pathnamep path) (setq path (pathname path)))
  (if (pathname-complete-p path)
      path
    ;; (let ((cwd mkcl:*current-working-directory*)
    ;; 	  (acwd mkcl:*all-current-working-directories*)
    ;; 	  (path-host (pathname-host path))
    ;; 	  #+windows (path-device (pathname-device path))
    ;; 	  (defaults (if has-defaults-p defaults *default-pathname-defaults*))
    ;; 	  )
    ;;   (unless (and cwd (string= (pathname-host path) (pathname-host cwd))
    ;; 		   #+windows (and (not (logical-pathname-p path)) (string= (pathname-device path) (pathname-device path))))
    ;; 	(setq cwd
    ;; 	      (dolist (wd acwd nil)
    ;; 		(when (string= path-host (pathname-host wd))
    ;; 		  (when (and #+windows (logical-pathname-p path) #+windows (string= path-device (pathname-device wd)))
    ;; 		    (return wd))))))
    ;;   (meld-pathnames (if cwd
    ;; 			  (meld-pathnames (pathname+ path :root-directory (pathname-directory cwd)) defaults)
    ;; 			(merge-pathnames path defaults))
    ;; 		      +pathname-closer+)
    ;;   )
    (meld-pathnames (meld-pathnames (absolute-pathname path) defaults) +pathname-closer+))
  )

(defun physically-complete-pathname (pathspec)
  (let ((path (if (pathnamep pathspec) pathspec (pathname pathspec))))
    (unless (pathname-complete-p path) (setq path (complete-pathname path)))
    (when (logical-pathname-p path)
      (setq path (translate-logical-pathname path))
      (unless (pathname-complete-p path) (setq path (complete-pathname path)))
      )
    path
    )
  )

(defun file-pathname (pathspec)
  (make-pathname :host nil :device nil :directory nil :defaults pathspec))

(defun full-directory-pathname (pathspec)
  (make-pathname :name nil :type nil :version nil :defaults pathspec))

(defun full-directory-namestring (pathspec)
  (namestring (make-pathname :name nil :type nil :version nil :defaults pathspec)))


(defun relative-pathname (pathspec root-dirspec)
  (unless (pathnamep pathspec) (setq pathspec (pathname pathspec)))
  (unless (pathnamep root-dirspec) (setq root-dirspec (pathname root-dirspec)))

  (unless (directory-p root-dirspec) (return-from relative-pathname (values nil root-dirspec)))

  (unless (absolute-pathname-p root-dirspec) (setq root-dirspec (absolute-pathname root-dirspec)))
  (unless (absolute-pathname-p pathspec) (setq pathspec (absolute-pathname pathspec)))

  (let ((true-root (probe-file root-dirspec)))
    (unless true-root (return-from relative-pathname (values nil root-dirspec)))
    (setq root-dirspec true-root))

  (unless (and root-dirspec pathspec
	       (if (logical-pathname-p pathspec)
		   (and (logical-pathname-p root-dirspec)
			(equalp (pathname-host pathspec) (pathname-host root-dirspec)))
		 (and (equal (pathname-host pathspec) (pathname-host root-dirspec))
		      (equal (pathname-device pathspec) (pathname-device root-dirspec))))
	       )
    (return-from relative-pathname (values nil root-dirspec)))

  (let ((dir-path (pathname-directory pathspec))
	(dir-root (pathname-directory root-dirspec))
	;;(depth 0)
	;;dir-level
	rel-dir)
    (unless (and (listp (pathname-directory pathspec)) (listp (pathname-directory root-dirspec)))
      (return-from relative-pathname (values nil root-dirspec)))
      
    (si:while (and dir-path dir-root (equal (car dir-path) (car dir-root)))
      ;;(incf depth)
      (pop dir-path)
      (pop dir-root))

    (setq rel-dir dir-path)

    (dotimes (i (list-length dir-root))
      (push :up rel-dir))

    (push :relative rel-dir)
    (make-pathname :directory rel-dir :defaults pathspec)
    )
  )

(defun relative-namestring (pathspec root-dirspec)
  (let ((rel-pathspec (relative-pathname pathspec root-dirspec)))
    (when rel-pathspec (namestring rel-pathspec))))


;;;;;;;;;;;;;;;;
