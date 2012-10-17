;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: SYSTEM -*-
;;;;
;;;;  Copyright (c) 2011, Juan Jose Garcia-Ripoll.
;;;;  Copyright (c) 2011-2012, Jean-Claude Beaudoin.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 3 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../../Copyright' for full details.
;;;;
;;;;  uni-cond.lsp -- Unicode stream encoding and decoding conditions
;;;;

(in-package "SYSTEM")

;;;;
;;;; ENCODING / DECODING ERRORS
;;;;

(define-condition character-coding-error (error)
  ((external-format :initarg :external-format :reader character-coding-error-external-format)))

(define-condition character-encoding-error (character-coding-error)
  ((character-codepoint :initarg :character-codepoint :reader character-encoding-error-codepoint)))

(define-condition character-decoding-error (character-coding-error)
  ((octets :initarg :octets :reader character-decoding-error-octets)))

(define-condition mkcl:stream-encoding-error (stream-error character-encoding-error)
  ()
  (:report
   (lambda (c s)
     (let ((stream (stream-error-stream c))
           (character-codepoint (character-encoding-error-codepoint c))
	   (*print-base* 16)
	   (*print-radix* t)
	   )
       (format s "~@<encoding error on stream ~S (position ~D): ~2I~_~
                  character codepoint ~S is invalid for external-format ~S.~@:>"
               stream (file-position stream) 
               character-codepoint (character-coding-error-external-format c))))))

(define-condition mkcl:stream-decoding-error (stream-error character-decoding-error)
  ()
  (:report
   (lambda (c s)
     (let ((stream (stream-error-stream c))
           (octets (character-decoding-error-octets c))
	   (*print-base* 16)
	   (*print-radix* t)
	   )
       (format s "~@<decoding error on stream ~S (position ~D): ~2I~_~
                  octet sequence ~S is invalid in external-format ~S.~@:>"
               stream (file-position stream)
               octets (character-coding-error-external-format c))))))

(defvar *default-replacement-character* #\uFFFD)
(defvar *default-replacement-character-alist* nil)

(defun standard-replacement-character-for (external-format)
  (when (consp external-format) (setq external-format (car external-format)))
  (case external-format
	((:utf-8
	  :utf-16 :utf-16le :utf-16be
	  :utf-32 :utf-32le :utf-32be) #\uFFFD)
	((:iso-8859-1 :latin-1) #\u00BF)
	((:us-ascii :ascii) #\u003F)
	(t (let ((replacement (cdr (assoc external-format *default-replacement-character-alist*))))
	     (or replacement *default-replacement-character*)))))

(defun read-replacement-char ()
  (format *query-io* "Enter replacement character (ex: #\a or 65): ")
  (multiple-value-list (read *query-io*)))

(defun stream-encoding-error (stream external-format character-codepoint)
  (let ((replacement (standard-replacement-character-for external-format)))
    (restart-case (error 'mkcl:stream-encoding-error
			 :stream stream
			 :external-format external-format
			 :character-codepoint character-codepoint)
      (continue ()
	:report ;; "Use standard error replacement character" (U+FFFD, U+00BF or U+003F depending)
	(lambda (display)
	  (format display "Write standard replacement character ~S instead." replacement))
	replacement)
      (use-value (ch)
	:report "Specify a character to be written instead of the erroneous character."
	:interactive read-replacement-char
	(if (characterp ch) ch (code-char ch))))))

(defun stream-decoding-error (stream external-format octets)
  (let ((replacement (standard-replacement-character-for external-format)))
  (restart-case (error 'mkcl:stream-decoding-error
                       :stream stream
                       :external-format external-format
                       :octets octets)
    (continue ()
      :report 
      (lambda (display) (format display "Read standard replacement character ~S instead." replacement))
      replacement)
    (use-value (ch)
      :report "Specify a character to be read instead of the erroneous sequence"
      :interactive read-replacement-char
      ;;(format t "~&In stream-decoding-error: told to use ~S~%" ch) (finish-output)
      (if (characterp ch) ch (code-char ch))))))

