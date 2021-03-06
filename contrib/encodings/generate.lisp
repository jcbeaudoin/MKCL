;;;  -*- Mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;;  Copyright (c) 2009, Juan Jose Garcia-Ripoll
;;;  Copyright (c) 2011, Jean-Claude Beaudoin
;;;
;;;    This program is free software; you can redistribute it and/or
;;;    modify it under the terms of the GNU Library General Public
;;;    License as published by the Free Software Foundation; either
;;;    version 3 of the License, or (at your option) any later version.
;;;
;;;    See file '../../Copyright' for full details.

(load (merge-pathnames "tools" *load-pathname*) :external-format '(:ascii :lf))

(loop for entry in +all-mappings+
   for name = (first entry)
   for orig = (make-pathname :name name :type "BIN" :defaults "../contrib/encodings/")
   ;; Here we use a logical pathname for target since at runtime #'load-encoding will use one. BUILD: is set in bare.lsp. JCB
   for copy = (ensure-directories-exist (make-pathname :name name :defaults #P"BUILD:ENCODINGS;FOO.BIN"))
   do (progn
	(unless (probe-file orig)
	  (format t "~&Mapping file ~A is missing. Update needed.~%" orig)
	  (when *update-mapping*
	    (let ((mapping (if (equalp name "JISX0208")
			       (mapcar #'rest (read-mapping name 3))
			     (read-mapping name))))
	      (dump-mapping-array mapping orig)))
	  )
	(copy-file orig copy :verbose *generate-verbose*)))

(defconstant +aliases+
  '((:us-ascii :ascii)
    (:utf-8 :utf8)
    (:utf-16 :ucs-2 :ucs2)
    (:utf-16le :ucs-2le :ucs2le)
    (:utf-16be :ucs-2be :ucs2be)
    (:utf-32 :ucs-4 :ucs4 :unicode)
    (:utf-32be :ucs-4be :ucs4be)
    (:utf-32le :ucs-4le :ucs4le)

    (:koi8-r :koi8r)
    
    (:iso-8859-1 :latin-1 :latin1 :cp819 :ibm819)
    (:iso-8859-2 :latin-2 :latin2)
    (:iso-8859-3 :latin-3 :latin3)
    (:iso-8859-4 :latin-4 :latin4)
    (:iso-8859-5 :cyrillic)
    (:iso-8859-6 :arabic)
    (:iso-8859-7 :greek :ecma-118)
    (:iso-8859-8 :hebrew)
    (:iso-8859-9 :latin-5 :latin5)
    (:iso-8859-10 :latin-6 :latin6)
    (:iso-8859-11 :thai)
    (:iso-8859-13 :latin-7 :latin7)
    (:iso-8859-14 :latin-8 :latin8)
    (:iso-8859-15 :latin-9 :latin9) 

    (:dos-cp437 :ibm437 :cp437)
    (:dos-cp850 :ibm850 :cp850)
    (:dos-cp852 :ibm852)
    (:dos-cp855 :ibm855)
    (:dos-cp857 :ibm857)
    (:dos-cp860 :ibm860)
    (:dos-cp861 :ibm861)
    (:dos-cp862 :ibm862 :cp862)
    (:dos-cp863 :ibm863)
    (:dos-cp864 :ibm864)
    (:dos-cp865 :ibm865)
    (:dos-cp866 :ibm866 :cp866)
    (:dos-cp869 :ibm869)

    (:windows-cp932 :windows-932 :cp932)
    (:windows-cp936 :windows-936 :cp936)
    (:windows-cp949 :windows-949 :cp949)
    (:windows-cp950 :windows-950 :cp950)

    (:windows-cp1250 :windows-1250 :ms-ee)
    (:windows-cp1251 :windows-1251 :ms-cyrl)
    (:windows-cp1252 :windows-1252 :ms-ansi)
    (:windows-cp1253 :windows-1253 :ms-greek)
    (:windows-cp1254 :windows-1254 :ms-turk)
    (:windows-cp1255 :windows-1255 :ms-hebr)
    (:windows-cp1256 :windows-1256 :ms-arab)
    (:windows-cp1257 :windows-1257 :winbaltrim)
    (:windows-cp1258 :windows-1258)
    ))

(loop for (name . aliases) in +aliases+
   do (loop for alias in aliases
	    for filename0 = (make-pathname :name (symbol-name alias) :defaults #P"BUILD:ENCODINGS;")
	    for filename = (ensure-directories-exist filename0)
	    do (with-open-file (out filename :direction :output :if-exists :supersede
				    :if-does-not-exist :create :external-format '(:ascii :lf))
			       (when *generate-verbose* (format t "~%;;; Creating alias ~A -> ~A, ~A" alias name filename))
			       (format out "(defparameter mk-ext::~A (si::make-encoding :~A))~%" alias name))))

;;(copy-file "../contrib/encodings/tools.lisp" "./ENCODINGS/tools.lisp" :verbose *generate-verbose*)
(copy-file "../contrib/encodings/ISO-2022-JP" #P"BUILD:ENCODINGS;ISO-2022-JP" :verbose *generate-verbose*)
(copy-file "../contrib/encodings/ISO-2022-JP-1" #P"BUILD:ENCODINGS;ISO-2022-JP-1" :verbose *generate-verbose*)
(when *generate-verbose* (terpri) (terpri))
