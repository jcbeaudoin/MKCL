;;;
;;; Note about the origin of this file.
;;;
;;; This file was inherited by MKCL from ECL 9.6.2 when MKCL was
;;; initially forked from it. But being in the contrib directory,
;;; it is likely that it originated from some other unidentified source.
;;; In fact, this file content can be found verbatim in a file of
;;; the same name (ucd.lisp) in the tools-for-build directory of SBCL.
;;; It is suspected that this SBCL version is in fact the original
;;; source. (sampled in SBCL 1.0.58).  As inherited from ECL 9.6.2,
;;; this file did not include any copyright notice, nor do any
;;; version included in SBCL up to this day (2012/11/14).
;;;
;;;
;;; This file was later modified for MKCL's purposes, mainly to
;;; improve its robustness, reliability and maintainability.
;;; For these modifications the same conditions apply as for the
;;; rest of MKCL:
;;;
;;; Copyright (c) 2012, Jean-Claude Beaudoin.
;;;

;;; In its original form, this code was barely of kleenex quality,
;;; full of unchecked limitations, good to be used at most once...
;;; Some of that state may persist despite efforts to redress it. JCB


;;; Common

(defparameter *extension-directory*
  (make-pathname :directory (pathname-directory *load-truename*)))

(defconstant *page-size-exponent* 8)
(defconstant *page-size* (ash 1 *page-size-exponent*))

(defconstant *unichar-codepoint-limit* #x110000)

(defconstant *ucd-version* "6.2.0")

;; #'cp-high and #'cp-low split a code-point in two parts.
;; The high part (from codepoints bits 21 to 8) becomes an index in the page table.
;; The low part (from codepoints bits 7 to 0) becomes an index to a specific
;; character info struct inside a given page.
(defun cp-high (cp)
  (ash cp (- *page-size-exponent*)))

(defun cp-low (cp)
  (ldb (byte *page-size-exponent* 0) cp))

;;; Generator

(defstruct unichar-info
  properties-sig-index
  transform
  )

#|
(defparameter *unicode-character-database* ;; yet another misnomer.
  (make-pathname :directory (pathname-directory *load-truename*)))
|#

(defparameter *unichar-info-pages* nil) ;; a vector of pages.
;; There is (cp-high *unichar-codepoint-limit*) [4352] pages in this vector.
;; Each page is itself a vector of *page-size* [256] unichar-info structs.

(defparameter *unicode-names* (make-hash-table)) ;; indexed on code-point. 
(defparameter *unicode-names-total-size* 0)

(defparameter *last-uppercase* nil)
(defparameter *uppercase-transition-count* 0)
(defparameter *different-titlecases* nil)
(defparameter *different-numerics* nil)

(defparameter *unichar-properties-signature-indices* nil)
(defparameter *unichar-properties-signature-table* nil)
(defparameter *unichar-properties-signature-sorted-table* nil)
(defparameter *unichar-properties-signature-to-sorted-mapping* nil)

(defparameter *both-cases* nil)

(defparameter *decompositions* nil)
(defparameter *decomposition-length-max* nil)
(defparameter *decomposition-types* nil)
(defparameter *decomposition-base* nil)



(defun find-unichar-properties-signature-index (general-category-index bidi-index ccc-index
						decimal-digit digit bidi-mirrored cl-both-case-p)
  (let* ((list (list general-category-index bidi-index ccc-index decimal-digit digit bidi-mirrored cl-both-case-p))
         (index (gethash list *unichar-properties-signature-indices*)))
    (or index
        (let ((index (vector-push list *unichar-properties-signature-table*)))
	  (setf (gethash list *unichar-properties-signature-indices*) index)))))

(defun compare-unichar-properties-signatures (left right)
  (destructuring-bind (left-general-category-index left-bidi-index left-ccc-index
                       left-decimal-digit left-digit left-bidi-mirrored
                       left-cl-both-case-p)
      left
    (destructuring-bind (right-general-category-index right-bidi-index right-ccc-index
                         right-decimal-digit right-digit right-bidi-mirrored
                         right-cl-both-case-p)
        right
      (or (and left-cl-both-case-p (not right-cl-both-case-p))
          (and (or left-cl-both-case-p (not right-cl-both-case-p))
               (or (< left-general-category-index right-general-category-index)
                   (and (= left-general-category-index right-general-category-index)
                        (or (< left-bidi-index right-bidi-index)
                            (and (= left-bidi-index right-bidi-index)
                                 (or (< left-ccc-index right-ccc-index)
                                     (and (= left-ccc-index right-ccc-index)
                                          (or (string< left-decimal-digit
                                                       right-decimal-digit)
                                              (and (string= left-decimal-digit
                                                            right-decimal-digit)
                                                   (or (string< left-digit right-digit)
                                                       (and (string= left-digit
                                                                     right-digit)
                                                            (string< left-bidi-mirrored
                                                                     right-bidi-mirrored))))))))))))))))

(defun build-unichar-properties-signature-sorted-table ()
  (let ((sig-max-index (fill-pointer *unichar-properties-signature-table*)))
    (setq *unichar-properties-signature-sorted-table* (copy-seq *unichar-properties-signature-table*))
    (sort *unichar-properties-signature-sorted-table* #'compare-unichar-properties-signatures)
    (setq *unichar-properties-signature-to-sorted-mapping* (make-array sig-max-index))
    (loop for i from 0 below sig-max-index
	  do (setf (aref *unichar-properties-signature-to-sorted-mapping*
			 (gethash (aref *unichar-properties-signature-sorted-table* i) *unichar-properties-signature-indices*))
		   i))))

(defun slurp-ucd ()
  (setq *last-uppercase* nil)
  (setq *uppercase-transition-count* 0)
  (setq *different-titlecases* nil)
  (setq *different-numerics* nil)
  (setq *unicode-names-total-size* 0)
  (setq *unichar-properties-signature-indices* (make-hash-table :test #'equal))
  (setq *unichar-properties-signature-table* (make-array 256 :fill-pointer 0))
  (setq *both-cases* nil)
  (setq *decompositions* 0)
  (setq *decomposition-types* (make-hash-table :test #'equal))
  (setq *decomposition-length-max* 0)
  (setq *decomposition-base* (make-array (cp-high *unichar-codepoint-limit*) :initial-element nil))
  (setq *unichar-info-pages* (make-array (cp-high *unichar-codepoint-limit*) :initial-element nil))

  (with-open-file (*standard-input*
		   (make-pathname :name "UnicodeData" :type "txt"
				  :defaults *extension-directory*)
                   :direction :input :external-format '(:ascii :lf))
    (loop for line = (read-line nil nil)
          while line
          do (slurp-ucd-line line)))
  (second-pass)
  (build-unichar-properties-signature-sorted-table)
  *decompositions*)

(defun split-string (line character)
  (loop for prev-position = 0 then (1+ position)
        for position = (position character line :start prev-position)
        collect (subseq line prev-position position)
        do (unless position
             (loop-finish))))

#|
(defun init-indices (strings)
  (let ((hash (make-hash-table :test #'equal)))
    (loop for string in strings
          for index from 0
          do (setf (gethash string hash) index))
    hash))

(defparameter *general-categories-indices*
  (init-indices '("Lu" "Ll" "Lt" "Lm" "Lo" "Cc" "Cf" "Co" "Cs" "Mc"
                  "Me" "Mn" "Nd" "Nl" "No" "Pc" "Pd" "Pe" "Pf" "Pi"
                  "Po" "Ps" "Sc" "Sk" "Sm" "So" "Zl" "Zp" "Zs")))
|#

(defun init-indices (strings)
  (let ((hash (make-hash-table :test #'equal)))
    (loop for (string c-string) across strings
          for index from 0
          do (setf (gethash string hash) index))
    hash))

(defconstant *general-categories*
  #(("Lu" "mkcl_ucd_Uppercase_Letter")
    ("Ll" "mkcl_ucd_Lowercase_Letter")
    ("Lt" "mkcl_ucd_Titlecase_Letter")
    ("Lm" "mkcl_ucd_Modified_Letter")
    ("Lo" "mkcl_ucd_Other_Letter")
    ("Cc" "mkcl_ucd_Control")
    ("Cf" "mkcl_ucd_Format")
    ("Co" "mkcl_ucd_Private_Use")
    ("Cs" "mkcl_ucd_Surrogate")
    ("Mc" "mkcl_ucd_Spacing_Mark")
    ("Me" "mkcl_ucd_Enclosing_Mark")
    ("Mn" "mkcl_ucd_Nonspacing_Mark")
    ("Nd" "mkcl_ucd_Decimal_Number")
    ("Nl" "mkcl_ucd_Letter_Number")
    ("No" "mkcl_ucd_Other_Number")
    ("Pc" "mkcl_ucd_Connector_Punctuation")
    ("Pd" "mkcl_ucd_Dash_Punctuation")
    ("Pe" "mkcl_ucd_Close_Punctuation")
    ("Pf" "mkcl_ucd_Final_Punctuation")
    ("Pi" "mkcl_ucd_Initial_Punctuation")
    ("Po" "mkcl_ucd_Other_Punctuation")
    ("Ps" "mkcl_ucd_Open_Punctuation")
    ("Sc" "mkcl_ucd_Currency_Symbol")
    ("Sk" "mkcl_ucd_Modifier_Symbol")
    ("Sm" "mkcl_ucd_Math_Symbol")
    ("So" "mkcl_ucd_Other_Symbol")
    ("Zl" "mkcl_ucd_Line_Separator")
    ("Zp" "mkcl_ucd_Paragraph_Separator")
    ("Zs" "mkcl_ucd_Space_Separator")
    ))

(defparameter *general-categories-indices*
  (init-indices *general-categories*))

#|
(defparameter *bidi-classes-indices*
  (init-indices '("AL" "AN" "B" "BN" "CS" "EN" "ES" "ET" "L" "LRE" "LRO"
                  "NSM" "ON" "PDF" "R" "RLE" "RLO" "S" "WS")))
|#

(defconstant *bidi-classes*
  #(("AL"  "mkcl_ucd_bidi_Arabic_Letter")
    ("AN"  "mkcl_ucd_bidi_Arabic_Number")
    ("B"   "mkcl_ucd_bidi_Paragraph_Separator")
    ("BN"  "mkcl_ucd_bidi_Boundary_Neutral")
    ("CS"  "mkcl_ucd_bidi_Common_Separator")
    ("EN"  "mkcl_ucd_bidi_European_Number")
    ("ES"  "mkcl_ucd_bidi_European_Separator")
    ("ET"  "mkcl_ucd_bidi_European_Terminator")
    ("L"   "mkcl_ucd_bidi_Left_To_Right")
    ("LRE" "mkcl_ucd_bidi_Left_To_Right_Embedding")
    ("LRO" "mkcl_ucd_bidi_Left_To_Right_Override")
    ("NSM" "mkcl_ucd_bidi_Nonspacing_Mark")
    ("ON"  "mkcl_ucd_bidi_Other_Neutral")
    ("PDF" "mkcl_ucd_bidi_Pop_Directional_Format")
    ("R"   "mkcl_ucd_bidi_Right_To_Left")
    ("RLE" "mkcl_ucd_bidi_Right_To_Left_Embedding")
    ("RLO" "mkcl_ucd_bidi_Right_To_Left_Override")
    ("S"   "mkcl_ucd_bidi_Segment_Separator")
    ("WS"  "mkcl_ucd_bidi_White_Space")
    ))

(defparameter *bidi-classes-indices*
  (init-indices *bidi-classes*))


(defparameter *block-first* nil)

(defun normalize-character-name (name)
  (when (find #\_ name)
    (error "Bad name for a character: ~A" name))
  (unless (or (zerop (length name)) (find #\< name) (find #\> name))
    (substitute #\_ #\Space name)))

;;;   3400  --  4DB5  : cjk ideograph extension a ;Lo;0;L;;;;;N;;;;;
;;;   AC00  --  D7A3  : hangul syllables ;Lo;0;L;;;;;N;;;;;
;;;   D800  --  F8FF  : surrogates and private use
;;;  20000  --  2A6D6 : cjk ideograph extension b ;Lo;0;L;;;;;N;;;;;
;;;  F0000  --  FFFFD : private use
;;; 100000  --  10FFFD: private use
(defun parse-ucd-line (line code-point)
  (destructuring-bind (name general-category canonical-combining-class
                            bidi-class decomposition-type-and-mapping
                            decimal-digit digit numeric bidi-mirrored
                            unicode-1-name iso-10646-comment simple-uppercase
                            simple-lowercase simple-titlecase)
      line
    (declare (ignore unicode-1-name iso-10646-comment))
    (if (and (> (length name) 8)
             (string= ", First>" name :start2 (- (length name) 8)))
        (progn
          (setq *block-first* code-point)
          nil)
        (let* ((general-category-index (or (gethash general-category *general-categories-indices*)
                             (error "unknown general category ~A"
                                    general-category)))
               (bidi-index (or (gethash bidi-class *bidi-classes-indices*)
                               (error "unknown bidirectional class ~A"
                                      bidi-class)))
               (ccc-index (parse-integer canonical-combining-class))
               (digit-index (unless (string= "" decimal-digit)
                              (parse-integer decimal-digit)))
               (upper-index (unless (string= "" simple-uppercase)
                              (parse-integer simple-uppercase :radix 16)))
               (lower-index (unless (string= "" simple-lowercase)
                              (parse-integer simple-lowercase :radix 16)))
               (title-index (unless (string= "" simple-titlecase)
                              (parse-integer simple-titlecase :radix 16)))
               (cl-both-case-p
                (not (null (or (and (= general-category-index 0) lower-index)
                               (and (= general-category-index 1) upper-index)))))
               (unichar-properties-signature-index
		(find-unichar-properties-signature-index general-category-index bidi-index ccc-index
							 decimal-digit digit bidi-mirrored cl-both-case-p)))
          (declare (ignore digit-index))
          (incf *unicode-names-total-size* (length name))
          (when (string/= "" decomposition-type-and-mapping)
            (let ((split (split-string decomposition-type-and-mapping
                                       #\Space)))
              (when (char= #\< (aref (first split) 0))
                (setf (gethash (pop split) *decomposition-types*) t))
              (unless (aref *decomposition-base* (cp-high code-point))
                (setf (aref *decomposition-base* (cp-high code-point))
                      (make-array *page-size*
                                  :initial-element nil)))
              (setf (aref (aref *decomposition-base* (cp-high code-point))
                          (cp-low code-point))
                    (mapcar #'(lambda (string)
                                (parse-integer string :radix 16))
                            split))
              (setq *decomposition-length-max*
                    (max *decomposition-length-max* (length split)))
              (incf *decompositions* (length split))))
          (when (and (string/= "" simple-uppercase)
                     (string/= "" simple-lowercase))
            (push (list code-point upper-index lower-index) *both-cases*))
          (when (string/= simple-uppercase simple-titlecase)
            (push (cons code-point title-index) *different-titlecases*))
          (when (string/= digit numeric)
            (push (cons code-point numeric) *different-numerics*))
          (cond
            ((= general-category-index 8) ;; Is this really testing for category "Cs"? JCB
             (unless *last-uppercase*
               (incf *uppercase-transition-count*))
             (setq *last-uppercase* t))
            (t
             (when *last-uppercase*
               (incf *uppercase-transition-count*))
             (setq *last-uppercase* nil)))
          (when (> ccc-index 255)
            (error "canonical combining class too large ~A" ccc-index))
          (let ((result (make-unichar-info :properties-sig-index unichar-properties-signature-index
					   :transform (or upper-index lower-index 0))))
            (when (and (> (length name) 7)
                       (string= ", Last>" name :start2 (- (length name) 7)))
              (let ((page-start (cp-high (+ *block-first*
					    *page-size*
					    -1)))
                    (page-end (cp-high code-point)))
                (loop for point from *block-first*
                      below (ash page-start *page-size-exponent*)
                      do (setf (aref (aref *unichar-info-pages* (cp-high point))
                                     (cp-low point))
                               result))
                (loop for page from page-start below page-end
                      do (setf (aref *unichar-info-pages* page)
                               (make-array *page-size*
                                           :initial-element result)))
                (loop for point from (ash page-end *page-size-exponent*)
                      below code-point
                      do (setf (aref (aref *unichar-info-pages* (cp-high point))
                                     (cp-low point))
                               result))))
            (values result (normalize-character-name name)))))))

(defun slurp-ucd-line (line)
  (let* ((split-line (split-string line #\;))
         (code-point (parse-integer (first split-line) :radix 16))
         (code-high (cp-high code-point))
         (code-low (cp-low code-point)))
    (unless (aref *unichar-info-pages* code-high)
      (setf (aref *unichar-info-pages* code-high)
            (make-array *page-size* :initial-element nil)))
    (multiple-value-bind (char-info char-name)
        (parse-ucd-line (cdr split-line) code-point)
      (setf (aref (aref *unichar-info-pages* code-high) code-low) char-info
            (gethash code-point *unicode-names*) char-name))))

(defun second-pass ()
  (loop for i from 0 below (length *unichar-info-pages*)
        when (aref *unichar-info-pages* i)
        do (loop for j from 0 below (length (aref *unichar-info-pages* i))
                 for result = (aref (aref *unichar-info-pages* i) j)
                 when result
                 when (let* ((transform-point (unichar-info-transform result))
                             (transform-high (cp-high transform-point))
                             (transform-low (cp-low transform-point)))
                        (and (plusp transform-point)
                             (/= (unichar-info-transform
                                  (aref (aref *unichar-info-pages* transform-high)
                                        transform-low))
                                 (+ (ash i *page-size-exponent*) j))))
                 do (destructuring-bind (general-category-index bidi-index ccc-index
                                         decimal-digit digit bidi-mirrored
                                         cl-both-case-p)
                        (aref *unichar-properties-signature-table* (unichar-info-properties-sig-index result))
                      (declare (ignore cl-both-case-p))
                      ;;(format t "~A~%" (+ (ash i *page-size-exponent*) j))
                      (setf (unichar-info-properties-sig-index result)
                            (find-unichar-properties-signature-index general-category-index bidi-index ccc-index
								     decimal-digit digit bidi-mirrored
								     nil))))))

(defun write-3-byte (triplet stream)
  (write-byte (ldb (byte 8 0) triplet) stream)
  (write-byte (ldb (byte 8 8) triplet) stream)
  (write-byte (ldb (byte 8 16) triplet) stream))

(defun digit-to-byte (digit)
  (if (string= "" digit)
      255
      (parse-integer digit)))

(defun output ()
  (let ((page-folding-bucket (make-hash-table :test #'equalp))
        (folded-page-top-index 0))
    (loop for page across *unichar-info-pages*
          do (when page
               (unless (gethash page page-folding-bucket)
                 (setf (gethash page page-folding-bucket)
                       (incf folded-page-top-index)))))
    (when (<= 255 folded-page-top-index) (error "~&In output: UCD page folding bucket has overflowed: ~S.~%" folded-page-top-index))
    (let ((unichar-info-folded-pages (make-array (1+ folded-page-top-index))))
      (maphash #'(lambda (key value)
                   (setf (aref unichar-info-folded-pages value) key))
               page-folding-bucket)
      (setf (aref unichar-info-folded-pages 0) ;; Is this a blank page simply because we do not know how to count from 0? Yep! JCB
            (make-array *page-size* :initial-element nil))
      (with-open-file (stream (make-pathname :name "ucd"
                                             :type "dat"
                                             :defaults *extension-directory*)
                              :direction :output
                              :element-type '(unsigned-byte 8)
                              :if-exists :supersede
                              :if-does-not-exist :create)
	(let ((offset (* (length *unichar-properties-signature-sorted-table*) 8)))
	  ;; "offset" is the external size of the properties signatures table in bytes.
	  (write-byte (mod offset 256) stream)
	  (write-byte (floor offset 256) stream))
        (loop for (general-category-index bidi-index ccc-index decimal-digit digit bidi-mirrored)
              across *unichar-properties-signature-sorted-table*
              do (write-byte general-category-index stream)
              do (write-byte bidi-index stream)
              do (write-byte ccc-index stream)
              do (write-byte (digit-to-byte decimal-digit) stream)
              do (write-byte (digit-to-byte digit) stream)
              do (write-byte (if (string= "N" bidi-mirrored) 0 1) stream)
              do (write-byte 0 stream)
              do (write-byte 0 stream))
        (loop for page across *unichar-info-pages*
           do (write-byte (if page (gethash page page-folding-bucket) 0) stream))
        (loop for page across unichar-info-folded-pages
           do (loop for entry across page
                 do (write-byte (if entry
                                    (aref *unichar-properties-signature-to-sorted-mapping*
					  (unichar-info-properties-sig-index entry))
                                    255) ;; 255 is an index that does/should not exist. JCB
                                stream)
                 do (write-3-byte (if entry (unichar-info-transform entry) 0)
                                  stream))))))
  ;;#+(or)
  (with-open-file (f (make-pathname :name "ucd-code-names" :type "lisp-expr"
                                    :defaults *extension-directory*)
		     :external-format '(:ascii :lf)
                     :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create)
    (with-standard-io-syntax
      ;;(write-string ";;; Do not edit by hand: generated by ucd.lisp" f)
      (maphash (lambda (code name)
                 (when name
                  (print code f)
                  (prin1 name f)))
               *unicode-names*)
      (terpri f))
    ;;(setf *unicode-names* nil) ;; why should we destroy this? JCB
    )
  (with-open-file (f (make-pathname :name "ucd-name-codes" :type "lisp-expr"
                                    :defaults *extension-directory*)
		     :external-format '(:ascii :lf)
                     :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create)
    (with-standard-io-syntax
      ;;(write-string ";;; Do not edit by hand: generated by ucd.lisp" f)
      (maphash (lambda (code name)
                 (when name
                  (print name f)
                  (prin1 code f)))
               *unicode-names*)
      (terpri f))
    ;;(setf *unicode-names* nil) ;; why should we destroy this? JCB
    )
  (with-open-file (*standard-output*
                   (make-pathname :name "numerics"
                                  :type "lisp-expr"
                                  :defaults *extension-directory*)
		   :external-format '(:ascii :lf)
                   :direction :output
                   :if-exists :supersede
                   :if-does-not-exist :create)
    (let ((*print-pretty* t))
      (prin1 (mapcar #'(lambda (x) (cons (car x) (read-from-string (cdr x))))
                     *different-numerics*))))
  (with-open-file (*standard-output*
                   (make-pathname :name "titlecases"
                                  :type "lisp-expr"
                                  :defaults *extension-directory*)
		   :external-format '(:ascii :lf)
                   :direction :output
                   :if-exists :supersede
                   :if-does-not-exist :create)
    (let ((*print-pretty* t))
      (prin1 *different-titlecases*)))
  (with-open-file (*standard-output*
                   (make-pathname :name "misc"
                                  :type "lisp-expr"
                                  :defaults *extension-directory*)
		   :external-format '(:ascii :lf)
                   :direction :output
                   :if-exists :supersede
                   :if-does-not-exist :create)
    (let ((*print-pretty* t))
      (prin1 `(:length ,(length *unichar-properties-signature-sorted-table*)
               :uppercase ,(loop for (general-category-index) across *unichar-properties-signature-sorted-table*
                                 for i from 0
                                 when (= general-category-index 0)
                                 collect i)
               :lowercase ,(loop for (general-category-index) across *unichar-properties-signature-sorted-table*
                                 for i from 0
                                 when (= general-category-index 1)
                                 collect i)
               :titlecase ,(loop for (general-category-index) across *unichar-properties-signature-sorted-table*
                                 for i from 0
                                 when (= general-category-index 2)
                                 collect i)))))
  (values))

(defun read-compiled-ucd ()
  (with-open-file (stream (make-pathname :name "ucd"
                                         :type "dat"
                                         :defaults *extension-directory*)
                          :direction :input
                          :element-type '(unsigned-byte 8))
    (let ((length (file-length stream)))
      (setq *compiled-ucd*
            (make-array length :element-type '(unsigned-byte 8)))
      (read-sequence *compiled-ucd* stream)))
  (values))

;;(slurp-ucd)
;;(output)

(defconstant *unichar-invalid-digit-value* 255)

(defun output-h-file (h-file)
  (format h-file "
/*
    This file was automatically generated by utility contrib/unicode/ucd.lisp
    for Unicode ~S

    Copyright (c) 2012, Jean-Claude Beaudoin.

    MKCL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 3 of the License, or (at your option) any later version.

    See file '../../Copyright' for full details.
*/~%" *ucd-version*)
  (terpri h-file)
  (format h-file "#ifndef MKCL_UNICODE_H~%")
  (format h-file "#define MKCL_UNICODE_H~2%")

  (format h-file "#define MKCL_UNICHAR_INVALID_DIGIT ((mkcl_uint8_t) ~D)~2%" *unichar-invalid-digit-value*)

  (format h-file "~%enum mkcl_ucd_general_category {~%")
  (loop for (string c-string) across *general-categories*
	do (progn (format h-file "  ~A,~%" c-string))
	)
  (format h-file "};~%")
  (format h-file "~%enum mkcl_ucd_bidi_class {~%")
  (loop for (string c-string) across *bidi-classes*
	do (progn (format h-file "  ~A,~%" c-string))
	)
  (format h-file "};~%")
  (terpri h-file)
  (format h-file "struct mkcl_unichar_properties_signature~%")
  (format h-file "{~%")
  (format h-file "  enum mkcl_ucd_general_category general_category;~%")
  (format h-file "  enum mkcl_ucd_bidi_class bidi;~%")
  (format h-file "  mkcl_uint8_t canonical_combining_class;~%")
  (format h-file "  mkcl_uint8_t decimal_digit;~%")
  (format h-file "  mkcl_uint8_t digit;~%")
  (format h-file "  bool bidi_mirrored;~%")
  (format h-file "};~%")
  (terpri h-file)
  (format h-file "extern const struct mkcl_unichar_properties_signature~%")
  (format h-file "                    _mkcl_unichar_properties_signatures[];~%")
  (terpri h-file)
  (format h-file "const mkcl_uint8_t _mkcl_unichar_info_pages[~D];~2%" (cp-high *unichar-codepoint-limit*))
  (format h-file "struct mkcl_unichar_info~%")
  (format h-file "{~%")
  (format h-file "  mkcl_uint8_t properties_signature_index;~%")
  (format h-file "  unsigned int transform:24;~%")
  (format h-file "};~2%")
  (format h-file "extern const struct mkcl_unichar_info _mkcl_unichar_info[][~D];~2%" *page-size*)

  (format h-file "#endif /* MKCL_UNICODE_H */~2%")
  )

(defun output-c-file (c-file)
  (format c-file "
/*
    This file was automatically generated by utility contrib/unicode/ucd.lisp
    for Unicode ~S

    Copyright (c) 2012, Jean-Claude Beaudoin.

    MKCL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 3 of the License, or (at your option) any later version.

    See file '../../Copyright' for full details.
*/~%" *ucd-version*)
  (terpri c-file)
  (format c-file "#include <mkcl/mkcl.h>~%")
  (terpri c-file)
  (format c-file "const struct mkcl_unichar_properties_signature~%")
  (format c-file "             _mkcl_unichar_properties_signatures[] =~%")
  (format c-file "{~%")
  (loop for (general-category-index bidi-index ccc-index decimal-digit digit bidi-mirrored)
	across *unichar-properties-signature-sorted-table*
	do (format c-file "  { ~A, " (cadr (aref *general-categories* general-category-index)))
	do (format c-file "~A, " (cadr (aref *bidi-classes* bidi-index)))
	do (format c-file "~D, " ccc-index)
	do (format c-file "~A, " (if (and decimal-digit (string/= "" decimal-digit)) decimal-digit *unichar-invalid-digit-value*))
	do (format c-file "~A, " (if (and digit (string/= "" digit)) digit *unichar-invalid-digit-value*))
	do (format c-file "~A },~%" (if (string= "N" bidi-mirrored) "false" "true"))
	)
  (format c-file "};~2%")

  (let ((page-folding-bucket (make-hash-table :test #'equalp))
        (folded-page-top-index 0))
    (loop for page across *unichar-info-pages*
          do (when page
               (unless (gethash page page-folding-bucket)
                 (setf (gethash page page-folding-bucket) folded-page-top-index)
		 (incf folded-page-top-index))))
    (when (<= 255 folded-page-top-index) (error "~&In output: UCD page folding bucket has overflowed: ~S.~%" folded-page-top-index))
    ;;(format t "~&In output: folded-page-top-index = ~S.~%" folded-page-top-index) ;; debug JCB
    (let ((unichar-info-folded-pages (make-array folded-page-top-index)))
      (maphash #'(lambda (key value)
                   (setf (aref unichar-info-folded-pages value) key))
               page-folding-bucket)

      (format c-file "const mkcl_uint8_t _mkcl_unichar_info_pages[~D] =~%" (cp-high *unichar-codepoint-limit*))
      (format c-file "{")
      (loop for page across *unichar-info-pages*
	    for count from 0
	    do (when (eql 0 (mod count 10)) (format c-file "~%  /* ~4D */ " count))
	    do (format c-file " ~D," (if page (gethash page page-folding-bucket) 0))
	    )
      (format c-file "~%};~2%")


      (format c-file "const struct mkcl_unichar_info~%")
      (format c-file "             _mkcl_unichar_info[][~D] =~%" *page-size*)
      (format c-file "{~%")
        (loop for page across unichar-info-folded-pages
	      for page-index from 0
	      do (format c-file "  { /* page ~D */" page-index)
	      do (loop for entry across page
		       for count from 0
		       do (when (eql 0 (mod count 8)) (format c-file "~%  /* ~2X */ " count))
		       do (format c-file " { ~D, 0x~X }," 
				  (if entry
				      (aref *unichar-properties-signature-to-sorted-mapping*
					    (unichar-info-properties-sig-index entry))
                                    255) ;; 255 is an index that does/should not exist. JCB
				  (if entry (unichar-info-transform entry) 0)
				  )
		       )
	      do (format c-file "~%  },~%")
	      )
      (format c-file "};~2%")
      )
    )
  )


(defun output-c-code ()
  (with-open-file (h-file (make-pathname :name "mkcl-unicode"
					 :type "h"
					 :defaults *extension-directory*)
			  :direction :output
			  :external-format '(:ascii :lf)
			  :if-exists :supersede
			  :if-does-not-exist :create)
		  (output-h-file h-file))
  (with-open-file (c-file (make-pathname :name "unicode"
					 :type "c"
					 :defaults *extension-directory*)
			  :direction :output
			  :external-format '(:ascii :lf)
			  :if-exists :supersede
			  :if-does-not-exist :create)
		  (output-c-file c-file)))

;;;;;;;;;;;;;;;;;;;

