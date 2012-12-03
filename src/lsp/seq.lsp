;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: SYSTEM -*-
;;;;
;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;  Copyright (c) 2012, Jean-Claude Beaudoin.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Lesser General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 3 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../../Copyright' for full details.

;;;;                           sequence routines

(in-package "SYSTEM")

(defun error-sequence-type (type)
  (error 'simple-type-error
	 :datum (vector) ;; Any sequence object will do, because it does not belong to TYPE
	 :expected-type type
	 :format-control "~S does not specify a sequence type"
	 :format-arguments (list type)))

(defun error-sequence-length (object type size)
  (error 'simple-type-error
	 :format-control "Cannot create a sequence of size ~S which matches type ~S."
	 :format-arguments (list size type)
	 :expected-type type
	 :datum object))

(defun closest-sequence-type (type)
  (let (elt-type length name args)
    (cond ((consp type)
	   (setq name (first type) args (cdr type)))
	  ((si::instancep type)
	   (setf name (class-name (the class type)) args nil))
	  (t
	   (setq name type args nil)))
    (case name
      ((LIST)
       ;; This is the only descriptor that does not match a real
       ;; array type.
       (setq elt-type 'LIST length '*))
      ((VECTOR)
       (setq elt-type (if (endp args) 'T (first args))
	     length (if (endp (rest args)) '* (second args))))
      ((SIMPLE-VECTOR)
       (setq elt-type 'T
	     length (if (endp args) '* (first args))))
      #-unicode
      ((STRING SIMPLE-STRING)
       (setq elt-type 'BASE-CHAR
	     length (if (endp args) '* (first args))))
      #+unicode
      ((BASE-STRING BASE-SIMPLE-STRING)
       (setq elt-type 'BASE-CHAR
	     length (if (endp args) '* (first args))))
      #+unicode
      ((STRING SIMPLE-STRING)
       (setq elt-type 'CHARACTER
	     length (if (endp args) '* (first args))))
      ((BIT-VECTOR SIMPLE-BIT-VECTOR)
       (setq elt-type 'BIT
	     length (if (endp args) '* (first args))))
      ((ARRAY SIMPLE-ARRAY)
       (when (or (endp (rest args))
		 (atom (setq length (second args)))
		 (endp length)
		 (not (endp (rest length))))
	 (error-sequence-type type))
       (setq elt-type (upgraded-array-element-type (first args))
	     length (first (second args))))
      (t
       ;; We arrive here when the sequence type is not easy to parse.
       ;; We give up trying to guess the length of the sequence.
       ;; Furthermore, we also give up trying to find if the element
       ;; type is *. Instead we just compare with some specialized
       ;; types and otherwise fail.
       (dolist (i '(
		    (NIL . NIL)
		    (LIST . LIST)
                    #-unicode
                    (SIMPLE-STRING . BASE-CHAR)
                    #-unicode
		    (STRING . BASE-CHAR)
                    #+unicode
                    (SIMPLE-BASE-STRING . BASE-CHAR)
                    #+unicode
                    (BASE-STRING . BASE-CHAR)
                    #+unicode
                    (SIMPLE-STRING . CHARACTER)
                    #+unicode
                    (STRING . CHARACTER)
		    (BIT-VECTOR . BIT)
		    ((VECTOR MKCL:NATURAL8) . MKCL:NATURAL8)
		    ((VECTOR MKCL:INTEGER8) . MKCL:INTEGER8)
		    ((VECTOR MKCL:CL-INDEX) . MKCL:CL-INDEX)
		    ((VECTOR FIXNUM) . FIXNUM)
		    ((VECTOR SHORT-FLOAT) . SHORT-FLOAT)
		    ((VECTOR LONG-FLOAT) . LONG-FLOAT)
		    ((VECTOR T) . T))
		(if (subtypep type 'vector)
		    ;; Does this have to be a type-error?
		    ;; 17.3 for MAKE-SEQUENCE says it should be an error,
		    ;; but does not specialize what kind.
		    (error "Cannot find the element type in vector type ~S" type)
		    (error-sequence-type type)))
	  (when (subtypep type (car i))
	    (setq elt-type (cdr i) length '*)
	    ;; The (NIL . NIL) case above
	    (unless elt-type
	      (error-sequence-type type))
	    (return)))))
    (values elt-type length)))

(defun make-sequence (type size	&key (initial-element nil iesp) &aux sequence)
  "Args: (type length &key initial-element)
Creates and returns a sequence of the given TYPE and LENGTH.  If INITIAL-
ELEMENT is given, then it becomes the elements of the created sequence.  The
default value of INITIAL-ELEMENT depends on TYPE."
  (multiple-value-bind (element-type length)
      (closest-sequence-type type)
    (cond ((eq element-type 'LIST)
	   (setq sequence (make-list size :initial-element initial-element))
	   (unless (subtypep 'LIST type)
	     (when (or (and (subtypep type 'NULL) (plusp size))
		       (and (subtypep type 'CONS) (zerop size)))
	       (error-sequence-length (make-list size :initial-element initial-element) type 0))))
	  (t
	   (setq sequence (sys:make-vector (if (eq element-type '*) T element-type)
					   size nil nil nil nil))
	   (when iesp
	     (do ((i 0 (1+ i))
		  (size size))
		 ((>= i size))
	       (declare (fixnum i size))
	       (setf (elt sequence i) initial-element)))
	   (unless (or (eql length '*) (eql length size))
	     (error-sequence-length sequence type size))))
    sequence))

(defun make-seq-iterator (sequence &optional (start 0))
  (cond ((null start)
	 (setf start 0))
	((not (integerp start))
	 (error "Value ~A is not a valid index into sequence ~A" start sequence)))
  (cond ((consp sequence)
	 (nthcdr start sequence))
	((>= start (length sequence))
	 nil)
	(t
	 start)))

(defun seq-iterator-ref (sequence iterator)
  (if (si::fixnump iterator)
      (elt sequence iterator)
      (first iterator)))

(defun seq-iterator-set (sequence iterator value)
  (if (si::fixnump iterator)
      (setf (elt sequence iterator) value)
      (setf (first iterator) value)))

(defun seq-iterator-next (sequence iterator)
  ;; returns nil on end of sequence (otherwise it's either a cons or a fixnum).
  (if (fixnump iterator)
      (and (< (incf iterator) (length sequence))
	   iterator)
      (rest iterator)))

(defun coerce-to-list (object)
  (if (listp object)
      object
      (do ((it (make-seq-iterator object) (seq-iterator-next object it))
	   (output nil))
	  ((null it) (nreverse output))
	(push (seq-iterator-ref object it) output))))

(defun coerce-to-vector (object elt-type length)
  (let ((output object))
    (unless (and (vectorp object)
		 (eq (array-element-type object) elt-type))
      (let* ((final-length (if (eq length '*) (length object) length)))
	(setf output (make-vector elt-type final-length nil nil nil 0))
	(do ((i (make-seq-iterator object) (seq-iterator-next output i))
	     (j 0 (1+ j)))
	    ((= j final-length)
	     (setf object output))
	  (declare (index j))
	  (setf (aref output j) (seq-iterator-ref object i)))))
    (unless (eq length '*)
      (unless (= length (length output))
	(let ((type `(vector ,elt-type (,length))))
	  ;;(check-type output type "coerced object") ;; check-type does not eval its second argument
	  ;; so this here above could not work.
	  ;; Here is below the manual expansion of check-type fixed for our need.
	  (unless (typep output type)
	    (setf output (si::do-check-type output type '"coerced object" 'output)))
	  )))
    output))

(defvar *trace-concatenate* nil)

(defun concatenate (result-type &rest sequences)
  "Args: (type &rest sequences)
Returns a new sequence of the specified type, consisting of all elements of
SEQUENCEs."
  (when *trace-concatenate*
    (format t "~&In concatenate for: ~S ~S." result-type sequences))
  (do* ((length-list (mapcar #'length sequences) (rest length-list))
	(output (make-sequence result-type (apply #'+ length-list)))
        (sequences sequences (rest sequences))
        (i (make-seq-iterator output)))
      ((null sequences) output)
    (do* ((s (first sequences))
	  (j (make-seq-iterator s) (seq-iterator-next s j)))
	 ((null j))
      (seq-iterator-set output i (seq-iterator-ref s j))
      (setq i (seq-iterator-next output i)))))


(defun map (result-type function sequence &rest more-sequences)
  "Args: (type function sequence &rest more-sequences)
Creates and returns a sequence of TYPE with K elements, with the N-th element
being the value of applying FUNCTION to the N-th elements of the given
SEQUENCEs, where K is the minimum length of the given SEQUENCEs."
  (setq more-sequences (cons sequence more-sequences))
  (do* ((l (apply #'min (mapcar #'length more-sequences)))
        (it (mapcar #'make-seq-iterator more-sequences))
	(val (make-sequence 'list (length more-sequences)))
	(x (unless (null result-type) (make-sequence result-type l)))
	(ix (unless (null result-type) (make-seq-iterator x))))
       (nil)
    (do ((i it (cdr i))
         (v val (cdr v))
	 (s more-sequences (cdr s)))
	((null i))
      (unless (car i) (return-from map x))
      (rplaca v (seq-iterator-ref (car s) (car i)))
      (rplaca i (seq-iterator-next (car s) (car i))))
    (let ((that-value (apply function val)))
      (unless (null result-type)
        (seq-iterator-set x ix that-value)
	(setq ix (seq-iterator-next x ix))))))

(eval-when (eval compile)
(defmacro def-seq-bool-parser (name doc test end-value)
 `(defun ,name (predicate sequence &rest more-sequences)
    ,doc
    (setq more-sequences (cons sequence more-sequences))
    (do ((it (mapcar #'make-seq-iterator more-sequences))
         (val (make-sequence 'list (length more-sequences))))
        (nil)
	;;(declare (optimize (safety 0)))
      (do ((i it (cdr i))
           (v val (cdr v))
	   (s more-sequences (cdr s)))
          ((null i))
        (unless (car i) (return-from ,name ,end-value))
        (rplaca v (seq-iterator-ref (car s) (car i)))
        (rplaca i (seq-iterator-next (car s) (car i))))
      (let ((that-value
             (apply predicate val)))
        ,test)))))

(def-seq-bool-parser some
  "Args: (predicate sequence &rest more-sequences)
Returns T if at least one of the elements in SEQUENCEs satisfies PREDICATE;
NIL otherwise."
  (when that-value (return that-value))
  nil)

(def-seq-bool-parser every
  "Args: (predicate sequence &rest more-sequences)
Returns T if every elements of SEQUENCEs satisfy PREDICATE; NIL otherwise."
  (unless that-value (return nil))
  t)

(defun every* (predicate &rest sequences)
  "Args: (predicate sequence &rest more-sequences)
Returns T if every elements of SEQUENCEs satisfy PREDICATE and all sequences
have the same length; NIL otherwise."
  (and (apply #'= (mapcar #'length sequences))
       (apply #'every predicate sequences)))


(defun notany (predicate sequence &rest more-sequences)
  "Args: (predicate sequence &rest more-sequences)
Returns T if none of the elements in SEQUENCEs satisfies PREDICATE; NIL
otherwise."
  (not (apply #'some predicate sequence more-sequences)))


(defun notevery (predicate sequence &rest more-sequences)
  "Args: (predicate sequence &rest more-sequences)
Returns T if at least one of the elements in SEQUENCEs does not satisfy
PREDICATE; NIL otherwise."
  (not (apply #'every predicate sequence more-sequences)))

(defun map-into (result-sequence function &rest sequences)
"Fills the output sequence with the values returned by applying FUNCTION to the
elements of the given sequences. The i-th element of RESULT-SEQUENCE is the output
of applying FUNCTION to the i-th element of each of the sequences. The map routine
stops when it reaches the end of one of the given sequences."
  (let ((nel (apply #'min (if (vectorp result-sequence)
			      (array-dimension result-sequence 0)
			      (length result-sequence))
		    (mapcar #'length sequences))))
    (declare (fixnum nel))
    ;; Set the fill pointer to the number of iterations
    (when (and (vectorp result-sequence)
	       (array-has-fill-pointer-p result-sequence))
      (setf (fill-pointer result-sequence) nel))
    ;; Perform mapping
    (do ((ir (make-seq-iterator result-sequence) (seq-iterator-next result-sequence ir))
         (it (mapcar #'make-seq-iterator sequences))
         (val (make-sequence 'list (length sequences))))
        ((null ir) result-sequence)
      (do ((i it (cdr i))
	   (v val (cdr v))
           (s sequences (cdr s)))
	  ((null i))
	(unless (car i) (return-from map-into result-sequence))
	(rplaca v (seq-iterator-ref (car s) (car i)))
	(rplaca i (seq-iterator-next (car s) (car i))))
      (seq-iterator-set result-sequence ir (apply function val)))))

;;;;;;;;;;;;;


(defun mkcl::str+ (&rest strings)
  (declare (dynamic-extent strings))
  (apply #'concatenate 'string strings))

(define-compiler-macro mkcl::str+ (&whole form &rest strings)
  `(concatenate 'string ,@strings))

(defun mkcl::bstr+ (&rest strings)
  (declare (dynamic-extent strings))
  (apply #'concatenate 'base-string strings))

(define-compiler-macro mkcl::bstr+ (&whole form &rest strings)
  `(concatenate 'base-string ,@strings))

(defun mkcl::split-string (str delim)
  (let ((start 0) (end 0) out)
    (declare (fixnum start end))
    (dotimes (i (length str) (setq end i))
      (declare (fixnum i))
      (when (char= delim (char str i))
	(setq end i)
	(when (> (- end start) 0)
	  (push (subseq str start end) out))
	(setq start (1+ end))
	)
      )
    (when (> (- end start) 0)
      (push (subseq str start end) out))
    (nreverse out)
    )
  )


(export '(mkcl::str+ mkcl::bstr+ mkcl::split-string) :mkcl)

