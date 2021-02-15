;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: SYSTEM -*-
;;;;
;;;;  Copyright (c) 2001, Juan Jose Garcia-Ripoll
;;;;  Copyright (c) 2012-2013,2020, Jean-Claude Beaudoin.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Lesser General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 3 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../../Copyright' for full details.

;;;; FFI  Symbols used in the foreign function interface. Basically, UFFI plus some extensions.

(defpackage "FFI"
  (:nicknames "UFFI")
  (:export "CLINES" "DEFENTRY" "DEFLA" "DEFCBODY" "DEFINLINE" "C-INLINE" ;; extension to UFFI
	   "DEFCALLBACK" "CALLBACK"                                      ;; extension to UFFI
	   "FOREIGN"                                                     ;; extension to UFFI

	   ;; The UFFI Protocol
	   "DEF-CONSTANT" "DEF-FOREIGN-TYPE" "DEF-ENUM" "DEF-STRUCT"
	   "DEF-ARRAY-POINTER" "DEF-FUNCTION" "DEF-UNION" "DEF-ARRAY"
	   "ALLOCATE-FOREIGN-OBJECT" "FREE-FOREIGN-OBJECT" "MAKE-NULL-POINTER"
	   "GET-SLOT-VALUE" "GET-SLOT-POINTER" "DEREF-ARRAY" "DEREF-POINTER"
	   "POINTER-ADDRESS" "SIZE-OF-FOREIGN-TYPE" "DEF-FOREIGN-VAR"
	   "NULL-CHAR-P" "ENSURE-CHAR-CHARACTER" "ENSURE-CHAR-INTEGER"
	   "NULL-POINTER-P" "+NULL-CSTRING-POINTER+" "WITH-FOREIGN-OBJECTS"
	   "MAKE-POINTER" "CHAR-ARRAY-TO-POINTER" "CONVERT-TO-FOREIGN-STRING"
	   "CONVERT-FROM-FOREIGN-STRING" "ALLOCATE-FOREIGN-STRING" 
           "WITH-FOREIGN-STRING" "WITH-FOREIGN-STRINGS"
           "FOREIGN-STRING-LENGTH" "WITH-FOREIGN-OBJECT"
	   "FIND-FOREIGN-LIBRARY" "LOAD-FOREIGN-LIBRARY"
	   "ENSURE-CHAR-STORABLE" "DEF-TYPE"
	   "WITH-CSTRING" "CONVERT-TO-CSTRING" "CONVERT-FROM-CSTRING" "FREE-CSTRING"
           "WITH-CAST-POINTER" "WITH-CSTRINGS"

	   )
  (:import-from "SYS" "NULL-POINTER-P"))

(in-package "FFI")

#-mkcl-min
(clines "#include <string.h>")

(defmacro def-constant (name value &key (export nil))
  "Macro to define a constant and to export it"
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (defconstant ,name ,value)
     ,(when export (list 'export `(quote ,name)))
    ',name))

;;;----------------------------------------------------------------------
;;; FOREIGN TYPES
;;;
;;; The basic foreign types are one of:
;;;
;;;     <basic-type> = :byte | :unsigned-byte
;;;                  | :char | :unsigned-char
;;;                  | :short | :unsigned-short
;;;                  | :int | :unsigned-int
;;;                  | :long | :unsigned-long
;;;                  | :long-long | :unsigned-long-long
;;;                  | :data-pointer | :function-pointer
;;;                  | :pointer-void | :object | :cstring ;; Not really basic! Should be typedefs.
;;;                  | :float | :double | :long-double
;;;                  | :int8-t | :uint8-t | :int16-t | :uint16-t ;; Not really basic! Should be typedefs.
;;;                  | :int32-t | :uint32-t | :int64-t | :uint64-t ;; Not really basic! Should be typedefs.
;;;                  | :float-complex | :double-complex | :long-double-complex
;;;                  | :float-imaginary | :double-imaginary | :long-double-imaginary
;;;                  | :bool
;;;                  | :void
;;;
;;;    <qualified-type> = (<qualifier> <type>) | <type>
;;;
;;;    <qualifier> = :const
;;;                | :volatile
;;;                | :restrict
;;;                | :const-volatile
;;;                | :const-restrict
;;;                | :volatile-restrict
;;;                | :const-volatile-restrict
;;;

(deftype foreign () 'si:foreign)

(defvar *ffi-C-names* (make-hash-table :size 128 :test 'equal))

(defvar *ffi-C-tags* (make-hash-table :size 128 :test 'equal))

(defun undef-foreign-name (name)
  (remhash name *ffi-C-names*))

(defun undef-foreign-tag (tag)
  (remhash tag *ffi-C-tags*))

(defun undef-all-foreign-names () (setq *ffi-C-names* (make-hash-table :size 128 :test 'equal)))
(defun undef-all-foreign-tags () (setq *ffi-C-tags* (make-hash-table :size 128 :test 'equal)))

(defun foreign-elt-type-p (name)
  (and (symbolp name)
       (member name '(:byte :unsigned-byte :char :unsigned-char
                      :short :unsigned-short
		      :int :unsigned-int
		      :long :unsigned-long
		      :long-long :unsigned-long-long
		      :pointer-void :object :cstring
		      :float :double :long-double
                      :int8-t :uint8-t
		      :int16-t :unit16-t :int32-t :uint32-t :int64-t :uint64-t
                      #-(and) :float-complex
                      #-(and) :double-complex
                      #-(and) :long-double-complex
                      #-(and) :float-imaginary
                      #-(and) :double-imaginary
                      #-(and) :long-double-imaginary
		      )
	       :test 'eq)))

(defstruct (C-tag #+mkcl (:type vector))
  name
  type
  size
  alignement
  )

(defmacro def-C-tag (name type-spec size alignment)
  (setf (gethash name *ffi-C-tags*) (make-C-tag :name name :type type-spec :size size :alignment alignment))
  nil)

(defmacro def-foreign-type (name definition)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (gethash ',name ffi::*ffi-C-names*) ',definition)))

(defmacro def-type (name definition)
  (declare (ignore definition))
  `(deftype ,name () t))

(defun %convert-to-ffi-type (type &optional context)
  (if (atom type)
    (if (member type context)
      type
      (multiple-value-bind (value present-p) (gethash type *ffi-C-names* type)
        (if present-p
          (%convert-to-ffi-type value (cons type context))
          value)))
    (cons (%convert-to-ffi-type (first type) context)
          (%convert-to-ffi-type (rest type) context))))

(defmacro %align-data (data align)
  `(setf ,data (* (ceiling (/ ,data ,align)) ,align)))

(defun size-of-foreign-type (name)
  (let* ((size 0) align
	 (type (%convert-to-ffi-type name)))
    (unless type
      (error "Incomplete or unknown foreign type ~A" name))
    (cond ((symbolp type)
	   (setf size (si::size-of-foreign-elt-type type)))
	  ((atom type)
	   (error "~A is not a valid foreign type identifier" name))
	  ((eq (setf name (first type)) :struct)
	   (setf size (slot-position type nil))
	   (setf align 
		 (apply #'max 
			(mapcar 
			 #'(lambda (field)
			     (multiple-value-bind (field-size field-align)
				 (size-of-foreign-type (second field))
			       (declare (ignore field-size))
			       field-align))
			 (rest type))))
	   (%align-data size align))
	  ((eq name :array)
	   (unless (and (setf size (third type)) (realp size))
	     (error "Incomplete foreign type: ~S" type))
	   (multiple-value-bind (elt-size elt-align)
	     (size-of-foreign-type (second type))
	     (setf size (* size elt-size))
	     (setf align elt-align)))
	  ((eq name :union)
	   (dolist (field (rest type))
	     (multiple-value-bind (field-size field-align)
	       (size-of-foreign-type (second field))
	       (when (> field-size size)
	         (setf size field-size))
	       (when (or (null align) (> field-align align))
	         (setf align field-align)))))
          ((eq name :enum)
           (setf size (si::size-of-foreign-elt-type :int)))
	  ((eq name '*)
	   (setf size (si::size-of-foreign-elt-type :pointer-void)))
          ((eq name 'quote)
           (size-of-foreign-type (second type)))
	  (t
	   (error "~A does not denote a foreign data type" name)))
    (unless align
      (setf align size))
    (values size
	    #+x86 (min align
		       #+unix 4
		       #+windows 8) ;; Good only for default /Zp option
	    #+x86-64 align
	    #+arm align
	    #+aarch64 align
	    )
    ))

(defun allocate-foreign-object (type &optional (size 0 size-flag))
  (declare (fixnum size))
  (let ((type-size (size-of-foreign-type type)))
    (cond ((null size-flag)
	   (si::allocate-foreign-data type type-size))
	  ((>= size 0)
	   (let ((bytes (* size type-size)))
	     (si::allocate-foreign-data `(:array ,type ,size) bytes)))
	  (t
	   (error "~A is not a valid array dimension size" size)))))

(defun free-foreign-object (ptr)
  (si::free-foreign-data ptr))

;;;----------------------------------------------------------------------
;;; ENUMERATION TYPES
;;;
;;; The enumaration type is represented by the following list:
;;;
;;;     (:ENUM <name-tag> { (<enum-name> . <enum-value>) }*)
;;;
;;;   Where <name-tag> is a string designator, possibly empty when denoting anonymous 'enum's.
;;;   <enum-name> is also a string designator naming an enumeration constant.
;;;   <enum-value> is an integer.
;;;
;;; A list (:ENUM <name-tag>) denotes a reference to the type described here just above.
;;;

(defmacro def-enum (name values-list &key (separator-string "#"))
  (let ((constants '())
	(value -1)
	enum-constant
	forms)
    (setf separator-string (string separator-string))
    (dolist (item values-list)
      (cond ((or (symbolp item) (stringp item) (characterp item))
	     (setf enum-constant (sring item))
	     (incf value))
	    ((and (consp item)
		  (symbolp (setf enum-constant (first item)))
		  (integerp (setf value (second item)))
		  (endp (cddr item))))
	    (t
	     (error "Not a valid argument to DEF-ENUM~%~a" values-list)))
      (setf enum-constant (concatenate 'string
			       (symbol-name name)
			       separator-string
			       (string enum-constant)))
      (push `(defconstant ,(intern enum-constant (symbol-package name)) ',value) forms)
      )
    `(progn
       (def-foreign-type ,name :int)
       ,@forms)))

;;;----------------------------------------------------------------------
;;;

(defun %foreign-data-set (obj ndx type value)
  (cond ((foreign-elt-type-p type)
         (si::foreign-set-elt obj ndx type value))
	((atom type)
	 (error "Unknown foreign primitive type: ~A" type))
	((eq (first type) '*)
	 (si::foreign-set-elt obj ndx :pointer-void value))
	(t
	 (si::foreign-set obj ndx value))))

(defun %foreign-data-ref (obj ndx type size) ;;&optional (size 0 size-p))
  (cond ((foreign-elt-type-p type) ;; primitive types.
         (si::foreign-ref-elt obj ndx type))
	((atom type)
	 (error "Unknown foreign primitive type: ~A" type))
	((eq (first type) '*) ;; pointer types
	 (si::foreign-recast (si::foreign-ref-elt obj ndx :pointer-void)
	                          (size-of-foreign-type (second type))
				  type))
	(t ;; agregate types (:struct :union :array)
	 ;;(si::foreign-ref obj ndx (if size-p size (size-of-foreign-type type)) type)
	 (si::foreign-ref obj ndx size)
	 )))

;;;----------------------------------------------------------------------
;;; STRUCTURE TYPES
;;;
;;; The structure type is represented by the following list:
;;;
;;;	(:STRUCT <name-tag> (<field-name> <field-type> <byte-offset> . <bit-offset>)*)
;;;
;;;   Where <name-tag> is a string designator, possibly empty when denoting anonymous 'struct's.
;;;   <field-name> is also a string designator, also possibly empty when denoting an unnamed bit field.
;;;   <field-type> can be any previously established foreign type, or one of the
;;;   bit field specification, each qualified or not, such that:
;;;
;;;       <field-type> = (<qualifier> <unqualified-field-type>) | <unqualified-field-type>
;;;
;;;       <unqualified-field-type> = <any known foreign data type>
;;;                                | (:unsigned-char-bit-field <width>)
;;;                                | (:signed-char-bit-field <width>)
;;;                                | (:char-bit-field <width>)
;;;                                | (:unsigned-short-bit-field <width>)
;;;                                | (:signed-short-bit-field <width>)
;;;                                | (:short-bit-field <width>)
;;;                                | (:unsigned-int-bit-field <width>)
;;;                                | (:signed-int-bit-field <width>)
;;;                                | (:int-bit-field <width>)
;;;                                | (:unsigned-long-bit-field <width>)
;;;                                | (:signed-long-bit-field <width>)
;;;                                | (:long-bit-field <width>)
;;;                                | (:bool-bit-field <width>)
;;;
;;;   where <width> is a small positive integer in the range 0 to "number of bits in a machine word"
;;;   (typically 32 or 64, maybe 128 in some future). A <width> of 0 has special meaning in C.
;;;
;;; A list (:STRUCT <name-tag>) denotes a reference to the type described here just above.
;;;
;;; FIXME! We do not take care of slot alignment!
;;;

(defmacro def-struct (name &rest fields)
  (let ((struct-type (list :struct))
        (name-tag (string name))
	field-name
	field-type)
    (dolist (item (subst `(* (:struct ,name-tag)) :pointer-self fields)) ;; :pointer-self should be deprecated! JCB.
      (unless (and (consp item)
                   (= (length item) 2)
                   (symbolp (setf field-name (first item))))
	(error "Not a valid DEF-STRUCT field ~A" item))
      (setf field-type (second item))
      (push (list* field-name field-type 0 0) struct-type))
    `(def-C-tag ,name-tag ,(nreverse struct-type))))

(defun slot-position (type field)
  (setf type (%convert-to-ffi-type type))
  (let ((ndx 0)
	(is-union nil))
    (cond ((atom type)
	   (error "~A is not a foreign STRUCT or UNION type" type))
	  ((eq (first type) :struct))
	  ((eq (first type) :union)
	   (setf is-union t))
	  (t
	   (error "~A is not a foreign STRUCT or UNION type" type)))
    (dolist (slot (rest type))
      (let* ((slot-name (car slot))
	     (slot-type (cadr slot)))
	(multiple-value-bind (slot-size slot-align)
	  (size-of-foreign-type slot-type)
	  (%align-data ndx slot-align)
	  (when (eq slot-name field)
	    (return-from slot-position (values ndx slot-type slot-size))) ;; Normal return.
	  (unless is-union
	    (incf ndx slot-size)))))
    (values ndx nil nil))) ;; Pathological return where ndx is the size of the agregate type.
                           ;; It is also silent on the fact that "field" was not found in the set of fields of the agregate!

(defun get-slot-value (object struct-type field)
  (multiple-value-bind (slot-ndx slot-type slot-size)
      (slot-position struct-type field)
    (unless slot-size
      (error "~A is not a field of the type ~A" field struct-type))
    (%foreign-data-ref object slot-ndx slot-type slot-size)))

(defun (setf get-slot-value) (value object struct-type field)
  (multiple-value-bind (slot-ndx slot-type slot-size)
      (slot-position struct-type field)
    (unless slot-size
      (error "~A is not a field of the type ~A" field struct-type))
    (%foreign-data-set object slot-ndx slot-type value)))

(defun get-slot-pointer (object struct-type field)
  (multiple-value-bind (slot-ndx slot-type slot-size)
      (slot-position struct-type field)
    (unless slot-size
      (error "~A is not a field of the type ~A" field struct-type))
    (si::foreign-indexed object slot-ndx slot-size slot-type)))


;;;----------------------------------------------------------------------
;;; UNIONS
;;;
;;; The union type is represented by the following list:
;;;
;;;   (:UNION <name-tag> (<field-name> <field-type> . <bit-offset>)*)
;;;
;;; See the description of structure types here above for the definition of
;;; <name-tag>,<field-name> and <field-type>.
;;;
;;; A list (:UNION <name-tag>) denotes a reference to the type described here just above.
;;;

(defmacro def-union (name &rest fields)
  (let ((union-type (list :union))
        (name-tag (string name))
	field-name
	field-type)
    (dolist (item (subst `(* (:union ,name-tag)) :pointer-self fields)) ;; :pointer-self should be deprecated! JCB.
      (unless (and (consp item)
		   (= (length item) 2)
		   (symbolp (setf field-name (first item))))
	(error "Not a valid DEF-UNION field ~A" item))
      (setf field-type (second item))
      (push (list* field-name field-type 0) union-type))
    `(def-C-tag ,name-tag ,(nreverse union-type))))

;;;----------------------------------------------------------------------
;;; POINTERS
;;;
;;; A pointer type is represented by the following list:
;;;
;;;     (* <qualified-type>)
;;;

;(defvar +null-cstring-pointer+ (si:allocate-foreign-data :cstring 0))
(defvar +null-cstring-pointer+ (si:make-foreign-null-pointer))

(defun pointer-address (ptr)
  (si::foreign-address ptr))

(defun deref-pointer (ptr type)
  ;; FIXME! No checking!
  (setf type (%convert-to-ffi-type type))
  (cond ((foreign-elt-type-p type)
         (si::foreign-ref-elt ptr 0 type))
	((atom type)
	 (error "Unknown foreign primitive type: ~A" type))
	((eq (first type) '*)
	 (si::foreign-recast (si::foreign-ref-elt ptr 0 :pointer-void)
	                          (size-of-foreign-type (second type))
				  (second type)))
	(t
	 (error "Cannot dereference pointer to foreign data, ~A" ptr))
  ))

(defun (setf deref-pointer) (value ptr type)
  ;; FIXME! No checking!
  (setf type (%convert-to-ffi-type type))
  (if (foreign-elt-type-p type)
      (si::foreign-set-elt ptr 0 type value)
      (si::foreign-set ptr 0 value)))

(defun make-null-pointer (type)
  ;;(si::allocate-foreign-data type 0)
  (let ((null-ptr (si::make-foreign-null-pointer)))
    (if (eq type :void)
	null-ptr
      (si::foreign-recast null-ptr 0 type))))

(defun make-pointer (addr type)
  (c-inline (type (size-of-foreign-type type) addr)
	    #-(and windows (or x86-64 aarch64)) (:object :unsigned-long :unsigned-long)
	    #+(and windows (or x86-64 aarch64)) (:object :unsigned-long-long :unsigned-long-long)
	    :object
            "{
             mkcl_object output = mkcl_make_foreign(env, #0, #1);
             ((void **) output->foreign.data)[0] = (void *)#2;
             @(return) = output;
             }"
	    :side-effects t
	    :one-liner nil))


;;;----------------------------------------------------------------------
;;; ARRAYS
;;;
;;; An array type is represented by the following list:
;;;
;;;     (:ARRAY <qualified-element-type> [<dimension>])
;;;
;;;   Where <qualified-element-type> can be any established foreign data type, with qualification
;;;   or not, and <dimension> must be a fixnum integer constant or the symbol '*'.
;;;   An array without any <dimension> is an incomplete array
;;;   and can only occur as the last field of a :struct or as
;;;   type of a :function parameter.
;;;   If <dimension> is symbol '*' then the array is a variable length array
;;;   and can only appear as type of a :function parameter.
;;;

(defmacro def-array-pointer (name element-type) ;; Deprecated, as it is redundant and useless! JCB
  `(def-foreign-type ,name (* ,element-type)))

(defun deref-array (array array-type position)
  (setf array-type (%convert-to-ffi-type array-type))
  (let* ((element-type (second array-type))
	 (element-size (size-of-foreign-type element-type))
	 (ndx (* position element-size))
	 (length (or (third array-type) '*)))
    (unless (or (eq length '*)
		(> length position -1))
      (error "Out of bounds when accessing array ~A." array))
    (%foreign-data-ref (si::foreign-recast array (+ ndx element-size) array-type) ndx element-type element-size)))

(defun (setf deref-array) (value array array-type position)
  (setf array-type (%convert-to-ffi-type array-type))
  (let* ((element-type (second array-type))
	 (element-size (size-of-foreign-type element-type))
	 (ndx (* position element-size))
	 (length (or (third array-type) '*)))
    (unless (or (eq length '*)
		(> length position -1))
      (error "Out of bounds when accessing array ~A." array))
    (%foreign-data-set (si::foreign-recast array (+ ndx element-size) array-type) ndx element-type value)))


;;;----------------------------------------------------------------------
;;; CHARACTERS AND STRINGS
;;;
;;; MKCL always returns a character when dereferencing (:array :char *)
;;;

(defun null-char-p (char)
  (eql char #.(code-char 0)))

(defun ensure-char-character (char)
  (cond ((characterp char) char)
	((integerp char) (code-char char))
	(t (error "~a cannot be coerced to type CHARACTER" char))))

(defun ensure-char-integer (char)
  (cond ((characterp char) (char-code char))
	((integerp char) char)
	(t (error "~a cannot be coerced to type INTEGER" char))))

(defun ensure-char-storable (char)
  char)

(defun char-array-to-pointer (obj)
  (si::foreign-indexed obj 0 1 '(* :unsigned-char)))

(defmacro convert-from-cstring (object)
  object)

(defmacro convert-to-cstring (object)
  ;; This enforces that the string contains only as many characters as the
  ;; fill-pointer determines Since MKCL always sets a 0 character after the
  ;; last element of a string, this way, the string is always zero-terminated
  `(si:copy-to-simple-base-string ,object))

(defmacro free-cstring (object)
  object)

(defmacro with-cstring ((cstring string) &body body)
  `(let ((,cstring (convert-to-cstring ,string))) ,@body))

(defmacro with-cstrings (bindings &rest body)
  (if bindings
    `(with-cstring ,(car bindings)
      (with-cstrings ,(cdr bindings)
        ,@body))
    `(progn ,@body)))

(defun foreign-string-length (foreign-string)
  (c-inline (foreign-string) (t) :int
	    "strlen((#0)->foreign.data)"
	    :side-effects nil
	    :one-liner t))

(defun convert-from-foreign-string (foreign-string
				    &key length (null-terminated-p t))
  (cond ((and (not length) null-terminated-p)
	 (setf length (foreign-string-length foreign-string)))
	((not (integerp length))
	 (error "~A is not a valid string length" length)))
  (c-inline (foreign-string length) (t fixnum) string
       "{
	mkcl_index length = #1;
	mkcl_object output = mkcl_alloc_simple_base_string(env, length);
	memcpy(output->base_string.self, (#0)->foreign.data, length);
	@(return) = output;
	}"
       :one-liner nil
       :side-effects t))

(defun convert-to-foreign-string (string-designator)
  (let ((lisp-string (string string-designator))) ;; FIXME: There is no guarantee that this is going to be a base-string! JCB
    (c-inline (lisp-string) (t) t
       "{
	mkcl_object lisp_string = #0;
	mkcl_index size = lisp_string->base_string.fillp;
	mkcl_object output = mkcl_allocate_foreign_data(env, @(* :char), size+1);
	memcpy(output->foreign.data, lisp_string->base_string.self, size);
	output->foreign.data[size] = '\\0';
	@(return) = output;
	}"
	:one-liner nil
	:side-effects t)
    ))

(defun allocate-foreign-string (size &key unsigned)
  (si::allocate-foreign-data `(* ,(if unsigned :unsigned-char :char)) (1+ size)))

(defmacro with-foreign-string ((foreign-string lisp-string) &rest body)
  (let ((result (gensym)))
    `(let* ((,foreign-string (convert-to-foreign-string ,lisp-string))
            (,result (progn ,@body)))
       (free-foreign-object ,foreign-string)
       ,result)))

(defmacro with-foreign-strings (bindings &rest body)
  (if bindings
    `(with-foreign-string ,(car bindings)
      (with-foreign-strings ,(cdr bindings)
        ,@body))
    `(progn ,@body)))

;;;----------------------------------------------------------------------
;;; MACROLOGY
;;;

(defmacro with-foreign-object ((var type) &body body)
  `(let ((,var (allocate-foreign-object ,type)))
     (unwind-protect
	 (progn ,@body)
       (free-foreign-object ,var))))

(defmacro with-foreign-objects (bindings &rest body)
  (if bindings
    `(with-foreign-object ,(car bindings)
      (with-foreign-objects ,(cdr bindings)
        ,@body))
    `(progn ,@body)))

(defmacro with-cast-pointer (bind &body body)
  (let (binding-name ptr type)
    (case (length bind)
      (2 (setf binding-name (first bind)
	       ptr binding-name
	       type (second bind)))
      (3 (setf binding-name (first bind)
	       ptr (second bind)
	       type (third bind)))
      (otherwise (error "Arguments missing in WITH-CAST-POINTER")))
    ;;; This was doing the pointer recasting twice. JCB
    ;; `(let ((,binding-name (si::foreign-indexed (si::foreign-recast ,ptr (size-of-foreign-type ',type) :void) 0
    ;; 						    (size-of-foreign-type ',type)
    ;; 						    ',type)))
    `(let ((,binding-name (si::foreign-recast ,ptr (size-of-foreign-type ',type) ',type)))
       ,@body)))

;;;----------------------------------------------------------------------
;;; INTERFACE TO C FUNCTIONS AND VARIABLES
;;;
;;; The function type is represented by the following list:
;;;
;;;     (:FUNCTION <return-type> <argument-list>)
;;;
;;;   where,
;;;
;;;     <return-type> = <any known foreign type> ;; but not :function or :array
;;;
;;;     <argument-list> = ( <arg-type> { <arg-type> }* ) | ()
;;;
;;;     <arg-type> = <qualified-type> | &...
;;;
;;;   An empty () argument-list denotes an old-style non-prototype function.
;;;
;;;   In a new-style prototype-based function argument-list, &... denotes a variadic function
;;;   and &... must be the last (rightmost) item of the argument-list.
;;;   An <argument-list> that consists of ( :void ) denotes a new-style prototype-based
;;;   function of zero argument and is the only way :void can be used as a valid <arg-type>.
;;;

(defun map-name-from-c-to-lisp (name)
  (cond ((or (stringp name)
             (symbolp name))
	 (values name (intern (string-upcase (substitute #\- #\_ (string name))))))
	((and (consp name)
	      (= (length name) 2))
	 (values (first name) (second name)))))

(defun %convert-to-arg-type (type)
  (let ((type (%convert-to-ffi-type type)))
    (cond ((atom type) type)
          ((eq (first type) '*) :pointer-void) ;; Cute dumbing down! JCB
	  ((eq (first type) :array) :pointer-void) ;; Cute dumbing down! JCB
          ((eq (first type) :function) :pointer-void) ;; Cute dumbing down! JCB
	  (t (error "Unsupported argument type: ~A" type))
    )))

(defun %convert-to-return-type (type)
  (let ((type (%convert-to-ffi-type type)))
    (cond ((atom type) type)
          ((eq (first type) '*) (second type)) ;; This one looks suspicious! JCB
	  (t type))))

(defun produce-function-call (c-name nargs) ;; Isn't there here a limit on the number of arguments that goes unchecked? JCB
  (format nil "~a(~a)" c-name               ;; Well, it goes somewhat checked by subseq but the error message is cryptic! JCB
	  (subseq "#0,#1,#2,#3,#4,#5,#6,#7,#8,#9,#a,#b,#c,#d,#e,#f,#g,#h,#i,#j,#k,#l,#m,#n,#o,#p,#q,#r,#s,#t,#u,#v,#w,#x,#y,#z"
		  0 (max 0 (1- (* nargs 3)))))) ;; There is 3 characters per argument specifier. JCB

;;; FIXME! We should turn this into a closure generator that produces no code.
(defmacro def-lib-function (name args &key returning module)
  (multiple-value-bind (c-name lisp-name) (map-name-from-c-to-lisp name)
    (let* ((return-type (ffi::%convert-to-return-type returning))
	   (return-required (not (eq return-type :void)))
	   (argtypes (mapcar #'(lambda (a) (ffi::%convert-to-arg-type (second a))) args)))
      `(let ((c-fun (si::find-foreign-symbol ',c-name ,module :pointer-void 0)))
	(defun ,lisp-name ,(mapcar #'first args)
	  (si::call-cfun c-fun ',return-type ',argtypes (list ,@(mapcar #'first args))))))))

(defmacro def-function (name args &key module (returning :void))
  (when module
    (return-from def-function
      `(def-lib-function ,name ,args :returning ,returning :module ,module)))
  (multiple-value-bind (c-name lisp-name)
      (map-name-from-c-to-lisp name)
    (let* ((arguments (mapcar #'first args))
	   (arg-types (mapcar #'(lambda (type) (%convert-to-arg-type (second type))) args))
	   (return-type (%convert-to-return-type returning))
	   (nargs (length arguments))
	   (c-string (produce-function-call c-name nargs))
	   (casting-required (not (or (member return-type '(:void :cstring))
				      (foreign-elt-type-p return-type))))
	   (inline-form `(c-inline ,arguments ,arg-types
				   ,(if casting-required :pointer-void return-type)
				   ,c-string
				   :one-liner t
				   :side-effects t)))
      (when casting-required
	(setf inline-form
	      `(si::foreign-recast ,inline-form
					(size-of-foreign-type ',return-type)
					',return-type)))
      (when (> nargs 36)
	(error "FFI can only handle C functions with up to 36 arguments"))
      `(defun ,lisp-name (,@arguments)
	 ,inline-form)
      )))

(defmacro def-foreign-var (name qualified-type module)
  (multiple-value-bind (c-name lisp-name)
      (map-name-from-c-to-lisp name)
    (let* ((ffi-type (%convert-to-ffi-type qualified-type))
           (can-deref (or (foreign-elt-type-p ffi-type)
                          (and (consp ffi-type)
                               (member (first ffi-type) '(* :array)))))
	   (inline-form (cond (module
			       `(si::find-foreign-symbol ,c-name ,module ',qualified-type
                                                         ,(size-of-foreign-type qualified-type)))
			      (t
			       `(c-inline () () :object
					  ,(format nil "mkcl_make_foreign(env, @~S, ~A, &~A)"
						   qualified-type (size-of-foreign-type qualified-type) c-name)
					  :side-effects t :one-liner t)))))
      (if can-deref
          `(progn
             (si::put-sysprop ',lisp-name 'ffi-foreign-var ,inline-form)
             (eval-when (:compile-toplevel :load-toplevel :execute)
               (define-symbol-macro ,lisp-name
                 (ffi:deref-pointer (si::get-sysprop ',lisp-name 'ffi-foreign-var) ',qualified-type)
                 )))
          `(defvar ,lisp-name ,inline-form))
      )))

(defun find-foreign-library (names directories &key drive-letters types)
  (unless (listp names)
    (setq names (list names)))
  (unless (listp directories)
    (setq directories (list directories)))
  (unless types
    (setq types #+windows '("lib")
                #+unix '("so" "a")))
  (unless (listp types)
    (setq types (list types)))
  (unless (listp drive-letters)
    (setq drive-letters (list drive-letters)))
  #-msvc
  (setq drive-letters '(nil))
  #+msvc
  (unless drive-letters
    (setq drive-letters '(nil)))
  (dolist (d drive-letters)
    (dolist (p directories)
      (dolist (n names)
        (dolist (e types)
	  (let ((full-path (probe-file (make-pathname
					  :device d
	                                  :directory (etypecase p
					               (pathname (pathname-directory p))
						       (string (pathname-directory (parse-namestring p)))
						       (list p))
					  :name n
					  :type e))))
	    (when full-path
	      (return-from find-foreign-library full-path))
	  )))))
  nil)

(defvar *referenced-libraries* nil) ;; used (read) by the CMP compiler during link phase.

(defun do-load-foreign-library (tmp)
  (let* ((path (if (pathnamep tmp) tmp (pathname (string tmp))))
	 (filename (namestring path))
	 )
    (unless (find filename ffi::*referenced-libraries* :test #+unix #'string= #+windows #'string-equal)
      (push filename ffi::*referenced-libraries*)
      t)))
  
(defmacro load-foreign-library (filename &key module supporting-libraries force-load)
  (declare (ignore module force-load supporting-libraries))
  (let ((compile-form (and (constantp filename)
			   `(eval-when (:compile-toplevel)
				       (do-load-foreign-library ,filename))))
	(dyn-form `(si:load-foreign-module ,filename)))
    (or compile-form dyn-form)
    ))

;;;----------------------------------------------------------------------
;;; CALLBACKS
;;;


(defmacro defcallback (name ret-type arg-desc &body body)
  (multiple-value-bind (name call-type) (if (consp name)
					    (values-list name)
					  (values name :cdecl))
                       (declare (ignorable call-type))
    ;;(format t "~&Expanding macro defcallback on ~S.~%" name)
    (let ((arg-types (mapcar #'second arg-desc))
	  (arg-names (mapcar #'first arg-desc)))
      `(si::make-dynamic-callback
	#'(si::lambda-block ,name ,arg-names ,@body) ',name ',ret-type ',arg-types))))

(defun callback (name)
  (let ((x (si::get-sysprop name :callback)))
    (unless x
      (error "There is no callback with name ~a" name))
    (first x)))

;;;----------------------------------------------------------------------
;;; COMPATIBILITY WITH OLDER FFI
;;;

(defun clines (&rest args)
  (declare (ignore args))
  ;;(error "The special form clines cannot be used in the interpreter: ~A" args) ;; why be so anal?
  )

(eval-when (:load-toplevel :execute)
  (defmacro c-inline (args arg-types ret-type &rest others)
    `(error "The special form c-inline cannot be used in the interpreter: ~A"
      (list (list ,@args) ',arg-types ',ret-type ,@others))))

(defmacro definline (fun arg-types type code)
"Syntax: (definline symbol ({arg-type}*) value-type body)" "

DEFINLINE behaves like a DEFCBODY (see), but also instructs the LISP compiler
to expand inline any call to function SYMBOL into code corresponding
to the C language expression BODY, whenever it can determine that
the actual arguments are of the specified type."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
              ;; defCbody must go first, because it clears symbol-plist of fun
              (defCbody ,fun ,arg-types ,type ,code)
              (declaim (ftype (function ,arg-types ,type) ,fun))
	      (def-inline ,fun :always ,arg-types ,type ,code)))

(defmacro defla (&rest body)
"Syntax: (defla name lambda-list {decl | doc}* {form}*)" "

Used to DEFine Lisp Alternative.  For the interpreter, DEFLA is equivalent to
DEFUN, but the compiler ignores this form."
  `(eval-when (:execute)
     (defun ,@body)))

(defmacro defcbody (name arg-types result-type C-expr)
"Syntax: (defcbody symbol ({arg-type}*) value-type body)" "

The compiler defines a Lisp function named by SYMBOL whose body consists of the
C code of the string BODY. In the BODY one can reference the arguments of the
function as \"#0\", \"#1\", etc.
The interpreter ignores this form.  ARG-TYPEs are argument types of the
defined Lisp function and VALUE-TYPE is its the return type."
  (let ((args (mapcar #'(lambda (x) (declare (ignore x)) (gensym)) arg-types)))
  `(defun ,name ,args
     (c-inline ,args ,arg-types ,result-type
	       ,C-expr :one-liner t))))

(defmacro defentry (name arg-types c-name &key no-interrupts)
"Syntax: (defentry symbol ({arg-type}*) (value-type function-name))

The compiler defines a Lisp function named by SYMBOL whose body consists of a
calling sequence to the C language function named by FUNCTION-NAME.  The
interpreter ignores this form.  ARG-TYPEs are argument types of the C function
and VALUE-TYPE is the return type of the C function.  Symbols OBJECT, INT,
CHAR, CHAR*, FLOAT, DOUBLE are allowed for these types."
  (let ((output-type :object)
	(args (mapcar #'(lambda (x) (declare (ignore x)) (gensym)) arg-types)))
    (if (consp c-name)
	(setf output-type (first c-name)
	      c-name (second c-name)))
    (let* ((call (produce-function-call (string c-name) (length arg-types)))
	   (full-text (if no-interrupts
			  #-(and)
			  (concatenate 'string
				       "mkcl_disable_interrupts(env);@(return)="
				       call
				       ";mkcl_enable_interrupts(env);")
			  #+(and)
			  (concatenate 'string "MKCL_NO_INTR(env, (@(return)=" call "));")
			  call)))
      `(defun ,name ,args
	 (c-inline ,args ,arg-types ,output-type
		   ,full-text
		   :one-liner ,(not no-interrupts))))))

