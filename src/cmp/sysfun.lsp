;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: C -*-
;;;;
;;;  CMPSYSFUN   Database for system functions.
;;;
;;;  Copyright (c) 2010-2022, Jean-Claude Beaudoin.
;;;  Copyright (c) 2003, Juan Jose Garcia Ripoll.
;;;  Copyright (c) 1991, Giuseppe Attardi. All rights reserved.
;;;
;;;    This program is free software; you can redistribute it and/or
;;;    modify it under the terms of the GNU Lesser General Public
;;;    License as published by the Free Software Foundation; either
;;;    version 3 of the License, or (at your option) any later version.
;;;
;;;    See file '../../Copyright' for full details.


;;;
;;; DATABASE OF FUNCTION PROCLAMATIONS AND INLINE EXPANSIONS
;;;
;;; What follows is the complete list of function type proclamations for the
;;; most important functions in the MKCL core library, together with some useful
;;; inline expansions.
;;;
;;; The function proclamations are created with PROCLAIM-FUNCTION, as in
;;;
;;;	(PROCLAIM-FUNCTION function-name ([arg-type]*) return-type
;;;		&key no-sp-change predicate no-side-effects)
;;;
;;; with the following interpretation: ARG-TYPE and RETURN-TYPE denote the most
;;; general types for the input and output values of this function. If the
;;; compiler detects that some of the values passed to this function does not
;;; match these types, it will generate an error. NO-SP-CHANGE should be
;;; supplied if the function is known to not change any special variable. A more
;;; strict declaration is NO-SIDE-EFFECTS which means that the function's output
;;; does only depend in the input values, that these input values are not
;;; changed, and that under normal conditions (i.e. no error signaled) the
;;; function has no side effect (i.e. does not change global variables, does not
;;; perform input/output, etc). Notice that allocating memory and creating new
;;; elementary objects (i.e. conses, floats, integers, etc) is not considered a
;;; side effect, while creating other objects (classes, streams, structures) is.
;;;
;;; Inline expansions, on the other hand, have the following syntax
;;;
;;;	(DEF-INLINE function-name kind ([arg-type]*) return-rep-type
;;;		expansion-string)
;;;
;;; Here, ARG-TYPE is the list of argument types belonging to the lisp family,
;;; while RETURN-REP-TYPE is a representation type, i.e. the C type of the
;;; output expression. EXPANSION-STRING is a C/C++ expression template, like the
;;; ones used by C-INLINE. Finally, KIND can be :ALWAYS, :SAFE or :UNSAFE,
;;; depending on whether the inline expression should be applied always, in safe
;;; or in unsafe compilation mode, respectively.
;;;
;;;
;;;  Usage consideration for PROCLAIM-FUNCTION and DEF-INLINE. (JCB)
;;;
;;;  Both these macros are pure side-effect producers that expand to NIL.
;;;  It is therefore useless to compile their invocation since all
;;;  their side-effects will stay in the compiler instance that compiled
;;;  them. To be of any useful effect they need to be "interpreted"
;;;  in the same context than the compiler instance that will benefit from them.
;;;


;;; Here :fixnum stands for the machine-level representational type that is used
;;; to unbox the Common Lisp type FIXNUM, the two types are not completely
;;; equivalent since :fixnum (usually mapped to a mkcl_word) has a somewhat wider
;;; range of values. See cmpffi.lsp for a list of representational types
;;; available for use in def-inline. JCB

;;; While writing a DEF-INLINE clause you should be aware that in its argument
;;; list only unboxed types can be considered as "safe", any other type will
;;; end up being mapped to :object and will thus skip any type validation
;;; either on function entry or assignment. During inlining only the following
;;; types are unboxed: FIXNUM, BASE-CHAR, CHARACTER, SINGLE-FLOAT, DOUBLE-FLOAT,
;;; LONG-FLOAT. JCB

(in-package "COMPILER")

(defmacro proclaim-function (name arg-types return-type
				  &key no-sp-change predicate no-side-effects)
  (unless (eq '* arg-types)
    (multiple-value-setq (required-arg-count arg-types) (si::validate-arg-typespec arg-types)))
  (unless (eq '* return-type)
    (setq return-type (si::validate-function-return-type return-type)))
  (let ((finfo (get-sysprop name 'SI::PROCLAIMED-FUNCTION-INFORMATION)))
    (if finfo
	(progn
	  (si::set-proclaimed-function-arg-types finfo arg-types)
	  (si::set-proclaimed-function-return-type finfo return-type)
	  (si::set-proclaimed-function-required-arg-count finfo required-arg-count)
	  )
      (setq finfo (si::make-proclaimed-function
		   :name name :arg-types arg-types :return-type return-type
		   :required-arg-count required-arg-count)))
    (put-sysprop name 'SI::PROCLAIMED-FUNCTION-INFORMATION finfo))

  (when no-sp-change
    (put-sysprop name 'no-sp-change t))
  (when predicate
    (put-sysprop name 'predicate t))
  (when no-side-effects
    (put-sysprop name 'no-side-effects t))
  (rem-sysprop name ':inline-always)
  (rem-sysprop name ':inline-safe)
  (rem-sysprop name ':inline-unsafe)
  nil)

(defconstant +rep-type-canonical-map+
  #+(and (or x86-64 aarch64) (not windows))
  '((:int8-t . :byte) (:uint8-t . :unsigned-byte)
    (:int16-t . :short) (:uint16-t . :unsigned-short)
    (:int32-t . :int) (:uint32-t . :unsigned-int)
    (:int64-t . :long) (:uint64-t . :unsigned-long)
    (:unsigned-char . :char)
    #-long-float
    (:long-double . :double)
    (t . :object)
    )
  #+(or x86 arm windows)
  '((:int8-t . :byte) (:uint8-t . :unsigned-byte)
    (:int16-t . :short) (:uint16-t . :unsigned-short)
    (:int32-t . :int) (:uint32-t . :unsigned-int)
    (:int64-t . :long-long) (:uint64-t . :unsigned-long-long)
    (:unsigned-char . :char)
    #-long-float
    (:long-double . :double)
    (t . :object)
    )
  )

(defun canonical-rep-type (rep-type)
  (or (cdr (assoc rep-type +rep-type-canonical-map+)) rep-type))

(defmacro def-inline (name safety arg-types return-rep-type expansion
                      &key (one-liner t) (exact-return-type nil)
		      &aux arg-rep-types)
  (setf safety
	(case safety
	  (:unsafe :inline-unsafe)
	  (:safe :inline-safe)
	  (:always :inline-always)
	  (t (error "In DEF-INLINE, wrong value of SAFETY"))))
  (setf arg-rep-types
	(mapcar #'(lambda (x) (if (eq x '*) x (lisp-type->rep-type x)))
		arg-types))
  (when (eq return-rep-type t)
    (setf return-rep-type :object))
  (setq return-rep-type (canonical-rep-type return-rep-type))
  (let* ((return-type (rep-type->lisp-type return-rep-type))
         (inline-info
          (make-inline-info :arg-rep-types arg-rep-types
                            :return-rep-type return-rep-type
                            :return-type (rep-type->lisp-type return-rep-type)
                            :arg-types arg-types
                            :exact-return-type exact-return-type
                            :one-liner one-liner
                            :expansion expansion))
         (previous (get-sysprop name safety)))
    (declare (ignorable return-type))
    #+(or)
    (loop for i in previous
       when (and (equalp (inline-info-arg-types i) arg-types)
                 (not (equalp return-type (inline-info-return-type i))))
       do (format t "~&;;; Redundand inline definition for ~A~&;;; ~<~A~>~&;;; ~<~A~>"
                  name i inline-info))
    (put-sysprop name safety (cons inline-info previous)))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; AUXILIARY TYPES
;;

(deftype string-designator () '(or string symbol character))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ALL FUNCTION DECLARATIONS AND INLINE FORMS
;;;

(defparameter +all-optimizers+
  '(
    (proclaim-function si:make-pure-array (t t t t t t) array)
    (proclaim-function si:make-vector (t t t t t t) vector)
    (proclaim-function aref (array &rest t) t :no-side-effects t)

    (def-inline aref :always (array t) t "mkcl_aref(env, #0, #1)")
    (def-inline aref :always (array fixnum) t "mkcl_aref_index(env, #0,#1)")

    (def-inline aref :always ((array t) fixnum) t "mkcl_aref_index_object(env, #0, #1)")
    (def-inline aref :always ((array fixnum) fixnum) t "mkcl_aref_index_fixnum(env, #0, #1)")
    (def-inline aref :always ((array double-float) fixnum) t "mkcl_aref_index_df(env, #0, #1)")
    (def-inline aref :always ((array single-float) fixnum) t "mkcl_aref_index_sf(env, #0, #1)")
    (def-inline aref :always ((array bit) fixnum) t "mkcl_aref_index_bit(env, #0, #1)")
    (def-inline aref :always ((array mkcl:cl-index) fixnum) t "mkcl_aref_index_index(env, #0, #1)")
    (def-inline aref :always ((array mkcl:cl-word) fixnum) t "mkcl_aref_index_word(env, #0, #1)")
    #+unicode
    (def-inline aref :always ((array character) fixnum) t "mkcl_aref_index_ch(env, #0, #1)")
    #+unicode
    (def-inline aref :always (string fixnum) t "mkcl_character_index(env, #0, #1)")
    (def-inline aref :always ((array base-char) fixnum) t "mkcl_aref_index_bc(env, #0, #1)")
    (def-inline aref :always (base-string fixnum) t "mkcl_base_char_index(env, #0, #1)")
    (def-inline aref :always ((array mkcl:natural64) fixnum) t "mkcl_aref_index_b64(env, #0, #1)")
    (def-inline aref :always ((array mkcl:integer64) fixnum) t "mkcl_aref_index_i64(env, #0, #1)")
    (def-inline aref :always ((array mkcl:natural32) fixnum) t "mkcl_aref_index_b32(env, #0, #1)")
    (def-inline aref :always ((array mkcl:integer32) fixnum) t "mkcl_aref_index_i32(env, #0, #1)")
    (def-inline aref :always ((array mkcl:natural16) fixnum) t "mkcl_aref_index_b16(env, #0, #1)")
    (def-inline aref :always ((array mkcl:integer16) fixnum) t "mkcl_aref_index_i16(env, #0, #1)")
    (def-inline aref :always ((array mkcl:natural8) fixnum) t "mkcl_aref_index_b8(env, #0, #1)")
    (def-inline aref :always ((array mkcl:integer8) fixnum) t "mkcl_aref_index_i8(env, #0, #1)")
    
    (def-inline aref :always ((array fixnum) fixnum) :fixnum "mkcl_aref_index_fixnum_raw(env, #0, #1) /*0*/")
    (def-inline aref :always ((array double-float) fixnum) :double "mkcl_aref_index_df_raw(env, #0, #1)")
    (def-inline aref :always ((array single-float) fixnum) :float "mkcl_aref_index_sf_raw(env, #0, #1)")
    (def-inline aref :always ((array bit) fixnum) :fixnum "mkcl_aref_index_bit_raw(env, #0, #1)")
    (def-inline aref :always ((array mkcl:cl-index) fixnum) :cl-index "mkcl_aref_index_index_raw(env, #0, #1)")
    (def-inline aref :always ((array mkcl:cl-word) fixnum) :fixnum "mkcl_aref_index_word_raw(env, #0, #1)")
    #+unicode
    (def-inline aref :always ((array character) fixnum) :wchar "mkcl_aref_index_ch_raw(env, #0, #1)")
    #+unicode
    (def-inline aref :always (string fixnum) :wchar "mkcl_character_index_raw(env, #0, #1)")
    (def-inline aref :always ((array base-char) fixnum) :char "mkcl_aref_index_bc_raw(env, #0, #1)")
    (def-inline aref :always (base-string fixnum) :char "mkcl_base_char_index_raw(env, #0, #1)")
    (def-inline aref :always ((array mkcl:natural64) fixnum) :uint64-t "mkcl_aref_index_b64_raw(env, #0, #1)")
    (def-inline aref :always ((array mkcl:integer64) fixnum) :int64-t "mkcl_aref_index_i64_raw(env, #0, #1)")
    #+(or x86-64 aarch64)
    (def-inline aref :always ((array mkcl:natural32) fixnum) :fixnum "mkcl_aref_index_b32_raw(env, #0, #1)")
    (def-inline aref :always ((array mkcl:integer32) fixnum) :fixnum "mkcl_aref_index_i32_raw(env, #0, #1)")
    (def-inline aref :always ((array mkcl:natural32) fixnum) :uint32-t "mkcl_aref_index_b32_raw(env, #0, #1)")
    (def-inline aref :always ((array mkcl:integer32) fixnum) :int32-t "mkcl_aref_index_i32_raw(env, #0, #1)")
    (def-inline aref :always ((array mkcl:natural16) fixnum) :fixnum "mkcl_aref_index_b16_raw(env, #0, #1) /*2*/")
    (def-inline aref :always ((array mkcl:integer16) fixnum) :fixnum "mkcl_aref_index_i16_raw(env, #0, #1) /*2*/")
    (def-inline aref :always ((array mkcl:natural16) fixnum) :uint16-t "mkcl_aref_index_b16_raw(env, #0, #1)")
    (def-inline aref :always ((array mkcl:integer16) fixnum) :int16-t "mkcl_aref_index_i16_raw(env, #0, #1)")
    (def-inline aref :always ((array mkcl:natural8) fixnum) :fixnum "mkcl_aref_index_b8_raw(env, #0, #1) /*2*/")
    (def-inline aref :always ((array mkcl:integer8) fixnum) :fixnum "mkcl_aref_index_i8_raw(env, #0, #1) /*2*/")
    (def-inline aref :always ((array mkcl:natural8) fixnum) :uint8-t "mkcl_aref_index_b8_raw(env, #0, #1) /*0*/")
    (def-inline aref :always ((array mkcl:integer8) fixnum) :int8-t "mkcl_aref_index_i8_raw(env, #0, #1) /*0*/")

    (def-inline aref :unsafe (t t t) t
      "@0;mkcl_aref_index(env, #0,mkcl_fixnum_to_word(#1)*(#0)->array.dims[1]+mkcl_fixnum_to_word(#2))")
    (def-inline aref :unsafe ((array t) t t) t
      "@0;(#0)->array.self.t[mkcl_fixnum_to_word(#1)*(#0)->array.dims[1]+mkcl_fixnum_to_word(#2)]")
    (def-inline aref :unsafe ((array fixnum) t t) t
      "@0;MKCL_MAKE_FIXNUM((#0)->array.self.word[mkcl_fixnum_to_word(#1)*(#0)->array.dims[1]+mkcl_fixnum_to_word(#2)])")
    (def-inline aref :unsafe ((array fixnum) t t) :fixnum
      "@0;((#0)->array.self.word[mkcl_fixnum_to_word(#1)*(#0)->array.dims[1]+mkcl_fixnum_to_word(#2)])")
    (def-inline aref :unsafe ((array bit) t t) t
      "@0;mkcl_bvref_index(env, #0,mkcl_fixnum_to_word(#1)*(#0)->array.dims[1]+mkcl_fixnum_to_word(#2))")
    (def-inline aref :unsafe ((array t) fixnum fixnum) :object
      "@0;(#0)->array.self.t[(#1)*(#0)->array.dims[1]+(#2)]")
    (def-inline aref :unsafe ((array fixnum) fixnum fixnum) :object
      "@0;MKCL_MAKE_FIXNUM((#0)->array.self.word[(#1)*(#0)->array.dims[1]+(#2)])")
    (def-inline aref :unsafe ((array fixnum) fixnum fixnum) :fixnum
      "@0;((#0)->array.self.word[(#1)*(#0)->array.dims[1]+(#2)])")
    (def-inline aref :unsafe ((array bit) fixnum fixnum) :fixnum
      "@0;mkcl_bvref_index_raw(env, #0,(#1)*(#0)->array.dims[1]+(#2))")
    #+unicode
    (def-inline aref :unsafe ((array character) fixnum fixnum) :wchar
      "@0;(#0)->array.self.ch[(#1)*(#0)->array.dims[1]+(#2)]")
    (def-inline aref :unsafe ((array base-char) fixnum fixnum) :char
      "@0;(#0)->array.self.bc[(#1)*(#0)->array.dims[1]+(#2)]")
    (def-inline aref :unsafe ((array double-float) fixnum fixnum) :double
      "@0;(#0)->array.self.df[(#1)*(#0)->array.dims[1]+(#2)]")
    (def-inline aref :unsafe ((array single-float) fixnum fixnum) :float
      "@0;(#0)->array.self.sf[(#1)*(#0)->array.dims[1]+(#2)]")
    (def-inline aref :unsafe ((array mkcl:cl-index) fixnum fixnum) :cl-index
      "@0;(#0)->array.self.index[(#1)*(#0)->array.dims[1]+(#2)]")
    (def-inline aref :unsafe ((array mkcl:cl-word) fixnum fixnum) :fixnum
      "@0;(#0)->array.self.word[(#1)*(#0)->array.dims[1]+(#2)]")
    (def-inline aref :unsafe ((array mkcl:natural64) fixnum fixnum) :uint64-t
      "@0;(#0)->array.self.b64[(#1)*(#0)->array.dims[1]+(#2)]")
    (def-inline aref :unsafe ((array mkcl:integer64) fixnum fixnum) :int64-t
      "@0;(#0)->array.self.i64[(#1)*(#0)->array.dims[1]+(#2)]")
    #+(or x86-64 aarch64)
    (def-inline aref :unsafe ((array mkcl:natural32) fixnum fixnum) :fixnum
      "@0;(#0)->array.self.b32[(#1)*(#0)->array.dims[1]+(#2)]")
    (def-inline aref :unsafe ((array mkcl:integer32) fixnum fixnum) :fixnum
      "@0;(#0)->array.self.i32[(#1)*(#0)->array.dims[1]+(#2)]")
    (def-inline aref :unsafe ((array mkcl:natural32) fixnum fixnum) :uint32-t
      "@0;(#0)->array.self.b32[(#1)*(#0)->array.dims[1]+(#2)]")
    (def-inline aref :unsafe ((array mkcl:integer32) fixnum fixnum) :int32-t
      "@0;(#0)->array.self.i32[(#1)*(#0)->array.dims[1]+(#2)]")
    (def-inline aref :unsafe ((array mkcl:natural16) fixnum fixnum) :fixnum
      "@0;(#0)->array.self.b16[(#1)*(#0)->array.dims[1]+(#2)]")
    (def-inline aref :unsafe ((array mkcl:integer16) fixnum fixnum) :fixnum
      "@0;(#0)->array.self.i16[(#1)*(#0)->array.dims[1]+(#2)]")
    (def-inline aref :unsafe ((array mkcl:natural16) fixnum fixnum) :uint16-t
      "@0;(#0)->array.self.b16[(#1)*(#0)->array.dims[1]+(#2)]")
    (def-inline aref :unsafe ((array mkcl:integer16) fixnum fixnum) :int16-t
      "@0;(#0)->array.self.i16[(#1)*(#0)->array.dims[1]+(#2)]")
    (def-inline aref :unsafe ((array mkcl:natural8) fixnum fixnum) :fixnum
      "@0;(#0)->array.self.b8[(#1)*(#0)->array.dims[1]+(#2)]")
    (def-inline aref :unsafe ((array mkcl:integer8) fixnum fixnum) :fixnum
      "@0;(#0)->array.self.i8[(#1)*(#0)->array.dims[1]+(#2)]")
    (def-inline aref :unsafe ((array mkcl:natural8) fixnum fixnum) :uint8-t
      "@0;(#0)->array.self.b8[(#1)*(#0)->array.dims[1]+(#2)]")
    (def-inline aref :unsafe ((array mkcl:integer8) fixnum fixnum) :int8-t
      "@0;(#0)->array.self.i8[(#1)*(#0)->array.dims[1]+(#2)]")

    (def-inline aref :unsafe (t t) t "mkcl_vref_index(env, #0,mkcl_fixnum_to_word(#1))")
    (def-inline aref :unsafe ((array t) t) :object "((#0)->array.self.t[mkcl_fixnum_to_word(#1)])")
    (def-inline aref :unsafe ((array fixnum) t) :object "MKCL_MAKE_FIXNUM((#0)->array.self.word[mkcl_fixnum_to_word(#1)])")
    (def-inline aref :unsafe ((array fixnum) t) :fixnum "((#0)->array.self.word[mkcl_fixnum_to_word(#1)])")
    (def-inline aref :unsafe ((array t) fixnum) :object "((#0)->array.self.t[#1])")
    (def-inline aref :unsafe ((array fixnum) fixnum) :object "MKCL_MAKE_FIXNUM((#0)->array.self.word[#1])")
    (def-inline aref :unsafe ((array fixnum) fixnum) :fixnum "((#0)->array.self.word[#1])")
    (def-inline aref :unsafe ((array bit) t) t "mkcl_bvref_index(env, #0,mkcl_fixnum_to_word(#1))") ;;not quite unsafe
    (def-inline aref :unsafe ((array bit) fixnum) :fixnum "mkcl_bvref_index_raw(env, #0,#1)") ;;not quite unsafe
    #+unicode
    (def-inline aref :unsafe ((array character) fixnum) :wchar "(#0)->string.self[#1]")
    (def-inline aref :unsafe ((array base-char) fixnum) :char "(#0)->base_string.self[#1]")
    (def-inline aref :unsafe ((array double-float) fixnum) :double "(#0)->array.self.df[#1]")
    (def-inline aref :unsafe ((array single-float) fixnum) :float "(#0)->array.self.sf[#1]")
    (def-inline aref :unsafe ((array mkcl:cl-index) fixnum) :cl-index "(#0)->array.self.index[#1]")
    (def-inline aref :unsafe ((array mkcl:cl-word) fixnum) :fixnum "(#0)->array.self.word[#1]")
    (def-inline aref :unsafe ((array mkcl:natural64) fixnum) :uint64-t "(#0)->array.self.b64[#1]")
    (def-inline aref :unsafe ((array mkcl:integer64) fixnum) :int64-t "(#0)->array.self.i64[#1]")
    #+(or x86-64 aarch64)
    (def-inline aref :unsafe ((array mkcl:natural32) fixnum) :fixnum "(#0)->array.self.b32[#1]")
    (def-inline aref :unsafe ((array mkcl:integer32) fixnum) :fixnum "(#0)->array.self.i32[#1]")
    (def-inline aref :unsafe ((array mkcl:natural32) fixnum) :uint32-t "(#0)->array.self.b32[#1]")
    (def-inline aref :unsafe ((array mkcl:integer32) fixnum) :int32-t "(#0)->array.self.i32[#1]")
    (def-inline aref :unsafe ((array mkcl:natural16) fixnum) :fixnum "(#0)->array.self.b16[#1]")
    (def-inline aref :unsafe ((array mkcl:integer16) fixnum) :fixnum "(#0)->array.self.i16[#1]")
    (def-inline aref :unsafe ((array mkcl:natural16) fixnum) :uint16-t "(#0)->array.self.b16[#1]")
    (def-inline aref :unsafe ((array mkcl:integer16) fixnum) :int16-t "(#0)->array.self.i16[#1]")
    (def-inline aref :unsafe ((array mkcl:natural8) fixnum) :fixnum "(#0)->array.self.b8[#1]")
    (def-inline aref :unsafe ((array mkcl:integer8) fixnum) :fixnum "(#0)->array.self.i8[#1]")
    (def-inline aref :unsafe ((array mkcl:natural8) fixnum) :uint8-t "(#0)->array.self.b8[#1]")
    (def-inline aref :unsafe ((array mkcl:integer8) fixnum) :int8-t "(#0)->array.self.i8[#1]")

    (proclaim-function si:aset (t array &rest t) t)

    (def-inline si:aset :always (t array t) t "mkcl_aset(env, #1, #2, #0)")
    (def-inline si:aset :always (t array fixnum) t "mkcl_aset_index(env, #1, #2, #0)")

    (def-inline si:aset :always (t (array t) fixnum) t "mkcl_aset_index_object(env, #1, #2, #0)")
    (def-inline si:aset :always (t (array fixnum) fixnum) t "mkcl_aset_index_fixnum(env, #1, #2, #0) /*1*/")
    (def-inline si:aset :always (t (array double-float) fixnum) t "mkcl_aset_index_df(env, #1, #2, #0)")
    (def-inline si:aset :always (t (array single-float) fixnum) t "mkcl_aset_index_sf(env, #1, #2, #0)")
    (def-inline si:aset :always (t (array bit) fixnum) t "mkcl_aset_index_bit(env, #1, #2, #0)")
    (def-inline si:aset :always (t (array mkcl:cl-index) fixnum) t "mkcl_aset_index_index(env, #1, #2, #0)")
    (def-inline si:aset :always (t (array mkcl:cl-word) fixnum) t "mkcl_aset_index_word(env, #1, #2, #0)")
    #+unicode
    (def-inline si:aset :always (t (array character) fixnum) t "mkcl_aset_index_ch(env, #1, #2, #0)")
    #+unicode
    (def-inline si:aset :always (t string fixnum) t "mkcl_character_set_index(env, #1, #2, #0)")
    (def-inline si:aset :always (t (array base-char) fixnum) t "mkcl_aset_index_bc(env, #1, #2, #0)")
    (def-inline si:aset :always (t base-string fixnum) t "mkcl_base_char_set_index(env, #1, #2, #0)")
    (def-inline si:aset :always (t (array mkcl:natural64) fixnum) t "mkcl_aset_index_b64(env, #1, #2, #0)")
    (def-inline si:aset :always (t (array mkcl:integer64) fixnum) t "mkcl_aset_index_i64(env, #1, #2, #0)")
    (def-inline si:aset :always (t (array mkcl:natural32) fixnum) t "mkcl_aset_index_b32(env, #1, #2, #0)")
    (def-inline si:aset :always (t (array mkcl:integer32) fixnum) t "mkcl_aset_index_i32(env, #1, #2, #0)")
    (def-inline si:aset :always (t (array mkcl:natural16) fixnum) t "mkcl_aset_index_b16(env, #1, #2, #0)")
    (def-inline si:aset :always (t (array mkcl:integer16) fixnum) t "mkcl_aset_index_i16(env, #1, #2, #0)")
    (def-inline si:aset :always (t (array mkcl:natural8) fixnum) t "mkcl_aset_index_b8(env, #1, #2, #0)")
    (def-inline si:aset :always (t (array mkcl:integer8) fixnum) t "mkcl_aset_index_i8(env, #1, #2, #0) /*0*/")

    (def-inline si:aset :always (fixnum (array fixnum) fixnum) :fixnum "mkcl_aset_index_fixnum_raw(env, #1, #2, #0) /*5*/")

    (def-inline si:aset :always (double-float (array double-float) fixnum) :double "mkcl_aset_index_df_raw(env, #1, #2, #0) /*2*/")
    (def-inline si:aset :always (single-float (array single-float) fixnum) :float "mkcl_aset_index_sf_raw(env, #1, #2, #0) /*0*/")
    (def-inline si:aset :always (fixnum (array bit) fixnum) :fixnum "mkcl_aset_index_bit_raw(env, #1, #2, #0)")
    (def-inline si:aset :always (mkcl:cl-index (array mkcl:cl-index) fixnum) :cl-index "mkcl_aset_index_index_raw(env, #1, #2, #0)")
    (def-inline si:aset :always (mkcl:cl-word (array mkcl:cl-word) fixnum) :fixnum "mkcl_aset_index_word_raw(env, #1, #2, #0)")
    #+unicode
    (def-inline si:aset :always (character (array character) fixnum) :wchar "mkcl_aset_index_ch_raw(env, #1, #2, #0)")
    #+unicode
    (def-inline si:aset :always (character string fixnum) :wchar "mkcl_character_set_index_raw(env, #1, #2, #0)")
    (def-inline si:aset :always (base-char (array base-char) fixnum) :char "mkcl_aset_index_bc_raw(env, #1, #2, #0)")
    (def-inline si:aset :always (base-char base-string fixnum) :char "mkcl_base_char_set_index_raw(env, #1, #2, #0)")

    (def-inline si:aset :always (mkcl:natural64 (array mkcl:natural64) fixnum) :uint64-t "mkcl_aset_index_b64_raw(env, #1, #2, #0)")
    (def-inline si:aset :always (mkcl:integer64 (array mkcl:integer64) fixnum) :int64-t "mkcl_aset_index_i64_raw(env, #1, #2, #0)/*1*/")

    #+(or x86-64 aarch64)
    (def-inline si:aset :always (fixnum (array mkcl:natural32) fixnum) :fixnum 
      "mkcl_fixnum_to_word(mkcl_aset_index_b32(env, #1, #2, MKCL_MAKE_FIXNUM(#0))) /*3*/")
    #+(or x86-64 aarch64)
    (def-inline si:aset :always (fixnum (array mkcl:natural32) fixnum) :void
      "mkcl_aset_index_b32(env, #1, #2, MKCL_MAKE_FIXNUM(#0)) /*4*/")
    #+(or x86-64 aarch64)
    (def-inline si:aset :always (mkcl:natural32 (array mkcl:natural32) fixnum) :uint32-t "mkcl_aset_index_b32_raw(env, #1, #2, #0) /*1*/")
    (def-inline si:aset :always (fixnum (array mkcl:integer32) fixnum) :fixnum
      "mkcl_fixnum_to_word(mkcl_aset_index_i32(env, #1, #2, MKCL_MAKE_FIXNUM(#0))) /*2*/")
    (def-inline si:aset :always (fixnum (array mkcl:integer32) fixnum) :void
      "mkcl_aset_index_i32(env, #1, #2, MKCL_MAKE_FIXNUM(#0)) /*3*/")
    (def-inline si:aset :always (mkcl:integer32 (array mkcl:integer32) fixnum) :int32-t "mkcl_aset_index_i32_raw(env, #1, #2, #0) /*1*/")
    
    (def-inline si:aset :always (fixnum (array mkcl:natural16) fixnum) :fixnum 
      "mkcl_fixnum_to_word(mkcl_aset_index_b16(env, #1, #2, MKCL_MAKE_FIXNUM(#0))) /*3*/")
    (def-inline si:aset :always (fixnum (array mkcl:natural16) fixnum) :void
      "mkcl_aset_index_b16(env, #1, #2, MKCL_MAKE_FIXNUM(#0)) /*4*/")
    (def-inline si:aset :always (mkcl:natural16 (array mkcl:natural16) fixnum) :uint16-t "mkcl_aset_index_b16_raw(env, #1, #2, #0) /*1*/")
    (def-inline si:aset :always (fixnum (array mkcl:integer16) fixnum) :fixnum
      "mkcl_fixnum_to_word(mkcl_aset_index_i16(env, #1, #2, MKCL_MAKE_FIXNUM(#0))) /*2*/")
    (def-inline si:aset :always (fixnum (array mkcl:integer16) fixnum) :void
      "mkcl_aset_index_i16(env, #1, #2, MKCL_MAKE_FIXNUM(#0)) /*3*/")
    (def-inline si:aset :always (mkcl:integer16 (array mkcl:integer16) fixnum) :int16-t "mkcl_aset_index_i16_raw(env, #1, #2, #0) /*1*/")
    
    (def-inline si:aset :always (fixnum (array mkcl:natural8) fixnum) :fixnum 
      "mkcl_fixnum_to_word(mkcl_aset_index_b8(env, #1, #2, MKCL_MAKE_FIXNUM(#0))) /*3*/")
    (def-inline si:aset :always (fixnum (array mkcl:natural8) fixnum) :void
      "mkcl_aset_index_b8(env, #1, #2, MKCL_MAKE_FIXNUM(#0)) /*4*/")
    (def-inline si:aset :always (mkcl:natural8 (array mkcl:natural8) fixnum) :uint8-t "mkcl_aset_index_b8_raw(env, #1, #2, #0) /*1*/")
    (def-inline si:aset :always (fixnum (array mkcl:integer8) fixnum) :fixnum
      "mkcl_fixnum_to_word(mkcl_aset_index_i8(env, #1, #2, MKCL_MAKE_FIXNUM(#0))) /*2*/")
    (def-inline si:aset :always (fixnum (array mkcl:integer8) fixnum) :void
      "mkcl_aset_index_i8(env, #1, #2, MKCL_MAKE_FIXNUM(#0)) /*3*/")
    (def-inline si:aset :always (mkcl:integer8 (array mkcl:integer8) fixnum) :int8-t "mkcl_aset_index_i8_raw(env, #1, #2, #0) /*1*/")
    
    (def-inline si:aset :unsafe (t t t t) t
      "@01;mkcl_aset_index(env, #1, mkcl_fixnum_to_word(#2)*(#1)->array.dims[1]+mkcl_fixnum_to_word(#3), #0)")
    (def-inline si:aset :unsafe (t (array t) t t) t
      "@01;((#1)->array.self.t[mkcl_fixnum_to_word(#2)*(#1)->array.dims[1]+mkcl_fixnum_to_word(#3)] = (#0))")
    (def-inline si:aset :unsafe (t (array fixnum) t t) t
      "@01;((#1)->array.self.word[mkcl_fixnum_to_word(#2)*(#1)->array.dims[1]+mkcl_fixnum_to_word(#3)] = mkcl_fixnum_to_word(#0), (#0))")
    (def-inline si:aset :unsafe (fixnum (array fixnum) t t) :fixnum
      "@01;((#1)->array.self.word[mkcl_fixnum_to_word(#2)*(#1)->array.dims[1]+mkcl_fixnum_to_word(#3)] = (#0))")
    (def-inline si:aset :unsafe (fixnum (array bit) t t) t
      "@01;mkcl_bvset_index(env, #1, mkcl_fixnum_to_word(#2)*(#1)->array.dims[1]+mkcl_fixnum_to_word(#3), #0)")
    (def-inline si:aset :unsafe (t (array t) fixnum fixnum) :object
      "@01;((#1)->array.self.t[(#2)*(#1)->array.dims[1]+(#3)] = (#0))")
    (def-inline si:aset :unsafe (t (array fixnum) fixnum fixnum) :object
      "@01;((#1)->array.self.word[(#2)*(#1)->array.dims[1]+(#3)] = mkcl_fixnum_to_word(#0), (#0))")
    (def-inline si:aset :unsafe (fixnum (array fixnum) fixnum fixnum) :fixnum
      "@01;((#1)->array.self.word[(#2)*(#1)->array.dims[1]+(#3)] = (#0))")
    (def-inline si:aset :unsafe (fixnum (array bit) fixnum fixnum) :fixnum
      "@01;mkcl_bvset_index_raw(env, #1,(#2)*(#1)->array.dims[1]+(#3), #0)")
    #+unicode
    (def-inline si:aset :unsafe (character (array character) fixnum fixnum) :wchar
      "@01;((#1)->array.self.ch[(#2)*(#1)->array.dims[1]+(#3)] = (#0))")
    (def-inline si:aset :unsafe (base-char (array base-char) fixnum fixnum) :char
      "@01;((#1)->array.self.bc[(#2)*(#1)->array.dims[1]+(#3)] = (#0))")
    (def-inline si:aset :unsafe (double-float (array double-float) fixnum fixnum) :double
      "@01;((#1)->array.self.df[(#2)*(#1)->array.dims[1]+(#3)] = (#0))")
    (def-inline si:aset :unsafe (single-float (array single-float) fixnum fixnum) :float
      "@01;((#1)->array.self.sf[(#2)*(#1)->array.dims[1]+(#3)] = (#0))")
    (def-inline si:aset :unsafe (mkcl:cl-index (array mkcl:cl-index) fixnum fixnum) :cl-index
      "@01;((#1)->array.self.index[(#2)*(#1)->array.dims[1]+(#3)] = (#0))")
    (def-inline si:aset :unsafe (mkcl:cl-word (array mkcl:cl-word) fixnum fixnum) :fixnum
      "@01;((#1)->array.self.word[(#2)*(#1)->array.dims[1]+(#3)] = (#0))")
    (def-inline si:aset :unsafe (mkcl:natural64 (array mkcl:natural64) fixnum fixnum) :uint64-t
      "@01;((#1)->array.self.b64[(#2)*(#1)->array.dims[1]+(#3)] = (#0))")
    (def-inline si:aset :unsafe (mkcl:integer64 (array mkcl:integer64) fixnum fixnum) :int64-t
      "@01;((#1)->array.self.i64[(#2)*(#1)->array.dims[1]+(#3)] = (#0))")
    #+(or x86-64 aarch64)
    (def-inline si:aset :unsafe (fixnum (array mkcl:natural32) fixnum fixnum) :fixnum
      "@01;((#1)->array.self.b32[(#2)*(#1)->array.dims[1]+(#3)] = (#0))")
    #+(or x86-64 aarch64)
    (def-inline si:aset :unsafe (mkcl:natural32 (array mkcl:natural32) fixnum fixnum) :uint32-t
      "@01;((#1)->array.self.b32[(#2)*(#1)->array.dims[1]+(#3)] = (#0))")
    (def-inline si:aset :unsafe (fixnum (array mkcl:integer32) fixnum fixnum) :fixnum
      "@01;((#1)->array.self.i32[(#2)*(#1)->array.dims[1]+(#3)] = (#0))")
    (def-inline si:aset :unsafe (mkcl:integer32 (array mkcl:integer32) fixnum fixnum) :int32-t
      "@01;((#1)->array.self.i32[(#2)*(#1)->array.dims[1]+(#3)] = (#0))")
    (def-inline si:aset :unsafe (fixnum (array mkcl:natural16) fixnum fixnum) :fixnum
      "@01;((#1)->array.self.b16[(#2)*(#1)->array.dims[1]+(#3)] = (#0))")
    (def-inline si:aset :unsafe (mkcl:natural16 (array mkcl:natural16) fixnum fixnum) :uint16-t
      "@01;((#1)->array.self.b16[(#2)*(#1)->array.dims[1]+(#3)] = (#0))")
    (def-inline si:aset :unsafe (fixnum (array mkcl:integer16) fixnum fixnum) :fixnum
      "@01;((#1)->array.self.i16[(#2)*(#1)->array.dims[1]+(#3)] = (#0))")
    (def-inline si:aset :unsafe (mkcl:integer16 (array mkcl:integer16) fixnum fixnum) :int16-t
      "@01;((#1)->array.self.i16[(#2)*(#1)->array.dims[1]+(#3)] = (#0))")
    (def-inline si:aset :unsafe (fixnum (array mkcl:natural8) fixnum fixnum) :fixnum
      "@01;((#1)->array.self.b8[(#2)*(#1)->array.dims[1]+(#3)] = (#0))")
    (def-inline si:aset :unsafe (mkcl:natural8 (array mkcl:natural8) fixnum fixnum) :uint8-t
      "@01;((#1)->array.self.b8[(#2)*(#1)->array.dims[1]+(#3)] = (#0))")
    (def-inline si:aset :unsafe (fixnum (array mkcl:integer8) fixnum fixnum) :fixnum
      "@01;((#1)->array.self.i8[(#2)*(#1)->array.dims[1]+(#3)] = (#0))")
    (def-inline si:aset :unsafe (mkcl:integer8 (array mkcl:integer8) fixnum fixnum) :int8-t
      "@01;((#1)->array.self.i8[(#2)*(#1)->array.dims[1]+(#3)] = (#0))")

    (def-inline si:aset :unsafe (t t t) t "mkcl_aset_index(env, #1, mkcl_fixnum_to_word(#2), #0)")
    (def-inline si:aset :unsafe (t (array t) t) t "((#1)->array.self.t[mkcl_fixnum_to_word(#2)] = (#0))")
    (def-inline si:aset :unsafe (t (array t) fixnum) t "((#1)->array.self.t[(#2)] = (#0))")
    (def-inline si:aset :unsafe (t (array fixnum) t) t "@0;((#1)->array.self.word[mkcl_fixnum_to_word(#2)] = mkcl_fixnum_to_word(#0), (#0))")
    (def-inline si:aset :unsafe (t (array fixnum) fixnum) t "@0;((#1)->array.self.word[(#2)] = mkcl_fixnum_to_word(#0), (#0))")
    (def-inline si:aset :unsafe (fixnum (array fixnum) t) fixnum "((#1)->array.self.word[mkcl_fixnum_to_word(#2)] = (#0))")
    (def-inline si:aset :unsafe (fixnum (array fixnum) fixnum) fixnum "((#1)->array.self.word[(#2)] = (#0))")

    (def-inline si:aset :unsafe (t (array bit) t) t
      "mkcl_bvset_index(env, #1,mkcl_fixnum_to_word(#2), #0)") ;; not quite unsafe
    (def-inline si:aset :unsafe (fixnum (array bit) fixnum) fixnum
      "mkcl_bvset_index_raw(env, #1, #2, #0)") ;; not quite unsafe
    #+unicode
    (def-inline si:aset :unsafe (character (array character) fixnum) :wchar "((#1)->string.self[#2] = (#0))")
    (def-inline si:aset :unsafe (base-char (array base-char) fixnum) :char "((#1)->base_string.self[#2] = (#0))")
    (def-inline si:aset :unsafe (double-float (array double-float) fixnum) :double "((#1)->array.self.df[#2] = (#0))")
    (def-inline si:aset :unsafe (single-float (array single-float) fixnum) :float "((#1)->array.self.sf[#2] = (#0))")
    (def-inline si:aset :unsafe (mkcl:cl-index (array mkcl:cl-index) fixnum) :cl-index "((#1)->array.self.index[#2] = (#0))")
    (def-inline si:aset :unsafe (mkcl:cl-word (array mkcl:cl-word) fixnum) :fixnum "((#1)->array.self.word[#2] = (#0))")
    (def-inline si:aset :unsafe (mkcl:natural64 (array mkcl:natural64) fixnum) :uint64-t "((#1)->array.self.b64[#2] = (#0))")
    (def-inline si:aset :unsafe (mkcl:integer64 (array mkcl:integer64) fixnum) :int64-t "((#1)->array.self.i64[#2] = (#0))")
    #+(or x86-64 aarch64)
    (def-inline si:aset :unsafe (fixnum (array mkcl:natural32) fixnum) :fixnum "((#1)->array.self.b32[#2] = (#0))")
    #+(or x86-64 aarch64)
    (def-inline si:aset :unsafe (mkcl:natural32 (array mkcl:natural32) fixnum) :uint32-t "((#1)->array.self.b32[#2] = (#0))")
    (def-inline si:aset :unsafe (fixnum (array mkcl:integer32) fixnum) :fixnum "((#1)->array.self.i32[#2] = (#0))")
    (def-inline si:aset :unsafe (mkcl:integer32 (array mkcl:integer32) fixnum) :int32-t "((#1)->array.self.i32[#2] = (#0))")
    (def-inline si:aset :unsafe (fixnum (array mkcl:natural16) fixnum) :fixnum "((#1)->array.self.b16[#2] = (#0))")
    (def-inline si:aset :unsafe (mkcl:natural16 (array mkcl:natural16) fixnum) :uint16-t "((#1)->array.self.b16[#2] = (#0))")
    (def-inline si:aset :unsafe (fixnum (array mkcl:integer16) fixnum) :fixnum "((#1)->array.self.i16[#2] = (#0))")
    (def-inline si:aset :unsafe (mkcl:integer16 (array mkcl:integer16) fixnum) :int16-t "((#1)->array.self.i16[#2] = (#0))")
    (def-inline si:aset :unsafe (fixnum (array mkcl:natural8) fixnum) :fixnum "((#1)->array.self.b8[#2] = (#0))")
    (def-inline si:aset :unsafe (mkcl:natural8 (array mkcl:natural8) fixnum) :uint8-t "((#1)->array.self.b8[#2] = (#0))")
    (def-inline si:aset :unsafe (fixnum (array mkcl:integer8) fixnum) :fixnum "((#1)->array.self.i8[#2] = (#0))")
    (def-inline si:aset :unsafe (mkcl:integer8 (array mkcl:integer8) fixnum) :int8-t "((#1)->array.self.i8[#2] = (#0))")


    (proclaim-function row-major-aref (array t) t :no-side-effects t)

    (def-inline row-major-aref :always (array t) t "mkcl_aref(env, #0, #1)")
    (def-inline row-major-aref :always (array fixnum) t "mkcl_aref_index(env, #0,#1)")

    (def-inline row-major-aref :always ((array t) fixnum) t "mkcl_aref_index_object(env, #0, #1)")
    (def-inline row-major-aref :always ((array fixnum) fixnum) t "mkcl_aref_index_fixnum(env, #0, #1)")
    (def-inline row-major-aref :always ((array double-float) fixnum) t "mkcl_aref_index_df(env, #0, #1)")
    (def-inline row-major-aref :always ((array single-float) fixnum) t "mkcl_aref_index_sf(env, #0, #1)")
    (def-inline row-major-aref :always ((array bit) fixnum) t "mkcl_aref_index_bit(env, #0, #1)")
    (def-inline row-major-aref :always ((array mkcl:cl-index) fixnum) t "mkcl_aref_index_index(env, #0, #1)")
    (def-inline row-major-aref :always ((array mkcl:cl-word) fixnum) t "mkcl_aref_index_word(env, #0, #1)")
    #+unicode
    (def-inline row-major-aref :always ((array character) fixnum) t "mkcl_aref_index_ch(env, #0, #1)")
    #+unicode
    (def-inline row-major-aref :always (string fixnum) t "mkcl_character_index(env, #0, #1)")
    (def-inline row-major-aref :always ((array base-char) fixnum) t "mkcl_aref_index_bc(env, #0, #1)")
    (def-inline row-major-aref :always (base-string fixnum) t "mkcl_base_char_index(env, #0, #1)")
    (def-inline row-major-aref :always ((array mkcl:natural64) fixnum) t "mkcl_aref_index_b64(env, #0, #1)")
    (def-inline row-major-aref :always ((array mkcl:integer64) fixnum) t "mkcl_aref_index_i64(env, #0, #1)")
    (def-inline row-major-aref :always ((array mkcl:natural32) fixnum) t "mkcl_aref_index_b32(env, #0, #1)")
    (def-inline row-major-aref :always ((array mkcl:integer32) fixnum) t "mkcl_aref_index_i32(env, #0, #1)")
    (def-inline row-major-aref :always ((array mkcl:natural16) fixnum) t "mkcl_aref_index_b16(env, #0, #1)")
    (def-inline row-major-aref :always ((array mkcl:integer16) fixnum) t "mkcl_aref_index_i16(env, #0, #1) /*0*/")
    (def-inline row-major-aref :always ((array mkcl:natural8) fixnum) t "mkcl_aref_index_b8(env, #0, #1)")
    (def-inline row-major-aref :always ((array mkcl:integer8) fixnum) t "mkcl_aref_index_i8(env, #0, #1)")
    
    (def-inline row-major-aref :always ((array fixnum) fixnum) :fixnum "mkcl_aref_index_fixnum_raw(env, #0, #1)")
    (def-inline row-major-aref :always ((array double-float) fixnum) :double "mkcl_aref_index_df_raw(env, #0, #1)")
    (def-inline row-major-aref :always ((array single-float) fixnum) :float "mkcl_aref_index_sf_raw(env, #0, #1)")
    (def-inline row-major-aref :always ((array bit) fixnum) :fixnum "mkcl_aref_index_bit_raw(env, #0, #1)")
    (def-inline row-major-aref :always ((array mkcl:cl-index) fixnum) :cl-index "mkcl_aref_index_index_raw(env, #0, #1)")
    (def-inline row-major-aref :always ((array mkcl:cl-word) fixnum) :fixnum "mkcl_aref_index_word_raw(env, #0, #1)")
    #+unicode
    (def-inline row-major-aref :always ((array character) fixnum) :wchar "mkcl_aref_index_ch_raw(env, #0, #1)")
    #+unicode
    (def-inline row-major-aref :always (string fixnum) :wchar "mkcl_character_index_raw(env, #0, #1)")
    (def-inline row-major-aref :always ((array base-char) fixnum) :char "mkcl_aref_index_bc_raw(env, #0, #1)")
    (def-inline row-major-aref :always (base-string fixnum) :char "mkcl_base_char_index_raw(env, #0, #1)")
    (def-inline row-major-aref :always ((array mkcl:natural64) fixnum) :uint64-t "mkcl_aref_index_b64_raw(env, #0, #1)")
    (def-inline row-major-aref :always ((array mkcl:integer64) fixnum) :int64-t "mkcl_aref_index_i64_raw(env, #0, #1)")
    #+(or x86-64 aarch64)
    (def-inline row-major-aref :always ((array mkcl:natural32) fixnum) :fixnum "mkcl_aref_index_b32_raw(env, #0, #1)")
    (def-inline row-major-aref :always ((array mkcl:integer32) fixnum) :fixnum "mkcl_aref_index_i32_raw(env, #0, #1)")
    (def-inline row-major-aref :always ((array mkcl:natural32) fixnum) :uint32-t "mkcl_aref_index_b32_raw(env, #0, #1)")
    (def-inline row-major-aref :always ((array mkcl:integer32) fixnum) :int32-t "mkcl_aref_index_i32_raw(env, #0, #1)")
    (def-inline row-major-aref :always ((array mkcl:natural16) fixnum) :fixnum "mkcl_aref_index_b16_raw(env, #0, #1)")
    (def-inline row-major-aref :always ((array mkcl:integer16) fixnum) :fixnum "mkcl_aref_index_i16_raw(env, #0, #1) /*1*/")
    (def-inline row-major-aref :always ((array mkcl:natural16) fixnum) :uint16-t "mkcl_aref_index_b16_raw(env, #0, #1)")
    (def-inline row-major-aref :always ((array mkcl:integer16) fixnum) :int16-t "mkcl_aref_index_i16_raw(env, #0, #1) /*2*/")
    (def-inline row-major-aref :always ((array mkcl:natural8) fixnum) :fixnum "mkcl_aref_index_b8_raw(env, #0, #1)")
    (def-inline row-major-aref :always ((array mkcl:integer8) fixnum) :fixnum "mkcl_aref_index_i8_raw(env, #0, #1)")
    (def-inline row-major-aref :always ((array mkcl:natural8) fixnum) :uint8-t "mkcl_aref_index_b8_raw(env, #0, #1)")
    (def-inline row-major-aref :always ((array mkcl:integer8) fixnum) :int8-t "mkcl_aref_index_i8_raw(env, #0, #1)")

    (def-inline row-major-aref :unsafe (t t) t "mkcl_vref_index(env, #0,mkcl_fixnum_to_word(#1))")
    (def-inline row-major-aref :unsafe ((array t) t) :object "((#0)->array.self.t[mkcl_fixnum_to_word(#1)])")
    (def-inline row-major-aref :unsafe ((array fixnum) t) :object "MKCL_MAKE_FIXNUM((#0)->array.self.word[mkcl_fixnum_to_word(#1)])")
    (def-inline row-major-aref :unsafe ((array fixnum) t) :fixnum "((#0)->array.self.word[mkcl_fixnum_to_word(#1)])")
    (def-inline row-major-aref :unsafe ((array t) fixnum) :object "((#0)->array.self.t[#1])")
    (def-inline row-major-aref :unsafe ((array fixnum) fixnum) :object "MKCL_MAKE_FIXNUM((#0)->array.self.word[#1])")
    (def-inline row-major-aref :unsafe ((array fixnum) fixnum) :fixnum "((#0)->array.self.word[#1])")
    (def-inline row-major-aref :unsafe ((array bit) t) t "mkcl_bvref_index(env, #0,mkcl_fixnum_to_word(#1))") ;;not quite unsafe
    (def-inline row-major-aref :unsafe ((array bit) fixnum) :fixnum "mkcl_bvref_index_raw(env, #0,#1)") ;;not quite unsafe
    #+unicode
    (def-inline row-major-aref :unsafe ((array character) fixnum) :wchar "(#0)->string.self[#1]")
    (def-inline row-major-aref :unsafe ((array base-char) fixnum) :char "(#0)->base_string.self[#1]")
    (def-inline row-major-aref :unsafe ((array double-float) fixnum) :double "(#0)->array.self.df[#1]")
    (def-inline row-major-aref :unsafe ((array single-float) fixnum) :float "(#0)->array.self.sf[#1]")
    (def-inline row-major-aref :unsafe ((array mkcl:cl-index) fixnum) :cl-index "(#0)->array.self.index[#1]")
    (def-inline row-major-aref :unsafe ((array mkcl:cl-word) fixnum) :fixnum "(#0)->array.self.word[#1]")
    (def-inline row-major-aref :unsafe ((array mkcl:natural64) fixnum) :uint64-t "(#0)->array.self.b64[#1]")
    (def-inline row-major-aref :unsafe ((array mkcl:integer64) fixnum) :int64-t "(#0)->array.self.i64[#1]")
    #+(or x86-64 aarch64)
    (def-inline row-major-aref :unsafe ((array mkcl:natural32) fixnum) :fixnum "(#0)->array.self.b32[#1]")
    (def-inline row-major-aref :unsafe ((array mkcl:integer32) fixnum) :fixnum "(#0)->array.self.i32[#1]")
    (def-inline row-major-aref :unsafe ((array mkcl:natural32) fixnum) :uint32-t "(#0)->array.self.b32[#1]")
    (def-inline row-major-aref :unsafe ((array mkcl:integer32) fixnum) :int32-t "(#0)->array.self.i32[#1]")
    (def-inline row-major-aref :unsafe ((array mkcl:natural16) fixnum) :fixnum "(#0)->array.self.b16[#1]")
    (def-inline row-major-aref :unsafe ((array mkcl:integer16) fixnum) :fixnum "(#0)->array.self.i16[#1]")
    (def-inline row-major-aref :unsafe ((array mkcl:natural16) fixnum) :uint16-t "(#0)->array.self.b16[#1]")
    (def-inline row-major-aref :unsafe ((array mkcl:integer16) fixnum) :int16-t "(#0)->array.self.i16[#1]")
    (def-inline row-major-aref :unsafe ((array mkcl:natural8) fixnum) :fixnum "(#0)->array.self.b8[#1]")
    (def-inline row-major-aref :unsafe ((array mkcl:integer8) fixnum) :fixnum "(#0)->array.self.i8[#1]")
    (def-inline row-major-aref :unsafe ((array mkcl:natural8) fixnum) :uint8-t "(#0)->array.self.b8[#1]")
    (def-inline row-major-aref :unsafe ((array mkcl:integer8) fixnum) :int8-t "(#0)->array.self.i8[#1]")

    (proclaim-function si:row-major-aset (array t t) t)
    (def-inline si:row-major-aset :always (array fixnum t) t "mkcl_aset_index(env, #0,#1,#2)")

    (def-inline si:row-major-aset :always (array t t) t "mkcl_aset(env, #0, #1, #2)")
    (def-inline si:row-major-aset :always (array fixnum t) t "mkcl_aset_index(env, #0, #1, #2)")

    (def-inline si:row-major-aset :always ((array t) fixnum t) t "mkcl_aset_index_object(env, #0, #1, #2)")
    (def-inline si:row-major-aset :always ((array fixnum) fixnum t) t "mkcl_aset_index_fixnum(env, #0, #1, #2)")
    (def-inline si:row-major-aset :always ((array double-float) fixnum t) t "mkcl_aset_index_df(env, #0, #1, #2)")
    (def-inline si:row-major-aset :always ((array single-float) fixnum t) t "mkcl_aset_index_sf(env, #0, #1, #2)")
    (def-inline si:row-major-aset :always ((array bit) fixnum t) t "mkcl_aset_index_bit(env, #0, #1, #2)")
    (def-inline si:row-major-aset :always ((array mkcl:cl-index) fixnum t) t "mkcl_aset_index_index(env, #0, #1, #2)")
    (def-inline si:row-major-aset :always ((array mkcl:cl-word) fixnum t) t "mkcl_aset_index_word(env, #0, #1, #2)")
    #+unicode
    (def-inline si:row-major-aset :always ((array character) fixnum t) t "mkcl_aset_index_ch(env, #0, #1, #2)")
    #+unicode
    (def-inline si:row-major-aset :always (string fixnum t) t "mkcl_character_set_index(env, #0, #1, #2)")
    (def-inline si:row-major-aset :always ((array base-char) fixnum t) t "mkcl_aset_index_bc(env, #0, #1, #2)")
    (def-inline si:row-major-aset :always (base-string fixnum t) t "mkcl_base_char_set_index(env, #0, #1, #2)")
    (def-inline si:row-major-aset :always ((array mkcl:natural64) fixnum t) t "mkcl_aset_index_b64(env, #0, #1, #2)")
    (def-inline si:row-major-aset :always ((array mkcl:integer64) fixnum t) t "mkcl_aset_index_i64(env, #0, #1, #2)")
    (def-inline si:row-major-aset :always ((array mkcl:natural32) fixnum t) t "mkcl_aset_index_b32(env, #0, #1, #2)")
    (def-inline si:row-major-aset :always ((array mkcl:integer32) fixnum t) t "mkcl_aset_index_i32(env, #0, #1, #2)")
    (def-inline si:row-major-aset :always ((array mkcl:natural16) fixnum t) t "mkcl_aset_index_b16(env, #0, #1, #2)")
    (def-inline si:row-major-aset :always ((array mkcl:integer16) fixnum t) t "mkcl_aset_index_i16(env, #0, #1, #2)")
    (def-inline si:row-major-aset :always ((array mkcl:natural8) fixnum t) t "mkcl_aset_index_b8(env, #0, #1, #2)")
    (def-inline si:row-major-aset :always ((array mkcl:integer8) fixnum t) t "mkcl_aset_index_i8(env, #0, #1, #2)")
    
    (def-inline si:row-major-aset :always ((array fixnum) fixnum fixnum) :fixnum "mkcl_aset_index_fixnum_raw(env, #0, #1, #2)")
    (def-inline si:row-major-aset :always ((array double-float) fixnum double-float) :double "mkcl_aset_index_df_raw(env, #0, #1, #2)")
    (def-inline si:row-major-aset :always ((array single-float) fixnum single-float) :float "mkcl_aset_index_sf_raw(env, #0, #1, #2)")
    (def-inline si:row-major-aset :always ((array bit) fixnum fixnum) :fixnum "mkcl_aset_index_bit_raw(env, #0, #1, #2)")
    (def-inline si:row-major-aset :always ((array mkcl:cl-index) fixnum mkcl:cl-index) :cl-index "mkcl_aset_index_index_raw(env, #0, #1, #2)")
    (def-inline si:row-major-aset :always ((array mkcl:cl-word) fixnum mkcl:cl-word) :fixnum "mkcl_aset_index_word_raw(env, #0, #1, #2)")
    #+unicode
    (def-inline si:row-major-aset :always ((array character) fixnum character) :wchar "mkcl_aset_index_ch_raw(env, #0, #1, #2)")
    #+unicode
    (def-inline si:row-major-aset :always (string fixnum character) :wchar "mkcl_character_set_index_raw(env, #0, #1, #2)")
    (def-inline si:row-major-aset :always ((array base-char) fixnum base-char) :char "mkcl_aset_index_bc_raw(env, #0, #1, #2)")
    (def-inline si:row-major-aset :always (base-string fixnum base-char) :char "mkcl_base_char_set_index_raw(env, #0, #1, #2)")
    (def-inline si:row-major-aset :always ((array mkcl:natural64) fixnum mkcl:natural64) :uint64-t "mkcl_aset_index_b64_raw(env, #0, #1, #2)")
    (def-inline si:row-major-aset :always ((array mkcl:integer64) fixnum mkcl:integer64) :int64-t "mkcl_aset_index_i64_raw(env, #0, #1, #2)")
    #+(or x86-64 aarch64)
    (def-inline si:row-major-aset :always ((array mkcl:natural32) fixnum fixnum) :fixnum "mkcl_fixnum_to_word(mkcl_aset_index_b32(env, #0, #1, MKCL_MAKE_FIXNUM(#2)))")
    #+(or x86-64 aarch64)
    (def-inline si:row-major-aset :always ((array mkcl:natural32) fixnum fixnum) :void "mkcl_aset_index_b32(env, #0, #1, MKCL_MAKE_FIXNUM(#2)))")
    #+(or x86-64 aarch64)
    (def-inline si:row-major-aset :always ((array mkcl:natural32) fixnum mkcl:natural32) :uint32-t "mkcl_aset_index_b32_raw(env, #0, #1, #2)")
    (def-inline si:row-major-aset :always ((array mkcl:integer32) fixnum fixnum) :fixnum "mkcl_fixnum_to_word(mkcl_aset_index_i32(env, #0, #1, MKCL_MAKE_FIXNUM(#2)))")
    (def-inline si:row-major-aset :always ((array mkcl:integer32) fixnum fixnum) :void "mkcl_aset_index_i32(env, #0, #1, MKCL_MAKE_FIXNUM(#2))")
    (def-inline si:row-major-aset :always ((array mkcl:integer32) fixnum mkcl:integer32) :int32-t "mkcl_aset_index_i32_raw(env, #0, #1, #2)")
    (def-inline si:row-major-aset :always ((array mkcl:natural16) fixnum fixnum) :fixnum "mkcl_fixnum_to_word(mkcl_aset_index_b16(env, #0, #1, MKCL_MAKE_FIXNUM(#2)))")
    (def-inline si:row-major-aset :always ((array mkcl:natural16) fixnum :fixnum) :void "mkcl_aset_index_b16(env, #0, #1, MKCL_MAKE_FIXNUM(#2))")
    (def-inline si:row-major-aset :always ((array mkcl:natural16) fixnum mkcl:natural16) :uint16-t "mkcl_aset_index_b16_raw(env, #0, #1, #2)")
    (def-inline si:row-major-aset :always ((array mkcl:integer16) fixnum fixnum) :fixnum "mkcl_fixnum_to_word(mkcl_aset_index_i16(env, #0, #1, MKCL_MAKE_FIXNUM(#2)))")
    (def-inline si:row-major-aset :always ((array mkcl:integer16) fixnum fixnum) :void "mkcl_aset_index_i16(env, #0, #1, MKCL_MAKE_FIXNUM(#2))")
    (def-inline si:row-major-aset :always ((array mkcl:integer16) fixnum mkcl:integer16) :int16-t "mkcl_aset_index_i16_raw(env, #0, #1, #2)")
    (def-inline si:row-major-aset :always ((array mkcl:natural8) fixnum fixnum) :fixnum "mkcl_fixnum_to_word(mkcl_aset_index_b8(env, #0, #1, MKCL_MAKE_FIXNUM(#2)))")
    (def-inline si:row-major-aset :always ((array mkcl:natural8) fixnum fixnum) :void "mkcl_aset_index_b8(env, #0, #1, MKCL_MAKE_FIXNUM(#2))")
    (def-inline si:row-major-aset :always ((array mkcl:natural8) fixnum mkcl:natural8) :uint8-t "mkcl_aset_index_b8_raw(env, #0, #1, #2)")
    (def-inline si:row-major-aset :always ((array mkcl:integer8) fixnum fixnum) :fixnum "mkcl_fixnum_to_word(mkcl_aset_index_i8(env, #0, #1, MKCL_MAKE_FIXNUM(#2)))")
    (def-inline si:row-major-aset :always ((array mkcl:integer8) fixnum fixnum) :void "mkcl_aset_index_i8(env, #0, #1, MKCL_MAKE_FIXNUM(#2))")
    (def-inline si:row-major-aset :always ((array mkcl:integer8) fixnum mkcl:integer8) :int8-t "mkcl_aset_index_i8_raw(env, #0, #1, #2)")

    (def-inline si:row-major-aset :unsafe (t t t) t "mkcl_aset_index(env, #0, mkcl_fixnum_to_word(#1), #2)")
    (def-inline si:row-major-aset :unsafe ((array t) t t) t "((#0)->array.self.t[mkcl_fixnum_to_word(#1)] = (#2))")
    (def-inline si:row-major-aset :unsafe ((array t) fixnum t) t "((#0)->array.self.t[#1] = (#2))")
    (def-inline si:row-major-aset :unsafe ((array fixnum) t t) t "@012;((#0)->array.self.word[mkcl_fixnum_to_word(#1)] = mkcl_fixnum_to_word(#2), (#2))")
    (def-inline si:row-major-aset :unsafe ((array fixnum) fixnum t) t "@012;((#0)->array.self.word[#1] = mkcl_fixnum_to_word(#2), (#2))")
    (def-inline si:row-major-aset :unsafe ((array fixnum) t fixnum) fixnum "((#0)->array.self.word[mkcl_fixnum_to_word(#1)] = (#2))")
    (def-inline si:row-major-aset :unsafe ((array fixnum) fixnum fixnum) fixnum "((#0)->array.self.word[(#1)] = (#2))")

    (def-inline si:row-major-aset :unsafe ((array bit) t t) t
      "mkcl_bvset_index(env, #0,mkcl_fixnum_to_word(#1), #2)") ;; not quite unsafe
    (def-inline si:row-major-aset :unsafe ((array bit) fixnum fixnum) :fixnum
      "mkcl_bvset_index_raw(env, #0, #1, #2)") ;; not quite unsafe
    #+unicode
    (def-inline si:row-major-aset :unsafe ((array character) fixnum character) :wchar "((#0)->string.self[#1] = (#2))")
    (def-inline si:row-major-aset :unsafe ((array base-char) fixnum base-char) :char "((#0)->base_string.self[#1] = (#2))")
    (def-inline si:row-major-aset :unsafe ((array double-float) fixnum double-float) :double "((#0)->array.self.df[#1] = (#2))")
    (def-inline si:row-major-aset :unsafe ((array single-float) fixnum single-float) :float "((#0)->array.self.sf[#1] = (#2))")
    (def-inline si:row-major-aset :unsafe ((array mkcl:cl-index) fixnum mkcl:cl-index) :cl-index "((#0)->array.self.index[#1] = (#2))")
    (def-inline si:row-major-aset :unsafe ((array mkcl:cl-word) fixnum mkcl:cl-word) :fixnum "((#0)->array.self.word[#1] = (#2))")
    (def-inline si:row-major-aset :unsafe ((array mkcl:natural64) fixnum mkcl:natural64) :uint64-t "((#0)->array.self.b64[#1] = (#2))")
    (def-inline si:row-major-aset :unsafe ((array mkcl:integer64) fixnum mkcl:integer64) :int64-t "((#0)->array.self.i64[#1] = (#2))")
    #+(or x86-64 aarch64)
    (def-inline si:row-major-aset :unsafe ((array mkcl:natural32) fixnum fixnum) :fixnum "((#0)->array.self.b32[#1] = (#2))")
    #+(or x86-64 aarch64)
    (def-inline si:row-major-aset :unsafe ((array mkcl:natural32) fixnum mkcl:natural32) :uint32-t "((#0)->array.self.b32[#1] = (#2))")
    (def-inline si:row-major-aset :unsafe ((array mkcl:integer32) fixnum fixnum) :fixnum "((#0)->array.self.i32[#1] = (#2))")
    (def-inline si:row-major-aset :unsafe ((array mkcl:integer32) fixnum mkcl:integer32) :int32-t "((#0)->array.self.i32[#1] = (#2))")
    (def-inline si:row-major-aset :unsafe ((array mkcl:natural16) fixnum fixnum) :fixnum "((#0)->array.self.b16[#1] = (#2))")
    (def-inline si:row-major-aset :unsafe ((array mkcl:natural16) fixnum mkcl:natural16) :uint16-t "((#0)->array.self.b16[#1] = (#2))")
    (def-inline si:row-major-aset :unsafe ((array mkcl:integer16) fixnum fixnum) :fixnum "((#0)->array.self.i16[#1] = (#2))")
    (def-inline si:row-major-aset :unsafe ((array mkcl:integer16) fixnum mkcl:integer16) :int16-t "((#0)->array.self.i16[#1] = (#2))")
    (def-inline si:row-major-aset :unsafe ((array mkcl:natural8) fixnum fixnum) :fixnum "((#0)->array.self.b8[#1] = (#2))")
    (def-inline si:row-major-aset :unsafe ((array mkcl:natural8) fixnum mkcl:natural8) :uint8-t "((#0)->array.self.b8[#1] = (#2))")
    (def-inline si:row-major-aset :unsafe ((array mkcl:integer8) fixnum fixnum) :fixnum "((#0)->array.self.i8[#1] = (#2))")
    (def-inline si:row-major-aset :unsafe ((array mkcl:integer8) fixnum mkcl:integer8) :int8-t "((#0)->array.self.i8[#1] = (#2))")


    (proclaim-function array-row-major-index (array &rest t) fixnum :no-side-effects t)

    (def-inline array-row-major-index :always (array t t) :fixnum "mkcl_array_row_major_index_2_t(env, #0, #1, #2) /*0*/")
    (def-inline array-row-major-index :always (array t t t) :fixnum "mkcl_array_row_major_index_3_t(env, #0, #1, #2, #3) /*0*/")
    (def-inline array-row-major-index :always (array fixnum fixnum) :fixnum
      "mkcl_array_row_major_index_2_index(env, #0, #1, #2) /*1*/")
    (def-inline array-row-major-index :always (array fixnum fixnum fixnum) :fixnum
      "mkcl_array_row_major_index_3_index(env, #0, #1, #2, #3) /*1*/")
    (def-inline array-row-major-index :always ((array * 2) fixnum fixnum) :fixnum
      "mkcl_array_row_major_index_2_index(env, #0, #1, #2) /*2*/")
    (def-inline array-row-major-index :always ((array * 3) fixnum fixnum fixnum) :fixnum
      "mkcl_array_row_major_index_3_index(env, #0, #1, #2, #3) /*2*/")

    (def-inline array-row-major-index :always (array t t) :cl-index "mkcl_array_row_major_index_2_t(env, #0, #1, #2) /*3*/")
    (def-inline array-row-major-index :always (array t t t) :cl-index "mkcl_array_row_major_index_3_t(env, #0, #1, #2, #3) /*3*/")
    (def-inline array-row-major-index :always ((array * 2) fixnum fixnum) :cl-index
      "mkcl_array_row_major_index_2_index(env, #0, #1, #2) /*3*/")
    (def-inline array-row-major-index :always ((array * 3) fixnum fixnum fixnum) :cl-index
      "mkcl_array_row_major_index_3_index(env, #0, #1, #2, #3) /*3*/")

    (def-inline array-row-major-index :unsafe (array t t) :fixnum
      "(mkcl_fixnum_to_word(#1)*(#0)->array.dims[1]+mkcl_fixnum_to_word(#2))")
    (def-inline array-row-major-index :unsafe (array fixnum fixnum) :fixnum
      "((#1)*(#0)->array.dims[1]+(#2))")
    (def-inline array-row-major-index :unsafe (array t t t) :fixnum
      "((mkcl_fixnum_to_word(#1)*(#0)->array.dims[1]+mkcl_fixnum_to_word(#2))*(#0)->array.dims[2]+mkcl_fixnum_to_word(#3))")
    (def-inline array-row-major-index :unsafe (array fixnum fixnum fixnum) :fixnum
      "(((#1)*(#0)->array.dims[1]+(#2))*(#0)->array.dims[2]+(#3))")



    (proclaim-function array-element-type (array) t)
    (proclaim-function array-rank (array) fixnum)
    (proclaim-function array-dimension (array fixnum) fixnum)
    (proclaim-function array-dimensions (array) list)
    (proclaim-function array-total-size (array) t :no-side-effects t)
    (def-inline array-total-size :always (array) t "MKCL_MAKE_FIXNUM(mkcl_array_total_size(env, #0))")
    (def-inline array-total-size :always (array) :fixnum "mkcl_array_total_size(env, #0)")
    (def-inline array-total-size :always (vector) t "MKCL_MAKE_FIXNUM(mkcl_vector_total_size(env, #0))")
    (def-inline array-total-size :always (vector) :fixnum "mkcl_vector_total_size(env, #0)")
    (def-inline array-total-size :always (string) t "MKCL_MAKE_FIXNUM(mkcl_string_total_size(env, #0))")
    (def-inline array-total-size :always (string) :fixnum "mkcl_string_total_size(env, #0)")
    (def-inline array-total-size :always (base-string) t "MKCL_MAKE_FIXNUM(mkcl_base_string_total_size(env, #0))")
    (def-inline array-total-size :always (base-string) :fixnum "mkcl_base_string_total_size(env, #0)")
    (def-inline array-total-size :unsafe (t) t "MKCL_MAKE_FIXNUM((#0)->array.dim)")
    (def-inline array-total-size :unsafe (t) :fixnum "((#0)->array.dim)")

    (proclaim-function adjustable-array-p (array) t :predicate t)
    (proclaim-function array-displacement (array) (values t fixnum) :predicate t)

    (proclaim-function svref (simple-vector fixnum) t :no-side-effects t)

    (def-inline svref :always ((vector t) t) t "mkcl_svref_index(env, #0, mkcl_safe_fixnum_to_word(env, #1))")
    (def-inline svref :always ((vector t) fixnum) t "mkcl_svref_index(env, #0, #1)")

    (def-inline svref :unsafe (t t) t "((#0)->vector.self.t[mkcl_fixnum_to_word(#1)])")
    (def-inline svref :unsafe (t fixnum) t "((#0)->vector.self.t[#1])")

    (proclaim-function si:svset (simple-vector fixnum t) t)

    (def-inline si:svset :always ((vector t) t t) t "mkcl_svset_index(env, #0, mkcl_safe_fixnum_to_word(env, #1), #2)")
    (def-inline si:svset :always ((vector t) fixnum t) t "mkcl_svset_index(env, #0, #1, #2)")

    (def-inline si:svset :unsafe (t t t) t "((#0)->vector.self.t[mkcl_fixnum_to_word(#1)]=(#2))")
    (def-inline si:svset :unsafe (t fixnum t) t "((#0)->vector.self.t[#1] = (#2))")

    (proclaim-function array-has-fill-pointer-p (array) t :predicate t)
    (proclaim-function fill-pointer (vector) fixnum :no-side-effects t)
    (def-inline fill-pointer :always (vector) t "MKCL_MAKE_FIXNUM(mkcl_vector_fill_pointer(env, #0))")
    (def-inline fill-pointer :always (vector) :fixnum "mkcl_vector_fill_pointer(env, #0)")
    (def-inline fill-pointer :always (string) t "MKCL_MAKE_FIXNUM(mkcl_string_fill_pointer(env, #0))")
    (def-inline fill-pointer :always (string) :fixnum "mkcl_string_fill_pointer(env, #0)")
    (def-inline fill-pointer :always (base-string) t "MKCL_MAKE_FIXNUM(mkcl_base_string_fill_pointer(env, #0))")
    (def-inline fill-pointer :always (base-string) :fixnum "mkcl_base_string_fill_pointer(env, #0)")
    (def-inline fill-pointer :unsafe (t) t "MKCL_MAKE_FIXNUM((#0)->vector.fillp)")
    (def-inline fill-pointer :unsafe (t) :fixnum "((#0)->vector.fillp)")

    (proclaim-function si:fill-pointer-set (vector fixnum) fixnum)
    (def-inline si:fill-pointer-set :always (vector t) t
      "MKCL_MAKE_FIXNUM(mkcl_vector_fill_pointer_set(env, #0, mkcl_safe_fixnum_to_word(env, #1)))")
    (def-inline si:fill-pointer-set :always (vector fixnum) t "MKCL_MAKE_FIXNUM(mkcl_vector_fill_pointer_set(env, #0, #1))")
    (def-inline si:fill-pointer-set :always (vector fixnum) :fixnum "mkcl_vector_fill_pointer_set(env, #0, #1)")
    (def-inline si:fill-pointer-set :always (string t) t
      "MKCL_MAKE_FIXNUM(mkcl_string_fill_pointer_set(env, #0, mkcl_safe_fixnum_to_word(env, #1)))")
    (def-inline si:fill-pointer-set :always (string fixnum) t "MKCL_MAKE_FIXNUM(mkcl_string_fill_pointer_set(env, #0, #1))")
    (def-inline si:fill-pointer-set :always (string fixnum) :fixnum "mkcl_string_fill_pointer_set(env, #0, #1)")
    (def-inline si:fill-pointer-set :always (base-string t) t
      "MKCL_MAKE_FIXNUM(mkcl_base_string_fill_pointer_set(env, #0, mkcl_safe_fixnum_to_word(env, #1)))")
    (def-inline si:fill-pointer-set :always (base-string fixnum) t "MKCL_MAKE_FIXNUM(mkcl_base_string_fill_pointer_set(env, #0, #1))")
    (def-inline si:fill-pointer-set :always (base-string fixnum) :fixnum "mkcl_base_string_fill_pointer_set(env, #0, #1)")
    (def-inline si:fill-pointer-set :unsafe (t fixnum) t "MKCL_MAKE_FIXNUM((#0)->vector.fillp=(#1))")
    (def-inline si:fill-pointer-set :unsafe (t fixnum) :fixnum "((#0)->vector.fillp=(#1))")

    (proclaim-function si:replace-array (t t) t)

    (proclaim-function bit ((array bit) &rest t) bit :no-side-effects t)

    (def-inline bit :always (t t) t "mk_cl_aref(env, 2, #0, #1)")
    (def-inline bit :always (t t t) t "mk_cl_aref(env, 3, #0, #1, #2)")
    (def-inline bit :always (array t) t "mkcl_aref(env, #0, #1)")
    (def-inline bit :always (array fixnum) t "mkcl_aref_index(env, #0, #1)")
    (def-inline bit :always ((array bit) fixnum) t "mkcl_aref_index_bit(env, #0, #1)")
    (def-inline bit :always ((array bit) fixnum) :fixnum "mkcl_aref_index_bit_raw(env, #0, #1)")

    (def-inline bit :unsafe (t t t) t
      "@0;mkcl_aref_index(env, #0,mkcl_fixnum_to_word(#1)*(#0)->array.dims[1]+mkcl_fixnum_to_word(#2))")
    (def-inline bit :unsafe ((array bit) t t) t
      "@0;mkcl_bvref_index(env, #0,mkcl_fixnum_to_word(#1)*(#0)->array.dims[1]+mkcl_fixnum_to_word(#2))")
    (def-inline bit :unsafe ((array bit) fixnum fixnum) t
      "@0;mkcl_bvref_index(env, #0,(#1)*(#0)->array.dims[1]+(#2))")
    (def-inline bit :unsafe ((array bit) fixnum fixnum) :fixnum
      "@0;mkcl_bvref_index_raw(env, #0,(#1)*(#0)->array.dims[1]+(#2))")
    (def-inline bit :unsafe (t t) t "mkcl_bvref_index(env, #0,mkcl_fixnum_to_word(#1))")
    (def-inline bit :unsafe ((array bit) t) t "mkcl_bvref_index(env, #0,mkcl_fixnum_to_word(#1))") ;;not quite unsafe
    (def-inline bit :unsafe ((array bit) fixnum) t "mkcl_bvref_index(env, #0,#1)") ;;not quite unsafe
    (def-inline bit :unsafe ((array bit) fixnum) :fixnum "mkcl_bvref_index_raw(env, #0,#1)") ;;not quite unsafe

    (proclaim-function sbit ((simple-array bit) &rest t) bit :no-side-effects t)

    (def-inline sbit :always (t t) t "mk_cl_aref(env, 2, #0, #1)")
    (def-inline sbit :always (t t t) t "mk_cl_aref(env, 3, #0, #1, #2)")
    (def-inline sbit :always (array t) t "mkcl_aref(env, #0, #1)")
    (def-inline sbit :always (array fixnum) t "mkcl_aref_index(env, #0, #1)")
    (def-inline sbit :always ((array bit) fixnum) t "mkcl_aref_index_bit(env, #0, #1)")
    (def-inline sbit :always ((array bit) fixnum) :fixnum "mkcl_aref_index_bit_raw(env, #0, #1)")

    (def-inline sbit :unsafe (t t t) t
      "@0;mkcl_aref_index(env, #0,mkcl_fixnum_to_word(#1)*(#0)->array.dims[1]+mkcl_fixnum_to_word(#2))")
    (def-inline sbit :unsafe ((array bit) t t) t
      "@0;mkcl_bvref_index(env, #0,mkcl_fixnum_to_word(#1)*(#0)->array.dims[1]+mkcl_fixnum_to_word(#2))")
    (def-inline sbit :unsafe ((array bit) fixnum fixnum) t
      "@0;mkcl_bvref_index(env, #0,(#1)*(#0)->array.dims[1]+(#2))")
    (def-inline sbit :unsafe ((array bit) fixnum fixnum) :fixnum
      "@0;mkcl_bvref_index_raw(env, #0,(#1)*(#0)->array.dims[1]+(#2))")
    (def-inline sbit :unsafe (t t) t "mkcl_bvref_index(env, #0,mkcl_fixnum_to_word(#1))")
    (def-inline sbit :unsafe ((array bit) t) t "mkcl_bvref_index(env, #0,mkcl_fixnum_to_word(#1))") ;;not quite unsafe
    (def-inline sbit :unsafe ((array bit) fixnum) t "mkcl_bvref_index(env, #0,#1)") ;;not quite unsafe
    (def-inline sbit :unsafe ((array bit) fixnum) :fixnum "mkcl_bvref_index_raw(env, #0,#1)") ;;not quite unsafe

    (proclaim-function si:bit-set (bit (array bit) &rest t) bit)

    (def-inline si:bit-set :always (t t t) t "mk_si_aset(env, 3, #0, #1, #2)")
    (def-inline si:bit-set :always (t t t t) t "mk_si_aset(env, 4, #0, #1, #2, #3)")
    (def-inline si:bit-set :always (t array t) t "mkcl_aset(env, #1, #2, #0)")
    (def-inline si:bit-set :always (t array fixnum) t "mkcl_aset_index(env, #1, #2, #0)")
    (def-inline si:bit-set :always (t (array bit) fixnum) t "mkcl_aset_index_bit(env, #1, #2, #0)")
    (def-inline si:bit-set :always (fixnum (array bit) fixnum) t "MKCL_MAKE_FIXNUM(mkcl_aset_index_bit_raw(env, #1, #2, #0))")
    (def-inline si:bit-set :always (fixnum (array bit) fixnum) :fixnum "mkcl_aset_index_bit_raw(env, #1, #2, #0)")

    (def-inline si:bit-set :unsafe (t t t t) t
      "@01;mkcl_aset_index(env, #1,mkcl_fixnum_to_word(#2)*(#1)->array.dims[1]+mkcl_fixnum_to_word(#3), mkcl_fixnum_to_word(#0))")
    (def-inline si:bit-set :unsafe (t (array bit) t t) t
      "@01;mkcl_bvset_index(env, #1,mkcl_fixnum_to_word(#2)*(#1)->array.dims[1]+mkcl_fixnum_to_word(#3), mkcl_fixnum_to_word(#0))")
    (def-inline si:bit-set :unsafe (t (array bit) fixnum fixnum) t
      "@01;MKCL_MAKE_FIXNUM(mkcl_bvset_index_raw(env, #1,(#2)*(#1)->array.dims[1]+(#3), mkcl_fixnum_to_word(#0)))")
    (def-inline si:bit-set :unsafe (t (array bit) fixnum fixnum) :fixnum
      "@01;mkcl_bvset_index_raw(env, #1,(#2)*(#1)->array.dims[1]+(#3), mkcl_fixnum_to_word(#0))")
    (def-inline si:bit-set :unsafe (t t t) t "mkcl_bvset_index(env, #1,mkcl_fixnum_to_word(#2), mkcl_fixnum_to_word(#0))")
    (def-inline si:bit-set :unsafe (t (array bit) t) t "mkcl_bvset_index(env, #1,mkcl_fixnum_to_word(#2), #0)") ;;not quite unsafe
    (def-inline si:bit-set :unsafe (fixnum (array bit) fixnum) t "MKCL_MAKE_FIXNUM(mkcl_bvset_index_raw(env, #1, #2, #0))") ;;not quite unsafe
    (def-inline si:bit-set :unsafe (fixnum (array bit) fixnum) :fixnum "mkcl_bvset_index_raw(env, #1, #2, #0)") ;;not quite unsafe

    (proclaim-function si:sbit-set (bit (simple-array bit) &rest t) bit)

    (def-inline si:sbit-set :always (t t t) t "mk_si_aset(env, 3, #0, #1, #2)")
    (def-inline si:sbit-set :always (t t t t) t "mk_si_aset(env, 4, #0, #1, #2, #3)")
    (def-inline si:sbit-set :always (t array t) t "mkcl_aset(env, #1, #2, #0)")
    (def-inline si:sbit-set :always (t array fixnum) t "mkcl_aset_index(env, #1, #2, #0)")
    (def-inline si:sbit-set :always (t (array bit) fixnum) t "mkcl_aset_index_bit(env, #1, #2, #0)")
    (def-inline si:sbit-set :always (fixnum (array bit) fixnum) t "MKCL_MAKE_FIXNUM(mkcl_aset_index_bit_raw(env, #1, #2, #0))")
    (def-inline si:sbit-set :always (fixnum (array bit) fixnum) :fixnum "mkcl_aset_index_bit_raw(env, #1, #2, #0)")

    (def-inline si:sbit-set :unsafe (t t t t) t
      "@01;mkcl_aset_index(env, #1,mkcl_fixnum_to_word(#2)*(#1)->array.dims[1]+mkcl_fixnum_to_word(#3), mkcl_fixnum_to_word(#0))")
    (def-inline si:sbit-set :unsafe (t (array bit) t t) t
      "@01;mkcl_bvset_index(env, #1,mkcl_fixnum_to_word(#2)*(#1)->array.dims[1]+mkcl_fixnum_to_word(#3), mkcl_fixnum_to_word(#0))")
    (def-inline si:sbit-set :unsafe (t (array bit) fixnum fixnum) t
      "@01;MKCL_MAKE_FIXNUM(mkcl_bvset_index_raw(env, #1,(#2)*(#1)->array.dims[1]+(#3), mkcl_fixnum_to_word(#0)))")
    (def-inline si:sbit-set :unsafe (t (array bit) fixnum fixnum) :fixnum
      "@01;mkcl_bvset_index_raw(env, #1,(#2)*(#1)->array.dims[1]+(#3), #0)")
    (def-inline si:sbit-set :unsafe (t t t) t "mkcl_bvset_index(env, #1,mkcl_fixnum_to_word(#2), #0)")
    (def-inline si:sbit-set :unsafe (t (array bit) t) t "mkcl_bvset_index(env, #1,mkcl_fixnum_to_word(#2), #0)") ;;not quite unsafe
    (def-inline si:sbit-set :unsafe (fixnum (array bit) fixnum) t "MKCL_MAKE_FIXNUM(mkcl_bvset_index_raw(env, #1, #2, #0))") ;;not quite unsafe
    (def-inline si:sbit-set :unsafe (fixnum (array bit) fixnum) :fixnum "mkcl_bvset_index_raw(env, #1, #2, #0)") ;;not quite unsafe

    ;; file assignment.d

    (proclaim-function set (symbol t) t)
    (proclaim-function si:fset ((or symbol (cons (member setf) (cons symbol null))) t &optional t t) t)
    (proclaim-function makunbound (symbol) t)
    (proclaim-function fmakunbound (t) t)
    (proclaim-function si:clear-compiler-properties (t) t)

    ;; file cfun.d

    (proclaim-function si:compiled-function-name (t) t)
    (proclaim-function si:set-compiled-function-name (t t) t)

    ;; file character.d

    (proclaim-function standard-char-p (character) t :predicate t)
    (def-inline standard-char-p :always (character) :bool "mkcl_standard_char_p(#0)")

    (proclaim-function graphic-char-p (character) t :predicate t)
    (def-inline graphic-char-p :always (character) :bool "mkcl_graphic_char_p(#0)")

    (proclaim-function alpha-char-p (character) t :predicate t :no-side-effects t)
    (def-inline alpha-char-p :always (character) :bool "mkcl_alpha_char_p(#0)")

    (proclaim-function upper-case-p (character) t :predicate t :no-side-effects t)
    (def-inline upper-case-p :always (character) :bool "mkcl_upper_case_p(#0)")

    (proclaim-function lower-case-p (character) t :predicate t :no-side-effects t)
    (def-inline lower-case-p :always (character) :bool "mkcl_lower_case_p(#0)")

    (proclaim-function both-case-p (character) t :predicate t :no-side-effects t)
    (def-inline both-case-p :always (character) :bool "mkcl_both_case_p(#0)")

    (proclaim-function digit-char-p (character &optional t) t :no-side-effects t)

    (proclaim-function alphanumericp (character) t :predicate t :no-side-effects t)
    (def-inline alphanumericp :always (character) :bool "mkcl_alphanumericp(#0)")

    (proclaim-function character (t) character)
    (proclaim-function char= (character &rest t) t :predicate t :no-side-effects t)
    (def-inline char= :always (t t) :bool "mkcl_char_code(env, #0)==mkcl_char_code(env, #1)")
    (def-inline char= :always (character character) :bool "(#0)==(#1)")

    (proclaim-function char/= (character &rest t) t :predicate t :no-side-effects t)
    (def-inline char/= :always (t t) :bool "mkcl_char_code(env, #0)!=mkcl_char_code(env, #1)")
    (def-inline char/= :always (character character) :bool "(#0)!=(#1)")

    (proclaim-function char< (character &rest t) t :predicate t :no-side-effects t)
    (def-inline char< :always (character character) :bool "(#0)<(#1)")

    (proclaim-function char> (character &rest t) t :predicate t :no-side-effects t)
    (def-inline char> :always (character character) :bool "(#0)>(#1)")

    (proclaim-function char<= (character &rest t) t :predicate t :no-side-effects t)
    (def-inline char<= :always (character character) :bool "(#0)<=(#1)")

    (proclaim-function char>= (character &rest t) t :predicate t :no-side-effects t)
    (def-inline char>= :always (character character) :bool "(#0)>=(#1)")

    (proclaim-function char-equal (character &rest t) t :predicate t)
    (proclaim-function char-not-equal (character &rest t) t :predicate t)
    (proclaim-function char-lessp (character &rest t) t :predicate t)
    (proclaim-function char-greaterp (character &rest t) t :predicate t)
    (proclaim-function char-not-greaterp (character &rest t) t :predicate t)
    (proclaim-function char-not-lessp (character &rest t) t :predicate t)
    (proclaim-function character (t) character)
    (proclaim-function char-code (character) fixnum :no-side-effects t)
    (def-inline char-code :always (character) t "(MKCL_MAKE_FIXNUM(#0))")
    (def-inline char-code :always (character) :fixnum "(#0)")

    (proclaim-function code-char (fixnum) (or character null) :no-side-effects t)
    (def-inline code-char :always (fixnum) t
      "@0;(((0 <= (#0) && (#0) < 0x0D800) || (0x0DFFF < (#0) && (#0) != 0x0FFFE && (#0) != 0x0FFFF && (#0) < MKCL_CHAR_CODE_LIMIT)) ? MKCL_CODE_CHAR(#0) : mk_cl_Cnil)")
    ;;(def-inline code-char :always (fixnum) :char "(#0) /* (code-char fixnum) */")
    ;;#+unicode  ;; For identical args signature, largest return type must be last.
    ;;(def-inline code-char :always (fixnum) :wchar "(#0)")

    (proclaim-function char-upcase (character) character :no-side-effects t)
    (def-inline char-upcase :always (character) t "(MKCL_CODE_CHAR(mkcl_char_upcase(#0)))")
    (def-inline char-upcase :always (character) :wchar "mkcl_char_upcase(#0)")
    (def-inline char-upcase :always (base-char) :char "mkcl_char_upcase(#0)")

    (proclaim-function char-downcase (character) character :no-side-effects t)
    (def-inline char-downcase :always (character) t "(MKCL_CODE_CHAR(mkcl_char_downcase(#0)))")
    (def-inline char-downcase :always (character) :wchar "mkcl_char_downcase(#0)")
    (def-inline char-downcase :always (base-char) :char "mkcl_char_downcase(#0)")

    (proclaim-function digit-char (fixnum &optional t) (or character null))
    (proclaim-function char-int (character) fixnum :no-side-effects t)
    (def-inline char-int :always (character) t "(MKCL_MAKE_FIXNUM(#0))")
    (def-inline char-int :always (character) :fixnum "(#0)")

    (proclaim-function char-name (character) (or string null))
    (proclaim-function name-char (string) (or character null))

    ;; file error.d

    (proclaim-function error (t &rest t) t)
    (proclaim-function cerror (t t &rest t) null)

    ;; file stacks.d

    (proclaim-function si:disable-interrupts () t)
    (def-inline si:disable-interrupts :always nil t
      "((env->disable_interrupts) ? mk_cl_Cnil : ((env->disable_interrupts = 1), mk_cl_Ct))")

    (proclaim-function si:enable-interrupts () t)
    (def-inline si:enable-interrupts :always nil t "(env->disable_interrupts = 0)")

    (proclaim-function si:ihs-top () t)
    (proclaim-function si:ihs-fun (t) t)
    (proclaim-function si:ihs-env (t) t)
    (proclaim-function si:frs-top () t)
    (proclaim-function si:frs-bds (t) t)
    (proclaim-function si:frs-tag (t) t)
    (proclaim-function si:frs-ihs (t) t)
    (proclaim-function si:bds-top () t)
    (proclaim-function si:bds-var (t) t)
    (proclaim-function si:bds-val (t) t)
    (proclaim-function si:sch-frs-base (t t) t)

    ;; file eval.d

    (proclaim-function apply (t &rest t) *)
    (proclaim-function funcall (t &rest t) *)
    (proclaim-function eval (t) *)
    ;;(proclaim-function evalhook (t t t *) t) ;; where is it? JCB
    ;;(proclaim-function applyhook (t t t t *) t) ;; where is it? JCB
    (proclaim-function constantp (t &optional t) t :predicate t)

    ;; file file.d

    (proclaim-function make-synonym-stream (symbol) synonym-stream)
    (proclaim-function make-broadcast-stream (&rest t) broadcast-stream)
    (proclaim-function make-concatenated-stream (&rest t) concatenated-stream)
    (proclaim-function make-two-way-stream (stream stream) two-way-stream)
    (proclaim-function make-echo-stream (stream stream) echo-stream)
    (proclaim-function make-string-input-stream (t &optional t t &key (start t) (end t)) string-stream)
    (proclaim-function make-string-output-stream (&key (element-type t)) string-stream)

    (proclaim-function broadcast-stream-streams (t) t)

    (proclaim-function concatenated-stream-streams (t) list)

    (proclaim-function get-output-stream-string (string-stream) string)
    (proclaim-function streamp (t) t :predicate t)
    (proclaim-function input-stream-p (stream) t :predicate t)
    (def-inline input-stream-p :always (stream) :bool "mkcl_input_stream_p(env, #0)")

    (proclaim-function output-stream-p (stream) t :predicate t)
    (def-inline output-stream-p :always (stream) :bool "mkcl_output_stream_p(env, #0)")

    (proclaim-function stream-element-type (t) t)
    (proclaim-function close (stream &key (abort t)) t)
    (proclaim-function file-position (stream &optional t) t)
    (proclaim-function file-length (stream) t)
    (proclaim-function si:make-string-output-stream-from-string (string (or symbol cons)) string-stream)

    ;; file unixfsys.d

    (proclaim-function truename (t) t)
    (proclaim-function rename-file (t t) (values t t t))
    (proclaim-function si:specialp (t) t :predicate t)
    (proclaim-function delete-file (t) t)
    (proclaim-function probe-file (t) t)
    (proclaim-function file-write-date (t) t)
    (proclaim-function file-author (t) t)
    (proclaim-function pathname (t) t)
    (proclaim-function user-homedir-pathname (&optional t) t)
    (proclaim-function directory (t &key (follow-symlinks t)) t)
    (proclaim-function mkcl:chdir (t &optional t) pathname)
    (proclaim-function mkcl:getcwd (&key (all-drives t) (change-default-pathname-defaults t)) pathname)
    (proclaim-function mkcl:mkdir (t fixnum) string)

    ;; file unixint.d

    ;; file format.d

    (proclaim-function format (t string &rest t) t)

    ;; file hash.d

    (proclaim-function make-hash-table (&key (size t) (test t) (rehash-size t) (rehash-threshold t)) t)
    (proclaim-function hash-table-p (t) t :predicate t)
    (proclaim-function gethash (t t &optional t) (values t t))
    (proclaim-function remhash (t t) t)
    (proclaim-function maphash (t t) null)
    (proclaim-function clrhash (t) t)
    (proclaim-function hash-table-count (t) si::index)
    (proclaim-function sxhash (t) fixnum)
    (proclaim-function si:hash-set (t t t) t)

    ;; file list.d

    (proclaim-function car (list) t :no-side-effects t)
    (def-inline car :unsafe (list) t "@0;MKCL_CAR(#0)") ;; Is this really safe? JCB

    (proclaim-function cdr (list) t :no-side-effects t)
    (def-inline cdr :unsafe (list) t "@0;MKCL_CDR(#0)") ;; Is this really safe? JCB

    (proclaim-function caar (list) t :no-side-effects t)
    (def-inline caar :always (list) t "mk_cl_car(env, mk_cl_car(env, #0))")
    (def-inline caar :unsafe (list) t "@0;MKCL_CAAR(#0)")

    (proclaim-function cadr (list) t :no-side-effects t)
    (def-inline cadr :always (list) t "mk_cl_car(env, mk_cl_cdr(env, #0))")
    (def-inline cadr :unsafe (list) t "@0;MKCL_CADR(#0)")

    (proclaim-function cdar (list) t :no-side-effects t)
    (def-inline cdar :always (list) t "mk_cl_cdr(env, mk_cl_car(env, #0))")
    (def-inline cdar :unsafe (list) t "@0;MKCL_CDAR(#0)")

    (proclaim-function cddr (list) t :no-side-effects t)
    (def-inline cddr :always (list) t "mk_cl_cdr(env, mk_cl_cdr(env, #0))")
    (def-inline cddr :unsafe (list) t "@0;MKCL_CDDR(#0)")

    (proclaim-function caaar (list) t :no-side-effects t)
    (def-inline caaar :always (list) t "mk_cl_car(env, mk_cl_car(env, mk_cl_car(env, #0)))")
    (def-inline caaar :unsafe (list) t "@0;MKCL_CAAAR(#0)")

    (proclaim-function caadr (list) t :no-side-effects t)
    (def-inline caadr :always (list) t "mk_cl_car(env, mk_cl_car(env, mk_cl_cdr(env, #0)))")
    (def-inline caadr :unsafe (list) t "@0;MKCL_CAADR(#0)")

    (proclaim-function cadar (list) t :no-side-effects t)
    (def-inline cadar :always (list) t "mk_cl_car(env, mk_cl_cdr(env, mk_cl_car(env, #0)))")
    (def-inline cadar :unsafe (list) t "@0;MKCL_CADAR(#0)")

    (proclaim-function caddr (list) t :no-side-effects t)
    (def-inline caddr :always (list) t "mk_cl_car(env, mk_cl_cdr(env, mk_cl_cdr(env, #0)))")
    (def-inline caddr :unsafe (list) t "@0;MKCL_CADDR(#0)")

    (proclaim-function cdaar (list) t :no-side-effects t)
    (def-inline cdaar :always (list) t "mk_cl_cdr(env, mk_cl_car(env, mk_cl_car(env, #0)))")
    (def-inline cdaar :unsafe (list) t "@0;MKCL_CDAAR(#0)")

    (proclaim-function cdadr (list) t :no-side-effects t)
    (def-inline cdadr :always (list) t "mk_cl_cdr(env, mk_cl_car(env, mk_cl_cdr(env, #0)))")
    (def-inline cdadr :unsafe (list) t "@0;MKCL_CDADR(#0)")

    (proclaim-function cddar (list) t :no-side-effects t)
    (def-inline cddar :always (list) t "mk_cl_cdr(env, mk_cl_cdr(env, mk_cl_car(env, #0)))")
    (def-inline cddar :unsafe (list) t "@0;MKCL_CDDAR(#0)")

    (proclaim-function cdddr (list) t :no-side-effects t)
    (def-inline cdddr :always (list) t "mk_cl_cdr(env, mk_cl_cdr(env, mk_cl_cdr(env, #0)))")
    (def-inline cdddr :unsafe (list) t "@0;MKCL_CDDDR(#0)")

    (proclaim-function caaaar (list) t :no-side-effects t)
    (def-inline caaaar :always (list) t "mk_cl_car(env, mk_cl_car(env, mk_cl_car(env, mk_cl_car(env, #0))))")
    (def-inline caaaar :unsafe (list) t "@0;MKCL_CAAAAR(#0)")

    (proclaim-function caaadr (list) t :no-side-effects t)
    (def-inline caaadr :always (list) t "mk_cl_car(env, mk_cl_car(env, mk_cl_car(env, mk_cl_cdr(env, #0))))")
    (def-inline caaadr :unsafe (list) t "@0;MKCL_CAAADR(#0)")

    (proclaim-function caadar (list) t :no-side-effects t)
    (def-inline caadar :always (list) t "mk_cl_car(env, mk_cl_car(env, mk_cl_cdr(env, mk_cl_car(env, #0))))")
    (def-inline caadar :unsafe (list) t "@0;MKCL_CAADAR(#0)")

    (proclaim-function caaddr (list) t :no-side-effects t)
    (def-inline caaddr :always (list) t "mk_cl_car(env, mk_cl_car(env, mk_cl_cdr(env, mk_cl_cdr(env, #0))))")
    (def-inline caaddr :unsafe (list) t "@0;MKCL_CAADDR(#0)")

    (proclaim-function cadaar (list) t :no-side-effects t)
    (def-inline cadaar :always (list) t "mk_cl_car(env, mk_cl_cdr(env, mk_cl_car(env, mk_cl_car(env, #0))))")
    (def-inline cadaar :unsafe (list) t "@0;MKCL_CADAAR(#0)")

    (proclaim-function cadadr (list) t :no-side-effects t)
    (def-inline cadadr :always (list) t "mk_cl_car(env, mk_cl_cdr(env, mk_cl_car(env, mk_cl_cdr(env, #0))))")
    (def-inline cadadr :unsafe (list) t "@0;MKCL_CADADR(#0)")

    (proclaim-function caddar (list) t :no-side-effects t)
    (def-inline caddar :always (list) t "mk_cl_car(env, mk_cl_cdr(env, mk_cl_cdr(env, mk_cl_car(env, #0))))")
    (def-inline caddar :unsafe (list) t "@0;MKCL_CADDAR(#0)")

    (proclaim-function cadddr (list) t :no-side-effects t)
    (def-inline cadddr :always (list) t "mk_cl_car(env, mk_cl_cdr(env, mk_cl_cdr(env, mk_cl_cdr(env, #0))))")
    (def-inline cadddr :unsafe (list) t "@0;MKCL_CADDDR(#0)")

    (proclaim-function cdaaar (list) t :no-side-effects t)
    (def-inline cdaaar :always (list) t "mk_cl_cdr(env, mk_cl_car(env, mk_cl_car(env, mk_cl_car(env, #0))))")
    (def-inline cdaaar :unsafe (list) t "@0;MKCL_CDAAAR(#0)")

    (proclaim-function cdaadr (list) t :no-side-effects t)
    (def-inline cdaadr :always (list) t "mk_cl_cdr(env, mk_cl_car(env, mk_cl_car(env, mk_cl_cdr(env, #0))))")
    (def-inline cdaadr :unsafe (list) t "@0;MKCL_CDAADR(#0)")

    (proclaim-function cdadar (list) t :no-side-effects t)
    (def-inline cdadar :always (list) t "mk_cl_cdr(env, mk_cl_car(env, mk_cl_cdr(env, mk_cl_car(env, #0))))")
    (def-inline cdadar :unsafe (list) t "@0;MKCL_CDADAR(#0)")

    (proclaim-function cdaddr (list) t :no-side-effects t)
    (def-inline cdaddr :always (list) t "mk_cl_cdr(env, mk_cl_car(env, mk_cl_cdr(env, mk_cl_cdr(env, #0))))")
    (def-inline cdaddr :unsafe (list) t "@0;MKCL_CDADDR(#0)")

    (proclaim-function cddaar (list) t :no-side-effects t)
    (def-inline cddaar :always (list) t "mk_cl_cdr(env, mk_cl_cdr(env, mk_cl_car(env, mk_cl_car(env, #0))))")
    (def-inline cddaar :unsafe (list) t "@0;MKCL_CDDAAR(#0)")

    (proclaim-function cddadr (list) t :no-side-effects t)
    (def-inline cddadr :always (list) t "mk_cl_cdr(env, mk_cl_cdr(env, mk_cl_car(env, mk_cl_cdr(env, #0))))")
    (def-inline cddadr :unsafe (list) t "@0;MKCL_CDDADR(#0)")

    (proclaim-function cdddar (list) t :no-side-effects t)
    (def-inline cdddar :always (list) t "mk_cl_cdr(env, mk_cl_cdr(env, mk_cl_cdr(env, mk_cl_car(env, #0))))")
    (def-inline cdddar :unsafe (list) t "@0;MKCL_CDDDAR(#0)")

    (proclaim-function cddddr (list) t :no-side-effects t)
    (def-inline cddddr :always (list) t "mk_cl_cdr(env, mk_cl_cdr(env, mk_cl_cdr(env, mk_cl_cdr(env, #0))))")
    (def-inline cddddr :unsafe (list) t "@0;MKCL_CDDDDR(#0)")

    (proclaim-function cons (t t) cons :no-side-effects t)
    (def-inline cons :always (t t) t "MKCL_CONS(env, #0,#1)")

    (proclaim-function tree-equal (t t &key (test t) (test-not t)) t :predicate t)
    (proclaim-function endp (list) t :predicate t :no-side-effects t)
    (def-inline endp :always (t) :bool "mkcl_endp(env, #0)")
    (def-inline endp :always (list) :bool "mkcl_Null(#0)")

    (proclaim-function list-length (list) (or null (integer 0 *)))
    (proclaim-function nth (integer list) t :no-side-effects t)
    (def-inline nth :always (t t) t "mkcl_nth(env, mkcl_safe_fixnum_to_word(env, #0),#1)")
    (def-inline nth :always (fixnum t) t "mkcl_nth(env, #0,#1)")
    (def-inline nth :unsafe (t t) t "mkcl_nth(env, mkcl_fixnum_to_word(#0),#1)")
    (def-inline nth :unsafe (fixnum t) t "mkcl_nth(env, #0,#1)")

    (proclaim-function first (list) t :no-side-effects t)
    (def-inline first :unsafe (list) t "@0;MKCL_CAR(#0)")

    (proclaim-function second (list) t :no-side-effects t)
    (def-inline second :always (list) t "mk_cl_car(env, mk_cl_cdr(env, #0))")

    (proclaim-function third (list) t :no-side-effects t)
    (def-inline third :always (list) t "mk_cl_car(env, mk_cl_cdr(env, mk_cl_cdr(env, #0)))")

    (proclaim-function fourth (list) t :no-side-effects t)
    (def-inline fourth :always (list) t "mk_cl_car(env, mk_cl_cdr(env, mk_cl_cdr(env, mk_cl_cdr(env, #0))))")

    (proclaim-function fifth (list) t)
    (proclaim-function sixth (list) t)
    (proclaim-function seventh (list) t)
    (proclaim-function eighth (list) t)
    (proclaim-function ninth (list) t)
    (proclaim-function tenth (list) t)
    (proclaim-function rest (list) t :no-side-effects t)
    (def-inline rest :unsafe (list) t "@0;MKCL_CDR(#0)")

    (proclaim-function nthcdr (fixnum list) t :no-side-effects t)
    (def-inline nthcdr :always (t t) t "mkcl_nthcdr(env, mkcl_safe_fixnum_to_word(env, #0),#1)")
    (def-inline nthcdr :always (fixnum t) t "mkcl_nthcdr(env, #0,#1)")
    (def-inline nthcdr :unsafe (t t) t "mkcl_nthcdr(env, mkcl_fixnum_to_word(#0),#1)")
    (def-inline nthcdr :unsafe (fixnum t) t "mkcl_nthcdr(env, #0,#1)")

    (proclaim-function last (list &optional integer) t)
    (def-inline last :always (t) t "mkcl_last(env, #0,1)")

    (proclaim-function list (&rest t) list :no-side-effects t)
    (def-inline list :always nil t "mk_cl_Cnil")
    (def-inline list :always (t) t "mkcl_list1(env, #0)")

    (proclaim-function list* (t &rest t) list :no-side-effects t)
    (def-inline list* :always nil t "mk_cl_Cnil")
    (def-inline list* :always (t) t "(#0)")
    (def-inline list* :always (t t) t "MKCL_CONS(env, #0,#1)")

    (proclaim-function make-list (fixnum &key (initial-element t)) list)
    (proclaim-function append (&rest t) list :no-side-effects t)
    (def-inline append :always (t t) t "mkcl_append(env, #0,#1)")

    (proclaim-function copy-list (list) list)
    (proclaim-function copy-alist (list) list)
    (proclaim-function copy-tree (t) t)
    (proclaim-function revappend (list t) t)
    (proclaim-function nconc (&rest t) t)
    (def-inline nconc :always (t t) t "mkcl_nconc(env, #0,#1)")

    (proclaim-function nreconc (list t) t)
    (proclaim-function butlast (list &optional t) list)
    (def-inline butlast :always (t) t "mkcl_butlast(env, #0,1)")
    (proclaim-function nbutlast (list &optional t) list)
    (def-inline nbutlast :always (t) t "mkcl_nbutlast(env, #0,1)")
    (proclaim-function ldiff (list t) list)
    (proclaim-function rplaca (cons t) cons)
    (def-inline rplaca :always (t t) t
      "@0;(MKCL_ATOM(#0) ? (mkcl_FEtype_error_cons(env, #0), (#0)) : (MKCL_RPLACA((#0), (#1)), (#0)))")
    (proclaim-function rplacd (cons t) cons)
    (def-inline rplacd :always (t t) t
      "@0;(MKCL_ATOM(#0) ? (mkcl_FEtype_error_cons(env, #0), (#0)) : (MKCL_RPLACD((#0), (#1)), (#0)))")
    (proclaim-function subst (t t t &key (key t) (test t) (test-not t)) t)
    (proclaim-function subst-if (t t t &key (key t)) t)
    (proclaim-function subst-if-not (t t t &key (key t)) t)
    (proclaim-function nsubst (t t t &key (key t) (test t) (test-not t)) t)
    (proclaim-function nsubst-if (t t t &key (key t)) t)
    (proclaim-function nsubst-if-not (t t t &key (key t)) t)
    (proclaim-function sublis (list t &key (key t) (test t) (test-not t)) t)
    (proclaim-function nsublis (list t &key (key t) (test t) (test-not t)) t)
    (proclaim-function member (t list &key (key t) (test t) (test-not t)) list)
    (proclaim-function member-if (t list &key (key t)) list)
    (proclaim-function member-if-not (t list &key (key t)) list)
    (proclaim-function si::member1 (t t t t t) t)
    (proclaim-function tailp (t list) t :predicate t)
    (proclaim-function adjoin (t list &key (key t) (test t) (test-not t)) list)
    (proclaim-function acons (t t list) list)
    (def-inline acons :always (t t t) t "MKCL_CONS(env, MKCL_CONS(env, (#0), (#1)), (#2))")
    (proclaim-function pairlis (list list &optional t) list)
    (proclaim-function assoc (t list &key (key t) (test t) (test-not t)) list)
    (proclaim-function assoc-if (t list &key (key t)) list)
    (proclaim-function assoc-if-not (t list &key (key t)) list)
    (proclaim-function rassoc (t list &key (key t) (test t) (test-not t)) list)
    (proclaim-function rassoc-if (t list &key (key t)) list)
    (proclaim-function rassoc-if-not (t list &key (key t)) list)
    (proclaim-function si::memq (t t) t)

    ;; file macros.d

    (proclaim-function macroexpand (t &optional t) (values t t))
    (proclaim-function macroexpand-1 (t &optional t) (values t t))

    ;; file main.d

    (proclaim-function mkcl:quit (&key (exit-code t) (verbose t) (clean t)) t)
    (proclaim-function identity (t) t)
    (proclaim-function mkcl:argc () t)
    (proclaim-function mkcl:argv (t) t)
    (proclaim-function mkcl:getenv (t) t)
    (proclaim-function si:pointer (t) t)

    ;; file mapfun.d

    (proclaim-function mapcar (t t &rest t) t)
    (proclaim-function maplist (t t &rest t) t)
    (proclaim-function mapc (t t &rest t) t)
    (proclaim-function mapl (t t &rest t) t)
    (proclaim-function mapcan (t t &rest t) t)
    (proclaim-function mapcon (t t &rest t) t)

    ;; file multival.d

    (proclaim-function values (&rest t) *)
    (proclaim-function values-list (t) *)

    ;; file num_arith.d

    (proclaim-function + (&rest t) t :no-side-effects t)
    (def-inline + :always (t t) t "mkcl_plus(env, #0,#1) /*t0+*/")
    #+long-float
    (progn
      (def-inline + :always (long-float long-float) :long-double "(long double)(#0)+(long double)(#1) /*ld1+*/") ;; redundant casts
      (def-inline + :always (long-float double-float) :long-double "(long double)(#0)+(long double)(#1) /*ld2+*/") ;; redundant casts
      (def-inline + :always (long-float single-float) :long-double "(long double)(#0)+(long double)(#1) /*ld2+*/") ;; redundant casts
      (def-inline + :always (long-float fixnum) :long-double "(long double)(#0)+(long double)(#1) /*ld2+*/") ;; redundant casts
      (def-inline + :always (double-float long-float) :long-double "(long double)(#0)+(long double)(#1) /*ld2+*/") ;; redundant casts
      (def-inline + :always (single-float long-float) :long-double "(long double)(#0)+(long double)(#1) /*ld2+*/") ;; redundant casts
      (def-inline + :always (fixnum long-float) :long-double "(long double)(#0)+(long double)(#1) /*ld2+*/") ;; redundant casts
      )
    (def-inline + :always (double-float double-float) :double "(double)(#0)+(double)(#1) /*d1.1+*/") ;; redundant casts
    (def-inline + :always (double-float single-float) :double "(double)(#0)+(double)(#1) /*d1.2+*/") ;; redundant casts
    (def-inline + :always (double-float fixnum) :double "(double)(#0)+(double)(#1) /*d1.3+*/") ;; redundant casts
    (def-inline + :always (single-float double-float) :double "(double)(#0)+(double)(#1) /*d1.4+*/") ;; redundant casts
    (def-inline + :always (fixnum double-float) :double "(double)(#0)+(double)(#1) /*d1.5+*/") ;; redundant casts
    (def-inline + :always (single-float single-float) :float "(float)(#0)+(float)(#1) /*f3.1+*/") ;; redundant casts
    (def-inline + :always (single-float fixnum) :float "(float)(#0)+(float)(#1) /*f3.3+*/") ;; redundant casts
    (def-inline + :always (fixnum single-float) :float "(float)(#0)+(float)(#1) /*f3.2+*/") ;; redundant casts
    (def-inline + :always (fixnum fixnum) t "mkcl_make_integer(env, (#0)+(#1)) /*fix0+*/")
    (def-inline + :always (fixnum fixnum) :fixnum "(#0)+(#1) /*fix3+*/" :exact-return-type t)

    (proclaim-function - (t &rest t) t :no-side-effects t)
    (def-inline - :always (t t) t "mkcl_minus(env, #0,#1)")
    (def-inline - :always (double-float double-float) :double "(double)(#0)-(double)(#1)") ;; redundant casts
    (def-inline - :always (double-float single-float) :double "(double)(#0)-(double)(#1)") ;; redundant casts
    (def-inline - :always (double-float fixnum) :double "(double)(#0)-(double)(#1)") ;; redundant casts
    (def-inline - :always (single-float double-float) :double "(double)(#0)-(double)(#1)") ;; redundant casts
    (def-inline - :always (fixnum double-float) :double "(double)(#0)-(double)(#1)") ;; redundant casts
    (def-inline - :always (single-float single-float) :float "(float)(#0)-(float)(#1)") ;; redundant casts
    (def-inline - :always (single-float fixnum) :float "(float)(#0)-(float)(#1)") ;; redundant casts
    (def-inline - :always (fixnum single-float) :float "(float)(#0)-(float)(#1)") ;; redundant casts
    (def-inline - :always (fixnum fixnum) t "mkcl_make_integer(env, (#0)-(#1))")
    (def-inline - :always (fixnum fixnum) :fixnum "(#0)-(#1)" :exact-return-type t)

    (def-inline - :always (t) t "mkcl_negate(env, #0)")
    (def-inline - :always (double-float) :double "(-(double)(#0))") ;; redundant casts
    (def-inline - :always (single-float) :double "(-(double)(#0))")
    (def-inline - :always (fixnum) :double "(-(double)(#0))")
    (def-inline - :always (single-float) :float "(-(float)(#0))") ;; redundant casts
    (def-inline - :always (fixnum) :float "(-(float)(#0))")
    (def-inline - :always (fixnum) t "mkcl_make_integer(env, -(#0))")
    (def-inline - :always (fixnum) :fixnum "(-(#0))")

    (proclaim-function * (&rest t) t :no-side-effects t)
    (def-inline * :always (t t) t "mkcl_times(env, #0,#1)")
    (def-inline * :always (double-float double-float) :double "(double)(#0)*(double)(#1)") ;; redundant casts
    (def-inline * :always (double-float single-float) :double "(double)(#0)*(double)(#1)") ;; redundant casts
    (def-inline * :always (double-float fixnum) :double "(double)(#0)*(double)(#1)") ;; redundant casts
    (def-inline * :always (single-float double-float) :double "(double)(#0)*(double)(#1)") ;; redundant casts
    (def-inline * :always (fixnum double-float) :double "(double)(#0)*(double)(#1)") ;; redundant casts
    (def-inline * :always (single-float single-float) :float "(float)(#0)*(float)(#1)") ;; redundant casts
    (def-inline * :always (single-float fixnum) :float "(float)(#0)*(float)(#1)") ;; redundant casts
    (def-inline * :always (fixnum single-float) :float "(float)(#0)*(float)(#1)") ;; redundant casts
    (def-inline * :always (fixnum fixnum) t "mkcl_word_times(env, #0,#1)")
    (def-inline * :always (fixnum fixnum) :fixnum "(#0)*(#1)" :exact-return-type t)

    (proclaim-function / (t &rest t) t :no-side-effects t)
    (def-inline / :always (t t) t "mkcl_divide(env, #0,#1)")
    (def-inline / :always (double-float double-float) :double "(double)(#0)/(double)(#1)") ;; redundant casts
    (def-inline / :always (double-float single-float) :double "(double)(#0)/(double)(#1)") ;; redundant casts
    (def-inline / :always (double-float fixnum) :double "(double)(#0)/(double)(#1)") ;; redundant casts
    (def-inline / :always (single-float double-float) :double "(double)(#0)/(double)(#1)") ;; redundant casts
    (def-inline / :always (fixnum double-float) :double "(double)(#0)/(double)(#1)") ;; redundant casts
    (def-inline / :always (single-float single-float) :float "(float)(#0)/(float)(#1)") ;; redundant casts
    (def-inline / :always (single-float fixnum) :float "(float)(#0)/(float)(#1)") ;; redundant casts
    (def-inline / :always (fixnum single-float) :float "(float)(#0)/(float)(#1)") ;; redundant casts
    (def-inline / :always (fixnum fixnum) t "MKCL_MAKE_FIXNUM((#0)/(#1))")
    (def-inline / :always (fixnum fixnum) :fixnum "((#0)/(#1))")

    (proclaim-function 1+ (t) t :no-side-effects t)
    (def-inline 1+ :always (t) t "mkcl_one_plus(env, #0)")
    (def-inline 1+ :always (double-float) :double "((double)(#0)+1)")
    (def-inline 1+ :always (single-float) :float "((float)(#0)+1)")
    (def-inline 1+ :always (fixnum) t
      "@0;(((#0) != MKCL_MOST_POSITIVE_FIXNUM) ? MKCL_MAKE_FIXNUM((#0)+1): mkcl_make_integer(env, (#0)+1))")
    (def-inline 1+ :always (fixnum) :fixnum "((#0)+1)" :exact-return-type t)

    (proclaim-function 1- (t) t :no-side-effects t)
    (def-inline 1- :always (t) t "mkcl_one_minus(env, #0)")
    (def-inline 1- :always (double-float) :double "((double)(#0)-1)")
    (def-inline 1- :always (single-float) :float "((float)(#0)-1)")
    (def-inline 1- :always (fixnum) t
      "@0;(((#0) != MKCL_MOST_NEGATIVE_FIXNUM) ? MKCL_MAKE_FIXNUM((#0)-1): mkcl_make_integer(env, (#0)-1))")
    (def-inline 1- :always (fixnum) :fixnum "((#0)-1)" :exact-return-type t)

    (proclaim-function conjugate (t) t)
    (proclaim-function gcd (&rest t) t)
    (proclaim-function lcm (&rest t) t)

    ;; file num_co.d

    (proclaim-function float (number &optional t) float :no-side-effects t)
    (def-inline float :always (t double-float) :double "mkcl_to_double(env, #0)")
    (def-inline float :always (t single-float) :float "mkcl_to_float(env, #0)")
    (def-inline float :always (double-float double-float) :double "((double)(#0))") ;; noop
    (def-inline float :always (single-float double-float) :double "((double)(#0))")
    (def-inline float :always (fixnum double-float) :double "((double)(#0))")
    (def-inline float :always (double-float single-float) :float "((float)(#0))")
    (def-inline float :always (single-float single-float) :float "((float)(#0))") ;; noop
    (def-inline float :always (fixnum single-float) :float "((float)(#0))")

    (def-inline float :always (double-float) :double "(#0)") ;; noop, as is as per spec.
    (def-inline float :always (single-float) :float "(#0)") ;; noop, as is as per spec.
    (def-inline float :always (fixnum) :float "((float)(#0))")

    (proclaim-function numerator (t) t)
    (proclaim-function denominator (t) t)
    (proclaim-function floor (t &optional t) (values t t) :no-side-effects t)
    (proclaim-function ffloor (t &optional t) (values t t) :no-side-effects t)

    (proclaim-function ceiling (t &optional t) (values t t))
    (proclaim-function fceiling (t &optional t) (values t t))
    (proclaim-function truncate (t &optional t) (values t t) :no-side-effects t)
    (proclaim-function ftruncate (t &optional t) (values t t) :no-side-effects t)

    (proclaim-function round (t &optional t) (values t t))
    (proclaim-function fround (t &optional t) (values t t))

    (proclaim-function mod (t t) t :no-side-effects t)
    (def-inline mod :always (fixnum fixnum) t
      "@01;MKCL_MAKE_FIXNUM(((#0)>=0&&(#1)>0) ? (#0)%(#1) : mkcl_imod(env, #0,#1))")
    (def-inline mod :always (fixnum fixnum) :fixnum
      "@01;(((#0)>=0&&(#1)>0) ? (#0)%(#1) : mkcl_imod(env, #0,#1))")

    (proclaim-function rem (t t) t :no-side-effects t)
    (def-inline rem :always (fixnum fixnum) t "MKCL_MAKE_FIXNUM((#0)%(#1))")
    (def-inline rem :always (fixnum fixnum) :fixnum "((#0)%(#1))")

    (proclaim-function decode-float (t) (values t t t))
    (proclaim-function scale-float (t t) t)
    (proclaim-function float-radix (t) fixnum)
    (proclaim-function float-sign (t &optional t) t)
    (proclaim-function float-digits (t) fixnum)
    (proclaim-function float-precision (t) fixnum)
    (proclaim-function integer-decode-float (t) (values t t t))
    (proclaim-function complex (t &optional t) t)
    (proclaim-function realpart (t) t)
    (proclaim-function imagpart (t) t)
    (proclaim-function = (t &rest t) t :predicate t :no-side-effects t)
    (def-inline = :always (t t) :bool "mkcl_number_equalp(env, #0,#1)")
    (def-inline = :always (fixnum-float fixnum-float) :bool "((#0)==(#1))")

    (proclaim-function /= (t &rest t) t :predicate t :no-side-effects t)
    (def-inline /= :always (t t) :bool "!mkcl_number_equalp(env, #0,#1)")
    (def-inline /= :always (fixnum-float fixnum-float) :bool "((#0)!=(#1))")

    (proclaim-function < (t &rest t) t :predicate t :no-side-effects t)
    (def-inline < :always (t t) :bool "(mkcl_number_compare(env, #0,#1)<0)")
    (def-inline < :always (fixnum-float fixnum-float) :bool "((#0)<(#1))")

    (proclaim-function > (t &rest t) t :predicate t :no-side-effects t)
    (def-inline > :always (t t) :bool "(mkcl_number_compare(env, #0,#1)>0)")
    (def-inline > :always (fixnum-float fixnum-float) :bool "((#0)>(#1))")

    (proclaim-function <= (t &rest t) t :predicate t :no-side-effects t)
    (def-inline <= :always (t t) :bool "(mkcl_number_compare(env, #0,#1)<=0)")
    (def-inline <= :always (fixnum-float fixnum-float) :bool "((#0)<=(#1))")

    (proclaim-function >= (t &rest t) t :predicate t :no-side-effects t)
    (def-inline >= :always (t t) :bool "(mkcl_number_compare(env, #0,#1)>=0)")
    (def-inline >= :always (fixnum-float fixnum-float) :bool "((#0)>=(#1))")

    (proclaim-function max (t &rest t) t :no-side-effects t)
    (def-inline max :always (t t) t "@01;(mkcl_number_compare(env, #0,#1)>=0?(#0):(#1))")
    (def-inline max :always (fixnum fixnum) t "@01;MKCL_MAKE_FIXNUM((#0)>=(#1)?(#0):(#1))")
    (def-inline max :always (fixnum fixnum) :fixnum "@01;((#0)>=(#1)?(#0):(#1))")

    (proclaim-function min (t &rest t) t :no-side-effects t)
    (def-inline min :always (t t) t "@01;(mkcl_number_compare(env, #0,#1)<=0?(#0):(#1))")
    (def-inline min :always (fixnum fixnum) t "@01;MKCL_MAKE_FIXNUM((#0)<=(#1)?(#0):(#1))")
    (def-inline min :always (fixnum fixnum) :fixnum "@01;((#0)<=(#1)?(#0):(#1))")

    ;; file num_log.d

    (proclaim-function logand (&rest t) integer :no-side-effects t)
    (def-inline logand :always nil t "MKCL_MAKE_FIXNUM(-1)")
    (def-inline logand :always nil :fixnum "(-1)")
    (def-inline logand :always (t t) t "mkcl_boole(env, MKCL_BOOLAND,(#0),(#1))")
    (def-inline logand :always (fixnum fixnum) t "MKCL_MAKE_FIXNUM((#0) & (#1))")
    (def-inline logand :always (fixnum fixnum) :fixnum "((#0) & (#1))")

    (proclaim-function logandc1 (integer integer) integer :no-side-effects t)
    (def-inline logandc1 :always (t t) t "mkcl_boole(env, MKCL_BOOLANDC1,(#0),(#1))")
    (def-inline logandc1 :always (fixnum fixnum) t "MKCL_MAKE_FIXNUM(~(#0) & (#1))")
    (def-inline logandc1 :always (fixnum fixnum) :fixnum "(~(#0) & (#1))")

    (proclaim-function logandc2 (integer integer) integer :no-side-effects t)
    (def-inline logandc2 :always (t t) t "mkcl_boole(env, MKCL_BOOLANDC2,(#0),(#1))")
    (def-inline logandc2 :always (fixnum fixnum) t "MKCL_MAKE_FIXNUM((#0) & ~(#1))")
    (def-inline logandc2 :always (fixnum fixnum) :fixnum "((#0) & ~(#1))")

    (proclaim-function logeqv (&rest t) integer :no-side-effects t)
    (def-inline logeqv :always nil t "MKCL_MAKE_FIXNUM(-1)")
    (def-inline logeqv :always nil :fixnum "(-1)")
    (def-inline logeqv :always (t t) t "mkcl_boole(env, MKCL_BOOLEQV,(#0),(#1))")
    (def-inline logeqv :always (fixnum fixnum) t "MKCL_MAKE_FIXNUM(~( (#0) ^ (#1) ))")
    (def-inline logeqv :always (fixnum fixnum) :fixnum "(~( (#0) ^ (#1) ))")

    (proclaim-function logior (&rest t) integer :no-side-effects t)
    (def-inline logior :always nil t "MKCL_MAKE_FIXNUM(0)")
    (def-inline logior :always nil :fixnum "0")
    (def-inline logior :always (t t) t "mkcl_boole(env, MKCL_BOOLIOR,(#0),(#1))")
    (def-inline logior :always (fixnum fixnum) t "MKCL_MAKE_FIXNUM((#0) | (#1))")
    (def-inline logior :always (fixnum fixnum) :fixnum "((#0) | (#1))")

    (proclaim-function lognand (integer integer) integer :no-side-effects t)
    (def-inline lognand :always (t t) t "mkcl_boole(env, MKCL_BOOLNAND,(#0),(#1))")
    (def-inline lognand :always (fixnum fixnum) t "MKCL_MAKE_FIXNUM(~( (#0) & (#1) ))")
    (def-inline lognand :always (fixnum fixnum) :fixnum "(~( (#0) & (#1) ))")

    (proclaim-function lognor (integer integer) integer :no-side-effects t)
    (def-inline lognor :always (t t) t "mkcl_boole(env, MKCL_BOOLNOR,(#0),(#1))")
    (def-inline lognor :always (fixnum fixnum) t "MKCL_MAKE_FIXNUM(~( (#0) | (#1) ))")
    (def-inline lognor :always (fixnum fixnum) :fixnum "(~( (#0) | (#1) ))")

    (proclaim-function lognot (integer) integer :no-side-effects t)
    (def-inline lognot :always (t) t "mkcl_boole(env, MKCL_BOOLXOR,(#0),MKCL_MAKE_FIXNUM(-1))")
    (def-inline lognot :always (fixnum) t "MKCL_MAKE_FIXNUM(~(#0))")
    (def-inline lognot :always (fixnum) :fixnum "(~(#0))")

    (proclaim-function logorc1 (integer integer) integer :no-side-effects t)
    (def-inline logorc1 :always (t t) t "mkcl_boole(env, MKCL_BOOLORC1,(#0),(#1))")
    (def-inline logorc1 :always (fixnum fixnum) t "MKCL_MAKE_FIXNUM(~(#0) | (#1))")
    (def-inline logorc1 :always (fixnum fixnum) :fixnum "(~(#0) | (#1))")

    (proclaim-function logorc2 (integer integer) integer :no-side-effects t)
    (def-inline logorc2 :always (t t) t "mkcl_boole(env, MKCL_BOOLORC2,(#0),(#1))")
    (def-inline logorc2 :always (fixnum fixnum) t "MKCL_MAKE_FIXNUM((#0) | ~(#1))")
    (def-inline logorc2 :always (fixnum fixnum) :fixnum "((#0) | ~(#1))")

    (proclaim-function logxor (&rest t) integer :no-side-effects t)
    (def-inline logxor :always nil t "MKCL_MAKE_FIXNUM(0)")
    (def-inline logxor :always nil :fixnum "0")
    (def-inline logxor :always (t t) t "mkcl_boole(env, MKCL_BOOLXOR,(#0),(#1))")
    (def-inline logxor :always (fixnum fixnum) t "MKCL_MAKE_FIXNUM((#0) ^ (#1))")
    (def-inline logxor :always (fixnum fixnum) :fixnum "((#0) ^ (#1))")

    (proclaim-function boole (t t t) t :no-side-effects t)
    (def-inline boole :always (fixnum t t) t "mkcl_boole(env, (#0),(#1),(#2))")

    (proclaim-function logbitp (t t) t :predicate t :no-side-effects t)
#|
    ;; These here do not work right now because type filtering of expressions promotes every
    ;; integral value to type FIXNUM therefore loosing the accuracy needed in these cases. JCB
    #+(or x86-64 aarch64)
    (def-inline logbitp :always ((integer -61 61) fixnum) :bool "(((#1) >> (#0)) & 1)")
    #-(or x86-64 aarch64)
    (def-inline logbitp :always ((integer -29 29) fixnum) :bool "(((#1) >> (#0)) & 1)")
|#

    (proclaim-function ash (integer integer) t)
    (def-inline ash :always (fixnum fixnum) :fixnum
      "@01;(((#1)>0) ? (((mkcl_word)(#0))<<(#1)) : (((mkcl_word)(#0))>>(-(#1))))" :exact-return-type t)

    (proclaim-function logcount (t) t)
    (proclaim-function integer-length (integer) fixnum)
    (def-inline integer-length :always (integer) t "MKCL_MAKE_FIXNUM(mkcl_integer_length(env, #0))")
    (def-inline integer-length :always (integer) :fixnum "mkcl_integer_length(env, #0)")

    (proclaim-function si:bit-array-op (t t t t) t)
    (proclaim-function zerop (t) t :predicate t :no-side-effects t)
    (def-inline zerop :always (t) :bool "mkcl_zerop(env, #0)")
    (def-inline zerop :always (fixnum-float) :bool "((#0)==0)")

    (proclaim-function plusp (t) t :predicate t :no-side-effects t)
    (def-inline plusp :always (t) :bool "mkcl_plusp(env, #0)")
    (def-inline plusp :always (fixnum-float) :bool "((#0)>0)")

    (proclaim-function minusp (t) t :predicate t :no-side-effects t)
    (def-inline minusp :always (t) :bool "mkcl_minusp(env, #0)")
    (def-inline minusp :always (fixnum-float) :bool "((#0)<0)")

    (proclaim-function oddp (t) t :predicate t :no-side-effects t)
    (def-inline oddp :always (t) :bool "mkcl_oddp(env, #0)")
    (def-inline oddp :always (fixnum fixnum) :bool "((#0) & 1)")

    (proclaim-function evenp (t) t :predicate t :no-side-effects t)
    (def-inline evenp :always (t) :bool "mkcl_evenp(env, #0)")
    (def-inline evenp :always (fixnum fixnum) :bool "(~(#0) & 1)")

    (proclaim-function random (t &optional t) t)
    (proclaim-function make-random-state (&optional t) t)
    (proclaim-function random-state-p (t) t :predicate t)

    (proclaim-function abs (number) real :no-side-effects t)
    #+long-float
    (def-inline abs :always (long-float) :long-double "@0;(((#0)>=0.0L) ? (#0) : -(#0))")
    (def-inline abs :always (double-float) :double "@0;(((#0)>=0.0) ? (#0) : -(#0))")
    (def-inline abs :always (single-float) :float "@0;(((#0)>=0.0F) ? (#0) : -(#0))")
    (def-inline abs :always (fixnum) t "@0;(((#0)>=0) ? MKCL_MAKE_FIXNUM(#0) : mkcl_make_integer(env, -(#0)))")
    (def-inline abs :always (fixnum) :fixnum "@0;(((#0)>=0) ? (#0) : -(#0))" :exact-return-type t)

    (proclaim-function expt (number number) number :no-side-effects t)
#|
    ;; These here do not work right now because type filtering of expressions promotes every
    ;; integral value to type FIXNUM therefore loosing the accuracy needed in these cases. JCB
    (def-inline expt :always ((integer 2 2) (integer 0 61)) t "MKCL_MAKE_FIXNUM(1<<(#1))")
    #+(or x86-64 aarch64)
    (def-inline expt :always ((integer 2 2) (integer 0 61)) :fixnum "(1<<(#1))")
    #-(or x86-64 aarch64)
    (def-inline expt :always ((integer 2 2) (integer 0 29)) :fixnum "(1<<(#1))")
    (def-inline expt :always ((integer 0 0) t) :fixnum "0")
    (def-inline expt :always ((integer 1 1) t) :fixnum "1")
|#

    (proclaim-function exp (number) number :no-side-effects t)
    #+long-float
    (def-inline exp :always (long-float) :long-double "expl(#0)")
    (def-inline exp :always (double-float) :double "exp(#0)")
    (def-inline exp :always (single-float) :float "expf(#0)")
    (def-inline exp :always (fixnum) :float "expf((float)(#0))")

    (proclaim-function log (number &optional t) number :no-side-effects t)
    #+long-float
    (def-inline log :always ((long-float 0.0 *)) :long-double "logl(#0)")
    (def-inline log :always ((double-float 0.0 *)) :double "log(#0)")
    (def-inline log :always ((single-float 0.0 *)) :float "logf(#0)")
    (def-inline log :always (si:index) :float "logf((float)(#0))")

    (proclaim-function sqrt (number) number :no-side-effects t)
    #+long-float
    (def-inline sqrt :always ((long-float 0.0 *)) :long-double "sqrtl(#0)")
    (def-inline sqrt :always ((double-float 0.0 *)) :double "sqrt(#0)")
    (def-inline sqrt :always ((single-float 0.0 *)) :float "sqrtf(#0)")
    (def-inline sqrt :always (si:index) :float "sqrtf((float)(#0))")

    (proclaim-function sin (number) number :no-side-effects t)
    #+long-float
    (def-inline sin :always (long-float) :long-double "sinl(#0)")
    (def-inline sin :always (double-float) :double "sin(#0)")
    (def-inline sin :always (single-float) :float "sinf(#0)")
    (def-inline sin :always (fixnum) :float "sinf((float)(#0))")

    (proclaim-function cos (number) number :no-side-effects t)
    #+long-float
    (def-inline cos :always (long-float) :long-double "cosl(#0)")
    (def-inline cos :always (double-float) :double "cos(#0)")
    (def-inline cos :always (single-float) :float "cosf(#0)")
    (def-inline cos :always (fixnum) :float "cosf(#0)")

    (proclaim-function tan (number) number :no-side-effects t)
    #+long-float
    (def-inline tan :always (long-float) :long-double "tanl(#0)")
    (def-inline tan :always (double-float) :double "tan(#0)")
    (def-inline tan :always (single-float) :float "tanf(#0)")
    (def-inline tan :always (fixnum) :float "tanf(#0)")


    ;; file package.d

    (proclaim-function make-package (t &key (nicknames t) (use t) (internal-size t) (external-size t)) t)
    (proclaim-function si:select-package (t) t)
    (proclaim-function find-package (t) t)
    (proclaim-function package-name (t) t)
    (proclaim-function package-nicknames (t) t)
    (proclaim-function rename-package (t t &optional t) t)
    (proclaim-function package-use-list (t) t)
    (proclaim-function package-used-by-list (t) t)
    (proclaim-function package-shadowing-symbols (t) t)
    (proclaim-function list-all-packages () t)
    (proclaim-function intern (t &optional t) (values t t))
    (proclaim-function find-symbol (t &optional t) (values t t))
    (proclaim-function unintern (t &optional t) t)
    (proclaim-function export (t &optional t) t)
    (proclaim-function unexport (t &optional t) t)
    (proclaim-function import (t &optional t) t)
    (proclaim-function shadowing-import (t &optional t) t)
    (proclaim-function shadow (t &optional t) t)
    (proclaim-function use-package (t &optional t) t)
    (proclaim-function unuse-package (t &optional t) t)

    ;; file pathname.d

    (proclaim-function pathname (t) t)
    (proclaim-function parse-namestring (t &optional t t &key (start t) (end t) (junk-allowed t)) (values t t))
    (proclaim-function merge-pathnames (t &optional t t) t)
    (proclaim-function make-pathname (&key (host t) (device t) (directory t) (name t) (type t) (version t) (defaults t) (case t)) t)
    (proclaim-function pathnamep (t) t :predicate t)
    (proclaim-function pathname-host (t &key (case t)) t)
    (proclaim-function pathname-device (t &key (case t)) t)
    (proclaim-function pathname-directory (t &key (case t)) t)
    (proclaim-function pathname-name (t &key (case t)) t)
    (proclaim-function pathname-type (t &key (case t)) t)
    (proclaim-function pathname-version (t) t)
    (proclaim-function wild-pathname-p (t &optional t) t)
    (proclaim-function namestring (t) (or string null))
    (proclaim-function file-namestring (t) (or string null))
    (proclaim-function directory-namestring (t) (or string null))
    (proclaim-function host-namestring (t) (or string null))
    (proclaim-function enough-namestring (t &optional t) (or string null))

    (proclaim-function null (t) t :predicate t :no-side-effects t)
    (def-inline null :always (t) :bool "((#0)==mk_cl_Cnil)")

    (proclaim-function symbolp (t) t :predicate t :no-side-effects t)
    (def-inline symbolp :always (t) :bool "MKCL_SYMBOLP(#0)")

    (proclaim-function atom (t) t :predicate t :no-side-effects t)
    (def-inline atom :always (t) :bool "MKCL_ATOM(#0)")

    (proclaim-function consp (t) t :predicate t :no-side-effects t)
    (def-inline consp :always (t) :bool "@0;MKCL_CONSP(#0)")

    (proclaim-function listp (t) t :predicate t :no-side-effects t)
    (def-inline listp :always (t) :bool "@0;MKCL_LISTP(#0)")

    (proclaim-function numberp (t) t :predicate t :no-side-effects t)
    (def-inline numberp :always (t) :bool "mkcl_numberp(env, #0)")

    (proclaim-function integerp (t) t :predicate t :no-side-effects t)
    (def-inline integerp :always (t) :bool
      "@0;(mkcl_type_of(#0)==mkcl_t_fixnum||mkcl_type_of(#0)==mkcl_t_bignum)")

    (proclaim-function rationalp (t) t :predicate t)
    (def-inline rationalp :always (t) :bool "mkcl_rationalp(env, #0)")

    (proclaim-function floatp (t) t :predicate t :no-side-effects t)
    (def-inline floatp :always (t) :bool "mkcl_floatp(env, #0)")

    (proclaim-function realp (t) t :predicate t :no-side-effects t)
    (def-inline realp :always (t) :bool "mkcl_realp(env, #0)")

    (proclaim-function complexp (t) t :predicate t :no-side-effects t)
    (def-inline complexp :always (t) :bool "@0;MKCL_COMPLEXP(#0)")

    (proclaim-function characterp (t) t :predicate t :no-side-effects t)
    (def-inline characterp :always (t) :bool "MKCL_CHARACTERP(#0)")

    (proclaim-function mkcl:base-char-p (t) t :predicate t :no-side-effects t)
    (def-inline mkcl:base-char-p :always (t) :bool "@0;(MKCL_CHARACTERP(#0) && MKCL_BASE_CHAR_P(#0))")
    (def-inline mkcl:base-char-p :always (character) :bool "MKCL_BASE_CHAR_CODE_P(#0)")

    (proclaim-function stringp (t) t :predicate t :no-side-effects t)
    (def-inline stringp :always (t) :bool "mkcl_stringp(env, #0)")

    (proclaim-function si:base-string-p (t) t :predicate t :no-side-effects t)
    (def-inline si:base-string-p :always (t) :bool "(mkcl_type_of(#0)==mkcl_t_base_string)")

    (proclaim-function bit-vector-p (t) t :predicate t :no-side-effects t)
    (def-inline bit-vector-p :always (t) :bool "(mkcl_type_of(#0)==mkcl_t_bitvector)")

    (proclaim-function vector (&rest t) t)

    (proclaim-function vectorp (t) t :predicate t :no-side-effects t)
    (def-inline vectorp :always (t) :bool "@0;MKCL_VECTORP(#0)")

    (proclaim-function vector-push (t vector) (or fixnum null) :no-sp-change t)
    (def-inline vector-push :always (t vector) t "mkcl_vector_push(env, #1, #0)")
    #+unicode
    (def-inline vector-push :always (character string) t "mkcl_string_push(env, #1, #0)")
    (def-inline vector-push :always (base-char base-string) t "mkcl_base_string_push(env, #1, #0)")

    (proclaim-function vector-push-extend (t vector &optional t) fixnum :no-sp-change t)
    (def-inline vector-push-extend :always (t vector) t "MKCL_MAKE_FIXNUM(mkcl_vector_push_extend(env, #1, #0))")
    (def-inline vector-push-extend :always (t vector) :fixnum "mkcl_vector_push_extend(env, #1, #0)")
    #+unicode
    (def-inline vector-push-extend :always (character string) t "MKCL_MAKE_FIXNUM(mkcl_string_push_extend(env, #1, #0))")
    #+unicode
    (def-inline vector-push-extend :always (character string) :fixnum "mkcl_string_push_extend(env, #1, #0)")
    (def-inline vector-push-extend :always (base-char base-string) t "MKCL_MAKE_FIXNUM(mkcl_base_string_push_extend(env, #1, #0))")
    (def-inline vector-push-extend :always (base-char base-string) :fixnum "mkcl_base_string_push_extend(env, #1, #0)")

    (proclaim-function simple-string-p (t) t :predicate t)
    (def-inline simple-string-p :always (t) :bool "mkcl_simple_string_p(env, #0)")

    (proclaim-function simple-bit-vector-p (t) t :predicate t)
    (def-inline simple-bit-vector-p :always (t) :bool "mkcl_simple_bit_vector_p(env, #0)")

    (proclaim-function simple-vector-p (t) t :predicate t)
    (def-inline simple-vector-p :always (t) :bool "mkcl_simple_vector_p(env, #0)")

    (proclaim-function arrayp (t) t :predicate t :no-side-effects t)
    (def-inline arrayp :always (t) :bool "@0;MKCL_ARRAYP(#0)")

    (proclaim-function packagep (t) t :predicate t)
    (def-inline packagep :always (t) :bool "(mkcl_type_of(#0) == mkcl_t_package)")

    (proclaim-function functionp (t) t :predicate t)
    (def-inline functionp :always (t) :bool "mkcl_functionp(env, #0)")

    (proclaim-function compiled-function-p (t) t :predicate t)
    (def-inline compiled-function-p :always (t) :bool "mkcl_compiled_function_p(env, #0)")

    (proclaim-function si:bytecodep (t) t :predicate t)
    (def-inline si:bytecodep :always (t) :bool "mkcl_bytecodep(env, #0)")

    (proclaim-function si:foreignp (t) t :predicate t)
    (def-inline si:foreignp :always (t) :bool "mkcl_foreignp(env, #0)")

    (proclaim-function eq (t t) t :predicate t :no-side-effects t)
    (def-inline eq :always (t t) :bool "((#0)==(#1)) /*2*/")
    (def-inline eq :always (fixnum fixnum) :bool "((#0)==(#1)) /*4*/")

    (proclaim-function eql (t t) t :predicate t :no-side-effects t)
    (def-inline eql :always (t t) :bool "mkcl_eql(env, #0,#1)")
    (def-inline eql :always ((not (or character number)) t) :bool "((#0)==(#1))")
    (def-inline eql :always (t (not (or character number))) :bool "((#0)==(#1))")
    (def-inline eql :always (character character) :bool "(#0)==(#1)")
    (def-inline eql :always (fixnum fixnum) :bool "((#0)==(#1))")

    (proclaim-function equal (t t) t :predicate t :no-side-effects t)
    (def-inline equal :always (t t) :bool "mkcl_equal(env, #0,#1)")
    (def-inline equal :always (fixnum fixnum) :bool "((#0)==(#1))")

    (proclaim-function equalp (t t) t :predicate t :no-side-effects t)
    (def-inline equalp :always (t t) :bool "mkcl_equalp(env, #0,#1)")
    (def-inline equalp :always (fixnum fixnum) :bool "((#0)==(#1))")

    (proclaim-function not (t) t :predicate t :no-side-effects t)
    (def-inline not :always (t) :bool "((#0)==mk_cl_Cnil)")

    ;; file print.d, read.d

    (proclaim-function clear-output (&optional t) NULL)
    (def-inline clear-output :always (stream) NULL "(mkcl_clear_output(env, #0),mk_cl_Cnil)")

    (proclaim-function finish-output (&optional t) NULL)
    (def-inline finish-output :always (stream) NULL "(mkcl_finish_output(env, #0),mk_cl_Cnil)")

    (proclaim-function force-output (&optional t) NULL)
    (def-inline force-output :always (stream) NULL "(mkcl_force_output(env, #0),mk_cl_Cnil)")

    (proclaim-function fresh-line (&optional t) t)
    (proclaim-function listen (&optional t) t)
    (proclaim-function peek-char (&optional t t t t t) t)
    (proclaim-function pprint (t &optional t) (values))
    (proclaim-function prin1 (t &optional t) t)
    (def-inline prin1 :always (t t) t "mkcl_prin1(env, #0,#1)")
    (def-inline prin1 :always (t) t "mkcl_prin1(env, #0,mk_cl_Cnil)")

    (proclaim-function princ (t &optional t) t)
    (def-inline princ :always (t t) t "mkcl_princ(env, #0,#1)")
    (def-inline princ :always (t) t "mkcl_princ(env, #0,mk_cl_Cnil)")

    (proclaim-function print (t &optional t) t)
    (def-inline print :always (t t) t "mkcl_print(env, #0,#1)")
    (def-inline print :always (t) t "mkcl_print(env, #0,mk_cl_Cnil)")

    (proclaim-function unread-char (t &optional t) t)
    (proclaim-function read (&optional t t t t) t)
    (proclaim-function read-char (&optional t t t t) t)
    (proclaim-function read-delimited-list (t &optional t t) t)
    (proclaim-function read-line (&optional t t t t) (values t t))
    (proclaim-function read-preserving-whitespace (&optional t t t t) t)
    (proclaim-function terpri (&optional t) null)
    (def-inline terpri :always (t) t "mkcl_terpri(env, #0)")
    (def-inline terpri :always nil t "mkcl_terpri(env, mk_cl_Cnil)")

    (proclaim-function write (t &key (stream t) (array t) (base t) (case t) (circle t) (escape t) (gensym t) (length t) (level t) (lines t) (miser-width t) (pprint-dispatch t) (pretty t) (radix t) (readably t) (right-margin t)) t)
    (proclaim-function write-byte (fixnum stream) t)
    (proclaim-function write-char (t &optional t) t)
    (def-inline write-char :always (t) t "@0;(mkcl_princ_char(env, mkcl_char_code(env, #0),mk_cl_Cnil),(#0))")

    (proclaim-function write-line (t &optional t &key (start t) (end t)) t)
    (proclaim-function write-string (t &optional t &key (start t) (end t)) t)
    (proclaim-function read-char-no-hang (&optional t t t t) t)
    (proclaim-function clear-input (&optional t) NULL)
    (def-inline clear-input :always (stream) NULL "(mkcl_clear_input(env, #0),mk_cl_Cnil)")

    (proclaim-function parse-integer (t &key (start t) (end t) (radix t) (junk-allowed t)) t)
    (proclaim-function read-byte (t &optional t t) t)
    (proclaim-function copy-readtable (&optional t t) t :no-side-effects t)
    (def-inline copy-readtable :always (null null) t "standard_readtable")

    (proclaim-function readtablep (t) t :predicate t)
    (proclaim-function set-syntax-from-char (t t &optional t t) t)
    (proclaim-function set-macro-character (t t &optional t t) t)
    (proclaim-function get-macro-character (t &optional t) t)
    (proclaim-function make-dispatch-macro-character (t &optional t t) t)
    (proclaim-function set-dispatch-macro-character (t t t &optional t) t)
    (proclaim-function get-dispatch-macro-character (t t &optional t) t)
    (proclaim-function si:fast-read-from-base-string (t) t)
    (proclaim-function si:standard-readtable () t)
    (proclaim-function symbol-function (t) t)
    (proclaim-function fboundp (t) t :predicate t)
    (proclaim-function symbol-value (symbol) t)
    (proclaim-function boundp (symbol) t :predicate t :no-side-effects t)
    (def-inline boundp :always (symbol) :bool
      "@0;(mkcl_likely(MKCL_SYMBOLP(#0)) ? (MKCL_SYM_VAL(env,#0)!=MKCL_OBJNULL) : (mkcl_FEtype_error_symbol(env, #0), FALSE))")

    (proclaim-function macro-function (t &optional t) t)
    (proclaim-function special-operator-p (t) t :predicate t)

    ;; file unixsys.d

    ;; file sequence.d

    (proclaim-function elt (sequence fixnum) t :no-side-effects t)
    (def-inline elt :always (t t) t "mkcl_elt(env, #0, mkcl_safe_fixnum_to_word(env, #1))")
    (def-inline elt :always (t fixnum) t "mkcl_elt(env, #0,#1)")
    (def-inline elt :always (list t) t "mkcl_nth(env, mkcl_safe_fixnum_to_word(env, #1), #0)")
    (def-inline elt :always (list fixnum) t "mkcl_nth(env, #1, #0)")
    (def-inline elt :unsafe (t t) t "mkcl_elt(env, #0,mkcl_fixnum_to_word(#1))")
    (def-inline elt :unsafe (t fixnum) t "mkcl_elt(env, #0,#1)")
    #+unicode
    (def-inline elt :always (string fixnum) t "mkcl_character_index(env, #0, #1)")
    (def-inline elt :always (base-string fixnum) t "mkcl_base_char_index(env, #0, #1)")
    #+unicode
    (def-inline elt :always (string fixnum) :wchar "mkcl_character_index_raw(env, #0, #1)")
    (def-inline elt :always (base-string fixnum) :char "mkcl_base_char_index_raw(env, #0, #1)")
    #+unicode
    (def-inline elt :unsafe (mkcl:character-string fixnum) :wchar "((#0)->string.self[#1])")
    (def-inline elt :unsafe (base-string fixnum) :char "((#0)->base_string.self[#1])")

    (proclaim-function si:elt-set (sequence fixnum t) t)
    (def-inline si:elt-set :always (t t t) t "mkcl_elt_set(env, #0,mkcl_safe_fixnum_to_word(env, #1),#2)")
    (def-inline si:elt-set :always (t fixnum t) t "mkcl_elt_set(env, #0,#1,#2)")
    (def-inline si:elt-set :unsafe (t t t) t "mkcl_elt_set(env, #0,mkcl_fixnum_to_word(#1),#2)")

    (proclaim-function subseq (sequence fixnum &optional t) sequence)
    (proclaim-function copy-seq (sequence) sequence)
    (proclaim-function length (sequence) fixnum :no-side-effects t)
    (def-inline length :always (t) t "mkcl_make_unsigned_integer(env, mkcl_length(env, #0))")
    #+unicode
    (def-inline length :always (string) t "mkcl_make_unsigned_integer(env, mkcl_string_length(env, #0))")
    (def-inline length :always (base-string) t "mkcl_make_unsigned_integer(env, mkcl_base_string_length(env, #0))")
    (def-inline length :unsafe (vector) t "MKCL_MAKE_FIXNUM((#0)->vector.fillp)")
    (def-inline length :unsafe (vector) :fixnum "(#0)->vector.fillp")

    (proclaim-function reverse (sequence) sequence)
    (proclaim-function nreverse (sequence) sequence)

    (proclaim-function fill (sequence t &key (start t) (end t)) sequence)
    #+unicode
    (def-inline fill :always (string character) t "mkcl_fill_string(env, #0, #1)")
    (def-inline fill :always (base-string base-char) t "mkcl_fill_base_string(env, #0, #1)")

    (proclaim-function replace (sequence sequence &key (start1 t) (end1 t) (start2 t) (end2 t)) sequence)
    #+unicode
    (def-inline replace :always (string string) t "mkcl_replace_in_string(env, #0, #1)")
    (def-inline replace :always (base-string base-string) t "mkcl_replace_in_base_string(env, #0, #1)")

    (proclaim-function search (sequence sequence &key (from-end t) (test t) (test-not t) (key t) (start1 t) (end1 t) (start2 t) (end2 t)) t)
    (def-inline search :always (base-string base-string) t "mkcl_search_in_base_string(env, #0, #1)")

    (proclaim-function map (t t t &rest t) t)
    (proclaim-function map-into (t t &rest t) t)

    ;; file character.d

    (proclaim-function char (string fixnum) character :no-side-effects t)
    (def-inline char :always (t t) t "MKCL_CODE_CHAR(mkcl_char(env, #0,  mkcl_safe_fixnum_to_word(env, #1)))")
    (def-inline char :always (t fixnum) t "MKCL_CODE_CHAR(mkcl_char(env, #0,#1))")
    (def-inline char :always (t t) :char "mkcl_char(env, #0, mkcl_safe_fixnum_to_word(env, #1))")
    (def-inline char :always (t fixnum) :char "mkcl_char(env, #0,#1) /* (char t fixnum) */")
    #+unicode  ;; For identical args signature, largest return type must be last.
    (def-inline char :always (t t) :wchar "mkcl_char(env, #0, mkcl_safe_fixnum_to_word(env, #1))")
    #+unicode  ;; For identical args signature, largest return type must be last.
    (def-inline char :always (t fixnum) :wchar "mkcl_char(env, #0,#1)")
    #+unicode
    (def-inline char :always (string t) t "mkcl_character_index(env, #0, mkcl_safe_fixnum_to_word(env, #1))")
    #+unicode
    (def-inline char :always (string fixnum) t "mkcl_character_index(env, #0, #1)")
    (def-inline char :always (base-string t) t "mkcl_base_char_index(env, #0, mkcl_safe_fixnum_to_word(env, #1))")
    (def-inline char :always (base-string fixnum) t "mkcl_base_char_index(env, #0, #1)")
    #+unicode
    (def-inline char :always (string t) :wchar "mkcl_character_index_raw(env, #0, mkcl_safe_fixnum_to_word(env, #1))")
    #+unicode
    (def-inline char :always (string fixnum) :wchar "mkcl_character_index_raw(env, #0, #1)")
    (def-inline char :always (base-string t) :char "mkcl_base_char_index_raw(env, #0, mkcl_safe_fixnum_to_word(env, #1))")
    (def-inline char :always (base-string fixnum) :char "mkcl_base_char_index_raw(env, #0, #1)")
    #+unicode
    (def-inline char :unsafe (mkcl:character-string t) :wchar "((#0)->string.self[mkcl_fixnum_to_word(#1)])")
    #+unicode
    (def-inline char :unsafe (mkcl:character-string fixnum) :wchar "((#0)->string.self[#1])")
    (def-inline char :unsafe (base-string t) :char "((#0)->base_string.self[mkcl_fixnum_to_word(#1)])")
    (def-inline char :unsafe (base-string fixnum) :char "((#0)->base_string.self[#1])")

    (proclaim-function si:char-set (string fixnum character) character)
    (def-inline si:char-set :always (t t character) t "MKCL_CODE_CHAR(mkcl_char_set(env, #0, mkcl_safe_fixnum_to_word(env, #1),#2))")
    (def-inline si:char-set :always (t fixnum character) t "MKCL_CODE_CHAR(mkcl_char_set(env, #0,#1,#2))")
    #+unicode
    (def-inline si:char-set :always (t fixnum character) :wchar "mkcl_char_set(env, #0,#1,#2)")
    (def-inline si:char-set :always (t fixnum base-char) :char "((mkcl_base_char) mkcl_char_set(env, #0,#1,#2))")
    #+unicode
    (def-inline si:char-set :always (string t t) t "mkcl_character_set_index(env, #0, mkcl_safe_fixnum_to_word(env, #1), #2)")
    #+unicode
    (def-inline si:char-set :always (string fixnum t) t "mkcl_character_set_index(env, #0, #1, #2)")
    (def-inline si:char-set :always (base-string t t) t "mkcl_base_char_set_index(env, #0, mkcl_safe_fixnum_to_word(env, #1), #2)")
    (def-inline si:char-set :always (base-string fixnum t) t "mkcl_base_char_set_index(env, #0, #1, #2)")
    #+unicode
    (def-inline si:char-set :always (string fixnum character) :wchar "mkcl_character_set_index_raw(env, #0, #1, #2)")
    (def-inline si:char-set :always (base-string fixnum base-char) :char "mkcl_base_char_set_index_raw(env, #0, #1, #2)")
    #+unicode
    (def-inline si:char-set :unsafe (mkcl:character-string t t) t
      "@2;((#0)->string.self[mkcl_fixnum_to_word(#1)]=mkcl_char_code(env, #2),(#2))")
    #+unicode
    (def-inline si:char-set :unsafe (mkcl:character-string fixnum t) t
      "@2;((#0)->string.self[#1]=mkcl_char_code(env, #2),(#2))")
    (def-inline si:char-set :unsafe (base-string t t) t
      "@2;((#0)->base_string.self[mkcl_fixnum_to_word(#1)]=mkcl_char_code(env, #2),(#2))")
    (def-inline si:char-set :unsafe (base-string fixnum t) t
      "@2;((#0)->base_string.self[#1]=mkcl_char_code(env, #2),(#2))")

    (proclaim-function schar (simple-string fixnum) character :no-side-effects t)
    (def-inline schar :always (t t) t "MKCL_CODE_CHAR(mkcl_char(env, #0, mkcl_safe_fixnum_to_word(env, #1)))")
    (def-inline schar :always (t fixnum) t "MKCL_CODE_CHAR(mkcl_char(env, #0,#1))")
    (def-inline schar :always (t t) :char "mkcl_char(env, #0, mkcl_safe_fixnum_to_word(env, #1)) /* (schar t t) */")
    (def-inline schar :always (t fixnum) :char "mkcl_char(env, #0,#1) /* (schar t fixnum) */")
    #+unicode  ;; For identical args signature, largest return type must be last.
    (def-inline schar :always (t t) :wchar "mkcl_char(env, #0, mkcl_safe_fixnum_to_word(env, #1))")
    #+unicode  ;; For identical args signature, largest return type must be last.
    (def-inline schar :always (t fixnum) :wchar "mkcl_char(env, #0,#1)")
    #+unicode
    (def-inline schar :always (string t) t "mkcl_character_index(env, #0, mkcl_safe_fixnum_to_word(env, #1))")
    #+unicode
    (def-inline schar :always (string fixnum) t "mkcl_character_index(env, #0, #1)")
    (def-inline schar :always (base-string t) t "mkcl_base_char_index(env, #0, mkcl_safe_fixnum_to_word(env, #1))")
    (def-inline schar :always (base-string fixnum) t "mkcl_base_char_index(env, #0, #1)")
    #+unicode
    (def-inline schar :always (string t) :wchar "mkcl_character_index_raw(env, #0, mkcl_safe_fixnum_to_word(env, #1))")
    #+unicode
    (def-inline schar :always (string fixnum) :wchar "mkcl_character_index_raw(env, #0, #1)")
    (def-inline schar :always (base-string t) :char "mkcl_base_char_index_raw(env, #0, mkcl_safe_fixnum_to_word(env, #1))")
    (def-inline schar :always (base-string fixnum) :char "mkcl_base_char_index_raw(env, #0, #1)")
    #+unicode
    (def-inline schar :unsafe (mkcl:character-string fixnum) :wchar "((#0)->string.self[#1])")
    (def-inline schar :unsafe (base-string fixnum) :char "((#0)->base_string.self[#1])")

    (proclaim-function si:schar-set (simple-string fixnum character) character)
    (def-inline si:schar-set :always (t t character) t "MKCL_CODE_CHAR(mkcl_char_set(env, #0, mkcl_safe_fixnum_to_word(env, #1),#2))")
    (def-inline si:schar-set :always (t fixnum character) t "MKCL_CODE_CHAR(mkcl_char_set(env, #0,#1,#2))")
    (def-inline si:schar-set :always (t fixnum character) :char "MKCL_CODE_CHAR(mkcl_char_set(env, #0,#1,#2))")
    #+unicode   ;; For identical args signature, largest return type must be last.
    (def-inline si:schar-set :always (t fixnum character) :wchar "MKCL_CODE_CHAR(mkcl_char_set(env, #0,#1,#2))")
    #+unicode
    (def-inline si:schar-set :always (string t t) t "mkcl_character_set_index(env, #0, mkcl_safe_fixnum_to_word(env, #1), #2)")
    #+unicode
    (def-inline si:schar-set :always (string fixnum t) t "mkcl_character_set_index(env, #0, #1, #2)")
    (def-inline si:schar-set :always (base-string t t) t "mkcl_base_char_set_index(env, #0, mkcl_safe_fixnum_to_word(env, #1), #2)")
    (def-inline si:schar-set :always (base-string fixnum t) t "mkcl_base_char_set_index(env, #0, #1, #2)")
    (def-inline si:schar-set :always (base-string fixnum base-char) :char "mkcl_base_char_set_index_raw(env, #0, #1, #2)")
    #+unicode  ;; For identical args signature, largest return type must be last.
    (def-inline si:schar-set :always (string fixnum character) :wchar "mkcl_character_set_index_raw(env, #0, #1, #2)")
    #+unicode
    (def-inline si:schar-set :unsafe (mkcl:character-string fixnum t) t
      "@2;((#0)->string.self[#1]=mkcl_char_code(env, #2),(#2))")
    #+unicode
    (def-inline si:schar-set :unsafe (mkcl:character-string fixnum t) t
      "@2;((#0)->string.self[mkcl_fixnum_to_word(#1)]=mkcl_char_code(env, #2),(#2))")
    (def-inline si:schar-set :unsafe (base-string fixnum t) t
      "@2;((#0)->base_string.self[#1]=mkcl_char_code(env, #2),(#2))")
    (def-inline si:schar-set :unsafe (base-string fixnum t) t
      "@2;((#0)->base_string.self[mkcl_fixnum_to_word(#1)]=mkcl_char_code(env, #2),(#2))")

    (proclaim-function string= (string-designator string-designator &key (start1 t) (end1 t) (start2 t) (end2 t)) t :predicate t :no-side-effects t)
    (def-inline string= :always (string string) :bool "mkcl_string_E(env, #0,#1)")
    (proclaim-function string/= (string-designator string-designator &key (start1 t) (end1 t) (start2 t) (end2 t)) t :predicate t)

    (proclaim-function string< (string-designator string-designator &key (start1 t) (end1 t) (start2 t) (end2 t)) t :predicate t)
    (proclaim-function string> (string-designator string-designator &key (start1 t) (end1 t) (start2 t) (end2 t)) t :predicate t)
    (proclaim-function string<= (string-designator string-designator &key (start1 t) (end1 t) (start2 t) (end2 t)) t :predicate t)
    (proclaim-function string>= (string-designator string-designator &key (start1 t) (end1 t) (start2 t) (end2 t)) t :predicate t)
    (proclaim-function string-equal (string-designator string-designator &key (start1 t) (end1 t) (start2 t) (end2 t)) t :predicate t :no-side-effects t)
    (proclaim-function string-not-equal (string-designator string-designator &key (start1 t) (end1 t) (start2 t) (end2 t)) t :predicate t)
    (proclaim-function string-lessp (string-designator string-designator &key (start1 t) (end1 t) (start2 t) (end2 t)) t :predicate t)
    (proclaim-function string-greaterp (string-designator string-designator &key (start1 t) (end1 t) (start2 t) (end2 t)) t :predicate t)
    (proclaim-function string-not-lessp (string-designator string-designator &key (start1 t) (end1 t) (start2 t) (end2 t)) t :predicate t)
    (proclaim-function string-not-greaterp (string-designator string-designator &key (start1 t) (end1 t) (start2 t) (end2 t)) t :predicate t)

    (proclaim-function make-string (fixnum &key (initial-element t) (element-type t)) string)
    (proclaim-function string-trim (t string-designator) string)
    (proclaim-function string-left-trim (t string-designator) string)
    (proclaim-function string-right-trim (t string-designator) string)
    (proclaim-function string-upcase (string-designator &key (start t) (end t)) string)
    (proclaim-function string-downcase (string-designator &key (start t) (end t)) string)
    (proclaim-function string-capitalize (string-designator &key (start t) (end t)) string)
    (proclaim-function nstring-upcase (string &key (start t) (end t)) string)
    (proclaim-function nstring-downcase (string &key (start t) (end t)) string)
    (proclaim-function nstring-capitalize (string &key (start t) (end t)) string)
    (proclaim-function string (t) string)
    (proclaim-function si:concatenate-strings (&rest t) string)
    #+unicode
    (def-inline si:concatenate-strings :always (string) t "mkcl_copy_string(env, #0)")
    #+unicode
    (def-inline si:concatenate-strings :always (string string) t "mkcl_concatenate_2_strings(env, #0, #1)")
    #+unicode
    (def-inline si:concatenate-strings :always (string string string) t "mkcl_concatenate_3_strings(env, #0, #1, #2)")

    (proclaim-function si:concatenate-base-strings (&rest t) base-string)
    (def-inline si:concatenate-base-strings :always
      (base-string) t "mkcl_copy_base_string(env, #0)")
    (def-inline si:concatenate-base-strings :always
      (base-string base-string) t "mkcl_concatenate_2_base_strings(env, #0, #1)")
    (def-inline si:concatenate-base-strings :always
      (base-string base-string base-string) t "mkcl_concatenate_3_base_strings(env, #0, #1, #2)")


    ;; file structure.d

    (proclaim-function si:make-structure (t &rest t) t)
    (proclaim-function copy-structure (t) t)
    (proclaim-function si:structure-name (t) symbol :no-side-effects t)
    (def-inline si:structure-name :unsafe (structure) symbol "MKCL_SNAME(#0)")

    (proclaim-function si:structure-ref (t t fixnum) t :no-side-effects t)
    (def-inline si:structure-ref :always (t t fixnum) t "mkcl_structure_ref(env, #0,#1,#2)")

    (proclaim-function si:structure-set (t t fixnum t) t)
    (def-inline si:structure-set :always (t t fixnum t) t "mkcl_structure_set(env, #0,#1,#2,#3)")
    (def-inline si:structure-set :always (t t fixnum t) :void "mkcl_structure_set(env, #0,#1,#2,#3) /* :void */")

    (proclaim-function si:structurep (t) t :predicate t)
    (proclaim-function si:structure-subtype-p (t t) t :predicate t)

    (proclaim-function si:*make-special (t) t)
    (proclaim-function si:*make-constant (t t) t)

    ;; file symbol.d

    (proclaim-function get (symbol t &optional t) t :no-side-effects t)
    (def-inline get :always (t t t) t "mkcl_get(env, #0,#1,#2)")
    (def-inline get :always (t t) t "mkcl_get(env, #0,#1,mk_cl_Cnil)")

    (proclaim-function remprop (symbol t) t)
    (proclaim-function symbol-plist (symbol) t :predicate t :no-side-effects t)

    (proclaim-function getf (t t &optional t) t)
    (proclaim-function get-properties (t t) (values t t t))
    (proclaim-function symbol-name (symbol) string :no-side-effects t)
    (def-inline symbol-name :always (t) string "mkcl_symbol_name(env, #0)")

    (proclaim-function make-symbol (string) symbol)
    (proclaim-function copy-symbol (symbol &optional t) symbol)
    (proclaim-function gensym (&optional t) symbol)
    (proclaim-function gentemp (&optional t t) symbol)
    (proclaim-function symbol-package (symbol) t)
    (proclaim-function keywordp (t) t :predicate t)
    (proclaim-function si:put-f (t t t) t)
    (proclaim-function si:rem-f (t t) (values t t))
    (proclaim-function si:set-symbol-plist (symbol t) t)
    (proclaim-function si:putprop (t t t) t)
    (proclaim-function si:put-sysprop (t t t) t)
    (proclaim-function si:get-sysprop (t t) (values t t))
    (proclaim-function si:rem-sysprop (t t) t)

    ;; file unixtime.d

    (proclaim-function get-universal-time () t)
    (proclaim-function get-decoded-time () *)
    (proclaim-function get-internal-run-time () t)
    (proclaim-function get-internal-real-time () t)
    (proclaim-function sleep (real) null)

    ;; file typeof.d

    (proclaim-function type-of (t) t)
    (proclaim-function typep (t t &optional t) t)

    ;; AKCL addition

    (proclaim-function si:copy-stream (t t) t)

    ;; file num_sfun.lsp

    (proclaim-function cosh (t) t)
    (proclaim-function sinh (t) t)
    (proclaim-function tanh (t) t)

    ;; file numlib.lsp

    (proclaim-function acos (t) t)
    (proclaim-function acosh (t) t)
    (proclaim-function asin (t) t)
    (proclaim-function asinh (t) t)
    (proclaim-function atanh (t) t)

    ;; file seq.lsp

    (proclaim-function concatenate (t &rest t) t)
    (proclaim-function make-sequence (t t &key (initial-element t)) t)
    (proclaim-function si::make-seq-iterator (t &optional t) t :no-sp-change t)
    (proclaim-function si::seq-iterator-ref (sequence t) t :no-sp-change t)
    (proclaim-function si::seq-iterator-set (sequence t t) t :no-sp-change t)
    (proclaim-function si::seq-iterator-next (sequence t) t :no-sp-change t)
    (proclaim-function some (t t &rest t) t)
    (proclaim-function every (t t &rest t) t)
    (proclaim-function notany (t t &rest t) t)
    (proclaim-function notevery (t t &rest t) t)

    ;; Additions used by the compiler.
    ;; The following functions do not exist. They are always expanded into the
    ;; given C code. References to these functions are generated in the C1 phase.

    (proclaim-function shift>> (t t) t :no-side-effects t)
    (def-inline shift>> :always (fixnum fixnum) :fixnum "((#0) >> (- (#1)))")

    (proclaim-function shift<< (t t) t :no-side-effects t)
    (def-inline shift<< :always (fixnum fixnum) :fixnum "((#0) << (#1))")

    (proclaim-function short-float-p (t) t :predicate t :no-side-effects t)
    (def-inline short-float-p :always (t) :bool "mkcl_type_of(#0)==mkcl_t_singlefloat")

    (proclaim-function single-float-p (t) t :predicate t :no-side-effects t)
    (def-inline single-float-p :always (t) :bool "mkcl_type_of(#0)==mkcl_t_singlefloat")

    (proclaim-function double-float-p (t) t :predicate t :no-side-effects t)
    (def-inline double-float-p :always (t) :bool "mkcl_type_of(#0)==mkcl_t_doublefloat")

    (proclaim-function long-float-p (t) t :predicate t :no-side-effects t)
    #-long-float
    (def-inline long-float-p :always (t) :bool "mkcl_type_of(#0)==mkcl_t_doublefloat")
    #+long-float
    (def-inline long-float-p :always (t) :bool "mkcl_type_of(#0)==mkcl_t_longfloat")

    ;; End of compiler faked functions.


    (proclaim-function mkcl:fixnump (t) t :predicate t :no-side-effects t)
    (def-inline mkcl:fixnump :always (t) :bool "MKCL_FIXNUMP(#0)")
    (def-inline mkcl:fixnump :always (fixnum) :bool "TRUE")

    (proclaim-function si:put-properties (t &rest t) t :no-sp-change t)

    (proclaim-function compiler::ldb1 (fixnum fixnum fixnum) fixnum :no-side-effects t)
    (def-inline compiler::ldb1 :always (fixnum fixnum fixnum) t
      "MKCL_MAKE_FIXNUM((((~((mkcl_word)-1 << (#0))) << (#1)) & (mkcl_word)(#2)) >> (#1))")
    (def-inline compiler::ldb1 :always (fixnum fixnum fixnum) :fixnum
      "((((~((mkcl_word)-1 << (#0))) << (#1)) & (mkcl_word)(#2)) >> (#1))")

    ;; Functions only available with CLOS

    (proclaim-function si:allocate-raw-instance (t t fixnum) t)
    (proclaim-function si::instance-sig (t) t)
    (def-inline si::instance-sig :always (standard-object) t "(#0)->instance.sig")
    (proclaim-function si::instance-sig-set (t) t)
    (def-inline si::instance-sig-set :always (standard-object) t "@0;((#0)->instance.sig = MKCL_CLASS_SLOTS(MKCL_CLASS_OF(#0)))")
    (proclaim-function si::instance-sig-set2 (t t) t)
    (def-inline si::instance-sig-set :always (standard-object t) t "((#0)->instance.sig = (#1))")

    (proclaim-function si:instance-ref-safe (t fixnum) t)
    (proclaim-function si:instance-ref (t fixnum) t :no-side-effects t)
    (def-inline si:instance-ref :always (t fixnum) t "mkcl_instance_ref(env, (#0),(#1))")
    (def-inline si:instance-ref :unsafe (standard-object fixnum) t "(#0)->instance.slots[#1]")

    (proclaim-function si:instance-set (t fixnum t) t)
    (def-inline si:instance-set :always (t fixnum t) t "mkcl_instance_set(env, (#0),(#1),(#2))")
    (def-inline si:instance-set :unsafe (standard-object fixnum t) t "(#0)->instance.slots[#1]=(#2)")

    (proclaim-function si:instance-class (t) t :no-side-effects t)
    (def-inline si:instance-class :always (standard-object) t 
      "@0;(mkcl_likely(MKCL_INSTANCEP(#0)) ? MKCL_CLASS_OF(#0) : (mkcl_FEtype_error_instance(env, #0), mk_cl_Cnil))")
    (def-inline si:instance-class :unsafe (standard-object) t "MKCL_CLASS_OF(#0)")
    (proclaim-function si:instance-class-set (t t) t)
    (proclaim-function si:instancep (t) t :predicate t)
    (def-inline si::instancep :always (t) :bool "@0;MKCL_INSTANCEP(#0)")
    (proclaim-function si:unbound () t :predicate t :no-side-effects t)
    (def-inline si:unbound :always nil t "MKCL_UNBOUND")

    (proclaim-function si:sl-boundp (t) t :predicate t :no-side-effects t)
    (def-inline si:sl-boundp :always (t) :bool "(#0)!=MKCL_UNBOUND")

    (proclaim-function si:sl-makunbound (t fixnum) t :predicate t)

#|
    (proclaim-function clos::standard-instance-access (standard-object fixnum) t :no-side-effects t)
    (def-inline clos::standard-instance-access :always (t fixnum) t "mkcl_instance_ref(env, (#0),(#1))")
    (def-inline clos::standard-instance-access :unsafe (standard-object fixnum) t "(#0)->instance.slots[#1]")

    (proclaim-function clos::funcallable-standard-instance-access (funcallable-standard-object fixnum) t :no-side-effects t)
    (def-inline clos::funcallable-standard-instance-access :always (t fixnum) t "mkcl_instance_ref(env, (#0),(#1))")
    (def-inline clos::funcallable-standard-instance-access :unsafe (funcallable-standard-object fixnum) t
      "(#0)->instance.slots[#1]")
|#

    (proclaim-function clos::associate-methods-to-gfun (generic-function &rest t) generic-function)
    (proclaim-function si:of-class-p (t t) t)

    (proclaim-function clos::class-slots (class) list)
    (proclaim-function clos::method-specializers (method) t)

    (proclaim-function si::do-deftype (t t t) t)
    (proclaim-function cl:upgraded-array-element-type (t &optional t) t)
    (proclaim-function cl:upgraded-complex-part-type (t &optional t) t)

    (proclaim-function cl:subtypep (t t &optional t) (values t t))

    (proclaim-function si::search-keyword (t t) t)

    (proclaim-function make-array (t &key (element-type t) (initial-element t) (initial-contents t) (adjustable t) (fill-pointer t) (displaced-to t) (displaced-index-offset t)) t)
    (proclaim-function array-in-bounds-p (t &rest t) t)
    (proclaim-function cl:bit (t &rest t) t)
    (proclaim-function cl:sbit (t &rest t) t)
    (proclaim-function cl:bit-and (t t &optional t) t)
    (proclaim-function cl:bit-ior (t t &optional t) t)
    (proclaim-function cl:bit-xor (t t &optional t) t)
    (proclaim-function cl:bit-eqv (t t &optional t) t)
    (proclaim-function cl:bit-nand (t t &optional t) t)
    (proclaim-function cl:bit-nor (t t &optional t) t)
    (proclaim-function cl:bit-andc1 (t t &optional t) t)
    (proclaim-function cl:bit-andc2 (t t &optional t) t)
    (proclaim-function cl:bit-orc1 (t t &optional t) t)
    (proclaim-function cl:bit-orc2 (t t &optional t) t)
    (proclaim-function cl:bit-not (t &optional t) t)
    (proclaim-function cl:adjust-array (t t &key (element-type t) (initial-element t) (initial-contents t) (fill-pointer t) (displaced-to t) (displaced-index-offset t)) t)

    (proclaim-function cl:read-from-string (t &optional t t &key (start t) (end t) (preserve-whitespace t) (encoding t)) (values t t))
    (proclaim-function cl:write-to-string (t &key (array t) (base t) (case t) (circle t) (escape t) (gensym t) (length t) (level t) (lines t) (miser-width t) (pprint-dispatch t) (pretty t) (radix t) (readably t) (right-margin t)) t)
    (proclaim-function mkcl:write-to-base-string (t &rest t &key (encoding t) &allow-other-keys) t)
    (proclaim-function mkcl:prin1-to-base-string (t &key (encoding t)) t)
    (proclaim-function mkcl:princ-to-base-string (t &key (encoding t)) t)
    (proclaim-function cl:ensure-directories-exist (t &key (verbose t)) (values t t))
    (proclaim-function mkcl:run-program (t t &rest t &key (input t) (if-input-does-not-exist t) (output t) (if-output-exists t) (error t) (if-error-exists t) (directory t) (search t) (wait t) (detached t) (external-format t) (element-type t)) (values t t t))

    (proclaim-function cl:union (t t &key (test t) (test-not t)) t)
    (proclaim-function cl:nunion (t t &key (test t) (test-not t)) t)
    (proclaim-function cl:intersection (t t &key (test t) (test-not t)) t)
    (proclaim-function cl:nintersection (t t &key (test t) (test-not t)) t)
    (proclaim-function cl:set-difference (t t &key (key t) (test t) (test-not t)) t)
    (proclaim-function cl:nset-difference (t t &key (key t) (test t) (test-not t)) t)
    (proclaim-function cl:set-exclusive-or (t t &key (key t) (test t) (test-not t)) t)
    (proclaim-function cl:nset-exclusive-or (t t &key (key t) (test t) (test-not t)) t)
    (proclaim-function cl:subsetp (t t &key (key t) (test t) (test-not t)) t)

    (proclaim-function cl:decode-universal-time (t &optional t) (values t t t t t t t t t))
    (proclaim-function cl:encode-universal-time (t t t t t t &optional t) t)

    (proclaim-function cl:atan (t &optional t) t)

    (proclaim-function cl:apropos (t &optional t) (values))
    (proclaim-function cl:apropos-list (t &optional t) t)

    (proclaim-function si:package-children (t &key (recurse t)) t)

    (proclaim-function cl:reduce (t t &key (key t) (from-end t) (start t) (end t) (initial-value t)) t)
    (proclaim-function cl:remove (t t &key (count t) (key t) (from-end t) (test t) (test-not t) (start t) (end t)) t)
    (proclaim-function cl:remove-if (t t &key (count t) (key t) (from-end t) (start t) (end t)) t)
    (proclaim-function cl:remove-if-not (t t &key (count t) (key t) (from-end t) (start t) (end t)) t)
    (proclaim-function cl:delete (t t &key (count t) (key t) (from-end t) (test t) (test-not t) (start t) (end t)) t)
    (proclaim-function cl:delete-if (t t &key (count t) (key t) (from-end t) (start t) (end t)) t)
    (proclaim-function cl:delete-if-not (t t &key (count t) (key t) (from-end t) (start t) (end t)) t)
    (proclaim-function cl:position (t t &key (key t) (from-end t) (test t) (test-not t) (start t) (end t)) t)
    (proclaim-function cl:position-if (t t &key (key t) (from-end t) (start t) (end t)) t)
    (proclaim-function cl:position-if-not (t t &key (key t) (from-end t) (start t) (end t)) t)
    (proclaim-function cl:count (t t &key (key t) (from-end t) (test t) (test-not t) (start t) (end t)) t)
    (proclaim-function cl:count-if (t t &key (key t) (from-end t) (start t) (end t)) t)
    (proclaim-function cl:count-if-not (t t &key (key t) (from-end t) (start t) (end t)) t)
    (proclaim-function cl:substitute (t t t &key (count t) (key t) (from-end t) (test t) (test-not t) (start t) (end t)) t)
    (proclaim-function cl:substitute-if (t t t &key (count t) (key t) (from-end t) (start t) (end t)) t)
    (proclaim-function cl:substitute-if-not (t t t &key (count t) (key t) (from-end t) (start t) (end t)) t)
    (proclaim-function cl:nsubstitute (t t t &key (count t) (key t) (from-end t) (test t) (test-not t) (start t) (end t)) t)
    (proclaim-function cl:nsubstitute-if (t t t &key (count t) (key t) (from-end t) (start t) (end t)) t)
    (proclaim-function cl:nsubstitute-if-not (t t t &key (count t) (key t) (from-end t) (start t) (end t)) t)
    (proclaim-function cl:find (t t &key (key t) (from-end t) (test t) (test-not t) (start t) (end t)) t)
    (proclaim-function cl:find-if (t t &key (key t) (from-end t) (start t) (end t)) t)
    (proclaim-function cl:find-if-not (t t &key (key t) (from-end t) (start t) (end t)) t)
    (proclaim-function cl:remove-duplicates (t &key (key t) (from-end t) (test t) (test-not t) (start t) (end t)) t)
    (proclaim-function cl:delete-duplicates (t &key (key t) (from-end t) (test t) (test-not t) (start t) (end t)) t)
    (proclaim-function cl:mismatch (t t &key (key t) (from-end t) (test t) (test-not t) (start1 t) (end1 t) (start2 t) (end2 t)) t)
    (proclaim-function cl:sort (t t &key (key t)) t)
    (proclaim-function cl:stable-sort (t t &key (key t)) t)
    (proclaim-function cl:merge (t t t t &key (key t)) t)

    (proclaim-function cl:method-combination-error (t &rest t) t)
    (proclaim-function cl:invalid-method-error (t t &rest t) t)

    (proclaim-function cl:pprint-newline (t &optional t) null)
    (proclaim-function cl:pprint-indent (t t &optional t) null)
    (proclaim-function cl:pprint-tab (t t t &optional t) null)
    (proclaim-function cl:pprint-fill (t t &optional t t) null)
    (proclaim-function cl:pprint-linear (t t &optional t t) null)
    (proclaim-function cl:pprint-tabular (t t &optional t t t) null)
    (proclaim-function cl:copy-pprint-dispatch (&optional t) t)
    (proclaim-function cl:pprint-dispatch (t &optional t) (values t t))
    (proclaim-function cl:set-pprint-dispatch (t t &optional t t) null)

    (proclaim-function cl:continue (&optional t) null)
    (proclaim-function cl:abort (&optional t) (values))
    (proclaim-function cl:muffle-warning (&optional t) (values))
    (proclaim-function cl:store-value (t &optional t) null)
    (proclaim-function cl:use-value (t &optional t) null)

    (proclaim-function cl:require (t &optional t) t)
    (proclaim-function cl:y-or-n-p (&optional t &rest t) t)
    (proclaim-function cl:yes-or-no-p (&optional t &rest t) t)

    (proclaim-function cl:break (&optional t &rest t) null)
    (proclaim-function cl:byte (t t) t)
    (proclaim-function cl:byte-position (t) t)
    (proclaim-function cl:byte-size (t) t)
    (proclaim-function cl:cell-error-name (t) t)
    (proclaim-function cl:cis (t) t)
    (proclaim-function cl:class-of (t) t)
    (proclaim-function cl:coerce (t t) t)
    (proclaim-function cl:compile (t &optional t) (values t t t))
    (proclaim-function cl:compile-file (t &key (output-file t) (verbose t) (print t) (external-format t) (c-file t) (h-file t) (data-file t) (fasl-p t) (libraries t)) (values t t t))
    (proclaim-function cl:compile-file-pathname (t &key (output-file t) (verbose t) (print t) (external-format t) (c-file t) (h-file t) (data-file t) (fasl-p t) (libraries t) &allow-other-keys) t)
    (proclaim-function cl:compiler-macro-function (t &optional t) t)
    (proclaim-function cl:complement (t) t)
    (proclaim-function cl:compute-restarts (&optional t) t)
    (proclaim-function cl:constantly (t) t)
    (proclaim-function cl:delete-package (t) t)
    (proclaim-function cl:deposit-field (t t t) t)
    (proclaim-function cl:describe (t &optional t) (values))
    (proclaim-function cl:describe-object (t t) (values))
    (proclaim-function cl:disassemble (t) null)
    (proclaim-function cl:documentation (t t) (or string null))
    (proclaim-function cl:dpb (t t t) t)
    (proclaim-function cl:dribble (&optional t) (values))
    (proclaim-function cl:echo-stream-input-stream (t) t)
    (proclaim-function cl:echo-stream-output-stream (t) t)
    (proclaim-function cl:ed (&optional t) t)
    (proclaim-function cl:fdefinition (t) t)
    (proclaim-function cl:file-error-pathname (t) t)
    (proclaim-function cl:file-string-length (t t) t)
    (proclaim-function cl:find-all-symbols (t) t)
    (proclaim-function cl:find-class (t &optional t t) t)
    (proclaim-function cl:find-method (t t t &optional t) t)
    (proclaim-function cl:find-restart (t &optional t) t)
    (proclaim-function cl:function-keywords (t) (values t t))
    (proclaim-function cl:function-lambda-expression (t) (values t t t))
    (proclaim-function cl:get-setf-expansion (t &optional t) (values t t t t t))
    (proclaim-function cl:hash-table-rehash-size (t) t)
    (proclaim-function cl:hash-table-rehash-threshold (t) t)
    (proclaim-function cl:hash-table-size (t) t)
    (proclaim-function cl:hash-table-test (t) t)
    (proclaim-function cl:initialize-instance (t &rest t &key &allow-other-keys) t)
    (proclaim-function cl:inspect (t) t)
    (proclaim-function cl:interactive-stream-p (t) t)
    (proclaim-function cl:invoke-debugger (t) (values))
    (proclaim-function cl:invoke-restart (t &rest t) *)
    (proclaim-function cl:invoke-restart-interactively (t) *)
    (proclaim-function cl:isqrt (t) t)
    (proclaim-function cl:ldb (t t) t)
    (proclaim-function cl:ldb-test (t t) t)
    (proclaim-function cl:lisp-implementation-type () t)
    (proclaim-function cl:lisp-implementation-version () t)
    (proclaim-function cl:load (t &key (verbose t) (print t) (if-does-not-exist t) (external-format t) (search-list t)) t)
    (proclaim-function cl:load-logical-pathname-translations (t) t)
    (proclaim-function cl:logical-pathname (t) t)
    (proclaim-function cl:logical-pathname-translations (t) t)
    (proclaim-function cl:logtest (t t) t)
    (proclaim-function cl:long-site-name () t)
    (proclaim-function cl:short-site-name () t)
    (proclaim-function cl:machine-instance () t)
    (proclaim-function cl:machine-type () t)
    (proclaim-function cl:machine-version () t)
    (proclaim-function cl:make-condition (t &rest t) t)
    (proclaim-function cl:make-instance (t &rest t &key &allow-other-keys) t)
    (proclaim-function cl:make-instances-obsolete (t) t)
    (proclaim-function cl:make-load-form (t &optional t) (values t &optional t))
    ;;(proclaim-function cl:make-load-form (t &optional t) *)

    (proclaim-function cl:make-load-form-saving-slots (t &key (slot-names t) (environment t)) (values t t))
    (proclaim-function cl:mask-field (t t) t)
    (proclaim-function cl:method-qualifiers (t) t)
    (proclaim-function cl:no-applicable-method (t &rest t) *)
    (proclaim-function cl:no-next-method (t t &rest t) *)
    (proclaim-function cl:open (t &key (direction t) (element-type t) (if-exists t) (if-does-not-exit t) (external-format t) (stdio-stream t)) t)
    (proclaim-function cl:open-stream-p (t) t)
    (proclaim-function cl:package-error-package (t) t)
    (proclaim-function cl:pathname-match-p (t t) t)
    (proclaim-function cl:phase (t) t)
    (proclaim-function cl:prin1-to-string (t) t)
    (proclaim-function cl:princ-to-string (t) t)
    (proclaim-function cl:print-not-readable-object (t) t)
    (proclaim-function cl:print-object (t t) t)
    (proclaim-function cl:print-not-readable-object (t) t)
    (proclaim-function cl:proclaim (t) t)
    (proclaim-function cl:provide (t) t)
    (proclaim-function cl:rational (t) t)
    (proclaim-function cl:rationalize (t) t)
    (proclaim-function cl:read-sequence (t t &key (start t) (end t)) t)
    (proclaim-function cl:readtable-case (t) t)
    (proclaim-function cl:reinitialize-instance (t &rest t &key (direct-superclasses t) (direct-slots t) &allow-other-keys) t)
    (proclaim-function cl:remove-method (t t) t)
    (proclaim-function cl:restart-name (t) t)
    (proclaim-function cl:room (&optional t) t)
    (proclaim-function cl:shared-initialize (t t &rest t &key (lambda-list t) (argument-precedence-order t) (documentation t) (declarations t) (method-combination t) (method-class t) (optimize-slot-access t) (sealedp t) &allow-other-keys) t)
    (proclaim-function cl:signal (t &rest t) null)
    (proclaim-function cl:signum (t) t)
    (proclaim-function cl:simple-condition-format-control (t) t)
    (proclaim-function cl:simple-condition-format-arguments (t) t)
    (proclaim-function cl:slot-boundp (t t) t)
    (proclaim-function cl:slot-exists-p (t t) t)
    (proclaim-function cl:slot-makunbound (t t) t)
    (proclaim-function cl:slot-missing (t t t t &optional t) *)
    (proclaim-function cl:slot-unbound (t t t) *)
    (proclaim-function cl:slot-value (t t) t)
    (proclaim-function cl:software-type () t)
    (proclaim-function cl:software-version () t)
    (proclaim-function cl:stream-error-stream (t) t)
    (proclaim-function cl:stream-external-format (t) t)
    (proclaim-function cl:synonym-stream-symbol (t) t)
    (proclaim-function cl:translate-logical-pathname (t &key) t)
    (proclaim-function cl:translate-pathname (t t t &key) t)
    (proclaim-function cl:two-way-stream-input-stream (t) t)
    (proclaim-function cl:two-way-stream-output-stream (t) t)
    (proclaim-function cl:type-error-datum (t) t)
    (proclaim-function cl:type-error-expected-type (t) t)
    (proclaim-function cl:unbound-slot-instance (t) t)
    (proclaim-function cl:update-instance-for-different-class (t t &rest t &key &allow-other-keys) t)
    (proclaim-function cl:update-instance-for-redefined-class (t t t t &rest t &key &allow-other-keys) t)
    (proclaim-function cl:vector-pop (t) t)
    (proclaim-function cl:warn (t &rest t) null)
    (proclaim-function cl:write-sequence (t &key (start t) (end t)) t)

    (proclaim-function si:bc-disassemble (t) t)
    (proclaim-function si:bc-split (t) t)
    (proclaim-function si:coerce-to-base-string (t) t)
    (proclaim-function si:coerce-to-character-string (t) t)
    (proclaim-function si:coerce-to-filename (t) t)
    (proclaim-function si:coerce-to-function (t) t)
    (proclaim-function si:coerce-to-package (t) t)
    (proclaim-function si:copy-to-simple-base-string (t) t)
    (proclaim-function si:compiled-function-block (t) t)
    (proclaim-function si:copy-stream (t) t)
    (proclaim-function si:do-read-sequence (t t t t) t)
    (proclaim-function si:do-write-sequence (t t t t) t)
    (proclaim-function si:eval-in-env (t &optional t t t t) t)
    (proclaim-function si:file-column (t) t)
    (proclaim-function si:file-kind (t &key (follow-symlinks t) (signal-error t)) t)
    (proclaim-function si:function-block-name (t) t)
    (proclaim-function si:get-SYS-library-pathname () t)
    (proclaim-function mkcl:getpid () t)
    (proclaim-function mkcl:gettid () t)
    (proclaim-function si:hash-table-iterator (t) t)
    (proclaim-function si:ihs-bds-marker (t) t)
    (proclaim-function si:ihs-next (t) t)
    (proclaim-function si:ihs-prev (t) t)
    (proclaim-function si:load-source (t t t t) t)
    (proclaim-function mkcl:logical-pathname-p (t) t)
    (proclaim-function si:make-lambda (t t) t)
    (proclaim-function si:mangle-name (t) t)
    (proclaim-function mkcl:mkstemp (t &key (element-type t) (external-format t)) t)
    (proclaim-function mkcl:rmdir (t) t)
    (proclaim-function mkcl:make-pipe () t)
    (proclaim-function si:package-hash-tables (t) t)
    (proclaim-function si:pathname-translations (t &optional t) t)
    (proclaim-function si:all-logical-pathname-translations () t)
    (proclaim-function si:process-declarations (t &optional t) t)
    (proclaim-function si:process-lambda-list (t t) t)
    (proclaim-function si:readtable-case-set (t t) t)
    (proclaim-function si:rem-sysprop (t t) t)
    (proclaim-function mkcl:run-command (t t &key (real-name t)) (values t t))
    (proclaim-function mkcl:run-program-1 (t t &key (input t) (output t) (error t) (external-format t) (element-type t) (environment t) (directory t) (search t) (wait t) (detached t)) (values t t t))
    (proclaim-function si:safe-eval (t t t) t)
    (proclaim-function mkcl:setenv (t t) t)
    (proclaim-function si:structure-length (t) t)
    (proclaim-function mkcl:system (t) t)
    (proclaim-function si:valid-function-name-p (t) t)
    (proclaim-function si:write-object (t t) t)
    (proclaim-function si:write-ugly-object (t t) t)
    (proclaim-function si:copy-instance (t) t)
    (proclaim-function si:generic-function-p (t) t)
    (proclaim-function si:instance-length (t) t)
    (proclaim-function si:sl-boundp (t) t)
    (proclaim-function si:subclassp (t t) boolean)
    (proclaim-function si:allocate-foreign-data (t t) t)
    (proclaim-function si:find-foreign-symbol (t t t t) t)
    (proclaim-function si:foreign-address (t) t)
    (proclaim-function si:foreign-indexed (t t t t) t)
    (proclaim-function si:foreign-recast (t t t) t)
    (proclaim-function si:foreign-ref (t t t t) t)
    (proclaim-function si:foreign-ref-elt (t t t) t)
    (proclaim-function si:foreign-set (t t t) t)
    (proclaim-function si:foreign-set-elt (t t t t) t)
    (proclaim-function si:foreign-tag (t) t)
    (proclaim-function si:free-foreign-data (t) t)
    (proclaim-function si:make-foreign-data-from-array (t) t)
    (proclaim-function si:load-foreign-module (t) t)
    (proclaim-function si:unload-foreign-module (t) t)
    (proclaim-function si:null-pointer-p (t) t)
    (proclaim-function si:size-of-foreign-elt-type (t) t)
    (proclaim-function si:gc (&optional t) (values))
    (proclaim-function si:gc-dump () (values))
    (proclaim-function si:gc-off () (values))
    (proclaim-function si:gc-on () (values))

    (proclaim-function mt:all-threads () t)
    (proclaim-function mt:exit-thread (t) t)
    (proclaim-function mt:terminate-thread (t) t)
    (proclaim-function mt:make-thread (&key (name t) (initial-bindings t) (call-stack-size t) (binding-stack-initial-size t) (binding-stack-size-limit t) (frame-stack-initial-size t) (frame-stack-size-limit t) (lisp-temp-stack-initial-size t) (lisp-temp-stack-size-limit t) (sigaltstack-size t)) t)
    (proclaim-function mt:thread-active-p (t) t)
    (proclaim-function mt:thread-enable (t) t)
    (proclaim-function mt:thread-yield () (values))
    (proclaim-function mt:thread-kill (t) t)
    (proclaim-function mt:thread-detach (t) t)
    (proclaim-function mt:thread-join (t) t)
    (proclaim-function mt:thread-name (t) t)
    (proclaim-function mt:thread-preset (t t &rest t) t)
    (proclaim-function mt:show-sigmask () t)
    (proclaim-function mt:reset-sigmask () t)
    (proclaim-function mt:block-signals () t)
    (proclaim-function mt:unblock-signals () t)
    (proclaim-function mt:thread-run-function (t t &rest t) t)
    (proclaim-function mt:make-lock (&key (name t) (recursive t) (fast t)) t)
    (proclaim-function mt:recursive-lock-p (t) t)
    (proclaim-function mt:lock-name (t) t)
    (proclaim-function mt:lock-holder (t) t)
    (proclaim-function mt:get-lock (t &optional t) t)
    (proclaim-function mt:giveup-lock (t) t)
    (proclaim-function mt:make-rwlock () t)
    (proclaim-function mt:giveup-rwlock (t &optional t) t)
    (proclaim-function mt:get-read-rwlock (t &optional t) t)
    (proclaim-function mt:get-write-rwlock (t &optional t) t)
    (proclaim-function mt:make-semaphore (&optional t) t)
    (proclaim-function mt:semaphore-count (t) t)
    (proclaim-function mt:semaphore-signal (t &optional t) t)
    (proclaim-function mt:semaphore-wait (t &optional t) t)
    (proclaim-function mt:make-condition-variable () t)
    (proclaim-function mt:condition-wait (t t &optional t) t)
    (proclaim-function mt:condition-signal (t) t)
    (proclaim-function mt:condition-broadcast (t) t)
    (proclaim-function mt:interrupt-thread (t t &key (force t) (call-stack-size t)) t)
    (proclaim-function mt:join-thread (t) t)
    (proclaim-function mt:detach-thread (t) t)
    (proclaim-function mt:thread-plist (t) t)
    (proclaim-function mt:set-thread-plist (t t) t)

    (proclaim-function clos::funcallable-standard-instance-access (t t) t)
    (proclaim-function clos::set-funcallable-instance-function (t t) t)

    (proclaim-function si:load-binary (t t t t) t)
    (proclaim-function si:call-cfun (t t t t &optional t) t)
    (proclaim-function si:make-dynamic-callback (t t t t &optional t) t)
    (proclaim-function si:do-sigsegv () t)
    (proclaim-function si:objnull () t)
    (proclaim-function si:display-signal-dispositions () t)
    (proclaim-function si:set-buffering-mode (t t) t)
    (proclaim-function si:get-finalizer (t) t)
    (proclaim-function si:set-finalizer (t t) (values))
    (proclaim-function si:find-relative-package (t) t)
    (proclaim-function si:package-parent (t) t)
    (proclaim-function si:gc-stats (t) (values t t t))
    (proclaim-function si:clear-gfun-cache (t) (values))
    (proclaim-function si:apply-from-temp-stack-frame (t t) *)
    (proclaim-function si:log1p (t) t)
    (proclaim-function si:compiled-function-file (t) (values t t))
    (proclaim-function si:hash-eql (&rest t) t)
    (proclaim-function si:hash-equal (&rest t) t)
    (proclaim-function si:hash-equalp (&rest t) t)

    (proclaim-function mkcl:copy-file (t t) t)

    (proclaim-function si:fill-array-with-elt (t t t t) array)
    (proclaim-function si::fill-array-with-seq (t t) array)
    (proclaim-function si:float-nan-p (t) t)
    (proclaim-function si:float-infinity-p (t) t)
    (proclaim-function si:read-object-or-ignore (t t) t)
    (proclaim-function si:unbound-value-p (t) t)
    (proclaim-function si:packages-in-waiting (t) t)
    (proclaim-function si:hash-tables-statistics (t) t)
    (proclaim-function si:mem-stats () (values t t t))
    (proclaim-function si:closurep (t) t)
    (proclaim-function si:closure-env (t) (values t t))
    (proclaim-function si:closure-producer (t) t)
    (proclaim-function si:compiled-function-owner (t) t)
    (proclaim-function si:set-compiled-function-owner (t t) t)
    (proclaim-function si:self-truename () t)
    (proclaim-function si:top-apply (t t) t)
    (proclaim-function si:convert-cmp-lexical-info (t) t)
    (proclaim-function si:install-sigsegv-monitor () (values))
    (proclaim-function si:list-libraries () t)
    (proclaim-function si:closure-depth (t) t)
    (proclaim-function si:closure-level (t t) t)
    (proclaim-function si:closure-level-size (t) t)
    (proclaim-function si:closure-level-var (t t) t)
    (proclaim-function si:closure-level-set-var (t t t) t)
    (proclaim-function si:closure-level-outer-level (t) t)
    (proclaim-function si:system-properties () t)

    (proclaim-function si:set-class-proper-name (t t) t)
    (proclaim-function si:clone-closure (&rest t) t)
    (proclaim-function si:update-function-references (t) t)
    (proclaim-function si:get-fun-ref-sym (t t) t)
    (proclaim-function si:generate-forward-fun-ref-handler (t t) t)
    (proclaim-function mkcl:getuid () t)
    (proclaim-function si:gdb () t)
    (proclaim-function si:trim-dynamic-cons-stack () t)
    (proclaim-function si:dyn-cons (t t) t)

    (proclaim-function si:disable-fpe (t) t)
    (proclaim-function si:enable-fpe (t) t)
    (proclaim-function si:all-enabled-fpe () t)
    (proclaim-function si:fpe-enabled-p (t) t)
    (proclaim-function si:all-raised-fpe () t)
    (proclaim-function si:fpe-raised-p (t) t)
    (proclaim-function si:raise-fpe (t) t)
    (proclaim-function si:clear-fpe (t) t)
    (proclaim-function si:clear-all-fpe () t)
    (proclaim-function si:initial-floating-point-exception-set () t)

    (proclaim-function si:simple-base-string-p (t) t)
    (proclaim-function si:stream-external-format-set (t t) (values t t))
    (proclaim-function si:get-buffering-mode (t) t)

    (proclaim-function si:utf-8 (t) t)
    (proclaim-function si:utf-8-p (t) t)
    (proclaim-function si:utf-8-length (t) t)
    (proclaim-function si:utf-8-as-is (t) t)
    (proclaim-function si:utf-8-char (t t) t)
    (proclaim-function si:utf-8+ (&rest t) t)
    (proclaim-function si:utf-8= (t t) t)
    (proclaim-function si:utf-8-push-extend (t t) t)
    (proclaim-function si:utf-8-last (t) t)
    (proclaim-function si:utf-16 (t) t)
    (proclaim-function si:utf-16-p (t) t)
    (proclaim-function si:utf-16-length (t) t)
    (proclaim-function si:utf-16-as-is (t) t)
    (proclaim-function si:utf-16-char (t t) t)
    (proclaim-function si:utf-16+ (&rest t) t)
    (proclaim-function si:utf-16= (t t) t)
    (proclaim-function si:utf-16-push-extend (t t) t)
    (proclaim-function si:utf-16-last (t) t)

    (proclaim-function mkcl:join-process (t) t)
    (proclaim-function mkcl:terminate-process (t &key (force t)) t)
    (proclaim-function mkcl:process-p (t) t)
    (proclaim-function mkcl:process-input (t) t)
    (proclaim-function mkcl:process-output (t) t)
    (proclaim-function mkcl:process-error (t) t)
    (proclaim-function mkcl:process-plist (t) t)
    (proclaim-function mkcl:process-to-worker (t) t)
    (proclaim-function mkcl:process-from-worker (t) t)
    (proclaim-function mkcl:process-error-from-worker (t) t)
    (proclaim-function mkcl:set-process-plist (t t) t)
    (proclaim-function mkcl:set-process-to-worker (t t) t)
    (proclaim-function mkcl:set-process-from-worker (t t) t)
    (proclaim-function mkcl:set-process-error-from-worker (t t) t)
    (proclaim-function mkcl:process-status (t) t)
    (proclaim-function mkcl:process-exit-code (t) t)
    (proclaim-function mkcl:process-command (t) t)
    (proclaim-function mkcl:process-argv (t) t)
    (proclaim-function mkcl:process-id (t) t)
    (proclaim-function mkcl:process-detached-p (t) t)
    (proclaim-function mkcl:detach-process (t) t)

    (proclaim-function si:sample-allocation-statistics () t)
    (proclaim-function si:reset-allocation-statistics () t)

    (proclaim-function si:room-report (t) null)

    (proclaim-function mkcl:probe-file-p (t) t)
    (proclaim-function mkcl:stream-filename (t) t)

    (proclaim-function si:mangle-function-name (t) (values t t t t))
    (proclaim-function si:mangle-string (t) t)
    (proclaim-function si:mangle-symbol (t) t)

    (proclaim-function si:close-package (t) t)
    (proclaim-function si:reopen-package (t) t)
    (proclaim-function si:package-closed-p (t) t)

    (proclaim-function mt:abandon-thread (t) t)

    (proclaim-function si:shutdown-in-progress-p () t)
    (proclaim-function si:register-shutdown-thread (t) t)
    (proclaim-function si:register-shutdown-watchdog-thread (t t) t)
    (proclaim-function si:shutdown-watchdog-thread () (values t t))
    (proclaim-function si:shutdown-mkcl (t t t t) t)
    (proclaim-function si:setup-for-gdb (&optional t) t)

    (proclaim-function mt:current-thread () t)

    (proclaim-function si:interrupt-status () t)
    (proclaim-function si:copy-to-simple-string (t) t)
    (proclaim-function si:scrub-values () (values))

    (proclaim-function mt:cancel-thread () t)
    (proclaim-function mt:try-to-wake-up-thread (t) t)
    (proclaim-function mt:request-thread-shutdown (t) t)
    (proclaim-function mt:thread-shutdown-requested-p (t) t)

    (proclaim-function si:make-foreign-null-pointer () t)
    (proclaim-function si:foreignp (t) t)
    (proclaim-function si:libc-error-string (t) t)
    (proclaim-function si:errno-string () t)

    (proclaim-function si:get-local-time-zone () t)
    (proclaim-function si:uname () (values t t t t t))

    (proclaim-function mkcl:pathname-complete-p (t) t)
    (proclaim-function mkcl:meld-pathnames (t &optional t t) t)

    (proclaim-function si:non-interactive-thread-debugger-trap (t t) (values))
    (proclaim-function si:mkcl-version () t)
    (proclaim-function si:mkcl-major-version () t)
    (proclaim-function si:mkcl-minor-version () t)
    (proclaim-function si:mkcl-patch-level () t)
    (proclaim-function si:set-binding-stack-limit (t) t)
    (proclaim-function si:get-binding-stack-limit () t)
    (proclaim-function si:set-frame-stack-limit (t) t)
    (proclaim-function si:get-frame-stack-limit () t)
    (proclaim-function si:set-lisp-temp-stack-limit (t) t)
    (proclaim-function si:get-lisp-temp-stack-limit () t)
    (proclaim-function si:set-heap-size-limit (t) t)
    (proclaim-function si:get-heap-size-limit () t)
    (proclaim-function si:get-call-stack-limit () t)

    (proclaim-function mt:thread-status (t) t)

    (proclaim-function si:trim-ffi-arguments-staging-area () t)
    (proclaim-function si:release-ffi-area () t)
    (proclaim-function si:list-all-children () t)
    (proclaim-function si:trace-specials () t)
    (proclaim-function si:untrace-specials () t)

    (proclaim-function mt:abort-thread () t)

    (proclaim-function mkcl:octets (t) t)
    (proclaim-function mkcl:double-octets (t) t)
    (proclaim-function mkcl:run-program (t t &rest t &key (input t) (if-input-does-not-exist t) (output t) (if-output-exists t) (error t) (if-error-exists t) (directory t) (search t) (wait t) (detached t) (external-format t) (element-type t)) t)

    (proclaim-function si:signum-to-signal-name (t) t)
    (proclaim-function si:objnull-value-p (t) t)
    (proclaim-function si:shutdown-mkcl-threads (t t t t) t)

    (proclaim-function si:ansi-close (t t) t)
    (proclaim-function si:ansi-streamp (t) t)
    (proclaim-function si:ansi-input-stream-p (t) t)
    (proclaim-function si:ansi-output-stream-p (t) t)
    (proclaim-function si:ansi-open-stream-p (t) t)
    (proclaim-function si:ansi-stream-element-type (t) t)


#|
|#
    )
  )  ;; end of (defparameter +all-optimizers+ ...)

(defparameter +mkcl-min-internals+
  '(
    (proclaim-function si::maybe-quote (t) t)
    (proclaim-function si::simple-program-error (t &rest t) null)
    (proclaim-function si::eval-feature (t) t)
    (proclaim-function si::do-read-feature (t t t t) t)
    (proclaim-function si::find-declarations (t &optional t) (values t t t))
    (proclaim-function si::cmp-env-for-bytecode (t) t)
    (proclaim-function si::macrolet-functions (t t) *)

    (proclaim-function sys::destructure (t t) (values t t t t t))
    (proclaim-function sys::expand-defmacro (t t t) (values t t t))
    (proclaim-function si::read-help-file (t) t)
    (proclaim-function si::dump-help-file (t t &optional t) t)
    (proclaim-function si::search-help-file (t t) t)

    (proclaim-function si::set-documentation (t t t) t)

    (proclaim-function si::remove-documentation (t) (values t t))
    (proclaim-function si::expand-set-documentation (t t t) t)
    (proclaim-function si::check-arg-length (t t t) t)

    (proclaim-function si::print-doc (t &optional t) *)
    (proclaim-function si::autoload (t &rest t) t)

    (proclaim-function si::setf-expand-1 (t t t) t)
    (proclaim-function si::setf-expand (t t) t)

    (proclaim-function clos::classp (t) t)
    (proclaim-function clos::class-name (t) t)
    (proclaim-function clos::print-object-as-struct (t t) t)

    (proclaim-function si::typep-in-env (t t t) t)
    (proclaim-function si::validate-type-name (t) t)
    (proclaim-function si::in-interval-p (t t) t)
    (proclaim-function si::match-dimensions (t t) t)
    (proclaim-function si::simple-array-p (t) t)
    (proclaim-function si::normalize-type (t) (value t t))
    (proclaim-function si::expand-deftype (t) t)
    (proclaim-function si::find-registered-tag (t) t)
    (proclaim-function si::find-registered-tag-equalp (t) t)
    (proclaim-function si::find-type-bounds (t t t t) (values t t))
    (proclaim-function si::maybe-save-types () t)
    (proclaim-function si::update-types (t t) t)
    (proclaim-function si::push-type (t t) t)
    (proclaim-function si::simple-member-type (t) t)
    (proclaim-function si::number-member-type (t) t)
    (proclaim-function si::register-member-type (t) t)
    (proclaim-function si::register-interval-type (t) t)
    (proclaim-function si::find-built-in-tag (t) t)
    (proclaim-function si::register-type (t t t) t)
    (proclaim-function si::parse-array-type (t) (values t t t))
    (proclaim-function si::canonical-type (t) t)
    (proclaim-function si::fast-subtypep (t t) (values t t))
    (proclaim-function si::fast-upgraded-array-element-type (t) t)
    (proclaim-function si::bounds-<= (t t) t)
    (proclaim-function si::bounds-< (t t) t)
    (proclaim-function si::register-elementary-interval (t t) t)
    (proclaim-function si::canonical-type (t) t)
    (proclaim-function si::extend-type-tag (t t) t)
    (proclaim-function si::register-class (t) t)
    (proclaim-function si::register-satisfies-type (t) t)
    (proclaim-function si::canonical-complex-type (t) t)
    (proclaim-function si::register-array-type (t) t)
    (proclaim-function si::register-function-type (t) t)
    (proclaim-function si::register-values-type (t) t)
    (proclaim-function si::safe-canonical-type (t) t)
    (proclaim-function si::subtypep-in-env (t t &optional t) (values t t))
    (proclaim-function si::fast-type= (t t) (values t t))
    (proclaim-function si::type-filter (t &optional t) t)
    (proclaim-function si::proclaimed-function-name (t) t)
    (proclaim-function si::proclaimed-function-arg-types (t) t)
    (proclaim-function si::proclaimed-function-return-type (t) t)
    (proclaim-function si::proclaimed-function-required-arg-count (t) t)
    (proclaim-function si::proclaimed-function-C-name (t) t)
    (proclaim-function si::set-proclaimed-function-name (t t) t)
    (proclaim-function si::set-proclaimed-function-arg-types (t t) t)
    (proclaim-function si::set-proclaimed-function-return-type (t t) t)
    (proclaim-function si::set-proclaimed-function-required-arg-count (t t) t)
    (proclaim-function si::set-proclaimed-function-C-name (t t) t)
    (proclaim-function si::validate-arg-typespec-key (t) t)
    (proclaim-function si::validate-arg-typespec-rest (t) t)
    (proclaim-function si::validate-arg-typespec-optionals (t) t)
    (proclaim-function si::validate-arg-typespec (t) t)
    (proclaim-function si::validate-function-return-type (t) t)
    (proclaim-function si::make-proclaimed-function (&key (name t) (arg-types t) (return-type t) (required-arg-count t) (C-name t)) t)
    (proclaim-function si::type-name-p (t) t)
    (proclaim-function si::typespecp (t) t)
    (proclaim-function si::proclaim-var (t t) t)
    (proclaim-function si::add-function-proclamation (t t) t)
    (proclaim-function si::do-declaration (t t) t)
    (proclaim-function si::valid-type-specifier (t) (values t t))

    ;; seq.lsp
    (proclaim-function si::error-sequence-type (t) t)
    (proclaim-function si::error-sequence-length (t t) t)
    (proclaim-function si::closest-sequence-type (t) (values t t))
    (proclaim-function mkcl::str+ (&rest t) t)
    (proclaim-function mkcl::bstr+ (&rest t) t)
    (proclaim-function mkcl:split-string (t t) t)

    (proclaim-function si::iterate-over-contents (t t t t) t)
    (proclaim-function si::copy-array-contents (t t) t)

    ;; assert.lsp
    (proclaim-function si::remove-otherwise-from-clauses (t) t)
    (proclaim-function si::accumulate-cases (t t t) t)
    (proclaim-function si::ecase-error (t t t) t)
    (proclaim-function si::etypecase-error (t t t) t)
    (proclaim-function si::do-check-type (t t t t) t)

    (proclaim-function si::process-boa-lambda-list (t t t t) (values t t))
    (proclaim-function si::make-predicate (t t t t) t)
    (proclaim-function si::parse-slot-description (t t &optional t) t)
    (proclaim-function si::overwrite-slot-descriptions (t t) t)
    (proclaim-function si::make-constructor (t t t t t) t)

    (proclaim-function si::check-keyword (t t t &optional (allow-other-keys t)) t)
    (proclaim-function si::load-encoding (t) (values t t))
    (proclaim-function si::make-encoding (t) (values t t))
    (proclaim-function si::absolute-logical-pathname (t &optional t) t)
    (proclaim-function si::absolute-pathname (t &optional t) t)
    (proclaim-function si::complete-pathname (t &optional t) t)
    (proclaim-function mkcl::directory-p (t) t)
    (proclaim-function si::absolute-pathname-p (t) t)
    (proclaim-function si::relative-pathname (t t) (values t t))
    (proclaim-function si::launch-to-subprocess-worker (t t) t)
    (proclaim-function si::launch-from-subprocess-worker (t t) t)
    (proclaim-function si::launch-error-from-subprocess-worker (t t) t)
    (proclaim-function si::swap-args (t) t)

    (proclaim-function si::leap-year-p (t) t)
    (proclaim-function si::number-of-days-from-1900 (t) t)
    (proclaim-function si::daylight-saving-time-p (t t) t)
    (proclaim-function si::recode-universal-time (t t t t t t t t) t)

    (proclaim-function si::complex-asin (t) t)
    (proclaim-function si::complex-acos (t) t)
    (proclaim-function si::complex-acosh (t) t)
    (proclaim-function si::complex-atanh (t) t)

    (proclaim-function si::expand-do-symbols (t t t t t) t)
    (proclaim-function si::packages-iterator (t t t) t)

    (proclaim-function si::sequence-limits (t t t) (values t t))
    (proclaim-function si::internal-count (t t &key (count t) (key t) (from-end t) (test t) (test-not t) (start t) (end t)) t)
    (proclaim-function si::seqtype (t) t)
    (proclaim-function si::list-merge-sort (t t t) t)
    (proclaim-function si::quick-sort (t t t t t) t)
    (proclaim-function si::trace-one (t) t)
    (proclaim-function si::tracing-body (t) t)
    (proclaim-function si::untrace-one (t) t)
    (proclaim-function si::steppable-function (t) t)
    (proclaim-function si::interactive-loop (&key (commands t) (prompt-hook t) (broken-at t) (quiet t)) t)

    ;; defstruct.lsp
    (proclaim-function si::define-structure (t t t t t t t t t t t t t t t t) t)
    (proclaim-function si::structure-type-error (t t t t) t)

    ;; loop3.lsp
    (proclaim-function si::loop-gentemp (&optional t) t)
    (proclaim-function si::make-loop-minimax (t t) t)
    (proclaim-function si::make-loop-minimax-internal (&key (answer-variable t) (type t) (temp-variable t) (flag-variable t) (operations t) (infinity-data t)) t)
    (proclaim-function si::loop-minimax-answer-variable (t) t)
    (proclaim-function si::loop-minimax-type (t) t)
    (proclaim-function si::loop-minimax-temp-variable (t) t)
    (proclaim-function si::loop-minimax-flag-variable (t) t)
    (proclaim-function si::loop-minimax-operations (t) t)
    (proclaim-function si::loop-minimax-infinity-data (t) t)
    (proclaim-function si::loop-typed-init (t) t)
    (proclaim-function si::hide-variable-reference (t t t) t)
    (proclaim-function si::make-loop-universe (&key (keywords t) (iteration-keywords t) (for-keywords t) (path-keywords t) (type-symbols t) (type-keywords t) (ansi t) (implicit-for-required t)) t)
    (proclaim-function si::loop-universe-keywords (t) t)
    (proclaim-function si::loop-universe-iteration-keywords (t) t)
    (proclaim-function si::loop-universe-for-keywords (t) t)
    (proclaim-function si::loop-universe-path-keywords (t) t)
    (proclaim-function si::loop-universe-type-symbols (t) t)
    (proclaim-function si::loop-universe-type-keywords (t) t)
    (proclaim-function si::loop-universe-ansi (t) t)
    (proclaim-function si::loop-universe-implicit-for-required (t) t)
    (proclaim-function si::loop-make-psetq (t) t)
    (proclaim-function si::loop-make-desetq (t) t)
    (proclaim-function si::loop-error (t &rest t) t)
    (proclaim-function si::loop-warn (t &rest t) t)
    (proclaim-function si::loop-optimization-quantities (t) (values t t t t t))
    (proclaim-function si::loop-code-duplication-threshold (t) t)
    (proclaim-function si::estimate-code-size (t t) t)
    (proclaim-function si::estimate-code-size-1 (t t) t)
    (proclaim-function si::destructuring-size (t) t)
    (proclaim-function si::subst-gensyms-for-nil (t) t)
    (proclaim-function si::loop-build-destructuring-bindings (t t) t)
    (proclaim-function si::loop-lookup-keyword (t t) t)
    (proclaim-function si::loop-hack-iteration (t) t)
    (proclaim-function si::loop-tmember (t t) t)
    (proclaim-function si::loop-tassoc (t t) t)
    (proclaim-function si::loop-tequal (t t) t)
    (proclaim-function si::loop-pseudo-body (t) t)
    (proclaim-function si::loop-construct-return (t) t)
    (proclaim-function si::loop-declare-variable (t t) t)
    (proclaim-function si::loop-make-variable (t t t &optional t) t)
    (proclaim-function si::loop-variable-p (t) t)
    (proclaim-function si::loop-constantp (t) t)
    (proclaim-function si::loop-disallow-conditional (&optional t) t)
    (proclaim-function si::loop-emit-body (t) t)
    (proclaim-function si::loop-construct-return (t) t)
    (proclaim-function si::make-loop-collector (&key (name t) (class t) (history t) (tempvars t) (dtype t) (data t)) t)
    (proclaim-function si::loop-collector-name (t) t)
    (proclaim-function si::loop-collector-class (t) t)
    (proclaim-function si::loop-collector-history (t) t)
    (proclaim-function si::loop-collector-tempvars (t) t)
    (proclaim-function si::loop-collector-dtype (t) t)
    (proclaim-function si::loop-collector-data (t) t)
    (proclaim-function si::loop-get-collection-info (t t t) (values t t))
    (proclaim-function si::loop-check-data-type (t t &optional t) t)
    (proclaim-function si::loop-emit-final-value (&optional t) t)
    (proclaim-function si::loop-construct-return (t) t)
    (proclaim-function si::loop-note-minimax-operation (t t) t)
    (proclaim-function si::loop-optional-type (&optional t) t)
    (proclaim-function si::loop-make-iteration-variable (t t t) t)
    (proclaim-function si::loop-constant-fold-if-possible (t &optional t) (values t t t))
    (proclaim-function si::loop-list-step (t) (values t t))
    (proclaim-function si::make-loop-path (&key (names t) (preposition-groups t) (inclusive-permitted t) (function t) (user-data t)) t)
    (proclaim-function si::loop-path-names (t) t)
    (proclaim-function si::loop-path-preposition-groups (t) t)
    (proclaim-function si::loop-path-inclusive-permitted (t) t)
    (proclaim-function si::loop-path-function (t) t)
    (proclaim-function si::loop-path-user-data (t) t)
    (proclaim-function si::loop-collect-prepositional-phrases (t &optional t t) t)
    (proclaim-function si::loop-sequencer (t t t t t t t t t t) t)
    (proclaim-function si::named-variable (t) (values t t))
    (proclaim-function si::make-standard-loop-universe (&key (keywords t) (for-keywords t) (iteration-keywords t) (path-keywords t) (type-keywords t) (type-symbols t) (ansi t)) t)
    (proclaim-function si::add-loop-path (t t t &key (preposition-groups t) (inclusive-permitted t) (user-data t)) t)
    (proclaim-function si::make-ansi-loop-universe (t) t)
    (proclaim-function si::loop-translate (t t t) t)
    (proclaim-function si::loop-standard-expansion (t t t) t)

    ;; format.lsp
    (proclaim-function si::float-string (t t t t t t t) (values t t t t t))
    (proclaim-function si::flonum-to-string (t &optional t t t t) (values t t t t t))
    (proclaim-function si::print-float-exponent (t t t) t)
    (proclaim-function si::scale-exponent (t) (values t t))
    (proclaim-function si::output-float-infinity (t t) t)
    (proclaim-function si::output-float-nan (t t) t)
    (proclaim-function si::output-float-aux (t t) t)
    (proclaim-function si::parse-directive (t t) t)
    (proclaim-function si::make-format-directive (&key (string t) (start t) (end t) (character t) (colonp t) (atsignp t) (params t)) t)
    (proclaim-function si::format-directive-string (t) t)
    (proclaim-function si::format-directive-start (t) t)
    (proclaim-function si::format-directive-end (t) t)
    (proclaim-function si::format-directive-character (t) t)
    (proclaim-function si::format-directive-colonp (t) t)
    (proclaim-function si::format-directive-atsignp (t) t)
    (proclaim-function si::format-directive-params (t) t)
    (proclaim-function si::format-directive-p (t) t)
    (proclaim-function si::tokenize-control-string (t) t)
    (proclaim-function si::interpret-directive-list (t t t t) t)
    (proclaim-function si::%formatter (t) t)
    (proclaim-function si::expand-control-string (t) t)
    (proclaim-function si::expand-directive-list (t) t)
    (proclaim-function si::expand-directive (t t) t)
    (proclaim-function si::find-directive (t t t) t)
    (proclaim-function si::format-write-field (t t t t t t t) t)
    (proclaim-function si::expand-next-arg (&optional t) t)
    (proclaim-function si::%set-format-directive-expander (t t) t)
    (proclaim-function si::%set-format-directive-interpreter (t t) t)
    (proclaim-function si::format-princ (t t t t t t t t) t)
    (proclaim-function si::format-prin1 (t t t t t t t t) t)
    (proclaim-function si::format-print-named-character (t t) t)
    (proclaim-function si::char-printing-p (t) t)
    (proclaim-function si::check-output-layout-mode (t) t)
    (proclaim-function si::format-add-commas (t t t) t)
    (proclaim-function si::expand-format-integer (t t t t) t)
    (proclaim-function si::format-print-integer (t t t t t t t t t) t)
    (proclaim-function si::format-print-old-roman (t t) t)
    (proclaim-function si::format-print-roman (t t) t)
    (proclaim-function si::format-print-ordinal (t t) t)
    (proclaim-function si::format-print-cardinal (t t) t)
    (proclaim-function si::format-print-cardinal-aux (t t t t) t)
    (proclaim-function si::format-print-small-cardinal (t t) t)
    (proclaim-function si::format-fixed (t t t t t t t t) t)
    (proclaim-function si::format-fixed-aux (t t t t t t t t) t)
    (proclaim-function si::decimal-string (t) t)
    (proclaim-function si::format-exponential (t t t t t t t t t t) t)
    (proclaim-function si::format-exp-aux (t t t t t t t t t t) t)
    (proclaim-function si::decimal-string (t) t)
    (proclaim-function si::format-exponent-marker (t) t)
    (proclaim-function si::format-general (t t t t t t t t t t) t)
    (proclaim-function si::format-general-aux (t t t t t t t t t t) t)
    (proclaim-function si::format-dollars (t t t t t t t t) t)
    (proclaim-function si::format-relative-tab (t t t) t)
    (proclaim-function si::format-absolute-tab (t t t) t)
    (proclaim-function si::pretty-stream-p (t) t)
    (proclaim-function si::output-spaces (t t) t)
    (proclaim-function si::formatter-aux (t t t &optional t) t)
    (proclaim-function si::parse-conditional-directive (t) (values t t t))
    (proclaim-function si::expand-maybe-conditional (t) t)
    (proclaim-function si::expand-true-false-conditional (t t) t)
    (proclaim-function si::parse-conditional-directive (t) (values t t t))
    (proclaim-function si::parse-format-justification (t) (values t t t t))
    (proclaim-function si::parse-format-logical-block (t t t t t t t) (values t t t t))
    (proclaim-function si::expand-format-logical-block (t t t t t) t)
    (proclaim-function si::expand-format-justification (t t t t t) t)
    (proclaim-function si::interpret-format-logical-block (t t t t t t t t) t)
    (proclaim-function si::interpret-format-justification (t t t t t t t t) t)
    (proclaim-function si::format-justification (t t t t t t t t t t t) t)
    (proclaim-function si::add-fill-style-newlines (t t t) t)
    (proclaim-function si::add-fill-style-newlines-aux (t t t) t)
    (proclaim-function si::pprint-pop-helper (t t t) t)
    (proclaim-function si::pprint-logical-block-helper (t t t t t t) t)
    (proclaim-function si::extract-user-function-name (t t t) t)

    ;; defpackage.lsp
    (proclaim-function si::find-duplicates (&rest lists) t)
    (proclaim-function si::find-or-make-symbol (t t) t)
    (proclaim-function si::dodefpackage (t t t t t t t t t t) t)

    ;; ffi.lsp
    (proclaim-function ffi::%convert-to-ffi-type (t &optional t) t)
    (proclaim-function ffi::slot-position (t t) (values t t t))
    (proclaim-function ffi::size-of-foreign-type (t) (values t t))
    (proclaim-function ffi::foreign-elt-type-p (t) t)
    (proclaim-function ffi::%foreign-data-ref (t t t t) t)
    (proclaim-function ffi::%foreign-data-set (t t t t) t)
    (proclaim-function ffi::foreign-string-length (t) t)
    (proclaim-function ffi::map-name-from-c-to-lisp (t) (values t t))
    (proclaim-function ffi::%convert-to-arg-type (t) t)
    (proclaim-function ffi::%convert-to-return-type (t) t)
    (proclaim-function ffi::produce-function-call (t t) t)

    ;; cmdline.lsp
    (proclaim-function si::command-arg-error (t &rest t) t)
    (proclaim-function si::produce-init-code (t t) (values t t t))

    ;; kernel.lsp
    (proclaim-function ensure-generic-function (t &key (argument-precedence-order t) (declare t) (documentation t) (environment t) (generic-function-class t) (lambda-list t) (method-class t) (method-combination t) (force-redefinition t) (source t)) t)
    (proclaim-function clos::mangle-internal-method-name (t t t) t)
    (proclaim-function clos::generic-function-method-class (t) t)
    (proclaim-function make-method (t t t t t t t t t) t)
    (proclaim-function add-method (t t) t)
    (proclaim-function clos::generic-function-name (t) t)
    (proclaim-function clos::generic-function-lambda-list (t) t)
    (proclaim-function clos::generic-function-methods (t) t)
    (proclaim-function clos::generic-function-a-p-o-function (t) t)
    (proclaim-function clos::generic-function-argument-precedence-order (t) t)
    (proclaim-function clos::generic-function-source (t) t)
    (proclaim-function clos::generic-function-closed-p (t) t)
    (proclaim-function clos::compare-methods (t t t t) t)
    (proclaim-function clos::compare-specializers-lists (t t t) t)
    (proclaim-function clos::compare-specializers (t t t) t)
    (proclaim-function clos::class-precedence-list (t) t)
    (proclaim-function clos::fast-subtypep (t t) t)
    (proclaim-function clos::class-direct-subclasses (t) t)
    (proclaim-function clos::class-direct-slots (t) t)
    (proclaim-function clos::class-direct-default-initargs (t) t)
    (proclaim-function clos::clear-cached-make-instance (t) t)
    (proclaim-function clos::class-slot-table (t) t)
    (proclaim-function clos::class-id (t) t)
    (proclaim-function clos::class-direct-superclasses (t) t)
    (proclaim-function clos::class-finalized-p (t) t)
    (proclaim-function clos::class-size (t) t)
    (proclaim-function clos::class-cached-make-instance (t) t)
    (proclaim-function clos::class-default-initargs (t) t)
    (proclaim-function clos::class-documentation (t) t)
    (proclaim-function clos::class-source (t) t)
    (proclaim-function clos::class-version (t) t)
    (proclaim-function clos::class-optimize-slot-access (t) t)


    ;; method.lsp
    (proclaim-function clos::parse-defmethod (t) (values t t t t))
    (proclaim-function clos::parse-specialized-lambda-list (t) (values t t t))
    (proclaim-function clos::expand-defmethod (t t t t t t t t) (values t t t))
    (proclaim-function clos::walk-method-lambda (t t t) (values t t t))
    (proclaim-function clos::environment-contains-closure (t) t)
    (proclaim-function clos::legal-generic-function-name-p (t) t)
    (proclaim-function clos::class-spec-users (t) t)
    (proclaim-function clos::method-generic-function (t) t)
    (proclaim-function clos::method-plist (t) t)
    (proclaim-function clos::method-lambda-list (t) t)
    (proclaim-function clos::method-documentation (t) t)
    (proclaim-function clos::method-fun-context-setter (t) t)
    (proclaim-function clos::method-source (t) t)
    (proclaim-function clos::convert-to-implicit-generic-function-lambda-list (t) t)
    (proclaim-function clos::compute-g-f-spec-list (t) t)
    (proclaim-function clos::maybe-clear-cached-make-instance (t t) t)
    (proclaim-function clos::register-method-as-spec-user (t t) t)

    ;; slot.lsp
    (proclaim-function clos::slot-definition-initfunction (t) t)
    (proclaim-function clos::slot-definition-initargs (t) t)
    (proclaim-function clos::slot-definition-initform (t) t)
    (proclaim-function clos::slot-definition-name (t) t)
    (proclaim-function clos::slot-definition-type (t) t)
    (proclaim-function clos::slot-definition-location (t) t)
    (proclaim-function clos::slot-definition-allocation (t) t)
    (proclaim-function clos::slot-definition-readers (t) t)
    (proclaim-function clos::slot-definition-writers (t) t)
    (proclaim-function clos::slot-definition-reader-methods (t) t)
    (proclaim-function clos::slot-definition-writer-methods (t) t)
    (proclaim-function clos::slot-definition-documentation (t) t)
    (proclaim-function clos::slot-definition-to-plist (t) t)
    (proclaim-function clos::update-instance (t) t)
    (proclaim-function clos::parse-slot (t &optional t) t)
    (proclaim-function clos::make-function-initform (t) t)

    ;; combin.lsp
    (proclaim-function clos::method-p (t) t)
    (proclaim-function clos::method-function (t) t)
    (proclaim-function clos::wrapped-method-function (t) t)
    (proclaim-function clos::effective-method-function (t &optional t) t)
    (proclaim-function clos::combine-method-functions (t t) t)
    (proclaim-function clos::error-qualifier (t t) t)
    (proclaim-function clos::standard-main-effective-method (t t t) t)
    (proclaim-function clos::define-complex-method-combination (t) t)
    (proclaim-function clos::standard-compute-effective-method (t t) t)
    (proclaim-function clos::compute-applicable-methods (t t) t)
    (proclaim-function clos::generic-function-method-combination (t) t)
    (proclaim-function clos::compute-effective-method (t t t) t)

    ;; boot.lsp
    (proclaim-function clos::setf-find-class (t t &optional t t) t)
    (proclaim-function clos::make-empty-standard-class (t t) t)
    (proclaim-function clos::parse-slots (t) t)
    (proclaim-function clos::canonical-slot-to-direct-slot (t t) t)
    (proclaim-function allocate-instance (t &rest t &key &allow-other-keys) t)
    (proclaim-function clos::standard-instance-get (t t) t)
    (proclaim-function clos::standard-instance-set (t t t) t)
    (proclaim-function clos::find-effective-slot-definition (t t) t)
    (proclaim-function clos::slot-value-using-class (t t t) t)
    (proclaim-function clos::slot-boundp-using-class (t t t) t)
    (proclaim-function clos::slot-makunbound-using-class (t t t) t)
    (proclaim-function clos::slot-exists-p-using-class (t t t) t)
    (proclaim-function clos::class-prototype (t) t)

    ;; defclass.lsp
    (proclaim-function clos::parse-default-initargs (t) t)

    ;; standard.lsp
    (proclaim-function clos::valid-keywords-from-methods (t) t)
    (proclaim-function clos::check-initargs (t t t) t)
    (proclaim-function clos::safe-slot-definition-location (t &optional t) t)
    (proclaim-function clos::finalize-inheritance (t) t)
    (proclaim-function clos::valid-keywords-from-methods (t) t)
    (proclaim-function clos::add-default-initargs (t t) t)
    (proclaim-function clos::compute-make-instance-function (t) t)
    (proclaim-function clos::forward-referenced-class-p (t) t)
    (proclaim-function clos::has-forward-referenced-parents (t) t)
    (proclaim-function clos::check-direct-superclasses (t t) t)
    (proclaim-function clos::provision-shared-slot (t) t)
    (proclaim-function clos::ensure-slot-readers-writers (t) t)
    (proclaim-function clos::add-direct-subclass (t t) t)
    (proclaim-function clos::finalize-unless-forward (t) t)
    (proclaim-function clos::compute-class-precedence-list (t) t)
    (proclaim-function clos::compute-clos-class-precedence-list (t t) t)
    (proclaim-function clos::compute-slots (t) t)
    (proclaim-function clos::std-create-slots-table (t) t)
    (proclaim-function clos::std-class-generate-accessors (t) t)
    (proclaim-function clos::compute-effective-slot-definition (t t t) t)
    (proclaim-function clos::migrate-method (t t t) t)
    (proclaim-function clos::ensure-class (t &rest t) t)
    (proclaim-function clos::change-class (t t &rest t) t)
    (proclaim-function clos::redefine-subclasses (t t) t)
    (proclaim-function clos::migrate-spec-users (t t) t)
    (proclaim-function clos::coerce-to-class (t &optional t) t)
    (proclaim-function clos::compute-instance-size (t) t)
    (proclaim-function clos::class-compute-slots (t t) t)
    (proclaim-function clos::std-class-compute-slots (t t) t)
    (proclaim-function clos::unbound-slot-error (t t) t)
    (proclaim-function clos::safe-instance-ref (t t) t)
    (proclaim-function clos::std-class-optimized-local-slot-accessors (t) (values t t))
    (proclaim-function clos::std-class-optimized-accessors (t) (values t t))
    (proclaim-function clos::std-class-accessors (t) (values t t))
    (proclaim-function clos::install-method (t t t t t t t t &optional t &rest t) t)
    (proclaim-function clos::compute-default-initargs (t) t)
    (proclaim-function clos::remove-direct-subclass (t t) t)

    ;; builtin.lsp
    (proclaim-function clos::unfinalize-inheritance (t) t)
    (proclaim-function clos::refinalize-inheritance (t) t)

    ;; generic.lsp
    (proclaim-function clos::parse-defgeneric (t) (values t t t))
    (proclaim-function clos::parse-lambda-list (t &optional t) t)
    (proclaim-function clos::parse-generic-options (t t) (values t t))
    (proclaim-function clos::lambda-list-required-arguments (t) (values t t t t t t t t t t t))
    (proclaim-function clos::congruent-lambda-lists-p (t t) t)

    ;; fixup.lsp
    (proclaim-function clos::convert-one-class (t) t)
    (proclaim-function clos::unregister-method-as-spec-user (t t) t)
    (proclaim-function clos::generic-function-lock (t) t)

    ;; print.lsp
    (proclaim-function clos::need-to-make-load-form-p (t) t)
    (proclaim-function si::print-unreadable-object-function (t t t t t) t)

    ;; streams.lsp
    (proclaim-function gray::stream-line-column (t) t)
    (proclaim-function gray::stream-write-char (t t) t)
    (proclaim-function gray::bug-or-error (t t) t)
    (proclaim-function gray::stream-start-line-p (t) t)
    (proclaim-function gray::stream-terpri (t) t)
    (proclaim-function gray::stream-read-char-no-hang (t) t)
    (proclaim-function gray::stream-unread-char (t t) t)
    (proclaim-function gray::stream-read-char (t) t)

    ;; funtype.lsp
    (proclaim-function si::canonical-values-type (t) t)
    (proclaim-function si::make-function-type (&key (required t) (optional t) (rest t) (key-p t) (keywords t) (keyword-types t) (allow-other-keys-p t) (output t)) t)
    (proclaim-function si::canonical-function-type (t) t)
    (proclaim-function si::function-type-required (t) t)
    (proclaim-function si::function-type-optional (t) t)
    (proclaim-function si::function-type-rest (t) t)
    (proclaim-function si::function-type-output (t) t)
    (proclaim-function si::function-type-key-p (t) t)
    (proclaim-function si::function-type-allow-other-keys-p (t) t)
    (proclaim-function si::function-type-keywords (t) t)
    (proclaim-function si::function-type-keyword-types (t) t)
    (proclaim-function si::make-values-type (&key (min-values t) (max-values t) (required t) (optional t) (rest t)) t)
    (proclaim-function si::values-type-min-values (t) t)
    (proclaim-function si::values-type-max-values (t) t)
    (proclaim-function si::values-type-required (t) t)
    (proclaim-function si::values-type-optional (t) t)
    (proclaim-function si::values-type-rest (t) t)

    ;; pprint.lsp
    (proclaim-function si::pretty-stream-buffer-offset (t) t)
    (proclaim-function si::pretty-stream-target (t) t)
    (proclaim-function si::pretty-stream-buffer-fill-pointer (t) t)
    (proclaim-function si::pretty-stream-buffer (t) t)
    (proclaim-function si::pretty-stream-buffer-offset (t) t)
    (proclaim-function si::pretty-stream-buffer-start-column (t) t)
    (proclaim-function si::pretty-stream-blocks (t) t)
    (proclaim-function si::pretty-stream-pending-blocks (t) t)
    (proclaim-function si::pretty-stream-line-length (t) t)
    (proclaim-function si::pretty-stream-line-number (t) t)
    (proclaim-function si::pretty-stream-prefix (t) t)
    (proclaim-function si::pretty-stream-suffix (t) t)
    (proclaim-function si::pretty-stream-queue-tail (t) t)
    (proclaim-function si::pretty-stream-queue-head (t) t)
    (proclaim-function si::posn-index (t t) t)
    (proclaim-function si::posn-column (t t) t)
    (proclaim-function si::index-column (t t) t)
    (proclaim-function si::index-posn (t t) t)
    (proclaim-function si::pretty-out (t t) t)
    (proclaim-function si::enqueue-newline (t t) t)
    (proclaim-function si::assure-space-in-buffer (t t) t)
    (proclaim-function si::pretty-sout (t t t t) t)
    (proclaim-function si::logical-block-per-line-prefix-end (t) t)
    (proclaim-function si::logical-block-start-column (t) t)
    (proclaim-function si::logical-block-section-column (t) t)
    (proclaim-function si::logical-block-prefix-length (t) t)
    (proclaim-function si::logical-block-suffix-length (t) t)
    (proclaim-function si::logical-block-section-start-line (t) t)
    (proclaim-function si::make-logical-block (&key (start-column t) (section-column t) (per-line-prefix-end t) (prefix-length t) (suffix-length t) (section-start-line t)) t)
    (proclaim-function si::set-indentation (t t) t)
    (proclaim-function si::make-newline (&key (kind t) (depth t) (section-end t) (posn t)) t)
    (proclaim-function si::section-start-p (t) t)
    (proclaim-function si::section-start-section-end (t) t)
    (proclaim-function si::section-start-depth (t) t)
    (proclaim-function si::make-newline (&key (kind t) (depth t) (section-end t) (posn t)) t)
    (proclaim-function si::newline-kind (t) t)
    (proclaim-function si::newline-posn (t) t)
    (proclaim-function si::newline-section-end (t) t)
    (proclaim-function si::maybe-output (t t) t)
    (proclaim-function si::make-indentation (&key (kind t) (amount t) (posn t)) t)
    (proclaim-function si::indentation-kind (t) t)
    (proclaim-function si::indentation-posn (t) t)
    (proclaim-function si::indentation-amount (t) t)
    (proclaim-function si::make-block-start (&key (block-end t) (prefix t) (suffix t) (depth t) (section-end t) (posn t)) t)
    (proclaim-function si::block-start-suffix (t) t)
    (proclaim-function si::block-start-section-end (t) t)
    (proclaim-function si::block-start-posn (t) t)
    (proclaim-function si::block-start-prefix (t) t)
    (proclaim-function si::block-start-block-end (t) t)
    (proclaim-function si::make-block-end (&key (suffix t) (posn t)) t)
    (proclaim-function si::make-tab (&key (sectionp t) (relativep t) (colnum t) (colinc t) (posn t)) t)
    (proclaim-function si::tab-posn (t) t)
    (proclaim-function si::tab-colnum (t) t)
    (proclaim-function si::tab-colinc (t) t)
    (proclaim-function si::tab-sectionp (t) t)
    (proclaim-function si::tab-relativep (t) t)
    (proclaim-function si::queued-op-posn (t) t)
    (proclaim-function si::compute-tab-size (t t t) t)
    (proclaim-function si::output-partial-line (t) t)
    (proclaim-function si::assure-space-in-buffer (t t) t)
    (proclaim-function si::misering-p (t) t)
    (proclaim-function si::fits-on-line-p (t t t) t)
    (proclaim-function si::output-line (t t) t)
    (proclaim-function si::really-start-logical-block (t t t t) t)
    (proclaim-function si::really-end-logical-block (t) t)
    (proclaim-function si::expand-tabs (t t) t)
    (proclaim-function si::search-print-circle (t) t)
    (proclaim-function si::make-pretty-stream (t) t)
    (proclaim-function si::do-pprint-logical-block (t t t t t t) t)
    (proclaim-function si::start-logical-block (t t t t) t)
    (proclaim-function si::end-logical-block (t) t)
    (proclaim-function si::force-pretty-output (t) t)
    (proclaim-function si::enqueue-indent (t t t) t)
    (proclaim-function si::enqueue-tab (t t t t) t)
    (proclaim-function si::make-pprint-dispatch-entry (&key (type t) (priority t) (initial-p t) (function t)) t)
    (proclaim-function si::pprint-dispatch-entry-type (t) t)
    (proclaim-function si::pprint-dispatch-entry-priority (t) t)
    (proclaim-function si::pprint-dispatch-entry-initial-p (t) t)
    (proclaim-function si::pprint-dispatch-entry-function (t) t)
    (proclaim-function si::pprint-dispatch-table-entries (t) t)
    (proclaim-function si::pprint-dispatch-table-cons-entries (t) t)
    (proclaim-function si::make-pprint-dispatch-table (&key (entries t) (cons-entries t)) t)
    (proclaim-function si::entry< (t t) t)
    (proclaim-function si::cons-type-specifier-p (t) t)
    (proclaim-function si::pprint-raw-array (t t) t)
    (proclaim-function si::pprint-vector (t t) t)
    (proclaim-function si::pprint-multi-dim-array (t t) t)
    (proclaim-function si::pprint-array-contents (t t) t)
    (proclaim-function si::pprint-lambda-list (t t &rest t) t)

    ;; conditions.lsp
    (proclaim-function si::restart-report (t t) t)
    (proclaim-function si::restart-name (t) t)
    (proclaim-function si::restart-function (t) t)
    (proclaim-function si::restart-report-function (t) t)
    (proclaim-function si::restart-interactive-function (t) t)
    (proclaim-function si::restart-test-function (t) t)
    (proclaim-function si::restart-p (t) t)
    (proclaim-function si::find-restart-never-fail (t &optional t) t)
    (proclaim-function si::find-subclasses-of-type (t t) t)
    (proclaim-function mkcl:segmentation-violation-address (t) t)
    (proclaim-function mkcl:stack-overflow-size (t) t)
    (proclaim-function mkcl:stack-overflow-type (t) t)
    (proclaim-function si::case-failure-name (t) t)
    (proclaim-function si::case-failure-possibilities (t) t)
    (proclaim-function si::invalid-slot-instance (t) t)
    (proclaim-function si::format-error-print-banner (t) t)
    (proclaim-function si::format-error-control-string (t) t)
    (proclaim-function si::format-error-offset (t) t)
    (proclaim-function si::bad-fasl-file-reason (t) t)
    (proclaim-function si::try-to-invoke-debugger (t) t)

    ;; mp-cond.lsp
    (proclaim-function mt::interrupt-error-thread (t) t)
    (proclaim-function mt::thread-sleeping-in-file (t) t)
    (proclaim-function mt::thread-sleeping-on-lineno (t) t)
    (proclaim-function mt::interrupt-disabler-file (t) t)
    (proclaim-function mt::interrupt-disabler-lineno (t) t)

    ;; uni-cond.lsp
    (proclaim-function si::character-coding-error-external-format (t) t)
    (proclaim-function si::character-encoding-error-codepoint (t) t)
    (proclaim-function si::character-decoding-error-octets (t) t)
    (proclaim-function si::standard-replacement-character-for (t) t)

    ;; module.lsp
    (proclaim-function si::require-error (t &rest t) t)

    ;; describe.lsp
    (proclaim-function si::inspect-object (t) t)
    (proclaim-function si::select-P (t) t)
    (proclaim-function si::select-E (t) t)
    (proclaim-function si::select-U () t)
    (proclaim-function si::select-? () t)
    (proclaim-function si::select-ht-N (t) t)
    (proclaim-function si::select-ht-L (t) t)
    (proclaim-function si::select-ht-J (t) t)
    (proclaim-function si::read-inspect-command (t t t) t)
    (proclaim-function clos::inspect-obj (t) t)
    (proclaim-function si::inspect-symbol (t) t)
    (proclaim-function si::inspect-package (t) t)
    (proclaim-function si::inspect-character (t) t)
    (proclaim-function si::inspect-number (t) t)
    (proclaim-function si::inspect-cons (t) t)
    (proclaim-function si::inspect-utf-8 (t) t)
    (proclaim-function si::inspect-utf-16 (t) t)
    (proclaim-function si::inspect-string (t) t)
    (proclaim-function si::inspect-vector (t) t)
    (proclaim-function si::inspect-array (t) t)
    (proclaim-function si::inspect-hashtable (t) t)
    (proclaim-function si::inspect-instance (t) t)
    (proclaim-function si::inspect-function (t) t)
    (proclaim-function si::inspect-pathname (t) t)
    (proclaim-function si::inspect-symbol (t) t)
    (proclaim-function si::get-documentation (t t) t)

    ;; inspect.lsp
    (proclaim-function clos::class-local-slots (t) t)
    (proclaim-function clos::class-shared-slots (t) t)
    (proclaim-function clos::select-clos-N (t) t)
    (proclaim-function clos::select-clos-L (t) t)
    (proclaim-function clos::select-clos-J (t) t)

    ;; top.lsp
    (proclaim-function si::terminal-interrupt (t) t)
    (proclaim-function si::grab-console (&key (wait t)) t)
    (proclaim-function si::ihs-search (t t &optional t) t)
    (proclaim-function si::decode-ihs-env (t) t)
    (proclaim-function si::tpl-print (t) t)
    (proclaim-function si::tpl-make-command (t t) t)
    (proclaim-function si::tpl-parse-forms (t &optional t) t)
    (proclaim-function si::tpl-parse-strings (t) t)
    (proclaim-function si::harden-command (t) t)
    (proclaim-function si::ihs-visible (t) t)
    (proclaim-function si::function-lambda-list (t) t)
    (proclaim-function si::reconstruct-bytecode-lambda-list (t) t)
    (proclaim-function si::dynamic-binding-value (t &optional t) t)
    (proclaim-function si::print-ihs (t) t)
    (proclaim-function si::print-frs (t) t)
    (proclaim-function si::print-ihs-line (t) t)
    (proclaim-function si::ihs-fname (t) t)
    (proclaim-function si::get-fname (t) t)
    (proclaim-function mkcl::help (&optional t) t)
    (proclaim-function si::step* (t) t)
    (proclaim-function si::trace* (t) t)
    (proclaim-function si::untrace* (t) t)
    (proclaim-function si::register-on-debugger-waiting-list (t) t)
    (proclaim-function si::remove-from-debugger-waiting-list (t) t)
    (proclaim-function si::compute-restart-commands (t &key (display t)) t)
    (proclaim-function si::update-debug-commands (t) t)
    (proclaim-function si::default-debugger (t) t)


    (proclaim-function si::every* (t &rest t) t) ;; seq.lsp

    (proclaim-function si::coerce-to-condition (t t t t) t)
    (proclaim-function si::make-restart (&key (name t) (function t) (report-function t) (interactive-function t) (test-function t)) t)

    (proclaim-function si::function-type-p (t) t) ;; funtype.lsp
    (proclaim-function si::values-type-p (t) t) ;; funtype.lsp
    )
  )

(dolist (opti-spec +all-optimizers+)
  ;; proclaim-function and def-inline are evaluated
  ;; for their side-effects here and now!
  (eval opti-spec))

(dolist (proclaim-spec +mkcl-min-internals+)
  (eval proclaim-spec))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; FUNCTIONS IMPLEMENTED IN LISP WHICH CAN BE CALLED DIRECTLY FROM C
;;;
;;; The following two lists contain all functions in the core library which are
;;; not implemented in the C part of the library, but which should have an exported C
;;; name that users (and compiled code) can refer to. This means, for instance, that
;;; SI::ECASE-ERROR will be compiled to a function called mk_si_8_ECASE__ERROR, etc.
;;;

(proclaim
 `(MKCL:C-EXPORT-FNAME
   (cl:acos "mk_cl_acos")
   (cl:acosh "mk_cl_acosh")
   (cl:adjust-array "mk_cl_adjust_array")
   (cl:apropos "mk_cl_apropos")
   (cl:apropos-list "mk_cl_apropos_list")
   (cl:array-dimensions "mk_cl_array_dimensions")
   (cl:array-in-bounds-p "mk_cl_array_in_bounds_p")
   (cl:asin "mk_cl_asin")
   (cl:asinh "mk_cl_asinh")
   (cl:assoc-if "mk_cl_assoc_if")
   (cl:assoc-if-not "mk_cl_assoc_if_not")
   (cl:atanh "mk_cl_atanh")
   (cl:bit "mk_cl_bit")
   (cl:bit-and "mk_cl_bit_and")
   (cl:bit-andc1 "mk_cl_bit_andc1")
   (cl:bit-andc2 "mk_cl_bit_andc2")
   (cl:bit-eqv "mk_cl_bit_eqv")
   (cl:bit-ior "mk_cl_bit_ior")
   (cl:bit-nand "mk_cl_bit_nand")
   (cl:bit-nor "mk_cl_bit_nor")
   (cl:bit-not "mk_cl_bit_not")
   (cl:bit-orc1 "mk_cl_bit_orc1")
   (cl:bit-orc2 "mk_cl_bit_orc2")
   (cl:bit-xor "mk_cl_bit_xor")
   (cl:byte "mk_cl_byte")
   (cl:byte-position "mk_cl_byte_position")
   (cl:byte-size "mk_cl_byte_size")
   (cl:cerror "mk_cl_cerror")
   (cl:cis "mk_cl_cis")
   (cl:coerce "mk_cl_coerce")
   (cl:complement "mk_cl_complement")
   (cl:concatenate "mk_cl_concatenate")
   (cl:continue "mk_cl_continue")
   (cl:constantly "mk_cl_constantly")
   (cl:copy-pprint-dispatch "mk_cl_copy_pprint_dispatch")
   (cl:count "mk_cl_count")
   (cl:count-if "mk_cl_count_if")
   (cl:count-if-not "mk_cl_count_if_not")
   (cl:decode-universal-time "mk_cl_decode_universal_time")
   (cl:delete "mk_cl_delete")
   (cl:delete-duplicates "mk_cl_delete_duplicates")
   (cl:delete-if "mk_cl_delete_if")
   (cl:delete-if-not "mk_cl_delete_if_not")
   (cl:deposit-field "mk_cl_deposit_field")
   (cl:dpb "mk_cl_dpb")
   (cl:encode-universal-time "mk_cl_encode_universal_time")
   (cl:ensure-directories-exist "mk_cl_ensure_directories_exist")
   (cl:every "mk_cl_every")
   (cl:fceiling "mk_cl_fceiling")
   (cl:ffloor "mk_cl_ffloor")
   (cl:fill "mk_cl_fill")
   (cl:find "mk_cl_find")
   (cl:find-all-symbols "mk_cl_find_all_symbols")
   (cl:find-if "mk_cl_find_if")
   (cl:find-if-not "mk_cl_find_if_not")
   (cl:fround "mk_cl_fround")
   (cl:ftruncate "mk_cl_ftruncate")
   (cl:get-decoded-time "mk_cl_get_decoded_time")
   (cl:intersection "mk_cl_intersection")
   (cl:isqrt "mk_cl_isqrt")
   (cl:ldb "mk_cl_ldb")
   (cl:ldb-test "mk_cl_ldb_test")
   (cl:load-logical-pathname-translations "mk_cl_load_logical_pathname_translations")
   (cl:logical-pathname-translations "mk_cl_logical_pathname_translations")
   (cl:logtest "mk_cl_logtest")
   (cl:make-array "mk_cl_make_array")
   (cl:make-sequence "mk_cl_make_sequence")
   (cl:map "mk_cl_map")
   (cl:map-into "mk_cl_map_into")
   (cl:mask-field "mk_cl_mask_field")
   (cl:member-if "mk_cl_member_if")
   (cl:member-if-not "mk_cl_member_if_not")
   (cl:merge "mk_cl_merge")
   (cl:mismatch "mk_cl_mismatch")
   (cl:nintersection "mk_cl_nintersection")
   (cl:notany "mk_cl_notany")
   (cl:notevery "mk_cl_notevery")
   (cl:nset-difference "mk_cl_nset_difference")
   (cl:nset-exclusive-or "mk_cl_nset_exclusive_or")
   (cl:nsubst-if "mk_cl_nsubst_if")
   (cl:nsubst-if-not "mk_cl_nsubst_if_not")
   (cl:nsubstitute "mk_cl_nsubstitute")
   (cl:nsubstitute-if "mk_cl_nsubstitute_if")
   (cl:nsubstitute-if-not "mk_cl_nsubstitute_if_not")
   (cl:nunion "mk_cl_nunion")
   (cl:phase "mk_cl_phase")
   (cl:position "mk_cl_position")
   (cl:position-if "mk_cl_position_if")
   (cl:position-if-not "mk_cl_position_if_not")
   (cl:pprint-dispatch "mk_cl_pprint_dispatch")
   (cl:pprint-fill "mk_cl_pprint_fill")
   (cl:pprint-indent "mk_cl_pprint_indent")
   (cl:pprint-linear "mk_cl_pprint_linear")
   (cl:pprint-newline "mk_cl_pprint_newline")
   (cl:pprint-tab "mk_cl_pprint_tab")
   (cl:pprint-tabular "mk_cl_pprint_tabular")
   (cl:prin1-to-string "mk_cl_prin1_to_string")
   (cl:princ-to-string "mk_cl_princ_to_string")
   (cl:provide "mk_cl_provide")
   (cl:rassoc-if "mk_cl_rassoc_if")
   (cl:rassoc-if-not "mk_cl_rassoc_if_not")
   (cl:read-from-string "mk_cl_read_from_string")
   (cl:reduce "mk_cl_reduce")
   (cl:remove "mk_cl_remove")
   (cl:remove-duplicates "mk_cl_remove_duplicates")
   (cl:remove-if "mk_cl_remove_if")
   (cl:remove-if-not "mk_cl_remove_if_not")
   (cl:replace "mk_cl_replace")
   (cl:require "mk_cl_require")
   (cl:sbit "mk_cl_sbit")
   (cl:search "mk_cl_search")
   (cl:set-difference "mk_cl_set_difference")
   (cl:set-exclusive-or "mk_cl_set_exclusive_or")
   (cl:set-pprint-dispatch "mk_cl_set_pprint_dispatch")
   (cl:signum "mk_cl_signum")
   (cl:some "mk_cl_some")
   (cl:sort "mk_cl_sort")
   (cl:stable-sort "mk_cl_stable_sort")
   (cl:subsetp "mk_cl_subsetp")
   (cl:subst-if "mk_cl_subst_if")
   (cl:subst-if-not "mk_cl_subst_if_not")
   (cl:substitute "mk_cl_substitute")
   (cl:substitute-if "mk_cl_substitute_if")
   (cl:substitute-if-not "mk_cl_substitute_if_not")
   (cl:subtypep "mk_cl_subtypep")
   (cl:typep "mk_cl_typep")
   (cl:union "mk_cl_union")
   (cl:upgraded-array-element-type "mk_cl_upgraded_array_element_type")
   (cl:upgraded-complex-part-type "mk_cl_upgraded_complex_part_type")
   (cl:vector "mk_cl_vector")
   (cl:vector-pop "mk_cl_vector_pop")
   (cl:vector-push "mk_cl_vector_push")
   (cl:vector-push-extend "mk_cl_vector_push_extend")
   (cl:write-to-string "mk_cl_write_to_string")
   (cl:y-or-n-p "mk_cl_y_or_n_p")
   (cl:yes-or-no-p "mk_cl_yes_or_no_p")
   (cl:invalid-method-error "mk_cl_invalid_method_error")
   (cl:method-combination-error "mk_cl_method_combination_error")
   (si:safe-eval "mk_si_safe_eval")
   (si:subclassp "mk_si_subclassp")
   (si:of-class-p "mk_si_of_class_p")
   (si:do-deftype "mk_si_do_deftype")
   (si:find-relative-package "mk_si_find_relative_package")
   (si:package-parent "mk_si_package_parent")
   (si:package-children "mk_si_package_children")
   (si:top-apply "mk_si_top_apply")
   (mkcl:prin1-to-base-string "mk_mkcl_prin1_to_base_string")
   (mkcl:princ-to-base-string "mk_mkcl_princ_to_base_string")
   (mkcl:write-to-base-string "mk_mkcl_write_to_base_string")
   (si:mkcl-version "mk_si_mkcl_version")
   (si:mkcl-major-version "mk_si_mkcl_major_version")
   (si:mkcl-minor-version "mk_si_mkcl_minor_version")
   (si:mkcl-patch-level "mk_si_mkcl_patch_level")
   (mkcl:run-program "mk_mkcl_run_program")
   (si:shutdown-mkcl-threads "mk_si_shutdown_mkcl_threads")
   )
 )

#+(or) ;; commented out until I receive valid motivation to do otherwise. JCB 2021/04/03.
(proclaim
 `(MKCL:C-EXPORT-FNAME
   si::ecase-error si::etypecase-error si::do-check-type
   si::ccase-error si::find-documentation si::find-declarations
   si::search-keyword si::check-keyword si::check-arg-length
   si::dm-too-few-arguments si::dm-bad-key
   si::remove-documentation si::get-documentation
   si::set-documentation si::expand-set-documentation
   si::packages-iterator
   si::pprint-logical-block-helper si::pprint-pop-helper
   si::make-seq-iterator si::seq-iterator-ref si::seq-iterator-set si::seq-iterator-next
   si::structure-type-error si::define-structure
   ;;si::coerce-to-list si::coerce-to-vector
   si::fill-array-with-seq
   si::format-princ
   si::format-prin1
   si::format-print-named-character
   si::format-print-integer
   si::format-print-cardinal si::format-print-ordinal si::format-print-old-roman
   si::format-print-roman si::format-fixed si::format-exponential
   si::format-general si::format-dollars
   si::format-relative-tab si::format-absolute-tab
   si::format-justification
   clos::ensure-class
   ;; boot.lsp
   clos::slot-boundp
   clos::slot-makunbound
   clos::slot-value
   clos::slot-exists-p
   clos::standard-instance-set
   ;; standard.lsp
   clos::safe-instance-ref
   ;; kernel.lsp
   clos::install-method
   clos::class-id
   clos::class-direct-superclasses
   clos::class-direct-subclasses
   clos::class-slots
   clos::class-precedence-list
   clos::class-direct-slots
   clos::generic-function-lambda-list
   clos::generic-function-argument-precedence-order
   clos::generic-function-method-combination
   clos::generic-function-method-class
   clos::generic-function-methods
   clos::method-generic-function
   clos::method-lambda-list
   clos::method-specializers
   clos::method-qualifiers
   clos::method-function
   clos::method-plist
   clos::associate-methods-to-gfun
   ))

;;

(provide "cmp")
(provide "CMP")

#-:mkcl-cmp-bootstrap
(si::close-package "COMPILER")

