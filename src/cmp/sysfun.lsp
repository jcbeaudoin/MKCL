;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: C -*-
;;;;
;;;  CMPSYSFUN   Database for system functions.
;;;
;;;  Copyright (c) 2010-2017, Jean-Claude Beaudoin.
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
  (unless (equal arg-types '(*))
    (put-sysprop name 'SI::PROCLAIMED-ARG-TYPES
		 (mapcar #'(lambda (x) 
			     (if (member x '(* &optional &rest &key &allow-other-keys))
				 x 
			       (si::type-filter x)))
			 arg-types)))
  (when (and return-type (not (eq 'T return-type)))
    (put-sysprop name 'SI::PROCLAIMED-RETURN-TYPE
		 (if (eql return-type '*) '* (si::type-filter return-type t))))
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
    (proclaim-function si:make-pure-array (*) array)
    (proclaim-function si:make-vector (*) vector)
    (proclaim-function aref (array *) t :no-side-effects t)

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

    (proclaim-function si:aset (t array *) t)

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


    (proclaim-function array-row-major-index (array *) fixnum :no-side-effects t)

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

    (proclaim-function array-has-fill-pointer-p (*) t :predicate t)
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

    (proclaim-function si:replace-array (*) t)

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
    ;;(proclaim-function fmakunbound (symbol) t)
    (proclaim-function si:clear-compiler-properties (*) t)

    ;; file cfun.d

    (proclaim-function si:compiled-function-name (*) t)

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

    (proclaim-function digit-char-p (character *) t :no-side-effects t)

    (proclaim-function alphanumericp (character) t :predicate t :no-side-effects t)
    (def-inline alphanumericp :always (character) :bool "mkcl_alphanumericp(#0)")

    (proclaim-function character (t) character)
    (proclaim-function char= (character *) t :predicate t :no-side-effects t)
    (def-inline char= :always (t t) :bool "mkcl_char_code(env, #0)==mkcl_char_code(env, #1)")
    (def-inline char= :always (character character) :bool "(#0)==(#1)")

    (proclaim-function char/= (character *) t :predicate t :no-side-effects t)
    (def-inline char/= :always (t t) :bool "mkcl_char_code(env, #0)!=mkcl_char_code(env, #1)")
    (def-inline char/= :always (character character) :bool "(#0)!=(#1)")

    (proclaim-function char< (character *) t :predicate t :no-side-effects t)
    (def-inline char< :always (character character) :bool "(#0)<(#1)")

    (proclaim-function char> (character *) t :predicate t :no-side-effects t)
    (def-inline char> :always (character character) :bool "(#0)>(#1)")

    (proclaim-function char<= (character *) t :predicate t :no-side-effects t)
    (def-inline char<= :always (character character) :bool "(#0)<=(#1)")

    (proclaim-function char>= (character *) t :predicate t :no-side-effects t)
    (def-inline char>= :always (character character) :bool "(#0)>=(#1)")

    (proclaim-function char-equal (character *) t :predicate t)
    (proclaim-function char-not-equal (character *) t :predicate t)
    (proclaim-function char-lessp (character *) t :predicate t)
    (proclaim-function char-greaterp (character *) t :predicate t)
    (proclaim-function char-not-greaterp (character *) t :predicate t)
    (proclaim-function char-not-lessp (character *) t :predicate t)
    (proclaim-function character (*) character)
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

    (proclaim-function digit-char (fixnum *) (or character null))
    (proclaim-function char-int (character) fixnum :no-side-effects t)
    (def-inline char-int :always (character) t "(MKCL_MAKE_FIXNUM(#0))")
    (def-inline char-int :always (character) :fixnum "(#0)")

    (proclaim-function char-name (character) (or string null))
    (proclaim-function name-char (string) (or character null))

    ;; file error.d

    (proclaim-function error (t *) t)
    (proclaim-function cerror (t t *) t)

    ;; file stacks.d

    (proclaim-function si:disable-interrupts () t)
    (def-inline si:disable-interrupts :always nil t
      "((env->disable_interrupts) ? mk_cl_Cnil : ((env->disable_interrupts = 1), mk_cl_Ct))")

    (proclaim-function si:enable-interrupts () t)
    (def-inline si:enable-interrupts :always nil t "(env->disable_interrupts = 0)")

    (proclaim-function si:ihs-top () t)
    (proclaim-function si:ihs-fun (*) t)
    (proclaim-function si:ihs-env (*) t)
    (proclaim-function si:frs-top (*) t)
    (proclaim-function si:frs-bds (*) t)
    (proclaim-function si:frs-tag (*) t)
    (proclaim-function si:frs-ihs (*) t)
    (proclaim-function si:bds-top (*) t)
    (proclaim-function si:bds-var (*) t)
    (proclaim-function si:bds-val (*) t)
    (proclaim-function si:sch-frs-base (*) t)

    ;; file eval.d

    (proclaim-function apply (t t *) t)
    (proclaim-function funcall (t *) t)
    (proclaim-function eval (t) t)
    (proclaim-function evalhook (t t t *) t)
    (proclaim-function applyhook (t t t t *) t)
    (proclaim-function constantp (t &optional t) t :predicate t)

    ;; file file.d

    (proclaim-function make-synonym-stream (symbol) synonym-stream)
    (proclaim-function make-broadcast-stream (*) broadcast-stream)
    (proclaim-function make-concatenated-stream (*) concatenated-stream)
    (proclaim-function make-two-way-stream (stream stream) two-way-stream)
    (proclaim-function make-echo-stream (stream stream) echo-stream)
    (proclaim-function make-string-input-stream (*) string-stream)
    (proclaim-function make-string-output-stream (*) string-stream)

    (proclaim-function get-output-stream-string (string-stream) string)
    (proclaim-function streamp (t) t :predicate t)
    (proclaim-function input-stream-p (stream) t :predicate t)
    (def-inline input-stream-p :always (stream) :bool "mkcl_input_stream_p(env, #0)")

    (proclaim-function output-stream-p (stream) t :predicate t)
    (def-inline output-stream-p :always (stream) :bool "mkcl_output_stream_p(env, #0)")

    (proclaim-function stream-element-type (t) t)
    (proclaim-function close (stream *) t)
    (proclaim-function file-position (stream *) t)
    (proclaim-function file-length (stream) t)
    (proclaim-function si:make-string-output-stream-from-string (string (or symbol cons)) string-stream)

    ;; file unixfsys.d

    (proclaim-function truename (t) t)
    (proclaim-function rename-file (t t) t)
    (proclaim-function si:specialp (t) t :predicate t)
    (proclaim-function delete-file (t) t)
    (proclaim-function probe-file (t) t)
    (proclaim-function file-write-date (t) t)
    (proclaim-function file-author (t) t)
    (proclaim-function pathname (t) t)
    (proclaim-function user-homedir-pathname (*) t)
    (proclaim-function directory (t *) t)
    (proclaim-function mkcl:chdir (t *) pathname)
    (proclaim-function mkcl:getcwd (*) pathname)
    (proclaim-function mkcl:mkdir (t fixnum) string)

    ;; file unixint.d

    ;; file format.d

    (proclaim-function format (t string *) t)

    ;; file hash.d

    (proclaim-function make-hash-table (*) t)
    (proclaim-function hash-table-p (t) t :predicate t)
    (proclaim-function values (*) *)
    (proclaim-function gethash (t t *) (values t t))
    (proclaim-function remhash (t t) t)
    (proclaim-function maphash (t t) t)
    (proclaim-function clrhash (t) t)
    (proclaim-function hash-table-count (t) si::index)
    (proclaim-function sxhash (t) fixnum)
    (proclaim-function si:hash-set (*) t)

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

    (proclaim-function tree-equal (t t *) t :predicate t)
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

    (proclaim-function list (*) list :no-side-effects t)
    (def-inline list :always nil t "mk_cl_Cnil")
    (def-inline list :always (t) t "mkcl_list1(env, #0)")

    (proclaim-function list* (t *) list :no-side-effects t)
    (def-inline list* :always nil t "mk_cl_Cnil")
    (def-inline list* :always (t) t "(#0)")
    (def-inline list* :always (t t) t "MKCL_CONS(env, #0,#1)")

    (proclaim-function make-list (fixnum *) list)
    (proclaim-function append (*) list :no-side-effects t)
    (def-inline append :always (t t) t "mkcl_append(env, #0,#1)")

    (proclaim-function copy-list (list) list)
    (proclaim-function copy-alist (list) list)
    (proclaim-function copy-tree (t) t)
    (proclaim-function revappend (list t) t)
    (proclaim-function nconc (*) t)
    (def-inline nconc :always (t t) t "mkcl_nconc(env, #0,#1)")

    (proclaim-function nreconc (list t) t)
    (proclaim-function butlast (list *) list)
    (def-inline butlast :always (t) t "mkcl_butlast(env, #0,1)")
    (proclaim-function nbutlast (list *) list)
    (def-inline nbutlast :always (t) t "mkcl_nbutlast(env, #0,1)")
    (proclaim-function ldiff (list t) list)
    (proclaim-function rplaca (cons t) cons)
    (def-inline rplaca :always (t t) t
      "@0;(MKCL_ATOM(#0) ? (mkcl_FEtype_error_cons(env, #0), (#0)) : (MKCL_RPLACA((#0), (#1)), (#0)))")
    (proclaim-function rplacd (cons t) cons)
    (def-inline rplacd :always (t t) t
      "@0;(MKCL_ATOM(#0) ? (mkcl_FEtype_error_cons(env, #0), (#0)) : (MKCL_RPLACD((#0), (#1)), (#0)))")
    (proclaim-function subst (t t t *) t)
    (proclaim-function subst-if (t t t *) t)
    (proclaim-function subst-if-not (t t t *) t)
    (proclaim-function nsubst (t t t *) t)
    (proclaim-function nsubst-if (t t t *) t)
    (proclaim-function nsubst-if-not (t t t *) t)
    (proclaim-function sublis (list t *) t)
    (proclaim-function nsublis (list t *) t)
    (proclaim-function member (t list *) list)
    (proclaim-function member-if (t list *) list)
    (proclaim-function member-if-not (t list *) list)
    (proclaim-function member1 (t t t t t) t)
    (proclaim-function tailp (t list) t :predicate t)
    (proclaim-function adjoin (t list *) list)
    (proclaim-function acons (t t list) list)
    (def-inline acons :always (t t t) t "MKCL_CONS(env, MKCL_CONS(env, (#0), (#1)), (#2))")
    (proclaim-function pairlis (list list *) list)
    (proclaim-function assoc (t list *) list)
    (proclaim-function assoc-if (t list &key t) list)
    (proclaim-function assoc-if-not (t list &key t) list)
    (proclaim-function rassoc (t list &key t) list)
    (proclaim-function rassoc-if (t list &key t) list)
    (proclaim-function rassoc-if-not (t list &key t) list)
    (proclaim-function si:memq (t t) t)

    ;; file macros.d

    (proclaim-function macroexpand (t *) (values t t))
    (proclaim-function macroexpand-1 (t *) (values t t))

    ;; file main.d

    (proclaim-function mkcl:quit (*) t)
    (proclaim-function identity (t) t)
    (proclaim-function mkcl:argc () t)
    (proclaim-function mkcl:argv (t) t)
    (proclaim-function mkcl:getenv (t) t)
    (proclaim-function si:pointer (t) t)

    ;; file mapfun.d

    (proclaim-function mapcar (t t *) t)
    (proclaim-function maplist (t t *) t)
    (proclaim-function mapc (t t *) t)
    (proclaim-function mapl (t t *) t)
    (proclaim-function mapcan (t t *) t)
    (proclaim-function mapcon (t t *) t)

    ;; file multival.d

    (proclaim-function values (*) t)
    (proclaim-function values-list (t) *)

    ;; file num_arith.d

    (proclaim-function + (*) t :no-side-effects t)
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

    (proclaim-function - (t *) t :no-side-effects t)
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

    (proclaim-function * (*) t :no-side-effects t)
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

    (proclaim-function / (t *) t :no-side-effects t)
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
    (proclaim-function gcd (*) t)
    (proclaim-function lcm (t *) t)

    ;; file num_co.d

    (proclaim-function float (number *) float :no-side-effects t)
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
    (proclaim-function float-sign (t *) t)
    (proclaim-function float-digits (t) fixnum)
    (proclaim-function float-precision (t) fixnum)
    (proclaim-function integer-decode-float (t) (values t t t))
    (proclaim-function complex (t *) t)
    (proclaim-function realpart (t) t)
    (proclaim-function imagpart (t) t)
    (proclaim-function = (t *) t :predicate t :no-side-effects t)
    (def-inline = :always (t t) :bool "mkcl_number_equalp(env, #0,#1)")
    (def-inline = :always (fixnum-float fixnum-float) :bool "((#0)==(#1))")

    (proclaim-function /= (t *) t :predicate t :no-side-effects t)
    (def-inline /= :always (t t) :bool "!mkcl_number_equalp(env, #0,#1)")
    (def-inline /= :always (fixnum-float fixnum-float) :bool "((#0)!=(#1))")

    (proclaim-function < (t *) t :predicate t :no-side-effects t)
    (def-inline < :always (t t) :bool "(mkcl_number_compare(env, #0,#1)<0)")
    (def-inline < :always (fixnum-float fixnum-float) :bool "((#0)<(#1))")

    (proclaim-function > (t *) t :predicate t :no-side-effects t)
    (def-inline > :always (t t) :bool "(mkcl_number_compare(env, #0,#1)>0)")
    (def-inline > :always (fixnum-float fixnum-float) :bool "((#0)>(#1))")

    (proclaim-function <= (t *) t :predicate t :no-side-effects t)
    (def-inline <= :always (t t) :bool "(mkcl_number_compare(env, #0,#1)<=0)")
    (def-inline <= :always (fixnum-float fixnum-float) :bool "((#0)<=(#1))")

    (proclaim-function >= (t *) t :predicate t :no-side-effects t)
    (def-inline >= :always (t t) :bool "(mkcl_number_compare(env, #0,#1)>=0)")
    (def-inline >= :always (fixnum-float fixnum-float) :bool "((#0)>=(#1))")

    (proclaim-function max (t *) t :no-side-effects t)
    (def-inline max :always (t t) t "@01;(mkcl_number_compare(env, #0,#1)>=0?(#0):(#1))")
    (def-inline max :always (fixnum fixnum) t "@01;MKCL_MAKE_FIXNUM((#0)>=(#1)?(#0):(#1))")
    (def-inline max :always (fixnum fixnum) :fixnum "@01;((#0)>=(#1)?(#0):(#1))")

    (proclaim-function min (t *) t :no-side-effects t)
    (def-inline min :always (t t) t "@01;(mkcl_number_compare(env, #0,#1)<=0?(#0):(#1))")
    (def-inline min :always (fixnum fixnum) t "@01;MKCL_MAKE_FIXNUM((#0)<=(#1)?(#0):(#1))")
    (def-inline min :always (fixnum fixnum) :fixnum "@01;((#0)<=(#1)?(#0):(#1))")

    ;; file num_log.d

    (proclaim-function logand (*) integer :no-side-effects t)
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

    (proclaim-function logeqv (*) integer :no-side-effects t)
    (def-inline logeqv :always nil t "MKCL_MAKE_FIXNUM(-1)")
    (def-inline logeqv :always nil :fixnum "(-1)")
    (def-inline logeqv :always (t t) t "mkcl_boole(env, MKCL_BOOLEQV,(#0),(#1))")
    (def-inline logeqv :always (fixnum fixnum) t "MKCL_MAKE_FIXNUM(~( (#0) ^ (#1) ))")
    (def-inline logeqv :always (fixnum fixnum) :fixnum "(~( (#0) ^ (#1) ))")

    (proclaim-function logior (*) integer :no-side-effects t)
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

    (proclaim-function logxor (*) integer :no-side-effects t)
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

    (proclaim-function si:bit-array-op (*) t)
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

    (proclaim-function random (t *) t)
    (proclaim-function make-random-state (*) t)
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

    (proclaim-function log (number *) number :no-side-effects t)
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

    (proclaim-function make-package (t *) t)
    (proclaim-function si:select-package (t) t)
    (proclaim-function find-package (t) t)
    (proclaim-function package-name (t) t)
    (proclaim-function package-nicknames (t) t)
    (proclaim-function rename-package (t t *) t)
    (proclaim-function package-use-list (t) t)
    (proclaim-function package-used-by-list (t) t)
    (proclaim-function package-shadowing-symbols (t) t)
    (proclaim-function list-all-packages () t)
    (proclaim-function intern (string *) (values t t))
    (proclaim-function find-symbol (string &optional t) (values t t))
    (proclaim-function unintern (t *) t)
    (proclaim-function export (t *) t)
    (proclaim-function unexport (t *) t)
    (proclaim-function import (t *) t)
    (proclaim-function shadowing-import (t *) t)
    (proclaim-function shadow (t *) t)
    (proclaim-function use-package (t *) t)
    (proclaim-function unuse-package (t *) t)

    ;; file pathname.d

    (proclaim-function pathname (t) t)
    (proclaim-function parse-namestring (t *) t)
    (proclaim-function merge-pathnames (t *) t)
    (proclaim-function make-pathname (*) t)
    (proclaim-function pathnamep (t) t :predicate t)
    (proclaim-function pathname-host (t *) t)
    (proclaim-function pathname-device (t *) t)
    (proclaim-function pathname-directory (t *) t)
    (proclaim-function pathname-name (t *) t)
    (proclaim-function pathname-type (t *) t)
    (proclaim-function pathname-version (t) t)
    (proclaim-function wild-pathname-p (t *) t)
    (proclaim-function namestring (t) string)
    (proclaim-function file-namestring (t) string)
    (proclaim-function directory-namestring (t) string)
    (proclaim-function host-namestring (t) string)
    (proclaim-function enough-namestring (t *) string)

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

    (proclaim-function clear-output (*) NULL)
    (def-inline clear-output :always (stream) NULL "(mkcl_clear_output(env, #0),mk_cl_Cnil)")

    (proclaim-function finish-output (*) NULL)
    (def-inline finish-output :always (stream) NULL "(mkcl_finish_output(env, #0),mk_cl_Cnil)")

    (proclaim-function force-output (*) NULL)
    (def-inline force-output :always (stream) NULL "(mkcl_force_output(env, #0),mk_cl_Cnil)")

    (proclaim-function fresh-line (*) t)
    (proclaim-function listen (*) t)
    (proclaim-function peek-char (*) t)
    (proclaim-function pprint (t *) t)
    (proclaim-function prin1 (t *) t)
    (def-inline prin1 :always (t t) t "mkcl_prin1(env, #0,#1)")
    (def-inline prin1 :always (t) t "mkcl_prin1(env, #0,mk_cl_Cnil)")

    (proclaim-function princ (t *) t)
    (def-inline princ :always (t t) t "mkcl_princ(env, #0,#1)")
    (def-inline princ :always (t) t "mkcl_princ(env, #0,mk_cl_Cnil)")

    (proclaim-function print (t *) t)
    (def-inline print :always (t t) t "mkcl_print(env, #0,#1)")
    (def-inline print :always (t) t "mkcl_print(env, #0,mk_cl_Cnil)")

    (proclaim-function unread-char (t *) t)
    (proclaim-function read (*) t)
    (proclaim-function read-char (*) t)
    (proclaim-function read-delimited-list (t *) t)
    (proclaim-function read-line (*) (values t t))
    (proclaim-function read-preserving-whitespace (*) t)
    (proclaim-function terpri (*) t)
    (def-inline terpri :always (t) t "mkcl_terpri(env, #0)")
    (def-inline terpri :always nil t "mkcl_terpri(env, mk_cl_Cnil)")

    (proclaim-function write (t *) t)
    (proclaim-function write-byte (fixnum stream) t)
    (proclaim-function write-char (t *) t)
    (def-inline write-char :always (t) t "@0;(mkcl_princ_char(env, mkcl_char_code(env, #0),mk_cl_Cnil),(#0))")

    (proclaim-function write-line (t *) t)
    (proclaim-function write-string (t *) t)
    (proclaim-function read-char-no-hang (*) t)
    (proclaim-function clear-input (*) NULL)
    (def-inline clear-input :always (stream) NULL "(mkcl_clear_input(env, #0),mk_cl_Cnil)")

    (proclaim-function parse-integer (t *) t)
    (proclaim-function read-byte (t *) t)
    (proclaim-function copy-readtable (*) t :no-side-effects t)
    (def-inline copy-readtable :always (null null) t "standard_readtable")

    (proclaim-function readtablep (t) t :predicate t)
    (proclaim-function set-syntax-from-char (t t *) t)
    (proclaim-function set-macro-character (t t *) t)
    (proclaim-function get-macro-character (t *) t)
    (proclaim-function make-dispatch-macro-character (*) t)
    (proclaim-function set-dispatch-macro-character (*) t)
    (proclaim-function get-dispatch-macro-character (*) t)
    (proclaim-function si:fast-read-from-base-string (t) t)
    (proclaim-function si:standard-readtable () t)
    (proclaim-function symbol-function (t) t)
    (proclaim-function fboundp (symbol) t :predicate t)
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

    (proclaim-function subseq (sequence fixnum *) sequence)
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

    (proclaim-function fill (sequence t &key t) sequence)
    #+unicode
    (def-inline fill :always (string character) t "mkcl_fill_string(env, #0, #1)")
    (def-inline fill :always (base-string base-char) t "mkcl_fill_base_string(env, #0, #1)")

    (proclaim-function replace (sequence sequence &key t) sequence)
    #+unicode
    (def-inline replace :always (string string) t "mkcl_replace_in_string(env, #0, #1)")
    (def-inline replace :always (base-string base-string) t "mkcl_replace_in_base_string(env, #0, #1)")

    (proclaim-function search (sequence sequence &key t) t)
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

    (proclaim-function string= (string-designator string-designator *) t :predicate t :no-side-effects t)
    (def-inline string= :always (string string) :bool "mkcl_string_E(env, #0,#1)")
    (proclaim-function string/= (string-designator string-designator *) t :predicate t)

    (proclaim-function string< (string-designator string-designator *) t :predicate t)
    (proclaim-function string> (string-designator string-designator *) t :predicate t)
    (proclaim-function string<= (string-designator string-designator *) t :predicate t)
    (proclaim-function string>= (string-designator string-designator *) t :predicate t)
    (proclaim-function string-equal (string-designator string-designator *) t :predicate t :no-side-effects t)
    (proclaim-function string-not-equal (string-designator string-designator *) t :predicate t)
    (proclaim-function string-lessp (string-designator string-designator *) t :predicate t)
    (proclaim-function string-greaterp (string-designator string-designator *) t :predicate t)
    (proclaim-function string-not-lessp (string-designator string-designator *) t :predicate t)
    (proclaim-function string-not-greaterp (string-designator string-designator *) t :predicate t)

    (proclaim-function make-string (fixnum *) string)
    (proclaim-function string-trim (t string-designator) string)
    (proclaim-function string-left-trim (t string-designator) string)
    (proclaim-function string-right-trim (t string-designator) string)
    (proclaim-function string-upcase (string-designator *) string)
    (proclaim-function string-downcase (string-designator *) string)
    (proclaim-function string-capitalize (string-designator *) string)
    (proclaim-function nstring-upcase (string *) string)
    (proclaim-function nstring-downcase (string *) string)
    (proclaim-function nstring-capitalize (string *) string)
    (proclaim-function string (t) string)
    (proclaim-function si:concatenate-strings (*) string)
    #+unicode
    (def-inline si:concatenate-strings :always (string) t "mkcl_copy_string(env, #0)")
    #+unicode
    (def-inline si:concatenate-strings :always (string string) t "mkcl_concatenate_2_strings(env, #0, #1)")
    #+unicode
    (def-inline si:concatenate-strings :always (string string string) t "mkcl_concatenate_3_strings(env, #0, #1, #2)")

    (proclaim-function si:concatenate-base-strings (*) base-string)
    (def-inline si:concatenate-base-strings :always
      (base-string) t "mkcl_copy_base_string(env, #0)")
    (def-inline si:concatenate-base-strings :always
      (base-string base-string) t "mkcl_concatenate_2_base_strings(env, #0, #1)")
    (def-inline si:concatenate-base-strings :always
      (base-string base-string base-string) t "mkcl_concatenate_3_base_strings(env, #0, #1, #2)")


    ;; file structure.d

    (proclaim-function si:make-structure (t *) t)
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

    (proclaim-function si:*make-special (*) t)
    (proclaim-function si:*make-constant (*) t)

    ;; file symbol.d

    (proclaim-function get (symbol t *) t :no-side-effects t)
    (def-inline get :always (t t t) t "mkcl_get(env, #0,#1,#2)")
    (def-inline get :always (t t) t "mkcl_get(env, #0,#1,mk_cl_Cnil)")

    (proclaim-function remprop (symbol t) t)
    (proclaim-function symbol-plist (symbol) t :predicate t :no-side-effects t)

    (proclaim-function getf (t t *) t)
    (proclaim-function get-properties (t t) *)
    (proclaim-function symbol-name (symbol) string :no-side-effects t)
    (def-inline symbol-name :always (t) string "mkcl_symbol_name(env, #0)")

    (proclaim-function make-symbol (string) symbol)
    (proclaim-function copy-symbol (symbol *) symbol)
    (proclaim-function gensym (*) symbol)
    (proclaim-function gentemp (*) symbol)
    (proclaim-function symbol-package (symbol) t)
    (proclaim-function keywordp (t) t :predicate t)
    (proclaim-function si:put-f (*) t)
    (proclaim-function si:rem-f (*) (values t t))
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
    (proclaim-function sleep (real) t)

    ;; file typeof.d

    (proclaim-function type-of (t) t)
    (proclaim-function typep (t t &optional t) t)

    ;; AKCL addition

    (proclaim-function si:copy-stream (t t) t)

    ;; file seq.lsp

    (proclaim-function concatenate (t &rest t) t)
    (proclaim-function make-sequence (t t &key t) t)
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

    (proclaim-function shift>> (*) nil :no-side-effects t)
    (def-inline shift>> :always (fixnum fixnum) :fixnum "((#0) >> (- (#1)))")

    (proclaim-function shift<< (*) nil :no-side-effects t)
    (def-inline shift<< :always (fixnum fixnum) :fixnum "((#0) << (#1))")

    (proclaim-function short-float-p (*) nil :predicate t :no-side-effects t)
    (def-inline short-float-p :always (t) :bool "mkcl_type_of(#0)==mkcl_t_singlefloat")

    (proclaim-function single-float-p (*) nil :predicate t :no-side-effects t)
    (def-inline single-float-p :always (t) :bool "mkcl_type_of(#0)==mkcl_t_singlefloat")

    (proclaim-function double-float-p (*) nil :predicate t :no-side-effects t)
    (def-inline double-float-p :always (t) :bool "mkcl_type_of(#0)==mkcl_t_doublefloat")

    (proclaim-function long-float-p (*) nil :predicate t :no-side-effects t)
    #-long-float
    (def-inline long-float-p :always (t) :bool "mkcl_type_of(#0)==mkcl_t_doublefloat")
    #+long-float
    (def-inline long-float-p :always (t) :bool "mkcl_type_of(#0)==mkcl_t_longfloat")

    (proclaim-function mkcl:fixnump (*) nil :predicate t :no-side-effects t)
    (def-inline mkcl:fixnump :always (t) :bool "MKCL_FIXNUMP(#0)")
    (def-inline mkcl:fixnump :always (fixnum) :bool "TRUE")

    (proclaim-function si:put-properties (*) nil :no-sp-change t)

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
    (proclaim-function si:unbound (*) t :predicate t :no-side-effects t)
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

    (proclaim-function clos::associate-methods-to-gfun (generic-function *) generic-function)
    (proclaim-function si:of-class-p (t t) t)

    (proclaim-function clos::class-slots (class) list)
    (proclaim-function clos::method-specializers (method) t)

    (proclaim-function si::do-deftype (t t t) t)
    (proclaim-function cl:upgraded-array-element-type (t &optional t) t)
    (proclaim-function cl:upgraded-complex-part-type (t &optional t) t)

    (proclaim-function cl:subtypep (t t &optional t) t)

    (proclaim-function si::search-keyword (t t) t)

    (proclaim-function make-array (t &key t) t)
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
    (proclaim-function cl:adjust-array (t t &key t) t)

    (proclaim-function cl:read-from-string (t &optional t t &key t) (values t t))
    (proclaim-function cl:write-to-string (t &key t) t)
    (proclaim-function mkcl:write-to-base-string (t &key t &allow-other-keys) t)
    (proclaim-function mkcl:prin1-to-base-string (t &key t) t)
    (proclaim-function mkcl:princ-to-base-string (t &key t) t)
    (proclaim-function cl:ensure-directories-exist (t &key t) (values t t))
    (proclaim-function mkcl:run-program (t t &rest t &key t) (values t t t))

    (proclaim-function cl:union (t t &key t) t)
    (proclaim-function cl:nunion (t t &key t) t)
    (proclaim-function cl:intersection (t t &key t) t)
    (proclaim-function cl:nintersection (t t &key t) t)
    (proclaim-function cl:set-difference (t t &key t) t)
    (proclaim-function cl:nset-difference (t t &key t) t)
    (proclaim-function cl:set-exclusive-or (t t &key t) t)
    (proclaim-function cl:nset-exclusive-or (t t &key t) t)
    (proclaim-function cl:subsetp (t t &key t) t)
    (proclaim-function cl:assoc (t t &key t) t)
    (proclaim-function cl:assoc-if (t t &key t) t)
    (proclaim-function cl:assoc-if-not (t t &key t) t)
    (proclaim-function cl:rassoc (t t &key t) t)
    (proclaim-function cl:rassoc-if (t t &key t) t)
    (proclaim-function cl:rassoc-if-not (t t &key t) t)
    (proclaim-function cl:member (t t &key t) t)
    (proclaim-function cl:member-if (t t &key t) t)
    (proclaim-function cl:member-if-not (t t &key t) t)
    (proclaim-function cl:subst (t t t &key t) t)
    (proclaim-function cl:subst-if (t t t &key t) t)
    (proclaim-function cl:subst-if-not (t t t &key t) t)
    (proclaim-function cl:nsubst (t t t &key t) t)
    (proclaim-function cl:nsubst-if (t t t &key t) t)
    (proclaim-function cl:nsubst-if-not (t t t &key t) t)

    (proclaim-function cl:decode-universal-time (t &optional t) (values t t t t t t t t t))
    (proclaim-function cl:encode-universal-time (t t t t t t &optional t) t)

    (proclaim-function cl:atan (t &optional t) t)

    (proclaim-function cl:apropos (t &optional t) (values))
    (proclaim-function cl:apropos-list (t &optional t) t)

    (proclaim-function si:package-children (t &key t) t)

    (proclaim-function cl:reduce (t t &key t) t)
    (proclaim-function cl:remove (t t &key t) t)
    (proclaim-function cl:remove-if (t t &key t) t)
    (proclaim-function cl:remove-if-not (t t &key t) t)
    (proclaim-function cl:delete (t t &key t) t)
    (proclaim-function cl:delete-if (t t &key t) t)
    (proclaim-function cl:delete-if-not (t t &key t) t)
    (proclaim-function cl:position (t t &key t) t)
    (proclaim-function cl:position-if (t t &key t) t)
    (proclaim-function cl:position-if-not (t t &key t) t)
    (proclaim-function cl:count (t t &key t) t)
    (proclaim-function cl:count-if (t t &key t) t)
    (proclaim-function cl:count-if-not (t t &key t) t)
    (proclaim-function cl:substitute (t t t &key t) t)
    (proclaim-function cl:substitute-if (t t t &key t) t)
    (proclaim-function cl:substitute-if-not (t t t &key t) t)
    (proclaim-function cl:nsubstitute (t t t &key t) t)
    (proclaim-function cl:nsubstitute-if (t t t &key t) t)
    (proclaim-function cl:nsubstitute-if-not (t t t &key t) t)
    (proclaim-function cl:find (t t &key t) t)
    (proclaim-function cl:find-if (t t &key t) t)
    (proclaim-function cl:find-if-not (t t &key t) t)
    (proclaim-function cl:remove-duplicates (t &key t) t)
    (proclaim-function cl:delete-duplicates (t &key t) t)
    (proclaim-function cl:mismatch (t t &key t) t)
    (proclaim-function cl:sort (t t &key t) t)
    (proclaim-function cl:stable-sort (t t &key t) t)
    (proclaim-function cl:merge (t t t t &key t) t)

    (proclaim-function cl:method-combination-error (t &rest t) t)
    (proclaim-function cl:invalid-method-error (t t &rest t) t)

    (proclaim-function cl:pprint-newline (t &optional t) t)
    (proclaim-function cl:pprint-indent (t t &optional t) t)
    (proclaim-function cl:pprint-tab (t t t &optional t) t)
    (proclaim-function cl:pprint-fill (t t &optional t t) t)
    (proclaim-function cl:pprint-linear (t t &optional t t) t)
    (proclaim-function cl:pprint-tabular (t t &optional t t t) t)
    (proclaim-function cl:copy-pprint-dispatch (&optional t) t)
    (proclaim-function cl:pprint-dispatch (t &optional t) (values t t))
    (proclaim-function cl:set-pprint-dispatch (t t &optional t t) t)

    (proclaim-function cl:continue (&optional t) t)
    (proclaim-function cl:abort (&optional t) t)
    (proclaim-function cl:muffle-warning (&optional t) t)
    (proclaim-function cl:store-value (t &optional t) t)
    (proclaim-function cl:use-value (t &optional t) t)

    (proclaim-function cl:require (t &optional t) t)
    (proclaim-function cl:y-or-n-p (&optional t &rest t) t)
    (proclaim-function cl:yes-or-no-p (&optional t &rest t) t)
    )
  )  ;; end of (defparameter +all-optimizers+ ...)


(dolist (opti-spec +all-optimizers+)
  ;; proclaim-function and def-inline are evaluated
  ;; for their side-effects here and now!
  (eval opti-spec))


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
 `(SI::C-EXPORT-FNAME
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

