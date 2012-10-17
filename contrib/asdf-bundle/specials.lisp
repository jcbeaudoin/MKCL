;;; -*- Mode: Lisp ; Syntax: ANSI-Common-Lisp -*-

#+xcvb (module ())

(in-package :asdf)

(defparameter *fasl-type* (pathname-type (compile-file-pathname "foo.lisp"))
  "pathname TYPE for lisp FASt Loading files")
