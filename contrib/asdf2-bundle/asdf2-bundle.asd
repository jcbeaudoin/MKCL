;;; -*- Mode: Lisp ; Syntax: ANSI-Common-Lisp -*-
;;; ASDF-Bundle, an extension to ASDF to build "bundles",
;;; such as one big fasl or DLL for an entire system, or an executable.
;;;
;;; Initially based on the asdf-ecl extension
;;; by Michael Goffioul and Juan Jose Garcia Ripoll.
;;;
;;; Free Software available under an MIT-style license.
;;; Copyright (c) 2005 - 2007, Michael Goffioul (michael dot goffioul at swing dot be)
;;; Copyright (c) 2008 - 2011, Juan Jose Garcia Ripoll
;;; Copyright (c) 2012 - 2012, Francois-Rene Rideau
;;;
;;; Currently works on
;;; Allegro, Clozure CL, CMUCL, ECL, GNU CLISP, LispWorks, MKCL, SBCL, SCL.
;;; For ABCL, see the abcl-jar contrib instead.

(defsystem :asdf2-bundle
  :licence "MIT"
  :description "Bundle operations for ASDF2"
  :long-description "Can bundle one or many asdf systems into one .fasl and/or one .so"
  :depends-on (:asdf2)
  :components ((:file "asdf2-bundle")))
