;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          rt.asd
;;;; Purpose:       ASDF definition file for Rt
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  Sep 2002
;;;;
;;;; $Id: rt.asd 7061 2003-09-07 06:34:45Z kevin $
;;;;
;;;; This file, part of cl-rt, is Copyright (c) 2002 by Kevin M. Rosenberg
;;;;
;;;; cl-rt users are granted the rights to distribute and use this software
;;;; as governed by the terms of the GNU Lesser General Public License 
;;;; (http://www.gnu.org/licenses/lgpl.html)
;;;; *************************************************************************

(in-package :asdf)

(defsystem :rt
  :name "cl-rt"
  :version "1990.12.19"
  :maintainer "Kevin M. Rosenberg <kmr@debian.org>"
  :licence "MIT"
  :description "MIT Regression Tester"
  :long-description "RT provides a framework for writing regression test suites"
  :perform (load-op :after (op rt)
	    (pushnew :rt cl:*features*))
  :components
  ((:file "rt")))


