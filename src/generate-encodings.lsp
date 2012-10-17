;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: SYSTEM -*-
;;;;
;;;;  Copyright (c) 2010, Jean-Claude Beaudoin.
;;;;
;;;;  This program is free software; you can redistribute it and/or
;;;;  modify it under the terms of the GNU Lesser General Public
;;;;  License as published by the Free Software Foundation; either
;;;;  version 3 of the License, or (at your option) any later version.
;;;;
;;;;  See file '../Copyright' for full details.

;;;
;;; * External formats
;;;
(defvar *generate-verbose*)
(setq *generate-verbose* nil) ;; Makes the generate.lisp script more verbose.

(defvar *update-mapping*)
(setq *update-mapping* nil) ;; Setting this to something other than nil may result in network activity.

#+UNICODE
(load "../contrib/encodings/generate.lisp" :external-format '(:ascii :lf))

(mkcl:quit :exit-code 0)
