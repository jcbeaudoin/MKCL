;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: SYSTEM -*-
;;;;
;;;;  Copyright (c) 2016, Jean-Claude Beaudoin.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Lesser General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 3 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../../Copyright' for full details.
;;;;

(setq si::*extended-character-names* (make-hash-table :test 'equalp :size 60000 :rehash-threshold 0.9))

(flet ((extract-fields-in-line (line delim)
         (let (fields (base 0))
           (let () ;;((len (length line)))
             (loop
              (let ((head (position delim line :start base)))
                (unless head (return-from extract-fields-in-line (values-list (nreverse fields))))
                (push (unless (eql base head) (subseq line base head)) fields)
                (setq base (1+ head))))))))

  (with-open-file (uni-db (make-pathname :name "UnicodeData" :type "txt" :defaults *load-truename*))
    (let ((line (read-line uni-db nil :eof)))
      (dotimes (i mkcl::base-char-code-limit)
        ;; skip first base-char-code-limit lines. That is the domain of base-char and it cannot be redefined.
        (setq line (read-line uni-db nil :eof)))
      (do ()
          ((eq line :eof))
        (multiple-value-bind (code-value name) (extract-fields-in-line line #\;)
          (let ((code (let ((*read-base* 16)) (read-from-string code-value)))) ;; code-value is in hexadecimal.
            (unless (eql #\< (char name 0))
              (setq name (substitute #\_ #\space  name))
              (setf (gethash code si::*extended-character-names*) name)
              (setf (gethash name si::*extended-character-names*) code))))
        (setq line (read-line uni-db nil :eof))))))


(provide "UNICODE-NAMES")

