;;
;; System definition used in readme.lisp
;;

(asdf:defsystem #:example
    :serial t
    :components ((:file "file1")
		 (:file "file2")))

