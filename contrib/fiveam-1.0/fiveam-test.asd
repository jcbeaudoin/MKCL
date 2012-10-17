;;

(defsystem :fiveam-test
  :author "Edward Marco Baringer <mb@bese.it>"
  :depends-on (:fiveam)
  :pathname "t/"
  :components ((:file "suite")
               (:file "tests" :depends-on ("suite"))))


