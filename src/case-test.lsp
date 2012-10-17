;;

(defun foo (y) 
  ;;(case y (2 (print "matches 2")) (t (print "matches t")) (1 (print "matches 1")) )
  (case y (2 (print "matches 2")) (1 (print "matches 1")) (t (print "matches t")) )
  )

(defun type-foo (y) 
  ;;(case y (2 (print "matches 2")) (t (print "matches t")) (1 (print "matches 1")) )
  (typecase y (fixnum (print "matches fixnum")) (cons (print "matches cons")) (t (print "matches t")) )
  )

