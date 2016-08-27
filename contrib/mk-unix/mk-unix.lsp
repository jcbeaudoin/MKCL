
(defpackage "MK-UNIX")


(in-package "MK-UNIX")

(defmacro signal-symbols ()
  `(progn
     ,@(loop for i from 1 to si::signal-limit
             collect (let ((sym (intern (si::signum-to-signal-name i))))
                       `(progn (defconstant ,sym ,i) (export ',sym))))))

(signal-symbols)
