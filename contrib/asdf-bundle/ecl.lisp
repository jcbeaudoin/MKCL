;;; -*- Mode: Lisp ; Syntax: ANSI-Common-Lisp -*-

#+xcvb (module (:depends-on ("specials")))

(in-package :asdf)

(defmethod output-files ((o fasl-op) (c system))
  (declare (ignorable o c))
  (loop :for file :in (call-next-method)
        :collect (make-pathname :type "fasb" :defaults file)))

(defmethod perform ((o bundle-op) (c system))
  (let* ((object-files (remove "fas" (input-files o c)
                               :key #'pathname-type :test #'string=))
         (output (output-files o c)))
    (ensure-directories-exist (first output))
    (apply #'c::builder (bundle-op-type o) (first output)
	   :lisp-files (append object-files (bundle-op-lisp-files o))
           (append (bundle-op-build-args o)
                   (when (and (typep o 'monolithic-bundle-op)
                              (monolithic-op-prologue-code o))
                     `(:prologue-code ,(monolithic-op-prologue-code o)))
                   (when (and (typep o 'monolithic-bundle-op)
                              (monolithic-op-epilogue-code o))
                     `(:epilogue-code ,(monolithic-op-epilogue-code o)))))))


;;;
;;; Final integration steps
;;;

(export '(make-build load-fasl-op))

(pushnew '("fasb" . si::load-binary) ext:*load-hooks* :test 'equal :key 'car)
