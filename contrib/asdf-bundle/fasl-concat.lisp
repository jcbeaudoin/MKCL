;;; -*- Mode: Lisp ; Syntax: ANSI-Common-Lisp -*-

#+xcvb (module (:depends-on ("bundle")))

(in-package :asdf)

(declaim (optimize (debug 3) (safety 3) (speed 1)))

#+sbcl
(defun concatenate-files (inputs output)
  (let ((infiles (mapcar #'sb-ext:native-namestring inputs))
        (outfile (sb-ext:native-namestring output)))
    (assert
     (= 0 (sb-ext:process-exit-code 
           #+win32
           (sb-ext:run-program
            "copy" `("/b" ,@(loop :for (i . morep) :on infiles
                                  :collect i :when morep :collect "+") ,output)
            :input nil :output nil :error nil :wait t)
           #-win32
           (sb-ext:run-program
            "cat" infiles :output outfile
            :if-output-exists :supersede :external-format :latin1
            :input nil :error nil :search t :wait t))))))

(defun combine-fasls (inputs output)
  #+ccl (ccl:fasl-concatenate output inputs :if-exists :supersede)
  #+sbcl (concatenate-files inputs output))

(defun call-with-staging-pathname (pathname fun)
  "Calls fun with a staging pathname, and atomically
renames the staging pathname to the pathname in the end.
Note: this protects only against failure of the program,
not against concurrent attempts.
For the latter case, we ought pick random suffix and atomically open it."
  (let* ((pathname (pathname pathname))
         (staging (make-pathname
                   :name (strcat (pathname-name pathname) "-staging")
                   :defaults pathname)))
    (unwind-protect
         (multiple-value-prog1
             (funcall fun staging)
           (rename-file staging pathname #+clozure :if-exists #+clozure :rename-and-delete))
      (when (probe-file* staging)
        (delete-file staging)))))

(defmacro with-staging-pathname ((pathname-var &optional (pathname-value pathname-var)) &body body)
  `(call-with-staging-pathname ,pathname-value #'(lambda (,pathname-var) ,@body)))

(defmethod perform ((o bundle-op) (c system))
  (let* ((input-files (input-files o c))
         (fasl-files (remove *fasl-type* input-files :key #'pathname-type :test-not #'string=))
         (non-fasl-files (remove *fasl-type* input-files :key #'pathname-type :test #'string=))
         (output-files (output-files o c))
         (output-file (first output-files)))
    (when input-files
      (assert output-files)
      (when non-fasl-files
        (error "On ~A, asdf-bundle can only bundle FASL files, but these were also produced: ~S"
               (implementation-type) non-fasl-files))
      (when (and (typep o 'monolithic-bundle-op)
                 (or (monolithic-op-prologue-code o) (monolithic-op-epilogue-code o)))
        (error "prologue-code and epilogue-code are not supported on ~A"
               (implementation-type)))
      (ensure-directories-exist output-file)
      (with-staging-pathname (output-file)
        (combine-fasls fasl-files output-file)))))

(defmethod output-files ((o fasl-op) (c component))
  (declare (ignorable o c))
  nil)

(defmethod input-files ((o fasl-op) (c component))
  (declare (ignorable o c))
  nil)
