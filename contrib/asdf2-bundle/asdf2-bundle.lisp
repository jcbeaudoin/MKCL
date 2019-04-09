;;; -*- Mode: Lisp ; Syntax: ANSI-Common-Lisp -*-

#+xcvb (module ())

(in-package :asdf)

#+mkcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; Make sure we have strict ANSI class redefinition semantics.
  (setq clos::*redefine-class-in-place* t))

;;;
;;; BUNDLE-OP
;;;
;;; This operation takes all components from one or more systems and
;;; creates a single output file, which may be
;;; a FASL, a statically linked library, a shared library, etc.
;;; The different targets are defined by specialization.
;;;

(defun fasl-type ()
  "pathname TYPE for lisp FASt Loading files"
  (#-ecl load-time-value #+ecl identity
   (pathname-type (compile-file-pathname "foo.lisp"))))

(defclass bundle-op (operation)
  ((build-args :initarg :args :initform nil :accessor bundle-op-build-args)
   (name-suffix :initarg :name-suffix :initform nil)
   #+ecl (type :reader bundle-op-type)
   #+ecl (lisp-files :initform nil :accessor bundle-op-lisp-files)
   #+mkcl (do-fasb :initarg :do-fasb :initform t :reader bundle-op-do-fasb-p)
   #+mkcl (do-static-library :initarg :do-static-library :initform t :reader bundle-op-do-static-library-p)))

(defclass fasl-op (bundle-op)
  ((type :initform :fasl)))

(defclass lib-op (bundle-op)
  ((type :initform :lib)))

(defclass dll-op (bundle-op)
  ((type :initform :dll)))

(defclass monolithic-bundle-op (bundle-op)
  ((prologue-code :accessor monolithic-op-prologue-code)
   (epilogue-code :accessor monolithic-op-epilogue-code)))

(defun bundle-op-monolithic-p (op)
  (typep op 'monolithic-bundle-op))

(defclass monolithic-fasl-op (monolithic-bundle-op fasl-op) ())

(defclass monolithic-lib-op (monolithic-bundle-op lib-op)
  ((type :initform :lib)))

(defclass monolithic-dll-op (monolithic-bundle-op dll-op)
  ((type :initform :dll)))

(defclass program-op (monolithic-bundle-op)
  ((type :initform :program)))

(defmethod initialize-instance :after ((instance bundle-op) &rest initargs
                                       &key (name-suffix nil name-suffix-p)
                                       &allow-other-keys)
  (declare (ignorable initargs name-suffix))
  (unless name-suffix-p
    (setf (slot-value instance 'name-suffix)
          (if (bundle-op-monolithic-p instance) ".system-and-dependencies" ".system")))
  (when (typep instance 'monolithic-bundle-op)
    (destructuring-bind (&rest original-initargs
			 &key lisp-files prologue-code epilogue-code
			 &allow-other-keys)
        (slot-value instance 'original-initargs)
      (setf (slot-value instance 'original-initargs)
            (remove-keys '(lisp-files epilogue-code prologue-code) original-initargs)
            (monolithic-op-prologue-code instance) prologue-code
            (monolithic-op-epilogue-code instance) epilogue-code)
      #-ecl (assert (null lisp-files))
      #+ecl (setf (bundle-op-lisp-files instance) lisp-files)))
  (setf (bundle-op-build-args instance)
        (remove-keys '(type monolithic name-suffix)
                     (slot-value instance 'original-initargs))))

(defmethod bundle-op-build-args :around ((op lib-op))
  (declare (ignorable op))
  (let ((args (call-next-method)))
    (remf args :ld-flags)
    args))

(defvar *force-load-p* nil)

(defmethod operation-done-p :around ((operation load-op) c)
  (declare (ignorable operation c))
  (if *force-load-p* nil (call-next-method)))

(defun gather-components (op-type system &key filter-system filter-type include-self)
  ;; This function creates a list of components,
  ;; matched together with an operation.
  ;; This list may be restricted to sub-components of SYSTEM
  ;; if GATHER-ALL = NIL (default), and it may include the system itself.
  (let* ((operation (make-instance op-type))
         (*force-load-p* t)
         (tree (traverse (make-instance 'load-op) system)))
    (append
     (loop :for (op . component) :in tree
       :when (and (typep op 'load-op)
                  (typep component filter-type)
                  (or (not filter-system) (eq (component-system component) filter-system)))
       :collect (progn
                  (when (eq component system) (setf include-self nil))
                  (cons operation component)))
     (and include-self (list (cons operation system))))))

;;;
;;; BUNDLE-SUB-OPERATIONS
;;;
;;; Builds a list of pairs (operation . component)
;;; which contains all the dependencies of this bundle.
;;; This list is used by TRAVERSE and also by INPUT-FILES.
;;; The dependencies depend on the strategy, as explained below.
;;;
(defgeneric bundle-sub-operations (operation component))
;;;
;;; First we handle monolithic bundles.
;;; These are standalone systems which contain everything,
;;; including other ASDF systems required by the current one.
;;; A PROGRAM is always monolithic.
;;;
;;; MONOLITHIC SHARED LIBRARIES, PROGRAMS, FASL
;;;
;;; Gather the static libraries of all components.
;;;
(defmethod bundle-sub-operations ((o monolithic-bundle-op) c)
  (declare (ignorable o))
  (gather-components 'lib-op c :filter-type 'system :include-self t))

;;;
;;; STATIC LIBRARIES
;;;
;;; Gather the object files of all components
;;; and, if monolithic, also of systems and subsystems.
;;;
(defmethod bundle-sub-operations ((o lib-op) c)
  (gather-components 'compile-op c
                     :filter-system (and (not (bundle-op-monolithic-p o)) c)
                     :filter-type '(not system)))
(defmethod bundle-sub-operations ((o monolithic-lib-op) c)
  (declare (ignorable o))
  (gather-components 'compile-op c
                     :filter-system nil
                     :filter-type '(not system)))
;;;
;;; SHARED LIBRARIES
;;;
;;; Gather the dynamically linked libraries of all components.
;;; They will be linked into this new shared library,
;;; together with the static library of this module.
;;;
(defmethod bundle-sub-operations ((o dll-op) c)
  (declare (ignorable o))
  (list (cons (make-instance 'lib-op) c)))
;;;
;;; FASL FILES
;;;
;;; Gather the statically linked library of this component.
;;;
(defmethod bundle-sub-operations ((o fasl-op) c)
  (declare (ignorable o))
  (list (cons (make-instance 'lib-op) c)))

#-mkcl
(defmethod component-depends-on ((o bundle-op) (c system))
  (loop :for (op . dep) :in (bundle-sub-operations o c)
        :when (typep dep 'system)
        :collect (list (class-name (class-of op))
                       (component-name dep))))

(defmethod component-depends-on ((o lib-op) (c system))
  (declare (ignorable o))
  (list (list 'compile-op (component-name c))))

(defmethod component-depends-on ((o bundle-op) c)
  (declare (ignorable o c))
  nil)

#-mkcl
(defmethod input-files ((o bundle-op) (c system))
  (loop :for (sub-op . sub-c) :in (bundle-sub-operations o c)
        :nconc (output-files sub-op sub-c)))

#-mkcl
(defmethod output-files ((o bundle-op) (c system))
  (list (compile-file-pathname
         (make-pathname
          :name (strcat (component-name c) (slot-value o 'name-suffix)
                        #|"-" (string-downcase (implementation-type))|#)
          :type "lisp"
          :defaults (system-source-directory c))
         #+ecl :type #+ecl (bundle-op-type o))))

(defmethod perform ((o bundle-op) (c t))
  (declare (ignorable o c))
  t)

(defmethod operation-done-p ((o bundle-op) (c source-file))
  (declare (ignorable o c))
  t)

(defun select-operation (monolithic type)
  (ecase type
    ((:binary)
     (if monolithic 'monolithic-binary-op 'binary-op))
    ((:dll :shared-library)
     (if monolithic 'monolithic-dll-op 'dll-op))
    ((:lib :static-library)
     (if monolithic 'monolithic-lib-op 'lib-op))
    ((:fasl)
     (if monolithic 'monolithic-fasl-op 'fasl-op))
    ((:program)
     'program-op)))

(defun make-build (system &rest args &key (monolithic nil) (type :fasl)
                   (move-here nil move-here-p)
                   &allow-other-keys)
  (let* ((operation-name (select-operation monolithic type))
         (move-here-path (if (and move-here
                                  (typep move-here '(or pathname string)))
                             (pathname move-here)
                             (merge-pathnames "./asdf-output/")))
         (operation (apply #'operate operation-name
                           system
                           (remove-keys '(monolithic type move-here) args)))
         (system (find-system system))
         (files (and system (output-files operation system))))
    (if (or move-here (and (null move-here-p)
                           (member operation-name '(:program :binary))))
        (loop :with dest-path = (truename (ensure-directories-exist move-here-path))
              :for f :in files
              :for new-f = (make-pathname :name (pathname-name f)
                                :type (pathname-type f)
                                :defaults dest-path)
              :do (progn
                    (when (probe-file new-f)
                      (delete-file new-f))
                    (rename-file f new-f))
              :collect new-f)
        files)))

;;;
;;; LOAD-FASL-OP
;;;
;;; This is like ASDF's LOAD-OP, but using monolithic fasl files.
;;;

(defclass load-fasl-op (operation) ())

(defmethod component-depends-on ((o load-fasl-op) (c system))
  (declare (ignorable o))
  (unless (trivial-system-p c)
    (subst 'load-fasl-op 'load-op
           (subst 'fasl-op 'compile-op
                  (component-depends-on (make-instance 'load-op) c)))))

(defmethod input-files ((o load-fasl-op) (c system))
  (declare (ignorable o))
  (unless (trivial-system-p c)
    (output-files (make-instance 'fasl-op) c)))

(defmethod perform ((o load-fasl-op) (c t))
  (declare (ignorable o c))
  nil)

(defmethod perform ((o load-fasl-op) (c system))
  (let ((l (input-files o c)))
    (and l
         (load (first l))
         (loop :for i :in (module-components c)
               :do (setf (gethash 'load-op (component-operation-times i))
                         (get-universal-time))))))

;;;
;;; PRECOMPILED FILES
;;;
;;; This component can be used to distribute ASDF systems in precompiled form.
;;; Only useful when the dependencies have also been precompiled.
;;;

(defclass compiled-file (component)
  ((type :initform nil)))

(defun trivial-system-p (c)
  (every #'(lambda (c) (typep c 'compiled-file)) (module-components c)))

(defmethod component-relative-pathname ((component compiled-file))
  (coerce-pathname
   (or (slot-value component 'relative-pathname)
       (component-name component))
   :type (fasl-type)
   :defaults (component-parent-pathname component)))

(defmethod output-files (o (c compiled-file))
  (declare (ignorable o c))
  nil)
(defmethod input-files (o (c compiled-file))
  (declare (ignorable o c))
  nil)
(defmethod perform ((o load-op) (c compiled-file))
  (declare (ignorable o))
  (load (component-pathname c)))
(defmethod perform ((o load-fasl-op) (c compiled-file))
  (declare (ignorable o))
  (load (component-pathname c)))
(defmethod perform (o (c compiled-file))
  (declare (ignorable o c))
  nil)

;;;
;;; Pre-built systems
;;;
(defclass prebuilt-system (system)
  ((static-library :accessor prebuilt-system-static-library :initarg :lib)))

(defmethod output-files ((o lib-op) (c prebuilt-system))
  (declare (ignorable o))
  (values (list (prebuilt-system-static-library c))
	  t)) ; Advertise that we do not want this path renamed by asdf-output-translations

(defmethod perform ((o lib-op) (c prebuilt-system))
  (first (output-files o c)))

(defmethod component-depends-on ((o lib-op) (c prebuilt-system))
  (declare (ignorable o c))
  nil)

(defmethod bundle-sub-operations ((o lib-op) (c prebuilt-system))
  (declare (ignorable o c))
  nil)

(defmethod bundle-sub-operations ((o monolithic-lib-op) (c prebuilt-system))
  (declare (ignorable o))
  (error "Prebuilt system ~S shipped with ECL can not be used in a monolithic library operation." c))

(defmethod bundle-sub-operations ((o monolithic-bundle-op) (c prebuilt-system))
  (declare (ignorable o c))
  nil)

;;;
;;; PREBUILT SYSTEM CREATOR
;;;

(defclass binary-op (bundle-op)
  ())

(defclass monolithic-binary-op (binary-op monolithic-bundle-op)
  ())

(defun binary-op-dependencies (o s)
  (multiple-value-bind (lib-op fasl-op)
      (if (bundle-op-monolithic-p o)
          (values 'monolithic-lib-op 'monolithic-fasl-op)
          (values 'lib-op 'fasl-op))
    (list (list (make-instance lib-op :args (bundle-op-build-args o))
                s)
          (list (make-instance fasl-op :args (bundle-op-build-args o))
                s))))

(defmethod component-depends-on ((o binary-op) (s system))
  (loop :for dep :in (binary-op-dependencies o s)
        :append (apply #'component-depends-on dep)))

(defmethod input-files ((o binary-op) (s system))
  (loop :for dep :in (binary-op-dependencies o s)
        :append (apply #'input-files dep)))

(defmethod output-files ((o binary-op) (s system))
  (list* (merge-pathnames* (make-pathname :name (component-name s)
                                          :type "asd")
                           (component-relative-pathname s))
         (loop :for dep :in (binary-op-dependencies o s)
               :append (apply #'output-files dep))))

(defmethod perform ((o binary-op) (s system))
  (let* ((dependencies (binary-op-dependencies o s))
         (library (first (apply #'output-files (first dependencies))))
         (fasl (first (apply #'output-files (second dependencies))))
         (filename (first (output-files o s)))
         (name (component-name s))
         (name-keyword (intern (string name) (find-package :keyword))))
    (dolist (dep dependencies)
      (apply #'perform dep))
    (with-open-file (s filename :direction :output :if-exists :supersede
                       :if-does-not-exist :create)
      (format s ";;; Prebuilt ASDF definition for system ~A" name)
      (format s ";;; Built for ~A ~A on a ~A/~A ~A"
              (lisp-implementation-type)
              (lisp-implementation-version)
              (software-type)
              (machine-type)
              (software-version))
      (let ((*package* (find-package :keyword)))
        (pprint `(defsystem ,name-keyword
                     :class asdf::prebuilt-system
                     :components ((:compiled-file ,(pathname-name fasl)))
                     :lib ,(make-pathname :name (pathname-name library)
                                          :type (pathname-type library)))
                s)))))

(defun copy-stream-to-stream (input output &key (element-type 'character) (buffer-size 8192))
  "Copy the contents of the INPUT stream into the OUTPUT stream,
using WRITE-SEQUENCE and a sensibly sized buffer." ; copied from xcvb-driver
  (with-open-stream (input input)
    (loop
      :for buffer = (make-array (list buffer-size) :element-type element-type)
      :for end = (read-sequence buffer input)
      :until (zerop end)
      :do (write-sequence buffer output :end end)
          (when (< end buffer-size) (return)))))

(defun concatenate-files (inputs output)
  (with-open-file (o output :element-type '(unsigned-byte 8)
                            :direction :output :if-exists :rename-and-delete)
    (dolist (input inputs)
      (with-open-file (i input :element-type '(unsigned-byte 8)
                               :direction :input :if-does-not-exist :error)
        (copy-stream-to-stream i o :element-type '(unsigned-byte 8))))))

(defun* add-pathname-suffix (pathname suffix)
  (make-pathname :name (strcat (pathname-name pathname) suffix)
                 :defaults pathname))

(defun combine-fasls (inputs output)
  #-(or clozure allegro clisp cmu sbcl scl lispworks) (declare (ignore inputs output))
  #+clozure (ccl:fasl-concatenate output inputs :if-exists :supersede)
  #+(or allegro clisp cmu sbcl scl) (concatenate-files inputs output)
  #+lispworks
  (let (fasls)
    (unwind-protect
         (progn
           (loop :for i :in inputs
                 :for n :from 1
                 :for f = (add-pathname-suffix
                           output (format nil "-FASL~D" n))
                 :do (lispworks:copy-file i f)
                     (push f fasls))
           (ignore-errors (lispworks:delete-system :fasls-to-concatenate))
           (eval `(scm:defsystem :fasls-to-concatenate
                    (:default-pathname ,(pathname-directory-pathname output))
                    :members
                    ,(loop :for f :in (reverse fasls)
                           :collect `(,(namestring f) :load-only t))))
           (scm:concatenate-system output :fasls-to-concatenate))
      (loop :for f :in fasls :do (ignore-errors (delete-file f)))
      (ignore-errors (lispworks:delete-system :fasls-to-concatenate))))
  #-(or allegro clisp clozure cmu lispworks sbcl scl)
  (error "~S is not supported on ~A" 'combine-fasls (implementation-type)))

(defun call-with-staging-pathname (pathname fun)
  "Calls fun with a staging pathname, and atomically
renames the staging pathname to the pathname in the end.
Note: this protects only against failure of the program,
not against concurrent attempts.
For the latter case, we ought pick random suffix and atomically open it."
  (let* ((pathname (pathname pathname))
         (staging (add-pathname-suffix pathname "-ASDF-TMP")))
    (unwind-protect
         (multiple-value-prog1
             (funcall fun staging)
           (rename-file staging pathname #+clozure :if-exists #+clozure :rename-and-delete))
      (when (probe-file* staging)
        (delete-file staging)))))

(defmacro with-staging-pathname ((pathname-var &optional (pathname-value pathname-var)) &body body)
  `(call-with-staging-pathname ,pathname-value #'(lambda (,pathname-var) ,@body)))

#-(or ecl mkcl)
(defmethod perform ((o bundle-op) (c system))
  (let* ((input-files (input-files o c))
         (fasl-files (remove (fasl-type) input-files :key #'pathname-type :test-not #'string=))
         (non-fasl-files (remove (fasl-type) input-files :key #'pathname-type :test #'string=))
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

(defmethod output-files ((o fasl-op) (c source-file))
  (declare (ignorable o c))
  nil)

(defmethod input-files ((o fasl-op) (c source-file))
  (declare (ignorable o c))
  nil)

(defclass precompiled-system (system)
  ((fasl :initarg :fasl :reader %system-fasl)))

(defgeneric system-fasl (system)
  (:method ((system precompiled-system))
    (let* ((f (%system-fasl system))
           (p (etypecase f
                ((or pathname string) f)
                (function (funcall f))
                (cons (eval f)))))
      (pathname p))))

(defmethod input-files ((o load-op) (s precompiled-system))
  (declare (ignorable o))
  (list (system-fasl s)))

(defmethod perform ((o load-op) (s precompiled-system))
  (declare (ignorable o))
  (load (system-fasl s)))

#| ;; Example use:
(asdf:defsystem :precompiled-asdf-utils :class asdf::precompiled-system :fasl (asdf:apply-output-translations (asdf:system-relative-pathname :asdf-utils "asdf-utils.system.fasl")))
(asdf:load-system :precompiled-asdf-utils)
|#

#+ecl
(defmethod output-files ((o fasl-op) (c system))
  (declare (ignorable o c))
  (loop :for file :in (call-next-method)
        :collect (make-pathname :type "fasb" :defaults file)))

#+ecl
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

#+mkcl
(progn
;;;
;;; BUNDLE-SUB-OPERATIONS
;;;
;;; Builds a list of pairs (operation . component) which contains all the
;;; dependencies of this bundle.
;;;

(defun mkcl-bundle-sub-operations (sys)
  (gather-components 'compile-op sys
		     :filter-system sys
		     :filter-type '(not system)))

(defun files-to-bundle (sys)
  (loop :for (op . comp) :in (mkcl-bundle-sub-operations sys)
    :for sub-files = (output-files op comp)
    :when sub-files
    :collect (first sub-files)))

(defmethod component-depends-on ((o bundle-op) (c system))
  (cons `(compile-op ,(component-name c)) (call-next-method)))

(defmethod output-files ((o bundle-op) (c system))
  (let* ((name (component-name c))
	 (static-lib-name (merge-pathnames
			   (compiler::builder-internal-pathname name :static-library)
			   (component-relative-pathname c)))
	 (fasl-bundle-name (merge-pathnames
			    (compiler::builder-internal-pathname name :fasb)
			    (component-relative-pathname c))))
    (list static-lib-name fasl-bundle-name)))

(defmethod perform ((o bundle-op) (c system))
  (let* ((object-files (files-to-bundle c))
	 (output (output-files o c)))
    (ensure-directories-exist (first output))
    (when (bundle-op-do-static-library-p o)
      (apply #'compiler::build-static-library (first output)
             :lisp-object-files object-files (bundle-op-build-args o)))
    (when (bundle-op-do-fasb-p o)
      (apply #'compiler::build-bundle (second output)
             :lisp-object-files object-files (bundle-op-build-args o)))))

(defun bundle-system (system &rest args &key force (verbose t) version &allow-other-keys)
  (declare (ignore force verbose version))
  (apply #'operate 'bundle-op system args))

;;;
;;; BUNDLED FILES
;;;
;;; This component can be used to distribute ASDF libraries in bundled form.
;;;

(defclass bundle (component) ())

(defmethod source-file-type ((c bundle) (s system))
  "fasb")

(defmethod perform ((o load-op) (c bundle))
  (load (component-pathname c)))

(defmethod perform (o (c bundle))
  (declare (ignore o))
  nil)

;; The ability to load a fasb bundle is separate from
;; the ability to build a fasb bundle, so this is somewhat unrelated to what is above.
);mkcl

;;;
;;; Final integration steps
;;;

(export '(load-fasl-op precompiled-system
          #+ecl make-build #+mkcl bundle-system))

#+(or ecl mkcl)
(pushnew '("fasb" . si::load-binary) si:*load-hooks* :test 'equal :key 'car)
