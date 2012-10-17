;;; -*- Mode: Lisp ; Syntax: ANSI-Common-Lisp -*-

#+xcvb (module (:depends-on ("specials")))

(in-package :asdf)

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; Make sure we have strict ANSI class redefinition semantics.
  (setq clos::*redefine-class-in-place* t))

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

(export '(bundle-system))


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
(pushnew '("fasb" . si::load-binary) si:*load-hooks* :test 'equal :key 'car)
