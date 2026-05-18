;;;
;;;
;;;

(progn
  (setq *package* (find-package "SYSTEM"))
  (setq *features* (cons :mkcl-bootstrap *features*))
  )

(load "bare.lsp")

;;;
;;; * Add include path to not yet installed headers.

(setq compiler::*mkcl-include-directory* (truename (pathname "./c")) ;; truename is needed by MS-Windows
      compiler::*mkcl-library-directory* (truename (pathname "."))
      )

;;; (setq compiler::*trace-cc* t)

(defun build-mkcl-small ()
  (terpri) (princ ";;; About to build main executable mkcl.") (terpri)

  #+unix
  (unless (compiler:build-program
	   "mkcl-small"
	   :use-mkcl-shared-libraries nil ;; force static linking
	   ;;:extra-ld-flags "-pg"  ;; for profiling
	   )
    (mkcl:quit :exit-code 1))


  #+windows
  (let ((target-name #+msvc "mkcl2" #+(or mingw32 mingw64) "mkcl-small"))
    (unless (compiler:build-program
	     target-name
	     )
      (mkcl:quit :exit-code 1))
    )
  )

