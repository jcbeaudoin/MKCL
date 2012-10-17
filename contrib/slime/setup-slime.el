;;;
;;;
;;;

(setq inferior-lisp-program (concat "\"" mkcl-install-dir "/bin/mkcl.exe" "\""))
(setq slime-net-coding-system 'utf-8-unix) ;; needed to transmit Unicode between Emacs and Swank.
(add-to-list 'load-path (concat mkcl-install-dir "/slime/"))
;(add-to-list 'load-path (concat mkcl-install-dir "/slime/contrib/"))
(require 'slime)
(slime-setup '(slime-fancy slime-asdf slime-tramp))

;;;
