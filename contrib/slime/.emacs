;; Slime setup

(defconst mkcl-install-dir "C:/Program Files/MKCL 1.1")
(let ((slime-el (concat mkcl-install-dir "/slime/setup-slime.el")))
  (if (file-readable-p slime-el)
      (load slime-el)))


;;;
