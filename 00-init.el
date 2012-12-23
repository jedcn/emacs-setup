;; This setup assumes emacs-starter-kit
;;
;; https://github.com/technomancy/emacs-starter-kit
;;
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(starter-kit starter-kit-lisp)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))
