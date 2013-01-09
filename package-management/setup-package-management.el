;; This is from https://github.com/technomancy/emacs-starter-kit
;;
;; Make sure that Marmalade is always included as a package
;; repository. Then make sure all of 'my-packages' are installed.
;;
(eval-after-load "package"
  '(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/")))

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(starter-kit starter-kit-lisp yasnippet rvm rspec-mode dired-details markdown-mode ace-jump-mode yaml-mode)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))
