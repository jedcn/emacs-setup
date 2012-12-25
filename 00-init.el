;; This setup assumes emacs-starter-kit
;;
;; https://github.com/technomancy/emacs-starter-kit
;;

;; Add marmalade to package repos
(eval-after-load "package"
  '(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/")))

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(starter-kit starter-kit-lisp yasnippet)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(setq jedcn-yasnippet-dir (concat esk-user-dir "/yasnippet"))
(add-to-list 'load-path jedcn-yasnippet-dir)
(load "setup-yasnippet.el")
