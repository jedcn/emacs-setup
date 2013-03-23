;; This is from https://github.com/technomancy/emacs-starter-kit
;;
;; Make sure that Marmalade is always included as a package
;; repository.
;;
(eval-after-load "package"
  '(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/")))

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(starter-kit
                      starter-kit-lisp
                      yasnippet
                      rvm
                      rspec-mode
                      dired-details
                      dired-details+
                      markdown-mode
                      ace-jump-mode
                      yaml-mode
                      haml-mode
                      feature-mode
                      puppet-mode
                      web
                      dash
                      s
                      projectile)

  "A list of packages that I use")
