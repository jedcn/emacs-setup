;; This setup assumes emacs-starter-kit
;;
;; https://github.com/technomancy/emacs-starter-kit
;;
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(starter-kit)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(global-set-key (kbd "M-x") 'smex)

;; Use C-x C-m to do M-x per Steve Yegge's advice
(global-set-key "\C-x\C-m" 'execute-extended-command)
