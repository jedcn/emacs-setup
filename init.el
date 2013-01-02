;; This setup assumes emacs-starter-kit
;;
;; https://github.com/technomancy/emacs-starter-kit
;;
(setq jedcn-package-management-dir (concat esk-user-dir "/package-management"))
(add-to-list 'load-path jedcn-package-management-dir)
(load "setup-package-management.el")

(setq jedcn-mac-dir (concat esk-user-dir "/mac"))
(add-to-list 'load-path jedcn-mac-dir)
(load "setup-mac.el")

(setq jedcn-yasnippet-dir (concat esk-user-dir "/yasnippet"))
(add-to-list 'load-path jedcn-yasnippet-dir)
(load "setup-yasnippet.el")

(setq jedcn-key-bindings-dir (concat esk-user-dir "/key-bindings"))
(add-to-list 'load-path jedcn-key-bindings-dir)
(load "setup-key-bindings.el")

(setq jedcn-themes-dir (concat esk-user-dir "/themes"))
(add-to-list 'load-path jedcn-themes-dir)
(load "setup-themes.el")

(add-hook 'after-init-hook
          #'(lambda ()

              (setq jedcn-rvm-dir (concat esk-user-dir "/rvm"))
              (add-to-list 'load-path jedcn-rvm-dir)
              (load "setup-rvm.el")

              (setq jedcn-rspec-mode-dir (concat esk-user-dir "/rspec-mode"))
              (add-to-list 'load-path jedcn-rspec-mode-dir)
              (load "setup-rspec-mode.el")

              (setq jedcn-magit-dir (concat esk-user-dir "/magit"))
              (add-to-list 'load-path jedcn-magit-dir)
              (eval-after-load 'magit '(load "setup-magit.el"))

              ;; Load every .el file in misc
              (setq jedcn-misc-dir (concat esk-user-dir "/misc"))
              (add-to-list 'load-path jedcn-misc-dir)
              (mapc 'load (directory-files jedcn-misc-dir nil "^[^#].*el$"))
              ))

