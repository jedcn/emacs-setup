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

(setq jedcn-rvm-dir (concat esk-user-dir "/rvm"))
(add-to-list 'load-path jedcn-rvm-dir)
(load "setup-rvm.el")

(setq jedcn-yasnippet-dir (concat esk-user-dir "/yasnippet"))
(add-to-list 'load-path jedcn-yasnippet-dir)
(load "setup-yasnippet.el")

(setq jedcn-key-bindings-dir (concat esk-user-dir "/key-bindings"))
(add-to-list 'load-path jedcn-key-bindings-dir)
(load "setup-key-bindings.el")
