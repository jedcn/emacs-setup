(require 'rvm)
(rvm-use-default)

;; See if RVM is active, and activate it whenever you open a Ruby file
;;
;; From:
;; http://devblog.avdi.org/2011/10/11/rvm-el-and-inf-ruby-emacs-reboot-14/
(add-hook 'ruby-mode-hook
          (lambda () (rvm-activate-corresponding-ruby)))
