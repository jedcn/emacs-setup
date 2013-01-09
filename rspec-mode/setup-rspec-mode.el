(require 'rspec-mode)

(defadvice rspec-compile (around rspec-compile-around)
  "Use BASH shell for running the specs because of ZSH issues."
  (let ((shell-file-name "/bin/bash"))
    ad-do-it))
(ad-activate 'rspec-compile)

(global-set-key (kbd "C-, a") 'rspec-verify-all)
(global-set-key (kbd "C-, f") 'rspec-verify)
(global-set-key (kbd "C-, l") 'rspec-verify-single) ;; mnemonic: l = line
