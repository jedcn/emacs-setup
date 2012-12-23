;;
;; These are other the keybindings of other people.
;;
;; Chiefly,
;;
;; https://github.com/technomancy/emacs-starter-kit/blob/v2/modules/starter-kit-bindings.el
;;
;; And,
;;
;; https://github.com/magnars/.emacs.d/blob/master/key-bindings.el
;;
(progn

  ;; Magnars:
  ;; I don't need to kill emacs that easily
  ;; the mnemonic is C-x REALLY QUIT
  (global-set-key (kbd "C-x r q") 'save-buffers-kill-terminal)
  (global-set-key (kbd "C-x C-c") 'delete-frame)

  ;; Technomancy:
  ;; It's all about the project.
  (global-set-key (kbd "C-c f") 'find-file-in-project)

  ;; Technomancy:
  ;; You know, like Readline.
  (global-set-key (kbd "C-M-h") 'backward-kill-word)

  ;; Technomancy:
  ;; Completion that uses many different methods to find options.
  (global-set-key (kbd "M-/") 'hippie-expand)

  ;; Technomancy:
  ;; Perform general cleanup.
  (global-set-key (kbd "C-c n") 'esk-cleanup-buffer)

  ;; Technomancy:
  ;; Jump to a definition in the current file. (Protip: this is awesome.)
  (global-set-key (kbd "C-x C-i") 'imenu)

  ;; Technomancy:
  ;; File finding
  (global-set-key (kbd "C-x M-f") 'ido-find-file-other-window)
  (global-set-key (kbd "C-c y") 'bury-buffer)
  (global-set-key (kbd "C-c r") 'revert-buffer)

  ;; Technomancy:
  ;; Help should search more than just commands
  (global-set-key (kbd "C-h a") 'apropos)

  ;; Technomancy:
  ;; This is a little hacky since VC doesn't support git add internally
  (eval-after-load 'vc
    (define-key vc-prefix-map "i"
      '(lambda () (interactive)
         (if (not (eq 'Git (vc-backend buffer-file-name)))
             (vc-register)
           (shell-command (format "git add %s" buffer-file-name))
           (message "Staged changes.")))))

  ;; Technomancy:  
  ;; Activate occur easily inside isearch
  (define-key isearch-mode-map (kbd "C-o")
    (lambda () (interactive)
      (let ((case-fold-search isearch-case-fold-search))
        (occur (if isearch-regexp isearch-string (regexp-quote isearch-string))))))

  ;; Technomany and Magnars:
  (global-set-key (kbd "M-x") 'smex)

  ;; Jed:
  ;; How I start a shell..
  (global-set-key (kbd "C-z") 'shell)

  ;; Jed:
  ;; How I switch windows.. rely on C-x o by default to jump. Might as
  ;; well have shift-array-key support
  (windmove-default-keybindings) ;; Shift+direction

  ;; Jed:
  ;; Use C-x C-m per Steve Yegge's advice:
  ;; https://sites.google.com/site/steveyegge2/effective-emacs
  (global-set-key "\C-x\C-m" 'execute-extended-command)

  ;; Jed (from Magnars):
  ;; How I start up Magit
  (global-set-key (kbd "C-x m") 'magit-status)

  ;; Technomancy:
  ;; Use regex searches by default.
  (global-set-key (kbd "C-s") 'isearch-forward-regexp)
  (global-set-key (kbd "\C-r") 'isearch-backward-regexp)

  ;; Jed:
  ;; Needs to be able to increase/decrease text at will for pairing,
  ;; presentations, missing glasses, etc.
  (define-key global-map (kbd "C-+") 'text-scale-increase)
  (define-key global-map (kbd "C--") 'text-scale-decrease))
