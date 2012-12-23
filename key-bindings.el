;; For an initial commit, this is just
;;
;; https://raw.github.com/technomancy/emacs-starter-kit/v2/modules/starter-kit-bindings.el
;;
;; reduced to a straight forward list of bindings.
;;
;; Will pare this down to what I actually use in a moment.
(progn

  ;; It's all about the project.
  (global-set-key (kbd "C-c f") 'find-file-in-project)

  ;; You know, like Readline.
  (global-set-key (kbd "C-M-h") 'backward-kill-word)

  ;; Completion that uses many different methods to find options.
  (global-set-key (kbd "M-/") 'hippie-expand)

  ;; Perform general cleanup.
  (global-set-key (kbd "C-c n") 'esk-cleanup-buffer)

  ;; Jump to a definition in the current file. (Protip: this is awesome.)
  (global-set-key (kbd "C-x C-i") 'imenu)

  ;; File finding
  (global-set-key (kbd "C-x M-f") 'ido-find-file-other-window)
  (global-set-key (kbd "C-c y") 'bury-buffer)
  (global-set-key (kbd "C-c r") 'revert-buffer)

  ;; Help should search more than just commands
  (global-set-key (kbd "C-h a") 'apropos)

  ;; This is a little hacky since VC doesn't support git add internally
  (eval-after-load 'vc
    (define-key vc-prefix-map "i"
      '(lambda () (interactive)
         (if (not (eq 'Git (vc-backend buffer-file-name)))
             (vc-register)
           (shell-command (format "git add %s" buffer-file-name))
           (message "Staged changes.")))))

  ;; Activate occur easily inside isearch
  (define-key isearch-mode-map (kbd "C-o")
    (lambda () (interactive)
      (let ((case-fold-search isearch-case-fold-search))
        (occur (if isearch-regexp isearch-string (regexp-quote isearch-string))))))

  ;; Begin..

  ;; How I start a shell..
  (global-set-key (kbd "C-z") 'shell)

  ;; How I switch windows.. rely on C-x o by default to jump. Might as
  ;; well have shift-array-key support
  (windmove-default-keybindings) ;; Shift+direction

  ;; Use C-x C-m per Steve Yegge's advice:
  ;; https://sites.google.com/site/steveyegge2/effective-emacs
  (global-set-key "\C-x\C-m" 'execute-extended-command)

  ;; How I start up Magit
  (global-set-key (kbd "C-x m") 'magit-status)

  ;; Use regex searches by default.
  (global-set-key (kbd "C-s") 'isearch-forward-regexp)
  (global-set-key (kbd "\C-r") 'isearch-backward-regexp)

  ;; Needs to be able to increase/decrease text at will for pairing,
  ;; presentations, missing glasses, etc.
  (define-key global-map (kbd "C-+") 'text-scale-increase)
  (define-key global-map (kbd "C--") 'text-scale-decrease))
