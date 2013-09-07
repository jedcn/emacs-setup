
(setq jedcn-es/dir (concat
                    user-emacs-directory
                    "emacs-setup"))

(package-initialize)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(unless
    (file-directory-p "~/.emacs.d/elpa")
  (package-list-packages))

(defun sacha/package-install (package &optional repository)
  "Install PACKAGE if it has not yet been installed.
If REPOSITORY is specified, use that."
  (unless (package-installed-p package)
    (let ((package-archives (if repository
                                (list (assoc repository package-archives))
                              package-archives)))
    (package-install package))))

(setq jedcn-env-path "/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin:/usr/texbin")

(defun jedcn-sync-env-path-and-exec-path (desired-path)
  "Sets exec-path and env 'PATH' based on DESIRED-PATH"
  (setenv "PATH" desired-path)
  (setq exec-path (split-string desired-path ":")))

(jedcn-sync-env-path-and-exec-path jedcn-env-path)

(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(server-start)

(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" user-emacs-directory))

(sacha/package-install 'zenburn-theme)
(load-theme 'zenburn t)

(set-face-attribute 'default nil :font "Menlo-18")

(setq user-full-name "Jed Northridge"
      user-mail-address "northridge@gmail.com")

(define-key isearch-mode-map (kbd "C-o")
  (lambda () (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp isearch-string (regexp-quote isearch-string))))))

(global-set-key "\C-x\C-m" 'smex)

(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)

(global-set-key (kbd "C-x g") 'goto-line)

(global-set-key [remap goto-line] 'goto-line-with-feedback)

(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input"
  (interactive)
  (unwind-protect
      (progn
        (linum-mode 1)
        (goto-line (read-number "Goto line: ")))
    (linum-mode -1)))

(global-set-key (kbd "C-x m") 'magit-status)

(setq mac-command-modifier 'meta)

(global-set-key (kbd "C-S-n")
                (lambda ()
                  (interactive)
                  (ignore-errors (next-line 5))))

(global-set-key (kbd "C-S-p")
                (lambda ()
                  (interactive)
                  (ignore-errors (previous-line 5))))

(global-set-key (kbd "C-S-f")
                (lambda ()
                  (interactive)
                  (ignore-errors (forward-char 5))))

(global-set-key (kbd "C-S-b")
                (lambda ()
                  (interactive)
                  (ignore-errors (backward-char 5))))

(defun move-line-down ()
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines 1))
    (forward-line)
    (move-to-column col)))

(defun move-line-up ()
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines -1))
    (move-to-column col)))
(global-set-key (kbd "<C-M-S-down>") 'move-line-down)
(global-set-key (kbd "<C-M-S-up>") 'move-line-up)

(setq visible-bell t
      inhibit-startup-message t)

(defun cleanup-buffer-safe ()
  "Perform a bunch of safe operations on the whitespace content of a buffer."
  (interactive)
  (untabify (point-min) (point-max))
  (delete-trailing-whitespace)
  (set-buffer-file-coding-system 'utf-8))

(add-hook 'before-save-hook 'cleanup-buffer-safe)

(defalias 'yes-or-no-p 'y-or-n-p)

(defun esk-local-comment-auto-fill ()
  (set (make-local-variable 'comment-auto-fill-only-comments) t)
  (auto-fill-mode t))
(add-hook 'prog-mode-hook 'esk-local-comment-auto-fill)

(add-hook 'text-mode-hook 'turn-on-auto-fill)

(defun esk-local-column-number-mode ()
  (make-local-variable 'column-number-mode)
  (column-number-mode t))

(add-hook 'prog-mode-hook 'esk-local-column-number-mode)

(defun esk-turn-on-hl-line-mode ()
  (when (> (display-color-cells) 8)
    (hl-line-mode t)))

(add-hook 'prog-mode-hook 'esk-turn-on-hl-line-mode)

(defun esk-pretty-lambdas ()
  (font-lock-add-keywords
   nil `(("(?\\(lambda\\>\\)"
          (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                    ,(make-char 'greek-iso8859-7 107))
                    nil))))))

(add-hook 'prog-mode-hook 'esk-pretty-lambdas)

(sacha/package-install 'better-defaults)

(sacha/package-install 'smex)
(setq smex-save-file (concat user-emacs-directory ".smex-items"))
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)

(sacha/package-install 'markdown-mode)
(sacha/package-install 'markdown-mode+)
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))

(sacha/package-install 'puppet-mode)
(add-to-list 'auto-mode-alist '("\\.pp$" . puppet-mode))

(sacha/package-install 'haml-mode)

(sacha/package-install 'slim-mode)

(sacha/package-install 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

(sacha/package-install 'coffee-mode)

(sacha/package-install 'rvm)

(rvm-use-default)

(add-hook 'ruby-mode-hook
          (lambda () (rvm-activate-corresponding-ruby)))

(sacha/package-install 'feature-mode)

(sacha/package-install 'rspec-mode)

(defadvice rspec-compile (around rspec-compile-around)
  "Use BASH shell for running the specs because of ZSH issues."
  (let ((shell-file-name "/bin/bash"))
    ad-do-it))
(ad-activate 'rspec-compile)

(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Vagrantfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.thor$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Thorfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Guardfile" . ruby-mode))

(sacha/package-install 'ruby-electric)

(sacha/package-install 'magit)

(require 'magit)

(defadvice magit-status (around magit-fullscreen activate)
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

(defun magit-quit-session ()
  "Restores the previous window configuration and kills the magit buffer"
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-fullscreen))

(define-key magit-status-mode-map (kbd "q") 'magit-quit-session)

(defun magit-toggle-whitespace ()
  (interactive)
  (if (member "-w" magit-diff-options)
      (magit-dont-ignore-whitespace)
    (magit-ignore-whitespace)))

(defun magit-ignore-whitespace ()
  (interactive)
  (add-to-list 'magit-diff-options "-w")
  (magit-refresh))

(defun magit-dont-ignore-whitespace ()
  (interactive)
  (setq magit-diff-options (remove "-w" magit-diff-options))
  (magit-refresh))

(define-key magit-status-mode-map (kbd "W") 'magit-toggle-whitespace)

(setq magit-emacsclient-executable "/usr/local/bin/emacsclient")

(sacha/package-install 'yasnippet)
(require 'yasnippet)
(setq yas-snippet-dirs (concat jedcn-es/dir "/snippets"))

(yas-global-mode 1)

(setq org-startup-folded 'content)

(setq org-hide-leading-stars 'hidestars)

(setq org-src-fontify-natively t)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (ruby . t)
   (sh . t)))

(setq org-agenda-files '("~/notes/org"))

(sacha/package-install 'expand-region)
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

(defconst jedcn-eval-buffer-commands
  '(("js" . "/usr/local/bin/node")
    ("rb" . "ruby")
    ("coffee" . "/usr/local/bin/coffee")
    ("clj" . "/Users/jim/local/bin/clojure")
    ("py" . "/usr/bin/python")))

(defconst jw-eval-buffer-name "*EVALBUFFER*")

(defun jw-eval-buffer ()
  "Evaluate the current buffer and display the result in a buffer."
  (interactive)
  (save-buffer)
  (let* ((file-name (buffer-file-name (current-buffer)))
         (file-extension (file-name-extension file-name))
         (buffer-eval-command-pair (assoc file-extension jedcn-eval-buffer-commands)))
    (if buffer-eval-command-pair
        (let ((command (concat (cdr buffer-eval-command-pair) " " file-name)))
          (shell-command-on-region (point-min) (point-max) command jw-eval-buffer-name nil)
          (pop-to-buffer jw-eval-buffer-name)
          (other-window 1)
          (jw-eval-buffer-pretty-up-errors jw-eval-buffer-name)
          (message ".."))
      (message "Unknown buffer type"))))

(defun jw-eval-buffer-pretty-up-errors (buffer)
  "Fix up the buffer to highlight the error message (if it contains one)."
  (save-excursion
    (set-buffer buffer)
    (goto-char (point-min))
    (let ((pos (search-forward-regexp "\\.rb:[0-9]+:\\(in.+:\\)? +" (point-max) t)))
      (if pos (progn
                (goto-char pos)
                (insert-string "\n\n")
                (end-of-line)
                (insert-string "\n"))))))

(defun jw-clear-eval-buffer ()
  (interactive)
  (save-excursion
    (set-buffer jw-eval-buffer-name)
    (kill-region (point-min) (point-max))))

(defun jw-eval-or-clear-buffer (n)
  (interactive "P")
  (cond ((null n) (jw-eval-buffer))
        (t (jw-clear-eval-buffer))))

(setq jedcn-es/composite-org (concat
                              jedcn-es/dir
                              "/emacs-setup.org"))

(setq jedcn-es/files-dir (concat
                          jedcn-es/dir
                          "/org"))

(setq jedcn-es/files '("introduction.org"
                       "general-setup.org"
                       "personal-information.org"
                       "key-bindings.org"
                       "behaviors.org"
                       "modes.org"
                       "various-and-sundry.org"
                       "appendix-a.org"
                       "appendix-b.org"))

(defun jedcn-es/concat-files (the-files target-file)
  "Concatenate a list of THE-FILES into a TARGET-FILE"
  (let* ((original-buffer (current-buffer))
         (result-file target-file)
         (files the-files)
         (file (car files)))
    ;; do..
    (find-file file)
    (write-region (point-min) (point-max) result-file)
    (setq files (cdr files))
    (setq file (car files))
    ;; while
    (while files
      (find-file file)
      (write-region (point-min) (point-max) result-file t)
      (setq files (cdr files))
      (setq file (car files)))
    (switch-to-buffer original-buffer)))

(defun jedcn-es/create-composite-org ()
  "Create a composite org file based on my list of config files"
  (jedcn-es/concat-files
   (mapcar (lambda (file)
             (concat jedcn-es/files-dir "/" file))
           jedcn-es/files)
   jedcn-es/composite-org))

(setq jedcn-es/composite-el (concat jedcn-es/dir "/emacs-setup.el"))

(defun jedcn-es/tangle-composite-org ()
  (org-babel-tangle-file jedcn-es/composite-org jedcn-es/composite-el))

(defun jedcn-es/load-composite-el ()
  (load-file jedcn-es/composite-el))

(defun jedcn-es/rebuild-and-reload ()
  "Rebuild the composite .org file, extract the elisp, and reload"
  (interactive)
  (jedcn-es/create-composite-org)
  (jedcn-es/tangle-composite-org)
  (jedcn-es/load-composite-el))
