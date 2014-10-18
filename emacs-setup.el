
(setq jedcn-es/dir (concat
                    user-emacs-directory
                    "emacs-setup"))

(setq jedcn-env-path "/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin:/usr/texbin:/usr/local/share/npm/bin")

(defun jedcn-sync-env-path-and-exec-path (desired-path)
  "Sets exec-path and env 'PATH' based on DESIRED-PATH"
  (setenv "PATH" desired-path)
  (setq exec-path (split-string desired-path ":")))

(jedcn-sync-env-path-and-exec-path jedcn-env-path)

(setq explicit-bash-args '("--noediting" "--login"))

(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)

(require 'server)
(if (server-running-p)
    (message "Server is running")
  (progn
    (message "Starting server")
    (server-start)))

(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" user-emacs-directory))

(load-theme 'monokai t)

(if window-system
    (set-face-attribute 'default nil :font "Menlo-18"))

(require 'powerline)
(powerline-center-theme)

(setq gc-cons-threshold 20000000)

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

(setq linum-format " %4d ")

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

(defun increase-window-height (&optional arg)
  "Make the window taller by one line. Useful when bound to a repeatable key combination."
  (interactive "p")
  (enlarge-window arg))

(defun decrease-window-height (&optional arg)
  "Make the window shorter by one line. Useful when bound to a repeatable key combination."
  (interactive "p")
  (enlarge-window (- 0 arg)))

(defun decrease-window-width (&optional arg)
  "Make the window narrower by one line. Useful when bound to a repeatable key combination."
  (interactive "p")
  (enlarge-window (- 0 arg) t))

(defun increase-window-width (&optional arg)
  "Make the window shorter by one line. Useful when bound to a repeatable key combination."
  (interactive "p")
  (enlarge-window arg t))

(global-set-key (kbd "C->")
                'increase-window-height)

(global-set-key (kbd "C-<")
                'decrease-window-height)

(global-set-key (kbd "C-,")
                'decrease-window-width)

(global-set-key (kbd "C-.")
                'increase-window-width)

(add-hook 'org-mode-hook
          (lambda ()
            (define-key org-mode-map [(ctrl \,)]
              'decrease-window-width)))

(defun create-new-buffer ()
  "Create a new buffer named *new*[num]."
  (interactive)
  (switch-to-buffer (generate-new-buffer-name "*new*")))

(global-set-key (kbd "C-c n")
                'create-new-buffer)

(when window-system
  (defun new-emacs-instance ()
    (interactive)
    (let ((path-to-emacs
           (locate-file invocation-name
                        (list invocation-directory) exec-suffixes)))
      (call-process path-to-emacs nil 0 nil)))

  (global-set-key (kbd "C-c N")
                  'new-emacs-instance))

(defun newline-anywhere ()
  "Add a newline from anywhere in the line."
  (interactive)
  (end-of-line)
  (newline-and-indent))

(global-set-key (kbd "M-RET")
                'newline-anywhere)

(setq visible-bell t
      inhibit-startup-message t)

(defun major-mode-from-name ()
  "Choose proper mode for buffers created by switch-to-buffer."
  (let ((buffer-file-name (or buffer-file-name (buffer-name))))
    (set-auto-mode)))
(setq-default major-mode 'major-mode-from-name)

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

(setq line-number-mode t)
(setq column-number-mode t)

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

(global-set-key (kbd "C-x m") 'magit-status)

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

(setq magit-diff-refine-hunk 'all)

(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)

(setq org-startup-folded 'content)

(setq org-hide-leading-stars 'hidestars)

(setq org-log-done 'time)

(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)

(setq org-agenda-include-diary t)

(setq org-src-fontify-natively t)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (ruby . t)
   (sh . t)))

(setq org-agenda-files '("~/notes/org"))

(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))

(setq coffee-tab-width 2)

(add-to-list 'auto-mode-alist '("\\.pp$" . puppet-mode))

(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

(setq js-indent-level 2)

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

(add-hook 'ruby-mode-hook
          (lambda ()
            (define-key (current-local-map) [remap newline] 'reindent-then-newline-and-indent)))

(require 'yasnippet)
(setq yas-snippet-dirs (concat jedcn-es/dir "/snippets"))

(yas-global-mode 1)

(require 'smartparens-config)
(smartparens-global-mode)
(show-smartparens-global-mode +1)

(require 'ace-jump-mode)
(define-key global-map
  (kbd "C-c SPC") 'ace-jump-mode)
(add-hook 'org-mode-hook
          (lambda ()
            (define-key org-mode-map
              (kbd "C-c SPC") 'ace-jump-mode)))

(add-hook 'after-init-hook #'global-flycheck-mode)

(defadvice rspec-compile (around rspec-compile-around)
  "Use BASH shell for running the specs because of ZSH issues."
  (let ((shell-file-name "/bin/bash"))
    ad-do-it))
(ad-activate 'rspec-compile)

(eval-after-load "yasnippet" '(diminish 'yas-minor-mode))
(eval-after-load "project-persist" '(diminish 'project-persist-mode))
(diminish 'auto-fill-function)
(diminish 'smartparens-mode)

(rvm-use-default)

(add-hook 'ruby-mode-hook
          (lambda () (rvm-activate-corresponding-ruby)))

(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

(require 'helm)
(require 'helm-config)

(global-set-key (kbd "M-x") 'helm-M-x)

(global-set-key (kbd "C-x b") 'helm-mini)

(global-set-key (kbd "M-y") 'helm-show-kill-ring)

(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-c h s") 'helm-semantic-or-imenu)

;; Don't use marks or mark-ring. Start?
(global-set-key (kbd "C-c m") 'helm-all-mark-rings)
(global-set-key (kbd "C-c h o") 'helm-occur)

;; Don't use eshell. Start?
(add-hook 'eshell-mode-hook
          #'(lambda ()
              (define-key eshell-mode-map (kbd "M-l")  'helm-eshell-history)))

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-z")  'helm-select-action)

(helm-mode 1)

(require 'projectile)

(project-persist-mode t)

(require 'project-persist)

(setq jedcn/pp-project-roots
      (list (concat (getenv "HOME") "/c/galileo")
            (concat (getenv "HOME") "/c/misc")
            (concat (getenv "HOME") "/c/personal")
            (concat (getenv "HOME") "/d")))

(defun jedcn/pp-create-projects-under-root (root)
  "Create project-persist projects for directories under root"
  (let* ((dirs (directory-files root))
         (dir (car dirs))
         (ignore-dirs '("." ".." ".DS_Store")))
    (while dirs
      (unless (member dir ignore-dirs)
        (unless (pp/project-exists dir)
          (pp/project-setup (concat root "/" dir "/") dir)))
      (setq dirs (cdr dirs))
      (setq dir (car dirs)))))

(defun jedcn/pp-create-all-projects (project-roots)
  "Create all project-persist projects based on PROJECT-ROOTS"
  (let* ((project-root (car project-roots)))
    (while project-roots
      (jedcn/pp-create-projects-under-root project-root)
      (setq project-roots (cdr project-roots))
      (setq project-root (car project-roots)))))

(defun jedcn/pp-destroy-all-projects ()
  "Remove all previously created project-persist projects"
  (let ((projects (pp/project-list)))
    (while projects
      (pp/project-destroy (car projects))
      (setq projects (cdr projects)))))

(defun jedcn-pp/rebuild-projects ()
  (interactive)
  (if (jedcn-pp/project-persist-initialized-p)
    (jedcn/pp-destroy-all-projects))
  (jedcn/pp-create-all-projects jedcn/pp-project-roots))

(defun jedcn-pp/project-persist-initialized-p ()
  "Has project-persist been initialized?"
  (file-directory-p project-persist-settings-dir))

(unless (jedcn-pp/project-persist-initialized-p)
  (message "Initializing Project Persist.")
  (jedcn-pp/rebuild-projects))

(defun jedcn-after-open-project (dir)
  "Open up a dired for that project."
  (dired dir))

(add-hook 'project-persist-after-load-hook
          (lambda ()
            (jedcn-after-open-project project-persist-current-project-root-dir)))

(global-set-key "\M-1"
                'helm-project-persist)

(global-set-key "\C-cp"
                'helm-project-persist)

(global-set-key "\M-2"
                'helm-projectile)

(global-set-key "\C-cp"
                'helm-projectile)

(global-set-key "\C-cg"
                'helm-google-suggest)


(add-hook 'magit-mode-hook
          (lambda ()
            (define-key magit-mode-map "\M-1"
              'project-persist-find)
            (define-key magit-mode-map "\M-2"
              'projectile-find-file)))

(defconst jedcn-eval-buffer-commands
  '(("js" . "/usr/local/bin/node")
    ("rb" . "ruby")
    ("coffee" . "/usr/local/share/npm/bin/coffee")
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
