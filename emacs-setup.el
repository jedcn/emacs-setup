
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

(setq jedcn-env-path "/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin:/usr/texbin:/usr/local/share/npm/bin")

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
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)

(server-start)

(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" user-emacs-directory))

(sacha/package-install 'zenburn-theme)
(sacha/package-install 'base16-theme)
(load-theme 'base16-default t)

(set-face-attribute 'default nil :font "Menlo-18")

(sacha/package-install 'powerline)
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

(sacha/package-install 'better-defaults)

(sacha/package-install 'smex)
(setq smex-save-file (concat user-emacs-directory ".smex-items"))
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)

(sacha/package-install 'flx-ido)
(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
(setq ido-use-faces nil)

(sacha/package-install 'ace-jump-mode)
(require 'ace-jump-mode)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

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

(defun jedcn-coffee-custom ()
  "jedcn's coffee-mode-hook"
  (define-key coffee-mode-map [(meta c)] 'coffee-compile-buffer)
  (make-local-variable 'tab-width)
  (set 'tab-width 2))

(add-hook 'coffee-mode-hook '(lambda () (jedcn-coffee-custom)))

(sacha/package-install 'scss-mode)

(setq js-indent-level 2)

;;  (sacha/package-install 'flycheck)
;;  (add-hook 'after-init-hook #'global-flycheck-mode)

(sacha/package-install 'diminish)
(eval-after-load "yasnippet" '(diminish 'yas-minor-mode))
(diminish 'auto-fill-function)

(sacha/package-install 'smartparens)
(require 'smartparens-config)
(smartparens-global-mode)
(show-smartparens-global-mode +1)

(sacha/package-install 'projectile)
(require 'projectile)

(sacha/package-install 'project-persist)
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

(defun jedcn-pp/rebuild-projects ()
  (interactive)
  (jedcn/pp-create-all-projects jedcn/pp-project-roots))

(jedcn-pp/rebuild-projects)

(global-set-key "\M-1"
                'project-persist-find)

(global-set-key "\M-2"
                'projectile-find-file)

(add-hook 'magit-mode-hook
          (lambda ()
            (define-key magit-mode-map "\M-1"
              'project-persist-find)
            (define-key magit-mode-map "\M-2"
              'projectile-find-file)
            (define-key magit-mode-map "\M-s"
              'sr-speedbar-toggle)))

(global-set-key (kbd "C->")
                'increase-window-height)

(global-set-key (kbd "C-<")
                'decrease-window-height)

(global-set-key (kbd "C-,")
                'decrease-window-width)

(global-set-key (kbd "C-.")
                'increase-window-width)

(global-set-key (kbd "C-c s")
                'sr-speedbar-select-window)

(defun kill-default-buffer ()
  "Kill the currently active buffer -- set to C-x k so that users are not asked which buffer they want to kill."
  (interactive)
  (let (kill-buffer-query-functions) (kill-buffer)))

(defun kill-buffer-if-file (buf)
  "Kill a buffer only if it is file-based."
  (when (buffer-file-name buf)
    (when (buffer-modified-p buf)
        (when (y-or-n-p (format "Buffer %s is modified - save it?" (buffer-name buf)))
            (save-some-buffers nil buf)))
    (set-buffer-modified-p nil)
    (kill-buffer buf)))

(defun kill-all-buffers ()
    "Kill all file-based buffers."
    (interactive)
    (mapc (lambda (buf) (kill-buffer-if-file buf))
     (buffer-list)))

(defun kill-buffer-and-window ()
  "Close the current window and kill the buffer it's visiting."
  (interactive)
  (progn
    (kill-buffer)
    (delete-window)))

(defun create-new-buffer ()
  "Create a new buffer named *new*[num]."
  (interactive)
  (switch-to-buffer (generate-new-buffer-name "*new*")))

(defun insert-semicolon-at-end-of-line ()
  "Add a closing semicolon from anywhere in the line."
  (interactive)
  (save-excursion
    (end-of-line)
    (insert ";")))

(defun comment-current-line-dwim ()
  "Comment or uncomment the current line."
  (interactive)
  (save-excursion
    (push-mark (beginning-of-line) t t)
    (end-of-line)
    (comment-dwim nil)))

(defun newline-anywhere ()
  "Add a newline from anywhere in the line."
  (interactive)
  (end-of-line)
  (newline-and-indent))

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

;; Create a new instance of emacs
(when window-system
  (defun new-emacs-instance ()
    (interactive)
    (let ((path-to-emacs
           (locate-file invocation-name
                        (list invocation-directory) exec-suffixes)))
      (call-process path-to-emacs nil 0 nil))))

(sacha/package-install 'sr-speedbar)
(require 'sr-speedbar)

(defvar graphene-speedbar-refresh-hooks '(after-save-hook)
  "List of hooks which on being run will cause speedbar to refresh.")

(global-set-key (kbd "M-s") 'sr-speedbar-toggle)

(setq speedbar-hide-button-brackets-flag t
      speedbar-show-unknown-files t
      speedbar-smart-directory-expand-flag t
      speedbar-directory-button-trim-method 'trim
      speedbar-use-images nil
      speedbar-indentation-width 2
      speedbar-use-imenu-flag t
      speedbar-file-unshown-regexp "flycheck-.*"
      sr-speedbar-width 20
      sr-speedbar-width-x 24
      sr-speedbar-auto-refresh nil
      sr-speedbar-skip-other-window-p t
      sr-speedbar-right-side nil)

;; Refresh the speedbar when relevant hooks are run.
(defvar graphene-speedbar-refresh-hooks)
(defvar graphene-speedbar-refresh-hooks-added nil
  "Whether hooks have been added to refresh speedbar.")

(add-hook 'speedbar-mode-hook
          (when (not graphene-speedbar-refresh-hooks-added)
            (lambda ()
              (mapc (lambda (hook)
                      (add-hook hook 'speedbar-refresh))
                    graphene-speedbar-refresh-hooks)
              (setq graphene-speedbar-refresh-hooks-added t))))

;; More familiar keymap settings.
(add-hook 'speedbar-reconfigure-keymaps-hook
          '(lambda ()
             (define-key speedbar-mode-map [S-up] 'speedbar-up-directory)
             (define-key speedbar-mode-map [right] 'speedbar-flush-expand-line)
             (define-key speedbar-mode-map [left] 'speedbar-contract-line)))

;; Highlight the current line
(add-hook 'speedbar-mode-hook '(lambda () (hl-line-mode 1)))

;; Pin and unpin the speedbar
(defvar graphene-speedbar-pinned-directory)

(defadvice speedbar-update-directory-contents
  (around graphene-speedbar-pin-directory activate disable)
  "Pin the speedbar to the directory set in graphene-speedbar-pinned-directory."
  (let ((default-directory graphene-speedbar-pinned-directory))
    ad-do-it))

(defadvice speedbar-dir-follow
  (around graphene-speedbar-prevent-follow activate disable)
  "Prevent speedbar changing directory on button clicks."
  (speedbar-toggle-line-expansion))

(defadvice speedbar-directory-buttons-follow
  (around graphene-speedbar-prevent-root-follow activate disable)
  "Prevent speedbar changing root directory on button clicks.")

 (defvar graphene-speedbar-pin-advice
   '((speedbar-update-directory-contents around graphene-speedbar-pin-directory)
     (speedbar-dir-follow around graphene-speedbar-prevent-follow)
     (speedbar-directory-buttons-follow around graphene-speedbar-prevent-root-follow))
   "Advice to be enabled and disabled on graphene-[un]-pin-speedbar.")

(defun graphene-speedbar-pin-advice-activate ()
  "Activate the advice applied to speedbar functions in order to pin it to a directory."
  (mapc 'ad-activate (mapcar 'car graphene-speedbar-pin-advice)))

(defun graphene-pin-speedbar (directory)
  "Prevent the speedbar from changing the displayed root directory."
  (setq graphene-speedbar-pinned-directory directory)
  (mapc (lambda (ls) (apply 'ad-enable-advice ls)) graphene-speedbar-pin-advice)
  (graphene-speedbar-pin-advice-activate))

(defun graphene-unpin-speedbar ()
  "Allow the speedbar to change the displayed root directory."
  (mapc (lambda (ls) (apply 'ad-disable-advice ls)) graphene-speedbar-pin-advice)
  (graphene-speedbar-pin-advice-activate))

;; Always use the last selected window for loading files from speedbar.
(defvar last-selected-window
  (if (not (eq (selected-window) sr-speedbar-window))
      (selected-window)
    (other-window 1)))

(defadvice select-window (after remember-selected-window activate)
  "Remember the last selected window."
  (unless (or (eq (selected-window) sr-speedbar-window) (not (window-live-p (selected-window))))
    (setq last-selected-window (selected-window))))

(defun sr-speedbar-before-visiting-file-hook ()
  "Function that hooks `speedbar-before-visiting-file-hook'."
  (select-window last-selected-window))

(defun sr-speedbar-before-visiting-tag-hook ()
  "Function that hooks `speedbar-before-visiting-tag-hook'."
  (select-window last-selected-window))

(defun sr-speedbar-visiting-file-hook ()
  "Function that hooks `speedbar-visiting-file-hook'."
  (select-window last-selected-window))

(defun sr-speedbar-visiting-tag-hook ()
  "Function that hooks `speedbar-visiting-tag-hook'."
  (select-window last-selected-window))

(defvar graphene-speedbar-auto t
  "Whether graphene should open sr-speedbar when a project is loaded.")

(defvar graphene-project-pin-speedbar t
  "Pin the speedbar directory when opening a project.")

(defun graphene-set-project-root (dir)
  "Change the default directory and update speedbar if used."
  (setq default-directory dir)
  (when graphene-speedbar-auto
    (sr-speedbar-open)
    (speedbar-update-contents)
    (when graphene-project-pin-speedbar
      (graphene-pin-speedbar dir))))

(defun graphene-load-project-desktop ()
  "Load the project's desktop if available."
  (ignore-errors
    (setq default-directory project-persist-current-project-settings-dir)
    (message (format "Loading project desktop from %s" default-directory))
    (desktop-read project-persist-current-project-settings-dir)))

 ;; Kill all file-based buffers and unpin the speedbar before opening a project.
(add-hook 'project-persist-before-load-hook
          (lambda ()
            (graphene-unpin-speedbar)
            (kill-all-buffers)))

 ;; Kill all file-based buffers and unpin the speedbar after closing a project.
(add-hook 'project-persist-after-close-hook
          (lambda ()
            (kill-all-buffers)
            (graphene-unpin-speedbar)))

;; Set the project root directory, load the project desktop and update speedbar.
(add-hook 'project-persist-after-load-hook
          (lambda ()
            (graphene-load-project-desktop)
            (graphene-set-project-root project-persist-current-project-root-dir)))

;; Save the project desktop.
(add-hook 'project-persist-after-save-hook
          (lambda ()
            (message (format "Saving project desktop in %s" project-persist-current-project-settings-dir))
            (desktop-save project-persist-current-project-settings-dir)))

;; http://www.emacswiki.org/DeskTop#toc4: Overriding stale desktop locks
;;; desktop-override-stale-locks.el begins here
(defun emacs-process-p (pid)
  "If pid is the process ID of an emacs process, return t, else nil.
Also returns nil if pid is nil."
  (when pid
    (let ((attributes (process-attributes pid)) (cmd))
      (dolist (attr attributes)
        (if (string= "comm" (car attr))
            (setq cmd (cdr attr))))
      (if (and cmd (or (string= "emacs" cmd) (string= "emacs.exe" cmd))) t))))

(defadvice desktop-owner (after pry-from-cold-dead-hands activate)
  "Don't allow dead emacsen to own the desktop file."
  (when (not (emacs-process-p ad-return-value))
    (setq ad-return-value nil)))
;;; desktop-override-stale-locks.el ends here

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

(add-hook 'ruby-mode-hook
          (lambda ()
            (define-key (current-local-map) [remap newline] 'reindent-then-newline-and-indent)))

(global-set-key (kbd "C-x m") 'magit-status)

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

(sacha/package-install 'expand-region)
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

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
