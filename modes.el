
(defun sacha/package-install (package &optional repository)
  "Install PACKAGE if it has not yet been installed.
If REPOSITORY is specified, use that."
  (unless (package-installed-p package)
    (let ((package-archives (if repository
                                (list (assoc repository package-archives))
                              package-archives)))
    (package-install package))))

(sacha/package-install 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))

(sacha/package-install 'puppet-mode)
(add-to-list 'auto-mode-alist '("\\.pp$" . puppet-mode))

(sacha/package-install 'haml-mode)

(sacha/package-install 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

(add-to-list 'auto-mode-alist '("Vagrantfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))
