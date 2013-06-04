
(defun sacha/package-install (package &optional repository)
  "Install PACKAGE if it has not yet been installed.
If REPOSITORY is specified, use that."
  (unless (package-installed-p package)
    (let ((package-archives (if repository
                                (list (assoc repository package-archives))
                              package-archives)))
    (package-install package))))

(sacha/package-install 'markdown-mode)

(sacha/package-install 'puppet-mode)

(sacha/package-install 'haml-mode)
