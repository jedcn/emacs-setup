
(defun cleanup-buffer-safe ()
  "Perform a bunch of safe operations on the whitespace content of a buffer."
  (interactive)
  (untabify (point-min) (point-max))
  (delete-trailing-whitespace)
  (set-buffer-file-coding-system 'utf-8))

(add-hook 'before-save-hook 'cleanup-buffer-safe)
