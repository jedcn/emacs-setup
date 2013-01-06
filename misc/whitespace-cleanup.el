;; Make it so that trailing whitespace is removed and tabs are
;; converted to spaces every time you save.
;;
;; See: http://whattheemacsd.com/buffer-defuns.el-01.html
(defun cleanup-buffer-safe ()
  "Perform a bunch of safe operations on the whitespace content of a buffer."
  (interactive)
  (untabify (point-min) (point-max))
  (delete-trailing-whitespace)
  (set-buffer-file-coding-system 'utf-8))

(add-hook 'before-save-hook 'cleanup-buffer-safe)
