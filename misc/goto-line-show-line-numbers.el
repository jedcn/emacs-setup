;; See: http://whattheemacsd.com/key-bindings.el-01.html
;;
;; 'goto-line is bound to C-x g. Remap it to goto-line-with-feedback
;;
(global-set-key [remap goto-line] 'goto-line-with-feedback)

(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input"
  (interactive)
  (unwind-protect
      (progn
        (linum-mode 1)
        (goto-line (read-number "Goto line: ")))
    (linum-mode -1)))
