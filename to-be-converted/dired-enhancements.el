;; Learned about dired-details from what-the-emacs-d, but saw that
;; dired-details+ was available and I don't see any downside to it.
;;
;; Details on dired-details+: http://www.emacswiki.org/DiredDetails
;;
;;
(require 'dired-details+)

;; Whether it's dired-details or dired-details+, the following makes
;; it so that M-< and M-> jump to the first or last file in the
;; directory. This one is from what-the-emacs-d:
;; http://whattheemacsd.com/setup-dired.el-02.html
(defun dired-back-to-top ()
  (interactive)
  (beginning-of-buffer)
  (dired-next-line 4))

(define-key dired-mode-map
  (vector 'remap 'beginning-of-buffer) 'dired-back-to-top)

(defun dired-jump-to-bottom ()
  (interactive)
  (end-of-buffer)
  (dired-next-line -1))

(define-key dired-mode-map
  (vector 'remap 'end-of-buffer) 'dired-jump-to-bottom)
