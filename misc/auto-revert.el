;; What happens if a file in an Emacs buffer changes on disk?
;;
;; Automatically reload. Helpful after a git checkout..
(global-auto-revert-mode 1)

;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; See:
;;
;; https://github.com/magnars/.emacs.d/blob/master/sane-defaults.el
