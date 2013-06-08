;; See
;; http://whattheemacsd.com/init.el-03.html
;;
;; Make it so that you return to the same line number that you were
;; last in when you re-visit a file
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" user-emacs-directory))
