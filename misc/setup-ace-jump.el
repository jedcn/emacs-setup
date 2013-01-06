;; See: http://www.emacswiki.org/emacs/AceJump
;;
;; C-c SPC ace-jump-word-mode
;;
;;     enter first char of a word, select the highlight key to move to.
;;
;; C-u C-c SPC ace-jump-char-mode
;;
;;     enter a char for query, select the highlight key to move to.
;;
;; C-u C-u C-c SPC ace-jump-line-mode
;;
;;     each non-empty line will be marked, select the highlight key to
;;     move to.
;;
(require 'ace-jump-mode)
(define-key global-map (kbd "C-;") 'ace-jump-mode)
