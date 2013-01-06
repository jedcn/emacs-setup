;; This mode requires that the 'markdown' executable be in your path.
;;
;; I 'got' markdown with 'brew install markdown'
;;
;; You can change the markdown executable, or read more about the
;; mode, here: http://jblevins.org/projects/markdown-mode/
;;
;; Highlights:
;;
;; * C-c C-c p
;;   Run markdown on buffer contents. Open result in browser.
;;
(autoload 'markdown-mode "markdown-mode.el"
  "Major mode for editing Markdown files" t)
(setq auto-mode-alist
   (cons '("\\.md" . markdown-mode) auto-mode-alist))
