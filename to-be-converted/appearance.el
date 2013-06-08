(custom-set-faces
 ;; TODO: Deconstruct..
 '(highlight ((((class color) (min-colors 88) (background dark)) (:background "#111111")))))

;; Prefer dark backgrounds with white text
(set-face-background 'default "Black")
(set-face-foreground 'default "White")

;; TODO: Document. 'region described the background color that is
;; shown when you move your cursor away from a region?
(set-face-background 'region "#464740")
(set-face-foreground 'font-lock-warning-face "#ff6666")

;; Matching parents get blue-ish, mismatched go red
(set-face-background 'show-paren-match "#333399")
(set-face-background 'show-paren-mismatch "Red")

(when window-system
  ;; Make the title of emacs reflect the buffer being edited
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  ;; Make the cursor blink when inactive
  (blink-cursor-mode 1))
