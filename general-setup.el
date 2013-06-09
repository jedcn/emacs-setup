
(package-initialize)

(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/") t)

(set-face-attribute 'default nil :font "Menlo-18")

(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(server-start)
