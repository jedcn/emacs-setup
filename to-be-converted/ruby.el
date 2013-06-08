;;
;; This is straight-up from emacs starter kit: starter-kit-ruby.el
;;
;; However, some mis-match between starter-kit-ruby.el and a version
;; of inf-ruby was giving me constant problems, so relevant parts have
;; been inlined here

;;
;; turn on ruby-mode whenever editing a file that matches these names..
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Vagrantfile$" . ruby-mode))

;;
;; Don't use these, but maybe someday?
;;(add-to-list 'auto-mode-alist '("\\.thor$" . ruby-mode))
;;(add-to-list 'auto-mode-alist '("Thorfile$" . ruby-mode))
