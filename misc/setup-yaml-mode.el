;; Haven't verified that this works on direct startup-- only in
;; *scratch* once we're up and running
;;
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
