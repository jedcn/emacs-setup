;; I am looking to try out projectile. It's pretty neat.
;;
;; The Marmalade version is at.. 0.8? and it seems Projectile is at
;; 0.9.+, so, the current plan is to clone it and reference it here.
;;
;; I installed it via package.el initially, and this brought in the
;; dependencies "dash" and "s" via ELPA.
;;
(add-to-list 'load-path "~/d/projectile")
(require 'projectile)
(projectile-global-mode)
(setq projectile-enable-caching t)
