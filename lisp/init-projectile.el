;; init-projectile.el --- Better lookings and appearances.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Visual (UI) configurations for better lookings and appearances.
;;

;;; Code:
(use-package projectile
  :init (projectile-mode 1)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  ("s-p" . projectile-command-map)
  :custom
  (projectile-project-search-path '("D:/ws/")))


(provide 'init-projectile)
;;; init-projectile.el ends here
