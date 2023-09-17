;; init-ui.el --- Better lookings and appearances.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Visual (UI) configurations for better lookings and appearances.
;;

;;; Code:

(eval-when-compile
  (require 'init-const)
  (require 'init-custom))


;; Theme

;; builtin theme
;;(load-theme 'wombat)
(use-package doom-themes
  :demand t
  :config (load-theme 'doom-one t))

;; Icons
(use-package nerd-icons
  :config
  (when (and (display-graphic-p)
             (not (font-installed-p nerd-icons-font-family)))
    (nerd-icons-install-fonts)))

;; Modeline
(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :custom ((doom-modeline-height 15)))

;; rainbow delimiters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package neotree
  :bind ("<f6>" . neotree-toggle))

;; Which-key
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

;; Ligatures support
;; TODO

;; Window Manage
(setq display-buffer-alist
      '(
        ("\\*Help\\*" display-buffer-reuse-window
         (reusable-frames . visible)
         (window-height . 0.3)
         (window-parameter . ((side . bottom))))
        ("\\*scratch.*\\*" display-buffer-same-window)
        ("\\*magit.*\\*" display-buffer-in-side-window
         (window-side . right)
         (window-slot . 0))
        )
      )

(provide 'init-ui)
;;; init-ui.el ends here
