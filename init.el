;;; init.el --- KCMacs Configuration -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

;; Load path
;; Optimize: Force "lisp"" and "site-lisp" at the head to reduce the startup time.

(defun update-load-path (&rest _)
  "Update `load-path'."
  (dolist (dir '("lisp"))
    (push (expand-file-name dir user-emacs-directory) load-path)))

(update-load-path)

(defun open-config-file()
  "Open init.el file."
  (interactive)
  (find-file "~/.emacs.d/init.el")
  )
(global-set-key (kbd "<f2>") 'open-config-file)

(icomplete-mode t)
(setq make-backup-files nil)
(global-hl-line-mode t)

;; Requisites
(require 'init-const)
(require 'init-custom)
(require 'init-funcs)

;; Packages
(require 'init-package)

(require 'init-keybinding)
;; Preference
(require 'init-base)
(require 'init-ui)
(require 'init-edit)
(require 'init-completion)
;;(require 'init-corfu)

(require 'init-dashboard)
(require 'init-projectile)

(require 'init-lsp)

;;; init.el ends here
