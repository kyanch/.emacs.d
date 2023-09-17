;; init-base.el --- Better default configurations.	-*- lexical-binding: t -*-
;;; Commentary:
;;
;; Better defaults.
;;

;;; Code:

(require 'init-funcs)
(setq user-full-name kcmac-full-name
      user-mail-address kcmac-mail-address)

(with-no-warnings
  ;; Key Modifiers
  (cond
   (sys/win32p
    (setq w32-apps-modifier 'super)
    (w32-register-hot-key [s-t]))
   (sys/mac-port-p)
   ;; TODo
   )
  (bind-keys ([(super a)] . mark-whole-buffer)
             ([(super c)] . kill-ring-save)
             ([(super s)] . save-buffer)
             ([(super v)] . yank)
             ([(super z)] . undo)
             ([(super x)] . kill-region))
  ;; Todo: Optimization
  (when sys/win32p
    (setq w32-get-true-file-attributes nil   ; decrease file IO worklaod
          w32-use-native-image-API t         ; use native w32 API
          w32-pipe-read-delay 0              ; faster IPC
          w32-pipe-buffer-size (* 64 1024))) ; read more at a time (was 4K)
  (unless sys/macp
    (setq command-line-ns-option-alist nil))
  (unless sys/linuxp
    (setq command-line-x-option-alist nil))

  ;; Increase how much is read from processes in a single chunk (default is 4kb)
  (setq read-process-output-max (* 1024 1024))  ; 64kb

  ;; Don't ping things that look like domain names.
  (setq ffap-machine-p-known 'reject))

;; Garbage Collector Magic Hack
(use-package gcmh
  :diminish
  :hook (emacs-startup . gcmh-mode)
  :init
  (setq gcmh-idle-delay 'auto
        gcmh-auto-idle-delay-factor 10
        gcmh-high-cons-threshold #x1000000)) ; 16MB

;; Set UTF-8 as the default coding system
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))
(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(setq system-time-locale "C")
(unless sys/win32p
  (set-selection-coding-system 'utf-8))

(use-package simple
  :ensure nil
  :demand t
  :hook((after-init . size-indication-mode)
        (text-mode . visual-line-mode)
        ((prog-mode markdown-mode conf-mode) . enable-trailing-whitespace))
  :init
  (setq column-number-mode t
        inhibit-startup-message t
        line-move-visual nil
        track-eol t
        set-mark-command-repeat-pop t)
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (tooltip-mode -1)
  (set-fringe-mode 10)
  ;; Set line number
  (column-number-mode)
  (global-display-line-numbers-mode)
  ;; Disable line numbers for some modes
  (dolist (mode '(org-mode-hook
                  term-mode-hook
                  eshell-mode-hook))
    (add-hook mode (lambda() (display-line-numbers-mode 0))))

  ;; Visualize TAB, (HARD) SPACE, NEWLINE
  (setq-default show-trailing-whitespace nil) ; Don't show trailing whitespace by default
  (defun enable-trailing-whitespace ()
    "Show trailing spaces and delete on saving."
    (setq show-trailing-whitespace t)
    (add-hook 'before-save-hook #'delete-trailing-whitespace nil t))
  ;; Prettify the process list
  ;;  TODO
  )

;; Misc
(if (boundp 'use-short-answers)
    (setq use-short-answers t)
  (fset 'yes-or-no-p 'y-or-n-p))

(setq-default major-mode 'text-mode
	          fill-column 80
	          tab-width 4
	          indent-tabs-mode nil)

(setq visible-bell t
      inhibit-compacting-font-caches t  ; Don’t compact font caches during GC
      delete-by-moving-to-trash t       ; Deleting files go to OS's trash folder
      make-backup-files nil             ; Forbide to make backup files
      auto-save-default nil             ; Disable auto save

      uniquify-buffer-name-style 'post-forward-angle-brackets ; Show path if names are same
      adaptive-fill-regexp "[ t]+|[ t]*([0-9]+.|*+)[ t]*"
      adaptive-fill-first-line-regexp "^* *$"
      sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*"
      sentence-end-double-space nil
      word-wrap-by-category t)

;; Sqlite
(when (fboundp 'sqlite-open)
  (use-package emacsql-sqlite-builtin))

;; set Font (directly)
(set-face-attribute 'default nil
		    :family "FiraCode Nerd Font"
		    :height 110)

(set-fontset-font t 'symbol (font-spec :family "Symbol") nil 'prepend)

(set-fontset-font t '(#x4e00 . #x9fff) (font-spec :family "Microsoft Yahei"))



(provide 'init-base)
;;; init-base.el ends here
