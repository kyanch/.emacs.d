;; -*- lexical-binding: t -*-

(use-package csv-mode)
(use-package cmake-mode)
(use-package lua-mode)
(use-package csharp-mode)

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook (((c-mode c++-mode cmake-mode) . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

(use-package lsp-ui :commands lsp-ui-mode)

(use-package dap-mode)

(setq compilation-read-command nil)

(set-default-coding-systems 'utf-8)

(global-set-key (kbd "<f5>") 'compile)

(provide 'init-lsp)
