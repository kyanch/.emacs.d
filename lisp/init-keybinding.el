;;; init-keybinding.el --- Setup keybindings packages  -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

;; Set <ESC> as <C-g>
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; general Keybinding
;; (use-package general
;;   :config
;;   (general-evil-setup t)
;;   (general-create-definer rune/leader-keys
;;     :keymaps '(normal insert visual emacs)
;;     :prefix "SPC"
;;     :global-prefix "C-SPC")

;;   (rune/leader-keys
;;     "t"  '(:ignore t :which-key "toggles")
;;     "tt" '(load-theme :which-key "choose theme")))

;; hydra
;; (use-package hydra)

;; (defhydra hydra-text-scale (:timeout 4)
;;   "scale text"
;;   ("j" text-scale-increase "in")
;;   ("k" text-scale-decrease "out")
;;   ("f" nil "finished" :exit t))

;; (rune/leader-keys
;;   "ts" '(hydra-text-scale/body :which-key "scale text"))

;; Last, be EVIL

;; (defun rune/evil-hook ()
;;   "..."
;;   (dolist (mode '(custom-mode
;;                   eshell-mode
;;                   git-rebase-mode
;;                   erc-mode
;;                   circe-server-mode
;;                   circe-chat-mode
;;                   circe-query-mode
;;                   sauron-mode
;;                   term-mode))
;;     (add-to-list 'evil-emacs-state-modes mode)))

(use-package evil
  :demand t
  ;; :init
  ;;   (setq evil-want-integration t)
  ;;   (setq evil-want-keybinding nil)
  ;;   (setq evil-want-C-u-scroll t)
  ;;   (setq evil-want-C-i-jump nil)
  ;;   :hook (evil-mode . rune/evil-hook)
  :config
  (evil-mode 1)

  ;;   (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  ;;   (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;;   ;; Use visual line motions even outside of visual-line-mode buffers
  ;;   (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  ;;   (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  ;;   (evil-set-initial-state 'messages-buffer-mode 'normal)
  ;;   (evil-set-initial-state 'dashboard-mode 'normal))

  ;; (use-package evil-collection
  ;;   :after evil
  ;;   :config
  ;;   (evil-collection-init)
  )

(provide 'init-keybinding)
;;; init-keybinding.el ends here
