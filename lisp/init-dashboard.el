;; init-dashboard.el --- Initialize dashboard configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Dashboard configurations.
;;

;;; Code:

(eval-when-compile
  (require 'init-custom))

(use-package dashboard
  :demand t
  :diminish dashboard-mode
  :config (dashboard-setup-startup-hook)
  :custom
  (dashboard-items '((recents  . 5)
                     (projects . 5)
                     (agenda   . 5)))
  :custom-face
  (dashboard-heading ((t (:inherit (font-lock-string-face bold)))))
  (dashboard-items-face ((t (:weight normal))))
  (dashboard-no-items-face ((t (:weight normal))))
  :bind (("<f1>" . open-dashboard)
         :map dashboard-mode-map
         ("H" . browse-homepage)
         ("R" . restore-session)
         ("L" . restore-session)
         ("S" . find-custom-file)
         ("U" . update-config-and-packages)
         ("q" . quit-dashboard)
         ("h" . dashboard-hydra/body)
         ("?" . dashboard-hydra/body))
  :hook (dashboard-mode . (lambda ()
                            ;; No title
                            (setq-local frame-title-format nil)
                            ;; Enable `page-break-lines-mode'
                            (when (fboundp 'page-break-lines-mode)
                              (page-break-lines-mode 1))))
  )

(provide 'init-dashboard)

;;; init-dashboard.el ends here
