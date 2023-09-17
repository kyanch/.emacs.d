;;; init-funcs.el --- Define some functions -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
(require 'init-const)

(defun font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (find-font (font-spec :name font-name)))

(defun too-long-file-p ()
  "Check whether the file is too long."
  (if (fboundp 'buffer-line-statistics)
      (> (car (buffer-line-statistics)) 10000)
    (> (buffer-size) 100000)))

(defun icons-displayable-p ()
  "Return non-nil if icons are displayable."
  (and t
       (or (featurep 'nerd-icons)
           (require 'nerd-icons nil t))))


(provide 'init-funcs)
