;; init-custom.el --- Define customizations.	-*- lexical-binding: t -*-
;;; Commentary:
;;
;; Customization.
;;

;;; Code:

(defgroup kcmac nil
  "KC Emacs customization.")

(defcustom kcmac-logo (expand-file-name
                       (if (display-graphic-p) "logo.png" "banner.txt")
                       user-emacs-directory)
  "Set KCmac logo.  nil means official logo."
  :group 'kcmac
  :type 'string)
(defcustom kcmac-full-name user-full-name
  "Set user full name."
  :group 'kcmac
  :type 'string)

(defcustom kcmac-mail-address user-mail-address
  "Set user email address."
  :group 'kcmac
  :type 'string)
(defcustom kcmac-fonts "FiraCode Nerd Font"
  "Set fonts"
  :group 'kcmac
  :type 'string )
(defcustom kcmac-server t
  "Enable `server-mode' or not."
  :group 'kcmac
  :type 'boolean)

;; Emacs Lisp Package Archive (ELPA)
;; @see https://github.com/melpa/melpa and https://elpa.emacs-china.org/.
(defcustom kcmac-package-archives-alist
  (let ((proto (if (gnutls-available-p) "https" "http")))
    `((melpa    . (("gnu"    . ,(format "%s://elpa.gnu.org/packages/" proto))
                   ("nongnu" . ,(format "%s://elpa.nongnu.org/nongnu/" proto))
                   ("melpa"  . ,(format "%s://melpa.org/packages/" proto))))
      (emacs-cn . (("gnu"    . "http://1.15.88.122/gnu/")
                   ("nongnu" . "http://1.15.88.122/nongnu/")
                   ("melpa"  . "http://1.15.88.122/melpa/")))
      (bfsu     . (("gnu"    . ,(format "%s://mirrors.bfsu.edu.cn/elpa/gnu/" proto))
                   ("nongnu" . ,(format "%s://mirrors.bfsu.edu.cn/elpa/nongnu/" proto))
                   ("melpa"  . ,(format "%s://mirrors.bfsu.edu.cn/elpa/melpa/" proto))))
      (netease  . (("gnu"    . ,(format "%s://mirrors.163.com/elpa/gnu/" proto))
                   ("nongnu" . ,(format "%s://mirrors.163.com/elpa/nongnu/" proto))
                   ("melpa"  . ,(format "%s://mirrors.163.com/elpa/melpa/" proto))))
      (sjtu     . (("gnu"    . ,(format "%s://mirrors.sjtug.sjtu.edu.cn/emacs-elpa/gnu/" proto))
                   ("nongnu" . ,(format "%s://mirrors.sjtug.sjtu.edu.cn/emacs-elpa/nongnu/" proto))
                   ("melpa"  . ,(format "%s://mirrors.sjtug.sjtu.edu.cn/emacs-elpa/melpa/" proto))))
      (tuna     . (("gnu"    . ,(format "%s://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/" proto))
                   ("nongnu" . ,(format "%s://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/" proto))
                   ("melpa"  . ,(format "%s://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/" proto))))
      (ustc     . (("gnu"    . ,(format "%s://mirrors.ustc.edu.cn/elpa/gnu/" proto))
                   ("nongnu" . ,(format "%s://mirrors.ustc.edu.cn/elpa/nongnu/" proto))
                   ("melpa"  . ,(format "%s://mirrors.ustc.edu.cn/elpa/melpa/" proto))))))
  "A list of the package archives."
  :group 'kcmac
  :type '(alist :key-type (symbol :tag "Archive group name")
                :value-type (alist :key-type (string :tag "Archive name")
                                   :value-type (string :tag "URL or directory name"))))
(defun kcmac-setup-fonts()
  "Setup fonts."
  (when (display-graphic-p)
    (cl-loop for font in '("FiraCode Nerd Font")
	     when (font-installed-p font)
	     return (set-face-attribute 'default nil
					:family font
					:height 130))
    )
  )

;; Load `custom-file'
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(provide 'init-custom)
