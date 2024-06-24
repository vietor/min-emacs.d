;;; editor-enhance.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Use key help
(use-package which-key
  :ensure t
  :diminish
  :hook (after-init . which-key-mode)
  :config
  (setq-default which-key-idle-delay 1.5))

;; Move text lines
(use-package move-dup
  :ensure t
  :diminish
  :hook (after-init . global-move-dup-mode))

;; Multiple cursors editor
(use-package multiple-cursors
  :ensure t
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-M->" . mc/skip-to-next-like-this)
         ("C-M-<" .  mc/skip-to-previous-like-this)
         ("M-<down-mouse-1>" . nil)
         ("M-<mouse-1>" . mc/add-cursor-on-click))
  :init
  (defun mc/save-lists () "Ignore save history."))

(provide 'editor-enhance)
;;; editor-enhance.el ends here
