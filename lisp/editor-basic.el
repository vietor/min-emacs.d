;;; editor-basic.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Load better theme
(load-theme 'tango-dark)

;; Clean starup screen
(setq inhibit-startup-screen t
      initial-scratch-message "")

;; Use text as default
(setq-default major-mode 'text-mode)
(setq initial-major-mode 'text-mode)

;; Use better behavior
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(setq-default case-fold-search t
              column-number-mode t
              truncate-lines nil
              indicate-empty-lines t
              sentence-end-double-space nil)

;; Use better selection
(cua-selection-mode t)
(setq-default shift-select-mode nil)
(add-hook 'after-init-hook 'delete-selection-mode)

;; Use better mark operation
(global-set-key (kbd "C-.") 'set-mark-command)
(global-set-key (kbd "C-x C-.") 'pop-global-mark)
(setq-default set-mark-command-repeat-pop t)
(add-hook 'after-init-hook 'transient-mark-mode)

;; Use better paren highlight
(add-hook 'after-init-hook 'show-paren-mode)
(when (fboundp 'electric-pair-mode)
  (add-hook 'after-init-hook 'electric-pair-mode))

;; Disable file lock & backup
(setq create-lockfiles nil
      make-backup-files nil
      auto-save-default nil
      auto-save-list-file-prefix nil)

;; Disable bell
(setq visible-bell nil
      ring-bell-function 'ignore)

;; Use better indent behavior
(setq-default tab-width 4
              standard-indent 4
              indent-tabs-mode nil)
(add-hook 'after-init-hook 'electric-indent-mode)
(add-hook 'c-mode-common-hook
          (lambda() (when indent-tabs-mode
		      (setq tab-width c-basic-offset))))

;; Use better mouse behavior

(when window-system
  (mouse-wheel-mode t)
  (setq mouse-wheel-scroll-amount '(1
                                    ((shift) . 5)
                                    ((control))))
  ;; Disable ugly text-scale
  (defun mouse-wheel-text-scale (event)
    (interactive (list last-input-event))))

(when (fboundp 'pixel-scroll-precision-mode)
  (pixel-scroll-precision-mode))

;; Use better `bind-key'
(bind-key "C-h" 'delete-backward-char)
(bind-key "M-g r" 'replace-string)
(bind-key "M-g f r" 'rename-visited-file)

(provide 'editor-basic)
;;; editor-basic.el ends here
