;;; editor-basic.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Clean startup gui
(when (fboundp 'tooltip-mode)
  (tooltip-mode -1))
(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(when (fboundp 'horizontal-scroll-bar-mode)
  (horizontal-scroll-bar-mode -1))

;; Clean starup screen
(setq inhibit-startup-screen t
      initial-scratch-message "")

;; Use text as default
(setq-default major-mode 'text-mode)
(setq initial-major-mode 'text-mode)

;; Use bettert behavior
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

(provide 'editor-basic)
;;; editor-basic.el ends here
