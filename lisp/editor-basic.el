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

;; Use text editor as default
(setq-default major-mode 'text-mode)
(setq initial-major-mode 'text-mode)

;; Use better editor behavior

(setq-default column-number-mode t
              truncate-lines nil
              indicate-empty-lines t
              sentence-end-double-space nil
              shift-select-mode nil
              case-fold-search t)

(cua-selection-mode t)
(add-hook 'after-init-hook 'show-paren-mode)
(add-hook 'after-init-hook 'transient-mark-mode)
(add-hook 'after-init-hook 'delete-selection-mode)

(provide 'editor-basic)
;;; editor-basic.el ends here
