;;; editor-basic.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Better keys
(bind-key "C-h" 'delete-backward-char)
(bind-key "M-g r" 'replace-string)
(bind-key "M-g f r" 'rename-visited-file)

;; Clean starup screen
(setq inhibit-startup-screen t
      initial-scratch-message nil
      frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))
(advice-add #'display-startup-echo-area-message :override #'ignore)

;; Better behavior

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(setq use-short-answers t
      initial-major-mode 'fundamental-mode)
(setq-default case-fold-search t
              column-number-mode t
              truncate-lines nil
              truncate-partial-width-windows nil
              indicate-empty-lines nil
              indicate-buffer-boundaries nil
              sentence-end-double-space nil
              bidi-display-reordering nil)

;; Better cursor
(blink-cursor-mode -1)
(setq x-stretch-cursor nil)

;; Better selection
(cua-selection-mode t)
(setq-default shift-select-mode nil)
(add-hook 'after-init-hook 'delete-selection-mode)

;; Better mark operation
(global-set-key (kbd "C-.") 'set-mark-command)
(global-set-key (kbd "C-x C-.") 'pop-global-mark)
(setq-default set-mark-command-repeat-pop t)
(add-hook 'after-init-hook 'transient-mark-mode)

;; Better paren highlight
(add-hook 'after-init-hook 'show-paren-mode)
(when (fboundp 'electric-pair-mode)
  (add-hook 'after-init-hook 'electric-pair-mode))

;; Active eldoc
(when (fboundp 'global-eldoc-mode)
  (add-hook 'after-init-hook 'global-eldoc-mode))

;; Disable line numbers
(when (fboundp 'display-line-numbers-mode)
  (setq-default display-line-numbers-width 3)
  (add-hook 'prog-mode-hook 'display-line-numbers-mode))

;; Disable file lock & backup
(setq create-lockfiles nil
      make-backup-files nil
      auto-save-default nil
      auto-save-list-file-prefix nil)

;; Disable bell
(setq visible-bell nil
      ring-bell-function 'ignore)

;; Disable dialog
(setq use-dialog-box nil
      use-file-dialog nil)

;; Better indent behavior
(setq-default tab-width 4
              standard-indent 4
              indent-tabs-mode nil)
(add-hook 'after-init-hook 'electric-indent-mode)
(add-hook 'c-mode-common-hook
          (lambda() (when indent-tabs-mode
                      (setq tab-width c-basic-offset))))

;; Better scroll behavior
(setq-default scroll-preserve-screen-position 'always)
(when (fboundp 'pixel-scroll-precision-mode)
  (pixel-scroll-precision-mode))

;; Better mouse behavior
(when window-system
  (mouse-wheel-mode t)
  (setq mouse-yank-at-point t
        mouse-wheel-scroll-amount '(1
                                    ((shift) . 5)
                                    ((control))))
  ;; Disable ugly text-scale
  (defun mouse-wheel-text-scale (event)
    (interactive (list last-input-event))))

;; Better long lines behavior
(when (fboundp 'so-long-mode)
  (add-hook 'after-init-hook 'global-so-long-mode))

;; Active recent file history
(bind-key "M-g f l" 'recentf-open)
(setq-default recentf-max-saved-items 1000
              recentf-exclude `("/tmp/" "/ssh:"))
(add-hook 'after-init-hook 'recentf-mode)

(provide 'editor-basic)
;;; editor-basic.el ends here
