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
      confirm-kill-processes nil
      initial-major-mode 'fundamental-mode)
(setq-default case-fold-search t
              column-number-mode t
              truncate-lines nil
              truncate-partial-width-windows nil
              indicate-empty-lines nil
              indicate-buffer-boundaries nil
              sentence-end-double-space nil
              tab-bar-show nil
              bidi-display-reordering nil)

;; Disable ispell backends in text-mode
(setq-default text-mode-ispell-word-completion nil)

;; Better cursor
(blink-cursor-mode -1)
(setq x-stretch-cursor nil)

;; Better selection
(cua-selection-mode t)
(setq-default shift-select-mode nil)
(add-hook 'after-init-hook 'delete-selection-mode)

;; Better mark operation
(global-set-key (kbd "M-SPC") 'set-mark-command)
(setq-default set-mark-command-repeat-pop t)
(add-hook 'after-init-hook 'transient-mark-mode)

;; Better paren highlight
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

;; Disable dialog
(setq use-dialog-box nil
      use-file-dialog nil)

;; Better indent behavior
(setq-default tab-width 4
              standard-indent 4
              indent-tabs-mode nil)
(add-hook 'after-init-hook 'electric-indent-mode)

;; Better scroll behavior
(setq jit-lock-defer-time 0
      fast-but-imprecise-scrolling t
      redisplay-skip-fontification-on-input t)
(setq-default scroll-preserve-screen-position 'always)
(when (fboundp 'pixel-scroll-precision-mode)
  (add-hook 'after-init-hook 'pixel-scroll-precision-mode))

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
(setq-default recentf-max-saved-items 300
              recentf-exclude `("/tmp/" "/ssh:"))
(add-hook 'after-init-hook 'recentf-mode)

;; Disable startup time
(add-hook 'emacs-startup-hook
          (lambda()
            (message "Load completed in %.02fs."
                     (float-time (time-subtract (current-time)
                                                before-init-time)))))

(provide 'editor-basic)
;;; editor-basic.el ends here
