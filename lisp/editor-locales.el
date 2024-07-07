;;; editor-locales.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq system-time-locale "C")
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))

(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(unless (eq system-type 'windows-nt)
  (set-selection-coding-system 'utf-8))

;; Better font usage

(defun my/font-installed-p (font-name)
  (find-font (font-spec :name font-name)))

(dolist (font-name '("Noto Color Emoji" "Apple Color Emoji" "Segoe UI Emoji"))
  (when (my/font-installed-p font-name)
    (set-fontset-font t 'emoji (font-spec :family font-name) nil 'prepend)))

(provide 'editor-locales)
;;; editor-locales.el ends here
