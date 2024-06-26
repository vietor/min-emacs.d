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

(provide 'editor-locales)
;;; editor-locales.el ends here
