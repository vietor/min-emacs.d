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

(defun my/try-set-font (characters font-names)
  (cl-loop for font-name in font-names
           do (let ((font (font-spec :name font-name)))
                (when (find-font font)
                  (set-fontset-font t characters font nil 'prepend)
                  (cl-return)))))

(defun my/setup-better-fonts ()
  (my/try-set-font 'emoji '("Noto Color Emoji"
                            "Apple Color Emoji"
                            "Segoe UI Emoji"))
  (my/try-set-font 'han '("Microsoft Yahei UI")))
(add-hook 'window-setup-hook #'my/setup-better-fonts)
(add-hook 'server-after-make-frame-hook #'my/setup-better-fonts)

(provide 'editor-locales)
;;; editor-locales.el ends here
