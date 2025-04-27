;;; language-web.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; CSS
(setq-default css-fontify-colors nil)

;; HTML
(use-package web-mode
  :ensure t
  :mode ("\\.ejs\\'" "\\.njk\\'" "\\.html?\\'")
  :config
  (setq-default web-mode-markup-indent-offset 2
                web-mode-css-indent-offset 2
                web-mode-code-indent-offset 2)
  (addx-to-list 'web-mode-indentation-params
                '(("lineup-args" . nil)
                  ("lineup-calls" . nil)
                  ("lineup-concats" . nil)
                  ("lineup-ternary" . nil))))

(provide 'language-web)
;;; language-web.el ends here
