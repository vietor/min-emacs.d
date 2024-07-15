;;; language-web.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; CSS
(setq-default css-fontify-colors nil)
(add-to-list 'my/treesit-remap-langs "css")

;; HTML
(use-package web-mode
  :ensure t
  :mode ("\\.ejs\\'" "\\.njk\\'" "\\.html?\\'")
  :config
  (dolist (param '(("lineup-args" . nil)
                   ("lineup-calls" . nil)
                   ("lineup-concats" . nil)
                   ("lineup-ternary" . nil)))
    (add-to-list 'web-mode-indentation-params param)))

(provide 'language-web)
;;; language-web.el ends here
