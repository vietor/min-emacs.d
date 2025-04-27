;;; language-javascript.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; JSON
(use-package json-mode
  :ensure t
  :init
  (add-to-list 'my/eglot-language-ignore-modes 'json-mode)

  (defun my/json-formatter-beautify ()
    (call-interactively 'json-mode-beautify))
  (addx-to-list 'my/formatter-beautify-alist
                '((json-mode . my/json-formatter-beautify)
                  (json-ts-mode . my/json-formatter-beautify))))

;; Javascript
(use-package js-mode
  :mode "\\.\\(\\js\\|es6\\|mjs\\)\\'"
  :bind ((:map js-mode-map
               ("M-." . nil))
         (:map js-ts-mode-map
               ("M-." . nil)))
  :init
  (setq-default js-indent-level 2))

(if (my/treesit-available-p)
    (use-package typescript-ts-mode)
  (use-package typescript-mode
    :ensure t
    :mode ("\\.ts[x]\\'" . typescript-mode)))

(addx-to-list 'my/formatter-beautify-prettier-modes
              '(js-mode
                js-ts-mode
                tsx-ts-mode
                typescript-mode
                typescript-ts-mode))

(when (executable-find "typescript-language-server")
  (add-to-list 'my/eglot-language-grouped-modes 'js-mode)
  (addx-to-list 'my/eglot-language-alias-key
                '(("typescript" . "javascript")
                  ("typescriptreact" . "javascript"))))

(provide 'language-javascript)
;;; language-javascript.el ends here
