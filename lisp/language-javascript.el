;;; language-javascript.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; JSON
(use-package json-mode
  :ensure t
  :init
  (add-to-list 'my/treesit-remap-langs "json")

  (defun my/json-formatter-beautify ()
    (call-interactively 'json-mode-beautify))
  (dolist (entity '((json-mode . my/json-formatter-beautify)
                    (json-ts-mode . my/json-formatter-beautify)))
    (add-to-list 'my/formatter-beautify-alist entity)))

;; Javascript
(use-package js-mode
  :mode "\\.\\(\\js\\|es6\\|mjs\\)\\'"
  :bind ((:map js-mode-map
               ("M-." . nil))
         (:map js-ts-mode-map
               ("M-." . nil)))
  :init
  (setq-default js-indent-level 2)
  (add-to-list 'my/treesit-remap-langs "js"))

(if (my/treesit-available-p)
    (use-package typescript-ts-mode)
  (use-package typescript-mode
    :ensure t
    :mode ("\\.ts[x]\\'" . typescript-mode)))

(use-package prettier-js
  :ensure t
  :when (executable-find "prettier")
  :commands (prettier-js)
  :init
  (dolist (entity '((js-mode . prettier-js)
                    (js-ts-mode . prettier-js)
                    (typescript-mode . prettier-js)
                    (typescript-ts-mode . prettier-js)))
    (add-to-list 'my/formatter-beautify-alist entity)))

(when (executable-find "typescript-language-server")
  (dolist (hook '(js-mode-hook
                  js-ts-mode-hook
                  tsx-ts-mode-hook
                  typescript-ts-mode-hook
                  typescript-mode-hook))
    (add-hook hook 'eglot-ensure))
  (add-to-list 'my/eglot-language-alias-key '("typescript" . "javascript")))

(provide 'language-javascript)
;;; language-javascript.el ends here
