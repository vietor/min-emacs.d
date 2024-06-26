;;; language-javascript.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package js-mode
  :mode "\\.\\(\\js\\|es6\\|mjs\\)\\(\\.erb\\)?\\'"
  :bind ((:map js-mode-map
               ("M-." . nil))
         (:map js-ts-mode-map
               ("M-." . nil)))
  :init
  (setq-default js-indent-level 2)
  (add-to-list 'my/treesit-active-langs "js"))

(use-package typescript-mode
  :ensure t
  :init
  (add-to-list 'my/treesit-active-langs "typescript"))

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

(provide 'language-javascript)
;;; language-javascript.el ends here
