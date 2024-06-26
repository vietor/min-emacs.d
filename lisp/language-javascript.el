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
  (setq-default js-indent-level 2))

(use-package prettier-js
  :ensure t
  :when (executable-find "prettier")
  :commands (prettier-js)
  :init
  (add-to-list 'my/formatter-beautify-alist '(js-mode . prettier-js))
  (add-to-list 'my/formatter-beautify-alist '(js-ts-mode . prettier-js)))

(provide 'language-javascript)
;;; language-javascript.el ends here
