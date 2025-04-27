;;; language-python.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package python
  :init
  (setq python-shell-interpreter "python3")
  (add-to-list 'my/eglot-language-grouped-modes 'python-mode))

(use-package pip-requirements
  :ensure t)

(provide 'language-python)
;;; language-python.el ends here
