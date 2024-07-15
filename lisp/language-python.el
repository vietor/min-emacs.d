;;; language-python.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package python
  :hook ((python-mode . eglot-ensure)
         (python-ts-mode . eglot-ensure))
  :init
  (setq python-shell-interpreter "python3")
  (add-to-list 'my/treesit-remap-langs "python"))

(use-package pip-requirements
  :ensure t)

(provide 'language-python)
;;; language-python.el ends here
