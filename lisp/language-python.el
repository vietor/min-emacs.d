;;; language-python.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package python
  :init
  (setq python-shell-interpreter "python3")
  (add-to-list 'my/eglot-language-grouped-modes 'python-mode)

  (defun my/python-workspace-configuration (current-root)
    (let ((venv-path (expand-file-name ".venv" current-root)))
      (when (file-directory-p venv-path)
        `(:pylsp (:plugins (:jedi (:environment ,venv-path)))))))
  (add-to-list 'my/eglot-workspace-configuration-alist
               '("python" . my/python-workspace-configuration)))

(use-package pip-requirements
  :ensure t)

(provide 'language-python)
;;; language-python.el ends here
