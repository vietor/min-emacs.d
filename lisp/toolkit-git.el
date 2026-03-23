;;; toolkit-git.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package git-modes
  :ensure t)

(use-package magit
  :ensure t
  :when (executable-find "git")
  :bind ("C-x g" . magit-status)
  :config
  (setq-default magit-diff-refine-hunk 'all))

(provide 'toolkit-git)
;;; toolkit-git.el ends here
