;;; toolkit-grep.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; grep
(use-package grep
  :init
  (setq-default grep-highlight-matches t
		grep-scroll-output t))

;; wgrep
(use-package wgrep
  :ensure t
  :config
  (setq wgrep-enable-key "e"
        wgrep-auto-save-buffer t))

;; agtags
(use-package agtags
  :ensure t
  :when (executable-find "global")
  :config
  (setq agtags-global-treat-text t)
  (dolist (item '("GPATH" "GTAGS" "GRTAGS"))
    (add-to-list 'grep-find-ignored-files item))

  (defun my/agtags-mode-on()
    (agtags-mode 1)
    (diminish 'agtags-mode))
  
  (agtags-bind-keys)
  (dolist (hook '(prog-mode-hook text-mode-hook))
    (add-hook hook 'my/agtags-mode-on)))

(provide 'toolkit-grep)
;;; toolkit-grep.el ends here
