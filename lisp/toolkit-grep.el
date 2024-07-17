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
  :diminish
  :when (executable-find "global")
  :config
  (setq agtags-global-treat-text t)
  (dolist (file agtags-created-tag-files)
    (add-to-list 'grep-find-ignored-files file))

  (agtags-bind-keys)
  (dolist (hook '(prog-mode-hook text-mode-hook))
    (add-hook hook 'agtags-mode)))

(provide 'toolkit-grep)
;;; toolkit-grep.el ends here
