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
  (setq-default agtags-key-prefix "M-g t"
                agtags-global-treat-text t)
  (addx-to-list 'grep-find-ignored-files
                agtags-created-tag-files)

  (agtags-bind-keys)
  (addx-hook '(prog-mode-hook
               text-mode-hook)
             'agtags-mode))

(provide 'toolkit-grep)
;;; toolkit-grep.el ends here
