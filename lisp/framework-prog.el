;;; framework-prog.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Display line numbers
(when (fboundp 'display-line-numbers-mode)
  (setq-default display-line-numbers-width 3)
  (add-hook 'prog-mode-hook 'display-line-numbers-mode))

;; Fixup `tab-with' in c mode
(defun my/fixup-c-style-tab-width()
  (when indent-tabs-mode
    (setq tab-width c-basic-offset)))
(add-hook 'c-mode-common-hook 'my/fixup-c-style-tab-width)

(use-package eldoc
  :diminish
  :init
  (when (fboundp 'global-eldoc-mode)
    (add-hook 'after-init-hook 'global-eldoc-mode)))

(use-package flymake
  :bind (:map flymake-mode-map
              ("M-n" . flymake-goto-next-error)
              ("M-p" . flymake-goto-prev-error))
  :config
  (setq-default flymake-mode-line-lighter "🪰"))

(provide 'framework-prog)
;;; framework-prog.el ends here
