;;; toolkit-complete.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq tab-always-indent 'complete
      completion-category-defaults nil
      completion-category-overrides nil
      completion-cycle-threshold 4)

(use-package vertico
  :ensure t
  :hook (after-init . vertico-mode))

(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless basic)))

(use-package corfu
  :ensure t
  :hook (after-init . global-corfu-mode)
  :config
  (setq-default corfu-auto t
                corfu-preview-current nil
                corfu-quit-no-match 'separator)

  (with-eval-after-load 'corfu
    (corfu-popupinfo-mode)))

(use-package corfu-terminal
  :ensure t
  :after corfu
  :config
  (with-eval-after-load 'corfu
    (corfu-terminal-mode)))

(provide 'toolkit-complete)
;;; toolkit-complete.el ends here
