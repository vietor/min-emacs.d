;;; editor-enhance.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Use key help
(use-package which-key
  :ensure t
  :diminish
  :hook (after-init . which-key-mode)
  :config
  (setq-default which-key-idle-delay 1.5))

;; Move text lines
(use-package move-dup
  :ensure t
  :diminish
  :hook (after-init . global-move-dup-mode))

;; Use better completion

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

(provide 'editor-enhance)
;;; editor-enhance.el ends here
