;;; language-others.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; PHP
(use-package php-mode
  :ensure t
  :bind (:map php-mode-map
              ("C-." . nil)))

;; Rust
(use-package rust-mode
  :ensure t
  :when (executable-find "rust-analyzer")
  :hook (rust-mode . eglot-ensure)
  :init
  (setq rust-format-on-save t
        rust-mode-treesitter-derive (my/treesit-available-p)))

(use-package dart-mode
  :ensure t
  :when (executable-find "dart")
  :hook (dart-mode . eglot-ensure))

(provide 'language-others)
;;; language-others.el ends here
