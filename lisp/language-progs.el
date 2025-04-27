;;; language-progs.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; PHP
(use-package php-mode
  :ensure t)

;; Rust
(use-package rust-mode
  :ensure t
  :when (executable-find "rust-analyzer")
  :init
  (setq rust-format-on-save t
        rust-mode-treesitter-derive (my/treesit-available-p))
  (add-to-list 'my/eglot-language-grouped-modes 'rust-mode))

(use-package dart-mode
  :ensure t
  :when (executable-find "dart")
  :init
  (add-to-list 'my/eglot-language-grouped-modes 'dart-mode))

(provide 'language-progs)
;;; language-progs.el ends here
