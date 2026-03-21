;;; language-cpp.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package cc-mode
  :when (executable-find "clangd")
  :init
  (add-to-list 'my/eglot-language-grouped-modes 'c-mode))

(when my/treesit-is-available
  (use-package cmake-ts-mode))

(provide 'language-cpp)
;;; language-cpp.el ends here
