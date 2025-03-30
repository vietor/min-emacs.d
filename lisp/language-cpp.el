;;; language-cpp.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package cc-mode
  :when (executable-find "clangd")
  :init
  (addx-to-list 'my/treesit-remap-langs
                '("c" "c++"))
  (addx-hook '(c-mode-hook
               c++-mode-hook
               c-ts-mode-hook
               c++-ts-mode-hook)
             'eglot-ensure))

(when (my/treesit-available-p)
  (use-package cmake-ts-mode))

(provide 'language-cpp)
;;; language-cpp.el ends here
