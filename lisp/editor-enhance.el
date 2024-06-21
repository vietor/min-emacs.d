;;; editor-enhance.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Simple move lines
(use-package move-dup
  :ensure t
  :diminish
  :hook (after-init . global-move-dup-mode))

(provide 'editor-enhance)
;;; editor-enhance.el ends here
