;;; language-texts.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Markdown
(use-package markdown-mode
  :ensure t)

;; YAML
(use-package yaml-mode
  :ensure t
  :init
  (add-to-list 'my/treesit-remap-langs "yaml")
  (add-to-list 'my/formatter-beautify-prettier-modes 'yaml-mode))

;; XML
(use-package nxml-mode
  :init
  (setq nxml-slash-auto-complete-flag t)

  (defun my/nxml-formatter-beautify ()
    (interactive)
    (let ((begin (point-min))
          (end (point-max)))
      (save-excursion
        (goto-char begin)
        (while (search-forward-regexp ">[ \t]*<[^/]" end t)
          (backward-char 2) (insert "\n") (cl-incf end))
        (goto-char begin)
        (while (search-forward-regexp "<.*?/.*?>[ \t]*<" end t)
          (backward-char) (insert "\n") (cl-incf end))
        (goto-char begin)
        (while (search-forward-regexp "\\(<\\([a-zA-Z][-:A-Za-z0-9]*\\)\\|['\"]\\) \\(xmlns[=:]\\)" end t)
          (goto-char (match-end 0))
          (backward-char 6) (insert "\n") (cl-incf end))
        (indent-region begin end nil))))
  (add-to-list 'my/formatter-beautify-alist '(nxml-mode . my/nxml-formatter-beautify)))

;; Treesit
(when (my/treesit-available-p)
  (use-package dockerfile-ts-mode))

(provide 'language-texts)
;;; language-texts.el ends here
