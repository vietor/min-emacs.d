;;; text-others.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Markdown
(use-package markdown-mode
  :ensure t)

;; YAML
(use-package yaml-mode
  :ensure t
  :init
  (add-to-list 'my/treesit-active-langs "yaml"))

;; JSON
(use-package json-mode
  :ensure t
  :init
  (add-to-list 'my/treesit-active-langs "json")

  (defun my/json-formatter-beautify ()
    (call-interactively 'json-mode-beautify))
  (add-to-list 'my/formatter-beautify-alist '(json-mode . my/json-formatter-beautify)))

;; XML
(use-package nxml-mode
  :init
  (setq nxml-slash-auto-complete-flag t)

  (defun my/nxml-formatter-beautify (begin end)
    (interactive "r")
    (unless (use-region-p)
      (setq beg (point-min)
            end (point-max)))
    (save-excursion
      (goto-char begin)
      (while (search-forward-regexp ">[ \t]*<[^/]" end t)
        (backward-char 2) (insert "\n") (incf end))
      (goto-char begin)
      (while (search-forward-regexp "<.*?/.*?>[ \t]*<" end t)
        (backward-char) (insert "\n") (incf end))
      (goto-char begin)
      (while (search-forward-regexp "\\(<\\([a-zA-Z][-:A-Za-z0-9]*\\)\\|['\"]\\) \\(xmlns[=:]\\)" end t)
        (goto-char (match-end 0))
        (backward-char 6) (insert "\n") (incf end))
      (indent-region begin end nil)))
  (add-to-list 'my/formatter-beautify-alist '(nxml-mode . my/nxml-formatter-beautify)))

(provide 'text-others)
;;; text-others.el ends here
