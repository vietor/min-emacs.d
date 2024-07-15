;;; toolkit-treesit.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defvar my/treesit-remap-langs nil)

(defun my/treesit-available-p()
  (and (fboundp 'treesit-available-p)
	   (treesit-available-p)))

(use-package treesit
  :when (my/treesit-available-p)
  :init
  (setq treesit-font-lock-level 4)
  (add-to-list 'treesit-extra-load-path
	           (concat user-emacs-space-directory "tree-sitter"))

  (defconst my/treesit-lang-to-grammar '(("c++" . "cpp")
					                     ("js" . "javascript")))

  (defconst my/treesit-lang-to-extra '(("c++" . "c-or-c++")))

  (defun my/treesit-lang-p (lang)
    (treesit-language-available-p
     (intern (or (cdr (assoc-string lang my/treesit-lang-to-grammar))
		         lang))))

  (defun my/treesit-lang-remap (lang)
    (let ((ts-mode (intern (concat lang "-ts-mode")))
          (non-ts-mode (intern (concat lang "-mode"))))
      (when (and (fboundp ts-mode)
		         (fboundp non-ts-mode))
        (add-to-list 'major-mode-remap-alist (cons non-ts-mode ts-mode)))))

  (defun my/treesit-auto-configure()
    (dolist (lang my/treesit-remap-langs)
      (when (my/treesit-lang-p lang)
	    (my/treesit-lang-remap lang)
	    (let ((extra-lang (cdr (assoc-string lang my/treesit-lang-to-extra))))
	      (when extra-lang
	        (my/treesit-lang-remap extra-lang))))))
  (add-hook 'after-init-hook 'my/treesit-auto-configure))

(provide 'toolkit-treesit)
;;; toolkit-treesit.el ends here
