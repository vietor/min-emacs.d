;;; toolkit-treesit.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defvar my/treesit-active-langs nil)

(use-package treesit
  :when (and (fboundp 'treesit-available-p)
	     (treesit-available-p))
  :init
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
        (message "treesit: using %s in place of %s" ts-mode non-ts-mode)
        (add-to-list 'major-mode-remap-alist (cons non-ts-mode ts-mode)))))

  (defun my/auto-configure-treesit()
    (dolist (lang my/treesit-active-langs)
      (when (my/treesit-lang-p lang)
	(my/treesit-lang-remap lang)
	(let ((extra-lang (cdr (assoc-string lang my/treesit-lang-to-extra))))
	  (when extra-lang
	    (my/treesit-lang-remap extra-lang))))))
  (add-hook 'after-init-hook 'my/auto-configure-treesit))

(provide 'toolkit-treesit)
;;; toolkit-treesit.el ends here
