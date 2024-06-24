;;; framework-format.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defvar my/formatter-beautify-alist nil)
(defvar my/formatter-beautify-minor-alist nil)

(defun my/formatter-indent ()
  "Reformat current buffer by indent."
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max)))

(defun my/formatter-beautify ()
  "Reformat current buffer by customize."
  (interactive)
  (let ((beautify (cdr (assoc major-mode my/formatter-beautify-alist))))
    (unless beautify
      (setq beautify (cdr (assoc nil my/formatter-beautify-minor-alist
                                 (lambda (mode _)
                                   (and (car (member mode minor-mode-list))
                                        (symbol-value mode)))))))
    (when (not beautify)
      (error "No formatter for this buffer"))

    (let ((c-point (point))
          (w-start (window-start)))
      (if (not (commandp beautify t))
          (funcall beautify)
        (call-interactively beautify))
      (goto-char c-point)
      (goto-char (line-beginning-position))
      (set-window-start (selected-window) w-start))))

(bind-key "M-g b i" 'my/formatter-indent)
(bind-key "M-g b f" 'my/formatter-beautify)

(provide 'framework-format)
;;; framework-format.el ends here
