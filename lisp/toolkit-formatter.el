;;; toolkit-formatter.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defvar my/formatter-beautify-alist nil)
(defvar my/formatter-beautify-minor-alist nil)
(defvar my/formatter-beautify-prettier-modes nil)

(when (executable-find "prettier")
  (defun prettier-beautify ()
    (interactive)
    (let* ((ext (file-name-extension buffer-file-name t))
           (bufferfile (make-temp-file "prettier" nil ext))
           (outputfile (make-temp-file "prettier" nil ext))
           (coding-system-for-read 'utf-8)
           (coding-system-for-write 'utf-8))
      (unwind-protect
          (save-restriction
            (widen)
            (write-region nil nil bufferfile)
            (when (zerop (apply 'call-process
                                "prettier" bufferfile (list (list :file outputfile) nil)
                                nil (list "--stdin-filepath" buffer-file-name)))
              (insert-file-contents outputfile nil nil nil t)
              (message "Applied prettier successed")))
        (delete-file bufferfile)
        (delete-file outputfile)))))

(defun my/formatter-indent ()
  "Reformat current buffer by indent."
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max)))

(defun my/formatter-beautify ()
  "Reformat current buffer by customize."
  (interactive)
  (let ((beautify nil))
    (when (and (fboundp 'prettier-beautify)
               (member major-mode my/formatter-beautify-prettier-modes))
      (setq beautify 'prettier-beautify))
    (unless beautify
      (setq beautify (cdr (assoc major-mode my/formatter-beautify-alist))))
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

(bind-key "<f12>" 'my/formatter-indent)
(bind-key "M-<f12>" 'my/formatter-beautify)

(provide 'toolkit-formatter)
;;; toolkit-formatter.el ends here
