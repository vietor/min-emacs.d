;;; setup-compat.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(unless (fboundp 'rename-visited-file)
  (defun rename-visited-file (new-name)
    (interactive "sNew name: ")
    (let ((name (buffer-name))
          (filename (buffer-file-name)))
      (unless filename
        (error "Buffer '%s' is not visiting a file!" name))
      (progn
        (when (file-exists-p filename)
          (rename-file filename new-name 1))
        (set-visited-file-name new-name)
        (rename-buffer new-name)))))

(provide 'setup-compat)
;;; setup-compat.el ends here
