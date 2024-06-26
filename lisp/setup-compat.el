;;; setup-compat.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; Supplement functions

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

;;; Platform hooks

(when (eq system-type 'windows-nt)
  (setq w32-pipe-read-delay 0
        w32-pipe-buffer-size (* 64 1024)
        w32-get-true-file-attributes nil
        inhibit-compacting-font-caches t))

(when (eq system-type 'darwin)
  (setq ns-pop-up-frames nil
        mac-command-modifier 'meta
        mac-option-modifier 'none)
  (setq-default locate-command "mdfind")
  (set-fontset-font t 'symbol
                    (font-spec :family "Apple Color Emoji") nil 'prepend))

(provide 'setup-compat)
;;; setup-compat.el ends here
