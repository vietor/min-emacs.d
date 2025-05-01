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

(unless (fboundp 'recentf-open)
  (defun recentf-open()
    (interactive)
    (find-file (completing-read "Open recent file: " recentf-list))))

;;; Platform hooks

(when (eq system-type 'windows-nt)
  (setq w32-pipe-read-delay 0
        w32-pipe-buffer-size (* 64 1024)
        w32-get-true-file-attributes nil))

(when (eq system-type 'darwin)
  (setq ns-pop-up-frames nil
        mac-command-modifier 'meta
        mac-option-modifier 'none)
  (setq-default locate-command "mdfind"))

;;; Better functions

(unless (fboundp 'mix-add-hook)
  (defun mix-add-hook (hook function &optional depth local)
    (if (not (listp hook))
        (add-hook hook function depth local)
      (dolist (elt hook)
        (add-hook elt function depth local)))))

(unless (fboundp 'mix-add-to-list)
  (defun mix-add-to-list(symbol element &optional append compare-fn)
    (if (not (listp element))
        (add-to-list symbol element append compare-fn)
      (dolist (elt element)
        (add-to-list symbol elt append compare-fn)))))

(provide 'setup-compat)
;;; setup-compat.el ends here
