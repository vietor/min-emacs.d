;;; editor-plugins.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun my/comment-like-eclipse ()
  (interactive)
  (let ((start (line-beginning-position))
        (end (line-end-position)))
    (when (region-active-p)
      (setq start (save-excursion
                    (goto-char (region-beginning))
                    (beginning-of-line)
                    (point))
            end (save-excursion
                  (goto-char (region-end))
                  (end-of-line)
                  (point))))
    (comment-or-uncomment-region start end)))
(bind-key "M-;" 'my/comment-like-eclipse)

(defun my/delete-current-buffer-file ()
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))
(bind-key "M-g f d" 'my/delete-current-buffer-file)

(defun my/rename-current-buffer-file ()
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (when (file-exists-p filename)
            (rename-file filename new-name 1))
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))
(bind-key "M-g f r" 'my/rename-current-buffer-file)

(when window-system
  (defun my/browse-current-file ()
    (interactive)
    (let ((filename (buffer-file-name)))
      (if (and (fboundp 'tramp-tramp-file-p)
               (tramp-tramp-file-p filename))
          (error "Cannot open tramp file")
	(browse-url (concat "file://" (if (eq system-type 'windows-nt) "/") filename)))))
  (bind-key "M-g f b" 'my/browse-current-file)

  (defun my/open-new-emacs()
    "Open a new Emacs process."
    (interactive)
    (cond
     ((eq system-type 'darwin)
      (shell-command "open -n -a Emacs.app"))
     ((eq system-type 'windows-nt)
      (w32-shell-execute "open" (concat (file-name-directory (car command-line-args)) "runemacs.exe")))
     (t (call-process-shell-command (concat (car command-line-args) " & disown")))))
  (bind-key "M-g z" 'my/open-new-emacs))

(provide 'editor-plugins)
;;; editor-plugins.el ends here
