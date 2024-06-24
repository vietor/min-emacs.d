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
