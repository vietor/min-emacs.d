;;; editor-plugins.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Use better comment behavior
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

;; Switch to last buffer

(defun my/candidate-buffer-p(buffer)
  (let ((name (buffer-name buffer)))
    (and (not (get-buffer-window buffer))
         (or (not (or (string-prefix-p " " name)
                      (string-prefix-p "*" name)))
             (seq-contains '("*scratch*" "*shell*") name)))))

(defun my/switch-to-candidate-buffer ()
  (interactive)
  (let ((buffer nil))
    (setq buffer (seq-find 'my/candidate-buffer-p (cdr (buffer-list))))
    (when buffer
      (switch-to-buffer buffer))))
(bind-key "<f7>" 'my/switch-to-candidate-buffer)

;;; Use better buffer behavior

(defun my/switch-to-scratch-buffer ()
  (interactive)
  (let ((exists (get-buffer "*scratch*"))
        (buffer (get-buffer-create "*scratch*")))
    (unless exists
      (set-buffer-major-mode buffer))
    (switch-to-buffer buffer)))
(bind-key "<f8>" 'my/switch-to-scratch-buffer)

(defun my/switch-to-empty-scratch-buffer ()
  (interactive)
  (my/switch-to-scratch-buffer)
  (buffer-disable-undo)
  (erase-buffer)
  (buffer-enable-undo))
(bind-key "C-<f8>" 'my/switch-to-empty-scratch-buffer)

(defun my/switch-to-shell-buffer ()
  (interactive)
  (let ((buffer (get-buffer "*shell*")))
    (unless (buffer-live-p buffer)
      (shell)
      (setq buffer (get-buffer "*shell*")))
    (switch-to-buffer buffer)))
(bind-key "M-<f8>" 'my/switch-to-shell-buffer)

(defun my/kill-current-buffer ()
  (interactive)
  (kill-buffer (current-buffer))
  (ignore-errors
    (delete-window)))
(bind-key "C-x C-k" 'my/kill-current-buffer)

(defun my/kill-buffers-exclude-current ()
  (interactive)
  (delete-other-windows)
  (mapc 'kill-buffer (cdr (buffer-list (current-buffer)))))
(bind-key "M-g b 1" 'my/kill-buffers-exclude-current)

(defun my/kill-buffers-switch-scratch ()
  (interactive)
  (delete-other-windows)
  (switch-to-empty-scratch-buffer)
  (mapc 'kill-buffer (cdr (buffer-list (current-buffer)))))
(bind-key "M-g b 0" 'my/kill-buffers-switch-scratch)

;; Use better file behavior

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
  (bind-key "M-g f b" 'my/browse-current-file))

;; Open new emacs instance
(when window-system
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
