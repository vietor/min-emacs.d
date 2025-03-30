;;; framework-eglot.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defvar my/eglot-language-alias-key nil)
(defvar my/eglot-language-auto-modes nil)
(defvar my/eglot-language-ignore-modes nil)

(use-package eglot
  :ensure t
  :demand t
  :bind (:map eglot-mode-map
              ("C-c r" . eglot-rename)
              ("C-c o" . eglot-code-actions)
              ("C-c C-r" . eglot-reconnect))
  :init
  (add-to-list 'my/formatter-beautify-minor-alist
               '(eglot--managed-mode . eglot-format-buffer))

  ;; Fix windows-nt EOL
  (defun my/eglot--text-clean-eol(&rest args)
    (when (eq system-type 'windows-nt)
      (save-excursion
	    (goto-char (point-min))
	    (while (re-search-forward "\r+$" nil t)
	      (replace-match "" t t)))))
  (advice-add #'eglot--apply-text-edits :after #'my/eglot--text-clean-eol)

  ;; Advice for ignore *-mode
  (defun my/eglot-current-server (orig-fn)
    (if (derived-mode-p my/eglot-language-ignore-modes)
        (setq eglot--cached-server nil)
      (funcall orig-fn)))
  (advice-add #'eglot-current-server :around #'my/eglot-current-server)

  :config
  (setq-default eglot-menu-string "♿"
                eglot-autoshutdown t
                eglot-events-buffer-size 0
                eglot-confirm-server-initiated-edits nil
                eglot-ignored-server-capabilities '(:hoverProvider
                                                    :codeLensProvider
                                                    :inlayHintProvider
                                                    :documentHighlightProvider
                                                    :documentOnTypeFormattingProvider))

  ;; Remove control link
  (dolist (type '(eglot-note eglot-warning eglot-error))
    (cl-remf (symbol-plist type) 'flymake-overlay-control))

  (defun my/eglot--language-key (server)
    (let* ((language-id (if (fboundp 'eglot--language-id)
                            (eglot--language-id server)
                          (car (eglot--language-ids server))))
           (language-key (replace-regexp-in-string "\\(-ts\\)$" "" language-id t t)))
      (or (cdr (assoc-string language-key my/eglot-language-alias-key)) language-key)))

  (defun my/eglot--language-etc-json-read (file)
    (ignore-errors
      (let ((file-path (expand-file-name (concat "etc/" file) user-emacs-directory))
            (base-url (concat "file://"
                              (if (eq system-type 'windows-nt) "/")
                              (expand-file-name "etc" user-emacs-directory))))
        (with-temp-buffer
          (insert-file-contents file-path)
          (goto-char (point-min))
          (while (search-forward "{{BASE-URL}}" nil t)
            (replace-match base-url t t))
          (goto-char (point-min))
          (json-parse-buffer :object-type 'plist :false-object :json-false)))))

  ;; Custom language workspace configuration
  (defun my/eglot--workspace-configuration (server)
    (or (my/eglot--language-etc-json-read
         (concat "lsp-" (my/eglot--language-key server) "-workspace.json"))
        ()))
  (setq-default eglot-workspace-configuration 'my/eglot--workspace-configuration)

  ;; Auto start session for language group
  (defun my/eglot--auto-configure()
    (cl-loop
     for (mode-or-modes . contact) in eglot-server-programs
     for group-modes = (cl-mapcar (lambda (x)
                                    (if (not (consp x)) x (car x)))
                                  (if (listp mode-or-modes) mode-or-modes (list mode-or-modes)))
     when (cl-intersection group-modes my/eglot-language-auto-modes)
     do (dolist (mode group-modes)
          (add-hook (intern (concat (symbol-name mode) "-hook")) 'eglot-ensure))))
  (add-hook 'after-init-hook 'my/eglot--auto-configure))

(provide 'framework-eglot)
;;; framework-eglot.el ends here
