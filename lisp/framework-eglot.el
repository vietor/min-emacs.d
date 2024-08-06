;;; framework-eglot.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package flymake
  :bind (:map flymake-mode-map
              ("M-n" . flymake-goto-next-error)
              ("M-p" . flymake-goto-prev-error))
  :config
  (setq-default flymake-mode-line-lighter "🪰"))

(defvar my/eglot-language-alias-key nil)

(use-package eglot
  :ensure t
  :demand t
  :bind (:map eglot-mode-map
              ("C-c r" . eglot-rename)
              ("C-c o" . eglot-code-actions)
              ("C-c C-r" . eglot-reconnect))
  :init
  ;; Fix windows-nt EOL
  (defun my/eglot--text-clean-eol(&rest args)
    (when (eq system-type 'windows-nt)
      (save-excursion
	    (goto-char (point-min))
	    (while (re-search-forward "\r+$" nil t)
	      (replace-match "" t t)))))
  (advice-add #'eglot--apply-text-edits :after #'my/eglot--text-clean-eol)

  (add-to-list 'my/formatter-beautify-minor-alist
               '(eglot--managed-mode . eglot-format-buffer))

  :config
  (setq-default eglot-menu-string "♿"
                eglot-autoshutdown t
                eglot-sync-connect 1
                eglot-connect-timeout 60
                eglot-events-buffer-size 0
                eglot-autoreconnect 1
                eglot-send-changes-idle-time 0.75
                eglot-confirm-server-initiated-edits nil
                eglot-ignored-server-capabilities '(:hoverProvider
                                                    :colorProvider
                                                    :codeLensProvider
                                                    :inlayHintProvider
                                                    :foldingRangeProvider
                                                    :documentLinkProvider
                                                    :documentHighlightProvider
                                                    :documentOnTypeFormattingProvider))

  ;; Remove control link
  (dolist (type '(eglot-note eglot-warning eglot-error))
    (cl-remf (symbol-plist type) 'flymake-overlay-control))

  (defun my/eglot--language-key (server)
    (let* ((language-id (if (fboundp 'eglot--language-id)
                            (eglot--language-id server)
                          (car (eglot--language-ids server))))
           (language-key (replace-regexp-in-string "\\(-ts\\|react\\)$" "" language-id t t)))
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
         (concat "lsp-" (my/eglot--language-key server) "-settings.json"))
        ()))
  (setq-default eglot-workspace-configuration 'my/eglot--workspace-configuration))

(provide 'framework-eglot)
;;; framework-eglot.el ends here
