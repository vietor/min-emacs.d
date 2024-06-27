;;; framework-eglot.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package eglot
  :ensure t
  :demand t
  :bind (:map eglot-mode-map
              ("C-c r" . eglot-rename)
              ("C-c o" . eglot-code-actions)
              ("C-c C-r" . eglot-reconnect))
  :init
  ;; Fix windows-nt EOL
  (when (eq system-type 'windows-nt)
    (defun my/eglot--text-clean-eol(&rest args)
      (save-excursion
	    (goto-char (point-min))
	    (while (re-search-forward "\r+$" nil t)
	      (replace-match "" t t))))
    (advice-add #'eglot--apply-text-edits :after #'my/eglot--text-clean-eol))

  (add-to-list 'my/formatter-beautify-minor-alist
               '(eglot--managed-mode . eglot-format-buffer))

  :config
  (setq-default eglot-autoshutdown t
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
                                                    :signatureHelpProvider
                                                    :foldingRangeProvider
                                                    :documentLinkProvider
                                                    :documentHighlightProvider
                                                    :documentOnTypeFormattingProvider))

  ;; Remove control link
  (dolist (type '(eglot-note eglot-warning eglot-error))
    (cl-remf (symbol-plist type) 'flymake-overlay-control))

  (defun my/eglot--language-id (server)
    (string-remove-suffix "-ts"
                          (if (fboundp 'eglot--language-id)
                              (eglot--language-id server)
                            ;; Use first language
                            (car (eglot--language-ids server)))))

  (defun my/eglot--language-etc-path (&optional file)
    (expand-file-name (concat "etc/" (or file "")) user-emacs-directory))

  (defun my/eglot--language-etc-url (&optional file)
    (concat "file://" (if (eq system-type 'windows-nt) "/") (my/eglot--language-etc-path file)))

  (defun my/eglot--language-etc-json-read (file)
    (ignore-errors
      (with-temp-buffer
        (insert-file-contents (my/eglot--language-etc-path file))
        (goto-char (point-min))
        (when (search-forward "{{ETC-" nil t)
          (replace-string "{{ETC-PATH}}" (my/eglot--language-etc-path))
          (goto-char (point-min))
          (replace-string "{{ETC-URL}}" (my/eglot--language-etc-url)))
        (goto-char (point-min))
        (json-parse-buffer :object-type 'plist :false-object :json-false))))

  ;; Custom language workspace configuration
  (defun my/eglot--workspace-configuration (server)
    (let* ((language-id (my/eglot--language-id server))
           (settings-file (concat "lsp-" language-id "-settings.json")))
      (or (my/eglot--language-etc-json-read settings-file) ())))
  (setq-default eglot-workspace-configuration 'my/eglot--workspace-configuration))

(provide 'framework-eglot)
;;; framework-eglot.el ends here
