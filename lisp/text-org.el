;;; text-org.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; RESTful client
(use-package verb
  :ensure t
  :config
  (setq-default verb-auto-kill-response-buffers t))

(use-package org
  :after verb
  :config
  (define-key org-mode-map (kbd "C-c C-r") verb-command-map))

(provide 'text-org)
;;; text-org.el ends here
