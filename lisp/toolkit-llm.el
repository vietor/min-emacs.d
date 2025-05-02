;;; toolkit-llm.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package ellama
  :ensure t
  :bind ("M-g e" . ellama)
  :hook (org-ctrl-c-ctrl-c-final . ellama-chat-send-last-message)
  :init
  (require 'llm-ollama)
  (setq ellama-provider
  	    (make-llm-ollama
  	     :chat-model "deepseek-r1:7b"
  	     :embedding-model "nomic-embed-text"))
  (setq ellama-coding-provider
  	    (make-llm-ollama
  	     :chat-model "deepcoder:1.5b"
  	     :embedding-model "nomic-embed-text"))
  (setq ellama-instant-display-action-function #'display-buffer-at-bottom)
  (setq ellama-sessions-directory (concat user-emacs-space-directory "ellama-sessions")))

(provide 'toolkit-llm)
;;; toolkit-llm.el ends here
