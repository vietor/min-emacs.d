;;; init.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq debug-on-error t)
(let ((minver "28.1"))
  (when (version< emacs-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(setq custom-file (locate-user-emacs-file "custom.el"))

(require 'elpa)

(require 'editor-basic)
(require 'editor-enhance)

(require 'text-editor)

(when (file-exists-p custom-file)
  (load custom-file))

(provide 'init)
;;; init.el ends here

