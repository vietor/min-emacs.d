;;; init.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq debug-on-error t)
(let ((minver "28.1"))
  (when (version< emacs-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))

(setq custom-file (locate-user-emacs-file "custom.el"))
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'setup-elpa)

(require 'editor-basic)
(require 'editor-enhance)

(require 'toolkit-git)

(require 'text-editor)

(when (file-exists-p custom-file)
  (load custom-file))

(provide 'init)
;;; init.el ends here
