;;; init.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(let ((minver "28.1"))
  (when (version< emacs-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))

;; Initliaze `load-path'
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Disable custom-file & dir-local
(setq custom-file null-device)
(setq enable-dir-local-variables nil)

;; Define extend space directory
(defvar user-emacs-space-directory
  (expand-file-name "../.emacs.space/" user-emacs-directory))

;; Load preload configurations
(let ((preload-file (locate-user-emacs-file "preload.el")))
  (when (file-exists-p preload-file)
    (load preload-file)))

;; Load main configurations

(require 'setup-elpa)
(require 'setup-compat)
(require 'setup-proxy)

(require 'editor-basic)
(require 'editor-locales)
(require 'editor-enhance)
(require 'editor-plugins)

(require 'toolkit-git)
(require 'toolkit-grep)
(require 'toolkit-complete)
(require 'toolkit-formatter)
(require 'toolkit-treesit)

(require 'framework-prog)
(require 'framework-eglot)

(require 'language-cpp)
(require 'language-java)
(require 'language-python)
(require 'language-javascript)
(require 'language-web)
(require 'language-org)

(require 'language-progs)
(require 'language-texts)

;; Allow access from emacsclient
(add-hook 'after-init-hook
          (lambda ()
            (require 'server)
            (unless (server-running-p)
              (server-start))))

(provide 'init)
;;; init.el ends here
