;;; setup-elpa.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'package)

;; Initialize repository

(add-to-list 'package-archives '( "melpa" . "https://melpa.org/packages/") t)
(setq package-archive-priorities
      '(("nognu" . 50)
        ("melpa" . 30)
        ("gnu" . 10)))

(setq package-user-dir
      (expand-file-name (format "elpa-%s.%s" emacs-major-version emacs-minor-version)
                        user-emacs-directory))

;; Initialize package

(setq package-check-signature nil
      package-enable-at-startup nil)

(package-initialize)

;; Initailize use-package

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(use-package diminish :ensure t)

(provide 'setup-elpa)
;;; setup-elpa.el ends here
