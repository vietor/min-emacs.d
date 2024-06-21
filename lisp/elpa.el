;;; elpa.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'package)

;; Initialize repository

(dolist (archive '(("gnu"    . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                   ("nongnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
                   ("melpa"  . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
  (unless (assoc (car archive) package-archives)
    (add-to-list 'package-archives archive t)))

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

(provide 'elpa)
;;; elpa.el ends here
