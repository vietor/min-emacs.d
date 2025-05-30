;;; early-init.el --- Emacs 27+ pre-initialisation config
;;; This file is loaded before the package system and GUI is initialized
;;; Commentary:
;;; Code:

;; Initialize package
(setq package-enable-at-startup nil)

;; Clean startup gui
(when (fboundp 'tooltip-mode)
  (tooltip-mode -1))
(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(when (fboundp 'horizontal-scroll-bar-mode)
  (horizontal-scroll-bar-mode -1))

;; Better process performance
(setq read-process-output-max (* 4 1024 1024)
      process-adaptive-read-buffering nil)

;; Adjust default GC threshold
(setq gc-cons-percentage 0.5)
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook
	      (lambda ()
	        (setq gc-cons-threshold (* 128 1024 1024))))

(provide 'early-init)
;;; early-init.el ends here
