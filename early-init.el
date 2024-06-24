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

(provide 'early-init)
;;; early-init.el ends here
