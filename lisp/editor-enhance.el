;;; editor-enhance.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Load better theme

(unless window-system
  (load-theme 'tango-dark))

(use-package doom-themes
  :ensure t
  :when window-system
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-dracula t))

;; Diminish `abbrev'
(use-package abbrev
  :diminish)

;; Use and diminish `autorevert'
(use-package autorevert
  :diminish auto-revert-mode
  :hook (after-init . global-auto-revert-mode)
  :init
  (setq global-auto-revert-non-file-buffers t
        auto-revert-verbose nil))

;; Use `uniquify' for `buffer-name'
(use-package uniquify
  :init
  (setq uniquify-buffer-name-style 'reverse
        uniquify-separator " • "
        uniquify-after-kill-buffer-p t
        uniquify-ignore-buffers-re "^\\*"))

;; Use better `list-buffers'
(use-package ibuffer
  :bind ([remap list-buffers] . ibuffer)
  :config
  (setq ibuffer-show-empty-filter-groups nil
        ibuffer-filter-group-name-face 'font-lock-doc-face
        ibuffer-saved-filter-groups '(("beautify"
                                       ("Org" (mode . org-mode))
                                       ("Magit" (name . "^magit"))
                                       ("Temporary" (name . "^\\*")))))
  (add-hook 'ibuffer-mode-hook
            (lambda ()
              (ibuffer-switch-to-saved-filter-groups "beautify"))))

;; Use better `switch-window'
(use-package switch-window
  :ensure t
  :bind ("C-x o" . switch-window)
  :init
  (setq-default switch-window-shortcut-style 'qwerty)
  (setq-default switch-window-timeout nil))

;; Use `bind-key' help
(use-package which-key
  :ensure t
  :diminish
  :hook (after-init . which-key-mode)
  :config
  (setq-default which-key-idle-delay 1.5))

;; Move text lines
(use-package move-dup
  :ensure t
  :diminish
  :hook (after-init . global-move-dup-mode))

;; Multiple cursors editor
(use-package multiple-cursors
  :ensure t
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-M->" . mc/skip-to-next-like-this)
         ("C-M-<" .  mc/skip-to-previous-like-this)
         ("M-<down-mouse-1>" . nil)
         ("M-<mouse-1>" . mc/add-cursor-on-click))
  :init
  (defun mc/save-lists () "Ignore save history."))

(provide 'editor-enhance)
;;; editor-enhance.el ends here
