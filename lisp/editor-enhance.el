;;; editor-enhance.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Load theme

(if (not window-system)
    (load-theme 'tango-dark)
  (use-package monokai-theme
    :ensure t
    :config
    (load-theme 'monokai t)))

;; Diminish `abbrev'
(use-package abbrev
  :diminish)

;; Active `autorevert'
(use-package autorevert
  :diminish auto-revert-mode
  :hook (after-init . global-auto-revert-mode)
  :init
  (setq global-auto-revert-non-file-buffers t
        auto-revert-verbose nil))

;; Better `buffer-name'
(use-package uniquify
  :init
  (setq uniquify-buffer-name-style 'reverse
        uniquify-separator " • "
        uniquify-after-kill-buffer-p t
        uniquify-ignore-buffers-re "^\\*"))

;; Better dired behavior
(use-package dired
  :init
  (setq-default dired-dwim-target t
                dired-kill-when-opening-new-dired-buffer t))

;; Better `list-buffers'

(use-package ibuffer
  :bind ([remap list-buffers] . ibuffer)
  :config
  (setq ibuffer-show-empty-filter-groups nil
        ibuffer-filter-group-name-face 'font-lock-doc-face))

(use-package ibuffer-vc
  :ensure t
  :config
  (defun my/ibuffer-set-up-preferred-filters ()
    (ibuffer-vc-set-filter-groups-by-vc-root)
    (unless (eq ibuffer-sorting-mode 'filename/process)
      (ibuffer-do-sort-by-filename/process)))
  (add-hook 'ibuffer-hook 'my/ibuffer-set-up-preferred-filters))

;; Better `switch-window'
(use-package switch-window
  :ensure t
  :bind ("C-x o" . switch-window)
  :init
  (setq-default switch-window-shortcut-style 'qwerty)
  (setq-default switch-window-timeout nil))

;; Active `bind-key' help
(use-package which-key
  :ensure t
  :diminish
  :hook (after-init . which-key-mode)
  :config
  (setq-default which-key-idle-delay 1.5))

;; Move lines
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
