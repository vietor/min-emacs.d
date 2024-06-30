;;; editor-mouse.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Disable mouse

(let ((mouse-numbers '(2 3 4 5 6 7)))
  (dolist (m-n mouse-numbers)
    (unbind-key (format "<mouse-%d>" m-n))))

(let ((prefix-keys '("C" "M" "S" "C-M" "C-S" "M-S" "M-C-S"))
      (wheel-events '("wheel-up" "wheel-down" "wheel-left" "wheel-right"))
      (mouse-events '("mouse" "up-mouse" "down-mouse" "drag-mouse"))
      (mouse-numbers '(1 2 3 4 5 6 7)))
  (dolist (p-k prefix-keys)
    (dolist (w-e wheel-events)
      (unbind-key (format "%s-<%s>" p-k w-e)))
    (dolist (m-e mouse-events)
      (dolist (m-n mouse-numbers)
        (unbind-key (format "%s-<%s-%d>" p-k m-e m-n))))))

;; Disable strong mouse

(setq-default mouse-wheel-scroll-amount nil)

(defun my/disable-strong-mouse()
  (unbind-key "C-<down-mouse-2>"))
(add-hook 'after-init-hook 'my/disable-strong-mouse)

(provide 'editor-mouse)
;;; editor-mouse.el ends here
