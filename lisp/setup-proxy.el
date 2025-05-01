;;; setup-proxy.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defvar my/proxy-server "127.0.0.1:1080")

(defun my/proxy-show ()
  (interactive)
  (if url-proxy-services
      (message "HTTP proxy on '%s'" my/proxy-server)
    (message "HTTP proxy off")))

(defun my/proxy-toggle ()
  (interactive)
  (if url-proxy-services
      (setq url-proxy-services nil)
    (setq url-proxy-services
          `(("http" . ,my/proxy-server)
            ("https" . ,my/proxy-server)
            ("no_proxy" . "^\\(localhost\\|127.0.0.1\\|192.168.*\\|10.*\\)"))))
  (my/proxy-show))

(provide 'setup-proxy)
;;; setup-proxy.el ends here
