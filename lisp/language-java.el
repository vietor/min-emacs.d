;;; language-java.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package groovy-mode
  :ensure t
  :when (executable-find "java")
  :mode "\\.gradle\\'")

(use-package java
  :when (executable-find "java")
  :hook ((java-mode . my/eglot-java-ensure)
         (java-ts-mode . my/eglot-java-ensure))
  :init
  (defconst my/eclipse-jdt--cache-dir
    (concat user-emacs-space-directory "eclipse.caches/"))

  (add-to-list 'my/treesit-remap-langs "java")
  (add-to-list 'recentf-exclude my/eclipse-jdt--cache-dir)

  (defun my/eglot-java-ensure ()
    (setq fill-column 100)
    (if (string-prefix-p my/eclipse-jdt--cache-dir
                         (buffer-file-name))
        (read-only-mode)
      (eglot-ensure)))

  (defclass eglot-eclipse-jdt (eglot-lsp-server) ()
    :documentation "Eclipse's Java Development Tools Language Server.")

  (cl-defmethod eglot-execute-command ((_server eglot-eclipse-jdt)
                                       (_cmd (eql java.apply.workspaceEdit))
                                       arguments)
    (mapc #'eglot--apply-workspace-edit arguments))

  (cl-defmethod eglot-initialization-options ((_server eglot-eclipse-jdt))
    `(:extendedClientCapabilities (:classFileContentsSupport t)))

  (defun my/eclipse-jdt--uri-to-path (orig-fn uri)
    (when (keywordp uri)
      (setq uri (substring (symbol-name uri) 1)))
    (if (not (string-match "jdt://contents/\\(.*\\)\\.class\\?" uri))
        (funcall orig-fn uri)
      (let* ((source-file (concat my/eclipse-jdt--cache-dir (match-string 1 uri) ".java"))
             (source-directory (file-name-directory source-file)))
        (unless (file-readable-p source-file)
          (let ((content (jsonrpc-request (eglot--current-server-or-lose)
                                          :java/classFileContents (list :uri uri))))
            (unless (file-directory-p source-directory)
              (make-directory source-directory t))
            (with-temp-file source-file
              (insert content)
              (my/eglot--text-clean-eol))))
        source-file)))
  (advice-add 'eglot--uri-to-path :around #'my/eclipse-jdt--uri-to-path)

  (defun my/eclipse-jdt--found (directory match-regexp &optional default-file)
    (or (ignore-errors
          (car (last (directory-files directory t match-regexp))))
        (if (not default-file)
            nil
          (expand-file-name default-file directory))))

  (defun my/eclipse-jdt--workspace()
    (let ((project-dir (project-root (eglot--current-project))))
      (concat (car (last (split-string project-dir "/") 2))
              "@"
              (md5 project-dir))))

  (defun my/eclipse-jdt--contact (interactive)
    (let* ((install-dir
            (my/eclipse-jdt--found user-emacs-space-directory "jdt-language-server-*" "eclipse.jdt.ls"))
           (config-dir
            (expand-file-name (cond ((string= system-type "darwin") "config_mac")
                                    ((string= system-type "windows-nt") "config_win")
                                    (t "config_linux"))
                              install-dir))
           (workspace-dir
            (expand-file-name (my/eclipse-jdt--workspace)
                              (concat user-emacs-space-directory "eclipse.workspaces")))
           (launcher-jar nil)
           (lombok-jar nil)
           (runtime-jdt-vmargs '("-Xmx1G")))

      (unless (file-directory-p install-dir)
        (eglot--error "Not found 'eclipse.jdt.ls' directory in '%s'" user-emacs-space-directory))

      (setq launcher-jar
            (my/eclipse-jdt--found (expand-file-name "plugins" install-dir)
                                   "org\\.eclipse\\.equinox\\.launcher_.*\\.jar$"))
      (unless (and launcher-jar (file-exists-p launcher-jar))
        (eglot--error "Not found 'eclipse.jdt.ls' launcher jar"))

      (setq lombok-jar
            (my/eclipse-jdt--found (concat user-emacs-space-directory "eclipse.assists")
                                   "lombok-.*\\.jar$"))
      (when (and lombok-jar (file-exists-p lombok-jar))
        (push (concat "-javaagent:" lombok-jar) runtime-jdt-vmargs))

      (unless (file-directory-p workspace-dir)
        (make-directory workspace-dir t))

      (cons 'eglot-eclipse-jdt `(,(executable-find "java")
                                 "-Declipse.application=org.eclipse.jdt.ls.core.id1"
                                 "-Dosgi.bundles.defaultStartLevel=4"
                                 "-Declipse.product=org.eclipse.jdt.ls.core.product"
                                 "-Dosgi.checkConfiguration=true"
			                     ,(concat "-Dosgi.sharedConfiguration.area=" config-dir)
			                     "-Dosgi.sharedConfiguration.area.readOnly=true"
			                     "-Dosgi.configuration.cascaded=true"
                                 "--add-modules=ALL-SYSTEM"
                                 "--add-opens" "java.base/java.util=ALL-UNNAMED"
                                 "--add-opens" "java.base/java.lang=ALL-UNNAMED"
                                 ,@runtime-jdt-vmargs
                                 "-jar" ,launcher-jar
                                 "-data" ,workspace-dir))))
  (add-to-list 'eglot-server-programs '((java-mode java-ts-mode) . my/eclipse-jdt--contact)))

(provide 'language-java)
;;; language-java.el ends here
