;;; language-java.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package groovy-mode
  :ensure t
  :when (executable-find "java")
  :mode "\\.gradle\\'")

(defun my/regexp-file-found (directory match-regexp &optional default-file)
  (or (ignore-errors
        (car (last (directory-files directory t match-regexp))))
      (if (not default-file)
          nil
        (expand-file-name default-file directory))))

(use-package java
  :when (executable-find "java")
  :hook ((java-mode . my/eglot-java-ensure)
         (java-ts-mode . my/eglot-java-ensure))
  :init
  (defconst my/java-ls--cache-dir
    (concat user-emacs-space-directory "java-ls.caches/"))

  (add-to-list 'recentf-exclude my/java-ls--cache-dir)

  (defun my/eglot-java-ensure ()
    (if (string-prefix-p my/java-ls--cache-dir
                         (buffer-file-name))
        (read-only-mode)
      (eglot-ensure)))

  (defclass eglot-java-ls (eglot-lsp-server) ()
    :documentation "Eclipse's Java Development Tools Language Server.")

  (cl-defmethod eglot-execute-command ((_server eglot-java-ls)
                                       (_cmd (eql java.apply.workspaceEdit))
                                       arguments)
    (mapc #'eglot--apply-workspace-edit arguments))

  (cl-defmethod eglot-initialization-options ((_server eglot-java-ls))
    `(:extendedClientCapabilities (:classFileContentsSupport t)))

  (defun my/java-ls--uri-handler(_operation &rest args)
    (let* ((uri (car args))
           (source-file
            (concat
             my/java-ls--cache-dir
             (save-match-data
               (when (string-match "jdt://contents/\\(.*\\)\\.class\\?" uri)
                 (concat (match-string 1 uri) ".java")))))
           (source-directory (file-name-directory source-file)))
      (unless (file-readable-p source-file)
        (let ((content (jsonrpc-request (eglot--current-server-or-lose)
                                        :java/classFileContents (list :uri uri))))
          (unless (file-directory-p source-directory)
            (make-directory source-directory t))
          (with-temp-file source-file
            (insert content)
            (my/eglot--text-clean-eol))))
      source-file))
  (add-to-list 'file-name-handler-alist '("\\`jdt://" . my/java-ls--uri-handler))

  (defun my/java-ls--workspace()
    (let ((project-dir (project-root (eglot--current-project))))
      (concat (car (last (split-string project-dir "/") 2))
              "@"
              (md5 project-dir))))

  (defun my/java-ls--contact (interactive)
    (let* ((install-dir
            (my/regexp-file-found user-emacs-space-directory "jdt-language-server-*" "eclipse.jdt.ls"))
           (config-dir
            (expand-file-name (cond ((string= system-type "darwin") "config_mac")
                                    ((string= system-type "windows-nt") "config_win")
                                    (t "config_linux"))
                              install-dir))
           (workspace-dir
            (expand-file-name (my/java-ls--workspace)
                              (concat user-emacs-space-directory "java-ls.workspaces")))
           (launcher-jar nil)
           (lombok-jar nil)
           (runtime-jdt-vmargs '("-Xmx1G")))

      (unless (file-directory-p install-dir)
        (eglot--error "Not found 'eclipse.jdt.ls' directory in '%s'" user-emacs-space-directory))

      (setq launcher-jar
            (my/regexp-file-found (expand-file-name "plugins" install-dir)
                                  "org\\.eclipse\\.equinox\\.launcher_.*\\.jar$"))
      (unless (and launcher-jar (file-exists-p launcher-jar))
        (eglot--error "Not found 'eclipse.jdt.ls' launcher jar"))

      (setq lombok-jar
            (my/regexp-file-found (concat user-emacs-space-directory "java.assists")
                                  "lombok-.*\\.jar$"))
      (when (and lombok-jar (file-exists-p lombok-jar))
        (push (concat "-javaagent:" lombok-jar) runtime-jdt-vmargs))

      (unless (file-directory-p workspace-dir)
        (make-directory workspace-dir t))

      (cons 'eglot-java-ls `(,(executable-find "java")
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
  (add-to-list 'eglot-server-programs '((java-mode java-ts-mode) . my/java-ls--contact)))

(use-package kotlin-mode
  :ensure t
  :when (executable-find "java")
  :hook (kotlin-mode . eglot-ensure)
  :init
  (defclass eglot-kotlin-ls (eglot-lsp-server) ()
    :documentation "Kotlin Language Server.")

  (defun my/kotlin-ls--contact (interactive)
    (let* ((install-dir
            (my/regexp-file-found user-emacs-space-directory "kotlin-language-server-*" "kotlin-language-server"))
           (execute-file
            (expand-file-name (concat "bin/kotlin-language-server"
                                      (cond ((string= system-type "windows-nt") ".bat") (t "")))
                              install-dir)))
      (unless (file-directory-p install-dir)
        (eglot--error "Not found 'kotlin-language-server' directory in '%s'" user-emacs-space-directory))

      (cons 'eglot-java-ls `(,execute-file))))
  (add-to-list 'eglot-server-programs '((kotlin-mode kotlin-ts-mode) . my/kotlin-ls--contact)))

(provide 'language-java)
;;; language-java.el ends here
