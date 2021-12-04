;;; my-go --- Summary
;;; setup go development
;;; Commentary:

;;; Code:
;; which one??
(require 'lsp-mode)
(require 'edit-indirect)
(add-hook 'go-mode-hook #'lsp)
(require 'lsp-ui)

(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

(setq lsp-prefer-flymake nil)
(add-hook 'go-mode-hook 'flycheck-mode)
(add-hook 'lsp-mode-hook 'lsp-ui-mode)
(add-hook 'go-mode-hook
          (lambda () (local-set-key (kbd "C-c '") #'edit-indirect-region)))

(add-hook 'go-mode-hook
          (lambda ()
            (setq-default tab-width 4)))

(setq lsp-gopls-staticcheck t
      lsp-eldoc-render-all t
      lsp-ui-doc-enable nil
      lsp-ui-peek-enable t
      lsp-ui-sideline-enable t
      lsp-ui-imenu-enable t
      lsp-ui-flycheck-enable nil
      lsp-headerline-breadcrumb-mode nil)

(defun sql-params (x)
  "Insert a sequence of $1, $2,... $x into a postgres query string."
  (interactive "nEnd: ")
  (dotimes (i (- x 1)) (insert (format "$%d, " (1+ i))))
  (insert (format "$%d" x)))

(defun go-create-playground ()
  "Create a new temporary file with a skeletal Go application."
  (interactive)
  (let ((filename (concat (file-name-as-directory (make-temp-file "go-play-" t)) "main.go")))
    (find-file filename)
    (rename-buffer (generate-new-buffer-name "Go Playground"))
    (insert (concat "package main\n\nimport (\n\t\"fmt\"\n)\n\nfunc main() {\n\tfmt.Println(\"This file is located in " filename "\")\n}"))
    (save-buffer)
    (previous-line)
    (end-of-line)
    (insert "\n\t")
    (go-mode)
    (shell-command "go mod init go-playground")))



(defun go-switch-to-playground ()
  "Switch to Go Playground buffer, creating if necessary."
  (interactive)
  (let ((playground (get-buffer "Go Playground")))
    (if playground
        (switch-to-buffer playground)
      (go-create-playground))))

(global-set-key (kbd "C-c C-g r") 'go-play-buffer)
(global-set-key (kbd "C-c C-g p") 'go-switch-to-playground)
(global-set-key (kbd "C-c C-g n") 'go-create-playground)
(global-set-key (kbd "C-c C-g q") 'sql-params)

(provide 'my-go)
;;; my-go.el ends here
