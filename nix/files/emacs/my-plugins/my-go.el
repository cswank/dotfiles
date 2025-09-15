;;; my-go --- Summary
;;; setup go development
;;; Commentary:

;;; Code:
;; which one??
(require 'lsp-mode)
(require 'edit-indirect)
(require 'undo-tree)
(add-hook 'go-mode-hook #'lsp)
(require 'lsp-ui)

(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

(add-to-list 'undo-tree-incompatible-major-modes #'magit-status-mode)

(setq lsp-prefer-flymake nil)
(add-hook 'go-mode-hook 'undo-tree-mode)
(add-hook 'go-mode-hook 'flycheck-mode)
(add-hook 'lsp-mode-hook 'lsp-ui-mode)

(add-hook 'go-mode-hook
          (lambda () (local-set-key (kbd "C-c '") #'edit-indirect-region)))

(add-hook 'go-mode-hook
          (lambda ()
            (setq-default tab-width 4)))


(defun go-sql-buffer()
   (goto-char 0))

(add-hook 'edit-indirect-after-creation-hook #'go-sql-buffer)

(defun edit-indirect-custom-guess-major-mode (_parent-buffer _beg _end)
  "Assume indirect edits from a go buffer will always be sql or json"
  (setq first-char (string (char-after 1)))
  (defvar selected-mode)
  (with-current-buffer _parent-buffer
    (if (eq major-mode 'go-mode)
        (if (or (string= first-char "{") (string= first-char "["))
            (setq selected-mode 'json-mode)
          (setq selected-mode 'sql-mode))
      (setq selected-mode `normal-mode)))
  (funcall selected-mode))

(setq edit-indirect-guess-mode-function #'edit-indirect-custom-guess-major-mode)

(setq lsp-gopls-staticcheck t
      lsp-eldoc-render-all t
      lsp-ui-doc-enable nil
      lsp-ui-peek-enable t
      lsp-ui-sideline-enable t
      lsp-ui-imenu-enable t
      lsp-ui-flycheck-enable nil
      lsp-headerline-breadcrumb-enable nil)

(defun sql-params (x)
  "Insert a sequence of $1, $2,... $x into a postgres query string."
  (interactive "nEnd: ")
  (dotimes (i (- x 1)) (insert (format "$%d, " (1+ i))))
  (insert (format "$%d" x)))

(global-set-key (kbd "C-c C-g r") 'go-play-buffer)
;; (global-set-key (kbd "C-c C-g p") 'go-switch-to-playground)
;; (global-set-key (kbd "C-c C-g n") 'go-create-playground)

(global-set-key (kbd "C-c C-g q") 'sql-params)

;; for handling build tags in work project
(defun insurance-api-configuration ()
  "Configure LSP Go environment for insurance-api projects."
  (when-let* ((project-root (project-root (project-current)))
              (repo-name (file-name-nondirectory
                         (directory-file-name project-root))))
    (when (string-match-p "insurance-api" repo-name)
      (setq lsp-go-env '((GOFLAGS . "-tags=e2e")))
      (message "Set GOFLAGS=-tags=e2e for %s" repo-name))))

(add-hook 'lsp-after-initialize-hook 'insurance-api-configuration)

(provide 'my-go)
;;; my-go.el ends here
