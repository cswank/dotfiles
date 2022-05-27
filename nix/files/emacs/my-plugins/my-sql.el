;;; my-sql --- Summary
;;; setup sql-mode stuff
;;; Commentary:

(eval-after-load 'sql-mode
  '(load-library "sql-indent"))

;; (defun org-babel-edit-prep:sql (babel-info)
;;   (setq-local buffer-file-name (->> babel-info caddr (alist-get :tangle)))
;;   (setq-local lsp-buffer-uri (->> babel-info caddr (alist-get :tangle) lsp--path-to-uri))
;;   (lsp))

;; (add-hook 'sql-mode-hook 'lsp)
;; (setq lsp-sqls-workspace-config-path nil)
;; (setq lsp-sqls-connections
;;     '(((driver . "postgresql") (dataSourceName . "host=parsyl.cc7inmz0czn4.us-east-1.rds.amazonaws.com port=5432 user=parsyl dbname=parsyl"))))

(defun sql-indent-string ()
  "Indents the string under the cursor as SQL."
  (interactive)
  (save-excursion
    (er/mark-inside-quotes)
    (let* ((text (buffer-substring-no-properties (region-beginning) (region-end)))
           (pos (region-beginning))
           (column (progn (goto-char pos) (current-column)))
           (formatted-text (with-temp-buffer
                             (insert text)
                             (delete-trailing-whitespace)
                             (sql-indent-buffer)
                             (replace-string "\n" (concat "\n" (make-string column (string-to-char " "))) nil (point-min) (point-max))
                             (buffer-string))))
      (delete-region (region-beginning) (region-end))
      (goto-char pos)
      (insert formatted-text))))
