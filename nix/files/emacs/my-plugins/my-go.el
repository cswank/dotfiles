;;; my-go --- Summary
;;; setup go development
;;; Commentary:

;;; Code:
;; which one??
(require 'eglot)
(require 'edit-indirect)
(require 'undo-tree)

(add-hook 'go-mode-hook 'eglot-ensure)
(add-hook 'go-mode-hook 'undo-tree-mode)
(add-hook 'go-mode-hook 'flymake-mode)
(add-hook 'go-mode-hook 'company-mode)

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

(defun sql-params (x)
  "Insert a sequence of $1, $2,... $x into a postgres query string."
  (interactive "nEnd: ")
  (dotimes (i (- x 1)) (insert (format "$%d, " (1+ i))))
  (insert (format "$%d" x)))

(global-set-key (kbd "C-c C-g q") 'sql-params)

(provide 'my-go)
;;; my-go.el ends here
