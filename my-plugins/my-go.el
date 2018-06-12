;;; my-go --- Summary
;;; setup go development
;;; Commentary:

;;; Code:
;; which one??
(require 'go-mode)
(defun auto-complete-for-go ()
  "Come on, I don't want to document this."
  (auto-complete-mode 1))

;;(when (memq window-system '(mac ns))
;;  (exec-path-from-shell-initialize)
;;  (exec-path-from-shell-copy-env "GOPATH"))

(defun my-go-mode-hook ()
  "Come on, I don't want to document this."
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save)
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           "go build -v && go test -v && go vet"))
  (local-set-key (kbd "M-.") 'godef-jump))
(add-hook 'go-mode-hook 'my-go-mode-hook)


(defun go-create-playground ()
  "Create a new temporary file with a skeletal Go application."
  (interactive)
  (let ((filename (make-temp-file "go-play-" nil ".go")))
    (find-file filename)
    (rename-buffer (generate-new-buffer-name "Go Playground"))
    (insert (concat "package main\n\nimport (\n\t\"fmt\"\n)\n\nfunc main() {\n\tfmt.Println(\"This file is located in " filename "\")\n}"))
    (save-buffer)
    (previous-line)
    (end-of-line)
    (insert "\n\t")
    (go-mode)))

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

(add-hook 'go-mode-hook
          (lambda ()
            (add-hook 'before-save-hook 'gofmt-before-save)
            (add-hook 'completion-at-point-functions 'go-complete-at-point)
            (setq tab-width 4)
            (setq indent-tabs-mode 1)))

(add-hook 'go-mode-hook 'flycheck-mode)

;;(add-hook 'go-mode-hook 'auto-complete-for-go)
(provide 'my-go)
;;; my-go.el ends here
