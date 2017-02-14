; Go Oracle

(defun auto-complete-for-go ()
    (auto-complete-mode 1))


(defun my-go-mode-hook ()
  ; Use goimports instead of go-fmt
  (setq gofmt-command "goimports")
  ; Call Gofmt before saving
  (add-hook 'before-save-hook 'gofmt-before-save)
  ; Customize compile command to run go build
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           "go build -v && go test -v && go vet"))
  ; Go oracle
  ;(load-file "$GOPATH/src/golang.org/x/tools/cmd/oracle/oracle.el")
  ; Godef jump key binding
  (local-set-key (kbd "M-.") 'godef-jump))
(add-hook 'go-mode-hook 'my-go-mode-hook)


(defun go-create-playground ()
  "Creates a new temporary file with a skeletal Go application"
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
  "Switch to Go Playground buffer, creating if necessary"
  (interactive)
  (let ((playground (get-buffer "Go Playground")))
    (if playground
        (switch-to-buffer playground)
      (go-create-playground))))

(global-set-key (kbd "C-c C-g p") 'go-switch-to-playground)
(global-set-key (kbd "C-c C-g n") 'go-create-playground)

(add-hook 'go-mode-hook
          (lambda ()
            (add-hook 'before-save-hook 'gofmt-before-save)
            (setq tab-width 4)
            (setq indent-tabs-mode 1)))
(add-hook 'go-mode-hook 'flycheck-mode)
(add-hook 'go-mode-hook 'auto-complete-for-go)
