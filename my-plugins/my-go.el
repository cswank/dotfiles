; Go Oracle
;(load-file "$GOPATH/src/code.google.com/p/go.tools/cmd/oracle/oracle.el")
(when (> (length (getenv "GOPATH")) 0)
  (defun my-go-mode-hook ()
    (setq gofmt-command "goimports")
    (add-hook 'before-save-hook 'gofmt-before-save)
    (if (not (string-match "go" compile-command))
        (set (make-local-variable 'compile-command)
             "go build -v && go test -v && go vet"))
    (local-set-key (kbd "M-.") 'godef-jump))
  (add-hook 'go-mode-hook 'my-go-mode-hook)

  ;;(add-to-list 'load-path (concat (getenv "GOPATH")  "/src/github.com/dougm/goflymake"))
  ;;(require 'go-flymake)

  ;;(add-to-list 'load-path (concat (getenv "GOPATH")  "/src/github.com/dougm/goflymake"))
  ;;(require 'go-flycheck)

  ;;(add-to-list 'load-path (concat (getenv "GOPATH")  "/src/github.com/golang/lint/misc/emacs"))
  ;;  (require 'golint)

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
  )

;; (eval-after-load "go-mode"
;;   '(progn
;;      (flycheck-define-checker go-gofmt
;;        "A Go syntax and style checker using the gofmt utility."
;;        :command '("gofmt" source-inplace)
;;        :error-patterns '(("^\\(?1:.*\\):\\(?2:[0-9]+\\):\\(?3:[0-9]+\\): \\(?4:.*\\)$" error))
;;        :modes 'go-mode)
;;      (add-to-list 'flycheck-checkers 'go-gofmt)))
