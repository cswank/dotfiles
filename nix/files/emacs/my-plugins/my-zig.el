;;; my-zig --- Summary
;;; setup zig development
;;; Commentary:

(use-package zig-mode
  :hook
  ('zig-mode . #'lsp-deferred))

(add-hook 'zig-mode-hook 'company-mode)
(add-hook 'eglot-managed-mode-hook
          (lambda () (eglot-inlay-hints-mode -1)))
