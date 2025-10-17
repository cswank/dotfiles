;;; my-zig --- Summary
;;; setup zig development
;;; Commentary:

(use-package emacs
  :hook (zig-mode . eglot-ensure))

(add-hook 'zig-mode-hook 'company-mode)
(add-hook 'eglot-managed-mode-hook
          (lambda () (eglot-inlay-hints-mode -1)))
