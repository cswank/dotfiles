;;; my-c --- Summary
;;; setup c development
;;; Commentary:

(use-package emacs
  :hook (zig-mode . eglot-ensure))

(add-hook 'zig-mode-hook 'company-mode)
