;;; my-javascript --- Summary
;;; setup for email via mu4e
;;; Commentary:

;;; Code:
(use-package js2-mode
  :ensure t
  :defer t
  :mode "\\.js\\'")

(use-package json-mode
  :ensure t
  :defer t
  :mode "\\.json\\'")

(add-hook 'json-mode-hook
          (lambda ()
            (add-hook 'before-save-hook 'editorconfig-format-buffer nil t)))
;;; my-javascript.el ends here

