;;; my-web --- Summary
;;; setup web development
;;; Commentary:

;;; Code:
(require 'web-mode)

(add-to-list 'auto-mode-alist '("\\.ghtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.gohtml\\'" . web-mode))

(setq web-mode-engines-alist
      '(("go" . "\\.ghtml")
        ("go" . "\\.gohtml")))

(provide 'my-web)
;;; my-web.el ends here
