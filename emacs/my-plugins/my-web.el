;;; my-web --- Summary
;;; setup c development
;;; Commentary:

;;; Code:


(require 'web-mode)

(add-to-list 'auto-mode-alist '("\\.ghtml\\'" . web-mode))

(setq web-mode-engines-alist
      '(("go"    . "\\.ghtml\\'"))
)

(provide 'my-web)
;;; my-web.el ends here
