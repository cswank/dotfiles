;;; my-terraform --- Summary
;;; start terraform-ls
;;; Commentary:
;;; Code:

(require 'terraform-mode)

(add-hook 'terraform-mode-hook 'eglot-ensure)
(add-hook 'terraform-mode-hook 'flymake-mode)
(add-hook 'terraform-mode-hook 'company-mode)

(add-hook 'terraform-mode-hook #'terraform-format-on-save-mode)

(add-to-list 'eglot-server-programs
   		'(terraform-mode . ("terraform-ls" "serve")))
(provide 'my-terraform)
;;; my-weather.el ends here
