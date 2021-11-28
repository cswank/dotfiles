;;; my-python --- Summary
;;; setup stuff for python developing
;;; Commentary:
;;; Blah Blah Blah
;;; Code:

(add-hook 'python-mode-hook (lambda () (show-paren-mode 1)))

(defun set-newline-and-indent ()
  "Map C-j with `newline-and-indent'"
  (local-set-key (kbd "C-j") 'newline-and-indent))
(add-hook 'python-mode-hook 'set-newline-and-indent)
(add-hook 'python-mode-hook (lambda () (electric-indent-local-mode -1)))

(fset 'ipdb
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ("from ipdb import set_trace; set_trace()" 0 "%d")) arg)))
(fset 'pypath
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ("os.path.abspath(os.path.dirname(__file__))" 0 "%d")) arg)))
(fset 'stars
   "print '*' * 80\C-m")
(fset 'jq
	  "$('#')\C-b\C-b")

;;; my-python.el ends here
