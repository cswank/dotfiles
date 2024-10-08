;;; my-keybindings --- Summary
;;; setup keybindings
;;; Commentary:
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;My Keyboard Bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun expand-region-twice ()
  "Run `expand-region'"
  (interactive)
  (er/expand-region 2))

(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-cc" 'comment-region)
(global-set-key "\C-cu" 'uncomment-region)
(global-set-key "\C-ci" 'indent-region)
(global-set-key "\C-cl" 'global-display-line-numbers-mode)
(global-set-key "\C-cr" 'query-replace)
(global-set-key "\C-cR" 'query-replace-regexp)
(global-set-key "\C-cw" 'browse-kill-ring)
(global-set-key "\C-cq" 'load-sql)
(global-set-key "\C-c=" 'expand-region-twice)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;multiple-cursors mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C-c C-n") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c C-p") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-a") 'mc/mark-all-like-this)

(provide 'my-keybindings)
;;; my-keybindings.el ends here
