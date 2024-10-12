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

(global-set-key (kbd "C-x C-m") 'execute-extended-command)
(global-set-key (kbd "C-c c") 'comment-region)
(global-set-key (kbd "C-c u") 'uncomment-region)
(global-set-key (kbd "C-c i") 'indent-region)
(global-set-key (kbd "C-c l") 'global-display-line-numbers-mode)
(global-set-key (kbd "C-c r") 'query-replace)
(global-set-key (kbd "C-c R") 'query-replace-regexp)
(global-set-key (kbd "C-c w") 'browse-kill-ring)
(global-set-key (kbd "C-c q") 'load-sql)
(global-set-key (kbd "C-c =") 'expand-region-twice)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)
(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
(define-key global-map (kbd "C-c b") 'ace-jump-buffer)
(global-set-key (kbd "C-c y") 'browse-kill-ring)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;multiple-cursors mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C-c C-n") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c C-p") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-a") 'mc/mark-all-like-this)

(provide 'my-keybindings)
;;; my-keybindings.el ends here
