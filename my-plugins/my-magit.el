;;; my-magit --- Summary
;;; setup magit stuff
;;; Commentary:

;;; Code:
(global-set-key (kbd "C-x g") 'magit-status)

(defun display-buffer-full-screen (buffer alist)
  "Emacs is yelling at me for not having a doc string that doesn't include BUFFER and ALIST."
  (delete-other-windows)
  (set-window-dedicated-p nil nil)
  (set-window-buffer nil buffer)
  (get-buffer-window buffer))

(defvar magit-status-buffer-switch-function)
(setq magit-status-buffer-switch-function
      (lambda (buffer) ; there might already be an Emacs function which does this
        (pop-to-buffer buffer)
        (delete-other-windows)))

(provide 'my-magit)
;;; my-magit.el ends here

