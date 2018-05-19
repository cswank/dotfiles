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

(defvar magit-display-buffer-function)
(setq magit-display-buffer-function
      (lambda (buffer)
        (if magit-display-buffer-noselect
            (magit-display-buffer-traditional buffer)
          (display-buffer buffer '(display-buffer-full-screen)))))

(provide 'my-magit)
;;; my-magit.el ends here
