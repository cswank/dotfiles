;;; my-terminal --- Summary
;;; 
;;; Commentary:

;;; Code:
(defvar my-term-shell "/bin/zsh")

(defun terminal ()
  "Switch to terminal.  Launch if nonexistent."
  (interactive)
  (if (get-buffer "*ansi-term*")
      (switch-to-buffer "*ansi-term*")
    (ansi-term "/bin/zsh"))
  (get-buffer-process "*ansi-term*"))

(global-set-key "\C-ct" 'terminal)

(defadvice term-handle-exit
  (after term-kill-buffer-on-exit activate)
(kill-buffer))

(add-hook 'term-mode-hook
          (lambda ()
            (define-key term-raw-map (kbd "C-y") 'term-paste)))

(provide 'my-terminal)
;;; my-terminal ends here
