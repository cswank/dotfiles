;;; my-terminal --- Summary
;;; 
;;; Commentary:

;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ansi-term
;; add this to zsh (to maybe make zsh play nicly with ansi-term
;; if [[ -n ${EMACS} ]]; then
;;     zstyle ':prezto:module:terminal' auto-title 'no'
;; fi
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar my-term-shell "/bin/zsh")

(defun terminal ()
  "Switch to terminal.  Launch if nonexistent."
  (interactive)
  (if (get-buffer "*ansi-term*")
      (switch-to-buffer "*ansi-term*")
    (ansi-term "/bin/zsh"))
  (get-buffer-process "*ansi-term*"))

(defun dired-open-term ()
  "Open an `ansi-term' that corresponds to current directory."
  (interactive)
  (let ((current-dir (dired-current-directory)))
    (term-send-string
     (terminal)
     (if (file-remote-p current-dir)
         (let ((v (tramp-dissect-file-name current-dir t)))
           (format "ssh %s@%s\n"
                   (aref v 1) (aref v 2)))
       (format "cd '%s'\n" current-dir)))))

(define-key dired-mode-map (kbd "C-x t") 'dired-open-term)
(provide 'my-terminal)
;;; my-terminal ends here
