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

(global-set-key "\C-ct" 'terminal)
(provide 'my-terminal)
;;; my-terminal ends here
