;;; my-terminal --- Summary  -*- lexical-binding: t; -*-
;;; 
;;; Commentary:

;;; Code:
(defvar my-term-shell "/bin/zsh")

(defun terminal ()
  "Switch to terminal.  Launch if nonexistent."
  (interactive)
  (if (get-buffer "*ansi-term*")
      (switch-to-buffer "*ansi-term*")
    (ansi-term "zsh"))
  (get-buffer-process "*ansi-term*"))

(global-set-key
 "\C-ct"
 '(lambda ()
    (interactive)
    (let ((current-dir (expand-file-name default-directory))
          (proc (terminal)))
      (term-send-string
       proc
       (format "cd '%s'\n" current-dir)))))

(defadvice term-handle-exit
  (after term-kill-buffer-on-exit activate)
(kill-buffer))

(add-hook 'term-mode-hook
          (lambda ()
            (define-key term-raw-map (kbd "C-y") 'term-paste)))

;; ghostel is installed via nix (only on some hosts), which ships the
;; prebuilt native module; never try to download/compile one.
(when (require 'ghostel nil t)
  (setq ghostel-module-auto-install nil)

  (global-set-key
   "\C-ct"
   (lambda ()
     (interactive)
     (let ((current-dir (expand-file-name default-directory)))
       (with-current-buffer (ghostel)
         (ghostel-send-string (format "cd '%s'\n" current-dir)))))))

(provide 'my-terminal)
;;; my-terminal ends here
