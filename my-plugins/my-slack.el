;;; my-slack --- Summary
;;; setup slack client
;;; Commentary:

;;; Code:

(setq slack-id (getenv "SLACK_CLIENT_ID"))
(setq slack-secret (getenv "SLACK_CLIENT_SECRET"))
(setq slack-token (getenv "SLACK_TOKEN"))
(require 'slack)
(require 'alert)

(use-package slack
  :commands (slack-start)
  :init
  (setq slack-buffer-emojify t) ;; if you want to enable emoji, default nil
  (setq slack-prefer-current-team t)
  :config
  (slack-register-team
   :name "Sendgrid"
   :default t
   :client-id slack-id
   :client-secret slack-secret
   :token slack-token
   :subscribed-channels '(mcbe-private)
   :full-and-display-names t)

(use-package alert
  :commands (alert)
  :init
  (setq alert-default-style 'notifier))
)

;; (setq slack-completing-read-function

;; (setq slack-buffer-function #'switch-to-buffer)
;; (setq slack-prefer-current-team t)
(setq slack-display-team-name nil)

;; ;;; Go to any channel with `C-x j'.
(define-key ctl-x-map "j" #'slack-select-rooms)
;; ;;; Go to the next buffer that has an unread message.
(define-key ctl-x-map "n" #'tracking-next-buffer)

(define-key slack-mode-map "@"
  (defun endless/slack-message-embed-mention ()
    (interactive)
    (call-interactively #'slack-message-embed-mention)
    (insert " ")))


(define-key slack-mode-map (kbd "C-c C-d")
  #'slack-message-delete)

(define-key slack-mode-map (kbd "C-c C-k")
  #'slack-channel-leave)

(define-key slack-mode-map (kbd "C-c C-e")
  #'slack-message-edit)

(provide 'my-slack)
;;; my-slack.el ends here
