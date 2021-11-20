;;; my-email --- Summary
;;; setup for email via mu4e
;;; Commentary:

;;(load-file "~/.config/mu4e/mu4e-config.el")

(add-to-list 'load-path "$HOME/.nix-profile/share/emacs/site-lisp/mu4e")

(require 'mu4e)

(setq mu4e-sent-messages-behavior 'delete
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-stream-type 'starttls
      mu4e-get-mail-command "offlineimap"
      smtpmail-smtp-service 587)

(defvar my-mu4e-account-alist
  '(
    ("Gmail"
     (mu4e-sent-folder "/[Gmail].Sent Mail")
     (mu4e-drafts-folder "/[Gmail].Drafts")
     (mu4e-trash-folder "/[Gmail].Trash")
     (user-mail-address "craigswank@gmail.com")
     (smtpmail-smtp-user "craigswank@gmail.com")
     (smtpmail-stream-type starttls))
    ;; ("Parsyl"
    ;;  (mu4e-sent-folder "/Parsyl/Saved Items")
    ;;  (mu4e-drafts-folder "/Parsyl/Drafts")
    ;;  (user-mail-address "cswank@parsyl.com")
    ;;  (smtpmail-smtp-user "cswank@parsyl.com")
    ;;  (smtpmail-stream-type starttls))
    ))

(provide 'my-email)
;;; my-email.el ends here

