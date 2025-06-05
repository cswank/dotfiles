;;; init --- Summary
;;; emacs init
;;; Commentary:

;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Load Paths
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path "~/.emacs.d/elpa")
(add-to-list 'load-path "~/.emacs.d/color")

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                     ("elpa" . "https://elpa.gnu.org/packages/")
                     ("melpa" . "https://melpa.org/packages/")))

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Load Libraries
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load-library "my-color-theme")
(load-file "~/.emacs.d/my-plugins/my-random-stuff.el")
(load-file "~/.emacs.d/my-plugins/my-beer.el")
;;(load-file "~/.emacs.d/my-plugins/my-c.el")
(load-file "~/.emacs.d/my-plugins/my-go.el")
(load-file "~/.emacs.d/my-plugins/my-go-templates.el")
(load-file "~/.emacs.d/my-plugins/my-javascript.el")
(load-file "~/.emacs.d/my-plugins/my-jira.el")
(load-file "~/.emacs.d/my-plugins/my-keybindings.el")
(load-file "~/.emacs.d/my-plugins/my-lisp.el")
(load-file "~/.emacs.d/my-plugins/my-magit.el")
(load-file "~/.emacs.d/my-plugins/my-org.el")
(load-file "~/.emacs.d/my-plugins/my-python.el")
(load-file "~/.emacs.d/my-plugins/my-sops.el")
(load-file "~/.emacs.d/my-plugins/my-sql.el")
(load-file "~/.emacs.d/my-plugins/my-tab.el")
(load-file "~/.emacs.d/my-plugins/my-terminal.el")
(load-file "~/.emacs.d/my-plugins/my-weather.el")
(load-file "~/.emacs.d/my-plugins/my-web.el")
;;(load-file "~/.emacs.d/my-plugins/my-vue.el")
(load-file "~/.emacs.d/my-plugins/my-zig.el")
(when (string= (getenv "EMACS_EMAIL") "true")
  (load-file "~/.emacs.d/my-plugins/my-email.el"))
(when (not (string= (getenv "SLACK_CLIENT_ID") "nil"))
  (load-file "~/.emacs.d/my-plugins/my-slack.el"))

(put 'set-goal-column 'disabled nil)
(provide 'init)
;;; init.el ends here

