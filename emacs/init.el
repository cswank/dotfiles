;;; init --- Summary
;;; emacs init
;;; Commentary:

;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Load Paths
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(package-initialize)
(add-to-list 'load-path "~/.emacs.d/elpa")
(add-to-list 'load-path "~/.emacs.d/color")
;;(add-to-list 'load-path "~/.emacs.d/my-plugins")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;MELPA packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   '("melpa" . "http://melpa.org/packages/")
   t)
    (package-initialize))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Custom Set Variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ag-ignore-list (quote ("*~")))
 '(alert-default-style (quote fringe))
 '(coffee-tab-width 2)
 '(column-number-mode t)
 '(global-linum-mode nil)
 '(go-add-tags-style (quote snake-case))
 '(max-specpdl-size 10460)
 '(org-export-backends (quote (ascii html icalendar latex md)))
 '(osx-clipboard-mode t)
 '(package-selected-packages
   (quote
    (gnuplot thrift scala-mode chronos avy skewer-mode erc-terminal-notifier slack projectile go-guru ivy go-direx paradox json-mode js2-mode ace-jump-buffer ace-jump-mode go-errcheck markdown-mode dockerfile-mode undo-tree forecast go-mode use-package poporg protobuf-mode nix-mode go-tag go-rename yaml-mode terraform-mode request browse-kill-ring company-emacs-eclim company eclim gnuplot-mode chess org-jira github-modern-theme ag go-complete osx-clipboard ob-restclient ob-go org-bullets swiper hackernews magit-gh-pulls helm-google go-snippets go-playground-cli go-stacktracer go-add-tags sqlup-mode popup-kill-ring multiple-cursors magit go-projectile go-dlv go-autocomplete flymake-cursor flycheck)))
 '(projectile-globally-ignored-directories
   (quote
    (".idea" ".ensime_cache" ".eunit" ".git" ".hg" ".fslckout" "_FOSSIL_" ".bzr" "_darcs" ".tox" ".svn" ".stack-work" ".terraform")))
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(ido-mode)
(put 'narrow-to-region 'disabled nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Load Libraries
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load-library "my-color-theme")
(load-file "~/.emacs.d/my-plugins/my-c.el")
(load-file "~/.emacs.d/my-plugins/my-go.el")
(load-file "~/.emacs.d/my-plugins/my-javascript.el")
(load-file "~/.emacs.d/my-plugins/my-keybindings.el")
(load-file "~/.emacs.d/my-plugins/my-lisp.el")
(load-file "~/.emacs.d/my-plugins/my-magit.el")
(load-file "~/.emacs.d/my-plugins/my-org.el")
(load-file "~/.emacs.d/my-plugins/my-python.el")
(load-file "~/.emacs.d/my-plugins/my-random-stuff.el")
(load-file "~/.emacs.d/my-plugins/my-terminal.el")
(load-file "~/.emacs.d/my-plugins/my-weather.el")
(when (string= (getenv "EMACS_EMAIL") "true")
  (load-file "~/.emacs.d/my-plugins/my-email.el"))
(when (not (string= (getenv "SLACK_CLIENT_ID") "nil"))
  (load-file "~/.emacs.d/my-plugins/my-slack.el"))
(electric-indent-mode -1)
(provide 'init)
;;; init.el ends here
