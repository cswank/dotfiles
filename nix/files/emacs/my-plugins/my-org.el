;;; my-org --- Summary
;;; setup vars for org-mode
;;; Commentary:
;;; Code:

(use-package ob-duckdb
  :straight (:host github :repo "gggion/ob-duckdb")
  :after org
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   (append org-babel-load-languages '((duckdb . t)))))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
    (sql . t)
    (duckdb . t)
    (ditaa . t)
    (shell . t)
    (restclient . t)
    (go . t)
    (gnuplot . t)
    (http . t)
    (calc . t)
    (plantuml . t)
    ))

(require 'org-tempo)
(setq org-startup-folded t)
(setq org-src-fontify-natively t)
(setq org-confirm-babel-evaluate nil)

(setq org-mobile-directory "~/Library/Mobile Documents/iCloud~com~mobileorg~mobileorg/Documents")
(setq org-directory "~/Documents/Org")
(setq org-mobile-inbox-for-pull "~/Documents/Org/flagged.org")
(setq org-mobile-files
      '("recipies.org" "astronomy.org"))

(setq org-plantuml-jar-path "~/.nix-profile/lib/plantuml.jar")

(provide 'my-org)
;;; my-org.el ends here
