;;; my-org --- Summary
;;; setup vars for org-mode
;;; Commentary:
;;; Code:
(org-babel-do-load-languages
 'org-babel-load-languages
 '( (python . t)
    (sql . t)
    (ditaa . t)
    (shell . t)
	(restclient . t)
    (go . t)
    (gnuplot . t)
    ))

(setq org-src-fontify-natively t)
(setq org-confirm-babel-evaluate nil)
(provide 'my-org)
;;; my-org.el ends here

