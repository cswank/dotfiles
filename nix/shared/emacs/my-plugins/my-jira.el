;;; my-jira --- Summary
;;; setup org-jira stuff
;;; Commentary:

;;; Code:

(setq jiralib-url "https://parsyl.atlassian.net")

;;; used by M-x org-jira-get-issues-from-custom-jql

(setq org-jira-custom-jqls
  '(
    (:jql "project IN (PAR)
and sprint in openSprints()
order by priority, created DESC"
          :limit 20
          :filename "eng-priority-items")
    ))

;; project IN (PAR)
;; and status IN ('Sprint Queue', 'Code Review' 'Done' 'IN TEST' 'In Dev')
;; AND (labels IN ('webapp' 'backend'))
;; order by priority, created DESC

(provide 'my-jira)
;;; my-jira.el ends here
