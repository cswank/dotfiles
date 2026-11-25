;;; my-sql --- Summary  -*- lexical-binding: t; -*-
;;; setup sql-mode stuff
;;; Commentary:

(eval-after-load 'sql-mode
  '(load-library "sql-indent"))

;; (defun org-babel-edit-prep:sql (babel-info)
;;   (setq-local buffer-file-name (->> babel-info caddr (alist-get :tangle)))
;;   (setq-local lsp-buffer-uri (->> babel-info caddr (alist-get :tangle) lsp--path-to-uri))
;;   (lsp))

;; (add-hook 'sql-mode-hook 'lsp)
;; (setq lsp-sqls-workspace-config-path nil)
;; (setq lsp-sqls-connections
;;     '(((driver . "postgresql") (dataSourceName . "host=parsyl.cc7inmz0czn4.us-east-1.rds.amazonaws.com port=5432 user=parsyl dbname=parsyl"))))

(defun sql-indent-string ()
  "Indents the string under the cursor as SQL."
  (interactive)
  (save-excursion
    (er/mark-inside-quotes)
    (let* ((text (buffer-substring-no-properties (region-beginning) (region-end)))
           (pos (region-beginning))
           (column (progn (goto-char pos) (current-column)))
           (formatted-text (with-temp-buffer
                             (insert text)
                             (delete-trailing-whitespace)
                             (sql-indent-buffer)
                             (replace-string "\n" (concat "\n" (make-string column (string-to-char " "))) nil (point-min) (point-max))
                             (buffer-string))))
      (delete-region (region-beginning) (region-end))
      (goto-char pos)
      (insert formatted-text))))

;; completion of table and column names in sql-mode (including org
;; babel edit buffers) for the database selected with `load-sql'
(defvar my-sql--schema-cache (make-hash-table :test 'equal)
  "Table and column names, keyed by (host port name).")

(defun my-sql--schema-key ()
  (list database-host database-port database-name))

(defun my-sql--fetch-schema ()
  "Return table and column names from the current database, or nil on failure."
  (let ((process-environment (cons "PGCONNECT_TIMEOUT=3" process-environment))
        (query "select table_name from information_schema.tables
                where table_schema not in ('pg_catalog', 'information_schema')
                union
                select column_name from information_schema.columns
                where table_schema not in ('pg_catalog', 'information_schema')"))
    (with-temp-buffer
      (when (zerop (call-process "psql" nil '(t nil) nil "-X" "-At"
                                 "-h" database-host "-p" database-port
                                 "-U" database-user "-d" database-name
                                 "-c" query))
        (split-string (buffer-string) "\n" t)))))

(defun my-sql--candidates ()
  (let ((key (my-sql--schema-key)))
    (or (gethash key my-sql--schema-cache)
        (let ((names (my-sql--fetch-schema)))
          (when names
            (puthash key names my-sql--schema-cache))))))

(defun my-sql-refresh-completions ()
  "Forget cached table and column names for the current database."
  (interactive)
  (remhash (my-sql--schema-key) my-sql--schema-cache)
  (message "%d names for %s" (length (my-sql--candidates)) database-name))

(defun my-sql-completion-at-point ()
  (when (boundp 'database-name)
    (let ((bounds (bounds-of-thing-at-point 'symbol)))
      (list (or (car bounds) (point))
            (or (cdr bounds) (point))
            (completion-table-dynamic (lambda (_) (my-sql--candidates)))
            :exclusive 'no))))

(add-hook 'sql-mode-hook
          (lambda ()
            (add-hook 'completion-at-point-functions #'my-sql-completion-at-point nil t)
            (company-mode)))

(defun load-sql ()
  "Prompt user to pick a choice from a list."
  (interactive)
  (let ((choices '("insurance" "pas")))
    (setq db (completing-read "Select db:" choices )))
  (when db
    (let ((choices '("dev" "test" "prod" "local")))
      (setq env (completing-read (format "Select environment for %s:" db) choices )))
    (when env
      (load-file (format "~/.parsyl/%s/%s.el" db env))
      (unless (equal env "local")
        (sql-open-tunnel db env)))))

;; SSM tunnels through the bastion, same as ins-db in zshrc
(defvar sql-tunnel-config-names '(("pas" . "pas") ("insurance" . "wb"))
  "Map `load-sql' db names to ~/.parsyl/database/<name>-<env>.env names.")

(defun sql-tunnel--read-config (file)
  "Return an alist of KEY=VALUE settings in the shell env FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (let (settings)
      (while (re-search-forward "^\\([A-Z_]+\\)=[\"']?\\([^\"'\n]*\\)" nil t)
        (push (cons (match-string 1) (match-string 2)) settings))
      settings)))

(defun sql-tunnel--port-open-p (port)
  (/= 2 (call-process "pg_isready" nil nil nil "-q" "-h" "localhost" "-p" port)))

(defun sql-open-tunnel (db env)
  "Open an SSM tunnel to the DB database in ENV unless its port is already open."
  (interactive
   (list (completing-read "Select db:" (mapcar #'car sql-tunnel-config-names))
         (completing-read "Select environment:" '("dev" "test" "prod"))))
  (let* ((file (format "~/.parsyl/database/%s-%s.env"
                       (alist-get db sql-tunnel-config-names nil nil #'equal) env))
         (conf (sql-tunnel--read-config file))
         (get (lambda (key &optional default) (or (cdr (assoc key conf)) default)))
         (profile (funcall get "PROFILE"))
         (region (funcall get "REGION" "us-east-1"))
         (port (funcall get "LOCAL_PORT" "15432"))
         (name (format "sql-tunnel %s-%s" db env)))
    (if (sql-tunnel--port-open-p port)
        (message "%s: localhost:%s is already open" name port)
      (message "%s: finding bastion..." name)
      (let ((target (string-trim
                     (with-output-to-string
                       (call-process "aws" nil (list standard-output nil) nil
                                     "ec2" "describe-instances"
                                     "--profile" profile "--region" region
                                     "--filters" "Name=tag:Name,Values=SSHBastion"
                                     "Name=instance-state-name,Values=running"
                                     "--query" "Reservations[0].Instances[0].InstanceId"
                                     "--output" "text")))))
        (unless (string-prefix-p "i-" target)
          (error "%s: no bastion found (aws sso login --profile %s?)" name profile))
        (let ((proc (start-process
                     name (format "*%s*" name) "aws" "ssm" "start-session"
                     "--profile" profile "--region" region "--target" target
                     "--document-name" "AWS-StartPortForwardingSessionToRemoteHost"
                     "--parameters" (json-serialize
                                     `((host . [,(funcall get "DB_HOST")])
                                       (portNumber . ["5432"])
                                       (localPortNumber . [,port]))))))
          (set-process-query-on-exit-flag proc nil)
          (message "%s: waiting for localhost:%s..." name port)
          (let ((tries 40))
            (while (and (> tries 0)
                        (process-live-p proc)
                        (not (sql-tunnel--port-open-p port)))
              (sleep-for 0.5)
              (setq tries (1- tries))))
          (if (sql-tunnel--port-open-p port)
              (message "%s: open on localhost:%s" name port)
            (when (process-live-p proc)
              (kill-process proc))
            (display-buffer (process-buffer proc))
            (error "%s: tunnel failed to start" name)))))))

(defun sql-close-tunnels ()
  "Close every tunnel opened by `sql-open-tunnel'."
  (interactive)
  (dolist (proc (process-list))
    (when (string-prefix-p "sql-tunnel " (process-name proc))
      (kill-process proc)
      (message "closed %s" (process-name proc)))))
