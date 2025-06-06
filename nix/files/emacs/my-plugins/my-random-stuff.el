;;; my-random-stuff --- Summary
;;; 
;;; Commentary:

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Dired Ignores
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'dired-x)
(setq-default dired-omit-files-p t) ; this is buffer-local variable
(setq dired-omit-files "^\.[a-z|A-Z]+\|^\.?#\|^\.$")
(setq dired-omit-extensions '(".pyo" ".pyc" "~" ".bak" ".pt.cache" ".svn" ".egg-info" ".git" ".gitignore" ".coverprofile"))
(add-hook 'dired-mode-hook 'dired-omit-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;define modes for file extensions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                                                                        
(add-to-list 'auto-mode-alist '("\\.pde\\'" . c-mode))
(add-to-list 'auto-mode-alist '("\\.pde\\'" . html-mode))
(add-to-list 'auto-mode-alist '("\\.mxml\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.zcml\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.pt\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\`/tmp/zsh[a-z0-9A-Z]+\\'" . sh-mode))
(setq auto-mode-alist (cons '("\\.py$" . python-mode) auto-mode-alist))
(setq interpreter-mode-alist (cons '("python" . python-mode)
                                   interpreter-mode-alist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Other stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; example
;; query-replace-regexp
;;  "./\(.+\).json" → "./\,(to-camelcase).json"
;; will find strings within a quote like "./this_and_that.json"
;; and change it to "./ThisAndThat.json"
(defun to-camelcase ()
  "use query-replace-regexp with a replacment expresion like '\,(to-camelcase)'"
  (string-inflection-pascal-case-function (match-string 1))
)

(defun abv (a b)
  "alcohol by volume"
  (* (- a b) 131.25))

(electric-indent-mode -1)
(electric-pair-mode -1)

(ido-mode 1)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(setq-default indent-tabs-mode nil)

;; keepasscx
(require 'keepass-mode)

;; make *scratch* buffer org
(setq initial-major-mode 'org-mode)

;; edit s3 files
(require 's3ed)
(s3ed-mode)
;;(global-set-key (kbd "C-c s f") 's3ed-find-file)
;;(global-set-key (kbd "C-c s s") 's3ed-save-file)
;;(global-set-key (kbd "C-c s p") 's3ed-set-profile)

;; recentf
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)

;;swiper (replace isearch)
(ivy-mode 1)
(ivy-prescient-mode 1)
(setq ivy-use-virtual-buffers t)

;;old style full screen
(setq ns-use-native-fullscreen nil)

(menu-bar-mode -1)
(global-auto-revert-mode 1)
(defalias 'qrr 'query-replace-regexp)
(setq-default truncate-lines t)
(put 'dired-find-alternate-file 'disabled nil)

(defun beautify-json ()
  (interactive)
  (let ((b (if mark-active (min (point) (mark)) (point-min)))
        (e (if mark-active (max (point) (mark)) (point-max))))
    (shell-command-on-region b e "jq ." (current-buffer) t)))

(defun un-beautify-json-json ()
  "Remove the following letters: white space from json."
  (interactive)
  (let ((b (if mark-active (min (point) (mark)) (point-min)))
        (e (if mark-active (max (point) (mark)) (point-max))))
    (shell-command-on-region b e "jq -c ." (current-buffer) t)))

;;; turn on syntax highlighting
(global-font-lock-mode 1)

(show-paren-mode 1)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Terraform
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'terraform-mode-hook #'terraform-format-on-save-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;dockerfile mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'dockerfile-mode)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Projectile mode (der)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'projectile)
(projectile-mode +1)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; YASnippets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'yasnippet)
(yas-global-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parsyl stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun token ()
  "Get a parsyl auth token"
  (interactive)
  (setenv "AUTH" (format "Authorization: Bearer %s" (shell-command-to-string "token"))))

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
  
;;; my-random-stuff.el ends here
