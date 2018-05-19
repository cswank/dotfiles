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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;define modes for file extensions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                                                                        
(add-to-list 'auto-mode-alist '("\\.pde\\'" . c-mode))
(add-to-list 'auto-mode-alist '("\\.pde\\'" . html-mode))
(add-to-list 'auto-mode-alist '("\\.mxml\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.zcml\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.pt\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(setq auto-mode-alist (cons '("\\.py$" . python-mode) auto-mode-alist))
(setq interpreter-mode-alist (cons '("python" . python-mode)
                                   interpreter-mode-alist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Other stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;swiper (replace isearch)
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)

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
    (shell-command-on-region b e
      "jq ." (current-buffer) t)))

;;; turn on syntax highlighting
(global-font-lock-mode 1)

;;; turn on flycheck for all buffers
(add-hook 'after-init-hook #'global-flycheck-mode)

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
;;Undo tree (make sure it is installed with
;;package-list-packages)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'undo-tree)
(global-undo-tree-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;dockerfile mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'dockerfile-mode)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Auto Complete Mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'auto-complete-config)
(require 'go-autocomplete)
(add-to-list 'ac-modes 'go-mode)
(ac-config-default)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Ace stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
(define-key global-map (kbd "C-c b") 'ace-jump-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Kill ring menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key "\C-cy" '(lambda ()
   (interactive)
   (popup-menu 'yank-menu)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ansi-term
;; add this to zsh (to maybe make zsh play nicly with ansi-term
;; if [[ -n ${EMACS} ]]; then
;;     zstyle ':prezto:module:terminal' auto-title 'no'
;; fi
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar my-term-shell "/bin/zsh")
(defadvice ansi-term (before force-bash)
  (interactive (list my-term-shell)))
(ad-activate 'ansi-term)

(defadvice term-sentinel (around my-advice-term-sentinel (proc msg))
  (if (memq (process-status proc) '(signal exit))
      (let ((buffer (process-buffer proc)))
        ad-do-it
        (kill-buffer buffer))
    ad-do-it))
(ad-activate 'term-sentinel)
(global-set-key (kbd "C-x t") 'ansi-term)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Projectile mode (der)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'projectile)
(projectile-global-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;multiple-cursors mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C-c C-n") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c C-p") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-a") 'mc/mark-all-like-this)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;My Keyboard Bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key "\C-x\C-m" 'execute-extended-command)
(setq-default indent-tabs-mode nil)

;;; my-random-stuff.el ends here

