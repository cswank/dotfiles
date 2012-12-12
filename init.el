;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Load Paths
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path "~/.emacs.d/plugins")
(add-to-list 'load-path "~/.emacs.d/plugins/el-get")
(add-to-list 'load-path "~/.emacs.d/color")
(add-to-list 'load-path "~/.emacs.d/my-plugins")
(add-to-list 'load-path "~/.emacs.d/plugins/company")
(add-to-list 'load-path "~/.emacs.d/plugins/share/emacs/site-lisp")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;MMM mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(require 'mmm-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Auto Complete Mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/plugins/ac-dict")
(ac-config-default)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;My Keyboard Bindings3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)
(global-set-key "\M-s" 'svn-status)
(setq-default indent-tabs-mode nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;el-get
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-master-branch)
      (goto-char (point-max))
      (eval-print-last-sexp))))
(el-get 'sync)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Load Libraries
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load-library "my-color-theme")
;;(load-file "~/.emacs.d/emacs-for-python/epy-init.el")
(load-file "~/.emacs.d/my-plugins/my-python.el")
(require 'xcscope)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Dired Ignores
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'dired-load-hook
(lambda ()
(load "dired-x")))
(add-hook 'dired-mode-hook
(lambda ()
(setq dired-omit-files "^\.[a-z|A-Z]+\|^\.?#\|^\.$")
(setq dired-omit-extensions '(".pyo" ".pyc" "~" ".bak" ".pt.cache" ".svn"))))
;;(dired-omit-mode 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;My Macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(fset 'ipdb
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ("from ipdb import set_trace; set_trace()" 0 "%d")) arg)))
(fset 'pypath
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ("os.path.abspath(os.path.dirname(__file__))" 0 "%d")) arg)))
(fset 'stars
   "print '*' * 80\C-m")
(fset 'jq
   "$('#')\C-b\C-b")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;define modes for file extensions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                                                                        
(add-to-list 'auto-mode-alist '("\\.pde\\'" . c-mode))
(add-to-list 'auto-mode-alist '("\\.pde\\'" . html-mode))
(add-to-list 'auto-mode-alist '("\\.mxml\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.zcml\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.pt\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.txt\\'" . doctest-mode))
(setq auto-mode-alist (cons '("\\.py$" . python-mode) auto-mode-alist))
(setq interpreter-mode-alist (cons '("python" . python-mode)
                                   interpreter-mode-alist))
(autoload 'python-mode "python-mode" "Python editing mode." t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Other stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-auto-revert-mode 1)
(add-hook 'python-mode-hook (lambda () (show-paren-mode 1)))
(defalias 'qrr 'query-replace-regexp)
;;(add-to-list 'load-path "~/emacs/plugins/tramp/")
;;(require 'tramp)
;;(setq tramp-default-method "scp")
;;(setq tramp-auto-save-directory "~/.emacs.d/tramp-autosave")
(setq-default truncate-lines t)
(put 'dired-find-alternate-file 'disabled nil)
(require 'psvn)
(require 'pretty-mode)

(autoload 'multi-mode
  "multi-mode"
  "Allowing multiple major modes in a buffer."
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Custom Set Variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(global-linum-mode nil)
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
