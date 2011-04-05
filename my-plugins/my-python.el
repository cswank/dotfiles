(setenv "PYMACS_PYTHON" "~/.emacs.d/usr/bin/python")
(require 'yasnippet)
(require 'pymacs)
(require 'pysmell)
(require 'doctest-mode)
;;(require 'python-mode)
(require 'python)
;;(require 'ipython)
(add-hook 'py-mode-hook '(lambda () (require 'virtualenv)))

(setq yas/trigger-key (kbd "C-c 8"))
(yas/initialize)
(yas/load-directory "~/.emacs.d/plugins/yasnippet-0.6.1c/snippets")

(defun load-ropemacs ()
    "Load pymacs and ropemacs"
    (interactive)
    (require 'pymacs)
    (pymacs-load "ropemacs" "rope-")
    ;; Automatically save project python buffers before refactorings
    (setq ropemacs-confirm-saving 'nil)
  )
(global-set-key "\C-xpl" 'load-ropemacs)
